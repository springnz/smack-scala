package smack.scala

import java.io.{ InputStream, File }
import java.net.URI

import Client._
import OutOfBandData._
import _root_.smack.scala.aws.S3Adapter
import akka.actor.{ Actor, ActorRef, FSM }
import com.typesafe.config.ConfigFactory
import java.util.{ UUID, Collection }
import org.jivesoftware.smack.ConnectionConfiguration.SecurityMode
import org.jivesoftware.smack.XMPPException.XMPPErrorException
import org.jivesoftware.smack.chat._
import org.jivesoftware.smack.packet._
import org.jivesoftware.smack.roster.{ Roster, RosterListener }
import org.jivesoftware.smack.tcp.{ XMPPTCPConnection, XMPPTCPConnectionConfiguration }
import org.jivesoftware.smackx.iqregister.AccountManager
import org.jivesoftware.smackx.receipts.{ DeliveryReceiptManager, ReceiptReceivedListener }
import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Failure, Success, Try }
import akka.actor.Status.{ Failure ⇒ ActorFailure }

object Client {

  object ConfigKeys {
    val domain = "messaging.domain"
    val host = "messaging.host"
  }

  case class User(value: String) extends AnyVal {
    def splitUserIntoNameAndDomain(defaultDomain: Domain): (UserWithoutDomain, Domain) = {
      val (u, _) = value.span(c ⇒ c != '@')
      (new UserWithoutDomain(u), defaultDomain)
    }

    def getFullyQualifiedUser(defaultDomain: Domain): UserWithDomain = {
      val (user, domain) = splitUserIntoNameAndDomain(defaultDomain)
      return user getFullyQualifiedUser domain
    }
  }

  case class UserWithoutDomain(value: String) extends AnyVal {
    def getFullyQualifiedUser(d: Domain): UserWithDomain = {
      UserWithDomain(s"${value}@${d.value}")
    }
  }

  case class UserWithDomain(value: String) extends AnyVal

  case class Password(value: String) extends AnyVal
  case class Domain(value: String) extends AnyVal
  case class MessageId(value: String) extends AnyVal

  sealed trait State
  case object Unconnected extends State
  case object Connected extends State

  sealed trait MessageStatus
  case object Acknowledged extends MessageStatus
  case object Unacknowledged extends MessageStatus

  case class MessageState(
    message: String,
    id: MessageId,
    status: MessageStatus)

  case class ChatState(
    channel: Chat,
    messages: Seq[MessageState])

  case class Context(
    connection: Option[XMPPTCPConnection],
    chats: Map[UserWithDomain, ChatState],
    eventListeners: Set[ActorRef])

  object Messages {
    case class RegisterUser(user: User, password: Password)
    object DeleteUser
    case class RegisterEventListener(actor: ActorRef)

    case class Connect(user: User, password: Password)
    object Connected
    case class ConnectError(t: Throwable)
    object Disconnect

    object GetRoster
    case class GetRosterResponse(roster: Roster)

    case class SendMessage(recipient: User, message: String)
    case class SendUrlMessage(recipient: User, fileUri: URI, description: FileDescription)
    case class SendFileMessage(recipient: User, file: File, description: FileDescription)
    case class SendStreamMessage(recipient: User, stream: InputStream, description: FileDescription)

    case class GetUnackMessages(user: User)
    case class GetUnackMessagesResponse(user: User, ids: Seq[MessageState])

    sealed trait ListenerEvent
    case class MessageReceived(chat: Chat, message: Message) extends ListenerEvent
    case class FileMessageReceived(chat: Chat, message: Message, outOfBandData: OutOfBandData) extends ListenerEvent
    case class UserBecameAvailable(user: User) extends ListenerEvent
    case class UserBecameUnavailable(user: User) extends ListenerEvent
    case class MessageDelivered(user: User, messageId: MessageId) extends ListenerEvent
    case class FileUploaded(forUser: User, uri: URI, description: FileDescription) extends ListenerEvent

    sealed trait SmackError extends Throwable with ListenerEvent
    case class DuplicateUser(user: User) extends SmackError
    case class InvalidUserName(user: User) extends SmackError
    case class GeneralSmackError(reason: Throwable) extends SmackError
    case class FileUploadError(reason: Throwable) extends SmackError
  }
}

class Client extends FSM[State, Context] {
  startWith(Unconnected, Context(connection = None, chats = Map.empty, eventListeners = Set.empty))

  lazy val config = ConfigFactory.load()
  lazy val defaultDomain = Domain(config.getString(ConfigKeys.domain))
  lazy val host = config.getString(ConfigKeys.host)
  lazy val uploadAdapter: FileUpload = new S3Adapter

  when(Unconnected) {
    case Event(c: Messages.Connect, ctx) ⇒
      Try { connect(c.user, c.password) } match {
        case Success(connection) ⇒
          log.info(s"${c.user} successfully connected")
          goto(Connected) using ctx.copy(connection = Some(connection))
        case Failure(t) ⇒
          log.error(t, s"unable to connect as ${c.user}")
          sender ! Messages.ConnectError(t)
          stay
      }

    case Event(Messages.RegisterEventListener(actor), ctx) ⇒
      stay using ctx.copy(eventListeners = ctx.eventListeners + actor)

    case Event(msg: Messages.ListenerEvent, ctx) ⇒
      ctx.eventListeners foreach { _ ! msg }
      stay
  }

  onTransition {
    // format: OFF
    case Unconnected -> Connected ⇒
      sender ! Messages.Connected
    // format: ON
  }

  when(Connected) {
    case Event(Messages.Disconnect, ctx) ⇒
      disconnect(ctx)
      goto(Unconnected) using ctx.copy(connection = None, chats = Map.empty)

    case Event(Messages.RegisterEventListener(actor), ctx) ⇒
      stay using ctx.copy(eventListeners = ctx.eventListeners + actor)

    case Event(msg: Messages.ListenerEvent, ctx @ Context(Some(connection), chats, eventListeners)) ⇒
      val newCtx = msg match {
        case Messages.MessageReceived(chat, message) ⇒
          val (user, domain) = User(chat.getParticipant).splitUserIntoNameAndDomain(defaultDomain)
          subscribeToStatus(connection, user, domain)
          ctx
        case Messages.MessageDelivered(recipient, messageId) ⇒
          val fullUser = recipient.getFullyQualifiedUser(defaultDomain)
          val chat = ctx.chats(fullUser)
          val index = chat.messages.indexWhere(m ⇒ m.id == messageId)
          ctx.copy(chats = ctx.chats + (fullUser -> chat.copy(messages = chat.messages.updated(index, chat.messages(index).copy(status = Acknowledged)))))
        case msg: Messages.ListenerEvent ⇒ ctx
      }
      val reportMsg = msg match {
        case e: Messages.SmackError ⇒ ActorFailure(e)
        case e                      ⇒ e
      }
      eventListeners foreach { _ ! reportMsg }
      stay using newCtx

    case Event(Messages.SendMessage(recipient, message), ctx @ Context(Some(connection), chats, _)) ⇒
      val (user, domain) = recipient.splitUserIntoNameAndDomain(defaultDomain)
      val fullUser = user getFullyQualifiedUser domain
      val chat = chats.getOrElse(key = fullUser, ChatState(createChat(connection, user, domain), Seq.empty))
      val messageToSend = new Message(recipient.value, message)
      chat.channel.sendMessage(messageToSend)
      log.info(s"message sent to $recipient")
      sender ! MessageId(messageToSend.getStanzaId)
      val msgState = MessageState(message, MessageId(messageToSend.getStanzaId), Unacknowledged)
      stay using ctx.copy(chats = ctx.chats + (fullUser → chat.copy(messages = chat.messages :+ msgState)))

    case Event(Messages.SendUrlMessage(recipient, fileUri, description), ctx) ⇒
      val (user, domain) = recipient.splitUserIntoNameAndDomain(defaultDomain)
      val fullUser = user getFullyQualifiedUser domain
      val chat = ctx.chats.getOrElse(key = fullUser, ChatState(createChat(ctx.connection.get, user, domain), Seq.empty))
      val fileInformation = OutOfBandData(fileUri, description)
      val infoText = "This message contains a link to a file, your client needs to " +
        "implement XEP-0066. If you don't see the file, kindly ask the client developer."
      val message = new Message(recipient.value, infoText)
      message.addExtension(fileInformation)
      chat.channel.sendMessage(message)
      log.info(s"file message sent to $recipient")
      sender ! MessageId(message.getStanzaId)
      val msgState = MessageState(message.getBody, MessageId(message.getStanzaId), Unacknowledged)
      stay using ctx.copy(chats = ctx.chats + (fullUser → chat.copy(messages = chat.messages :+ msgState)))

    case Event(Messages.SendFileMessage(recipient, file, description), ctx) ⇒
      val uploadFuture = uploadAdapter.upload(file, description)
      uploadFuture onComplete {
        case Success(uri) ⇒
          self ! Messages.FileUploaded(User(ctx.connection.get.getUser), uri, description)
          self ! Messages.SendUrlMessage(recipient, uri, description)
        case Failure(ex) ⇒
          log.error(ex, s"could not upload file!")
          self ! Messages.FileUploadError(ex)
      }
      stay

    case Event(register: Messages.RegisterUser, Context(Some(connection), chats, _)) ⇒
      log.info(s"trying to register ${register.user}")
      val accountManager = AccountManager.getInstance(connection)
      Try {
        val (username, _) = register.user.splitUserIntoNameAndDomain(defaultDomain)
        if (username.value == defaultDomain.value) throw new Messages.InvalidUserName(register.user)
        accountManager.createAccount(username.value, register.password.value)
      } match {
        case Success(s) ⇒ log.info(s"${register.user} successfully created")
        case Failure(t) ⇒
          log.error(t, s"could not register ${register.user}!")
          val response: ActorFailure = t match {
            case ex: Messages.InvalidUserName ⇒ ActorFailure(ex)
            case ex: XMPPErrorException ⇒
              if (ex.getXMPPError.getCondition == XMPPError.Condition.conflict && ex.getXMPPError.getType == XMPPError.Type.CANCEL)
                ActorFailure(Messages.DuplicateUser(register.user))
              else ActorFailure(Messages.GeneralSmackError(t))

            case _ ⇒ ActorFailure(t)
          }
          sender ! response
      }
      stay

    case Event(Messages.GetUnackMessages(user), Context(_, chats, _)) ⇒
      val fullUser = user.getFullyQualifiedUser(defaultDomain)
      val chatlist = chats.get(fullUser)
      val unack = if (chatlist.isDefined) chatlist.get.messages.filter(m ⇒ m.status == Unacknowledged) else Seq[MessageState]()
      sender ! Messages.GetUnackMessagesResponse(user, unack)
      stay

    case Event(Messages.DeleteUser, ctx @ Context(Some(connection), _, _)) ⇒
      log.info(s"trying to delete user")
      val accountManager = AccountManager.getInstance(connection)
      Try {
        accountManager.deleteAccount()
      } match {
        case Success(s) ⇒ log.info(s"user successfully deleted")
        case Failure(t) ⇒ log.error(t, s"could not delete user!")
      }
      goto(Unconnected) using ctx.copy(connection = None, chats = Map.empty)

    case Event(Messages.GetRoster, Context(Some(connection), chats, _)) ⇒
      val roster = Roster.getInstanceFor(connection)
      sender ! Messages.GetRosterResponse(roster)
      stay
  }

  def connect(user: User, password: Password): XMPPTCPConnection = {
    val (username, domain) = user.splitUserIntoNameAndDomain(defaultDomain)
    val connection = new XMPPTCPConnection(
      XMPPTCPConnectionConfiguration.builder
        .setUsernameAndPassword(username.value, password.value)
        .setServiceName(domain.value)
        .setHost(host)
        .setSecurityMode(SecurityMode.disabled)
        .setSendPresence(true)
        .build)
    connection.connect().login()
    setupChatManager(connection)
    setupRosterListener(connection)
    setupDeliveryReceiptManager(connection)
    connection
  }

  def disconnect(ctx: Context): Unit = {
    ctx.chats.values.foreach(_.channel.close())
    ctx.connection.foreach(_.disconnect())
    log.info("disconnected")
  }

  def setupChatManager(connection: XMPPTCPConnection): ChatManager = {
    val chatManager = ChatManager.getInstanceFor(connection)
    chatManager.addChatListener(new ChatManagerListener {
      override def chatCreated(chat: Chat, createdLocally: Boolean): Unit = {
        log.info(s"ChatManagerListener: chat created: $chat; locally: $createdLocally")
        chat.addMessageListener(chatMessageListener)
      }
    })
    chatManager
  }

  def setupRosterListener(connection: XMPPTCPConnection): Unit =
    Roster.getInstanceFor(connection).addRosterListener(rosterListener)

  def setupDeliveryReceiptManager(connection: XMPPTCPConnection): Unit = {
    val deliveryManager = DeliveryReceiptManager.getInstanceFor(connection)
    deliveryManager.addReceiptReceivedListener(deliveryReceiptListener)
    deliveryManager.autoAddDeliveryReceiptRequests()
    deliveryManager.setAutoReceiptMode(DeliveryReceiptManager.AutoReceiptMode.always)
    log.info(s"delivery manager created")
  }

  def createChat(connection: XMPPTCPConnection, recipient: UserWithoutDomain, domain: Domain): Chat = {
    val chatManager = ChatManager.getInstanceFor(connection)
    val chat = chatManager.createChat(recipient.getFullyQualifiedUser(domain).value)
    log.debug(s"chat with $recipient created")
    subscribeToStatus(connection, recipient, domain)
    chat
  }

  def subscribeToStatus(connection: XMPPTCPConnection, user: UserWithoutDomain, domain: Domain): Unit = {
    if (user.value != domain.value) {
      val username = user.getFullyQualifiedUser(domain).value
      val roster = Roster.getInstanceFor(connection)
      if (!roster.getEntries.contains(username)) {
        val presence = new Presence(Presence.Type.subscribe)
        presence.setTo(username)
        log.info(s"requesting roster presence permissions for $user")
        connection.sendStanza(presence)
        log.info(s"requested roster presence permissions for $user")
      }
    }
  }

  val chatMessageListener = new ChatMessageListener {
    override def processMessage(chat: Chat, message: Message): Unit = {
      // pretty shitty of smack to take a type parameter there... all they do is cast it!
      val fileExtension = message.getExtension[ExtensionElement](OutOfBandData.ElementName, OutOfBandData.XmlNamespace)
      if (fileExtension == null) {
        log.debug(s"ChatMessageListener: received message for $chat : $message")
        self ! Messages.MessageReceived(chat, message)
      } else {
        OutOfBandData.fromXml(fileExtension.toXML) match {
          case Success(outOfBandData) ⇒
            log.debug(s"ChatMessageListener: received file message for $chat : $message")
            self ! Messages.FileMessageReceived(chat, message, outOfBandData)
          case Failure(t) ⇒
            log.error(t, "ChatMessageListener: received file message but was unable to parse the extension into a XEP-0066 format")
            self ! Messages.MessageReceived(chat, message)
        }
      }
    }
  }

  val rosterListener = new RosterListener {
    def entriesAdded(entries: Collection[String]): Unit = {
      log.debug("roster entries added: " + entries.toList)
    }

    def entriesDeleted(entries: Collection[String]): Unit = {
      log.debug("roster entries deleted: " + entries.toList)
    }

    def entriesUpdated(entries: Collection[String]): Unit = {
      log.debug("roster entries updated: " + entries.toList)
    }

    def presenceChanged(presence: Presence): Unit = {
      val user = User(presence.getFrom)
      presence.getType match {
        case Presence.Type.available ⇒
          log.debug(s"$user became available")
          self ! Messages.UserBecameAvailable(user)
        case Presence.Type.unavailable ⇒
          log.debug(s"$user became unavailable")
          self ! Messages.UserBecameUnavailable(user)
        case _ ⇒ log.debug(s"presence changed: $presence")
      }
    }
  }

  val deliveryReceiptListener = new ReceiptReceivedListener {
    override def onReceiptReceived(from: String, to: String, receiptId: String, stanza: Stanza): Unit = {
      log.debug(s"receipt received $from sent to $to with id $receiptId")
      val index = from.lastIndexOf('/')
      val user = if (index > 0) User(from.substring(0, index)) else User(from)
      self ! Messages.MessageDelivered(user, MessageId(receiptId))
    }
  }

  initialize()
}

