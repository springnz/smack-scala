package ylabs.messaging

import Client._
import OutOfBandData._
import akka.actor.{ Actor, ActorRef, FSM }
import com.typesafe.config.ConfigFactory
import java.util.Collection
import org.jivesoftware.smack.ConnectionConfiguration.SecurityMode
import org.jivesoftware.smack.chat._
import org.jivesoftware.smack.packet.{ ExtensionElement, Message, Presence }
import org.jivesoftware.smack.roster.{ Roster, RosterListener }
import org.jivesoftware.smack.tcp.{ XMPPTCPConnection, XMPPTCPConnectionConfiguration }
import org.jivesoftware.smackx.iqregister.AccountManager
import scala.collection.JavaConversions._
import scala.util.{ Failure, Success, Try }

object Client {
  object ConfigKeys {
    val domain = "messaging.domain"
    val host = "messaging.host"
  }

  case class User(value: String) extends AnyVal
  case class Password(value: String) extends AnyVal

  sealed trait State
  case object Unconnected extends State
  case object Connected extends State

  case class Context(
    connection: Option[XMPPTCPConnection],
    chats: Map[User, Chat],
    eventListeners: Set[ActorRef]
  )

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
    case class SendFileMessage(recipient: User, fileUrl: String, description: Option[String])

    sealed trait ListenerEvent
    case class MessageReceived(chat: Chat, message: Message) extends ListenerEvent
    case class FileMessageReceived(chat: Chat, message: Message, outOfBandData: OutOfBandData) extends ListenerEvent
    case class UserBecameAvailable(user: User) extends ListenerEvent
    case class UserBecameUnavailable(user: User) extends ListenerEvent
  }
}

class Client extends FSM[State, Context] {
  startWith(Unconnected, Context(connection = None, chats = Map.empty, eventListeners = Set.empty))

  lazy val config = ConfigFactory.load()
  lazy val domain = config.getString(ConfigKeys.domain)
  lazy val host = config.getString(ConfigKeys.host)

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

    case Event(msg: Messages.ListenerEvent, ctx) ⇒
      ctx.eventListeners foreach { _ ! msg }
      stay

    case Event(Messages.SendMessage(recipient, message), ctx @ Context(Some(connection), chats, _)) ⇒
      val chat = chats.getOrElse(key = recipient, createChat(connection, recipient))
      chat.sendMessage(message)
      log.info(s"message sent to $recipient")
      stay using ctx.copy(chats = chats + (recipient → chat))

    case Event(Messages.SendFileMessage(recipient, fileUrl, description), ctx) ⇒
      val chat = ctx.chats.getOrElse(key = recipient, createChat(ctx.connection.get, recipient))
      val fileInformation = OutOfBandData(fileUrl, description)
      val infoText = "This message contains a link to a file, your client needs to " +
        "implement XEP-0066. If you don't see the file, kindly ask the client developer."
      val message = new Message(recipient.value, infoText)
      message.addExtension(fileInformation)
      chat.sendMessage(message)
      log.info(s"file message sent to $recipient")
      stay using ctx.copy(chats = ctx.chats + (recipient → chat))

    case Event(register: Messages.RegisterUser, Context(Some(connection), chats, _)) ⇒
      log.info(s"trying to register ${register.user}")
      val accountManager = AccountManager.getInstance(connection)
      Try {
        accountManager.createAccount(register.user.value, register.password.value)
      } match {
        case Success(s) ⇒ log.info(s"${register.user} successfully created")
        case Failure(t) ⇒ log.error(t, s"could not register ${register.user}!")
      }
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
    val connection = new XMPPTCPConnection(
      XMPPTCPConnectionConfiguration.builder
      .setUsernameAndPassword(user.value, password.value)
      .setServiceName(domain)
      .setHost(host)
      .setSecurityMode(SecurityMode.disabled)
      .setSendPresence(true)
      .build
    )
    connection.connect().login()
    setupChatManager(connection)
    setupRosterListener(connection)
    connection
  }

  def disconnect(ctx: Context): Unit = {
    ctx.chats.values.foreach(_.close())
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

  def createChat(connection: XMPPTCPConnection, recipient: User): Chat = {
    val chatManager = ChatManager.getInstanceFor(connection)
    val chat = chatManager.createChat(s"${recipient.value}@$domain")
    chat.addMessageListener(chatMessageListener)
    log.debug(s"chat with $recipient created")
    subscribeToStatus(connection, recipient)
    chat
  }

  def subscribeToStatus(connection: XMPPTCPConnection, user: User): Unit = {
    val username = s"${user.value}@$domain"
    val roster = Roster.getInstanceFor(connection)
    if (!roster.getEntries.contains(username)) {
      val presence = new Presence(Presence.Type.subscribe)
      presence.setTo(username)
      log.info(s"requesting roster presence permissions for $user")
      connection.sendStanza(presence)
      log.info(s"requested roster presence permissions for $user")
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

  initialize()
}

