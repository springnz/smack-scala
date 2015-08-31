package ylabs.messaging

import Client._
import OutOfBandData._
import akka.actor.{ Actor, ActorRef, FSM }
import com.typesafe.config.ConfigFactory
import org.jivesoftware.smack.ConnectionConfiguration.SecurityMode
import org.jivesoftware.smack.chat._
import org.jivesoftware.smack.packet.Message
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
    chatManager: Option[ChatManager],
    chats: Map[User, Chat],
    messageListeners: Set[ActorRef]
  )

  object Messages {
    case class RegisterUser(user: User, password: Password)
    object DeleteUser
    case class RegisterMessageListener(actor: ActorRef)

    case class Connect(user: User, password: Password)
    object Connected
    case class ConnectError(t: Throwable)

    object Disconnect

    case class ChatTo(recipient: User)
    case class SendMessage(recipient: User, message: String)
    case class SendFileMessage(recipient: User, fileUrl: String, description: Option[String])
    case class LeaveChat(recipient: User)

    case class MessageReceived(chat: Chat, message: Message)
  }
}

class Client extends FSM[State, Context] {
  startWith(Unconnected, Context(None, None, Map.empty, Set.empty))

  lazy val config = ConfigFactory.load()
  lazy val domain = config.getString(ConfigKeys.domain)
  lazy val host = config.getString(ConfigKeys.host)

  when(Unconnected) {
    case Event(c: Messages.Connect, ctx) ⇒
      Try {
        val connection = connect(c.user, c.password)
        val chatManager = setupChatManager(connection)
        (connection, chatManager)
      } match {
        case Success((connection, chatManager)) ⇒
          log.info(s"${c.user} successfully connected")
          goto(Connected) using ctx.copy(connection = Some(connection), chatManager = Some(chatManager))
        case Failure(t) ⇒
          log.error(t, s"unable to connect as ${c.user}")
          sender ! Messages.ConnectError(t)
          stay
      }

    case Event(Messages.RegisterMessageListener(actor), ctx) ⇒
      stay using ctx.copy(messageListeners = ctx.messageListeners + actor)
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
      goto(Unconnected) using ctx.copy(connection = None, chatManager = None, chats = Map.empty)

    case Event(Messages.RegisterMessageListener(actor), ctx) ⇒
      stay using ctx.copy(messageListeners = ctx.messageListeners + actor)

    case Event(Messages.ChatTo(user), ctx) if ctx.chatManager.isDefined ⇒
      log.info(s"creating chat to $user")
      val chat = chatTo(ctx.chatManager.get, user)
      stay using ctx.copy(chats = ctx.chats + (user → chat))

    case Event(Messages.LeaveChat(recipient), ctx) ⇒
      ctx.chats.get(recipient) match {
        case Some(chat) ⇒
          chat.close()
          log.info(s"left chat with $recipient")
          stay using ctx.copy(chats = ctx.chats - recipient)
        case _ ⇒
          log.warning(s"no chat open with user $recipient")
          stay
      }

    case Event(msg: Messages.MessageReceived, ctx) ⇒
      ctx.messageListeners foreach { _ ! msg }
      stay

    case Event(Messages.SendMessage(recipient, message), ctx) ⇒
      ctx.chats.get(recipient) match {
        case Some(chat) ⇒
          chat.sendMessage(message)
          log.info(s"message sent to $recipient")
        case None ⇒ log.error(s"no chat with user $recipient found!")
      }
      stay

    case Event(Messages.SendFileMessage(recipient, fileUrl, description), ctx) ⇒
      ctx.chats.get(recipient) match {
        case Some(chat) ⇒
          val fileInformation = OutOfBandData(fileUrl, description)
          val infoText = "This message contains a link to a file, your client needs to " +
            "implement XEP-0066. If you don't see the file, kindly ask the client developer."
          val message = new Message(recipient.value, infoText)
          message.addExtension(fileInformation)
          chat.sendMessage(message)
          log.info(s"file message sent to $recipient")
        case None ⇒ log.error(s"no chat with user $recipient found!")
      }
      stay

    case Event(register: Messages.RegisterUser, ctx) if ctx.connection.isDefined ⇒
      log.info(s"trying to register ${register.user}")
      val accountManager = AccountManager.getInstance(ctx.connection.get)
      Try {
        accountManager.createAccount(register.user.value, register.password.value)
      } match {
        case Success(s) ⇒ log.info(s"${register.user} successfully created")
        case Failure(t) ⇒ log.error(t, s"could not register ${register.user}!")
      }
      stay

    case Event(Messages.DeleteUser, ctx) if ctx.connection.isDefined ⇒
      log.info(s"trying to delete user")
      val accountManager = AccountManager.getInstance(ctx.connection.get)
      Try {
        accountManager.deleteAccount()
      } match {
        case Success(s) ⇒ log.info(s"user successfully deleted")
        case Failure(t) ⇒ log.error(t, s"could not delete user!")
      }
      self ! Messages.Disconnect
      stay

  }

  def connect(user: User, password: Password): XMPPTCPConnection = {
    val connection = new XMPPTCPConnection(
      XMPPTCPConnectionConfiguration.builder
      .setUsernameAndPassword(user.value, password.value)
      .setServiceName(domain)
      .setHost(host)
      .setSecurityMode(SecurityMode.disabled)
      .build
    )
    connection.connect().login()
    connection
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

  def disconnect(ctx: Context): Unit = {
    ctx.chats.values.foreach(_.close())
    ctx.connection.foreach(_.disconnect())
    log.info("disconnected")
  }

  def chatTo(chatManager: ChatManager, user: User): Chat = {
    val chat = chatManager.createChat(s"${user.value}@$domain")
    chat.addMessageListener(chatMessageListener)
    chat
  }

  val chatMessageListener = new ChatMessageListener {
    override def processMessage(chat: Chat, message: Message): Unit = {
      log.debug(s"ChatMessageListener: received message for $chat : $message")
      self ! Messages.MessageReceived(chat, message)
    }
  }

  initialize()
}

