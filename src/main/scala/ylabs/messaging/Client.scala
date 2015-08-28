package ylabs.messaging

import Client._
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

  type User = String
  type Password = String

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
    case class RegisterUser(username: User, password: Password)
    object DeleteUser
    case class RegisterMessageListener(actor: ActorRef)

    case class Connect(username: String, password: String)
    object Connected
    case class ConnectError(t: Throwable)

    object Disconnect

    case class ChatTo(otherUser: User)
    case class SendMessage(otherUser: User, message: String)
    case class LeaveChat(otherUser: User)

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
        val connection = connect(c.username, c.password)
        val chatManager = setupChatManager(connection)
        (connection, chatManager)
      } match {
        case Success((connection, chatManager)) ⇒
          log.info(s"user ${c.username} successfully connected")
          goto(Connected) using ctx.copy(connection = Some(connection), chatManager = Some(chatManager))
        case Failure(t) ⇒
          log.error(t, s"unable to connect as user ${c.username}")
          sender ! Messages.ConnectError(t)
          stay
      }

    case Event(Messages.RegisterMessageListener(actor), ctx) ⇒
      stay using ctx.copy(messageListeners = ctx.messageListeners + actor)
  }

  onTransition {
    case Unconnected -> Connected =>
      sender ! Messages.Connected
  }

  when(Connected) {
    case Event(Messages.ChatTo(name), ctx) if ctx.chatManager.isDefined ⇒
      log.info(s"creating chat to $name")
      val chat = chatTo(ctx.chatManager.get, name)
      stay using ctx.copy(chats = ctx.chats + (name → chat))

    case Event(Messages.Disconnect, ctx) ⇒
      disconnect(ctx)
      goto(Unconnected) using ctx.copy(connection = None, chatManager = None, chats = Map.empty)

    case Event(Messages.RegisterMessageListener(actor), ctx) ⇒
      stay using ctx.copy(messageListeners = ctx.messageListeners + actor)

    case Event(Messages.SendMessage(otherUser, message), ctx) ⇒
      ctx.chats.get(otherUser) match {
        case Some(chat) ⇒
          chat.sendMessage(message)
          log.info(s"message sent to $otherUser")
        case None ⇒ log.error(s"no chat with user $otherUser found!")
      }
      stay

    case Event(Messages.LeaveChat(otherUser), ctx) ⇒
      ctx.chats.get(otherUser) match {
        case Some(chat) ⇒
          chat.close()
          log.info(s"left chat with $otherUser")
          stay using ctx.copy(chats = ctx.chats - otherUser)
        case _ ⇒
          log.warning(s"no chat open with user $otherUser")
          stay
      }

    case Event(msg: Messages.MessageReceived, ctx) ⇒
      ctx.messageListeners foreach { _ ! msg }
      stay

    case Event(register: Messages.RegisterUser, ctx) if ctx.connection.isDefined ⇒
      log.info(s"trying to register user ${register.username}")
      val accountManager = AccountManager.getInstance(ctx.connection.get)
      Try {
        accountManager.createAccount(register.username, register.password)
      } match {
        case Success(s) ⇒ log.info(s"user ${register.username} successfully created")
        case Failure(t) ⇒ log.error(t, s"could not register user ${register.username}!")
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

  def connect(username: String, password: String): XMPPTCPConnection = {
    val connection = new XMPPTCPConnection(
      XMPPTCPConnectionConfiguration.builder
      .setUsernameAndPassword(username, password)
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
    log.info("connected")
    chatManager
  }

  def disconnect(ctx: Context): Unit = {
    ctx.chats.values.foreach(_.close())
    ctx.connection.foreach(_.disconnect())
    log.info("disconnected")
  }

  def chatTo(chatManager: ChatManager, otherUser: User): Chat = {
    val chat = chatManager.createChat(s"$otherUser@$domain")
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

