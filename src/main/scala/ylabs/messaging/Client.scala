package ylabs.messaging

import Client._
import akka.actor.{ Actor, ActorRef, FSM }
import org.jivesoftware.smack.ConnectionConfiguration.SecurityMode
import org.jivesoftware.smack.chat._
import org.jivesoftware.smack.packet.Message
import org.jivesoftware.smack.tcp.{ XMPPTCPConnection, XMPPTCPConnectionConfiguration }
import scala.collection.JavaConversions._

object Client {
  type User = String

  sealed trait State
  case object Unconnected extends State
  case object Connected extends State

  case class Context(
    connection: Option[XMPPTCPConnection],
    chatManager: Option[ChatManager],
    chats: Map[User, Chat],
    messageListeners: Seq[ActorRef]
  )

  object Messages {
    case class RegisterMessageListener(actor: ActorRef)

    case class Connect(username: String, password: String)
    object Connected

    object Disconnect

    case class ChatTo(otherUser: User)
    case class SendMessage(otherUser: User, message: String)
    case class LeaveChat(otherUser: User)

    case class MessageReceived(chat: Chat, message: Message)
  }

  val domain = "corp"
  val host = "akllap015.corp"
}

class Client extends FSM[State, Context] {
  startWith(Unconnected, Context(None, None, Map.empty, Seq.empty))
  // val log = LoggerFactory.getLogger(getClass)

  when(Unconnected) {
    case Event(c: Messages.Connect, ctx) ⇒
      val connection = connect(c.username, c.password)
      val chatManager = setupChatManager(connection)
      sender ! Messages.Connected
      goto(Connected) using ctx.copy(connection = Some(connection), chatManager = Some(chatManager))

    case Event(Messages.RegisterMessageListener(actor), ctx) ⇒
      stay using ctx.copy(messageListeners = ctx.messageListeners :+ actor)
  }

  when(Connected) {
    case Event(Messages.ChatTo(name), ctx) ⇒
      log.info(s"creating chat to $name")
      val chat = chatTo(ctx.chatManager.get, name)
      stay using ctx.copy(chats = ctx.chats + (name → chat))

    case Event(Messages.Disconnect, ctx) ⇒
      disconnect(ctx)
      goto(Unconnected) using ctx.copy(connection = None, chatManager = None, chats = Map.empty)

    case Event(Messages.RegisterMessageListener(actor), ctx) ⇒
      stay using ctx.copy(messageListeners = ctx.messageListeners :+ actor)

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
  }

  def connect(username: String, password: String): XMPPTCPConnection = {
    val connection = new XMPPTCPConnection(
      XMPPTCPConnectionConfiguration.builder
      .setUsernameAndPassword(username, password)
      .setServiceName("corp")
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

