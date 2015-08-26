package ylabs.messaging

import akka.actor.ActorRef
import akka.actor.{ Actor, ActorSystem }
import org.jivesoftware.smack.ConnectionConfiguration.SecurityMode
import org.jivesoftware.smack.chat._
import org.jivesoftware.smack.packet.Message
import org.jivesoftware.smack.tcp.{ XMPPTCPConnection, XMPPTCPConnectionConfiguration }
import scala.collection.JavaConversions._
import scala.collection.mutable

object Client {
  type User = String

  object Messages {
    case class RegisterMessageListener(actor: ActorRef)

    case class Connect(username: String, password: String)
    object Connected

    case class ChatTo(otherUser: User)
    case class SendMessage(otherUser: User, message: String)
    case class LeaveChat(otherUser: User)

    object Disconnect
    object Shutdown
  }

  case class MessageReceived(chat: Chat, message: Message)

  val domain = "corp"
  val host = "akllap015.corp"
  def computerSays(s: String) = println(s">> $s")
}

class Client extends Actor {
  import Client._
  import Client.Messages._
  import context._

  var connection: XMPPTCPConnection = _
  var chatManager: ChatManager = _
  val chats = mutable.Map.empty[User, Chat]
  val messageListeners = mutable.ArrayBuffer.empty[ActorRef]

  override def receive = {
    case c: Connect ⇒
      connect(c)
      sender ! Connected
      become(connected)

    case Shutdown ⇒ context.system.shutdown()
    case RegisterMessageListener(actor) => messageListeners += actor
    case _        ⇒ computerSays("not connected!")
  }

  def connected: Receive = {
    case ChatTo(name) ⇒
      println(s"creating chat to $name")
      chatTo(name)
      become(inChat)

    case Disconnect ⇒
      disconnect()
      unbecome()

    case Shutdown ⇒
      self ! Disconnect
      self ! Shutdown

    case Connect ⇒ computerSays("already connected!")
    case RegisterMessageListener(actor) => messageListeners += actor
    case _       ⇒ computerSays("connected, but not in chat!")
  }

  def inChat: Receive = {
    case LeaveChat(otherUser) ⇒
      chats.get(otherUser) match {
        case Some(chat) ⇒
          chat.close()
          chats -= otherUser
          computerSays(s"left chat with $otherUser")
        case _ ⇒ computerSays(s"no chat open with user $otherUser")
      }

    case SendMessage(otherUser, message) ⇒
      chats.get(otherUser) match {
        case Some(chat) ⇒
          chat.sendMessage(message)
          computerSays(s"message sent to $otherUser")
        case None ⇒ computerSays(s"no chat with user $otherUser found!")
      }

    case Disconnect ⇒
      disconnect()
      unbecome()

    case Shutdown ⇒
      self ! Disconnect
      self ! Shutdown

    case RegisterMessageListener(actor) => messageListeners += actor
    case Connect ⇒ computerSays("already connected!")
  }

  def connect(connect: Connect) = {
    connection = new XMPPTCPConnection(
      XMPPTCPConnectionConfiguration.builder
      .setUsernameAndPassword(connect.username, connect.password)
      .setServiceName("corp")
      .setHost(host)
      .setSecurityMode(SecurityMode.disabled)
      .build
    )
    connection.connect().login()

    chatManager = ChatManager.getInstanceFor(connection)
    chatManager.addChatListener(new ChatManagerListener {
      override def chatCreated(chat: Chat, createdLocally: Boolean): Unit = {
        computerSays(s"ChatManagerListener: chat created: $chat; locally: $createdLocally")
        chat.addMessageListener(printingMessageListener)
      }
    })
    computerSays("connected")
  }

  def disconnect() = {
    chats.values.foreach(_.close())
    chats.clear()
    Option(connection).map(_.disconnect())
    computerSays("disconnected")
  }

  def chatTo(otherUser: User): Unit = {
    val chat = chatManager.createChat(s"$otherUser@$domain")
    chat.addMessageListener(printingMessageListener)
    chats += otherUser → chat
  }

  def printingMessageListener = new ChatMessageListener {
    override def processMessage(chat: Chat, message: Message): Unit = {
      computerSays(s"ChatMessageListener: received message for $chat : $message")
      messageListeners foreach { _ ! MessageReceived(chat, message) }
    }
  }
}

