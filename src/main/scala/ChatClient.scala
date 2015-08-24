import org.jivesoftware.smack.ConnectionConfiguration.SecurityMode
import akka.actor.ActorSystem
import akka.actor.Actor
import akka.actor.Props
import org.jivesoftware.smack.chat._
import org.jivesoftware.smack.packet.Message
import org.jivesoftware.smack.tcp.XMPPTCPConnection
import org.jivesoftware.smack.tcp.XMPPTCPConnectionConfiguration
import collection.JavaConversions._
import collection.mutable

object ChatClient extends App {
  type User = String
  object Messages {
    case class Connect(username: String, password: String)
    case class ChatTo(otherUser: User)
    case class SendMessage(otherUser: User, message: String)
    case class LeaveChat(otherUser: User)
    object Disconnect
    object Shutdown
  }
  import Messages._

  val host = "akllap015.corp"
  val domain = "corp"
  val system = ActorSystem()
  val chattie = system.actorOf(Props[ChatActor], "chatClient")

  var on = true
  computerSays("What now?")
  while (on) {
    io.StdIn.readLine match {
      case "connect" ⇒
        println("username: "); val username = io.StdIn.readLine
        val password = username
        // println("password: "); val password = io.StdIn.readLine
        // val username = "admin5"
        // val password = "admin5"
        chattie ! Connect(username, password)

      case "openchat" ⇒
        computerSays("who may i connect you with, sir?")
        val user = io.StdIn.readLine
        chattie ! ChatTo(user)

      case "message" ⇒
        computerSays("who do you want to send a message to, sir?")
        val user = io.StdIn.readLine
        computerSays("what's your message, sir?")
        val message = io.StdIn.readLine
        chattie ! SendMessage(user, message)

      case "leavechat" ⇒
        computerSays("who may i disconnect you from, sir?")
        val user = io.StdIn.readLine
        chattie ! LeaveChat(user)

      case "disconnect" ⇒
        chattie ! Disconnect

      case "exit" ⇒
        computerSays("shutting down")
        chattie ! Shutdown
        on = false

      case _ ⇒ computerSays("Que? No comprendo. Try again, sir!")
    }
  }

  def computerSays(s: String) = println(s">> $s")
}

class ChatActor extends Actor {
  import ChatClient._
  import ChatClient.Messages._
  import context._

  var connection: XMPPTCPConnection = _
  var chatManager: ChatManager = _
  val chats = mutable.Map.empty[User, Chat]

  override def receive = {
    case c: Connect ⇒
      connect(c)
      become(connected)

    case Shutdown ⇒ context.system.shutdown()
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

    case Connect ⇒ computerSays("already connected!")
  }

  def connect(connect: Connect) = {
    connection = new XMPPTCPConnection(
      XMPPTCPConnectionConfiguration.builder
      .setUsernameAndPassword(connect.username, connect.password)
      .setServiceName("corp")
      .setHost(ChatClient.host)
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
    }
  }
}

