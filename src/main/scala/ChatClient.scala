import org.jivesoftware.smack.ConnectionConfiguration.SecurityMode
import akka.actor.ActorSystem
import akka.actor.Actor
import akka.actor.Props
import org.jivesoftware.smack.chat._
import org.jivesoftware.smack.packet.Message
import org.jivesoftware.smack.tcp.XMPPTCPConnection
import org.jivesoftware.smack.tcp.XMPPTCPConnectionConfiguration
import collection.JavaConversions._
import akka.pattern.ask
import akka.util.Timeout
import concurrent.duration._
import concurrent.Await

object ChatClient extends App {
  object Messages {
    case class Connect(username: String, password: String)
    object Shutdown
    object Connected
  }
  import Messages._
  implicit val timeout = Timeout(5 seconds)

  val host = "akllap015.corp"
  val system = ActorSystem()
  val chattie = system.actorOf(Props[ChatActor], "chatClient")

  var on = true
  while (on) {
    println("What next?")

    io.StdIn.readLine match {
      case "connect" ⇒
        // println("username: "); val username = io.StdIn.readLine
        // println("password: "); val password = io.StdIn.readLine
        val username = "admin4"
        val password = "admin4"
        Await.ready(chattie ? Connect(username, password), 5 seconds)
        println("connected")

      case "exit" ⇒
        chattie ! Shutdown
        on = false

      case _ ⇒ println("Que? No comprendo. Try again!")
    }
  }
}

class ChatActor extends Actor {
  import ChatClient.Messages._

  var connection: XMPPTCPConnection = _

  override def receive = {
    case c: Connect ⇒
      connect(c)
      sender ! Connected

    case Shutdown ⇒
      println("shutting down")
      disconnect()
      context.system.shutdown()
  }

  def connect(connect: Connect) = {
    disconnect()
    connection = new XMPPTCPConnection(
      XMPPTCPConnectionConfiguration.builder
      .setUsernameAndPassword(connect.username, connect.password)
      .setServiceName("corp")
      .setHost(ChatClient.host)
      .setSecurityMode(SecurityMode.disabled)
      .build
    )
    connection.connect().login()

    val chatManager = ChatManager.getInstanceFor(connection)
    chatManager.addChatListener(new ChatManagerListener() {
      override def chatCreated(chat: Chat, createdLocally: Boolean): Unit = {
        println(s"ChatManagerListener: chat created: $chat; locally: $createdLocally")
      }
    })
  }

  def disconnect() = Option(connection).map(_.disconnect())
}

//   def chatTo(otherUser: String): Unit = {
//     val chat = chatManager.createChat(otherUser)
//     chat.addMessageListener(new ChatMessageListener() {
//       override def processMessage(chat: Chat, message: Message): Unit = {
//         println(s"ChatMessageListener: received message for $chat : $message")
//       }
//     })
//     chat.sendMessage(s"hello $otherUser")
//   }
