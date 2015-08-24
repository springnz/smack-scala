import org.jivesoftware.smack.ConnectionConfiguration.SecurityMode
import akka.actor.ActorSystem
import akka.actor.Actor
import akka.actor.Props
import org.jivesoftware.smack.chat._
import org.jivesoftware.smack.packet.Message
import org.jivesoftware.smack.tcp.XMPPTCPConnection
import org.jivesoftware.smack.tcp.XMPPTCPConnectionConfiguration
import collection.JavaConversions._

object ChatClient extends App {
  object Messages {
    case class Echo(message: String)
    object Shutdown
  }
  import Messages._

  case class Config(username: String, password: String)

  val parser = new scopt.OptionParser[Config]("scopt") {
    head("ChatClient")

    opt[String]('u', "username") required () action { (x, c) ⇒
      c.copy(username = x)
    } text ("username is a mandatory string")

    opt[String]('p', "password") required () action { (x, c) ⇒
      c.copy(password = x)
    } text ("password is a mandatory string")
  }

  parser.parse(args, Config("req", "req")) match {
    case None ⇒
    case Some(config) ⇒
      val host = "akllap015.corp"
      val xmppConfigBuilder = XMPPTCPConnectionConfiguration.builder
        .setUsernameAndPassword(config.username, config.password)
        .setServiceName("corp")
        .setHost(host)
        .setSecurityMode(SecurityMode.disabled)
        .build

      println(s"connecting to $host using ${config.username} / ${config.password}")
      new ChatClient(xmppConfigBuilder)
  }

  // val system = ActorSystem()
  // val chattie = system.actorOf(Props(classOf[ChatActor], "blub"), "chatClient")

  // chattie ! Echo("test")
  // chattie ! Shutdown
}

class ChatActor(xmppConfig: String) extends Actor {
  import ChatClient.Messages._

  override def receive = {
    case Echo(message) => println(message)

    case Shutdown =>
      println("shutting down system")
      context.system.shutdown()
  }
}


class ChatClient(xmppConfig: XMPPTCPConnectionConfiguration) {
  val connection = new XMPPTCPConnection(xmppConfig)
  connection.connect().login()

  val chatManager = ChatManager.getInstanceFor(connection)
  chatManager.addChatListener(new ChatManagerListener() {
    override def chatCreated(chat: Chat, createdLocally: Boolean): Unit = {
      println(s"ChatManagerListener: chat created: $chat; locally: $createdLocally")
    }
  })

  def chatTo(otherUser: String): Unit = {
    val chat = chatManager.createChat(otherUser)
    chat.addMessageListener(new ChatMessageListener() {
      override def processMessage(chat: Chat, message: Message): Unit = {
        println(s"ChatMessageListener: received message for $chat : $message")
      }
    })
    chat.sendMessage(s"hello $otherUser")
  }

  // chatTo("admin5@corp")

  Thread.sleep(1000)
  connection.disconnect()
  println("done")
}
