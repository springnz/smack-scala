package ylabs.messaging

import Client.Messages._
import akka.actor.{ ActorSystem, Props }
import akka.pattern.ask
import akka.testkit.{ TestActorRef, TestProbe }
import akka.util.Timeout
import java.util.UUID
import org.jivesoftware.smack.packet.ExtensionElement
import org.scalatest.{ BeforeAndAfterEach, Matchers, WordSpec }
import scala.collection.JavaConversions._
import scala.concurrent.duration._
import scala.util.{ Success, Try }
import Client.{User, Password}

// this test depends on a running xmpp server (e.g. ejabberd) configured so that admin users can create unlimited users in your environment!
// see http://docs.ejabberd.im/admin/guide/configuration/#modregister for more details
@tags.RequiresEjabberd
class ClientTest extends WordSpec with Matchers with BeforeAndAfterEach {
  implicit var system: ActorSystem = _
  implicit val timeout = Timeout(5 seconds)

  val adminUsername = "admin"
  val adminPassword = "admin"

  "connects to the xmpp server" in new Fixture {
    val connected = adminUser ? Connect(User(adminUsername), Password(adminPassword))
    connected.value.get shouldBe Success(Connected)
    adminUser ! Disconnect
  }

  "allows user registration and deletion" in new Fixture {
    val username = randomUsername
    val userPass = Password (username.value)

    val connected = adminUser ? Connect(User(adminUsername), Password(adminPassword))
    adminUser ! RegisterUser(username, userPass)

    val connected1 = user1 ? Connect(username, userPass)
    connected1.value.get shouldBe Success(Connected)

    user1 ! DeleteUser
    val connected2 = user1 ? Connect(username, userPass)
    connected2.value.get.get match {
      case ConnectError(t) ⇒ //that's all we want to check
    }
  }

  "enables users to chat to each other" in new Fixture {
    withTwoUsers { case ((username1, user1Pass), (username2, user2Pass)) ⇒
      user1 ! Connect(username1, user1Pass)
      user2 ! Connect(username2, user2Pass)
      user2 ! RegisterMessageListener(messageListener.ref)

      user1 ! ChatTo(username2)
      val testMessage = "unique test message" + UUID.randomUUID
      user1 ! SendMessage(username2, testMessage)

      verifyMessageArrived(username1, username2, testMessage)
    }
  }

  "enables async chats (message recipient offline)" in new Fixture {
    withTwoUsers { case ((username1, user1Pass), (username2, user2Pass)) ⇒
      user1 ! Connect(username1, user1Pass)
      user2 ! RegisterMessageListener(messageListener.ref)

      user1 ! ChatTo(username2)
      val testMessage = "unique test message" + UUID.randomUUID
      user1 ! SendMessage(username2, testMessage)

      // yeah, sleeping is bad, but I dunno how else to make this guaranteed async.
      Thread.sleep(1000)
      user2 ! Connect(username2, user2Pass)

      verifyMessageArrived(username1, username2, testMessage)
    }
  }

  "file transmission" when {
    "both clients are available" in new Fixture {
      withTwoUsers { case ((username1, user1Pass), (username2, user2Pass)) ⇒
        user1 ! Connect(username1, user1Pass)
        user2 ! Connect(username2, user2Pass)
        user2 ! RegisterMessageListener(messageListener.ref)


        val fileUrl = "https://raw.githubusercontent.com/mpollmeier/gremlin-scala/master/README.md"
        user1 ! ChatTo(username2)
        user1 ! SendFileMessage(username2, fileUrl, Some("file description"))

        messageListener.expectMsgPF(3 seconds, "xep-0066 file transfer") {
          case MessageReceived(chat, message) ⇒
            chat.getParticipant should startWith(username1.value)
            message.getTo should startWith(username2.value)

            // pretty shitty of smack to take a type parameter there... they just cast it!
            val extension = message.getExtension[ExtensionElement](OutOfBandData.ElementName, OutOfBandData.XmlNamespace)
            val xml = extension.toXML.toString
            xml should include(fileUrl) // TODO: parse message properly, check whole schema etc.
          // TODO: assert file description aswell
        }
      }
    }
  }

  trait Fixture {
    val adminUser = TestActorRef(Props[Client])
    val user1 = TestActorRef(Props[Client])
    val user2 = TestActorRef(Props[Client])
    val messageListener = TestProbe()
    messageListener.ignoreMsg {
      case MessageReceived(_, message) ⇒ message.getSubject == "Welcome!"
    }

    def withTwoUsers(block: ((User, Password), (User, Password)) ⇒ Unit): Unit = {
      val username1 = randomUsername
      val user1Pass = Password(username1.value)
      val username2 = randomUsername
      val user2Pass = Password(username2.value)

      adminUser ! Connect(User(adminUsername), Password(adminPassword))
      adminUser ! RegisterUser(username1, Password(username1.value))
      adminUser ! RegisterUser(username2, Password(username2.value))

      try {
        block((username1, user1Pass), (username2, user2Pass))
      } finally {
        user1 ! DeleteUser
        user2 ! DeleteUser
      }
    }

    def verifyMessageArrived(sender: User, recipient: User, messageBody: String): Unit = {
      messageListener.fishForMessage(3 seconds, "expected message to be delivered") {
        case MessageReceived(chat, message) ⇒
          chat.getParticipant should startWith(sender.value)
          message.getTo should startWith(recipient.value)
          message.getBody shouldBe messageBody
          true
        case _ ⇒ false
      }
    }
  }

  def randomUsername = User(s"testuser-${UUID.randomUUID.toString.substring(9)}")

  override def beforeEach() {
    system = ActorSystem()
  }

  override def afterEach() {
    system.shutdown()
  }
}
