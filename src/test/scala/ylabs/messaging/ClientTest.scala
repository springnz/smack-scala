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
import Client.User

// this test depends on a running xmpp server (e.g. ejabberd) configured so that admin users can create unlimited users in your environment!
// see http://docs.ejabberd.im/admin/guide/configuration/#modregister for more details
@tags.RequiresEjabberd
class ClientTest extends WordSpec with Matchers with BeforeAndAfterEach {
  implicit var system: ActorSystem = _
  implicit val timeout = Timeout(5 seconds)

  val adminUsername = "admin"
  val adminPassword = "admin"
  def randomUsername = s"testuser-${UUID.randomUUID.toString.substring(9)}"

  "connects to the xmpp server" in new Fixture {
    val connected = adminUser ? Connect(adminUsername, adminPassword)
    connected.value.get shouldBe Success(Connected)
    adminUser ! Disconnect
  }

  "allows user registration and deletion" in new Fixture {
    val username = randomUsername

    adminUser ! Connect(adminUsername, adminPassword)
    adminUser ! RegisterUser(username, password = username)

    val connected1 = user1 ? Connect(username, password = username)
    connected1.value.get shouldBe Success(Connected)

    user1 ! DeleteUser
    val connected2 = user1 ? Connect(username, password = username)
    connected2.value.get.get match {
      case ConnectError(t) ⇒ //that's all we want to check
    }
  }

  "enables users to chat to each other" in new Fixture {
    withTwoUsers { (username1, username2) ⇒
      user1 ! Connect(username1, password = username1)
      user2 ! Connect(username2, password = username2)
      user2 ! RegisterMessageListener(messageListener.ref)

      user1 ! ChatTo(username2)
      val testMessage = "unique test message" + UUID.randomUUID
      user1 ! SendMessage(username2, testMessage)

      verifyMessageArrived(username1, username2, testMessage)
    }
  }

  "enables async chats (message recipient offline)" in new Fixture {
    withTwoUsers { (username1, username2) ⇒
      user1 ! Connect(username1, password = username1)
      user2 ! RegisterMessageListener(messageListener.ref)

      user1 ! ChatTo(username2)
      val testMessage = "unique test message" + UUID.randomUUID
      user1 ! SendMessage(username2, testMessage)

      // yeah, sleeping is bad, but I dunno how else to make this guaranteed async.
      Thread.sleep(1000)
      user2 ! Connect(username2, password = username2)

      verifyMessageArrived(username1, username2, testMessage)
    }
  }

  "file transmission" when {
    "both clients are available" taggedAs (org.scalatest.Tag("foo")) in new Fixture {
      withTwoUsers { (username1, username2) ⇒
        user1 ! Connect(username1, password = username1)
        user2 ! Connect(username2, password = username2)
        user2 ! RegisterMessageListener(messageListener.ref)


        val fileUrl = "https://raw.githubusercontent.com/mpollmeier/gremlin-scala/master/README.md"
        user1 ! ChatTo(username2)
        user1 ! SendFileMessage(username2, fileUrl, Some("file description"))

        messageListener.expectMsgPF(3 seconds, "xep-0066 file transfer") {
          case MessageReceived(chat, message) ⇒
            chat.getParticipant should startWith(username1)
            message.getTo should startWith(username2)

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

    def withTwoUsers(block: (User, User) ⇒ Unit): Unit = {
      val username1 = randomUsername
      val username2 = randomUsername

      adminUser ! Connect(adminUsername, adminPassword)
      adminUser ! RegisterUser(username1, password = username1)
      adminUser ! RegisterUser(username2, password = username2)

      try {
        block(username1, username2)
      } finally {
        user1 ! DeleteUser
        user2 ! DeleteUser
      }
    }

    def verifyMessageArrived(sender: User, recipient: User, messageBody: String): Unit = {
      messageListener.fishForMessage(3 seconds, "expected message to be delivered") {
        case MessageReceived(chat, message) ⇒
          chat.getParticipant should startWith(sender)
          message.getTo should startWith(recipient)
          message.getBody shouldBe messageBody
          true
        case _ ⇒ false
      }
    }
  }

  override def beforeEach() {
    system = ActorSystem()
  }

  override def afterEach() {
    system.shutdown()
  }
}
