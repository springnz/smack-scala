package ylabs.messaging

import Client.Messages._
import akka.actor.{ ActorSystem, Props }
import akka.pattern.ask
import akka.testkit.{ TestActorRef, TestProbe }
import akka.util.Timeout
import java.util.UUID
import org.jivesoftware.smackx.iqregister.packet.Registration
import org.scalatest.{ BeforeAndAfterEach, Matchers, WordSpec }
import scala.collection.JavaConversions._
import scala.concurrent.duration._
import scala.util.Failure
import scala.util.Success

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

  "user registration and deletion" in new Fixture {
    val user = randomUsername

    adminUser ! Connect(adminUsername, adminPassword)
    adminUser ! RegisterUser(user, password = user)

    val connected1 = user1 ? Connect(user, password = user)
    connected1.value.get shouldBe Success(Connected)

    user1 ! DeleteUser
    val connected2 = user1 ? Connect(user, password = user)
    connected2.value.get.get match {
      case ConnectError(t) => //that's all we want to check
    }
  }

  // "chats to other users" in new Fixture {
  //   val messageListener = TestProbe()
  //   client2 ! RegisterMessageListener(messageListener.ref)

  //   client1 ! Connect(user1, pass1)
  //   client2 ! Connect(user2, pass2)
  //   client1 ! ChatTo(user2)
  //   val testMessage = "test message"
  //   client1 ! SendMessage(user2, testMessage)

  //   messageListener.expectMsgPF(3 seconds, "expected message to be delivered") {
  //     case MessageReceived(chat, message) ⇒
  //       chat.getParticipant shouldBe s"$user1@$domain/Smack"
  //       message.getTo shouldBe s"$user2@$domain"
  //       message.getBody shouldBe testMessage
  //       true
  //   }

  //   client1 ! Disconnect
  //   client2 ! Disconnect
  // }

  // "allows async chats" in new Fixture {
  //   val messageListener = TestProbe()
  //   client2 ! RegisterMessageListener(messageListener.ref)

  //   client1 ! Connect(user1, pass1)
  //   // client2 is not connected
  //   client1 ! ChatTo(user2)
  //   val testMessage = "async test message"
  //   client1 ! SendMessage(user2, testMessage)

  //   // sleep is bad, but I dunno how else to make this guaranteed async. 
  //   Thread.sleep(1000)
  //   client2 ! Connect(user2, pass2)

  //   messageListener.expectMsgPF(3 seconds, "expected message to be delivered") {
  //     case MessageReceived(chat, message) ⇒
  //       chat.getParticipant shouldBe s"$user1@$domain/Smack"
  //       message.getTo shouldBe s"$user2@$domain"
  //       message.getBody shouldBe testMessage
  //       true
  //   }

  //   client1 ! Disconnect
  //   client2 ! Disconnect
  // }

  trait Fixture {
    val adminUser = TestActorRef(Props[Client])
    val user1 = TestActorRef(Props[Client])
    val user2 = TestActorRef(Props[Client])

    // def withOneUser(block: () => Unit): Unit {

    // }
  }

  override def beforeEach() {
    system = ActorSystem()
  }

  override def afterEach() {
    system.shutdown()
  }
}
