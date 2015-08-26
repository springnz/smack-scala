package ylabs.messaging

import Client.Messages._
import akka.actor.{ ActorSystem, Props }
import akka.pattern.ask
import akka.testkit.{ TestActorRef, TestProbe }
import akka.util.Timeout
import org.scalatest.{ BeforeAndAfterEach, Matchers, WordSpec }
import scala.collection.JavaConversions._
import scala.concurrent.duration._
import scala.util.Success

// this test depends on a running xmpp server (e.g. ejabberd) in your environment!
@tags.RequiresEjabberd
class ClientTest extends WordSpec with Matchers with BeforeAndAfterEach {
  implicit var system: ActorSystem = _
  implicit val timeout = Timeout(5 seconds)

  // TODO: hand host and domain into client
  val user1 = "admin4"
  val pass1 = "admin4"
  val user2 = "admin5"
  val pass2 = "admin5"

  "connects to the xmpp server" in new Fixture {
    val connected = client1 ? Connect(user1, pass1)
    connected.value.get shouldBe Success(Connected)
    client1 ! Disconnect
  }

  "chats to other users" in new Fixture {
    val messageListener = TestProbe()
    client2 ! RegisterMessageListener(messageListener.ref)

    client1 ! Connect(user1, pass1)
    client2 ! Connect(user2, pass2)
    client1 ! ChatTo(user2)
    val testMessage = "test message"
    client1 ! SendMessage(user2, testMessage)

    messageListener.expectMsgPF(3 seconds, "expected message to be delivered") {
      case MessageReceived(chat, message) ⇒
        chat.getParticipant shouldBe s"$user1@${Client.domain}/Smack"
        message.getTo shouldBe s"$user2@${Client.domain}"
        message.getBody shouldBe testMessage
        true
    }

    client1 ! Disconnect
    client2 ! Disconnect
  }

  trait Fixture {
    val client1 = TestActorRef(Props[Client])
    val client2 = TestActorRef(Props[Client])
  }

  override def beforeEach() {
    system = ActorSystem()
  }

  override def afterEach() {
    system.shutdown()
  }
}
