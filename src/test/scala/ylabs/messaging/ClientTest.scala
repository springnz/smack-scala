package ylabs.messaging

import akka.actor.ActorSystem
import akka.actor.Props
import akka.util.Timeout
import org.scalatest.BeforeAndAfterEach
import org.scalatest.{ Matchers, WordSpec }
import scala.collection.JavaConversions._
import akka.testkit.TestActorRef
import Client.Messages._
import akka.pattern.ask
import concurrent.duration._
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
    client1 ! Connect(user1, pass1)
    client2 ! Connect(user2, pass2)

    client1 ! ChatTo(user2)

    val message = "test message"
    client1 ! SendMessage(user2, message)

    // TODO: await message at client 2
    Thread.sleep(1000)

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
