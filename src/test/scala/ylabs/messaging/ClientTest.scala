package ylabs.messaging

import org.scalatest.{ Matchers, WordSpec }
import scala.collection.JavaConversions._

// this test depends on a running xmpp server (e.g. ejabberd) in your environment!
@tags.RequiresEjabberd
class ClientTest extends WordSpec with Matchers {

  "connects" in new Fixture {
  }

  // "blubs" taggedAs(org.scalatest.Tag("foo")) in {

  // }

  trait Fixture {
  }
}
