package smack.scala.extensions

import smack.scala.Client.MessageId
import org.jivesoftware.smack.packet.Message
import org.joda.time.format.ISODateTimeFormat
import org.scalatest.{ Matchers, WordSpec }
import scala.util.Success

class MAMFinTest extends WordSpec with Matchers {

  "Message serialization having" when {

    "should succeed" in {
      val res = MAMFin(Some(MessageId("Test Id")), Some(MessageId("Last Id")), Some(10), Some(20))
      MAMFin.fromXml(res.toXML).toString shouldBe Success(res).toString
    }

    "should succeed with just complete" in {
      val res = MAMFin()
      MAMFin.fromXml(res.toXML).toString shouldBe Success(res).toString
    }

  }

  "XML parsing" when {
    "has formation errors handles" should {
      "wrong tag" in {
        val notIq = "<bfin></bfin>"
        MAMFin.fromXml(notIq).failed.get.getMessage should include("message must be type fin")
      }

      "missing xmlns" in {
        val notIq = "<fin></fin>"
        MAMFin.fromXml(notIq).failed.get.getMessage should include("message must be in namespace urn:xmpp:mam:0")
      }

      "bad xmlns" in {
        val notIq = """<fin xmlns="bad"></fin>"""
        MAMFin.fromXml(notIq).failed.get.getMessage should include("message must be in namespace urn:xmpp:mam:0")
      }

      "complete not set" in {
        val notIq = """<fin xmlns="urn:xmpp:mam:0"></fin>"""
        MAMFin.fromXml(notIq).failed.get.getMessage should include("message must have complete set")
      }

      "complete bad value" in {
        val notIq = """<fin xmlns="urn:xmpp:mam:0" complete="bad"></fin>"""
        MAMFin.fromXml(notIq).failed.get.getMessage should include("message must have complete set to a boolean")
      }

      "missing set tag" in {
        val notIq = """<fin xmlns="urn:xmpp:mam:0" complete="true"></fin>"""
        MAMFin.fromXml(notIq).failed.get.getMessage should include("message must have set tag")
      }

      "missing set xmlns" in {
        val notIq = """<fin xmlns="urn:xmpp:mam:0" complete="true"><set></set></fin>"""
        MAMFin.fromXml(notIq).failed.get.getMessage should include("set tag must be in namespace http://jabber.org/protocol/rsm")
      }

      "bad set xmlns" in {
        val notIq = """<fin xmlns="urn:xmpp:mam:0" complete="true"><set xmlns="bad"></set></fin>"""
        MAMFin.fromXml(notIq).failed.get.getMessage should include("set tag must be in namespace http://jabber.org/protocol/rsm")
      }

      "bad first tag" in {
        val notIq =
          <fin xmlns="urn:xmpp:mam:0" complete="true">
            <set xmlns="http://jabber.org/protocol/rsm">
              <first></first>
            </set>
          </fin>.toString()
        MAMFin.fromXml(notIq).failed.get.getMessage should include(""""first" tag must have index attribute of int""")
      }

      "empty first id" in {
        val notIq =
          <fin xmlns="urn:xmpp:mam:0" complete="true">
            <set xmlns="http://jabber.org/protocol/rsm">
              <first index="10"></first>
            </set>
          </fin>.toString()
        MAMFin.fromXml(notIq).failed.get.getMessage should include(""""first" tag must have message id text""")
      }

      "empty last id" in {
        val notIq =
          <fin xmlns="urn:xmpp:mam:0" complete="true">
            <set xmlns="http://jabber.org/protocol/rsm">
              <first index="10">Test ID</first>
              <last></last>
            </set>
          </fin>.toString()
        MAMFin.fromXml(notIq).failed.get.getMessage should include(""""last" tag must have message id text""")
      }

      "empty count" in {
        val notIq =
          <fin xmlns="urn:xmpp:mam:0" complete="true">
            <set xmlns="http://jabber.org/protocol/rsm">
              <first index="10">Test ID</first>
              <last>Another Test ID</last>
              <count></count>
            </set>
          </fin>.toString()
        MAMFin.fromXml(notIq).failed.get.getMessage should include(""""count" tag must have int value""")
      }

    }
  }
}