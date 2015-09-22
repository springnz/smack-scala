package smack.scala.extensions

import org.jivesoftware.smack.packet.Message
import org.joda.time.format.ISODateTimeFormat
import smack.scala.Client.{ MessageId, User, Domain, UserWithDomain }
import org.scalatest.{ Matchers, WordSpec }
import scala.util.Success

class MAMResponseTest extends WordSpec with Matchers {

  "Message serialization having" when {

    "is ok" in {
      val res = <result id="testId" xmlns="urn:xmpp:mam:0">
                  <forwarded xmlns="urn:xmpp:forward:0">
                    <delay stamp="2010-06-07T12:00:00.000+12:00" xmlns="urn:xmpp:delay"/>
                    <message to='test@test.com' id='NY6C0-1'><body>test string</body></message>
                  </forwarded>
                </result>.toString
      MAMResponse.fromXml(res).get
    }

    "should succeed" in {
      val message = new Message("test@test.com", "test string")
      val dt = ISODateTimeFormat.dateTimeParser().parseDateTime("2010-06-07T00:00:00Z")
      val res = MAMResponse(message, dt, "testId")
      MAMResponse.fromXml(res.toXML).toString shouldBe Success(res).toString
    }
  }

  "XML parsing" when {
    "has formation errors handles" should {
      "wrong tag" in {
        val notIq = "<bresult></bresult>"
        MAMResponse.fromXml(notIq).failed.get.getMessage should include("message must be type result")
      }

      "missing xmlns" in {
        val notIq = "<result></result>"
        MAMResponse.fromXml(notIq).failed.get.getMessage should include(s"message must be in namespace ${MAMResponse.XmlNamespace}")
      }

      "wrong xmlns" in {
        val notIq = <result xmlns="bad"></result>.toString
        MAMResponse.fromXml(notIq).failed.get.getMessage should include(s"message must be in namespace ${MAMResponse.XmlNamespace}")
      }

      "missing id" in {
        val notIq = <result xmlns="urn:xmpp:mam:0"></result>.toString
        MAMResponse.fromXml(notIq).failed.get.getMessage should include(s"message must have id")
      }

      "missing forwarded" in {
        val notIq = <result xmlns="urn:xmpp:mam:0" id="msgId"></result>.toString
        MAMResponse.fromXml(notIq).failed.get.getMessage should include(s"message must have forwarded tag")
      }

      "missing forwarded xmlns" in {
        val notIq = <result xmlns="urn:xmpp:mam:0" id="msgId"><forwarded></forwarded></result>.toString
        MAMResponse.fromXml(notIq).failed.get.getMessage should include(s"forward tag must have namespace ${MAMResponse.ForwardedNamespace}")
      }

      "bad forwarded xmlns" in {
        val notIq = <result xmlns="urn:xmpp:mam:0" id="msgId"><forwarded xmlns="bad"></forwarded></result>.toString
        MAMResponse.fromXml(notIq).failed.get.getMessage should include(s"forward tag must have namespace ${MAMResponse.ForwardedNamespace}")
      }

      "missing delay tag" in {
        val notIq = <result xmlns="urn:xmpp:mam:0" id="msgId"><forwarded xmlns="urn:xmpp:forward:0"></forwarded></result>.toString
        MAMResponse.fromXml(notIq).failed.get.getMessage should include(s"message must have delay tag")
      }

      "missing delay xmlns" in {
        val notIq = <result xmlns="urn:xmpp:mam:0" id="msgId"><forwarded xmlns="urn:xmpp:forward:0"><delay></delay></forwarded></result>.toString
        MAMResponse.fromXml(notIq).failed.get.getMessage should include(s"delay tag must have namespace ${MAMResponse.DelayNamespace}")
      }

      "bad delay xmlns" in {
        val notIq = <result xmlns="urn:xmpp:mam:0" id="msgId"><forwarded xmlns="urn:xmpp:forward:0"><delay xmlns="bad"></delay></forwarded></result>.toString
        MAMResponse.fromXml(notIq).failed.get.getMessage should include(s"delay tag must have namespace ${MAMResponse.DelayNamespace}")
      }

      "missing stamp" in {
        val notIq = <result xmlns="urn:xmpp:mam:0" id="msgId"><forwarded xmlns="urn:xmpp:forward:0"><delay xmlns="urn:xmpp:delay"></delay></forwarded></result>.toString
        MAMResponse.fromXml(notIq).failed.get.getMessage should include(s"stamp must be set in delay")
      }

      "invalid stamp" in {
        val notIq = <result xmlns="urn:xmpp:mam:0" id="msgId"><forwarded xmlns="urn:xmpp:forward:0"><delay xmlns="urn:xmpp:delay" stamp="bad"></delay></forwarded></result>.toString
        MAMResponse.fromXml(notIq).failed.get.getMessage should include(s"stamp format was not ISO")
      }

      "missing message" in {
        val notIq = <result xmlns="urn:xmpp:mam:0" id="msgId"><forwarded xmlns="urn:xmpp:forward:0"><delay xmlns="urn:xmpp:delay" stamp="2010-07-10T23:08:25Z"></delay></forwarded></result>.toString
        MAMResponse.fromXml(notIq).failed.get.getMessage should include(s"message missing")
      }

    }
  }
}