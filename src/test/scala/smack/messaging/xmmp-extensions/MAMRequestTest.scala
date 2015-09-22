package smack.scala.extensions

import org.joda.time.format.ISODateTimeFormat
import smack.scala.Client.{ MessageId, User, Domain, UserWithDomain }
import org.scalatest.{ Matchers, WordSpec }
import scala.util.Success

class MAMRequestTest extends WordSpec with Matchers {

  "Message serialization having" when {

    "user" in {
      val req = MAMRequest(UserWithDomain("test@test.com"))
      MAMRequest.fromXml(req.toXML) shouldBe Success(req)
    }

    "start" in {
      val start = ISODateTimeFormat.dateTimeParser().parseDateTime("2010-06-07T00:00:00Z")
      val req = MAMRequest(UserWithDomain("test@test.com"), start = Some(start))
      MAMRequest.fromXml(req.toXML) shouldBe Success(req)
    }

    "start and end" in {
      val start = ISODateTimeFormat.dateTimeParser().parseDateTime("2010-06-07T00:00:00Z")
      val end = ISODateTimeFormat.dateTimeParser().parseDateTime("2010-07-07T00:00:00Z")
      val req = MAMRequest(UserWithDomain("test@test.com"), start = Some(start), end = Some(end))
      MAMRequest.fromXml(req.toXML) shouldBe Success(req)
    }

    "limit" in {
      val req = MAMRequest(UserWithDomain("test@test.com"), limit = Some(15))
      MAMRequest.fromXml(req.toXML) shouldBe Success(req)
    }

    "skipTo" in {
      val req = MAMRequest(UserWithDomain("test@test.com"), skipTo = Some(MessageId("skipId")))
      MAMRequest.fromXml(req.toXML) shouldBe Success(req)
    }

    "everything" in {
      val start = ISODateTimeFormat.dateTimeParser().parseDateTime("2010-06-07T00:00:00Z")
      val end = ISODateTimeFormat.dateTimeParser().parseDateTime("2010-07-07T00:00:00Z")
      val req = MAMRequest(UserWithDomain("test@test.com"), start = Some(start), end = Some(end), limit = Some(15), skipTo = Some(MessageId("skipId")))
      MAMRequest.fromXml(req.toXML) shouldBe Success(req)
    }
  }

  "XML parsing" when {
    "correct" should {
      "have user name" in {
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x xmlns="jabber:x:data" type="submit">
                                                         <field var="FORM_TYPE" type="hidden"><value>urn:xmpp:mam:0</value></field>
                                                         <field var="with"><value>test@testing.com</value></field>
                                                       </x>
                                                     </query></iq>.toString
        MAMRequest.fromXml(missingData).get.user.value shouldBe "test@testing.com"
      }

      "have start date" in {
        val start = "2010-06-07T00:00:00Z"
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x xmlns="jabber:x:data" type="submit">
                                                         <field var="FORM_TYPE" type="hidden"><value>urn:xmpp:mam:0</value></field>
                                                         <field var="with"><value>test@testing.com</value></field>
                                                         <field var="start"><value>{ start }</value></field>
                                                       </x>
                                                     </query></iq>.toString
        MAMRequest.fromXml(missingData).get.start shouldBe Some(ISODateTimeFormat.dateTimeParser().parseDateTime(start))
        MAMRequest.fromXml(missingData).get.end shouldBe None
      }

      "have end date" in {
        val start = "2010-06-07T00:00:00Z"
        val end = "2010-07-07T13:23:54Z"
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x xmlns="jabber:x:data" type="submit">
                                                         <field var="FORM_TYPE" type="hidden"><value>urn:xmpp:mam:0</value></field>
                                                         <field var="with"><value>test@testing.com</value></field>
                                                         <field var="start"><value>{ start }</value></field>
                                                         <field var="end"><value>{ end }</value></field>
                                                       </x>
                                                     </query></iq>.toString
        MAMRequest.fromXml(missingData).get.start shouldBe Some(ISODateTimeFormat.dateTimeParser().parseDateTime(start))
        MAMRequest.fromXml(missingData).get.end shouldBe Some(ISODateTimeFormat.dateTimeParser().parseDateTime(end))
      }

      "have max" in {
        val max = 15
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x xmlns="jabber:x:data" type="submit">
                                                         <field var="FORM_TYPE" type="hidden"><value>urn:xmpp:mam:0</value></field>
                                                         <field var="with"><value>test@testing.com</value></field>
                                                       </x>
                                                       <set xmlns="http://jabber.org/protocol/rsm">
                                                         <max>{ max }</max>
                                                       </set>
                                                     </query></iq>.toString
        MAMRequest.fromXml(missingData).get.limit shouldBe Some(max)
      }

      "has after" in {
        val after = "testid"
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x xmlns="jabber:x:data" type="submit">
                                                         <field var="FORM_TYPE" type="hidden"><value>urn:xmpp:mam:0</value></field>
                                                         <field var="with"><value>test@testing.com</value></field>
                                                       </x>
                                                       <set xmlns="http://jabber.org/protocol/rsm">
                                                         <after>{ after }</after>
                                                       </set>
                                                     </query></iq>.toString
        MAMRequest.fromXml(missingData).get.skipTo shouldBe Some(MessageId(after))
      }
    }

    "has formation errors handles" should {
      "wrong tag" in {
        val notIq = "<biq></biq>"
        MAMRequest.fromXml(notIq).failed.get.getMessage should include("message must be iq type")
      }

      "missing iq type" in {
        val notType = "<iq></iq>"
        MAMRequest.fromXml(notType).failed.get.getMessage should include("""iq attr "type" must be equal to "set"""")
      }

      "incorrect iq type" in {
        val notCorrectType = """<iq type="error"></iq>"""
        MAMRequest.fromXml(notCorrectType).failed.get.getMessage should include("""iq attr "type" must be equal to "set"""")
      }

      "missing id" in {
        val missingId = """<iq type="set"></iq>"""
        MAMRequest.fromXml(missingId).failed.get.getMessage should include("""iq attr "id" must be set""")
      }

      "missing query" in {
        val missingQuery = """<iq type="set" id="testId"></iq>"""
        MAMRequest.fromXml(missingQuery).failed.get.getMessage should include("there must be one query element")
      }

      "query missing xmlns" in {
        val missingXmlns = """<iq type="set" id="testId"><query></query></iq>"""
        MAMRequest.fromXml(missingXmlns).failed.get.getMessage should include(s"query namespace must be ${MAMRequest.XmlNamespace}")
      }

      "query wrong xmlns" in {
        val wrongXmlns = """<iq type="set" id="testId"><query xmlns="wrong"></query></iq>"""
        MAMRequest.fromXml(wrongXmlns).failed.get.getMessage should include(s"query namespace must be ${MAMRequest.XmlNamespace}")
      }

      "missing data element" in {
        val missingData = """<iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0"></query></iq>"""
        MAMRequest.fromXml(missingData).failed.get.getMessage should include(s"there must be one data element")
      }

      "missing data xmlns" in {
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x></x>
                                                     </query></iq>.toString
        MAMRequest.fromXml(missingData).failed.get.getMessage should include(s"data element x namespace must be ${MAMRequest.DataNamespace}")
      }

      "wrong data xmlns" in {
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x xmlns="bad"></x>
                                                     </query></iq>.toString
        MAMRequest.fromXml(missingData).failed.get.getMessage should include(s"data element x namespace must be ${MAMRequest.DataNamespace}")
      }

      "missing data type" in {
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x xmlns="jabber:x:data"></x>
                                                     </query></iq>.toString
        MAMRequest.fromXml(missingData).failed.get.getMessage should include(s"""data element x attr "type" must be "submit"""")
      }

      "wrong data type" in {
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x xmlns="jabber:x:data" type="bad"></x>
                                                     </query></iq>.toString
        MAMRequest.fromXml(missingData).failed.get.getMessage should include(s"""data element x attr "type" must be "submit"""")
      }

      "missing FORM_TYPE field" in {
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x xmlns="jabber:x:data" type="submit">
                                                       </x>
                                                     </query></iq>.toString
        MAMRequest.fromXml(missingData).failed.get.getMessage should include(s"""field with attr "var" equal to "FORM_TYPE" must be set""")
      }

      "missing FORM_TYPE type" in {
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x xmlns="jabber:x:data" type="submit">
                                                         <field var="FORM_TYPE"><value>urn:xmpp:mam:0</value></field>
                                                       </x>
                                                     </query></iq>.toString
        MAMRequest.fromXml(missingData).failed.get.getMessage should include(s"""FORM_TYPE field must have attr "type" equal to "hidden"""")
      }

      "bad FORM_TYPE type" in {
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x xmlns="jabber:x:data" type="submit">
                                                         <field var="FORM_TYPE" type="bad"><value>urn:xmpp:mam:0</value></field>
                                                       </x>
                                                     </query></iq>.toString
        MAMRequest.fromXml(missingData).failed.get.getMessage should include(s"""FORM_TYPE field must have attr "type" equal to "hidden"""")
      }

      "missing FORM_TYPE value" in {
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x xmlns="jabber:x:data" type="submit">
                                                         <field var="FORM_TYPE" type="hidden"></field>
                                                       </x>
                                                     </query></iq>.toString
        MAMRequest.fromXml(missingData).failed.get.getMessage should include(s"""FORM_TYPE field must have value equal to "urn:xmpp:mam:0"""")
      }

      "bad FORM_TYPE value" in {
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x xmlns="jabber:x:data" type="submit">
                                                         <field var="FORM_TYPE" type="hidden"><value>badvalue</value></field>
                                                       </x>
                                                     </query></iq>.toString
        MAMRequest.fromXml(missingData).failed.get.getMessage should include(s"""FORM_TYPE field must have value equal to "urn:xmpp:mam:0"""")
      }

      "missing user" in {
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x xmlns="jabber:x:data" type="submit">
                                                         <field var="FORM_TYPE" type="hidden"><value>urn:xmpp:mam:0</value></field>
                                                       </x>
                                                     </query></iq>.toString
        MAMRequest.fromXml(missingData).failed.get.getMessage should include(s"""field with attr "var" equal to "with" must be set""")
      }

      "missing user value" in {
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x xmlns="jabber:x:data" type="submit">
                                                         <field var="FORM_TYPE" type="hidden"><value>urn:xmpp:mam:0</value></field>
                                                         <field var="with"></field>
                                                       </x>
                                                     </query></iq>.toString
        MAMRequest.fromXml(missingData).failed.get.getMessage should include(s""""with" field must have value set""")
      }

      "have missing set xmlns" in {
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x xmlns="jabber:x:data" type="submit">
                                                         <field var="FORM_TYPE" type="hidden"><value>urn:xmpp:mam:0</value></field>
                                                         <field var="with"><value>test@test.com</value></field>
                                                       </x>
                                                       <set>
                                                       </set>
                                                     </query></iq>.toString
        MAMRequest.fromXml(missingData).failed.get.getMessage should include(s""""set" element namespace must have value equal to {MAMRequest.PagingNamespace}""")
      }

      "have incorrect set xmlns" in {
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x xmlns="jabber:x:data" type="submit">
                                                         <field var="FORM_TYPE" type="hidden"><value>urn:xmpp:mam:0</value></field>
                                                         <field var="with"><value>test@test.com</value></field>
                                                       </x>
                                                       <set xmlns="bad">
                                                       </set>
                                                     </query></iq>.toString
        MAMRequest.fromXml(missingData).failed.get.getMessage should include(s""""set" element namespace must have value equal to {MAMRequest.PagingNamespace}""")
      }

      "have missing max value" in {
        val missingData = <iq type="set" id="testId"><query xmlns="urn:xmpp:mam:0">
                                                       <x xmlns="jabber:x:data" type="submit">
                                                         <field var="FORM_TYPE" type="hidden"><value>urn:xmpp:mam:0</value></field>
                                                         <field var="with"><value>test@test.com</value></field>
                                                       </x>
                                                       <set xmlns="http://jabber.org/protocol/rsm">
                                                         <max>bad</max>
                                                       </set>
                                                     </query></iq>.toString
        an[NumberFormatException] should be thrownBy MAMRequest.fromXml(missingData).get
      }
    }
  }
}