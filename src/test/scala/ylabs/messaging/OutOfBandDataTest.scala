package ylabs.messaging

import org.scalatest.{ Matchers, WordSpec }
import scala.util.Success

class OutOfBandDataTest extends WordSpec with Matchers {

  "serialises and deserialises url" in {
    val oob = OutOfBandData("url")
    OutOfBandData.fromXml(oob.toXML) shouldBe Success(oob)
  }

  "serialises and deserialises url and description" in {
    val oob = OutOfBandData("url", Some("optional description"))
    OutOfBandData.fromXml(oob.toXML) shouldBe Success(oob)
  }

  "handles missing mandatory element" in {
    val xml = """<x xmlns="jabber:x:oob"><desc>optional description</desc></x>"""
    OutOfBandData.fromXml(xml) shouldBe ('failure)
  }

  "handles invalid xml" in {
    val invalid = "<this>is</invalid>"
    OutOfBandData.fromXml(invalid) shouldBe ('failure)
  }

}
