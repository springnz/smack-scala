package smack.scala.extensions

import java.io.StringReader

import org.jivesoftware.smack.packet.{ Message, ExtensionElement, IQ }
import org.jivesoftware.smack.util.PacketParserUtils
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import org.xmlpull.v1.{ XmlPullParserFactory }


import scala.util.{ Failure, Success, Try }
import scala.xml.XML

// implementation of XEP-0313 Message Archive Management, for querying XMMP server for historical messages
// see http://xmpp.org/extensions/xep-0313.html

object MAMResponse extends ExtensionInfoProvider {
  val XmlNamespace = "urn:xmpp:mam:0"
  val ElementName = "result"

  val ForwardedNamespace = "urn:xmpp:forward:0"
  val DelayNamespace = "urn:xmpp:delay"

  def fromXml(xml: CharSequence): Try[MAMResponse] =
    Try {
      val root = XML.loadString(xml.toString)
      assert(root.label == "result", s"message must be type result in $xml")
      assert(root.namespace == XmlNamespace, s"message must be in namespace $XmlNamespace in $xml")
      val id = root \@ "id"
      assert(!id.isEmpty, s"message must have id in $xml")
      val queryId = root \@ "queryid"

      val forwarded = root \ "forwarded"
      assert(forwarded.size == 1, s"message must have forwarded tag in $xml")
      val forwardNode = forwarded(0)
      assert(forwardNode.namespace == ForwardedNamespace, s"forward tag must have namespace $ForwardedNamespace in $xml")

      val delay = forwardNode \ "delay"
      assert(delay.size == 1, s"message must have delay tag in $xml")
      val delayNode = delay(0)
      assert(delayNode.namespace == DelayNamespace, s"delay tag must have namespace $DelayNamespace in $xml")
      val stampStr = delayNode \@ "stamp"
      assert(!stampStr.isEmpty, s"stamp must be set in delay tag in $xml")

      val stamp = Try(ISODateTimeFormat.dateTimeParser().parseDateTime(delayNode \@ "stamp"))
      assert(stamp.isSuccess, s"stamp format was not ISO")
      val message = forwardNode \ "message"
      assert(message.size == 1, s"message missing in $xml")
      val parser = XmlPullParserFactory.newInstance().newPullParser()
      println(message(0).toString)
      parser.setInput(new StringReader(message(0).toString))
      parser.nextTag()
      val parsedMessage = PacketParserUtils.parseMessage(parser)

      MAMResponse(parsedMessage, stamp.get, id, if (queryId.isEmpty) None else Some(queryId))
    }
}

case class MAMResponse(message: Message, origStamp: DateTime, id: String, queryId: Option[String] = None) extends ExtensionElement {
  import MAMResponse._

  override def getNamespace: String = XmlNamespace
  override def getElementName: String = ElementName

  override def toXML: CharSequence = {
    val messageXML = message.toXML
    <result xmlns={ XmlNamespace } queryid={ queryId.getOrElse(null) } id={ id }>
      <forwarded xmlns={ ForwardedNamespace }>
        <delay xmlns={ DelayNamespace } stamp={ origStamp.toString(ISODateTimeFormat.dateTime) }/>
        <message></message>
      </forwarded>
    </result>.toString.replace("<message></message>", message.toXML)
  }
}
