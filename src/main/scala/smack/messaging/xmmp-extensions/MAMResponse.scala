package smack.scala.extensions

import org.jivesoftware.smack.packet.{ Message, ExtensionElement, IQ }
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

// implementation of XEP-0313 Message Archive Management, for querying XMMP server for historical messages
// see http://xmpp.org/extensions/xep-0313.html

object MAMResponse extends ExtensionInfoProvider {
  val XmlNamespace = "urn:xmpp:mam:0"
  val ElementName = "result"

  val ForwardedNamespace = "urn:xmpp:foward:0"
  val DelayNamespace = "urn:xmpp:delay"

  //  def fromXml(xml: CharSequence): Try[MAMResponseExtension] =
  //    Try {
  //      val root = XML.loadString(xml.toString)
  //      val url = root \ "url"
  //      assert(url.text.length > 0, s"url must be present but was not while trying to parse $xml")
  //
  //      val desc = (root \ "desc").text match {
  //        case ""   ⇒ FileDescription(None)
  //        case desc ⇒ FileDescription(Some(desc))
  //      }
  //
  //      MAMResponseExtension(new Message(), new DateTime(), "")
  //    }
}

case class MAMResponse(message: Message, origStamp: DateTime, id: String, queryId: Option[String] = None) extends ExtensionElement {
  import MAMResponse._

  override def getNamespace: String = XmlNamespace
  override def getElementName: String = ElementName

  override def toXML: CharSequence =
    <result xmlns={ XmlNamespace } queryid={ queryId.getOrElse(null) } id={ id }>
      <forwarded xmlns={ ForwardedNamespace }>
        <delay xmlns={ DelayNamespace } stamp={ origStamp.toString(ISODateTimeFormat.dateTime) }/>
        { message.toXML }
      </forwarded>
    </result>.toString
}
