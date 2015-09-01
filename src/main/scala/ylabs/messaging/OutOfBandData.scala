package ylabs.messaging

import org.jivesoftware.smack.packet.ExtensionElement
import org.jivesoftware.smack.packet.IQ
import org.jivesoftware.smack.packet.IQ.IQChildElementXmlStringBuilder
import scala.util.Try
import scala.xml.XML

// implementation of XEP-0066 Out of Band Data, for (initiation of) file transfers via XMPP
// see http://xmpp.org/extensions/xep-0066.html
// leaving out the IQ negotiation parts because they are complex and require both clients to be online

object OutOfBandData {
  val XmlNamespace = "jabber:x:oob"
  val ElementName = "x"

  def fromXml(xml: CharSequence): Try[OutOfBandData] =
    Try {
      val root = XML.loadString(xml.toString)
      val url = root \ "url"
      assert(url.text.length > 0, s"url must be present but was not while trying to parse $xml")

      val desc = (root \ "desc").text match {
        case ""   ⇒ None
        case desc ⇒ Some(desc)
      }

      OutOfBandData(url.text, desc)
    }
}

case class OutOfBandData(url: String, desc: Option[String] = None) extends ExtensionElement {
  import OutOfBandData._

  override def getNamespace: String = XmlNamespace
  override def getElementName: String = ElementName

  override def toXML: CharSequence =
    <x xmlns={ XmlNamespace }>
      <url>{ url }</url>
      { desc match { case Some(d) ⇒ <desc>{ d }</desc>; case _ ⇒ "" } }
    </x>.toString
}
