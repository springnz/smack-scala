package smack.scala.extensions

import smack.scala.Client.{ MessageId }
import org.jivesoftware.smack.packet.{ ExtensionElement }

// implementation of XEP-0313 Message Archive Management, for querying XMMP server for historical messages
// see http://xmpp.org/extensions/xep-0313.html

//        <fin xmlns="urn:xmpp:mam:0" complete="BOOLEAN">
//          <set xmlns="http://jabber.org/protocol/rsm">
//            <first index="INT">MESSAGEID</first>
//            <last>MESSAGEID</last>
//            <count>INT</count>
//          </set>
//        </fin>

object MAMFin extends ExtensionInfoProvider {
  val XmlNamespace = "urn:xmpp:mam:0"
  val ElementName = "fin"

  val SetNamespace = "http://jabber.org/protocol/rsm"

  //  def fromXml(xml: CharSequence): Try[MessageArchiveResponse] =
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
  //      MessageArchiveResponse(URI.create(url.text), desc)
  //    }
}

case class MAMFin(firstMessageId: Option[MessageId] = None, lastMessageId: Option[MessageId] = None, index: Option[Int] = None, count: Option[Int] = None, complete: Boolean = true) extends ExtensionElement {
  import MAMFin._

  override def getNamespace: String = XmlNamespace
  override def getElementName: String = ElementName

  override def toXML: CharSequence =
    <fin xmlns={ XmlNamespace } complete={ complete.toString }>
      <set xmlns={ SetNamespace }>
        {
          firstMessageId match {
            case Some(id)⇒ if (index.isDefined) <first index={ index.get.toString }>{ id }</first>; else <first>{ id }</first>
            case _  ⇒ ""
          }
        }
        { lastMessageId match { case Some(id) ⇒ <last>{ id }</last>; case _ ⇒ "" } }
        { count match { case Some(num) ⇒ <count>{ num }</count>; case _ ⇒ "" } }
      </set>
    </fin>.toString
}
