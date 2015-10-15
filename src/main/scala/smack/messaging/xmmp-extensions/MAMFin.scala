package smack.scala.extensions

import smack.scala.Client.{ MessageId }
import org.jivesoftware.smack.packet.{ ExtensionElement }

import scala.util.Try
import scala.xml.{ NodeSeq, XML }

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

  def apply() = new MAMFin()

  def fromXml(xml: CharSequence): Try[MAMFin] =
    Try {
      val root = XML.loadString(xml.toString)
      assert(root.label == "fin", s"message must be type fin in $xml")
      assert(root.namespace == XmlNamespace, s"message must be in namespace $XmlNamespace in $xml")
      val complete = root \@ "complete"
      assert(!complete.isEmpty, s"message must have complete set in $xml")
      val parsedComplete = Try(complete.toBoolean)
      assert(parsedComplete.isSuccess, s"message must have complete set to a boolean in $xml")

      val setNodes = root \ "set"

      if (setNodes.size == 0) return Try(MAMFin())

      val setNode = setNodes(0)
      assert(setNode.namespace == SetNamespace, s"set tag must be in namespace $SetNamespace in $xml")

      val first = setNode \ "first" match {
        case a: NodeSeq if a.length == 1 ⇒
          val pindex = Try((a(0) \@ "index").toInt)
          assert(!a(0).text.isEmpty, s""""first" tag must have message id text in $xml""")
          new { val index = pindex.toOption; val messageId = Option(MessageId(a(0).text)) }
        case _ ⇒ new { val index = None; val messageId = None }
      }

      val last = setNode \ "last" match {
        case a: NodeSeq if a.length == 1 ⇒
          assert(!a(0).text.isEmpty, s""""last" tag must have message id text in $xml""")
          Option(MessageId(a(0).text))
        case _ ⇒ None
      }

      val count = setNode \ "count" match {
        case a: NodeSeq if a.length == 1 ⇒
          val pCount = Try(a(0).text.toInt)
          assert(pCount.isSuccess, s""""count" tag must have int value in $xml""")
          Option(pCount.get)
        case _ ⇒ None
      }

      MAMFin(first.messageId, last, first.index, count, parsedComplete.get)
    }
}

case class MAMFin(firstMessageId: Option[MessageId], lastMessageId: Option[MessageId], index: Option[Int], count: Option[Int], complete: Boolean = false) extends ExtensionElement {
  def this() = this(None, None, None, None, true)

  import MAMFin._

  override def getNamespace: String = XmlNamespace
  override def getElementName: String = ElementName

  override def toXML: CharSequence =
    <fin xmlns={ XmlNamespace } complete={ complete.toString }>
      <set xmlns={ SetNamespace }>
        {
          firstMessageId match {
            case Some(MessageId(id))⇒ if (index.isDefined) <first index={ index.get.toString }>{ id }</first>; else <first>{ id }</first>
            case _             ⇒ ""
          }
        }
        { lastMessageId match { case Some(MessageId(id)) ⇒ <last>{ id }</last>; case _ ⇒ "" } }
        { count match { case Some(num) ⇒ <count>{ num }</count>; case _ ⇒ "" } }
      </set>
    </fin>.toString
}
