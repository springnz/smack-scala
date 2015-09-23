package smack.scala.extensions

import smack.scala.Client.{ MessageId, UserWithDomain }
import org.jivesoftware.smack.packet.IQ
import org.jivesoftware.smack.packet.IQ.IQChildElementXmlStringBuilder
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import scala.util.Try
import scala.xml.XML

// implementation of XEP-0313 Message Archive Management, for querying XMMP server for historical messages
// see http://xmpp.org/extensions/xep-0313.html

object MAMRequest {
  val XmlNamespace = "urn:xmpp:mam:0"
  val ElementName = "query"
  val DataNamespace = "jabber:x:data"
  val PagingNamespace = "http://jabber.org/protocol/rsm"

  def fromXml(xml: CharSequence): Try[MAMRequest] =
    Try {
      val root = XML.loadString(xml.toString)
      assert(root.label == "iq", s"message must be iq type in $xml")
      assert((root \@ "type") == "set", s"""iq attr "type" must be equal to "set" in $xml""")
      assert(!(root \@ "id").isEmpty, s"""iq attr "id" must be set in $xml""")
      val query = root \ "query"
      assert(query.length == 1, s"there must be one query element while trying to parse $xml")
      val queryNode = query(0)
      assert(queryNode.namespace == XmlNamespace, s"query namespace must be $XmlNamespace in $xml")

      val data = queryNode \ "x"
      assert(data.length == 1, s"""there must be one data element "x" in $xml""")
      val dataNode = data(0)
      assert(dataNode.namespace == DataNamespace, s"data element x namespace must be $DataNamespace in $xml")
      assert(dataNode \@ "type" == "submit", s"""data element x attr "type" must be "submit" in $xml"""")

      val fieldInfo = (dataNode \ "field" map (n ⇒ {
        n \@ "var" -> new { val fieldType: String = n \@ "type"; val fieldValue: String = (n \ "value").text }
      })).toMap

      assert(fieldInfo.get("FORM_TYPE").isDefined, s"""field with attr "var" equal to "FORM_TYPE" must be set in $xml""")
      assert(fieldInfo.get("FORM_TYPE").get.fieldType == "hidden", s"""FORM_TYPE field must have attr "type" equal to "hidden" in $xml""")
      assert(fieldInfo.get("FORM_TYPE").get.fieldValue == "urn:xmpp:mam:0", s"""FORM_TYPE field must have value equal to "urn:xmpp:mam:0" in $xml""")

      val withField = fieldInfo.get("with")
      assert(withField.isDefined, s"""field with attr "var" equal to "with" must be set in $xml""")
      assert(!withField.get.fieldValue.isEmpty, s""""with" field must have value set in $xml""")

      val setNodes = queryNode \ "set"
      val allFields = fieldInfo ++ {
        if (setNodes.length == 0) Map.empty
        else {
          val setNode = setNodes(0)
          assert(setNode.namespace == PagingNamespace, s""""set" element namespace must have value equal to $PagingNamespace in $xml""")
          fieldInfo ++ (setNode.child map (n ⇒ {
            n.label -> new { val fieldValue: String = n.text }
          })).toMap
        }
      }

      val start = allFields.get("start") map (f ⇒ ISODateTimeFormat.dateTimeParser().parseDateTime(f.fieldValue))
      val end = allFields.get("end") map (f ⇒ ISODateTimeFormat.dateTimeParser().parseDateTime(f.fieldValue))
      val limit = allFields.get("max") map (m ⇒ Integer.parseInt(m.fieldValue))
      val skipTo = allFields.get("after") map (f ⇒ new MessageId(f.fieldValue))
      MAMRequest(UserWithDomain(withField.get.fieldValue), start, end, limit, skipTo)

    }
}

case class MAMRequest(user: UserWithDomain, start: Option[DateTime] = None, end: Option[DateTime] = None, limit: Option[Int] = None, skipTo: Option[MessageId] = None) extends IQ(MAMRequest.ElementName, MAMRequest.XmlNamespace) {
  super.setType(IQ.Type.set)

  import MAMRequest._

  def dataXml: CharSequence =
    <x xmlns={ DataNamespace } type="submit">
      <field var="FORM_TYPE" type="hidden"><value>{ XmlNamespace }</value></field>
      <field var="with"><value>{ user.value }</value></field>
      { start match { case Some(date) ⇒ <field var="start"><value>{ date }</value></field>; case _ ⇒ "" } }
      { end match { case Some(date) ⇒ <field var="end"><value>{ date }</value></field>; case _ ⇒ "" } }
    </x>.toString

  def pagingXml: CharSequence =
    if (!limit.isDefined && !skipTo.isDefined) ""
    else
      <set xmlns={ PagingNamespace }>
        { limit match { case Some(max) ⇒ <max>{ max }</max>; case _ ⇒ "" } }
        { skipTo match { case Some(msgId) ⇒ <after>{ msgId.value }</after>; case _ ⇒ "" } }
      </set>.toString

  override def getIQChildElementBuilder(xml: IQChildElementXmlStringBuilder): IQChildElementXmlStringBuilder = {
    xml.rightAngleBracket
    xml.append(dataXml)
    xml.append(pagingXml)
    xml
  }
}