package smack.scala.extensions

import org.jivesoftware.smack.util.PacketParserUtils
import org.joda.time.format.{ ISODateTimeFormat }
import org.xmlpull.v1.XmlPullParser
import org.jivesoftware.smack.provider.ExtensionElementProvider

// implementation of XEP-0313 Message Archive Management, for querying XMMP server for historical messages
// see http://xmpp.org/extensions/xep-0313.html

//    <result xmlns="urn:xmpp:mam:0" queryid="STRING" id="STRING">
//      <forwarded xmlns="urn:xmpp:foward:0">
//        <delay xmlns="urn:xmpp:delay" stamp="DATETIME" />
//          <message xmlns="jabber:client" to="STRING" from="STRING" type="STRING[chat]" id="NUMBER">
//              <body>STRING</body>
//          </message>
//      </forwarded>
//    </result>
object MAMResponseProvider extends ExtensionElementProvider[MAMResponse] {
  override def parse(parser: XmlPullParser, initialDepth: Int): MAMResponse = {
    val queryId = Option(parser.getAttributeValue(null, "queryid")) //null is for namespace
    val id = parser.getAttributeValue(null, "id")
    parser.nextTag //forwarded
    parser.nextTag //delay
    val origStamp = ISODateTimeFormat.dateTimeParser().parseDateTime(parser.getAttributeValue(null, "stamp"))
    parser.nextTag //end of delay

    parser.nextTag //message
    val message = PacketParserUtils.parseMessage(parser)
    parser.next //close message
    parser.next //close forwarded
    parser.next //close result
    println("PARSED ARCHIVE")
    val res = MAMResponse(message, origStamp, id, queryId)
    res
  }
}
