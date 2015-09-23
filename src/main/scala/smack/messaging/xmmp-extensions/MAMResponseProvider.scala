package smack.scala.extensions

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
object MAMResponseProvider extends ExtensionElementProvider[MAMResponse] with NestedExtractionProvider {
  override def parse(parser: XmlPullParser, initialDepth: Int): MAMResponse = {
    val xml = text(parser, initialDepth)
    println("PARSING " + xml)
    MAMResponse.fromXml(xml).get
  }
}
