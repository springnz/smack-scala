package smack.scala.extensions

import org.jivesoftware.smack.provider.ExtensionElementProvider
import org.xmlpull.v1.XmlPullParser

//    MESSAGE END
//        <fin xmlns="urn:xmpp:mam:0" complete="BOOLEAN">
//          <set xmlns="http://jabber.org/protocol/rsm">
//            <first index="INT">MESSAGEID</first>
//            <last>MESSAGEID</last>
//            <count>INT</count>
//          </set>
//        </fin>

object MAMFinProvider extends ExtensionElementProvider[MAMFin] {
  override def parse(parser: XmlPullParser, initialDepth: Int): MAMFin = {
    val complete = Option(parser.getAttributeValue(null, "complete")) //null is for namespace
    parser.nextTag //set or end
    parser.getName match {
      case "fin" ⇒ MAMFin()
    }
  }
}
