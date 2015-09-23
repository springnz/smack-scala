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

object MAMFinProvider extends ExtensionElementProvider[MAMFin] with NestedExtractionProvider {
  override def parse(parser: XmlPullParser, initialDepth: Int): MAMFin = {
    val xml = text(parser, initialDepth)
    MAMFin.fromXml(xml).get
  }
}
