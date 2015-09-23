package smack.scala.extensions

import org.jivesoftware.smack.provider.ExtensionElementProvider
import org.xmlpull.v1.XmlPullParser

// implementation of XEP-0066 Out of Band Data, for (initiation of) file transfers via XMPP
// see http://xmpp.org/extensions/xep-0066.html
// leaving out the IQ negotiation parts because they are complex and require both clients to be online

//<x xmlns={ XmlNamespace }>
//<url>{ url }</url>
//{ desc match { case FileDescription(Some(d)) ⇒ <desc>{ d }</desc>; case _ ⇒ "" } }
//</x>

object OutOfBandDataProvider extends ExtensionElementProvider[OutOfBandData] with FlatExtractionProvider {
  override def parse(parser: XmlPullParser, initialDepth: Int): OutOfBandData = {
    val xml = text(parser, initialDepth)
    OutOfBandData.fromXml(xml).get
  }
}