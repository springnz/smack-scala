package smack.scala.extensions

import smack.scala.FileDescription
import org.jivesoftware.smack.provider.ExtensionElementProvider
import org.xmlpull.v1.XmlPullParser
import java.net.URI

// implementation of XEP-0066 Out of Band Data, for (initiation of) file transfers via XMPP
// see http://xmpp.org/extensions/xep-0066.html
// leaving out the IQ negotiation parts because they are complex and require both clients to be online

//<x xmlns={ XmlNamespace }>
//<url>{ url }</url>
//{ desc match { case FileDescription(Some(d)) ⇒ <desc>{ d }</desc>; case _ ⇒ "" } }
//</x>

object OutOfBandDataProvider extends ExtensionElementProvider[OutOfBandData] {
  override def parse(parser: XmlPullParser, initialDepth: Int): OutOfBandData = {
    parser.nextTag //url
    parser.next //go to value in url
    val url = URI.create(parser.getText)
    parser.nextTag //go to close url
    parser.nextTag //go to desc
    val name = parser.getName
    parser.next //go to value in desc
    val text = if (name == "desc") Some(parser.getText) else None
    val desc = FileDescription(text)
    OutOfBandData(url, desc)
  }
}