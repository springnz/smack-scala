package smack.scala.extensions

import org.xmlpull.v1.XmlPullParser

//The XmlPullParser is really tricky because you are prohibited from reading closing tags of the full element if it's not root
//For instance in
//<root>
//   <node>
//      <closed attr="test" />
//      <item>text</item>
//   </node>
//</root>
//We have to stop reading at </item> or else it messes with the rest of the message parsing -- haven't figured out why
//This provider stops at the first end tag of an embedded element
trait NestedExtractionProvider {
  def text(parser: XmlPullParser, initialDepth: Int): String = {
    def textRecursive(priorTags: List[String]): String = {
      if (parser.getEventType == XmlPullParser.END_TAG) {
        val all = priorTags.view map (t ⇒ s"</$t>") mkString ("")
        all
      } else if (parser.getEventType == XmlPullParser.START_TAG) {
        val thisText = parser.getText
        val name = parser.getName
        val event = parser.next
        val newTags = event match {
          case XmlPullParser.END_TAG if parser.getDepth != initialDepth ⇒
            parser.nextTag
            priorTags
          case XmlPullParser.END_TAG =>
            priorTags
          case XmlPullParser.TEXT ⇒
            name :: priorTags
          case _ ⇒ name :: priorTags
        }
        thisText + textRecursive(newTags)
      } else {
        val thisText = xml.Utility.escape(parser.getText)
        parser.next
        thisText + textRecursive(priorTags)
      }
    }
    textRecursive(List.empty)
  }
}
