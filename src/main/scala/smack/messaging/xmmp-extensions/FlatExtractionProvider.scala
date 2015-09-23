package smack.scala.extensions

import org.xmlpull.v1.XmlPullParser

//The XmlPullParser is really tricky because you are prohibited from reading closing tags of the full element if it's not root,
// but must read elements if they are within root
//
//For instance in
//<root>
//   <item>text</item>
//</root>
//We have to stop reading at </root> or else it messes with the rest of the message parsing -- haven't figured out why
//This provider stops at the end of the root tag in contrast to NestedExtractionProvider
trait FlatExtractionProvider {
  def text(parser: XmlPullParser, initialDepth: Int): String = {
    def textRecursive(parser: XmlPullParser, currentDepth: Int, priorTags: List[String], first: Boolean): String = {
      if (parser.getEventType == XmlPullParser.END_TAG && parser.getDepth == currentDepth) {
        val all = priorTags.view map (t ⇒ s"</$t>") mkString ("")
        all
      } else if (parser.getEventType == XmlPullParser.START_TAG) {
        val thisText = parser.getText
        val name = parser.getName
        val event = parser.next
        val newTags = event match {
          case XmlPullParser.END_TAG ⇒
            parser.nextTag
            priorTags
          case XmlPullParser.TEXT ⇒
            priorTags
          case _ ⇒ priorTags
        }
        thisText + textRecursive(parser, initialDepth, newTags, false)
      } else {
        val thisText = if (parser.getEventType == XmlPullParser.END_TAG) parser.getText else xml.Utility.escape(parser.getText)
        parser.next
        thisText + textRecursive(parser, initialDepth, priorTags, false)
      }
    }
    textRecursive(parser, initialDepth, List[String](parser.getName), true)
  }
}
