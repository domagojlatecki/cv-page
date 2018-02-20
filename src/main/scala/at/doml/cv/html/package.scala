package at.doml.cv

package object html {

    final implicit class String2HtmlElement(val string: String) extends AnyVal {
        def unary_-(implicit builder: HtmlBuilder): Unit = builder.createLeaf(string)
    }

    sealed abstract class HtmlElement(name: String)

    sealed abstract class HtmlNodeElement(name: String) extends HtmlElement(name) {

        def apply(`class`: String = null,
                  id: String = null,
                  href: String = null,
                  disabled: Boolean = false,
                  hidden: Boolean = false)(contents: => Unit)(implicit builder: HtmlBuilder): Unit = {
            builder.startBranch(name) //TODO attrs
            contents
            builder.endBranch()
        }

        def apply(contents: => Unit)(implicit builder: HtmlBuilder): Unit = apply()(contents)(builder)
    }

    sealed abstract class HtmlLeafElement(name: String) extends HtmlElement(name) {

        //TODO attributes
        def apply(implicit builder: HtmlBuilder): Unit = {
            builder.createLeaf(s"<$name/>")
        }
    }

    // branch elements

    object h1 extends HtmlNodeElement("h1")

    object h2 extends HtmlNodeElement("h2")

    object h3 extends HtmlNodeElement("h3")

    object h4 extends HtmlNodeElement("h4")

    object h5 extends HtmlNodeElement("h5")

    object h6 extends HtmlNodeElement("h6")

    object div extends HtmlNodeElement("div")

    object span extends HtmlNodeElement("span")

    object p extends HtmlNodeElement("p")

    object pre extends HtmlNodeElement("pre")

    object a extends HtmlNodeElement("a")

    object code extends HtmlNodeElement("code")

    // leaf elements

    object br extends HtmlLeafElement("br")

    object img extends HtmlLeafElement("img")

}
