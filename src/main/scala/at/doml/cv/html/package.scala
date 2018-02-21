package at.doml.cv

package object html {

    final implicit class String2HtmlElement(val string: String) extends AnyVal {
        def unary_-(implicit builder: HtmlBuilder): Unit = builder.createLeaf(string)
    }

    private type Attributes = Map[Symbol, Option[String]]

    trait BasicHtmlAttributes {
        self: HtmlElement =>

        final def apply(`class`: String = null,
                        id: String = null,
                        href: String = null,
                        disabled: Boolean = false,
                        hidden: Boolean = false): This = {
            self.newLikeThis(
                Map(
                    'class -> Option(`class`),
                    'id -> Option(id),
                    'href -> Option(href),
                    'disabled -> someIfTrueElseNone(disabled, "disabled"),
                    'hidden -> someIfTrueElseNone(hidden, "hidden")
                )
            )
        }

        private def someIfTrueElseNone(condition: Boolean, some: String): Option[String] =
            if (condition) {
                Some(some)
            } else {
                None
            }
    }

    sealed abstract class HtmlElement(val name: String, val attributes: Attributes) {
        type This <: HtmlElement
        def newLikeThis(attributes: Attributes): This

        protected[this] final def attributesToString: String =
            if (attributes.nonEmpty) {
                attributes.filter(_._2.nonEmpty)
                    .map(kv => s"""${kv._1.name}="${kv._2.get}"""")
                    .mkString(" ", ", ", "")
            } else {
                ""
            }
    }

    class HtmlBranchElement private(name: String, attributes: Attributes) extends HtmlElement(name, attributes) {
        def this(name: String) = this(name, Map())
        override final type This = HtmlBranchElement
        override final def newLikeThis(attributes: Attributes): This = new HtmlBranchElement(this.name, attributes)

        final def apply(content: => Unit)(implicit builder: HtmlBuilder): Unit = {
            builder.startBranch(this.name, attributesToString)
            content
            builder.endBranch()
        }
    }

    class HtmlLeafElement private(name: String, attributes: Attributes) extends HtmlElement(name, attributes) {
        def this(name: String) = this(name, Map())
        override final type This = HtmlLeafElement
        override final def newLikeThis(attributes: Attributes): This = new HtmlLeafElement(this.name, attributes)

        final def unary_-(implicit builder: HtmlBuilder): Unit = {
            builder.createLeaf(s"<$name$attributesToString/>") // TODO
        }
    }

    // branch elements

    object h1 extends HtmlBranchElement("h1") with BasicHtmlAttributes
    object h2 extends HtmlBranchElement("h2") with BasicHtmlAttributes
    object h3 extends HtmlBranchElement("h3") with BasicHtmlAttributes
    object h4 extends HtmlBranchElement("h4") with BasicHtmlAttributes
    object h5 extends HtmlBranchElement("h5") with BasicHtmlAttributes
    object h6 extends HtmlBranchElement("h6") with BasicHtmlAttributes
    object div extends HtmlBranchElement("div") with BasicHtmlAttributes
    object span extends HtmlBranchElement("span") with BasicHtmlAttributes
    object p extends HtmlBranchElement("p") with BasicHtmlAttributes
    object pre extends HtmlBranchElement("pre") with BasicHtmlAttributes
    object a extends HtmlBranchElement("a") with BasicHtmlAttributes
    object code extends HtmlBranchElement("code") with BasicHtmlAttributes

    // leaf elements

    object br extends HtmlLeafElement("br")
    object img extends HtmlLeafElement("img") {
        def apply(src: String = null): This = newLikeThis(Map('src -> Option(src)))
    }
}
