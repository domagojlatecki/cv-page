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

    sealed abstract class HtmlElement(val attributes: Attributes) {
        type This <: HtmlElement
        def newLikeThis(attributes: Attributes): This

        protected[this] final def attributesToString: String =
            if (this.attributes.nonEmpty) {
                this.attributes.filter(_._2.nonEmpty)
                    .map(kv => s"""${kv._1.name}="${kv._2.get}"""")
                    .mkString(" ", ", ", "")
            } else {
                ""
            }
    }

    abstract class HtmlBranchElement protected[html](attributes: Attributes) extends HtmlElement(attributes) {
        def this() = this(Map())
        def productPrefix: String

        override final type This = HtmlBranchElement
        override final def newLikeThis(attributes: Attributes): This = new NamedBranchElement(productPrefix, attributes)

        final def apply(content: => Unit)(implicit builder: HtmlBuilder): Unit = {
            builder.startBranch(productPrefix, attributesToString)
            content
            builder.endBranch()
        }
    }

    private class NamedBranchElement(name: String, attributes: Attributes) extends HtmlBranchElement(attributes) {
        override def productPrefix: String = name
    }

    abstract class HtmlLeafElement protected[html](attributes: Attributes) extends HtmlElement(attributes) {
        def this() = this(Map())
        def productPrefix: String

        override final type This = HtmlLeafElement
        override final def newLikeThis(attributes: Attributes): This = new NamedLeafElement(productPrefix, attributes)

        final def unary_-(implicit builder: HtmlBuilder): Unit = {
            builder.createLeaf(s"<$productPrefix$attributesToString/>") // TODO
        }
    }

    private class NamedLeafElement(name: String, attributes: Attributes) extends HtmlLeafElement(attributes) {
        override def productPrefix: String = name
    }

    // branch elements

    case object h1 extends HtmlBranchElement with BasicHtmlAttributes
    case object h2 extends HtmlBranchElement with BasicHtmlAttributes
    case object h3 extends HtmlBranchElement with BasicHtmlAttributes
    case object h4 extends HtmlBranchElement with BasicHtmlAttributes
    case object h5 extends HtmlBranchElement with BasicHtmlAttributes
    case object h6 extends HtmlBranchElement with BasicHtmlAttributes
    case object div extends HtmlBranchElement with BasicHtmlAttributes
    case object span extends HtmlBranchElement with BasicHtmlAttributes
    case object p extends HtmlBranchElement with BasicHtmlAttributes
    case object pre extends HtmlBranchElement with BasicHtmlAttributes
    case object a extends HtmlBranchElement with BasicHtmlAttributes
    case object code extends HtmlBranchElement with BasicHtmlAttributes

    // leaf elements

    case object br extends HtmlLeafElement
    case object img extends HtmlLeafElement {
        def apply(src: String = null): This = newLikeThis(Map('src -> Option(src)))
    }
}
