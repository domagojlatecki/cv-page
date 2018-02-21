package at.doml.cv.html

trait Page {

    val title: String
    implicit val builder: HtmlBuilder = new HtmlBuilder

    private def elem(name: String, content: => Unit, builder: HtmlBuilder): Unit = {
        builder.startBranch(name)
        content
        builder.endBranch()
    }

    private def html(content: => Unit)(implicit builder: HtmlBuilder): Unit = elem("html", content, builder)
    private def head(content: => Unit)(implicit builder: HtmlBuilder): Unit = elem("head", content, builder)
    private def meta(charset: String)(implicit builder: HtmlBuilder): Unit = -s"""<meta charset="$charset"/>"""
    private def title(content: String)(implicit builder: HtmlBuilder): Unit = -s"""<title>$content</title>"""

    private def body()(implicit builder: HtmlBuilder): Unit = {
        builder.startBranch("body")
        builder.currentBranch.children ++= this.builder.tree.children
        builder.endBranch()
    }

    // TODO scripts, stylesheets, icon
    def render(): Unit = {
        implicit val builder: HtmlBuilder = new HtmlBuilder

        -"<!DOCTYPE html>"
        html {
            head {
                meta(charset = "utf-8")
                title(title)
            }
            body()
        }

        builder.print()
    }
}
