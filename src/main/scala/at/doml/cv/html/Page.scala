package at.doml.cv.html

import at.doml.cv.html.Page.{head, html}

trait Page {

    val title: String
    implicit val builder: HtmlBuilder = new HtmlBuilder

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

private object Page {
    case object html extends HtmlBranchElement
    case object head extends HtmlBranchElement
}
