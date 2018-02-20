package at.doml.cv

import at.doml.cv.html.{Page, String2HtmlElement, div, p}

object index extends Page {

    override val title = "Curriculum Vitae"

    div {
        p {
            -"This is some test content"
        }
    }
}
