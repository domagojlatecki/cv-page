package at.doml.cv

import at.doml.cv.html.{Page, String2HtmlElement, br, div, img, p}

object index extends Page {

    override val title = "Curriculum Vitae"

    div(`class` = "a") {
        p {
            -"This is some test content"; -br
            -img(src = "abc")
        }
    }
}
