package at.doml.cv

import at.doml.cv.html.HtmlElement.String2HtmlElement
import at.doml.cv.html.{Page, div, p}

object Main extends App with Page {

    override val title = "Curriculum Vitae"

    div {
        p {
            -"This is some test content"
        }
    }

    render()
}
