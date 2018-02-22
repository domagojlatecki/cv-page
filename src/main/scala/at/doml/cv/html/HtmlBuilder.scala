package at.doml.cv.html

import at.doml.cv.html.HtmlBuilder.{Branch, Leaf, Node, NodeWithChildren, Root}
import scala.collection.mutable.ListBuffer

private[html] object HtmlBuilder {

    trait Node {
        def parent: NodeWithChildren
    }

    trait NodeWithChildren extends Node {
        val children: ListBuffer[Node]
    }

    case class Root(children: ListBuffer[Node] = ListBuffer()) extends NodeWithChildren {
        def parent: NodeWithChildren = this
    }

    case class Branch(name: String, attributes: String, parent: NodeWithChildren,
                      children: ListBuffer[Node] = ListBuffer()) extends NodeWithChildren

    case class Leaf(content: String, parent: NodeWithChildren) extends Node
}

final class HtmlBuilder {
    private[html] val tree: Root = Root()
    private[html] var currentBranch: NodeWithChildren = tree

    private[html] def startBranch(name: String, attributes: String = ""): Unit = {
        val newBranch = Branch(name, attributes, currentBranch)

        currentBranch.children += newBranch
        currentBranch = newBranch
    }

    private[html] def endBranch(): Unit = currentBranch = currentBranch.parent
    private[html] def createLeaf(content: String): Unit = currentBranch.children += Leaf(content, currentBranch)

    def print(indentLevel: Int = 0, indentStep: Int = 4): Unit = {
        printNode(tree, indentLevel, indentStep)
    }

    private def printNode(node: Node, indentLevel: Int, indentStep: Int): Unit = {
        val indent = " " * indentLevel

        node match {
            case Root(children) => children.foreach(printNode(_, indentLevel, indentStep))
            case Branch(name, attributes, _, children) => {
                println(s"$indent<$name$attributes>")
                children.foreach(printNode(_, indentLevel + indentStep, indentStep))
                println(s"$indent</$name>")
            }
            case Leaf(content, _) => println(s"$indent$content")
        }
    }
}
