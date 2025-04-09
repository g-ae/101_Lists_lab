import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class Node(var item: String, var next: Option[Node]) {
  def this(item: String, next: Node) = this(item, Option(next))
  def this(item: String) = this(item, Option.empty)
}
class LinkedList(var head: Option[Node]) {
  def this() = this(Option.empty)
  def this(str: String) = this(Option(new Node(str, Option.empty)))
  def this(head: Node) = this(Option(head))

  def addToStart(s: String): Unit = this.head = Option(new Node(s, this.head))
  def removeFirstElement(): Unit = if (this.head.isDefined) this.head = this.head.get.next
  def getLastElement(): Option[Node] = {
    if (!head.isDefined) return None

    @tailrec
    def getLastElementRecursive(node: Option[Node]): Option[Node] = {
      if (node.get.next.isEmpty) return node
      getLastElementRecursive(node.get.next)
    }

    getLastElementRecursive(head)
  }

  def addToEnd(element: String): Unit = {
    @tailrec
    def recurseAddToEnd(node: Node): Unit = {
      if (node.next.isDefined) recurseAddToEnd(node.next.get)
      else node.next = Option(new Node(element))
    }
    if (this.head.isDefined) recurseAddToEnd(this.head.get)
    else this.head = Option(new Node(element))
  }

  def isPresent(e:String): Boolean = {
    var tempNext = head
    while (tempNext.isDefined) {
      if (tempNext.get.item == e) return true
      tempNext = tempNext.get.next
    }
    false
  }

  def getSize(): Int = {
    def getSizeRecursive(node: Option[Node]): Int = {
      if (node.isEmpty) 0
      else 1 + getSizeRecursive(node.get.next)
    }
    getSizeRecursive(head)
  }

  def findElement(s:String): Option[Node] = {
    @tailrec
    def findElementRecursive(node: Option[Node], s: String): Option[Node] = {
      if (node.isDefined) {
        if (node.get.item == s) node
        else findElementRecursive(node.get.next, s)
      }
      else Option.empty
    }

    findElementRecursive(head, s)
  }

  def swapElements(e1: String, e2: String): Unit = {
    val nodeE1 = findElement(e1)
    val nodeE2 = findElement(e2)

    if (nodeE1.isDefined && nodeE2.isDefined) {
      nodeE1.get.item = e2
      nodeE2.get.item = e1
    }
  }

  def removeLastElement(): Unit = {
    if (head.isDefined) {
      if (head.get.next.isDefined) {
        var tempnext = head
        while (tempnext.get.next.get.next.isDefined) {
          tempnext = tempnext.get.next
        }
        tempnext.get.next = Option.empty
      } else this.head = Option.empty
    }
  }

  def removeElement(e: String): Unit = {
    var tempBefore: Option[Node] = None
    var tempNext = head

    while (tempNext.isDefined) {
      if (tempNext.get.item == e) {
        if (tempBefore.isEmpty) {
          head = tempNext.get.next
        }
        else tempBefore.get.next = tempNext.get.next
        return
      }
      tempBefore = tempNext
      tempNext = tempNext.get.next
    }
  }

  def insertAfter(before: String, after: String): Unit = {
    val beforeElement = findElement(before)
    if (beforeElement.isDefined) {
      if (beforeElement.get.next.isDefined) beforeElement.get.next = Option(new Node(after, beforeElement.get.next))
      else beforeElement.get.next = Option(new Node(after))
    }
  }

  override def toString: String = {
    var tempNext = head
    val strings = ArrayBuffer[String]()
    while (tempNext.isDefined) {
      strings += tempNext.get.item
      tempNext = tempNext.get.next
    }
    s"List content (size ${getSize()}) : " + strings.mkString(" -> ")
  }
}