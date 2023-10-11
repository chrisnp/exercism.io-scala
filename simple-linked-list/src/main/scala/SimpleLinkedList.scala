trait SimpleLinkedList[T] {
    def isEmpty: Boolean
    def value: T
    def add(item: T): SimpleLinkedList[T]
    def next: SimpleLinkedList[T]
    def reverse: SimpleLinkedList[T]
    def toSeq: Seq[T]
}
import scala.language.postfixOps

case class EmptyNode[T]() extends SimpleLinkedList[T] {
    override def isEmpty: Boolean = true
    override def value: T = throw new Exception("empty node: no data")
    override def add(item: T): SimpleLinkedList[T] = Node(item, EmptyNode())
    override def next: SimpleLinkedList[T] = 
            throw new Exception("empty node: no link to next node")
    override def reverse: SimpleLinkedList[T] = EmptyNode()
    override def toSeq: Seq[T] = Seq empty
}

case class Node[T](val data: T, val link: SimpleLinkedList[T]) 
extends SimpleLinkedList[T] {
    override def isEmpty: Boolean = false
    override def value: T = data
    override def add(item: T): SimpleLinkedList[T] = Node(value, next add(item))
    override def next: SimpleLinkedList[T] = link
    override def reverse: SimpleLinkedList[T] = next.reverse add(value)
    override def toSeq: Seq[T] = value +: (next toSeq)
}

object SimpleLinkedList {
    def apply[T](): SimpleLinkedList[T] = EmptyNode()
    def apply[T](elems: T*): SimpleLinkedList[T] = fromSeq (elems)
    def fromSeq[T](list: Seq[T]): SimpleLinkedList[T] = 
            list.foldLeft(SimpleLinkedList[T]()) {
                (sllist, elem) => sllist add(elem)
            }             
}
