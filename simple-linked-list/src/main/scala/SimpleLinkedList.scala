trait SimpleLinkedList[T] {
  def isEmpty: Boolean
  def value: T
  def add(item: T): SimpleLinkedList[T]
  def next: SimpleLinkedList[T]
  def reverse: SimpleLinkedList[T]
  def toSeq: Seq[T]
}

case class Empty[T]() extends SimpleLinkedList[T] {
  override def isEmpty: Boolean = true
  override def value: T = 
      throw new Exception("no payload in empty node")
  override def add(item: T): SimpleLinkedList[T] =
      Node(item, this)
  override def next: SimpleLinkedList[T] =
      throw new Exception("no link to next node in node")
  override def reverse: SimpleLinkedList[T] =
      this
  override def toSeq: Seq[T] = 
      Seq.empty
}

case class Node[T](val data: T, val link: SimpleLinkedList[T]) 
     extends SimpleLinkedList[T] {
  override def isEmpty: Boolean = false
    override def value: T = data
  override def add(item: T): SimpleLinkedList[T] =
      Node(value, next.add(item))
  override def next: SimpleLinkedList[T] = link
  override def reverse: SimpleLinkedList[T] =
      next.reverse.add(value)
  override def toSeq: Seq[T] = 
      value +: next.toSeq
}

object SimpleLinkedList {
  def apply[T](): SimpleLinkedList[T] =
      Empty()
  def apply[T](elems: T*): SimpleLinkedList[T] =
      fromSeq(elems)
  def fromSeq[T](list: Seq[T]): SimpleLinkedList[T] =
      list
      .foldLeft(SimpleLinkedList[T]())
            {case (sll, elm) => sll.add(elm)}             
}