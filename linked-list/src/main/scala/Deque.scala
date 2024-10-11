class Deque[T] {
    private var head: Option[Node[T]] = None
    private var last: Option[Node[T]] = None
    def push(item: T) = {
        last = Option(Node(item, last, None))
        head.foreach(_.next = last)
        head = head.orElse(last)
    }
    def pop: Option[T] = {
        val node = last
        head = if (head != last) head else None
        last = node.flatMap(x => x.prev)
        node.map(x => x.value)
    }
    def shift: Option[T] = {
        val node = head
        last = if (last != head) last else None
        head = node.flatMap(x => x.next)
        node.map(x => x.value)
    }
    def unshift(item: T) = {
        head = Option(Node(item, None, head))
        last.foreach(_.prev = head)
        last = last.orElse(head)
    }
}

case class Node[T]( var value: T, 
                    var prev: Option[Node[T]] = None, 
                    var next: Option[Node[T]] = None )
{}

object Deque {
    def apply[T](): Deque[T] = new Deque[T]()
}