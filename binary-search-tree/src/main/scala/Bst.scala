import scala.math.Ordering.Implicits._

case class Bst[T: Ordering](value: T, 
                            left: Option[Bst[T]] = None, 
                            right: Option[Bst[T]] = None) {
    def insert(item: T): Bst[T] =
        if (item <= value) 
            copy(left = left.map(_ insert item)
                            .orElse(Some(Bst(item))))
        else 
            copy(right = right.map(_ insert item)
                              .orElse(Some(Bst(item))))
}

object Bst {
    def apply[T: Ordering](value: T) = new Bst(value, None, None)

    def fromList[T: Ordering](list: List [T]): Bst[T] = {
        require(!list.isEmpty) 
        list.tail.foldLeft (Bst(list.head)) (_ insert _)
    }

    def toList[T: Ordering](bst: Bst[T]): List[T] = {
        val leftBST  = bst.left.map(toList(_)).getOrElse(List()) 
        val value    = List(bst.value)
        val rightBST = bst.right.map(toList(_)).getOrElse(List())
        leftBST ::: value ::: rightBST
    }

}