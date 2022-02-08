case class Bst[T: Ordering](data: T, left: Option[Bst[T]], right: Option[Bst[T]]) {
    def insert(newValue: T): Bst[T] =
        if (Ordering[T].lteq(newValue, data))
            new Bst(data, insert(newValue, left), right)
        else
            new Bst(data, left, insert(newValue, right))

    def insert(newValue: T, tree: Option[Bst[T]]): Option[Bst[T]] = tree match {
        case Some(branch) => Some(branch.insert(newValue))
        case None => Some(Bst(newValue))
    }
}

object BST{
    def apply[T: Ordering](data: T): Bst[T] = new Bst(data, None, None)
    def fromList[T: Ordering](values: List[T]) = values match {
        case x :: xs => xs.foldLeft(Bst(x))((tree, data) => tree.insert(data))
        case Nil => throw new IllegalArgumentException("Cannot create tree from empty list")
    }
    def toList[T: Ordering](tree: Bst[T]): List[T] = toList(Some(tree))
    private def toList[T: Ordering](tree: Option[Bst[T]]): List[T] = tree match {
        case Some(x) => toList(x.left) ++ List(x.data) ++ toList(x.right)
        case None => Nil
    }
}