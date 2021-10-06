class CustomSet[+T](items: Seq[T]) {
    val elements = items.distinct
}

object CustomSet {

    def fromList[T](items: Seq[T]): CustomSet[T] =
        new CustomSet(items)

    def toList[T](set: CustomSet[T]): Seq[T] =
        set.elements toSeq

    def empty[T](set: CustomSet[T]): Boolean =
        set.elements isEmpty

    def singleton[T](set: CustomSet[T]): Boolean = 
        set.elements.size == 1

    def member[T](set: CustomSet[T], item: T): Boolean =
        set.elements contains item

    def isEqual[T](set1: CustomSet[T], set2: CustomSet[T]): Boolean =
        CustomSet.isSubsetOf(set1, set2) &&
        CustomSet.isSubsetOf(set2, set1)
        
    def isSubsetOf[T](set1: CustomSet[T], set2: CustomSet[T]): Boolean = 
        set1.elements.forall(member(set2, _))

    def isDisjointFrom[T](set1: CustomSet[T], set2: CustomSet[T]): Boolean = 
        !set1.elements.exists(member(set2, _))

    def insert[T](set: CustomSet[T], item: T): CustomSet[T] =
        new CustomSet(set.elements :+ item)

    def intersection[T](set1: CustomSet[T], set2: CustomSet[T]): CustomSet[T] =
        new CustomSet(set1.elements.filter(member(set2, _)))

    def difference[T](set1: CustomSet[T], set2: CustomSet[T]): CustomSet[T] =
        new CustomSet(set1.elements.filter(!member(set2, _)))

    def union[T](set1: CustomSet[T], set2: CustomSet[T]): CustomSet[T] =
        new CustomSet(set1.elements ++ set2.elements)
}   