class CustomSet[+T](items: Seq[T]) {
    val elements = items.distinct
}

object CustomSet {
    def fromList[T](items: Seq[T]): CustomSet[T] = new CustomSet(items)
    
    def toList[T](set: CustomSet[T]): Seq[T] = set.elements.toSeq
    
    def empty[T](set: CustomSet[T]): Boolean = set.elements.isEmpty
    
    def singleton[T](set: CustomSet[T]): Boolean = set.elements.size == 1
    
    def member[T](set: CustomSet[T], item: T): Boolean = 
            set.elements contains item
    
    def isEqual[T](s1: CustomSet[T], s2: CustomSet[T]): Boolean = 
            CustomSet.isSubsetOf(s1, s2) && CustomSet.isSubsetOf(s2, s1)
    
    def isSubsetOf[T](s1: CustomSet[T], s2: CustomSet[T]): Boolean = 
            s1.elements.forall(CustomSet.member(s2, _))
    
    def isDisjointFrom[T](s1: CustomSet[T], s2: CustomSet[T]): Boolean = 
            !s1.elements.exists(CustomSet.member(s2, _))
    
    def insert[T](set: CustomSet[T], item: T): CustomSet[T] = 
            new CustomSet(set.elements :+ item)
    
    def intersection[T](s1: CustomSet[T], s2: CustomSet[T]): CustomSet[T] = 
            new CustomSet(s1.elements.filter(CustomSet.member(s2, _)))
    
    def difference[T](s1: CustomSet[T], s2: CustomSet[T]): CustomSet[T] = 
        new CustomSet(s1.elements.filter(!CustomSet.member(s2, _)))
    
    def union[T](s1: CustomSet[T], s2: CustomSet[T]): CustomSet[T] = {
        new CustomSet(s1.elements :++ s2.elements)
    }
}   