object FlattenArray {
    def flatten(list: List[_]): List[_] = {
        list.flatMap (elem => elem match {
                case xs : List[_] => flatten(xs)
                case null         => List() 
                case x            => List(x)
            }
        )
    }
}