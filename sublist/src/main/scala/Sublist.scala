object Sublist extends Enumeration { 
    type ListOverLap = Value
    val Equal, Sublist, Superlist, Unequal = Value

    implicit class Compare(sup: List[_]) {
        def contained(sub: List[_]) =
            if (sub.isEmpty) 
                1
            else 
                sup.sliding(sub.size, 1)
                   .count(_ == sub)
    }

    def sublist(xs: List[_], ys: List[_]): ListOverLap =
        (xs.contained(ys), ys.contained(xs)) match {
            case (1, 1) => Equal
            case (1, 0) => Superlist
            case (0, 1) => Sublist
            case (_, _) => Unequal
        }
}



