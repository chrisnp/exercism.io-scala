object SumOfMultiples {
    def sum(factors: Set[Int], limit: Int): Int = 
        factors.flatMap { f => List.range(f, limit, f) }
               .foldLeft(0) {_+_}
}