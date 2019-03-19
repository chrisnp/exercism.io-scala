object CollatzConjecture {
    
    def steps(number: Int): Option[Int] = 
        Option(number).filter(_ > 0).map(iter(_).takeWhile(_ != 1).length)

    private def iter(num: Int): Stream[Int] =
        Stream.iterate(num)(next)

    private def next(num: Int): Int = 
        if(num % 2 == 0) num / 2 else (num * 3) +1
}