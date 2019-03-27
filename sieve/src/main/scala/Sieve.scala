object Sieve {

    private def sieve: Seq[Int] => Seq[Int] = {
        case head +: tail => head +: sieve(tail filter (_ % head > 0))
        case Nil => Nil
    }

    def primes(num: Int): Seq[Int] = sieve(2 to num)

}