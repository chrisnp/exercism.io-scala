object Sieve {

    private def sieve(num: Int): List[Int] = {
        
        // a stream of multiples of the odd naturals
        // up to the sq. root of num
        val compositeOdds = 
            Stream.from(3, 2)
                  .takeWhile(_ <= Math.sqrt(num).toInt)
                  .flatMap(x => Stream.from(x * x, 2 * x)
                  .takeWhile(_ <= num))

        if (num <= 1) { 
            List() // no primes below 2
        } 
        else {
        // the primes up to a num are 2 
        // and the not sieved non-composite odds less than num
            2 :: Stream.from(3, 2)
                       .takeWhile(_ <= num)
                       .diff(compositeOdds)
                       .toList
        }
    }

    def primes(num: Int): List[Int] = sieve(num)

}