object NthPrime {

    private def isPrime(candidate: Int):  Boolean = 
        (2 to Math.sqrt(candidate).toInt).forall(candidate % _ != 0)

    private lazy val primes: Stream[Int] = 2 #:: Stream.from(3, 2)
                                                       .filter(isPrime)

    def prime(n: Int): Option[Int] = if (n < 2) 
                                        None 
                                     else 
                                        Some(primes.take(n).last)
}