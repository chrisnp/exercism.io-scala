object NthPrime {
    import scala.language.postfixOps
    
    private def isPrime(n : Int) :  Boolean =
        (2 to Math.sqrt(n).toInt) forall (n % _ != 0) 
    
    private lazy val primes : Stream[Int] = 
        2 #:: (Stream.from(3, 2) filter isPrime)

    def prime(n : Int) : Option[Int] = 
        if (n < 1) None 
        else Some (primes take(n) last)
}