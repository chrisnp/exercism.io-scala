object PrimeFactors {

    def sieve(num: Long, factor: Long): Seq[Long] = {
        num % factor match {
            case _ if (num <= 1) => Seq()
            case _ if (factor * factor > num) => Seq(num) 
            case 0 => factor +: sieve(num / factor, factor)
            case _ => sieve(num, factor + 1)
        }
    }

    def factors(number: Long): Seq[Long] = sieve(number, 2)
    
}