object Luhn {
    import scala.util.Try

     def valid(number: String): Boolean = 
        Try {
            val sanitary = number.replaceAll(" ", "") 
            sanitary.length > 1 && 
            sanitary.map { 
                        case d if d.isDigit => d
                        case _ => throw new Exception( "not a digit" )
                    }
                    .reverse.zipWithIndex.map(id => (id._2, id._1))
                    .map(addend _).reverse.sum % 10 == 0
        }.getOrElse(false)  

    private def addend(indexedDigit: (Int, Char)) = {
        val idig = (indexedDigit._1, indexedDigit._2.asDigit)
        idig match {
            case (i, d) if i % 2 == 0 =>  d
            case (_, d) if d > 4      => (d << 1) - 9
            case (_, d)               =>  d << 1
        }
    }
}