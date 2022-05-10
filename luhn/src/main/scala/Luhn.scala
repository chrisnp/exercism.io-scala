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
                    .reverse
                    .zipWithIndex
                    .map(id => (id._2, id._1))
                    .map(addend _)
                    .reverse
                    .sum % 10 == 0
        }.getOrElse(false)  

    private def addend(indexedDigit: (Int, Char)) = 
        indexedDigit match {
            case (i, d) if i % 2 == 0    => d.asDigit
            case (_, d) if d.asDigit > 4 => 2 * d.asDigit - 9
            case (_, d)                  => 2 * d.asDigit 
        }
}