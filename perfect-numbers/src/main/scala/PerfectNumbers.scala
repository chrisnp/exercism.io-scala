object PerfectNumbers {
    import NumberType._

    def classify(number: Int): Either[String, NumberType] = {
        if (number <= 0) 
            Left ("Classification is only possible for natural numbers.")
        else aliquotSum(number) match {
            case `number`        => Right (Perfect)
            case n if n > number => Right (Abundant) 
            case n if n < number => Right (Deficient)    
            case _               => Left  ("Other error")
        }
    }

    def aliquotSum(number: Int): Int =
        (1 to number >>> 1).filter {number % _ == 0}.foldLeft(0) {_+_}
}