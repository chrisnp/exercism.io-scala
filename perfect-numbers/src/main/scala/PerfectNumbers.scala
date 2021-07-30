object PerfectNumbers {
    import NumberType._

    def classify(number: Int): Either[String, NumberType] = {
        if (number <= 0) 
            Left("Classification is only possible for natural numbers.")
        else aliquotSum(number) match {
            case a if a > number => Right(NumberType.Abundant)
            case d if d < number => Right(NumberType.Deficient)
            case _ => Right(NumberType.Perfect)
        }
    }

    def aliquotSum(number: Int): Int =
        (1 to number / 2)
        .filter {number % _ == 0} 
        .foldLeft(0) {_+_}
}