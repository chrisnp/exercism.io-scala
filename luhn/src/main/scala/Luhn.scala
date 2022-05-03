class Luhn (val value: Long){
    private def addend(di: (Char, Int)) = 
        di match {
            case (d, i) if i % 2 == 0 => d.asDigit
            case (d, _) if d.asDigit > 4 => 2 * d.asDigit - 9
            case (d, _) => 2 * d.asDigit 
        }
    
    def addends = value.toString.reverse.zipWithIndex.map(addend).reverse

    def checkDigit = value % 10

    def checkSum = addends.sum % 10

    def isValid = checkSum == 0

    lazy val create = (0 to 9)
                .map(10 * value + _)
                // .map(_.isValid)
                .find(_.isValid)
                .get
                .value
}

object Luhn {
    def valid(number: Long) = new Luhn(number);
}