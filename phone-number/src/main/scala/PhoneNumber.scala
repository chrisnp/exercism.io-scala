object PhoneNumber {
    def clean(number: String): Option[String] = {
        val digits = number.filter(_.isDigit).toList
        val areaCodeOk = digits(0) > 1
        val exchCodeOk = digits(3) > 1
        digits match {
            case '1' :: rest if digits.size == 11 => clean(rest.mkString)
            case listDigits if listDigits.size == 10 && 
                 listDigits.head.asDigit > 1 && 
                 listDigits(3).asDigit > 1 => Some(listDigits.mkString)
            case _ => None
        }
    }
}