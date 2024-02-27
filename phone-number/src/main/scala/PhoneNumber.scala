object PhoneNumber {
    def clean(number: String): Option[String] = {
        val digits = number.filter(_.isDigit).toList
        val codesOk = digits(0).asDigit > 1 && digits(3).asDigit > 1
        digits match {
            case '1' :: rest if digits.size == 11 => 
                clean(rest.mkString)
            case ds if ds.size == 10 && codesOk => 
                Some(ds.mkString)
            case _ => 
                None
        }
    }
}