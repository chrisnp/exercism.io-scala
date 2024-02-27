object PhoneNumber {
    def clean(number: String): Option[String] = {
        val digits = number.filter(_.isDigit).toList
        val areaCodeOk = digits.head.asDigit > 1
        val exchCodeOk = digits(3).asDigit > 1
        digits match {
            case '1' :: rest if digits.size == 11 => 
                clean(rest.mkString)
            case digs if digs.size == 10 && areaCodeOk && exchCodeOk => 
                Some(digs.mkString)
            case _ => 
                None
        }
    }
}