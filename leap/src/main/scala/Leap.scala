object Leap {
  def leapYear(year: Int): Boolean = {
    val dividedBy: (Int) => Boolean = 
        year % _ == 0
    (dividedBy(4), dividedBy(100), dividedBy(400)) 
    match {
      case (_, _, true) => true
      case (_, true, _) => false
      case (true, _, _) => true
      case _            => false
    }
  }
}
