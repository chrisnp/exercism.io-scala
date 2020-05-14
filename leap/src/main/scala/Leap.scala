object Leap {
  def leapYear(year: Int): Boolean = {
    val isFactor: (Int) => Boolean = 
        year % _ == 0
    (isFactor(4), isFactor(100), isFactor(400)) 
    match {
      case (_, _, true) => true
      case (_, true, _) => false
      case (true, _, _) => true
      case _ => false
    }
  }
}
