object Clock {

    case class Clock( h:Int, m:Int ) {

        private def mod(x:Double, d:Double) = 
            (((x % d) + d) % d).toInt 

        private lazy val hours = mod((h * 60 + m) / 60.0 , 24)
        private lazy val minutes = mod(m, 60)

        def + (other:Clock):Clock = 
            new Clock(hours + other.hours, minutes + other.minutes)
        def - (other:Clock):Clock = 
            new Clock(hours - other.hours, minutes - other.minutes)
  
        override def equals(that:Any):Boolean = {
            that match {
                case that:Clock => 
                    that.isInstanceOf[Clock] && 
                    that.hours == this.hours && that.minutes == this.minutes
                case _ => 
                    false
            }
        }
    }

    def apply(hours: Int, minutes: Int) = new Clock(hours, minutes)
    def apply(minutes: Int) = new Clock(0, minutes)
}