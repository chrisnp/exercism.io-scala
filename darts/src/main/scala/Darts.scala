object Darts {

    import scala.math.sqrt

    def score (x: Double, y: Double): Int = 
        sqrt(x*x + y*y) match {
            case r if r <= 1  => 10
            case r if r <= 5  => 5
            case r if r <= 10 => 1
            case _ => 0
        }
}