class Robot {

    import Robot._
    
    var name: String = namesGen.next()
   
    def reset(): Unit = name = namesGen.next()
}

object Robot {

    import scala.util.{Random => r}

    val namesGen: Iterator[String] = r.shuffle(
        for( a1 <- 'A' to 'Z'; a2 <- 'A' to 'Z'; d1 <- 0 to 9; d2 <- 0 to 9; d3 <- 0 to 9) 
            yield {s"$a1$a2$d1$d2$d3"}).iterator
}