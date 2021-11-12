class Robot {

    import Robot._
    
    var name: String = namesGen.next()
   
    def reset(): Unit = name = namesGen.next()
}

object Robot {

    import scala.util.{Random => r}

    val namesGen: Iterator[String] = r.shuffle(
        for( a1 <- 'A' to 'Z'; a2 <- 'A' to 'Z'; ds <- 0 to 999) 
            yield {s"$a1$a2$ds"}).iterator
}