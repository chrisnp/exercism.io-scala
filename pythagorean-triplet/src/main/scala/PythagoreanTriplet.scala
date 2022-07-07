object PythagoreanTriplet {
    def isPythagorean(triplet: (Int, Int, Int)): Boolean = {
        val Seq(x: Int, y: Int, z: Int) = 
            Seq(triplet._1, triplet._2, triplet._3).sortWith(_ > _)
        x * x == y * y + z * z 
    }
    
    def pythagoreanTriplets(start: Int, end: Int): Seq[(Int, Int, Int)] = {
        for {
            x <- (start to end - 2).toSeq
            y <- x to end - 1
            z <- y to end
            triplet = (x, y, z)
            if isPythagorean(triplet)
        } yield triplet
    }
}