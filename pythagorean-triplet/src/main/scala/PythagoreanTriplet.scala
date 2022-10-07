object PythagoreanTriplet {
    def pythagoreanTripletsSum(sum : Int) : Seq[(Int,Int,Int)] = {
        for { x <- 3 to sum / 3
              y = ((sum * sum) - (2 * sum * x)) / (2 * (sum - x))
              r = ((sum * sum) - (2 * sum * x)) % (2 * (sum - x))
              z = sum - x - y
              triplet = (x, y, z)
              if r == 0 && x < y 
        } yield triplet
    }
    
    def pythagoreanTriplets(start: Int, end: Int): Seq[(Int, Int, Int)] = {
        for { x <- start to end - 2
              y <- x to end - 1
              z <- y to end
              triplet = (x, y, z)
              if isPythagorean(triplet)
        } yield triplet
    }

    def isPythagorean(triplet: (Int, Int, Int)): Boolean = {
        val sq = (x: Int) => x * x
        val Seq(x: Int, y: Int, z: Int) = 
            Seq(triplet._1, triplet._2, triplet._3).sortWith(_ > _)
        2 * sq(x) == List(x, y, z).map(a => sq(a)).fold(0)(_ + _)
    }
}