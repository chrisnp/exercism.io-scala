case class PalindromeProducts (min: Int, max: Int) {

    import scala.util.Try

    private def isPalindrome (n: Int): Boolean = 
        n == n.toString.reverse.toInt

    private def factorPairs (n: Int): Seq[(Int, Int)] = 
        for {
            x <- min.max(1) to max.min(Math.sqrt(n).floor.toInt)
            if n % x == 0
            if min <= n / x && n / x <= max
        } yield (x, n / x)

    private def palindromes (xs: Stream[Int]) = 
        for {
            x <- xs 
            if isPalindrome(x)
            if !factorPairs(x).isEmpty
        } yield (x, factorPairs(x).toSet)

    lazy val smallest: Option[(Int, Set[(Int,Int)])] = 
        Try(palindromes(
                Stream.range(Math.pow(min, 2).toInt, 
                             Math.pow(max, 2).toInt)
            ).head
        ).toOption

    lazy val largest: Option[(Int, Set[(Int,Int)])] = 
        Try(palindromes(
                Stream.range(Math.pow(max, 2).toInt, 
                             Math.pow(min, 2).toInt, -1)
            ).head
        ).toOption
}