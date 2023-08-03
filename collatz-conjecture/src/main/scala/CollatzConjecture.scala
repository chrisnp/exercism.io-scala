import scala.language.postfixOps

object CollatzConjecture {

    def steps(number: Int): Option[Int] = 
        Option(number) filter(_ > 0) map(iter(_) takeWhile(_ != 1) length)

    private def iter(num: Int): Stream[Int] =
        Stream.iterate(num) (n => if (n % 2 == 0) n >>> 1 else n * 3 + 1)
}