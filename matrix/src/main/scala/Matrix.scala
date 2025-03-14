import scala.language.postfixOps

class Matrix(private val matrix: Seq[Seq[Int]]) {
    def row(r: Int): Seq[Int] = matrix(r)
    def column(c: Int): Seq[Int] = matrix map(_(c))
}

object Matrix {
    private def splitStr(string: String, separator: String): Seq[String] =
        string `split` separator toSeq
    def apply(str: String): Matrix = 
        new Matrix(splitStr(str, "\n") map{splitStr(_, " ") map{_ toInt}})
}
