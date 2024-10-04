object AllYourBase {
    import scala.annotation.tailrec
    import scala.math.pow

    private 
    def inValidInput(inbase: Int, ds: List[Int], outbase: Int): Boolean =
        inbase < 2 || outbase < 2 || ds.exists(d => d < 0 || d >= inbase)

    private 
    def toDec(base: Int, ds: List[Int]): Int =
        ds
        .zipWithIndex
        .foldRight(0.0) { case ((d, i), acc) => 
                          d * pow(base, ds.length - i - 1) + acc  }
        .toInt

    @tailrec
    private 
    def convert(in: Int, xs: List[Int] =List(0)) (out: Int): List[Int] =
        in match {
            case 0 => if (xs != Nil) xs else List(0)
            case n => convert(n / out, (n % out) :: xs) (out)
        }

    def rebase(inbase: Int, ds: List[Int], outbase: Int): Option[List[Int]] =
    {
        if (inValidInput(inbase, ds, outbase)) return None
        Some (convert(toDec(inbase, ds), List()) (outbase))
    }
}

