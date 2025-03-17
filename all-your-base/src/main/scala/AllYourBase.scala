object AllYourBase {
    import scala.annotation.tailrec
    import scala.math.pow

    private 
    def inValidInput(inbase: Int, ds: Seq[Int], outbase: Int): Boolean =
        inbase < 2 || outbase < 2 || ds.exists(d => d < 0 || d >= inbase)

    private 
    def toDec(base: Int, ds: Seq[Int]): Int =
        ds
        .zipWithIndex
        .foldRight(0.0) { case ((d, i), acc) => 
                          d * pow(base, ds.length - i - 1) + acc  }
        .toInt

    @tailrec
    private 
    def convert(in: Int, xs: Seq[Int] = Seq(0)) (out: Int): Seq[Int] =
        in match {
            case 0 => if (xs != Nil) xs else Seq(0)
            case n => convert(n / out, xs.+:(n % out)) (out)
        }

    def rebase(inbase: Int, ds: Seq[Int], outbase: Int): Option[Seq[Int]] =
    {
        if (inValidInput(inbase, ds, outbase)) return None
        Some (convert(toDec(inbase, ds), Seq()) (outbase))
    }
}

