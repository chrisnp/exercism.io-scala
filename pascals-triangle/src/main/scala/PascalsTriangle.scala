object PascalsTriangle {
    def rows(count: Int): Seq[Seq[Int]] = {
        (0 until count).foldRight(Seq[Seq[Int]]()) { 
            (x, acc) => (0 until x).foldLeft(Seq(1)) { 
                (r, y) => r.appended(r(y) * (x - y) / (y + 1))
            }.toSeq +: acc
        }
    }
}