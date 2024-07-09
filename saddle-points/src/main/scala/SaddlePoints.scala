object Matrix {
    def saddlePoints(rows: List[List[Int]]): Set[(Int, Int)] = {
        lazy val cols = rows.transpose
        for (
            (row, rdx) <- rows.filter(_.nonEmpty).map(_.max)
                              .zipWithIndex.toSet;
            (col, cdx) <- cols.filter(_.nonEmpty).map(_.min)
                              .zipWithIndex.toSet
            if row == col
        )
        yield (rdx, cdx)
    }
}

case class Matrix(rows: List[List[Int]])  {
    val saddlePoints = Matrix.saddlePoints(rows)
}