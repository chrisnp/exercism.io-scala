object RunLengthEncoding {
    def encode(xs: String): String =
        xs.headOption match {
            case None => ""
            case Some(letter) =>
                val (ys, rest) = xs.span { _ == letter }
                val cnt = if (ys.length <= 1) ""
                          else ys.length.toString
                cnt + letter + encode(rest)
        }

    def decode(xs: String): Char | String =
        xs.headOption match {
            case None => ""
            case Some(x) =>
                val (cnt, rest) = xs.span { _.isDigit }
                val acc = if (cnt.isEmpty) rest.head.toString
                          else rest.head.toString * cnt.toInt
                acc + decode(rest.tail)
        }
}