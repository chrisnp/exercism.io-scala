object RunLengthEncoding {
    def encode(xs: String): String =
        xs.headOption match {
            case None => ""
            case Some(letter) =>
                val (ys, rest) = xs.span { _ == letter }
                val count = if (ys.length <= 1) "" 
                            else ys.length.toString
                count + letter + encode(rest)
        }

    def decode(xs: String): Char | String =
        xs.headOption match {
            case None => ""
            case Some(x) =>
                val (count, rest) = xs.span { _.isDigit }
                val acc = if (count.isEmpty) rest.head.toString
                          else rest.head.toString * count.toInt
                acc + decode(rest.tail)
        }
}