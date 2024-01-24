object CryptoSquare {
    def ciphertext(msg: String): String = {
        val sanitary: String = 
            msg.filter(_.isLetterOrDigit).toLowerCase
        val sqrt: Double = math.ceil(math.sqrt(sanitary.size))
        val square: (Int, Int) = 
            (sqrt.toInt, math.ceil(sanitary.size / sqrt).toInt)
        sanitary.isEmpty match {
            case true => sanitary
            case _ => sanitary.padTo(square._1 * square._2, " ")
                              .grouped(square._1)
                              .toSeq.transpose
                              .map(_.mkString).mkString(" ")
        }
    }
}