import scala.util.Random

case class Cipher (keyOpt: Option[String]) {

    val key = keyOpt.getOrElse(generateKey(100))
    require(key.nonEmpty && key.forall(_.isLower))

    def encode(plainText: String): String = shift(plainText, right)
    def decode(cipherText: String): String = shift(cipherText, left)

    private def generateKey(len: Int) = 
        Random.alphanumeric.filter(_ isLetter)
              .map(_ toLower).take(len).mkString

    private val right: (Int, Int) => Int = {case (x, y) => x + y}
    private val left: (Int, Int) => Int = {case (x, y) => x - y}

    private def shift(text: String, f: (Int, Int) => Int): String =
        text
        .zip(key.map(_ - 'a'))
        .map {
            case (c, offset) => ((f(c, offset) - 'a') % 26 + 'a').toChar
        }
        .mkString
}