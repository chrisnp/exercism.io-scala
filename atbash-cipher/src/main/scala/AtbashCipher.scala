object AtbashCipher {
    import scala.language.postfixOps

    private val alpha = ('a' to 'z') toList
    private val numeric = ('0' to '9') toList

    def shift = 
        (alpha zip(alpha reverse) toMap) ++: (numeric zip(numeric) toMap)

    def encode(string: String): String = {
        val normalized = string filter(_ isLetterOrDigit) map(_ toLower)
        normalized map(shift) sliding(5, 5) map(_ mkString) mkString(" ") 
    }

    def decode(string: String): String =
        string filter(!_.isSpaceChar) map(shift) mkString 

}