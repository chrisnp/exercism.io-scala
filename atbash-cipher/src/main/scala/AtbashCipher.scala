object AtbashCipher {

    private val alpha = ('a' to 'z')
    private val numeric = ('0' to '9')

    val shift = 
        (alpha.zip(alpha reverse) ++: (numeric.zip(numeric))) toMap

    def encode(string: String): String = 
        string.filter(_ isLetterOrDigit)
              .map(_ toLower)
              .map(shift)
              .sliding(5, 5)
              .map(_ mkString)
              .mkString(" ") 

    def decode(string: String): String =
        string.filter(!_.isSpaceChar)
              .map(shift)
              .mkString 

}