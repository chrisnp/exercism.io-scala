object Isogram {
    def isIsogram(str: String): Boolean = {
        val sanitized = str filter(_ isLetter) map(_ toLower)
        (sanitized.toSet size) == (sanitized size)
    }
}