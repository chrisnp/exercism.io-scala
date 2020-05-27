object Anagram {

    def findAnagrams(word: String, 
                     candidates: Seq[String]): Seq[String] = 
    {
        candidates
        .filterNot(_ equalsIgnoreCase word)
        .filter(_.sortBy(_.toLower) 
                equalsIgnoreCase 
                word.sortBy(_.toLower))
    }
}