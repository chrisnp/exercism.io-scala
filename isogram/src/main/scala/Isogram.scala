object Isogram {

    def isIsogram(str: String): Boolean = {
        
        val normalized = 
            str.filter(_.isLetter).map(_.toLower)
        
        normalized.distinct.size == normalized.size
    }
}