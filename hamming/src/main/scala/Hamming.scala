object Hamming {

    def distance(strand0 : String, strand1 : String) : Option[Int] = {

        if (strand0.length() != strand1.length()) return None 
 
        Some((strand0, strand1)
             .zipped
             .map(_!=_)
             .foldLeft(0)((count, x) => count + (if (x) 1 else 0)))
    }
}