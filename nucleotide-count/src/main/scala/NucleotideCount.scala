case class DNA(sequence : String) {
  
    val nucleotideCounts : Either[String, Map[Char, Int]] = 
    {   
        if (sequence forall("ACGT" contains(_))) 
            Right( Map('A' -> 0,'C' -> 0,'G' -> 0,'T' -> 0) 
                   ++ 
                   sequence.groupBy(identity).mapValues(_ size) )
        else
            Left( "Invalid nucleotide(s) in sequence" )
    }
}