object ProteinTranslation {

    private def translate(codon: String): String =
        codon match {
            case "AUG" => "Methionine"
            case "UUU" | "UUC" => "Phenylalanine"
            case "UUA" | "UUG" => "Leucine"
            case "UCU" | "UCC" | "UCA" | "UCG" => "Serine"
            case "UAU" | "UAC" => "Tyrosine"
            case "UGU" | "UGC" => "Cysteine"
            case "UGG" => "Tryptophan"
            case "UAA" | "UAG" | "UGA" => "STOP"
            case _ => 
                throw new IllegalArgumentException("Unknown codon")
        }

    def proteins(strand: String): Seq[String] = 
        strand grouped(3) takeWhile(translate(_) != "STOP") map(translate _) toSeq
}