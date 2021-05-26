import scala.util.Try

object RnaTranscription {

    def rnaComplement(nuc: Char): Char = {
        nuc match {
            case 'A' => 'U'
            case 'C' => 'G'
            case 'G' => 'C'
            case 'T' => 'A'
        }
    }

    def toRna(dna: String): Option[String] = 
        Try { dna.map(rnaComplement) }.toOption
}