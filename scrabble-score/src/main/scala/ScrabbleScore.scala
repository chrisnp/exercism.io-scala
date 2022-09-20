object ScrabbleScore {
    import scala.language.postfixOps
    
    def score(letter : Char) : Int = {
        letter toUpper match {
            case 'A' | 'E' | 'I' | 'O' | 'U'  =>  1
            case 'L' | 'N' | 'R' | 'S' | 'T'  =>  1
            case 'D' | 'G'                    =>  2
            case 'B' | 'C' | 'M' | 'P'        =>  3
            case 'F' | 'H' | 'V' | 'W' | 'Y'  =>  4
            case 'K'                          =>  5
            case 'J' | 'X'                    =>  8
            case 'Q' | 'Z'                    =>  10
        }
    }

    def score(word : String) : Int = 
        word map(letter => score(letter)) sum    
}