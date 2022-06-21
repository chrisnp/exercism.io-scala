object RotationalCipher {
    
    def rotate(char: Char, shift: Int): Char = {
        val base = if (char.isLower) 'a' else 'A'
        (base + (char - base + shift) % 26)
        .toChar 
    }

    def rotate(plain: String, shift: Int): String = {
        if (shift == 0) return plain
        plain
        .map(ch => if (!ch.isLetter) ch else rotate(ch, shift))
        .mkString   
    }
}