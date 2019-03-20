object Pangrams {

  private val Alphabet = 'a' to 'z'

  def isPangram(input: String): Boolean = Alphabet.forall(input.toLowerCase.contains(_))

}

