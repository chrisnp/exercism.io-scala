object Pangrams {
  def isPangram(input: String): Boolean =
    ('a' to 'z') == (('a' to 'z') intersect (input toLowerCase))
}
