object Acronym {

  def abbreviate(phrase: String): String =
    "(\\w|\')+".r findAllIn(phrase) map(x => x(0) toUpper) mkString
}
