object Bob {

  private def shout (str: String): Boolean = str.exists(_.isUpper) && !str.exists(_.isLower)
  private def question(str: String): Boolean = str.endsWith("?")
  private def silent(str: String): Boolean = str.isEmpty

  def response(statement: String): String = statement.trim match {
    case x if silent(x) => "Fine. Be that way!"
    case x if shout(x) && question(x) => "Calm down, I know what I'm doing!"
    case x if question(x) => "Sure."
    case x if shout(x) => "Whoa, chill out!"
    case _ => "Whatever."
  }
}
