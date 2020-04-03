object Bob {

    private def shout (str: String): Int = 
        if (str.exists(_.isUpper) && 
            !str.exists(_.isLower)) 1 else 0

    private def question(str: String): Int = 
        if (str.trim.endsWith("?")) 1 else 0

    private def silent(str: String): Int = 
        if (str.trim.isEmpty) 1 else 0

    def response(query: String): String = 
      (shout(query), question(query), silent(query)) 
      match {
        case (1, 1, 0) => 
            "Calm down, I know what I'm doing!"
        case (1, 0, 0) => "Whoa, chill out!"
        case (0, 1, 0) => "Sure."
        case (0, 0, 1) => "Fine. Be that way!"
        case (_, _, _) => "Whatever."
      }
}
