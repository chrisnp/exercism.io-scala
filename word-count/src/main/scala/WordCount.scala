case class WordCount(private val phrase : String) {

    private lazy val words = phrase
                             .toLowerCase
                             .split("\\W'|'\\W|[^\\w']+")
                             .filter(!_.isEmpty)

    def countWords() : Map[String, Int] = words
                                          .groupBy(identity)
                                          .mapValues(_.size)
}