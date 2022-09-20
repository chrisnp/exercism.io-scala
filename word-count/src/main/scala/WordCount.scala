case class WordCount(private val phrase : String) {

    private lazy val words = 
        phrase.toLowerCase.split("\\W'|'\\W|[^\\w']+")
              .filter(!_.isEmpty)

    def countWords() : Map[String, Int] = 
        words.groupBy(identity).view.mapValues(_.size)
             .toMap
}