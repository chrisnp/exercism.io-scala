case class WordCount(private val phrase : String) {
    import scala.language.postfixOps
    
    private lazy val words = 
        phrase.toLowerCase.split("\\W'|'\\W|[^\\w']+") filter(!_.isEmpty)
    
    def countWords(): Map[String, Int] = 
        words groupBy(identity) map {case(word, count) => (word, count.size)}
}