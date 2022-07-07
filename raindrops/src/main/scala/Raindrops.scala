object Raindrops {

  def convert(n: Int): String = {
    val dropz = Map[Int, String]( 3 -> "Pling", 
                                  5 -> "Plang", 
                                  7 -> "Plong")
                .filter(n % _._1 == 0)
                .values
                .reduceOption {_++_}
                .getOrElse("")
    if (dropz.isEmpty) n.toString
    else dropz
  } 
}