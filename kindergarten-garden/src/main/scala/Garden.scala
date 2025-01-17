object Garden {
    final val plant: Map[Char, Plant.Plant] = 
        Map( 'C' -> Plant.Clover, 
             'G' -> Plant.Grass,
             'R' -> Plant.Radishes,
             'V' -> Plant.Violets )
    
    final val children: List[String] = 
        List( "Alice", "Bob", "Charlie", "David",
              "Eve", "Fred", "Ginny", "Harriet",
              "Ileana", "Joseph", "Kincaid", "Larry" )

    def defaultGarden(garden: String): Garden =
        new Garden(garden.split("\n")
                         .map(r => r.toCharArray()
                                    .map(p => plant.getOrElse(p, null))
                                                   .toList))
}

class Garden(garden: Seq[List[Plant.Plant]]) {
    def plants(name: String): List[Plant.Plant] = {
        val idx = 2 * Garden.children.indexOf(name)
        garden.foldLeft[List[Plant.Plant]] (List.empty)
                                           ((list, r) => 
                                                list ::: r.slice(idx, idx + 2))
    }
}

object Plant extends Enumeration {
  type Plant = Value
  val Clover, Grass, Radishes, Violets = Value
}