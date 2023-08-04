class School {

  type DB = Map[Int, Seq[String]]

  private var school : DB = 
      Map().withDefaultValue(Nil)

  def add(name: String, g: Int) : Unit = 
      school += (g -> ( grade(g) :+ name ))

  def db: DB = school

  def grade(g: Int): Seq[String] = school.getOrElse(g, Nil)

  def sorted: DB = {
      val studentsInGrade = school.map {
            case (grade, names) => (grade, names.sorted)   
      }
      Map(studentsInGrade.toSeq.sortBy(_._1): _*)
  }
}

