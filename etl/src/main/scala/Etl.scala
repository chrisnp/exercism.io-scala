object Etl {

def transform(legacy : Map[Int, Seq[String]]): Map[String, Int] =
    for {
        (score, letters) <- legacy;
        letter <- letters.map(_.toLowerCase)
    } yield (letter -> score)
}