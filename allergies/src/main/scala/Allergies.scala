object Allergen extends Enumeration {
  type Allergen = Value
  val Eggs, Peanuts, Shellfish, Strawberries, 
      Tomatoes, Chocolate, Pollen, Cats = Value
}

object Allergies {
    import Allergen._

    def allergicTo(allergen: Allergen.Value, score: Int): Boolean =
        ((1 << allergen.id) & score) != 0

    def list(score: Int): List[Allergen] = 
        Allergen.values.filter(allergicTo(_, score)).toList
}