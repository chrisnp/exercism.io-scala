case class Triangle(a: Double, b: Double, c: Double) {

    def triangleInequality: Boolean = 
        Set(a, b, c).min > 0 && 
        2 * Seq(a, b, c).max <= Seq(a, b, c).sum 

    def equalSides: Int = 4 - Set(a, b, c).size

    def equilateral: Boolean = 
        triangleInequality && (equalSides == 3)
    def isosceles: Boolean = 
        triangleInequality && (equalSides >= 2)
    def scalene: Boolean = 
        triangleInequality && (equalSides == 1)
}

    