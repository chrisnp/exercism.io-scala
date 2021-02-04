case class Triangle(a: Double, b: Double, c: Double) {
    
    def triangleInequality: Boolean = 
        Set(a, b, c).min > 0 && 
        (a <= b + c) && (b <= c + a) && (c <= a + b) 

    def equalSides: Int = 4 - Set(a, b, c).size

    def equilateral: Boolean =
        triangleInequality && (equalSides == 3)

    def isosceles: Boolean = 
        triangleInequality && (equalSides >= 2)

    def scalene: Boolean = 
        triangleInequality && (equalSides == 1)
}

    