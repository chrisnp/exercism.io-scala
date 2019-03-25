case class Triangle(sideA: Double, sideB: Double, sideC: Double) {
    def triangleInequality: Boolean = 
        Set(sideA, sideB, sideC).min > 0 &&
        (sideA <= sideB + sideC) &&
        (sideB <= sideC + sideC) &&
        (sideC <= sideA + sideB) 

    def equalSides: Int = 
        4 - Set(sideA, sideB, sideC).size

    def equilateral: Boolean =
        triangleInequality && (equalSides == 3)

    def isosceles: Boolean = 
        triangleInequality && (equalSides >= 2)

    def scalene: Boolean = 
        triangleInequality && (equalSides == 1)
}

    