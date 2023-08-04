import scala.math.{cos, sin, sqrt, exp => xp}

case class ComplexNumber(real:Double = 0.0, imaginary:Double = 0.0) {
    lazy val abs: Double = sqrt(real * real + imaginary * imaginary)
    lazy val conjugate:ComplexNumber = ComplexNumber(real, -imaginary)
    def + (z: ComplexNumber): ComplexNumber = 
            ComplexNumber(real + z.real, imaginary + z.imaginary)
    def - (z: ComplexNumber): ComplexNumber = 
            ComplexNumber(real - z.real, imaginary - z.imaginary)
    def * (z: ComplexNumber): ComplexNumber =
            ComplexNumber(real * z.real - imaginary * z.imaginary, 
                          imaginary * z.real + real * z.imaginary)
    def / (z: ComplexNumber): ComplexNumber = { 
        val denom = z.abs * z.abs
        ComplexNumber((real * z.real + imaginary * z.imaginary) / denom, 
                      (imaginary * z.real - real * z.imaginary) / denom)
    }
}

object ComplexNumber {
    def exp(z: ComplexNumber): ComplexNumber = 
        ComplexNumber(xp(z.real) * cos(z.imaginary), 
                      xp(z.real) * sin(z.imaginary))
}