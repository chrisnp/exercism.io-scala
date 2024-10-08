object DifferenceOfSquares {
  private def square(n: Int):Int = n * n
  def sumOfSquares(n: Int): Int = (1 to n) map( x => square(x) ) sum
  def squareOfSum(n: Int): Int = square( (1 to n ) sum )
  def differenceOfSquares(n: Int): Int = squareOfSum(n) - sumOfSquares(n)
}
