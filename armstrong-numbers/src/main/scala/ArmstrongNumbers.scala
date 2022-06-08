object ArmstrongNumbers {
    def isArmstrongNumber(num: Int): Boolean = {
        num == 
        num.toString().map(_ asDigit).map(Math.pow(_, num.toString.length) toInt).sum
    }
}