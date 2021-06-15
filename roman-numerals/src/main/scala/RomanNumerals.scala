object RomanNumerals {
  
  def roman(number: Int): String = {
    
    var arabic = number;
    val roman = new StringBuffer()
    
    while (arabic > 0) {
      
      arabic match {
        
        case a if a >= 1000 => 
                roman.append("M");  
                arabic -= 1000
        case a if a >= 900 => 
                roman.append("CM"); 
                arabic -= 900
        case a if a >= 500 => 
                roman.append("D"); 
                arabic -= 500
        case a if a >= 400 => 
                roman.append("CD"); 
                arabic -= 400
        case a if a >= 100 => 
                roman.append("C"); 
                arabic -= 100
        case a if a >= 90 => 
                roman.append("XC"); 
                arabic -= 90
        case a if a >= 50 => 
                roman.append("L"); 
                arabic -= 50
        case a if a >= 40 => 
                roman.append("XL"); 
                arabic -= 40
        case a if a >= 10 => 
                roman.append("X"); 
                arabic -= 10
        case a if a >= 9 => 
                roman.append("IX"); 
                arabic -= 9
        case a if a >= 5 => 
                roman.append("V"); 
                arabic -= 5
        case a if a >= 4 => 
                roman.append("IV"); 
                arabic -= 4
        case a if a >  0 => 
                roman.append("I"); 
                arabic -= 1
      }
    }
    roman.toString()
  }
}
