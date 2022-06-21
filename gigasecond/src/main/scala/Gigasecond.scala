import java.time.LocalDate
import java.time.LocalDateTime

object Gigasecond {

  final val GIGASECOND = Math pow(10, 9) toLong 

  def add(startDate: LocalDate): LocalDateTime = 
        startDate atStartOfDay() plusSeconds(GIGASECOND)

  def add(startDateTime: LocalDateTime): LocalDateTime = 
        startDateTime plusSeconds(GIGASECOND)
}
