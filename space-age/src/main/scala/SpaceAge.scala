object SpaceAge {

    private val round = 
        (num:Double, precision:Int) =>
            (num * (math pow (10, precision))).round / 
            (math pow (10, precision))

    private val planetAge = 
        (seconds:Double, orbitalPeriod:Double) =>
            round(seconds / (31557600 * orbitalPeriod), 2)

    val onMercury = (seconds:Double) => 
                planetAge(seconds, 0.2408467)
    val onVenus   = (seconds:Double) => 
                planetAge(seconds, 0.61519726)
    val onEarth   = (seconds:Double) => 
                planetAge(seconds, 1)
    val onMars    = (seconds:Double) => 
                planetAge(seconds, 1.8808158)
    val onJupiter = (seconds:Double) => 
                planetAge(seconds, 11.862615)
    val onSaturn  = (seconds:Double) => 
                planetAge(seconds, 29.447498)
    val onUranus  = (seconds:Double) => 
                planetAge(seconds, 84.016846)
    val onNeptune = (seconds:Double) => 
                planetAge(seconds, 164.79132)
}
