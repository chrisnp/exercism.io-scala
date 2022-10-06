object NumberType {
    sealed trait NumberType

    case object Abundant extends NumberType
    case object Perfect extends NumberType
    case object Deficient extends NumberType
}