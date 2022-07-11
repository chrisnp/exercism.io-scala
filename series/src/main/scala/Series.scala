object Series {
    import scala.language.postfixOps
    def slices(size: Int, text: String): Seq[Seq[Int]] = {
        if (text == "") return Seq()
        windows(text split("") map(_ toInt), size)
    }

    def windows[T](list: Seq[T], size: Int): Seq[Seq[T]] = {
        val window = list take(size)
        if ((window length) < size) Seq()
        else window +: windows(list tail, size)
    }
}