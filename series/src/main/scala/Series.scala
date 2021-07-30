object Series {

    def slices(size: Int, text: String): Seq[Seq[Int]] = {
        if (text == "") return Seq()
        windows(text split("") map(_.toInt), size)
    }

    private def windows[T](alist: Seq[T], size: Int): Seq[Seq[T]] = {
        val window = alist take(size)
        if (window.length < size) Seq()
        else window +: windows(alist tail, size)
    }
}