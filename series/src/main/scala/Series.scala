object Series {

    def windows[T](list: Seq[T], size: Int): Seq[Seq[T]] = {
        val window = list.take(size)
        if (window.length > size - 1)
            window +: windows(list.tail, size)
        else
            Seq()
    }
    
    def slices(size: Int, text: String): Seq[Seq[Int]] = {
        if (text == "") return Seq()
        val list = text.split("").map(_.toInt)
        windows(list, size)
    }
}