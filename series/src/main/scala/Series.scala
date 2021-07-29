object Series {

    def windows[T](list: List[T], size: Int): List[List[T]] = {
        val window = list.take(size).toList
        if (window.length > size - 1)
            window :: windows(list.tail, size)
        else
            Nil 
    }
    
    def slices(size: Int, text: String): Seq[Seq[Int]] = {
        if (text == "") return Nil
        val list = text.split("").map(_.toInt).toList
        windows(list, size).toSeq
    }
}