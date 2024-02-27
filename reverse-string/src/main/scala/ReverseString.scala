object ReverseString {
  def reverse(str: String): String = {
    val reversedSeq =
      for (idx <- str.length until 0 by -1) 
        yield str(idx - 1)
    reversedSeq.mkString("")
  }
}

