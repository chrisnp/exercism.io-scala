object ReverseString {
  def reverse(str: String): String = {
    val reversedSeq =
      for (idx <- str.length to 1 by -1) 
        yield str(idx - 1)
    reversedSeq.mkString("")
  }
}

