object BinarySearch {
        
    def find(haystaq: Seq[Int], needle: Int): Option[Int] = {
        def search(min: Int, max: Int): Option[Int] = {
            val size = max - min
            val mid = min + size / 2 
            if (min >= max) None
            else if (haystaq(mid) == needle) Some(mid)
            else search(min, mid)
                 .orElse(search(mid + 1, max))
        }
        if (haystaq.isEmpty) None 
        else search(0, haystaq.size)
    }
}