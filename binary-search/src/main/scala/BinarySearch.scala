object BinarySearch {

    def find(haystaq: Seq[Int], needle: Int): Option[Int] = {
        
        @annotation.tailrec
        def search(min: Int, max: Int): Option[Int] = {
            if (min >= max) return None
            val size = max - min
            val half = min + size / 2
            val mid = haystaq apply half

            haystaq(mid) match {
                case `needle` => Some(mid)
                case x if x > needle => search(min, mid - 1)
                case x if x < needle => search(mid + 1, max) 
            }
        }
        
        search(0, haystaq.length - 1)
    }
}