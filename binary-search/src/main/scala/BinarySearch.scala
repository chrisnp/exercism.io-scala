object BinarySearch {

    def find(haystaq: Seq[Int], needle: Int): Option[Int] = {
        
        @annotation.tailrec
        def search(min: Int, max: Int): Option[Int] = {
            if (min >= max) return None
            val size = max - min
            val half = min + size / 2
            val mid = haystaq apply half

            if (haystaq(mid) == `needle`) 
                Some(mid)
            else if (mid > needle) 
                search(min, mid - 1)
            else 
                search(mid + 1, max)
        }
        
        search(0, haystaq.length - 1)
    }
}