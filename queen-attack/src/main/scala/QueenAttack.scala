case class Queen private (rank: Int, file: Int)

object Queen {
    def create(rank: Int, file: Int): Option[Queen] = {
        if (0 <= rank && rank <= 7 && 
            0 <= file && file <= 7) 
             Some(Queen(rank, file))
        else None
    } 
}

object QueenAttack {
    def canAttack(white: Queen, black: Queen): Boolean = 
    {
        val deltaRank = Math.abs(white.rank - black.rank)
        val deltaFile = Math.abs(white.file - black.file)

        (deltaRank, deltaFile) match {
            case (0, 0) => throw new Exception("same square")
            case (_, 0) => true 
            case (0, _) => true
            case (x, y) => x / y == 1
        }
    }
}