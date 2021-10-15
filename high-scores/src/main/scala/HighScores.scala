object HighScores {
    def personalTop(scores: Seq[Int]): Seq[Int] = 
        scores.sortWith(_>_).take(3)
    def latest(scores: List[Int]): Int = 
        scores.last
    def personalBest(scores: List[Int]): Int = 
        scores.max
    def report(scores: List[Int]): String = {
        val premise: String = 
            s"Your latest score was ${latest(scores)}. "
        val difference: String = 
            if (personalBest(scores) == latest(scores)) " "
            else s" ${personalBest(scores) - latest(scores)} short of "
        val conclusion: String = 
            s"That's${difference}your personal best!"
        s"${premise}${conclusion}"
    }
}