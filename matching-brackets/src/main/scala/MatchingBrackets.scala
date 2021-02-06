object MatchingBrackets {

    private val brackets = 
        Set('(',')','{','}','[',']')

    def isPaired(input: String): Boolean = {
        def isPaired(brackets: List[Char], 
                     stack: List[Char]): Boolean = 
            (brackets, stack) match {
                case (List(), List()) => 
                    true
                case (('(' | '{' | '[')::xs, stk) => 
                    isPaired(xs, brackets
                                 .headOption
                                 .getOrElse(' ') +: stk)
                case ((')'::xs, '('::ys)) => 
                    isPaired(xs, ys)
                case (('}'::xs, '{'::ys)) => 
                    isPaired(xs, ys)
                case ((']'::xs, '['::ys)) => 
                    isPaired(xs, ys)
                case _ => 
                    false
            } 
        isPaired(input.filter(brackets).toList, 
                 List.empty)   
    }
}