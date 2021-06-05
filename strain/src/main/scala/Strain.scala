object Strain {

    def keep[A](xs : Seq[A], f : A => Boolean) : Seq[A] = 
    {
        for (x <- xs if f(x)) yield x
    }

    def discard[A](xs : Seq[A], f : A => Boolean) : Seq[A] = 
    {
        keep(xs, !f(_: A))
    }

}