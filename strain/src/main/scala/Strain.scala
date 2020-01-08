object Strain {

    def keep[A](xs : Seq[A], f : A => Boolean) : Seq[A] = {
        for (x : A <- xs if f(x)) yield x
    }

    def discard[A](xs : Seq[A], f : A => Boolean) : Seq[A] = {
        for (x : A <- xs if !f(x)) yield x
    }

}