object Twofer {

    def twofer(name : String = null): String = {

        val who = Option(name) getOrElse "you"

        s"One for ${who}, one for me."

    }
}
