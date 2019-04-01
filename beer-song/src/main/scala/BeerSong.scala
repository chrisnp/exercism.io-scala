object BeerSong {

    private def firstLine(bottles: Int): String = {
        bottles match {
            case 0 => "No more bottles of beer on the wall, no more bottles of beer."
            case 1 => "1 bottle of beer on the wall, 1 bottle of beer."
            case _ => s"$bottles bottles of beer on the wall, $bottles bottles of beer."
        }
    }

    private def secondLine(bottles: Int): String = {
        bottles match {
            case 0 => "Go to the store and buy some more, 99 bottles of beer on the wall."
            case 1 => "Take it down and pass it around, no more bottles of beer on the wall."
            case 2 => s"Take one down and pass it around, ${bottles - 1} bottle of beer on the wall."
            case _ => s"Take one down and pass it around, ${bottles - 1} bottles of beer on the wall."
        }
    }

    private def verse(bottles: Int): String = {
        firstLine(bottles) + "\n" + secondLine(bottles) + "\n"
    }

    def recite(beersUp:Int, beersDown:Int): String = {
        (beersUp until (beersUp - beersDown) by -1).map(bottles => verse(bottles)).mkString("\n")
    }
 }