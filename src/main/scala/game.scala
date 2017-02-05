case class Pos(x : Int, y : Int)

class Game {
	/* Initialisation of the game */
	var pieces : List[Piece] = List()
	for(i <- 0 to 7) {
		pieces = new Pawn(this, 0, Pos(i, 6)) :: pieces
		pieces = new Pawn(this, 1, Pos(i, 1)) :: pieces
	}

	def empty_cell(x : Int, y : Int) : Boolean = {
		for(c <- pieces) c match {
			case Piece(_, _, Pos(i, j)) if (i, j) == (x, y) => return false
			case _ => ()
		}
		return true
	}
}
