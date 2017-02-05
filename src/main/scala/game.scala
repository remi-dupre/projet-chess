case class Pos(x : Int, y : Int)

class Game {
	var pieces : List[Piece] = List()

	/* Initialisation of the game */
	for(i <- 0 to 7) {
		pieces = new Pawn(this, 0, Pos(1, i)) :: pieces
		pieces = new Pawn(this, 1, Pos(5, i)) :: pieces
	}

	def empty_cell(x : Int, y : Int) : Boolean = {
		for(c <- pieces) c match {
			case Piece(_, _, Pos(x, y)) => return false
			case _ => ()
		}
		return true
	}
}
