case class Pos(x : Int, y : Int)

class Game {
	/* Initialisation of the game */
	var grid = Array.ofDim[Piece](8, 8) // Vaut soit null soit une pièce
	var pieces : List[Piece] = List()
	for(i <- 0 to 7) {
		pieces = new Pawn(this, 0, Pos(i, 6)) :: pieces
		pieces = new Pawn(this, 1, Pos(i, 1)) :: pieces
	}
	pieces =
		new King(this, 0, Pos(4, 7)) ::
		new King(this, 1, Pos(4, 0)) ::
		new Queen(this, 0, Pos(3, 7)) ::
		new Queen(this, 1, Pos(3, 0)) ::
		new Rook(this, 0, Pos(0, 7)) ::
		new Rook(this, 0, Pos(7, 7)) ::
		new Rook(this, 1, Pos(0, 0)) :: 
		new Rook(this, 1, Pos(7, 0)) ::
		new Knight(this, 0, Pos(1, 7)) ::
		new Knight(this, 0, Pos(6, 7)) ::
		new Knight(this, 1, Pos(1, 0)) ::
		new Knight(this, 1, Pos(6, 0)) ::
		new Bishop(this, 0, Pos(2, 7)) ::
		new Bishop(this, 0, Pos(5, 7)) ::
		new Bishop(this, 1, Pos(2, 0)) ::
		new Bishop(this, 1, Pos(5, 0)) ::
		pieces

	def cell_player(x : Int, y : Int) : Int = {
		for(c <- pieces) c match {
			case Piece(_, p, Pos(i, j)) if (i, j) == (x, y) => return p
			case _ => ()
		}
		return -1
	}

	def cell_role(x : Int, y : Int) : String = {
		for(c <- pieces) c match {
			case Piece(_, _, Pos(i, j)) if (i, j) == (x, y) => return c.role
			case _ => ()
		}
		return "empty"
	}

	def empty_cell(x : Int, y : Int) : Boolean = {
		for(c <- pieces) c match {
			case Piece(_, _, Pos(i, j)) if (i, j) == (x, y) => return false
			case _ => ()
		}
		return true
	}
}

