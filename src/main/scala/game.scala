case class Pos(x : Int, y : Int)

class Game {
	/* Initialisation of the game */
	var pieces : List[Piece] = List()
	val players : Array[Player] = Array(null, null)
	var playing = 0
	var changed = () => {}

	def start = {
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

		playing = 0
		changed()
		players(playing).wait_play
	}

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

	def remove(p : Piece, pieces : List[Piece]) : List[Piece] = pieces match {
		case piece :: subpieces => if (piece.pos == p.pos) {return subpieces} else { piece :: (remove(p, subpieces)) }
		case _ => pieces
	}

	def move(p : Piece, pos : Pos) : Unit = {
		println(p.pos)
		val possibleMoves : List[Pos] = p.possible_move()
		for(position <- possibleMoves) position match {
			case position if position == pos => p.pos = pos
			case _ => Nil
		}
		playing = 1 - playing
		players(playing).wait_play
		changed()
	}

	def every_possible_move(player : Int) : List[Pos] = { 
		var pos_move : List[Pos] = List()
		for(c <- pieces) c match {
			case piece if (piece.player == player) => pos_move = pos_move ++ piece.possible_move
			case _ => ()
		}
		return pos_move
	}

	def inCheck(player : Int) : Boolean = {
		var pos_move : List[Pos] = every_possible_move(1 - player)
		var pos : Pos = Pos(-1,-1)
		for(c <- pieces) c match {
			case piece if ( (piece.role == "king") && (piece.player == player) ) => var pos = piece.pos
			case _ => ()
		}
		for(position <- pos_move) position match {
			case position if (position == pos) => return true
			case _ => ()
		}
		return false
	}

	def getPiece(i : Int, j : Int) : Piece = {
		for(c <- pieces) c match {
			case piece if (piece.pos == Pos(i,j)) => return piece
			case _ => ()
		}
		return null
	}
}

