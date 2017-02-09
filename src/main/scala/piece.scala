abstract case class Piece(game : Game, player : Int, pos : Pos) {
	def role : String
	def possible_moves : List[Pos] = {
		var ret : List[Pos] = List()
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				ret = (Pos(i, j)) :: ret
			}
		}
		return ret
	}
}

class King(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	override def role = "king"
}

class Queen(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	override def role = "queen"
}

class Rook(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	override def role = "rook"
}

class Bishop(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	override def role = "bishop"
}

class Knight(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	override def role = "knight"
}

class Pawn(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	override def role = "pawn"
}

