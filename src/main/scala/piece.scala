abstract case class Piece(game : Game, player : Int, pos : Pos) {
	def role : String
}

class King(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	override def role = "King"
}

class Pawn(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	override def role = "Pawn"
}

