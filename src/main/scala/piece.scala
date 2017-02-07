abstract case class Piece(game : Game, player : Int, pos : Pos) {
	def role : String
	def possible_move : List [Pos]
	def In_board(x : Int, y: Int) : Boolean =
		return ((0 <= x)&&(x <= 8)&&(0 <= y)&&(y <= 8))
	def annexe_possible_move(direction:(Int,Int)) : List [Pos] = /* direction appartient à [-1..1]²\{0,0} , et renvoie */
		val (i,j) = direction									/* la liste des moves possibles dans une direction */
		val x = pos.x
		val y = pos.y
		var pos_move = List()
		var bool = true
		for(k <- 1 to 8) {
			if(bool && In_board(x+k*i,y+k*i) ) {
				if( game.cell_player(x+k*i, y+k*i) == -1 ) {
					pos_move = Pos(x+k*i, y+k*i)::pos_move
				}
				else{
					if( player != game.cell_player(x+k*i, y+k*i) ) {
						pos_move = Pos(x+k*i, y+k*i)::pos_move
						bool = false
					}
					else{
						bool = false
					}
				}
			}
		} 
		return pos_move


}

class King(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	override def role = "king"
	override def possible_move = {
		val x = pos.x
		val y = pos.y
		var pos_move = List()
		for(i <- 0 to 2) {
			for(j <- 0 to 2) {
				if(((i,j) != (1,1)) && In_board(x+i-1,y+j-1)) {
					pos_move = Pos(x+i-1,y+j-1)::pos_move
				}
			}
		}
		for(piece <- game) {
			if(pos_move.contains(piece.pos) && player == piece.player) {
				remove(piece.pos,game)
			}
		}
		return pos_move /* A ne pas mettre en echec le roi */
	}
}

class Queen(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	override def role = "queen"
	override def possible_move = {
		var pos_move = List()
		for(i <- 0 to 2) {
			for(j <- 0 to 2) {
				if((i,j) != (1,1)) {}
					pos_move = pos_move ++ annexe_possible_move((i-1,j-1))
			}
		}
		return pos_move
	}
}

class Rook(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	override def role = "rook"
	override def possible_move = {
		var pos_move = List()
		pos_move = annexe_possible_move(1,0) ++ annexe_possible_move(0,1) ++ annexe_possible_move(-1,0) ++ annexe_possible_move(0,-1)
		return pos_move
	}
}

class Bishop(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	override def role = "bishop"
	override def possible_move = {
		var pos_move = List()
		pos_move = annexe_possible_move(1,1) ++ annexe_possible_move(-1,1) ++ annexe_possible_move(-1,-1) ++ annexe_possible_move(1,-1)
		return pos_move
	}
}

class Knight(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	override def role = "knight"
	override def possible_move = {
		var pos_move = List()
		var list_move = List((2,1),(1,2),(2,-1),(-1,2),(-2,1),(1,-2),(-2,-1),(-1,-2))
		for(moves <- list_move) {
			var (x,y) = moves
			if(In_board((x,y)) && game.cell_player(x,y) != player ) {
				pos_move = Pos(x,y)::pos_move
			}
		}
		return pos_move
	}
}

class Pawn(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	already_moved = false
	override def role = "pawn"
	override def possible_move = {
		var x = pos.x
		var y = pos.y
		var pos_move = List()
		if(In_board(x+1,y)&&game.empty_cell(x+1, y)) {
			pos_move = Pos(x+1,y)::pos_move
		}
		if(In_board(x-1,y+1) && (game.cell_player(x-1,y+1) = 1-player)) {
			pos_move = Pos(x-1,y+1)::pos_move
		}
		if(In_board(x+1,y+1) && (game.cell_player(x+1,y+1) = 1-player)) {
			pos_move = Pos(x+1,y+1)::pos_move
		}
		if(!already_move && In_board(x+2,y) && game.empty_cell(x+2,y)) {
			pos_move = Pos(x+2,y)::pos_move
		}
		return pos_move
	}
}
