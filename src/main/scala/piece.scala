abstract case class Piece(game : Game, player : Int, var pos : Pos) {
	var already_moved : Int = -1
	var Pawn_Rules : Boolean = false
	def role : String
	def possible_move() : List[Pos]

	
	def inCheck(new_pos : Pos) : Boolean = {
        val g2 = game.copy
        val this2 = g2.getPiece(pos.x, pos.y)
        g2.board(new_pos.x)(new_pos.y) = this2
        g2.remove(this2)
        this2.pos = new_pos
		return g2.inCheck(player)
	}

	def removeInCheckMoves(pos_move : List[Pos]) : List[Pos] = {
		var new_pos_move : List[Pos] = List()
		for(new_pos <- pos_move) {
			if( !inCheck(new_pos) ) {
				new_pos_move = new_pos::new_pos_move
			}
		}
		return new_pos_move
	}

	def in_board(x : Int, y: Int) : Boolean = {
		return ((0 <= x) && (x < 8) && (0 <= y) && (y < 8))
	}

	def annexe_possible_move(direction : (Int,Int)) : List [Pos] = {
		/* direction appartient à [-1..1]²\{0,0} , et renvoie */
		val (i,j) = direction // la liste des moves possibles dans une direction
		val Pos(x, y) = pos
		var pos_move: List[Pos] = List()
		var clear = true
		for(k <- 1 to 7) {
			val (a, b) = (x+(k*i), y+(k*j))
			if(clear && in_board(a, b) ) {
				if( game.cell_player(a, b) == -1 ) {
					pos_move = Pos(a, b)::pos_move
				}
				else{
					clear = false
					if( player != game.cell_player(a, b) ) {
						pos_move = Pos(a, b)::pos_move
					}
				}
			}
		} 
		return pos_move
	}
}

class King(game : Game, player : Int, m_pos : Pos) extends Piece(game, player, m_pos) {
	override def role = "king"
	override def possible_move() : List[Pos] = {
		val x = pos.x
		val y = pos.y
		var pos_move: List[Pos] = List()
		for(i <- -1 to 1) {
			for(j <- -1 to 1) {
				if(((i,j) != (0,0)) && in_board(x+i, y+j) && game.cell_player(x+i, y+j) != player) {
					pos_move = Pos(x+i, y+j)::pos_move
				}
			}
		}
		if ( already_moved == -1) {
			/* petit roque */
			if (game.empty_cell(x + 1, y) && game.empty_cell(x + 2, y) && !game.getControlledCell(x + 1, y, player) && !game.getControlledCell(x + 2, y, player) && !game.getControlledCell(x,y, player) && !game.getControlledCell(x + 3, y, player)) {
				pos_move = pos_move			
			}
			/* Grand roque */
			if (game.empty_cell(x - 1, y) && game.empty_cell(x - 2, y) && game.empty_cell(x - 3, y) && !game.getControlledCell(x - 1, y, player) && !game.getControlledCell(x - 2, y, player) && !game.getControlledCell(x,y, player) && !game.getControlledCell(x-3, y, player) && !game.getControlledCell(x-4, y, player)) {
				pos_move = pos_move
			}
		}
		return pos_move /* A ne pas mettre en echec le roi */
	}
}

class Queen(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	override def role = "queen"
	override def possible_move() : List[Pos] = {
		var pos_move: List[Pos] = List()
		for(i <- 0 to 2) {
			for(j <- 0 to 2) {
				if((i,j) != (1,1)) {
					pos_move = pos_move ++ annexe_possible_move((i-1,j-1))
				}
			}
		}
		return pos_move
	}
}

class Rook(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	override def role = "rook"
	override def possible_move() : List[Pos] = annexe_possible_move(1,0) ++ annexe_possible_move(0,1) ++ annexe_possible_move(-1,0) ++ annexe_possible_move(0,-1)
}

class Bishop(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	override def role = "bishop"
	override def possible_move : List[Pos] = annexe_possible_move((1,1)) ++ annexe_possible_move((-1,1)) ++ annexe_possible_move((-1,-1)) ++ annexe_possible_move((1,-1))
}

class Knight(game : Game, player : Int, m_pos : Pos) extends Piece(game, player, m_pos) {
	override def role = "knight"
	override def possible_move() : List[Pos] = {
		var x = pos.x
		var y = pos.y
		var pos_move: List[Pos] = List()
		var list_move = List((2,1),(1,2),(2,-1),(-1,2),(-2,1),(1,-2),(-2,-1),(-1,-2))
		for(moves <- list_move) {
			var (i,j) = moves
			if(in_board(x+i,y+j) && game.cell_player(x+i,y+j) != player ) {
				pos_move = Pos(x+i,y+j)::pos_move
			}
		}
		return pos_move
	}
}

class Pawn(game : Game, player : Int, m_pos : Pos) extends Piece(game, player, m_pos) {
	override def role = "pawn"
	override def possible_move() : List[Pos] = {
		var x = pos.x
		var y = pos.y
		var pos_move: List[Pos] = List()
		var vecteur : Int = -1+2*player
	/* mouvement de base */
		if(in_board(x,y+vecteur)&&game.empty_cell(x, y+vecteur)) {
			pos_move = Pos(x,y+vecteur)::pos_move
		}
		if(in_board(x+vecteur,y+vecteur) && (game.cell_player(x+vecteur,y+vecteur) == 1-player)) {
			pos_move = Pos(x+vecteur,y+vecteur)::pos_move
		}
		if(in_board(x-vecteur,y+vecteur) && (game.cell_player(x-vecteur,y+vecteur) == 1-player)) {
			pos_move = Pos(x-vecteur,y+vecteur)::pos_move
		}
		if( already_moved == -1 && in_board(x,y+2*vecteur) && game.empty_cell(x,y+2*vecteur) && game.empty_cell(x,y+vecteur)) {
			pos_move = Pos(x,y+2*vecteur)::pos_move
		}
	/* Règle de la prise en passant */
		if (in_board(x+1,y) && game.board(x+1)(y) != null) {
			if(game.board(x+1)(y).role == "pawn" && game.board(x+1)(y).Pawn_Rules == true && game.board(x+1)(y).already_moved == (game.turn -1) && game.board(x+1)(y).player == (1 - player)) {
				pos_move = Pos(x+1,y+vecteur)::pos_move
			}
		}
		if (in_board(x-1,y) && game.board(x-1)(y) != null) {
			if(game.board(x-1)(y).role == "pawn" && game.board(x-1)(y).Pawn_Rules == true && game.board(x-1)(y).already_moved == (game.turn -1) && game.board(x-1)(y).player == (1 - player)) {
				pos_move = Pos(x-1,y+vecteur)::pos_move
			}
		}

		return pos_move
	}

}

