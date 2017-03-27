import scala.math.abs

abstract case class Piece(game : Game, player : Int, var pos : Pos) {
	var already_moved : Int = -1
	var pawn_rules : Boolean = false
	def role : String

	def copy_for(new_game : Game) : Piece = {
		val ret = role match {
			case "king" => new King(new_game, player, pos)
			case "queen" => new Queen(new_game, player, pos)
			case "rook" => new Rook(new_game, player, pos)
			case "bishop" => new Bishop(new_game, player, pos)
			case "knight" => new Knight(new_game, player, pos)
			case "pawn" => new Pawn(new_game, player, pos)
		}
		ret.already_moved = already_moved
		ret.pawn_rules = pawn_rules
		return ret
	}

	def attacked_cells() : List[Pos]
	def possible_move() : List[Pos] = {
		attacked_cells()
	}

	def move_to(new_pos : Pos) {
		game.board(new_pos.x)(new_pos.y) = this
		game.board(pos.x)(pos.y) = null
		pos = new_pos
	}
	
	def inCheck(new_pos : Pos) : Boolean = {
		val g_clone = game.copy
		val this_clone = g_clone.getPiece(pos.x, pos.y)
		if(this_clone != null) {
		   this_clone.move_to(new_pos)
		}
		return g_clone.inCheck(player)
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
class Pyramid(game: Game, player: Int, m_pos: Pos) extends Piece(game, player, m_pos) {
	override def role = "pyramid"
	override def attacked_cells() : List[Pos] = { 
		return List() 
	}
	override def possible_move() : List[Pos] = {
		return List()
	}
}

class King(game : Game, player : Int, m_pos : Pos) extends Piece(game, player, m_pos) {
	override def role = "king"

	// les mouvements standarts
	override def attacked_cells() : List[Pos] = {
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
		return pos_move
	}

	// Avec le rock
	override def possible_move() : List[Pos] = {
		val x = pos.x
		val y = pos.y
		var pos_move = attacked_cells

		if ( already_moved == -1) {
			/* petit roque */
			if( game.board(x+3)(y) != null && game.board(x+3)(y).already_moved == -1 ) // La tour n'a pas bougé
			if( game.empty_cell(x + 1, y) && game.empty_cell(x + 2, y)) // C'est vide entre le roi et la tour
			if( !game.isControlledCell(x + 1, y, player) && !game.isControlledCell(x + 2, y, player)
			  && !game.isControlledCell(x,y, player) && !game.isControlledCell(x + 3, y, player)) { // Aucune des cases entre n'est en échec
				pos_move = Pos(x+2, y)::pos_move			
			}

			/* Grand roque */
			if( game.board(x-4)(y) != null && game.board(x-4)(y).already_moved == -1 ) // La tour n'a pas bougé
			if( game.empty_cell(x-1, y) && game.empty_cell(x-2, y) && game.empty_cell(x-3, y) ) // C'est vide
			if( !game.isControlledCell(x-1, y, player) && !game.isControlledCell(x-2, y, player)
			  && !game.isControlledCell(x, y, player) && !game.isControlledCell(x-3, y, player) && !game.isControlledCell(x-4, y, player)) { // Il n'y a pas de case en échec
				pos_move = Pos(x-3, y)::pos_move
			}
		}
		return pos_move
	}

	override def move_to(new_pos : Pos) {
		/* Petit roque */
		if(role == "king" && pos.x - new_pos.x == -2) {
			game.board(pos.x+3)(pos.y).move_to(Pos(pos.x+1, pos.y))
		}
		/* Grand roque */
		if(role == "king" && pos.x - new_pos.x == 3) {
			game.board(pos.x-4)(pos.y).move_to(Pos(pos.x-2, pos.y))
		}

		super.move_to(new_pos)
	}
}

class Queen(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	override def role = "queen"
	override def attacked_cells() : List[Pos] = {
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
	override def attacked_cells() : List[Pos] = annexe_possible_move(1,0) ++ annexe_possible_move(0,1) ++ annexe_possible_move(-1,0) ++ annexe_possible_move(0,-1)
}

class Bishop(game : Game, player : Int, pos : Pos) extends Piece(game, player, pos) {
	override def role = "bishop"
	override def attacked_cells : List[Pos] =
		annexe_possible_move((1,1)) ++
		annexe_possible_move((-1,1)) ++
		annexe_possible_move((-1,-1)) ++
		annexe_possible_move((1,-1))
}

class Knight(game : Game, player : Int, m_pos : Pos) extends Piece(game, player, m_pos) {
	override def role = "knight"
	override def attacked_cells() : List[Pos] = {
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

	override def attacked_cells() : List[Pos] = {
		var x = pos.x
		var y = pos.y
		var pos_move: List[Pos] = List()
		var vecteur : Int = -1+2*player
		
		/* Mouvement de base */
		if(in_board(x+vecteur,y+vecteur) && (game.cell_player(x+vecteur,y+vecteur) == 1-player)) {
			pos_move = Pos(x+vecteur, y+vecteur)::pos_move
		}
		if(in_board(x-vecteur,y+vecteur) && (game.cell_player(x-vecteur,y+vecteur) == 1-player)) {
			pos_move = Pos(x-vecteur, y+vecteur)::pos_move
		}
		
		/* Règle de la prise en passant */
		if( in_board(x+1,y) && game.board(x+1)(y) != null ) {
			if( game.board(x+1)(y).role == "pawn"
			  && game.board(x+1)(y).pawn_rules == true && game.board(x+1)(y).already_moved == (game.turn -1)
			  && game.board(x+1)(y).player == (1-player)) {
				pos_move = Pos(x+1, y+vecteur)::pos_move
			}
		}
		if( in_board(x-1,y) && game.board(x-1)(y) != null ) {
			if( game.board(x-1)(y).role == "pawn"
			  && game.board(x-1)(y).pawn_rules == true && game.board(x-1)(y).already_moved == (game.turn-1)
			  && game.board(x-1)(y).player == (1-player) ) {
				pos_move = Pos(x-1, y+vecteur)::pos_move
			}
		}

		return pos_move
	}
	override def possible_move() : List[Pos] = {
		var x = pos.x
		var y = pos.y
		var pos_move: List[Pos] = List()
		var vecteur : Int = -1+2*player

		// Déplacement standart
		if(in_board(x,y+vecteur) && game.empty_cell(x, y+vecteur)) {
			pos_move = Pos(x, y+vecteur)::pos_move
		}

		// Déplacement double
		if(already_moved == -1)
		if(in_board(x,y+2*vecteur) && game.empty_cell(x,y+2*vecteur) && game.empty_cell(x,y+vecteur)) {
			pos_move = Pos(x, y+2*vecteur)::pos_move
		}

		return pos_move ++ attacked_cells
	}
	
	override def move_to(new_pos : Pos) {
		val dir_y = -1 + 2*player
		if(abs(pos.y - new_pos.y) == 2) { // Le pion peut être mangé par prise en passant
			pawn_rules = true
		}
		else if(abs(pos.x - new_pos.x) == 1 // Un mouvement diagonal
		  && game.board(new_pos.x)(new_pos.y - dir_y) != null
		  && game.board(new_pos.x)(new_pos.y - dir_y).role == "pawn" // Avec un piont à côté
		  && game.board(new_pos.x)(new_pos.y) == null ) { // Dont la destination est vide
			game.board(new_pos.x)(new_pos.y - dir_y) = null
		}
  
		super.move_to(new_pos)
	}
}

class Pawn_proteus(game:Game, player:Int, m_pos:Pos) extends Piece(game, player, m_pos) {
	override def role = "pawn"
	override def attacked_cells() : List[Pos] = {
		var x = pos.x
		var y = pos.y
		var pos_move: List[Pos] = List()
		var vecteur : Int = -1+2*player
		
		/* Mouvement de base */
		if(in_board(x+vecteur,y+vecteur) && (game.cell_player(x+vecteur,y+vecteur) == 1-player)) {
			pos_move = Pos(x+vecteur, y+vecteur)::pos_move
		}
		if(in_board(x-vecteur,y+vecteur) && (game.cell_player(x-vecteur,y+vecteur) == 1-player)) {
			pos_move = Pos(x-vecteur, y+vecteur)::pos_move
		}
		return pos_move
	}
	override def possible_move() : List[Pos] = {
		var x = pos.x
		var y = pos.y
		var pos_move: List[Pos] = List()
		var vecteur : Int = -1+2*player

		// Déplacement standart
		if(in_board(x,y+vecteur) && game.empty_cell(x, y+vecteur)) {
			pos_move = Pos(x, y+vecteur)::pos_move
		}

		// Déplacement double
		if(in_board(x,y+2*vecteur) && game.empty_cell(x,y+2*vecteur) && game.empty_cell(x,y+vecteur)) {
			pos_move = Pos(x, y+2*vecteur)::pos_move
		}

		return pos_move ++ attacked_cells
	}
}

