import scala.math.min
import scala.math.max

/* ******************** Proteus : Dé ******************** */

object Dice {
	/** Juste pour implémenter quelques trucs statiques */
	val seq_roles = Array("pyramid", "pawn", "bishop", "knight", "rook", "queen")
}

class Dice(game: Game, player: Int, m_pos:Pos) extends Piece(game, player, m_pos) {
	override def role = "dice"

	var i_role : Int = 1

	/** Supprime d'une liste les postions qui correspondent à une pyramide */
/*	def protect_pyramids(moves : List[Pos]) : List[Pos] = {
		return moves.filter(
			((pos : Pos) => {
				val piece = game.board(pos.x)(pos.y)
				(piece == null) || (piece.role != "pyramid" ) || (piece.role == "dice" && Dice.seq_roles(piece.asInstanceOf[Dice].i_role) != "pyramid")
			})
		)
	}*/

	def protect_pyramids(moves : List[Pos]) : List[Pos] = {
		if(moves == List()) {
			return List()
		}
		else {
			if(game.board(moves.head.x)(moves.head.y) != null && game.board(moves.head.x)(moves.head.y).role == "dice" && Dice.seq_roles(game.board(moves.head.x)(moves.head.y).asInstanceOf[Dice].i_role) == "pyramid") {
				return protect_pyramids(moves.tail)
			}
			else {
				moves.head :: (protect_pyramids(moves.tail))
			}
		}
	}

	/** Par défaut on copie les déplacements des pièces classiques */
	override def attacked_cells() : List[Pos] = {
		val save_piece = this
		val new_piece = Dice.seq_roles(i_role) match {
			case "king" => new King(game, player, pos)
			case "queen" => new Queen(game, player, pos)
			case "rook" => new Rook(game, player, pos)
			case "bishop" => new Bishop(game, player, pos)
			case "knight" => new Knight(game, player, pos)
			case "pawn" => new Pawn_proteus(game, player, pos)
		}
		new_piece.already_moved = already_moved
		game.board(pos.x)(pos.y) = new_piece
		val ret = new_piece.attacked_cells
		game.board(pos.x)(pos.y) = save_piece
		return protect_pyramids(ret)
	}

	override def possible_move() : List[Pos] = {
		val save_piece = this
		val new_piece = Dice.seq_roles(i_role) match {
			case "king" => new King(game, player, pos)
			case "queen" => new Queen(game, player, pos)
			case "rook" => new Rook(game, player, pos)
			case "bishop" => new Bishop(game, player, pos)
			case "knight" => new Knight(game, player, pos)
			case "pawn" => new Pawn_proteus(game, player, pos)
			case "pyramid" => new Pyramid(game, player, pos)
		}
		new_piece.already_moved = already_moved
		game.board(pos.x)(pos.y) = new_piece
		val ret = new_piece.possible_move
		game.board(pos.x)(pos.y) = save_piece
		return protect_pyramids(ret)
	}
	
	/** Yolo */
	override def inCheck(new_pos : Pos) : Boolean = {
		return false
	}

	/** Change le rôle suivant une direction */
	def roll(up : Boolean) : Boolean = {
		if(up && i_role <= 4) {
			i_role = min(5, i_role + 1)
			return true
		}
		else if (!up && i_role >= 1) {
			i_role = max(0, i_role - 1)
			return true
		}
		return false
	}

	/** Les rôles étaient déjà triés par ordre croissant de points */
	def points : Int = {
		if( i_role == 0 ) {
			return 0
		}
		else {
			return (i_role + 1)
		}
	}
}

/* ******************** Proteus : Pyramide ******************** */

class Pyramid(game: Game, player: Int, m_pos: Pos) extends Piece(game, player, m_pos) {
	/** La pyramide ne fait rien, mais ne peut pas être mangée */

	override def role = "pyramid"
	override def attacked_cells() : List[Pos] = { 
		return List() 
	}
	override def possible_move() : List[Pos] = {
		return List()
	}
}

/* ******************** Proteus : Pion ******************** */

class Pawn_proteus(game:Game, player:Int, m_pos:Pos) extends Piece(game, player, m_pos) {
	/** Dans proteus, le pion peut toujours se déplacer de deux */
	def protect_pyramids(moves : List[Pos]) : List[Pos] = {
		if(moves == List()) {
			return List()
		}
		else {
			if(game.board(moves.head.x)(moves.head.y) != null && game.board(moves.head.x)(moves.head.y).role == "dice" && Dice.seq_roles(game.board(moves.head.x)(moves.head.y).asInstanceOf[Dice].i_role) == "pyramid") {
				return protect_pyramids(moves.tail)
			}
			else {
				moves.head :: (protect_pyramids(moves.tail))
			}
		}
	}
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
		return protect_pyramids(pos_move)
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

		return protect_pyramids(pos_move ++ attacked_cells)
	}
}
