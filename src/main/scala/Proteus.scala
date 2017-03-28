import scala.math.min
import scala.math.max

object Dice {
	/** Juste pour implémenter quelques trucs statiques */
	val seq_roles = Array("pyramid", "pawn", "bishop", "knight", "rook", "queen")
}

class Dice(game: Game, player: Int, m_pos:Pos) extends Piece(game, player, m_pos) {
	override def role = "dice"
	var i_role : Int = 1

	/** Supprime d'une liste les postions qui correspondent à une pyramide */
	def protect_pyramids(moves : List[Pos]) : List[Pos] = {
		return moves.filter(
			((pos : Pos) => {
				val piece = game.board(pos.x)(pos.y)
				(piece == null) || (Dice.seq_roles(piece.asInstanceOf[Dice].i_role) != "pyramid")
			})
		)
	}

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
	
	override def inCheck(new_pos : Pos) : Boolean = {
		return false
	}

	def roll(up:Boolean) : Boolean = {
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

	def point : Int = {
		if( i_role == 0 ) {
			return 0
		}
		else {
			return (i_role + 1)
		}
	}

	
}

class ProtGame() extends Game() {
	override def init = {
		for(i <- 0 to 3) {
			board(2*i)(7) = new Dice(this, 0, Pos(2*i, 7))
			board(2*i+1)(6) = new Dice(this, 0, Pos(2*i+1, 6))
			board(2*i)(1) = new Dice(this, 1, Pos(2*i, 1))
			board(2*i+1)(0) = new Dice(this, 1, Pos(2*i+1, 0))
		}
	}

	def compteur_proteus(player:Int): Int = {
		var n = 0
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				if(board(i)(j) != null && board(i)(j).player == player) {
					n = n + 1
				}
			}
		}
		return n
	}

	override def move(p:Piece, pos:Pos): Boolean = {
		val possibleMoves : List[Pos] = p.possible_move()
		if(possibleMoves.contains(pos)) {
			if(save_root == null) { // Un nouvel arbre de sauvegardes est nécessaire
				save_root = Save(Move(p.pos, pos), List())
				save_current = save_root
			}
			else {
				val new_save = Save(Move(p.pos, pos), List())
				save_current.saveList = new_save :: save_current.saveList
				save_current = new_save
				//Backup.addMoveToSave(Move(p, pos), save)
			}

		if(board(pos.x)(pos.y) != null) {
//			players(playing).points += board(pos.x)(pos.y).point
		}

		p.move_to(pos)

		//!\\ Ca fera jamais rien, qu'est-ce qu'il est sucé se passer ?
		val dir = 1 - (2*playing)
		if(in_board(pos.x, pos.y+dir) && board(pos.x)(pos.y+dir) != null
		  && board(pos.x)(pos.y+dir).asInstanceOf[Dice].i_role == 5) { ////D/QSODQSDHQS
			board(pos.x)(pos.y+dir) = null
		}

		changed()

		if(players(playing) != null && !over) {
				players(playing).wait_roll(pos)
			}
			return true
		}
		return false
	}

	def roll(pos : Pos, up : Boolean = true) : Boolean = {
		board(pos.x)(pos.y).asInstanceOf[Dice].roll(up)
		playing = 1 - playing
		turn = 1 + turn
		changed()
		players(playing).wait_play
		return true
	}

	override def over = {false}

	def winning() : Int = {
/*		if(players(playing).points > players(1-playing).points) {
			return playing
		}
		if(players(playing).points = players(1-playing).points) {
			return -1
		}
		else {
			return 1 - playing
		}
*/
		return -1
	}
	
	def over_proteus() : Boolean = {
		return every_possible_move(playing).isEmpty || compteur_proteus(playing) == 1
	}
}
