import scala.math.min
import scala.math.max

class Dice(game: Game, player: Int, m_pos:Pos) extends Piece(game, player, m_pos) {
	override def role = "dice"
	val seq_roles = Array("pyramid", "pawn", "bishop", "knight", "rook", "queen")
	var i_role : Int = 1
	if(i_role == 0) {
		this.asInstanceOf[Pyramid]
	}
	if(i_role == 1) {
		this.asInstanceOf[Pawn_proteus]
	}
	if(i_role == 2) {
		this.asInstanceOf[Bishop]
	}
	if(i_role == 3) {
		this.asInstanceOf[Knight]
	}
	if(i_role == 4) {
		this.asInstanceOf[Rook]
	}
	if(i_role == 5) {
		this.asInstanceOf[Queen]
	}

	override def attacked_cells() : List[Pos] = {
		return List()
	}
	def rotateUp = {
		i_role = min(5, i_role + 1)
	}
	def rotateDown = {
		i_role = max(0, i_role - 1)
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
	def init_proteus = {
		for(i <- 0 to 3) {
			board(2*i)(6) = new Dice(this, 0, Pos(2*i, 6))
			board(2*i+1)(7) = new Dice(this, 0, Pos(2*i+1, 7))
			board(2*i)(0) = new Dice(this, 1, Pos(2*i, 0))
			board(2*i+1)(1) = new Dice(this, 1, Pos(2*i, 0))
		}
	}

	def start_proteus = {
		init_proteus
		playing = 0
		changed()
		players(playing).wait_play
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
	def over_proteus() : Boolean = {
		return every_possible_move(playing).isEmpty || compteur_proteus(playing) == 1
	}

	def in_board(x : Int, y: Int) : Boolean = {
		return ((0 <= x) && (x < 8) && (0 <= y) && (y < 8))
	}

	def move_proteus(p:Dice, pos:Pos): Boolean = {
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

		p.move_to(pos)
		if(in_board(pos.x, pos.y-1) && board(pos.x)(pos.y-1) != null && board(pos.x)(pos.y-1).role == "queen") {
			board(pos.x)(pos.y-1) = null
		}

		playing = 1 - playing
		turn = turn + 1
		changed()


		if(players(playing) != null && !over) {
				players(playing).wait_play
			}
			return true
		}
		return false
	}

	def roll_proteus(p:Dice, up:Boolean) : Boolean = {
		if(up && p.i_role <= 4) {
			p.rotateUp
			return true
		}
		else if (!up && p.i_role >= 1) {
			p.rotateDown
			return true
		}
		return false
	}

}


























