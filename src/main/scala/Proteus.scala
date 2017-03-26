import scala.math.min
import scala.math.max

class Dice(game: Game, player: Int, m_pos:Pos) extends Piece(game, player, m_pos) {
	val seq_roles = Array("pyramid", "pawn", "bishop", "knight", "rook", "queen")
	var i_role : Int = 1

	def RotateUp = {
		i_role = min(5, i_role + 1)
	}
	def RotateDown = {
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



























