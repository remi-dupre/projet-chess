import scala.util.Random
import java.util.Timer
import java.util.TimerTask

abstract class Player(val color : Int, game : Game) {
	/** Spécifie que la partie attend une action du joueur */
	def wait_play : Unit
	def get_promotion_type : String
}

/**
 * Représente un joueur humain qui interagit par la fenêtre graphique
 *  @param color L'id du joueur (0 pour blanc ou 1 pour noir)
 *  @param interface La fenêtre servant à interragir avec le joueur
 *  @param game La partie
 */
class Human(color : Int, interface : GameWin, game : Game) extends Player(color, game) {
	override def wait_play = {
		interface.state = SelectPiece(this)
	}

	override def get_promotion_type : String = {
		return interface.promo_btn.role
	}

	var selected = (-1, -1) /** La pièce sélectionnée */
	/** Sélectonne un pièce si possible
	 * Retourne false si ca échoue */
	def select(x : Int, y : Int) : Boolean = {
		if(game.cell_player(x, y) == color) {
			selected = (x, y)
			interface.state = WaitDirection(this)
			return true
		}
		else return false
	}
	
	/** Effectue le dépacement de la pièce sélectionnée si possible
	 * Retourne false si ca échoue */
	def move(x : Int, y : Int) : Boolean = {
		var (i, j) = selected
		val piece = game.getPiece(i, j)
		val ret = game.move(piece, Pos(x, y))
		if(game.save_root != null)
			Backup.CreatePGNfromSave(game.save_root, "save.pgn")
		return ret
	}
}

/**
 * Représente un IA
 * Par défault elle fait des movements aléatoires */
class IA(color : Int, game : Game, speed : Int = 0) extends Player(color, game) {
	override def wait_play = {
		var pos_move : List[(Piece, Pos)] = List()
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				val piece = game.board(i)(j)
				if(piece != null && piece.player == color) {
					for(pos <- piece.removeInCheckMoves(piece.possible_move)) {
						pos_move = (piece, pos) :: pos_move
					}
				}
			}
		}

		Thread.sleep(speed)
		val t = new Thread(new Runnable() {
		 	def run() {
				val (piece, dest) = Random.shuffle(pos_move).head
				game.move(piece, dest)
				Thread.currentThread().interrupt()
			}
		});
		t.start()
	}

	override def get_promotion_type : String =  {
		"queen"
	}
}

