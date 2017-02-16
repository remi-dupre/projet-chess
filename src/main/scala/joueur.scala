import scala.util.Random
import java.util.Timer
import java.util.TimerTask

abstract class Player(val color : Int, game : Game) {
	/** Spécifie que la partie attend une action du joueur */
	def wait_play : Unit
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

	var selected = (-1, -1) /** La pièce sélectionnée */
	/** Sélectonne un pièce si possible
	 * Retourne false si ca échoue */
	def select(x : Int, y : Int) : Boolean = {
		if(game.cell_player(x, y) == color) {
			selected = (x, y)
			println("selected " + x + "," + y)
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
		return game.move(piece, Pos(x, y))
	}
}

/**
 * Représente un IA
 * Par défault elle fait des movements aléatoires */
class IA(color : Int, game : Game) extends Player(color, game) {
	override def wait_play = {
		Thread.sleep(1000)
		var pos_move : List[(Piece, Pos)] = List()
		for(c <- game.pieces) c match {
			case piece if (piece.player == color) => 
				for(pos <- piece.removeInCheckMoves(piece.possible_move)) {
					pos_move = (piece, pos) :: pos_move
				}
			case _ => ()
		}
	

		val t = new Timer()
		t.schedule(new TimerTask {
			override def run(): Unit = {
				val (piece, dest) = Random.shuffle(pos_move).head
				println((piece, dest))
				game.move(piece, dest)
			}
  		}, 1)
	}	
}

