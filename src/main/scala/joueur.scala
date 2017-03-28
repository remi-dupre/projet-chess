import scala.util.Random
import java.util.Timer
import java.util.TimerTask

abstract class Player(val color : Int, game : Game) {
	/** Spécifie que la partie attend une action du joueur */
	def wait_play : Unit = {}
	def wait_roll(last_pos : Pos) : Unit = {}
	def get_roll_direction(up : Boolean) : Unit = {}
	def get_promotion_type : String
}

/**
 * Représente un joueur humain qui interagit par la fenêtre graphique
 *  @param color L'id du joueur (0 pour blanc ou 1 pour noir)
 *  @param interface La fenêtre servant à interragir avec le joueur
 *  @param game La partie
 */
class Human(color : Int, interface : GameWin, game : Game, save : Boolean = false) extends Player(color, game) {
	var selected = (-1, -1) /** La pièce sélectionnée */
	var points = 0 /* nombre de points dans la partie pour Proteus */

	var for_move = true

	override def wait_play = {
		for_move = true
		interface.state = SelectPiece(this)
	}

	override def wait_roll(last_pos : Pos) = {
		for_move = false
		interface.state = SelectPiece(this, List(last_pos))
	}

	override def get_roll_direction(direction : Boolean) = {
		val (x, y) = selected
		game.asInstanceOf[ProtGame].roll(Pos(x, y), direction)
	}

	override def get_promotion_type : String = {
		return interface.promo_btn.role
	}

	/** Sélectonne un pièce si possible
	 * Retourne false si ca échoue */
	def select(x : Int, y : Int) : Boolean = {
		if(for_move) {
			if(game.cell_player(x, y) == color) {
				selected = (x, y)
				interface.state = WaitDirection(this)
				return true
			}
			else return false
		}
		else {
			if(game.cell_player(x, y) == color) {
				selected = (x, y)
				interface.roll_btn.select(game.board(x)(y).asInstanceOf[Dice].i_role)
				interface.state = WaitRollDirection(this)
				return true
			}
		}
		return false
	}
	
	/** Effectue le dépacement de la pièce sélectionnée si possible
	 * Retourne false si ca échoue */
	def move(x : Int, y : Int) : Boolean = {
		var (i, j) = selected
		val piece = game.getPiece(i, j)
		val ret = game.move(piece, Pos(x, y))
		if(game.save_root != null && save)
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

	override def wait_roll(last_pos : Pos) = {
		var pos_move : List[(Piece, Pos)] = List()
		var piece_list : List[Piece] = List()
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				val piece = game.board(i)(j)
				if(piece != null && piece.player == color && piece.pos != last_pos) {
					piece_list = piece :: piece_list
					for(pos <- piece.removeInCheckMoves(piece.possible_move)) {
						pos_move = (piece, pos) :: pos_move
					}
				}
			}
		}

		val piece = Random.shuffle(piece_list).head
		var direction = Random.shuffle(List(true, false)).head
		if(piece.role == "pyramid") {
			direction = true
		}
		if(piece.role == "queen") {
			direction = false
		}

		Thread.sleep(speed)
		val t = new Thread(new Runnable() {
			def run() {
				game.asInstanceOf[ProtGame].roll(piece.pos, direction)
			}
		});
		t.start
	}
}
