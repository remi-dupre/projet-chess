import swing._
import java.awt.Color
import javax.swing.ImageIcon
import scala.swing.event.Key
import scala.swing.event.KeyPressed
import scala.math._

/**
 * Défini un état de l'interface :
 *  - SelectPiece(p) : attend que le joueur p dise quelle pièce il veut déplacer
 *  - WaitDirection(p) : attend que le joueur dise où il veut poser sa pièce
 *  - Wait() : on sait pas trop ce qu'il se passe
 */
class InterfaceState
case class SelectPiece(p : Human, exclude : List[Pos] = List()) extends InterfaceState
case class WaitDirection(p : Human) extends InterfaceState
case class WaitRollDirection(p : Human) extends InterfaceState
case class Wait() extends InterfaceState


/**
 * Une fenêtre qui sert d'affichage de la partie et d'interface pour les éventuels joueurs
 * (au passage elle génère une partie dans sont constructeur)
 */
class GameWin(game_type : GameType.Value = GameType.Normal) extends MainFrame {
	preferredSize = new Dimension(600, 630)
	minimumSize = new Dimension(500, 530)
	/* Caractéristiques de la fenêtre */
	val mainWin = this
	title = "Chess"

	/** La partie associée */
	val game = game_type match {
		case GameType.Normal => new Game()
		case GameType.Proteus => new ProtGame()
	}

	game.changed = () => {
		mainWin.refresh
	}

	override def closeOperation() {
		Thread.sleep(100)
		System.exit(0)
	}

	/** État de la partie */
	var state : InterfaceState = new Wait()

	val msg = new Label("test")
	val promo_btn = new PromoBtn()

	val back_btn = new Button(Action("Retour") {
		if(game.save_hist == null) {
			game.save_hist = game.save_current
			game.save_root.game_list()
		}
		game.copy_config_of(game.save_hist.father.game_state)
		if(game.save_hist.father != null )
			game.save_hist = game.save_hist.father
		refresh
	})
	back_btn.visible = false

	val leave_btn = new Button(Action("Menu") {
		close()
		Main.menu.visible = true
	})
	val roll_btn = new RollBtn(this)
	var label_timer = new TimeCounter(game)

	/** L'ensemble des cases de la partie */
	val grid = Array.ofDim[CellBtn](8, 8)
	contents = new BoxPanel(Orientation.Vertical) {
		// Main grid
		preferredSize = new Dimension(600, 700)
		contents += new GridPanel(8, 8) {
			minimumSize = new Dimension(500, 500)
			for(j <- 0 to 7) {
				for(i <- 0 to 7) {
					grid(i)(j) = new CellBtn(i, j, game, mainWin)
					contents += grid(i)(j)
				}
			}
		}

		// Bottom bar
		contents += new GridPanel(1,5) {
			preferredSize = new Dimension(1000, 60);
			contents += leave_btn
			contents += back_btn

			contents += promo_btn
			promo_btn.icon = Tools.icon_resized("src/ressources/pieces/white/" + promo_btn.roles(promo_btn.r_i) + ".png", 30, 30)

			contents += roll_btn

			contents += msg
			contents += label_timer
		}
	}


	/** Met en valeur les cases sur lesquelles la pièce peut être déplacée */
	def highlight_possible(p : Piece) = {
		for(pos <- p.removeInCheckMoves(p.possible_move())) {
			grid(pos.x)(pos.y).highlight
		}
	}

	/** Réaffiche les pièces là où elles sont et vire tous les effets graphiques */
	def refresh : Unit = {
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				grid(i)(j).refresh
			}
		}
		mainWin.msg.text = "Au " + (if(game.playing == 0) "blanc" else "noir")  + " de jouer "
		if(game.inCheck(game.playing)) {
			mainWin.msg.text += "(Échec) "
		}
		if(game.over) {
			roll_btn.visible = false
			promo_btn.visible = false
			back_btn.visible = true
		}
		if(game.pat) {
			mainWin.msg.text = "PAT "
			if(game.triple_repetition) {
				mainWin.msg.text += "(triple répétition) "
			}
			if(game.impossibilityOfCheckMate) {
				mainWin.msg.text += "(Impossibilité de Mater) "
			}
		}
		else if(game.over) {
			mainWin.msg.text = "Le " + (if(game.winner == 0) "blanc" else "noir")  + " a gagné "
		}
	}

	centerOnScreen()
	refresh
}


/**
 * Représente une case cliquable de l'interface
 */
class CellBtn(x : Int, y : Int, game : Game, mainWin : GameWin) extends Button {
	refresh

	listenTo(keys)
	reactions += {
		case KeyPressed(_, Key.Escape, _, _) => {
			mainWin.state match {
				case WaitDirection(p) =>
					mainWin.state = SelectPiece(p)
					refresh
				case _ => ()
			}
			mainWin.refresh
		}
	}

	action = Action("") {
		mainWin.state match {
			case Wait() => println("waiting ...")
			case WaitRollDirection(p) =>
				p.select(x, y)
			case WaitDirection(p) =>
				if(p.move(x, y)) {
					game.changed()
				}
				else if (p.select(x, y)) {
					var piece = game.getPiece(x, y)
					if(!piece.removeInCheckMoves(piece.possible_move()).isEmpty) {
						mainWin.refresh
						mainWin.highlight_possible(game.getPiece(x, y))
					}
				}
			case SelectPiece(p, excl) =>
				if(!excl.contains(Pos(x, y)) && p.select(x, y)) {
					var piece = game.getPiece(x, y)
					if(p.for_move)
						mainWin.highlight_possible(game.getPiece(x, y))
				}
		}
	}
	
	tooltip = "x: " + x + ", y: " + y

	def highlight = {
		val light_ok = new Color(120, 220, 130)
		val dark_ok = new Color(75, 150, 85)
		background = if((x+y) % 2 == 0) light_ok else dark_ok
	}

	/** Met à jours l'affichage de la case */
	def refresh = {
		def piece_icon(piece : Piece) : String = {
			if(piece == null) return ""
			val player = if(piece.player == 0) "white" else "black"
			if(piece.role == "dice") {
				val role = Dice.seq_roles(piece.asInstanceOf[Dice].i_role)
				return "src/ressources/pieces/" + player + "/" + role + ".png"
			}
			else {
				return "src/ressources/pieces/" + player + "/" + piece.role + ".png"
			}
		}

		background = if((x+y) % 2 == 0) Color.white else Color.darkGray
		icon = new ImageIcon(piece_icon(game.board(x)(y))) ; disabledIcon = icon
		enabled = !game.over
	}
}

/**
 * Sélectionne la pièce vers laquelle promouvoir
 */
class PromoBtn() extends Button {
	val promo_btn = this
	val roles = Array("queen", "knight", "bishop", "rook")
	var r_i = 0
	preferredSize = new Dimension(300, 500)
	
	action = Action("promotion") {
		promo_btn.r_i = (promo_btn.r_i+1) % 4
		icon = Tools.icon_resized("src/ressources/pieces/white/" + promo_btn.roles(promo_btn.r_i) + ".png", 30, 30)
	}

	def role = {
		roles(r_i)
	}
}

/** Un choix de pièce vers laquelle roll */
class RollBtn(a_interface : GameWin) extends GridPanel(1, 2) {
	val interface = a_interface
	val btn_group = this
	val btn_up = new Button(Action(""){ btn_group.selected(true) })
	val btn_down = new Button(Action(""){ btn_group.selected(false) })
	contents += btn_down
	contents += btn_up
	visible = false

	def select(i_role : Int) = {
		visible = true

		var role_down = Dice.seq_roles(max(0, i_role-1))
		btn_down.enabled = (i_role > 0)
		btn_down.icon = Tools.icon_resized("src/ressources/pieces/white/" + role_down + ".png", 30, 30)

		var role_up = Dice.seq_roles(min(5, i_role+1))
		btn_up.enabled = (i_role < 5)
		btn_up.icon = Tools.icon_resized("src/ressources/pieces/white/" + role_up + ".png", 30, 30)
	}

	def selected(up : Boolean) = {
		visible = false
		interface.state match {
			case WaitRollDirection(p) => p.get_roll_direction(up)
			case _ => println("wtf : " + interface.state)
		}
	}
}
