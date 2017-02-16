import swing._
import java.awt.Color
import javax.swing.ImageIcon
import scala.swing.event.Key
import scala.swing.event.KeyPressed


/**
 * Défini un état de l'interface :
 *  - SelectPiece(p) : attend que le joueur p dise quelle pièce il veut déplacer
 *  - WaitDirection(p) : attend que le joueur dise où il veut poser sa pièce
 *  - Wait() : on sait pas trop ce qu'il se passe
 */
class InterfaceState
case class SelectPiece(p : Human) extends InterfaceState
case class WaitDirection(p : Human) extends InterfaceState
case class Wait() extends InterfaceState


/**
 * Une fenêtre qui sert d'affichage de la partie et d'interface pour les éventuels joueurs
 * (au passage elle génère une partie dans sont constructeur)
 */
class GameWin() extends MainFrame {
	/* Caractéristiques de la fenêtre */
	val mainWin = this
	title = "Chess"
	minimumSize = new Dimension(500, 500)
	preferredSize = new Dimension(600, 600)

	/** La partie associée */
	val game = new Game()
	game.changed = () => {
		mainWin.refresh
	}
	/** État de la partie */
	var state : InterfaceState = new Wait()
	
	/** L'ensemble des cases de la partie */
	val grid = Array.ofDim[CellBtn](8, 8)
	contents = new GridPanel(8, 8) {
		for(j <- 0 to 7) {
			for(i <- 0 to 7) {
				grid(i)(j) = new CellBtn(i, j, game, mainWin)
				contents += grid(i)(j)
			}
		}
	}


	/** Met en valeur les cases sur lesquelles la pièce peut être déplacée */
	def highlight_possible(p : Piece) = {
		for(pos <- p.removeInCheckMoves(p.possible_move())) {
			grid(pos.x)(pos.y).highlight
		}
	}

	/** Réaffiche les pièces là où elles sont et vire tous les effets graphiques */
	def refresh = {
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				grid(i)(j).refresh
			}
		}
	}
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
			case SelectPiece(p) =>
				if(p.select(x, y)) {
					var piece = game.getPiece(x, y)
					if(p.removeInCheckMoves(piece.possible_move().isEmpty))
						mainWin.state = SelectPiece(p)
					else
						mainWin.highlight_possible(game.getPiece(x, y))
				}
			case WaitDirection(p) =>
				if(p.move(x, y))
					game.changed()
		}
	}
	
	def highlight = {
		val light_ok = new Color(120, 220, 130)
		val dark_ok = new Color(75, 150, 85)
		background = if((x+y) % 2 == 0) light_ok else dark_ok
	}

	/** Met à jours l'affichage de la case */
	def refresh = {
		background = if((x+y) % 2 == 0) Color.white else Color.darkGray
		val player = if(game.cell_player(x, y) == 0) "white" else "black"
		game.cell_role(x, y) match {
			case "empty" => icon = null 
			case role => icon = new ImageIcon("src/ressources/pieces/" + player + "/" + role + ".png")
		}
	}
}

