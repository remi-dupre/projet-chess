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
    preferredSize = new Dimension(600, 630)
    minimumSize = new Dimension(500, 530)
	/* Caractéristiques de la fenêtre */
	val mainWin = this
	title = "Chess"

	/** La partie associée */
	val game = new Game()
	game.changed = () => {
		mainWin.refresh
	}

	override def closeOperation() {
		// game.pieces = List()
		Thread.sleep(100)
		System.exit(0)
	}

	/** État de la partie */
	var state : InterfaceState = new Wait()

    val msg = new Label("test")
    val promo_btn = new PromoBtn()
    val leave_btn = new Button(Action("Leave") {
        close()
        Main.menu.visible = true
    })

	/** L'ensemble des cases de la partie */
	val grid = Array.ofDim[CellBtn](8, 8)
	contents = new BoxPanel(Orientation.Vertical) {
        // main grid
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

        // bottom bar
        contents += new GridPanel(1,3) {
            preferredSize = new Dimension(1000, 60);
            contents += leave_btn
            
            contents += promo_btn
	        promo_btn.icon = tools.icon_resized("src/ressources/pieces/white/" + promo_btn.roles(promo_btn.r_i) + ".png", 30, 30)

            //contents += new Separator(Orientation.Horizontal) {}
            contents += msg
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
        mainWin.msg.text = "Au " + (if(game.playing == 0) "blanc" else "noir")  + " de jouer "
        if(game.inCheck(game.playing)) {
            mainWin.msg.text += "(Échec) "
        }
        if(game.pat) {
            mainWin.msg.text = "PAT "
        }
        else if(game.over) {
            mainWin.msg.text = "Le " + (if(game.playing == 1) "blanc" else "noir")  + " a gagné "
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
			case SelectPiece(p) =>
				if(p.select(x, y)) {
					var piece = game.getPiece(x, y)
					if(piece.removeInCheckMoves(piece.possible_move()).isEmpty)
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
		val role = game.cell_role(x, y)
        if(role == "empty") {
            icon = null
        }
        else {
			icon = new ImageIcon("src/ressources/pieces/" + player + "/" + role + ".png") ; disabledIcon = icon
		}
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
	    icon = tools.icon_resized("src/ressources/pieces/white/" + promo_btn.roles(promo_btn.r_i) + ".png", 30, 30)
    }

    def role = {
        roles(r_i)
    }
}

