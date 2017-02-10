import swing._
import java.awt.Color
import javax.swing.ImageIcon

class InterfaceState
case class SelectPiece(p : Human) extends InterfaceState
case class WaitDirection(p : Human) extends InterfaceState
case class Wait() extends InterfaceState

class GameWin() extends MainFrame {
	val mainWin = this
	title = "Chess"
	minimumSize = new Dimension(500, 500)
	preferredSize = new Dimension(600, 600)

	val game = new Game()
	game.changed = () => {
		mainWin.refresh
	}
	var state : InterfaceState = new Wait()
	
	val grid = Array.ofDim[CellBtn](8, 8)
	contents = new GridPanel(8, 8) {
		for(j <- 0 to 7) {
			for(i <- 0 to 7) {
				grid(i)(j) = new CellBtn(i, j, game, mainWin)
				contents += grid(i)(j)
			}
		}
	}

	def highlight_possible(x : Int, y : Int) = {
		val p = game.getPiece(x, y)
		for(pos <- p.possible_move()) {
			grid(pos.x)(pos.y).background = Color.green
		}
	}

	def refresh = {
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				grid(i)(j).refresh
			}
		}
	}
}

class CellBtn(x : Int, y : Int, game : Game, mainWin : GameWin) extends Button {
	refresh

	action = Action("") {
		mainWin.state match {
			case Wait() => println("waiting ...")
			case SelectPiece(p) => if(p.select(x, y))
				mainWin.highlight_possible(x, y)
			case WaitDirection(p) => if(p.move(x, y))
				game.changed()
		}
	}

	def refresh = {
		background = if((x+y) % 2 == 0) Color.white else Color.darkGray
		val player = if(game.cell_player(x, y) == 0) "white" else "black"
		game.cell_role(x, y) match {
			case "empty" => icon = null 
			case role => icon = new ImageIcon("src/ressources/pieces/" + player + "/" + role + ".png")
		}
	}
}

