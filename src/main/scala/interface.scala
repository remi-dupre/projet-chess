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

	def refresh = {
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				grid(i)(j).refresh
			}
		}
	}
}

class CellBtn(x : Int, y : Int, game : Game, mainWin : GameWin) extends Button {
	background = if((x+y) % 2 == 0) Color.white else Color.darkGray
	refresh

	action = Action("") {
		mainWin.state match {
			case Wait() => println("waiting ...")
			case SelectPiece(p) => p.select(x, y)
			case WaitDirection(p) => {p.move(x, y) ; game.changed()}
		}
	}

	def refresh = {
		val player = if(game.cell_player(x, y) == 0) "white" else "black"
		game.cell_role(x, y) match {
			case "empty" => icon = null 
			case role => icon = new ImageIcon("src/ressources/pieces/" + player + "/" + role + ".png")
		}
	}
}

