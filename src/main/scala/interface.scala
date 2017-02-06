import swing._
import java.awt.Color
import javax.swing.ImageIcon

class GameWin(game : Game) extends MainFrame {
	title = "Chess"
	preferredSize = new Dimension(600, 600)

	val grid = Array.ofDim[Button](8, 8)
	contents = new GridPanel(8, 8) {
		for(j <- 0 to 7) {
			for(i <- 0 to 7) {
				grid(i)(j) = new CellBtn(i, j, game)
				contents += grid(i)(j)
			}
		}
	}

	def refresh = {
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				
			}
		}
	}

	refresh
}

class CellBtn(x : Int, y : Int, game : Game) extends Button {
	background = if((x+y) % 2 == 0) Color.white else Color.darkGray
	refresh

	def refresh = {
		val player = if(game.cell_player(x, y) == 0) "white" else "black"
		game.cell_role(x, y) match {
			case "empty" => icon = null 
			case role => icon = new ImageIcon("src/ressources/pieces/" + player + "/" + role + ".png")
		}
	}
}

