import swing._
import java.awt.Color

class GameWin(game : Game) extends MainFrame {
	title = "Chess"
	preferredSize = new Dimension(600, 600)

	val grid = Array.ofDim[Button](8, 8)
	contents = new GridPanel(8, 8) {
		for(j <- 0 to 7) {
			for(i <- 0 to 7) {
				grid(i)(j) = new CellBtn(i, j)
				contents += grid(i)(j)
			}
		}
	}

	def refresh = {
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				if(!game.empty_cell(i, j))
					grid(i)(j).text = "X"
				else
					grid(i)(j).text = ""
			}
		}
	}

	refresh
}

class CellBtn(x : Int, y : Int) extends Button {
	background = if((x+y) % 2 == 0) Color.white else Color.black
}

