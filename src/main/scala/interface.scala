import swing._

class GameWin(game : Game) extends MainFrame {
	title = "Chess"
	preferredSize = new Dimension(600, 600)
	val grid = Array.ofDim[Button](8, 8)
	contents = new GridPanel(8, 8) {
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				grid(i)(j) = new Button("" + (i, j))
				contents += grid(i)(j)
			}
		}
	}
}