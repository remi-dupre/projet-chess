import swing._

class MenuWin extends MainFrame {
	title = "test"
	contents = new Button(Action("Start") {
		println("Starting the game")
		visible = false

		val fen = new GameWin(new Game)
		fen.visible = true
	})
}

