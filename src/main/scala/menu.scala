import swing._

class MenuWin extends MainFrame {
	title = "test"
	contents = new BoxPanel(Orientation.Vertical) {
		contents += new Button(Action("Joueur vs. Joueur") {
			visible = false
			val fen = new GameWin()
			fen.game.players(0) = new Human(0, fen, fen.game)
			fen.game.players(1) = new Human(1, fen, fen.game)
			fen.game.start
			fen.visible = true
		})
		contents += new Button(Action("IA vs. Joueur") {
			visible = false
			val fen = new GameWin()
			fen.game.players(0) = new Human(0, fen, fen.game)
			fen.game.players(1) = new IA(1, fen.game)
			fen.game.start
			fen.visible = true
		})
		contents += new Button(Action("IA vs. IA") {
			visible = false
			val fen = new GameWin()
			fen.game.players(0) = new IA(0, fen.game)
			fen.game.players(1) = new IA(1, fen.game)
			fen.game.start
			fen.visible = true
		})
	}	
}

