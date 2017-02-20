import swing._

class MenuWin extends MainFrame {
	title = "test"
	val menu = this

	contents = new GridPanel(5, 1) {
		contents += new Button(Action("Joueur vs. Joueur") {
			menu.visible = false
			val fen = new GameWin()
			fen.game.players(0) = new Human(0, fen, fen.game)
			fen.game.players(1) = new Human(1, fen, fen.game)
			fen.game.start
			fen.visible = true
		})
		contents += new Button(Action("IA vs. Joueur blanc") {
			menu.visible = false
			val fen = new GameWin()
			fen.game.players(0) = new Human(0, fen, fen.game)
			fen.game.players(1) = new IA(1, fen.game, 0)
			fen.game.start
			fen.visible = true
		})
		contents += new Button(Action("IA vs. Joueur noir") {
			menu.visible = false
			val fen = new GameWin()
			fen.game.players(1) = new Human(1, fen, fen.game)
			fen.game.players(0) = new IA(0, fen.game, 0)
			fen.game.start
			fen.visible = true
		})
		contents += new Button(Action("IA vs. IA (500ms / tour)") {
			menu.visible = false
			val fen = new GameWin()
			fen.game.players(0) = new IA(0, fen.game, 500)
			fen.game.players(1) = new IA(1, fen.game, 500)
			fen.game.start
			fen.visible = true
		})
		contents += new Button(Action("IA vs. IA (20ms / tour)") {
			menu.visible = false
			val fen = new GameWin()
			fen.game.players(0) = new IA(0, fen.game, 20)
			fen.game.players(1) = new IA(1, fen.game, 20)
			fen.game.start
			fen.visible = true
		})  
	}

	centerOnScreen()
}

