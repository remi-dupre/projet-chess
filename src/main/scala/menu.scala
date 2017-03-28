import swing._

class MenuWin extends MainFrame {
	title = "test"
	val menu = this

	def create_game(joueur1 : String, joueur2 : String,
	  get_save : Boolean = false,
	  ia_delay : Int = 0) = {
		menu.visible = false
		val fen = new GameWin()
		fen.game.players(0) = joueur1 match {
			case "human" => new Human(0, fen, fen.game)
			case "ia"    => new IA(0, fen.game, ia_delay)
		}
		fen.game.players(1) = joueur2 match {
			case "human" => new Human(1, fen, fen.game)
			case "ia"    => new IA(1, fen.game, ia_delay)
		}

		fen.visible = true

		if(get_save) {
			val save = Backup.createSaveFromPGN("save.pgn")
			fen.game.init
			save.apply_moves(fen.game)
			fen.game.players(fen.game.playing).wait_play
		}
		else {
			fen.game.start
		}
	}

	contents = new GridPanel(7, 1) {
		val use_save = new CheckBox("Récupérer la sauvegarde")

		contents += new Button(Action("Joueur vs. Joueur") {
			create_game("human", "human", use_save.selected)
		})
		contents += new Button(Action("IA vs. Joueur blanc") {
			create_game("human", "ia", use_save.selected)
		})
		contents += new Button(Action("IA vs. Joueur noir") {
			create_game("ia", "human", use_save.selected)
		})
		contents += new Button(Action("IA vs. IA (500ms / tour)") {
			create_game("ia", "ia", use_save.selected, 500)
		})
		contents += new Button(Action("IA vs. IA (rapide)") {
			create_game("ia", "ia", use_save.selected)
		})
		contents += new Button(Action("Proteus") {
			menu.visible = false
			val fen = new GameWin(GameType.Proteus)
			fen.game.players(0) = new Human(0, fen, fen.game)
			fen.game.players(1) = new Human(1, fen, fen.game)
			fen.visible = true
		})
		contents += use_save
	}

	centerOnScreen()
}