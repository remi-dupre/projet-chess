import swing._

def create_game(joueur1 : String, joueur2 : String,
  get_save : Boolean = false,
  ia0_delay : Int = 0, ia1_delay : Int = 0,
  mode : GameType.Value = GameType.Normal) = {
	menu.visible = false
	val fen = new GameWin(mode)
	fen.game.players(0) = joueur1 match {
		case "human" => new Human(0, fen, fen.game, true)
		case "ia"    => new IA(0, fen.game, ia0_delay)
	}
	fen.game.players(1) = joueur2 match {
		case "human" => new Human(1, fen, fen.game, true)
		case "ia"    => new IA(1, fen.game, ia1_delay)
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

class MenuWin extends MainFrame {
	title = "test"
	val menu = this

	contents = new GridPanel(5, 1) {
		val use_save = new CheckBox("Récupérer la sauvegarde")

		// Boutons de selection des joueurs
		val select_white = new SelectPlayer()
		val select_black = new SelectPlayer()
		contents += new GridPanel(1, 2) {
			contents += select_white
			contents += select_black
		}

		contents += new Button(Action("Partie Stardart") {
			var ia0_delay = 0
			var ia1_delay = 0

			var j0_type : String = select_white.i match {
				case 0 => "human"
				case _ => "ia"
			}
			if(select_white.i == 2)
				ia0_delay = 500

			var j1_type : String = select_black.i match {
				case 0 => "human"
				case _ => "ia"
			}
			if(select_black.i == 2)
				ia1_delay = 500

			create_game(j0_type, j1_type, use_save.selected, ia0_delay, ia1_delay)
		})

		contents += new Button(Action("Proteus") {
			var ia0_delay = 0
			var ia1_delay = 0

			var j0_type : String = select_white.i match {
				case 0 => "human"
				case _ => "ia"
			}
			if(select_white.i == 2)
				ia0_delay = 500

			var j1_type : String = select_black.i match {
				case 0 => "human"
				case _ => "ia"
			}
			if(select_black.i == 2)
				ia1_delay = 500

			create_game(j0_type, j1_type, use_save.selected, ia0_delay, ia1_delay, GameType.Proteus)
		})
		contents += new GridPanel(1, 2) {
			new SelectPlayer()
		}
		contents += use_save
	}
	centerOnScreen()
}

class SelectPlayer() extends Button {
	var i = 0
	val types = Array("human", "ia_fast", "ia_slow")
	action = Action("") {
		i = (i + 1) % 3
		refresh
	}
	def refresh = {
		action.title = i match {
			case 0 => "Joueur"
			case 1 => "IA (rapide)"
			case 2 => "IA (lent)"
		}
	}
	refresh
}
