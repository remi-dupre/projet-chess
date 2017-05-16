import swing._

class SettingsWin extends Dialog {
	title = "Paramètres"

	val timer_duration_input = new TextField("2min30s")
	val timer_moves_input = new TextField("5")
	val timer_count_input = new TextField("120")

	val ia_depth = new TextField("3")
	val ia_material = new CheckBox()	{ selected = true }
	val ia_positioning = new CheckBox()	{ selected = true }
	val ia_bishop = new CheckBox()		{ selected = true }
	val ia_nopawn = new CheckBox()		{ selected = true }
	val ia_rook_moves = new CheckBox()	{ selected = false }

	contents = new GridPanel(11, 1) {
		/* *************** Champs en rapport au timer *************** */
		contents += new Label("Paramètrage du temps")
		contents += new GridPanel(1, 2) {
			contents += new Label("Durée d'une cadence")
			contents += timer_duration_input
		}
		contents += new GridPanel(1, 2) {
			contents += new Label("Nombre de coups par cadences")
			contents += timer_moves_input
		}
		contents += new GridPanel(1, 2) {
			contents += new Label("Nombre de cadences")
			contents += timer_count_input
		}

		/* *************** Champs à l'IA *************** */
		contents += new Label("Paramètrage de l'IA")
		contents += new GridPanel(1, 2) {
			contents += new Label("Niveau de l'IA (1, 2 ou 3)")
			contents += ia_depth
		}
		contents += new GridPanel(1, 2) {
			contents += new Label("Matériel sur l'échiquier")
			contents += ia_material
		}
		contents += new GridPanel(1, 2) {
			contents += new Label("Positionnement des pièces")
			contents += ia_positioning
		}
		contents += new GridPanel(1, 2) {
			contents += new Label("Bonus bishop pair")
			contents += ia_bishop
		}
		contents += new GridPanel(1, 2) {
			contents += new Label("Penalty no pawn")
			contents += ia_nopawn
		}
		contents += new GridPanel(1, 2) {
			contents += new Label("Rook possible moves")
			contents += ia_rook_moves
		}
	}

	centerOnScreen()

	/* *************** Définition des getters *************** */

	val hms = """(?:(\d*)h)?(?:(\d*)min)?(?:(\d+)s?)?""".r
	def timer_duration = timer_duration_input.text match {
		case hms(h, min, sec) => println(Tools.of_str(sec) + 60 * (Tools.of_str(min) + 60 * Tools.of_str(h))); Tools.of_str(sec) + 60 * (Tools.of_str(min) + 60 * Tools.of_str(h))
		case _ => 180 // Par défaut, 3 minutes
	}

	def timer_moves = timer_moves_input.text.toInt
	def timer_count = timer_count_input.text.toInt

	def get_timer : Cadency = {
		var l : List[Period] = List()
		for(i <- 1 to timer_count) {
			l = (Period(timer_duration, timer_moves)) :: l
		}
		return new Cadency(l, false)
	}
}
