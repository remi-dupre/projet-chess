import swing._

class SettingsWin extends Dialog {
	title = "Paramètres"

	val timer_duration = new TextField("2min 30s")
	val timer_moves = new TextField("5")
	val timer_count = new TextField("120")

	val ia_depth = new TextField("2")
	val ia_material = new CheckBox()	{ selected = true }
	val ia_positioning = new CheckBox()	{ selected = true }
	val ia_bishop = new CheckBox()		{ selected = true }
	val ia_nopawn = new CheckBox()		{ selected = true }
	val ia_rook_moves = new CheckBox()	{ selected = true }

	contents = new GridPanel(11, 1) {
		/* *************** Champs en rapport au timer *************** */
		contents += new Label("Paramètrage du temps")
		contents += new GridPanel(1, 2) {
			contents += new Label("Durée d'une cadence")
			contents += timer_duration
		}
		contents += new GridPanel(1, 2) {
			contents += new Label("Nombre de coups par cadences")
			contents += timer_moves
		}
		contents += new GridPanel(1, 2) {
			contents += new Label("Nombre de cadences")
			contents += timer_count
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
}
