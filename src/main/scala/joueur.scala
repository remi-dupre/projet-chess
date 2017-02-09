abstract class Player(val color : Int, game : Game) {
	def wait_play : Unit
}

class Human(color : Int, interface : GameWin, game : Game) extends Player(color, game) {
	def wait_play = {
		interface.state = SelectPiece(this)
	}

	var selected = (-1, -1)
	def select(x : Int, y : Int) {
		if(game.cell_player(x, y) == color) {
			selected = (x, y)
			println("selected " + x + "," + y)
			interface.state = WaitDirection(this)
		}
	}

	def move(x : Int, y : Int) {
		var (i, j) = selected
		val piece = game.get_piece(i, j)
		game.move(piece, x, y)
	}
}

