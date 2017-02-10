abstract class Player(val color : Int, game : Game) {
	def wait_play : Unit
}

class Human(color : Int, interface : GameWin, game : Game) extends Player(color, game) {
	def wait_play = {
		interface.state = SelectPiece(this)
	}

	var selected = (-1, -1)
	def select(x : Int, y : Int) : Boolean = {
		if(game.cell_player(x, y) == color) {
			selected = (x, y)
			println("selected " + x + "," + y)
			interface.state = WaitDirection(this)
			return true
		}
		else return false
	}

	def move(x : Int, y : Int) : Boolean = {
		var (i, j) = selected
		val piece = game.getPiece(i, j)
		return game.move(piece, Pos(x, y))
	}

}

