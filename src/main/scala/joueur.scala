abstract class Player(val color : Int, game : Game) {
	def wait_play : Unit
}

class Human(color : Int, interface : GameWin, game : Game) extends Player(color, game) {
	def wait_play = {
		interface.state = SelectPiece(this)
	}

	def select(x : Int, y : Int) {
		if(game.cell_player(x, y) == color) {
			println("selected " + x + "," + y)
			interface.state = Wait()
		}
	}
}

