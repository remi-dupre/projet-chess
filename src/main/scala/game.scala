abstract class Cell
case class Some(t : Piece)
case class Void

class Game {
	var grid = Array.ofDim[Cell](8, 8)
}
