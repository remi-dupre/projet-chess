import scala.io.Source
import java.io._

case class Move(p: Piece, pos: Pos)

case class Save(var move: Move, var saveList: List[Save])

object Backup {
	/* Algebrique notation : n. (Z)z9 (Z)z9 */
	def addMoveToSave(move: Move, save: Save):Unit = {
		if(save.saveList.isEmpty) {
			save.saveList = List(Save(move, List()))
		}
		else {
			addMoveToSave(move, save.saveList.head)
		}
	}
	def createSaveFromPGN(filename:String, game: Game) : List[Move] = {
		var bool = false
		var move_list : List[Move] = List()
		var n = 0
		var i = -1
		var j = -1
		var bloc = 0
		var x = -1
		var y = -1
		for (line <- Source.fromFile(filename).getLines) {
			if (line != "") {
				bool = true
			}
			if (bool) {
				for(c <- line) {
					if (c == ' ') { /* Avoir une meilleure facon de separer clairement les blocs */
						bloc = bloc + 1
					}

					if ((c - '0') >= 0 && (c - '0') <= 9 && bloc == 0) {
						n = n + (c-'0')*(Math.pow(10, i).toInt)
						i = i+1
					}

					if ((c -'a') >= 0 && (c - 'a') <= 7 && bloc == 1) {
						i = (c - 'a')
					}

					if ((c - '0') >= 1 && (c - '0') <= 8 && bloc == 1) {
						j = (c - 'a' - 1)
					}

					if ((c - 'a') >= 0 && (c - 'a') <= 7 && bloc == 2) {
						x = (c - 'a')
					}

					if ((c - '0') >= 1 && (c - '0') <= 8 && bloc == 2) {
						y = (c - 'a' - 1)
					}
					if (bloc == 3) {
						move_list = Move(game.board(i)(j), Pos(x, y)) :: move_list
						n = 0
						i = -1
						j = -1
						bloc = 0
						x = -1
						y = -1
					}
				}
			}
		}
		return move_list
	}

	def CreateAlgraebricFromMove(p : Piece, pos : Pos, n : Int, game: Game) : String = { /* y = {1,8}, x = {a,h} */
		val x = pos.x
		val y = pos.y
		val i = p.pos.x
		val j = p.pos.y
		var a = ""
		var b = ""
		val piece = game.board(i)(j)
		val listRole : List[String] = List("king","queen","rook","bishop","knight")
		for (role <- listRole) {
			if(p.role == role) {
				a = (role(0) - 32).toChar.toString
			}
			if(piece != null && piece.role == role) {
				b = (role(0) - 32).toChar.toString
			}
		} 
		var res = n.toString + ". " + a + ('a' + x).toChar + (y+1).toString + " " + b + ('a' + i).toChar + (j+1).toString
		return res
	}

	def CreatePGNfromSave(save: Save, nom: String) : Unit = {
		val writer = new PrintWriter(new File(nom + ".txt"))
		def list_moves(v: Save):List[Move] = {
			if(!v.saveList.isEmpty) {
				return (v.move :: list_moves(v.saveList.head))
			}
			else {
				return List(v.move)
			}
		}
		val listMoves = list_moves(save)
		var n = 0
		val game = new Game()
		for(move <- listMoves) {
			writer.write(CreateAlgraebricFromMove(move.p, move.pos, n, game ))
			game.move(move.p, move.pos)
			n = n +1
		}
		writer.close()
	}
}