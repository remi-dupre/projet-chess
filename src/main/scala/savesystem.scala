import scala.io.Source
import java.io._

case class Move(from: Pos, to: Pos) {
	var promote_to : String = null
}

case class Save(var move: Move, var saveList: List[Save]) {
	var game_state : Game = null
}

class FakePlayer(color : Int, game : Game, promotion_type : String) extends Player(color, game) {
	override def wait_play = {}
	override def get_promotion_type : String = {
		return promotion_type
	}
}

object Backup {
	/* Algebrique notation : n. (Z)z9 (Z)z9 */
	def createGameListFromSave(game:Game, save:Save) : List[Game] = {
		if(save.game_state == null) {
			val game_c = game.copy
			game_c.players(game.playing) = new FakePlayer(game.playing, game_c, save.move.promote_to)
			game_c.move(
				game_c.board(save.move.from.x)(save.move.from.y),
				save.move.to
			)
			save.game_state = game_c
		}
		if( save.saveList.isEmpty )
			return List(save.game_state)
		else
			return save.game_state :: createGameListFromSave(save.game_state, save.saveList.head)
	}

	def cinquanteCoup(game:Game, save:Save) = {}

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
						move_list = Move(game.board(i)(j).pos, Pos(x, y) ) :: move_list
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
			if(game.board(x)(y) != null && game.board(x)(y).role == role) {
				b = (role(0) - 32).toChar.toString
			}
		} 
		var res = n.toString + ". " + a + ('a' + x).toChar + (y+1).toString + " " + b + ('a' + i).toChar + (j+1).toString + " "
		return res
	}

	def CreatePGNfromSave(save: Save, nom: String) : Unit = {
		val writer = new PrintWriter(new File(nom))

		def list_moves(v: Save):List[Move] = {
			if(!v.saveList.isEmpty) {
				return (v.move :: list_moves(v.saveList.head))
			}
			else {
				return List(v.move)
			}
		}

		var n = 0
		val game = new Game()
		for(move <- list_moves(save)) {
			var piece = game.board(move.from.x)(move.from.y)
			writer.write(CreateAlgraebricFromMove(piece, move.to, n, game))
			game.move(piece, move.to)
			n = n +1
		}
		writer.write("\n")
		writer.close()
	}
}

