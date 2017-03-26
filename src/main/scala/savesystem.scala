import scala.io.Source
import java.io._

case class Move(from: Pos, to: Pos) {
	var promote_to : String = null
}

case class Save(var move: Move, var saveList: List[Save])

class FakePlayer(color : Int, game : Game, promotion_type : String) extends Player(color, game) {
	override def wait_play = {}

	override def get_promotion_type : String = {
		return promotion_type
	}
}

object Backup {
	/* Algebrique notation : n. (Z)z9 (Z)z9 */
	def createGameListFromSave(game:Game, save:Save) : List[Game] = {
		val game_c = game.copy
		game_c.players(game.playing) = new FakePlayer(game.playing, game_c, save.move.promote_to)
		game_c.move(
			game_c.board(save.move.from.x)(save.move.from.y),
			save.move.to
		)
		if( save.saveList.isEmpty )
			return List(game_c)
		else
			return game_c::createGameListFromSave(game_c, save.saveList.head)
	}

	def compteur(game:Game) : Int = {
		var n = 0
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				if (game.board(i)(j) != null) {
					n = n+1
				}
			}
		}
		return n
	}

	def isIdentique(game1:Game, game2:Game): Boolean = {
/*		var res = game1.board == game2.board
		res = res && game1.every_possible_move_nocheck(0) == game2.every_possible_move_nocheck(0)
		res = res && game1.every_possible_move_nocheck(1) == game2.every_possible_move_nocheck(1)*/
		return game1.is_copy_of(game2)
	}

	def hasPawnMoved(game1:Game, game2:Game): Boolean = {
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				if(game1.board(i)(j) != null && game1.board(i)(j).role == "pawn") {
					if(game2.board(i)(j) == null || game2.board(i)(j).role != "pawn") {
						return true
					}
				}
			}
		}
		return false
	}

	def tripleRepetition(game:Game, save:Save): Boolean = {
		val n = compteur(game)
		var i = 0
		val listGame = createGameListFromSave(new Game(), save).reverse
		def count_repet(game:Game, listGame:List[Game]) : Boolean = {
			if(listGame.isEmpty) {
				return false
			}
			else {
				val hd_game = listGame.head
				if(n != compteur(hd_game))
					return false
				if(isIdentique(game, hd_game)) {
					i += 1
					if(i == 3)
						return true
				}
				return count_repet(game, listGame.tail)
			}
		}
		return count_repet(game, listGame)
	}

	def cinquanteCoup(game:Game, save:Save): Boolean = {
		val n = compteur(game)
		val listGame = createGameListFromSave(new Game(), save).reverse
		def count_repet(game:Game, listGame:List[Game],k) : Boolean = {
			if(k == 50) {
				return true
			}
			if(listGame.isEmpty) {
				return false
			}
			else {
				val game1 = listGame.head
				if(hasPawnMoved(game, game1)) {
					return false
				}
				if(n != compteur(game1)) {
					return false
				}
				return count_repet(game, listGame.tail, k+1)
			}
		}
		return count_repet(game, listGame, 0)
	}

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
			//writer.write(CreateAlgraebricFromMove(move.p, move.pos, n, game ))
			//game.move(move.p, move.pos)
			n = n +1
		}
		writer.close()
	}
}

