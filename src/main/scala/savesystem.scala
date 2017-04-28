import scala.io.Source
import java.io._

case class Move(from: Pos, to: Pos) {
	/** Représente un tour d'action pour un joueur */
	var promote_to : String = null // S'il y a promotion, sa nature
}

class FakePlayer(color : Int, game : Game, promotion_type : String) extends Player(color, game) {
	/** Une classe qui simule un joueur inactif
	 * Elle sert surtout à stocker un promotion à effectuer pour un "move"
	 */

	override def get_promotion_type : String = {
		return promotion_type
	}
}

case class Save(var move: Move, var saveList: List[Save], var father: Save) {
	/** Arbre de sauvegardes */

	/** La game représentée par ce noeud de l'arbre */
	var game_state : Game = null
	
	/** Applique les moves successifs à la fame passée en argument */
	def apply_moves(game : Game = new Game()) : Unit = {
		if(!saveList.isEmpty) {
			saveList.head.apply_moves(game)
		}
		val save_players = game.players
		game.players = Array(
			new FakePlayer(0, game, move.promote_to),
			new FakePlayer(1, game, move.promote_to)
		)
		game.move(
			game.board(move.from.x)(move.from.y),
			move.to
		)
		game.players = save_players
	}

	/** La liste des games vus dans le chemin depuis la racine */
	def game_list(game : Game = new Game()) : List[Game] = {
		if(game_state == null) {
			val game_c = game.copy
			game_c.players(game.playing) = new FakePlayer(game.playing, game_c, move.promote_to)
			game_c.move(
				game_c.board(move.from.x)(move.from.y),
				move.to
			)
			game_state = game_c
		}
		if( saveList.isEmpty )
			return List(game_state)
		else
			return game_state :: saveList.head.game_list(game_state)
	}

	/** La liste des moves successifs */
	def move_list : List[Move] = {
		if( saveList.isEmpty )
			return List(move)
		else
			return move :: saveList.head.move_list
	}
}

object Backup {
	/* Algebrique notation : n. (Z)z9 (Z)z9 */

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
		val n = game.nb_pieces
		var i = 0
		val listGame = save.game_list().reverse
		def count_repet(game:Game, listGame:List[Game]) : Boolean = {
			if(listGame.isEmpty) {
				return false
			}
			else {
				val hd_game = listGame.head
				if(n != hd_game.nb_pieces)
					return false
				if(game.is_copy_of(hd_game)) {
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
		val n = game.nb_pieces
		val listGame = save.game_list().reverse
		def count_repet_bis(game:Game, listGame:List[Game], k:Int) : Boolean = {
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
				if(n != game1.nb_pieces) {
					return false
				}
				return count_repet_bis(game, listGame.tail, k+1)
			}
		}
		return count_repet_bis(game, listGame, 0)
	}

	def addMoveToSave(move: Move, save: Save):Unit = {
		if(save.saveList.isEmpty) {
			save.saveList = List(Save(move, List(), save))
		}
		else {
			addMoveToSave(move, save.saveList.head)
		}
	}
	def createSaveFromPGN(filename:String) : Save = {
		var game = new Game()
		var bool = false
		var move_list : Save = null
		var n = 0
		var i = -1
		var j = -1
		var bloc = 0
		var x = -1
		var y = -1
		var last_is_eq = false
		var role : String = null
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
						j = c - '1'
					}

					if ((c - 'a') >= 0 && (c - 'a') <= 7 && bloc == 2) {
						x = c - 'a'
					}

					if ((c - '0') >= 1 && (c - '0') <= 8 && bloc == 2) {
						y = c - '1'
					}
					if(last_is_eq) {
						role = c match {
							case 'Q' => "queen"
							case 'K' => "knight"
							case 'B' => "bishop"
							case 'R' => "rook"
							case _ => println("promotion not specified") ; null
						}
					}
					last_is_eq = (c == '=')

					if (bloc == 3) {
						val move = Move(Pos(x, y), Pos(i, j))
						move.promote_to = role

						if(move_list == null) {
							move_list = Save(move, List(), null)
						}
						else {
							move_list = Save(move, List(move_list), null)
						}
						n = 0
						i = -1
						j = -1
						bloc = 0
						x = -1
						y = -1
						role = null
					}
				}
			}
		}
		def make_papa(save : Save, papa : Save = null) {
			save.father = papa
			if(!save.saveList.isEmpty) {
				make_papa(save.saveList.head, save)
			}
		}
		make_papa(move_list)
		return move_list
	}

	def CreateAlgraebricFromMove(p : Piece, pos : Pos, n : Int, game: Game, promotion : String) : String = { /* y = {1,8}, x = {a,h} */
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
		var res = n.toString + ". " + a + ('a' + x).toChar + (y+1).toString + " " + b + ('a' + i).toChar + (j+1).toString
		if(p.role == "pawn" && (pos.y == 0 || pos.y == 7)) {
			res += "=" + (promotion(0) - 32).toChar
		}
		return res + " "
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
			writer.write(CreateAlgraebricFromMove(piece, move.to, n, game, move.promote_to))
			game.players = Array(
				new FakePlayer(0, game, move.promote_to),
				new FakePlayer(1, game, move.promote_to)
			)
			game.move(piece, move.to)
			n = n +1
		}
		writer.write("\n")
		writer.close()
	}
}

