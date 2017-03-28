import scala.math.abs

object GameType extends Enumeration {
	val Normal, Proteus = Value
}

case class Pos(x : Int, y : Int)

/**
 * Représente une partie d'échecs
 */
class Game() {
	/** Plateau de jeu */
	var board = Array.ofDim[Piece](8, 8)

	/** Les deux joueurs */
	var players : Array[Player] = Array(null, null)
	val timer = new Cadency(List(
		Period(60, 3), Period(50, 3), Period(50, 3), Period(50, 3), Period(50, 3), Period(10, 1)
	))
	var timers : Array[Cadency] = Array(timer.copy, timer.copy)
	var turn_start : Long = -1 // le timestamp du début du tour

	/** L'indice dans 'players' du joueur qui doit jouer */
	/** 0 = blanc, 1 = noir **/
	var playing = 0

	/** Une fonction qui est appelée quand le jeu est modifié */
	var changed = () => {}

	/** Nombre de tours de la partie **/
	var turn = 0

	/** Système de sauvegarde **/
	var save_root: Save = null
	var save_current : Save = null

	/** Initialise le plateau de jeu et lance la partie */
	def init = {
		for(i <- 0 to 7) {
			board(i)(1) = new Pawn(this, 1, Pos(i, 1))
			board(i)(6) = new Pawn(this, 0, Pos(i, 6))
		}
		board(4)(7) = new King(this, 0, Pos(4, 7))
		board(4)(0) = new King(this, 1, Pos(4, 0))
		board(3)(7) = new Queen(this, 0, Pos(3, 7))
		board(3)(0) = new Queen(this, 1, Pos(3, 0))
		board(0)(7) = new Rook(this, 0, Pos(0, 7))
		board(7)(7) = new Rook(this, 0, Pos(7, 7))
		board(0)(0) = new Rook(this, 1, Pos(0, 0))
		board(7)(0) = new Rook(this, 1, Pos(7, 0))
		board(1)(7) = new Knight(this, 0, Pos(1, 7))
		board(6)(7) = new Knight(this, 0, Pos(6, 7))
		board(1)(0) = new Knight(this, 1, Pos(1, 0))
		board(6)(0) = new Knight(this, 1, Pos(6, 0))
		board(2)(7) = new Bishop(this, 0, Pos(2, 7))
		board(5)(7) = new Bishop(this, 0, Pos(5, 7))
		board(2)(0) = new Bishop(this, 1, Pos(2, 0))
		board(5)(0) = new Bishop(this, 1, Pos(5, 0))
		turn_start = tools.timestamp
	}

	def start = {
		init
		playing = 0
		changed()
		println("dan")
		players(playing).wait_play
	}

	init

	/** Retourne l'id du joueur qui contrôle la piece en position (x, y), -1 s'il n'y en a pas */
	def cell_player(x : Int, y : Int) : Int = {
		if ( board(x)(y) == null ) 
			{ return -1 }
		else 
			{ return board(x)(y).player }
	}

	/** Retourne le rôle de la pièce (ex : "king") */
	def cell_role(x : Int, y : Int) : String = {
		if ( board(x)(y) == null ) 
			{ return "empty" }
		else 
			{ return board(x)(y).role }
	}

	/** Vérifie si la case (x, y) est vide */
	def empty_cell(x : Int, y : Int) : Boolean = {
		return board(x)(y) == null
	}

	/** Déplace la pièce 'p' en position 'pos'
	 * Si ce n'est pas possible, retourne false */
	def move(p : Piece, pos : Pos) : Boolean = {
		val possibleMoves : List[Pos] = p.removeInCheckMoves(p.possible_move())

		if(possibleMoves.contains(pos)) {
			if(timers != null) {
				timers(playing).spend(tools.timestamp-turn_start)
			}
			if(save_root == null) { // Un nouvel arbre de sauvegardes est nécessaire
				save_root = Save(Move(p.pos, pos), List())
				save_current = save_root
			}
			else {
				val new_save = Save(Move(p.pos, pos), List())
				save_current.saveList = new_save :: save_current.saveList
				save_current = new_save
			}

			p.move_to(pos)

			if(p.role == "pawn") {
				if(p.pos.y == 7 || p.pos.y == 0) {
					val new_role = players(playing).get_promotion_type
					board(pos.x)(pos.y) = new_role match {
						case "queen"  => new Queen(this, p.player, pos)
						case "knight" => new Knight(this, p.player, pos)
						case "bishop" => new Bishop(this, p.player, pos)
						case "rook"   => new Rook(this, p.player, pos)
						case s => println("Promotion was refused : " + s) ; board(pos.x)(pos.y)
					}
					save_current.move.promote_to = board(pos.x)(pos.y).role
				}
			}
			p.already_moved = turn
			playing = 1 - playing
			turn = turn + 1
			changed()
			
			if(players(playing) != null && !over) {
				turn_start = tools.timestamp
				players(playing).wait_play
			}
			return true
		}
		return false
	}

	def triple_repetition : Boolean = {
		if(save_root == null)
			return false

		val n =  nb_pieces
		var i = 0
		val listGame = save_root.game_list().reverse
		def count_repet(listGame:List[Game]) : Boolean = {
			if(listGame.isEmpty) {
				return false
			}
			else {
				val hd_game = listGame.head
				if( n != hd_game.nb_pieces )
					return false
				if( is_copy_of(hd_game)
				  && legal_moves(0) == hd_game.legal_moves(0)
				  && legal_moves(1) == hd_game.legal_moves(1) ) {
					i += 1
					if(i == 3)
						return true
				}
				return count_repet(listGame.tail)
			}
		}
		return count_repet(listGame)
	}

	def over : Boolean = {
		triple_repetition || legal_moves(playing).isEmpty || impossibilityOfCheckMate || defeat
	}

	def time_defeat : Boolean = {
		return timers != null && timers(1-playing).free_time < 0
	}

	def defeat : Boolean = {
		return time_defeat
	}

	def victory : Boolean = {
		(over && inCheck(playing) && !defeat)
	}

	def pat : Boolean = {
		!defeat && !victory && over
	}

	def nb_pieces : Int = {
		var n = 0
		for(i <- 0 to 7)
		for(j <- 0 to 7)
			if( board(i)(j) != null )
				n += 1
		return n
	}

	/** Retourne la liste des positions attaquées */
	def every_attacked_cells(player : Int) : List[Pos] = {
	   	var pos_move : List[Pos] = List()
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				if ( board(i)(j) != null && board(i)(j).player == player ) {
					pos_move = pos_move ++ board(i)(j).attacked_cells
				}
			}
		}
		return pos_move
	}

	/** Retourne la liste des positions où le joueur donné peut déplacer une pièce mais en se mettant en echec */
	def every_possible_move(player : Int) : List[Pos] = { 
		var pos_move : List[Pos] = List()
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				if ( board(i)(j) != null && board(i)(j).player == player ) {
					pos_move = pos_move ++ board(i)(j).possible_move
				}
			}
		}
		return pos_move
	}

	/** retourn la liste des positions où le jouer donné peut LEGALEMENT déplacer sa pièce **/
	def legal_moves(player : Int) : List[Pos] = { 
		var pos_move : List[Pos] = List()
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				if ( board(i)(j) != null && board(i)(j).player == player ) {
					pos_move = pos_move ++ board(i)(j).removeInCheckMoves(board(i)(j).possible_move)
				}
			}
		}
		return pos_move
	}

	/** voir si le roi du player est en echec */
	def inCheck(player : Int) : Boolean = {
		var pos_move : List[Pos] = every_possible_move(1 - player)
		var pos : Pos = Pos(-1,-1)
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				if ( board(i)(j) != null && board(i)(j).role == "king" && board(i)(j).player == player ) {
					pos = board(i)(j).pos
				}
			}
		}
		for(position <- pos_move) {
			if (position == pos) { 
				return true
			}
		}
		return false
	}

	/** Retourne l'éventuelle pièce présente en (i, j) */
	def getPiece(i : Int, j : Int) : Piece = {
		return board(i)(j)
	}

	def isControlledCell(i : Int, j: Int, player : Int) : Boolean = {
		return every_attacked_cells(1 - player).contains(Pos(i, j))
	}

	def is_copy_of(game_c : Game) : Boolean = {
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				val p1 = board(i)(j)
				val p2 = game_c.board(i)(j)
				if( !(p1 == null && p2 == null) )
				if( (p1 != null && p2 == null) || (p1 == null && p2 != null)
				  || (p1.role != p2.role) || (p1.player != p2.player) )
					return false
			}
		}
		return true
	}

	def copy : Game = {
		val g = new Game
		// g.players = players //!\\ Checker la stabilité de cette ligne si elle a un interêt un jours 
		g.playing = playing
		g.turn = turn
		// g.changed -> ne semble pas pertinent
		g.board = Array.ofDim(8, 8)
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				val c = board(i)(j)
				if( c != null) {
					g.board(i)(j) = c.copy_for(g)
				}
			}
		}
		return g
	}

	def impossibilityOfCheckMate: Boolean = {
		var white = 0
		var black = 0
		var whiteBishop = 0
		var blackBishop = 0
		var res = false
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				if( board(i)(j) != null && board(i)(j).role != "king") {
					if( board(i)(j).role == "queen" || board(i)(j).role == "rook" || board(i)(j).role == "pawn") {
						return false
					}
					else if(board(i)(j).role == "bishop") {
						if(board(i)(j).player == 0) {
							white = white + 1
							whiteBishop = 2- (i+j)%2
						}
						else {
							black = black + 1
							blackBishop = 2 - (i+j)%2
						}
					}
					else {
						if(board(i)(j).player == 0) {
							white = white + 1
						}
						else {
							black = black + 1
						}
					}
				}
			}
		}
		res = res || (white == 0 && black <= 1) || (white <= 1 && black == 0) || ( white == 1 && black == 1 && whiteBishop != 0 && whiteBishop == blackBishop )
		return res
	}
}
