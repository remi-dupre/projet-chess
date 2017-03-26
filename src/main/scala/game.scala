import scala.math.abs

case class Pos(x : Int, y : Int)

/**
 * Représente une partie d'échecs
 */
class Game {
	/** Plateau de jeu */
	var board = Array.ofDim[Piece](8, 8)

	/** Les deux joueurs */
	val players : Array[Player] = Array(null, null)

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
	}

	def start = {
		init
		playing = 0
		changed()
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
		return ( board(x)(y) == null )
	}

	/** Supprime la pièce p de la partie */
	def remove(p : Piece) = {
		board(p.pos.x)(p.pos.y) = null
	}

	/** Déplace la pièce 'p' en position 'pos'
	 * Si ce n'est pas possible, retourne false */
	def move(p : Piece, pos : Pos) : Boolean = {
		val possibleMoves : List[Pos] = p.removeInCheckMoves(p.possible_move())

		if(possibleMoves.contains(pos)) {
			if(save_root == null) { // Un nouvel arbre de sauvegardes est nécessaire
				save_root = Save(Move(p.pos, pos), List())
				save_current = save_root
			}
			else {
				val new_save = Save(Move(p.pos, pos), List())
				save_current.saveList = new_save :: save_current.saveList
				save_current = new_save
				//Backup.addMoveToSave(Move(p, pos), save)
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
						case _ => println("Promotion was refused") ; board(pos.x)(pos.y)
					}
				}
				save_current.move.promote_to = board(pos.x)(pos.y).role
			}
			p.already_moved = turn
			playing = 1 - playing
			turn = turn + 1
			changed()
			
			if(players(playing) != null && !over) {
				players(playing).wait_play
			}
			return true
		}
		return false
	}

	def over : Boolean = {
		(save_root != null && Backup.tripleRepetition(this, save_root)) || every_possible_move_nocheck(playing).isEmpty
	}

	def victory : Boolean = {
		over && inCheck(playing)
	}

	def pat : Boolean = {
		!victory && over
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
	def every_possible_move_nocheck(player : Int) : List[Pos] = { 
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
		var ret = true
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				val p1 = board(i)(j)
				val p2 = game_c.board(i)(j)
				ret = ret && ((p1 == p2) // Gère le cas "null == null"
					|| ((p1 != null && p2 != null) && (p1.role, p1.player) == (p2.role, p2.player)))
			}
		}
		return ret
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
}

