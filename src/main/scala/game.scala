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
	var save: Save = Save(Move(null,Pos(-1,-1)), List())

	/** Initialise le plateau de jeu et lance la partie */
	def start = {
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

		playing = 0
		changed()
		players(playing).wait_play
	}

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
		for(position <- possibleMoves)
        if(position == pos) {
        	/* prise en passant */
        	if(p.role == "pawn") {
	        	if(Math.abs(p.pos.y - pos.y) == 2) {
		    		p.Pawn_Rules = true
			    }
			    if(Math.abs(p.pos.x - pos.x) == 1) {
			    	remove(board(pos.x)(pos.y - (-1+2*playing)))
			    }
			}
			/* petit roque */
			if(p.role == "king" && p.pos.x - pos.x == -2) {
				board(p.pos.x+1)(p.pos.y) = board(p.pos.x+3)(p.pos.y)
				remove(board(p.pos.x+3)(p.pos.y))
			}
			/* grand roque */
			if(p.role == "king" && p.pos.x - pos.x == 3) {
				board(p.pos.x-2)(p.pos.y) = board(p.pos.x-4)(p.pos.y)
				remove(board(p.pos.x-4)(p.pos.y))
			}


			/* REMPLACEMENT DANS LA PARTIE */
            board(pos.x)(pos.y) = p
            remove(p)
            /* REMPLACEMENT DANS LA PARTIE */

			p.pos = pos
            if(p.role == "pawn") {
                if(p.pos.y == 7 || p.pos.y == 0) {
    			    board(pos.x)(pos.y) = new Queen(this, p.player, pos)
	    	    }
            }
			p.already_moved = turn
			playing = 1 - playing
			if(turn == 0) {
				save = Save(Move(p, pos), List())
			}
			else {
				Backup.addMoveToSave(Move(p, pos), save)
			}
			turn = turn + 1
			changed()
			
			if(!over) {
				players(playing).wait_play
			}
			return true
		}
		return false
	}

    def over : Boolean = {
        every_possible_move_nocheck(playing).isEmpty
    }

    def victory : Boolean = {
        over && inCheck(playing)
    }

    def pat : Boolean = {
        !victory && over
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

	def getControlledCell(i : Int, j: Int, player : Int) : Boolean = {
		val pos_move : List[Pos] = every_possible_move(1 - player)
		var ret = false
		for(c <- pos_move) {
            ret = ret || (c.x == i && c.y == j)
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
    				g.board(i)(j) = board(i)(j).role match {
	    				case "king" => new King(g, c.player, c.pos)
		    			case "queen" => new Queen(g, c.player, c.pos)
			    		case "rook" => new Rook(g, c.player, c.pos)
				    	case "bishop" => new Bishop(g, c.player, c.pos)
					    case "knight" => new Knight(g, c.player, c.pos)
    					case "pawn" => new Pawn(g, c.player, c.pos)
	    				case _ => null
              		}
          	    	g.board(i)(j).already_moved = c.already_moved
            	}
            }
        }
        return g
    }
}

