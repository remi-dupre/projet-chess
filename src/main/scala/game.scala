case class Pos(x : Int, y : Int)

/**
 * Représente une partie d'échecs
 */
class Game {
	/** Liste des pièces sur le plateau */
	var pieces : List[Piece] = List()

	/** Les deux joueurs */
	val players : Array[Player] = Array(null, null)

	/** L'indice dans 'players' du joueur qui doit jouer */
	var playing = 0

	/** Une fonction qui est appelée quand le jeu est modifié */
	var changed = () => {}

	/** Initialise le plateau de jeu et lance la partie */
	def start = {
		for(i <- 0 to 7) {
			pieces = new Pawn(this, 0, Pos(i, 6)) :: pieces
			pieces = new Pawn(this, 1, Pos(i, 1)) :: pieces
		}
		pieces =
			new King(this, 0, Pos(4, 7)) ::
			new King(this, 1, Pos(4, 0)) ::
			new Queen(this, 0, Pos(3, 7)) ::
			new Queen(this, 1, Pos(3, 0)) ::
			new Rook(this, 0, Pos(0, 7)) ::
			new Rook(this, 0, Pos(7, 7)) ::
			new Rook(this, 1, Pos(0, 0)) :: 
			new Rook(this, 1, Pos(7, 0)) ::
			new Knight(this, 0, Pos(1, 7)) ::
			new Knight(this, 0, Pos(6, 7)) ::
			new Knight(this, 1, Pos(1, 0)) ::
			new Knight(this, 1, Pos(6, 0)) ::
			new Bishop(this, 0, Pos(2, 7)) ::
			new Bishop(this, 0, Pos(5, 7)) ::
			new Bishop(this, 1, Pos(2, 0)) ::
			new Bishop(this, 1, Pos(5, 0)) ::
			pieces

		playing = 0
		changed()
		players(playing).wait_play
	}

	/** Retourne l'id du joueur qui contrôle la piece en position (x, y), -1 s'il n'y en a pas */
	def cell_player(x : Int, y : Int) : Int = {
		for(c <- pieces) c match {
			case Piece(_, p, Pos(i, j)) if (i, j) == (x, y) => return p
			case _ => ()
		}
		return -1
	}

	/** Retourne le rôle de la pièce (ex : "king") */
	def cell_role(x : Int, y : Int) : String = {
		for(c <- pieces) c match {
			case Piece(_, _, Pos(i, j)) if (i, j) == (x, y) => return c.role
			case _ => ()
		}
		return "empty"
	}

	/** Vérifie si la case (x, y) est vide */
	def empty_cell(x : Int, y : Int) : Boolean = {
		for(c <- pieces) c match {
			case Piece(_, _, Pos(i, j)) if (i, j) == (x, y) => return false
			case _ => ()
		}
		return true
	}

	/** Supprime la pièce p de la partie */
	def remove(p : Piece) = {
		def run(l : List[Piece]) : List[Piece] = l match {
			case List() => List()
			case Piece(_, _, pos)::subpieces if pos == p.pos => subpieces
			case piece::subpieces => piece :: run(subpieces)
		}
		pieces = run(pieces)
	}

	/** Déplace la pièce 'p' en position 'pos'
	 * Si ce n'est pas possible, retourne false */
	def move(p : Piece, pos : Pos) : Boolean = {
		val possibleMoves : List[Pos] = p.removeInCheckMoves(p.possible_move())

		for(position <- possibleMoves) position match {
			case position if position == pos =>
				val ans = getPiece(pos.x, pos.y)
				if(ans != null) remove(ans)
				p.pos = pos
				if(p.role == "pawn" && (p.pos.y == 7 || p.pos.y == 0)) {
					remove(p)
					pieces = (new Queen(this, p.player, pos)) :: pieces
				}

				p.already_moved = true
				playing = 1 - playing
				changed()

				if(!over) {
					players(playing).wait_play
				}
				return true
			case _ => Nil
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
		for(c <- pieces) c match {
			case piece if (piece.player == player) => 
				pos_move = pos_move ++ piece.possible_move
			case _ => ()
		}
		return pos_move
	}

	/** retourn la liste des positions où le jouer donné peut LEGALEMENT déplacer sa pièce **/
	def every_possible_move_nocheck(player : Int) : List[Pos] = { 
		var pos_move : List[Pos] = List()
		for(c <- pieces) c match {
			case piece if (piece.player == player) => 
				pos_move = pos_move ++ piece.removeInCheckMoves(piece.possible_move)
			case _ => ()
		}
		return pos_move
	}


	/** voir si le roi du player est en echec */
	def inCheck(player : Int) : Boolean = {
		var pos_move : List[Pos] = every_possible_move(1 - player)
		var pos : Pos = Pos(-1,-1)
		for(c <- pieces) c match {
			case piece if ( (piece.role == "king") && (piece.player == player) ) => pos = piece.pos
			case _ => ()
		}
		for(position <- pos_move) position match {
			case position if (position == pos) => return true
			case _ => ()
		}
		return false
	}

	/** Retourne l'éventuelle pièce présente en (i, j) */
	def getPiece(i : Int, j : Int) : Piece = {
		for(c <- pieces) c match {
			case piece if (piece.pos == Pos(i,j)) => return piece
			case _ => ()
		}
		return null
	}

	def getControlledCell(i : Int, j: Int, player : Int) : Boolean = {
		val pos_move : List[Pos] = every_possible_move_nocheck(1 - player)
		var Booly = false
		for(c <- pos_move) c match {
			case Pos(x,y) if ( x == i, y == j ) => Bool = true
			case Pos(x,y) => ()
		}
		return Booly
	}
}

