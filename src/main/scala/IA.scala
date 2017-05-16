
object IATools {

	var whitePoint = 24050
	var blackPoint = 24050

	var points = Array.ofDim[Int](2)

	points(0) = whitePoint
	points(1) = blackPoint

	val bonusBishopPoints = 50
	val penaltyNoPawnPoints = -50
	val facteurRookPossibleMove = 1
	var matos = true
	var position = true
	var bishop = true
	var nopawn = true
	var rookies = false
	
	def hash(s : String) : Int = {
		if( s == "king" ) {
			return 0
		}
		if ( s == "queen" ) {
			return 1
		}
		if ( s == "rook" ) {
			return 2
		}
		if ( s == "knight" ) {
			return 3
		}
		if ( s == "bishop" ) {
			return 4
		}
		return 5
	}
	/* les matrices qui reconsidère les points des pièces selon leurs positions sur le board */
	var pointPositionning = Array.ofDim[Int](2, 6, 8, 8) /* 0 = king, 1 = queen, 2 = rook, 3 = knight, 4 = bishop, 5 = pawn */

	/* king early game */
	pointPositionning(0)(0) = 
	Array(
	Array(-50,-40,-30,-20,-20,-30,-40,-50),
	Array(-30,-20,-10,  0,  0,-10,-20,-30),
	Array(-30,-10, 20, 30, 30, 20,-10,-30),
	Array(-30,-10, 30, 40, 40, 30,-10,-30),
	Array(-30,-10, 30, 40, 40, 30,-10,-30),
	Array(-30,-10, 20, 30, 30, 20,-10,-30),
	Array(-30,-30,  0,  0,  0,  0,-30,-30),
	Array(-50,-30,-30,-30,-30,-30,-30,-50)
	)
	pointPositionning(1)(0) = reverseArray(pointPositionning(1)(0))
	/* queen */
	pointPositionning(0)(1) = 
	Array(
	Array(-20,-10,-10, -5, -5,-10,-10,-20),
	Array(-10,  0,  0,  0,  0,  0,  0,-10),
	Array(-10,  0,  5,  5,  5,  5,  0,-10),
	Array(-5,  0,  5,  5,  5,  5,  0, -5),
	Array( 0,  0,  5,  5,  5,  5,  0, -5),
	Array(-10,  5,  5,  5,  5,  5,  0,-10),
	Array(-10,  0,  5,  0,  0,  0,  0,-10),
	Array(-20,-10,-10, -5, -5,-10,-10,-20)
	)
	pointPositionning(1)(1) = reverseArray(pointPositionning(1)(1))
	/* rook */
	pointPositionning(0)(2) =
	Array(
	Array(0,  0,  0,  0,  0,  0,  0,  0),
	Array( 5, 10, 10, 10, 10, 10, 10,  5),
	Array(-5,  0,  0,  0,  0,  0,  0, -5),
	Array(-5,  0,  0,  0,  0,  0,  0, -5),
	Array(-5,  0,  0,  0,  0,  0,  0, -5),
	Array(-5,  0,  0,  0,  0,  0,  0, -5),
	Array(-5,  0,  0,  0,  0,  0,  0, -5),
	Array( 0,  0,  0,  5,  5,  0,  0,  0)
	)
	pointPositionning(1)(2) = reverseArray(pointPositionning(1)(2))
	/* knight */
	pointPositionning(0)(3) =
	Array(
	Array(-50,-40,-30,-30,-30,-30,-40,-50),
	Array(-40,-20,  0,  0,  0,  0,-20,-40),
	Array(-30,  0, 10, 15, 15, 10,  0,-30),
	Array(-30,  5, 15, 20, 20, 15,  5,-30),
	Array(-30,  0, 15, 20, 20, 15,  0,-30),
	Array(-30,  5, 10, 15, 15, 10,  5,-30),
	Array(-40,-20,  0,  5,  5,  0,-20,-40),
	Array(-50,-40,-30,-30,-30,-30,-40,-50)
	)
	pointPositionning(1)(3) = reverseArray(pointPositionning(1)(3))
	/* bishop */
	pointPositionning(0)(4) =
	Array(
	Array(-20,-10,-10,-10,-10,-10,-10,-20),
	Array(-10,  0,  0,  0,  0,  0,  0,-10),
	Array(-10,  0,  5, 10, 10,  5,  0,-10),
	Array(-10,  5,  5, 10, 10,  5,  5,-10),
	Array(-10,  0, 10, 10, 10, 10,  0,-10),
	Array(-10, 10, 10, 10, 10, 10, 10,-10),
	Array(-10,  5,  0,  0,  0,  0,  5,-10),
	Array(-20,-10,-10,-10,-10,-10,-10,-20)
	)
	pointPositionning(1)(4) = reverseArray(pointPositionning(1)(4))
	/* pawn */
	pointPositionning(0)(5) =
	Array(
	Array(0,  0,  0,  0,  0,  0,  0,  0),
	Array(50, 50, 50, 50, 50, 50, 50, 50),
	Array(10, 10, 20, 30, 30, 20, 10, 10),
	Array( 5,  5, 10, 25, 25, 10,  5,  5),
	Array( 0,  0,  0, 20, 20,  0,  0,  0),
	Array( 5, -5,-10,  0,  0,-10, -5,  5),
	Array( 5, 10, 10,-20,-20, 10, 10,  5),
	Array( 0,  0,  0,  0,  0,  0,  0,  0)
	)
	pointPositionning(1)(5) = reverseArray(pointPositionning(1)(5))

	def initPoint(game: Game) : Unit = {
		whitePoint += game.board(4)(0).value 		/* king = 20 000 */
		whitePoint += game.board(3)(0).value 		/* queen =  900 */
		whitePoint += game.board(7)(0).value * 2    /* rook =   500 */
		whitePoint += game.board(6)(0).value * 2 	/* knight =  325 */
		whitePoint += game.board(5)(0).value * 2 	/* bishop =  350 */
		whitePoint += game.board(0)(1).value * 8  /* */ 

		blackPoint = whitePoint
	}

	/* Chose à faire ici : */
	var bonusesPenalties = Array.ofDim[Boolean](2,5) /* 0 = blanc, 1 = noir */
	bonusesPenalties(0)(0) = true
	bonusesPenalties(0)(1) = false
	bonusesPenalties(0)(2) = false
	bonusesPenalties(0)(3) = false
	bonusesPenalties(0)(4) = false

	bonusesPenalties(1)(0) = true
	bonusesPenalties(1)(1) = false
	bonusesPenalties(1)(2) = false
	bonusesPenalties(1)(3) = false
	bonusesPenalties(1)(4) = false

	def copyGame(game: Game) : Game = {
		val g = new Game
		g.copy_config_of(game)
		return g
	}

	def everyPossibleMoves(player: Int, game: Game) : List[(Piece, Pos)] = {
		var pos_move : List[(Piece, Pos)] = List()
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				val piece = game.board(i)(j)
				if(piece != null && piece.player == player) {
					for(pos <- piece.removeInCheckMoves(piece.possible_move)) {
						pos_move = (piece, pos) :: pos_move
					}
				}
			}
		}
		return pos_move
	}

	def everyCaptureMoves(player: Int, game: Game) : List[(Piece, Pos)] = {
		var pos_move : List[(Piece, Pos)] = List()
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				val piece = game.board(i)(j)
				if(piece != null && piece.player == player) {
					for(pos <- piece.removeInCheckMoves(piece.possible_move)) {
						val x = pos.x
						val y = pos.y
						if((game.board(x)(y) != null) && (game.board(x)(y).player == 1-player)) {
							pos_move = (piece, pos) :: pos_move
						}
					}
				}
			}
		}
		return pos_move
	}

	def reverseArray(position: Array[Array[Int]]) : Array[Array[Int]] = {
		val newArray = Array.ofDim[Int](8, 8)
		for(i <- 0 to 7) {
			for (j <- 0 to 7) {
				newArray(i)(j) = position(7-i)(j)
			}
		}
		return newArray
	}

	def equalPos(x:Pos, y:Pos) : Boolean = {
		return (x.x==y.x)&&(x.y==y.y)
	}
	def supElem(list:List[Pos], x:Pos):List[Pos] = {
		if(list.isEmpty) {
			return List()
		}
		val hd = list.head
		if(equalPos(hd, x)) {
			return supElem(list.tail, x)
		}
		else {
			return hd::supElem(list.tail, x)
		}

	}
	def supDoublon(list:List[Pos]) : List[Pos] = {
		if(list.isEmpty) {
			return List()
		}
		val hd = list.head
		return hd::supDoublon(supElem(list, hd))

	}

	/* Fonctions essentiels */

	def evaluate(game: Game) : Array[Int] = {
		/* voir en début de fichier pour matos, position, bishop, nopawn, rookies */
		val points = Array(0, 0)
		val bonusBishopPair = Array(0, 0)
		val penaltyNoPawn = Array(0, 0)
		var possibleRookmoves : Array[List[Pos]] = Array(List(), List())
		for(i <- 0 to 7) {
			for (j <- 0 to 7) {
				val piece = game.board(i)(j)
				if( piece != null ) {
					val player = piece.player
					if(matos) {
						points(player) += piece.value /* matériel sur l'échiquier' */
					}
					if(position) {
						points(player) += pointPositionning(player)(hash(piece.role))(i)(j) /* positionnement des pièces sur l'échiquier */
					}
					if(bishop) {
						if( piece.role == "bishop") {
							bonusBishopPair(player) += 1 /* pair de fou, vaut mieux que 2 fou indépendant : Bonus Bishop pair */
						}
					}
					if(nopawn) {
						if( piece.role == "pawn") {
							penaltyNoPawn(player) += 1 /* Malus si plus aucun pions -> mauvais pour la fin de partie */
						}
					}
					if(rookies) {
						if(piece.role == "rook") { /* plus les tours ont de choix de moves, plus c'est intéressant !*/
							possibleRookmoves(player) = piece.removeInCheckMoves(piece.possible_move) ++ possibleRookmoves(player) 
						}
					}
				}
			}
		}
		for(i <- 0 to 1) {
			if(bishop && bonusBishopPair(i) == 2) {
				points(i) += bonusBishopPoints 
			}
			if(nopawn && penaltyNoPawn(i) == 0) {
				points(i) -= penaltyNoPawnPoints 
			}
			if(rookies) {
				points(i) += supDoublon(possibleRookmoves(i)).length * facteurRookPossibleMove 
			}
		}
		return points
	}

	def quiesce(player: Int, alpha : Int, beta : Int, game : Game) : Int = {
		var a = alpha
		var b = beta
		val points = evaluate(game)
		val standPat = points(player) - points(1-player)
		if( standPat >= b ) {
			return b
		}
		if(a < standPat) {
			a = standPat
		}
		val captureMoves = everyCaptureMoves(player, game)
		for(move <- captureMoves) {
			var piece0 = move._1
			val pos = move._2
			val copygame = copyGame(game)
			val piece = copygame.board(piece0.pos.x)(piece0.pos.y)
			val movebis = (piece, pos)
			piece.move_to(pos)

			val v = -quiesce(1-player, -b, -a, copygame)
			if( v >= b ) {
				return b
			}
			if ( v > a) {
				a = v
			}
		}
		return a
	}

	def playab(player: Int, alpha : Int, beta : Int, game : Game, depth : Int) : (Piece, Pos) = {
		var max_val = -70000
		var a = alpha
		var b = beta
		val pos_move = everyPossibleMoves(player, game)
		var best_move : (Piece, Pos) = (null, null)
		for(move <- pos_move) {
			var piece0 = move._1
			val pos = move._2
			val copygame = copyGame(game)
			val piece = copygame.board(piece0.pos.x)(piece0.pos.y)
			val movebis = (piece, pos)
			piece.move_to(pos)

			val value = ab(copygame, depth, a, b, 1-player, false)
			if( value > max_val) {
				max_val = value
				best_move = (piece0, pos)
			}
		}
		return best_move
	}

	def ab(game : Game, depth : Int, alpha : Int, beta : Int, player : Int, maximizingPlayer : Boolean) : Int = {
		if(depth == 0) {
			val points = evaluate(game)
			return quiesce(player, alpha, beta, game)
		}
		var a = alpha
		var b = beta
		val pos_move = everyPossibleMoves(player, game)
		var best_move :(Piece, Pos) = (null, null)
		if(maximizingPlayer) {
			var v = -70000
			for(move <- pos_move) {
				var piece0 = move._1
				val pos = move._2
				val copygame = copyGame(game)
				val piece = copygame.board(piece0.pos.x)(piece0.pos.y)
				val movebis = (piece, pos)
				piece.move_to(pos)

				val value = ab(copygame, depth - 1, a, b, 1-player, false)
				if( value > v) {
					v = value
				}
				if( a < v) {
					a = v
				}
				if( b <= a) {
					return v
				}
			}
			return v
		}
		else {
			var v = 70000
			for(move <- pos_move) {
				var piece0 = move._1
				val pos = move._2
				val copygame = copyGame(game)
				val piece = copygame.board(piece0.pos.x)(piece0.pos.y)
				val movebis = (piece, pos)
				piece.move_to(pos)

				val value = ab(copygame, depth - 1, a, b, 1-player, true)
				if( v > value) {
					v = value
				}
				if( b > v) {
					b = value
				}
				if(b <= a) {
					return v
				}
			}
			return v
		}
	}
}
