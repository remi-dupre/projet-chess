
object IATools {

	var whitePoint = 24050
	var blackPoint = 24050

	var points = Array.ofDim[Int](2)

	points(0) = whitePoint
	points(1) = blackPoint

	val bonusBishopPoints = 50
	val penaltyNoPawnPoints = -50

	/* les matrices qui reconsidère les points des pièces selon leurs positions sur le board */
	def hash(s : String) : Int = {
		if( s == "king" ) return 0
		else if ( s == "queen" ) return 1
		else if ( s == "rook" ) return 2
		else if ( s == "knight" ) return 3
		else if ( s == "bishop" ) return 4
		return 5
	}
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
	Array( 5, -5,-10,  9999999,  0,-10, -5,  5),
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
	/* 
	Le tableau sert à gagner de la complexité temporelle sur les calculs des bonus / pénalités suivantes

	bonus bishop pair /* done */     /* 0 */ /* 50 points */
	penalty rook pair  				 /* 1 */ 
	penalty knight pair 		     /* 2 */ 
	trade down bonus /* ~ done ~ */  /* 3 */ /* */
	penalty no pawn /* ~ done ~ */   /* 4 */ /* -50 points */
	Elephantiasis effect             /* 5 */ /* pas encore pris en compte dans le tableau au dessus car dur à programmer */
	*/

	/******************/
	def bonusBishopPair(player: Int, game: Game) : Boolean = { /* worth 50 points */
		var res = 0
		for(i <- 0 to 7) {
			for (j <- 0 to 7) {
				val piece = game.board(i)(j)
				if ( piece != null && piece.player == player && piece.role == "bishop") {
					res += 1
				}
			}
		}
		return (res == 2)
	}

	def RefreshBonusBishopPair(player: Int, game: Game, move: Move, last: Boolean) : Boolean = {
		if(last == false) {
			return false
		}
		val x = move.to.x
		val y = move.to.y
		if( game.board(x)(y) != null && game.board(x)(y).role == "bishop" && game.board(x)(y).player == player) {
			return false
		}
		return true
	}
	/******************/

	def penaltyRookPair(player: Int, game: Game) : Boolean = {
		return false
	}

	def penaltyKnightPair(player: Int, game: Game): Boolean = {
		return false
	}

	/******************/
	def tradeDownBonus(player: Int, game: Game): Boolean = { /* à décider, à savoir quand est-ce que l'on gagne ? */ /* pour l'instant : one more major piece */
		var res = false
		val n1 = countMajorPiece(player, game)
		val n2 = countMajorPiece(1 - player, game)
		if((n1 - n2) >= 1) {
			return true
		}
		return false
	}

	def refreshTradeDownBonus(player: Int, game: Game, move: (Piece, Pos), last: Boolean) : Boolean = {
		val piece = move._1
		val x = piece.pos.x
		val y = piece.pos.y
		return true
	}
	/******************/
	def penaltyNoPawn(player: Int, game: Game): Boolean = {
		for(i <- 0 to 7) {
			for (j <- 0 to 7) {
				val piece = game.board(i)(j)
				if( piece != null && piece.player == player && piece.role == "pawn") {
					return false
				}
			}
		}
		return true
	}

	/* Chose à faire au dessus */
	/* Fonctions utiles */
	def countMajorPiece(player: Int, game: Game) : Int = {
		var res = 0
		for(i <- 0 to 7) {
			for (j <- 0 to 7) {
				val piece = game.board(i)(j)
				if( piece != null && piece.player == player && piece.role != "pawn" && piece.role != "king" ) {
					res += piece.value/100
				}
			}
		}
		return res
	}

	def countPawn(player: Int, game: Game) : Int = {
		var res = 0
		for(i <- 0 to 7) {
			for (j <- 0 to 7) {
				val piece = game.board(i)(j)
				if( piece != null && piece.player == player && piece.role == "pawn"  ) {
					res += 1
				}
			}
		}
		return res
	}


	def copyGame(game: Game) : Game = {
		val g = new Game
		g.copy_config_of(game)
		return g
	}

	def copyPoints(p : Array[Int]) : Array[Int] = {
		val resPoint = Array.ofDim[Int](2)
		resPoint(0) = points(0)
		resPoint(1) = points(1)
		return resPoint
	}

	def copyBonuses(bonusesP: Array[Array[Boolean]]) : Array[Array[Boolean]] = {
		val resBonuses = Array.ofDim[Boolean](2, 5)
		for(i <- 0 to 1) {
			for(j <- 0 to 4) {
				resBonuses(i)(j) = bonusesP(i)(j)
			}
		}
		return resBonuses
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
						if((game.board(x)(y) != null)) {
							pos_move = (piece, pos) :: pos_move
						}
					}
				}
			}
		}
		return pos_move
	}

	def maximum(list : List[(Piece, Pos, Int)]) : (Piece, Pos) = {
		var maxi = list.head._3
		var piece = list.head._1
		var pos = list.head._2
		for(movebis <- list.tail) {
			if( movebis._3 > maxi) {
				maxi = movebis._3
				piece = movebis._1
				pos = movebis._2
			}
		}
		return (piece, pos)
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



	/* Fonctions essentiels */

	def evaluate(game: Game) : Array[Int] = {
		val points = Array.ofDim[Int](2)
		val bonusBishopPair = Array.ofDim[Int](2)
		val penaltyNoPawn = Array.ofDim[Int](2)
		bonusBishopPair(0) = 0
		bonusBishopPair(1) = 0
		points(0) = 0
		points(1) = 0
		for(i <- 0 to 7) {
			for (j <- 0 to 7) {
				val piece = game.board(i)(j)
				if( piece != null ) {
					val player = piece.player
					points(player) += piece.value
					points(player) += pointPositionning(player)(hash(piece.role))(i)(j)
					if( piece.role == "bishop") {
						bonusBishopPair(player) += 1
					}
					if( piece.role == "pawn") {
						penaltyNoPawn(player) += 1
					}
				}
			}
		}
		for(i <- 0 to 1) {
			if(bonusBishopPair(i) == 2) {
				points(i) += bonusBishopPoints
			}
			if( penaltyNoPawn(i) == 0) {
				points(i) -= penaltyNoPawnPoints
			}
		}
		return points
	}

/*	def refreshPointPlayer(points: Array[Int], bonusesPenalties: Array[Array[Boolean]], player: Int, game: Game, move: (Piece, Pos)) : (Array[Int], Array[Array[Boolean]]) = {
		val resPoints = copyPoints(points)
		val resBonuses = copyBonuses(bonusesPenalties)

		val piece1 = move._1
		val x = piece1.pos.x
		val y = piece1.pos.y

		val i = move._2.x
		val j = move._2.y
		val piece2 = game.board(i)(j)
	/* mise à jour de resBonuses */
		if( piece2 != null ) {
	/*
	bonus bishop pair /* done */     /* 0 */ /* 50 points */
	penalty rook pair  				 /* 1 */ 
	penalty knight pair 		     /* 2 */ 
	trade down bonus /* ~ done ~ */  /* 3 */
	penalty no pawn /* ~ done ~ */   /* 4 */ /* 50 points */
	*/
			if( resBonuses(1-player)(0) && piece2.role == "bishop") { /* bonus bishop pair */
				resBonuses(1-player)(0) = false
				resPoints(1-player) -= 50

			}

			/* trade down bonus */
			resBonuses(player)(3) = refreshTradeDownBonus(player, game, move, resBonuses(player)(3))
			resBonuses(1 - player)(3) = refreshTradeDownBonus(1 - player, game, move, resBonuses(1 - player)(3))

			if( !resBonuses(1-player)(4) && piece2.role == "pawn") { /* penalty no pawn */
				if(countPawn(1-player, game) == 0) {
					resBonuses(1-player)(4) = true
					resPoints(1-player) -= 50
				}
			}

			/* promotion du pion */

			



		}
		if(piece1.role == "pawn" && ( j == 0 | j == 7 ))
				resPoints(player) += 900 - 100
		resPoints(player) += pointPositionning(hash(piece1.role))(i)(j) - pointPositionning(hash(piece1.role))(x)(y)

		return (resPoints, resBonuses)
	}
*/
/*	def quiesce(player: Int, var alpha : Int, var beta : Int, game:Game, bestmovebis: (Piece, Pos)) : (Int, (Piece, Pos)) = {
		val standPat = (points(player) - points(1-player))
		var bestmove : (Piece, Pos) = bestmovebis
		var b = beta
		var a = alpha
		if(standPat >= b) {
			return (b, bestmove)
		}
		if(standPat > a) {
			a = standPat
		}
		var pos_move = everyCaptureMoves(player, game)
		for(movebis <- pos_move) {
			val piece0 = movebis._1
			val pos = movebis._2



			var copygame = copyGame(game)
			val piece = copygame.board(piece0.pos.x)(piece0.pos.y)
			val move = (piece, pos)

/*			copygame.players(0) = new FakePlayer(0, copygame, "queen")
			copygame.players(1) = new FakePlayer(1, copygame, "queen")
*/

			val pointBonuses = refreshPointPlayer(points, bonusesPenalties, player, copygame, move)
			piece.move_to(pos)

			val resPoints = pointBonuses._1
			val resBonuses = pointBonuses._2

			val quiet = quiesce(resPoints, resBonuses, 1-player, -b, -a, copygame, bestmove)
			val score = -quiet._1
			val bMove = quiet._2
			copygame = copyGame(game)

			if( score >= b ) {
				return (b, bestmove)
			}
			if( score > a ) {
				return (score, bMove)
			}
		}
		return (a, bestmove)
	}
*/
	def quiesce(player: Int, alpha : Int, beta : Int, game : Game) : (Int, (Piece, Pos)) = {
		var a = alpha
		var b = beta
		val points = evaluate(game)
		val standPat = points(player)
		var bestmove : (Piece, Pos) = null
		if( standPat >= b ) {
			return (b, bestmove)
		}
		if(alpha < standPat) {
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

			val score = -quiesce(1-player, -b, -a, copygame)._1
			if( score >= b ) {
				return (b, bestmove)
			}
			if ( score > a) {
				a = score
				bestmove = move
			}
		}
		return (a, bestmove)
	}
/*
	def alphaBeta(points: Array[Int], bonusesPenalties: Array[Array[Boolean]], player: Int, alpha : Int, beta : Int, depth: Int, game:Game, bestscorebis: Int = 0, bestmovebis : (Piece, Pos) ) : (Int, (Piece, Pos), Int, Int) = {
		var bestscore = bestscorebis
		var bestmove : (Piece, Pos) = bestmovebis
		var a = alpha
		var b = beta
		if( depth == 0 ) {
			val quiet = quiesce(points, bonusesPenalties, player, a, b, game, bestmove)
			return (quiet._1, quiet._2, a, b)
		}
		var pos_move = everyPossibleMoves(player, game)
		for(movebis <- pos_move) {
			val piece0 = movebis._1
			val pos = movebis._2



			val copygame = copyGame(game)
			val piece = copygame.board(piece0.pos.x)(piece0.pos.y)
			val move = (piece, pos)
/*			copygame.players(0) = new FakePlayer(0, copygame, "queen")
			copygame.players(1) = new FakePlayer(1, copygame, "queen")
*/
			val pointBonuses = refreshPointPlayer(points, bonusesPenalties, player, copygame, move)
			piece.move_to(pos)

			val resPoints = pointBonuses._1
			val resBonuses = pointBonuses._2
			val alphabeta = alphaBeta(resPoints, resBonuses, 1-player, -b, -a, depth -1, copygame, -bestscore, bestmove)
			var score = -alphabeta._1
			var bMove = alphabeta._2
			a = alphabeta._3
			b = alphabeta._4
			bestmove = bMove
			if(score >= b) {
				return (score, bMove, a, b)
			}
			if(score > bestscore) {
				bestscore = score
				bestmove = bMove
				if( score > a ) {
					a = score
				}
			}
		}
		return (bestscore, bestmove, a, b)
	}
*/
	def alphabeta(player : Int, alpha : Int, beta : Int, depth : Int, game : Game, first : Boolean) : (Int, (Piece, Pos)) = {
		var a = alpha
		var b = beta
		var bestscore = -50000
		var bestmove : (Piece, Pos) = null
		if(depth == 0) {
			return quiesce(player, a, b, game)
		}
		val pos_move = everyPossibleMoves(player, game)
		for(move <- pos_move) {
			var piece0 = move._1
			val pos = move._2
			val copygame = copyGame(game)
			val piece = copygame.board(piece0.pos.x)(piece0.pos.y)
			val movebis = (piece, pos)
			val score = - alphabeta(1-player, -b, -a, depth -1, copygame, false)._1
			if(score >= b) {
				return (score, bestmove)
			}
			if(score > bestscore) {
				bestscore = score
				if(first) {
					bestmove = move
				}
				if( score > alpha) {
					a = score
				}
			}

		}
		return (bestscore, bestmove)
	}


}































