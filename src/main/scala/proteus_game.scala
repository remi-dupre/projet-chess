class ProtGame() extends Game() {
	var points : Array[Int] = Array(0, 0)
	var first_phase : Boolean = true

	override def init = {
		for(i <- 0 to 3) {
			board(2*i)(7) = new Dice(this, 0, Pos(2*i, 7))
			board(2*i+1)(6) = new Dice(this, 0, Pos(2*i+1, 6))
			board(2*i)(1) = new Dice(this, 1, Pos(2*i, 1))
			board(2*i+1)(0) = new Dice(this, 1, Pos(2*i+1, 0))
		}
		points = Array(0, 0)
		first_phase = true
		turn_start = Tools.timestamp
	}

	def compteur_proteus(player:Int): Int = {
		var n = 0
		for(i <- 0 to 7) {
			for(j <- 0 to 7) {
				if(board(i)(j) != null && board(i)(j).player == player) {
					n = n + 1
				}
			}
		}
		return n
	}

	override def move(p:Piece, pos:Pos): Boolean = {
		val possibleMoves : List[Pos] = p.possible_move()
		if(possibleMoves.contains(pos)) {
			if(save_root == null) { // Un nouvel arbre de sauvegardes est nécessaire
				save_root = Save(Move(p.pos, pos), List(), null)
				save_current = save_root
			}
			else {
				val new_save = Save(Move(p.pos, pos), List(), save_current)
				save_current.saveList = new_save :: save_current.saveList
				save_current = new_save
			}

			if(save_root == null) { // Un nouvel arbre de sauvegardes est nécessaire
				save_root = Save(Move(p.pos, pos), List(), null)
				save_current = save_root
			}
			else {
				val new_save = Save(Move(p.pos, pos), List(), null)
				save_current.saveList = new_save :: save_current.saveList
				save_current = new_save
			}

			if(board(pos.x)(pos.y) != null) {
				points(playing) += board(pos.x)(pos.y).asInstanceOf[Dice].points
			}

			p.move_to(pos)
			first_phase = false

			// Prise des dames par derrière
			val dir = 1 - (2*playing)
			if(in_board(pos.x, pos.y+dir) && board(pos.x)(pos.y+dir) != null
			  && board(pos.x)(pos.y+dir).asInstanceOf[Dice].i_role == 5) { ////D/QSODQSDHQS
				board(pos.x)(pos.y+dir) = null
			}

			changed()

			if(players(playing) != null && !over) {
				players(playing).wait_roll(pos)
			}
			return true
		}
		return false
	}

	def roll(pos : Pos, up : Boolean = true) : Boolean = {
		if(timers != null) {
			timers(playing).spend(Tools.timestamp-turn_start)
		}
		turn_start = Tools.timestamp

		board(pos.x)(pos.y).asInstanceOf[Dice].roll(up)
		playing = 1 - playing
		turn = 1 + turn
		first_phase = true
		changed()

		if(!over) {
			players(playing).wait_play
		}

		turn = turn + 1
		return true
	}

	override def pat = {
		over && points(0) == points(1)
	}

	override def over = {
		first_phase && (every_possible_move(1-playing).isEmpty || every_possible_move(playing).isEmpty || compteur_proteus(playing) == 1)
	}

	override def winner : Int = {
		if(points(0) > points(1))
			return 0
		else if(points(1) > points(0))
			return 1
		else
			return -1
	}
}
