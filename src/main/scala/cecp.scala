import scala.util.matching

abstract class CECP_engine {
	// Une liste de fonctions qui seront appelées lorsqu'un message est émis par le moteur
	var listeners : List[String => Unit] = List()

	// Permet d'envoyer un message au moteur CECP
	def send(msg : String) : Unit
}

object CECP_msg {
	val rows = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
	val cols = Array('8', '7', '6', '5', '4', '3', '2', '1')

	val gc_move = """My move is : ([a-h][1-8][a-h][1-8])""".r
	val move = """([a-h][1-8][a-h][1-8])""".r
}

class CECP_player(engine : CECP_engine, color : Int, game : Game) extends Player(color, game) {
	val rows = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
	val cols = Array('8', '7', '6', '5', '4', '3', '2', '1')

	var plays = false

	def apply_cecp_instruction(msg : String) : Unit = {
		if(plays) {
			msg match {
				case CECP_msg.move(desc) => {
					val f_x = CECP_msg.rows.indexOf( desc(0) )
					val f_y = CECP_msg.cols.indexOf( desc(1) )
					val t_x = CECP_msg.rows.indexOf( desc(2) )
					val t_y = CECP_msg.cols.indexOf( desc(3) )

					val piece = game.board(f_x)(f_y)
					val dest = Pos(t_x, t_y)

					println("Moving " + piece.role + " to " + dest)
					game.move(piece, dest)
					plays = false
				}
			}
		}
	}
	engine.listeners = (apply_cecp_instruction(_ : String)) :: engine.listeners

	override def wait_play = {
		// send time information to the CECP engine
		if(game.timers != null) {
			val timer = game.timers(game.playing)
			val moves = timer.period_moves + game.turn
			val secs = timer.free_time.toFloat
			engine.send("time " + secs*100)
			//engine.send("level " + moves + " " + minutes + " 1")

			val otimer = game.timers(1-game.playing)
			val omoves = otimer.period_moves + game.turn
			val osecs = otimer.free_time.toFloat
			engine.send("otim " + osecs*100)
		}

		// trys to apply last move
		if(game.save_root != null) {
			val move = game.save_root.move_list.last
			engine.send( "" + rows(move.from.x) + cols(move.from.y) + rows(move.to.x) + cols(move.to.y) )
		}

		// waits for next move
		plays = true
		engine.send("go")
	}


	override def get_promotion_type : String =  {
		"queen"
	}
}
