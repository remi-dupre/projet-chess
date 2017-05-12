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

	val error_invalid = "Invalid move:.*".r

	val forfait = "resign.*".r
	val gc_move = "My move is : ([a-h][1-8][a-h][1-8])q?".r // Le ".?" permet de capturer une éventuelle instruction de promotion, toujours en dame
	val move = "([a-h][1-8][a-h][1-8])".r
}

class CECP_player(engine : CECP_engine, color : Int, game : Game) extends Player(color, game) {
	val rows = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
	val cols = Array('8', '7', '6', '5', '4', '3', '2', '1')

	def plays = game.playing == color

	def apply_cecp_instruction(msg : String) : Unit = {
		if(!plays) {
			/* Parfois gnuchess s'emballe et part sur un tour supplémentaire
			 * Quand c'est le cas on se contente d'annuler ce coup sur la partie correspondante
			 */
			println("""/!\ Joue en dehors de son tour : annulation""")
			engine.send("undo")
			return
		}

		msg match {
			case CECP_msg.move(desc) => {
				val f_x = CECP_msg.rows.indexOf( desc(0) )
				val f_y = CECP_msg.cols.indexOf( desc(1) )
				val t_x = CECP_msg.rows.indexOf( desc(2) )
				val t_y = CECP_msg.cols.indexOf( desc(3) )

				val piece = game.board(f_x)(f_y)
				val dest = Pos(t_x, t_y)
if(piece == null)
	{println("pouet"+ (f_x,f_y))
	println(dest)
	return}
				val success = game.move(piece, dest)
				println("(j" + color +  ") Moving " + piece.role + " to " + dest + " : " + success)
			}
			case forfait => println("""/!\ GNU Chess a abandonné""")
		}
	}
	engine.listeners = (apply_cecp_instruction(_ : String)) :: engine.listeners

	override def wait_play = {
		// send time information to the CECP engine
		if(game.timers != null) {
			val timer = game.timers(game.playing)
			val moves = timer.period_moves + game.turn
			val secs = timer.free_time.toFloat
			engine.send("time " + secs*100) // Une gestion du temps assez grossière (alloué pour la partie entière)
		}

		// trys to apply last move
		if(game.save_root != null) {
			val move = game.save_root.move_list.last
			engine.send( "" + rows(move.from.x) + cols(move.from.y) + rows(move.to.x) + cols(move.to.y) )
		}

		// waits for next move
		engine.send("go")
	}


	override def get_promotion_type : String =  {
		println("MAIS PUTAIN IL PASSE BIEN PAR ICI MERDE --'")
		"queen"
	}
}
