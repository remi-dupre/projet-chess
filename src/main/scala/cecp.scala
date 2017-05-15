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

	var already_played = false // Si l'ia n'a pas encore joué, elle a besoins de récupérer la position du jeu

	def plays = game.playing == color

	def apply_cecp_instruction(msg : String) : Unit = {
		if(!plays) {
			/* Parfois gnuchess s'emballe et part sur un tour supplémentaire
			 * Quand c'est le cas on se contente d'annuler ce coup sur la partie correspondante
			 */
			//println("""/!\ Joue en dehors de son tour : annulation""")
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
				if(piece == null) {
					//println("""/!\ Le cecp semble avoir beguaillé""")
					return
				}

				val success = game.move(piece, dest)
				//println("(j" + color +  ") Moving " + piece.role + " to " + dest + " : " + success)
			}
			case forfait => println("""/!\ GNU Chess a abandonné""")
		}
	}
	engine.listeners = (apply_cecp_instruction(_ : String)) :: engine.listeners


	def move_txt(move : Move) : String = {
		var ret = "" + rows(move.from.x) + cols(move.from.y) + rows(move.to.x) + cols(move.to.y)
		ret += (move.promote_to match {
			case null => ""
			case "queen" => "q"
			case "rook" => "r"
			case "bishop" => "b"
			case "knight" => "n"
		})
		return ret
	}

	override def wait_play = {
		// send time information to the CECP engine
		if(game.timers != null) {
			val timer = game.timers(game.playing)
			val moves = timer.period_moves + game.turn
			val secs = timer.free_time.toFloat
			engine.send("time " + secs*100) // Une gestion du temps assez grossière (alloué pour la partie entière)
		}

		// loads the game
		if(!already_played && game.save_root != null) {
			for(move <- game.save_root.move_list) {
				engine.send(move_txt(move))
			}
			Thread.sleep(1000)
		}
		// trys to apply last move
		else if(game.save_root != null) {
			val move = game.save_root.move_list.last
			engine.send(move_txt(move))
		}

		// waits for next move
		already_played = true
		engine.send("go")
	}


	override def get_promotion_type : String =  {
		"queen"
	}
}
