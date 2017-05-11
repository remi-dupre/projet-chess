abstract class CECP_engine {
	// Une liste de fonctions qui seront appelées lorsqu'un message est émis par le moteur
	var listeners : List[String => Unit] = List()

	// Permet d'envoyer un message au moteur CECP
	def send(msg : String) : Unit
}

class CECP_player(engine : CECP_engine, color : Int, game : Game) extends Player(color, game) {
	val rows = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
	val cols = Array('8', '7', '6', '5', '4', '3', '2', '1')

	var plays = false

	def apply_cecp_instruction(msg : String) : Unit = {
		if(msg.length == 4 && plays) {
			val f_x = rows.indexOf( msg(0) )
			val f_y = cols.indexOf( msg(1) )
			val t_x = rows.indexOf( msg(2) )
			val t_y = cols.indexOf( msg(3) )

			val piece = game.board(f_x)(f_y)
			val dest = Pos(t_x, t_y)

			println("Moving " + piece.role + " to " + dest)
			game.move(piece, dest)
			plays = false
		}
	}
	engine.listeners = (apply_cecp_instruction(_ : String)) :: engine.listeners

	override def wait_play = {
		// trys to apply last move
		if(game.save_root != null) {
			val move = game.save_root.move_list.last
			println("---> " + rows(move.from.x) + cols(move.from.y) + rows(move.to.x) + cols(move.to.y) )
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
