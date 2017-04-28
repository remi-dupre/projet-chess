import java.io._
import scala.io._




class GnuChess(color : Int, game : Game, speed : Int = 0) extends Player(color, game) {
	val rows = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
	val cols = Array('8', '7', '6', '5', '4', '3', '2', '1')

	val engine = "gnuchess --easy --xboard"	// Command to start the xboard engine

	/* The process runing the engine */
	val proc = Runtime.getRuntime.exec(engine)
	val out = new PrintWriter(proc.getOutputStream)

	var plays = false


	val reader = new Thread("stderr reader for : " + engine) {
		override def run() {
			for(line <- Source.fromInputStream(proc.getInputStream).getLines) {
				println("#" + line + "")
				if(plays) {
					val inst = line.split(" ").last
					if(inst.length == 4) {
						val f_x = rows.indexOf( inst(0) )
						val f_y = cols.indexOf( inst(1) )
						val t_x = rows.indexOf( inst(2) )
						val t_y = cols.indexOf( inst(3) )

						if(line.substring(0, 7) == "My move") {
							if(f_x >= 0 && f_y >= 0 && t_x >= 0 && t_y >= 0) {
								println("Effect of '" + line + "' :")
								val piece = game.board(f_x)(f_y)
								val dest = Pos(t_x, t_y)
								game.move(piece, dest)
								println("Moving " + piece.role + " to " + dest)
								plays = false
							}
						}
					}
				}
			}
		}
	}.start()


	override def wait_play = {
		// trys to apply last move
		if(game.save_root != null) {
			val move = game.save_root.move_list.last
			println("---> " + rows(move.from.x) + cols(move.from.y) + rows(move.to.x) + cols(move.to.y) )
			out.println( "" + rows(move.from.x) + cols(move.from.y) + rows(move.to.x) + cols(move.to.y) )
		}
		// waits for next move
		plays = true
		out.println("go")
		out.flush()
		//out.close()
	}


	override def get_promotion_type : String =  {
		"queen"
	}

}





/*class Linker() {
	val engine = "gnuchess --xboard"	// Command to start the xboard engine

	/* The process runing the engine */
	val proc = Runtime.getRuntime.exec(engine)

	val reader = new Thread("stderr reader for : " + engine) {
	  override def run() {
	  	while(true) {
			for(line <- Source.fromInputStream(proc.getInputStream).getLines)
			  println(line)
		  	}
	  	}
	}.start()

	def send(msg : String) = {
		new Thread("stdin writer for : " + engine) {
			override def run() {
				val out = new PrintWriter(proc.getOutputStream)
				out.println(msg)
				out.close()
			}
		}.start()
	}
}*/
