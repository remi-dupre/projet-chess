import java.io._
import scala.io._

/* TODO
 *  - utiliser des regex
 *  - gérer les "resign"
 */

class GnuChess extends CECP_engine {
	val rows = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
	val cols = Array('8', '7', '6', '5', '4', '3', '2', '1')

	val command = "gnuchess --xboard"	// Command to start the xboard engine

	/* The process runing the engine */
	val proc = Runtime.getRuntime.exec(command)
	val out = new PrintWriter(proc.getOutputStream)


	val reader = new Thread("stderr reader for : " + command) {
		override def run() {
			for(line <- Source.fromInputStream(proc.getInputStream).getLines) {
				println("#" + line)
				val msg = line.split(" ").last
				if(msg.length == 4) {
					val f_x = rows.indexOf( msg(0) )
					val f_y = cols.indexOf( msg(1) )
					val t_x = rows.indexOf( msg(2) )
					val t_y = cols.indexOf( msg(3) )

					if(line.substring(0, 7) == "My move") {
						if(f_x >= 0 && f_y >= 0 && t_x >= 0 && t_y >= 0) {
							// On suppose que ça suffit à caractériser un move
							listeners.foreach((f) => f(msg))
						}
					}
				}
			}
		}
	}.start()

	def send(msg : String) : Unit = {
		out.println(msg)
		println("(sent) ---> " + msg)
		out.flush()
	}
}
