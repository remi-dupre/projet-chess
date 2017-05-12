import java.io._
import scala.io._

/* TODO
 *  - gérer les "resign"
 */

class GnuChess extends CECP_engine {
	val rows = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
	val cols = Array('8', '7', '6', '5', '4', '3', '2', '1')

	val command = "gnuchess --xboard"	// Command to start the gnuchess engine

	/* The process runing the engine */
	val proc = Runtime.getRuntime.exec(command)
	val out = new PrintWriter(proc.getOutputStream)


	val reader = new Thread("stderr reader for : " + command) {
		override def run() {
			for(line <- Source.fromInputStream(proc.getInputStream).getLines) {
				println("#" + line)
				line match {
					case CECP_msg.gc_move(desc) => listeners.foreach((f) => f(desc))
					case CECP_msg.forfait() => listeners.foreach((f) => f(line))
					case CECP_msg.error_invalid() => println("GNU Chess a interprété un message comme invalide")
					case _ => ()
				}
			}
		}
	}.start()

	def send(msg : String) : Unit = {
		out.println(msg)
		//println("(sent) ---> " + msg)
		out.flush()
	}
}
