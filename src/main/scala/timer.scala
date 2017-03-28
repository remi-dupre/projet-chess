import swing._


case class Period(var duration : Long, var moves : Int)

class Cadency(a_periods : List[Period]) {
	var repeat_last = true
	var periods = a_periods

	/** Le nombre de moves sur la période actuelle */
	def period_moves : Int = {
		if(periods.isEmpty) {
			return 0
		}
		else {
			return periods.head.moves
		}
	}

	/** Donne le temps qu'on peut passer à faire un tour dans l'état actuel */
	def free_time : Long = {
		if(periods.isEmpty) {
			return -1
		}
		else {
			return periods.head.duration
		}
	}

	/** Indique que du temps a été passé, met à jour les périodes en conséquence */
	def spend(time : Long) = {
		if(time > free_time) { // On a dépassé le temps apparti
			periods = List()
		}
		else if(periods.head.moves == 1) { // On a joué tous les coups de la période
			periods = periods.tail
			if( repeat_last && periods.tail.isEmpty ) {
				periods = periods.head :: periods
			}
		}
		else {
			periods.head.moves -= 1
			periods.head.duration -= time
		}
	}

	/** Copy a Cadency */
	def copy : Cadency = {
		return new Cadency(
			periods.map(
				(p : Period) => new Period(p.duration, p.moves)
			)
		)
	}
}

class TimeCounter(game : Game) extends Label {
	val counter_label = this

	val thread = new Thread(new Runnable() {
	 	def run() {
			while(!game.over) {
				val turn_duration = tools.timestamp - game.turn_start
				if(game.timers == null) {
					counter_label.text = turn_duration + "s"
				}
				else {
					val cad = game.timers(game.playing)
					if( cad.free_time < turn_duration ) {
						counter_label.text = "Dépasse de " + (turn_duration - cad.free_time) + "s"
					}
					else {
						counter_label.text = (cad.free_time - turn_duration) + "s pour " + cad.period_moves + " coups"
					}
				}
				Thread.sleep(50)
			}
		}
	});
	thread.start()
}
