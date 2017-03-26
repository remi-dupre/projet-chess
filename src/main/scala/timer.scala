case class Period(var duration : Int, var moves : Int)

class Cadency(a_periods : List[Period]) {
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
	def free_time : Int = {
		if(periods.isEmpty) {
			return -1
		}
		else {
			return periods.head.duration
		}
	}

	/** Indique que du temps a été passé, met à jour les périodes en conséquence */
	def spend(time : Int) = {
		if(time > free_time) { // On a dépassé le temps apparti
			periods = List()
		}
		else if(periods.head.moves == 1) { // On a joué tous les coups de la période
			periods = periods.tail
		}
		else {
			periods.head.moves -= 1
			periods.head.duration -= time
		}
	}
}
