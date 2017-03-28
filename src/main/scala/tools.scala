import javax.swing.ImageIcon
import java.sql.Timestamp

object Tools {
	/** http://stackoverflow.com/questions/6714045/how-to-resize-jlabel-imageicon */
	def icon_resized(file : String, width : Integer, height : Integer) : ImageIcon = {
		val imageIcon = new ImageIcon(file); // load the image to a imageIcon
		val image = imageIcon.getImage(); // transform it 
		val newimg = image.getScaledInstance(width, height,  java.awt.Image.SCALE_SMOOTH); // scale it the smooth way  
		return new ImageIcon(newimg);  // transform it back
	}

	/** Donne le timestamp en secondes */
	def timestamp : Long = {
		val stamp = new Timestamp(System.currentTimeMillis())
		return stamp.getTime / 1000
	}

	/** Formate le temps sous la form %h %min %s */
	def time_to_string(time : Long) : String = {
		val sec = time % 60
		val min = ((time - 60) / 60) % 60
		val h = (time - 60*min - sec) / (60 * 60)

		if(h > 0)
			return h + "h " + min + "min " + sec + "s"
		else if(min > 0)
			return min + "min " + sec + "s"
		else(sec > 0)
			return sec + "s "
	}
}
