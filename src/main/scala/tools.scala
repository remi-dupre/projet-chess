import javax.swing.ImageIcon
/*import java.time.Instant.now*/

object tools {
	/** http://stackoverflow.com/questions/6714045/how-to-resize-jlabel-imageicon */
	def icon_resized(file : String, width : Integer, height : Integer) : ImageIcon = {
		val imageIcon = new ImageIcon(file); // load the image to a imageIcon
		val image = imageIcon.getImage(); // transform it 
		val newimg = image.getScaledInstance(width, height,  java.awt.Image.SCALE_SMOOTH); // scale it the smooth way  
		return new ImageIcon(newimg);  // transform it back
	}

	def timestamp : Long = {
		return 0
		/*return now.getEpochSecond*/
	}
}
