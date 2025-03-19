package Image

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object SaveImage {
  import ImageManip.*
  val outputFile = "output.png"
  val outputFormatFile = "png"
  def SaveImage(img : BufferedImage) : Unit = {
    ImageIO.write(img, outputFormatFile, new File(outputFile))
  }
}
