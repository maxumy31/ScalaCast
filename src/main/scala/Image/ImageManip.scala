package Image


import java.awt.image.BufferedImage

object ImageManip {
  def CreateEmptyImage(width:Int,height:Int) : BufferedImage = {
    BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
  }

  def FillImage(img: BufferedImage, pixels: Seq[Int]): BufferedImage = {
    var newImage = CreateEmptyImage(img.getWidth, img.getHeight)
    newImage.setRGB(0, 0, img.getWidth, img.getHeight, pixels.toArray, 0, img.getWidth)
    newImage
  }
}
