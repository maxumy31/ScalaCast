package RayCast

import Math.Vec3.*

object Viewport {
  def CreateViewport(cameraPosition : Vec3, focalDistance: Double, width:Int, height:Int, viewportWidth : Double, viewportHeight : Double) : Seq[Vec3] = {
    val aspectRatio = height.toDouble / width
    val viewportHeight = viewportWidth * aspectRatio
    val pixelSize = viewportWidth / width

    val l = (0 until height).flatMap(y => {
      (0 until width).map(x => {
        val newX = (x-width/2.0) * pixelSize
        val newY = (height/2.0-y) * pixelSize
        Vec3(newX,newY,focalDistance)
      })
    })
    l
  }
}
