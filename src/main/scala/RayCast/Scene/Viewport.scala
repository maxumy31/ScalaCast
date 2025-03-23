package RayCast.Scene

import Math.Vec3.*
import RayCast.Scene.Camera.Camera

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

  def CreateViewportFromCamera(cam : Camera) : Seq[Vec3] = {
    val viewportHeight = 2 * math.tan(math.toRadians(cam.fov / 2)) * cam.focalLength
    val viewportWidth = viewportHeight * cam.aspectRatio
    //Смотрим всегда на z+, это не очень хорошо ну да ладно
    val camDir = Vec3(0,0,1)
    
  }
}
