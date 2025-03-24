package RayCast.Scene

import Math.Vec3.*
import RayCast.Scene.Camera.Camera

object Viewport {
  /*def CreateViewport(cameraPosition : Vec3, focalDistance: Double, width:Int, height:Int, viewportWidth : Double, viewportHeight : Double) : Seq[Vec3] = {
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
  }*/

  def CreateViewportFromCamera(cam: Camera, pointsPerWidth: Int, pointsPerHeight: Int): Seq[Vec3] = {
    val viewportHeight = 2 * math.tan(math.toRadians(cam.fov / 2)) * cam.focalLength
    val viewportWidth = viewportHeight * cam.aspectRatio

    val pixelWidth = viewportWidth / (pointsPerWidth)
    val pixelHeight = viewportHeight / (pointsPerHeight)

    val camForward = Vec3(0, 0, 1)

    (0 until pointsPerHeight).flatMap { y =>
      (0 until pointsPerWidth).map { x =>
        val u = (x.toDouble / (pointsPerWidth)) - 0.5
        val v = 0.5 - (y.toDouble / (pointsPerHeight))

        val xPos = u * viewportWidth
        val yPos = v * viewportHeight
        val zPos = cam.focalLength

        Vec3(xPos + cam.position.x,yPos + cam.position.y,zPos + cam.position.z)
      }
    }
  }
}
