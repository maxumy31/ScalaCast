package RayCast.Scene

import Math.Vec3.Vec3

object Camera {
  //Смотрит всегда на z+, TODO: передалть чтобы не всегда туда смотрела
  case class Camera(position:Vec3,fov:Double,aspectRatio:Double,focalLength:Double)
}
