package RayCast

import RayCast.Vec3.*
object Ray {
  case class Ray(origin:Vec3,direction:Vec3)

  def RayAt(ray: Ray, x:Double) : Vec3 = {
    add(mult(ray.direction,x),ray.origin)
  }
}
