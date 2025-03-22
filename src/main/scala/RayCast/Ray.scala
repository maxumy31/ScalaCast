package RayCast

import Math.Vec3.*
object Ray {
  case class Ray(origin:Vec3,direction:Vec3)

  def RayAt(ray: Ray, x:Double) : Vec3 = {
    Add(Mult(ray.direction,x),ray.origin)
  }
}
