package RayCast

import Math.Vec3.*
object Ray {
  case class Ray(origin:Vec3,direction:Vec3)

  def RayAt(ray: Ray, x:Double) : Vec3 = {
    Add(Mult(ray.direction,x),ray.origin)
  }

  def ReflectRay(ray: Ray, normal: Vec3) = {
    val I = Normalize(ray.direction)
    val N = Normalize(normal)
    val reflection = Sub(I,Mult(N,2 * Dot(N,I)))
    reflection
  }
}
