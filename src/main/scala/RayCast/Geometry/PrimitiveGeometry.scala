package RayCast.Geometry

import RayCast.Ray.*
import Math.Vec3.*

object PrimitiveGeometry {
  trait GeometryObject
  case class SphereGeometry(radius:Double, position:Vec3) extends GeometryObject

  def Intersect(ray: Ray, sphere: SphereGeometry): Option[Double] = {
    val oc = Sub(sphere.position, ray.origin)
    val a = Dot(ray.direction,ray.direction)
    val b = -2.0 * Dot(ray.direction,oc)
    val c = Dot(oc,oc) - sphere.radius * sphere.radius
    val discriminant = b *b - 4 * a * c
    if (discriminant < 0) {
      None
    } else {
      val sqrtDiscriminant = math.sqrt(discriminant)
      val t1 = (-b - sqrtDiscriminant) / (2.0 * a)
      val t2 = (-b + sqrtDiscriminant) / (2.0 * a)

      if (t1 > 0 && t2 > 0) Some(math.min(t1, t2))
      else if (t1 > 0) Some(t1)
      else if (t2 > 0) Some(t2)
      else None
    }
  }


  def GetNormal(position:Vec3, geom: GeometryObject) = {
    geom match
      case SphereGeometry(rad,cent) => Normalize(Sub(position,cent))
  }
}


