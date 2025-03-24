package RayCast.Geometry

import RayCast.Ray.*
import Math.Vec3.*

object PrimitiveGeometry {
  trait GeometryObject
  case class SphereGeometry(radius:Double, position:Vec3) extends GeometryObject
  case class TriangleGeometry(a:Vec3, b: Vec3, c : Vec3) extends GeometryObject

  def IntersectSphere(ray: Ray, sphere: SphereGeometry): Option[Double] = {
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

  def IntersectTriangle(ray:Ray,triangle:TriangleGeometry) : Option[Double] = {
    val edge1 = Sub(triangle.b,triangle.a)
    val edge2 = Sub(triangle.c,triangle.a)
    val h = Cross(ray.direction,edge2)
    val a = Dot(edge1,h)

    if (math.abs(a) < 1e-8) return None

    val f = 1.0 / a
    val s = Sub(ray.origin,triangle.a)
    val u = f * Dot(s,h)
    if (u < 0 || u > 1) return None

    val q = Cross(s,edge1)
    val v = f * Dot(ray.direction,q)
    if (v < 0 || u + v > 1) return None

    val t = f * Dot(edge2,q)
    if (t > 1e-8) Some(t) else None
  }

  def Intersect(ray:Ray, geometryObject: GeometryObject) : Option[Double] = {
    geometryObject match
      case SphereGeometry(rad,pos) => IntersectSphere(ray,SphereGeometry(rad,pos))
      case TriangleGeometry(a,b,c) => IntersectTriangle(ray,TriangleGeometry(a,b,c))
      case _ => println("Unknown intersection geometry.");None
  }


  def GetNormal(position:Vec3, geom: GeometryObject) = {
    geom match
      case SphereGeometry(rad,cent) => Normalize(Sub(position,cent))
      case TriangleGeometry(a,b,c) => Normalize(Cross(Sub(b,a),Sub(c,a)))
  }
}


