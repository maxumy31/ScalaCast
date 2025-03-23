package RayCast.Primitives

import RayCast.Color.Color
import RayCast.Geometry.PrimitiveGeometry
import RayCast.Ray.*
import RayCast.Geometry.PrimitiveGeometry.*
import RayCast.Primitives.Material.Material

object Primitives {
  trait Primitive
  case class MonocolorSphere(material: Material,geometry:SphereGeometry) extends Primitive

  def Intersect(ray:Ray,sphere: MonocolorSphere):Option[Double] = {
    PrimitiveGeometry.Intersect(ray,sphere.geometry) match
      case Some(t) => Some(t)
      case None => None
  }
}
