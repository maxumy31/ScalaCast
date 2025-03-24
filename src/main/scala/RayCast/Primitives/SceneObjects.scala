package RayCast.Primitives

import RayCast.Color.Color
import RayCast.Geometry.PrimitiveGeometry
import RayCast.Ray.*
import RayCast.Geometry.PrimitiveGeometry.*
import RayCast.Primitives.Material.Material

object SceneObjects {
  trait SceneObject
  case class MonochromeSphere(material: Material, geometry:SphereGeometry) extends SceneObject
  case class MonochromeTriangle(material:Material,geometry: TriangleGeometry) extends SceneObject

  def IntersectMonochromeSphere(ray:Ray, sphere: MonochromeSphere):Option[Double] = {
    PrimitiveGeometry.IntersectSphere(ray,sphere.geometry) match
      case Some(t) => Some(t)
      case None => None
  }

  def IntersectObject(ray:Ray,obj:SceneObject) : Option[Double] = {
    obj match
      case MonochromeSphere(material, geometry) => Intersect(ray,geometry)
      case MonochromeTriangle(mat,geo) => Intersect(ray,geo)
      case _ => println("Unknown primitive for intersection."); None
  }
  
  def GetMaterial(prim : SceneObject) : Material = {
    prim match
      case MonochromeSphere(mat,_) => mat
      case MonochromeTriangle(mat,_) => mat 
  }

  def GetGeometry(prim: SceneObject): GeometryObject = {
    prim match
      case MonochromeSphere(_, geom) => geom
      case MonochromeTriangle(_,geom) => geom
  }
}
