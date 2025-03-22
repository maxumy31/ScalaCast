package RayCast
import Math.Vec3.*
import RayCast.Light.Light.*
import RayCast.Ray.*
import RayCast.Color.*
import RayCast.Geometry.PrimitiveGeometry.GetNormal
import RayCast.Primitives.Primitives.*

object Scene {
  case class Scene(
                  objects: Seq[MonocolorSphere],
                  lights : Seq[LightSource]
                  )
  def IntersectClosestObject(ray:Ray,scene:Scene):Option[(MonocolorSphere,Double)] = {
      scene.objects.flatMap { obj =>
        Intersect(ray, obj) match {
          case Some(t) =>
            Some(obj,t)
          case None => None
        }}.minByOption({ case (_, t) => t })
  }
  
  def ComputeLightForIntersection(ray:Ray,t:Double,normal:Vec3,scene:Scene) : Double = {
    scene.lights.map(l => {
      
      ComputeLightning(RayAt(ray,t),normal,l)
    }).sum
  }


}
