package RayCast.Scene

import Math.Vec3.*
import RayCast.Color.*
import RayCast.Geometry.PrimitiveGeometry.GetNormal
import RayCast.Light.Light.*
import RayCast.Primitives.Primitives.*
import RayCast.Ray.*

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
      LightDirection(RayAt(ray,t),l) match
        case None => ComputeLightning(RayAt(ray,t),normal,l)
        case Some(dir) =>
          //Если есть направление света
          IntersectClosestObject(Ray(Add(RayAt(ray,t),Mult(dir,0.01)),dir),scene) match
            case None => ComputeLightning(RayAt(ray,t),normal,l)
            //Если свет загорожен другим объектом
            case _ => 0
    }).sum
  }

  def ComputeReflection(ray:Ray,t:Double,obj:MonocolorSphere,maxDepth:Int,atteniation:Double,scene:Scene) : Option[Color] = {
    if maxDepth <= 0  || atteniation <= 0.01 then None else {
      val normal = GetNormal(RayAt(ray, t), obj.geometry)
      val reflDir = ReflectRay(ray, normal)
      val newRay = Ray(Add(RayAt(ray, t),Mult(normal,0.01)), reflDir)
      IntersectClosestObject(newRay, scene) match {
        case None => None
        case Some(sphere, newT) =>
          val reflectedColor = sphere.material.albedo
          ComputeReflection(newRay, newT, sphere, maxDepth - 1, atteniation * obj.material.reflective,scene) match
            case Some(col) =>
              val light = ComputeLightForIntersection(newRay,newT,GetNormal(RayAt(newRay,newT),sphere.geometry),scene)
              val newColor = (Mix(reflectedColor, col,obj.material.reflective))
              Some(newColor)
            case None =>
              Some(reflectedColor)
      }
    }
  }


}
