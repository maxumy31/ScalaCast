package RayCast.Scene

import Math.Vec3.*
import RayCast.Color.*
import RayCast.Geometry.PrimitiveGeometry.{GeometryObject, GetNormal}
import RayCast.Light.Light.*
import RayCast.Primitives.Material.Material
import RayCast.Primitives.SceneObjects.*
import RayCast.Ray.*

object Scene {
  case class Scene(objects: Seq[SceneObject], lights : Seq[LightSource])

  def IntersectClosestObject(ray:Ray,scene:Scene):Option[(SceneObject,Double)] = {
      scene.objects.flatMap { obj =>
        IntersectObject(ray, obj) match {
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

  def ComputeReflection(ray:Ray, t:Double, obj:SceneObject, maxDepth:Int, atteniation:Double, scene:Scene) : Option[Color] = {
    def refl(geometry: GeometryObject,material:Material) = {
      val normal = GetNormal(RayAt(ray, t), geometry)
      val reflDir = ReflectRay(ray, normal)
      val newRay = Ray(Add(RayAt(ray, t), Mult(normal, 0.01)), reflDir)
      IntersectClosestObject(newRay, scene) match {
        case None => None
        case Some(sphere, newT) =>
          val reflectedColor = material.albedo
          ComputeReflection(newRay, newT, sphere, maxDepth - 1, atteniation * material.reflective, scene) match
            case Some(col) =>
              val light = ComputeLightForIntersection(newRay, newT, GetNormal(RayAt(newRay, newT), geometry), scene)
              val newColor = (Mix(reflectedColor, col, material.reflective))
              Some(newColor)
            case None =>
              Some(reflectedColor)
      }
    }

    if maxDepth <= 0  || atteniation <= 0.01 then None else {
      obj match
        case MonochromeSphere(material,geometry) => refl(geometry,material)
        case MonochromeTriangle(mat,geo) => refl(geo,mat)
    }
  }


}
