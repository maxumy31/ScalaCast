package RayCast.Light

import Math.Vec3.*

object Light {
  trait LightSource
  case class AmbientLight(intensity:Double) extends LightSource
  case class PointLight(intensity:Double,position:Vec3) extends LightSource
  case class DirectionLight(intensity:Double,direction:Vec3) extends LightSource

  def ComputeLightning(point:Vec3,normal:Vec3, light: LightSource) : Double = {
    light match
      case AmbientLight(intensity) => intensity
      case PointLight(intensity,position) =>
        val lightDirection = Sub(position,point)
        val DotNL = Dot(normal,lightDirection)
        if DotNL > 0
        then intensity * DotNL / (Length(normal)*Length(lightDirection)) else 0
      case DirectionLight(intensity,direction) =>
        val lightDirection = direction
        val DotNL = Dot(normal, lightDirection)
        if DotNL > 0
        then intensity * DotNL / (Length(normal) * Length(lightDirection)) else 0
  }
  
  def LightDirection(position:Vec3,light:LightSource): Option[Vec3] = {
    light match
      case AmbientLight(intensity) => None
      case PointLight(intensity,point) => Some(Sub(point, position))
      case DirectionLight(intensity,dir) => Some(dir)
  }
}
