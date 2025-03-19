package RayCast

import RayCast.Ray.*
import RayCast.Vec3.*

object Primitives {
  trait Primitive
  case class Sphere(radius:Double,position:Vec3)

  def Intersect(ray: Ray, sphere: Sphere): Option[Double] = {
    val oc = sub(ray.origin, sphere.position) // Вектор от центра сферы до начала луча
    val a = length(ray.direction) // Квадрат длины направления луча
    val b = mult(dot(oc, ray.direction),2) // Скалярное произведение oc и направления луча
    val c = length(oc) - (sphere.radius * sphere.radius) // Квадрат длины oc минус квадрат радиуса
    val discriminant = length(b) * length(b) - 4 * a * c// Дискриминант квадратного уравнения
    if (discriminant < 0) {
      None // Нет пересечения
    } else {

      val sqrtDiscriminant = math.sqrt(discriminant)
      val t1 = (-length(b) - sqrtDiscriminant) / (2.0 * a) // Первое решение
      val t2 = (-length(b) + sqrtDiscriminant) / (2.0 * a) // Второе решение
      //println((t1, t2))
      Some(math.min(t1, t2))
      // Возвращаем минимальное положительное значение t
      /*if (t1 > 0 && t2 > 0) Some(math.min(t1, t2))
      else if (t1 > 0) Some(t1)
      else if (t2 > 0) Some(t2)
      else None*/
    }
  }
}
