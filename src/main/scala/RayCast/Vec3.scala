package RayCast

object Vec3 {
  case class Vec3(x:Double,y:Double,z:Double)

  def add(a:Vec3,b:Vec3):Vec3 = {
    Vec3(a.x + b.x, a.y + b.y, a.z + b.z)
  }

  def sub(a:Vec3,b:Vec3):Vec3 = {
    Vec3(a.x - b.x, a.y - b.y, a.z - b.z)
  }

  def mult(a:Vec3, scalar : Double):Vec3 = {
    Vec3(a.x * scalar, a.y * scalar, a.z * scalar)
  }

  def div(a:Vec3, scalar : Double):Vec3 = {
    Vec3(a.x / scalar, a.y / scalar, a.z / scalar)
  }

  def length(a:Vec3):Double = {
    (a.x * a.x) + (a.y * a.y) + (a.z * a.z)
  }

  def dot(a:Vec3, b:Vec3) = {
    Vec3(a.x * b.x,a.y * b.y,a.z * b.z)
  }

  def normalize(a:Vec3) = {
    val l = length(a)
    Vec3(a.x/l,a.y/l,a.z/l)
  }
}
