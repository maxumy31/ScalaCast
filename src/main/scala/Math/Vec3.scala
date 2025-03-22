package Math

object Vec3 {
  case class Vec3(x:Double,y:Double,z:Double)

  def Add(a:Vec3, b:Vec3):Vec3 = {
    Vec3(a.x + b.x, a.y + b.y, a.z + b.z)
  }

  def Sub(a:Vec3, b:Vec3):Vec3 = {
    Vec3(a.x - b.x, a.y - b.y, a.z - b.z)
  }

  def Mult(a:Vec3, scalar : Double):Vec3 = {
    Vec3(a.x * scalar, a.y * scalar, a.z * scalar)
  }

  def Div(a:Vec3, scalar : Double):Vec3 = {
    Vec3(a.x / scalar, a.y / scalar, a.z / scalar)
  }

  def Length(a:Vec3):Double = {
    (a.x * a.x) + (a.y * a.y) + (a.z * a.z)
  }

  def Dot(a:Vec3, b:Vec3) = {
    a.x * b.x + a.y * b.y + a.z * b.z
  }

  def Normalize(a:Vec3) = {
    val l = Length(a)
    Vec3(a.x/l,a.y/l,a.z/l)
  }

  def Cross(a:Vec3,b:Vec3) = {
    Vec3(a.y * b.z - a.z * b.y,
      a.z * b.x - a.x * b.z,
      a.x * b.y - a.y * b.x)
  }
}
