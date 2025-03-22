package Math

object Utils {
  def Clamp(a:Double,min:Double,max:Double) : Double = {
    if a > max then max else
      if a < min then min else
        a
  }
}
