package RayCast
import Math.Utils.*

object Color {
  case class Color(a:Char,r:Char,g:Char,b:Char)

  def ColorToInt(col:Color): Int = {
    (col.a << 24) | (col.r << 16) | (col.g << 8) | col.b
  }

  def ColorsToPixels(cols:Seq[Color]):Seq[Int] = {
    cols.map(col => ColorToInt(col))
  }

  def Lighten(intensity:Double, color: Color) = {
    //if intensity > 1.0 then println(intensity)
    val coef = Clamp(intensity,0.0,1.0)
    Color(color.a, (color.r * coef).toChar, (color.g * coef).toChar, (color.b * coef).toChar)
  }

}
