package RayCast


object Color {
  case class Color(a:Char,r:Char,g:Char,b:Char)

  def ColorToInt(col:Color): Int = {
    (col.a << 24) | (col.r << 16) | (col.g << 8) | col.b
  }

  def ColorsToPixels(cols:Seq[Color]):Seq[Int] = {
    cols.map(col => ColorToInt(col))
  }

}
