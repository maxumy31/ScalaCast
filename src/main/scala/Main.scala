import RayCast.Primitives.*
import RayCast.Ray.*
import RayCast.Vec3.*


object Main extends App {
  import java.awt.image.BufferedImage
  import javax.imageio.ImageIO
  import java.io.File
  import Image.ImageManip.*
  import RayCast.Color.*

  val width = 800
  val height = 600
  val aspectRatio = width / height.toDouble

  val viewport_height = 2.0
  val viewport_width = viewport_height * (width/height.toDouble)

  val focal_length = 1.0
  val cameraPos = Vec3(0,0,0)

  val viewport_u = Vec3(viewport_width,0,0)
  val viewport_v = Vec3(0,-viewport_height,0)

  val pixel_delta_u = div(viewport_u,width)
  val pixel_delta_v = div(viewport_v,height)

  val pixels = new Array[Int](width * height) // Буфер ARGB
  val image = CreateEmptyImage(800,600)

  val upper_left = sub(sub(sub(cameraPos, Vec3(0, 0, focal_length)),div(viewport_u, 2)),div(viewport_v, 2))
  println(upper_left)
  // Заполнение буфера градиентом
  for (y <- 0 until height; x <- 0 until width) {
    val pixel_center = add(upper_left, add(mult(pixel_delta_u, x), mult(pixel_delta_v, y)))
    val ray_dir = normalize(sub(pixel_center,cameraPos))
    val r = (x.toDouble / width * 255).toInt
    val g = (y.toDouble / height * 255).toInt
    val b = 128
    val c = Color(0xFF,r.toChar,g.toChar,b.toChar)
    val ray = Ray(cameraPos,ray_dir)
    pixels(y * width + x) = (ColorToInt(rc(ray)))
  }
  val colorMap = pixels

  def rc(r:Ray) ={
    val dir = r.direction
    val a = 0.5 * (dir.y + 1)
    val sp = Sphere(0.5,Vec3(0,0,-1))
    Intersect(r, sp) match
      case Some(v) => Color(0xFF,0,0,0)
      case None => Color(0xFF,((1.0-a + 0.5*a) * 255).toChar,((1.0-a + 0.7*a) * 255).toChar,((1.0-a + 1.0*a) * 255).toChar)
    //println(RayAt(r, v));
    //println(r);
    //println("######");
  }

  val img = FillImage(image,colorMap)


  ImageIO.write(img, "png", new File("output.png"))

}