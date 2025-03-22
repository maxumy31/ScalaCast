import RayCast.Geometry.PrimitiveGeometry.*
import RayCast.Primitives.Primitives
import RayCast.Primitives.Primitives.*
import RayCast.Ray.*
import Math.Vec3.*
import RayCast.Scene.*
import RayCast.Viewport.*
import RayCast.Light.*
import RayCast.Color.*
import RayCast.Light.Light.{AmbientLight, ComputeLightning, DirectionLight, PointLight}

import scala.concurrent.duration.*
import java.time.*
import java.time.format.DateTimeFormatter
import scala.compiletime.ops.double.%


object Main extends App {
  import java.awt.image.BufferedImage
  import javax.imageio.ImageIO
  import java.io.File
  import Image.ImageManip.*
  import RayCast.Color.*

  val width = 800
  val height = 600
  val cameraPos = Vec3(0,0,0)
  val focalLength = 2.0
  val viewportWidth = 2.0
  val viewportHeight = viewportWidth * (height.toDouble/width.toDouble)
  val positions = CreateViewport(cameraPos,focalLength,width,height,viewportWidth,viewportHeight)
  val pixels = new Array[Int](width * height)
  val image = CreateEmptyImage(width, height)


  val(currentScene,sceneLoadTime) = measureTime({
    val spGeom1 = SphereGeometry(3, Vec3(4, 4, 20))
    val sphere1 = MonocolorSphere(Color(0xFF, 255, 255, 0), spGeom1)

    val spGeom2 = SphereGeometry(3, Vec3(4, -4, 17))
    val sphere2 = MonocolorSphere(Color(0xFF, 255, 0, 255), spGeom2)

    val spGeom3 = SphereGeometry(8, Vec3(6, 10, 30))
    val sphere3 = MonocolorSphere(Color(0xFF, 0, 255, 255), spGeom3)

    val spGeom4 = SphereGeometry(8, Vec3(-10, -3, 25))
    val sphere4 = MonocolorSphere(Color(0xFF, 255, 255, 255), spGeom4)
    val pointLight = PointLight(1.0, Vec3(4, 0, 20))
    val ambient = AmbientLight(0.15)
    val direct = DirectionLight(0.4,Vec3(1,0,0))
    Scene(Seq(sphere1,sphere2,sphere3,sphere4),Seq(ambient,direct))
  })

  println("Scene loaded in " + sceneLoadTime.toMillis.toString + "ms")

  val (_, renderingTime) = measureTime({
    val scene = currentScene
    positions.map(pos => {
      val ray_dir = Normalize(Sub(pos, cameraPos))
      val ray = Ray(cameraPos, ray_dir)
      IntersectClosestObject(ray, scene) match
        case Some(obj, t) =>
          val light = ComputeLightForIntersection(ray, t, GetNormal(RayAt(ray, t), obj.geometry), scene)
          ColorToInt(Lighten(light, obj.color))
        case None => ColorToInt(Color(0xFF, 0, 0, 0))

    }).zipWithIndex.foreach { z => {
      pixels(z._2) = z._1
    }
    }
  })
  println("Image rendered in " + renderingTime.toMillis.toString + "ms")
  val colorMap = pixels

  val img = FillImage(image,colorMap)

  ImageIO.write(img, "png", new File("output.png"))
  println("New image")
}

def measureTime[T](block: => T): (T, FiniteDuration) = {
  val startTime = System.nanoTime()
  val result = block // Выполняем переданный блок кода
  val endTime = System.nanoTime()
  val duration = (endTime - startTime).nanos
  (result, duration)
}