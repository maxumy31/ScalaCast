import RayCast.Geometry.PrimitiveGeometry.*
import RayCast.Primitives.Primitives
import RayCast.Primitives.Primitives.*
import RayCast.Ray.*
import Math.Vec3.*
import RayCast.Scene.Scene.*
import RayCast.Scene.Viewport.*
import RayCast.Light.*
import RayCast.Color.*
import RayCast.Light.Light.{AmbientLight, ComputeLightning, DirectionLight, PointLight}
import RayCast.Primitives.Material.Material

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

  val width = 1920
  val height = 1080
  val cameraPos = Vec3(0,0,0)
  val focalLength = 2.0
  val viewportWidth = 2.0
  val viewportHeight = viewportWidth * (height.toDouble/width.toDouble)
  val positions = CreateViewport(cameraPos,focalLength,width,height,viewportWidth,viewportHeight)
  val pixels = new Array[Int](width * height)
  val image = CreateEmptyImage(width, height)


  val(currentScene,sceneLoadTime) = measureTime({
    val spGeom1 = SphereGeometry(3, Vec3(4, 4, 20))
    val material1 = Material(1.0,Color(0xFF, 255, 255, 0))
    val sphere1 = MonocolorSphere(material1, spGeom1)

    val spGeom2 = SphereGeometry(3, Vec3(4, -4, 17))
    val material2 = Material(1.0,Color(0xFF, 255, 0, 255))
    val sphere2 = MonocolorSphere(material2, spGeom2)

    val spGeom3 = SphereGeometry(8, Vec3(6, 10, 30))
    val material3 = Material(0.5,Color(0xFF, 0, 255, 255))
    val sphere3 = MonocolorSphere(material3, spGeom3)

    val spGeom4 = SphereGeometry(8, Vec3(-10, -3, 20))
    val material4 = Material(0.3,Color(0xFF, 255, 255, 255))
    val sphere4 = MonocolorSphere(material4, spGeom4)

    val spGeom5 = SphereGeometry(8, Vec3(0, 0, 35))
    val material5 = Material(0.0,Color(0xFF, 140, 140, 140))
    val sphere5 = MonocolorSphere(material5, spGeom5)

    val pointLight = PointLight(1.0, Vec3(4, 0, 20))
    val ambient = AmbientLight(0.5)
    val direct = DirectionLight(0.6,Vec3(1,0,0))
    Scene(Seq(sphere1,sphere2,sphere3,sphere4,sphere5),Seq(ambient,direct))
  })

  println("Scene loaded in " + sceneLoadTime.toMillis.toString + "ms")

  val (_, renderingTime) = measureTime({
    val scene = currentScene
    positions.map(pos => {
      val ray_dir = Normalize(Sub(pos, cameraPos))
      val ray = Ray(cameraPos, ray_dir)
      IntersectClosestObject(ray, scene) match
        case Some(obj, t) =>
          val refl = ComputeReflection(ray,t,obj,3,obj.material.reflective,scene)
          refl match
            case None =>
              val light = ComputeLightForIntersection(ray, t, GetNormal(RayAt(ray, t), obj.geometry), scene)
              ColorToInt(Lighten(light, obj.material.albedo))
            case Some(reflColor) =>
              val light = ComputeLightForIntersection(ray, t, GetNormal(RayAt(ray, t), obj.geometry), scene)
              ColorToInt(Lighten(light, Mix(reflColor,obj.material.albedo,0.8)))
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