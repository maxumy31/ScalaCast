import RayCast.Geometry.PrimitiveGeometry.*
import RayCast.Primitives.SceneObjects
import RayCast.Primitives.SceneObjects.*
import RayCast.Ray.*
import Math.Vec3.*
import RayCast.Scene.Scene.*
import RayCast.Scene.Viewport.*
import RayCast.Light.*
import RayCast.Color.*
import RayCast.Light.Light.*
import RayCast.Primitives.Material.Material
import RayCast.Scene.Camera.Camera

import scala.concurrent.duration.*


object Main extends App {
  import javax.imageio.ImageIO
  import java.io.File
  import Image.ImageManip.*
  import RayCast.Color.*

  val width = 1920
  val height = 1080
  val cameraPos = Vec3(4,0,30)
  val focalLength = 2.0

  val camera = Camera(cameraPos,45,width/height.toDouble,focalLength)
  val positions = CreateViewportFromCamera(camera,width,height)
  val pixels = new Array[Int](width * height)
  val image = CreateEmptyImage(width, height)


  val(currentScene,sceneLoadTime) = measureTime({
    /*val spGeom1 = SphereGeometry(3, Vec3(4, 4, 20))
    val material1 = Material(1.0,Color(0xFF, 255, 255, 0))
    val sphere1 = MonochromeSphere(material1, spGeom1)

    val spGeom2 = SphereGeometry(3, Vec3(4, -4, 17))
    val material2 = Material(1.0,Color(0xFF, 255, 0, 255))
    val sphere2 = MonochromeSphere(material2, spGeom2)

    val spGeom3 = SphereGeometry(8, Vec3(6, 10, 30))
    val material3 = Material(0.5,Color(0xFF, 0, 255, 255))
    val sphere3 = MonochromeSphere(material3, spGeom3)

    val spGeom4 = SphereGeometry(8, Vec3(-10, -3, 20))
    val material4 = Material(0.3,Color(0xFF, 255, 255, 255))
    val sphere4 = MonochromeSphere(material4, spGeom4)

    val spGeom5 = SphereGeometry(8, Vec3(0, 0, 35))
    val material5 = Material(0.0,Color(0xFF, 140, 140, 140))
    val sphere5 = MonochromeSphere(material5, spGeom5)

    //val pointLight = PointLight(0.5, Vec3(0, -10, 10))
    val ambient = AmbientLight(0.5)
    val direct = DirectionLight(0.6,Vec3(1,0,0))
    Scene(Seq[Primitive](sphere1,sphere2,sphere3,sphere4,sphere5),Seq(ambient,direct))*/
    val spGeom1 = SphereGeometry(3, Vec3(0, 0, 50))
    val material1 = Material(0.0, Color(0xFF, 255, 215, 0))
    val sphere1 = MonochromeSphere(material1, spGeom1)

    val spGeom2 = SphereGeometry(3, Vec3(-4, 0, 50))
    val material2 = Material(0.9, Color(0xFF, 176, 196, 222))
    val sphere2 = MonochromeSphere(material2, spGeom2)

    val trGeom = TriangleGeometry(Vec3(100,-3,0),Vec3(-100,-3,0),Vec3(-100,-3,300))
    val trMaterial = Material(0.9,Color(0xFF,192,192,192))
    val triangle = MonochromeTriangle(trMaterial,trGeom)

    val trGeom2 = TriangleGeometry(Vec3(-100, -3, 300), Vec3(100, -3, 300), Vec3(100, -3, 0))
    val trMaterial2 = Material(0.9, Color(0xFF, 192, 192, 192))
    val triangle2 = MonochromeTriangle(trMaterial2, trGeom2)

    val ambient = AmbientLight(0.4)
    //val direct = DirectionLight(0.6, Vec3(0, 0, 1))
    val point = PointLight(0.9,Vec3(0,15,15))

    Scene(Seq[SceneObject](sphere1,triangle,triangle2),
      Seq(ambient,point))
  })

  println("Scene loaded in " + sceneLoadTime.toMillis.toString + "ms")

  val (colors, renderingTime) = measureTime({
    val backgroundColor = Color(0xFF,0,0,0)
    val scene = currentScene
    val rays = positions.map(pos => {
      val ray_dir = Normalize(Sub(pos, cameraPos))
      Ray(cameraPos, ray_dir)
    })

    val intersections = rays.map(ray => {
      IntersectClosestObject(ray, scene)
    })

    val lights = rays.zip(intersections).map((ray,opt) => {
      opt match
        case None => 0.0
        case Some(prim,t) =>
          val geo = GetGeometry(prim)
          ComputeLightForIntersection(ray, t, GetNormal(RayAt(ray, t), geo), scene)
    })

    val cols = intersections.map(_.map((prim,opt) => GetMaterial(prim).albedo))

    val reflections = intersections.zip(rays).map((opt,ray) => {
      opt match
        case None => None
        case Some(prim,t) =>
          ComputeReflection(ray, t, prim, 5, GetMaterial(prim).reflective, scene)
    })

    val mixedColors = reflections.zip(cols).map((refl,col) => {
      col match
        case None => backgroundColor
        case Some(baseColor) =>
          refl match
            case None => baseColor
            case Some(reflColor) => Mix(reflColor,baseColor,0.4)
    })

    lightColors(mixedColors,lights).map(ColorToInt)
  })
  println("Image rendered in " + renderingTime.toMillis.toString + "ms")

  val img = FillImage(image,colors)

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

def lightColors(cols:Seq[Color],lights:Seq[Double]):Seq[Color] = {
  cols.zip(lights).map((col,lig) => Lighten(lig,col))
}