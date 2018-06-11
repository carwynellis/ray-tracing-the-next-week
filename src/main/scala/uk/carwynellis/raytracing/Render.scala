package uk.carwynellis.raytracing

import uk.carwynellis.raytracing.hitable.BoundingVolumeHierarchy

object Render extends App {

  val width = 1200
  val height = 800
  val samples = 10

  val origin = Vec3(13, 2, 3)
  val target = Vec3(0, 0, 0)

  val time0 = 0.0
  val time1 = 0.2

  val camera = Camera(
    origin = origin,
    target = target,
    upVector = Vec3(0, 1, 0),
    verticalFieldOfView = 20,
    aspectRatio =  width.toDouble / height.toDouble,
    aperture = 0.0,
    focusDistance = 10,
    time0 = 0.0,
    time1 = 0.2
  )

  val filename = "image.ppm"

  println(s"Rendering scene to $filename")

  val bvh = BoundingVolumeHierarchy.ofHitables(Scene.randomScene().hitables, time0, time1)

  val renderer = Renderer(camera, Scene.randomScene(), width, height, samples)
  val imageWriter = ImageWriter(width, height, "image.ppm")

  renderer.renderScene().foreach(imageWriter.writePixel)

  imageWriter.close()

  println("\nFinished")
}
