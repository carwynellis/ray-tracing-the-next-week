package uk.carwynellis.raytracing

import uk.carwynellis.raytracing.hitable.BoundingVolumeHierarchy

object Render extends App {

  val width = 1200
  val height = 800
  val samples = 10

  val origin = Vec3(478, 278, -600)
  val target = Vec3(278, 278, 0)

  val time0 = 0.0
  val time1 = 1

  val camera = Camera(
    origin = origin,
    target = target,
    upVector = Vec3(0, 1, 0),
    verticalFieldOfView = 40,
    aspectRatio =  width.toDouble / height.toDouble,
    aperture = 0.0,
    focusDistance = 10,
    time0 = time0,
    time1 = time1
  )

  val filename = "image.ppm"

  println(s"Rendering scene to $filename")

  val bvh = BoundingVolumeHierarchy
    .fromHitables(Scene.finalScene.hitables, time0, time1)

  val renderer = Renderer(camera, bvh, width, height, samples)
  val imageWriter = ImageWriter(width, height, filename)

  renderer.renderScenePar().foreach(imageWriter.writePixel)

  imageWriter.close()

  println("\nFinished")
}
