package uk.carwynellis.raytracing

import uk.carwynellis.raytracing.hitable.Hitable

class Renderer(camera: Camera, scene: Hitable, width: Int, height: Int, samples: Int) {

  // When rendering some rays may may include a floating point error preventing them from being treated as 0.
  // We increase the minimum value we accept slight which yields a smoother image without visible noise.
  val ImageSmoothingLimit = 0.001

  val BlackBackground = Vec3(0, 0, 0)

  /**
    * Compute the color for a given ray.
    *
    * TODO - do we need to pass in depth parameter?
    *
    * @param r
    * @return
    */
  def color(r: Ray, world: Hitable, depth: Int): Vec3 = {

    val hitResult = world.hit(r, ImageSmoothingLimit, Double.MaxValue)

    hitResult match {
      case Some(hit) =>
        val emitted = hit.material.emitted(hit.u, hit.v, hit.p)
        // TODO - scatter should return a boolean to indicate that we can ignore scattering
        if (depth < 50 && emitted == BlackBackground) {
          val (scattered, attenuation) = hit.material.scatter(r, hit)
          emitted + attenuation * color(scattered, world, depth + 1)
        }
        else emitted
      // If the ray hits nothing draw return black.
      case None => BlackBackground
    }

  }

  /**
    * Sample a number of randomly generated rays for the current pixel.
    *
    * Higher sample counts yield a better quality image at the expense of longer render times.
    * @param x
    * @param y
    * @return
    */
  def renderPixel(x: Int, y: Int): Pixel = {
    val result = (0 until samples).map { _ =>
      val xR = (x.toDouble + math.random()) / width
      val yR = (y.toDouble + math.random()) / height
      val ray = camera.getRay(xR, yR)
      color(ray, scene, 0)
    }.reduce(_ + _) / samples
    result.toPixel
  }

  /**
    * Renders the entire scene returning a list of Pixels representing the rendered scene.
    *
    * @return
    */
  def renderScene(): Seq[Pixel] = (height-1 to 0 by -1).flatMap { j: Int =>
    showProgress(j)
    (0 until width).map(renderPixel(_, j))
  }

  // Basic progress indication, updated for each horizontal line of the image.
  private def showProgress(hPos: Int): Unit = {
    val percentComplete = 100 - ((hPos.toDouble / height) * 100)
    printf("\r% 4d%s complete", percentComplete.toInt, "%")
  }

}

object Renderer {
  def apply(camera: Camera, scene: Hitable, width: Int, height: Int, samples: Int) =
    new Renderer(camera, scene, width, height, samples)
}
