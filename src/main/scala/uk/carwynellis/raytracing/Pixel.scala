package uk.carwynellis.raytracing

case class Pixel(r: Int, g: Int, b: Int)

object Pixel {
  def fromVec3(v: Vec3): Pixel = {
    // Gamma correct the current pixel using gamma2 e.g. sqrt of each component.
    val gammaCorrected = Vec3(
      x = clip(math.sqrt(v.x)),
      y = clip(math.sqrt(v.y)),
      z = clip(math.sqrt(v.z))
    )

    new Pixel(
      r = (255.99 * gammaCorrected.x).toInt,
      g = (255.99 * gammaCorrected.y).toInt,
      b = (255.99 * gammaCorrected.z).toInt
    )
  }

  // TODO - Temporary fudge until the real cause of the pixel values exceeding 255 is located.
  private def clip(d: Double) = if (d > 1) 1.0 else d
}
