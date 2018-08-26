package uk.carwynellis.raytracing.texture
import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import uk.carwynellis.raytracing.Vec3

class ImageTexture(image: BufferedImage) extends Texture {

  // Enable texture coordinates so we can map the image to the surface of the sphere.
  override def requiresTextureCoordinates: Boolean = true

  private val width = image.getWidth()
  private val height = image.getHeight()

  // TODO - clean up the API to remove the Vec3 here?
  // Note, coords map the image from top left (0, 1) to bottom right (1, 0).
  override def value(u: Double, v: Double, notUsed: Vec3): Vec3 = {
    val i = u.toBoundedImageCoordinate(width)
    val j = (1 - v).toBoundedImageCoordinate(height)

    val pixel = new Color(image.getRGB(i, j))

    Vec3(
      pixel.getRed / 255.0,
      pixel.getGreen / 255.0,
      pixel.getBlue / 255.0
    )
  }

  private implicit class DoubleOps(d: Double) {
    def toBoundedImageCoordinate(max: Int): Int =
    if (d < 0) 0
    else if (d > max - 1) max - 1
    else d.toInt
  }

}

object ImageTexture {

  // Run headless to prevent Boot process appearing and stealing focus.
  System.setProperty("java.awt.headless", "true")

  // TODO - this can throw
  def apply(path: String) = new ImageTexture(ImageIO.read(new File(path)))

  def apply(image: BufferedImage) = new ImageTexture(image)

}
