package uk.carwynellis.raytracing.texture
import java.awt.Color
import java.io.File

import javax.imageio.ImageIO
import uk.carwynellis.raytracing.Vec3

class ImageTexture(path: String) extends Texture {

  // Enable texture coordinates so we can map the image to the surface of the sphere.
  override def requiresTextureCoordinates: Boolean = true

  // TODO - set this elsewhere - stops 'Boot' process appearing on Mac arising from use of awt.
  System.setProperty("java.awt.headless", "true")

  // TODO - this can throw
  private val image = ImageIO.read(new File(path))

  override def value(u: Double, v: Double, p: Vec3): Vec3 = {
    val i: Int = {
      val t = u * image.getWidth()
      if (t < 0) 0
      else if (t > image.getWidth() - 1) image.getWidth() - 1
      else t.toInt
    }
    val j: Int = {
      val t = (1 - v) * image.getHeight() - 0.001
      if (t < 0) 0
      else if (t > image.getHeight() - 1) image.getHeight() - 1
      else t.toInt
    }

    val pixel = new Color(image.getRGB(i, j))

    Vec3(
      pixel.getRed / 255.0,
      pixel.getGreen / 255.0,
      pixel.getBlue / 255.0
    )
  }

}

object ImageTexture {

  // TODO - maybe pass a Buffered image and have this method take care of loading...
  def apply(path: String) = new ImageTexture(path)

}
