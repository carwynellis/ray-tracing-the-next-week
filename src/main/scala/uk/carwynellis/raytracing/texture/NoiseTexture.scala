package uk.carwynellis.raytracing.texture
import uk.carwynellis.raytracing.Vec3

class NoiseTexture extends Texture {

  private val perlinNoise = new Perlin

  override def value(u: Double, v: Double, p: Vec3): Vec3 = Vec3(1, 1, 1) * perlinNoise.noise(p)

}

object NoiseTexture {
  def apply() = new NoiseTexture
}
