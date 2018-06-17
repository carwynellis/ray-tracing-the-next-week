package uk.carwynellis.raytracing.material

import uk.carwynellis.raytracing._
import uk.carwynellis.raytracing.texture.Texture

abstract class Material(val albedo: Texture) {

  // TODO - for now scatter returns a tuple of Ray and Vec3 (where the Vec3 is the attenuation) - this could be refined.
  def scatter(rayIn: Ray, record: HitRecord): (Ray, Vec3)

  /**
    * Default emitted implementation that returns black.
    *
    * This can be overridden to define materials that are light sources.
    *
    * @param u
    * @param v
    * @param p
    * @return
    */
  def emitted(u: Double, v: Double, p: Vec3): Vec3 = Vec3(0, 0, 0)

}

object Material {

  def reflect(v: Vec3, n: Vec3): Vec3 = v - ( 2 * v.dot(n) * n)

}




