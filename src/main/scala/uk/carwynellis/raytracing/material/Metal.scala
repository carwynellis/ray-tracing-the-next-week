package uk.carwynellis.raytracing.material

import uk.carwynellis.raytracing.hitable.Sphere
import uk.carwynellis.raytracing.{HitRecord, Ray, Vec3}
import uk.carwynellis.raytracing.texture.Texture

class Metal(albedo: Texture, fuzziness: Double) extends Material(albedo) {
  override def scatter(rayIn: Ray, record: HitRecord): (Ray, Vec3) = {
    val reflected = Material.reflect(rayIn.direction.unitVector, record.normal)
    (Ray(record.p, reflected + (fuzziness * Sphere.randomPointInUnitSphere()), rayIn.time), albedo.value(0, 0, record.p))
  }
}

object Metal {
  def apply(albedo: Texture, fuzziness: Double) = new Metal(albedo, fuzziness)
}
