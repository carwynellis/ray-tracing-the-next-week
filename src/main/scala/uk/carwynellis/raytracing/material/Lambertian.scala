package uk.carwynellis.raytracing.material

import uk.carwynellis.raytracing.{HitRecord, Ray, Vec3}
import uk.carwynellis.raytracing.hitable.Sphere
import uk.carwynellis.raytracing.texture.Texture

class Lambertian(albedo: Texture) extends Material(albedo) {
  override def scatter(rayIn: Ray, record: HitRecord): (Ray, Vec3) = {
    val target = record.p + record.normal + Sphere.randomPointInUnitSphere()
    (Ray(record.p, target - record.p, rayIn.time), albedo.value(record.u, record.v, record.p))
  }
}

object Lambertian {
  def apply(albedo: Texture) = new Lambertian(albedo)
}