package uk.carwynellis.raytracing.material

import uk.carwynellis.raytracing.hitable.Sphere
import uk.carwynellis.raytracing.texture.Texture
import uk.carwynellis.raytracing.{HitRecord, Ray}

class IsoTropic(albedo: Texture) extends Material(albedo) {

  override def scatter(rayIn: Ray, record: HitRecord): Option[ScatterResult] = {
    val scattered = Ray(record.p, Sphere.randomPointInUnitSphere())
    val attenuation = albedo.value(record.u, record.v, record.p)
    Some(ScatterResult(scattered, attenuation))
  }

}

object IsoTropic {
  def apply(albedo: Texture) = new IsoTropic(albedo)
}
