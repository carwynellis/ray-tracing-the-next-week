package uk.carwynellis.raytracing.material

import uk.carwynellis.raytracing._
import uk.carwynellis.raytracing.hitable.Sphere
import uk.carwynellis.raytracing.texture.{ConstantTexture, Texture}

// TODO - move into a material package and split out into separate files

abstract class Material(val albedo: Texture) {

  // TODO - for now scatter returns a tuple of Ray and Vec3 (where the Vec3 is the attenuation) - this could be refined.
  def scatter(rayIn: Ray, record: HitRecord): (Ray, Vec3)

}

object Material {

  def reflect(v: Vec3, n: Vec3): Vec3 = v - ( 2 * v.dot(n) * n)

}

class Lambertian(albedo: Texture) extends Material(albedo) {
  override def scatter(rayIn: Ray, record: HitRecord): (Ray, Vec3) = {
    val target = record.p + record.normal + Sphere.randomPointInUnitSphere()
    (Ray(record.p, target - record.p, rayIn.time), albedo.value(record.u, record.v, record.p))
  }
}

object Lambertian {
  def apply(albedo: Texture) = new Lambertian(albedo)
}

class Metal(albedo: Texture, fuzziness: Double) extends Material(albedo) {
  override def scatter(rayIn: Ray, record: HitRecord): (Ray, Vec3) = {
    val reflected = Material.reflect(rayIn.direction.unitVector, record.normal)
    (Ray(record.p, reflected + (fuzziness * Sphere.randomPointInUnitSphere()), rayIn.time), albedo.value(0, 0, record.p))
  }
}

object Metal {
  def apply(albedo: Texture, fuzziness: Double) = new Metal(albedo, fuzziness)
}

class Dielectric(refractiveIndex: Double) extends Material(ConstantTexture(Vec3(1,1,1))) {

  override def scatter(rayIn: Ray, record: HitRecord): (Ray, Vec3) = {
    val reflected = Material.reflect(rayIn.direction,record.normal)

    val (outwardNormal, niOverNt, cosine) =
      if (rayIn.direction.dot(record.normal) > 0)
        (-record.normal, refractiveIndex, refractiveIndex * rayIn.direction.dot(record.normal) / rayIn.direction.length)
      else
        (record.normal, 1.0 / refractiveIndex, -rayIn.direction.dot(record.normal) / rayIn.direction.length)

    val refracted = refract(rayIn.direction, outwardNormal, niOverNt)

    val reflectionProbability =
      if (refracted == rayIn.direction) 1.0
      else schlick(cosine)

    val rayOut = if (math.random() < reflectionProbability) Ray(record.p, reflected, rayIn.time)
    else Ray(record.p, refracted, rayIn.time)

    (rayOut, albedo.value(0, 0, record.p))
  }

  private def refract(v: Vec3, n: Vec3, niOverNt: Double): Vec3 = {
    val unitVectorOfV = v.unitVector
    val dt = unitVectorOfV.dot(n)
    val discriminant = 1.0 - (niOverNt * niOverNt * (1 - (dt * dt)))
    if (discriminant > 0) (niOverNt * (unitVectorOfV - (n * dt))) - (n * math.sqrt(discriminant))
    else v
  }

  // Polynomial approximation for glass reflectivity.
  private def schlick(cosine: Double): Double = {
    val r0 = (1 - refractiveIndex) / (1 + refractiveIndex)
    val r0Squared = r0 * r0
    r0Squared + (1 - r0Squared) * math.pow(1 - cosine, 5)
  }
}

object Dielectric {
  def apply(refractiveIndex: Double) = new Dielectric(refractiveIndex)
}