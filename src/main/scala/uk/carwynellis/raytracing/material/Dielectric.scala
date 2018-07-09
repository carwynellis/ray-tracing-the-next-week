package uk.carwynellis.raytracing.material

import org.apache.commons.math3.util.FastMath
import uk.carwynellis.raytracing.{HitRecord, Ray, Vec3}
import uk.carwynellis.raytracing.texture.ConstantTexture

class Dielectric(refractiveIndex: Double) extends Material(ConstantTexture(Vec3(1,1,1))) {

  override def scatter(rayIn: Ray, record: HitRecord): Option[(Ray, Vec3)] = {
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

    Some((rayOut, albedo.value(0, 0, record.p)))
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
    r0Squared + (1 - r0Squared) * FastMath.pow(1 - cosine, 5)
  }
}

object Dielectric {
  def apply(refractiveIndex: Double) = new Dielectric(refractiveIndex)
}
