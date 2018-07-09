package uk.carwynellis.raytracing.hitable
import uk.carwynellis.raytracing.material.IsoTropic
import uk.carwynellis.raytracing.texture.Texture
import uk.carwynellis.raytracing.{AxisAlignedBoundingBox, HitRecord, Ray, Vec3}

// TODO - any value in differentiating materials from functions?
class ConstantMedium(boundary: Hitable, density: Double, albedo: Texture) extends Hitable {

  private val phaseFunction = IsoTropic(albedo)

  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] =
    boundary.hit(r, Double.MinValue, Double.MaxValue).flatMap { hit1 =>
      boundary.hit(r, hit1.t + 0.0001, Double.MaxValue).flatMap { hit2 =>
        // TODO - tidy this up, getting the original logic working first
        val t1 = if (hit1.t < tMin) tMin else hit1.t
        val t2 = if (hit2.t > tMax) tMax else hit2.t
        if (t1 >= t2) None
        else {
          val boundedT1 = if (t1 < 0) 0 else t1
          val distanceInsideBoundary = (t2 - boundedT1) * r.direction.length
          val hitDistance = -(1/density) * math.log(math.random())
          if (hitDistance < distanceInsideBoundary) {
            val hitT: Double = boundedT1 + hitDistance / r.direction.length
            Some(HitRecord(
              t = hitT,
              p = r.pointAtParameter(hitT),
              normal = ConstantMedium.Normal, // Arbitrary value
              material = phaseFunction,
              // TODO - should these values be set to something else?
              u = 0.0,
              v = 0.0
            ))
          }
          else None
        }
      }
    }


  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = boundary.boundingBox(t0, t1)
}

object ConstantMedium {
  def apply(boundary: Hitable, density: Double, albedo: Texture) = new ConstantMedium(boundary, density, albedo)

  val Normal = Vec3(1, 0, 0)

}
