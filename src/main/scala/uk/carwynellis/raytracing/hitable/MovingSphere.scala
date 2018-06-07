package uk.carwynellis.raytracing.hitable

import uk.carwynellis.raytracing._

/**
  * Class representing a sphere in motion.
  *
  * TODO - this could (and probably should) be consolidated into Sphere and perhaps hitable in general so all objects
  *        that can be rendered support the concept of motion.
  *
  * @param centre0
  * @param centre1
  * @param radius
  * @param material
  * @param time0
  * @param time1
  */
class MovingSphere(val centre0: Vec3,
                   val centre1: Vec3,
                   val radius: Double,
                   val material: Material,
                   val time0: Double,
                   val time1: Double) extends Hitable {

  // Compute the location of the sphere centre at the specified time.
  private def centreAtTime(time: Double): Vec3 = centre0 + ( (time - time0) / (time1 - time0) ) * (centre1 - centre0)

  // TODO - get some test coverage of this method
  // TODO - refactor and tidy up
  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    val centre = centreAtTime(r.time)
    val oc = r.origin - centre

    val a = r.direction.dot(r.direction)
    val b = oc.dot(r.direction)
    val c = oc.dot(oc) - (radius * radius)

    val discriminant = (b * b) - (a * c)
    val discriminantRoot = math.sqrt(discriminant)

    if (discriminant > 0) {
      val x = (-b - discriminantRoot) / a
      if (x < tMax && x > tMin) {
        val record = HitRecord(
          t = x,
          p = r.pointAtParameter(x),
          normal = (r.pointAtParameter(x) - centre) / radius,
          material = material
        )
        return Some(record)
      }

      val y = (-b + discriminantRoot) / a
      if (y < tMax && y > tMin) {
        val record = HitRecord(
          t = y,
          p = r.pointAtParameter(y),
          normal = (r.pointAtParameter(y) - centre) / radius,
          material = material
        )
        return Some(record)
      }
    }

    None
  }

  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = {
    val radiusVector = Vec3(radius, radius, radius)

    def boundingBoxAtTime(t: Double) = AxisAlignedBoundingBox(
      min = centreAtTime(t) - radiusVector,
      max = centreAtTime(t) + radiusVector
    )

    Some(AxisAlignedBoundingBox.surroundingBox(
      box0 = boundingBoxAtTime(t0),
      box1 = boundingBoxAtTime(t1)
    ))
  }

}

object MovingSphere {
  def apply(centre0: Vec3, centre1: Vec3, radius: Double, material: Material, time0: Double, time1: Double) =
    new MovingSphere(centre0, centre1, radius, material, time0, time1)
}
