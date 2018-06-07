package uk.carwynellis.raytracing.hitable

import uk.carwynellis.raytracing._

import scala.annotation.tailrec

class Sphere(val centre: Vec3, val radius: Double, val material: Material) extends Hitable {

  // TODO - get some test coverage of this method
  // TODO - refactor and tidy up
  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
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
    Some(AxisAlignedBoundingBox(
      min = centre - radiusVector,
      max = centre + radiusVector
    ))
  }

}

object Sphere {
  def apply(centre: Vec3, radius: Double, material: Material) = new Sphere(centre, radius, material)

  @tailrec
  def randomPointInUnitSphere(): Vec3 = {
    val randomPoint = 2.0 * Vec3(
      x = math.random(),
      y = math.random(),
      z = math.random()
    )
    if (randomPoint.squaredLength >= 1) randomPointInUnitSphere()
    else randomPoint
  }
}

