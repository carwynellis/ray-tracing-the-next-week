package uk.carwynellis.raytracing.hitable

import uk.carwynellis.raytracing._
import uk.carwynellis.raytracing.material.Material

import scala.annotation.tailrec

class Sphere(val centre: Vec3, val radius: Double, val material: Material) extends Hitable {

  // TODO - get some test coverage of this method
  // TODO - refactor and tidy up
  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    val oc = r.origin - centre

    val a = r.direction.dot(r.direction)
    val b = oc.dot(r.direction)
    val c = oc.dot(oc) - (radius * radius)

    val discriminant = Math.pow(b, 2) - (a * c)

    if (discriminant > 0) {
      val discriminantRoot = math.sqrt(discriminant)
      val x = (-b - discriminantRoot) / a
      if (x < tMax && x > tMin) {
        val pointAtParameter = r.pointAtParameter(x)
        val normal = (pointAtParameter - centre) / radius
        val record = HitRecord(
          t = x,
          u = Sphere.getSphereU(normal),
          v = Sphere.getSphereV(normal),
          p = pointAtParameter,
          normal = (pointAtParameter - centre) / radius,
          material = material
        )
        return Some(record)
      }

      val y = (-b + discriminantRoot) / a
      if (y < tMax && y > tMin) {
        val pointAtParameter = r.pointAtParameter(y)
        val normal = (pointAtParameter - centre) / radius
        val record = HitRecord(
          t = y,
          u = Sphere.getSphereU(normal),
          v = Sphere.getSphereV(normal),
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
    val randomPoint = (2.0 * Vec3(
      x = Random.double,
      y = Random.double,
      z = Random.double
    )) - Vec3(1, 1, 1)
    if (randomPoint.squaredLength >= 1) randomPointInUnitSphere()
    else randomPoint
  }

  /**
    * To assist with image maps we need to compute a scaled image coordinate which can be used to map to a pixel on the
    * image.
    *
    * @param p
    * @return
    */
  def getSphereU(p: Vec3): Double = {
    val phi = Math.atan2(p.z, p.x)
    1 - (phi + math.Pi) / (2 * math.Pi)
  }

  /**
    * To assist with image maps we need to compute a scaled image coordinate which can be used to map to a pixel on the
    * image.
    *
    * @param p
    * @return
    */
  def getSphereV(p: Vec3): Double = {
    val theta = Math.asin(p.y)
    (theta + math.Pi/2) / math.Pi
  }

}

