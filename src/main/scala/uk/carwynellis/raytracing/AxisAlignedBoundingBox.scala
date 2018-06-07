package uk.carwynellis.raytracing

import scala.annotation.tailrec

/**
  * Defines an axis aligned bounding box (AABB) around a set of objects.
  *
  * Used to rapidly exclude rays that will never hit an object.
  *
  * @param min
  * @param max
  */
class AxisAlignedBoundingBox(min: Vec3, max: Vec3) {

  /**
    * Determine whether a given ray has hit the bounding box or not.
    *
    * @param ray
    * @return
    */
  def hit(ray: Ray): Boolean = {
    @tailrec
    def loop(i: Int, tMin: Double, tMax: Double): Boolean = {
      if (i >= 3) true
      else {
        val iMin = (min.get(i) - ray.origin.get(i)) / ray.direction.get(i)
        val iMax = (max.get(i) - ray.origin.get(i)) / ray.direction.get(i)

        val t0 = minD(iMin, iMax)
        val t1 = maxD(iMin, iMax)

        val sMin = minD(t0, t1)
        val sMax = maxD(t0, t1)

        if (sMax <= sMin) false
        else loop(i + 1, sMin, sMax)
      }
    }

    loop(0, 0, 0)
  }

  // TODO - are these any faster than math.min / math.max?
  private def minD(a: Double, b: Double) = if (a < b) a else b
  private def maxD(a: Double, b: Double) = if (a > b) a else b

}

object AxisAlignedBoundingBox {
  def apply(min: Vec3, max: Vec3) = new AxisAlignedBoundingBox(min, max)
}
