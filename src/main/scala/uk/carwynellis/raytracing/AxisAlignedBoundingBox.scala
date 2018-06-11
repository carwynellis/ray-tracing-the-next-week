package uk.carwynellis.raytracing

import scala.annotation.tailrec
import uk.carwynellis.raytracing.AxisAlignedBoundingBox.minD
import uk.carwynellis.raytracing.AxisAlignedBoundingBox.maxD

/**
  * Defines an axis aligned bounding box (AABB) around a set of objects.
  *
  * Used to rapidly exclude rays that will never hit an object.
  *
  * @param min
  * @param max
  */
class AxisAlignedBoundingBox(val min: Vec3, val max: Vec3) {

  /**
    * Determine whether a given ray has hit the bounding box or not.
    *
    * Note that this is a slightly faster algorithm offered as an alternative in the book and credited to Andrew Kensler
    * at Pixar.
    *
    * @param ray
    * @param tMin
    * @param tMax
    * @return
    */
  def hit(ray: Ray, tMin: Double, tMax: Double): Boolean = {
    @tailrec
    def loop(i: Int, lMin: Double, lMax: Double): Boolean = {
      val invD = 1.0 / ray.direction.get(i)
      val t0 = (min.get(i) - ray.origin.get(i)) * invD
      val t1 = (max.get(i) - ray.origin.get(i)) * invD

      val (s0, s1) = if (invD < 0.0) (t1, t0) else (t0, t1)

      val sMin = if (s0 > lMin) s0 else lMin
      val sMax = if (s1 < lMax) s1 else lMax

      if (sMax <= sMin) false
      else if (i == 2) true
      else loop(i + 1, sMin, sMax)
    }

    loop(0, tMin, tMax)
  }

}

object AxisAlignedBoundingBox {
  def apply(min: Vec3, max: Vec3) = new AxisAlignedBoundingBox(min, max)

  def surroundingBox(box0: AxisAlignedBoundingBox, box1: AxisAlignedBoundingBox): AxisAlignedBoundingBox = {
    val min = Vec3(
      x = minD(box0.min.x, box1.min.x),
      y = minD(box0.min.y, box1.min.y),
      z = minD(box0.min.z, box1.min.z)
    )

    val max = Vec3(
      x = maxD(box0.max.x, box1.max.x),
      y = maxD(box0.max.y, box1.max.y),
      z = maxD(box0.max.z, box1.max.z)
    )

    AxisAlignedBoundingBox(min, max)
  }

  def minD(a: Double, b: Double): Double = if (a < b) a else b
  def maxD(a: Double, b: Double): Double = if (a > b) a else b
}
