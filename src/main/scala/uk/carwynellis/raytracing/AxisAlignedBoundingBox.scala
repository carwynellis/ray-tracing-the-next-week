package uk.carwynellis.raytracing

/**
  * Defines an axis aligned bounding box (AABB) around a set of objects.
  *
  * Used to rapidly exclude rays that will never hit an object.
  *
  * @param min
  * @param max
  */
class AxisAlignedBoundingBox(min: Vec3, max: Vec3) {

  // TODO - can this index based algorithm be expressed differently?
  def hit(ray: Ray): Boolean = {
    // TODO - this is a straight port of the existing method that uses mutable state.
    //      - consider a refactor once tests are in place
    var tMin = 0.0
    var tMax = 0.0

    (0 until 3) foreach { i =>
      val iMin = (min.get(i) - ray.origin.get(i)) / ray.direction.get(i)
      val iMax = (max.get(i) - ray.origin.get(i)) / ray.direction.get(i)

      val t0 = minD(iMin, iMax)
      val t1 = maxD(iMin, iMax)

      tMin = minD(t0, t1)
      tMax = maxD(t0, t1)

      if (tMax <= tMin) return false
    }

    true
  }

  // TODO - are these any faster than math.min / math.max?
  private def minD(a: Double, b: Double) = if (a < b) a else b
  private def maxD(a: Double, b: Double) = if (a > b) a else b

}

object AxisAlignedBoundingBox {
  def apply(min: Vec3, max: Vec3) = new AxisAlignedBoundingBox(min, max)
}
