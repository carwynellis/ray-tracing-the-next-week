package uk.carwynellis.raytracing.hitable
import uk.carwynellis.raytracing.{AxisAlignedBoundingBox, HitRecord, Ray}

class BoundingVolumeHierarchy(val node: Hitable,
                              val left: Option[Hitable],
                              val right: Option[Hitable],
                              val n: Int, // TODO - is this necessary?
                              val time0: Double,
                              val time1: Double) extends Hitable {

  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    // TODO - which tiem values do we use here??
    val box = boundingBox(time0, time1)

    box.flatMap { b =>
      if (b.hit(r)) {
        val hitLeft = left.flatMap(_.hit(r, tMin, tMax))
        val hitRight = right.flatMap(_.hit(r, tMin, tMax))
        (hitLeft, hitRight) match {
          case (Some(l), Some(r)) => if (l.t < r.t) Some(l) else Some(r)
          case (Some(l), None) => Some(l)
          case (None, Some(r)) => Some(r)
          case _ => None
        }
      }
      else None
    }
  }

  // TODO - we just delegate to the bounding box method on the current hitable. Is this correct? The sample code is
  //        somewhat opaque on this.
  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = node.boundingBox(t0, t1)

}
