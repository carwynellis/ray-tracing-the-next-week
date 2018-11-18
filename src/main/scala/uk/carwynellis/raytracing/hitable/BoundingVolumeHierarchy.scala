package uk.carwynellis.raytracing.hitable
import uk.carwynellis.raytracing.{AxisAlignedBoundingBox, HitRecord, Random, Ray}

class BoundingVolumeHierarchy(val left: Option[Hitable],
                              val right: Option[Hitable],
                              val box: Option[AxisAlignedBoundingBox],
                              val time0: Double,
                              val time1: Double) extends Hitable {

  /**
    * Compute whether a ray hits any of the hitables within the BVH.
    *
    * Originally this was implemented as a pattern match on the tuple (leftHit, rightHit) but replacing this with the
    * ugly if syntax below yielded a slight reduction in render times.
    *
    * @param ray
    * @param tMin
    * @param tMax
    * @return
    */
  override def hit(ray: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    // TODO - review this and tidy up
    box.flatMap { b =>
      if (b.hit(ray, tMin, tMax)) {
        val leftHit = left.flatMap(_.hit(ray, tMin,tMax))
        val rightHit = right.flatMap(_.hit(ray, tMin, tMax))

        leftHit.map { lh =>
          rightHit.fold(lh) { rh => if (lh.t < rh.t) lh else rh }
        }.orElse(rightHit)
      }
      else None
    }
  }

  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = box
}

object BoundingVolumeHierarchy {

  def apply(left: Option[Hitable], right: Option[Hitable], boundingBox: Option[AxisAlignedBoundingBox], time0: Double, time1: Double) =
    new BoundingVolumeHierarchy(left, right, boundingBox, time0, time1)

  // TODO - review this and see if it can be simplified
  def ofHitables(hitables: List[Hitable], time0: Double, time1: Double): BoundingVolumeHierarchy = {

    val axis = (3 * Random.double).toInt

    val sortedHitables =
      if (axis == 0)  hitables.sortWith(compareXAxis)
      else if (axis == 1) hitables.sortWith(compareYAxis)
      else hitables.sortWith(compareZAxis)

    val (left: Option[Hitable], right: Option[Hitable]) = sortedHitables.size match {
      case 1 => (sortedHitables.headOption, None)
      case 2 => (sortedHitables.headOption, sortedHitables.lastOption)
      case n =>
        val (hitablesLeft, hitablesRight) = sortedHitables.splitAt(n / 2)
        val leftNode = ofHitables(hitablesLeft, time0, time1)
        val rightNode = ofHitables(hitablesRight, time0, time1)
        (Some(leftNode), Some(rightNode))
    }

    val boundingBox = (left.flatMap(_.boundingBox(time0, time1)), right.flatMap(_.boundingBox(time0, time1))) match {
      case (Some(lb), Some(rb)) => Some(AxisAlignedBoundingBox.surroundingBox(lb, rb))
      case (lb, None) => lb
      case (None, rb) => rb
      case _ => None
    }

    new BoundingVolumeHierarchy(left, right, boundingBox, time0, time1)
  }

  private def compareAxis(l: Hitable,
                          r: Hitable,
                          f: (AxisAlignedBoundingBox, AxisAlignedBoundingBox) => (Double, Double)): Boolean = {
    val result = for {
      bl <- l.boundingBox(0, 0)
      br <- r.boundingBox(0, 0)
      bounds <- Some(f(bl, br)) // We could define a lift here to pull the result up into an Option but it seems like overkill.
    } yield (bounds._1 - bounds._2) < 0.0

    result.fold(false)(b => b)
  }

  private def compareXAxis(l: Hitable, r: Hitable) = compareAxis(l, r, { (bl, br) => (bl.min.x, br.min.x)} )
  private def compareYAxis(l: Hitable, r: Hitable) = compareAxis(l, r, { (bl, br) => (bl.min.y, br.min.y)} )
  private def compareZAxis(l: Hitable, r: Hitable) = compareAxis(l, r, { (bl, br) => (bl.min.z, br.min.z)} )

}
