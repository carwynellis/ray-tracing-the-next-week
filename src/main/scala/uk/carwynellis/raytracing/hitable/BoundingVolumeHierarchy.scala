package uk.carwynellis.raytracing.hitable
import uk.carwynellis.raytracing.{AxisAlignedBoundingBox, HitRecord, Random, Ray}

// TODO - review the node definition here
//      - do we need node as the whole list or can we select the centre of the list as the node?
class BoundingVolumeHierarchy(val node: List[Hitable],
                              val left: Hitable,
                              val right: Hitable,
                              val box: AxisAlignedBoundingBox,
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
    if (box.hit(ray, tMin, tMax)) {
      val leftHit = left.hit(ray, tMin,tMax)
      val rightHit = right.hit(ray, tMin, tMax)

      leftHit.map { lh =>
        rightHit.fold(lh) { rh => if (lh.t < rh.t) lh else rh }
      }.orElse(rightHit)
    }
    else None
  }

  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = Some(box)
}

object BoundingVolumeHierarchy {

  def apply(hitables: List[Hitable], left: Hitable, right: Hitable, boundingBox: AxisAlignedBoundingBox, time0: Double, time1: Double) =
    new BoundingVolumeHierarchy(hitables, left, right, boundingBox, time0, time1)

  // TODO - review this and see if it can be simplified
  def ofHitables(hitables: List[Hitable], time0: Double, time1: Double): BoundingVolumeHierarchy = {

    val axis = (3 * Random.double).toInt

    val sortedHitables =
      if (axis == 0)  hitables.sortWith(compareXAxis)
      else if (axis == 1) hitables.sortWith(compareYAxis)
      else hitables.sortWith(compareZAxis)

    val (left: Hitable, right: Hitable) = sortedHitables.size match {
      case 1 => (sortedHitables.head, sortedHitables.head)
      case 2 => (sortedHitables.head, sortedHitables.last)
      case n =>
        val (hitablesLeft, hitablesRight) = sortedHitables.splitAt(n / 2)
        val leftNode = ofHitables(hitablesLeft, time0, time1)
        val rightNode = ofHitables(hitablesRight, time0, time1)
        (leftNode, rightNode)
    }

    val result = for {
      bl <- left.boundingBox(time0, time1)
      br <- right.boundingBox(time0, time1)
    } yield AxisAlignedBoundingBox.surroundingBox(bl, br)

    val boundingBox = result match {
      case None => throw new IllegalArgumentException("No bounding box found during construction from list of Hitables")
      case Some(b) => b
    }

    new BoundingVolumeHierarchy(sortedHitables, left, right, boundingBox, time0, time1)
  }

  private def compareAxis(l: Hitable,
                          r: Hitable,
                          f: (AxisAlignedBoundingBox, AxisAlignedBoundingBox) => (Double, Double)): Boolean = {
    val result = for {
      bl <- l.boundingBox(0, 0)
      br <- r.boundingBox(0, 0)
      bounds <- Some(f(bl, br)) // We could define a lift here to pull the result up into an Option but it seems like overkill.
    } yield (bounds._1 - bounds._2) < 0.0

    result match {
      case Some(b) => b
      // TODO - nicer way to handle this? Using exception for now for expediency
      case None => throw new IllegalArgumentException("No bounding box for either hitable when comparing")
    }
  }

  private def compareXAxis(l: Hitable, r: Hitable) = compareAxis(l, r, { (bl, br) => (bl.min.x, br.min.x)} )
  private def compareYAxis(l: Hitable, r: Hitable) = compareAxis(l, r, { (bl, br) => (bl.min.y, br.min.y)} )
  private def compareZAxis(l: Hitable, r: Hitable) = compareAxis(l, r, { (bl, br) => (bl.min.z, br.min.z)} )

}
