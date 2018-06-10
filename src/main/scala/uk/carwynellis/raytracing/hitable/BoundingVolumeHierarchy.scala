package uk.carwynellis.raytracing.hitable
import uk.carwynellis.raytracing.{AxisAlignedBoundingBox, HitRecord, Ray}

// TODO - review the node definition here
//      - do we need node as the whole list or can we select the centre of the list as the node?
class BoundingVolumeHierarchy(val node: List[Hitable],
                              val left: Option[Hitable],
                              val right: Option[Hitable],
                              val box: AxisAlignedBoundingBox,
                              val time0: Double,
                              val time1: Double) extends Hitable {

  // TODO - this is very slow - why?
  override def hit(ray: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
//    println("bvh.hit()")
    if (box.hit(ray, tMin, tMax)) {
      val hitLeft = left.flatMap(_.hit(ray, tMin, tMax))
      val hitRight = right.flatMap(_.hit(ray, tMin, tMax))
      (hitLeft, hitRight) match {
        case (Some(l), Some(r)) => if (l.t < r.t) Some(l) else Some(r)
        case (Some(l), None) => Some(l)
        case (None, Some(r)) => Some(r)
        case _ => None
      }
    }
    else None
  }

  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = Some(box)
}

object BoundingVolumeHierarchy {

  def apply(hitables: List[Hitable], left: Option[Hitable], right: Option[Hitable], boundingBox: AxisAlignedBoundingBox, time0: Double, time1: Double) =
    new BoundingVolumeHierarchy(hitables, left, right, boundingBox, time0, time1)

  // TODO - review this and see if it can be simplified
  def ofHitables(hitables: List[Hitable], time0: Double, time1: Double): BoundingVolumeHierarchy = {

    val axis = (3 * math.random()).toInt

    val sortedHitables =
      if (axis == 0)  hitables.sortWith(compareXAxis)
      else if (axis == 1) hitables.sortWith(compareYAxis)
      else hitables.sortWith(compareZAxis)

    val (left: Option[Hitable], right: Option[Hitable]) = sortedHitables.size match {
      case 1 => (sortedHitables.headOption, sortedHitables.headOption)
      case 2 => (sortedHitables.headOption, sortedHitables.lastOption)
      case n =>
        val (hitablesLeft, hitablesRight) = sortedHitables.splitAt(n / 2)
        (
          Some(ofHitables(hitablesLeft, time0, time1)),
          Some(ofHitables(hitablesRight, time0, time1))
        )
    }

    val result = for {
      l <- left
      r <- right
      bl <- l.boundingBox(time0, time1)
      br <- r.boundingBox(time0, time1)
      box = AxisAlignedBoundingBox.surroundingBox(bl, br)
    } yield box

    val boundingBox = result match {
      case None => throw new IllegalArgumentException("No bounding box found during construction from list of Hitables")
      case Some(b) => b
    }

    new BoundingVolumeHierarchy(sortedHitables, left, right, boundingBox, time0, time1)
  }

  // TODO - refactor the following to apply some DRY

  private def compareXAxis(l: Hitable, r: Hitable) = {
    val boxLeft = l.boundingBox(0, 0)
    val boxRight = r.boundingBox(0, 0)

    val result = for {
      bl <- boxLeft
      br <- boxRight
      lX = bl.min.x
      rX = br.min.x
    } yield (lX - rX) < 0.0

    result match {
      case Some(b) => b
      // TODO - nicer way to handle this? Using exception for now for expediency
      case None => throw new IllegalArgumentException("No bounding box for either hitable when comparing")
    }
  }

  private def compareYAxis(l: Hitable, r: Hitable) = {
    val boxLeft = l.boundingBox(0, 0)
    val boxRight = r.boundingBox(0, 0)

    val result = for {
      bl <- boxLeft
      br <- boxRight
      lY = bl.min.y
      rY = br.min.y
    } yield (lY - rY) < 0.0

    result match {
      case Some(b) => b
      // TODO - nicer way to handle this? Using exception for now for expediency
      case None => throw new IllegalArgumentException("No bounding box for either hitable when comparing")
    }
  }

  private def compareZAxis(l: Hitable, r: Hitable) = {
    val boxLeft = l.boundingBox(0, 0)
    val boxRight = r.boundingBox(0, 0)

    val result = for {
      bl <- boxLeft
      br <- boxRight
      lZ = bl.min.z
      rZ = br.min.z
    } yield (lZ - rZ) < 0.0

    result match {
      case Some(b) => b
      // TODO - nicer way to handle this? Using exception for now for expediency
      case None => throw new IllegalArgumentException("No bounding box for either hitable when comparing")
    }

  }

}
