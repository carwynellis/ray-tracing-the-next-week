package uk.carwynellis.raytracing.hitable
import uk.carwynellis.raytracing.{AxisAlignedBoundingBox, HitRecord, Ray}

// TODO - review the node definition here
//      - do we need node as the whole list or can we select the centre of the list as the node?
class BoundingVolumeHierarchy(val node: List[Hitable],
                              val left: Hitable,
                              val right: Hitable,
                              val box: AxisAlignedBoundingBox,
                              val time0: Double,
                              val time1: Double) extends Hitable {

  override def hit(ray: Ray, tMin: Double, tMax: Double): Option[HitRecord] =
    if (box.hit(ray, tMin, tMax)) {
      (left.hit(ray, tMin, tMax), right.hit(ray, tMin, tMax)) match {
        case (Some(l), Some(r)) => if (l.t < r.t) Some(l) else Some(r)
        case (Some(l), None) => Some(l)
        case (None, Some(r)) => Some(r)
        case _ => None
      }
    }
    else None

  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = Some(box)
}

object BoundingVolumeHierarchy {

  def apply(hitables: List[Hitable], left: Hitable, right: Hitable, boundingBox: AxisAlignedBoundingBox, time0: Double, time1: Double) =
    new BoundingVolumeHierarchy(hitables, left, right, boundingBox, time0, time1)

  // TODO - review this and see if it can be simplified
  def ofHitables(hitables: List[Hitable], time0: Double, time1: Double): BoundingVolumeHierarchy = {

    val axis = (3 * math.random()).toInt

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
