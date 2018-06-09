package uk.carwynellis.raytracing.hitable
import uk.carwynellis.raytracing.{AxisAlignedBoundingBox, HitRecord, Ray}

class BoundingVolumeHierarchy(val node: Hitable,
                              val left: Option[Hitable],
                              val right: Option[Hitable],
                              val box: AxisAlignedBoundingBox,
                              val time0: Double,
                              val time1: Double) extends Hitable {

  override def hit(ray: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    // TODO - which time values do we use here??
    val box = boundingBox(time0, time1)

    box.flatMap { b =>
      if (b.hit(ray)) {
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
  }

  // TODO - we just delegate to the bounding box method on the current hitable. Is this correct? The sample code is
  //        somewhat opaque on this.
  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = node.boundingBox(t0, t1)

}

object BoundingVolumeHierarchy {

  def ofHitables(hitables: List[Hitable], time0: Double, time1: Double): Unit = {

    // TODO - why do we select a random axis?
    val axis = (3 * math.random()).toInt

    val sortedHitables =
      if (axis == 0)  hitables.sortWith(compareXAxis)
      else if (axis == 1) hitables.sortWith(compareYAxis)
      else hitables.sortWith(compareZAxis)

    val (left: Hitable, right: Hitable) = hitables.size match {
      case 1 => (hitables.head, hitables.head)
      case 2 => (hitables.head, hitables.tail)
      case n =>
        val (hitablesLeft, hitablesRight) = hitables.splitAt(n / 2)
        (
          ofHitables(hitablesLeft, time0, time1),
          ofHitables(hitablesRight, time0, time1)
        )
    }

    val boxRight = right.boundingBox(time0, time1)

    val result = for {
      bl <- left.boundingBox(time0, time1)
      br <- right.boundingBox(time0, time1)
      box = AxisAlignedBoundingBox.surroundingBox(bl, br)
    } yield box

    val boundingBox = result match {
      case None => throw new IllegalArgumentException("No bounding box found during construction from list of Hitables")
      case Some(b) => b
    }

    hitables.size match {
      case 1 => new BoundingVolumeHierarchy(hitables.head, None, None, boundingBox, time0, time1)
      case 2 => ???
      case n => ???
    }
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
