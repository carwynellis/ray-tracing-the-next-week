package uk.carwynellis.raytracing.hitable

import uk.carwynellis.raytracing.{HitRecord, Ray}

import scala.annotation.tailrec

class HitableList(val hitables: List[Hitable]) extends Hitable {

  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {

    @tailrec
    def loop(hs: List[Hitable], closest: Double, hitAnything: Boolean, record: Option[HitRecord]): Option[HitRecord] = {
      hs match {
        case x :: xs =>
          val hitResult = x.hit(r, tMin, closest)
          hitResult match {
            case Some(updatedRecord) => loop(xs, updatedRecord.t, hitAnything = true, Some(updatedRecord))
            case None => loop(xs, closest, hitAnything = hitAnything, record)
          }
        case Nil => record
      }
    }

    loop(hitables, closest = tMax, hitAnything = false, None)
  }

}

object HitableList {
  def apply(hitables: List[Hitable]) = new HitableList(hitables)
}
