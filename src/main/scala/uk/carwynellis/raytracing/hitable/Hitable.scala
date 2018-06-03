package uk.carwynellis.raytracing.hitable

import uk.carwynellis.raytracing._

// TODO - better name for this trait?
trait Hitable {

  def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord]

}



