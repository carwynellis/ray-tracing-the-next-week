package uk.carwynellis.raytracing.texture

import uk.carwynellis.raytracing.Vec3

trait Texture {

  def value(u: Double, v: Double): Vec3

}