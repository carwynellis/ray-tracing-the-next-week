package uk.carwynellis.raytracing.hitable.transform

import uk.carwynellis.raytracing.{AxisAlignedBoundingBox, HitRecord, Ray, Vec3}
import uk.carwynellis.raytracing.hitable.Hitable

class RotateY(p: Hitable, angle: Double) extends Hitable {

  private val angleRadians = (math.Pi / 180) * angle

  private val sinTheta = math.sin(angleRadians)
  private val cosTheta = math.cos(angleRadians)

  private val boundingBox = p.boundingBox(0, 1).map { box =>
      // TODO - can this be expressed without vars?
      var min = Vec3(Double.MaxValue, Double.MaxValue, Double.MaxValue)
      var max = Vec3(Double.MinValue, Double.MinValue, Double.MinValue)
      (0 until 2) foreach { i: Int =>
        (0 until 2) foreach { j: Int =>
          (0 until 2) foreach { k: Int =>
            val x = i * box.max.x + (1 - i) * box.min.x
            val y = j * box.max.y + (1 - j) * box.min.y
            val z = k * box.max.z + (1 - k) * box.min.z
            val newX = cosTheta * x + sinTheta * z
            val newZ = -sinTheta * x + cosTheta * z
            val tester = Vec3(newX, y, newZ)
            (0 until 2) foreach { c =>
              if (tester.get(c) > max.get(c)) {
                // TODO - provide a method on Vec3....
                max = c match {
                  case 0 => max.copy(x = tester.x)
                  case 1 => max.copy(y = tester.y)
                  case 2 => max.copy(z = tester.z)
                  case _ => throw new IllegalArgumentException(s"Index $c out of bounds")
                }
              }
              if (tester.get(c) < min.get(c)) {
                // TODO - provide a method on Vec3....
                min = c match {
                  case 0 => min.copy(x = tester.x)
                  case 1 => min.copy(y = tester.y)
                  case 2 => min.copy(z = tester.z)
                  case _ => throw new IllegalArgumentException(s"Index $c out of bounds")
                }
              }
            }
          }
        }
      }
    AxisAlignedBoundingBox(min, max)
  }

  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    val origin = r.origin.copy(
      x = cosTheta * r.origin.x - sinTheta * r.origin.z,
      z = sinTheta * r.origin.x + cosTheta * r.origin.z
    )
    val direction = r.direction.copy(
      x = cosTheta * r.direction.x - sinTheta * r.direction.z,
      z = sinTheta * r.direction.x + cosTheta * r.direction.z
    )

    val rotatedRay = Ray(origin, direction, r.time)

    p.hit(rotatedRay, tMin, tMax) map { hit =>
      val p = hit.p.copy(
        x = cosTheta * hit.p.x + sinTheta * hit.p.z,
        z = -sinTheta * hit.p.x + cosTheta * hit.p.z
      )
      val normal = hit.normal.copy(
        x = cosTheta * hit.normal.x + sinTheta * hit.normal.z,
        z = -sinTheta * hit.normal.x + cosTheta * hit.normal.z
      )
      hit.copy(p = p, normal = normal)
    }
  }

  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = boundingBox

}

object RotateY {
  def apply(p: Hitable, angle: Double) = new RotateY(p, angle)
}