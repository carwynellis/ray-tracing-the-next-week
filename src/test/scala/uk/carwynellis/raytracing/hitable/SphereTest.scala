package uk.carwynellis.raytracing.hitable

import org.scalatest.FunSuite
import uk.carwynellis.raytracing.{Ray, Vec3}
import uk.carwynellis.raytracing.material.Dielectric

class SphereTest extends FunSuite {

  test("should compute hit in reasonable time") {
    val sphere = Sphere(randomVec3, 10.0, new Dielectric(1))

    val start = System.currentTimeMillis()
    val res = (0 until 1000).map { i =>
      sphere.hit(Ray(randomVec3, randomVec3, math.random()), math.random(), math.random())
    }
    val end = System.currentTimeMillis()
    println(s"runtime: ${end-start} ms")
    println(res.size)
  }

  private def randomVec3 = Vec3(math.random(), math.random(), math.random())

}
