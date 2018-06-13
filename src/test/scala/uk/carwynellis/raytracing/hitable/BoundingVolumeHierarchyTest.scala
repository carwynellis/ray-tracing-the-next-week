package uk.carwynellis.raytracing.hitable

import org.scalatest.{FunSuite, Matchers}
import uk.carwynellis.raytracing.Vec3
import uk.carwynellis.raytracing.material.Lambertian
import uk.carwynellis.raytracing.texture.ConstantTexture

class BoundingVolumeHierarchyTest extends FunSuite with Matchers {

  private val sphere = Sphere(Vec3(0,0,0), 1.0, Lambertian(ConstantTexture(Vec3(0, 0, 0))))
  private val anotherSphere = Sphere(Vec3(1,2,3), 5.0, Lambertian(ConstantTexture(Vec3(4, 4, 4))))

  test("should build a node with both child nodes set to the single element for a list of a single hitable") {
    val hitables = List(sphere)

    val result = BoundingVolumeHierarchy.ofHitables(hitables, 0, 0)

    result.left shouldBe sphere
    result.right shouldBe sphere
  }

  test("should build a single node with left node set to first element and right node set to second for list of two hitables") {
    val hitables = List(sphere, anotherSphere)

    val result = BoundingVolumeHierarchy.ofHitables(hitables, 0, 0)

    result.left shouldBe anotherSphere
    result.right shouldBe sphere
  }

  test("should build a tree from a list of hitables") {
    val hitables = List(sphere, sphere, sphere, sphere, sphere)

    val result = BoundingVolumeHierarchy.ofHitables(hitables, 0, 0)

    // TODO - inspect the result - this is a cludge to check that the recursion terminates correctly
  }

}
