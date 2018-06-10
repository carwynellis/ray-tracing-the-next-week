package uk.carwynellis.raytracing.hitable

import org.scalatest.{FunSuite, Matchers}
import uk.carwynellis.raytracing.{Lambertian, Vec3}

class BoundingVolumeHierarchyTest extends FunSuite with Matchers {

  private val sphere = Sphere(Vec3(0,0,0), 1.0, Lambertian(Vec3(0, 0, 0)))
  private val anotherSphere = Sphere(Vec3(1,2,3), 5.0, Lambertian(Vec3(4, 4, 4)))

  test("should build a node with both child nodes set to the single element for a list of a single hitable") {
    val hitables = List(sphere)

    val result = BoundingVolumeHierarchy.ofHitables(hitables, 0, 0)

    result.left shouldBe None
    result.right shouldBe None
  }

  test("should build a single node with left node set to first element and right node set to second for list of two hitables") {
    val hitables = List(sphere, anotherSphere)

    val result = BoundingVolumeHierarchy.ofHitables(hitables, 0, 0)

    result.left shouldBe Some(sphere)
    result.right shouldBe Some(anotherSphere)
  }

  test("should build a tree from a list of hitables") {
    val hitables = List(sphere, sphere, sphere, sphere, sphere)

    val result = BoundingVolumeHierarchy.ofHitables(hitables, 0, 0)

    // TODO - inspect result properly - this is a quick cludge to check the recursion terminates correctly
    result.left.isDefined shouldBe true
    result.right.isDefined shouldBe true
  }

}
