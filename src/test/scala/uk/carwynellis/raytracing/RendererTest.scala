package uk.carwynellis.raytracing

import org.scalatest.{FunSuite, Matchers}
import uk.carwynellis.raytracing.hitable._
import uk.carwynellis.raytracing.material.{DiffuseLight, Lambertian}
import uk.carwynellis.raytracing.texture.ConstantTexture

// TODO - define a similar case that exercises the color method
//      - confirm that the iteration is working as expected (doesn't seem to)
class RendererTest extends FunSuite with Matchers {

  // Test that replicates the generation of component values exceeding 255.
  // This seems to be caused by having values in the light exceed 1
  // TODO - is this accounted for in the code somehow? It doesn't seem to be in mine! :/
  test("renderer should not generate illegal pixel component values") {
    val width = 600
    val height = 400
    val samples = 4

    val origin = Vec3(278, 278, -800)
    val target = Vec3(278, 278, 0)

    val time0 = 0.0
    val time1 = 0.2

    val camera = Camera(
      origin = origin,
      target = target,
      upVector = Vec3(0, 1, 0),
      verticalFieldOfView = 40,
      aspectRatio =  width.toDouble / height.toDouble,
      aperture = 0.0,
      focusDistance = 10,
      time0 = time0,
      time1 = time1
    )

    // Replicate scene generating bad values.
    val red = Lambertian(ConstantTexture(Vec3(0.65, 0.05, 0.05)))
    val light = DiffuseLight(ConstantTexture(Vec3(15, 15, 15)))

    val failingScene = HitableList(List(
      YZRectangle(0, 555, 0, 555, 0, red), // Right wall
      XZRectangle(213, 343, 227, 332, 554, light),
    ))

    val renderer = Renderer(camera, failingScene, width, height, samples)

    // This pixel is known to generate values out of bounds.
    val pixel = renderer.renderPixel(265, 347)

    pixel.r should be < 256
    pixel.g should be < 256
    pixel.b should be < 256
  }

  // Test case to try and isolate issues with the Cornell box render that isn't quite right.
  test("renderer should render light sources correctly") {
    val width = 80
    val height = 60
    val samples = 5000

    val origin = Vec3(278, 278, -0.0001)
    val target = Vec3(278, 278, 0)

    val time0 = 0.0
    val time1 = 0.2

    val camera = Camera(
      origin = origin,
      target = target,
      upVector = Vec3(0, 1, 0),
      verticalFieldOfView = 40,
      aspectRatio =  width.toDouble / height.toDouble,
      aperture = 0.0,
      focusDistance = 10,
      time0 = time0,
      time1 = time1
    )

    val red = Lambertian(ConstantTexture(Vec3(0.65, 0.05, 0.05)))
    val white = Lambertian(ConstantTexture(Vec3(0.73, 0.73, 0.73)))
    val light = DiffuseLight(ConstantTexture(Vec3(15, 15, 15)))

    val scene = HitableList(List(
      YZRectangle(0, 555, 0, 555, 0, red), // Right wall
      XZRectangle(213, 343, 227, 332, 554, light),
      FlipNormals(XYRectangle(0, 555, 0, 555, 555, white))
    ))

    val renderer = Renderer(camera, scene, width, height, samples)

    // TODO - debug this - the noLight pixel should be illuminated...
    val noLight = renderer.renderPixel(29, height-1)
    noLight.r should be > 48 // 48 corresponds to the intensity of a dark section of the rectangle
    println(noLight)

    val illuminated = renderer.renderPixel(32, height-1)
    println(illuminated)

  }

}
