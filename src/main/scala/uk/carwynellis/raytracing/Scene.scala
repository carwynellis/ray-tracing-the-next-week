package uk.carwynellis.raytracing

import uk.carwynellis.raytracing.hitable._
import uk.carwynellis.raytracing.material.{Dielectric, DiffuseLight, Lambertian, Metal}
import uk.carwynellis.raytracing.texture.{CheckerBoard, ConstantTexture, ImageTexture, NoiseTexture}

// TODO - replace List with Seq

object Scene {

  val staticScene = HitableList(List(
    Sphere(Vec3(0, 0, -1), 0.5, Lambertian(ConstantTexture(Vec3(0.5, 0.5, 0.6)))),
    Sphere(Vec3(0, -100.5, -1), 100, Lambertian(ConstantTexture(Vec3(0.8, 0.8, 0.0)))),
    Sphere(Vec3(1, 0, -1), 0.5, Metal(ConstantTexture(Vec3(0.8, 0.6, 0.2)), 0.3)),
    Sphere(Vec3(-1, 0, -1), -0.5, Dielectric(1.5))
  ))

  val twoPerlinSpheres = HitableList(List(
    Sphere(Vec3(0, -1000, 0), 1000, Lambertian(NoiseTexture(1))),
    Sphere(Vec3(0, 2, 0), 2, Lambertian(NoiseTexture(20))),
  ))

  val imagePath = "/Users/carwyn/Downloads/earthmap.jpg"

  val perlinAndImageSpheres = HitableList(List(
    Sphere(Vec3(0, -1000, 0), 1000, Lambertian(NoiseTexture(1))),
    Sphere(Vec3(0, 2, 0), 2, Lambertian(ImageTexture(imagePath)))
  ))

  val perlinAndLight = HitableList(List(
    Sphere(Vec3(0, -1000, 0), 1000, Lambertian(NoiseTexture(1))),
    Sphere(Vec3(0, 2, 0), 2, DiffuseLight(ConstantTexture(Vec3(4, 4, 4))))
  ))

  val simpleLightScene = HitableList(List(
    Sphere(Vec3(0, -1000, 0), 1000, Lambertian(NoiseTexture(4))),
    Sphere(Vec3(0, 2, 0), 2, Lambertian(NoiseTexture(4))),
    SimpleRectangle(3, 5, 1, 3, -2, DiffuseLight(ConstantTexture(Vec3(4, 4, 4))))
  ))

  def randomScene(): HitableList = {

    val lowerBound = -11
    val upperBound = 11

    val range = lowerBound until upperBound

    // TODO - refactor - this is a rough port of the C++ code
    def generateSpheres: List[Hitable] = range.flatMap { a =>
      range.flatMap { b =>
        val materialSelector = math.random()

        val centre = Vec3(
          x = a + 0.9 * math.random(),
          y = 0.2,
          z = b + 0.9 * math.random()
        )
        generateSphere(centre, materialSelector)
      }
    }.toList

    // TODO - refactor as above
    def generateSphere(c: Vec3, m: Double): Option[Hitable] = if ((c - Vec3(4, 0.2, 0)).length > 0.9) {
      if (m < 0.8)
        Some(MovingSphere(
          centre0 = c,
          centre1 = c + Vec3(0, math.random() * 0.5, 0),
          radius = 0.2,
          time0 = 0.0,
          time1 = 1.0,
          material = Lambertian(ConstantTexture(Vec3(math.random(), math.random(), math.random())))
        ))
      else if (m < 0.95) {
        def randomColor = 0.5 * (1 + math.random())
        Some(Sphere(c, 0.2,
          Metal(
            ConstantTexture(Vec3(randomColor, randomColor, randomColor)),
            0.5 * math.random()
          )
        ))
      }
      else Some(Sphere(c, 0.2, Dielectric(1.5)))
    }
    else None

    val checkerboard = CheckerBoard(
      odd = ConstantTexture(Vec3(0.1, 0.1, 0.3)),
      even = ConstantTexture(Vec3(0.9, 0.9, 0.9))
    )

    val scene = List(
      Sphere(Vec3(0, -1000, 0), 1000, Lambertian(checkerboard)),
      Sphere(Vec3(0, 1, 0), 1, Dielectric(1.5)),
      Sphere(Vec3(-4, 1, 0), 1, Lambertian(ConstantTexture(Vec3(0.4, 0.2, 0.1)))),
      Sphere(Vec3(4, 1, 0), 1, Metal(ConstantTexture(Vec3(0.7, 0.6, 0.5)), 0))
    ) ++ generateSpheres
    HitableList(scene)
  }

}
