package uk.carwynellis.raytracing.texture

import uk.carwynellis.raytracing.Vec3

import scala.collection.mutable.ArrayBuffer

// TODO - move into noise texture?
class Perlin {

  private val xPermutations = generateIndexPermutation()
  private val yPermutations = generateIndexPermutation()
  private val zPermutations = generateIndexPermutation()

  private val randomValues = generateNoise()

  def noise(p: Vec3): Double = {
    val u = p.x - math.floor(p.x)
    val v = p.y - math.floor(p.y)
    val w = p.z - math.floor(p.z)

    val i = math.floor(p.x).toInt
    val j = math.floor(p.y).toInt
    val k = math.floor(p.z).toInt

    val c = ArrayBuffer.fill(2, 2, 2)(0.0)

    (0 until 2).foreach { di =>
      (0 until 2).foreach { dj =>
        (0 until 2).foreach { dk =>
          c(di)(dj)(dk) =
            randomValues(xPermutations((i + di) & 255) ^ yPermutations((j + dj) & 255) ^ zPermutations((k + dk) & 255))
        }
      }
    }

    triLinearInterpolation(c, u, v, w)
  }

  private def generateNoise() = Seq.fill(256)(0.0).map(_ => math.random())

  private def generateIndexPermutation(): IndexedSeq[Int] = util.Random.shuffle((0 until 256).toList).toIndexedSeq

  // TODO - better way to express this in scala - it's more or less a straight port of the original C code
  // TODO - a for comp might do this quite nicely
  private def triLinearInterpolation(c: IndexedSeq[IndexedSeq[IndexedSeq[Double]]],
                                     u: Double, v: Double, w: Double) = {

    var accumulator = 0.0

    (0 until 2).foreach { i =>
      (0 until 2).foreach { j =>
        (0 until 2).foreach { k =>
          accumulator = accumulator +
            ((i * u + ((1 - i) * (1 - u))) *
            (j * v + ((1 - j) * (1 - v))) *
            (k * w + ((1 - k) * (1 - w))) *
            c(i)(j)(k))
        }
      }
    }

    accumulator
  }

}
