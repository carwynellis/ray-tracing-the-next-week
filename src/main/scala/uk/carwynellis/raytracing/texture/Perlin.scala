package uk.carwynellis.raytracing.texture

import uk.carwynellis.raytracing.Vec3

import scala.annotation.tailrec
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

    val c = ArrayBuffer.fill(2, 2, 2)(Vec3(0, 0, 0))

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

  def turbulence(p: Vec3, iterations: Int = 7): Double = {
    @tailrec
    def loop(acc: Double, weight: Double, q: Vec3, count: Int): Double = {
      if (count >= iterations) acc
      else loop(acc + weight * noise(q), weight * 0.5, q * 2, count + 1)
    }

    loop(0.0, 1.0, p, 0)
  }

  private def generateNoise() =
    Seq.fill(256)(0.0).map(_ => Vec3(-1 + 2 * math.random(), -1 + 2 * math.random, -1 + 2 * math.random()).unitVector)

  private def generateIndexPermutation(): IndexedSeq[Int] = util.Random.shuffle((0 until 256).toList).toIndexedSeq

  // TODO - better way to express this in scala - it's more or less a straight port of the original C code
  // TODO - a for comp might do this quite nicely
  private def triLinearInterpolation(c: IndexedSeq[IndexedSeq[IndexedSeq[Vec3]]],
                                     u: Double, v: Double, w: Double) = {

    var accumulator = 0.0

    // Use hermite cubic to smooth interpolation results.
    val uu = u * u * (3 - 2 * u)
    val vv = v * v * (3 - 2 * v)
    val ww = w * w * (3 - 2 * w)

    (0 until 2).foreach { i =>
      (0 until 2).foreach { j =>
        (0 until 2).foreach { k =>
          val weight = Vec3(u-i, v-j, w-k)
          accumulator = accumulator +
            ((i * uu + ((1 - i) * (1 - uu))) *
            (j * vv + ((1 - j) * (1 - vv))) *
            (k * ww + ((1 - k) * (1 - ww))) *
            c(i)(j)(k).dot(weight))
        }
      }
    }

    accumulator
  }

}
