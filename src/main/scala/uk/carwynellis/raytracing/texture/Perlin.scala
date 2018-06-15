package uk.carwynellis.raytracing.texture

import uk.carwynellis.raytracing.Vec3

// TODO - move into noise texture?
class Perlin {

  private val xPermutations = generateIndexPermutation()
  private val yPermutations = generateIndexPermutation()
  private val zPermutations = generateIndexPermutation()

  private val randomValues = generateNoise()

  def noise(p: Vec3): Double = {
    val i = (4 * p.x).toInt & 255
    val j = (4 * p.y).toInt & 255
    val k = (4 * p.z).toInt & 255

    randomValues(xPermutations(i) ^ yPermutations(j) ^ zPermutations(k))
  }

  private def generateNoise() = Seq.fill(256)(0.0).map(_ => math.random())

  private def generateIndexPermutation(): IndexedSeq[Int] = util.Random.shuffle((0 until 256).toList).toIndexedSeq

}
