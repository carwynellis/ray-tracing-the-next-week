package uk.carwynellis.raytracing

import uk.carwynellis.raytracing.material.Material

case class HitRecord(t: Double, p: Vec3, normal: Vec3, material: Material)
