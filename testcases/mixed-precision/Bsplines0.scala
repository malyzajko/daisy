
import daisy.lang._
import Real._


object Bsplines0 {

  // f32
  def bspline0_32(u: Real): Real = {
    require(0 <= u && u <= 1)
    (1 - u) * (1 - u) * (1 - u) / 6.0
  } ensuring(res => res +/- 9e-8)

  // 0.5 f32
  def bspline0_32_05(u: Real): Real = {
    require(0 <= u && u <= 1)
    (1 - u) * (1 - u) * (1 - u) / 6.0
  } ensuring(res => res +/- 4.5e-8)

  // 0.1 f32
  def bspline0_32_01(u: Real): Real = {
    require(0 <= u && u <= 1)
    (1 - u) * (1 - u) * (1 - u) / 6.0
  } ensuring(res => res +/- 9e-9)

  // 0.01 f32
  def bspline0_32_001(u: Real): Real = {
    require(0 <= u && u <= 1)
    (1 - u) * (1 - u) * (1 - u) / 6.0
  } ensuring(res => res +/- 9e-10)

  // f64
  def bspline0_64(u: Real): Real = {
    require(0 <= u && u <= 1)
    (1 - u) * (1 - u) * (1 - u) / 6.0
  } ensuring(res => res +/- 2e-16)

  // 0.5 f64
  def bspline0_64_05(u: Real): Real = {
    require(0 <= u && u <= 1)
    (1 - u) * (1 - u) * (1 - u) / 6.0
  } ensuring(res => res +/- 1e-16)

  // 0.1 f64
  def bspline0_64_01(u: Real): Real = {
    require(0 <= u && u <= 1)
    (1 - u) * (1 - u) * (1 - u) / 6.0
  } ensuring(res => res +/- 2e-17)

  // 0.01 f64
  def bspline0_64_001(u: Real): Real = {
    require(0 <= u && u <= 1)
    (1 - u) * (1 - u) * (1 - u) / 6.0
  } ensuring(res => res +/- 2e-18)

  // dbldbl
  def bspline0_dbldbl(u: Real): Real = {
    require(0 <= u && u <= 1)
    (1 - u) * (1 - u) * (1 - u) / 6.0
  } ensuring(res => res +/- 4e-32)

}
