
import daisy.lang._
import Real._


object Bsplines2 {

  // f32
  def bspline2_32(u: Real): Real = {
    require(0 <= u && u <= 1)
    (-3 * u*u*u  + 3*u*u + 3*u + 1) / 6.0
  } ensuring(res => res +/- 3.5e-7)

  // 0.5 f32
  def bspline2_32_05(u: Real): Real = {
    require(0 <= u && u <= 1)
    (-3 * u*u*u  + 3*u*u + 3*u + 1) / 6.0
  } ensuring(res => res +/- 1.75e-7)

  // 0.1 f32
  def bspline2_32_01(u: Real): Real = {
    require(0 <= u && u <= 1)
    (-3 * u*u*u  + 3*u*u + 3*u + 1) / 6.0
  } ensuring(res => res +/- 3.5e-8)

  // 0.01 f32
  def bspline2_32_001(u: Real): Real = {
    require(0 <= u && u <= 1)
    (-3 * u*u*u  + 3*u*u + 3*u + 1) / 6.0
  } ensuring(res => res +/- 3.5e-9)

  // f64
  def bspline2_64(u: Real): Real = {
    require(0 <= u && u <= 1)
    (-3 * u*u*u  + 3*u*u + 3*u + 1) / 6.0
  } ensuring(res => res +/- 6.5e-16)

  // 0.5 f64
  def bspline2_64_05(u: Real): Real = {
    require(0 <= u && u <= 1)
    (-3 * u*u*u  + 3*u*u + 3*u + 1) / 6.0
  } ensuring(res => res +/- 3.25e-16)

  // 0.1 f64
  def bspline2_64_01(u: Real): Real = {
    require(0 <= u && u <= 1)
    (-3 * u*u*u  + 3*u*u + 3*u + 1) / 6.0
  } ensuring(res => res +/- 6.5e-17)

  // 0.01 f64
  def bspline2_64_001(u: Real): Real = {
    require(0 <= u && u <= 1)
    (-3 * u*u*u  + 3*u*u + 3*u + 1) / 6.0
  } ensuring(res => res +/- 6.5e-18)

  // dbldbl
  def bspline2_dbldbl(u: Real): Real = {
    require(0 <= u && u <= 1)
    (-3 * u*u*u  + 3*u*u + 3*u + 1) / 6.0
  } ensuring(res => res +/- 2e-31)

}
