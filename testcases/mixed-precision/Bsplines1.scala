
import daisy.lang._
import Real._


object Bsplines1 {

  // f32
  def bspline1_32(u: Real): Real = {
    require(0 <= u && u <= 1)
    (3 * u*u*u - 6 * u*u + 4) / 6.0
  } ensuring (res => res +/- 4e-7)

  //0.5 f32
  def bspline1_32_05(u: Real): Real = {
    require(0 <= u && u <= 1)
    (3 * u*u*u - 6 * u*u + 4) / 6.0
  } ensuring (res => res +/- 2e-7)

  // 0.1 f32
  def bspline1_32_01(u: Real): Real = {
    require(0 <= u && u <= 1)
    (3 * u*u*u - 6 * u*u + 4) / 6.0
  } ensuring (res => res +/- 4e-8)

  // 0.01 f32
  def bspline1_32_001(u: Real): Real = {
    require(0 <= u && u <= 1)
    (3 * u*u*u - 6 * u*u + 4) / 6.0
  } ensuring (res => res +/- 4e-9)

  // f64
  def bspline1_64(u: Real): Real = {
    require(0 <= u && u <= 1)
    (3 * u*u*u - 6 * u*u + 4) / 6.0
  } ensuring (res => res +/- 7.5e-16)

  // 0.5 f64
  def bspline1_64_05(u: Real): Real = {
    require(0 <= u && u <= 1)
    (3 * u*u*u - 6 * u*u + 4) / 6.0
  } ensuring (res => res +/- 3.75e-16)

  // 0.1 f64
  def bspline1_64_01(u: Real): Real = {
    require(0 <= u && u <= 1)
    (3 * u*u*u - 6 * u*u + 4) / 6.0
  } ensuring (res => res +/- 7.5e-17)

  // 0.01 f64
  def bspline1_64_001(u: Real): Real = {
    require(0 <= u && u <= 1)
    (3 * u*u*u - 6 * u*u + 4) / 6.0
  } ensuring (res => res +/- 7.5e-18)

  // dbldbl
  def bspline1_dbldbl(u: Real): Real = {
    require(0 <= u && u <= 1)
    (3 * u*u*u - 6 * u*u + 4) / 6.0
  } ensuring (res => res +/- 2e-31)


}
