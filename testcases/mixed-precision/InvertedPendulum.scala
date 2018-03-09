

import daisy.lang._
import Real._


object InvertedPendulum {

  // f32
  def invpendulum_out_32(s1: Real, s2: Real, s3: Real, s4: Real) = {
    require(-50 <= s1 && s1 <= 50 && -10 <= s2 && s2 <= 10 && -0.785 <= s3 && s3 <= 0.785 && -0.785 <= s4 && s4 <= 0.785)

    1.0000 * s1 + 1.6567 * s2 + (-18.6854) * s3 + (-3.4594) * s4
  } ensuring(res => res +/- 2e-5)

  // 0.5 f32
  def invpendulum_out_32_05(s1: Real, s2: Real, s3: Real, s4: Real) = {
    require(-50 <= s1 && s1 <= 50 && -10 <= s2 && s2 <= 10 && -0.785 <= s3 && s3 <= 0.785 && -0.785 <= s4 && s4 <= 0.785)

    1.0000 * s1 + 1.6567 * s2 + (-18.6854) * s3 + (-3.4594) * s4
  } ensuring(res => res +/- 1e-5)

  // 0.1 f32
  def invpendulum_out_32_01(s1: Real, s2: Real, s3: Real, s4: Real) = {
    require(-50 <= s1 && s1 <= 50 && -10 <= s2 && s2 <= 10 && -0.785 <= s3 && s3 <= 0.785 && -0.785 <= s4 && s4 <= 0.785)

    1.0000 * s1 + 1.6567 * s2 + (-18.6854) * s3 + (-3.4594) * s4
  } ensuring(res => res +/- 2e-6)

  // 0.01 f32
  def invpendulum_out_32_001(s1: Real, s2: Real, s3: Real, s4: Real) = {
    require(-50 <= s1 && s1 <= 50 && -10 <= s2 && s2 <= 10 && -0.785 <= s3 && s3 <= 0.785 && -0.785 <= s4 && s4 <= 0.785)

    1.0000 * s1 + 1.6567 * s2 + (-18.6854) * s3 + (-3.4594) * s4
  } ensuring(res => res +/- 2e-7)

  // f64
  def invpendulum_out_64(s1: Real, s2: Real, s3: Real, s4: Real) = {
    require(-50 <= s1 && s1 <= 50 && -10 <= s2 && s2 <= 10 && -0.785 <= s3 && s3 <= 0.785 && -0.785 <= s4 && s4 <= 0.785)

    1.0000 * s1 + 1.6567 * s2 + (-18.6854) * s3 + (-3.4594) * s4
  } ensuring(res => res +/- 4e-14)

  // 0.5 f64
  def invpendulum_out_64_05(s1: Real, s2: Real, s3: Real, s4: Real) = {
    require(-50 <= s1 && s1 <= 50 && -10 <= s2 && s2 <= 10 && -0.785 <= s3 && s3 <= 0.785 && -0.785 <= s4 && s4 <= 0.785)

    1.0000 * s1 + 1.6567 * s2 + (-18.6854) * s3 + (-3.4594) * s4
  } ensuring(res => res +/- 2e-14)

  // 0.1 f64
  def invpendulum_out_64_01(s1: Real, s2: Real, s3: Real, s4: Real) = {
    require(-50 <= s1 && s1 <= 50 && -10 <= s2 && s2 <= 10 && -0.785 <= s3 && s3 <= 0.785 && -0.785 <= s4 && s4 <= 0.785)

    1.0000 * s1 + 1.6567 * s2 + (-18.6854) * s3 + (-3.4594) * s4
  } ensuring(res => res +/- 4e-15)

  // 0.01 f64
  def invpendulum_out_64_001(s1: Real, s2: Real, s3: Real, s4: Real) = {
    require(-50 <= s1 && s1 <= 50 && -10 <= s2 && s2 <= 10 && -0.785 <= s3 && s3 <= 0.785 && -0.785 <= s4 && s4 <= 0.785)

    1.0000 * s1 + 1.6567 * s2 + (-18.6854) * s3 + (-3.4594) * s4
  } ensuring(res => res +/- 4e-16)

  // dbldbl
  def invpendulum_out_dbldbl(s1: Real, s2: Real, s3: Real, s4: Real) = {
    require(-50 <= s1 && s1 <= 50 && -10 <= s2 && s2 <= 10 && -0.785 <= s3 && s3 <= 0.785 && -0.785 <= s4 && s4 <= 0.785)

    1.0000 * s1 + 1.6567 * s2 + (-18.6854) * s3 + (-3.4594) * s4
  } ensuring(res => res +/- 1.5e-29)

}