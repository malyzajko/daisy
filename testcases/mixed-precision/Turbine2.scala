

import daisy.lang._
import Real._


object Turbine2 {
  // f32
  def turbine2_32(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 3.8 <= r && r <= 7.8)
    6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5
  } ensuring(res => res +/- 7e-5)

  // 0.5 f32
  def turbine2_32_05(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 3.8 <= r && r <= 7.8)
    6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5
  } ensuring(res => res +/- 3.5e-5)

  // 0.1 f32
  def turbine2_32_01(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 3.8 <= r && r <= 7.8)
    6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5
  } ensuring(res => res +/- 7e-6)

  // 0.01 f32
  def turbine2_32_001(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 3.8 <= r && r <= 7.8)
    6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5
  } ensuring(res => res +/- 7e-7)

  // f64
  def turbine2_64(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 3.8 <= r && r <= 7.8)
    6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5
  } ensuring(res => res +/- 1.5e-13)

  // 0.5 f64
  def turbine2_64_05(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 3.8 <= r && r <= 7.8)
    6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5
  } ensuring(res => res +/- 7.5e-14)

  // 0.1 f64
  def turbine2_64_01(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 3.8 <= r && r <= 7.8)
    6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5
  } ensuring(res => res +/- 1.5e-14)

  // 0.01 f64
  def turbine2_64_001(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 3.8 <= r && r <= 7.8)
    6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5
  } ensuring(res => res +/- 1.5e-15)

  // dbldbl
  def turbine2_dbldbl(v: Real, w: Real, r: Real): Real = {
    require(-4.5 <= v && v <= -0.3 && 0.4 <= w && w <= 0.9 && 3.8 <= r && r <= 7.8)
    6*v - 0.5 * v * (w*w*r*r) / (1-v) - 2.5
  } ensuring(res => res +/- 4.5e-29)

}