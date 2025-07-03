import scala.annotation.strictfp
import scala.math._

@strictfp
object allTests_tuning {

  /* @pre: ((-100.0 <= u) && (u <= 100.0) && (20.0 <= v) && (v <= 20000.0) && (-30.0 <= T) && (T <= 50.0)) */
  /* @post: (res) => (res +/- 2.25e-13) */

    def doppler_64_05(u: Double, v: Double, T: Double): Double = {
    val _const0: DblDouble = 0.6
    val _const1: DblDouble = 331.4
    val _tmp: DblDouble = (_const0 * T)
    val t1: DblDouble = (_const1 + _tmp)
    val _tmp1: Double = -(t1).toDouble
    val _tmp4: Double = (_tmp1 * v)
    val _tmp2: Double = (t1 + u).toDouble
    val _tmp3: Double = (t1 + u).toDouble
    val _tmp5: Double = (_tmp2 * _tmp3)
    (_tmp4 / _tmp5)
  } // [-158.7191444098274, -0.02944244059231351] +/- 2.2355864078143494e-13

  /* @pre: ((4.0 <= x1) && (x1 <= 6.36) && (4.0 <= x2) && (x2 <= 6.36) && (4.0 <= x3) && (x3 <= 6.36) && (4.0 <= x4) && (x4 <= 6.36) && (4.0 <= x5) && (x5 <= 6.36) && (4.0 <= x6) && (x6 <= 6.36)) */
  /* @post: (res) => (res +/- 4.75e-14) */

    def kepler0_64_05(x1: Double, x2: Double, x3: Double, x4: Double, x5: Double, x6: DblDouble): DblDouble = {
    val _tmp6: DblDouble = (x2 * x5)
    val _tmp7: DblDouble = (x3 * x6)
    val _tmp8: DblDouble = (_tmp6 + _tmp7)
    val _tmp9: DblDouble = (x2 * x3)
    val _tmp10: DblDouble = (_tmp8 - _tmp9)
    val _tmp11: DblDouble = (x5 * x6)
    val _tmp18: DblDouble = (_tmp10 - _tmp11)
    val _tmp12: DblDouble = -(x1)
    val _tmp13: DblDouble = (_tmp12 + x2)
    val _tmp14: DblDouble = (_tmp13 + x3)
    val _tmp15: DblDouble = (_tmp14 - x4)
    val _tmp16: DblDouble = (_tmp15 + x5)
    val _tmp17: DblDouble = (_tmp16 + x6)
    val _tmp19: DblDouble = (x1 * _tmp17)
    (_tmp18 + _tmp19)
  } // [-35.7792, 159.8176] +/- 4.3050008002865075e-14

  /* @pre: ((-15.0 <= x1) && (x1 <= 15.0) && (-15.0 <= x2) && (x2 <= 15.0) && (-15.0 <= x3) && (x3 <= 15.0)) */
  /* @post: (res) => (res +/- 1.75e-13) */

    def rigidBody1_64_05(x1: Double, x2: Double, x3: DblDouble): DblDouble = {
    val _const0: DblDouble = 2
    val _tmp20: DblDouble = -(x1)
    val _tmp22: DblDouble = (_tmp20 * x2)
    val _tmp21: DblDouble = (_const0 * x2)
    val _tmp23: DblDouble = (_tmp21 * x3)
    val _tmp24: DblDouble = (_tmp22 - _tmp23)
    val _tmp25: DblDouble = (_tmp24 - x1)
    (_tmp25 - x3)
  } // [-705.0, 705.0] +/- 6.838973831690964e-14

  /* @pre: ((-15.0 <= x1) && (x1 <= 15.0) && (-15.0 <= x2) && (x2 <= 15.0) && (-15.0 <= x3) && (x3 <= 15.0)) */
  /* @post: (res) => (res +/- 2.0e-11) */

    def rigidBody2_64_05(x1: DblDouble, x2: DblDouble, x3: DblDouble): Double = {
    val _const0: DblDouble = 2
    val _const1: Double = 3
    val _const2: Float = 3f
    val _tmp26: Double = (x1 * x2).toDouble
    val _tmp27: Double = (_tmp26 * x3).toDouble
    val _tmp29: Double = (_const0 * _tmp27).toDouble
    val _tmp28: Double = (_const1 * x3).toDouble
    val _tmp30: Double = (_tmp28 * x3).toDouble
    val _tmp33: Double = (_tmp29 + _tmp30)
    val _tmp31: Double = (x1 * x2).toDouble
    val _tmp32: Double = (_tmp31 * x3).toDouble
    val _tmp34: Double = (x2 * _tmp32).toDouble
    val _tmp36: Double = (_tmp33 - _tmp34)
    val _tmp35: Double = (_const2 * x3).toDouble
    val _tmp37: Double = (_tmp35 * x3).toDouble
    val _tmp38: Double = (_tmp36 + _tmp37)
    (_tmp38 - x2)
  } // [-58740.0, 58740.0] +/- 1.9532819806045154e-11

  /* @pre: ((-4.5 <= v) && (v <= -0.3) && (0.4 <= w) && (w <= 0.9) && (3.8 <= r) && (r <= 7.8)) */
  /* @post: (res) => (res +/- 4.5e-14) */

    def turbine1_64_05(v: DblDouble, w: DblDouble, r: DblDouble): Double = {
    val _const0: DblDouble = 2
    val _const1: DblDouble = 3
    val _const2: DblDouble = 2
    val _const3: DblDouble = 3
    val _const4: DblDouble = 0.125
    val _const5: DblDouble = 1
    val _const6: DblDouble = 4.5
    val _tmp39: DblDouble = (r * r)
    val _tmp40: DblDouble = (_const0 / _tmp39)
    val _tmp49: DblDouble = (_const1 + _tmp40)
    val _tmp41: DblDouble = (_const2 * v)
    val _tmp42: DblDouble = (_const3 - _tmp41)
    val _tmp45: DblDouble = (_const4 * _tmp42)
    val _tmp43: DblDouble = (w * w)
    val _tmp44: Double = (_tmp43 * r).toDouble
    val _tmp46: Double = (_tmp44 * r).toDouble
    val _tmp47: Double = (_tmp45 * _tmp46).toDouble
    val _tmp48: Double = (_const5 - v).toDouble
    val _tmp50: Double = (_tmp47 / _tmp48)
    val _tmp51: Double = (_tmp49 - _tmp50).toDouble
    (_tmp51 - _const6)
  } // [-58.32912689020381, -1.5505285721480735] +/- 4.0091690651094673e-14

  /* @pre: ((-4.5 <= v) && (v <= -0.3) && (0.4 <= w) && (w <= 0.9) && (3.8 <= r) && (r <= 7.8)) */
  /* @post: (res) => (res +/- 7.5e-14) */

    def turbine2_64_05(v: DblDouble, w: DblDouble, r: DblDouble): Double = {
    val _const0: DblDouble = 6
    val _const1: Float = 0.5f
    val _const2: Float = 1f
    val _const3: Double = 2.5
    val _tmp58: Double = (_const0 * v).toDouble
    val _tmp54: DblDouble = (_const1 * v)
    val _tmp52: DblDouble = (w * w)
    val _tmp53: Double = (_tmp52 * r).toDouble
    val _tmp55: Double = (_tmp53 * r).toDouble
    val _tmp56: Double = (_tmp54 * _tmp55).toDouble
    val _tmp57: Double = (_const2 - v).toDouble
    val _tmp59: Double = (_tmp56 / _tmp57)
    val _tmp60: Double = (_tmp58 - _tmp59)
    (_tmp60 - _const3)
  } // [-29.43698909090909, 80.993] +/- 6.983917717644337e-14

  /* @pre: ((-4.5 <= v) && (v <= -0.3) && (0.4 <= w) && (w <= 0.9) && (3.8 <= r) && (r <= 7.8)) */
  /* @post: (res) => (res +/- 3.25e-14) */

    def turbine3_64_05(v: DblDouble, w: DblDouble, r: DblDouble): Double = {
    val _const0: DblDouble = 2
    val _const1: DblDouble = 3
    val _const2: DblDouble = 2
    val _const3: DblDouble = 1
    val _const4: DblDouble = 0.125
    val _const5: DblDouble = 1
    val _const6: DblDouble = 0.5
    val _tmp61: DblDouble = (r * r)
    val _tmp62: DblDouble = (_const0 / _tmp61)
    val _tmp71: DblDouble = (_const1 - _tmp62)
    val _tmp63: DblDouble = (_const2 * v)
    val _tmp64: DblDouble = (_const3 + _tmp63)
    val _tmp67: DblDouble = (_const4 * _tmp64)
    val _tmp65: DblDouble = (w * w)
    val _tmp66: Double = (_tmp65 * r).toDouble
    val _tmp68: Double = (_tmp66 * r).toDouble
    val _tmp69: Double = (_tmp67 * _tmp68).toDouble
    val _tmp70: Double = (_const5 - v).toDouble
    val _tmp72: Double = (_tmp69 / _tmp70)
    val _tmp73: Double = (_tmp71 - _tmp72).toDouble
    (_tmp73 - _const6)
  } // [0.4660958448753463, 40.375126890203816] +/- 2.8185317327929985e-14

}
