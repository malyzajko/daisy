import daisy.lang._
import Real._


object BatchReactorSingle {

  // s1, s2, s3, s4, y1, y2: <1, 16, 14>
  def BatchReactorSingle_out1(s1: Real) = { //, s2: Real, s3: Real, s4: Real
    require(-1 <= s1 && s1 <= 1) // && -1 <= s2 && s2 <= 1 && -1 <= s3 && s3 <= 1 && -1 <= s4 && s4 <= 1)
    (-0.058300) * s1 + (-0.908300) * 0.16 + (-0.325800) * -0.984 + (-0.872100) * 0.75
  }

  def BatchReactorSingle_out2(s1: Real) = { // , s2: Real, s3: Real, s4: Real
    require(-1 <= s1 && s1 <= 1) // && -1 <= s2 && s2 <= 1 && -1 <= s3 && s3 <= 1 && -1 <= s4 && s4 <= 1)
    (2.463800) * s1 + (0.050400) * 0.16 + (1.709900) * -0.984 + (-1.165300) * 0.75
  }

  def BatchReactorSingle_state1(s1: Real) = { // , s2: Real, s3: Real, s4: Real, y1: Real, y2: Real
    require(-1 <= s1 && s1 <= 1) // && -1 <= s2 && s2 <= 1 && -1 <= s3 && s3 <= 1 && -1 <= s4 && s4 <= 1 &&
//      -1 <= y1 && y1 <= 1 && -1 <= y2 && y2 <= 1)
    (0.934292) * s1 + (0.008415) * 0.16 + (-0.014106) * -0.984 + (0.023954) * 0.75 + (0.077400) * 0.65 + (-0.010300) * -0.00703
  }

  def BatchReactorSingle_state2(s1: Real) = { // , s2: Real, s3: Real, s4: Real, y1: Real, y2: Real
    require(-1 <= s1 && s1 <= 1) // && -1 <= s2 && s2 <= 1 && -1 <= s3 && s3 <= 1 && -1 <= s4 && s4 <= 1 &&
//      -1 <= y1 && y1 <= 1 && -1 <= y2 && y2 <= 1)
    (-0.006769) * s1 + (0.884924) * 0.16 + (-0.016066) * -0.984 + (-0.044019) * 0.75 + (-0.002200) * 0.65 + (0.022700) * -0.00703
  }

  def BatchReactorSingle_state3(s1: Real) = { // , s2: Real, s3: Real, s4: Real, y1: Real, y2: Real
    require(-1 <= s1 && s1 <= 1) // && -1 <= s2 && s2 <= 1 && -1 <= s3 && s3 <= 1 && -1 <= s4 && s4 <= 1 &&
//      -1 <= y1 && y1 <= 1 && -1 <= y2 && y2 <= 1)
    (-0.092148) * s1 + (-0.011039) * 0.16 + (0.853511) * -0.984 + (0.107537) * 0.75 + (0.026700) * 0.65 + (0.039800) * -0.00703
  }

  def BatchReactorSingle_state4(s1: Real) = { // , s2: Real, s3: Real, s4: Real, y1: Real, y2: Real
    require(-1 <= s1 && s1 <= 1) // && -1 <= s2 && s2 <= 1 && -1 <= s3 && s3 <= 1 && -1 <= s4 && s4 <= 1 &&
//      -1 <= y1 && y1 <= 1 && -1 <= y2 && y2 <= 1)
    (-0.036410) * s1 + (0.030194) * 0.16 + (-0.027155) * -0.984 + (1.004619) * 0.75 + (0.035600) * 0.65 + (0.000100) * -0.00703
  }



}