import daisy.lang._
import Real._

object Misc {

  // a, d: <1, 16, 11>, b, c, e: <1, 16, 13>
  def martel(a: Real, b: Real, c: Real, d: Real) = {
    require(-14 <= a && a <= -13 && -3 <= b && b <= -2 && 3 <= c && c <= 3.5 && 12.5 <= d && d <= 13.5 && 2 <= e && e <= 2)
    (a + (b + (c + d))) * e
  }

  def euclideanProjection() = {
    Block 0
ai ai 1 16 11 -10.0 10.0
aj aj 1 16 11 -10.0 10.0
ak ak 1 16 11 -10.0 10.0
xi xi 1 16 8 -100.0 100.0
xj xj 1 16 8 -100.0 100.0
xk xk 1 16 8 -100.0 100.0
b b 1 16 11 -10.0 10.0
End of Inputs
    xi + ((b - (ai*xi + aj*xj + ak*xk))/(ai*ai + aj*aj + ak*ak)) * ai
  }


  def fieldControlledDCMotor() = {
Block 0
iff iff 1 16 14 0 1.5
ia ia 1 16 14 0.1 1.5
omega omega 1 16 14 0.1 1.5
End of Inputs

    (1 / ((0.01) + ia)) * ((((1.0) * ((1.0 + 1.0) * (iff * ia))) + ((1.0) * ((1.0) * iff))) - ((1.0) * (((iff * iff) * omega) * (1.0))))
  }


  def newtonsmethod() = {
    Block 0
c0 c0 1 16 11 -10.0 10.0
c1 c1 1 16 11 7.5 8.5
c2 c2 1 16 13 -3.75 -3.25
c3 c3 1 16 14 0.833 1.167
x x 1 16 8 -100.0 100.0
End of Inputs

    x - (c3*x*x*x + c2*x*x + c1*x + c0)/(3.0*c3*x*x + 2.0*c2*x + c1)
  }

  def
}