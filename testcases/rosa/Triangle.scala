
import daisy.lang._
import Real._


/*
  The famous formula for computing the area of a triangle.
 */
object Triangle {

  def triangleUnstable(a: Real, b: Real, c: Real): Real = {
    // TODO: this precondition can only be handled with SMT, and only
    // if we add the usage of the additional constraints
    //require(1 < a && a < 9 && 1 < b && b < 9 && 1 < c && c < 9 &&
    //  a + b > c + 0.000001 && a + c > b + 0.000001 && b + c > a + 0.000001)
    require(4 <= a && a <= 5 && 4 <= b && b <= 4 && 4 <= c && c <= 5)

    val s = (a + b + c)/2.0
    sqrt(s * (s - a) * (s - b) * (s - c))

  } ensuring (res => res >= 0.0 && res +/- 2e-9)


  // def triangleKahan(aa: Real, bb: Real, cc: Real): Real = {
  //   var a = aa
  //   var b = bb
  //   var c = cc

  //   if(b < a) {
  //     val t = a
  //     if(c < b) {
  //       a = c; c = t
  //     }
  //     else {
  //       if(c < a) {
  //         a = b; b = c; c = t
  //       }
  //       else {
  //         a = b; b = t
  //       }
  //     }
  //   }
  //   else if(c < b) {
  //     val t = c; c = b;
  //     if(c < a) {
  //       b = a; a = t
  //     }
  //     else {
  //       b = t
  //     }
  //   }
  //   sqrt((a+(b+c)) * (c-(a-b)) * (c+(a-b)) * (a+(b-c))) / 4.0
  // }

}