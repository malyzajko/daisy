import daisy.lang._
import Real._

object TimesPlusReal {

 // def sumPlus2terms(u: Real): Real = {
 //   require(-11.633685649025693 <= u && u <= -11.633685649025693) // 100
 //   // maxAbsError: 2.926454093647371e-17 maxRelError: 4.6812116338827325e-12
 //   0.0447 * u + 0.483292 + 0.03674
//
 // }

  def plusSum(u: Real): Real ={
    require(-11.633685649025693 <= u && u <= -11.633685649025693)
    // maxAbsError: 7.783679826382432e-17 maxRelError: 1.2450922307913817e-11
    0.483292 + 0.03674 + 0.0447 * u
  }

def plus2TermsSum(u: Real): Real = {
	require(-11.633685649025693 <= u && u <= -11.633685649025693)
	0.520032 + 0.0447 * u
}
//  def sumPlus1term(u: Real): Real ={
//    require(-11.633685649025693 <= u && u <= -11.633685649025693)
  // maxAbsError: 7.783679826382432e-17 maxRelError: 1.2450922307913817e-11
//     0.0447 * u + 0.520032
//  }
//
 // def plusSum2TermsPure(u: Real, x: Real): Real = {
 //   require(-11.633685649025693 <= u && u <= -11.633685649025693 && 1 <= x && x<=1)
 //   (0.483292 + 0.03674*x ) + 0.0447 * u
 // }

 // def plusSum2TermsParenthesis(u: Real, x: Real): Real = {
 //   require(-11.633685649025693 <= u && u <= -11.633685649025693 && 1<=x && x<=1)
 //   0.483292 + (0.03674 *x + 0.0447 * u)
 // }
}

// seed = 1486647796240
