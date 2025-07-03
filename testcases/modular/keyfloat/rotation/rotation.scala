import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object rotation {

 def rotate_x(vec0: Real, vec1: Real, degree: Real): Real = {
    require((-3.0 <= vec0) && (vec0 <=  3.0)&& (-3.0 <= vec1) && (vec1 <= 3.0) && (0.0 <= degree) && (degree <= 360))
    vec0 * cos(degree) - vec1 * sin(degree)
  }


  def rotate_y(vec0: Real, vec1: Real, degree: Real): Real = {
    require((-3.0 <= vec0) && (vec0 <=  3.0)&& (-3.0 <= vec1) && (vec1 <= 3.0) && (0.0 <= degree) && (degree <= 360))
   vec0 * sin(degree) + vec1 * cos(degree)
 }

  def computeError_x(vec0: Real, vec1: Real): Real = {
    require((1.0 <= vec0) && (vec0 <=  2.0)&& (1.0 <= vec1) && (vec1 <= 2.0))

    val t1_x: Real =  rotate_x(vec0,vec1, 90.0)
    val t1_y: Real =  rotate_y(vec0,vec1, 90.0)
    val t2_x: Real = rotate_x(t1_x,t1_y, 90.0)
    val t2_y: Real = rotate_y(t1_x,t1_y, 90.0)
    val t3_x: Real = rotate_x(t2_x,t2_y, 90.0)
    val t3_y: Real = rotate_y(t2_x,t2_y, 90.0)
    val t4_x: Real = rotate_x(t3_x,t3_y, 90.0)
    //  t4_x - vec0
    t4_x
  }

  def computeError_y(vec0: Real, vec1: Real): Real = {
    require((1.0 <= vec0) && (vec0 <=  2.0)&& (1.0 <= vec1) && (vec1 <= 2.0))

    val t1_x: Real =  rotate_x(vec0,vec1, 90.0)
    val t1_y: Real =  rotate_y(vec0,vec1, 90.0)
    val t2_x: Real = rotate_x(t1_x,t1_y, 90.0)
    val t2_y: Real = rotate_y(t1_x,t1_y, 90.0)
    val t3_x: Real = rotate_x(t2_x,t2_y, 90.0)
    val t3_y: Real = rotate_y(t2_x,t2_y, 90.0)
    val t4_y: Real = rotate_y(t3_x,t3_y, 90.0)
    // t4_y - vec1
    t4_y
  }
}
