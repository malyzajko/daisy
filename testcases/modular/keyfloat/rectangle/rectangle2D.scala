import scala.annotation.strictfp
import daisy.lang._
import Real._
@strictfp
object rectangle2D {

  def createTransformedArea_upperLeftPointX(x: Real, affineTransform_m00: Real): Real = {
    require(x <= -3.38 && x >= -5.53 && affineTransform_m00 <= 1.0 && affineTransform_m00 > 0.0)

    val oldX: Real = x
    val newX: Real = (oldX * affineTransform_m00)
    newX
  }

  def createTransformedArea_upperLeftPointY(y: Real, affineTransform_m11: Real): Real = {
    require(y <= -3.38 && y >= -5.53 && affineTransform_m11 <= 1.0 && affineTransform_m11 > 0.0)
    val oldY: Real = y

    val newY: Real = (oldY * affineTransform_m11)

    newY
  }

  def createTransformedArea_width(x: Real, width: Real, affineTransform_m00: Real): Real = {
    require(x <= -3.38 && x >= -5.53 && width <= 3.7332 &&
      width > 3.1 && affineTransform_m00 <= 4.1 && affineTransform_m00 > 3.0)

    val oldX: Real = x
    val oldUpperRightPointX: Real = oldX + width
    val newX: Real = (oldX * affineTransform_m00)
    val newUpperRightPointX: Real = (oldUpperRightPointX * affineTransform_m00)

    newX - newUpperRightPointX
  }

  def createTransformedArea_hight(x: Real, y: Real, height: Real, affineTransform_m11: Real): Real = {
    require(x <= -3.38 && x >= -5.53 && y <= -3.38 && y >= -5.53 && height <= 4.0004 &&
      height > 3.0000001 && affineTransform_m11 <= 3.1 && affineTransform_m11 > -6.5)

    val oldY: Real = y
    val oldLlowerLeftPointY: Real = oldY - height
    val newY: Real = (oldY * affineTransform_m11)
    val newLowerLeftPointY: Real = (oldLlowerLeftPointY * affineTransform_m11)

    newY - newLowerLeftPointY
  }


  def areaOfScaled(x: Real, y: Real, width: Real, height: Real, arg1: Real, arg2: Real): Real = {
    require(x <= -3.38 && x >= -5.53 && y <= -3.38 && y >= -5.53 &&
      width <= 3.7332 && width > 3.1 && height <= 4.0004 && height > 3.0000001 &&
      arg1 <= 4.0024 && arg1 > 3.0003001 && arg2 <= 3.0001 && arg2 > -6.4000003)

    val affineTransform_m00: Real = arg1
    val affineTransform_m11: Real = arg2

    /* val scaled_x: Real = createTransformedArea_upperLeftPointX(x, affineTransform_m00)
      val scaled_y: Real = createTransformedArea_upperLeftPointY(y, affineTransform_m11) */
    val scaled_width: Real = createTransformedArea_width(x, width, affineTransform_m00)
    val scaled_hight: Real = createTransformedArea_hight(x, y, height, affineTransform_m11)

    scaled_width * scaled_hight
  }
}
