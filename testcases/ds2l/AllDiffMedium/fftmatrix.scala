import daisy.lang._
import Real._
import daisy.lang.Vector._

object fftmatrix {

	def fftmatrix(m: Matrix): Matrix = {
require(m >= -326.68 && m <= 677.57 && m.size(128,2)
	 && m.specM(Set((Set((0, 0)),(-214.95, 411.95)), (Set((0, 1)),(348.81, 588.21)),
		(Set((1, 0)),(140.55, 425.99)), (Set((1, 1)),(374.38, 592.6)),
		(Set((2, 0)),(-20.34, 666.55)), (Set((2, 1)),(-153.75, 96.97)),
		(Set((3, 0)),(13.04, 623.64)), (Set((3, 1)),(-290.99, -279.1)),
		(Set((4, 0)),(-246.92, -28.78)), (Set((4, 1)),(-40.74, 206.7)),
		(Set((5, 0)),(-326.63, 31.15)), (Set((5, 1)),(-161.77, 297.13)),
		(Set((6, 0)),(-279.65, 130.65)), (Set((6, 1)),(5.28, 134.18)),
		(Set((7, 0)),(-320.05, -102.34)), (Set((7, 1)),(-41.15, 590.65)),
		(Set((8, 0)),(-36.8, 672.81)), (Set((8, 1)),(98.07, 312.72)),
		(Set((9, 0)),(216.77, 517.6)), (Set((9, 1)),(-290.63, 3.78)),
		(Set((10, 0)),(-80.79, 144.55)), (Set((10, 1)),(-33.46, 181.16)),
		(Set((11, 0)),(619.47, 669.19)), (Set((11, 1)),(440.22, 484.18)),
		(Set((12, 0)),(-229.09, 8.5)), (Set((12, 1)),(79.46, 135.05)),
		(Set((13, 0)),(37.83, 623.97)), (Set((13, 1)),(139.58, 364.36)),
		(Set((14, 0)),(-315.71, -14.35)), (Set((14, 1)),(-296.64, 677.55)),
		(Set((15, 0)),(376.78, 528.5)), (Set((15, 1)),(0.49, 295.52)),
		(Set((16, 0)),(-190.96, -0.47)), (Set((16, 1)),(-170.62, 89.39)),
		(Set((17, 0)),(-307.95, 618.48)), (Set((17, 1)),(268.64, 304.56)),
		(Set((18, 0)),(-29.96, 414.74)), (Set((18, 1)),(-273.78, 409.07)),
		(Set((19, 0)),(-47.31, 432.55)), (Set((19, 1)),(558.53, 602.71)),
		(Set((20, 0)),(-298.02, 0.55)), (Set((20, 1)),(141.03, 161.93)),
		(Set((21, 0)),(237.8, 242.6)), (Set((21, 1)),(4.41, 386.6)),
		(Set((22, 0)),(18.35, 293.15)), (Set((22, 1)),(262.45, 621.04)),
		(Set((23, 0)),(-25.6, 562.43)), (Set((23, 1)),(-61.63, -61.59)),
		(Set((24, 0)),(-123.1, 447.96)), (Set((24, 1)),(168.98, 522.41)),
		(Set((25, 0)),(587.53, 641.36)), (Set((25, 1)),(-237.16, 463.89)),
		(Set((26, 0)),(-11.05, 243.83)), (Set((26, 1)),(-131.87, -107.67)),
		(Set((27, 0)),(382.78, 676.62)), (Set((27, 1)),(-137.11, 400.75)),
		(Set((28, 0)),(176.07, 322.97)), (Set((28, 1)),(181.13, 215.28)),
		(Set((29, 0)),(507.4, 665.72)), (Set((29, 1)),(406.51, 670.73)),
		(Set((30, 0)),(30.37, 220.31)), (Set((30, 1)),(-222.88, 428.82)),
		(Set((31, 0)),(-231.8, 347.21)), (Set((31, 1)),(100.64, 164.9)),
		(Set((32, 0)),(402.61, 419.42)), (Set((32, 1)),(565.15, 579.23)),
		(Set((33, 0)),(-257.66, -104.05)), (Set((33, 1)),(242.01, 282.09)),
		(Set((34, 0)),(-220.95, 217.17)), (Set((34, 1)),(-255.34, 278.19)),
		(Set((35, 0)),(-317.81, 388.86)), (Set((35, 1)),(-299.45, 97.45)),
		(Set((36, 0)),(51.46, 522.59)), (Set((36, 1)),(-210.54, -30.06)),
		(Set((37, 0)),(218.01, 534.52)), (Set((37, 1)),(-270.43, 277.59)),
		(Set((38, 0)),(-181.07, 516.46)), (Set((38, 1)),(-320.12, -74.89)),
		(Set((39, 0)),(-201.48, 143.84)), (Set((39, 1)),(503.54, 510.74)),
		(Set((40, 0)),(276.96, 326.11)), (Set((40, 1)),(218.14, 482.06)),
		(Set((41, 0)),(176.32, 506.2)), (Set((41, 1)),(620.7, 635.69)),
		(Set((42, 0)),(-127.35, -41.63)), (Set((42, 1)),(149.42, 402.35)),
		(Set((43, 0)),(354.77, 654.31)), (Set((43, 1)),(316.53, 336.74)),
		(Set((44, 0)),(-248.15, 55.48)), (Set((44, 1)),(81.33, 241.88)),
		(Set((45, 0)),(165.65, 590.45)), (Set((45, 1)),(338.82, 517.0)),
		(Set((46, 0)),(-26.46, 404.21)), (Set((46, 1)),(-239.35, 228.29)),
		(Set((47, 0)),(107.51, 190.42)), (Set((47, 1)),(-225.19, 145.64)),
		(Set((48, 0)),(400.93, 457.17)), (Set((48, 1)),(332.7, 421.46)),
		(Set((49, 0)),(317.52, 508.46)), (Set((49, 1)),(70.99, 587.74)),
		(Set((50, 0)),(-318.78, -137.67)), (Set((50, 1)),(35.41, 222.82)),
		(Set((51, 0)),(-106.97, 572.59)), (Set((51, 1)),(299.89, 455.02)),
		(Set((52, 0)),(-278.73, 248.06)), (Set((52, 1)),(-155.4, 20.43)),
		(Set((53, 0)),(-181.6, 74.91)), (Set((53, 1)),(49.12, 114.53)),
		(Set((54, 0)),(-184.61, 54.03)), (Set((54, 1)),(90.16, 205.69)),
		(Set((55, 0)),(-311.47, 62.05)), (Set((55, 1)),(-137.94, 433.73)),
		(Set((56, 0)),(132.31, 447.59)), (Set((56, 1)),(514.21, 592.01)),
		(Set((57, 0)),(183.63, 204.01)), (Set((57, 1)),(317.27, 405.08)),
		(Set((58, 0)),(-285.07, -117.58)), (Set((58, 1)),(-175.31, 566.99)),
		(Set((59, 0)),(-13.64, 501.5)), (Set((59, 1)),(-78.85, 10.26)),
		(Set((60, 0)),(72.52, 667.79)), (Set((60, 1)),(-218.64, 625.49)),
		(Set((61, 0)),(119.25, 210.22)), (Set((61, 1)),(-218.22, 357.71)),
		(Set((62, 0)),(328.74, 574.19)), (Set((62, 1)),(-55.01, -53.5)),
		(Set((63, 0)),(-231.45, 385.05)), (Set((63, 1)),(295.66, 507.17)),
		(Set((64, 0)),(-169.68, 68.95)), (Set((64, 1)),(-233.94, 277.3)),
		(Set((65, 0)),(-319.76, 40.86)), (Set((65, 1)),(-33.83, 87.68)),
		(Set((66, 0)),(57.2, 463.74)), (Set((66, 1)),(-108.62, 616.42)),
		(Set((67, 0)),(-1.72, 611.51)), (Set((67, 1)),(82.85, 617.51)),
		(Set((68, 0)),(-175.54, 561.64)), (Set((68, 1)),(-83.1, 161.44)),
		(Set((69, 0)),(-293.87, 533.3)), (Set((69, 1)),(211.77, 356.45)),
		(Set((70, 0)),(74.5, 77.36)), (Set((70, 1)),(108.09, 282.15)),
		(Set((71, 0)),(185.69, 195.51)), (Set((71, 1)),(70.08, 668.17)),
		(Set((72, 0)),(-210.74, -199.03)), (Set((72, 1)),(-249.08, 73.08)),
		(Set((73, 0)),(581.85, 594.45)), (Set((73, 1)),(82.19, 180.97)),
		(Set((74, 0)),(102.48, 509.23)), (Set((74, 1)),(45.29, 70.82)),
		(Set((75, 0)),(-90.94, 280.16)), (Set((75, 1)),(-281.21, 610.56)),
		(Set((76, 0)),(-311.68, -6.3)), (Set((76, 1)),(461.23, 572.06)),
		(Set((77, 0)),(168.75, 610.13)), (Set((77, 1)),(-26.91, 327.74)),
		(Set((78, 0)),(-25.39, 1.66)), (Set((78, 1)),(-123.54, 33.23)),
		(Set((79, 0)),(-321.31, 496.23)), (Set((79, 1)),(-99.08, 199.3)),
		(Set((80, 0)),(185.7, 500.62)), (Set((80, 1)),(228.49, 309.06)),
		(Set((81, 0)),(123.63, 371.35)), (Set((81, 1)),(-240.73, -74.93)),
		(Set((82, 0)),(365.42, 572.65)), (Set((82, 1)),(429.72, 496.82)),
		(Set((83, 0)),(489.18, 639.39)), (Set((83, 1)),(-200.49, 171.84)),
		(Set((84, 0)),(333.35, 648.73)), (Set((84, 1)),(19.16, 666.49)),
		(Set((85, 0)),(501.64, 580.51)), (Set((85, 1)),(362.09, 610.67)),
		(Set((86, 0)),(2.59, 49.89)), (Set((86, 1)),(-285.73, -217.06)),
		(Set((87, 0)),(255.97, 645.47)), (Set((87, 1)),(1.52, 280.02)),
		(Set((88, 0)),(-287.42, -205.84)), (Set((88, 1)),(199.52, 619.27)),
		(Set((89, 0)),(468.5, 476.11)), (Set((89, 1)),(-227.86, 630.83)),
		(Set((90, 0)),(-161.78, -25.51)), (Set((90, 1)),(-171.95, 514.11)),
		(Set((91, 0)),(-110.88, 178.24)), (Set((91, 1)),(101.2, 367.43)),
		(Set((92, 0)),(-266.61, 503.71)), (Set((92, 1)),(-115.03, 487.71)),
		(Set((93, 0)),(302.53, 346.46)), (Set((93, 1)),(-172.22, 349.01)),
		(Set((94, 0)),(-251.15, -222.08)), (Set((94, 1)),(51.13, 429.11)),
		(Set((95, 0)),(-45.51, 615.03)), (Set((95, 1)),(139.37, 506.46)),
		(Set((96, 0)),(-18.02, 312.48)), (Set((96, 1)),(-112.98, 297.0)),
		(Set((97, 0)),(229.49, 257.97)), (Set((97, 1)),(451.38, 585.66)),
		(Set((98, 0)),(-192.21, 85.05)), (Set((98, 1)),(89.87, 586.74)),
		(Set((99, 0)),(24.92, 547.86)), (Set((99, 1)),(287.46, 517.26)),
		(Set((100, 0)),(480.68, 601.92)), (Set((100, 1)),(-275.45, 290.53)),
		(Set((101, 0)),(-324.02, 53.11)), (Set((101, 1)),(-84.08, 349.51)),
		(Set((102, 0)),(-130.1, 626.46)), (Set((102, 1)),(520.4, 547.67)),
		(Set((103, 0)),(313.58, 490.85)), (Set((103, 1)),(-280.22, -149.03)),
		(Set((104, 0)),(509.86, 553.77)), (Set((104, 1)),(584.82, 590.2)),
		(Set((105, 0)),(306.55, 377.47)), (Set((105, 1)),(604.34, 608.47)),
		(Set((106, 0)),(60.46, 227.04)), (Set((106, 1)),(354.87, 569.51)),
		(Set((107, 0)),(352.82, 552.58)), (Set((107, 1)),(183.23, 294.51)),
		(Set((108, 0)),(-323.76, 107.5)), (Set((108, 1)),(-119.52, 512.6)),
		(Set((109, 0)),(-78.51, 243.6)), (Set((109, 1)),(-190.47, 135.59)),
		(Set((110, 0)),(-238.87, -131.23)), (Set((110, 1)),(-253.54, 140.48)),
		(Set((111, 0)),(-79.53, 39.23)), (Set((111, 1)),(-84.91, 191.29)),
		(Set((112, 0)),(342.36, 390.42)), (Set((112, 1)),(210.91, 555.93)),
		(Set((113, 0)),(-120.92, 346.98)), (Set((113, 1)),(9.39, 90.4)),
		(Set((114, 0)),(-323.72, 79.24)), (Set((114, 1)),(198.53, 368.7)),
		(Set((115, 0)),(-254.27, 604.02)), (Set((115, 1)),(29.0, 51.96)),
		(Set((116, 0)),(11.96, 533.71)), (Set((116, 1)),(262.46, 546.65)),
		(Set((117, 0)),(-154.07, -69.1)), (Set((117, 1)),(-236.26, -117.72)),
		(Set((118, 0)),(62.84, 206.85)), (Set((118, 1)),(289.37, 305.62)),
		(Set((119, 0)),(505.04, 652.43)), (Set((119, 1)),(236.44, 326.59)),
		(Set((120, 0)),(407.69, 627.28)), (Set((120, 1)),(237.26, 510.77)),
		(Set((121, 0)),(-141.81, 629.43)), (Set((121, 1)),(-298.38, -239.86)),
		(Set((122, 0)),(96.18, 346.77)), (Set((122, 1)),(-222.71, 116.5)),
		(Set((123, 0)),(168.62, 312.56)), (Set((123, 1)),(-19.57, 152.74)),
		(Set((124, 0)),(-253.48, 572.58)), (Set((124, 1)),(597.88, 601.88)),
		(Set((125, 0)),(179.7, 645.01)), (Set((125, 1)),(111.11, 422.38)),
		(Set((126, 0)),(-29.89, 461.28)), (Set((126, 1)),(-87.67, 438.77)),
		(Set((127, 0)),(-218.92, 609.46)), (Set((127, 1)),(230.21, 501.94))))
	)
/* m: (real part of signal / Fourier coeff., imaginary part of signal / Fourier coeff. ) */
        if (m.numRows() == 1)
            m
        else {
            val scalar: Real = 1
            val Pi: Real = 3.1415926
            val n: Int = m.numRows()   /* signal length, has to be power of 2 */
            val direction: Vector = Vector(List(0.0, -2.0))
            val evens: Matrix = fftmatrix(m.everyNth(2, 0))
            val odds: Matrix = fftmatrix(m.everyNth(2, 1))

            val resleft: Matrix = evens.enumRowsMap((k:Int, x:Vector) => {
                //val (k, x) = y
                val base: Vector = x / scalar
                val offset: Vector = (direction.*(Pi * k / n)).exp() x odds.row(k) / scalar // here vector multiplication should be specialy defined for complex numbers? is it the same as cross product of vectors?
                base + offset
            })
            val resright: Matrix = evens.enumRowsMap((k:Int, x:Vector)  => {
                //val (k, x) = y
                val base: Vector = x / scalar
                val offset: Vector = (direction.*(Pi * k / n)).exp() x odds.row(k) / scalar // here vector multiplication should be specialy defined for complex numbers?
                base - offset
            })

            resleft ++ resright
        }
    }


}