import daisy.lang._
import Real._
import daisy.lang.Vector._

object fftmatrix {

	def fftmatrix(m: Matrix): Matrix = {
require(m >= -326.68 && m <= 677.57 && m.size(512,2)
	 && m.specM(Set((Set((58, 1)),(-213.17, 536.36)), (Set((4, 0)),(49.36, 660.55)),
		(Set((56, 1), (45, 1)),(436.68, 620.2)), (Set((441, 1), (270, 0), (19, 0), (456, 1)),(-137.42, 555.38)),
		(Set((402, 0), (164, 0)),(68.19, 323.59)), (Set((380, 0), (151, 0), (509, 1), (392, 1), (7, 1)),(-250.67, 315.88)),
		(Set((472, 0), (87, 0), (494, 0), (459, 0)),(-232.04, 11.95)), (Set((221, 0), (243, 0), (141, 1), (364, 0), (498, 0)),(-238.52, 606.12)),
		(Set((326, 1), (99, 1), (88, 1), (471, 1), (209, 1), (330, 1), (114, 1)),(44.39, 447.18)), (Set((194, 0), (183, 0)),(-188.83, 250.86)),
		(Set((170, 0), (53, 0)),(-268.63, 523.42)), (Set((433, 1), (182, 1), (262, 0), (314, 1), (292, 1), (251, 0), (327, 1)),(289.63, 552.09)),
		(Set((316, 1), (76, 1), (372, 0)),(68.58, 325.81)), (Set((65, 1), (121, 0), (132, 0), (387, 0), (15, 0)),(-82.43, 419.86)),
		(Set((133, 1), (224, 0), (254, 1), (213, 0)),(-137.81, -86.09)), (Set((334, 0), (3, 1), (14, 1), (490, 0), (148, 1), (455, 0), (83, 0)),(80.65, 534.76)),
		(Set((239, 0), (360, 0), (349, 0), (122, 0), (201, 1), (333, 1), (478, 1), (467, 1)),(-315.91, -154.32)), (Set((216, 1), (307, 0), (296, 0), (417, 0)),(-163.95, -52.7)),
		(Set((155, 0), (231, 1), (456, 0), (60, 0), (163, 1), (284, 1), (57, 1), (44, 1)),(-279.61, 618.13)), (Set((33, 1)),(44.42, 392.32)),
		(Set((509, 0), (178, 1), (485, 0), (189, 1), (323, 1), (258, 0)),(-87.31, 67.17)), (Set((310, 1), (72, 1), (7, 0)),(-260.01, 127.67)),
		(Set((444, 1), (152, 0), (193, 1), (394, 0), (380, 1), (75, 0), (209, 0), (196, 0)),(161.07, 360.47)), (Set((330, 0)),(-276.82, -116.36)),
		(Set((341, 0), (406, 1), (486, 0), (10, 1), (356, 0), (459, 1), (63, 1)),(134.79, 573.69)), (Set((208, 1), (342, 1), (288, 0)),(-295.61, -66.37)),
		(Set((318, 1), (329, 1), (409, 0), (91, 1), (37, 0), (182, 0), (223, 1)),(627.06, 666.93)), (Set((212, 1), (292, 0)),(-154.21, 291.95)),
		(Set((314, 0)),(-21.11, 560.73)), (Set((448, 0), (41, 0), (25, 1), (170, 1), (280, 1), (64, 1)),(-33.43, 312.54)),
		(Set((29, 1), (109, 0)),(-255.2, 352.48)), (Set((185, 1), (174, 1)),(125.25, 302.96)),
		(Set((265, 0), (3, 0), (375, 0), (148, 0), (135, 0), (372, 1), (132, 1)),(29.36, 259.61)), (Set((504, 1), (493, 1), (333, 0), (26, 1), (322, 0)),(-125.2, 307.15)),
		(Set((478, 0), (71, 0)),(277.96, 397.53)), (Set((82, 0), (387, 1), (227, 0)),(-204.77, 579.59)),
		(Set((147, 1), (443, 0), (97, 0), (231, 0), (94, 1)),(171.85, 309.94)), (Set((70, 1), (59, 1), (295, 0), (481, 1)),(-15.61, 99.08)),
		(Set((33, 0), (470, 1), (167, 0), (189, 0)),(-17.95, -0.76)), (Set((178, 0), (219, 1), (416, 0), (165, 0)),(-52.76, 565.07)),
		(Set((154, 0), (444, 0), (420, 0), (193, 0)),(-260.81, 333.64)), (Set((497, 0), (155, 1), (166, 1), (177, 1), (311, 1), (432, 1), (406, 0), (10, 0)),(-133.64, 94.37)),
		(Set((131, 0)),(597.01, 675.27)), (Set((234, 1), (390, 1), (63, 0)),(311.79, 426.53)),
		(Set((500, 1), (128, 1), (208, 0), (249, 1), (238, 1), (22, 1), (340, 0), (439, 0)),(375.53, 638.69)), (Set((78, 0), (89, 0), (212, 0), (264, 1)),(-254.55, 504.93)),
		(Set((93, 0), (196, 1), (352, 1)),(402.72, 653.26)), (Set((473, 1), (66, 1), (25, 0), (397, 0), (211, 1), (90, 1), (356, 1), (291, 0)),(-312.51, 321.3)),
		(Set((412, 0), (423, 0), (40, 0)),(-163.39, 37.92)), (Set((174, 0), (161, 0), (306, 0), (158, 1), (17, 1)),(-274.49, 475.47)),
		(Set((28, 1), (504, 0), (173, 1), (363, 0), (385, 0)),(-227.31, 444.0)), (Set((80, 1), (69, 1), (376, 0)),(216.33, 260.47)),
		(Set((510, 0), (259, 0)),(-280.85, -175.14)), (Set((8, 0), (137, 1)),(302.74, 570.1)),
		(Set((338, 0), (273, 1)),(315.03, 498.94)), (Set((486, 1)),(-311.46, 374.95)),
		(Set((38, 0)),(-196.85, 614.13)), (Set((436, 0), (303, 1)),(14.48, 457.09)),
		(Set((448, 1), (11, 0), (398, 0)),(77.89, 145.16)), (Set((136, 0), (371, 1), (505, 1)),(-159.69, -114.57)),
		(Set((410, 1)),(226.89, 404.82)), (Set((466, 0), (361, 1)),(-158.39, 317.94)),
		(Set((110, 1), (166, 0), (311, 0)),(241.26, 248.65)), (Set((295, 1)),(-255.5, 423.83)),
		(Set((167, 1)),(-195.42, -168.63)), (Set((368, 0), (379, 0), (139, 0)),(153.74, 433.08)),
		(Set((391, 1), (151, 1), (140, 1)),(-169.61, 122.57)), (Set((79, 0), (101, 0)),(43.17, 118.84)),
		(Set((235, 0)),(-0.81, 460.94)), (Set((353, 1)),(-174.05, 446.09)),
		(Set((102, 1)),(74.03, 326.27)), (Set((186, 0), (421, 1)),(377.68, 524.2)),
		(Set((159, 1), (371, 0), (505, 0)),(-92.41, 506.71)), (Set((121, 1), (467, 0)),(-188.24, 103.37)),
		(Set((257, 1)),(-104.11, 156.03)), (Set((284, 0), (44, 0), (287, 1)),(-326.5, 509.12)),
		(Set((181, 1)),(-306.13, 576.32)), (Set((382, 0), (113, 1), (355, 1)),(-289.07, 420.62)),
		(Set((511, 1)),(-235.93, 531.91)), (Set((450, 0), (143, 1)),(-218.4, 541.07)),
		(Set((344, 0), (317, 1)),(568.94, 572.86)), (Set((451, 1), (462, 1), (200, 1)),(-263.86, 456.77)),
		(Set((222, 1), (401, 0)),(-109.03, 583.07)), (Set((150, 0), (55, 0)),(153.0, 402.51)),
		(Set((279, 1), (424, 1), (435, 1)),(-280.15, 133.28)), (Set((413, 1), (162, 1)),(84.68, 323.06)),
		(Set((480, 0)),(234.02, 532.97)), (Set((257, 0), (268, 0)),(292.43, 472.7)),
		(Set((6, 0), (492, 1)),(-59.47, 325.35)), (Set((386, 1), (135, 1), (74, 0)),(306.34, 614.38)),
		(Set((219, 0)),(-120.34, 67.54)), (Set((443, 1), (454, 1)),(102.37, 164.25)),
		(Set((203, 1), (337, 1)),(243.0, 470.04)), (Set((276, 0), (432, 0), (47, 0)),(155.58, 649.83)),
		(Set((405, 1), (165, 1)),(-62.56, 429.75)), (Set((154, 1)),(301.07, 428.0)),
		(Set((48, 1), (355, 0)),(27.39, 250.0)), (Set((500, 0)),(-191.56, 58.81)),
		(Set((104, 0), (489, 0), (238, 0)),(-286.91, -241.27)), (Set((260, 0)),(83.72, 623.87)),
		(Set((143, 0), (378, 1), (367, 1)),(508.72, 649.33)), (Set((105, 1)),(527.26, 594.97)),
		(Set((317, 0), (503, 1), (66, 0)),(279.37, 354.34)), (Set((200, 0), (252, 1), (184, 1)),(-302.34, 197.19)),
		(Set((67, 1)),(265.18, 589.72)), (Set((89, 1)),(319.43, 380.54)),
		(Set((320, 1)),(-88.38, -47.44)), (Set((465, 1)),(61.52, 310.23)),
		(Set((17, 0), (214, 1)),(-320.93, 265.02)), (Set((415, 0), (492, 0), (241, 0)),(279.02, 317.56)),
		(Set((230, 0), (20, 1)),(235.3, 519.64)), (Set((126, 0)),(-283.33, 78.02)),
		(Set((350, 1), (506, 1)),(-302.93, 671.13)), (Set((495, 1), (203, 0)),(-178.6, 155.78)),
		(Set((233, 1)),(-208.13, 21.09)), (Set((445, 0), (328, 0)),(-101.44, 482.12)),
		(Set((446, 1)),(334.72, 590.89)), (Set((50, 1), (457, 1), (396, 0)),(493.02, 541.15)),
		(Set((407, 0), (290, 0), (274, 1)),(-233.24, -58.63)), (Set((263, 1)),(-175.86, 439.23)),
		(Set((12, 1)),(-189.17, 412.33)), (Set((157, 1), (475, 0)),(590.14, 657.65)),
		(Set((168, 1)),(-133.89, -104.88)), (Set((358, 0), (347, 0), (503, 0)),(214.96, 595.63)),
		(Set((96, 0)),(-1.38, 13.97)), (Set((118, 0)),(173.96, 514.17)),
		(Set((1, 0), (487, 1)),(-302.64, -247.53)), (Set((476, 1), (236, 1)),(-87.54, 121.26)),
		(Set((426, 0), (119, 1)),(-296.86, 307.39)), (Set((309, 0), (465, 0)),(443.2, 594.92)),
		(Set((80, 0), (187, 1)),(416.38, 620.48)), (Set((198, 1), (81, 1), (388, 0)),(-173.47, 206.96)),
		(Set((137, 0)),(-15.11, 314.5)), (Set((271, 0)),(-217.18, -78.94)),
		(Set((20, 0), (31, 0)),(47.03, 317.65)), (Set((4, 1)),(156.16, 463.04)),
		(Set((138, 1), (205, 0)),(215.94, 493.67)), (Set((350, 0), (32, 1), (43, 1)),(-53.04, 300.12)),
		(Set((495, 0)),(-321.74, 136.26)), (Set((244, 0), (285, 1)),(-274.75, -89.19)),
		(Set((217, 1), (111, 1)),(49.06, 545.31)), (Set((100, 1), (312, 0)),(-76.57, -59.36)),
		(Set((301, 0), (457, 0), (61, 0)),(79.27, 304.34)), (Set((50, 0), (206, 0)),(326.92, 575.14)),
		(Set((195, 0)),(-260.5, 188.51)), (Set((315, 1), (12, 0), (157, 0)),(-316.24, 326.89)),
		(Set((168, 0)),(305.32, 418.7)), (Set((410, 0), (236, 0), (277, 1)),(-269.32, 280.74)),
		(Set((266, 1), (225, 0), (361, 0)),(8.4, 152.59)), (Set((92, 1)),(5.6, 42.67)),
		(Set((345, 1), (490, 1), (479, 1)),(-75.87, 677.25)), (Set((228, 1), (429, 0), (440, 0)),(-276.94, 399.18)),
		(Set((72, 0), (255, 0), (296, 1)),(-39.16, 253.42))))
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