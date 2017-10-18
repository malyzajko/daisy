import daisy.lang._
import Real._


object BatchProcessorSingle_state1 {

  def BatchProcessorSingle_state1_tillM75(y1: Real) = { // s1: Real, s2: Real, s3: Real, s4: Real,
    require(//-10 <= s1 && s1 <= 10 && -10 <= s2 && s2 <= 10 && -10 <= s3 && s3 <= 10 && -10 <= s4 && s4 <= 10 &&
      -10 <= y1 && y1 <= -7.5)
    //0.9670 * 0.378 + (-0.0019) * -1.65 + 0.0187 * 6.13 + (-0.0088) * -4.175 + 0.0447 * y1
    0.520032 + (0.0447 * y1)
  }

 def BatchProcessorSingle_state1_tillM5(y1: Real) = { // s1: Real, s2: Real, s3: Real, s4: Real,
    require(//-10 <= s1 && s1 <= 10 && -10 <= s2 && s2 <= 10 && -10 <= s3 && s3 <= 10 && -10 <= s4 && s4 <= 10 &&
      -7.5 <= y1 && y1 <= -5)
    //0.9670 * 0.378 + (-0.0019) * -1.65 + 0.0187 * 6.13 + (-0.0088) * -4.175 + 0.0447 * y1
    0.520032 + (0.0447 * y1)
  }
   def BatchProcessorSingle_state1_tillM25(y1: Real) = { // s1: Real, s2: Real, s3: Real, s4: Real,
    require(//-10 <= s1 && s1 <= 10 && -10 <= s2 && s2 <= 10 && -10 <= s3 && s3 <= 10 && -10 <= s4 && s4 <= 10 &&
      -5 <= y1 && y1 <= -2.5)
    //0.9670 * 0.378 + (-0.0019) * -1.65 + 0.0187 * 6.13 + (-0.0088) * -4.175 + 0.0447 * y1
    0.520032 + (0.0447 * y1)
  }
   def BatchProcessorSingle_state1_till0(y1: Real) = { // s1: Real, s2: Real, s3: Real, s4: Real,
    require(//-10 <= s1 && s1 <= 10 && -10 <= s2 && s2 <= 10 && -10 <= s3 && s3 <= 10 && -10 <= s4 && s4 <= 10 &&
      -2.5 <= y1 && y1 <= 0)
    //0.9670 * 0.378 + (-0.0019) * -1.65 + 0.0187 * 6.13 + (-0.0088) * -4.175 + 0.0447 * y1
    0.520032 + (0.0447 * y1)
  }
   def BatchProcessorSingle_state1_till25(y1: Real) = { // s1: Real, s2: Real, s3: Real, s4: Real,
    require(//-10 <= s1 && s1 <= 10 && -10 <= s2 && s2 <= 10 && -10 <= s3 && s3 <= 10 && -10 <= s4 && s4 <= 10 &&
      0 <= y1 && y1 <= 2.5)
    //0.9670 * 0.378 + (-0.0019) * -1.65 + 0.0187 * 6.13 + (-0.0088) * -4.175 + 0.0447 * y1
    0.520032 + (0.0447 * y1)
  }
   def BatchProcessorSingle_state1_till5(y1: Real) = { // s1: Real, s2: Real, s3: Real, s4: Real,
    require(//-10 <= s1 && s1 <= 10 && -10 <= s2 && s2 <= 10 && -10 <= s3 && s3 <= 10 && -10 <= s4 && s4 <= 10 &&
      2.5 <= y1 && y1 <= 5)
    //0.9670 * 0.378 + (-0.0019) * -1.65 + 0.0187 * 6.13 + (-0.0088) * -4.175 + 0.0447 * y1
    0.520032 + (0.0447 * y1)
  }
   def BatchProcessorSingle_state1_till75(y1: Real) = { // s1: Real, s2: Real, s3: Real, s4: Real,
    require(//-10 <= s1 && s1 <= 10 && -10 <= s2 && s2 <= 10 && -10 <= s3 && s3 <= 10 && -10 <= s4 && s4 <= 10 &&
      5 <= y1 && y1 <= 7.5)
    //0.9670 * 0.378 + (-0.0019) * -1.65 + 0.0187 * 6.13 + (-0.0088) * -4.175 + 0.0447 * y1
    0.520032 + (0.0447 * y1)
  }
   def BatchProcessorSingle_state1_till10(y1: Real) = { // s1: Real, s2: Real, s3: Real, s4: Real,
    require(//-10 <= s1 && s1 <= 10 && -10 <= s2 && s2 <= 10 && -10 <= s3 && s3 <= 10 && -10 <= s4 && s4 <= 10 &&
      7.5 <= y1 && y1 <= 10)
    //0.9670 * 0.378 + (-0.0019) * -1.65 + 0.0187 * 6.13 + (-0.0088) * -4.175 + 0.0447 * y1
    0.520032 + (0.0447 * y1)
  }
}