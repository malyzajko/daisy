
import daisy.lang._
import Real._


//Unrolled loops
object Pendulum {

  def pendulumT1(t_0: Real, w_0: Real): Real = {
    require(-2 <= t_0 && t_0 <= 2 && -5 <= w_0 && w_0 <= 5)

    val h: Real = 0.01
    val L: Real = 2.0
    val m: Real = 1.5
    val g: Real = 9.80665

    val k1t = w_0
    val k1w = -g/L * (t_0 - t_0 * t_0 * t_0/6 + t_0 * t_0 * t_0 * t_0 * t_0/120)
    val k2t = w_0 + h/2*k1w
    val arg = t_0 + h/2*k1t
    val k2w = -g/L * (arg - arg * arg * arg/6 + arg * arg * arg * arg * arg/120)
    t_0 + h*k2t
    
  }

  def pendulumW1(t_0: Real, w_0: Real): Real = {
    require(-2 <= t_0 && t_0 <= 2 && -5 <= w_0 && w_0 <= 5)

    val h: Real = 0.01
    val L: Real = 2.0
    val m: Real = 1.5
    val g: Real = 9.80665

    val k1t = w_0
    val k1w = -g/L * (t_0 - t_0 * t_0 * t_0/6 + t_0 * t_0 * t_0 * t_0 * t_0/120)
    val k2t = w_0 + h/2*k1w
    val arg = t_0 + h/2*k1t
    val k2w = -g/L * (arg - arg * arg * arg/6 + arg * arg * arg * arg * arg/120)
    w_0 + h*k2w
  }

  // TODO: takes forever, maybe because of Rationals getting too big?
  def pendulumT3(t_0: Real, w_0: Real): Real = {
    require(-2 <= t_0 && t_0 <= 2 && -5 <= w_0 && w_0 <= 5)

    val h: Real = 0.01
    val L: Real = 2.0
    val m: Real = 1.5
    val g: Real = 9.80665

    // loop 1
    val k1t_1 = w_0
    val k1w_1 = -g/L * (t_0 - t_0 * t_0 * t_0/6 + t_0 * t_0 * t_0 * t_0 * t_0/120)
    val k2t_1 = w_0 + h/2*k1w_1
    val arg_1 = t_0 + h/2*k1t_1
    val k2w_1 = -g/L * (arg_1 - arg_1 * arg_1 * arg_1/6 + arg_1 * arg_1 * arg_1 * arg_1 * arg_1/120)
    
    val t_1 = t_0 + h*k2t_1
    val w_1 = w_0 + h*k2w_1

    // loop 2
    val k1t_2 = w_1
    val k1w_2 = -g/L * (t_1 - t_1 * t_1 * t_1/6 + t_1 * t_1 * t_1 * t_1 * t_1/120)
    val k2t_2 = w_1 + h/2*k1w_2
    val arg_2 = t_1 + h/2*k1t_2
    val k2w_2 = -g/L * (arg_2 - arg_2 * arg_2 * arg_2/6 + arg_2 * arg_2 * arg_2 * arg_2 * arg_2/120)
    
    val t_2 = t_1 + h*k2t_2
    val w_2 = w_1 + h*k2w_2    
    
    // loop 3
    val k1t_3 = w_2
    val k1w_3 = -g/L * (t_2 - t_2 * t_2 * t_2/6 + t_2 * t_2 * t_2 * t_2 * t_2/120)
    val k2t_3 = w_2 + h/2*k1w_3
    val arg_3 = t_2 + h/2*k1t_3
    val k2w_3 = -g/L * (arg_3 - arg_3 * arg_3 * arg_3/6 + arg_3 * arg_3 * arg_3 * arg_3 * arg_3/120)
    
    t_2 + h*k2t_3
  }

  def pendulumW3(t_0: Real, w_0: Real): Real = {
    require(-2 <= t_0 && t_0 <= 2 && -5 <= w_0 && w_0 <= 5)

    val h: Real = 0.01
    val L: Real = 2.0
    val m: Real = 1.5
    val g: Real = 9.80665

    // loop 1
    val k1t_1 = w_0
    val k1w_1 = -g/L * (t_0 - t_0 * t_0 * t_0/6 + t_0 * t_0 * t_0 * t_0 * t_0/120)
    val k2t_1 = w_0 + h/2*k1w_1
    val arg_1 = t_0 + h/2*k1t_1
    val k2w_1 = -g/L * (arg_1 - arg_1 * arg_1 * arg_1/6 + arg_1 * arg_1 * arg_1 * arg_1 * arg_1/120)
    
    val t_1 = t_0 + h*k2t_1
    val w_1 = w_0 + h*k2w_1

    // loop 2
    val k1t_2 = w_1
    val k1w_2 = -g/L * (t_1 - t_1 * t_1 * t_1/6 + t_1 * t_1 * t_1 * t_1 * t_1/120)
    val k2t_2 = w_1 + h/2*k1w_2
    val arg_2 = t_1 + h/2*k1t_2
    val k2w_2 = -g/L * (arg_2 - arg_2 * arg_2 * arg_2/6 + arg_2 * arg_2 * arg_2 * arg_2 * arg_2/120)
    
    val t_2 = t_1 + h*k2t_2
    val w_2 = w_1 + h*k2w_2    
    
    // loop 3
    val k1t_3 = w_2
    val k1w_3 = -g/L * (t_2 - t_2 * t_2 * t_2/6 + t_2 * t_2 * t_2 * t_2 * t_2/120)
    val k2t_3 = w_2 + h/2*k1w_3
    val arg_3 = t_2 + h/2*k1t_3
    val k2w_3 = -g/L * (arg_3 - arg_3 * arg_3 * arg_3/6 + arg_3 * arg_3 * arg_3 * arg_3 * arg_3/120)
    
    w_2 + h*k2w_3    
  }

  def pendulumT5(t_0: Real, w_0: Real): Real = {
    require(-2 <= t_0 && t_0 <= 2 && -5 <= w_0 && w_0 <= 5)

    val h: Real = 0.01
    val L: Real = 2.0
    val m: Real = 1.5
    val g: Real = 9.80665

    // loop 1
    val k1t_1 = w_0
    val k1w_1 = -g/L * (t_0 - t_0 * t_0 * t_0/6 + t_0 * t_0 * t_0 * t_0 * t_0/120)
    val k2t_1 = w_0 + h/2*k1w_1
    val arg_1 = t_0 + h/2*k1t_1
    val k2w_1 = -g/L * (arg_1 - arg_1 * arg_1 * arg_1/6 + arg_1 * arg_1 * arg_1 * arg_1 * arg_1/120)
    
    val t_1 = t_0 + h*k2t_1
    val w_1 = w_0 + h*k2w_1

    // loop 2
    val k1t_2 = w_1
    val k1w_2 = -g/L * (t_1 - t_1 * t_1 * t_1/6 + t_1 * t_1 * t_1 * t_1 * t_1/120)
    val k2t_2 = w_1 + h/2*k1w_2
    val arg_2 = t_1 + h/2*k1t_2
    val k2w_2 = -g/L * (arg_2 - arg_2 * arg_2 * arg_2/6 + arg_2 * arg_2 * arg_2 * arg_2 * arg_2/120)
    
    val t_2 = t_1 + h*k2t_2
    val w_2 = w_1 + h*k2w_2    
    
    // loop 3
    val k1t_3 = w_2
    val k1w_3 = -g/L * (t_2 - t_2 * t_2 * t_2/6 + t_2 * t_2 * t_2 * t_2 * t_2/120)
    val k2t_3 = w_2 + h/2*k1w_3
    val arg_3 = t_2 + h/2*k1t_3
    val k2w_3 = -g/L * (arg_3 - arg_3 * arg_3 * arg_3/6 + arg_3 * arg_3 * arg_3 * arg_3 * arg_3/120)
    
    val t_3 = t_2 + h*k2t_3
    val w_3 = w_2 + h*k2w_3  

    // loop 4
    val k1t_4 = w_3
    val k1w_4 = -g/L * (t_3 - t_3 * t_3 * t_3/6 + t_3 * t_3 * t_3 * t_3 * t_3/120)
    val k2t_4 = w_3 + h/2*k1w_4
    val arg_4 = t_3 + h/2*k1t_4
    val k2w_4 = -g/L * (arg_4 - arg_4 * arg_4 * arg_4/6 + arg_4 * arg_4 * arg_4 * arg_4 * arg_4/120)
    
    val t_4 = t_3 + h*k2t_4
    val w_4 = w_3 + h*k2w_4    

    // loop 5
    val k1t_5 = w_4
    val k1w_5 = -g/L * (t_4 - t_4 * t_4 * t_4/6 + t_4 * t_4 * t_4 * t_4 * t_4/120)
    val k2t_5 = w_4 + h/2*k1w_5
    val arg_5 = t_4 + h/2*k1t_5
    val k2w_5 = -g/L * (arg_5 - arg_5 * arg_5 * arg_5/6 + arg_5 * arg_5 * arg_5 * arg_5 * arg_5/120)
    
    t_4 + h*k2t_5
    
  }

  def pendulumW5(t_0: Real, w_0: Real): Real = {
    require(-2 <= t_0 && t_0 <= 2 && -5 <= w_0 && w_0 <= 5)

    val h: Real = 0.01
    val L: Real = 2.0
    val m: Real = 1.5
    val g: Real = 9.80665

    // loop 1
    val k1t_1 = w_0
    val k1w_1 = -g/L * (t_0 - t_0 * t_0 * t_0/6 + t_0 * t_0 * t_0 * t_0 * t_0/120)
    val k2t_1 = w_0 + h/2*k1w_1
    val arg_1 = t_0 + h/2*k1t_1
    val k2w_1 = -g/L * (arg_1 - arg_1 * arg_1 * arg_1/6 + arg_1 * arg_1 * arg_1 * arg_1 * arg_1/120)
    
    val t_1 = t_0 + h*k2t_1
    val w_1 = w_0 + h*k2w_1

    // loop 2
    val k1t_2 = w_1
    val k1w_2 = -g/L * (t_1 - t_1 * t_1 * t_1/6 + t_1 * t_1 * t_1 * t_1 * t_1/120)
    val k2t_2 = w_1 + h/2*k1w_2
    val arg_2 = t_1 + h/2*k1t_2
    val k2w_2 = -g/L * (arg_2 - arg_2 * arg_2 * arg_2/6 + arg_2 * arg_2 * arg_2 * arg_2 * arg_2/120)
    
    val t_2 = t_1 + h*k2t_2
    val w_2 = w_1 + h*k2w_2    
    
    // loop 3
    val k1t_3 = w_2
    val k1w_3 = -g/L * (t_2 - t_2 * t_2 * t_2/6 + t_2 * t_2 * t_2 * t_2 * t_2/120)
    val k2t_3 = w_2 + h/2*k1w_3
    val arg_3 = t_2 + h/2*k1t_3
    val k2w_3 = -g/L * (arg_3 - arg_3 * arg_3 * arg_3/6 + arg_3 * arg_3 * arg_3 * arg_3 * arg_3/120)
    
    val t_3 = t_2 + h*k2t_3
    val w_3 = w_2 + h*k2w_3  

    // loop 4
    val k1t_4 = w_3
    val k1w_4 = -g/L * (t_3 - t_3 * t_3 * t_3/6 + t_3 * t_3 * t_3 * t_3 * t_3/120)
    val k2t_4 = w_3 + h/2*k1w_4
    val arg_4 = t_3 + h/2*k1t_4
    val k2w_4 = -g/L * (arg_4 - arg_4 * arg_4 * arg_4/6 + arg_4 * arg_4 * arg_4 * arg_4 * arg_4/120)
    
    val t_4 = t_3 + h*k2t_4
    val w_4 = w_3 + h*k2w_4    

    // loop 5
    val k1t_5 = w_4
    val k1w_5 = -g/L * (t_4 - t_4 * t_4 * t_4/6 + t_4 * t_4 * t_4 * t_4 * t_4/120)
    val k2t_5 = w_4 + h/2*k1w_5
    val arg_5 = t_4 + h/2*k1t_5
    val k2w_5 = -g/L * (arg_5 - arg_5 * arg_5 * arg_5/6 + arg_5 * arg_5 * arg_5 * arg_5 * arg_5/120)
    
    w_4 + h*k2w_5
  }
}






