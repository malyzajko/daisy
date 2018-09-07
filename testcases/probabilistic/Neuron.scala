

import daisy.lang._
import Real._


object Neuron {

  def neuron(x1 : Real, x2: Real) = {
    require(1 <= x1 && x1 <= 1 && 0.4 <= x2 && x2 <= 0.5)
    1/(1+exp(-(-2.9389/(1+exp(-(5.6706*x1+0.5058*x2+12.4057)))+28.6263/(1+exp(-(17.5208*x2+13.9847*x1-21.5653)))-14.1628)))
  }

}