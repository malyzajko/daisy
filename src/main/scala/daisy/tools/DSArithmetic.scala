

package daisy
package tools


trait DSArithmetic[T] { self: T =>



  def +(other: T): T
  def -(other: T): T
  def *(other: T): T
  def /(other: T): T
  def squareRoot: T
  def inverse: T
  def toDSI: Interval

}