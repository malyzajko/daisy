// Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy
package tools


trait RangeArithmetic[T] { self: T =>

  def unary_- : T
  def +(other: T): T
  def -(other: T): T
  def *(other: T): T
  def *(r: Rational): T
  def /(other: T): T
  //  def ^(n: T): T
  def ^(n: Int): T
  def squareRoot: T
  def inverse: T
  def sine: T
  def cosine: T
  def tangent: T
  def arcsine: T
  def arccosine: T
  def arctangent: T
  def exp: T
  def log: T
  def toInterval: Interval

  /* Add an error */
  def +/-(r: Rational): T

  def addConstraint(e: Set[lang.Trees.Expr]): T
}
