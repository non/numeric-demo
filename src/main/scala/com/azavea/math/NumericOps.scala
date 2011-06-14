package com.azavea.math

trait NumericOps[@specialized A] {
  val lhs:A
  val n:Numeric[A]

  def abs() = n.abs(lhs)
  def compare(rhs:A) = n.compare(lhs, rhs)
  def /(rhs:A) = n.div(lhs, rhs)
  def equiv(rhs:A) = n.equiv(lhs, rhs)
  def >(rhs:A) = n.gt(lhs, rhs)
  def >=(rhs:A) = n.gteq(lhs, rhs)
  def <(rhs:A) = n.lt(lhs, rhs)
  def <=(rhs:A) = n.lteq(lhs, rhs)
  def max(rhs:A) = n.max(lhs, rhs)
  def min(rhs:A) = n.min(lhs, rhs)
  def -(rhs:A) = n.minus(lhs, rhs)
  def %(rhs:A) = n.mod(lhs, rhs)
  def unary_-() = n.negate(lhs)
  def plus(rhs:A) = n.plus(lhs, rhs)
  def +(rhs:A) = n.plus(lhs, rhs)
  def signum() = n.signum(lhs)
  def *(rhs:A) = n.times(lhs, rhs)
  def **(rhs:A) = n.pow(lhs, rhs)

  def toByte() = n.toByte(lhs)
  def toShort() = n.toShort(lhs)
  def toInt() = n.toInt(lhs)
  def toLong() = n.toLong(lhs)
  def toFloat() = n.toFloat(lhs)
  def toDouble() = n.toDouble(lhs)
}

class NumericOpsCls[@specialized A:Numeric](a:A) extends NumericOps[A] {
  val lhs = a
  val n = implicitly[Numeric[A]]
}


