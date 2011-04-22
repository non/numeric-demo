package demo

import scala.math

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

  def toByte() = n.toByte(lhs)
  def toShort() = n.toShort(lhs)
  def toInt() = n.toInt(lhs)
  def toLong() = n.toLong(lhs)
  def toFloat() = n.toFloat(lhs)
  def toDouble() = n.toDouble(lhs)
}

trait Numeric[@specialized A] extends ConvertableFrom[A] with ConvertableTo[A] {
  def abs(a:A): A
  def compare(a:A, b:A): Int = if (lt(a, b)) -1 else if (gt(a, b)) 1 else 0
  def div(a:A, b:A): A
  def equiv(a:A, b:A): Boolean
  def gt(a:A, b:A): Boolean
  def gteq(a:A, b:A): Boolean
  def lt(a:A, b:A): Boolean
  def lteq(a:A, b:A): Boolean
  def max(a:A, b:A): A
  def min(a:A, b:A): A
  def minus(a:A, b:A): A
  def mod(a:A, b:A): A
  def negate(a:A): A
  def one(): A
  def plus(a:A, b:A): A
  def signum(a:A): Int = compare(a, zero)
  def times(a:A, b:A): A
  def zero(): A

  def fromType[@specialized B](b:B)(implicit c:ConvertableFrom[B]): A

  implicit def mkNumericOps(a:A): NumericOps[A] = new NumericOps[A] {
    val lhs = a
    val n = Numeric.this
  }
}

object Numeric {
  implicit def infixNumericOps[T](a:T)(implicit num:Numeric[T]):NumericOps[T] = new NumericOps[T] {
    val lhs = a
    val n = num
  }

  def numeric[A:Numeric]() = implicitly [Numeric[A]]
 
  trait NumericInt extends Numeric[Int] with ConvertableFrom.ConvertableFromInt with ConvertableTo.ConvertableToInt {
    def abs(a:Int): Int = scala.math.abs(a)
    def div(a:Int, b:Int): Int = a / b
    def equiv(a:Int, b:Int): Boolean = a == b
    def gt(a:Int, b:Int): Boolean = a > b
    def gteq(a:Int, b:Int): Boolean = a >= b
    def lt(a:Int, b:Int): Boolean = a < b
    def lteq(a:Int, b:Int): Boolean = a <= b
    def max(a:Int, b:Int): Int = scala.math.max(a, b)
    def min(a:Int, b:Int): Int = scala.math.min(a, b)
    def minus(a:Int, b:Int): Int = a - b
    def mod(a:Int, b:Int): Int = a % b
    def negate(a:Int): Int = -a
    def one: Int = 1
    def plus(a:Int, b:Int): Int = a + b
    def times(a:Int, b:Int): Int = a * b
    def zero: Int = 0
  
    def fromType[@specialized B](b:B)(implicit c:ConvertableFrom[B]) = c.toInt(b)
  }
  implicit object NumericInt extends NumericInt
  
  trait NumericLong extends Numeric[Long] with ConvertableFrom.ConvertableFromLong with ConvertableTo.ConvertableToLong {
    def abs(a:Long): Long = scala.math.abs(a)
    def div(a:Long, b:Long): Long = a / b
    def equiv(a:Long, b:Long): Boolean = a == b
    def gt(a:Long, b:Long): Boolean = a > b
    def gteq(a:Long, b:Long): Boolean = a >= b
    def lt(a:Long, b:Long): Boolean = a < b
    def lteq(a:Long, b:Long): Boolean = a <= b
    def max(a:Long, b:Long): Long = scala.math.max(a, b)
    def min(a:Long, b:Long): Long = scala.math.min(a, b)
    def minus(a:Long, b:Long): Long = a - b
    def mod(a:Long, b:Long): Long = a % b
    def negate(a:Long): Long = -a
    def one: Long = 1L
    def plus(a:Long, b:Long): Long = a + b
    def times(a:Long, b:Long): Long = a * b
    def zero: Long = 0L
  
    def fromType[@specialized B](b:B)(implicit c:ConvertableFrom[B]) = c.toLong(b)
  }
  implicit object NumericLong extends NumericLong
  
  trait NumericFloat extends Numeric[Float] with ConvertableFrom.ConvertableFromFloat with ConvertableTo.ConvertableToFloat {
    def abs(a:Float): Float = scala.math.abs(a)
    def div(a:Float, b:Float): Float = a / b
    def equiv(a:Float, b:Float): Boolean = a == b
    def gt(a:Float, b:Float): Boolean = a > b
    def gteq(a:Float, b:Float): Boolean = a >= b
    def lt(a:Float, b:Float): Boolean = a < b
    def lteq(a:Float, b:Float): Boolean = a <= b
    def max(a:Float, b:Float): Float = scala.math.max(a, b)
    def min(a:Float, b:Float): Float = scala.math.min(a, b)
    def minus(a:Float, b:Float): Float = a - b
    def mod(a:Float, b:Float): Float = a % b
    def negate(a:Float): Float = -a
    def one: Float = 1.0F
    def plus(a:Float, b:Float): Float = a + b
    def times(a:Float, b:Float): Float = a * b
    def zero: Float = 0.0F
  
    def fromType[@specialized B](b:B)(implicit c:ConvertableFrom[B]) = c.toFloat(b)
  }
  implicit object NumericFloat extends NumericFloat
  
  trait NumericDouble extends Numeric[Double] with ConvertableFrom.ConvertableFromDouble with ConvertableTo.ConvertableToDouble {
    def abs(a:Double): Double = scala.math.abs(a)
    def div(a:Double, b:Double): Double = a / b
    def equiv(a:Double, b:Double): Boolean = a == b
    def gt(a:Double, b:Double): Boolean = a > b
    def gteq(a:Double, b:Double): Boolean = a >= b
    def lt(a:Double, b:Double): Boolean = a < b
    def lteq(a:Double, b:Double): Boolean = a <= b
    def max(a:Double, b:Double): Double = scala.math.max(a, b)
    def min(a:Double, b:Double): Double = scala.math.min(a, b)
    def minus(a:Double, b:Double): Double = a - b
    def mod(a:Double, b:Double): Double = a % b
    def negate(a:Double): Double = -a
    def one: Double = 1.0
    def plus(a:Double, b:Double): Double = a + b
    def times(a:Double, b:Double): Double = a * b
    def zero: Double = 0.0
  
    def fromType[@specialized B](b:B)(implicit c:ConvertableFrom[B]) = c.toDouble(b)
  }
  implicit object NumericDouble extends NumericDouble

}
