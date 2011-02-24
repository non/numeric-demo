package demo

import scala.math

trait Numeric2[@specialized A] extends Convertable[A] with Ordering[A] {
  def abs(a:A): A
  override def compare(a:A, b:A): Int = if (lt(a, b)) -1 else if (gt(a, b)) 1 else 0
  def div(a:A, b:A): A
  override def equiv(a:A, b:A): Boolean
  override def gt(a:A, b:A): Boolean
  override def gteq(a:A, b:A): Boolean
  override def lt(a:A, b:A): Boolean
  override def lteq(a:A, b:A): Boolean
  override def max(a:A, b:A): A
  override def min(a:A, b:A): A
  def minus(a:A, b:A): A
  def mod(a:A, b:A): A
  def negate(a:A): A
  def one(): A
  def plus(a:A, b:A): A
  def signum(a:A): Int = compare(a, zero)
  def times(a:A, b:A): A
  def zero(): A

  def fromType[B](b:B)(implicit c:Convertable[B]): A

  class Ops(lhs:A) {
    def abs(): A = Numeric2.this.abs(lhs)
    def compare(rhs:A) = Numeric2.this.compare(lhs, rhs)
    def /(rhs:A) = div(lhs, rhs)
    def equiv(rhs:A) = Numeric2.this.equiv(lhs, rhs)
    def >(rhs:A) = gt(lhs, rhs)
    def >=(rhs:A) = gteq(lhs, rhs)
    def <(rhs:A) = lt(lhs, rhs)
    def <=(rhs:A) = lteq(lhs, rhs)
    def max(rhs:A): A = Numeric2.this.max(lhs, rhs)
    def min(rhs:A): A = Numeric2.this.min(lhs, rhs)
    def -(rhs:A) = minus(lhs, rhs)
    def %(rhs:A) = mod(lhs, rhs)
    def unary_-() = negate(lhs)
    def plus(rhs:A) = Numeric2.this.plus(lhs, rhs)
    def +(rhs:A) = Numeric2.this.plus(lhs, rhs)
    def signum(): Int = Numeric2.this.signum(lhs)
    def *(rhs:A) = times(lhs, rhs)
  
    def toByte(): Byte = Numeric2.this.toByte(lhs)
    def toShort(): Short = Numeric2.this.toShort(lhs)
    def toInt(): Int = Numeric2.this.toInt(lhs)
    def toLong(): Long = Numeric2.this.toLong(lhs)
    def toFloat(): Float = Numeric2.this.toFloat(lhs)
    def toDouble(): Double = Numeric2.this.toDouble(lhs)    
  }

  implicit def mkNumeric2Ops(lhs:A): Ops = new Ops(lhs)
}

object Numeric2 {
  trait Numeric2Byte extends Numeric2[Byte] with Convertable.ConvertableByte {
    def abs(a:Byte): Byte = scala.math.abs(a).toByte
    def div(a:Byte, b:Byte): Byte = (a / b).toByte
    override def equiv(a:Byte, b:Byte): Boolean = a == b
    override def gt(a:Byte, b:Byte): Boolean = a > b
    override def gteq(a:Byte, b:Byte): Boolean = a >= b
    override def lt(a:Byte, b:Byte): Boolean = a < b
    override def lteq(a:Byte, b:Byte): Boolean = a <= b
    override def max(a:Byte, b:Byte): Byte = scala.math.max(a, b).toByte
    override def min(a:Byte, b:Byte): Byte = scala.math.min(a, b).toByte
    def minus(a:Byte, b:Byte): Byte = (a - b).toByte
    def mod(a:Byte, b:Byte): Byte = (a % b).toByte
    def negate(a:Byte): Byte = (-a).toByte
    def one: Byte = 1
    def plus(a:Byte, b:Byte): Byte = (a + b).toByte
    def times(a:Byte, b:Byte): Byte = (a * b).toByte
    def zero: Byte = 0

    def fromType[B](b:B)(implicit c:Convertable[B]) = c.toByte(b)
  }
  implicit object Numeric2Byte extends Numeric2Byte

  trait Numeric2Short extends Numeric2[Short] with Convertable.ConvertableShort {
    def abs(a:Short): Short = scala.math.abs(a).toShort
    def div(a:Short, b:Short): Short = (a / b).toShort
    override def equiv(a:Short, b:Short): Boolean = a == b
    override def gt(a:Short, b:Short): Boolean = a > b
    override def gteq(a:Short, b:Short): Boolean = a >= b
    override def lt(a:Short, b:Short): Boolean = a < b
    override def lteq(a:Short, b:Short): Boolean = a <= b
    override def max(a:Short, b:Short): Short = scala.math.max(a, b).toShort
    override def min(a:Short, b:Short): Short = scala.math.min(a, b).toShort
    def minus(a:Short, b:Short): Short = (a - b).toShort
    def mod(a:Short, b:Short): Short = (a % b).toShort
    def negate(a:Short): Short = (-a).toShort
    def one: Short = 1
    def plus(a:Short, b:Short): Short = (a + b).toShort
    def times(a:Short, b:Short): Short = (a * b).toShort
    def zero: Short = 0
    
    def fromType[@specialized B](b:B)(implicit c:Convertable[B]) = c.toShort(b)
  }
  implicit object Numeric2Short extends Numeric2Short
  
  trait Numeric2Int extends Numeric2[Int] with Convertable.ConvertableInt {
    def abs(a:Int): Int = scala.math.abs(a)
    def div(a:Int, b:Int): Int = a / b
    override def equiv(a:Int, b:Int): Boolean = a == b
    override def gt(a:Int, b:Int): Boolean = a > b
    override def gteq(a:Int, b:Int): Boolean = a >= b
    override def lt(a:Int, b:Int): Boolean = a < b
    override def lteq(a:Int, b:Int): Boolean = a <= b
    override def max(a:Int, b:Int): Int = scala.math.max(a, b)
    override def min(a:Int, b:Int): Int = scala.math.min(a, b)
    def minus(a:Int, b:Int): Int = a - b
    def mod(a:Int, b:Int): Int = a % b
    def negate(a:Int): Int = -a
    def one: Int = 1
    def plus(a:Int, b:Int): Int = a + b
    def times(a:Int, b:Int): Int = a * b
    def zero: Int = 0
  
    def fromType[@specialized B](b:B)(implicit c:Convertable[B]) = c.toInt(b)
  }
  implicit object Numeric2Int extends Numeric2Int
  
  trait Numeric2Long extends Numeric2[Long] with Convertable.ConvertableLong {
    def abs(a:Long): Long = scala.math.abs(a)
    def div(a:Long, b:Long): Long = a / b
    override def equiv(a:Long, b:Long): Boolean = a == b
    override def gt(a:Long, b:Long): Boolean = a > b
    override def gteq(a:Long, b:Long): Boolean = a >= b
    override def lt(a:Long, b:Long): Boolean = a < b
    override def lteq(a:Long, b:Long): Boolean = a <= b
    override def max(a:Long, b:Long): Long = scala.math.max(a, b)
    override def min(a:Long, b:Long): Long = scala.math.min(a, b)
    def minus(a:Long, b:Long): Long = a - b
    def mod(a:Long, b:Long): Long = a % b
    def negate(a:Long): Long = -a
    def one: Long = 1L
    def plus(a:Long, b:Long): Long = a + b
    def times(a:Long, b:Long): Long = a * b
    def zero: Long = 0L
  
    def fromType[@specialized B](b:B)(implicit c:Convertable[B]) = c.toLong(b)
  }
  implicit object Numeric2Long extends Numeric2Long
  
  trait Numeric2Float extends Numeric2[Float] with Convertable.ConvertableFloat {
    def abs(a:Float): Float = scala.math.abs(a)
    def div(a:Float, b:Float): Float = a / b
    override def equiv(a:Float, b:Float): Boolean = a == b
    override def gt(a:Float, b:Float): Boolean = a > b
    override def gteq(a:Float, b:Float): Boolean = a >= b
    override def lt(a:Float, b:Float): Boolean = a < b
    override def lteq(a:Float, b:Float): Boolean = a <= b
    override def max(a:Float, b:Float): Float = scala.math.max(a, b)
    override def min(a:Float, b:Float): Float = scala.math.min(a, b)
    def minus(a:Float, b:Float): Float = a - b
    def mod(a:Float, b:Float): Float = a % b
    def negate(a:Float): Float = -a
    def one: Float = 1.0F
    def plus(a:Float, b:Float): Float = a + b
    def times(a:Float, b:Float): Float = a * b
    def zero: Float = 0.0F
  
    def fromType[@specialized B](b:B)(implicit c:Convertable[B]) = c.toFloat(b)
  }
  implicit object Numeric2Float extends Numeric2Float
  
  trait Numeric2Double extends Numeric2[Double] with Convertable.ConvertableDouble {
    def abs(a:Double): Double = scala.math.abs(a)
    def div(a:Double, b:Double): Double = a / b
    override def equiv(a:Double, b:Double): Boolean = a == b
    override def gt(a:Double, b:Double): Boolean = a > b
    override def gteq(a:Double, b:Double): Boolean = a >= b
    override def lt(a:Double, b:Double): Boolean = a < b
    override def lteq(a:Double, b:Double): Boolean = a <= b
    override def max(a:Double, b:Double): Double = scala.math.max(a, b)
    override def min(a:Double, b:Double): Double = scala.math.min(a, b)
    def minus(a:Double, b:Double): Double = a - b
    def mod(a:Double, b:Double): Double = a % b
    def negate(a:Double): Double = -a
    def one: Double = 1.0
    def plus(a:Double, b:Double): Double = a + b
    def times(a:Double, b:Double): Double = a * b
    def zero: Double = 0.0
  
    def fromType[@specialized B](b:B)(implicit c:Convertable[B]) = c.toDouble(b)
  }
  implicit object Numeric2Double extends Numeric2Double

}
