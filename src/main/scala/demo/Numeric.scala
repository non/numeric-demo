package demo

import scala.math.{abs, min, max, pow}

/**
 * @author Erik Osheim
 */


/**
 * Numeric typeclass for doing operations on generic types.
 *
 * Importantly, this package does not deliver classes for you to instantiate. Rather,
 * it gives you a trait to associated with your generic types, which allows actual
 * uses of your generic code with concrete types (e.g. Int) to link up with concrete
 * implementations (e.g. NumericInt) of Numeric's method for that type.
 *
 * @example {{{
 *   import demo.Numeric
 *   import demo.Numeric.infixNumericOps
 *
 *   def pythagoreanTheorem[T:Numeric](a:T, b:T): Double = {
 *     val c = (a * a) + (b * b)
 *     math.sqrt(c.toDouble)
 *   }
 *
 *   def 
 * }}}
 * 
 */
trait Numeric[@specialized A] extends ConvertableFrom[A] with ConvertableTo[A] {

  /**
   * Computes the absolute value of `a`.
   * 
   * @return the absolute value of `a`
   */
  def abs(a:A):A

  /**
   * Returns an integer whose sign denotes the relationship between
   * `a` and `b`. If `a` < `b` it returns -1, if `a` == `b` it returns
   * 0 and if `a` > `b` it returns 1.
   *
   * @return -1, 0 or 1
   *
   * @see math.abs
   */
  def compare(a:A, b:A):Int = if (lt(a, b)) -1 else if (gt(a, b)) 1 else 0

  /**
   * Divides `a` by `b`.
   *
   * This method maintains the type of the arguments (`A`). If this
   * method is used with `Int` or `Long` then the quotient (as in
   * integer division). Otherwise (with `Float` and `Double`) a
   * fractional result is returned.
   *
   * @return `a` / `b`
   */
  def div(a:A, b:A):A

  /**
   * Tests if `a` and `b` are equivalent.
   *
   * @return `a` == `b`
   */
  def equiv(a:A, b:A):Boolean

  /**
   * Tests if `a` is greater than `b`.
   *
   * @return `a` > `b`
   */
  def gt(a:A, b:A):Boolean

  /**
   * Tests if `a` is greater than or equal to `b`.
   *
   * @return `a` >= `b`
   */
  def gteq(a:A, b:A):Boolean

  /**
   * Tests if `a` is less than `b`.
   *
   * @return `a` <= `b`
   */
  def lt(a:A, b:A):Boolean

  /**
   * Tests if `a` is less than or equal to `b`.
   *
   * @return `a` <= `b`
   */
  def lteq(a:A, b:A):Boolean

  /**
   * Returns the larger of `a` and `b`.
   *
   * @return max(`a`, `b`)
   *
   * @see math.max
   */
  def max(a:A, b:A):A

  /**
   * Returns the smaller of `a` and `b`.
   *
   * @return min(`a`, `b`)
   *
   * @see math.min
   */
  def min(a:A, b:A):A

  /**
   * Returns `a` minus `b`.
   *
   * @return `a` - `b`
   */
  def minus(a:A, b:A):A

  /**
   * Returns `a` modulo `b`.
   *
   * @return `a` % `b`
   */
  def mod(a:A, b:A):A

  /**
   * Returns the additive inverse `a`.
   *
   * @return -`a`
   */
  def negate(a:A):A

  /**
   * Returns one.
   *
   * @return 1
   */
  def one():A

  /**
   * Returns `a` plus `b`.
   *
   * @return `a` + `b`
   */
  def plus(a:A, b:A):A

  /**
   * Returns `a` to the `b`th power.
   *
   * Note that with large numbers this method will overflow and
   * return Infinity, which becomes MaxValue for whatever type
   * is being used. This behavior is inherited from `math.pow`.
   * 
   * @returns pow(`a`, `b`)
   * 
   * @see math.pow
   */
  def pow(a:A, b:A):A

  /**
   * Returns an integer whose sign denotes the sign of `a`.
   * If `a` is negative it returns -1, if `a` is zero it
   * returns 0 and if `a` is positive it returns 1.
   *
   * @return -1, 0 or 1
   */
  def signum(a:A):Int = compare(a, zero)

  /**
   * Returns `a` times `b`.
   *
   * @return `a` * `b`
   */
  def times(a:A, b:A):A

  /**
   * Returns zero.
   *
   * @return 0
   */
  def zero():A

  /**
   * Convert a value `b` of type `B` to type `A`.
   *
   * This method can be used to coerce one generic numeric type to
   * another, to allow operations on them jointly.
   *
   * @example {{{
   *   def foo[A:Numeric,B:Numeric](a:A, b:B) = {
   *     val n = implicitly[Numeric[A]]
   *     n.add(a, n.fromType(b))
   *   }
   * }}}
   *
   * Note that `b` may lose precision when represented as an `A`
   * (e.g. if B is Long and A is Int).
   * 
   * @return the value of `b` encoded in type `A`
   */
  def fromType[@specialized B](b:B)(implicit c:ConvertableFrom[B]): A
}

trait NumericInt extends Numeric[Int] with ConvertableFromInt with ConvertableToInt {
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
  def pow(a:Int, b:Int): Int = scala.math.pow(a, b).toInt
  def times(a:Int, b:Int): Int = a * b
  def zero: Int = 0
  
  def fromType[@specialized B](b:B)(implicit c:ConvertableFrom[B]) = c.toInt(b)
}

trait NumericLong extends Numeric[Long] with ConvertableFromLong with ConvertableToLong {
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
  def pow(a:Long, b:Long): Long = scala.math.pow(a, b).toLong
  def times(a:Long, b:Long): Long = a * b
  def zero: Long = 0L
  
  def fromType[@specialized B](b:B)(implicit c:ConvertableFrom[B]) = c.toLong(b)
}

trait NumericFloat extends Numeric[Float] with ConvertableFromFloat with ConvertableToFloat {
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
  def pow(a:Float, b:Float): Float = scala.math.pow(a, b).toFloat
  def times(a:Float, b:Float): Float = a * b
  def zero: Float = 0.0F
  
  def fromType[@specialized B](b:B)(implicit c:ConvertableFrom[B]) = c.toFloat(b)
}

trait NumericDouble extends Numeric[Double] with ConvertableFromDouble with ConvertableToDouble {
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
  def pow(a:Double, b:Double): Double = scala.math.pow(a, b)
  def times(a:Double, b:Double): Double = a * b
  def zero: Double = 0.0
  
  def fromType[@specialized B](b:B)(implicit c:ConvertableFrom[B]) = c.toDouble(b)
}

object Numeric {
  implicit def infixNumericOps[@specialized A:Numeric](a:A):NumericOps[A] = new NumericOpsCls(a)

  //implicit def convertToDouble[@specialized A:Numeric](a:A) = a.toDouble
  implicit def convertToDouble[A](a:A)(implicit n:Numeric[A]):Double = n.toDouble(a)
  implicit def convertFromInt[A](x:Int)(implicit n:Numeric[A]):A = n.fromInt(x)

  def numeric[@specialized A:Numeric]() = implicitly[Numeric[A]]

  implicit object NumericInt extends NumericInt
  implicit object NumericLong extends NumericLong
  implicit object NumericFloat extends NumericFloat
  implicit object NumericDouble extends NumericDouble

}
