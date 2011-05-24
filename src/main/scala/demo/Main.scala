package main2

import scala.math.max
import scala.math.{Numeric => OldNumeric, Integral, Fractional, min, max}
import scala.util.Random
import scala.testing.Benchmark

import Console.printf

import demo.Numeric
import demo.Numeric.{numeric, infixNumericOps, convertFromInt, convertToDouble}

// define some constant sizes and random arrays that we can use for our various
// performance tests. if things run way too slow or way too fast you can try
// removing or adding zeros. :)
object Constant {
  val SM_SIZE  = 10000
  val MD_SIZE  = 100000
  val ML_SIZE  = 1000000
  val LG_SIZE  = 10000000

  val SM_DATA  = Array.ofDim[Int](SM_SIZE).map(i => Random.nextInt())
  val MD_DATA  = Array.ofDim[Int](MD_SIZE).map(i => Random.nextInt())
  val LG_DATA  = Array.ofDim[Int](LG_SIZE).map(i => Random.nextInt())
  val LG_DATA2  = Array.ofDim[Int](LG_SIZE).map(i => Random.nextInt())

  val LG_DBL_DATA  = Array.ofDim[Double](LG_SIZE).map(i => Random.nextDouble())

  val mediumSize = (1 * 1000 * 1000)
  val mediumIntArray = Array.ofDim[Int](mediumSize).map(i => Random.nextInt())
  val mediumLongArray = Array.ofDim[Long](mediumSize).map(i => Random.nextLong())
  val mediumFloatArray = Array.ofDim[Float](mediumSize).map(i => Random.nextFloat())
  val mediumDoubleArray = Array.ofDim[Double](mediumSize).map(i => Random.nextDouble())

  val largeSize = (10 * 1000 * 1000)
  val largeIntArray = Array.ofDim[Int](largeSize).map(i => Random.nextInt())
  val largeLongArray = Array.ofDim[Long](largeSize).map(i => Random.nextLong())
  val largeFloatArray = Array.ofDim[Float](largeSize).map(i => Random.nextFloat())
  val largeDoubleArray = Array.ofDim[Double](largeSize).map(i => Random.nextDouble())
}
import Constant._

case class TestResult(tavg:Double, tmax:Long, tmin:Long)

// represents a particular performance test we want to run
trait TestCase {
  def name: String

  // direct implementation using primitives
  def direct(): Option[Any]

  // implemented using the new Numeric trait
  def newGeneric(): Option[Any]

  // implemented using the built-in Numeric trait
  def oldGeneric(): Option[Any]

  object CaseDirect extends Benchmark { def run = direct }
  object CaseGeneric extends Benchmark { def run = newGeneric }
  object CaseOld extends Benchmark { def run = oldGeneric }

  val cases = List(CaseDirect, CaseGeneric, CaseOld)

  def runCase(c:Benchmark, warmupRuns:Int, liveRuns:Int) = {
    c.runBenchmark(warmupRuns)
    val results = c.runBenchmark(liveRuns)

    val total = results.foldLeft(0L)(_ + _)

    val tmax = results.foldLeft(0L)(max(_, _))
    val tmin = results.foldLeft(Long.MaxValue)(min(_, _))
    val tavg = total.toDouble / liveRuns

    TestResult(tavg, tmax, tmin)
  }

  def run(n:Int, m:Int):List[TestResult] = cases.map(runCase(_, n, m))

  def test() {
    //val results = run(1, 1)
    val results = run(2, 8)

    val times = results.map(_.tavg)
    val tstrs = times.map("%6.1fms".format(_))

    val List(t1, t2, t3) = times

    def mkp(a:Double, b:Double) = if (a == 0.0 || b == 0.0) 0.0 else a / b

    val percs = List(mkp(t2, t1), mkp(t3, t1), mkp(t3, t2))
    val pstrs = percs.map(p => if(p == 0.0) "    n/a" else "%5.2fx".format(p))

    val fields = ("%-20s".format(name) :: tstrs) ++ ("/" :: pstrs)
    println(fields.reduceLeft(_ + "  " + _))
  }
}


// ===============================================================
trait FromIntToX extends TestCase {
  def directToInt(a:Array[Int]) = {
    val b = Array.ofDim[Int](a.length)
    var i = 0
    while (i < a.length) {
      b(i) = a(i).toInt
      i += 1
    }
    b
  }
  def directToLong(a:Array[Int]) = {
    val b = Array.ofDim[Long](a.length)
    var i = 0
    while (i < a.length) {
      b(i) = a(i).toLong
      i += 1
    }
    b
  }

  def directToFloat(a:Array[Int]) = {
    val b = Array.ofDim[Float](a.length)
    var i = 0
    while (i < a.length) {
      b(i) = a(i).toFloat
      i += 1
    }
    b
  }

  def directToDouble(a:Array[Int]) = {
    val b = Array.ofDim[Double](a.length)
    var i = 0
    while (i < a.length) {
      b(i) = a(i).toDouble
      i += 1
    }
    b
  }

  def newFromInts[@specialized A:Numeric:Manifest](a:Array[Int]): Array[A] = {
    val b = Array.ofDim[A](a.length)
    var i = 0
    while (i < a.length) {
      b(i) = numeric.fromInt(a(i))
      i += 1
    }
    b
  }

  def oldFromInts[A:OldNumeric:Manifest](a:Array[Int]): Array[A] = {
    val m = implicitly[OldNumeric[A]]
    val b = Array.ofDim[A](a.length)
    var i = 0
    while (i < a.length) {
      b(i) = m.fromInt(a(i))
      i += 1
    }
    b
  }
}

class FromIntToInt extends FromIntToX {
  def name = "from-int-to-int"
  def direct() = Option(directToInt(largeIntArray))
  def newGeneric() = Option(newFromInts[Int](largeIntArray))
  def oldGeneric() = Option(oldFromInts[Int](largeIntArray))
}

class FromIntToLong extends FromIntToX {
  def name = "from-int-to-long"
  def direct() = Option(directToLong(largeIntArray))
  def newGeneric() = Option(newFromInts[Long](largeIntArray))
  def oldGeneric() = Option(oldFromInts[Long](largeIntArray))
}

class FromIntToFloat extends FromIntToX {
  def name = "from-int-to-float"
  def direct() = Option(directToFloat(largeIntArray))
  def newGeneric() = Option(newFromInts[Float](largeIntArray))
  def oldGeneric() = Option(oldFromInts[Float](largeIntArray))
}

class FromIntToDouble extends FromIntToX {
  def name = "from-int-to-double"
  def direct() = Option(directToDouble(largeIntArray))
  def newGeneric() = Option(newFromInts[Double](largeIntArray))
  def oldGeneric() = Option(oldFromInts[Double](largeIntArray))
}



// =================================================================
trait BaseAdder extends TestCase {
  def newAdder[@specialized A](a:A, b:A)(implicit m:Numeric[A]): A
  def oldAdder[A](a:A, b:A)(implicit m:OldNumeric[A]): A

  def directIntAdder(a:Int, b:Int):Int = a + b
  def directLongAdder(a:Long, b:Long):Long = a + b
  def directFloatAdder(a:Float, b:Float):Float = a + b
  def directDoubleAdder(a:Double, b:Double):Double = a + b
}

trait BaseAdderInt extends BaseAdder {
  def direct() = {
    var s:Int = 0; (0 until largeSize).foreach(i => s = directIntAdder(s, i)); Option(s)
  }
  def newGeneric() = {
    var s:Int = 0; (0 until largeSize).foreach(i => s = newAdder(s, i)); Option(s)
  }
  def oldGeneric() = {
    var s:Int = 0; (0 until largeSize).foreach(i => s = oldAdder(s, i)); Option(s)
  }
}

trait BaseAdderLong extends BaseAdder {
  def direct() = {
    var s:Long = 0L; (0 until largeSize).foreach(i => s = directLongAdder(s, i)); Option(s)
  }
  def newGeneric() = {
    var s:Long = 0L; (0 until largeSize).foreach(i => s = newAdder(s, i)); Option(s)
  }
  def oldGeneric() = {
    var s:Long = 0L; (0 until largeSize).foreach(i => s = oldAdder(s, i)); Option(s)
  }
}

trait BaseAdderFloat extends BaseAdder {
  def direct() = {
    var s:Float = 0.0F; (0 until largeSize).foreach(i => s = directFloatAdder(s, i)); Option(s)
  }
  def newGeneric() = {
    var s:Float = 0.0F; (0 until largeSize).foreach(i => s = newAdder(s, i)); Option(s)
  }
  def oldGeneric() = {
    var s:Float = 0.0F; (0 until largeSize).foreach(i => s = oldAdder(s, i)); Option(s)
  }
}

trait BaseAdderDouble extends BaseAdder {
  def direct() = {
    var s:Double = 0.0; (0 until largeSize).foreach(i => s = directDoubleAdder(s, i)); Option(s)
  }
  def newGeneric() = {
    var s:Double = 0.0; (0 until largeSize).foreach(i => s = newAdder(s, i)); Option(s)
  }
  def oldGeneric() = {
    var s:Double = 0.0; (0 until largeSize).foreach(i => s = oldAdder(s, i)); Option(s)
  }
}


// =========================================================
trait Adder extends BaseAdder {
  def newAdder[@specialized A](a:A, b:A)(implicit m:Numeric[A]): A = m.plus(a, b)
  def oldAdder[A](a:A, b:A)(implicit m:OldNumeric[A]): A = m.plus(a, b)
}

class AdderInt extends Adder with BaseAdderInt { def name = "adder-int" }
class AdderLong extends Adder with BaseAdderLong { def name = "adder-long" }
class AdderFloat extends Adder with BaseAdderFloat { def name = "adder-float" }
class AdderDouble extends Adder with BaseAdderDouble{ def name = "adder-double" }


// =====================================================
trait BaseArrayOps extends TestCase {
  def directIntArrayOp(a:Int, b:Int): Int
  def directIntArrayOps(a:Array[Int]) = {
    var total = 0
    var i = 0
    while (i < a.length) {
      total = directIntArrayOp(total, a(i))
      i += 1
    }
    total
  }

  def directLongArrayOp(a:Long, b:Long): Long
  def directLongArrayOps(a:Array[Long]) = {
    var total = 0L
    var i = 0
    while (i < a.length) {
      total = directLongArrayOp(total, a(i))
      i += 1
    }
    total
  }

  def directFloatArrayOp(a:Float, b:Float): Float
  def directFloatArrayOps(a:Array[Float]) = {
    var total = 0.0F
    var i = 0
    while (i < a.length) {
      total = directFloatArrayOp(total, a(i))
      i += 1
    }
    total
  }
    
  def directDoubleArrayOp(a:Double, b:Double): Double
  def directDoubleArrayOps(a:Array[Double]) = {
    var total = 0.0
    var i = 0
    while (i < a.length) {
      total = directDoubleArrayOp(total, a(i))
      i += 1
    }
    total
  }

  def newArrayOp[@specialized A:Numeric](a:A, b:A): A
  def newArrayOps[@specialized A:Numeric:Manifest](a:Array[A]) = {
    var total = numeric.zero
    var i = 0
    while (i < a.length) {
      total = newArrayOp(total, a(i))
      i += 1
    }
    total
  }

  def oldArrayOp[A](a:A, b:A)(implicit m:OldNumeric[A]): A
  def oldArrayOps[A:OldNumeric:Manifest](a:Array[A]) = {
    val m = implicitly[OldNumeric[A]]
    var total = m.zero
    var i = 0
    while (i < a.length) {
      total = oldArrayOp(total, a(i))
      i += 1
    }
    total
  }
}


trait BaseArrayMapOps extends TestCase {
  def directIntArrayOp(a:Int): Int
  def directIntArrayOps(a:Array[Int]) = {
    var i = 0
    while (i < a.length) {
      a(i) = directIntArrayOp(a(i))
      i += 1
    }
    a
  }

  def directLongArrayOp(a:Long): Long
  def directLongArrayOps(a:Array[Long]) = {
    var i = 0
    while (i < a.length) {
      a(i) = directLongArrayOp(a(i))
      i += 1
    }
    a
  }

  def directFloatArrayOp(a:Float): Float
  def directFloatArrayOps(a:Array[Float]) = {
    var i = 0
    while (i < a.length) {
      a(i) = directFloatArrayOp(a(i))
      i += 1
    }
    a
  }
    
  def directDoubleArrayOp(a:Double): Double
  def directDoubleArrayOps(a:Array[Double]) = {
    var i = 0
    while (i < a.length) {
      a(i) = directDoubleArrayOp(a(i))
      i += 1
    }
    a
  }

  def newArrayOp[@specialized A:Numeric](a:A): A
  def newArrayOps[@specialized A:Numeric:Manifest](a:Array[A]) = {
    var i = 0
    while (i < a.length) {
      a(i) = newArrayOp(a(i))
      i += 1
    }
    a
  }

  def oldArrayOp[A](a:A)(implicit m:OldNumeric[A]): A
  def oldArrayOps[A:OldNumeric:Manifest](a:Array[A]) = {
    val m = implicitly[OldNumeric[A]]
    var i = 0
    while (i < a.length) {
      a(i) = oldArrayOp(a(i))
      i += 1
    }
    a
  }
}


// ======================================================
trait ArrayAdder extends BaseArrayOps {
  def directIntArrayOp(a:Int, b:Int) = a + b
  def directLongArrayOp(a:Long, b:Long) = a + b
  def directFloatArrayOp(a:Float, b:Float) = a + b
  def directDoubleArrayOp(a:Double, b:Double) = a + b

  def newArrayOp[@specialized A:Numeric](a:A, b:A) = numeric.plus(a, b)
  def oldArrayOp[A](a:A, b:A)(implicit m:OldNumeric[A]) = m.plus(a, b)
}

class IntArrayAdder extends ArrayAdder {
  def name = "array-total-int"
  def direct() = Option(directIntArrayOps(largeIntArray))
  def newGeneric() = Option(newArrayOps(largeIntArray))
  def oldGeneric() = Option(oldArrayOps(largeIntArray))
}

class LongArrayAdder extends ArrayAdder {
  def name = "array-total-long"
  def direct() = Option(directLongArrayOps(largeLongArray))
  def newGeneric() = Option(newArrayOps(largeLongArray))
  def oldGeneric() = Option(oldArrayOps(largeLongArray))
}

class FloatArrayAdder extends ArrayAdder {
  def name = "array-total-float"
  def direct() = Option(directFloatArrayOps(largeFloatArray))
  def newGeneric() = Option(newArrayOps(largeFloatArray))
  def oldGeneric() = Option(oldArrayOps(largeFloatArray))
}

class DoubleArrayAdder extends ArrayAdder {
  def name = "array-total-double"
  def direct() = Option(directDoubleArrayOps(largeDoubleArray))
  def newGeneric() = Option(newArrayOps(largeDoubleArray))
  def oldGeneric() = Option(oldArrayOps(largeDoubleArray))
}




// ==========================================================================
trait ArrayRescale extends BaseArrayMapOps {
  def directIntArrayOp(b:Int) = (b * 5) / 3
  def directLongArrayOp(b:Long) = (b * 5L) / 3L
  def directFloatArrayOp(b:Float) = (b * 5.0F) / 3.0F
  def directDoubleArrayOp(b:Double) = (b * 5.0) / 3.0

  def newArrayOp[@specialized A](b:A)(implicit m:Numeric[A]) = m.div(m.times(b, m.fromDouble(5.0)), m.fromDouble(3.0))
  def oldArrayOp[A](b:A)(implicit m:OldNumeric[A]) = m.fromInt((m.toDouble(b) * 5.0 / 3.0).toInt)
}

class IntArrayRescale extends ArrayRescale {
  def name = "array-rescale-int"
  def direct() = Option(directIntArrayOps(largeIntArray))
  def newGeneric() = Option(newArrayOps(largeIntArray))
  def oldGeneric() = Option(oldArrayOps(largeIntArray))
}

class LongArrayRescale extends ArrayRescale {
  def name = "array-rescale-long"
  def direct() = Option(directLongArrayOps(largeLongArray))
  def newGeneric() = Option(newArrayOps(largeLongArray))
  def oldGeneric() = Option(oldArrayOps(largeLongArray))
}

class FloatArrayRescale extends ArrayRescale {
  def name = "array-rescale-float"
  def direct() = Option(directFloatArrayOps(largeFloatArray))
  def newGeneric() = Option(newArrayOps(largeFloatArray))
  def oldGeneric() = Option(oldArrayOps(largeFloatArray))
}

class DoubleArrayRescale extends ArrayRescale {
  def name = "array-rescale-double"
  def direct() = Option(directDoubleArrayOps(largeDoubleArray))
  def newGeneric() = Option(newArrayOps(largeDoubleArray))
  def oldGeneric() = Option(oldArrayOps(largeDoubleArray))
}



// ==========================================================================
trait InfixAdder extends BaseAdder {
  def newAdder[@specialized A:Numeric](a:A, b:A): A = a + b
  def oldAdder[A](a:A, b:A)(implicit m:OldNumeric[A]): A = {
    import m._
    a + b
  }
}

class InfixAdderInt extends InfixAdder with BaseAdderInt { def name = "infix-adder-int" }
class InfixAdderLong extends InfixAdder with BaseAdderLong { def name = "infix-adder-long" }
class InfixAdderFloat extends InfixAdder with BaseAdderFloat { def name = "infix-adder-float" }
class InfixAdderDouble extends InfixAdder with BaseAdderDouble{ def name = "infix-adder-double" }


// Finding the maximum value in Array[A], where A=Int
class FindMax extends TestCase {
  def name = "find-max"

  def directMax(a:Int, b:Int) = scala.math.max(a, b)
  def direct = {
    var curr = LG_DATA(0)
    var i = 1
    while (i < LG_SIZE) {
      curr = directMax(curr, LG_DATA(i))
      i += 1
    }
    Option(curr)
  }

  def newMax[@specialized A:Numeric](a:A, b:A): A = numeric.max(a, b)
  def newGeneric = {
    var curr = LG_DATA(0)
    var i = 1
    while (i < LG_SIZE) {
      curr = newMax(curr, LG_DATA(i))
      i += 1
    }
    Option(curr)
  }

  def oldMax[A:OldNumeric](a:A, b:A): A = {
    val m = implicitly[OldNumeric[A]]
    m.max(a, b)
  }
  def oldGeneric = {
    var curr = LG_DATA(0)
    var i = 1
    while (i < LG_SIZE) {
      curr = oldMax(curr, LG_DATA(i))
      i += 1
    }
    Option(curr)
  }
}

// Finding the maximum value in Array[A], where A=Int
class FindMax2 extends TestCase {
  def name = "find-max2"

  def directFindMax(a:Array[Int]) = {
    var curr = a(0)
    val len = a.length
    var i = 1
    while (i < len) {
      curr = max(curr, a(i))
      i += 1
    }
    Option(curr)
  }
  def direct = directFindMax(LG_DATA)

  def newFindMax[@specialized A:Numeric:Manifest](a:Array[A]) = {
    var curr = a(0)
    val len = a.length
    var i = 1
    while (i < len) {
      curr = numeric.max(curr, a(i))
      i += 1
    }
    Option(curr)
  }
  def newGeneric = newFindMax(LG_DATA)

  def oldFindMax[A:OldNumeric:Manifest](a:Array[A]) = {
    val m = implicitly[OldNumeric[A]]
    var curr = a(0)
    val len = a.length
    var i = 1
    while (i < len) {
      curr = m.max(curr, a(i))
      i += 1
    }
    Option(curr)
  }
  def oldGeneric = oldFindMax(LG_DATA)
}


// ================================================================
trait BaseSort extends TestCase {
  def directIntSorter(a:Array[Int]): Array[Int]
  def directLongSorter(a:Array[Long]): Array[Long]
  def directFloatSorter(a:Array[Float]): Array[Float]
  def directDoubleSorter(a:Array[Double]): Array[Double]

  def newGenericSorter[@specialized A:Numeric:Manifest](a:Array[A]): Array[A]
  def oldGenericSorter[A:OldNumeric:Manifest](a:Array[A]): Array[A]

  def directInt(a:Array[Int]) = Option(directIntSorter(a))
  def directLong(a:Array[Long]) = Option(directLongSorter(a))
  def directFloat(a:Array[Float]) = Option(directFloatSorter(a))
  def directDouble(a:Array[Double]) = Option(directDoubleSorter(a))

  def newGenericSort[@specialized A:Numeric:Manifest](a:Array[A]) = Option(newGenericSorter(a))
  def oldGenericSort[A:OldNumeric:Manifest](a:Array[A]) = Option(oldGenericSorter(a))
}


// =======================================================================
trait Quicksort extends BaseSort {
  def directIntSorter(a:Array[Int]) = { val d = a.clone; scala.util.Sorting.quickSort(d); d }
  def directLongSorter(a:Array[Long]) = { val d = a.clone; scala.util.Sorting.quickSort(d); d }
  def directFloatSorter(a:Array[Float]) = { val d = a.clone; scala.util.Sorting.quickSort(d); d }
  def directDoubleSorter(a:Array[Double]) = { val d = a.clone; scala.util.Sorting.quickSort(d); d }

  def newGeneric() = None
  def newGenericSorter[@specialized A:Numeric:Manifest](a:Array[A]) = null
  def oldGenericSorter[A:OldNumeric:Manifest](a:Array[A]) = { val d = a.clone; scala.util.Sorting.quickSort(d); d }
}

class QuicksortInt extends Quicksort {
  def name = "quicksort-int"
  def direct() = directInt(mediumIntArray)
  def oldGeneric() = oldGenericSort(mediumIntArray)
}

class QuicksortLong extends Quicksort {
  def name = "quicksort-long"
  def direct() = directLong(mediumLongArray)
  def oldGeneric() = oldGenericSort(mediumLongArray)
}

class QuicksortFloat extends Quicksort {
  def name = "quicksort-float"
  def direct() = directFloat(mediumFloatArray)
  def oldGeneric() = oldGenericSort(mediumFloatArray)
}

class QuicksortDouble extends Quicksort {
  def name = "quicksort-double"
  def direct() = directDouble(mediumDoubleArray)
  def oldGeneric() = oldGenericSort(mediumDoubleArray)
}

// use insertion sort to sort an Array[A] in place, where A=Int
class InsertionSort extends TestCase {
  def name = "insertion-sort"

  def directIsort(a:Array[Int]) {
    var i = 0
    val last = a.length - 1
    while (i < last) {
      var j = i + 1
      var k = i
      while (j <= last) {
        if (a(j) < a(i)) k = j
        j += 1
      }
      val temp = a(i)
      a(i) = a(k)
      a(k) = temp
      i += 1
    }
  }
  def direct = {
    val data2 = SM_DATA.clone
    directIsort(data2)
    Option(data2)
  }

  def newIsort[@specialized A:Numeric:Manifest](a:Array[A]) {
    var i = 0
    val last = a.length - 1
    while (i < last) {
      var j = i + 1
      var k = i
      while (j <= last) {
        if (numeric.lt(a(j), a(i))) k = j
        j += 1
      }
      val temp = a(i)
      a(i) = a(k)
      a(k) = temp
      i += 1
    }
  }
  def newGeneric = {
    val data2 = SM_DATA.clone
    newIsort(data2)
    Option(data2)
  }

  def oldIsort[A:OldNumeric:Manifest](a:Array[A]) {
    val m = implicitly[OldNumeric[A]]
    var i = 0
    val last = a.length - 1
    while (i < last) {
      var j = i + 1
      var k = i
      while (j <= last) {
        if (m.lt(a(j), a(i))) k = j
        j += 1
      }
      val temp = a(i)
      a(i) = a(k)
      a(k) = temp
      i += 1
    }
  }
  def oldGeneric = {
    val data2 = SM_DATA.clone
    oldIsort(data2)
    Option(data2)
  }
}

class ArrayAllocator extends TestCase {
  def name = "array-allocator"

  def directAllocator(num:Int, dim:Int, const:Int) = {
    val outer = Array.ofDim[Array[Int]](num)
    var i = 0
    while (i < num) {
      outer(i) = Array.fill(dim)(const)
      i += 1
    }
    outer
  }
  def direct = Option(directAllocator(ML_SIZE, 5, 13))

  def newAllocator[@specialized A:Numeric:Manifest](num:Int, dim:Int, const:A) = {
  //def newAllocator[A:Numeric:Manifest](num:Int, dim:Int, const:A) = {
    val outer = Array.ofDim[Array[A]](num)
    var i = 0
    while (i < num) {
      outer(i) = Array.fill(dim)(const)
      i += 1
    }
    outer
  }
  def newGeneric = Option(newAllocator(ML_SIZE, 5, 13))

  def oldAllocator[A:OldNumeric:Manifest](num:Int, dim:Int, const:A) = {
    val outer = Array.ofDim[Array[A]](num)
    var i = 0
    while (i < num) {
      outer(i) = Array.fill(dim)(const)
      i += 1
    }
    outer
  }
  def oldGeneric = Option(oldAllocator(ML_SIZE, 5, 13))
}

// use a merge sort to sort an Array[A] in place, where A=Int
class MergeSort extends TestCase {
  def name = "merge-sort"

  def directMsort(a:Array[Int]) {
    val len = a.length
    if (len > 1) {
      val llen = len / 2
      val rlen = len - llen

      val left  = Array.ofDim[Int](llen)
      Array.copy(a, 0, left, 0, llen)
      directMsort(left)
  
      val right = Array.ofDim[Int](rlen)
      Array.copy(a, llen, right, 0, rlen)
      directMsort(right)

      var i = 0
      var j = 0
      var k = 0
      while (i < llen || j < rlen) {
        if (j == rlen) {
          a(k) = left(i)
          i += 1
        } else if (i == llen) {
          a(k) = right(j)
          j += 1
        } else if (left(i) < right(j)) {
          a(k) = left(i)
          i += 1
        } else {
          a(k) = right(j)
          j += 1
        }
        k += 1
      }
    }
  }
  def direct = {
    val data2 = MD_DATA.clone
    directMsort(data2)
    Option(data2)
  }

  def newMsort[@specialized A:Numeric:Manifest](a:Array[A]) {
    val len = a.length
    if (len > 1) {
      val llen = len / 2
      val rlen = len - llen

      val left  = Array.ofDim[A](llen)
      Array.copy(a, 0, left, 0, llen)
      newMsort(left)
  
      val right = Array.ofDim[A](rlen)
      Array.copy(a, llen, right, 0, rlen)
      newMsort(right)

      var i = 0
      var j = 0
      var k = 0
      while (i < llen || j < rlen) {
        if (j == rlen) {
          a(k) = left(i)
          i += 1
        } else if (i == llen) {
          a(k) = right(j)
          j += 1
        } else if (numeric.lt(left(i), right(j))) {
          a(k) = left(i)
          i += 1
        } else {
          a(k) = right(j)
          j += 1
        }
        k += 1
      }
    }
  }
  def newGeneric  = {
    val data2 = MD_DATA.clone
    newMsort(data2)
    Option(data2)
  }

  def oldMsort[A:OldNumeric:Manifest](a:Array[A]) {
    val m = implicitly[OldNumeric[A]]
    val len = a.length
    if (len > 1) {
      val llen = len / 2
      val rlen = len - llen

      val left  = Array.ofDim[A](llen)
      Array.copy(a, 0, left, 0, llen)
      oldMsort(left)
  
      val right = Array.ofDim[A](rlen)
      Array.copy(a, llen, right, 0, rlen)
      oldMsort(right)

      var i = 0
      var j = 0
      var k = 0
      while (i < llen || j < rlen) {
        if (j == rlen) {
          a(k) = left(i)
          i += 1
        } else if (i == llen) {
          a(k) = right(j)
          j += 1
        } else if (m.lt(left(i), right(j))) {
          a(k) = left(i)
          i += 1
        } else {
          a(k) = right(j)
          j += 1
        }
        k += 1
      }
    }
  }
  def oldGeneric = {
    val data2 = MD_DATA.clone
    oldMsort(data2)
    Option(data2)
  }
}

object Main {
  val tests = List(new FromIntToInt,
                   new FromIntToLong,
                   new FromIntToFloat,
                   new FromIntToDouble,

                   new AdderInt,
                   new AdderLong,
                   new AdderFloat,
                   new AdderDouble,

                   new IntArrayAdder,
                   new LongArrayAdder,
                   new FloatArrayAdder,
                   new DoubleArrayAdder,

                   new IntArrayRescale,
                   new LongArrayRescale,
                   new FloatArrayRescale,
                   new DoubleArrayRescale,

                   new QuicksortInt,
                   new QuicksortLong,
                   new QuicksortFloat,
                   new QuicksortDouble,

                   new ArrayAllocator,
                   new InsertionSort,
                   new MergeSort,

                   new InfixAdderInt,
                   new InfixAdderLong,
                   new InfixAdderFloat,
                   new InfixAdderDouble)

  //implicit def convertToDouble[A:Numeric](a:A) = a.toDouble
  // import demo.Numeric.convertToDouble
  // def mysqrt[A:Numeric](a:A) = math.sqrt(a)
  // println(mysqrt(49.0))
  
  def main(args:Array[String]): Unit = {
    printf("%-20s  %8s  %8s  %8s  /  %6s  %6s  %6s\n", "test", "direct", "new", "old", "n:d", "o:d", "o:n")
    tests.foreach(_.test)
  }
}
