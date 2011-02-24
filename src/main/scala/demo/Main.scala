package demo

import scala.math.max
import scala.math.{Numeric, Integral}
import scala.util.Random

import Console.printf

import demo.Numeric3.{numeric,infixNumericOps}

// define some constant sizes and random arrays that we can use for our various
// performance tests
object Constant {
  val SM_SIZE  = 1000
  val MD_SIZE  = 100000
  val ML_SIZE  = 1000000
  val LG_SIZE  = 10000000

  val SM_DATA  = Array.ofDim[Int](SM_SIZE).map { i => Random.nextInt(1000) }
  val MD_DATA  = Array.ofDim[Int](MD_SIZE).map { i => Random.nextInt(1000) }
  val LG_DATA  = Array.ofDim[Int](LG_SIZE).map { i => Random.nextInt(1000) }
  val LG_DATA2  = Array.ofDim[Int](LG_SIZE).map { i => Random.nextInt(1000) }
}
import Constant._

// represents a particular performance test we want to run
trait TestCase[A] {
  def name: String

  // direct implementation using primitives
  def direct: Option[A]

  // implemented using the new Numeric3 trait
  def newGeneric: Option[A]

  // implemented using the built-in Numeric trait
  def oldGeneric: Option[A]

  // race our various implementations and display results
  def test {
    println(name)

    // warm up run
    this.direct
    this.newGeneric
    this.oldGeneric

    // warm up run #2
    this.direct
    this.newGeneric
    this.oldGeneric

    // timed run
    time("direct", direct)
    time("new-numeric", newGeneric)
    time("old-numeric", oldGeneric)
  }

  // used to time a particular implementation
  def time[A](label:String, x: => A) {
    val start = System.currentTimeMillis
    val r = x
    val duration = System.currentTimeMillis - start
    r match {
      case None => printf("  %-12s     n/a\n", label)
      case Some(a) => printf("  %-12s %4d ms\n", label, duration)
    }
  }
}

// converting Array[A] -> Array[Double], where A=Int
class ToDoubles extends TestCase[Array[Double]] {
  def name = "to-double"

  def directToDouble(a:Array[Int]) = {
    val len = a.length
    val b = Array.ofDim[Double](len)
    var i = 0
    while (i < len) {
      b(i) = a(i).toDouble
      i += 1
    }
    b
  }
  def direct = Some(directToDouble(LG_DATA))

  def newToDouble[@specialized A:Numeric3](a:Array[A]) = {
    val len = a.length
    val b = Array.ofDim[Double](len)
    var i = 0
    while (i < len) {
      b(i) = numeric.toDouble(a(i))
      i += 1
    }
    b
  }
  def newGeneric = Some(newToDouble(LG_DATA))

  def oldToDouble[A](a:Array[A])(implicit m:Numeric[A], c:Manifest[A]) = {
    val len = a.length
    val b = Array.ofDim[Double](len)
    var i = 0
    while (i < len) {
      b(i) = m.toDouble(a(i))
      i += 1
    }
    b
  }
  def oldGeneric = Some(oldToDouble(LG_DATA))
}

// converting Array[Int] -> Array[A], where A=Double
class FromInts extends TestCase[Array[Double]] {
  def name = "from-ints"

  def directFromInts(a:Array[Int]) = {
    val len = a.length
    val b = Array.ofDim[Double](len)
    var i = 0
    while (i < len) {
      b(i) = a(i).toDouble
      i += 1
    }
    b
  }
  def direct = Some(directFromInts(LG_DATA))

  def newFromInts[@specialized A:Numeric3:Manifest](a:Array[Int]): Array[A] = {
    val len = a.length
    val b = Array.ofDim[A](len)
    var i = 0
    while (i < len) {
      b(i) = numeric.fromInt(a(i))
      i += 1
    }
    b
  }
  def newGeneric = Some(newFromInts[Double](LG_DATA))

  def oldFromInts[A](a:Array[Int])(implicit m:Numeric[A], c:Manifest[A]): Array[A] = {
    val len = a.length
    val b = Array.ofDim[A](len)
    var i = 0
    while (i < len) {
      b(i) = m.fromInt(a(i))
      i += 1
    }
    b
  }
  def oldGeneric = Some(oldFromInts(LG_DATA))
}

// Adding many A values together, where A=Long
class Addition extends TestCase[Long] {
  def name = "addition"

  def directAdder(a:Long, b:Long) = a + b
  def direct = {
    var s = 0L
    var i = 0L
    while (i < LG_SIZE) {
      s = directAdder(s, i)
      i += 1L
    }
    Some(s)
  }

  def newAdder[@specialized A:Numeric3](a:A, b:A): A = numeric.plus(a, b)
  def newGeneric = {
    var s = 0L
    var i = 0L
    while (i < LG_SIZE) {
      s = newAdder(s, i)
      i += 1L
    }
    Some(s)
  }

  def oldAdder[A](a:A, b:A)(implicit m:Numeric[A]): A = m.plus(a, b)
  def oldGeneric = {
    var s = 0L
    var i = 0L
    while (i < LG_SIZE) {
      s = oldAdder(s, i)
      i += 1L
    }
    Some(s)
  }
}

// adding an Array[A] together, where A=Int
class Addition2 extends TestCase[Int] {
  def name = "addition2"

  def directAdder(a:Array[Int]) = {
    var total = 0
    val len = a.length
    var i = 0
    while (i < len) {
      total = total + a(i)
      i += 1
    }
    total
  }
  def direct = Some(directAdder(LG_DATA))

  def newAdder[@specialized A:Numeric3](a:Array[A]) = {
    var total = numeric.zero
    val len = a.length
    var i = 0
    while (i < len) {
      total = numeric.plus(total, a(i))
      i += 1
    }
    total
  }
  def newGeneric = Some(newAdder(LG_DATA))

  def oldAdder[A](a:Array[A])(implicit m:Numeric[A], c:Manifest[A]) = {
    var total = m.zero
    val len = a.length
    var i = 0
    while (i < len) {
      total = m.plus(total, a(i))
      i += 1
    }
    total
  }
  def oldGeneric = Some(oldAdder(LG_DATA))
}


// using numeric() instead of implicitly[Numeric3[A]]
// see Numeric3 numeric()  
class Addition3 extends Addition2 {
  override def name = "addition3, using numeric() instead of implicitly()"

  override def newGeneric = Some(newSyntaxAdder(LG_DATA))
  def newSyntaxAdder[@specialized A:Numeric3](a:Array[A]) = { 
    var total = numeric.zero
    val len = a.length
    var i = 0
    while (i < len) {
      total = numeric.plus(total, a(i))
      i += 1
    }
    total
  }
}


// testing demo.Numeric3.infixNumericOps, an implicit conversion 
// that allows infix operators to be used

class AdditionInfix extends Addition2 {
  override def name = "addition with infix"

  override def newGeneric = Some(infixAdder(LG_DATA))

  def infixAdder[@specialized A:Numeric3](a:Array[A]) = {
    var total = numeric.zero
    val len = a.length
    var i = 0
    while (i < len) {
      total = total + a(i)
      i += 1
    }
    total
  }
}

// Scaling an Array[A] by 5/3, where A=Int
class Rescale extends TestCase[Array[Int]] {
  def name = "rescale"

  def directScale(a:Int, num:Int, denom:Int) = (a * num) / denom
  def direct = {
    var i = 0
    val data2 = LG_DATA.clone
    while (i < LG_SIZE) {
      data2(i) = directScale(data2(i), 5, 3)
      i += 1
    }
    Some(data2)
  }

  def newScale[@specialized A:Numeric3](a:A, num:A, denom:A) = {
    numeric.div(numeric.times(a, num), denom)
  }
  def newGeneric = {
    var i = 0
    val data2 = LG_DATA.clone
    while (i < LG_SIZE) {
      data2(i) = newScale(data2(i), 5, 3)
      i += 1
    }
    Some(data2)
  }

  def oldScale[A](a:A, num:A, denom:A)(implicit m:Integral[A]): A = {
    m.quot(m.times(a, num), denom)
  }
  def oldGeneric = {
    var i = 0
    val data2 = LG_DATA.clone
    while (i < LG_SIZE) {
      data2(i) = oldScale(data2(i), 5, 3)
      i += 1
    }
    Some(data2)
  }
}

// Finding the maximum value in Array[A], where A=Int
class FindMax extends TestCase[Int] {
  def name = "find-max"

  def directMax(a:Int, b:Int) = scala.math.max(a, b)
  def direct = {
    var curr = LG_DATA(0)
    var i = 1
    while (i < LG_SIZE) {
      curr = directMax(curr, LG_DATA(i))
      i += 1
    }
    Some(curr)
  }

  def newMax[@specialized A:Numeric3](a:A, b:A): A = numeric.max(a, b)
  def newGeneric = {
    var curr = LG_DATA(0)
    var i = 1
    while (i < LG_SIZE) {
      curr = newMax(curr, LG_DATA(i))
      i += 1
    }
    Some(curr)
  }

  def oldMax[A](a:A, b:A)(implicit m:Numeric[A]): A = m.max(a, b)
  def oldGeneric = {
    var curr = LG_DATA(0)
    var i = 1
    while (i < LG_SIZE) {
      curr = oldMax(curr, LG_DATA(i))
      i += 1
    }
    Some(curr)
  }
}

// Finding the maximum value in Array[A], where A=Int
class FindMax2 extends TestCase[Int] {
  def name = "find-max2"

  def directFindMax(a:Array[Int]) = {
    var curr = a(0)
    val len = a.length
    var i = 1
    while (i < len) {
      curr = max(curr, a(i))
      i += 1
    }
    Some(curr)
  }
  def direct = directFindMax(LG_DATA)

  def newFindMax[@specialized A:Numeric3:Manifest](a:Array[A]) = {
    var curr = a(0)
    val len = a.length
    var i = 1
    while (i < len) {
      curr = numeric.max(curr, a(i))
      i += 1
    }
    Some(curr)
  }
  def newGeneric = newFindMax(LG_DATA)

  def oldFindMax[A](a:Array[A])(implicit m:Numeric[A], c:Manifest[A]) = {
    var curr = a(0)
    val len = a.length
    var i = 1
    while (i < len) {
      curr = m.max(curr, a(i))
      i += 1
    }
    Some(curr)
  }
  def oldGeneric = oldFindMax(LG_DATA)
}

// use scala.util.Sorting.quickSort to sort an Array[A] in place, where A=Int
class Quicksort extends TestCase[Array[Int]] {
  def name = "quick-sort"

  def directQuicksort(a:Array[Int]): Unit = scala.util.Sorting.quickSort(a)
  def direct = {
    val data2 = MD_DATA.clone
    directQuicksort(data2)
    Some(data2)
  }

  // unless we specialize Ordering (and scala.util.Sorting) we can't really
  // test our new Numeric on this.

  //def newQuicksort[@specialized A](a:Array[A])(implicit m:Numeric2[A]): Unit = scala.util.Sorting.quickSort(a)
  //def newGeneric = {
  //  val data2 = MD_DATA.clone
  //  newQuicksort(data2)
  //  data2
  //}
  def newGeneric = None

  def oldQuicksort[A](a:Array[A])(implicit m:Numeric[A]): Unit = scala.util.Sorting.quickSort(a)
  def oldGeneric = {
    val data2 = MD_DATA.clone
    oldQuicksort(data2)
    Some(data2)
  }
}

// use insertion sort to sort an Array[A] in place, where A=Int
class InsertionSort extends TestCase[Array[Int]] {
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
    Some(data2)
  }

  def newIsort[@specialized A:Numeric3:Manifest](a:Array[A]) {
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
    Some(data2)
  }

  def oldIsort[A](a:Array[A])(implicit m:Numeric[A], c:Manifest[A]) {
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
    Some(data2)
  }
}

class ArrayAllocator extends TestCase[Array[Array[Int]]] {
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
  def direct = Some(directAllocator(ML_SIZE, 5, 13))

  def newAllocator[@specialized A:Numeric3:Manifest](num:Int, dim:Int, const:A) = {
    val outer = Array.ofDim[Array[A]](num)
    var i = 0
    while (i < num) {
      outer(i) = Array.fill(dim)(const)
      i += 1
    }
    outer
  }
  def newGeneric = Some(newAllocator(ML_SIZE, 5, 13))

  def oldAllocator[A](num:Int, dim:Int, const:A)(implicit m:Numeric[A], c:Manifest[A]) = {
    val outer = Array.ofDim[Array[A]](num)
    var i = 0
    while (i < num) {
      outer(i) = Array.fill(dim)(const)
      i += 1
    }
    outer
  }
  def oldGeneric = Some(oldAllocator(ML_SIZE, 5, 13))
}

// use a merge sort to sort an Array[A] in place, where A=Int
class MergeSort extends TestCase[Array[Int]] {
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
    Some(data2)
  }

  def newMsort[@specialized A:Numeric3:Manifest](a:Array[A]) {
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
    Some(data2)
  }

  def oldMsort[A](a:Array[A])(implicit m:Numeric[A], c:Manifest[A]) {
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
    Some(data2)
  }
}

object Main {
  val tests = List(new Addition,
                   new Addition2,
                   new ToDoubles,
                   new FromInts,
                   new ArrayAllocator,
                   new Rescale,
                   new Quicksort,
                   new InsertionSort,
                   new MergeSort,
                   new FindMax,
                   new FindMax2,
                   new Addition3,
                   new AdditionInfix)
  
  def main(args:Array[String]): Unit = tests.foreach(_.test)
}
