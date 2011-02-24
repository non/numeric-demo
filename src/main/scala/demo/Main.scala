package demo

import scala.math.max
import scala.math.{Numeric, Integral}
import scala.util.Random

import Console.printf

object Constant {
  val SM_SIZE  = 100000
  val SM_DATA  = Array.ofDim[Int](SM_SIZE).map { i => Random.nextInt(1000) }

  val LG_SIZE  = 1000000
  val LG_DATA  = Array.ofDim[Int](LG_SIZE).map { i => Random.nextInt(1000) }
}

import Constant._

trait TestCase[A] {
  def name: String

  def direct: A
  def generic: A
  def numeric: A

  def test {
    println(name)

    // warm up run
    this.direct
    this.generic
    this.numeric

    // timed run
    time("direct", direct)
    time("generic", generic)
    time("numeric", numeric)
  }

  def time[A](label:String, x: => A) {
    val start = System.currentTimeMillis
    val r = x
    val duration = System.currentTimeMillis - start
    printf("  %-8s %d ms\n", label, duration)
  }
}

class Addition extends TestCase[Long] {
  def name = "addition"

  def adder(a:Long, b:Long) = a + b
  def direct = {
    var s = 0L
    var i = 0L
    while (i < LG_SIZE) {
      s = adder(s, i)
      i += 1L
    }
    s
  }

  def gadder[@specialized A](a:A, b:A)(implicit m:MathableB[A]): A = m.plus(a, b)
  def generic = {
    var s = 0L
    var i = 0L
    while (i < LG_SIZE) {
      s = gadder(s, i)
      i += 1L
    }
    s
  }

  def nadder[A](a:A, b:A)(implicit m:Numeric[A]): A = m.plus(a, b)
  def numeric = {
    var s = 0L
    var i = 0L
    while (i < LG_SIZE) {
      s = nadder(s, i)
      i += 1L
    }
    s
  }
}

class Addition2 extends TestCase[Int] {
  def name = "addition2"

  def adder(a:Array[Int]) = {
    var total = 0
    val len = a.length
    var i = 0
    while (i < len) {
      total = total + a(i)
      i += 1
    }
    total
  }
  def direct = adder(LG_DATA)

  def gadder[@specialized A](a:Array[A])(implicit m:MathableB[A], c:Manifest[A]) = {
    var total = m.zero
    val len = a.length
    var i = 0
    while (i < len) {
      total = m.plus(total, a(i))
      i += 1
    }
    total
  }
  def generic = gadder(LG_DATA)

  def nadder[A](a:Array[A])(implicit m:Numeric[A], c:Manifest[A]) = {
    var total = m.zero
    val len = a.length
    var i = 0
    while (i < len) {
      total = m.plus(total, a(i))
      i += 1
    }
    total
  }
  def numeric = nadder(LG_DATA)
}

class Rescale extends TestCase[Array[Int]] {
  def name = "rescale"

  def scale(a:Int, num:Int, denom:Int) = (a * num) / denom
  def direct = {
    var i = 0
    val data2 = LG_DATA.clone
    while (i < LG_SIZE) {
      data2(i) = scale(data2(i), 5, 3)
      i += 1
    }
    data2
  }

  def gscale[@specialized A](a:A, num:A, denom:A)(implicit m:MathableB[A]): A = {
    m.div(m.times(a, num), denom)
  }
  def generic = {
    var i = 0
    val data2 = LG_DATA.clone
    while (i < LG_SIZE) {
      data2(i) = gscale(data2(i), 5, 3)
      i += 1
    }
    data2
  }

  def nscale[A](a:A, num:A, denom:A)(implicit m:Integral[A]): A = {
    m.quot(m.times(a, num), denom)
  }
  def numeric = {
    var i = 0
    val data2 = LG_DATA.clone
    while (i < LG_SIZE) {
      data2(i) = nscale(data2(i), 5, 3)
      i += 1
    }
    data2
  }
}

class FindMax extends TestCase[Int] {
  def name = "findmax"

  def dmax(a:Int, b:Int) = scala.math.max(a, b)
  def direct = {
    var curr = LG_DATA(0)
    var i = 1
    while (i < LG_SIZE) {
      curr = dmax(curr, LG_DATA(i))
      i += 1
    }
    curr
  }

  def gmax[@specialized A](a:A, b:A)(implicit m:MathableB[A]): A = m.max(a, b)
  def generic = {
    var curr = LG_DATA(0)
    var i = 1
    while (i < LG_SIZE) {
      curr = gmax(curr, LG_DATA(i))
      i += 1
    }
    curr
  }

  def nmax[A](a:A, b:A)(implicit m:Numeric[A]): A = m.max(a, b)
  def numeric = {
    var curr = LG_DATA(0)
    var i = 1
    while (i < LG_SIZE) {
      curr = nmax(curr, LG_DATA(i))
      i += 1
    }
    curr
  }
}

//class Quicksort extends TestCase[Array[Int]] {
//  def name = "quicksort"
//
//  def sort(a:Array[Int]): Unit = scala.util.Sorting.quickSort(a)
//  def direct = {
//    val data2 = SM_DATA.clone
//    sort(data2)
//    data2
//  }
//
//  def gsort[@specialized A](a:Array[A])(implicit m:MathableB[A]): Unit = scala.util.Sorting.quickSort(a)
//  def generic = {
//    val data2 = SM_DATA.clone
//    gsort(data2)
//    data2
//  }
//
//  def nsort[A](a:Array[A])(implicit m:Numeric[A]): Unit = scala.util.Sorting.quickSort(a)
//  def numeric = {
//    val data2 = SM_DATA.clone
//    gsort(data2)
//    data2
//  }
//}

class MergeSort extends TestCase[Array[Int]] {
  def name = "mergesort"

  def msort(a:Array[Int]) {
    val len = a.length
    if (len > 1) {
      val llen = len / 2
      val rlen = len - llen

      val left  = Array.ofDim[Int](llen)
      Array.copy(a, 0, left, 0, llen)
      msort(left)
  
      val right = Array.ofDim[Int](rlen)
      Array.copy(a, llen, right, 0, rlen)
      msort(right)

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
    val data2 = SM_DATA.clone
    msort(data2)
    data2
  }

  def gmsort[@specialized A](a:Array[A])(implicit m:MathableB[A], c:Manifest[A]) {
    val len = a.length
    if (len > 1) {
      val llen = len / 2
      val rlen = len - llen

      val left  = Array.ofDim[A](llen)
      Array.copy(a, 0, left, 0, llen)
      gmsort(left)
  
      val right = Array.ofDim[A](rlen)
      Array.copy(a, llen, right, 0, rlen)
      gmsort(right)

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
  def generic  = {
    val data2 = SM_DATA.clone
    gmsort(data2)
    data2
  }

  def nmsort[A](a:Array[A])(implicit m:Numeric[A], c:Manifest[A]) {
    val len = a.length
    if (len > 1) {
      val llen = len / 2
      val rlen = len - llen

      val left  = Array.ofDim[A](llen)
      Array.copy(a, 0, left, 0, llen)
      nmsort(left)
  
      val right = Array.ofDim[A](rlen)
      Array.copy(a, llen, right, 0, rlen)
      nmsort(right)

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
  def numeric = {
    val data2 = SM_DATA.clone
    gmsort(data2)
    data2
  }
}

object Main {
  val tests = List(new Addition,
                   new Addition2,
                   new Rescale,
                   //new Quicksort,
                   new MergeSort,
                   new FindMax)
  
  def main(args:Array[String]): Unit = tests.foreach(_.test)
}
