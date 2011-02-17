package demo

import scala.math.max
import scala.util.Random

import Console.printf


trait TestCase[A] {
  def name: String
  def direct: A
  def generic: A
  def test {
    this.direct
    this.generic
    println(name)
    time("direct", direct)
    time("generic", generic)
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

  def direct = {
    var s = 0L
    var i = 0L
    while (i < 100000000L) {
      s = s + i
      i += 1L
    }
    s
  }

  def genericf[@specialized A](a:A, b:A)(implicit m:Mathable[A]): A = m.plus(a, b)
  def generic = {
    var s = 0L
    var i = 0L
    while (i < 100000000L) {
      s = genericf(s, i)
      i += 1L
    }
    s
  }
}

class FindMax extends TestCase[Int] {
  def name = "findmax"

  val size = 10000000
  val data = Array.ofDim[Int](size).map { i => Random.nextInt(100000) }

  def dmax(a:Int, b:Int) = scala.math.max(a, b)
  def direct = {
    var curr = data(0)
    var i = 1
    val len = data.length
    while (i < len) {
      curr = dmax(curr, data(i))
      i += 1
    }
    curr
  }

  //def gmax[@specialized A](a:A, b:A)(implicit m:Mathable[A]): A = m.max(a, b)
  def gmax[@specialized A](a:A, b:A)(implicit m:Mathable[A]): A = m.plus(a, b)
  def generic = {
    var curr = data(0)
    var i = 1
    val len = data.length
    while (i < len) {
      curr = gmax(curr, data(i))
      i += 1
    }
    curr
  }
}

//class Quicksort extends TestCase[Array[Int]] {
//  def name = "quicksort"
//
//  val size = 1000000
//  val data = Array.ofDim[Int](size).map { i => Random.nextInt(100000) }
//
//  def sort(a:Array[Int]): Unit = scala.util.Sorting.quickSort(a)
//  def direct = {
//    val data2 = data.clone
//    data2
//  }
//
//  def gsort[@specialized A](a:Array[A])(implicit m:Mathable[A]): Unit = scala.util.Sorting.quickSort(a)
//  def generic = {
//    val data2 = data.clone
//    gsort(data2)
//    data2
//  }
//}

object Main {
  //val tests = List(new Addition, new FindMax, new Quicksort)
  val tests = List(new Addition, new FindMax)
  
  def main(args:Array[String]): Unit = {
    tests.foreach {
      test => test.test
    }
  }
}
