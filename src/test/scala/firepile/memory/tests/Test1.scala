package firepile.memory.tests

import scala.collection.mutable.HashMap

class Test1 {
  def g(a: Int) = {
    var r: Array[Int] = null
    var a = 3
    while (a < 10) {
      val x = a * a
      r = new Array(x)
      a += 1
    }

    r(0)
  }

  case class Point(var x: Int, var y: Int)

  def writeFromLoop(points: Array[Point] /* global */, n: Int) = {
    var i = 0
    while (i < n) {
      val X = new Point(0, 0)		// private or local
      X.x = 1
      X.y = 100

      points(i) = X   // write from private or local to global

      i += 1
    }
  }

  def g1(x: Int) = {
    var r: Array[Int] = null
    var a = 3
    while (a < 10) {
      r = new Array(x)
      val t = new Array[Int](100)
      a += 1
    }

    a
  }

  def g1b(x: Int) = {
    var r: Array[Int] = null
    var a = 3
    while (a < 10) {
      r = new Array(x)
      val t = new Array[Int](100)
      a += 1
    }

    r
  }

  def g2(a: Int) = {
    var r: Array[Int] = null
    var a = 0
    while (a < 10) {
      r = new Array(200)
      a += 1
    }

    r(0)
  }

  def g3(d: Int) = {
    var r: Array[Int] = null
    var a = 0
    while (a < 10) {
      r = new Array(d)
      a += 1
    }

    r(0)
  }

  def g4(d: Int) = {
    var r: Array[Int] = null
    val z = new Array[Point](10)
    var a = 0
    while (a < 10) {
      r = new Array(d)
      val p = new Point(0, 0)
      z(a) = p
      a += 1
    }

    r(0)
  }

  def g5(d: Int) = {
    var r: Array[Int] = null
    val z = new Array[Point](10)
    var a = 0
    while (a < 10) {
      r = new Array(d)
      z(a) = new Point(0, 0)
      a += 1
    }

    r(0)
  }

  def h(a: Int) = {
    var r: Array[Int] = null
    if (a > 10)
      r = new Array(a)
    //else
    //  r = new Array[Int](a * 10)

    r(0)
  }

  def i(j: Int) = {
    var r: Array[Int] = null
    var a = 3
    while (a < 10) {
      val x = a * a
      r = new Array(j + x)
      a += 1
    }

    r(0)
  }

  def j(a: Int) = {
    val r = new Array[Int](10)
    val z = new HashMap[String, String]()


    r(0)
  }


  def k(a: Int) = {
    var r: Array[Int] = null
    var a = 3
    while (a < 10) {
      var startloop = a
      val z = new HashMap[String, String]()
      val x = new HashMap[String, String]()
      a += 1
    }
    a
  }
}

