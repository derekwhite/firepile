package firepile.tests

object TestMemUse2 {

  import firepile._
  import firepile.util.BufferBackedArray._
  import scala.collection.JavaConversions._
  import firepile.Marshaling._
  import scala.util.Random

  case class Point(x: Float, y: Float) { }

  var NUM_ITEMS = 16384


  def main(args: Array[String]) = {
    run
  }

  def run = {
    val random = new Random(0)
    val randInput = Array.fill(NUM_ITEMS)(random.nextFloat)

    // rayTrace(BBArray.fromArray(randInput).directCopy, 17)(firepile.gpu)

  }

/*  def rayTrace(scene: Scene, rgb: Array[Int], screenHeight: Int, screenWidth: Int)(implicit dev: Device) {
    val space = dev.defaultPaddedPartition(screenHeight)
    // val odata = BBArray.ofDim[Float](space.blocks).directCopy
    val n = idata.length
    Kernel.output("odata")

    space.spawn {
      for (group <- space.groups) {
          for (item <- group.items) {


              val z = new Array[Float](10)

           }
       }
     }

  }*/
}