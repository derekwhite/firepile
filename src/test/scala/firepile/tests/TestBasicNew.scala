package firepile.tests

object TestBasicNew {

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

    testBasicNew(BBArray.fromArray(randInput).directCopy, 17)(firepile.gpu)

  }

  def testBasicNew(idata: BBArray[Float], r: Int)(implicit dev: Device) {
    val space = dev.defaultPaddedPartition(idata.length)
    val odata = BBArray.ofDim[Float](space.blocks).directCopy
    val n = idata.length
    Kernel.output("odata")

    space.spawn {
      space.groups.foreach {
        g => {
          val sdata = Array.ofDim[Float](g.items.size)

          g.items.foreach {
            item => {

              var i = 0
              while (i < 12) {
                val x = new Point(r, r)
                i += 1
              }
              // val z = new Array[Float](r)
              // var i = 0
              // while (i < 32) {
              //  val z = new Array[Float](arraySize * 3)
              //  i += 1
              // }
            }
          }
        }
      }

      (odata, idata, n)
    }

  }

}
