package firepile.tests

object TestMemUse5 {

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

    testBasicNew(BBArray.fromArray(randInput).directCopy, 20)(firepile.gpu)

  }

  def testBasicNew(idata: BBArray[Float], arraySize: Int)(implicit dev: Device) {
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

              // val x = new Point(r, r)
              // val z = new Array[Float](r)
              var i = 0
              while (i < 5) {
                var j = 3
                while (j < 10) {
                  val z = new Array[Float](arraySize)
                  j += 1
                }
                i += 1
              }
            }
          }
        }
      }

      (odata, idata, n)
    }

  }

}
