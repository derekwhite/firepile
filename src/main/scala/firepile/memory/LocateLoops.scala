package firepile.memory

import soot.Body
import soot.jimple.toolkits.annotation.logic.{Loop, LoopFinder}
import scala.collection.JavaConversions._

class LocateLoops(body: Body) {
  val loopFinder = new LoopFinder()

  def loops: List[Loop] = {
    loopFinder.transform(body)

    loopFinder.loops().toList
  }
}