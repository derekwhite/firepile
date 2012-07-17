package firepile.memory

import soot.toolkits.graph.UnitGraph
import soot.toolkits.graph.DirectedGraph
import soot.toolkits.graph.ExceptionalUnitGraph
import scala.collection.JavaConversions._
import soot.grimp.Grimp
import soot.options.Options
import soot.{SootMethod, Body, Scene}
import abc.analoop.LLTreePrinter
import abc.analoop.LoopComplexityAnalyzer

import soot.{Unit => SootUnit}

object RunMemUse {
  def main(args: Array[String]) {
    if (args.length < 2) {
      println("usage: TypeFlow className methodSig")
      sys.exit(1)
    }

    var className = args(0)
    val methodSig = args(1)
    var b: Body = null

    Scene.v.setSootClassPath(Scene.v.defaultClassPath
      + ":/Users/dwhite/firepile-experiment/fp-memuse/out/production/fp-memuse"
      + ":/Users/dwhite/opt/scala-2.9.1.final/lib/scala-library.jar")

    Options.v.set_keep_line_number(true)
    Options.v.setPhaseOption("jb", "use-original-names:true")
    Options.v.setPhaseOption("cg", "verbose:true")
    Options.v.set_allow_phantom_refs(true)

    val c = Scene.v.loadClassAndSupport(className)
    Scene.v.loadNecessaryClasses()
    c.setApplicationClass()

    for ((m: SootMethod) <- c.methodIterator) {
      println("Checking: " + m.getName + soot.AbstractJasminClass.jasminDescriptorOf(m.makeRef) + " for " + methodSig)
      if ((m.getName + soot.AbstractJasminClass.jasminDescriptorOf(m.makeRef)).equals(methodSig)) {
        if (! m.isConcrete)
          throw new RuntimeException("Can only run MemUse on concrete methods")
        b = m.retrieveActiveBody
        className = m.getDeclaringClass.getName
      }
    }

    if (b == null)
      println("Method " + methodSig + " not found in class " + className + ". Checking for raw name.")

    for ((m: SootMethod) <- c.methodIterator) {
      if (m.getName.equals(methodSig)) {
        if (! m.isConcrete)
          throw new RuntimeException("Can only run TypeFlow on concrete methods")
        b = m.retrieveActiveBody
        className = m.getDeclaringClass.getName
      }
    }

    if (b == null)
      throw new RuntimeException("Method " + methodSig + " not found in class " + className)

    println("declaring class is " + className)
    val gb = Grimp.v().newBody(b, "gb")
    println("GRIMP\n" + gb)
    val g = new ExceptionalUnitGraph(gb)


    val mufa = new MemUse(g)

    val mrfa = new MemRegions(g, mufa.getParams)

    val loops = mufa.buildLoops

    //    println("# of loops: " + loops.length)

    /*    for (l <- loops) {
      println("Loop: " + LLTreePrinter(l))

      val (z1, c1) = LoopComplexityAnalyzer(l) match {
        case Some(res) => (res._1, res._2)
        case None => throw new RuntimeException("no z-relation and complexity computed for: " + LLTreePrinter(l))
      }

      println("")
      // println("Z-relation: " + LLTreePrinter(z1))
      println("Complexity: " + LLTreePrinter(c1))
      // println("Complexity with vars: " + ComplexityExpression(c1, Map("d" -> 300)))

    }*/

    val finalFlow = mrfa.getFlowAfter(g.last)

    println("MemRegions: " + finalFlow)

    val allocationUnits = mufa.getAllocationUnits

    for ((k,v) <- allocationUnits) {
      v.scope = finalFlow.get(v.localName) match {
        case Some(scope) => scope
        case None => throw new RuntimeException("Cannot get scope for " + v.localName)
      }
    }

    //tfa.getAllocationUnits.foreach[Unit]((u: SootUnit, i: Int) => { println("Unit: " + u + " allocation point: " + i) })  // (((u, i)) => println("Unit: " + u + " allocation point: " + i))
    allocationUnits.keys.foreach((u: SootUnit) => {
      val ap = allocationUnits.get(u)
      println("Unit: " + u + " allocation point: " + ap)

      val loop = ap.get.loop
      if (loop != null) {
        val (z1, c1) = LoopComplexityAnalyzer(loop) match {
          case Some(res) => (res._1, res._2)
          case None => throw new RuntimeException("no z-relation and complexity computed for: " + LLTreePrinter(loop))
        }
        println("")
        // println("Z-relation: " + LLTreePrinter(z1))
        println("Complexity: " + LLTreePrinter(c1))
        println("Complexity with vars: " + ComplexityExpression(c1, Map("j" -> 300, "a" -> 20, "n" -> 10, "d" -> 100)))

      }


    })

  }

}