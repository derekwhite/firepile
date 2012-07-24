package firepile.memory

import soot.grimp.internal.GLeExpr
import soot.toolkits.scalar._
import soot.{Unit => SootUnit, Type => SootType, ArrayType => SootArrayType}
import soot.toolkits.graph.UnitGraph
import soot.toolkits.graph.DirectedGraph
import soot.toolkits.graph.ExceptionalUnitGraph
import soot.util._
import soot.jimple._
import internal.AbstractBinopExpr
import soot.Body
import soot.Scene
import soot.Local
import soot.Value
import soot.ValueBox
import soot.SootClass
import soot.ArrayType
import soot.SootMethodRef
import soot.SootFieldRef
import soot.SootMethod
import soot.grimp.Grimp
import soot.grimp.GrimpBody
import soot.options.Options
import soot.tagkit.Tag
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._
import firepile.compiler.GrimpUnapply._

class MemRegions(graph: UnitGraph, params: List[String]) extends ForwardFlowAnalysis[SootUnit,Map[String,Scope]](graph) {
  val emptySet: Map[String,Scope] = new HashMap[String,Scope]()

  doAnalysis()

  protected def newInitialFlow(): Map[String,Scope] = {
    emptySet.clone()
  }

  protected def entryInitialFlow(): Map[String,Scope] = {
    val paramMap = new HashMap[String,Scope]()
    val methodName = graph.getBody.getMethod.getName
    val methodSig = methodName + soot.AbstractJasminClass.jasminDescriptorOf(graph.getBody.getMethod.makeRef)

    for (i <- params)
      paramMap(i) = Returned

    println("entryInitialFlow for: " + methodSig)


    println("entryInitialFlow generated: " + paramMap)
    paramMap
  }

  protected def flowThrough(inValue: Map[String,Scope], unit: SootUnit, outValue: Map[String,Scope]) {
    outValue ++= inValue

    var currentScope: Scope = MethodReuse

    if (insideLoop(unit))
      currentScope = LoopScope

    for (d <- unit.getDefBoxes) {

      localName(d.getValue) match {
        case name: String => {
          println("def of: " + d.getValue + " in unit " + unit)

          unit match {
            case GIdentity(left, right) => {

            }
            case GAssignStmt(left, right) => right match {
              case GNew(typ) => outValue(name) = currentScope
              case GNewArray(typ, _) => outValue.get(name) match {
                  case Some(scope) => if (scope > currentScope) outValue(name) = currentScope
                  case None => outValue(name) = currentScope
              }
              case GNewInvoke(_, _, _) => outValue.get(name) match {
                case Some(scope) => if (scope > currentScope) outValue(name) = currentScope
                case None => outValue(name) = currentScope
              }
              case _ => {
                println("Assignment to " + left + " from " + right)
                val leastRight = leastRegion(right, inValue)
                println("leastRight = " + leastRight)
                outValue.get(name) match {
                  case Some(scope) => {
                    println("scopeLeft = " + scope)
                    if (scope < leastRight)
                      if (right.getUseBoxes.length > 0)
                        for (use <- right.getUseBoxes)
                          outValue(localName(use.asInstanceOf[Value])) = scope
                      else
                        outValue(localName(right)) = scope
                    else
                      outValue(name) = leastRight
                  }
                  case None => outValue(name) = currentScope
                }

              }
            }
            case x => throw new RuntimeException("def in " + x + " instead of AssignStmt")
          }
        }
        case _ => throw new RuntimeException("Something other than local def")
      }
    }

    unit match {
//      case GAssignStmt(left, right) => {
//        left match {
//          case l: Local => {
//            val leastRight = leastRegion(right, inValue)
//            val leastLeft = leastRegion(left, inValue)
//            if (outValue(l.getName) < leastRight)
//              for (ub <- right.getUseBoxes)
//                ub.asInstanceOf[ValueBox].getValue match {
//                  case lcl: Local => outValue(lcl.getName) = leastLeft
//                  case _ => {}
//                }
//            else
//              outValue(l.getName) = leastRight
//          }
//          case GArrayRef(base, _) => {
//            val leastRight = leastRegion(right, inValue)
//            val leastLeft = leastRegion(left, inValue)
//            if (outValue(base.asInstanceOf[Local].getName) < leastRight)
//              for (ub <- right.getUseBoxes)
//                ub.asInstanceOf[ValueBox].getValue match {
//                  case lcl: Local => outValue(lcl.getName) = leastLeft
//                  case _ => {}
//                }
//            else
//              outValue(base.asInstanceOf[Local].getName) = leastRight
//          }
//        }
//      }
      case GReturn(ret) =>
        for (u <- unit.getUseBoxes)
          u.getValue match {
            case x: Local => outValue(x.getName) = Returned
            case x => println("something other than Local returned: " + x)
          }

      case _ => { }

    }

  }

  override protected def merge(u: SootUnit, in1: Map[String,Scope], in2: Map[String,Scope], out: Map[String,Scope]) {
    println("MERGE on " + u)
    println("in1: " + in1)
    println("in2: " + in2)
    for ((n: String, s: Scope) <- in1)
      if (in2.contains(s))
        if (s < in2(n))
          out(n) = in1(n)
        else
          out(n) = in2(n)
      else
        out(n) = in1(n)

    for ((n: String, s: Scope) <- in2)
      if (!in1.contains(s))
        out(n) = in2(n)
    println("out: " + out)

  }

  protected def merge(in1: Map[String,Scope], in2: Map[String,Scope], out: Map[String,Scope]) {
    /*
      println("MERGE")
      println("in1: " + in1)
      println("in2: " + in2)
    */

  }

  protected def copy(source: Map[String,Scope], dest: Map[String,Scope]) {
    dest ++= source
  }

  def insideLoop(u: SootUnit): Boolean = {
    println("Is unit " + u + " inside a loop? " + u.hasTag("LoopTag"))
    u.hasTag("LoopTag")
  }

  def localName(v: Value): String = v match {
    case l: Local => l.getName
    case GArrayRef(base: Local, _) => base.getName
    case _ => { println("localName has " + v) ; null }
  }

  def leastRegion(v: Value, in: Map[String,Scope]) = {
    var least: Scope = LoopScope
    println("-----------\nleastRegion of " + v + " containing " + v.getUseBoxes.length + " uses")

    if (v.getUseBoxes.length > 0) {
      for (use <- v.getUseBoxes) {
        println("for " + localName(use.asInstanceOf[ValueBox].getValue))
        localName(use.asInstanceOf[ValueBox].getValue) match {
          case name: String => in.get(name) match {
            case Some(scope) => if (scope < least) least = scope
            case None => throw new RuntimeException("leastRegion: var " + name + " not found in in set (" + in + ")")
          }
          case _ => {}
        }
      }
    }
    else {
      println("for " + localName(v))
      localName(v) match {
        case name: String => in.get(name) match {
          case Some(scope) => if (scope < least) least = scope
          case None => throw new RuntimeException("leastRegion: var not found in in set")
        }
        case _ => {}
      }
    }
    println("----------- " + least)
    least
  }
}
