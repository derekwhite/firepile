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

import abc.analoop.LoopLanguageTrees._
import abc.analoop.{LLTreePrinter, LoopComplexityAnalyzer}

case class AllocationPoint(mallocNum: Int, localName: String, var scope: Scope, loop: ForLoop, typ: SootType) {
  def getElementType = typ match {
    case at: SootArrayType => at.getElementType
    case _ => typ
  }
}

object LoopUtils {
  def innermostLoop(loop: Statement): ForLoop = {
    loop match {
      case ForLoop(_, _, _, Nil) => loop match {
        case l: ForLoop => l
        case _ => throw new RuntimeException("Expect type ForLoop but some other Statement encountered")
      }
      case ForLoop(_, _, _, l :: lst) => innermostLoop(l)
      case _ => throw new RuntimeException("Unexpected loop body found")
    }
  }
}

class MemUse(graph: UnitGraph) extends ForwardFlowAnalysis[SootUnit,Map[String,Boolean]](graph) {
  val trackParams = new ListBuffer[String]()
  val emptySet: Map[String,Boolean] = new HashMap[String,Boolean]()
//  val localAssigns = new HashMap[String,IntConstant]()
  val localAssigns = new HashMap[String,AExpr]()
  val localIncrements = new HashMap[String,AExpr]()
  val loopList = new ListBuffer[ForLoop]()
  val condList = new HashMap[BExpr,List[(AExpr,SootType)]]()
  val unitsList = new HashMap[BExpr,List[SootUnit]]()
  val arrayAllocs = new ListBuffer[(AExpr,SootType)]()
  var genVarCounter = -1
  var allocationPoint = -1
  val allocationPoints = new HashMap[SootUnit,AllocationPoint]()
  val unitsInLoopScope = new ListBuffer[SootUnit]()
  var loopScope = false

  doAnalysis()

  protected def newInitialFlow(): Map[String,Boolean] = {
      emptySet.clone()
  }

  protected def entryInitialFlow(): Map[String,Boolean] = {
    val paramMap = new HashMap[String,Boolean]()
    val methodName = graph.getBody.getMethod.getName
    val methodSig = methodName + soot.AbstractJasminClass.jasminDescriptorOf(graph.getBody.getMethod.makeRef)


    println("entryInitialFlow for: " + methodSig)


    println("entryInitialFlow generated: " + paramMap)
    paramMap
  }

  protected def flowThrough(inValue: Map[String,Boolean], unit: SootUnit, outValue: Map[String,Boolean]) {
    // Compute gen from getDefs/kill set

    outValue ++= inValue
    println("CURRENT UNIT: " + unit)


    // kill
    for (box <- unit.getDefBoxes) {
      // println("Use box: " + box.getValue.getType.toString)

      box.getValue match {
        case x: Local => {
          if (inValue.contains(x.getName)) {
            // outValue(x.getName) = inValue(x.getName)
//            println("Local: " + x.getName + " -> " + outValue(x.getName))
          }
          else {
            print("DEF: " + x.getName)
            outValue += x.getName -> false
//            println("Local: " + x.getName)
          }
          /* outValue -= x.getName */

        }

        case x: IntConstant => println("IntConstant: " + x.toString)
        case x: GLeExpr => println("GLeExpr: " + x.toString)
        case x => println("wtf " + x + ": " + x.getClass.getName)
      }
    }

/*    for (box <- unit.getUseBoxes) {
      // println("Use box: " + box.getValue.getType.toString)
      print("USE: ")
      box.getValue match {
        case x: Local => /*outValue -= x.getName;*/ println("Local: " + x.getName)
        case x: IntConstant => println("IntConstant: " + x.toString)
        case x: GLeExpr => println("GLeExpr: " + x.toString)
        case x => println("wtf " + x + ": " + x.getClass.getName)
      }
    }*/

    unitsInLoopScope += unit

    // gen
    unit match {
      case GAssignStmt(left,right) => {
        matchExp(left, outValue)
        val allocPoint = matchExp(right, outValue)
        println("GAssignStmt: " + left + " = " + right)
        (left,right) match {
          case (l: Local, r: IntConstant) => localAssigns += l.getName -> r
          case (l: Local, binop: BinopExpr) => localIncrements += l.getName -> binop
          case _ => { }
        }
        
        if (allocPoint > -1) {
          val lclVal = left match {
            case l: Local => l
            case GArrayRef(base: Local, _) => base
          }
          val lclTyp = lclVal match {
            case at: ArrayType => at.getElementType
            case t => t.getType
          }
          allocationPoints += unit -> new AllocationPoint(allocPoint, lclVal.getName, null, null, lclTyp)
          println("ADDING ALLOCATION POINT FOR TYPE: " + lclVal.getName)
        }
      }
      case GReturn(ret) => {
        for (u <- unit.getUseBoxes)
          u.getValue match {
            case x: Local => outValue += x.getName -> false
            case x => println("something other than Local returned: " + x)
          }
      }
      case GIdentity(l, r) => {
        outValue(l.asInstanceOf[Local].getName) = false; println("GIdentity: " + l.asInstanceOf[Local].getName)

        r match {
          case GParameterRef(_, _) => trackParams += l.asInstanceOf[Local].getName
          case _ => { }
        }
      }
      case GIf(_,_) => {
        loopScope = true
        unitsInLoopScope.clear()
      }
        
      case x =>  {} //println("wtf " + x + ": " + x.getClass.getName)
    }

    if (loopScope)
      unit.addTag(new LoopTag)

    println("flowThrough: " + outValue)
  }

  override protected def merge(u: SootUnit, in1: Map[String,Boolean], in2: Map[String,Boolean], out: Map[String,Boolean]) {
    println("MERGE on " + u)
    println("in1: " + in1)
    println("in2: " + in2)

    for (u <- in1.keys.toList ::: in2.keys.toList)
      if (in2.contains(u) && !in1.contains(u))
        out += u -> true
      else
        out += u -> in1(u)

    val bexpr: BExpr = u match {
      case GIf(cond: BinopExpr, _) => cond
      case x: ReturnStmt => null
    }

    if (bexpr != null) {
      println("Adding to the COND list: " + u + " hash code " + u.hashCode + " and bexpr hash code " + bexpr.hashCode)
      condList += bexpr -> arrayAllocs.toList
      unitsList += bexpr -> unitsInLoopScope.toList
    }

    loopScope = false
    arrayAllocs.clear()

  }

  implicit def binopExpr2BExpr(bexpr: BinopExpr): BExpr = bexpr match {
    // reversing bytecode condition to match program logic as written in code
    case GLe(op1, op2) => {
      GreaterThan(op1, op2)
    }
    case GLt(op1, op2) => {
      GreaterEqual(op1, op2)
    }
    case GGe(op1, op2) => {
      LessThan(op1, op2)
    }
    case GGt(op1, op2) => {
      LessEqual(op1, op2)
    }
    case GEq(op1, op2) => {
      Equals(op1, op2)
    }
    case _ => {
      println("Complicated condition");
      null
    }
  }

  implicit def binopExpr2AExpr(binop: BinopExpr): AExpr = binop match {
    case GAdd(op1, op2) => Sum(op1, op2)
    case GMul(op1, op2) => Mult(op1, op2)
    case GSub(op1, op2) => Sub(op1, op2)
    case GDiv(op1, op2) => Div(op1, op2)
  }

  implicit def value2AExpr(v: Value): AExpr = v match {
    case l: Local => /* localIncrements.get(l.getName) match {
      case Some(x) => x
      case None => IntVar(l.getName)
    } // */ IntVar(l.getName)
    case ic: IntConstant => IntVal(ic.value)
    case ifr: InstanceFieldRef => IntVar(ifr.getField.getName.substring(0, ifr.getField.getName.indexOf("$")))
    case boe: BinopExpr => boe
  }


  protected def merge(in1: Map[String,Boolean], in2: Map[String,Boolean], out: Map[String,Boolean]) {
  /*
    println("MERGE")
    println("in1: " + in1)
    println("in2: " + in2)
  */

  }


  private def matchExp(v: Value, out: Map[String,Boolean]): Int = {
    
    val allocPoint = v match {
      case GNew(newTyp) => {
        println("Creating new class: " + newTyp.getClass.getName)
        out += newTyp.getClass.getName -> false
        allocationPoint = allocationPoint + 1
        allocationPoint
      }
      case GNewInvoke(base, method, args) => {
        println("Creating new class: " + base.getClassName)
        out += base.getClassName -> false
        allocationPoint = allocationPoint + 1
        allocationPoint
      }
      case GNewArray(newTyp, size) => {
        println("Creating new array of " + newTyp.toString + " with size " + size)
        arrayAllocs += Tuple2(size, newTyp)
        allocationPoint = allocationPoint + 1
        allocationPoint
      }

      case x =>  -1 // {} // println("wtf " + x + ": " + x.getClass.getName)
    }
    
    allocPoint
  }


  protected def copy(source: Map[String,Boolean], dest: Map[String,Boolean]) {
    dest ++= source
  }

  def buildLoops: List[ForLoop] = {
    condList.keys.map(c => {
      c match {
        case x@LessEqual(a1, a2) => {
          println("LESS EQUAL")
          val (assign, mod) = lookupAssignMods(a1) match {
            case (Some(a), Some(m)) => (a, m)
            case (None, Some(m)) => throw new RuntimeException("No initialization of " + a1) // (null, m)
            case (Some(a), None) => throw new RuntimeException("No increment of " + a1) // (a, m)
            case _ => throw new RuntimeException("No init or increment of " + a1) // (null, null)
          }
          // ForLoop(Assignement(a1, 0), x, Assignement(a1, a2), Nil)
          condList.get(c) match {
            case Some(Nil) => {
              assocLoopAllocPoint(ForLoop(assign, x, mod, Nil), unitsList.get(c))
              List(ForLoop(assign, x, mod, Nil))
            }
            case Some(alloc) => {
              alloc.map(a => a._1).map(al => {
                val generatedVar = generateVarName
                val al_expanded = al match {
                  case a: IntVal => a
                  case a: IntVar => localIncrements.get(a.id) match {
                    case Some(ex) => ex
                    case None => a
                  }
                  case Sum(op1, op2: IntVar) => Sum(op1, localIncrements(op2.id))
                  case _ => al
                }
                val loop = ForLoop(assign, x, mod, List(ForLoop(Assignement(generatedVar,0), LessThan(generatedVar,al_expanded), Assignement(generatedVar,generatedVar + 1),Nil)))
                assocLoopAllocPoint(loop, unitsList.get(c))
                loop
              }).toList
            }
            case None => {
              assocLoopAllocPoint(ForLoop(assign, x, mod, Nil), unitsList.get(c))
              List(ForLoop(assign, x, mod, Nil))
            }
          }
        }
        case x@LessThan(a1, a2) => {
          println("LESS THAN")
          val (assign, mod) = lookupAssignMods(a1) match {
            case (Some(a), Some(m)) => (a, m)
            case (None, Some(m)) => throw new RuntimeException("No initialization of " + a1) // (null, m)
            case (Some(a), None) => throw new RuntimeException("No increment of " + a1) // (a, m)
            case _ => throw new RuntimeException("No init or increment of " + a1) // (null, null)
          }

          println("Got assign/mod for LESS THAN: " + assign + "/" + mod)
          // ForLoop(Assignement(a1, 0), x, Assignement(a1, a2), Nil)
          condList.get(c) match {
            case Some(Nil) => {
              assocLoopAllocPoint(ForLoop(assign, x, mod, Nil), unitsList.get(c))
              List(ForLoop(assign, x, mod, Nil))
            }
            case Some(alloc) => {
              println("Found " + alloc + " for LESS THAN")
              alloc.map(a => a._1).map(al => {
                val generatedVar = generateVarName
                val al_expanded = al match {
                  case a: IntVal => a
                  case a: IntVar => localIncrements.get(a.id) match {
                    case Some(ex) => ex
                    case None => a
                  }
                  case Sum(op1, op2: IntVar) => Sum(op1, localIncrements(op2.id))
                  case _ => al
                }
                val loop = ForLoop(assign, x, mod, List(ForLoop(Assignement(generatedVar,0), LessThan(generatedVar,al_expanded), Assignement(generatedVar,generatedVar + 1),Nil)))
                assocLoopAllocPoint(loop, unitsList.get(c))
                loop
              }).toList
            }
            case None => {
              assocLoopAllocPoint(ForLoop(assign, x, mod, Nil), unitsList.get(c))
              List(ForLoop(assign, x, mod, Nil))
            }
          }
        }
        case x@GreaterEqual(a1, a2) => {
          println("GREATER EQUAL")
          val (assign, mod) = lookupAssignMods(a1) match {
            case (Some(a), Some(m)) => (a, m)
            case (None, Some(m)) => throw new RuntimeException("No initialization of " + a1) // (null, m)
            case (Some(a), None) => throw new RuntimeException("No increment of " + a1) // (a, m)
            case _ => throw new RuntimeException("No init or increment of " + a1) // (null, null)
          }
          // ForLoop(Assignement(a1, 0), x, Assignement(a1, a2), Nil)
          condList.get(c) match {
            case Some(Nil) => {
              assocLoopAllocPoint(ForLoop(assign, x, mod, Nil), unitsList.get(c))
              List(ForLoop(assign, x, mod, Nil))
            }
            case Some(alloc) => {
              alloc.map(a => a._1).map(al => {
                val generatedVar = generateVarName
                val al_expanded = al match {
                  case a: IntVal => a
                  case a: IntVar => localIncrements.get(a.id) match {
                    case Some(ex) => ex
                    case None => a
                  }
                  case Sum(op1, op2: IntVar) => Sum(op1, localIncrements(op2.id))
                  case _ => al
                }
                val loop = ForLoop(assign, x, mod, List(ForLoop(Assignement(generatedVar,0), LessThan(generatedVar,al_expanded), Assignement(generatedVar,generatedVar + 1),Nil)))
                assocLoopAllocPoint(loop, unitsList.get(c))
                loop
              }).toList
            }
            case None => {
              assocLoopAllocPoint(ForLoop(assign, x, mod, Nil), unitsList.get(c))
              List(ForLoop(assign, x, mod, Nil))
            }
          }
        }
        case x@GreaterThan(a1, a2) => {
          val (assign, mod) = lookupAssignMods(a1) match {
            case (Some(a), Some(m)) => (a, m)
            case (None, Some(m)) => throw new RuntimeException("No initialization of " + a1) // (null, m)
            case (Some(a), None) => throw new RuntimeException("No increment of " + a1) // (a, m)
            case _ => throw new RuntimeException("No init or increment of " + a1) // (null, null)
          }
          // ForLoop(Assignement(a1, 0), x, Assignement(a1, a2), Nil)
          condList.get(c) match {
            case Some(Nil) => {
              assocLoopAllocPoint(ForLoop(assign, x, mod, Nil), unitsList.get(c))
              List(ForLoop(assign, x, mod, Nil))
            }
            case Some(alloc) => {
              alloc.map(a => a._1).map(al => {
                val generatedVar = generateVarName
                val al_expanded = al match {
                  case a: IntVal => a
                  case a: IntVar => localIncrements.get(a.id) match {
                    case Some(ex) => ex
                    case None => a
                  }
                  case Sum(op1, op2: IntVar) => Sum(op1, localIncrements(op2.id))
                  case _ => al
                }
                val loop = ForLoop(assign, x, mod, List(ForLoop(Assignement(generatedVar,0), LessThan(generatedVar,al_expanded), Assignement(generatedVar,generatedVar + 1),Nil)))
                assocLoopAllocPoint(loop, unitsList.get(c))
                loop
              }).toList
            }
            case None => {
              assocLoopAllocPoint(ForLoop(assign, x, mod, Nil), unitsList.get(c))
              List(ForLoop(assign, x, mod, Nil))
            }
          }
        }
        case x@Equals(a1, a2) => {
          val (assign, mod) = lookupAssignMods(a1) match {
            case (Some(a), Some(m)) => (a, m)
            case (None, Some(m)) => throw new RuntimeException("No initialization of " + a1) // (null, m)
            case (Some(a), None) => throw new RuntimeException("No increment of " + a1) // (a, m)
            case _ => throw new RuntimeException("No init or increment of " + a1) // (null, null)
          }
          // ForLoop(Assignement(a1, 0), x, Assignement(a1, a2), Nil)
          condList.get(c) match {
            case Some(Nil) => {
              assocLoopAllocPoint(ForLoop(assign, x, mod, Nil), unitsList.get(c))
              List(ForLoop(assign, x, mod, Nil))
            }
            case Some(alloc) => {
              // val generatedVar = IntVar("X_1")
              // ForLoop(assign, x, mod, List(ForLoop(Assignement(generatedVar,0), LessThan(generatedVar,alloc.head._1), Assignement(generatedVar,generatedVar + 1),Nil)))
              alloc.map(a => a._1).map(al => {
                val generatedVar = generateVarName
                val al_expanded = al match {
                  case a: IntVal => a
                  case a: IntVar => localIncrements.get(a.id) match {
                    case Some(ex) => ex
                    case None => a
                  }
                  case Sum(op1, op2: IntVar) => Sum(op1, localIncrements(op2.id))
                  case _ => al
                }
                val loop = ForLoop(assign, x, mod, List(ForLoop(Assignement(generatedVar,0), LessThan(generatedVar,al_expanded), Assignement(generatedVar,generatedVar + 1),Nil)))
                assocLoopAllocPoint(loop, unitsList.get(c))
                loop
              }).toList
            }
            case None => {
              assocLoopAllocPoint(ForLoop(assign, x, mod, Nil), unitsList.get(c))
              List(ForLoop(assign, x, mod, Nil))
            }
          }
        }
      }
    }).flatten.toList
  }

  def assocLoopAllocPoint(loop: ForLoop, units: Option[List[SootUnit]]) {
    units match {
      case Some(unitsL) => {
        for (u <- unitsL)
          allocationPoints.get(u) match {
            case Some(ap) => allocationPoints(u) = AllocationPoint(ap.mallocNum, ap.localName, ap.scope, loop, ap.typ)
            case None => {
            }
          }
      }
      case None => { }
    }  
  }
  
  def fakeLoopForArray = {
    for ((u: SootUnit, ap: AllocationPoint) <- allocationPoints) {
      ap match {
        case AllocationPoint(mallocNum, name, scope, null, typ: SootArrayType) =>  {
          println("Unit and allocation point for non-looped array: " + u + " -> " + ap)
          u match {
            case GAssignStmt(left, right: NewArrayExpr) => {
              val expr = right.getSize
              val arrayGenIntVar = IntVar("array_generated")
              println("New array created with size expression: " + expr)
              allocationPoints(u) = AllocationPoint(mallocNum, name, scope, ForLoop(Assignement(arrayGenIntVar, 0), LessThan(arrayGenIntVar, expr), Assignement(arrayGenIntVar, arrayGenIntVar + 1), Nil), typ)
            }
            case _ => throw new RuntimeException("fakeLoopForArray: AllocationPoint with null loop but Unit does not contain NewArrayExpr!")
          }
          // allocationPoints(u) = AllocationPoint(mallocNum, name, scope, null, typ)
        }
        case _ => { }
      }
    }
  }
  
  def generateVarName: IntVar = {
    genVarCounter += 1
    IntVar("X_" + genVarCounter)
  }

  def lookupAssignMods(a: AExpr): (Option[Assignement], Option[Assignement]) = a match {
    case intVar: IntVar => {
      val init = localAssigns.get(intVar.id) match {
        case Some(x: AExpr) => Some(Assignement(intVar, x))
        case None => None
      }

      val mod = localIncrements.get(intVar.id) match {
        case Some(x: AExpr) => Some(Assignement(intVar, x))
        case None => None
      }

      (init, mod)
    }
    case Sum(op1: IntVar, op2) => {
      val init = localAssigns.get(op1.id) match {
        case Some(x: AExpr) => Some(Assignement(op1, x))
        case None => None
      }

      val mod = localIncrements.get(op1.id) match {
        case Some(x: AExpr) => Some(Assignement(op1, x))
        case None => None
      }

      (init, mod)
    }
    case Sub(op1: IntVar, op2) => {
      val init = localAssigns.get(op1.id) match {
        case Some(x: AExpr) => Some(Assignement(op1, x))
        case None => None
      }

      val mod = localIncrements.get(op1.id) match {
        case Some(x: AExpr) => Some(Assignement(op1, x))
        case None => None
      }

      (init, mod)
    }
    case Mult(op1: IntVar, op2) => {
      val init = localAssigns.get(op1.id) match {
        case Some(x: AExpr) => Some(Assignement(op1, x))
        case None => None
      }

      val mod = localIncrements.get(op1.id) match {
        case Some(x: AExpr) => Some(Assignement(op1, x))
        case None => None
      }

      (init, mod)
    }
    case Div(op1: IntVar, op2) => {
      val init = localAssigns.get(op1.id) match {
        case Some(x: AExpr) => Some(Assignement(op1, x))
        case None => None
      }

      val mod = localIncrements.get(op1.id) match {
        case Some(x: AExpr) => Some(Assignement(op1, x))
        case None => None
      }

      (init, mod)
    }
    case _ => (None, None)
  }

  def getAllocationUnits = {
    fakeLoopForArray
    allocationPoints
  }

  def getParams = trackParams.distinct.toList

}