package firepile.memory

import abc.analoop.LoopLanguageTrees._

object ComplexityExpression {
  def apply(exp: AExpr, vars: Map[String,Int]): Int = {
    solve(exp, vars)
  }
  
  def solve(exp: AExpr, vars: Map[String, Int]): Int = {
    exp match {
      case e: Sum => solve(e, vars)
      case e: Sub => solve(e, vars)
      case e: Mult => solve(e, vars)
      case e: Div => solve(e, vars)
      case e: IntDiv => solve(e, vars)
      case e: IntVar => solve(e, vars)
      case e: IntVal => solve(e)
      case x => throw new RuntimeException("Unexpected complexity expression: " + x)
    }
  }
  
  /*
    case class Sum(a1: AExpr, a2: AExpr) extends AExpr
  case class Sub(a1: AExpr, a2: AExpr) extends AExpr
  case class Mult(a1: AExpr, a2: AExpr) extends AExpr
  case class Div(a1: AExpr, a2: AExpr) extends AExpr

	case class IntDiv(a1: AExpr, a2: AExpr) extends AExpr
  
   */

  def solve(exp: Sum, vars: Map[String, Int]): Int = solve(exp.a1, vars) + solve(exp.a2, vars)
  def solve(exp: Sub, vars: Map[String, Int]): Int = solve(exp.a1, vars) - solve(exp.a2, vars)
  def solve(exp: Mult, vars: Map[String, Int]): Int = solve(exp.a1, vars) * solve(exp.a2, vars)
  def solve(exp: Div, vars: Map[String, Int]): Int = solve(exp.a1, vars) / solve(exp.a2, vars)
  def solve(exp: IntDiv, vars: Map[String, Int]): Int = solve(exp.a1, vars) / solve(exp.a2, vars)
  

  def solve(intVar: IntVar, vars: Map[String, Int]): Int = vars.get(intVar.id) match {
    case Some(i) => i
    case None => throw new RuntimeException("No value found in map for IntVar: " + intVar.id)
  }
  def solve(int: IntVal): Int = int.n

}
