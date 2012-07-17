package firepile.memory

abstract class Scope {
  val ORDER: Int
  override def toString: String
  def <(s: Scope) = this.ORDER < s.ORDER
  def >(s: Scope) = this.ORDER > s.ORDER
}
case object LoopScope extends Scope {
  val ORDER = 4
  override def toString = "LoopScope"
}
case object MethodReuse extends Scope {
  val ORDER = 3
  override def toString = "MethodReuse"
}
case object MethodScope extends Scope {
  val ORDER = 2
  override def toString = "MethodScope"
}
case object ReturnedReuse extends Scope {
  val ORDER = 1
  override def toString = "ReturnedReuse"
}
case object Returned extends Scope {
  val ORDER = 0
  override def toString = "Returned"
}
