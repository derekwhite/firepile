package firepile.memory

import soot.tagkit.Tag

class LoopTag extends Tag {
  def getName = "LoopTag"
  def getValue = new Array[Byte](1)
}