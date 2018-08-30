package org.xingyi.brainfuck

trait BrainFuckOps extends (String => BrainFuckFn) {
  def plus: BrainFuckFn
  def minus: BrainFuckFn
  def less: BrainFuckFn
  def more: BrainFuckFn
  def comma: BrainFuckFn
  def dot: BrainFuckFn
  def open: BrainFuckFn
  def close: BrainFuckFn
  def ignore: BrainFuckFn
  lazy val map = Map("<" -> less, ">" -> more, "," -> comma, "." -> dot, "+" -> plus, "-" -> minus, "[" -> open, "]" -> close)
  def apply(s: String) = map.getOrElse(s, ignore)
}

object BrainFuckOps {
  implicit def defaultOps(implicit bFinput: BFinput, bFOutput: BFOutput, s: StateOps, i: InstructionOps) = new BrainFuckOps {
    val plus: BrainFuckFn = BFInstruction(i.next, s.plus)
    val minus: BrainFuckFn = BFInstruction(i.next, s.minus)
    val less: BrainFuckFn = BFInstruction(i.next, s.less)
    val more: BrainFuckFn = BFInstruction(i.next, s.more)
    val comma: BrainFuckFn = BFInstruction(i.next, s.comma)
    val dot: BrainFuckFn = BFInstruction(i.next, s.dot)
    val open: BrainFuckFn = OpenInstruction(i.findClose, i.next)
    val close: BrainFuckFn = CloseInstruction(i.findOpen, i.next)
    val ignore: BrainFuckFn = BFInstruction(i.next, s.identity)
  }
}


case class BFInstruction(instructionFn: InstructionFn, stateFn: StateFn) extends BrainFuckFn {
  override def apply(bf: Brainfuck) = bf.copy(instructions = instructionFn(bf.instructions), state = stateFn(bf.state))
}
case class OpenInstruction(findClose: OpenCloseFn, next: InstructionFn) extends BrainFuckFn {
  override def apply(bf: Brainfuck): Brainfuck =
    bf.setInstructions(ifStateZero = findClose(bf.indexDataResult, bf.instructions), ifStateNotZero = next(bf.instructions))
}
case class CloseInstruction(findOpen: OpenCloseFn, next: InstructionFn) extends BrainFuckFn {
  override def apply(bf: Brainfuck): Brainfuck =
    bf.setInstructions(ifStateZero = next(bf.instructions), ifStateNotZero = findOpen(bf.indexDataResult, bf.instructions))
}
