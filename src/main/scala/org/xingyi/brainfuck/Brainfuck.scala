package org.xingyi.brainfuck
import org.xingyi.brainfuck

import scala.annotation.tailrec

case class Brainfuck(instructions: Instructions, state: State) {
  def canExecute: Boolean = instructions.canExecute

  def currentInstruction = instructions.current
}
object Brainfuck {
  def apply(s: String): Brainfuck = Brainfuck(Instructions(s), State())
}


case class BFInstruction(instructionFn: InstructionFn, stateFn: StateFn) extends BrainFuckFn {
  override def apply(bf: Brainfuck) = Brainfuck(instructionFn(bf.instructions), stateFn(bf.state))
}

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
    val open: BrainFuckFn = BFInstruction(i.findClose, s.identity)
    val close: BrainFuckFn = BFInstruction(i.findClose, s.identity)
    val ignore: BrainFuckFn = BFInstruction(i.next, s.identity)
  }
}


class BrainFuckExecutor(implicit brainFuckOps: BrainFuckOps) {
  def executeOne(bf: Brainfuck) = brainFuckOps(bf.currentInstruction)(bf)

  @tailrec
  final def execute(bf: Brainfuck): Brainfuck = {
    if (bf.canExecute) execute(executeOne(bf)) else bf
  }
}

object BrainFuckExecutor {

  def main(args: Array[String]): Unit = {
    println("hello")
    val executor = new BrainFuckExecutor
    executor.execute(Brainfuck("+++   qlwkejqlwje ++[->+++<]"))
  }
}


