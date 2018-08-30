package org.xingyi.brainfuck
import scala.annotation.tailrec

case class Brainfuck(instructions: Instructions, state: State, indexDataResult: IndexDataResult) {
  def canExecute: Boolean = instructions.canExecute
  def currentInstruction: String = instructions.current
  def setInstructions(ifStateZero: => Instructions, ifStateNotZero: => Instructions): Brainfuck = copy(instructions = if (state.get == 0) ifStateZero else ifStateNotZero)
}
object Brainfuck {
  def apply(s: String)(implicit makeIndexData: MakeIndexData): Brainfuck = Brainfuck(Instructions(s, 0), State(), makeIndexData(s))
}


class BrainFuckExecutor(implicit brainFuckOps: BrainFuckOps) {
  def executeOne(bf: Brainfuck) = brainFuckOps(bf.currentInstruction)(bf)

  @tailrec
  final def execute(bf: Brainfuck): Brainfuck = if (bf.canExecute) execute(executeOne(bf)) else bf
}

object BrainFuckExecutor extends App {
  println("Executing")
  val executor = new BrainFuckExecutor
  executor.execute(Brainfuck("+++   qlwkejqlwje ++[->+++<]"))
}


