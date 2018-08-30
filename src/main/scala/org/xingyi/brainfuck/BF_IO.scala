package org.xingyi.brainfuck

trait BFOutput extends (Int => Unit)

object BFOutput {
  implicit def default: BFOutput = i => System.out.println(i)
}
trait BFinput extends (() => Int)
object BFinput {
  implicit def default: BFinput = () => Console.readChar()
}
