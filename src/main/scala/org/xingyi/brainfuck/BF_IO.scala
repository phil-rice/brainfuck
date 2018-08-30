package org.xingyi.brainfuck

trait BFOutput extends (Int => Unit)

object BFOutput {
  implicit def default: BFOutput = i => Console.print(i.toChar)
}
trait BFinput extends (() => Int)
object BFinput {
  implicit def default: BFinput = () => Console.readChar()
}
