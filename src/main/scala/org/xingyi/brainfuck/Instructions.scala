package org.xingyi.brainfuck

trait InstructionProcessor extends (String => Brainfuck => Brainfuck)


case class Instructions(commands: String, pointer: Int = 0) {
  def canExecute: Boolean = pointer>=0 && pointer<commands.length
  def current: String = get(pointer)
  def get(p: Int): String = commands.drop(p).take(1)
}


trait InstructionOps {
  def next: InstructionFn
  def findOpen: InstructionFn
  def findClose: InstructionFn
}


object InstructionOps {
  implicit object DefaultInstructionops extends InstructionOps {
    val next: InstructionFn = {case Instructions(c, p) => Instructions(c, p + 1)}
    val findOpen: InstructionFn = { i: Instructions =>
      def findOpen(p: Int, count: Int): Instructions = {
//        println(s"findOpen($p, $count, ${i.get(p)}")
        if (p < 0) throw new MismatchedBracketsException
        i.get(p) match {
          case "[" if count == 1 => i.copy(pointer = p)
          case "[" => findOpen(p - 1, count - 1)
          case "]" => findOpen(p - 1, count + 1)
          case _ => findOpen(p - 1, count)
        }
      }
      if (i.current == "]") findOpen(i.pointer - 1, 1) else throw new NotAtCloseException(i)
    }


    val findClose: InstructionFn = {
      i =>
        def findClose(p: Int, count: Int): Instructions = {
//          println(s"findClose($p, $count, ${i.get(p)}")
          i.get(p) match {
            case "[" => findClose(p + 1, count + 1)
            case "]" if count == 1 => i.copy(pointer = p)
            case "]" => findClose(p + 1, count - 1)
            case "" => throw new MismatchedBracketsException
            case _ => findClose(p + 1, count)
          }
        }
        if (i.current == "[") findClose(i.pointer + 1, 1) else throw new NotAtOpenException(i)
    }
  }
}

