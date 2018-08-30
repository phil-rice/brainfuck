package org.xingyi.brainfuck

trait InstructionProcessor extends (String => Brainfuck => Brainfuck)


case class Instructions(commands: String, pointer: Int = 0) {
  def canExecute: Boolean = pointer >= 0 && pointer < commands.length
  def current: String = get(pointer)
  def get(p: Int): String = commands.drop(p).take(1)

}


trait InstructionOps {
  def next: InstructionFn
  def findOpen: OpenCloseFn
  def findClose: OpenCloseFn
}

//
//class OtherBrackerFinder(thisBracket: String, otherBracket: String, next: Int => Int, exception: Instructions => Exception) extends InstructionFn {
//  override def apply(i: Instructions): Instructions = {
//    if (i.current != thisBracket) throw exception(i)
//    var p = i.pointer
//    var count = 0
//    var cont = true
//    while (cont) {
//      p = next(p)
//      if (p < 0 || p >= i.commands.length) throw new MismatchedBracketsException
//      i.get(p) match {
//        case x if x == thisBracket => count += 1
//        case x if x == otherBracket && count == 0 => cont = false
//        case x if x == otherBracket => count -= 1
//        case _ =>
//      }
//    }
//    Instructions(i.commands, p)
//  }
//}

trait MakeIndexData extends (String => IndexDataResult)

object MakeIndexData {


  implicit object defaultMakeIndexData extends MakeIndexData {
    def operationsMap(acc: IndexData) = Map('[' -> acc.open, ']' -> acc.close)

    override def apply(s: String): IndexDataResult =
      s.zipWithIndex.foldLeft(IndexData()) { case (acc, (ch, i)) => operationsMap(acc).get(ch).fold(acc)(fn => fn(i)) }.finished
  }
}
case class IndexData(count: Int = 0, data: List[(Int, Boolean, Int, Int)] = List()) {
  def fold[X](countIsZero: => X, countNotZero: => X): X = if (count == 0) countIsZero else countNotZero

  val open: Int => IndexData = { index => IndexData(count + 1, data :+ (data.size, true, count, index)) }
  val close: Int => IndexData = { index => fold(throw new MismatchedBracketsException, IndexData(count - 1, data :+ (data.size, false, count - 1, index))) }
  def result = IndexDataResult(data.filter(_._2).map { case (fromIndex, _, c, from) => data.drop(fromIndex + 1).find(_._3 == c).map { to => (from, to._4) }.head })
  def finished: IndexDataResult = fold(result, throw new MismatchedBracketsException)

}
case class IndexDataResult(data: List[(Int, Int)]) {
  val openToClose = data.toMap
  val closeToOpen = data.map(_.swap).toMap
}

object InstructionOps {
  implicit object DefaultInstructionops extends InstructionOps {
    val findOpen: OpenCloseFn = { (indexData, i) => i.copy(pointer = indexData.closeToOpen.getOrElse(i.pointer, throw new NotAtCloseException(i))) }
    val findClose: OpenCloseFn = { (indexData, i) => i.copy(pointer = indexData.openToClose.getOrElse(i.pointer, throw new NotAtOpenException(i))) }
    val next: InstructionFn = {case Instructions(c, p) => Instructions(c, p + 1)}
  }
}

