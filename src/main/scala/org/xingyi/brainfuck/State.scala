package org.xingyi.brainfuck

case class State(data: Map[Int, Int] = Map(), pointer: Int = 0) {
  def get: Int = data.getOrElse(pointer, 0)
}

trait StateOps {
  def less: StateFn
  def more: StateFn
  def minus: StateFn
  def plus: StateFn
  def dot: StateFn
  def comma: StateFn
  def identity: StateFn
}
object StateOps {
  implicit def DefaultStateOps(implicit bFOutput: BFOutput, bFinput: BFinput) = new StateOps {
    val less: StateFn = {case State(data, pointer) => State(data, pointer - 1)}
    val more: StateFn = {case State(data, pointer) => State(data, pointer + 1)}
    val minus: StateFn = {case s@State(data, pointer) => State(data + (pointer -> (s.get - 1)), pointer)}
    val plus: StateFn = {case s@State(data, pointer) => State(data + (pointer -> (s.get + 1)), pointer)}
    val dot: StateFn = s => {bFOutput(s.get); s}
    val comma: StateFn = {case s@State(data, pointer) => State(data + (pointer -> (bFinput())), pointer)}
    val identity: StateFn = s => s
  }
}