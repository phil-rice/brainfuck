package org.xingyi.brainfuck
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.mockito.Mockito._
class BrainfuckSpec extends BFSpec {

  behavior of "Brainfuck"

  it should "have a can executethat delegates to instructions" in {
    def checkCanExecute(expected: Boolean) {
      val i = mock[Instructions]
      when(i.canExecute) thenReturn expected
      Brainfuck(i, State()).canExecute shouldBe expected
    }
    checkCanExecute(false)
    checkCanExecute(true)
  }

  it should "have a currentInstruction method that delegates to instructions" in {
    val i = mock[Instructions]
    when(i.current) thenReturn "some"
    Brainfuck(i, State()).currentInstruction shouldBe "some"
  }

  it should "have a helper method that makes a BrainFuck from a string" in {
    Brainfuck("abd") shouldBe Brainfuck(Instructions("abd", 0), State(Map(), 0))
  }


  behavior of "BFInstruction"

  it should " have an apply that returns a new BF with the stateOps and instructionops applied" in {
    val stateFn = mock[StateFn]
    val instructionFn = mock[InstructionFn]

    val state1 = mock[State]
    val state2 = mock[State]
    val instruction1 = mock[Instructions]
    val instruction2 = mock[Instructions]
    when(stateFn.apply(state1)) thenReturn state2
    when(instructionFn.apply(instruction1)) thenReturn instruction2

    val bFInstruction = BFInstruction(instructionFn, stateFn)
    bFInstruction(Brainfuck(instruction1, state1)) shouldBe Brainfuck(instruction2, state2)

  }
  behavior of "BrainFuckOps"

  val i = implicitly[InstructionOps]
  val s = implicitly[StateOps]
  val bfOps = BrainFuckOps.defaultOps(mock[BFinput], mock[BFOutput], s, i)

  import bfOps._
  it should "have a apply that is set up correctly" in {
    apply("<") shouldBe less
    apply(">") shouldBe more
    apply("+") shouldBe plus
    apply("-") shouldBe minus
    apply(".") shouldBe dot
    apply(",") shouldBe comma
    apply("[") shouldBe open
    apply("]") shouldBe close
    apply("x") shouldBe bfOps.ignore
    apply("y") shouldBe bfOps.ignore
    apply("=") shouldBe bfOps.ignore
  }

  //not sure of value of this..
  it should "have operations that delegate to instructions and state" in {
    plus shouldBe BFInstruction(i.next, s.plus)
    minus shouldBe BFInstruction(i.next, s.minus)
    less shouldBe BFInstruction(i.next, s.less)
    more shouldBe BFInstruction(i.next, s.more)
    dot shouldBe BFInstruction(i.next, s.dot)
    comma shouldBe BFInstruction(i.next, s.comma)
    open shouldBe BFInstruction(i.findClose, s.identity)
    close shouldBe BFInstruction(i.findClose, s.identity)
    bfOps.ignore shouldBe BFInstruction(i.next, s.identity)

  }

  behavior of "BrainFuckExecutor"

  it should "have an executeOne instruction that finds the BfInstruction based on the current instruction and execute it" in {
    implicit val brainFuckOps = mock[BrainFuckOps]
    val executor = new BrainFuckExecutor
    val input = mock[Brainfuck]
    val instruction = mock[BFInstruction]
    val expected = mock[Brainfuck]
    when(input.currentInstruction) thenReturn "cur"
    when(brainFuckOps.apply("cur")) thenReturn instruction
    when(instruction.apply(input)) thenReturn expected
    executor.executeOne(input) shouldBe expected
  }

}
