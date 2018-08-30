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
      Brainfuck(i, State(), mock[IndexDataResult]).canExecute shouldBe expected
    }
    checkCanExecute(false)
    checkCanExecute(true)
  }

  it should "have a currentInstruction method that delegates to instructions" in {
    val i = mock[Instructions]
    when(i.current) thenReturn "some"
    Brainfuck(i, State(), mock[IndexDataResult]).currentInstruction shouldBe "some"
  }

  it should "have a helper method that makes a BrainFuck from a string" in {
    Brainfuck("ab[c]d[e]") shouldBe Brainfuck(Instructions("ab[c]d[e]", 0), State(Map(), 0), IndexDataResult(List((2, 4), (6, 8))))
  }


  it should "have a setInstructions instruction that calls 'ifStateZero' if the state is zero, and ifNotZero if the state is not zero" in {
    val instructions0 = mock[Instructions]
    val instructions1 = mock[Instructions]
    val bf0 = Brainfuck("something")
    val bf1 = bf0.copy(state = State(Map(0 -> 1), 0))

    bf0.setInstructions(instructions0, instructions1) shouldBe bf0.copy(instructions = instructions0)
    bf1.setInstructions(instructions0, instructions1) shouldBe  bf1.copy(instructions = instructions1)
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
    val dataResult = mock[IndexDataResult]
    bFInstruction(Brainfuck(instruction1, state1, dataResult)) shouldBe Brainfuck(instruction2, state2, dataResult)

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

  //I wasn't sure of value of this.. but it found a bug!
  it should "have operations that delegate to instructions and state" in {
    plus shouldBe BFInstruction(i.next, s.plus)
    minus shouldBe BFInstruction(i.next, s.minus)
    less shouldBe BFInstruction(i.next, s.less)
    more shouldBe BFInstruction(i.next, s.more)
    dot shouldBe BFInstruction(i.next, s.dot)
    comma shouldBe BFInstruction(i.next, s.comma)
    open shouldBe OpenInstruction(i.findClose, i.next)
    close shouldBe CloseInstruction(i.findOpen, i.next)
    bfOps.ignore shouldBe BFInstruction(i.next, s.identity)
  }

  behavior of "OpenInstruction"

  it should "if the current state is zero will findClose" in {
    val findClose = mock[OpenCloseFn]
    val startInstructions = mock[Instructions]
    val endInstructions = mock[Instructions]
    val indexData = mock[IndexDataResult]
    when(findClose.apply(indexData, startInstructions)) thenReturn (endInstructions)

    val start = Brainfuck(startInstructions, State(), indexData)
    val o = OpenInstruction(findClose, mock[InstructionFn])

    o(start) shouldBe Brainfuck(endInstructions, State(), indexData)

  }
  it should "if the current state is not zero will next" in {
    val next = mock[InstructionFn]
    val startInstructions = mock[Instructions]
    val endInstructions = mock[Instructions]
    val indexData = mock[IndexDataResult]
    when(next.apply(startInstructions)) thenReturn (endInstructions)

    val start = Brainfuck(startInstructions, State(Map(0 -> 1), 0), indexData)
    val o = OpenInstruction(mock[OpenCloseFn], next)

    o(start) shouldBe Brainfuck(endInstructions, State(Map(0 -> 1)), indexData)

  }

  behavior of "CloseInstruction"

  it should "if the current state is zero will next" in {
    val next = mock[InstructionFn]
    val startInstructions = mock[Instructions]
    val endInstructions = mock[Instructions]
    when(next.apply(startInstructions)) thenReturn (endInstructions)
    val indexData = mock[IndexDataResult]

    val start = Brainfuck(startInstructions, State(), indexData)
    val o = CloseInstruction(mock[OpenCloseFn], next)

    o(start) shouldBe Brainfuck(endInstructions, State(), indexData)

  }
  //
  //    it should "if the current state is not zero will findOpen" in { //TODO
  //      val next = mock[InstructionFn]
  //      val startInstructions = mock[Instructions]
  //      val endInstructions = mock[Instructions]
  //      when(next.apply(startInstructions)) thenReturn (endInstructions)
  //
  //      val start = Brainfuck(startInstructions, State(Map(0 -> 1), 0))
  //      val o = OpenInstruction(mock[InstructionFn], next)
  //
  //      o(start) shouldBe Brainfuck(endInstructions, State(Map(0 -> 1)))
  //
  //
  //    }

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
