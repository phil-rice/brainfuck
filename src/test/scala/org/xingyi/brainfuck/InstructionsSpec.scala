package org.xingyi.brainfuck

class InstructionsSpec extends BFSpec {

  behavior of "Instructions"

  val s = "someInstructions"

  it should "have an initial pointer of zero" in {
    Instructions(s) shouldBe Instructions(s, 0)
  }

  it should "have a current method that returns the current instruction as a string" in {
    s.zipWithIndex.foreach { case (ch, i) =>
      withClue(s"Ch is [$ch], i is $i current is  ${Instructions(s, i).current}") {
        Instructions(s, i).current shouldBe ch.toString
      }
    }
  }
  it should "have a get method that returns the character in the instruction as a string" in {
    s.zipWithIndex.foreach { case (ch, i) =>
      withClue(s"Ch is [$ch], i is $i current is  ${Instructions(s, i).current}") {
        Instructions(s, 999).get(i) shouldBe ch.toString
      }
    }
  }

  it should "have a canexecute that returns false if pointer outside range of commands" in {
    Instructions(s, -1).canExecute shouldBe false
    Instructions(s, s.length).canExecute shouldBe false
    Instructions(s, s.length+1).canExecute shouldBe false
  }
  it should "have a canexecute that returns true if pointer inside range of commands" in {
    Instructions(s, 0).canExecute shouldBe true
    Instructions(s, s.length-1).canExecute shouldBe true
  }

  private val withBrackets = "some[stuff[in]the]way"
  private val helper = "      012345678901234567890"

  behavior of "InstructionOps"

  implicit val instructionOps = implicitly[InstructionOps]
  import instructionOps._

  it should "have a findClose method that throws an exception if the current instruction isn't [" in {
    intercept[NotAtOpenException](findClose(Instructions(withBrackets, 0)))
    intercept[NotAtOpenException](findClose(Instructions(withBrackets, 3)))
    intercept[NotAtOpenException](findClose(Instructions(withBrackets, 5)))
  }
  it should "have a findClose method that throws an mismatched brackets if findClose is called and there are no matches" in {
    intercept[MismatchedBracketsException](findClose(Instructions("[[]", 0)))
    intercept[MismatchedBracketsException](findClose(Instructions("[[", 1)))
  }

  it should "have a findClose method that finds the correct ]" in {
    findClose(Instructions(withBrackets, 4)) shouldBe Instructions(withBrackets, 17)
    findClose(Instructions(withBrackets, 10)) shouldBe Instructions(withBrackets, 13)
  }

  it should "have a findOpen method that throws an exception if the current instruction isn't ]" in {
    intercept[NotAtCloseException](findOpen(Instructions(withBrackets, 14)))
    intercept[NotAtCloseException](findOpen(Instructions(withBrackets, 12)))
    intercept[NotAtCloseException](findOpen(Instructions(withBrackets, 0)))
  }

  it should "have a findOpen method that finds the correct [" in {
    findOpen(Instructions(withBrackets, 13)) shouldBe Instructions(withBrackets, 10)
    findOpen(Instructions(withBrackets, 17)) shouldBe Instructions(withBrackets, 4)
  }
  it should "have a findOpen method that throws an mismatched brackets if findOpen is called and there are no matches" in {
    intercept[MismatchedBracketsException](findOpen(Instructions("[]]", 2)))
    intercept[MismatchedBracketsException](findOpen(Instructions("]]", 1)))
  }

  it should " have a next that adds one to the pointer" in {
    next(Instructions(s, 3)) shouldBe Instructions(s, 4)
  }
}
