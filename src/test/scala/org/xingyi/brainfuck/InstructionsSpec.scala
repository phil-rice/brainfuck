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
    Instructions(s, s.length + 1).canExecute shouldBe false
  }
  it should "have a canexecute that returns true if pointer inside range of commands" in {
    Instructions(s, 0).canExecute shouldBe true
    Instructions(s, s.length - 1).canExecute shouldBe true
  }

  private val withBrackets = "some[stuff[in]the]way"
  private val helper = "      012345678901234567890"

  behavior of "InstructionOps"

  implicit val instructionOps = implicitly[InstructionOps]
  import instructionOps._
  val result = IndexDataResult(List((4, 17), (10, 13)))

  it should "have a findClose method that throws an exception if the current instruction isn't [" in {
    intercept[NotAtOpenException](findClose(result, Instructions(withBrackets, 0)))
    intercept[NotAtOpenException](findClose(result, Instructions(withBrackets, 3)))
    intercept[NotAtOpenException](findClose(result, Instructions(withBrackets, 5)))
  }

  it should "have a findClose method that finds the correct ]" in {
    findClose(result, Instructions(withBrackets, 4)) shouldBe Instructions(withBrackets, 17)
    findClose(result, Instructions(withBrackets, 10)) shouldBe Instructions(withBrackets, 13)
  }

  it should "have a findOpen method that throws an exception if the current instruction isn't ]" in {
    intercept[NotAtCloseException](findOpen(result, Instructions(withBrackets, 14)))
    intercept[NotAtCloseException](findOpen(result, Instructions(withBrackets, 12)))
    intercept[NotAtCloseException](findOpen(result, Instructions(withBrackets, 0)))
  }

  it should "have a findOpen method that finds the correct [" in {
    findOpen(result, Instructions(withBrackets, 13)) shouldBe Instructions(withBrackets, 10)
    findOpen(result, Instructions(withBrackets, 17)) shouldBe Instructions(withBrackets, 4)
  }

  it should " have a next that adds one to the pointer" in {
    next(Instructions(s, 3)) shouldBe Instructions(s, 4)
  }

  behavior of "MakeIndexData"

  val make = implicitly[MakeIndexData]
  it should "make a 'results' that matchs the open and close brackets" in {
    make("123") shouldBe IndexDataResult(List())
    make("[123]") shouldBe IndexDataResult(List((0, 4)))
    make("1[2]3") shouldBe IndexDataResult(List((1, 3)))
    make("1[2]3[5]6") shouldBe IndexDataResult(List((1, 3), (5, 7)))
    make("1[2[3]5]6") shouldBe IndexDataResult(List((1, 7), (3, 5)))
  }

  it should "throw mismatched bratestckets if the brackets don't match" in {
    def check(s: String) = intercept[MismatchedBracketsException](make(s))
    check("[")
    check("]")
    check("[[")
    check("[[]")
    check("[]]")
    check("][][][")
  }
}
