package org.xingyi.brainfuck
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._
class StateSpec extends BFSpec {

  behavior of "State"
  it should "have a get method that returns the data at the current location, or zero if nothing there" in {
    State().get shouldBe 0
    State(pointer = 6).get shouldBe 0
    State(Map(1 -> 3), 1).get shouldBe 3
  }

  it should "start with an empty map" in {
    State().data.size shouldBe 0
  }
  it should "start with the pointer set to zero" in {
    State().pointer shouldBe 0
  }
  behavior of "default Stateops"

  val stateOps = implicitly[StateOps]
  import stateOps._

  it should "have a less method that decrements the pointer, preserving the date" in {
    val data = mock[Map[Int, Int]]
    less(State(data)) shouldBe State(data, -1)
    less(State(data, 3)) shouldBe State(data, 2)
  }

  it should "have a more method that increments the pointer, preserving the date" in {
    val data = mock[Map[Int, Int]]
    more(State(data, -2)) shouldBe State(data, -1)
    more(State(data)) shouldBe State(data, 1)
    more(State(data, 3)) shouldBe State(data, 4)
  }

  it should "have a plus method  that increments the data at the pointer, keeping the pointer constant" in {
    plus(State(Map(), -2)) shouldBe State(Map(-2 -> 1), -2)
    plus(State(Map(-2 -> 1), -2)) shouldBe State(Map(-2 -> 2), -2)

    plus(State(Map(), 2)) shouldBe State(Map(2 -> 1), 2)
    plus(State(Map(2 -> 1), 2)) shouldBe State(Map(2 -> 2), 2)

    plus(State(Map(1 -> 3, 2 -> 1), 2)) shouldBe State(Map(1 -> 3, 2 -> 2), 2)
  }

  it should "have a minus method  that decrements the data at the pointer, keeping the pointer constant" in {
    minus(State(Map(), -2)) shouldBe State(Map(-2 -> -1), -2)
    minus(State(Map(-2 -> 1), -2)) shouldBe State(Map(-2 -> 0), -2)

    minus(State(Map(), 2)) shouldBe State(Map(2 -> -1), 2)
    minus(State(Map(2 -> 1), 2)) shouldBe State(Map(2 -> 0), 2)

    minus(State(Map(1 -> 3, 2 -> 1), 2)) shouldBe State(Map(1 -> 3, 2 -> 0), 2)
  }

  def checkDotFor(pointer: Int)(data: (Int, Int)*)(expected: Int) = {
    implicit val printer = mock[BFOutput]
    val stateOps = implicitly[StateOps]
    val state = State(Map(data: _*), pointer)
    stateOps.dot.apply(state) shouldBe state
    verify(printer, times(1)).apply(expected)
  }

  it should "have a `.` method that prints the current character, without changing the state" in {
    checkDotFor(0)()(expected = 0)
    checkDotFor(0)(0 -> 10, 1 -> 2)(expected = 10)
    checkDotFor(1)(0 -> 10, 1 -> 2)(expected = 2)
  }
  def checkCommaFor(pointer: Int)(input: Int, data: (Int, Int)*)(expected: (Int, Int)*) = {
    implicit val reader = mock[BFinput]
    val stateOps = implicitly[StateOps]
    when(reader.apply()) thenReturn input
    val initialState = State(Map(data: _*), pointer)
    val expectedState = State(Map(expected: _*), pointer)
    stateOps.comma.apply(initialState) shouldBe expectedState
  }

  it should " have a ',' method that puts the next byte from the input, without changing the state" in {
    checkCommaFor(2)(999)(2 -> 999)
    checkCommaFor(2)(999, 1 -> 3)(1 -> 3, 2 -> 999)
  }
}
