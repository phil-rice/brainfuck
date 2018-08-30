package org.xingyi.brainfuck
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

class BFIOSpec extends BFSpec {

  behavior of "BFOutput"

  it should "print a nubtmber to the console" in {
    val output = new ByteArrayOutputStream()
    Console.withOut(output) {implicitly[BFOutput] apply 1}
    output.toByteArray shouldBe Array(49) //TODO
  }


  behavior of "BFInput"

  it should "read an character from the input" in {
    val input = new ByteArrayInputStream(Array(1, 2, 3))
    Console.withIn(input) {
      implicitly[BFinput].apply shouldBe 1
    }
  }
}
