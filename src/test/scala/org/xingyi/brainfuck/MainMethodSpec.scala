package org.xingyi.brainfuck
import java.io.OutputStream

class MainMethodSpec extends BFSpec {


  behavior of "MainMethod"

  it should "terminate" in {
    println("starting main")
    Console.withOut(mock[OutputStream]){BrainFuckExecutor.main(Array())}
  }
}
