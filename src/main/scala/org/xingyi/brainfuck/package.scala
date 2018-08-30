
package org.xingyi

package object brainfuck {

  type StateFn  = State => State
  type InstructionFn = Instructions => Instructions
  type BrainFuckFn = Brainfuck=> Brainfuck
}
