package org.xingyi.brainfuck


class NotAtOpenException(i: Instructions) extends RuntimeException(i.toString)
class NotAtCloseException(i: Instructions) extends RuntimeException(i.toString)
class MismatchedBracketsException extends RuntimeException
