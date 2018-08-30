name := "brainfuck"

version := "0.1"


val versions = new {
  val scala = "2.12.6"
  val scalatest = "3.0.5"
  val mockito = "1.10.19"
}

version := "0.3"
organization := "one.xingyi"
scalaVersion := versions.scala
scalacOptions ++= Seq("-feature")
libraryDependencies += "org.mockito" % "mockito-all" % versions.mockito % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % versions.scalatest % "test"


