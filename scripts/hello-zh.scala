#!/usr/bin/env scala
import scala.io.StdIn.{readLine}


val name = readLine("What is your name? ")
val nihao = "你好" + name.split(" ").map(_.capitalize).mkString(" ")

val mults = readLine("Choose a number between 1 and 10: ")
var mult = 1
if (((x: String) => x forall Character.isDigit)(mults))
  mult = (mults.toInt-1) % 10 + 1

print((nihao + "\n") * mult)
