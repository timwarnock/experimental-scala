# Scala is a Scripting Language!
Behold! Scala is a perfectly acceptable shell scripting langauge.

Unfortunately, I keep seeing things like this,
```scala
#!/bin/sh
exec scala "$0" "$@"
!#
object HelloWorld {
  def main(args: Array[String]) {
    println("Hello, world! " + args.mkString(","))
  }
}
HelloWorld.main(args)

```

Stop that. Seriously. That's a bash script that unecessarily calls *exec scala* -- the effect of this is to fork an additional process for no apparent reason.

Instead, you can simply do this,
```scala
#!/usr/bin/env scala

println(s"Hello, world! ${args.mkString(",")}")
```

## Creating a Scala shell script:

### Step 1.
Write your script, e.g.,

```scala
#!/usr/bin/env scala
import scala.io.StdIn.{readLine}

val name = readLine("What is your name? ")
val nihao = "你好" + name.split(" ").map(_.capitalize).mkString(" ")

val mults = readLine("Choose a number between 1 and 10: ")
var mult = 1
if (((x: String) => x forall Character.isDigit)(mults))
  mult = (mults.toInt-1) % 10 + 1

print((nihao + "\n") * mult)

```

You can import all the usual scala libraries, including your own application code.

### Step 2.
Make the scala script executable

```shell
$ chmod 0755 hello-zh.scala
```

### Step 3.
Run the script as you would any other.

```shell
$ ./hello-zh.scala
What is your name? tim warnock
Choose a number between 1 and 10: 5
你好Tim Warnock
你好Tim Warnock
你好Tim Warnock
你好Tim Warnock
你好Tim Warnock

```

