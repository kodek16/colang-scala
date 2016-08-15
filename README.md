# colang - CO language compiler
CO is a language designed for use in programming olympiads and contests like IOI and ACM ICPC.
It features a simple C-style syntax, opinionated algorithm-oriented standard library,
C++ tier performance, and many small bits that make solving tasks easier and more fun.

colang translates CO source code into C source code, so you can use CO on every judge that
accepts C or C++.

## Status
colang is in early development stage, so it can't do much yet. This section will be updated
when most core language features will be implemented, so stay tuned!

## Building
If you still want to try it out right now, grab the source, install SBT, and build the latest
version with `sbt assembly`. It will produce an executable standalone JAR file under
`target/scala-2.11`.

colang requires a file named `prelude.co` to exist under one of standard libraries folders.
It provides the entry point to the standard library. That said, CO doesn't really have a
standard library yet, so it should only contain native symbols declarations. This file under
`~/.colang-libs/prelude.co` will be enough to compile and run the solution to the A + B task:
```
native struct void

native struct double {
    native double plus(double other)
}

native void read(double x)
native void writeln(double x)
```

The solution looks like this by the way:
```
void main() {
    double a, b
    read(a)
    read(b)
    writeln(a + b)
}
```

The syntax is by no means final: a much more convenient way to read variables will be eventually
supported.