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

colang depends on the CO standard library that must be present at any of the following
locations: `~/.colang-libs/`, `/usr/local/lib/colang`, `/usr/lib/colang`, `/lib/colang`.
The standard library is included in this repo (the `stdlib` directory), so the most simple
installation method (on Unix-like systems) is creating a symlink from `~/.colang-libs/`
to the `stdlib` directory:
```
ln -s path/to/repo/stdlib ~/.colang-libs
```

You can now compile and execute the solution to the A + B problem:
```
void main() {
    int a, b
    readInt(a)
    readInt(b)
    writeIntLn(a + b)
}
```

The syntax is by no means final: a much more convenient I/O interface will be eventually
supported.