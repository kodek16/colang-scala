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
```java
void main() {
    int a, b
    read(a)
    read(b)
    println(a + b)
}
```

The syntax is not final: a more convenient I/O interface will be eventually supported.

Let's go for a more fun example. At this point, you can already solve quadratic equations
with CO:

```java
double abs(double x) {
    if (x >= 0.0) return x else return -x
}

double sqr(double x) {
    return x * x
}

double EPS = 1.0e-9

//If we have no sqrt(), we can just write our own :)
double sqrt(double x) {
    double r = 10.0

    while (abs(r * r - x) > EPS) {
        r = r - (sqr(r) - x) / (2.0 * r)
    }

    return r
}

void main() {
    double a, b, c
    read(a)
    read(b)
    read(c)

    double det = sqr(b) - 4.0 * a * c
    if (det < 0.0) {
        //Can't print strings yet :(
        println(-1)
        return
    }

    double r1 = (-b - sqrt(det)) / (2.0 * a)
    double r2 = (-b + sqrt(det)) / (2.0 * a)

    println(r1)
    println(r2)
}
```

For the curious, the generated code looks like [this](http://pastebin.com/fNauPY0K).

