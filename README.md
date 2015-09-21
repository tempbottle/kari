# Kari

Kari is a programming language designed to be embedded in Rust programs and the API and language design should reflect that. It is inspired by the Lua language in many ways.

## Example
It doesn't do much yet but here is a simple example:
```
def f(x) {
    x + 1
}

let a := f(1)
let b := a / 10
let c := a * b + 1 / 2

if a = b {
    let b := a
}
else {
    let b := c
}
```

## Features
 - Scripts can be compiled to bytecode once and then loaded at runtime, skipping the overhead from compiling and optimizing

## Goals

### Simplicity
Kari is intended to be as simple as possible, but still allow expressive typing. This means that it sacrifices the ability to easily construct very large software for the ability to easily construct small software. This is because the intended use case of Kari is as an embedded language for small parts of a large project written in a language designed for large projects: Rust. This is the rationale behind some of the decisions such as dynamic typing.

### Pleasant API
It should be as easy as possible to embed Kari in another Rust program. This means that the API for embedding should be as easy to use and pleasant as possible. Other languages seem to do a phenomenally bad job of this, I want to do better. In this case it is inspired by ChaiScript for C++.

### Speed
Performance is always important. Kari should have very little overhead and should never be the performance bottleneck in an application. This could even mean implementing a JIT compiler if needed.


## Future Work
 - Extend the language to the point that it could be considered usable
 - Implement a easy to use API, hopefully involving very little work to expose existing code
 - Optimize the bytecode compiler's output

### Possibly
 - JT
