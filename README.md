# About

**STAP - a statically typed programming language that compiles down to javascript.**

STAP, as its description suggests is a programming language that aims to add type safety to vanilla javascript. Unlike Typescript STAP syntax is actually very similar to the language its written in (Rust) although having an original take on some things.

## Manual

A way to hop around the README easier.

- [About](#about)
- [Startup](#startup)
- [Version information](#version-information)
- [Syntax](#syntax)
    - [Functions](#functions)
     
## Startup

To run the compiler simply build it for your current platform through cargo or rustc (or checkout the pre-built binaries if they are ready [here](src/bin)) and run it by passing in the path to your file as an argument.
## Version information
STAP is currently in its alpha and a lot of things are still experimental and may change in the near future!

For information on current Versions checkout: 

- [Versions.md](versions.md)
    - [Manual](versions.md#manual)


## Syntax
Syntax in STAP is very similar to that of Rust as Rust syntax is objectively short and easier to follow. Although its similar STAP syntax takes its own original take on syntax.

### Functions

**Added in:** [0.0.1A](versions.md#001a)

Functions in STAP are defined with the word 'fn' to shorten 'function' from javascript. To define a function just say fn followed by the name and the arguments in paren brackets.

**Example(s)**:
*[NOTE] These examples currently do not work as parameters and others are yet to be implemented and this is just made as a way to demonstrate the idea of the syntax*
```sl
fn add(a: number, b: number) -> number {
    ret a+b;
}
```