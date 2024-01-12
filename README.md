# Brewlis: Simple Lisp Interpreter in Rust

## Overview
This project is a basic Lisp interpreter implemented in Rust, aimed primarily at learning and exploring the Rust programming language. The interpreter is minimalistic, yet functional, providing a hands-on approach to understanding both Lisp and Rust.

## Features
- Basic interpretation of Lisp code.
- Simple and intuitive error handling.
- Lightweight with minimal dependencies.

## Open
- Only floats, no strings, no integer

## Lisp example
This section explains the implementation of the factorial function in Brewlis, a Lisp-like programming language. The factorial of a number is the product of all positive integers less than or equal to that number.

```lisp
(begin
  (define factorial
    (lambda n
      (if (<= n 1)
          1
          (* n (factorial (- n 1))))))
(factorial 5)
)
```

## Dependencies
- `anyhow`: Provides idiomatic error handling.
- `logos`: Used for lexical analysis/tokenization.

## Getting Started

### Prerequisites
- Rust: Make sure you have Rust installed on your system. You can download it from [the official Rust website](https://www.rust-lang.org/).

### Installation
1. Clone the repository:
   ```bash
   git clone https://github.com/szma/brewlis
   ```
2. Run the REPL
   ```bash
   cargo run
   ```
