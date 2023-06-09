# Racket to x86 Compiler

This project aims to develop a compiler that transforms Racket, a high-level programming language, into x86 assembly language, which is commonly used for modern register machine architectures.

## Objectives

- Implement the basic translations necessary to compile modern high-level languages to modern register machine architectures.
- Identify and describe limitations in existing languages.
- Design and implement languages to lift limitations.
- Describe language features at different levels of abstractions, corresponding to different stages of compilation.
- Differentiate between invariants that must be enforced, and invariants that can be relied upon.

## Compiler Features Studied

1. Introduction to Language Design:
   - Understanding a language
   - Statement-based language
   - x64

2. Interpreters:
   - Type checking
   - Creating abstractions: variables and value-based language
   - The stack
   - Run-time systems
   - Instruction selection

3. The Pros and Cons of Abstraction:
   - Register allocation
   - Program analysis
   - Graph coloring
   - Optimization

4. Exposing Low-Level Control Flow:
   - Jumps
   - Conditional statements
   - Basic blocks
   - Control flow and program analysis

5. Abstracting Control Flow:
   - Tail calls (non-returning functions)
   - Continuation-passing style
   - Calling conventions
   - Control flow and program analysis

6. Function Call and Return:
   - Non-tail calls

7. Exposing Memory Access:
   - Heap
   - Memory safety

8. Adding Data Structures and Primitives:
   - Tagging
   - Immediate data: booleans, symbols, integers
   - Structured data: strings, lists, arrays
   - Dynamic typing
   - Memory management
   - Garbage collection

9. First-class Computations:
   - Closure conversion
   - Lexical scope
   - Closure optimizations

10. Surface Language Niceties:
    - Implicit casts
    - Syntactic sugar

