emaccs
======

Emaccs is many things:

* A partial implementation of R5RS Scheme on Lua;
* A modal text editor emulating Vim;
* A tabbing window manager;
* A complete reimplementation of CraftOS's shell in Scheme.

Scheme 51
=========

Scheme 51 is what emaccs is written in. Scheme 51 is a derivative of
R5RS scheme, that supports most of the standard features except for
what's listed here:

* Port support is basically entirely missing, but is WIP;
* `call-with-current-continuation` will never be implemented for
  ideological reasons

Since R5RS does not _require_ complex numbers, we don't support them
either. Rational numbers, however, are fully supported.

The Scheme 51 ["runtime"](/boot/runtime.lua) consists of the mortal
remains of the interpreter that was previously used to boot Scheme 51.
However, the language has since progressed to the point where the
interpreter is _not_ capable of booting the compiler! It's only kept
alive for the implementation of the reader and some runtime support
functions (rationals, primitives that would be unwieldy to implement in
Scheme, etc).

Booting
-------

"Booting" is the process by which Scheme 51 loads and compiles itself.
The boot driver was originally a shell script, but is now a Scheme file,
`/boot.ss`.

This file can **always** create a new compiler starting from the sources
and runtime library. Don't lose it!

The Language
------------

Scheme 51 is standard fare as far as Scheme-like languages go.
Procedures are created with `(lambda args . body)` forms, all code is
lists, all lists are cons lists, etc. Here are the notable differences:

### Typing

Report Scheme is a dynamically typed language, and so is Scheme 51, but
Scheme 51 is much laxer when it comes to strong typing than report
scheme. For example, the domain of `(car)` encompasses any hash-table
that has an entry for the number `1`, symbols, etc.

### Documentation

In a (futile) effort to be self-documenting, Scheme 51 compiles
procedures of the form below into a Lua table with `doc` and `args`
fields, containing the documentation string and a quoted representation
of the arguments respectively.

```scheme
  (lambda args
    "Documentation string"
    body)
```

This documentation can be accessed with `documentation-for-procedure`.
These "documented procedures" behave normally: they can be called, can
be applied, respond #t to `procedure?`, etc.

### The module system

Scheme 51's implementation of `(use-modules)` tries to (running on
ComputerCraft) pre- and re-compile files when they've changed. This
entails some weirdness. For example, if your module has any "main" code,
you damn better put it in a function!

The `(phase)` parameter indicates whether a module is being compiled
(`(= (phase) 'compiling)`) or executed (`(= (phase) loading)`). Code is
subject to run multiple times under different phases.

### Using the compiler

The compiler presents itself with the regular Scheme interface for
dealing with code at runtime, namely `(eval)` and `(load)`. However, it
has some extra functionality:

* `(compile-and-load expression)` - Under the hood, this is what `(eval)` is
implemented with. It compiles a Scheme expression to a procedure, using
Lua's `load` function. This procedure can be stored and called any
number of times. Instead of `(lambda () (eval e))`, try
`(compile-and-load e)`!

* `(compile-file path)` - Compile a Scheme _file_ to a Lua chunk, and
print the result to standard output. This is used for booting and
precompiling modules.

* `(escape-symbol s)` - Turn a Scheme symbol into a string that
represents the Lua identifier that will be used to represent that
symbol.

* `(repl)` - A simple REPL with no fanciness. Suitable for debugging.

Disclaimer
==========

This program is not a program of honour.

No highly esteemed procedure is executed here.

What is here is dangerous and repulsive to us.

The danger is still present, in your time, as it was in ours, without
even the warranty, express or implied, of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE.

This program is best shunned and left unused, though it is free
software, and you are welcome to redistribute and modify it under the
conditions of the BSD 3-clause licence.

