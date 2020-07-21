emaccs
======

TBD. A startup file/text editor/window manager powered by an in-house
Lisp implementation.

Scheme 51
=========

A Scheme interpreter disguised as a startup file disguised as a Scheme
interpreter. The interpreter (which is continuation-passing for no
reason at all) can load the compiler, which can compile itself, and a
compiler-compiled compiler is identical to an interpreter-compiled
compiler modulo generated identifier names `#.X`.

The compiler-compiled compiler retains the compiler at runtime, which
is used both for `(load)` and `(eval)`. Primitive module support is
built on `(load)`.

Booting
=======

Both the interpreted and compiled implementations initially prompt drop
to a Scheme REPL, which loads the file `boot.ss` (The compiled
implementation has this file baked in). Commands can be written to the
REPL on standard input, with output collected from standard output. The
`boot.sh` script loads and compiles the necessary files to produce a
compiled image capable of booting again.

Booting in CC
-------------

Since the interpreter is ungodly slow, booting in computercraft is not
recommended, and, in fact, barely possible. ComputerCraft does not
support redirecting output, so a hack will have to be used instead.

The `scm_print` procedure of the interpreter, which is hooked up to the
Scheme procedure `(write)`, uses the Lua global `write` for output.
Overwriting the global `write` with something that writes to a file will
then have the effect of redirecting compiler output to a file.

To compile:
```lua
(load "case.ss")         ; needed for compiler
(load "compiler.ss")     ; the compiler itself
(compile-file "boot.ss") ; writes the compiled code to stdout
(compile-file "case.ss")
(compile-file "compiler.ss")
```

Since the interpreter supports applying Lua procedures, compiled procs
and interpreted procs can be freely mixed. Something like this can be
used to replace the interpreted REPL with a compiled REPL live:

```scheme
(load "case.ss")     ; initialise the compiler
(load "compiler.ss")
(load "boot.ss")     ; initialise the compiled environment
(load "case.ss")
(load "compiler.ss")
(repl #t 0)
```

If this succeeded, the prompt should have changed to `load>`.

Using a booted REPL
-------------------

This will be orders of magnitude faster than the interpreter. Get one by
doing `bash boot.sh startup.lua` in a competent UNIX environment: the
produced file `scheme51.lua` will function as though the six commands
above had been run. The booted REPL has the same `>` prompt as the
interpreter REPL.

You can check whether or not an implementation was booted by running:

```
> platform
```

If it's either `Scheme 51` or `Boot Scheme`, this is the compiler.
Otherwise, it's the interpreter.

The Scheme 51 System
====================

Compiling Scheme Code
---------------------

Expressions can be turned into procedures using `(compile-and-load)`.
They can be run with `(compile-and-run)`, or the shorter `(eval)`.

**Beware**: the interpreter also supports `(eval)`, but of course, this
won't be a fast implementation.

Calling Native Code
-------------------

The `call/native` proc can address and call a Lua function as long as
it's global. Both the interpreter and compiler support calling Lua
functions as though they were Scheme procs. For example:

```scheme
> (define (clear)
    (call/native '(term clear))
    (call/native '(term setCursorPos) 1 1))
```

The macro `run/native` supports executing Lua code directly:

```
> (run/native "function x() return 1 end")
nil
> (call/native 'x)
1
```

The macro `define/native` defines a Scheme procedure with a Lua body.

**Note**: The names of the procedure and all of its arguments will be
escaped. The proc `(escape-symbol)` escapes a symbol. If the symbol
contains no non-identifier characters, just prefix `_` and you'll be
fine.

```
> (define/native (p x) "return print(_x)")
> (p 1)
1
1
```

Global Bindings
---------------

Bindings declared with `(define)` at top-level are dynamically, and not
lexically, scoped. They can still be shadowed by lexical variables
defined in an inner scope, but they will not be in the closure of any
`(lambda x x)` expressions by default.

This means that you can redefine how the Scheme system works at runtime:

```
> (set! compile-expr
    (let ((old-ce compile-expr))
      (lambda (r e)
        (if (= e '(+ 2 2))
          (r "5")
          (old-ce r e)))))
> (+ 2 2)
5
```

Compiler Behaviour
------------------

Here is a short list of things that might be modified with productive
effect:

* The **macro expander**. Change the procedure `(expand)`.
* The **expression compiler**. Change the procedure `(compile-expr)`.
* The **function body compiler**. Change the procedure `(compile-body)`.
* The **symbol escaper**. Change the procedure `(escape-symbol)`.
* The **loader**. Change the procedure `(load)`.

Macro Expansion
---------------

Macro expansion is carried out by a Scheme procedure, `(expand e)`,
defined in `boot.ss`. If you change `boot.ss`, beware that macros will
only function _after_ the definition for `(expand)` has been
interpreted.

This is **not** the case in the compiler, since that will re-use the
host `(expand)` for its `load`s. However, it is highly preferrable for
the boot chain not to be interrupted, so that you only need a
`startup.lua` interpreter for the system to function.

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

