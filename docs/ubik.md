Ubik
=====

*This document is a work-in-progress, and everything in it is subject to
change. The notes present in the docs directory are much more likely to
represent the current state of affairs.*

Ubik will eventually be a programming language. Ubik aims to be a
programming language that approximates the combination of the.
conciseness of Haskell, the simplicity of Lisp, the productivity of
Python and the deployability of Go. Ubik is what I wish I could write
everything in, but first I have to write Ubik.

Ubik will have strict, static typing with full type inference. Ubik
will be homoiconic, and provide language-level features that allow users
to control order-of-evaluation; in doing so, it will obviate the need
for macros. Ubik will draw heavily from Python's view of
everything-as-namespace. Ubik will, at first, be interpreted, but
eventually will be compiled into LLVM IR (or similar). Ubik will not
have a null value or type. Ubik will have no mutable state.

## Ubik in examples

Syntactically, Ubik borrows heavily from Haskell and Hoon. As in Hoon,
punctuation is used in lieu of keywords, allowing any alphabetic symbol
to be a valid user operator. Of course, there are also symbols provided
by the standard library; these, however, need not be imported.

The type system borrows heavily from Haskell, using typeclasses to
genericize methods over types.

    : square-and-add
        ^ Num a => a -> a -> a
        = \x -> + (^ x 2)
    >>> ^ Num a => a -> a -> a = square-and-add

    square-and-add 1 2
    >>> ^ Int = 3

A binding is created using the `:` (pronounced "bind-name") operator.
Function precedence is enforced with parentheses. The `^` operator
(prounced "type") is used to annotate the type on a binding, and
the `=` operator denotes the value of the binding. Bindings are scoped to
the block they are in.

Anonymous functions look similar to Haskell; a `\` (pronounced "lambda";
the literal lambda character is a synonym) begins a function definition
followed by the function's arguments; an arrow operator `->` separates
the function's arguments from its body.

A binding is a block; all lines of a block must be further-indented than
the first line of the block.

    {   : f = square-and-add 7
        map f [1, 4, 9]
    >>> ^ List Int = [50, 53, 58]

The `{` (pronounced "new-block") operator introduces a new scoping
block. This allows for what other languages call `with` or `let`; in
Ubik, to create symbols of limited scope, you create a binding within a
scope block. Note that in this example, the type signature of `f` is
inferred.

All functions are curried, allowing for both partial application and
ease of definitions of functions like square-and-add above. An
equivalent formulation of square-and-add would be:

    : square-and-add
        ^ a -> a -> a | Num a
        = \x, y -> + (^ x 2) y

### Types, classes and ADTs

Types are defined using the `^` operator, similar to attaching a type to
a binding. Types can be simple aliases, like:

    ^ CustomerId = Int

They can also be algebraic data types; algebraic data types are types
that are parameterized by another type. They can be simple,
single-variant types:

    ^ Pair t = Tuple t t

Or more complex multi-variant types:

    ^ Maybe t
        = Just t
        = Nothing

Each of these variants defines a "constructor" which can be used to
create and unpack the type. Creation treats the constructor like a
function, and unpacking relies on pattern matching to get at the values
inside the ADT. For example, using the `Maybe` definition from above:

    : double-or-nothing
        ^ Maybe Float -> Maybe Float
        = \ Just x -> Just (* x 2)
        = \ Nothing -> Nothing

Notice here that we provide more than one definition for the binding,
relying on pattern matching to call the appropriate function.

The last aspect of the type system to mention is type classes. Type
classes allow you to write generic code using a functional equivalent to
what object-oriented programming would call an interface; a type class
enforces a contract that certain functions can be called on the given
type, and it is up to the creator of the type to provide the
implementation of said functions.

Take as a concrete example the desire to write a function that doubles a
value by adding it to itself; we would like to repeat ourselves as
little as possible, but support any type that allows for addition. To do
so, we can write a type class using the `_` (pronounced "class") and `.`
(pronounced "member") operators:

    _ Addable
        . add ^ a -> a -> a | Addable a

This declares a class of types called `Addable`, with a method `add`
that takes two `Addable` and returns another. We now need to implement
our type class on the types we want to be a member of the class using
the "is" and "class" operators:

    _ Int = Addable
        . add = \x, y -> iadd x y

    _ Float = Addable
        . add = \x, y -> fadd x y

    _ Vector3 = Addable
        . add
            = \ Vector x1 y1 z1, Vector x2 y2 z2
                -> Vector (add x1 x2)
                          (add y1 y2)
                          (add z1 z2)

With these definitions, we can then write a nice, generic function to
double an `Addable`:

    : double-me-up
        ^ a -> a | Addable a
        = \x -> (add x x)

We specify the classes of the type variables in the type signature by
separating the type class restrictions from the type signature with a
`|` (pronounced "given that"). At runtime the correct version of
the `add` function is chosen for the method to work correctly.

You can also make type classes that inherit from other classes; for
example, continuing the above example, we could make a `Multipliable`
class whose members were restricted to be `Addable` as well. We do so by
specifying that the type of a `Multipliable` must be `Addable` as well:

    _ Multipliable
        ^ Addable
        . mul ^ a -> a -> a | Multipliable a

With this defintion, for a type to be a member of `Multipliable` it is
required to be a member of `Addable` as well.

### Record types

Ubik has record types, which are structured types containing multiple
typed and named fields. Unlike many other functional languages, Ubik
seeks to avoid polluting the record's enclosing namespace by accessing
child fields using the member operator instead of creating accessor
functions for each.

    ^ Sphere
        . origin ^ Vector
        . radius ^ Float

    : volume
        ^ Sphere -> Float
        = \s -> { : r-cubed = (** (. radius s) 3)
                  * (/ 4 3) (* pi r-cubed)

### Function composition

Functions can be composed using the `,` (pronounced "then") operator:

    ^ Ball
        . sphere ^ Sphere

    : ball-location
        ^ Ball -> Vector
        = \b -> , (. sphere) (. origin)

Note that `(, f g) x` is equivalent to `g (f x)`; when using composition
the functions are specified in the order that they are applied to the
input.

### Compile-time partial application

Compile-time partially applied functions (CTPAs) are functions that have
ultimate control over the evaluation of both their arguments and their
body.  CTPAs can delay the evaluation of their arguments and instead act
upon the abstract syntax tree (AST) of their arguments. CTPAs may also
run at compile time instead of runtime; these may output either a bare
value, in which case the value is substituted at compile time for the
function call, or an AST structure, in which case the returned AST is
substituted for the function call.

As an example, examine the case of the member operator. While some
uses of the member operator are language built-ins, the usage of the
member operator to access a field in a record is implemented in native
Ubik as a CTPA. Record types are all instances of a `Record`
typeclass:

    _ Record
        . .  ^ Symbol! -> a -> AST | Record a

`Symbol` is a type provided by the prelude that represents a symbol in
the AST of the program. Note the `!` (pronounced "eager") operator after
the `Symbol` type; this tells Ubik that this is an unevaluated
expression tree instead of an actual value. It is a compile-time error
to pass this function anything but a symbol as its first argument.

At compile time, a record definition like this:

    ^ Sphere
        . origin ^ Vector
        . radius ^ Float

Expands to this:

    ^ Sphere = Sphere Vector Float
    _ Sphere = Record
        . .
          // case 1:
          = \ Symbol! "origin", Sphere origin _ -> origin
          // case 2:
          = \ Symbol! "radius", Sphere _ radius -> radius
          // case 3:
          = \ Symbol! name, _
              -> error (str/concat "Sphere has no field " name)

Our record type turns into a tuple, and our member operator uses pattern
matching to determine which element of the tuple should be pulled out.
This allows us to access record fields with symbols instead of strings,
and also allows for compile-time checking that we are using the right
member names. The error raised inside case 3 above is raised at
compile-time, and the compiler attaches line number and other
information to the result.

This works because these functions are partially-applied at compile
time, and if the partial application results in all inputs to the
function body being defined, the function is run at compile time. In
case 1 and 2, not all inputs to the function are defined at compile
time, as the value of the second argument is also used. However, in case
3, the value of the second argument is ignored, so we can call the
function during compilation, which results in the error being thrown.

## Input, output and external state

Ubik has to deal with a problem common to all side-effect-free
languages: how does such a system mutate the state of the system it runs
on? Modern computing systems are state modification machines; no real
programming language can ignore this. However, the advantages of
stateless computing are innumerable: repeatability, parallelization and
understandability shouldn't be compromised wherever possible.

To this goal, the Ubik runtime provides a view into state outside the
system that enforces repeatability and a weak form of immutability,
while falling short on the ability to parallelize in some cases. Ubik
programs declare what external state sources they need access to; this
could be a file, a random number generator, or an interface to a
physical device. The runtime provides a monadic view to these state
sources, allowing easy read operations from these devices. The runtime
also enforces the immutability of these state sources; a source of state
will only be loaded once, and repeated requests for the same source of
state are guaranteed to return the same result. For files this is
acheived through on-disk or in-memory caches of the contents of the file
when first loaded; for random number generators this is achieved through
a randomly-chosen initial starting seed for the lifetime of the runtime.

Writing external state to a state sink is harder, especially when
coupled with the desire for immutability. Ubik solves this by only
allowing one handle to each state sink to be created over the lifetime
of the runtime. This is equivalent to saying that a file may only be
opened for writing once over the lifetime of the program. It is also
notable that writing to an existing file does not change the contents of
that file as visible to the runtime itself; the runtime can only read
the contents of the file as they were at the start of the runtime. It
does so by creating an in-memory or on-disk cache of the file's original
contents upon writing, and then destroying that cache on exit.

These primitives use more memory and disk space than normal imperative
I/O primitives normally would. They also provide strong constraints on
mutability and allow us to avoid the trap that many other languages fall
into, in which everything but that which interacts with the outside world is
fully repeatable.

## All Ubik operators

### `:`
Pronounced "bind name", creates a binding with a name.

### `=`
Pronounced "is", associates an object with its value.

### `\` and `λ`
Backslash and lambda are synonyms and are both pronounced "function".
They create a function.

### `^`
Pronounced "type", creates types and annotates bindings with types.

### `.`
Pronounced "member", used for defining and accessing members of
composite structures.

### `->`
Pronounced "becomes", used to mark the beginning of a function's body
and in type expressions to denote function application.

### `{`
Pronounced "new block", starts a new scoping block.

### `|`
Pronounced "given that", used to qualify type expressions or to create
match expressions in pattern-match situations.

### `_`
Pronounced "class", used to create a new type class and provide bindings
between a type and a type class.

### `,`
Pronounced "compose", used to chain actions together.

### `!`
Pronounced "eager", used to indicate that a type represents an AST node
as opposed to a runtime value. In function signatures, this means that
the argument annotated with the eager operator should be applied at
compile time.

### `+`
Pronounced "uses", used to indicate the usage of an external resource (a
library, a state sink/source, etc).

### `=>`
Pronounced "implies", used to express that something is executed when
a condition is true.

### `\>`
Pronounced "opposes", used to express that something is executed when
a condition is not true.

## Ideas to expand
- Data substrate
- Function polymorphism
- Compile-time functions
- Pattern matching
- I/O
