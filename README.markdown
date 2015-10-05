Expel
====

Expel will eventually be a programming language. Expel aims to be a
programming language that approximates the combination of the.
conciseness of Haskell, the simplicity of Lisp, the productivity of
Python and the deployability of Go. Expel is what I wish I could write
everything in, but first I have to write Expel.

Expel will have strict, static typing with full type inference. Expel
will be homoiconic, and provide language-level features that allow users
to control order-of-evaluation; in doing so, it will obviate the need
for macros. Expel will draw heavily from Python's view of
everything-as-namespace. Expel will, at first, be interpreted, but
eventually will be compiled into LLVM IR (or similar). Expel will not
have a null value or type. Expel will have no mutable state.

Expel in examples
---

Syntactically, Expel borrows heavily from Haskell and Hoon. As in Hoon,
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
(prounced "with-type") is used to annotate the type on a binding, and
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
Expel, to create symbols of limited scope, you create a binding within a
scope block. Note that in this example, the type signature of `f` is
inferred.

All functions are curried, allowing for both partial application and
ease of definitions of functions like square-and-add above. An
equivalent formulation of square-and-add would be:

    : square-and-add
        ^ Num a => a -> a -> a
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
        . add ^ Addable -> Addable -> Addable

This declares a class of types called `Addable`, with a method `add`
that takes two `Addable` and returns another. We now need to implement
our type class on the types we want to be a member of the class using
the arrow and class operators:

    _ Int -> Addable
        : add = \x, y -> iadd x y

    _ Float -> Addable
        : add = \x, y -> fadd x y

    _ Vector3 -> Addable
        : add
            = \ Vector x1 y1 z1, Vector x2 y2 z2
                -> Vector (add x1 x2) (add y1 y2) (add z1 z2)

With these definitions, we can then write a nice, generic function to
double an `Addable`:

    : double-me-up
        ^ a -> a | Addable a
        = \x -> (add x x)

We specify the classes of the type variables in the type signature by
separating the type class restrictions from the type signature with a
`|` (pronounced "given that"). At compile-time the correct version of
the `double-me-up` is generated based on the types of the argument, in a
way not dissimilar from C++ templates.
