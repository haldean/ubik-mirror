
f0 is a normal-old number, defined by applying this super-weird anonymous
function::

    : f0 ^ Number =
    (\ a0 -> {

f1, an inner function, takes a single argument, and if that argument is zero,
it returns zero. If the argument is nonzero, it calls f2 with an argument of
zero::

        : f1 = \ a1 -> {

f2, an inner-inner-function, takes a single argument and returns the result of
calling its enclosing function, f1, with that argument::
            : f2 = \ a2 -> {
                : x = f1 a2
                ! x
            }

Now we're back to the body of f1. If a1 is zero, it returns zero. If a1 isn't
zero, it's going to call f2 with zero. f2 then, in turn, calls f1 with zero,
which returns zero. So this function always returns zero::

    # comment for proper code formatting with RST :/

            ! ? {
                . eq a1 0 => 0
                .         => f2 0
            }
        }
        ! f1 1
    }) 0

    ! emit (humanize f0)
    ~ ubik-tests/recursion-hard
