===================================
On the literacy of Ubik programmers
===================================
A treatise
-----------------------------------

Ubik programmers are all disciplined people. Because of that, they
always start their modules with the appropriate preamble::

    . ubik-literate

With the preamble written, they will often define a simple function like
this one.

::

    : always-hello
        ^ String -> String
        = \x -> "hello"

*Wonderful*. This is equivalent, of course, to the following Python
program:

.. code:: python

    def always_hello(x):
        return "hello"

Ubik programmers also enjoy writing immediate statements on their
modules as a **tiny unit test**:

.. code:: ubik

    ! emit (always-hello "goodbye")

And that's that!
