Everything is the wrong way around
==================================

In many languages, you need to do everything in the **right order**.
That means things like starting your file with a package declaration, or
the imports. Like in Python:

.. code:: python
    import re
    import os

Or in Java:

.. code:: java
    package com.company.department.team.project.subproject.impl.beans;
    import com.google.java.everything;

In still other languages, you have to define all of the stuff you use
before you use it. Take C for example:

.. code:: c
    // this would fail!
    // int y = x + 5;

    int x = 4;

    // but this one doesn't:
    int y = x + 5;

But in Ubik, *not so!* In fact, you can define stuff well before you
specify where it comes from. Without any definitions at all, you can do
something like::

    ! emit (humanize y)

But we haven't even defined ``y`` yet! We can then go on to define ``y``
(but check it out, the value of ``y`` depends on something we define
*after* it)::

    : y ^ Word = uadd x 5
    : x ^ Word = 4

In fact, you can do that even before you specify what package you're
defining::

    . ubik-tests/literate-out-of-order

In fact, this file is a runnable Ubik program, and those things should
execute just fine. When you run it, the number "9" should be printed to
your terminal. How nuts is that!
