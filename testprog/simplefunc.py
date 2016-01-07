from expelc.libexpel import *

c0 = const(uint8(), t(7, 0))
c1 = const(uint8(), t(2, 0))
c2 = const(uint8(), t(4, 0))
c3 = const(uint8(), t(8, 0))

g = graph(uint8(), 1)
partial = apply(func=g, arg=c0)
res1 = apply(func=partial, arg=c1)
res2 = apply(func=partial, arg=c0)
pres = apply(func=g, arg=c2)

a1 = apply(func=partial, arg=c1)
a3 = apply(func=pres, arg=c3)

a4 = apply(func=g, arg=a1)

g0 = [
    #0
    c0,
    #1
    c1,
    #2
    c2,
    #3
    c3,
    #4
    g,
    #5
    partial,
    #6
    res1,
    #7
    res2,
    #8
    pres,
    #9
    a1,
    #10
    a3,
    #11
    a4,
    #12: should be 8
    apply(func=a4, arg=a3, terminal=True)
]

arg0 = arg(0)
arg1 = arg(1, terminal=True)
g1 = [arg0, arg1]

encode([g0, g1])
