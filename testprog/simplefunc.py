from expelc.libexpel import *

c0 = const(uint8(), t(7, 0))
c1 = const(uint8(), t(2, 0))
g = graph(uint8(), 1)
partial = apply(func=g, arg=c0)
res = apply(func=partial, arg=c1, terminal=True)
g0 = [c0, c1, partial, g, res]

arg0 = arg(0)
arg1 = arg(1, terminal=True)
g1 = [arg0, arg1]

encode([g0, g1])
