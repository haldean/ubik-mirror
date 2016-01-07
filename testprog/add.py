from expelc.libexpel import *

c0 = const(uint8(), t(7, 0))
c1 = const(uint8(), t(2, 0))
l = load(uri=uri(name=pack("add"), scope=pack("native")))
partial = apply(func=l, arg=c0)
res = apply(func=partial, arg=c1, terminal=True)

encode([c1, l, c0, partial, res])
