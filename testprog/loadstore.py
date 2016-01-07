from expelc.libexpel import *

c0 = const(uint8(), t(7, 10))
c1 = const(uint8(), t(2, 30))
s = store("wheee", c0)
l = load("wheee", dep=s, terminal=True)

encode([c1, l, c0, s])
