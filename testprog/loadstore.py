from expelc.libexpel import *

muri = uri(name="super cool lambda biz", scope=pack("userdef"))

g_ref = graph(uint8(), 1)
s = store(muri, g_ref)
l = load(muri, dep=s, terminal=True)
g0 = [g_ref, s, l]

c0 = const(uint8(), t(7, 10), terminal=True)
g1 = [c0]

encode([g0, g1])
