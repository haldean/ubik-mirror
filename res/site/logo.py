"""
Draws the Ubik logo into logo.svg.

Requires the svgwrite library to be accessible when running.
Consider this python file the project's first branding guide?
"""

import svgwrite

#  * L -XW-
#  T
#  |   1  3   44     7   9  0
#  |   1  3              9
# YH   1  3     55   8   9
#  |   1  3          8   9  1
#  |   2222   6666   8   9  1
#          -S-

# x-width of letters
xw = 36
# y-height of letters
yh = 100
# letter spacing
s  = 24
# stroke width
sw = 8
# right pad on rectangle
rp = 0
# length of lower dash on I
li = 67
# length of upper dash on I
ui = 14

fg = "#35357f"
bg = "#f5f5f5"

# half stroke width
hsw = sw / 2.
# top start point
t   = 2 * sw + hsw
# left start point
l   = 2 * sw + hsw
# projected dash length (length of k-dash when projected onto x/y axes)
pdl = min(xw, yh) * .4

width = max(
    2 * l + 3 * xw + 3 * s + rp,
    yh + 2 * t - hsw)
t += width - sw - hsw - yh - t

logo = svgwrite.Drawing(
    "logo.svg", size=(width + sw, width + sw), profile="tiny")
line = lambda x1, y1, x2, y2: logo.line(
    (x1, y1), (x2, y2),
    stroke=fg,
    stroke_width=sw)

logo.add(logo.rect(
    (hsw, hsw), (width, width),
    fill=bg,
    stroke=bg,
    stroke_width=sw,
    ))

# U
logo.add(line(l, t - hsw, l, t + yh + hsw))
logo.add(line(l, t + yh, l + xw, t + yh))
logo.add(line(l + xw, t + yh + hsw, l + xw, t + sw))

# B
l += xw + s
logo.add(line(l, t - hsw, l, t + yh + hsw))
logo.add(line(l + xw / 2, t + yh / 2, l + xw, t + yh / 2))
logo.add(line(l + xw, t + yh / 2 - hsw, l + xw, t + yh + hsw))
logo.add(line(l, t + yh, l + xw, t + yh))

# I
l += xw + s
logo.add(line(l, t - hsw, l, t + ui))
logo.add(line(l, t + yh - li, l, t + yh + hsw))

# K
l += s + hsw
logo.add(line(l, t - hsw, l, t + yh + hsw))
logo.add(line(l + xw, t + yh / 2, l + xw, t + yh + hsw))
logo.add(line(l + xw + hsw, t + yh / 2, l + xw / 2, t + yh / 2))
logo.add(line(l + xw - pdl, t + pdl, l + xw, t))

logo.save()
