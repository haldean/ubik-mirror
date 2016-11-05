# logo.py: draws the Ubik logo
# Copyright (C) 2016, Haldean Brown
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


"""
Draws the Ubik logo into logo.svg.

Requires the svgwrite library to be accessible when running.
Consider this python file the project's first branding guide?
"""

import svgwrite
import sys

#  * L -XW-
#  T
#  |   1      4      8   0  3
#  |   1  3   4          0
# YH   1  3   4 55   9   0 22
#  |   1  3   4  6   9   0  1
#  |   2222   7777   9   0  1
#          -S-

logotype = sys.argv[1] if sys.argv[1:] else "logo"

dark  = "#354f7f"
light = "#f5f5f5"

if logotype == "logo":
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
    fg = dark
    bg = light
elif logotype == "favicon":
    # x-width of letters
    xw = 40
    # y-height of letters
    yh = 200
    # letter spacing
    s  = 24
    # stroke width
    sw = 16
    # right pad on rectangle
    rp = 0
    # length of lower dash on I
    li = 134
    # length of upper dash on I
    ui = 28
    fg = light
    bg = dark
else:
    raise ValueError("unknown logo type %s" % logotype)


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
    logotype + ".svg", size=(width + sw, width + sw), profile="tiny")
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
logo.add(line(l + xw, t + yh + hsw, l + xw, t + ui))

if logotype != "favicon":
    # B
    l += xw + s
    # logo.add(line(l, t - hsw, l, t + yh + hsw))
    logo.add(line(l + xw / 2, t + yh / 2, l + xw, t + yh / 2))
    logo.add(line(l + xw, t + yh / 2 - hsw, l + xw, t + yh + hsw))
    # logo.add(line(l, t + yh, l + xw, t + yh))

    # I
    l += xw + s
    logo.add(line(l, t - hsw, l, t + ui))
    # logo.add(line(l, t + yh - li, l, t + yh + hsw))

    # K
    l += s + hsw
    # logo.add(line(l, t - hsw, l, t + yh + hsw))
    logo.add(line(l + xw, t + yh / 2, l + xw, t + yh + hsw))
    logo.add(line(l + xw + hsw, t + yh / 2, l + xw / 2, t + yh / 2))
    logo.add(line(l + xw - pdl, t + pdl, l + xw, t))

logo.save()
