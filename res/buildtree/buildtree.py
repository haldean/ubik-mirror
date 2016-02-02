# buildtree.py: code generator for tree constructors
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


class Stream(object):
    def __init__(self, string):
        self._string = string
        self._pos = 0

    def empty(self):
        return self._pos == len(self._string)

    def take(self):
        if self.empty():
            raise StopIteration("no more elements in stream")
        res = " "
        while res == " " and not self.empty():
            res = self._string[self._pos]
            self._pos += 1
        if res == " ":
            raise StopIteration("no more elements in stream")
        return res

    def take_until(self, char):
        pos = self._string.find(char, self._pos)
        if pos == -1:
            return ""
        res = self._string[self._pos:pos]
        self._pos = pos
        return res.strip()

    def push(self):
        if self._pos == 0:
            raise RuntimeException("can't push onto rewound stream")
        self._pos -= 1
        while self._string[self._pos] == " " and self._pos > 0:
            self._pos -= 1


def build_tree(stream):
    c = stream.take()
    if c != "{":
        raise Exception("expected {, got %s" % c)

    c = stream.take()
    stream.push()
    if c == "{":
        left = build_tree(stream)
    else:
        left = stream.take_until(",")

    # Take the comma
    stream.take()

    c = stream.take()
    stream.push()
    if c == "{":
        right = build_tree(stream)
    else:
        right = stream.take_until("}")

    c = stream.take()
    if c != "}":
        raise Exception("expected }, got %s" % c)

    return dict(left=left, right=right)


def label_tree(root):
    if "label" not in root:
        root["label"] = "root"
    if isinstance(root["left"], dict):
        root["left"]["label"] = root["label"] + "->left.v"
        label_tree(root["left"])
    if isinstance(root["right"], dict):
        root["right"]["label"] = root["label"] + "->right.v"
        label_tree(root["right"])
    return root


def parse(string):
    return label_tree(build_tree(Stream(string)))


def emit(tree, prefix=""):
    if isinstance(tree, basestring):
        print "%s%s" % (prefix, tree)
        return

    def pr(*x):
        if prefix:
            print "%s%s" % (prefix, " ".join(x))
        else:
            print " ".join(x)
    pr(tree["label"])
    pr("left:")
    emit(tree["left"], prefix + "  ")
    pr("right:")
    emit(tree["right"], prefix + "  ")


if __name__ == "__main__":
    emit(parse("{{{ t:left->hello, w:20 }, g:ext_ref->func->graph}, w:0 }"))
