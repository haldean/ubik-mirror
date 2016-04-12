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

import re


class ParseError(Exception):
    pass


class Stream(object):
    def __init__(self, string):
        self._string = re.sub(r"[\s]", "", string)
        self._pos = 0

    def empty(self):
        return self._pos == len(self._string)

    def take(self):
        if self.empty():
            raise StopIteration("no more elements in stream")
        res = self._string[self._pos]
        self._pos += 1
        return res

    def take_until(self, char):
        if self.empty():
            raise StopIteration("no more elements in stream")
        pos = self._string.find(char, self._pos)
        if pos < 0:
            pos = len(self._string)
        res = self._string[self._pos:pos]
        self._pos = pos
        return res

    def push(self):
        if self._pos == 0:
            raise RuntimeException("can't push onto rewound stream")
        self._pos -= 1


def build_tree(stream):
    c = stream.take()
    if c != "{":
        raise ParseError("expected {, got %s" % c)

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
        raise ParseError("expected }, got %s" % c)

    return dict(left=left, right=right)


def read_opts(string):
    opt_pairs = string.split(";")[1:]
    opt_pairs = map(str.strip, opt_pairs)
    opt_pairs = filter(bool, opt_pairs)
    opt_pairs = map(lambda p: p.split(":", 1), opt_pairs)
    opt_pairs = map(lambda p: (p[0].strip().replace(" ", "_"), p[1].strip()), opt_pairs)
    options = dict(opt_pairs)
    options.setdefault("root", "root")
    options.setdefault("on_error", "return err")
    return options


def label_tree(root, options):
    if "label" not in root:
        root["label"] = options["root"]
    if isinstance(root["left"], dict):
        root["left"]["label"] = root["label"] + "->left.t"
        label_tree(root["left"], options)
    if isinstance(root["right"], dict):
        root["right"]["label"] = root["label"] + "->right.t"
        label_tree(root["right"], options)
    return root


def type_to_tag_suffix(typ):
    if typ == "t":
        return "NODE"
    if typ == "g":
        return "GRAPH"
    if typ in ("w", "f"):
        return "WORD"
    raise ParseError("unknown value type %s" % typ)


def emit_c(tree, options):
    res = """
err = ubik_value_new(&{root});
if (err != OK)
        {do_on_err};
""".format(root=tree["label"], do_on_err=options["on_error"])

    left = tree["left"]
    if isinstance(left, dict):
        left_res = emit_c(left, options)
        left_tag = "TAG_LEFT_NODE";
    else:
        left_type, left_assign = left.split(":", 1)
        left_res = "{root}->left.{left_type} = {left_assign};".format(
            root=tree["label"], **locals())
        left_tag = "TAG_LEFT_" + type_to_tag_suffix(left_type)
    res += "\n" + left_res

    right = tree["right"]
    if isinstance(right, dict):
        right_res = emit_c(right, options)
        right_tag = "TAG_RIGHT_NODE"
    else:
        right_type, right_assign = right.split(":", 1)
        right_res = "{root}->right.{right_type} = {right_assign};".format(
            root=tree["label"], **locals())
        right_tag = "TAG_RIGHT_" + type_to_tag_suffix(right_type)
    res += "\n" + right_res

    res += "\n{root}->tag = TAG_VALUE | {left_tag} | {right_tag};".format(
        root=tree["label"], left_tag=left_tag, right_tag=right_tag)
    return res


def parse(string):
    options = read_opts(string)
    tree = label_tree(build_tree(Stream(string)), options)
    return tree, options


def parse_file(in_fname, out_fname):
    with open(in_fname) as in_f:
        contents = in_f.read()
    tree, options = parse(contents)
    res = emit_c(tree, options)
    with open(out_fname, "w") as out_f:
        out_f.write(res)


if __name__ == "__main__":
    import sys
    try:
        parse_file(sys.argv[1], sys.argv[2])
    except ParseError as e:
        print(e)
        sys.exit(1)
