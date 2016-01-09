# Python DSL for encoding Expel DAGCs.

import struct
import sys

def t(l, r):
    return (l, r)

def uint8():
    return t(pack("uint08"), 0)

def word():
    return t(pack("word"), 0)

def boolean():
    return t(pack("bool"), 0)

def const(typ, val, terminal=False):
    return dict(
        type="const",
        ctype=typ,
        cvalue=val,
        subtype=pack("cvalue"),
        is_term=terminal,
    )

def const_word(val, terminal=False):
    return const(word(), t(val, 0))

def graph_ref(typ, graph_num, terminal=False):
    return dict(
        type="const",
        ctype=typ,
        cvalue=graph_num,
        subtype=pack("cgraph"),
        is_term=terminal,
    )

def load(uri, dep=None, terminal=False):
    return dict(
        type="load",
        uri=uri,
        dep=dep,
        is_term=terminal,
    )

def store(uri, val, terminal=False):
    return dict(
        type="store",
        uri=uri,
        value=val,
        is_term=terminal,
    )

def apply(func, arg, terminal=False):
    return dict(
        type="apply",
        func=func,
        arg=arg,
        is_term=terminal,
    )

def arg(arg_num, type, terminal=False):
    return dict(
        type="input",
        arg_num=arg_num,
        req_type=type,
        is_term=terminal,
    )

def cond(condition, true, false, terminal=False):
    return dict(
        type="cond",
        condition=condition,
        true=true,
        false=false,
        is_term=terminal,
    )

def pack(s):
    if len(s) > 8:
        raise ValueError("can only pack strings of length >= 8")
    if len(s) < 8:
        s = s.rjust(8)
    return struct.pack("8s", s)

def uri(name, version=0, scope=pack("userdef")):
    return t(version, t(scope, pack_string(name)))

def pack_string(s):
    bits = []
    bytes = s.encode("utf-8")
    while s:
        bits.append(s[:8])
        s = s[8:]
    tree = 0
    while bits:
        tree = t(bits[-1].ljust(8, '\x00'), tree)
        bits = bits[:-1]
    return t(len(bytes), tree)

def pack_tree(t):
    tag = 0x10

    if isinstance(t[0], tuple):
        tag |= 0x01
        left = pack_tree(t[0])
    else:
        tag |= 0x02
        if isinstance(t[0], basestring):
            left = t[0]
        else:
            left = struct.pack(">Q", t[0])

    if isinstance(t[1], tuple):
        tag |= 0x04
        right = pack_tree(t[1])
    else:
        tag |= 0x08
        if isinstance(t[1], basestring):
            right = t[1]
        else:
            right = struct.pack(">Q", t[1])


    return struct.pack(">B", tag) + left + right

def graph(nodes, result_idx):
    return dict(
        nodes=nodes,
        result_idx=result_idx,
    )

def encode(graphs, expect=None):
    if len(sys.argv) < 2:
        print "missing output file"
        return

    with open(sys.argv[1], "wb") as f:
        f.write("expl")
        f.write(struct.pack(">I", 1))
        f.write(struct.pack(">Q", len(graphs)))

        for graph in graphs:
            nodes = graph["nodes"]
            result_idx = graph["result_idx"]
            if result_idx < 0:
                result_idx += len(nodes)

            f.write(struct.pack(">Q", len(nodes)))
            f.write(struct.pack(">Q", result_idx))
            for i, node in enumerate(nodes):
                node["idx"] = i

            for node in nodes:
                node_type = node["type"]
                f.write(struct.pack("8s", pack(node_type)))
                f.write(struct.pack(">Bxxx", 1 if node["is_term"] else 0))

                if node_type == "const":
                    f.write(node["subtype"])
                    f.write(pack_tree(node["ctype"]))
                    if node["subtype"] == "  cgraph":
                        f.write(struct.pack(">Q", node["cvalue"]))
                    else:
                        f.write(pack_tree(node["cvalue"]))
                elif node_type == "load":
                    if node["dep"] is None:
                        f.write(struct.pack(">Q", 0xFFFFFFFFFFFFFFFF))
                    else:
                        f.write(struct.pack(">Q", node["dep"]["idx"]))
                    f.write(pack_tree(node["uri"]))
                elif node_type == "store":
                    f.write(struct.pack(">Q", node["value"]["idx"]))
                    f.write(pack_tree(node["uri"]))
                elif node_type == "apply":
                    f.write(struct.pack(">Q", node["func"]["idx"]))
                    f.write(struct.pack(">Q", node["arg"]["idx"]))
                elif node_type == "input":
                    f.write(struct.pack(">Q", node["arg_num"]))
                    f.write(pack_tree(node["req_type"]))
                elif node_type == "cond":
                    f.write(struct.pack(">Q", node["condition"]["idx"]))
                    f.write(struct.pack(">Q", node["true"]["idx"]))
                    f.write(struct.pack(">Q", node["false"]["idx"]))
                else:
                    raise NotImplementedError(
                        "node type %s not supported" % node_type)
        if expect is not None:
            f.write(pack_tree(expect))
