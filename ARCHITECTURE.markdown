Architecture of the Expel Runtime
===

This document attempts to fully explain the implementation and behavior of the
Expel runtime. When the code and this document disagree, the code is incorrect;
on the other hand, don't trust anything in this document.

Runtime representations
---
Expel is based on a simple primitive: unbalanced binary trees of 64-bit values
(called "words"). Everything is expressed in this way, from integers to types
to functions; this homogeneity of data representation allows us to simplify
otherwise-complicated tasks and has a satisfying purity. Note that the
in-memory representation is extremely close to the on-disk representation of
compiled Expel programs.

Each node in the tree has a left value, a right value, and a tag. Each value can
be one of two things: a pointer to another node or a word; the tag tells you how
to interpret the left and right values.

### Tags

Tags are stored in a byte which is the bitwise union of the relevant masks:

    0b00000001 (0x01): left child is a node
    0b00000010 (0x02): left child is a word
    0b00000100 (0x04): right child is a node
    0b00001000 (0x05): right child is a word

Thus, a node whose left value is a node and whose right value is a word would
have the tag `0b00001001`, or `0x09`.

### Types

Types can be base types or derived types. Base types are special in that they
are identified only by an integer constant; derived types (quite predictably)
require quite a bit more information about the type itself.

The base types of Expel are a few flavors of integer, lists and tuples. Each of
these has a base type code and comes with a tree encoding. The full list of base
type codes is:

|Code|Description|
|----|-----------|
| `....word` | unsigned 64-bit integer
| `..sint64` | signed 64-bit integer
| `..uint32` | unsigned 32-bit integer
| `..sint32` | signed 32-bit integer
| `..uint16` | unsigned 16-bit integer
| `..sint16` | signed 16-bit integer
| `..uint08` | unsigned 8-bit integer
| `..sint08` | signed 8-bit integer
| `....list` | homogenous list
| `...tuple` | fixed-length tuple
| `..packed` | packed list
| `....type` | type descriptor

Each type code is actually a word that contains the numeric equivalent of a
8-byte string, with the first letter in the byte with the lowest address. Note
that periods in the above table stand in for space characters (`0x20`).

The tree encoding of integer types is very simple; on its left is a word
containing the type code of the relevant integer type and on its right is the
representation of the value. The value representation is a word in which the
represented value is stored in the lowest bits.

The tree encoding of list types puts a list type descriptor on its left and the
value of the list on its right. The list type descriptor has a constant word
`list` on its left and a node representing the type of the elements of the list
on its right. Each item on the right of the list should have a value on its
left and either a node representing a list or a word set to zero on its right;
finding the zero-word is a sentinel that the end of the list has been reached.

The tree encoding of tuple types is similar to lists; there's a tuple type
descriptor on the left and a tuple value on the right. The values are cons
cells just like lists. However, the type descriptor is more complicated, as
tuple elements are not homogenous. Instead, the type descriptor is a node whose
left is the constant word `tuple` and whose right is a zero-word-terminated
list of type encodings.

The tree encoding of packed list types is provided for better space usage than a
straight list would provide. The left type descriptor for a packed list is a
node whose left is the constant word `packed` and whose right is the type of the
elements of the list; this type is restricted to be a built-in integral type or
`char`. The right value of a packed list is a node whose left is a word
containing the number of items in the list, and whose right is a list of packed
values encoded as cons cells, where each cell has a left of 8 bytes of packed
data, and a right that either points to the next node or a zero-word. If the
data ends off of an 8-byte boundary, the remaining bytes in the left word are
zeroed; it is up to the user of the tree value to respect the length given in
the first word of the value.

Strings are stored as UTF-8 and are represented as packed lists of bytes; the
represented string is the concatenation of all left values in the list. Note
that this means that each individual left value may in fact be poorly-formed
UTF-8, as a multibyte code point may be split across words.

// NEEDS WORK: type descriptor

// IDEA:
maybe there is not an implicit relationship between left and right, and instead
there is a typeof node whose right is a node whose left is a type and whose
right is a value? seems somehow more general.
