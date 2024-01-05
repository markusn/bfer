bfer
==========
[![Build Status](https://travis-ci.org/markusn/bfer.png?branch=master)](https://travis-ci.org/markusn/bfer)
[![Coverage Status](https://coveralls.io/repos/markusn/bfer/badge.png?branch=master)](https://coveralls.io/r/markusn/bfer?branch=master)

Brainfuck to LLVM IR compiler written in Erlang.

## Building

To build as a library to be included in another Erlang project:

```bash
rebar compile
```

To build as a standalone escript:

```bash
rebar compile escriptize
```

## Usage

When using as a library in another Erlang module the main entry point is `bfer_lib:compile/2` which
takes Brainfuck code as a string and returns LLVM IR as a string.

Used as a standalone escript `bfer` can generate native code if llvm and some kind of cc is installed:

```bash
bfer [-o] INPUT.bf OUTPUT
```

Specifying -o "optimizes" the generated LLVM IR.

## Author
Markus Ekholm (markus at botten dot org).

## License
3-clause BSD. For details see `COPYING`.

