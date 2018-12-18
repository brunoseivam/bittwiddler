#!/bin/sh

# Path to the LLVM interpreter
LLI="lli"

# Path to the LLVM compiler
LLC="llc"

# Path to the C compiler
CC="cc"

# Path to the bittwiddler compiler.  Usually "./bittwiddler.native"
BITTWIDDLER="./bittwiddler.native"

# Set time limit for all operations
ulimit -t 30

$BITTWIDDLER $1 | $LLC -relocation-model=pic > $1.s
$CC -o $1.exe $1.s runtime.o

rm $1.s
