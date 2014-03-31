#!/bin/bash

for f in output/*.s
do
  nasm -O0 -f elf -g -F dwarf $f
done

ld -o main output/*.o

./main

echo "Returned: $?"
