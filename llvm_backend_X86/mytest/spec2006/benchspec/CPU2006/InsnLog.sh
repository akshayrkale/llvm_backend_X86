#!/bin/bash  

java InsnCount > insns.log
echo "Sorting Instructions"
echo "`sort -n insns.log`" > insns.log
echo "`uniq insns.log`" > insns.log

echo "Output file: insns.log"


