#!/bin/bash -x

NAME=bzip2

#PATH=~/llvm/built/Debug+Asserts/bin:$PATH

if [ "$1" = "clean" ]; then
echo "Cleaning *.o *.bc *.s"
rm -rf *.c.o *.c.bc *.c.S
exit 1;
fi
LIBC_PATH=../musl-1.0.4
LIBS=`echo $LIBC_PATH/lib/{crt1.o,libc.a,softfloat.o}`
TOOLS_PATH=~/llvm523/llvm_backend_X86/build_523/Debug+Asserts/bin
for c in *.c; do
     if [ -f $c.o ]
     then
         continue
     fi
    #echo "[Running] gcc $c"
    #gcc -nostdinc -c -o $c.o -I$LIBC_PATH/include $c
    echo "[Running] clang $c"
    $TOOLS_PATH/clang -Wno-main-return-type -emit-llvm -DENABLE_SCORE -DENABLE_PREVIEW -DENABLE_HIGH_SCORE -O3 -S -c -o $c.bc -nobuiltininc -isystem $LIBC_PATH/include $c -isystem .  -isystem */ -DSPEC_CPU -msoft-float
    echo "[Running] llc $c.bc"
    $TOOLS_PATH/llc -soft-float -march=cse523 -O3 $c.bc -o $c.S
    #~/llvm/build/Debug+Asserts/bin/llc -soft-float -O3 $c.bc -o $c.S
    echo "[Running] as $c.S"
    as -o $c.o $c.S
done
for c in */*.c; do
     if [ -f $c.o ]
     then
         continue
     fi
    #echo "[Running] gcc $c"
    #gcc -nostdinc -c -o $c.o -I$LIBC_PATH/include $c
    echo "[Running] clang $c"
    $TOOLS_PATH/clang -Wno-main-return-type -emit-llvm -DENABLE_SCORE -DENABLE_PREVIEW -DENABLE_HIGH_SCORE -O3 -S -c -o $c.bc -nobuiltininc -isystem $LIBC_PATH/include $c -isystem .  -isystem */
    echo "[Running] llc $c.bc"
    $TOOLS_PATH/llc -soft-float -march=cse523 -O3 $c.bc -o $c.S
    #~/llvm/build/Debug+Asserts/bin/llc -soft-float -O3 $c.bc -o $c.S
    echo "[Running] as $c.S"
    as -o $c.o $c.S
done

echo "[Running] ld $NAME"
ld -static -o $NAME *.o  $LIBC_PATH/lib/crt1.o $LIBC_PATH/lib/libc.a $LIBC_PATH/lib/softfloat.o

