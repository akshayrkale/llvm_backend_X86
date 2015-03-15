	.file	"cppdefault.c.bc"
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"NONE/include"
	.size	.L.str, 13

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"/usr/include"
	.size	.L.str1, 13

	.type	cpp_include_defaults,@object # @cpp_include_defaults
	.section	.rodata,"a",@progbits
	.globl	cpp_include_defaults
	.align	16
cpp_include_defaults:
	.quad	.L.str
	.quad	0
	.long	0                       # 0x0
	.long	1                       # 0x1
	.quad	.L.str1
	.quad	0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	24
	.size	cpp_include_defaults, 72

	.type	cpp_GCC_INCLUDE_DIR,@object # @cpp_GCC_INCLUDE_DIR
	.globl	cpp_GCC_INCLUDE_DIR
cpp_GCC_INCLUDE_DIR:
	.zero	1
	.size	cpp_GCC_INCLUDE_DIR, 1

	.type	cpp_GCC_INCLUDE_DIR_len,@object # @cpp_GCC_INCLUDE_DIR_len
	.globl	cpp_GCC_INCLUDE_DIR_len
	.align	8
cpp_GCC_INCLUDE_DIR_len:
	.quad	0                       # 0x0
	.size	cpp_GCC_INCLUDE_DIR_len, 8


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
