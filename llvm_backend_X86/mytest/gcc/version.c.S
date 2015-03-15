	.file	"version.c.bc"
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"3.2"
	.size	.L.str, 4

	.type	version_string,@object  # @version_string
	.section	.rodata,"a",@progbits
	.globl	version_string
	.align	8
version_string:
	.quad	.L.str
	.size	version_string, 8


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
