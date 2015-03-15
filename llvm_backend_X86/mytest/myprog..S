	.file	"myprog.bc"
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	movabsq	$.L.str, %rdi
	movq	$285, %rsi              # imm = 0x11D
	xorq	%rax, %rax
	callq	printf
	xorq	%rax, %rax
	popq	%rdx
	retq
.Ltmp2:
	.size	main, .Ltmp2-main
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"sum: %d\n"
	.size	.L.str, 9


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
