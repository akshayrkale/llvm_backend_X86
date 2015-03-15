	.file	"clear_mat.c.bc"
	.text
	.globl	clear_su3mat
	.align	16, 0x90
	.type	clear_su3mat,@function
clear_su3mat:                           # @clear_su3mat
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	xorq	%rsi, %rsi
	movq	$144, %rdx
	callq	memset
	popq	%rax
	retq
.Ltmp2:
	.size	clear_su3mat, .Ltmp2-clear_su3mat
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
