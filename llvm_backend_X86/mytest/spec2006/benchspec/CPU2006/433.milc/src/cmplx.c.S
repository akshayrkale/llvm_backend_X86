	.file	"cmplx.c.bc"
	.text
	.globl	cmplx
	.align	16, 0x90
	.type	cmplx,@function
cmplx:                                  # @cmplx
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, %rax
	movq	%rsi, %rdx
	retq
.Ltmp0:
	.size	cmplx, .Ltmp0-cmplx
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
