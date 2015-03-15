	.file	"clearvec.c.bc"
	.text
	.globl	clearvec
	.align	16, 0x90
	.type	clearvec,@function
clearvec:                               # @clearvec
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$0, 40(%rdi)
	movq	$0, 32(%rdi)
	movq	$0, 24(%rdi)
	movq	$0, 16(%rdi)
	movq	$0, 8(%rdi)
	movq	$0, (%rdi)
	retq
.Ltmp0:
	.size	clearvec, .Ltmp0-clearvec
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
