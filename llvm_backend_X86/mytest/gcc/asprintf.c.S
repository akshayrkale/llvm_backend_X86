	.file	"asprintf.c.bc"
	.text
	.globl	asprintf
	.align	16, 0x90
	.type	asprintf,@function
asprintf:                               # @asprintf
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$72, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 80
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	80(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	leaq	48(%rsp), %rdx
	callq	vasprintf
	addq	$72, %rsp
	retq
.Ltmp2:
	.size	asprintf, .Ltmp2-asprintf
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
