	.file	"su3mat_copy.c.bc"
	.text
	.globl	su3mat_copy
	.align	16, 0x90
	.type	su3mat_copy,@function
su3mat_copy:                            # @su3mat_copy
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	movq	8(%rdi), %rcx
	movq	%rcx, 8(%rsi)
	movq	%rax, (%rsi)
	movq	16(%rdi), %rax
	movq	24(%rdi), %rcx
	movq	%rcx, 24(%rsi)
	movq	%rax, 16(%rsi)
	movq	32(%rdi), %rax
	movq	40(%rdi), %rcx
	movq	%rcx, 40(%rsi)
	movq	%rax, 32(%rsi)
	movq	48(%rdi), %rax
	movq	56(%rdi), %rcx
	movq	%rcx, 56(%rsi)
	movq	%rax, 48(%rsi)
	movq	64(%rdi), %rax
	movq	72(%rdi), %rcx
	movq	%rcx, 72(%rsi)
	movq	%rax, 64(%rsi)
	movq	80(%rdi), %rax
	movq	88(%rdi), %rcx
	movq	%rcx, 88(%rsi)
	movq	%rax, 80(%rsi)
	movq	96(%rdi), %rax
	movq	104(%rdi), %rcx
	movq	%rcx, 104(%rsi)
	movq	%rax, 96(%rsi)
	movq	112(%rdi), %rax
	movq	120(%rdi), %rcx
	movq	%rcx, 120(%rsi)
	movq	%rax, 112(%rsi)
	movq	128(%rdi), %rax
	movq	136(%rdi), %rcx
	movq	%rcx, 136(%rsi)
	movq	%rax, 128(%rsi)
	retq
.Ltmp0:
	.size	su3mat_copy, .Ltmp0-su3mat_copy
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
