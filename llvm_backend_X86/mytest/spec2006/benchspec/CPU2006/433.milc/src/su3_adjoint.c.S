	.file	"su3_adjoint.c.bc"
	.text
	.globl	su3_adjoint
	.align	16, 0x90
	.type	su3_adjoint,@function
su3_adjoint:                            # @su3_adjoint
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6:
	.cfi_def_cfa_offset 32
.Ltmp7:
	.cfi_offset %rbx, -32
.Ltmp8:
	.cfi_offset %r14, -24
.Ltmp9:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rax
	movq	%rsi, %r14
	movq	%rax, (%r14)
	movq	8(%rbx), %rsi
	movabsq	$-9223372036854775808, %r15 # imm = 0x8000000000000000
	movq	%r15, %rdi
	callq	float64_sub
	movq	%rax, 8(%r14)
	movq	48(%rbx), %rax
	movq	%rax, 16(%r14)
	movq	56(%rbx), %rsi
	movq	%r15, %rdi
	callq	float64_sub
	movq	%rax, 24(%r14)
	movq	96(%rbx), %rax
	movq	%rax, 32(%r14)
	movq	104(%rbx), %rsi
	movq	%r15, %rdi
	callq	float64_sub
	movq	%rax, 40(%r14)
	movq	16(%rbx), %rax
	movq	%rax, 48(%r14)
	movq	24(%rbx), %rsi
	movq	%r15, %rdi
	callq	float64_sub
	movq	%rax, 56(%r14)
	movq	64(%rbx), %rax
	movq	%rax, 64(%r14)
	movq	72(%rbx), %rsi
	movq	%r15, %rdi
	callq	float64_sub
	movq	%rax, 72(%r14)
	movq	112(%rbx), %rax
	movq	%rax, 80(%r14)
	movq	120(%rbx), %rsi
	movq	%r15, %rdi
	callq	float64_sub
	movq	%rax, 88(%r14)
	movq	32(%rbx), %rax
	movq	%rax, 96(%r14)
	movq	40(%rbx), %rsi
	movq	%r15, %rdi
	callq	float64_sub
	movq	%rax, 104(%r14)
	movq	80(%rbx), %rax
	movq	%rax, 112(%r14)
	movq	88(%rbx), %rsi
	movq	%r15, %rdi
	callq	float64_sub
	movq	%rax, 120(%r14)
	movq	128(%rbx), %rax
	movq	%rax, 128(%r14)
	movq	136(%rbx), %rsi
	movq	%r15, %rdi
	callq	float64_sub
	movq	%rax, 136(%r14)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	su3_adjoint, .Ltmp10-su3_adjoint
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
