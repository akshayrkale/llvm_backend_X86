	.file	"s_m_mat.c.bc"
	.text
	.globl	scalar_mult_su3_matrix
	.align	16, 0x90
	.type	scalar_mult_su3_matrix,@function
scalar_mult_su3_matrix:                 # @scalar_mult_su3_matrix
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp8:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp9:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp10:
	.cfi_def_cfa_offset 48
.Ltmp11:
	.cfi_offset %rbx, -48
.Ltmp12:
	.cfi_offset %r12, -40
.Ltmp13:
	.cfi_offset %r13, -32
.Ltmp14:
	.cfi_offset %r14, -24
.Ltmp15:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rax
	movq	8(%rbx), %r12
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, 8(%r14)
	movq	%r13, (%r14)
	movq	16(%rbx), %rsi
	movq	24(%rbx), %r12
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, 24(%r14)
	movq	%r13, 16(%r14)
	movq	32(%rbx), %rsi
	movq	40(%rbx), %r12
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, 40(%r14)
	movq	%r13, 32(%r14)
	movq	48(%rbx), %rsi
	movq	56(%rbx), %r12
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, 56(%r14)
	movq	%r13, 48(%r14)
	movq	64(%rbx), %rsi
	movq	72(%rbx), %r12
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, 72(%r14)
	movq	%r13, 64(%r14)
	movq	80(%rbx), %rsi
	movq	88(%rbx), %r12
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, 88(%r14)
	movq	%r13, 80(%r14)
	movq	96(%rbx), %rsi
	movq	104(%rbx), %r12
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, 104(%r14)
	movq	%r13, 96(%r14)
	movq	112(%rbx), %rsi
	movq	120(%rbx), %r12
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, 120(%r14)
	movq	%r13, 112(%r14)
	movq	128(%rbx), %rsi
	movq	136(%rbx), %r12
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, 136(%r14)
	movq	%rbx, 128(%r14)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp16:
	.size	scalar_mult_su3_matrix, .Ltmp16-scalar_mult_su3_matrix
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
