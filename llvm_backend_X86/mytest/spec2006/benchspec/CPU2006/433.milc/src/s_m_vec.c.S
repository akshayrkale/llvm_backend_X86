	.file	"s_m_vec.c.bc"
	.text
	.globl	scalar_mult_su3_vector
	.align	16, 0x90
	.type	scalar_mult_su3_vector,@function
scalar_mult_su3_vector:                 # @scalar_mult_su3_vector
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
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, 40(%r14)
	movq	%rbx, 32(%r14)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp16:
	.size	scalar_mult_su3_vector, .Ltmp16-scalar_mult_su3_vector
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
