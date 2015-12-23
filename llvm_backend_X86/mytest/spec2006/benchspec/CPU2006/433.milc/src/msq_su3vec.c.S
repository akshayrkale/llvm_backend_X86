	.file	"msq_su3vec.c.bc"
	.text
	.globl	magsq_su3vec
	.align	16, 0x90
	.type	magsq_su3vec,@function
magsq_su3vec:                           # @magsq_su3vec
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
	movq	%rdi, %r14
	movq	(%r14), %rdi
	movq	8(%r14), %rbx
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	float64_add
	movq	16(%r14), %rdi
	movq	%rax, %r15
	movq	%rdi, %rsi
	callq	float64_mul
	movq	24(%r14), %rdi
	movq	%rax, %rbx
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	32(%r14), %rdi
	movq	%rax, %r15
	movq	%rdi, %rsi
	callq	float64_mul
	movq	40(%r14), %rdi
	movq	%rax, %rbx
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	magsq_su3vec, .Ltmp10-magsq_su3vec
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
