	.file	"realtr.c.bc"
	.text
	.globl	realtrace_su3
	.align	16, 0x90
	.type	realtrace_su3,@function
realtrace_su3:                          # @realtrace_su3
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
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	(%rbx), %rdi
	movq	8(%rbx), %r15
	movq	(%r14), %rsi
	movq	8(%r14), %r12
	callq	float64_mul
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_add
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	float64_add
	movq	16(%rbx), %rdi
	movq	16(%r14), %rsi
	movq	%rax, %r15
	callq	float64_mul
	movq	24(%rbx), %rdi
	movq	24(%r14), %rsi
	movq	%rax, %r12
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	32(%rbx), %rdi
	movq	32(%r14), %rsi
	movq	%rax, %r15
	callq	float64_mul
	movq	40(%rbx), %rdi
	movq	40(%r14), %rsi
	movq	%rax, %r12
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	48(%rbx), %rdi
	movq	48(%r14), %rsi
	movq	%rax, %r15
	callq	float64_mul
	movq	56(%rbx), %rdi
	movq	56(%r14), %rsi
	movq	%rax, %r12
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	64(%rbx), %rdi
	movq	64(%r14), %rsi
	movq	%rax, %r15
	callq	float64_mul
	movq	72(%rbx), %rdi
	movq	72(%r14), %rsi
	movq	%rax, %r12
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	80(%rbx), %rdi
	movq	80(%r14), %rsi
	movq	%rax, %r15
	callq	float64_mul
	movq	88(%rbx), %rdi
	movq	88(%r14), %rsi
	movq	%rax, %r12
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	96(%rbx), %rdi
	movq	96(%r14), %rsi
	movq	%rax, %r15
	callq	float64_mul
	movq	104(%rbx), %rdi
	movq	104(%r14), %rsi
	movq	%rax, %r12
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	112(%rbx), %rdi
	movq	112(%r14), %rsi
	movq	%rax, %r15
	callq	float64_mul
	movq	120(%rbx), %rdi
	movq	120(%r14), %rsi
	movq	%rax, %r12
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	128(%rbx), %rdi
	movq	128(%r14), %rsi
	movq	%rax, %r15
	callq	float64_mul
	movq	136(%rbx), %rdi
	movq	136(%r14), %rsi
	movq	%rax, %rbx
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp16:
	.size	realtrace_su3, .Ltmp16-realtrace_su3
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
