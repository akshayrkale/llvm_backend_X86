	.file	"addmat.c.bc"
	.text
	.globl	add_su3_matrix
	.align	16, 0x90
	.type	add_su3_matrix,@function
add_su3_matrix:                         # @add_su3_matrix
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
	subq	$16, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 64
.Ltmp12:
	.cfi_offset %rbx, -48
.Ltmp13:
	.cfi_offset %r12, -40
.Ltmp14:
	.cfi_offset %r13, -32
.Ltmp15:
	.cfi_offset %r14, -24
.Ltmp16:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movq	(%rbx), %rdi
	movq	8(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	(%r15), %rsi
	movq	8(%r15), %r13
	movq	%rdx, %r14
	callq	float64_add
	movq	%rax, %r12
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, 8(%r14)
	movq	%r12, (%r14)
	movq	16(%rbx), %rdi
	movq	24(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	16(%r15), %rsi
	movq	24(%r15), %r13
	callq	float64_add
	movq	%rax, %r12
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, 24(%r14)
	movq	%r12, 16(%r14)
	movq	32(%rbx), %rdi
	movq	40(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	32(%r15), %rsi
	movq	40(%r15), %r13
	callq	float64_add
	movq	%rax, %r12
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, 40(%r14)
	movq	%r12, 32(%r14)
	movq	48(%rbx), %rdi
	movq	56(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	48(%r15), %rsi
	movq	56(%r15), %r13
	callq	float64_add
	movq	%rax, %r12
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, 56(%r14)
	movq	%r12, 48(%r14)
	movq	64(%rbx), %rdi
	movq	72(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	64(%r15), %rsi
	movq	72(%r15), %r13
	callq	float64_add
	movq	%rax, %r12
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, 72(%r14)
	movq	%r12, 64(%r14)
	movq	80(%rbx), %rdi
	movq	88(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	80(%r15), %rsi
	movq	88(%r15), %r13
	callq	float64_add
	movq	%rax, %r12
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, 88(%r14)
	movq	%r12, 80(%r14)
	movq	96(%rbx), %rdi
	movq	104(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	96(%r15), %rsi
	movq	104(%r15), %r13
	callq	float64_add
	movq	%rax, %r12
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, 104(%r14)
	movq	%r12, 96(%r14)
	movq	112(%rbx), %rdi
	movq	120(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	112(%r15), %rsi
	movq	120(%r15), %r13
	callq	float64_add
	movq	%rax, %r12
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, 120(%r14)
	movq	%r12, 112(%r14)
	movq	128(%rbx), %rdi
	movq	136(%rbx), %r12
	movq	128(%r15), %rsi
	movq	136(%r15), %r15
	callq	float64_add
	movq	%rax, %rbx
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	float64_add
	movq	%rax, 136(%r14)
	movq	%rbx, 128(%r14)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	add_su3_matrix, .Ltmp17-add_su3_matrix
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
