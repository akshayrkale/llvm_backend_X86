	.file	"addvec.c.bc"
	.text
	.globl	add_su3_vector
	.align	16, 0x90
	.type	add_su3_vector,@function
add_su3_vector:                         # @add_su3_vector
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
	movq	40(%rbx), %r12
	movq	32(%r15), %rsi
	movq	40(%r15), %r15
	callq	float64_add
	movq	%rax, %rbx
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	float64_add
	movq	%rax, 40(%r14)
	movq	%rbx, 32(%r14)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	add_su3_vector, .Ltmp17-add_su3_vector
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
