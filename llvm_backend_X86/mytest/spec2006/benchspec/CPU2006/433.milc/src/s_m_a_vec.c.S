	.file	"s_m_a_vec.c.bc"
	.text
	.globl	scalar_mult_add_su3_vector
	.align	16, 0x90
	.type	scalar_mult_add_su3_vector,@function
scalar_mult_add_su3_vector:             # @scalar_mult_add_su3_vector
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
	subq	$32, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 80
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
	movq	%rsi, %r12
	movq	%rdi, %rbx
	movq	(%rbx), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	8(%rbx), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	(%r12), %r13
	movq	8(%r12), %rsi
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r13
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	float64_add
	movq	%rax, 8(%r14)
	movq	%r13, (%r14)
	movq	16(%rbx), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	24(%rbx), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	16(%r12), %r13
	movq	24(%r12), %rsi
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r13
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	float64_add
	movq	%rax, 24(%r14)
	movq	%r13, 16(%r14)
	movq	32(%rbx), %r13
	movq	40(%rbx), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	32(%r12), %rbx
	movq	40(%r12), %rsi
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, %r12
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	float64_add
	movq	%rax, 40(%r14)
	movq	%rbx, 32(%r14)
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	scalar_mult_add_su3_vector, .Ltmp17-scalar_mult_add_su3_vector
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
