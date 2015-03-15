	.file	"m_su2_mat_vec_a.c.bc"
	.text
	.globl	mult_su2_mat_vec_elem_a
	.align	16, 0x90
	.type	mult_su2_mat_vec_elem_a,@function
mult_su2_mat_vec_elem_a:                # @mult_su2_mat_vec_elem_a
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
	subq	$80, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 128
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
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	(%rsi), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	8(%rsi), %rbx
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	(%rdx), %rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	8(%rdx), %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	movq	(%rdi), %r12
	movq	8(%rdi), %r15
	movq	%rax, %rdi
	movq	%rax, %r13
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %r14
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	16(%rbx), %r14
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	56(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	24(%rbx), %r13
	movq	%rax, %rbx
	movq	64(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	%rax, %r14
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r14
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	callq	float64_add
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	%rax, (%rbx)
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r14, %rsi
	callq	float64_add
	movq	%rax, 8(%rbx)
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %r13
	movq	40(%rax), %r14
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rax, %r12
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	48(%rbx), %r13
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	56(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	56(%rbx), %rbx
	movq	%rax, %r15
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r15
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rax, %r13
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r13
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	float64_add
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rax, (%rbx)
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, 8(%rbx)
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	mult_su2_mat_vec_elem_a, .Ltmp17-mult_su2_mat_vec_elem_a
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
