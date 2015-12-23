	.file	"m_matvec.c.bc"
	.text
	.globl	mult_su3_mat_vec
	.align	16, 0x90
	.type	mult_su3_mat_vec,@function
mult_su3_mat_vec:                       # @mult_su3_mat_vec
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
	subq	$48, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 96
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
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	(%rdi), %rbx
	movq	8(%rdi), %r12
	movq	(%rsi), %r13
	movq	8(%rsi), %r15
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rax, %r14
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r14
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	float64_add
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	float64_add
	movq	%rax, 16(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	float64_add
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	16(%r14), %r15
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	16(%rbx), %rsi
	movq	%rsi, (%rsp)            # 8-byte Spill
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rdi
	callq	float64_mul
	movq	24(%r14), %r14
	movq	24(%rbx), %r12
	movq	%rax, %r13
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %r15
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%r14, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rbx, %rsi
	callq	float64_add
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	32(%r14), %r15
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	32(%rbx), %rsi
	movq	%rsi, (%rsp)            # 8-byte Spill
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rdi
	callq	float64_mul
	movq	40(%r14), %r14
	movq	40(%rbx), %r12
	movq	%rax, %r13
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %r15
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%r14, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	float64_add
	movq	%rax, %r14
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r14, %rsi
	callq	float64_add
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, (%rcx)
	movq	%rax, 8(%rcx)
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	48(%rax), %r12
	movq	56(%rax), %r14
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %r15
	movq	8(%rax), %rbx
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, %r13
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r13
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	float64_add
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	float64_add
	movq	%rax, 16(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	float64_add
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	64(%r14), %r15
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	16(%rbx), %rsi
	movq	%rsi, (%rsp)            # 8-byte Spill
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rdi
	callq	float64_mul
	movq	72(%r14), %r14
	movq	24(%rbx), %r12
	movq	%rax, %r13
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %r15
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%r14, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rbx, %rsi
	callq	float64_add
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	80(%r14), %r15
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	32(%rbx), %rsi
	movq	%rsi, (%rsp)            # 8-byte Spill
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rdi
	callq	float64_mul
	movq	88(%r14), %r14
	movq	40(%rbx), %r12
	movq	%rax, %r13
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %r15
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%r14, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	float64_add
	movq	%rax, %r14
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r14, %rsi
	callq	float64_add
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, 16(%rcx)
	movq	%rax, 24(%rcx)
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	96(%rax), %r12
	movq	104(%rax), %r14
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %r15
	movq	8(%rax), %rbx
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, %r13
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r13
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	float64_add
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	float64_add
	movq	%rax, 16(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	float64_add
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	112(%r14), %r15
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	16(%rbx), %rsi
	movq	%rsi, (%rsp)            # 8-byte Spill
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rdi
	callq	float64_mul
	movq	120(%r14), %r14
	movq	24(%rbx), %r12
	movq	%rax, %r13
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %r15
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%r14, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rbx, %rsi
	callq	float64_add
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	128(%r14), %r15
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	32(%rbx), %rsi
	movq	%rsi, (%rsp)            # 8-byte Spill
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rdi
	callq	float64_mul
	movq	136(%r14), %r14
	movq	40(%rbx), %r12
	movq	%rax, %r13
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %r15
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%r14, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	float64_add
	movq	%rax, %r14
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r14, %rsi
	callq	float64_add
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, 32(%rcx)
	movq	%rax, 40(%rcx)
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	mult_su3_mat_vec, .Ltmp17-mult_su3_mat_vec
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
