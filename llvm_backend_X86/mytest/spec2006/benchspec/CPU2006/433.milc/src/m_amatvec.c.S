	.file	"m_amatvec.c.bc"
	.text
	.globl	mult_adj_su3_mat_vec
	.align	16, 0x90
	.type	mult_adj_su3_mat_vec,@function
mult_adj_su3_mat_vec:                   # @mult_adj_su3_mat_vec
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
	subq	$64, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 112
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
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	xorq	%r14, %r14
	.align	16, 0x90
.LBB0_1:                                # %for.cond1.preheader
                                        # =>This Inner Loop Header: Depth=1
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r14), %rbx
	movq	8(%rax,%r14), %r13
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rsi
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	8(%rax), %r15
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%rax, %r12
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r12
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	float64_add
	movq	%rax, 40(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	float64_add
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	48(%rcx,%r14), %r15
	movq	56(%rcx,%r14), %r13
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	16(%rbx), %rsi
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	callq	float64_mul
	movq	24(%rbx), %r12
	movq	%rax, %rbx
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	callq	float64_add
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	96(%rcx,%r14), %r15
	movq	104(%rcx,%r14), %r13
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	32(%rbx), %rsi
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	callq	float64_mul
	movq	40(%rbx), %r12
	movq	%rax, %rbx
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r15
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, %rbx
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	float64_add
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rbx, (%rcx,%r14)
	movq	%rax, 8(%rcx,%r14)
	addq	$16, %r14
	cmpq	$48, %r14
	jne	.LBB0_1
# BB#2:                                 # %for.end52
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	mult_adj_su3_mat_vec, .Ltmp17-mult_adj_su3_mat_vec
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
