	.file	"s_m_s_mat.c.bc"
	.text
	.globl	scalar_mult_sub_su3_matrix
	.align	16, 0x90
	.type	scalar_mult_sub_su3_matrix,@function
scalar_mult_sub_su3_matrix:             # @scalar_mult_sub_su3_matrix
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
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	$32, %r12
	.align	16, 0x90
.LBB0_1:                                # %for.cond1.preheader
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	-32(%rbx,%r12), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	-24(%rbx,%r12), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	-32(%r14,%r12), %r13
	movq	-24(%r14,%r12), %rsi
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	float64_sub
	movq	(%rsp), %r13            # 8-byte Reload
	movq	%rax, -24(%r13,%r12)
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, -32(%r13,%r12)
	movq	-16(%rbx,%r12), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	-8(%rbx,%r12), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	-16(%r14,%r12), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	-8(%r14,%r12), %rsi
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	float64_sub
	movq	%rax, -8(%r13,%r12)
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, -16(%r13,%r12)
	movq	(%rbx,%r12), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	8(%rbx,%r12), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	(%r14,%r12), %rbx
	movq	8(%r14,%r12), %rsi
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	float64_sub
	movq	%rax, 8(%r13,%r12)
	movq	%rbx, (%r13,%r12)
	addq	$48, %r12
	cmpq	$176, %r12
	jne	.LBB0_1
# BB#2:                                 # %for.end39
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	scalar_mult_sub_su3_matrix, .Ltmp17-scalar_mult_sub_su3_matrix
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
