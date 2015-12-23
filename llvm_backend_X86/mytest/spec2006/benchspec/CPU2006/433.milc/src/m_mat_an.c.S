	.file	"m_mat_an.c.bc"
	.text
	.globl	mult_su3_an
	.align	16, 0x90
	.type	mult_su3_an,@function
mult_su3_an:                            # @mult_su3_an
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
	subq	$96, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 144
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
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	movq	%rdi, 80(%rsp)          # 8-byte Spill
	addq	$8, %rdx
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB0_1:                                # %for.cond1.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_2 Depth 2
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	$4, %rcx
	movq	24(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	addq	%rdi, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$-48, %rcx
	.align	16, 0x90
.LBB0_2:                                # %for.cond4.preheader
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %r13
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	48(%rax,%rcx), %r12
	movq	56(%rax,%rcx), %rbx
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	$4, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	8(%rax,%rdx), %r15
	movq	48(%rax,%rdx), %r14
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	float64_add
	movq	%rax, 48(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	float64_add
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	96(%rbx,%rcx), %rsi
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	callq	float64_mul
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	56(%rcx,%rdx), %r12
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	104(%rbx,%rcx), %r15
	movq	%rax, %r13
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r13
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	callq	float64_add
	movq	80(%rsp), %r14          # 8-byte Reload
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	96(%r14,%rbx), %r12
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	72(%rsp), %r13          # 8-byte Reload
	movq	144(%r13,%rcx), %r15
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	104(%r14,%rbx), %rbx
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	152(%r13,%rcx), %r13
	movq	%rax, %r14
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r14
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rax, %r12
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r15
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	float64_add
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, -8(%rdx)
	movq	%rax, (%rdx)
	addq	$16, %rdx
	addq	$16, %rcx
	jne	.LBB0_2
# BB#3:                                 # %for.inc71
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	incq	%rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	8(%rsp), %rdx           # 8-byte Reload
	addq	$48, %rdx
	cmpq	$3, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	jne	.LBB0_1
# BB#4:                                 # %for.end73
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	mult_su3_an, .Ltmp17-mult_su3_an
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
