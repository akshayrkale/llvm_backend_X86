	.file	"m_mat_na.c.bc"
	.text
	.globl	mult_su3_na
	.align	16, 0x90
	.type	mult_su3_na,@function
mult_su3_na:                            # @mult_su3_na
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
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	%rdi, 88(%rsp)          # 8-byte Spill
	addq	$8, %rdx
	xorq	%rax, %rax
	.align	16, 0x90
.LBB0_1:                                # %for.cond1.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_2 Depth 2
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	leaq	(%rax,%rax,2), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$4, %rcx
	shlq	%cl, %rax
	addq	%rdi, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$40, %r14
	movq	%rdx, %rcx
	.align	16, 0x90
.LBB0_2:                                # %for.cond4.preheader
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	-40(%rax,%r14), %rbx
	movq	-32(%rax,%r14), %r13
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%rax, %r12
	movq	$4, %rcx
	movq	24(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx,%rax), %r15
	movq	16(%rcx,%rax), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
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
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	float64_add
	movq	%rax, 48(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	float64_add
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	-24(%rbx,%r14), %r15
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	float64_mul
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	24(%rcx,%rdx), %r13
	movq	-16(%rbx,%r14), %r12
	movq	%rax, %rbx
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	callq	float64_add
	movq	88(%rsp), %r15          # 8-byte Reload
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	32(%r15,%rbx), %rdi
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	80(%rsp), %r13          # 8-byte Reload
	movq	-8(%r13,%r14), %r12
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	%r12, %rsi
	callq	float64_mul
	movq	40(%r15,%rbx), %rbx
	movq	(%r13,%r14), %r13
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r15
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, %rbx
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	float64_add
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, -8(%rcx)
	movq	%rax, (%rcx)
	addq	$48, %r14
	addq	$16, %rcx
	cmpq	$184, %r14
	jne	.LBB0_2
# BB#3:                                 # %for.inc71
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	(%rsp), %rax            # 8-byte Reload
	incq	%rax
	movq	8(%rsp), %rdx           # 8-byte Reload
	addq	$48, %rdx
	cmpq	$3, %rax
	movq	88(%rsp), %rdi          # 8-byte Reload
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
	.size	mult_su3_na, .Ltmp17-mult_su3_na
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
