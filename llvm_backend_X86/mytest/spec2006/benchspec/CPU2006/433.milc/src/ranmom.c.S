	.file	"ranmom.c.bc"
	.text
	.globl	ranmom
	.align	16, 0x90
	.type	ranmom,@function
ranmom:                                 # @ranmom
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6:
	.cfi_def_cfa_offset 32
.Ltmp7:
	.cfi_offset %rbx, -32
.Ltmp8:
	.cfi_offset %r14, -24
.Ltmp9:
	.cfi_offset %r15, -16
	movslq	sites_on_node(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_3
# BB#1:                                 # %for.cond1.preheader.lr.ph
	movq	lattice(%rip), %rbx
	addq	$2080, %rbx             # imm = 0x820
	xorq	%r15, %r15
	.align	16, 0x90
.LBB0_2:                                # %for.cond1.preheader
                                        # =>This Inner Loop Header: Depth=1
	leaq	-240(%rbx), %rdi
	leaq	-2064(%rbx), %r14
	movq	%r14, %rsi
	callq	random_anti_hermitian
	leaq	-160(%rbx), %rdi
	movq	%r14, %rsi
	callq	random_anti_hermitian
	leaq	-80(%rbx), %rdi
	movq	%r14, %rsi
	callq	random_anti_hermitian
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	random_anti_hermitian
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movslq	sites_on_node(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$3200, %rbx             # imm = 0xC80
	cmpq	%rdx, %rax
	jl	.LBB0_2
.LBB0_3:                                # %for.end6
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	ranmom, .Ltmp10-ranmom
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
