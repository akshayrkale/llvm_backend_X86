	.file	"f_meas.c.bc"
	.text
	.globl	f_meas_imp
	.align	16, 0x90
	.type	f_meas_imp,@function
f_meas_imp:                             # @f_meas_imp
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
	movq	%rdx, %r15
	movq	%r15, (%rsp)            # 8-byte Spill
	movq	%rsi, %r14
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %rbx
	movq	$0, 24(%rsp)
	movq	$0, 32(%rsp)
	movq	$0, 40(%rsp)
	movq	$3, %rdx
                                        # kill: RDI<def> RBX<kill>
	movq	%r15, %rsi
	callq	grsource_imp
	movq	$1280, %rdi             # imm = 0x500
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	%rbx, %r12
	movq	%r15, %rcx
	callq	mat_invert_uml
	movslq	even_sites_on_node(%rip), %rbx
	testq	%rbx, %rbx
	jle	.LBB0_3
# BB#1:                                 # %for.body5.lr.ph
	movq	lattice(%rip), %r14
	movq	$32, %rcx
	movq	8(%rsp), %rax           # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	%r12, %r15
	xorq	%r12, %r12
	.align	16, 0x90
.LBB0_2:                                # %for.body5
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	leaq	(%r14,%rax), %r13
	leaq	(%r14,%r15), %rdi
	movq	%r13, %rsi
	callq	su3_dot
	movq	24(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 24(%rsp)
	leaq	1280(%r14), %rdi
	movq	%r13, %rsi
	callq	su3_dot
	movq	40(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 40(%rsp)
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movslq	even_sites_on_node(%rip), %rbx
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$1808, %r14             # imm = 0x710
	cmpq	%rbx, %rax
	jl	.LBB0_2
.LBB0_3:                                # %for.end
	movslq	sites_on_node(%rip), %rax
	cmpq	%rax, %rbx
	movq	8(%rsp), %r15           # 8-byte Reload
	jge	.LBB0_6
# BB#4:                                 # %for.body20.lr.ph
	movq	lattice(%rip), %rax
	imulq	$1808, %rbx, %r14       # imm = 0x710
	addq	%rax, %r14
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	.align	16, 0x90
.LBB0_5:                                # %for.body20
                                        # =>This Inner Loop Header: Depth=1
	leaq	(%r14,%r15), %rsi
	leaq	1280(%r14), %rdi
	callq	su3_dot
	movq	32(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 32(%rsp)
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movslq	sites_on_node(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$1808, %r14             # imm = 0x710
	cmpq	%rdx, %rax
	jl	.LBB0_5
.LBB0_6:                                # %for.end31
	leaq	32(%rsp), %rdi
	callq	g_doublesum
	leaq	40(%rsp), %rdi
	callq	g_doublesum
	leaq	24(%rsp), %rdi
	callq	g_doublesum
	movl	volume(%rip), %edi
	callq	int32_to_float64
	movq	%rax, %rbx
	movabsq	$4611686018427387904, %rdi # imm = 0x4000000000000000
	movq	%rbx, %rsi
	callq	float64_div
	movq	24(%rsp), %r15
	movq	%rax, %r12
	movabsq	$4607182418800017408, %rdi # imm = 0x3FF0000000000000
	movq	%rbx, %rsi
	callq	float64_div
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movl	this_node(%rip), %ecx
	movq	%rax, %r15
	testq	%rcx, %rcx
	jne	.LBB0_9
# BB#7:                                 # %if.end
	movq	40(%rsp), %rsi
	movq	32(%rsp), %rbx
	movq	%r12, %rdi
	callq	float64_mul
	movq	%rax, %r13
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%rax, %rcx
	movabsq	$.L.str, %rdi
	movq	$1, %r8
	movq	$1, %r9
	xorq	%rax, %rax
	movq	(%rsp), %rsi            # 8-byte Reload
	movq	%r13, %rdx
	callq	printf
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_9
# BB#8:                                 # %if.then45
	movabsq	$.L.str1, %rdi
	movq	$1, %rcx
	movq	$1, %r8
	xorq	%rax, %rax
	movq	(%rsp), %rsi            # 8-byte Reload
	movq	%r15, %rdx
	callq	printf
.LBB0_9:                                # %for.inc49
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	f_meas_imp, .Ltmp17-f_meas_imp
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"PBP: mass %e     %e  %e ( %d of %d )\n"
	.size	.L.str, 38

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"FACTION: mass = %e,  %e ( %d of %d )\n"
	.size	.L.str1, 38


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
