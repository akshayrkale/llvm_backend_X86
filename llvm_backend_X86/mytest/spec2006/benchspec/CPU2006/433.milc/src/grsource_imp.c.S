	.file	"grsource_imp.c.bc"
	.text
	.globl	grsource_imp
	.align	16, 0x90
	.type	grsource_imp,@function
grsource_imp:                           # @grsource_imp
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
	movslq	sites_on_node(%rip), %rax
	movq	%rdx, %r14
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %r12
	testq	%rax, %rax
	jle	.LBB0_3
# BB#1:                                 # %for.cond1.preheader.lr.ph
	movq	lattice(%rip), %rbx
	addq	$2472, %rbx             # imm = 0x9A8
	xorq	%r15, %r15
	.align	16, 0x90
.LBB0_2:                                # %for.cond1.preheader
                                        # =>This Inner Loop Header: Depth=1
	leaq	-2456(%rbx), %r13
	movq	%r13, %rdi
	callq	gaussian_rand_no
	movq	%rax, -40(%rbx)
	movq	%r13, %rdi
	callq	gaussian_rand_no
	movq	%rax, -32(%rbx)
	movq	%r13, %rdi
	callq	gaussian_rand_no
	movq	%rax, -24(%rbx)
	movq	%r13, %rdi
	callq	gaussian_rand_no
	movq	%rax, -16(%rbx)
	movq	%r13, %rdi
	callq	gaussian_rand_no
	movq	%rax, -8(%rbx)
	movq	%r13, %rdi
	callq	gaussian_rand_no
	movq	%rax, (%rbx)
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
.LBB0_3:                                # %for.end12
	movl	valid_longlinks(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_5
# BB#4:                                 # %if.then
	xorq	%rax, %rax
	callq	load_longlinks
.LBB0_5:                                # %if.end
	movl	valid_fatlinks(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_7
# BB#6:                                 # %if.then14
	xorq	%rax, %rax
	callq	load_fatlinks
.LBB0_7:                                # %if.end15
	movq	$2432, %rdi             # imm = 0x980
	movq	%r12, %rsi
	movq	%r14, %rdx
	callq	dslash_fn
	movabsq	$-4616189618054758400, %rsi # imm = 0xBFF0000000000000
	movq	%r12, %rdi
	movq	%r12, %rdx
	movq	%r14, %rcx
	callq	scalar_mult_latvec
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rdi, %rsi
	callq	float64_add
	movq	$2432, %rsi             # imm = 0x980
	movq	%r12, %rdi
	movq	%rax, %rdx
	movq	%r12, %rcx
	movq	%r14, %r8
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	scalar_mult_add_latvec  # TAILCALL
.Ltmp17:
	.size	grsource_imp, .Ltmp17-grsource_imp
	.cfi_endproc

	.globl	checkmul_imp
	.align	16, 0x90
	.type	checkmul_imp,@function
checkmul_imp:                           # @checkmul_imp
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp24:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp25:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp26:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp27:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp28:
	.cfi_def_cfa_offset 48
.Ltmp29:
	.cfi_offset %rbx, -48
.Ltmp30:
	.cfi_offset %r12, -40
.Ltmp31:
	.cfi_offset %r13, -32
.Ltmp32:
	.cfi_offset %r14, -24
.Ltmp33:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$2384, %rsi             # imm = 0x950
	movq	$3, %rdx
                                        # kill: RDI<def> R14<kill>
	callq	dslash_fn
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_add
	movq	$2384, %rdi             # imm = 0x950
	movq	$2384, %rcx             # imm = 0x950
	movq	$3, %r8
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	scalar_mult_add_latvec
	movslq	sites_on_node(%rip), %rax
	testq	%rax, %rax
	jle	.LBB1_3
# BB#1:                                 # %for.body.lr.ph
	movq	lattice(%rip), %r14
	xorq	%r13, %r13
	movabsq	$.L.str1, %r15
	.align	16, 0x90
.LBB1_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movswq	(%r14), %rsi
	movswq	2(%r14), %rdx
	movswq	4(%r14), %rcx
	movswq	6(%r14), %r8
	xorq	%rax, %rax
	movabsq	$.L.str, %rdi
	callq	printf
	movq	2384(%r14), %r12
	movq	2432(%r14), %rbx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	float64_sub
	movq	%rax, %r9
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rcx
	movq	%r12, %r8
	callq	printf
	movq	2392(%r14), %r12
	movq	2440(%r14), %rbx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	float64_sub
	movq	%rax, %r9
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rcx
	movq	%r12, %r8
	callq	printf
	movq	2400(%r14), %r12
	movq	2448(%r14), %rbx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	float64_sub
	movq	%rax, %r9
	movq	$1, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rcx
	movq	%r12, %r8
	callq	printf
	movq	2408(%r14), %r12
	movq	2456(%r14), %rbx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	float64_sub
	movq	%rax, %r9
	movq	$1, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rcx
	movq	%r12, %r8
	callq	printf
	movq	2416(%r14), %r12
	movq	2464(%r14), %rbx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	float64_sub
	movq	%rax, %r9
	movq	$2, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rcx
	movq	%r12, %r8
	callq	printf
	movq	2424(%r14), %r12
	movq	2472(%r14), %rbx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	float64_sub
	movq	%rax, %r9
	movq	$2, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rcx
	movq	%r12, %r8
	callq	printf
	movq	$10, %rdi
	callq	putchar
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movslq	sites_on_node(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$3200, %r14             # imm = 0xC80
	cmpq	%rdx, %rax
	jl	.LBB1_2
.LBB1_3:                                # %for.end66
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp34:
	.size	checkmul_imp, .Ltmp34-checkmul_imp
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Site %d %d %d %d\n"
	.size	.L.str, 18

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"%d %d\t%e\t%e\t%e\n"
	.size	.L.str1, 16


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
