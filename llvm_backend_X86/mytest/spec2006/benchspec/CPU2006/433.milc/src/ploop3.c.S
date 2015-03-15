	.file	"ploop3.c.bc"
	.text
	.globl	ploop
	.align	16, 0x90
	.type	ploop,@function
ploop:                                  # @ploop
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
	subq	$192, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 240
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
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	callq	cmplx
	movq	%rax, 176(%rsp)
	movq	%rdx, 184(%rsp)
	movq	gen_pt(%rip), %r8
	movl	%ebx, 24(%rsp)
	movl	%ebx, 20(%rsp)
	movl	%ebx, 16(%rsp)
	movq	$544, %rdi              # imm = 0x220
	movq	$144, %rsi
	movq	$3, %rdx
	movq	$2, %rcx
	callq	start_gather
	movq	%rax, %r14
	movq	%r14, %rdi
	callq	wait_gather
	movslq	even_sites_on_node(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_3
# BB#1:                                 # %for.body.lr.ph
	movq	lattice(%rip), %r15
	addq	$2912, %r15             # imm = 0xB60
	movq	$1, %rbx
	.align	16, 0x90
.LBB0_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	gen_pt(%rip), %rax
	movq	-8(%rax,%rbx,8), %rsi
	leaq	-2368(%r15), %rdi
	movq	%r15, %rdx
	callq	mult_su3_nn
	movslq	even_sites_on_node(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	addq	$3200, %r15             # imm = 0xC80
	cmpq	%rax, %rdx
	jl	.LBB0_2
.LBB0_3:                                # %for.end
	movq	%r14, %rdi
	callq	cleanup_gather
	movslq	nt(%rip), %rax
	cmpq	$3, %rax
	jl	.LBB0_11
# BB#4:                                 # %for.body14.lr.ph
	movq	$2, %r12
	leaq	32(%rsp), %r13
	.align	16, 0x90
.LBB0_5:                                # %for.body14
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_7 Depth 2
	movq	gen_pt(%rip), %r8
	movl	%r12d, 28(%rsp)
	movq	$2912, %rdi             # imm = 0xB60
	movq	$144, %rsi
	movq	$2, %rcx
	leaq	16(%rsp), %rdx
	callq	start_general_gather
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rax, %rdi
	callq	wait_general_gather
	movslq	even_sites_on_node(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_10
# BB#6:                                 # %for.body27.lr.ph
                                        #   in Loop: Header=BB0_5 Depth=1
	movq	lattice(%rip), %rbx
	movq	$1, %r14
	movq	$2912, %r15             # imm = 0xB60
	.align	16, 0x90
.LBB0_7:                                # %for.body27
                                        #   Parent Loop BB0_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movswq	-2906(%rbx,%r15), %rcx
	cmpq	$1, %rcx
	jg	.LBB0_9
# BB#8:                                 # %if.end
                                        #   in Loop: Header=BB0_7 Depth=2
	movq	gen_pt(%rip), %rax
	movq	-8(%rax,%r14,8), %rsi
	leaq	(%rbx,%r15), %rdi
	movq	%r13, %rdx
	callq	mult_su3_nn
	movq	lattice(%rip), %rdi
	addq	%r15, %rdi
	movq	$144, %rdx
	movq	%r13, %rsi
	callq	memcpy
	movl	even_sites_on_node(%rip), %eax
.LBB0_9:                                # %for.inc38
                                        #   in Loop: Header=BB0_7 Depth=2
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r14
	addq	$3200, %r15             # imm = 0xC80
	cmpq	%rsi, %rdx
	jl	.LBB0_7
.LBB0_10:                               # %for.end41
                                        #   in Loop: Header=BB0_5 Depth=1
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	cleanup_general_gather
	addq	$2, %r12
	movq	$32, %rcx
	movq	%r12, %rax
	movslq	nt(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB0_5
.LBB0_11:                               # %for.end43
	movslq	even_sites_on_node(%rip), %rdx
	testq	%rdx, %rdx
	jle	.LBB0_16
# BB#12:                                # %for.body47.lr.ph
	movq	lattice(%rip), %rbx
	addq	$2912, %rbx             # imm = 0xB60
	xorq	%r13, %r13
	.align	16, 0x90
.LBB0_13:                               # %for.body47
                                        # =>This Inner Loop Header: Depth=1
	movswq	-2906(%rbx), %rax
	cmpq	$1, %rax
	jg	.LBB0_15
# BB#14:                                # %if.end53
                                        #   in Loop: Header=BB0_13 Depth=1
	movq	%rbx, %rdi
	callq	trace_su3
	movq	176(%rsp), %rdi
	movq	184(%rsp), %r14
	movq	%rdx, %r15
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r12
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	float64_add
	movl	even_sites_on_node(%rip), %edx
	movq	%rax, 184(%rsp)
	movq	%r12, 176(%rsp)
.LBB0_15:                               # %for.inc61
                                        #   in Loop: Header=BB0_13 Depth=1
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	$3200, %rbx             # imm = 0xC80
	cmpq	%rsi, %rax
	jl	.LBB0_13
.LBB0_16:                               # %for.end64
	leaq	176(%rsp), %rdi
	callq	g_complexsum
	movl	nx(%rip), %eax
	movl	ny(%rip), %ecx
	movl	nz(%rip), %edx
	imulq	%rax, %rcx
	imulq	%rcx, %rdx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdx, %rdi
	callq	int32_to_float64
	movq	%rax, %rbx
	movq	176(%rsp), %rdi
	movq	184(%rsp), %r14
	movq	%rbx, %rsi
	callq	float64_div
	movq	%rax, %r15
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	float64_div
	movq	%rax, %rcx
	movq	%r15, %rax
	movq	%rcx, %rdx
	addq	$192, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	ploop, .Ltmp17-ploop
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
