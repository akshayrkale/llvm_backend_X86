	.file	"mat_invert.c.bc"
	.text
	.globl	mat_invert_cg
	.align	16, 0x90
	.type	mat_invert_cg,@function
mat_invert_cg:                          # @mat_invert_cg
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
	subq	$16, %rsp
.Ltmp7:
	.cfi_def_cfa_offset 48
.Ltmp8:
	.cfi_offset %rbx, -32
.Ltmp9:
	.cfi_offset %r14, -24
.Ltmp10:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$3, %rsi
	movq	%rbx, %rdi
	callq	clear_latvec
	movq	rsqprop(%rip), %r8
	movl	niter(%rip), %ecx
	leaq	8(%rsp), %rax
	movq	$3, %r9
	movq	%rax, (%rsp)
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	ks_congrad
	movq	%rax, %r15
	movq	$2384, %rsi             # imm = 0x950
	movq	$3, %rdx
	movq	%rbx, %rdi
	callq	dslash_fn
	movabsq	$-4611686018427387904, %rsi # imm = 0xC000000000000000
	movq	%r14, %rdi
	callq	float64_mul
	movq	$2384, %rdi             # imm = 0x950
	movq	$2384, %rcx             # imm = 0x950
	movq	$3, %r8
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	scalar_mult_add_latvec
	movabsq	$-4616189618054758400, %rsi # imm = 0xBFF0000000000000
	movq	$2384, %rdi             # imm = 0x950
	movq	$3, %rcx
	movq	%rbx, %rdx
	callq	scalar_mult_latvec
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp11:
	.size	mat_invert_cg, .Ltmp11-mat_invert_cg
	.cfi_endproc

	.globl	mat_invert_uml
	.align	16, 0x90
	.type	mat_invert_uml,@function
mat_invert_uml:                         # @mat_invert_uml
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp18:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp19:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp20:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp21:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp22:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp23:
	.cfi_def_cfa_offset 96
.Ltmp24:
	.cfi_offset %rbx, -48
.Ltmp25:
	.cfi_offset %r12, -40
.Ltmp26:
	.cfi_offset %r13, -32
.Ltmp27:
	.cfi_offset %r14, -24
.Ltmp28:
	.cfi_offset %r15, -16
	movq	%rcx, %r12
	movq	%rdx, %r15
	movq	%rsi, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r15, %rcx
	andq	%rax, %rcx
	andq	%rdi, %rax
	cmpq	%rcx, %rax
	je	.LBB1_5
# BB#1:                                 # %if.end
	movq	$2384, %rsi             # imm = 0x950
	movq	$2, %rdx
	movq	%rdi, %rbx
	movq	%rbx, 16(%rsp)          # 8-byte Spill
                                        # kill: RDI<def> RBX<kill>
	callq	dslash_fn
	movabsq	$-4611686018427387904, %rsi # imm = 0xC000000000000000
	movq	%r12, %rdi
	callq	float64_mul
	movq	$2384, %rdi             # imm = 0x950
	movq	$2, %r8
	movq	%rbx, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	callq	scalar_mult_add_latvec
	movabsq	$-4616189618054758400, %rsi # imm = 0xBFF0000000000000
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	%r15, %rdx
	callq	scalar_mult_latvec
	movq	rsqprop(%rip), %r8
	movl	niter(%rip), %ecx
	leaq	40(%rsp), %rax
	movq	$2, %r9
	movq	%rax, (%rsp)
	movq	%r15, %rdi
	movq	%r14, %rbx
	movq	%rbx, %rsi
	movq	%r12, %rdx
	callq	ks_congrad
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$2384, %rsi             # imm = 0x950
	movq	$1, %rdx
	movq	%rbx, %rdi
	callq	dslash_fn
	movslq	even_sites_on_node(%rip), %r15
	movslq	sites_on_node(%rip), %rax
	cmpq	%rax, %r15
	jge	.LBB1_4
# BB#2:                                 # %for.body.lr.ph
	movq	lattice(%rip), %rax
	imulq	$3200, %r15, %r14       # imm = 0xC80
	addq	%rax, %r14
	movq	%r12, %rdi
	movq	%r12, %rsi
	callq	float64_add
	movabsq	$4607182418800017408, %rdi # imm = 0x3FF0000000000000
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, %r13
	movq	$32, %rcx
	movq	16(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, %r12
	.align	16, 0x90
.LBB1_3:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	leaq	2384(%r14), %rsi
	leaq	(%r14,%r13), %rbx
	leaq	(%r14,%r12), %rdi
	movq	%rbx, %rdx
	callq	sub_su3_vector
	movq	%rbx, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	callq	scalar_mult_su3_vector
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movslq	sites_on_node(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$3200, %r14             # imm = 0xC80
	cmpq	%rdx, %rax
	jl	.LBB1_3
.LBB1_4:                                # %for.end
	movq	24(%rsp), %rax          # 8-byte Reload
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_5:                                # %if.then
	movabsq	$.Lstr, %rax
	movq	%rax, %rdi
	callq	puts
	xorq	%rdi, %rdi
	callq	exit
.Ltmp29:
	.size	mat_invert_uml, .Ltmp29-mat_invert_uml
	.cfi_endproc

	.globl	check_invert
	.align	16, 0x90
	.type	check_invert,@function
check_invert:                           # @check_invert
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp36:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp37:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp38:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp39:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp40:
	.cfi_def_cfa_offset 48
	subq	$112, %rsp
.Ltmp41:
	.cfi_def_cfa_offset 160
.Ltmp42:
	.cfi_offset %rbx, -48
.Ltmp43:
	.cfi_offset %r12, -40
.Ltmp44:
	.cfi_offset %r13, -32
.Ltmp45:
	.cfi_offset %r14, -24
.Ltmp46:
	.cfi_offset %r15, -16
	movq	%rcx, %rbx
	movq	%rdx, %r15
	movq	%rsi, %r13
	movq	%rdi, %r14
	movq	$2288, %rsi             # imm = 0x8F0
	movq	$3, %rdx
                                        # kill: RDI<def> R14<kill>
	callq	dslash_fn
	movslq	sites_on_node(%rip), %rax
	testq	%rax, %rax
	jle	.LBB2_1
# BB#5:                                 # %for.body.lr.ph
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movq	lattice(%rip), %r12
	movq	%r15, %rdi
	movq	%r15, %rsi
	callq	float64_add
	movq	%rax, %r15
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB2_6:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	leaq	(%r12,%r14), %rsi
	leaq	2288(%r12), %rdi
	movq	%r15, %rdx
	movq	%rdi, %rcx
	callq	scalar_mult_add_su3_vector
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movslq	sites_on_node(%rip), %rax
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$3200, %r12             # imm = 0xC80
	cmpq	%rax, %rdx
	jl	.LBB2_6
# BB#7:                                 # %for.end
	movq	$0, 104(%rsp)
	movq	$0, 96(%rsp)
	testq	%rax, %rax
	movq	%r13, %r14
	jle	.LBB2_2
# BB#8:                                 # %for.cond9.preheader.lr.ph
	movq	lattice(%rip), %r12
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	%r14, 72(%rsp)          # 8-byte Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movabsq	$9223372036854775807, %rbx # imm = 0x7FFFFFFFFFFFFFFF
	.align	16, 0x90
.LBB2_9:                                # %for.cond9.preheader
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, 56(%rsp)          # 8-byte Spill
	movq	(%r12,%r14), %r15
	movq	8(%r12,%r14), %r13
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movq	2288(%r12), %rsi
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	2296(%r12), %r14
	movq	%r15, %rdi
	callq	float64_sub
	movq	%rax, %r12
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%r14, %r13
	callq	float64_sub
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	andq	%rbx, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	__gtdf2
	movq	%r15, %rbx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB2_15
# BB#10:                                # %lor.lhs.false
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	64(%rsp), %r15          # 8-byte Reload
	movabsq	$9223372036854775807, %rax # imm = 0x7FFFFFFFFFFFFFFF
	andq	%rax, %r15
	movq	%r15, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r14
	je	.LBB2_12
# BB#11:                                # %lor.lhs.false
                                        #   in Loop: Header=BB2_9 Depth=1
	movabsq	$1, %r14
.LBB2_12:                               # %lor.lhs.false
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	%r15, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	__ledf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rax
	jg	.LBB2_14
# BB#13:                                # %lor.lhs.false
                                        #   in Loop: Header=BB2_9 Depth=1
	movabsq	$1, %rax
.LBB2_14:                               # %lor.lhs.false
                                        #   in Loop: Header=BB2_9 Depth=1
	xorq	%rcx, %rcx
	orq	%rax, %r14
	movq	32(%rsp), %rbx          # 8-byte Reload
	jne	.LBB2_16
.LBB2_15:                               # %if.then37
                                        #   in Loop: Header=BB2_9 Depth=1
	movabsq	$0, %rdx
	xorq	%rax, %rax
	movq	%r13, (%rsp)
	movabsq	$.L.str1, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rcx
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	40(%rsp), %r9           # 8-byte Reload
	callq	printf
	xorq	%rdi, %rdi
	callq	terminate
	movq	$1, %rcx
.LBB2_16:                               # %if.end64
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rax, %r14
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	104(%rsp), %rsi
	movq	%rax, %rdi
	callq	float64_add
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rax, 104(%rsp)
	movq	56(%rsp), %r12          # 8-byte Reload
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	16(%r12,%rax), %rdi
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	movq	24(%r12,%rax), %r14
	movq	%r14, 48(%rsp)          # 8-byte Spill
	movq	2304(%r12), %rsi
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	2312(%r12), %r15
	movq	%r15, 32(%rsp)          # 8-byte Spill
	callq	float64_sub
	movq	%r14, %rdi
	movq	%rax, %rbx
	movq	%r15, %rsi
	callq	float64_sub
	movq	%rax, %r14
	movq	%rbx, %rdi
	movabsq	$9223372036854775807, %r15 # imm = 0x7FFFFFFFFFFFFFFF
	andq	%r15, %rdi
	movq	88(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rsi
	callq	__gtdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	testq	%rax, %rax
	jg	.LBB2_20
# BB#17:                                # %lor.lhs.false.1
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	%r14, %rdi
	andq	%r15, %rdi
	movq	%r13, %rsi
	callq	__gtdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB2_20
# BB#18:                                # %if.end.1
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	jne	.LBB2_20
# BB#19:                                #   in Loop: Header=BB2_9 Depth=1
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	movq	8(%rsp), %r15           # 8-byte Reload
	jmp	.LBB2_21
	.align	16, 0x90
.LBB2_20:                               # %if.then37.1
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	$1, %rdx
	xorq	%rax, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, (%rsp)
	movabsq	$.L.str1, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	40(%rsp), %r9           # 8-byte Reload
	callq	printf
	xorq	%rdi, %rdi
	callq	terminate
	movq	104(%rsp), %r15
.LBB2_21:                               # %if.end64.1
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	%rbx, %rdi
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	%r14, %rdi
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	float64_add
	movq	%rax, %r14
	movq	%r14, 104(%rsp)
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%r12,%rax), %rdi
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	movq	40(%r12,%rax), %rbx
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	2320(%r12), %rsi
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	2328(%r12), %r15
	callq	float64_sub
	movq	%rax, %r13
	movq	%rbx, %rdi
	movq	%r15, %rbx
	movq	%rbx, %rsi
	callq	float64_sub
	movq	%rax, %r15
	movq	%r13, %rdi
	movabsq	$9223372036854775807, %rax # imm = 0x7FFFFFFFFFFFFFFF
	andq	%rax, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	__gtdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB2_24
# BB#22:                                # %lor.lhs.false.2
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	%r15, %rdi
	movabsq	$9223372036854775807, %rax # imm = 0x7FFFFFFFFFFFFFFF
	andq	%rax, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	__gtdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB2_24
# BB#23:                                # %lor.lhs.false.2
                                        #   in Loop: Header=BB2_9 Depth=1
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	je	.LBB2_25
	.align	16, 0x90
.LBB2_24:                               # %if.then37.2
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	$2, %rdx
	xorq	%rax, %rax
	movq	%rbx, (%rsp)
	movabsq	$.L.str1, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	40(%rsp), %r9           # 8-byte Reload
	callq	printf
	xorq	%rdi, %rdi
	callq	terminate
	movq	104(%rsp), %r14
.LBB2_25:                               # %if.end64.2
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	%r13, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	float64_add
	movq	%rax, 104(%rsp)
	movq	72(%rsp), %r14          # 8-byte Reload
	leaq	(%r12,%r14), %rdi
	callq	magsq_su3vec
	movq	96(%rsp), %rsi
	movq	%rax, %rdi
	callq	float64_add
	movq	%rax, 96(%rsp)
	movq	80(%rsp), %rax          # 8-byte Reload
	incq	%rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rax
	movslq	sites_on_node(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$3200, %r12             # imm = 0xC80
	cmpq	%rdx, %rax
	movabsq	$9223372036854775807, %rbx # imm = 0x7FFFFFFFFFFFFFFF
	jl	.LBB2_9
	jmp	.LBB2_2
.LBB2_1:                                # %for.end.thread
	movq	$0, 104(%rsp)
	movq	$0, 96(%rsp)
.LBB2_2:                                # %for.end78
	leaq	104(%rsp), %rdi
	callq	g_doublesum
	leaq	96(%rsp), %rdi
	callq	g_doublesum
	xorq	%rax, %rax
	callq	g_sync
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB2_4
# BB#3:                                 # %if.then81
	movq	104(%rsp), %rdi
	movq	96(%rsp), %rsi
	callq	float64_div
	movq	%rax, %rdi
	callq	sqrt
	movq	%rax, %rcx
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	printf
	movq	stdout(%rip), %rdi
	callq	fflush
.LBB2_4:                                # %if.end85
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp47:
	.size	check_invert, .Ltmp47-check_invert
	.cfi_endproc

	.type	.L.str1,@object         # @.str1
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str1:
	.asciz	"%d %d  ( %.4e , %.4e )  ( %.4e , %.4e )\n"
	.size	.L.str1, 41

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Inversion checked, frac. error = %e\n"
	.size	.L.str2, 37

	.type	.Lstr,@object           # @str
.Lstr:
	.asciz	"BOTCH"
	.size	.Lstr, 6


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
