	.file	"make_lattice.c.bc"
	.text
	.globl	make_lattice
	.align	16, 0x90
	.type	make_lattice,@function
make_lattice:                           # @make_lattice
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
	subq	$32, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 80
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
	movslq	sites_on_node(%rip), %rdi
	movq	$3200, %rsi             # imm = 0xC80
	callq	calloc
	testq	%rax, %rax
	movq	%rax, lattice(%rip)
	movq	$-128, %rbx
	jne	.LBB0_2
# BB#1:                                 # %if.then
	movl	this_node(%rip), %esi
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	printf
	movq	$1, %rdi
	callq	terminate
	.align	16, 0x90
.LBB0_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movslq	sites_on_node(%rip), %rdi
	movq	$8, %rsi
	callq	calloc
	testq	%rax, %rax
	movq	%rax, gen_pt+128(%rbx)
	jne	.LBB0_4
# BB#3:                                 # %if.then11
                                        #   in Loop: Header=BB0_2 Depth=1
	movl	this_node(%rip), %esi
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	callq	printf
	movq	$1, %rdi
	callq	terminate
.LBB0_4:                                # %for.inc
                                        #   in Loop: Header=BB0_2 Depth=1
	addq	$8, %rbx
	jne	.LBB0_2
# BB#5:                                 # %for.cond14.preheader
	movslq	nt(%rip), %rdx
	testq	%rdx, %rdx
	jle	.LBB0_22
# BB#6:                                 # %for.cond18.preheader.lr.ph
	movl	nz(%rip), %eax
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB0_7:                                # %for.cond18.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_9 Depth 2
                                        #       Child Loop BB0_11 Depth 3
                                        #         Child Loop BB0_13 Depth 4
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jle	.LBB0_21
# BB#8:                                 # %for.cond22.preheader.lr.ph
                                        #   in Loop: Header=BB0_7 Depth=1
	movl	ny(%rip), %edx
	xorq	%r15, %r15
	.align	16, 0x90
.LBB0_9:                                # %for.cond22.preheader
                                        #   Parent Loop BB0_7 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_11 Depth 3
                                        #         Child Loop BB0_13 Depth 4
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jle	.LBB0_19
# BB#10:                                # %for.cond26.preheader.lr.ph
                                        #   in Loop: Header=BB0_9 Depth=2
	movl	nx(%rip), %eax
	movq	24(%rsp), %rcx          # 8-byte Reload
	leaq	(%r15,%rcx), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	xorq	%r12, %r12
	.align	16, 0x90
.LBB0_11:                               # %for.cond26.preheader
                                        #   Parent Loop BB0_7 Depth=1
                                        #     Parent Loop BB0_9 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_13 Depth 4
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jle	.LBB0_17
# BB#12:                                # %for.body29.lr.ph
                                        #   in Loop: Header=BB0_11 Depth=3
	movq	8(%rsp), %rax           # 8-byte Reload
	leaq	(%rax,%r12), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	.align	16, 0x90
.LBB0_13:                               # %for.body29
                                        #   Parent Loop BB0_7 Depth=1
                                        #     Parent Loop BB0_9 Depth=2
                                        #       Parent Loop BB0_11 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	node_number
	movq	%rax, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, %r14
	andq	%r14, %rbx
	xorq	%rax, %rax
	callq	mynode
	andq	%r14, %rax
	cmpq	%rax, %rbx
	jne	.LBB0_15
# BB#14:                                # %if.then34
                                        #   in Loop: Header=BB0_13 Depth=4
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rcx
	callq	node_index
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	lattice(%rip), %rdi
	sarq	%cl, %rax
	imulq	$3200, %rax, %rax       # imm = 0xC80
	movw	%r13w, (%rdi,%rax)
	movw	%r12w, 2(%rdi,%rax)
	movw	%r15w, 4(%rdi,%rax)
	movw	%bx, 6(%rdi,%rax)
	movl	nx(%rip), %ecx
	movl	ny(%rip), %esi
	movl	nz(%rip), %edx
	imulq	%rbx, %rdx
	addq	%r15, %rdx
	imulq	%rsi, %rdx
	addq	%r12, %rdx
	imulq	%rcx, %rdx
	addq	%r13, %rdx
	movl	%edx, 12(%rdi,%rax)
	movq	16(%rsp), %rcx          # 8-byte Reload
	leaq	(%rcx,%r13), %rcx
	andq	$1, %rcx
	movq	$2, %rsi
	subq	%rcx, %rsi
	movb	%sil, 8(%rdi,%rax)
	movl	iseed(%rip), %esi
	leaq	16(%rdi,%rax), %rdi
	callq	initialize_prn
.LBB0_15:                               # %for.inc76
                                        #   in Loop: Header=BB0_13 Depth=4
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rdx
	movslq	nx(%rip), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB0_13
# BB#16:                                # %for.cond26.for.inc79_crit_edge
                                        #   in Loop: Header=BB0_11 Depth=3
	movl	ny(%rip), %edx
.LBB0_17:                               # %for.inc79
                                        #   in Loop: Header=BB0_11 Depth=3
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rsi
	jl	.LBB0_11
# BB#18:                                # %for.cond22.for.inc82_crit_edge
                                        #   in Loop: Header=BB0_9 Depth=2
	movl	nz(%rip), %eax
.LBB0_19:                               # %for.inc82
                                        #   in Loop: Header=BB0_9 Depth=2
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rsi
	jl	.LBB0_9
# BB#20:                                # %for.cond18.for.inc85_crit_edge
                                        #   in Loop: Header=BB0_7 Depth=1
	movl	nt(%rip), %edx
.LBB0_21:                               # %for.inc85
                                        #   in Loop: Header=BB0_7 Depth=1
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, %rsi
	incq	%rsi
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rsi
	jl	.LBB0_7
.LBB0_22:                               # %for.end87
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	make_lattice, .Ltmp17-make_lattice
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"NODE %d: no room for lattice\n"
	.size	.L.str, 30

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"NODE %d: no room for pointer vector\n"
	.size	.L.str1, 37


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
