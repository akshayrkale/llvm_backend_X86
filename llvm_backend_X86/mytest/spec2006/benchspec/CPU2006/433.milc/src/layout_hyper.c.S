	.file	"layout_hyper.c.bc"
	.text
	.globl	setup_layout
	.align	16, 0x90
	.type	setup_layout,@function
setup_layout:                           # @setup_layout
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
.Ltmp11:
	.cfi_offset %rbx, -48
.Ltmp12:
	.cfi_offset %r12, -40
.Ltmp13:
	.cfi_offset %r13, -32
.Ltmp14:
	.cfi_offset %r14, -24
.Ltmp15:
	.cfi_offset %r15, -16
	xorq	%rax, %rax
	callq	mynode
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	jne	.LBB0_2
# BB#1:                                 # %if.then
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	printf
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	callq	printf
	movq	$10, %rdi
	callq	putchar
.LBB0_2:                                # %if.end
	movl	nx(%rip), %eax
	movl	%eax, squaresize(%rip)
	movl	ny(%rip), %eax
	movl	%eax, squaresize+4(%rip)
	movl	nz(%rip), %eax
	movl	%eax, squaresize+8(%rip)
	movl	nt(%rip), %eax
	movl	%eax, squaresize+12(%rip)
	movq	$1, %r15
	movl	%r15d, nsquares+12(%rip)
	movl	%r15d, nsquares+8(%rip)
	movl	%r15d, nsquares+4(%rip)
	movl	%r15d, nsquares(%rip)
	xorq	%rax, %rax
	callq	numnodes
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB0_43
# BB#3:
	movabsq	$0, %r12
	movabsq	$1, %rax
	.align	16, 0x90
.LBB0_4:                                # %for.cond.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_29 Depth 2
                                        #     Child Loop BB0_36 Depth 2
	movl	squaresize(%rip), %r8d
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$1, %rdx
	movq	%r12, %rcx
	jle	.LBB0_6
# BB#5:                                 # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	%rax, %rcx
.LBB0_6:                                # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	testq	$1, %r8
	movq	%r12, %rdx
	jne	.LBB0_8
# BB#7:                                 # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	%rax, %rdx
.LBB0_8:                                # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	testq	%rdx, %rcx
	movq	%r8, %rsi
	jne	.LBB0_10
# BB#9:                                 # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	$1, %rsi
.LBB0_10:                               # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movl	squaresize+4(%rip), %ebx
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdi, %rdx
	movq	%r12, %rcx
	jle	.LBB0_12
# BB#11:                                # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	%rax, %rcx
.LBB0_12:                               # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	testq	$1, %rbx
	movq	%r12, %rdi
	jne	.LBB0_14
# BB#13:                                # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	%rax, %rdi
.LBB0_14:                               # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	testq	%rdi, %rcx
	jne	.LBB0_16
# BB#15:                                # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	%rsi, %rbx
.LBB0_16:                               # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movl	squaresize+8(%rip), %esi
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdx, %rdi
	movq	%r12, %rcx
	jle	.LBB0_18
# BB#17:                                # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	%rax, %rcx
.LBB0_18:                               # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	testq	$1, %rsi
	movq	%r12, %rdi
	jne	.LBB0_20
# BB#19:                                # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	%rax, %rdi
.LBB0_20:                               # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	testq	%rdi, %rcx
	jne	.LBB0_22
# BB#21:                                # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	%rbx, %rsi
.LBB0_22:                               # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movl	squaresize+12(%rip), %edi
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rdx, %rbx
	movq	%r12, %rcx
	jle	.LBB0_24
# BB#23:                                # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	%rax, %rcx
.LBB0_24:                               # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	testq	$1, %rdi
	movq	%r12, %rdx
	jne	.LBB0_26
# BB#25:                                # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	%rax, %rdx
.LBB0_26:                               # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	testq	%rdx, %rcx
	jne	.LBB0_28
# BB#27:                                # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	%rsi, %rdi
.LBB0_28:                               # %for.cond.preheader
                                        #   in Loop: Header=BB0_4 Depth=1
	xorq	%rdx, %rdx
	movq	%r8, %rcx
	jmp	.LBB0_29
	.align	16, 0x90
.LBB0_32:                               # %for.inc27.for.body17_crit_edge
                                        #   in Loop: Header=BB0_29 Depth=2
	movl	squaresize+4(,%rdx,4), %ecx
	movq	%rax, %rdx
.LBB0_29:                               # %for.body17
                                        #   Parent Loop BB0_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rdi, %rsi
	andq	%r13, %rsi
	andq	%r13, %rcx
	cmpq	%rsi, %rcx
	jne	.LBB0_31
# BB#30:                                # %land.lhs.true21
                                        #   in Loop: Header=BB0_29 Depth=2
	movslq	nsquares(,%rdx,4), %rcx
	cmpq	$1, %rcx
	jg	.LBB0_33
.LBB0_31:                               # %for.inc27
                                        #   in Loop: Header=BB0_29 Depth=2
	leaq	1(%rdx), %rax
	movq	$32, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	xorq	%r14, %r14
	cmpq	$3, %rbx
	jle	.LBB0_32
	jmp	.LBB0_36
	.align	16, 0x90
.LBB0_33:                               # %for.end29
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%r14, %r14
	cmpq	$3, %rax
	jg	.LBB0_36
	jmp	.LBB0_42
	.align	16, 0x90
.LBB0_35:                               # %for.cond32.for.body34_crit_edge
                                        #   in Loop: Header=BB0_36 Depth=2
	movl	squaresize+4(,%r14,4), %r8d
	movq	%rdx, %r14
.LBB0_36:                               # %for.body34
                                        #   Parent Loop BB0_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	%r13, %r8
	cmpq	%rsi, %r8
	je	.LBB0_37
# BB#34:                                # %for.cond32
                                        #   in Loop: Header=BB0_36 Depth=2
	leaq	1(%r14), %rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$3, %rax
	jle	.LBB0_35
# BB#38:                                # %if.then45.loopexit
                                        #   in Loop: Header=BB0_4 Depth=1
	incq	%r14
	jmp	.LBB0_39
	.align	16, 0x90
.LBB0_37:                               # %if.end43
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$3, %rax
	movq	%r14, %rdx
	jle	.LBB0_42
.LBB0_39:                               # %if.then45
                                        #   in Loop: Header=BB0_4 Depth=1
	xorq	%rax, %rax
	callq	mynode
	testq	%r13, %rax
	jne	.LBB0_41
# BB#40:                                # %if.then48
                                        #   in Loop: Header=BB0_4 Depth=1
	movabsq	$.Lstr5, %rdi
	callq	puts
.LBB0_41:                               # %if.end50
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	$1, %rdi
	callq	terminate
	movq	%r14, %rdx
.LBB0_42:                               # %if.end51
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	$33, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	squaresize(,%rdx,4), %eax
	addq	%r15, %r15
	movq	$31, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	addq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	nsquares(,%rdx,4), %eax
	movq	$1, %rcx
	shrq	%cl, %rsi
	movl	%esi, squaresize(,%rdx,4)
	addq	%rax, %rax
	movl	%eax, nsquares(,%rdx,4)
	xorq	%rax, %rax
	callq	numnodes
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rbx
	movabsq	$1, %rax
	jl	.LBB0_4
.LBB0_43:                               # %while.end
	movq	squaresize(%rip), %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movq	squaresize+8(%rip), %rsi
	imulq	%rax, %rdx
	imulq	%rsi, %rdx
	movq	$32, %rcx
	shrq	%cl, %rsi
	imulq	%rdx, %rsi
	movl	%esi, sites_on_node(%rip)
	xorq	%rax, %rax
	callq	mynode
	testq	%r13, %rax
	jne	.LBB0_46
# BB#44:                                # %if.then62
	movzbl	sites_on_node(%rip), %eax
	testq	$1, %rax
	je	.LBB0_46
# BB#45:                                # %if.then65
	movabsq	$.Lstr, %rdi
	callq	puts
	xorq	%rdi, %rdi
	callq	terminate
.LBB0_46:                               # %if.end68
	movl	sites_on_node(%rip), %eax
	movq	$31, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$33, %rcx
	sarq	%cl, %rdx
	movl	%edx, odd_sites_on_node(%rip)
	movl	%edx, even_sites_on_node(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp16:
	.size	setup_layout, .Ltmp16-setup_layout
	.cfi_endproc

	.globl	node_number
	.align	16, 0x90
	.type	node_number,@function
node_number:                            # @node_number
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rcx, %r8
	movq	%rdx, %r9
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	squaresize(%rip), %r11
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%r11, %r10
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	movq	%rdi, %rax
	cqto
	idivq	%r10
	movq	%rax, %r10
	movq	$32, %rcx
	sarq	%cl, %r11
	movq	%rsi, %rax
	cqto
	idivq	%r11
	movq	%rax, %r11
	movq	squaresize+8(%rip), %rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%r9, %rax
	cqto
	idivq	%rdi
	movq	%rax, %r9
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%r8, %rax
	cqto
	idivq	%rsi
	movq	nsquares(%rip), %rdi
	movl	nsquares+8(%rip), %edx
	movq	$32, %rcx
	movq	%rdi, %rsi
	shrq	%cl, %rsi
	imulq	%rax, %rdx
	addq	%r9, %rdx
	imulq	%rsi, %rdx
	addq	%r11, %rdx
	imulq	%rdi, %rdx
	addq	%r10, %rdx
	movq	%rdx, %rax
	retq
.Ltmp17:
	.size	node_number, .Ltmp17-node_number
	.cfi_endproc

	.globl	node_index
	.align	16, 0x90
	.type	node_index,@function
node_index:                             # @node_index
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
	movq	%rcx, %rax
	movq	%rax, -8(%rsp)          # 8-byte Spill
	movq	%rdx, %r9
	movq	$32, %rcx
	movq	%rax, %r10
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	movq	$32, %rcx
	movq	%r9, %r11
	shlq	%cl, %r11
	movq	$32, %rcx
	sarq	%cl, %r11
	movq	$32, %rcx
	movq	%rsi, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	squaresize(%rip), %r12
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cqto
	idivq	%rbx
	movq	%rdx, %r14
	movq	$32, %rcx
	movq	%r12, %r13
	shrq	%cl, %r13
	movq	$32, %rcx
	movq	%r12, %rbx
	sarq	%cl, %rbx
	movq	%r15, %rax
	cqto
	idivq	%rbx
	movq	%rdx, %r15
	movq	squaresize+8(%rip), %r8
	movq	$32, %rcx
	movq	%r8, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%r11, %rax
	cqto
	idivq	%rbx
	movq	%rdx, %r11
	movq	$32, %rcx
	movq	%r8, %rbx
	sarq	%cl, %rbx
	movq	%r10, %rax
	cqto
	idivq	%rbx
	imulq	%r8, %rdx
	addq	%r11, %rdx
	imulq	%r13, %rdx
	addq	%r15, %rdx
	imulq	%r12, %rdx
	addq	%r14, %rdx
	addq	%rdi, %rsi
	addq	%r9, %rsi
	movq	-8(%rsp), %rax          # 8-byte Reload
	addq	%rax, %rsi
	testq	$1, %rsi
	je	.LBB2_2
# BB#1:                                 # %if.else
	movl	sites_on_node(%rip), %eax
	addq	%rax, %rdx
.LBB2_2:                                # %return
	movabsq	$2147483648, %rax       # imm = 0x80000000
	andq	%rdx, %rax
	movq	$31, %rcx
	shrq	%cl, %rax
	addq	%rdx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$33, %rcx
	sarq	%cl, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp34:
	.size	node_index, .Ltmp34-node_index
	.cfi_endproc

	.globl	num_sites
	.align	16, 0x90
	.type	num_sites,@function
num_sites:                              # @num_sites
	.cfi_startproc
# BB#0:                                 # %entry
	movl	sites_on_node(%rip), %eax
	retq
.Ltmp35:
	.size	num_sites, .Ltmp35-num_sites
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"LAYOUT = Hypercubes, options = "
	.size	.L.str, 32

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"EVENFIRST,"
	.size	.L.str1, 11

	.type	squaresize,@object      # @squaresize
	.comm	squaresize,16,16
	.type	nsquares,@object        # @nsquares
	.comm	nsquares,16,16
	.type	.Lstr,@object           # @str
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lstr:
	.asciz	"SORRY, CAN'T LAY OUT THIS LATTICE"
	.size	.Lstr, 34

	.type	.Lstr5,@object          # @str5
	.align	16
.Lstr5:
	.asciz	"LAYOUT: Can't lay out this lattice, not enough factors of 2"
	.size	.Lstr5, 60


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
