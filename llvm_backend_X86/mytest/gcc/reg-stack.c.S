	.file	"reg-stack.c.bc"
	.text
	.globl	stack_regs_mentioned
	.align	16, 0x90
	.type	stack_regs_mentioned,@function
stack_regs_mentioned:                   # @stack_regs_mentioned
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
	movq	%rdi, %r15
	movzbl	(%r15), %eax
	movzbl	rtx_class(%rax), %ecx
	xorq	%rax, %rax
	cmpq	$105, %rcx
	jne	.LBB0_12
# BB#1:                                 # %entry
	movq	stack_regs_mentioned_data(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB0_12
# BB#2:                                 # %if.end
	movl	8(%r15), %r14d
	movl	(%rbx), %eax
	cmpq	%rax, %r14
	jb	.LBB0_4
# BB#3:                                 # %if.then6
	movabsq	$-3689348814741910323, %rcx # imm = 0xCCCCCCCCCCCCCCCD
	movq	%r14, %rax
	mulq	%rcx
	movq	$4, %rcx
	shrq	%cl, %rdx
	movq	%r14, %rax
	addq	%rdx, %rax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	movq	%rbx, %rdi
	callq	varray_grow
	movq	%rax, %rbx
	movq	%rbx, stack_regs_mentioned_data(%rip)
.LBB0_4:                                # %if.end8
	movzbl	32(%rbx,%r14), %edx
	movq	$56, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rdx, %rdx
	jne	.LBB0_9
# BB#5:                                 # %if.then14
	movq	32(%r15), %rdi
	callq	stack_regs_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_6
# BB#7:                                 # %if.then14
	movabsq	$1, %rax
	jmp	.LBB0_8
.LBB0_6:
	movabsq	$0, %rax
.LBB0_8:                                # %if.then14
	xorq	$1, %rax
	incq	%rax
	movb	%al, 32(%rbx,%r14)
.LBB0_9:                                # %if.end24
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$1, %rcx
	jne	.LBB0_10
# BB#11:                                # %if.end24
	movabsq	$1, %rax
	jmp	.LBB0_12
.LBB0_10:
	movabsq	$0, %rax
.LBB0_12:                               # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	stack_regs_mentioned, .Ltmp10-stack_regs_mentioned
	.cfi_endproc

	.align	16, 0x90
	.type	stack_regs_mentioned_p,@function
stack_regs_mentioned_p:                 # @stack_regs_mentioned_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp17:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp18:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp19:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp20:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp21:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp22:
	.cfi_def_cfa_offset 64
.Ltmp23:
	.cfi_offset %rbx, -48
.Ltmp24:
	.cfi_offset %r12, -40
.Ltmp25:
	.cfi_offset %r13, -32
.Ltmp26:
	.cfi_offset %r14, -24
.Ltmp27:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movzbl	(%r14), %ecx
	cmpq	$61, %rcx
	jne	.LBB1_2
# BB#1:                                 # %land.lhs.true
	movl	8(%r14), %eax
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rax, %rdx
	cmpq	$8, %rdx
	movq	$1, %rax
	je	.LBB1_13
.LBB1_2:                                # %if.end
	movzbl	rtx_length(%rcx), %r12d
	testq	%r12, %r12
	je	.LBB1_3
# BB#4:                                 # %for.body.lr.ph
	movq	rtx_format(,%rcx,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB1_5:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_10 Depth 2
	movq	%r12, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movzbl	-1(%rcx,%rax), %ecx
	leaq	-1(%rax), %r12
	cmpq	$69, %rcx
	jne	.LBB1_6
# BB#9:                                 # %if.then21
                                        #   in Loop: Header=BB1_5 Depth=1
	movq	(%r14,%rax,8), %rbx
	movslq	(%rbx), %r15
	.align	16, 0x90
.LBB1_10:                               # %for.cond26
                                        #   Parent Loop BB1_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB1_7
# BB#11:                                # %for.body29
                                        #   in Loop: Header=BB1_10 Depth=2
	movq	(%rbx,%r15,8), %rdi
	decq	%r15
	callq	stack_regs_mentioned_p
	testq	%r13, %rax
	movq	$1, %rax
	je	.LBB1_10
	jmp	.LBB1_13
	.align	16, 0x90
.LBB1_6:                                # %for.body
                                        #   in Loop: Header=BB1_5 Depth=1
	cmpq	$101, %rcx
	jne	.LBB1_7
# BB#12:                                # %land.lhs.true43
                                        #   in Loop: Header=BB1_5 Depth=1
	movq	(%r14,%rax,8), %rdi
	callq	stack_regs_mentioned_p
	testq	%r13, %rax
	movq	$1, %rax
	jne	.LBB1_13
.LBB1_7:                                # %for.cond.backedge
                                        #   in Loop: Header=BB1_5 Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB1_5
# BB#8:
	xorq	%rax, %rax
	jmp	.LBB1_13
.LBB1_3:
	xorq	%rax, %rax
.LBB1_13:                               # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp28:
	.size	stack_regs_mentioned_p, .Ltmp28-stack_regs_mentioned_p
	.cfi_endproc

	.globl	reg_to_stack
	.align	16, 0x90
	.type	reg_to_stack,@function
reg_to_stack:                           # @reg_to_stack
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp35:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp36:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp37:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp38:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp39:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp40:
	.cfi_def_cfa_offset 64
.Ltmp41:
	.cfi_offset %rbx, -48
.Ltmp42:
	.cfi_offset %r12, -40
.Ltmp43:
	.cfi_offset %r13, -32
.Ltmp44:
	.cfi_offset %r14, -24
.Ltmp45:
	.cfi_offset %r15, -16
	movq	stack_regs_mentioned_data(%rip), %rax
	movq	%rsi, %r15
	movq	%rdi, %r14
	testq	%rax, %rax
	je	.LBB2_2
# BB#1:                                 # %if.then2
	movq	%rax, %rdi
	callq	free
	movq	$0, stack_regs_mentioned_data(%rip)
.LBB2_2:                                # %if.end3
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	movq	$8, %rbx
	jne	.LBB2_5
# BB#3:                                 # %if.then5
	xorq	%rdi, %rdi
	callq	split_all_insns
	.align	16, 0x90
.LBB2_5:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	regs_ever_live(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB2_6
# BB#4:                                 # %for.cond
                                        #   in Loop: Header=BB2_5 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$15, %rax
	jle	.LBB2_5
	jmp	.LBB2_89
.LBB2_6:                                # %for.end
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	$15, %rbx
	jle	.LBB2_7
.LBB2_89:                               # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_7:                                # %if.end12
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	jne	.LBB2_9
# BB#8:                                 # %if.then14
	callq	max_reg_num
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	%r15, %rdx
	callq	find_basic_blocks
	xorq	%rdi, %rdi
	movq	$1, %rsi
	callq	count_or_remove_death_notes
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	life_analysis
.LBB2_9:                                # %if.end16
	callq	mark_dfs_back_edges
	movq	$64, %rdi
	callq	alloc_aux_for_blocks
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB2_14
# BB#10:                                # %for.body20.lr.ph
	movq	basic_block_info(%rip), %r8
	movabsq	$entry_exit_blocks, %rsi
	.align	16, 0x90
.LBB2_11:                               # %for.body20
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_12 Depth 2
	movq	24(%r8,%rax,8), %rcx
	movq	32(%rcx), %rdi
	jmp	.LBB2_12
	.align	16, 0x90
.LBB2_27:                               # %for.inc32
                                        #   in Loop: Header=BB2_12 Depth=2
	movq	(%rdi), %rdi
.LBB2_12:                               # %for.body20
                                        #   Parent Loop BB2_11 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rdi, %rdi
	je	.LBB2_13
# BB#24:                                # %for.body26
                                        #   in Loop: Header=BB2_12 Depth=2
	movzbl	48(%rdi), %ebx
	testq	$32, %rbx
	jne	.LBB2_27
# BB#25:                                # %land.lhs.true
                                        #   in Loop: Header=BB2_12 Depth=2
	movq	16(%rdi), %rbx
	cmpq	%rsi, %rbx
	je	.LBB2_27
# BB#26:                                # %if.then29
                                        #   in Loop: Header=BB2_12 Depth=2
	movq	80(%rcx), %rbx
	movl	60(%rbx), %edx
	incq	%rdx
	movl	%edx, 60(%rbx)
	jmp	.LBB2_27
	.align	16, 0x90
.LBB2_13:                               # %for.cond18.loopexit
                                        #   in Loop: Header=BB2_11 Depth=1
	decq	%rax
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	jg	.LBB2_11
.LBB2_14:                               # %for.cond36.preheader
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movl	class_narrowest_mode+8(%rip), %r13d
	movl	class_narrowest_mode+24(%rip), %r14d
	movq	$8, %r15
	.align	16, 0x90
.LBB2_15:                               # %for.body38
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_17 Depth 2
                                        #     Child Loop BB2_20 Depth 2
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	je	.LBB2_18
# BB#16:                                # %for.body41.lr.ph
                                        #   in Loop: Header=BB2_15 Depth=1
	leaq	-8(%r15), %r12
	movq	%r13, %rbx
	.align	16, 0x90
.LBB2_17:                               # %for.body41
                                        #   Parent Loop BB2_15 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_REG
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	imulq	$472, %r12, %rcx        # imm = 0x1D8
	movq	%rax, FP_mode_reg(%rcx,%rbx,8)
	movzbl	mode_wider_mode(%rbx), %ebx
	testq	%rbx, %rbx
	jne	.LBB2_17
.LBB2_18:                               # %for.end51
                                        #   in Loop: Header=BB2_15 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	je	.LBB2_21
# BB#19:                                # %for.body55.lr.ph
                                        #   in Loop: Header=BB2_15 Depth=1
	leaq	-8(%r15), %r12
	movq	%r14, %rbx
	.align	16, 0x90
.LBB2_20:                               # %for.body55
                                        #   Parent Loop BB2_15 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_REG
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	imulq	$472, %r12, %rcx        # imm = 0x1D8
	movq	%rax, FP_mode_reg(%rcx,%rbx,8)
	movzbl	mode_wider_mode(%rbx), %ebx
	testq	%rbx, %rbx
	jne	.LBB2_20
.LBB2_21:                               # %for.inc67
                                        #   in Loop: Header=BB2_15 Depth=1
	incq	%r15
	cmpq	$16, %r15
	jne	.LBB2_15
# BB#22:                                # %for.end69
	movq	$52, %rdi
	movq	$17, %rsi
	callq	gen_rtx_REG
	movl	flag_pic(%rip), %ecx
	movq	%rax, ix86_flags_rtx(%rip)
	testq	%rcx, %rcx
	je	.LBB2_28
# BB#23:                                # %if.then72
	movq	const_tiny_rtx+120(%rip), %rax
	jmp	.LBB2_29
.LBB2_28:                               # %if.else
	xorq	%rdi, %rdi
	movq	$2143289344, %rsi       # imm = 0x7FC00000
	callq	gen_rtx_CONST_INT
	movq	$15, %rdi
	movq	%rax, %rsi
	callq	gen_lowpart
	movq	%rax, nan(%rip)
	movq	$15, %rdi
	movq	%rax, %rsi
	callq	force_const_mem
.LBB2_29:                               # %if.end76
	movq	%rax, nan(%rip)
	callq	get_max_uid
	leaq	1(%rax), %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movabsq	$.L.str, %rdx
	movq	$1, %rsi
	callq	varray_init
	movslq	n_basic_blocks(%rip), %r14
	movq	%rax, stack_regs_mentioned_data(%rip)
	testq	%r14, %r14
	jle	.LBB2_38
# BB#30:
	movabsq	$4294967294, %r15       # imm = 0xFFFFFFFE
	.align	16, 0x90
.LBB2_31:                               # %for.body.i.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_32 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	24(%rax,%r14,8), %r12
	movq	80(%r12), %r13
	decq	%r14
	movl	%r15d, (%r13)
	movq	$8, %rbx
	.align	16, 0x90
.LBB2_32:                               # %for.body3.i.i
                                        #   Parent Loop BB2_31 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	72(%r12), %rdi
	movq	%rbx, %rsi
	callq	bitmap_bit_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_34
# BB#33:                                # %if.then.i.i
                                        #   in Loop: Header=BB2_32 Depth=2
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	orq	%rax, 48(%r13)
.LBB2_34:                               # %if.end.i.i
                                        #   in Loop: Header=BB2_32 Depth=2
	movq	64(%r12), %rdi
	movq	%rbx, %rsi
	callq	bitmap_bit_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_36
# BB#35:                                # %if.then6.i.i
                                        #   in Loop: Header=BB2_32 Depth=2
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	orq	%rax, 8(%r13)
.LBB2_36:                               # %for.inc.i.i
                                        #   in Loop: Header=BB2_32 Depth=2
	incq	%rbx
	cmpq	$16, %rbx
	jne	.LBB2_32
# BB#37:                                # %for.cond.loopexit.i.i
                                        #   in Loop: Header=BB2_31 Depth=1
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB2_31
.LBB2_38:                               # %for.cond14.preheader.i.i
	movq	entry_exit_blocks+40(%rip), %r15
	xorq	%rsi, %rsi
	jmp	.LBB2_39
	.align	16, 0x90
.LBB2_44:                               # %for.end40.i.i
                                        #   in Loop: Header=BB2_39 Depth=1
	movl	%r12d, (%rbx)
	movq	8(%r15), %r15
.LBB2_39:                               # %for.cond14.preheader.i.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_41 Depth 2
	testq	%r15, %r15
	je	.LBB2_45
# BB#40:                                # %for.body16.i.i
                                        #   in Loop: Header=BB2_39 Depth=1
	movq	24(%r15), %rax
	movq	80(%rax), %rbx
	movq	$15, %r14
	movq	$14, %r13
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB2_41:                               # %for.body24.i.i
                                        #   Parent Loop BB2_39 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx), %rax
	leaq	1(%r13), %rcx
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB2_43
# BB#42:                                # %if.then30.i.i
                                        #   in Loop: Header=BB2_41 Depth=2
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movb	%r14b, 16(%rax,%rbx)
	movq	FP_mode_reg+120(%rip), %rdx
	movq	nan(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	insert_insn_on_edge
	movq	$1, %rsi
.LBB2_43:                               # %for.inc38.i.i
                                        #   in Loop: Header=BB2_41 Depth=2
	decq	%r14
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%r13
	cmpq	$7, %rax
	jg	.LBB2_41
	jmp	.LBB2_44
.LBB2_45:                               # %convert_regs_entry.exit.i
	movq	%rsi, %r14
	movq	current_function_decl(%rip), %rbx
	movq	96(%rbx), %rdi
	callq	aggregate_value_p
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	jne	.LBB2_90
# BB#46:                                # %if.end.i.i.i
	movq	96(%rbx), %rax
	cmpq	$0, 144(%rax)
	je	.LBB2_90
# BB#47:                                # %if.end30.i.i.i
	movq	8(%rax), %rdi
	callq	ix86_function_value
	testq	%rax, %rax
	je	.LBB2_90
# BB#48:                                # %land.lhs.true.i.i.i
	movl	(%rax), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB2_90
# BB#49:                                # %land.lhs.true33.i.i.i
	movl	8(%rax), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jne	.LBB2_90
# BB#50:                                # %cond.true.i.i
	movq	$14, %rcx
	shrq	%cl, %rsi
	andq	$1020, %rsi             # imm = 0x3FC
	movl	mode_class(%rsi), %ecx
	cmpq	$5, %rcx
	movq	$2, %r8
	movq	8(%rsp), %r12           # 8-byte Reload
	je	.LBB2_55
# BB#51:                                # %lor.rhs.i.i
	cmpq	$6, %rcx
	jne	.LBB2_52
# BB#53:                                # %lor.rhs.i.i
	movabsq	$1, %r8
	jmp	.LBB2_54
.LBB2_90:                               # %if.end.thread.i.i
	movq	entry_exit_blocks+192(%rip), %rdi
	movq	8(%rsp), %r12           # 8-byte Reload
	movl	%r15d, (%rdi)
.LBB2_60:                               # %convert_regs_exit.exit.i
	movq	$1, %rax
	movl	%eax, 56(%rdi)
	movq	entry_exit_blocks+40(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB2_63
# BB#61:
	movq	%r14, %r13
	.align	16, 0x90
.LBB2_62:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rsi
	movq	%r12, %rdi
	callq	convert_regs_2
	movq	8(%rbx), %rbx
	movq	%rax, %r14
	orq	%r13, %r14
	testq	%rbx, %rbx
	movq	%r14, %r13
	jne	.LBB2_62
.LBB2_63:                               # %for.cond2.preheader.i
	movq	%r12, %rbx
	movslq	n_basic_blocks(%rip), %rdx
	testq	%rdx, %rdx
	jle	.LBB2_84
# BB#64:
	movq	$4, %r12
	movq	$15, %r13
	.align	16, 0x90
.LBB2_65:                               # %for.body3.i
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	(%rax,%r12,8), %rsi
	movq	80(%rsi), %rax
	movl	56(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB2_83
# BB#66:                                # %if.then.i
                                        #   in Loop: Header=BB2_65 Depth=1
	movl	%r15d, (%rax)
	movq	8(%rax), %rcx
	xorq	%rdx, %rdx
	testq	$32768, %rcx            # imm = 0x8000
	je	.LBB2_68
# BB#67:                                # %if.then11.i
                                        #   in Loop: Header=BB2_65 Depth=1
	xorq	%rdx, %rdx
	movl	%edx, (%rax)
	movb	%r13b, 16(%rax)
	movq	$1, %rdx
.LBB2_68:                               # %for.inc18.i
                                        #   in Loop: Header=BB2_65 Depth=1
	testq	$16384, %rcx            # imm = 0x4000
	je	.LBB2_70
# BB#69:                                # %if.then11.1.i
                                        #   in Loop: Header=BB2_65 Depth=1
	movl	%edx, (%rax)
	movq	$14, %rcx
	movb	%cl, 16(%rax,%rdx)
	movq	8(%rax), %rcx
.LBB2_70:                               # %for.inc18.1.i
                                        #   in Loop: Header=BB2_65 Depth=1
	testq	$8192, %rcx             # imm = 0x2000
	je	.LBB2_72
# BB#71:                                # %if.then11.2.i
                                        #   in Loop: Header=BB2_65 Depth=1
	movl	(%rax), %edx
	incq	%rdx
	movl	%edx, (%rax)
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$13, %rcx
	movb	%cl, 16(%rdx,%rax)
	movq	8(%rax), %rcx
.LBB2_72:                               # %for.inc18.2.i
                                        #   in Loop: Header=BB2_65 Depth=1
	testq	$4096, %rcx             # imm = 0x1000
	je	.LBB2_74
# BB#73:                                # %if.then11.3.i
                                        #   in Loop: Header=BB2_65 Depth=1
	movl	(%rax), %edx
	incq	%rdx
	movl	%edx, (%rax)
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$12, %rcx
	movb	%cl, 16(%rdx,%rax)
	movq	8(%rax), %rcx
.LBB2_74:                               # %for.inc18.3.i
                                        #   in Loop: Header=BB2_65 Depth=1
	testq	$2048, %rcx             # imm = 0x800
	je	.LBB2_76
# BB#75:                                # %if.then11.4.i
                                        #   in Loop: Header=BB2_65 Depth=1
	movl	(%rax), %edx
	incq	%rdx
	movl	%edx, (%rax)
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$11, %rcx
	movb	%cl, 16(%rdx,%rax)
	movq	8(%rax), %rcx
.LBB2_76:                               # %for.inc18.4.i
                                        #   in Loop: Header=BB2_65 Depth=1
	testq	$1024, %rcx             # imm = 0x400
	je	.LBB2_78
# BB#77:                                # %if.then11.5.i
                                        #   in Loop: Header=BB2_65 Depth=1
	movl	(%rax), %edx
	incq	%rdx
	movl	%edx, (%rax)
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$10, %rcx
	movb	%cl, 16(%rdx,%rax)
	movq	8(%rax), %rcx
.LBB2_78:                               # %for.inc18.5.i
                                        #   in Loop: Header=BB2_65 Depth=1
	testq	$512, %rcx              # imm = 0x200
	je	.LBB2_80
# BB#79:                                # %if.then11.6.i
                                        #   in Loop: Header=BB2_65 Depth=1
	movl	(%rax), %edx
	incq	%rdx
	movl	%edx, (%rax)
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$9, %rcx
	movb	%cl, 16(%rdx,%rax)
	movq	8(%rax), %rcx
.LBB2_80:                               # %for.inc18.6.i
                                        #   in Loop: Header=BB2_65 Depth=1
	testq	$256, %rcx              # imm = 0x100
	je	.LBB2_82
# BB#81:                                # %if.then11.7.i
                                        #   in Loop: Header=BB2_65 Depth=1
	movl	(%rax), %edx
	incq	%rdx
	movl	%edx, (%rax)
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$8, %rcx
	movb	%cl, 16(%rdx,%rax)
.LBB2_82:                               # %for.inc18.7.i
                                        #   in Loop: Header=BB2_65 Depth=1
	movq	%rbx, %rdi
	callq	convert_regs_2
	movl	n_basic_blocks(%rip), %edx
	orq	%rax, %r14
.LBB2_83:                               # %for.inc23.i
                                        #   in Loop: Header=BB2_65 Depth=1
	leaq	-3(%r12), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r12
	cmpq	%rsi, %rax
	jl	.LBB2_65
.LBB2_84:                               # %for.end25.i
	callq	fixup_abnormal_edges
	testq	%r15, %r14
	je	.LBB2_86
# BB#85:                                # %if.then27.i
	callq	commit_edge_insertions
.LBB2_86:                               # %if.end28.i
	movq	%rbx, %rsi
	testq	%rsi, %rsi
	je	.LBB2_88
# BB#87:                                # %if.then30.i
	movq	$10, %rdi
	callq	fputc
.LBB2_88:                               # %convert_regs.exit
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free_aux_for_blocks  # TAILCALL
.LBB2_52:
	movabsq	$0, %r8
.LBB2_54:                               # %lor.rhs.i.i
	incq	%r8
.LBB2_55:                               # %if.end.i1.i
	movq	entry_exit_blocks+192(%rip), %rdi
	cmpq	%r15, %rax
	je	.LBB2_56
# BB#57:                                # %if.else.i.i
	leaq	-1(%r8,%rax), %rsi
	addq	%rax, %r8
	movq	%rsi, %rcx
	subq	%rax, %rcx
	movl	%ecx, (%rdi)
	incq	%rdx
.LBB2_58:                               # %for.body.i7.i
                                        # =>This Inner Loop Header: Depth=1
	leaq	-1(%rdx), %rax
	movq	%rsi, %rbx
	subq	%rax, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movb	%al, 16(%rbx,%rdi)
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	orq	%rbx, 8(%rdi)
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r8, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	incq	%rdx
	cmpq	%rax, %rbx
	jg	.LBB2_58
# BB#59:                                # %convert_regs_exit.exit.loopexit.i
	movq	entry_exit_blocks+192(%rip), %rdi
	jmp	.LBB2_60
.LBB2_56:                               # %if.then58.i.i
	movl	%r15d, (%rdi)
	jmp	.LBB2_60
.Ltmp46:
	.size	reg_to_stack, .Ltmp46-reg_to_stack
	.cfi_endproc

	.align	16, 0x90
	.type	convert_regs_2,@function
convert_regs_2:                         # @convert_regs_2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp53:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp54:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp55:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp56:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp57:
	.cfi_def_cfa_offset 48
	subq	$112, %rsp
.Ltmp58:
	.cfi_def_cfa_offset 160
.Ltmp59:
	.cfi_offset %rbx, -48
.Ltmp60:
	.cfi_offset %r12, -40
.Ltmp61:
	.cfi_offset %r13, -32
.Ltmp62:
	.cfi_offset %r14, -24
.Ltmp63:
	.cfi_offset %r15, -16
	movslq	n_basic_blocks(%rip), %rax
	movq	%rsi, %rbx
	movq	%rdi, %r12
	movq	%r12, 80(%rsp)          # 8-byte Spill
	movq	$3, %rcx
	shlq	%cl, %rax
	movq	%rax, %rdi
	callq	xmalloc
	movq	%rax, 24(%rsp)          # 8-byte Spill
	leaq	8(%rax), %rcx
	movq	%rbx, (%rax)
	movq	%rcx, %rax
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB3_1:                                # %do.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_2 Depth 2
                                        #     Child Loop BB3_27 Depth 2
                                        #     Child Loop BB3_71 Depth 2
                                        #     Child Loop BB3_67 Depth 2
                                        #     Child Loop BB3_85 Depth 2
                                        #     Child Loop BB3_80 Depth 2
                                        #     Child Loop BB3_95 Depth 2
	movq	%rdx, %r14
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	-8(%rax), %r13
	movq	32(%r13), %rdi
	movq	80(%r13), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	testq	%rdi, %rdi
	movabsq	$0, %r15
	je	.LBB3_22
	.align	16, 0x90
.LBB3_2:                                # %for.body.i
                                        #   Parent Loop BB3_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	48(%rdi), %eax
	testq	$32, %rax
	jne	.LBB3_20
# BB#3:                                 # %if.else.i
                                        #   in Loop: Header=BB3_2 Depth=2
	testq	%r15, %r15
	movq	%rdi, %rax
	je	.LBB3_21
# BB#4:                                 # %if.else4.i
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	16(%r15), %r8
	movl	104(%r8), %ecx
	movl	52(%r15), %eax
	imulq	%rcx, %rax
	addq	$5000, %rax             # imm = 0x1388
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$3777893186295716171, %rcx # imm = 0x346DC5D63886594B
	movq	%rcx, %r10
	imulq	%r10
	movq	%rdx, %rsi
	movq	$63, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	$11, %rcx
	sarq	%cl, %rsi
	addq	%rax, %rsi
	movq	$32, %rcx
	movq	%rsi, %rbx
	movq	16(%rdi), %r9
	shlq	%cl, %rbx
	movq	$32, %rcx
	movl	104(%r9), %edx
	movl	52(%rdi), %eax
	sarq	%cl, %rbx
	imulq	%rdx, %rax
	addq	$5000, %rax             # imm = 0x1388
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	%r10
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$11, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rbx
	movq	%rdi, %rax
	jl	.LBB3_21
# BB#5:                                 # %if.else12.i
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jg	.LBB3_20
# BB#6:                                 # %if.else27.i
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	56(%r15), %rax
	movq	56(%rdi), %rcx
	cmpq	%rcx, %rax
	movq	%rdi, %rax
	jl	.LBB3_21
# BB#7:                                 # %if.else31.i
                                        #   in Loop: Header=BB3_2 Depth=2
	jg	.LBB3_20
# BB#8:                                 # %if.else36.i
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	40(%r9), %rax
	movq	8(%rax), %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB3_11
# BB#9:                                 # %land.rhs.i
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	24(%rdi), %rcx
	movq	32(%rcx), %rcx
	cmpq	$0, (%rcx)
	movabsq	$0, %rcx
	je	.LBB3_11
# BB#10:                                # %land.rhs.i
                                        #   in Loop: Header=BB3_2 Depth=2
	movabsq	$1, %rcx
.LBB3_11:                               # %land.end.i
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	40(%r8), %rdx
	cmpq	$0, 8(%rdx)
	jne	.LBB3_14
# BB#12:                                # %land.end.land.end51_crit_edge.i
                                        #   in Loop: Header=BB3_2 Depth=2
	testq	%rcx, %rcx
	jne	.LBB3_17
	jmp	.LBB3_13
.LBB3_14:                               # %land.end51.i
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	24(%r15), %rdx
	movq	32(%rdx), %rdx
	cmpq	$0, (%rdx)
	movabsq	$0, %rdx
	je	.LBB3_16
# BB#15:                                # %land.end51.i
                                        #   in Loop: Header=BB3_2 Depth=2
	movabsq	$1, %rdx
.LBB3_16:                               # %land.end51.i
                                        #   in Loop: Header=BB3_2 Depth=2
	xorq	%rdx, %rcx
	cmpq	$1, %rcx
	jne	.LBB3_13
.LBB3_17:                               # %if.then57.i
                                        #   in Loop: Header=BB3_2 Depth=2
	testq	%rax, %rax
	je	.LBB3_20
# BB#18:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	24(%rdi), %rax
	movq	32(%rax), %rax
	cmpq	$0, (%rax)
	je	.LBB3_20
# BB#19:                                # %select.mid
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	%rdi, %r15
	jmp	.LBB3_20
.LBB3_13:                               # %if.else67.i
                                        #   in Loop: Header=BB3_2 Depth=2
	movslq	88(%r9), %rax
	movslq	88(%r8), %rcx
	cmpq	%rcx, %rax
	movq	%rdi, %rax
	jl	.LBB3_21
	.align	16, 0x90
.LBB3_20:                               # %select.end
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	%r15, %rax
.LBB3_21:                               # %for.inc.i
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	movq	%rax, %r15
	jne	.LBB3_2
.LBB3_22:                               # %for.end.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %eax
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	cmpq	%rcx, %rax
	movabsq	$0, %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	jne	.LBB3_24
# BB#23:                                # %if.then85.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	compensate_edge
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	%r15, 48(%rsp)          # 8-byte Spill
.LBB3_24:                               # %if.end88.i
                                        #   in Loop: Header=BB3_1 Depth=1
	testq	%r12, %r12
	movq	%r13, current_block(%rip)
	je	.LBB3_26
# BB#25:                                # %if.then90.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movl	88(%r13), %edx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str1, %rsi
	callq	fprintf
	movq	%r12, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	print_stack
.LBB3_26:                               # %if.end94.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	%r14, 40(%rsp)          # 8-byte Spill
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx), %rax
	movq	(%r13), %rbx
	movq	%rax, 104(%rsp)
	movq	(%rcx), %rax
	movq	8(%rcx), %rcx
	addq	$-8, 64(%rsp)           # 8-byte Folded Spill
	movq	%rcx, 96(%rsp)
	movq	%rax, 88(%rsp)
	.align	16, 0x90
.LBB3_27:                               # %do.body.i
                                        #   Parent Loop BB3_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rbx), %rax
	testq	%rax, %rax
	je	.LBB3_102
# BB#28:                                # %if.end99.i
                                        #   in Loop: Header=BB3_27 Depth=2
	movq	8(%r13), %rcx
	xorq	%r14, %r14
	cmpq	%rcx, %rbx
	je	.LBB3_30
# BB#29:                                # %select.mid16
                                        #   in Loop: Header=BB3_27 Depth=2
	movq	%rax, %r14
.LBB3_30:                               # %select.end15
                                        #   in Loop: Header=BB3_27 Depth=2
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB3_40
# BB#31:                                # %select.end15
                                        #   in Loop: Header=BB3_27 Depth=2
	movq	stack_regs_mentioned_data(%rip), %r15
	testq	%r15, %r15
	je	.LBB3_40
# BB#32:                                # %if.end.i.i
                                        #   in Loop: Header=BB3_27 Depth=2
	movq	%r13, %r12
	movl	8(%rbx), %r13d
	movl	(%r15), %eax
	cmpq	%rax, %r13
	jb	.LBB3_34
# BB#33:                                # %if.then6.i.i
                                        #   in Loop: Header=BB3_27 Depth=2
	movq	%r13, %rax
	movabsq	$-3689348814741910323, %rcx # imm = 0xCCCCCCCCCCCCCCCD
	mulq	%rcx
	movq	$4, %rcx
	shrq	%cl, %rdx
	movq	%r13, %rsi
	addq	%rdx, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	movq	%r15, %rdi
	callq	varray_grow
	movq	%rax, %r15
	movq	%r15, stack_regs_mentioned_data(%rip)
.LBB3_34:                               # %if.end8.i.i
                                        #   in Loop: Header=BB3_27 Depth=2
	movzbl	32(%r15,%r13), %edx
	movq	$56, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rdx, %rdx
	jne	.LBB3_38
# BB#35:                                # %if.then14.i.i
                                        #   in Loop: Header=BB3_27 Depth=2
	movq	32(%rbx), %rdi
	callq	stack_regs_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$0, %rax
	je	.LBB3_37
# BB#36:                                # %if.then14.i.i
                                        #   in Loop: Header=BB3_27 Depth=2
	movabsq	$1, %rax
.LBB3_37:                               # %if.then14.i.i
                                        #   in Loop: Header=BB3_27 Depth=2
	xorq	$1, %rax
	incq	%rax
	movb	%al, 32(%r15,%r13)
.LBB3_38:                               # %stack_regs_mentioned.exit.i
                                        #   in Loop: Header=BB3_27 Depth=2
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	movq	%r12, %r13
	movq	80(%rsp), %r12          # 8-byte Reload
	je	.LBB3_41
# BB#39:                                # %stack_regs_mentioned.exit.lor.lhs.false_crit_edge.i
                                        #   in Loop: Header=BB3_27 Depth=2
	movl	(%rbx), %eax
.LBB3_40:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB3_27 Depth=2
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$34, %rax
	jne	.LBB3_44
.LBB3_41:                               # %if.then108.i
                                        #   in Loop: Header=BB3_27 Depth=2
	testq	%r12, %r12
	je	.LBB3_43
# BB#42:                                # %if.then110.i
                                        #   in Loop: Header=BB3_27 Depth=2
	movl	8(%rbx), %edx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str3, %rsi
	callq	fprintf
	movq	%r12, %rdi
	leaq	88(%rsp), %rsi
	callq	print_stack
.LBB3_43:                               # %if.end114.i
                                        #   in Loop: Header=BB3_27 Depth=2
	movq	%rbx, %rdi
	leaq	88(%rsp), %rsi
	callq	subst_stack_regs
.LBB3_44:                               # %do.cond.i
                                        #   in Loop: Header=BB3_27 Depth=2
	testq	%r14, %r14
	movq	%r14, %rbx
	jne	.LBB3_27
# BB#45:                                # %do.end.i
                                        #   in Loop: Header=BB3_1 Depth=1
	testq	%r12, %r12
	movq	56(%rsp), %r14          # 8-byte Reload
	je	.LBB3_63
# BB#46:                                # %if.then118.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	$25, %rsi
	movq	$1, %rdx
	movabsq	$.L.str4, %rdi
	movq	%r12, %rcx
	callq	fwrite
	movq	48(%r14), %rax
	testq	$256, %rax              # imm = 0x100
	movabsq	$.L.str5, %rbx
	je	.LBB3_48
# BB#47:                                # %if.then126.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	$8, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	fprintf
	movq	48(%r14), %rax
.LBB3_48:                               # %for.inc129.i
                                        #   in Loop: Header=BB3_1 Depth=1
	testq	$512, %rax              # imm = 0x200
	je	.LBB3_50
# BB#49:                                # %if.then126.1.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	$9, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	fprintf
	movq	48(%r14), %rax
.LBB3_50:                               # %for.inc129.1.i
                                        #   in Loop: Header=BB3_1 Depth=1
	testq	$1024, %rax             # imm = 0x400
	je	.LBB3_52
# BB#51:                                # %if.then126.2.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	$10, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	fprintf
	movq	48(%r14), %rax
.LBB3_52:                               # %for.inc129.2.i
                                        #   in Loop: Header=BB3_1 Depth=1
	testq	$2048, %rax             # imm = 0x800
	je	.LBB3_54
# BB#53:                                # %if.then126.3.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	$11, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	fprintf
	movq	48(%r14), %rax
.LBB3_54:                               # %for.inc129.3.i
                                        #   in Loop: Header=BB3_1 Depth=1
	testq	$4096, %rax             # imm = 0x1000
	je	.LBB3_56
# BB#55:                                # %if.then126.4.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	$12, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	fprintf
	movq	48(%r14), %rax
.LBB3_56:                               # %for.inc129.4.i
                                        #   in Loop: Header=BB3_1 Depth=1
	testq	$8192, %rax             # imm = 0x2000
	je	.LBB3_58
# BB#57:                                # %if.then126.5.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	$13, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	fprintf
	movq	48(%r14), %rax
.LBB3_58:                               # %for.inc129.5.i
                                        #   in Loop: Header=BB3_1 Depth=1
	testq	$16384, %rax            # imm = 0x4000
	je	.LBB3_60
# BB#59:                                # %if.then126.6.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	$14, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	fprintf
	movq	48(%r14), %rax
.LBB3_60:                               # %for.inc129.6.i
                                        #   in Loop: Header=BB3_1 Depth=1
	testq	$32768, %rax            # imm = 0x8000
	je	.LBB3_62
# BB#61:                                # %if.then126.7.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	$15, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	fprintf
.LBB3_62:                               # %for.inc129.7.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	$17, %rsi
	movq	$1, %rdx
	movabsq	$.L.str6, %rdi
	movq	%r12, %rcx
	callq	fwrite
	movq	%r12, %rdi
	leaq	88(%rsp), %rsi
	callq	print_stack
.LBB3_63:                               # %if.end132.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	8(%r13), %r15
	movzbl	(%r15), %eax
	movq	%r14, %r12
	cmpq	$33, %rax
	jne	.LBB3_64
# BB#65:                                # %if.then138.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	%r13, 32(%rsp)          # 8-byte Spill
	movq	16(%r15), %r15
	jmp	.LBB3_66
	.align	16, 0x90
.LBB3_64:                               #   in Loop: Header=BB3_1 Depth=1
	movq	%r13, 32(%rsp)          # 8-byte Spill
.LBB3_66:                               # %for.cond143.preheader.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	$8, %rbx
	cmpq	$0, 80(%rsp)            # 8-byte Folded Reload
	movabsq	$FP_mode_reg+120, %rax
	movq	%rax, %r14
	movq	%rax, %r13
	je	.LBB3_71
	.align	16, 0x90
.LBB3_67:                               # %for.body146.us.i
                                        #   Parent Loop BB3_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	48(%r12), %rdx
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	testq	%rax, %rdx
	je	.LBB3_70
# BB#68:                                # %land.lhs.true152.us.i
                                        #   in Loop: Header=BB3_67 Depth=2
	movq	96(%rsp), %rcx
	testq	%rax, %rcx
	jne	.LBB3_70
# BB#69:                                # %if.end161.us.i
                                        #   in Loop: Header=BB3_67 Depth=2
	xorq	%rax, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str7, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movq	(%r14), %rdx
	movq	nan(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	emit_insn_after
	movq	%rax, %r15
	movq	%r15, %rdi
	leaq	88(%rsp), %rsi
	callq	subst_stack_regs
.LBB3_70:                               # %for.inc167.us.i
                                        #   in Loop: Header=BB3_67 Depth=2
	incq	%rbx
	addq	$472, %r14              # imm = 0x1D8
	cmpq	$16, %rbx
	jne	.LBB3_67
	jmp	.LBB3_75
	.align	16, 0x90
.LBB3_71:                               # %for.body146.i
                                        #   Parent Loop BB3_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	48(%r12), %rdx
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	testq	%rax, %rdx
	je	.LBB3_74
# BB#72:                                # %land.lhs.true152.i
                                        #   in Loop: Header=BB3_71 Depth=2
	movq	96(%rsp), %rcx
	testq	%rax, %rcx
	jne	.LBB3_74
# BB#73:                                # %if.end161.i
                                        #   in Loop: Header=BB3_71 Depth=2
	movq	(%r13), %rdx
	movq	nan(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	emit_insn_after
	movq	%rax, %r15
	movq	%r15, %rdi
	leaq	88(%rsp), %rsi
	callq	subst_stack_regs
.LBB3_74:                               # %for.inc167.i
                                        #   in Loop: Header=BB3_71 Depth=2
	incq	%rbx
	addq	$472, %r13              # imm = 0x1D8
	cmpq	$16, %rbx
	jne	.LBB3_71
.LBB3_75:                               # %for.end169.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	96(%rsp), %rax
	movq	48(%r12), %rcx
	cmpq	%rcx, %rax
	jne	.LBB3_76
# BB#78:                                # %win.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	104(%rsp), %rax
	movq	%rax, 40(%r12)
	movq	88(%rsp), %rax
	movq	96(%rsp), %rcx
	movq	%rcx, 32(%r12)
	movq	%rax, 24(%r12)
	movq	32(%rsp), %r13          # 8-byte Reload
	movq	40(%r13), %rbx
	testq	%rbx, %rbx
	movabsq	$entry_exit_blocks+112, %r15
	movq	80(%rsp), %r12          # 8-byte Reload
	je	.LBB3_79
	.align	16, 0x90
.LBB3_85:                               # %for.body179.i
                                        #   Parent Loop BB3_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	48(%rbx), %ecx
	movq	24(%rbx), %rax
	testq	$32, %rcx
	jne	.LBB3_87
# BB#86:                                # %lor.lhs.false183.i
                                        #   in Loop: Header=BB3_85 Depth=2
	cmpq	%r15, %rax
	movq	%r15, %rax
	jne	.LBB3_90
.LBB3_87:                               # %if.then187.i
                                        #   in Loop: Header=BB3_85 Depth=2
	movq	80(%rax), %rcx
	movl	56(%rcx), %ecx
	testq	%rcx, %rcx
	jne	.LBB3_89
# BB#88:                                # %if.then187.i
                                        #   in Loop: Header=BB3_85 Depth=2
	cmpq	%r13, %rax
	jne	.LBB3_103
.LBB3_89:                               # %if.end196.i
                                        #   in Loop: Header=BB3_85 Depth=2
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	compensate_edge
	orq	%rax, 72(%rsp)          # 8-byte Folded Spill
.LBB3_90:                               # %for.inc201.i
                                        #   in Loop: Header=BB3_85 Depth=2
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB3_85
.LBB3_79:                               # %for.cond205.preheader.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	32(%r13), %rbx
	testq	%rbx, %rbx
	movq	72(%rsp), %r15          # 8-byte Reload
	movq	48(%rsp), %rcx          # 8-byte Reload
	je	.LBB3_94
	.align	16, 0x90
.LBB3_80:                               # %for.body207.i
                                        #   Parent Loop BB3_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%rcx, %rbx
	je	.LBB3_93
# BB#81:                                # %land.lhs.true210.i
                                        #   in Loop: Header=BB3_80 Depth=2
	movzbl	48(%rbx), %eax
	testq	$32, %rax
	jne	.LBB3_93
# BB#82:                                # %land.lhs.true214.i
                                        #   in Loop: Header=BB3_80 Depth=2
	movq	%rcx, %r14
	movq	16(%rbx), %rax
	movabsq	$entry_exit_blocks, %rcx
	cmpq	%rcx, %rax
	je	.LBB3_92
# BB#83:                                # %if.then218.i
                                        #   in Loop: Header=BB3_80 Depth=2
	movq	80(%rax), %rax
	movl	56(%rax), %eax
	testq	%rax, %rax
	je	.LBB3_84
# BB#91:                                # %if.end224.i
                                        #   in Loop: Header=BB3_80 Depth=2
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	compensate_edge
	orq	%rax, %r15
.LBB3_92:                               # %for.inc229.i
                                        #   in Loop: Header=BB3_80 Depth=2
	movq	%r14, %rcx
.LBB3_93:                               # %for.inc229.i
                                        #   in Loop: Header=BB3_80 Depth=2
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB3_80
.LBB3_94:                               # %convert_regs_1.exit
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	80(%r13), %rax
	movq	$1, %rcx
	movl	%ecx, 56(%rax)
	movq	40(%r13), %rax
	jmp	.LBB3_95
	.align	16, 0x90
.LBB3_99:                               # %for.inc
                                        #   in Loop: Header=BB3_95 Depth=2
	movq	8(%rax), %rax
.LBB3_95:                               # %convert_regs_1.exit
                                        #   Parent Loop BB3_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rax, %rax
	je	.LBB3_100
# BB#96:                                # %for.body
                                        #   in Loop: Header=BB3_95 Depth=2
	movzbl	48(%rax), %ecx
	testq	$32, %rcx
	jne	.LBB3_99
# BB#97:                                # %if.then
                                        #   in Loop: Header=BB3_95 Depth=2
	movq	24(%rax), %rcx
	movq	80(%rcx), %rdx
	movl	60(%rdx), %esi
	decq	%rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	testq	%rdi, %rsi
	movl	%esi, 60(%rdx)
	jne	.LBB3_99
# BB#98:                                # %if.then9
                                        #   in Loop: Header=BB3_95 Depth=2
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	%rcx, (%rdx)
	addq	$8, %rdx
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	jmp	.LBB3_99
	.align	16, 0x90
.LBB3_100:                              # %do.cond
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	orq	%r15, %rax
	movq	%rax, %rdx
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	64(%rsp), %rcx          # 8-byte Reload
	cmpq	%rax, %rcx
	movq	%rcx, %rax
	jne	.LBB3_1
# BB#101:                               # %do.end
	movq	%rdx, %rax
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB3_102:                              # %if.then98.i
	movabsq	$.L.str2, %rdi
	movabsq	$.L__FUNCTION__.convert_regs_1, %rdx
	movq	$2683, %rsi             # imm = 0xA7B
	callq	fancy_abort
.LBB3_103:                              # %if.then195.i
	movabsq	$.L.str2, %rdi
	movabsq	$.L__FUNCTION__.convert_regs_1, %rdx
	movq	$2755, %rsi             # imm = 0xAC3
	callq	fancy_abort
.LBB3_76:                               # %if.end175.i
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.convert_regs_1, %rcx
	movq	$2743, %rsi             # imm = 0xAB7
	jmp	.LBB3_77
.LBB3_84:                               # %if.then223.i
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.convert_regs_1, %rcx
	movq	$2765, %rsi             # imm = 0xACD
.LBB3_77:                               # %if.end175.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp64:
	.size	convert_regs_2, .Ltmp64-convert_regs_2
	.cfi_endproc

	.align	16, 0x90
	.type	compensate_edge,@function
compensate_edge:                        # @compensate_edge
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp70:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp71:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp72:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp73:
	.cfi_def_cfa_offset 40
	subq	$56, %rsp
.Ltmp74:
	.cfi_def_cfa_offset 96
.Ltmp75:
	.cfi_offset %rbx, -40
.Ltmp76:
	.cfi_offset %r12, -32
.Ltmp77:
	.cfi_offset %r14, -24
.Ltmp78:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	16(%r14), %rbx
	movq	24(%r14), %rax
	movq	80(%rbx), %rcx
	movq	80(%rax), %r15
	movq	%rbx, current_block(%rip)
	movq	40(%rcx), %rdx
	movq	%rdx, 48(%rsp)
	movq	24(%rcx), %rdx
	movq	32(%rcx), %rcx
	movq	%rsi, %r12
	testq	%r12, %r12
	movq	%rcx, 40(%rsp)
	movq	%rdx, 32(%rsp)
	je	.LBB4_2
# BB#1:                                 # %if.then
	movl	88(%rax), %ecx
	movl	88(%rbx), %edx
	movabsq	$.L.str18, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	fprintf
.LBB4_2:                                # %if.end
	movl	(%r15), %eax
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	movslq	32(%rsp), %rsi
	sarq	%cl, %rdi
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	cmpq	%rcx, %rax
	jne	.LBB4_14
# BB#3:                                 # %for.cond.preheader
	testq	%rsi, %rsi
	js	.LBB4_8
# BB#4:                                 # %for.body.lr.ph
	movq	8(%r15), %rax
	.align	16, 0x90
.LBB4_6:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	48(%rsp,%rsi), %ecx
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rax, %rdx
	je	.LBB4_8
# BB#5:                                 # %for.cond
                                        #   in Loop: Header=BB4_6 Depth=1
	movq	$32, %rcx
	leaq	-1(%rsi), %rdx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	movq	%rdx, %rsi
	jg	.LBB4_6
# BB#7:                                 # %for.cond.for.end_crit_edge
	movq	%rdx, %rsi
.LBB4_8:                                # %for.end
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	cmpq	%rax, %rsi
	je	.LBB4_9
# BB#12:                                # %if.end17
	testq	%r12, %r12
	je	.LBB4_25
# BB#13:                                # %if.then19
	movabsq	$.L.str20, %rdi
	movq	$23, %rsi
	movq	$1, %rdx
	movq	%r12, %rcx
	callq	fwrite
	jmp	.LBB4_25
.LBB4_14:                               # %if.else
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rsi
	cmpq	%rsi, %rax
	jne	.LBB4_23
# BB#15:                                # %for.cond28.preheader
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	js	.LBB4_20
# BB#16:
	movq	%rdi, %rax
	.align	16, 0x90
.LBB4_18:                               # %for.body31
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%r15,%rax), %ecx
	movzbl	48(%rsp,%rax), %esi
	cmpq	%rsi, %rcx
	jne	.LBB4_20
# BB#17:                                # %for.cond28
                                        #   in Loop: Header=BB4_18 Depth=1
	movq	$32, %rcx
	leaq	-1(%rax), %rsi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rsi, %rax
	jg	.LBB4_18
# BB#19:                                # %for.cond28.for.end46_crit_edge
	movq	%rsi, %rax
.LBB4_20:                               # %for.end46
	andq	%rdx, %rax
	cmpq	%rdx, %rax
	je	.LBB4_21
.LBB4_23:                               # %if.end55
	testq	%r12, %r12
	je	.LBB4_25
# BB#24:                                # %if.then57
	movabsq	$.L.str22, %rdi
	movq	$20, %rsi
	movq	$1, %rdx
	movq	%r12, %rcx
	callq	fwrite
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	print_stack
.LBB4_25:                               # %if.end60
	movl	48(%r14), %eax
	movq	%rax, %rcx
	andq	$12, %rcx
	cmpq	$8, %rcx
	je	.LBB4_26
# BB#27:                                # %if.else66
	testq	$4, %rax
	je	.LBB4_33
# BB#28:                                # %if.then70
	cmpq	$0, 8(%r15)
	jne	.LBB4_29
# BB#31:                                # %eh1
	movq	40(%rsp), %rax
	cmpq	$256, %rax              # imm = 0x100
	jne	.LBB4_32
.LBB4_26:                               # %if.then64
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, (%r15)
	xorq	%rbx, %rbx
	jmp	.LBB4_42
.LBB4_9:                                # %if.then12
	testq	%r12, %r12
	je	.LBB4_11
# BB#10:                                # %if.then14
	movabsq	$.L.str19, %rdi
	movq	$34, %rsi
	movq	$1, %rdx
	movq	%r12, %rcx
	callq	fwrite
.LBB4_11:                               # %if.end16
	movq	48(%rsp), %rax
	movq	32(%rsp), %rcx
	movq	40(%rsp), %rdx
	movq	%rax, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 8(%rsp)
	movq	8(%rbx), %rdi
	leaq	8(%rsp), %rsi
	xorq	%rcx, %rcx
	xorq	%rbx, %rbx
	movq	%r15, %rdx
	callq	change_stack
	jmp	.LBB4_42
.LBB4_33:                               # %if.else82
	movq	40(%rbx), %rcx
	andq	$2, %rax
	cmpq	$0, 8(%rcx)
	jne	.LBB4_39
# BB#34:                                # %if.else82
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB4_39
# BB#35:                                # %if.then88
	movq	48(%rsp), %rax
	movq	32(%rsp), %rcx
	movq	40(%rsp), %rdx
	movq	%rax, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rcx, 8(%rsp)
	movq	8(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$33, %rax
	jne	.LBB4_36
# BB#37:                                # %if.then88
	movabsq	$1, %rcx
	jmp	.LBB4_38
.LBB4_21:                               # %if.then49
	xorq	%rbx, %rbx
	testq	%r12, %r12
	je	.LBB4_42
# BB#22:                                # %if.then51
	movabsq	$.L.str21, %rdi
	movq	$18, %rsi
	movq	$1, %rdx
	movq	%r12, %rcx
	callq	fwrite
	jmp	.LBB4_42
.LBB4_39:                               # %if.else93
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB4_40
# BB#41:                                # %if.end100
	movq	$0, current_block(%rip)
	callq	start_sequence
	movabsq	$4294967197, %rsi       # imm = 0xFFFFFF9D
	xorq	%rdi, %rdi
	callq	emit_note
	movq	48(%rsp), %rcx
	movq	32(%rsp), %rdx
	movq	40(%rsp), %rsi
	movq	%rcx, 24(%rsp)
	movq	%rsi, 16(%rsp)
	movq	%rdx, 8(%rsp)
	leaq	8(%rsp), %rsi
	movq	$1, %rcx
	movq	$1, %rbx
	movq	%rax, %rdi
	movq	%r15, %rdx
	callq	change_stack
	callq	gen_sequence
	movq	%rax, %r15
	callq	end_sequence
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	insert_insn_on_edge
	jmp	.LBB4_42
.LBB4_36:
	movabsq	$0, %rcx
.LBB4_38:                               # %if.then88
	leaq	8(%rsp), %rsi
	movq	%r15, %rdx
	callq	change_stack
	xorq	%rbx, %rbx
.LBB4_42:                               # %return
	movq	%rbx, %rax
	addq	$56, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB4_29:                               # %if.end75
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.compensate_edge, %rcx
	movq	$2559, %rsi             # imm = 0x9FF
	jmp	.LBB4_30
.LBB4_32:                               # %if.end80
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.compensate_edge, %rcx
	movq	$2564, %rsi             # imm = 0xA04
	jmp	.LBB4_30
.LBB4_40:                               # %if.then99
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.compensate_edge, %rcx
	movq	$2591, %rsi             # imm = 0xA1F
.LBB4_30:                               # %if.end75
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp79:
	.size	compensate_edge, .Ltmp79-compensate_edge
	.cfi_endproc

	.align	16, 0x90
	.type	print_stack,@function
print_stack:                            # @print_stack
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp85:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp86:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp87:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp88:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp89:
	.cfi_def_cfa_offset 48
.Ltmp90:
	.cfi_offset %rbx, -40
.Ltmp91:
	.cfi_offset %r12, -32
.Ltmp92:
	.cfi_offset %r14, -24
.Ltmp93:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	%rdi, %r14
	testq	%r14, %r14
	je	.LBB5_10
# BB#1:                                 # %if.end
	movl	(%r12), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	cmpq	%rcx, %rax
	jne	.LBB5_2
# BB#5:                                 # %if.then4
	movabsq	$.L.str14, %rdi
	movq	$6, %rsi
	jmp	.LBB5_4
.LBB5_10:                               # %if.end13
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB5_2:                                # %if.end
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	cmpq	%rcx, %rax
	jne	.LBB5_6
# BB#3:                                 # %if.then1
	movabsq	$.L.str13, %rdi
	movq	$14, %rsi
	jmp	.LBB5_4
.LBB5_6:                                # %if.else6
	movabsq	$.L.str15, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movslq	(%r12), %rax
	testq	%rax, %rax
	js	.LBB5_9
# BB#7:                                 # %for.body.preheader
	xorq	%rbx, %rbx
	movabsq	$.L.str16, %r15
	.align	16, 0x90
.LBB5_8:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%r12,%rbx), %edx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	fprintf
	movslq	(%r12), %rax
	movq	$32, %rcx
	leaq	1(%rbx), %rdx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rax, %rbx
	movq	%rdx, %rbx
	jl	.LBB5_8
.LBB5_9:                                # %for.end
	movabsq	$.L.str17, %rdi
	movq	$2, %rsi
.LBB5_4:                                # %if.then1
	movq	$1, %rdx
	movq	%r14, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	fwrite  # TAILCALL
.Ltmp94:
	.size	print_stack, .Ltmp94-print_stack
	.cfi_endproc

	.align	16, 0x90
	.type	subst_stack_regs,@function
subst_stack_regs:                       # @subst_stack_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp101:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp102:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp103:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp104:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp105:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp106:
	.cfi_def_cfa_offset 80
.Ltmp107:
	.cfi_offset %rbx, -48
.Ltmp108:
	.cfi_offset %r12, -40
.Ltmp109:
	.cfi_offset %r13, -32
.Ltmp110:
	.cfi_offset %r14, -24
.Ltmp111:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movl	(%r15), %eax
	movq	%rsi, %r14
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$34, %rcx
	jne	.LBB6_13
# BB#1:                                 # %if.then
	movslq	(%r14), %rdx
	testq	%rdx, %rdx
	js	.LBB6_13
# BB#2:                                 # %if.then3
	xorq	%rax, %rax
	testq	%rdx, %rdx
	jle	.LBB6_6
# BB#3:                                 # %for.body.for.body_crit_edge.i.lr.ph
	movq	16(%r15), %rdi
	movq	8(%r14), %rax
	movq	%rax, 16(%rsp)
	movl	%edx, 8(%rsp)
	movq	$8, %rax
	movb	%al, 24(%rsp,%rdx)
	decq	%rdx
	.align	16, 0x90
.LBB6_4:                                # %for.body.for.body_crit_edge.i
                                        # =>This Inner Loop Header: Depth=1
	movl	8(%rsp), %eax
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	$8, %rax
	subq	%rdx, %rax
	testq	%rsi, %rsi
	movb	%al, 24(%rsp,%rdx)
	leaq	-1(%rdx), %rdx
	jg	.LBB6_4
# BB#5:                                 # %straighten_stack.exit
	leaq	8(%rsp), %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rsi
	callq	change_stack
	movslq	(%r14), %rax
	testq	%rax, %rax
	js	.LBB6_12
.LBB6_6:                                # %while.body.lr.ph
	movq	8(%r14), %rdx
	movq	%rax, %rsi
	.align	16, 0x90
.LBB6_7:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rdi
	movq	$32, %rcx
	movq	%rsi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	8(%rsi), %rcx
	movq	$1, %rdx
	shlq	%cl, %rdx
	notq	%rdx
	andq	%rdi, %rdx
	decq	%rsi
	testq	%rbx, %rbx
	jg	.LBB6_7
# BB#8:                                 # %while.cond.if.end8.loopexit_crit_edge
	testq	%rax, %rax
	js	.LBB6_9
# BB#10:                                # %while.cond.if.end8.loopexit_crit_edge
	movq	$-1, %rax
	jmp	.LBB6_11
.LBB6_9:
	decq	%rax
.LBB6_11:                               # %while.cond.if.end8.loopexit_crit_edge
	movq	%rdx, 8(%r14)
	movl	%eax, (%r14)
.LBB6_12:                               # %if.end8.loopexit
	movl	(%r15), %eax
.LBB6_13:                               # %if.end8
	movq	%r14, (%rsp)            # 8-byte Spill
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB6_31
# BB#14:                                # %if.end8
	movq	stack_regs_mentioned_data(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB6_31
# BB#15:                                # %if.end.i
	movl	8(%r15), %r12d
	movl	(%rbx), %eax
	cmpq	%rax, %r12
	jb	.LBB6_17
# BB#16:                                # %if.then6.i
	movabsq	$-3689348814741910323, %rcx # imm = 0xCCCCCCCCCCCCCCCD
	movq	%r12, %rax
	mulq	%rcx
	movq	$4, %rcx
	shrq	%cl, %rdx
	movq	%r12, %rax
	addq	%rdx, %rax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	movq	%rbx, %rdi
	callq	varray_grow
	movq	%rax, %rbx
	movq	%rbx, stack_regs_mentioned_data(%rip)
.LBB6_17:                               # %if.end8.i
	movzbl	32(%rbx,%r12), %edx
	movq	$56, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rdx, %rdx
	jne	.LBB6_22
# BB#18:                                # %if.then14.i
	movq	32(%r15), %rdi
	callq	stack_regs_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_19
# BB#20:                                # %if.then14.i
	movabsq	$1, %rax
	jmp	.LBB6_21
.LBB6_19:
	movabsq	$0, %rax
.LBB6_21:                               # %if.then14.i
	xorq	$1, %rax
	incq	%rax
	movb	%al, 32(%rbx,%r12)
.LBB6_22:                               # %stack_regs_mentioned.exit
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	cmpq	$1, %rax
	jne	.LBB6_31
# BB#23:                                # %if.then9
	movq	32(%r15), %rdi
	callq	asm_noperands
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB6_24
# BB#42:                                # %if.then15
	movq	%r15, %rdi
	movq	(%rsp), %rsi            # 8-byte Reload
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	subst_asm_stack_regs  # TAILCALL
.LBB6_24:                               # %if.end16
	movq	32(%r15), %r12
	movzbl	(%r12), %eax
	cmpq	$39, %rax
	jne	.LBB6_30
# BB#25:                                # %for.cond.preheader
	movq	8(%r12), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB6_31
# BB#26:
	movq	$1, %r13
	.align	16, 0x90
.LBB6_27:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%r13,8), %rbx
	movq	%rbx, %rdi
	callq	stack_regs_mentioned_p
	testq	%r14, %rax
	je	.LBB6_29
# BB#28:                                # %if.then39
                                        #   in Loop: Header=BB6_27 Depth=1
	movq	%r15, %rdi
	movq	(%rsp), %rsi            # 8-byte Reload
	movq	%rbx, %rdx
	callq	subst_stack_regs_pat
	movq	32(%r15), %r12
.LBB6_29:                               # %for.inc
                                        #   in Loop: Header=BB6_27 Depth=1
	movq	8(%r12), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r13
	cmpq	%rdx, %rsi
	jl	.LBB6_27
	jmp	.LBB6_31
.LBB6_30:                               # %if.else
	movq	%r15, %rdi
	movq	(%rsp), %rsi            # 8-byte Reload
	movq	%r12, %rdx
	callq	subst_stack_regs_pat
.LBB6_31:                               # %if.end54
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$37, %rcx
	movq	(%rsp), %r14            # 8-byte Reload
	je	.LBB6_41
# BB#32:                                # %if.end54
	andq	$134217728, %rax        # imm = 0x8000000
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB6_41
# BB#33:                                # %if.end62
	movq	56(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB6_41
# BB#34:
	leaq	56(%r15), %r13
	movabsq	$4294967288, %r12       # imm = 0xFFFFFFF8
	.align	16, 0x90
.LBB6_35:                               # %for.body68
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$655360, %rax           # imm = 0xA0000
	jne	.LBB6_39
# BB#36:                                # %land.lhs.true
                                        #   in Loop: Header=BB6_35 Depth=1
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB6_39
# BB#37:                                # %land.lhs.true79
                                        #   in Loop: Header=BB6_35 Depth=1
	movl	8(%rax), %eax
	andq	%r12, %rax
	cmpq	$8, %rax
	jne	.LBB6_39
# BB#38:                                # %if.then94
                                        #   in Loop: Header=BB6_35 Depth=1
	movq	16(%rbx), %rax
	movq	%rax, (%r13)
	movq	8(%rbx), %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	emit_pop_insn
	movq	%rax, %r15
	jmp	.LBB6_40
	.align	16, 0x90
.LBB6_39:                               # %if.else102
                                        #   in Loop: Header=BB6_35 Depth=1
	leaq	16(%rbx), %r13
.LBB6_40:                               # %for.inc107
                                        #   in Loop: Header=BB6_35 Depth=1
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB6_35
.LBB6_41:                               # %for.end111
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp112:
	.size	subst_stack_regs, .Ltmp112-subst_stack_regs
	.cfi_endproc

	.align	16, 0x90
	.type	subst_asm_stack_regs,@function
subst_asm_stack_regs:                   # @subst_asm_stack_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp116:
	.cfi_def_cfa_offset 16
.Ltmp117:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp118:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$136, %rsp
.Ltmp119:
	.cfi_offset %rbx, -56
.Ltmp120:
	.cfi_offset %r12, -48
.Ltmp121:
	.cfi_offset %r13, -40
.Ltmp122:
	.cfi_offset %r14, -32
.Ltmp123:
	.cfi_offset %r15, -24
	movq	%rdi, -120(%rbp)        # 8-byte Spill
	movq	32(%rdi), %r15
	movq	%rsi, %r13
	callq	check_asm_stack_operands
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB7_164
# BB#1:                                 # %if.end
	movq	-120(%rbp), %rdi        # 8-byte Reload
	callq	extract_insn
	movq	$1, %rdi
	callq	constrain_operands
	movslq	which_alternative(%rip), %rax
	movq	%rax, -112(%rbp)        # 8-byte Spill
	callq	preprocess_constraints
	movzbl	(%r15), %eax
	cmpq	$39, %rax
	je	.LBB7_6
# BB#2:                                 # %if.end
	cmpq	$41, %rax
	jne	.LBB7_3
# BB#12:                                # %if.then12.i
	movq	32(%r15), %rax
	jmp	.LBB7_13
.LBB7_6:                                # %land.lhs.true21.i
	movq	8(%r15), %rax
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$41, %rcx
	je	.LBB7_9
# BB#7:                                 # %land.lhs.true21.i
	cmpq	$47, %rcx
	jne	.LBB7_10
# BB#8:                                 # %if.then29.i
	movq	16(%rax), %rax
.LBB7_9:                                # %if.then55.i
	movq	32(%rax), %rax
	jmp	.LBB7_13
.LBB7_3:                                # %if.end
	cmpq	$47, %rax
	jne	.LBB7_10
# BB#4:                                 # %land.lhs.true.i
	movq	16(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$41, %rcx
	jne	.LBB7_10
# BB#5:                                 # %if.then.i
	movq	32(%rax), %rax
.LBB7_13:                               # %get_asm_operand_n_inputs.exit
	cmpq	$0, -112(%rbp)          # 8-byte Folded Reload
	js	.LBB7_165
# BB#14:                                # %for.cond.preheader
	movsbq	recog_data+996(%rip), %rcx
	movq	%rcx, -80(%rbp)         # 8-byte Spill
	movl	(%rax), %eax
	movq	%rax, -128(%rbp)        # 8-byte Spill
	testq	%rcx, %rcx
	jle	.LBB7_20
# BB#15:
	xorq	%rax, %rax
	movq	-80(%rbp), %rdx         # 8-byte Reload
	.align	16, 0x90
.LBB7_16:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	recog_data(,%rax,8), %rcx
	movzbl	(%rcx), %esi
	cmpq	$63, %rsi
	jne	.LBB7_19
# BB#17:                                # %land.lhs.true
                                        #   in Loop: Header=BB7_16 Depth=1
	movq	8(%rcx), %rsi
	movzbl	(%rsi), %esi
	cmpq	$61, %rsi
	jne	.LBB7_19
# BB#18:                                # %if.then21
                                        #   in Loop: Header=BB7_16 Depth=1
	addq	$8, %rcx
	movq	%rcx, recog_data+240(,%rax,8)
	movq	(%rcx), %rcx
	movq	%rcx, recog_data(,%rax,8)
	movzbl	recog_data+996(%rip), %edx
.LBB7_19:                               # %for.inc
                                        #   in Loop: Header=BB7_16 Depth=1
	incq	%rax
	movq	$56, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rsi, %rdi
	jl	.LBB7_16
.LBB7_20:                               # %for.end
	movq	-120(%rbp), %rax        # 8-byte Reload
	movq	56(%rax), %rax
	movq	-80(%rbp), %r12         # 8-byte Reload
	movq	-128(%rbp), %rcx        # 8-byte Reload
	subq	%rcx, %r12
	testq	%rax, %rax
	je	.LBB7_166
# BB#21:
	xorq	%rdx, %rdx
	movq	%rax, %rcx
	.align	16, 0x90
.LBB7_22:                               # %for.body42
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rcx), %rcx
	incq	%rdx
	testq	%rcx, %rcx
	jne	.LBB7_22
# BB#23:                                # %for.end48
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	15(,%rdx,8), %rcx
	andq	$-16, %rcx
	movq	%rsp, %r9
	subq	%rcx, %r9
	movq	%r9, %rsp
	movq	%rsp, %rsi
	subq	%rcx, %rsi
	movq	%rsi, -104(%rbp)        # 8-byte Spill
	movq	%rsi, %rsp
	leaq	15(,%rdx,4), %rcx
	andq	$-16, %rcx
	movq	%rsp, %r10
	subq	%rcx, %r10
	movq	%r10, %rsp
	xorq	%r11, %r11
	testq	%rax, %rax
	je	.LBB7_38
# BB#24:
	movabsq	$4294967288, %r8        # imm = 0xFFFFFFF8
	.align	16, 0x90
.LBB7_25:                               # %for.body59
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rsi
	movl	(%rsi), %ecx
	leaq	8(%rax), %rdi
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$63, %rdx
	jne	.LBB7_26
# BB#27:                                # %land.lhs.true70
                                        #   in Loop: Header=BB7_25 Depth=1
	movq	8(%rsi), %rbx
	movzbl	(%rbx), %ecx
	cmpq	$61, %rcx
	leaq	8(%rsi), %rdx
	je	.LBB7_29
# BB#28:                                # %land.lhs.true70
                                        #   in Loop: Header=BB7_25 Depth=1
	movq	%rdi, %rdx
.LBB7_29:                               # %land.lhs.true70
                                        #   in Loop: Header=BB7_25 Depth=1
	je	.LBB7_31
# BB#30:                                # %land.lhs.true70
                                        #   in Loop: Header=BB7_25 Depth=1
	movq	%rsi, %rbx
.LBB7_31:                               # %land.lhs.true70
                                        #   in Loop: Header=BB7_25 Depth=1
	movl	(%rbx), %ecx
	movq	%rbx, %rsi
	jmp	.LBB7_32
	.align	16, 0x90
.LBB7_26:                               #   in Loop: Header=BB7_25 Depth=1
	movq	%rdi, %rdx
.LBB7_32:                               # %if.end85
                                        #   in Loop: Header=BB7_25 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB7_37
# BB#33:                                # %land.lhs.true90
                                        #   in Loop: Header=BB7_25 Depth=1
	movl	8(%rsi), %ecx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB7_37
# BB#34:                                # %land.lhs.true101
                                        #   in Loop: Header=BB7_25 Depth=1
	movzbl	2(%rax), %ebx
	cmpq	$1, %rbx
	je	.LBB7_36
# BB#35:                                # %land.lhs.true101
                                        #   in Loop: Header=BB7_25 Depth=1
	cmpq	$10, %rbx
	jne	.LBB7_37
.LBB7_36:                               # %if.then111
                                        #   in Loop: Header=BB7_25 Depth=1
	movq	$32, %rcx
	movq	%r11, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%rsi, (%r9,%rdi,8)
	movq	-104(%rbp), %rcx        # 8-byte Reload
	movq	%rdx, (%rcx,%rdi,8)
	movl	%ebx, (%r10,%rdi,4)
	incq	%r11
	.align	16, 0x90
.LBB7_37:                               # %for.inc123
                                        #   in Loop: Header=BB7_25 Depth=1
	movq	16(%rax), %rax
	testq	%rax, %rax
	jne	.LBB7_25
	jmp	.LBB7_38
.LBB7_166:                              # %for.end48.thread
	leaq	-48(%rbp), %r10
	xorq	%r11, %r11
	movq	%r10, -104(%rbp)        # 8-byte Spill
	movq	%r10, %r9
.LBB7_38:                               # %for.end127
	movzbl	(%r15), %eax
	cmpq	$39, %rax
	jne	.LBB7_39
# BB#40:                                # %if.then132
	movq	%r11, -136(%rbp)        # 8-byte Spill
	movq	%r10, -160(%rbp)        # 8-byte Spill
	movq	%r9, %r11
	movq	8(%r15), %r9
	movslq	(%r9), %r10
	movq	%rsp, %rcx
	leaq	15(,%r10,8), %rax
	andq	$-16, %rax
	subq	%rax, %rcx
	movq	%rcx, -96(%rbp)         # 8-byte Spill
	movq	%rcx, %rsp
                                        # kill: RCX<def> RSP<kill>
	subq	%rax, %rcx
	movq	%rcx, -152(%rbp)        # 8-byte Spill
	movq	%rcx, %rsp
	movq	$0, -88(%rbp)           # 8-byte Folded Spill
	testq	%r10, %r10
	jle	.LBB7_54
# BB#41:
	movq	$1, %rsi
	movabsq	$4294967288, %r8        # imm = 0xFFFFFFF8
	.align	16, 0x90
.LBB7_42:                               # %for.body150
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r9,%rsi,8), %rbx
	movzbl	(%rbx), %eax
	cmpq	$49, %rax
	jne	.LBB7_53
# BB#43:                                # %if.then160
                                        #   in Loop: Header=BB7_42 Depth=1
	movq	8(%rbx), %rdi
	movl	(%rdi), %ecx
	addq	$8, %rbx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB7_44
# BB#45:                                # %land.lhs.true179
                                        #   in Loop: Header=BB7_42 Depth=1
	movq	8(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	leaq	8(%rdi), %rdx
	je	.LBB7_47
# BB#46:                                # %land.lhs.true179
                                        #   in Loop: Header=BB7_42 Depth=1
	movq	%rdi, %rax
.LBB7_47:                               # %land.lhs.true179
                                        #   in Loop: Header=BB7_42 Depth=1
	je	.LBB7_49
# BB#48:                                # %land.lhs.true179
                                        #   in Loop: Header=BB7_42 Depth=1
	movq	%rbx, %rdx
.LBB7_49:                               # %land.lhs.true179
                                        #   in Loop: Header=BB7_42 Depth=1
	movl	(%rax), %ecx
	movq	%rdx, %rbx
	jmp	.LBB7_50
	.align	16, 0x90
.LBB7_44:                               #   in Loop: Header=BB7_42 Depth=1
	movq	%rdi, %rax
.LBB7_50:                               # %if.end194
                                        #   in Loop: Header=BB7_42 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB7_53
# BB#51:                                # %land.lhs.true199
                                        #   in Loop: Header=BB7_42 Depth=1
	movl	8(%rax), %ecx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB7_53
# BB#52:                                # %if.then211
                                        #   in Loop: Header=BB7_42 Depth=1
	movq	$32, %rcx
	movq	-88(%rbp), %rdi         # 8-byte Reload
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	-96(%rbp), %rcx         # 8-byte Reload
	movq	%rax, (%rcx,%rdx,8)
	movq	-152(%rbp), %rax        # 8-byte Reload
	movq	%rbx, (%rax,%rdx,8)
	incq	%rdi
	movq	%rdi, -88(%rbp)         # 8-byte Spill
	.align	16, 0x90
.LBB7_53:                               # %for.inc219
                                        #   in Loop: Header=BB7_42 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rsi
	cmpq	%r10, %rax
	jl	.LBB7_42
.LBB7_54:
	movq	%r11, %r9
	jmp	.LBB7_55
.LBB7_39:
	movq	%r11, -136(%rbp)        # 8-byte Spill
	movq	%r10, -160(%rbp)        # 8-byte Spill
	movq	$0, -88(%rbp)           # 8-byte Folded Spill
	movq	$0, -152(%rbp)          # 8-byte Folded Spill
	movq	$0, -96(%rbp)           # 8-byte Folded Spill
.LBB7_55:                               # %if.end222
	movq	%r9, -144(%rbp)         # 8-byte Spill
	movq	16(%r13), %rax
	movq	%rax, -56(%rbp)
	movq	(%r13), %rax
	movq	8(%r13), %rcx
	movq	%rcx, -64(%rbp)
	movq	%rax, -72(%rbp)
	movq	$32, %rcx
	movq	-128(%rbp), %rax        # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB7_56
# BB#64:                                # %for.body226.lr.ph
	movq	$32, %rcx
	movq	%r12, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	.align	16, 0x90
.LBB7_65:                               # %for.body226
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_74 Depth 2
	movq	recog_data(,%r15,8), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB7_79
# BB#66:                                # %land.lhs.true233
                                        #   in Loop: Header=BB7_65 Depth=1
	movl	8(%rax), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	jne	.LBB7_79
# BB#67:                                # %land.lhs.true249
                                        #   in Loop: Header=BB7_65 Depth=1
	movq	$5, %rcx
	movq	-112(%rbp), %rbx        # 8-byte Reload
	shlq	%cl, %rbx
	imulq	$960, %r15, %r14        # imm = 0x3C0
	movl	recog_op_alt+8(%r14,%rbx), %edi
	movq	$15, %rsi
	callq	reg_class_subset_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB7_79
# BB#68:                                # %land.lhs.true256
                                        #   in Loop: Header=BB7_65 Depth=1
	leaq	recog_op_alt+8(%r14,%rbx), %rax
	movl	(%rax), %eax
	cmpq	$15, %rax
	je	.LBB7_79
# BB#69:                                # %if.then264
                                        #   in Loop: Header=BB7_65 Depth=1
	movq	recog_data(,%r15,8), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB7_71
# BB#70:                                # %land.lhs.true.i836
                                        #   in Loop: Header=BB7_65 Depth=1
	movl	8(%rax), %eax
	movq	%rax, %rcx
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	jne	.LBB7_71
# BB#72:                                # %if.end.i
                                        #   in Loop: Header=BB7_65 Depth=1
	movslq	-72(%rbp), %rdx
	testq	%rdx, %rdx
	movq	%rdx, %rsi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	js	.LBB7_76
	.align	16, 0x90
.LBB7_74:                               # %for.body.i
                                        #   Parent Loop BB7_65 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	-56(%rbp,%rsi), %ecx
	cmpq	%rax, %rcx
	je	.LBB7_75
# BB#73:                                # %for.cond.i
                                        #   in Loop: Header=BB7_74 Depth=2
	movq	$32, %rcx
	leaq	-1(%rsi), %rdi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	movq	%rdi, %rsi
	jg	.LBB7_74
	jmp	.LBB7_76
.LBB7_75:                               # %get_hard_regnum.exit
                                        #   in Loop: Header=BB7_65 Depth=1
	movq	$8, %rdi
	subq	%rsi, %rdi
	addq	%rdx, %rdi
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	js	.LBB7_76
# BB#77:                                # %if.end271
                                        #   in Loop: Header=BB7_65 Depth=1
	movq	%rdi, %rcx
	andq	%rbx, %rcx
	cmpq	%rax, %rcx
	je	.LBB7_79
# BB#78:                                # %if.then279
                                        #   in Loop: Header=BB7_65 Depth=1
	movq	$8, %rsi
	subq	%rdi, %rsi
	addq	%rdx, %rsi
	movq	$8, %rdi
	subq	%rax, %rdi
	addq	%rdx, %rdi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	-56(%rbp,%rsi), %eax
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movzbl	-56(%rbp,%rdi), %ecx
	movb	%cl, -56(%rbp,%rsi)
	movb	%al, -56(%rbp,%rdi)
	.align	16, 0x90
.LBB7_79:                               # %for.inc306
                                        #   in Loop: Header=BB7_65 Depth=1
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	-80(%rbp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	jl	.LBB7_65
# BB#80:                                # %for.end308
	movq	$32, %rcx
	movq	-128(%rbp), %rbx        # 8-byte Reload
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	-72(%rbp), %rdx
	movq	$1, %rcx
	movq	-120(%rbp), %rdi        # 8-byte Reload
	movq	%r13, %rsi
	callq	change_stack
	testq	%rbx, %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movabsq	$4294967288, %r8        # imm = 0xFFFFFFF8
	jle	.LBB7_57
# BB#81:                                # %for.body313.lr.ph
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	.align	16, 0x90
.LBB7_82:                               # %for.body313
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_86 Depth 2
	movq	recog_data(,%rax,8), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB7_96
# BB#83:                                # %land.lhs.true320
                                        #   in Loop: Header=BB7_82 Depth=1
	movl	8(%rcx), %edx
	movq	%rdx, %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB7_96
# BB#84:                                # %if.end.i847
                                        #   in Loop: Header=BB7_82 Depth=1
	movslq	(%r13), %rsi
	testq	%rsi, %rsi
	movq	%rsi, %rdi
	js	.LBB7_88
	.align	16, 0x90
.LBB7_86:                               # %for.body.i856
                                        #   Parent Loop BB7_82 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	16(%r13,%rdi), %ecx
	cmpq	%rdx, %rcx
	je	.LBB7_87
# BB#85:                                # %for.cond.i851
                                        #   in Loop: Header=BB7_86 Depth=2
	movq	$32, %rcx
	leaq	-1(%rdi), %rbx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rbx, %rdi
	jg	.LBB7_86
	jmp	.LBB7_88
	.align	16, 0x90
.LBB7_87:                               # %get_hard_regnum.exit861
                                        #   in Loop: Header=BB7_82 Depth=1
	movq	$8, %rdx
	subq	%rdi, %rdx
	addq	%rsi, %rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	js	.LBB7_88
# BB#89:                                # %if.end343
                                        #   in Loop: Header=BB7_82 Depth=1
	movq	%rdx, %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB7_92
# BB#90:                                # %lor.lhs.false2.i
                                        #   in Loop: Header=BB7_82 Depth=1
	movq	recog_data+240(,%rax,8), %rsi
	movq	(%rsi), %rcx
	movl	(%rcx), %ebx
	movq	%rbx, %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$61, %rdi
	jne	.LBB7_92
# BB#91:                                # %land.lhs.true.i867
                                        #   in Loop: Header=BB7_82 Depth=1
	movl	8(%rcx), %ecx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB7_92
# BB#93:                                # %if.end.i869
                                        #   in Loop: Header=BB7_82 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movl	mode_class(,%rbx,4), %ecx
	orq	$4, %rcx
	andq	%r14, %rcx
	cmpq	$6, %rcx
	jne	.LBB7_94
# BB#95:                                # %replace_reg.exit
                                        #   in Loop: Header=BB7_82 Depth=1
	addq	$-8, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	imulq	$472, %rdx, %rcx        # imm = 0x1D8
	movq	FP_mode_reg(%rcx,%rbx,8), %rcx
	movq	%rcx, (%rsi)
.LBB7_96:                               # %for.inc347
                                        #   in Loop: Header=BB7_82 Depth=1
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	-80(%rbp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rdx
	jl	.LBB7_82
	jmp	.LBB7_57
.LBB7_56:                               # %for.end308.thread
	leaq	-72(%rbp), %rdx
	movq	$1, %rcx
	movq	-120(%rbp), %rdi        # 8-byte Reload
	movq	%r13, %rsi
	callq	change_stack
.LBB7_57:                               # %for.cond350.preheader
	movq	$32, %rcx
	movq	-136(%rbp), %r11        # 8-byte Reload
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	-144(%rbp), %r9         # 8-byte Reload
	movq	-160(%rbp), %r10        # 8-byte Reload
	jle	.LBB7_97
# BB#58:                                # %for.body353.lr.ph
	xorq	%rax, %rax
	movabsq	$4294967288, %r8        # imm = 0xFFFFFFF8
	.align	16, 0x90
.LBB7_59:                               # %for.body353
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_103 Depth 2
	movl	(%r10,%rax,4), %ecx
	cmpq	$1, %rcx
	jne	.LBB7_110
# BB#60:                                # %if.then358
                                        #   in Loop: Header=BB7_59 Depth=1
	movq	(%r9,%rax,8), %rcx
	movzbl	(%rcx), %esi
	cmpq	$61, %rsi
	jne	.LBB7_71
# BB#61:                                # %land.lhs.true.i876
                                        #   in Loop: Header=BB7_59 Depth=1
	movl	8(%rcx), %esi
	movq	%rsi, %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB7_71
# BB#62:                                # %if.end.i880
                                        #   in Loop: Header=BB7_59 Depth=1
	movslq	(%r13), %rdi
	testq	%rdi, %rdi
	movq	%rdi, %rbx
	js	.LBB7_63
	.align	16, 0x90
.LBB7_103:                              # %for.body.i889
                                        #   Parent Loop BB7_59 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	16(%r13,%rbx), %ecx
	cmpq	%rsi, %rcx
	je	.LBB7_104
# BB#102:                               # %for.cond.i884
                                        #   in Loop: Header=BB7_103 Depth=2
	movq	$32, %rcx
	leaq	-1(%rbx), %rdx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	movq	%rdx, %rbx
	jg	.LBB7_103
	jmp	.LBB7_63
	.align	16, 0x90
.LBB7_104:                              # %get_hard_regnum.exit894
                                        #   in Loop: Header=BB7_59 Depth=1
	movq	$8, %rsi
	subq	%rbx, %rsi
	addq	%rdi, %rsi
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB7_63
# BB#105:                               # %if.end366
                                        #   in Loop: Header=BB7_59 Depth=1
	movq	%rsi, %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB7_92
# BB#106:                               # %lor.lhs.false2.i898
                                        #   in Loop: Header=BB7_59 Depth=1
	movq	-104(%rbp), %rcx        # 8-byte Reload
	movq	(%rcx,%rax,8), %rdi
	movq	(%rdi), %rcx
	movl	(%rcx), %ebx
	movq	%rbx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB7_92
# BB#107:                               # %land.lhs.true.i901
                                        #   in Loop: Header=BB7_59 Depth=1
	movl	8(%rcx), %ecx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB7_92
# BB#108:                               # %if.end.i907
                                        #   in Loop: Header=BB7_59 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movl	mode_class(,%rbx,4), %ecx
	orq	$4, %rcx
	andq	%r14, %rcx
	cmpq	$6, %rcx
	jne	.LBB7_94
# BB#109:                               # %replace_reg.exit912
                                        #   in Loop: Header=BB7_59 Depth=1
	addq	$-8, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	imulq	$472, %rsi, %rcx        # imm = 0x1D8
	movq	FP_mode_reg(%rcx,%rbx,8), %rcx
	movq	%rcx, (%rdi)
.LBB7_110:                              # %for.inc370
                                        #   in Loop: Header=BB7_59 Depth=1
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r11, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB7_59
.LBB7_97:                               # %for.cond373.preheader
	movq	$32, %rcx
	movq	-88(%rbp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB7_111
# BB#98:                                # %for.body376.lr.ph
	xorq	%rax, %rax
	movabsq	$4294967288, %r8        # imm = 0xFFFFFFF8
	.align	16, 0x90
.LBB7_99:                               # %for.body376
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_118 Depth 2
	movq	-96(%rbp), %rcx         # 8-byte Reload
	movq	(%rcx,%rax,8), %rcx
	movzbl	(%rcx), %esi
	cmpq	$61, %rsi
	jne	.LBB7_71
# BB#100:                               # %land.lhs.true.i918
                                        #   in Loop: Header=BB7_99 Depth=1
	movl	8(%rcx), %esi
	movq	%rsi, %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB7_71
# BB#101:                               # %if.end.i922
                                        #   in Loop: Header=BB7_99 Depth=1
	movslq	(%r13), %rdi
	testq	%rdi, %rdi
	movq	%rdi, %rbx
	js	.LBB7_121
	.align	16, 0x90
.LBB7_118:                              # %for.body.i931
                                        #   Parent Loop BB7_99 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	16(%r13,%rbx), %ecx
	cmpq	%rsi, %rcx
	je	.LBB7_119
# BB#117:                               # %for.cond.i926
                                        #   in Loop: Header=BB7_118 Depth=2
	movq	$32, %rcx
	leaq	-1(%rbx), %rdx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	movq	%rdx, %rbx
	jg	.LBB7_118
	jmp	.LBB7_121
	.align	16, 0x90
.LBB7_119:                              # %get_hard_regnum.exit936
                                        #   in Loop: Header=BB7_99 Depth=1
	movq	$8, %rsi
	subq	%rbx, %rsi
	addq	%rdi, %rsi
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB7_121
# BB#120:                               # %if.then383
                                        #   in Loop: Header=BB7_99 Depth=1
	addq	$-8, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	imulq	$472, %rsi, %rcx        # imm = 0x1D8
	movq	FP_mode_reg+128(%rcx), %rcx
	movq	-152(%rbp), %rdx        # 8-byte Reload
	movq	(%rdx,%rax,8), %rdx
	movq	%rcx, (%rdx)
	.align	16, 0x90
.LBB7_121:                              # %for.inc391
                                        #   in Loop: Header=BB7_99 Depth=1
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	-88(%rbp), %rsi         # 8-byte Reload
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB7_99
.LBB7_111:                              # %for.cond394.preheader
	movq	$32, %rcx
	movq	-128(%rbp), %rax        # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB7_127
# BB#112:                               # %for.body398.lr.ph
	movq	$32, %rcx
	movq	%r12, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	.align	16, 0x90
.LBB7_113:                              # %for.body398
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_124 Depth 2
	movq	recog_data(,%r15,8), %rsi
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	jne	.LBB7_126
# BB#114:                               # %land.lhs.true405
                                        #   in Loop: Header=BB7_113 Depth=1
	movl	8(%rsi), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	jne	.LBB7_126
# BB#115:                               # %for.cond423.preheader
                                        #   in Loop: Header=BB7_113 Depth=1
	movq	$32, %rcx
	movq	-88(%rbp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %rbx
	testq	%rax, %rax
	jg	.LBB7_124
	jmp	.LBB7_116
	.align	16, 0x90
.LBB7_123:                              # %for.cond423.for.body426_crit_edge
                                        #   in Loop: Header=BB7_124 Depth=2
	movq	recog_data(,%r15,8), %rsi
	incq	%rbx
.LBB7_124:                              # %for.body426
                                        #   Parent Loop BB7_113 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-96(%rbp), %rax         # 8-byte Reload
	movq	-8(%rax,%rbx,8), %rdi
	callq	operands_match_p
	testq	%r14, %rax
	jne	.LBB7_125
# BB#122:                               # %for.cond423
                                        #   in Loop: Header=BB7_124 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	-88(%rbp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB7_123
.LBB7_116:                              # %lor.lhs.false440
                                        #   in Loop: Header=BB7_113 Depth=1
	movq	$5, %rcx
	movq	-112(%rbp), %rax        # 8-byte Reload
	shlq	%cl, %rax
	imulq	$960, %r15, %rcx        # imm = 0x3C0
	movslq	recog_op_alt+16(%rcx,%rax), %rax
	testq	%rax, %rax
	js	.LBB7_126
.LBB7_125:                              # %if.then447
                                        #   in Loop: Header=BB7_113 Depth=1
	movslq	(%r13), %rax
	movzbl	16(%r13,%rax), %ecx
	movq	$1, %rdx
	shlq	%cl, %rdx
	notq	%rdx
	andq	%rdx, 8(%r13)
	decq	%rax
	movl	%eax, (%r13)
	.align	16, 0x90
.LBB7_126:                              # %for.inc456
                                        #   in Loop: Header=BB7_113 Depth=1
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	-80(%rbp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	jl	.LBB7_113
.LBB7_127:                              # %for.cond459.preheader
	movq	$15, %rax
	movabsq	$4294967288, %r9        # imm = 0xFFFFFFF8
	movq	$15, %rdx
	movq	-160(%rbp), %r8         # 8-byte Reload
	movq	-136(%rbp), %r11        # 8-byte Reload
	.align	16, 0x90
.LBB7_128:                              # %for.cond464.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_129 Depth 2
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	movq	$1, %rsi
	jle	.LBB7_148
	.align	16, 0x90
.LBB7_129:                              # %for.body467
                                        #   Parent Loop BB7_128 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	recog_data-8(,%rsi,8), %rcx
	movzbl	(%rcx), %edi
	cmpq	$61, %rdi
	jne	.LBB7_147
# BB#130:                               # %land.lhs.true474
                                        #   in Loop: Header=BB7_129 Depth=2
	movl	8(%rcx), %ecx
	movq	%rcx, %rdi
	andq	%r9, %rdi
	cmpq	$8, %rdi
	jne	.LBB7_147
# BB#131:                               # %land.lhs.true474
                                        #   in Loop: Header=BB7_129 Depth=2
	movq	%rax, %rdi
	andq	%r14, %rdi
	cmpq	%rdi, %rcx
	je	.LBB7_132
.LBB7_147:                              # %for.inc509
                                        #   in Loop: Header=BB7_129 Depth=2
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	incq	%rsi
	cmpq	%rbx, %rdi
	jl	.LBB7_129
	jmp	.LBB7_148
.LBB7_132:                              # %if.then498
                                        #   in Loop: Header=BB7_128 Depth=1
	movl	(%r13), %esi
	incq	%rsi
	movl	%esi, (%r13)
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movb	%dl, 16(%r13,%rsi)
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	orq	%rsi, 8(%r13)
	.align	16, 0x90
.LBB7_148:                              # %for.inc512
                                        #   in Loop: Header=BB7_128 Depth=1
	decq	%rax
	decq	%rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$7, %rsi
	jg	.LBB7_128
# BB#133:                               # %for.cond515.preheader
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB7_134
# BB#149:
	movq	%r14, %r15
	xorq	%r14, %r14
	movq	-144(%rbp), %r9         # 8-byte Reload
	jmp	.LBB7_150
.LBB7_157:                              # %if.then564
                                        #   in Loop: Header=BB7_150 Depth=1
	movq	%r8, %rbx
	xorq	%rcx, %rcx
	movq	-120(%rbp), %rdi        # 8-byte Reload
	movq	%r13, %rsi
	callq	emit_pop_insn
	movq	-136(%rbp), %r11        # 8-byte Reload
	movq	-144(%rbp), %r9         # 8-byte Reload
	movq	%rax, -120(%rbp)        # 8-byte Spill
	jmp	.LBB7_160
	.align	16, 0x90
.LBB7_150:                              # %for.body518
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_155 Depth 2
	movq	recog_data(,%r14,8), %rdx
	movzbl	(%rdx), %eax
	cmpq	$61, %rax
	jne	.LBB7_159
# BB#151:                               # %land.lhs.true525
                                        #   in Loop: Header=BB7_150 Depth=1
	movl	8(%rdx), %eax
	movq	$32, %rcx
	movq	%r11, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%rax, %rsi
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rsi
	testq	%rdi, %rdi
	movabsq	$0, %rcx
	jg	.LBB7_153
# BB#152:                               # %land.lhs.true525
                                        #   in Loop: Header=BB7_150 Depth=1
	movabsq	$1, %rcx
.LBB7_153:                              # %land.lhs.true525
                                        #   in Loop: Header=BB7_150 Depth=1
	cmpq	$8, %rsi
	jne	.LBB7_159
# BB#154:                               # %land.lhs.true525
                                        #   in Loop: Header=BB7_150 Depth=1
	movq	$1, %rsi
	testq	%rcx, %rcx
	jne	.LBB7_159
	.align	16, 0x90
.LBB7_155:                              # %for.body546
                                        #   Parent Loop BB7_150 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-8(%r9,%rsi,8), %rcx
	movl	8(%rcx), %ecx
	cmpq	%rcx, %rax
	jne	.LBB7_158
# BB#156:                               # %land.lhs.true559
                                        #   in Loop: Header=BB7_155 Depth=2
	movl	-4(%r8,%rsi,4), %ecx
	cmpq	$10, %rcx
	je	.LBB7_157
.LBB7_158:                              # %for.inc569
                                        #   in Loop: Header=BB7_155 Depth=2
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%r11, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	incq	%rsi
	cmpq	%rbx, %rdi
	jl	.LBB7_155
	.align	16, 0x90
.LBB7_159:                              #   in Loop: Header=BB7_150 Depth=1
	movq	%r8, %rbx
.LBB7_160:                              # %for.inc573
                                        #   in Loop: Header=BB7_150 Depth=1
	incq	%r14
	movq	%r14, %rax
	andq	%r15, %rax
	movq	%r12, %rcx
	andq	%r15, %rcx
	cmpq	%rcx, %rax
	movq	%rbx, %r8
	jne	.LBB7_150
	jmp	.LBB7_135
.LBB7_134:
	movq	-144(%rbp), %r9         # 8-byte Reload
.LBB7_135:                              # %for.cond576.preheader
	movq	$32, %rcx
	movq	-128(%rbp), %rax        # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$4294967288, %r14       # imm = 0xFFFFFFF8
	jle	.LBB7_164
# BB#136:                               # %for.body580.lr.ph
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movabsq	$0, %r15
	jmp	.LBB7_137
.LBB7_146:                              # %if.then637
                                        #   in Loop: Header=BB7_137 Depth=1
	movq	%rax, %rbx
	xorq	%rcx, %rcx
	movq	-120(%rbp), %rdi        # 8-byte Reload
	movq	%r13, %rsi
	callq	emit_pop_insn
	movq	-136(%rbp), %r11        # 8-byte Reload
	movq	-144(%rbp), %r9         # 8-byte Reload
	movq	%rax, -120(%rbp)        # 8-byte Spill
	jmp	.LBB7_163
	.align	16, 0x90
.LBB7_137:                              # %for.body580
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_143 Depth 2
	movq	%r8, %rbx
	movq	recog_data(,%r12,8), %rdx
	movzbl	(%rdx), %eax
	cmpq	$61, %rax
	jne	.LBB7_163
# BB#138:                               # %land.lhs.true587
                                        #   in Loop: Header=BB7_137 Depth=1
	movl	8(%rdx), %r10d
	movq	$32, %rcx
	movq	%r11, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%r10, %rsi
	andq	%r14, %rsi
	testq	%rdi, %rdi
	movq	%r15, %rcx
	movq	%rbx, %rax
	jg	.LBB7_140
# BB#139:                               # %land.lhs.true587
                                        #   in Loop: Header=BB7_137 Depth=1
	movabsq	$1, %rcx
.LBB7_140:                              # %land.lhs.true587
                                        #   in Loop: Header=BB7_137 Depth=1
	cmpq	$8, %rsi
	jne	.LBB7_162
# BB#141:                               # %land.lhs.true587
                                        #   in Loop: Header=BB7_137 Depth=1
	testq	%rcx, %rcx
	jne	.LBB7_162
# BB#142:                               # %for.body608.lr.ph
                                        #   in Loop: Header=BB7_137 Depth=1
	movq	$1, %rbx
	movq	$1, %r8
	movq	%r10, %rcx
	shlq	%cl, %r8
	.align	16, 0x90
.LBB7_143:                              # %for.body608
                                        #   Parent Loop BB7_137 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-8(%r9,%rbx,8), %rcx
	movl	8(%rcx), %ecx
	cmpq	%rcx, %r10
	jne	.LBB7_161
# BB#144:                               # %land.lhs.true621
                                        #   in Loop: Header=BB7_143 Depth=2
	movl	-4(%rax,%rbx,4), %ecx
	cmpq	$1, %rcx
	jne	.LBB7_161
# BB#145:                               # %land.lhs.true626
                                        #   in Loop: Header=BB7_143 Depth=2
	movq	8(%r13), %rcx
	testq	%r8, %rcx
	jne	.LBB7_146
.LBB7_161:                              # %for.inc642
                                        #   in Loop: Header=BB7_143 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%r11, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rbx
	cmpq	%rdi, %rsi
	jl	.LBB7_143
.LBB7_162:                              #   in Loop: Header=BB7_137 Depth=1
	movq	%rax, %rbx
.LBB7_163:                              # %for.inc646
                                        #   in Loop: Header=BB7_137 Depth=1
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	-80(%rbp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	movq	%rbx, %r8
	jl	.LBB7_137
.LBB7_164:                              # %for.end648
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB7_63:                               # %if.then365
	movabsq	$.L.str2, %rdi
	movabsq	$.L__FUNCTION__.subst_asm_stack_regs, %rdx
	movq	$2043, %rsi             # imm = 0x7FB
	callq	fancy_abort
.LBB7_10:                               # %if.end67.i
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.get_asm_operand_n_inputs, %rcx
	movq	$804, %rsi              # imm = 0x324
	jmp	.LBB7_11
.LBB7_165:                              # %if.then4
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.subst_asm_stack_regs, %rcx
	movq	$1904, %rsi             # imm = 0x770
	jmp	.LBB7_11
.LBB7_76:                               # %if.then270
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.subst_asm_stack_regs, %rcx
	movq	$1998, %rsi             # imm = 0x7CE
	jmp	.LBB7_11
.LBB7_88:                               # %if.then342
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.subst_asm_stack_regs, %rcx
	movq	$2032, %rsi             # imm = 0x7F0
	jmp	.LBB7_11
.LBB7_92:                               # %if.then.i868
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.replace_reg, %rcx
	movq	$851, %rsi              # imm = 0x353
	jmp	.LBB7_11
.LBB7_94:                               # %sw.default.i
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.replace_reg, %rcx
	movq	$855, %rsi              # imm = 0x357
	jmp	.LBB7_11
.LBB7_71:                               # %if.then.i837
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.get_hard_regnum, %rcx
	movq	$900, %rsi              # imm = 0x384
.LBB7_11:                               # %if.end67.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp124:
	.size	subst_asm_stack_regs, .Ltmp124-subst_asm_stack_regs
	.cfi_endproc

	.align	16, 0x90
	.type	subst_stack_regs_pat,@function
subst_stack_regs_pat:                   # @subst_stack_regs_pat
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp131:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp132:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp133:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp134:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp135:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp136:
	.cfi_def_cfa_offset 144
.Ltmp137:
	.cfi_offset %rbx, -48
.Ltmp138:
	.cfi_offset %r12, -40
.Ltmp139:
	.cfi_offset %r13, -32
.Ltmp140:
	.cfi_offset %r14, -24
.Ltmp141:
	.cfi_offset %r15, -16
	movq	%rdx, %r13
	movzbl	(%r13), %eax
	cmpq	$47, %rax
	je	.LBB8_61
# BB#1:                                 # %entry
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	cmpq	$49, %rax
	jne	.LBB8_17
# BB#2:
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movabsq	$4294967288, %r15       # imm = 0xFFFFFFF8
	movq	%r13, %rdx
	.align	16, 0x90
.LBB8_3:                                # %for.cond.i1229
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %r12
	movq	8(%r12), %rdx
	movl	(%rdx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$123, %rcx
	je	.LBB8_3
# BB#4:                                 # %for.cond.i1229
                                        #   in Loop: Header=BB8_3 Depth=1
	leaq	-125(%rcx), %rsi
	andq	%r14, %rsi
	cmpq	$2, %rsi
	jb	.LBB8_3
# BB#5:                                 # %for.cond.i1229
                                        #   in Loop: Header=BB8_3 Depth=1
	cmpq	$63, %rcx
	jne	.LBB8_9
# BB#6:                                 # %sw.bb.i1235
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	8(%rdx), %rbx
	movl	(%rbx), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB8_3
# BB#7:                                 # %land.lhs.true.i1238
                                        #   in Loop: Header=BB8_3 Depth=1
	movl	8(%rbx), %edi
	movq	%rdi, %rcx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_3
# BB#8:                                 # %if.then.i1254
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movl	16(%rdx), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rcx
	callq	subreg_regno_offset
	movl	8(%rbx), %ecx
	movzbl	2(%rbx), %edx
	leaq	-8(%rax,%rcx), %rax
	andq	%r14, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %rdx
	movq	%rdx, 8(%r12)
	movl	(%rdx), %eax
.LBB8_9:                                # %get_true_reg.exit1258
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB8_392
# BB#10:                                # %land.lhs.true29
	movl	8(%rdx), %eax
	andq	%r15, %rax
	cmpq	$8, %rax
	jne	.LBB8_392
# BB#11:                                # %if.then39
	movq	$1, %rsi
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	32(%rbx), %rcx
	movq	%rax, %rbx
	cmpq	%r13, %rcx
	je	.LBB8_43
# BB#12:                                # %if.then45
	movq	8(%r12), %rdx
	testq	%rbx, %rbx
	je	.LBB8_39
# BB#13:                                # %if.then47
	movq	$1, %rcx
	movq	48(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	emit_pop_insn
	jmp	.LBB8_14
.LBB8_61:                               # %sw.bb109
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	leaq	8(%r13), %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movabsq	$4294967288, %r9        # imm = 0xFFFFFFF8
	jmp	.LBB8_62
	.align	16, 0x90
.LBB8_394:                              # %sw.bb37.i1347
                                        #   in Loop: Header=BB8_62 Depth=1
	addq	$8, %rdx
	movq	%rdx, %rbx
.LBB8_62:                               # %for.cond.i1318
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rdx
	movl	(%rdx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	leaq	-125(%rcx), %rsi
	andq	%r14, %rsi
	cmpq	$2, %rsi
	jb	.LBB8_394
# BB#63:                                # %for.cond.i1318
                                        #   in Loop: Header=BB8_62 Depth=1
	cmpq	$123, %rcx
	je	.LBB8_394
# BB#64:                                # %for.cond.i1318
                                        #   in Loop: Header=BB8_62 Depth=1
	cmpq	$63, %rcx
	jne	.LBB8_68
# BB#65:                                # %sw.bb.i1324
                                        #   in Loop: Header=BB8_62 Depth=1
	movq	8(%rdx), %r12
	movl	(%r12), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB8_394
# BB#66:                                # %land.lhs.true.i1327
                                        #   in Loop: Header=BB8_62 Depth=1
	movl	8(%r12), %edi
	movq	%rdi, %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_394
# BB#67:                                # %if.then.i1343
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movl	16(%rdx), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rcx
	movq	%r9, %r15
	callq	subreg_regno_offset
	movq	%r15, %r9
	movl	8(%r12), %ecx
	movzbl	2(%r12), %edx
	leaq	-8(%rax,%rcx), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	imulq	$472, %rcx, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %rax
	movq	%rax, (%rbx)
.LBB8_68:                               # %get_true_reg.exit1348
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	leaq	16(%r13), %r12
	movq	%r12, %r15
	jmp	.LBB8_69
	.align	16, 0x90
.LBB8_395:                              # %sw.bb37.i1381
                                        #   in Loop: Header=BB8_69 Depth=1
	addq	$8, %rdx
	movq	%rdx, %r15
.LBB8_69:                               # %for.cond.i1352
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r15), %rdx
	movl	(%rdx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	leaq	-125(%rcx), %rsi
	andq	%r14, %rsi
	cmpq	$2, %rsi
	jb	.LBB8_395
# BB#70:                                # %for.cond.i1352
                                        #   in Loop: Header=BB8_69 Depth=1
	cmpq	$123, %rcx
	je	.LBB8_395
# BB#71:                                # %for.cond.i1352
                                        #   in Loop: Header=BB8_69 Depth=1
	cmpq	$63, %rcx
	jne	.LBB8_75
# BB#72:                                # %sw.bb.i1358
                                        #   in Loop: Header=BB8_69 Depth=1
	movq	8(%rdx), %rbx
	movl	(%rbx), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB8_395
# BB#73:                                # %land.lhs.true.i1361
                                        #   in Loop: Header=BB8_69 Depth=1
	movl	8(%rbx), %edi
	movq	%rdi, %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_395
# BB#74:                                # %if.then.i1377
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movl	16(%rdx), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rcx
	callq	subreg_regno_offset
	movabsq	$4294967288, %r9        # imm = 0xFFFFFFF8
	movl	8(%rbx), %ecx
	movzbl	2(%rbx), %edx
	leaq	-8(%rax,%rcx), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	imulq	$472, %rcx, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %rdx
	movq	%rdx, (%r15)
	movl	(%rdx), %eax
.LBB8_75:                               # %get_true_reg.exit1382
	movq	(%r12), %r12
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	movq	56(%rsp), %rsi          # 8-byte Reload
	jne	.LBB8_77
# BB#76:                                # %land.lhs.true124
	movl	8(%rdx), %ecx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	je	.LBB8_81
.LBB8_77:                               # %lor.lhs.false134
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rbx
	movl	(%rbx), %edi
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB8_83
# BB#78:                                # %land.lhs.true138
	movl	8(%rbx), %ecx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_83
# BB#79:                                # %land.lhs.true148
	addq	$-55, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$11, %rcx
	ja	.LBB8_83
# BB#80:                                # %land.lhs.true148
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$2113, %rax             # imm = 0x841
	je	.LBB8_83
.LBB8_81:                               # %if.then160
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rdx
.LBB8_82:                               # %if.then160
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	move_for_stack_reg  # TAILCALL
.LBB8_17:                               # %entry
	cmpq	$48, %rax
	jne	.LBB8_392
# BB#18:
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movabsq	$4294967288, %r14       # imm = 0xFFFFFFF8
	.align	16, 0x90
.LBB8_19:                               # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%r13, %r12
	movq	8(%r12), %r13
	movl	(%r13), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$123, %rcx
	je	.LBB8_19
# BB#20:                                # %for.cond.i
                                        #   in Loop: Header=BB8_19 Depth=1
	leaq	-125(%rcx), %rdx
	andq	%r15, %rdx
	cmpq	$2, %rdx
	jb	.LBB8_19
# BB#21:                                # %for.cond.i
                                        #   in Loop: Header=BB8_19 Depth=1
	cmpq	$63, %rcx
	jne	.LBB8_25
# BB#22:                                # %sw.bb.i
                                        #   in Loop: Header=BB8_19 Depth=1
	movq	8(%r13), %rbx
	movl	(%rbx), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB8_19
# BB#23:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB8_19 Depth=1
	movl	8(%rbx), %edi
	movq	%rdi, %rcx
	andq	%r14, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_19
# BB#24:                                # %if.then.i
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movl	16(%r13), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rcx
	callq	subreg_regno_offset
	movl	8(%rbx), %ecx
	movzbl	2(%rbx), %edx
	leaq	-8(%rax,%rcx), %rax
	andq	%r15, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %r13
	movq	%r13, 8(%r12)
	movl	(%r13), %eax
.LBB8_25:                               # %get_true_reg.exit
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	movq	56(%rsp), %rbx          # 8-byte Reload
	jne	.LBB8_29
# BB#26:                                # %land.lhs.true
	movl	8(%r13), %edx
	movq	%rdx, %rcx
	andq	%r14, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_29
# BB#27:                                # %land.lhs.true11
	movq	$1, %rsi
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	find_regno_note
	movq	8(%r12), %r13
	testq	%rax, %rax
	je	.LBB8_28
# BB#393:                               # %if.then
	xorq	%rcx, %rcx
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r13, %rdx
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_pop_insn  # TAILCALL
.LBB8_83:                               # %if.end161
	movzbl	(%r12), %ecx
	cmpq	$123, %rcx
	jg	.LBB8_102
# BB#84:                                # %if.end161
	cmpq	$49, %rcx
	jg	.LBB8_91
# BB#85:                                # %if.end161
	cmpq	$42, %rcx
	jne	.LBB8_104
# BB#86:                                # %sw.bb628
	movl	16(%r12), %eax
	cmpq	$10, %rax
	je	.LBB8_298
# BB#87:                                # %sw.bb628
	cmpq	$9, %rax
	je	.LBB8_88
# BB#279:                               # %sw.bb628
	decq	%rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	cmpq	$1, %rax
	ja	.LBB8_303
# BB#280:                               # %sw.bb631
	movq	8(%r12), %r15
	addq	$8, %r15
	jmp	.LBB8_281
	.align	16, 0x90
.LBB8_398:                              # %sw.bb37.i1985
                                        #   in Loop: Header=BB8_281 Depth=1
	addq	$8, %rdx
	movq	%rdx, %r15
.LBB8_281:                              # %for.cond.i1956
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r15), %rdx
	movl	(%rdx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	leaq	-125(%rcx), %rsi
	andq	%r14, %rsi
	cmpq	$2, %rsi
	jb	.LBB8_398
# BB#282:                               # %for.cond.i1956
                                        #   in Loop: Header=BB8_281 Depth=1
	cmpq	$123, %rcx
	je	.LBB8_398
# BB#283:                               # %for.cond.i1956
                                        #   in Loop: Header=BB8_281 Depth=1
	cmpq	$63, %rcx
	jne	.LBB8_284
# BB#285:                               # %sw.bb.i1962
                                        #   in Loop: Header=BB8_281 Depth=1
	movq	8(%rdx), %rbx
	movl	(%rbx), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB8_398
# BB#286:                               # %land.lhs.true.i1965
                                        #   in Loop: Header=BB8_281 Depth=1
	movl	8(%rbx), %edi
	movq	%rdi, %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_398
# BB#287:                               # %if.then.i1981
	movq	%r9, %r12
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movl	16(%rdx), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rcx
	callq	subreg_regno_offset
	movl	8(%rbx), %ecx
	movzbl	2(%rbx), %edx
	leaq	-8(%rax,%rcx), %rax
	andq	%r14, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %rdx
	movq	%rdx, (%r15)
	jmp	.LBB8_288
.LBB8_102:                              # %if.end161
	addq	$-124, %rcx
	andq	%r14, %rcx
	cmpq	$6, %rcx
	ja	.LBB8_104
# BB#103:                               # %if.end161
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$97, %rax
	jne	.LBB8_123
	jmp	.LBB8_104
.LBB8_91:                               # %if.end161
	cmpq	$60, %rcx
	jg	.LBB8_100
# BB#92:                                # %if.end161
	cmpq	$50, %rcx
	jne	.LBB8_104
# BB#93:                                # %sw.bb165
	movl	8(%rbx), %esi
	movq	%rsi, %rdx
	andq	%r9, %rdx
	movq	$16, %rcx
	movq	%rdi, %rax
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB8_97
# BB#94:                                # %sw.bb165
	leaq	-21(%rsi), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB8_97
# BB#95:                                # %sw.bb165
	leaq	-45(%rsi), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB8_97
# BB#96:                                # %sw.bb165
	leaq	-29(%rsi), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB8_97
# BB#108:                               # %cond.false
	cmpq	$24, %rax
	jne	.LBB8_109
# BB#112:                               # %cond.true229
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	movq	40(%rsp), %r10          # 8-byte Reload
	jmp	.LBB8_113
.LBB8_43:                               # %if.else55
	testq	%rbx, %rbx
	movq	56(%rsp), %rbx          # 8-byte Reload
	jne	.LBB8_392
# BB#44:                                # %land.lhs.true57
	movq	8(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB8_31
# BB#45:                                # %land.lhs.true.i1272
	movl	8(%rax), %eax
	movq	%rax, %rcx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_31
# BB#46:                                # %if.end.i1276
	movslq	(%rbx), %rdx
	testq	%rdx, %rdx
	js	.LBB8_51
# BB#47:
	movq	%rdx, %rsi
.LBB8_49:                               # %for.body.i1285
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx,%rsi), %ecx
	cmpq	%rax, %rcx
	je	.LBB8_50
# BB#48:                                # %for.cond.i1280
                                        #   in Loop: Header=BB8_49 Depth=1
	movq	$32, %rcx
	leaq	-1(%rsi), %rdi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	movq	%rdi, %rsi
	jg	.LBB8_49
	jmp	.LBB8_51
.LBB8_100:                              # %if.end161
	addq	$-61, %rcx
	andq	%r14, %rcx
	cmpq	$18, %rcx
	ja	.LBB8_104
# BB#101:                               # %if.end161
	jmpq	*.LJTI8_0(,%rcx,8)
.LBB8_144:                              # %sw.bb316
	leaq	8(%r12), %rbx
	jmp	.LBB8_145
	.align	16, 0x90
.LBB8_396:                              # %sw.bb37.i1509
                                        #   in Loop: Header=BB8_145 Depth=1
	addq	$8, %rdx
	movq	%rdx, %rbx
.LBB8_145:                              # %for.cond.i1480
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rdx
	movl	(%rdx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	leaq	-125(%rcx), %rsi
	andq	%r14, %rsi
	cmpq	$2, %rsi
	jb	.LBB8_396
# BB#146:                               # %for.cond.i1480
                                        #   in Loop: Header=BB8_145 Depth=1
	cmpq	$123, %rcx
	je	.LBB8_396
# BB#147:                               # %for.cond.i1480
                                        #   in Loop: Header=BB8_145 Depth=1
	cmpq	$63, %rcx
	jne	.LBB8_148
# BB#149:                               # %sw.bb.i1486
                                        #   in Loop: Header=BB8_145 Depth=1
	movq	8(%rdx), %r15
	movl	(%r15), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB8_396
# BB#150:                               # %land.lhs.true.i1489
                                        #   in Loop: Header=BB8_145 Depth=1
	movl	8(%r15), %edi
	movq	%rdi, %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_396
# BB#151:                               # %if.then.i1505
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	%r9, %r13
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movl	16(%rdx), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rcx
	callq	subreg_regno_offset
	movl	8(%r15), %ecx
	movzbl	2(%r15), %edx
	leaq	-8(%rax,%rcx), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	imulq	$472, %rcx, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %rdx
	movq	%rdx, (%rbx)
	jmp	.LBB8_152
.LBB8_284:
	movq	%r9, %r12
.LBB8_288:                              # %get_true_reg.exit1986
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	56(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rsi
	callq	emit_swap_insn
	movq	(%r15), %rax
	movl	8(%rax), %edx
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	find_regno_note
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rcx
	movl	(%rcx), %edx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$61, %rsi
	movq	%r12, %rdi
	jne	.LBB8_292
# BB#289:                               # %land.lhs.true644
	movl	8(%rcx), %ecx
	andq	%rdi, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_292
# BB#290:                               # %if.end.i1999
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movl	mode_class(,%rdx,4), %ecx
	orq	$4, %rcx
	andq	%r14, %rcx
	cmpq	$6, %rcx
	jne	.LBB8_42
# BB#291:                               # %replace_reg.exit2002
	movq	FP_mode_reg(,%rdx,8), %rcx
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rcx, (%rdx)
.LBB8_292:                              # %if.end657
	testq	%rax, %rax
	je	.LBB8_293
# BB#294:                               # %if.then659
	movq	8(%rax), %rcx
	movl	(%rcx), %edx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$61, %rsi
	jne	.LBB8_16
# BB#295:                               # %land.lhs.true.i2009
	movl	8(%rcx), %ecx
	andq	%rdi, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_16
# BB#296:                               # %if.end.i2015
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movl	mode_class(,%rdx,4), %ecx
	orq	$4, %rcx
	andq	%r14, %rcx
	cmpq	$6, %rcx
	jne	.LBB8_42
# BB#297:                               # %replace_reg.exit2018
	movq	FP_mode_reg(,%rdx,8), %rcx
	movq	%rcx, 8(%rax)
	movl	(%r13), %eax
	decq	%rax
	movl	%eax, (%r13)
	movq	(%r15), %rdx
	movl	8(%rdx), %ecx
	movq	$1, %rax
	shlq	%cl, %rax
	notq	%rax
	andq	%rax, 8(%r13)
	jmp	.LBB8_140
.LBB8_39:                               # %if.else49
	movq	$10, %rsi
	movq	48(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdi
	callq	find_reg_note
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB8_40
.LBB8_14:                               # %if.end54
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	remove_note
	movq	8(%r12), %rcx
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB8_16
# BB#15:                                # %land.lhs.true.i1263
	movl	8(%rcx), %ecx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_16
# BB#41:                                # %if.end.i1266
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	mode_class(,%rax,4), %ecx
	orq	$4, %rcx
	andq	%r14, %rcx
	cmpq	$6, %rcx
	jne	.LBB8_42
# BB#391:                               # %replace_reg.exit
	movq	FP_mode_reg+3304(,%rax,8), %rax
	movq	%rax, 8(%r12)
	jmp	.LBB8_392
.LBB8_28:                               # %land.lhs.true11.if.else_crit_edge
	movl	(%r13), %eax
.LBB8_29:                               # %if.else
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB8_31
# BB#30:                                # %land.lhs.true.i1221
	movl	8(%r13), %edx
	andq	%rdx, %r14
	cmpq	$8, %r14
	jne	.LBB8_31
# BB#33:                                # %if.end.i
	movslq	(%rbx), %rax
	testq	%rax, %rax
	js	.LBB8_38
# BB#34:
	movq	%rax, %rsi
	.align	16, 0x90
.LBB8_36:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx,%rsi), %ecx
	cmpq	%rdx, %rcx
	je	.LBB8_37
# BB#35:                                # %for.cond.i1223
                                        #   in Loop: Header=BB8_36 Depth=1
	movq	$32, %rcx
	leaq	-1(%rsi), %rdi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	movq	%rdi, %rsi
	jg	.LBB8_36
	jmp	.LBB8_38
.LBB8_37:                               # %get_hard_regnum.exit
	addq	$8, %rax
	subq	%rsi, %rax
	andq	%r15, %rax
	cmpq	%r15, %rax
	jne	.LBB8_392
.LBB8_38:                               # %if.then19
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.subst_stack_regs_pat, %rcx
	movq	$1441, %rsi             # imm = 0x5A1
	jmp	.LBB8_32
.LBB8_298:                              # %sw.bb674
	movq	8(%r12), %rax
	movq	8(%rax), %r12
	movzbl	(%r12), %eax
	cmpq	$42, %rax
	movq	48(%rsp), %rdi          # 8-byte Reload
	jne	.LBB8_300
# BB#299:                               # %lor.lhs.false684
	movl	16(%r12), %eax
	cmpq	$9, %rax
	je	.LBB8_89
.LBB8_300:                              # %if.then690
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.subst_stack_regs_pat, %rcx
	movq	$1739, %rsi             # imm = 0x6CB
	jmp	.LBB8_32
.LBB8_88:
	movq	48(%rsp), %rdi          # 8-byte Reload
.LBB8_89:                               # %sw.bb692
	movq	8(%r12), %rax
	movq	8(%rax), %rdx
	movzbl	(%rdx), %eax
	cmpq	$74, %rax
	jne	.LBB8_90
# BB#301:                               # %if.end703
	movq	56(%rsp), %rsi          # 8-byte Reload
.LBB8_302:                              # %if.end703
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	compare_for_stack_reg  # TAILCALL
.LBB8_97:                               # %cond.true
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	movq	40(%rsp), %r10          # 8-byte Reload
	je	.LBB8_113
# BB#98:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB8_99
# BB#106:                               # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB8_113
.LBB8_148:
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	%r9, %r13
.LBB8_152:                              # %get_true_reg.exit1510
	leaq	16(%r12), %r9
	movq	%r13, %r8
	jmp	.LBB8_153
	.align	16, 0x90
.LBB8_397:                              # %sw.bb37.i1543
                                        #   in Loop: Header=BB8_153 Depth=1
	addq	$8, %rbx
	movq	%rbx, %r9
.LBB8_153:                              # %for.cond.i1514
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r9), %rbx
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	leaq	-125(%rcx), %rsi
	andq	%r14, %rsi
	cmpq	$2, %rsi
	jb	.LBB8_397
# BB#154:                               # %for.cond.i1514
                                        #   in Loop: Header=BB8_153 Depth=1
	cmpq	$123, %rcx
	je	.LBB8_397
# BB#155:                               # %for.cond.i1514
                                        #   in Loop: Header=BB8_153 Depth=1
	cmpq	$63, %rcx
	jne	.LBB8_156
# BB#157:                               # %sw.bb.i1520
                                        #   in Loop: Header=BB8_153 Depth=1
	movq	8(%rbx), %r13
	movl	(%r13), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB8_397
# BB#158:                               # %land.lhs.true.i1523
                                        #   in Loop: Header=BB8_153 Depth=1
	movl	8(%r13), %edi
	movq	%rdi, %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_397
# BB#159:                               # %if.then.i1539
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movl	16(%rbx), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rcx
	movq	%r9, %rbx
	callq	subreg_regno_offset
	movq	%rbx, %r9
	movl	8(%r13), %ecx
	movzbl	2(%r13), %edx
	leaq	-8(%rax,%rcx), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	imulq	$472, %rcx, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %rbx
	movq	%rbx, (%r9)
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	(%r13), %rdx
	jmp	.LBB8_160
.LBB8_156:
	movq	24(%rsp), %r13          # 8-byte Reload
.LBB8_160:                              # %get_true_reg.exit1544
	movzbl	(%rdx), %eax
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	cmpq	$61, %rax
	movabsq	$4294967288, %r11       # imm = 0xFFFFFFF8
	jne	.LBB8_161
# BB#162:                               # %land.lhs.true329
	movl	8(%rdx), %edx
	movq	%rdx, %rax
	andq	%r11, %rax
	cmpq	$8, %rax
	movabsq	$0, %rcx
	jne	.LBB8_164
# BB#163:                               # %if.then341
	movq	$1, %rsi
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r11, %r15
	movq	%r9, %rbx
	callq	find_regno_note
	movq	%rbx, %r9
	movq	%r15, %r11
	movq	(%r9), %rbx
	movq	%rax, %rcx
	jmp	.LBB8_164
.LBB8_293:                              # %if.end657.if.end673_crit_edge
	movq	(%r15), %rdx
	jmp	.LBB8_140
.LBB8_104:                              # %sw.default899
	movabsq	$.L.str2, %rdi
	movabsq	$.L__FUNCTION__.subst_stack_regs_pat, %rdx
	movq	$1846, %rsi             # imm = 0x736
	callq	fancy_abort
.LBB8_109:                              # %cond.false
	cmpq	$18, %rax
	movq	40(%rsp), %r10          # 8-byte Reload
	jne	.LBB8_111
# BB#110:                               # %cond.true221
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB8_113
.LBB8_161:
	xorq	%rcx, %rcx
.LBB8_164:                              # %if.end347
	movq	%r9, 32(%rsp)           # 8-byte Spill
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	jne	.LBB8_167
# BB#165:                               # %land.lhs.true352
	movl	8(%rbx), %edx
	movq	%rdx, %rax
	andq	%r11, %rax
	cmpq	$8, %rax
	jne	.LBB8_167
# BB#166:                               # %if.then364
	movq	$1, %rsi
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r11, %r15
	movq	%rcx, %rbx
	callq	find_regno_note
	movq	%rbx, %rcx
	movq	%r15, %r11
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB8_167:                              # %if.end370
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	movq	56(%rsp), %rbx          # 8-byte Reload
	jne	.LBB8_185
# BB#168:                               # %land.lhs.true375
	movl	8(%rax), %edi
	movq	%rdi, %rax
	andq	%r11, %rax
	cmpq	$8, %rax
	jne	.LBB8_185
# BB#169:                               # %lor.lhs.false387
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB8_185
# BB#170:                               # %land.lhs.true392
	movl	8(%rax), %esi
	movq	%rsi, %rax
	andq	%r11, %rax
	cmpq	$8, %rax
	jne	.LBB8_185
# BB#171:                               # %if.end.i1554
	movslq	(%rbx), %r9
	testq	%r9, %r9
	js	.LBB8_182
# BB#172:
	movq	%rbx, %rax
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%r9, %rbx
.LBB8_174:                              # %for.body.i1563
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rax,%rbx), %ecx
	cmpq	%rdi, %rcx
	je	.LBB8_175
# BB#173:                               # %for.cond.i1558
                                        #   in Loop: Header=BB8_174 Depth=1
	movq	$32, %rcx
	leaq	-1(%rbx), %rdx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	movq	%rdx, %rbx
	jg	.LBB8_174
	jmp	.LBB8_176
.LBB8_50:                               # %get_hard_regnum.exit1290
	addq	$8, %rdx
	subq	%rsi, %rdx
	andq	%r14, %rdx
	cmpq	%r14, %rdx
	jne	.LBB8_52
.LBB8_51:                               # %if.then60
	addq	$-8, %rax
	andq	%r14, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg+120(%rax), %rdx
	movq	nan(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%rax, 32(%rdi)
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	move_for_stack_reg
.LBB8_52:                               # %land.lhs.true72
	movq	8(%r12), %rax
	movzbl	2(%rax), %ecx
	movl	mode_class(,%rcx,4), %ecx
	addq	$-5, %rcx
	andq	%r14, %rcx
	cmpq	$1, %rcx
	ja	.LBB8_392
# BB#53:                                # %land.lhs.true84
	movl	8(%rax), %r8d
	leaq	-8(%r8), %rcx
	andq	%r14, %rcx
	imulq	$472, %rcx, %rcx        # imm = 0x1D8
	movq	FP_mode_reg+128(%rcx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB8_31
# BB#54:                                # %land.lhs.true.i1296
	movl	8(%rcx), %esi
	andq	%rsi, %r15
	cmpq	$8, %r15
	jne	.LBB8_31
# BB#55:                                # %if.end.i1300
	movslq	(%rbx), %rdx
	testq	%rdx, %rdx
	js	.LBB8_60
# BB#56:
	movq	%rdx, %rdi
.LBB8_58:                               # %for.body.i1309
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx,%rdi), %ecx
	cmpq	%rsi, %rcx
	je	.LBB8_59
# BB#57:                                # %for.cond.i1304
                                        #   in Loop: Header=BB8_58 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	leaq	-1(%rdi), %rbx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rbx, %rdi
	movq	%rax, %rbx
	jg	.LBB8_58
	jmp	.LBB8_60
.LBB8_111:                              # %for.cond.preheader
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jg	.LBB8_113
	jmp	.LBB8_116
.LBB8_99:
	movabsq	$0, %rax
	incq	%rax
.LBB8_113:                              # %for.body.lr.ph
	movq	56(%rsp), %rbx          # 8-byte Reload
	.align	16, 0x90
.LBB8_114:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	leaq	-1(%rax), %r8
	movq	$32, %rcx
	movq	%r8, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movl	(%rbx), %edx
	leaq	-1(%rsi,%rax), %rsi
	incq	%rdx
	movl	%edx, (%rbx)
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movb	%sil, 16(%rbx,%rdx)
	movq	(%r10), %rdx
	movl	8(%rdx), %esi
	leaq	-1(%rsi,%rax), %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	orq	%rax, 8(%rbx)
	testq	%rdi, %rdi
	movq	%r8, %rax
	jg	.LBB8_114
# BB#115:                               # %for.cond.for.end_crit_edge
	movl	(%rdx), %edi
.LBB8_116:                              # %for.end
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB8_16
# BB#117:                               # %for.end
	andq	%r9, %rsi
	cmpq	$8, %rsi
	jne	.LBB8_16
# BB#118:                               # %if.end.i1394
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movl	mode_class(,%rdi,4), %eax
	orq	$4, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$6, %rcx
	jne	.LBB8_42
# BB#119:                               # %replace_reg.exit1397
	movq	FP_mode_reg(,%rdi,8), %rax
	movq	%rax, (%r10)
	jmp	.LBB8_392
.LBB8_120:                              # %sw.bb265
	movq	global_rtl+8(%rip), %rax
	cmpq	%rax, %rbx
	jne	.LBB8_121
# BB#122:                               # %sw.bb270
	testq	%r15, %r15
	jne	.LBB8_130
	.align	16, 0x90
.LBB8_123:                              # %for.cond.i1401
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, %r15
	movq	8(%r15), %r12
	movl	(%r12), %eax
	addq	$8, %r15
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$123, %rcx
	je	.LBB8_123
# BB#124:                               # %for.cond.i1401
                                        #   in Loop: Header=BB8_123 Depth=1
	leaq	-125(%rcx), %rdx
	andq	%r14, %rdx
	cmpq	$2, %rdx
	jb	.LBB8_123
# BB#125:                               # %for.cond.i1401
                                        #   in Loop: Header=BB8_123 Depth=1
	cmpq	$63, %rcx
	jne	.LBB8_126
# BB#127:                               # %sw.bb.i1407
                                        #   in Loop: Header=BB8_123 Depth=1
	movq	8(%r12), %rbx
	movl	(%rbx), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB8_123
# BB#128:                               # %land.lhs.true.i1410
                                        #   in Loop: Header=BB8_123 Depth=1
	movl	8(%rbx), %edi
	movq	%rdi, %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_123
# BB#129:                               # %if.then.i1426
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movl	16(%r12), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rcx
	movq	%r9, %r12
	callq	subreg_regno_offset
	movq	%r12, %r9
	movl	8(%rbx), %ecx
	movzbl	2(%rbx), %edx
	leaq	-8(%rax,%rcx), %rax
	andq	%r14, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %rdx
	movq	%rdx, (%r15)
	jmp	.LBB8_130
.LBB8_126:
	movq	%r12, %rdx
.LBB8_130:                              # %if.end278
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	56(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rsi
	movq	%r9, %r13
	callq	emit_swap_insn
	movq	(%r15), %rax
	movl	8(%rax), %edx
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	find_regno_note
	movq	%r13, %rdi
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rcx
	movl	(%rcx), %edx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$61, %rsi
	jne	.LBB8_134
# BB#131:                               # %land.lhs.true287
	movl	8(%rcx), %ecx
	andq	%rdi, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_134
# BB#132:                               # %if.end.i1443
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movl	mode_class(,%rdx,4), %ecx
	orq	$4, %rcx
	andq	%r14, %rcx
	cmpq	$6, %rcx
	jne	.LBB8_42
# BB#133:                               # %replace_reg.exit1446
	movq	FP_mode_reg(,%rdx,8), %rcx
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rcx, (%rdx)
.LBB8_134:                              # %if.end300
	testq	%rax, %rax
	je	.LBB8_135
# BB#136:                               # %if.then302
	movq	8(%rax), %rcx
	movl	(%rcx), %edx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$61, %rsi
	jne	.LBB8_16
# BB#137:                               # %land.lhs.true.i1452
	movl	8(%rcx), %ecx
	andq	%rdi, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_16
# BB#138:                               # %if.end.i1458
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movl	mode_class(,%rdx,4), %ecx
	orq	$4, %rcx
	andq	%r14, %rcx
	cmpq	$6, %rcx
	jne	.LBB8_42
# BB#139:                               # %replace_reg.exit1461
	movq	FP_mode_reg(,%rdx,8), %rcx
	movq	%rcx, 8(%rax)
	movl	(%r12), %eax
	decq	%rax
	movl	%eax, (%r12)
	movq	(%r15), %rdx
	movl	8(%rdx), %ecx
	movq	$1, %rax
	shlq	%cl, %rax
	notq	%rax
	andq	%rax, 8(%r12)
	jmp	.LBB8_140
.LBB8_135:                              # %if.end300.if.end315_crit_edge
	movq	(%r15), %rdx
.LBB8_140:                              # %if.end315
	movl	(%rdx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB8_16
# BB#141:                               # %land.lhs.true.i1467
	movl	8(%rdx), %ecx
	andq	%rdi, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_16
# BB#142:                               # %if.end.i1473
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	mode_class(,%rax,4), %ecx
	orq	$4, %rcx
	andq	%r14, %rcx
	cmpq	$6, %rcx
	jne	.LBB8_42
# BB#143:                               # %replace_reg.exit1476
	movq	FP_mode_reg(,%rax,8), %rax
	movq	%rax, (%r15)
.LBB8_392:                              # %sw.epilog902
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB8_304:                              # %sw.bb704
	leaq	16(%r12), %r15
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jmp	.LBB8_305
.LBB8_399:                              # %sw.bb37.i2067
                                        #   in Loop: Header=BB8_305 Depth=1
	addq	$8, %rdx
	movq	%rdx, %r15
.LBB8_305:                              # %for.cond.i2038
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r15), %rdx
	movl	(%rdx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	leaq	-125(%rcx), %rsi
	andq	%r14, %rsi
	cmpq	$2, %rsi
	jb	.LBB8_399
# BB#306:                               # %for.cond.i2038
                                        #   in Loop: Header=BB8_305 Depth=1
	cmpq	$123, %rcx
	je	.LBB8_399
# BB#307:                               # %for.cond.i2038
                                        #   in Loop: Header=BB8_305 Depth=1
	cmpq	$63, %rcx
	jne	.LBB8_308
# BB#309:                               # %sw.bb.i2044
                                        #   in Loop: Header=BB8_305 Depth=1
	movq	8(%rdx), %rbx
	movl	(%rbx), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB8_399
# BB#310:                               # %land.lhs.true.i2047
                                        #   in Loop: Header=BB8_305 Depth=1
	movl	8(%rbx), %edi
	movq	%rdi, %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_399
# BB#311:                               # %if.then.i2063
	movq	%r9, %r13
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movl	16(%rdx), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rcx
	callq	subreg_regno_offset
	movl	8(%rbx), %ecx
	movzbl	2(%rbx), %edx
	leaq	-8(%rax,%rcx), %rax
	andq	%r14, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %rdx
	movq	%rdx, (%r15)
	jmp	.LBB8_312
.LBB8_105:                              # %sw.bb164
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%r12, %rdx
	jmp	.LBB8_302
.LBB8_308:
	movq	%r9, %r13
.LBB8_312:                              # %get_true_reg.exit2068
	leaq	24(%r12), %r14
	movq	%r13, %r8
	jmp	.LBB8_313
.LBB8_400:                              # %sw.bb37.i2101
                                        #   in Loop: Header=BB8_313 Depth=1
	addq	$8, %rbx
	movq	%rbx, %r14
.LBB8_313:                              # %for.cond.i2072
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r14), %rbx
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	leaq	-125(%rcx), %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rsi
	cmpq	$2, %rsi
	jb	.LBB8_400
# BB#314:                               # %for.cond.i2072
                                        #   in Loop: Header=BB8_313 Depth=1
	cmpq	$123, %rcx
	je	.LBB8_400
# BB#315:                               # %for.cond.i2072
                                        #   in Loop: Header=BB8_313 Depth=1
	cmpq	$63, %rcx
	jne	.LBB8_319
# BB#316:                               # %sw.bb.i2078
                                        #   in Loop: Header=BB8_313 Depth=1
	movq	8(%rbx), %r13
	movl	(%r13), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB8_400
# BB#317:                               # %land.lhs.true.i2081
                                        #   in Loop: Header=BB8_313 Depth=1
	movl	8(%r13), %edi
	movq	%rdi, %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_400
# BB#318:                               # %if.then.i2097
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movl	16(%rbx), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rcx
	callq	subreg_regno_offset
	movl	8(%r13), %ecx
	movzbl	2(%r13), %edx
	leaq	-8(%rax,%rcx), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %rax
	movq	%rax, (%r14)
	movq	(%r15), %rdx
.LBB8_319:                              # %get_true_reg.exit2102
	movl	8(%rdx), %edx
	movq	$1, %rsi
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	find_regno_note
	movq	(%r14), %rcx
	movl	8(%rcx), %edx
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	$1, %rsi
	movq	%rbx, %rdi
	movq	%rbx, %r13
	callq	find_regno_note
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rbx
	movzbl	(%rbx), %ecx
	movq	%rax, %r8
	cmpq	$61, %rcx
	movabsq	$4294967288, %r9        # imm = 0xFFFFFFF8
	jne	.LBB8_31
# BB#320:                               # %land.lhs.true.i2108
	movl	8(%rbx), %esi
	movq	%rsi, %rax
	andq	%r9, %rax
	cmpq	$8, %rax
	jne	.LBB8_31
# BB#321:                               # %if.end.i2112
	movq	56(%rsp), %rax          # 8-byte Reload
	movslq	(%rax), %rax
	testq	%rax, %rax
	js	.LBB8_348
# BB#322:
	movq	%rax, %rdx
.LBB8_324:                              # %for.body.i2121
                                        # =>This Inner Loop Header: Depth=1
	movq	56(%rsp), %rcx          # 8-byte Reload
	movzbl	16(%rcx,%rdx), %ecx
	cmpq	%rsi, %rcx
	je	.LBB8_325
# BB#323:                               # %for.cond.i2116
                                        #   in Loop: Header=BB8_324 Depth=1
	movq	$32, %rcx
	leaq	-1(%rdx), %rdi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movq	%rdi, %rdx
	jg	.LBB8_324
	jmp	.LBB8_348
.LBB8_59:                               # %get_hard_regnum.exit1314
	addq	$8, %rdx
	subq	%rdi, %rdx
	andq	%r14, %rdx
	cmpq	%r14, %rdx
	jne	.LBB8_392
.LBB8_60:                               # %if.then94
	addq	$-7, %r8
	andq	%r14, %r8
	imulq	$472, %r8, %rax         # imm = 0x1D8
	movq	FP_mode_reg+120(%rax), %rdx
	movq	nan(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%rax, 32(%rdi)
	movq	%rbx, %rsi
	movq	%rax, %rdx
	jmp	.LBB8_82
.LBB8_325:                              # %get_hard_regnum.exit2126
	leaq	8(%rax), %rdi
	subq	%rdx, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$8, %rdi
	jl	.LBB8_348
# BB#326:                               # %land.lhs.true724
	movq	56(%rsp), %rcx          # 8-byte Reload
	movzbl	16(%rcx,%rax), %edx
	cmpq	%rdx, %rsi
	je	.LBB8_348
# BB#327:                               # %if.then735
	movq	%r8, 24(%rsp)           # 8-byte Spill
	movq	(%r15), %rcx
	movl	8(%rcx), %edi
	cmpq	%rdx, %rdi
	jne	.LBB8_329
# BB#328:                               # %if.then735
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	jne	.LBB8_331
.LBB8_329:                              # %lor.lhs.false748
	movq	(%r14), %rsi
	movl	8(%rsi), %esi
	cmpq	%rdx, %rsi
	jne	.LBB8_346
# BB#330:                               # %lor.lhs.false748
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	je	.LBB8_346
.LBB8_331:                              # %if.then761
	movq	%r9, %rbx
	movzbl	(%rcx), %ecx
	cmpq	$61, %rcx
	jne	.LBB8_31
# BB#332:                               # %if.then761
	movq	%rdi, %rcx
	andq	%rbx, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_31
# BB#333:
	movabsq	$4294967287, %rsi       # imm = 0xFFFFFFF7
	movq	%rdx, %rcx
	movq	%rax, %r9
	jmp	.LBB8_336
.LBB8_175:                              # %cond.true.i1566
	leaq	8(%r9), %r8
	subq	%rbx, %r8
.LBB8_176:                              # %if.end.i1578
	testq	%r9, %r9
	movq	%rax, %rbx
	js	.LBB8_182
# BB#177:
	movq	%r9, %rdi
.LBB8_179:                              # %for.body.i1587
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx,%rdi), %ecx
	cmpq	%rsi, %rcx
	je	.LBB8_180
# BB#178:                               # %for.cond.i1582
                                        #   in Loop: Header=BB8_179 Depth=1
	movq	$32, %rcx
	leaq	-1(%rdi), %rdx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rdx, %rdi
	jg	.LBB8_179
	jmp	.LBB8_182
.LBB8_180:                              # %get_hard_regnum.exit1592
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %r8
	cmpq	%rsi, %r8
	je	.LBB8_182
# BB#181:                               # %get_hard_regnum.exit1592
	movq	$8, %rcx
	subq	%rdi, %rcx
	addq	%r9, %rcx
	andq	%rsi, %rcx
	cmpq	%rsi, %rcx
	je	.LBB8_182
# BB#183:                               # %if.end414
	cmpq	$8, %r8
	je	.LBB8_186
# BB#184:                               # %if.end414
	cmpq	$8, %rcx
	je	.LBB8_186
.LBB8_185:                              # %if.then420
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdx
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r13, %r15
	movq	%r11, %r13
	callq	emit_swap_insn
	movq	%r13, %r11
	movq	%r15, %r13
.LBB8_186:                              # %if.end422
	movq	(%r13), %rcx
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	movq	32(%rsp), %r10          # 8-byte Reload
	jne	.LBB8_195
# BB#187:                               # %land.lhs.true427
	movl	8(%rcx), %edx
	movq	%rdx, %rcx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_195
# BB#188:                               # %if.end.i1602
	movslq	(%rbx), %r8
	testq	%r8, %r8
	js	.LBB8_16
# BB#189:
	movq	%r8, %rdi
.LBB8_191:                              # %for.body.i1611
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx,%rdi), %ecx
	cmpq	%rdx, %rcx
	je	.LBB8_192
# BB#190:                               # %for.cond.i1606
                                        #   in Loop: Header=BB8_191 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rsi
	leaq	-1(%rdi), %rbx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rbx, %rdi
	movq	%rsi, %rbx
	jg	.LBB8_191
	jmp	.LBB8_16
.LBB8_192:                              # %get_hard_regnum.exit1616
	movq	$8, %rdx
	subq	%rdi, %rdx
	addq	%r8, %rdx
	movq	%rdx, %rcx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_16
# BB#193:                               # %if.end.i1628
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	mode_class(,%rax,4), %ecx
	orq	$4, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	cmpq	$6, %rsi
	jne	.LBB8_42
# BB#194:                               # %replace_reg.exit1632
	addq	$-8, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	imulq	$472, %rdx, %rcx        # imm = 0x1D8
	movq	FP_mode_reg(%rcx,%rax,8), %rax
	movq	%rax, (%r13)
.LBB8_195:                              # %if.end441
	movq	(%r10), %rcx
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB8_204
# BB#196:                               # %land.lhs.true446
	movl	8(%rcx), %edx
	movq	%rdx, %rcx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_204
# BB#197:                               # %if.end.i1642
	movslq	(%rbx), %r8
	testq	%r8, %r8
	js	.LBB8_16
# BB#198:
	movq	%r8, %rdi
.LBB8_200:                              # %for.body.i1651
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx,%rdi), %ecx
	cmpq	%rdx, %rcx
	je	.LBB8_201
# BB#199:                               # %for.cond.i1646
                                        #   in Loop: Header=BB8_200 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rsi
	leaq	-1(%rdi), %rbx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rbx, %rdi
	movq	%rsi, %rbx
	jg	.LBB8_200
	jmp	.LBB8_16
.LBB8_201:                              # %get_hard_regnum.exit1656
	movq	$8, %rdx
	subq	%rdi, %rdx
	addq	%r8, %rdx
	movq	%rdx, %rcx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_16
# BB#202:                               # %if.end.i1669
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	mode_class(,%rax,4), %ecx
	orq	$4, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	cmpq	$6, %rsi
	jne	.LBB8_42
# BB#203:                               # %replace_reg.exit1674
	addq	$-8, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	imulq	$472, %rdx, %rcx        # imm = 0x1D8
	movq	FP_mode_reg(%rcx,%rax,8), %rax
	movq	%rax, (%r10)
.LBB8_204:                              # %if.end460
	movq	16(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	je	.LBB8_233
# BB#205:                               # %if.then462
	movq	8(%rax), %rsi
	movq	%rax, %r15
	movslq	(%rbx), %r8
	movl	8(%rsi), %eax
	movzbl	16(%rbx,%r8), %ecx
	cmpq	%rcx, %rax
	jne	.LBB8_215
# BB#206:                               # %if.then476
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	movl	8(%rdi), %eax
	movq	8(%rbx), %r9
	movq	%rbx, %rdx
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	orq	%rbx, %r9
	movq	%r9, 8(%rdx)
	movl	(%rdi), %edi
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB8_31
# BB#207:                               # %if.then476
	movq	%rax, %rcx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_31
# BB#208:                               # %if.end.i1684
	movq	%rdx, %rsi
	testq	%r8, %r8
	js	.LBB8_16
# BB#209:
	movq	%r8, %rbx
.LBB8_211:                              # %for.body.i1693
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rsi,%rbx), %ecx
	cmpq	%rax, %rcx
	je	.LBB8_212
# BB#210:                               # %for.cond.i1688
                                        #   in Loop: Header=BB8_211 Depth=1
	movq	$32, %rcx
	leaq	-1(%rbx), %rdx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	movq	%rdx, %rbx
	jg	.LBB8_211
	jmp	.LBB8_16
.LBB8_233:                              # %if.else521
	movq	8(%rsp), %rcx           # 8-byte Reload
	testq	%rcx, %rcx
	je	.LBB8_259
# BB#234:                               # %if.then523
	movq	%r10, %r14
	movq	8(%rcx), %rax
	movq	%rcx, %r15
	movslq	(%rbx), %r10
	movl	8(%rax), %esi
	movzbl	16(%rbx,%r10), %ecx
	cmpq	%rcx, %rsi
	jne	.LBB8_244
# BB#235:                               # %if.then537
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	movl	8(%rdi), %eax
	movq	8(%rbx), %r8
	movq	%rbx, %rdx
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	orq	%rbx, %r8
	movq	%r8, 8(%rdx)
	movl	(%rdi), %r9d
	movq	%r9, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB8_31
# BB#236:                               # %if.then537
	movq	%rax, %rcx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_31
# BB#237:                               # %if.end.i1784
	movq	%rdx, %rdi
	testq	%r10, %r10
	js	.LBB8_16
# BB#238:
	movq	%r10, %rbx
.LBB8_240:                              # %for.body.i1793
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rdi,%rbx), %ecx
	cmpq	%rax, %rcx
	je	.LBB8_241
# BB#239:                               # %for.cond.i1788
                                        #   in Loop: Header=BB8_240 Depth=1
	movq	$32, %rcx
	leaq	-1(%rbx), %rsi
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	movq	%rsi, %rbx
	jg	.LBB8_240
	jmp	.LBB8_16
.LBB8_215:                              # %if.else485
	movzbl	(%rsi), %edx
	cmpq	$61, %rdx
	jne	.LBB8_31
# BB#216:                               # %if.else485
	movq	%rax, %rdx
	andq	%r11, %rdx
	cmpq	$8, %rdx
	jne	.LBB8_31
# BB#217:                               # %if.end.i1726
	testq	%r8, %r8
	js	.LBB8_223
# BB#218:
	movq	%r8, %rsi
	jmp	.LBB8_221
.LBB8_220:                              # %for.cond.i1730.for.body.i1735_crit_edge
                                        #   in Loop: Header=BB8_221 Depth=1
	movzbl	15(%rbx,%rsi), %ecx
	decq	%rsi
.LBB8_221:                              # %for.body.i1735
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rcx
	cmpq	%rax, %rcx
	je	.LBB8_222
# BB#219:                               # %for.cond.i1730
                                        #   in Loop: Header=BB8_221 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jg	.LBB8_220
	jmp	.LBB8_223
.LBB8_212:                              # %get_hard_regnum.exit1698
	movq	$8, %rax
	subq	%rbx, %rax
	addq	%r8, %rax
	movq	%rax, %rcx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_16
# BB#213:                               # %if.end.i1711
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movl	mode_class(,%rdi,4), %ecx
	orq	$4, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$6, %rdx
	jne	.LBB8_42
# BB#214:                               # %replace_reg.exit1716
	addq	$-8, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdi,8), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
	movq	%rsi, %rbx
	jmp	.LBB8_228
.LBB8_259:                              # %if.else583
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdx
	movl	8(%rdx), %eax
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	orq	%rsi, 8(%rbx)
	movl	(%rdx), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB8_31
# BB#260:                               # %if.else583
	movq	%rax, %rcx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_31
# BB#261:                               # %if.end.i1884
	movslq	(%rbx), %r8
	testq	%r8, %r8
	js	.LBB8_16
# BB#262:
	movq	%r8, %rdi
.LBB8_264:                              # %for.body.i1893
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx,%rdi), %ecx
	cmpq	%rax, %rcx
	je	.LBB8_265
# BB#263:                               # %for.cond.i1888
                                        #   in Loop: Header=BB8_264 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rsi
	leaq	-1(%rdi), %rbx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rbx, %rdi
	movq	%rsi, %rbx
	jg	.LBB8_264
	jmp	.LBB8_16
.LBB8_265:                              # %get_hard_regnum.exit1898
	movq	$8, %rax
	subq	%rdi, %rax
	addq	%r8, %rax
	movq	%rax, %rcx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_16
# BB#266:                               # %if.end.i1911
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movl	mode_class(,%rdx,4), %ecx
	orq	$4, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	cmpq	$6, %rsi
	jne	.LBB8_42
# BB#267:                               # %replace_reg.exit1916
	addq	$-8, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
	jmp	.LBB8_268
.LBB8_244:                              # %if.else546
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB8_31
# BB#245:                               # %if.else546
	movq	%rsi, %rax
	andq	%r11, %rax
	cmpq	$8, %rax
	jne	.LBB8_31
# BB#246:                               # %if.end.i1826
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%r10, %r10
	js	.LBB8_253
# BB#247:
	movq	%rbx, %rdx
	movq	%r10, %rbx
	jmp	.LBB8_250
.LBB8_249:                              # %for.cond.i1830.for.body.i1835_crit_edge
                                        #   in Loop: Header=BB8_250 Depth=1
	movzbl	15(%rdx,%rbx), %ecx
	decq	%rbx
.LBB8_250:                              # %for.body.i1835
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rcx
	cmpq	%rsi, %rcx
	je	.LBB8_251
# BB#248:                               # %for.cond.i1830
                                        #   in Loop: Header=BB8_250 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	jg	.LBB8_249
	jmp	.LBB8_252
.LBB8_241:                              # %get_hard_regnum.exit1798
	movq	$8, %rax
	subq	%rbx, %rax
	addq	%r10, %rax
	movq	%rax, %rcx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_16
# BB#242:                               # %if.end.i1811
	movq	$16, %rcx
	shrq	%cl, %r9
	andq	$255, %r9
	movl	mode_class(,%r9,4), %ecx
	orq	$4, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$6, %rdx
	movq	%r14, %r10
	jne	.LBB8_42
# BB#243:                               # %replace_reg.exit1816
	addq	$-8, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%r9,8), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
	movq	%rdi, %rbx
	jmp	.LBB8_258
.LBB8_222:                              # %cond.true.i1738
	leaq	8(%r8), %r14
	subq	%rsi, %r14
.LBB8_223:                              # %get_hard_regnum.exit1740
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	movq	$1, %rdx
	shlq	%cl, %rdx
	orq	%rdx, 8(%rbx)
	movq	%r14, %rdx
	andq	%r11, %rdx
	cmpq	$8, %rdx
	jne	.LBB8_16
# BB#224:                               # %lor.lhs.false2.i1744
	movl	(%rax), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB8_16
# BB#225:                               # %lor.lhs.false2.i1744
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_16
# BB#226:                               # %if.end.i1753
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	mode_class(,%rax,4), %ecx
	orq	$4, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$6, %rdx
	jne	.LBB8_42
# BB#227:                               # %replace_reg.exit1758
	addq	$-8, %r14
	movq	$32, %rcx
	movq	%r8, %rdx
	subq	%r14, %rdx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	imulq	$472, %r14, %rcx        # imm = 0x1D8
	movq	FP_mode_reg(%rcx,%rax,8), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
	movzbl	16(%rbx,%r8), %eax
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movb	%al, 16(%rbx,%rdx)
	movq	8(%rbx), %r9
.LBB8_228:                              # %if.end504
	movq	8(%r15), %rax
	movl	8(%rax), %ecx
	movq	$1, %rdx
	shlq	%cl, %rdx
	notq	%rdx
	andq	%r9, %rdx
	jmp	.LBB8_229
.LBB8_251:                              # %cond.true.i1838
	leaq	8(%r10), %rax
	subq	%rbx, %rax
.LBB8_252:                              # %get_hard_regnum.exit1840
	movq	%rdx, %rbx
.LBB8_253:                              # %get_hard_regnum.exit1840
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rsi
	movl	8(%rsi), %ecx
	movq	$1, %rdi
	shlq	%cl, %rdi
	orq	%rdi, 8(%rbx)
	movq	%rax, %rdi
	andq	%r11, %rdi
	cmpq	$8, %rdi
	jne	.LBB8_16
# BB#254:                               # %lor.lhs.false2.i1844
	movl	(%rsi), %esi
	movq	%rsi, %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$61, %rdi
	jne	.LBB8_16
# BB#255:                               # %lor.lhs.false2.i1844
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_16
# BB#256:                               # %if.end.i1853
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movl	mode_class(,%rsi,4), %ecx
	orq	$4, %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rcx, %rdi
	cmpq	$6, %rdi
	jne	.LBB8_42
# BB#257:                               # %replace_reg.exit1858
	addq	$-8, %rax
	movq	$32, %rcx
	movq	%r10, %rdi
	subq	%rax, %rdi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rsi,8), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
	movzbl	16(%rbx,%r10), %eax
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movb	%al, 16(%rbx,%rdi)
	movq	8(%rbx), %r8
	movq	%r14, %r10
.LBB8_258:                              # %if.end566
	movq	8(%r15), %rax
	movl	8(%rax), %ecx
	movq	$1, %rdx
	shlq	%cl, %rdx
	notq	%rdx
	andq	%r8, %rdx
.LBB8_229:                              # %if.end504
	movq	%rdx, 8(%rbx)
	movl	(%rax), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB8_16
# BB#230:                               # %if.end504
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_16
# BB#231:                               # %if.end.i1771
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	mode_class(,%rax,4), %ecx
	orq	$4, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$6, %rdx
	jne	.LBB8_42
# BB#232:                               # %replace_reg.exit1774
	movl	(%rbx), %ecx
	movq	FP_mode_reg(,%rax,8), %rax
	movq	%rax, 8(%r15)
	decq	%rcx
	movl	%ecx, (%rbx)
.LBB8_268:                              # %if.end593
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$99, %rax
	jne	.LBB8_392
# BB#269:                               # %land.lhs.true601
	movq	(%r13), %rax
	movl	(%rax), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB8_392
# BB#270:                               # %land.lhs.true606
	movq	(%r10), %rcx
	movzbl	(%rcx), %esi
	cmpq	$61, %rsi
	jne	.LBB8_392
# BB#271:                               # %land.lhs.true611
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	(%rsi), %rsi
	movl	8(%rax), %eax
	movl	8(%rsi), %esi
	cmpq	%rsi, %rax
	je	.LBB8_392
# BB#272:                               # %if.then620
	movl	8(%rcx), %edi
	movq	%rdi, %rcx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_16
# BB#273:                               # %if.then620
	movq	%rax, %rcx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_16
# BB#274:                               # %if.end.i1929
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movl	mode_class(,%rdx,4), %ecx
	orq	$4, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$6, %rcx
	jne	.LBB8_42
# BB#275:                               # %lor.lhs.false2.i1938
	addq	$-8, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	imulq	$472, %rdi, %rcx        # imm = 0x1D8
	movq	FP_mode_reg(%rcx,%rdx,8), %rcx
	movq	%rcx, (%r13)
	movq	(%r10), %rcx
	movl	(%rcx), %edx
	movq	%rdx, %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$61, %rdi
	jne	.LBB8_16
# BB#276:                               # %land.lhs.true.i1941
	movl	8(%rcx), %ecx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_16
# BB#277:                               # %if.end.i1947
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movl	mode_class(,%rdx,4), %ecx
	orq	$4, %rcx
	andq	%rsi, %rcx
	cmpq	$6, %rcx
	jne	.LBB8_42
# BB#278:                               # %replace_reg.exit1952
	addq	$-8, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %rax
	movq	%rax, (%r10)
	jmp	.LBB8_392
.LBB8_303:                              # %sw.default
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.subst_stack_regs_pat, %rcx
	movq	$1756, %rsi             # imm = 0x6DC
	jmp	.LBB8_32
.LBB8_40:                               # %if.then52
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.subst_stack_regs_pat, %rcx
	movq	$1468, %rsi             # imm = 0x5BC
	jmp	.LBB8_32
.LBB8_90:                               # %if.then702
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.subst_stack_regs_pat, %rcx
	movq	$1750, %rsi             # imm = 0x6D6
	jmp	.LBB8_32
.LBB8_121:                              # %if.then268
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.subst_stack_regs_pat, %rcx
	movq	$1545, %rsi             # imm = 0x609
	jmp	.LBB8_32
.LBB8_346:                              # %if.else795
	movq	%r13, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r9, %rbx
	callq	emit_swap_insn
	jmp	.LBB8_347
.LBB8_335:                              # %for.cond.i2140.for.body.i2145_crit_edge
                                        #   in Loop: Header=BB8_336 Depth=1
	movq	56(%rsp), %rcx          # 8-byte Reload
	movzbl	15(%rcx,%r9), %ecx
	decq	%r9
.LBB8_336:                              # %for.body.i2145
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rcx
	cmpq	%rdi, %rcx
	je	.LBB8_337
# BB#334:                               # %for.cond.i2140
                                        #   in Loop: Header=BB8_336 Depth=1
	movq	$32, %rcx
	movq	%r9, %r8
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	testq	%r8, %r8
	jg	.LBB8_335
	jmp	.LBB8_338
.LBB8_182:                              # %if.then413
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.subst_stack_regs_pat, %rcx
	movq	$1621, %rsi             # imm = 0x655
	jmp	.LBB8_32
.LBB8_337:                              # %cond.true.i2148
	movq	%rax, %rsi
	subq	%r9, %rsi
.LBB8_338:                              # %get_hard_regnum.exit2150
	movq	(%r14), %rcx
	movzbl	(%rcx), %edi
	cmpq	$61, %rdi
	jne	.LBB8_31
# BB#339:                               # %land.lhs.true.i2156
	movl	8(%rcx), %edi
	movq	%rdi, %rcx
	andq	%rbx, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_31
# BB#340:
	movabsq	$4294967287, %r9        # imm = 0xFFFFFFF7
	movq	%rax, %r8
	jmp	.LBB8_343
.LBB8_342:                              # %for.cond.i2164.for.body.i2169_crit_edge
                                        #   in Loop: Header=BB8_343 Depth=1
	movq	56(%rsp), %rcx          # 8-byte Reload
	movzbl	15(%rcx,%r8), %edx
	decq	%r8
.LBB8_343:                              # %for.body.i2169
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rdx
	cmpq	%rdi, %rdx
	je	.LBB8_344
# BB#341:                               # %for.cond.i2164
                                        #   in Loop: Header=BB8_343 Depth=1
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jg	.LBB8_342
	jmp	.LBB8_345
.LBB8_344:                              # %cond.true.i2172
	movq	%rax, %r9
	subq	%r8, %r9
.LBB8_345:                              # %get_hard_regnum.exit2174
	subq	%rsi, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	56(%rsp), %rsi          # 8-byte Reload
	movb	%dil, 16(%rsi,%rax)
	movq	(%r15), %rax
	movl	(%rsi), %edx
	movzbl	8(%rax), %eax
	subq	%r9, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movb	%al, 16(%rsi,%rdx)
	movq	8(%r12), %rax
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	reversed_comparison_code
	movq	8(%r12), %rcx
	movw	%ax, (%rcx)
.LBB8_347:                              # %if.end797
	movq	%rbx, %r9
	movq	24(%rsp), %r8           # 8-byte Reload
.LBB8_348:                              # %if.end797
	movq	$0, 64(%rsp)
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, 72(%rsp)
	movq	%r8, 80(%rsp)
	movq	(%r15), %rcx
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	movq	56(%rsp), %rbx          # 8-byte Reload
	jne	.LBB8_357
# BB#349:                               # %land.lhs.true807
	movl	8(%rcx), %edx
	movq	%rdx, %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_357
# BB#350:                               # %if.end.i2184
	movslq	(%rbx), %r8
	testq	%r8, %r8
	js	.LBB8_16
# BB#351:
	movq	%r8, %rdi
.LBB8_353:                              # %for.body.i2193
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx,%rdi), %ecx
	cmpq	%rdx, %rcx
	je	.LBB8_354
# BB#352:                               # %for.cond.i2188
                                        #   in Loop: Header=BB8_353 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rsi
	leaq	-1(%rdi), %rbx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rbx, %rdi
	movq	%rsi, %rbx
	jg	.LBB8_353
	jmp	.LBB8_16
.LBB8_354:                              # %get_hard_regnum.exit2198
	movq	$8, %rdx
	subq	%rdi, %rdx
	addq	%r8, %rdx
	movq	%rdx, %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_16
# BB#355:                               # %if.end.i2211
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	mode_class(,%rax,4), %ecx
	orq	$4, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$6, %rcx
	jne	.LBB8_42
# BB#356:                               # %replace_reg.exit2216
	addq	$-8, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	imulq	$472, %rdx, %rcx        # imm = 0x1D8
	movq	FP_mode_reg(%rcx,%rax,8), %rax
	movq	%rax, (%r15)
.LBB8_357:                              # %if.end821
	movq	(%r14), %rcx
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB8_366
# BB#358:                               # %land.lhs.true826
	movl	8(%rcx), %edx
	movq	%rdx, %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_366
# BB#359:                               # %if.end.i2226
	movslq	(%rbx), %r8
	testq	%r8, %r8
	js	.LBB8_16
# BB#360:
	movq	%r8, %rdi
.LBB8_362:                              # %for.body.i2235
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx,%rdi), %ecx
	cmpq	%rdx, %rcx
	je	.LBB8_363
# BB#361:                               # %for.cond.i2230
                                        #   in Loop: Header=BB8_362 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rsi
	leaq	-1(%rdi), %rbx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rbx, %rdi
	movq	%rsi, %rbx
	jg	.LBB8_362
	jmp	.LBB8_16
.LBB8_363:                              # %get_hard_regnum.exit2240
	movq	$8, %rdx
	subq	%rdi, %rdx
	addq	%r8, %rdx
	movq	%rdx, %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_16
# BB#364:                               # %if.end.i2253
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	mode_class(,%rax,4), %ecx
	orq	$4, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$6, %rcx
	jne	.LBB8_42
# BB#365:                               # %replace_reg.exit2258
	addq	$-8, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	imulq	$472, %rdx, %rcx        # imm = 0x1D8
	movq	FP_mode_reg(%rcx,%rax,8), %rax
	movq	%rax, (%r14)
.LBB8_366:                              # %for.cond841.preheader
	movq	%rbx, %r15
	leaq	56(%r13), %r14
	movq	$1, %rbx
.LBB8_367:                              # %for.body844
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_370 Depth 2
	movq	64(%rsp,%rbx,8), %r8
	testq	%r8, %r8
	je	.LBB8_377
# BB#368:                               # %if.then848
                                        #   in Loop: Header=BB8_367 Depth=1
	movq	%r9, %r12
	movq	8(%r8), %rcx
	movslq	(%r15), %rdx
	movl	8(%rcx), %ecx
	movzbl	16(%r15,%rdx), %edx
	cmpq	%rdx, %rcx
	je	.LBB8_375
# BB#369:                               # %if.then866
                                        #   in Loop: Header=BB8_367 Depth=1
	movq	(%r14), %rdx
	testq	%rdx, %rdx
	movq	%r14, %rsi
	je	.LBB8_374
	.align	16, 0x90
.LBB8_370:                              # %for.body.i2263
                                        #   Parent Loop BB8_367 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rdx), %edi
	andq	$16711680, %rdi         # imm = 0xFF0000
	cmpq	$65536, %rdi            # imm = 0x10000
	jne	.LBB8_373
# BB#371:                               # %land.lhs.true.i2264
                                        #   in Loop: Header=BB8_370 Depth=2
	movq	8(%rdx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB8_373
# BB#372:                               # %land.lhs.true7.i
                                        #   in Loop: Header=BB8_370 Depth=2
	movl	8(%rdi), %eax
	cmpq	%rcx, %rax
	je	.LBB8_376
.LBB8_373:                              # %if.else.i
                                        #   in Loop: Header=BB8_370 Depth=2
	movq	16(%rdx), %rax
	movq	%rdx, %rsi
	addq	$16, %rsi
	testq	%rax, %rax
	movq	%rax, %rdx
	jne	.LBB8_370
	jmp	.LBB8_374
.LBB8_376:                              # %remove_regno_note.exit
                                        #   in Loop: Header=BB8_367 Depth=1
	movq	16(%rdx), %rax
	movq	%rax, (%rsi)
	movq	8(%r8), %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	emit_pop_insn
	movq	%r12, %r9
.LBB8_377:                              # %for.inc
                                        #   in Loop: Header=BB8_367 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$3, %rax
	jl	.LBB8_367
# BB#378:                               # %for.end877
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %r8
	movzbl	(%r8), %eax
	cmpq	$61, %rax
	movq	%r15, %rbx
	jne	.LBB8_31
# BB#379:                               # %land.lhs.true.i2274
	movl	8(%r8), %eax
	movq	%rax, %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jne	.LBB8_31
# BB#380:                               # %if.end.i2278
	movslq	(%rbx), %rsi
	testq	%rsi, %rsi
	js	.LBB8_386
# BB#381:
	movq	%rsi, %rdi
.LBB8_383:                              # %for.body.i2287
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx,%rdi), %ecx
	cmpq	%rax, %rcx
	je	.LBB8_384
# BB#382:                               # %for.cond.i2282
                                        #   in Loop: Header=BB8_383 Depth=1
	movq	$32, %rcx
	leaq	-1(%rdi), %rdx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rdx, %rdi
	jg	.LBB8_383
	jmp	.LBB8_386
.LBB8_384:                              # %get_hard_regnum.exit2292
	leaq	8(%rsi), %rdx
	subq	%rdi, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$7, %rdx
	jg	.LBB8_385
.LBB8_386:                              # %if.then881
	incq	%rsi
	movl	%esi, (%rbx)
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movb	%al, 16(%rbx,%rsi)
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	(%rdi), %r8
	movl	8(%r8), %eax
	jmp	.LBB8_387
.LBB8_385:
	movq	40(%rsp), %rdi          # 8-byte Reload
.LBB8_387:                              # %if.end891
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	orq	%rsi, 8(%rbx)
	movl	(%r8), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB8_16
# BB#388:                               # %if.end891
	andq	%r9, %rax
	cmpq	$8, %rax
	jne	.LBB8_16
# BB#389:                               # %if.end.i2305
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movl	mode_class(,%rdx,4), %eax
	orq	$4, %rax
	andq	%r10, %rax
	cmpq	$6, %rax
	jne	.LBB8_42
# BB#390:                               # %replace_reg.exit2308
	movq	FP_mode_reg(,%rdx,8), %rax
	movq	%rax, (%rdi)
	jmp	.LBB8_392
.LBB8_375:                              # %if.else873
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.subst_stack_regs_pat, %rcx
	movq	$1833, %rsi             # imm = 0x729
	jmp	.LBB8_32
.LBB8_374:                              # %for.end.i
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.remove_regno_note, %rcx
	movq	$885, %rsi              # imm = 0x375
	jmp	.LBB8_32
.LBB8_31:                               # %if.then.i1222
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.get_hard_regnum, %rcx
	movq	$900, %rsi              # imm = 0x384
	jmp	.LBB8_32
.LBB8_16:                               # %if.then.i1264
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.replace_reg, %rcx
	movq	$851, %rsi              # imm = 0x353
	jmp	.LBB8_32
.LBB8_42:                               # %sw.default.i
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.replace_reg, %rcx
	movq	$855, %rsi              # imm = 0x357
.LBB8_32:                               # %if.then.i1222
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp142:
	.size	subst_stack_regs_pat, .Ltmp142-subst_stack_regs_pat
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI8_0:
	.quad	.LBB8_120
	.quad	.LBB8_104
	.quad	.LBB8_104
	.quad	.LBB8_104
	.quad	.LBB8_104
	.quad	.LBB8_104
	.quad	.LBB8_104
	.quad	.LBB8_104
	.quad	.LBB8_104
	.quad	.LBB8_104
	.quad	.LBB8_104
	.quad	.LBB8_304
	.quad	.LBB8_104
	.quad	.LBB8_105
	.quad	.LBB8_144
	.quad	.LBB8_144
	.quad	.LBB8_123
	.quad	.LBB8_144
	.quad	.LBB8_144

	.text
	.align	16, 0x90
	.type	emit_pop_insn,@function
emit_pop_insn:                          # @emit_pop_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp149:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp150:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp151:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp152:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp153:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp154:
	.cfi_def_cfa_offset 64
.Ltmp155:
	.cfi_offset %rbx, -48
.Ltmp156:
	.cfi_offset %r12, -40
.Ltmp157:
	.cfi_offset %r13, -32
.Ltmp158:
	.cfi_offset %r14, -24
.Ltmp159:
	.cfi_offset %r15, -16
	movq	%rdx, %r13
	movl	(%r13), %eax
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	$14, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$1020, %rdx             # imm = 0x3FC
	movl	mode_class(%rdx), %ecx
	movq	%rsi, %rbx
	movq	%rdi, %r15
	addq	$-5, %rcx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rcx
	cmpq	$1, %rcx
	ja	.LBB9_21
# BB#1:                                 # %if.then
	movl	8(%r13), %ecx
	leaq	-8(%rcx), %rax
	andq	%r12, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg+128(%rax), %rdx
	movzbl	(%rdx), %eax
	cmpq	$61, %rax
	jne	.LBB9_3
# BB#2:                                 # %land.lhs.true.i
	movl	8(%rdx), %eax
	movabsq	$4294967288, %r14       # imm = 0xFFFFFFF8
	movq	%rax, %rsi
	andq	%r14, %rsi
	cmpq	$8, %rsi
	jne	.LBB9_3
# BB#5:                                 # %if.end.i
	addq	$-7, %rcx
	andq	%r12, %rcx
	imulq	$472, %rcx, %rcx        # imm = 0x1D8
	movslq	(%rbx), %r8
	movq	FP_mode_reg+128(%rcx), %r13
	xorq	%r12, %r12
	testq	%r8, %r8
	js	.LBB9_11
# BB#6:
	movq	%r8, %rdi
	.align	16, 0x90
.LBB9_8:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx,%rdi), %ecx
	cmpq	%rax, %rcx
	je	.LBB9_9
# BB#7:                                 # %for.cond.i
                                        #   in Loop: Header=BB9_8 Depth=1
	movq	$32, %rcx
	leaq	-1(%rdi), %rsi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rsi, %rdi
	jg	.LBB9_8
	jmp	.LBB9_11
.LBB9_21:                               # %if.end28
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB9_3
# BB#22:                                # %land.lhs.true.i96
	movl	8(%r13), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jne	.LBB9_3
# BB#23:                                # %if.end.i100
	movslq	(%rbx), %rdx
	testq	%rdx, %rdx
	js	.LBB9_28
# BB#24:
	movq	%rdx, %rsi
	.align	16, 0x90
.LBB9_26:                               # %for.body.i109
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx,%rsi), %ecx
	cmpq	%rax, %rcx
	je	.LBB9_27
# BB#25:                                # %for.cond.i104
                                        #   in Loop: Header=BB9_26 Depth=1
	movq	$32, %rcx
	leaq	-1(%rsi), %rdi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	movq	%rdi, %rsi
	jg	.LBB9_26
	jmp	.LBB9_28
.LBB9_27:                               # %get_hard_regnum.exit114
	movq	$8, %r14
	subq	%rsi, %r14
	addq	%rdx, %r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$7, %rax
	jle	.LBB9_28
# BB#29:                                # %if.end32
	addq	$-8, %r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg+128(%rax), %rdx
	movq	FP_mode_reg+128(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rax, %rdi
	movq	%r15, %rsi
	testq	%r12, %rcx
	je	.LBB9_30
# BB#31:                                # %if.else
	callq	emit_insn_before
	jmp	.LBB9_32
.LBB9_30:                               # %if.then39
	callq	emit_insn_after
.LBB9_32:                               # %if.end42
	movq	%rax, %r12
	movq	FP_mode_reg+128(%rip), %rdx
	movq	56(%r12), %rcx
	movq	$3, %rdi
	movq	$1, %rsi
	movq	$1, %r15
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%r12)
	movslq	(%rbx), %rax
	movzbl	16(%rbx,%rax), %edx
	subq	%r14, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movb	%dl, 16(%rbx,%rax)
	movl	(%rbx), %eax
	decq	%rax
	movl	%eax, (%rbx)
	movl	8(%r13), %ecx
	shlq	%cl, %r15
	notq	%r15
	andq	%r15, 8(%rbx)
	jmp	.LBB9_33
.LBB9_9:                                # %get_hard_regnum.exit
	addq	$8, %r8
	subq	%rdi, %r8
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	testq	%r8, %r8
	js	.LBB9_11
# BB#10:                                # %if.then19
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	8(%rsp), %rcx           # 8-byte Reload
	callq	emit_pop_insn
	movq	%rax, %r12
.LBB9_11:                               # %if.end
	movzbl	(%r13), %eax
	cmpq	$61, %rax
	jne	.LBB9_3
# BB#12:                                # %land.lhs.true.i120
	movl	8(%r13), %edx
	andq	%rdx, %r14
	cmpq	$8, %r14
	jne	.LBB9_3
# BB#13:                                # %if.end.i124
	movslq	(%rbx), %rax
	testq	%rax, %rax
	js	.LBB9_19
# BB#14:
	movq	%rax, %rsi
	.align	16, 0x90
.LBB9_16:                               # %for.body.i133
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx,%rsi), %ecx
	cmpq	%rdx, %rcx
	je	.LBB9_17
# BB#15:                                # %for.cond.i128
                                        #   in Loop: Header=BB9_16 Depth=1
	movq	$32, %rcx
	leaq	-1(%rsi), %rdi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	movq	%rdi, %rsi
	jg	.LBB9_16
	jmp	.LBB9_19
.LBB9_17:                               # %get_hard_regnum.exit138
	addq	$8, %rax
	subq	%rsi, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB9_19
# BB#18:                                # %if.then23
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	callq	emit_pop_insn
	movq	%rax, %r12
.LBB9_19:                               # %if.end25
	testq	%r12, %r12
	je	.LBB9_20
.LBB9_33:                               # %return
	movq	%r12, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB9_28:                               # %if.then31
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.emit_pop_insn, %rcx
	movq	$946, %rsi              # imm = 0x3B2
	jmp	.LBB9_4
.LBB9_3:                                # %if.then.i
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.get_hard_regnum, %rcx
	movq	$900, %rsi              # imm = 0x384
	jmp	.LBB9_4
.LBB9_20:                               # %if.then26
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.emit_pop_insn, %rcx
	movq	$939, %rsi              # imm = 0x3AB
.LBB9_4:                                # %if.then.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp160:
	.size	emit_pop_insn, .Ltmp160-emit_pop_insn
	.cfi_endproc

	.align	16, 0x90
	.type	move_for_stack_reg,@function
move_for_stack_reg:                     # @move_for_stack_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp167:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp168:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp169:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp170:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp171:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp172:
	.cfi_def_cfa_offset 64
.Ltmp173:
	.cfi_offset %rbx, -48
.Ltmp174:
	.cfi_offset %r12, -40
.Ltmp175:
	.cfi_offset %r13, -32
.Ltmp176:
	.cfi_offset %r14, -24
.Ltmp177:
	.cfi_offset %r15, -16
	movq	%rdx, %r12
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %r10
	leaq	16(%r12), %r14
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movabsq	$4294967288, %r8        # imm = 0xFFFFFFF8
	jmp	.LBB10_1
	.align	16, 0x90
.LBB10_101:                             # %sw.bb37.i
                                        #   in Loop: Header=BB10_1 Depth=1
	addq	$8, %rbx
	movq	%rbx, %r14
.LBB10_1:                               # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r14), %rbx
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	leaq	-125(%rcx), %rdx
	andq	%r9, %rdx
	cmpq	$2, %rdx
	jb	.LBB10_101
# BB#2:                                 # %for.cond.i
                                        #   in Loop: Header=BB10_1 Depth=1
	cmpq	$123, %rcx
	je	.LBB10_101
# BB#3:                                 # %for.cond.i
                                        #   in Loop: Header=BB10_1 Depth=1
	cmpq	$63, %rcx
	jne	.LBB10_4
# BB#5:                                 # %sw.bb.i
                                        #   in Loop: Header=BB10_1 Depth=1
	movq	8(%rbx), %r15
	movl	(%r15), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB10_101
# BB#6:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB10_1 Depth=1
	movl	8(%r15), %edi
	movq	%rdi, %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB10_101
# BB#7:                                 # %if.then.i
	movq	%r10, (%rsp)            # 8-byte Spill
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movl	16(%rbx), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rcx
	movq	%r8, %r13
	movq	%r9, %rbx
	callq	subreg_regno_offset
	movq	%rbx, %r9
	movq	%r13, %r8
	movl	8(%r15), %ecx
	movzbl	2(%r15), %edx
	leaq	-8(%rax,%rcx), %rax
	andq	%r9, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %rbx
	movq	%rbx, (%r14)
	jmp	.LBB10_8
.LBB10_4:
	movq	%r10, (%rsp)            # 8-byte Spill
	.align	16, 0x90
.LBB10_8:                               # %for.cond.i337
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, %r13
	movq	8(%r13), %r12
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$123, %rcx
	je	.LBB10_8
# BB#9:                                 # %for.cond.i337
                                        #   in Loop: Header=BB10_8 Depth=1
	leaq	-125(%rcx), %rdx
	andq	%r9, %rdx
	cmpq	$2, %rdx
	jb	.LBB10_8
# BB#10:                                # %for.cond.i337
                                        #   in Loop: Header=BB10_8 Depth=1
	cmpq	$63, %rcx
	jne	.LBB10_14
# BB#11:                                # %sw.bb.i343
                                        #   in Loop: Header=BB10_8 Depth=1
	movq	8(%r12), %r15
	movl	(%r15), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB10_8
# BB#12:                                # %land.lhs.true.i346
                                        #   in Loop: Header=BB10_8 Depth=1
	movl	8(%r15), %edi
	movq	%rdi, %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB10_8
# BB#13:                                # %if.then.i362
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movl	16(%r12), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rcx
	movq	%r8, %r12
	movq	%r9, %rbx
	callq	subreg_regno_offset
	movq	%rbx, %r9
	movq	%r12, %r8
	movl	8(%r15), %ecx
	movzbl	2(%r15), %edx
	leaq	-8(%rax,%rcx), %rax
	andq	%r9, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %r12
	movq	%r12, 8(%r13)
	movq	(%r14), %rbx
.LBB10_14:                              # %get_true_reg.exit366
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	movq	8(%rsp), %r15           # 8-byte Reload
	jne	.LBB10_87
# BB#15:                                # %land.lhs.true
	movl	8(%rbx), %edx
	movq	%rdx, %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB10_70
# BB#16:                                # %land.lhs.true13
	movzbl	(%r12), %ecx
	cmpq	$61, %rcx
	jne	.LBB10_69
# BB#17:                                # %land.lhs.true17
	movl	8(%r12), %ecx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB10_69
# BB#18:                                # %if.then
	movq	$1, %rsi
	movq	(%rsp), %rdi            # 8-byte Reload
	callq	find_regno_note
	movl	8(%rbx), %r8d
	movl	8(%r12), %edx
	testq	%rax, %rax
	je	.LBB10_39
# BB#19:                                # %if.then31
	movq	%rdx, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	andq	%rax, %r8
	cmpq	%rsi, %r8
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	je	.LBB10_20
# BB#22:                                # %if.end
	movslq	(%r15), %r9
	testq	%r9, %r9
	movq	(%rsp), %r13            # 8-byte Reload
	js	.LBB10_33
# BB#23:
	movq	%r9, %r14
	.align	16, 0x90
.LBB10_25:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%r15,%r14), %ecx
	cmpq	%r8, %rcx
	je	.LBB10_26
# BB#24:                                # %for.cond
                                        #   in Loop: Header=BB10_25 Depth=1
	movq	$32, %rcx
	leaq	-1(%r14), %rdi
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	testq	%r14, %r14
	movq	%rdi, %r14
	jg	.LBB10_25
	jmp	.LBB10_33
.LBB10_69:                              # %if.else
	cmpq	$61, %rax
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	jne	.LBB10_87
.LBB10_70:                              # %land.lhs.true127
	movl	8(%rbx), %eax
	andq	%r8, %rax
	cmpq	$8, %rax
	jne	.LBB10_87
# BB#71:                                # %if.then139
	movq	%r9, %r13
	movq	(%rsp), %r12            # 8-byte Reload
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	emit_swap_insn
	movl	8(%rbx), %edx
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	find_regno_note
	testq	%rax, %rax
	je	.LBB10_76
# BB#72:                                # %if.then145
	movq	8(%rax), %rcx
	movl	(%rcx), %edx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$61, %rsi
	movabsq	$4294967288, %rsi       # imm = 0xFFFFFFF8
	jne	.LBB10_61
# BB#73:                                # %land.lhs.true.i455
	movl	8(%rcx), %ecx
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	jne	.LBB10_61
# BB#74:                                # %if.end.i461
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movl	mode_class(,%rdx,4), %ecx
	orq	$4, %rcx
	andq	%r13, %rcx
	cmpq	$6, %rcx
	jne	.LBB10_63
# BB#75:                                # %replace_reg.exit464
	movq	FP_mode_reg(,%rdx,8), %rcx
	movq	%rcx, 8(%rax)
	movl	(%r15), %eax
	decq	%rax
	movl	%eax, (%r15)
	movl	8(%rbx), %ecx
	movq	$1, %rax
	shlq	%cl, %rax
	notq	%rax
	andq	%rax, 8(%r15)
	jmp	.LBB10_82
.LBB10_87:                              # %if.else200
	movzbl	(%r12), %eax
	cmpq	$61, %rax
	jne	.LBB10_100
# BB#88:                                # %land.lhs.true205
	movl	8(%r12), %eax
	movq	%rax, %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB10_100
# BB#89:                                # %if.end.i490
	movslq	(%r15), %rdx
	testq	%rdx, %rdx
	js	.LBB10_97
# BB#90:
	movq	%rdx, %rsi
	.align	16, 0x90
.LBB10_92:                              # %for.body.i499
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%r15,%rsi), %ecx
	cmpq	%rax, %rcx
	je	.LBB10_93
# BB#91:                                # %for.cond.i494
                                        #   in Loop: Header=BB10_92 Depth=1
	movq	$32, %rcx
	leaq	-1(%rsi), %rdi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	movq	%rdi, %rsi
	jg	.LBB10_92
	jmp	.LBB10_95
.LBB10_93:                              # %get_hard_regnum.exit504
	leaq	8(%rdx), %rdi
	subq	%rsi, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$8, %rdi
	jge	.LBB10_94
.LBB10_95:                              # %if.end222
	cmpq	$8, %rdx
	jge	.LBB10_96
.LBB10_97:                              # %if.end227
	incq	%rdx
	movl	%edx, (%r15)
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movb	%al, 16(%r15,%rdx)
	movl	8(%r12), %ecx
	movq	$1, %rax
	shlq	%cl, %rax
	orq	%rax, 8(%r15)
	movq	8(%r13), %rcx
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB10_61
# BB#98:                                # %land.lhs.true.i511
	movl	8(%rcx), %ecx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB10_61
# BB#99:                                # %if.end.i517
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	mode_class(,%rax,4), %ecx
	orq	$4, %rcx
	andq	%r9, %rcx
.LBB10_67:                              # %if.end.i445
	cmpq	$6, %rcx
	jne	.LBB10_63
# BB#68:                                # %replace_reg.exit448
	movq	FP_mode_reg(,%rax,8), %rax
	movq	%rax, 8(%r13)
	jmp	.LBB10_86
.LBB10_76:                              # %if.else159
	movzbl	2(%rbx), %eax
	leaq	-17(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$1, %rcx
	movabsq	$4294967288, %rsi       # imm = 0xFFFFFFF8
	ja	.LBB10_82
# BB#77:                                # %land.lhs.true170
	movslq	(%r15), %rcx
	cmpq	$6, %rcx
	jg	.LBB10_82
# BB#78:                                # %if.then174
	movq	%rsi, %r15
	movq	FP_mode_reg(,%rax,8), %r13
	movq	%r13, %rdi
	movq	%r13, %rsi
	cmpq	$18, %rax
	jne	.LBB10_80
# BB#79:                                # %if.then185
	callq	gen_movtf
	jmp	.LBB10_81
.LBB10_26:                              # %for.end
	movq	$32, %rcx
	movq	%r14, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	js	.LBB10_33
# BB#27:                                # %lor.lhs.false
	movzbl	(%r12), %ecx
	cmpq	$61, %rcx
	jne	.LBB10_53
# BB#28:                                # %lor.lhs.false
	andq	%rdx, %rax
	cmpq	$8, %rax
	jne	.LBB10_53
# BB#29:
	movq	%r9, %rdi
	.align	16, 0x90
.LBB10_31:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%r15,%rdi), %eax
	cmpq	%rsi, %rax
	je	.LBB10_32
# BB#30:                                # %for.cond.i373
                                        #   in Loop: Header=BB10_31 Depth=1
	movq	$32, %rcx
	leaq	-1(%rdi), %rax
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rax, %rdi
	jg	.LBB10_31
	jmp	.LBB10_34
.LBB10_39:                              # %if.end82
	movq	%rdx, %r10
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r10
	movq	%r8, %rsi
	andq	%rax, %rsi
	cmpq	%r10, %rsi
	movabsq	$4294967288, %r11       # imm = 0xFFFFFFF8
	jne	.LBB10_43
# BB#40:                                # %if.then91
	movq	$10, %rsi
	movq	(%rsp), %rbx            # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r8, %rdx
	callq	find_regno_note
	testq	%rax, %rax
	je	.LBB10_42
# BB#41:                                # %if.then97
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	emit_pop_insn
.LBB10_42:                              # %if.end99
	movq	%rbx, %rdi
	jmp	.LBB10_37
.LBB10_32:                              # %get_hard_regnum.exit
	addq	$8, %r9
	subq	%rdi, %r9
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	cmpq	$8, %r9
	jge	.LBB10_33
.LBB10_34:                              # %if.end55
	movq	$10, %rsi
	movq	%r13, %rdi
	callq	find_regno_note
	testq	%rax, %rax
	je	.LBB10_38
# BB#35:                                # %if.then61
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	emit_pop_insn
	jmp	.LBB10_36
.LBB10_38:                              # %if.end64
	movzbl	8(%r12), %eax
	movb	%al, 16(%r15,%r14)
	movl	8(%r12), %ecx
	movq	$1, %rax
	shlq	%cl, %rax
	movl	8(%rbx), %ecx
	movq	8(%r15), %rdx
	movq	$1, %rsi
	orq	%rax, %rdx
	shlq	%cl, %rsi
	notq	%rsi
	andq	%rdx, %rsi
	movq	%rsi, 8(%r15)
.LBB10_36:                              # %if.then61
	movq	%r13, %rdi
.LBB10_37:                              # %if.then61
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	delete_insn  # TAILCALL
.LBB10_43:                              # %if.end101
	movzbl	(%r12), %eax
	cmpq	$61, %rax
	jne	.LBB10_53
# BB#44:                                # %if.end101
	movq	%rdx, %rax
	andq	%r11, %rax
	cmpq	$8, %rax
	jne	.LBB10_53
# BB#45:                                # %if.end.i385
	movslq	(%r15), %r9
	testq	%r9, %r9
	js	.LBB10_51
# BB#46:
	movq	%r9, %rax
.LBB10_48:                              # %for.body.i394
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%r15,%rax), %ecx
	cmpq	%r10, %rcx
	je	.LBB10_49
# BB#47:                                # %for.cond.i389
                                        #   in Loop: Header=BB10_48 Depth=1
	movq	$32, %rcx
	leaq	-1(%rax), %rdi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rdi, %rax
	jg	.LBB10_48
	jmp	.LBB10_51
.LBB10_80:                              # %if.else187
	callq	gen_movxf
.LBB10_81:                              # %if.end189
	movq	%r12, %rbx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	emit_insn_before
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$1, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
	movq	%r15, %rsi
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB10_82:                              # %if.end199
	movq	(%r14), %rcx
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB10_61
# BB#83:                                # %land.lhs.true.i471
	movl	8(%rcx), %ecx
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	jne	.LBB10_61
# BB#84:                                # %if.end.i477
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	mode_class(,%rax,4), %ecx
	orq	$4, %rcx
	andq	%r13, %rcx
	cmpq	$6, %rcx
	jne	.LBB10_63
# BB#85:                                # %replace_reg.exit480
	movq	FP_mode_reg(,%rax,8), %rax
	movq	%rax, (%r14)
.LBB10_86:                              # %if.end247
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB10_49:                              # %get_hard_regnum.exit399
	leaq	8(%r9), %rdi
	subq	%rax, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$8, %rdi
	jge	.LBB10_50
.LBB10_51:                              # %if.end106
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	jne	.LBB10_53
# BB#52:                                # %if.end106
	andq	%r11, %r8
	cmpq	$8, %r8
	jne	.LBB10_53
# BB#54:                                # %if.end.i409
	testq	%r9, %r9
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	js	.LBB10_61
# BB#55:
	movq	%r9, %rax
	.align	16, 0x90
.LBB10_57:                              # %for.body.i418
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%r15,%rax), %ecx
	cmpq	%rsi, %rcx
	je	.LBB10_58
# BB#56:                                # %for.cond.i413
                                        #   in Loop: Header=BB10_57 Depth=1
	movq	$32, %rcx
	leaq	-1(%rax), %rdi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rdi, %rax
	jg	.LBB10_57
	jmp	.LBB10_61
.LBB10_58:                              # %get_hard_regnum.exit423
	movq	$8, %rsi
	subq	%rax, %rsi
	addq	%r9, %rsi
	movq	%rsi, %rax
	andq	%r11, %rax
	cmpq	$8, %rax
	jne	.LBB10_61
# BB#59:                                # %lor.lhs.false2.i
	movq	(%r14), %rax
	movl	(%rax), %edi
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB10_61
# BB#60:                                # %land.lhs.true.i428
	movl	8(%rax), %eax
	andq	%r11, %rax
	cmpq	$8, %rax
	jne	.LBB10_61
# BB#62:                                # %if.end.i431
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movl	mode_class(,%rdi,4), %eax
	orq	$4, %rax
	andq	%rbx, %rax
	cmpq	$6, %rax
	jne	.LBB10_63
# BB#64:                                # %replace_reg.exit
	addq	$-8, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	imulq	$472, %rsi, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdi,8), %rax
	movq	%rax, (%r14)
	incq	%r9
	movl	%r9d, (%r15)
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movb	%dl, 16(%r15,%r9)
	movl	8(%r12), %ecx
	movq	$1, %rax
	shlq	%cl, %rax
	orq	%rax, 8(%r15)
	movq	8(%r13), %rcx
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB10_61
# BB#65:                                # %land.lhs.true.i439
	movl	8(%rcx), %ecx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB10_61
# BB#66:                                # %if.end.i445
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	mode_class(,%rax,4), %ecx
	orq	$4, %rcx
	andq	%rbx, %rcx
	jmp	.LBB10_67
.LBB10_20:                              # %if.then39
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.move_for_stack_reg, %rcx
	movq	$1085, %rsi             # imm = 0x43D
	jmp	.LBB10_21
.LBB10_100:                             # %if.else244
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.move_for_stack_reg, %rcx
	movq	$1200, %rsi             # imm = 0x4B0
	jmp	.LBB10_21
.LBB10_94:                              # %if.then221
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.move_for_stack_reg, %rcx
	movq	$1190, %rsi             # imm = 0x4A6
	jmp	.LBB10_21
.LBB10_96:                              # %if.then226
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.move_for_stack_reg, %rcx
	movq	$1193, %rsi             # imm = 0x4A9
	jmp	.LBB10_21
.LBB10_33:                              # %if.then54
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.move_for_stack_reg, %rcx
	movq	$1093, %rsi             # imm = 0x445
	jmp	.LBB10_21
.LBB10_50:                              # %if.then105
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.move_for_stack_reg, %rcx
	movq	$1134, %rsi             # imm = 0x46E
	jmp	.LBB10_21
.LBB10_53:                              # %if.then.i372
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.get_hard_regnum, %rcx
	movq	$900, %rsi              # imm = 0x384
	jmp	.LBB10_21
.LBB10_63:                              # %sw.default.i
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.replace_reg, %rcx
	movq	$855, %rsi              # imm = 0x357
	jmp	.LBB10_21
.LBB10_61:                              # %if.then.i429
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.replace_reg, %rcx
	movq	$851, %rsi              # imm = 0x353
.LBB10_21:                              # %if.then39
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp178:
	.size	move_for_stack_reg, .Ltmp178-move_for_stack_reg
	.cfi_endproc

	.align	16, 0x90
	.type	compare_for_stack_reg,@function
compare_for_stack_reg:                  # @compare_for_stack_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp185:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp186:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp187:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp188:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp189:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp190:
	.cfi_def_cfa_offset 80
.Ltmp191:
	.cfi_offset %rbx, -48
.Ltmp192:
	.cfi_offset %r12, -40
.Ltmp193:
	.cfi_offset %r13, -32
.Ltmp194:
	.cfi_offset %r14, -24
.Ltmp195:
	.cfi_offset %r15, -16
	movq	%rdx, %r13
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	leaq	8(%r13), %r14
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movabsq	$4294967288, %r8        # imm = 0xFFFFFFF8
	jmp	.LBB11_1
	.align	16, 0x90
.LBB11_139:                             # %sw.bb37.i
                                        #   in Loop: Header=BB11_1 Depth=1
	addq	$8, %rdx
	movq	%rdx, %r14
.LBB11_1:                               # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r14), %rdx
	movl	(%rdx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	leaq	-125(%rcx), %rsi
	andq	%r15, %rsi
	cmpq	$2, %rsi
	jb	.LBB11_139
# BB#2:                                 # %for.cond.i
                                        #   in Loop: Header=BB11_1 Depth=1
	cmpq	$123, %rcx
	je	.LBB11_139
# BB#3:                                 # %for.cond.i
                                        #   in Loop: Header=BB11_1 Depth=1
	cmpq	$63, %rcx
	jne	.LBB11_7
# BB#4:                                 # %sw.bb.i
                                        #   in Loop: Header=BB11_1 Depth=1
	movq	8(%rdx), %rbx
	movl	(%rbx), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB11_139
# BB#5:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB11_1 Depth=1
	movl	8(%rbx), %edi
	movq	%rdi, %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB11_139
# BB#6:                                 # %if.then.i
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movl	16(%rdx), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rcx
	movq	%r8, %r12
	callq	subreg_regno_offset
	movq	%r12, %r8
	movl	8(%rbx), %ecx
	movzbl	2(%rbx), %edx
	leaq	-8(%rax,%rcx), %rax
	andq	%r15, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %rax
	movq	%rax, (%r14)
.LBB11_7:                               # %get_true_reg.exit
	addq	$16, %r13
	movq	%r13, %r12
	jmp	.LBB11_8
	.align	16, 0x90
.LBB11_140:                             # %sw.bb37.i265
                                        #   in Loop: Header=BB11_8 Depth=1
	addq	$8, %rdx
	movq	%rdx, %r12
.LBB11_8:                               # %for.cond.i237
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r12), %rdx
	movl	(%rdx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	leaq	-125(%rcx), %rsi
	andq	%r15, %rsi
	cmpq	$2, %rsi
	jb	.LBB11_140
# BB#9:                                 # %for.cond.i237
                                        #   in Loop: Header=BB11_8 Depth=1
	cmpq	$123, %rcx
	je	.LBB11_140
# BB#10:                                # %for.cond.i237
                                        #   in Loop: Header=BB11_8 Depth=1
	cmpq	$63, %rcx
	jne	.LBB11_14
# BB#11:                                # %sw.bb.i243
                                        #   in Loop: Header=BB11_8 Depth=1
	movq	8(%rdx), %rbx
	movl	(%rbx), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB11_140
# BB#12:                                # %land.lhs.true.i246
                                        #   in Loop: Header=BB11_8 Depth=1
	movl	8(%rbx), %edi
	movq	%rdi, %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB11_140
# BB#13:                                # %if.then.i262
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movl	16(%rdx), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rcx
	callq	subreg_regno_offset
	movl	8(%rbx), %ecx
	movzbl	2(%rbx), %edx
	leaq	-8(%rax,%rcx), %rax
	andq	%r15, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %rax
	movq	%rax, (%r12)
.LBB11_14:                              # %while.cond.i
	movq	24(%rsp), %rbx          # 8-byte Reload
	.align	16, 0x90
.LBB11_15:                              # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	current_block(%rip), %rax
	movq	8(%rax), %rax
	cmpq	%rax, %rbx
	je	.LBB11_20
# BB#16:                                # %while.body.i
                                        #   in Loop: Header=BB11_15 Depth=1
	movq	24(%rbx), %rbx
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB11_19
# BB#17:                                # %land.lhs.true.i272
                                        #   in Loop: Header=BB11_15 Depth=1
	movq	ix86_flags_rtx(%rip), %rdi
	movq	32(%rbx), %rsi
	callq	reg_mentioned_p
	testq	%r15, %rax
	jne	.LBB11_20
# BB#18:                                # %land.lhs.true.if.end_crit_edge.i
                                        #   in Loop: Header=BB11_15 Depth=1
	movl	(%rbx), %eax
.LBB11_19:                              # %if.end.i
                                        #   in Loop: Header=BB11_15 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$34, %rax
	jne	.LBB11_15
.LBB11_20:                              # %next_flags_user.exit
	movq	(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB11_34
# BB#21:                                # %land.lhs.true
	movl	8(%rax), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	jne	.LBB11_34
# BB#22:                                # %lor.lhs.false
	movq	(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB11_23
# BB#24:                                # %land.lhs.true17
	movl	8(%rax), %edx
	movq	%rdx, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	jne	.LBB11_25
# BB#26:                                # %if.end.i280
	movq	16(%rsp), %rax          # 8-byte Reload
	movslq	(%rax), %rax
	testq	%rax, %rax
	js	.LBB11_27
# BB#30:
	movq	%rax, %rsi
	.align	16, 0x90
.LBB11_31:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	movzbl	16(%rcx,%rsi), %ecx
	cmpq	%rdx, %rcx
	je	.LBB11_32
# BB#28:                                # %for.cond.i281
                                        #   in Loop: Header=BB11_31 Depth=1
	movq	$32, %rcx
	leaq	-1(%rsi), %rdi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	movq	%rdi, %rsi
	jg	.LBB11_31
# BB#29:
	movq	24(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB11_48
.LBB11_23:
	movq	24(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB11_48
.LBB11_25:
	movq	24(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB11_48
.LBB11_27:
	movq	24(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB11_48
.LBB11_32:                              # %get_hard_regnum.exit
	addq	$8, %rax
	subq	%rsi, %rax
	andq	%r15, %rax
	cmpq	$8, %rax
	jne	.LBB11_33
.LBB11_34:                              # %land.lhs.true30
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	swap_rtx_condition
	testq	%r15, %rax
	je	.LBB11_48
# BB#35:                                # %if.then
	movq	%rbx, %r12
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	(%rcx), %rax
	movq	(%r13), %rdx
	movq	%rdx, (%rcx)
	movq	%rax, (%r13)
	movq	%rcx, %r14
	movabsq	$4294967288, %r8        # imm = 0xFFFFFFF8
	jmp	.LBB11_36
	.align	16, 0x90
.LBB11_59:                              # %sw.bb37.i315
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	%rdx, %r14
	addq	$8, %r14
	movq	%rbx, %rdx
.LBB11_36:                              # %for.cond.i287
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rdx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$122, %rcx
	jg	.LBB11_55
# BB#37:                                # %for.cond.i287
                                        #   in Loop: Header=BB11_36 Depth=1
	cmpq	$63, %rcx
	jne	.LBB11_41
# BB#38:                                # %sw.bb.i293
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	8(%rdx), %rbx
	movl	(%rbx), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB11_59
# BB#39:                                # %land.lhs.true.i296
                                        #   in Loop: Header=BB11_36 Depth=1
	movl	8(%rbx), %edi
	movq	%rdi, %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB11_59
	jmp	.LBB11_40
	.align	16, 0x90
.LBB11_55:                              # %for.cond.i287
                                        #   in Loop: Header=BB11_36 Depth=1
	cmpq	$126, %rcx
	je	.LBB11_58
# BB#56:                                # %for.cond.i287
                                        #   in Loop: Header=BB11_36 Depth=1
	cmpq	$125, %rcx
	je	.LBB11_58
# BB#57:                                # %for.cond.i287
                                        #   in Loop: Header=BB11_36 Depth=1
	cmpq	$123, %rcx
	jne	.LBB11_41
.LBB11_58:                              # %for.cond.i287.sw.bb37.i315_crit_edge
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	8(%rdx), %rbx
	jmp	.LBB11_59
.LBB11_40:                              # %if.then.i312
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movl	16(%rdx), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rcx
	callq	subreg_regno_offset
	movabsq	$4294967288, %r8        # imm = 0xFFFFFFF8
	movl	8(%rbx), %ecx
	movzbl	2(%rbx), %edx
	leaq	-8(%rax,%rcx), %rax
	andq	%r15, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %rax
	movq	%rax, (%r14)
	jmp	.LBB11_41
	.align	16, 0x90
.LBB11_60:                              # %sw.bb37.i348
                                        #   in Loop: Header=BB11_41 Depth=1
	addq	$8, %rdx
	movq	%rdx, %r13
.LBB11_41:                              # %for.cond.i320
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r13), %rdx
	movl	(%rdx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	leaq	-125(%rcx), %rsi
	andq	%r15, %rsi
	cmpq	$2, %rsi
	jb	.LBB11_60
# BB#42:                                # %for.cond.i320
                                        #   in Loop: Header=BB11_41 Depth=1
	cmpq	$123, %rcx
	je	.LBB11_60
# BB#43:                                # %for.cond.i320
                                        #   in Loop: Header=BB11_41 Depth=1
	cmpq	$63, %rcx
	jne	.LBB11_47
# BB#44:                                # %sw.bb.i326
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	8(%rdx), %rbx
	movl	(%rbx), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB11_60
# BB#45:                                # %land.lhs.true.i329
                                        #   in Loop: Header=BB11_41 Depth=1
	movl	8(%rbx), %edi
	movq	%rdi, %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB11_60
# BB#46:                                # %if.then.i345
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movl	16(%rdx), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rcx
	callq	subreg_regno_offset
	movl	8(%rbx), %ecx
	movzbl	2(%rbx), %edx
	leaq	-8(%rax,%rcx), %rax
	andq	%r15, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %rax
	movq	%rax, (%r13)
.LBB11_47:                              # %get_true_reg.exit349
	movq	%r12, %rbx
	movl	%r15d, 40(%rbx)
	movq	%r13, %r12
.LBB11_48:                              # %if.end
	movq	(%r14), %rax
	movl	8(%rax), %edx
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	find_regno_note
	movq	(%r12), %rcx
	movzbl	(%rcx), %edx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	cmpq	$61, %rdx
	movq	16(%rsp), %r13          # 8-byte Reload
	jne	.LBB11_51
# BB#49:                                # %land.lhs.true61
	movl	8(%rcx), %edx
	movq	%rdx, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	jne	.LBB11_51
# BB#50:                                # %if.then71
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	find_regno_note
	movq	%rax, 24(%rsp)          # 8-byte Spill
.LBB11_51:                              # %if.end76
	movq	(%r14), %rdx
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	emit_swap_insn
	movq	(%r14), %rcx
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	movabsq	$4294967288, %r11       # imm = 0xFFFFFFF8
	jne	.LBB11_53
# BB#52:                                # %land.lhs.true.i354
	movl	8(%rcx), %ecx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB11_53
# BB#61:                                # %if.end.i357
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	mode_class(,%rax,4), %ecx
	orq	$4, %rcx
	andq	%r15, %rcx
	cmpq	$6, %rcx
	jne	.LBB11_62
# BB#63:                                # %replace_reg.exit
	movq	FP_mode_reg(,%rax,8), %rax
	movq	%rax, (%r14)
	movq	(%r12), %rcx
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB11_72
# BB#64:                                # %land.lhs.true80
	movl	8(%rcx), %edx
	movq	%rdx, %rcx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB11_72
# BB#65:                                # %if.end.i367
	movq	%rbx, %r8
	movslq	(%r13), %rsi
	testq	%rsi, %rsi
	js	.LBB11_53
# BB#66:
	movq	%rsi, %rdi
	.align	16, 0x90
.LBB11_68:                              # %for.body.i376
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%r13,%rdi), %ecx
	cmpq	%rdx, %rcx
	je	.LBB11_69
# BB#67:                                # %for.cond.i371
                                        #   in Loop: Header=BB11_68 Depth=1
	movq	$32, %rcx
	leaq	-1(%rdi), %rbx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rbx, %rdi
	jg	.LBB11_68
	jmp	.LBB11_53
.LBB11_69:                              # %get_hard_regnum.exit381
	movq	$8, %rdx
	subq	%rdi, %rdx
	addq	%rsi, %rdx
	movq	%rdx, %rcx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB11_53
# BB#70:                                # %if.end.i393
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	mode_class(,%rax,4), %ecx
	orq	$4, %rcx
	andq	%r15, %rcx
	cmpq	$6, %rcx
	jne	.LBB11_62
# BB#71:                                # %replace_reg.exit397
	addq	$-8, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	imulq	$472, %rdx, %rcx        # imm = 0x1D8
	movq	FP_mode_reg(%rcx,%rax,8), %rax
	movq	%rax, (%r12)
	movq	%r8, %rbx
.LBB11_72:                              # %if.end92
	movq	%rbx, %r10
	movq	8(%rsp), %rbx           # 8-byte Reload
	testq	%rbx, %rbx
	je	.LBB11_94
# BB#73:                                # %if.then94
	movq	8(%rbx), %r9
	movl	8(%r9), %eax
	movslq	(%r13), %r8
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	notq	%rdx
	andq	%rdx, 8(%r13)
	leaq	-1(%r8), %rcx
	movl	%ecx, (%r13)
	movzbl	16(%r13,%r8), %ecx
	cmpq	%rax, %rcx
	je	.LBB11_90
# BB#74:                                # %if.then94
	testq	%r8, %r8
	jle	.LBB11_90
# BB#75:                                # %for.body.i403.preheader
	movq	%r8, %rdx
	negq	%rdx
	movq	%r8, %rbx
	movq	%r8, %rdi
	.align	16, 0x90
.LBB11_77:                              # %for.body.i403
                                        # =>This Inner Loop Header: Depth=1
	movzbl	15(%r13,%rdi), %ecx
	leaq	-1(%rdi), %rsi
	cmpq	%rax, %rcx
	je	.LBB11_78
# BB#76:                                # %for.cond.i401
                                        #   in Loop: Header=BB11_77 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	decq	%rbx
	incq	%rdx
	testq	%rdi, %rdi
	movq	%rsi, %rdi
	jg	.LBB11_77
	jmp	.LBB11_90
.LBB11_78:                              # %for.cond14.preheader.i
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r8, %rax
	jge	.LBB11_90
# BB#79:                                # %for.body17.lr.ph.i
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r8
	movq	%r8, %r9
	jge	.LBB11_81
# BB#80:                                # %for.body17.lr.ph.i
	movq	%rdi, %r9
.LBB11_81:                              # %for.body17.lr.ph.i
	subq	%rdi, %r9
	andq	%r15, %r9
	leaq	1(%r9), %rax
	movq	%rax, %rcx
	andq	$-4, %rcx
	andq	$-4, %rax
	movq	%rsi, %rax
	je	.LBB11_87
# BB#82:                                # %vector.body.preheader
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r8
	movq	%r8, %rcx
	jge	.LBB11_84
# BB#83:                                # %vector.body.preheader
	movq	%rbx, %rcx
.LBB11_84:                              # %vector.body.preheader
	addq	%rsi, (%rsp)            # 8-byte Folded Spill
	addq	%rdx, %rcx
	andq	%r15, %rcx
	incq	%rcx
	andq	$-4, %rcx
	leaq	16(%r13), %rax
	.align	16, 0x90
.LBB11_85:                              # %vector.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax,%rdi), %r11d
	movzbl	2(%rax,%rdi), %ebx
	movzbl	1(%rax,%rdi), %r13d
	movzbl	3(%rax,%rdi), %edx
	movb	%dl, 2(%rax,%rdi)
	movb	%bl, 1(%rax,%rdi)
	movb	%r13b, (%rax,%rdi)
	movb	%r11b, -1(%rax,%rdi)
	addq	$4, %rax
	addq	$-4, %rcx
	jne	.LBB11_85
# BB#86:
	movq	(%rsp), %rax            # 8-byte Reload
	movq	16(%rsp), %r13          # 8-byte Reload
	movabsq	$4294967288, %r11       # imm = 0xFFFFFFF8
.LBB11_87:                              # %middle.block
	leaq	1(%r9,%rsi), %rcx
	cmpq	%rax, %rcx
	je	.LBB11_89
	.align	16, 0x90
.LBB11_88:                              # %for.body17.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	17(%r13,%rax), %ecx
	movb	%cl, 16(%r13,%rax)
	leaq	1(%rax), %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%r8, %rdx
	jl	.LBB11_88
.LBB11_89:                              # %pop_stack.exit.loopexit
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	8(%rax), %r9
.LBB11_90:                              # %pop_stack.exit
	movl	(%r9), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB11_53
# BB#91:                                # %land.lhs.true.i413
	movl	8(%r9), %ecx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB11_53
# BB#92:                                # %if.end.i419
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	mode_class(,%rax,4), %ecx
	orq	$4, %rcx
	andq	%r15, %rcx
	cmpq	$6, %rcx
	jne	.LBB11_62
# BB#93:                                # %replace_reg.exit422
	movq	FP_mode_reg(,%rax,8), %rax
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rax, 8(%rbx)
.LBB11_94:                              # %if.end104
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	movq	%r10, %rdi
	je	.LBB11_130
# BB#95:                                # %land.lhs.true106
	movq	(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB11_100
# BB#96:                                # %land.lhs.true110
	movl	8(%rax), %eax
	movq	%rax, %rcx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB11_100
# BB#97:                                # %land.lhs.true120
	movq	(%r12), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB11_100
# BB#98:                                # %land.lhs.true124
	movl	8(%rcx), %ecx
	movq	%rcx, %rdx
	andq	%r11, %rdx
	cmpq	$8, %rdx
	jne	.LBB11_100
# BB#99:                                # %land.lhs.true124
	cmpq	%rcx, %rax
	je	.LBB11_130
.LBB11_100:                             # %if.then142
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %r9
	movzbl	(%r9), %eax
	cmpq	$61, %rax
	jne	.LBB11_102
# BB#101:                               # %land.lhs.true.i428
	movl	8(%r9), %eax
	movq	%rax, %rcx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB11_102
# BB#103:                               # %if.end.i432
	movslq	(%r13), %r8
	testq	%r8, %r8
	js	.LBB11_131
# BB#104:
	movq	%r8, %rdx
	.align	16, 0x90
.LBB11_106:                             # %for.body.i441
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%r13,%rdx), %ecx
	cmpq	%rax, %rcx
	je	.LBB11_107
# BB#105:                               # %for.cond.i436
                                        #   in Loop: Header=BB11_106 Depth=1
	movq	$32, %rcx
	leaq	-1(%rdx), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movq	%rsi, %rdx
	jg	.LBB11_106
	jmp	.LBB11_131
.LBB11_107:                             # %get_hard_regnum.exit446
	leaq	8(%r8), %rcx
	subq	%rdx, %rcx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	jne	.LBB11_131
# BB#108:                               # %get_hard_regnum.exit446
	testq	%rbx, %rbx
	je	.LBB11_131
# BB#109:                               # %if.then150
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	notq	%rdx
	andq	%rdx, 8(%r13)
	leaq	-1(%r8), %rcx
	movl	%ecx, (%r13)
	movzbl	16(%r13,%r8), %ecx
	cmpq	%rax, %rcx
	je	.LBB11_126
# BB#110:                               # %if.then150
	testq	%r8, %r8
	jle	.LBB11_126
# BB#111:                               # %for.body.i467.preheader
	movq	%r8, %rdx
	negq	%rdx
	movq	%r8, %rsi
	movq	%r8, %rdi
	.align	16, 0x90
.LBB11_113:                             # %for.body.i467
                                        # =>This Inner Loop Header: Depth=1
	movzbl	15(%r13,%rdi), %ecx
	leaq	-1(%rdi), %rbx
	cmpq	%rax, %rcx
	je	.LBB11_114
# BB#112:                               # %for.cond.i461
                                        #   in Loop: Header=BB11_113 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	decq	%rsi
	incq	%rdx
	testq	%rdi, %rdi
	movq	%rbx, %rdi
	jg	.LBB11_113
	jmp	.LBB11_126
.LBB11_131:                             # %if.else160
	movq	56(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB11_137
# BB#132:
	leaq	56(%rdi), %rdx
	.align	16, 0x90
.LBB11_133:                             # %for.body.i499
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rcx), %esi
	andq	$16711680, %rsi         # imm = 0xFF0000
	cmpq	$65536, %rsi            # imm = 0x10000
	jne	.LBB11_136
# BB#134:                               # %land.lhs.true.i500
                                        #   in Loop: Header=BB11_133 Depth=1
	movq	8(%rcx), %rsi
	movzbl	(%rsi), %ebx
	cmpq	$61, %rbx
	jne	.LBB11_136
# BB#135:                               # %land.lhs.true7.i
                                        #   in Loop: Header=BB11_133 Depth=1
	movl	8(%rsi), %esi
	cmpq	%rax, %rsi
	je	.LBB11_138
.LBB11_136:                             # %if.else.i
                                        #   in Loop: Header=BB11_133 Depth=1
	movq	16(%rcx), %rsi
	movq	%rcx, %rdx
	addq	$16, %rdx
	testq	%rsi, %rsi
	movq	%rsi, %rcx
	jne	.LBB11_133
.LBB11_137:                             # %for.end.i
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.remove_regno_note, %rcx
	movq	$885, %rsi              # imm = 0x375
	jmp	.LBB11_54
.LBB11_138:                             # %remove_regno_note.exit
	movq	16(%rcx), %rax
	movq	%rax, (%rdx)
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rsi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_pop_insn  # TAILCALL
.LBB11_114:                             # %for.cond14.preheader.i469
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r8, %rax
	jge	.LBB11_126
# BB#115:                               # %for.body17.lr.ph.i471
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r8
	movq	%r8, %r10
	jge	.LBB11_117
# BB#116:                               # %for.body17.lr.ph.i471
	movq	%rdi, %r10
.LBB11_117:                             # %for.body17.lr.ph.i471
	subq	%rdi, %r10
	andq	%r15, %r10
	leaq	1(%r10), %rax
	movq	%rax, %r9
	andq	$-4, %r9
	andq	$-4, %rax
	movq	%rbx, %rax
	je	.LBB11_123
# BB#118:                               # %vector.body655.preheader
	movq	%r11, %r12
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r8
	movq	%r8, %rcx
	jge	.LBB11_120
# BB#119:                               # %vector.body655.preheader
	movq	%rsi, %rcx
.LBB11_120:                             # %vector.body655.preheader
	addq	%rbx, %r9
	addq	%rdx, %rcx
	andq	%r15, %rcx
	incq	%rcx
	andq	$-4, %rcx
	leaq	16(%r13), %rax
	.align	16, 0x90
.LBB11_121:                             # %vector.body655
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax,%rdi), %r11d
	movzbl	2(%rax,%rdi), %esi
	movzbl	1(%rax,%rdi), %r14d
	movzbl	3(%rax,%rdi), %edx
	movb	%dl, 2(%rax,%rdi)
	movb	%sil, 1(%rax,%rdi)
	movb	%r14b, (%rax,%rdi)
	movb	%r11b, -1(%rax,%rdi)
	addq	$4, %rax
	addq	$-4, %rcx
	jne	.LBB11_121
# BB#122:
	movq	%r9, %rax
	movq	%r12, %r11
.LBB11_123:                             # %middle.block656
	leaq	1(%r10,%rbx), %rcx
	cmpq	%rax, %rcx
	je	.LBB11_125
.LBB11_124:                             # %for.body17.i477
                                        # =>This Inner Loop Header: Depth=1
	movzbl	17(%r13,%rax), %ecx
	movb	%cl, 16(%r13,%rax)
	leaq	1(%rax), %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%r8, %rdx
	jl	.LBB11_124
.LBB11_125:                             # %pop_stack.exit478.loopexit
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %r9
.LBB11_126:                             # %pop_stack.exit478
	movl	(%r9), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB11_53
# BB#127:                               # %land.lhs.true.i485
	movl	8(%r9), %ecx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	jne	.LBB11_53
# BB#128:                               # %if.end.i491
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	mode_class(,%rax,4), %ecx
	orq	$4, %rcx
	andq	%r15, %rcx
	cmpq	$6, %rcx
	jne	.LBB11_62
# BB#129:                               # %replace_reg.exit494
	movq	FP_mode_reg+472(,%rax,8), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rax, 8(%rcx)
.LBB11_130:                             # %if.end172
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB11_33:
	movq	24(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB11_48
.LBB11_102:                             # %if.then.i429
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.get_hard_regnum, %rcx
	movq	$900, %rsi              # imm = 0x384
	jmp	.LBB11_54
.LBB11_53:                              # %if.then.i355
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.replace_reg, %rcx
	movq	$851, %rsi              # imm = 0x353
	jmp	.LBB11_54
.LBB11_62:                              # %sw.default.i
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.replace_reg, %rcx
	movq	$855, %rsi              # imm = 0x357
.LBB11_54:                              # %if.then.i355
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp196:
	.size	compare_for_stack_reg, .Ltmp196-compare_for_stack_reg
	.cfi_endproc

	.align	16, 0x90
	.type	emit_swap_insn,@function
emit_swap_insn:                         # @emit_swap_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp203:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp204:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp205:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp206:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp207:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp208:
	.cfi_def_cfa_offset 96
.Ltmp209:
	.cfi_offset %rbx, -48
.Ltmp210:
	.cfi_offset %r12, -40
.Ltmp211:
	.cfi_offset %r13, -32
.Ltmp212:
	.cfi_offset %r14, -24
.Ltmp213:
	.cfi_offset %r15, -16
	movzbl	(%rdx), %eax
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	cmpq	$61, %rax
	jne	.LBB12_2
# BB#1:                                 # %land.lhs.true.i
	movl	8(%rdx), %edx
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	movq	%rdx, %rax
	andq	%rcx, %rax
	cmpq	$8, %rax
	jne	.LBB12_2
# BB#4:                                 # %if.end.i
	movslq	(%rsi), %rax
	testq	%rax, %rax
	js	.LBB12_9
# BB#5:
	movq	%rax, %rdi
	.align	16, 0x90
.LBB12_7:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rsi,%rdi), %ecx
	cmpq	%rdx, %rcx
	je	.LBB12_8
# BB#6:                                 # %for.cond.i
                                        #   in Loop: Header=BB12_7 Depth=1
	movq	$32, %rcx
	leaq	-1(%rdi), %rbx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rbx, %rdi
	jg	.LBB12_7
	jmp	.LBB12_9
.LBB12_8:                               # %get_hard_regnum.exit
	movq	$8, %r14
	subq	%rdi, %r14
	addq	%rax, %r14
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$7, %rdx
	jle	.LBB12_9
# BB#10:                                # %if.end
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	%r14, %r9
	andq	%rdi, %r9
	movq	%r9, 32(%rsp)           # 8-byte Spill
	cmpq	$8, %r9
	je	.LBB12_70
# BB#11:                                # %if.end6
	leaq	-8(%r14), %r8
	movq	%r8, 24(%rsp)           # 8-byte Spill
	movq	%rax, %rdx
	subq	%r8, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movzbl	16(%rsi,%rax), %eax
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	16(%rsi,%rdx), %ecx
	movb	%al, 16(%rsi,%rdx)
	movslq	(%rsi), %rax
	movb	%cl, 16(%rsi,%rax)
	movq	current_block(%rip), %rax
	xorq	%r15, %r15
	testq	%rax, %rax
	movabsq	$0, %rbx
	je	.LBB12_65
# BB#12:                                # %land.lhs.true
	movq	(%rax), %rax
	xorq	%r15, %r15
	movq	40(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	movabsq	$0, %rbx
	je	.LBB12_65
# BB#13:                                # %if.then23
	movq	16(%rax), %r12
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %r13
	xorq	%r15, %r15
	cmpq	%r12, %r13
	movabsq	$0, %rbx
	je	.LBB12_65
# BB#14:
	movabsq	$4294967216, %rdx       # imm = 0xFFFFFFB0
	.align	16, 0x90
.LBB12_15:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r13), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	addq	$-32, %rcx
	andq	%rdi, %rcx
	cmpq	$5, %rcx
	ja	.LBB12_18
# BB#16:                                # %while.body
                                        #   in Loop: Header=BB12_15 Depth=1
	jmpq	*.LJTI12_0(,%rcx,8)
.LBB12_19:                              # %land.lhs.true55
                                        #   in Loop: Header=BB12_15 Depth=1
	movzbl	rtx_class+32(%rip), %eax
	cmpq	$105, %rax
	jne	.LBB12_18
# BB#20:                                # %land.lhs.true55
                                        #   in Loop: Header=BB12_15 Depth=1
	movq	stack_regs_mentioned_data(%rip), %r15
	testq	%r15, %r15
	je	.LBB12_18
# BB#21:                                # %if.end.i220
                                        #   in Loop: Header=BB12_15 Depth=1
	movl	8(%r13), %esi
	movl	(%r15), %eax
	cmpq	%rax, %rsi
	jb	.LBB12_23
# BB#22:                                # %if.then6.i
                                        #   in Loop: Header=BB12_15 Depth=1
	movq	%rsi, %rax
	movabsq	$-3689348814741910323, %rcx # imm = 0xCCCCCCCCCCCCCCCD
	mulq	%rcx
	movq	$4, %rcx
	shrq	%cl, %rdx
	movq	%rsi, %rbx
                                        # kill: RSI<def> RBX<kill>
	addq	%rdx, %rsi
	andq	%rdi, %rsi
	movq	%r15, %rdi
	callq	varray_grow
	movq	%rbx, %rsi
	movq	%rax, %r15
	movq	%r15, stack_regs_mentioned_data(%rip)
.LBB12_23:                              # %if.end8.i
                                        #   in Loop: Header=BB12_15 Depth=1
	movzbl	32(%r15,%rsi), %edx
	movq	$56, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rdx, %rdx
	movq	24(%rsp), %r8           # 8-byte Reload
	movq	32(%rsp), %r9           # 8-byte Reload
	jne	.LBB12_27
# BB#24:                                # %if.then14.i
                                        #   in Loop: Header=BB12_15 Depth=1
	movq	%rsi, %rbx
	movq	32(%r13), %rdi
	callq	stack_regs_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$0, %rax
	je	.LBB12_26
# BB#25:                                # %if.then14.i
                                        #   in Loop: Header=BB12_15 Depth=1
	movabsq	$1, %rax
.LBB12_26:                              # %if.then14.i
                                        #   in Loop: Header=BB12_15 Depth=1
	xorq	$1, %rax
	incq	%rax
	movb	%al, 32(%r15,%rbx)
	movq	24(%rsp), %r8           # 8-byte Reload
	movq	32(%rsp), %r9           # 8-byte Reload
.LBB12_27:                              # %stack_regs_mentioned.exit
                                        #   in Loop: Header=BB12_15 Depth=1
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rax
	cmpq	$1, %rax
	movabsq	$4294967216, %rdx       # imm = 0xFFFFFFB0
	jne	.LBB12_18
	jmp	.LBB12_28
.LBB12_17:                              # %land.lhs.true45
                                        #   in Loop: Header=BB12_15 Depth=1
	movl	40(%r13), %ecx
	cmpq	%rdx, %rcx
	je	.LBB12_30
	.align	16, 0x90
.LBB12_18:                              # %while.cond.backedge
                                        #   in Loop: Header=BB12_15 Depth=1
	movq	16(%r13), %r13
	xorq	%r15, %r15
	cmpq	%r12, %r13
	movabsq	$0, %rbx
	jne	.LBB12_15
	jmp	.LBB12_65
.LBB12_28:                              # %if.end63
	xorq	%r15, %r15
	testq	%r13, %r13
	movabsq	$0, %rbx
	je	.LBB12_65
# BB#29:                                # %if.end63.land.lhs.true66_crit_edge
	movl	(%r13), %eax
.LBB12_30:                              # %land.lhs.true66
	movq	%r9, 32(%rsp)           # 8-byte Spill
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	movq	$1, %r15
	jne	.LBB12_31
# BB#32:                                # %cond.true
	movq	%rdi, %rbx
	movq	32(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB12_34
# BB#33:                                # %cond.false
	movq	%r8, %r12
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	single_set_2
	movq	%r12, %r8
.LBB12_34:                              # %cond.end90
	movq	%rbx, %r9
	testq	%rax, %rax
	je	.LBB12_35
# BB#36:                                # %if.then94
	leaq	16(%rax), %r12
	movabsq	$4294967288, %r8        # imm = 0xFFFFFFF8
	jmp	.LBB12_37
	.align	16, 0x90
.LBB12_71:                              # %sw.bb37.i
                                        #   in Loop: Header=BB12_37 Depth=1
	addq	$8, %rbx
	movq	%rbx, %r12
.LBB12_37:                              # %for.cond.i225
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r12), %rbx
	movl	(%rbx), %r10d
	movq	%r10, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	leaq	-125(%rcx), %rdx
	andq	%r9, %rdx
	cmpq	$2, %rdx
	jb	.LBB12_71
# BB#38:                                # %for.cond.i225
                                        #   in Loop: Header=BB12_37 Depth=1
	cmpq	$123, %rcx
	je	.LBB12_71
# BB#39:                                # %for.cond.i225
                                        #   in Loop: Header=BB12_37 Depth=1
	cmpq	$63, %rcx
	jne	.LBB12_40
# BB#41:                                # %sw.bb.i
                                        #   in Loop: Header=BB12_37 Depth=1
	movq	8(%rbx), %rdx
	movl	(%rdx), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB12_71
# BB#42:                                # %land.lhs.true.i230
                                        #   in Loop: Header=BB12_37 Depth=1
	movl	8(%rdx), %edi
	movq	%rdi, %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB12_71
# BB#43:                                # %if.then.i234
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movl	16(%rbx), %edx
	movq	$16, %rcx
	shrq	%cl, %r10
	andq	$255, %r10
	movq	%r10, %rcx
	movq	%rax, %rbx
	callq	subreg_regno_offset
	movq	%rbx, %r10
	movabsq	$4294967288, %r8        # imm = 0xFFFFFFF8
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	16(%rsp), %rdx          # 8-byte Reload
	movl	8(%rdx), %ecx
	movzbl	2(%rdx), %edx
	leaq	-8(%rax,%rcx), %rax
	andq	%r9, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %rbx
	movq	%rbx, (%r12)
	jmp	.LBB12_44
.LBB12_31:
	movq	%r13, %rbx
	jmp	.LBB12_65
.LBB12_40:
	movq	%rax, %r10
	.align	16, 0x90
.LBB12_44:                              # %for.cond.i238
                                        # =>This Inner Loop Header: Depth=1
	movq	%r10, %r12
	movq	8(%r12), %r10
	movl	(%r10), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$123, %rcx
	je	.LBB12_44
# BB#45:                                # %for.cond.i238
                                        #   in Loop: Header=BB12_44 Depth=1
	leaq	-125(%rcx), %rdx
	andq	%r9, %rdx
	cmpq	$2, %rdx
	jb	.LBB12_44
# BB#46:                                # %for.cond.i238
                                        #   in Loop: Header=BB12_44 Depth=1
	cmpq	$63, %rcx
	jne	.LBB12_50
# BB#47:                                # %sw.bb.i244
                                        #   in Loop: Header=BB12_44 Depth=1
	movq	8(%r10), %rdx
	movl	(%rdx), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB12_44
# BB#48:                                # %land.lhs.true.i247
                                        #   in Loop: Header=BB12_44 Depth=1
	movl	8(%rdx), %edi
	movq	%rdi, %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB12_44
# BB#49:                                # %if.then.i263
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movl	16(%r10), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rcx
	callq	subreg_regno_offset
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	16(%rsp), %rdx          # 8-byte Reload
	movl	8(%rdx), %ecx
	movzbl	2(%rdx), %edx
	leaq	-8(%rax,%rcx), %rax
	andq	%r9, %rax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	movq	FP_mode_reg(%rax,%rdx,8), %r10
	movq	%r10, 8(%r12)
	movl	(%r10), %eax
.LBB12_50:                              # %get_true_reg.exit267
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB12_56
# BB#51:                                # %land.lhs.true109
	movl	8(%r10), %ecx
	cmpq	$8, %rcx
	jne	.LBB12_56
# BB#52:                                # %land.lhs.true114
	movzbl	(%rbx), %ecx
	cmpq	$61, %rcx
	jne	.LBB12_56
# BB#53:                                # %land.lhs.true119
	movl	8(%rbx), %ecx
	decq	%r14
	andq	%r9, %r14
	cmpq	%r14, %rcx
	jne	.LBB12_56
# BB#54:                                # %land.lhs.true126
	movq	%rbx, %r14
	movq	%r10, %rbx
	movq	$1, %rsi
	movq	$8, %rdx
	movq	%r13, %rdi
	callq	find_regno_note
	testq	%rax, %rax
	je	.LBB12_70
# BB#55:                                # %land.lhs.true126.if.end131_crit_edge
	movq	%rbx, %r10
	movl	(%r10), %eax
	movq	%r14, %rbx
.LBB12_56:                              # %if.end131
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	jne	.LBB12_57
# BB#58:                                # %land.lhs.true136
	movl	8(%r10), %eax
	cmpq	%rcx, %rax
	movq	24(%rsp), %r8           # 8-byte Reload
	jne	.LBB12_59
# BB#60:                                # %land.lhs.true142
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	jne	.LBB12_61
# BB#62:                                # %land.lhs.true147
	movq	%r8, %r14
	movl	8(%rbx), %eax
	cmpq	$8, %rax
	jne	.LBB12_63
# BB#64:                                # %land.lhs.true153
	movq	$1, %rsi
	movq	$8, %rdx
	movq	$1, %r15
	movq	%r13, %rdi
	callq	find_regno_note
	testq	%rax, %rax
	movq	%r13, %rbx
	movq	%r14, %r8
	jne	.LBB12_65
.LBB12_70:                              # %if.end175
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB12_35:
	movq	%r13, %rbx
	jmp	.LBB12_65
.LBB12_57:
	movq	%r13, %rbx
	movq	24(%rsp), %r8           # 8-byte Reload
	jmp	.LBB12_65
.LBB12_59:
	movq	%r13, %rbx
	jmp	.LBB12_65
.LBB12_61:
	movq	%r13, %rbx
	jmp	.LBB12_65
.LBB12_63:
	movq	%r13, %rbx
	movq	%r14, %r8
.LBB12_65:                              # %if.end159
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	imulq	$472, %r8, %rax         # imm = 0x1D8
	movq	FP_mode_reg+136(%rax), %rdi
	movq	FP_mode_reg+136(%rip), %rsi
	callq	gen_swapxf
	testq	%r15, %r15
	je	.LBB12_66
# BB#72:                                # %if.then166
	movq	%rax, %rdi
	movq	%rbx, %rsi
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_insn_after  # TAILCALL
.LBB12_66:                              # %if.else
	movq	current_block(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB12_69
# BB#67:                                # %if.then169
	movq	(%rcx), %rsi
	movq	%rax, %rdi
	jmp	.LBB12_68
.LBB12_69:                              # %if.else172
	movq	%rax, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
.LBB12_68:                              # %if.then169
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_insn_before  # TAILCALL
.LBB12_2:                               # %if.then.i
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.get_hard_regnum, %rcx
	movq	$900, %rsi              # imm = 0x384
	jmp	.LBB12_3
.LBB12_9:                               # %if.then
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.emit_swap_insn, %rcx
	movq	$990, %rsi              # imm = 0x3DE
.LBB12_3:                               # %if.then.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp214:
	.size	emit_swap_insn, .Ltmp214-emit_swap_insn
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI12_0:
	.quad	.LBB12_19
	.quad	.LBB12_18
	.quad	.LBB12_28
	.quad	.LBB12_18
	.quad	.LBB12_28
	.quad	.LBB12_17

	.text
	.align	16, 0x90
	.type	swap_rtx_condition,@function
swap_rtx_condition:                     # @swap_rtx_condition
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp220:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp221:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp222:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp223:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp224:
	.cfi_def_cfa_offset 48
.Ltmp225:
	.cfi_offset %rbx, -40
.Ltmp226:
	.cfi_offset %r12, -32
.Ltmp227:
	.cfi_offset %r14, -24
.Ltmp228:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	32(%rbx), %r15
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$47, %rcx
	jne	.LBB13_11
# BB#1:                                 # %land.lhs.true
	movq	8(%r15), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB13_11
# BB#2:                                 # %land.lhs.true7
	movl	8(%rcx), %ecx
	cmpq	$17, %rcx
	jne	.LBB13_11
# BB#3:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	xorq	%r14, %r14
	.align	16, 0x90
.LBB13_4:                               # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	current_block(%rip), %rax
	movq	8(%rax), %rax
	cmpq	%rax, %rbx
	je	.LBB13_42
# BB#5:                                 # %while.body.i
                                        #   in Loop: Header=BB13_4 Depth=1
	movq	24(%rbx), %rbx
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB13_8
# BB#6:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB13_4 Depth=1
	movq	ix86_flags_rtx(%rip), %rdi
	movq	32(%rbx), %rsi
	callq	reg_mentioned_p
	testq	%r15, %rax
	jne	.LBB13_9
# BB#7:                                 # %land.lhs.true.if.end_crit_edge.i
                                        #   in Loop: Header=BB13_4 Depth=1
	movl	(%rbx), %eax
.LBB13_8:                               # %if.end.i
                                        #   in Loop: Header=BB13_4 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$34, %rax
	jne	.LBB13_4
	jmp	.LBB13_42
.LBB13_9:                               # %next_flags_user.exit
	testq	%rbx, %rbx
	je	.LBB13_42
# BB#10:                                # %if.end
	movq	32(%rbx), %r15
	movl	(%r15), %eax
.LBB13_11:                              # %if.end19
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$47, %rax
	jne	.LBB13_31
# BB#12:                                # %land.lhs.true23
	movq	16(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$42, %rcx
	jne	.LBB13_31
# BB#13:                                # %land.lhs.true30
	movl	16(%rax), %eax
	cmpq	$9, %rax
	jne	.LBB13_31
# BB#14:                                # %if.then37
	movq	8(%r15), %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	xorq	%r14, %r14
	.align	16, 0x90
.LBB13_15:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	current_block(%rip), %rax
	movq	8(%rax), %rax
	cmpq	%rax, %rbx
	je	.LBB13_20
# BB#16:                                # %while.body
                                        #   in Loop: Header=BB13_15 Depth=1
	movq	24(%rbx), %rbx
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB13_19
# BB#17:                                # %land.lhs.true50
                                        #   in Loop: Header=BB13_15 Depth=1
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	reg_mentioned_p
	testq	%r12, %rax
	jne	.LBB13_20
# BB#18:                                # %land.lhs.true50.if.end53_crit_edge
                                        #   in Loop: Header=BB13_15 Depth=1
	movl	(%rbx), %eax
.LBB13_19:                              # %if.end53
                                        #   in Loop: Header=BB13_15 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$34, %rax
	jne	.LBB13_15
	jmp	.LBB13_42
.LBB13_20:                              # %while.end
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	xorq	%r14, %r14
	cmpq	$47, %rcx
	jne	.LBB13_42
# BB#21:                                # %lor.lhs.false
	movq	16(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$42, %rcx
	jne	.LBB13_42
# BB#22:                                # %lor.lhs.false74
	movl	16(%rax), %eax
	cmpq	$10, %rax
	jne	.LBB13_42
# BB#23:                                # %lor.lhs.false83
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	dead_or_set_p
	testq	%r12, %rax
	je	.LBB13_42
.LBB13_24:                              # %while.cond.i169
                                        # =>This Inner Loop Header: Depth=1
	movq	current_block(%rip), %rax
	movq	8(%rax), %rax
	cmpq	%rax, %rbx
	je	.LBB13_42
# BB#25:                                # %while.body.i177
                                        #   in Loop: Header=BB13_24 Depth=1
	movq	24(%rbx), %rbx
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB13_28
# BB#26:                                # %land.lhs.true.i182
                                        #   in Loop: Header=BB13_24 Depth=1
	movq	ix86_flags_rtx(%rip), %rdi
	movq	32(%rbx), %rsi
	callq	reg_mentioned_p
	testq	%r12, %rax
	jne	.LBB13_29
# BB#27:                                # %land.lhs.true.if.end_crit_edge.i184
                                        #   in Loop: Header=BB13_24 Depth=1
	movl	(%rbx), %eax
.LBB13_28:                              # %if.end.i188
                                        #   in Loop: Header=BB13_24 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$34, %rax
	jne	.LBB13_24
	jmp	.LBB13_42
.LBB13_29:                              # %next_flags_user.exit190
	testq	%rbx, %rbx
	je	.LBB13_42
# BB#30:                                # %if.end92
	movq	32(%rbx), %r15
.LBB13_31:                              # %if.end96
	movq	%r15, %rdi
	callq	swap_rtx_condition_1
	xorq	%r14, %r14
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB13_42
# BB#32:                                # %if.then99
	movl	%r12d, 40(%rbx)
	movq	%rbx, %rdi
	callq	recog_memoized_1
	andq	%r12, %rax
	cmpq	%r12, %rax
	je	.LBB13_41
# BB#33:                                # %if.else
	movq	ix86_flags_rtx(%rip), %rsi
	movq	%rbx, %rdi
	callq	dead_or_set_p
	testq	%r12, %rax
	movq	$1, %r14
	jne	.LBB13_42
	.align	16, 0x90
.LBB13_34:                              # %while.cond.i194
                                        # =>This Inner Loop Header: Depth=1
	movq	current_block(%rip), %rax
	movq	8(%rax), %rax
	cmpq	%rax, %rbx
	je	.LBB13_41
# BB#35:                                # %while.body.i202
                                        #   in Loop: Header=BB13_34 Depth=1
	movq	24(%rbx), %rbx
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB13_38
# BB#36:                                # %land.lhs.true.i207
                                        #   in Loop: Header=BB13_34 Depth=1
	movq	ix86_flags_rtx(%rip), %rdi
	movq	32(%rbx), %rsi
	callq	reg_mentioned_p
	testq	%r12, %rax
	jne	.LBB13_39
# BB#37:                                # %land.lhs.true.if.end_crit_edge.i209
                                        #   in Loop: Header=BB13_34 Depth=1
	movl	(%rbx), %eax
.LBB13_38:                              # %if.end.i213
                                        #   in Loop: Header=BB13_34 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$34, %rax
	jne	.LBB13_34
	jmp	.LBB13_41
.LBB13_39:                              # %next_flags_user.exit215
	testq	%rbx, %rbx
	je	.LBB13_41
# BB#40:                                # %lor.lhs.false120
	movq	%rbx, %rdi
	callq	swap_rtx_condition
	testq	%r12, %rax
	jne	.LBB13_42
.LBB13_41:                              # %if.then128
	movq	%r15, %rdi
	callq	swap_rtx_condition_1
	xorq	%r14, %r14
.LBB13_42:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp229:
	.size	swap_rtx_condition, .Ltmp229-swap_rtx_condition
	.cfi_endproc

	.align	16, 0x90
	.type	swap_rtx_condition_1,@function
swap_rtx_condition_1:                   # @swap_rtx_condition_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp236:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp237:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp238:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp239:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp240:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp241:
	.cfi_def_cfa_offset 64
.Ltmp242:
	.cfi_offset %rbx, -48
.Ltmp243:
	.cfi_offset %r12, -40
.Ltmp244:
	.cfi_offset %r13, -32
.Ltmp245:
	.cfi_offset %r14, -24
.Ltmp246:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movzbl	(%r14), %edi
	movzbl	rtx_class(%rdi), %eax
	cmpq	$60, %rax
	jne	.LBB14_3
# BB#1:                                 # %if.then
	callq	swap_condition
	movw	%ax, (%r14)
	movq	$1, %r12
	jmp	.LBB14_2
.LBB14_3:                               # %if.else
	movq	rtx_format(,%rdi,8), %r15
	movzbl	rtx_length(%rdi), %r13d
	xorq	%r12, %r12
	jmp	.LBB14_4
	.align	16, 0x90
.LBB14_11:                              # %if.then43
                                        #   in Loop: Header=BB14_4 Depth=1
	movq	(%r14,%rbx,8), %rdi
	callq	swap_rtx_condition_1
	orq	%rax, %r12
.LBB14_4:                               # %for.cond.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB14_5 Depth 2
                                        #     Child Loop BB14_9 Depth 2
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
.LBB14_5:                               # %for.cond
                                        #   Parent Loop BB14_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r13, %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB14_2
# BB#6:                                 # %for.body
                                        #   in Loop: Header=BB14_5 Depth=2
	movzbl	-1(%r15,%rbx), %eax
	leaq	-1(%rbx), %r13
	cmpq	$101, %rax
	je	.LBB14_11
# BB#7:                                 # %for.body
                                        #   in Loop: Header=BB14_5 Depth=2
	cmpq	$69, %rax
	jne	.LBB14_5
# BB#8:                                 # %if.then22
                                        #   in Loop: Header=BB14_4 Depth=1
	movq	(%r14,%rbx,8), %rax
	movslq	(%rax), %rdx
	testq	%rdx, %rdx
	jle	.LBB14_4
	jmp	.LBB14_9
.LBB14_10:                              # %for.body29.for.body29_crit_edge
                                        #   in Loop: Header=BB14_9 Depth=2
	movq	(%r14,%rbx,8), %rax
.LBB14_9:                               # %for.body29.lr.ph
                                        #   Parent Loop BB14_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%rdx
	movq	8(%rax,%rdx,8), %rdi
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	%rdx, %r12
	callq	swap_rtx_condition_1
	movq	%r12, %rdx
	movq	%rax, %r12
	movq	8(%rsp), %rax           # 8-byte Reload
	orq	%rax, %r12
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB14_10
	jmp	.LBB14_4
.LBB14_2:                               # %if.end53
	movq	%r12, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp247:
	.size	swap_rtx_condition_1, .Ltmp247-swap_rtx_condition_1
	.cfi_endproc

	.align	16, 0x90
	.type	check_asm_stack_operands,@function
check_asm_stack_operands:               # @check_asm_stack_operands
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp251:
	.cfi_def_cfa_offset 16
.Ltmp252:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp253:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$168, %rsp
.Ltmp254:
	.cfi_offset %rbx, -56
.Ltmp255:
	.cfi_offset %r12, -48
.Ltmp256:
	.cfi_offset %r13, -40
.Ltmp257:
	.cfi_offset %r14, -32
.Ltmp258:
	.cfi_offset %r15, -24
	movq	%rdi, -168(%rbp)        # 8-byte Spill
	movq	32(%rdi), %rbx
	callq	extract_insn
	movq	$1, %rdi
	callq	constrain_operands
	movslq	which_alternative(%rip), %rax
	movq	%rax, -184(%rbp)        # 8-byte Spill
	callq	preprocess_constraints
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$39, %rcx
	je	.LBB15_5
# BB#1:                                 # %entry
	cmpq	$41, %rcx
	jne	.LBB15_2
# BB#10:                                # %if.then12.i
	movq	32(%rbx), %rcx
	jmp	.LBB15_11
.LBB15_5:                               # %land.lhs.true21.i
	movq	8(%rbx), %rcx
	movq	8(%rcx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$41, %rdx
	je	.LBB15_8
# BB#6:                                 # %land.lhs.true21.i
	cmpq	$47, %rdx
	jne	.LBB15_9
# BB#7:                                 # %if.then29.i
	movq	16(%rcx), %rcx
.LBB15_8:                               # %if.then55.i
	movq	32(%rcx), %rcx
	jmp	.LBB15_11
.LBB15_2:                               # %entry
	cmpq	$47, %rcx
	jne	.LBB15_9
# BB#3:                                 # %land.lhs.true.i
	movq	16(%rbx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$41, %rdx
	jne	.LBB15_9
# BB#4:                                 # %if.then.i
	movq	32(%rcx), %rcx
.LBB15_11:                              # %get_asm_operand_n_inputs.exit
	cmpq	$0, -184(%rbp)          # 8-byte Folded Reload
	js	.LBB15_103
# BB#12:                                # %for.cond.preheader
	movsbq	recog_data+996(%rip), %r14
	movl	(%rcx), %ecx
	movq	%rcx, -192(%rbp)        # 8-byte Spill
	testq	%r14, %r14
	jle	.LBB15_19
# BB#13:
	movq	$1, %rax
	movq	%r14, %rdx
	.align	16, 0x90
.LBB15_14:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	recog_data-8(,%rax,8), %rcx
	movzbl	(%rcx), %esi
	cmpq	$63, %rsi
	jne	.LBB15_17
# BB#15:                                # %land.lhs.true
                                        #   in Loop: Header=BB15_14 Depth=1
	movq	8(%rcx), %rcx
	movzbl	(%rcx), %esi
	cmpq	$61, %rsi
	jne	.LBB15_17
# BB#16:                                # %if.then22
                                        #   in Loop: Header=BB15_14 Depth=1
	movq	%rcx, recog_data-8(,%rax,8)
	movzbl	recog_data+996(%rip), %edx
.LBB15_17:                              # %for.inc
                                        #   in Loop: Header=BB15_14 Depth=1
	movq	$56, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rax
	cmpq	%rsi, %rdi
	jl	.LBB15_14
# BB#18:                                # %for.cond.for.end_crit_edge
	movl	(%rbx), %eax
.LBB15_19:                              # %for.end
	andq	$65535, %rax            # imm = 0xFFFF
	xorq	%rdi, %rdi
	cmpq	$39, %rax
	movabsq	$0, %rax
	movq	%rax, -176(%rbp)        # 8-byte Spill
	movabsq	$0, %r15
	jne	.LBB15_32
# BB#20:                                # %if.then35
	movq	8(%rbx), %r9
	movslq	(%r9), %rdx
	movq	%rsp, %rax
	leaq	15(,%rdx,8), %rcx
	andq	$-16, %rcx
	subq	%rcx, %rax
	movq	%rax, -176(%rbp)        # 8-byte Spill
	movq	%rax, %rsp
	xorq	%r15, %r15
	testq	%rdx, %rdx
	jle	.LBB15_32
# BB#21:
	movq	$1, %rsi
	movabsq	$4294967288, %r8        # imm = 0xFFFFFFF8
	.align	16, 0x90
.LBB15_22:                              # %for.body46
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r9,%rsi,8), %rcx
	movzbl	(%rcx), %edi
	cmpq	$49, %rdi
	jne	.LBB15_30
# BB#23:                                # %if.then56
                                        #   in Loop: Header=BB15_22 Depth=1
	movq	8(%rcx), %rdi
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB15_27
# BB#24:                                # %land.lhs.true70
                                        #   in Loop: Header=BB15_22 Depth=1
	movq	8(%rdi), %rbx
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	je	.LBB15_26
# BB#25:                                # %land.lhs.true70
                                        #   in Loop: Header=BB15_22 Depth=1
	movq	%rdi, %rbx
.LBB15_26:                              # %land.lhs.true70
                                        #   in Loop: Header=BB15_22 Depth=1
	movl	(%rbx), %eax
	movq	%rbx, %rdi
.LBB15_27:                              # %if.end82
                                        #   in Loop: Header=BB15_22 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB15_30
# BB#28:                                # %land.lhs.true87
                                        #   in Loop: Header=BB15_22 Depth=1
	movl	8(%rdi), %eax
	andq	%r8, %rax
	cmpq	$8, %rax
	jne	.LBB15_30
# BB#29:                                # %if.then98
                                        #   in Loop: Header=BB15_22 Depth=1
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	-176(%rbp), %rcx        # 8-byte Reload
	movq	%rdi, (%rcx,%rax,8)
	incq	%r15
	.align	16, 0x90
.LBB15_30:                              # %for.inc104
                                        #   in Loop: Header=BB15_22 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rsi
	cmpq	%rdx, %rax
	jl	.LBB15_22
# BB#31:
	xorq	%rdi, %rdi
.LBB15_32:                              # %if.end107
	movq	%r14, %r12
	movq	-192(%rbp), %rax        # 8-byte Reload
	subq	%rax, %r12
	movb	%dil, -44(%rbp)
	movl	%edi, -48(%rbp)
	movq	$0, -56(%rbp)
	movq	$0, -64(%rbp)
	movq	$0, -72(%rbp)
	movq	$0, -80(%rbp)
	movq	$0, -88(%rbp)
	movq	$0, -96(%rbp)
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB15_46
# BB#33:                                # %for.body111.lr.ph
	xorq	%rbx, %rbx
	movabsq	$4294967288, %r13       # imm = 0xFFFFFFF8
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB15_34:                              # %for.body111
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB15_40 Depth 2
	movq	recog_data(,%rbx,8), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB15_45
# BB#35:                                # %land.lhs.true118
                                        #   in Loop: Header=BB15_34 Depth=1
	movl	8(%rax), %eax
	movq	%rax, %rcx
	andq	%r13, %rcx
	cmpq	$8, %rcx
	jne	.LBB15_45
# BB#36:                                # %if.then134
                                        #   in Loop: Header=BB15_34 Depth=1
	movq	$5, %rcx
	movq	-184(%rbp), %rdx        # 8-byte Reload
	shlq	%cl, %rdx
	imulq	$960, %rbx, %rcx        # imm = 0x3C0
	movslq	recog_op_alt+8(%rcx,%rdx), %rcx
	movl	reg_class_size(,%rcx,4), %ecx
	cmpq	$1, %rcx
	jne	.LBB15_38
# BB#37:                                # %for.cond144.preheader
                                        #   in Loop: Header=BB15_34 Depth=1
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%r13, %r13
	testq	%rdx, %rdx
	jle	.LBB15_42
	.align	16, 0x90
.LBB15_40:                              # %for.body147
                                        #   Parent Loop BB15_34 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-176(%rbp), %rcx        # 8-byte Reload
	movq	(%rcx,%r13,8), %rcx
	movl	8(%rcx), %ecx
	cmpq	%rcx, %rax
	je	.LBB15_41
# BB#39:                                # %for.cond144
                                        #   in Loop: Header=BB15_40 Depth=2
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB15_40
	jmp	.LBB15_42
.LBB15_38:                              # %if.then143
                                        #   in Loop: Header=BB15_34 Depth=1
	xorq	%rax, %rax
	movq	-168(%rbp), %rdi        # 8-byte Reload
	movabsq	$.L.str8, %rsi
	movq	%rbx, %rdx
	callq	error_for_asm
	movq	$1, %rdi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	jmp	.LBB15_45
.LBB15_41:                              # %if.then160
                                        #   in Loop: Header=BB15_34 Depth=1
	movq	reg_names(,%rax,8), %rcx
	xorq	%rax, %rax
	movq	-168(%rbp), %rdi        # 8-byte Reload
	movabsq	$.L.str9, %rsi
	movq	%rbx, %rdx
	callq	error_for_asm
	movq	$1, %rdi
.LBB15_42:                              # %for.end171
                                        #   in Loop: Header=BB15_34 Depth=1
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%r15, %rax
	andq	%rdx, %rax
	andq	%rdx, %r13
	cmpq	%rax, %r13
	jne	.LBB15_44
# BB#43:                                # %if.then174
                                        #   in Loop: Header=BB15_34 Depth=1
	movq	recog_data(,%rbx,8), %rax
	movl	8(%rax), %eax
	movq	$1, %rcx
	movb	%cl, -96(%rbp,%rax)
.LBB15_44:                              # %for.inc185
                                        #   in Loop: Header=BB15_34 Depth=1
	movabsq	$4294967288, %r13       # imm = 0xFFFFFFF8
	.align	16, 0x90
.LBB15_45:                              # %for.inc185
                                        #   in Loop: Header=BB15_34 Depth=1
	incq	%rbx
	movq	%rbx, %rax
	andq	%rdx, %rax
	movq	%r12, %rcx
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jne	.LBB15_34
.LBB15_46:                              # %for.cond188.preheader
	movq	$8, %rdx
	movq	$8, %rsi
	.align	16, 0x90
.LBB15_48:                              # %for.body191
                                        # =>This Inner Loop Header: Depth=1
	movzbl	-96(%rbp,%rdx), %eax
	testq	%rax, %rax
	movq	%rdx, %rax
	je	.LBB15_49
# BB#47:                                # %for.cond188
                                        #   in Loop: Header=BB15_48 Depth=1
	incq	%rdx
	incq	%rsi
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$15, %rax
	movq	%rsi, %rax
	jle	.LBB15_48
.LBB15_49:                              # %for.cond199.preheader
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$15, %rdx
	jg	.LBB15_50
# BB#51:                                # %for.body202.lr.ph
	movq	%r12, -200(%rbp)        # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	.align	16, 0x90
.LBB15_53:                              # %for.body202
                                        # =>This Inner Loop Header: Depth=1
	movzbl	-96(%rbp,%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB15_54
# BB#52:                                # %for.cond199
                                        #   in Loop: Header=BB15_53 Depth=1
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$15, %rdx
	jle	.LBB15_53
	jmp	.LBB15_54
.LBB15_50:
	movq	%r12, -200(%rbp)        # 8-byte Spill
.LBB15_54:                              # %for.end210
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rax
	cmpq	$16, %rax
	je	.LBB15_56
# BB#55:                                # %if.then213
	movabsq	$.L.str10, %rsi
	xorq	%rax, %rax
	movq	-168(%rbp), %rdi        # 8-byte Reload
	callq	error_for_asm
	movq	$1, %rdi
.LBB15_56:                              # %if.end214
	movq	%rdi, -208(%rbp)        # 8-byte Spill
	xorq	%rax, %rax
	movb	%al, -108(%rbp)
	movl	%eax, -112(%rbp)
	movq	$0, -120(%rbp)
	movq	$0, -128(%rbp)
	movq	$0, -136(%rbp)
	movq	$0, -144(%rbp)
	movq	$0, -152(%rbp)
	movq	$0, -160(%rbp)
	movq	$32, %rcx
	movq	-192(%rbp), %rax        # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$8, %rdx
	testq	%rax, %rax
	movq	$8, %rax
	jle	.LBB15_72
# BB#57:                                # %for.body218.lr.ph
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	-200(%rbp), %rdx        # 8-byte Reload
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rax, %rax
	jle	.LBB15_58
	.align	16, 0x90
.LBB15_64:                              # %for.body218.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB15_68 Depth 2
	movq	%r14, %r12
	movq	recog_data(,%rbx,8), %rsi
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	jne	.LBB15_70
# BB#65:                                # %land.lhs.true225.us
                                        #   in Loop: Header=BB15_64 Depth=1
	movl	8(%rsi), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	movq	$1, %r14
	je	.LBB15_68
	jmp	.LBB15_70
	.align	16, 0x90
.LBB15_67:                              # %for.cond243.us.for.body246.us_crit_edge
                                        #   in Loop: Header=BB15_68 Depth=2
	movq	recog_data(,%rbx,8), %rsi
	incq	%r14
.LBB15_68:                              # %for.body246.us
                                        #   Parent Loop BB15_64 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-176(%rbp), %rax        # 8-byte Reload
	movq	-8(%rax,%r14,8), %rdi
	callq	operands_match_p
	testq	%r13, %rax
	jne	.LBB15_69
# BB#66:                                # %for.cond243.us
                                        #   in Loop: Header=BB15_68 Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB15_67
# BB#105:                               # %lor.lhs.false.us
                                        #   in Loop: Header=BB15_64 Depth=1
	movq	$5, %rcx
	movq	-184(%rbp), %rax        # 8-byte Reload
	shlq	%cl, %rax
	imulq	$960, %rbx, %rcx        # imm = 0x3C0
	movslq	recog_op_alt+16(%rcx,%rax), %rax
	testq	%rax, %rax
	js	.LBB15_70
.LBB15_69:                              # %if.then266.us
                                        #   in Loop: Header=BB15_64 Depth=1
	movq	recog_data(,%rbx,8), %rax
	movl	8(%rax), %eax
	movq	$1, %rcx
	movb	%cl, -160(%rbp,%rax)
	.align	16, 0x90
.LBB15_70:                              # %for.inc276.us
                                        #   in Loop: Header=BB15_64 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%r12, %r14
	cmpq	%r14, %rax
	movq	$8, %rdx
	movq	$8, %rax
	jl	.LBB15_64
	jmp	.LBB15_72
.LBB15_58:                              # %for.body218.preheader
	movq	$32, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$5, %rcx
	movq	-184(%rbp), %rax        # 8-byte Reload
	shlq	%cl, %rax
	imulq	$960, %rbx, %rcx        # imm = 0x3C0
	incq	%rbx
	leaq	recog_op_alt+16(%rcx,%rax), %rsi
	movabsq	$4294967288, %rdi       # imm = 0xFFFFFFF8
	movq	$1, %r8
	movq	$8, %rdx
	.align	16, 0x90
.LBB15_59:                              # %for.body218
                                        # =>This Inner Loop Header: Depth=1
	movq	recog_data-8(,%rbx,8), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB15_63
# BB#60:                                # %land.lhs.true225
                                        #   in Loop: Header=BB15_59 Depth=1
	movl	8(%rax), %eax
	movq	%rax, %rcx
	andq	%rdi, %rcx
	cmpq	$8, %rcx
	jne	.LBB15_63
# BB#61:                                # %lor.lhs.false
                                        #   in Loop: Header=BB15_59 Depth=1
	movslq	(%rsi), %rcx
	testq	%rcx, %rcx
	js	.LBB15_63
# BB#62:                                # %if.then266
                                        #   in Loop: Header=BB15_59 Depth=1
	movb	%r8b, -160(%rbp,%rax)
	.align	16, 0x90
.LBB15_63:                              # %for.inc276
                                        #   in Loop: Header=BB15_59 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rbx
	addq	$960, %rsi              # imm = 0x3C0
	cmpq	%r14, %rax
	movq	$8, %rax
	jl	.LBB15_59
	.align	16, 0x90
.LBB15_72:                              # %for.body282
                                        # =>This Inner Loop Header: Depth=1
	movzbl	-160(%rbp,%rdx), %ecx
	testq	%rcx, %rcx
	movq	%rdx, %rsi
	je	.LBB15_73
# BB#71:                                # %for.cond279
                                        #   in Loop: Header=BB15_72 Depth=1
	incq	%rdx
	incq	%rax
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$15, %rsi
	movq	%rax, %rsi
	jle	.LBB15_72
.LBB15_73:                              # %for.cond291.preheader
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$15, %rax
	jg	.LBB15_74
# BB#75:                                # %for.body294.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	-208(%rbp), %rdi        # 8-byte Reload
	movq	%r13, %r8
	.align	16, 0x90
.LBB15_77:                              # %for.body294
                                        # =>This Inner Loop Header: Depth=1
	movzbl	-160(%rbp,%rsi), %eax
	testq	%rax, %rax
	jne	.LBB15_78
# BB#76:                                # %for.cond291
                                        #   in Loop: Header=BB15_77 Depth=1
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$15, %rax
	jle	.LBB15_77
	jmp	.LBB15_78
.LBB15_74:
	movq	-208(%rbp), %rdi        # 8-byte Reload
	movq	%r13, %r8
.LBB15_78:                              # %for.end302
	movq	%r14, -176(%rbp)        # 8-byte Spill
	andq	%r8, %rsi
	cmpq	$16, %rsi
	movq	-200(%rbp), %r15        # 8-byte Reload
	je	.LBB15_80
# BB#79:                                # %if.then305
	movabsq	$.L.str11, %rsi
	xorq	%rax, %rax
	movq	-168(%rbp), %rdi        # 8-byte Reload
	movq	%r8, %rbx
	callq	error_for_asm
	movq	%rbx, %r8
	movq	$1, %rdi
.LBB15_80:                              # %for.cond307.preheader
	movq	$32, %rcx
	movq	-192(%rbp), %rdx        # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB15_81
# BB#82:                                # %for.body311.lr.ph
	movq	$32, %rcx
	movq	%r15, %rax
	movq	%rax, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	testq	%r12, %r12
	jle	.LBB15_90
# BB#83:
	movq	%rdi, %r15
	andq	%r8, %rax
	movq	%rax, %r13
	.align	16, 0x90
.LBB15_84:                              # %for.body311.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB15_86 Depth 2
	movq	$5, %rcx
	movq	-184(%rbp), %rax        # 8-byte Reload
	shlq	%cl, %rax
	imulq	$960, %r12, %rcx        # imm = 0x3C0
	movl	recog_op_alt+16(%rcx,%rax), %eax
	cmpq	%r8, %rax
	jne	.LBB15_89
# BB#85:                                # %for.body324.lr.ph.us
                                        #   in Loop: Header=BB15_84 Depth=1
	movq	$1, %rbx
	.align	16, 0x90
.LBB15_86:                              # %for.body324.us
                                        #   Parent Loop BB15_84 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	recog_data-8(,%rbx,8), %rdi
	movq	recog_data(,%r12,8), %rsi
	movq	%r8, %r14
	callq	operands_match_p
	movq	%r14, %r8
	testq	%r8, %rax
	je	.LBB15_88
# BB#87:                                # %if.then331.us
                                        #   in Loop: Header=BB15_86 Depth=2
	leaq	-1(%rbx), %rdx
	xorq	%rax, %rax
	movq	-168(%rbp), %rdi        # 8-byte Reload
	movabsq	$.L.str12, %rsi
	movq	%r8, %r14
	callq	error_for_asm
	movq	%r14, %r8
	movq	$1, %r15
.LBB15_88:                              # %for.inc333.us
                                        #   in Loop: Header=BB15_86 Depth=2
	movq	%rbx, %rax
	andq	%r8, %rax
	incq	%rbx
	cmpq	%r13, %rax
	jne	.LBB15_86
.LBB15_89:                              # %for.inc337.us
                                        #   in Loop: Header=BB15_84 Depth=1
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	-176(%rbp), %rcx        # 8-byte Reload
	cmpq	%rcx, %rax
	jl	.LBB15_84
	jmp	.LBB15_102
.LBB15_81:
	movq	%rdi, %r15
	jmp	.LBB15_102
.LBB15_90:                              # %for.inc337.preheader
	movq	%r8, %r9
	movq	%rdi, %r15
	movq	-176(%rbp), %rbx        # 8-byte Reload
	leaq	1(%rbx), %r8
	movq	%r8, %rax
	subq	%rdx, %rax
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rbx, %rsi
	jge	.LBB15_92
# BB#91:                                # %for.inc337.preheader
	movq	%rbx, %rax
.LBB15_92:                              # %for.inc337.preheader
	leaq	-1(%rdx,%rax), %rax
	subq	%rbx, %rax
	andq	%r9, %rax
	leaq	1(%rax), %rcx
	movq	%rcx, %rsi
	andq	$-4, %rsi
	andq	$-4, %rcx
	movq	%r12, %rdi
	je	.LBB15_98
# BB#93:                                # %vector.body.preheader
	subq	%rdx, %r8
	movq	$32, %rcx
	movq	%r8, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rbx, %rdi
	jge	.LBB15_95
# BB#94:                                # %vector.body.preheader
	movq	%rbx, %r8
.LBB15_95:                              # %vector.body.preheader
	addq	%r12, %rsi
	leaq	-1(%rdx,%r8), %rcx
	subq	%rbx, %rcx
	andq	%r9, %rcx
	incq	%rcx
	andq	$-4, %rcx
	.align	16, 0x90
.LBB15_96:                              # %vector.body
                                        # =>This Inner Loop Header: Depth=1
	addq	$-4, %rcx
	jne	.LBB15_96
# BB#97:
	movq	%rsi, %rdi
.LBB15_98:                              # %middle.block
	leaq	1(%rax,%r12), %rax
	cmpq	%rdi, %rax
	jne	.LBB15_100
# BB#99:
	movq	%r9, %r8
	jmp	.LBB15_102
.LBB15_100:                             # %for.inc337.preheader1
	incq	%rdi
	movq	%r9, %r8
	.align	16, 0x90
.LBB15_101:                             # %for.inc337
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rdi
	cmpq	%rbx, %rax
	jl	.LBB15_101
.LBB15_102:                             # %for.end339
	movq	$1, %rbx
	testq	%r8, %r15
	je	.LBB15_104
.LBB15_103:                             # %if.then341
	movq	const_int_rtx+512(%rip), %rdx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	callq	gen_rtx_fmt_e
	movq	-168(%rbp), %rcx        # 8-byte Reload
	movq	%rax, 32(%rcx)
.LBB15_104:                             # %cleanup
	movq	%rbx, %rax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB15_9:                               # %if.end67.i
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.get_asm_operand_n_inputs, %rcx
	movq	$804, %rsi              # imm = 0x324
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp259:
	.size	check_asm_stack_operands, .Ltmp259-check_asm_stack_operands
	.cfi_endproc

	.align	16, 0x90
	.type	change_stack,@function
change_stack:                           # @change_stack
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp266:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp267:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp268:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp269:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp270:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp271:
	.cfi_def_cfa_offset 64
.Ltmp272:
	.cfi_offset %rbx, -48
.Ltmp273:
	.cfi_offset %r12, -40
.Ltmp274:
	.cfi_offset %r13, -32
.Ltmp275:
	.cfi_offset %r14, -24
.Ltmp276:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %r14
	xorq	%rdx, %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	jne	.LBB16_6
# BB#1:                                 # %if.then
	movq	current_block(%rip), %rax
	xorq	%rdx, %rdx
	testq	%rax, %rax
	je	.LBB16_5
# BB#2:                                 # %land.lhs.true
	movq	8(%rax), %rax
	cmpq	%r14, %rax
	jne	.LBB16_3
# BB#4:                                 # %land.lhs.true
	movabsq	$1, %rdx
	jmp	.LBB16_5
.LBB16_3:
	movabsq	$0, %rdx
.LBB16_5:                               # %if.end
	movq	24(%r14), %r14
.LBB16_6:                               # %if.end3
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movslq	(%r12), %r13
	testq	%r13, %r13
	js	.LBB16_10
	.align	16, 0x90
.LBB16_7:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%r12,%r13), %ecx
	movq	8(%r15), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rax, %rdx
	jne	.LBB16_9
# BB#8:                                 # %if.then8
                                        #   in Loop: Header=BB16_7 Depth=1
	imulq	$472, %rcx, %rax        # imm = 0x1D8
	movq	FP_mode_reg-3648(%rax), %rdx
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	emit_pop_insn
.LBB16_9:                               # %for.inc
                                        #   in Loop: Header=BB16_7 Depth=1
	movq	$32, %rcx
	leaq	-1(%r13), %rax
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	testq	%r13, %r13
	movq	%rax, %r13
	jg	.LBB16_7
.LBB16_10:                              # %for.end
	movl	(%r15), %esi
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	cmpq	%rax, %rsi
	jne	.LBB16_15
# BB#11:                                # %if.then20
	movl	(%r12), %eax
	movl	%eax, (%r15)
	movq	16(%r12), %rax
	movq	%rax, 16(%r15)
	jmp	.LBB16_12
.LBB16_15:                              # %if.else
	movq	8(%r12), %rax
	movq	8(%r15), %rcx
	cmpq	%rcx, %rax
	jne	.LBB16_16
# BB#18:                                # %win
	movl	(%r12), %eax
	cmpq	%rsi, %rax
	jne	.LBB16_19
# BB#20:                                # %if.end36
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	cmpq	%rax, %rsi
	je	.LBB16_12
# BB#21:                                # %while.cond.preheader
	movq	%rsi, %rax
	jmp	.LBB16_22
	.align	16, 0x90
.LBB16_31:                              # %while.cond.backedge
                                        #   in Loop: Header=BB16_22 Depth=1
	movq	FP_mode_reg-3648(%rax), %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	emit_swap_insn
	movl	(%r12), %eax
	movl	(%r15), %esi
.LBB16_22:                              # %while.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB16_26 Depth 2
                                        #     Child Loop BB16_34 Depth 2
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	16(%r12,%rdx), %edi
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	16(%r15,%rdx), %ebx
	movq	$32, %rcx
	cmpq	%rbx, %rdi
	jne	.LBB16_23
# BB#36:                                # %for.cond87.preheader
                                        #   in Loop: Header=BB16_22 Depth=1
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jns	.LBB16_34
	jmp	.LBB16_37
	.align	16, 0x90
.LBB16_33:                              # %for.cond87.for.body90_crit_edge
                                        #   in Loop: Header=BB16_34 Depth=2
	movzbl	15(%r15,%rdx), %edi
	decq	%rdx
.LBB16_34:                              # %for.body90
                                        #   Parent Loop BB16_22 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	16(%r12,%rdx), %ecx
	andq	$255, %rdi
	cmpq	%rcx, %rdi
	jne	.LBB16_35
# BB#32:                                # %for.cond87
                                        #   in Loop: Header=BB16_34 Depth=2
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jg	.LBB16_33
	jmp	.LBB16_37
	.align	16, 0x90
.LBB16_23:                              # %for.cond54.preheader
                                        #   in Loop: Header=BB16_22 Depth=1
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB16_26
	jmp	.LBB16_28
	.align	16, 0x90
.LBB16_25:                              # %for.cond54.for.body57_crit_edge
                                        #   in Loop: Header=BB16_26 Depth=2
	movzbl	15(%r15,%rdx), %ebx
	decq	%rdx
.LBB16_26:                              # %for.body57
                                        #   Parent Loop BB16_22 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rbx
	cmpq	%rdi, %rbx
	movq	%rdx, %rsi
	je	.LBB16_28
# BB#24:                                # %for.cond54
                                        #   in Loop: Header=BB16_26 Depth=2
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB16_25
# BB#27:                                # %for.cond54.for.end73_crit_edge
                                        #   in Loop: Header=BB16_22 Depth=1
	decq	%rdx
	movq	%rdx, %rsi
.LBB16_28:                              # %for.end73
                                        #   in Loop: Header=BB16_22 Depth=1
	movq	%rsi, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	je	.LBB16_29
# BB#30:                                # %if.end77
                                        #   in Loop: Header=BB16_22 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	16(%r12,%rsi), %eax
	imulq	$472, %rax, %rax        # imm = 0x1D8
	jmp	.LBB16_31
	.align	16, 0x90
.LBB16_35:                              # %if.then101
                                        #   in Loop: Header=BB16_22 Depth=1
	imulq	$472, %rcx, %rax        # imm = 0x1D8
	jmp	.LBB16_31
.LBB16_37:                              # %if.end116
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB16_12
# BB#38:                                # %for.body121.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	.align	16, 0x90
.LBB16_40:                              # %for.body121
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%r12,%rax), %ecx
	movzbl	16(%r15,%rax), %edx
	cmpq	%rdx, %rcx
	jne	.LBB16_41
# BB#39:                                # %for.cond118
                                        #   in Loop: Header=BB16_40 Depth=1
	movq	$32, %rcx
	leaq	-1(%rax), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rdx, %rax
	jg	.LBB16_40
.LBB16_12:                              # %if.end137
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	je	.LBB16_14
# BB#13:                                # %if.then139
	movq	16(%r14), %rax
	movq	current_block(%rip), %rcx
	movq	%rax, 8(%rcx)
.LBB16_14:                              # %if.end144
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB16_16:                              # %if.end30
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.change_stack, %rcx
	movq	$2293, %rsi             # imm = 0x8F5
	jmp	.LBB16_17
.LBB16_19:                              # %if.then35
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.change_stack, %rcx
	movq	$2296, %rsi             # imm = 0x8F8
	jmp	.LBB16_17
.LBB16_29:                              # %if.then76
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.change_stack, %rcx
	movq	$2320, %rsi             # imm = 0x910
	jmp	.LBB16_17
.LBB16_41:                              # %if.then132
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.change_stack, %rcx
	movq	$2343, %rsi             # imm = 0x927
.LBB16_17:                              # %if.end30
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp277:
	.size	change_stack, .Ltmp277-change_stack
	.cfi_endproc

	.type	stack_regs_mentioned_data,@object # @stack_regs_mentioned_data
	.local	stack_regs_mentioned_data
	.comm	stack_regs_mentioned_data,8,8
	.type	FP_mode_reg,@object     # @FP_mode_reg
	.local	FP_mode_reg
	.comm	FP_mode_reg,3776,16
	.type	ix86_flags_rtx,@object  # @ix86_flags_rtx
	.local	ix86_flags_rtx
	.comm	ix86_flags_rtx,8,8
	.type	nan,@object             # @nan
	.local	nan
	.comm	nan,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"stack_regs_mentioned cache"
	.size	.L.str, 27

	.type	current_block,@object   # @current_block
	.local	current_block
	.comm	current_block,8,8
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"\nBasic block %d\nInput stack: "
	.size	.L.str1, 30

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"reg-stack.c"
	.size	.L.str2, 12

	.type	.L__FUNCTION__.convert_regs_1,@object # @__FUNCTION__.convert_regs_1
.L__FUNCTION__.convert_regs_1:
	.asciz	"convert_regs_1"
	.size	.L__FUNCTION__.convert_regs_1, 15

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"  insn %d input stack: "
	.size	.L.str3, 24

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"Expected live registers ["
	.size	.L.str4, 26

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	" %d"
	.size	.L.str5, 4

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	" ]\nOutput stack: "
	.size	.L.str6, 18

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"Emitting insn initializing reg %d\n"
	.size	.L.str7, 35

	.type	.L__FUNCTION__.emit_pop_insn,@object # @__FUNCTION__.emit_pop_insn
.L__FUNCTION__.emit_pop_insn:
	.asciz	"emit_pop_insn"
	.size	.L__FUNCTION__.emit_pop_insn, 14

	.type	.L__FUNCTION__.get_hard_regnum,@object # @__FUNCTION__.get_hard_regnum
.L__FUNCTION__.get_hard_regnum:
	.asciz	"get_hard_regnum"
	.size	.L__FUNCTION__.get_hard_regnum, 16

	.type	.L__FUNCTION__.subst_stack_regs_pat,@object # @__FUNCTION__.subst_stack_regs_pat
.L__FUNCTION__.subst_stack_regs_pat:
	.asciz	"subst_stack_regs_pat"
	.size	.L__FUNCTION__.subst_stack_regs_pat, 21

	.type	.L__FUNCTION__.remove_regno_note,@object # @__FUNCTION__.remove_regno_note
.L__FUNCTION__.remove_regno_note:
	.asciz	"remove_regno_note"
	.size	.L__FUNCTION__.remove_regno_note, 18

	.type	.L__FUNCTION__.emit_swap_insn,@object # @__FUNCTION__.emit_swap_insn
.L__FUNCTION__.emit_swap_insn:
	.asciz	"emit_swap_insn"
	.size	.L__FUNCTION__.emit_swap_insn, 15

	.type	.L__FUNCTION__.move_for_stack_reg,@object # @__FUNCTION__.move_for_stack_reg
.L__FUNCTION__.move_for_stack_reg:
	.asciz	"move_for_stack_reg"
	.size	.L__FUNCTION__.move_for_stack_reg, 19

	.type	.L__FUNCTION__.replace_reg,@object # @__FUNCTION__.replace_reg
.L__FUNCTION__.replace_reg:
	.asciz	"replace_reg"
	.size	.L__FUNCTION__.replace_reg, 12

	.type	.L__FUNCTION__.subst_asm_stack_regs,@object # @__FUNCTION__.subst_asm_stack_regs
.L__FUNCTION__.subst_asm_stack_regs:
	.asciz	"subst_asm_stack_regs"
	.size	.L__FUNCTION__.subst_asm_stack_regs, 21

	.type	.L__FUNCTION__.change_stack,@object # @__FUNCTION__.change_stack
.L__FUNCTION__.change_stack:
	.asciz	"change_stack"
	.size	.L__FUNCTION__.change_stack, 13

	.type	.L__FUNCTION__.get_asm_operand_n_inputs,@object # @__FUNCTION__.get_asm_operand_n_inputs
.L__FUNCTION__.get_asm_operand_n_inputs:
	.asciz	"get_asm_operand_n_inputs"
	.size	.L__FUNCTION__.get_asm_operand_n_inputs, 25

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"output constraint %d must specify a single register"
	.size	.L.str8, 52

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"output constraint %d cannot be specified together with \"%s\" clobber"
	.size	.L.str9, 68

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"output regs must be grouped at top of stack"
	.size	.L.str10, 44

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"implicitly popped regs must be grouped at top of stack"
	.size	.L.str11, 55

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"output operand %d must use `&' constraint"
	.size	.L.str12, 42

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"uninitialized\n"
	.size	.L.str13, 15

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"empty\n"
	.size	.L.str14, 7

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"[ "
	.size	.L.str15, 3

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"%d "
	.size	.L.str16, 4

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"]\n"
	.size	.L.str17, 3

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"Edge %d->%d: "
	.size	.L.str18, 14

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"new block; copying stack position\n"
	.size	.L.str19, 35

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"new block; pops needed\n"
	.size	.L.str20, 24

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"no changes needed\n"
	.size	.L.str21, 19

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"correcting stack to "
	.size	.L.str22, 21

	.type	.L__FUNCTION__.compensate_edge,@object # @__FUNCTION__.compensate_edge
.L__FUNCTION__.compensate_edge:
	.asciz	"compensate_edge"
	.size	.L__FUNCTION__.compensate_edge, 16


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
