	.file	"ssa-ccp.c.bc"
	.text
	.globl	ssa_const_prop
	.align	16, 0x90
	.type	ssa_const_prop,@function
ssa_const_prop:                         # @ssa_const_prop
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
	callq	init_alias_analysis
	callq	df_init
	movq	%rax, df_analyzer(%rip)
	xorq	%rsi, %rsi
	movq	$1248, %rdx             # imm = 0x4E0
	xorq	%rbx, %rbx
	movq	%rax, %rdi
	callq	df_analyse
	callq	get_max_uid
	movq	%rax, %rdi
	callq	compute_bb_for_insn
	movq	df_analyzer(%rip), %rdi
	callq	ssa_fast_dce
	callq	create_edge_list
	movq	ssa_definition(%rip), %rcx
	movq	%rax, edges(%rip)
	movq	(%rcx), %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	ssa_definition(%rip), %rcx
	movq	%rax, values(%rip)
	cmpq	$0, (%rcx)
	je	.LBB0_5
# BB#1:
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	xorq	%rsi, %rsi
	jmp	.LBB0_2
	.align	16, 0x90
.LBB0_3:                                # %for.body.for.body_crit_edge
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	values(%rip), %rax
.LBB0_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rcx
	andq	%rdx, %rcx
	cmpq	$53, %rcx
	sbbq	%rdi, %rdi
	movq	$4, %rcx
	shlq	%cl, %rbx
	andq	$2, %rdi
	movl	%edi, (%rax,%rbx)
	movq	$0, 8(%rax,%rbx)
	movq	ssa_definition(%rip), %rax
	movq	(%rax), %rax
	incq	%rsi
	movq	%rsi, %rbx
	andq	%rdx, %rbx
	cmpq	%rax, %rbx
	jb	.LBB0_3
# BB#4:                                 # %for.cond.for.end_crit_edge
	movq	%rax, %rbx
.LBB0_5:                                # %for.end
	movq	%rbx, %rdi
	callq	sbitmap_alloc
	movq	%rax, ssa_edges(%rip)
	movq	%rax, %rdi
	callq	sbitmap_zero
	movl	n_basic_blocks(%rip), %edi
	callq	sbitmap_alloc
	movq	%rax, executable_blocks(%rip)
	movq	%rax, %rdi
	callq	sbitmap_zero
	movq	edges(%rip), %rax
	movl	4(%rax), %edi
	callq	sbitmap_alloc
	movq	%rax, executable_edges(%rip)
	movq	%rax, %rdi
	callq	sbitmap_zero
	movq	edges(%rip), %rax
	movslq	4(%rax), %rdi
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	entry_exit_blocks+40(%rip), %rbx
	movq	%rax, edge_info(%rip)
	xorq	%r15, %r15
	testq	%rbx, %rbx
	movq	%rbx, flow_edges(%rip)
	je	.LBB0_9
# BB#6:
	movabsq	$4294967232, %r14       # imm = 0xFFFFFFC0
	.align	16, 0x90
.LBB0_7:                                # %for.body24
                                        # =>This Inner Loop Header: Depth=1
	movq	edges(%rip), %rdi
	movq	16(%rbx), %rsi
	movq	24(%rbx), %rdx
	callq	find_edge_index
	movq	%rax, %rdx
	andq	%r14, %rdx
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	movq	executable_edges(%rip), %rdi
	movq	$3, %rcx
	shrq	%cl, %rdx
	orq	%rsi, 16(%rdi,%rdx)
	movq	8(%rbx), %rdx
	movq	$32, %rcx
	movq	edge_info(%rip), %rsi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, (%rsi,%rax,8)
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_7
# BB#8:                                 # %for.cond23.do.bodythread-pre-split_crit_edge
	movq	flow_edges(%rip), %r15
	.align	16, 0x90
.LBB0_9:                                # %do.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_13 Depth 2
                                        #       Child Loop BB0_15 Depth 3
                                        #       Child Loop BB0_24 Depth 3
                                        #     Child Loop BB0_39 Depth 2
                                        #       Child Loop BB0_40 Depth 3
	testq	%r15, %r15
	je	.LBB0_35
# BB#10:                                #   in Loop: Header=BB0_9 Depth=1
	movabsq	$entry_exit_blocks+112, %r13
	movabsq	$0, %r12
	.align	16, 0x90
.LBB0_13:                               # %while.body.i
                                        #   Parent Loop BB0_9 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_15 Depth 3
                                        #       Child Loop BB0_24 Depth 3
	movq	16(%r15), %rsi
	movq	24(%r15), %r14
	movq	edges(%rip), %rdi
	movq	%r14, %rdx
	callq	find_edge_index
	movq	$32, %rcx
	movq	edge_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%rdx,%rax,8), %r15
	cmpq	%r13, %r14
	movq	%r15, flow_edges(%rip)
	je	.LBB0_12
# BB#14:                                # %if.end.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	%r14, %rdi
	callq	first_insn_after_basic_block_note
	movq	%rax, %rbx
	jmp	.LBB0_15
	.align	16, 0x90
.LBB0_19:                               # %for.body.i
                                        #   in Loop: Header=BB0_15 Depth=3
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	visit_phi_node
	movq	24(%rbx), %rbx
.LBB0_15:                               # %if.end.i
                                        #   Parent Loop BB0_9 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB0_20
# BB#16:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB0_15 Depth=3
	movzbl	(%rbx), %eax
	cmpq	$32, %rax
	jne	.LBB0_20
# BB#17:                                # %land.lhs.true5.i
                                        #   in Loop: Header=BB0_15 Depth=3
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB0_20
# BB#18:                                # %land.rhs.i
                                        #   in Loop: Header=BB0_15 Depth=3
	movq	16(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$152, %rax
	je	.LBB0_19
	.align	16, 0x90
.LBB0_20:                               # %for.end.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movl	88(%r14), %eax
	movq	executable_blocks(%rip), %rdx
	movq	$6, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	movq	16(%rdx,%rsi,8), %rdi
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	testq	%rdi, %rbx
	jne	.LBB0_11
# BB#21:                                # %if.then26.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	40(%r14), %r15
	orq	%rdi, %rbx
	movq	%rbx, 16(%rdx,%rsi,8)
	movq	(%r14), %rbx
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %ecx
	movq	8(%r14), %rax
	cmpq	$105, %rcx
	movq	%r12, %rcx
	jne	.LBB0_23
# BB#22:                                # %if.then26.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movabsq	$1, %rcx
.LBB0_23:                               # %if.then26.i
                                        #   in Loop: Header=BB0_13 Depth=2
	cmpq	%rax, %rbx
	movq	%rbx, %rdi
	je	.LBB0_29
	.align	16, 0x90
.LBB0_24:                               # %while.body37.i
                                        #   Parent Loop BB0_9 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	$1, %rcx
	je	.LBB0_26
# BB#25:                                # %if.then44.i
                                        #   in Loop: Header=BB0_24 Depth=3
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	visit_expression
	movq	8(%r14), %rax
.LBB0_26:                               # %if.end45.i
                                        #   in Loop: Header=BB0_24 Depth=3
	movq	24(%rbx), %rbx
	movzbl	(%rbx), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	movq	%r12, %rcx
	jne	.LBB0_28
# BB#27:                                # %if.end45.i
                                        #   in Loop: Header=BB0_24 Depth=3
	movabsq	$1, %rcx
.LBB0_28:                               # %if.end45.i
                                        #   in Loop: Header=BB0_24 Depth=3
	cmpq	%rax, %rbx
	movq	%rax, %rdi
	jne	.LBB0_24
.LBB0_29:                               # %while.end.i
                                        #   in Loop: Header=BB0_13 Depth=2
	testq	%rcx, %rcx
	je	.LBB0_31
# BB#30:                                # %if.then56.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	%r14, %rsi
	callq	visit_expression
.LBB0_31:                               # %if.end57.i
                                        #   in Loop: Header=BB0_13 Depth=2
	testq	%r15, %r15
	je	.LBB0_11
# BB#32:                                # %land.lhs.true60.i
                                        #   in Loop: Header=BB0_13 Depth=2
	cmpq	$0, 8(%r15)
	jne	.LBB0_11
# BB#33:                                # %land.lhs.true63.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	edges(%rip), %rdi
	movq	16(%r15), %rsi
	movq	24(%r15), %rdx
	callq	find_edge_index
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rax
	movq	executable_edges(%rip), %rdx
	movq	$3, %rcx
	shrq	%cl, %rax
	movq	16(%rdx,%rax), %rbx
	movq	edges(%rip), %rdi
	movq	16(%r15), %rsi
	movq	24(%r15), %rdx
	callq	find_edge_index
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	%rbx, %rdx
	jne	.LBB0_11
# BB#34:                                # %if.then79.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	edges(%rip), %rdi
	movq	16(%r15), %rsi
	movq	24(%r15), %rdx
	callq	find_edge_index
	movq	edges(%rip), %rdi
	movq	16(%r15), %rsi
	movq	24(%r15), %rdx
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	callq	find_edge_index
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rax
	movq	executable_edges(%rip), %rdx
	movq	$3, %rcx
	shrq	%cl, %rax
	orq	%rbx, 16(%rdx,%rax)
	movq	flow_edges(%rip), %rbx
	movq	edges(%rip), %rdi
	movq	16(%r15), %rsi
	movq	24(%r15), %rdx
	callq	find_edge_index
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	edge_info(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rbx, (%rdx,%rax,8)
	movq	%r15, flow_edges(%rip)
	jmp	.LBB0_12
	.align	16, 0x90
.LBB0_11:                               # %while.condthread-pre-split.loopexit.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	flow_edges(%rip), %r15
.LBB0_12:                               # %while.condthread-pre-split.loopexit.i
                                        #   in Loop: Header=BB0_13 Depth=2
	testq	%r15, %r15
	jne	.LBB0_13
.LBB0_35:                               # %examine_flow_edges.exit
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	ssa_edges(%rip), %rdi
	callq	sbitmap_first_set_bit
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB0_49
# BB#36:                                #   in Loop: Header=BB0_9 Depth=1
	movabsq	$4294967232, %r14       # imm = 0xFFFFFFC0
	.align	16, 0x90
.LBB0_39:                               # %while.body.i53
                                        #   Parent Loop BB0_9 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_40 Depth 3
	movq	ssa_edges(%rip), %rdi
	callq	sbitmap_first_set_bit
	movq	%rax, %rdx
	andq	%r14, %rdx
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	movq	ssa_edges(%rip), %rdi
	notq	%rsi
	movq	$3, %rcx
	shrq	%cl, %rdx
	andq	%rsi, 16(%rdi,%rdx)
	movq	df_analyzer(%rip), %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	40(%rdx), %rdx
	sarq	%cl, %rax
	movq	$5, %rcx
	shlq	%cl, %rax
	movq	8(%rdx,%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB0_38
	.align	16, 0x90
.LBB0_40:                               # %for.body.i54
                                        #   Parent Loop BB0_9 Depth=1
                                        #     Parent Loop BB0_39 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%rbx), %rax
	movq	8(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB0_46
# BB#41:                                # %land.lhs.true.i57
                                        #   in Loop: Header=BB0_40 Depth=3
	movzbl	(%rdi), %eax
	cmpq	$32, %rax
	jne	.LBB0_46
# BB#42:                                # %land.lhs.true6.i
                                        #   in Loop: Header=BB0_40 Depth=3
	movq	32(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB0_46
# BB#43:                                # %land.lhs.true11.i
                                        #   in Loop: Header=BB0_40 Depth=3
	movq	16(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$152, %rax
	jne	.LBB0_46
# BB#44:                                # %if.then.i
                                        #   in Loop: Header=BB0_40 Depth=3
	movslq	8(%rdi), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rsi
	movl	88(%rsi), %ecx
	movq	executable_blocks(%rip), %rax
	movq	%rcx, %rdx
	shrq	$6, %rdx
	movq	16(%rax,%rdx,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rax, %rdx
	je	.LBB0_48
# BB#45:                                # %if.then42.i
                                        #   in Loop: Header=BB0_40 Depth=3
	callq	visit_phi_node
	jmp	.LBB0_48
	.align	16, 0x90
.LBB0_46:                               # %if.else.i
                                        #   in Loop: Header=BB0_40 Depth=3
	movslq	8(%rdi), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rsi
	movl	88(%rsi), %ecx
	movq	executable_blocks(%rip), %rax
	movq	%rcx, %rdx
	shrq	$6, %rdx
	movq	16(%rax,%rdx,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rax, %rdx
	je	.LBB0_48
# BB#47:                                # %if.then77.i
                                        #   in Loop: Header=BB0_40 Depth=3
	callq	visit_expression
.LBB0_48:                               # %for.inc.i
                                        #   in Loop: Header=BB0_40 Depth=3
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_40
# BB#37:                                # %for.cond.while.cond.loopexit_crit_edge.i
                                        #   in Loop: Header=BB0_39 Depth=2
	movq	ssa_edges(%rip), %rdi
.LBB0_38:                               # %while.cond.loopexit.i
                                        #   in Loop: Header=BB0_39 Depth=2
	callq	sbitmap_first_set_bit
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB0_39
.LBB0_49:                               # %follow_def_use_chains.exit
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	flow_edges(%rip), %r15
	testq	%r15, %r15
	jne	.LBB0_9
# BB#50:                                # %do.end
	movq	ssa_definition(%rip), %rax
	movq	(%rax), %rcx
	cmpq	$54, %rcx
	jb	.LBB0_78
# BB#51:
	movq	$53, %r13
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	$53, %r14
	.align	16, 0x90
.LBB0_52:                               # %for.body.i63
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_67 Depth 2
	movq	values(%rip), %rdx
	movq	$4, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movl	(%rdx,%rsi), %ecx
	cmpq	$1, %rcx
	jne	.LBB0_77
# BB#53:                                # %if.then.i70
                                        #   in Loop: Header=BB0_52 Depth=1
	movq	32(%rax,%r13,8), %r12
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_77
# BB#54:                                # %cond.true.i
                                        #   in Loop: Header=BB0_52 Depth=1
	movq	32(%r12), %rbx
	movzbl	(%rbx), %eax
	cmpq	$47, %rax
	je	.LBB0_56
# BB#55:                                # %cond.false.i
                                        #   in Loop: Header=BB0_52 Depth=1
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	single_set_2
	movq	%rax, %rbx
.LBB0_56:                               # %cond.end25.i
                                        #   in Loop: Header=BB0_52 Depth=1
	testq	%rbx, %rbx
	je	.LBB0_77
# BB#57:                                # %if.end.i73
                                        #   in Loop: Header=BB0_52 Depth=1
	movzbl	(%r12), %eax
	testq	%r12, %r12
	je	.LBB0_61
# BB#58:                                # %if.end.i73
                                        #   in Loop: Header=BB0_52 Depth=1
	cmpq	$32, %rax
	jne	.LBB0_61
# BB#59:                                # %land.lhs.true33.i
                                        #   in Loop: Header=BB0_52 Depth=1
	movq	32(%r12), %rcx
	movzbl	(%rcx), %edx
	cmpq	$47, %rdx
	jne	.LBB0_61
# BB#60:                                # %land.lhs.true41.i
                                        #   in Loop: Header=BB0_52 Depth=1
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$152, %rcx
	je	.LBB0_66
.LBB0_61:                               # %land.lhs.true52.i
                                        #   in Loop: Header=BB0_52 Depth=1
	cmpq	$32, %rax
	jne	.LBB0_63
# BB#62:                                # %land.lhs.true57.i
                                        #   in Loop: Header=BB0_52 Depth=1
	movq	16(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	je	.LBB0_66
.LBB0_63:                               # %if.then65.i
                                        #   in Loop: Header=BB0_52 Depth=1
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_65
# BB#64:                                # %if.then67.i
                                        #   in Loop: Header=BB0_52 Depth=1
	movq	32(%r12), %rax
	movq	8(%rax), %rax
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str2, %rsi
	callq	fprintf
.LBB0_65:                               # %if.end77.i
                                        #   in Loop: Header=BB0_52 Depth=1
	movq	values(%rip), %rax
	movq	$4, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	8(%rax,%rdx), %rax
	movq	%rax, 16(%rbx)
	movl	%r15d, 40(%r12)
	movslq	8(%r12), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	df_analyzer(%rip), %rdi
	movq	32(%rcx,%rax,8), %rsi
	movq	%r12, %rdx
	callq	df_insn_modify
.LBB0_66:                               # %if.end91.i
                                        #   in Loop: Header=BB0_52 Depth=1
	movq	df_analyzer(%rip), %rax
	movq	40(%rax), %rax
	movq	$5, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	8(%rax,%rdx), %rbx
	jmp	.LBB0_67
	.align	16, 0x90
.LBB0_76:                               # %for.inc.i82
                                        #   in Loop: Header=BB0_67 Depth=2
	movq	(%rbx), %rbx
.LBB0_67:                               # %if.end91.i
                                        #   Parent Loop BB0_52 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB0_77
# BB#68:                                # %for.body96.i
                                        #   in Loop: Header=BB0_67 Depth=2
	movq	8(%rbx), %rax
	movq	8(%rax), %r12
	movl	(%r12), %eax
	testq	$134217728, %rax        # imm = 0x8000000
	jne	.LBB0_76
# BB#69:                                # %land.lhs.true100.i
                                        #   in Loop: Header=BB0_67 Depth=2
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$37, %rcx
	jne	.LBB0_71
# BB#70:                                # %land.lhs.true105.i
                                        #   in Loop: Header=BB0_67 Depth=2
	movl	40(%r12), %ecx
	movabsq	$4294967197, %rdx       # imm = 0xFFFFFF9D
	cmpq	%rdx, %rcx
	je	.LBB0_76
.LBB0_71:                               # %land.lhs.true111.i
                                        #   in Loop: Header=BB0_67 Depth=2
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$32, %rax
	jne	.LBB0_76
# BB#72:                                # %if.then120.i
                                        #   in Loop: Header=BB0_67 Depth=2
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	values(%rip), %rdx
	movq	(%rax,%r13,8), %rdi
	movq	$4, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	8(%rdx,%rax), %rsi
	movq	%r12, %rdx
	callq	validate_replace_src
	testq	%r15, %rax
	je	.LBB0_76
# BB#73:                                # %if.then128.i
                                        #   in Loop: Header=BB0_67 Depth=2
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_75
# BB#74:                                # %if.then130.i
                                        #   in Loop: Header=BB0_67 Depth=2
	movl	8(%r12), %ecx
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movq	%r14, %rdx
	callq	fprintf
.LBB0_75:                               # %if.end135.i
                                        #   in Loop: Header=BB0_67 Depth=2
	movl	%r15d, 40(%r12)
	movslq	8(%r12), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	df_analyzer(%rip), %rdi
	movq	32(%rcx,%rax,8), %rsi
	movq	%r12, %rdx
	callq	df_insn_modify
	jmp	.LBB0_76
	.align	16, 0x90
.LBB0_77:                               # %for.inc149.i
                                        #   in Loop: Header=BB0_52 Depth=1
	movq	ssa_definition(%rip), %rax
	movq	(%rax), %rcx
	incq	%r14
	movq	%r14, %r13
	andq	%r15, %r13
	cmpq	%rcx, %r13
	jb	.LBB0_52
.LBB0_78:                               # %ssa_ccp_substitute_constants.exit
	movq	edges(%rip), %r14
	movslq	4(%r14), %rax
	testq	%rax, %rax
	jle	.LBB0_95
# BB#79:                                # %for.body.lr.ph.i
	movq	executable_edges(%rip), %r15
	xorq	%r13, %r13
	.align	16, 0x90
.LBB0_80:                               # %for.body.i89
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_84 Depth 2
	movq	$3, %rcx
	movq	%r13, %rdx
	shrq	%cl, %rdx
	andq	$536870904, %rdx        # imm = 0x1FFFFFF8
	movq	16(%r15,%rdx), %rdx
	movq	$1, %rsi
	movq	%r13, %rcx
	shlq	%cl, %rsi
	testq	%rdx, %rsi
	jne	.LBB0_94
# BB#81:                                # %if.then.i91
                                        #   in Loop: Header=BB0_80 Depth=1
	movq	8(%r14), %rcx
	movq	(%rcx,%r13,8), %r12
	movzbl	48(%r12), %ecx
	testq	$2, %rcx
	jne	.LBB0_94
# BB#82:                                # %if.end.i93
                                        #   in Loop: Header=BB0_80 Depth=1
	movq	24(%r12), %rdi
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %rdi
	je	.LBB0_91
# BB#83:                                # %if.then7.i
                                        #   in Loop: Header=BB0_80 Depth=1
	callq	first_insn_after_basic_block_note
	movq	%rax, %rbx
	jmp	.LBB0_84
	.align	16, 0x90
.LBB0_90:                               # %if.end41.i
                                        #   in Loop: Header=BB0_84 Depth=2
	movq	24(%rbx), %rbx
.LBB0_84:                               # %if.then7.i
                                        #   Parent Loop BB0_80 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB0_91
# BB#85:                                # %land.lhs.true.i99
                                        #   in Loop: Header=BB0_84 Depth=2
	movzbl	(%rbx), %eax
	cmpq	$32, %rax
	jne	.LBB0_91
# BB#86:                                # %land.lhs.true11.i104
                                        #   in Loop: Header=BB0_84 Depth=2
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$47, %rax
	jne	.LBB0_91
# BB#87:                                # %land.rhs.i107
                                        #   in Loop: Header=BB0_84 Depth=2
	movq	16(%rdi), %rax
	movzbl	(%rax), %eax
	cmpq	$152, %rax
	jne	.LBB0_91
# BB#88:                                # %while.body.i108
                                        #   in Loop: Header=BB0_84 Depth=2
	movq	16(%r12), %rsi
	callq	remove_phi_alternative
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_90
# BB#89:                                # %if.then30.i
                                        #   in Loop: Header=BB0_84 Depth=2
	movq	32(%rbx), %rax
	movq	16(%r12), %rcx
	movq	8(%rax), %rax
	movl	88(%rcx), %edx
	movl	8(%rax), %ecx
	xorq	%rax, %rax
	movabsq	$.L.str, %rsi
	callq	fprintf
	jmp	.LBB0_90
	.align	16, 0x90
.LBB0_91:                               # %if.end45.i111
                                        #   in Loop: Header=BB0_80 Depth=1
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_93
# BB#92:                                # %if.then47.i
                                        #   in Loop: Header=BB0_80 Depth=1
	movq	16(%r12), %rax
	movq	24(%r12), %rcx
	movl	88(%rcx), %ecx
	movl	88(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str1, %rsi
	callq	fprintf
.LBB0_93:                               # %if.end53.i
                                        #   in Loop: Header=BB0_80 Depth=1
	movq	%r12, %rdi
	callq	remove_edge
	movl	4(%r14), %eax
.LBB0_94:                               # %for.inc.i114
                                        #   in Loop: Header=BB0_80 Depth=1
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB0_80
.LBB0_95:                               # %for.cond55.preheader.i
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_108
# BB#96:
	movq	$4, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_97:                               # %for.body57.i
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	(%rax,%rbx,8), %r12
	cmpq	$0, 32(%r12)
	je	.LBB0_107
# BB#98:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB0_97 Depth=1
	movq	8(%r12), %r14
	movzbl	(%r14), %eax
	cmpq	$33, %rax
	jne	.LBB0_107
# BB#99:                                # %if.end68.i
                                        #   in Loop: Header=BB0_97 Depth=1
	movq	40(%r12), %r13
	movq	%r14, %rdi
	callq	condjump_p
	testq	%r15, %rax
	je	.LBB0_107
# BB#100:                               # %land.lhs.true71.i
                                        #   in Loop: Header=BB0_97 Depth=1
	movq	%r14, %rdi
	callq	simplejump_p
	testq	%r15, %rax
	jne	.LBB0_107
# BB#101:                               # %land.lhs.true74.i
                                        #   in Loop: Header=BB0_97 Depth=1
	movq	40(%r12), %rax
	testq	%rax, %rax
	je	.LBB0_107
# BB#102:                               # %land.lhs.true77.i
                                        #   in Loop: Header=BB0_97 Depth=1
	cmpq	$0, 8(%rax)
	jne	.LBB0_107
# BB#103:                               # %if.then80.i
                                        #   in Loop: Header=BB0_97 Depth=1
	movzbl	48(%r13), %eax
	testq	$1, %rax
	jne	.LBB0_104
# BB#105:                               # %if.else.i123
                                        #   in Loop: Header=BB0_97 Depth=1
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	movq	64(%r14), %rdx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$67, %rdi
	callq	gen_rtx_fmt_u00
	movq	32(%r14), %rcx
	movq	%rax, 16(%rcx)
	movq	%r14, %rdi
	callq	emit_barrier_after
	movl	%r15d, 40(%r14)
	jmp	.LBB0_106
.LBB0_104:                              # %if.then84.i
                                        #   in Loop: Header=BB0_97 Depth=1
	movq	$37, %rax
	movw	%ax, (%r14)
	movabsq	$4294967197, %rax       # imm = 0xFFFFFF9D
	movl	%eax, 40(%r14)
.LBB0_106:                              # %if.end105.i
                                        #   in Loop: Header=BB0_97 Depth=1
	movslq	8(%r14), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	df_analyzer(%rip), %rdi
	movq	32(%rcx,%rax,8), %rsi
	movq	%r14, %rdx
	callq	df_insn_modify
	.align	16, 0x90
.LBB0_107:                              # %for.inc114.i
                                        #   in Loop: Header=BB0_97 Depth=1
	movslq	n_basic_blocks(%rip), %rax
	leaq	-3(%rbx), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB0_97
.LBB0_108:                              # %optimize_unexecutable_edges.exit
	callq	find_unreachable_blocks
	movslq	n_basic_blocks(%rip), %r14
	testq	%r14, %r14
	jle	.LBB0_112
# BB#109:                               # %for.body.lr.ph.i126
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_110:                              # %for.body.i135
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_120 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	24(%rax,%r14,8), %rax
	movzbl	108(%rax), %ecx
	testq	$1, %rcx
	jne	.LBB0_111
# BB#113:                               # %if.then.i140
                                        #   in Loop: Header=BB0_110 Depth=1
	movq	8(%rax), %r12
	movzbl	(%r12), %ecx
	movq	(%rax), %rbx
	cmpq	$33, %rcx
	jne	.LBB0_120
# BB#114:                               # %land.lhs.true.i143
                                        #   in Loop: Header=BB0_110 Depth=1
	movq	64(%r12), %rax
	testq	%rax, %rax
	je	.LBB0_120
# BB#115:                               # %land.lhs.true6.i145
                                        #   in Loop: Header=BB0_110 Depth=1
	movq	24(%rax), %rax
	testq	%rax, %rax
	je	.LBB0_120
# BB#116:                               # %land.lhs.true11.i146
                                        #   in Loop: Header=BB0_110 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB0_120
# BB#117:                               # %land.lhs.true15.i
                                        #   in Loop: Header=BB0_110 Depth=1
	movq	32(%rax), %rcx
	movl	(%rcx), %ecx
	andq	$65534, %rcx            # imm = 0xFFFE
	cmpq	$44, %rcx
	je	.LBB0_119
# BB#118:                               # %land.lhs.true15.i
                                        #   in Loop: Header=BB0_110 Depth=1
	movq	%r12, %rax
.LBB0_119:                              # %land.lhs.true15.i
                                        #   in Loop: Header=BB0_110 Depth=1
	movq	%rax, %r12
	.align	16, 0x90
.LBB0_120:                              # %while.body.i148
                                        #   Parent Loop BB0_110 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rbx), %eax
	movq	24(%rbx), %r13
	addq	$-32, %rax
	andq	%r15, %rax
	cmpq	$2, %rax
	ja	.LBB0_122
# BB#121:                               # %if.then44.i152
                                        #   in Loop: Header=BB0_120 Depth=2
	movslq	8(%rbx), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	df_analyzer(%rip), %rdi
	movq	32(%rcx,%rax,8), %rsi
	movq	%rbx, %rdx
	callq	df_insn_delete
.LBB0_122:                              # %if.end51.i
                                        #   in Loop: Header=BB0_120 Depth=2
	cmpq	%r12, %rbx
	movq	%r13, %rbx
	jne	.LBB0_120
.LBB0_111:                              # %for.cond.backedge.i
                                        #   in Loop: Header=BB0_110 Depth=1
	decq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB0_110
.LBB0_112:                              # %ssa_ccp_df_delete_unreachable_insns.exit
	movq	df_analyzer(%rip), %rdi
	callq	ssa_fast_dce
	movq	values(%rip), %rdi
	callq	free
	movq	edge_info(%rip), %rdi
	movq	$0, values(%rip)
	callq	free
	movq	executable_blocks(%rip), %rdi
	movq	$0, edge_info(%rip)
	callq	free
	movq	ssa_edges(%rip), %rdi
	movq	$0, executable_blocks(%rip)
	callq	free
	movq	edges(%rip), %rdi
	movq	$0, ssa_edges(%rip)
	callq	free_edge_list
	movq	executable_edges(%rip), %rdi
	movq	$0, edges(%rip)
	callq	free
	movq	df_analyzer(%rip), %rdi
	movq	$0, executable_edges(%rip)
	callq	df_finish
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	end_alias_analysis  # TAILCALL
.Ltmp16:
	.size	ssa_const_prop, .Ltmp16-ssa_const_prop
	.cfi_endproc

	.align	16, 0x90
	.type	ssa_fast_dce,@function
ssa_fast_dce:                           # @ssa_fast_dce
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp23:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp24:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp25:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp26:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp27:
	.cfi_def_cfa_offset 48
.Ltmp28:
	.cfi_offset %rbx, -48
.Ltmp29:
	.cfi_offset %r12, -40
.Ltmp30:
	.cfi_offset %r13, -32
.Ltmp31:
	.cfi_offset %r14, -24
.Ltmp32:
	.cfi_offset %r15, -16
	movq	ssa_definition(%rip), %rax
	movq	(%rax), %rax
	movq	%rdi, %r14
	movq	%rax, %rdi
	callq	sbitmap_alloc
	movq	%rax, %r12
	movq	%r12, %rdi
	callq	sbitmap_ones
	movq	%r12, %rdi
	callq	sbitmap_first_set_bit
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_8
# BB#1:                                 # %while.body.lr.ph
	movabsq	$4294967232, %r15       # imm = 0xFFFFFFC0
	.align	16, 0x90
.LBB1_2:                                # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_11 Depth 2
	movq	%r12, %rdi
	callq	sbitmap_first_set_bit
	movq	%rax, %rbx
	movq	$32, %rcx
                                        # kill: RAX<def> RBX<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rbx, %rdx
	andq	%r15, %rdx
	movq	$1, %rsi
	movq	%rbx, %rcx
	shlq	%cl, %rsi
	notq	%rsi
	movq	$3, %rcx
	shrq	%cl, %rdx
	andq	%rsi, 16(%r12,%rdx)
	cmpq	$53, %rax
	jl	.LBB1_7
# BB#3:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	$32, %rcx
	movq	ssa_definition(%rip), %rax
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	32(%rax,%rbx,8), %rax
	testq	%rax, %rax
	je	.LBB1_7
# BB#4:                                 # %lor.lhs.false8
                                        #   in Loop: Header=BB1_2 Depth=1
	movl	(%rax), %ecx
	testq	$134217728, %rcx        # imm = 0x8000000
	jne	.LBB1_7
# BB#5:                                 # %lor.lhs.false14
                                        #   in Loop: Header=BB1_2 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$37, %rcx
	jne	.LBB1_9
# BB#6:                                 # %land.lhs.true
                                        #   in Loop: Header=BB1_2 Depth=1
	movl	40(%rax), %ecx
	movabsq	$4294967197, %rdx       # imm = 0xFFFFFF9D
	cmpq	%rdx, %rcx
	je	.LBB1_7
.LBB1_9:                                # %lor.lhs.false30
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	32(%rax), %rdi
	callq	side_effects_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB1_7
# BB#10:                                # %if.end
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	40(%r14), %rax
	movq	$5, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	8(%rax,%rdx), %rax
	movq	ssa_definition(%rip), %rcx
	jmp	.LBB1_11
	.align	16, 0x90
.LBB1_18:                               # %for.inc
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	(%rax), %rax
.LBB1_11:                               # %if.end
                                        #   Parent Loop BB1_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rax, %rax
	je	.LBB1_19
# BB#12:                                # %for.body
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	8(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB1_18
# BB#13:                                # %land.lhs.true44
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	8(%rdx), %rdx
	testq	%rdx, %rdx
	je	.LBB1_18
# BB#14:                                # %land.lhs.true47
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	(%rdx), %esi
	testq	$134217728, %rsi        # imm = 0x8000000
	jne	.LBB1_18
# BB#15:                                # %land.lhs.true54
                                        #   in Loop: Header=BB1_11 Depth=2
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$37, %rsi
	jne	.LBB1_17
# BB#16:                                # %land.lhs.true61
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	40(%rdx), %esi
	movabsq	$4294967197, %rdi       # imm = 0xFFFFFF9D
	cmpq	%rdi, %rsi
	je	.LBB1_18
.LBB1_17:                               # %land.lhs.true69
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	32(%rcx,%rbx,8), %rsi
	cmpq	%rsi, %rdx
	je	.LBB1_18
	jmp	.LBB1_7
.LBB1_19:                               # %if.then81.critedge
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	32(%rcx,%rbx,8), %r13
	leaq	32(%r13), %rdi
	movabsq	$mark_references, %rsi
	movq	%r12, %rdx
	callq	for_each_rtx
	movslq	8(%r13), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rsi
	movq	%r14, %rdi
	movq	%r13, %rdx
	callq	df_insn_delete
	movq	ssa_definition(%rip), %rax
	movq	$0, 32(%rax,%rbx,8)
	.align	16, 0x90
.LBB1_7:                                # %while.cond.backedge
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	%r12, %rdi
	callq	sbitmap_first_set_bit
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB1_2
.LBB1_8:                                # %while.end
	movq	%r12, %rdi
	callq	free
	movq	df_analyzer(%rip), %rdi
	xorq	%rsi, %rsi
	movq	$1248, %rdx             # imm = 0x4E0
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	df_analyse  # TAILCALL
.Ltmp33:
	.size	ssa_fast_dce, .Ltmp33-ssa_fast_dce
	.cfi_endproc

	.align	16, 0x90
	.type	mark_references,@function
mark_references:                        # @mark_references
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp37:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp38:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp39:
	.cfi_def_cfa_offset 32
.Ltmp40:
	.cfi_offset %rbx, -24
.Ltmp41:
	.cfi_offset %r14, -16
	movq	(%rdi), %rdi
	movq	%rsi, %rdx
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB2_15
# BB#1:                                 # %if.end
	movzbl	(%rdi), %ecx
	cmpq	$61, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	je	.LBB2_13
# BB#2:                                 # %if.end
	cmpq	$49, %rcx
	je	.LBB2_15
# BB#3:                                 # %if.end
	cmpq	$47, %rcx
	jne	.LBB2_14
# BB#4:                                 # %if.then2
	movq	8(%rdi), %r8
	movl	(%r8), %r9d
	movq	%r9, %rbx
	andq	$65535, %rbx            # imm = 0xFFFF
	leaq	-63(%rbx), %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	cmpq	$2, %rax
	movq	%r9, %rax
	movq	%r8, %rcx
	jb	.LBB2_6
# BB#5:                                 # %if.then2
	addq	$-132, %rbx
	andq	%r14, %rbx
	cmpq	$1, %rbx
	movq	%r9, %rax
	movq	%r8, %rcx
	jbe	.LBB2_6
	jmp	.LBB2_11
	.align	16, 0x90
.LBB2_9:                                # %while.body
                                        #   in Loop: Header=BB2_6 Depth=1
	movq	8(%rcx), %rcx
	movl	(%rcx), %eax
.LBB2_6:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-63(%rax), %rsi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rsi
	cmpq	$2, %rsi
	jb	.LBB2_9
# BB#7:                                 # %while.cond
                                        #   in Loop: Header=BB2_6 Depth=1
	cmpq	$61, %rax
	je	.LBB2_10
# BB#8:                                 # %while.cond
                                        #   in Loop: Header=BB2_6 Depth=1
	addq	$-132, %rax
	andq	%rbx, %rax
	cmpq	$1, %rax
	jbe	.LBB2_9
	jmp	.LBB2_11
.LBB2_13:                               # %if.then58
	movl	8(%rdi), %ecx
	movq	$1, %rsi
	shlq	%cl, %rsi
	shrq	$6, %rcx
	orq	%rsi, 16(%rdx,%rcx,8)
	jmp	.LBB2_15
.LBB2_14:                               # %if.else78
	xorq	%rax, %rax
	jmp	.LBB2_15
.LBB2_10:                               # %if.then38
	movl	8(%rcx), %ecx
	movq	$1, %rax
	shlq	%cl, %rax
	shrq	$6, %rcx
	orq	%rax, 16(%rdx,%rcx,8)
	movl	(%r8), %r9d
.LBB2_11:                               # %if.end46
	andq	$65535, %r9             # imm = 0xFFFF
	cmpq	$61, %r9
	movabsq	$0, %rax
	jne	.LBB2_15
# BB#12:                                # %if.then50
	addq	$16, %rdi
	movabsq	$mark_references, %rsi
	callq	for_each_rtx
	movq	%r14, %rax
.LBB2_15:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp42:
	.size	mark_references, .Ltmp42-mark_references
	.cfi_endproc

	.align	16, 0x90
	.type	visit_phi_node,@function
visit_phi_node:                         # @visit_phi_node
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp49:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp50:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp51:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp52:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp53:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp54:
	.cfi_def_cfa_offset 80
.Ltmp55:
	.cfi_offset %rbx, -48
.Ltmp56:
	.cfi_offset %r12, -40
.Ltmp57:
	.cfi_offset %r13, -32
.Ltmp58:
	.cfi_offset %r14, -24
.Ltmp59:
	.cfi_offset %r15, -16
	movq	32(%rdi), %rax
	movq	16(%rax), %rcx
	movq	8(%rcx), %r13
	movq	8(%rax), %rax
	movl	(%r13), %ecx
	movl	8(%rax), %eax
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	je	.LBB3_1
# BB#2:
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	xorq	%r12, %r12
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	xorq	%rdx, %rdx
	movq	16(%rsp), %r14          # 8-byte Reload
	.align	16, 0x90
.LBB3_3:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%r12, %r15
	orq	$1, %r15
	andq	%rbx, %r15
	movq	8(%r13,%r15,8), %rax
	movq	8(%rax), %rax
	movq	basic_block_info(%rip), %rcx
	movq	edges(%rip), %rdi
	movq	32(%rcx,%rax,8), %rsi
	movq	%r14, %rdx
	callq	find_edge_index
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rax
	movq	8(%r13,%r15,8), %rdx
	movq	executable_edges(%rip), %rsi
	movq	$3, %rcx
	movq	8(%rdx), %rdx
	movq	basic_block_info(%rip), %rbx
	shrq	%cl, %rax
	movq	16(%rsi,%rax), %r15
	movq	edges(%rip), %rdi
	movq	32(%rbx,%rdx,8), %rsi
	movq	%r14, %rdx
	callq	find_edge_index
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	%r15, %rdx
	jne	.LBB3_5
# BB#4:                                 #   in Loop: Header=BB3_3 Depth=1
	movq	24(%rsp), %rdx          # 8-byte Reload
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB3_14
	.align	16, 0x90
.LBB3_5:                                # %if.then
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rdi
	andq	%rdi, %rax
	movq	8(%r13,%rax,8), %rax
	movl	8(%rax), %eax
	movq	values(%rip), %r8
	movq	$4, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movl	(%r8,%rsi), %esi
	cmpq	$2, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%rdi, %rbx
	je	.LBB3_6
# BB#7:                                 # %if.end
                                        #   in Loop: Header=BB3_3 Depth=1
	cmpq	$1, %rsi
	jne	.LBB3_10
# BB#8:                                 # %if.end
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	%rdx, %rcx
	andq	%rbx, %rcx
	cmpq	$1, %rcx
	jne	.LBB3_10
# BB#9:                                 # %land.lhs.true44
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	$4, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	8(%r8,%rdi), %rcx
	movq	8(%rsp), %rdi           # 8-byte Reload
	cmpq	%rdi, %rcx
	jne	.LBB3_6
.LBB3_10:                               # %if.end49
                                        #   in Loop: Header=BB3_3 Depth=1
	testq	%rbx, %rdx
	jne	.LBB3_14
# BB#11:                                # %if.end49
                                        #   in Loop: Header=BB3_3 Depth=1
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	jne	.LBB3_14
# BB#12:                                # %if.end49
                                        #   in Loop: Header=BB3_3 Depth=1
	cmpq	$1, %rsi
	jne	.LBB3_14
# BB#13:                                # %if.then55
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	8(%r8,%rax), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$1, %rdx
.LBB3_14:                               # %for.inc
                                        #   in Loop: Header=BB3_3 Depth=1
	addq	$2, %r12
	movq	%r12, %rax
	andq	%rbx, %rax
	movl	(%r13), %ecx
	cmpq	%rcx, %rax
	jb	.LBB3_3
	jmp	.LBB3_15
.LBB3_1:
	xorq	%rdx, %rdx
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	jmp	.LBB3_15
.LBB3_6:
	movq	$2, %rdx
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
.LBB3_15:                               # %for.end
	movq	values(%rip), %rax
	movq	$4, %rcx
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movl	(%rax,%rbx), %ecx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rdx, %rsi
	cmpq	%rcx, %rsi
	je	.LBB3_17
# BB#16:                                # %if.then66
	addq	%rax, %rbx
	movl	%edx, (%rbx)
	movq	$4, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 8(%rax,%rdx)
	movq	ssa_edges(%rip), %rax
	movq	$1, %rdx
	movq	%rdi, %rcx
	shlq	%cl, %rdx
	shrq	$6, %rdi
	orq	%rdx, 16(%rax,%rdi,8)
.LBB3_17:                               # %if.end79
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp60:
	.size	visit_phi_node, .Ltmp60-visit_phi_node
	.cfi_endproc

	.align	16, 0x90
	.type	visit_expression,@function
visit_expression:                       # @visit_expression
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp67:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp68:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp69:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp70:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp71:
	.cfi_def_cfa_offset 48
.Ltmp72:
	.cfi_offset %rbx, -48
.Ltmp73:
	.cfi_offset %r12, -40
.Ltmp74:
	.cfi_offset %r13, -32
.Ltmp75:
	.cfi_offset %r14, -24
.Ltmp76:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	(%r14), %eax
	movq	%rsi, %r13
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$34, %rcx
	jne	.LBB4_8
# BB#1:                                 # %land.lhs.true
	movq	8(%r13), %rcx
	cmpq	%r14, %rcx
	jne	.LBB4_8
# BB#2:                                 # %if.then
	movq	40(%r13), %rbx
	testq	%rbx, %rbx
	je	.LBB4_8
# BB#3:
	movabsq	$4294967232, %r15       # imm = 0xFFFFFFC0
	.align	16, 0x90
.LBB4_4:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	edges(%rip), %rdi
	movq	16(%rbx), %rsi
	movq	24(%rbx), %rdx
	callq	find_edge_index
	movq	%rax, %rdx
	movq	executable_edges(%rip), %r8
	andq	%r15, %rdx
	movq	$6, %rcx
	shrq	%cl, %rdx
	movq	16(%r8,%rdx,8), %rdi
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rdi, %rsi
	jne	.LBB4_6
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB4_4 Depth=1
	orq	%rdi, %rsi
	movq	%rsi, 16(%r8,%rdx,8)
	movq	flow_edges(%rip), %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	edge_info(%rip), %rsi
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, (%rsi,%rax,8)
	movq	%rbx, flow_edges(%rip)
.LBB4_6:                                # %for.inc
                                        #   in Loop: Header=BB4_4 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB4_4
# BB#7:                                 # %for.cond.if.end14.loopexit_crit_edge
	movl	(%r14), %eax
.LBB4_8:                                # %if.end14
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB4_12
# BB#9:                                 # %cond.true
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB4_11
# BB#10:                                # %cond.false
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB4_11:                               # %cond.end35
	testq	%rax, %rax
	je	.LBB4_12
# BB#24:                                # %if.end39
	movq	8(%rax), %r15
	movzbl	(%r15), %ecx
	movq	16(%rax), %r12
	cmpq	$61, %rcx
	je	.LBB4_38
# BB#25:                                # %if.end39
	movq	global_rtl(%rip), %rax
	cmpq	%rax, %r15
	je	.LBB4_38
# BB#26:                                # %if.then53
	movq	df_analyzer(%rip), %rax
	movslq	8(%r14), %rcx
	movq	56(%rax), %rax
	leaq	(%rcx,%rcx,2), %rcx
	movq	(%rax,%rcx,8), %rax
	testq	%rax, %rax
	je	.LBB4_237
# BB#27:
	movq	$2, %r8
	.align	16, 0x90
.LBB4_28:                               # %for.body.i1500
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rcx
	movq	(%rcx), %rsi
	movl	(%rsi), %ebx
	movq	%rbx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$63, %rdx
	movq	%rsi, %rcx
	jne	.LBB4_30
# BB#29:                                # %cond.true.i1503
                                        #   in Loop: Header=BB4_28 Depth=1
	movq	8(%rsi), %rcx
.LBB4_30:                               # %cond.end.i1510
                                        #   in Loop: Header=BB4_28 Depth=1
	movl	8(%rcx), %edi
	movq	values(%rip), %r9
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	(%r9,%rdi), %ecx
	cmpq	$2, %rcx
	je	.LBB4_35
# BB#31:                                # %if.then.i1512
                                        #   in Loop: Header=BB4_28 Depth=1
	leaq	8(%rsi), %rcx
	cmpq	$63, %rdx
	jne	.LBB4_32
# BB#33:                                # %cond.true37.i1518
                                        #   in Loop: Header=BB4_28 Depth=1
	movq	(%rcx), %rdx
	leaq	8(%rdx), %rcx
	jmp	.LBB4_34
	.align	16, 0x90
.LBB4_32:                               #   in Loop: Header=BB4_28 Depth=1
	movq	%rsi, %rdx
.LBB4_34:                               # %cond.end46.i1533
                                        #   in Loop: Header=BB4_28 Depth=1
	movl	(%rcx), %ecx
	movl	8(%rdx), %edx
	movq	ssa_edges(%rip), %rdi
	movq	$1, %rbx
	shlq	%cl, %rbx
	shrq	$6, %rdx
	orq	%rbx, 16(%rdi,%rdx,8)
	movl	(%rsi), %ebx
.LBB4_35:                               # %if.end.i1537
                                        #   in Loop: Header=BB4_28 Depth=1
	andq	$65535, %rbx            # imm = 0xFFFF
	cmpq	$63, %rbx
	jne	.LBB4_37
# BB#36:                                # %cond.true58.i1540
                                        #   in Loop: Header=BB4_28 Depth=1
	movq	8(%rsi), %rsi
.LBB4_37:                               # %cond.end67.i1549
                                        #   in Loop: Header=BB4_28 Depth=1
	movl	8(%rsi), %edx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	%r8d, (%r9,%rdx)
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB4_28
	jmp	.LBB4_237
.LBB4_12:                               # %if.then38
	movq	df_analyzer(%rip), %rax
	movslq	8(%r14), %rcx
	movq	56(%rax), %rax
	leaq	(%rcx,%rcx,2), %rcx
	movq	(%rax,%rcx,8), %rax
	testq	%rax, %rax
	je	.LBB4_237
# BB#13:
	movq	$2, %r8
	.align	16, 0x90
.LBB4_14:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rcx
	movq	(%rcx), %rsi
	movl	(%rsi), %ebx
	movq	%rbx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$63, %rdx
	movq	%rsi, %rcx
	jne	.LBB4_16
# BB#15:                                # %cond.true.i
                                        #   in Loop: Header=BB4_14 Depth=1
	movq	8(%rsi), %rcx
.LBB4_16:                               # %cond.end.i
                                        #   in Loop: Header=BB4_14 Depth=1
	movl	8(%rcx), %edi
	movq	values(%rip), %r9
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	(%r9,%rdi), %ecx
	cmpq	$2, %rcx
	je	.LBB4_21
# BB#17:                                # %if.then.i
                                        #   in Loop: Header=BB4_14 Depth=1
	leaq	8(%rsi), %rcx
	cmpq	$63, %rdx
	jne	.LBB4_18
# BB#19:                                # %cond.true37.i
                                        #   in Loop: Header=BB4_14 Depth=1
	movq	(%rcx), %rdx
	leaq	8(%rdx), %rcx
	jmp	.LBB4_20
	.align	16, 0x90
.LBB4_18:                               #   in Loop: Header=BB4_14 Depth=1
	movq	%rsi, %rdx
.LBB4_20:                               # %cond.end46.i
                                        #   in Loop: Header=BB4_14 Depth=1
	movl	(%rcx), %ecx
	movl	8(%rdx), %edx
	movq	ssa_edges(%rip), %rdi
	movq	$1, %rbx
	shlq	%cl, %rbx
	shrq	$6, %rdx
	orq	%rbx, 16(%rdi,%rdx,8)
	movl	(%rsi), %ebx
.LBB4_21:                               # %if.end.i
                                        #   in Loop: Header=BB4_14 Depth=1
	andq	$65535, %rbx            # imm = 0xFFFF
	cmpq	$63, %rbx
	jne	.LBB4_23
# BB#22:                                # %cond.true58.i
                                        #   in Loop: Header=BB4_14 Depth=1
	movq	8(%rsi), %rsi
.LBB4_23:                               # %cond.end67.i
                                        #   in Loop: Header=BB4_14 Depth=1
	movl	8(%rsi), %edx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	%r8d, (%r9,%rdx)
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB4_14
	jmp	.LBB4_237
.LBB4_38:                               # %if.end54
	cmpq	$61, %rcx
	jne	.LBB4_52
# BB#39:                                # %land.lhs.true59
	movl	8(%r15), %eax
	cmpq	$52, %rax
	ja	.LBB4_52
# BB#40:                                # %if.then64
	movq	df_analyzer(%rip), %rax
	movslq	8(%r14), %rcx
	movq	56(%rax), %rax
	leaq	(%rcx,%rcx,2), %rcx
	movq	(%rax,%rcx,8), %rax
	testq	%rax, %rax
	je	.LBB4_237
# BB#41:
	movq	$2, %r8
	.align	16, 0x90
.LBB4_42:                               # %for.body.i1436
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rcx
	movq	(%rcx), %rsi
	movl	(%rsi), %ebx
	movq	%rbx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$63, %rdx
	movq	%rsi, %rcx
	jne	.LBB4_44
# BB#43:                                # %cond.true.i1439
                                        #   in Loop: Header=BB4_42 Depth=1
	movq	8(%rsi), %rcx
.LBB4_44:                               # %cond.end.i1446
                                        #   in Loop: Header=BB4_42 Depth=1
	movl	8(%rcx), %edi
	movq	values(%rip), %r9
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	(%r9,%rdi), %ecx
	cmpq	$2, %rcx
	je	.LBB4_49
# BB#45:                                # %if.then.i1448
                                        #   in Loop: Header=BB4_42 Depth=1
	leaq	8(%rsi), %rcx
	cmpq	$63, %rdx
	jne	.LBB4_46
# BB#47:                                # %cond.true37.i1454
                                        #   in Loop: Header=BB4_42 Depth=1
	movq	(%rcx), %rdx
	leaq	8(%rdx), %rcx
	jmp	.LBB4_48
.LBB4_46:                               #   in Loop: Header=BB4_42 Depth=1
	movq	%rsi, %rdx
.LBB4_48:                               # %cond.end46.i1469
                                        #   in Loop: Header=BB4_42 Depth=1
	movl	(%rcx), %ecx
	movl	8(%rdx), %edx
	movq	ssa_edges(%rip), %rdi
	movq	$1, %rbx
	shlq	%cl, %rbx
	shrq	$6, %rdx
	orq	%rbx, 16(%rdi,%rdx,8)
	movl	(%rsi), %ebx
.LBB4_49:                               # %if.end.i1473
                                        #   in Loop: Header=BB4_42 Depth=1
	andq	$65535, %rbx            # imm = 0xFFFF
	cmpq	$63, %rbx
	jne	.LBB4_51
# BB#50:                                # %cond.true58.i1476
                                        #   in Loop: Header=BB4_42 Depth=1
	movq	8(%rsi), %rsi
.LBB4_51:                               # %cond.end67.i1485
                                        #   in Loop: Header=BB4_42 Depth=1
	movl	8(%rsi), %edx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	%r8d, (%r9,%rdx)
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB4_42
	jmp	.LBB4_237
.LBB4_52:                               # %if.end65
	movl	(%r12), %esi
	movq	%rsi, %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$61, %rdi
	jne	.LBB4_53
# BB#56:                                # %land.lhs.true101
	cmpq	$61, %rcx
	jne	.LBB4_62
# BB#57:                                # %if.then106
	movl	8(%r12), %ebx
	movq	values(%rip), %rdi
	movl	8(%r15), %eax
	movq	$4, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movl	(%rdi,%rdx), %r8d
	movq	$4, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movl	(%rdi,%rdx), %ecx
	leaq	(%rdi,%rdx), %r9
	cmpq	%r8, %rcx
	jne	.LBB4_58
# BB#59:                                # %lor.lhs.false
	movq	$4, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	8(%rdi,%rsi), %rdx
	movq	$4, %rcx
	shlq	%cl, %rbx
	movq	8(%rdi,%rbx), %rbx
	leaq	8(%rdi,%rsi), %rdi
	cmpq	%rbx, %rdx
	jne	.LBB4_60
	jmp	.LBB4_61
.LBB4_53:                               # %if.end65
	cmpq	$54, %rdi
	jne	.LBB4_62
# BB#54:                                # %land.lhs.true70
	movzbl	(%r14), %eax
	cmpq	$32, %rax
	jne	.LBB4_62
# BB#55:                                # %if.then75
	movl	8(%r15), %eax
	movq	values(%rip), %rdx
	movq	$4, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$1, %rdi
	movl	%edi, (%rdx,%rsi)
	movq	32(%r14), %rcx
	movq	16(%rcx), %rcx
	movq	%rcx, 8(%rdx,%rsi)
	movq	ssa_edges(%rip), %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdi
	shrq	$6, %rax
	orq	%rdi, 16(%rdx,%rax,8)
	jmp	.LBB4_237
.LBB4_62:                               # %if.else148
	movzbl	(%r14), %eax
	cmpq	$33, %rax
	jne	.LBB4_102
# BB#63:                                # %if.then153
	movq	%r14, %rdi
	callq	pc_set
	movzbl	(%r12), %eax
	cmpq	$72, %rax
	jne	.LBB4_64
# BB#69:                                # %if.else193
	movq	8(%r12), %rax
	movq	8(%rax), %rdx
	movl	(%rax), %r14d
	movzbl	(%rdx), %esi
	movq	16(%rax), %r8
	cmpq	$61, %rsi
	jne	.LBB4_71
# BB#70:                                # %land.lhs.true216
	movl	8(%rdx), %eax
	movq	values(%rip), %rdi
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	(%rdi,%rax), %eax
	testq	%rax, %rax
	je	.LBB4_237
.LBB4_71:                               # %lor.lhs.false225
	movzbl	(%r8), %eax
	cmpq	$61, %rax
	jne	.LBB4_73
# BB#72:                                # %land.lhs.true230
	movl	8(%r8), %edi
	movq	values(%rip), %rbx
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	(%rbx,%rdi), %ecx
	testq	%rcx, %rcx
	je	.LBB4_237
.LBB4_73:                               # %if.end240
	cmpq	$61, %rsi
	jne	.LBB4_75
# BB#74:                                # %land.lhs.true245
	movl	8(%rdx), %edi
	movq	values(%rip), %rbx
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	(%rbx,%rdi), %ecx
	cmpq	$2, %rcx
	je	.LBB4_77
.LBB4_75:                               # %lor.lhs.false254
	cmpq	$61, %rax
	jne	.LBB4_82
# BB#76:                                # %land.lhs.true259
	movl	8(%r8), %edi
	movq	values(%rip), %rbx
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	(%rbx,%rdi), %ecx
	cmpq	$2, %rcx
	jne	.LBB4_82
.LBB4_77:                               # %if.then268
	movq	40(%r13), %rbx
	testq	%rbx, %rbx
	je	.LBB4_237
# BB#78:
	movabsq	$4294967232, %r14       # imm = 0xFFFFFFC0
.LBB4_79:                               # %for.body272
                                        # =>This Inner Loop Header: Depth=1
	movq	edges(%rip), %rdi
	movq	16(%rbx), %rsi
	movq	24(%rbx), %rdx
	callq	find_edge_index
	movq	%rax, %rdx
	movq	executable_edges(%rip), %r8
	andq	%r14, %rdx
	movq	$6, %rcx
	shrq	%cl, %rdx
	movq	16(%r8,%rdx,8), %rdi
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rdi, %rsi
	jne	.LBB4_81
# BB#80:                                # %if.end287
                                        #   in Loop: Header=BB4_79 Depth=1
	orq	%rdi, %rsi
	movq	%rsi, 16(%r8,%rdx,8)
	movq	flow_edges(%rip), %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	edge_info(%rip), %rsi
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, (%rsi,%rax,8)
	movq	%rbx, flow_edges(%rip)
.LBB4_81:                               # %for.inc298
                                        #   in Loop: Header=BB4_79 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB4_79
	jmp	.LBB4_237
.LBB4_102:                              # %if.else414
	testq	%r14, %r14
	je	.LBB4_106
# BB#103:                               # %if.else414
	cmpq	$32, %rax
	jne	.LBB4_106
# BB#104:                               # %land.lhs.true421
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB4_106
# BB#105:                               # %land.lhs.true429
	movq	16(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$152, %rax
	je	.LBB4_237
.LBB4_106:                              # %if.then440
	movsbq	rtx_class(%rdi), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	cmpq	$97, %rax
	jg	.LBB4_124
# BB#107:                               # %if.then440
	cmpq	$59, %rax
	jg	.LBB4_137
# BB#108:                               # %if.then440
	andq	%rcx, %rax
	cmpq	$49, %rax
	je	.LBB4_163
# BB#109:                               # %if.then440
	cmpq	$50, %rax
	je	.LBB4_126
# BB#110:                               # %if.then440
	cmpq	$51, %rax
	jne	.LBB4_213
	jmp	.LBB4_111
.LBB4_64:                               # %if.then159
	movq	40(%r13), %rbx
	testq	%rbx, %rbx
	je	.LBB4_237
# BB#65:
	movabsq	$4294967232, %r14       # imm = 0xFFFFFFC0
	.align	16, 0x90
.LBB4_66:                               # %for.body164
                                        # =>This Inner Loop Header: Depth=1
	movq	edges(%rip), %rdi
	movq	16(%rbx), %rsi
	movq	24(%rbx), %rdx
	callq	find_edge_index
	movq	%rax, %rdx
	movq	executable_edges(%rip), %r8
	andq	%r14, %rdx
	movq	$6, %rcx
	shrq	%cl, %rdx
	movq	16(%r8,%rdx,8), %rdi
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rdi, %rsi
	jne	.LBB4_68
# BB#67:                                # %if.end179
                                        #   in Loop: Header=BB4_66 Depth=1
	orq	%rdi, %rsi
	movq	%rsi, 16(%r8,%rdx,8)
	movq	flow_edges(%rip), %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	edge_info(%rip), %rsi
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, (%rsi,%rax,8)
	movq	%rbx, flow_edges(%rip)
.LBB4_68:                               # %for.inc190
                                        #   in Loop: Header=BB4_66 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB4_66
	jmp	.LBB4_237
.LBB4_124:                              # %if.then440
	andq	%rcx, %rax
	cmpq	$98, %rax
	jne	.LBB4_125
.LBB4_111:                              # %sw.bb672
	movq	8(%r12), %r11
	movq	16(%r12), %r8
	movzbl	(%r11), %eax
	movq	24(%r12), %r9
	cmpq	$61, %rax
	jne	.LBB4_113
# BB#112:                               # %land.lhs.true689
	movl	8(%r11), %ebx
	movq	values(%rip), %r13
	movq	$4, %rcx
	shlq	%cl, %rbx
	movl	(%r13,%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB4_117
.LBB4_113:                              # %lor.lhs.false698
	movzbl	(%r8), %r12d
	cmpq	$61, %r12
	jne	.LBB4_115
# BB#114:                               # %land.lhs.true703
	movl	8(%r8), %ebx
	movq	values(%rip), %r13
	movq	$4, %rcx
	shlq	%cl, %rbx
	movl	(%r13,%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB4_117
.LBB4_115:                              # %lor.lhs.false712
	movzbl	(%r9), %r10d
	cmpq	$61, %r10
	jne	.LBB4_197
# BB#116:                               # %land.lhs.true717
	movl	8(%r9), %ebx
	movq	values(%rip), %r13
	movq	$4, %rcx
	shlq	%cl, %rbx
	movl	(%r13,%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB4_117
.LBB4_197:                              # %if.end727
	cmpq	$61, %rax
	jne	.LBB4_200
# BB#198:                               # %land.lhs.true732
	movl	8(%r11), %eax
	movq	values(%rip), %rdx
	movq	$4, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movl	(%rdx,%rbx), %ecx
	cmpq	$1, %rcx
	jne	.LBB4_200
# BB#199:                               # %if.then741
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	8(%rdx,%rax), %r11
.LBB4_200:                              # %if.end748
	cmpq	$61, %r12
	jne	.LBB4_203
# BB#201:                               # %land.lhs.true753
	movl	8(%r8), %eax
	movq	values(%rip), %rdx
	movq	$4, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movl	(%rdx,%rbx), %ecx
	cmpq	$1, %rcx
	jne	.LBB4_203
# BB#202:                               # %if.then762
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	8(%rdx,%rax), %r8
.LBB4_203:                              # %if.end769
	cmpq	$61, %r10
	jne	.LBB4_206
# BB#204:                               # %land.lhs.true774
	movl	8(%r9), %ebx
	movq	values(%rip), %rdx
	movq	$4, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movl	(%rdx,%rax), %eax
	cmpq	$1, %rax
	jne	.LBB4_206
# BB#205:                               # %if.then783
	movq	$4, %rcx
	shlq	%cl, %rbx
	movq	8(%rdx,%rbx), %r9
.LBB4_206:                              # %if.end790
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%rsi, %rdx
	movq	%r11, %rcx
	callq	simplify_ternary_operation
	jmp	.LBB4_207
.LBB4_58:                               # %if.then106.if.then129_crit_edge
	movq	$4, %rcx
	shlq	%cl, %rbx
	movq	8(%rdi,%rbx), %rbx
	movq	$4, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	leaq	8(%rdi,%rdx), %rdi
.LBB4_60:                               # %if.then129
	movq	ssa_edges(%rip), %rdx
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	shrq	$6, %rax
	orq	%rsi, 16(%rdx,%rax,8)
	movq	%rbx, %rdx
.LBB4_61:                               # %if.end138
	movl	%r8d, (%r9)
	movq	%rdx, (%rdi)
	jmp	.LBB4_237
.LBB4_137:                              # %if.then440
	andq	%rcx, %rax
	cmpq	$60, %rax
	jne	.LBB4_213
# BB#138:                               # %sw.bb
	movq	8(%r12), %rdx
	movl	(%rdx), %esi
	movq	16(%r12), %r8
	movq	%rsi, %r11
	andq	$65535, %r11            # imm = 0xFFFF
	cmpq	$61, %r11
	jne	.LBB4_140
# BB#139:                               # %land.lhs.true456
	movl	8(%rdx), %eax
	movq	values(%rip), %r12
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	(%r12,%rax), %eax
	testq	%rax, %rax
	je	.LBB4_142
.LBB4_140:                              # %lor.lhs.false465
	movl	(%r8), %r10d
	movq	%r10, %r9
	andq	$65535, %r9             # imm = 0xFFFF
	cmpq	$61, %r9
	jne	.LBB4_154
# BB#141:                               # %land.lhs.true470
	movl	8(%r8), %eax
	movq	values(%rip), %r12
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	(%r12,%rax), %eax
	testq	%rax, %rax
	je	.LBB4_142
.LBB4_154:                              # %if.end480
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	jne	.LBB4_156
# BB#155:                               # %if.then486
	movq	$16, %rcx
	shrq	%cl, %r10
	andq	$255, %r10
	movq	%r10, %rsi
.LBB4_156:                              # %if.end490
	cmpq	$61, %r11
	jne	.LBB4_159
# BB#157:                               # %land.lhs.true495
	movl	8(%rdx), %ebx
	movq	values(%rip), %r10
	movq	$4, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movl	(%r10,%rax), %eax
	cmpq	$1, %rax
	jne	.LBB4_159
# BB#158:                               # %if.then504
	movq	$4, %rcx
	shlq	%cl, %rbx
	movq	8(%r10,%rbx), %rdx
.LBB4_159:                              # %if.end511
	cmpq	$61, %r9
	jne	.LBB4_162
# BB#160:                               # %land.lhs.true516
	movl	8(%r8), %ebx
	movq	values(%rip), %r9
	movq	$4, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movl	(%r9,%rax), %eax
	cmpq	$1, %rax
	jne	.LBB4_162
# BB#161:                               # %if.then525
	movq	$4, %rcx
	shlq	%cl, %rbx
	movq	8(%r9,%rbx), %r8
.LBB4_162:                              # %if.end532
	movq	%r8, %rcx
	callq	simplify_relational_operation
	jmp	.LBB4_207
.LBB4_125:                              # %if.then440
	cmpq	$99, %rax
	jne	.LBB4_213
.LBB4_126:                              # %sw.bb586
	movq	8(%r12), %rdx
	movzbl	(%rdx), %r10d
	movq	16(%r12), %r8
	cmpq	$61, %r10
	jne	.LBB4_128
# BB#127:                               # %land.lhs.true599
	movl	8(%rdx), %eax
	movq	values(%rip), %r11
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	(%r11,%rax), %eax
	testq	%rax, %rax
	je	.LBB4_130
.LBB4_128:                              # %lor.lhs.false608
	movzbl	(%r8), %r9d
	cmpq	$61, %r9
	jne	.LBB4_185
# BB#129:                               # %land.lhs.true613
	movl	8(%r8), %eax
	movq	values(%rip), %r11
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	(%r11,%rax), %eax
	testq	%rax, %rax
	je	.LBB4_130
.LBB4_185:                              # %if.end623
	cmpq	$61, %r10
	jne	.LBB4_188
# BB#186:                               # %land.lhs.true628
	movl	8(%rdx), %ebx
	movq	values(%rip), %r10
	movq	$4, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movl	(%r10,%rax), %eax
	cmpq	$1, %rax
	jne	.LBB4_188
# BB#187:                               # %if.then637
	movq	$4, %rcx
	shlq	%cl, %rbx
	movq	8(%r10,%rbx), %rdx
.LBB4_188:                              # %if.end644
	cmpq	$61, %r9
	jne	.LBB4_191
# BB#189:                               # %land.lhs.true649
	movl	8(%r8), %ebx
	movq	values(%rip), %r9
	movq	$4, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movl	(%r9,%rax), %eax
	cmpq	$1, %rax
	jne	.LBB4_191
# BB#190:                               # %if.then658
	movq	$4, %rcx
	shlq	%cl, %rbx
	movq	8(%r9,%rbx), %r8
.LBB4_191:                              # %if.end665
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%r8, %rcx
	callq	simplify_binary_operation
	jmp	.LBB4_207
.LBB4_213:                              # %sw.default
	movq	df_analyzer(%rip), %rax
	movslq	8(%r14), %rcx
	movq	56(%rax), %rax
	leaq	(%rcx,%rcx,2), %rcx
	movq	(%rax,%rcx,8), %rax
	testq	%rax, %rax
	je	.LBB4_225
# BB#214:
	movq	$2, %r8
.LBB4_215:                              # %for.body.i1117
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rcx
	movq	(%rcx), %rsi
	movl	(%rsi), %ebx
	movq	%rbx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$63, %rdx
	movq	%rsi, %rcx
	jne	.LBB4_217
# BB#216:                               # %cond.true.i1120
                                        #   in Loop: Header=BB4_215 Depth=1
	movq	8(%rsi), %rcx
.LBB4_217:                              # %cond.end.i1127
                                        #   in Loop: Header=BB4_215 Depth=1
	movl	8(%rcx), %edi
	movq	values(%rip), %r9
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	(%r9,%rdi), %ecx
	cmpq	$2, %rcx
	je	.LBB4_222
# BB#218:                               # %if.then.i1129
                                        #   in Loop: Header=BB4_215 Depth=1
	leaq	8(%rsi), %rcx
	cmpq	$63, %rdx
	jne	.LBB4_219
# BB#220:                               # %cond.true37.i1135
                                        #   in Loop: Header=BB4_215 Depth=1
	movq	(%rcx), %rdx
	leaq	8(%rdx), %rcx
	jmp	.LBB4_221
.LBB4_219:                              #   in Loop: Header=BB4_215 Depth=1
	movq	%rsi, %rdx
.LBB4_221:                              # %cond.end46.i1150
                                        #   in Loop: Header=BB4_215 Depth=1
	movl	(%rcx), %ecx
	movl	8(%rdx), %edx
	movq	ssa_edges(%rip), %rdi
	movq	$1, %rbx
	shlq	%cl, %rbx
	shrq	$6, %rdx
	orq	%rbx, 16(%rdi,%rdx,8)
	movl	(%rsi), %ebx
.LBB4_222:                              # %if.end.i1154
                                        #   in Loop: Header=BB4_215 Depth=1
	andq	$65535, %rbx            # imm = 0xFFFF
	cmpq	$63, %rbx
	jne	.LBB4_224
# BB#223:                               # %cond.true58.i1157
                                        #   in Loop: Header=BB4_215 Depth=1
	movq	8(%rsi), %rsi
.LBB4_224:                              # %cond.end67.i1166
                                        #   in Loop: Header=BB4_215 Depth=1
	movl	8(%rsi), %edx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	%r8d, (%r9,%rdx)
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB4_215
	jmp	.LBB4_225
.LBB4_82:                               # %if.end301
	cmpq	$61, %rsi
	jne	.LBB4_85
# BB#83:                                # %land.lhs.true306
	movl	8(%rdx), %ebx
	movq	values(%rip), %rdi
	movq	$4, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movl	(%rdi,%rsi), %ecx
	cmpq	$1, %rcx
	jne	.LBB4_85
# BB#84:                                # %if.then315
	movq	$4, %rcx
	shlq	%cl, %rbx
	movq	8(%rdi,%rbx), %rdx
.LBB4_85:                               # %if.end322
	cmpq	$61, %rax
	jne	.LBB4_88
# BB#86:                                # %land.lhs.true327
	movl	8(%r8), %eax
	movq	values(%rip), %rsi
	movq	$4, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movl	(%rsi,%rdi), %ecx
	cmpq	$1, %rcx
	jne	.LBB4_88
# BB#87:                                # %if.then336
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	8(%rsi,%rax), %r8
.LBB4_88:                               # %if.end343
	movq	%r14, %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	movq	$16, %rcx
	shrq	%cl, %r14
	andq	$255, %r14
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r8, %rcx
	callq	gen_rtx
	movq	16(%r12), %r8
	movq	24(%r12), %r9
	movq	$72, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	simplify_ternary_operation
	movq	40(%r13), %rbx
	movq	%rax, %r14
	testq	%rbx, %rbx
	je	.LBB4_237
# BB#89:                                # %for.body366.lr.ph
	testq	%r14, %r14
	je	.LBB4_90
# BB#94:
	movabsq	$4294967232, %r15       # imm = 0xFFFFFFC0
.LBB4_95:                               # %for.body366
                                        # =>This Inner Loop Header: Depth=1
	movq	edges(%rip), %rdi
	movq	16(%rbx), %rsi
	movq	24(%rbx), %rdx
	callq	find_edge_index
	movq	%rax, %rdx
	movq	executable_edges(%rip), %r8
	andq	%r15, %rdx
	movq	$6, %rcx
	shrq	%cl, %rdx
	movq	16(%r8,%rdx,8), %rdi
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rdi, %rsi
	jne	.LBB4_101
# BB#96:                                # %lor.lhs.false383
                                        #   in Loop: Header=BB4_95 Depth=1
	movq	global_rtl(%rip), %rcx
	cmpq	%rcx, %r14
	jne	.LBB4_98
# BB#97:                                # %land.lhs.true386
                                        #   in Loop: Header=BB4_95 Depth=1
	movzbl	48(%rbx), %ecx
	testq	$1, %rcx
	jne	.LBB4_100
.LBB4_98:                               # %lor.lhs.false389
                                        #   in Loop: Header=BB4_95 Depth=1
	movzbl	(%r14), %ecx
	cmpq	$67, %rcx
	jne	.LBB4_101
# BB#99:                                # %land.lhs.true394
                                        #   in Loop: Header=BB4_95 Depth=1
	movzbl	48(%rbx), %ecx
	testq	$1, %rcx
	jne	.LBB4_101
.LBB4_100:                              # %if.then398
                                        #   in Loop: Header=BB4_95 Depth=1
	orq	%rsi, %rdi
	movq	%rdi, 16(%r8,%rdx,8)
	movq	flow_edges(%rip), %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	edge_info(%rip), %rsi
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, (%rsi,%rax,8)
	movq	%rbx, flow_edges(%rip)
.LBB4_101:                              # %for.inc410
                                        #   in Loop: Header=BB4_95 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB4_95
	jmp	.LBB4_237
.LBB4_117:                              # %if.then726
	movq	df_analyzer(%rip), %rax
	movslq	8(%r14), %rcx
	movq	56(%rax), %rax
	leaq	(%rcx,%rcx,2), %rcx
	movq	(%rax,%rcx,8), %rax
	testq	%rax, %rax
	je	.LBB4_225
# BB#118:
	xorq	%r8, %r8
.LBB4_119:                              # %for.body.i1181
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rcx
	movq	(%rcx), %rdi
	movl	(%rdi), %ebx
	movq	%rbx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$63, %rsi
	movq	%rdi, %rcx
	jne	.LBB4_121
# BB#120:                               # %cond.true.i1184
                                        #   in Loop: Header=BB4_119 Depth=1
	movq	8(%rdi), %rcx
.LBB4_121:                              # %cond.end.i1191
                                        #   in Loop: Header=BB4_119 Depth=1
	movl	8(%rcx), %edx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	(%r13,%rdx), %ecx
	testq	%rcx, %rcx
	je	.LBB4_194
# BB#122:                               # %if.then.i1193
                                        #   in Loop: Header=BB4_119 Depth=1
	leaq	8(%rdi), %rcx
	cmpq	$63, %rsi
	jne	.LBB4_123
# BB#192:                               # %cond.true37.i1199
                                        #   in Loop: Header=BB4_119 Depth=1
	movq	(%rcx), %rsi
	leaq	8(%rsi), %rcx
	jmp	.LBB4_193
.LBB4_123:                              #   in Loop: Header=BB4_119 Depth=1
	movq	%rdi, %rsi
.LBB4_193:                              # %cond.end46.i1214
                                        #   in Loop: Header=BB4_119 Depth=1
	movl	(%rcx), %ecx
	movl	8(%rsi), %edx
	movq	ssa_edges(%rip), %rsi
	movq	$1, %rbx
	shlq	%cl, %rbx
	shrq	$6, %rdx
	orq	%rbx, 16(%rsi,%rdx,8)
	movl	(%rdi), %ebx
.LBB4_194:                              # %if.end.i1218
                                        #   in Loop: Header=BB4_119 Depth=1
	andq	$65535, %rbx            # imm = 0xFFFF
	cmpq	$63, %rbx
	jne	.LBB4_196
# BB#195:                               # %cond.true58.i1221
                                        #   in Loop: Header=BB4_119 Depth=1
	movq	8(%rdi), %rdi
.LBB4_196:                              # %cond.end67.i1230
                                        #   in Loop: Header=BB4_119 Depth=1
	movl	8(%rdi), %edx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	%r8d, (%r13,%rdx)
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB4_119
	jmp	.LBB4_225
.LBB4_163:                              # %sw.bb536
	movq	8(%r12), %rdx
	movl	(%rdx), %eax
	movq	$16, %rcx
	movq	%rax, %rbx
	shrq	%cl, %rbx
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB4_179
# BB#164:                               # %land.lhs.true548
	movl	8(%rdx), %eax
	movq	values(%rip), %r9
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	(%r9,%rax), %eax
	testq	%rax, %rax
	je	.LBB4_165
# BB#177:                               # %land.lhs.true563
	movl	8(%rdx), %r9d
	movq	values(%rip), %r8
	movq	$4, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movl	(%r8,%rax), %eax
	cmpq	$1, %rax
	jne	.LBB4_179
# BB#178:                               # %if.then572
	movq	$4, %rcx
	shlq	%cl, %r9
	movq	8(%r8,%r9), %rdx
.LBB4_179:                              # %if.end579
	andq	$255, %rbx
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%rbx, %rcx
	callq	simplify_unary_operation
.LBB4_207:                              # %sw.epilog
	testq	%rax, %rax
	je	.LBB4_225
# BB#208:                               # %land.lhs.true801
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB4_225
# BB#209:                               # %if.then806
	movl	8(%r15), %ebx
	movq	values(%rip), %rdi
	movq	$4, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movl	(%rdi,%rdx), %ecx
	leaq	(%rdi,%rdx), %r8
	cmpq	$1, %rcx
	leaq	8(%rdi,%rdx), %rdi
	jne	.LBB4_211
# BB#210:                               # %lor.lhs.false815
	movq	(%rdi), %rcx
	cmpq	%rax, %rcx
	je	.LBB4_212
.LBB4_211:                              # %if.then824
	movq	ssa_edges(%rip), %rdx
	movq	$1, %rsi
	movq	%rbx, %rcx
	shlq	%cl, %rsi
	shrq	$6, %rbx
	orq	%rsi, 16(%rdx,%rbx,8)
.LBB4_212:                              # %if.end839
	movq	$1, %rcx
	movl	%ecx, (%r8)
	movq	%rax, (%rdi)
	jmp	.LBB4_237
.LBB4_130:                              # %if.then622
	movq	df_analyzer(%rip), %rax
	movslq	8(%r14), %rcx
	movq	56(%rax), %rax
	leaq	(%rcx,%rcx,2), %rcx
	movq	(%rax,%rcx,8), %rax
	testq	%rax, %rax
	je	.LBB4_225
# BB#131:
	xorq	%r8, %r8
.LBB4_132:                              # %for.body.i1244
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rcx
	movq	(%rcx), %rsi
	movl	(%rsi), %edi
	movq	%rdi, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$63, %rdx
	movq	%rsi, %rcx
	jne	.LBB4_134
# BB#133:                               # %cond.true.i1247
                                        #   in Loop: Header=BB4_132 Depth=1
	movq	8(%rsi), %rcx
.LBB4_134:                              # %cond.end.i1254
                                        #   in Loop: Header=BB4_132 Depth=1
	movl	8(%rcx), %ebx
	movq	$4, %rcx
	shlq	%cl, %rbx
	movl	(%r11,%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB4_182
# BB#135:                               # %if.then.i1256
                                        #   in Loop: Header=BB4_132 Depth=1
	leaq	8(%rsi), %rcx
	cmpq	$63, %rdx
	jne	.LBB4_136
# BB#180:                               # %cond.true37.i1262
                                        #   in Loop: Header=BB4_132 Depth=1
	movq	(%rcx), %rdx
	leaq	8(%rdx), %rcx
	jmp	.LBB4_181
.LBB4_136:                              #   in Loop: Header=BB4_132 Depth=1
	movq	%rsi, %rdx
.LBB4_181:                              # %cond.end46.i1277
                                        #   in Loop: Header=BB4_132 Depth=1
	movl	(%rcx), %ecx
	movl	8(%rdx), %edx
	movq	ssa_edges(%rip), %rdi
	movq	$1, %rbx
	shlq	%cl, %rbx
	shrq	$6, %rdx
	orq	%rbx, 16(%rdi,%rdx,8)
	movl	(%rsi), %edi
.LBB4_182:                              # %if.end.i1281
                                        #   in Loop: Header=BB4_132 Depth=1
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$63, %rdi
	jne	.LBB4_184
# BB#183:                               # %cond.true58.i1284
                                        #   in Loop: Header=BB4_132 Depth=1
	movq	8(%rsi), %rsi
.LBB4_184:                              # %cond.end67.i1293
                                        #   in Loop: Header=BB4_132 Depth=1
	movl	8(%rsi), %edx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	%r8d, (%r11,%rdx)
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB4_132
	jmp	.LBB4_225
.LBB4_90:
	movabsq	$4294967232, %r14       # imm = 0xFFFFFFC0
.LBB4_91:                               # %for.body366.us
                                        # =>This Inner Loop Header: Depth=1
	movq	edges(%rip), %rdi
	movq	16(%rbx), %rsi
	movq	24(%rbx), %rdx
	callq	find_edge_index
	movq	%rax, %rdx
	movq	executable_edges(%rip), %r8
	andq	%r14, %rdx
	movq	$6, %rcx
	shrq	%cl, %rdx
	movq	16(%r8,%rdx,8), %rdi
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rdi, %rsi
	jne	.LBB4_93
# BB#92:                                # %if.then398.us
                                        #   in Loop: Header=BB4_91 Depth=1
	orq	%rsi, %rdi
	movq	%rdi, 16(%r8,%rdx,8)
	movq	flow_edges(%rip), %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	edge_info(%rip), %rsi
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, (%rsi,%rax,8)
	movq	%rbx, flow_edges(%rip)
.LBB4_93:                               # %for.inc410.us
                                        #   in Loop: Header=BB4_91 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB4_91
	jmp	.LBB4_237
.LBB4_142:                              # %if.then479
	movq	df_analyzer(%rip), %rax
	movslq	8(%r14), %rcx
	movq	56(%rax), %rax
	leaq	(%rcx,%rcx,2), %rcx
	movq	(%rax,%rcx,8), %rax
	testq	%rax, %rax
	je	.LBB4_225
# BB#143:
	xorq	%r8, %r8
.LBB4_144:                              # %for.body.i1372
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rcx
	movq	(%rcx), %rsi
	movl	(%rsi), %edi
	movq	%rdi, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$63, %rdx
	movq	%rsi, %rcx
	jne	.LBB4_146
# BB#145:                               # %cond.true.i1375
                                        #   in Loop: Header=BB4_144 Depth=1
	movq	8(%rsi), %rcx
.LBB4_146:                              # %cond.end.i1382
                                        #   in Loop: Header=BB4_144 Depth=1
	movl	8(%rcx), %ebx
	movq	$4, %rcx
	shlq	%cl, %rbx
	movl	(%r12,%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB4_151
# BB#147:                               # %if.then.i1384
                                        #   in Loop: Header=BB4_144 Depth=1
	leaq	8(%rsi), %rcx
	cmpq	$63, %rdx
	jne	.LBB4_148
# BB#149:                               # %cond.true37.i1390
                                        #   in Loop: Header=BB4_144 Depth=1
	movq	(%rcx), %rdx
	leaq	8(%rdx), %rcx
	jmp	.LBB4_150
.LBB4_148:                              #   in Loop: Header=BB4_144 Depth=1
	movq	%rsi, %rdx
.LBB4_150:                              # %cond.end46.i1405
                                        #   in Loop: Header=BB4_144 Depth=1
	movl	(%rcx), %ecx
	movl	8(%rdx), %edx
	movq	ssa_edges(%rip), %rdi
	movq	$1, %rbx
	shlq	%cl, %rbx
	shrq	$6, %rdx
	orq	%rbx, 16(%rdi,%rdx,8)
	movl	(%rsi), %edi
.LBB4_151:                              # %if.end.i1409
                                        #   in Loop: Header=BB4_144 Depth=1
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$63, %rdi
	jne	.LBB4_153
# BB#152:                               # %cond.true58.i1412
                                        #   in Loop: Header=BB4_144 Depth=1
	movq	8(%rsi), %rsi
.LBB4_153:                              # %cond.end67.i1421
                                        #   in Loop: Header=BB4_144 Depth=1
	movl	8(%rsi), %edx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	%r8d, (%r12,%rdx)
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB4_144
	jmp	.LBB4_225
.LBB4_165:                              # %if.then557
	movq	df_analyzer(%rip), %rax
	movslq	8(%r14), %rcx
	movq	56(%rax), %rax
	leaq	(%rcx,%rcx,2), %rcx
	movq	(%rax,%rcx,8), %rax
	testq	%rax, %rax
	je	.LBB4_225
# BB#166:
	xorq	%r8, %r8
.LBB4_167:                              # %for.body.i1308
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rcx
	movq	(%rcx), %rsi
	movl	(%rsi), %edi
	movq	%rdi, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$63, %rdx
	movq	%rsi, %rcx
	jne	.LBB4_169
# BB#168:                               # %cond.true.i1311
                                        #   in Loop: Header=BB4_167 Depth=1
	movq	8(%rsi), %rcx
.LBB4_169:                              # %cond.end.i1318
                                        #   in Loop: Header=BB4_167 Depth=1
	movl	8(%rcx), %ebx
	movq	$4, %rcx
	shlq	%cl, %rbx
	movl	(%r9,%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB4_174
# BB#170:                               # %if.then.i1320
                                        #   in Loop: Header=BB4_167 Depth=1
	leaq	8(%rsi), %rcx
	cmpq	$63, %rdx
	jne	.LBB4_171
# BB#172:                               # %cond.true37.i1326
                                        #   in Loop: Header=BB4_167 Depth=1
	movq	(%rcx), %rdx
	leaq	8(%rdx), %rcx
	jmp	.LBB4_173
.LBB4_171:                              #   in Loop: Header=BB4_167 Depth=1
	movq	%rsi, %rdx
.LBB4_173:                              # %cond.end46.i1341
                                        #   in Loop: Header=BB4_167 Depth=1
	movl	(%rcx), %ecx
	movl	8(%rdx), %edx
	movq	ssa_edges(%rip), %rdi
	movq	$1, %rbx
	shlq	%cl, %rbx
	shrq	$6, %rdx
	orq	%rbx, 16(%rdi,%rdx,8)
	movl	(%rsi), %edi
.LBB4_174:                              # %if.end.i1345
                                        #   in Loop: Header=BB4_167 Depth=1
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$63, %rdi
	jne	.LBB4_176
# BB#175:                               # %cond.true58.i1348
                                        #   in Loop: Header=BB4_167 Depth=1
	movq	8(%rsi), %rsi
.LBB4_176:                              # %cond.end67.i1357
                                        #   in Loop: Header=BB4_167 Depth=1
	movl	8(%rsi), %edx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	%r8d, (%r9,%rdx)
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB4_167
.LBB4_225:                              # %if.else852
	movq	df_analyzer(%rip), %rax
	movslq	8(%r14), %rcx
	movq	56(%rax), %rax
	leaq	(%rcx,%rcx,2), %rcx
	movq	(%rax,%rcx,8), %rax
	testq	%rax, %rax
	je	.LBB4_237
# BB#226:
	movq	$2, %r8
	.align	16, 0x90
.LBB4_227:                              # %for.body.i1053
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rcx
	movq	(%rcx), %rsi
	movl	(%rsi), %ebx
	movq	%rbx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$63, %rdx
	movq	%rsi, %rcx
	jne	.LBB4_229
# BB#228:                               # %cond.true.i1056
                                        #   in Loop: Header=BB4_227 Depth=1
	movq	8(%rsi), %rcx
.LBB4_229:                              # %cond.end.i1063
                                        #   in Loop: Header=BB4_227 Depth=1
	movl	8(%rcx), %edi
	movq	values(%rip), %r9
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	(%r9,%rdi), %ecx
	cmpq	$2, %rcx
	je	.LBB4_234
# BB#230:                               # %if.then.i1065
                                        #   in Loop: Header=BB4_227 Depth=1
	leaq	8(%rsi), %rcx
	cmpq	$63, %rdx
	jne	.LBB4_231
# BB#232:                               # %cond.true37.i1071
                                        #   in Loop: Header=BB4_227 Depth=1
	movq	(%rcx), %rdx
	leaq	8(%rdx), %rcx
	jmp	.LBB4_233
.LBB4_231:                              #   in Loop: Header=BB4_227 Depth=1
	movq	%rsi, %rdx
.LBB4_233:                              # %cond.end46.i1086
                                        #   in Loop: Header=BB4_227 Depth=1
	movl	(%rcx), %ecx
	movl	8(%rdx), %edx
	movq	ssa_edges(%rip), %rdi
	movq	$1, %rbx
	shlq	%cl, %rbx
	shrq	$6, %rdx
	orq	%rbx, 16(%rdi,%rdx,8)
	movl	(%rsi), %ebx
.LBB4_234:                              # %if.end.i1090
                                        #   in Loop: Header=BB4_227 Depth=1
	andq	$65535, %rbx            # imm = 0xFFFF
	cmpq	$63, %rbx
	jne	.LBB4_236
# BB#235:                               # %cond.true58.i1093
                                        #   in Loop: Header=BB4_227 Depth=1
	movq	8(%rsi), %rsi
.LBB4_236:                              # %cond.end67.i1102
                                        #   in Loop: Header=BB4_227 Depth=1
	movl	8(%rsi), %edx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	%r8d, (%r9,%rdx)
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB4_227
.LBB4_237:                              # %if.end857
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp77:
	.size	visit_expression, .Ltmp77-visit_expression
	.cfi_endproc

	.type	df_analyzer,@object     # @df_analyzer
	.local	df_analyzer
	.comm	df_analyzer,8,8
	.type	edges,@object           # @edges
	.local	edges
	.comm	edges,8,8
	.type	values,@object          # @values
	.local	values
	.comm	values,8,8
	.type	ssa_edges,@object       # @ssa_edges
	.local	ssa_edges
	.comm	ssa_edges,8,8
	.type	executable_blocks,@object # @executable_blocks
	.local	executable_blocks
	.comm	executable_blocks,8,8
	.type	executable_edges,@object # @executable_edges
	.local	executable_edges
	.comm	executable_edges,8,8
	.type	edge_info,@object       # @edge_info
	.local	edge_info
	.comm	edge_info,8,8
	.type	flow_edges,@object      # @flow_edges
	.local	flow_edges
	.comm	flow_edges,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Removing alternative for bb %d of phi %d\n"
	.size	.L.str, 42

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Removing unexecutable edge from %d to %d\n"
	.size	.L.str1, 42

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Register %d is now set to a constant\n"
	.size	.L.str2, 38

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"Register %d in insn %d replaced with constant\n"
	.size	.L.str3, 47


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
