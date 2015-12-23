	.file	"local-alloc.c.bc"
	.text
	.globl	local_alloc
	.align	16, 0x90
	.type	local_alloc,@function
local_alloc:                            # @local_alloc
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
	subq	$128, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 176
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
	xorq	%r12, %r12
	movb	%r12b, recorded_label_ref(%rip)
	callq	x86_order_regs_for_local_alloc
	movslq	max_regno(%rip), %rdi
	movq	$40, %rsi
	callq	xcalloc
	movq	%rax, reg_equiv(%rip)
	leaq	104(%rsp), %rdi
	callq	bitmap_initialize
	callq	init_alias_analysis
	movslq	n_basic_blocks(%rip), %rsi
	testq	%rsi, %rsi
	jle	.LBB0_134
# BB#1:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_2:                                # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_4 Depth 2
                                        #       Child Loop BB0_6 Depth 3
                                        #         Child Loop BB0_10 Depth 4
                                        #       Child Loop BB0_26 Depth 3
                                        #       Child Loop BB0_41 Depth 3
                                        #       Child Loop BB0_72 Depth 3
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r12,8), %rbx
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	movq	8(%rbx), %rax
	movq	(%rbx), %r14
	movq	24(%rax), %rax
	cmpq	%rax, %r14
	je	.LBB0_98
# BB#3:                                 #   in Loop: Header=BB0_2 Depth=1
	movq	%r12, 88(%rsp)          # 8-byte Spill
	movl	92(%rbx), %eax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	jmp	.LBB0_4
.LBB0_59:                               # %if.end225.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB0_62
# BB#60:                                # %land.lhs.true228.i
                                        #   in Loop: Header=BB0_4 Depth=2
	xorq	%rsi, %rsi
	xorq	%r13, %r13
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	rtx_varies_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_64
# BB#61:                                # %if.end233.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	$4, %rsi
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	set_unique_reg_note
	testq	%rax, %rax
	je	.LBB0_64
.LBB0_62:                               # %land.lhs.true235.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	8(%rax), %rcx
	movzbl	(%rcx), %ecx
	xorq	%r13, %r13
	cmpq	$3, %rcx
	je	.LBB0_64
# BB#63:                                # %land.lhs.true235.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	%rax, %r13
.LBB0_64:                               # %if.end244.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r15,8), %rax
	movl	12(%rax), %eax
	cmpq	$1, %rax
	je	.LBB0_75
# BB#65:                                # %land.lhs.true252.i
                                        #   in Loop: Header=BB0_4 Depth=2
	testq	%r13, %r13
	je	.LBB0_69
# BB#66:                                # %lor.lhs.false254.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	8(%r13), %rdi
	xorq	%rsi, %rsi
	callq	rtx_varies_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_69
# BB#67:                                # %lor.lhs.false260.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	reg_equiv(%rip), %rax
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	8(%rax,%rcx,8), %rsi
	testq	%rsi, %rsi
	je	.LBB0_75
# BB#68:                                # %land.lhs.true264.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	8(%r13), %rdi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_75
.LBB0_69:                               # %if.then273.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movzbl	(%r12), %eax
	cmpq	$61, %rax
	movq	96(%rsp), %rbx          # 8-byte Reload
	jne	.LBB0_96
# BB#70:                                # %if.end.i1020.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movslq	8(%r12), %r12
	movq	reg_equiv(%rip), %rax
	leaq	(%r12,%r12,4), %rcx
	movq	32(%rax,%rcx,8), %r13
	movq	const_int_rtx+512(%rip), %rdx
	cmpq	%rdx, %r13
	je	.LBB0_96
# BB#71:                                # %for.cond.preheader.i1022.i
                                        #   in Loop: Header=BB0_4 Depth=2
	testq	%r13, %r13
	je	.LBB0_74
	.align	16, 0x90
.LBB0_72:                               # %for.body.i1030.i
                                        #   Parent Loop BB0_2 Depth=1
                                        #     Parent Loop BB0_4 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%r13), %r15
	movq	$3, %rsi
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	callq	find_reg_note
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	remove_note
	movq	16(%r13), %r13
	testq	%r13, %r13
	jne	.LBB0_72
# BB#73:                                # %for.cond.for.end_crit_edge.i1033.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	const_int_rtx+512(%rip), %rdx
	movq	reg_equiv(%rip), %rax
.LBB0_74:                               # %for.end.i1036.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	$3, %rcx
	shlq	%cl, %r12
	leaq	(%r12,%r12,4), %rcx
	movq	%rdx, 32(%rax,%rcx)
	movq	reg_equiv(%rip), %rax
	movq	$0, 8(%rax,%rcx)
	jmp	.LBB0_96
.LBB0_75:                               # %if.end274.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	reg_equiv(%rip), %rax
	movq	$3, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rbx
	movq	32(%rax,%rbx), %rcx
	movq	$4, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_ue
	movq	reg_equiv(%rip), %rcx
	movq	%rax, 32(%rcx,%rbx)
	testq	%r13, %r13
	je	.LBB0_78
# BB#76:                                # %land.lhs.true283.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	8(%r13), %rdi
	xorq	%rsi, %rsi
	callq	rtx_varies_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_78
# BB#77:                                # %if.then289.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	$3, %rax
	movb	%al, 2(%r13)
.LBB0_78:                               # %if.end292.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	$3, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	movq	96(%rsp), %rbx          # 8-byte Reload
	jne	.LBB0_83
# BB#79:                                # %land.lhs.true296.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r15,8), %rax
	movslq	36(%rax), %rax
	testq	%rax, %rax
	js	.LBB0_96
# BB#80:                                # %land.lhs.true304.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdx
	movzbl	(%rdx), %eax
	cmpq	$66, %rax
	jne	.LBB0_96
# BB#81:                                # %land.lhs.true312.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	validate_equiv_mem
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_96
# BB#82:                                # %if.end329.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdx
	movq	56(%r14), %rcx
	movq	$3, %rdi
	movq	$3, %rsi
	callq	gen_rtx_fmt_ee
	testq	%rax, %rax
	movq	%rax, 56(%r14)
	je	.LBB0_96
.LBB0_83:                               # %if.then331.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	8(%rax), %rdx
	movzbl	(%rdx), %ecx
	movslq	8(%r12), %r15
	cmpq	$67, %rcx
	je	.LBB0_87
# BB#84:                                # %if.then331.i
                                        #   in Loop: Header=BB0_4 Depth=2
	cmpq	$58, %rcx
	jne	.LBB0_88
# BB#85:                                # %land.lhs.true351.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	8(%rdx), %rcx
	movzbl	(%rcx), %esi
	cmpq	$75, %rsi
	jne	.LBB0_88
# BB#86:                                # %land.lhs.true362.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	8(%rcx), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$67, %rcx
	jne	.LBB0_88
.LBB0_87:                               # %if.then376.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	$1, %rcx
	movb	%cl, recorded_label_ref(%rip)
.LBB0_88:                               # %if.end377.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	reg_equiv(%rip), %rsi
	movq	$3, %rcx
	movq	%r15, %rdi
	shlq	%cl, %rdi
	leaq	(%rdi,%rdi,4), %rcx
	movq	%rdx, 8(%rsi,%rcx)
	movq	reg_equiv(%rip), %rdx
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, 16(%rdx,%rcx)
	movq	reg_equiv(%rip), %rdx
	movq	80(%rsp), %rsi          # 8-byte Reload
	movl	%esi, 24(%rdx,%rcx)
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%r15,8), %rcx
	movslq	28(%rcx), %rdx
	testq	%rdx, %rdx
	js	.LBB0_96
# BB#89:                                # %if.end377.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movl	optimize(%rip), %esi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	testq	%rdi, %rsi
	je	.LBB0_96
# BB#90:                                # %if.then400.i
                                        #   in Loop: Header=BB0_4 Depth=2
	addq	%rdx, %rdx
	movl	%edx, 28(%rcx)
	movl	16(%rcx), %ecx
	cmpq	$2, %rcx
	jne	.LBB0_96
# BB#91:                                # %land.lhs.true412.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	8(%rax), %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_93
# BB#92:                                # %lor.lhs.false418.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	equiv_init_varies_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_96
.LBB0_93:                               # %land.lhs.true421.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movzbl	(%r14), %eax
	cmpq	$32, %rax
	jne	.LBB0_96
# BB#94:                                # %land.lhs.true426.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	32(%r14), %rdi
	movq	%r15, %rsi
	callq	equiv_init_movable_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_96
# BB#95:                                # %if.then432.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	reg_equiv(%rip), %rax
	leaq	(%r15,%r15,4), %rcx
	movq	$1, %rdx
	movb	%dl, (%rax,%rcx,8)
	jmp	.LBB0_96
	.align	16, 0x90
.LBB0_4:                                # %for.body9.i
                                        #   Parent Loop BB0_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_6 Depth 3
                                        #         Child Loop BB0_10 Depth 4
                                        #       Child Loop BB0_26 Depth 3
                                        #       Child Loop BB0_41 Depth 3
                                        #       Child Loop BB0_72 Depth 3
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB0_96
# BB#5:                                 # %if.end.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	56(%r14), %r12
	testq	%r12, %r12
	je	.LBB0_15
	.align	16, 0x90
.LBB0_6:                                # %for.body19.i
                                        #   Parent Loop BB0_2 Depth=1
                                        #     Parent Loop BB0_4 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_10 Depth 4
	movl	(%r12), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$131072, %rax           # imm = 0x20000
	jne	.LBB0_13
# BB#7:                                 # %if.then24.i
                                        #   in Loop: Header=BB0_6 Depth=3
	movq	8(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB0_13
# BB#8:                                 # %if.end.i.i
                                        #   in Loop: Header=BB0_6 Depth=3
	movslq	8(%rax), %r13
	movq	reg_equiv(%rip), %rax
	leaq	(%r13,%r13,4), %rcx
	movq	32(%rax,%rcx,8), %r15
	movq	const_int_rtx+512(%rip), %rdx
	cmpq	%rdx, %r15
	je	.LBB0_13
# BB#9:                                 # %for.cond.preheader.i.i
                                        #   in Loop: Header=BB0_6 Depth=3
	testq	%r15, %r15
	je	.LBB0_12
	.align	16, 0x90
.LBB0_10:                               # %for.body.i.i
                                        #   Parent Loop BB0_2 Depth=1
                                        #     Parent Loop BB0_4 Depth=2
                                        #       Parent Loop BB0_6 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%r15), %rbx
	movq	$3, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	remove_note
	movq	16(%r15), %r15
	testq	%r15, %r15
	jne	.LBB0_10
# BB#11:                                # %for.cond.for.end_crit_edge.i.i
                                        #   in Loop: Header=BB0_6 Depth=3
	movq	const_int_rtx+512(%rip), %rdx
	movq	reg_equiv(%rip), %rax
.LBB0_12:                               # %for.end.i.i
                                        #   in Loop: Header=BB0_6 Depth=3
	movq	$3, %rcx
	shlq	%cl, %r13
	leaq	(%r13,%r13,4), %rcx
	movq	%rdx, 32(%rax,%rcx)
	movq	reg_equiv(%rip), %rax
	movq	$0, 8(%rax,%rcx)
	.align	16, 0x90
.LBB0_13:                               # %for.inc.i
                                        #   in Loop: Header=BB0_6 Depth=3
	movq	16(%r12), %r12
	testq	%r12, %r12
	jne	.LBB0_6
# BB#14:                                # %for.cond18.for.end_crit_edge.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movl	(%r14), %eax
.LBB0_15:                               # %for.end.i
                                        #   in Loop: Header=BB0_4 Depth=2
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	movq	32(%r14), %rcx
	cmpq	$105, %rax
	jne	.LBB0_22
# BB#16:                                # %cond.true.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movzbl	(%rcx), %eax
	cmpq	$47, %rax
	jne	.LBB0_20
# BB#17:                                #   in Loop: Header=BB0_4 Depth=2
	movq	%rcx, %r13
	jmp	.LBB0_21
.LBB0_20:                               # %cond.false.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	%r14, %rdi
	movq	%rcx, %rsi
	callq	single_set_2
	movq	32(%r14), %rcx
	movq	%rax, %r13
.LBB0_21:                               # %cond.end55.i
                                        #   in Loop: Header=BB0_4 Depth=2
	testq	%r13, %r13
	jne	.LBB0_24
.LBB0_22:                               # %if.then59.i
                                        #   in Loop: Header=BB0_4 Depth=2
	xorq	%rdx, %rdx
	movq	%rcx, %rdi
	jmp	.LBB0_23
.LBB0_24:                               # %if.else.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movzbl	(%rcx), %eax
	cmpq	$39, %rax
	jne	.LBB0_30
# BB#25:                                # %if.then70.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	8(%rcx), %rax
	movslq	(%rax), %rbx
	testq	%rbx, %rbx
	jle	.LBB0_30
	jmp	.LBB0_26
	.align	16, 0x90
.LBB0_28:                               # %for.cond76.backedge.for.body79_crit_edge.i
                                        #   in Loop: Header=BB0_26 Depth=3
	movq	32(%r14), %rax
	movq	8(%rax), %rax
.LBB0_26:                               # %for.body79.lr.ph.i
                                        #   Parent Loop BB0_2 Depth=1
                                        #     Parent Loop BB0_4 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	decq	%rbx
	movq	8(%rax,%rbx,8), %rdi
	cmpq	%r13, %rdi
	je	.LBB0_27
# BB#29:                                # %if.then90.i
                                        #   in Loop: Header=BB0_26 Depth=3
	xorq	%rdx, %rdx
	movabsq	$no_equiv, %rsi
	callq	note_stores
.LBB0_27:                               # %for.cond76.backedge.i
                                        #   in Loop: Header=BB0_26 Depth=3
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB0_28
.LBB0_30:                               # %if.end95.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	8(%r13), %r12
	movzbl	(%r12), %eax
	movq	16(%r13), %rsi
	cmpq	$66, %rax
	jne	.LBB0_47
# BB#31:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	jne	.LBB0_47
# BB#32:                                # %land.lhs.true110.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movslq	8(%rsi), %rax
	cmpq	$53, %rax
	jl	.LBB0_47
# BB#33:                                # %land.lhs.true115.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rcx
	movslq	36(%rcx), %rdx
	testq	%rdx, %rdx
	js	.LBB0_47
# BB#34:                                # %land.lhs.true121.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movl	12(%rcx), %ecx
	cmpq	$1, %rcx
	jne	.LBB0_47
# BB#35:                                # %land.lhs.true128.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	reg_equiv(%rip), %rcx
	leaq	(%rax,%rax,4), %rbx
	movq	32(%rcx,%rbx,8), %rax
	testq	%rax, %rax
	je	.LBB0_47
# BB#36:                                # %land.lhs.true128.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB0_47
# BB#37:                                # %land.lhs.true139.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	8(%rax), %rdi
	movq	%rsi, %r15
	movq	$3, %rsi
	xorq	%rdx, %rdx
	callq	find_reg_note
	movq	%r15, %rsi
	testq	%rax, %rax
	jne	.LBB0_47
# BB#38:                                # %land.lhs.true148.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	8(%r12), %rdi
	movq	%rsi, %r15
	callq	contains_replace_regs
	movq	%r15, %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_47
# BB#39:                                # %if.then154.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	reg_equiv(%rip), %rax
	movq	32(%rax,%rbx,8), %rax
	movq	8(%rax), %rdi
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	movq	%rsi, %rbx
                                        # kill: RSI<def> RBX<kill>
	movq	%r12, %rdx
	callq	validate_equiv_mem
	movq	%rbx, %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_47
# BB#40:                                # %land.lhs.true163.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	%rsi, %r15
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	24(%rax), %rbx
	movq	24(%r14), %rax
	jmp	.LBB0_41
.LBB0_45:                               # %for.cond.backedge.i.i
                                        #   in Loop: Header=BB0_41 Depth=3
	movq	24(%rbx), %rbx
.LBB0_41:                               # %land.lhs.true163.i
                                        #   Parent Loop BB0_2 Depth=1
                                        #     Parent Loop BB0_4 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	cmpq	%rax, %rbx
	je	.LBB0_46
# BB#42:                                # %for.body.i1005.i
                                        #   in Loop: Header=BB0_41 Depth=3
	movzbl	(%rbx), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB0_45
# BB#43:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB0_41 Depth=3
	movq	32(%rbx), %rsi
	movq	%r12, %rdi
	callq	memref_referenced_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%r15, %rsi
	jne	.LBB0_47
# BB#44:                                # %land.lhs.true.for.cond.backedge_crit_edge.i.i
                                        #   in Loop: Header=BB0_41 Depth=3
	movq	%rsi, %r15
	movq	24(%r14), %rax
	jmp	.LBB0_45
.LBB0_46:                               # %if.then166.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$3, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
	movq	%r15, %rsi
.LBB0_47:                               # %if.end175.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movzbl	(%r12), %eax
	cmpq	$61, %rax
	jne	.LBB0_58
# BB#48:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movslq	8(%r12), %r15
	cmpq	$53, %r15
	jl	.LBB0_58
# BB#49:                                # %lor.lhs.false185.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	reg_equiv(%rip), %rax
	leaq	(%r15,%r15,4), %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	32(%rax,%rcx,8), %rax
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB0_58
# BB#50:                                # %lor.lhs.false191.i
                                        #   in Loop: Header=BB0_4 Depth=2
	leaq	16(%r13), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	movq	%rsi, %rbx
	callq	reg_preferred_class
	movq	%rbx, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	je	.LBB0_57
# BB#51:                                # %lor.lhs.false195.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	%r15, %rdi
	movq	%rdx, %rbx
	callq	reg_preferred_class
	movq	%rbx, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$2, %rax
	je	.LBB0_57
# BB#52:                                # %lor.lhs.false199.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	%r15, %rdi
	movq	%rdx, %rbx
	callq	reg_preferred_class
	movq	%rbx, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$3, %rax
	je	.LBB0_57
# BB#53:                                # %lor.lhs.false203.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	%r15, %rdi
	movq	%rdx, %rbx
	callq	reg_preferred_class
	movq	%rbx, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$4, %rax
	je	.LBB0_57
# BB#54:                                # %lor.lhs.false207.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	%r15, %rdi
	movq	%rdx, %rbx
	callq	reg_preferred_class
	movq	%rbx, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$7, %rax
	je	.LBB0_57
# BB#55:                                # %lor.lhs.false211.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	%r15, %rdi
	movq	%rdx, %rbx
	callq	reg_preferred_class
	movq	%rbx, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$5, %rax
	je	.LBB0_57
# BB#56:                                # %lor.lhs.false215.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	%r15, %rdi
	movq	%rdx, %rbx
	callq	reg_preferred_class
	movq	%rbx, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$6, %rax
	jne	.LBB0_59
.LBB0_57:                               # %land.lhs.true219.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movzbl	(%rdx), %eax
	cmpq	$66, %rax
	jne	.LBB0_59
.LBB0_58:                               # %if.then224.i
                                        #   in Loop: Header=BB0_4 Depth=2
	xorq	%rdx, %rdx
	movq	%r13, %rdi
.LBB0_23:                               # %for.inc438.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movabsq	$no_equiv, %rsi
	callq	note_stores
	movq	96(%rsp), %rbx          # 8-byte Reload
.LBB0_96:                               # %for.inc438.i
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	8(%rbx), %rax
	movq	24(%r14), %r14
	movq	24(%rax), %rax
	cmpq	%rax, %r14
	jne	.LBB0_4
# BB#97:                                # %for.cond5.for.inc443_crit_edge.i
                                        #   in Loop: Header=BB0_2 Depth=1
	movl	n_basic_blocks(%rip), %esi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%rbx, %r15
	movq	88(%rsp), %r12          # 8-byte Reload
.LBB0_98:                               # %for.inc443.i
                                        #   in Loop: Header=BB0_2 Depth=1
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB0_2
# BB#18:                                # %for.cond446.preheader.i
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_134
# BB#19:                                # %for.body449.lr.ph.i
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB0_100:                              # %for.body449.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_102 Depth 2
                                        #       Child Loop BB0_104 Depth 3
                                        #         Child Loop BB0_115 Depth 4
                                        #         Child Loop BB0_117 Depth 4
                                        #           Child Loop BB0_118 Depth 5
	movq	basic_block_info(%rip), %rax
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	movq	24(%rax,%rsi,8), %rdi
	movq	%rdi, 80(%rsp)          # 8-byte Spill
	movq	(%rdi), %rax
	movq	8(%rdi), %r13
	movq	16(%rax), %rcx
	leaq	-1(%rsi), %rdx
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	cmpq	%rcx, %r13
	je	.LBB0_99
# BB#101:                               #   in Loop: Header=BB0_100 Depth=1
	movq	%rdi, 80(%rsp)          # 8-byte Spill
	movl	92(%rdi), %r12d
	movq	%r12, 72(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB0_102:                              # %for.body464.i
                                        #   Parent Loop BB0_100 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_104 Depth 3
                                        #         Child Loop BB0_115 Depth 4
                                        #         Child Loop BB0_117 Depth 4
                                        #           Child Loop BB0_118 Depth 5
	movzbl	(%r13), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB0_128
# BB#103:                               # %if.end473.i
                                        #   in Loop: Header=BB0_102 Depth=2
	movq	56(%r13), %r14
	testq	%r14, %r14
	je	.LBB0_128
	.align	16, 0x90
.LBB0_104:                              # %for.body479.i
                                        #   Parent Loop BB0_100 Depth=1
                                        #     Parent Loop BB0_102 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_115 Depth 4
                                        #         Child Loop BB0_117 Depth 4
                                        #           Child Loop BB0_118 Depth 5
	movl	(%r14), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$65536, %rax            # imm = 0x10000
	jne	.LBB0_126
# BB#105:                               # %land.lhs.true485.i
                                        #   in Loop: Header=BB0_104 Depth=3
	movq	8(%r14), %rdi
	movq	32(%r13), %rsi
	callq	reg_mentioned_p
	testq	%r15, %rax
	je	.LBB0_126
# BB#106:                               # %if.then494.i
                                        #   in Loop: Header=BB0_104 Depth=3
	movq	8(%r14), %rax
	movslq	8(%rax), %rdi
	movq	reg_equiv(%rip), %rax
	leaq	(%rdi,%rdi,4), %rcx
	movzbl	(%rax,%rcx,8), %ecx
	testq	%rcx, %rcx
	je	.LBB0_126
# BB#107:                               # %lor.lhs.false506.i
                                        #   in Loop: Header=BB0_104 Depth=3
	leaq	(%rdi,%rdi,4), %rbx
	movslq	24(%rax,%rbx,8), %rdx
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB0_126
# BB#108:                               # %if.end513.i
                                        #   in Loop: Header=BB0_104 Depth=3
	movq	32(%rax,%rbx,8), %rax
	testq	%rax, %rax
	je	.LBB0_110
# BB#109:                               # %lor.lhs.false519.i
                                        #   in Loop: Header=BB0_104 Depth=3
	cmpq	$0, 16(%rax)
	jne	.LBB0_110
# BB#112:                               # %if.end529.i
                                        #   in Loop: Header=BB0_104 Depth=3
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movq	%rdi, 96(%rsp)          # 8-byte Spill
	movq	8(%rax), %rbx
	movq	%rbx, %rdi
	callq	can_throw_internal
	testq	%rax, %rax
	jne	.LBB0_126
# BB#113:                               # %if.end538.i
                                        #   in Loop: Header=BB0_104 Depth=3
	movq	32(%rbx), %rdi
	callq	asm_noperands
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB0_122
# BB#114:                               # %land.lhs.true545.i
                                        #   in Loop: Header=BB0_104 Depth=3
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	reg_equiv(%rip), %rcx
	movq	80(%rax), %rax
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	16(%rcx,%rdx,8), %rcx
	movq	96(%rsp), %rdx          # 8-byte Reload
	movq	(%rax,%rdx,8), %rdi
	movq	(%rcx), %rsi
	movq	%r13, %rdx
	callq	validate_replace_rtx
	testq	%r15, %rax
	movq	%r14, %rax
	jne	.LBB0_115
.LBB0_122:                              # %if.else606.i
                                        #   in Loop: Header=BB0_104 Depth=3
	movq	16(%r13), %rax
	cmpq	%rbx, %rax
	movq	72(%rsp), %r12          # 8-byte Reload
	je	.LBB0_126
# BB#123:                               # %land.lhs.true660.i
                                        #   in Loop: Header=BB0_104 Depth=3
	movq	32(%rbx), %rdi
	movq	%r13, %rsi
	callq	emit_insn_before
	movq	56(%rbx), %rcx
	movq	%rbx, %rdi
	movq	%rax, %rbx
	movq	%rcx, 56(%rbx)
	movq	$0, 56(%rdi)
	movl	40(%rdi), %eax
	movl	%eax, 40(%rbx)
	callq	delete_insn
	movq	reg_equiv(%rip), %rax
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	32(%rax,%rcx,8), %rax
	movq	%rbx, 8(%rax)
	movq	reg_n_info(%rip), %rax
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	32(%rax,%rsi,8), %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	movl	%ecx, 36(%rax)
	movq	$2, 28(%rax)
	movq	basic_block_info(%rip), %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	24(%rax,%rcx,8), %rax
	movq	(%rax), %rcx
	cmpq	%rcx, %r13
	jne	.LBB0_125
# BB#124:                               # %if.then668.i
                                        #   in Loop: Header=BB0_104 Depth=3
	movq	16(%r13), %rcx
	movq	%rcx, (%rax)
.LBB0_125:                              # %if.end677.i
                                        #   in Loop: Header=BB0_104 Depth=3
	leaq	104(%rsp), %rdi
	callq	bitmap_set_bit
	incq	40(%rsp)                # 8-byte Folded Spill
	jmp	.LBB0_126
	.align	16, 0x90
.LBB0_115:                              # %for.cond555.i
                                        #   Parent Loop BB0_100 Depth=1
                                        #     Parent Loop BB0_102 Depth=2
                                        #       Parent Loop BB0_104 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	%rax, %r12
	movq	16(%r12), %rax
	testq	%rax, %rax
	jne	.LBB0_115
# BB#116:                               # %for.end565.i
                                        #   in Loop: Header=BB0_104 Depth=3
	movq	56(%rbx), %r15
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	jmp	.LBB0_117
	.align	16, 0x90
.LBB0_120:                              # %if.then578.i
                                        #   in Loop: Header=BB0_117 Depth=4
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	callq	remove_note
	movq	%rbx, 16(%r12)
	movq	$0, 16(%rbx)
	movq	%rbx, %r12
.LBB0_117:                              # %while.cond.outer.i
                                        #   Parent Loop BB0_100 Depth=1
                                        #     Parent Loop BB0_102 Depth=2
                                        #       Parent Loop BB0_104 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB0_118 Depth 5
	movq	96(%rsp), %rdi          # 8-byte Reload
	.align	16, 0x90
.LBB0_118:                              # %while.cond.i
                                        #   Parent Loop BB0_100 Depth=1
                                        #     Parent Loop BB0_102 Depth=2
                                        #       Parent Loop BB0_104 Depth=3
                                        #         Parent Loop BB0_117 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	%r15, %rbx
	testq	%rbx, %rbx
	je	.LBB0_121
# BB#119:                               # %while.body.i
                                        #   in Loop: Header=BB0_118 Depth=5
	movl	(%rbx), %eax
	movq	16(%rbx), %r15
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$65536, %rax            # imm = 0x10000
	jne	.LBB0_118
	jmp	.LBB0_120
.LBB0_121:                              # %while.end.i
                                        #   in Loop: Header=BB0_104 Depth=3
	movq	%r13, %rsi
	movq	%rdi, %rbx
	callq	remove_death
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%rbx,8), %rax
	movq	$0, 16(%rax)
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	delete_insn
	movq	reg_equiv(%rip), %rax
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	32(%rax,%rdx,8), %rcx
	movq	16(%rcx), %rcx
	movq	%rcx, 32(%rax,%rdx,8)
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%rbx, %r15
	movq	72(%rsp), %r12          # 8-byte Reload
	.align	16, 0x90
.LBB0_126:                              # %for.inc682.i
                                        #   in Loop: Header=BB0_104 Depth=3
	movq	16(%r14), %r14
	testq	%r14, %r14
	jne	.LBB0_104
# BB#127:                               # %for.cond477.for.inc687.loopexit_crit_edge.i
                                        #   in Loop: Header=BB0_102 Depth=2
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
.LBB0_128:                              # %for.inc687.i
                                        #   in Loop: Header=BB0_102 Depth=2
	movq	16(%r13), %r13
	movq	16(%rax), %rcx
	cmpq	%rcx, %r13
	jne	.LBB0_102
.LBB0_99:                               # %for.cond446.loopexit.i
                                        #   in Loop: Header=BB0_100 Depth=1
	movq	$32, %rcx
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB0_100
# BB#129:                               # %for.end694.i
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%r15, %rax
	je	.LBB0_134
# BB#130:                               # %if.then696.i
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$9, %rax
	jl	.LBB0_139
# BB#131:                               # %for.cond700.preheader.i
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_134
# BB#132:
	movq	$4, %rbx
	leaq	104(%rsp), %r14
	.align	16, 0x90
.LBB0_133:                              # %for.body703.i
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	(%rax,%rbx,8), %rax
	movq	64(%rax), %rdi
	movq	$1, %rcx
	movq	%rdi, %rsi
	movq	%r14, %rdx
	callq	bitmap_operation
	movq	basic_block_info(%rip), %rax
	movq	(%rax,%rbx,8), %rax
	movq	72(%rax), %rdi
	movq	$1, %rcx
	movq	%rdi, %rsi
	movq	%r14, %rdx
	callq	bitmap_operation
	movslq	n_basic_blocks(%rip), %rax
	leaq	-3(%rbx), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB0_133
	jmp	.LBB0_134
.LBB0_139:                              # %do.body.i
	movq	104(%rsp), %rdx
	jmp	.LBB0_140
.LBB0_155:                              # %if.end793.1.i
                                        #   in Loop: Header=BB0_140 Depth=1
	movq	(%rdx), %rdx
.LBB0_140:                              # %do.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_142 Depth 2
                                        #       Child Loop BB0_145 Depth 3
                                        #     Child Loop BB0_149 Depth 2
                                        #       Child Loop BB0_152 Depth 3
	testq	%rdx, %rdx
	je	.LBB0_134
# BB#141:                               # %for.cond747.preheader.i
                                        #   in Loop: Header=BB0_140 Depth=1
	movq	%rdx, 96(%rsp)          # 8-byte Spill
	movq	24(%rdx), %r12
	xorq	%r14, %r14
	testq	%r12, %r12
	je	.LBB0_148
	.align	16, 0x90
.LBB0_142:                              # %for.body759.i
                                        #   Parent Loop BB0_140 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_145 Depth 3
	movq	$1, %r13
	movq	%r14, %rcx
	shlq	%cl, %r13
	testq	%r12, %r13
	je	.LBB0_147
# BB#143:                               # %if.then762.i
                                        #   in Loop: Header=BB0_142 Depth=2
	movl	16(%rdx), %ebx
	movslq	n_basic_blocks(%rip), %rax
	movq	$7, %rcx
	shlq	%cl, %rbx
	testq	%rax, %rax
	jle	.LBB0_146
# BB#144:                               #   in Loop: Header=BB0_142 Depth=2
	movq	$4, %r15
	addq	%r14, %rbx
	.align	16, 0x90
.LBB0_145:                              # %for.body771.i
                                        #   Parent Loop BB0_140 Depth=1
                                        #     Parent Loop BB0_142 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	basic_block_info(%rip), %rax
	movq	(%rax,%r15,8), %rax
	movq	64(%rax), %rdi
	movq	%rbx, %rsi
	callq	bitmap_clear_bit
	movq	basic_block_info(%rip), %rax
	movq	(%rax,%r15,8), %rax
	movq	72(%rax), %rdi
	movq	%rbx, %rsi
	callq	bitmap_clear_bit
	movslq	n_basic_blocks(%rip), %rax
	leaq	-3(%r15), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r15
	cmpq	%rax, %rdx
	jl	.LBB0_145
.LBB0_146:                              # %for.end784.i
                                        #   in Loop: Header=BB0_142 Depth=2
	notq	%r13
	andq	%r13, %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%rbx, %r15
	movq	96(%rsp), %rdx          # 8-byte Reload
	je	.LBB0_148
.LBB0_147:                              # %for.inc790.i
                                        #   in Loop: Header=BB0_142 Depth=2
	incq	%r14
	movq	%r14, %rax
	andq	%r15, %rax
	cmpq	$64, %rax
	jb	.LBB0_142
.LBB0_148:                              # %if.end793.i
                                        #   in Loop: Header=BB0_140 Depth=1
	movq	32(%rdx), %r12
	xorq	%r14, %r14
	testq	%r12, %r12
	je	.LBB0_155
	.align	16, 0x90
.LBB0_149:                              # %for.body759.1.i
                                        #   Parent Loop BB0_140 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_152 Depth 3
	movq	$1, %r13
	movq	%r14, %rcx
	shlq	%cl, %r13
	testq	%r12, %r13
	je	.LBB0_154
# BB#150:                               # %if.then762.1.i
                                        #   in Loop: Header=BB0_149 Depth=2
	movl	16(%rdx), %eax
	movslq	n_basic_blocks(%rip), %rdx
	movq	$7, %rcx
	shlq	%cl, %rax
	testq	%rdx, %rdx
	jle	.LBB0_153
# BB#151:                               #   in Loop: Header=BB0_149 Depth=2
	movq	$4, %rbx
	leaq	64(%r14,%rax), %r15
	.align	16, 0x90
.LBB0_152:                              # %for.body771.1.i
                                        #   Parent Loop BB0_140 Depth=1
                                        #     Parent Loop BB0_149 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	basic_block_info(%rip), %rax
	movq	(%rax,%rbx,8), %rax
	movq	64(%rax), %rdi
	movq	%r15, %rsi
	callq	bitmap_clear_bit
	movq	basic_block_info(%rip), %rax
	movq	(%rax,%rbx,8), %rax
	movq	72(%rax), %rdi
	movq	%r15, %rsi
	callq	bitmap_clear_bit
	movslq	n_basic_blocks(%rip), %rax
	leaq	-3(%rbx), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB0_152
.LBB0_153:                              # %for.end784.1.i
                                        #   in Loop: Header=BB0_149 Depth=2
	notq	%r13
	andq	%r13, %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%rbx, %r15
	movq	96(%rsp), %rdx          # 8-byte Reload
	je	.LBB0_155
.LBB0_154:                              # %for.inc790.1.i
                                        #   in Loop: Header=BB0_149 Depth=2
	incq	%r14
	movq	%r14, %rax
	andq	%r15, %rax
	cmpq	$64, %rax
	jb	.LBB0_149
	jmp	.LBB0_155
.LBB0_134:                              # %update_equiv_regs.exit
	callq	end_alias_analysis
	leaq	104(%rsp), %rdi
	callq	bitmap_clear
	movq	reg_equiv(%rip), %rdi
	callq	free
	movl	max_regno(%rip), %r12d
	addq	$-53, %r12
	movq	$32, %rcx
	movq	%r12, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	imulq	$44, %r14, %rdi
	callq	xmalloc
	movq	%rax, qty(%rip)
	movq	$3, %rcx
	movq	%r14, %r15
	shlq	%cl, %r15
	movq	%r15, %rdi
	callq	xmalloc
	movq	%rax, qty_phys_copy_sugg(%rip)
	addq	%r14, %r14
	movq	%r14, %rdi
	callq	xmalloc
	movq	%rax, qty_phys_num_copy_sugg(%rip)
	movq	%r15, %rdi
	callq	xmalloc
	movq	%rax, qty_phys_sugg(%rip)
	movq	%r14, %rdi
	callq	xmalloc
	movslq	max_regno(%rip), %rdi
	movq	%rax, qty_phys_num_sugg(%rip)
	movq	$2, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	max_regno(%rip), %rdi
	movq	%rax, reg_qty(%rip)
	callq	xmalloc
	movslq	max_regno(%rip), %rdi
	movq	%rax, reg_offset(%rip)
	movq	$2, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	max_regno(%rip), %rcx
	movq	%rax, reg_next_in_qty(%rip)
	cmpq	$54, %rcx
	jl	.LBB0_158
# BB#135:                               # %for.body.lr.ph
	movq	reg_n_info(%rip), %rax
	movq	reg_qty(%rip), %rdx
	xorq	%rsi, %rsi
	movabsq	$4294967294, %r8        # imm = 0xFFFFFFFE
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_136:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	456(%rax,%rsi,8), %rcx
	movslq	36(%rcx), %rbx
	testq	%rbx, %rbx
	js	.LBB0_156
# BB#137:                               # %land.lhs.true
                                        #   in Loop: Header=BB0_136 Depth=1
	movl	24(%rcx), %ecx
	cmpq	$1, %rcx
	jne	.LBB0_156
# BB#138:                               # %if.then
                                        #   in Loop: Header=BB0_136 Depth=1
	movl	%r8d, 212(%rdx,%rsi,4)
	jmp	.LBB0_157
	.align	16, 0x90
.LBB0_156:                              # %if.else
                                        #   in Loop: Header=BB0_136 Depth=1
	movl	%r9d, 212(%rdx,%rsi,4)
.LBB0_157:                              # %for.inc
                                        #   in Loop: Header=BB0_136 Depth=1
	movslq	max_regno(%rip), %rbx
	leaq	54(%rsi), %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rsi
	cmpq	%rbx, %rdi
	jl	.LBB0_136
.LBB0_158:                              # %for.end
	movslq	n_basic_blocks(%rip), %rax
	movl	%r12d, next_qty(%rip)
	testq	%rax, %rax
	jle	.LBB0_330
# BB#159:                               # %for.body38.lr.ph
	xorq	%rsi, %rsi
	movzbl	rtx_class+32(%rip), %eax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	$255, %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%rbx, %rbx
	jmp	.LBB0_160
	.align	16, 0x90
.LBB0_329:                              # %block_alloc.exit.for.body38_crit_edge
                                        #   in Loop: Header=BB0_160 Depth=1
	movl	next_qty(%rip), %r12d
	xorq	%rsi, %rsi
	movq	%r13, %r14
.LBB0_160:                              # %for.body38
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_163 Depth 2
                                        #     Child Loop BB0_166 Depth 2
                                        #     Child Loop BB0_172 Depth 2
                                        #       Child Loop BB0_183 Depth 3
                                        #       Child Loop BB0_189 Depth 3
                                        #         Child Loop BB0_199 Depth 4
                                        #       Child Loop BB0_255 Depth 3
                                        #       Child Loop BB0_258 Depth 3
                                        #       Child Loop BB0_269 Depth 3
                                        #     Child Loop BB0_284 Depth 2
                                        #     Child Loop BB0_296 Depth 2
                                        #     Child Loop BB0_300 Depth 2
                                        #     Child Loop BB0_316 Depth 2
                                        #     Child Loop BB0_322 Depth 2
                                        #       Child Loop BB0_326 Depth 3
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$5, %rax
	jg	.LBB0_164
# BB#161:                               # %for.cond42.preheader
                                        #   in Loop: Header=BB0_160 Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rbx, %r13
	jle	.LBB0_165
# BB#162:                               # %for.body45.lr.ph
                                        #   in Loop: Header=BB0_160 Depth=1
	movq	qty_phys_copy_sugg(%rip), %r8
	movq	qty_phys_num_copy_sugg(%rip), %r9
	movq	qty_phys_sugg(%rip), %r10
	movq	qty_phys_num_sugg(%rip), %rdi
	movq	$1, %rbx
	.align	16, 0x90
.LBB0_163:                              # %for.body45
                                        #   Parent Loop BB0_160 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$0, -8(%r8,%rbx,8)
	movw	%si, -2(%r9,%rbx,2)
	movq	$0, -8(%r10,%rbx,8)
	movw	%si, -2(%rdi,%rbx,2)
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rdx, %rax
	jl	.LBB0_163
	jmp	.LBB0_165
	.align	16, 0x90
.LBB0_164:                              # %if.else57
                                        #   in Loop: Header=BB0_160 Depth=1
	movq	qty_phys_copy_sugg(%rip), %rdi
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	$3, %rcx
	shlq	%cl, %r12
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	memset
	movslq	next_qty(%rip), %rdx
	movq	qty_phys_num_copy_sugg(%rip), %rdi
	addq	%rdx, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movslq	next_qty(%rip), %rdx
	movq	qty_phys_sugg(%rip), %rdi
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movslq	next_qty(%rip), %rdx
	movq	qty_phys_num_sugg(%rip), %rdi
	addq	%rdx, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movq	%rbx, %r13
.LBB0_165:                              # %if.end66
                                        #   in Loop: Header=BB0_160 Depth=1
	xorq	%rbx, %rbx
	movl	%ebx, next_qty(%rip)
	callq	get_max_uid
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%r13,8), %r8
	leaq	8(%r8), %rsi
	jmp	.LBB0_166
	.align	16, 0x90
.LBB0_170:                              # %if.end10.i
                                        #   in Loop: Header=BB0_166 Depth=2
	addq	$16, %rsi
.LBB0_166:                              # %while.body.i96
                                        #   Parent Loop BB0_160 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rsi), %rsi
	movzbl	(%rsi), %ecx
	cmpq	$37, %rcx
	je	.LBB0_169
# BB#167:                               # %if.then.i
                                        #   in Loop: Header=BB0_166 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rdi
	jge	.LBB0_331
# BB#168:                               #   in Loop: Header=BB0_166 Depth=2
	incq	%rbx
.LBB0_169:                              # %if.end3.i
                                        #   in Loop: Header=BB0_166 Depth=2
	movq	(%r8), %rcx
	cmpq	%rcx, %rsi
	jne	.LBB0_170
# BB#171:                               # %while.end.i104
                                        #   in Loop: Header=BB0_160 Depth=1
	addq	%rbx, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$8589934592, %rax       # imm = 0x200000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$8, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	basic_block_info(%rip), %rcx
	movq	%rax, regs_live_at(%rip)
	movq	$0, regs_live(%rip)
	movq	32(%rcx,%r13,8), %rax
	movq	64(%rax), %rsi
	movabsq	$regs_live, %rdi
	callq	reg_set_to_hard_reg_set
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r13,8), %r12
	xorq	%rdi, %rdi
	movq	%r14, 64(%rsp)          # 8-byte Spill
	jmp	.LBB0_172
	.align	16, 0x90
.LBB0_282:                              # %if.end543.i
                                        #   in Loop: Header=BB0_172 Depth=2
	addq	$24, %r12
.LBB0_172:                              # %while.body22.i
                                        #   Parent Loop BB0_160 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_183 Depth 3
                                        #       Child Loop BB0_189 Depth 3
                                        #         Child Loop BB0_199 Depth 4
                                        #       Child Loop BB0_255 Depth 3
                                        #       Child Loop BB0_258 Depth 3
                                        #       Child Loop BB0_269 Depth 3
	movq	(%r12), %r12
	movzbl	(%r12), %eax
	cmpq	$37, %rax
	movabsq	$0, %rcx
	je	.LBB0_174
# BB#173:                               # %while.body22.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movabsq	$1, %rcx
.LBB0_174:                              # %while.body22.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movzbl	rtx_class(%rax), %eax
	addq	%rcx, %rdi
	cmpq	$105, %rax
	jne	.LBB0_281
# BB#175:                               # %if.then37.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	%r13, 56(%rsp)          # 8-byte Spill
	movl	%edi, this_insn_number(%rip)
	movq	%rdi, 80(%rsp)          # 8-byte Spill
	movq	%r12, this_insn(%rip)
	movq	%r12, %rdi
	callq	extract_insn
	movl	recog_data+996(%rip), %esi
	movq	%r14, %r10
	movl	%r10d, which_alternative(%rip)
	movl	optimize(%rip), %eax
	movq	$16, %rcx
	movq	%rsi, %r13
	shrq	%cl, %r13
	xorq	%r15, %r15
	testq	%rax, %rax
	je	.LBB0_176
# BB#177:                               # %if.then37.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	$56, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	cmpq	$2, %rdx
	jl	.LBB0_178
# BB#179:                               # %land.lhs.true42.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	recog_data+480(%rip), %rcx
	movzbl	(%rcx), %edx
	xorq	%r15, %r15
	cmpq	$61, %rdx
	jne	.LBB0_180
# BB#181:                               # %land.lhs.true47.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movzbl	1(%rcx), %ecx
	xorq	%r15, %r15
	cmpq	$38, %rcx
	movabsq	$0, %rbx
	je	.LBB0_218
# BB#182:                               # %for.body.lr.ph.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	%r12, 32(%rsp)          # 8-byte Spill
	movq	$56, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	$56, %rcx
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	movq	%rsi, 96(%rsp)          # 8-byte Spill
	xorq	%rbx, %rbx
	xorq	%r14, %r14
	movq	%r10, %r15
	.align	16, 0x90
.LBB0_183:                              # %for.body.i108
                                        #   Parent Loop BB0_160 Depth=1
                                        #     Parent Loop BB0_172 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	recog_data+488(,%rbx,8), %rdi
	movq	%r10, %r12
	callq	requires_inout
	movq	%r12, %r10
	movq	%rax, %rdx
	andq	%r10, %rdx
	movq	88(%rsp), %rsi          # 8-byte Reload
	andq	%r10, %rsi
	leaq	1(%rbx), %rcx
	cmpq	%rsi, %rdx
	movq	%rcx, %rdx
	je	.LBB0_185
# BB#184:                               # %for.body.i108
                                        #   in Loop: Header=BB0_183 Depth=3
	movq	%r15, %rdx
.LBB0_185:                              # %for.body.i108
                                        #   in Loop: Header=BB0_183 Depth=3
	addq	%rax, %r14
	addq	$2, %rbx
	andq	%r10, %rbx
	movq	96(%rsp), %rax          # 8-byte Reload
	andq	%r10, %rax
	cmpq	%rax, %rbx
	movq	%rcx, %rbx
	movq	%rdx, %r15
	jne	.LBB0_183
# BB#186:                               # %for.body70.lr.ph.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	recog_data(%rip), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	jns	.LBB0_188
# BB#187:                               # %for.body70.lr.ph.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movabsq	$1, %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB0_188:                              # %for.body70.lr.ph.i
                                        #   in Loop: Header=BB0_172 Depth=2
	leaq	1(%rdx), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	leaq	-1(%rdx), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%rdx, 96(%rsp)          # 8-byte Spill
	xorq	%rbx, %rbx
	movq	$1, %r12
	.align	16, 0x90
.LBB0_189:                              # %for.body70.i
                                        #   Parent Loop BB0_160 Depth=1
                                        #     Parent Loop BB0_172 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_199 Depth 4
	movq	%r12, %rax
	andq	%r10, %rax
	movq	96(%rsp), %rcx          # 8-byte Reload
	andq	%r10, %rcx
	cmpq	%rcx, %rax
	movabsq	$0, %rcx
	jne	.LBB0_191
# BB#190:                               # %for.body70.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movabsq	$1, %rcx
.LBB0_191:                              # %for.body70.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movq	88(%rsp), %rdx          # 8-byte Reload
	orq	%rdx, %rcx
	jne	.LBB0_196
# BB#192:                               # %land.lhs.true76.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movq	48(%rsp), %rcx          # 8-byte Reload
	andq	%r10, %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_194
# BB#193:                               # %land.lhs.true80.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movq	recog_data+472(,%r12,8), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$37, %rcx
	je	.LBB0_196
.LBB0_194:                              # %land.lhs.true87.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movq	40(%rsp), %rcx          # 8-byte Reload
	andq	%r10, %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_216
# BB#195:                               # %land.lhs.true91.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movq	recog_data+480(,%r12,8), %rax
	movzbl	(%rax), %eax
	cmpq	$37, %rax
	jne	.LBB0_216
	.align	16, 0x90
.LBB0_196:                              # %if.end99.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movq	$56, %rcx
	shlq	%cl, %r13
	movq	$56, %rcx
	sarq	%cl, %r13
	movq	recog_data+480(,%r12,8), %r15
	andq	%r10, %r13
	movq	%r14, %rax
	andq	%r10, %rax
	cmpq	%r13, %rax
	jne	.LBB0_198
# BB#197:                               # %land.lhs.true103.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movq	%r15, %rdi
	movq	%r10, %r13
	callq	requires_inout
	movq	%r13, %r10
	testq	%r10, %rax
	je	.LBB0_216
.LBB0_198:                              # %if.end110.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movzbl	(%r15), %eax
	movq	recog_data(,%r12,8), %rbx
	cmpq	$112, %rax
	jne	.LBB0_202
	jmp	.LBB0_199
	.align	16, 0x90
.LBB0_201:                              # %while.body129.i
                                        #   in Loop: Header=BB0_199 Depth=4
	movq	8(%rbx), %rbx
.LBB0_199:                              # %while.cond120.i
                                        #   Parent Loop BB0_160 Depth=1
                                        #     Parent Loop BB0_172 Depth=2
                                        #       Parent Loop BB0_189 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movzbl	(%rbx), %eax
	cmpq	$75, %rax
	je	.LBB0_201
# BB#200:                               # %while.cond120.i
                                        #   in Loop: Header=BB0_199 Depth=4
	cmpq	$78, %rax
	je	.LBB0_201
.LBB0_202:                              # %if.end134.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movq	%r10, %r13
	movq	cfun(%rip), %rdi
	movq	%rbx, %rsi
	callq	get_hard_reg_initial_reg
	testq	%rax, %rax
	je	.LBB0_207
# BB#203:                               # %if.then138.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB0_207
# BB#204:                               # %land.lhs.true143.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movl	8(%rax), %eax
	cmpq	$52, %rax
	ja	.LBB0_207
# BB#205:                               # %land.lhs.true150.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movzbl	call_used_regs(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_207
# BB#206:                               #   in Loop: Header=BB0_189 Depth=3
	movq	%r13, %r10
	jmp	.LBB0_216
	.align	16, 0x90
.LBB0_207:                              # %if.end159.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movq	72(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %eax
	orq	$2, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB0_208
# BB#209:                               # %if.then168.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movq	$32, %rcx
	movq	96(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rax
	js	.LBB0_211
# BB#210:                               # %if.then168.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movabsq	$1, %rax
.LBB0_211:                              # %if.then168.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movq	recog_data(,%r12,8), %rcx
	cmpq	%rcx, %rbx
	movabsq	$0, %rdx
	jne	.LBB0_213
# BB#212:                               # %if.then168.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movabsq	$1, %rdx
.LBB0_213:                              # %if.then168.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movl	(%rbx), %ecx
	orq	$2, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB0_214
# BB#215:                               # %if.end187.i
                                        #   in Loop: Header=BB0_189 Depth=3
	andq	%rax, %rdx
	xorq	%r9, %r9
	movq	%rbx, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	32(%rsp), %r8           # 8-byte Reload
	callq	combine_regs
	movq	%rax, %r15
	movq	%r13, %r10
	testq	%r10, %r15
	je	.LBB0_216
	jmp	.LBB0_217
.LBB0_208:                              #   in Loop: Header=BB0_189 Depth=3
	movq	%r13, %r10
	jmp	.LBB0_216
.LBB0_214:                              #   in Loop: Header=BB0_189 Depth=3
	movq	%r13, %r10
	.align	16, 0x90
.LBB0_216:                              # %for.inc191.i
                                        #   in Loop: Header=BB0_189 Depth=3
	movl	recog_data+996(%rip), %r13d
	incq	%r12
	movq	$56, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$16, %rcx
	shrq	%cl, %r13
	xorq	%r15, %r15
	cmpq	%rax, %rdx
	jl	.LBB0_189
.LBB0_217:                              # %if.end194thread-pre-split.loopexit.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movl	optimize(%rip), %eax
	movq	32(%rsp), %r12          # 8-byte Reload
	jmp	.LBB0_218
.LBB0_176:                              #   in Loop: Header=BB0_172 Depth=2
	xorq	%rbx, %rbx
	jmp	.LBB0_218
.LBB0_178:                              #   in Loop: Header=BB0_172 Depth=2
	xorq	%rbx, %rbx
	jmp	.LBB0_218
.LBB0_180:                              #   in Loop: Header=BB0_172 Depth=2
	xorq	%rbx, %rbx
.LBB0_218:                              # %if.end194.i
                                        #   in Loop: Header=BB0_172 Depth=2
	testq	%r10, %rax
	jne	.LBB0_220
# BB#219:                               #   in Loop: Header=BB0_172 Depth=2
	movq	56(%rsp), %r13          # 8-byte Reload
	jmp	.LBB0_252
	.align	16, 0x90
.LBB0_220:                              # %land.lhs.true196.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	32(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$49, %rcx
	movq	56(%rsp), %r13          # 8-byte Reload
	jne	.LBB0_252
# BB#221:                               # %land.lhs.true204.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	%r12, %r14
	movq	8(%rax), %rcx
	movzbl	(%rcx), %eax
	cmpq	$61, %rax
	jne	.LBB0_222
# BB#223:                               # %land.lhs.true215.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	%rcx, 96(%rsp)          # 8-byte Spill
	movq	%r10, %r12
	movq	$7, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB0_224
# BB#225:                               # %land.lhs.true219.i115
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	8(%rax), %rdi
	testq	%rdi, %rdi
	movq	%r12, %r10
	jne	.LBB0_227
# BB#226:                               #   in Loop: Header=BB0_172 Depth=2
	movq	%r14, %r12
	jmp	.LBB0_252
.LBB0_222:                              #   in Loop: Header=BB0_172 Depth=2
	movq	%r14, %r12
	jmp	.LBB0_252
.LBB0_227:                              # %land.lhs.true225.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movzbl	(%rdi), %ecx
	cmpq	$32, %rcx
	movq	%r14, %r12
	jne	.LBB0_252
# BB#228:                               # %land.lhs.true225.i
                                        #   in Loop: Header=BB0_172 Depth=2
	cmpq	$105, 24(%rsp)          # 8-byte Folded Reload
	jne	.LBB0_252
# BB#229:                               # %cond.true.i117
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	32(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB0_231
# BB#230:                               # %cond.false.i118
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	%rax, %rsi
	movq	%r12, %r14
	movq	%r10, %r12
	callq	single_set_2
	movq	%r12, %r10
	movq	%r14, %r12
.LBB0_231:                              # %cond.end272.i
                                        #   in Loop: Header=BB0_172 Depth=2
	testq	%rax, %rax
	je	.LBB0_252
# BB#232:                               # %land.lhs.true276.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	8(%rax), %rcx
	movq	96(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %rcx
	movq	%rdx, %rcx
	jne	.LBB0_252
# BB#233:                               # %land.lhs.true282.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	16(%rax), %rax
	cmpq	%rcx, %rax
	jne	.LBB0_252
# BB#234:                               # %land.lhs.true288.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	%r12, %r14
	movq	%r10, %r12
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdi
	movq	$4, %rsi
	xorq	%rdx, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB0_235
.LBB0_224:                              #   in Loop: Header=BB0_172 Depth=2
	movq	%r12, %r10
	movq	%r14, %r12
	.align	16, 0x90
.LBB0_252:                              # %if.end401.i
                                        #   in Loop: Header=BB0_172 Depth=2
	testq	%r10, %r15
	jne	.LBB0_255
# BB#253:                               #   in Loop: Header=BB0_172 Depth=2
	movq	%r10, %r14
	jmp	.LBB0_257
	.align	16, 0x90
.LBB0_254:                              # %while.body409.i
                                        #   in Loop: Header=BB0_255 Depth=3
	movq	(%rbx), %rbx
.LBB0_255:                              # %while.body409.i
                                        #   Parent Loop BB0_160 Depth=1
                                        #     Parent Loop BB0_172 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rbx), %eax
	addq	$8, %rbx
	cmpq	$63, %rax
	je	.LBB0_254
# BB#256:                               # %while.end413.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movl	(%rbx), %r14d
.LBB0_257:                              # %if.end417.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	56(%r12), %rbx
	jmp	.LBB0_258
	.align	16, 0x90
.LBB0_267:                              # %for.inc465.i
                                        #   in Loop: Header=BB0_258 Depth=3
	movq	16(%rbx), %rbx
.LBB0_258:                              # %if.end417.i
                                        #   Parent Loop BB0_160 Depth=1
                                        #     Parent Loop BB0_172 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB0_268
# BB#259:                               # %for.body423.i
                                        #   in Loop: Header=BB0_258 Depth=3
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$65536, %rax            # imm = 0x10000
	jne	.LBB0_267
# BB#260:                               # %land.lhs.true428.i
                                        #   in Loop: Header=BB0_258 Depth=3
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB0_267
# BB#261:                               # %land.lhs.true436.i
                                        #   in Loop: Header=BB0_258 Depth=3
	movl	8(%rax), %ecx
	movq	%r14, %rdx
	andq	%r10, %rdx
	cmpq	%rcx, %rdx
	je	.LBB0_267
# BB#262:                               # %land.lhs.true445.i
                                        #   in Loop: Header=BB0_258 Depth=3
	movq	64(%rsp), %rdx          # 8-byte Reload
	andq	%r10, %rdx
	cmpq	%rcx, %rdx
	jne	.LBB0_263
# BB#264:                               # %lor.lhs.false454.i
                                        #   in Loop: Header=BB0_258 Depth=3
	movq	$9, %rsi
	movq	%r12, %rdi
	movq	%rax, %rdx
	movq	%r10, %r15
	callq	find_reg_note
	movq	%r15, %r10
	testq	%rax, %rax
	jne	.LBB0_267
# BB#265:                               # %lor.lhs.false454.if.then460_crit_edge.i
                                        #   in Loop: Header=BB0_258 Depth=3
	movq	%r10, %r15
	movq	8(%rbx), %rax
	jmp	.LBB0_266
.LBB0_263:                              #   in Loop: Header=BB0_258 Depth=3
	movq	%r10, %r15
.LBB0_266:                              # %if.then460.i
                                        #   in Loop: Header=BB0_258 Depth=3
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	wipe_dead_reg
	movq	%r15, %r10
	jmp	.LBB0_267
	.align	16, 0x90
.LBB0_268:                              # %for.end469.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	%r10, %r14
	movq	32(%r12), %rdi
	xorq	%rdx, %rdx
	movabsq	$reg_is_set, %rsi
	callq	note_stores
	movq	56(%r12), %rbx
	jmp	.LBB0_269
	.align	16, 0x90
.LBB0_273:                              # %for.inc497.i
                                        #   in Loop: Header=BB0_269 Depth=3
	movq	16(%rbx), %rbx
.LBB0_269:                              # %for.end469.i
                                        #   Parent Loop BB0_160 Depth=1
                                        #     Parent Loop BB0_172 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB0_274
# BB#270:                               # %for.body478.i
                                        #   in Loop: Header=BB0_269 Depth=3
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$655360, %rax           # imm = 0xA0000
	jne	.LBB0_273
# BB#271:                               # %land.lhs.true484.i
                                        #   in Loop: Header=BB0_269 Depth=3
	movq	8(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB0_273
# BB#272:                               # %if.then492.i
                                        #   in Loop: Header=BB0_269 Depth=3
	movq	$1, %rsi
	callq	wipe_dead_reg
	jmp	.LBB0_273
	.align	16, 0x90
.LBB0_274:                              # %for.end501.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB0_276
# BB#275:                               #   in Loop: Header=BB0_172 Depth=2
	movq	80(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB0_281
	.align	16, 0x90
.LBB0_276:                              # %land.lhs.true505.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$32, %rcx
	jne	.LBB0_277
# BB#278:                               # %land.lhs.true513.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	32(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$49, %rax
	movq	$-1, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	je	.LBB0_280
# BB#279:                               # %land.lhs.true513.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	64(%rsp), %rax          # 8-byte Reload
.LBB0_280:                              # %land.lhs.true513.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	%rax, 64(%rsp)          # 8-byte Spill
	jmp	.LBB0_281
.LBB0_277:                              #   in Loop: Header=BB0_172 Depth=2
	movq	80(%rsp), %rdi          # 8-byte Reload
	.align	16, 0x90
.LBB0_281:                              # %if.end526.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	regs_live(%rip), %rax
	movq	$33, %rcx
	movq	%rdi, %rdx
	movq	regs_live_at(%rip), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	orq	%rax, (%rsi,%rdx,8)
	movq	regs_live(%rip), %rax
	orq	%rax, 8(%rsi,%rdx,8)
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r13,8), %rax
	movq	8(%rax), %rax
	cmpq	%rax, %r12
	jne	.LBB0_282
	jmp	.LBB0_283
.LBB0_235:                              # %if.then295.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	8(%rax), %rbx
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movl	(%rbx), %ecx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	movq	%rbx, %rax
	jne	.LBB0_238
# BB#236:                               # %land.rhs303.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	no_conflict_p
	testq	%r12, %rax
	jne	.LBB0_332
# BB#237:                               # %land.rhs303.if.else_crit_edge.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movl	(%rax), %ecx
.LBB0_238:                              # %if.else.i119
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	movq	rtx_format(,%rdx,8), %rdx
	movzbl	(%rdx), %edx
	cmpq	$101, %rdx
	jne	.LBB0_239
# BB#240:                               # %land.lhs.true321.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	8(%rax), %rbx
	movl	(%rbx), %edx
	orq	$2, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$63, %rdx
	jne	.LBB0_241
# BB#242:                               # %land.lhs.true338.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	no_conflict_p
	movq	%r12, %r10
	testq	%r10, %rax
	jne	.LBB0_333
# BB#243:                               # %land.lhs.true338.if.else343_crit_edge.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movl	(%rax), %ecx
	jmp	.LBB0_244
.LBB0_239:                              #   in Loop: Header=BB0_172 Depth=2
	movq	%r12, %r10
	jmp	.LBB0_244
.LBB0_332:                              # %if.then308.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	$1, %rdx
	movq	$1, %r9
	movq	%rbx, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	%r14, %r8
	callq	combine_regs
	movq	%rax, %r15
	movq	%r12, %r10
	movq	%r14, %r12
	jmp	.LBB0_250
.LBB0_241:                              #   in Loop: Header=BB0_172 Depth=2
	movq	%r12, %r10
.LBB0_244:                              # %if.else343.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	%rcx, %rdx
	andq	$65534, %rdx            # imm = 0xFFFE
	cmpq	$102, %rdx
	movq	%r14, %r12
	je	.LBB0_246
# BB#245:                               # %lor.lhs.false359.i
                                        #   in Loop: Header=BB0_172 Depth=2
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$99, %rcx
	jne	.LBB0_250
.LBB0_246:                              # %land.lhs.true370.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	%r10, %r14
	movq	16(%rax), %rbx
	movl	(%rbx), %eax
	orq	$2, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB0_247
# BB#248:                               # %land.lhs.true387.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	no_conflict_p
	movq	%r14, %r10
	testq	%r10, %rax
	je	.LBB0_250
# BB#249:                               # %if.then390.i
                                        #   in Loop: Header=BB0_172 Depth=2
	xorq	%rdx, %rdx
	movq	$1, %r9
	movq	%rbx, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	%r12, %r8
	movq	%r10, %r14
	callq	combine_regs
	movq	%r14, %r10
	movq	%rax, %r15
	jmp	.LBB0_250
.LBB0_247:                              #   in Loop: Header=BB0_172 Depth=2
	movq	%r14, %r10
	jmp	.LBB0_250
.LBB0_333:                              # %if.then341.i
                                        #   in Loop: Header=BB0_172 Depth=2
	xorq	%rdx, %rdx
	movq	$1, %r9
	movq	%rbx, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	%r14, %r8
	movq	%r10, %r15
	callq	combine_regs
	movq	%r15, %r10
	movq	%rax, %r15
	movq	%r14, %r12
.LBB0_250:                              # %if.end394.i
                                        #   in Loop: Header=BB0_172 Depth=2
	testq	%r10, %r15
	movq	%r10, %r14
	je	.LBB0_257
# BB#251:                               # %if.then396.i
                                        #   in Loop: Header=BB0_172 Depth=2
	movl	8(%rbx), %eax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	jmp	.LBB0_252
	.align	16, 0x90
.LBB0_283:                              # %while.end547.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movq	%r13, 56(%rsp)          # 8-byte Spill
	movslq	next_qty(%rip), %rdi
	movq	$2, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%r14, %r10
	movslq	next_qty(%rip), %rsi
	movq	%rax, %r15
	xorq	%rax, %rax
	testq	%rsi, %rsi
	movq	$65535, %r13            # imm = 0xFFFF
	jle	.LBB0_285
	.align	16, 0x90
.LBB0_284:                              # %for.body554.i
                                        #   Parent Loop BB0_160 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	%eax, (%r15,%rax,4)
	incq	%rax
	movq	%rax, %rcx
	andq	%r10, %rcx
	movq	%rsi, %rdx
	andq	%r10, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB0_284
.LBB0_285:                              # %for.end559.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movq	%rsi, %rax
	andq	%r10, %rax
	cmpq	$2, %rax
	jb	.LBB0_295
# BB#286:                               # %for.end559.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movq	%r10, %rbx
	je	.LBB0_292
# BB#287:                               # %for.end559.i
                                        #   in Loop: Header=BB0_160 Depth=1
	cmpq	$3, %rax
	jne	.LBB0_294
# BB#288:                               # %sw.bb.i
                                        #   in Loop: Header=BB0_160 Depth=1
	xorq	%rdi, %rdi
	movq	$1, %rsi
	callq	qty_sugg_compare
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_290
# BB#289:                               # %if.then563.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movl	(%r15), %eax
	movl	4(%r15), %ecx
	movl	%ecx, (%r15)
	movl	%eax, 4(%r15)
.LBB0_290:                              # %if.end568.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movq	$1, %rdi
	movq	$2, %rsi
	callq	qty_sugg_compare
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_292
# BB#291:                               # %if.then572.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movl	8(%r15), %eax
	movl	4(%r15), %ecx
	movl	%ecx, 8(%r15)
	movl	%eax, 4(%r15)
	.align	16, 0x90
.LBB0_292:                              # %sw.bb578.i
                                        #   in Loop: Header=BB0_160 Depth=1
	xorq	%rdi, %rdi
	movq	$1, %rsi
	callq	qty_sugg_compare
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rbx, %r10
	jle	.LBB0_295
# BB#293:                               # %if.then582.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movl	(%r15), %eax
	movl	4(%r15), %ecx
	movl	%ecx, (%r15)
	movl	%eax, 4(%r15)
	jmp	.LBB0_295
.LBB0_294:                              # %sw.default.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movq	$4, %rdx
	movq	%r15, %rdi
	movabsq	$qty_sugg_compare_1, %rcx
	callq	specqsort
	movq	%rbx, %r10
	.align	16, 0x90
.LBB0_295:                              # %for.cond589.preheader.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movslq	next_qty(%rip), %rsi
	testq	%rsi, %rsi
	movq	$1, %r14
	jle	.LBB0_301
	.align	16, 0x90
.LBB0_296:                              # %for.body592.i
                                        #   Parent Loop BB0_160 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-4(%r15,%r14,4), %rdx
	movq	qty_phys_num_sugg(%rip), %rax
	movzbl	(%rax,%rdx,2), %eax
	testq	%rax, %rax
	jne	.LBB0_298
# BB#297:                               # %lor.lhs.false600.i
                                        #   in Loop: Header=BB0_296 Depth=2
	movq	qty_phys_num_copy_sugg(%rip), %rax
	movzbl	(%rax,%rdx,2), %eax
	testq	%rax, %rax
	jne	.LBB0_298
# BB#305:                               # %if.else619.i
                                        #   in Loop: Header=BB0_296 Depth=2
	movq	qty(%rip), %rax
	imulq	$44, %rdx, %rcx
	movw	%r13w, 40(%rax,%rcx)
	jmp	.LBB0_306
	.align	16, 0x90
.LBB0_298:                              # %if.then606.i
                                        #   in Loop: Header=BB0_296 Depth=2
	movq	qty(%rip), %rax
	imulq	$44, %rdx, %rbx
	movl	8(%rax,%rbx), %r9d
	movl	36(%rax,%rbx), %esi
	movl	28(%rax,%rbx), %edi
	movl	12(%rax,%rbx), %eax
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%rax, (%rsp)
	movq	%r10, %r12
	callq	find_free_reg
	movq	%r12, %r10
	movq	qty(%rip), %rcx
	movw	%ax, 40(%rcx,%rbx)
	movl	next_qty(%rip), %esi
.LBB0_306:                              # %for.inc624.i
                                        #   in Loop: Header=BB0_296 Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r14
	cmpq	%rdx, %rax
	jl	.LBB0_296
# BB#299:                               # %for.cond627.preheader.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	jle	.LBB0_301
	.align	16, 0x90
.LBB0_300:                              # %for.body630.i
                                        #   Parent Loop BB0_160 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	%eax, (%r15,%rax,4)
	incq	%rax
	movq	%rax, %rcx
	andq	%r10, %rcx
	movq	%rsi, %rdx
	andq	%r10, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB0_300
.LBB0_301:                              # %for.end635.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movq	%rsi, %rax
	andq	%r10, %rax
	movq	%r10, %r13
	cmpq	$2, %rax
	jb	.LBB0_314
# BB#302:                               # %for.end635.i
                                        #   in Loop: Header=BB0_160 Depth=1
	cmpq	$3, %rax
	jne	.LBB0_303
# BB#307:                               # %sw.bb636.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movq	qty(%rip), %rax
	movslq	44(%rax), %rdi
	callq	floor_log2_wide
	movq	qty(%rip), %rbx
	movl	48(%rbx), %ecx
	movl	60(%rbx), %edi
	imulq	%rax, %rcx
	imulq	%rcx, %rdi
	andq	%r13, %rdi
	callq	int32_to_float64
	movl	56(%rbx), %edi
	movl	52(%rbx), %ecx
	movq	%rax, %r14
	subq	%rcx, %rdi
	andq	%r13, %rdi
	callq	int32_to_float64
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rdi
	movabsq	$4621819117588971520, %rax # imm = 0x4024000000000000
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	callq	float64_to_int64
	movslq	(%rbx), %rdi
	movq	%rax, %r14
	callq	floor_log2_wide
	movq	qty(%rip), %rbx
	movl	4(%rbx), %ecx
	movl	16(%rbx), %edi
	imulq	%rax, %rcx
	imulq	%rcx, %rdi
	andq	%r13, %rdi
	callq	int32_to_float64
	movl	12(%rbx), %edi
	movl	8(%rbx), %ecx
	movq	%rax, %r12
	subq	%rcx, %rdi
	andq	%r13, %rdi
	callq	int32_to_float64
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rdi
	movabsq	$4621819117588971520, %rsi # imm = 0x4024000000000000
	movq	%rsi, %r12
	callq	float64_mul
	movq	%rax, %rdi
	callq	float64_to_int64
	subq	%rax, %r14
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	testq	%r14, %r14
	jle	.LBB0_309
# BB#308:                               # %if.then640.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movl	(%r15), %eax
	movl	4(%r15), %ecx
	movl	%ecx, (%r15)
	movl	%eax, 4(%r15)
.LBB0_309:                              # %if.end645.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movslq	88(%rbx), %rdi
	callq	floor_log2_wide
	movq	qty(%rip), %rbx
	movl	92(%rbx), %ecx
	movl	104(%rbx), %edi
	imulq	%rax, %rcx
	imulq	%rcx, %rdi
	andq	%r13, %rdi
	callq	int32_to_float64
	movl	100(%rbx), %edi
	movl	96(%rbx), %ecx
	movq	%rax, %r14
	subq	%rcx, %rdi
	andq	%r13, %rdi
	callq	int32_to_float64
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	callq	float64_to_int64
	movslq	44(%rbx), %rdi
	movq	%rax, %r14
	callq	floor_log2_wide
	movq	qty(%rip), %rbx
	movl	48(%rbx), %ecx
	movl	60(%rbx), %edi
	imulq	%rax, %rcx
	imulq	%rcx, %rdi
	andq	%r13, %rdi
	callq	int32_to_float64
	movl	56(%rbx), %edi
	movl	52(%rbx), %ecx
	movq	%rax, %r12
	subq	%rcx, %rdi
	andq	%r13, %rdi
	callq	int32_to_float64
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rdi
	movabsq	$4621819117588971520, %rsi # imm = 0x4024000000000000
	callq	float64_mul
	movq	%rax, %rdi
	callq	float64_to_int64
	subq	%rax, %r14
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	testq	%r14, %r14
	jle	.LBB0_311
# BB#310:                               # %if.then649.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movl	8(%r15), %eax
	movl	4(%r15), %ecx
	movl	%ecx, 8(%r15)
	movl	%eax, 4(%r15)
	jmp	.LBB0_311
	.align	16, 0x90
.LBB0_303:                              # %for.end635.i
                                        #   in Loop: Header=BB0_160 Depth=1
	cmpq	$2, %rax
	jne	.LBB0_313
# BB#304:                               # %for.end635.sw.bb655_crit_edge.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movq	qty(%rip), %rbx
.LBB0_311:                              # %sw.bb655.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movslq	44(%rbx), %rdi
	callq	floor_log2_wide
	movq	qty(%rip), %rbx
	movl	48(%rbx), %ecx
	movl	60(%rbx), %edi
	imulq	%rax, %rcx
	imulq	%rcx, %rdi
	andq	%r13, %rdi
	callq	int32_to_float64
	movl	56(%rbx), %edi
	movl	52(%rbx), %ecx
	movq	%rax, %r14
	subq	%rcx, %rdi
	andq	%r13, %rdi
	callq	int32_to_float64
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rdi
	movabsq	$4621819117588971520, %r12 # imm = 0x4024000000000000
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	callq	float64_to_int64
	movslq	(%rbx), %rdi
	movq	%rax, %r14
	callq	floor_log2_wide
	movq	qty(%rip), %rbx
	movl	4(%rbx), %ecx
	movl	16(%rbx), %edi
	imulq	%rax, %rcx
	imulq	%rcx, %rdi
	andq	%r13, %rdi
	callq	int32_to_float64
	movl	12(%rbx), %edi
	movl	8(%rbx), %ecx
	movq	%rax, %rbx
	subq	%rcx, %rdi
	andq	%r13, %rdi
	callq	int32_to_float64
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	callq	float64_to_int64
	subq	%rax, %r14
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	testq	%r14, %r14
	jle	.LBB0_314
# BB#312:                               # %if.then659.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movl	(%r15), %eax
	movl	4(%r15), %ecx
	movl	%ecx, (%r15)
	movl	%eax, 4(%r15)
	jmp	.LBB0_314
.LBB0_313:                              # %sw.default666.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movq	$4, %rdx
	movq	%r15, %rdi
	movabsq	$qty_compare_1, %rcx
	callq	specqsort
	.align	16, 0x90
.LBB0_314:                              # %for.cond668.preheader.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movslq	next_qty(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_328
# BB#315:                               # %for.body671.lr.ph.i
                                        #   in Loop: Header=BB0_160 Depth=1
	movq	qty(%rip), %r10
	movq	$1, %rbx
	.align	16, 0x90
.LBB0_316:                              # %for.body671.i
                                        #   Parent Loop BB0_160 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-4(%r15,%rbx,4), %r14
	imulq	$44, %r14, %rax
	movswq	40(%r10,%rax), %rax
	testq	%rax, %rax
	jns	.LBB0_320
# BB#317:                               # %if.then680.i
                                        #   in Loop: Header=BB0_316 Depth=2
	imulq	$44, %r14, %r12
	movl	36(%r10,%r12), %esi
	movl	28(%r10,%r12), %edi
	movl	8(%r10,%r12), %r9d
	movl	12(%r10,%r12), %eax
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%rax, (%rsp)
	movq	%r14, %rdx
	callq	find_free_reg
	movq	$48, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	qty(%rip), %r10
	movq	$48, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movw	%ax, 40(%r10,%r12)
	jns	.LBB0_320
# BB#318:                               # %if.end781.i
                                        #   in Loop: Header=BB0_316 Depth=2
	movl	32(%r10,%r12), %edi
	testq	%rdi, %rdi
	je	.LBB0_320
# BB#319:                               # %if.then809.i
                                        #   in Loop: Header=BB0_316 Depth=2
	movl	8(%r10,%r12), %r9d
	movl	36(%r10,%r12), %esi
	movl	12(%r10,%r12), %eax
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%rax, (%rsp)
	movq	%r14, %rdx
	callq	find_free_reg
	movq	qty(%rip), %r10
	movw	%ax, 40(%r10,%r12)
	.align	16, 0x90
.LBB0_320:                              # %for.inc829.i
                                        #   in Loop: Header=BB0_316 Depth=2
	movslq	next_qty(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB0_316
# BB#321:                               # %for.cond832.preheader.i
                                        #   in Loop: Header=BB0_160 Depth=1
	xorq	%rsi, %rsi
	testq	%rax, %rax
	jle	.LBB0_328
	.align	16, 0x90
.LBB0_322:                              # %for.body835.i
                                        #   Parent Loop BB0_160 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_326 Depth 3
	imulq	$44, %rsi, %rcx
	movswq	40(%r10,%rcx), %rdx
	testq	%rdx, %rdx
	js	.LBB0_327
# BB#323:                               # %if.then842.i
                                        #   in Loop: Header=BB0_322 Depth=2
	movslq	24(%r10,%rcx), %rbx
	testq	%rbx, %rbx
	js	.LBB0_327
# BB#324:                               # %for.body848.lr.ph.i
                                        #   in Loop: Header=BB0_322 Depth=2
	movq	reg_offset(%rip), %r8
	movsbq	(%r8,%rbx), %r11
	movq	reg_renumber(%rip), %r9
	movq	reg_next_in_qty(%rip), %r14
	addq	%rdx, %r11
	movw	%r11w, (%r9,%rbx,2)
	movslq	(%r14,%rbx,4), %rdx
	testq	%rdx, %rdx
	js	.LBB0_327
# BB#325:                               #   in Loop: Header=BB0_322 Depth=2
	leaq	40(%r10,%rcx), %rbx
	.align	16, 0x90
.LBB0_326:                              # %for.body848.for.body848_crit_edge.i
                                        #   Parent Loop BB0_160 Depth=1
                                        #     Parent Loop BB0_322 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rbx), %edi
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movsbq	(%r8,%rdx), %rcx
	addq	%rdi, %rcx
	movw	%cx, (%r9,%rdx,2)
	movslq	(%r14,%rdx,4), %rdx
	testq	%rdx, %rdx
	jns	.LBB0_326
	.align	16, 0x90
.LBB0_327:                              # %for.inc865.i
                                        #   in Loop: Header=BB0_322 Depth=2
	incq	%rsi
	movq	%rsi, %rcx
	andq	%r13, %rcx
	movq	%rax, %rdx
	andq	%r13, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB0_322
.LBB0_328:                              # %block_alloc.exit
                                        #   in Loop: Header=BB0_160 Depth=1
	movq	regs_live_at(%rip), %rdi
	callq	free
	movq	%r15, %rdi
	callq	free
	movq	56(%rsp), %rsi          # 8-byte Reload
	incq	%rsi
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	movq	%rsi, %rbx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB0_329
.LBB0_330:                              # %for.end69
	movq	qty(%rip), %rdi
	callq	free
	movq	qty_phys_copy_sugg(%rip), %rdi
	callq	free
	movq	qty_phys_num_copy_sugg(%rip), %rdi
	callq	free
	movq	qty_phys_sugg(%rip), %rdi
	callq	free
	movq	qty_phys_num_sugg(%rip), %rdi
	callq	free
	movq	reg_qty(%rip), %rdi
	callq	free
	movq	reg_offset(%rip), %rdi
	callq	free
	movq	reg_next_in_qty(%rip), %rdi
	callq	free
	movzbl	recorded_label_ref(%rip), %eax
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_110:                              # %if.then528.i
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.update_equiv_regs, %rcx
	movq	$1078, %rsi             # imm = 0x436
	jmp	.LBB0_111
.LBB0_331:                              # %if.then2.i
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.block_alloc, %rcx
	movq	$1242, %rsi             # imm = 0x4DA
.LBB0_111:                              # %if.then528.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp17:
	.size	local_alloc, .Ltmp17-local_alloc
	.cfi_endproc

	.globl	function_invariant_p
	.align	16, 0x90
	.type	function_invariant_p,@function
function_invariant_p:                   # @function_invariant_p
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %edx
	cmpq	$133, %rdx
	movq	$1, %rax
	jg	.LBB1_3
# BB#1:                                 # %entry
	leaq	-54(%rdx), %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	ja	.LBB1_5
# BB#2:                                 # %entry
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	jne	.LBB1_16
	jmp	.LBB1_5
.LBB1_3:                                # %entry
	cmpq	$134, %rdx
	je	.LBB1_16
# BB#4:                                 # %entry
	cmpq	$140, %rdx
	je	.LBB1_16
.LBB1_5:                                # %if.end
	movq	global_rtl+24(%rip), %rcx
	cmpq	%rdi, %rcx
	je	.LBB1_16
# BB#6:                                 # %lor.lhs.false29
	movq	global_rtl+40(%rip), %rsi
	cmpq	%rdi, %rsi
	je	.LBB1_16
# BB#7:                                 # %if.end32
	cmpq	$75, %rdx
	jne	.LBB1_15
# BB#8:                                 # %land.lhs.true
	movq	8(%rdi), %rdx
	cmpq	%rcx, %rdx
	je	.LBB1_10
# BB#9:                                 # %land.lhs.true
	cmpq	%rsi, %rdx
	jne	.LBB1_15
.LBB1_10:                               # %land.lhs.true42
	movq	16(%rdi), %rcx
	movzbl	(%rcx), %edx
	cmpq	$133, %rdx
	jg	.LBB1_13
# BB#11:                                # %land.lhs.true42
	addq	$-54, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$14, %rcx
	ja	.LBB1_15
# BB#12:                                # %land.lhs.true42
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB1_16
	jmp	.LBB1_15
.LBB1_13:                               # %land.lhs.true42
	cmpq	$134, %rdx
	je	.LBB1_16
# BB#14:                                # %land.lhs.true42
	cmpq	$140, %rdx
	je	.LBB1_16
.LBB1_15:                               # %if.end99
	xorq	%rax, %rax
.LBB1_16:                               # %return
	retq
.Ltmp18:
	.size	function_invariant_p, .Ltmp18-function_invariant_p
	.cfi_endproc

	.globl	dump_local_alloc
	.align	16, 0x90
	.type	dump_local_alloc,@function
dump_local_alloc:                       # @dump_local_alloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp23:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp24:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp25:
	.cfi_def_cfa_offset 32
.Ltmp26:
	.cfi_offset %rbx, -32
.Ltmp27:
	.cfi_offset %r14, -24
.Ltmp28:
	.cfi_offset %r15, -16
	movslq	max_regno(%rip), %rax
	movq	%rdi, %r14
	cmpq	$54, %rax
	jl	.LBB2_5
# BB#1:
	movq	$53, %rbx
	movabsq	$.L.str, %r15
	.align	16, 0x90
.LBB2_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	reg_renumber(%rip), %rcx
	movzbl	(%rcx,%rbx,2), %r8d
	cmpq	$65535, %r8             # imm = 0xFFFF
	je	.LBB2_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	$48, %rcx
	shlq	%cl, %r8
	movq	$48, %rcx
	sarq	%cl, %r8
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	%r8, %rcx
	callq	fprintf
	movl	max_regno(%rip), %eax
.LBB2_4:                                # %for.inc
                                        #   in Loop: Header=BB2_2 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB2_2
.LBB2_5:                                # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp29:
	.size	dump_local_alloc, .Ltmp29-dump_local_alloc
	.cfi_endproc

	.align	16, 0x90
	.type	requires_inout,@function
requires_inout:                         # @requires_inout
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp35:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp36:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp37:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp38:
	.cfi_def_cfa_offset 40
.Ltmp39:
	.cfi_offset %rbx, -40
.Ltmp40:
	.cfi_offset %r12, -32
.Ltmp41:
	.cfi_offset %r14, -24
.Ltmp42:
	.cfi_offset %r15, -16
	movzbl	(%rdi), %r12d
	xorq	%r10, %r10
	testq	%r12, %r12
	movabsq	$0, %r11
	movabsq	$0, %rax
	je	.LBB3_72
# BB#1:                                 # %while.body.lr.ph
	movl	target_flags(%rip), %ecx
	incq	%rdi
	movq	%rcx, %r8
	andq	$33, %r8
	movq	%rcx, %r9
	andq	$327680, %r9            # imm = 0x50000
	movq	%rcx, %rax
	andq	$262144, %rax           # imm = 0x40000
	xorq	%r10, %r10
	testq	$16384, %rcx            # imm = 0x4000
	je	.LBB3_6
# BB#2:                                 # %while.body.lr.ph.while.body.lr.ph.split_crit_edge
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB3_3
# BB#55:
	movabsq	$64441745417, %r14      # imm = 0xF01070009
	xorq	%r11, %r11
	xorq	%rax, %rax
	jmp	.LBB3_56
.LBB3_6:                                # %while.body.lr.ph.split.us
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB3_7
# BB#24:
	movabsq	$64441745417, %rdx      # imm = 0xF01070009
	xorq	%r11, %r11
	xorq	%rax, %rax
	jmp	.LBB3_25
.LBB3_3:
	movabsq	$64424968201, %rdx      # imm = 0xF00070009
	xorq	%r11, %r11
	xorq	%rax, %rax
	jmp	.LBB3_4
.LBB3_7:
	movabsq	$64424968201, %rdx      # imm = 0xF00070009
	xorq	%r11, %r11
	xorq	%rax, %rax
	jmp	.LBB3_8
.LBB3_58:                               # %sw.bb1
                                        #   in Loop: Header=BB3_56 Depth=1
	xorq	$1, %rax
	orq	%r11, %rax
	xorq	$1, %rax
	addq	%rax, %r10
	xorq	%rax, %rax
	xorq	%r11, %r11
	jmp	.LBB3_71
.LBB3_59:                               # %sw.bb4
                                        #   in Loop: Header=BB3_56 Depth=1
	movq	$1, %rax
	jmp	.LBB3_71
	.align	16, 0x90
.LBB3_56:                               # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_61 Depth 2
	movq	$56, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	addq	$-33, %rdx
	andq	%r15, %rdx
	cmpq	$82, %rdx
	ja	.LBB3_62
# BB#57:                                # %while.body
                                        #   in Loop: Header=BB3_56 Depth=1
	jmpq	*.LJTI3_0(,%rdx,8)
.LBB3_60:                               # %while.cond6.preheader
                                        #   in Loop: Header=BB3_56 Depth=1
	decq	%rdi
	.align	16, 0x90
.LBB3_61:                               # %while.cond6
                                        #   Parent Loop BB3_56 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%rdi), %ecx
	movzbl	_sch_istable(%rcx,%rcx), %ecx
	incq	%rdi
	testq	$4, %rcx
	jne	.LBB3_61
	jmp	.LBB3_71
	.align	16, 0x90
.LBB3_62:                               # %sw.default
                                        #   in Loop: Header=BB3_56 Depth=1
	movq	$56, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	cmpq	$112, %rdx
	jg	.LBB3_68
# BB#63:                                # %sw.default
                                        #   in Loop: Header=BB3_56 Depth=1
	addq	$-65, %r12
	andq	$255, %r12
	cmpq	$37, %r12
	ja	.LBB3_71
# BB#64:                                # %sw.default
                                        #   in Loop: Header=BB3_56 Depth=1
	movq	$1, %rdx
	movq	%r12, %rcx
	shlq	%cl, %rdx
	testq	%r14, %rdx
	jne	.LBB3_67
# BB#65:                                # %sw.default
                                        #   in Loop: Header=BB3_56 Depth=1
	cmpq	$37, %r12
	jne	.LBB3_71
	jmp	.LBB3_66
	.align	16, 0x90
.LBB3_68:                               # %sw.default
                                        #   in Loop: Header=BB3_56 Depth=1
	addq	$-113, %r12
	andq	$255, %r12
	cmpq	$8, %r12
	ja	.LBB3_71
# BB#69:                                # %sw.default
                                        #   in Loop: Header=BB3_56 Depth=1
	jmpq	*.LJTI3_1(,%r12,8)
.LBB3_66:                               # %cond.true57
                                        #   in Loop: Header=BB3_56 Depth=1
	testq	%r15, %r8
	je	.LBB3_71
	jmp	.LBB3_67
.LBB3_70:                               # %cond.true89
                                        #   in Loop: Header=BB3_56 Depth=1
	testq	%r15, %r9
	je	.LBB3_71
	.align	16, 0x90
.LBB3_67:                               # %sw.bb162
                                        #   in Loop: Header=BB3_56 Depth=1
	movq	$1, %r11
.LBB3_71:                               # %while.cond.backedge
                                        #   in Loop: Header=BB3_56 Depth=1
	movzbl	(%rdi), %r12d
	incq	%rdi
	testq	%r12, %r12
	jne	.LBB3_56
	jmp	.LBB3_72
.LBB3_30:                               # %sw.bb1.us
                                        #   in Loop: Header=BB3_25 Depth=1
	xorq	$1, %rax
	orq	%r11, %rax
	xorq	$1, %rax
	addq	%rax, %r10
	xorq	%rax, %rax
	xorq	%r11, %r11
	jmp	.LBB3_40
.LBB3_29:                               # %sw.bb4.us
                                        #   in Loop: Header=BB3_25 Depth=1
	movq	$1, %rax
	jmp	.LBB3_40
	.align	16, 0x90
.LBB3_25:                               # %while.body.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_28 Depth 2
	movq	$56, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	addq	$-33, %rsi
	andq	%rbx, %rsi
	cmpq	$82, %rsi
	ja	.LBB3_31
# BB#26:                                # %while.body.us
                                        #   in Loop: Header=BB3_25 Depth=1
	jmpq	*.LJTI3_4(,%rsi,8)
.LBB3_27:                               # %while.cond6.us.preheader
                                        #   in Loop: Header=BB3_25 Depth=1
	decq	%rdi
	.align	16, 0x90
.LBB3_28:                               # %while.cond6.us
                                        #   Parent Loop BB3_25 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%rdi), %ecx
	movzbl	_sch_istable(%rcx,%rcx), %ecx
	incq	%rdi
	testq	$4, %rcx
	jne	.LBB3_28
	jmp	.LBB3_40
	.align	16, 0x90
.LBB3_31:                               # %sw.default.us
                                        #   in Loop: Header=BB3_25 Depth=1
	movq	$56, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	cmpq	$112, %rsi
	jg	.LBB3_36
# BB#32:                                # %sw.default.us
                                        #   in Loop: Header=BB3_25 Depth=1
	addq	$-65, %r12
	andq	$255, %r12
	cmpq	$37, %r12
	ja	.LBB3_40
# BB#33:                                # %sw.default.us
                                        #   in Loop: Header=BB3_25 Depth=1
	movq	$1, %rsi
	movq	%r12, %rcx
	shlq	%cl, %rsi
	testq	%rdx, %rsi
	jne	.LBB3_39
# BB#34:                                # %sw.default.us
                                        #   in Loop: Header=BB3_25 Depth=1
	cmpq	$37, %r12
	jne	.LBB3_40
	jmp	.LBB3_35
	.align	16, 0x90
.LBB3_36:                               # %sw.default.us
                                        #   in Loop: Header=BB3_25 Depth=1
	addq	$-113, %r12
	andq	$255, %r12
	cmpq	$7, %r12
	ja	.LBB3_40
# BB#37:                                # %sw.default.us
                                        #   in Loop: Header=BB3_25 Depth=1
	jmpq	*.LJTI3_5(,%r12,8)
.LBB3_35:                               # %cond.true45.us
                                        #   in Loop: Header=BB3_25 Depth=1
	testq	%rbx, %r8
	jne	.LBB3_39
	jmp	.LBB3_40
.LBB3_38:                               # %cond.true89.us
                                        #   in Loop: Header=BB3_25 Depth=1
	testq	%rbx, %r9
	je	.LBB3_40
	.align	16, 0x90
.LBB3_39:                               # %sw.bb162.us
                                        #   in Loop: Header=BB3_25 Depth=1
	movq	$1, %r11
.LBB3_40:                               # %while.cond.backedge.us
                                        #   in Loop: Header=BB3_25 Depth=1
	movzbl	(%rdi), %r12d
	incq	%rdi
	testq	%r12, %r12
	jne	.LBB3_25
	jmp	.LBB3_72
.LBB3_44:                               # %sw.bb1.us235
                                        #   in Loop: Header=BB3_4 Depth=1
	xorq	$1, %rax
	orq	%r11, %rax
	xorq	$1, %rax
	addq	%rax, %r10
	xorq	%rax, %rax
	xorq	%r11, %r11
	jmp	.LBB3_54
.LBB3_43:                               # %sw.bb4.us234
                                        #   in Loop: Header=BB3_4 Depth=1
	movq	$1, %rax
	jmp	.LBB3_54
	.align	16, 0x90
.LBB3_4:                                # %while.body.us221
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_42 Depth 2
	movq	$56, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$56, %rcx
	sarq	%cl, %rbx
	addq	$-33, %rbx
	andq	%r15, %rbx
	cmpq	$82, %rbx
	ja	.LBB3_45
# BB#5:                                 # %while.body.us221
                                        #   in Loop: Header=BB3_4 Depth=1
	jmpq	*.LJTI3_2(,%rbx,8)
.LBB3_41:                               # %while.cond6.us227.preheader
                                        #   in Loop: Header=BB3_4 Depth=1
	decq	%rdi
	.align	16, 0x90
.LBB3_42:                               # %while.cond6.us227
                                        #   Parent Loop BB3_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%rdi), %ecx
	movzbl	_sch_istable(%rcx,%rcx), %ecx
	incq	%rdi
	testq	$4, %rcx
	jne	.LBB3_42
	jmp	.LBB3_54
	.align	16, 0x90
.LBB3_45:                               # %sw.default.us238
                                        #   in Loop: Header=BB3_4 Depth=1
	movq	$56, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	cmpq	$112, %rsi
	jg	.LBB3_50
# BB#46:                                # %sw.default.us238
                                        #   in Loop: Header=BB3_4 Depth=1
	addq	$-65, %r12
	andq	$255, %r12
	cmpq	$37, %r12
	ja	.LBB3_54
# BB#47:                                # %sw.default.us238
                                        #   in Loop: Header=BB3_4 Depth=1
	movq	$1, %rsi
	movq	%r12, %rcx
	shlq	%cl, %rsi
	testq	%rdx, %rsi
	jne	.LBB3_53
# BB#48:                                # %sw.default.us238
                                        #   in Loop: Header=BB3_4 Depth=1
	cmpq	$37, %r12
	jne	.LBB3_54
	jmp	.LBB3_49
	.align	16, 0x90
.LBB3_50:                               # %sw.default.us238
                                        #   in Loop: Header=BB3_4 Depth=1
	addq	$-113, %r12
	andq	$255, %r12
	cmpq	$8, %r12
	ja	.LBB3_54
# BB#51:                                # %sw.default.us238
                                        #   in Loop: Header=BB3_4 Depth=1
	jmpq	*.LJTI3_3(,%r12,8)
.LBB3_49:                               # %cond.true45.us243
                                        #   in Loop: Header=BB3_4 Depth=1
	testq	%r15, %r8
	jne	.LBB3_53
	jmp	.LBB3_54
.LBB3_52:                               # %cond.true89.us241
                                        #   in Loop: Header=BB3_4 Depth=1
	testq	%r15, %r9
	je	.LBB3_54
	.align	16, 0x90
.LBB3_53:                               # %sw.bb162.us245
                                        #   in Loop: Header=BB3_4 Depth=1
	movq	$1, %r11
.LBB3_54:                               # %while.cond.backedge.us249
                                        #   in Loop: Header=BB3_4 Depth=1
	movzbl	(%rdi), %r12d
	incq	%rdi
	testq	%r12, %r12
	jne	.LBB3_4
	jmp	.LBB3_72
.LBB3_11:                               # %sw.bb1.us.us
                                        #   in Loop: Header=BB3_8 Depth=1
	xorq	$1, %rax
	orq	%r11, %rax
	xorq	$1, %rax
	addq	%rax, %r10
	xorq	%rax, %rax
	xorq	%r11, %r11
	jmp	.LBB3_21
.LBB3_10:                               # %sw.bb4.us.us
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	$1, %rax
	jmp	.LBB3_21
	.align	16, 0x90
.LBB3_8:                                # %while.body.us.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_23 Depth 2
	movq	$56, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	addq	$-33, %rsi
	andq	%rbx, %rsi
	cmpq	$82, %rsi
	ja	.LBB3_12
# BB#9:                                 # %while.body.us.us
                                        #   in Loop: Header=BB3_8 Depth=1
	jmpq	*.LJTI3_6(,%rsi,8)
.LBB3_22:                               # %while.cond6.us.us.preheader
                                        #   in Loop: Header=BB3_8 Depth=1
	decq	%rdi
	.align	16, 0x90
.LBB3_23:                               # %while.cond6.us.us
                                        #   Parent Loop BB3_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%rdi), %ecx
	movzbl	_sch_istable(%rcx,%rcx), %ecx
	incq	%rdi
	testq	$4, %rcx
	jne	.LBB3_23
	jmp	.LBB3_21
	.align	16, 0x90
.LBB3_12:                               # %sw.default.us.us
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	$56, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	cmpq	$112, %rsi
	jg	.LBB3_17
# BB#13:                                # %sw.default.us.us
                                        #   in Loop: Header=BB3_8 Depth=1
	addq	$-65, %r12
	andq	$255, %r12
	cmpq	$37, %r12
	ja	.LBB3_21
# BB#14:                                # %sw.default.us.us
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	$1, %rsi
	movq	%r12, %rcx
	shlq	%cl, %rsi
	testq	%rdx, %rsi
	jne	.LBB3_20
# BB#15:                                # %sw.default.us.us
                                        #   in Loop: Header=BB3_8 Depth=1
	cmpq	$37, %r12
	jne	.LBB3_21
	jmp	.LBB3_16
	.align	16, 0x90
.LBB3_17:                               # %sw.default.us.us
                                        #   in Loop: Header=BB3_8 Depth=1
	addq	$-113, %r12
	andq	$255, %r12
	cmpq	$7, %r12
	ja	.LBB3_21
# BB#18:                                # %sw.default.us.us
                                        #   in Loop: Header=BB3_8 Depth=1
	jmpq	*.LJTI3_7(,%r12,8)
.LBB3_16:                               # %cond.true45.us.us
                                        #   in Loop: Header=BB3_8 Depth=1
	testq	%rbx, %r8
	jne	.LBB3_20
	jmp	.LBB3_21
.LBB3_19:                               # %cond.true89.us.us
                                        #   in Loop: Header=BB3_8 Depth=1
	testq	%rbx, %r9
	je	.LBB3_21
	.align	16, 0x90
.LBB3_20:                               # %sw.bb162.us.us
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	$1, %r11
.LBB3_21:                               # %while.cond.backedge.us.us
                                        #   in Loop: Header=BB3_8 Depth=1
	movzbl	(%rdi), %r12d
	incq	%rdi
	testq	%r12, %r12
	jne	.LBB3_8
.LBB3_72:                               # %while.end163
	xorq	$1, %rax
	orq	%r11, %rax
	xorq	$1, %rax
	addq	%r10, %rax
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp43:
	.size	requires_inout, .Ltmp43-requires_inout
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI3_0:
	.quad	.LBB3_71
	.quad	.LBB3_62
	.quad	.LBB3_71
	.quad	.LBB3_62
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_58
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_59
	.quad	.LBB3_60
	.quad	.LBB3_60
	.quad	.LBB3_60
	.quad	.LBB3_60
	.quad	.LBB3_60
	.quad	.LBB3_60
	.quad	.LBB3_60
	.quad	.LBB3_60
	.quad	.LBB3_60
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_71
	.quad	.LBB3_62
	.quad	.LBB3_71
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_67
	.quad	.LBB3_62
	.quad	.LBB3_71
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_62
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_67
	.quad	.LBB3_62
	.quad	.LBB3_67
	.quad	.LBB3_71
.LJTI3_1:
	.quad	.LBB3_67
	.quad	.LBB3_67
	.quad	.LBB3_71
	.quad	.LBB3_66
	.quad	.LBB3_66
	.quad	.LBB3_71
	.quad	.LBB3_71
	.quad	.LBB3_70
	.quad	.LBB3_67
.LJTI3_2:
	.quad	.LBB3_54
	.quad	.LBB3_45
	.quad	.LBB3_54
	.quad	.LBB3_45
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_44
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_43
	.quad	.LBB3_41
	.quad	.LBB3_41
	.quad	.LBB3_41
	.quad	.LBB3_41
	.quad	.LBB3_41
	.quad	.LBB3_41
	.quad	.LBB3_41
	.quad	.LBB3_41
	.quad	.LBB3_41
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_54
	.quad	.LBB3_45
	.quad	.LBB3_54
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_53
	.quad	.LBB3_45
	.quad	.LBB3_54
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_45
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_53
	.quad	.LBB3_45
	.quad	.LBB3_53
	.quad	.LBB3_54
.LJTI3_3:
	.quad	.LBB3_53
	.quad	.LBB3_53
	.quad	.LBB3_54
	.quad	.LBB3_49
	.quad	.LBB3_49
	.quad	.LBB3_54
	.quad	.LBB3_54
	.quad	.LBB3_52
	.quad	.LBB3_53
.LJTI3_4:
	.quad	.LBB3_40
	.quad	.LBB3_31
	.quad	.LBB3_40
	.quad	.LBB3_31
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_30
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_29
	.quad	.LBB3_27
	.quad	.LBB3_27
	.quad	.LBB3_27
	.quad	.LBB3_27
	.quad	.LBB3_27
	.quad	.LBB3_27
	.quad	.LBB3_27
	.quad	.LBB3_27
	.quad	.LBB3_27
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_40
	.quad	.LBB3_31
	.quad	.LBB3_40
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_39
	.quad	.LBB3_31
	.quad	.LBB3_40
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_31
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_39
	.quad	.LBB3_31
	.quad	.LBB3_39
	.quad	.LBB3_40
.LJTI3_5:
	.quad	.LBB3_39
	.quad	.LBB3_39
	.quad	.LBB3_40
	.quad	.LBB3_35
	.quad	.LBB3_35
	.quad	.LBB3_40
	.quad	.LBB3_40
	.quad	.LBB3_38
.LJTI3_6:
	.quad	.LBB3_21
	.quad	.LBB3_12
	.quad	.LBB3_21
	.quad	.LBB3_12
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_11
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_10
	.quad	.LBB3_22
	.quad	.LBB3_22
	.quad	.LBB3_22
	.quad	.LBB3_22
	.quad	.LBB3_22
	.quad	.LBB3_22
	.quad	.LBB3_22
	.quad	.LBB3_22
	.quad	.LBB3_22
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_21
	.quad	.LBB3_12
	.quad	.LBB3_21
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_20
	.quad	.LBB3_12
	.quad	.LBB3_21
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_12
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_20
	.quad	.LBB3_12
	.quad	.LBB3_20
	.quad	.LBB3_21
.LJTI3_7:
	.quad	.LBB3_20
	.quad	.LBB3_20
	.quad	.LBB3_21
	.quad	.LBB3_16
	.quad	.LBB3_16
	.quad	.LBB3_21
	.quad	.LBB3_21
	.quad	.LBB3_19

	.text
	.align	16, 0x90
	.type	combine_regs,@function
combine_regs:                           # @combine_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp50:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp51:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp52:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp53:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp54:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp55:
	.cfi_def_cfa_offset 128
.Ltmp56:
	.cfi_offset %rbx, -48
.Ltmp57:
	.cfi_offset %r12, -40
.Ltmp58:
	.cfi_offset %r13, -32
.Ltmp59:
	.cfi_offset %r14, -24
.Ltmp60:
	.cfi_offset %r15, -16
	movq	%r9, 40(%rsp)           # 8-byte Spill
	movq	%r8, 64(%rsp)           # 8-byte Spill
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	%rdx, %r11
	movq	%rsi, %r15
	movq	%rdi, %rbx
	xorq	%r14, %r14
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	jmp	.LBB4_1
	.align	16, 0x90
.LBB4_10:                               # %while.cond.outer
                                        #   in Loop: Header=BB4_1 Depth=1
	addq	%rax, %r14
	movq	%r12, %r11
.LBB4_1:                                # %while.cond.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_2 Depth 2
	movl	(%rbx), %esi
	.align	16, 0x90
.LBB4_2:                                # %while.cond
                                        #   Parent Loop BB4_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %r13
	movq	%rsi, %r8
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	je	.LBB4_11
# BB#3:                                 # %while.cond
                                        #   in Loop: Header=BB4_2 Depth=2
	xorq	%rbx, %rbx
	cmpq	$63, %rax
	jne	.LBB4_136
# BB#4:                                 # %while.body
                                        #   in Loop: Header=BB4_2 Depth=2
	movq	8(%r13), %rbx
	movl	(%rbx), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB4_2
# BB#5:                                 # %if.then
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rsi
	movl	target_flags(%rip), %r9d
	andq	$255, %rsi
	movzbl	mode_size(%rsi), %eax
	movq	$23, %rcx
	shrq	%cl, %r9
	andq	$4, %r9
	addq	$4, %r9
	movq	%r9, %rcx
	movabsq	$4294967292, %rdx       # imm = 0xFFFFFFFC
	andq	%rdx, %rcx
	xorq	%r12, %r12
	cmpq	%rcx, %rax
	ja	.LBB4_7
# BB#6:                                 # %if.then
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	%r11, %r12
.LBB4_7:                                # %if.then
                                        #   in Loop: Header=BB4_1 Depth=1
	movl	8(%rbx), %edi
	movl	16(%r13), %eax
	cmpq	$52, %rdi
	ja	.LBB4_9
# BB#8:                                 # %if.then14
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	movq	%rax, %rdx
	movq	%r8, %rcx
	movq	%r10, %r13
	callq	subreg_regno_offset
	movq	%r13, %r10
	jmp	.LBB4_10
	.align	16, 0x90
.LBB4_9:                                # %if.else
                                        #   in Loop: Header=BB4_1 Depth=1
	andq	%r10, %rax
	andq	%r10, %r9
	xorq	%rdx, %rdx
	divq	%r9
	jmp	.LBB4_10
.LBB4_11:                               # %if.end41
	movl	8(%r13), %r9d
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	cmpq	$52, %rax
	jg	.LBB4_26
# BB#12:                                # %if.then47
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%r9, %rax
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	cmpq	$8, %rax
	je	.LBB4_16
# BB#13:                                # %if.then47
	leaq	-21(%r9), %rax
	andq	%r10, %rax
	cmpq	$8, %rax
	jb	.LBB4_16
# BB#14:                                # %if.then47
	leaq	-45(%r9), %rax
	andq	%r10, %rax
	cmpq	$8, %rax
	jb	.LBB4_16
# BB#15:                                # %if.then47
	leaq	-29(%r9), %rax
	andq	%r10, %rax
	cmpq	$7, %rax
	jbe	.LBB4_16
# BB#21:                                # %cond.false
	cmpq	$24, %r8
	jne	.LBB4_22
# BB#24:                                # %cond.true99
	movq	%r14, 72(%rsp)          # 8-byte Spill
	movzbl	target_flags+3(%rip), %edi
	andq	$2, %rdi
	xorq	$6, %rdi
	jmp	.LBB4_28
.LBB4_26:                               # %if.else123
	movq	%r14, 72(%rsp)          # 8-byte Spill
	movq	$16, %rcx
	shrq	%cl, %r8
	movl	target_flags(%rip), %esi
	andq	$255, %r8
	movzbl	mode_size(%r8), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rsi,%rax), %rax
.LBB4_27:                               # %while.cond140.preheader
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %rdi
	jmp	.LBB4_28
.LBB4_16:                               # %cond.true
	movq	%r14, 72(%rsp)          # 8-byte Spill
	movl	mode_class(,%r8,4), %eax
	cmpq	$5, %rax
	movq	$2, %rdi
	je	.LBB4_28
# BB#17:                                # %lor.rhs
	cmpq	$6, %rax
	jne	.LBB4_18
# BB#19:                                # %lor.rhs
	movabsq	$1, %rdi
	incq	%rdi
	jmp	.LBB4_28
.LBB4_22:                               # %cond.false
	movq	%r14, 72(%rsp)          # 8-byte Spill
	cmpq	$18, %r8
	jne	.LBB4_25
# BB#23:                                # %cond.true89
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	xorq	$3, %rdi
	jmp	.LBB4_28
.LBB4_18:
	movabsq	$0, %rdi
	incq	%rdi
.LBB4_28:                               # %while.cond140.preheader
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	jmp	.LBB4_29
	.align	16, 0x90
.LBB4_38:                               # %while.cond140.outer
                                        #   in Loop: Header=BB4_29 Depth=1
	subq	%rax, 72(%rsp)          # 8-byte Folded Spill
	movq	%r14, %r11
	movl	8(%r13), %r9d
	movq	56(%rsp), %rdi          # 8-byte Reload
.LBB4_29:                               # %while.cond140.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_30 Depth 2
	movl	(%r15), %esi
	movq	72(%rsp), %r14          # 8-byte Reload
	.align	16, 0x90
.LBB4_30:                               # %while.cond140
                                        #   Parent Loop BB4_29 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %r12
	movq	%rsi, %r8
	xorq	%rbx, %rbx
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	je	.LBB4_39
# BB#31:                                # %while.cond140
                                        #   in Loop: Header=BB4_30 Depth=2
	cmpq	$63, %rax
	jne	.LBB4_136
# BB#32:                                # %while.body145
                                        #   in Loop: Header=BB4_30 Depth=2
	movq	8(%r12), %r15
	movl	(%r15), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB4_30
# BB#33:                                # %if.then154
                                        #   in Loop: Header=BB4_29 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rsi
	movl	target_flags(%rip), %ebx
	andq	$255, %rsi
	movzbl	mode_size(%rsi), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	addq	$4, %rbx
	movq	%rbx, %rcx
	movabsq	$4294967292, %rdx       # imm = 0xFFFFFFFC
	andq	%rdx, %rcx
	xorq	%r14, %r14
	cmpq	%rcx, %rax
	ja	.LBB4_35
# BB#34:                                # %if.then154
                                        #   in Loop: Header=BB4_29 Depth=1
	movq	%r11, %r14
.LBB4_35:                               # %if.then154
                                        #   in Loop: Header=BB4_29 Depth=1
	movl	8(%r15), %edi
	movl	16(%r12), %eax
	cmpq	$52, %rdi
	ja	.LBB4_37
# BB#36:                                # %if.then173
                                        #   in Loop: Header=BB4_29 Depth=1
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	movq	%rax, %rdx
	movq	%r8, %rcx
	movq	%r10, %rbx
	callq	subreg_regno_offset
	movq	%rbx, %r10
	jmp	.LBB4_38
	.align	16, 0x90
.LBB4_37:                               # %if.else188
                                        #   in Loop: Header=BB4_29 Depth=1
	andq	%r10, %rax
	andq	%r10, %rbx
	xorq	%rdx, %rdx
	divq	%rbx
	jmp	.LBB4_38
.LBB4_39:                               # %if.end205
	movl	8(%r12), %esi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	cmpq	$52, %rax
	jg	.LBB4_54
# BB#40:                                # %if.then211
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rsi, %rdx
	movq	$16, %rcx
	movq	%r8, %rax
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB4_44
# BB#41:                                # %if.then211
	leaq	-21(%rsi), %rcx
	andq	%r10, %rcx
	cmpq	$8, %rcx
	jb	.LBB4_44
# BB#42:                                # %if.then211
	leaq	-45(%rsi), %rcx
	andq	%r10, %rcx
	cmpq	$8, %rcx
	jb	.LBB4_44
# BB#43:                                # %if.then211
	leaq	-29(%rsi), %rcx
	andq	%r10, %rcx
	cmpq	$7, %rcx
	jbe	.LBB4_44
# BB#49:                                # %cond.false253
	cmpq	$24, %rax
	jne	.LBB4_50
# BB#52:                                # %cond.true269
	movzbl	target_flags+3(%rip), %esi
	andq	$2, %rsi
	xorq	$6, %rsi
	jmp	.LBB4_56
.LBB4_54:                               # %if.else295
	movq	$16, %rcx
	movq	%r8, %rax
	shrq	%cl, %rax
	movl	target_flags(%rip), %esi
	andq	$255, %rax
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rsi,%rax), %rax
.LBB4_55:                               # %if.end311
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %rsi
	jmp	.LBB4_56
.LBB4_44:                               # %cond.true235
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %rsi
	je	.LBB4_56
# BB#45:                                # %lor.rhs243
	cmpq	$6, %rax
	jne	.LBB4_46
# BB#47:                                # %lor.rhs243
	movabsq	$1, %rsi
	incq	%rsi
	jmp	.LBB4_56
.LBB4_50:                               # %cond.false253
	cmpq	$18, %rax
	jne	.LBB4_53
# BB#51:                                # %cond.true259
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	xorq	$3, %rsi
	jmp	.LBB4_56
.LBB4_25:                               # %cond.false103
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%r8), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	jmp	.LBB4_27
.LBB4_46:
	movabsq	$0, %rsi
	incq	%rsi
.LBB4_56:                               # %if.end311
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jl	.LBB4_58
# BB#57:                                # %land.lhs.true314
	movq	reg_qty(%rip), %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movslq	(%rax,%rcx,4), %rax
	testq	%rax, %rax
	js	.LBB4_136
.LBB4_58:                               # %lor.lhs.false319
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	(%rdi,%r14), %rax
	testq	%rdx, %rdx
	jle	.LBB4_60
# BB#59:                                # %lor.lhs.false319
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rsi, %r15
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	movq	%r15, %rsi
	jg	.LBB4_136
.LBB4_60:                               # %lor.lhs.false326
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jns	.LBB4_62
# BB#61:                                # %lor.lhs.false326
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB4_136
.LBB4_62:                               # %lor.lhs.false333
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	%r11, %r15
	jle	.LBB4_65
# BB#63:                                # %lor.lhs.false333
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jl	.LBB4_65
# BB#64:                                # %land.lhs.true339
	movq	reg_qty(%rip), %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movslq	(%rax,%rcx,4), %rax
	movq	qty(%rip), %rcx
	imulq	$44, %rax, %rax
	movslq	16(%rcx,%rax), %rax
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB4_136
.LBB4_65:                               # %lor.lhs.false346
	movq	$32, %rcx
	movl	8(%r12), %edx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jl	.LBB4_67
# BB#66:                                # %land.lhs.true349
	movq	reg_qty(%rip), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	movl	(%rax,%rcx,4), %eax
	cmpq	%r10, %rax
	je	.LBB4_136
.LBB4_67:                               # %lor.lhs.false354
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jl	.LBB4_76
# BB#68:                                # %land.lhs.true357
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	movabsq	$0, %rsi
	movabsq	$1, %rax
	movq	%rsi, %rcx
	jge	.LBB4_70
# BB#69:                                # %land.lhs.true357
	movq	%rax, %rcx
.LBB4_70:                               # %land.lhs.true357
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	movq	%r15, (%rsp)            # 8-byte Spill
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$53, %rdx
	jge	.LBB4_72
# BB#71:                                # %land.lhs.true357
	movq	%rax, %rsi
.LBB4_72:                               # %land.lhs.true357
	movq	%rsi, %r15
	movq	$9, %rsi
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB4_136
# BB#73:                                # %land.lhs.true357
	movl	8(%r13), %r9d
	movl	8(%r12), %esi
	cmpq	%rsi, %r9
	movq	8(%rsp), %rax           # 8-byte Reload
	je	.LBB4_136
# BB#74:                                # %land.lhs.true357
	andq	%rax, %r15
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	jne	.LBB4_136
# BB#75:                                # %land.lhs.true357.lor.lhs.false369_crit_edge
	movl	(%r12), %r8d
	movq	(%rsp), %r15            # 8-byte Reload
	jmp	.LBB4_82
.LBB4_53:                               # %cond.false273
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	jmp	.LBB4_55
.LBB4_76:                               # %lor.lhs.false360
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rdx, %rax
	movq	%rdx, %rsi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	movabsq	$0, %rax
	movabsq	$1, %r11
	movq	%rax, %rdx
	jge	.LBB4_78
# BB#77:                                # %lor.lhs.false360
	movq	%r11, %rdx
.LBB4_78:                               # %lor.lhs.false360
	movq	$32, %rcx
	movq	%r9, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$53, %rdi
	jge	.LBB4_80
# BB#79:                                # %lor.lhs.false360
	movq	%r11, %rax
.LBB4_80:                               # %lor.lhs.false360
	cmpq	%rsi, %r9
	je	.LBB4_136
# BB#81:                                # %lor.lhs.false360
	andq	%rdx, %rax
	jne	.LBB4_136
.LBB4_82:                               # %lor.lhs.false369
	movq	%rsi, %rdi
	movl	(%r13), %eax
	movq	$16, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	$16, %rcx
	movq	%r8, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	cmpq	%rdx, %rsi
	je	.LBB4_95
# BB#83:                                # %lor.lhs.false378
	leaq	-3(%rsi), %rcx
	andq	%r10, %rcx
	cmpq	$2, %rcx
	jb	.LBB4_89
# BB#84:                                # %lor.lhs.false378
	cmpq	$2, %rsi
	jne	.LBB4_87
# BB#85:                                # %land.lhs.true396
	movzbl	target_flags+3(%rip), %ecx
	testq	$2, %rcx
	jne	.LBB4_89
# BB#86:                                # %lor.lhs.false399
	movl	ix86_cpu(%rip), %ecx
	movl	x86_partial_reg_stall(%rip), %r11d
	movq	$1, %rsi
	shlq	%cl, %rsi
	andq	%r11, %rsi
	testq	%r10, %rsi
	je	.LBB4_89
.LBB4_87:                               # %lor.lhs.false402
	movq	%rax, %rcx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$327680, %rcx           # imm = 0x50000
	jne	.LBB4_136
# BB#88:                                # %land.lhs.true408
	movzbl	target_flags+3(%rip), %ecx
	testq	$2, %rcx
	je	.LBB4_136
.LBB4_89:                               # %land.lhs.true411
	addq	$-3, %rdx
	andq	%r10, %rdx
	cmpq	$2, %rdx
	jb	.LBB4_95
# BB#90:                                # %lor.lhs.false423
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$131072, %rax           # imm = 0x20000
	jne	.LBB4_93
# BB#91:                                # %land.lhs.true429
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	jne	.LBB4_95
# BB#92:                                # %lor.lhs.false432
	movl	ix86_cpu(%rip), %ecx
	movl	x86_partial_reg_stall(%rip), %eax
	movq	$1, %rdx
	shlq	%cl, %rdx
	andq	%rax, %rdx
	testq	%r10, %rdx
	je	.LBB4_95
.LBB4_93:                               # %lor.lhs.false436
	andq	$16711680, %r8          # imm = 0xFF0000
	cmpq	$327680, %r8            # imm = 0x50000
	jne	.LBB4_136
# BB#94:                                # %land.lhs.true442
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	je	.LBB4_136
.LBB4_95:                               # %if.end446
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	jg	.LBB4_106
# BB#96:                                # %if.then449
	movq	%r15, %r8
	movq	reg_qty(%rip), %rax
	movq	24(%rsp), %rdx          # 8-byte Reload
	movl	(%rax,%rdx,4), %eax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	cmpq	%rcx, %rax
	jne	.LBB4_98
# BB#97:                                # %if.then454
	movq	48(%rsp), %rsi          # 8-byte Reload
	addq	%rsi, %rsi
	movq	%r12, %rdi
	movq	%r10, %r14
	movq	%r9, %r12
	movq	%rdx, %r15
	movq	%r8, %r13
	callq	reg_is_born
	movq	%r13, %r8
	movq	%r12, %r9
	movq	%r14, %r10
	movq	reg_qty(%rip), %rax
	movl	(%rax,%r15,4), %eax
.LBB4_98:                               # %if.end455
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB4_136
# BB#99:                                # %if.then460
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%r10, %r8
	je	.LBB4_100
# BB#103:                               # %land.lhs.true462
	movq	qty_phys_copy_sugg(%rip), %rsi
	movq	(%rsi,%rax,8), %rdi
	movq	$1, %rdx
	movq	%r9, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rdi
	jne	.LBB4_101
# BB#104:                               # %if.then470
	orq	%rdx, %rdi
	movq	%rdi, (%rsi,%rax,8)
	movq	qty_phys_num_copy_sugg(%rip), %rcx
	jmp	.LBB4_105
.LBB4_106:                              # %if.end506
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	movq	24(%rsp), %rsi          # 8-byte Reload
	jg	.LBB4_113
# BB#107:                               # %if.then509
	movq	%rdi, %rcx
	testq	%r10, %r15
	je	.LBB4_108
# BB#111:                               # %land.lhs.true511
	movq	reg_qty(%rip), %rdx
	movq	32(%rsp), %rax          # 8-byte Reload
	movslq	(%rdx,%rax,4), %rsi
	movq	qty_phys_copy_sugg(%rip), %r8
	movq	(%r8,%rsi,8), %rdi
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%rax, %rdi
	jne	.LBB4_109
# BB#112:                               # %if.then520
	orq	%rax, %rdi
	movq	%rdi, (%r8,%rsi,8)
	movq	qty_phys_num_copy_sugg(%rip), %rax
	movzbl	(%rax,%rsi,2), %ecx
	incq	%rcx
	movw	%cx, (%rax,%rsi,2)
	jmp	.LBB4_136
.LBB4_113:                              # %if.end557
	movq	reg_qty(%rip), %rax
	movslq	(%rax,%rsi,4), %rcx
	cmpq	$-2, %rcx
	jg	.LBB4_136
# BB#114:                               # %lor.lhs.false562
	movq	cfun(%rip), %rcx
	movzbl	425(%rcx), %ecx
	testq	$1, %rcx
	je	.LBB4_120
# BB#115:                               # %land.lhs.true567
	movq	%rdi, %r11
	movq	reg_n_info(%rip), %rdi
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	32(%rdi,%rcx,8), %rcx
	movslq	32(%rcx), %rcx
	testq	%rcx, %rcx
	movabsq	$0, %rcx
	movabsq	$1, %r8
	movq	%rcx, %rdx
	jle	.LBB4_117
# BB#116:                               # %land.lhs.true567
	movq	%r8, %rdx
.LBB4_117:                              # %land.lhs.true567
	movq	32(%rdi,%rsi,8), %rdi
	movslq	32(%rdi), %rdi
	testq	%rdi, %rdi
	jle	.LBB4_119
# BB#118:                               # %land.lhs.true567
	movq	%r8, %rcx
.LBB4_119:                              # %land.lhs.true567
	xorq	%rcx, %rdx
	movq	%r11, %rdi
	jne	.LBB4_136
.LBB4_120:                              # %if.end582
	movq	40(%rsp), %rcx          # 8-byte Reload
	testq	%r10, %rcx
	movq	%r10, %r13
	jne	.LBB4_123
# BB#121:                               # %lor.lhs.false584
	movq	$1, %rsi
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r9, %rdx
	callq	find_regno_note
	testq	%rax, %rax
	je	.LBB4_136
# BB#122:                               # %lor.lhs.false584.land.lhs.true587_crit_edge
	movq	reg_qty(%rip), %rax
	movl	8(%r12), %edi
.LBB4_123:                              # %land.lhs.true587
	movq	32(%rsp), %rcx          # 8-byte Reload
	movslq	(%rax,%rcx,4), %rax
	movq	qty(%rip), %rcx
	imulq	$44, %rax, %rax
	movl	28(%rcx,%rax), %r15d
	callq	reg_preferred_class
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	reg_class_subset_p
	testq	%r13, %rax
	movq	%r13, %r8
	jne	.LBB4_125
# BB#124:                               # %lor.rhs.i
	movq	%r15, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	%r8, %r13
	callq	reg_class_subset_p
	movq	%r13, %r8
	testq	%r8, %rax
	je	.LBB4_136
.LBB4_125:                              # %if.then594
	movq	reg_qty(%rip), %rax
	movq	32(%rsp), %rdx          # 8-byte Reload
	movslq	(%rax,%rdx,4), %rcx
	movq	24(%rsp), %rsi          # 8-byte Reload
	movl	%ecx, (%rax,%rsi,4)
	movq	reg_offset(%rip), %rax
	movzbl	(%rax,%rdx), %edx
	addq	%r14, %rdx
	movb	%dl, (%rax,%rsi)
	movq	qty(%rip), %rax
	imulq	$44, %rcx, %r13
	movl	24(%rax,%r13), %ecx
	movq	reg_next_in_qty(%rip), %rdx
	movl	%ecx, (%rdx,%rsi,4)
	movl	8(%r12), %edi
	movl	%edi, 24(%rax,%r13)
	movq	%r8, %rbx
	callq	reg_preferred_class
	movq	qty(%rip), %rcx
	movl	28(%rcx,%r13), %esi
	movq	%rax, %rdi
	movq	%rax, %r15
	callq	reg_class_subset_p
	testq	%rbx, %rax
	je	.LBB4_127
# BB#126:                               # %if.then.i
	movq	qty(%rip), %rax
	movl	%r15d, 28(%rax,%r13)
.LBB4_127:                              # %if.end.i
	movl	8(%r12), %edi
	callq	reg_alternate_class
	movq	qty(%rip), %rcx
	movl	32(%rcx,%r13), %esi
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	reg_class_subset_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB4_129
# BB#128:                               # %if.then10.i
	movq	qty(%rip), %rax
	movl	%ebx, 32(%rax,%r13)
.LBB4_129:                              # %if.end14.i
	movq	reg_n_info(%rip), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	32(%rax,%rcx,8), %rsi
	movzbl	40(%rsi), %ecx
	movq	qty(%rip), %rax
	testq	%rcx, %rcx
	movq	56(%rsp), %rdi          # 8-byte Reload
	je	.LBB4_131
# BB#130:                               # %if.then19.i
	movq	$1, %rcx
	movb	%cl, 42(%rax,%r13)
.LBB4_131:                              # %update_qty_class.exit
	movq	$32, %rcx
	shlq	%cl, %rdi
	movl	32(%rsi), %ebx
	movq	%rdi, %rdx
	movl	20(%rax,%r13), %edi
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	%rbx, %rdi
	movl	%edi, 20(%rax,%r13)
	movl	16(%rsi), %r8d
	movl	(%rax,%r13), %edi
	movq	$32, %rcx
	movq	16(%rsp), %rbx          # 8-byte Reload
	shlq	%cl, %rbx
	addq	%r8, %rdi
	movl	%edi, (%rax,%r13)
	movl	20(%rsi), %edi
	movl	4(%rax,%r13), %esi
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rdi, %rsi
	cmpq	%rdx, %rbx
	movl	%esi, 4(%rax,%r13)
	movq	$1, %rbx
	jle	.LBB4_136
# BB#132:                               # %if.then638
	movslq	24(%rax,%r13), %rdx
	testq	%rdx, %rdx
	js	.LBB4_135
.LBB4_133:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	reg_offset(%rip), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	(%rax,%rdx), %ecx
	subq	%r14, %rcx
	movb	%cl, (%rax,%rdx)
	movq	reg_next_in_qty(%rip), %rax
	movslq	(%rax,%rdx,4), %rdx
	testq	%rdx, %rdx
	jns	.LBB4_133
# BB#134:                               # %for.cond.for.end_crit_edge
	movq	qty(%rip), %rax
.LBB4_135:                              # %for.end
	movq	16(%rsp), %rcx          # 8-byte Reload
	movl	%ecx, 16(%rax,%r13)
	movzbl	2(%r12), %ecx
	movl	%ecx, 36(%rax,%r13)
	jmp	.LBB4_136
.LBB4_100:                              # %if.then460.if.else481_crit_edge
	movq	$1, %rdx
	movq	%r9, %rcx
	shlq	%cl, %rdx
.LBB4_101:                              # %if.else481
	movq	qty_phys_sugg(%rip), %rcx
	movq	(%rcx,%rax,8), %rsi
	testq	%rdx, %rsi
	jne	.LBB4_136
# BB#102:                               # %if.then490
	orq	%rdx, %rsi
	movq	%rsi, (%rcx,%rax,8)
	movq	qty_phys_num_sugg(%rip), %rcx
.LBB4_105:                              # %return
	movzbl	(%rcx,%rax,2), %edx
	incq	%rdx
	movw	%dx, (%rcx,%rax,2)
	jmp	.LBB4_136
.LBB4_108:                              # %if.then509.if.else533_crit_edge
	movq	reg_qty(%rip), %rdx
	movq	$1, %rax
	shlq	%cl, %rax
.LBB4_109:                              # %if.else533
	movq	32(%rsp), %rcx          # 8-byte Reload
	movslq	(%rdx,%rcx,4), %rcx
	movq	qty_phys_sugg(%rip), %rdx
	movq	(%rdx,%rcx,8), %rsi
	testq	%rax, %rsi
	jne	.LBB4_136
# BB#110:                               # %if.then542
	orq	%rax, %rsi
	movq	%rsi, (%rdx,%rcx,8)
	movq	qty_phys_num_sugg(%rip), %rax
	movzbl	(%rax,%rcx,2), %edx
	incq	%rdx
	movw	%dx, (%rax,%rcx,2)
.LBB4_136:                              # %return
	movq	%rbx, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp61:
	.size	combine_regs, .Ltmp61-combine_regs
	.cfi_endproc

	.align	16, 0x90
	.type	no_conflict_p,@function
no_conflict_p:                          # @no_conflict_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp68:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp69:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp70:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp71:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp72:
	.cfi_def_cfa_offset 48
.Ltmp73:
	.cfi_offset %rbx, -48
.Ltmp74:
	.cfi_offset %r12, -40
.Ltmp75:
	.cfi_offset %r13, -32
.Ltmp76:
	.cfi_offset %r14, -24
.Ltmp77:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$7, %rsi
	xorq	%rdx, %rdx
	xorq	%r15, %r15
                                        # kill: RDI<def> RBX<kill>
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB5_16
# BB#1:                                 # %lor.lhs.false
	movzbl	(%r14), %ecx
	cmpq	$63, %rcx
	jne	.LBB5_2
# BB#4:                                 # %land.lhs.true7
	movq	8(%r14), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB5_7
# BB#5:                                 # %land.lhs.true13
	movl	8(%rcx), %ecx
	jmp	.LBB5_6
.LBB5_2:                                # %lor.lhs.false
	cmpq	$61, %rcx
	jne	.LBB5_7
# BB#3:                                 # %land.lhs.true
	movl	8(%r14), %ecx
.LBB5_6:                                # %land.lhs.true13
	cmpq	$53, %rcx
	jb	.LBB5_16
.LBB5_7:                                # %if.end
	movq	8(%rax), %r13
	xorq	%r12, %r12
	xorq	%r15, %r15
	.align	16, 0x90
.LBB5_8:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB5_9
# BB#10:                                # %for.cond
                                        #   in Loop: Header=BB5_8 Depth=1
	cmpq	%r13, %rbx
	je	.LBB5_11
# BB#12:                                # %for.body
                                        #   in Loop: Header=BB5_8 Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB5_8
# BB#13:                                # %if.then33
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	$1, %rsi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB5_15
# BB#14:                                # %if.then33
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	$1, %r12
.LBB5_15:                               # %if.then33
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	$9, %rsi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB5_8
	jmp	.LBB5_16
.LBB5_9:
	movq	%r12, %r15
	jmp	.LBB5_16
.LBB5_11:
	movq	%r12, %r15
.LBB5_16:                               # %return
	movq	%r15, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp78:
	.size	no_conflict_p, .Ltmp78-no_conflict_p
	.cfi_endproc

	.align	16, 0x90
	.type	wipe_dead_reg,@function
wipe_dead_reg:                          # @wipe_dead_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp85:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp86:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp87:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp88:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp89:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp90:
	.cfi_def_cfa_offset 64
.Ltmp91:
	.cfi_offset %rbx, -48
.Ltmp92:
	.cfi_offset %r12, -40
.Ltmp93:
	.cfi_offset %r13, -32
.Ltmp94:
	.cfi_offset %r14, -24
.Ltmp95:
	.cfi_offset %r15, -16
	movq	this_insn(%rip), %rax
	movq	32(%rax), %rcx
	movq	%rdi, %r14
	movzbl	(%rcx), %ecx
	movslq	8(%r14), %r13
	movq	%rsi, %r15
	cmpq	$39, %rcx
	jne	.LBB6_12
# BB#1:                                 # %land.lhs.true
	movq	%rax, %rdi
	callq	multiple_sets
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB6_12
# BB#2:                                 # %if.then
	movq	this_insn(%rip), %rax
	movq	32(%rax), %rax
	movq	8(%rax), %rax
	movslq	(%rax), %rbx
	testq	%rbx, %rbx
	jle	.LBB6_12
.LBB6_3:                                # %for.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_4 Depth 2
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	decq	%rbx
	.align	16, 0x90
.LBB6_4:                                # %for.body
                                        #   Parent Loop BB6_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	this_insn(%rip), %rax
	movq	32(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax,%rbx,8), %r15
	movzbl	(%r15), %eax
	cmpq	$47, %rax
	jne	.LBB6_7
# BB#5:                                 # %land.lhs.true19
                                        #   in Loop: Header=BB6_4 Depth=2
	movq	8(%r15), %rsi
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	je	.LBB6_7
# BB#6:                                 # %land.lhs.true26
                                        #   in Loop: Header=BB6_4 Depth=2
	movq	%r14, %rdi
	callq	rtx_equal_p
	testq	%r12, %rax
	je	.LBB6_9
.LBB6_7:                                # %for.cond.backedge
                                        #   in Loop: Header=BB6_4 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB6_4
	jmp	.LBB6_8
.LBB6_9:                                # %land.lhs.true32
                                        #   in Loop: Header=BB6_3 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	movq	8(%r15), %rsi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%r14, %rdi
	callq	reg_overlap_mentioned_p
	testq	%r12, %rax
	movq	8(%rsp), %r15           # 8-byte Reload
	je	.LBB6_11
# BB#10:                                # %land.lhs.true32
                                        #   in Loop: Header=BB6_3 Depth=1
	movq	$1, %r15
.LBB6_11:                               # %land.lhs.true32
                                        #   in Loop: Header=BB6_3 Depth=1
	cmpq	$0, (%rsp)              # 8-byte Folded Reload
	jg	.LBB6_3
	jmp	.LBB6_12
.LBB6_8:
	movq	8(%rsp), %r15           # 8-byte Reload
.LBB6_12:                               # %if.end39
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %r15
	jne	.LBB6_16
# BB#13:                                # %land.lhs.true41
	movq	this_insn(%rip), %rdi
	movq	$2, %rsi
	movq	%r13, %rdx
	callq	find_regno_note
	testq	%rax, %rax
	je	.LBB6_14
# BB#15:                                # %land.lhs.true41
	movabsq	$1, %r15
	jmp	.LBB6_16
.LBB6_14:
	movabsq	$0, %r15
.LBB6_16:                               # %if.end45
	cmpq	$52, %r13
	jg	.LBB6_36
# BB#17:                                # %if.then47
	movzbl	2(%r14), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%r13, %rcx
	cmpq	$8, %rcx
	je	.LBB6_21
# BB#18:                                # %if.then47
	leaq	-21(%r13), %rcx
	andq	%r12, %rcx
	cmpq	$8, %rcx
	jb	.LBB6_21
# BB#19:                                # %if.then47
	leaq	-45(%r13), %rcx
	andq	%r12, %rcx
	cmpq	$8, %rcx
	jb	.LBB6_21
# BB#20:                                # %if.then47
	leaq	-29(%r13), %rcx
	andq	%r12, %rcx
	cmpq	$7, %rcx
	jbe	.LBB6_21
# BB#26:                                # %cond.false.i
	cmpq	$24, %rax
	jne	.LBB6_27
# BB#29:                                # %cond.true22.i
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB6_31
.LBB6_36:                               # %if.else
	movq	reg_qty(%rip), %rax
	movslq	(%rax,%r13,4), %rax
	testq	%rax, %rax
	js	.LBB6_38
# BB#37:                                # %if.then60
	movl	this_insn_number(%rip), %ecx
	movq	qty(%rip), %rdx
	leaq	(%r15,%rcx,2), %rcx
	imulq	$44, %rax, %rax
	movl	%ecx, 12(%rdx,%rax)
	jmp	.LBB6_38
.LBB6_21:                               # %cond.true.i
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB6_31
# BB#22:                                # %lor.rhs.i
	cmpq	$6, %rcx
	jne	.LBB6_23
# BB#24:                                # %lor.rhs.i
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB6_31
.LBB6_27:                               # %cond.false.i
	cmpq	$18, %rax
	jne	.LBB6_30
# BB#28:                                # %cond.true18.i
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB6_31
.LBB6_23:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB6_31
.LBB6_30:                               # %cond.end38.i
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB6_34
.LBB6_31:                               # %while.body48.lr.ph.i
	movq	regs_live(%rip), %rsi
	.align	16, 0x90
.LBB6_32:                               # %while.body48.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rdi
	leaq	-1(%rax,%r13), %rdx
	leaq	-1(%rax), %rax
	movq	$32, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$1, %rsi
	movq	%rdx, %rcx
	shlq	%cl, %rsi
	notq	%rsi
	andq	%rdi, %rsi
	testq	%rbx, %rbx
	jg	.LBB6_32
# BB#33:                                # %while.cond44.if.end.loopexit_crit_edge.i
	movq	%rsi, regs_live(%rip)
.LBB6_34:                               # %mark_life.exit
	testq	%r12, %r15
	je	.LBB6_38
# BB#35:                                # %if.then51
	movl	this_insn_number(%rip), %eax
	movzbl	2(%r14), %esi
	leaq	1(%rax,%rax), %rcx
	leaq	(%rax,%rax), %rdx
	movq	%r13, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	post_mark_life  # TAILCALL
.LBB6_38:                               # %if.end68
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp96:
	.size	wipe_dead_reg, .Ltmp96-wipe_dead_reg
	.cfi_endproc

	.align	16, 0x90
	.type	reg_is_set,@function
reg_is_set:                             # @reg_is_set
	.cfi_startproc
# BB#0:                                 # %entry
	movl	(%rdi), %ecx
	movq	%rsi, %rax
	orq	$2, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB7_5
# BB#1:                                 # %if.end
	movl	this_insn_number(%rip), %esi
	movzbl	(%rax), %eax
	addq	%rsi, %rsi
	cmpq	$49, %rax
	jne	.LBB7_2
# BB#3:                                 # %if.end
	movabsq	$1, %rax
	subq	%rax, %rsi
	jmp	reg_is_born  # TAILCALL
.LBB7_5:                                # %return
	retq
.LBB7_2:
	movabsq	$0, %rax
	subq	%rax, %rsi
	jmp	reg_is_born  # TAILCALL
.Ltmp97:
	.size	reg_is_set, .Ltmp97-reg_is_set
	.cfi_endproc

	.align	16, 0x90
	.type	qty_sugg_compare,@function
qty_sugg_compare:                       # @qty_sugg_compare
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp104:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp105:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp106:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp107:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp108:
	.cfi_def_cfa_offset 48
.Ltmp109:
	.cfi_offset %rbx, -48
.Ltmp110:
	.cfi_offset %r12, -40
.Ltmp111:
	.cfi_offset %r13, -32
.Ltmp112:
	.cfi_offset %r14, -24
.Ltmp113:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	$32, %rcx
	movq	qty_phys_num_copy_sugg(%rip), %rax
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movzbl	(%rax,%r14,2), %ebx
	testq	%rbx, %rbx
	je	.LBB8_2
# BB#1:                                 # %cond.true
	movq	$48, %rcx
	shlq	%cl, %rbx
	movq	$48, %rcx
	sarq	%cl, %rbx
	jmp	.LBB8_3
.LBB8_2:                                # %cond.false
	movq	qty_phys_num_sugg(%rip), %rcx
	movswq	(%rcx,%r14,2), %rcx
	imulq	$53, %rcx, %rbx
.LBB8_3:                                # %cond.end
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	(%rax,%rsi,2), %eax
	testq	%rax, %rax
	je	.LBB8_5
# BB#4:                                 # %cond.true11
	movq	$48, %rcx
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	jmp	.LBB8_6
.LBB8_5:                                # %cond.false15
	movq	qty_phys_num_sugg(%rip), %rax
	movswq	(%rax,%rsi,2), %rax
	imulq	$53, %rax, %rax
.LBB8_6:                                # %cond.end20
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%rax, %rcx
	andq	%r12, %rcx
	movq	%rbx, %rdx
	andq	%r12, %rdx
	cmpq	%rcx, %rdx
	jne	.LBB8_8
# BB#7:                                 # %if.end
	movq	qty(%rip), %rax
	imulq	$44, %rsi, %rbx
	movslq	(%rax,%rbx), %rdi
	callq	floor_log2_wide
	movq	qty(%rip), %r13
	movl	4(%r13,%rbx), %ecx
	movl	16(%r13,%rbx), %edi
	imulq	%rax, %rcx
	imulq	%rcx, %rdi
	andq	%r12, %rdi
	callq	int32_to_float64
	movl	12(%r13,%rbx), %edi
	movl	8(%r13,%rbx), %ecx
	movq	%rax, %r15
	subq	%rcx, %rdi
	andq	%r12, %rdi
	callq	int32_to_float64
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movabsq	$4621819117588971520, %r15 # imm = 0x4024000000000000
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	callq	float64_to_int64
	imulq	$44, %r14, %r14
	movslq	(%r13,%r14), %rdi
	movq	%rax, %rbx
	callq	floor_log2_wide
	movq	qty(%rip), %r13
	movl	4(%r13,%r14), %ecx
	movl	16(%r13,%r14), %edi
	imulq	%rax, %rcx
	imulq	%rcx, %rdi
	andq	%r12, %rdi
	callq	int32_to_float64
	movl	12(%r13,%r14), %edi
	movl	8(%r13,%r14), %ecx
	movq	%rax, %r14
	subq	%rcx, %rdi
	andq	%r12, %rdi
	callq	int32_to_float64
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	callq	float64_to_int64
.LBB8_8:                                # %return
	subq	%rax, %rbx
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp114:
	.size	qty_sugg_compare, .Ltmp114-qty_sugg_compare
	.cfi_endproc

	.align	16, 0x90
	.type	qty_sugg_compare_1,@function
qty_sugg_compare_1:                     # @qty_sugg_compare_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp121:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp122:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp123:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp124:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp125:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp126:
	.cfi_def_cfa_offset 64
.Ltmp127:
	.cfi_offset %rbx, -48
.Ltmp128:
	.cfi_offset %r12, -40
.Ltmp129:
	.cfi_offset %r13, -32
.Ltmp130:
	.cfi_offset %r14, -24
.Ltmp131:
	.cfi_offset %r15, -16
	movslq	(%rdi), %r12
	movq	qty_phys_num_copy_sugg(%rip), %rax
	movzbl	(%rax,%r12,2), %ebx
	movslq	(%rsi), %rsi
	testq	%rbx, %rbx
	je	.LBB9_2
# BB#1:                                 # %cond.true
	movq	$48, %rcx
	shlq	%cl, %rbx
	movq	$48, %rcx
	sarq	%cl, %rbx
	jmp	.LBB9_3
.LBB9_2:                                # %cond.false
	movq	qty_phys_num_sugg(%rip), %rcx
	movswq	(%rcx,%r12,2), %rcx
	imulq	$53, %rcx, %rbx
.LBB9_3:                                # %cond.end
	movzbl	(%rax,%rsi,2), %eax
	testq	%rax, %rax
	je	.LBB9_5
# BB#4:                                 # %cond.true11
	movq	$48, %rcx
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	jmp	.LBB9_6
.LBB9_5:                                # %cond.false15
	movq	qty_phys_num_sugg(%rip), %rax
	movswq	(%rax,%rsi,2), %rax
	imulq	$53, %rax, %rax
.LBB9_6:                                # %cond.end20
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	%rax, %rcx
	andq	%r14, %rcx
	movq	%rbx, %rdx
	andq	%r14, %rdx
	cmpq	%rcx, %rdx
	jne	.LBB9_7
# BB#8:                                 # %if.end
	movq	qty(%rip), %rax
	imulq	$44, %rsi, %rbx
	movslq	(%rax,%rbx), %rdi
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	callq	floor_log2_wide
	movq	qty(%rip), %r13
	movl	4(%r13,%rbx), %ecx
	movl	16(%r13,%rbx), %edi
	imulq	%rax, %rcx
	imulq	%rcx, %rdi
	andq	%r14, %rdi
	callq	int32_to_float64
	movl	12(%r13,%rbx), %edi
	movl	8(%r13,%rbx), %ecx
	movq	%rax, %r15
	subq	%rcx, %rdi
	andq	%r14, %rdi
	callq	int32_to_float64
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movabsq	$4621819117588971520, %rsi # imm = 0x4024000000000000
	movq	%rax, %rdi
	callq	float64_mul
	movq	%rax, %rdi
	callq	float64_to_int64
	imulq	$44, %r12, %r15
	movslq	(%r13,%r15), %rdi
	movq	%rax, %rbx
	movq	%rbx, %r13
	andq	%r14, %r13
	callq	floor_log2_wide
	movq	%r12, (%rsp)            # 8-byte Spill
	movq	qty(%rip), %r12
	movl	4(%r12,%r15), %ecx
	movl	16(%r12,%r15), %edi
	imulq	%rax, %rcx
	imulq	%rcx, %rdi
	andq	%r14, %rdi
	callq	int32_to_float64
	movl	12(%r12,%r15), %edi
	movl	8(%r12,%r15), %ecx
	movq	%rax, %r15
	subq	%rcx, %rdi
	andq	%r14, %rdi
	callq	int32_to_float64
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rdi
	movabsq	$4621819117588971520, %rsi # imm = 0x4024000000000000
	callq	float64_mul
	movq	%rax, %rdi
	callq	float64_to_int64
	andq	%rax, %r14
	cmpq	%r14, %r13
	jne	.LBB9_9
# BB#10:                                # %if.end70
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rsp), %rcx            # 8-byte Reload
	subq	%rax, %rcx
	movq	%rcx, %rbx
	jmp	.LBB9_11
.LBB9_7:                                # %if.then
	subq	%rax, %rbx
	jmp	.LBB9_11
.LBB9_9:                                # %if.then69
	subq	%rax, %rbx
.LBB9_11:                               # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp132:
	.size	qty_sugg_compare_1, .Ltmp132-qty_sugg_compare_1
	.cfi_endproc

	.align	16, 0x90
	.type	find_free_reg,@function
find_free_reg:                          # @find_free_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp139:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp140:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp141:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp142:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp143:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp144:
	.cfi_def_cfa_offset 144
.Ltmp145:
	.cfi_offset %rbx, -48
.Ltmp146:
	.cfi_offset %r12, -40
.Ltmp147:
	.cfi_offset %r13, -32
.Ltmp148:
	.cfi_offset %r14, -24
.Ltmp149:
	.cfi_offset %r15, -16
	movq	%r9, 48(%rsp)           # 8-byte Spill
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, 88(%rsp)          # 8-byte Spill
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB10_56
# BB#1:                                 # %entry
	movq	144(%rsp), %r10
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jg	.LBB10_56
# BB#2:                                 # %if.end.lr.ph
	movq	$32, %rcx
	movq	24(%rsp), %r11          # 8-byte Reload
	shlq	%cl, %r11
	movq	$32, %rcx
	sarq	%cl, %r11
	movq	%r11, 40(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	16(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	88(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	jmp	.LBB10_3
	.align	16, 0x90
.LBB10_46:                              # %if.then172
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	%r9, %rdi
	xorq	%rcx, %rcx
	movw	%cx, (%rax,%r11,2)
	movq	$1, %r8
	movq	48(%rsp), %r9           # 8-byte Reload
.LBB10_3:                               # %if.end
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB10_14 Depth 2
                                        #     Child Loop BB10_21 Depth 2
                                        #       Child Loop BB10_36 Depth 3
	movq	cfun(%rip), %rax
	movzbl	425(%rax), %eax
	testq	$1, %rax
	je	.LBB10_6
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	qty(%rip), %rax
	imulq	$44, %r11, %rcx
	movslq	20(%rax,%rcx), %rax
	testq	%rax, %rax
	jg	.LBB10_5
.LBB10_6:                               # %if.end4
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	64(%rsp), %rax          # 8-byte Reload
	testq	%rdi, %rax
	je	.LBB10_7
# BB#10:                                # %if.then16
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	call_fixed_reg_set(%rip), %rcx
	movq	losing_caller_save_reg_set(%rip), %rax
	orq	%rcx, %rax
	jmp	.LBB10_11
	.align	16, 0x90
.LBB10_7:                               # %if.end14
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	qty(%rip), %rax
	imulq	$44, %r11, %rcx
	movl	20(%rax,%rcx), %eax
	testq	%rax, %rax
	jne	.LBB10_9
# BB#8:                                 #   in Loop: Header=BB10_3 Depth=1
	movq	fixed_reg_set(%rip), %rax
	jmp	.LBB10_11
	.align	16, 0x90
.LBB10_9:                               # %select.mid
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	call_used_reg_set(%rip), %rax
.LBB10_11:                              # %for.cond.preheader
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	$32, %rcx
	movq	%r10, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r9, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jge	.LBB10_12
# BB#13:                                # %for.body.lr.ph
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	regs_live_at(%rip), %rdx
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%rdi, %r9
	.align	16, 0x90
.LBB10_14:                              # %for.body
                                        #   Parent Loop BB10_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-8(%rdx,%rsi,8), %rcx
	orq	%rcx, %rax
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%r10, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	incq	%rsi
	cmpq	%rbx, %rdi
	jl	.LBB10_14
	jmp	.LBB10_15
	.align	16, 0x90
.LBB10_12:                              #   in Loop: Header=BB10_3 Depth=1
	movq	%rdi, %r9
.LBB10_15:                              # %for.end
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	reg_class_contents(,%rcx,8), %r14
	notq	%r14
	orq	%rax, %r14
	orq	$1114176, %r14          # imm = 0x110040
	testq	%r9, %r8
	movq	%r8, 72(%rsp)           # 8-byte Spill
	movq	%r14, %r13
	je	.LBB10_20
# BB#16:                                # %if.then36
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	qty_phys_num_copy_sugg(%rip), %rax
	movzbl	(%rax,%r11,2), %eax
	testq	%rax, %rax
	jne	.LBB10_17
# BB#18:                                # %if.else46
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	qty_phys_sugg(%rip), %rax
	jmp	.LBB10_19
	.align	16, 0x90
.LBB10_17:                              # %if.then41
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	qty_phys_copy_sugg(%rip), %rax
.LBB10_19:                              # %if.end52
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	(%rax,%r11,8), %r13
	notq	%r13
	orq	%r14, %r13
.LBB10_20:                              # %if.end52
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	reg_class_contents+192(%rip), %rax
	movq	%r13, %rcx
	notq	%rcx
	xorq	%r12, %r12
	testq	%rcx, %rax
	je	.LBB10_41
	.align	16, 0x90
.LBB10_21:                              # %for.body61
                                        #   Parent Loop BB10_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB10_36 Depth 3
	movl	reg_alloc_order(,%r12,4), %r15d
	movq	$1, %rax
	movq	%r15, %rcx
	shlq	%cl, %rax
	testq	%r13, %rax
	jne	.LBB10_40
# BB#22:                                # %land.lhs.true68
                                        #   in Loop: Header=BB10_21 Depth=2
	movq	%r15, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%r9, %rbx
	callq	ix86_hard_regno_mode_ok
	movq	%rbx, %r9
	testq	%r9, %rax
	je	.LBB10_40
# BB#23:                                # %land.lhs.true70
                                        #   in Loop: Header=BB10_21 Depth=2
	movq	%r15, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	je	.LBB10_27
# BB#24:                                # %land.lhs.true70
                                        #   in Loop: Header=BB10_21 Depth=2
	leaq	-21(%r15), %rax
	andq	%r9, %rax
	cmpq	$8, %rax
	jb	.LBB10_27
# BB#25:                                # %land.lhs.true70
                                        #   in Loop: Header=BB10_21 Depth=2
	leaq	-45(%r15), %rax
	andq	%r9, %rax
	cmpq	$8, %rax
	jb	.LBB10_27
# BB#26:                                # %land.lhs.true70
                                        #   in Loop: Header=BB10_21 Depth=2
	leaq	-29(%r15), %rax
	andq	%r9, %rax
	cmpq	$7, %rax
	jbe	.LBB10_27
# BB#28:                                # %cond.false
                                        #   in Loop: Header=BB10_21 Depth=2
	movq	88(%rsp), %rax          # 8-byte Reload
	andq	%r9, %rax
	cmpq	$24, %rax
	jne	.LBB10_29
# BB#31:                                # %cond.true120
                                        #   in Loop: Header=BB10_21 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB10_33
.LBB10_27:                              # %cond.true
                                        #   in Loop: Header=BB10_21 Depth=2
	movq	80(%rsp), %rax          # 8-byte Reload
	movl	mode_class(,%rax,4), %eax
	addq	$-5, %rax
	andq	%r9, %rax
	cmpq	$2, %rax
	movq	$2, %rax
	jb	.LBB10_34
	jmp	.LBB10_39
.LBB10_29:                              # %cond.false
                                        #   in Loop: Header=BB10_21 Depth=2
	cmpq	$18, %rax
	jne	.LBB10_32
# BB#30:                                # %cond.true113
                                        #   in Loop: Header=BB10_21 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB10_33
.LBB10_32:                              # %cond.false124
                                        #   in Loop: Header=BB10_21 Depth=2
	movl	target_flags(%rip), %esi
	movq	80(%rsp), %rax          # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB10_33:                              # %cond.end137
                                        #   in Loop: Header=BB10_21 Depth=2
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rsi
	cmpq	$2, %rdx
	jl	.LBB10_38
.LBB10_34:                              # %land.rhs.lr.ph
                                        #   in Loop: Header=BB10_21 Depth=2
	movq	$2, %rdx
	.align	16, 0x90
.LBB10_36:                              # %land.rhs
                                        #   Parent Loop BB10_3 Depth=1
                                        #     Parent Loop BB10_21 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1(%rdx,%r15), %rcx
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%r14, %rsi
	jne	.LBB10_37
# BB#35:                                # %for.cond139
                                        #   in Loop: Header=BB10_36 Depth=3
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%rdx, %rsi
	movq	$32, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	incq	%rdx
	cmpq	%rbx, %rdi
	jl	.LBB10_36
	jmp	.LBB10_38
.LBB10_37:                              #   in Loop: Header=BB10_21 Depth=2
	decq	%rdx
	movq	%rdx, %rsi
.LBB10_38:                              # %for.end150
                                        #   in Loop: Header=BB10_21 Depth=2
	andq	%r9, %rax
	andq	%r9, %rsi
	cmpq	%rax, %rsi
	je	.LBB10_39
.LBB10_40:                              # %for.inc156
                                        #   in Loop: Header=BB10_21 Depth=2
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jl	.LBB10_21
.LBB10_41:                              # %fail
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	testq	%r9, %rax
	je	.LBB10_47
# BB#42:                                # %land.lhs.true160
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	qty_phys_num_copy_sugg(%rip), %rax
	movq	40(%rsp), %r11          # 8-byte Reload
	movzbl	(%rax,%r11,2), %ecx
	testq	%rcx, %rcx
	je	.LBB10_43
# BB#44:                                # %land.lhs.true166
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	qty_phys_num_sugg(%rip), %rcx
	movzbl	(%rcx,%r11,2), %ecx
	testq	%rcx, %rcx
	movq	144(%rsp), %r8
	movq	%r8, %r10
	jne	.LBB10_46
# BB#45:
	movq	%r9, %r15
	jmp	.LBB10_55
.LBB10_39:                              # %if.then153
	movq	%r15, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	144(%rsp), %rcx
	callq	post_mark_life
.LBB10_55:                              # %return
	movq	%r15, %rax
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB10_47:                              # %if.end176
	movl	flag_caller_saves(%rip), %eax
	testq	%rax, %rax
	je	.LBB10_48
# BB#49:                                # %if.end176
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	64(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	movq	%r9, %r15
	movq	48(%rsp), %r9           # 8-byte Reload
	movq	144(%rsp), %rbx
	jne	.LBB10_55
# BB#50:                                # %land.lhs.true182
	movq	qty(%rip), %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	imulq	$44, %rcx, %rdi
	movl	20(%rdx,%rdi), %esi
	testq	%rsi, %rsi
	je	.LBB10_51
# BB#52:                                # %land.lhs.true188
	movq	$34, %rcx
	movslq	(%rdx,%rdi), %rdx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	movq	%rax, %r15
	jge	.LBB10_55
# BB#53:                                # %if.then196
	movq	%rbx, (%rsp)
	movq	$1, %rcx
	xorq	%r8, %r8
	movq	$1, %rbx
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	find_free_reg
	movq	%rax, %r15
	movq	$32, %rcx
                                        # kill: RAX<def> R15<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB10_55
# BB#54:                                # %if.then200
	movl	%ebx, caller_save_needed(%rip)
	jmp	.LBB10_55
.LBB10_43:
	movq	%r9, %r15
	jmp	.LBB10_55
.LBB10_5:
	movq	%rdi, %r15
	jmp	.LBB10_55
.LBB10_48:
	movq	%r9, %r15
	jmp	.LBB10_55
.LBB10_51:
	movq	%rax, %r15
	jmp	.LBB10_55
.LBB10_56:                              # %if.then
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.find_free_reg, %rcx
	movq	$2206, %rsi             # imm = 0x89E
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp150:
	.size	find_free_reg, .Ltmp150-find_free_reg
	.cfi_endproc

	.align	16, 0x90
	.type	qty_compare_1,@function
qty_compare_1:                          # @qty_compare_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp157:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp158:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp159:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp160:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp161:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp162:
	.cfi_def_cfa_offset 64
.Ltmp163:
	.cfi_offset %rbx, -48
.Ltmp164:
	.cfi_offset %r12, -40
.Ltmp165:
	.cfi_offset %r13, -32
.Ltmp166:
	.cfi_offset %r14, -24
.Ltmp167:
	.cfi_offset %r15, -16
	movslq	(%rsi), %rcx
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	qty(%rip), %rax
	imulq	$44, %rcx, %rbx
	movslq	(%rax,%rbx), %rax
	movslq	(%rdi), %r14
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movq	%rax, %rdi
	callq	floor_log2_wide
	movq	qty(%rip), %r12
	movl	4(%r12,%rbx), %ecx
	movl	16(%r12,%rbx), %edi
	imulq	%rax, %rcx
	imulq	%rcx, %rdi
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rdi
	callq	int32_to_float64
	movl	12(%r12,%rbx), %edi
	movl	8(%r12,%rbx), %ecx
	movq	%rax, %r15
	subq	%rcx, %rdi
	andq	%r13, %rdi
	callq	int32_to_float64
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movabsq	$4621819117588971520, %rsi # imm = 0x4024000000000000
	movq	%rax, %rdi
	callq	float64_mul
	movq	%rax, %rdi
	callq	float64_to_int64
	imulq	$44, %r14, %rbx
	movslq	(%r12,%rbx), %rdi
	movq	%rax, %r15
	movq	%r15, %r12
	andq	%r13, %r12
	callq	floor_log2_wide
	movq	qty(%rip), %r14
	movl	4(%r14,%rbx), %ecx
	movl	16(%r14,%rbx), %edi
	imulq	%rax, %rcx
	imulq	%rcx, %rdi
	andq	%r13, %rdi
	callq	int32_to_float64
	movl	12(%r14,%rbx), %edi
	movl	8(%r14,%rbx), %ecx
	movq	%rax, %rbx
	subq	%rcx, %rdi
	andq	%r13, %rdi
	callq	int32_to_float64
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rdi
	movabsq	$4621819117588971520, %rsi # imm = 0x4024000000000000
	callq	float64_mul
	movq	%rax, %rdi
	callq	float64_to_int64
	andq	%rax, %r13
	cmpq	%r13, %r12
	je	.LBB11_1
# BB#2:                                 # %entry
	subq	%rax, %r15
	movq	%r15, %rax
	jmp	.LBB11_3
.LBB11_1:
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rsp), %rcx            # 8-byte Reload
	subq	%rcx, %rax
.LBB11_3:                               # %entry
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp168:
	.size	qty_compare_1, .Ltmp168-qty_compare_1
	.cfi_endproc

	.align	16, 0x90
	.type	post_mark_life,@function
post_mark_life:                         # @post_mark_life
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rcx, %r8
	movq	%rdx, %r9
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rdi, %rax
	cmpq	$8, %rax
	je	.LBB12_4
# BB#1:                                 # %entry
	leaq	-21(%rdi), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_4
# BB#2:                                 # %entry
	leaq	-45(%rdi), %rcx
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_4
# BB#3:                                 # %entry
	leaq	-29(%rdi), %rcx
	andq	%rax, %rcx
	cmpq	$7, %rcx
	jbe	.LBB12_4
# BB#9:                                 # %cond.false
	andq	%rsi, %rax
	cmpq	$24, %rax
	jne	.LBB12_10
# BB#17:                                # %cond.true22
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB12_18
.LBB12_4:                               # %cond.true
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	mode_class(,%rsi,4), %ecx
	xorq	%r10, %r10
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB12_19
# BB#5:                                 # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB12_6
# BB#7:                                 # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB12_19
.LBB12_10:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB12_12
# BB#11:                                # %cond.true18
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
.LBB12_18:                              # %while.body
	xorq	%r10, %r10
	jmp	.LBB12_19
.LBB12_6:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB12_19
.LBB12_12:                              # %while.cond.preheader
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	movl	target_flags(%rip), %r10d
	sarq	%cl, %rsi
	movzbl	mode_size(%rsi), %eax
	movq	$23, %rcx
	shrq	%cl, %r10
	andq	$4, %r10
	leaq	3(%rax,%r10), %rax
	addq	$4, %r10
	cqto
	idivq	%r10
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rsi, %rsi
	testq	%rdx, %rdx
	movabsq	$0, %r10
	jle	.LBB12_13
	.align	16, 0x90
.LBB12_19:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	leaq	-1(%rax,%rdi), %r11
	leaq	-1(%rax), %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rsi
	movq	%r11, %rcx
	shlq	%cl, %rsi
	orq	%r10, %rsi
	testq	%rdx, %rdx
	movq	%rsi, %r10
	jg	.LBB12_19
.LBB12_13:                              # %while.cond44.preheader
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB12_16
# BB#14:                                # %while.body47.lr.ph
	movq	regs_live_at(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	incq	%r9
	.align	16, 0x90
.LBB12_15:                              # %while.body47
                                        # =>This Inner Loop Header: Depth=1
	orq	%rsi, -8(%rax,%r9,8)
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r8, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%r9
	cmpq	%rdi, %rdx
	jl	.LBB12_15
.LBB12_16:                              # %if.end
	retq
.Ltmp169:
	.size	post_mark_life, .Ltmp169-post_mark_life
	.cfi_endproc

	.align	16, 0x90
	.type	reg_is_born,@function
reg_is_born:                            # @reg_is_born
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp176:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp177:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp178:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp179:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp180:
	.cfi_def_cfa_offset 48
.Ltmp181:
	.cfi_offset %rbx, -48
.Ltmp182:
	.cfi_offset %r12, -40
.Ltmp183:
	.cfi_offset %r13, -32
.Ltmp184:
	.cfi_offset %r14, -24
.Ltmp185:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movzbl	(%r15), %eax
	movq	%rsi, %r14
	cmpq	$63, %rax
	jne	.LBB13_3
# BB#1:                                 # %if.then
	movq	8(%r15), %rax
	movslq	8(%rax), %r13
	cmpq	$52, %r13
	jg	.LBB13_24
# BB#2:                                 # %if.then4
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	subreg_hard_regno
	movq	%rax, %r13
	jmp	.LBB13_4
.LBB13_3:                               # %if.else
	movl	8(%r15), %r13d
.LBB13_4:                               # %if.end8
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	jg	.LBB13_24
# BB#5:                                 # %if.then10
	movzbl	2(%r15), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%r13, %rcx
	cmpq	$8, %rcx
	je	.LBB13_9
# BB#6:                                 # %if.then10
	leaq	-21(%r13), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB13_9
# BB#7:                                 # %if.then10
	leaq	-45(%r13), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB13_9
# BB#8:                                 # %if.then10
	leaq	-29(%r13), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB13_9
# BB#14:                                # %cond.false.i
	cmpq	$24, %rax
	jne	.LBB13_15
# BB#17:                                # %cond.true22.i
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB13_19
.LBB13_24:                              # %if.else20
	movq	$32, %rcx
	movq	%r13, %r12
	movq	reg_qty(%rip), %rsi
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movl	(%rsi,%r12,4), %eax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	cmpq	%rcx, %rax
	jne	.LBB13_26
# BB#25:                                # %if.then23
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	(%rax,%r12,8), %rax
	movzbl	2(%rax), %eax
	movl	target_flags(%rip), %edi
	movzbl	2(%r15), %r8d
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	movslq	next_qty(%rip), %rcx
	leaq	1(%rcx), %rdx
	movl	%edx, next_qty(%rip)
	movl	%ecx, (%rsi,%r12,4)
	movq	reg_offset(%rip), %rdx
	xorq	%rsi, %rsi
	movb	%sil, (%rdx,%r12)
	movq	reg_next_in_qty(%rip), %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movl	%esi, (%rdx,%r12,4)
	movq	qty(%rip), %rdx
	imulq	$44, %rcx, %rbx
	movl	%r13d, 24(%rdx,%rbx)
	movl	%eax, 16(%rdx,%rbx)
	movl	%r8d, 36(%rdx,%rbx)
	movl	%r14d, 8(%rdx,%rbx)
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r12,8), %rax
	movl	32(%rax), %eax
	movl	%eax, 20(%rdx,%rbx)
	movq	%r13, %rdi
	callq	reg_preferred_class
	movq	qty(%rip), %rcx
	movl	%eax, 28(%rcx,%rbx)
	movq	%r13, %rdi
	callq	reg_alternate_class
	movq	qty(%rip), %rcx
	movl	%eax, 32(%rcx,%rbx)
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r12,8), %rax
	movl	16(%rax), %edx
	movl	%edx, (%rcx,%rbx)
	movl	20(%rax), %edx
	movl	%edx, 4(%rcx,%rbx)
	movzbl	40(%rax), %eax
	movb	%al, 42(%rcx,%rbx)
	movq	reg_qty(%rip), %rax
	movl	(%rax,%r12,4), %eax
.LBB13_26:                              # %if.end37
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB13_28
# BB#27:                                # %if.then42
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	qty(%rip), %rdx
	sarq	%cl, %rax
	imulq	$44, %rax, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, 12(%rdx,%rax)
	jmp	.LBB13_28
.LBB13_9:                               # %cond.true.i
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB13_19
# BB#10:                                # %lor.rhs.i
	cmpq	$6, %rcx
	jne	.LBB13_11
# BB#12:                                # %lor.rhs.i
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB13_19
.LBB13_15:                              # %cond.false.i
	cmpq	$18, %rax
	jne	.LBB13_18
# BB#16:                                # %cond.true18.i
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB13_19
.LBB13_11:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB13_19
.LBB13_18:                              # %cond.end38.i
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB13_22
.LBB13_19:                              # %while.body.lr.ph.i
	movq	regs_live(%rip), %rsi
	.align	16, 0x90
.LBB13_20:                              # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rdi
	leaq	-1(%rax,%r13), %rdx
	leaq	-1(%rax), %rax
	movq	$32, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$1, %rsi
	movq	%rdx, %rcx
	shlq	%cl, %rsi
	orq	%rdi, %rsi
	testq	%rbx, %rbx
	jg	.LBB13_20
# BB#21:                                # %while.cond.if.end.loopexit80_crit_edge.i
	movq	%rsi, regs_live(%rip)
.LBB13_22:                              # %mark_life.exit
	movl	this_insn_number(%rip), %eax
	movq	$33, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jle	.LBB13_28
# BB#23:                                # %if.then14
	movzbl	2(%r15), %esi
	addq	%rax, %rax
	movq	%r13, %rdi
	movq	%r14, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	post_mark_life  # TAILCALL
.LBB13_28:                              # %if.end48
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp186:
	.size	reg_is_born, .Ltmp186-reg_is_born
	.cfi_endproc

	.align	16, 0x90
	.type	no_equiv,@function
no_equiv:                               # @no_equiv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp191:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp192:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp193:
	.cfi_def_cfa_offset 32
.Ltmp194:
	.cfi_offset %rbx, -32
.Ltmp195:
	.cfi_offset %r14, -24
.Ltmp196:
	.cfi_offset %r15, -16
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB14_6
# BB#1:                                 # %if.end
	movslq	8(%rdi), %r14
	movq	reg_equiv(%rip), %rax
	leaq	(%r14,%r14,4), %rcx
	movq	32(%rax,%rcx,8), %rbx
	movq	const_int_rtx+512(%rip), %rdx
	cmpq	%rdx, %rbx
	je	.LBB14_6
# BB#2:                                 # %for.cond.preheader
	testq	%rbx, %rbx
	je	.LBB14_5
	.align	16, 0x90
.LBB14_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %r15
	movq	$3, %rsi
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	callq	find_reg_note
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	remove_note
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB14_3
# BB#4:                                 # %for.cond.for.end_crit_edge
	movq	const_int_rtx+512(%rip), %rdx
	movq	reg_equiv(%rip), %rax
.LBB14_5:                               # %for.end
	movq	$3, %rcx
	shlq	%cl, %r14
	leaq	(%r14,%r14,4), %rcx
	movq	%rdx, 32(%rax,%rcx)
	movq	reg_equiv(%rip), %rax
	movq	$0, 8(%rax,%rcx)
.LBB14_6:                               # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp197:
	.size	no_equiv, .Ltmp197-no_equiv
	.cfi_endproc

	.align	16, 0x90
	.type	contains_replace_regs,@function
contains_replace_regs:                  # @contains_replace_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp204:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp205:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp206:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp207:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp208:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp209:
	.cfi_def_cfa_offset 64
.Ltmp210:
	.cfi_offset %rbx, -48
.Ltmp211:
	.cfi_offset %r12, -40
.Ltmp212:
	.cfi_offset %r13, -32
.Ltmp213:
	.cfi_offset %r14, -24
.Ltmp214:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movzbl	(%r14), %eax
	cmpq	$133, %rax
	jg	.LBB15_4
# BB#1:                                 # %entry
	leaq	-54(%rax), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$15, %rcx
	ja	.LBB15_8
# BB#2:                                 # %entry
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$57399, %rdx            # imm = 0xE037
	je	.LBB15_6
# BB#3:
	xorq	%rax, %rax
	jmp	.LBB15_19
.LBB15_4:                               # %entry
	cmpq	$134, %rax
	jne	.LBB15_8
# BB#5:
	xorq	%rax, %rax
	jmp	.LBB15_19
.LBB15_6:                               # %entry
	cmpq	$7, %rcx
	jne	.LBB15_8
# BB#7:                                 # %sw.bb1
	movl	8(%r14), %eax
	movq	reg_equiv(%rip), %rcx
	leaq	(%rax,%rax,4), %rax
	movsbq	(%rcx,%rax,8), %rax
	jmp	.LBB15_19
.LBB15_8:                               # %sw.epilog
	movzbl	rtx_length(%rax), %r12d
	testq	%r12, %r12
	je	.LBB15_9
# BB#10:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB15_11:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB15_17 Depth 2
	movq	%r12, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movsbq	-1(%rcx,%rax), %rcx
	leaq	-1(%rax), %r12
	andq	%r13, %rcx
	cmpq	$69, %rcx
	jne	.LBB15_12
# BB#16:                                # %sw.bb16
                                        #   in Loop: Header=BB15_11 Depth=1
	movq	(%r14,%rax,8), %rbx
	movslq	(%rbx), %r15
	.align	16, 0x90
.LBB15_17:                              # %for.cond21
                                        #   Parent Loop BB15_11 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB15_14
# BB#18:                                # %for.body24
                                        #   in Loop: Header=BB15_17 Depth=2
	movq	(%rbx,%r15,8), %rdi
	decq	%r15
	callq	contains_replace_regs
	testq	%r13, %rax
	movq	$1, %rax
	je	.LBB15_17
	jmp	.LBB15_19
	.align	16, 0x90
.LBB15_12:                              # %for.body
                                        #   in Loop: Header=BB15_11 Depth=1
	cmpq	$101, %rcx
	jne	.LBB15_14
# BB#13:                                # %sw.bb12
                                        #   in Loop: Header=BB15_11 Depth=1
	movq	(%r14,%rax,8), %rdi
	callq	contains_replace_regs
	testq	%r13, %rax
	movq	$1, %rax
	jne	.LBB15_19
.LBB15_14:                              # %for.cond.backedge
                                        #   in Loop: Header=BB15_11 Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB15_11
# BB#15:
	xorq	%rax, %rax
	jmp	.LBB15_19
.LBB15_9:
	xorq	%rax, %rax
.LBB15_19:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp215:
	.size	contains_replace_regs, .Ltmp215-contains_replace_regs
	.cfi_endproc

	.align	16, 0x90
	.type	validate_equiv_mem,@function
validate_equiv_mem:                     # @validate_equiv_mem
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp222:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp223:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp224:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp225:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp226:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp227:
	.cfi_def_cfa_offset 64
.Ltmp228:
	.cfi_offset %rbx, -48
.Ltmp229:
	.cfi_offset %r12, -40
.Ltmp230:
	.cfi_offset %r13, -32
.Ltmp231:
	.cfi_offset %r14, -24
.Ltmp232:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %r14
	movq	%r15, equiv_mem(%rip)
	xorq	%r13, %r13
	movb	%r13b, equiv_mem_modified(%rip)
	movq	%r15, %rdi
	callq	side_effects_p
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	jne	.LBB16_16
# BB#1:                                 # %entry
	testq	%r14, %r14
	je	.LBB16_16
# BB#2:                                 # %land.rhs.lr.ph
	movq	$1, %r13
	.align	16, 0x90
.LBB16_3:                               # %land.rhs
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB16_9 Depth 2
	movzbl	equiv_mem_modified(%rip), %eax
	testq	%rax, %rax
	jne	.LBB16_15
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB16_14
# BB#5:                                 # %if.end5
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	$1, %rsi
	movq	%r14, %rdi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB16_16
# BB#6:                                 # %if.end9
                                        #   in Loop: Header=BB16_3 Depth=1
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$34, %rcx
	jne	.LBB16_8
# BB#7:                                 # %land.lhs.true
                                        #   in Loop: Header=BB16_3 Depth=1
	movl	(%r15), %ecx
	orq	%rax, %rcx
	testq	$67108864, %rcx         # imm = 0x4000000
	je	.LBB16_15
.LBB16_8:                               # %if.end23
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	32(%r14), %rdi
	xorq	%rdx, %rdx
	movabsq	$validate_equiv_mem_from_store, %rsi
	callq	note_stores
	movq	56(%r14), %rbx
	jmp	.LBB16_9
	.align	16, 0x90
.LBB16_13:                              # %for.inc
                                        #   in Loop: Header=BB16_9 Depth=2
	movq	16(%rbx), %rbx
.LBB16_9:                               # %if.end23
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB16_14
# BB#10:                                # %for.body30
                                        #   in Loop: Header=BB16_9 Depth=2
	movzbl	2(%rbx), %eax
	decq	%rax
	andq	%r12, %rax
	cmpq	$1, %rax
	ja	.LBB16_13
# BB#11:                                # %land.lhs.true41
                                        #   in Loop: Header=BB16_9 Depth=2
	movq	8(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB16_13
# BB#12:                                # %land.lhs.true49
                                        #   in Loop: Header=BB16_9 Depth=2
	movq	%r15, %rsi
	callq	reg_overlap_mentioned_p
	testq	%r12, %rax
	je	.LBB16_13
	jmp	.LBB16_15
	.align	16, 0x90
.LBB16_14:                              # %for.inc60
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	24(%r14), %r14
	testq	%r14, %r14
	jne	.LBB16_3
.LBB16_15:
	xorq	%r13, %r13
.LBB16_16:                              # %return
	movq	%r13, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp233:
	.size	validate_equiv_mem, .Ltmp233-validate_equiv_mem
	.cfi_endproc

	.align	16, 0x90
	.type	equiv_init_varies_p,@function
equiv_init_varies_p:                    # @equiv_init_varies_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp240:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp241:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp242:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp243:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp244:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp245:
	.cfi_def_cfa_offset 64
.Ltmp246:
	.cfi_offset %rbx, -48
.Ltmp247:
	.cfi_offset %r12, -40
.Ltmp248:
	.cfi_offset %r13, -32
.Ltmp249:
	.cfi_offset %r14, -24
.Ltmp250:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	(%rbx), %ecx
	movq	%rcx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	movq	$1, %rax
	cmpq	$53, %rsi
	jg	.LBB17_5
# BB#1:                                 # %entry
	cmpq	$41, %rsi
	jne	.LBB17_3
# BB#2:                                 # %sw.bb13
	testq	$134217728, %rcx        # imm = 0x8000000
	jne	.LBB17_26
	jmp	.LBB17_3
.LBB17_5:                               # %entry
	leaq	-54(%rsi), %rdx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rdx
	cmpq	$17, %rdx
	ja	.LBB17_3
# BB#6:                                 # %entry
	jmpq	*.LJTI17_0(,%rdx,8)
.LBB17_11:                              # %sw.bb5
	xorq	%rax, %rax
	jmp	.LBB17_26
.LBB17_3:                               # %sw.epilog
	movzbl	rtx_length(%rsi), %r14d
	testq	%r14, %r14
	je	.LBB17_4
# BB#16:                                # %for.body.lr.ph
	movq	rtx_format(,%rsi,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB17_17:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB17_24 Depth 2
	movq	%r14, %r15
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	-1(%rax,%r15), %eax
	leaq	-1(%r15), %r14
	cmpq	$69, %rax
	jne	.LBB17_18
# BB#21:                                # %for.cond45.preheader
                                        #   in Loop: Header=BB17_17 Depth=1
	movq	(%rbx,%r15,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r12
	jle	.LBB17_19
	.align	16, 0x90
.LBB17_24:                              # %for.body51
                                        #   Parent Loop BB17_17 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r12,8), %rdi
	callq	equiv_init_varies_p
	testq	%r13, %rax
	jne	.LBB17_25
# BB#23:                                # %for.cond45
                                        #   in Loop: Header=BB17_24 Depth=2
	movq	(%rbx,%r15,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r12
	cmpq	%rdx, %rsi
	jl	.LBB17_24
	jmp	.LBB17_19
	.align	16, 0x90
.LBB17_18:                              # %for.body
                                        #   in Loop: Header=BB17_17 Depth=1
	cmpq	$101, %rax
	jne	.LBB17_19
# BB#22:                                # %if.then30
                                        #   in Loop: Header=BB17_17 Depth=1
	movq	(%rbx,%r15,8), %rdi
	callq	equiv_init_varies_p
	testq	%r13, %rax
	jne	.LBB17_25
.LBB17_19:                              # %for.cond.backedge
                                        #   in Loop: Header=BB17_17 Depth=1
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB17_17
# BB#20:
	xorq	%rax, %rax
	jmp	.LBB17_26
.LBB17_25:
	movq	$1, %rax
	jmp	.LBB17_26
.LBB17_4:
	xorq	%rax, %rax
.LBB17_26:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB17_12:                              # %sw.bb6
	movl	8(%rbx), %eax
	movq	reg_equiv(%rip), %rcx
	leaq	(%rax,%rax,4), %rax
	movzbl	(%rcx,%rax,8), %ecx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	jne	.LBB17_26
# BB#13:                                # %land.rhs
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	rtx_varies_p
	testq	%r14, %rax
	je	.LBB17_14
# BB#15:                                # %land.rhs
	movabsq	$1, %rax
	jmp	.LBB17_26
.LBB17_7:                               # %sw.bb
	testq	$67108864, %rcx         # imm = 0x4000000
	movq	$1, %rax
	je	.LBB17_26
# BB#8:                                 # %lor.rhs
	movq	8(%rbx), %rdi
	callq	equiv_init_varies_p
	testq	%r14, %rax
	je	.LBB17_9
# BB#10:                                # %lor.rhs
	movabsq	$1, %rax
	jmp	.LBB17_26
.LBB17_9:
	movabsq	$0, %rax
	jmp	.LBB17_26
.LBB17_14:
	movabsq	$0, %rax
	jmp	.LBB17_26
.Ltmp251:
	.size	equiv_init_varies_p, .Ltmp251-equiv_init_varies_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI17_0:
	.quad	.LBB17_11
	.quad	.LBB17_11
	.quad	.LBB17_11
	.quad	.LBB17_3
	.quad	.LBB17_11
	.quad	.LBB17_3
	.quad	.LBB17_3
	.quad	.LBB17_12
	.quad	.LBB17_3
	.quad	.LBB17_3
	.quad	.LBB17_3
	.quad	.LBB17_3
	.quad	.LBB17_7
	.quad	.LBB17_11
	.quad	.LBB17_11
	.quad	.LBB17_3
	.quad	.LBB17_3
	.quad	.LBB17_26

	.text
	.align	16, 0x90
	.type	equiv_init_movable_p,@function
equiv_init_movable_p:                   # @equiv_init_movable_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp258:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp259:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp260:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp261:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp262:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp263:
	.cfi_def_cfa_offset 64
.Ltmp264:
	.cfi_offset %rbx, -48
.Ltmp265:
	.cfi_offset %r12, -40
.Ltmp266:
	.cfi_offset %r13, -32
.Ltmp267:
	.cfi_offset %r14, -24
.Ltmp268:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %rbx
	xorq	%rax, %rax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	jmp	.LBB18_1
.LBB18_8:                               # %sw.bb
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	16(%rbx), %rbx
	.align	16, 0x90
.LBB18_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rbx), %ecx
	movq	%rcx, %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$95, %rdi
	jg	.LBB18_7
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB18_1 Depth=1
	cmpq	$68, %rdi
	jg	.LBB18_6
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB18_1 Depth=1
	cmpq	$60, %rdi
	jg	.LBB18_9
# BB#4:                                 # %tailrecurse
                                        #   in Loop: Header=BB18_1 Depth=1
	leaq	-41(%rdi), %rdx
	andq	%rsi, %rdx
	cmpq	$8, %rdx
	ja	.LBB18_17
# BB#5:                                 # %tailrecurse
                                        #   in Loop: Header=BB18_1 Depth=1
	jmpq	*.LJTI18_0(,%rdx,8)
.LBB18_16:                              # %sw.bb25
	testq	$134217728, %rcx        # imm = 0x8000000
	movq	$41, %rdi
	jne	.LBB18_29
	jmp	.LBB18_17
.LBB18_7:                               # %tailrecurse
	leaq	-96(%rdi), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$6, %rcx
	jb	.LBB18_29
	jmp	.LBB18_17
.LBB18_6:                               # %tailrecurse
	cmpq	$69, %rdi
	jne	.LBB18_17
	jmp	.LBB18_29
.LBB18_9:                               # %tailrecurse
	cmpq	$61, %rdi
	jne	.LBB18_17
# BB#10:                                # %sw.bb3
	movl	8(%rbx), %r8d
	movq	reg_equiv(%rip), %rdx
	leaq	(%r8,%r8,4), %rsi
	movslq	24(%rdx,%rsi,8), %rdi
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	leaq	(%r15,%r15,4), %rcx
	movslq	24(%rdx,%rcx,8), %rcx
	cmpq	%rcx, %rdi
	jl	.LBB18_12
# BB#11:                                # %land.lhs.true
	movzbl	(%rdx,%rsi,8), %ecx
	testq	%rcx, %rcx
	movq	$1, %rax
	jne	.LBB18_29
.LBB18_12:                              # %lor.rhs
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%r8,8), %rax
	movslq	36(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	jns	.LBB18_29
# BB#13:                                # %land.rhs
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	rtx_varies_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB18_14
# BB#15:                                # %land.rhs
	movabsq	$1, %rax
	jmp	.LBB18_29
.LBB18_17:                              # %sw.epilog
	movzbl	rtx_length(%rdi), %r12d
	testq	%r12, %r12
	je	.LBB18_18
# BB#19:                                # %for.body.lr.ph
	movq	rtx_format(,%rdi,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB18_20:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB18_26 Depth 2
	movq	%r12, %r13
	movq	8(%rsp), %rax           # 8-byte Reload
	movsbq	-1(%rax,%r13), %rax
	leaq	-1(%r13), %r12
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$69, %rax
	jne	.LBB18_21
# BB#25:                                # %sw.bb48
                                        #   in Loop: Header=BB18_20 Depth=1
	movq	(%rbx,%r13,8), %rax
	movslq	(%rax), %r14
	.align	16, 0x90
.LBB18_26:                              # %for.cond53
                                        #   Parent Loop BB18_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB18_23
# BB#27:                                # %for.body56
                                        #   in Loop: Header=BB18_26 Depth=2
	movq	(%rbx,%r13,8), %rax
	movq	(%rax,%r14,8), %rdi
	decq	%r14
	movq	%r15, %rsi
	callq	equiv_init_movable_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB18_26
	jmp	.LBB18_28
	.align	16, 0x90
.LBB18_21:                              # %for.body
                                        #   in Loop: Header=BB18_20 Depth=1
	cmpq	$101, %rax
	jne	.LBB18_23
# BB#22:                                # %sw.bb39
                                        #   in Loop: Header=BB18_20 Depth=1
	movq	(%rbx,%r13,8), %rdi
	movq	%r15, %rsi
	callq	equiv_init_movable_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB18_28
.LBB18_23:                              # %for.cond.backedge
                                        #   in Loop: Header=BB18_20 Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB18_20
# BB#24:
	movq	$1, %rax
	jmp	.LBB18_29
.LBB18_28:
	xorq	%rax, %rax
	jmp	.LBB18_29
.LBB18_18:
	movq	$1, %rax
.LBB18_29:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB18_14:
	movabsq	$0, %rax
	jmp	.LBB18_29
.Ltmp269:
	.size	equiv_init_movable_p, .Ltmp269-equiv_init_movable_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI18_0:
	.quad	.LBB18_16
	.quad	.LBB18_17
	.quad	.LBB18_29
	.quad	.LBB18_17
	.quad	.LBB18_17
	.quad	.LBB18_17
	.quad	.LBB18_8
	.quad	.LBB18_17
	.quad	.LBB18_29

	.text
	.align	16, 0x90
	.type	memref_referenced_p,@function
memref_referenced_p:                    # @memref_referenced_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp276:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp277:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp278:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp279:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp280:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp281:
	.cfi_def_cfa_offset 64
.Ltmp282:
	.cfi_offset %rbx, -48
.Ltmp283:
	.cfi_offset %r12, -40
.Ltmp284:
	.cfi_offset %r13, -32
.Ltmp285:
	.cfi_offset %r14, -24
.Ltmp286:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	jmp	.LBB19_1
	.align	16, 0x90
.LBB19_16:                              # %if.end36
                                        #   in Loop: Header=BB19_1 Depth=1
	movq	16(%rbx), %rbx
.LBB19_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %ecx
	cmpq	$133, %rcx
	jg	.LBB19_4
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB19_1 Depth=1
	leaq	-47(%rcx), %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	cmpq	$22, %rdx
	ja	.LBB19_11
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB19_1 Depth=1
	xorq	%rax, %rax
	jmpq	*.LJTI19_0(,%rdx,8)
.LBB19_13:                              # %sw.bb13
                                        #   in Loop: Header=BB19_1 Depth=1
	movq	8(%rbx), %rsi
	movzbl	(%rsi), %eax
	cmpq	$66, %rax
	jne	.LBB19_15
# BB#14:                                # %if.then18
                                        #   in Loop: Header=BB19_1 Depth=1
	movq	8(%rsi), %rsi
.LBB19_15:                              # %if.else
                                        #   in Loop: Header=BB19_1 Depth=1
	movq	%r14, %rdi
	callq	memref_referenced_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rax
	je	.LBB19_16
	jmp	.LBB19_26
.LBB19_4:                               # %tailrecurse
	leaq	-134(%rcx), %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	$2, %rax
	jae	.LBB19_11
# BB#5:
	xorq	%rax, %rax
	jmp	.LBB19_26
.LBB19_6:                               # %sw.bb1
	movl	8(%rbx), %eax
	movq	reg_equiv(%rip), %rcx
	leaq	(%rax,%rax,4), %rax
	movq	8(%rcx,%rax,8), %rsi
	xorq	%rax, %rax
	testq	%rsi, %rsi
	je	.LBB19_26
# BB#7:                                 # %land.rhs
	movq	%r14, %rdi
	callq	memref_referenced_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB19_8
# BB#9:                                 # %land.rhs
	movabsq	$1, %rax
	jmp	.LBB19_26
.LBB19_10:                              # %sw.bb10
	movabsq	$rtx_varies_p, %rcx
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	true_dependence
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rax
	movq	$66, %rcx
	jne	.LBB19_26
.LBB19_11:                              # %sw.epilog
	movzbl	rtx_length(%rcx), %r13d
	testq	%r13, %r13
	je	.LBB19_12
# BB#17:                                # %for.body.lr.ph
	movq	rtx_format(,%rcx,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB19_18:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_24 Depth 2
	movq	%r13, %r12
	movq	8(%rsp), %rax           # 8-byte Reload
	movsbq	-1(%rax,%r12), %rax
	leaq	-1(%r12), %r13
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$69, %rax
	jne	.LBB19_19
# BB#23:                                # %sw.bb59
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	(%rbx,%r12,8), %rax
	movslq	(%rax), %r15
	.align	16, 0x90
.LBB19_24:                              # %for.cond64
                                        #   Parent Loop BB19_18 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB19_21
# BB#25:                                # %for.body67
                                        #   in Loop: Header=BB19_24 Depth=2
	movq	(%rbx,%r12,8), %rax
	movq	(%rax,%r15,8), %rsi
	decq	%r15
	movq	%r14, %rdi
	callq	memref_referenced_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rax
	je	.LBB19_24
	jmp	.LBB19_26
	.align	16, 0x90
.LBB19_19:                              # %for.body
                                        #   in Loop: Header=BB19_18 Depth=1
	cmpq	$101, %rax
	jne	.LBB19_21
# BB#20:                                # %sw.bb50
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	(%rbx,%r12,8), %rsi
	movq	%r14, %rdi
	callq	memref_referenced_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rax
	jne	.LBB19_26
.LBB19_21:                              # %for.cond.backedge
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB19_18
# BB#22:
	xorq	%rax, %rax
	jmp	.LBB19_26
.LBB19_12:
	xorq	%rax, %rax
.LBB19_26:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB19_8:
	movabsq	$0, %rax
	jmp	.LBB19_26
.Ltmp287:
	.size	memref_referenced_p, .Ltmp287-memref_referenced_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI19_0:
	.quad	.LBB19_13
	.quad	.LBB19_11
	.quad	.LBB19_11
	.quad	.LBB19_11
	.quad	.LBB19_11
	.quad	.LBB19_11
	.quad	.LBB19_11
	.quad	.LBB19_26
	.quad	.LBB19_26
	.quad	.LBB19_26
	.quad	.LBB19_11
	.quad	.LBB19_26
	.quad	.LBB19_26
	.quad	.LBB19_11
	.quad	.LBB19_6
	.quad	.LBB19_11
	.quad	.LBB19_11
	.quad	.LBB19_11
	.quad	.LBB19_11
	.quad	.LBB19_10
	.quad	.LBB19_26
	.quad	.LBB19_26
	.quad	.LBB19_26

	.text
	.align	16, 0x90
	.type	validate_equiv_mem_from_store,@function
validate_equiv_mem_from_store:          # @validate_equiv_mem_from_store
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp290:
	.cfi_def_cfa_offset 16
.Ltmp291:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB20_3
# BB#1:                                 # %land.lhs.true
	movq	equiv_mem(%rip), %rsi
	movq	%rbx, %rdi
	callq	reg_overlap_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB20_5
# BB#2:                                 # %land.lhs.true.lor.lhs.false_crit_edge
	movl	(%rbx), %eax
.LBB20_3:                               # %lor.lhs.false
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB20_6
# BB#4:                                 # %land.lhs.true4
	movq	equiv_mem(%rip), %rdx
	movabsq	$rtx_varies_p, %rcx
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	true_dependence
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB20_6
.LBB20_5:                               # %if.then
	movq	$1, %rax
	movb	%al, equiv_mem_modified(%rip)
.LBB20_6:                               # %if.end
	popq	%rbx
	retq
.Ltmp292:
	.size	validate_equiv_mem_from_store, .Ltmp292-validate_equiv_mem_from_store
	.cfi_endproc

	.type	recorded_label_ref,@object # @recorded_label_ref
	.local	recorded_label_ref
	.comm	recorded_label_ref,1,1
	.type	qty,@object             # @qty
	.local	qty
	.comm	qty,8,8
	.type	qty_phys_copy_sugg,@object # @qty_phys_copy_sugg
	.local	qty_phys_copy_sugg
	.comm	qty_phys_copy_sugg,8,8
	.type	qty_phys_num_copy_sugg,@object # @qty_phys_num_copy_sugg
	.local	qty_phys_num_copy_sugg
	.comm	qty_phys_num_copy_sugg,8,8
	.type	qty_phys_sugg,@object   # @qty_phys_sugg
	.local	qty_phys_sugg
	.comm	qty_phys_sugg,8,8
	.type	qty_phys_num_sugg,@object # @qty_phys_num_sugg
	.local	qty_phys_num_sugg
	.comm	qty_phys_num_sugg,8,8
	.type	reg_qty,@object         # @reg_qty
	.local	reg_qty
	.comm	reg_qty,8,8
	.type	reg_offset,@object      # @reg_offset
	.local	reg_offset
	.comm	reg_offset,8,8
	.type	reg_next_in_qty,@object # @reg_next_in_qty
	.local	reg_next_in_qty
	.comm	reg_next_in_qty,8,8
	.type	next_qty,@object        # @next_qty
	.local	next_qty
	.comm	next_qty,4,4
	.type	reg_renumber,@object    # @reg_renumber
	.comm	reg_renumber,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	";; Register %d in %d.\n"
	.size	.L.str, 23

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"local-alloc.c"
	.size	.L.str1, 14

	.type	.L__FUNCTION__.block_alloc,@object # @__FUNCTION__.block_alloc
.L__FUNCTION__.block_alloc:
	.asciz	"block_alloc"
	.size	.L__FUNCTION__.block_alloc, 12

	.type	regs_live_at,@object    # @regs_live_at
	.local	regs_live_at
	.comm	regs_live_at,8,8
	.type	regs_live,@object       # @regs_live
	.local	regs_live
	.comm	regs_live,8,8
	.type	this_insn_number,@object # @this_insn_number
	.local	this_insn_number
	.comm	this_insn_number,4,4
	.type	this_insn,@object       # @this_insn
	.local	this_insn
	.comm	this_insn,8,8
	.type	.L__FUNCTION__.find_free_reg,@object # @__FUNCTION__.find_free_reg
.L__FUNCTION__.find_free_reg:
	.asciz	"find_free_reg"
	.size	.L__FUNCTION__.find_free_reg, 14

	.type	reg_equiv,@object       # @reg_equiv
	.local	reg_equiv
	.comm	reg_equiv,8,8
	.type	.L__FUNCTION__.update_equiv_regs,@object # @__FUNCTION__.update_equiv_regs
.L__FUNCTION__.update_equiv_regs:
	.asciz	"update_equiv_regs"
	.size	.L__FUNCTION__.update_equiv_regs, 18

	.type	equiv_mem,@object       # @equiv_mem
	.local	equiv_mem
	.comm	equiv_mem,8,8
	.type	equiv_mem_modified,@object # @equiv_mem_modified
	.local	equiv_mem_modified
	.comm	equiv_mem_modified,1,1

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
