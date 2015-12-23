	.file	"ssa-dce.c.bc"
	.text
	.globl	ssa_eliminate_dead_code
	.align	16, 0x90
	.type	ssa_eliminate_dead_code,@function
ssa_eliminate_dead_code:                # @ssa_eliminate_dead_code
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
	subq	$112, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 160
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
	callq	get_max_uid
	movq	%rax, %r13
	callq	get_insns
	jmp	.LBB0_2
	.align	16, 0x90
.LBB0_1:                                # %for.body.i
                                        #   in Loop: Header=BB0_2 Depth=1
	movl	(%rax), %ecx
	orq	$268435456, %rcx        # imm = 0x10000000
	movl	%ecx, (%rax)
	movq	24(%rax), %rax
.LBB0_2:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB0_1
# BB#3:                                 # %mark_all_insn_unnecessary.exit
	movabsq	$.L.str, %rdx
	movq	$64, %rdi
	movq	$8, %rsi
	callq	varray_init
	movslq	n_basic_blocks(%rip), %rbx
	movq	%rax, 88(%rsp)
	movq	$16, %rdi
	callq	xmalloc
	movq	%rax, %r15
	movq	%r15, 8(%rsp)           # 8-byte Spill
	addq	$2, %rbx
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	movq	%rdi, %r12
	sarq	%cl, %r12
	movl	%ebx, 8(%r15)
	movq	$29, %rcx
	sarq	%cl, %rdi
	callq	xmalloc
	movq	%rax, %r14
	testq	%r12, %r12
	movq	%r14, (%r15)
	jle	.LBB0_6
# BB#4:                                 # %for.body.i711.preheader
	movq	$1, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	56(%rsp), %r12          # 8-byte Reload
	andq	%r15, %r12
	.align	16, 0x90
.LBB0_5:                                # %for.body.i711
                                        # =>This Inner Loop Header: Depth=1
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, -8(%r14,%rbx,8)
	movq	%rbx, %rax
	andq	%r15, %rax
	incq	%rbx
	cmpq	%r12, %rax
	jne	.LBB0_5
.LBB0_6:                                # %control_dependent_block_to_edge_map_create.exit
	movq	%r14, 48(%rsp)          # 8-byte Spill
	callq	connect_infinite_loops_to_exit
	movq	%r13, %rdi
	callq	compute_bb_for_insn
	movslq	n_basic_blocks(%rip), %rdi
	movq	$2, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rdx
	movq	%rax, %r14
	testq	%rdx, %rdx
	jle	.LBB0_9
# BB#7:
	movq	$1, %rax
	movabsq	$4294967293, %rsi       # imm = 0xFFFFFFFD
	.align	16, 0x90
.LBB0_8:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	%esi, -4(%r14,%rax,4)
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rax
	cmpq	%rdx, %rdi
	jl	.LBB0_8
.LBB0_9:                                # %for.end
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	$1, %rbx
	movq	%r14, %rdi
	callq	calculate_dominance_info
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_14
# BB#10:
	movabsq	$4294967293, %rdx       # imm = 0xFFFFFFFD
	movabsq	$4294967294, %rsi       # imm = 0xFFFFFFFE
	.align	16, 0x90
.LBB0_11:                               # %for.body9
                                        # =>This Inner Loop Header: Depth=1
	movl	-4(%r14,%rbx,4), %ecx
	cmpq	%rdx, %rcx
	jne	.LBB0_13
# BB#12:                                # %if.then
                                        #   in Loop: Header=BB0_11 Depth=1
	movl	%esi, -4(%r14,%rbx,4)
.LBB0_13:                               # %for.inc16
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rbx
	cmpq	%rax, %rdi
	jl	.LBB0_11
.LBB0_14:                               # %for.end18
	movq	%r14, 40(%rsp)          # 8-byte Spill
	callq	create_edge_list
	movslq	4(%rax), %rdi
	testq	%rdi, %rdi
	jle	.LBB0_38
# BB#15:                                # %for.body.lr.ph.i
	xorq	%rbx, %rbx
	movabsq	$entry_exit_blocks+112, %r14
	.align	16, 0x90
.LBB0_16:                               # %for.body.i713
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_29 Depth 2
	movq	8(%rax), %rcx
	movq	(%rcx,%rbx,8), %rcx
	movq	16(%rcx), %rdx
	cmpq	%r14, %rdx
	je	.LBB0_17
# BB#19:                                # %if.end.i.i
                                        #   in Loop: Header=BB0_16 Depth=1
	movabsq	$entry_exit_blocks, %rsi
	cmpq	%rsi, %rdx
	jne	.LBB0_21
# BB#20:                                # %cond.true.i.i
                                        #   in Loop: Header=BB0_16 Depth=1
	movq	basic_block_info(%rip), %rdx
	movq	32(%rdx), %r12
	jmp	.LBB0_27
	.align	16, 0x90
.LBB0_21:                               # %cond.false.i.i
                                        #   in Loop: Header=BB0_16 Depth=1
	testq	%rdx, %rdx
	je	.LBB0_22
# BB#23:                                # %if.end.i.i.i
                                        #   in Loop: Header=BB0_16 Depth=1
	movslq	88(%rdx), %rdx
	cmpq	$-3, %rdx
	je	.LBB0_24
# BB#25:                                # %lor.lhs.false.i.i.i
                                        #   in Loop: Header=BB0_16 Depth=1
	movq	40(%rsp), %rsi          # 8-byte Reload
	movslq	(%rsi,%rdx,4), %rdx
	cmpq	$-2, %rdx
	movq	%r14, %r12
	je	.LBB0_27
# BB#26:                                # %if.else10.i.i.i
                                        #   in Loop: Header=BB0_16 Depth=1
	movq	basic_block_info(%rip), %rsi
	movq	32(%rsi,%rdx,8), %r12
.LBB0_27:                               # %cond.end.i.i
                                        #   in Loop: Header=BB0_16 Depth=1
	movq	24(%rcx), %r15
	cmpq	%r12, %r15
	je	.LBB0_37
# BB#28:                                # %cond.end.i.i
                                        #   in Loop: Header=BB0_16 Depth=1
	cmpq	%r14, %r15
	je	.LBB0_37
	.align	16, 0x90
.LBB0_29:                               # %for.body.i.i
                                        #   Parent Loop BB0_16 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	56(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movl	88(%r15), %edi
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$2, %rdi
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jge	.LBB0_170
# BB#30:                                # %set_control_dependent_block_to_edge_map_bit.exit.i.i
                                        #   in Loop: Header=BB0_29 Depth=2
	movq	%rax, %r13
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%rdi,8), %rdi
	movq	%rbx, %rsi
	callq	bitmap_set_bit
	testq	%r15, %r15
	je	.LBB0_171
# BB#31:                                # %if.end.i38.i.i
                                        #   in Loop: Header=BB0_29 Depth=2
	movslq	88(%r15), %rdx
	cmpq	$-3, %rdx
	movq	%r13, %rax
	je	.LBB0_172
# BB#32:                                # %if.end2.i41.i.i
                                        #   in Loop: Header=BB0_29 Depth=2
	movabsq	$entry_exit_blocks, %rcx
	cmpq	%rcx, %r15
	jne	.LBB0_43
# BB#33:                                # %if.then4.i44.i.i
                                        #   in Loop: Header=BB0_29 Depth=2
	movq	basic_block_info(%rip), %rcx
	addq	$32, %rcx
	jmp	.LBB0_34
	.align	16, 0x90
.LBB0_43:                               # %if.else.i46.i.i
                                        #   in Loop: Header=BB0_29 Depth=2
	cmpq	%r14, %r15
	je	.LBB0_36
# BB#44:                                # %lor.lhs.false.i50.i.i
                                        #   in Loop: Header=BB0_29 Depth=2
	movq	40(%rsp), %rcx          # 8-byte Reload
	movslq	(%rcx,%rdx,4), %rdx
	cmpq	$-2, %rdx
	je	.LBB0_36
# BB#45:                                # %if.else10.i55.i.i
                                        #   in Loop: Header=BB0_29 Depth=2
	movq	basic_block_info(%rip), %rcx
	leaq	32(%rcx,%rdx,8), %rcx
.LBB0_34:                               # %for.cond.backedge.i.i
                                        #   in Loop: Header=BB0_29 Depth=2
	movq	(%rcx), %r15
	cmpq	%r12, %r15
	je	.LBB0_36
# BB#35:                                # %for.cond.backedge.i.i
                                        #   in Loop: Header=BB0_29 Depth=2
	cmpq	%r14, %r15
	jne	.LBB0_29
.LBB0_36:                               # %find_control_dependence.exit.loopexit.i
                                        #   in Loop: Header=BB0_16 Depth=1
	movl	4(%rax), %edi
.LBB0_37:                               # %find_control_dependence.exit.i
                                        #   in Loop: Header=BB0_16 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB0_16
.LBB0_38:                               # %find_all_control_dependences.exit
	movq	%rax, 72(%rsp)          # 8-byte Spill
	callq	get_insns
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB0_46
# BB#39:
	movabsq	$inherently_necessary_register_1, %r13
	leaq	104(%rsp), %r14
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movabsq	$4026531839, %r15       # imm = 0xEFFFFFFF
	.align	16, 0x90
.LBB0_40:                               # %if.else.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, 104(%rsp)
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	for_each_rtx
	testq	%r12, %rax
	jne	.LBB0_86
# BB#41:                                # %if.else2.i
                                        #   in Loop: Header=BB0_40 Depth=1
	movzbl	(%rbx), %eax
	addq	$-32, %rax
	andq	%r12, %rax
	cmpq	$14, %rax
	ja	.LBB0_83
# BB#42:                                # %if.else2.i
                                        #   in Loop: Header=BB0_40 Depth=1
	jmpq	*.LJTI0_0(,%rax,8)
.LBB0_80:                               # %sw.bb19.i
                                        #   in Loop: Header=BB0_40 Depth=1
	xorq	%rax, %rax
	movl	%eax, 100(%rsp)
	movq	32(%rbx), %rdi
	movabsq	$note_inherently_necessary_set, %rsi
	leaq	100(%rsp), %rdx
	callq	note_stores
	movl	100(%rsp), %eax
	testq	%rax, %rax
	jne	.LBB0_86
# BB#81:                                # %lor.lhs.false24.i
                                        #   in Loop: Header=BB0_40 Depth=1
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$40, %rax
	je	.LBB0_86
# BB#82:                                # %lor.rhs31.i
                                        #   in Loop: Header=BB0_40 Depth=1
	callq	asm_noperands
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB0_86
	jmp	.LBB0_89
.LBB0_84:                               # %land.lhs.true.i
                                        #   in Loop: Header=BB0_40 Depth=1
	movq	32(%rbx), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	je	.LBB0_86
# BB#85:                                # %find_inherently_necessary.exit
                                        #   in Loop: Header=BB0_40 Depth=1
	movq	%rbx, %rdi
	callq	computed_jump_p
	testq	%r12, %rax
	je	.LBB0_89
	.align	16, 0x90
.LBB0_86:                               # %if.then26
                                        #   in Loop: Header=BB0_40 Depth=1
	movl	(%rbx), %eax
	andq	%r15, %rax
	movl	%eax, (%rbx)
	movq	88(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB0_88
# BB#87:                                # %if.then29
                                        #   in Loop: Header=BB0_40 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, 88(%rsp)
	movq	8(%rax), %rcx
.LBB0_88:                               # %if.end33
                                        #   in Loop: Header=BB0_40 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB0_89:                               # %for.inc38
                                        #   in Loop: Header=BB0_40 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_40
.LBB0_46:                               # %while.cond.preheader
	movq	88(%rsp), %rax
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	movq	48(%rsp), %rsi          # 8-byte Reload
	je	.LBB0_72
# BB#47:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_48:                               # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_52 Depth 2
                                        #       Child Loop BB0_53 Depth 3
                                        #       Child Loop BB0_61 Depth 3
                                        #     Child Loop BB0_95 Depth 2
                                        #       Child Loop BB0_96 Depth 3
                                        #         Child Loop BB0_97 Depth 4
                                        #         Child Loop BB0_105 Depth 4
	movq	24(%rax,%rcx,8), %rdx
	decq	%rcx
	movq	%rdx, 80(%rsp)
	movq	%rcx, 8(%rax)
	movzbl	(%rdx), %eax
	movzbl	rtx_class(%rax), %ecx
	cmpq	$105, %rcx
	jne	.LBB0_71
# BB#49:                                # %land.lhs.true
                                        #   in Loop: Header=BB0_48 Depth=1
	cmpq	$33, %rax
	jne	.LBB0_51
# BB#50:                                # %land.lhs.true61
                                        #   in Loop: Header=BB0_48 Depth=1
	movq	32(%rdx), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	je	.LBB0_71
.LBB0_51:                               # %do.body77
                                        #   in Loop: Header=BB0_48 Depth=1
	movslq	8(%rdx), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	88(%rax), %eax
	addq	$2, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%rsi,%rax,8), %rax
	movq	(%rax), %r8
	testq	%r8, %r8
	movq	72(%rsp), %rdi          # 8-byte Reload
	je	.LBB0_69
	.align	16, 0x90
.LBB0_52:                               # %for.cond105.preheader
                                        #   Parent Loop BB0_48 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_53 Depth 3
                                        #       Child Loop BB0_61 Depth 3
	movq	%r8, 64(%rsp)           # 8-byte Spill
	movq	24(%r8), %rbx
	testq	%rbx, %rbx
	movq	$1, %r14
	je	.LBB0_60
	.align	16, 0x90
.LBB0_53:                               # %for.body117
                                        #   Parent Loop BB0_48 Depth=1
                                        #     Parent Loop BB0_52 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1(%r14), %rax
	movq	$1, %r13
	movq	%rax, %rcx
	shlq	%cl, %r13
	testq	%rbx, %r13
	je	.LBB0_59
# BB#54:                                # %if.then120
                                        #   in Loop: Header=BB0_53 Depth=3
	movl	16(%r8), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	addq	%rdx, %rax
	movq	$32, %rcx
	movq	8(%rdi), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%rdx,%rax,8), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %r12
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$268500991, %rcx        # imm = 0x1000FFFF
	cmpq	$268435489, %rcx        # imm = 0x10000021
	jne	.LBB0_58
# BB#55:                                # %if.then137
                                        #   in Loop: Header=BB0_53 Depth=3
	movabsq	$4026531839, %rcx       # imm = 0xEFFFFFFF
	andq	%rcx, %rax
	movl	%eax, (%r12)
	movq	88(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB0_57
# BB#56:                                # %if.then145
                                        #   in Loop: Header=BB0_53 Depth=3
	addq	%rsi, %rsi
	movq	%rax, %rdi
	movq	%r8, %r15
	callq	varray_grow
	movq	%r15, %r8
	movq	72(%rsp), %rdi          # 8-byte Reload
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%rax, 88(%rsp)
	movq	8(%rax), %rcx
.LBB0_57:                               # %if.end149
                                        #   in Loop: Header=BB0_53 Depth=3
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r12, 32(%rax,%rcx,8)
.LBB0_58:                               # %if.end156
                                        #   in Loop: Header=BB0_53 Depth=3
	notq	%r13
	andq	%r13, %rbx
	je	.LBB0_60
.LBB0_59:                               # %for.inc162
                                        #   in Loop: Header=BB0_53 Depth=3
	movq	%r14, %rax
	andq	%r15, %rax
	incq	%r14
	cmpq	$64, %rax
	jb	.LBB0_53
.LBB0_60:                               # %if.end165
                                        #   in Loop: Header=BB0_52 Depth=2
	movq	32(%r8), %r14
	xorq	%rbx, %rbx
	testq	%r14, %r14
	je	.LBB0_68
	.align	16, 0x90
.LBB0_61:                               # %for.body117.1
                                        #   Parent Loop BB0_48 Depth=1
                                        #     Parent Loop BB0_52 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$1, %r13
	movq	%rbx, %rcx
	shlq	%cl, %r13
	testq	%r14, %r13
	je	.LBB0_67
# BB#62:                                # %if.then120.1
                                        #   in Loop: Header=BB0_61 Depth=3
	movl	16(%r8), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	64(%rbx,%rax), %rax
	movq	$32, %rcx
	movq	8(%rdi), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%rdx,%rax,8), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %r12
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$268500991, %rcx        # imm = 0x1000FFFF
	cmpq	$268435489, %rcx        # imm = 0x10000021
	jne	.LBB0_66
# BB#63:                                # %if.then137.1
                                        #   in Loop: Header=BB0_61 Depth=3
	movabsq	$4026531839, %rcx       # imm = 0xEFFFFFFF
	andq	%rcx, %rax
	movl	%eax, (%r12)
	movq	88(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB0_65
# BB#64:                                # %if.then145.1
                                        #   in Loop: Header=BB0_61 Depth=3
	addq	%rsi, %rsi
	movq	%rdi, %r15
	movq	%rax, %rdi
	callq	varray_grow
	movq	%r15, %rdi
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%rax, 88(%rsp)
	movq	8(%rax), %rcx
.LBB0_65:                               # %if.end149.1
                                        #   in Loop: Header=BB0_61 Depth=3
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r12, 32(%rax,%rcx,8)
	movq	64(%rsp), %r8           # 8-byte Reload
.LBB0_66:                               # %if.end156.1
                                        #   in Loop: Header=BB0_61 Depth=3
	notq	%r13
	andq	%r13, %r14
	je	.LBB0_68
.LBB0_67:                               # %for.inc162.1
                                        #   in Loop: Header=BB0_61 Depth=3
	incq	%rbx
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	$64, %rax
	jb	.LBB0_61
.LBB0_68:                               # %if.end165.1
                                        #   in Loop: Header=BB0_52 Depth=2
	movq	(%r8), %r8
	testq	%r8, %r8
	jne	.LBB0_52
.LBB0_69:                               # %do.end172
                                        #   in Loop: Header=BB0_48 Depth=1
	leaq	80(%rsp), %rdi
	movabsq	$propagate_necessity_through_operand, %rsi
	leaq	88(%rsp), %rdx
	callq	for_each_rtx
	movq	80(%rsp), %rax
	testq	%rax, %rax
	movq	48(%rsp), %rsi          # 8-byte Reload
	je	.LBB0_71
# BB#70:                                # %land.lhs.true175
                                        #   in Loop: Header=BB0_48 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$32, %rcx
	jne	.LBB0_71
# BB#90:                                # %land.lhs.true180
                                        #   in Loop: Header=BB0_48 Depth=1
	movq	32(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB0_71
# BB#91:                                # %land.lhs.true188
                                        #   in Loop: Header=BB0_48 Depth=1
	movq	16(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$152, %rcx
	jne	.LBB0_71
# BB#92:                                # %if.then199
                                        #   in Loop: Header=BB0_48 Depth=1
	movq	8(%rax), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movl	(%rax), %eax
	leaq	-2(%rax), %rdi
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB0_71
# BB#93:                                # %for.body213.lr.ph
                                        #   in Loop: Header=BB0_48 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	.align	16, 0x90
.LBB0_95:                               # %for.body213
                                        #   Parent Loop BB0_48 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_96 Depth 3
                                        #         Child Loop BB0_97 Depth 4
                                        #         Child Loop BB0_105 Depth 4
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx,%rax,8), %rax
	movq	8(%rax), %rax
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movq	8(%rax), %rax
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	88(%rax), %eax
	addq	$2, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%rsi,%rax,8), %rax
	movq	(%rax), %r8
	testq	%r8, %r8
	movq	72(%rsp), %rdi          # 8-byte Reload
	je	.LBB0_94
	.align	16, 0x90
.LBB0_96:                               # %for.cond266.preheader
                                        #   Parent Loop BB0_48 Depth=1
                                        #     Parent Loop BB0_95 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_97 Depth 4
                                        #         Child Loop BB0_105 Depth 4
	movq	%r8, 64(%rsp)           # 8-byte Spill
	movq	24(%r8), %r14
	testq	%r14, %r14
	movq	$1, %rbx
	je	.LBB0_104
	.align	16, 0x90
.LBB0_97:                               # %for.body280
                                        #   Parent Loop BB0_48 Depth=1
                                        #     Parent Loop BB0_95 Depth=2
                                        #       Parent Loop BB0_96 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	leaq	-1(%rbx), %rax
	movq	$1, %r13
	movq	%rax, %rcx
	shlq	%cl, %r13
	testq	%r14, %r13
	je	.LBB0_103
# BB#98:                                # %if.then287
                                        #   in Loop: Header=BB0_97 Depth=4
	movl	16(%r8), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	addq	%rdx, %rax
	movq	$32, %rcx
	movq	8(%rdi), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%rdx,%rax,8), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %r12
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$268500991, %rcx        # imm = 0x1000FFFF
	cmpq	$268435489, %rcx        # imm = 0x10000021
	jne	.LBB0_102
# BB#99:                                # %if.then310
                                        #   in Loop: Header=BB0_97 Depth=4
	movabsq	$4026531839, %rcx       # imm = 0xEFFFFFFF
	andq	%rcx, %rax
	movl	%eax, (%r12)
	movq	88(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB0_101
# BB#100:                               # %if.then318
                                        #   in Loop: Header=BB0_97 Depth=4
	addq	%rsi, %rsi
	movq	%rax, %rdi
	movq	%r8, %r15
	callq	varray_grow
	movq	%r15, %r8
	movq	72(%rsp), %rdi          # 8-byte Reload
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%rax, 88(%rsp)
	movq	8(%rax), %rcx
.LBB0_101:                              # %if.end322
                                        #   in Loop: Header=BB0_97 Depth=4
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r12, 32(%rax,%rcx,8)
.LBB0_102:                              # %if.end329
                                        #   in Loop: Header=BB0_97 Depth=4
	notq	%r13
	andq	%r13, %r14
	je	.LBB0_104
.LBB0_103:                              # %for.inc335
                                        #   in Loop: Header=BB0_97 Depth=4
	movq	%rbx, %rax
	andq	%r15, %rax
	incq	%rbx
	cmpq	$64, %rax
	jb	.LBB0_97
.LBB0_104:                              # %if.end338
                                        #   in Loop: Header=BB0_96 Depth=3
	movq	32(%r8), %r14
	xorq	%rbx, %rbx
	testq	%r14, %r14
	je	.LBB0_112
	.align	16, 0x90
.LBB0_105:                              # %for.body280.1
                                        #   Parent Loop BB0_48 Depth=1
                                        #     Parent Loop BB0_95 Depth=2
                                        #       Parent Loop BB0_96 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	$1, %r13
	movq	%rbx, %rcx
	shlq	%cl, %r13
	testq	%r14, %r13
	je	.LBB0_111
# BB#106:                               # %if.then287.1
                                        #   in Loop: Header=BB0_105 Depth=4
	movl	16(%r8), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	64(%rbx,%rax), %rax
	movq	$32, %rcx
	movq	8(%rdi), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%rdx,%rax,8), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %r12
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$268500991, %rcx        # imm = 0x1000FFFF
	cmpq	$268435489, %rcx        # imm = 0x10000021
	jne	.LBB0_110
# BB#107:                               # %if.then310.1
                                        #   in Loop: Header=BB0_105 Depth=4
	movabsq	$4026531839, %rcx       # imm = 0xEFFFFFFF
	andq	%rcx, %rax
	movl	%eax, (%r12)
	movq	88(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB0_109
# BB#108:                               # %if.then318.1
                                        #   in Loop: Header=BB0_105 Depth=4
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, 88(%rsp)
	movq	8(%rax), %rcx
.LBB0_109:                              # %if.end322.1
                                        #   in Loop: Header=BB0_105 Depth=4
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r12, 32(%rax,%rcx,8)
	movq	64(%rsp), %r8           # 8-byte Reload
.LBB0_110:                              # %if.end329.1
                                        #   in Loop: Header=BB0_105 Depth=4
	notq	%r13
	andq	%r13, %r14
	je	.LBB0_112
.LBB0_111:                              # %for.inc335.1
                                        #   in Loop: Header=BB0_105 Depth=4
	incq	%rbx
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	$64, %rax
	jb	.LBB0_105
.LBB0_112:                              # %if.end338.1
                                        #   in Loop: Header=BB0_96 Depth=3
	movq	(%r8), %r8
	testq	%r8, %r8
	jne	.LBB0_96
.LBB0_94:                               # %for.cond210.loopexit
                                        #   in Loop: Header=BB0_95 Depth=2
	movq	32(%rsp), %rdi          # 8-byte Reload
	addq	$-2, %rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	leaq	-2(%rdx), %rdx
	movq	48(%rsp), %rsi          # 8-byte Reload
	jns	.LBB0_95
	.align	16, 0x90
.LBB0_71:                               # %while.cond.backedge
                                        #   in Loop: Header=BB0_48 Depth=1
	movq	88(%rsp), %rax
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	jne	.LBB0_48
.LBB0_72:                               # %while.end351
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	callq	get_insns
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB0_150
# BB#73:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_74:                               # %for.body357
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_118 Depth 2
                                        #       Child Loop BB0_121 Depth 3
                                        #     Child Loop BB0_132 Depth 2
                                        #       Child Loop BB0_135 Depth 3
	movzbl	3(%r14), %eax
	testq	$16, %rax
	je	.LBB0_149
# BB#75:                                # %if.then362
                                        #   in Loop: Header=BB0_74 Depth=1
	movq	%r14, %rdi
	callq	any_condjump_p
	testq	%r13, %rax
	jne	.LBB0_76
# BB#146:                               # %if.else
                                        #   in Loop: Header=BB0_74 Depth=1
	movzbl	(%r14), %eax
	cmpq	$33, %rax
	je	.LBB0_149
# BB#147:                               # %if.end.i741
                                        #   in Loop: Header=BB0_74 Depth=1
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_149
# BB#148:                               # %if.end3.i
                                        #   in Loop: Header=BB0_74 Depth=1
	movq	%r14, %rdi
	callq	delete_insn
	jmp	.LBB0_149
	.align	16, 0x90
.LBB0_76:                               # %if.then365
                                        #   in Loop: Header=BB0_74 Depth=1
	movslq	8(%r14), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %r15
	testq	%r15, %r15
	je	.LBB0_22
# BB#77:                                # %if.end.i
                                        #   in Loop: Header=BB0_74 Depth=1
	movslq	88(%r15), %rax
	cmpq	$-3, %rax
	je	.LBB0_24
# BB#78:                                # %if.end2.i
                                        #   in Loop: Header=BB0_74 Depth=1
	movabsq	$entry_exit_blocks, %rcx
	cmpq	%rcx, %r15
	jne	.LBB0_113
# BB#79:                                # %if.then4.i
                                        #   in Loop: Header=BB0_74 Depth=1
	movq	basic_block_info(%rip), %rax
	addq	$32, %rax
	jmp	.LBB0_116
.LBB0_113:                              # %if.else.i725
                                        #   in Loop: Header=BB0_74 Depth=1
	movabsq	$entry_exit_blocks+112, %rcx
	cmpq	%rcx, %r15
	je	.LBB0_130
# BB#114:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB0_74 Depth=1
	movq	40(%rsp), %rcx          # 8-byte Reload
	movslq	(%rcx,%rax,4), %rax
	cmpq	$-2, %rax
	jne	.LBB0_115
.LBB0_130:                              # %find_pdom.exit.thread
                                        #   in Loop: Header=BB0_74 Depth=1
	movq	40(%r15), %r12
	jmp	.LBB0_131
.LBB0_115:                              # %if.else10.i
                                        #   in Loop: Header=BB0_74 Depth=1
	movq	basic_block_info(%rip), %rcx
	leaq	32(%rcx,%rax,8), %rax
.LBB0_116:                              # %find_pdom.exit
                                        #   in Loop: Header=BB0_74 Depth=1
	movq	(%rax), %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	40(%r15), %r12
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %rcx
	jne	.LBB0_117
.LBB0_131:                              # %while.cond378.preheader
                                        #   in Loop: Header=BB0_74 Depth=1
	testq	%r12, %r12
	je	.LBB0_142
	.align	16, 0x90
.LBB0_132:                              # %while.body380
                                        #   Parent Loop BB0_74 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_135 Depth 3
	movzbl	48(%r12), %eax
	movq	8(%r12), %r15
	testq	$1, %rax
	jne	.LBB0_141
# BB#133:                               # %if.then384
                                        #   in Loop: Header=BB0_132 Depth=2
	movq	24(%r12), %rdi
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %rdi
	je	.LBB0_140
# BB#134:                               # %if.then387
                                        #   in Loop: Header=BB0_132 Depth=2
	callq	first_insn_after_basic_block_note
	movq	%rax, %rbx
	jmp	.LBB0_135
	.align	16, 0x90
.LBB0_139:                              # %while.body418
                                        #   in Loop: Header=BB0_135 Depth=3
	movq	16(%r12), %rsi
	callq	remove_phi_alternative
	movq	24(%rbx), %rbx
.LBB0_135:                              # %if.then387
                                        #   Parent Loop BB0_74 Depth=1
                                        #     Parent Loop BB0_132 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB0_140
# BB#136:                               # %land.lhs.true393
                                        #   in Loop: Header=BB0_135 Depth=3
	movzbl	(%rbx), %eax
	cmpq	$32, %rax
	jne	.LBB0_140
# BB#137:                               # %land.lhs.true398
                                        #   in Loop: Header=BB0_135 Depth=3
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$47, %rax
	jne	.LBB0_140
# BB#138:                               # %land.rhs406
                                        #   in Loop: Header=BB0_135 Depth=3
	movq	16(%rdi), %rax
	movzbl	(%rax), %eax
	cmpq	$152, %rax
	je	.LBB0_139
	.align	16, 0x90
.LBB0_140:                              # %if.end428
                                        #   in Loop: Header=BB0_132 Depth=2
	movq	%r12, %rdi
	callq	remove_edge
.LBB0_141:                              # %while.cond378.backedge
                                        #   in Loop: Header=BB0_132 Depth=2
	testq	%r15, %r15
	movq	%r15, %r12
	jne	.LBB0_132
.LBB0_142:                              # %while.end430
                                        #   in Loop: Header=BB0_74 Depth=1
	movq	$37, %rax
	movw	%ax, (%r14)
	movabsq	$4294967197, %rax       # imm = 0xFFFFFF9D
	movl	%eax, 40(%r14)
	jmp	.LBB0_149
.LBB0_117:                              # %while.cond438.preheader
                                        #   in Loop: Header=BB0_74 Depth=1
	testq	%r12, %r12
	je	.LBB0_128
	.align	16, 0x90
.LBB0_118:                              # %while.body440
                                        #   Parent Loop BB0_74 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_121 Depth 3
	movzbl	48(%r12), %eax
	movq	8(%r12), %r13
	testq	$2, %rax
	jne	.LBB0_127
# BB#119:                               # %if.end447
                                        #   in Loop: Header=BB0_118 Depth=2
	movq	24(%r12), %rdi
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %rdi
	je	.LBB0_126
# BB#120:                               # %if.then451
                                        #   in Loop: Header=BB0_118 Depth=2
	callq	first_insn_after_basic_block_note
	movq	%rax, %rbx
	jmp	.LBB0_121
	.align	16, 0x90
.LBB0_125:                              # %while.body482
                                        #   in Loop: Header=BB0_121 Depth=3
	movq	16(%r12), %rsi
	callq	remove_phi_alternative
	movq	24(%rbx), %rbx
.LBB0_121:                              # %if.then451
                                        #   Parent Loop BB0_74 Depth=1
                                        #     Parent Loop BB0_118 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB0_126
# BB#122:                               # %land.lhs.true457
                                        #   in Loop: Header=BB0_121 Depth=3
	movzbl	(%rbx), %eax
	cmpq	$32, %rax
	jne	.LBB0_126
# BB#123:                               # %land.lhs.true462
                                        #   in Loop: Header=BB0_121 Depth=3
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$47, %rax
	jne	.LBB0_126
# BB#124:                               # %land.rhs470
                                        #   in Loop: Header=BB0_121 Depth=3
	movq	16(%rdi), %rax
	movzbl	(%rax), %eax
	cmpq	$152, %rax
	je	.LBB0_125
	.align	16, 0x90
.LBB0_126:                              # %if.end492
                                        #   in Loop: Header=BB0_118 Depth=2
	movq	%r12, %rdi
	callq	remove_edge
.LBB0_127:                              # %while.cond438.backedge
                                        #   in Loop: Header=BB0_118 Depth=2
	testq	%r13, %r13
	movq	%r13, %r12
	jne	.LBB0_118
.LBB0_128:                              # %while.end493
                                        #   in Loop: Header=BB0_74 Depth=1
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rsi
	callq	make_edge
	movq	(%r12), %r15
	movzbl	(%r15), %eax
	cmpq	$36, %rax
	jne	.LBB0_143
# BB#129:                               #   in Loop: Header=BB0_74 Depth=1
	movq	%r15, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jmp	.LBB0_145
.LBB0_143:                              # %if.else.i732
                                        #   in Loop: Header=BB0_74 Depth=1
	callq	gen_label_rtx
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	emit_label_before
	movq	(%r12), %rcx
	movq	%rax, %rbx
	cmpq	%rcx, %r15
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jne	.LBB0_145
# BB#144:                               # %if.then4.i733
                                        #   in Loop: Header=BB0_74 Depth=1
	movq	%rbx, (%r12)
.LBB0_145:                              # %find_block_label.exit
                                        #   in Loop: Header=BB0_74 Depth=1
	movq	$67, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_u00
	movq	32(%r14), %rcx
	movq	%rax, 16(%rcx)
	movl	%r13d, 40(%r14)
	movq	%rbx, 64(%r14)
	movl	32(%rbx), %eax
	incq	%rax
	movl	%eax, 32(%rbx)
	movq	%r14, %rdi
	callq	emit_barrier_after
	.align	16, 0x90
.LBB0_149:                              # %for.inc522
                                        #   in Loop: Header=BB0_74 Depth=1
	movq	24(%r14), %r14
	testq	%r14, %r14
	jne	.LBB0_74
.LBB0_150:                              # %for.end526
	callq	remove_fake_edges
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_157
# BB#151:
	movq	$4, %rbx
	.align	16, 0x90
.LBB0_152:                              # %for.body530
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	(%rcx,%rbx,8), %rcx
	cmpq	$0, 40(%rcx)
	jne	.LBB0_156
# BB#153:                               # %if.then539
                                        #   in Loop: Header=BB0_152 Depth=1
	movq	8(%rcx), %rdi
	movq	24(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB0_155
# BB#154:                               # %lor.lhs.false546
                                        #   in Loop: Header=BB0_152 Depth=1
	movzbl	(%rcx), %ecx
	cmpq	$35, %rcx
	je	.LBB0_156
.LBB0_155:                              # %if.then551
                                        #   in Loop: Header=BB0_152 Depth=1
	callq	emit_barrier_after
	movl	n_basic_blocks(%rip), %eax
.LBB0_156:                              # %for.inc556
                                        #   in Loop: Header=BB0_152 Depth=1
	leaq	-3(%rbx), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rsi, %rdx
	jl	.LBB0_152
.LBB0_157:                              # %for.end558
	callq	get_insns
	testq	%rax, %rax
	movq	48(%rsp), %r14          # 8-byte Reload
	je	.LBB0_160
# BB#158:
	movabsq	$4026531839, %rcx       # imm = 0xEFFFFFFF
	.align	16, 0x90
.LBB0_159:                              # %for.body563
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rax), %edx
	andq	%rcx, %rdx
	movl	%edx, (%rax)
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	.LBB0_159
.LBB0_160:                              # %for.end570
	movq	88(%rsp), %rdi
	cmpq	$0, 8(%rdi)
	jne	.LBB0_161
# BB#162:                               # %do.body576
	testq	%rdi, %rdi
	je	.LBB0_164
# BB#163:                               # %if.then578
	callq	free
	movq	$0, 88(%rsp)
.LBB0_164:                              # %do.end580
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_169
# BB#165:
	movq	$1, %rbx
	.align	16, 0x90
.LBB0_166:                              # %do.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r14,%rbx,8), %rdi
	testq	%rdi, %rdi
	je	.LBB0_168
# BB#167:                               # %if.then.i749
                                        #   in Loop: Header=BB0_166 Depth=1
	callq	bitmap_clear
	movq	-8(%r14,%rbx,8), %rdi
	callq	free
	movq	$0, -8(%r14,%rbx,8)
.LBB0_168:                              # %for.inc.i
                                        #   in Loop: Header=BB0_166 Depth=1
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB0_166
.LBB0_169:                              # %control_dependent_block_to_edge_map_free.exit
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	free
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	free_edge_list
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_83:                               # %sw.default.i
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.find_inherently_necessary, %rdx
	movq	$399, %rsi              # imm = 0x18F
	callq	fancy_abort
.LBB0_170:                              # %if.then.i29.i.i
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.set_control_dependent_block_to_edge_map_bit, %rdx
	movq	$185, %rsi
	callq	fancy_abort
.LBB0_171:                              # %if.then.i35.i.i
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.find_pdom, %rdx
	movq	$273, %rsi              # imm = 0x111
	callq	fancy_abort
.LBB0_172:                              # %if.then1.i39.i.i
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.find_pdom, %rdx
	movq	$275, %rsi              # imm = 0x113
	callq	fancy_abort
.LBB0_17:                               # %if.then.i.i
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.find_control_dependence, %rcx
	movq	$247, %rsi
	jmp	.LBB0_18
.LBB0_22:                               # %if.then.i.i.i
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.find_pdom, %rcx
	movq	$273, %rsi              # imm = 0x111
	jmp	.LBB0_18
.LBB0_24:                               # %if.then1.i.i.i
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.find_pdom, %rcx
	movq	$275, %rsi              # imm = 0x113
	jmp	.LBB0_18
.LBB0_161:                              # %if.then574
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.ssa_eliminate_dead_code, %rcx
	movq	$737, %rsi              # imm = 0x2E1
.LBB0_18:                               # %if.then.i.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp17:
	.size	ssa_eliminate_dead_code, .Ltmp17-ssa_eliminate_dead_code
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI0_0:
	.quad	.LBB0_80
	.quad	.LBB0_84
	.quad	.LBB0_86
	.quad	.LBB0_86
	.quad	.LBB0_89
	.quad	.LBB0_89
	.quad	.LBB0_83
	.quad	.LBB0_83
	.quad	.LBB0_83
	.quad	.LBB0_83
	.quad	.LBB0_83
	.quad	.LBB0_83
	.quad	.LBB0_83
	.quad	.LBB0_83
	.quad	.LBB0_86

	.text
	.align	16, 0x90
	.type	propagate_necessity_through_operand,@function
propagate_necessity_through_operand:    # @propagate_necessity_through_operand
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp21:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp22:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp23:
	.cfi_def_cfa_offset 32
.Ltmp24:
	.cfi_offset %rbx, -24
.Ltmp25:
	.cfi_offset %r14, -16
	movq	(%rdi), %rax
	movq	%rsi, %rbx
	testq	%rax, %rax
	je	.LBB1_9
# BB#1:                                 # %if.end
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB1_9
# BB#2:                                 # %sw.bb
	movl	8(%rax), %eax
	cmpq	$52, %rax
	ja	.LBB1_4
# BB#3:                                 # %sw.bb
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rax, %rcx
	cmpq	$16, %rcx
	jne	.LBB1_9
.LBB1_4:                                # %if.then11
	movq	ssa_definition(%rip), %rcx
	movq	32(%rcx,%rax,8), %r14
	testq	%r14, %r14
	je	.LBB1_9
# BB#5:                                 # %land.lhs.true
	movl	(%r14), %eax
	testq	$268435456, %rax        # imm = 0x10000000
	je	.LBB1_9
# BB#6:                                 # %if.then20
	movabsq	$4026531839, %rcx       # imm = 0xEFFFFFFF
	andq	%rcx, %rax
	movl	%eax, (%r14)
	movq	(%rbx), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB1_8
# BB#7:                                 # %if.then24
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, (%rbx)
	movq	8(%rax), %rcx
.LBB1_8:                                # %if.end26
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB1_9:                                # %return
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp26:
	.size	propagate_necessity_through_operand, .Ltmp26-propagate_necessity_through_operand
	.cfi_endproc

	.align	16, 0x90
	.type	note_inherently_necessary_set,@function
note_inherently_necessary_set:          # @note_inherently_necessary_set
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB2_1
	.align	16, 0x90
.LBB2_6:                                # %while.body
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	8(%rdi), %rdi
.LBB2_1:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdi), %ecx
	cmpq	$131, %rcx
	jg	.LBB2_5
# BB#2:                                 # %while.cond
                                        #   in Loop: Header=BB2_1 Depth=1
	addq	$-42, %rcx
	andq	%rax, %rcx
	cmpq	$24, %rcx
	ja	.LBB2_8
# BB#3:                                 # %while.cond
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$16777219, %rsi         # imm = 0x1000003
	jne	.LBB2_7
# BB#4:                                 # %while.cond
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$6291456, %rsi          # imm = 0x600000
	jne	.LBB2_6
	jmp	.LBB2_8
	.align	16, 0x90
.LBB2_5:                                # %while.cond
                                        #   in Loop: Header=BB2_1 Depth=1
	addq	$-132, %rcx
	andq	%rax, %rcx
	cmpq	$1, %rcx
	jbe	.LBB2_6
	jmp	.LBB2_8
.LBB2_7:                                # %if.then
	movq	$1, %rax
	movl	%eax, (%rdx)
.LBB2_8:                                # %if.end
	retq
.Ltmp27:
	.size	note_inherently_necessary_set, .Ltmp27-note_inherently_necessary_set
	.cfi_endproc

	.align	16, 0x90
	.type	inherently_necessary_register_1,@function
inherently_necessary_register_1:        # @inherently_necessary_register_1
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB3_3
# BB#1:                                 # %if.end
	movzbl	(%rcx), %edx
	cmpq	$49, %rdx
	je	.LBB3_2
# BB#4:                                 # %if.end
	cmpq	$61, %rdx
	jne	.LBB3_5
# BB#6:                                 # %sw.bb2
	movl	8(%rcx), %edi
	cmpq	$52, %rdi
	movabsq	$0, %rdx
	movabsq	$1, %r8
	movq	%rdx, %rax
	jbe	.LBB3_8
# BB#7:                                 # %sw.bb2
	movq	%r8, %rax
.LBB3_8:                                # %sw.bb2
	cmpq	$17, %rdi
	movq	%rdx, %rsi
	jne	.LBB3_10
# BB#9:                                 # %sw.bb2
	movq	%r8, %rsi
.LBB3_10:                               # %sw.bb2
	orq	%rsi, %rax
	cmpq	$16, %rdi
	movq	%rdx, %rdi
	jne	.LBB3_12
# BB#11:                                # %sw.bb2
	movq	%r8, %rdi
.LBB3_12:                               # %sw.bb2
	movq	global_rtl(%rip), %rsi
	orq	%rdi, %rax
	cmpq	%rsi, %rcx
	jne	.LBB3_14
# BB#13:                                # %sw.bb2
	movq	%r8, %rdx
.LBB3_14:                               # %sw.bb2
	orq	%rdx, %rax
	notq	%rax
	andq	$1, %rax
	retq
.LBB3_2:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
.LBB3_3:                                # %return
	retq
.LBB3_5:                                # %if.end
	cmpq	$59, %rdx
	retq
.Ltmp28:
	.size	inherently_necessary_register_1, .Ltmp28-inherently_necessary_register_1
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"unprocessed instructions"
	.size	.L.str, 25

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"ssa-dce.c"
	.size	.L.str1, 10

	.type	.L__FUNCTION__.ssa_eliminate_dead_code,@object # @__FUNCTION__.ssa_eliminate_dead_code
.L__FUNCTION__.ssa_eliminate_dead_code:
	.asciz	"ssa_eliminate_dead_code"
	.size	.L__FUNCTION__.ssa_eliminate_dead_code, 24

	.type	.L__FUNCTION__.find_pdom,@object # @__FUNCTION__.find_pdom
.L__FUNCTION__.find_pdom:
	.asciz	"find_pdom"
	.size	.L__FUNCTION__.find_pdom, 10

	.type	.L__FUNCTION__.find_inherently_necessary,@object # @__FUNCTION__.find_inherently_necessary
.L__FUNCTION__.find_inherently_necessary:
	.asciz	"find_inherently_necessary"
	.size	.L__FUNCTION__.find_inherently_necessary, 26

	.type	.L__FUNCTION__.find_control_dependence,@object # @__FUNCTION__.find_control_dependence
.L__FUNCTION__.find_control_dependence:
	.asciz	"find_control_dependence"
	.size	.L__FUNCTION__.find_control_dependence, 24

	.type	.L__FUNCTION__.set_control_dependent_block_to_edge_map_bit,@object # @__FUNCTION__.set_control_dependent_block_to_edge_map_bit
.L__FUNCTION__.set_control_dependent_block_to_edge_map_bit:
	.asciz	"set_control_dependent_block_to_edge_map_bit"
	.size	.L__FUNCTION__.set_control_dependent_block_to_edge_map_bit, 44


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
