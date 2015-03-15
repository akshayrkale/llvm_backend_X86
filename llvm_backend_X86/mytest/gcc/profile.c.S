	.file	"profile.c.bc"
	.text
	.globl	branch_prob
	.align	16, 0x90
	.type	branch_prob,@function
branch_prob:                            # @branch_prob
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
	subq	$144, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 192
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
	movl	flag_test_coverage(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_2
# BB#1:                                 # %if.then
	movq	cfun(%rip), %rax
	movq	40(%rax), %rdi
	movq	$-2, %rsi
	callq	output_gcov_string
.LBB0_2:                                # %if.end
	movl	total_num_times_called(%rip), %eax
	incq	%rax
	movl	%eax, total_num_times_called(%rip)
	xorq	%rdi, %rdi
	xorq	%rbx, %rbx
	callq	flow_call_edges_add
	callq	add_noreturn_fake_exit_edges
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_45
# BB#3:
	movabsq	$entry_exit_blocks, %r13
	movabsq	$entry_exit_blocks+112, %r15
	.align	16, 0x90
.LBB0_4:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_13 Depth 2
                                        #     Child Loop BB0_6 Depth 2
                                        #     Child Loop BB0_21 Depth 2
                                        #     Child Loop BB0_28 Depth 2
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rbx,8), %rbx
	movq	8(%rbx), %rax
	movq	(%rbx), %r14
	movq	24(%rax), %rax
	cmpq	%rax, %r14
	je	.LBB0_20
# BB#5:                                 # %for.body5.lr.ph
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_13
	.align	16, 0x90
.LBB0_6:                                # %for.body5.us
                                        #   Parent Loop BB0_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r14), %eax
	cmpq	$34, %rax
	jne	.LBB0_12
# BB#7:                                 # %land.lhs.true.us
                                        #   in Loop: Header=BB0_6 Depth=2
	movq	$28, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB0_8
.LBB0_12:                               # %for.inc.us
                                        #   in Loop: Header=BB0_6 Depth=2
	movq	8(%rbx), %rax
	movq	24(%r14), %r14
	movq	24(%rax), %rax
	cmpq	%rax, %r14
	jne	.LBB0_6
	jmp	.LBB0_20
	.align	16, 0x90
.LBB0_13:                               # %for.body5
                                        #   Parent Loop BB0_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r14), %eax
	cmpq	$34, %rax
	jne	.LBB0_19
# BB#14:                                # %land.lhs.true
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	$28, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB0_19
# BB#15:                                # %if.then9
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	je	.LBB0_17
# BB#16:                                # %lor.lhs.false
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	24(%rax), %rax
	cmpq	%rax, %r14
	jne	.LBB0_17
# BB#18:                                # %if.end27
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	$16, %rdx
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	make_edge
.LBB0_19:                               # %for.inc
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	8(%rbx), %rax
	movq	24(%r14), %r14
	movq	24(%rax), %rax
	cmpq	%rax, %r14
	jne	.LBB0_13
	jmp	.LBB0_20
.LBB0_8:                                # %if.then9.us
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	je	.LBB0_17
# BB#9:                                 # %lor.lhs.false.us
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	24(%rax), %rax
	cmpq	%rax, %r14
	je	.LBB0_10
.LBB0_17:                               # %if.then19
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	16(%r14), %rsi
	movq	%rbx, %rdi
	callq	split_block
	movq	24(%rax), %rsi
	movq	$16, %rdx
	movq	%r13, %rdi
	callq	make_edge
	.align	16, 0x90
.LBB0_20:                               # %for.end
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	40(%rbx), %rdx
	xorq	%r14, %r14
	testq	%rdx, %rdx
	movabsq	$0, %rax
	movabsq	$0, %rcx
	je	.LBB0_27
	.align	16, 0x90
.LBB0_21:                               # %for.body36
                                        #   Parent Loop BB0_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	48(%rdx), %edi
	movq	24(%rdx), %rsi
	testq	$6, %rdi
	je	.LBB0_24
# BB#22:                                # %land.lhs.true38
                                        #   in Loop: Header=BB0_21 Depth=2
	cmpq	%r15, %rsi
	je	.LBB0_24
# BB#23:                                # %land.lhs.true38
                                        #   in Loop: Header=BB0_21 Depth=2
	movq	$1, %rax
.LBB0_24:                               # %if.end42
                                        #   in Loop: Header=BB0_21 Depth=2
	cmpq	%r15, %rsi
	movq	$1, %rsi
	je	.LBB0_26
# BB#25:                                # %if.end42
                                        #   in Loop: Header=BB0_21 Depth=2
	movq	%rcx, %rsi
.LBB0_26:                               # %if.end42
                                        #   in Loop: Header=BB0_21 Depth=2
	movq	8(%rdx), %rdx
	testq	%rdx, %rdx
	movq	%rsi, %rcx
	jne	.LBB0_21
.LBB0_27:                               # %for.end48
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	32(%rbx), %rdx
	testq	%rdx, %rdx
	movabsq	$0, %r12
	je	.LBB0_34
	.align	16, 0x90
.LBB0_28:                               # %for.body51
                                        #   Parent Loop BB0_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	48(%rdx), %edi
	movq	16(%rdx), %rsi
	testq	$6, %rdi
	je	.LBB0_31
# BB#29:                                # %land.lhs.true55
                                        #   in Loop: Header=BB0_28 Depth=2
	cmpq	%r13, %rsi
	je	.LBB0_31
# BB#30:                                # %land.lhs.true55
                                        #   in Loop: Header=BB0_28 Depth=2
	movq	$1, %r14
.LBB0_31:                               # %if.end58
                                        #   in Loop: Header=BB0_28 Depth=2
	cmpq	%r13, %rsi
	movq	$1, %rsi
	je	.LBB0_33
# BB#32:                                # %if.end58
                                        #   in Loop: Header=BB0_28 Depth=2
	movq	%r12, %rsi
.LBB0_33:                               # %if.end58
                                        #   in Loop: Header=BB0_28 Depth=2
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	movq	%rsi, %r12
	jne	.LBB0_28
.LBB0_34:                               # %for.end64
                                        #   in Loop: Header=BB0_4 Depth=1
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	movq	%rdx, %rax
	je	.LBB0_39
# BB#35:                                # %for.end64
                                        #   in Loop: Header=BB0_4 Depth=1
	testq	%rax, %rcx
	jne	.LBB0_39
# BB#36:                                # %if.then68
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_38
# BB#37:                                # %if.then70
                                        #   in Loop: Header=BB0_4 Depth=1
	movl	88(%rbx), %edx
	xorq	%rax, %rax
	movabsq	$.L.str1, %rsi
	callq	fprintf
.LBB0_38:                               # %if.end72
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	$16, %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	make_edge
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
.LBB0_39:                               # %if.end74
                                        #   in Loop: Header=BB0_4 Depth=1
	testq	%rax, %r14
	je	.LBB0_44
# BB#40:                                # %if.end74
                                        #   in Loop: Header=BB0_4 Depth=1
	testq	%rax, %r12
	jne	.LBB0_44
# BB#41:                                # %if.then78
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_43
# BB#42:                                # %if.then80
                                        #   in Loop: Header=BB0_4 Depth=1
	movl	88(%rbx), %edx
	xorq	%rax, %rax
	movabsq	$.L.str2, %rsi
	callq	fprintf
.LBB0_43:                               # %if.end83
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	$16, %rdx
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	make_edge
.LBB0_44:                               # %for.inc86
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	40(%rsp), %rbx          # 8-byte Reload
	incq	%rbx
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB0_4
.LBB0_45:                               # %for.end88
	callq	create_edge_list
	movq	%rax, %rbx
	movslq	4(%rbx), %r14
	movq	$4, %rdi
	callq	alloc_aux_for_edges
	movq	%rbx, %r11
	xorq	%r9, %r9
	testq	%r14, %r14
	jle	.LBB0_52
# BB#46:                                # %for.body93.lr.ph
	xorq	%r9, %r9
	movq	$1, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%r14, %rdx
	andq	%rcx, %rdx
	movabsq	$entry_exit_blocks, %rsi
	movabsq	$entry_exit_blocks+112, %r8
	.align	16, 0x90
.LBB0_47:                               # %for.body93
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r11), %rbx
	movq	-8(%rbx,%rax,8), %rbx
	movq	$0, 56(%rbx)
	movzbl	48(%rbx), %edi
	testq	$6, %rdi
	je	.LBB0_51
# BB#48:                                # %land.lhs.true100
                                        #   in Loop: Header=BB0_47 Depth=1
	movq	16(%rbx), %rdi
	cmpq	%rsi, %rdi
	je	.LBB0_51
# BB#49:                                # %land.lhs.true103
                                        #   in Loop: Header=BB0_47 Depth=1
	movq	24(%rbx), %rdi
	cmpq	%r8, %rdi
	je	.LBB0_51
# BB#50:                                # %if.then106
                                        #   in Loop: Header=BB0_47 Depth=1
	movq	40(%rbx), %rdi
	movzbl	(%rdi), %ebx
	orq	$4, %rbx
	movb	%bl, (%rdi)
	incq	%r9
	.align	16, 0x90
.LBB0_51:                               # %for.inc111
                                        #   in Loop: Header=BB0_47 Depth=1
	movq	%rax, %rdi
	andq	%rcx, %rdi
	incq	%rax
	cmpq	%rdx, %rdi
	jne	.LBB0_47
.LBB0_52:                               # %for.end113
	movl	flag_test_coverage(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_76
# BB#53:                                # %for.cond117.preheader
	movq	%r9, 32(%rsp)           # 8-byte Spill
	movq	%r14, 40(%rsp)          # 8-byte Spill
	movq	%r11, 24(%rsp)          # 8-byte Spill
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_75
# BB#54:
	xorq	%r15, %r15
	xorq	%r12, %r12
	movabsq	$4294967212, %rsi       # imm = 0xFFFFFFAC
	.align	16, 0x90
.LBB0_55:                               # %for.body119
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_59 Depth 2
	movq	%rsi, %r14
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r15,8), %r13
	movq	(%r13), %rdi
	callq	prev_nonnote_insn
	testq	%rax, %rax
	jne	.LBB0_57
# BB#56:                                # %if.then129
                                        #   in Loop: Header=BB0_55 Depth=1
	callq	get_insns
	movq	%rax, %rbx
	jmp	.LBB0_58
	.align	16, 0x90
.LBB0_57:                               # %if.else131
                                        #   in Loop: Header=BB0_55 Depth=1
	movq	24(%rax), %rbx
.LBB0_58:                               # %if.end135
                                        #   in Loop: Header=BB0_55 Depth=1
	movq	bb_file(%rip), %rcx
	movl	%r12d, 52(%rsp)
	movq	$1, %rsi
	movq	$4, %rdx
	leaq	52(%rsp), %rdi
	callq	fwrite
	movq	8(%r13), %rax
	cmpq	%rax, %rbx
	movq	$1, %rdx
	movq	%r14, %rsi
	je	.LBB0_74
	.align	16, 0x90
.LBB0_59:                               # %while.body
                                        #   Parent Loop BB0_55 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB0_73
# BB#60:                                # %if.then142
                                        #   in Loop: Header=BB0_59 Depth=2
	movl	40(%rbx), %r14d
	cmpq	%rsi, %r14
	jne	.LBB0_62
# BB#61:                                # %if.then146
                                        #   in Loop: Header=BB0_59 Depth=2
	movb	%dl, branch_prob.ignore_next_note(%rip)
	jmp	.LBB0_73
	.align	16, 0x90
.LBB0_62:                               # %if.else147
                                        #   in Loop: Header=BB0_59 Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_73
# BB#63:                                # %if.then152
                                        #   in Loop: Header=BB0_59 Depth=2
	movzbl	branch_prob.ignore_next_note(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB0_65
# BB#64:                                # %if.then154
                                        #   in Loop: Header=BB0_59 Depth=2
	movb	%r12b, branch_prob.ignore_next_note(%rip)
	jmp	.LBB0_73
.LBB0_65:                               # %if.else155
                                        #   in Loop: Header=BB0_59 Depth=2
	movq	last_bb_file_name(%rip), %r12
	testq	%r12, %r12
	je	.LBB0_68
# BB#66:                                # %lor.lhs.false157
                                        #   in Loop: Header=BB0_59 Depth=2
	movq	32(%rbx), %rdi
	movq	%r12, %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_69
# BB#67:                                # %if.then164
                                        #   in Loop: Header=BB0_59 Depth=2
	movq	%r12, %rdi
	callq	free
.LBB0_68:                               # %if.end165
                                        #   in Loop: Header=BB0_59 Depth=2
	movq	32(%rbx), %rdi
	callq	xstrdup
	movq	%rax, last_bb_file_name(%rip)
	movq	32(%rbx), %rdi
	movq	$-1, %rsi
	callq	output_gcov_string
	movl	40(%rbx), %r14d
.LBB0_69:                               # %if.end173
                                        #   in Loop: Header=BB0_59 Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$63, %rcx
	movq	%rax, %rdx
	sarq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rdx, %rax
	movq	bb_file(%rip), %r8
	xorq	%rdx, %rax
	movb	%al, 52(%rsp)
	movq	$63, %rcx
	movq	%rax, %rsi
	sarq	%cl, %rsi
	movq	$56, %rcx
	movq	%rsi, %rdx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	movq	$8, %rcx
	shrq	%cl, %rdx
	movb	%dl, 53(%rsp)
	movq	$48, %rcx
	movq	%rsi, %rdx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	movq	$16, %rcx
	shrq	%cl, %rdx
	movb	%dl, 54(%rsp)
	movq	$40, %rcx
	shrq	%cl, %rsi
	addq	%rax, %rsi
	movq	$24, %rcx
	shrq	%cl, %rsi
	cqto
	movabsq	$4294967296, %rcx       # imm = 0x100000000
	idivq	%rcx
	andq	$127, %rsi
	incq	%rax
	cmpq	$2, %rax
	jb	.LBB0_71
# BB#70:                                # %__store_gcov_type.exit.thread2.i
                                        #   in Loop: Header=BB0_59 Depth=2
	movb	%sil, 55(%rsp)
	jmp	.LBB0_72
.LBB0_71:                               # %if.else.i
                                        #   in Loop: Header=BB0_59 Depth=2
	movq	$24, %rcx
	shrq	%cl, %r14
	andq	$128, %r14
	orq	%r14, %rsi
	movb	%sil, 55(%rsp)
	movq	$1, %rsi
	movq	$4, %rdx
	leaq	52(%rsp), %rdi
	movq	%r8, %rcx
	callq	fwrite
.LBB0_72:                               # %if.end181
                                        #   in Loop: Header=BB0_59 Depth=2
	xorq	%r12, %r12
	movq	$1, %rdx
	movabsq	$4294967212, %rsi       # imm = 0xFFFFFFAC
	.align	16, 0x90
.LBB0_73:                               # %if.end181
                                        #   in Loop: Header=BB0_59 Depth=2
	movq	24(%rbx), %rbx
	movq	8(%r13), %rax
	cmpq	%rax, %rbx
	jne	.LBB0_59
.LBB0_74:                               # %for.inc185
                                        #   in Loop: Header=BB0_55 Depth=1
	incq	%r15
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB0_55
.LBB0_75:                               # %for.end187
	movq	bb_file(%rip), %rcx
	xorq	%rax, %rax
	movb	%al, 52(%rsp)
	movb	%al, 53(%rsp)
	movb	%al, 54(%rsp)
	movb	%al, 55(%rsp)
	leaq	52(%rsp), %rdi
	movq	$1, %rsi
	movq	$4, %rdx
	callq	fwrite
	movq	24(%rsp), %r11          # 8-byte Reload
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	32(%rsp), %r9           # 8-byte Reload
.LBB0_76:                               # %if.end189
	movl	4(%r11), %r8d
	movslq	n_basic_blocks(%rip), %rcx
	movq	$entry_exit_blocks+112, entry_exit_blocks+192(%rip)
	movq	$entry_exit_blocks, entry_exit_blocks+80(%rip)
	testq	%rcx, %rcx
	movabsq	$entry_exit_blocks+112, %rax
	jle	.LBB0_79
# BB#77:
	movq	$4, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	.align	16, 0x90
.LBB0_78:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rdi
	movq	(%rdi,%rdx,8), %rdi
	movq	%rdi, 80(%rdi)
	leaq	-3(%rdx), %rdi
	andq	%rsi, %rdi
	incq	%rdx
	cmpq	%rcx, %rdi
	jne	.LBB0_78
	.align	16, 0x90
.LBB0_79:                               # %while.cond.i.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	movq	80(%rcx), %rax
	cmpq	%rcx, %rax
	jne	.LBB0_79
# BB#80:                                # %while.cond2.preheader.i.i.i
	movq	entry_exit_blocks+192(%rip), %rax
	movabsq	$entry_exit_blocks, %rdx
	cmpq	%rcx, %rax
	je	.LBB0_83
# BB#81:                                # %while.body5.lr.ph.i.i.i
	movabsq	$entry_exit_blocks+192, %rsi
	.align	16, 0x90
.LBB0_82:                               # %while.body5.i.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, (%rsi)
	movq	80(%rax), %rdi
	movq	%rax, %rsi
	addq	$80, %rsi
	cmpq	%rcx, %rdi
	movq	%rdi, %rax
	jne	.LBB0_82
.LBB0_83:
	movq	%r9, %r15
	.align	16, 0x90
.LBB0_84:                               # %while.cond.i7.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rax
	movq	80(%rax), %rdx
	cmpq	%rax, %rdx
	jne	.LBB0_84
# BB#85:                                # %while.cond2.preheader.i10.i.i
	movq	entry_exit_blocks+80(%rip), %rdx
	cmpq	%rax, %rdx
	je	.LBB0_88
# BB#86:                                # %while.body5.lr.ph.i11.i.i
	movabsq	$entry_exit_blocks+80, %rsi
	.align	16, 0x90
.LBB0_87:                               # %while.body5.i15.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, (%rsi)
	movq	80(%rdx), %rdi
	movq	%rdx, %rsi
	addq	$80, %rsi
	cmpq	%rax, %rdi
	movq	%rdi, %rdx
	jne	.LBB0_87
.LBB0_88:                               # %find_group.exit16.i.i
	cmpq	%rax, %rcx
	je	.LBB0_89
# BB#90:                                # %union_groups.exit.i
	movq	%rax, 80(%rcx)
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_171
# BB#91:                                # %for.body8.lr.ph.i
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB0_92:                               # %for.body8.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_95 Depth 2
                                        #     Child Loop BB0_98 Depth 2
                                        #     Child Loop BB0_100 Depth 2
                                        #     Child Loop BB0_103 Depth 2
                                        #     Child Loop BB0_106 Depth 2
                                        #     Child Loop BB0_109 Depth 2
                                        #     Child Loop BB0_111 Depth 2
                                        #     Child Loop BB0_114 Depth 2
	movq	8(%r11), %rax
	movq	(%rax,%rdx,8), %r10
	movzbl	48(%r10), %eax
	testq	$22, %rax
	je	.LBB0_117
# BB#93:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB0_92 Depth=1
	movq	40(%r10), %r9
	movzbl	(%r9), %eax
	testq	$4, %rax
	jne	.LBB0_117
# BB#94:                                # %land.lhs.true13.i
                                        #   in Loop: Header=BB0_92 Depth=1
	movq	16(%r10), %rax
	movq	%rax, %rsi
	.align	16, 0x90
.LBB0_95:                               # %while.cond.i.i
                                        #   Parent Loop BB0_92 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rsi, %rdi
	movq	80(%rdi), %rsi
	cmpq	%rdi, %rsi
	jne	.LBB0_95
# BB#96:                                # %while.cond2.preheader.i.i
                                        #   in Loop: Header=BB0_92 Depth=1
	movq	80(%rax), %rbx
	cmpq	%rdi, %rbx
	je	.LBB0_99
# BB#97:                                # %while.body5.lr.ph.i.i
                                        #   in Loop: Header=BB0_92 Depth=1
	addq	$80, %rax
	.align	16, 0x90
.LBB0_98:                               # %while.body5.i.i
                                        #   Parent Loop BB0_92 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rdi, (%rax)
	movq	80(%rbx), %rsi
	movq	%rbx, %rax
	addq	$80, %rax
	cmpq	%rdi, %rsi
	movq	%rsi, %rbx
	jne	.LBB0_98
.LBB0_99:                               # %find_group.exit.i
                                        #   in Loop: Header=BB0_92 Depth=1
	movq	24(%r10), %rbx
	movq	%rbx, %rsi
	.align	16, 0x90
.LBB0_100:                              # %while.cond.i169.i
                                        #   Parent Loop BB0_92 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rsi, %rax
	movq	80(%rax), %rsi
	cmpq	%rax, %rsi
	jne	.LBB0_100
# BB#101:                               # %while.cond2.preheader.i172.i
                                        #   in Loop: Header=BB0_92 Depth=1
	movq	80(%rbx), %rsi
	cmpq	%rax, %rsi
	je	.LBB0_104
# BB#102:                               # %while.body5.lr.ph.i173.i
                                        #   in Loop: Header=BB0_92 Depth=1
	addq	$80, %rbx
	.align	16, 0x90
.LBB0_103:                              # %while.body5.i177.i
                                        #   Parent Loop BB0_92 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, (%rbx)
	movq	80(%rsi), %rcx
	movq	%rsi, %rbx
	addq	$80, %rbx
	cmpq	%rax, %rcx
	movq	%rcx, %rsi
	jne	.LBB0_103
.LBB0_104:                              # %find_group.exit178.i
                                        #   in Loop: Header=BB0_92 Depth=1
	cmpq	%rax, %rdi
	je	.LBB0_117
# BB#105:                               # %if.then.i
                                        #   in Loop: Header=BB0_92 Depth=1
	movzbl	(%r9), %eax
	orq	$2, %rax
	movb	%al, (%r9)
	movq	16(%r10), %rdi
	movq	24(%r10), %rsi
	movq	%rdi, %rax
	.align	16, 0x90
.LBB0_106:                              # %while.cond.i.i236.i
                                        #   Parent Loop BB0_92 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %rcx
	movq	80(%rcx), %rax
	cmpq	%rcx, %rax
	jne	.LBB0_106
# BB#107:                               # %while.cond2.preheader.i.i239.i
                                        #   in Loop: Header=BB0_92 Depth=1
	movq	80(%rdi), %rbx
	cmpq	%rcx, %rbx
	movq	%rsi, %rax
	je	.LBB0_111
# BB#108:                               # %while.body5.lr.ph.i.i240.i
                                        #   in Loop: Header=BB0_92 Depth=1
	addq	$80, %rdi
	.align	16, 0x90
.LBB0_109:                              # %while.body5.i.i244.i
                                        #   Parent Loop BB0_92 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, (%rdi)
	movq	80(%rbx), %rax
	movq	%rbx, %rdi
	addq	$80, %rdi
	cmpq	%rcx, %rax
	movq	%rax, %rbx
	jne	.LBB0_109
# BB#110:                               #   in Loop: Header=BB0_92 Depth=1
	movq	%rsi, %rax
	.align	16, 0x90
.LBB0_111:                              # %while.cond.i7.i248.i
                                        #   Parent Loop BB0_92 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %rbx
	movq	80(%rbx), %rax
	cmpq	%rbx, %rax
	jne	.LBB0_111
# BB#112:                               # %while.cond2.preheader.i10.i251.i
                                        #   in Loop: Header=BB0_92 Depth=1
	movq	80(%rsi), %rdi
	cmpq	%rbx, %rdi
	je	.LBB0_115
# BB#113:                               # %while.body5.lr.ph.i11.i252.i
                                        #   in Loop: Header=BB0_92 Depth=1
	addq	$80, %rsi
	.align	16, 0x90
.LBB0_114:                              # %while.body5.i15.i256.i
                                        #   Parent Loop BB0_92 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, (%rsi)
	movq	80(%rdi), %rax
	movq	%rdi, %rsi
	addq	$80, %rsi
	cmpq	%rbx, %rax
	movq	%rax, %rdi
	jne	.LBB0_114
.LBB0_115:                              # %find_group.exit16.i258.i
                                        #   in Loop: Header=BB0_92 Depth=1
	cmpq	%rbx, %rcx
	je	.LBB0_89
# BB#116:                               # %union_groups.exit260.i
                                        #   in Loop: Header=BB0_92 Depth=1
	movq	%rbx, 80(%rcx)
	.align	16, 0x90
.LBB0_117:                              # %for.inc21.i
                                        #   in Loop: Header=BB0_92 Depth=1
	incq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r8, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rax
	jl	.LBB0_92
# BB#118:
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB0_119:                              # %for.body26.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_122 Depth 2
                                        #     Child Loop BB0_125 Depth 2
                                        #     Child Loop BB0_127 Depth 2
                                        #     Child Loop BB0_130 Depth 2
                                        #     Child Loop BB0_133 Depth 2
                                        #     Child Loop BB0_136 Depth 2
                                        #     Child Loop BB0_138 Depth 2
                                        #     Child Loop BB0_141 Depth 2
	movq	8(%r11), %rax
	movq	(%rax,%rdx,8), %r10
	movq	16(%r10), %rsi
	movq	40(%rsi), %rax
	cmpq	$0, 8(%rax)
	je	.LBB0_144
# BB#120:                               # %land.lhs.true33.i
                                        #   in Loop: Header=BB0_119 Depth=1
	movq	24(%r10), %rdi
	movq	32(%rdi), %rax
	cmpq	$0, (%rax)
	je	.LBB0_144
# BB#121:                               # %land.lhs.true36.i
                                        #   in Loop: Header=BB0_119 Depth=1
	movq	40(%r10), %r9
	movzbl	(%r9), %eax
	testq	$4, %rax
	movq	%rsi, %rax
	jne	.LBB0_144
	.align	16, 0x90
.LBB0_122:                              # %while.cond.i264.i
                                        #   Parent Loop BB0_119 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %rbx
	movq	80(%rbx), %rax
	cmpq	%rbx, %rax
	jne	.LBB0_122
# BB#123:                               # %while.cond2.preheader.i267.i
                                        #   in Loop: Header=BB0_119 Depth=1
	movq	80(%rsi), %rcx
	cmpq	%rbx, %rcx
	movq	%rdi, %rax
	je	.LBB0_127
# BB#124:                               # %while.body5.lr.ph.i268.i
                                        #   in Loop: Header=BB0_119 Depth=1
	addq	$80, %rsi
	.align	16, 0x90
.LBB0_125:                              # %while.body5.i272.i
                                        #   Parent Loop BB0_119 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, (%rsi)
	movq	80(%rcx), %rax
	movq	%rcx, %rsi
	addq	$80, %rsi
	cmpq	%rbx, %rax
	movq	%rax, %rcx
	jne	.LBB0_125
# BB#126:                               #   in Loop: Header=BB0_119 Depth=1
	movq	%rdi, %rax
	.align	16, 0x90
.LBB0_127:                              # %while.cond.i277.i
                                        #   Parent Loop BB0_119 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %rcx
	movq	80(%rcx), %rax
	cmpq	%rcx, %rax
	jne	.LBB0_127
# BB#128:                               # %while.cond2.preheader.i280.i
                                        #   in Loop: Header=BB0_119 Depth=1
	movq	80(%rdi), %rsi
	cmpq	%rcx, %rsi
	je	.LBB0_131
# BB#129:                               # %while.body5.lr.ph.i281.i
                                        #   in Loop: Header=BB0_119 Depth=1
	addq	$80, %rdi
	.align	16, 0x90
.LBB0_130:                              # %while.body5.i285.i
                                        #   Parent Loop BB0_119 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, (%rdi)
	movq	80(%rsi), %rax
	movq	%rsi, %rdi
	addq	$80, %rdi
	cmpq	%rcx, %rax
	movq	%rax, %rsi
	jne	.LBB0_130
.LBB0_131:                              # %find_group.exit286.i
                                        #   in Loop: Header=BB0_119 Depth=1
	cmpq	%rcx, %rbx
	je	.LBB0_144
# BB#132:                               # %if.then49.i
                                        #   in Loop: Header=BB0_119 Depth=1
	movzbl	(%r9), %eax
	orq	$2, %rax
	movb	%al, (%r9)
	movq	16(%r10), %rdi
	movq	24(%r10), %rsi
	movq	%rdi, %rax
	.align	16, 0x90
.LBB0_133:                              # %while.cond.i.i208.i
                                        #   Parent Loop BB0_119 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %rcx
	movq	80(%rcx), %rax
	cmpq	%rcx, %rax
	jne	.LBB0_133
# BB#134:                               # %while.cond2.preheader.i.i211.i
                                        #   in Loop: Header=BB0_119 Depth=1
	movq	80(%rdi), %rbx
	cmpq	%rcx, %rbx
	movq	%rsi, %rax
	je	.LBB0_138
# BB#135:                               # %while.body5.lr.ph.i.i212.i
                                        #   in Loop: Header=BB0_119 Depth=1
	addq	$80, %rdi
	.align	16, 0x90
.LBB0_136:                              # %while.body5.i.i216.i
                                        #   Parent Loop BB0_119 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, (%rdi)
	movq	80(%rbx), %rax
	movq	%rbx, %rdi
	addq	$80, %rdi
	cmpq	%rcx, %rax
	movq	%rax, %rbx
	jne	.LBB0_136
# BB#137:                               #   in Loop: Header=BB0_119 Depth=1
	movq	%rsi, %rax
	.align	16, 0x90
.LBB0_138:                              # %while.cond.i7.i220.i
                                        #   Parent Loop BB0_119 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %rbx
	movq	80(%rbx), %rax
	cmpq	%rbx, %rax
	jne	.LBB0_138
# BB#139:                               # %while.cond2.preheader.i10.i223.i
                                        #   in Loop: Header=BB0_119 Depth=1
	movq	80(%rsi), %rdi
	cmpq	%rbx, %rdi
	je	.LBB0_142
# BB#140:                               # %while.body5.lr.ph.i11.i224.i
                                        #   in Loop: Header=BB0_119 Depth=1
	addq	$80, %rsi
	.align	16, 0x90
.LBB0_141:                              # %while.body5.i15.i228.i
                                        #   Parent Loop BB0_119 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, (%rsi)
	movq	80(%rdi), %rax
	movq	%rdi, %rsi
	addq	$80, %rsi
	cmpq	%rbx, %rax
	movq	%rax, %rdi
	jne	.LBB0_141
.LBB0_142:                              # %find_group.exit16.i230.i
                                        #   in Loop: Header=BB0_119 Depth=1
	cmpq	%rbx, %rcx
	je	.LBB0_89
# BB#143:                               # %union_groups.exit232.i
                                        #   in Loop: Header=BB0_119 Depth=1
	movq	%rbx, 80(%rcx)
	.align	16, 0x90
.LBB0_144:                              # %for.inc57.i
                                        #   in Loop: Header=BB0_119 Depth=1
	incq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r8, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rax
	jl	.LBB0_119
# BB#145:
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB0_146:                              # %for.body62.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_147 Depth 2
                                        #     Child Loop BB0_150 Depth 2
                                        #     Child Loop BB0_152 Depth 2
                                        #     Child Loop BB0_155 Depth 2
                                        #     Child Loop BB0_159 Depth 2
                                        #     Child Loop BB0_162 Depth 2
                                        #     Child Loop BB0_164 Depth 2
                                        #     Child Loop BB0_167 Depth 2
	movq	8(%r11), %rax
	movq	(%rax,%rdx,8), %r9
	movq	16(%r9), %rax
	movq	%rax, %rdi
	.align	16, 0x90
.LBB0_147:                              # %while.cond.i195.i
                                        #   Parent Loop BB0_146 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rdi, %rsi
	movq	80(%rsi), %rdi
	cmpq	%rsi, %rdi
	jne	.LBB0_147
# BB#148:                               # %while.cond2.preheader.i198.i
                                        #   in Loop: Header=BB0_146 Depth=1
	movq	80(%rax), %rdi
	cmpq	%rsi, %rdi
	je	.LBB0_151
# BB#149:                               # %while.body5.lr.ph.i199.i
                                        #   in Loop: Header=BB0_146 Depth=1
	addq	$80, %rax
	.align	16, 0x90
.LBB0_150:                              # %while.body5.i203.i
                                        #   Parent Loop BB0_146 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rsi, (%rax)
	movq	80(%rdi), %rcx
	movq	%rdi, %rax
	addq	$80, %rax
	cmpq	%rsi, %rcx
	movq	%rcx, %rdi
	jne	.LBB0_150
.LBB0_151:                              # %find_group.exit204.i
                                        #   in Loop: Header=BB0_146 Depth=1
	movq	24(%r9), %rdi
	movq	%rdi, %rax
	.align	16, 0x90
.LBB0_152:                              # %while.cond.i182.i
                                        #   Parent Loop BB0_146 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %rcx
	movq	80(%rcx), %rax
	cmpq	%rcx, %rax
	jne	.LBB0_152
# BB#153:                               # %while.cond2.preheader.i185.i
                                        #   in Loop: Header=BB0_146 Depth=1
	movq	80(%rdi), %rbx
	cmpq	%rcx, %rbx
	je	.LBB0_156
# BB#154:                               # %while.body5.lr.ph.i186.i
                                        #   in Loop: Header=BB0_146 Depth=1
	addq	$80, %rdi
	.align	16, 0x90
.LBB0_155:                              # %while.body5.i190.i
                                        #   Parent Loop BB0_146 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, (%rdi)
	movq	80(%rbx), %rax
	movq	%rbx, %rdi
	addq	$80, %rdi
	cmpq	%rcx, %rax
	movq	%rax, %rbx
	jne	.LBB0_155
.LBB0_156:                              # %find_group.exit191.i
                                        #   in Loop: Header=BB0_146 Depth=1
	cmpq	%rcx, %rsi
	je	.LBB0_170
# BB#157:                               # %land.lhs.true72.i
                                        #   in Loop: Header=BB0_146 Depth=1
	movq	40(%r9), %rax
	movzbl	(%rax), %esi
	testq	$4, %rsi
	jne	.LBB0_170
# BB#158:                               # %if.then79.i
                                        #   in Loop: Header=BB0_146 Depth=1
	orq	$2, %rsi
	movb	%sil, (%rax)
	movq	16(%r9), %rdi
	movq	24(%r9), %rsi
	movq	%rdi, %rax
	.align	16, 0x90
.LBB0_159:                              # %while.cond.i.i143.i
                                        #   Parent Loop BB0_146 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %rcx
	movq	80(%rcx), %rax
	cmpq	%rcx, %rax
	jne	.LBB0_159
# BB#160:                               # %while.cond2.preheader.i.i145.i
                                        #   in Loop: Header=BB0_146 Depth=1
	movq	80(%rdi), %rbx
	cmpq	%rcx, %rbx
	movq	%rsi, %rax
	je	.LBB0_164
# BB#161:                               # %while.body5.lr.ph.i.i146.i
                                        #   in Loop: Header=BB0_146 Depth=1
	addq	$80, %rdi
	.align	16, 0x90
.LBB0_162:                              # %while.body5.i.i150.i
                                        #   Parent Loop BB0_146 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, (%rdi)
	movq	80(%rbx), %rax
	movq	%rbx, %rdi
	addq	$80, %rdi
	cmpq	%rcx, %rax
	movq	%rax, %rbx
	jne	.LBB0_162
# BB#163:                               #   in Loop: Header=BB0_146 Depth=1
	movq	%rsi, %rax
	.align	16, 0x90
.LBB0_164:                              # %while.cond.i7.i154.i
                                        #   Parent Loop BB0_146 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %rbx
	movq	80(%rbx), %rax
	cmpq	%rbx, %rax
	jne	.LBB0_164
# BB#165:                               # %while.cond2.preheader.i10.i156.i
                                        #   in Loop: Header=BB0_146 Depth=1
	movq	80(%rsi), %rdi
	cmpq	%rbx, %rdi
	je	.LBB0_168
# BB#166:                               # %while.body5.lr.ph.i11.i157.i
                                        #   in Loop: Header=BB0_146 Depth=1
	addq	$80, %rsi
	.align	16, 0x90
.LBB0_167:                              # %while.body5.i15.i161.i
                                        #   Parent Loop BB0_146 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, (%rsi)
	movq	80(%rdi), %rax
	movq	%rdi, %rsi
	addq	$80, %rsi
	cmpq	%rbx, %rax
	movq	%rax, %rdi
	jne	.LBB0_167
.LBB0_168:                              # %find_group.exit16.i163.i
                                        #   in Loop: Header=BB0_146 Depth=1
	cmpq	%rbx, %rcx
	je	.LBB0_89
# BB#169:                               # %union_groups.exit165.i
                                        #   in Loop: Header=BB0_146 Depth=1
	movq	%rbx, 80(%rcx)
.LBB0_170:                              # %for.inc87.i
                                        #   in Loop: Header=BB0_146 Depth=1
	incq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r8, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rax
	jl	.LBB0_146
.LBB0_171:                              # %for.end89.i
	movslq	n_basic_blocks(%rip), %rdx
	movq	$0, entry_exit_blocks+192(%rip)
	movq	$0, entry_exit_blocks+80(%rip)
	testq	%rdx, %rdx
	jle	.LBB0_174
# BB#172:
	movq	$4, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rdx, %rsi
	andq	%rcx, %rsi
	.align	16, 0x90
.LBB0_173:                              # %for.body92.i
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rdi
	movq	(%rdi,%rax,8), %rdi
	movq	$0, 80(%rdi)
	leaq	-3(%rax), %rdi
	andq	%rcx, %rdi
	incq	%rax
	cmpq	%rsi, %rdi
	jne	.LBB0_173
.LBB0_174:                              # %for.cond190.preheader
	testq	%r14, %r14
	jle	.LBB0_175
# BB#176:                               # %for.body193.lr.ph
	movq	$1, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%r14, %rdx
	andq	%rcx, %rdx
	.align	16, 0x90
.LBB0_177:                              # %for.body193
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r11), %rsi
	movq	-8(%rsi,%rax,8), %rsi
	movzbl	48(%rsi), %edi
	testq	$16, %rdi
	je	.LBB0_180
# BB#178:                               # %land.lhs.true202
                                        #   in Loop: Header=BB0_177 Depth=1
	movq	40(%rsi), %rsi
	movzbl	(%rsi), %edi
	testq	$6, %rdi
	jne	.LBB0_180
# BB#179:                               # %if.then212
                                        #   in Loop: Header=BB0_177 Depth=1
	orq	$4, %rdi
	movb	%dil, (%rsi)
	incq	%r15
.LBB0_180:                              # %for.inc218
                                        #   in Loop: Header=BB0_177 Depth=1
	movq	%rax, %rsi
	andq	%rcx, %rsi
	incq	%rax
	cmpq	%rdx, %rsi
	jne	.LBB0_177
# BB#181:                               # %for.cond190.for.end220_crit_edge
	movq	%r15, %rbx
	movl	n_basic_blocks(%rip), %edx
	jmp	.LBB0_182
.LBB0_175:
	movq	%r15, %rbx
.LBB0_182:                              # %for.end220
	movq	%r11, 24(%rsp)          # 8-byte Spill
	movl	total_num_blocks(%rip), %eax
	movq	rtl_dump_file(%rip), %rdi
	leaq	2(%rdx,%rax), %rax
	movl	%eax, total_num_blocks(%rip)
	testq	%rdi, %rdi
	je	.LBB0_183
# BB#184:                               # %if.end225
	movabsq	$.L.str3, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movl	total_num_edges(%rip), %eax
	movq	rtl_dump_file(%rip), %rdi
	addq	%r14, %rax
	testq	%rdi, %rdi
	movl	%eax, total_num_edges(%rip)
	je	.LBB0_185
# BB#186:                               # %if.end230
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	fprintf
	movl	total_num_edges_ignored(%rip), %eax
	movq	rtl_dump_file(%rip), %rdi
	movq	%rbx, %rdx
	addq	%rdx, %rax
	testq	%rdi, %rdi
	movl	%eax, total_num_edges_ignored(%rip)
	je	.LBB0_188
# BB#187:                               # %if.then233
	movabsq	$.L.str5, %rsi
	xorq	%rax, %rax
	movq	%rdx, %rbx
	callq	fprintf
	movq	%rbx, %rdx
	jmp	.LBB0_188
.LBB0_183:                              # %if.end230.thread787
	movl	total_num_edges(%rip), %eax
	movl	total_num_edges_ignored(%rip), %ecx
	addq	%r14, %rax
	movl	%eax, total_num_edges(%rip)
	movq	%rbx, %rdx
	addq	%rdx, %rcx
	movl	%ecx, total_num_edges_ignored(%rip)
	jmp	.LBB0_188
.LBB0_185:                              # %if.end230.thread
	movl	total_num_edges_ignored(%rip), %eax
	movq	%rbx, %rdx
	addq	%rdx, %rax
	movl	%eax, total_num_edges_ignored(%rip)
.LBB0_188:                              # %if.end235
	movl	flag_test_coverage(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_220
# BB#189:                               # %if.then237
	movq	%rdx, %rbx
	movl	n_basic_blocks(%rip), %esi
	addq	$2, %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$63, %rcx
	movq	%rax, %rdx
	sarq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rdx, %rax
	movq	bbg_file(%rip), %r8
	xorq	%rdx, %rax
	movb	%al, 52(%rsp)
	movq	$63, %rcx
	movq	%rax, %rdi
	sarq	%cl, %rdi
	movq	$56, %rcx
	movq	%rdi, %rdx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	movq	$8, %rcx
	shrq	%cl, %rdx
	movb	%dl, 53(%rsp)
	movq	$48, %rcx
	movq	%rdi, %rdx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	movq	$16, %rcx
	shrq	%cl, %rdx
	movb	%dl, 54(%rsp)
	movq	$40, %rcx
	shrq	%cl, %rdi
	addq	%rax, %rdi
	movq	$24, %rcx
	shrq	%cl, %rdi
	cqto
	movabsq	$4294967296, %rcx       # imm = 0x100000000
	idivq	%rcx
	andq	$127, %rdi
	incq	%rax
	cmpq	$2, %rax
	jb	.LBB0_191
# BB#190:                               # %__store_gcov_type.exit.thread2.i487
	movb	%dil, 55(%rsp)
	jmp	.LBB0_192
.LBB0_191:                              # %if.else.i491
	movq	$24, %rcx
	shrq	%cl, %rsi
	andq	$128, %rsi
	orq	%rsi, %rdi
	movb	%dil, 55(%rsp)
	leaq	52(%rsp), %rdi
	movq	$1, %rsi
	movq	$4, %rdx
	movq	%r8, %rcx
	callq	fwrite
	movq	bbg_file(%rip), %r8
.LBB0_192:                              # %__write_long.exit492
	subq	%rbx, %r14
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$63, %rcx
	movq	%rax, %rdx
	sarq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rdx, %rax
	xorq	%rdx, %rax
	movb	%al, 52(%rsp)
	movq	$63, %rcx
	movq	%rax, %rsi
	sarq	%cl, %rsi
	movq	$56, %rcx
	movq	%rsi, %rdx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	movq	$8, %rcx
	shrq	%cl, %rdx
	movb	%dl, 53(%rsp)
	movq	$48, %rcx
	movq	%rsi, %rdx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	movq	$16, %rcx
	shrq	%cl, %rdx
	movb	%dl, 54(%rsp)
	movq	$40, %rcx
	shrq	%cl, %rsi
	addq	%rax, %rsi
	movq	$24, %rcx
	shrq	%cl, %rsi
	cqto
	movabsq	$4294967296, %rcx       # imm = 0x100000000
	idivq	%rcx
	andq	$127, %rsi
	incq	%rax
	cmpq	$2, %rax
	jb	.LBB0_194
# BB#193:                               # %__store_gcov_type.exit.thread2.i512
	movb	%sil, 55(%rsp)
	jmp	.LBB0_195
.LBB0_194:                              # %if.else.i516
	movq	$24, %rcx
	shrq	%cl, %r14
	andq	$128, %r14
	orq	%r14, %rsi
	movb	%sil, 55(%rsp)
	leaq	52(%rsp), %rdi
	movq	$1, %rsi
	movq	$4, %rdx
	movq	%r8, %rcx
	callq	fwrite
.LBB0_195:                              # %for.cond244.preheader
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	js	.LBB0_219
# BB#196:                               # %for.body248.lr.ph
	incq	%rax
	xorq	%r12, %r12
	leaq	52(%rsp), %r15
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB0_197:                              # %for.body248
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_202 Depth 2
                                        #     Child Loop BB0_207 Depth 2
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdi
	movabsq	$entry_exit_blocks, %rbx
	je	.LBB0_200
# BB#198:                               # %cond.false
                                        #   in Loop: Header=BB0_197 Depth=1
	movq	%rdi, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	andq	%rdx, %rax
	cmpq	%rax, %rcx
	movabsq	$entry_exit_blocks+112, %rbx
	je	.LBB0_200
# BB#199:                               # %cond.false256
                                        #   in Loop: Header=BB0_197 Depth=1
	movq	basic_block_info(%rip), %rax
	movq	24(%rax,%rdi,8), %rbx
.LBB0_200:                              # %cond.end262
                                        #   in Loop: Header=BB0_197 Depth=1
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	40(%rbx), %rax
	xorq	%rsi, %rsi
	jmp	.LBB0_202
	.align	16, 0x90
.LBB0_201:                              # %for.body269
                                        #   in Loop: Header=BB0_202 Depth=2
	movq	40(%rax), %rcx
	movzbl	(%rcx), %edx
	movq	8(%rax), %rax
	movq	$2, %rcx
	shrq	%cl, %rdx
	notq	%rdx
	andq	$1, %rdx
	addq	%rdx, %rsi
.LBB0_202:                              # %for.body269
                                        #   Parent Loop BB0_197 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rax, %rax
	jne	.LBB0_201
# BB#203:                               # %for.end281
                                        #   in Loop: Header=BB0_197 Depth=1
	movq	bbg_file(%rip), %r8
	movq	$63, %rcx
	movq	%rsi, %rdx
	sarq	%cl, %rdx
	leaq	(%rsi,%rdx), %rax
	xorq	%rdx, %rax
	movb	%al, 52(%rsp)
	movq	$63, %rcx
	movq	%rax, %rdi
	sarq	%cl, %rdi
	movq	$56, %rcx
	movq	%rdi, %rdx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	movq	$8, %rcx
	shrq	%cl, %rdx
	movb	%dl, 53(%rsp)
	movq	$48, %rcx
	movq	%rdi, %rdx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	movq	$16, %rcx
	shrq	%cl, %rdx
	movb	%dl, 54(%rsp)
	movq	$40, %rcx
	shrq	%cl, %rdi
	addq	%rax, %rdi
	movq	$24, %rcx
	shrq	%cl, %rdi
	cqto
	movabsq	$4294967296, %rcx       # imm = 0x100000000
	idivq	%rcx
	andq	$127, %rdi
	incq	%rax
	cmpq	$2, %rax
	jb	.LBB0_205
# BB#204:                               # %__store_gcov_type.exit.thread2.i537
                                        #   in Loop: Header=BB0_197 Depth=1
	movb	%dil, 55(%rsp)
	jmp	.LBB0_206
	.align	16, 0x90
.LBB0_205:                              # %if.else.i541
                                        #   in Loop: Header=BB0_197 Depth=1
	movq	$56, %rcx
	shrq	%cl, %rsi
	andq	$128, %rsi
	orq	%rsi, %rdi
	movb	%dil, 55(%rsp)
	movq	$1, %rsi
	movq	$4, %rdx
	movq	%r15, %rdi
	movq	%r8, %rcx
	callq	fwrite
.LBB0_206:                              # %for.cond284.preheader
                                        #   in Loop: Header=BB0_197 Depth=1
	movq	40(%rbx), %r13
	jmp	.LBB0_207
	.align	16, 0x90
.LBB0_217:                              # %for.inc337
                                        #   in Loop: Header=BB0_207 Depth=2
	movq	8(%r13), %r13
.LBB0_207:                              # %for.cond284.preheader
                                        #   Parent Loop BB0_197 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r13, %r13
	je	.LBB0_218
# BB#208:                               # %for.body286
                                        #   in Loop: Header=BB0_207 Depth=2
	movq	40(%r13), %rax
	movzbl	(%rax), %eax
	testq	$4, %rax
	jne	.LBB0_217
# BB#209:                               # %if.then294
                                        #   in Loop: Header=BB0_207 Depth=2
	movq	$1, %rcx
	shrq	%cl, %rax
	movl	48(%r13), %r14d
	movq	$3, %rcx
	movq	%r14, %rdx
	shrq	%cl, %rdx
	movq	24(%r13), %rdi
	movq	$2, %rcx
	shlq	%cl, %r14
	xorq	%rsi, %rsi
	movabsq	$entry_exit_blocks, %rcx
	cmpq	%rcx, %rdi
	je	.LBB0_213
# BB#210:                               # %cond.false318
                                        #   in Loop: Header=BB0_207 Depth=2
	movabsq	$entry_exit_blocks+112, %rcx
	cmpq	%rcx, %rdi
	movabsq	$n_basic_blocks, %rcx
	je	.LBB0_212
# BB#211:                               # %cond.false318
                                        #   in Loop: Header=BB0_207 Depth=2
	addq	$88, %rdi
	movq	%rdi, %rcx
.LBB0_212:                              # %cond.false318
                                        #   in Loop: Header=BB0_207 Depth=2
	movl	(%rcx), %esi
	incq	%rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
.LBB0_213:                              # %cond.end330
                                        #   in Loop: Header=BB0_207 Depth=2
	andq	$1, %rax
	andq	$2, %rdx
	orq	%rax, %rdx
	andq	$4, %r14
	orq	%rdx, %r14
	movq	bbg_file(%rip), %r8
	movq	$63, %rcx
	movq	%rsi, %rdx
	sarq	%cl, %rdx
	leaq	(%rsi,%rdx), %rax
	xorq	%rdx, %rax
	movb	%al, 52(%rsp)
	movq	$63, %rcx
	movq	%rax, %rdi
	sarq	%cl, %rdi
	movq	$56, %rcx
	movq	%rdi, %rdx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	movq	$8, %rcx
	shrq	%cl, %rdx
	movb	%dl, 53(%rsp)
	movq	$48, %rcx
	movq	%rdi, %rdx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	movq	$16, %rcx
	shrq	%cl, %rdx
	movb	%dl, 54(%rsp)
	movq	$40, %rcx
	shrq	%cl, %rdi
	addq	%rax, %rdi
	movq	$24, %rcx
	shrq	%cl, %rdi
	cqto
	movabsq	$4294967296, %rcx       # imm = 0x100000000
	idivq	%rcx
	andq	$127, %rdi
	incq	%rax
	cmpq	$2, %rax
	jb	.LBB0_215
# BB#214:                               # %__store_gcov_type.exit.thread2.i562
                                        #   in Loop: Header=BB0_207 Depth=2
	movb	%dil, 55(%rsp)
	movq	%r15, %rdi
	jmp	.LBB0_216
.LBB0_215:                              # %if.else.i566
                                        #   in Loop: Header=BB0_207 Depth=2
	movq	$56, %rcx
	shrq	%cl, %rsi
	andq	$128, %rsi
	orq	%rsi, %rdi
	movb	%dil, 55(%rsp)
	movq	$1, %rsi
	movq	$4, %rdx
	movq	%r15, %rbx
	movq	%rbx, %rdi
	movq	%r8, %rcx
	callq	fwrite
	movq	%rbx, %rdi
	movq	bbg_file(%rip), %r8
.LBB0_216:                              # %__write_long.exit567
                                        #   in Loop: Header=BB0_207 Depth=2
	movb	%r14b, 52(%rsp)
	movb	%r12b, 53(%rsp)
	movb	%r12b, 54(%rsp)
	movb	%r12b, 55(%rsp)
	movq	$1, %rsi
	movq	$4, %rdx
	movq	%rdi, %r15
	movq	%r8, %rcx
	callq	fwrite
	jmp	.LBB0_217
	.align	16, 0x90
.LBB0_218:                              # %for.inc340
                                        #   in Loop: Header=BB0_197 Depth=1
	movl	n_basic_blocks(%rip), %eax
	movq	40(%rsp), %rdi          # 8-byte Reload
	incq	%rdi
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB0_197
.LBB0_219:                              # %for.end342
	movq	bbg_file(%rip), %rcx
	movq	$1, %r14
	movb	%r14b, 52(%rsp)
	xorq	%rbx, %rbx
	movb	%bl, 53(%rsp)
	movb	%bl, 54(%rsp)
	movb	%bl, 55(%rsp)
	leaq	52(%rsp), %r15
	movq	$1, %rsi
	movq	$4, %rdx
	movq	%r15, %rdi
	callq	fwrite
	movq	bbg_file(%rip), %rcx
	movl	%ebx, 52(%rsp)
	movq	$1, %rsi
	movq	$4, %rdx
	movq	%r15, %rdi
	callq	fwrite
	movq	bbg_file(%rip), %rcx
	movb	%r14b, 52(%rsp)
	movb	%bl, 53(%rsp)
	movb	%bl, 54(%rsp)
	movb	%bl, 55(%rsp)
	movq	$1, %rsi
	movq	$4, %rdx
	movq	%r15, %rdi
	callq	fwrite
	movq	bbg_file(%rip), %rcx
	movb	%r14b, 52(%rsp)
	movb	%bl, 53(%rsp)
	movb	%bl, 54(%rsp)
	movq	$128, %rax
	movb	%al, 55(%rsp)
	movq	$1, %rsi
	movq	$4, %rdx
	movq	%r15, %rdi
	callq	fwrite
.LBB0_220:                              # %if.end347
	movl	flag_branch_probabilities(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_353
# BB#221:                               # %if.then349
	movq	$24, %rdi
	callq	alloc_aux_for_blocks
	movl	n_basic_blocks(%rip), %r10d
	leaq	2(%r10), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB0_382
# BB#222:                               # %for.body.i617.preheader
	leaq	1(%r10), %r11
	xorq	%rsi, %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movabsq	$entry_exit_blocks, %r9
	movabsq	$entry_exit_blocks+112, %r8
	.align	16, 0x90
.LBB0_223:                              # %for.body.i617
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_227 Depth 2
                                        #     Child Loop BB0_232 Depth 2
	testq	%rdi, %rsi
	movq	%r9, %rcx
	je	.LBB0_226
# BB#224:                               # %cond.false.i
                                        #   in Loop: Header=BB0_223 Depth=1
	movq	%rsi, %rcx
	andq	%rdi, %rcx
	movq	%r11, %rdx
	andq	%rdi, %rdx
	cmpq	%rdx, %rcx
	movq	%r8, %rcx
	je	.LBB0_226
# BB#225:                               # %cond.false5.i
                                        #   in Loop: Header=BB0_223 Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	24(%rcx,%rsi,8), %rcx
.LBB0_226:                              # %cond.end7.i
                                        #   in Loop: Header=BB0_223 Depth=1
	movq	40(%rcx), %rbx
	jmp	.LBB0_227
	.align	16, 0x90
.LBB0_230:                              # %for.inc.i
                                        #   in Loop: Header=BB0_227 Depth=2
	movq	8(%rbx), %rbx
.LBB0_227:                              # %cond.end7.i
                                        #   Parent Loop BB0_223 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB0_231
# BB#228:                               # %for.body10.i
                                        #   in Loop: Header=BB0_227 Depth=2
	movq	40(%rbx), %rdx
	movzbl	(%rdx), %edx
	testq	$4, %rdx
	jne	.LBB0_230
# BB#229:                               # %if.then.i624
                                        #   in Loop: Header=BB0_227 Depth=2
	movq	80(%rcx), %rdx
	incq	8(%rdx)
	jmp	.LBB0_230
	.align	16, 0x90
.LBB0_231:                              # %for.end.i
                                        #   in Loop: Header=BB0_223 Depth=1
	movq	%rdi, %rax
	movq	32(%rcx), %rbx
	jmp	.LBB0_232
	.align	16, 0x90
.LBB0_235:                              # %for.inc26.i
                                        #   in Loop: Header=BB0_232 Depth=2
	movq	(%rbx), %rbx
.LBB0_232:                              # %for.end.i
                                        #   Parent Loop BB0_223 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB0_236
# BB#233:                               # %for.body15.i
                                        #   in Loop: Header=BB0_232 Depth=2
	movq	40(%rbx), %rdx
	movzbl	(%rdx), %edx
	testq	$4, %rdx
	jne	.LBB0_235
# BB#234:                               # %if.then22.i
                                        #   in Loop: Header=BB0_232 Depth=2
	movq	80(%rcx), %rdx
	incq	16(%rdx)
	jmp	.LBB0_235
	.align	16, 0x90
.LBB0_236:                              # %for.inc28.i
                                        #   in Loop: Header=BB0_223 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rsi
	movq	$32, %rcx
	movq	%r11, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rdx
	movq	%rax, %rdi
	jl	.LBB0_223
# BB#237:                               # %for.body36.lr.ph.i
	movq	entry_exit_blocks+192(%rip), %rax
	movq	$2, 8(%rax)
	movq	entry_exit_blocks+80(%rip), %rax
	movq	$2, 16(%rax)
	xorq	%r12, %r12
	movabsq	$entry_exit_blocks, %rsi
	movabsq	$0, %r14
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB0_238:                              # %for.body36.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_242 Depth 2
	testq	%rdi, %r12
	movq	%rsi, %r15
	je	.LBB0_241
# BB#239:                               # %cond.false40.i
                                        #   in Loop: Header=BB0_238 Depth=1
	movq	%r12, %rax
	andq	%rdi, %rax
	leaq	1(%r10), %rcx
	andq	%rdi, %rcx
	cmpq	%rcx, %rax
	movabsq	$entry_exit_blocks+112, %r15
	je	.LBB0_241
# BB#240:                               # %cond.false44.i
                                        #   in Loop: Header=BB0_238 Depth=1
	movq	basic_block_info(%rip), %rax
	movq	24(%rax,%r12,8), %r15
.LBB0_241:                              # %cond.end52.i
                                        #   in Loop: Header=BB0_238 Depth=1
	movq	40(%r15), %r13
	testq	%r13, %r13
	je	.LBB0_257
	.align	16, 0x90
.LBB0_242:                              # %for.body58.i
                                        #   Parent Loop BB0_238 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	40(%r13), %rax
	movzbl	(%rax), %ecx
	testq	$6, %rcx
	jne	.LBB0_255
# BB#243:                               # %if.then71.i
                                        #   in Loop: Header=BB0_242 Depth=2
	movq	da_file(%rip), %rcx
	incq	%rbx
	testq	%rcx, %rcx
	jne	.LBB0_244
# BB#252:                               # %if.else.i631
                                        #   in Loop: Header=BB0_242 Depth=2
	movq	$0, 56(%r13)
	jmp	.LBB0_253
	.align	16, 0x90
.LBB0_244:                              # %if.then74.i
                                        #   in Loop: Header=BB0_242 Depth=2
	movq	$1, %rsi
	movq	$8, %rdx
	leaq	52(%rsp), %rdi
	callq	fread
	xorq	%rdx, %rdx
	cmpq	$8, %rax
	jne	.LBB0_251
# BB#245:                               # %if.else.i.i
                                        #   in Loop: Header=BB0_242 Depth=2
	movsbq	59(%rsp), %rax
	movq	%rax, %rcx
	andq	$127, %rcx
	shlq	$8, %rcx
	movzbl	58(%rsp), %edx
	orq	%rcx, %rdx
	movq	$8, %rcx
	shlq	%cl, %rdx
	movzbl	57(%rsp), %esi
	orq	%rdx, %rsi
	movq	$8, %rcx
	shlq	%cl, %rsi
	movzbl	56(%rsp), %edx
	orq	%rsi, %rdx
	movq	$8, %rcx
	shlq	%cl, %rdx
	movzbl	55(%rsp), %esi
	orq	%rdx, %rsi
	movq	$8, %rcx
	shlq	%cl, %rsi
	movzbl	54(%rsp), %edi
	orq	%rsi, %rdi
	movq	$8, %rcx
	movzbl	53(%rsp), %esi
	shlq	%cl, %rdi
	movzbl	52(%rsp), %edx
	orq	%rdi, %rsi
	movq	$8, %rcx
	shlq	%cl, %rsi
	orq	%rsi, %rdx
	testq	%rax, %rax
	movq	%r14, %rax
	jns	.LBB0_247
# BB#246:                               # %if.else.i.i
                                        #   in Loop: Header=BB0_242 Depth=2
	movabsq	$1, %rax
.LBB0_247:                              # %if.else.i.i
                                        #   in Loop: Header=BB0_242 Depth=2
	testq	%rdx, %rdx
	movq	%r14, %rcx
	jle	.LBB0_249
# BB#248:                               # %if.else.i.i
                                        #   in Loop: Header=BB0_242 Depth=2
	movabsq	$1, %rcx
.LBB0_249:                              # %if.else.i.i
                                        #   in Loop: Header=BB0_242 Depth=2
	testq	%rcx, %rax
	je	.LBB0_251
# BB#250:                               #   in Loop: Header=BB0_242 Depth=2
	negq	%rdx
.LBB0_251:                              # %__read_gcov_type.exit.i
                                        #   in Loop: Header=BB0_242 Depth=2
	movq	%rdx, 56(%r13)
	movq	40(%r13), %rax
.LBB0_253:                              # %if.end76.i
                                        #   in Loop: Header=BB0_242 Depth=2
	movzbl	(%rax), %ecx
	orq	$1, %rcx
	movb	%cl, (%rax)
	movq	80(%r15), %rax
	decq	8(%rax)
	movq	24(%r13), %rax
	movq	80(%rax), %rcx
	decq	16(%rcx)
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_255
# BB#254:                               # %if.then86.i
                                        #   in Loop: Header=BB0_242 Depth=2
	movl	88(%rax), %ecx
	movl	88(%r15), %edx
	xorq	%rax, %rax
	movabsq	$.L.str34, %rsi
	callq	fprintf
	movq	rtl_dump_file(%rip), %rdi
	movq	56(%r13), %rdx
	xorq	%rax, %rax
	movabsq	$.L.str35, %rsi
	callq	fprintf
.LBB0_255:                              # %for.inc94.i
                                        #   in Loop: Header=BB0_242 Depth=2
	movq	8(%r13), %r13
	testq	%r13, %r13
	jne	.LBB0_242
# BB#256:                               # %for.cond56.for.inc97_crit_edge.i
                                        #   in Loop: Header=BB0_238 Depth=1
	movl	n_basic_blocks(%rip), %r10d
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movabsq	$entry_exit_blocks, %rsi
.LBB0_257:                              # %for.inc97.i
                                        #   in Loop: Header=BB0_238 Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r12
	leaq	1(%r10), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB0_238
	jmp	.LBB0_258
.LBB0_382:                              # %for.end30.thread.i
	movq	entry_exit_blocks+192(%rip), %rax
	movq	$2, 8(%rax)
	movq	entry_exit_blocks+80(%rip), %rax
	movq	$2, 16(%rax)
	xorq	%rbx, %rbx
.LBB0_258:                              # %for.end99.i
	movq	rtl_dump_file(%rip), %rdi
	xorq	%r14, %r14
	testq	%rdi, %rdi
	je	.LBB0_261
# BB#259:                               # %if.then101.i
	movabsq	$.L.str36, %rsi
	xorq	%rax, %rax
	xorq	%r14, %r14
	movq	%rbx, %rdx
	callq	fprintf
	.align	16, 0x90
.LBB0_261:                              # %while.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_263 Depth 2
                                        #       Child Loop BB0_274 Depth 3
                                        #       Child Loop BB0_270 Depth 3
                                        #       Child Loop BB0_279 Depth 3
                                        #       Child Loop BB0_281 Depth 3
                                        #       Child Loop BB0_285 Depth 3
                                        #       Child Loop BB0_287 Depth 3
	movslq	n_basic_blocks(%rip), %r15
	incq	%r14
	cmpq	$-1, %r15
	jl	.LBB0_290
# BB#262:                               # %for.body109.lr.ph.i
                                        #   in Loop: Header=BB0_261 Depth=1
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	movabsq	$entry_exit_blocks, %r9
	movabsq	$entry_exit_blocks+112, %r8
	.align	16, 0x90
.LBB0_263:                              # %for.body109.i
                                        #   Parent Loop BB0_261 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_274 Depth 3
                                        #       Child Loop BB0_270 Depth 3
                                        #       Child Loop BB0_279 Depth 3
                                        #       Child Loop BB0_281 Depth 3
                                        #       Child Loop BB0_285 Depth 3
                                        #       Child Loop BB0_287 Depth 3
	testq	%r10, %rdx
	movq	%r9, %r11
	je	.LBB0_266
# BB#264:                               # %cond.false113.i
                                        #   in Loop: Header=BB0_263 Depth=2
	movl	n_basic_blocks(%rip), %eax
	movq	%rdx, %rcx
	andq	%r10, %rcx
	incq	%rax
	andq	%r10, %rax
	cmpq	%rax, %rcx
	movq	%r8, %r11
	je	.LBB0_266
# BB#265:                               # %cond.false117.i
                                        #   in Loop: Header=BB0_263 Depth=2
	leaq	-1(%r15), %rax
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rsi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rsi,%rax,8), %r11
.LBB0_266:                              # %cond.end125.i
                                        #   in Loop: Header=BB0_263 Depth=2
	movq	80(%r11), %r12
	movzbl	(%r12), %esi
	testq	$1, %rsi
	jne	.LBB0_276
# BB#267:                               # %if.then132.i
                                        #   in Loop: Header=BB0_263 Depth=2
	cmpq	$0, 8(%r12)
	jne	.LBB0_271
# BB#268:                               # %if.then135.i
                                        #   in Loop: Header=BB0_263 Depth=2
	movq	40(%r11), %rbx
	xorq	%rsi, %rsi
	jmp	.LBB0_270
	.align	16, 0x90
.LBB0_269:                              # %for.body140.i
                                        #   in Loop: Header=BB0_270 Depth=3
	movq	56(%rbx), %rax
	movq	8(%rbx), %rbx
	addq	%rax, %rsi
.LBB0_270:                              # %for.body140.i
                                        #   Parent Loop BB0_261 Depth=1
                                        #     Parent Loop BB0_263 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	jne	.LBB0_269
	jmp	.LBB0_275
	.align	16, 0x90
.LBB0_271:                              # %if.else150.i
                                        #   in Loop: Header=BB0_263 Depth=2
	cmpq	$0, 16(%r12)
	jne	.LBB0_276
# BB#272:                               # %if.then153.i
                                        #   in Loop: Header=BB0_263 Depth=2
	movq	32(%r11), %rbx
	xorq	%rsi, %rsi
	jmp	.LBB0_274
	.align	16, 0x90
.LBB0_273:                              # %for.body159.i
                                        #   in Loop: Header=BB0_274 Depth=3
	movq	56(%rbx), %rax
	movq	(%rbx), %rbx
	addq	%rax, %rsi
.LBB0_274:                              # %for.body159.i
                                        #   Parent Loop BB0_261 Depth=1
                                        #     Parent Loop BB0_263 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	jne	.LBB0_273
.LBB0_275:                              # %for.end164.i
                                        #   in Loop: Header=BB0_263 Depth=2
	movq	%rsi, 96(%r11)
	movzbl	(%r12), %esi
	orq	$1, %rsi
	movb	%sil, (%r12)
	movq	$1, %rbx
.LBB0_276:                              # %if.end171.i
                                        #   in Loop: Header=BB0_263 Depth=2
	testq	$1, %rsi
	je	.LBB0_289
# BB#277:                               # %if.then176.i
                                        #   in Loop: Header=BB0_263 Depth=2
	movq	8(%r12), %rax
	cmpq	$1, %rax
	jne	.LBB0_283
# BB#278:                               # %if.then179.i
                                        #   in Loop: Header=BB0_263 Depth=2
	movq	40(%r11), %rbx
	xorq	%rsi, %rsi
	testq	%rbx, %rbx
	movq	%rbx, %rax
	je	.LBB0_383
	.align	16, 0x90
.LBB0_279:                              # %for.body185.i
                                        #   Parent Loop BB0_261 Depth=1
                                        #     Parent Loop BB0_263 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rsi, %rdi
	movq	56(%rax), %rsi
	movq	8(%rax), %rax
	addq	%rdi, %rsi
	testq	%rax, %rax
	jne	.LBB0_279
	.align	16, 0x90
.LBB0_281:                              # %for.body194.i
                                        #   Parent Loop BB0_261 Depth=1
                                        #     Parent Loop BB0_263 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	40(%rbx), %rax
	movzbl	(%rax), %edi
	testq	$5, %rdi
	je	.LBB0_282
# BB#280:                               # %for.cond192.i
                                        #   in Loop: Header=BB0_281 Depth=3
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_281
	jmp	.LBB0_383
	.align	16, 0x90
.LBB0_282:                              # %if.end216.i
                                        #   in Loop: Header=BB0_263 Depth=2
	movq	96(%r11), %rcx
	subq	%rsi, %rcx
	orq	$1, %rdi
	movb	%dil, (%rax)
	movq	%rcx, 56(%rbx)
	decq	8(%r12)
	movq	24(%rbx), %rax
	movq	80(%rax), %rax
	decq	16(%rax)
	movq	$1, %rbx
.LBB0_283:                              # %if.end228.i
                                        #   in Loop: Header=BB0_263 Depth=2
	movq	16(%r12), %rax
	cmpq	$1, %rax
	jne	.LBB0_289
# BB#284:                               # %if.then231.i
                                        #   in Loop: Header=BB0_263 Depth=2
	movq	32(%r11), %rbx
	xorq	%rsi, %rsi
	testq	%rbx, %rbx
	movq	%rbx, %rax
	je	.LBB0_384
	.align	16, 0x90
.LBB0_285:                              # %for.body237.i
                                        #   Parent Loop BB0_261 Depth=1
                                        #     Parent Loop BB0_263 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rsi, %rcx
	movq	56(%rax), %rsi
	movq	(%rax), %rax
	addq	%rcx, %rsi
	testq	%rax, %rax
	jne	.LBB0_285
	.align	16, 0x90
.LBB0_287:                              # %for.body246.i
                                        #   Parent Loop BB0_261 Depth=1
                                        #     Parent Loop BB0_263 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	40(%rbx), %rdi
	movzbl	(%rdi), %eax
	testq	$5, %rax
	je	.LBB0_288
# BB#286:                               # %for.cond244.i
                                        #   in Loop: Header=BB0_287 Depth=3
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_287
	jmp	.LBB0_384
	.align	16, 0x90
.LBB0_288:                              # %if.end270.i
                                        #   in Loop: Header=BB0_263 Depth=2
	movq	96(%r11), %rcx
	movq	56(%rbx), %r11
	subq	%rsi, %rcx
	addq	%r11, %rcx
	orq	$1, %rax
	movb	%al, (%rdi)
	movq	%rcx, 56(%rbx)
	decq	16(%r12)
	movq	16(%rbx), %rax
	movq	80(%rax), %rax
	decq	8(%rax)
	movq	$1, %rbx
.LBB0_289:                              # %for.inc283.i
                                        #   in Loop: Header=BB0_263 Depth=2
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%r15
	decq	%rdx
	testq	%rax, %rax
	jg	.LBB0_263
# BB#260:                               # %while.cond.loopexit.i
                                        #   in Loop: Header=BB0_261 Depth=1
	testq	%r10, %rbx
	jne	.LBB0_261
.LBB0_290:                              # %while.end.i
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_291
# BB#292:                               # %if.end288.i
	callq	dump_flow_info
	movl	total_num_passes(%rip), %eax
	movq	rtl_dump_file(%rip), %rdi
	addq	%r14, %rax
	testq	%rdi, %rdi
	movl	%eax, total_num_passes(%rip)
	je	.LBB0_294
# BB#293:                               # %if.then291.i
	movabsq	$.L.str37, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	fprintf
	jmp	.LBB0_294
.LBB0_291:                              # %if.end288.thread.i
	movl	total_num_passes(%rip), %eax
	addq	%r14, %rax
	movl	%eax, total_num_passes(%rip)
.LBB0_294:                              # %for.cond294.preheader.i
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_299
# BB#295:                               # %for.body296.lr.ph.i
	movq	basic_block_info(%rip), %rdx
	movq	$4, %rsi
	.align	16, 0x90
.LBB0_296:                              # %for.body296.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdx,%rsi,8), %rcx
	movq	80(%rcx), %rcx
	cmpq	$0, 8(%rcx)
	jne	.LBB0_301
# BB#297:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB0_296 Depth=1
	cmpq	$0, 16(%rcx)
	jne	.LBB0_301
# BB#298:                               # %for.cond294.i
                                        #   in Loop: Header=BB0_296 Depth=1
	leaq	-3(%rsi), %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rsi
	cmpq	%rax, %rdi
	jl	.LBB0_296
.LBB0_299:                              # %for.cond313.preheader.i
	movq	$0, 136(%rsp)
	movq	$0, 128(%rsp)
	movq	$0, 120(%rsp)
	movq	$0, 112(%rsp)
	movq	$0, 104(%rsp)
	movq	$0, 96(%rsp)
	movq	$0, 88(%rsp)
	movq	$0, 80(%rsp)
	movq	$0, 72(%rsp)
	movq	$0, 64(%rsp)
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%r14, %r14
	testq	%rdx, %rdx
	js	.LBB0_300
# BB#302:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movabsq	$0, %rdi
	movabsq	$1, %r15
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB0_303:                              # %for.body324.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_309 Depth 2
                                        #     Child Loop BB0_318 Depth 2
                                        #     Child Loop BB0_326 Depth 2
                                        #     Child Loop BB0_330 Depth 2
                                        #     Child Loop BB0_334 Depth 2
                                        #     Child Loop BB0_336 Depth 2
	testq	%r12, %r14
	movabsq	$entry_exit_blocks, %rcx
	je	.LBB0_306
# BB#304:                               # %cond.false328.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movq	%r14, %rcx
	andq	%r12, %rcx
	andq	%r12, %rax
	cmpq	%rax, %rcx
	movabsq	$entry_exit_blocks+112, %rax
	movq	%rax, %rcx
	je	.LBB0_306
# BB#305:                               # %cond.false332.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movq	basic_block_info(%rip), %rax
	movq	24(%rax,%r14,8), %rcx
.LBB0_306:                              # %cond.end340.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	%r14, 40(%rsp)          # 8-byte Spill
	movq	96(%rcx), %r14
	movq	40(%rcx), %r13
	testq	%r14, %r14
	jne	.LBB0_307
# BB#325:                               # %for.cond426.preheader.i
                                        #   in Loop: Header=BB0_303 Depth=1
	xorq	%rcx, %rcx
	testq	%r13, %r13
	movq	%r13, %rax
	movabsq	$0, %rbx
	je	.LBB0_337
	.align	16, 0x90
.LBB0_326:                              # %for.body428.i
                                        #   Parent Loop BB0_303 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	48(%rax), %edx
	testq	$30, %rdx
	movq	%rdi, %rdx
	jne	.LBB0_328
# BB#327:                               # %for.body428.i
                                        #   in Loop: Header=BB0_326 Depth=2
	movq	%r15, %rdx
.LBB0_328:                              # %for.body428.i
                                        #   in Loop: Header=BB0_326 Depth=2
	movq	8(%rax), %rax
	addq	%rdx, %rcx
	testq	%rax, %rax
	jne	.LBB0_326
# BB#329:                               # %for.end437.i
                                        #   in Loop: Header=BB0_303 Depth=1
	xorq	%rsi, %rsi
	testq	%rcx, %rcx
	movq	%r13, %rax
	jne	.LBB0_330
	.align	16, 0x90
.LBB0_334:                              # %for.body461.i
                                        #   Parent Loop BB0_303 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rax), %rax
	incq	%rsi
	testq	%rax, %rax
	jne	.LBB0_334
# BB#335:                               # %for.body469.lr.ph.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movq	$10000, %rax            # imm = 0x2710
	xorq	%rdx, %rdx
	idivq	%rsi
	.align	16, 0x90
.LBB0_336:                              # %for.body469.i
                                        #   Parent Loop BB0_303 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	%eax, 52(%r13)
	movq	8(%r13), %r13
	testq	%r13, %r13
	jne	.LBB0_336
	jmp	.LBB0_337
	.align	16, 0x90
.LBB0_330:                              # %for.body443.i
                                        #   Parent Loop BB0_303 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	48(%r13), %eax
	testq	$30, %rax
	jne	.LBB0_332
# BB#331:                               # %if.then447.i
                                        #   in Loop: Header=BB0_330 Depth=2
	movq	$10000, %rax            # imm = 0x2710
	xorq	%rdx, %rdx
	idivq	%rcx
	movl	%eax, 52(%r13)
	jmp	.LBB0_333
	.align	16, 0x90
.LBB0_332:                              # %if.else451.i
                                        #   in Loop: Header=BB0_330 Depth=2
	movl	%ebx, 52(%r13)
.LBB0_333:                              # %for.inc454.i
                                        #   in Loop: Header=BB0_330 Depth=2
	movq	8(%r13), %r13
	testq	%r13, %r13
	jne	.LBB0_330
.LBB0_337:                              # %if.end476.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movq	32(%rsp), %rbx          # 8-byte Reload
	movslq	88(%rbx), %rax
	testq	%rax, %rax
	js	.LBB0_338
# BB#339:                               # %land.lhs.true480.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movq	%rdi, %r13
	movq	8(%rbx), %rdi
	callq	any_condjump_p
	testq	%r12, %rax
	movq	40(%rsp), %r14          # 8-byte Reload
	jne	.LBB0_341
# BB#340:                               #   in Loop: Header=BB0_303 Depth=1
	movq	%r13, %rdi
	jmp	.LBB0_344
	.align	16, 0x90
.LBB0_307:                              # %for.cond348.preheader.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movq	%rdi, %rbx
	testq	%r13, %r13
	je	.LBB0_312
# BB#308:                               # %for.body350.lr.ph.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movq	$63, %rcx
	movq	%r14, %r15
	shrq	%cl, %r15
	addq	%r14, %r15
	movq	$1, %rcx
	sarq	%cl, %r15
	.align	16, 0x90
.LBB0_309:                              # %for.body350.i
                                        #   Parent Loop BB0_303 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	56(%r13), %rax
	imulq	$10000, %rax, %rax      # imm = 0x2710
	addq	%r15, %rax
	cqto
	idivq	%r14
	movq	%rax, %rcx
                                        # kill: RAX<def> RCX<kill>
	andq	%r12, %rax
	cmpq	$10001, %rax            # imm = 0x2711
	movl	%ecx, 52(%r13)
	jb	.LBB0_311
# BB#310:                               # %if.then361.i
                                        #   in Loop: Header=BB0_309 Depth=2
	movq	16(%r13), %rax
	movq	24(%r13), %rdx
	movl	88(%rdx), %edx
	movl	88(%rax), %esi
	xorq	%rax, %rax
	movabsq	$.L.str38, %rdi
	callq	error
	movq	$5000, %rax             # imm = 0x1388
	movl	%eax, 52(%r13)
.LBB0_311:                              # %for.inc369.i
                                        #   in Loop: Header=BB0_309 Depth=2
	movq	8(%r13), %r13
	testq	%r13, %r13
	jne	.LBB0_309
.LBB0_312:                              # %for.end371.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movq	32(%rsp), %r13          # 8-byte Reload
	movslq	88(%r13), %rax
	testq	%rax, %rax
	js	.LBB0_313
# BB#314:                               # %land.lhs.true375.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movq	8(%r13), %rdi
	callq	any_condjump_p
	testq	%r12, %rax
	movq	%rbx, %rdi
	movq	40(%rsp), %r14          # 8-byte Reload
	movabsq	$1, %r15
	je	.LBB0_344
# BB#315:                               # %land.lhs.true378.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movq	40(%r13), %rax
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB0_344
# BB#316:                               # %for.cond386.preheader.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movzbl	48(%rax), %edx
	testq	$17, %rdx
	je	.LBB0_319
# BB#317:                               # %for.inc389.i.preheader
                                        #   in Loop: Header=BB0_303 Depth=1
	movzbl	48(%rcx), %eax
	testq	$17, %rax
	movq	%rcx, %rax
	je	.LBB0_319
	.align	16, 0x90
.LBB0_318:                              # %for.inc389.for.inc389_crit_edge.i
                                        #   Parent Loop BB0_303 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rax), %rax
	movzbl	48(%rax), %ecx
	testq	$17, %rcx
	jne	.LBB0_318
.LBB0_319:                              # %for.end391.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movslq	52(%rax), %rsi
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	$2, %rcx
	shlq	%cl, %rsi
	leaq	(%rsi,%rsi,4), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$3777893186295716171, %rcx # imm = 0x346DC5D63886594B
	imulq	%rcx
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$11, %rcx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	leaq	-200000(%rsi,%rsi,4), %rax
	andq	%r12, %rax
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$10000, %rax            # imm = 0x2710
	movq	$19, %rax
	jb	.LBB0_321
# BB#320:                               # %for.end391.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movq	%rdx, %rax
.LBB0_321:                              # %for.end391.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movl	64(%rsp,%rax,4), %ecx
	incq	%rcx
	movl	%ecx, 64(%rsp,%rax,4)
	movq	8(%r13), %rdi
	movq	$16, %rsi
	xorq	%rdx, %rdx
	callq	find_reg_note
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	gen_rtx_CONST_INT
	testq	%rbx, %rbx
	jne	.LBB0_322
# BB#324:                               # %if.else409.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movq	8(%r13), %rcx
	movq	56(%rcx), %rcx
	movq	$3, %rdi
	movq	$16, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	8(%r13), %rcx
	movq	%rax, 56(%rcx)
	jmp	.LBB0_323
	.align	16, 0x90
.LBB0_338:                              #   in Loop: Header=BB0_303 Depth=1
	movq	40(%rsp), %r14          # 8-byte Reload
	jmp	.LBB0_344
	.align	16, 0x90
.LBB0_313:                              #   in Loop: Header=BB0_303 Depth=1
	movq	%rbx, %rdi
	movq	40(%rsp), %r14          # 8-byte Reload
	movabsq	$1, %r15
	jmp	.LBB0_344
.LBB0_341:                              # %land.lhs.true484.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movq	40(%rbx), %rax
	cmpq	$0, 8(%rax)
	movq	%r13, %rdi
	movq	%rdi, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	je	.LBB0_343
# BB#342:                               # %land.lhs.true484.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movq	%r15, %rax
.LBB0_343:                              # %land.lhs.true484.i
                                        #   in Loop: Header=BB0_303 Depth=1
	addq	%rax, %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	jmp	.LBB0_344
.LBB0_322:                              # %if.then405.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movq	%rax, 8(%rbx)
.LBB0_323:                              # %for.inc492.i
                                        #   in Loop: Header=BB0_303 Depth=1
	incq	16(%rsp)                # 8-byte Folded Spill
	movabsq	$0, %rdi
	.align	16, 0x90
.LBB0_344:                              # %for.inc492.i
                                        #   in Loop: Header=BB0_303 Depth=1
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movl	n_basic_blocks(%rip), %eax
	sarq	%cl, %rdx
	incq	%r14
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB0_303
	jmp	.LBB0_345
.LBB0_300:
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
.LBB0_345:                              # %for.end494.i
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_352
# BB#346:                               # %if.then496.i
	movabsq	$.L.str39, %rsi
	xorq	%rax, %rax
	xorq	%r14, %r14
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdx
	callq	fprintf
	movq	rtl_dump_file(%rip), %rdi
	movabsq	$.L.str40, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movq	%rbx, %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	je	.LBB0_349
# BB#347:                               # %for.body504.i.preheader
	leaq	140(%rsp), %r12
	leaq	64(%rsp), %r13
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB0_348:                              # %for.body504.i
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r13), %eax
	movl	(%r12), %ecx
	movq	rtl_dump_file(%rip), %rdi
	addq	%rax, %rcx
	imulq	$100, %rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rdx, %r15
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %rcx
	leaq	5(%r14,%r14,4), %r8
	xorq	%rax, %rax
	movabsq	$.L.str25, %rsi
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	fprintf
	movq	%r15, %rdx
	incq	%r14
	addq	$5, %rbx
	addq	$-4, %r12
	addq	$4, %r13
	cmpq	$50, %rbx
	jne	.LBB0_348
.LBB0_349:                              # %if.end520.i
	movl	total_num_branches(%rip), %eax
	addq	%rdx, %rax
	movl	%eax, total_num_branches(%rip)
	xorq	%rax, %rax
	.align	16, 0x90
.LBB0_350:                              # %for.body526.i
                                        # =>This Inner Loop Header: Depth=1
	movl	64(%rsp,%rax,4), %ecx
	movl	total_hist_br_prob(,%rax,4), %edx
	addq	%rcx, %rdx
	movl	%edx, total_hist_br_prob(,%rax,4)
	incq	%rax
	cmpq	$20, %rax
	jne	.LBB0_350
# BB#351:                               # %for.end534.i
	movq	rtl_dump_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	movq	rtl_dump_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
.LBB0_352:                              # %compute_branch_probabilities.exit
	callq	free_aux_for_blocks
.LBB0_353:                              # %if.end350
	movl	profile_arc_flag(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_376
# BB#354:                               # %if.then352
	movq	24(%rsp), %rax          # 8-byte Reload
	movl	4(%rax), %eax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	callq	remove_fake_edges
	movl	n_basic_blocks(%rip), %eax
	leaq	2(%rax), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%r15, %r15
	testq	%rdx, %rdx
	jle	.LBB0_373
# BB#355:
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB0_356:                              # %for.body.i641
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_360 Depth 2
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdi
	movabsq	$entry_exit_blocks, %rcx
	je	.LBB0_359
# BB#357:                               # %cond.false.i642
                                        #   in Loop: Header=BB0_356 Depth=1
	movq	%rdi, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%rdx, %rsi
	andq	%rsi, %rcx
	leaq	1(%rax), %rdx
	andq	%rsi, %rdx
	cmpq	%rdx, %rcx
	movabsq	$entry_exit_blocks+112, %rcx
	je	.LBB0_359
# BB#358:                               # %cond.false6.i
                                        #   in Loop: Header=BB0_356 Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	24(%rcx,%rdi,8), %rcx
.LBB0_359:                              # %cond.end8.i
                                        #   in Loop: Header=BB0_356 Depth=1
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	40(%rcx), %r14
	testq	%r14, %r14
	je	.LBB0_372
	.align	16, 0x90
.LBB0_360:                              # %while.body.i648
                                        #   Parent Loop BB0_356 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	40(%r14), %rax
	movzbl	(%rax), %eax
	testq	$6, %rax
	jne	.LBB0_370
# BB#361:                               # %if.then.i651
                                        #   in Loop: Header=BB0_360 Depth=2
	movzbl	48(%r14), %eax
	testq	$2, %rax
	jne	.LBB0_385
# BB#362:                               # %if.end.i
                                        #   in Loop: Header=BB0_360 Depth=2
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_367
# BB#363:                               # %if.then19.i
                                        #   in Loop: Header=BB0_360 Depth=2
	movq	16(%r14), %rcx
	movq	24(%r14), %rax
	movq	40(%rcx), %rsi
	movl	88(%rcx), %edx
	movl	88(%rax), %ecx
	cmpq	$0, 8(%rsi)
	movabsq	$.L.str32, %r8
	je	.LBB0_366
# BB#364:                               # %land.rhs.i
                                        #   in Loop: Header=BB0_360 Depth=2
	movq	32(%rax), %rax
	cmpq	$0, (%rax)
	movabsq	$.L.str31, %r8
	jne	.LBB0_366
# BB#365:                               # %select.mid
                                        #   in Loop: Header=BB0_360 Depth=2
	movabsq	$.L.str32, %r8
.LBB0_366:                              # %land.end.i
                                        #   in Loop: Header=BB0_360 Depth=2
	xorq	%rax, %rax
	movabsq	$.L.str30, %rsi
	callq	fprintf
.LBB0_367:                              # %if.end27.i
                                        #   in Loop: Header=BB0_360 Depth=2
	movl	total_num_edges_instrumented(%rip), %r12d
	movq	$1, %rax
	movb	%al, need_func_profiler(%rip)
	addq	%r15, %r12
	movq	$64, %rdi
	movq	$1, %rsi
	xorq	%rdx, %rdx
	callq	mode_for_size
	movq	%rax, %r13
	callq	start_sequence
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	profiler_label(%rip), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	force_reg
	movq	$35, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	plus_constant_wide
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rdi
	callq	validize_mem
	movq	const_int_rtx+520(%rip), %rcx
	movq	%rax, %rbx
	movq	$75, %rsi
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	%r13, %rdi
	movq	%rbx, %rdx
	movq	%rbx, %r8
	callq	expand_simple_binop
	movq	%rax, %r12
	cmpq	%rbx, %r12
	je	.LBB0_369
# BB#368:                               # %if.then.i.i661
                                        #   in Loop: Header=BB0_360 Depth=2
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	emit_move_insn
.LBB0_369:                              # %gen_edge_profiler.exit.i
                                        #   in Loop: Header=BB0_360 Depth=2
	incq	%r15
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	insert_insn_on_edge
.LBB0_370:                              # %if.end30.i
                                        #   in Loop: Header=BB0_360 Depth=2
	movq	8(%r14), %r14
	testq	%r14, %r14
	jne	.LBB0_360
# BB#371:                               # %for.inc.i666.loopexit
                                        #   in Loop: Header=BB0_356 Depth=1
	movl	n_basic_blocks(%rip), %eax
.LBB0_372:                              # %for.inc.i666
                                        #   in Loop: Header=BB0_356 Depth=1
	movq	$32, %rcx
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rdi
	leaq	1(%rax), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB0_356
.LBB0_373:                              # %for.end.i668
	movl	total_num_edges_instrumented(%rip), %eax
	addq	%r15, %rax
	movl	%eax, total_num_edges_instrumented(%rip)
	movl	total_num_blocks_created(%rip), %ecx
	movq	rtl_dump_file(%rip), %rdi
	movl	%eax, count_instrumented_edges(%rip)
	movq	32(%rsp), %rax          # 8-byte Reload
	addq	%rax, %rcx
	movl	%ecx, total_num_blocks_created(%rip)
	testq	%rdi, %rdi
	je	.LBB0_375
# BB#374:                               # %if.then36.i
	movabsq	$.L.str33, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdx
	callq	fprintf
.LBB0_375:                              # %instrument_edges.exit
	callq	commit_edge_insertions
	callq	max_reg_num
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	allocate_reg_info
.LBB0_376:                              # %if.end355
	callq	remove_fake_edges
	movl	profile_arc_flag(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_377
# BB#378:                               # %if.end355
	movabsq	$1, %rdi
	jmp	.LBB0_379
.LBB0_377:
	movabsq	$0, %rdi
.LBB0_379:                              # %if.end355
	callq	cleanup_cfg
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_381
# BB#380:                               # %if.then360
	callq	dump_flow_info
.LBB0_381:                              # %if.end361
	callq	free_aux_for_edges
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	free_edge_list
	addq	$144, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_383:                              # %if.then215.i
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.compute_branch_probabilities, %rdx
	movq	$352, %rsi              # imm = 0x160
	callq	fancy_abort
.LBB0_384:                              # %if.then269.i
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.compute_branch_probabilities, %rdx
	movq	$379, %rsi              # imm = 0x17B
	callq	fancy_abort
.LBB0_385:                              # %if.then17.i
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.instrument_edges, %rdx
	movq	$151, %rsi
	callq	fancy_abort
.LBB0_10:                               # %if.then26
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.branch_prob, %rcx
	movq	$592, %rsi              # imm = 0x250
	jmp	.LBB0_11
.LBB0_89:                               # %if.then.i.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.union_groups, %rcx
	movq	$858, %rsi              # imm = 0x35A
	jmp	.LBB0_11
.LBB0_301:                              # %if.then308.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.compute_branch_probabilities, %rcx
	movq	$403, %rsi              # imm = 0x193
.LBB0_11:                               # %if.then26
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp17:
	.size	branch_prob, .Ltmp17-branch_prob
	.cfi_endproc

	.align	16, 0x90
	.type	output_gcov_string,@function
output_gcov_string:                     # @output_gcov_string
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
	subq	$48, %rsp
.Ltmp29:
	.cfi_def_cfa_offset 96
.Ltmp30:
	.cfi_offset %rbx, -48
.Ltmp31:
	.cfi_offset %r12, -40
.Ltmp32:
	.cfi_offset %r13, -32
.Ltmp33:
	.cfi_offset %r14, -24
.Ltmp34:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	$63, %rcx
	movq	%rsi, %rax
	sarq	%cl, %rax
	leaq	(%rsi,%rax), %r14
	movq	bb_file(%rip), %r8
	xorq	%rax, %r14
	movb	%r14b, 38(%rsp)
	movq	$63, %rcx
	movq	%r14, %r13
	sarq	%cl, %r13
	movq	$56, %rcx
	movq	%r13, %rax
	shrq	%cl, %rax
	addq	%r14, %rax
	movq	$8, %rcx
	sarq	%cl, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movb	%al, 39(%rsp)
	movq	$48, %rcx
	movq	%r13, %r15
	shrq	%cl, %r15
	addq	%r14, %r15
	movq	$16, %rcx
	sarq	%cl, %r15
	movb	%r15b, 40(%rsp)
	movq	$40, %rcx
	movq	%r13, %r12
	shrq	%cl, %r12
	addq	%r14, %r12
	movq	$24, %rcx
	shrq	%cl, %r12
	andq	$127, %r12
	movq	$32, %rcx
	shrq	%cl, %r13
	addq	%r14, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	incq	%r13
	cmpq	$2, %r13
	jb	.LBB1_2
# BB#1:                                 # %__store_gcov_type.exit.thread2.i
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movb	%r12b, 41(%rsp)
	movq	%r8, 8(%rsp)            # 8-byte Spill
	jmp	.LBB1_3
.LBB1_2:                                # %if.else.i
	movq	$56, %rcx
	movq	%rsi, %rax
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	shrq	%cl, %rax
	andq	$128, %rax
	orq	%r12, %rax
	movb	%al, 41(%rsp)
	leaq	38(%rsp), %rdi
	movq	$1, %rsi
	movq	$4, %rdx
	movq	%r8, %rcx
	callq	fwrite
	movq	bb_file(%rip), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB1_3:                                # %__write_long.exit
	movq	%rbx, %rdi
	callq	strlen
	movq	%rbx, %rdi
	movq	%rax, %rbx
	incq	%rbx
	movq	$1, %rdx
	movq	%rbx, %rsi
	movq	8(%rsp), %rcx           # 8-byte Reload
	callq	fwrite
	andq	$3, %rbx
	je	.LBB1_5
# BB#4:                                 # %if.then
	movq	$4, %rdx
	movq	bb_file(%rip), %rcx
	subq	%rbx, %rdx
	xorq	%rax, %rax
	movl	%eax, 32(%rsp)
	leaq	32(%rsp), %rdi
	movq	$1, %rsi
	callq	fwrite
.LBB1_5:                                # %if.end
	cmpq	$2, %r13
	movq	bb_file(%rip), %rax
	movb	%r14b, 38(%rsp)
	movq	16(%rsp), %rcx          # 8-byte Reload
	movb	%cl, 39(%rsp)
	movb	%r15b, 40(%rsp)
	movq	24(%rsp), %rdx          # 8-byte Reload
	jb	.LBB1_7
# BB#6:                                 # %__store_gcov_type.exit.thread2.i32
	movb	%r12b, 41(%rsp)
	jmp	.LBB1_8
.LBB1_7:                                # %if.else.i36
	movq	$56, %rcx
	shrq	%cl, %rdx
	andq	$128, %rdx
	orq	%rdx, %r12
	movb	%r12b, 41(%rsp)
	leaq	38(%rsp), %rdi
	movq	$1, %rsi
	movq	$4, %rdx
	movq	%rax, %rcx
	callq	fwrite
.LBB1_8:                                # %__write_long.exit37
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp35:
	.size	output_gcov_string, .Ltmp35-output_gcov_string
	.cfi_endproc

	.globl	init_branch_prob
	.align	16, 0x90
	.type	init_branch_prob,@function
init_branch_prob:                       # @init_branch_prob
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp39:
	.cfi_def_cfa_offset 16
.Ltmp40:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp41:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
	subq	$48, %rsp
.Ltmp42:
	.cfi_offset %rbx, -48
.Ltmp43:
	.cfi_offset %r12, -40
.Ltmp44:
	.cfi_offset %r14, -32
.Ltmp45:
	.cfi_offset %r15, -24
	movl	flag_test_coverage(%rip), %eax
	movq	%rdi, %r15
	testq	%rax, %rax
	je	.LBB2_4
# BB#1:                                 # %if.then
	movq	%r15, %rdi
	callq	strlen
	movq	%rax, %r14
	movq	$32, %rcx
	movq	%r14, %r12
	shlq	%cl, %r12
	movabsq	$17179869184, %rax      # imm = 0x400000000
	addq	%r12, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$15, %rax
	andq	$-16, %rax
	movq	%rsp, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rsp
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	strcpy
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	strip_off_ending
	movq	%rbx, %rdi
	callq	strlen
	movq	$6447662, %rcx          # imm = 0x62622E
	movl	%ecx, (%rax,%rbx)
	movabsq	$.L.str7, %rsi
	movq	%rbx, %rdi
	callq	fopen
	testq	%rax, %rax
	movq	%rax, bb_file(%rip)
	je	.LBB2_11
# BB#2:                                 # %if.end
	movabsq	$21474836480, %rax      # imm = 0x500000000
	addq	%rax, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	addq	$15, %r12
	andq	$-16, %r12
	movq	%rsp, %rbx
	subq	%r12, %rbx
	movq	%rbx, %rsp
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	strcpy
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	strip_off_ending
	movq	%rbx, %rdi
	callq	strlen
	xorq	%rcx, %rcx
	movb	%cl, 4(%rax,%rbx)
	movq	$1734500910, %rcx       # imm = 0x6762622E
	movl	%ecx, (%rbx,%rax)
	movabsq	$.L.str7, %rsi
	movq	%rbx, %rdi
	callq	fopen
	testq	%rax, %rax
	movq	%rax, bbg_file(%rip)
	je	.LBB2_11
# BB#3:                                 # %if.end16
	movq	$0, last_bb_file_name(%rip)
.LBB2_4:                                # %if.end17
	movl	flag_branch_probabilities(%rip), %eax
	testq	%rax, %rax
	je	.LBB2_8
# BB#5:                                 # %if.then19
	movq	%r15, %rdi
	callq	strlen
	movq	%rax, %r14
	movq	%rsp, %rbx
	leaq	19(%r14), %rax
	andq	$-16, %rax
	subq	%rax, %rbx
	movq	%rbx, %rsp
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	strcpy
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	strip_off_ending
	movq	%rbx, %rdi
	callq	strlen
	movq	$6382638, %rcx          # imm = 0x61642E
	movl	%ecx, (%rax,%rbx)
	movabsq	$.L.str11, %rsi
	movq	%rbx, %rdi
	callq	fopen
	testq	%rax, %rax
	movq	%rax, da_file(%rip)
	jne	.LBB2_7
# BB#6:                                 # %if.end29
	movabsq	$.L.str12, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	warning
	movq	da_file(%rip), %rax
	testq	%rax, %rax
	je	.LBB2_8
.LBB2_7:                                # %if.then31
	leaq	-74(%rbp), %rdi
	movq	$1, %rsi
	movq	$8, %rdx
	movq	%rax, %rcx
	callq	fread
.LBB2_8:                                # %if.end34
	movl	profile_arc_flag(%rip), %eax
	testq	%rax, %rax
	je	.LBB2_10
# BB#9:                                 # %if.then36
	movabsq	$.L.str27, %rsi
	movabsq	$.L.str28, %rdx
	leaq	-64(%rbp), %r14
	movq	$2, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	sprintf
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	%r14, %rdi
	callq	ggc_alloc_string
	movq	$68, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_s
	movq	%rax, profiler_label(%rip)
	movabsq	$profiler_label, %rdi
	movq	$1, %rsi
	callq	ggc_add_rtx_root
.LBB2_10:                               # %if.end37
	xorq	%rax, %rax
	movl	%eax, total_num_blocks(%rip)
	movl	%eax, total_num_edges(%rip)
	movl	%eax, total_num_edges_ignored(%rip)
	movl	%eax, total_num_edges_instrumented(%rip)
	movl	%eax, total_num_blocks_created(%rip)
	movl	%eax, total_num_passes(%rip)
	movl	%eax, total_num_times_called(%rip)
	movl	%eax, total_num_branches(%rip)
	movq	$0, total_hist_br_prob+72(%rip)
	movq	$0, total_hist_br_prob+64(%rip)
	movq	$0, total_hist_br_prob+56(%rip)
	movq	$0, total_hist_br_prob+48(%rip)
	movq	$0, total_hist_br_prob+40(%rip)
	movq	$0, total_hist_br_prob+32(%rip)
	movq	$0, total_hist_br_prob+24(%rip)
	movq	$0, total_hist_br_prob+16(%rip)
	movq	$0, total_hist_br_prob+8(%rip)
	movq	$0, total_hist_br_prob(%rip)
	leaq	-32(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB2_11:                               # %if.then7
	movabsq	$.L.str8, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	callq	fatal_io_error
.Ltmp46:
	.size	init_branch_prob, .Ltmp46-init_branch_prob
	.cfi_endproc

	.globl	end_branch_prob
	.align	16, 0x90
	.type	end_branch_prob,@function
end_branch_prob:                        # @end_branch_prob
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
	subq	$16, %rsp
.Ltmp58:
	.cfi_def_cfa_offset 64
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
	movl	flag_test_coverage(%rip), %eax
	testq	%rax, %rax
	je	.LBB3_2
# BB#1:                                 # %if.then
	movq	bb_file(%rip), %rdi
	callq	fclose
	movq	bbg_file(%rip), %rdi
	callq	fclose
.LBB3_2:                                # %if.end
	movl	flag_branch_probabilities(%rip), %eax
	testq	%rax, %rax
	je	.LBB3_9
# BB#3:                                 # %if.end
	movq	da_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB3_9
# BB#4:                                 # %if.then5
	callq	feof
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB3_6
# BB#5:                                 # %if.then8
	movabsq	$.L.str13, %rdi
	xorq	%rax, %rax
	callq	error
.LBB3_6:                                # %if.end9
	movq	da_file(%rip), %rcx
	leaq	6(%rsp), %rdi
	movq	$1, %rsi
	movq	$8, %rdx
	callq	fread
	cmpq	$8, %rax
	jne	.LBB3_8
# BB#7:                                 # %if.then11
	movabsq	$.L.str14, %rdi
	xorq	%rax, %rax
	callq	error
.LBB3_8:                                # %if.end12
	movq	da_file(%rip), %rdi
	callq	fclose
.LBB3_9:                                # %if.end15
	movq	rtl_dump_file(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB3_16
# BB#10:                                # %if.then17
	movq	$10, %rdi
	callq	fputc
	movq	rtl_dump_file(%rip), %rdi
	movl	total_num_blocks(%rip), %edx
	movabsq	$.L.str16, %rsi
	xorq	%rax, %rax
	xorq	%r15, %r15
	callq	fprintf
	movq	rtl_dump_file(%rip), %rdi
	movl	total_num_edges(%rip), %edx
	movabsq	$.L.str17, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movq	rtl_dump_file(%rip), %rdi
	movl	total_num_edges_ignored(%rip), %edx
	movabsq	$.L.str18, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movq	rtl_dump_file(%rip), %rdi
	movl	total_num_edges_instrumented(%rip), %edx
	movabsq	$.L.str19, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movq	rtl_dump_file(%rip), %rdi
	movl	total_num_blocks_created(%rip), %edx
	movabsq	$.L.str20, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movq	rtl_dump_file(%rip), %rdi
	movl	total_num_passes(%rip), %edx
	movabsq	$.L.str21, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movl	total_num_times_called(%rip), %esi
	testq	%rsi, %rsi
	je	.LBB3_12
# BB#11:                                # %if.then26
	movl	total_num_passes(%rip), %edx
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$1, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	addq	%rdx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	rtl_dump_file(%rip), %rdi
	cqto
	idivq	%rsi
	movq	%rax, %rcx
	movabsq	$.L.str22, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdx
	callq	fprintf
.LBB3_12:                               # %if.end28
	movq	rtl_dump_file(%rip), %rdi
	movl	total_num_branches(%rip), %edx
	movabsq	$.L.str23, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movq	rtl_dump_file(%rip), %rdi
	movabsq	$.L.str24, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movl	total_num_branches(%rip), %esi
	testq	%rsi, %rsi
	je	.LBB3_16
# BB#13:
	movabsq	$total_hist_br_prob+76, %r12
	movabsq	$total_hist_br_prob, %r13
	movabsq	$.L.str25, %r14
	xorq	%rbx, %rbx
	jmp	.LBB3_14
	.align	16, 0x90
.LBB3_15:                               # %for.body.for.body_crit_edge
                                        #   in Loop: Header=BB3_14 Depth=1
	incq	%r15
	movl	total_num_branches(%rip), %esi
	addq	$5, %rbx
	addq	$-4, %r12
	addq	$4, %r13
.LBB3_14:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r13), %eax
	movl	(%r12), %ecx
	movq	rtl_dump_file(%rip), %rdi
	addq	%rax, %rcx
	imulq	$100, %rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %rcx
	leaq	5(%r15,%r15,4), %r8
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	fprintf
	cmpq	$45, %rbx
	jne	.LBB3_15
.LBB3_16:                               # %if.end43
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp64:
	.size	end_branch_prob, .Ltmp64-end_branch_prob
	.cfi_endproc

	.globl	output_func_start_profiler
	.align	16, 0x90
	.type	output_func_start_profiler,@function
output_func_start_profiler:             # @output_func_start_profiler
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp71:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp72:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp73:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp74:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp75:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp76:
	.cfi_def_cfa_offset 80
.Ltmp77:
	.cfi_offset %rbx, -48
.Ltmp78:
	.cfi_offset %r12, -40
.Ltmp79:
	.cfi_offset %r13, -32
.Ltmp80:
	.cfi_offset %r14, -24
.Ltmp81:
	.cfi_offset %r15, -16
	movq	$64, %rdi
	movq	$1, %rsi
	xorq	%rdx, %rdx
	xorq	%r13, %r13
	callq	mode_for_size
	movzbl	need_func_profiler(%rip), %ecx
	movq	%rax, %r14
	cmpq	$1, %rcx
	jne	.LBB4_7
# BB#1:                                 # %if.end
	movb	%r13b, need_func_profiler(%rip)
	movq	$73, %rdi
	callq	get_file_function_name
	movq	32(%rax), %rdi
	movabsq	$.L.str26, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	concat
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	get_identifier
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	free
	movq	global_trees+216(%rip), %rdi
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	andq	$-257, 48(%rbx)         # imm = 0xFFFFFFFFFFFFFEFF
	movzbl	targetm+329(%rip), %eax
	xorq	$1, %rax
	andq	$255, %rax
	movl	16(%rbx), %edx
	movq	$19, %rcx
	shlq	%cl, %rax
	movabsq	$4294377471, %rcx       # imm = 0xFFF6FFFF
	andq	%rdx, %rcx
	orq	%rax, %rcx
	orq	$65536, %rcx            # imm = 0x10000
	movl	%ecx, 16(%rbx)
	movq	global_trees+216(%rip), %rdx
	movq	$36, %rdi
	xorq	%rsi, %rsi
	callq	build_decl
	movq	%rax, 96(%rbx)
	movq	%rbx, %rdi
	callq	pushdecl
	movq	%rax, %r12
	xorq	%rsi, %rsi
	movq	$1, %rdx
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	callq	rest_of_decl_compilation
	movq	%r12, %rdi
	callq	announce_function
	movq	global_trees(%rip), %rax
	movq	%r12, current_function_decl(%rip)
	movq	%rax, 104(%r12)
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	input_filename(%rip), %rsi
	movl	lineno(%rip), %edx
	movq	%r12, %rdi
	callq	init_function_start
	xorq	%rdi, %rdi
	callq	pushlevel
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	expand_function_start
	movabsq	$.L.str27, %rsi
	movabsq	$.L.str28, %rdx
	leaq	(%rsp), %r15
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	sprintf
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	%r15, %rdi
	callq	ggc_alloc_string
	movq	$68, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_s
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	force_reg
	movl	target_flags(%rip), %esi
	movq	%rax, %rbx
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movabsq	$.L.str29, %rdx
	movq	$68, %rdi
	callq	gen_rtx_fmt_s
	movl	target_flags(%rip), %r9d
	movq	%rax, %rdx
	movq	$25, %rcx
	shrq	%cl, %r9
	andq	$1, %r9
	orq	$4, %r9
	xorq	%rsi, %rsi
	movq	$1, %rcx
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	%r14, %rdx
	movq	%rbx, %r8
	callq	emit_library_call
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	xorq	%rdx, %rdx
	callq	expand_function_end
	movq	$1, %rdi
	xorq	%rsi, %rsi
	movq	$1, %rdx
	callq	poplevel
	movl	%r13d, flag_inline_functions(%rip)
	movl	%r13d, flag_test_coverage(%rip)
	movl	%r13d, profile_arc_flag(%rip)
	movl	%r13d, flag_branch_probabilities(%rip)
	movq	%r12, %rdi
	callq	rest_of_compilation
	movl	flag_inline_functions(%rip), %eax
	movl	%eax, flag_inline_functions(%rip)
	movl	quiet_flag(%rip), %eax
	movl	flag_test_coverage(%rip), %ecx
	movl	%ecx, flag_test_coverage(%rip)
	movl	profile_arc_flag(%rip), %ecx
	movl	%ecx, profile_arc_flag(%rip)
	movl	flag_branch_probabilities(%rip), %ecx
	movl	%ecx, flag_branch_probabilities(%rip)
	testq	%rax, %rax
	jne	.LBB4_3
# BB#2:                                 # %if.then35
	movq	asm_out_file(%rip), %rdi
	callq	fflush
.LBB4_3:                                # %if.end37
	movzbl	targetm+329(%rip), %eax
	movq	$0, current_function_decl(%rip)
	testq	%rax, %rax
	je	.LBB4_7
# BB#4:                                 # %if.then39
	movq	144(%r12), %rax
	movq	targetm+152(%rip), %rbx
	testq	%rax, %rax
	jne	.LBB4_6
# BB#5:                                 # %cond.false
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rax
.LBB4_6:                                # %cond.end
	movq	8(%rax), %rdi
	movq	$65535, %rsi            # imm = 0xFFFF
	callq	*%rbx
.LBB4_7:                                # %if.end47
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp82:
	.size	output_func_start_profiler, .Ltmp82-output_func_start_profiler
	.cfi_endproc

	.type	total_num_times_called,@object # @total_num_times_called
	.local	total_num_times_called
	.comm	total_num_times_called,4,4
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"profile.c"
	.size	.L.str, 10

	.type	.L__FUNCTION__.branch_prob,@object # @__FUNCTION__.branch_prob
.L__FUNCTION__.branch_prob:
	.asciz	"branch_prob"
	.size	.L__FUNCTION__.branch_prob, 12

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Adding fake exit edge to bb %i\n"
	.size	.L.str1, 32

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Adding fake entry edge to bb %i\n"
	.size	.L.str2, 33

	.type	branch_prob.ignore_next_note,@object # @branch_prob.ignore_next_note
	.local	branch_prob.ignore_next_note
	.comm	branch_prob.ignore_next_note,1,1
	.type	bb_file,@object         # @bb_file
	.local	bb_file
	.comm	bb_file,8,8
	.type	last_bb_file_name,@object # @last_bb_file_name
	.local	last_bb_file_name
	.comm	last_bb_file_name,8,8
	.type	total_num_blocks,@object # @total_num_blocks
	.local	total_num_blocks
	.comm	total_num_blocks,4,4
	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"%d basic blocks\n"
	.size	.L.str3, 17

	.type	total_num_edges,@object # @total_num_edges
	.local	total_num_edges
	.comm	total_num_edges,4,4
	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"%d edges\n"
	.size	.L.str4, 10

	.type	total_num_edges_ignored,@object # @total_num_edges_ignored
	.local	total_num_edges_ignored
	.comm	total_num_edges_ignored,4,4
	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"%d ignored edges\n"
	.size	.L.str5, 18

	.type	bbg_file,@object        # @bbg_file
	.local	bbg_file
	.comm	bbg_file,8,8
	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"wb"
	.size	.L.str7, 3

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"can't open %s"
	.size	.L.str8, 14

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	".bbg"
	.size	.L.str9, 5

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"rb"
	.size	.L.str11, 3

	.type	da_file,@object         # @da_file
	.local	da_file
	.comm	da_file,8,8
	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"file %s not found, execution counts assumed to be zero"
	.size	.L.str12, 55

	.type	total_num_edges_instrumented,@object # @total_num_edges_instrumented
	.local	total_num_edges_instrumented
	.comm	total_num_edges_instrumented,4,4
	.type	total_num_blocks_created,@object # @total_num_blocks_created
	.local	total_num_blocks_created
	.comm	total_num_blocks_created,4,4
	.type	total_num_passes,@object # @total_num_passes
	.local	total_num_passes
	.comm	total_num_passes,4,4
	.type	total_num_branches,@object # @total_num_branches
	.local	total_num_branches
	.comm	total_num_branches,4,4
	.type	total_hist_br_prob,@object # @total_hist_br_prob
	.local	total_hist_br_prob
	.comm	total_hist_br_prob,80,16
	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	".da file contents exhausted too early"
	.size	.L.str13, 38

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	".da file contents not exhausted"
	.size	.L.str14, 32

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"Total number of blocks: %d\n"
	.size	.L.str16, 28

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"Total number of edges: %d\n"
	.size	.L.str17, 27

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"Total number of ignored edges: %d\n"
	.size	.L.str18, 35

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"Total number of instrumented edges: %d\n"
	.size	.L.str19, 40

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"Total number of blocks created: %d\n"
	.size	.L.str20, 36

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"Total number of graph solution passes: %d\n"
	.size	.L.str21, 43

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"Average number of graph solution passes: %d\n"
	.size	.L.str22, 45

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"Total number of branches: %d\n"
	.size	.L.str23, 30

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"Total number of branches never executed: %d\n"
	.size	.L.str24, 45

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"%d%% branches in range %d-%d%%\n"
	.size	.L.str25, 32

	.type	need_func_profiler,@object # @need_func_profiler
	.local	need_func_profiler
	.comm	need_func_profiler,1,1
	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"GCOV"
	.size	.L.str26, 5

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"*.%s%u"
	.size	.L.str27, 7

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"LPBX"
	.size	.L.str28, 5

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"__bb_init_func"
	.size	.L.str29, 15

	.type	count_instrumented_edges,@object # @count_instrumented_edges
	.comm	count_instrumented_edges,4,4
	.type	profiler_label,@object  # @profiler_label
	.local	profiler_label
	.comm	profiler_label,8,8
	.type	.L__FUNCTION__.union_groups,@object # @__FUNCTION__.union_groups
.L__FUNCTION__.union_groups:
	.asciz	"union_groups"
	.size	.L__FUNCTION__.union_groups, 13

	.type	.L__FUNCTION__.instrument_edges,@object # @__FUNCTION__.instrument_edges
.L__FUNCTION__.instrument_edges:
	.asciz	"instrument_edges"
	.size	.L__FUNCTION__.instrument_edges, 17

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"Edge %d to %d instrumented%s\n"
	.size	.L.str30, 30

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	" (and split)"
	.size	.L.str31, 13

	.type	.L.str32,@object        # @.str32
.L.str32:
	.zero	1
	.size	.L.str32, 1

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"%d edges instrumented\n"
	.size	.L.str33, 23

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"\nRead edge from %i to %i, count:"
	.size	.L.str34, 33

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"%lld"
	.size	.L.str35, 5

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"\n%d edge counts read\n"
	.size	.L.str36, 22

	.type	.L__FUNCTION__.compute_branch_probabilities,@object # @__FUNCTION__.compute_branch_probabilities
.L__FUNCTION__.compute_branch_probabilities:
	.asciz	"compute_branch_probabilities"
	.size	.L__FUNCTION__.compute_branch_probabilities, 29

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"Graph solving took %d passes.\n\n"
	.size	.L.str37, 32

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"corrupted profile info: prob for %d-%d thought to be %d"
	.size	.L.str38, 56

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"%d branches\n"
	.size	.L.str39, 13

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"%d branches never executed\n"
	.size	.L.str40, 28


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
