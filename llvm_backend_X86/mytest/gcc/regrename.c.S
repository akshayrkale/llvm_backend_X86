	.file	"regrename.c.bc"
	.text
	.globl	regrename_optimize
	.align	16, 0x90
	.type	regrename_optimize,@function
regrename_optimize:                     # @regrename_optimize
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
	subq	$640, %rsp              # imm = 0x280
.Ltmp11:
	.cfi_def_cfa_offset 688
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
	leaq	128(%rsp), %rdi
	xorq	%rsi, %rsi
	movq	$212, %rdx
	callq	memset
	movabsq	$rename_obstack, %rdi
	callq	gcc_obstack_init
	movq	rename_obstack+32(%rip), %rcx
	movq	rename_obstack+24(%rip), %rax
	cmpq	%rax, %rcx
	jns	.LBB0_2
# BB#1:                                 # %if.then
	movabsq	$rename_obstack, %rdi
	xorq	%rsi, %rsi
	callq	_obstack_newchunk
	movq	rename_obstack+24(%rip), %rax
.LBB0_2:                                # %if.end
	movq	rename_obstack+16(%rip), %rbx
	cmpq	%rbx, %rax
	jne	.LBB0_4
# BB#3:                                 # %if.then8
	movzbl	rename_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, rename_obstack+80(%rip)
.LBB0_4:                                # %if.end9
	movslq	rename_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, rename_obstack+24(%rip)
	movq	rename_obstack+32(%rip), %rax
	movq	rename_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rax, %rdi
	subq	%rdx, %rdi
	cmpq	%rdi, %rsi
	jle	.LBB0_6
# BB#5:                                 # %if.then29
	movq	%rax, rename_obstack+24(%rip)
	movq	%rax, %rcx
.LBB0_6:                                # %if.end32
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	movslq	n_basic_blocks(%rip), %rax
	movq	%rcx, rename_obstack+16(%rip)
	testq	%rax, %rax
	jle	.LBB0_205
# BB#7:                                 # %for.body.lr.ph
	xorq	%r15, %r15
	movabsq	$4294967288, %r14       # imm = 0xFFFFFFF8
	movabsq	$.L.str13, %r12
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB0_8:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_11 Depth 2
                                        #       Child Loop BB0_16 Depth 3
                                        #       Child Loop BB0_24 Depth 3
                                        #       Child Loop BB0_26 Depth 3
                                        #       Child Loop BB0_31 Depth 3
                                        #       Child Loop BB0_37 Depth 3
                                        #       Child Loop BB0_39 Depth 3
                                        #       Child Loop BB0_51 Depth 3
                                        #       Child Loop BB0_49 Depth 3
                                        #       Child Loop BB0_63 Depth 3
                                        #       Child Loop BB0_72 Depth 3
                                        #       Child Loop BB0_92 Depth 3
                                        #       Child Loop BB0_76 Depth 3
                                        #       Child Loop BB0_81 Depth 3
                                        #       Child Loop BB0_83 Depth 3
                                        #       Child Loop BB0_98 Depth 3
                                        #       Child Loop BB0_87 Depth 3
                                        #       Child Loop BB0_105 Depth 3
                                        #     Child Loop BB0_112 Depth 2
                                        #       Child Loop BB0_126 Depth 3
                                        #     Child Loop BB0_132 Depth 2
                                        #     Child Loop BB0_135 Depth 2
                                        #     Child Loop BB0_137 Depth 2
                                        #       Child Loop BB0_143 Depth 3
                                        #       Child Loop BB0_148 Depth 3
                                        #         Child Loop BB0_153 Depth 4
                                        #       Child Loop BB0_161 Depth 3
                                        #         Child Loop BB0_175 Depth 4
                                        #         Child Loop BB0_183 Depth 4
                                        #       Child Loop BB0_197 Depth 3
	movq	basic_block_info(%rip), %rax
	movq	rtl_dump_file(%rip), %rdi
	movq	32(%rax,%r15,8), %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	testq	%rdi, %rdi
	je	.LBB0_10
# BB#9:                                 # %if.then43
                                        #   in Loop: Header=BB0_8 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str, %rsi
	movq	%r15, %rdx
	callq	fprintf
.LBB0_10:                               # %if.end44
                                        #   in Loop: Header=BB0_8 Depth=1
	movq	%r15, 72(%rsp)          # 8-byte Spill
	movq	$0, closed_chains(%rip)
	movq	$0, open_chains(%rip)
	movq	104(%rsp), %r15         # 8-byte Reload
	jmp	.LBB0_11
	.align	16, 0x90
.LBB0_109:                              # %for.inc505.i
                                        #   in Loop: Header=BB0_11 Depth=2
	addq	$24, %r15
.LBB0_11:                               # %for.cond.i
                                        #   Parent Loop BB0_8 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_16 Depth 3
                                        #       Child Loop BB0_24 Depth 3
                                        #       Child Loop BB0_26 Depth 3
                                        #       Child Loop BB0_31 Depth 3
                                        #       Child Loop BB0_37 Depth 3
                                        #       Child Loop BB0_39 Depth 3
                                        #       Child Loop BB0_51 Depth 3
                                        #       Child Loop BB0_49 Depth 3
                                        #       Child Loop BB0_63 Depth 3
                                        #       Child Loop BB0_72 Depth 3
                                        #       Child Loop BB0_92 Depth 3
                                        #       Child Loop BB0_76 Depth 3
                                        #       Child Loop BB0_81 Depth 3
                                        #       Child Loop BB0_83 Depth 3
                                        #       Child Loop BB0_98 Depth 3
                                        #       Child Loop BB0_87 Depth 3
                                        #       Child Loop BB0_105 Depth 3
	movq	(%r15), %r15
	movzbl	(%r15), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_108
# BB#12:                                # %if.then.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movslq	40(%r15), %rax
	testq	%rax, %rax
	jns	.LBB0_14
# BB#13:                                # %cond.false.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movq	%r15, %rdi
	callq	recog_memoized_1
.LBB0_14:                               # %cond.end.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	callq	extract_insn
	movq	$1, %rdi
	callq	constrain_operands
	callq	preprocess_constraints
	movsbq	recog_data+996(%rip), %r12
	movslq	which_alternative(%rip), %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	testq	%r12, %r12
	jle	.LBB0_29
# BB#15:                                # %for.body.lr.ph.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movq	32(%r15), %rax
	movq	%r15, %r9
	movzbl	(%rax), %r8d
	movq	$5, %rcx
	movq	112(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	leaq	recog_op_alt+20(%rdx), %rdx
	xorq	%rsi, %rsi
	movq	$2, %rax
	.align	16, 0x90
.LBB0_16:                               # %for.body.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movslq	-4(%rdx), %rdi
	testq	%rdi, %rdi
	js	.LBB0_18
# BB#17:                                # %if.then26.i
                                        #   in Loop: Header=BB0_16 Depth=3
	movq	$5, %rcx
	movq	112(%rsp), %rbx         # 8-byte Reload
	shlq	%cl, %rbx
	imulq	$960, %rdi, %rcx        # imm = 0x3C0
	movl	recog_op_alt+8(%rcx,%rbx), %ecx
	movl	%ecx, -12(%rdx)
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	jmp	.LBB0_21
	.align	16, 0x90
.LBB0_18:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB0_16 Depth=3
	movslq	(%rdx), %rcx
	testq	%rcx, %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	jns	.LBB0_21
# BB#19:                                # %lor.lhs.false44.i
                                        #   in Loop: Header=BB0_16 Depth=3
	cmpq	$38, %r8
	jne	.LBB0_22
# BB#20:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB0_16 Depth=3
	movl	recog_data+840(,%rsi,4), %ecx
	cmpq	$1, %rcx
	jne	.LBB0_22
	.align	16, 0x90
.LBB0_21:                               # %if.then49.i
                                        #   in Loop: Header=BB0_16 Depth=3
	movl	%eax, recog_data+840(,%rsi,4)
.LBB0_22:                               # %for.inc.i
                                        #   in Loop: Header=BB0_16 Depth=3
	incq	%rsi
	movq	%rsi, %rcx
	andq	%rdi, %rcx
	movq	%r12, %rbx
	andq	%rdi, %rbx
	addq	$960, %rdx              # imm = 0x3C0
	xorq	%r13, %r13
	cmpq	%rbx, %rcx
	jne	.LBB0_16
# BB#23:                                #   in Loop: Header=BB0_11 Depth=2
	movq	%r12, 88(%rsp)          # 8-byte Spill
	xorq	%r15, %r15
	movq	%r9, %r14
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_24:                               # %for.body56.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	recog_data+240(,%r15,8), %rsi
	movl	recog_data+840(,%r15,4), %r8d
	xorq	%rdx, %rdx
	movq	$1, %rcx
	xorq	%r9, %r9
	movq	%r14, %rdi
	callq	scan_rtx
	incq	%r15
	movq	%r15, %rax
	andq	%r12, %rax
	cmpq	%rbx, %rax
	jne	.LBB0_24
# BB#25:                                #   in Loop: Header=BB0_11 Depth=2
	movq	%r14, %r15
	movq	88(%rsp), %r12          # 8-byte Reload
	.align	16, 0x90
.LBB0_26:                               # %for.body67.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	recog_data(,%r13,8), %rax
	movq	recog_data+480(,%r13,8), %rcx
	movq	%rax, 384(%rsp,%r13,8)
	movzbl	(%rcx), %eax
	testq	%rax, %rax
	je	.LBB0_28
# BB#27:                                # %if.end79.i
                                        #   in Loop: Header=BB0_26 Depth=3
	movq	global_rtl+8(%rip), %rax
	movq	recog_data+240(,%r13,8), %rcx
	movq	%rax, (%rcx)
.LBB0_28:                               # %for.inc82.i
                                        #   in Loop: Header=BB0_26 Depth=3
	incq	%r13
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rbx, %rax
	jne	.LBB0_26
.LBB0_29:                               # %for.cond85.preheader.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movsbq	recog_data+997(%rip), %r9
	testq	%r9, %r9
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	96(%rsp), %r10          # 8-byte Reload
	jle	.LBB0_35
# BB#30:                                # %for.body89.lr.ph.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movq	$32, %rcx
	movq	%r10, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rcx
	leaq	insn_data+24(,%rcx,8), %r8
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB0_31:                               # %for.body89.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$32, %rcx
	movq	%r10, %rdi
	shlq	%cl, %rdi
	movq	recog_data+960(,%rsi,8), %rbx
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	(%rbx), %rdx
	movq	global_rtl+8(%rip), %rax
	movsbq	recog_data+992(%rsi), %rcx
	testq	%rdi, %rdi
	movq	%rdx, 352(%rsp,%rsi,8)
	movq	%rax, (%rbx)
	js	.LBB0_34
# BB#32:                                # %land.lhs.true101.i
                                        #   in Loop: Header=BB0_31 Depth=3
	movq	(%r8), %rax
	leaq	(%rcx,%rcx,2), %rdx
	movzbl	19(%rax,%rdx,8), %eax
	testq	%rax, %rax
	jne	.LBB0_34
# BB#33:                                # %if.then109.i
                                        #   in Loop: Header=BB0_31 Depth=3
	movq	recog_data(,%rcx,8), %rax
	movq	%rax, 352(%rsp,%rsi,8)
.LBB0_34:                               # %for.inc115.i
                                        #   in Loop: Header=BB0_31 Depth=3
	incq	%rsi
	movq	%rsi, %rax
	andq	%r14, %rax
	movq	%r9, %rcx
	andq	%r14, %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_31
.LBB0_35:                               # %for.end117.i
                                        #   in Loop: Header=BB0_11 Depth=2
	leaq	32(%r15), %rbx
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	scan_rtx
	movsbq	recog_data+997(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_38
# BB#36:                                # %for.body125.i.preheader
                                        #   in Loop: Header=BB0_11 Depth=2
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB0_37:                               # %for.body125.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	352(%rsp,%rcx,8), %rdx
	movq	recog_data+960(,%rcx,8), %rsi
	movq	%rdx, (%rsi)
	incq	%rcx
	movq	%rcx, %rdx
	andq	%r14, %rdx
	movq	%rax, %rsi
	andq	%r14, %rsi
	cmpq	%rsi, %rdx
	jne	.LBB0_37
.LBB0_38:                               # %for.cond133.preheader.i
                                        #   in Loop: Header=BB0_11 Depth=2
	xorq	%rax, %rax
	testq	%r12, %r12
	jle	.LBB0_40
	.align	16, 0x90
.LBB0_39:                               # %for.body136.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	384(%rsp,%rax,8), %rcx
	movq	recog_data+240(,%rax,8), %rdx
	movq	%rcx, (%rdx)
	incq	%rax
	movq	%rax, %rcx
	andq	%r14, %rcx
	movq	%r12, %rdx
	andq	%r14, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB0_39
.LBB0_40:                               # %for.end143.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movzbl	(%r15), %eax
	cmpq	$34, %rax
	jne	.LBB0_43
# BB#41:                                # %land.lhs.true148.i
                                        #   in Loop: Header=BB0_11 Depth=2
	cmpq	$0, 64(%r15)
	je	.LBB0_43
# BB#42:                                # %if.then153.i
                                        #   in Loop: Header=BB0_11 Depth=2
	leaq	64(%r15), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%r15, %rdi
	callq	scan_rtx
.LBB0_43:                               # %if.end157.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movq	(%rbx), %rdi
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	callq	asm_noperands
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%r12, %r12
	movabsq	$0, %rcx
	jg	.LBB0_45
# BB#44:                                # %if.end157.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movabsq	$1, %rcx
.LBB0_45:                               # %if.end157.i
                                        #   in Loop: Header=BB0_11 Depth=2
	testq	%rax, %rax
	jle	.LBB0_47
# BB#46:                                # %if.end157.i
                                        #   in Loop: Header=BB0_11 Depth=2
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	jne	.LBB0_47
	.align	16, 0x90
.LBB0_51:                               # %for.body168.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	recog_data+240(,%rbx,8), %rsi
	movq	(%rsi), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB0_55
# BB#52:                                # %land.lhs.true175.i
                                        #   in Loop: Header=BB0_51 Depth=3
	movl	8(%rax), %ecx
	movl	16(%rax), %eax
	cmpq	%rax, %rcx
	jne	.LBB0_55
# BB#53:                                # %land.lhs.true183.i
                                        #   in Loop: Header=BB0_51 Depth=3
	movl	recog_data+840(,%rbx,4), %eax
	orq	$2, %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	jne	.LBB0_55
# BB#54:                                # %if.then193.i
                                        #   in Loop: Header=BB0_51 Depth=3
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%r15, %rdi
	callq	scan_rtx
	.align	16, 0x90
.LBB0_55:                               # %for.inc195.i
                                        #   in Loop: Header=BB0_51 Depth=3
	incq	%rbx
	movq	%rbx, %rax
	andq	%r14, %rax
	movq	%r12, %rcx
	andq	%r14, %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_51
.LBB0_47:                               # %for.cond199.preheader.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movq	%r15, %r13
	movsbq	recog_data+997(%rip), %rax
	addq	%r12, %rax
	testq	%rax, %rax
	jle	.LBB0_62
# BB#48:                                # %for.body203.lr.ph.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movq	$1, %rbx
	movabsq	$recog_data+240, %r15
	.align	16, 0x90
.LBB0_49:                               # %for.body203.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1(%rbx), %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%r12, %rdx
	jge	.LBB0_56
# BB#50:                                #   in Loop: Header=BB0_49 Depth=3
	movq	%r15, %rdx
	jmp	.LBB0_57
	.align	16, 0x90
.LBB0_56:                               # %cond.false219.i
                                        #   in Loop: Header=BB0_49 Depth=3
	subq	%r12, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movsbq	recog_data+992(%rax), %rcx
	leaq	recog_data+960(,%rax,8), %rdx
	movq	%rcx, %rax
.LBB0_57:                               # %cond.end223.i
                                        #   in Loop: Header=BB0_49 Depth=3
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	recog_data+480(,%rax,8), %rcx
	movzbl	(%rcx), %esi
	movq	$5, %rcx
	movq	112(%rsp), %rdi         # 8-byte Reload
	shlq	%cl, %rdi
	testq	%rsi, %rsi
	je	.LBB0_61
# BB#58:                                # %if.end240.i
                                        #   in Loop: Header=BB0_49 Depth=3
	movq	(%rdx), %rsi
	imulq	$960, %rax, %r8         # imm = 0x3C0
	movq	$5, %rcx
	movq	112(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movzbl	recog_op_alt+24(%r8,%rdx), %ecx
	movl	recog_op_alt+8(%r8,%rdi), %edx
	testq	$64, %rcx
	jne	.LBB0_59
# BB#60:                                # %if.else.i
                                        #   in Loop: Header=BB0_49 Depth=3
	movl	recog_data+840(,%rax,4), %r8d
	movq	$4, %rcx
	xorq	%r9, %r9
	movq	%r13, %rdi
	callq	scan_rtx
	jmp	.LBB0_61
	.align	16, 0x90
.LBB0_59:                               # %if.then248.i
                                        #   in Loop: Header=BB0_49 Depth=3
	movq	$4, %rcx
	movq	%r13, %rdi
	callq	scan_rtx_address
.LBB0_61:                               # %for.inc250.i
                                        #   in Loop: Header=BB0_49 Depth=3
	movsbq	recog_data+997(%rip), %rax
	addq	%r12, %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	addq	$8, %r15
	cmpq	%rax, %rdx
	jl	.LBB0_49
.LBB0_62:                               # %for.end252.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movq	%r13, %r15
	movq	56(%r15), %rbx
	jmp	.LBB0_63
	.align	16, 0x90
.LBB0_69:                               # %for.inc280.i
                                        #   in Loop: Header=BB0_63 Depth=3
	movq	16(%rbx), %rbx
.LBB0_63:                               # %for.end252.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB0_70
# BB#64:                                # %for.body258.i
                                        #   in Loop: Header=BB0_63 Depth=3
	movzbl	2(%rbx), %eax
	cmpq	$2, %rax
	jne	.LBB0_65
# BB#67:                                # %if.then274.i
                                        #   in Loop: Header=BB0_63 Depth=3
	leaq	8(%rbx), %rsi
	movq	$24, %rdx
	movq	$4, %rcx
	movq	$2, %r8
	jmp	.LBB0_68
	.align	16, 0x90
.LBB0_65:                               # %for.body258.i
                                        #   in Loop: Header=BB0_63 Depth=3
	cmpq	$1, %rax
	jne	.LBB0_69
# BB#66:                                # %if.then264.i
                                        #   in Loop: Header=BB0_63 Depth=3
	leaq	8(%rbx), %rsi
	xorq	%rdx, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
.LBB0_68:                               # %for.inc280.i
                                        #   in Loop: Header=BB0_63 Depth=3
	xorq	%r9, %r9
	movq	%r15, %rdi
	callq	scan_rtx
	jmp	.LBB0_69
	.align	16, 0x90
.LBB0_70:                               # %for.end284.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movzbl	(%r15), %eax
	cmpq	$34, %rax
	movq	96(%rsp), %rbx          # 8-byte Reload
	jne	.LBB0_73
# BB#71:                                # %for.cond290.preheader.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movq	open_chains(%rip), %rax
	jmp	.LBB0_72
	.align	16, 0x90
.LBB0_208:                              # %for.body292.i
                                        #   in Loop: Header=BB0_72 Depth=3
	movzbl	36(%rax), %ecx
	orq	$1, %rcx
	movb	%cl, 36(%rax)
	movq	(%rax), %rax
.LBB0_72:                               # %for.cond290.preheader.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rax, %rax
	jne	.LBB0_208
.LBB0_73:                               # %for.cond298.preheader.i
                                        #   in Loop: Header=BB0_11 Depth=2
	xorq	%rax, %rax
	testq	%r12, %r12
	jle	.LBB0_74
	.align	16, 0x90
.LBB0_92:                               # %for.body301.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	recog_data(,%rax,8), %rcx
	movl	recog_data+840(,%rax,4), %edx
	movq	%rcx, 384(%rsp,%rax,8)
	cmpq	$2, %rdx
	jne	.LBB0_94
# BB#93:                                # %if.then310.i
                                        #   in Loop: Header=BB0_92 Depth=3
	movq	global_rtl+8(%rip), %rcx
	movq	recog_data+240(,%rax,8), %rdx
	movq	%rcx, (%rdx)
.LBB0_94:                               # %for.inc314.i
                                        #   in Loop: Header=BB0_92 Depth=3
	incq	%rax
	movq	%rax, %rcx
	andq	%r14, %rcx
	movq	%r12, %rdx
	andq	%r14, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB0_92
.LBB0_74:                               # %for.cond317.preheader.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movsbq	recog_data+997(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_79
# BB#75:                                # %for.body321.i.preheader
                                        #   in Loop: Header=BB0_11 Depth=2
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB0_76:                               # %for.body321.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	recog_data+960(,%rcx,8), %rdx
	movsbq	recog_data+992(%rcx), %rsi
	movq	(%rdx), %rdi
	movl	recog_data+840(,%rsi,4), %esi
	movq	%rdi, 352(%rsp,%rcx,8)
	cmpq	$2, %rsi
	jne	.LBB0_78
# BB#77:                                # %if.then334.i
                                        #   in Loop: Header=BB0_76 Depth=3
	movq	global_rtl+8(%rip), %rsi
	movq	%rsi, (%rdx)
.LBB0_78:                               # %for.inc338.i
                                        #   in Loop: Header=BB0_76 Depth=3
	incq	%rcx
	movq	%rcx, %rdx
	andq	%r14, %rdx
	movq	%rax, %rsi
	andq	%r14, %rsi
	cmpq	%rsi, %rdx
	jne	.LBB0_76
.LBB0_79:                               # %for.end340.i
                                        #   in Loop: Header=BB0_11 Depth=2
	xorq	%rdx, %rdx
	movq	$2, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	scan_rtx
	movsbq	recog_data+997(%rip), %rax
	testq	%rax, %rax
	movq	%r14, %rcx
	movabsq	$4294967288, %r13       # imm = 0xFFFFFFF8
	movq	%r13, %r14
	movq	%rcx, %rdi
	jle	.LBB0_82
# BB#80:                                # %for.body348.i.preheader
                                        #   in Loop: Header=BB0_11 Depth=2
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB0_81:                               # %for.body348.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	352(%rsp,%rcx,8), %rdx
	movq	recog_data+960(,%rcx,8), %rsi
	movq	%rdx, (%rsi)
	incq	%rcx
	movq	%rcx, %rdx
	andq	%rdi, %rdx
	movq	%rax, %rsi
	andq	%rdi, %rsi
	cmpq	%rsi, %rdx
	jne	.LBB0_81
.LBB0_82:                               # %for.cond356.preheader.i
                                        #   in Loop: Header=BB0_11 Depth=2
	xorq	%rax, %rax
	testq	%r12, %r12
	jle	.LBB0_84
	.align	16, 0x90
.LBB0_83:                               # %for.body359.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	384(%rsp,%rax,8), %rcx
	movq	recog_data+240(,%rax,8), %rdx
	movq	%rcx, (%rdx)
	incq	%rax
	movq	%rax, %rcx
	andq	%rdi, %rcx
	movq	%r12, %rdx
	andq	%rdi, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB0_83
.LBB0_84:                               # %for.end366.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movq	(%rbx), %rdi
	callq	asm_noperands
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_95
# BB#85:                                # %for.cond374.preheader.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movq	%r15, %r13
	testq	%r12, %r12
	jle	.LBB0_104
# BB#86:                                # %for.body377.lr.ph.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movq	$5, %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	leaq	recog_op_alt+24(%rax), %r15
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB0_87:                               # %for.body377.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	recog_data+840(,%rbx,4), %eax
	cmpq	$1, %rax
	jne	.LBB0_91
# BB#88:                                # %if.then382.i
                                        #   in Loop: Header=BB0_87 Depth=3
	movq	recog_data+240(,%rbx,8), %rsi
	movq	(%rsi), %rax
	movzbl	(%rax), %ecx
	movl	-16(%r15), %edx
	cmpq	$61, %rcx
	jne	.LBB0_90
# BB#89:                                # %land.lhs.true397.i
                                        #   in Loop: Header=BB0_87 Depth=3
	movl	8(%rax), %ecx
	movl	16(%rax), %eax
	cmpq	%rax, %rcx
	je	.LBB0_91
.LBB0_90:                               # %if.end407.i
                                        #   in Loop: Header=BB0_87 Depth=3
	movzbl	(%r15), %r9d
	andq	$1, %r9
	movq	$5, %rcx
	movq	$1, %r8
	movq	%r13, %rdi
	callq	scan_rtx
.LBB0_91:                               # %for.inc416.i
                                        #   in Loop: Header=BB0_87 Depth=3
	incq	%rbx
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	andq	%rdx, %rax
	movq	%r12, %rcx
	andq	%rdx, %rcx
	addq	$960, %r15              # imm = 0x3C0
	cmpq	%rcx, %rax
	jne	.LBB0_87
	jmp	.LBB0_104
	.align	16, 0x90
.LBB0_95:                               # %if.else419.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movzbl	(%r15), %eax
	movq	%r15, %r13
	cmpq	$34, %rax
	je	.LBB0_104
# BB#96:                                # %for.cond425.preheader.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movsbq	recog_data+997(%rip), %rax
	leaq	(%rax,%r12), %rcx
	testq	%rcx, %rcx
	jle	.LBB0_104
# BB#97:                                # %for.body430.lr.ph.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movq	$1, %rbx
	movabsq	$recog_data+240, %r15
	.align	16, 0x90
.LBB0_98:                               # %for.body430.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1(%rbx), %rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%r12, %rsi
	jge	.LBB0_100
# BB#99:                                #   in Loop: Header=BB0_98 Depth=3
	movq	%r15, %rsi
	jmp	.LBB0_101
	.align	16, 0x90
.LBB0_100:                              # %cond.false448.i
                                        #   in Loop: Header=BB0_98 Depth=3
	subq	%r12, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movsbq	recog_data+992(%rdx), %rcx
	leaq	recog_data+960(,%rdx,8), %rsi
	movq	%rcx, %rdx
.LBB0_101:                              # %cond.end452.i
                                        #   in Loop: Header=BB0_98 Depth=3
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	recog_data+840(,%rdx,4), %ecx
	cmpq	$1, %rcx
	jne	.LBB0_103
# BB#102:                               # %if.then464.i
                                        #   in Loop: Header=BB0_98 Depth=3
	movq	$5, %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	imulq	$960, %rdx, %rcx        # imm = 0x3C0
	movq	(%rsi), %rsi
	movzbl	recog_op_alt+24(%rcx,%rax), %r9d
	movl	recog_op_alt+8(%rcx,%rax), %edx
	andq	$1, %r9
	movq	$5, %rcx
	movq	$1, %r8
	movq	%r13, %rdi
	callq	scan_rtx
	movzbl	recog_data+997(%rip), %eax
.LBB0_103:                              # %for.inc474.i
                                        #   in Loop: Header=BB0_98 Depth=3
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	addq	%r12, %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	addq	$8, %r15
	cmpq	%rdx, %rsi
	jl	.LBB0_98
	.align	16, 0x90
.LBB0_104:                              # %for.cond482.preheader.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movq	%r13, %r15
	movq	56(%r15), %rbx
	testq	%rbx, %rbx
	movabsq	$.L.str13, %r12
	je	.LBB0_108
	.align	16, 0x90
.LBB0_105:                              # %for.body484.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$655360, %rax           # imm = 0xA0000
	jne	.LBB0_107
# BB#106:                               # %if.then490.i
                                        #   in Loop: Header=BB0_105 Depth=3
	leaq	8(%rbx), %rsi
	xorq	%rdx, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%r15, %rdi
	callq	scan_rtx
.LBB0_107:                              # %for.inc495.i
                                        #   in Loop: Header=BB0_105 Depth=3
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_105
.LBB0_108:                              # %if.end500.i
                                        #   in Loop: Header=BB0_11 Depth=2
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	cmpq	%rax, %r15
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jne	.LBB0_109
# BB#110:                               # %build_def_use.exit
                                        #   in Loop: Header=BB0_8 Depth=1
	movq	closed_chains(%rip), %rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	cmpq	$0, rtl_dump_file(%rip)
	je	.LBB0_128
# BB#111:                               # %build_def_use.exit
                                        #   in Loop: Header=BB0_8 Depth=1
	movq	80(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	movq	%rax, %r15
	je	.LBB0_128
	.align	16, 0x90
.LBB0_112:                              # %while.body.i
                                        #   Parent Loop BB0_8 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_126 Depth 3
	movq	24(%r15), %rax
	movq	(%rax), %rax
	movl	8(%rax), %edx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	movzbl	2(%rax), %eax
	sarq	%cl, %rsi
	movq	%rdx, %rcx
	andq	%r14, %rcx
	cmpq	$8, %rcx
	je	.LBB0_116
# BB#113:                               # %while.body.i
                                        #   in Loop: Header=BB0_112 Depth=2
	leaq	-21(%rdx), %rcx
	andq	%r13, %rcx
	cmpq	$8, %rcx
	jb	.LBB0_116
# BB#114:                               # %while.body.i
                                        #   in Loop: Header=BB0_112 Depth=2
	leaq	-45(%rdx), %rcx
	andq	%r13, %rcx
	cmpq	$8, %rcx
	jb	.LBB0_116
# BB#115:                               # %while.body.i
                                        #   in Loop: Header=BB0_112 Depth=2
	addq	$-29, %rdx
	andq	%r13, %rdx
	cmpq	$7, %rdx
	jbe	.LBB0_116
# BB#120:                               # %cond.false.i561
                                        #   in Loop: Header=BB0_112 Depth=2
	cmpq	$24, %rax
	jne	.LBB0_121
# BB#123:                               # %cond.true37.i
                                        #   in Loop: Header=BB0_112 Depth=2
	movzbl	target_flags+3(%rip), %ebx
	andq	$2, %rbx
	xorq	$6, %rbx
	jmp	.LBB0_125
	.align	16, 0x90
.LBB0_116:                              # %cond.true.i
                                        #   in Loop: Header=BB0_112 Depth=2
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %rbx
	je	.LBB0_125
# BB#117:                               # %lor.rhs.i
                                        #   in Loop: Header=BB0_112 Depth=2
	cmpq	$6, %rax
	movabsq	$0, %rbx
	jne	.LBB0_119
# BB#118:                               # %lor.rhs.i
                                        #   in Loop: Header=BB0_112 Depth=2
	movabsq	$1, %rbx
.LBB0_119:                              # %lor.rhs.i
                                        #   in Loop: Header=BB0_112 Depth=2
	incq	%rbx
	jmp	.LBB0_125
	.align	16, 0x90
.LBB0_121:                              # %cond.false.i561
                                        #   in Loop: Header=BB0_112 Depth=2
	cmpq	$18, %rax
	jne	.LBB0_124
# BB#122:                               # %cond.true28.i
                                        #   in Loop: Header=BB0_112 Depth=2
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	xorq	$3, %rbx
	jmp	.LBB0_125
.LBB0_124:                              # %cond.false41.i
                                        #   in Loop: Header=BB0_112 Depth=2
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	movq	%rax, %rbx
	.align	16, 0x90
.LBB0_125:                              # %cond.end57.i
                                        #   in Loop: Header=BB0_112 Depth=2
	movq	rtl_dump_file(%rip), %rdi
	movq	reg_names(,%rsi,8), %rdx
	xorq	%rax, %rax
	movabsq	$.L.str12, %rsi
	movq	%rbx, %rcx
	callq	fprintf
	movq	rtl_dump_file(%rip), %rsi
	testq	%r15, %r15
	movq	%r15, %rbx
	je	.LBB0_127
	.align	16, 0x90
.LBB0_126:                              # %while.body63.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_112 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	16(%rbx), %rax
	movl	32(%rbx), %ecx
	movq	reg_class_names(,%rcx,8), %rcx
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movq	%rsi, %rdi
	movq	%r12, %rsi
	callq	fprintf
	movq	8(%rbx), %rbx
	movq	rtl_dump_file(%rip), %rsi
	testq	%rbx, %rbx
	jne	.LBB0_126
.LBB0_127:                              # %while.end.i
                                        #   in Loop: Header=BB0_112 Depth=2
	movq	$10, %rdi
	callq	fputc
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB0_112
.LBB0_128:                              # %if.end48
                                        #   in Loop: Header=BB0_8 Depth=1
	movl	frame_pointer_needed(%rip), %eax
	xorq	%r11, %r11
	testq	%rax, %rax
	movq	72(%rsp), %r15          # 8-byte Reload
	jne	.LBB0_130
# BB#129:                               #   in Loop: Header=BB0_8 Depth=1
	movq	80(%rsp), %r8           # 8-byte Reload
	jmp	.LBB0_136
	.align	16, 0x90
.LBB0_130:                              # %if.then50
                                        #   in Loop: Header=BB0_8 Depth=1
	movl	target_flags(%rip), %esi
	andq	$33554432, %rsi         # imm = 0x2000000
	movq	$25, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movzbl	mode_size+4(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	testq	%r13, %rax
	movq	80(%rsp), %r8           # 8-byte Reload
	je	.LBB0_136
# BB#131:                               # %for.body87.lr.ph
                                        #   in Loop: Header=BB0_8 Depth=1
	movq	%rax, %rdx
	andq	%r13, %rdx
	leaq	19(%rdx), %rcx
	xorq	%r11, %r11
	movq	%rax, %rsi
	.align	16, 0x90
.LBB0_132:                              # %for.body87
                                        #   Parent Loop BB0_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r11, %rdi
	decq	%rsi
	movq	$1, %r11
	shlq	%cl, %r11
	orq	%rdi, %r11
	decq	%rcx
	testq	%r13, %rsi
	jne	.LBB0_132
# BB#133:                               # %for.end
                                        #   in Loop: Header=BB0_8 Depth=1
	testq	%r13, %rax
	je	.LBB0_136
# BB#134:                               # %for.body131.lr.ph
                                        #   in Loop: Header=BB0_8 Depth=1
	addq	$5, %rdx
	.align	16, 0x90
.LBB0_135:                              # %for.body131
                                        #   Parent Loop BB0_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%rax
	movq	$1, %rsi
	movq	%rdx, %rcx
	shlq	%cl, %rsi
	orq	%rsi, %r11
	decq	%rdx
	testq	%r13, %rax
	jne	.LBB0_135
	.align	16, 0x90
.LBB0_136:                              # %if.end137
                                        #   in Loop: Header=BB0_8 Depth=1
	movq	%r11, 64(%rsp)          # 8-byte Spill
	jmp	.LBB0_137
.LBB0_201:                              #   in Loop: Header=BB0_137 Depth=2
	movq	72(%rsp), %r15          # 8-byte Reload
	movq	80(%rsp), %r8           # 8-byte Reload
	movq	64(%rsp), %r11          # 8-byte Reload
	movabsq	$4294967288, %r14       # imm = 0xFFFFFFF8
	.align	16, 0x90
.LBB0_137:                              # %if.end137
                                        #   Parent Loop BB0_8 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_143 Depth 3
                                        #       Child Loop BB0_148 Depth 3
                                        #         Child Loop BB0_153 Depth 4
                                        #       Child Loop BB0_161 Depth 3
                                        #         Child Loop BB0_175 Depth 4
                                        #         Child Loop BB0_183 Depth 4
                                        #       Child Loop BB0_197 Depth 3
	testq	%r8, %r8
	je	.LBB0_150
# BB#138:                               # %while.body
                                        #   in Loop: Header=BB0_137 Depth=2
	movq	%r8, %rcx
	movq	24(%rcx), %rax
	movq	(%rax), %rax
	movslq	8(%rax), %r10
	movzbl	fixed_regs(%r10), %eax
	movq	(%rcx), %r8
	testq	%rax, %rax
	jne	.LBB0_137
# BB#139:                               # %lor.lhs.false
                                        #   in Loop: Header=BB0_137 Depth=2
	movzbl	global_regs(%r10), %eax
	testq	%rax, %rax
	jne	.LBB0_137
# BB#140:                               # %lor.lhs.false159
                                        #   in Loop: Header=BB0_137 Depth=2
	movl	frame_pointer_needed(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_142
# BB#141:                               # %lor.lhs.false159
                                        #   in Loop: Header=BB0_137 Depth=2
	movq	%r10, %rax
	andq	%r13, %rax
	cmpq	$6, %rax
	je	.LBB0_137
.LBB0_142:                              # %if.end164
                                        #   in Loop: Header=BB0_137 Depth=2
	movq	%r11, 120(%rsp)
	movq	%rcx, %rdx
	movq	8(%rdx), %rcx
	xorq	%rsi, %rsi
	testq	%rcx, %rcx
	movq	%r11, %rax
	movq	%rdx, %rbx
	movq	%rdx, %r9
	je	.LBB0_137
	.align	16, 0x90
.LBB0_143:                              # %for.body167
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_137 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rbx, %rdx
	movl	32(%rdx), %edx
	movq	%rax, %rdi
	movq	reg_class_contents(,%rdx,8), %rax
	movq	%rsi, %rdx
	movq	%rcx, %rbx
	movq	8(%rbx), %rcx
	notq	%rax
	orq	%rdi, %rax
	leaq	1(%rdx), %rsi
	testq	%rcx, %rcx
	jne	.LBB0_143
# BB#144:                               # %for.end173
                                        #   in Loop: Header=BB0_137 Depth=2
	movq	%rax, 120(%rsp)
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB0_137
# BB#145:                               # %if.end177
                                        #   in Loop: Header=BB0_137 Depth=2
	movq	%r10, 88(%rsp)          # 8-byte Spill
	movq	%r8, 80(%rsp)           # 8-byte Spill
	movl	32(%rbx), %ecx
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	reg_class_contents(,%rcx,8), %rcx
	notq	%rcx
	orq	%rcx, %rax
	movq	%rax, 120(%rsp)
	movq	%r9, %r14
	movzbl	36(%r14), %ecx
	testq	$1, %rcx
	je	.LBB0_147
# BB#146:                               # %if.then186
                                        #   in Loop: Header=BB0_137 Depth=2
	movq	call_used_reg_set(%rip), %rcx
	orq	%rax, %rcx
	movq	%rcx, 120(%rsp)
.LBB0_147:                              # %while.cond1.preheader.lr.ph.i
                                        #   in Loop: Header=BB0_137 Depth=2
	movq	$0, 632(%rsp)
	movq	104(%rsp), %rbx         # 8-byte Reload
	movq	64(%rbx), %rsi
	leaq	632(%rsp), %rdi
	callq	reg_set_to_hard_reg_set
	movq	(%rbx), %r12
	movq	%r14, %rax
	.align	16, 0x90
.LBB0_148:                              # %while.cond1.preheader.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_137 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_153 Depth 4
	movq	%rax, %r13
	movq	16(%r13), %r15
	cmpq	%r15, %r12
	jne	.LBB0_153
# BB#149:                               #   in Loop: Header=BB0_148 Depth=3
	movq	%r12, %r15
	jmp	.LBB0_158
	.align	16, 0x90
.LBB0_153:                              # %while.body3.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_137 Depth=2
                                        #       Parent Loop BB0_148 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_157
# BB#154:                               # %if.then.i573
                                        #   in Loop: Header=BB0_153 Depth=4
	movq	56(%r12), %rdx
	movq	$1, %rsi
	leaq	632(%rsp), %rbx
	movq	%rbx, %rdi
	callq	clear_dead_regs
	movq	32(%r12), %rdi
	movabsq	$note_sets, %rsi
	movq	%rbx, %rdx
	callq	note_stores
	cmpq	%r14, %r13
	je	.LBB0_156
# BB#155:                               # %if.then12.i
                                        #   in Loop: Header=BB0_153 Depth=4
	movq	632(%rsp), %rax
	orq	%rax, 120(%rsp)
.LBB0_156:                              # %if.end.i
                                        #   in Loop: Header=BB0_153 Depth=4
	movq	56(%r12), %rdx
	movq	$10, %rsi
	leaq	632(%rsp), %rdi
	callq	clear_dead_regs
	movq	16(%r13), %r15
.LBB0_157:                              # %if.end16.i
                                        #   in Loop: Header=BB0_153 Depth=4
	movq	24(%r12), %r12
	cmpq	%r15, %r12
	jne	.LBB0_153
.LBB0_158:                              # %while.end.i576
                                        #   in Loop: Header=BB0_148 Depth=3
	movq	632(%rsp), %rax
	orq	%rax, 120(%rsp)
	movq	8(%r13), %rax
	testq	%rax, %rax
	movq	%r15, %r12
	jne	.LBB0_148
# BB#159:                               # %if.end26.i
                                        #   in Loop: Header=BB0_148 Depth=3
	movq	32(%r15), %rdi
	movabsq	$note_sets, %rsi
	leaq	120(%rsp), %rdx
	callq	note_stores
	movq	8(%r13), %rax
	testq	%rax, %rax
	movq	%r15, %r12
	jne	.LBB0_148
# BB#160:                               # %for.cond189.preheader
                                        #   in Loop: Header=BB0_137 Depth=2
	movq	%r14, 112(%rsp)         # 8-byte Spill
	xorq	%r15, %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movabsq	$4294967288, %r13       # imm = 0xFFFFFFF8
	movq	%r13, %r14
	movabsq	$4294967275, %r8        # imm = 0xFFFFFFEB
	movabsq	$4294967251, %r9        # imm = 0xFFFFFFD3
	movabsq	$4294967267, %r10       # imm = 0xFFFFFFE3
	movq	88(%rsp), %r11          # 8-byte Reload
	.align	16, 0x90
.LBB0_161:                              # %for.body192
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_137 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_175 Depth 4
                                        #         Child Loop BB0_183 Depth 4
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	24(%rax), %rax
	movq	(%rax), %rax
	movl	(%rax), %esi
	movq	%r15, %rdx
	andq	%r14, %rdx
	movq	$16, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	je	.LBB0_165
# BB#162:                               # %for.body192
                                        #   in Loop: Header=BB0_161 Depth=3
	leaq	(%r15,%r8), %rcx
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	jb	.LBB0_165
# BB#163:                               # %for.body192
                                        #   in Loop: Header=BB0_161 Depth=3
	leaq	(%r15,%r9), %rcx
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	jb	.LBB0_165
# BB#164:                               # %for.body192
                                        #   in Loop: Header=BB0_161 Depth=3
	leaq	(%r15,%r10), %rcx
	andq	%rdx, %rcx
	cmpq	$7, %rcx
	jbe	.LBB0_165
# BB#169:                               # %cond.false234
                                        #   in Loop: Header=BB0_161 Depth=3
	cmpq	$24, %rax
	jne	.LBB0_170
# BB#172:                               # %cond.true252
                                        #   in Loop: Header=BB0_161 Depth=3
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB0_174
	.align	16, 0x90
.LBB0_165:                              # %cond.true217
                                        #   in Loop: Header=BB0_161 Depth=3
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB0_174
# BB#166:                               # %lor.rhs
                                        #   in Loop: Header=BB0_161 Depth=3
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB0_168
# BB#167:                               # %lor.rhs
                                        #   in Loop: Header=BB0_161 Depth=3
	movabsq	$1, %rax
.LBB0_168:                              # %lor.rhs
                                        #   in Loop: Header=BB0_161 Depth=3
	incq	%rax
	jmp	.LBB0_174
	.align	16, 0x90
.LBB0_170:                              # %cond.false234
                                        #   in Loop: Header=BB0_161 Depth=3
	cmpq	$18, %rax
	jne	.LBB0_173
# BB#171:                               # %cond.true241
                                        #   in Loop: Header=BB0_161 Depth=3
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB0_174
.LBB0_173:                              # %cond.false256
                                        #   in Loop: Header=BB0_161 Depth=3
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	.align	16, 0x90
.LBB0_174:                              # %for.cond280.preheader
                                        #   in Loop: Header=BB0_161 Depth=3
	movq	120(%rsp), %rdi
	.align	16, 0x90
.LBB0_175:                              # %for.cond280
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_137 Depth=2
                                        #       Parent Loop BB0_161 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB0_182
# BB#176:                               # %for.body283
                                        #   in Loop: Header=BB0_175 Depth=4
	decq	%rax
	leaq	(%rax,%r15), %rdx
	movq	$1, %rbx
	movq	%rdx, %rcx
	shlq	%cl, %rbx
	testq	%rbx, %rdi
	jne	.LBB0_190
# BB#177:                               # %lor.lhs.false289
                                        #   in Loop: Header=BB0_175 Depth=4
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	fixed_regs(%rdx), %ecx
	testq	%rcx, %rcx
	jne	.LBB0_190
# BB#178:                               # %lor.lhs.false295
                                        #   in Loop: Header=BB0_175 Depth=4
	movzbl	global_regs(%rdx), %ecx
	testq	%rcx, %rcx
	jne	.LBB0_190
# BB#179:                               # %lor.lhs.false301
                                        #   in Loop: Header=BB0_175 Depth=4
	movzbl	regs_ever_live(%rdx), %ecx
	testq	%rcx, %rcx
	jne	.LBB0_181
# BB#180:                               # %land.lhs.true306
                                        #   in Loop: Header=BB0_175 Depth=4
	movzbl	call_used_regs(%rdx), %ecx
	testq	%rcx, %rcx
	je	.LBB0_190
.LBB0_181:                              # %lor.lhs.false311
                                        #   in Loop: Header=BB0_175 Depth=4
	leaq	(%rax,%r11), %rcx
	andq	%r14, %rcx
	cmpq	$8, %rcx
	jne	.LBB0_175
	jmp	.LBB0_190
.LBB0_182:                              # %for.cond328.preheader
                                        #   in Loop: Header=BB0_161 Depth=3
	movq	%r8, %r12
	movq	112(%rsp), %rax         # 8-byte Reload
	testq	%rax, %rax
	movq	%rax, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	je	.LBB0_187
	jmp	.LBB0_183
	.align	16, 0x90
.LBB0_186:                              # %lor.lhs.false337.for.body330_crit_edge
                                        #   in Loop: Header=BB0_183 Depth=4
	movq	24(%rbx), %rax
	movq	(%rax), %rax
	movl	(%rax), %esi
.LBB0_183:                              # %for.body330
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_137 Depth=2
                                        #       Parent Loop BB0_161 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%r15, %rdi
	callq	ix86_hard_regno_mode_ok
	testq	%r13, %rax
	je	.LBB0_184
# BB#185:                               # %lor.lhs.false337
                                        #   in Loop: Header=BB0_183 Depth=4
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_186
.LBB0_187:                              # %if.then350
                                        #   in Loop: Header=BB0_161 Depth=3
	movq	96(%rsp), %rax          # 8-byte Reload
	andq	%r13, %rax
	cmpq	%r13, %rax
	movq	%r12, %r8
	movabsq	$4294967251, %r9        # imm = 0xFFFFFFD3
	movabsq	$4294967267, %r10       # imm = 0xFFFFFFE3
	movq	88(%rsp), %r11          # 8-byte Reload
	je	.LBB0_189
# BB#188:                               # %lor.lhs.false353
                                        #   in Loop: Header=BB0_161 Depth=3
	movq	$32, %rcx
	movq	96(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movslq	128(%rsp,%rax,4), %rax
	movslq	128(%rsp,%r15,4), %rcx
	cmpq	%rcx, %rax
	jle	.LBB0_190
.LBB0_189:                              # %if.then360
                                        #   in Loop: Header=BB0_161 Depth=3
	movq	%r15, 96(%rsp)          # 8-byte Spill
	jmp	.LBB0_190
.LBB0_184:                              #   in Loop: Header=BB0_161 Depth=3
	movq	%r12, %r8
	movabsq	$4294967251, %r9        # imm = 0xFFFFFFD3
	movabsq	$4294967267, %r10       # imm = 0xFFFFFFE3
	movq	88(%rsp), %r11          # 8-byte Reload
	.align	16, 0x90
.LBB0_190:                              # %for.inc363
                                        #   in Loop: Header=BB0_161 Depth=3
	incq	%r15
	cmpq	$53, %r15
	jne	.LBB0_161
# BB#191:                               # %for.end365
                                        #   in Loop: Header=BB0_137 Depth=2
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	movabsq	$.L.str13, %r12
	je	.LBB0_194
# BB#192:                               # %if.then367
                                        #   in Loop: Header=BB0_137 Depth=2
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	16(%rbx), %rax
	movq	reg_names(,%r11,8), %rdx
	movl	8(%rax), %ecx
	xorq	%rax, %rax
	movabsq	$.L.str1, %rsi
	callq	fprintf
	movzbl	36(%rbx), %eax
	testq	$1, %rax
	je	.LBB0_194
# BB#193:                               # %if.then378
                                        #   in Loop: Header=BB0_137 Depth=2
	movq	rtl_dump_file(%rip), %rcx
	movq	$15, %rsi
	movq	$1, %rdx
	movabsq	$.L.str2, %rdi
	callq	fwrite
.LBB0_194:                              # %if.end381
                                        #   in Loop: Header=BB0_137 Depth=2
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %r13
	andq	%r13, %rax
	cmpq	%r13, %rax
	movq	112(%rsp), %rbx         # 8-byte Reload
	jne	.LBB0_197
# BB#195:                               # %if.then384
                                        #   in Loop: Header=BB0_137 Depth=2
	movq	rtl_dump_file(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB0_201
# BB#196:                               # %if.then386
                                        #   in Loop: Header=BB0_137 Depth=2
	movq	$25, %rsi
	movq	$1, %rdx
	movabsq	$.L.str3, %rdi
	callq	fwrite
	jmp	.LBB0_201
	.align	16, 0x90
.LBB0_197:                              # %while.body.i587
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_137 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	24(%rbx), %rax
	movq	(%rax), %rax
	movl	16(%rax), %r15d
	movzbl	2(%rax), %edi
	movq	%rsi, %r14
	callq	gen_raw_REG
	movq	24(%rbx), %rcx
	cmpq	$53, %r15
	movq	%rax, (%rcx)
	jb	.LBB0_199
# BB#198:                               # %if.then.i588
                                        #   in Loop: Header=BB0_197 Depth=3
	movq	24(%rbx), %rax
	movq	(%rax), %rax
	movl	%r15d, 16(%rax)
.LBB0_199:                              # %if.end.i591
                                        #   in Loop: Header=BB0_197 Depth=3
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	movq	%r14, %rsi
	jne	.LBB0_197
# BB#200:                               # %do_replace.exit
                                        #   in Loop: Header=BB0_137 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	movq	rtl_dump_file(%rip), %rdi
	sarq	%cl, %rsi
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	%eax, 128(%rsp,%rsi,4)
	incq	%rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rdi, %rdi
	je	.LBB0_201
# BB#202:                               # %if.then394
                                        #   in Loop: Header=BB0_137 Depth=2
	movq	reg_names(,%rsi,8), %rdx
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	callq	fprintf
	jmp	.LBB0_201
	.align	16, 0x90
.LBB0_150:                              # %while.end
                                        #   in Loop: Header=BB0_8 Depth=1
	movq	rename_obstack+8(%rip), %rax
	movq	56(%rsp), %rsi          # 8-byte Reload
	cmpq	%rax, %rsi
	jbe	.LBB0_203
# BB#151:                               # %while.end
                                        #   in Loop: Header=BB0_8 Depth=1
	movq	rename_obstack+32(%rip), %rax
	cmpq	%rax, %rsi
	jae	.LBB0_203
# BB#152:                               # %if.then409
                                        #   in Loop: Header=BB0_8 Depth=1
	movq	%rsi, rename_obstack+16(%rip)
	movq	%rsi, rename_obstack+24(%rip)
	jmp	.LBB0_204
	.align	16, 0x90
.LBB0_203:                              # %if.else
                                        #   in Loop: Header=BB0_8 Depth=1
	movabsq	$rename_obstack, %rdi
	callq	obstack_free
.LBB0_204:                              # %for.inc413
                                        #   in Loop: Header=BB0_8 Depth=1
	incq	%r15
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB0_8
.LBB0_205:                              # %if.else430
	movabsq	$rename_obstack, %rdi
	xorq	%rsi, %rsi
	callq	obstack_free
	movq	rtl_dump_file(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB0_207
# BB#206:                               # %if.then433
	movq	$10, %rdi
	callq	fputc
.LBB0_207:                              # %if.end435
	xorq	%rdi, %rdi
	movq	$1, %rsi
	callq	count_or_remove_death_notes
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$5, %rdx
	callq	update_life_info
	addq	$640, %rsp              # imm = 0x280
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	regrename_optimize, .Ltmp17-regrename_optimize
	.cfi_endproc

	.globl	copyprop_hardreg_forward
	.align	16, 0x90
	.type	copyprop_hardreg_forward,@function
copyprop_hardreg_forward:               # @copyprop_hardreg_forward
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
	subq	$160, %rsp
.Ltmp29:
	.cfi_def_cfa_offset 208
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
	movslq	n_basic_blocks(%rip), %rax
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,4), %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rcx
	movq	%rax, %r14
	testq	%rcx, %rcx
	jle	.LBB1_184
# BB#1:                                 # %for.body.preheader
	leaq	8(%r14), %rcx
	xorq	%r15, %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	xorq	%r13, %r13
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB1_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_8 Depth 2
                                        #     Child Loop BB1_11 Depth 2
                                        #       Child Loop BB1_18 Depth 3
                                        #       Child Loop BB1_26 Depth 3
                                        #       Child Loop BB1_30 Depth 3
                                        #       Child Loop BB1_84 Depth 3
                                        #       Child Loop BB1_62 Depth 3
                                        #         Child Loop BB1_97 Depth 4
                                        #       Child Loop BB1_103 Depth 3
                                        #         Child Loop BB1_105 Depth 4
                                        #         Child Loop BB1_108 Depth 4
                                        #       Child Loop BB1_172 Depth 3
	movq	%r13, 64(%rsp)          # 8-byte Spill
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r13,8), %rbx
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	movq	32(%rbx), %rax
	testq	%rax, %rax
	movabsq	$0, %rdx
	je	.LBB1_8
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB1_2 Depth=1
	cmpq	$0, (%rax)
	movq	32(%rsp), %rcx          # 8-byte Reload
	movabsq	$0, %rdx
	jne	.LBB1_8
# BB#4:                                 # %land.lhs.true5
                                        #   in Loop: Header=BB1_2 Depth=1
	movzbl	48(%rax), %ecx
	testq	$12, %rcx
	movq	32(%rsp), %rcx          # 8-byte Reload
	movabsq	$0, %rdx
	jne	.LBB1_8
# BB#5:                                 # %land.lhs.true8
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	16(%rax), %rax
	movl	88(%rax), %esi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r12, %rsi
	movq	32(%rsp), %rcx          # 8-byte Reload
	movabsq	$0, %rdx
	je	.LBB1_8
# BB#6:                                 # %land.lhs.true8
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	movq	32(%rsp), %rcx          # 8-byte Reload
	movabsq	$0, %rdx
	jge	.LBB1_8
# BB#7:                                 # %if.then
                                        #   in Loop: Header=BB1_2 Depth=1
	leaq	(%r13,%r13,4), %rdx
	movq	$7, %rcx
	shlq	%cl, %rdx
	movq	%rdx, 152(%rsp)         # 8-byte Spill
	leaq	(%r14,%rdx), %rdi
	movq	%rdi, 144(%rsp)         # 8-byte Spill
	leaq	(%rax,%rax,4), %rsi
	movq	$7, %rcx
	shlq	%cl, %rsi
	addq	%r14, %rsi
	movq	$640, %rdx              # imm = 0x280
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	callq	memcpy
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	152(%rsp), %rax         # 8-byte Reload
	leaq	636(%r14,%rax), %rdx
	jmp	.LBB1_10
	.align	16, 0x90
.LBB1_8:                                # %for.body.i
                                        #   Parent Loop BB1_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	%r15d, -8(%rcx)
	movl	%edx, -4(%rcx)
	movl	%r12d, (%rcx)
	incq	%rdx
	addq	$12, %rcx
	cmpq	$53, %rdx
	jne	.LBB1_8
# BB#9:                                 # %init_value_data.exit
                                        #   in Loop: Header=BB1_2 Depth=1
	leaq	(%r13,%r13,4), %rax
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	636(%r14,%rax), %rdx
	leaq	(%r14,%rax), %rcx
	movq	%rcx, 144(%rsp)         # 8-byte Spill
	movl	%r15d, 636(%r14,%rax)
.LBB1_10:                               # %if.end
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	$0, 96(%rsp)            # 8-byte Folded Spill
	movq	%rbx, %rcx
	jmp	.LBB1_11
	.align	16, 0x90
.LBB1_176:                              # %for.inc570.i
                                        #   in Loop: Header=BB1_11 Depth=2
	addq	$24, %rcx
.LBB1_11:                               # %for.cond.i
                                        #   Parent Loop BB1_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB1_18 Depth 3
                                        #       Child Loop BB1_26 Depth 3
                                        #       Child Loop BB1_30 Depth 3
                                        #       Child Loop BB1_84 Depth 3
                                        #       Child Loop BB1_62 Depth 3
                                        #         Child Loop BB1_97 Depth 4
                                        #       Child Loop BB1_103 Depth 3
                                        #         Child Loop BB1_105 Depth 4
                                        #         Child Loop BB1_108 Depth 4
                                        #       Child Loop BB1_172 Depth 3
	movq	(%rcx), %rcx
	movzbl	(%rcx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB1_12
# BB#13:                                # %cond.true.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	32(%rcx), %rsi
	movzbl	(%rsi), %eax
	cmpq	$47, %rax
	movq	%rcx, %rbx
	jne	.LBB1_15
# BB#14:                                #   in Loop: Header=BB1_11 Depth=2
	movq	%rsi, 112(%rsp)         # 8-byte Spill
	jmp	.LBB1_16
	.align	16, 0x90
.LBB1_12:                               # %if.then.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	8(%rbx), %rax
	jmp	.LBB1_175
	.align	16, 0x90
.LBB1_15:                               # %cond.false.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	%rbx, %rdi
	callq	single_set_2
	movq	%rax, 112(%rsp)         # 8-byte Spill
.LBB1_16:                               # %cond.end25.i
                                        #   in Loop: Header=BB1_11 Depth=2
	leaq	32(%rbx), %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	%rbx, %rdi
	callq	extract_insn
	movq	$1, %rdi
	callq	constrain_operands
	callq	preprocess_constraints
	movslq	which_alternative(%rip), %r13
	movsbq	recog_data+996(%rip), %r15
	movq	%r15, 152(%rsp)         # 8-byte Spill
	movq	32(%rbx), %rdi
	callq	asm_noperands
	movq	32(%rbx), %rdi
	movq	%rax, 128(%rsp)         # 8-byte Spill
	testq	%r15, %r15
	jle	.LBB1_33
# BB#17:                                # %for.body.lr.ph.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	%rbx, 136(%rsp)         # 8-byte Spill
	movzbl	(%rdi), %r8d
	movq	$5, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	leaq	recog_op_alt+20(%rdx), %rdx
	xorq	%rsi, %rsi
	movq	$2, %rax
	movq	%r13, %r9
	.align	16, 0x90
.LBB1_18:                               # %for.body.i55
                                        #   Parent Loop BB1_2 Depth=1
                                        #     Parent Loop BB1_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movslq	-4(%rdx), %rdi
	testq	%rdi, %rdi
	js	.LBB1_20
# BB#19:                                # %if.then52.i
                                        #   in Loop: Header=BB1_18 Depth=3
	movq	$5, %rcx
	movq	%r9, %rbx
	shlq	%cl, %rbx
	imulq	$960, %rdi, %rcx        # imm = 0x3C0
	movl	recog_op_alt+8(%rcx,%rbx), %ecx
	movl	%ecx, -12(%rdx)
	jmp	.LBB1_23
	.align	16, 0x90
.LBB1_20:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB1_18 Depth=3
	movslq	(%rdx), %rcx
	testq	%rcx, %rcx
	jns	.LBB1_23
# BB#21:                                # %lor.lhs.false71.i
                                        #   in Loop: Header=BB1_18 Depth=3
	cmpq	$38, %r8
	jne	.LBB1_24
# BB#22:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB1_18 Depth=3
	movl	recog_data+840(,%rsi,4), %ecx
	cmpq	$1, %rcx
	jne	.LBB1_24
	.align	16, 0x90
.LBB1_23:                               # %if.then76.i
                                        #   in Loop: Header=BB1_18 Depth=3
	movl	%eax, recog_data+840(,%rsi,4)
.LBB1_24:                               # %for.inc.i
                                        #   in Loop: Header=BB1_18 Depth=3
	incq	%rsi
	movq	%rsi, %rcx
	andq	%r12, %rcx
	movq	152(%rsp), %r13         # 8-byte Reload
	andq	%r12, %r13
	addq	$960, %rdx              # imm = 0x3C0
	cmpq	%r13, %rcx
	jne	.LBB1_18
# BB#25:                                # %for.body83.i.preheader
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	$5, %rcx
	movq	%r9, %rax
	movq	%r9, 120(%rsp)          # 8-byte Spill
	shlq	%cl, %rax
	leaq	recog_op_alt+24(%rax), %rbx
	movq	$1, %r15
	.align	16, 0x90
.LBB1_26:                               # %for.body83.i
                                        #   Parent Loop BB1_2 Depth=1
                                        #     Parent Loop BB1_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rbx), %eax
	testq	$1, %rax
	je	.LBB1_28
# BB#27:                                # %if.then91.i
                                        #   in Loop: Header=BB1_26 Depth=3
	movq	recog_data-8(,%r15,8), %rdi
	movq	144(%rsp), %rsi         # 8-byte Reload
	callq	kill_value
.LBB1_28:                               # %for.inc95.i
                                        #   in Loop: Header=BB1_26 Depth=3
	movq	%r15, %rax
	andq	%r12, %rax
	incq	%r15
	addq	$960, %rbx              # imm = 0x3C0
	cmpq	%r13, %rax
	jne	.LBB1_26
# BB#29:                                # %for.body108.i.preheader
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	104(%rsp), %r15         # 8-byte Reload
	movq	(%r15), %rdi
	movabsq	$kill_clobbered_value, %rsi
	movq	144(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdx
	callq	note_stores
	movq	%r15, %rdi
	movabsq	$kill_autoinc_value, %rsi
	movq	%rbx, %rdx
	callq	for_each_rtx
	movq	$5, %rcx
	movq	120(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	leaq	recog_op_alt+24(%rax), %r15
	movq	$1, %rbx
	.align	16, 0x90
.LBB1_30:                               # %for.body108.i
                                        #   Parent Loop BB1_2 Depth=1
                                        #     Parent Loop BB1_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%r15), %eax
	testq	$1, %rax
	je	.LBB1_32
# BB#31:                                # %if.then118.i
                                        #   in Loop: Header=BB1_30 Depth=3
	movq	recog_data-8(,%rbx,8), %rdi
	movq	144(%rsp), %rsi         # 8-byte Reload
	callq	kill_value
.LBB1_32:                               # %for.inc122.i
                                        #   in Loop: Header=BB1_30 Depth=3
	movq	%rbx, %rax
	andq	%r12, %rax
	incq	%rbx
	addq	$960, %r15              # imm = 0x3C0
	cmpq	%r13, %rax
	jne	.LBB1_30
	jmp	.LBB1_34
	.align	16, 0x90
.LBB1_33:                               # %for.end124.critedge.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	%r13, 120(%rsp)         # 8-byte Spill
	movq	%rbx, 136(%rsp)         # 8-byte Spill
	movabsq	$kill_clobbered_value, %rsi
	movq	144(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdx
	callq	note_stores
	movq	104(%rsp), %rdi         # 8-byte Reload
	movabsq	$kill_autoinc_value, %rsi
	movq	%rbx, %rdx
	callq	for_each_rtx
.LBB1_34:                               # %for.end124.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$0, 112(%rsp)           # 8-byte Folded Reload
	je	.LBB1_60
# BB#35:                                # %land.lhs.true126.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	16(%rax), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movl	(%rax), %r15d
	movq	%r15, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB1_60
# BB#36:                                # %if.then134.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	48(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %r10d
	movq	$16, %rcx
	shrq	%cl, %r15
	movq	64(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%rax,4), %r11
	movq	%r11, 72(%rsp)          # 8-byte Spill
	movq	$7, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	addq	%r14, %rax
	leaq	(%r10,%r10,2), %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movl	(%rax,%rcx,4), %r9d
	andq	$255, %r15
	movq	%r15, 80(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r9, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%r9, %r15
	je	.LBB1_69
# BB#37:                                # %if.then148.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	%r10, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	je	.LBB1_41
# BB#38:                                # %if.then148.i
                                        #   in Loop: Header=BB1_11 Depth=2
	leaq	-21(%r10), %rax
	andq	%r12, %rax
	cmpq	$8, %rax
	jb	.LBB1_41
# BB#39:                                # %if.then148.i
                                        #   in Loop: Header=BB1_11 Depth=2
	leaq	-45(%r10), %rax
	andq	%r12, %rax
	cmpq	$8, %rax
	jb	.LBB1_41
# BB#40:                                # %if.then148.i
                                        #   in Loop: Header=BB1_11 Depth=2
	leaq	-29(%r10), %rax
	andq	%r12, %rax
	cmpq	$7, %rax
	jbe	.LBB1_41
# BB#49:                                # %cond.false182.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$24, %r15
	jne	.LBB1_50
# BB#53:                                # %cond.true191.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	target_flags(%rip), %edi
	movq	$24, %rcx
	movq	%rdi, %r8
	shrq	%cl, %r8
	andq	$2, %r8
	xorq	$6, %r8
	jmp	.LBB1_54
.LBB1_41:                               # %cond.true172.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	mode_class(,%r15,4), %eax
	cmpq	$5, %rax
	movq	$2, %r8
	je	.LBB1_45
# BB#42:                                # %lor.rhs.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$6, %rax
	movabsq	$0, %r8
	jne	.LBB1_44
# BB#43:                                # %lor.rhs.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movabsq	$1, %r8
.LBB1_44:                               # %lor.rhs.i
                                        #   in Loop: Header=BB1_11 Depth=2
	incq	%r8
.LBB1_45:                               # %cond.true234.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	mode_class(,%rsi,4), %eax
	cmpq	$5, %rax
	je	.LBB1_69
# BB#46:                                # %lor.rhs243.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$6, %rax
	movabsq	$0, %rdi
	jne	.LBB1_48
# BB#47:                                # %lor.rhs243.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movabsq	$1, %rdi
.LBB1_48:                               # %lor.rhs243.i
                                        #   in Loop: Header=BB1_11 Depth=2
	incq	%rdi
	jmp	.LBB1_59
.LBB1_50:                               # %cond.false182.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$18, %r15
	jne	.LBB1_52
# BB#51:                                # %cond.end209.thread783.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	%rdi, %r8
	shrq	%cl, %r8
	andq	$1, %r8
	xorq	$3, %r8
	jmp	.LBB1_54
.LBB1_52:                               # %cond.false195.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%r15), %eax
	movq	$23, %rcx
	movq	%rdi, %rbx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
	movq	%rax, %r8
.LBB1_54:                               # %cond.false254.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$24, %r9
	jne	.LBB1_55
# BB#57:                                # %cond.true272.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	$24, %rcx
	shrq	%cl, %rdi
	andq	$2, %rdi
	xorq	$6, %rdi
	jmp	.LBB1_59
.LBB1_55:                               # %cond.false254.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$18, %r9
	jne	.LBB1_58
# BB#56:                                # %cond.true261.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	xorq	$3, %rdi
	jmp	.LBB1_59
.LBB1_58:                               # %cond.false276.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movzbl	mode_size(%rsi), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rdi,%rax), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	movq	%rax, %rdi
.LBB1_59:                               # %cond.end297.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	cmpq	%rdi, %r8
	jg	.LBB1_60
.LBB1_69:                               # %if.end303.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx), %rax
	movzbl	(%rax), %eax
	leaq	16(%rcx), %rcx
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	cmpq	$61, %rax
	jne	.LBB1_76
# BB#70:                                # %if.then311.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	regclass_map(,%r10,4), %edi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	144(%rsp), %rdx         # 8-byte Reload
	movq	%r10, %r13
	movq	%r11, %rbx
	callq	find_oldest_value_reg
	movq	%rbx, %r11
	movq	%r13, %r10
	testq	%rax, %rax
	je	.LBB1_76
# BB#71:                                # %land.lhs.true316.i
                                        #   in Loop: Header=BB1_11 Depth=2
	xorq	%rcx, %rcx
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%r10, %rbx
	movq	%r11, %r13
	movq	%rax, 24(%rsp)          # 8-byte Spill
	callq	validate_change
	movq	%r13, %r11
	movq	%rbx, %r10
	testq	%r12, %rax
	movq	24(%rsp), %rax          # 8-byte Reload
	jne	.LBB1_72
.LBB1_76:                               # %if.end333.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	$7, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	addq	%r14, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movl	4(%rax,%rcx,4), %edx
	jmp	.LBB1_84
	.align	16, 0x90
.LBB1_83:                               # %for.inc380.i
                                        #   in Loop: Header=BB1_84 Depth=3
	movq	$7, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	addq	%r14, %rax
	movl	8(%rax,%rbx,4), %edx
.LBB1_84:                               # %for.inc380.i
                                        #   Parent Loop BB1_2 Depth=1
                                        #     Parent Loop BB1_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	cmpq	%r10, %rdx
	je	.LBB1_60
# BB#77:                                # %for.body340.i
                                        #   in Loop: Header=BB1_84 Depth=3
	movq	%rdx, %rax
	andq	%r12, %rax
	movq	$7, %rcx
	movq	%r11, %rsi
	shlq	%cl, %rsi
	addq	%r14, %rsi
	leaq	(%rax,%rax,2), %rbx
	movl	(%rsi,%rbx,4), %esi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r15, %rsi
	je	.LBB1_79
# BB#78:                                # %lor.lhs.false347.i
                                        #   in Loop: Header=BB1_84 Depth=3
	movzbl	mode_size(%rax), %eax
	movzbl	mode_size(%r15), %ecx
	cmpq	%rcx, %rax
	jb	.LBB1_83
.LBB1_79:                               # %if.then354.i
                                        #   in Loop: Header=BB1_84 Depth=3
	movq	$61, %rdi
	movq	%r15, %rsi
	movq	%r10, %r15
	callq	gen_rtx_fmt_i0
	movq	%rax, %r13
	xorq	%rcx, %rcx
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%r13, %rdx
	callq	validate_change
	movq	72(%rsp), %r11          # 8-byte Reload
	movq	%r15, %r10
	movq	80(%rsp), %r15          # 8-byte Reload
	testq	%r12, %rax
	je	.LBB1_83
# BB#80:                                # %if.then361.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	48(%rsp), %rax          # 8-byte Reload
	movl	16(%rax), %eax
	movl	%eax, 16(%r13)
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	jne	.LBB1_82
# BB#81:                                #   in Loop: Header=BB1_11 Depth=2
	movq	$1, 96(%rsp)            # 8-byte Folded Spill
	jmp	.LBB1_101
	.align	16, 0x90
.LBB1_60:                               # %for.cond386.preheader.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$0, 152(%rsp)           # 8-byte Folded Reload
	jle	.LBB1_101
# BB#61:                                # %for.body389.lr.ph.i
                                        #   in Loop: Header=BB1_11 Depth=2
	xorq	%r13, %r13
	.align	16, 0x90
.LBB1_62:                               # %for.body389.i
                                        #   Parent Loop BB1_2 Depth=1
                                        #     Parent Loop BB1_11 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB1_97 Depth 4
	movq	recog_data+480(,%r13,8), %rax
	movzbl	(%rax), %eax
	testq	%rax, %rax
	je	.LBB1_100
# BB#63:                                # %if.end397.i
                                        #   in Loop: Header=BB1_62 Depth=3
	movq	$32, %rcx
	movq	128(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_66
# BB#64:                                # %land.lhs.true400.i
                                        #   in Loop: Header=BB1_62 Depth=3
	movq	recog_data(,%r13,8), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB1_66
# BB#65:                                # %land.lhs.true407.i
                                        #   in Loop: Header=BB1_62 Depth=3
	movl	8(%rax), %ecx
	movl	16(%rax), %eax
	cmpq	%rax, %rcx
	je	.LBB1_100
	.align	16, 0x90
.LBB1_66:                               # %if.end421.i
                                        #   in Loop: Header=BB1_62 Depth=3
	movl	recog_data+840(,%r13,4), %eax
	testq	%rax, %rax
	jne	.LBB1_91
# BB#67:                                # %if.then426.i
                                        #   in Loop: Header=BB1_62 Depth=3
	movq	$5, %rcx
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	imulq	$960, %r13, %rax        # imm = 0x3C0
	movzbl	recog_op_alt+24(%rax,%rdx), %ecx
	testq	$64, %rcx
	jne	.LBB1_68
# BB#85:                                # %if.else446.i
                                        #   in Loop: Header=BB1_62 Depth=3
	movq	recog_data(,%r13,8), %rdi
	movzbl	(%rdi), %ecx
	cmpq	$66, %rcx
	je	.LBB1_92
# BB#86:                                # %if.else446.i
                                        #   in Loop: Header=BB1_62 Depth=3
	cmpq	$61, %rcx
	jne	.LBB1_100
# BB#87:                                # %if.then453.i
                                        #   in Loop: Header=BB1_62 Depth=3
	movq	recog_data+240(,%r13,8), %r15
	movq	$5, %rcx
	movq	120(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	(%r15), %rsi
	movl	recog_op_alt+8(%rax,%rdx), %edi
	movq	144(%rsp), %rdx         # 8-byte Reload
	callq	find_oldest_value_reg
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB1_100
# BB#88:                                # %if.then.i.i
                                        #   in Loop: Header=BB1_62 Depth=3
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB1_90
# BB#89:                                # %if.then2.i.i
                                        #   in Loop: Header=BB1_62 Depth=3
	movq	(%r15), %rax
	movl	8(%rbx), %r8d
	movq	136(%rsp), %rcx         # 8-byte Reload
	movl	8(%rcx), %edx
	movl	8(%rax), %ecx
	xorq	%rax, %rax
	movabsq	$.L.str11, %rsi
	callq	fprintf
.LBB1_90:                               # %if.end491.thread825.i
                                        #   in Loop: Header=BB1_62 Depth=3
	leaq	recog_data(,%r13,8), %rcx
	leaq	recog_data+240(,%r13,8), %rax
	movq	%rbx, (%r15)
	jmp	.LBB1_95
	.align	16, 0x90
.LBB1_91:                               # %if.else478.i
                                        #   in Loop: Header=BB1_62 Depth=3
	movq	recog_data(,%r13,8), %rdi
	movzbl	(%rdi), %eax
	cmpq	$66, %rax
	jne	.LBB1_100
.LBB1_92:                               # %if.then485.i
                                        #   in Loop: Header=BB1_62 Depth=3
	addq	$8, %rdi
	movq	$12, %rsi
	jmp	.LBB1_93
.LBB1_68:                               # %if.then436.i
                                        #   in Loop: Header=BB1_62 Depth=3
	movq	recog_data+240(,%r13,8), %rdi
	movq	$5, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movl	recog_op_alt+8(%rax,%rdx), %esi
.LBB1_93:                               # %if.end491.i
                                        #   in Loop: Header=BB1_62 Depth=3
	movq	136(%rsp), %rdx         # 8-byte Reload
	movq	144(%rsp), %rcx         # 8-byte Reload
	callq	replace_oldest_value_addr
	testq	%rax, %rax
	je	.LBB1_100
# BB#94:                                # %if.end491.i.if.then493.i_crit_edge
                                        #   in Loop: Header=BB1_62 Depth=3
	leaq	recog_data+240(,%r13,8), %rax
	leaq	recog_data(,%r13,8), %rcx
.LBB1_95:                               # %if.then493.i
                                        #   in Loop: Header=BB1_62 Depth=3
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	%rax, (%rcx)
	movsbq	recog_data+997(%rip), %rcx
	testq	%rcx, %rcx
	movq	$1, 96(%rsp)            # 8-byte Folded Spill
	jle	.LBB1_100
# BB#96:                                # %for.body503.i.preheader
                                        #   in Loop: Header=BB1_62 Depth=3
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB1_97:                               # %for.body503.i
                                        #   Parent Loop BB1_2 Depth=1
                                        #     Parent Loop BB1_11 Depth=2
                                        #       Parent Loop BB1_62 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movsbq	recog_data+992(%rdx), %rsi
	movq	%r13, %rdi
	andq	%r12, %rdi
	andq	%r12, %rsi
	cmpq	%rdi, %rsi
	jne	.LBB1_99
# BB#98:                                # %if.then509.i
                                        #   in Loop: Header=BB1_97 Depth=4
	movq	recog_data+960(,%rdx,8), %rsi
	movq	%rax, (%rsi)
.LBB1_99:                               # %for.inc513.i
                                        #   in Loop: Header=BB1_97 Depth=4
	incq	%rdx
	movq	%rdx, %rsi
	andq	%r12, %rsi
	movq	%rcx, %rdi
	andq	%r12, %rdi
	cmpq	%rdi, %rsi
	jne	.LBB1_97
	.align	16, 0x90
.LBB1_100:                              # %for.inc517.i
                                        #   in Loop: Header=BB1_62 Depth=3
	incq	%r13
	movq	%r13, %rax
	andq	%r12, %rax
	movq	152(%rsp), %rcx         # 8-byte Reload
	andq	%r12, %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_62
	jmp	.LBB1_101
.LBB1_82:                               # %if.then369.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	$1, 96(%rsp)            # 8-byte Folded Spill
	movl	8(%r13), %r8d
	jmp	.LBB1_75
.LBB1_72:                               # %if.then322.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	jne	.LBB1_74
# BB#73:                                #   in Loop: Header=BB1_11 Depth=2
	movq	$1, 96(%rsp)            # 8-byte Folded Spill
	jmp	.LBB1_101
.LBB1_74:                               # %if.then324.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	$1, 96(%rsp)            # 8-byte Folded Spill
	movl	8(%rax), %r8d
.LBB1_75:                               # %did_replacement.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	136(%rsp), %rax         # 8-byte Reload
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str11, %rsi
	movq	%r10, %rcx
	callq	fprintf
	.align	16, 0x90
.LBB1_101:                              # %did_replacement.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	136(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$34, %rax
	movabsq	$0, %r15
	movq	64(%rsp), %r13          # 8-byte Reload
	jne	.LBB1_111
# BB#102:                               # %for.cond525.preheader.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	regs_invalidated_by_call(%rip), %r8
	xorq	%r11, %r11
	.align	16, 0x90
.LBB1_103:                              # %for.body528.i
                                        #   Parent Loop BB1_2 Depth=1
                                        #     Parent Loop BB1_11 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB1_105 Depth 4
                                        #         Child Loop BB1_108 Depth 4
	movq	$1, %rax
	movq	%r11, %rcx
	shlq	%cl, %rax
	testq	%r8, %rax
	je	.LBB1_110
# BB#104:                               # %if.then531.i
                                        #   in Loop: Header=BB1_103 Depth=3
	leaq	(%r13,%r13,4), %rdi
	movq	$7, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	addq	%r14, %rax
	leaq	(%r11,%r11,2), %r10
	movl	4(%rax,%r10,4), %ebx
	leaq	4(%rax,%r10,4), %r9
	movq	%r11, %rdx
	andq	%r12, %rdx
	cmpq	%rdx, %rbx
	jne	.LBB1_105
# BB#107:                               # %if.else.i767.i
                                        #   in Loop: Header=BB1_103 Depth=3
	movq	$7, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	addq	%r14, %rax
	movl	8(%rax,%r10,4), %edx
	cmpq	%r12, %rdx
	movq	%rdx, %rsi
	je	.LBB1_109
	.align	16, 0x90
.LBB1_108:                              # %for.body29.i.i
                                        #   Parent Loop BB1_2 Depth=1
                                        #     Parent Loop BB1_11 Depth=2
                                        #       Parent Loop BB1_103 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	andq	%r12, %rsi
	movq	$7, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	addq	%r14, %rax
	leaq	(%rsi,%rsi,2), %rcx
	movl	%edx, 4(%rax,%rcx,4)
	movl	8(%rax,%rcx,4), %esi
	cmpq	%r12, %rsi
	jne	.LBB1_108
	jmp	.LBB1_109
	.align	16, 0x90
.LBB1_105:                              # %for.cond.i764.i
                                        #   Parent Loop BB1_2 Depth=1
                                        #     Parent Loop BB1_11 Depth=2
                                        #       Parent Loop BB1_103 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	andq	%r12, %rbx
	movq	$7, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	addq	%r14, %rsi
	leaq	(%rbx,%rbx,2), %rax
	movl	8(%rsi,%rax,4), %ebx
	cmpq	%rdx, %rbx
	jne	.LBB1_105
# BB#106:                               # %for.end.i765.i
                                        #   in Loop: Header=BB1_103 Depth=3
	movq	$7, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	addq	%r14, %rdx
	movl	8(%rdx,%r10,4), %ecx
	leaq	8(%rsi,%rax,4), %rax
	movl	%ecx, (%rax)
.LBB1_109:                              # %kill_value_regno.exit.i
                                        #   in Loop: Header=BB1_103 Depth=3
	movq	$7, %rcx
	shlq	%cl, %rdi
	addq	%r14, %rdi
	movl	%r15d, (%rdi,%r10,4)
	movl	%r11d, (%r9)
	movl	%r12d, 8(%rdi,%r10,4)
.LBB1_110:                              # %for.inc533.i
                                        #   in Loop: Header=BB1_103 Depth=3
	incq	%r11
	cmpq	$53, %r11
	jne	.LBB1_103
.LBB1_111:                              # %if.end536.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rdi
	movabsq	$kill_set_value, %rsi
	movq	144(%rsp), %rdx         # 8-byte Reload
	callq	note_stores
	movq	112(%rsp), %rdx         # 8-byte Reload
	testq	%rdx, %rdx
	je	.LBB1_174
# BB#112:                               # %land.lhs.true541.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	8(%rdx), %rax
	movl	(%rax), %edi
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB1_174
# BB#113:                               # %land.lhs.true549.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	16(%rdx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB1_174
# BB#114:                               # %if.then557.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	8(%rax), %r11d
	movl	8(%rcx), %esi
	cmpq	%r11, %rsi
	je	.LBB1_174
# BB#115:                               # %if.then557.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$7, %r11
	je	.LBB1_174
# BB#116:                               # %if.end6.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	frame_pointer_needed(%rip), %eax
	testq	%rax, %rax
	je	.LBB1_118
# BB#117:                               # %if.end6.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$6, %r11
	je	.LBB1_174
.LBB1_118:                              # %if.end9.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	%r11, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	cmpq	$8, %rax
	je	.LBB1_122
# BB#119:                               # %if.end9.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	leaq	-21(%r11), %rax
	andq	%r12, %rax
	cmpq	$8, %rax
	jb	.LBB1_122
# BB#120:                               # %if.end9.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	leaq	-45(%r11), %rax
	andq	%r12, %rax
	cmpq	$8, %rax
	jb	.LBB1_122
# BB#121:                               # %if.end9.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	leaq	-29(%r11), %rax
	andq	%r12, %rax
	cmpq	$7, %rax
	jbe	.LBB1_122
# BB#126:                               # %cond.false.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$24, %rdi
	jne	.LBB1_127
# BB#129:                               # %cond.true44.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movzbl	target_flags+3(%rip), %r9d
	andq	$2, %r9
	xorq	$6, %r9
	jmp	.LBB1_131
.LBB1_122:                              # %cond.true.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	mode_class(,%rdi,4), %eax
	cmpq	$5, %rax
	movq	$2, %r9
	je	.LBB1_131
# BB#123:                               # %lor.rhs.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$6, %rax
	movabsq	$0, %r9
	jne	.LBB1_125
# BB#124:                               # %lor.rhs.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movabsq	$1, %r9
.LBB1_125:                              # %lor.rhs.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	incq	%r9
	jmp	.LBB1_131
.LBB1_127:                              # %cond.false.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$18, %rdi
	jne	.LBB1_130
# BB#128:                               # %cond.true36.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	target_flags(%rip), %r9d
	movq	$25, %rcx
	shrq	%cl, %r9
	andq	$1, %r9
	xorq	$3, %r9
	jmp	.LBB1_131
.LBB1_130:                              # %cond.false48.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rdi), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
	movq	%rax, %r9
.LBB1_131:                              # %cond.end63.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	%rsi, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	movabsq	$0, %rax
	jne	.LBB1_133
# BB#132:                               # %cond.end63.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movabsq	$1, %rax
.LBB1_133:                              # %cond.end63.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	leaq	-21(%rsi), %rcx
	andq	%r12, %rcx
	cmpq	$8, %rcx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	orq	%rax, %rcx
	leaq	-45(%rsi), %rax
	andq	%r12, %rax
	cmpq	$8, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
	orq	%rcx, %rax
	leaq	-29(%rsi), %rcx
	andq	%r12, %rcx
	cmpq	$8, %rcx
	sbbq	%rbx, %rbx
	andq	$1, %rbx
	orq	%rax, %rbx
	je	.LBB1_138
# BB#134:                               # %cond.true88.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	mode_class(,%rdi,4), %eax
	cmpq	$5, %rax
	movq	$2, %r8
	je	.LBB1_143
# BB#135:                               # %lor.rhs96.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$6, %rax
	movabsq	$0, %r8
	jne	.LBB1_137
# BB#136:                               # %lor.rhs96.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movabsq	$1, %r8
.LBB1_137:                              # %lor.rhs96.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	incq	%r8
	jmp	.LBB1_143
.LBB1_138:                              # %cond.false106.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$24, %rdi
	jne	.LBB1_139
# BB#141:                               # %cond.true122.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movzbl	target_flags+3(%rip), %r8d
	andq	$2, %r8
	xorq	$6, %r8
	jmp	.LBB1_143
.LBB1_139:                              # %cond.false106.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$18, %rdi
	jne	.LBB1_142
# BB#140:                               # %cond.true112.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	target_flags(%rip), %r8d
	movq	$25, %rcx
	shrq	%cl, %r8
	andq	$1, %r8
	xorq	$3, %r8
	jmp	.LBB1_143
.LBB1_142:                              # %cond.false126.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	target_flags(%rip), %r8d
	movzbl	mode_size(%rdi), %eax
	movq	$23, %rcx
	shrq	%cl, %r8
	andq	$4, %r8
	leaq	3(%rax,%r8), %rax
	addq	$4, %r8
	cqto
	idivq	%r8
	movq	%rax, %r8
.LBB1_143:                              # %cond.end146.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	%rsi, %r11
	jbe	.LBB1_145
# BB#144:                               # %cond.end146.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	leaq	(%r8,%rsi), %rax
	andq	%r12, %rax
	cmpq	%rax, %r11
	jb	.LBB1_174
.LBB1_145:                              # %lor.lhs.false154.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	%r11, %rsi
	jbe	.LBB1_147
# BB#146:                               # %lor.lhs.false154.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	addq	%r11, %r9
	andq	%r12, %r9
	cmpq	%r9, %rsi
	jb	.LBB1_174
.LBB1_147:                              # %if.end162.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	leaq	(%r13,%r13,4), %r9
	movq	$7, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	addq	%r14, %rdx
	leaq	(%rsi,%rsi,2), %r10
	movl	(%rdx,%r10,4), %edi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rdi, %rdi
	jne	.LBB1_160
# BB#148:                               # %if.then167.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	$7, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	addq	%r14, %rax
	leaq	(%r11,%r11,2), %rcx
	movslq	(%rax,%rcx,4), %rax
	leaq	(%rdx,%r10,4), %rcx
	movl	%eax, (%rcx)
	testq	$1, %rbx
	je	.LBB1_153
# BB#149:                               # %cond.true.i.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB1_158
# BB#150:                               # %lor.rhs.i.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB1_152
# BB#151:                               # %lor.rhs.i.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movabsq	$1, %rax
.LBB1_152:                              # %lor.rhs.i.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	incq	%rax
	jmp	.LBB1_158
.LBB1_160:                              # %if.else.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	testq	$1, %rbx
	je	.LBB1_165
# BB#161:                               # %cond.true195.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB1_170
# BB#162:                               # %lor.rhs204.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB1_164
# BB#163:                               # %lor.rhs204.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movabsq	$1, %rax
.LBB1_164:                              # %lor.rhs204.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	incq	%rax
	jmp	.LBB1_170
.LBB1_153:                              # %cond.false.i.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	%rax, %rcx
	andq	%r12, %rcx
	cmpq	$24, %rcx
	jne	.LBB1_154
# BB#156:                               # %cond.true25.i.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB1_158
.LBB1_165:                              # %cond.false215.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$24, %rdi
	jne	.LBB1_166
# BB#168:                               # %cond.true233.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB1_170
.LBB1_154:                              # %cond.false.i.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$18, %rcx
	jne	.LBB1_157
# BB#155:                               # %cond.true21.i.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB1_158
.LBB1_166:                              # %cond.false215.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	$18, %rdi
	jne	.LBB1_169
# BB#167:                               # %cond.true222.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB1_170
.LBB1_157:                              # %cond.false29.i.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
.LBB1_158:                              # %cond.end41.i.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	8(%rsp), %rdi           # 8-byte Reload
	movl	(%rdi), %ecx
	movq	%rax, %rdx
	andq	%r12, %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB1_171
# BB#159:                               # %if.then.i.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	%eax, (%rdi)
	jmp	.LBB1_171
.LBB1_169:                              # %cond.false237.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
.LBB1_170:                              # %cond.end258.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	andq	%r12, %rax
	andq	%r12, %r8
	cmpq	%rax, %r8
	ja	.LBB1_174
.LBB1_171:                              # %if.end264.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	$7, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	addq	%r14, %rax
	movl	4(%rax,%r10,4), %ecx
	leaq	(%r11,%r11,2), %rdx
	movl	%ecx, 4(%rax,%rdx,4)
	.align	16, 0x90
.LBB1_172:                              # %for.cond.i.i
                                        #   Parent Loop BB1_2 Depth=1
                                        #     Parent Loop BB1_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	andq	%r12, %rsi
	movq	$7, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	addq	%r14, %rax
	leaq	(%rsi,%rsi,2), %rcx
	movl	8(%rax,%rcx,4), %esi
	cmpq	%r12, %rsi
	jne	.LBB1_172
# BB#173:                               # %for.end.i.i
                                        #   in Loop: Header=BB1_11 Depth=2
	leaq	8(%rax,%rcx,4), %rax
	movl	%r11d, (%rax)
	.align	16, 0x90
.LBB1_174:                              # %if.end564.i
                                        #   in Loop: Header=BB1_11 Depth=2
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	8(%rbx), %rax
	movq	136(%rsp), %rcx         # 8-byte Reload
.LBB1_175:                              # %if.end564.i
                                        #   in Loop: Header=BB1_11 Depth=2
	cmpq	%rax, %rcx
	jne	.LBB1_176
# BB#177:                               # %copyprop_hardreg_forward_1.exit
                                        #   in Loop: Header=BB1_2 Depth=1
	testq	$1, 96(%rsp)            # 8-byte Folded Reload
	movq	$1, %rax
	jne	.LBB1_179
# BB#178:                               # %copyprop_hardreg_forward_1.exit
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
.LBB1_179:                              # %copyprop_hardreg_forward_1.exit
                                        #   in Loop: Header=BB1_2 Depth=1
	incq	%r13
	movslq	n_basic_blocks(%rip), %rdx
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	32(%rsp), %rcx          # 8-byte Reload
	addq	$640, %rcx              # imm = 0x280
	cmpq	%rdx, %rsi
	movq	%rax, 16(%rsp)          # 8-byte Spill
	jl	.LBB1_2
# BB#180:                               # %for.end
	testq	$1, %rax
	je	.LBB1_184
# BB#181:                               # %if.then31
	movq	rtl_dump_file(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB1_183
# BB#182:                               # %if.then33
	movabsq	$.L.str5, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
.LBB1_183:                              # %if.end35
	callq	get_insns
	movq	%rax, %rdi
	callq	delete_noop_moves
	xorq	%rdi, %rdi
	movq	$2, %rsi
	movq	$25, %rdx
	callq	update_life_info
.LBB1_184:                              # %if.end37
	movq	%r14, %rdi
	addq	$160, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.Ltmp35:
	.size	copyprop_hardreg_forward, .Ltmp35-copyprop_hardreg_forward
	.cfi_endproc

	.globl	debug_value_data
	.align	16, 0x90
	.type	debug_value_data,@function
debug_value_data:                       # @debug_value_data
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp42:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp43:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp44:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp45:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp46:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp47:
	.cfi_def_cfa_offset 64
.Ltmp48:
	.cfi_offset %rbx, -48
.Ltmp49:
	.cfi_offset %r12, -40
.Ltmp50:
	.cfi_offset %r13, -32
.Ltmp51:
	.cfi_offset %r14, -24
.Ltmp52:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	xorq	%rsi, %rsi
	jmp	.LBB2_1
.LBB2_5:                                # %if.end16
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	mode_name(,%rax,8), %r8
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%rax, %rax
	movq	stderr(%rip), %rdi
	movabsq	$.L.str7, %rsi
	movq	%r12, %rdx
	jmp	.LBB2_6
.LBB2_4:                                # %if.then11
                                        #   in Loop: Header=BB2_1 Depth=1
	xorq	%rax, %rax
	movq	stderr(%rip), %rdi
	movq	%rsi, %r14
	movabsq	$.L.str6, %rsi
	movq	%r12, %rdx
	callq	fprintf
	movq	%r14, %rsi
	jmp	.LBB2_11
.LBB2_9:                                # %if.end47
                                        #   in Loop: Header=BB2_6 Depth=2
	movslq	(%r15,%r14,4), %rdx
	movq	mode_name(,%rdx,8), %r8
	orq	%rax, %rsi
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	xorq	%rax, %rax
	movq	stderr(%rip), %rdi
	movabsq	$.L.str7, %rsi
	movq	%rcx, %rdx
	.align	16, 0x90
.LBB2_6:                                # %if.end16
                                        #   Parent Loop BB2_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r8, %rcx
	callq	fprintf
	movl	8(%r15,%r14,4), %ecx
	cmpq	%r13, %rcx
	je	.LBB2_10
# BB#7:                                 # %for.body30
                                        #   in Loop: Header=BB2_6 Depth=2
	movq	$1, %rax
	shlq	%cl, %rax
	movq	8(%rsp), %rdx           # 8-byte Reload
	testq	%rdx, %rax
	movq	%rdx, %rsi
	jne	.LBB2_20
# BB#8:                                 # %if.end35
                                        #   in Loop: Header=BB2_6 Depth=2
	movq	%rcx, %rdx
	andq	%r13, %rdx
	leaq	(%rdx,%rdx,2), %r14
	movl	4(%r15,%r14,4), %r8d
	cmpq	%r12, %r8
	jne	.LBB2_21
	jmp	.LBB2_9
.LBB2_10:                               # %for.end
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	$10, %rdi
	movq	stderr(%rip), %rsi
	callq	fputc
	movq	8(%rsp), %rsi           # 8-byte Reload
	jmp	.LBB2_11
	.align	16, 0x90
.LBB2_1:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_6 Depth 2
	leaq	(%rbx,%rbx,2), %r14
	movl	4(%r15,%r14,4), %r12d
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	%rax, %r12
	jne	.LBB2_11
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB2_1 Depth=1
	movslq	(%r15,%r14,4), %rax
	testq	%rax, %rax
	jne	.LBB2_5
# BB#3:                                 # %if.then6
                                        #   in Loop: Header=BB2_1 Depth=1
	movl	8(%r15,%r14,4), %ecx
	cmpq	%r13, %rcx
	jne	.LBB2_4
.LBB2_11:                               # %for.inc64
                                        #   in Loop: Header=BB2_1 Depth=1
	incq	%rbx
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$53, %rax
	jb	.LBB2_1
# BB#12:                                # %for.body68.preheader
	addq	$8, %r15
	xorq	%rbx, %rbx
	movabsq	$.L.str10, %r12
	jmp	.LBB2_13
.LBB2_17:                               # %if.then89
                                        #   in Loop: Header=BB2_13 Depth=1
	movq	mode_name(,%rax,8), %rcx
	movl	(%r15), %r9d
	xorq	%rax, %rax
	movq	stderr(%rip), %rdi
	movq	%rsi, %r14
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movq	%r14, %rsi
	jmp	.LBB2_18
	.align	16, 0x90
.LBB2_13:                               # %for.body68
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	testq	%rsi, %rax
	jne	.LBB2_18
# BB#14:                                # %land.lhs.true
                                        #   in Loop: Header=BB2_13 Depth=1
	movslq	-8(%r15), %rax
	movl	-4(%r15), %r8d
	testq	%rax, %rax
	jne	.LBB2_17
# BB#15:                                # %land.lhs.true
                                        #   in Loop: Header=BB2_13 Depth=1
	movq	%rbx, %rcx
	andq	%r13, %rcx
	movq	%r8, %rdx
	andq	%r13, %rdx
	cmpq	%rcx, %rdx
	jne	.LBB2_17
# BB#16:                                # %lor.lhs.false83
                                        #   in Loop: Header=BB2_13 Depth=1
	movl	(%r15), %ecx
	cmpq	%r13, %rcx
	jne	.LBB2_17
.LBB2_18:                               # %for.inc106
                                        #   in Loop: Header=BB2_13 Depth=1
	incq	%rbx
	addq	$12, %r15
	cmpq	$53, %rbx
	jne	.LBB2_13
# BB#19:                                # %for.end108
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_21:                               # %if.then41
	movabsq	$.L.str9, %rsi
	xorq	%rax, %rax
	movq	stderr(%rip), %rdi
	movq	%rcx, %rdx
	movq	%r8, %rcx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fprintf  # TAILCALL
.LBB2_20:                               # %if.then33
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	movq	stderr(%rip), %rdi
	movq	%rcx, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fprintf  # TAILCALL
.Ltmp53:
	.size	debug_value_data, .Ltmp53-debug_value_data
	.cfi_endproc

	.align	16, 0x90
	.type	kill_value,@function
kill_value:                             # @kill_value
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp60:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp61:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp62:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp63:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp64:
	.cfi_def_cfa_offset 48
.Ltmp65:
	.cfi_offset %rbx, -48
.Ltmp66:
	.cfi_offset %r12, -40
.Ltmp67:
	.cfi_offset %r13, -32
.Ltmp68:
	.cfi_offset %r14, -24
.Ltmp69:
	.cfi_offset %r15, -16
	movq	%rdi, %rax
	movl	(%rax), %edi
	movq	%rsi, %rbx
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB3_2
# BB#1:                                 # %if.then
	movq	8(%rax), %rsi
	movl	16(%rax), %eax
	movzbl	2(%rsi), %edx
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movq	%rax, %rcx
	callq	simplify_subreg
	movl	(%rax), %edi
.LBB3_2:                                # %if.end
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB3_53
# BB#3:                                 # %if.then14
	movl	8(%rax), %r9d
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	movq	%r9, %rax
	andq	%rcx, %rax
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	cmpq	$8, %rax
	je	.LBB3_7
# BB#4:                                 # %if.then14
	leaq	-21(%r9), %rax
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %rax
	cmpq	$8, %rax
	jb	.LBB3_7
# BB#5:                                 # %if.then14
	leaq	-45(%r9), %rax
	andq	%r8, %rax
	cmpq	$8, %rax
	jb	.LBB3_7
# BB#6:                                 # %if.then14
	leaq	-29(%r9), %rax
	andq	%r8, %rax
	cmpq	$7, %rax
	jbe	.LBB3_7
# BB#12:                                # %cond.false
	cmpq	$24, %rdi
	jne	.LBB3_13
# BB#15:                                # %cond.true53
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB3_17
.LBB3_7:                                # %cond.true
	movl	mode_class(,%rdi,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB3_17
# BB#8:                                 # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB3_9
# BB#10:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB3_17
.LBB3_13:                               # %cond.false
	cmpq	$18, %rdi
	jne	.LBB3_16
# BB#14:                                # %cond.true46
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB3_17
.LBB3_9:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB3_17
.LBB3_16:                               # %cond.end72
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rdi), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	testq	%r8, %rax
	je	.LBB3_24
.LBB3_17:                               # %for.body.lr.ph
	xorq	%r10, %r10
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	xorq	%r8, %r8
	.align	16, 0x90
.LBB3_18:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_19 Depth 2
                                        #     Child Loop BB3_22 Depth 2
	leaq	(%r8,%r9), %r14
	movq	%r14, %rcx
	andq	%rdx, %rcx
	leaq	(%rcx,%rcx,2), %r15
	movl	4(%rbx,%r15,4), %edi
	leaq	4(%rbx,%r15,4), %r11
	cmpq	%rcx, %rdi
	jne	.LBB3_19
# BB#21:                                # %if.else.i
                                        #   in Loop: Header=BB3_18 Depth=1
	movl	8(%rbx,%r15,4), %ecx
	cmpq	%rdx, %rcx
	movq	%rcx, %rdi
	je	.LBB3_23
	.align	16, 0x90
.LBB3_22:                               # %for.body29.i
                                        #   Parent Loop BB3_18 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	%rdx, %rdi
	leaq	(%rdi,%rdi,2), %rsi
	movl	%ecx, 4(%rbx,%rsi,4)
	movl	8(%rbx,%rsi,4), %edi
	cmpq	%rdx, %rdi
	jne	.LBB3_22
	jmp	.LBB3_23
	.align	16, 0x90
.LBB3_19:                               # %for.cond.i
                                        #   Parent Loop BB3_18 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	%rdx, %rdi
	leaq	(%rdi,%rdi,2), %rsi
	movl	8(%rbx,%rsi,4), %edi
	cmpq	%rcx, %rdi
	jne	.LBB3_19
# BB#20:                                # %for.end.i
                                        #   in Loop: Header=BB3_18 Depth=1
	movl	8(%rbx,%r15,4), %ecx
	leaq	8(%rbx,%rsi,4), %rsi
	movl	%ecx, (%rsi)
.LBB3_23:                               # %kill_value_regno.exit
                                        #   in Loop: Header=BB3_18 Depth=1
	movl	%r10d, (%rbx,%r15,4)
	movl	%r14d, (%r11)
	incq	%r8
	movl	%edx, 8(%rbx,%r15,4)
	movq	%r8, %rcx
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	jne	.LBB3_18
.LBB3_24:                               # %for.end
	movl	636(%rbx), %eax
	xorq	%r10, %r10
	movq	%r9, %rcx
	subq	%rax, %rcx
	jb	.LBB3_26
# BB#25:                                # %for.end
	movq	%rcx, %r10
.LBB3_26:                               # %for.end
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%r10, %rcx
	andq	%r8, %rcx
	cmpq	%r9, %rcx
	jae	.LBB3_53
# BB#27:                                # %for.body86.lr.ph
	cmpq	%rax, %r9
	movq	%r9, %r12
	ja	.LBB3_29
# BB#28:                                # %for.body86.lr.ph
	movq	%rax, %r12
.LBB3_29:                               # %for.body86.lr.ph
	subq	%rax, %r12
	andq	%r8, %r12
	xorq	%r15, %r15
	.align	16, 0x90
.LBB3_30:                               # %for.body86
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_46 Depth 2
                                        #       Child Loop BB3_47 Depth 3
                                        #       Child Loop BB3_50 Depth 3
	leaq	(%r12,%r12,2), %rax
	movl	(%rbx,%rax,4), %edx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rdx, %rdx
	je	.LBB3_52
# BB#31:                                # %if.end92
                                        #   in Loop: Header=BB3_30 Depth=1
	movq	%r12, %rcx
	movabsq	$4294967288, %rsi       # imm = 0xFFFFFFF8
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	je	.LBB3_35
# BB#32:                                # %if.end92
                                        #   in Loop: Header=BB3_30 Depth=1
	leaq	-21(%r12), %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jb	.LBB3_35
# BB#33:                                # %if.end92
                                        #   in Loop: Header=BB3_30 Depth=1
	leaq	-45(%r12), %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jb	.LBB3_35
# BB#34:                                # %if.end92
                                        #   in Loop: Header=BB3_30 Depth=1
	leaq	-29(%r12), %rcx
	andq	%r8, %rcx
	cmpq	$7, %rcx
	jbe	.LBB3_35
# BB#39:                                # %cond.false136
                                        #   in Loop: Header=BB3_30 Depth=1
	cmpq	$24, %rdx
	jne	.LBB3_40
# BB#42:                                # %cond.true154
                                        #   in Loop: Header=BB3_30 Depth=1
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB3_44
	.align	16, 0x90
.LBB3_35:                               # %cond.true116
                                        #   in Loop: Header=BB3_30 Depth=1
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB3_44
# BB#36:                                # %lor.rhs125
                                        #   in Loop: Header=BB3_30 Depth=1
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB3_38
# BB#37:                                # %lor.rhs125
                                        #   in Loop: Header=BB3_30 Depth=1
	movabsq	$1, %rax
.LBB3_38:                               # %lor.rhs125
                                        #   in Loop: Header=BB3_30 Depth=1
	incq	%rax
	jmp	.LBB3_44
.LBB3_40:                               # %cond.false136
                                        #   in Loop: Header=BB3_30 Depth=1
	cmpq	$18, %rdx
	jne	.LBB3_43
# BB#41:                                # %cond.true143
                                        #   in Loop: Header=BB3_30 Depth=1
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB3_44
.LBB3_43:                               # %cond.false158
                                        #   in Loop: Header=BB3_30 Depth=1
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	.align	16, 0x90
.LBB3_44:                               # %cond.end179
                                        #   in Loop: Header=BB3_30 Depth=1
	leaq	(%rax,%r12), %rcx
	andq	%r8, %rcx
	cmpq	%r9, %rcx
	jbe	.LBB3_52
# BB#45:                                # %cond.end179
                                        #   in Loop: Header=BB3_30 Depth=1
	xorq	%rcx, %rcx
	testq	%r8, %rax
	je	.LBB3_52
	.align	16, 0x90
.LBB3_46:                               # %for.body188
                                        #   Parent Loop BB3_30 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_47 Depth 3
                                        #       Child Loop BB3_50 Depth 3
	leaq	(%rcx,%r12), %r14
	movq	%r14, %rdx
	andq	%r8, %rdx
	leaq	(%rdx,%rdx,2), %rdi
	movl	4(%rbx,%rdi,4), %r11d
	leaq	4(%rbx,%rdi,4), %r13
	cmpq	%rdx, %r11
	jne	.LBB3_47
# BB#49:                                # %if.else.i290
                                        #   in Loop: Header=BB3_46 Depth=2
	movl	8(%rbx,%rdi,4), %esi
	cmpq	%r8, %rsi
	movq	%rsi, %rdx
	je	.LBB3_51
	.align	16, 0x90
.LBB3_50:                               # %for.body29.i296
                                        #   Parent Loop BB3_30 Depth=1
                                        #     Parent Loop BB3_46 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	andq	%r8, %rdx
	leaq	(%rdx,%rdx,2), %rdx
	movl	%esi, 4(%rbx,%rdx,4)
	movl	8(%rbx,%rdx,4), %edx
	cmpq	%r8, %rdx
	jne	.LBB3_50
	jmp	.LBB3_51
	.align	16, 0x90
.LBB3_47:                               # %for.cond.i285
                                        #   Parent Loop BB3_30 Depth=1
                                        #     Parent Loop BB3_46 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	andq	%r8, %r11
	leaq	(%r11,%r11,2), %rsi
	movl	8(%rbx,%rsi,4), %r11d
	cmpq	%rdx, %r11
	jne	.LBB3_47
# BB#48:                                # %for.end.i287
                                        #   in Loop: Header=BB3_46 Depth=2
	movl	8(%rbx,%rdi,4), %edx
	leaq	8(%rbx,%rsi,4), %rsi
	movl	%edx, (%rsi)
.LBB3_51:                               # %kill_value_regno.exit299
                                        #   in Loop: Header=BB3_46 Depth=2
	movl	%r15d, (%rbx,%rdi,4)
	movl	%r14d, (%r13)
	movl	%r8d, 8(%rbx,%rdi,4)
	incq	%rcx
	movq	%rcx, %rdx
	andq	%r8, %rdx
	movq	%rax, %rsi
	andq	%r8, %rsi
	cmpq	%rsi, %rdx
	jne	.LBB3_46
.LBB3_52:                               # %for.inc194
                                        #   in Loop: Header=BB3_30 Depth=1
	incq	%r10
	movq	%r10, %rax
	andq	%r8, %rax
	incq	%r12
	cmpq	%r9, %rax
	jne	.LBB3_30
.LBB3_53:                               # %if.end197
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp70:
	.size	kill_value, .Ltmp70-kill_value
	.cfi_endproc

	.align	16, 0x90
	.type	kill_clobbered_value,@function
kill_clobbered_value:                   # @kill_clobbered_value
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rsi), %eax
	cmpq	$49, %rax
	jne	.LBB4_1
# BB#2:                                 # %if.then
	movq	%rdx, %rsi
	jmp	kill_value  # TAILCALL
.LBB4_1:                                # %if.end
	retq
.Ltmp71:
	.size	kill_clobbered_value, .Ltmp71-kill_clobbered_value
	.cfi_endproc

	.align	16, 0x90
	.type	kill_autoinc_value,@function
kill_autoinc_value:                     # @kill_autoinc_value
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp75:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp76:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp77:
	.cfi_def_cfa_offset 32
.Ltmp78:
	.cfi_offset %rbx, -24
.Ltmp79:
	.cfi_offset %r14, -16
	movq	(%rdi), %rax
	movzbl	(%rax), %ecx
	movzbl	rtx_class(%rcx), %edx
	movq	%rsi, %r14
	xorq	%rcx, %rcx
	cmpq	$97, %rdx
	jne	.LBB5_13
# BB#1:                                 # %if.then
	movq	8(%rax), %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	kill_value
	movl	8(%rbx), %edx
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	andq	$1, %rax
	orq	$4, %rax
	leaq	(%rdx,%rdx,2), %rcx
	movl	%eax, (%r14,%rcx,4)
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	je	.LBB5_5
# BB#2:                                 # %if.then
	leaq	-21(%rdx), %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdi
	cmpq	$8, %rdi
	jb	.LBB5_5
# BB#3:                                 # %if.then
	leaq	-45(%rdx), %rdi
	andq	%rcx, %rdi
	cmpq	$8, %rdi
	jb	.LBB5_5
# BB#4:                                 # %if.then
	addq	$-29, %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB5_5
# BB#10:                                # %cond.false29.i
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	jmp	.LBB5_11
.LBB5_5:                                # %cond.true.i
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB5_11
# BB#6:                                 # %lor.rhs.i
	cmpq	$6, %rcx
	jne	.LBB5_7
# BB#8:                                 # %lor.rhs.i
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB5_11
.LBB5_7:
	movabsq	$0, %rax
	incq	%rax
.LBB5_11:                               # %cond.end41.i
	movl	636(%r14), %edx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rax, %rsi
	andq	%rcx, %rsi
	cmpq	%rdx, %rsi
	jbe	.LBB5_13
# BB#12:                                # %if.then.i
	movl	%eax, 636(%r14)
.LBB5_13:                               # %return
	movq	%rcx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp80:
	.size	kill_autoinc_value, .Ltmp80-kill_autoinc_value
	.cfi_endproc

	.align	16, 0x90
	.type	find_oldest_value_reg,@function
find_oldest_value_reg:                  # @find_oldest_value_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp87:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp88:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp89:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp90:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp91:
	.cfi_def_cfa_offset 48
.Ltmp92:
	.cfi_offset %rbx, -48
.Ltmp93:
	.cfi_offset %r12, -40
.Ltmp94:
	.cfi_offset %r13, -32
.Ltmp95:
	.cfi_offset %r14, -24
.Ltmp96:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movl	8(%r14), %r10d
	movq	%rdx, %r13
	leaq	(%r10,%r10,2), %r11
	movl	(%r13,%r11,4), %r12d
	movzbl	2(%r14), %esi
	movq	$32, %rcx
	movq	%r12, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	cmpq	%r12, %rsi
	je	.LBB6_26
# BB#1:                                 # %if.then
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%r10, %rax
	cmpq	$8, %rax
	je	.LBB6_5
# BB#2:                                 # %if.then
	leaq	-21(%r10), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB6_5
# BB#3:                                 # %if.then
	leaq	-45(%r10), %rcx
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB6_5
# BB#4:                                 # %if.then
	leaq	-29(%r10), %rcx
	andq	%rax, %rcx
	cmpq	$7, %rcx
	jbe	.LBB6_5
# BB#13:                                # %cond.false
	cmpq	$24, %rsi
	jne	.LBB6_14
# BB#19:                                # %cond.true27
	movl	target_flags(%rip), %r9d
	movq	$24, %rcx
	movq	%r9, %r8
	shrq	%cl, %r8
	andq	$2, %r8
	xorq	$6, %r8
	jmp	.LBB6_20
.LBB6_5:                                # %cond.true
	movl	mode_class(,%rsi,4), %eax
	cmpq	$5, %rax
	movq	$2, %r8
	je	.LBB6_10
# BB#6:                                 # %lor.rhs
	cmpq	$6, %rax
	jne	.LBB6_7
# BB#8:                                 # %lor.rhs
	movabsq	$1, %r8
	jmp	.LBB6_9
.LBB6_14:                               # %cond.false
	cmpq	$18, %rsi
	jne	.LBB6_16
# BB#15:                                # %cond.end43.thread252
	movl	target_flags(%rip), %r9d
	movq	$25, %rcx
	movq	%r9, %r8
	shrq	%cl, %r8
	andq	$1, %r8
	xorq	$3, %r8
	jmp	.LBB6_20
.LBB6_7:
	movabsq	$0, %r8
.LBB6_9:                                # %lor.rhs
	incq	%r8
.LBB6_10:                               # %cond.true68
	movl	mode_class(,%r15,4), %eax
	cmpq	$5, %rax
	je	.LBB6_26
# BB#11:                                # %lor.rhs77
	cmpq	$6, %rax
	jne	.LBB6_12
# BB#17:                                # %lor.rhs77
	movabsq	$1, %r9
	incq	%r9
	jmp	.LBB6_25
.LBB6_16:                               # %cond.false31
	movl	target_flags(%rip), %r9d
	movzbl	mode_size(%rsi), %eax
	movq	$23, %rcx
	movq	%r9, %rbx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
	movq	%rax, %r8
.LBB6_20:                               # %cond.false88
	cmpq	$24, %r12
	jne	.LBB6_21
# BB#23:                                # %cond.true106
	movq	$24, %rcx
	shrq	%cl, %r9
	andq	$2, %r9
	xorq	$6, %r9
	jmp	.LBB6_25
.LBB6_21:                               # %cond.false88
	cmpq	$18, %r12
	jne	.LBB6_24
# BB#22:                                # %cond.true95
	movq	$25, %rcx
	shrq	%cl, %r9
	andq	$1, %r9
	xorq	$3, %r9
	jmp	.LBB6_25
.LBB6_12:
	movabsq	$0, %r9
	incq	%r9
	jmp	.LBB6_25
.LBB6_24:                               # %cond.false110
	movzbl	mode_size(%r15), %eax
	movq	$23, %rcx
	shrq	%cl, %r9
	andq	$4, %r9
	leaq	3(%rax,%r9), %rax
	addq	$4, %r9
	cqto
	idivq	%r9
	movq	%rax, %r9
.LBB6_25:                               # %cond.end131
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	xorq	%rax, %rax
	cmpq	%r9, %r8
	jg	.LBB6_33
.LBB6_26:                               # %if.end136
	movl	4(%r13,%r11,4), %ebx
	xorq	%rax, %rax
	cmpq	%r10, %rbx
	je	.LBB6_33
# BB#27:                                # %for.body.lr.ph
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %rdi
	movq	reg_class_contents(,%rdi,8), %r9
	.align	16, 0x90
.LBB6_28:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdx
	andq	%r8, %rdx
	movq	$1, %rdi
	movq	%rbx, %rcx
	shlq	%cl, %rdi
	testq	%rdi, %r9
	je	.LBB6_32
# BB#29:                                # %land.lhs.true146
                                        #   in Loop: Header=BB6_28 Depth=1
	leaq	(%rdx,%rdx,2), %rcx
	movl	(%r13,%rcx,4), %r11d
	movq	$32, %rcx
	movq	%r11, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rsi, %r11
	je	.LBB6_31
# BB#30:                                # %lor.lhs.false153
                                        #   in Loop: Header=BB6_28 Depth=1
	movzbl	mode_size(%rdi), %ecx
	movzbl	mode_size(%rsi), %edi
	cmpq	%rdi, %rcx
	jae	.LBB6_31
.LBB6_32:                               # %for.inc
                                        #   in Loop: Header=BB6_28 Depth=1
	leaq	(%rdx,%rdx,2), %rcx
	movl	8(%r13,%rcx,4), %ebx
	cmpq	%r10, %rbx
	jne	.LBB6_28
	jmp	.LBB6_33
.LBB6_31:                               # %if.then159
	movq	$61, %rdi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_i0
	movl	16(%r14), %ecx
	movl	%ecx, 16(%rax)
.LBB6_33:                               # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp97:
	.size	find_oldest_value_reg, .Ltmp97-find_oldest_value_reg
	.cfi_endproc

	.align	16, 0x90
	.type	replace_oldest_value_addr,@function
replace_oldest_value_addr:              # @replace_oldest_value_addr
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
	subq	$48, %rsp
.Ltmp109:
	.cfi_def_cfa_offset 96
.Ltmp110:
	.cfi_offset %rbx, -48
.Ltmp111:
	.cfi_offset %r12, -40
.Ltmp112:
	.cfi_offset %r13, -32
.Ltmp113:
	.cfi_offset %r14, -24
.Ltmp114:
	.cfi_offset %r15, -16
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%rdi, %r14
	xorq	%rbx, %rbx
	jmp	.LBB7_1
	.align	16, 0x90
.LBB7_64:                               # %sw.bb505
                                        #   in Loop: Header=BB7_1 Depth=1
	addq	$8, %r15
	movq	$12, 32(%rsp)           # 8-byte Folded Spill
	movq	%r15, %r14
.LBB7_1:                                # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r14), %r15
	movzbl	(%r15), %eax
	cmpq	$95, %rax
	jg	.LBB7_69
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB7_1 Depth=1
	cmpq	$66, %rax
	je	.LBB7_64
# BB#3:                                 # %tailrecurse
	cmpq	$61, %rax
	jne	.LBB7_4
# BB#65:                                # %sw.bb507
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	find_oldest_value_reg
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB7_82
# BB#66:                                # %if.then.i
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB7_68
# BB#67:                                # %if.then2.i
	movq	(%r14), %rax
	movl	8(%r15), %r8d
	movq	16(%rsp), %rcx          # 8-byte Reload
	movl	8(%rcx), %edx
	movl	8(%rax), %ecx
	movabsq	$.L.str11, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB7_68:                               # %if.end.i
	movq	%r15, (%r14)
	movq	$1, %rbx
	jmp	.LBB7_82
.LBB7_69:                               # %tailrecurse
	leaq	-96(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$6, %rdx
	jae	.LBB7_70
.LBB7_82:                               # %return
	movq	%rbx, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB7_4:                                # %tailrecurse
	cmpq	$75, %rax
	jne	.LBB7_70
# BB#5:                                 # %sw.bb
	movq	8(%r15), %rdx
	movq	16(%r15), %r8
	movzbl	(%rdx), %esi
	movl	(%r8), %eax
	cmpq	$63, %rsi
	jne	.LBB7_7
# BB#6:                                 # %if.then
	movq	8(%rdx), %rdx
	movzbl	(%rdx), %esi
.LBB7_7:                                # %if.end
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB7_9
# BB#8:                                 # %if.then18
	movq	8(%r8), %r8
	movl	(%r8), %eax
.LBB7_9:                                # %if.end24
	leaq	8(%r15), %r13
	leaq	16(%r15), %rdi
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	je	.LBB7_10
# BB#11:                                # %switch.early.test
	leaq	-78(%rsi), %rcx
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	andq	%r9, %rcx
	cmpq	$44, %rcx
	ja	.LBB7_13
# BB#12:                                # %switch.early.test
	movq	$1, %rbx
	shlq	%cl, %rbx
	movabsq	$30786325577729, %rcx   # imm = 0x1C0000000001
	testq	%rcx, %rbx
	je	.LBB7_13
.LBB7_10:
	movq	%rdi, %rax
	movq	%r13, %rdi
	movq	%rax, %r13
	jmp	.LBB7_60
.LBB7_70:                               # %sw.epilog
	movq	rtx_format(,%rax,8), %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movzbl	rtx_length(%rax), %r13d
	addq	$8, %r15
	movq	%r15, 8(%rsp)           # 8-byte Spill
	xorq	%rbx, %rbx
	jmp	.LBB7_71
.LBB7_79:                               #   in Loop: Header=BB7_71 Depth=1
	movq	8(%rsp), %r15           # 8-byte Reload
	.align	16, 0x90
.LBB7_71:                               # %for.cond.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_72 Depth 2
                                        #     Child Loop BB7_78 Depth 2
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	leaq	(%r15,%r13,8), %r14
.LBB7_72:                               # %for.cond
                                        #   Parent Loop BB7_71 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB7_81
# BB#73:                                # %for.body
                                        #   in Loop: Header=BB7_72 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	movzbl	-1(%rax,%r13), %eax
	decq	%r13
	addq	$-8, %r14
	cmpq	$69, %rax
	je	.LBB7_76
# BB#74:                                # %for.body
                                        #   in Loop: Header=BB7_72 Depth=2
	cmpq	$101, %rax
	jne	.LBB7_72
# BB#75:                                # %if.then521
                                        #   in Loop: Header=BB7_71 Depth=1
	movq	%r14, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	replace_oldest_value_addr
	andq	$1, %rbx
	orq	%rax, %rbx
	jmp	.LBB7_71
	.align	16, 0x90
.LBB7_76:                               # %if.then539
                                        #   in Loop: Header=BB7_71 Depth=1
	movq	(%r14), %rdi
	movslq	(%rdi), %r12
	testq	%r12, %r12
	jle	.LBB7_71
# BB#77:                                # %for.body547.lr.ph
                                        #   in Loop: Header=BB7_71 Depth=1
	movq	$3, %rcx
	leaq	-1(%r12), %r15
	shlq	%cl, %r12
	jmp	.LBB7_78
	.align	16, 0x90
.LBB7_80:                               # %for.body547.for.body547_crit_edge
                                        #   in Loop: Header=BB7_78 Depth=2
	movq	(%r14), %rdi
	decq	%r15
	addq	$-8, %r12
.LBB7_78:                               # %for.body547
                                        #   Parent Loop BB7_71 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	addq	%r12, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	replace_oldest_value_addr
	andq	$1, %rbx
	orq	%rax, %rbx
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB7_80
	jmp	.LBB7_79
.LBB7_81:                               # %for.end565
	andq	$1, %rbx
	jmp	.LBB7_82
.LBB7_13:                               # %if.else
	cmpq	$66, %rsi
	je	.LBB7_60
# BB#14:                                # %switch.early.test4
	leaq	-78(%rax), %rcx
	andq	%r9, %rcx
	cmpq	$44, %rcx
	ja	.LBB7_16
# BB#15:                                # %switch.early.test4
	movq	$1, %rbx
	shlq	%cl, %rbx
	movabsq	$30786325577729, %rcx   # imm = 0x1C0000000001
	testq	%rcx, %rbx
	jne	.LBB7_60
.LBB7_16:                               # %if.else56
	xorq	%rbx, %rbx
	leaq	-54(%rsi), %rcx
	andq	%r9, %rcx
	cmpq	$14, %rcx
	ja	.LBB7_19
# BB#17:                                # %if.else56
	movq	$1, %r10
	shlq	%cl, %r10
	testq	$24593, %r10            # imm = 0x6011
	je	.LBB7_19
# BB#18:
	movq	%rdi, %r13
	jmp	.LBB7_62
.LBB7_19:                               # %if.else68
	leaq	-54(%rax), %rcx
	andq	%r9, %rcx
	cmpq	$14, %rcx
	ja	.LBB7_21
# BB#20:                                # %if.else68
	movq	$1, %r10
	shlq	%cl, %r10
	testq	$24593, %r10            # imm = 0x6011
	jne	.LBB7_62
.LBB7_21:                               # %if.else80
	cmpq	$61, %rsi
	jne	.LBB7_56
# BB#22:                                # %if.else80
	cmpq	$61, %rax
	jne	.LBB7_56
# BB#23:                                # %if.then83
	movl	8(%rdx), %edx
	cmpq	$44, %rdx
	ja	.LBB7_25
# BB#24:                                # %if.then83
	movq	$1, %rax
	movq	%rdx, %rcx
	shlq	%cl, %rax
	movabsq	$35046933135487, %rcx   # imm = 0x1FE00000007F
	testq	%rcx, %rax
	jne	.LBB7_28
.LBB7_25:                               # %lor.lhs.false97
	movq	reg_renumber(%rip), %rdi
	movzbl	(%rdi,%rdx,2), %eax
	cmpq	$44, %rax
	ja	.LBB7_27
# BB#26:                                # %lor.lhs.false97
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	movabsq	$35046933135487, %rax   # imm = 0x1FE00000007F
	testq	%rax, %rsi
	je	.LBB7_27
.LBB7_28:                               # %land.lhs.true122
	movl	8(%r8), %r8d
	xorq	%rsi, %rsi
	cmpq	$20, %r8
	ja	.LBB7_30
# BB#29:                                # %land.lhs.true122
	movq	$1, %rax
	movq	%r8, %rcx
	shlq	%cl, %rax
	testq	$1114367, %rax          # imm = 0x1100FF
	jne	.LBB7_55
.LBB7_30:                               # %lor.lhs.false140
	leaq	-37(%r8), %rax
	andq	%r9, %rax
	cmpq	$8, %rax
	jb	.LBB7_55
# BB#31:                                # %lor.lhs.false152
	movq	reg_renumber(%rip), %rdi
	movzbl	(%rdi,%r8,2), %eax
	cmpq	$44, %rax
	ja	.LBB7_33
# BB#32:                                # %lor.lhs.false152
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	movabsq	$35046933135615, %rax   # imm = 0x1FE0000000FF
	testq	%rax, %rbx
	jne	.LBB7_55
	jmp	.LBB7_33
.LBB7_56:                               # %if.else459
	cmpq	$61, %rsi
	je	.LBB7_10
# BB#57:                                # %if.else469
	xorq	%rbx, %rbx
	cmpq	$61, %rax
	je	.LBB7_59
# BB#58:                                # %if.else469
	movabsq	$0, %r13
.LBB7_59:                               # %if.else469
	jne	.LBB7_62
	jmp	.LBB7_60
.LBB7_27:                               # %lor.lhs.false97.if.else180_crit_edge
	movl	8(%r8), %r8d
.LBB7_33:                               # %if.else180
	movq	%r8, %rbx
	andq	%r9, %rbx
	cmpq	$44, %rbx
	ja	.LBB7_35
# BB#34:                                # %if.else180
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	movabsq	$35046933135487, %rcx   # imm = 0x1FE00000007F
	testq	%rcx, %rax
	jne	.LBB7_37
.LBB7_35:                               # %lor.lhs.false198
	movzbl	(%rdi,%rbx,2), %eax
	cmpq	$44, %rax
	ja	.LBB7_42
# BB#36:                                # %lor.lhs.false198
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	movabsq	$35046933135487, %rax   # imm = 0x1FE00000007F
	testq	%rax, %rsi
	je	.LBB7_42
.LBB7_37:                               # %land.lhs.true225
	movq	$1, %rsi
	cmpq	$20, %rdx
	ja	.LBB7_39
# BB#38:                                # %land.lhs.true225
	movq	$1, %rax
	movq	%rdx, %rcx
	shlq	%cl, %rax
	testq	$1114367, %rax          # imm = 0x1100FF
	jne	.LBB7_55
.LBB7_39:                               # %lor.lhs.false243
	leaq	-37(%rdx), %rax
	andq	%r9, %rax
	cmpq	$8, %rax
	jb	.LBB7_55
# BB#40:                                # %lor.lhs.false255
	movzbl	(%rdi,%rdx,2), %eax
	cmpq	$44, %rax
	ja	.LBB7_42
# BB#41:                                # %lor.lhs.false255
	movq	$1, %r10
	movq	%rax, %rcx
	shlq	%cl, %r10
	movabsq	$35046933135615, %rax   # imm = 0x1FE0000000FF
	testq	%rax, %r10
	jne	.LBB7_55
.LBB7_42:                               # %if.else283
	xorq	%rsi, %rsi
	cmpq	$20, %rbx
	ja	.LBB7_44
# BB#43:                                # %if.else283
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	testq	$1114367, %rax          # imm = 0x1100FF
	jne	.LBB7_55
.LBB7_44:                               # %lor.lhs.false301
	addq	$-37, %r8
	andq	%r9, %r8
	cmpq	$8, %r8
	jb	.LBB7_55
# BB#45:                                # %lor.lhs.false313
	movzbl	(%rdi,%rbx,2), %eax
	cmpq	$44, %rax
	ja	.LBB7_47
# BB#46:                                # %lor.lhs.false313
	movq	$1, %r8
	movq	%rax, %rcx
	shlq	%cl, %r8
	movabsq	$35046933135615, %rcx   # imm = 0x1FE0000000FF
	testq	%rcx, %r8
	jne	.LBB7_55
.LBB7_47:                               # %if.else341
	movq	$1, %rsi
	cmpq	$20, %rdx
	ja	.LBB7_49
# BB#48:                                # %if.else341
	movq	$1, %r8
	movq	%rdx, %rcx
	shlq	%cl, %r8
	testq	$1114367, %r8           # imm = 0x1100FF
	jne	.LBB7_55
.LBB7_49:                               # %lor.lhs.false359
	leaq	-37(%rdx), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB7_55
# BB#50:                                # %lor.lhs.false371
	movzbl	(%rdi,%rdx,2), %ecx
	leaq	-37(%rcx), %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$8, %rdx
	jb	.LBB7_55
# BB#51:                                # %lor.lhs.false371
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB7_55
# BB#52:                                # %lor.lhs.false371
	cmpq	$7, %rbx
	jb	.LBB7_55
# BB#53:                                # %lor.lhs.false371
	leaq	-37(%rax), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB7_55
# BB#54:                                # %lor.lhs.false435
	cmpq	$7, %rax
	sbbq	%rsi, %rsi
	andq	$1, %rsi
.LBB7_55:                               # %if.end450
	movq	%r15, %rax
	leaq	8(%rax,%rsi,8), %rdi
	xorq	$1, %rsi
	leaq	8(%rax,%rsi,8), %r13
.LBB7_60:                               # %if.end485
	xorq	%rbx, %rbx
	testq	%rdi, %rdi
	je	.LBB7_62
# BB#61:                                # %if.then487
	movq	$10, %rsi
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	replace_oldest_value_addr
	movq	%rax, %rbx
.LBB7_62:                               # %if.end492
	testq	%r13, %r13
	je	.LBB7_82
# BB#63:                                # %if.then494
	movq	$12, %rsi
	movq	%r13, %rdi
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	replace_oldest_value_addr
	orq	%rax, %rbx
	jmp	.LBB7_82
.Ltmp115:
	.size	replace_oldest_value_addr, .Ltmp115-replace_oldest_value_addr
	.cfi_endproc

	.align	16, 0x90
	.type	kill_set_value,@function
kill_set_value:                         # @kill_set_value
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp119:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp120:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp121:
	.cfi_def_cfa_offset 32
.Ltmp122:
	.cfi_offset %rbx, -24
.Ltmp123:
	.cfi_offset %r14, -16
	movzbl	(%rsi), %eax
	movq	%rdx, %r14
	movq	%rdi, %rbx
	cmpq	$49, %rax
	je	.LBB8_18
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	kill_value
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB8_18
# BB#2:                                 # %if.then4
	movl	8(%rbx), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	leaq	(%rdx,%rdx,2), %rcx
	movl	%eax, (%r14,%rcx,4)
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	je	.LBB8_6
# BB#3:                                 # %if.then4
	leaq	-21(%rdx), %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	cmpq	$8, %rsi
	jb	.LBB8_6
# BB#4:                                 # %if.then4
	leaq	-45(%rdx), %rsi
	andq	%rcx, %rsi
	cmpq	$8, %rsi
	jb	.LBB8_6
# BB#5:                                 # %if.then4
	addq	$-29, %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB8_6
# BB#11:                                # %cond.false.i
	cmpq	$24, %rax
	jne	.LBB8_12
# BB#14:                                # %cond.true25.i
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB8_16
.LBB8_6:                                # %cond.true.i
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB8_16
# BB#7:                                 # %lor.rhs.i
	cmpq	$6, %rcx
	jne	.LBB8_8
# BB#9:                                 # %lor.rhs.i
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB8_16
.LBB8_12:                               # %cond.false.i
	cmpq	$18, %rax
	jne	.LBB8_15
# BB#13:                                # %cond.true21.i
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB8_16
.LBB8_8:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB8_16
.LBB8_15:                               # %cond.false29.i
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB8_16:                               # %cond.end41.i
	movl	636(%r14), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB8_18
# BB#17:                                # %if.then.i
	movl	%eax, 636(%r14)
.LBB8_18:                               # %if.end7
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp124:
	.size	kill_set_value, .Ltmp124-kill_set_value
	.cfi_endproc

	.align	16, 0x90
	.type	scan_rtx,@function
scan_rtx:                               # @scan_rtx
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
	subq	$48, %rsp
.Ltmp136:
	.cfi_def_cfa_offset 96
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
	movq	%r9, 16(%rsp)           # 8-byte Spill
	movq	%r8, %rbx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, %r15
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	24(%rsp), %r12          # 8-byte Reload
	jmp	.LBB9_1
	.align	16, 0x90
.LBB9_17:                               # %sw.bb18
                                        #   in Loop: Header=BB9_1 Depth=1
	xorq	$1, %r8
	addq	%r8, %r8
	movq	32(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	movq	%r12, %rdx
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rcx
	movq	16(%rsp), %r9           # 8-byte Reload
	callq	scan_rtx
	leaq	16(%r13), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	xorq	%rbx, %rbx
	movq	%r15, %rdi
	movq	%r12, %rdx
	movq	%r14, %rcx
	callq	scan_rtx
	addq	$24, %r13
	movq	%r13, %r15
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
.LBB9_1:                                # %tailrecurse.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_2 Depth 2
	movq	(%r15), %r13
.LBB9_2:                                # %tailrecurse
                                        #   Parent Loop BB9_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r13), %eax
	cmpq	$46, %rax
	jg	.LBB9_6
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB9_2 Depth=2
	cmpq	$3, %rax
	jne	.LBB9_21
# BB#4:                                 # %sw.bb33
                                        #   in Loop: Header=BB9_2 Depth=2
	leaq	8(%r13), %rsi
	xorq	%r9, %r9
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, %r8
	callq	scan_rtx
	movq	16(%r13), %rax
	movq	%r13, %r15
	addq	$16, %r15
	testq	%rax, %rax
	movq	%rax, %r13
	jne	.LBB9_2
	jmp	.LBB9_5
	.align	16, 0x90
.LBB9_6:                                # %tailrecurse
                                        #   in Loop: Header=BB9_1 Depth=1
	cmpq	$131, %rax
	jg	.LBB9_14
# BB#7:                                 # %tailrecurse
                                        #   in Loop: Header=BB9_1 Depth=1
	cmpq	$95, %rax
	jg	.LBB9_18
# BB#8:                                 # %tailrecurse
                                        #   in Loop: Header=BB9_1 Depth=1
	leaq	-47(%rax), %rcx
	andq	%r14, %rcx
	cmpq	$22, %rcx
	ja	.LBB9_21
# BB#9:                                 # %tailrecurse
                                        #   in Loop: Header=BB9_1 Depth=1
	jmpq	*.LJTI9_0(,%rcx,8)
.LBB9_12:                               # %sw.bb7
                                        #   in Loop: Header=BB9_1 Depth=1
	leaq	8(%r13), %r15
	addq	$16, %r13
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	scan_rtx
	movq	$1, %rbx
	jmp	.LBB9_1
	.align	16, 0x90
.LBB9_14:                               # %tailrecurse
                                        #   in Loop: Header=BB9_1 Depth=1
	leaq	-132(%rax), %rcx
	andq	%r14, %rcx
	cmpq	$2, %rcx
	jae	.LBB9_21
# BB#15:                                # %sw.bb18
                                        #   in Loop: Header=BB9_1 Depth=1
	leaq	8(%r13), %rsi
	testq	%r14, %rbx
	movabsq	$0, %r8
	jne	.LBB9_17
# BB#16:                                # %sw.bb18
                                        #   in Loop: Header=BB9_1 Depth=1
	movabsq	$1, %r8
	jmp	.LBB9_17
.LBB9_20:                               # %sw.bb29
                                        #   in Loop: Header=BB9_1 Depth=1
	addq	$8, %r13
	movq	$1, %rbx
	movq	%r13, %r15
	movq	$1, 16(%rsp)            # 8-byte Folded Spill
	jmp	.LBB9_1
.LBB9_13:                               # %sw.bb14
                                        #   in Loop: Header=BB9_1 Depth=1
	addq	$8, %r13
	movq	$2, %rbx
	movq	%r13, %r15
	jmp	.LBB9_1
.LBB9_18:                               # %tailrecurse
	leaq	-96(%rax), %rcx
	andq	%r14, %rcx
	cmpq	$6, %rcx
	jb	.LBB9_19
.LBB9_21:                               # %sw.epilog
	movzbl	rtx_length(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB9_5
# BB#22:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB9_23:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_29 Depth 2
	movq	%rdx, %r14
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	-1(%rax,%r14), %eax
	leaq	-1(%r14), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB9_24
# BB#27:                                # %if.then63
                                        #   in Loop: Header=BB9_23 Depth=1
	movq	(%r13,%r14,8), %rsi
	movslq	(%rsi), %r12
	testq	%r12, %r12
	jle	.LBB9_25
# BB#28:                                # %for.body71.lr.ph
                                        #   in Loop: Header=BB9_23 Depth=1
	movq	$3, %rcx
	leaq	-1(%r12), %r15
	shlq	%cl, %r12
	jmp	.LBB9_29
	.align	16, 0x90
.LBB9_30:                               # %for.body71.for.body71_crit_edge
                                        #   in Loop: Header=BB9_29 Depth=2
	movq	(%r13,%r14,8), %rsi
	decq	%r15
	addq	$-8, %r12
.LBB9_29:                               # %for.body71
                                        #   Parent Loop BB9_23 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	addq	%r12, %rsi
	xorq	%r9, %r9
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, %r8
	callq	scan_rtx
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB9_30
	jmp	.LBB9_25
	.align	16, 0x90
.LBB9_24:                               # %for.body
                                        #   in Loop: Header=BB9_23 Depth=1
	cmpq	$101, %rax
	jne	.LBB9_25
# BB#26:                                # %if.then53
                                        #   in Loop: Header=BB9_23 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	leaq	8(%r13,%rax,8), %rsi
	xorq	%r9, %r9
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, %r8
	callq	scan_rtx
.LBB9_25:                               # %for.cond.backedge
                                        #   in Loop: Header=BB9_23 Depth=1
	movq	$32, %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB9_23
.LBB9_5:                                # %for.end82
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB9_10:                               # %sw.bb3
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, %r8
	movq	16(%rsp), %r9           # 8-byte Reload
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	scan_rtx_reg  # TAILCALL
.LBB9_11:                               # %sw.bb4
	addq	$8, %r13
	movq	$12, %rdx
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	40(%rsp), %rcx          # 8-byte Reload
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	scan_rtx_address  # TAILCALL
.LBB9_19:                               # %sw.bb28
	movabsq	$.L.str40, %rax
	movabsq	$.L__FUNCTION__.scan_rtx, %rcx
	movq	$722, %rsi              # imm = 0x2D2
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp142:
	.size	scan_rtx, .Ltmp142-scan_rtx
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI9_0:
	.quad	.LBB9_12
	.quad	.LBB9_21
	.quad	.LBB9_20
	.quad	.LBB9_21
	.quad	.LBB9_21
	.quad	.LBB9_21
	.quad	.LBB9_21
	.quad	.LBB9_5
	.quad	.LBB9_5
	.quad	.LBB9_5
	.quad	.LBB9_21
	.quad	.LBB9_5
	.quad	.LBB9_5
	.quad	.LBB9_21
	.quad	.LBB9_10
	.quad	.LBB9_21
	.quad	.LBB9_21
	.quad	.LBB9_13
	.quad	.LBB9_21
	.quad	.LBB9_11
	.quad	.LBB9_5
	.quad	.LBB9_5
	.quad	.LBB9_5

	.text
	.align	16, 0x90
	.type	scan_rtx_address,@function
scan_rtx_address:                       # @scan_rtx_address
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
	subq	$32, %rsp
.Ltmp154:
	.cfi_def_cfa_offset 80
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
	movq	%rcx, %r12
	movq	%rdx, %rbx
	movq	%rsi, %r15
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%r12, %rax
	andq	%r13, %rax
	cmpq	$5, %rax
	je	.LBB10_66
# BB#1:
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	jmp	.LBB10_2
	.align	16, 0x90
.LBB10_67:                              # %sw.bb496
                                        #   in Loop: Header=BB10_2 Depth=1
	addq	$8, %r14
	movq	$12, %rbx
	movq	%r14, %r15
.LBB10_2:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r15), %r14
	movzbl	(%r14), %eax
	cmpq	$95, %rax
	jg	.LBB10_68
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB10_2 Depth=1
	cmpq	$66, %rax
	je	.LBB10_67
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB10_2 Depth=1
	cmpq	$75, %rax
	jne	.LBB10_5
# BB#7:                                 # %sw.bb
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	8(%r14), %rax
	movq	16(%r14), %r9
	movq	%r14, %rdx
	movzbl	(%rax), %ebx
	movl	(%r9), %edi
	cmpq	$63, %rbx
	jne	.LBB10_9
# BB#8:                                 # %if.then11
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	8(%rax), %rax
	movzbl	(%rax), %ebx
.LBB10_9:                               # %if.end17
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB10_11
# BB#10:                                # %if.then21
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	8(%r9), %r9
	movl	(%r9), %edi
.LBB10_11:                              # %if.end27
                                        #   in Loop: Header=BB10_2 Depth=1
	leaq	8(%rdx), %r8
	leaq	16(%rdx), %rsi
	movq	%rdx, %r10
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$66, %rdi
	je	.LBB10_12
# BB#13:                                # %switch.early.test
                                        #   in Loop: Header=BB10_2 Depth=1
	leaq	-78(%rbx), %rcx
	andq	%r13, %rcx
	cmpq	$44, %rcx
	ja	.LBB10_15
# BB#14:                                # %switch.early.test
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$30786325577729, %rcx   # imm = 0x1C0000000001
	testq	%rcx, %rdx
	je	.LBB10_15
.LBB10_12:                              #   in Loop: Header=BB10_2 Depth=1
	movq	%rsi, %r15
	movq	%r8, %rsi
	jmp	.LBB10_63
.LBB10_15:                              # %if.else
                                        #   in Loop: Header=BB10_2 Depth=1
	cmpq	$66, %rbx
	jne	.LBB10_17
# BB#16:                                #   in Loop: Header=BB10_2 Depth=1
	movq	%r8, %r15
	jmp	.LBB10_63
.LBB10_17:                              # %switch.early.test4
                                        #   in Loop: Header=BB10_2 Depth=1
	leaq	-78(%rdi), %rcx
	andq	%r13, %rcx
	cmpq	$44, %rcx
	ja	.LBB10_20
# BB#18:                                # %switch.early.test4
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$30786325577729, %rcx   # imm = 0x1C0000000001
	testq	%rcx, %rdx
	je	.LBB10_20
# BB#19:                                #   in Loop: Header=BB10_2 Depth=1
	movq	%r8, %r15
	jmp	.LBB10_63
.LBB10_20:                              # %if.else59
                                        #   in Loop: Header=BB10_2 Depth=1
	leaq	-54(%rbx), %rcx
	andq	%r13, %rcx
	cmpq	$14, %rcx
	ja	.LBB10_22
# BB#21:                                # %if.else59
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24593, %rdx            # imm = 0x6011
	movq	%rsi, %r15
	jne	.LBB10_65
.LBB10_22:                              # %if.else71
                                        #   in Loop: Header=BB10_2 Depth=1
	leaq	-54(%rdi), %rcx
	andq	%r13, %rcx
	cmpq	$14, %rcx
	ja	.LBB10_24
# BB#23:                                # %if.else71
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24593, %rdx            # imm = 0x6011
	movq	%r8, %r15
	jne	.LBB10_65
.LBB10_24:                              # %if.else83
                                        #   in Loop: Header=BB10_2 Depth=1
	cmpq	$61, %rbx
	jne	.LBB10_59
# BB#25:                                # %if.else83
                                        #   in Loop: Header=BB10_2 Depth=1
	cmpq	$61, %rdi
	jne	.LBB10_59
# BB#26:                                # %if.then86
                                        #   in Loop: Header=BB10_2 Depth=1
	movl	8(%rax), %r8d
	cmpq	$44, %r8
	ja	.LBB10_28
# BB#27:                                # %if.then86
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$1, %rdx
	movq	%r8, %rcx
	shlq	%cl, %rdx
	movabsq	$35046933135487, %rax   # imm = 0x1FE00000007F
	testq	%rax, %rdx
	jne	.LBB10_31
.LBB10_28:                              # %lor.lhs.false100
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	reg_renumber(%rip), %rbx
	movzbl	(%rbx,%r8,2), %ecx
	cmpq	$44, %rcx
	ja	.LBB10_30
# BB#29:                                # %lor.lhs.false100
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$35046933135487, %rax   # imm = 0x1FE00000007F
	testq	%rax, %rdx
	je	.LBB10_30
.LBB10_31:                              # %land.lhs.true125
                                        #   in Loop: Header=BB10_2 Depth=1
	movl	8(%r9), %esi
	xorq	%rdi, %rdi
	cmpq	$20, %rsi
	ja	.LBB10_33
# BB#32:                                # %land.lhs.true125
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$1, %rdx
	movq	%rsi, %rcx
	shlq	%cl, %rdx
	testq	$1114367, %rdx          # imm = 0x1100FF
	jne	.LBB10_58
.LBB10_33:                              # %lor.lhs.false143
                                        #   in Loop: Header=BB10_2 Depth=1
	leaq	-37(%rsi), %rcx
	andq	%r13, %rcx
	cmpq	$8, %rcx
	jb	.LBB10_58
# BB#34:                                # %lor.lhs.false155
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	reg_renumber(%rip), %rbx
	movzbl	(%rbx,%rsi,2), %ecx
	cmpq	$44, %rcx
	ja	.LBB10_36
# BB#35:                                # %lor.lhs.false155
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$35046933135615, %rax   # imm = 0x1FE0000000FF
	testq	%rax, %rdx
	jne	.LBB10_58
	jmp	.LBB10_36
.LBB10_59:                              # %if.else462
                                        #   in Loop: Header=BB10_2 Depth=1
	cmpq	$61, %rbx
	je	.LBB10_12
# BB#60:                                # %if.else472
                                        #   in Loop: Header=BB10_2 Depth=1
	cmpq	$61, %rdi
	je	.LBB10_62
# BB#61:                                # %if.else472
                                        #   in Loop: Header=BB10_2 Depth=1
	movabsq	$0, %r8
.LBB10_62:                              # %if.else472
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	%r8, %r15
	jne	.LBB10_65
	jmp	.LBB10_63
.LBB10_30:                              # %lor.lhs.false100.if.else183_crit_edge
                                        #   in Loop: Header=BB10_2 Depth=1
	movl	8(%r9), %esi
.LBB10_36:                              # %if.else183
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	%rsi, %rdx
	andq	%r13, %rdx
	cmpq	$44, %rdx
	ja	.LBB10_38
# BB#37:                                # %if.else183
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$1, %rdi
	movq	%rdx, %rcx
	shlq	%cl, %rdi
	movabsq	$35046933135487, %rax   # imm = 0x1FE00000007F
	testq	%rax, %rdi
	jne	.LBB10_40
.LBB10_38:                              # %lor.lhs.false201
                                        #   in Loop: Header=BB10_2 Depth=1
	movzbl	(%rbx,%rdx,2), %ecx
	cmpq	$44, %rcx
	ja	.LBB10_45
# BB#39:                                # %lor.lhs.false201
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$1, %rdi
	shlq	%cl, %rdi
	movabsq	$35046933135487, %rax   # imm = 0x1FE00000007F
	testq	%rax, %rdi
	je	.LBB10_45
.LBB10_40:                              # %land.lhs.true228
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$1, %rdi
	cmpq	$20, %r8
	ja	.LBB10_42
# BB#41:                                # %land.lhs.true228
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$1, %rax
	movq	%r8, %rcx
	shlq	%cl, %rax
	testq	$1114367, %rax          # imm = 0x1100FF
	jne	.LBB10_58
.LBB10_42:                              # %lor.lhs.false246
                                        #   in Loop: Header=BB10_2 Depth=1
	leaq	-37(%r8), %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	jb	.LBB10_58
# BB#43:                                # %lor.lhs.false258
                                        #   in Loop: Header=BB10_2 Depth=1
	movzbl	(%rbx,%r8,2), %ecx
	cmpq	$44, %rcx
	ja	.LBB10_45
# BB#44:                                # %lor.lhs.false258
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$35046933135615, %rcx   # imm = 0x1FE0000000FF
	testq	%rcx, %rax
	jne	.LBB10_58
.LBB10_45:                              # %if.else286
                                        #   in Loop: Header=BB10_2 Depth=1
	xorq	%rdi, %rdi
	cmpq	$20, %rdx
	ja	.LBB10_47
# BB#46:                                # %if.else286
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$1, %rax
	movq	%rdx, %rcx
	shlq	%cl, %rax
	testq	$1114367, %rax          # imm = 0x1100FF
	jne	.LBB10_58
.LBB10_47:                              # %lor.lhs.false304
                                        #   in Loop: Header=BB10_2 Depth=1
	addq	$-37, %rsi
	andq	%r13, %rsi
	cmpq	$8, %rsi
	jb	.LBB10_58
# BB#48:                                # %lor.lhs.false316
                                        #   in Loop: Header=BB10_2 Depth=1
	movzbl	(%rbx,%rdx,2), %esi
	cmpq	$44, %rsi
	ja	.LBB10_50
# BB#49:                                # %lor.lhs.false316
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$1, %rax
	movq	%rsi, %rcx
	shlq	%cl, %rax
	movabsq	$35046933135615, %rcx   # imm = 0x1FE0000000FF
	testq	%rcx, %rax
	jne	.LBB10_58
.LBB10_50:                              # %if.else344
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$1, %rdi
	cmpq	$20, %r8
	ja	.LBB10_52
# BB#51:                                # %if.else344
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$1, %rax
	movq	%r8, %rcx
	shlq	%cl, %rax
	testq	$1114367, %rax          # imm = 0x1100FF
	jne	.LBB10_58
.LBB10_52:                              # %lor.lhs.false362
                                        #   in Loop: Header=BB10_2 Depth=1
	leaq	-37(%r8), %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	jb	.LBB10_58
# BB#53:                                # %lor.lhs.false374
                                        #   in Loop: Header=BB10_2 Depth=1
	movzbl	(%rbx,%r8,2), %eax
	leaq	-37(%rax), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB10_58
# BB#54:                                # %lor.lhs.false374
                                        #   in Loop: Header=BB10_2 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$8, %rax
	jb	.LBB10_58
# BB#55:                                # %lor.lhs.false374
                                        #   in Loop: Header=BB10_2 Depth=1
	cmpq	$7, %rdx
	jb	.LBB10_58
# BB#56:                                # %lor.lhs.false374
                                        #   in Loop: Header=BB10_2 Depth=1
	leaq	-37(%rsi), %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$8, %rax
	jb	.LBB10_58
# BB#57:                                # %lor.lhs.false438
                                        #   in Loop: Header=BB10_2 Depth=1
	cmpq	$7, %rsi
	sbbq	%rdi, %rdi
	andq	$1, %rdi
.LBB10_58:                              # %if.end453
                                        #   in Loop: Header=BB10_2 Depth=1
	leaq	8(%r10,%rdi,8), %rsi
	xorq	$1, %rdi
	leaq	8(%r10,%rdi,8), %r15
	.align	16, 0x90
.LBB10_63:                              # %if.end488
                                        #   in Loop: Header=BB10_2 Depth=1
	testq	%rsi, %rsi
	je	.LBB10_65
# BB#64:                                # %if.then490
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$10, %rdx
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rcx
	callq	scan_rtx_address
.LBB10_65:                              # %if.end491
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$12, %rbx
	testq	%r15, %r15
	jne	.LBB10_2
	jmp	.LBB10_66
.LBB10_68:                              # %if.end
	leaq	-96(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$6, %rcx
	jae	.LBB10_69
.LBB10_70:                              # %sw.epilog
	movzbl	rtx_length(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB10_66
# BB#71:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	.align	16, 0x90
.LBB10_72:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB10_78 Depth 2
	movq	%rdx, %r15
	movq	(%rsp), %rax            # 8-byte Reload
	movzbl	-1(%rax,%r15), %eax
	leaq	-1(%r15), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB10_73
# BB#76:                                # %if.then526
                                        #   in Loop: Header=BB10_72 Depth=1
	movq	(%r14,%r15,8), %rsi
	movslq	(%rsi), %r12
	testq	%r12, %r12
	jle	.LBB10_74
# BB#77:                                # %for.body534.lr.ph
                                        #   in Loop: Header=BB10_72 Depth=1
	movq	$3, %rcx
	leaq	-1(%r12), %r13
	shlq	%cl, %r12
	jmp	.LBB10_78
	.align	16, 0x90
.LBB10_79:                              # %for.body534.for.body534_crit_edge
                                        #   in Loop: Header=BB10_78 Depth=2
	movq	(%r14,%r15,8), %rsi
	decq	%r13
	addq	$-8, %r12
.LBB10_78:                              # %for.body534
                                        #   Parent Loop BB10_72 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	addq	%r12, %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	16(%rsp), %rcx          # 8-byte Reload
	callq	scan_rtx_address
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB10_79
	jmp	.LBB10_74
	.align	16, 0x90
.LBB10_73:                              # %for.body
                                        #   in Loop: Header=BB10_72 Depth=1
	cmpq	$101, %rax
	jne	.LBB10_74
# BB#75:                                # %if.then515
                                        #   in Loop: Header=BB10_72 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	leaq	8(%r14,%rax,8), %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	16(%rsp), %rcx          # 8-byte Reload
	callq	scan_rtx_address
.LBB10_74:                              # %for.cond.backedge
                                        #   in Loop: Header=BB10_72 Depth=1
	movq	$32, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB10_72
.LBB10_66:                              # %for.end545
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB10_5:                               # %if.end
	cmpq	$61, %rax
	jne	.LBB10_69
# BB#6:                                 # %sw.bb502
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	scan_rtx_reg  # TAILCALL
.LBB10_69:                              # %sw.epilog.loopexit
	movq	%r12, 16(%rsp)          # 8-byte Spill
	jmp	.LBB10_70
.Ltmp160:
	.size	scan_rtx_address, .Ltmp160-scan_rtx_address
	.cfi_endproc

	.align	16, 0x90
	.type	scan_rtx_reg,@function
scan_rtx_reg:                           # @scan_rtx_reg
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
	subq	$64, %rsp
.Ltmp172:
	.cfi_def_cfa_offset 112
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
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	(%rsi), %rax
	movl	8(%rax), %r10d
	movq	%r9, %r14
	movq	%rcx, %rbx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movzbl	2(%rax), %eax
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	movq	%r10, %rcx
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	je	.LBB11_4
# BB#1:                                 # %entry
	leaq	-21(%r10), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB11_4
# BB#2:                                 # %entry
	leaq	-45(%r10), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB11_4
# BB#3:                                 # %entry
	leaq	-29(%r10), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB11_4
# BB#9:                                 # %cond.false
	cmpq	$24, %rax
	jne	.LBB11_10
# BB#12:                                # %cond.true23
	movzbl	target_flags+3(%rip), %r11d
	andq	$2, %r11
	xorq	$6, %r11
	jmp	.LBB11_14
.LBB11_4:                               # %cond.true
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %r11
	je	.LBB11_14
# BB#5:                                 # %lor.rhs
	cmpq	$6, %rax
	jne	.LBB11_6
# BB#7:                                 # %lor.rhs
	movabsq	$1, %r11
	incq	%r11
	jmp	.LBB11_14
.LBB11_10:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB11_13
# BB#11:                                # %cond.true19
	movl	target_flags(%rip), %r11d
	movq	$25, %rcx
	shrq	%cl, %r11
	andq	$1, %r11
	xorq	$3, %r11
	jmp	.LBB11_14
.LBB11_6:
	movabsq	$0, %r11
	incq	%r11
	jmp	.LBB11_14
.LBB11_13:                              # %cond.false27
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %r11
.LBB11_14:                              # %cond.end39
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%rbx, %rax
	andq	%r15, %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	cmpq	$5, %rax
	jne	.LBB11_23
# BB#15:                                # %if.then
	andq	%r15, %r8
	cmpq	$1, %r8
	jne	.LBB11_108
# BB#16:                                # %if.then45
	movq	rename_obstack+32(%rip), %rax
	movq	rename_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$39, %rax
	jg	.LBB11_18
# BB#17:                                # %if.then49
	movabsq	$rename_obstack, %rdi
	movq	$40, %rsi
	callq	_obstack_newchunk
	movq	rename_obstack+24(%rip), %rcx
.LBB11_18:                              # %if.end
	addq	$40, %rcx
	movq	%rcx, rename_obstack+24(%rip)
	movq	rename_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB11_20
# BB#19:                                # %if.then56
	movzbl	rename_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, rename_obstack+80(%rip)
.LBB11_20:                              # %if.end59
	movslq	rename_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, rename_obstack+24(%rip)
	movq	rename_obstack+32(%rip), %rbx
	movq	rename_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB11_22
# BB#21:                                # %if.then81
	movq	%rbx, rename_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB11_22:                              # %if.end84
	movq	%rdx, rename_obstack+16(%rip)
	movq	$0, 8(%rax)
	movq	open_chains(%rip), %rcx
	movq	%rcx, (%rax)
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 24(%rax)
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 16(%rax)
	movq	40(%rsp), %rcx          # 8-byte Reload
	movl	%ecx, 32(%rax)
	movzbl	36(%rax), %ecx
	addq	%r14, %r14
	andq	$2, %r14
	andq	$252, %rcx
	orq	%r14, %rcx
	movb	%cl, 36(%rax)
	movq	%rax, open_chains(%rip)
	jmp	.LBB11_108
.LBB11_23:                              # %if.end98
	andq	%r15, %r8
	cmpq	$1, %r8
	jne	.LBB11_25
# BB#24:                                # %if.end98
	cmpq	$2, 56(%rsp)            # 8-byte Folded Reload
	jne	.LBB11_108
.LBB11_25:                              # %lor.lhs.false104
	cmpq	$1, %r8
	je	.LBB11_27
# BB#26:                                # %lor.lhs.false104
	cmpq	$2, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB11_108
.LBB11_27:                              # %for.cond.preheader
	movq	open_chains(%rip), %r12
	testq	%r12, %r12
	je	.LBB11_108
# BB#28:                                # %for.body.lr.ph.lr.ph
	leaq	(%r11,%r10), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	andq	%rbx, %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movabsq	$open_chains, %r14
	movabsq	$1, %r13
.LBB11_29:                              # %for.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB11_64 Depth 2
                                        #     Child Loop BB11_30 Depth 2
	cmpq	$4, 56(%rsp)            # 8-byte Folded Reload
	jne	.LBB11_64
	.align	16, 0x90
.LBB11_30:                              # %for.body.us
                                        #   Parent Loop BB11_29 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r12, %r8
	movq	24(%r8), %rax
	movq	(%rax), %rax
	movq	global_rtl+8(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB11_65
# BB#31:                                # %if.else.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movl	8(%rax), %r9d
	movzbl	2(%rax), %eax
	movq	%r9, %rcx
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	movabsq	$0, %rsi
	movq	%r13, %r12
	je	.LBB11_40
# BB#32:                                # %if.else.us
                                        #   in Loop: Header=BB11_30 Depth=2
	leaq	-21(%r9), %rcx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	jb	.LBB11_40
# BB#33:                                # %if.else.us
                                        #   in Loop: Header=BB11_30 Depth=2
	leaq	-45(%r9), %rcx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	jb	.LBB11_40
# BB#34:                                # %if.else.us
                                        #   in Loop: Header=BB11_30 Depth=2
	leaq	-29(%r9), %rcx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	jb	.LBB11_40
# BB#35:                                # %cond.false169.us
                                        #   in Loop: Header=BB11_30 Depth=2
	cmpq	$18, %rax
	jne	.LBB11_36
# BB#38:                                # %cond.true176.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB11_44
	.align	16, 0x90
.LBB11_40:                              # %cond.true149.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB11_44
# BB#41:                                # %lor.rhs158.us
                                        #   in Loop: Header=BB11_30 Depth=2
	cmpq	$6, %rcx
	movq	%rsi, %rax
	jne	.LBB11_43
# BB#42:                                # %lor.rhs158.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movq	%r12, %rax
.LBB11_43:                              # %lor.rhs158.us
                                        #   in Loop: Header=BB11_30 Depth=2
	incq	%rax
	jmp	.LBB11_44
.LBB11_36:                              # %cond.false169.us
                                        #   in Loop: Header=BB11_30 Depth=2
	cmpq	$24, %rax
	jne	.LBB11_39
# BB#37:                                # %cond.true187.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB11_44
.LBB11_39:                              # %cond.false191.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	.align	16, 0x90
.LBB11_44:                              # %cond.end212.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movq	$32, %rcx
	movq	%r10, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%r10, %r9
	movq	%rsi, %rdx
	jne	.LBB11_46
# BB#45:                                # %cond.end212.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movq	%r12, %rdx
.LBB11_46:                              # %cond.end212.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movq	%r11, %rcx
	andq	%r15, %rcx
	movq	%rax, %rbx
	andq	%r15, %rbx
	cmpq	%rcx, %rbx
	movq	%rsi, %rbx
	jne	.LBB11_48
# BB#47:                                # %cond.end212.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movq	%r12, %rbx
.LBB11_48:                              # %cond.end212.us
                                        #   in Loop: Header=BB11_30 Depth=2
	addq	%r9, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdi, %rax
	jle	.LBB11_66
# BB#49:                                # %cond.end212.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	32(%rsp), %rdi          # 8-byte Reload
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rax, %rdi
	jle	.LBB11_66
# BB#50:                                # %if.then231.us
                                        #   in Loop: Header=BB11_30 Depth=2
	andq	%rbx, %rdx
	je	.LBB11_94
# BB#51:                                # %if.end234.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%r15, %rax
	jne	.LBB11_95
# BB#52:                                # %if.end315.us
                                        #   in Loop: Header=BB11_30 Depth=2
	cmpq	$1, 56(%rsp)            # 8-byte Folded Reload
	movq	%rsi, %rax
	jne	.LBB11_54
# BB#53:                                # %if.end315.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movq	%r12, %rax
.LBB11_54:                              # %if.end315.us
                                        #   in Loop: Header=BB11_30 Depth=2
	testq	%rdx, %rax
	jne	.LBB11_66
# BB#55:                                # %if.then320.us
                                        #   in Loop: Header=BB11_30 Depth=2
	cmpq	$2, 24(%rsp)            # 8-byte Folded Reload
	movq	%rsi, %rax
	jne	.LBB11_57
# BB#56:                                # %if.then320.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movq	%r12, %rax
.LBB11_57:                              # %if.then320.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movq	%r12, %r13
	movq	(%r8), %r12
	movq	%r14, %rbx
	testq	%rdx, %rax
	je	.LBB11_58
# BB#60:                                # %if.then330.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movq	closed_chains(%rip), %rax
	movq	%rax, (%r8)
	movq	rtl_dump_file(%rip), %rdi
	movq	%r8, closed_chains(%rip)
	testq	%rdi, %rdi
	je	.LBB11_63
# BB#61:                                # %if.then333.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movq	24(%r8), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	movq	reg_names(,%rax,8), %rdx
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	scan_actions_name(,%rax,8), %r8
	movq	48(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %ecx
	xorq	%rax, %rax
	movabsq	$.L.str41, %rsi
	jmp	.LBB11_62
	.align	16, 0x90
.LBB11_58:                              # %if.else345.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB11_63
# BB#59:                                # %if.then347.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movq	reg_names(,%r9,8), %rdx
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	scan_actions_name(,%rax,8), %r8
	movq	48(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %ecx
	xorq	%rax, %rax
	movabsq	$.L.str42, %rsi
.LBB11_62:                              # %if.end361.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movq	%r10, %r15
	movq	%r11, %r14
	callq	fprintf
	movq	%r14, %r11
	movq	%r15, %r10
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
.LBB11_63:                              # %if.end361.us
                                        #   in Loop: Header=BB11_30 Depth=2
	movq	%rbx, %r14
	movq	%r12, (%r14)
	testq	%r12, %r12
	jne	.LBB11_30
	jmp	.LBB11_108
	.align	16, 0x90
.LBB11_64:                              # %for.body
                                        #   Parent Loop BB11_29 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r12, %r8
	movq	24(%r8), %rax
	movq	(%rax), %rax
	movq	global_rtl+8(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB11_65
# BB#67:                                # %if.else
                                        #   in Loop: Header=BB11_64 Depth=2
	movl	8(%rax), %r9d
	movzbl	2(%rax), %eax
	movq	%r9, %rcx
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	movabsq	$0, %rsi
	movq	%r13, %r12
	je	.LBB11_71
# BB#68:                                # %if.else
                                        #   in Loop: Header=BB11_64 Depth=2
	leaq	-21(%r9), %rcx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	jb	.LBB11_71
# BB#69:                                # %if.else
                                        #   in Loop: Header=BB11_64 Depth=2
	leaq	-45(%r9), %rcx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	jb	.LBB11_71
# BB#70:                                # %if.else
                                        #   in Loop: Header=BB11_64 Depth=2
	leaq	-29(%r9), %rcx
	andq	%r15, %rcx
	cmpq	$7, %rcx
	jbe	.LBB11_71
# BB#75:                                # %cond.false169
                                        #   in Loop: Header=BB11_64 Depth=2
	cmpq	$24, %rax
	jne	.LBB11_76
# BB#78:                                # %cond.true187
                                        #   in Loop: Header=BB11_64 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB11_80
	.align	16, 0x90
.LBB11_71:                              # %cond.true149
                                        #   in Loop: Header=BB11_64 Depth=2
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB11_80
# BB#72:                                # %lor.rhs158
                                        #   in Loop: Header=BB11_64 Depth=2
	cmpq	$6, %rcx
	movq	%rsi, %rax
	jne	.LBB11_74
# BB#73:                                # %lor.rhs158
                                        #   in Loop: Header=BB11_64 Depth=2
	movq	%r12, %rax
.LBB11_74:                              # %lor.rhs158
                                        #   in Loop: Header=BB11_64 Depth=2
	incq	%rax
	jmp	.LBB11_80
	.align	16, 0x90
.LBB11_76:                              # %cond.false169
                                        #   in Loop: Header=BB11_64 Depth=2
	cmpq	$18, %rax
	jne	.LBB11_79
# BB#77:                                # %cond.true176
                                        #   in Loop: Header=BB11_64 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB11_80
.LBB11_79:                              # %cond.false191
                                        #   in Loop: Header=BB11_64 Depth=2
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	.align	16, 0x90
.LBB11_80:                              # %cond.end212
                                        #   in Loop: Header=BB11_64 Depth=2
	movq	$32, %rcx
	movq	%r10, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%r10, %r9
	movq	%rsi, %rdx
	jne	.LBB11_82
# BB#81:                                # %cond.end212
                                        #   in Loop: Header=BB11_64 Depth=2
	movq	%r12, %rdx
.LBB11_82:                              # %cond.end212
                                        #   in Loop: Header=BB11_64 Depth=2
	movq	%r11, %rcx
	andq	%r15, %rcx
	movq	%rax, %rbx
	andq	%r15, %rbx
	cmpq	%rcx, %rbx
	movq	%rsi, %rbx
	jne	.LBB11_84
# BB#83:                                # %cond.end212
                                        #   in Loop: Header=BB11_64 Depth=2
	movq	%r12, %rbx
.LBB11_84:                              # %cond.end212
                                        #   in Loop: Header=BB11_64 Depth=2
	addq	%r9, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdi, %rax
	jle	.LBB11_66
# BB#85:                                # %cond.end212
                                        #   in Loop: Header=BB11_64 Depth=2
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	32(%rsp), %rdi          # 8-byte Reload
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rax, %rdi
	jle	.LBB11_66
# BB#86:                                # %if.end315
                                        #   in Loop: Header=BB11_64 Depth=2
	andq	%rbx, %rdx
	cmpq	$1, 56(%rsp)            # 8-byte Folded Reload
	movq	%rsi, %rax
	jne	.LBB11_88
# BB#87:                                # %if.end315
                                        #   in Loop: Header=BB11_64 Depth=2
	movq	%r12, %rax
.LBB11_88:                              # %if.end315
                                        #   in Loop: Header=BB11_64 Depth=2
	testq	%rdx, %rax
	jne	.LBB11_66
# BB#89:                                # %if.then320
                                        #   in Loop: Header=BB11_64 Depth=2
	cmpq	$2, 24(%rsp)            # 8-byte Folded Reload
	movq	%rsi, %rax
	jne	.LBB11_91
# BB#90:                                # %if.then320
                                        #   in Loop: Header=BB11_64 Depth=2
	movq	%r12, %rax
.LBB11_91:                              # %if.then320
                                        #   in Loop: Header=BB11_64 Depth=2
	movq	%r12, %r13
	movq	(%r8), %r12
	andq	%rdx, %rax
	cmpq	$1, %rax
	jne	.LBB11_104
# BB#92:                                # %if.then330
                                        #   in Loop: Header=BB11_64 Depth=2
	movq	closed_chains(%rip), %rax
	movq	%rax, (%r8)
	movq	rtl_dump_file(%rip), %rdi
	movq	%r8, closed_chains(%rip)
	testq	%rdi, %rdi
	je	.LBB11_107
# BB#93:                                # %if.then333
                                        #   in Loop: Header=BB11_64 Depth=2
	movq	24(%r8), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	movq	reg_names(,%rax,8), %rdx
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	scan_actions_name(,%rax,8), %r8
	movq	48(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %ecx
	xorq	%rax, %rax
	movabsq	$.L.str41, %rsi
	jmp	.LBB11_106
	.align	16, 0x90
.LBB11_104:                             # %if.else345
                                        #   in Loop: Header=BB11_64 Depth=2
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB11_107
# BB#105:                               # %if.then347
                                        #   in Loop: Header=BB11_64 Depth=2
	movq	reg_names(,%r9,8), %rdx
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	scan_actions_name(,%rax,8), %r8
	movq	48(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %ecx
	xorq	%rax, %rax
	movabsq	$.L.str42, %rsi
.LBB11_106:                             # %if.end361
                                        #   in Loop: Header=BB11_64 Depth=2
	movq	%r10, %rbx
	movq	%r11, %r15
	callq	fprintf
	movq	%r15, %r11
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%rbx, %r10
.LBB11_107:                             # %if.end361
                                        #   in Loop: Header=BB11_64 Depth=2
	movq	%r12, (%r14)
	testq	%r12, %r12
	jne	.LBB11_64
	jmp	.LBB11_108
.LBB11_65:                              #   in Loop: Header=BB11_29 Depth=1
	movq	%r13, %r12
.LBB11_66:                              # %for.cond.outer.backedge
                                        #   in Loop: Header=BB11_29 Depth=1
	movq	%r12, %r13
	movq	(%r8), %r12
	testq	%r12, %r12
	movq	%r8, %r14
	jne	.LBB11_29
	jmp	.LBB11_108
.LBB11_95:                              # %if.then237
	movq	rename_obstack+32(%rip), %rax
	movq	rename_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$39, %rax
	jg	.LBB11_97
# BB#96:                                # %if.then252
	movabsq	$rename_obstack, %rdi
	movq	$40, %rsi
	callq	_obstack_newchunk
	movq	rename_obstack+24(%rip), %rcx
.LBB11_97:                              # %if.end253
	addq	$40, %rcx
	movq	%rcx, rename_obstack+24(%rip)
	movq	rename_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB11_99
# BB#98:                                # %if.then266
	movzbl	rename_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, rename_obstack+80(%rip)
.LBB11_99:                              # %if.end271
	movslq	rename_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, rename_obstack+24(%rip)
	movq	rename_obstack+32(%rip), %rbx
	movq	rename_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB11_101
# BB#100:                               # %if.then296
	movq	%rbx, rename_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB11_101:                             # %if.end299
	movq	%rdx, rename_obstack+16(%rip)
	movq	$0, 8(%rax)
	movq	(%r14), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, (%rax)
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 24(%rax)
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 16(%rax)
	movq	40(%rsp), %rcx          # 8-byte Reload
	movl	%ecx, 32(%rax)
	movzbl	36(%rax), %ecx
	andq	$254, %rcx
	movb	%cl, 36(%rax)
	.align	16, 0x90
.LBB11_102:                             # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rcx
	movq	(%rcx), %rdx
	testq	%rdx, %rdx
	leaq	8(%rdx), %r14
	jne	.LBB11_102
# BB#103:                               # %while.end
	movq	%rax, (%rcx)
.LBB11_108:                             # %for.end
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB11_94:                              # %if.then233
	movabsq	$.L.str40, %rax
	movabsq	$.L__FUNCTION__.scan_rtx_reg, %rcx
	movq	$463, %rsi              # imm = 0x1CF
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp178:
	.size	scan_rtx_reg, .Ltmp178-scan_rtx_reg
	.cfi_endproc

	.align	16, 0x90
	.type	clear_dead_regs,@function
clear_dead_regs:                        # @clear_dead_regs
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
.Ltmp190:
	.cfi_offset %rbx, -48
.Ltmp191:
	.cfi_offset %r12, -40
.Ltmp192:
	.cfi_offset %r13, -32
.Ltmp193:
	.cfi_offset %r14, -24
.Ltmp194:
	.cfi_offset %r15, -16
	movq	%rdx, %r8
	testq	%r8, %r8
	je	.LBB12_23
# BB#1:
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	andq	%r9, %rsi
	movabsq	$4294967288, %r10       # imm = 0xFFFFFFF8
	movabsq	$0, %r11
	movabsq	$1, %r14
	.align	16, 0x90
.LBB12_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB12_20 Depth 2
	movzbl	2(%r8), %eax
	cmpq	%rsi, %rax
	jne	.LBB12_22
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	8(%r8), %rcx
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB12_22
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB12_2 Depth=1
	movl	8(%rcx), %r15d
	movq	%r15, %rdx
	andq	%r10, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB12_8
# BB#5:                                 # %if.then
                                        #   in Loop: Header=BB12_2 Depth=1
	leaq	-21(%r15), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_8
# BB#6:                                 # %if.then
                                        #   in Loop: Header=BB12_2 Depth=1
	leaq	-45(%r15), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_8
# BB#7:                                 # %if.then
                                        #   in Loop: Header=BB12_2 Depth=1
	leaq	-29(%r15), %rcx
	andq	%r9, %rcx
	cmpq	$7, %rcx
	jbe	.LBB12_8
# BB#12:                                # %cond.false
                                        #   in Loop: Header=BB12_2 Depth=1
	cmpq	$24, %rax
	jne	.LBB12_13
# BB#15:                                # %cond.true46
                                        #   in Loop: Header=BB12_2 Depth=1
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB12_17
.LBB12_8:                               # %cond.true
                                        #   in Loop: Header=BB12_2 Depth=1
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB12_17
# BB#9:                                 # %lor.rhs
                                        #   in Loop: Header=BB12_2 Depth=1
	cmpq	$6, %rcx
	movq	%r11, %rax
	jne	.LBB12_11
# BB#10:                                # %lor.rhs
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%r14, %rax
.LBB12_11:                              # %lor.rhs
                                        #   in Loop: Header=BB12_2 Depth=1
	incq	%rax
	jmp	.LBB12_17
.LBB12_13:                              # %cond.false
                                        #   in Loop: Header=BB12_2 Depth=1
	cmpq	$18, %rax
	jne	.LBB12_16
# BB#14:                                # %cond.true38
                                        #   in Loop: Header=BB12_2 Depth=1
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB12_17
.LBB12_16:                              # %cond.false50
                                        #   in Loop: Header=BB12_2 Depth=1
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
	.align	16, 0x90
.LBB12_17:                              # %cond.end65
                                        #   in Loop: Header=BB12_2 Depth=1
	leaq	(%rax,%r15), %rcx
	andq	%r9, %rcx
	cmpq	$53, %rcx
	ja	.LBB12_24
# BB#18:                                # %while.cond.preheader
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB12_22
# BB#19:                                # %while.body.lr.ph
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	(%rdi), %rbx
	.align	16, 0x90
.LBB12_20:                              # %while.body
                                        #   Parent Loop BB12_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %r12
	leaq	-1(%rax,%r15), %r13
	leaq	-1(%rax), %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rbx
	movq	%r13, %rcx
	shlq	%cl, %rbx
	notq	%rbx
	andq	%r12, %rbx
	testq	%rdx, %rdx
	jg	.LBB12_20
# BB#21:                                # %while.cond.for.inc.loopexit_crit_edge
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%rbx, (%rdi)
	.align	16, 0x90
.LBB12_22:                              # %for.inc
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	16(%r8), %r8
	testq	%r8, %r8
	jne	.LBB12_2
.LBB12_23:                              # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB12_24:                              # %if.then70
	movabsq	$.L.str40, %rax
	movabsq	$.L__FUNCTION__.clear_dead_regs, %rcx
	movq	$144, %rsi
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp195:
	.size	clear_dead_regs, .Ltmp195-clear_dead_regs
	.cfi_endproc

	.align	16, 0x90
	.type	note_sets,@function
note_sets:                              # @note_sets
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp197:
	.cfi_def_cfa_offset 16
	movl	(%rdi), %eax
	movq	%rdx, %r8
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB13_20
# BB#1:                                 # %if.end
	movl	8(%rdi), %r9d
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%r9, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB13_5
# BB#2:                                 # %if.end
	leaq	-21(%r9), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB13_5
# BB#3:                                 # %if.end
	leaq	-45(%r9), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB13_5
# BB#4:                                 # %if.end
	leaq	-29(%r9), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB13_5
# BB#10:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB13_11
# BB#13:                                # %cond.true35
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB13_15
.LBB13_5:                               # %cond.true
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB13_15
# BB#6:                                 # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB13_7
# BB#8:                                 # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB13_15
.LBB13_11:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB13_14
# BB#12:                                # %cond.true28
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB13_15
.LBB13_7:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB13_15
.LBB13_14:                              # %cond.false39
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB13_15:                              # %cond.end54
	leaq	(%rax,%r9), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$53, %rdx
	ja	.LBB13_21
# BB#16:                                # %while.cond.preheader
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB13_20
# BB#17:                                # %while.body.lr.ph
	movq	(%r8), %rsi
	.align	16, 0x90
.LBB13_18:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %r10
	leaq	-1(%rax,%r9), %rdx
	leaq	-1(%rax), %rax
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$1, %rsi
	movq	%rdx, %rcx
	shlq	%cl, %rsi
	orq	%r10, %rsi
	testq	%rdi, %rdi
	jg	.LBB13_18
# BB#19:                                # %while.cond.while.end.loopexit_crit_edge
	movq	%rsi, (%r8)
.LBB13_20:                              # %while.end
	popq	%rax
	retq
.LBB13_21:                              # %if.then59
	movabsq	$.L.str40, %rax
	movabsq	$.L__FUNCTION__.note_sets, %rcx
	movq	$119, %rsi
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp198:
	.size	note_sets, .Ltmp198-note_sets
	.cfi_endproc

	.type	rename_obstack,@object  # @rename_obstack
	.local	rename_obstack
	.comm	rename_obstack,88,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"\nBasic block %d:\n"
	.size	.L.str, 18

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Register %s in insn %d"
	.size	.L.str1, 23

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	" crosses a call"
	.size	.L.str2, 16

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"; no available registers\n"
	.size	.L.str3, 26

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	", renamed as %s\n"
	.size	.L.str4, 17

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"\n\n"
	.size	.L.str5, 3

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"[%u] Bad next_regno for empty chain (%u)\n"
	.size	.L.str6, 42

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"[%u %s] "
	.size	.L.str7, 9

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"[%u] Loop in regno chain\n"
	.size	.L.str8, 26

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"[%u] Bad oldest_regno (%u)\n"
	.size	.L.str9, 28

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"[%u] Non-empty reg in chain (%s %u %i)\n"
	.size	.L.str10, 40

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"insn %u: replaced reg %u with %u\n"
	.size	.L.str11, 34

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"Register %s (%d):"
	.size	.L.str12, 18

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	" %d [%s]"
	.size	.L.str13, 9

	.type	reg_class_names,@object # @reg_class_names
	.section	.rodata,"a",@progbits
	.align	16
reg_class_names:
	.quad	.L.str15
	.quad	.L.str16
	.quad	.L.str17
	.quad	.L.str18
	.quad	.L.str19
	.quad	.L.str20
	.quad	.L.str21
	.quad	.L.str22
	.quad	.L.str23
	.quad	.L.str24
	.quad	.L.str25
	.quad	.L.str26
	.quad	.L.str27
	.quad	.L.str28
	.quad	.L.str29
	.quad	.L.str30
	.quad	.L.str31
	.quad	.L.str32
	.quad	.L.str33
	.quad	.L.str34
	.quad	.L.str35
	.quad	.L.str36
	.quad	.L.str37
	.quad	.L.str38
	.quad	.L.str39
	.size	reg_class_names, 200

	.type	.L.str15,@object        # @.str15
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str15:
	.asciz	"NO_REGS"
	.size	.L.str15, 8

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"AREG"
	.size	.L.str16, 5

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"DREG"
	.size	.L.str17, 5

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"CREG"
	.size	.L.str18, 5

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"BREG"
	.size	.L.str19, 5

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"SIREG"
	.size	.L.str20, 6

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"DIREG"
	.size	.L.str21, 6

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"AD_REGS"
	.size	.L.str22, 8

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"Q_REGS"
	.size	.L.str23, 7

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"NON_Q_REGS"
	.size	.L.str24, 11

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"INDEX_REGS"
	.size	.L.str25, 11

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"LEGACY_REGS"
	.size	.L.str26, 12

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"GENERAL_REGS"
	.size	.L.str27, 13

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"FP_TOP_REG"
	.size	.L.str28, 11

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"FP_SECOND_REG"
	.size	.L.str29, 14

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"FLOAT_REGS"
	.size	.L.str30, 11

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"SSE_REGS"
	.size	.L.str31, 9

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"MMX_REGS"
	.size	.L.str32, 9

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"FP_TOP_SSE_REGS"
	.size	.L.str33, 16

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"FP_SECOND_SSE_REGS"
	.size	.L.str34, 19

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"FLOAT_SSE_REGS"
	.size	.L.str35, 15

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"FLOAT_INT_REGS"
	.size	.L.str36, 15

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"INT_SSE_REGS"
	.size	.L.str37, 13

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"FLOAT_INT_SSE_REGS"
	.size	.L.str38, 19

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"ALL_REGS"
	.size	.L.str39, 9

	.type	closed_chains,@object   # @closed_chains
	.local	closed_chains
	.comm	closed_chains,8,8
	.type	open_chains,@object     # @open_chains
	.local	open_chains
	.comm	open_chains,8,8
	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"regrename.c"
	.size	.L.str40, 12

	.type	.L__FUNCTION__.scan_rtx_reg,@object # @__FUNCTION__.scan_rtx_reg
.L__FUNCTION__.scan_rtx_reg:
	.asciz	"scan_rtx_reg"
	.size	.L__FUNCTION__.scan_rtx_reg, 13

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"Closing chain %s at insn %d (%s)\n"
	.size	.L.str41, 34

	.type	scan_actions_name,@object # @scan_actions_name
	.section	.rodata,"a",@progbits
	.align	16
scan_actions_name:
	.quad	.L.str43
	.quad	.L.str44
	.quad	.L.str45
	.quad	.L.str46
	.quad	.L.str47
	.quad	.L.str48
	.size	scan_actions_name, 48

	.type	.L.str42,@object        # @.str42
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str42:
	.asciz	"Discarding chain %s at insn %d (%s)\n"
	.size	.L.str42, 37

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"terminate_all_read"
	.size	.L.str43, 19

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"terminate_overlapping_read"
	.size	.L.str44, 27

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"terminate_write"
	.size	.L.str45, 16

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"terminate_dead"
	.size	.L.str46, 15

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"mark_read"
	.size	.L.str47, 10

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"mark_write"
	.size	.L.str48, 11

	.type	.L__FUNCTION__.scan_rtx,@object # @__FUNCTION__.scan_rtx
.L__FUNCTION__.scan_rtx:
	.asciz	"scan_rtx"
	.size	.L__FUNCTION__.scan_rtx, 9

	.type	.L__FUNCTION__.note_sets,@object # @__FUNCTION__.note_sets
.L__FUNCTION__.note_sets:
	.asciz	"note_sets"
	.size	.L__FUNCTION__.note_sets, 10

	.type	.L__FUNCTION__.clear_dead_regs,@object # @__FUNCTION__.clear_dead_regs
.L__FUNCTION__.clear_dead_regs:
	.asciz	"clear_dead_regs"
	.size	.L__FUNCTION__.clear_dead_regs, 16


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
