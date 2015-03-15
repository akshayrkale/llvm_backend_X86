	.file	"cfgbuild.c.bc"
	.text
	.globl	find_basic_blocks
	.align	16, 0x90
	.type	find_basic_blocks,@function
find_basic_blocks:                      # @find_basic_blocks
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
	subq	$48, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 96
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
	movq	%rdi, %rbx
	movq	$3, %rdi
	callq	timevar_push
	cmpq	$0, basic_block_info(%rip)
	movq	$0, basic_block_for_insn(%rip)
	je	.LBB0_6
# BB#1:                                 # %if.then
	callq	clear_edges
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_4
# BB#2:
	movq	$4, %rdx
	.align	16, 0x90
.LBB0_3:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	(%rcx,%rdx,8), %rcx
	movq	$0, 80(%rcx)
	leaq	-3(%rdx), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rdx
	cmpq	%rax, %rsi
	jl	.LBB0_3
.LBB0_4:                                # %do.body
	movq	basic_block_info(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_6
# BB#5:                                 # %if.then2
	callq	free
	movq	$0, basic_block_info(%rip)
.LBB0_6:                                # %if.end3
	xorq	%r14, %r14
	testq	%rbx, %rbx
	movq	%rbx, %rax
	je	.LBB0_7
# BB#8:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%rax, %rbx
	movq	%rax, %r13
	xorq	%r15, %r15
	.align	16, 0x90
.LBB0_9:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %ecx
	leaq	-35(%rcx), %rax
	andq	%r12, %rax
	cmpq	$1, %rax
	ja	.LBB0_13
# BB#10:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	%r14, %rax
	andq	$1, %rax
	je	.LBB0_12
# BB#11:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB0_9 Depth=1
	xorq	%r14, %r14
.LBB0_12:                               # %land.lhs.true.i
                                        #   in Loop: Header=BB0_9 Depth=1
	addq	%rax, %r15
.LBB0_13:                               # %if.end.i
                                        #   in Loop: Header=BB0_9 Depth=1
	testq	$1, %r14
	jne	.LBB0_26
# BB#14:                                # %land.lhs.true6.i
                                        #   in Loop: Header=BB0_9 Depth=1
	leaq	-32(%rcx), %rdx
	andq	%r12, %rdx
	cmpq	$5, %rdx
	ja	.LBB0_56
# BB#15:                                # %land.lhs.true6.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	$1, %rax
	jmpq	*.LJTI0_0(,%rdx,8)
.LBB0_22:                               # %sw.bb41.i.i
                                        #   in Loop: Header=BB0_9 Depth=1
	xorq	%rax, %rax
	jmp	.LBB0_23
.LBB0_20:                               # %sw.bb25.i.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	32(%rbx), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	movabsq	$0, %rax
	je	.LBB0_23
# BB#21:                                # %sw.bb25.i.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movabsq	$1, %rax
	jmp	.LBB0_23
.LBB0_16:                               # %sw.bb.i.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	24(%rbx), %rdx
	testq	%rdx, %rdx
	je	.LBB0_23
# BB#17:                                # %lor.lhs.false.i.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movzbl	(%rdx), %esi
	cmpq	$33, %rsi
	jne	.LBB0_23
# BB#18:                                # %lor.rhs.i.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	32(%rdx), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	movabsq	$0, %rax
	je	.LBB0_23
# BB#19:                                # %lor.rhs.i.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movabsq	$1, %rax
	.align	16, 0x90
.LBB0_23:                               # %inside_basic_block_p.exit.i
                                        #   in Loop: Header=BB0_9 Depth=1
	testq	%rax, %rax
	movq	$1, %rax
	jne	.LBB0_25
# BB#24:                                # %inside_basic_block_p.exit.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	%r14, %rax
.LBB0_25:                               # %inside_basic_block_p.exit.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	%rax, %r14
.LBB0_26:                               # %if.end8.i
                                        #   in Loop: Header=BB0_9 Depth=1
	testq	$1, %r14
	je	.LBB0_39
# BB#27:                                # %land.lhs.true10.i
                                        #   in Loop: Header=BB0_9 Depth=1
	addq	$-32, %rcx
	andq	%r12, %rcx
	cmpq	$5, %rcx
	ja	.LBB0_105
# BB#28:                                # %land.lhs.true10.i
                                        #   in Loop: Header=BB0_9 Depth=1
	xorq	%rax, %rax
	jmpq	*.LJTI0_1(,%rcx,8)
.LBB0_35:                               # %sw.bb19.i.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movl	flag_non_call_exceptions(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB0_34
	jmp	.LBB0_36
.LBB0_29:                               # %sw.bb1.i.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	32(%rbx), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	movabsq	$0, %rax
	je	.LBB0_36
# BB#30:                                # %sw.bb1.i.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movabsq	$1, %rax
	jmp	.LBB0_36
.LBB0_31:                               # %sw.bb10.i.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	cfun(%rip), %rax
	cmpq	$0, 176(%rax)
	je	.LBB0_34
# BB#32:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	$23, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	%rax, %rcx
	testq	%rcx, %rcx
	movq	$1, %rax
	je	.LBB0_36
# BB#33:                                # %lor.lhs.false.i41.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	8(%rcx), %rcx
	cmpq	$0, 8(%rcx)
	jns	.LBB0_36
.LBB0_34:                               # %lor.rhs.i42.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	%rbx, %rdi
	callq	can_throw_internal
	.align	16, 0x90
.LBB0_36:                               # %control_flow_insn_p.exit.i
                                        #   in Loop: Header=BB0_9 Depth=1
	xorq	%rcx, %rcx
	testq	%rax, %rax
	jne	.LBB0_38
# BB#37:                                # %control_flow_insn_p.exit.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	%r14, %rcx
.LBB0_38:                               # %control_flow_insn_p.exit.i
                                        #   in Loop: Header=BB0_9 Depth=1
	addq	%rax, %r15
	movq	%rcx, %r14
.LBB0_39:                               # %for.inc.i
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_9
	jmp	.LBB0_40
.LBB0_7:
	movq	%rax, %r13
	xorq	%r15, %r15
.LBB0_40:                               # %for.end.i
	andq	$1, %r14
	addq	%r15, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	jne	.LBB0_42
# BB#41:                                # %if.then20.i
	movq	const_int_rtx+512(%rip), %rdx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	$1, %r14
.LBB0_42:                               # %count_basic_blocks.exit
	movl	%r14d, n_basic_blocks(%rip)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movabsq	$.L.str, %rdx
	movq	$8, %rsi
	movq	%r14, %rdi
	callq	varray_init
	xorq	%rbx, %rbx
	movq	%r13, %rcx
	testq	%rcx, %rcx
	movq	%rax, basic_block_info(%rip)
	je	.LBB0_43
# BB#44:
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	xorq	%r15, %r15
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB0_45:                               # %for.body.i13
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_90 Depth 2
	movq	%rcx, %r13
	movl	(%r13), %eax
	movq	24(%r13), %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	%rax, %r14
	andq	$65535, %r14            # imm = 0xFFFF
	cmpq	$36, %r14
	jne	.LBB0_46
# BB#49:                                # %land.lhs.true.i14
                                        #   in Loop: Header=BB0_45 Depth=1
	testq	%r15, %r15
	jne	.LBB0_48
# BB#50:                                #   in Loop: Header=BB0_45 Depth=1
	xorq	%r15, %r15
	jmp	.LBB0_51
	.align	16, 0x90
.LBB0_46:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB0_45 Depth=1
	cmpq	$35, %r14
	jne	.LBB0_51
# BB#47:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB0_45 Depth=1
	testq	%r15, %r15
	je	.LBB0_51
.LBB0_48:                               # %if.then.i
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, %rdi
	leaq	1(%rax), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	create_basic_block_structure
	movl	(%r13), %eax
	xorq	%r15, %r15
	xorq	%rbx, %rbx
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
.LBB0_51:                               # %if.end.i16
                                        #   in Loop: Header=BB0_45 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	addq	$-32, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$5, %rax
	ja	.LBB0_56
# BB#52:                                # %if.end.i16
                                        #   in Loop: Header=BB0_45 Depth=1
	jmpq	*.LJTI0_2(,%rax,8)
.LBB0_57:                               # %inside_basic_block_p.exit.i33
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	32(%r13), %rcx
	jmp	.LBB0_58
.LBB0_53:                               # %sw.bb.i.i18
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	24(%r13), %rcx
	testq	%rcx, %rcx
	je	.LBB0_59
# BB#54:                                # %lor.lhs.false.i.i22
                                        #   in Loop: Header=BB0_45 Depth=1
	movzbl	(%rcx), %edx
	cmpq	$33, %rdx
	jne	.LBB0_59
# BB#55:                                # %lor.rhs.i.i27
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	32(%rcx), %rcx
.LBB0_58:                               # %inside_basic_block_p.exit.i33
                                        #   in Loop: Header=BB0_45 Depth=1
	movl	(%rcx), %ecx
	andq	$65534, %rcx            # imm = 0xFFFE
	cmpq	$44, %rcx
	je	.LBB0_62
	.align	16, 0x90
.LBB0_59:                               # %if.then8.i
                                        #   in Loop: Header=BB0_45 Depth=1
	testq	%r15, %r15
	movq	%r13, %rcx
	je	.LBB0_61
# BB#60:                                # %if.then8.i
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	%r15, %rcx
.LBB0_61:                               # %if.then8.i
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	%rcx, %r15
	movq	%r13, %rbx
.LBB0_62:                               # %if.end12.i
                                        #   in Loop: Header=BB0_45 Depth=1
	testq	%r15, %r15
	jne	.LBB0_64
# BB#63:                                #   in Loop: Header=BB0_45 Depth=1
	xorq	%r15, %r15
	jmp	.LBB0_73
	.align	16, 0x90
.LBB0_64:                               # %land.lhs.true14.i
                                        #   in Loop: Header=BB0_45 Depth=1
	cmpq	$5, %rax
	ja	.LBB0_105
# BB#65:                                # %land.lhs.true14.i
                                        #   in Loop: Header=BB0_45 Depth=1
	jmpq	*.LJTI0_3(,%rax,8)
.LBB0_70:                               # %sw.bb19.i.i49
                                        #   in Loop: Header=BB0_45 Depth=1
	movl	flag_non_call_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_73
	jmp	.LBB0_71
.LBB0_66:                               # %sw.bb1.i.i36
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	32(%r13), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	jne	.LBB0_72
	jmp	.LBB0_73
.LBB0_67:                               # %sw.bb10.i.i39
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	cfun(%rip), %rax
	cmpq	$0, 176(%rax)
	je	.LBB0_71
# BB#68:                                # %land.lhs.true.i.i42
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	$23, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB0_72
# BB#69:                                # %lor.lhs.false.i221.i
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	8(%rax), %rax
	cmpq	$0, 8(%rax)
	jns	.LBB0_72
.LBB0_71:                               # %control_flow_insn_p.exit.i51
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	%r13, %rdi
	callq	can_throw_internal
	testq	%rax, %rax
	je	.LBB0_73
.LBB0_72:                               # %if.then16.i
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, %rdi
	leaq	1(%rax), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	create_basic_block_structure
	xorq	%r15, %r15
	xorq	%rbx, %rbx
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB0_73:                               # %if.end19.i
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	%rbx, %r12
	cmpq	$37, %r14
	ja	.LBB0_83
# BB#74:                                # %if.end19.i
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	$1, %rax
	movq	%r14, %rcx
	shlq	%cl, %rax
	movabsq	$115964116992, %rcx     # imm = 0x1B00000000
	testq	%rcx, %rax
	jne	.LBB0_85
# BB#75:                                # %if.end19.i
                                        #   in Loop: Header=BB0_45 Depth=1
	cmpq	$34, %r14
	jne	.LBB0_76
# BB#80:                                # %sw.bb30.i
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	32(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$141, %rcx
	jne	.LBB0_85
# BB#81:                                # %if.then37.i
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	8(%rax), %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	find_label_refs
	movq	32(%r13), %rcx
	movq	16(%rcx), %rdi
	movq	%rax, %rsi
	callq	find_label_refs
	movq	32(%r13), %rcx
	movq	24(%rcx), %rdi
	movq	%rax, %rsi
	callq	find_label_refs
	movq	32(%r13), %rcx
	movq	32(%rcx), %rsi
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rsi, %rsi
	je	.LBB0_85
# BB#82:                                # %if.then66.i
                                        #   in Loop: Header=BB0_45 Depth=1
	xorq	%rdi, %rdi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	alloc_EXPR_LIST
	movq	%rax, 8(%rsp)           # 8-byte Spill
	jmp	.LBB0_85
.LBB0_76:                               # %if.end19.i
                                        #   in Loop: Header=BB0_45 Depth=1
	cmpq	$37, %r14
	jne	.LBB0_83
# BB#77:                                # %sw.bb.i
                                        #   in Loop: Header=BB0_45 Depth=1
	movl	40(%r13), %eax
	movabsq	$4294967216, %rcx       # imm = 0xFFFFFFB0
	cmpq	%rcx, %rax
	jne	.LBB0_85
# BB#78:                                # %if.then23.i
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	jne	.LBB0_84
# BB#79:                                #   in Loop: Header=BB0_45 Depth=1
	movq	%r13, 24(%rsp)          # 8-byte Spill
	jmp	.LBB0_85
.LBB0_84:                               # %if.else.i
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	%r13, %rdi
	callq	delete_insn
	movq	%rax, 32(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB0_85:                               # %sw.epilog.i
                                        #   in Loop: Header=BB0_45 Depth=1
	movl	(%r13), %eax
	orq	$2, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$34, %rax
	jne	.LBB0_86
# BB#89:                                # %if.then83.i
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	56(%r13), %rbx
	jmp	.LBB0_90
	.align	16, 0x90
.LBB0_99:                               # %for.inc.i55
                                        #   in Loop: Header=BB0_90 Depth=2
	movq	16(%rbx), %rbx
.LBB0_90:                               # %if.then83.i
                                        #   Parent Loop BB0_45 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB0_86
# BB#91:                                # %for.body89.i
                                        #   in Loop: Header=BB0_90 Depth=2
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$851968, %rax           # imm = 0xD0000
	jne	.LBB0_99
# BB#92:                                # %if.then93.i
                                        #   in Loop: Header=BB0_90 Depth=2
	movq	8(%rbx), %r14
	movq	%r14, %rdi
	callq	next_nonnote_insn
	testq	%rax, %rax
	je	.LBB0_95
# BB#93:                                # %land.lhs.true100.i
                                        #   in Loop: Header=BB0_90 Depth=2
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB0_95
# BB#94:                                # %land.lhs.true104.i
                                        #   in Loop: Header=BB0_90 Depth=2
	movq	32(%rax), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	je	.LBB0_99
	.align	16, 0x90
.LBB0_95:                               # %if.else119.i
                                        #   in Loop: Header=BB0_90 Depth=2
	movzbl	(%r14), %eax
	cmpq	$37, %rax
	je	.LBB0_99
# BB#96:                                # %if.else124.i
                                        #   in Loop: Header=BB0_90 Depth=2
	movq	24(%r13), %rdi
	movzbl	(%rdi), %eax
	cmpq	$33, %rax
	jne	.LBB0_98
# BB#97:                                # %land.lhs.true131.i
                                        #   in Loop: Header=BB0_90 Depth=2
	movq	$13, %rsi
	movq	%r14, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB0_99
.LBB0_98:                               # %if.else138.i
                                        #   in Loop: Header=BB0_90 Depth=2
	movq	8(%rbx), %rsi
	xorq	%rdi, %rdi
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	alloc_EXPR_LIST
	movq	%rax, 40(%rsp)          # 8-byte Spill
	jmp	.LBB0_99
	.align	16, 0x90
.LBB0_86:                               # %for.cond.backedge.i
                                        #   in Loop: Header=BB0_45 Depth=1
	movq	32(%rsp), %rcx          # 8-byte Reload
	testq	%rcx, %rcx
	movq	%r12, %rbx
	jne	.LBB0_45
# BB#87:                                # %for.end152.i
	movq	%rbx, %rdx
	testq	%r15, %r15
	je	.LBB0_100
# BB#88:                                # %if.then154.i
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	leaq	1(%rbx), %rbx
	movq	%r15, %rsi
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	create_basic_block_structure
	jmp	.LBB0_102
.LBB0_56:                               # %sw.default.i.i
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.inside_basic_block_p, %rdx
	movq	$91, %rsi
	callq	fancy_abort
.LBB0_105:                              # %sw.default.i43.i
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.control_flow_insn_p, %rdx
	movq	$134, %rsi
	callq	fancy_abort
.LBB0_43:
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	jmp	.LBB0_103
.LBB0_83:                               # %sw.default.i
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.find_basic_blocks_1, %rdx
	movq	$554, %rsi              # imm = 0x22A
	callq	fancy_abort
.LBB0_100:                              # %if.else157.i
	movq	24(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	movq	16(%rsp), %rbx          # 8-byte Reload
	je	.LBB0_102
# BB#101:                               # %if.then159.i
	callq	delete_insn
.LBB0_102:                              # %if.end162.i
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	40(%rsp), %rsi          # 8-byte Reload
.LBB0_103:                              # %if.end162.i
	movl	n_basic_blocks(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rbx
	cmpq	%rax, %rbx
	jne	.LBB0_106
# BB#104:                               # %find_basic_blocks_1.exit
	movq	%rsi, label_value_list(%rip)
	movq	%rdx, tail_recursion_label_list(%rip)
	callq	get_max_uid
	movq	%rax, %rdi
	callq	compute_bb_for_insn
	movl	n_basic_blocks(%rip), %edx
	movq	label_value_list(%rip), %rdi
	decq	%rdx
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	callq	make_edges
	callq	tidy_fallthru_edges
	movq	$3, %rdi
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	timevar_pop  # TAILCALL
.LBB0_106:                              # %if.then164.i
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.find_basic_blocks_1, %rcx
	movq	$596, %rsi              # imm = 0x254
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp17:
	.size	find_basic_blocks, .Ltmp17-find_basic_blocks
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI0_0:
	.quad	.LBB0_23
	.quad	.LBB0_20
	.quad	.LBB0_23
	.quad	.LBB0_22
	.quad	.LBB0_16
	.quad	.LBB0_22
.LJTI0_1:
	.quad	.LBB0_35
	.quad	.LBB0_29
	.quad	.LBB0_31
	.quad	.LBB0_36
	.quad	.LBB0_36
	.quad	.LBB0_36
.LJTI0_2:
	.quad	.LBB0_59
	.quad	.LBB0_57
	.quad	.LBB0_59
	.quad	.LBB0_62
	.quad	.LBB0_53
	.quad	.LBB0_62
.LJTI0_3:
	.quad	.LBB0_70
	.quad	.LBB0_66
	.quad	.LBB0_67
	.quad	.LBB0_73
	.quad	.LBB0_73
	.quad	.LBB0_73

	.text
	.align	16, 0x90
	.type	make_edges,@function
make_edges:                             # @make_edges
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
	movq	cfun(%rip), %rax
	movl	424(%rax), %r8d
	movabsq	$4294965247, %rbx       # imm = 0xFFFFF7FF
	andq	%r8, %rbx
	movl	%ebx, 424(%rax)
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	32(%rax), %rax
	movq	%rcx, %rbx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, %r14
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	xorq	%r12, %r12
	orq	%rdi, %rax
	je	.LBB1_10
# BB#1:                                 # %if.then
	movl	n_basic_blocks(%rip), %edi
	movq	%rdi, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %esi
	movq	%rax, %r12
	movq	%r12, %rdi
	callq	sbitmap_vector_zero
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB1_10
# BB#2:                                 # %if.then
	movq	$32, %rcx
	movq	24(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jg	.LBB1_10
# BB#3:                                 # %for.body.lr.ph
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	basic_block_info(%rip), %r8
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$entry_exit_blocks+112, %rsi
	.align	16, 0x90
.LBB1_4:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_5 Depth 2
	movq	32(%r8,%rax,8), %rcx
	movq	40(%rcx), %rdi
	jmp	.LBB1_5
	.align	16, 0x90
.LBB1_8:                                # %for.inc
                                        #   in Loop: Header=BB1_5 Depth=2
	movq	8(%rdi), %rdi
.LBB1_5:                                # %for.body
                                        #   Parent Loop BB1_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rdi, %rdi
	je	.LBB1_9
# BB#6:                                 # %for.body6
                                        #   in Loop: Header=BB1_5 Depth=2
	movq	24(%rdi), %rcx
	cmpq	%rsi, %rcx
	je	.LBB1_8
# BB#7:                                 # %if.then8
                                        #   in Loop: Header=BB1_5 Depth=2
	movl	88(%rcx), %ecx
	movq	(%r12,%rax,8), %rbx
	movq	$1, %rdx
	shlq	%cl, %rdx
	shrq	$6, %rcx
	orq	%rdx, 16(%rbx,%rcx,8)
	jmp	.LBB1_8
	.align	16, 0x90
.LBB1_9:                                # %for.inc16
                                        #   in Loop: Header=BB1_4 Depth=1
	movq	$32, %rcx
	leaq	1(%rax), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	24(%rsp), %rdi          # 8-byte Reload
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rax
	movq	%rdx, %rax
	jl	.LBB1_4
.LBB1_10:                               # %if.end19
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	jne	.LBB1_12
# BB#11:                                # %if.then21
	movq	basic_block_info(%rip), %rax
	movq	32(%rax), %rdx
	movabsq	$entry_exit_blocks, %rsi
	movq	$1, %rcx
	movq	%r12, %rdi
	callq	cached_make_edge
	xorq	%r14, %r14
.LBB1_12:                               # %for.cond27.preheader
	movq	$32, %rcx
	movq	24(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jg	.LBB1_99
# BB#13:                                # %for.body29.lr.ph
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	jmp	.LBB1_14
.LBB1_42:                               #   in Loop: Header=BB1_14 Depth=1
	movq	%r15, 16(%rsp)          # 8-byte Spill
	addq	$8, %rax
.LBB1_44:                               # %if.then85
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	(%rax), %r15
	movslq	(%r15), %rbx
	testq	%rbx, %rbx
	jle	.LBB1_49
# BB#45:                                # %for.body110.lr.ph
                                        #   in Loop: Header=BB1_14 Depth=1
	decq	%rbx
	.align	16, 0x90
.LBB1_46:                               # %for.body110
                                        #   Parent Loop BB1_14 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%r15,%rbx,8), %rax
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB1_21
# BB#47:                                # %if.end.i
                                        #   in Loop: Header=BB1_46 Depth=2
	movslq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_48
# BB#102:                               # %if.end3.i
                                        #   in Loop: Header=BB1_46 Depth=2
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rdx
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	cached_make_edge
.LBB1_48:                               # %for.cond108.backedge
                                        #   in Loop: Header=BB1_46 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB1_46
.LBB1_49:                               # %for.end117
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	16(%rsp), %r15          # 8-byte Reload
	movzbl	(%r15), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB1_27
# BB#50:                                # %cond.true
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	32(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB1_52
# BB#51:                                # %cond.false
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB1_52:                               # %cond.end140
                                        #   in Loop: Header=BB1_14 Depth=1
	testq	%rax, %rax
	je	.LBB1_27
# BB#53:                                # %land.lhs.true144
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	8(%rax), %rcx
	movq	global_rtl(%rip), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB1_27
# BB#54:                                # %land.lhs.true150
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	16(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$72, %rcx
	jne	.LBB1_27
# BB#55:                                # %land.lhs.true158
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	24(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$67, %rcx
	jne	.LBB1_27
# BB#56:                                # %if.then169
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	8(%rax), %rax
	jmp	.LBB1_57
	.align	16, 0x90
.LBB1_14:                               # %for.body29
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_46 Depth 2
                                        #     Child Loop BB1_63 Depth 2
                                        #     Child Loop BB1_68 Depth 2
                                        #     Child Loop BB1_20 Depth 2
                                        #     Child Loop BB1_83 Depth 2
                                        #     Child Loop BB1_92 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r14,8), %r13
	movq	(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB1_17
# BB#15:                                # %land.lhs.true
                                        #   in Loop: Header=BB1_14 Depth=1
	cmpq	$0, 64(%rax)
	je	.LBB1_17
# BB#16:                                # %if.then41
                                        #   in Loop: Header=BB1_14 Depth=1
	xorq	%rdi, %rdi
	xorq	%rcx, %rcx
	movabsq	$entry_exit_blocks, %rsi
	movq	%r13, %rdx
	callq	cached_make_edge
.LBB1_17:                               # %if.end43
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	8(%r13), %r15
	movzbl	(%r15), %eax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	cmpq	$33, %rax
	jne	.LBB1_27
# BB#18:                                # %if.then47
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	32(%r15), %rax
	movzbl	(%rax), %eax
	cmpq	$53, %rax
	jne	.LBB1_36
# BB#19:                                # %if.then53
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	%r15, %rdi
	callq	reachable_handlers
	movq	%rax, %rbx
	testq	%rbx, %rbx
	movq	%rbx, 40(%rsp)
	je	.LBB1_26
	.align	16, 0x90
.LBB1_20:                               # %for.body.i
                                        #   Parent Loop BB1_14 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB1_21
# BB#23:                                # %if.end.i.i
                                        #   in Loop: Header=BB1_20 Depth=2
	movslq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_25
# BB#24:                                # %if.end3.i.i
                                        #   in Loop: Header=BB1_20 Depth=2
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rdx
	movq	$10, %rcx
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	cached_make_edge
.LBB1_25:                               # %make_label_edge.exit.i
                                        #   in Loop: Header=BB1_20 Depth=2
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB1_20
.LBB1_26:                               # %make_eh_edge.exit
                                        #   in Loop: Header=BB1_14 Depth=1
	leaq	40(%rsp), %rdi
	callq	free_INSN_LIST_list
	jmp	.LBB1_27
	.align	16, 0x90
.LBB1_36:                               # %if.else
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	$27, %rsi
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB1_27
# BB#37:                                # %if.else57
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	64(%r15), %rax
	testq	%rax, %rax
	je	.LBB1_61
# BB#38:                                # %land.lhs.true62
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	24(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_61
# BB#39:                                # %land.lhs.true67
                                        #   in Loop: Header=BB1_14 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB1_61
# BB#40:                                # %land.lhs.true71
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	32(%rax), %rax
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65534, %rdx            # imm = 0xFFFE
	cmpq	$44, %rdx
	jne	.LBB1_61
# BB#41:                                # %if.then85
                                        #   in Loop: Header=BB1_14 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$44, %rcx
	je	.LBB1_42
# BB#43:                                # %if.then85
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	%r15, 16(%rsp)          # 8-byte Spill
	addq	$16, %rax
	jmp	.LBB1_44
.LBB1_61:                               # %if.else180
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	%r15, %rdi
	callq	computed_jump_p
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	jne	.LBB1_62
# BB#73:                                # %if.else211
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	%r15, %rdi
	callq	returnjump_p
	testq	%rbx, %rax
	jne	.LBB1_74
# BB#75:                                # %if.else216
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	64(%r15), %rax
	testq	%rax, %rax
	je	.LBB1_76
.LBB1_57:                               # %if.end222
                                        #   in Loop: Header=BB1_14 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB1_21
# BB#58:                                # %if.end.i481
                                        #   in Loop: Header=BB1_14 Depth=1
	movslq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_27
# BB#59:                                # %if.end3.i487
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rdx
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	cached_make_edge
	jmp	.LBB1_27
.LBB1_62:                               # %if.then183
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$2048, %rcx             # imm = 0x800
	movq	8(%rsp), %rdx           # 8-byte Reload
	testq	%rdx, %rdx
	movl	%ecx, 424(%rax)
	movq	%rdx, %rbx
	je	.LBB1_67
	.align	16, 0x90
.LBB1_63:                               # %for.body189
                                        #   Parent Loop BB1_14 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB1_21
# BB#64:                                # %if.end.i451
                                        #   in Loop: Header=BB1_63 Depth=2
	movslq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_66
# BB#65:                                # %if.end3.i457
                                        #   in Loop: Header=BB1_63 Depth=2
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rdx
	movq	$2, %rcx
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	cached_make_edge
.LBB1_66:                               # %make_label_edge.exit458
                                        #   in Loop: Header=BB1_63 Depth=2
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB1_63
.LBB1_67:                               # %for.end197
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	32(%rax), %rbx
	jmp	.LBB1_68
	.align	16, 0x90
.LBB1_72:                               # %make_label_edge.exit473
                                        #   in Loop: Header=BB1_68 Depth=2
	movq	16(%rbx), %rbx
.LBB1_68:                               # %for.end197
                                        #   Parent Loop BB1_14 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB1_27
# BB#69:                                # %for.body202
                                        #   in Loop: Header=BB1_68 Depth=2
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB1_21
# BB#70:                                # %if.end.i466
                                        #   in Loop: Header=BB1_68 Depth=2
	movslq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_72
# BB#71:                                # %if.end3.i472
                                        #   in Loop: Header=BB1_68 Depth=2
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rdx
	movq	$2, %rcx
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	cached_make_edge
	jmp	.LBB1_72
.LBB1_74:                               # %if.then214
                                        #   in Loop: Header=BB1_14 Depth=1
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	movq	%r13, %rsi
	movabsq	$entry_exit_blocks+112, %rdx
	callq	cached_make_edge
	.align	16, 0x90
.LBB1_27:                               # %if.end231
                                        #   in Loop: Header=BB1_14 Depth=1
	cmpq	$34, 32(%rsp)           # 8-byte Folded Reload
	jne	.LBB1_77
# BB#28:                                # %land.lhs.true234
                                        #   in Loop: Header=BB1_14 Depth=1
	movl	(%r15), %ebx
	testq	$16777216, %rbx         # imm = 0x1000000
	je	.LBB1_79
# BB#29:                                # %if.then238
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	$6, %rcx
	movq	%r12, %rdi
	movq	%r13, %rsi
	movabsq	$entry_exit_blocks+112, %rdx
	callq	cached_make_edge
	jmp	.LBB1_30
	.align	16, 0x90
.LBB1_77:                               # %if.else240
                                        #   in Loop: Header=BB1_14 Depth=1
	movl	flag_non_call_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB1_30
# BB#78:                                # %if.else240.if.then245_crit_edge
                                        #   in Loop: Header=BB1_14 Depth=1
	movl	(%r15), %ebx
.LBB1_79:                               # %if.then245
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	%r15, %rdi
	movq	%r15, 16(%rsp)          # 8-byte Spill
	callq	reachable_handlers
	movq	%rax, %r15
	testq	%r15, %r15
	movq	%r15, 40(%rsp)
	je	.LBB1_87
# BB#80:                                # %for.body.lr.ph.i496
                                        #   in Loop: Header=BB1_14 Depth=1
	andq	$65535, %rbx            # imm = 0xFFFF
	cmpq	$34, %rbx
	movabsq	$0, %rax
	jne	.LBB1_82
# BB#81:                                # %for.body.lr.ph.i496
                                        #   in Loop: Header=BB1_14 Depth=1
	movabsq	$1, %rax
.LBB1_82:                               # %for.body.lr.ph.i496
                                        #   in Loop: Header=BB1_14 Depth=1
	leaq	10(,%rax,4), %rbx
	.align	16, 0x90
.LBB1_83:                               # %for.body.i503
                                        #   Parent Loop BB1_14 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB1_103
# BB#84:                                # %if.end.i.i508
                                        #   in Loop: Header=BB1_83 Depth=2
	movslq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_86
# BB#85:                                # %if.end3.i.i514
                                        #   in Loop: Header=BB1_83 Depth=2
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rdx
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rcx
	callq	cached_make_edge
.LBB1_86:                               # %make_label_edge.exit.i518
                                        #   in Loop: Header=BB1_83 Depth=2
	movq	16(%r15), %r15
	testq	%r15, %r15
	jne	.LBB1_83
.LBB1_87:                               # %make_eh_edge.exit519
                                        #   in Loop: Header=BB1_14 Depth=1
	leaq	40(%rsp), %rdi
	callq	free_INSN_LIST_list
	cmpq	$34, 32(%rsp)           # 8-byte Folded Reload
	movq	16(%rsp), %r15          # 8-byte Reload
	jne	.LBB1_30
# BB#88:                                # %land.lhs.true248
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	cfun(%rip), %rax
	cmpq	$0, 176(%rax)
	je	.LBB1_30
# BB#89:                                # %if.then250
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	$23, %rsi
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB1_91
# BB#90:                                # %lor.lhs.false254
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	8(%rax), %rax
	cmpq	$0, 8(%rax)
	js	.LBB1_30
.LBB1_91:                               # %if.then262
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	cfun(%rip), %rax
	movq	176(%rax), %rbx
	jmp	.LBB1_92
	.align	16, 0x90
.LBB1_96:                               # %make_label_edge.exit534
                                        #   in Loop: Header=BB1_92 Depth=2
	movq	16(%rbx), %rbx
.LBB1_92:                               # %if.then262
                                        #   Parent Loop BB1_14 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB1_30
# BB#93:                                # %for.body266
                                        #   in Loop: Header=BB1_92 Depth=2
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB1_21
# BB#94:                                # %if.end.i527
                                        #   in Loop: Header=BB1_92 Depth=2
	movslq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_96
# BB#95:                                # %if.end3.i533
                                        #   in Loop: Header=BB1_92 Depth=2
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rdx
	movq	$6, %rcx
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	cached_make_edge
	jmp	.LBB1_96
	.align	16, 0x90
.LBB1_30:                               # %if.end278
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	%r15, %rdi
	callq	next_nonnote_insn
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB1_31
# BB#97:                                # %if.then286
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%r13, %rsi
	movabsq	$entry_exit_blocks+112, %rdx
	callq	cached_make_edge
	leaq	1(%r14), %r15
	jmp	.LBB1_98
	.align	16, 0x90
.LBB1_31:                               # %lor.lhs.false281
                                        #   in Loop: Header=BB1_14 Depth=1
	leaq	1(%r14), %r15
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB1_98
# BB#32:                                # %if.then292
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	basic_block_info(%rip), %rax
	movq	40(%rax,%r14,8), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB1_34
# BB#33:                                # %if.then305
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	%rax, %rdi
	callq	next_nonnote_insn
.LBB1_34:                               # %lor.lhs.false309
                                        #   in Loop: Header=BB1_14 Depth=1
	cmpq	%rax, %rbx
	jne	.LBB1_98
# BB#35:                                # %if.then312
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	basic_block_info(%rip), %rax
	movq	40(%rax,%r14,8), %rdx
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	cached_make_edge
	.align	16, 0x90
.LBB1_98:                               # %for.inc322
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	$32, %rcx
	movq	24(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r14
	movq	%r15, %r14
	jl	.LBB1_14
.LBB1_99:                               # %for.end324
	testq	%r12, %r12
	je	.LBB1_101
# BB#100:                               # %if.then326
	movq	%r12, %rdi
	callq	free
.LBB1_101:                              # %if.end327
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_103:                              # %if.then.i.i504
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.make_label_edge, %rdx
	movq	$239, %rsi
	callq	fancy_abort
.LBB1_76:                               # %if.then221
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.make_edges, %rcx
	movq	$402, %rsi              # imm = 0x192
	jmp	.LBB1_22
.LBB1_21:                               # %if.then.i.i
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.make_label_edge, %rcx
	movq	$239, %rsi
.LBB1_22:                               # %if.then.i.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp35:
	.size	make_edges, .Ltmp35-make_edges
	.cfi_endproc

	.globl	find_many_sub_basic_blocks
	.align	16, 0x90
	.type	find_many_sub_basic_blocks,@function
find_many_sub_basic_blocks:             # @find_many_sub_basic_blocks
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
.Ltmp47:
	.cfi_offset %rbx, -48
.Ltmp48:
	.cfi_offset %r12, -40
.Ltmp49:
	.cfi_offset %r13, -32
.Ltmp50:
	.cfi_offset %r14, -24
.Ltmp51:
	.cfi_offset %r15, -16
	movslq	n_basic_blocks(%rip), %r10
	xorq	%r13, %r13
	testq	%r10, %r10
	jle	.LBB2_14
# BB#1:
	movabsq	$0, %r9
	movabsq	$1, %r8
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB2_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$3, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	andq	$536870904, %rax        # imm = 0x1FFFFFF8
	movq	16(%rdi,%rax), %rax
	movq	$1, %rsi
	movq	%rbx, %rcx
	shlq	%cl, %rsi
	testq	%rsi, %rax
	movq	%r9, %rcx
	je	.LBB2_4
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	%r8, %rcx
.LBB2_4:                                # %for.body
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	basic_block_info(%rip), %rax
	incq	%rcx
	movq	32(%rax,%rbx,8), %rax
	movq	%rcx, 80(%rax)
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r10, %rax
	jl	.LBB2_2
# BB#5:                                 # %for.cond3.preheader
	testq	%r10, %r10
	jle	.LBB2_14
# BB#6:
	movq	$4, %rbx
	.align	16, 0x90
.LBB2_7:                                # %for.body6
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	(%rax,%rbx,8), %rdi
	movl	80(%rdi), %eax
	cmpq	$2, %rax
	jne	.LBB2_9
# BB#8:                                 # %if.then
                                        #   in Loop: Header=BB2_7 Depth=1
	callq	find_bb_boundaries
	movl	n_basic_blocks(%rip), %r10d
.LBB2_9:                                # %for.inc19
                                        #   in Loop: Header=BB2_7 Depth=1
	leaq	-3(%rbx), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r10, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rsi, %rax
	jl	.LBB2_7
# BB#10:                                # %for.cond22.preheader
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB2_14
# BB#11:                                # %for.body25.lr.ph
	movq	basic_block_info(%rip), %rax
	xorq	%r13, %r13
	.align	16, 0x90
.LBB2_12:                               # %for.body25
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax,%r13,8), %rcx
	movl	80(%rcx), %ecx
	cmpq	$1, %rcx
	jne	.LBB2_14
# BB#13:                                # %for.cond22
                                        #   in Loop: Header=BB2_12 Depth=1
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%r10, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rsi
	jl	.LBB2_12
.LBB2_14:                               # %for.cond39.preheader
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rax, %rsi
	movq	%r13, %r14
	jge	.LBB2_19
# BB#15:                                # %for.body42.lr.ph
	movq	basic_block_info(%rip), %rax
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%r13, %r14
	.align	16, 0x90
.LBB2_16:                               # %for.body42
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax,%rsi,8), %rcx
	movl	80(%rcx), %ecx
	cmpq	$1, %rcx
	je	.LBB2_18
# BB#17:                                # %for.body42
                                        #   in Loop: Header=BB2_16 Depth=1
	movq	%rsi, %r14
.LBB2_18:                               # %for.body42
                                        #   in Loop: Header=BB2_16 Depth=1
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%r10, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rdi
	jl	.LBB2_16
.LBB2_19:                               # %for.end55
	xorq	%rdi, %rdi
	movq	$1, %rcx
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	make_edges
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jg	.LBB2_27
# BB#20:                                # %for.body59.lr.ph
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movabsq	$3777893186295716171, %r12 # imm = 0x346DC5D63886594B
	.align	16, 0x90
.LBB2_21:                               # %for.body59
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_24 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r13,8), %rdi
	movq	80(%rdi), %rax
	movq	%rax, %rcx
	andq	%r15, %rcx
	cmpq	$1, %rcx
	je	.LBB2_26
# BB#22:                                # %for.body59
                                        #   in Loop: Header=BB2_21 Depth=1
	testq	%r15, %rax
	jne	.LBB2_25
# BB#23:                                # %if.then74
                                        #   in Loop: Header=BB2_21 Depth=1
	movq	$0, 96(%rdi)
	xorq	%r8, %r8
	movl	%r8d, 104(%rdi)
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	movabsq	$0, %rsi
	je	.LBB2_25
	.align	16, 0x90
.LBB2_24:                               # %for.body77
                                        #   Parent Loop BB2_21 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	56(%rbx), %rax
	addq	%rax, %rsi
	movq	%rsi, 96(%rdi)
	movq	16(%rbx), %rax
	movl	104(%rax), %ecx
	movl	52(%rbx), %eax
	imulq	%rcx, %rax
	addq	$5000, %rax             # imm = 0x1388
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	%r12
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$11, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	addq	%rdx, %r8
	movl	%r8d, 104(%rdi)
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB2_24
.LBB2_25:                               # %if.end87
                                        #   in Loop: Header=BB2_21 Depth=1
	callq	compute_outgoing_frequencies
.LBB2_26:                               # %for.inc88
                                        #   in Loop: Header=BB2_21 Depth=1
	movq	$32, %rcx
	leaq	1(%r13), %rax
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %r13
	movq	%rax, %r13
	jl	.LBB2_21
.LBB2_27:                               # %for.cond91.preheader
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB2_30
# BB#28:
	movq	$4, %rdx
	.align	16, 0x90
.LBB2_29:                               # %for.body94
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	(%rcx,%rdx,8), %rcx
	movq	$0, 80(%rcx)
	leaq	-3(%rdx), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rdx
	cmpq	%rax, %rsi
	jl	.LBB2_29
.LBB2_30:                               # %for.end102
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp52:
	.size	find_many_sub_basic_blocks, .Ltmp52-find_many_sub_basic_blocks
	.cfi_endproc

	.align	16, 0x90
	.type	find_bb_boundaries,@function
find_bb_boundaries:                     # @find_bb_boundaries
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp58:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp59:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp60:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp61:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp62:
	.cfi_def_cfa_offset 48
.Ltmp63:
	.cfi_offset %rbx, -40
.Ltmp64:
	.cfi_offset %r12, -32
.Ltmp65:
	.cfi_offset %r14, -24
.Ltmp66:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	(%r14), %rbx
	movq	8(%r14), %r15
	cmpq	%r15, %rbx
	je	.LBB3_36
# BB#1:                                 # %if.end
	movzbl	(%rbx), %eax
	xorq	%r12, %r12
	cmpq	$36, %rax
	jne	.LBB3_3
# BB#2:                                 # %if.then4
	movq	24(%rbx), %rbx
	jmp	.LBB3_3
	.align	16, 0x90
.LBB3_32:                               # %if.end38
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	24(%rbx), %rbx
	movq	%rax, %r12
.LBB3_3:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$36, %rax
	jne	.LBB3_8
# BB#4:                                 # %if.then9
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	16(%rbx), %rsi
	movq	%r14, %rdi
	callq	split_block
	testq	%r12, %r12
	je	.LBB3_6
# BB#5:                                 # %if.then13
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	%r12, 8(%r14)
.LBB3_6:                                # %if.end15
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	24(%rax), %r14
	movq	%rax, %rdi
	callq	remove_edge
	xorq	%r12, %r12
	cmpq	$0, 64(%rbx)
	je	.LBB3_19
# BB#7:                                 # %if.then19
                                        #   in Loop: Header=BB3_3 Depth=1
	movabsq	$entry_exit_blocks, %rdi
	xorq	%rdx, %rdx
	xorq	%r12, %r12
	movq	%r14, %rsi
	callq	make_edge
	jmp	.LBB3_19
	.align	16, 0x90
.LBB3_8:                                # %if.end22
                                        #   in Loop: Header=BB3_3 Depth=1
	testq	%r12, %r12
	je	.LBB3_18
# BB#9:                                 # %land.lhs.true
                                        #   in Loop: Header=BB3_3 Depth=1
	addq	$-32, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$5, %rcx
	ja	.LBB3_14
# BB#10:                                # %land.lhs.true
                                        #   in Loop: Header=BB3_3 Depth=1
	jmpq	*.LJTI3_0(,%rcx,8)
.LBB3_15:                               # %inside_basic_block_p.exit
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	32(%rbx), %rax
	jmp	.LBB3_16
.LBB3_11:                               # %sw.bb.i
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	24(%rbx), %rax
	testq	%rax, %rax
	je	.LBB3_17
# BB#12:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB3_3 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB3_17
# BB#13:                                # %lor.rhs.i
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	32(%rax), %rax
.LBB3_16:                               # %inside_basic_block_p.exit
                                        #   in Loop: Header=BB3_3 Depth=1
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	je	.LBB3_19
.LBB3_17:                               # %if.then25
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	16(%rbx), %rsi
	movq	%r14, %rdi
	callq	split_block
	movq	%r12, 8(%r14)
	movq	24(%rax), %r14
	movq	%rax, %rdi
	callq	remove_edge
.LBB3_18:                               # %if.end32
                                        #   in Loop: Header=BB3_3 Depth=1
	xorq	%r12, %r12
.LBB3_19:                               # %if.end32
                                        #   in Loop: Header=BB3_3 Depth=1
	movzbl	(%rbx), %eax
	addq	$-32, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$5, %rcx
	ja	.LBB3_37
# BB#20:                                # %if.end32
                                        #   in Loop: Header=BB3_3 Depth=1
	xorq	%rax, %rax
	jmpq	*.LJTI3_1(,%rcx,8)
.LBB3_28:                               # %sw.bb19.i
                                        #   in Loop: Header=BB3_3 Depth=1
	movl	flag_non_call_exceptions(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB3_27
	jmp	.LBB3_29
.LBB3_21:                               # %sw.bb1.i
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	32(%rbx), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	je	.LBB3_22
# BB#23:                                # %sw.bb1.i
                                        #   in Loop: Header=BB3_3 Depth=1
	movabsq	$1, %rax
	jmp	.LBB3_29
.LBB3_24:                               # %sw.bb10.i
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	cfun(%rip), %rax
	cmpq	$0, 176(%rax)
	je	.LBB3_27
# BB#25:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	$23, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	%rax, %rcx
	testq	%rcx, %rcx
	movq	$1, %rax
	je	.LBB3_29
# BB#26:                                # %lor.lhs.false.i80
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	8(%rcx), %rcx
	cmpq	$0, 8(%rcx)
	jns	.LBB3_29
.LBB3_27:                               # %lor.rhs.i81
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	%rbx, %rdi
	callq	can_throw_internal
	jmp	.LBB3_29
.LBB3_22:                               #   in Loop: Header=BB3_3 Depth=1
	movabsq	$0, %rax
	.align	16, 0x90
.LBB3_29:                               # %control_flow_insn_p.exit
                                        #   in Loop: Header=BB3_3 Depth=1
	testq	%rax, %rax
	movq	%rbx, %rax
	jne	.LBB3_31
# BB#30:                                # %control_flow_insn_p.exit
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	%r12, %rax
.LBB3_31:                               # %control_flow_insn_p.exit
                                        #   in Loop: Header=BB3_3 Depth=1
	cmpq	%r15, %rbx
	jne	.LBB3_32
# BB#33:                                # %while.end
	testq	%rax, %rax
	je	.LBB3_35
# BB#34:                                # %if.then43
	movq	%rax, 8(%r14)
.LBB3_35:                               # %if.end45
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	purge_dead_edges  # TAILCALL
.LBB3_36:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB3_37:                               # %sw.default.i82
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.control_flow_insn_p, %rdx
	movq	$134, %rsi
	callq	fancy_abort
.LBB3_14:                               # %sw.default.i
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.inside_basic_block_p, %rdx
	movq	$91, %rsi
	callq	fancy_abort
.Ltmp67:
	.size	find_bb_boundaries, .Ltmp67-find_bb_boundaries
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI3_0:
	.quad	.LBB3_17
	.quad	.LBB3_15
	.quad	.LBB3_17
	.quad	.LBB3_19
	.quad	.LBB3_11
	.quad	.LBB3_19
.LJTI3_1:
	.quad	.LBB3_28
	.quad	.LBB3_21
	.quad	.LBB3_24
	.quad	.LBB3_29
	.quad	.LBB3_29
	.quad	.LBB3_29

	.text
	.align	16, 0x90
	.type	compute_outgoing_frequencies,@function
compute_outgoing_frequencies:           # @compute_outgoing_frequencies
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp70:
	.cfi_def_cfa_offset 16
.Ltmp71:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	40(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB4_12
# BB#1:                                 # %land.lhs.true
	movq	8(%rsi), %rax
	testq	%rax, %rax
	je	.LBB4_10
# BB#2:                                 # %land.lhs.true3
	cmpq	$0, 8(%rax)
	jne	.LBB4_9
# BB#3:                                 # %if.then
	movq	8(%rbx), %rdi
	movq	$16, %rsi
	xorq	%rdx, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB4_12
# BB#4:                                 # %if.end
	movq	8(%rbx), %rdi
	movq	$16, %rsi
	xorq	%rdx, %rdx
	callq	find_reg_note
	movq	40(%rbx), %rsi
	movq	8(%rax), %rax
	movl	48(%rsi), %r9d
	movq	8(%rax), %r8
	andq	$1, %r9
	movq	%rsi, %r10
	je	.LBB4_6
# BB#5:                                 # %cond.true
	movq	8(%rsi), %r10
.LBB4_6:                                # %cond.end
	movl	%r8d, 52(%r10)
	movq	96(%rbx), %rdi
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	%rdi, %rax
	addq	$5000, %rax             # imm = 0x1388
	movabsq	$3777893186295716171, %rcx # imm = 0x346DC5D63886594B
	imulq	%rcx
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$11, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r9
	movq	%rdx, 56(%r10)
	movq	%rsi, %rax
	jne	.LBB4_8
# BB#7:                                 # %cond.false28
	movq	8(%rsi), %rax
.LBB4_8:                                # %cond.end31
	movq	$10000, %rcx            # imm = 0x2710
	subq	%r8, %rcx
	movl	%ecx, 52(%rax)
	subq	%rdx, %rdi
	movq	%rdi, 56(%rax)
.LBB4_9:                                # %if.end38
	testq	%rsi, %rsi
	je	.LBB4_12
.LBB4_10:                               # %land.lhs.true41
	cmpq	$0, 8(%rsi)
	jne	.LBB4_12
# BB#11:                                # %if.then45
	movq	$10000, %rax            # imm = 0x2710
	movl	%eax, 52(%rsi)
	movq	96(%rbx), %rax
	movq	%rax, 56(%rsi)
.LBB4_12:                               # %if.end50
	popq	%rbx
	retq
.Ltmp72:
	.size	compute_outgoing_frequencies, .Ltmp72-compute_outgoing_frequencies
	.cfi_endproc

	.globl	find_sub_basic_blocks
	.align	16, 0x90
	.type	find_sub_basic_blocks,@function
find_sub_basic_blocks:                  # @find_sub_basic_blocks
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp79:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp80:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp81:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp82:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp83:
	.cfi_def_cfa_offset 48
.Ltmp84:
	.cfi_offset %rbx, -48
.Ltmp85:
	.cfi_offset %r12, -40
.Ltmp86:
	.cfi_offset %r13, -32
.Ltmp87:
	.cfi_offset %r14, -24
.Ltmp88:
	.cfi_offset %r15, -16
	movl	88(%rdi), %edx
	movq	$32, %rcx
	movq	%rdx, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	movl	n_basic_blocks(%rip), %eax
	sarq	%cl, %r12
	decq	%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	xorq	%rbx, %rbx
	cmpq	%rax, %rdx
	movq	%rdx, %r14
	je	.LBB5_2
# BB#1:                                 # %cond.false
	leaq	1(%r14), %rax
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %rbx
.LBB5_2:                                # %cond.end
	callq	find_bb_boundaries
	testq	%rbx, %rbx
	je	.LBB5_3
# BB#4:                                 # %cond.end
	addq	$88, %rbx
	jmp	.LBB5_5
.LBB5_3:
	movabsq	$n_basic_blocks, %rbx
.LBB5_5:                                # %cond.end
	movl	(%rbx), %r15d
	decq	%r15
	movq	$32, %rcx
	movq	%r15, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	xorq	%rdi, %rdi
	movq	$1, %rcx
	movq	%r14, %rbx
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	make_edges
	movq	$32, %rcx
	movq	%rbx, %rax
	movq	%rbx, %r14
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r13, %rax
	jg	.LBB5_11
# BB#6:                                 # %for.body.lr.ph
	movabsq	$3777893186295716171, %r13 # imm = 0x346DC5D63886594B
	.align	16, 0x90
.LBB5_7:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_9 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r12,8), %rdi
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%r14, %rax
	je	.LBB5_10
# BB#8:                                 # %if.then
                                        #   in Loop: Header=BB5_7 Depth=1
	movq	$0, 96(%rdi)
	xorq	%r8, %r8
	movl	%r8d, 104(%rdi)
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	movabsq	$0, %rsi
	je	.LBB5_10
	.align	16, 0x90
.LBB5_9:                                # %for.body18
                                        #   Parent Loop BB5_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	56(%rbx), %rax
	addq	%rax, %rsi
	movq	%rsi, 96(%rdi)
	movq	16(%rbx), %rax
	movl	104(%rax), %ecx
	movl	52(%rbx), %eax
	imulq	%rcx, %rax
	addq	$5000, %rax             # imm = 0x1388
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	%r13
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$11, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	addq	%rdx, %r8
	movl	%r8d, 104(%rdi)
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB5_9
.LBB5_10:                               # %if.end
                                        #   in Loop: Header=BB5_7 Depth=1
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	callq	compute_outgoing_frequencies
	incq	%r12
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rbx
	jl	.LBB5_7
.LBB5_11:                               # %for.end27
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp89:
	.size	find_sub_basic_blocks, .Ltmp89-find_sub_basic_blocks
	.cfi_endproc

	.align	16, 0x90
	.type	find_label_refs,@function
find_label_refs:                        # @find_label_refs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp95:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp96:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp97:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp98:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp99:
	.cfi_def_cfa_offset 48
.Ltmp100:
	.cfi_offset %rbx, -40
.Ltmp101:
	.cfi_offset %r12, -32
.Ltmp102:
	.cfi_offset %r14, -24
.Ltmp103:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	jmp	.LBB6_1
	.align	16, 0x90
.LBB6_15:                               # %for.inc73
                                        #   in Loop: Header=BB6_1 Depth=1
	movq	24(%r15), %r15
.LBB6_1:                                # %entry
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_5 Depth 2
	testq	%r15, %r15
	je	.LBB6_16
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB6_1 Depth=1
	movzbl	(%r15), %eax
	movzbl	rtx_class(%rax), %ecx
	cmpq	$105, %rcx
	jne	.LBB6_15
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB6_1 Depth=1
	cmpq	$33, %rax
	je	.LBB6_15
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB6_1 Depth=1
	movq	56(%r15), %rbx
	jmp	.LBB6_5
	.align	16, 0x90
.LBB6_14:                               # %for.inc
                                        #   in Loop: Header=BB6_5 Depth=2
	movq	16(%rbx), %rbx
.LBB6_5:                                # %if.then
                                        #   Parent Loop BB6_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB6_15
# BB#6:                                 # %for.body9
                                        #   in Loop: Header=BB6_5 Depth=2
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$851968, %rax           # imm = 0xD0000
	jne	.LBB6_14
# BB#7:                                 # %if.then14
                                        #   in Loop: Header=BB6_5 Depth=2
	movq	8(%rbx), %r12
	movq	%r12, %rdi
	callq	next_nonnote_insn
	testq	%rax, %rax
	je	.LBB6_10
# BB#8:                                 # %land.lhs.true20
                                        #   in Loop: Header=BB6_5 Depth=2
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB6_10
# BB#9:                                 # %land.lhs.true25
                                        #   in Loop: Header=BB6_5 Depth=2
	movq	32(%rax), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	je	.LBB6_14
	.align	16, 0x90
.LBB6_10:                               # %if.else
                                        #   in Loop: Header=BB6_5 Depth=2
	movzbl	(%r12), %eax
	cmpq	$37, %rax
	je	.LBB6_14
# BB#11:                                # %if.else46
                                        #   in Loop: Header=BB6_5 Depth=2
	movq	24(%r15), %rdi
	movzbl	(%rdi), %eax
	cmpq	$33, %rax
	jne	.LBB6_13
# BB#12:                                # %land.lhs.true54
                                        #   in Loop: Header=BB6_5 Depth=2
	movq	$13, %rsi
	movq	%r12, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB6_14
.LBB6_13:                               # %if.else61
                                        #   in Loop: Header=BB6_5 Depth=2
	movq	8(%rbx), %rsi
	xorq	%rdi, %rdi
	movq	%r14, %rdx
	callq	alloc_EXPR_LIST
	movq	%rax, %r14
	jmp	.LBB6_14
.LBB6_16:                               # %for.end77
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp104:
	.size	find_label_refs, .Ltmp104-find_label_refs
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"basic_block_info"
	.size	.L.str, 17

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"cfgbuild.c"
	.size	.L.str1, 11

	.type	.L__FUNCTION__.control_flow_insn_p,@object # @__FUNCTION__.control_flow_insn_p
.L__FUNCTION__.control_flow_insn_p:
	.asciz	"control_flow_insn_p"
	.size	.L__FUNCTION__.control_flow_insn_p, 20

	.type	.L__FUNCTION__.inside_basic_block_p,@object # @__FUNCTION__.inside_basic_block_p
.L__FUNCTION__.inside_basic_block_p:
	.asciz	"inside_basic_block_p"
	.size	.L__FUNCTION__.inside_basic_block_p, 21

	.type	.L__FUNCTION__.make_edges,@object # @__FUNCTION__.make_edges
.L__FUNCTION__.make_edges:
	.asciz	"make_edges"
	.size	.L__FUNCTION__.make_edges, 11

	.type	.L__FUNCTION__.make_label_edge,@object # @__FUNCTION__.make_label_edge
.L__FUNCTION__.make_label_edge:
	.asciz	"make_label_edge"
	.size	.L__FUNCTION__.make_label_edge, 16

	.type	.L__FUNCTION__.find_basic_blocks_1,@object # @__FUNCTION__.find_basic_blocks_1
.L__FUNCTION__.find_basic_blocks_1:
	.asciz	"find_basic_blocks_1"
	.size	.L__FUNCTION__.find_basic_blocks_1, 20


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
