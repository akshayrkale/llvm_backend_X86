	.file	"sibcall.c.bc"
	.text
	.globl	replace_call_placeholder
	.align	16, 0x90
	.type	replace_call_placeholder,@function
replace_call_placeholder:               # @replace_call_placeholder
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rsi, %rcx
	andq	%rax, %rcx
	cmpq	$1, %rcx
	je	.LBB0_8
# BB#1:                                 # %entry
	andq	%rax, %rsi
	cmpq	$3, %rsi
	jne	.LBB0_2
# BB#7:                                 # %if.then5
	movq	32(%rbx), %rax
	movq	16(%rax), %rdi
	jmp	.LBB0_4
.LBB0_8:                                # %if.then15
	movq	32(%rbx), %rax
	movq	8(%rax), %rdi
	jmp	.LBB0_4
.LBB0_2:                                # %entry
	cmpq	$2, %rsi
	jne	.LBB0_9
# BB#3:                                 # %if.then
	movq	32(%rbx), %rax
	movq	24(%rax), %rdi
.LBB0_4:                                # %if.end25
	movq	%rbx, %rsi
	callq	emit_insns_before
	movq	32(%rbx), %rax
	movq	32(%rax), %rax
	testq	%rax, %rax
	je	.LBB0_6
# BB#5:                                 # %if.then32
	movl	(%rax), %ecx
	movabsq	$4026531839, %rdx       # imm = 0xEFFFFFFF
	andq	%rcx, %rdx
	movl	%edx, (%rax)
.LBB0_6:                                # %if.end39
	movq	%rbx, %rdi
	popq	%rbx
	jmp	remove_insn  # TAILCALL
.LBB0_9:                                # %if.else23
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.replace_call_placeholder, %rcx
	movq	$550, %rsi              # imm = 0x226
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp4:
	.size	replace_call_placeholder, .Ltmp4-replace_call_placeholder
	.cfi_endproc

	.globl	optimize_sibling_and_tail_recursive_calls
	.align	16, 0x90
	.type	optimize_sibling_and_tail_recursive_calls,@function
optimize_sibling_and_tail_recursive_calls: # @optimize_sibling_and_tail_recursive_calls
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp11:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp12:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp13:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp14:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp15:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp16:
	.cfi_def_cfa_offset 112
.Ltmp17:
	.cfi_offset %rbx, -48
.Ltmp18:
	.cfi_offset %r12, -40
.Ltmp19:
	.cfi_offset %r13, -32
.Ltmp20:
	.cfi_offset %r14, -24
.Ltmp21:
	.cfi_offset %r15, -16
	callq	get_insns
	movq	%rax, %r15
	callq	find_exception_handler_labels
	movq	%r15, %rdi
	callq	rebuild_jump_labels
	callq	max_reg_num
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	find_basic_blocks
	movq	$24, %rdi
	callq	cleanup_cfg
	movl	n_basic_blocks(%rip), %eax
	testq	%rax, %rax
	je	.LBB1_200
# BB#1:                                 # %if.end
	movq	entry_exit_blocks+144(%rip), %r14
	movq	$0, return_value_pseudo(%rip)
	testq	%r14, %r14
	je	.LBB1_2
# BB#3:
	movabsq	$entry_exit_blocks+112, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jmp	.LBB1_4
.LBB1_200:                              # %return
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_2:
	movabsq	$entry_exit_blocks+112, %rbx
	jmp	.LBB1_25
.LBB1_22:                               # %if.else
                                        #   in Loop: Header=BB1_4 Depth=1
	movq	$0, return_value_pseudo(%rip)
	movq	%r12, %rbx
	jmp	.LBB1_23
	.align	16, 0x90
.LBB1_4:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_7 Depth 2
	movq	24(%r14), %rax
	cmpq	%r12, %rax
	movq	%r12, %rbx
	jne	.LBB1_23
# BB#5:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB1_4 Depth=1
	cmpq	$0, 8(%r14)
	movq	%r12, %rbx
	jne	.LBB1_23
# BB#6:                                 # %if.end8
                                        #   in Loop: Header=BB1_4 Depth=1
	movl	n_basic_blocks(%rip), %eax
	decq	%rax
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %rax
	movq	(%rax), %rbx
	jmp	.LBB1_7
	.align	16, 0x90
.LBB1_19:                               # %for.inc
                                        #   in Loop: Header=BB1_7 Depth=2
	movq	24(%rbx), %rbx
.LBB1_7:                                # %if.end8
                                        #   Parent Loop BB1_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB1_21
# BB#8:                                 # %for.body11
                                        #   in Loop: Header=BB1_7 Depth=2
	movzbl	(%rbx), %eax
	leaq	-36(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$2, %rcx
	jb	.LBB1_19
# BB#9:                                 # %for.body11
                                        #   in Loop: Header=BB1_7 Depth=2
	cmpq	$32, %rax
	jne	.LBB1_20
# BB#10:                                # %land.lhs.true
                                        #   in Loop: Header=BB1_7 Depth=2
	movq	32(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB1_19
# BB#11:                                # %land.lhs.true32
                                        #   in Loop: Header=BB1_7 Depth=2
	movzbl	rtx_class+32(%rip), %eax
	cmpq	$105, %rax
	jne	.LBB1_20
# BB#12:                                # %cond.true
                                        #   in Loop: Header=BB1_7 Depth=2
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB1_14
# BB#13:                                # %cond.false
                                        #   in Loop: Header=BB1_7 Depth=2
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB1_14:                               # %cond.end55
                                        #   in Loop: Header=BB1_7 Depth=2
	testq	%rax, %rax
	je	.LBB1_20
# BB#15:                                # %land.lhs.true58
                                        #   in Loop: Header=BB1_7 Depth=2
	movq	cfun(%rip), %rcx
	movq	8(%rax), %rdx
	movq	120(%rcx), %rcx
	cmpq	%rcx, %rdx
	jne	.LBB1_20
# BB#16:                                # %land.lhs.true64
                                        #   in Loop: Header=BB1_7 Depth=2
	movq	16(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB1_20
# BB#17:                                # %land.lhs.true64
                                        #   in Loop: Header=BB1_7 Depth=2
	movq	return_value_pseudo(%rip), %rcx
	testq	%rcx, %rcx
	jne	.LBB1_20
# BB#18:                                # %if.then74
                                        #   in Loop: Header=BB1_7 Depth=2
	movq	%rax, return_value_pseudo(%rip)
	jmp	.LBB1_19
.LBB1_20:                               # %for.end
                                        #   in Loop: Header=BB1_4 Depth=1
	testq	%rbx, %rbx
	jne	.LBB1_22
	.align	16, 0x90
.LBB1_21:                               # %if.then84
                                        #   in Loop: Header=BB1_4 Depth=1
	movq	16(%r14), %rbx
.LBB1_23:                               # %for.inc86
                                        #   in Loop: Header=BB1_4 Depth=1
	movq	(%r14), %r14
	testq	%r14, %r14
	je	.LBB1_25
# BB#24:                                # %for.inc86
                                        #   in Loop: Header=BB1_4 Depth=1
	cmpq	%r12, %rbx
	je	.LBB1_4
.LBB1_25:                               # %for.end87
	movq	%r15, %rdi
	callq	sequence_uses_addressof
	movq	%rax, %r14
	testq	%r15, %r15
	je	.LBB1_199
# BB#26:
	xorq	%r12, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB1_27
.LBB1_144:                              #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	32(%rsp), %r13          # 8-byte Reload
	jmp	.LBB1_154
	.align	16, 0x90
.LBB1_27:                               # %for.body94
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_38 Depth 2
                                        #     Child Loop BB1_40 Depth 2
	movzbl	(%r15), %eax
	cmpq	$34, %rax
	jne	.LBB1_116
# BB#28:                                # %land.lhs.true99
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$141, %rcx
	jne	.LBB1_116
# BB#29:                                # %if.then107
                                        #   in Loop: Header=BB1_27 Depth=1
	testq	%r13, %r14
	jne	.LBB1_112
# BB#30:                                # %lor.lhs.false147
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	cfun(%rip), %rcx
	cmpq	$0, 264(%rcx)
	jne	.LBB1_112
# BB#31:                                # %lor.lhs.false147
                                        #   in Loop: Header=BB1_27 Depth=1
	movl	424(%rcx), %r12d
	movq	%r12, %rcx
	andq	$16, %rcx
	testq	%r13, %rcx
	jne	.LBB1_112
# BB#32:                                # %lor.lhs.false154
                                        #   in Loop: Header=BB1_27 Depth=1
	movslq	8(%r15), %rcx
	movq	basic_block_for_insn(%rip), %rdx
	movq	32(%rdx,%rcx,8), %rcx
	movq	40(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB1_112
# BB#33:                                # %lor.lhs.false157
                                        #   in Loop: Header=BB1_27 Depth=1
	cmpq	$0, 8(%rdx)
	jne	.LBB1_112
# BB#34:                                # %lor.lhs.false162
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rax), %rdi
	movq	24(%rdx), %rdx
	movq	24(%rax), %rsi
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %rdx
	je	.LBB1_36
# BB#35:                                # %lor.lhs.false162
                                        #   in Loop: Header=BB1_27 Depth=1
	cmpq	%rbx, %rdx
	jne	.LBB1_112
.LBB1_36:                               # %lor.lhs.false172
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	8(%rcx), %rax
	movq	16(%rax), %rdi
	callq	next_nonnote_insn
	cmpq	%r15, %rax
	je	.LBB1_167
# BB#37:                                # %if.end.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	32(%r15), %rax
	movq	8(%rax), %rax
	.align	16, 0x90
.LBB1_38:                               # %while.cond.i.i.i
                                        #   Parent Loop BB1_27 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %r13
	movq	24(%r13), %rax
	testq	%rax, %rax
	jne	.LBB1_38
	jmp	.LBB1_40
	.align	16, 0x90
.LBB1_39:                               # %while.body8.i.i.i
                                        #   in Loop: Header=BB1_40 Depth=2
	movq	16(%r13), %r13
.LBB1_40:                               # %while.body8.i.i.i
                                        #   Parent Loop BB1_27 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r13), %eax
	cmpq	$34, %rax
	jne	.LBB1_39
# BB#41:                                # %while.end12.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$39, %rcx
	jne	.LBB1_43
# BB#42:                                # %land.lhs.true40.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
.LBB1_43:                               # %land.lhs.true40.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	cmpq	$47, %rcx
	movq	%r15, %rcx
	jne	.LBB1_109
# BB#44:                                # %land.lhs.true50.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$50, %rcx
	movq	%r15, %rcx
	jne	.LBB1_109
# BB#45:                                # %if.end78.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	8(%rax), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	movq	%r15, %rcx
	jne	.LBB1_109
# BB#46:                                # %if.end83.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%r13, %rdi
	callq	next_nonnote_insn
	movq	%rax, %rdx
	testq	%rdx, %rdx
	je	.LBB1_56
# BB#47:                                # %if.then.i.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movzbl	(%rdx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB1_56
# BB#48:                                # %cond.true.i.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%rdx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB1_50
# BB#49:                                #   in Loop: Header=BB1_27 Depth=1
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	jmp	.LBB1_51
.LBB1_50:                               # %cond.false.i.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%rdx, %rdi
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	%rax, %rsi
	callq	single_set_2
.LBB1_51:                               # %if.end.i.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	testq	%rax, %rax
	movq	16(%rsp), %rdi          # 8-byte Reload
	je	.LBB1_56
# BB#52:                                # %land.lhs.true20.i.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rax), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$75, %rcx
	jne	.LBB1_56
# BB#53:                                # %land.lhs.true28.i.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	8(%rdx), %rcx
	movq	global_rtl+16(%rip), %rsi
	cmpq	%rsi, %rcx
	jne	.LBB1_56
# BB#54:                                # %land.lhs.true37.i.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rdx), %rdx
	movzbl	(%rdx), %edx
	cmpq	$54, %rdx
	jne	.LBB1_56
# BB#55:                                # %land.lhs.true48.i.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	8(%rax), %rax
	cmpq	%rcx, %rax
	je	.LBB1_57
.LBB1_56:                               # %if.end55.i.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%r13, %rdi
.LBB1_57:                               # %skip_stack_adjustment.exit.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	testq	%rdi, %rdi
	movq	%r15, %rcx
	je	.LBB1_109
# BB#58:                                # %if.end86.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	callq	next_nonnote_insn
	movq	%rax, %r13
	testq	%r13, %r13
	je	.LBB1_64
# BB#59:                                # %if.then.i241.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB1_64
# BB#60:                                # %cond.true.i247.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB1_62
# BB#61:                                # %cond.false.i249.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB1_62:                               # %if.end.i252.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	testq	%rax, %rax
	je	.LBB1_64
# BB#63:                                # %land.lhs.true20.i255.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	8(%rax), %rax
	movq	pic_offset_table_rtx(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB1_65
.LBB1_64:                               # %if.end27.i.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rsp), %r13          # 8-byte Reload
.LBB1_65:                               # %skip_pic_restore.exit.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	testq	%r13, %r13
	movq	%r15, %rcx
	je	.LBB1_109
# BB#66:                                # %if.end90.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	24(%r13), %r13
	testq	%r13, %r13
	movq	%r15, %rcx
	je	.LBB1_109
# BB#67:                                # %if.end96.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	movq	%r15, %rcx
	jne	.LBB1_109
# BB#68:                                # %cond.true.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB1_70
# BB#69:                                # %cond.false.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB1_70:                               # %cond.end118.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	testq	%rax, %rax
	movq	%r15, %rcx
	je	.LBB1_109
# BB#71:                                # %lor.lhs.false.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rax), %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %rcx
	movq	%r15, %rcx
	jne	.LBB1_109
# BB#72:                                # %if.end127.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	24(%r13), %r13
	movq	8(%rax), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	testq	%r13, %r13
	jne	.LBB1_74
# BB#73:                                #   in Loop: Header=BB1_27 Depth=1
	xorq	%r13, %r13
	jmp	.LBB1_80
.LBB1_74:                               # %land.lhs.true135.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB1_80
# BB#75:                                # %cond.true143.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB1_77
# BB#76:                                # %cond.false155.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB1_77:                               # %cond.end163.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	testq	%rax, %rax
	je	.LBB1_80
# BB#78:                                # %land.lhs.true167.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rax), %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %rcx
	jne	.LBB1_80
# BB#79:                                # %if.then173.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	8(%rax), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	24(%r13), %r13
.LBB1_80:                               # %if.end180.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	movq	%r15, %rcx
	jne	.LBB1_109
# BB#81:                                # %lor.lhs.false185.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %eax
	cmpq	$53, %rax
	movq	%r15, %rcx
	jb	.LBB1_109
# BB#82:                                # %if.end191.i.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	xorq	%rdx, %rdx
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	reg_set_between_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%r15, %rcx
	jne	.LBB1_109
# BB#83:                                # %if.end.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%r15, %rdi
	callq	next_nonnote_insn
	movq	%rax, %r13
	testq	%r13, %r13
	movq	%r15, %rcx
	je	.LBB1_109
# BB#84:                                # %if.end4.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	movq	%r15, %rcx
	jne	.LBB1_109
# BB#85:                                # %cond.true.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB1_87
# BB#86:                                # %cond.false.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB1_87:                               # %cond.end23.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	testq	%rax, %rax
	movq	%r15, %rcx
	je	.LBB1_109
# BB#88:                                # %if.end27.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	return_value_pseudo(%rip), %rcx
	movq	8(%rax), %rsi
	testq	%rcx, %rcx
	jne	.LBB1_89
# BB#91:                                # %if.end42.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	cfun(%rip), %rcx
	movq	120(%rcx), %rdx
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movzbl	(%rsi), %ecx
	cmpq	%rdx, %rsi
	jne	.LBB1_96
# BB#92:                                # %if.end42.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	cmpq	$61, %rcx
	jne	.LBB1_96
# BB#93:                                # %land.lhs.true56.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	8(%rsp), %rdx           # 8-byte Reload
	movl	8(%rdx), %edx
	movq	24(%rsp), %rsi          # 8-byte Reload
	movl	8(%rsi), %esi
	cmpq	%rsi, %rdx
	jne	.LBB1_96
# BB#94:                                # %land.lhs.true67.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rax), %rdx
	movq	16(%rsp), %rsi          # 8-byte Reload
	cmpq	%rsi, %rdx
	jne	.LBB1_96
# BB#95:                                #   in Loop: Header=BB1_27 Depth=1
	movq	%r13, %rcx
	jmp	.LBB1_109
.LBB1_89:                               # %if.then29.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	cmpq	%rcx, %rsi
	movq	%r15, %rcx
	jne	.LBB1_109
# BB#90:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rax), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB1_107
.LBB1_96:                               # %if.end74.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	cmpq	$61, %rcx
	jne	.LBB1_108
# BB#97:                                # %land.lhs.true82.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rax), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB1_108
# BB#98:                                # %if.then88.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%r13, %rdi
	callq	next_nonnote_insn
	movq	%rax, %r13
	testq	%r13, %r13
	movq	%r15, %rcx
	je	.LBB1_109
# BB#99:                                # %if.end95.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	movq	%r15, %rcx
	jne	.LBB1_109
# BB#100:                               # %cond.true103.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB1_102
# BB#101:                               # %cond.false115.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB1_102:                              # %cond.end123.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	testq	%rax, %rax
	movq	%r15, %rcx
	je	.LBB1_109
# BB#103:                               # %if.end127.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	cfun(%rip), %rdx
	movq	8(%rax), %rcx
	movq	120(%rdx), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB1_108
# BB#104:                               # %land.lhs.true134.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB1_108
# BB#105:                               # %land.lhs.true142.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movl	8(%rcx), %ecx
	movq	24(%rsp), %rdx          # 8-byte Reload
	movl	8(%rdx), %edx
	cmpq	%rdx, %rcx
	jne	.LBB1_108
# BB#106:                               # %land.lhs.true154.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rax), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
.LBB1_107:                              # %land.lhs.true154.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	cmpq	%rcx, %rax
	movq	%r13, %rcx
	je	.LBB1_109
.LBB1_108:                              # %if.end162.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%r15, %rcx
.LBB1_109:                              # %skip_copy_to_return_value.exit.i
                                        #   in Loop: Header=BB1_27 Depth=1
	cmpq	$0, return_value_pseudo(%rip)
	je	.LBB1_119
# BB#110:                               # %skip_copy_to_return_value.exit.i
                                        #   in Loop: Header=BB1_27 Depth=1
	cmpq	%r15, %rcx
	je	.LBB1_111
.LBB1_119:                              # %if.end4.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %rcx
	je	.LBB1_167
# BB#120:                               # %if.end7.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%rcx, %rdi
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	callq	next_nonnote_insn
	movq	%rax, %r13
	testq	%r13, %r13
	je	.LBB1_129
# BB#121:                               # %if.then.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB1_129
# BB#122:                               # %cond.true.i52.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB1_124
# BB#123:                               # %cond.false.i53.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB1_124:                              # %if.end.i54.i
                                        #   in Loop: Header=BB1_27 Depth=1
	testq	%rax, %rax
	je	.LBB1_129
# BB#125:                               # %land.lhs.true20.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rax), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$75, %rcx
	jne	.LBB1_129
# BB#126:                               # %land.lhs.true28.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	8(%rdx), %rcx
	movq	global_rtl+16(%rip), %rsi
	cmpq	%rsi, %rcx
	jne	.LBB1_129
# BB#127:                               # %land.lhs.true37.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rdx), %rdx
	movzbl	(%rdx), %edx
	cmpq	$54, %rdx
	jne	.LBB1_129
# BB#128:                               # %land.lhs.true48.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	8(%rax), %rax
	cmpq	%rcx, %rax
	je	.LBB1_130
.LBB1_129:                              # %if.end55.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	24(%rsp), %r13          # 8-byte Reload
.LBB1_130:                              # %skip_stack_adjustment.exit.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r13
	je	.LBB1_167
# BB#131:                               # %if.end11.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%r13, %rdi
	callq	next_nonnote_insn
	testq	%rax, %rax
	je	.LBB1_136
# BB#132:                               # %land.lhs.true.i62.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$32, %rcx
	jne	.LBB1_136
# BB#133:                               # %land.lhs.true1.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%rax), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$49, %rcx
	jne	.LBB1_136
# BB#134:                               # %land.lhs.true5.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	cfun(%rip), %rcx
	movq	8(%rdx), %rdx
	movq	120(%rcx), %rcx
	cmpq	%rcx, %rdx
	je	.LBB1_137
# BB#135:                               # %land.lhs.true5.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rdx
	je	.LBB1_137
.LBB1_136:                              # %if.end.i68.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%r13, %rax
.LBB1_137:                              # %skip_use_of_return_value.exit.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB1_167
# BB#138:                               # %if.end15.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%rcx, %r13
	movq	%rax, %rdi
	movq	%rax, 24(%rsp)          # 8-byte Spill
	callq	next_nonnote_insn
	testq	%rax, %rax
	je	.LBB1_153
# BB#139:                               # %land.lhs.true.i75.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$32, %rcx
	jne	.LBB1_153
# BB#140:                               # %land.lhs.true1.i81.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%rax), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$49, %rcx
	jne	.LBB1_153
# BB#141:                               # %land.lhs.true5.i84.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	8(%rdx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	movq	24(%rsp), %rdi          # 8-byte Reload
	jne	.LBB1_154
# BB#142:                               # %land.lhs.true15.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movl	8(%rax), %eax
	cmpq	$53, %rax
	jb	.LBB1_154
# BB#143:                               # %if.then.i86.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	next_nonnote_insn
	movq	%rax, 24(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB1_144
# BB#145:                               # %if.end.i87.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB1_144
# BB#146:                               # %cond.true.i89.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB1_148
# BB#147:                               # %cond.false.i90.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	single_set_2
.LBB1_148:                              # %cond.end49.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	testq	%rax, %rax
	je	.LBB1_144
# BB#149:                               # %lor.lhs.false.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	32(%rcx), %rcx
	movq	16(%rax), %rdx
	movq	8(%rcx), %rcx
	cmpq	%rcx, %rdx
	jne	.LBB1_144
# BB#150:                               # %lor.lhs.false63.i.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	cfun(%rip), %rcx
	movq	8(%rax), %rax
	movq	120(%rcx), %rcx
	cmpq	%rcx, %rax
	je	.LBB1_152
# BB#151:                               # %select.mid
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, 24(%rsp)          # 8-byte Spill
.LBB1_152:                              # %select.end
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%rsp), %r13          # 8-byte Reload
.LBB1_153:                              # %skip_unreturned_value.exit.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	24(%rsp), %rdi          # 8-byte Reload
.LBB1_154:                              # %skip_unreturned_value.exit.i
                                        #   in Loop: Header=BB1_27 Depth=1
	cmpq	%r13, %rdi
	je	.LBB1_167
# BB#155:                               # %if.end19.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	callq	next_nonnote_insn
	movq	%rax, %r13
	testq	%r13, %r13
	je	.LBB1_160
# BB#156:                               # %land.lhs.true.i99.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movzbl	(%r13), %eax
	cmpq	$32, %rax
	jne	.LBB1_160
# BB#157:                               # %land.lhs.true1.i105.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$48, %rcx
	jne	.LBB1_160
# BB#158:                               # %land.lhs.true5.i112.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	cfun(%rip), %rcx
	movq	8(%rax), %rax
	movq	120(%rcx), %rcx
	cmpq	%rcx, %rax
	je	.LBB1_161
# BB#159:                               # %land.lhs.true5.i112.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB1_161
.LBB1_160:                              # %if.end.i113.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	24(%rsp), %r13          # 8-byte Reload
.LBB1_161:                              # %skip_use_of_return_value.exit116.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r13
	je	.LBB1_167
# BB#162:                               # %if.end23.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%r13, %rdi
	callq	next_nonnote_insn
	testq	%rax, %rax
	je	.LBB1_165
# BB#163:                               # %land.lhs.true.i122.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB1_165
# BB#164:                               # %land.lhs.true1.i123.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%rax, %rdi
	movq	%rax, 24(%rsp)          # 8-byte Spill
	callq	any_uncondjump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	jne	.LBB1_166
.LBB1_165:                              # %if.end.i124.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%r13, %rcx
.LBB1_166:                              # %call_ends_block_p.exit
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %rcx
	jne	.LBB1_111
.LBB1_167:                              # %if.end176
                                        #   in Loop: Header=BB1_27 Depth=1
	andq	$196672, %r12           # imm = 0x30040
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %r12
	movabsq	$0, %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	jne	.LBB1_169
# BB#168:                               # %if.end176
                                        #   in Loop: Header=BB1_27 Depth=1
	movabsq	$1, %rax
.LBB1_169:                              # %if.end176
                                        #   in Loop: Header=BB1_27 Depth=1
	testq	%rcx, %rcx
	movabsq	$0, %rcx
	movq	40(%rsp), %rdx          # 8-byte Reload
	je	.LBB1_171
# BB#170:                               # %if.end176
                                        #   in Loop: Header=BB1_27 Depth=1
	movabsq	$1, %rcx
.LBB1_171:                              # %if.end176
                                        #   in Loop: Header=BB1_27 Depth=1
	andq	%rcx, %rax
	movq	$1, %r12
	jne	.LBB1_173
# BB#172:                               # %if.end176
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	56(%rsp), %r12          # 8-byte Reload
.LBB1_173:                              # %if.end176
                                        #   in Loop: Header=BB1_27 Depth=1
	testq	%rdx, %rdx
	jne	.LBB1_176
# BB#174:                               # %cond.end187
                                        #   in Loop: Header=BB1_27 Depth=1
	testq	$1, %rax
	je	.LBB1_175
# BB#178:                               # %if.then5.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%r15), %rax
	movq	16(%rax), %rdi
	jmp	.LBB1_177
.LBB1_176:                              # %if.then.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%r15), %rax
	movq	24(%rax), %rdi
.LBB1_177:                              # %if.end25.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%r15, %rsi
	callq	emit_insns_before
	movq	%r12, 56(%rsp)          # 8-byte Spill
	jmp	.LBB1_113
.LBB1_111:                              #   in Loop: Header=BB1_27 Depth=1
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jmp	.LBB1_112
.LBB1_175:                              #   in Loop: Header=BB1_27 Depth=1
	movq	%r12, 56(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB1_112:                              # %if.then15.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%r15), %rax
	movq	8(%rax), %rdi
	movq	%r15, %rsi
	callq	emit_insns_before
.LBB1_113:                              # %if.end25.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	32(%r15), %rax
	movq	32(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_115
# BB#114:                               # %if.then32.i
                                        #   in Loop: Header=BB1_27 Depth=1
	movl	(%rax), %ecx
	movabsq	$4026531839, %rdx       # imm = 0xEFFFFFFF
	andq	%rdx, %rcx
	movl	%ecx, (%rax)
.LBB1_115:                              # %replace_call_placeholder.exit
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	%r15, %rdi
	callq	remove_insn
	movq	$1, %r12
.LBB1_116:                              # %for.inc190
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	24(%r15), %r15
	testq	%r15, %r15
	jne	.LBB1_27
# BB#117:                               # %for.end194
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r13, %rax
	je	.LBB1_197
# BB#118:                               # %if.then196
	callq	get_insns
	movq	%rax, %rbx
	jmp	.LBB1_182
	.align	16, 0x90
.LBB1_181:                              # %for.inc.i
                                        #   in Loop: Header=BB1_182 Depth=1
	movq	24(%rbx), %rbx
.LBB1_182:                              # %for.inc.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_180 Depth 2
	testq	%rbx, %rbx
	jne	.LBB1_180
	jmp	.LBB1_183
	.align	16, 0x90
.LBB1_179:                              # %if.then.i313
                                        #   in Loop: Header=BB1_180 Depth=2
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	remove_note
.LBB1_180:                              # %if.then.i313
                                        #   Parent Loop BB1_182 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$3, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB1_179
	jmp	.LBB1_181
.LBB1_183:                              # %purge_reg_equiv_notes.exit
	callq	get_insns
	movq	%rax, %rbx
	jmp	.LBB1_184
	.align	16, 0x90
.LBB1_187:                              # %for.inc214
                                        #   in Loop: Header=BB1_184 Depth=1
	movq	24(%rbx), %rbx
.LBB1_184:                              # %purge_reg_equiv_notes.exit
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB1_188
# BB#185:                               # %for.body201
                                        #   in Loop: Header=BB1_184 Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB1_187
# BB#186:                               # %if.then209
                                        #   in Loop: Header=BB1_184 Depth=1
	movq	32(%rbx), %rdi
	callq	purge_mem_unchanging_flag
	jmp	.LBB1_187
.LBB1_188:                              # %for.end218
	movq	current_function_decl(%rip), %rax
	movq	88(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB1_197
# BB#189:
	movabsq	$4227858431, %r14       # imm = 0xFBFFFFFF
	.align	16, 0x90
.LBB1_190:                              # %for.body221
                                        # =>This Inner Loop Header: Depth=1
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB1_192
# BB#191:                               # %cond.false227
                                        #   in Loop: Header=BB1_190 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB1_192:                              # %cond.end230
                                        #   in Loop: Header=BB1_190 Depth=1
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB1_196
# BB#193:                               # %if.then236
                                        #   in Loop: Header=BB1_190 Depth=1
	testq	%rax, %rax
	jne	.LBB1_195
# BB#194:                               # %cond.false243
                                        #   in Loop: Header=BB1_190 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movl	(%rax), %ecx
.LBB1_195:                              # %cond.end246
                                        #   in Loop: Header=BB1_190 Depth=1
	andq	%r14, %rcx
	movl	%ecx, (%rax)
.LBB1_196:                              # %for.inc251
                                        #   in Loop: Header=BB1_190 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB1_190
.LBB1_197:                              # %if.end253
	testq	%r13, %r12
	je	.LBB1_199
# BB#198:                               # %if.then255
	callq	reorder_blocks
.LBB1_199:                              # %if.end256
	xorq	%rdi, %rdi
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free_basic_block_vars  # TAILCALL
.Ltmp22:
	.size	optimize_sibling_and_tail_recursive_calls, .Ltmp22-optimize_sibling_and_tail_recursive_calls
	.cfi_endproc

	.align	16, 0x90
	.type	sequence_uses_addressof,@function
sequence_uses_addressof:                # @sequence_uses_addressof
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp28:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp29:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp30:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp31:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp32:
	.cfi_def_cfa_offset 48
.Ltmp33:
	.cfi_offset %rbx, -40
.Ltmp34:
	.cfi_offset %r12, -32
.Ltmp35:
	.cfi_offset %r14, -24
.Ltmp36:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB2_15
# BB#1:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB2_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %ecx
	cmpq	$105, %rcx
	jne	.LBB2_14
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	32(%rbx), %r14
	cmpq	$34, %rax
	jne	.LBB2_11
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB2_2 Depth=1
	movzbl	(%r14), %eax
	cmpq	$141, %rax
	jne	.LBB2_11
# BB#5:                                 # %if.then11
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	8(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB2_7
# BB#6:                                 # %land.lhs.true20
                                        #   in Loop: Header=BB2_2 Depth=1
	callq	sequence_uses_addressof
	testq	%r12, %rax
	movq	$1, %r15
	jne	.LBB2_16
.LBB2_7:                                # %if.end
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	16(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB2_9
# BB#8:                                 # %land.lhs.true37
                                        #   in Loop: Header=BB2_2 Depth=1
	callq	sequence_uses_addressof
	testq	%r12, %rax
	movq	$1, %r15
	jne	.LBB2_16
.LBB2_9:                                # %if.end47
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	24(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB2_14
# BB#10:                                # %land.lhs.true56
                                        #   in Loop: Header=BB2_2 Depth=1
	callq	sequence_uses_addressof
	testq	%r12, %rax
	movq	$1, %r15
	je	.LBB2_14
	jmp	.LBB2_16
	.align	16, 0x90
.LBB2_11:                               # %if.else
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	%r14, %rdi
	callq	uses_addressof
	testq	%r12, %rax
	movq	$1, %r15
	jne	.LBB2_16
# BB#12:                                # %lor.lhs.false
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	56(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB2_14
# BB#13:                                # %land.lhs.true76
                                        #   in Loop: Header=BB2_2 Depth=1
	callq	uses_addressof
	testq	%r12, %rax
	jne	.LBB2_16
	.align	16, 0x90
.LBB2_14:                               # %for.inc
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB2_2
.LBB2_15:
	xorq	%r15, %r15
.LBB2_16:                               # %return
	movq	%r15, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp37:
	.size	sequence_uses_addressof, .Ltmp37-sequence_uses_addressof
	.cfi_endproc

	.align	16, 0x90
	.type	purge_mem_unchanging_flag,@function
purge_mem_unchanging_flag:              # @purge_mem_unchanging_flag
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp44:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp45:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp46:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp47:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp48:
	.cfi_def_cfa_offset 48
.Ltmp49:
	.cfi_offset %rbx, -48
.Ltmp50:
	.cfi_offset %r12, -40
.Ltmp51:
	.cfi_offset %r13, -32
.Ltmp52:
	.cfi_offset %r14, -24
.Ltmp53:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	testq	%r15, %r15
	je	.LBB3_15
# BB#1:                                 # %if.end
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB3_8
# BB#2:                                 # %if.then2
	testq	$67108864, %rax         # imm = 0x4000000
	je	.LBB3_15
# BB#3:                                 # %land.lhs.true
	movq	cfun(%rip), %rdx
	movq	8(%r15), %rcx
	movq	128(%rdx), %rdx
	cmpq	%rdx, %rcx
	je	.LBB3_7
# BB#4:                                 # %lor.lhs.false
	movzbl	(%rcx), %esi
	cmpq	$75, %rsi
	jne	.LBB3_15
# BB#5:                                 # %land.lhs.true12
	movq	8(%rcx), %rsi
	cmpq	%rdx, %rsi
	jne	.LBB3_15
# BB#6:                                 # %land.lhs.true21
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$54, %rcx
	jne	.LBB3_15
.LBB3_7:                                # %if.then31
	movabsq	$4227858431, %rcx       # imm = 0xFBFFFFFF
	andq	%rcx, %rax
	movl	%eax, (%r15)
	jmp	.LBB3_15
.LBB3_8:                                # %if.end35
	movzbl	rtx_length(%rcx), %r14d
	testq	%r14, %r14
	je	.LBB3_15
# BB#9:                                 # %for.body.lr.ph
	movq	rtx_format(,%rcx,8), %r12
	xorq	%r13, %r13
	.align	16, 0x90
.LBB3_10:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_13 Depth 2
	movzbl	(%r12), %eax
	cmpq	$101, %rax
	jne	.LBB3_11
# BB#16:                                # %if.then44
                                        #   in Loop: Header=BB3_10 Depth=1
	movq	8(%r15,%r13,8), %rdi
	callq	purge_mem_unchanging_flag
	jmp	.LBB3_14
	.align	16, 0x90
.LBB3_11:                               # %for.body
                                        #   in Loop: Header=BB3_10 Depth=1
	cmpq	$69, %rax
	jne	.LBB3_14
# BB#12:                                # %for.cond53.preheader
                                        #   in Loop: Header=BB3_10 Depth=1
	movq	8(%r15,%r13,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %rbx
	jle	.LBB3_14
	.align	16, 0x90
.LBB3_13:                               # %for.body59
                                        #   Parent Loop BB3_10 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%rbx,8), %rdi
	callq	purge_mem_unchanging_flag
	movq	8(%r15,%r13,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB3_13
	.align	16, 0x90
.LBB3_14:                               # %for.inc68
                                        #   in Loop: Header=BB3_10 Depth=1
	incq	%r13
	incq	%r12
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r14, %rax
	jl	.LBB3_10
.LBB3_15:                               # %for.end70
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp54:
	.size	purge_mem_unchanging_flag, .Ltmp54-purge_mem_unchanging_flag
	.cfi_endproc

	.align	16, 0x90
	.type	uses_addressof,@function
uses_addressof:                         # @uses_addressof
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp61:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp62:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp63:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp64:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp65:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp66:
	.cfi_def_cfa_offset 80
.Ltmp67:
	.cfi_offset %rbx, -48
.Ltmp68:
	.cfi_offset %r12, -40
.Ltmp69:
	.cfi_offset %r13, -32
.Ltmp70:
	.cfi_offset %r14, -24
.Ltmp71:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	xorq	%rbx, %rbx
	testq	%r14, %r14
	je	.LBB4_16
# BB#1:                                 # %if.end
	movzbl	(%r14), %eax
	cmpq	$70, %rax
	jne	.LBB4_3
# BB#2:
	movq	$1, %rbx
	jmp	.LBB4_16
.LBB4_3:                                # %lor.lhs.false
	movq	cfun(%rip), %rcx
	movq	128(%rcx), %rcx
	cmpq	%r14, %rcx
	je	.LBB4_4
# BB#5:                                 # %if.end4
	cmpq	$66, %rax
	je	.LBB4_16
# BB#6:                                 # %if.end7
	movzbl	rtx_length(%rax), %ecx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	testq	%rcx, %rcx
	je	.LBB4_16
# BB#7:                                 # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %r12
	xorq	%r13, %r13
	.align	16, 0x90
.LBB4_8:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_15 Depth 2
	movzbl	(%r12), %eax
	cmpq	$101, %rax
	jne	.LBB4_9
# BB#12:                                # %if.then15
                                        #   in Loop: Header=BB4_8 Depth=1
	movq	8(%r14,%r13,8), %rdi
	callq	uses_addressof
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB4_11
	jmp	.LBB4_13
	.align	16, 0x90
.LBB4_9:                                # %for.body
                                        #   in Loop: Header=BB4_8 Depth=1
	cmpq	$69, %rax
	jne	.LBB4_11
# BB#10:                                # %for.cond24.preheader
                                        #   in Loop: Header=BB4_8 Depth=1
	movq	8(%r14,%r13,8), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movslq	(%rax), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movq	$1, %rbx
	movq	$1, %r15
	jle	.LBB4_11
	.align	16, 0x90
.LBB4_15:                               # %for.body30
                                        #   Parent Loop BB4_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r15,8), %rdi
	callq	uses_addressof
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB4_16
# BB#14:                                # %for.cond24
                                        #   in Loop: Header=BB4_15 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r15
	movq	16(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jl	.LBB4_15
	.align	16, 0x90
.LBB4_11:                               # %for.inc43
                                        #   in Loop: Header=BB4_8 Depth=1
	incq	%r13
	incq	%r12
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rbx, %rbx
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	%rcx, %rax
	jl	.LBB4_8
	jmp	.LBB4_16
.LBB4_4:
	movq	$1, %rbx
.LBB4_16:                               # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB4_13:
	movq	$1, %rbx
	jmp	.LBB4_16
.Ltmp72:
	.size	uses_addressof, .Ltmp72-uses_addressof
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"sibcall.c"
	.size	.L.str, 10

	.type	.L__FUNCTION__.replace_call_placeholder,@object # @__FUNCTION__.replace_call_placeholder
.L__FUNCTION__.replace_call_placeholder:
	.asciz	"replace_call_placeholder"
	.size	.L__FUNCTION__.replace_call_placeholder, 25

	.type	return_value_pseudo,@object # @return_value_pseudo
	.local	return_value_pseudo
	.comm	return_value_pseudo,8,8

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
