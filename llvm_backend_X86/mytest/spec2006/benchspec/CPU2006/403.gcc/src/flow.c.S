	.file	"flow.c.bc"
	.text
	.globl	check_function_return_warnings
	.align	16, 0x90
	.type	check_function_return_warnings,@function
check_function_return_warnings:         # @check_function_return_warnings
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	movl	warn_missing_noreturn(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_6
# BB#1:                                 # %land.lhs.true
	movq	cfun(%rip), %rax
	movq	48(%rax), %rdi
	movl	16(%rdi), %eax
	testq	$2048, %rax             # imm = 0x800
	jne	.LBB0_6
# BB#2:                                 # %land.lhs.true
	movq	entry_exit_blocks+144(%rip), %rax
	testq	%rax, %rax
	jne	.LBB0_6
# BB#3:                                 # %land.lhs.true
	movq	lang_missing_noreturn_ok_p(%rip), %rax
	testq	%rax, %rax
	je	.LBB0_6
# BB#4:                                 # %land.lhs.true5
	callq	*%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_6
# BB#5:                                 # %if.then
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB0_6:                                # %if.end
	movq	cfun(%rip), %rax
	movq	48(%rax), %rcx
	movl	16(%rcx), %edx
	movq	entry_exit_blocks+144(%rip), %rcx
	testq	$2048, %rdx             # imm = 0x800
	je	.LBB0_10
# BB#7:                                 # %if.end
	testq	%rcx, %rcx
	je	.LBB0_10
# BB#8:                                 # %if.then17
	movabsq	$.L.str1, %rdi
.LBB0_9:                                # %if.then17
	xorq	%rax, %rax
	popq	%rdx
	jmp	warning  # TAILCALL
.LBB0_10:                               # %if.else
	movl	warn_return_type(%rip), %edx
	testq	%rdx, %rdx
	je	.LBB0_16
# BB#11:                                # %land.lhs.true19
	cmpq	$0, 256(%rax)
	je	.LBB0_16
# BB#12:                                # %land.lhs.true19
	testq	%rcx, %rcx
	je	.LBB0_16
# BB#13:                                # %if.then23
	callq	get_max_uid
	movq	cfun(%rip), %rcx
	movq	256(%rcx), %rdx
	movq	$32, %rcx
	movslq	8(%rdx), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jge	.LBB0_16
# BB#14:                                # %if.then27
	movq	basic_block_for_insn(%rip), %rax
	cmpq	$0, 32(%rax,%rdx,8)
	je	.LBB0_16
# BB#15:                                # %if.then34
	movabsq	$.L.str2, %rdi
	jmp	.LBB0_9
.LBB0_16:                               # %if.end38
	popq	%rax
	retq
.Ltmp2:
	.size	check_function_return_warnings, .Ltmp2-check_function_return_warnings
	.cfi_endproc

	.globl	first_insn_after_basic_block_note
	.align	16, 0x90
	.type	first_insn_after_basic_block_note,@function
first_insn_after_basic_block_note:      # @first_insn_after_basic_block_note
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp4:
	.cfi_def_cfa_offset 16
	movq	(%rdi), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB1_6
# BB#1:                                 # %if.end
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$36, %rdx
	jne	.LBB1_3
# BB#2:                                 # %if.then2
	movq	24(%rcx), %rcx
	movl	(%rcx), %eax
.LBB1_3:                                # %if.end3
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$37, %rax
	jne	.LBB1_7
# BB#4:                                 # %land.lhs.true
	movl	40(%rcx), %eax
	movabsq	$4294967216, %rdx       # imm = 0xFFFFFFB0
	cmpq	%rdx, %rax
	jne	.LBB1_7
# BB#5:                                 # %if.end11
	movq	24(%rcx), %rax
.LBB1_6:                                # %return
	popq	%rdx
	retq
.LBB1_7:                                # %if.then10
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.first_insn_after_basic_block_note, %rcx
	movq	$400, %rsi              # imm = 0x190
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp5:
	.size	first_insn_after_basic_block_note, .Ltmp5-first_insn_after_basic_block_note
	.cfi_endproc

	.globl	life_analysis
	.align	16, 0x90
	.type	life_analysis,@function
life_analysis:                          # @life_analysis
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp12:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp13:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp14:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp15:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp16:
	.cfi_def_cfa_offset 48
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
	movl	optimize(%rip), %eax
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$1114112, elim_reg_set(%rip) # imm = 0x110000
	testq	%rax, %rax
	jne	.LBB2_2
# BB#1:
	andq	$-99, %r15
.LBB2_2:                                # %entry
	movl	reload_completed(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB2_4
# BB#3:                                 # %select.mid
	andq	$-69, %r15
.LBB2_4:                                # %select.end
	movq	%r15, %r13
	andq	$16, %r13
	testq	%rax, %rax
	je	.LBB2_7
# BB#5:                                 # %select.end
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	je	.LBB2_7
# BB#6:                                 # %if.then8
	callq	init_alias_analysis
.LBB2_7:                                # %if.end9
	callq	delete_noop_moves
	xorq	%rdi, %rdi
	callq	purge_all_dead_edges
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	jne	.LBB2_14
# BB#8:                                 # %if.then11
	movq	cfun(%rip), %rax
	movl	424(%rax), %eax
	andq	$64, %rax
	movq	$6, %rcx
	shrq	%cl, %rax
	movq	%rax, %rcx
	xorq	$1, %rcx
	testq	%rax, %rax
	movl	%ecx, current_function_sp_is_unchanging(%rip)
	jne	.LBB2_14
# BB#9:                                 # %if.then11
	testq	%rbx, %rbx
	je	.LBB2_14
# BB#10:
	movabsq	$notice_stack_pointer_modification_1, %r12
	.align	16, 0x90
.LBB2_11:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB2_13
# BB#12:                                # %if.then6.i
                                        #   in Loop: Header=BB2_11 Depth=1
	movq	32(%rbx), %rdi
	xorq	%rdx, %rdx
	movq	%r12, %rsi
	callq	note_stores
	movl	current_function_sp_is_unchanging(%rip), %eax
	testq	%rax, %rax
	je	.LBB2_14
.LBB2_13:                               # %for.inc.i
                                        #   in Loop: Header=BB2_11 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB2_11
.LBB2_14:                               # %if.end12
	callq	max_reg_num
	movl	%eax, max_regno(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
	movq	%rax, %rdi
	callq	allocate_reg_info
	movslq	max_regno(%rip), %rax
	testq	%rax, %rax
	jle	.LBB2_17
# BB#15:                                # %for.body.lr.ph.i
	movq	reg_n_info(%rip), %rax
	movq	$4, %rdx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB2_16:                               # %for.body.i38
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rdx,8), %rcx
	movl	%ebx, 12(%rcx)
	movl	%ebx, 16(%rcx)
	movl	%ebx, 24(%rcx)
	movl	%ebx, 32(%rcx)
	movl	%ebx, 28(%rcx)
	movl	%r8d, 36(%rcx)
	movslq	max_regno(%rip), %rdi
	leaq	-3(%rdx), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rdx
	cmpq	%rdi, %rsi
	jl	.LBB2_16
.LBB2_17:                               # %allocate_reg_life_data.exit
	callq	allocate_bb_life_data
	movl	reload_completed(%rip), %eax
	movq	entry_exit_blocks+176(%rip), %r12
	testq	%rax, %rax
	jne	.LBB2_26
# BB#18:                                # %lor.lhs.false.i
	callq	ix86_frame_pointer_required
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_19
# BB#25:                                # %lor.lhs.false5.i
	movl	current_function_sp_is_unchanging(%rip), %eax
	testq	%rax, %rax
	jne	.LBB2_26
	jmp	.LBB2_27
.LBB2_19:                               # %land.lhs.true.i
	movq	cfun(%rip), %rax
	movzbl	424(%rax), %eax
	testq	$64, %rax
	movabsq	$0, %rax
	movabsq	$1, %rsi
	movq	%rax, %rdx
	jne	.LBB2_21
# BB#20:                                # %land.lhs.true.i
	movq	%rsi, %rdx
.LBB2_21:                               # %land.lhs.true.i
	movl	flag_omit_frame_pointer(%rip), %edi
	testq	%rdi, %rdi
	je	.LBB2_23
# BB#22:                                # %land.lhs.true.i
	movq	%rsi, %rax
.LBB2_23:                               # %land.lhs.true.i
	testq	%rax, %rdx
	jne	.LBB2_26
# BB#24:                                # %land.lhs.true.i
	movl	current_function_sp_is_unchanging(%rip), %eax
	testq	%rcx, %rax
	je	.LBB2_27
.LBB2_26:                               # %if.then.i
	movq	$7, %rsi
	movq	%r12, %rdi
	callq	bitmap_set_bit
.LBB2_27:                               # %if.end.i
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB2_29
# BB#28:                                # %if.end.i
	movl	frame_pointer_needed(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_30
.LBB2_29:                               # %if.then10.i
	movq	$20, %rsi
	movq	%r12, %rdi
	callq	bitmap_set_bit
	movq	$6, %rsi
	movq	%r12, %rdi
	callq	bitmap_set_bit
.LBB2_30:                               # %if.end11.i
	movl	flag_pic(%rip), %eax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB2_34
# BB#31:                                # %if.end11.i
	movl	target_flags(%rip), %eax
	andq	$33554432, %rax         # imm = 0x2000000
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_34
# BB#32:                                # %if.end11.i
	movzbl	fixed_regs+3(%rip), %eax
	testq	$255, %rax
	je	.LBB2_34
# BB#33:                                # %lor.end29.i
	movq	$3, %rsi
	movq	%r12, %rdi
	callq	bitmap_set_bit
	.align	16, 0x90
.LBB2_34:                               # %for.body.i45
                                        # =>This Inner Loop Header: Depth=1
	movzbl	global_regs(%rbx), %eax
	testq	%rax, %rax
	je	.LBB2_36
# BB#35:                                # %if.then38.i
                                        #   in Loop: Header=BB2_34 Depth=1
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	bitmap_set_bit
.LBB2_36:                               # %for.inc.i46
                                        #   in Loop: Header=BB2_34 Depth=1
	incq	%rbx
	cmpq	$53, %rbx
	jne	.LBB2_34
# BB#37:                                # %for.end.i
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB2_46
# BB#38:
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB2_39:                               # %for.body45.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	regs_ever_live(%rbx), %eax
	testq	%rax, %rax
	je	.LBB2_42
# BB#40:                                # %land.lhs.true50.i
                                        #   in Loop: Header=BB2_39 Depth=1
	movq	regs_invalidated_by_call(%rip), %rax
	movq	$1, %rdx
	movq	%rbx, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	jne	.LBB2_42
# BB#41:                                # %if.then53.i
                                        #   in Loop: Header=BB2_39 Depth=1
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	bitmap_set_bit
.LBB2_42:                               # %for.inc55.i
                                        #   in Loop: Header=BB2_39 Depth=1
	incq	%rbx
	cmpq	$53, %rbx
	jne	.LBB2_39
# BB#43:                                # %if.end58.i
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB2_46
# BB#44:                                # %land.lhs.true60.i
	movq	cfun(%rip), %rax
	movzbl	424(%rax), %eax
	testq	$128, %rax
	je	.LBB2_50
# BB#45:                                # %if.end77.i
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	bitmap_set_bit
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	bitmap_set_bit
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	jne	.LBB2_50
.LBB2_46:                               # %land.lhs.true79.i
	movq	cfun(%rip), %rax
	movzbl	424(%rax), %eax
	testq	$128, %rax
	je	.LBB2_50
# BB#47:                                # %if.then85.i
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	$2, %rsi
	callq	gen_rtx_REG
	testq	%rax, %rax
	je	.LBB2_50
# BB#48:                                # %land.lhs.true91.i
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB2_50
# BB#49:                                # %if.then96.i
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	mark_reg
.LBB2_50:                               # %mark_regs_live_at_end.exit
	movabsq	$mark_reg, %rdi
	movq	%r12, %rsi
	callq	diddle_return_value
	testq	$4, %r15
	je	.LBB2_52
# BB#51:                                # %if.then15
	xorq	%rax, %rax
	movb	%al, regs_ever_live+52(%rip)
	movl	%eax, regs_ever_live+48(%rip)
	movq	$0, regs_ever_live+40(%rip)
	movq	$0, regs_ever_live+32(%rip)
	movq	$0, regs_ever_live+24(%rip)
	movq	$0, regs_ever_live+16(%rip)
	movq	$0, regs_ever_live+8(%rip)
	movq	$0, regs_ever_live(%rip)
.LBB2_52:                               # %if.end16
	xorq	%rdi, %rdi
	movq	$1, %rsi
	movq	%r15, %rdx
	callq	update_life_info
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB2_55
# BB#53:                                # %if.end16
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	je	.LBB2_55
# BB#54:                                # %if.then21
	callq	end_alias_analysis
.LBB2_55:                               # %if.end22
	testq	%r14, %r14
	je	.LBB2_57
# BB#56:                                # %if.then24
	movq	%r14, %rdi
	callq	dump_flow_info
.LBB2_57:                               # %if.end25
	callq	get_insns
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB2_61
# BB#58:
	movabsq	$.L.str8, %r14
	.align	16, 0x90
.LBB2_59:                               # %for.body.i56
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %rbx
	movl	(%rbx), %eax
	movq	24(%rbx), %r15
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$36, %rcx
	jne	.LBB2_60
# BB#62:                                # %land.lhs.true.i59
                                        #   in Loop: Header=BB2_59 Depth=1
	movl	32(%rbx), %edx
	movq	$28, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	cmpq	%rax, %rdx
	jne	.LBB2_60
# BB#63:                                # %land.lhs.true6.i
                                        #   in Loop: Header=BB2_59 Depth=1
	movzbl	(%r15), %eax
	cmpq	$33, %rax
	jne	.LBB2_60
# BB#64:                                # %land.lhs.true10.i
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	32(%r15), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	jne	.LBB2_59
# BB#65:                                # %if.then.i60
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	movq	%r15, %rax
	je	.LBB2_67
# BB#66:                                # %if.then24.i
                                        #   in Loop: Header=BB2_59 Depth=1
	movl	8(%rbx), %edx
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	fprintf
	movq	24(%rbx), %rax
.LBB2_67:                               # %if.end.i62
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	%rax, %rdi
	callq	delete_insn
	movq	%rbx, %rdi
	callq	delete_insn
	movq	24(%r15), %r15
	.align	16, 0x90
.LBB2_60:                               # %for.cond.backedge.i
                                        #   in Loop: Header=BB2_59 Depth=1
	testq	%r15, %r15
	jne	.LBB2_59
.LBB2_61:                               # %delete_dead_jumptables.exit
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp22:
	.size	life_analysis, .Ltmp22-life_analysis
	.cfi_endproc

	.globl	delete_noop_moves
	.align	16, 0x90
	.type	delete_noop_moves,@function
delete_noop_moves:                      # @delete_noop_moves
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp29:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp30:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp31:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp32:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp33:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp34:
	.cfi_def_cfa_offset 80
.Ltmp35:
	.cfi_offset %rbx, -48
.Ltmp36:
	.cfi_offset %r12, -40
.Ltmp37:
	.cfi_offset %r13, -32
.Ltmp38:
	.cfi_offset %r14, -24
.Ltmp39:
	.cfi_offset %r15, -16
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB3_8
# BB#1:
	xorq	%rdi, %rdi
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB3_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_3 Depth 2
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rdi,8), %r13
	movq	8(%r13), %rcx
	movq	(%r13), %r12
	movq	24(%rcx), %rcx
	cmpq	%rcx, %r12
	je	.LBB3_7
	.align	16, 0x90
.LBB3_3:                                # %for.body5
                                        #   Parent Loop BB3_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r12, %rbx
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	movq	24(%rbx), %r12
	cmpq	$105, %rax
	jne	.LBB3_5
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	%rbx, %rdi
	callq	noop_move_p
	testq	%r14, %rax
	je	.LBB3_5
# BB#9:                                 # %if.then
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	%r14, %r15
	movq	$7, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB3_12
# BB#10:                                # %land.lhs.true15
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	8(%r14), %rax
	cmpq	%rbx, %rax
	je	.LBB3_12
# BB#11:                                # %if.then21
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	%rbx, %rdi
	callq	next_real_insn
	movq	8(%r14), %rdi
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$6, %rsi
	xorq	%rdx, %rdx
	callq	find_reg_note
	movq	8(%r14), %rdx
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	56(%r14), %rcx
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$4, %rdi
	movq	$7, %rsi
	callq	gen_rtx_fmt_ue
	movq	%r14, %rcx
	movq	%rax, 56(%rcx)
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rcx, 8(%rax)
.LBB3_12:                               # %if.end
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	$37, %rax
	movw	%ax, (%rbx)
	movabsq	$4294967197, %rax       # imm = 0xFFFFFF9D
	movl	%eax, 40(%rbx)
	movq	$0, 32(%rbx)
	movq	%r15, %r14
.LBB3_5:                                # %for.cond2.backedge
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	8(%r13), %rax
	movq	24(%rax), %rax
	cmpq	%rax, %r12
	jne	.LBB3_3
# BB#6:                                 # %for.cond2.for.inc47_crit_edge
                                        #   in Loop: Header=BB3_2 Depth=1
	movl	n_basic_blocks(%rip), %eax
.LBB3_7:                                # %for.inc47
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	24(%rsp), %rdi          # 8-byte Reload
	incq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB3_2
.LBB3_8:                                # %for.end48
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp40:
	.size	delete_noop_moves, .Ltmp40-delete_noop_moves
	.cfi_endproc

	.globl	allocate_reg_life_data
	.align	16, 0x90
	.type	allocate_reg_life_data,@function
allocate_reg_life_data:                 # @allocate_reg_life_data
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp43:
	.cfi_def_cfa_offset 16
.Ltmp44:
	.cfi_offset %rbx, -16
	callq	max_reg_num
	movl	%eax, max_regno(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
	movq	%rax, %rdi
	callq	allocate_reg_info
	movslq	max_regno(%rip), %rax
	testq	%rax, %rax
	jle	.LBB4_3
# BB#1:                                 # %for.body.lr.ph
	movq	reg_n_info(%rip), %rax
	movq	$4, %rdx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB4_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rdx,8), %rcx
	movl	%ebx, 12(%rcx)
	movl	%ebx, 16(%rcx)
	movl	%ebx, 24(%rcx)
	movl	%ebx, 32(%rcx)
	movl	%ebx, 28(%rcx)
	movl	%r8d, 36(%rcx)
	movslq	max_regno(%rip), %rdi
	leaq	-3(%rdx), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rdx
	cmpq	%rdi, %rsi
	jl	.LBB4_2
.LBB4_3:                                # %for.end
	popq	%rbx
	retq
.Ltmp45:
	.size	allocate_reg_life_data, .Ltmp45-allocate_reg_life_data
	.cfi_endproc

	.globl	allocate_bb_life_data
	.align	16, 0x90
	.type	allocate_bb_life_data,@function
allocate_bb_life_data:                  # @allocate_bb_life_data
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp51:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp52:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp53:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp54:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp55:
	.cfi_def_cfa_offset 48
.Ltmp56:
	.cfi_offset %rbx, -40
.Ltmp57:
	.cfi_offset %r12, -32
.Ltmp58:
	.cfi_offset %r14, -24
.Ltmp59:
	.cfi_offset %r15, -16
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB5_15
# BB#1:
	movq	$4, %r12
	movabsq	$flow_obstack, %r14
	.align	16, 0x90
.LBB5_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	flow_obstack+32(%rip), %rdx
	movq	flow_obstack+24(%rip), %rax
	movq	(%rcx,%r12,8), %r15
	subq	%rax, %rdx
	cmpq	$23, %rdx
	jg	.LBB5_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	$24, %rsi
	movq	%r14, %rdi
	callq	_obstack_newchunk
	movq	flow_obstack+24(%rip), %rax
.LBB5_4:                                # %if.end
                                        #   in Loop: Header=BB5_2 Depth=1
	addq	$24, %rax
	movq	%rax, flow_obstack+24(%rip)
	movq	flow_obstack+16(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB5_6
# BB#5:                                 # %if.then10
                                        #   in Loop: Header=BB5_2 Depth=1
	movzbl	flow_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, flow_obstack+80(%rip)
.LBB5_6:                                # %if.end11
                                        #   in Loop: Header=BB5_2 Depth=1
	movslq	flow_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, flow_obstack+24(%rip)
	movq	flow_obstack+32(%rip), %rbx
	movq	flow_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rbx, %rax
	subq	%rdx, %rax
	cmpq	%rax, %rsi
	jle	.LBB5_8
# BB#7:                                 # %if.then31
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	%rbx, flow_obstack+24(%rip)
	movq	%rbx, %rcx
.LBB5_8:                                # %if.end34
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	%rcx, flow_obstack+16(%rip)
	callq	bitmap_initialize
	movq	%rax, 64(%r15)
	movq	flow_obstack+32(%rip), %rcx
	movq	flow_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$23, %rcx
	jg	.LBB5_10
# BB#9:                                 # %if.then52
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	$24, %rsi
	movq	%r14, %rdi
	callq	_obstack_newchunk
	movq	flow_obstack+24(%rip), %rax
.LBB5_10:                               # %if.end53
                                        #   in Loop: Header=BB5_2 Depth=1
	addq	$24, %rax
	movq	%rax, flow_obstack+24(%rip)
	movq	flow_obstack+16(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB5_12
# BB#11:                                # %if.then66
                                        #   in Loop: Header=BB5_2 Depth=1
	movzbl	flow_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, flow_obstack+80(%rip)
.LBB5_12:                               # %if.end71
                                        #   in Loop: Header=BB5_2 Depth=1
	movslq	flow_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, flow_obstack+24(%rip)
	movq	flow_obstack+32(%rip), %rbx
	movq	flow_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rbx, %rax
	subq	%rdx, %rax
	cmpq	%rax, %rsi
	jle	.LBB5_14
# BB#13:                                # %if.then96
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	%rbx, flow_obstack+24(%rip)
	movq	%rbx, %rcx
.LBB5_14:                               # %if.end99
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	%rcx, flow_obstack+16(%rip)
	callq	bitmap_initialize
	movq	%rax, 72(%r15)
	movslq	n_basic_blocks(%rip), %rax
	leaq	-3(%r12), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r12
	cmpq	%rax, %rdx
	jl	.LBB5_2
.LBB5_15:                               # %for.end
	movq	flow_obstack+32(%rip), %rcx
	movq	flow_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$23, %rcx
	jg	.LBB5_17
# BB#16:                                # %if.then118
	movabsq	$flow_obstack, %rdi
	movq	$24, %rsi
	callq	_obstack_newchunk
	movq	flow_obstack+24(%rip), %rax
.LBB5_17:                               # %if.end119
	addq	$24, %rax
	movq	%rax, flow_obstack+24(%rip)
	movq	flow_obstack+16(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB5_19
# BB#18:                                # %if.then132
	movzbl	flow_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, flow_obstack+80(%rip)
.LBB5_19:                               # %if.end137
	movslq	flow_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, flow_obstack+24(%rip)
	movq	flow_obstack+32(%rip), %rbx
	movq	flow_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rbx, %rax
	subq	%rdx, %rax
	cmpq	%rax, %rsi
	jle	.LBB5_21
# BB#20:                                # %if.then162
	movq	%rbx, flow_obstack+24(%rip)
	movq	%rbx, %rcx
.LBB5_21:                               # %if.end165
	movq	%rcx, flow_obstack+16(%rip)
	callq	bitmap_initialize
	movq	flow_obstack+32(%rip), %rdx
	movq	flow_obstack+24(%rip), %rcx
	movq	%rax, entry_exit_blocks+72(%rip)
	subq	%rcx, %rdx
	cmpq	$23, %rdx
	jg	.LBB5_23
# BB#22:                                # %if.then184
	movabsq	$flow_obstack, %rdi
	movq	$24, %rsi
	callq	_obstack_newchunk
	movq	flow_obstack+24(%rip), %rcx
.LBB5_23:                               # %if.end185
	addq	$24, %rcx
	movq	%rcx, flow_obstack+24(%rip)
	movq	flow_obstack+16(%rip), %rdi
	cmpq	%rdi, %rcx
	jne	.LBB5_25
# BB#24:                                # %if.then198
	movzbl	flow_obstack+80(%rip), %eax
	orq	$2, %rax
	movb	%al, flow_obstack+80(%rip)
.LBB5_25:                               # %if.end203
	movslq	flow_obstack+48(%rip), %rax
	addq	%rax, %rcx
	notq	%rax
	andq	%rcx, %rax
	movq	%rax, flow_obstack+24(%rip)
	movq	flow_obstack+32(%rip), %rbx
	movq	flow_obstack+8(%rip), %rdx
	movq	%rax, %rsi
	subq	%rdx, %rsi
	movq	%rbx, %rcx
	subq	%rdx, %rcx
	cmpq	%rcx, %rsi
	jle	.LBB5_27
# BB#26:                                # %if.then228
	movq	%rbx, flow_obstack+24(%rip)
	movq	%rbx, %rax
.LBB5_27:                               # %if.end231
	movq	%rax, flow_obstack+16(%rip)
	callq	bitmap_initialize
	movq	flow_obstack+32(%rip), %rdx
	movq	flow_obstack+24(%rip), %rcx
	movq	%rax, entry_exit_blocks+176(%rip)
	subq	%rcx, %rdx
	cmpq	$23, %rdx
	jg	.LBB5_29
# BB#28:                                # %if.then250
	movabsq	$flow_obstack, %rdi
	movq	$24, %rsi
	callq	_obstack_newchunk
	movq	flow_obstack+24(%rip), %rcx
.LBB5_29:                               # %if.end251
	addq	$24, %rcx
	movq	%rcx, flow_obstack+24(%rip)
	movq	flow_obstack+16(%rip), %rdi
	cmpq	%rdi, %rcx
	jne	.LBB5_31
# BB#30:                                # %if.then264
	movzbl	flow_obstack+80(%rip), %eax
	orq	$2, %rax
	movb	%al, flow_obstack+80(%rip)
.LBB5_31:                               # %if.end269
	movslq	flow_obstack+48(%rip), %rax
	addq	%rax, %rcx
	notq	%rax
	andq	%rcx, %rax
	movq	%rax, flow_obstack+24(%rip)
	movq	flow_obstack+32(%rip), %rbx
	movq	flow_obstack+8(%rip), %rdx
	movq	%rax, %rsi
	subq	%rdx, %rsi
	movq	%rbx, %rcx
	subq	%rdx, %rcx
	cmpq	%rcx, %rsi
	jle	.LBB5_33
# BB#32:                                # %if.then294
	movq	%rbx, flow_obstack+24(%rip)
	movq	%rbx, %rax
.LBB5_33:                               # %if.end297
	movq	%rax, flow_obstack+16(%rip)
	callq	bitmap_initialize
	movq	%rax, regs_live_at_setjmp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp60:
	.size	allocate_bb_life_data, .Ltmp60-allocate_bb_life_data
	.cfi_endproc

	.globl	update_life_info
	.align	16, 0x90
	.type	update_life_info,@function
update_life_info:                       # @update_life_info
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
	subq	$208, %rsp
.Ltmp72:
	.cfi_def_cfa_offset 256
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
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, %rbx
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	movq	%rdi, %r12
	leaq	112(%rsp), %rdi
	callq	bitmap_initialize
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rbx
	movq	$6, %rdi
	je	.LBB6_5
# BB#1:                                 # %lor.rhs
	testq	%r12, %r12
	je	.LBB6_2
# BB#3:                                 # %lor.rhs
	movabsq	$1, %rdi
	jmp	.LBB6_4
.LBB6_2:
	movabsq	$0, %rdi
.LBB6_4:                                # %lor.rhs
	addq	$5, %rdi
.LBB6_5:                                # %lor.end
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r13, %rax
	movabsq	$0, %rbx
	movabsq	$1, %r15
	movq	%rbx, %r14
	jne	.LBB6_7
# BB#6:                                 # %lor.end
	movq	%r15, %r14
.LBB6_7:                                # %lor.end
	callq	timevar_push
	testq	%r12, %r12
	je	.LBB6_9
# BB#8:                                 # %lor.end
	movq	%r15, %rbx
.LBB6_9:                                # %lor.end
	testq	$32, 16(%rsp)           # 8-byte Folded Reload
	je	.LBB6_11
# BB#10:                                # %lor.end
	orq	%rbx, %r14
	jne	.LBB6_199
.LBB6_11:                               # %if.end
	movq	%r12, 96(%rsp)          # 8-byte Spill
	testq	$2, 16(%rsp)            # 8-byte Folded Reload
	movq	%r13, %r15
	je	.LBB6_31
# BB#12:                                # %if.then9
	movq	96(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	je	.LBB6_13
# BB#18:                                # %do.body.i
	movl	4(%rax), %eax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB6_31
# BB#19:
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB6_20:                               # %for.body11.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_22 Depth 2
                                        #       Child Loop BB6_24 Depth 3
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	16(%rax,%rdx,8), %r12
	testq	%r12, %r12
	je	.LBB6_30
# BB#21:                                # %for.body20.preheader.i
                                        #   in Loop: Header=BB6_20 Depth=1
	movq	$6, %rcx
	movq	88(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB6_22:                               # %for.body20.i
                                        #   Parent Loop BB6_20 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB6_24 Depth 3
	movq	$1, %r14
	movq	%rbx, %rcx
	shlq	%cl, %r14
	testq	%r12, %r14
	je	.LBB6_29
# BB#23:                                # %if.then23.i
                                        #   in Loop: Header=BB6_22 Depth=2
	movq	104(%rsp), %rax         # 8-byte Reload
	leaq	(%rbx,%rax), %rax
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %r13
	movq	8(%r13), %rax
	movq	(%r13), %r15
	jmp	.LBB6_24
	.align	16, 0x90
.LBB6_27:                               # %for.inc47.i
                                        #   in Loop: Header=BB6_24 Depth=3
	movq	24(%r15), %r15
.LBB6_24:                               # %if.then23.i
                                        #   Parent Loop BB6_20 Depth=1
                                        #     Parent Loop BB6_22 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	24(%rax), %rcx
	cmpq	%rcx, %r15
	je	.LBB6_28
# BB#25:                                # %for.body34.i
                                        #   in Loop: Header=BB6_24 Depth=3
	movzbl	(%r15), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB6_27
# BB#26:                                # %if.then42.i
                                        #   in Loop: Header=BB6_24 Depth=3
	leaq	48(%r15), %rdi
	callq	free_INSN_LIST_list
	movq	8(%r13), %rax
	jmp	.LBB6_27
	.align	16, 0x90
.LBB6_28:                               # %for.end51.i
                                        #   in Loop: Header=BB6_22 Depth=2
	notq	%r14
	andq	%r14, %r12
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	je	.LBB6_30
.LBB6_29:                               # %for.inc57.i
                                        #   in Loop: Header=BB6_22 Depth=2
	incq	%rbx
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	$64, %rax
	jb	.LBB6_22
.LBB6_30:                               # %for.inc60.i
                                        #   in Loop: Header=BB6_20 Depth=1
	movq	88(%rsp), %rdx          # 8-byte Reload
	incq	%rdx
	movq	%rdx, %rax
	andq	%r15, %rax
	movq	80(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB6_20
	jmp	.LBB6_31
.LBB6_13:                               # %if.then.i
	callq	get_insns
	movq	%rax, %rbx
	jmp	.LBB6_14
	.align	16, 0x90
.LBB6_17:                               # %for.inc.i
                                        #   in Loop: Header=BB6_14 Depth=1
	movq	24(%rbx), %rbx
.LBB6_14:                               # %if.then.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB6_31
# BB#15:                                # %for.body.i
                                        #   in Loop: Header=BB6_14 Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB6_17
# BB#16:                                # %if.then3.i
                                        #   in Loop: Header=BB6_14 Depth=1
	leaq	48(%rbx), %rdi
	callq	free_INSN_LIST_list
	jmp	.LBB6_17
.LBB6_31:                               # %if.end10
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r15, %rax
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	40(%rsp), %r12          # 8-byte Reload
	je	.LBB6_139
# BB#32:                                # %for.cond.preheader
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, %rcx
	andq	$48, %rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	%rax, %rcx
	andq	$40, %rcx
	movq	%rax, %r8
	andq	$24, %r8
	movq	%r8, 24(%rsp)           # 8-byte Spill
	andq	%r15, %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	%rax, 64(%rsp)          # 8-byte Spill
	jmp	.LBB6_33
	.align	16, 0x90
.LBB6_153:                              # %if.end28
                                        #   in Loop: Header=BB6_33 Depth=1
	movq	$1, %rdi
	callq	cleanup_cfg
.LBB6_33:                               # %for.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_34 Depth 2
                                        #     Child Loop BB6_40 Depth 2
                                        #     Child Loop BB6_42 Depth 2
                                        #       Child Loop BB6_44 Depth 3
                                        #     Child Loop BB6_53 Depth 2
                                        #     Child Loop BB6_60 Depth 2
                                        #       Child Loop BB6_63 Depth 3
                                        #       Child Loop BB6_122 Depth 3
                                        #     Child Loop BB6_129 Depth 2
                                        #     Child Loop BB6_87 Depth 2
                                        #       Child Loop BB6_89 Depth 3
                                        #     Child Loop BB6_151 Depth 2
	leaq	184(%rsp), %rdi
	callq	bitmap_initialize
	movq	%rax, %r14
	leaq	136(%rsp), %rdi
	callq	bitmap_initialize
	movq	%rax, %r12
	leaq	160(%rsp), %rdi
	callq	bitmap_initialize
	movq	%rax, %r13
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB6_34:                               # %for.body.i366
                                        #   Parent Loop BB6_33 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	call_used_regs(%rbx), %eax
	testq	%rax, %rax
	je	.LBB6_36
# BB#35:                                # %if.then.i367
                                        #   in Loop: Header=BB6_34 Depth=2
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	bitmap_set_bit
.LBB6_36:                               # %for.inc.i368
                                        #   in Loop: Header=BB6_34 Depth=2
	incq	%rbx
	cmpq	$53, %rbx
	jne	.LBB6_34
# BB#37:                                # %for.end.i
                                        #   in Loop: Header=BB6_33 Depth=1
	movl	n_basic_blocks(%rip), %edi
	addq	$2, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rdx
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	96(%rsp), %r11          # 8-byte Reload
	testq	%r11, %r11
	leaq	16(%rax,%rdx,8), %rsi
	jne	.LBB6_38
# BB#48:                                # %for.cond65.preheader.i
                                        #   in Loop: Header=BB6_33 Depth=1
	movq	%r13, 104(%rsp)         # 8-byte Spill
	testq	%rdx, %rdx
	movq	%rsi, %r13
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	jle	.LBB6_55
# BB#49:                                # %for.body68.lr.ph.i
                                        #   in Loop: Header=BB6_33 Depth=1
	movq	%r11, %rbx
	leaq	-1(%rdx), %rax
	andq	%r15, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	cmpq	$1, %rdx
	jg	.LBB6_50
# BB#51:                                # %for.body68.lr.ph.i
                                        #   in Loop: Header=BB6_33 Depth=1
	xorq	%rax, %rax
	jmp	.LBB6_52
	.align	16, 0x90
.LBB6_38:                               # %for.cond13.preheader.i
                                        #   in Loop: Header=BB6_33 Depth=1
	movq	%r13, 104(%rsp)         # 8-byte Spill
	testq	%rdx, %rdx
	jle	.LBB6_41
# BB#39:                                # %for.body16.i.preheader
                                        #   in Loop: Header=BB6_33 Depth=1
	decq	%rdx
	.align	16, 0x90
.LBB6_40:                               # %for.body16.i
                                        #   Parent Loop BB6_33 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rdx,8), %rax
	movq	$0, 80(%rax)
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rdx
	testq	%rax, %rax
	jg	.LBB6_40
.LBB6_41:                               # %do.body.i374
                                        #   in Loop: Header=BB6_33 Depth=1
	movl	4(%r11), %r8d
	xorq	%r9, %r9
	testq	%r8, %r8
	movq	%rsi, %r13
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	je	.LBB6_55
	.align	16, 0x90
.LBB6_42:                               # %for.body28.i
                                        #   Parent Loop BB6_33 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB6_44 Depth 3
	movq	16(%r11,%r9,8), %rdi
	testq	%rdi, %rdi
	je	.LBB6_47
# BB#43:                                # %for.body38.preheader.i
                                        #   in Loop: Header=BB6_42 Depth=2
	movq	$6, %rcx
	movq	%r9, %r10
	shlq	%cl, %r10
	movq	$1, %rbx
	.align	16, 0x90
.LBB6_44:                               # %for.body38.i
                                        #   Parent Loop BB6_33 Depth=1
                                        #     Parent Loop BB6_42 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1(%rbx), %rax
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	%rdi, %rdx
	je	.LBB6_46
# BB#45:                                # %if.then42.i381
                                        #   in Loop: Header=BB6_44 Depth=3
	addq	%r10, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	basic_block_info(%rip), %rsi
	movq	$32, %rcx
	sarq	%cl, %rax
	notq	%rdx
	movq	32(%rsi,%rax,8), %rax
	andq	%rdx, %rdi
	movq	%rax, -8(%r13)
	leaq	-8(%r13), %r13
	movq	%rax, 80(%rax)
	je	.LBB6_47
.LBB6_46:                               # %for.inc58.i
                                        #   in Loop: Header=BB6_44 Depth=3
	movq	%rbx, %rax
	andq	%r15, %rax
	incq	%rbx
	cmpq	$64, %rax
	jb	.LBB6_44
.LBB6_47:                               # %for.inc62.i
                                        #   in Loop: Header=BB6_42 Depth=2
	incq	%r9
	movq	%r9, %rax
	andq	%r15, %rax
	cmpq	%r8, %rax
	jne	.LBB6_42
	jmp	.LBB6_55
.LBB6_50:                               #   in Loop: Header=BB6_33 Depth=1
	negq	%rax
.LBB6_52:                               # %for.body68.lr.ph.i
                                        #   in Loop: Header=BB6_33 Depth=1
	leaq	8(%rax,%rdx,8), %r13
	movq	88(%rsp), %rax          # 8-byte Reload
	leaq	8(%rax,%rdx,8), %rax
	movq	$4, %rcx
	.align	16, 0x90
.LBB6_53:                               # %for.body68.i
                                        #   Parent Loop BB6_33 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	basic_block_info(%rip), %rsi
	movq	(%rsi,%rcx,8), %rsi
	movq	%rsi, (%rax)
	movq	%rsi, 80(%rsi)
	leaq	-3(%rcx), %rsi
	andq	%r15, %rsi
	movq	%rdx, %rdi
	andq	%r15, %rdi
	incq	%rcx
	addq	$-8, %rax
	cmpq	%rdi, %rsi
	jne	.LBB6_53
# BB#54:                                # %for.cond65.if.end80.loopexit_crit_edge.i
                                        #   in Loop: Header=BB6_33 Depth=1
	movq	88(%rsp), %rax          # 8-byte Reload
	addq	%rax, %r13
	movq	%rbx, %r11
.LBB6_55:                               # %if.end80.i
                                        #   in Loop: Header=BB6_33 Depth=1
	testq	%r11, %r11
	movq	$0, entry_exit_blocks+192(%rip)
	movq	$0, entry_exit_blocks+80(%rip)
	je	.LBB6_57
# BB#56:                                # %if.then82.i
                                        #   in Loop: Header=BB6_33 Depth=1
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	sbitmap_zero
.LBB6_57:                               # %while.cond.preheader.i
                                        #   in Loop: Header=BB6_33 Depth=1
	movq	88(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r13
	je	.LBB6_79
# BB#58:                                # %while.body.lr.ph.lr.ph.i
                                        #   in Loop: Header=BB6_33 Depth=1
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	%rax, 80(%rsp)          # 8-byte Spill
	jmp	.LBB6_60
.LBB6_120:                              # %if.end343.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	64(%r15), %rdi
	movq	%r12, %rsi
	callq	bitmap_copy
.LBB6_121:                              # %if.end345.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	32(%r15), %rax
	testq	%rax, %rax
	movq	72(%rsp), %rsi          # 8-byte Reload
	je	.LBB6_59
	.align	16, 0x90
.LBB6_122:                              # %for.body348.i
                                        #   Parent Loop BB6_33 Depth=1
                                        #     Parent Loop BB6_60 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	16(%rax), %rcx
	cmpq	$0, 80(%rcx)
	jne	.LBB6_126
# BB#123:                               # %if.then353.i
                                        #   in Loop: Header=BB6_122 Depth=3
	movq	80(%rsp), %rdi          # 8-byte Reload
	leaq	8(%rdi), %rdx
	cmpq	%rsi, %rdx
	movq	%rcx, (%rdi)
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, 80(%rsp)          # 8-byte Spill
	je	.LBB6_125
# BB#124:                               # %if.then353.i
                                        #   in Loop: Header=BB6_122 Depth=3
	movq	%rdx, 80(%rsp)          # 8-byte Spill
.LBB6_125:                              # %if.then353.i
                                        #   in Loop: Header=BB6_122 Depth=3
	movq	%rcx, 80(%rcx)
.LBB6_126:                              # %for.inc361.i
                                        #   in Loop: Header=BB6_122 Depth=3
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB6_122
	jmp	.LBB6_59
	.align	16, 0x90
.LBB6_60:                               # %while.body.i
                                        #   Parent Loop BB6_33 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB6_63 Depth 3
                                        #       Child Loop BB6_122 Depth 3
	movq	(%r13), %r15
	leaq	8(%r13), %rax
	movq	72(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	movq	88(%rsp), %r13          # 8-byte Reload
	je	.LBB6_62
# BB#61:                                # %while.body.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	%rax, %r13
.LBB6_62:                               # %while.body.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	$0, 80(%r15)
	movq	%r12, %rdi
	callq	bitmap_clear
	movq	40(%r15), %rbx
	jmp	.LBB6_63
	.align	16, 0x90
.LBB6_67:                               # %for.inc126.i
                                        #   in Loop: Header=BB6_63 Depth=3
	callq	bitmap_operation
	movq	8(%rbx), %rbx
.LBB6_63:                               # %while.body.i
                                        #   Parent Loop BB6_33 Depth=1
                                        #     Parent Loop BB6_60 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB6_68
# BB#64:                                # %for.body102.i
                                        #   in Loop: Header=BB6_63 Depth=3
	movq	24(%rbx), %rax
	movzbl	48(%rbx), %ecx
	movq	64(%rax), %rax
	testq	$8, %rcx
	jne	.LBB6_65
# BB#66:                                # %if.else110.i
                                        #   in Loop: Header=BB6_63 Depth=3
	movq	$2, %rcx
	movq	%r12, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	jmp	.LBB6_67
	.align	16, 0x90
.LBB6_65:                               # %if.then107.i
                                        #   in Loop: Header=BB6_63 Depth=3
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	104(%rsp), %rdx         # 8-byte Reload
	callq	bitmap_operation
	movq	$2, %rcx
	movq	%r12, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	jmp	.LBB6_67
	.align	16, 0x90
.LBB6_68:                               # %if.end136.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	$7, %rsi
	movq	%r12, %rdi
	callq	bitmap_set_bit
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	jne	.LBB6_75
# BB#69:                                # %if.then138.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	$20, %rsi
	movq	%r12, %rdi
	callq	bitmap_set_bit
	movzbl	fixed_regs+16(%rip), %eax
	testq	%rax, %rax
	je	.LBB6_71
# BB#70:                                # %if.then140.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	$16, %rsi
	movq	%r12, %rdi
	callq	bitmap_set_bit
.LBB6_71:                               # %if.end141.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movl	flag_pic(%rip), %eax
	testq	%rax, %rax
	je	.LBB6_75
# BB#72:                                # %if.end141.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movl	target_flags(%rip), %eax
	andq	$33554432, %rax         # imm = 0x2000000
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB6_75
# BB#73:                                # %if.end141.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movzbl	fixed_regs+3(%rip), %eax
	testq	$255, %rax
	je	.LBB6_75
# BB#74:                                # %lor.end164.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	$3, %rsi
	movq	%r12, %rdi
	callq	bitmap_set_bit
.LBB6_75:                               # %if.end167.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movl	in_ssa_form(%rip), %eax
	testq	%rax, %rax
	je	.LBB6_77
# BB#76:                                # %if.then169.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	%r15, %rdi
	movabsq	$set_phi_alternative_reg, %rsi
	movq	%r12, %rdx
	callq	for_each_successor_phi
.LBB6_77:                               # %if.end171.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movabsq	$entry_exit_blocks, %rax
	cmpq	%rax, %r15
	jne	.LBB6_97
# BB#78:                                # %if.then174.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	entry_exit_blocks+72(%rip), %rdi
	movq	%r12, %rsi
	callq	bitmap_copy
	jmp	.LBB6_59
	.align	16, 0x90
.LBB6_97:                               # %if.end175.i
                                        #   in Loop: Header=BB6_60 Depth=2
	cmpq	$0, 48(%r15)
	jne	.LBB6_111
# BB#98:                                # %if.then178.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	flow_obstack+32(%rip), %rcx
	movq	flow_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$23, %rcx
	jg	.LBB6_100
# BB#99:                                # %if.then185.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	$24, %rsi
	movabsq	$flow_obstack, %rdi
	callq	_obstack_newchunk
	movq	flow_obstack+24(%rip), %rax
.LBB6_100:                              # %if.end186.i
                                        #   in Loop: Header=BB6_60 Depth=2
	addq	$24, %rax
	movq	%rax, flow_obstack+24(%rip)
	movq	flow_obstack+16(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB6_102
# BB#101:                               # %if.then196.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movzbl	flow_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, flow_obstack+80(%rip)
.LBB6_102:                              # %if.end197.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movslq	flow_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, flow_obstack+24(%rip)
	movq	flow_obstack+32(%rip), %rbx
	movq	flow_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rbx, %rax
	subq	%rdx, %rax
	cmpq	%rax, %rsi
	jle	.LBB6_104
# BB#103:                               # %if.then220.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	%rbx, flow_obstack+24(%rip)
	movq	%rbx, %rcx
.LBB6_104:                              # %if.end223.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	%rcx, flow_obstack+16(%rip)
	callq	bitmap_initialize
	movq	%rax, 48(%r15)
	movq	flow_obstack+32(%rip), %rcx
	movq	flow_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$23, %rcx
	jg	.LBB6_106
# BB#105:                               # %if.then243.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	$24, %rsi
	movabsq	$flow_obstack, %rdi
	callq	_obstack_newchunk
	movq	flow_obstack+24(%rip), %rax
.LBB6_106:                              # %if.end244.i
                                        #   in Loop: Header=BB6_60 Depth=2
	addq	$24, %rax
	movq	%rax, flow_obstack+24(%rip)
	movq	flow_obstack+16(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB6_108
# BB#107:                               # %if.then257.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movzbl	flow_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, flow_obstack+80(%rip)
.LBB6_108:                              # %if.end262.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movslq	flow_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, flow_obstack+24(%rip)
	movq	flow_obstack+32(%rip), %rbx
	movq	flow_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rbx, %rax
	subq	%rdx, %rax
	cmpq	%rax, %rsi
	jle	.LBB6_110
# BB#109:                               # %if.then287.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	%rbx, flow_obstack+24(%rip)
	movq	%rbx, %rcx
.LBB6_110:                              # %if.end290.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	%rcx, flow_obstack+16(%rip)
	callq	bitmap_initialize
	movq	%rax, 56(%r15)
	movq	$1, %rax
	jmp	.LBB6_115
	.align	16, 0x90
.LBB6_111:                              # %if.else295.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	%r14, %rdi
	callq	bitmap_clear
	movq	72(%r15), %rsi
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%r12, %rdx
	callq	bitmap_operation
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB6_115
# BB#112:                               # %if.end302.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	%r14, %rdi
	callq	bitmap_clear
	movq	56(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	bitmap_operation
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB6_115
# BB#113:                               # %if.then304.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	%r14, %rdi
	callq	bitmap_clear
	movq	72(%r15), %rsi
	movq	$3, %rcx
	movq	%r14, %rdi
	movq	%r12, %rdx
	callq	bitmap_operation
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_59
# BB#114:                               # %if.end309.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	48(%r15), %rdx
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%r14, %rsi
	callq	bitmap_operation
	.align	16, 0x90
.LBB6_115:                              # %if.end313.i
                                        #   in Loop: Header=BB6_60 Depth=2
	cmpq	$0, 96(%rsp)            # 8-byte Folded Reload
	je	.LBB6_117
# BB#116:                               # %if.then315.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movl	88(%r15), %ecx
	movq	$1, %rdx
	shlq	%cl, %rdx
	shrq	$6, %rcx
	movq	96(%rsp), %rsi          # 8-byte Reload
	orq	%rdx, 16(%rsi,%rcx,8)
.LBB6_117:                              # %if.end322.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	72(%r15), %rdx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	jne	.LBB6_119
# BB#118:                               # %if.then324.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	bitmap_operation
	movq	72(%r15), %rdi
	movq	%r12, %rsi
	callq	bitmap_copy
	movq	64(%r15), %rdi
	movq	$2, %rcx
	movq	%rdi, %rsi
	movq	%r14, %rdx
	callq	bitmap_operation
	testq	%rbx, %rax
	je	.LBB6_59
	jmp	.LBB6_121
	.align	16, 0x90
.LBB6_119:                              # %if.else334.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	%rdx, %rdi
	movq	%r12, %rsi
	callq	bitmap_copy
	movq	48(%r15), %rdx
	movq	56(%r15), %rcx
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	48(%rsp), %r8           # 8-byte Reload
	callq	propagate_block
	movq	64(%r15), %rdi
	movq	%r12, %rsi
	callq	bitmap_equal_p
	testq	%rbx, %rax
	je	.LBB6_120
.LBB6_59:                               # %while.cond.loopexit.i
                                        #   in Loop: Header=BB6_60 Depth=2
	movq	80(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r13
	jne	.LBB6_60
.LBB6_79:                               # %do.body363.i
                                        #   in Loop: Header=BB6_33 Depth=1
	testq	%r14, %r14
	je	.LBB6_81
# BB#80:                                # %if.then365.i
                                        #   in Loop: Header=BB6_33 Depth=1
	movq	%r14, %rdi
	callq	bitmap_clear
.LBB6_81:                               # %do.body368.i
                                        #   in Loop: Header=BB6_33 Depth=1
	testq	%r12, %r12
	movq	104(%rsp), %rbx         # 8-byte Reload
	je	.LBB6_83
# BB#82:                                # %if.then370.i
                                        #   in Loop: Header=BB6_33 Depth=1
	movq	%r12, %rdi
	callq	bitmap_clear
.LBB6_83:                               # %do.body373.i
                                        #   in Loop: Header=BB6_33 Depth=1
	testq	%rbx, %rbx
	je	.LBB6_85
# BB#84:                                # %if.then375.i
                                        #   in Loop: Header=BB6_33 Depth=1
	movq	%rbx, %rdi
	callq	bitmap_clear
.LBB6_85:                               # %do.end377.i
                                        #   in Loop: Header=BB6_33 Depth=1
	movq	96(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	je	.LBB6_127
# BB#86:                                # %do.body380.i
                                        #   in Loop: Header=BB6_33 Depth=1
	movl	4(%rax), %eax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	xorq	%r15, %r15
	testq	%rax, %rax
	je	.LBB6_133
	.align	16, 0x90
.LBB6_87:                               # %for.body395.i
                                        #   Parent Loop BB6_33 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB6_89 Depth 3
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	16(%rax,%r15,8), %r13
	testq	%r13, %r13
	je	.LBB6_96
# BB#88:                                # %for.body406.preheader.i
                                        #   in Loop: Header=BB6_87 Depth=2
	movq	$6, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	$1, %rbx
	.align	16, 0x90
.LBB6_89:                               # %for.body406.i
                                        #   Parent Loop BB6_33 Depth=1
                                        #     Parent Loop BB6_87 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1(%rbx), %rax
	movq	$1, %r14
	movq	%rax, %rcx
	shlq	%cl, %r14
	testq	%r13, %r14
	je	.LBB6_95
# BB#90:                                # %if.then414.i
                                        #   in Loop: Header=BB6_89 Depth=3
	movq	104(%rsp), %rcx         # 8-byte Reload
	addq	%rcx, %rax
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %r12
	movq	48(%r12), %rdi
	notq	%r14
	testq	%rdi, %rdi
	je	.LBB6_92
# BB#91:                                # %if.then428.i
                                        #   in Loop: Header=BB6_89 Depth=3
	callq	bitmap_clear
	movq	$0, 48(%r12)
.LBB6_92:                               # %do.body433.i
                                        #   in Loop: Header=BB6_89 Depth=3
	movq	56(%r12), %rdi
	andq	%r14, %r13
	testq	%rdi, %rdi
	je	.LBB6_94
# BB#93:                                # %if.then436.i
                                        #   in Loop: Header=BB6_89 Depth=3
	callq	bitmap_clear
	movq	$0, 56(%r12)
.LBB6_94:                               # %do.end440.i
                                        #   in Loop: Header=BB6_89 Depth=3
	testq	%r13, %r13
	je	.LBB6_96
.LBB6_95:                               # %for.inc446.i
                                        #   in Loop: Header=BB6_89 Depth=3
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	incq	%rbx
	cmpq	$64, %rax
	jb	.LBB6_89
.LBB6_96:                               # %for.inc450.i
                                        #   in Loop: Header=BB6_87 Depth=2
	incq	%r15
	movq	%r15, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	80(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB6_87
	jmp	.LBB6_133
	.align	16, 0x90
.LBB6_127:                              # %if.else454.i
                                        #   in Loop: Header=BB6_33 Depth=1
	movslq	n_basic_blocks(%rip), %rbx
	testq	%rbx, %rbx
	jle	.LBB6_133
# BB#128:                               # %for.body459.lr.ph.i
                                        #   in Loop: Header=BB6_33 Depth=1
	decq	%rbx
	.align	16, 0x90
.LBB6_129:                              # %for.body459.i
                                        #   Parent Loop BB6_33 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rbx,8), %r14
	movq	48(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB6_131
# BB#130:                               # %if.then469.i
                                        #   in Loop: Header=BB6_129 Depth=2
	callq	bitmap_clear
	movq	$0, 48(%r14)
.LBB6_131:                              # %do.body474.i
                                        #   in Loop: Header=BB6_129 Depth=2
	movq	56(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB6_132
# BB#200:                               # %if.then477.i
                                        #   in Loop: Header=BB6_129 Depth=2
	callq	bitmap_clear
	movq	$0, 56(%r14)
.LBB6_132:                              # %for.cond456.backedge.i
                                        #   in Loop: Header=BB6_129 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB6_129
.LBB6_133:                              # %calculate_global_regs_live.exit
                                        #   in Loop: Header=BB6_33 Depth=1
	movq	88(%rsp), %rdi          # 8-byte Reload
	callq	free
	cmpq	$40, 32(%rsp)           # 8-byte Folded Reload
	jne	.LBB6_134
# BB#135:                               # %if.end18
                                        #   in Loop: Header=BB6_33 Depth=1
	movslq	n_basic_blocks(%rip), %rbx
	testq	%rbx, %rbx
	movq	40(%rsp), %r12          # 8-byte Reload
	jle	.LBB6_136
# BB#150:                               # %for.body.lr.ph
                                        #   in Loop: Header=BB6_33 Depth=1
	decq	%rbx
	xorq	%rax, %rax
	movq	24(%rsp), %r13          # 8-byte Reload
	.align	16, 0x90
.LBB6_151:                              # %for.body
                                        #   Parent Loop BB6_33 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rbx,8), %r14
	movq	72(%r14), %rsi
	movq	%rax, %r15
	movq	%r12, %rdi
	callq	bitmap_copy
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%r13, %r8
	callq	propagate_block
	orq	%r15, %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%rbx
	testq	%rdx, %rdx
	jg	.LBB6_151
# BB#152:                               # %for.end
                                        #   in Loop: Header=BB6_33 Depth=1
	movabsq	$4294967271, %rcx       # imm = 0xFFFFFFE7
	andq	%rcx, 64(%rsp)          # 8-byte Folded Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	jne	.LBB6_153
	jmp	.LBB6_137
.LBB6_134:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	40(%rsp), %r12          # 8-byte Reload
	jmp	.LBB6_137
.LBB6_136:                              # %for.end.thread
	movabsq	$4294967271, %rax       # imm = 0xFFFFFFE7
	andq	%rax, 64(%rsp)          # 8-byte Folded Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
.LBB6_137:                              # %for.end30
	movq	56(%rsp), %rax          # 8-byte Reload
	andq	%r15, %rax
	cmpq	$2, %rax
	jne	.LBB6_139
# BB#138:                               # %if.then32
	movq	$1, %rsi
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	count_or_remove_death_notes
.LBB6_139:                              # %if.end35
	movq	%r12, %rbx
	movq	96(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	je	.LBB6_154
# BB#140:                               # %do.body
	movl	4(%rax), %eax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB6_158
# BB#141:
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB6_142:                              # %for.body44
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_144 Depth 2
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	16(%rax,%rdx,8), %r12
	testq	%r12, %r12
	je	.LBB6_149
# BB#143:                               # %for.body52.preheader
                                        #   in Loop: Header=BB6_142 Depth=1
	movq	$6, %rcx
	movq	88(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	$1, %r14
	.align	16, 0x90
.LBB6_144:                              # %for.body52
                                        #   Parent Loop BB6_142 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%r14), %rax
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	%r12, %rdx
	je	.LBB6_148
# BB#145:                               # %if.then56
                                        #   in Loop: Header=BB6_144 Depth=2
	movq	104(%rsp), %rcx         # 8-byte Reload
	addq	%rcx, %rax
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rsi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rsi,%rax,8), %r13
	movq	72(%r13), %rsi
	notq	%rdx
	andq	%rdx, %r12
	movq	%rbx, %rdi
	callq	bitmap_copy
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	64(%rsp), %r8           # 8-byte Reload
	callq	propagate_block
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r15, %rax
	jne	.LBB6_147
# BB#146:                               # %if.then67
                                        #   in Loop: Header=BB6_144 Depth=2
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	verify_local_live_at_start
.LBB6_147:                              # %if.end68
                                        #   in Loop: Header=BB6_144 Depth=2
	testq	%r12, %r12
	je	.LBB6_149
.LBB6_148:                              # %for.inc73
                                        #   in Loop: Header=BB6_144 Depth=2
	movq	%r14, %rax
	andq	%r15, %rax
	incq	%r14
	cmpq	$64, %rax
	jb	.LBB6_144
.LBB6_149:                              # %for.inc76
                                        #   in Loop: Header=BB6_142 Depth=1
	movq	88(%rsp), %rdx          # 8-byte Reload
	incq	%rdx
	movq	%rdx, %rax
	andq	%r15, %rax
	movq	80(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB6_142
	jmp	.LBB6_158
.LBB6_154:                              # %if.else
	movslq	n_basic_blocks(%rip), %r13
	testq	%r13, %r13
	jle	.LBB6_158
# BB#155:                               # %for.body82.lr.ph
	decq	%r13
	.align	16, 0x90
.LBB6_156:                              # %for.body82
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r13,8), %r14
	movq	72(%r14), %rsi
	movq	%rbx, %r12
	movq	%r12, %rdi
	callq	bitmap_copy
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	64(%rsp), %r8           # 8-byte Reload
	callq	propagate_block
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r15, %rax
	jne	.LBB6_157
# BB#201:                               # %if.then92
                                        #   in Loop: Header=BB6_156 Depth=1
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	verify_local_live_at_start
.LBB6_157:                              # %for.cond80.backedge
                                        #   in Loop: Header=BB6_156 Depth=1
	movq	%r12, %rbx
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%r13
	testq	%rax, %rax
	jg	.LBB6_156
.LBB6_158:                              # %do.body98
	movq	%rbx, %rdi
	testq	%rdi, %rdi
	je	.LBB6_160
# BB#159:                               # %if.then100
	callq	bitmap_clear
.LBB6_160:                              # %do.end102
	testq	$4, 16(%rsp)            # 8-byte Folded Reload
	movq	%r15, %r14
	je	.LBB6_193
# BB#161:                               # %do.body106
	movq	entry_exit_blocks+72(%rip), %rax
	movq	(%rax), %r10
	testq	%r10, %r10
	je	.LBB6_175
# BB#162:                               # %for.cond125.preheader.lr.ph
	movl	16(%r10), %eax
	testq	%rax, %rax
	movq	$53, %rbx
	je	.LBB6_164
# BB#163:                               # %select.mid
	xorq	%rbx, %rbx
.LBB6_164:                              # %select.end
	movq	reg_n_info(%rip), %r9
	movabsq	$4294967294, %r8        # imm = 0xFFFFFFFE
	.align	16, 0x90
.LBB6_165:                              # %for.cond125.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_167 Depth 2
                                        #     Child Loop BB6_171 Depth 2
	movq	24(%r10), %rdi
	testq	%rdi, %rdi
	je	.LBB6_170
# BB#166:                               # %for.body136.preheader
                                        #   in Loop: Header=BB6_165 Depth=1
	movq	%rbx, %rax
	andq	%r14, %rax
	.align	16, 0x90
.LBB6_167:                              # %for.body136
                                        #   Parent Loop BB6_165 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rdi, %rsi
	je	.LBB6_169
# BB#168:                               # %if.then142
                                        #   in Loop: Header=BB6_167 Depth=2
	movl	16(%r10), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	addq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	32(%r9,%rdx,8), %rcx
	notq	%rsi
	andq	%rsi, %rdi
	movl	%r8d, 36(%rcx)
	je	.LBB6_170
.LBB6_169:                              # %for.inc157
                                        #   in Loop: Header=BB6_167 Depth=2
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r14, %rcx
	incq	%rax
	cmpq	$64, %rcx
	jb	.LBB6_167
.LBB6_170:                              # %if.end160
                                        #   in Loop: Header=BB6_165 Depth=1
	movq	32(%r10), %rdi
	xorq	%rax, %rax
	testq	%rdi, %rdi
	movabsq	$0, %rbx
	je	.LBB6_174
	.align	16, 0x90
.LBB6_171:                              # %for.body136.1
                                        #   Parent Loop BB6_165 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rdi, %rsi
	je	.LBB6_173
# BB#172:                               # %if.then142.1
                                        #   in Loop: Header=BB6_171 Depth=2
	movl	16(%r10), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	leaq	64(%rax,%rdx), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	32(%r9,%rdx,8), %rcx
	notq	%rsi
	andq	%rsi, %rdi
	movl	%r8d, 36(%rcx)
	je	.LBB6_174
.LBB6_173:                              # %for.inc157.1
                                        #   in Loop: Header=BB6_171 Depth=2
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r14, %rcx
	incq	%rax
	cmpq	$64, %rcx
	jb	.LBB6_171
.LBB6_174:                              # %if.end160.1
                                        #   in Loop: Header=BB6_165 Depth=1
	movq	(%r10), %r10
	xorq	%rbx, %rbx
	testq	%r10, %r10
	jne	.LBB6_165
.LBB6_175:                              # %do.body168
	movq	regs_live_at_setjmp(%rip), %rax
	movq	(%rax), %r10
	testq	%r10, %r10
	je	.LBB6_193
# BB#176:                               # %for.cond196.preheader.lr.ph
	movl	16(%r10), %eax
	testq	%rax, %rax
	movq	$53, %rbx
	je	.LBB6_178
# BB#177:                               # %select.mid96
	xorq	%rbx, %rbx
.LBB6_178:                              # %select.end95
	movq	cfun(%rip), %r9
	movq	reg_n_info(%rip), %r8
	.align	16, 0x90
.LBB6_179:                              # %for.cond196.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_181 Depth 2
                                        #     Child Loop BB6_187 Depth 2
	movq	24(%r10), %rdi
	testq	%rdi, %rdi
	je	.LBB6_186
# BB#180:                               # %for.body208.preheader
                                        #   in Loop: Header=BB6_179 Depth=1
	movq	%rbx, %rax
	andq	%r14, %rax
	.align	16, 0x90
.LBB6_181:                              # %for.body208
                                        #   Parent Loop BB6_179 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rdi, %rsi
	je	.LBB6_185
# BB#182:                               # %if.then215
                                        #   in Loop: Header=BB6_181 Depth=2
	notq	%rsi
	andq	%rsi, %rdi
	movl	16(%r10), %esi
	movq	$7, %rcx
	movq	24(%r9), %rdx
	shlq	%cl, %rsi
	addq	%rax, %rsi
	movq	$32, %rcx
	movq	80(%rdx), %rdx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$0, (%rdx,%rsi,8)
	je	.LBB6_184
# BB#183:                               # %if.then226
                                        #   in Loop: Header=BB6_181 Depth=2
	movq	32(%r8,%rsi,8), %rcx
	movl	%r14d, 28(%rcx)
	movl	%r14d, 36(%rcx)
.LBB6_184:                              # %if.end236
                                        #   in Loop: Header=BB6_181 Depth=2
	testq	%rdi, %rdi
	je	.LBB6_186
.LBB6_185:                              # %for.inc241
                                        #   in Loop: Header=BB6_181 Depth=2
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r14, %rcx
	incq	%rax
	cmpq	$64, %rcx
	jb	.LBB6_181
.LBB6_186:                              # %if.end244
                                        #   in Loop: Header=BB6_179 Depth=1
	movq	32(%r10), %r11
	xorq	%rax, %rax
	testq	%r11, %r11
	movabsq	$0, %rbx
	je	.LBB6_192
	.align	16, 0x90
.LBB6_187:                              # %for.body208.1
                                        #   Parent Loop BB6_179 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%r11, %rsi
	je	.LBB6_191
# BB#188:                               # %if.then215.1
                                        #   in Loop: Header=BB6_187 Depth=2
	notq	%rsi
	andq	%rsi, %r11
	movl	16(%r10), %edx
	movq	$7, %rcx
	movq	24(%r9), %rdi
	shlq	%cl, %rdx
	leaq	64(%rax,%rdx), %rsi
	movq	$32, %rcx
	movq	80(%rdi), %rdx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$0, (%rdx,%rsi,8)
	je	.LBB6_190
# BB#189:                               # %if.then226.1
                                        #   in Loop: Header=BB6_187 Depth=2
	movq	32(%r8,%rsi,8), %rcx
	movl	%r14d, 28(%rcx)
	movl	%r14d, 36(%rcx)
.LBB6_190:                              # %if.end236.1
                                        #   in Loop: Header=BB6_187 Depth=2
	testq	%r11, %r11
	je	.LBB6_192
.LBB6_191:                              # %for.inc241.1
                                        #   in Loop: Header=BB6_187 Depth=2
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r14, %rcx
	incq	%rax
	cmpq	$64, %rcx
	jb	.LBB6_187
.LBB6_192:                              # %if.end244.1
                                        #   in Loop: Header=BB6_179 Depth=1
	movq	(%r10), %r10
	xorq	%rbx, %rbx
	testq	%r10, %r10
	jne	.LBB6_179
.LBB6_193:                              # %if.end252
	movq	$6, %rdi
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	je	.LBB6_198
# BB#194:                               # %lor.rhs254
	cmpq	$0, 96(%rsp)            # 8-byte Folded Reload
	je	.LBB6_195
# BB#196:                               # %lor.rhs254
	movabsq	$1, %rdi
	jmp	.LBB6_197
.LBB6_195:
	movabsq	$0, %rdi
.LBB6_197:                              # %lor.rhs254
	addq	$5, %rdi
.LBB6_198:                              # %lor.end256
	callq	timevar_pop
	addq	$208, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB6_199:                              # %if.then
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.update_life_info, %rcx
	movq	$650, %rsi              # imm = 0x28A
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp78:
	.size	update_life_info, .Ltmp78-update_life_info
	.cfi_endproc

	.globl	free_basic_block_vars
	.align	16, 0x90
	.type	free_basic_block_vars,@function
free_basic_block_vars:                  # @free_basic_block_vars
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp80:
	.cfi_def_cfa_offset 16
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdi
	jne	.LBB7_5
# BB#1:                                 # %if.then
	cmpq	$0, basic_block_info(%rip)
	je	.LBB7_4
# BB#2:                                 # %if.then2
	callq	clear_edges
	movq	basic_block_info(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB7_4
# BB#3:                                 # %if.then4
	callq	free
	movq	$0, basic_block_info(%rip)
.LBB7_4:                                # %if.end5
	xorq	%rax, %rax
	movl	%eax, n_basic_blocks(%rip)
	movq	$0, entry_exit_blocks+192(%rip)
	movq	$0, entry_exit_blocks+176(%rip)
	movq	$0, entry_exit_blocks+80(%rip)
	movq	$0, entry_exit_blocks+72(%rip)
.LBB7_5:                                # %if.end6
	popq	%rax
	retq
.Ltmp81:
	.size	free_basic_block_vars, .Ltmp81-free_basic_block_vars
	.cfi_endproc

	.globl	propagate_block
	.align	16, 0x90
	.type	propagate_block,@function
propagate_block:                        # @propagate_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp88:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp89:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp90:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp91:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp92:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp93:
	.cfi_def_cfa_offset 64
.Ltmp94:
	.cfi_offset %rbx, -48
.Ltmp95:
	.cfi_offset %r12, -40
.Ltmp96:
	.cfi_offset %r13, -32
.Ltmp97:
	.cfi_offset %r14, -24
.Ltmp98:
	.cfi_offset %r15, -16
	movq	%r8, %rbx
	movq	%rsi, %r14
	movq	%rdi, 8(%rsp)           # 8-byte Spill
                                        # kill: RSI<def> R14<kill>
                                        # kill: R8<def> RBX<kill>
	callq	init_propagate_block_info
	movq	%rax, %r12
	andq	$4, %rbx
	movq	%rbx, (%rsp)            # 8-byte Spill
	je	.LBB8_12
# BB#1:                                 # %do.body
	movq	(%r14), %r11
	testq	%r11, %r11
	je	.LBB8_12
# BB#2:                                 # %for.cond7.preheader.lr.ph
	movq	reg_n_info(%rip), %r10
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movabsq	$4294967294, %r8        # imm = 0xFFFFFFFE
	.align	16, 0x90
.LBB8_3:                                # %for.cond7.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_4 Depth 2
                                        #     Child Loop BB8_8 Depth 2
	movq	24(%r11), %rbx
	testq	%rbx, %rbx
	movq	$1, %rdi
	je	.LBB8_7
	.align	16, 0x90
.LBB8_4:                                # %for.body14
                                        #   Parent Loop BB8_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%rdi), %rax
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rbx, %rsi
	je	.LBB8_6
# BB#5:                                 # %if.then17
                                        #   in Loop: Header=BB8_4 Depth=2
	movl	16(%r11), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	addq	%rdx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%r10,%rax,8), %rax
	notq	%rsi
	andq	%rsi, %rbx
	movl	%r8d, 36(%rax)
	je	.LBB8_7
.LBB8_6:                                # %for.inc
                                        #   in Loop: Header=BB8_4 Depth=2
	movq	%rdi, %rax
	andq	%r9, %rax
	incq	%rdi
	cmpq	$64, %rax
	jb	.LBB8_4
.LBB8_7:                                # %if.end28
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	32(%r11), %rbx
	xorq	%rax, %rax
	testq	%rbx, %rbx
	je	.LBB8_11
	.align	16, 0x90
.LBB8_8:                                # %for.body14.1
                                        #   Parent Loop BB8_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rbx, %rsi
	je	.LBB8_10
# BB#9:                                 # %if.then17.1
                                        #   in Loop: Header=BB8_8 Depth=2
	movl	16(%r11), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	leaq	64(%rax,%rdx), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	32(%r10,%rdx,8), %rcx
	notq	%rsi
	andq	%rsi, %rbx
	movl	%r8d, 36(%rcx)
	je	.LBB8_11
.LBB8_10:                               # %for.inc.1
                                        #   in Loop: Header=BB8_8 Depth=2
	incq	%rax
	movq	%rax, %rcx
	andq	%r9, %rcx
	cmpq	$64, %rcx
	jb	.LBB8_8
.LBB8_11:                               # %if.end28.1
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	(%r11), %r11
	testq	%r11, %r11
	jne	.LBB8_3
.LBB8_12:                               # %if.end35
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	8(%rax), %rbx
	xorq	%r13, %r13
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movabsq	$0, %r14
	.align	16, 0x90
.LBB8_13:                               # %for.cond36
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsp), %rax            # 8-byte Reload
	testq	%r15, %rax
	je	.LBB8_17
# BB#14:                                # %land.lhs.true39
                                        #   in Loop: Header=BB8_13 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB8_17
# BB#15:                                # %land.lhs.true41
                                        #   in Loop: Header=BB8_13 Depth=1
	movq	$28, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB8_17
# BB#16:                                # %if.then44
                                        #   in Loop: Header=BB8_13 Depth=1
	movq	regs_live_at_setjmp(%rip), %rdi
	movq	8(%r12), %rdx
	movq	$2, %rcx
	movq	%rdi, %rsi
	callq	bitmap_operation
	.align	16, 0x90
.LBB8_17:                               # %if.end46
                                        #   in Loop: Header=BB8_13 Depth=1
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	propagate_one_insn
	movq	24(%rax), %rcx
	cmpq	%rbx, %rcx
	movq	%r14, %rcx
	je	.LBB8_19
# BB#18:                                # %if.end46
                                        #   in Loop: Header=BB8_13 Depth=1
	movabsq	$1, %rcx
.LBB8_19:                               # %if.end46
                                        #   in Loop: Header=BB8_13 Depth=1
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	(%rdx), %rdx
	orq	%rcx, %r13
	cmpq	%rdx, %rbx
	movq	%rax, %rbx
	jne	.LBB8_13
# BB#20:                                # %for.end55
	leaq	32(%r12), %rdi
	callq	free_EXPR_LIST_list
	movq	16(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB8_22
# BB#21:                                # %if.then.i
	callq	bitmap_clear
	movq	16(%r12), %rdi
	callq	free
	movq	$0, 16(%r12)
.LBB8_22:                               # %do.end.i
	movq	24(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB8_24
# BB#23:                                # %if.then5.i
	callq	free
.LBB8_24:                               # %free_propagate_block_info.exit
	movq	%r12, %rdi
	callq	free
	movq	%r13, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp99:
	.size	propagate_block, .Ltmp99-propagate_block
	.cfi_endproc

	.globl	count_or_remove_death_notes
	.align	16, 0x90
	.type	count_or_remove_death_notes,@function
count_or_remove_death_notes:            # @count_or_remove_death_notes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp106:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp107:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp108:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp109:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp110:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp111:
	.cfi_def_cfa_offset 80
.Ltmp112:
	.cfi_offset %rbx, -48
.Ltmp113:
	.cfi_offset %r12, -40
.Ltmp114:
	.cfi_offset %r13, -32
.Ltmp115:
	.cfi_offset %r14, -24
.Ltmp116:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	movl	n_basic_blocks(%rip), %r12d
	jmp	.LBB9_1
.LBB9_30:                               #   in Loop: Header=BB9_1 Depth=1
	movq	8(%rsp), %rdi           # 8-byte Reload
	.align	16, 0x90
.LBB9_1:                                # %for.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_5 Depth 2
                                        #       Child Loop BB9_8 Depth 3
                                        #         Child Loop BB9_9 Depth 4
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB9_32
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB9_1 Depth=1
	decq	%r12
	testq	%rdi, %rdi
	je	.LBB9_4
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	%r12, %rax
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rax
	movq	$3, %rcx
	shrq	%cl, %rax
	movq	16(%rdi,%rax), %rax
	movq	$1, %rdx
	movq	%r12, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB9_1
.LBB9_4:                                # %if.end
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	movq	basic_block_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %r15
	movq	%r15, 16(%rsp)          # 8-byte Spill
	jmp	.LBB9_5
	.align	16, 0x90
.LBB9_31:                               # %for.inc
                                        #   in Loop: Header=BB9_5 Depth=2
	addq	$24, %r15
.LBB9_5:                                # %for.cond5
                                        #   Parent Loop BB9_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB9_8 Depth 3
                                        #         Child Loop BB9_9 Depth 4
	movq	(%r15), %r15
	movzbl	(%r15), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB9_29
# BB#6:                                 # %if.then10
                                        #   in Loop: Header=BB9_5 Depth=2
	movq	56(%r15), %rax
	testq	%rax, %rax
	je	.LBB9_29
# BB#7:                                 #   in Loop: Header=BB9_5 Depth=2
	leaq	56(%r15), %r14
.LBB9_8:                                # %while.body.lr.ph
                                        #   Parent Loop BB9_1 Depth=1
                                        #     Parent Loop BB9_5 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB9_9 Depth 4
	movq	%rax, %rdi
	.align	16, 0x90
.LBB9_9:                                # %while.body
                                        #   Parent Loop BB9_1 Depth=1
                                        #     Parent Loop BB9_5 Depth=2
                                        #       Parent Loop BB9_8 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movzbl	2(%rdi), %eax
	cmpq	$10, %rax
	je	.LBB9_26
# BB#10:                                # %while.body
                                        #   in Loop: Header=BB9_9 Depth=4
	cmpq	$1, %rax
	jne	.LBB9_28
# BB#11:                                # %sw.bb
                                        #   in Loop: Header=BB9_9 Depth=4
	movq	8(%rdi), %rax
	movl	(%rax), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB9_26
# BB#12:                                # %if.then22
                                        #   in Loop: Header=BB9_9 Depth=4
	movl	8(%rax), %esi
	cmpq	$52, %rsi
	movq	$1, %rax
	ja	.LBB9_25
# BB#13:                                # %if.else
                                        #   in Loop: Header=BB9_9 Depth=4
	movq	%rsi, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	cmpq	$8, %rax
	je	.LBB9_16
# BB#14:                                # %if.else
                                        #   in Loop: Header=BB9_9 Depth=4
	cmpq	$44, %rsi
	ja	.LBB9_16
# BB#15:                                # %if.else
                                        #   in Loop: Header=BB9_9 Depth=4
	addq	$-21, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	cmpq	$15, %rsi
	jbe	.LBB9_16
# BB#20:                                # %cond.false
                                        #   in Loop: Header=BB9_9 Depth=4
	cmpq	$24, %rdx
	jne	.LBB9_21
# BB#23:                                # %cond.true106
                                        #   in Loop: Header=BB9_9 Depth=4
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB9_25
.LBB9_16:                               # %cond.true
                                        #   in Loop: Header=BB9_9 Depth=4
	movl	mode_class(,%rdx,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB9_25
# BB#17:                                # %lor.rhs
                                        #   in Loop: Header=BB9_9 Depth=4
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB9_19
# BB#18:                                # %lor.rhs
                                        #   in Loop: Header=BB9_9 Depth=4
	movabsq	$1, %rax
.LBB9_19:                               # %lor.rhs
                                        #   in Loop: Header=BB9_9 Depth=4
	incq	%rax
	jmp	.LBB9_25
.LBB9_21:                               # %cond.false
                                        #   in Loop: Header=BB9_9 Depth=4
	cmpq	$18, %rdx
	jne	.LBB9_24
# BB#22:                                # %cond.true96
                                        #   in Loop: Header=BB9_9 Depth=4
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB9_25
.LBB9_24:                               # %cond.false110
                                        #   in Loop: Header=BB9_9 Depth=4
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rdx), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	.align	16, 0x90
.LBB9_25:                               # %if.end130
                                        #   in Loop: Header=BB9_9 Depth=4
	addq	%rax, 24(%rsp)          # 8-byte Folded Spill
.LBB9_26:                               # %sw.bb133
                                        #   in Loop: Header=BB9_9 Depth=4
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB9_28
# BB#27:                                # %if.then135
                                        #   in Loop: Header=BB9_9 Depth=4
	movq	16(%rdi), %r13
	callq	free_EXPR_LIST_node
	testq	%r13, %r13
	movq	%r13, (%r14)
	movq	%r13, %rdi
	jne	.LBB9_9
	jmp	.LBB9_29
	.align	16, 0x90
.LBB9_28:                               # %sw.default
                                        #   in Loop: Header=BB9_8 Depth=3
	movq	16(%rdi), %rax
	addq	$16, %rdi
	testq	%rax, %rax
	movq	%rdi, %r14
	jne	.LBB9_8
	.align	16, 0x90
.LBB9_29:                               # %if.end143
                                        #   in Loop: Header=BB9_5 Depth=2
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	cmpq	%rax, %r15
	jne	.LBB9_31
	jmp	.LBB9_30
.LBB9_32:                               # %for.end152
	movq	24(%rsp), %rax          # 8-byte Reload
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp117:
	.size	count_or_remove_death_notes, .Ltmp117-count_or_remove_death_notes
	.cfi_endproc

	.align	16, 0x90
	.type	verify_local_live_at_start,@function
verify_local_live_at_start:             # @verify_local_live_at_start
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp124:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp125:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp126:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp127:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp128:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp129:
	.cfi_def_cfa_offset 80
.Ltmp130:
	.cfi_offset %rbx, -48
.Ltmp131:
	.cfi_offset %r12, -40
.Ltmp132:
	.cfi_offset %r13, -32
.Ltmp133:
	.cfi_offset %r14, -24
.Ltmp134:
	.cfi_offset %r15, -16
	movl	reload_completed(%rip), %ecx
	movq	64(%rsi), %rax
	movq	%rsi, %rbx
	movq	%rdi, %r14
	testq	%rcx, %rcx
	je	.LBB10_5
# BB#1:                                 # %if.then
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	bitmap_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB10_28
# BB#2:                                 # %if.then2
	movq	rtl_dump_file(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB10_4
# BB#3:                                 # %if.then4
	movl	88(%rbx), %edx
	movabsq	$.L.str9, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	fprintf
	movq	rtl_dump_file(%rip), %rax
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	debug_bitmap_file
	movq	rtl_dump_file(%rip), %rax
	movabsq	$.L.str10, %rcx
	movq	$5, %rsi
	movq	$1, %rdx
	movq	%rcx, %rdi
	movq	%rax, %rcx
	callq	fwrite
	movq	rtl_dump_file(%rip), %rax
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	dump_bb
.LBB10_4:                               # %if.end
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.verify_local_live_at_start, %rcx
	movq	$583, %rsi              # imm = 0x247
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB10_5:                               # %if.else
	movq	$3, %rcx
	movq	%r14, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	bitmap_operation
	movq	(%r14), %rax
	jmp	.LBB10_6
	.align	16, 0x90
.LBB10_27:                              # %for.end47
                                        #   in Loop: Header=BB10_6 Depth=1
	movq	%rdx, %rbx
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
.LBB10_6:                               # %if.else
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB10_8 Depth 2
                                        #       Child Loop BB10_10 Depth 3
                                        #         Child Loop BB10_16 Depth 4
	movq	%rax, 16(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB10_28
# BB#7:                                 # %for.cond17.preheader
                                        #   in Loop: Header=BB10_6 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	.align	16, 0x90
.LBB10_8:                               # %for.body19
                                        #   Parent Loop BB10_6 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB10_10 Depth 3
                                        #         Child Loop BB10_16 Depth 4
	movq	%rsi, (%rsp)            # 8-byte Spill
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	24(%rax,%rsi,8), %r14
	testq	%r14, %r14
	je	.LBB10_26
# BB#9:                                 # %for.body24.preheader
                                        #   in Loop: Header=BB10_8 Depth=2
	movq	$6, %rcx
	movq	(%rsp), %rax            # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%r13, %r13
	.align	16, 0x90
.LBB10_10:                              # %for.body24
                                        #   Parent Loop BB10_6 Depth=1
                                        #     Parent Loop BB10_8 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB10_16 Depth 4
	movq	$1, %r12
	movq	%r13, %rcx
	shlq	%cl, %r12
	testq	%r14, %r12
	je	.LBB10_25
# BB#11:                                # %if.then26
                                        #   in Loop: Header=BB10_10 Depth=3
	movq	16(%rsp), %rax          # 8-byte Reload
	movl	16(%rax), %eax
	movq	$7, %rcx
	movq	64(%rdx), %rdi
	shlq	%cl, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	leaq	(%r13,%rcx), %rbx
	addq	%rax, %rbx
	movq	%rdx, %r15
	movq	%rbx, %rsi
	callq	bitmap_bit_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB10_12
# BB#15:                                # %if.end39
                                        #   in Loop: Header=BB10_10 Depth=3
	movl	%ebx, 28(%rsp)
	movq	8(%r15), %rbx
	notq	%r12
	andq	%r12, %r14
	movq	%r15, %r12
	movq	%r15, %rcx
	jmp	.LBB10_16
	.align	16, 0x90
.LBB10_20:                              # %if.end16.i
                                        #   in Loop: Header=BB10_16 Depth=4
	addq	$24, %r12
.LBB10_16:                              # %while.body.i
                                        #   Parent Loop BB10_6 Depth=1
                                        #     Parent Loop BB10_8 Depth=2
                                        #       Parent Loop BB10_10 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	(%r12), %r12
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB10_19
# BB#17:                                # %if.then.i
                                        #   in Loop: Header=BB10_16 Depth=4
	movq	%rcx, %r15
	leaq	32(%r12), %rdi
	movabsq	$verify_wide_reg_1, %rsi
	leaq	28(%rsp), %rdx
	callq	for_each_rtx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	je	.LBB10_24
# BB#18:                                # %if.then.i
                                        #   in Loop: Header=BB10_16 Depth=4
	cmpq	$2, %rax
	movq	%r15, %rcx
	je	.LBB10_21
	.align	16, 0x90
.LBB10_19:                              # %if.end12.i
                                        #   in Loop: Header=BB10_16 Depth=4
	cmpq	%rbx, %r12
	jne	.LBB10_20
	jmp	.LBB10_21
	.align	16, 0x90
.LBB10_24:                              # %verify_wide_reg.exit
                                        #   in Loop: Header=BB10_10 Depth=3
	testq	%r14, %r14
	movq	%r15, %rdx
	je	.LBB10_26
.LBB10_25:                              # %for.inc
                                        #   in Loop: Header=BB10_10 Depth=3
	incq	%r13
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB10_10
.LBB10_26:                              # %if.end44
                                        #   in Loop: Header=BB10_8 Depth=2
	movq	(%rsp), %rsi            # 8-byte Reload
	incq	%rsi
	movq	%rsi, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$2, %rax
	jb	.LBB10_8
	jmp	.LBB10_27
.LBB10_28:                              # %if.end51
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB10_12:                              # %if.then34
	movq	%r15, %r14
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB10_14
# BB#13:                                # %if.then36
	movabsq	$.L.str11, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	fprintf
	movq	rtl_dump_file(%rip), %rsi
	movq	%r14, %rdi
	callq	dump_bb
.LBB10_14:                              # %if.end38
	movabsq	$.L.str3, %rdi
	movabsq	$.L__FUNCTION__.verify_local_live_at_start, %rdx
	movq	$604, %rsi              # imm = 0x25C
	callq	fancy_abort
.LBB10_21:                              # %while.end.i
	movq	%rcx, %rbx
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB10_23
# BB#22:                                # %if.then20.i
	movl	28(%rsp), %edx
	movabsq	$.L.str11, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movq	rtl_dump_file(%rip), %rsi
	movq	%rbx, %rdi
	callq	dump_bb
.LBB10_23:                              # %if.end22.i
	movabsq	$.L.str3, %rdi
	movabsq	$.L__FUNCTION__.verify_wide_reg, %rdx
	movq	$557, %rsi              # imm = 0x22D
	callq	fancy_abort
.Ltmp135:
	.size	verify_local_live_at_start, .Ltmp135-verify_local_live_at_start
	.cfi_endproc

	.globl	initialize_uninitialized_subregs
	.align	16, 0x90
	.type	initialize_uninitialized_subregs,@function
initialize_uninitialized_subregs:       # @initialize_uninitialized_subregs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp142:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp143:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp144:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp145:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp146:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp147:
	.cfi_def_cfa_offset 112
.Ltmp148:
	.cfi_offset %rbx, -48
.Ltmp149:
	.cfi_offset %r12, -40
.Ltmp150:
	.cfi_offset %r13, -32
.Ltmp151:
	.cfi_offset %r14, -24
.Ltmp152:
	.cfi_offset %r15, -16
	movq	entry_exit_blocks+40(%rip), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB11_7
# BB#1:                                 # %for.body.lr.ph
	movq	$0, (%rsp)              # 8-byte Folded Spill
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB11_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB11_10 Depth 2
                                        #       Child Loop BB11_12 Depth 3
                                        #         Child Loop BB11_14 Depth 4
                                        #       Child Loop BB11_22 Depth 3
                                        #         Child Loop BB11_24 Depth 4
	movq	24(%rcx), %rax
	movq	64(%rax), %rax
	movq	(%rax), %rdx
	testq	%rdx, %rdx
	jne	.LBB11_8
# BB#3:                                 #   in Loop: Header=BB11_2 Depth=1
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	jmp	.LBB11_4
	.align	16, 0x90
.LBB11_8:                               # %for.cond8.preheader.lr.ph
                                        #   in Loop: Header=BB11_2 Depth=1
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movl	16(%rdx), %eax
	testq	%rax, %rax
	movq	$53, %r15
	je	.LBB11_10
# BB#9:                                 # %select.mid
                                        #   in Loop: Header=BB11_2 Depth=1
	xorq	%r15, %r15
	.align	16, 0x90
.LBB11_10:                              # %for.cond8.preheader
                                        #   Parent Loop BB11_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB11_12 Depth 3
                                        #         Child Loop BB11_14 Depth 4
                                        #       Child Loop BB11_22 Depth 3
                                        #         Child Loop BB11_24 Depth 4
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	24(%rdx), %rdi
	testq	%rdi, %rdi
	je	.LBB11_21
# BB#11:                                # %for.body15.preheader
                                        #   in Loop: Header=BB11_10 Depth=2
	movq	%r15, %rbx
	andq	%rsi, %rbx
	.align	16, 0x90
.LBB11_12:                              # %for.body15
                                        #   Parent Loop BB11_2 Depth=1
                                        #     Parent Loop BB11_10 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB11_14 Depth 4
	movq	$1, %r12
	movq	%rbx, %rcx
	shlq	%cl, %r12
	testq	%rdi, %r12
	je	.LBB11_20
# BB#13:                                # %if.then17
                                        #   in Loop: Header=BB11_12 Depth=3
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movl	16(%rdx), %r14d
	movq	$7, %rcx
	shlq	%cl, %r14
	addq	%rbx, %r14
	movq	$32, %rcx
	movq	%r14, %rax
	movq	reg_n_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %rax
	movl	(%rax), %r13d
	callq	get_insns
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	jmp	.LBB11_14
	.align	16, 0x90
.LBB11_16:                              # %for.inc
                                        #   in Loop: Header=BB11_14 Depth=4
	movq	24(%rax), %rax
.LBB11_14:                              # %if.then17
                                        #   Parent Loop BB11_2 Depth=1
                                        #     Parent Loop BB11_10 Depth=2
                                        #       Parent Loop BB11_12 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	testq	%rax, %rax
	movq	%rax, 40(%rsp)
	je	.LBB11_19
# BB#15:                                # %land.rhs27
                                        #   in Loop: Header=BB11_14 Depth=4
	movl	8(%rax), %ecx
	movq	%r13, %rdx
	andq	%rsi, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB11_16
# BB#17:                                # %if.then35
                                        #   in Loop: Header=BB11_12 Depth=3
	movl	%r14d, 48(%rsp)
	leaq	40(%rsp), %rdi
	movq	%rsi, %r14
	movabsq	$find_regno_partial, %rsi
	leaq	48(%rsp), %rdx
	callq	for_each_rtx
	movq	%r14, %rsi
	movq	56(%rsp), %rdi
	testq	%rdi, %rdi
	je	.LBB11_19
# BB#18:                                # %if.then39
                                        #   in Loop: Header=BB11_12 Depth=3
	movzbl	2(%rdi), %eax
	movq	%rsi, %r14
	movq	const_tiny_rtx(,%rax,8), %rsi
	callq	gen_move_insn
	movq	%rax, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	insert_insn_on_edge
	movq	%r14, %rsi
	movq	$1, (%rsp)              # 8-byte Folded Spill
	.align	16, 0x90
.LBB11_19:                              # %if.end46
                                        #   in Loop: Header=BB11_12 Depth=3
	notq	%r12
	movq	24(%rsp), %rdi          # 8-byte Reload
	andq	%r12, %rdi
	movq	32(%rsp), %rdx          # 8-byte Reload
	je	.LBB11_21
.LBB11_20:                              # %for.inc51
                                        #   in Loop: Header=BB11_12 Depth=3
	incq	%r15
	movq	%r15, %rax
	andq	%rsi, %rax
	incq	%rbx
	cmpq	$64, %rax
	jb	.LBB11_12
.LBB11_21:                              # %if.end53
                                        #   in Loop: Header=BB11_10 Depth=2
	movq	32(%rdx), %rdi
	xorq	%r12, %r12
	testq	%rdi, %rdi
	movabsq	$0, %r13
	je	.LBB11_31
	.align	16, 0x90
.LBB11_22:                              # %for.body15.1
                                        #   Parent Loop BB11_2 Depth=1
                                        #     Parent Loop BB11_10 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB11_24 Depth 4
	movq	$1, %rbx
	movq	%r12, %rcx
	shlq	%cl, %rbx
	testq	%rdi, %rbx
	je	.LBB11_30
# BB#23:                                # %if.then17.1
                                        #   in Loop: Header=BB11_22 Depth=3
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movl	16(%rdx), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	64(%r12,%rax), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	reg_n_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %rax
	movl	(%rax), %r14d
	movq	%rsi, %r15
	callq	get_insns
	movq	%r15, %rsi
	jmp	.LBB11_24
	.align	16, 0x90
.LBB11_26:                              # %for.inc.1
                                        #   in Loop: Header=BB11_24 Depth=4
	movq	24(%rax), %rax
.LBB11_24:                              # %if.then17.1
                                        #   Parent Loop BB11_2 Depth=1
                                        #     Parent Loop BB11_10 Depth=2
                                        #       Parent Loop BB11_22 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	testq	%rax, %rax
	movq	%rax, 40(%rsp)
	je	.LBB11_29
# BB#25:                                # %land.rhs27.1
                                        #   in Loop: Header=BB11_24 Depth=4
	movl	8(%rax), %ecx
	movq	%r14, %rdx
	andq	%rsi, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB11_26
# BB#27:                                # %if.then35.1
                                        #   in Loop: Header=BB11_22 Depth=3
	movq	16(%rsp), %rax          # 8-byte Reload
	movl	%eax, 48(%rsp)
	leaq	40(%rsp), %rdi
	movq	%rsi, %r14
	movabsq	$find_regno_partial, %rsi
	leaq	48(%rsp), %rdx
	callq	for_each_rtx
	movq	%r14, %rsi
	movq	56(%rsp), %rdi
	testq	%rdi, %rdi
	je	.LBB11_29
# BB#28:                                # %if.then39.1
                                        #   in Loop: Header=BB11_22 Depth=3
	movzbl	2(%rdi), %eax
	movq	%rsi, %r14
	movq	const_tiny_rtx(,%rax,8), %rsi
	callq	gen_move_insn
	movq	%rax, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	insert_insn_on_edge
	movq	%r14, %rsi
	movq	$1, (%rsp)              # 8-byte Folded Spill
	.align	16, 0x90
.LBB11_29:                              # %if.end46.1
                                        #   in Loop: Header=BB11_22 Depth=3
	notq	%rbx
	movq	24(%rsp), %rdi          # 8-byte Reload
	andq	%rbx, %rdi
	movq	32(%rsp), %rdx          # 8-byte Reload
	je	.LBB11_31
.LBB11_30:                              # %for.inc51.1
                                        #   in Loop: Header=BB11_22 Depth=3
	incq	%r13
	movq	%r13, %rax
	andq	%rsi, %rax
	incq	%r12
	cmpq	$64, %rax
	jb	.LBB11_22
.LBB11_31:                              # %if.end53.1
                                        #   in Loop: Header=BB11_10 Depth=2
	movq	(%rdx), %rdx
	xorq	%r15, %r15
	testq	%rdx, %rdx
	jne	.LBB11_10
.LBB11_4:                               # %for.inc60
                                        #   in Loop: Header=BB11_2 Depth=1
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB11_2
# BB#5:                                 # %for.end61
	xorq	%rax, %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	testq	%rsi, %rcx
	je	.LBB11_7
# BB#6:                                 # %if.then63
	movq	%rcx, %rbx
	callq	commit_edge_insertions
	movq	%rbx, %rax
.LBB11_7:                               # %if.end64
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp153:
	.size	initialize_uninitialized_subregs, .Ltmp153-initialize_uninitialized_subregs
	.cfi_endproc

	.align	16, 0x90
	.type	find_regno_partial,@function
find_regno_partial:                     # @find_regno_partial
	.cfi_startproc
# BB#0:                                 # %entry
	movl	(%rsi), %r9d
	movq	$0, 8(%rsi)
	movq	(%rdi), %rdi
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB12_7
# BB#1:                                 # %if.end
	movzbl	(%rdi), %edx
	leaq	-132(%rdx), %rcx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %rcx
	cmpq	$2, %rcx
	jb	.LBB12_4
# BB#2:                                 # %if.end
	cmpq	$63, %rdx
	je	.LBB12_4
# BB#3:                                 # %if.end
	cmpq	$64, %rdx
	jne	.LBB12_7
.LBB12_4:                               # %sw.bb17
	movq	8(%rdi), %rdi
	movzbl	(%rdi), %ecx
	cmpq	$61, %rcx
	jne	.LBB12_7
# BB#5:                                 # %land.lhs.true24
	movl	8(%rdi), %ecx
	andq	%r8, %r9
	cmpq	%r9, %rcx
	jne	.LBB12_7
# BB#6:                                 # %if.then32
	movq	%rdi, 8(%rsi)
	movq	$1, %rax
.LBB12_7:                               # %return
	retq
.Ltmp154:
	.size	find_regno_partial, .Ltmp154-find_regno_partial
	.cfi_endproc

	.globl	propagate_one_insn
	.align	16, 0x90
	.type	propagate_one_insn,@function
propagate_one_insn:                     # @propagate_one_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp161:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp162:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp163:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp164:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp165:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp166:
	.cfi_def_cfa_offset 80
.Ltmp167:
	.cfi_offset %rbx, -48
.Ltmp168:
	.cfi_offset %r12, -40
.Ltmp169:
	.cfi_offset %r13, -32
.Ltmp170:
	.cfi_offset %r14, -24
.Ltmp171:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	movq	16(%r14), %r13
	movq	%rdi, %r15
	cmpq	$105, %rax
	jne	.LBB13_59
# BB#1:                                 # %if.end
	movq	%r13, 16(%rsp)          # 8-byte Spill
	movl	64(%r15), %ebx
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	movq	%r14, %rdi
	callq	find_reg_note
	movq	%rax, %r12
	testq	$16, %rbx
	movabsq	$0, %rbx
	je	.LBB13_60
# BB#2:                                 # %if.then4
	movq	32(%r14), %rsi
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	callq	insn_dead_p
	movq	%rax, %r13
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
                                        # kill: RAX<def> R13<kill>
	andq	%rdx, %rax
	je	.LBB13_3
# BB#4:                                 # %if.then4
	movabsq	$1, %rcx
	jmp	.LBB13_5
.LBB13_3:
	movabsq	$0, %rcx
.LBB13_5:                               # %if.then4
	testq	%rax, %rax
	je	.LBB13_6
# BB#7:                                 # %if.then4
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	testq	%r12, %r12
	je	.LBB13_8
# BB#9:                                 # %land.rhs
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	movq	$1, %rbx
	jne	.LBB13_34
# BB#10:                                # %cond.true.i
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	movq	%rdx, %r12
	cmpq	$47, %rcx
	je	.LBB13_12
# BB#11:                                # %cond.false.i
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB13_12:                              # %cond.end15.i
	testq	%rax, %rax
	movq	%r12, %rdx
	je	.LBB13_13
# BB#14:                                # %if.then.i
	movq	16(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	movq	8(%rsp), %r12           # 8-byte Reload
	jne	.LBB13_34
# BB#15:                                # %if.then24.i
	movq	8(%r12), %rax
	cmpq	%r14, %rax
	je	.LBB13_16
	.align	16, 0x90
.LBB13_19:                              # %land.rhs.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$34, %rcx
	je	.LBB13_20
# BB#17:                                # %while.cond.i
                                        #   in Loop: Header=BB13_19 Depth=1
	movq	24(%rax), %rax
	cmpq	%r14, %rax
	jne	.LBB13_19
# BB#18:
	xorq	%rbx, %rbx
	jmp	.LBB13_34
.LBB13_6:
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	xorq	%rbx, %rbx
	jmp	.LBB13_34
.LBB13_8:
	xorq	%rbx, %rbx
	jmp	.LBB13_34
.LBB13_13:
	movq	8(%rsp), %r12           # 8-byte Reload
	jmp	.LBB13_34
.LBB13_16:
	xorq	%rbx, %rbx
	jmp	.LBB13_34
.LBB13_20:                              # %if.end.i
	movq	32(%rax), %rsi
	movzbl	(%rsi), %eax
	cmpq	$39, %rax
	jne	.LBB13_21
# BB#22:                                # %if.then48.i
	movq	8(%rsi), %r8
	movslq	(%r8), %rsi
	testq	%rsi, %rsi
	jle	.LBB13_23
# BB#24:                                # %for.body.lr.ph.i
	leaq	-1(%rsi), %rax
.LBB13_25:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r8,%rax,8), %rcx
	movzbl	(%rcx), %edi
	cmpq	$47, %rdi
	jne	.LBB13_26
# BB#28:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB13_25 Depth=1
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$50, %rcx
	je	.LBB13_29
.LBB13_26:                              # %for.cond.backedge.i
                                        #   in Loop: Header=BB13_25 Depth=1
	decq	%rsi
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	decq	%rax
	testq	%rdi, %rdi
	jg	.LBB13_25
# BB#27:
	xorq	%rbx, %rbx
	jmp	.LBB13_34
.LBB13_21:
	movq	%rdx, %rbx
.LBB13_31:                              # %if.end87.i
	movq	$1, %rdx
	movq	%r15, %rdi
	callq	insn_dead_p
	movq	%rbx, %rdx
	testq	%rdx, %rax
	je	.LBB13_32
# BB#33:                                # %if.end87.i
	movabsq	$1, %rbx
	jmp	.LBB13_34
.LBB13_32:
	movabsq	$0, %rbx
	jmp	.LBB13_34
.LBB13_23:
	xorq	%rbx, %rbx
	jmp	.LBB13_34
.LBB13_29:                              # %for.end.i
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	movabsq	$0, %rbx
	jle	.LBB13_34
# BB#30:                                # %if.end80.i
	movq	%rdx, %rbx
	movq	8(%r8,%rax,8), %rsi
	jmp	.LBB13_31
.LBB13_34:                              # %if.end17
	movl	64(%r15), %eax
	testq	$8, %rax
	je	.LBB13_60
# BB#35:                                # %if.end17
	testq	%rdx, %r13
	je	.LBB13_60
# BB#36:                                # %if.then22
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB13_42
# BB#37:                                # %land.lhs.true24
	movq	current_function_decl(%rip), %rax
	movq	8(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$23, %rcx
	jne	.LBB13_39
# BB#38:                                # %land.lhs.true30
	movzbl	62(%rax), %eax
	testq	$2, %rax
	jne	.LBB13_42
.LBB13_39:                              # %land.lhs.true37
	movq	%r14, %rdi
	callq	prologue_epilogue_contains
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB13_41
# BB#40:                                # %lor.lhs.false
	movq	%r14, %rdi
	callq	sibcall_epilogue_contains
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_42
.LBB13_41:                              # %land.lhs.true42
	movq	$25, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB13_127
.LBB13_42:                              # %if.end47
	movq	32(%r14), %rsi
	movq	%r15, %rdi
	movq	%r14, %rdx
	callq	mark_set_regs
	xorq	%rax, %rax
	movl	%eax, 60(%r15)
	testq	%rbx, %rbx
	je	.LBB13_44
# BB#43:                                # %if.then52
	movq	8(%r12), %rdi
	movq	16(%rdi), %r13
	movq	%r14, %rsi
	callq	delete_insn_chain
	jmp	.LBB13_59
.LBB13_60:                              # %if.end55
	movq	16(%r15), %rdi
	callq	bitmap_clear
	movq	32(%r14), %rsi
	testq	%rbx, %rbx
	je	.LBB13_62
# BB#61:                                # %if.then57
	movq	%r15, %rdi
	movq	%r14, %rdx
	callq	mark_set_regs
	movq	8(%r12), %rax
	movq	16(%rax), %r13
	jmp	.LBB13_59
.LBB13_62:                              # %if.else67
	movzbl	(%rsi), %eax
	cmpq	$47, %rax
	jne	.LBB13_68
# BB#63:                                # %land.lhs.true75
	movq	8(%rsi), %rax
	movq	global_rtl+16(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB13_68
# BB#64:                                # %land.lhs.true84
	movq	16(%rsi), %rcx
	movzbl	(%rcx), %edx
	cmpq	$75, %rdx
	jne	.LBB13_68
# BB#65:                                # %land.lhs.true95
	movq	8(%rcx), %rdx
	cmpq	%rax, %rdx
	jne	.LBB13_68
# BB#66:                                # %land.lhs.true107
	movq	16(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB13_68
# BB#67:                                # %land.lhs.true107.if.end352_crit_edge
	movl	64(%r15), %eax
	andq	$4, %rax
	movq	%rax, %r12
	movq	16(%rsp), %r13          # 8-byte Reload
	jmp	.LBB13_115
.LBB13_68:                              # %if.else122
	movzbl	(%r14), %eax
	movl	64(%r15), %ecx
	movq	%rcx, %r12
	andq	$4, %r12
	cmpq	$34, %rax
	jne	.LBB13_81
# BB#69:                                # %if.else122
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %r12
	je	.LBB13_81
# BB#70:                                # %do.body
	movq	8(%r15), %rax
	movq	(%rax), %r10
	testq	%r10, %r10
	je	.LBB13_81
# BB#71:                                # %for.cond149.preheader.lr.ph
	movq	reg_n_info(%rip), %r9
	.align	16, 0x90
.LBB13_72:                              # %for.cond149.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_73 Depth 2
                                        #     Child Loop BB13_77 Depth 2
	movq	24(%r10), %r11
	testq	%r11, %r11
	movq	$1, %rbx
	je	.LBB13_76
	.align	16, 0x90
.LBB13_73:                              # %for.body161
                                        #   Parent Loop BB13_72 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%rbx), %rax
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	testq	%r11, %rdi
	je	.LBB13_75
# BB#74:                                # %if.then165
                                        #   in Loop: Header=BB13_73 Depth=2
	movl	16(%r10), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	addq	%rdx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%r9,%rax,8), %rax
	movl	32(%rax), %ecx
	notq	%rdi
	incq	%rcx
	andq	%rdi, %r11
	movl	%ecx, 32(%rax)
	je	.LBB13_76
.LBB13_75:                              # %for.inc
                                        #   in Loop: Header=BB13_73 Depth=2
	movq	%rbx, %rax
	andq	%r8, %rax
	incq	%rbx
	cmpq	$64, %rax
	jb	.LBB13_73
.LBB13_76:                              # %if.end178
                                        #   in Loop: Header=BB13_72 Depth=1
	movq	32(%r10), %rdx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	je	.LBB13_80
	.align	16, 0x90
.LBB13_77:                              # %for.body161.1
                                        #   Parent Loop BB13_72 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	testq	%rdx, %rdi
	je	.LBB13_79
# BB#78:                                # %if.then165.1
                                        #   in Loop: Header=BB13_77 Depth=2
	movl	16(%r10), %ebx
	movq	$7, %rcx
	shlq	%cl, %rbx
	leaq	64(%rax,%rbx), %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	32(%r9,%rbx,8), %rcx
	movl	32(%rcx), %ebx
	notq	%rdi
	incq	%rbx
	andq	%rdi, %rdx
	movl	%ebx, 32(%rcx)
	je	.LBB13_80
.LBB13_79:                              # %for.inc.1
                                        #   in Loop: Header=BB13_77 Depth=2
	incq	%rax
	movq	%rax, %rcx
	andq	%r8, %rcx
	cmpq	$64, %rcx
	jb	.LBB13_77
.LBB13_80:                              # %if.end178.1
                                        #   in Loop: Header=BB13_72 Depth=1
	movq	(%r10), %r10
	testq	%r10, %r10
	jne	.LBB13_72
.LBB13_81:                              # %if.end185
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rdi
	movq	%r14, %rdx
	callq	mark_set_regs
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$34, %rcx
	jne	.LBB13_94
# BB#82:                                # %if.then193
	movq	32(%r14), %rcx
	movzbl	(%rcx), %edx
	xorq	%r12, %r12
	cmpq	$38, %rdx
	jne	.LBB13_84
# BB#83:                                # %if.then203
	movq	8(%rcx), %r12
.LBB13_84:                              # %if.end210
	testq	$67108864, %rax         # imm = 0x4000000
	jne	.LBB13_86
# BB#85:                                # %if.then215
	leaq	32(%r15), %rdi
	callq	free_EXPR_LIST_list
	xorq	%rax, %rax
	movl	%eax, 56(%r15)
.LBB13_86:                              # %if.end216
	movq	64(%r14), %r13
	xorq	%rbx, %rbx
	jmp	.LBB13_87
.LBB13_90:                              # %for.inc239
                                        #   in Loop: Header=BB13_87 Depth=1
	movq	16(%r13), %r13
.LBB13_87:                              # %if.end216
                                        # =>This Inner Loop Header: Depth=1
	testq	%r13, %r13
	je	.LBB13_91
# BB#88:                                # %for.body222
                                        #   in Loop: Header=BB13_87 Depth=1
	movq	8(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$49, %rcx
	jne	.LBB13_90
# BB#89:                                # %if.then230
                                        #   in Loop: Header=BB13_87 Depth=1
	movq	8(%rax), %rdx
	movl	64(%r15), %r9d
	movq	$49, %rsi
	movq	%r15, %rdi
	movq	%r12, %rcx
	movq	%r14, %r8
	callq	mark_set_1
	jmp	.LBB13_90
	.align	16, 0x90
.LBB13_91:                              # %for.body247
                                        # =>This Inner Loop Header: Depth=1
	movq	regs_invalidated_by_call(%rip), %rax
	movq	$1, %rdx
	movq	%rbx, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB13_93
# BB#92:                                # %if.then252
                                        #   in Loop: Header=BB13_91 Depth=1
	movl	reg_raw_mode(,%rbx,4), %edi
	movq	%rbx, %rsi
	callq	gen_rtx_REG
	movl	64(%r15), %r9d
	andq	$-6, %r9
	movq	$49, %rsi
	movq	%r15, %rdi
	movq	%rax, %rdx
	movq	%r12, %rcx
	movq	%r14, %r8
	callq	mark_set_1
.LBB13_93:                              # %for.inc259
                                        #   in Loop: Header=BB13_91 Depth=1
	incq	%rbx
	cmpq	$53, %rbx
	jne	.LBB13_91
.LBB13_94:                              # %if.end262
	xorq	%rax, %rax
	movl	%eax, 60(%r15)
	movq	24(%rsp), %rbx          # 8-byte Reload
	testq	%rbx, %rbx
	movq	16(%rsp), %r13          # 8-byte Reload
	jne	.LBB13_96
# BB#95:                                # %if.then265
	movq	32(%r14), %rsi
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%r14, %rcx
	callq	mark_used_regs
.LBB13_96:                              # %if.end269
	movl	64(%r15), %eax
	testq	$128, %rax
	je	.LBB13_100
# BB#97:                                # %land.lhs.true272
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB13_99
# BB#98:                                # %lor.lhs.false275
	movq	$3, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB13_100
.LBB13_99:                              # %if.then278
	movq	8(%rax), %rsi
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%r14, %rcx
	callq	mark_used_regs
.LBB13_100:                             # %if.end282
	testq	%rbx, %rbx
	je	.LBB13_102
# BB#101:
	movq	8(%rsp), %r12           # 8-byte Reload
	jmp	.LBB13_115
.LBB13_44:                              # %if.else
	movq	(%r15), %r15
	movq	$13, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	movq	16(%rsp), %r13          # 8-byte Reload
	je	.LBB13_57
# BB#45:                                # %land.lhs.true.i584
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB13_57
# BB#46:                                # %if.then.i591
	movq	8(%rax), %rdi
	movl	(%rdi), %eax
	movl	32(%rdi), %edx
	movq	$28, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	incq	%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rax, %rdx
	jne	.LBB13_57
# BB#47:                                # %land.lhs.true6.i
	callq	next_nonnote_insn
	testq	%rax, %rax
	je	.LBB13_57
# BB#48:                                # %land.lhs.true9.i
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB13_57
# BB#49:                                # %land.lhs.true13.i
	movq	32(%rax), %rcx
	movl	(%rcx), %edx
	movq	%rdx, %rsi
	andq	$65534, %rsi            # imm = 0xFFFE
	cmpq	$44, %rsi
	jne	.LBB13_57
# BB#50:                                # %if.then26.i
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$45, %rdx
	jne	.LBB13_51
# BB#52:                                # %if.then26.i
	movabsq	$1, %rdx
	jmp	.LBB13_53
.LBB13_102:                             # %land.lhs.true284
	movzbl	(%r14), %eax
	cmpq	$34, %rax
	jne	.LBB13_103
# BB#104:                               # %if.then289
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	xorq	%r12, %r12
	cmpq	$38, %rcx
	jne	.LBB13_106
# BB#105:                               # %if.then300
	movq	8(%rax), %r12
.LBB13_106:                             # %if.end307
	movq	64(%r14), %rbx
	jmp	.LBB13_107
.LBB13_110:                             # %for.inc329
                                        #   in Loop: Header=BB13_107 Depth=1
	movq	16(%rbx), %rbx
.LBB13_107:                             # %if.end307
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB13_111
# BB#108:                               # %for.body313
                                        #   in Loop: Header=BB13_107 Depth=1
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$48, %rcx
	jne	.LBB13_110
# BB#109:                               # %if.then321
                                        #   in Loop: Header=BB13_107 Depth=1
	movq	8(%rax), %rsi
	movq	%r15, %rdi
	movq	%r12, %rdx
	movq	%r14, %rcx
	callq	mark_used_regs
	jmp	.LBB13_110
.LBB13_103:
	movq	8(%rsp), %r12           # 8-byte Reload
	jmp	.LBB13_115
.LBB13_111:                             # %for.end333
	movq	8(%r15), %rdi
	movq	$7, %rsi
	callq	bitmap_set_bit
	xorq	%rbx, %rbx
	movq	8(%rsp), %r12           # 8-byte Reload
	.align	16, 0x90
.LBB13_112:                             # %for.body338
                                        # =>This Inner Loop Header: Depth=1
	movzbl	global_regs(%rbx), %eax
	testq	%rax, %rax
	je	.LBB13_114
# BB#113:                               # %if.then342
                                        #   in Loop: Header=BB13_112 Depth=1
	movl	reg_raw_mode(,%rbx,4), %edi
	movq	%rbx, %rsi
	callq	gen_rtx_REG
	movq	%r15, %rdi
	movq	%rax, %rsi
	movq	%r14, %rdx
	callq	mark_used_reg
.LBB13_114:                             # %for.inc347
                                        #   in Loop: Header=BB13_112 Depth=1
	incq	%rbx
	cmpq	$53, %rbx
	jne	.LBB13_112
.LBB13_115:                             # %if.end352
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %r12
	je	.LBB13_59
# BB#116:                               # %do.body356
	movq	8(%r15), %rax
	movq	(%rax), %r10
	testq	%r10, %r10
	je	.LBB13_59
# BB#117:                               # %for.cond387.preheader.lr.ph
	movq	reg_n_info(%rip), %r9
	.align	16, 0x90
.LBB13_118:                             # %for.cond387.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_119 Depth 2
                                        #     Child Loop BB13_123 Depth 2
	movq	24(%r10), %rbx
	testq	%rbx, %rbx
	movq	$1, %rdx
	je	.LBB13_122
	.align	16, 0x90
.LBB13_119:                             # %for.body401
                                        #   Parent Loop BB13_118 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%rdx), %rax
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	testq	%rbx, %rdi
	je	.LBB13_121
# BB#120:                               # %if.then408
                                        #   in Loop: Header=BB13_119 Depth=2
	movl	16(%r10), %esi
	movq	$7, %rcx
	shlq	%cl, %rsi
	addq	%rsi, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%r9,%rax,8), %rax
	movl	28(%rax), %ecx
	notq	%rdi
	incq	%rcx
	andq	%rdi, %rbx
	movl	%ecx, 28(%rax)
	je	.LBB13_122
.LBB13_121:                             # %for.inc426
                                        #   in Loop: Header=BB13_119 Depth=2
	movq	%rdx, %rax
	andq	%r8, %rax
	incq	%rdx
	cmpq	$64, %rax
	jb	.LBB13_119
.LBB13_122:                             # %if.end429
                                        #   in Loop: Header=BB13_118 Depth=1
	movq	32(%r10), %rbx
	xorq	%rax, %rax
	testq	%rbx, %rbx
	je	.LBB13_126
	.align	16, 0x90
.LBB13_123:                             # %for.body401.1
                                        #   Parent Loop BB13_118 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	%rbx, %rdx
	je	.LBB13_125
# BB#124:                               # %if.then408.1
                                        #   in Loop: Header=BB13_123 Depth=2
	movl	16(%r10), %esi
	movq	$7, %rcx
	shlq	%cl, %rsi
	leaq	64(%rax,%rsi), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	32(%r9,%rsi,8), %rcx
	movl	28(%rcx), %esi
	notq	%rdx
	incq	%rsi
	andq	%rdx, %rbx
	movl	%esi, 28(%rcx)
	je	.LBB13_126
.LBB13_125:                             # %for.inc426.1
                                        #   in Loop: Header=BB13_123 Depth=2
	incq	%rax
	movq	%rax, %rcx
	andq	%r8, %rcx
	cmpq	$64, %rcx
	jb	.LBB13_123
.LBB13_126:                             # %if.end429.1
                                        #   in Loop: Header=BB13_118 Depth=1
	movq	(%r10), %r10
	testq	%r10, %r10
	jne	.LBB13_118
	jmp	.LBB13_59
.LBB13_51:
	movabsq	$0, %rdx
.LBB13_53:                              # %if.then26.i
	movq	8(%rcx,%rdx,8), %r8
	movslq	(%r8), %rdx
	testq	%rdx, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	jle	.LBB13_56
# BB#54:
	movq	$1, %rsi
	andq	%rcx, %rdx
.LBB13_55:                              # %for.body.i597
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r8,%rsi,8), %rdi
	movq	8(%rdi), %rdi
	movl	32(%rdi), %ebx
	decq	%rbx
	movl	%ebx, 32(%rdi)
	movq	%rsi, %rdi
	andq	%rcx, %rdi
	incq	%rsi
	cmpq	%rdx, %rdi
	jne	.LBB13_55
.LBB13_56:                              # %for.end.i598
	movq	%rax, %rdi
	callq	delete_insn
.LBB13_57:                              # %if.end50.i
	movq	8(%r15), %rbx
	movq	%r14, %rdi
	callq	delete_insn
	cmpq	%r14, %rbx
	jne	.LBB13_59
# BB#58:                                # %if.then57.i
	movq	%r15, %rdi
	callq	purge_dead_edges
.LBB13_59:                              # %return
	movq	%r13, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB13_127:                             # %if.then46
	movabsq	$.L.str4, %rax
	movabsq	$.L.str3, %rdx
	movabsq	$.L__FUNCTION__.propagate_one_insn, %rbx
	movq	$1615, %rcx             # imm = 0x64F
	movq	%rax, %rdi
	movq	%r14, %rsi
	movq	%rbx, %r8
	callq	_fatal_insn
.Ltmp172:
	.size	propagate_one_insn, .Ltmp172-propagate_one_insn
	.cfi_endproc

	.align	16, 0x90
	.type	insn_dead_p,@function
insn_dead_p:                            # @insn_dead_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp179:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp180:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp181:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp182:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp183:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp184:
	.cfi_def_cfa_offset 64
.Ltmp185:
	.cfi_offset %rbx, -48
.Ltmp186:
	.cfi_offset %r12, -40
.Ltmp187:
	.cfi_offset %r13, -32
.Ltmp188:
	.cfi_offset %r14, -24
.Ltmp189:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movzbl	(%r12), %eax
	movq	%rdx, %r15
	movq	%rdi, %rdx
	cmpq	$39, %rax
	je	.LBB14_48
# BB#1:                                 # %entry
	cmpq	$49, %rax
	jne	.LBB14_2
# BB#56:                                # %land.lhs.true272
	movq	8(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB14_59
# BB#57:                                # %land.lhs.true280
	movl	8(%rax), %esi
	cmpq	$53, %rsi
	jb	.LBB14_59
# BB#58:                                # %land.lhs.true289
	movq	8(%rdx), %rdi
	callq	bitmap_bit_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %r13
	jne	.LBB14_59
	jmp	.LBB14_60
.LBB14_48:                              # %if.then227
	movq	8(%r12), %rax
	movslq	(%rax), %rbx
	testq	%rbx, %rbx
	jle	.LBB14_49
# BB#50:                                # %for.body234.lr.ph
	decq	%rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jmp	.LBB14_51
	.align	16, 0x90
.LBB14_55:                              # %for.cond231.backedge.for.body234_crit_edge
                                        #   in Loop: Header=BB14_51 Depth=1
	movq	8(%r12), %rax
	decq	%rbx
.LBB14_51:                              # %for.body234
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax,%rbx,8), %rsi
	movl	(%rsi), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$48, %rax
	je	.LBB14_53
# BB#52:                                # %land.lhs.true255
                                        #   in Loop: Header=BB14_51 Depth=1
	movq	%rdx, %rdi
	movq	%rdx, %r13
	movq	%r15, %rdx
	callq	insn_dead_p
	movq	%r13, %rdx
	xorq	%r13, %r13
	testq	%r14, %rax
	je	.LBB14_60
.LBB14_53:                              # %for.cond231.backedge
                                        #   in Loop: Header=BB14_51 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB14_55
# BB#54:
	movq	$1, %r13
	jmp	.LBB14_60
.LBB14_2:                               # %entry
	cmpq	$47, %rax
	jne	.LBB14_59
# BB#3:                                 # %if.then
	movq	16(%r12), %rdi
	movzbl	(%rdi), %eax
	movq	8(%r12), %r12
	cmpq	$50, %rax
	jne	.LBB14_5
# BB#4:                                 # %if.then7
	xorq	%r13, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	jne	.LBB14_6
	jmp	.LBB14_60
.LBB14_49:
	movq	$1, %r13
	jmp	.LBB14_60
.LBB14_5:                               # %if.else
	movq	%rdx, %rbx
	callq	volatile_refs_p
	movq	%rbx, %rdx
	xorq	%r13, %r13
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB14_60
.LBB14_6:                               # %if.end15
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB14_7
# BB#12:                                # %if.then19
	xorq	%r13, %r13
	testq	$134217728, %rax        # imm = 0x8000000
	jne	.LBB14_60
# BB#13:                                # %if.then19
	andq	$16711680, %rax         # imm = 0xFF0000
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	cmpq	$3342336, %rax          # imm = 0x330000
	je	.LBB14_60
# BB#14:                                # %if.end28
	movq	%r12, %rdi
	movq	%rdx, %rbx
	callq	canon_rtx
	movq	32(%rbx), %rbx
	movq	%rax, %r14
	jmp	.LBB14_15
.LBB14_7:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB14_8
	.align	16, 0x90
.LBB14_11:                              # %while.body
                                        #   in Loop: Header=BB14_8 Depth=1
	movq	8(%r12), %r12
	movl	(%r12), %eax
.LBB14_8:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-63(%rax), %rcx
	andq	%r15, %rcx
	cmpq	$2, %rcx
	jb	.LBB14_11
# BB#9:                                 # %while.cond
                                        #   in Loop: Header=BB14_8 Depth=1
	cmpq	$61, %rax
	je	.LBB14_20
# BB#10:                                # %while.cond
                                        #   in Loop: Header=BB14_8 Depth=1
	cmpq	$133, %rax
	je	.LBB14_11
	jmp	.LBB14_59
.LBB14_19:                              # %for.inc
                                        #   in Loop: Header=BB14_15 Depth=1
	movq	16(%rbx), %rbx
.LBB14_15:                              # %if.end28
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB14_59
# BB#16:                                # %for.body
                                        #   in Loop: Header=BB14_15 Depth=1
	movq	8(%rbx), %rsi
	movq	%r12, %rdi
	callq	anti_dependence
	testq	%r15, %rax
	je	.LBB14_19
# BB#17:                                # %if.then36
                                        #   in Loop: Header=BB14_15 Depth=1
	movq	8(%rbx), %r13
	movq	8(%r14), %rdi
	movq	8(%r13), %rsi
	callq	rtx_equal_p
	testq	%r15, %rax
	je	.LBB14_19
# BB#18:                                # %land.lhs.true
                                        #   in Loop: Header=BB14_15 Depth=1
	movzbl	2(%r14), %eax
	movzbl	2(%r13), %ecx
	movzbl	mode_size(%rax), %eax
	movzbl	mode_size(%rcx), %ecx
	cmpq	%rcx, %rax
	movq	$1, %r13
	ja	.LBB14_19
	jmp	.LBB14_60
.LBB14_59:                              # %if.end302
	xorq	%r13, %r13
.LBB14_60:                              # %return
	movq	%r13, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB14_20:                              # %if.then87
	movslq	8(%r12), %r14
	movq	8(%rdx), %rdi
	movq	%r14, %rsi
	movq	%rdx, %rbx
	callq	bitmap_bit_p
	xorq	%r13, %r13
	testq	%r15, %rax
	jne	.LBB14_60
# BB#21:                                # %if.end93
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	cmpq	$52, %r14
	jg	.LBB14_39
# BB#22:                                # %if.then96
	movzbl	2(%r12), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%r14, %rcx
	cmpq	$8, %rcx
	je	.LBB14_26
# BB#23:                                # %if.then96
	leaq	-21(%r14), %rcx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	jb	.LBB14_26
# BB#24:                                # %if.then96
	leaq	-45(%r14), %rcx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	jb	.LBB14_26
# BB#25:                                # %if.then96
	leaq	-29(%r14), %rcx
	andq	%r15, %rcx
	cmpq	$7, %rcx
	jbe	.LBB14_26
# BB#31:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB14_32
# BB#34:                                # %cond.true150
	movzbl	target_flags+3(%rip), %ebx
	andq	$2, %rbx
	xorq	$6, %rbx
	jmp	.LBB14_36
.LBB14_26:                              # %cond.true
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %rbx
	je	.LBB14_36
# BB#27:                                # %lor.rhs127
	cmpq	$6, %rax
	jne	.LBB14_28
# BB#29:                                # %lor.rhs127
	movabsq	$1, %rbx
	incq	%rbx
	jmp	.LBB14_36
.LBB14_32:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB14_35
# BB#33:                                # %cond.true141
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	xorq	$3, %rbx
	jmp	.LBB14_36
.LBB14_28:
	movabsq	$0, %rbx
	incq	%rbx
	jmp	.LBB14_36
.LBB14_35:                              # %cond.false154
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %rbx
.LBB14_36:                              # %while.cond172
                                        # =>This Inner Loop Header: Depth=1
	decq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB14_38
# BB#37:                                # %while.body175
                                        #   in Loop: Header=BB14_36 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	8(%rax), %rdi
	leaq	(%rbx,%r14), %rsi
	callq	bitmap_bit_p
	testq	%r15, %rax
	je	.LBB14_36
	jmp	.LBB14_60
.LBB14_38:                              # %land.lhs.true186
	movzbl	global_regs(%r14), %eax
	testq	%rax, %rax
	jne	.LBB14_60
.LBB14_39:                              # %if.end192
	cmpq	$15, %r14
	jg	.LBB14_42
# BB#40:                                # %if.end192
	andq	%r15, %r14
	cmpq	$6, %r14
	je	.LBB14_44
# BB#41:                                # %if.end192
	cmpq	$7, %r14
	jne	.LBB14_47
	jmp	.LBB14_60
.LBB14_42:                              # %if.end192
	andq	%r15, %r14
	cmpq	$16, %r14
	jne	.LBB14_43
# BB#46:                                # %land.lhs.true215
	movzbl	fixed_regs+16(%rip), %eax
	testq	%rax, %rax
	jne	.LBB14_60
	jmp	.LBB14_47
.LBB14_43:                              # %if.end192
	cmpq	$20, %r14
	jne	.LBB14_47
.LBB14_44:                              # %land.lhs.true199
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB14_60
# BB#45:                                # %land.lhs.true199
	movl	frame_pointer_needed(%rip), %eax
	testq	%r15, %rax
	jne	.LBB14_60
.LBB14_47:                              # %if.end221
	movq	$1, %r13
	jmp	.LBB14_60
.Ltmp190:
	.size	insn_dead_p, .Ltmp190-insn_dead_p
	.cfi_endproc

	.align	16, 0x90
	.type	mark_set_regs,@function
mark_set_regs:                          # @mark_set_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp197:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp198:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp199:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp200:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp201:
	.cfi_def_cfa_offset 48
.Ltmp202:
	.cfi_offset %rbx, -48
.Ltmp203:
	.cfi_offset %r12, -40
.Ltmp204:
	.cfi_offset %r13, -32
.Ltmp205:
	.cfi_offset %r14, -24
.Ltmp206:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r13
	movq	%rdi, %r15
	xorq	%r12, %r12
	testq	%r14, %r14
	je	.LBB15_9
# BB#1:                                 # %if.then
	movq	56(%r14), %rbx
	jmp	.LBB15_2
	.align	16, 0x90
.LBB15_7:                               # %for.inc
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	16(%rbx), %rbx
.LBB15_2:                               # %if.then
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB15_9
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB15_2 Depth=1
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$131072, %rax           # imm = 0x20000
	jne	.LBB15_7
# BB#4:                                 # %if.then2
                                        #   in Loop: Header=BB15_2 Depth=1
	movzbl	(%r13), %eax
	movq	8(%rbx), %rdx
	xorq	%rcx, %rcx
	cmpq	$38, %rax
	jne	.LBB15_6
# BB#5:                                 # %cond.true
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	8(%r13), %rcx
.LBB15_6:                               # %cond.end
                                        #   in Loop: Header=BB15_2 Depth=1
	movl	64(%r15), %r9d
	movq	$47, %rsi
	movq	%r15, %rdi
	movq	%r14, %r8
	callq	mark_set_1
	jmp	.LBB15_7
	.align	16, 0x90
.LBB15_8:                               # %sw.bb23
                                        #   in Loop: Header=BB15_9 Depth=1
	movq	8(%r13), %r12
	movq	16(%r13), %r13
.LBB15_9:                               # %retry
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r13), %esi
	cmpq	$46, %rsi
	jg	.LBB15_20
# BB#10:                                # %retry
                                        #   in Loop: Header=BB15_9 Depth=1
	cmpq	$38, %rsi
	je	.LBB15_8
# BB#11:                                # %retry
	cmpq	$39, %rsi
	jne	.LBB15_26
# BB#12:                                # %sw.bb30
	movq	8(%r13), %rax
	movl	(%rax), %ebx
	jmp	.LBB15_13
.LBB15_20:                              # %retry
	cmpq	$47, %rsi
	je	.LBB15_22
# BB#21:                                # %retry
	cmpq	$49, %rsi
	jne	.LBB15_26
.LBB15_22:                              # %sw.bb
	movq	8(%r13), %rdx
	movl	64(%r15), %r9d
	movq	%r15, %rdi
	movq	%r12, %rcx
	movq	%r14, %r8
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	mark_set_1  # TAILCALL
.LBB15_25:                              # %sw.bb61
                                        #   in Loop: Header=BB15_13 Depth=1
	movq	8(%rax), %rdx
	movl	64(%r15), %r9d
	movq	%r15, %rdi
	movq	%r12, %rcx
	movq	%r14, %r8
	callq	mark_set_1
.LBB15_13:                              # %for.cond33.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB15_14 Depth 2
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
.LBB15_14:                              # %for.cond33
                                        #   Parent Loop BB15_13 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB15_26
# BB#15:                                # %for.body35
                                        #   in Loop: Header=BB15_14 Depth=2
	movq	8(%r13), %rax
	movq	(%rax,%rbx,8), %rax
	movzbl	(%rax), %esi
	decq	%rbx
	cmpq	$49, %rsi
	je	.LBB15_25
# BB#16:                                # %for.body35
                                        #   in Loop: Header=BB15_14 Depth=2
	cmpq	$47, %rsi
	je	.LBB15_25
# BB#17:                                # %for.body35
                                        #   in Loop: Header=BB15_14 Depth=2
	cmpq	$38, %rsi
	jne	.LBB15_14
# BB#18:                                # %sw.bb43
                                        #   in Loop: Header=BB15_13 Depth=1
	testq	%r12, %r12
	jne	.LBB15_19
# BB#23:                                # %if.end46
                                        #   in Loop: Header=BB15_13 Depth=1
	movq	8(%rax), %r12
	movq	16(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	movq	$38, %rsi
	je	.LBB15_25
# BB#24:                                # %if.end46
                                        #   in Loop: Header=BB15_13 Depth=1
	cmpq	$49, %rcx
	jne	.LBB15_13
	jmp	.LBB15_25
.LBB15_26:                              # %sw.epilog69
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB15_19:                              # %if.then45
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.mark_set_regs, %rcx
	movq	$2468, %rsi             # imm = 0x9A4
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp207:
	.size	mark_set_regs, .Ltmp207-mark_set_regs
	.cfi_endproc

	.align	16, 0x90
	.type	mark_set_1,@function
mark_set_1:                             # @mark_set_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp214:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp215:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp216:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp217:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp218:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp219:
	.cfi_def_cfa_offset 144
.Ltmp220:
	.cfi_offset %rbx, -48
.Ltmp221:
	.cfi_offset %r12, -40
.Ltmp222:
	.cfi_offset %r13, -32
.Ltmp223:
	.cfi_offset %r14, -24
.Ltmp224:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movl	(%r15), %ebx
	movq	%r8, 48(%rsp)           # 8-byte Spill
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%rdi, %r12
	movq	%rbx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	cmpq	$131, %rax
	jg	.LBB16_17
# BB#1:                                 # %entry
	cmpq	$60, %rax
	jg	.LBB16_7
# BB#2:                                 # %entry
	movq	%r9, 64(%rsp)           # 8-byte Spill
	cmpq	$39, %rax
	jne	.LBB16_56
# BB#3:                                 # %sw.bb
	movq	8(%r15), %rax
	movslq	(%rax), %rbx
	testq	%rbx, %rbx
	jg	.LBB16_4
	jmp	.LBB16_174
	.align	16, 0x90
.LBB16_6:                               # %for.cond.backedge.for.body_crit_edge
                                        #   in Loop: Header=BB16_4 Depth=1
	movq	8(%r15), %rax
.LBB16_4:                               # %for.body.lr.ph
                                        # =>This Inner Loop Header: Depth=1
	decq	%rbx
	movq	8(%rax,%rbx,8), %rax
	movq	8(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB16_5
# BB#19:                                # %if.then
                                        #   in Loop: Header=BB16_4 Depth=1
	movq	%r12, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	64(%rsp), %r9           # 8-byte Reload
	callq	mark_set_1
.LBB16_5:                               # %for.cond.backedge
                                        #   in Loop: Header=BB16_4 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB16_6
	jmp	.LBB16_174
.LBB16_17:                              # %entry
	addq	$-132, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$2, %rcx
	jb	.LBB16_11
	jmp	.LBB16_18
.LBB16_7:                               # %entry
	cmpq	$61, %rax
	je	.LBB16_8
# BB#9:                                 # %entry
	cmpq	$63, %rax
	jne	.LBB16_10
# BB#35:                                # %sw.bb125
	movq	8(%r15), %r8
	movl	(%r8), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB16_36
# BB#37:                                # %if.then133
	movq	%r9, 64(%rsp)           # 8-byte Spill
	movq	$16, %rcx
	shrq	%cl, %rbx
	movq	$16, %rcx
	movslq	8(%r8), %r14
	shrq	%cl, %rsi
	andq	$255, %rbx
	andq	$255, %rsi
	cmpq	$52, %r14
	jg	.LBB16_53
# BB#38:                                # %if.then151
	movl	16(%r15), %edx
	movq	%r14, %rdi
	movq	%rbx, %rcx
	callq	subreg_regno_offset
	leaq	(%rax,%r14), %rdi
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rdi, %rcx
	cmpq	$8, %rcx
	je	.LBB16_42
# BB#39:                                # %if.then151
	leaq	-21(%rax,%r14), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB16_42
# BB#40:                                # %if.then151
	leaq	-45(%rax,%r14), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB16_42
# BB#41:                                # %if.then151
	leaq	-29(%rax,%r14), %rax
	andq	%rcx, %rax
	cmpq	$7, %rax
	jbe	.LBB16_42
# BB#47:                                # %cond.false192
	cmpq	$24, %rbx
	jne	.LBB16_48
# BB#50:                                # %cond.true202
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB16_52
.LBB16_8:
	movq	%r9, 64(%rsp)           # 8-byte Spill
	jmp	.LBB16_15
.LBB16_10:                              # %entry
	cmpq	$64, %rax
	jne	.LBB16_18
	.align	16, 0x90
.LBB16_11:                              # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r15), %r15
	movzbl	(%r15), %eax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	leaq	-63(%rax), %rcx
	andq	%r14, %rcx
	cmpq	$2, %rcx
	jb	.LBB16_11
# BB#12:                                # %do.body
                                        #   in Loop: Header=BB16_11 Depth=1
	leaq	-132(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$2, %rdx
	jb	.LBB16_11
# BB#13:                                # %do.body
	cmpq	$66, %rax
	jne	.LBB16_14
.LBB16_18:
	movq	%r9, 64(%rsp)           # 8-byte Spill
.LBB16_56:                              # %sw.epilog
	movq	%r14, 56(%rsp)          # 8-byte Spill
	jmp	.LBB16_57
.LBB16_14:                              # %if.end38
	movq	%r9, 64(%rsp)           # 8-byte Spill
	movq	8(%r12), %rdi
	movl	8(%r15), %esi
	callq	bitmap_bit_p
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
.LBB16_15:                              # %sw.bb41
	movslq	8(%r15), %rdi
	cmpq	$52, %rdi
	jg	.LBB16_16
# BB#20:                                # %if.then47
	movzbl	2(%r15), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rdi, %rcx
	cmpq	$8, %rcx
	je	.LBB16_24
# BB#21:                                # %if.then47
	leaq	-21(%rdi), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB16_24
# BB#22:                                # %if.then47
	leaq	-45(%rdi), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB16_24
# BB#23:                                # %if.then47
	leaq	-29(%rdi), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB16_24
# BB#29:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB16_30
# BB#32:                                # %cond.true99
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB16_34
.LBB16_16:
	movq	%rdi, %r14
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	jmp	.LBB16_57
.LBB16_24:                              # %cond.true
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB16_34
# BB#25:                                # %lor.rhs76
	cmpq	$6, %rcx
	jne	.LBB16_26
# BB#27:                                # %lor.rhs76
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB16_34
.LBB16_36:
	movq	%r9, 64(%rsp)           # 8-byte Spill
	jmp	.LBB16_54
.LBB16_30:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB16_33
# BB#31:                                # %cond.true91
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB16_34
.LBB16_53:                              # %if.else
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rbx), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	4(%rdi), %r9
	leaq	3(%rax,%rdi), %rax
	cqto
	idivq	%r9
	movq	%rax, %rbx
	movq	$32, %rcx
	movzbl	mode_size(%rsi), %eax
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	3(%rdi,%rax), %rax
	cqto
	idivq	%r9
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rbx
	jge	.LBB16_54
# BB#55:                                # %if.then254
	movq	8(%r12), %rdi
	movq	%r14, %rsi
	callq	bitmap_bit_p
	movq	$32, %rcx
	movq	8(%r15), %r15
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	jmp	.LBB16_56
.LBB16_54:
	movq	%r14, 56(%rsp)          # 8-byte Spill
	movq	%r8, %r15
	jmp	.LBB16_57
.LBB16_26:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB16_34
.LBB16_33:                              # %cond.false103
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB16_34:                              # %cond.end120
	leaq	-1(%rdi,%rax), %r14
	movq	%rdi, 56(%rsp)          # 8-byte Spill
.LBB16_57:                              # %sw.epilog
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB16_81
# BB#58:                                # %sw.epilog
	movq	64(%rsp), %rax          # 8-byte Reload
	andq	$16, %rax
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB16_81
# BB#59:                                # %if.then272
	movzbl	(%r15), %eax
	cmpq	$61, %rax
	jne	.LBB16_61
# BB#60:                                # %if.then277
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	invalidate_mems_from_set
.LBB16_61:                              # %if.end278
	cmpq	$0, 48(%rsp)            # 8-byte Folded Reload
	je	.LBB16_68
# BB#62:                                # %land.lhs.true280
	movzbl	(%r15), %eax
	cmpq	$66, %rax
	jne	.LBB16_68
# BB#63:                                # %if.then285
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	56(%rax), %rbx
	jmp	.LBB16_64
	.align	16, 0x90
.LBB16_67:                              # %for.inc.i834
                                        #   in Loop: Header=BB16_64 Depth=1
	movq	16(%rbx), %rbx
.LBB16_64:                              # %if.then285
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB16_68
# BB#65:                                # %for.body.i830
                                        #   in Loop: Header=BB16_64 Depth=1
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$131072, %rax           # imm = 0x20000
	jne	.LBB16_67
# BB#66:                                # %if.then.i
                                        #   in Loop: Header=BB16_64 Depth=1
	movq	8(%rbx), %rsi
	movq	%r12, %rdi
	callq	invalidate_mems_from_set
	jmp	.LBB16_67
.LBB16_68:                              # %if.end286
	movzbl	(%r15), %eax
	cmpq	$66, %rax
	jne	.LBB16_81
# BB#69:                                # %land.lhs.true291
	movq	%r15, %rdi
	callq	side_effects_p
	testq	%r13, %rax
	jne	.LBB16_81
# BB#70:                                # %land.lhs.true291
	cmpq	$0, 80(%rsp)            # 8-byte Folded Reload
	jne	.LBB16_81
# BB#71:                                # %land.lhs.true296
	movq	global_rtl+16(%rip), %rdi
	movq	%r15, %rsi
	callq	reg_mentioned_p
	testq	%r13, %rax
	jne	.LBB16_81
# BB#72:                                # %if.then299
	movq	%r15, %rdi
	callq	canon_rtx
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movl	(%rax), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$3342336, %rax          # imm = 0x330000
	je	.LBB16_81
# BB#73:                                # %if.end.i
	movq	32(%r12), %rbx
	movq	%r12, 88(%rsp)          # 8-byte Spill
	jmp	.LBB16_74
.LBB16_78:                              # %for.inc.i
                                        #   in Loop: Header=BB16_74 Depth=1
	movq	16(%rbx), %rbx
.LBB16_74:                              # %if.end.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB16_79
# BB#75:                                # %for.body.i
                                        #   in Loop: Header=BB16_74 Depth=1
	movq	8(%rbx), %r12
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdi
	movq	8(%r12), %rsi
	callq	rtx_equal_p
	testq	%r13, %rax
	je	.LBB16_78
# BB#76:                                # %if.then8.i
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %eax
	movzbl	2(%r12), %ecx
	movzbl	mode_size(%rax), %eax
	movzbl	mode_size(%rcx), %ecx
	cmpq	%rcx, %rax
	movq	88(%rsp), %r12          # 8-byte Reload
	jbe	.LBB16_81
# BB#77:                                # %if.then21.i
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%rbx)
	jmp	.LBB16_81
.LBB16_79:                              # %for.end.i
	movq	88(%rsp), %r12          # 8-byte Reload
	movslq	56(%r12), %rax
	cmpq	$99, %rax
	jg	.LBB16_81
# BB#80:                                # %if.then32.i
	movq	32(%r12), %rdx
	xorq	%rdi, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	alloc_EXPR_LIST
	movq	%rax, 32(%r12)
	movl	56(%r12), %eax
	incq	%rax
	movl	%eax, 56(%r12)
.LBB16_81:                              # %if.end302
	movzbl	(%r15), %eax
	cmpq	$62, %rax
	je	.LBB16_172
# BB#82:                                # %if.end302
	cmpq	$61, %rax
	jne	.LBB16_174
# BB#83:                                # %land.lhs.true307
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, %rdx
	andq	%rsi, %rdx
	cmpq	$6, %rdx
	je	.LBB16_89
# BB#84:                                # %land.lhs.true307
	cmpq	$16, %rdx
	jne	.LBB16_85
# BB#91:                                # %land.lhs.true324
	movzbl	fixed_regs+16(%rip), %ecx
	testq	%rcx, %rcx
	movq	$16, %r13
	je	.LBB16_92
# BB#168:                               # %if.else582
	cmpq	$62, %rax
	jne	.LBB16_169
.LBB16_172:                             # %if.then600
	testq	$1, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB16_174
# BB#173:                               # %if.then603
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	56(%rbx), %rdx
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	alloc_EXPR_LIST
	movq	%rax, 56(%rbx)
	jmp	.LBB16_174
.LBB16_89:                              # %land.lhs.true317
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB16_170
# BB#90:                                # %land.lhs.true317
	movl	frame_pointer_needed(%rip), %eax
	movq	$6, %r13
	jmp	.LBB16_88
.LBB16_85:                              # %land.lhs.true307
	cmpq	$20, %rdx
	movq	%rcx, %r13
	jne	.LBB16_92
# BB#86:                                # %land.lhs.true310
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB16_170
# BB#87:                                # %land.lhs.true310
	movl	frame_pointer_needed(%rip), %eax
	movq	$20, %r13
.LBB16_88:                              # %land.lhs.true310
	testq	%rsi, %rax
	jne	.LBB16_170
.LBB16_92:                              # %for.cond330.preheader
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%r12, 88(%rsp)          # 8-byte Spill
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
	xorq	%rbx, %rbx
	cmpq	%rax, %rdx
	movabsq	$0, %rdi
	jg	.LBB16_120
# BB#93:                                # %for.body333.lr.ph
	cmpq	$0, 80(%rsp)            # 8-byte Folded Reload
	je	.LBB16_94
# BB#102:                               # %for.body333.lr.ph.for.body333.lr.ph.split_crit_edge
	movq	32(%rsp), %rax          # 8-byte Reload
	andq	%rsi, %rax
	xorq	%rbx, %rbx
	xorq	%rdi, %rdi
	cmpq	$49, %rax
	jne	.LBB16_111
	.align	16, 0x90
.LBB16_103:                             # %for.body333.us857
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	%rdi, 80(%rsp)          # 8-byte Spill
	movq	88(%rsp), %rbx          # 8-byte Reload
	movq	8(%rbx), %rdi
	movq	%r13, %rsi
	callq	bitmap_bit_p
	cmpq	$0, 40(%rbx)
	movq	%rax, %r12
	je	.LBB16_108
# BB#104:                               # %land.lhs.true340.us864
                                        #   in Loop: Header=BB16_103 Depth=1
	movq	88(%rsp), %rbx          # 8-byte Reload
	movq	48(%rbx), %rdi
	movq	%r13, %rsi
	callq	bitmap_clear_bit
	movq	40(%rbx), %rdi
	movq	%r13, %rsi
	callq	bitmap_bit_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB16_106
# BB#105:                               # %if.then344.us867
                                        #   in Loop: Header=BB16_103 Depth=1
	movq	48(%rbx), %rdi
	jmp	.LBB16_107
.LBB16_106:                             # %if.else346.us868
                                        #   in Loop: Header=BB16_103 Depth=1
	movq	40(%rbx), %rdi
.LBB16_107:                             # %if.end353.us871
                                        #   in Loop: Header=BB16_103 Depth=1
	movq	%r13, %rsi
	callq	bitmap_set_bit
.LBB16_108:                             # %if.end353.us871
                                        #   in Loop: Header=BB16_103 Depth=1
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %r12
	movabsq	$0, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	72(%rsp), %rbx          # 8-byte Reload
	jne	.LBB16_110
# BB#109:                               # %if.end353.us871
                                        #   in Loop: Header=BB16_103 Depth=1
	movabsq	$1, %rax
.LBB16_110:                             # %if.end353.us871
                                        #   in Loop: Header=BB16_103 Depth=1
	orq	%r12, %rdi
	orq	%rax, %rbx
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r13
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB16_103
	jmp	.LBB16_120
.LBB16_169:                             # %if.else582
	cmpq	$61, %rax
	jne	.LBB16_174
.LBB16_170:                             # %if.then587
	testq	$66, 64(%rsp)           # 8-byte Folded Reload
	je	.LBB16_174
# BB#171:                               # %if.then590
	movq	$32, %rcx
	movq	24(%r12), %rax
	movq	56(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$0, (%rax,%rdx,8)
	jmp	.LBB16_174
.LBB16_94:
	xorq	%rbx, %rbx
	andq	%rsi, 32(%rsp)          # 8-byte Folded Spill
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB16_95:                              # %for.body333.us
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	%rdi, 80(%rsp)          # 8-byte Spill
	movq	88(%rsp), %rbx          # 8-byte Reload
	movq	8(%rbx), %rdi
	movq	%r13, %rsi
	callq	bitmap_bit_p
	cmpq	$0, 40(%rbx)
	movq	%rax, %r12
	je	.LBB16_97
# BB#96:                                # %if.else346.us
                                        #   in Loop: Header=BB16_95 Depth=1
	movq	88(%rsp), %rbx          # 8-byte Reload
	movq	48(%rbx), %rdi
	movq	%r13, %rsi
	callq	bitmap_clear_bit
	movq	40(%rbx), %rdi
	movq	%r13, %rsi
	callq	bitmap_set_bit
.LBB16_97:                              # %if.end349.us
                                        #   in Loop: Header=BB16_95 Depth=1
	cmpq	$49, 32(%rsp)           # 8-byte Folded Reload
	je	.LBB16_99
# BB#98:                                # %if.then352.us
                                        #   in Loop: Header=BB16_95 Depth=1
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rdi
	movq	%r13, %rsi
	callq	bitmap_set_bit
.LBB16_99:                              # %if.end353.us
                                        #   in Loop: Header=BB16_95 Depth=1
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %r12
	movabsq	$0, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	72(%rsp), %rbx          # 8-byte Reload
	jne	.LBB16_101
# BB#100:                               # %if.end353.us
                                        #   in Loop: Header=BB16_95 Depth=1
	movabsq	$1, %rax
.LBB16_101:                             # %if.end353.us
                                        #   in Loop: Header=BB16_95 Depth=1
	orq	%r12, %rdi
	orq	%rax, %rbx
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r13
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB16_95
	jmp	.LBB16_120
.LBB16_111:
	movq	88(%rsp), %r12          # 8-byte Reload
	.align	16, 0x90
.LBB16_112:                             # %for.body333
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	%rdi, 80(%rsp)          # 8-byte Spill
	movq	8(%r12), %rdi
	movq	%r13, %rsi
	callq	bitmap_bit_p
	cmpq	$0, 40(%r12)
	movq	%rax, %rbx
	je	.LBB16_117
# BB#113:                               # %land.lhs.true340
                                        #   in Loop: Header=BB16_112 Depth=1
	movq	48(%r12), %rdi
	movq	%r13, %rsi
	callq	bitmap_clear_bit
	movq	40(%r12), %rdi
	movq	%r13, %rsi
	callq	bitmap_bit_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB16_115
# BB#114:                               # %if.then344
                                        #   in Loop: Header=BB16_112 Depth=1
	movq	48(%r12), %rdi
	jmp	.LBB16_116
.LBB16_115:                             # %if.else346
                                        #   in Loop: Header=BB16_112 Depth=1
	movq	40(%r12), %rdi
.LBB16_116:                             # %if.end353
                                        #   in Loop: Header=BB16_112 Depth=1
	movq	%r13, %rsi
	callq	bitmap_set_bit
.LBB16_117:                             # %if.end353
                                        #   in Loop: Header=BB16_112 Depth=1
	movq	16(%r12), %rdi
	movq	%r13, %rsi
	callq	bitmap_set_bit
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rbx
	movabsq	$0, %rcx
	jne	.LBB16_119
# BB#118:                               # %if.end353
                                        #   in Loop: Header=BB16_112 Depth=1
	movabsq	$1, %rcx
.LBB16_119:                             # %if.end353
                                        #   in Loop: Header=BB16_112 Depth=1
	movq	80(%rsp), %rdi          # 8-byte Reload
	orq	%rbx, %rdi
	movq	72(%rsp), %rbx          # 8-byte Reload
	orq	%rcx, %rbx
	incq	%r13
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB16_112
.LBB16_120:                             # %for.end357
	movq	%rdi, 80(%rsp)          # 8-byte Spill
	movq	%rsi, %r13
	movq	64(%rsp), %rcx          # 8-byte Reload
	testq	$71, %rcx
	je	.LBB16_161
# BB#121:                               # %if.then360
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movl	88(%rax), %eax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	testq	$66, %rcx
	movq	%rcx, %r8
	movq	56(%rsp), %rbx          # 8-byte Reload
	je	.LBB16_125
# BB#122:                               # %if.then363
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	24(%rdx), %rdx
	sarq	%cl, %rax
	movq	(%rdx,%rax,8), %rdi
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rax, %rsi
	jl	.LBB16_125
	jmp	.LBB16_123
	.align	16, 0x90
.LBB16_124:                             # %for.body369.for.body369_crit_edge
                                        #   in Loop: Header=BB16_123 Depth=1
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	24(%rcx), %rdx
	incq	%rax
.LBB16_123:                             # %for.body369
                                        # =>This Inner Loop Header: Depth=1
	movq	$0, (%rdx,%rax,8)
	movq	$32, %rcx
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
	jl	.LBB16_124
.LBB16_125:                             # %if.end376
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	%r8, %rdi
	movq	%rdi, %rax
	movq	%rdi, %r12
	andq	$4, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	je	.LBB16_142
# BB#126:                               # %for.cond380.preheader
	movq	$32, %rcx
	movq	%rbx, %rax
	movq	%rbx, %r9
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB16_134
# BB#127:                               # %for.body383.lr.ph
	movq	$32, %rcx
	movq	%r9, %rsi
	shlq	%cl, %rsi
	movq	reg_n_info(%rip), %rdi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movabsq	$3777893186295716171, %r8 # imm = 0x346DC5D63886594B
	.align	16, 0x90
.LBB16_128:                             # %for.body383
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rdi,%rsi,8), %rbx
	movl	12(%rbx), %eax
	incq	%rax
	movl	%eax, 12(%rbx)
	movl	16(%rbx), %eax
	incq	%rax
	movl	%eax, 16(%rbx)
	movl	optimize_size(%rip), %eax
	testq	%rax, %rax
	movq	$1000, %rdx             # imm = 0x3E8
	jne	.LBB16_133
# BB#129:                               # %lor.lhs.false394
                                        #   in Loop: Header=BB16_128 Depth=1
	movl	flag_branch_probabilities(%rip), %eax
	testq	%rax, %rax
	je	.LBB16_131
# BB#130:                               # %lor.lhs.false394
                                        #   in Loop: Header=BB16_128 Depth=1
	movq	entry_exit_blocks+96(%rip), %rax
	testq	%rax, %rax
	je	.LBB16_133
.LBB16_131:                             # %cond.false399
                                        #   in Loop: Header=BB16_128 Depth=1
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movl	104(%rax), %eax
	imulq	$1000, %rax, %rax       # imm = 0x3E8
	leaq	9999(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$19999, %rcx            # imm = 0x4E1F
	movq	$1, %rdx
	jb	.LBB16_133
# BB#132:                               # %cond.true403
                                        #   in Loop: Header=BB16_128 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	%r8
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$11, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	.align	16, 0x90
.LBB16_133:                             # %cond.end411
                                        #   in Loop: Header=BB16_128 Depth=1
	movl	20(%rbx), %eax
	addq	%rdx, %rax
	movl	%eax, 20(%rbx)
	movl	28(%rbx), %eax
	incq	%rax
	movl	%eax, 28(%rbx)
	movq	$32, %rcx
	leaq	1(%rsi), %rax
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rsi
	movq	%rax, %rsi
	jl	.LBB16_128
.LBB16_134:                             # %for.end425
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	cmpq	$52, %rax
	jg	.LBB16_137
# BB#135:                               # %for.cond429.preheader
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB16_142
# BB#136:                               # %for.body432.lr.ph
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	regs_ever_live(%rax), %rdi
	movq	%r14, %rdx
	subq	%r9, %rdx
	andq	%r13, %rdx
	incq	%rdx
	movq	$1, %rsi
	callq	memset
	jmp	.LBB16_142
.LBB16_137:                             # %if.else438
	movq	%r9, %rax
	movq	reg_n_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %rax
	movl	36(%rax), %ecx
	cmpq	%r13, %rcx
	je	.LBB16_138
# BB#139:                               # %if.else451
	movq	32(%rsp), %rdx          # 8-byte Reload
	andq	%r13, %rdx
	cmpq	%rdx, %rcx
	je	.LBB16_142
# BB#140:                               # %if.then459
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	jmp	.LBB16_141
.LBB16_42:                              # %cond.true180
	movl	mode_class(,%rbx,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB16_52
# BB#43:                                # %lor.rhs185
	cmpq	$6, %rcx
	jne	.LBB16_44
# BB#45:                                # %lor.rhs185
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB16_52
.LBB16_138:                             # %if.then445
	movq	32(%rsp), %rcx          # 8-byte Reload
.LBB16_141:                             # %if.end468
	movl	%ecx, 36(%rax)
.LBB16_142:                             # %if.end468
	movq	72(%rsp), %rax          # 8-byte Reload
	testq	%r13, %rax
	je	.LBB16_143
# BB#149:                               # %if.else506
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	jne	.LBB16_161
# BB#150:                               # %if.else509
	movq	%r12, %rsi
	movq	80(%rsp), %rax          # 8-byte Reload
	testq	%r13, %rax
	je	.LBB16_151
# BB#155:                               # %if.else532
	testq	$1, %rsi
	je	.LBB16_161
# BB#156:                               # %if.else532
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB16_161
# BB#157:                               # %for.body539.lr.ph
	movq	$32, %rcx
	movq	56(%rsp), %rbx          # 8-byte Reload
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	48(%rsp), %r15          # 8-byte Reload
.LBB16_158:                             # %for.body539
                                        # =>This Inner Loop Header: Depth=1
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdi
	movq	%rbx, %rsi
	callq	bitmap_bit_p
	testq	%r13, %rax
	jne	.LBB16_160
# BB#159:                               # %if.then543
                                        #   in Loop: Header=BB16_158 Depth=1
	movl	reg_raw_mode(,%rbx,4), %edi
	movq	%rbx, %rsi
	callq	gen_rtx_REG
	movq	56(%r15), %rdx
	movq	$10, %rdi
	movq	%rax, %rsi
	callq	alloc_EXPR_LIST
	movq	%rax, 56(%r15)
.LBB16_160:                             # %for.inc555
                                        #   in Loop: Header=BB16_158 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rbx
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB16_158
	jmp	.LBB16_161
.LBB16_143:                             # %if.then470
	testq	$2, %r12
	movq	16(%rsp), %rdx          # 8-byte Reload
	je	.LBB16_161
# BB#144:                               # %if.then470
	testq	%rdx, %rdx
	je	.LBB16_161
# BB#145:                               # %land.lhs.true475
	movslq	8(%rdx), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	88(%rax), %eax
	movq	32(%rsp), %rcx          # 8-byte Reload
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	jne	.LBB16_161
# BB#146:                               # %land.lhs.true486
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	jg	.LBB16_148
# BB#147:                               # %lor.lhs.false489
	movq	32(%rdx), %rdi
	movq	%rdx, %rbx
	callq	asm_noperands
	movq	%rbx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB16_161
.LBB16_148:                             # %if.then496
	movq	48(%rdx), %rsi
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%rdx, %rbx
	callq	alloc_INSN_LIST
	movq	%rax, 48(%rbx)
	jmp	.LBB16_161
.LBB16_151:                             # %if.then511
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%r13, %rax
	je	.LBB16_153
# BB#152:                               # %if.then514
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	reg_n_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %rax
	movl	24(%rax), %ecx
	incq	%rcx
	movl	%ecx, 24(%rax)
.LBB16_153:                             # %if.end520
	testq	$1, %rsi
	je	.LBB16_161
# BB#154:                               # %if.then523
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	56(%rbx), %rdx
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	alloc_EXPR_LIST
	movq	%rax, 56(%rbx)
.LBB16_161:                             # %if.end562
	cmpq	$7, 24(%rsp)            # 8-byte Folded Reload
	movq	88(%rsp), %r15          # 8-byte Reload
	je	.LBB16_174
# BB#162:                               # %if.end562
	movq	80(%rsp), %rax          # 8-byte Reload
	testq	%r13, %rax
	je	.LBB16_174
# BB#163:                               # %if.end562
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB16_174
# BB#164:                               # %for.body571.lr.ph
	movq	56(%rsp), %rbx          # 8-byte Reload
	.align	16, 0x90
.LBB16_165:                             # %for.body571
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	subq	%rax, %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	testq	%rcx, %rax
	jne	.LBB16_167
# BB#166:                               # %if.then575
                                        #   in Loop: Header=BB16_165 Depth=1
	movq	8(%r15), %rdi
	movq	%rbx, %rsi
	callq	bitmap_clear_bit
.LBB16_167:                             # %for.inc578
                                        #   in Loop: Header=BB16_165 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rbx
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB16_165
.LBB16_174:                             # %if.end614
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB16_48:                              # %cond.false192
	cmpq	$18, %rbx
	jne	.LBB16_51
# BB#49:                                # %cond.true195
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB16_52
.LBB16_44:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB16_52
.LBB16_51:                              # %cond.false206
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rbx), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB16_52:                              # %cond.end223
	movq	%rdi, %rsi
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	leaq	-1(%rsi,%rax), %r14
	movq	%rbx, %rdi
	callq	gen_rtx_REG
	movq	%rax, %r15
	jmp	.LBB16_57
.Ltmp225:
	.size	mark_set_1, .Ltmp225-mark_set_1
	.cfi_endproc

	.align	16, 0x90
	.type	mark_used_regs,@function
mark_used_regs:                         # @mark_used_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp232:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp233:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp234:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp235:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp236:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp237:
	.cfi_def_cfa_offset 112
.Ltmp238:
	.cfi_offset %rbx, -48
.Ltmp239:
	.cfi_offset %r12, -40
.Ltmp240:
	.cfi_offset %r13, -32
.Ltmp241:
	.cfi_offset %r14, -24
.Ltmp242:
	.cfi_offset %r15, -16
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	%rdx, %r14
	movq	%rsi, %r8
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	testq	%r8, %r8
	je	.LBB17_68
# BB#1:                                 # %if.end.lr.ph.lr.ph.lr.ph
	movq	56(%rsp), %rax          # 8-byte Reload
	leaq	32(%rax), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
.LBB17_2:                               # %if.end.lr.ph.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB17_3 Depth 2
                                        #       Child Loop BB17_4 Depth 3
                                        #         Child Loop BB17_14 Depth 4
                                        #           Child Loop BB17_15 Depth 5
                                        #         Child Loop BB17_22 Depth 4
                                        #         Child Loop BB17_30 Depth 4
                                        #           Child Loop BB17_31 Depth 5
                                        #         Child Loop BB17_53 Depth 4
                                        #         Child Loop BB17_56 Depth 4
                                        #           Child Loop BB17_66 Depth 5
	movq	56(%rsp), %rax          # 8-byte Reload
	movl	64(%rax), %eax
	andq	$16, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
.LBB17_3:                               # %if.end.lr.ph
                                        #   Parent Loop BB17_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB17_4 Depth 3
                                        #         Child Loop BB17_14 Depth 4
                                        #           Child Loop BB17_15 Depth 5
                                        #         Child Loop BB17_22 Depth 4
                                        #         Child Loop BB17_30 Depth 4
                                        #           Child Loop BB17_31 Depth 5
                                        #         Child Loop BB17_53 Depth 4
                                        #         Child Loop BB17_56 Depth 4
                                        #           Child Loop BB17_66 Depth 5
	movq	%r14, 8(%rsp)           # 8-byte Spill
.LBB17_4:                               # %if.end
                                        #   Parent Loop BB17_2 Depth=1
                                        #     Parent Loop BB17_3 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB17_14 Depth 4
                                        #           Child Loop BB17_15 Depth 5
                                        #         Child Loop BB17_22 Depth 4
                                        #         Child Loop BB17_30 Depth 4
                                        #           Child Loop BB17_31 Depth 5
                                        #         Child Loop BB17_53 Depth 4
                                        #         Child Loop BB17_56 Depth 4
                                        #           Child Loop BB17_66 Depth 5
	movl	(%r8), %eax
	movq	%rax, %r15
	andq	$65535, %r15            # imm = 0xFFFF
	cmpq	$151, %r15
	jg	.LBB17_67
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB17_4 Depth=3
	leaq	-38(%r15), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$30, %rcx
	ja	.LBB17_54
# BB#6:                                 # %if.end
                                        #   in Loop: Header=BB17_4 Depth=3
	jmpq	*.LJTI17_0(,%rcx,8)
.LBB17_51:                              # %if.end197
                                        #   in Loop: Header=BB17_4 Depth=3
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r8, %rbx
	callq	free_EXPR_LIST_list
	movq	%rbx, %r8
	movq	56(%rsp), %rax          # 8-byte Reload
	xorq	%rcx, %rcx
	movl	%ecx, 56(%rax)
	cmpq	$41, %r15
	jne	.LBB17_54
	jmp	.LBB17_52
	.align	16, 0x90
.LBB17_67:                              # %if.end
                                        #   in Loop: Header=BB17_4 Depth=3
	cmpq	$152, %r15
	je	.LBB17_68
.LBB17_54:                              # %sw.epilog
                                        #   in Loop: Header=BB17_4 Depth=3
	movzbl	rtx_length(%r15), %edx
	testq	%rdx, %rdx
	je	.LBB17_68
# BB#55:                                # %for.body235.lr.ph
                                        #   in Loop: Header=BB17_4 Depth=3
	movq	rtx_format(,%r15,8), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB17_56:                              # %for.body235
                                        #   Parent Loop BB17_2 Depth=1
                                        #     Parent Loop BB17_3 Depth=2
                                        #       Parent Loop BB17_4 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB17_66 Depth 5
	movq	%rdx, %rdi
	movq	32(%rsp), %rax          # 8-byte Reload
	movzbl	-1(%rax,%rdi), %eax
	leaq	-1(%rdi), %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB17_57
# BB#65:                                # %for.cond261.preheader
                                        #   in Loop: Header=BB17_56 Depth=4
	movq	(%r8,%rdi,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r15
	movq	48(%rsp), %r13          # 8-byte Reload
	jle	.LBB17_58
	.align	16, 0x90
.LBB17_66:                              # %for.body269
                                        #   Parent Loop BB17_2 Depth=1
                                        #     Parent Loop BB17_3 Depth=2
                                        #       Parent Loop BB17_4 Depth=3
                                        #         Parent Loop BB17_56 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	(%rax,%r15,8), %rsi
	movq	%rdi, %r12
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rdx
	movq	%r13, %rcx
	movq	%r14, %rbx
	movq	%r8, %r14
	callq	mark_used_regs
	movq	%r12, %rdi
	movq	%r14, %r8
	movq	%rbx, %r14
	movq	(%r8,%rdi,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r15
	cmpq	%rdx, %rsi
	jl	.LBB17_66
	jmp	.LBB17_58
	.align	16, 0x90
.LBB17_57:                              # %for.body235
                                        #   in Loop: Header=BB17_56 Depth=4
	cmpq	$101, %rax
	movq	48(%rsp), %r13          # 8-byte Reload
	jne	.LBB17_58
# BB#61:                                # %if.then241
                                        #   in Loop: Header=BB17_56 Depth=4
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	40(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB17_62
# BB#64:                                # %if.end248
                                        #   in Loop: Header=BB17_56 Depth=4
	movq	(%r8,%rdi,8), %rsi
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rdx
	movq	48(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rcx
	movq	%r8, %rbx
	callq	mark_used_regs
	movq	%rbx, %r8
.LBB17_58:                              # %for.cond232.backedge
                                        #   in Loop: Header=BB17_56 Depth=4
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB17_56
	jmp	.LBB17_68
.LBB17_62:                              # %if.then244
                                        #   in Loop: Header=BB17_4 Depth=3
	movq	8(%r8), %r8
.LBB17_63:                              # %retry.backedge
                                        #   in Loop: Header=BB17_4 Depth=3
	testq	%r8, %r8
	jne	.LBB17_4
	jmp	.LBB17_68
.LBB17_49:                              # %lor.lhs.false189
                                        #   in Loop: Header=BB17_4 Depth=3
	testq	$134217728, %rax        # imm = 0x8000000
	je	.LBB17_52
# BB#50:                                # %if.end197.thread
                                        #   in Loop: Header=BB17_4 Depth=3
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r8, %rbx
	callq	free_EXPR_LIST_list
	movq	%rbx, %r8
	movq	56(%rsp), %rax          # 8-byte Reload
	xorq	%rcx, %rcx
	movl	%ecx, 56(%rax)
.LBB17_52:                              # %for.cond.preheader
                                        #   in Loop: Header=BB17_4 Depth=3
	movq	32(%r8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %rbx
	jle	.LBB17_54
	.align	16, 0x90
.LBB17_53:                              # %for.body
                                        #   Parent Loop BB17_2 Depth=1
                                        #     Parent Loop BB17_3 Depth=2
                                        #       Parent Loop BB17_4 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	(%rax,%rbx,8), %rsi
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rdx
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	%r15, %r12
	movq	%r14, %r15
	movq	%r8, %r14
	callq	mark_used_regs
	movq	%r14, %r8
	movq	%r15, %r14
	movq	%r12, %r15
	movq	32(%r8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB17_53
	jmp	.LBB17_54
.LBB17_28:                              # %sw.bb65
                                        #   in Loop: Header=BB17_4 Depth=3
	movq	%r15, %r12
	movq	%r14, %r15
	movq	8(%r8), %r13
	movq	%r8, %r14
	movl	(%r13), %r8d
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	je	.LBB17_69
# BB#29:                                # %while.cond80.preheader
                                        #   in Loop: Header=BB17_4 Depth=3
	movl	target_flags(%rip), %r9d
	movq	$23, %rcx
	shrq	%cl, %r9
	andq	$4, %r9
	leaq	4(%r9), %r10
	orq	$3, %r9
	xorq	%rax, %rax
	movq	%r13, %rbx
	jmp	.LBB17_30
.LBB17_34:                              # %while.body94.if.else130_crit_edge
                                        #   in Loop: Header=BB17_30 Depth=4
	movq	8(%rbx), %rbx
	movl	(%rbx), %r8d
	movq	$1, %rax
.LBB17_30:                              # %while.cond80.outer
                                        #   Parent Loop BB17_2 Depth=1
                                        #     Parent Loop BB17_3 Depth=2
                                        #       Parent Loop BB17_4 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB17_31 Depth 5
	movq	%rax, %rsi
	movq	%r8, %r11
	.align	16, 0x90
.LBB17_31:                              # %while.cond80
                                        #   Parent Loop BB17_2 Depth=1
                                        #     Parent Loop BB17_3 Depth=2
                                        #       Parent Loop BB17_4 Depth=3
                                        #         Parent Loop BB17_30 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	%r11, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB17_32
# BB#35:                                # %land.lhs.true98
                                        #   in Loop: Header=BB17_31 Depth=5
	movq	8(%rbx), %rbx
	movl	(%rbx), %r8d
	movq	$16, %rcx
	movq	%r8, %rax
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %eax
	leaq	-1(%rax,%r10), %rax
	cqto
	idivq	%r10
	movq	%rax, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$16, %rcx
	shrq	%cl, %r11
	andq	$255, %r11
	movzbl	mode_size(%r11), %eax
	addq	%r9, %rax
	cqto
	idivq	%r10
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdi
	movq	$1, %rax
	movq	%r8, %r11
	jle	.LBB17_31
	jmp	.LBB17_30
.LBB17_32:                              # %while.cond80
                                        #   in Loop: Header=BB17_30 Depth=4
	leaq	-132(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$2, %rcx
	jb	.LBB17_34
# BB#33:                                # %while.cond80
                                        #   in Loop: Header=BB17_30 Depth=4
	cmpq	$64, %rax
	je	.LBB17_34
# BB#36:                                # %while.end135
                                        #   in Loop: Header=BB17_4 Depth=3
	andq	$16777215, %r11         # imm = 0xFFFFFF
	cmpq	$3342375, %r11          # imm = 0x330027
	movq	%r14, %r8
	movq	%r15, %r14
	movq	%r12, %r15
	je	.LBB17_44
# BB#37:                                # %lor.lhs.false146
                                        #   in Loop: Header=BB17_4 Depth=3
	cmpq	$61, %rax
	jne	.LBB17_54
# BB#38:                                # %land.lhs.true151
                                        #   in Loop: Header=BB17_4 Depth=3
	movl	8(%rbx), %eax
	cmpq	$6, %rax
	je	.LBB17_41
# BB#39:                                # %land.lhs.true151
                                        #   in Loop: Header=BB17_4 Depth=3
	cmpq	$16, %rax
	jne	.LBB17_40
# BB#43:                                # %land.lhs.true170
                                        #   in Loop: Header=BB17_4 Depth=3
	movzbl	fixed_regs+16(%rip), %eax
	testq	%rax, %rax
	jne	.LBB17_54
	jmp	.LBB17_44
.LBB17_26:                              # %sw.bb55
                                        #   in Loop: Header=BB17_4 Depth=3
	movq	8(%r8), %r8
	movzbl	(%r8), %eax
	cmpq	$61, %rax
	jne	.LBB17_63
	jmp	.LBB17_27
.LBB17_9:                               # %sw.bb13
                                        #   in Loop: Header=BB17_4 Depth=3
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB17_54
# BB#10:                                # %sw.bb13
                                        #   in Loop: Header=BB17_4 Depth=3
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	16(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB17_54
# BB#11:                                # %if.then16
                                        #   in Loop: Header=BB17_4 Depth=3
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movq	8(%r8), %rax
	movl	(%rax), %eax
	andq	$67174399, %rax         # imm = 0x400FFFF
	cmpq	$67108932, %rax         # imm = 0x4000044
	movq	56(%rsp), %r12          # 8-byte Reload
	je	.LBB17_20
# BB#12:                                # %if.else
                                        #   in Loop: Header=BB17_4 Depth=3
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rbx
	xorq	%r15, %r15
	testq	%rbx, %rbx
	je	.LBB17_20
.LBB17_14:                              # %while.body.lr.ph
                                        #   Parent Loop BB17_2 Depth=1
                                        #     Parent Loop BB17_3 Depth=2
                                        #       Parent Loop BB17_4 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB17_15 Depth 5
	movq	%r15, %r14
	movq	%rbx, %rax
.LBB17_15:                              # %while.body
                                        #   Parent Loop BB17_2 Depth=1
                                        #     Parent Loop BB17_3 Depth=2
                                        #       Parent Loop BB17_4 Depth=3
                                        #         Parent Loop BB17_14 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	8(%rax), %rdi
	movq	16(%rax), %rbx
	movq	%rax, %r15
	movq	%r8, %rsi
	movq	%r8, %r13
	callq	anti_dependence
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_13
# BB#16:                                # %if.then39
                                        #   in Loop: Header=BB17_15 Depth=5
	testq	%r14, %r14
	jne	.LBB17_17
# BB#18:                                # %if.else45
                                        #   in Loop: Header=BB17_15 Depth=5
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rbx, (%rax)
	jmp	.LBB17_19
.LBB17_17:                              # %if.then41
                                        #   in Loop: Header=BB17_15 Depth=5
	movq	%rbx, 16(%r14)
.LBB17_19:                              # %if.end47
                                        #   in Loop: Header=BB17_15 Depth=5
	movq	%r15, %rdi
	callq	free_EXPR_LIST_node
	movl	56(%r12), %eax
	decq	%rax
	movl	%eax, 56(%r12)
	testq	%rbx, %rbx
	movq	%rbx, %rax
	movq	%r13, %r8
	jne	.LBB17_15
	jmp	.LBB17_20
.LBB17_13:                              # %while.cond.outer.loopexit
                                        #   in Loop: Header=BB17_14 Depth=4
	testq	%rbx, %rbx
	movq	%r13, %r8
	jne	.LBB17_14
.LBB17_20:                              # %if.end50
                                        #   in Loop: Header=BB17_4 Depth=3
	cmpq	$0, 48(%rsp)            # 8-byte Folded Reload
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	40(%rsp), %r15          # 8-byte Reload
	je	.LBB17_54
# BB#21:                                # %if.then52
                                        #   in Loop: Header=BB17_4 Depth=3
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	56(%rax), %rbx
	jmp	.LBB17_22
.LBB17_25:                              # %for.inc.i
                                        #   in Loop: Header=BB17_22 Depth=4
	movq	16(%rbx), %rbx
.LBB17_22:                              # %if.then52
                                        #   Parent Loop BB17_2 Depth=1
                                        #     Parent Loop BB17_3 Depth=2
                                        #       Parent Loop BB17_4 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	testq	%rbx, %rbx
	je	.LBB17_54
# BB#23:                                # %for.body.i
                                        #   in Loop: Header=BB17_22 Depth=4
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$131072, %rax           # imm = 0x20000
	jne	.LBB17_25
# BB#24:                                # %if.then.i
                                        #   in Loop: Header=BB17_22 Depth=4
	movq	8(%rbx), %rsi
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %r12
	movq	%r14, %r15
	movq	%r8, %r14
	callq	invalidate_mems_from_set
	movq	%r14, %r8
	movq	%r15, %r14
	movq	%r12, %r15
	jmp	.LBB17_25
.LBB17_40:                              # %land.lhs.true151
                                        #   in Loop: Header=BB17_4 Depth=3
	cmpq	$20, %rax
	jne	.LBB17_44
.LBB17_41:                              # %land.rhs
                                        #   in Loop: Header=BB17_4 Depth=3
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB17_54
# BB#42:                                # %land.rhs
                                        #   in Loop: Header=BB17_4 Depth=3
	movl	frame_pointer_needed(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB17_54
	jmp	.LBB17_44
.LBB17_59:                              # %sw.bb211
                                        #   in Loop: Header=BB17_3 Depth=2
	testq	%r14, %r14
	jne	.LBB17_70
# BB#60:                                # %if.end215
                                        #   in Loop: Header=BB17_3 Depth=2
	movq	8(%r8), %rsi
	xorq	%rdx, %rdx
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	%r8, %rbx
	callq	mark_used_regs
	movq	%rbx, %r8
	movq	8(%r8), %r14
	movq	16(%r8), %r8
	testq	%r8, %r8
	jne	.LBB17_3
	jmp	.LBB17_68
.LBB17_7:                               # %sw.bb2
                                        #   in Loop: Header=BB17_2 Depth=1
	movq	8(%r8), %r8
	movzbl	(%r8), %eax
	cmpq	$66, %rax
	jne	.LBB17_68
# BB#8:                                 # %if.then5
                                        #   in Loop: Header=BB17_2 Depth=1
	movq	%r14, %r15
	addq	$8, %r8
	jmp	.LBB17_48
.LBB17_44:                              # %if.then175
                                        #   in Loop: Header=BB17_2 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB17_46
# BB#45:                                # %if.then177
                                        #   in Loop: Header=BB17_2 Depth=1
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	%r14, %rdx
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	%r8, %rbx
	callq	mark_used_regs
	movq	%rbx, %r8
.LBB17_46:                              # %if.end181
                                        #   in Loop: Header=BB17_2 Depth=1
	movq	%r14, %r15
	jmp	.LBB17_47
.LBB17_69:                              # %if.then72
                                        #   in Loop: Header=BB17_2 Depth=1
	movq	8(%r13), %rsi
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rdx
	movq	48(%rsp), %rcx          # 8-byte Reload
	callq	mark_used_regs
	movq	%r14, %r8
.LBB17_47:                              # %tailrecurse.backedge
                                        #   in Loop: Header=BB17_2 Depth=1
	addq	$16, %r8
.LBB17_48:                              # %tailrecurse.backedge
                                        #   in Loop: Header=BB17_2 Depth=1
	movq	(%r8), %r8
	testq	%r8, %r8
	movq	%r15, %r14
	jne	.LBB17_2
.LBB17_68:                              # %for.end284
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB17_27:                              # %sw.bb64
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r8, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	mark_used_reg  # TAILCALL
.LBB17_70:                              # %if.then214
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.mark_used_regs, %rcx
	movq	$3893, %rsi             # imm = 0xF35
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp243:
	.size	mark_used_regs, .Ltmp243-mark_used_regs
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI17_0:
	.quad	.LBB17_59
	.quad	.LBB17_54
	.quad	.LBB17_51
	.quad	.LBB17_49
	.quad	.LBB17_54
	.quad	.LBB17_51
	.quad	.LBB17_68
	.quad	.LBB17_68
	.quad	.LBB17_54
	.quad	.LBB17_28
	.quad	.LBB17_54
	.quad	.LBB17_7
	.quad	.LBB17_54
	.quad	.LBB17_54
	.quad	.LBB17_51
	.quad	.LBB17_54
	.quad	.LBB17_68
	.quad	.LBB17_68
	.quad	.LBB17_68
	.quad	.LBB17_54
	.quad	.LBB17_68
	.quad	.LBB17_68
	.quad	.LBB17_54
	.quad	.LBB17_27
	.quad	.LBB17_54
	.quad	.LBB17_26
	.quad	.LBB17_54
	.quad	.LBB17_54
	.quad	.LBB17_9
	.quad	.LBB17_68
	.quad	.LBB17_68

	.text
	.align	16, 0x90
	.type	mark_used_reg,@function
mark_used_reg:                          # @mark_used_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp250:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp251:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp252:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp253:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp254:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp255:
	.cfi_def_cfa_offset 96
.Ltmp256:
	.cfi_offset %rbx, -48
.Ltmp257:
	.cfi_offset %r12, -40
.Ltmp258:
	.cfi_offset %r13, -32
.Ltmp259:
	.cfi_offset %r14, -24
.Ltmp260:
	.cfi_offset %r15, -16
	movl	8(%rsi), %r13d
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	cmpq	$52, %r13
	movq	%r13, %rax
	ja	.LBB18_16
# BB#1:                                 # %if.then
	movzbl	2(%rsi), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%r13, %rcx
	cmpq	$8, %rcx
	je	.LBB18_5
# BB#2:                                 # %if.then
	leaq	-21(%r13), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB18_5
# BB#3:                                 # %if.then
	leaq	-45(%r13), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB18_5
# BB#4:                                 # %if.then
	leaq	-29(%r13), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB18_5
# BB#10:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB18_11
# BB#13:                                # %cond.true34
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB18_15
.LBB18_5:                               # %cond.true
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB18_15
# BB#6:                                 # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB18_7
# BB#8:                                 # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB18_15
.LBB18_11:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB18_14
# BB#12:                                # %cond.true27
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB18_15
.LBB18_7:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB18_15
.LBB18_14:                              # %cond.false38
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
.LBB18_15:                              # %cond.end53
	leaq	-1(%r13,%rax), %rax
.LBB18_16:                              # %if.end
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	xorq	%r14, %r14
	cmpq	%rax, %r13
	movabsq	$0, %r15
	movabsq	$0, %rdi
	ja	.LBB18_27
# BB#17:                                # %for.body.lr.ph
	xorq	%r15, %r15
	movq	%r13, %rbx
	xorq	%r14, %r14
	.align	16, 0x90
.LBB18_18:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdi
	movq	%rbx, %rsi
	movq	%r8, %r12
	callq	bitmap_bit_p
	movq	%r12, %r8
	testq	%r8, %rax
	movabsq	$0, %rcx
	jne	.LBB18_20
# BB#19:                                # %for.body
                                        #   in Loop: Header=BB18_18 Depth=1
	movabsq	$1, %rcx
.LBB18_20:                              # %for.body
                                        #   in Loop: Header=BB18_18 Depth=1
	orq	%rax, %r14
	incq	%rbx
	orq	%rcx, %r15
	movq	%rbx, %rax
	andq	%r8, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jbe	.LBB18_18
# BB#21:                                # %for.cond61.preheader
	xorq	%rdi, %rdi
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r13
	ja	.LBB18_27
# BB#22:                                # %for.body64.lr.ph
	xorq	%rdi, %rdi
	movq	%r13, %rbx
	movq	%r13, 16(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB18_23:                              # %for.body64
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %r13
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rdi
	movq	%rbx, %rsi
	movq	%r8, %r12
	callq	bitmap_bit_p
	movq	%r12, %r8
	testq	%r8, %rax
	movabsq	$0, %rax
	jne	.LBB18_25
# BB#24:                                # %for.body64
                                        #   in Loop: Header=BB18_23 Depth=1
	movabsq	$1, %rax
.LBB18_25:                              # %for.body64
                                        #   in Loop: Header=BB18_23 Depth=1
	incq	%rbx
	movq	%r13, %rdi
	orq	%rax, %rdi
	movq	%rbx, %rax
	andq	%r8, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jbe	.LBB18_23
# BB#26:
	movq	16(%rsp), %r13          # 8-byte Reload
.LBB18_27:                              # %for.end72
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	64(%rax), %esi
	testq	$66, %rsi
	je	.LBB18_29
# BB#28:                                # %if.then75
	movq	24(%rax), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, (%rax,%r13,8)
.LBB18_29:                              # %if.end78
	testq	$4, %rsi
	je	.LBB18_50
# BB#30:                                # %if.then82
	cmpq	$52, %r13
	ja	.LBB18_40
# BB#31:                                # %if.then85
	movq	%rdi, %rbx
	movq	elim_reg_set(%rip), %rax
	movq	$1, %rdx
	movq	%r13, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	je	.LBB18_33
# BB#32:                                # %if.then85
	movq	%rdx, %rcx
.LBB18_33:                              # %if.then85
	movabsq	$4294967291, %rdi       # imm = 0xFFFFFFFB
	andq	%r13, %rdi
	cmpq	$16, %rdi
	movq	%rbx, %rdi
	jne	.LBB18_35
# BB#34:                                # %if.then85
	movq	%rdx, %rax
.LBB18_35:                              # %if.then85
	testq	%rax, %rcx
	jne	.LBB18_50
# BB#36:                                # %if.then85
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r13
	ja	.LBB18_50
# BB#37:
	movq	$1, %rax
	movq	%r13, %rcx
	movq	32(%rsp), %rsi          # 8-byte Reload
	.align	16, 0x90
.LBB18_38:                              # %for.body98
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	andq	%r8, %rdx
	incq	%rcx
	movb	%al, regs_ever_live(%rdx)
	movq	%rcx, %rdx
	andq	%r8, %rdx
	cmpq	%rsi, %rdx
	jbe	.LBB18_38
# BB#39:                                # %for.cond95.if.end163.loopexit_crit_edge
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	64(%rax), %esi
	jmp	.LBB18_50
.LBB18_40:                              # %if.else
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r13,8), %rbx
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movl	36(%rbx), %edx
	movl	88(%rax), %ecx
	cmpq	%r8, %rdx
	je	.LBB18_43
# BB#41:                                # %if.else116
	andq	%r8, %rcx
	cmpq	%rcx, %rdx
	je	.LBB18_44
# BB#42:                                # %if.then124
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
.LBB18_43:                              # %if.end131
	movl	%ecx, 36(%rbx)
.LBB18_44:                              # %if.end131
	movl	optimize_size(%rip), %ecx
	testq	%rcx, %rcx
	movq	$1000, %rdx             # imm = 0x3E8
	jne	.LBB18_49
# BB#45:                                # %lor.lhs.false133
	movl	flag_branch_probabilities(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB18_47
# BB#46:                                # %lor.lhs.false133
	movq	entry_exit_blocks+96(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB18_49
.LBB18_47:                              # %cond.false138
	movl	104(%rax), %eax
	imulq	$1000, %rax, %rax       # imm = 0x3E8
	leaq	9999(%rax), %rcx
	andq	%r8, %rcx
	cmpq	$19999, %rcx            # imm = 0x4E1F
	movq	$1, %rdx
	jb	.LBB18_49
# BB#48:                                # %cond.true142
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
	sarq	%cl, %rdx
	addq	%rax, %rdx
.LBB18_49:                              # %cond.end150
	movl	20(%rbx), %eax
	addq	%rdx, %rax
	movl	%eax, 20(%rbx)
	movl	16(%rbx), %eax
	incq	%rax
	movl	%eax, 16(%rbx)
.LBB18_50:                              # %if.end163
	testq	$5, %rsi
	je	.LBB18_53
# BB#51:                                # %if.end163
	testq	%r8, %r15
	je	.LBB18_53
# BB#52:                                # %if.end163
	testq	%r8, %rdi
	je	.LBB18_53
# BB#56:                                # %if.then171
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r13
	jae	.LBB18_57
# BB#58:                                # %for.body178.lr.ph
	movq	%r13, %rbx
	movq	%r13, 16(%rsp)          # 8-byte Spill
	movq	40(%rsp), %r15          # 8-byte Reload
	.align	16, 0x90
.LBB18_59:                              # %for.body178
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%r15), %rdi
	movq	%rbx, %rsi
	movq	%r15, %r12
	movq	%r8, %r15
	movq	%rax, %r13
	callq	bitmap_bit_p
	movq	%r15, %r8
	movq	%r12, %r15
	incq	%rbx
	orq	%rax, %r14
	movq	%rbx, %rax
	andq	%r8, %rax
	cmpq	%r13, %rax
	movq	%r13, %rax
	jbe	.LBB18_59
	jmp	.LBB18_60
.LBB18_57:
	movq	%r13, 16(%rsp)          # 8-byte Spill
	movq	40(%rsp), %r15          # 8-byte Reload
.LBB18_60:                              # %if.end185
	testq	%r8, %r14
	movq	16(%rsp), %r13          # 8-byte Reload
	je	.LBB18_67
# BB#61:                                # %for.cond213.preheader
	cmpq	%rax, %r13
	ja	.LBB18_55
# BB#62:
	movq	%r13, %rbx
	.align	16, 0x90
.LBB18_63:                              # %for.body216
                                        # =>This Inner Loop Header: Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdi
	movq	%rbx, %rsi
	movq	%r8, %r14
	callq	bitmap_bit_p
	movq	%r14, %r8
	testq	%r8, %rax
	jne	.LBB18_66
# BB#64:                                # %land.lhs.true220
                                        #   in Loop: Header=BB18_63 Depth=1
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r8, %r14
	callq	dead_or_set_regno_p
	movq	%r14, %r8
	testq	%r8, %rax
	jne	.LBB18_66
# BB#65:                                # %if.then223
                                        #   in Loop: Header=BB18_63 Depth=1
	movq	%rbx, %rax
	andq	%r8, %rax
	movl	reg_raw_mode(,%rax,4), %edi
	movq	%rbx, %rsi
	movq	%r8, %r15
	callq	gen_rtx_REG
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	56(%r14), %rdx
	movq	$1, %rdi
	movq	%rax, %rsi
	callq	alloc_EXPR_LIST
	movq	%r15, %r8
	movq	%rax, 56(%r14)
.LBB18_66:                              # %for.inc235
                                        #   in Loop: Header=BB18_63 Depth=1
	incq	%rbx
	movq	%rbx, %rax
	andq	%r8, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jbe	.LBB18_63
	jmp	.LBB18_53
.LBB18_67:                              # %if.then187
	movzbl	64(%r15), %eax
	testq	$1, %rax
	je	.LBB18_70
# BB#68:                                # %land.lhs.true191
	movq	$1, %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rdx
	movq	%r8, %rbx
	callq	find_regno_note
	movq	%rbx, %r8
	testq	%rax, %rax
	jne	.LBB18_70
# BB#69:                                # %if.then194
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	56(%rbx), %rdx
	movq	$1, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r8, %r14
	callq	alloc_EXPR_LIST
	movq	%r14, %r8
	movq	%rax, 56(%rbx)
.LBB18_70:                              # %if.end201
	movzbl	64(%r15), %eax
	testq	$4, %rax
	je	.LBB18_53
# BB#71:                                # %if.then205
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r13,8), %rax
	movl	24(%rax), %ecx
	incq	%rcx
	movl	%ecx, 24(%rax)
.LBB18_53:                              # %for.cond240.preheader
	movq	32(%rsp), %r15          # 8-byte Reload
	cmpq	%r15, %r13
	movq	40(%rsp), %r14          # 8-byte Reload
	ja	.LBB18_55
	.align	16, 0x90
.LBB18_54:                              # %for.body243
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r14), %rdi
	movq	%r13, %rsi
	movq	%r8, %rbx
	callq	bitmap_set_bit
	movq	%rbx, %r8
	incq	%r13
	movq	%r13, %rax
	andq	%r8, %rax
	cmpq	%r15, %rax
	jbe	.LBB18_54
.LBB18_55:                              # %for.end247
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp261:
	.size	mark_used_reg, .Ltmp261-mark_used_reg
	.cfi_endproc

	.globl	init_propagate_block_info
	.align	16, 0x90
	.type	init_propagate_block_info,@function
init_propagate_block_info:              # @init_propagate_block_info
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp268:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp269:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp270:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp271:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp272:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp273:
	.cfi_def_cfa_offset 80
.Ltmp274:
	.cfi_offset %rbx, -48
.Ltmp275:
	.cfi_offset %r12, -40
.Ltmp276:
	.cfi_offset %r13, -32
.Ltmp277:
	.cfi_offset %r14, -24
.Ltmp278:
	.cfi_offset %r15, -16
	movq	%r8, %r12
	movq	%rcx, %r13
	movq	%rdx, %rbx
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	$72, %rdi
	callq	xmalloc
	movq	%r15, (%rax)
	movq	%r14, 8(%rax)
	movq	$0, 32(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 56(%rax)
	movq	%rbx, 40(%rax)
	movq	%rax, %rbx
	movq	%r13, 48(%rbx)
	movl	%ecx, 60(%rbx)
	movl	%r12d, 64(%rbx)
	testq	$66, %r12
	je	.LBB19_2
# BB#1:                                 # %if.then
	callq	max_reg_num
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$8, %rsi
	movq	%rax, %rdi
	callq	xcalloc
	movq	%rax, 24(%rbx)
	jmp	.LBB19_3
.LBB19_2:                               # %if.else
	movq	$0, 24(%rbx)
.LBB19_3:                               # %if.end
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movl	optimize(%rip), %ecx
	movq	%rax, 16(%rbx)
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	movq	%r15, %r14
	movq	%r14, 16(%rsp)          # 8-byte Spill
	je	.LBB19_38
# BB#4:                                 # %land.lhs.true
	movq	current_function_decl(%rip), %rax
	movq	8(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$23, %rcx
	jne	.LBB19_7
# BB#5:                                 # %land.lhs.true13
	movzbl	62(%rax), %eax
	testq	$2, %rax
	jne	.LBB19_38
# BB#6:                                 # %land.lhs.true13
	andq	$16, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	jne	.LBB19_8
	jmp	.LBB19_38
.LBB19_7:                               # %land.lhs.true20
	testq	$16, %r12
	je	.LBB19_38
.LBB19_8:                               # %land.lhs.true23
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.LBB19_12
# BB#9:                                 # %lor.lhs.false
	cmpq	$0, 8(%rax)
	jne	.LBB19_38
# BB#10:                                # %land.lhs.true29
	movq	24(%rax), %rax
	movabsq	$entry_exit_blocks+112, %rcx
	cmpq	%rcx, %rax
	jne	.LBB19_38
# BB#11:                                # %land.lhs.true33
	movq	cfun(%rip), %rax
	movzbl	424(%rax), %eax
	testq	$128, %rax
	jne	.LBB19_38
.LBB19_12:                              # %if.then38
	movq	(%r14), %rax
	movq	8(%r14), %rbx
	cmpq	%rax, %rbx
	je	.LBB19_38
# BB#13:
	movzbl	rtx_class+32(%rip), %r15d
	xorq	%r13, %r13
	andq	$255, %r15
	movq	%r15, (%rsp)            # 8-byte Spill
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	.align	16, 0x90
.LBB19_14:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_29 Depth 2
	movzbl	(%rbx), %eax
	cmpq	$32, %rax
	jne	.LBB19_37
# BB#15:                                # %for.body
                                        #   in Loop: Header=BB19_14 Depth=1
	cmpq	$105, %r15
	jne	.LBB19_37
# BB#16:                                # %cond.true
                                        #   in Loop: Header=BB19_14 Depth=1
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB19_18
# BB#17:                                # %cond.false
                                        #   in Loop: Header=BB19_14 Depth=1
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB19_18:                              # %cond.end65
                                        #   in Loop: Header=BB19_14 Depth=1
	testq	%rax, %rax
	je	.LBB19_37
# BB#19:                                # %land.lhs.true68
                                        #   in Loop: Header=BB19_14 Depth=1
	movq	8(%rax), %rdi
	movzbl	(%rdi), %eax
	cmpq	$66, %rax
	jne	.LBB19_37
# BB#20:                                # %if.then76
                                        #   in Loop: Header=BB19_14 Depth=1
	callq	canon_rtx
	movl	(%rax), %esi
	testq	$67108864, %rsi         # imm = 0x4000000
	jne	.LBB19_37
# BB#21:                                # %if.end86
                                        #   in Loop: Header=BB19_14 Depth=1
	movq	8(%rax), %rdi
	movq	global_rtl+24(%rip), %rcx
	cmpq	%rcx, %rdi
	jne	.LBB19_22
# BB#26:                                # %if.then120
                                        #   in Loop: Header=BB19_14 Depth=1
	andq	$16711680, %rsi         # imm = 0xFF0000
.LBB19_27:                              # %if.then120
                                        #   in Loop: Header=BB19_14 Depth=1
	cmpq	$3342336, %rsi          # imm = 0x330000
	je	.LBB19_37
# BB#28:                                # %if.end.i
                                        #   in Loop: Header=BB19_14 Depth=1
	movq	8(%rsp), %rcx           # 8-byte Reload
	testq	%rcx, %rcx
	movq	%rcx, %r12
	je	.LBB19_35
	jmp	.LBB19_29
	.align	16, 0x90
.LBB19_34:                              # %for.inc.i.for.body.i_crit_edge
                                        #   in Loop: Header=BB19_29 Depth=2
	movq	8(%rax), %rdi
.LBB19_29:                              # %for.body.i
                                        #   Parent Loop BB19_14 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %r15
	movq	8(%r12), %r14
	movq	8(%r14), %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB19_30
# BB#33:                                # %for.inc.i
                                        #   in Loop: Header=BB19_29 Depth=2
	movq	16(%r12), %r12
	testq	%r12, %r12
	movq	%r15, %rax
	jne	.LBB19_34
.LBB19_35:                              # %for.end.i
                                        #   in Loop: Header=BB19_14 Depth=1
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$99, %rdx
	movq	16(%rsp), %r14          # 8-byte Reload
	movq	(%rsp), %r15            # 8-byte Reload
	jg	.LBB19_37
# BB#36:                                # %if.then32.i
                                        #   in Loop: Header=BB19_14 Depth=1
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	alloc_EXPR_LIST
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rax, 32(%rcx)
	incq	%r13
	movl	%r13d, 56(%rcx)
	jmp	.LBB19_37
.LBB19_22:                              # %lor.lhs.false92
                                        #   in Loop: Header=BB19_14 Depth=1
	movzbl	(%rdi), %edx
	cmpq	$75, %rdx
	jne	.LBB19_37
# BB#23:                                # %land.lhs.true100
                                        #   in Loop: Header=BB19_14 Depth=1
	movq	8(%rdi), %rdx
	cmpq	%rcx, %rdx
	jne	.LBB19_37
# BB#24:                                # %land.lhs.true109
                                        #   in Loop: Header=BB19_14 Depth=1
	movq	16(%rdi), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$54, %rcx
	jne	.LBB19_37
# BB#25:                                # %land.lhs.true109
                                        #   in Loop: Header=BB19_14 Depth=1
	andq	$16711680, %rsi         # imm = 0xFF0000
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	jmp	.LBB19_27
.LBB19_30:                              # %if.then8.i
                                        #   in Loop: Header=BB19_14 Depth=1
	movzbl	2(%r15), %eax
	movzbl	2(%r14), %ecx
	movzbl	mode_size(%rax), %eax
	movzbl	mode_size(%rcx), %ecx
	cmpq	%rcx, %rax
	jbe	.LBB19_31
# BB#32:                                # %if.then21.i
                                        #   in Loop: Header=BB19_14 Depth=1
	movq	%r15, 8(%r12)
.LBB19_31:                              #   in Loop: Header=BB19_14 Depth=1
	movq	16(%rsp), %r14          # 8-byte Reload
	movq	(%rsp), %r15            # 8-byte Reload
	.align	16, 0x90
.LBB19_37:                              # %for.inc
                                        #   in Loop: Header=BB19_14 Depth=1
	movq	16(%rbx), %rbx
	movq	(%r14), %rax
	cmpq	%rax, %rbx
	jne	.LBB19_14
.LBB19_38:                              # %if.end126
	movq	24(%rsp), %rax          # 8-byte Reload
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp279:
	.size	init_propagate_block_info, .Ltmp279-init_propagate_block_info
	.cfi_endproc

	.globl	free_propagate_block_info
	.align	16, 0x90
	.type	free_propagate_block_info,@function
free_propagate_block_info:              # @free_propagate_block_info
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp282:
	.cfi_def_cfa_offset 16
.Ltmp283:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	leaq	32(%rbx), %rdi
	callq	free_EXPR_LIST_list
	movq	16(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB20_2
# BB#1:                                 # %if.then
	callq	bitmap_clear
	movq	16(%rbx), %rdi
	callq	free
	movq	$0, 16(%rbx)
.LBB20_2:                               # %do.end
	movq	24(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB20_4
# BB#3:                                 # %if.then5
	callq	free
.LBB20_4:                               # %if.end7
	movq	%rbx, %rdi
	popq	%rbx
	jmp	free  # TAILCALL
.Ltmp284:
	.size	free_propagate_block_info, .Ltmp284-free_propagate_block_info
	.cfi_endproc

	.globl	regno_uninitialized
	.align	16, 0x90
	.type	regno_uninitialized,@function
regno_uninitialized:                    # @regno_uninitialized
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp287:
	.cfi_def_cfa_offset 16
.Ltmp288:
	.cfi_offset %rbx, -16
	movl	n_basic_blocks(%rip), %eax
	movq	%rdi, %rbx
	testq	%rax, %rax
	je	.LBB21_5
# BB#1:                                 # %lor.lhs.false
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	cmpq	$52, %rax
	ja	.LBB21_6
# BB#2:                                 # %land.lhs.true
	movzbl	global_regs(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB21_5
# BB#3:                                 # %lor.lhs.false2
	movzbl	fixed_regs(%rax), %eax
	testq	%rax, %rax
	jne	.LBB21_5
# BB#4:                                 # %lor.lhs.false7
	movq	%rbx, %rdi
	callq	ix86_function_arg_regno_p
	testq	%rax, %rax
	je	.LBB21_6
.LBB21_5:                               # %return
	xorq	%rax, %rax
	popq	%rbx
	retq
.LBB21_6:                               # %if.end
	movq	basic_block_info(%rip), %rax
	movq	32(%rax), %rax
	movq	64(%rax), %rdi
	movq	%rbx, %rsi
	popq	%rbx
	jmp	bitmap_bit_p  # TAILCALL
.Ltmp289:
	.size	regno_uninitialized, .Ltmp289-regno_uninitialized
	.cfi_endproc

	.globl	regno_clobbered_at_setjmp
	.align	16, 0x90
	.type	regno_clobbered_at_setjmp,@function
regno_clobbered_at_setjmp:              # @regno_clobbered_at_setjmp
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp292:
	.cfi_def_cfa_offset 16
.Ltmp293:
	.cfi_offset %rbx, -16
	movl	n_basic_blocks(%rip), %ecx
	movq	%rdi, %rbx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB22_6
# BB#1:                                 # %if.end
	movq	$32, %rcx
	movq	%rbx, %rax
	movq	reg_n_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %rax
	movslq	12(%rax), %rax
	cmpq	$1, %rax
	jg	.LBB22_3
# BB#2:                                 # %lor.lhs.false
	movq	basic_block_info(%rip), %rax
	movq	32(%rax), %rax
	movq	64(%rax), %rdi
	movq	%rbx, %rsi
	callq	bitmap_bit_p
	movq	%rax, %rcx
	xorq	%rax, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB22_6
.LBB22_3:                               # %land.rhs
	movq	regs_live_at_setjmp(%rip), %rdi
	movq	%rbx, %rsi
	callq	bitmap_bit_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB22_4
# BB#5:                                 # %land.rhs
	movabsq	$1, %rax
.LBB22_6:                               # %return
	popq	%rbx
	retq
.LBB22_4:
	movabsq	$0, %rax
	popq	%rbx
	retq
.Ltmp294:
	.size	regno_clobbered_at_setjmp, .Ltmp294-regno_clobbered_at_setjmp
	.cfi_endproc

	.globl	find_use_as_address
	.align	16, 0x90
	.type	find_use_as_address,@function
find_use_as_address:                    # @find_use_as_address
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp301:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp302:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp303:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp304:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp305:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp306:
	.cfi_def_cfa_offset 80
.Ltmp307:
	.cfi_offset %rbx, -48
.Ltmp308:
	.cfi_offset %r12, -40
.Ltmp309:
	.cfi_offset %r13, -32
.Ltmp310:
	.cfi_offset %r14, -24
.Ltmp311:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movl	(%r13), %eax
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, %r15
	movq	%rax, %rbx
	andq	$65535, %rbx            # imm = 0xFFFF
	cmpq	$66, %rbx
	jne	.LBB23_9
# BB#1:                                 # %land.lhs.true
	movq	8(%r13), %rcx
	cmpq	%r15, %rcx
	jne	.LBB23_4
# BB#2:                                 # %land.lhs.true
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	jne	.LBB23_4
# BB#3:
	movq	%r13, %rax
	jmp	.LBB23_30
.LBB23_4:                               # %land.lhs.true6
	movzbl	(%rcx), %edx
	cmpq	$75, %rdx
	jne	.LBB23_9
# BB#5:                                 # %land.lhs.true13
	movq	8(%rcx), %rdx
	cmpq	%r15, %rdx
	jne	.LBB23_9
# BB#6:                                 # %land.lhs.true21
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB23_9
# BB#7:                                 # %land.lhs.true31
	movq	8(%rcx), %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %rcx
	jne	.LBB23_9
# BB#8:
	movq	%r13, %rax
	jmp	.LBB23_30
.LBB23_9:                               # %if.end42
	movq	rtx_format(,%rbx,8), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$132, %rax
	jne	.LBB23_11
# BB#10:                                # %if.then45
	movq	8(%r13), %rdi
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	callq	find_use_as_address
	testq	%rax, %rax
	movq	$1, %rax
	je	.LBB23_12
	jmp	.LBB23_30
.LBB23_11:                              # %if.end52
	movq	$1, %rax
.LBB23_12:                              # %if.end52
	cmpq	%r15, %r13
	je	.LBB23_30
# BB#13:                                # %if.end55
	movzbl	rtx_length(%rbx), %ebx
	xorq	%rax, %rax
	testq	%rbx, %rbx
	je	.LBB23_30
# BB#14:                                # %for.body.lr.ph.lr.ph
	xorq	%r14, %r14
.LBB23_16:                              # %for.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB23_17 Depth 2
                                        #     Child Loop BB23_26 Depth 2
                                        #       Child Loop BB23_27 Depth 3
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	decq	%rbx
	.align	16, 0x90
.LBB23_17:                              # %for.body
                                        #   Parent Loop BB23_16 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	16(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax,%rbx), %eax
	cmpq	$101, %rax
	jne	.LBB23_18
# BB#21:                                # %if.then65
                                        #   in Loop: Header=BB23_17 Depth=2
	movq	8(%r13,%rbx,8), %rdi
	movq	%r15, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	find_use_as_address
	movq	%rax, %rcx
	testq	%r14, %r14
	je	.LBB23_22
# BB#23:                                # %if.else
                                        #   in Loop: Header=BB23_17 Depth=2
	movq	$1, %rax
	testq	%rcx, %rcx
	jne	.LBB23_30
.LBB23_19:                              # %for.cond.backedge
                                        #   in Loop: Header=BB23_17 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB23_17
	jmp	.LBB23_20
.LBB23_18:                              # %for.body
                                        #   in Loop: Header=BB23_17 Depth=2
	cmpq	$69, %rax
	jne	.LBB23_19
# BB#24:                                # %if.then85
                                        #   in Loop: Header=BB23_16 Depth=1
	movq	8(%r13,%rbx,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movl	(%rax), %r12d
	jmp	.LBB23_26
.LBB23_22:                              #   in Loop: Header=BB23_16 Depth=1
	movq	%rcx, %r14
.LBB23_15:                              # %for.cond.outer.backedge
                                        #   in Loop: Header=BB23_16 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r14, %rax
	jg	.LBB23_16
	jmp	.LBB23_30
.LBB23_25:                              # %for.cond90.outer.loopexit
                                        #   in Loop: Header=BB23_26 Depth=2
	movq	%rcx, %r14
.LBB23_26:                              # %for.cond90.outer
                                        #   Parent Loop BB23_16 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB23_27 Depth 3
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	.align	16, 0x90
.LBB23_27:                              # %for.cond90
                                        #   Parent Loop BB23_16 Depth=1
                                        #     Parent Loop BB23_26 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB23_15
# BB#28:                                # %for.body93
                                        #   in Loop: Header=BB23_27 Depth=3
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax,%r12,8), %rdi
	movq	%r15, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	find_use_as_address
	movq	%rax, %rcx
	decq	%r12
	testq	%r14, %r14
	je	.LBB23_25
# BB#29:                                # %if.else104
                                        #   in Loop: Header=BB23_27 Depth=3
	movq	$1, %rax
	testq	%rcx, %rcx
	je	.LBB23_27
	jmp	.LBB23_30
.LBB23_20:
	movq	%r14, %rax
.LBB23_30:                              # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp312:
	.size	find_use_as_address, .Ltmp312-find_use_as_address
	.cfi_endproc

	.globl	dump_regset
	.align	16, 0x90
	.type	dump_regset,@function
dump_regset:                            # @dump_regset
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp319:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp320:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp321:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp322:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp323:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp324:
	.cfi_def_cfa_offset 64
.Ltmp325:
	.cfi_offset %rbx, -48
.Ltmp326:
	.cfi_offset %r12, -40
.Ltmp327:
	.cfi_offset %r13, -32
.Ltmp328:
	.cfi_offset %r14, -24
.Ltmp329:
	.cfi_offset %r15, -16
	movq	%rsi, %r8
	testq	%rdi, %rdi
	je	.LBB24_17
# BB#1:                                 # %do.body
	movq	(%rdi), %rdx
	jmp	.LBB24_2
	.align	16, 0x90
.LBB24_15:                              # %if.end34.1
                                        #   in Loop: Header=BB24_2 Depth=1
	movq	(%rdx), %rdx
.LBB24_2:                               # %do.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB24_4 Depth 2
                                        #     Child Loop BB24_10 Depth 2
	testq	%rdx, %rdx
	je	.LBB24_16
# BB#3:                                 # %for.cond9.preheader
                                        #   in Loop: Header=BB24_2 Depth=1
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	24(%rdx), %r13
	testq	%r13, %r13
	movq	$1, %r14
	je	.LBB24_9
	.align	16, 0x90
.LBB24_4:                               # %for.body16
                                        #   Parent Loop BB24_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%r14), %r12
	movq	$1, %rax
	movq	%r12, %rcx
	shlq	%cl, %rax
	testq	%r13, %rax
	je	.LBB24_8
# BB#5:                                 # %if.then18
                                        #   in Loop: Header=BB24_4 Depth=2
	notq	%rax
	movl	16(%rdx), %edx
	andq	%rax, %r13
	movq	$7, %rcx
	shlq	%cl, %rdx
	addq	%rdx, %r12
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	xorq	%rax, %rax
	movq	%r8, %rdi
	movq	%r8, %r15
	movabsq	$.L.str6, %rsi
	movq	%r12, %rdx
	callq	fprintf
	cmpq	$52, %rbx
	jg	.LBB24_7
# BB#6:                                 # %if.then25
                                        #   in Loop: Header=BB24_4 Depth=2
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	reg_names(,%r12,8), %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str7, %rsi
	callq	fprintf
.LBB24_7:                               # %if.end29
                                        #   in Loop: Header=BB24_4 Depth=2
	testq	%r13, %r13
	movq	%r15, %r8
	movq	8(%rsp), %rdx           # 8-byte Reload
	je	.LBB24_9
.LBB24_8:                               # %for.inc
                                        #   in Loop: Header=BB24_4 Depth=2
	movq	%r14, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	incq	%r14
	cmpq	$64, %rax
	jb	.LBB24_4
.LBB24_9:                               # %if.end34
                                        #   in Loop: Header=BB24_2 Depth=1
	movq	32(%rdx), %r14
	xorq	%rbx, %rbx
	testq	%r14, %r14
	je	.LBB24_15
	.align	16, 0x90
.LBB24_10:                              # %for.body16.1
                                        #   Parent Loop BB24_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	testq	%r14, %rax
	je	.LBB24_14
# BB#11:                                # %if.then18.1
                                        #   in Loop: Header=BB24_10 Depth=2
	notq	%rax
	movl	16(%rdx), %edx
	andq	%rax, %r14
	movq	$7, %rcx
	shlq	%cl, %rdx
	leaq	64(%rbx,%rdx), %r13
	movq	$32, %rcx
	movq	%r13, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	xorq	%rax, %rax
	movq	%r8, %rdi
	movq	%r8, %r15
	movabsq	$.L.str6, %rsi
	movq	%r13, %rdx
	callq	fprintf
	cmpq	$52, %r12
	jg	.LBB24_13
# BB#12:                                # %if.then25.1
                                        #   in Loop: Header=BB24_10 Depth=2
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	reg_names(,%r13,8), %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str7, %rsi
	callq	fprintf
.LBB24_13:                              # %if.end29.1
                                        #   in Loop: Header=BB24_10 Depth=2
	testq	%r14, %r14
	movq	%r15, %r8
	movq	8(%rsp), %rdx           # 8-byte Reload
	je	.LBB24_15
.LBB24_14:                              # %for.inc.1
                                        #   in Loop: Header=BB24_10 Depth=2
	incq	%rbx
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB24_10
	jmp	.LBB24_15
.LBB24_16:                              # %do.end
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB24_17:                              # %if.then
	movabsq	$.L.str5, %rdi
	movq	$6, %rsi
	movq	$1, %rdx
	movq	%r8, %rcx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fwrite  # TAILCALL
.Ltmp330:
	.size	dump_regset, .Ltmp330-dump_regset
	.cfi_endproc

	.globl	debug_regset
	.align	16, 0x90
	.type	debug_regset,@function
debug_regset:                           # @debug_regset
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp333:
	.cfi_def_cfa_offset 16
.Ltmp334:
	.cfi_offset %rbx, -16
	movq	stderr(%rip), %rbx
	movq	%rbx, %rsi
	callq	dump_regset
	movq	$10, %rdi
	movq	%rbx, %rsi
	popq	%rbx
	jmp	putc  # TAILCALL
.Ltmp335:
	.size	debug_regset, .Ltmp335-debug_regset
	.cfi_endproc

	.globl	recompute_reg_usage
	.align	16, 0x90
	.type	recompute_reg_usage,@function
recompute_reg_usage:                    # @recompute_reg_usage
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp338:
	.cfi_def_cfa_offset 16
.Ltmp339:
	.cfi_offset %rbx, -16
	callq	max_reg_num
	movl	%eax, max_regno(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
	movq	%rax, %rdi
	callq	allocate_reg_info
	movslq	max_regno(%rip), %rax
	testq	%rax, %rax
	jle	.LBB26_3
# BB#1:                                 # %for.body.lr.ph.i
	movq	reg_n_info(%rip), %rax
	movq	$4, %rdx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB26_2:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rdx,8), %rcx
	movl	%ebx, 12(%rcx)
	movl	%ebx, 16(%rcx)
	movl	%ebx, 24(%rcx)
	movl	%ebx, 32(%rcx)
	movl	%ebx, 28(%rcx)
	movl	%r8d, 36(%rcx)
	movslq	max_regno(%rip), %rdi
	leaq	-3(%rdx), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rdx
	cmpq	%rdi, %rsi
	jl	.LBB26_2
.LBB26_3:                               # %allocate_reg_life_data.exit
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rdx
	popq	%rbx
	jmp	update_life_info  # TAILCALL
.Ltmp340:
	.size	recompute_reg_usage, .Ltmp340-recompute_reg_usage
	.cfi_endproc

	.globl	reg_set_to_hard_reg_set
	.align	16, 0x90
	.type	reg_set_to_hard_reg_set,@function
reg_set_to_hard_reg_set:                # @reg_set_to_hard_reg_set
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp345:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp346:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp347:
	.cfi_def_cfa_offset 32
.Ltmp348:
	.cfi_offset %rbx, -32
.Ltmp349:
	.cfi_offset %r14, -24
.Ltmp350:
	.cfi_offset %r15, -16
	movq	(%rsi), %r9
	testq	%r9, %r9
	je	.LBB27_10
# BB#1:
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
.LBB27_2:                               # %for.cond6.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB27_3 Depth 2
                                        #       Child Loop BB27_4 Depth 3
	xorq	%r10, %r10
	xorq	%r11, %r11
	.align	16, 0x90
.LBB27_3:                               # %for.body8
                                        #   Parent Loop BB27_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB27_4 Depth 3
	movq	24(%r9,%r11,8), %r14
	testq	%r14, %r14
	movq	$1, %rdx
	movq	%r10, %r15
	je	.LBB27_8
	.align	16, 0x90
.LBB27_4:                               # %for.body13
                                        #   Parent Loop BB27_2 Depth=1
                                        #     Parent Loop BB27_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1(%rdx), %rcx
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%r14, %rsi
	je	.LBB27_7
# BB#5:                                 # %if.then15
                                        #   in Loop: Header=BB27_4 Depth=3
	movl	16(%r9), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	addq	%r15, %rax
	movq	$32, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	$52, %rbx
	jg	.LBB27_10
# BB#6:                                 # %if.end22
                                        #   in Loop: Header=BB27_4 Depth=3
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	orq	%rbx, (%rdi)
	notq	%rsi
	andq	%rsi, %r14
	testq	%r14, %r14
	je	.LBB27_8
.LBB27_7:                               # %for.inc
                                        #   in Loop: Header=BB27_4 Depth=3
	movq	%rdx, %rax
	andq	%r8, %rax
	incq	%rdx
	incq	%r15
	cmpq	$64, %rax
	jb	.LBB27_4
.LBB27_8:                               # %if.end29
                                        #   in Loop: Header=BB27_3 Depth=2
	incq	%r11
	movq	%r11, %rax
	andq	%r8, %rax
	addq	$64, %r10
	cmpq	$2, %rax
	jb	.LBB27_3
# BB#9:                                 # %for.end32
                                        #   in Loop: Header=BB27_2 Depth=1
	movq	(%r9), %r9
	testq	%r9, %r9
	jne	.LBB27_2
.LBB27_10:                              # %do.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp351:
	.size	reg_set_to_hard_reg_set, .Ltmp351-reg_set_to_hard_reg_set
	.cfi_endproc

	.align	16, 0x90
	.type	invalidate_mems_from_set,@function
invalidate_mems_from_set:               # @invalidate_mems_from_set
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp358:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp359:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp360:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp361:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp362:
	.cfi_def_cfa_offset 48
.Ltmp363:
	.cfi_offset %rbx, -48
.Ltmp364:
	.cfi_offset %r12, -40
.Ltmp365:
	.cfi_offset %r13, -32
.Ltmp366:
	.cfi_offset %r14, -24
.Ltmp367:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	32(%r15), %r13
	movq	%rsi, %r14
	testq	%r13, %r13
	je	.LBB28_8
# BB#1:                                 # %while.body.lr.ph.lr.ph
	xorq	%r12, %r12
.LBB28_3:                               # %while.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB28_6 Depth 2
                                        #     Child Loop BB28_4 Depth 2
	testq	%r12, %r12
	movq	%r13, %rbx
	jne	.LBB28_6
	.align	16, 0x90
.LBB28_4:                               # %while.body.us
                                        #   Parent Loop BB28_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r13
	movq	%r14, %rdi
	callq	reg_overlap_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB28_2
# BB#5:                                 # %if.end.us
                                        #   in Loop: Header=BB28_4 Depth=2
	movq	%r13, 32(%r15)
	movq	%rbx, %rdi
	callq	free_EXPR_LIST_node
	movl	56(%r15), %eax
	decq	%rax
	testq	%r13, %r13
	movl	%eax, 56(%r15)
	movq	%r13, %rbx
	jne	.LBB28_4
	jmp	.LBB28_8
	.align	16, 0x90
.LBB28_6:                               # %while.body
                                        #   Parent Loop BB28_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r13
	movq	%r14, %rdi
	callq	reg_overlap_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB28_2
# BB#7:                                 # %if.end
                                        #   in Loop: Header=BB28_6 Depth=2
	movq	%r13, 16(%r12)
	movq	%rbx, %rdi
	callq	free_EXPR_LIST_node
	movl	56(%r15), %eax
	decq	%rax
	testq	%r13, %r13
	movl	%eax, 56(%r15)
	movq	%r13, %rbx
	jne	.LBB28_6
	jmp	.LBB28_8
	.align	16, 0x90
.LBB28_2:                               # %while.cond.outer.loopexit
                                        #   in Loop: Header=BB28_3 Depth=1
	testq	%r13, %r13
	movq	%rbx, %r12
	jne	.LBB28_3
.LBB28_8:                               # %while.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp368:
	.size	invalidate_mems_from_set, .Ltmp368-invalidate_mems_from_set
	.cfi_endproc

	.align	16, 0x90
	.type	set_phi_alternative_reg,@function
set_phi_alternative_reg:                # @set_phi_alternative_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp370:
	.cfi_def_cfa_offset 16
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	callq	bitmap_set_bit
	xorq	%rax, %rax
	popq	%rdx
	retq
.Ltmp371:
	.size	set_phi_alternative_reg, .Ltmp371-set_phi_alternative_reg
	.cfi_endproc

	.align	16, 0x90
	.type	mark_reg,@function
mark_reg:                               # @mark_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp376:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp377:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp378:
	.cfi_def_cfa_offset 32
.Ltmp379:
	.cfi_offset %rbx, -32
.Ltmp380:
	.cfi_offset %r14, -24
.Ltmp381:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	(%rbx), %eax
	movq	%rsi, %r15
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$3342336, %rax          # imm = 0x330000
	je	.LBB30_19
# BB#1:                                 # %if.end
	movl	8(%rbx), %r14d
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	bitmap_set_bit
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	jg	.LBB30_18
# BB#2:                                 # %if.then2
	movzbl	2(%rbx), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%r14, %rcx
	cmpq	$8, %rcx
	je	.LBB30_6
# BB#3:                                 # %if.then2
	leaq	-21(%r14), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB30_6
# BB#4:                                 # %if.then2
	leaq	-45(%r14), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB30_6
# BB#5:                                 # %if.then2
	leaq	-29(%r14), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB30_6
# BB#11:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB30_12
# BB#14:                                # %cond.true38
	movzbl	target_flags+3(%rip), %ebx
	andq	$2, %rbx
	xorq	$6, %rbx
	jmp	.LBB30_17
.LBB30_6:                               # %cond.true
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$1, %rbx
	je	.LBB30_16
# BB#7:                                 # %lor.rhs
	cmpq	$6, %rax
	jne	.LBB30_8
# BB#9:                                 # %lor.rhs
	movabsq	$1, %rbx
	incq	%rbx
	jmp	.LBB30_17
.LBB30_12:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB30_15
# BB#13:                                # %cond.true31
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	xorq	$3, %rbx
	jmp	.LBB30_17
.LBB30_8:
	movabsq	$0, %rbx
	incq	%rbx
	jmp	.LBB30_17
.LBB30_15:                              # %cond.false42
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %rbx
	jmp	.LBB30_17
.LBB30_19:                              # %if.then
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.mark_reg, %rcx
	movq	$918, %rsi              # imm = 0x396
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB30_16:                              # %while.body
	leaq	(%rbx,%r14), %rsi
	movq	%r15, %rdi
	callq	bitmap_set_bit
.LBB30_17:                              # %while.body
	decq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB30_16
.LBB30_18:                              # %if.end62
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp382:
	.size	mark_reg, .Ltmp382-mark_reg
	.cfi_endproc

	.align	16, 0x90
	.type	notice_stack_pointer_modification_1,@function
notice_stack_pointer_modification_1:    # @notice_stack_pointer_modification_1
	.cfi_startproc
# BB#0:                                 # %entry
	movq	global_rtl+16(%rip), %rax
	cmpq	%rdi, %rax
	je	.LBB31_4
# BB#1:                                 # %lor.lhs.false
	movzbl	(%rdi), %ecx
	cmpq	$66, %rcx
	jne	.LBB31_5
# BB#2:                                 # %land.lhs.true
	movq	8(%rdi), %rcx
	movzbl	(%rcx), %edx
	movzbl	rtx_class(%rdx), %edx
	cmpq	$97, %rdx
	jne	.LBB31_5
# BB#3:                                 # %land.lhs.true7
	movq	8(%rcx), %rcx
	cmpq	%rax, %rcx
	jne	.LBB31_5
.LBB31_4:                               # %if.then
	xorq	%rax, %rax
	movl	%eax, current_function_sp_is_unchanging(%rip)
.LBB31_5:                               # %if.end
	retq
.Ltmp383:
	.size	notice_stack_pointer_modification_1, .Ltmp383-notice_stack_pointer_modification_1
	.cfi_endproc

	.align	16, 0x90
	.type	verify_wide_reg_1,@function
verify_wide_reg_1:                      # @verify_wide_reg_1
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rcx
	movl	(%rcx), %edx
	movq	%rdx, %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	xorq	%rax, %rax
	cmpq	$61, %rdi
	jne	.LBB32_3
# BB#1:                                 # %land.lhs.true
	movl	(%rsi), %esi
	movl	8(%rcx), %ecx
	cmpq	%rsi, %rcx
	jne	.LBB32_3
# BB#2:                                 # %if.then
	movq	$15, %rcx
	shrq	%cl, %rdx
	andq	$510, %rdx              # imm = 0x1FE
	movl	target_flags(%rip), %eax
	movzbl	mode_bitsize(%rdx), %edx
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rax, %rcx
	cmpq	%rdx, %rcx
	sbbq	%rax, %rax
	notq	%rax
	andq	$1, %rax
	incq	%rax
.LBB32_3:                               # %return
	retq
.Ltmp384:
	.size	verify_wide_reg_1, .Ltmp384-verify_wide_reg_1
	.cfi_endproc

	.type	lang_missing_noreturn_ok_p,@object # @lang_missing_noreturn_ok_p
	.comm	lang_missing_noreturn_ok_p,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"function might be possible candidate for attribute `noreturn'"
	.size	.L.str, 62

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"`noreturn' function does return"
	.size	.L.str1, 32

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"control reaches end of non-void function"
	.size	.L.str2, 41

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"flow.c"
	.size	.L.str3, 7

	.type	.L__FUNCTION__.first_insn_after_basic_block_note,@object # @__FUNCTION__.first_insn_after_basic_block_note
.L__FUNCTION__.first_insn_after_basic_block_note:
	.asciz	"first_insn_after_basic_block_note"
	.size	.L__FUNCTION__.first_insn_after_basic_block_note, 34

	.type	elim_reg_set,@object    # @elim_reg_set
	.local	elim_reg_set
	.comm	elim_reg_set,8,8
	.type	.L__FUNCTION__.update_life_info,@object # @__FUNCTION__.update_life_info
.L__FUNCTION__.update_life_info:
	.asciz	"update_life_info"
	.size	.L__FUNCTION__.update_life_info, 17

	.type	reg_n_info,@object      # @reg_n_info
	.comm	reg_n_info,8,8
	.type	regs_live_at_setjmp,@object # @regs_live_at_setjmp
	.comm	regs_live_at_setjmp,8,8
	.type	max_regno,@object       # @max_regno
	.comm	max_regno,4,4
	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"Attempt to delete prologue/epilogue insn:"
	.size	.L.str4, 42

	.type	.L__FUNCTION__.propagate_one_insn,@object # @__FUNCTION__.propagate_one_insn
.L__FUNCTION__.propagate_one_insn:
	.asciz	"propagate_one_insn"
	.size	.L__FUNCTION__.propagate_one_insn, 19

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	" (nil)"
	.size	.L.str5, 7

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	" %d"
	.size	.L.str6, 4

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	" [%s]"
	.size	.L.str7, 6

	.type	flow2_completed,@object # @flow2_completed
	.comm	flow2_completed,4,4
	.type	regset_bytes,@object    # @regset_bytes
	.comm	regset_bytes,4,4
	.type	regset_size,@object     # @regset_size
	.comm	regset_size,4,4
	.type	regs_may_share,@object  # @regs_may_share
	.comm	regs_may_share,8,8
	.type	.L__FUNCTION__.mark_used_regs,@object # @__FUNCTION__.mark_used_regs
.L__FUNCTION__.mark_used_regs:
	.asciz	"mark_used_regs"
	.size	.L__FUNCTION__.mark_used_regs, 15

	.type	.L__FUNCTION__.mark_set_regs,@object # @__FUNCTION__.mark_set_regs
.L__FUNCTION__.mark_set_regs:
	.asciz	"mark_set_regs"
	.size	.L__FUNCTION__.mark_set_regs, 14

	.type	.L__FUNCTION__.mark_reg,@object # @__FUNCTION__.mark_reg
.L__FUNCTION__.mark_reg:
	.asciz	"mark_reg"
	.size	.L__FUNCTION__.mark_reg, 9

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"Dead jumptable %i removed\n"
	.size	.L.str8, 27

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"live_at_start mismatch in bb %d, aborting\nNew:\n"
	.size	.L.str9, 48

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"Old:\n"
	.size	.L.str10, 6

	.type	.L__FUNCTION__.verify_local_live_at_start,@object # @__FUNCTION__.verify_local_live_at_start
.L__FUNCTION__.verify_local_live_at_start:
	.asciz	"verify_local_live_at_start"
	.size	.L__FUNCTION__.verify_local_live_at_start, 27

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"Register %d died unexpectedly.\n"
	.size	.L.str11, 32

	.type	.L__FUNCTION__.verify_wide_reg,@object # @__FUNCTION__.verify_wide_reg
.L__FUNCTION__.verify_wide_reg:
	.asciz	"verify_wide_reg"
	.size	.L__FUNCTION__.verify_wide_reg, 16


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
