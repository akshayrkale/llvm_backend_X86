	.file	"sched-ebb.c.bc"
	.text
	.globl	schedule_ebbs
	.align	16, 0x90
	.type	schedule_ebbs,@function
schedule_ebbs:                          # @schedule_ebbs
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
	movl	n_basic_blocks(%rip), %eax
	movq	%rdi, %rbx
	testq	%rax, %rax
	je	.LBB0_31
# BB#1:                                 # %if.end
	callq	scope_to_insns_initialize
	movq	%rbx, %rdi
	callq	sched_init
	movq	$ebb_sched_info, current_sched_info(%rip)
	callq	allocate_reg_life_data
	callq	get_max_uid
	movq	%rax, %rdi
	callq	compute_bb_for_insn
	movslq	n_basic_blocks(%rip), %rsi
	testq	%rsi, %rsi
	jle	.LBB0_26
# BB#2:                                 # %for.body.lr.ph
	xorq	%r15, %r15
	.align	16, 0x90
.LBB0_3:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_6 Depth 2
                                        #       Child Loop BB0_9 Depth 3
                                        #     Child Loop BB0_16 Depth 2
                                        #       Child Loop BB0_17 Depth 3
                                        #     Child Loop BB0_36 Depth 2
	movq	$32, %rcx
	movq	%r15, %rax
	movq	basic_block_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %r8
	movq	(%r8), %rbx
	movq	8(%r8), %r12
	incq	%r15
	movq	%r15, %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	%rdi, %rax
	andq	%rax, %rcx
	movq	%rsi, %rdi
	andq	%rax, %rdi
	cmpq	%rdi, %rcx
	jne	.LBB0_5
# BB#4:                                 #   in Loop: Header=BB0_3 Depth=1
	movq	%rsi, %r15
	jmp	.LBB0_16
	.align	16, 0x90
.LBB0_5:                                # %lor.lhs.false.lr.ph
                                        #   in Loop: Header=BB0_3 Depth=1
	addq	$32, %rdx
	movq	$32, %rcx
	movq	%r15, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
.LBB0_6:                                # %lor.lhs.false
                                        #   Parent Loop BB0_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_9 Depth 3
	movq	(%rdx,%r14,8), %rcx
	movq	(%rcx), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$36, %rcx
	je	.LBB0_15
# BB#7:                                 # %if.end17
                                        #   in Loop: Header=BB0_6 Depth=2
	movq	40(%r8), %rax
	jmp	.LBB0_9
	.align	16, 0x90
.LBB0_8:                                # %for.cond18
                                        #   in Loop: Header=BB0_9 Depth=3
	movq	8(%rax), %rax
.LBB0_9:                                # %for.cond18
                                        #   Parent Loop BB0_3 Depth=1
                                        #     Parent Loop BB0_6 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rax, %rax
	je	.LBB0_15
# BB#10:                                # %for.body19
                                        #   in Loop: Header=BB0_9 Depth=3
	movzbl	48(%rax), %ecx
	testq	$1, %rcx
	je	.LBB0_8
# BB#11:                                # %if.end25
                                        #   in Loop: Header=BB0_6 Depth=2
	movzbl	(%r12), %eax
	cmpq	$33, %rax
	jne	.LBB0_13
# BB#12:                                # %if.then29
                                        #   in Loop: Header=BB0_6 Depth=2
	movq	$16, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB0_13
# BB#14:                                # %if.then32
                                        #   in Loop: Header=BB0_6 Depth=2
	movq	8(%rax), %rax
	movslq	8(%rax), %rax
	cmpq	$5000, %rax             # imm = 0x1388
	jg	.LBB0_15
.LBB0_13:                               # %for.cond3.backedge
                                        #   in Loop: Header=BB0_6 Depth=2
	movq	basic_block_info(%rip), %rdx
	movq	32(%rdx,%r14,8), %r8
	addq	$32, %rdx
	movq	8(%r8), %r12
	movl	n_basic_blocks(%rip), %ecx
	incq	%r15
	movq	%r15, %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rsi
	incq	%r14
	cmpq	%rcx, %rsi
	jne	.LBB0_6
	jmp	.LBB0_16
	.align	16, 0x90
.LBB0_15:                               # %if.then32.while.cond.preheader.loopexit_crit_edge
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	%r14, %r15
	jmp	.LBB0_16
.LBB0_19:                               # %if.then57
                                        #   in Loop: Header=BB0_16 Depth=2
	movq	16(%r12), %r12
	.align	16, 0x90
.LBB0_16:                               # %while.cond.preheader
                                        #   Parent Loop BB0_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_17 Depth 3
	cmpq	%r12, %rbx
	movq	%rbx, %r13
	je	.LBB0_24
	.align	16, 0x90
.LBB0_17:                               # %while.body
                                        #   Parent Loop BB0_3 Depth=1
                                        #     Parent Loop BB0_16 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	je	.LBB0_22
# BB#18:                                # %if.else
                                        #   in Loop: Header=BB0_17 Depth=3
	movzbl	(%r12), %ecx
	cmpq	$37, %rcx
	je	.LBB0_19
# BB#20:                                # %if.else61
                                        #   in Loop: Header=BB0_17 Depth=3
	cmpq	$36, %rax
	jne	.LBB0_21
.LBB0_22:                               # %while.cond.backedge
                                        #   in Loop: Header=BB0_17 Depth=3
	movq	24(%rbx), %rbx
	cmpq	%r12, %rbx
	jne	.LBB0_17
# BB#23:                                #   in Loop: Header=BB0_3 Depth=1
	movq	%r12, %r13
	movq	%r12, %rbx
	jmp	.LBB0_24
.LBB0_21:                               #   in Loop: Header=BB0_3 Depth=1
	movq	%r12, %r13
	.align	16, 0x90
.LBB0_24:                               # %while.end
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	no_real_insns_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_25
# BB#32:                                # %if.end.i
                                        #   in Loop: Header=BB0_3 Depth=1
	callq	init_deps_global
	leaq	24(%rsp), %r14
	movq	%r14, %rdi
	callq	init_deps
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	sched_analyze
	movq	%r14, %rdi
	callq	free_deps
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	compute_forward_dependences
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	set_priorities
	movq	16(%rbx), %rcx
	movq	current_sched_info(%rip), %rdx
	movq	%rcx, 64(%rdx)
	movq	24(%r13), %rcx
	movq	current_sched_info(%rip), %rdx
	movq	%rcx, 72(%rdx)
	movl	write_symbols(%rip), %ecx
	movq	%rax, %r12
	testq	%rcx, %rcx
	je	.LBB0_34
# BB#33:                                # %if.then5.i
                                        #   in Loop: Header=BB0_3 Depth=1
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	save_line_notes
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	rm_line_notes
.LBB0_34:                               # %if.end6.i
                                        #   in Loop: Header=BB0_3 Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_40
# BB#35:                                # %if.then10.i
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	56(%rbx), %r14
	jmp	.LBB0_36
	.align	16, 0x90
.LBB0_39:                               # %for.inc.i
                                        #   in Loop: Header=BB0_36 Depth=2
	movq	16(%r14), %r14
.LBB0_36:                               # %if.then10.i
                                        #   Parent Loop BB0_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r14, %r14
	je	.LBB0_40
# BB#37:                                # %for.body.i
                                        #   in Loop: Header=BB0_36 Depth=2
	movl	(%r14), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$1572864, %rax          # imm = 0x180000
	jne	.LBB0_39
# BB#38:                                # %if.then19.i
                                        #   in Loop: Header=BB0_36 Depth=2
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	remove_note
	movq	16(%r14), %r14
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	remove_note
	jmp	.LBB0_39
	.align	16, 0x90
.LBB0_40:                               # %if.end27.i
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	rm_other_notes
	movq	current_sched_info(%rip), %rax
	movzbl	96(%rax), %ecx
	orq	$1, %rcx
	movb	%cl, 96(%rax)
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	schedule_block
	movl	sched_n_insns(%rip), %eax
	andq	%rbx, %r12
	cmpq	%r12, %rax
	jne	.LBB0_44
# BB#41:                                # %if.end33.i
                                        #   in Loop: Header=BB0_3 Depth=1
	movl	write_symbols(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_43
# BB#42:                                # %if.then38.i
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	current_sched_info(%rip), %rax
	movq	80(%rax), %rdi
	movq	88(%rax), %rsi
	callq	restore_line_notes
.LBB0_43:                               # %if.end39.i
                                        #   in Loop: Header=BB0_3 Depth=1
	callq	finish_deps_global
.LBB0_25:                               # %for.cond.backedge
                                        #   in Loop: Header=BB0_3 Depth=1
	movslq	n_basic_blocks(%rip), %rsi
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rsi, %rax
	jl	.LBB0_3
.LBB0_26:                               # %for.end76
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_28
# BB#27:                                # %if.then78
	callq	get_insns
	movq	%rax, %rdi
	callq	reposition_prologue_and_epilogue_notes
.LBB0_28:                               # %if.end80
	movl	write_symbols(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_30
# BB#29:                                # %if.then83
	callq	rm_redundant_line_notes
.LBB0_30:                               # %if.end84
	callq	scope_to_insns_finalize
	callq	sched_finish
.LBB0_31:                               # %return
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_44:                               # %if.then32.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.schedule_ebb, %rcx
	movq	$265, %rsi              # imm = 0x109
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp17:
	.size	schedule_ebbs, .Ltmp17-schedule_ebbs
	.cfi_endproc

	.align	16, 0x90
	.type	init_ready_list,@function
init_ready_list:                        # @init_ready_list
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp22:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp23:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp24:
	.cfi_def_cfa_offset 32
.Ltmp25:
	.cfi_offset %rbx, -32
.Ltmp26:
	.cfi_offset %r14, -24
.Ltmp27:
	.cfi_offset %r15, -16
	movq	current_sched_info(%rip), %rax
	movq	64(%rax), %rcx
	movq	72(%rax), %r15
	xorq	%rax, %rax
	movl	%eax, target_n_insns(%rip)
	movl	%eax, sched_n_insns(%rip)
	movq	24(%rcx), %rbx
	movq	%rdi, %r14
	jmp	.LBB1_1
	.align	16, 0x90
.LBB1_8:                                # %for.cond.backedge
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	24(%rbx), %rbx
.LBB1_1:                                # %entry
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r15, %rbx
	je	.LBB1_9
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB1_1 Depth=1
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB1_8
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB1_1 Depth=1
	movslq	8(%rbx), %rcx
	movq	h_i_d(%rip), %rdx
	leaq	(%rcx,%rcx,2), %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movl	24(%rdx,%rsi), %ecx
	testq	%rcx, %rcx
	jne	.LBB1_7
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	24(%rbx), %rcx
	movl	(%rcx), %ecx
	testq	$268435456, %rcx        # imm = 0x10000000
	je	.LBB1_6
# BB#5:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB1_1 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	je	.LBB1_7
.LBB1_6:                                # %if.then26
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	ready_add
	movl	(%rbx), %eax
.LBB1_7:                                # %if.end27
                                        #   in Loop: Header=BB1_1 Depth=1
	testq	$268435456, %rax        # imm = 0x10000000
	jne	.LBB1_8
# BB#10:                                # %if.then31
                                        #   in Loop: Header=BB1_1 Depth=1
	movl	target_n_insns(%rip), %eax
	incq	%rax
	movl	%eax, target_n_insns(%rip)
	movq	24(%rbx), %rbx
	jmp	.LBB1_1
.LBB1_9:                                # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp28:
	.size	init_ready_list, .Ltmp28-init_ready_list
	.cfi_endproc

	.align	16, 0x90
	.type	can_schedule_ready_p,@function
can_schedule_ready_p:                   # @can_schedule_ready_p
	.cfi_startproc
# BB#0:                                 # %entry
	movl	sched_n_insns(%rip), %ecx
	incq	%rcx
	movq	$1, %rax
	movl	%ecx, sched_n_insns(%rip)
	retq
.Ltmp29:
	.size	can_schedule_ready_p, .Ltmp29-can_schedule_ready_p
	.cfi_endproc

	.align	16, 0x90
	.type	schedule_more_p,@function
schedule_more_p:                        # @schedule_more_p
	.cfi_startproc
# BB#0:                                 # %entry
	movslq	sched_n_insns(%rip), %rax
	movslq	target_n_insns(%rip), %rcx
	cmpq	%rcx, %rax
	jge	.LBB3_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB3_1:
	movabsq	$0, %rax
	retq
.Ltmp30:
	.size	schedule_more_p, .Ltmp30-schedule_more_p
	.cfi_endproc

	.align	16, 0x90
	.type	new_ready,@function
new_ready:                              # @new_ready
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rax
	retq
.Ltmp31:
	.size	new_ready, .Ltmp31-new_ready
	.cfi_endproc

	.align	16, 0x90
	.type	rank,@function
rank:                                   # @rank
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp32:
	.size	rank, .Ltmp32-rank
	.cfi_endproc

	.align	16, 0x90
	.type	print_insn,@function
print_insn:                             # @print_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp35:
	.cfi_def_cfa_offset 16
.Ltmp36:
	.cfi_offset %rbx, -16
	movl	8(%rdi), %edx
	movabsq	$print_insn.tmp, %rbx
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	sprintf
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp37:
	.size	print_insn, .Ltmp37-print_insn
	.cfi_endproc

	.align	16, 0x90
	.type	contributes_to_priority,@function
contributes_to_priority:                # @contributes_to_priority
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rax
	retq
.Ltmp38:
	.size	contributes_to_priority, .Ltmp38-contributes_to_priority
	.cfi_endproc

	.align	16, 0x90
	.type	compute_jump_reg_dependencies,@function
compute_jump_reg_dependencies:          # @compute_jump_reg_dependencies
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp42:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp43:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp44:
	.cfi_def_cfa_offset 32
.Ltmp45:
	.cfi_offset %rbx, -24
.Ltmp46:
	.cfi_offset %r14, -16
	movslq	8(%rdi), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movq	40(%rax), %rbx
	movq	%rsi, %r14
	jmp	.LBB8_1
	.align	16, 0x90
.LBB8_4:                                # %for.inc
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	8(%rbx), %rbx
.LBB8_1:                                # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB8_5
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB8_1 Depth=1
	movzbl	48(%rbx), %eax
	testq	$1, %rax
	jne	.LBB8_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	24(%rbx), %rax
	movq	64(%rax), %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	movq	%r14, %rsi
	callq	bitmap_operation
	jmp	.LBB8_4
.LBB8_5:                                # %for.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp47:
	.size	compute_jump_reg_dependencies, .Ltmp47-compute_jump_reg_dependencies
	.cfi_endproc

	.type	sched_n_insns,@object   # @sched_n_insns
	.local	sched_n_insns
	.comm	sched_n_insns,4,4
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"sched-ebb.c"
	.size	.L.str, 12

	.type	.L__FUNCTION__.schedule_ebb,@object # @__FUNCTION__.schedule_ebb
.L__FUNCTION__.schedule_ebb:
	.asciz	"schedule_ebb"
	.size	.L__FUNCTION__.schedule_ebb, 13

	.type	ebb_sched_info,@object  # @ebb_sched_info
	.data
	.align	8
ebb_sched_info:
	.quad	init_ready_list
	.quad	can_schedule_ready_p
	.quad	schedule_more_p
	.quad	new_ready
	.quad	rank
	.quad	print_insn
	.quad	contributes_to_priority
	.quad	compute_jump_reg_dependencies
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.byte	2                       # 0x2
	.zero	7
	.size	ebb_sched_info, 104

	.type	print_insn.tmp,@object  # @print_insn.tmp
	.local	print_insn.tmp
	.comm	print_insn.tmp,80,16
	.type	.L.str1,@object         # @.str1
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str1:
	.asciz	"%4d"
	.size	.L.str1, 4

	.type	target_n_insns,@object  # @target_n_insns
	.local	target_n_insns
	.comm	target_n_insns,4,4

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
