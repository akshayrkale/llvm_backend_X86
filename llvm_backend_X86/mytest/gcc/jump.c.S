	.file	"jump.c.bc"
	.text
	.globl	rebuild_jump_labels
	.align	16, 0x90
	.type	rebuild_jump_labels,@function
rebuild_jump_labels:                    # @rebuild_jump_labels
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
	movq	%rdi, %r14
	testq	%r14, %r14
	je	.LBB0_12
# BB#1:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%r14, %r15
	.align	16, 0x90
.LBB0_2:                                # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_6 Depth 2
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	addq	$-32, %rcx
	andq	%r12, %rcx
	cmpq	$4, %rcx
	ja	.LBB0_11
# BB#3:                                 # %for.body.i
                                        #   in Loop: Header=BB0_2 Depth=1
	jmpq	*.LJTI0_0(,%rcx,8)
.LBB0_5:                                # %if.then20.i
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	56(%r15), %r13
	jmp	.LBB0_6
.LBB0_4:                                # %if.then8.i
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	$0, 64(%r15)
	jmp	.LBB0_11
.LBB0_10:                               # %if.then.i
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	$28, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, 32(%r15)
	jmp	.LBB0_11
.LBB0_9:                                # %if.then42.i
                                        #   in Loop: Header=BB0_6 Depth=2
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	remove_note
	.align	16, 0x90
.LBB0_6:                                # %if.then20.i
                                        #   Parent Loop BB0_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r13, %r13
	je	.LBB0_11
# BB#7:                                 # %for.body26.i
                                        #   in Loop: Header=BB0_6 Depth=2
	movq	%r13, %rbx
	movl	(%rbx), %eax
	movq	16(%rbx), %r13
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$851968, %rax           # imm = 0xD0000
	jne	.LBB0_6
# BB#8:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB0_6 Depth=2
	movq	8(%rbx), %rdi
	movq	32(%r15), %rsi
	callq	reg_mentioned_p
	testq	%r12, %rax
	jne	.LBB0_6
	jmp	.LBB0_9
	.align	16, 0x90
.LBB0_11:                               # %if.end45.i
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	24(%r15), %r15
	testq	%r15, %r15
	jne	.LBB0_2
.LBB0_12:                               # %init_label_info.exit
	movq	%r14, %rdi
	callq	mark_all_labels
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	32(%rax), %rax
	jmp	.LBB0_13
	.align	16, 0x90
.LBB0_16:                               # %for.inc
                                        #   in Loop: Header=BB0_13 Depth=1
	movq	16(%rax), %rax
.LBB0_13:                               # %init_label_info.exit
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB0_17
# BB#14:                                # %for.body
                                        #   in Loop: Header=BB0_13 Depth=1
	movq	8(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$36, %rdx
	jne	.LBB0_16
# BB#15:                                # %if.then
                                        #   in Loop: Header=BB0_13 Depth=1
	movl	32(%rcx), %edx
	incq	%rdx
	movl	%edx, 32(%rcx)
	jmp	.LBB0_16
.LBB0_17:                               # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp16:
	.size	rebuild_jump_labels, .Ltmp16-rebuild_jump_labels
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI0_0:
	.quad	.LBB0_5
	.quad	.LBB0_4
	.quad	.LBB0_5
	.quad	.LBB0_11
	.quad	.LBB0_10

	.text
	.align	16, 0x90
	.type	mark_all_labels,@function
mark_all_labels:                        # @mark_all_labels
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp21:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp22:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp23:
	.cfi_def_cfa_offset 32
.Ltmp24:
	.cfi_offset %rbx, -32
.Ltmp25:
	.cfi_offset %r14, -24
.Ltmp26:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	jmp	.LBB1_1
	.align	16, 0x90
.LBB1_11:                               # %for.inc
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	24(%rbx), %rbx
.LBB1_1:                                # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB1_12
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB1_1 Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %ecx
	cmpq	$105, %rcx
	jne	.LBB1_11
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	32(%rbx), %rdi
	cmpq	$34, %rax
	jne	.LBB1_7
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB1_1 Depth=1
	movzbl	(%rdi), %eax
	cmpq	$141, %rax
	jne	.LBB1_7
# BB#5:                                 # %if.then11
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	8(%rdi), %rdi
	callq	mark_all_labels
	movq	32(%rbx), %rax
	movq	16(%rax), %rdi
	callq	mark_all_labels
	movq	32(%rbx), %rax
	movq	24(%rax), %rdi
	callq	mark_all_labels
	movq	32(%rbx), %rax
	movq	32(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB1_11
# BB#6:                                 # %if.then37
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	$67, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_u00
	movq	%rax, %r14
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	mark_jump_label
	movq	8(%r14), %rax
	movq	32(%rbx), %rcx
	movq	%rax, 32(%rcx)
	movq	24(%rbx), %rbx
	jmp	.LBB1_1
	.align	16, 0x90
.LBB1_7:                                # %if.end53
                                        #   in Loop: Header=BB1_1 Depth=1
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	mark_jump_label
	movl	(%rbx), %eax
	andq	$134283263, %rax        # imm = 0x800FFFF
	cmpq	$33, %rax
	jne	.LBB1_11
# BB#8:                                 # %if.then65
                                        #   in Loop: Header=BB1_1 Depth=1
	cmpq	$0, 64(%rbx)
	jne	.LBB1_11
# BB#9:                                 # %if.then71
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	$13, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB1_11
# BB#10:                                # %if.then74
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	8(%r14), %rdx
	movq	$67, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_u00
	movq	%rax, %r15
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	mark_jump_label
	movq	8(%r15), %rax
	movq	%rax, 8(%r14)
	movq	%rax, 64(%rbx)
	jmp	.LBB1_11
.LBB1_12:                               # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp27:
	.size	mark_all_labels, .Ltmp27-mark_all_labels
	.cfi_endproc

	.globl	cleanup_barriers
	.align	16, 0x90
	.type	cleanup_barriers,@function
cleanup_barriers:                       # @cleanup_barriers
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp31:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp32:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp33:
	.cfi_def_cfa_offset 32
.Ltmp34:
	.cfi_offset %rbx, -24
.Ltmp35:
	.cfi_offset %r14, -16
	callq	get_insns
	movq	%rax, %r14
	jmp	.LBB2_1
.LBB2_5:                                # %delete_barrier.exit
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%rbx, %rdi
	callq	delete_insn
	.align	16, 0x90
.LBB2_1:                                # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%r14, %r14
	je	.LBB2_8
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%r14, %rbx
	movzbl	(%rbx), %eax
	movq	24(%rbx), %r14
	cmpq	$35, %rax
	jne	.LBB2_1
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%rbx, %rdi
	callq	prev_nonnote_insn
	movzbl	(%rax), %ecx
	cmpq	$35, %rcx
	jne	.LBB2_6
# BB#4:                                 # %if.then5
                                        #   in Loop: Header=BB2_1 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$35, %rax
	je	.LBB2_5
	jmp	.LBB2_9
.LBB2_6:                                # %if.else
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	16(%rbx), %rcx
	cmpq	%rcx, %rax
	je	.LBB2_1
# BB#7:                                 # %if.then10
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	reorder_insns
	jmp	.LBB2_1
.LBB2_8:                                # %for.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB2_9:                                # %if.then.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.delete_barrier, %rcx
	movq	$1531, %rsi             # imm = 0x5FB
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp36:
	.size	cleanup_barriers, .Ltmp36-cleanup_barriers
	.cfi_endproc

	.globl	delete_barrier
	.align	16, 0x90
	.type	delete_barrier,@function
delete_barrier:                         # @delete_barrier
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp38:
	.cfi_def_cfa_offset 16
	movzbl	(%rdi), %eax
	cmpq	$35, %rax
	jne	.LBB3_1
# BB#2:                                 # %if.end
	popq	%rax
	jmp	delete_insn  # TAILCALL
.LBB3_1:                                # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.delete_barrier, %rcx
	movq	$1531, %rsi             # imm = 0x5FB
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp39:
	.size	delete_barrier, .Ltmp39-delete_barrier
	.cfi_endproc

	.globl	copy_loop_headers
	.align	16, 0x90
	.type	copy_loop_headers,@function
copy_loop_headers:                      # @copy_loop_headers
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp46:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp47:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp48:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp49:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp50:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp51:
	.cfi_def_cfa_offset 128
.Ltmp52:
	.cfi_offset %rbx, -48
.Ltmp53:
	.cfi_offset %r12, -40
.Ltmp54:
	.cfi_offset %r13, -32
.Ltmp55:
	.cfi_offset %r14, -24
.Ltmp56:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	testq	%r13, %r13
	je	.LBB4_6
# BB#1:                                 # %for.body.lr.ph
	movzbl	rtx_class+33(%rip), %r15d
	movzbl	rtx_class+32(%rip), %eax
	movabsq	$4294967200, %r12       # imm = 0xFFFFFFA0
	andq	$255, %r15
	andq	$255, %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB4_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_22 Depth 2
                                        #       Child Loop BB4_30 Depth 3
                                        #     Child Loop BB4_58 Depth 2
                                        #     Child Loop BB4_40 Depth 2
                                        #       Child Loop BB4_52 Depth 3
                                        #     Child Loop BB4_60 Depth 2
                                        #       Child Loop BB4_67 Depth 3
	movzbl	(%r13), %eax
	movq	24(%r13), %rbx
	cmpq	$37, %rax
	jne	.LBB4_5
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB4_2 Depth=1
	movl	40(%r13), %eax
	cmpq	%r12, %rax
	jne	.LBB4_5
# BB#4:                                 # %land.lhs.true4
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%r13, %rdi
	callq	next_nonnote_insn
	movq	%rax, %rcx
	testq	%rcx, %rcx
	je	.LBB4_5
# BB#7:                                 # %land.lhs.true6
                                        #   in Loop: Header=BB4_2 Depth=1
	movzbl	(%rcx), %eax
	cmpq	$33, %rax
	jne	.LBB4_5
# BB#8:                                 # %if.end.i.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	32(%rcx), %rax
	movl	(%rax), %edi
	movq	%rdi, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$39, %rdx
	movq	%rax, %rsi
	jne	.LBB4_10
# BB#9:                                 # %if.then4.i.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	8(%rax), %rsi
	movq	8(%rsi), %rsi
	movl	(%rsi), %edi
.LBB4_10:                               # %if.end8.i.i
                                        #   in Loop: Header=BB4_2 Depth=1
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$47, %rdi
	jne	.LBB4_5
# BB#11:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	8(%rsi), %rdi
	movzbl	(%rdi), %edi
	cmpq	$59, %rdi
	jne	.LBB4_5
# BB#12:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB4_2 Depth=1
	testq	%rsi, %rsi
	je	.LBB4_5
# BB#13:                                # %any_uncondjump_p.exit
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	16(%rsi), %rsi
	movzbl	(%rsi), %esi
	cmpq	$67, %rsi
	jne	.LBB4_5
# BB#14:                                # %any_uncondjump_p.exit
                                        #   in Loop: Header=BB4_2 Depth=1
	cmpq	$105, %r15
	jne	.LBB4_5
# BB#15:                                # %cond.true.i51
                                        #   in Loop: Header=BB4_2 Depth=1
	cmpq	$47, %rdx
	je	.LBB4_17
# BB#16:                                # %cond.false.i53
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB4_17:                               # %cond.end18.i
                                        #   in Loop: Header=BB4_2 Depth=1
	testq	%rax, %rax
	je	.LBB4_5
# BB#18:                                # %if.end23.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	8(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$59, %rcx
	jne	.LBB4_5
# BB#19:                                # %onlyjump_p.exit
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	16(%rax), %rdi
	callq	side_effects_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB4_5
# BB#20:                                # %if.then
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	16(%r13), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	callq	next_nonnote_insn
	movq	64(%rax), %rax
	movq	24(%rax), %r14
	callq	max_reg_num
	testq	%r14, %r14
	je	.LBB4_5
# BB#21:                                #   in Loop: Header=BB4_2 Depth=1
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	%r14, %rax
                                        # kill: R14<def> RAX<kill>
	movq	%rax, 40(%rsp)          # 8-byte Spill
.LBB4_22:                               # %land.rhs.i
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB4_30 Depth 3
	movzbl	(%r14), %eax
	addq	$-32, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$5, %rax
	ja	.LBB4_35
# BB#23:                                # %land.rhs.i
                                        #   in Loop: Header=BB4_22 Depth=2
	jmpq	*.LJTI4_0(,%rax,8)
.LBB4_24:                               # %land.rhs5.i
                                        #   in Loop: Header=BB4_22 Depth=2
	movl	40(%r14), %eax
	cmpq	%r12, %rax
	je	.LBB4_5
# BB#25:                                # %land.rhs5.i
                                        #   in Loop: Header=BB4_22 Depth=2
	movabsq	$4294967201, %rcx       # imm = 0xFFFFFFA1
	cmpq	%rcx, %rax
	je	.LBB4_36
# BB#26:                                # %land.rhs5.i
                                        #   in Loop: Header=BB4_22 Depth=2
	movabsq	$4294967202, %rcx       # imm = 0xFFFFFFA2
	cmpq	%rcx, %rax
	je	.LBB4_5
# BB#27:                                # %if.end.i34
                                        #   in Loop: Header=BB4_22 Depth=2
	movslq	optimize(%rip), %rcx
	cmpq	$1, %rcx
	jg	.LBB4_35
# BB#28:                                # %if.end.i34
                                        #   in Loop: Header=BB4_22 Depth=2
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rcx, %rax
	movabsq	$4294967198, %rcx       # imm = 0xFFFFFF9E
	cmpq	%rcx, %rax
	jne	.LBB4_35
	jmp	.LBB4_5
	.align	16, 0x90
.LBB4_29:                               # %while.body.i
                                        #   in Loop: Header=BB4_30 Depth=3
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	remove_note
.LBB4_30:                               # %while.body.i
                                        #   Parent Loop BB4_2 Depth=1
                                        #     Parent Loop BB4_22 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$5, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB4_29
# BB#31:                                # %while.end.i
                                        #   in Loop: Header=BB4_22 Depth=2
	movq	$32, %rcx
	movq	64(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$19, %rax
	jg	.LBB4_5
# BB#32:                                # %lor.lhs.false37.i
                                        #   in Loop: Header=BB4_22 Depth=2
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB4_5
# BB#33:                                # %lor.lhs.false40.i
                                        #   in Loop: Header=BB4_22 Depth=2
	movq	$7, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB4_5
# BB#34:                                #   in Loop: Header=BB4_22 Depth=2
	incq	64(%rsp)                # 8-byte Folded Spill
.LBB4_35:                               # %for.inc.i
                                        #   in Loop: Header=BB4_22 Depth=2
	movq	24(%r14), %r14
	testq	%r14, %r14
	jne	.LBB4_22
	jmp	.LBB4_5
.LBB4_36:                               # %for.end.i
                                        #   in Loop: Header=BB4_2 Depth=1
	testq	%r14, %r14
	je	.LBB4_5
# BB#37:                                # %for.cond51.preheader.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	%r14, %rax
	cmpq	%rax, %rbx
	jne	.LBB4_39
# BB#38:                                # %for.end173.thread.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	callq	gen_label_rtx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rbx, %rdi
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	jmp	.LBB4_115
.LBB4_39:                               # %for.body53.lr.ph.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	$32, %rcx
	movq	48(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	cmpq	$105, 72(%rsp)          # 8-byte Folded Reload
	movq	%rbx, %rdi
	movq	%rbx, %rcx
	movq	%rax, %rbx
	movq	%rcx, %rax
	jne	.LBB4_58
.LBB4_40:                               # %for.body53.i.us
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB4_52 Depth 3
	movzbl	(%rdi), %eax
	cmpq	$32, %rax
	jne	.LBB4_57
# BB#41:                                # %cond.true.i.us
                                        #   in Loop: Header=BB4_40 Depth=2
	movq	32(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB4_43
# BB#42:                                #   in Loop: Header=BB4_40 Depth=2
	movq	%rdi, %r14
	jmp	.LBB4_44
.LBB4_43:                               # %cond.false.i.us
                                        #   in Loop: Header=BB4_40 Depth=2
	movq	%rdi, %r14
	movq	%rax, %rsi
	callq	single_set_2
.LBB4_44:                               # %cond.end79.i.us
                                        #   in Loop: Header=BB4_40 Depth=2
	testq	%rax, %rax
	jne	.LBB4_46
# BB#45:                                #   in Loop: Header=BB4_40 Depth=2
	movq	%r14, %rdi
	jmp	.LBB4_57
.LBB4_46:                               # %land.lhs.true83.i.us
                                        #   in Loop: Header=BB4_40 Depth=2
	movq	8(%rax), %rcx
	movl	(%rcx), %r8d
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB4_48
# BB#47:                                #   in Loop: Header=BB4_40 Depth=2
	movq	%r14, %rdi
	jmp	.LBB4_50
.LBB4_48:                               # %land.lhs.true83.i.us
                                        #   in Loop: Header=BB4_40 Depth=2
	cmpq	$63, %rax
	movq	%r14, %rdi
	jne	.LBB4_57
# BB#49:                                # %land.lhs.true96.i.us
                                        #   in Loop: Header=BB4_40 Depth=2
	movq	8(%rcx), %rcx
	movl	(%rcx), %r8d
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB4_57
.LBB4_50:                               # %land.lhs.true104.i.us
                                        #   in Loop: Header=BB4_40 Depth=2
	movl	8(%rcx), %eax
	cmpq	$53, %rax
	jb	.LBB4_57
# BB#51:                                # %land.lhs.true109.i.us
                                        #   in Loop: Header=BB4_40 Depth=2
	movq	%rcx, %r14
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	(%rax), %ecx
	movl	8(%rdi), %esi
	cmpq	%rsi, %rcx
	movq	%rdi, %rcx
	jne	.LBB4_57
.LBB4_52:                               # %for.cond125.i.us
                                        #   Parent Loop BB4_2 Depth=1
                                        #     Parent Loop BB4_40 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	24(%rcx), %rcx
	cmpq	%rbx, %rcx
	je	.LBB4_57
# BB#53:                                # %for.body128.i.us
                                        #   in Loop: Header=BB4_52 Depth=3
	movl	4(%rax), %esi
	movl	8(%rcx), %edx
	cmpq	%rdx, %rsi
	jne	.LBB4_52
# BB#54:                                # %if.then150.i.us
                                        #   in Loop: Header=BB4_40 Depth=2
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	jne	.LBB4_56
# BB#55:                                # %if.then153.i.us
                                        #   in Loop: Header=BB4_40 Depth=2
	movq	$8, %rsi
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	xcalloc
	movl	(%r14), %r8d
	movq	%rax, 64(%rsp)          # 8-byte Spill
.LBB4_56:                               # %if.end156.i.us
                                        #   in Loop: Header=BB4_40 Depth=2
	movq	%r8, %rax
	orq	$268435456, %rax        # imm = 0x10000000
	movl	%eax, (%r14)
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	movq	%r8, %rdi
	callq	gen_reg_rtx
	movl	8(%r14), %ecx
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	%rax, (%rdx,%rcx,8)
	movq	16(%rsp), %rdi          # 8-byte Reload
.LBB4_57:                               # %for.inc169.i.us
                                        #   in Loop: Header=BB4_40 Depth=2
	movq	24(%rdi), %rdi
	cmpq	%rbx, %rdi
	jne	.LBB4_40
	jmp	.LBB4_59
.LBB4_58:                               # %for.inc169.i
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rax), %rax
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	cmpq	%rbx, %rax
	jne	.LBB4_58
.LBB4_59:                               # %for.body178.lr.ph.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	callq	gen_label_rtx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	40(%rsp), %rcx          # 8-byte Reload
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%rcx, %r14
.LBB4_60:                               # %for.body178.i
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB4_67 Depth 3
	movzbl	(%r14), %edx
	addq	$-32, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$5, %rdx
	ja	.LBB4_106
# BB#61:                                # %for.body178.i
                                        #   in Loop: Header=BB4_60 Depth=2
	jmpq	*.LJTI4_1(,%rdx,8)
.LBB4_64:                               # %sw.bb200.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	32(%r14), %rdi
	movq	%r14, 16(%rsp)          # 8-byte Spill
	callq	copy_insn
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	emit_insn_before
	movq	64(%rsp), %rsi          # 8-byte Reload
	testq	%rsi, %rsi
	je	.LBB4_66
# BB#65:                                # %if.then207.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	32(%rax), %rdi
	movq	$1, %rcx
	movq	%rax, %rbx
	movq	48(%rsp), %rdx          # 8-byte Reload
	callq	replace_regs
	movq	%rbx, %rax
.LBB4_66:                               # %if.end212.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	32(%rax), %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	movq	%rsi, %r14
	callq	mark_jump_label
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	56(%rax), %rbx
	jmp	.LBB4_67
.LBB4_80:                               # %sw.bb280.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	32(%r14), %rdi
	callq	copy_insn
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	emit_jump_insn_before
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB4_82
# BB#81:                                # %if.then287.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	32(%rax), %rdi
	movq	$1, %rcx
	movq	%rax, %rbx
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	callq	replace_regs
	movq	%rbx, %rax
.LBB4_82:                               # %if.end292.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	32(%rax), %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rbx
	movq	%rbx, %rsi
	callq	mark_jump_label
	movq	56(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB4_85
# BB#83:                                # %if.then300.i
                                        #   in Loop: Header=BB4_60 Depth=2
	callq	copy_insn_1
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	movq	%rax, 56(%rbx)
	je	.LBB4_85
# BB#84:                                # %if.then309.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	$1, %rcx
	movq	%rax, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	callq	replace_regs
.LBB4_85:                               # %if.end315.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movzbl	(%rbx), %eax
	cmpq	$33, %rax
	movq	%rbx, %rax
	jne	.LBB4_86
# BB#87:                                # %if.end.i.i.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	%r14, %rdi
	movq	32(%rax), %rsi
	movl	(%rsi), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$39, %rdx
	jne	.LBB4_89
# BB#88:                                # %if.then4.i.i.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	8(%rsi), %rcx
	movq	8(%rcx), %rsi
	movl	(%rsi), %ecx
.LBB4_89:                               # %if.end8.i.i.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	%rdi, %r14
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$47, %rcx
	movq	32(%rsp), %rbx          # 8-byte Reload
	jne	.LBB4_108
# BB#90:                                # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	8(%rsi), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$59, %rcx
	jne	.LBB4_108
# BB#91:                                # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB4_60 Depth=2
	testq	%rsi, %rsi
	je	.LBB4_108
# BB#92:                                # %if.end.i.i40
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	16(%rsi), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$72, %rcx
	jne	.LBB4_108
# BB#93:                                # %if.end2.i.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movq	%rax, %rbx
	movq	24(%rdx), %rcx
	movq	16(%rdx), %rdx
	movzbl	(%rcx), %eax
	movzbl	(%rdx), %ecx
	cmpq	$59, %rax
	jne	.LBB4_96
# BB#94:                                # %land.lhs.true.i.i44
                                        #   in Loop: Header=BB4_60 Depth=2
	cmpq	$51, %rcx
	je	.LBB4_100
# BB#95:                                # %land.lhs.true.i.i44
                                        #   in Loop: Header=BB4_60 Depth=2
	cmpq	$67, %rcx
	jne	.LBB4_97
	jmp	.LBB4_100
.LBB4_107:                              # %sw.bb181.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	%r13, %rdi
	callq	emit_barrier_before
.LBB4_108:                              # %sw.epilog339.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	24(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB4_109
.LBB4_62:                               # %sw.bb183.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movslq	40(%r14), %rdi
	testq	%rdi, %rdi
	movq	24(%rsp), %rdx          # 8-byte Reload
	js	.LBB4_109
# BB#63:                                # %if.then189.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	%r13, %rsi
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	%rdx, %rbx
	callq	emit_note_before
	movq	32(%r14), %rcx
	movq	%rbx, %rdx
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	%rcx, 32(%rax)
	jmp	.LBB4_109
.LBB4_73:                               # %for.inc263.i
                                        #   in Loop: Header=BB4_67 Depth=3
	movq	16(%rbx), %rbx
.LBB4_67:                               # %if.end212.i
                                        #   Parent Loop BB4_2 Depth=1
                                        #     Parent Loop BB4_60 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB4_74
# BB#68:                                # %for.body221.i
                                        #   in Loop: Header=BB4_67 Depth=3
	movl	(%rbx), %eax
	movq	$16, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	cmpq	$13, %rsi
	je	.LBB4_73
# BB#69:                                # %if.then227.i
                                        #   in Loop: Header=BB4_67 Depth=3
	movq	8(%rbx), %rdx
	movq	56(%r14), %rcx
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$3, %rax
	jne	.LBB4_71
# BB#70:                                # %if.then232.i
                                        #   in Loop: Header=BB4_67 Depth=3
	movq	$3, %rdi
	callq	gen_rtx_fmt_ee
	jmp	.LBB4_72
.LBB4_71:                               # %if.else.i
                                        #   in Loop: Header=BB4_67 Depth=3
	movq	$4, %rdi
	callq	gen_rtx_fmt_ue
.LBB4_72:                               # %for.inc263.i
                                        #   in Loop: Header=BB4_67 Depth=3
	movq	%rax, %rdi
	callq	copy_insn_1
	movq	%rax, 56(%r14)
	jmp	.LBB4_73
.LBB4_74:                               # %for.end267.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	64(%rsp), %rdx          # 8-byte Reload
	testq	%rdx, %rdx
	je	.LBB4_75
# BB#76:                                # %land.lhs.true269.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	%r14, %rax
	movq	56(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB4_77
# BB#79:                                # %if.then274.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	$1, %rcx
	movq	%rax, %rbx
	movq	%rdx, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	callq	replace_regs
	movq	%rbx, %rax
	jmp	.LBB4_77
.LBB4_75:                               #   in Loop: Header=BB4_60 Depth=2
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%r14, %rax
	jmp	.LBB4_78
.LBB4_86:                               #   in Loop: Header=BB4_60 Depth=2
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	32(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB4_109
.LBB4_96:                               # %lor.rhs.i.i
                                        #   in Loop: Header=BB4_60 Depth=2
	cmpq	$59, %rcx
	jne	.LBB4_97
# BB#98:                                # %land.rhs.i.i
                                        #   in Loop: Header=BB4_60 Depth=2
	cmpq	$51, %rax
	je	.LBB4_100
# BB#99:                                # %land.rhs.i.i
                                        #   in Loop: Header=BB4_60 Depth=2
	cmpq	$67, %rax
	jne	.LBB4_97
.LBB4_100:                              # %if.then318.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	64(%rbx), %r14
	testq	%r14, %r14
	jne	.LBB4_101
.LBB4_97:                               #   in Loop: Header=BB4_60 Depth=2
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, %rax
	jmp	.LBB4_78
.LBB4_101:                              # %if.then323.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	%r14, %rdi
	callq	prev_nonnote_insn
	testq	%rax, %rax
	je	.LBB4_104
# BB#102:                               # %land.lhs.true326.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	%r14, %rdi
	callq	prev_nonnote_insn
	movq	%rax, %rdi
	callq	prev_nonnote_insn
	movq	%rax, %r14
	movq	%r13, %rdi
	callq	next_nonnote_insn
	cmpq	%rax, %r14
	jne	.LBB4_104
# BB#103:                               # %if.then332.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	$12, %rsi
	movq	$1, %rdx
	movq	%rbx, %rdi
	callq	predict_insn_def
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	redirect_jump
	movq	%rbx, %rax
	jmp	.LBB4_77
.LBB4_104:                              # %if.else334.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	$12, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	predict_insn_def
	movq	%rbx, %rax
.LBB4_77:                               #   in Loop: Header=BB4_60 Depth=2
	movq	24(%rsp), %rdx          # 8-byte Reload
.LBB4_78:                               # %sw.epilog339.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	16(%rsp), %r14          # 8-byte Reload
	movq	32(%rsp), %rbx          # 8-byte Reload
.LBB4_109:                              # %sw.epilog339.i
                                        #   in Loop: Header=BB4_60 Depth=2
	testq	%rbx, %rbx
	movq	%rax, %rcx
	je	.LBB4_111
# BB#110:                               # %sw.epilog339.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	%rbx, %rcx
.LBB4_111:                              # %sw.epilog339.i
                                        #   in Loop: Header=BB4_60 Depth=2
	movq	24(%r14), %r14
	cmpq	%rdx, %r14
	movq	%rcx, %rbx
	jne	.LBB4_60
# BB#112:                               # %for.end347.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB4_114
# BB#113:                               #   in Loop: Header=BB4_2 Depth=1
	movq	24(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB4_115
.LBB4_114:                              # %lor.lhs.false349.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$35, %rcx
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %r14
	movq	40(%rsp), %rbx          # 8-byte Reload
	je	.LBB4_121
.LBB4_115:                              # %if.then354.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%rdi, %r14
	callq	next_nonnote_insn
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB4_117
# BB#116:                               # %lor.lhs.false.i.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$36, %rax
	je	.LBB4_118
.LBB4_117:                              # %if.then.i.i
                                        #   in Loop: Header=BB4_2 Depth=1
	callq	gen_label_rtx
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	emit_label_after
	xorq	%rax, %rax
	movl	%eax, 32(%rbx)
.LBB4_118:                              # %get_label_after.exit.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%rbx, %rdi
	callq	gen_jump
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	emit_jump_insn_before
	movq	32(%rsp), %rcx          # 8-byte Reload
	testq	%rcx, %rcx
	movq	%rax, %rbx
	je	.LBB4_120
# BB#119:                               # %get_label_after.exit.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%rcx, %rbx
.LBB4_120:                              # %get_label_after.exit.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	32(%rax), %rdi
	xorq	%rdx, %rdx
	movq	%rax, %r14
	movq	%r14, %rsi
	callq	mark_jump_label
	movq	%r13, %rdi
	callq	emit_barrier_before
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rbx          # 8-byte Reload
.LBB4_121:                              # %if.end365.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r13, %rsi
	callq	emit_label_before
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	callq	reg_scan_update
	movabsq	$4294967203, %rdi       # imm = 0xFFFFFFA3
	movq	%rbx, %rsi
	callq	emit_note_before
	movq	%r13, %rdi
	callq	next_nonnote_insn
	movq	%rax, %rdi
	callq	delete_related_insns
	movq	64(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB4_123
# BB#122:                               # %if.then371.i
                                        #   in Loop: Header=BB4_2 Depth=1
	callq	free
.LBB4_123:                              # %if.then17
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	24(%rax), %rbx
	.align	16, 0x90
.LBB4_5:                                # %for.cond.backedge
                                        #   in Loop: Header=BB4_2 Depth=1
	testq	%rbx, %rbx
	movq	%rbx, %r13
	jne	.LBB4_2
.LBB4_6:                                # %for.end
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB4_106:                              # %sw.default338.i
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.duplicate_loop_exit_test, %rdx
	movq	$487, %rsi              # imm = 0x1E7
	callq	fancy_abort
.Ltmp57:
	.size	copy_loop_headers, .Ltmp57-copy_loop_headers
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI4_0:
	.quad	.LBB4_30
	.quad	.LBB4_30
	.quad	.LBB4_5
	.quad	.LBB4_35
	.quad	.LBB4_5
	.quad	.LBB4_24
.LJTI4_1:
	.quad	.LBB4_64
	.quad	.LBB4_80
	.quad	.LBB4_106
	.quad	.LBB4_107
	.quad	.LBB4_106
	.quad	.LBB4_62

	.text
	.globl	any_uncondjump_p
	.align	16, 0x90
	.type	any_uncondjump_p,@function
any_uncondjump_p:                       # @any_uncondjump_p
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %ecx
	xorq	%rax, %rax
	cmpq	$33, %rcx
	jne	.LBB5_9
# BB#1:                                 # %if.end.i
	movq	32(%rdi), %rcx
	movl	(%rcx), %edx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$39, %rsi
	jne	.LBB5_3
# BB#2:                                 # %if.then4.i
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rcx
	movl	(%rcx), %edx
.LBB5_3:                                # %if.end8.i
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$47, %rdx
	jne	.LBB5_9
# BB#4:                                 # %land.lhs.true.i
	movq	8(%rcx), %rdx
	movzbl	(%rdx), %edx
	cmpq	$59, %rdx
	jne	.LBB5_9
# BB#5:                                 # %land.lhs.true.i
	testq	%rcx, %rcx
	je	.LBB5_9
# BB#6:                                 # %if.end
	movq	16(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$67, %rax
	jne	.LBB5_7
# BB#8:                                 # %if.end
	movabsq	$1, %rax
.LBB5_9:                                # %return
	retq
.LBB5_7:
	movabsq	$0, %rax
	retq
.Ltmp58:
	.size	any_uncondjump_p, .Ltmp58-any_uncondjump_p
	.cfi_endproc

	.globl	onlyjump_p
	.align	16, 0x90
	.type	onlyjump_p,@function
onlyjump_p:                             # @onlyjump_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp61:
	.cfi_def_cfa_offset 16
.Ltmp62:
	.cfi_offset %rbx, -16
	movzbl	(%rdi), %eax
	xorq	%rbx, %rbx
	cmpq	$33, %rax
	jne	.LBB6_9
# BB#1:                                 # %entry
	movzbl	rtx_class+33(%rip), %eax
	andq	$255, %rax
	cmpq	$105, %rax
	jne	.LBB6_9
# BB#2:                                 # %cond.true
	movq	32(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB6_4
# BB#3:                                 # %cond.false
	movq	%rax, %rsi
	callq	single_set_2
.LBB6_4:                                # %cond.end18
	testq	%rax, %rax
	je	.LBB6_9
# BB#5:                                 # %if.end23
	movq	8(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$59, %rcx
	jne	.LBB6_9
# BB#6:                                 # %if.end32
	movq	16(%rax), %rdi
	callq	side_effects_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB6_7
# BB#8:                                 # %if.end32
	movabsq	$1, %rbx
	jmp	.LBB6_9
.LBB6_7:
	movabsq	$0, %rbx
.LBB6_9:                                # %return
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp63:
	.size	onlyjump_p, .Ltmp63-onlyjump_p
	.cfi_endproc

	.globl	purge_line_number_notes
	.align	16, 0x90
	.type	purge_line_number_notes,@function
purge_line_number_notes:                # @purge_line_number_notes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp68:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp69:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp70:
	.cfi_def_cfa_offset 32
.Ltmp71:
	.cfi_offset %rbx, -32
.Ltmp72:
	.cfi_offset %r14, -24
.Ltmp73:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB7_11
# BB#1:
	xorq	%r15, %r15
	movabsq	$4294967209, %r14       # imm = 0xFFFFFFA9
	.align	16, 0x90
.LBB7_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB7_9
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB7_2 Depth=1
	movl	40(%rbx), %eax
	xorq	%rcx, %rcx
	cmpq	%r14, %rax
	je	.LBB7_10
# BB#4:                                 # %if.else
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB7_9
# BB#5:                                 # %if.then7
                                        #   in Loop: Header=BB7_2 Depth=1
	testq	%r15, %r15
	movq	%rbx, %rcx
	je	.LBB7_10
# BB#6:                                 # %land.lhs.true
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	32(%rbx), %rcx
	movq	32(%r15), %rdx
	cmpq	%rdx, %rcx
	movq	%rbx, %rcx
	jne	.LBB7_10
# BB#7:                                 # %land.lhs.true15
                                        #   in Loop: Header=BB7_2 Depth=1
	movl	40(%r15), %ecx
	cmpq	%rcx, %rax
	movq	%rbx, %rcx
	jne	.LBB7_10
# BB#8:                                 # %if.then23
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	%rbx, %rdi
	callq	delete_related_insns
	.align	16, 0x90
.LBB7_9:                                # %for.inc
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	%r15, %rcx
.LBB7_10:                               # %for.inc
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	movq	%rcx, %r15
	jne	.LBB7_2
.LBB7_11:                               # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp74:
	.size	purge_line_number_notes, .Ltmp74-purge_line_number_notes
	.cfi_endproc

	.globl	delete_related_insns
	.align	16, 0x90
	.type	delete_related_insns,@function
delete_related_insns:                   # @delete_related_insns
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp81:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp82:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp83:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp84:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp85:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp86:
	.cfi_def_cfa_offset 64
.Ltmp87:
	.cfi_offset %rbx, -48
.Ltmp88:
	.cfi_offset %r12, -40
.Ltmp89:
	.cfi_offset %r13, -32
.Ltmp90:
	.cfi_offset %r14, -24
.Ltmp91:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	(%r14), %r13d
	movq	24(%r14), %rax
	movq	16(%r14), %r15
	xorq	%rbx, %rbx
	testq	%rax, %rax
	movabsq	$0, %r12
	je	.LBB8_5
# BB#1:
	movabsq	$0, %r12
	.align	16, 0x90
.LBB8_2:                                # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movzbl	3(%rax), %ecx
	movq	$1, %rbx
	testq	$8, %rcx
	je	.LBB8_3
# BB#4:                                 # %while.body
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	24(%rax), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	jne	.LBB8_2
	jmp	.LBB8_5
.LBB8_3:
	movq	%rax, %r12
.LBB8_5:                                # %while.end
	testq	$134217728, %r13        # imm = 0x8000000
	jne	.LBB8_6
# BB#8:                                 # %if.end
	movq	%r14, %rdi
	callq	delete_insn
	testq	%rbx, %rbx
	je	.LBB8_11
# BB#9:                                 # %land.lhs.true
	movzbl	(%r12), %eax
	cmpq	$35, %rax
	jne	.LBB8_11
# BB#10:                                # %if.then20
	movq	%r12, %rdi
	callq	delete_insn
.LBB8_11:                               # %if.end22
	movzbl	(%r14), %eax
	cmpq	$33, %rax
	jne	.LBB8_22
# BB#12:                                # %land.lhs.true27
	movq	64(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB8_22
# BB#13:                                # %if.then32
	movl	32(%rdi), %eax
	testq	%rax, %rax
	je	.LBB8_14
# BB#18:                                # %if.else
	callq	next_nonnote_insn
	testq	%rax, %rax
	je	.LBB8_22
# BB#19:                                # %land.lhs.true58
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB8_22
# BB#20:                                # %land.lhs.true63
	movq	32(%rax), %rcx
	movl	(%rcx), %ecx
	andq	$65534, %rcx            # imm = 0xFFFE
	cmpq	$44, %rcx
	jne	.LBB8_22
# BB#21:                                # %if.then78
	movq	%rax, %rdi
	callq	delete_related_insns
.LBB8_22:                               # %if.end82
	movzbl	(%r14), %eax
	andq	$65535, %r13            # imm = 0xFFFF
	cmpq	$32, %rax
	je	.LBB8_43
# BB#23:                                # %if.end82
	cmpq	$34, %rax
	jne	.LBB8_24
.LBB8_43:                               # %if.then165
	movq	%r13, 8(%rsp)           # 8-byte Spill
	movq	56(%r14), %rbx
	jmp	.LBB8_49
	.align	16, 0x90
.LBB8_48:                               # %for.inc201
                                        #   in Loop: Header=BB8_49 Depth=1
	movq	16(%rbx), %rbx
.LBB8_49:                               # %for.inc201
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB8_38
# BB#44:                                # %for.body171
                                        #   in Loop: Header=BB8_49 Depth=1
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$851968, %rax           # imm = 0xD0000
	jne	.LBB8_48
# BB#45:                                # %land.lhs.true177
                                        #   in Loop: Header=BB8_49 Depth=1
	movq	8(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$36, %rax
	jne	.LBB8_48
# BB#46:                                # %if.then185
                                        #   in Loop: Header=BB8_49 Depth=1
	movl	32(%rdi), %eax
	testq	%rax, %rax
	jne	.LBB8_48
# BB#47:                                # %if.then194
                                        #   in Loop: Header=BB8_49 Depth=1
	callq	delete_related_insns
	jmp	.LBB8_48
.LBB8_24:                               # %if.end82
	movq	%r13, 8(%rsp)           # 8-byte Spill
	cmpq	$33, %rax
	jne	.LBB8_38
# BB#25:                                # %land.lhs.true87
	movq	32(%r14), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movl	(%rax), %eax
	movq	%rax, %rcx
	andq	$65534, %rcx            # imm = 0xFFFE
	cmpq	$44, %rcx
	jne	.LBB8_38
# BB#26:                                # %if.then103
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$45, %rax
	jne	.LBB8_27
# BB#28:                                # %if.then103
	movabsq	$1, %r14
	jmp	.LBB8_29
.LBB8_38:                               # %while.cond207.preheader
	xorq	%rbx, %rbx
	testq	%r15, %r15
	movabsq	$0, %r13
	je	.LBB8_51
# BB#39:
	movabsq	$0, %r13
	.align	16, 0x90
.LBB8_40:                               # %land.rhs209
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r15), %eax
	testq	$134217728, %rax        # imm = 0x8000000
	jne	.LBB8_50
# BB#41:                                # %land.rhs209
                                        #   in Loop: Header=BB8_40 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	movq	$1, %rbx
	cmpq	$37, %rax
	jne	.LBB8_42
.LBB8_50:                               # %while.body219
                                        #   in Loop: Header=BB8_40 Depth=1
	movq	16(%r15), %r15
	xorq	%rbx, %rbx
	testq	%r15, %r15
	jne	.LBB8_40
	jmp	.LBB8_51
.LBB8_42:
	movq	%r15, %r13
.LBB8_51:                               # %while.end223
	cmpq	$36, 8(%rsp)            # 8-byte Folded Reload
	jne	.LBB8_6
# BB#52:                                # %land.lhs.true225
	movq	24(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB8_56
# BB#53:                                # %land.lhs.true231
	movzbl	(%rdi), %eax
	cmpq	$33, %rax
	jne	.LBB8_56
# BB#54:                                # %land.lhs.true239
	movq	32(%rdi), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	jne	.LBB8_56
# BB#55:                                # %if.then261
	callq	delete_related_insns
	movq	%rax, %r12
.LBB8_56:                               # %if.end266
	testq	%rbx, %rbx
	je	.LBB8_6
# BB#57:                                # %land.lhs.true270
	movzbl	(%r13), %eax
	cmpq	$35, %rax
	jne	.LBB8_6
# BB#58:                                # %land.lhs.true270
	testq	%r12, %r12
	je	.LBB8_6
# BB#59:
	movabsq	$4294967205, %r14       # imm = 0xFFFFFFA5
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB8_62:                               # %land.rhs279
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %edx
	andq	$134283263, %rax        # imm = 0x800FFFF
	cmpq	$105, %rdx
	je	.LBB8_66
# BB#63:                                # %land.rhs279
                                        #   in Loop: Header=BB8_62 Depth=1
	cmpq	$37, %rcx
	je	.LBB8_66
# BB#64:                                # %land.rhs279
                                        #   in Loop: Header=BB8_62 Depth=1
	cmpq	$35, %rcx
	je	.LBB8_66
# BB#65:                                # %land.rhs279
                                        #   in Loop: Header=BB8_62 Depth=1
	movq	%rax, %rdx
	andq	%rbx, %rdx
	cmpq	$134217764, %rdx        # imm = 0x8000024
	jne	.LBB8_6
.LBB8_66:                               # %while.body304
                                        #   in Loop: Header=BB8_62 Depth=1
	cmpq	$37, %rcx
	jne	.LBB8_69
# BB#67:                                # %land.lhs.true307
                                        #   in Loop: Header=BB8_62 Depth=1
	movl	40(%r12), %eax
	cmpq	%r14, %rax
	je	.LBB8_60
# BB#68:                                # %if.then313
                                        #   in Loop: Header=BB8_62 Depth=1
	movq	24(%r12), %r12
	jmp	.LBB8_61
	.align	16, 0x90
.LBB8_69:                               # %if.else317
                                        #   in Loop: Header=BB8_62 Depth=1
	cmpq	$134217764, %rax        # imm = 0x8000024
	jne	.LBB8_60
# BB#70:                                # %if.then325
                                        #   in Loop: Header=BB8_62 Depth=1
	movq	24(%r12), %r12
	jmp	.LBB8_61
	.align	16, 0x90
.LBB8_60:                               # %if.else329
                                        #   in Loop: Header=BB8_62 Depth=1
	movq	%r12, %rdi
	callq	delete_related_insns
	movq	%rax, %r12
.LBB8_61:                               # %while.cond276
                                        #   in Loop: Header=BB8_62 Depth=1
	xorq	%rax, %rax
	testq	%r12, %r12
	jne	.LBB8_62
	jmp	.LBB8_7
.LBB8_14:                               # %if.then40
	callq	delete_related_insns
	xorq	%rax, %rax
	jmp	.LBB8_17
	.align	16, 0x90
.LBB8_16:                               # %while.body50
                                        #   in Loop: Header=BB8_17 Depth=1
	movq	24(%r12), %r12
.LBB8_17:                               # %while.body50
                                        # =>This Inner Loop Header: Depth=1
	testq	%r12, %r12
	je	.LBB8_7
# BB#15:                                # %land.rhs44
                                        #   in Loop: Header=BB8_17 Depth=1
	movzbl	3(%r12), %ecx
	testq	$8, %rcx
	jne	.LBB8_16
	jmp	.LBB8_6
.LBB8_27:
	movabsq	$0, %r14
.LBB8_29:                               # %if.then103
	movq	(%rsp), %rax            # 8-byte Reload
	movq	8(%rax,%r14,8), %rax
	movslq	(%rax), %r15
	testq	%r15, %r15
	jle	.LBB8_34
# BB#30:
	movq	$1, %rbx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r15
	jmp	.LBB8_31
	.align	16, 0x90
.LBB8_71:                               # %for.inc.for.body_crit_edge
                                        #   in Loop: Header=BB8_31 Depth=1
	movq	(%rsp), %rax            # 8-byte Reload
	movq	8(%rax,%r14,8), %rax
	incq	%rbx
.LBB8_31:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %rax
	movq	8(%rax), %rdi
	movl	32(%rdi), %eax
	testq	%rax, %rax
	jne	.LBB8_33
# BB#32:                                # %if.then129
                                        #   in Loop: Header=BB8_31 Depth=1
	movq	%rcx, %r13
	callq	delete_related_insns
	movq	%r13, %rcx
.LBB8_33:                               # %for.inc
                                        #   in Loop: Header=BB8_31 Depth=1
	movq	%rbx, %rax
	andq	%rcx, %rax
	cmpq	%r15, %rax
	jne	.LBB8_71
.LBB8_34:                               # %while.cond142.preheader
	xorq	%rax, %rax
	jmp	.LBB8_35
.LBB8_37:                               # %while.body150
                                        #   in Loop: Header=BB8_35 Depth=1
	movq	24(%r12), %r12
.LBB8_35:                               # %while.cond142.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%r12, %r12
	je	.LBB8_7
# BB#36:                                # %land.rhs144
                                        #   in Loop: Header=BB8_35 Depth=1
	movzbl	3(%r12), %ecx
	testq	$8, %rcx
	jne	.LBB8_37
.LBB8_6:
	movq	%r12, %rax
.LBB8_7:                                # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp92:
	.size	delete_related_insns, .Ltmp92-delete_related_insns
	.cfi_endproc

	.globl	squeeze_notes
	.align	16, 0x90
	.type	squeeze_notes,@function
squeeze_notes:                          # @squeeze_notes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp95:
	.cfi_def_cfa_offset 16
.Ltmp96:
	.cfi_offset %rbx, -16
	movq	(%rsi), %rax
	movq	(%rdi), %r10
	movq	24(%rax), %r8
	xorq	%rcx, %rcx
	cmpq	%r8, %r10
	je	.LBB9_11
# BB#1:
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	%r10, %rdx
.LBB9_3:                                # %for.body.lr.ph.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_5 Depth 2
                                        #       Child Loop BB9_6 Depth 3
	movq	%rcx, %r11
	movq	%rdx, %rcx
	movq	%r10, %rdx
.LBB9_5:                                # %for.body.lr.ph
                                        #   Parent Loop BB9_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB9_6 Depth 3
	movq	%rdx, %r10
	.align	16, 0x90
.LBB9_6:                                # %for.body
                                        #   Parent Loop BB9_3 Depth=1
                                        #     Parent Loop BB9_5 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rcx), %eax
	movq	24(%rcx), %rdx
	cmpq	$37, %rax
	jne	.LBB9_2
# BB#7:                                 # %land.lhs.true
                                        #   in Loop: Header=BB9_6 Depth=3
	movl	40(%rcx), %eax
	addq	$98, %rax
	andq	%r9, %rax
	cmpq	$5, %rax
	ja	.LBB9_2
# BB#8:                                 # %if.then
                                        #   in Loop: Header=BB9_6 Depth=3
	cmpq	%r10, %rcx
	je	.LBB9_4
# BB#9:                                 # %if.else
                                        #   in Loop: Header=BB9_6 Depth=3
	movq	16(%r10), %rax
	movq	16(%rcx), %rbx
	movq	%rax, 16(%rcx)
	movq	%r10, 24(%rcx)
	movq	%rcx, 24(%rax)
	movq	24(%rcx), %rax
	movq	%rcx, 16(%rax)
	cmpq	%r8, %rdx
	movq	%rdx, 24(%rbx)
	movq	%rbx, 16(%rdx)
	movq	%rdx, %rcx
	jne	.LBB9_6
	jmp	.LBB9_10
	.align	16, 0x90
.LBB9_4:                                # %for.cond.outer102.loopexit
                                        #   in Loop: Header=BB9_5 Depth=2
	movq	$1, %rax
	cmpq	%r8, %rdx
	movq	%rdx, %rcx
	jne	.LBB9_5
	jmp	.LBB9_13
	.align	16, 0x90
.LBB9_2:                                # %for.cond.outer.loopexit
                                        #   in Loop: Header=BB9_3 Depth=1
	cmpq	%r8, %rdx
	jne	.LBB9_3
	jmp	.LBB9_11
.LBB9_10:
	movq	%r11, %rcx
.LBB9_11:                               # %for.end
	movq	$1, %rax
	cmpq	%r8, %r10
	je	.LBB9_13
# BB#12:                                # %if.end68
	movq	%r10, (%rdi)
	movq	%rcx, (%rsi)
	xorq	%rax, %rax
.LBB9_13:                               # %return
	popq	%rbx
	retq
.Ltmp97:
	.size	squeeze_notes, .Ltmp97-squeeze_notes
	.cfi_endproc

	.globl	get_label_before
	.align	16, 0x90
	.type	get_label_before,@function
get_label_before:                       # @get_label_before
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp101:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp102:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp103:
	.cfi_def_cfa_offset 32
.Ltmp104:
	.cfi_offset %rbx, -24
.Ltmp105:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	prev_nonnote_insn
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB10_2
# BB#1:                                 # %lor.lhs.false
	movzbl	(%rbx), %eax
	cmpq	$36, %rax
	je	.LBB10_3
.LBB10_2:                               # %if.then
	movq	16(%r14), %r14
	callq	gen_label_rtx
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	emit_label_after
	xorq	%rax, %rax
	movl	%eax, 32(%rbx)
.LBB10_3:                               # %if.end
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp106:
	.size	get_label_before, .Ltmp106-get_label_before
	.cfi_endproc

	.globl	get_label_after
	.align	16, 0x90
	.type	get_label_after,@function
get_label_after:                        # @get_label_after
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp110:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp111:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp112:
	.cfi_def_cfa_offset 32
.Ltmp113:
	.cfi_offset %rbx, -24
.Ltmp114:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	next_nonnote_insn
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB11_2
# BB#1:                                 # %lor.lhs.false
	movzbl	(%rbx), %eax
	cmpq	$36, %rax
	je	.LBB11_3
.LBB11_2:                               # %if.then
	callq	gen_label_rtx
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	emit_label_after
	xorq	%rax, %rax
	movl	%eax, 32(%rbx)
.LBB11_3:                               # %if.end
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp115:
	.size	get_label_after, .Ltmp115-get_label_after
	.cfi_endproc

	.globl	reversed_comparison_code_parts
	.align	16, 0x90
	.type	reversed_comparison_code_parts,@function
reversed_comparison_code_parts:         # @reversed_comparison_code_parts
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp117:
	.cfi_def_cfa_offset 16
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	rtx_class(%rax), %eax
	cmpq	$60, %rax
	jne	.LBB12_1
# BB#2:                                 # %if.end
	movl	(%rsi), %r8d
	movq	$16, %rcx
	movq	%r8, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	jne	.LBB12_4
# BB#3:                                 # %if.then4
	movzbl	2(%rdx), %esi
.LBB12_4:                               # %if.end8
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rsi, %rcx
	andq	%rax, %rcx
	movl	mode_class(,%rcx,4), %ecx
	cmpq	$4, %rcx
	jne	.LBB12_34
# BB#5:                                 # %if.then13
	addq	$-57, %rsi
	andq	%rax, %rsi
	addq	$-102, %rdi
	andq	%rax, %rdi
	cmpq	$2, %rsi
	jb	.LBB12_19
# BB#6:                                 # %cond.true
	cmpq	$17, %rdi
	ja	.LBB12_78
# BB#7:                                 # %cond.true
	movq	$102, %rax
	jmpq	*.LJTI12_5(,%rdi,8)
.LBB12_18:                              # %sw.bb12.i
	xorq	%rax, %rax
	popq	%rdx
	retq
.LBB12_1:
	xorq	%rax, %rax
	popq	%rdx
	retq
.LBB12_34:                              # %if.end19
	addq	$-102, %rdi
	andq	%rax, %rdi
	cmpq	$17, %rdi
	ja	.LBB12_50
# BB#35:                                # %if.end19
	xorq	%rax, %rax
	jmpq	*.LJTI12_0(,%rdi,8)
.LBB12_36:                              # %sw.bb
	cmpq	$9, %rdi
	ja	.LBB12_78
# BB#37:                                # %sw.bb
	movq	$102, %rax
	jmpq	*.LJTI12_1(,%rdi,8)
.LBB12_39:                              # %sw.bb3.i171
	movq	$107, %rax
	popq	%rdx
	retq
.LBB12_50:                              # %sw.epilog
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	je	.LBB12_59
# BB#51:                                # %if.then24
	cmpq	$17, %rdi
	ja	.LBB12_78
# BB#52:                                # %if.then24
	movq	$102, %rax
	jmpq	*.LJTI12_2(,%rdi,8)
.LBB12_58:                              # %sw.bb12.i211
	xorq	%rax, %rax
	popq	%rdx
	retq
.LBB12_19:                              # %cond.false
	cmpq	$17, %rdi
	ja	.LBB12_33
# BB#20:                                # %cond.false
	movq	$102, %rax
	jmpq	*.LJTI12_4(,%rdi,8)
.LBB12_22:                              # %sw.bb3.i157
	movq	$118, %rax
	popq	%rdx
	retq
.LBB12_59:                              # %if.end90
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$54, %rax
	je	.LBB12_65
# BB#60:                                # %lor.lhs.false
	testq	$16711680, %r8          # imm = 0xFF0000
	je	.LBB12_61
# BB#62:                                # %land.lhs.true100
	cmpq	$8, %rcx
	ja	.LBB12_65
# BB#63:                                # %land.lhs.true100
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$340, %rax              # imm = 0x154
	je	.LBB12_65
# BB#64:
	xorq	%rax, %rax
	popq	%rdx
	retq
.LBB12_65:                              # %if.then120
	cmpq	$17, %rdi
	ja	.LBB12_78
# BB#66:                                # %if.then120
	movq	$102, %rax
	jmpq	*.LJTI12_3(,%rdi,8)
.LBB12_77:                              # %sw.bb12.i226
	xorq	%rax, %rax
	popq	%rdx
	retq
.LBB12_79:                              # %sw.bb1.i
	movq	$103, %rax
.LBB12_80:                              # %return
	popq	%rdx
	retq
.LBB12_78:                              # %sw.default.i
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.reverse_condition, %rdx
	movq	$824, %rsi              # imm = 0x338
	callq	fancy_abort
.LBB12_61:
	xorq	%rax, %rax
	popq	%rdx
	retq
.LBB12_21:                              # %sw.bb2.i156
	movq	$117, %rax
	popq	%rdx
	retq
.LBB12_24:                              # %sw.bb5.i159
	movq	$116, %rax
	popq	%rdx
	retq
.LBB12_23:                              # %sw.bb4.i158
	movq	$115, %rax
	popq	%rdx
	retq
.LBB12_26:                              # %sw.bb7.i161
	movq	$113, %rax
	popq	%rdx
	retq
.LBB12_27:                              # %sw.bb8.i162
	movq	$112, %rax
	popq	%rdx
	retq
.LBB12_32:                              # %sw.bb13.i
	movq	$119, %rax
	popq	%rdx
	retq
.LBB12_31:                              # %sw.bb12.i166
	movq	$107, %rax
	popq	%rdx
	retq
.LBB12_30:                              # %sw.bb11.i165
	movq	$106, %rax
	popq	%rdx
	retq
.LBB12_29:                              # %sw.bb10.i164
	movq	$105, %rax
	popq	%rdx
	retq
.LBB12_28:                              # %sw.bb9.i163
	movq	$104, %rax
	popq	%rdx
	retq
.LBB12_25:                              # %sw.bb6.i160
	movq	$114, %rax
	popq	%rdx
	retq
.LBB12_47:                              # %sw.bb7.i190
	movq	$113, %rax
	popq	%rdx
	retq
.LBB12_48:                              # %sw.bb8.i191
	movq	$112, %rax
	popq	%rdx
	retq
.LBB12_49:                              # %sw.bb13.i196
	movq	$119, %rax
	popq	%rdx
	retq
.LBB12_46:                              # %sw.bb6.i189
	movq	$114, %rax
	popq	%rdx
	retq
.LBB12_33:                              # %sw.default.i167
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.reverse_condition_maybe_unordered, %rdx
	movq	$872, %rsi              # imm = 0x368
	callq	fancy_abort
.LBB12_9:                               # %sw.bb3.i
	movq	$107, %rax
	popq	%rdx
	retq
.LBB12_8:                               # %sw.bb2.i
	movq	$106, %rax
	popq	%rdx
	retq
.LBB12_11:                              # %sw.bb5.i
	movq	$105, %rax
	popq	%rdx
	retq
.LBB12_10:                              # %sw.bb4.i
	movq	$104, %rax
	popq	%rdx
	retq
.LBB12_13:                              # %sw.bb7.i
	movq	$111, %rax
	popq	%rdx
	retq
.LBB12_12:                              # %sw.bb6.i
	movq	$110, %rax
	popq	%rdx
	retq
.LBB12_15:                              # %sw.bb9.i
	movq	$109, %rax
	popq	%rdx
	retq
.LBB12_14:                              # %sw.bb8.i
	movq	$108, %rax
	popq	%rdx
	retq
.LBB12_16:                              # %sw.bb10.i
	movq	$113, %rax
	popq	%rdx
	retq
.LBB12_17:                              # %sw.bb11.i
	movq	$112, %rax
	popq	%rdx
	retq
.LBB12_54:                              # %sw.bb3.i202
	movq	$107, %rax
	popq	%rdx
	retq
.LBB12_53:                              # %sw.bb2.i201
	movq	$106, %rax
	popq	%rdx
	retq
.LBB12_56:                              # %sw.bb5.i204
	movq	$105, %rax
	popq	%rdx
	retq
.LBB12_55:                              # %sw.bb4.i203
	movq	$104, %rax
	popq	%rdx
	retq
.LBB12_57:                              # %sw.bb11.i210
	movq	$112, %rax
	popq	%rdx
	retq
.LBB12_38:                              # %sw.bb2.i170
	movq	$106, %rax
	popq	%rdx
	retq
.LBB12_41:                              # %sw.bb5.i173
	movq	$105, %rax
	popq	%rdx
	retq
.LBB12_40:                              # %sw.bb4.i172
	movq	$104, %rax
	popq	%rdx
	retq
.LBB12_43:                              # %sw.bb7.i175
	movq	$111, %rax
	popq	%rdx
	retq
.LBB12_42:                              # %sw.bb6.i174
	movq	$110, %rax
	popq	%rdx
	retq
.LBB12_45:                              # %sw.bb9.i177
	movq	$109, %rax
	popq	%rdx
	retq
.LBB12_44:                              # %sw.bb8.i176
	movq	$108, %rax
	popq	%rdx
	retq
.LBB12_68:                              # %sw.bb3.i217
	movq	$107, %rax
	popq	%rdx
	retq
.LBB12_67:                              # %sw.bb2.i216
	movq	$106, %rax
	popq	%rdx
	retq
.LBB12_70:                              # %sw.bb5.i219
	movq	$105, %rax
	popq	%rdx
	retq
.LBB12_69:                              # %sw.bb4.i218
	movq	$104, %rax
	popq	%rdx
	retq
.LBB12_72:                              # %sw.bb7.i221
	movq	$111, %rax
	popq	%rdx
	retq
.LBB12_71:                              # %sw.bb6.i220
	movq	$110, %rax
	popq	%rdx
	retq
.LBB12_74:                              # %sw.bb9.i223
	movq	$109, %rax
	popq	%rdx
	retq
.LBB12_73:                              # %sw.bb8.i222
	movq	$108, %rax
	popq	%rdx
	retq
.LBB12_75:                              # %sw.bb10.i224
	movq	$113, %rax
	popq	%rdx
	retq
.LBB12_76:                              # %sw.bb11.i225
	movq	$112, %rax
	popq	%rdx
	retq
.Ltmp118:
	.size	reversed_comparison_code_parts, .Ltmp118-reversed_comparison_code_parts
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI12_0:
	.quad	.LBB12_36
	.quad	.LBB12_36
	.quad	.LBB12_50
	.quad	.LBB12_50
	.quad	.LBB12_50
	.quad	.LBB12_50
	.quad	.LBB12_36
	.quad	.LBB12_36
	.quad	.LBB12_36
	.quad	.LBB12_36
	.quad	.LBB12_47
	.quad	.LBB12_48
	.quad	.LBB12_49
	.quad	.LBB12_80
	.quad	.LBB12_80
	.quad	.LBB12_80
	.quad	.LBB12_80
	.quad	.LBB12_46
.LJTI12_1:
	.quad	.LBB12_79
	.quad	.LBB12_80
	.quad	.LBB12_39
	.quad	.LBB12_38
	.quad	.LBB12_41
	.quad	.LBB12_40
	.quad	.LBB12_43
	.quad	.LBB12_42
	.quad	.LBB12_45
	.quad	.LBB12_44
.LJTI12_2:
	.quad	.LBB12_79
	.quad	.LBB12_80
	.quad	.LBB12_54
	.quad	.LBB12_53
	.quad	.LBB12_56
	.quad	.LBB12_55
	.quad	.LBB12_78
	.quad	.LBB12_78
	.quad	.LBB12_78
	.quad	.LBB12_78
	.quad	.LBB12_78
	.quad	.LBB12_57
	.quad	.LBB12_58
	.quad	.LBB12_58
	.quad	.LBB12_58
	.quad	.LBB12_58
	.quad	.LBB12_58
	.quad	.LBB12_58
.LJTI12_3:
	.quad	.LBB12_79
	.quad	.LBB12_80
	.quad	.LBB12_68
	.quad	.LBB12_67
	.quad	.LBB12_70
	.quad	.LBB12_69
	.quad	.LBB12_72
	.quad	.LBB12_71
	.quad	.LBB12_74
	.quad	.LBB12_73
	.quad	.LBB12_75
	.quad	.LBB12_76
	.quad	.LBB12_77
	.quad	.LBB12_77
	.quad	.LBB12_77
	.quad	.LBB12_77
	.quad	.LBB12_77
	.quad	.LBB12_77
.LJTI12_4:
	.quad	.LBB12_79
	.quad	.LBB12_80
	.quad	.LBB12_22
	.quad	.LBB12_21
	.quad	.LBB12_24
	.quad	.LBB12_23
	.quad	.LBB12_33
	.quad	.LBB12_33
	.quad	.LBB12_33
	.quad	.LBB12_33
	.quad	.LBB12_26
	.quad	.LBB12_27
	.quad	.LBB12_32
	.quad	.LBB12_31
	.quad	.LBB12_30
	.quad	.LBB12_29
	.quad	.LBB12_28
	.quad	.LBB12_25
.LJTI12_5:
	.quad	.LBB12_79
	.quad	.LBB12_80
	.quad	.LBB12_9
	.quad	.LBB12_8
	.quad	.LBB12_11
	.quad	.LBB12_10
	.quad	.LBB12_13
	.quad	.LBB12_12
	.quad	.LBB12_15
	.quad	.LBB12_14
	.quad	.LBB12_16
	.quad	.LBB12_17
	.quad	.LBB12_18
	.quad	.LBB12_18
	.quad	.LBB12_18
	.quad	.LBB12_18
	.quad	.LBB12_18
	.quad	.LBB12_18

	.text
	.globl	reverse_condition
	.align	16, 0x90
	.type	reverse_condition,@function
reverse_condition:                      # @reverse_condition
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp120:
	.cfi_def_cfa_offset 16
	addq	$-102, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$17, %rcx
	ja	.LBB13_13
# BB#1:                                 # %entry
	movq	$102, %rax
	jmpq	*.LJTI13_0(,%rcx,8)
.LBB13_12:                              # %sw.bb12
	xorq	%rax, %rax
	popq	%rdx
	retq
.LBB13_13:                              # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.reverse_condition, %rdx
	movq	$824, %rsi              # imm = 0x338
	callq	fancy_abort
.LBB13_14:                              # %sw.bb1
	movq	$103, %rax
.LBB13_15:                              # %return
	popq	%rdx
	retq
.LBB13_3:                               # %sw.bb3
	movq	$107, %rax
	popq	%rdx
	retq
.LBB13_2:                               # %sw.bb2
	movq	$106, %rax
	popq	%rdx
	retq
.LBB13_5:                               # %sw.bb5
	movq	$105, %rax
	popq	%rdx
	retq
.LBB13_4:                               # %sw.bb4
	movq	$104, %rax
	popq	%rdx
	retq
.LBB13_7:                               # %sw.bb7
	movq	$111, %rax
	popq	%rdx
	retq
.LBB13_6:                               # %sw.bb6
	movq	$110, %rax
	popq	%rdx
	retq
.LBB13_9:                               # %sw.bb9
	movq	$109, %rax
	popq	%rdx
	retq
.LBB13_8:                               # %sw.bb8
	movq	$108, %rax
	popq	%rdx
	retq
.LBB13_10:                              # %sw.bb10
	movq	$113, %rax
	popq	%rdx
	retq
.LBB13_11:                              # %sw.bb11
	movq	$112, %rax
	popq	%rdx
	retq
.Ltmp121:
	.size	reverse_condition, .Ltmp121-reverse_condition
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI13_0:
	.quad	.LBB13_14
	.quad	.LBB13_15
	.quad	.LBB13_3
	.quad	.LBB13_2
	.quad	.LBB13_5
	.quad	.LBB13_4
	.quad	.LBB13_7
	.quad	.LBB13_6
	.quad	.LBB13_9
	.quad	.LBB13_8
	.quad	.LBB13_10
	.quad	.LBB13_11
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12

	.text
	.globl	reverse_condition_maybe_unordered
	.align	16, 0x90
	.type	reverse_condition_maybe_unordered,@function
reverse_condition_maybe_unordered:      # @reverse_condition_maybe_unordered
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp123:
	.cfi_def_cfa_offset 16
	addq	$-102, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$17, %rcx
	ja	.LBB14_14
# BB#1:                                 # %entry
	movq	$102, %rax
	jmpq	*.LJTI14_0(,%rcx,8)
.LBB14_15:                              # %sw.bb1
	movq	$103, %rax
.LBB14_16:                              # %return
	popq	%rdx
	retq
.LBB14_3:                               # %sw.bb3
	movq	$118, %rax
	popq	%rdx
	retq
.LBB14_2:                               # %sw.bb2
	movq	$117, %rax
	popq	%rdx
	retq
.LBB14_5:                               # %sw.bb5
	movq	$116, %rax
	popq	%rdx
	retq
.LBB14_4:                               # %sw.bb4
	movq	$115, %rax
	popq	%rdx
	retq
.LBB14_7:                               # %sw.bb7
	movq	$113, %rax
	popq	%rdx
	retq
.LBB14_8:                               # %sw.bb8
	movq	$112, %rax
	popq	%rdx
	retq
.LBB14_13:                              # %sw.bb13
	movq	$119, %rax
	popq	%rdx
	retq
.LBB14_12:                              # %sw.bb12
	movq	$107, %rax
	popq	%rdx
	retq
.LBB14_11:                              # %sw.bb11
	movq	$106, %rax
	popq	%rdx
	retq
.LBB14_10:                              # %sw.bb10
	movq	$105, %rax
	popq	%rdx
	retq
.LBB14_9:                               # %sw.bb9
	movq	$104, %rax
	popq	%rdx
	retq
.LBB14_6:                               # %sw.bb6
	movq	$114, %rax
	popq	%rdx
	retq
.LBB14_14:                              # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.reverse_condition_maybe_unordered, %rdx
	movq	$872, %rsi              # imm = 0x368
	callq	fancy_abort
.Ltmp124:
	.size	reverse_condition_maybe_unordered, .Ltmp124-reverse_condition_maybe_unordered
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI14_0:
	.quad	.LBB14_15
	.quad	.LBB14_16
	.quad	.LBB14_3
	.quad	.LBB14_2
	.quad	.LBB14_5
	.quad	.LBB14_4
	.quad	.LBB14_14
	.quad	.LBB14_14
	.quad	.LBB14_14
	.quad	.LBB14_14
	.quad	.LBB14_7
	.quad	.LBB14_8
	.quad	.LBB14_13
	.quad	.LBB14_12
	.quad	.LBB14_11
	.quad	.LBB14_10
	.quad	.LBB14_9
	.quad	.LBB14_6

	.text
	.globl	reversed_comparison_code
	.align	16, 0x90
	.type	reversed_comparison_code,@function
reversed_comparison_code:               # @reversed_comparison_code
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, %rcx
	movzbl	(%rcx), %edi
	movzbl	rtx_class(%rdi), %edx
	movq	%rsi, %rax
	cmpq	$60, %rdx
	jne	.LBB15_1
# BB#2:                                 # %if.end
	movq	8(%rcx), %rsi
	movq	16(%rcx), %rdx
	movq	%rax, %rcx
	jmp	reversed_comparison_code_parts  # TAILCALL
.LBB15_1:                               # %return
	xorq	%rax, %rax
	retq
.Ltmp125:
	.size	reversed_comparison_code, .Ltmp125-reversed_comparison_code
	.cfi_endproc

	.globl	swap_condition
	.align	16, 0x90
	.type	swap_condition,@function
swap_condition:                         # @swap_condition
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp127:
	.cfi_def_cfa_offset 16
	leaq	-102(%rdi), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$17, %rax
	ja	.LBB16_13
# BB#1:                                 # %entry
	jmpq	*.LJTI16_0(,%rax,8)
.LBB16_2:                               # %sw.bb2
	movq	$106, %rdi
	jmp	.LBB16_15
.LBB16_13:                              # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.swap_condition, %rdx
	movq	$919, %rsi              # imm = 0x397
	callq	fancy_abort
.LBB16_14:                              # %sw.bb1
	movq	$107, %rdi
	jmp	.LBB16_15
.LBB16_4:                               # %sw.bb4
	movq	$104, %rdi
	jmp	.LBB16_15
.LBB16_3:                               # %sw.bb3
	movq	$105, %rdi
	jmp	.LBB16_15
.LBB16_6:                               # %sw.bb6
	movq	$110, %rdi
	jmp	.LBB16_15
.LBB16_5:                               # %sw.bb5
	movq	$111, %rdi
	jmp	.LBB16_15
.LBB16_8:                               # %sw.bb8
	movq	$108, %rdi
	jmp	.LBB16_15
.LBB16_7:                               # %sw.bb7
	movq	$109, %rdi
	jmp	.LBB16_15
.LBB16_12:                              # %sw.bb12
	movq	$117, %rdi
	jmp	.LBB16_15
.LBB16_11:                              # %sw.bb11
	movq	$118, %rdi
	jmp	.LBB16_15
.LBB16_10:                              # %sw.bb10
	movq	$115, %rdi
	jmp	.LBB16_15
.LBB16_9:                               # %sw.bb9
	movq	$116, %rdi
.LBB16_15:                              # %return
	movq	%rdi, %rax
	popq	%rdx
	retq
.Ltmp128:
	.size	swap_condition, .Ltmp128-swap_condition
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI16_0:
	.quad	.LBB16_15
	.quad	.LBB16_15
	.quad	.LBB16_2
	.quad	.LBB16_14
	.quad	.LBB16_4
	.quad	.LBB16_3
	.quad	.LBB16_6
	.quad	.LBB16_5
	.quad	.LBB16_8
	.quad	.LBB16_7
	.quad	.LBB16_15
	.quad	.LBB16_15
	.quad	.LBB16_15
	.quad	.LBB16_12
	.quad	.LBB16_11
	.quad	.LBB16_10
	.quad	.LBB16_9
	.quad	.LBB16_15

	.text
	.globl	unsigned_condition
	.align	16, 0x90
	.type	unsigned_condition,@function
unsigned_condition:                     # @unsigned_condition
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp130:
	.cfi_def_cfa_offset 16
	leaq	-102(%rdi), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$9, %rax
	ja	.LBB17_5
# BB#1:                                 # %entry
	jmpq	*.LJTI17_0(,%rax,8)
.LBB17_2:                               # %sw.bb2
	movq	$108, %rdi
	jmp	.LBB17_7
.LBB17_6:                               # %sw.bb1
	movq	$109, %rdi
	jmp	.LBB17_7
.LBB17_4:                               # %sw.bb4
	movq	$110, %rdi
	jmp	.LBB17_7
.LBB17_3:                               # %sw.bb3
	movq	$111, %rdi
.LBB17_7:                               # %return
	movq	%rdi, %rax
	popq	%rdx
	retq
.LBB17_5:                               # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.unsigned_condition, %rdx
	movq	$951, %rsi              # imm = 0x3B7
	callq	fancy_abort
.Ltmp131:
	.size	unsigned_condition, .Ltmp131-unsigned_condition
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI17_0:
	.quad	.LBB17_7
	.quad	.LBB17_7
	.quad	.LBB17_2
	.quad	.LBB17_6
	.quad	.LBB17_4
	.quad	.LBB17_3
	.quad	.LBB17_7
	.quad	.LBB17_7
	.quad	.LBB17_7
	.quad	.LBB17_7

	.text
	.globl	signed_condition
	.align	16, 0x90
	.type	signed_condition,@function
signed_condition:                       # @signed_condition
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp133:
	.cfi_def_cfa_offset 16
	leaq	-102(%rdi), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$9, %rax
	ja	.LBB18_5
# BB#1:                                 # %entry
	jmpq	*.LJTI18_0(,%rax,8)
.LBB18_2:                               # %sw.bb2
	movq	$104, %rdi
	jmp	.LBB18_7
.LBB18_6:                               # %sw.bb1
	movq	$105, %rdi
	jmp	.LBB18_7
.LBB18_4:                               # %sw.bb4
	movq	$106, %rdi
	jmp	.LBB18_7
.LBB18_3:                               # %sw.bb3
	movq	$107, %rdi
.LBB18_7:                               # %return
	movq	%rdi, %rax
	popq	%rdx
	retq
.LBB18_5:                               # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.signed_condition, %rdx
	movq	$981, %rsi              # imm = 0x3D5
	callq	fancy_abort
.Ltmp134:
	.size	signed_condition, .Ltmp134-signed_condition
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI18_0:
	.quad	.LBB18_7
	.quad	.LBB18_7
	.quad	.LBB18_7
	.quad	.LBB18_7
	.quad	.LBB18_7
	.quad	.LBB18_7
	.quad	.LBB18_2
	.quad	.LBB18_6
	.quad	.LBB18_4
	.quad	.LBB18_3

	.text
	.globl	comparison_dominates_p
	.align	16, 0x90
	.type	comparison_dominates_p,@function
comparison_dominates_p:                 # @comparison_dominates_p
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rdi
	je	.LBB19_24
# BB#1:                                 # %entry
	testq	%r8, %rsi
	je	.LBB19_24
# BB#2:                                 # %if.end
	movq	%rsi, %rdx
	andq	%r8, %rdx
	movq	%rdi, %rcx
	andq	%r8, %rcx
	movq	$1, %rax
	cmpq	%rdx, %rcx
	je	.LBB19_24
# BB#3:                                 # %if.end4
	addq	$-103, %rdi
	andq	%r8, %rdi
	cmpq	$16, %rdi
	ja	.LBB19_23
# BB#4:                                 # %if.end4
	jmpq	*.LJTI19_0(,%rdi,8)
.LBB19_7:                               # %sw.bb10
	addq	$-104, %rsi
	andq	%r8, %rsi
	cmpq	$9, %rsi
	ja	.LBB19_23
# BB#8:                                 # %sw.bb10
	movq	$1, %rdx
	movq	%rsi, %rcx
	shlq	%cl, %rdx
	testq	$597, %rdx              # imm = 0x255
	jne	.LBB19_24
	jmp	.LBB19_23
.LBB19_14:                              # %sw.bb44
	addq	$-102, %rsi
	andq	%r8, %rsi
	cmpq	$17, %rsi
	ja	.LBB19_23
# BB#15:                                # %sw.bb44
	movq	$1, %rdx
	movq	%rsi, %rcx
	shlq	%cl, %rdx
	testq	$133125, %rdx           # imm = 0x20805
	jne	.LBB19_24
	jmp	.LBB19_23
.LBB19_10:                              # %sw.bb28
	addq	$-102, %rsi
	andq	%r8, %rsi
	cmpq	$17, %rsi
	ja	.LBB19_23
# BB#11:                                # %sw.bb28
	movq	$1, %rdx
	movq	%rsi, %rcx
	shlq	%cl, %rdx
	testq	$133137, %rdx           # imm = 0x20811
	jne	.LBB19_24
	jmp	.LBB19_23
.LBB19_19:                              # %sw.bb70
	cmpq	$102, %rdx
	je	.LBB19_24
# BB#20:                                # %sw.bb70
	cmpq	$108, %rdx
	jne	.LBB19_23
	jmp	.LBB19_24
.LBB19_18:                              # %sw.bb64
	movabsq	$4294967287, %rcx       # imm = 0xFFFFFFF7
	andq	%rcx, %rsi
	cmpq	$102, %rsi
	jne	.LBB19_23
	jmp	.LBB19_24
.LBB19_21:                              # %sw.bb76
	addq	$-102, %rsi
	andq	%r8, %rsi
	cmpq	$16, %rsi
	ja	.LBB19_23
# BB#22:                                # %sw.bb76
	movq	$1, %rdx
	movq	%rsi, %rcx
	shlq	%cl, %rdx
	testq	$126977, %rdx           # imm = 0x1F001
	jne	.LBB19_24
	jmp	.LBB19_23
.LBB19_5:                               # %sw.bb
	cmpq	$115, %rdx
	je	.LBB19_24
	jmp	.LBB19_6
.LBB19_12:                              # %sw.bb38
	cmpq	$102, %rdx
	je	.LBB19_24
# BB#13:                                # %sw.bb38
	cmpq	$115, %rdx
	jne	.LBB19_23
	jmp	.LBB19_24
.LBB19_9:                               # %sw.bb22
	cmpq	$102, %rdx
	je	.LBB19_24
.LBB19_6:                               # %sw.bb
	cmpq	$117, %rdx
	jne	.LBB19_23
	jmp	.LBB19_24
.LBB19_16:                              # %sw.bb58
	cmpq	$102, %rdx
	je	.LBB19_24
.LBB19_17:                              # %sw.bb54
	cmpq	$113, %rdx
	je	.LBB19_24
.LBB19_23:                              # %sw.epilog
	xorq	%rax, %rax
.LBB19_24:                              # %return
	retq
.Ltmp135:
	.size	comparison_dominates_p, .Ltmp135-comparison_dominates_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI19_0:
	.quad	.LBB19_7
	.quad	.LBB19_17
	.quad	.LBB19_14
	.quad	.LBB19_17
	.quad	.LBB19_10
	.quad	.LBB19_23
	.quad	.LBB19_19
	.quad	.LBB19_23
	.quad	.LBB19_18
	.quad	.LBB19_21
	.quad	.LBB19_23
	.quad	.LBB19_5
	.quad	.LBB19_23
	.quad	.LBB19_12
	.quad	.LBB19_23
	.quad	.LBB19_9
	.quad	.LBB19_16

	.text
	.globl	simplejump_p
	.align	16, 0x90
	.type	simplejump_p,@function
simplejump_p:                           # @simplejump_p
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %ecx
	xorq	%rax, %rax
	cmpq	$33, %rcx
	jne	.LBB20_6
# BB#1:                                 # %land.lhs.true
	movq	32(%rdi), %rcx
	movzbl	(%rcx), %edx
	cmpq	$47, %rdx
	jne	.LBB20_6
# BB#2:                                 # %land.lhs.true4
	movq	8(%rcx), %rdx
	movzbl	(%rdx), %edx
	cmpq	$59, %rdx
	jne	.LBB20_6
# BB#3:                                 # %land.rhs
	movq	16(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$67, %rax
	jne	.LBB20_4
# BB#5:                                 # %land.rhs
	movabsq	$1, %rax
.LBB20_6:                               # %land.end
	retq
.LBB20_4:
	movabsq	$0, %rax
	retq
.Ltmp136:
	.size	simplejump_p, .Ltmp136-simplejump_p
	.cfi_endproc

	.globl	condjump_p
	.align	16, 0x90
	.type	condjump_p,@function
condjump_p:                             # @condjump_p
	.cfi_startproc
# BB#0:                                 # %entry
	movq	32(%rdi), %rcx
	movzbl	(%rcx), %edx
	xorq	%rax, %rax
	cmpq	$47, %rdx
	jne	.LBB21_13
# BB#1:                                 # %lor.lhs.false
	movq	8(%rcx), %rdx
	movzbl	(%rdx), %edx
	cmpq	$59, %rdx
	jne	.LBB21_13
# BB#2:                                 # %if.end
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$67, %rdx
	movq	$1, %rax
	je	.LBB21_13
# BB#3:                                 # %if.end
	xorq	%rax, %rax
	cmpq	$72, %rdx
	jne	.LBB21_13
# BB#4:                                 # %land.rhs
	movq	24(%rcx), %rax
	movq	16(%rcx), %rdx
	movzbl	(%rax), %ecx
	movl	(%rdx), %edx
	cmpq	$59, %rcx
	jne	.LBB21_7
# BB#5:                                 # %land.lhs.true
	movq	$1, %rax
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$51, %rsi
	je	.LBB21_13
# BB#6:                                 # %land.lhs.true
	cmpq	$67, %rsi
	je	.LBB21_13
.LBB21_7:                               # %lor.rhs
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$59, %rdx
	movabsq	$0, %rax
	jne	.LBB21_13
# BB#8:                                 # %land.rhs42
	cmpq	$67, %rcx
	movabsq	$0, %rdx
	movabsq	$1, %rsi
	movq	%rdx, %rax
	jne	.LBB21_10
# BB#9:                                 # %land.rhs42
	movq	%rsi, %rax
.LBB21_10:                              # %land.rhs42
	cmpq	$51, %rcx
	jne	.LBB21_12
# BB#11:                                # %land.rhs42
	movq	%rsi, %rdx
.LBB21_12:                              # %land.rhs42
	orq	%rdx, %rax
.LBB21_13:                              # %return
	retq
.Ltmp137:
	.size	condjump_p, .Ltmp137-condjump_p
	.cfi_endproc

	.globl	condjump_in_parallel_p
	.align	16, 0x90
	.type	condjump_in_parallel_p,@function
condjump_in_parallel_p:                 # @condjump_in_parallel_p
	.cfi_startproc
# BB#0:                                 # %entry
	movq	32(%rdi), %rcx
	movzbl	(%rcx), %edx
	xorq	%rax, %rax
	cmpq	$39, %rdx
	jne	.LBB22_12
# BB#1:                                 # %if.else
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$47, %rdx
	jne	.LBB22_12
# BB#2:                                 # %if.end8
	movq	8(%rcx), %rdx
	movzbl	(%rdx), %edx
	cmpq	$59, %rdx
	jne	.LBB22_12
# BB#3:                                 # %if.end16
	movq	16(%rcx), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$67, %rcx
	movq	$1, %rax
	je	.LBB22_12
# BB#4:                                 # %if.end16
	cmpq	$72, %rcx
	jne	.LBB22_11
# BB#5:                                 # %if.end32
	movq	24(%rdx), %rcx
	movq	global_rtl(%rip), %rsi
	movq	16(%rdx), %rdx
	cmpq	%rsi, %rcx
	jne	.LBB22_8
# BB#6:                                 # %land.lhs.true
	movzbl	(%rdx), %edi
	cmpq	$51, %rdi
	je	.LBB22_12
# BB#7:                                 # %land.lhs.true
	cmpq	$67, %rdi
	je	.LBB22_12
.LBB22_8:                               # %if.end59
	cmpq	%rsi, %rdx
	jne	.LBB22_11
# BB#9:                                 # %land.lhs.true67
	movzbl	(%rcx), %ecx
	cmpq	$51, %rcx
	je	.LBB22_12
# BB#10:                                # %land.lhs.true67
	cmpq	$67, %rcx
	je	.LBB22_12
.LBB22_11:                              # %if.end88
	xorq	%rax, %rax
.LBB22_12:                              # %return
	retq
.Ltmp138:
	.size	condjump_in_parallel_p, .Ltmp138-condjump_in_parallel_p
	.cfi_endproc

	.globl	pc_set
	.align	16, 0x90
	.type	pc_set,@function
pc_set:                                 # @pc_set
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %ecx
	xorq	%rax, %rax
	cmpq	$33, %rcx
	jne	.LBB23_6
# BB#1:                                 # %if.end
	movq	32(%rdi), %rax
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$39, %rdx
	jne	.LBB23_3
# BB#2:                                 # %if.then4
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movl	(%rax), %ecx
.LBB23_3:                               # %if.end8
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$47, %rcx
	jne	.LBB23_5
# BB#4:                                 # %land.lhs.true
	movq	8(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$59, %rcx
	je	.LBB23_6
.LBB23_5:                               # %if.end19
	xorq	%rax, %rax
.LBB23_6:                               # %return
	retq
.Ltmp139:
	.size	pc_set, .Ltmp139-pc_set
	.cfi_endproc

	.globl	any_condjump_p
	.align	16, 0x90
	.type	any_condjump_p,@function
any_condjump_p:                         # @any_condjump_p
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %ecx
	xorq	%rax, %rax
	cmpq	$33, %rcx
	jne	.LBB24_16
# BB#1:                                 # %if.end.i
	movq	32(%rdi), %rcx
	movl	(%rcx), %edx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$39, %rsi
	jne	.LBB24_3
# BB#2:                                 # %if.then4.i
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rcx
	movl	(%rcx), %edx
.LBB24_3:                               # %if.end8.i
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$47, %rdx
	jne	.LBB24_16
# BB#4:                                 # %land.lhs.true.i
	movq	8(%rcx), %rdx
	movzbl	(%rdx), %edx
	cmpq	$59, %rdx
	jne	.LBB24_16
# BB#5:                                 # %land.lhs.true.i
	testq	%rcx, %rcx
	je	.LBB24_16
# BB#6:                                 # %if.end
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$72, %rdx
	jne	.LBB24_16
# BB#7:                                 # %if.end2
	movq	24(%rcx), %rax
	movq	16(%rcx), %rdx
	movzbl	(%rax), %ecx
	movzbl	(%rdx), %edx
	cmpq	$59, %rcx
	jne	.LBB24_10
# BB#8:                                 # %land.lhs.true
	movq	$1, %rax
	cmpq	$51, %rdx
	je	.LBB24_16
# BB#9:                                 # %land.lhs.true
	cmpq	$67, %rdx
	je	.LBB24_16
.LBB24_10:                              # %lor.rhs
	xorq	%rax, %rax
	cmpq	$59, %rdx
	jne	.LBB24_16
# BB#11:                                # %land.rhs
	cmpq	$67, %rcx
	movabsq	$0, %rdx
	movabsq	$1, %rsi
	movq	%rdx, %rax
	jne	.LBB24_13
# BB#12:                                # %land.rhs
	movq	%rsi, %rax
.LBB24_13:                              # %land.rhs
	cmpq	$51, %rcx
	jne	.LBB24_15
# BB#14:                                # %land.rhs
	movq	%rsi, %rdx
.LBB24_15:                              # %land.rhs
	orq	%rdx, %rax
.LBB24_16:                              # %return
	retq
.Ltmp140:
	.size	any_condjump_p, .Ltmp140-any_condjump_p
	.cfi_endproc

	.globl	condjump_label
	.align	16, 0x90
	.type	condjump_label,@function
condjump_label:                         # @condjump_label
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %ecx
	xorq	%rax, %rax
	cmpq	$33, %rcx
	jne	.LBB25_15
# BB#1:                                 # %if.end.i
	movq	32(%rdi), %rcx
	movl	(%rcx), %edx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$39, %rsi
	jne	.LBB25_3
# BB#2:                                 # %if.then4.i
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rcx
	movl	(%rcx), %edx
.LBB25_3:                               # %if.end8.i
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$47, %rdx
	jne	.LBB25_15
# BB#4:                                 # %land.lhs.true.i
	movq	8(%rcx), %rdx
	movzbl	(%rdx), %edx
	cmpq	$59, %rdx
	jne	.LBB25_15
# BB#5:                                 # %land.lhs.true.i
	testq	%rcx, %rcx
	je	.LBB25_15
# BB#6:                                 # %if.end
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$67, %rdx
	jne	.LBB25_8
# BB#7:
	movq	%rcx, %rax
	retq
.LBB25_8:                               # %if.end
	cmpq	$72, %rdx
	jne	.LBB25_15
# BB#9:                                 # %if.end7
	movq	24(%rcx), %rax
	movq	global_rtl(%rip), %rdx
	movq	16(%rcx), %rcx
	cmpq	%rdx, %rax
	jne	.LBB25_12
# BB#10:                                # %land.lhs.true
	movzbl	(%rcx), %esi
	cmpq	$67, %rsi
	jne	.LBB25_12
# BB#11:
	movq	%rcx, %rax
	retq
.LBB25_12:                              # %if.end22
	cmpq	%rdx, %rcx
	jne	.LBB25_14
# BB#13:                                # %land.lhs.true27
	movzbl	(%rax), %ecx
	cmpq	$67, %rcx
	je	.LBB25_15
.LBB25_14:                              # %if.end38
	xorq	%rax, %rax
.LBB25_15:                              # %return
	retq
.Ltmp141:
	.size	condjump_label, .Ltmp141-condjump_label
	.cfi_endproc

	.globl	returnjump_p
	.align	16, 0x90
	.type	returnjump_p,@function
returnjump_p:                           # @returnjump_p
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %eax
	cmpq	$33, %rax
	jne	.LBB26_1
# BB#2:                                 # %if.end
	addq	$32, %rdi
	movabsq	$returnjump_p_1, %rsi
	xorq	%rdx, %rdx
	jmp	for_each_rtx  # TAILCALL
.LBB26_1:                               # %return
	xorq	%rax, %rax
	retq
.Ltmp142:
	.size	returnjump_p, .Ltmp142-returnjump_p
	.cfi_endproc

	.align	16, 0x90
	.type	returnjump_p_1,@function
returnjump_p_1:                         # @returnjump_p_1
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB27_4
# BB#1:                                 # %land.rhs
	movl	(%rcx), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$51, %rcx
	movq	$1, %rax
	je	.LBB27_4
# BB#2:                                 # %lor.rhs
	cmpq	$47, %rcx
	movabsq	$0, %rax
	jne	.LBB27_4
# BB#3:                                 # %land.rhs4
	andq	$16777216, %rdx         # imm = 0x1000000
	movq	$24, %rcx
	shrq	%cl, %rdx
	movq	%rdx, %rax
.LBB27_4:                               # %land.end8
	retq
.Ltmp143:
	.size	returnjump_p_1, .Ltmp143-returnjump_p_1
	.cfi_endproc

	.globl	follow_jumps
	.align	16, 0x90
	.type	follow_jumps,@function
follow_jumps:                           # @follow_jumps
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp150:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp151:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp152:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp153:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp154:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp155:
	.cfi_def_cfa_offset 64
.Ltmp156:
	.cfi_offset %rbx, -48
.Ltmp157:
	.cfi_offset %r12, -40
.Ltmp158:
	.cfi_offset %r13, -32
.Ltmp159:
	.cfi_offset %r14, -24
.Ltmp160:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movzbl	rtx_class+33(%rip), %eax
	xorq	%r12, %r12
	movabsq	$4294967200, %r13       # imm = 0xFFFFFFA0
	andq	$255, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r14, %r15
.LBB28_1:                               # %land.lhs.true
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB28_22 Depth 2
                                        #     Child Loop BB28_26 Depth 2
	movq	%r15, %rdi
	callq	next_active_insn
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB28_33
# BB#2:                                 # %land.lhs.true2
                                        #   in Loop: Header=BB28_1 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$33, %rax
	jne	.LBB28_33
# BB#3:                                 # %land.lhs.true4
                                        #   in Loop: Header=BB28_1 Depth=1
	cmpq	$0, 64(%rbx)
	je	.LBB28_16
# BB#4:                                 # %if.end.i.i
                                        #   in Loop: Header=BB28_1 Depth=1
	movq	32(%rbx), %rax
	movl	(%rax), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$39, %rcx
	movq	%rax, %rdx
	jne	.LBB28_6
# BB#5:                                 # %if.then4.i.i
                                        #   in Loop: Header=BB28_1 Depth=1
	movq	8(%rax), %rdx
	movq	8(%rdx), %rdx
	movl	(%rdx), %esi
.LBB28_6:                               # %if.end8.i.i
                                        #   in Loop: Header=BB28_1 Depth=1
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$47, %rsi
	jne	.LBB28_16
# BB#7:                                 # %land.lhs.true.i.i
                                        #   in Loop: Header=BB28_1 Depth=1
	movq	8(%rdx), %rsi
	movzbl	(%rsi), %esi
	cmpq	$59, %rsi
	jne	.LBB28_16
# BB#8:                                 # %land.lhs.true.i.i
                                        #   in Loop: Header=BB28_1 Depth=1
	testq	%rdx, %rdx
	je	.LBB28_16
# BB#9:                                 # %any_uncondjump_p.exit
                                        #   in Loop: Header=BB28_1 Depth=1
	movq	16(%rdx), %rdx
	movzbl	(%rdx), %edx
	cmpq	$67, %rdx
	jne	.LBB28_16
# BB#10:                                # %any_uncondjump_p.exit
                                        #   in Loop: Header=BB28_1 Depth=1
	cmpq	$105, 8(%rsp)           # 8-byte Folded Reload
	jne	.LBB28_16
# BB#11:                                # %cond.true.i
                                        #   in Loop: Header=BB28_1 Depth=1
	cmpq	$47, %rcx
	je	.LBB28_13
# BB#12:                                # %cond.false.i
                                        #   in Loop: Header=BB28_1 Depth=1
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB28_13:                              # %cond.end18.i
                                        #   in Loop: Header=BB28_1 Depth=1
	testq	%rax, %rax
	je	.LBB28_16
# BB#14:                                # %if.end23.i
                                        #   in Loop: Header=BB28_1 Depth=1
	movq	8(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$59, %rcx
	jne	.LBB28_16
# BB#15:                                # %onlyjump_p.exit
                                        #   in Loop: Header=BB28_1 Depth=1
	movq	16(%rax), %rdi
	callq	side_effects_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB28_17
	.align	16, 0x90
.LBB28_16:                              # %lor.lhs.false
                                        #   in Loop: Header=BB28_1 Depth=1
	movq	32(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$51, %rax
	jne	.LBB28_33
.LBB28_17:                              # %land.lhs.true17
                                        #   in Loop: Header=BB28_1 Depth=1
	movq	24(%rbx), %rax
	testq	%rax, %rax
	je	.LBB28_33
# BB#18:                                # %land.rhs
                                        #   in Loop: Header=BB28_1 Depth=1
	movzbl	(%rax), %eax
	cmpq	$35, %rax
	jne	.LBB28_33
# BB#19:                                # %for.body
                                        #   in Loop: Header=BB28_1 Depth=1
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	jne	.LBB28_29
# BB#20:                                # %for.body
                                        #   in Loop: Header=BB28_1 Depth=1
	cmpq	%rbx, %r15
	je	.LBB28_29
# BB#21:                                # %for.body28.lr.ph
                                        #   in Loop: Header=BB28_1 Depth=1
	movl	flag_test_coverage(%rip), %eax
	testq	%rax, %rax
	movq	%r15, %rax
	movq	%r15, %rcx
	jne	.LBB28_22
	.align	16, 0x90
.LBB28_26:                              # %for.body28
                                        #   Parent Loop BB28_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rcx), %eax
	cmpq	$37, %rax
	jne	.LBB28_28
# BB#27:                                # %land.lhs.true32
                                        #   in Loop: Header=BB28_26 Depth=2
	movl	40(%rcx), %eax
	cmpq	%r13, %rax
	je	.LBB28_36
.LBB28_28:                              # %for.inc
                                        #   in Loop: Header=BB28_26 Depth=2
	movq	24(%rcx), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB28_26
	jmp	.LBB28_29
	.align	16, 0x90
.LBB28_22:                              # %for.body28.us
                                        #   Parent Loop BB28_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB28_25
# BB#23:                                # %land.lhs.true32.us
                                        #   in Loop: Header=BB28_22 Depth=2
	movl	40(%rax), %edx
	cmpq	%r13, %rdx
	je	.LBB28_36
# BB#24:                                # %land.lhs.true32.us
                                        #   in Loop: Header=BB28_22 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jg	.LBB28_36
.LBB28_25:                              # %for.inc.us
                                        #   in Loop: Header=BB28_22 Depth=2
	movq	24(%rax), %rax
	cmpq	%rbx, %rax
	jne	.LBB28_22
	.align	16, 0x90
.LBB28_29:                              # %if.end47
                                        #   in Loop: Header=BB28_1 Depth=1
	movq	64(%rbx), %rdi
	cmpq	%r14, %rdi
	je	.LBB28_35
# BB#30:                                # %if.end53
                                        #   in Loop: Header=BB28_1 Depth=1
	callq	next_active_insn
	testq	%rax, %rax
	je	.LBB28_32
# BB#31:                                # %land.lhs.true59
                                        #   in Loop: Header=BB28_1 Depth=1
	movq	32(%rax), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	je	.LBB28_33
.LBB28_32:                              # %if.end74
                                        #   in Loop: Header=BB28_1 Depth=1
	movq	64(%rbx), %r15
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$10, %rax
	jl	.LBB28_1
.LBB28_33:                              # %for.end79
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r12
	cmpq	$10, %r12
	je	.LBB28_35
# BB#34:                                # %for.end79
	movq	%r15, %r14
.LBB28_35:                              # %for.end79
	movq	%r14, %r15
.LBB28_36:                              # %return
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp161:
	.size	follow_jumps, .Ltmp161-follow_jumps
	.cfi_endproc

	.globl	mark_jump_label
	.align	16, 0x90
	.type	mark_jump_label,@function
mark_jump_label:                        # @mark_jump_label
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp168:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp169:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp170:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp171:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp172:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp173:
	.cfi_def_cfa_offset 64
.Ltmp174:
	.cfi_offset %rbx, -48
.Ltmp175:
	.cfi_offset %r12, -40
.Ltmp176:
	.cfi_offset %r13, -32
.Ltmp177:
	.cfi_offset %r14, -24
.Ltmp178:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	(%rbx), %eax
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%rax, %r12
	andq	$65535, %r12            # imm = 0xFFFF
	leaq	-44(%r12), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$25, %rdx
	ja	.LBB29_25
# BB#1:                                 # %entry
	jmpq	*.LJTI29_0(,%rdx,8)
.LBB29_17:                              # %sw.bb62
	movzbl	3(%r14), %eax
	testq	$8, %rax
	jne	.LBB29_34
# BB#18:                                # %if.then67
	cmpq	$45, %r12
	jne	.LBB29_19
# BB#20:                                # %if.then67
	movabsq	$1, %r14
	jmp	.LBB29_21
.LBB29_24:                              # %sw.bb1
	movq	$1, %r15
	jmp	.LBB29_25
.LBB29_5:                               # %sw.bb8
	movq	8(%rbx), %r13
	movzbl	(%r13), %ecx
	cmpq	$36, %rcx
	jne	.LBB29_6
# BB#9:                                 # %if.end20
	testq	$134217728, %rax        # imm = 0x8000000
	jne	.LBB29_25
# BB#10:                                # %if.end26
	movq	%r13, 8(%rbx)
	testq	%r14, %r14
	je	.LBB29_33
# BB#11:                                # %lor.lhs.false
	movl	(%r14), %eax
	testq	$134217728, %rax        # imm = 0x8000000
	jne	.LBB29_13
# BB#12:                                # %if.end39.thread177
	movl	32(%r13), %eax
	incq	%rax
	movl	%eax, 32(%r13)
	movl	(%r14), %eax
.LBB29_13:                              # %if.then41
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$33, %rax
	jne	.LBB29_15
# BB#14:                                # %if.then45
	movq	%r13, 64(%r14)
	jmp	.LBB29_34
.LBB29_2:                               # %sw.bb2
	testq	%rcx, %r15
	je	.LBB29_34
# BB#3:                                 # %if.end
	testq	$67108864, %rax         # imm = 0x4000000
	je	.LBB29_25
# BB#4:                                 # %if.then6
	movq	%rbx, %rdi
	callq	get_pool_constant
	movq	%rax, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	mark_jump_label
	jmp	.LBB29_25
.LBB29_6:                               # %sw.bb8
	cmpq	$37, %rcx
	jne	.LBB29_8
# BB#7:                                 # %land.lhs.true
	movl	40(%r13), %eax
	movabsq	$4294967208, %rcx       # imm = 0xFFFFFFA8
	cmpq	%rcx, %rax
	jne	.LBB29_8
.LBB29_25:                              # %sw.epilog
	movzbl	rtx_length(%r12), %edx
	testq	%rdx, %rdx
	je	.LBB29_34
# BB#26:                                # %for.body87.lr.ph
	movq	rtx_format(,%r12,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	.align	16, 0x90
.LBB29_27:                              # %for.body87
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB29_32 Depth 2
	movq	%rdx, %r12
	movq	(%rsp), %rax            # 8-byte Reload
	movzbl	-1(%rax,%r12), %eax
	leaq	-1(%r12), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB29_28
# BB#31:                                # %for.cond105.preheader
                                        #   in Loop: Header=BB29_27 Depth=1
	movq	(%rbx,%r12,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r13
	jle	.LBB29_29
	.align	16, 0x90
.LBB29_32:                              # %for.body113
                                        #   Parent Loop BB29_27 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r13,8), %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	mark_jump_label
	movq	(%rbx,%r12,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r13
	cmpq	%rdx, %rsi
	jl	.LBB29_32
	jmp	.LBB29_29
	.align	16, 0x90
.LBB29_28:                              # %for.body87
                                        #   in Loop: Header=BB29_27 Depth=1
	cmpq	$101, %rax
	jne	.LBB29_29
# BB#30:                                # %if.then93
                                        #   in Loop: Header=BB29_27 Depth=1
	movq	(%rbx,%r12,8), %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	mark_jump_label
.LBB29_29:                              # %for.cond84.backedge
                                        #   in Loop: Header=BB29_27 Depth=1
	movq	$32, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB29_27
	jmp	.LBB29_34
.LBB29_19:
	movabsq	$0, %r14
.LBB29_21:                              # %if.then67
	movq	8(%rbx,%r14,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB29_34
# BB#22:
	movq	$1, %r12
	.align	16, 0x90
.LBB29_23:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%r12,8), %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	mark_jump_label
	movq	8(%rbx,%r14,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r12
	cmpq	%rdx, %rsi
	jl	.LBB29_23
	jmp	.LBB29_34
.LBB29_33:                              # %if.end39
	movl	32(%r13), %eax
	incq	%rax
	movl	%eax, 32(%r13)
	jmp	.LBB29_34
.LBB29_15:                              # %if.else
	movq	$13, %rsi
	movq	%r14, %rdi
	movq	%r13, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB29_34
# BB#16:                                # %if.then51
	movq	56(%r14), %rcx
	movq	$4, %rdi
	movq	$13, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_ue
	movq	%rax, 56(%r14)
.LBB29_34:                              # %for.end127
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB29_8:                               # %if.then19
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.mark_jump_label, %rcx
	movq	$1451, %rsi             # imm = 0x5AB
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp179:
	.size	mark_jump_label, .Ltmp179-mark_jump_label
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI29_0:
	.quad	.LBB29_17
	.quad	.LBB29_17
	.quad	.LBB29_25
	.quad	.LBB29_25
	.quad	.LBB29_25
	.quad	.LBB29_34
	.quad	.LBB29_34
	.quad	.LBB29_25
	.quad	.LBB29_25
	.quad	.LBB29_25
	.quad	.LBB29_34
	.quad	.LBB29_34
	.quad	.LBB29_25
	.quad	.LBB29_25
	.quad	.LBB29_25
	.quad	.LBB29_34
	.quad	.LBB29_25
	.quad	.LBB29_34
	.quad	.LBB29_25
	.quad	.LBB29_34
	.quad	.LBB29_25
	.quad	.LBB29_25
	.quad	.LBB29_24
	.quad	.LBB29_5
	.quad	.LBB29_2
	.quad	.LBB29_34

	.text
	.globl	delete_jump
	.align	16, 0x90
	.type	delete_jump,@function
delete_jump:                            # @delete_jump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp183:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp184:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp185:
	.cfi_def_cfa_offset 32
.Ltmp186:
	.cfi_offset %rbx, -24
.Ltmp187:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB30_11
# BB#1:                                 # %cond.true
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB30_3
# BB#2:                                 # %cond.false
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB30_3:                               # %cond.end15
	testq	%rax, %rax
	je	.LBB30_11
# BB#4:                                 # %land.lhs.true
	movq	8(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$59, %rax
	jne	.LBB30_11
# BB#5:                                 # %if.then
	movq	56(%r14), %rbx
	jmp	.LBB30_6
.LBB30_11:                              # %if.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB30_9:                               # %if.end.i
                                        #   in Loop: Header=BB30_6 Depth=1
	movq	%r14, %rsi
	callq	delete_prior_computation
	.align	16, 0x90
.LBB30_6:                               # %if.then
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB30_10
# BB#7:                                 # %for.body.i
                                        #   in Loop: Header=BB30_6 Depth=1
	movq	%rbx, %rdi
	movl	(%rdi), %eax
	movq	16(%rdi), %rbx
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$65536, %rax            # imm = 0x10000
	jne	.LBB30_6
# BB#8:                                 # %lor.lhs.false.i
                                        #   in Loop: Header=BB30_6 Depth=1
	movq	8(%rdi), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB30_6
	jmp	.LBB30_9
.LBB30_10:                              # %delete_computation.exit
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	delete_related_insns  # TAILCALL
.Ltmp188:
	.size	delete_jump, .Ltmp188-delete_jump
	.cfi_endproc

	.globl	next_nondeleted_insn
	.align	16, 0x90
	.type	next_nondeleted_insn,@function
next_nondeleted_insn:                   # @next_nondeleted_insn
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	.LBB31_2
	.align	16, 0x90
.LBB31_1:                               # %while.body
                                        #   in Loop: Header=BB31_2 Depth=1
	movq	24(%rdi), %rdi
.LBB31_2:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	3(%rdi), %eax
	testq	$8, %rax
	jne	.LBB31_1
# BB#3:                                 # %while.end
	movq	%rdi, %rax
	retq
.Ltmp189:
	.size	next_nondeleted_insn, .Ltmp189-next_nondeleted_insn
	.cfi_endproc

	.globl	delete_for_peephole
	.align	16, 0x90
	.type	delete_for_peephole,@function
delete_for_peephole:                    # @delete_for_peephole
	.cfi_startproc
# BB#0:                                 # %entry
	.align	16, 0x90
.LBB32_1:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rax
	movl	(%rax), %edx
	movq	24(%rax), %rdi
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$37, %rcx
	je	.LBB32_6
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB32_1 Depth=1
	movq	16(%rax), %rcx
	orq	$134217728, %rdx        # imm = 0x8000000
	movl	%edx, (%rax)
	testq	%rcx, %rcx
	je	.LBB32_4
# BB#3:                                 # %if.then6
                                        #   in Loop: Header=BB32_1 Depth=1
	movq	%rdi, 24(%rcx)
.LBB32_4:                               # %if.end
                                        #   in Loop: Header=BB32_1 Depth=1
	testq	%rdi, %rdi
	je	.LBB32_6
# BB#5:                                 # %if.then11
                                        #   in Loop: Header=BB32_1 Depth=1
	movq	%rcx, 16(%rdi)
.LBB32_6:                               # %if.end16
                                        #   in Loop: Header=BB32_1 Depth=1
	cmpq	%rsi, %rax
	jne	.LBB32_1
# BB#7:                                 # %while.end
	retq
.Ltmp190:
	.size	delete_for_peephole, .Ltmp190-delete_for_peephole
	.cfi_endproc

	.globl	never_reached_warning
	.align	16, 0x90
	.type	never_reached_warning,@function
never_reached_warning:                  # @never_reached_warning
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp194:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp195:
	.cfi_def_cfa_offset 24
.Ltmp196:
	.cfi_offset %rbx, -24
.Ltmp197:
	.cfi_offset %r14, -16
	movl	warn_notreached(%rip), %eax
	testq	%rax, %rax
	je	.LBB33_20
# BB#1:                                 # %entry
	testq	%rdi, %rdi
	je	.LBB33_20
# BB#2:                                 # %for.body.lr.ph
	xorq	%rax, %rax
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	movabsq	$0, %r8
	movabsq	$1, %r9
	xorq	%rdx, %rdx
	xorq	%r11, %r11
	xorq	%r14, %r14
	.align	16, 0x90
.LBB33_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rdi), %ebx
	andq	$65535, %rbx            # imm = 0xFFFF
	testq	%rsi, %rsi
	jne	.LBB33_5
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB33_3 Depth=1
	cmpq	$36, %rbx
	je	.LBB33_18
.LBB33_5:                               # %if.end4
                                        #   in Loop: Header=BB33_3 Depth=1
	cmpq	$37, %rbx
	jne	.LBB33_12
# BB#6:                                 # %land.lhs.true8
                                        #   in Loop: Header=BB33_3 Depth=1
	movslq	40(%rdi), %rcx
	testq	%rcx, %rcx
	js	.LBB33_12
# BB#7:                                 # %if.then10
                                        #   in Loop: Header=BB33_3 Depth=1
	testq	%r14, %r14
	jne	.LBB33_9
# BB#8:                                 #   in Loop: Header=BB33_3 Depth=1
	movq	%rdi, %r14
	movq	%rdx, %rbx
	jmp	.LBB33_15
	.align	16, 0x90
.LBB33_12:                              # %if.else21
                                        #   in Loop: Header=BB33_3 Depth=1
	movzbl	rtx_class(%rbx), %ecx
	cmpq	$105, %rcx
	jne	.LBB33_13
# BB#14:                                # %if.then28
                                        #   in Loop: Header=BB33_3 Depth=1
	movq	$1, %rbx
	testq	%r10, %rax
	je	.LBB33_15
	jmp	.LBB33_18
	.align	16, 0x90
.LBB33_13:                              #   in Loop: Header=BB33_3 Depth=1
	movq	%rdx, %rbx
	jmp	.LBB33_15
.LBB33_9:                               # %if.else
                                        #   in Loop: Header=BB33_3 Depth=1
	movl	40(%r14), %ebx
	andq	%r10, %rcx
	cmpq	%rcx, %rbx
	movq	%r8, %rcx
	je	.LBB33_11
# BB#10:                                # %if.else
                                        #   in Loop: Header=BB33_3 Depth=1
	movq	%r9, %rcx
.LBB33_11:                              # %if.else
                                        #   in Loop: Header=BB33_3 Depth=1
	orq	%rcx, %r11
	movq	%rdx, %rbx
	.align	16, 0x90
.LBB33_15:                              # %if.end33
                                        #   in Loop: Header=BB33_3 Depth=1
	cmpq	%rsi, %rdi
	movq	$1, %rcx
	je	.LBB33_17
# BB#16:                                # %if.end33
                                        #   in Loop: Header=BB33_3 Depth=1
	movq	%rax, %rcx
.LBB33_17:                              # %if.end33
                                        #   in Loop: Header=BB33_3 Depth=1
	movq	24(%rdi), %rdi
	testq	%rdi, %rdi
	movq	%rcx, %rax
	movq	%rbx, %rdx
	jne	.LBB33_3
.LBB33_18:                              # %for.end
	testq	%r10, %r11
	je	.LBB33_20
# BB#19:                                # %for.end
	testq	%r10, %rdx
	je	.LBB33_20
# BB#21:                                # %if.then43
	movq	32(%r14), %rdi
	movl	40(%r14), %esi
	movabsq	$.L.str1, %rdx
	xorq	%rax, %rax
	popq	%rbx
	popq	%r14
	jmp	warning_with_file_and_line  # TAILCALL
.LBB33_20:                              # %if.end49
	popq	%rbx
	popq	%r14
	retq
.Ltmp198:
	.size	never_reached_warning, .Ltmp198-never_reached_warning
	.cfi_endproc

	.globl	redirect_jump_1
	.align	16, 0x90
	.type	redirect_jump_1,@function
redirect_jump_1:                        # @redirect_jump_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp203:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp204:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp205:
	.cfi_def_cfa_offset 32
.Ltmp206:
	.cfi_offset %rbx, -32
.Ltmp207:
	.cfi_offset %r14, -24
.Ltmp208:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r14
	callq	num_validated_changes
	movq	32(%r14), %rcx
	movzbl	(%rcx), %edx
	movq	%rax, %rbx
	cmpq	$39, %rdx
	jne	.LBB34_1
# BB#2:                                 # %if.then
	movq	8(%rcx), %rdi
	addq	$8, %rdi
	jmp	.LBB34_3
.LBB34_1:
	leaq	32(%r14), %rdi
.LBB34_3:                               # %if.end
	movq	64(%r14), %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	redirect_exp_1
	callq	num_validated_changes
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rax
	jle	.LBB34_4
# BB#5:                                 # %if.end
	movabsq	$1, %rax
	jmp	.LBB34_6
.LBB34_4:
	movabsq	$0, %rax
.LBB34_6:                               # %if.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp209:
	.size	redirect_jump_1, .Ltmp209-redirect_jump_1
	.cfi_endproc

	.align	16, 0x90
	.type	redirect_exp_1,@function
redirect_exp_1:                         # @redirect_exp_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp216:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp217:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp218:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp219:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp220:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp221:
	.cfi_def_cfa_offset 80
.Ltmp222:
	.cfi_offset %rbx, -48
.Ltmp223:
	.cfi_offset %r12, -40
.Ltmp224:
	.cfi_offset %r13, -32
.Ltmp225:
	.cfi_offset %r14, -24
.Ltmp226:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	(%r15), %r8
	movzbl	(%r8), %eax
	movq	%rcx, %r14
	cmpq	$67, %rax
	jne	.LBB35_6
# BB#1:                                 # %if.then
	movq	8(%r8), %rcx
	cmpq	%rsi, %rcx
	jne	.LBB35_15
# BB#2:                                 # %if.then2
	testq	%rdx, %rdx
	je	.LBB35_4
# BB#3:                                 # %if.then3
	movq	$67, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_u00
	jmp	.LBB35_5
.LBB35_6:                               # %if.else7
	cmpq	$51, %rax
	jne	.LBB35_10
# BB#7:                                 # %if.else7
	testq	%rsi, %rsi
	jne	.LBB35_10
# BB#8:                                 # %if.then10
	movq	$67, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_u00
	leaq	32(%r14), %rcx
	cmpq	%r15, %rcx
	jne	.LBB35_5
# BB#9:                                 # %if.then16
	movq	global_rtl(%rip), %rdx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	jmp	.LBB35_5
.LBB35_10:                              # %if.end21
	cmpq	$47, %rax
	jne	.LBB35_15
# BB#11:                                # %if.end21
	testq	%rdx, %rdx
	jne	.LBB35_15
# BB#12:                                # %land.lhs.true25
	movq	8(%r8), %rcx
	movq	global_rtl(%rip), %rdi
	cmpq	%rdi, %rcx
	jne	.LBB35_15
# BB#13:                                # %land.lhs.true30
	movq	16(%r8), %rcx
	movzbl	(%rcx), %edi
	cmpq	$67, %rdi
	jne	.LBB35_15
# BB#14:                                # %land.lhs.true37
	movq	8(%rcx), %rcx
	cmpq	%rsi, %rcx
	je	.LBB35_4
.LBB35_15:                              # %if.end48
	movq	%rsi, %r9
	movq	%r9, 24(%rsp)           # 8-byte Spill
	movq	%rdx, %rsi
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%r14, %r13
	movzbl	rtx_length(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB35_20
# BB#16:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	.align	16, 0x90
.LBB35_17:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB35_23 Depth 2
	movq	%rdx, %r10
	movq	(%rsp), %rax            # 8-byte Reload
	movzbl	-1(%rax,%r10), %eax
	leaq	-1(%r10), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB35_18
# BB#21:                                # %for.cond71.preheader
                                        #   in Loop: Header=BB35_17 Depth=1
	movq	(%r8,%r10,8), %rdi
	movslq	(%rdi), %rax
	testq	%rax, %rax
	movq	%r13, %rbx
	movq	%rsi, %r13
	movq	%r9, %r15
	jle	.LBB35_19
# BB#22:                                #   in Loop: Header=BB35_17 Depth=1
	movq	$8, %r14
	movq	$1, %r12
	.align	16, 0x90
.LBB35_23:                              # %for.body77
                                        #   Parent Loop BB35_17 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	addq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	movq	%r8, %r13
	movq	%r10, %r15
	callq	redirect_exp_1
	movq	%r15, %r10
	movq	%r13, %r8
	movq	16(%rsp), %r13          # 8-byte Reload
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	(%r8,%r10,8), %rdi
	movslq	(%rdi), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r12
	addq	$8, %r14
	cmpq	%rax, %rdx
	jl	.LBB35_23
	jmp	.LBB35_19
	.align	16, 0x90
.LBB35_18:                              # %for.body
                                        #   in Loop: Header=BB35_17 Depth=1
	cmpq	$101, %rax
	movq	%r13, %rbx
	movq	%rsi, %r13
	movq	%r9, %r15
	jne	.LBB35_19
# BB#24:                                # %if.then59
                                        #   in Loop: Header=BB35_17 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	leaq	8(%r8,%rax,8), %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	movq	%r8, %r14
	callq	redirect_exp_1
	movq	%r14, %r8
.LBB35_19:                              # %for.cond.backedge
                                        #   in Loop: Header=BB35_17 Depth=1
	movq	%r15, %r9
	movq	%r13, %rsi
	movq	%rbx, %r13
	movq	$32, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB35_17
.LBB35_20:                              # %for.end87
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB35_4:                               # %if.else
	movq	$51, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_
.LBB35_5:                               # %if.end
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	validate_change  # TAILCALL
.Ltmp227:
	.size	redirect_exp_1, .Ltmp227-redirect_exp_1
	.cfi_endproc

	.globl	redirect_jump
	.align	16, 0x90
	.type	redirect_jump,@function
redirect_jump:                          # @redirect_jump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp234:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp235:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp236:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp237:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp238:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp239:
	.cfi_def_cfa_offset 64
.Ltmp240:
	.cfi_offset %rbx, -48
.Ltmp241:
	.cfi_offset %r12, -40
.Ltmp242:
	.cfi_offset %r13, -32
.Ltmp243:
	.cfi_offset %r14, -24
.Ltmp244:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	64(%rbx), %r15
	movq	%rsi, %r12
	cmpq	%r12, %r15
	je	.LBB36_17
# BB#1:                                 # %if.end
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$39, %rcx
	jne	.LBB36_2
# BB#3:                                 # %if.then.i
	movq	8(%rax), %rdi
	addq	$8, %rdi
	jmp	.LBB36_4
.LBB36_2:
	leaq	32(%rbx), %rdi
.LBB36_4:                               # %if.end.i
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	callq	redirect_exp_1
	callq	num_validated_changes
	xorq	%r13, %r13
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB36_18
# BB#5:                                 # %redirect_exp.exit
	callq	apply_change_group
	testq	%r14, %rax
	je	.LBB36_18
# BB#6:                                 # %if.end2
	testq	%r12, %r12
	movq	%r12, 64(%rbx)
	je	.LBB36_12
# BB#7:                                 # %if.end10
	movl	32(%r12), %eax
	incq	%rax
	movl	%eax, 32(%r12)
	testq	%r15, %r15
	je	.LBB36_17
# BB#8:                                 # %land.lhs.true13
	movq	24(%r15), %rax
	testq	%rax, %rax
	je	.LBB36_13
# BB#9:                                 # %land.lhs.true18
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB36_13
# BB#10:                                # %land.lhs.true23
	movl	40(%rax), %eax
	movabsq	$4294967205, %rcx       # imm = 0xFFFFFFA5
	cmpq	%rcx, %rax
	jne	.LBB36_12
# BB#11:                                # %if.then31
	movabsq	$4294967205, %rdi       # imm = 0xFFFFFFA5
	movq	%r12, %rsi
	callq	emit_note_after
	jmp	.LBB36_13
.LBB36_12:                              # %if.end33
	testq	%r15, %r15
	je	.LBB36_17
.LBB36_13:                              # %land.lhs.true35
	movl	32(%r15), %eax
	decq	%rax
	testq	%r14, %rax
	movl	%eax, 32(%r15)
	jne	.LBB36_17
# BB#14:                                # %land.lhs.true35
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%r14, %rax
	je	.LBB36_17
# BB#15:                                # %land.lhs.true42
	movl	8(%r15), %eax
	testq	%rax, %rax
	je	.LBB36_17
# BB#16:                                # %if.then47
	movq	%r15, %rdi
	callq	delete_related_insns
.LBB36_17:                              # %return
	movq	$1, %r13
.LBB36_18:                              # %return
	movq	%r13, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp245:
	.size	redirect_jump, .Ltmp245-redirect_jump
	.cfi_endproc

	.globl	invert_jump_1
	.align	16, 0x90
	.type	invert_jump_1,@function
invert_jump_1:                          # @invert_jump_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp251:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp252:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp253:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp254:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp255:
	.cfi_def_cfa_offset 48
.Ltmp256:
	.cfi_offset %rbx, -40
.Ltmp257:
	.cfi_offset %r12, -32
.Ltmp258:
	.cfi_offset %r14, -24
.Ltmp259:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r12
	callq	num_validated_changes
	movq	%rax, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rbx
	movq	%r12, %rdi
	callq	invert_exp_1
	callq	num_validated_changes
	movq	%rax, %rcx
	andq	%r15, %rcx
	xorq	%rax, %rax
	cmpq	%rbx, %rcx
	je	.LBB37_7
# BB#1:                                 # %if.end
	callq	num_validated_changes
	movq	32(%r12), %rcx
	movzbl	(%rcx), %edx
	movq	%rax, %r15
	cmpq	$39, %rdx
	jne	.LBB37_2
# BB#3:                                 # %if.then.i
	movq	8(%rcx), %rdi
	addq	$8, %rdi
	jmp	.LBB37_4
.LBB37_2:
	leaq	32(%r12), %rdi
.LBB37_4:                               # %redirect_jump_1.exit
	movq	64(%r12), %rsi
	movq	%r14, %rdx
	movq	%r12, %rcx
	callq	redirect_exp_1
	callq	num_validated_changes
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	cmpq	%r15, %rax
	jle	.LBB37_5
# BB#6:                                 # %redirect_jump_1.exit
	movabsq	$1, %rax
	jmp	.LBB37_7
.LBB37_5:
	movabsq	$0, %rax
.LBB37_7:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp260:
	.size	invert_jump_1, .Ltmp260-invert_jump_1
	.cfi_endproc

	.align	16, 0x90
	.type	invert_exp_1,@function
invert_exp_1:                           # @invert_exp_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp266:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp267:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp268:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp269:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp270:
	.cfi_def_cfa_offset 48
.Ltmp271:
	.cfi_offset %rbx, -40
.Ltmp272:
	.cfi_offset %r12, -32
.Ltmp273:
	.cfi_offset %r14, -24
.Ltmp274:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movzbl	(%r14), %eax
	cmpq	$33, %rax
	jne	.LBB38_6
# BB#1:                                 # %if.end.i
	movq	32(%r14), %rax
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$39, %rdx
	jne	.LBB38_3
# BB#2:                                 # %if.then4.i
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movl	(%rax), %ecx
.LBB38_3:                               # %if.end8.i
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$47, %rcx
	jne	.LBB38_6
# BB#4:                                 # %land.lhs.true.i
	movq	8(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$59, %rcx
	jne	.LBB38_6
# BB#5:                                 # %land.lhs.true.i
	testq	%rax, %rax
	je	.LBB38_6
# BB#8:                                 # %if.end
	movq	16(%rax), %rbx
	movzbl	(%rbx), %eax
	cmpq	$72, %rax
	jne	.LBB38_14
# BB#9:                                 # %if.then1
	movq	8(%rbx), %r15
	movzbl	(%r15), %edi
	movzbl	rtx_class(%rdi), %eax
	cmpq	$60, %rax
	jne	.LBB38_13
# BB#10:                                # %reversed_comparison_code.exit
	movq	8(%r15), %rsi
	movq	16(%r15), %rdx
	movq	%r14, %rcx
	callq	reversed_comparison_code_parts
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB38_13
# BB#11:                                # %if.then7
	movq	8(%r15), %rdx
	movq	16(%r15), %rcx
	movzbl	2(%r15), %esi
	addq	$8, %rbx
	movq	%rax, %rdi
	callq	gen_rtx_fmt_ee
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	jmp	.LBB38_12
.LBB38_13:                              # %if.end21
	movq	16(%rbx), %r15
	movq	24(%rbx), %rdx
	leaq	24(%rbx), %r12
	leaq	16(%rbx), %rsi
	movq	$1, %rcx
	movq	%r14, %rdi
	callq	validate_change
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
.LBB38_12:                              # %if.then7
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	validate_change  # TAILCALL
.LBB38_6:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.invert_exp_1, %rcx
	movq	$2106, %rsi             # imm = 0x83A
	jmp	.LBB38_7
.LBB38_14:                              # %if.else
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.invert_exp_1, %rcx
	movq	$2139, %rsi             # imm = 0x85B
.LBB38_7:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp275:
	.size	invert_exp_1, .Ltmp275-invert_exp_1
	.cfi_endproc

	.globl	invert_jump
	.align	16, 0x90
	.type	invert_jump,@function
invert_jump:                            # @invert_jump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp282:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp283:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp284:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp285:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp286:
	.cfi_def_cfa_offset 48
.Ltmp287:
	.cfi_offset %rbx, -48
.Ltmp288:
	.cfi_offset %r12, -40
.Ltmp289:
	.cfi_offset %r13, -32
.Ltmp290:
	.cfi_offset %r14, -24
.Ltmp291:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	invert_exp_1
	callq	num_validated_changes
	xorq	%r14, %r14
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB39_6
# BB#1:                                 # %invert_exp.exit
	callq	apply_change_group
	testq	%r13, %rax
	je	.LBB39_6
# BB#2:                                 # %if.end
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	redirect_jump
	movq	%rbx, %rdi
	testq	%r13, %rax
	je	.LBB39_4
# BB#3:                                 # %if.then3
	callq	invert_br_probabilities
	movq	$1, %r14
	jmp	.LBB39_6
.LBB39_4:                               # %if.end4
	callq	invert_exp_1
	callq	num_validated_changes
	testq	%r13, %rax
	je	.LBB39_7
# BB#5:                                 # %invert_exp.exit16
	callq	apply_change_group
	testq	%r13, %rax
	je	.LBB39_7
.LBB39_6:                               # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB39_7:                               # %if.then7
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.invert_jump, %rcx
	movq	$2202, %rsi             # imm = 0x89A
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp292:
	.size	invert_jump, .Ltmp292-invert_jump
	.cfi_endproc

	.globl	rtx_renumbered_equal_p
	.align	16, 0x90
	.type	rtx_renumbered_equal_p,@function
rtx_renumbered_equal_p:                 # @rtx_renumbered_equal_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp299:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp300:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp301:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp302:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp303:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp304:
	.cfi_def_cfa_offset 80
.Ltmp305:
	.cfi_offset %rbx, -48
.Ltmp306:
	.cfi_offset %r12, -40
.Ltmp307:
	.cfi_offset %r13, -32
.Ltmp308:
	.cfi_offset %r14, -24
.Ltmp309:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r12
	cmpq	%r15, %r12
	movq	$1, %r14
	je	.LBB40_91
# BB#1:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB40_2:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r12), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	je	.LBB40_5
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	$63, %rdx
	jne	.LBB40_27
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB40_2 Depth=1
	movq	8(%r12), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$61, %rcx
	jne	.LBB40_27
	.align	16, 0x90
.LBB40_5:                               # %land.lhs.true6
                                        #   in Loop: Header=BB40_2 Depth=1
	movl	(%r15), %ebx
	movq	%rbx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	je	.LBB40_8
# BB#6:                                 # %land.lhs.true6
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	$63, %rcx
	jne	.LBB40_27
# BB#7:                                 # %land.lhs.true14
                                        #   in Loop: Header=BB40_2 Depth=1
	movq	8(%r15), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$61, %rcx
	je	.LBB40_8
.LBB40_27:                              # %if.end131
                                        #   in Loop: Header=BB40_2 Depth=1
	movl	(%r15), %ecx
	movq	%rcx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	%rsi, %rdx
	jne	.LBB40_90
# BB#28:                                # %if.end137
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	$43, %rdx
	jg	.LBB40_30
# BB#29:                                # %if.end137
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	$36, %rdx
	jne	.LBB40_38
	jmp	.LBB40_90
	.align	16, 0x90
.LBB40_30:                              # %if.end137
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	$53, %rdx
	jg	.LBB40_32
# BB#31:                                # %if.end137
                                        #   in Loop: Header=BB40_2 Depth=1
	leaq	-44(%rdx), %rsi
	andq	%r13, %rsi
	cmpq	$2, %rsi
	jae	.LBB40_38
	jmp	.LBB40_90
	.align	16, 0x90
.LBB40_32:                              # %if.end137
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	$66, %rdx
	jg	.LBB40_35
# BB#33:                                # %if.end137
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	$54, %rdx
	je	.LBB40_47
# BB#34:                                # %if.end137
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	$59, %rdx
	jne	.LBB40_38
	jmp	.LBB40_90
.LBB40_35:                              # %if.end137
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	$67, %rdx
	je	.LBB40_50
# BB#36:                                # %if.end137
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	$68, %rdx
	je	.LBB40_57
# BB#37:                                # %if.end137
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	$69, %rdx
	je	.LBB40_90
	.align	16, 0x90
.LBB40_38:                              # %sw.epilog
                                        #   in Loop: Header=BB40_2 Depth=1
	xorq	%rax, %rcx
	testq	$16711680, %rcx         # imm = 0xFF0000
	jne	.LBB40_90
# BB#39:                                # %if.end193
                                        #   in Loop: Header=BB40_2 Depth=1
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$102, %rax
	jne	.LBB40_40
# BB#60:                                # %land.lhs.true205
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	$75, %rdx
	jne	.LBB40_42
# BB#61:                                # %land.lhs.true205.if.else245_crit_edge
                                        #   in Loop: Header=BB40_2 Depth=1
	movzbl	rtx_class+75(%rip), %eax
	movq	$75, %rdx
	jmp	.LBB40_62
	.align	16, 0x90
.LBB40_40:                              # %lor.lhs.false199
                                        #   in Loop: Header=BB40_2 Depth=1
	movzbl	rtx_class(%rdx), %eax
	cmpq	$99, %rax
	jne	.LBB40_62
# BB#41:                                # %lor.lhs.false199
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	$75, %rdx
	jne	.LBB40_42
.LBB40_62:                              # %if.else245
                                        #   in Loop: Header=BB40_2 Depth=1
	andq	$255, %rax
	cmpq	$49, %rax
	jne	.LBB40_63
# BB#69:                                # %if.then283
                                        #   in Loop: Header=BB40_2 Depth=1
	movq	8(%r12), %r12
	movq	8(%r15), %r15
	cmpq	%r15, %r12
	jne	.LBB40_2
	jmp	.LBB40_91
.LBB40_47:                              # %sw.bb138
	movq	8(%r12), %rax
	movq	8(%r15), %rcx
	cmpq	%rcx, %rax
	jne	.LBB40_48
# BB#49:                                # %sw.bb138
	movabsq	$1, %r14
	jmp	.LBB40_91
.LBB40_8:                               # %if.then21
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	$16, %rcx
	movq	%rbx, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	xorq	%r14, %r14
	cmpq	%rsi, %rax
	jne	.LBB40_91
# BB#9:                                 # %if.end29
	movq	reg_renumber(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB40_92
# BB#10:                                # %if.end32
	cmpq	$63, %rdx
	jne	.LBB40_13
# BB#11:                                # %if.then34
	movq	8(%r12), %rdx
	movslq	8(%rdx), %r13
	movswq	(%rcx,%r13,2), %rdi
	movl	16(%r12), %r12d
	testq	%rdi, %rdi
	js	.LBB40_16
# BB#12:                                # %if.then46
	movzbl	2(%rdx), %esi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	subreg_regno_offset
	movl	(%r15), %ebx
	jmp	.LBB40_14
.LBB40_50:                              # %sw.bb146
	movq	8(%r12), %rdi
	orq	%rcx, %rax
	testq	$134217728, %rax        # imm = 0x8000000
	je	.LBB40_54
# BB#51:                                # %if.then155
	movq	8(%r15), %rax
	cmpq	%rax, %rdi
	jne	.LBB40_52
# BB#53:                                # %if.then155
	movabsq	$1, %r14
	jmp	.LBB40_91
.LBB40_57:                              # %sw.bb175
	movq	8(%r12), %rax
	movq	8(%r15), %rcx
	cmpq	%rcx, %rax
	jne	.LBB40_58
# BB#59:                                # %sw.bb175
	movabsq	$1, %r14
	jmp	.LBB40_91
.LBB40_48:
	movabsq	$0, %r14
	jmp	.LBB40_91
.LBB40_42:                              # %if.then208
	movq	8(%r12), %rdi
	movq	8(%r15), %rsi
	callq	rtx_renumbered_equal_p
	testq	%r13, %rax
	je	.LBB40_44
# BB#43:                                # %land.lhs.true217
	movq	16(%r12), %rdi
	movq	16(%r15), %rsi
	callq	rtx_renumbered_equal_p
	testq	%r13, %rax
	movq	$1, %r14
	jne	.LBB40_91
.LBB40_44:
	leaq	16(%r15), %rax
	movq	8(%r12), %rdi
	movq	(%rax), %rsi
	callq	rtx_renumbered_equal_p
	xorq	%r14, %r14
	testq	%r13, %rax
	je	.LBB40_91
# BB#45:                                # %land.rhs234
	movq	16(%r12), %rdi
	movq	8(%r15), %rsi
	callq	rtx_renumbered_equal_p
	testq	%r13, %rax
	jne	.LBB40_68
# BB#46:
	movabsq	$0, %r14
	jmp	.LBB40_91
.LBB40_58:
	movabsq	$0, %r14
	jmp	.LBB40_91
.LBB40_54:                              # %if.end164
	callq	next_real_insn
	movq	8(%r15), %rdi
	movq	%rax, %rbx
	callq	next_real_insn
	cmpq	%rax, %rbx
	jne	.LBB40_55
# BB#56:                                # %if.end164
	movabsq	$1, %r14
	jmp	.LBB40_91
.LBB40_52:
	movabsq	$0, %r14
	jmp	.LBB40_91
.LBB40_63:                              # %if.else245
	cmpq	$50, %rax
	je	.LBB40_65
# BB#64:                                # %if.else245
	cmpq	$60, %rax
	jne	.LBB40_70
.LBB40_65:                              # %if.then257
	movq	8(%r12), %rdi
	movq	8(%r15), %rsi
	callq	rtx_renumbered_equal_p
	xorq	%r14, %r14
	testq	%r13, %rax
	je	.LBB40_91
# BB#66:                                # %land.rhs266
	movq	16(%r12), %rdi
	movq	16(%r15), %rsi
	callq	rtx_renumbered_equal_p
	testq	%r13, %rax
	je	.LBB40_67
.LBB40_68:                              # %land.rhs266
	movabsq	$1, %r14
	jmp	.LBB40_91
.LBB40_92:                              # %if.then31
	movq	%r12, %rdi
	movq	%r15, %rsi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	rtx_equal_p  # TAILCALL
.LBB40_13:                              # %if.else
	movslq	8(%r12), %rax
	movswq	(%rcx,%rax,2), %r13
	testq	%r13, %r13
	jns	.LBB40_15
.LBB40_14:                              # %if.else
	movq	%rax, %r13
.LBB40_15:                              # %if.else
	xorq	%r12, %r12
.LBB40_16:                              # %if.end74
	movq	%rbx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB40_19
# BB#17:                                # %if.then79
	movq	8(%r15), %rcx
	movslq	8(%rcx), %rax
	movq	reg_renumber(%rip), %rdx
	movswq	(%rdx,%rax,2), %rdi
	movl	16(%r15), %edx
	testq	%rdi, %rdi
	js	.LBB40_22
# BB#18:                                # %if.then94
	movzbl	2(%rcx), %esi
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movq	%rbx, %rcx
	callq	subreg_regno_offset
	jmp	.LBB40_21
.LBB40_19:                              # %if.else109
	movslq	8(%r15), %rcx
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%rcx,2), %rax
	testq	%rax, %rax
	jns	.LBB40_21
# BB#20:                                # %if.else109
	movq	%rcx, %rax
.LBB40_21:                              # %if.else109
	xorq	%rdx, %rdx
.LBB40_22:                              # %if.end123
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%r14, %r14
	testq	%rsi, %rsi
	js	.LBB40_91
# BB#23:                                # %if.end123
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	andq	%rcx, %r13
	cmpq	%rax, %r13
	jne	.LBB40_91
# BB#24:                                # %land.rhs
	andq	%rcx, %rdx
	andq	%rcx, %r12
	cmpq	%rdx, %r12
	jne	.LBB40_25
# BB#26:                                # %land.rhs
	movabsq	$1, %r14
	jmp	.LBB40_91
.LBB40_55:
	movabsq	$0, %r14
	jmp	.LBB40_91
.LBB40_25:
	movabsq	$0, %r14
	jmp	.LBB40_91
.LBB40_70:                              # %if.end293
	movzbl	rtx_length(%rdx), %esi
	testq	%rsi, %rsi
	je	.LBB40_91
# BB#71:                                # %for.body.lr.ph
	movq	rtx_format(,%rdx,8), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
.LBB40_72:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB40_78 Depth 2
	movq	%rsi, %rbx
	movq	16(%rsp), %rax          # 8-byte Reload
	movsbq	-1(%rax,%rbx), %rax
	leaq	-1(%rbx), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	cmpq	$68, %rax
	jg	.LBB40_75
# BB#73:                                # %for.body
                                        #   in Loop: Header=BB40_72 Depth=1
	andq	%r13, %rax
	cmpq	$48, %rax
	je	.LBB40_74
	jmp	.LBB40_93
.LBB40_75:                              # %for.body
                                        #   in Loop: Header=BB40_72 Depth=1
	cmpq	$100, %rax
	jg	.LBB40_80
# BB#76:                                # %for.body
                                        #   in Loop: Header=BB40_72 Depth=1
	andq	%r13, %rax
	cmpq	$69, %rax
	jne	.LBB40_93
# BB#77:                                # %sw.bb380
                                        #   in Loop: Header=BB40_72 Depth=1
	movq	(%r12,%rbx,8), %rax
	movq	(%r15,%rbx,8), %rdx
	movl	(%rax), %eax
	movq	$32, %rcx
	movq	%rax, %r13
	movl	(%rdx), %edx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	cmpq	%rdx, %rax
	jne	.LBB40_90
.LBB40_78:                              # %for.cond399
                                        #   Parent Loop BB40_72 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB40_74
# BB#79:                                # %for.body402
                                        #   in Loop: Header=BB40_78 Depth=2
	movq	(%r12,%rbx,8), %rax
	movq	(%r15,%rbx,8), %rcx
	movq	(%rax,%r13,8), %rdi
	movq	(%rcx,%r13,8), %rsi
	leaq	-1(%r13), %r13
	callq	rtx_renumbered_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB40_78
	jmp	.LBB40_90
.LBB40_80:                              # %for.body
                                        #   in Loop: Header=BB40_72 Depth=1
	cmpq	$114, %rax
	jg	.LBB40_85
# BB#81:                                # %for.body
                                        #   in Loop: Header=BB40_72 Depth=1
	andq	%r13, %rax
	cmpq	$101, %rax
	jne	.LBB40_82
# BB#87:                                # %sw.bb354
                                        #   in Loop: Header=BB40_72 Depth=1
	movq	(%r12,%rbx,8), %rdi
	movq	(%r15,%rbx,8), %rsi
	callq	rtx_renumbered_equal_p
	testq	%r13, %rax
	jne	.LBB40_74
	jmp	.LBB40_90
.LBB40_85:                              # %for.body
                                        #   in Loop: Header=BB40_72 Depth=1
	addq	$-115, %rax
	andq	%r13, %rax
	cmpq	$4, %rax
	ja	.LBB40_93
# BB#86:                                # %for.body
                                        #   in Loop: Header=BB40_72 Depth=1
	jmpq	*.LJTI40_0(,%rax,8)
.LBB40_88:                              # %sw.bb304
                                        #   in Loop: Header=BB40_72 Depth=1
	movq	(%r12,%rbx,8), %rax
	movq	(%r15,%rbx,8), %rcx
	jmp	.LBB40_84
.LBB40_82:                              # %for.body
                                        #   in Loop: Header=BB40_72 Depth=1
	cmpq	$105, %rax
	jne	.LBB40_93
# BB#83:                                # %sw.bb317
                                        #   in Loop: Header=BB40_72 Depth=1
	movl	(%r12,%rbx,8), %eax
	movl	(%r15,%rbx,8), %ecx
.LBB40_84:                              # %sw.bb317
                                        #   in Loop: Header=BB40_72 Depth=1
	cmpq	%rcx, %rax
	jne	.LBB40_90
.LBB40_74:                              # %for.cond.backedge
                                        #   in Loop: Header=BB40_72 Depth=1
	movq	$32, %rcx
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jg	.LBB40_72
	jmp	.LBB40_91
.LBB40_89:                              # %sw.bb341
                                        #   in Loop: Header=BB40_72 Depth=1
	movq	(%r12,%rbx,8), %rdi
	movq	(%r15,%rbx,8), %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB40_74
.LBB40_90:
	xorq	%r14, %r14
.LBB40_91:                              # %return
	movq	%r14, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB40_67:
	movabsq	$0, %r14
	jmp	.LBB40_91
.LBB40_93:                              # %sw.default420
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.rtx_renumbered_equal_p, %rdx
	movq	$2400, %rsi             # imm = 0x960
	callq	fancy_abort
.Ltmp310:
	.size	rtx_renumbered_equal_p, .Ltmp310-rtx_renumbered_equal_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI40_0:
	.quad	.LBB40_89
	.quad	.LBB40_88
	.quad	.LBB40_88
	.quad	.LBB40_93
	.quad	.LBB40_88

	.text
	.globl	true_regnum
	.align	16, 0x90
	.type	true_regnum,@function
true_regnum:                            # @true_regnum
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp314:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp315:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp316:
	.cfi_def_cfa_offset 32
.Ltmp317:
	.cfi_offset %rbx, -24
.Ltmp318:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movzbl	(%rbx), %eax
	cmpq	$63, %rax
	jne	.LBB41_1
# BB#7:                                 # %if.then23
	movq	8(%rbx), %rdi
	callq	true_regnum
	movq	%rax, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r14, %rcx
	andq	%rax, %rcx
	cmpq	$52, %rcx
	ja	.LBB41_9
# BB#8:                                 # %if.then31
	movq	8(%rbx), %rax
	movzbl	2(%rbx), %ecx
	movl	16(%rbx), %edx
	movzbl	2(%rax), %esi
	movl	8(%rax), %edi
	callq	subreg_regno_offset
	addq	%r14, %rax
	jmp	.LBB41_9
.LBB41_1:                               # %entry
	cmpq	$61, %rax
	jne	.LBB41_2
# BB#3:                                 # %if.then
	movl	8(%rbx), %eax
	cmpq	$53, %rax
	jb	.LBB41_9
# BB#4:                                 # %land.lhs.true
	movq	reg_renumber(%rip), %rcx
	movswq	(%rcx,%rax,2), %rcx
	testq	%rcx, %rcx
	jns	.LBB41_6
# BB#5:                                 # %land.lhs.true
	movq	%rax, %rcx
.LBB41_6:                               # %land.lhs.true
	movq	%rcx, %rax
	jmp	.LBB41_9
.LBB41_2:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
.LBB41_9:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp319:
	.size	true_regnum, .Ltmp319-true_regnum
	.cfi_endproc

	.align	16, 0x90
	.type	delete_prior_computation,@function
delete_prior_computation:               # @delete_prior_computation
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp326:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp327:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp328:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp329:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp330:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp331:
	.cfi_def_cfa_offset 64
.Ltmp332:
	.cfi_offset %rbx, -48
.Ltmp333:
	.cfi_offset %r12, -40
.Ltmp334:
	.cfi_offset %r13, -32
.Ltmp335:
	.cfi_offset %r14, -24
.Ltmp336:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	8(%r15), %r14
	movq	%rsi, %rdi
	callq	prev_nonnote_insn
	movq	%rax, %r13
	testq	%r13, %r13
	je	.LBB42_81
# BB#1:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
.LBB42_2:                               # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r13), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movq	%rcx, %rdx
	orq	$2, %rdx
	cmpq	$34, %rdx
	jne	.LBB42_81
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB42_2 Depth=1
	movq	32(%r13), %rbx
	cmpq	$34, %rcx
	jne	.LBB42_4
# BB#5:                                 # %land.lhs.true
                                        #   in Loop: Header=BB42_2 Depth=1
	testq	$67108864, %rax         # imm = 0x4000000
	je	.LBB42_81
# BB#6:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB42_2 Depth=1
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$47, %rcx
	jne	.LBB42_81
# BB#7:                                 # %lor.lhs.false16
                                        #   in Loop: Header=BB42_2 Depth=1
	movq	16(%rbx), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$50, %rcx
	je	.LBB42_8
	jmp	.LBB42_81
	.align	16, 0x90
.LBB42_4:                               # %for.body.if.end_crit_edge
                                        #   in Loop: Header=BB42_2 Depth=1
	movl	(%rbx), %eax
.LBB42_8:                               # %if.end
                                        #   in Loop: Header=BB42_2 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$24, %rax
	je	.LBB42_81
# BB#9:                                 # %if.end
                                        #   in Loop: Header=BB42_2 Depth=1
	cmpq	$48, %rax
	jne	.LBB42_11
# BB#10:                                # %land.lhs.true31
                                        #   in Loop: Header=BB42_2 Depth=1
	movq	8(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$32, %rax
	je	.LBB42_81
.LBB42_11:                              # %if.end39
                                        #   in Loop: Header=BB42_2 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	reg_set_p
	testq	%r12, %rax
	jne	.LBB42_12
# BB#78:                                # %if.end313
                                        #   in Loop: Header=BB42_2 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	reg_overlap_mentioned_p
	testq	%r12, %rax
	jne	.LBB42_79
# BB#80:                                # %for.inc327
                                        #   in Loop: Header=BB42_2 Depth=1
	movq	%r13, %rdi
	callq	prev_nonnote_insn
	movq	%rax, %r13
	testq	%r13, %r13
	jne	.LBB42_2
	jmp	.LBB42_81
.LBB42_12:                              # %if.then42
	movq	%rbx, %rdi
	callq	side_effects_p
	testq	%r12, %rax
	je	.LBB42_14
# BB#13:                                # %land.lhs.true45
	movzbl	(%r13), %eax
	cmpq	$34, %rax
	jne	.LBB42_81
.LBB42_14:                              # %if.end50
	movzbl	(%rbx), %eax
	cmpq	$47, %rax
	jne	.LBB42_15
# BB#28:                                # %land.lhs.true84
	movq	8(%rbx), %rcx
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB42_81
# BB#29:                                # %if.then91
	movslq	8(%rcx), %r15
	cmpq	$52, %r15
	movq	$1, %rsi
	movq	$1, %r8
	jg	.LBB42_44
# BB#30:                                # %cond.true
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%r15, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB42_34
# BB#31:                                # %cond.true
	leaq	-21(%r15), %rcx
	andq	%r12, %rcx
	cmpq	$8, %rcx
	jb	.LBB42_34
# BB#32:                                # %cond.true
	leaq	-45(%r15), %rcx
	andq	%r12, %rcx
	cmpq	$8, %rcx
	jb	.LBB42_34
# BB#33:                                # %cond.true
	leaq	-29(%r15), %rcx
	andq	%r12, %rcx
	cmpq	$7, %rcx
	jbe	.LBB42_34
# BB#39:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB42_40
# BB#42:                                # %cond.true152
	movzbl	target_flags+3(%rip), %r8d
	andq	$2, %r8
	xorq	$6, %r8
	jmp	.LBB42_44
.LBB42_79:                              # %if.then316
	movq	56(%r13), %rax
	movq	%rax, 16(%r15)
	movq	%r15, 56(%r13)
.LBB42_81:                              # %for.end329
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB42_15:                              # %if.end50
	cmpq	$39, %rax
	jne	.LBB42_81
# BB#16:                                # %for.cond55.preheader
	movq	8(%rbx), %rdx
	movslq	(%rdx), %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	jle	.LBB42_22
# BB#17:
	movq	$1, %rsi
.LBB42_18:                              # %for.body59
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdx,%rsi,8), %rcx
	movzbl	(%rcx), %edi
	cmpq	$47, %rdi
	jne	.LBB42_20
# BB#19:                                # %land.lhs.true67
                                        #   in Loop: Header=BB42_18 Depth=1
	movq	8(%rcx), %rcx
	cmpq	%r14, %rcx
	jne	.LBB42_21
.LBB42_20:                              # %for.inc
                                        #   in Loop: Header=BB42_18 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%rsi, %rcx
	incq	%rsi
	cmpq	%rax, %rdi
	jl	.LBB42_18
	jmp	.LBB42_22
.LBB42_34:                              # %cond.true113
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %r8
	je	.LBB42_44
# BB#35:                                # %lor.rhs123
	cmpq	$6, %rax
	jne	.LBB42_36
# BB#37:                                # %lor.rhs123
	movabsq	$1, %r8
	incq	%r8
	jmp	.LBB42_44
.LBB42_21:                              # %land.lhs.true67.for.end_crit_edge
	decq	%rsi
	movq	%rsi, %rcx
.LBB42_22:                              # %for.end
	andq	%r12, %rax
	andq	%r12, %rcx
	cmpq	%rax, %rcx
	jne	.LBB42_81
# BB#23:                                # %if.then79
	movq	56(%r13), %rbx
	jmp	.LBB42_24
.LBB42_27:                              # %if.end.i476
                                        #   in Loop: Header=BB42_24 Depth=1
	movq	%r13, %rsi
	callq	delete_prior_computation
.LBB42_24:                              # %if.then79
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB42_64
# BB#25:                                # %for.body.i469
                                        #   in Loop: Header=BB42_24 Depth=1
	movq	%rbx, %rdi
	movl	(%rdi), %eax
	movq	16(%rdi), %rbx
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$65536, %rax            # imm = 0x10000
	jne	.LBB42_24
# BB#26:                                # %lor.lhs.false.i475
                                        #   in Loop: Header=BB42_24 Depth=1
	movq	8(%rdi), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB42_24
	jmp	.LBB42_27
.LBB42_40:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB42_43
# BB#41:                                # %cond.true141
	movl	target_flags(%rip), %r8d
	movq	$25, %rcx
	shrq	%cl, %r8
	andq	$1, %r8
	xorq	$3, %r8
	jmp	.LBB42_44
.LBB42_43:                              # %cond.false156
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	movq	%rax, %r8
	jmp	.LBB42_44
.LBB42_36:
	movabsq	$0, %r8
	incq	%r8
.LBB42_44:                              # %cond.end177
	movslq	8(%r14), %rdi
	cmpq	$52, %rdi
	jg	.LBB42_59
# BB#45:                                # %cond.true185
	movzbl	2(%r14), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rdi, %rcx
	cmpq	$8, %rcx
	je	.LBB42_49
# BB#46:                                # %cond.true185
	leaq	-21(%rdi), %rcx
	andq	%r12, %rcx
	cmpq	$8, %rcx
	jb	.LBB42_49
# BB#47:                                # %cond.true185
	leaq	-45(%rdi), %rcx
	andq	%r12, %rcx
	cmpq	$8, %rcx
	jb	.LBB42_49
# BB#48:                                # %cond.true185
	leaq	-29(%rdi), %rcx
	andq	%r12, %rcx
	cmpq	$7, %rcx
	jbe	.LBB42_49
# BB#54:                                # %cond.false227
	cmpq	$24, %rax
	jne	.LBB42_55
# BB#57:                                # %cond.true243
	movzbl	target_flags+3(%rip), %esi
	andq	$2, %rsi
	xorq	$6, %rsi
	jmp	.LBB42_59
.LBB42_49:                              # %cond.true209
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %rsi
	je	.LBB42_59
# BB#50:                                # %lor.rhs217
	cmpq	$6, %rax
	jne	.LBB42_51
# BB#52:                                # %lor.rhs217
	movabsq	$1, %rsi
	incq	%rsi
	jmp	.LBB42_59
.LBB42_55:                              # %cond.false227
	cmpq	$18, %rax
	jne	.LBB42_58
# BB#56:                                # %cond.true233
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	xorq	$3, %rsi
	jmp	.LBB42_59
.LBB42_51:
	movabsq	$0, %rsi
	incq	%rsi
	jmp	.LBB42_59
.LBB42_58:                              # %cond.false247
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %rsi
.LBB42_59:                              # %cond.end270
	leaq	(%r8,%r15), %rbx
	addq	%rdi, %rsi
	cmpq	%rdi, %r15
	jl	.LBB42_67
# BB#60:                                # %cond.end270
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jg	.LBB42_67
# BB#61:                                # %if.then278
	movq	56(%r13), %rbx
	jmp	.LBB42_63
.LBB42_66:                              # %if.end.i456
                                        #   in Loop: Header=BB42_63 Depth=1
	movq	%r13, %rsi
	callq	delete_prior_computation
	jmp	.LBB42_63
.LBB42_62:                              # %for.body.i449
                                        #   in Loop: Header=BB42_63 Depth=1
	movq	%rbx, %rdi
	movl	(%rdi), %eax
	movq	16(%rdi), %rbx
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$65536, %rax            # imm = 0x10000
	jne	.LBB42_63
# BB#65:                                # %lor.lhs.false.i455
                                        #   in Loop: Header=BB42_63 Depth=1
	movq	8(%rdi), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	je	.LBB42_66
.LBB42_63:                              # %for.cond.i443.backedge
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB42_62
	jmp	.LBB42_64
.LBB42_67:                              # %if.else279
	movq	%r8, 8(%rsp)            # 8-byte Spill
	cmpq	%rdi, %r15
	jg	.LBB42_81
# BB#68:                                # %if.else279
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rsi, %rax
	jl	.LBB42_81
# BB#69:                                # %if.then285
	movq	56(%r13), %rcx
	movq	$3, %rdi
	movq	$10, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%r13)
	movq	$32, %rcx
	movq	8(%rsp), %rax           # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB42_72
.LBB42_71:                              # %for.body297
                                        # =>This Inner Loop Header: Depth=1
	movq	$10, %rsi
	movq	%r13, %rdi
	movq	%r15, %rdx
	callq	find_regno_note
	testq	%rax, %rax
	je	.LBB42_72
# BB#70:                                # %for.cond294
                                        #   in Loop: Header=BB42_71 Depth=1
	incq	%r15
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB42_71
.LBB42_72:                              # %for.end304
	andq	%r12, %rbx
	andq	%r12, %r15
	cmpq	%rbx, %r15
	jne	.LBB42_81
# BB#73:                                # %if.then307
	movq	56(%r13), %rbx
	jmp	.LBB42_75
.LBB42_77:                              # %if.end.i
                                        #   in Loop: Header=BB42_75 Depth=1
	movq	%r13, %rsi
	callq	delete_prior_computation
	jmp	.LBB42_75
.LBB42_74:                              # %for.body.i
                                        #   in Loop: Header=BB42_75 Depth=1
	movq	%rbx, %rdi
	movl	(%rdi), %eax
	movq	16(%rdi), %rbx
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$65536, %rax            # imm = 0x10000
	jne	.LBB42_75
# BB#76:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB42_75 Depth=1
	movq	8(%rdi), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	je	.LBB42_77
.LBB42_75:                              # %for.cond.i.backedge
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB42_74
.LBB42_64:                              # %delete_computation.exit478
	movq	%r13, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	delete_related_insns  # TAILCALL
.Ltmp337:
	.size	delete_prior_computation, .Ltmp337-delete_prior_computation
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"jump.c"
	.size	.L.str, 7

	.type	.L__FUNCTION__.reverse_condition,@object # @__FUNCTION__.reverse_condition
.L__FUNCTION__.reverse_condition:
	.asciz	"reverse_condition"
	.size	.L__FUNCTION__.reverse_condition, 18

	.type	.L__FUNCTION__.reverse_condition_maybe_unordered,@object # @__FUNCTION__.reverse_condition_maybe_unordered
.L__FUNCTION__.reverse_condition_maybe_unordered:
	.asciz	"reverse_condition_maybe_unordered"
	.size	.L__FUNCTION__.reverse_condition_maybe_unordered, 34

	.type	.L__FUNCTION__.swap_condition,@object # @__FUNCTION__.swap_condition
.L__FUNCTION__.swap_condition:
	.asciz	"swap_condition"
	.size	.L__FUNCTION__.swap_condition, 15

	.type	.L__FUNCTION__.unsigned_condition,@object # @__FUNCTION__.unsigned_condition
.L__FUNCTION__.unsigned_condition:
	.asciz	"unsigned_condition"
	.size	.L__FUNCTION__.unsigned_condition, 19

	.type	.L__FUNCTION__.signed_condition,@object # @__FUNCTION__.signed_condition
.L__FUNCTION__.signed_condition:
	.asciz	"signed_condition"
	.size	.L__FUNCTION__.signed_condition, 17

	.type	.L__FUNCTION__.mark_jump_label,@object # @__FUNCTION__.mark_jump_label
.L__FUNCTION__.mark_jump_label:
	.asciz	"mark_jump_label"
	.size	.L__FUNCTION__.mark_jump_label, 16

	.type	.L__FUNCTION__.delete_barrier,@object # @__FUNCTION__.delete_barrier
.L__FUNCTION__.delete_barrier:
	.asciz	"delete_barrier"
	.size	.L__FUNCTION__.delete_barrier, 15

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"will never be executed"
	.size	.L.str1, 23

	.type	.L__FUNCTION__.invert_jump,@object # @__FUNCTION__.invert_jump
.L__FUNCTION__.invert_jump:
	.asciz	"invert_jump"
	.size	.L__FUNCTION__.invert_jump, 12

	.type	.L__FUNCTION__.rtx_renumbered_equal_p,@object # @__FUNCTION__.rtx_renumbered_equal_p
.L__FUNCTION__.rtx_renumbered_equal_p:
	.asciz	"rtx_renumbered_equal_p"
	.size	.L__FUNCTION__.rtx_renumbered_equal_p, 23

	.type	.L__FUNCTION__.invert_exp_1,@object # @__FUNCTION__.invert_exp_1
.L__FUNCTION__.invert_exp_1:
	.asciz	"invert_exp_1"
	.size	.L__FUNCTION__.invert_exp_1, 13

	.type	.L__FUNCTION__.duplicate_loop_exit_test,@object # @__FUNCTION__.duplicate_loop_exit_test
.L__FUNCTION__.duplicate_loop_exit_test:
	.asciz	"duplicate_loop_exit_test"
	.size	.L__FUNCTION__.duplicate_loop_exit_test, 25


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
