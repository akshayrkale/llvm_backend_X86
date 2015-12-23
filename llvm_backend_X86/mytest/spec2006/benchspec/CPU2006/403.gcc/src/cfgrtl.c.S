	.file	"cfgrtl.c.bc"
	.text
	.globl	delete_insn
	.align	16, 0x90
	.type	delete_insn,@function
delete_insn:                            # @delete_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6:
	.cfi_def_cfa_offset 32
.Ltmp7:
	.cfi_offset %rbx, -32
.Ltmp8:
	.cfi_offset %r14, -24
.Ltmp9:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	(%rbx), %eax
	movq	24(%rbx), %r14
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$36, %rcx
	jne	.LBB0_7
# BB#1:                                 # %if.then
	testq	$268435456, %rax        # imm = 0x10000000
	jne	.LBB0_5
# BB#2:                                 # %land.lhs.true.i
	cmpq	$0, 56(%rbx)
	jne	.LBB0_5
# BB#3:                                 # %land.lhs.true1.i
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	32(%rax), %rdi
	movq	%rbx, %rsi
	callq	in_expr_list_p
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	jne	.LBB0_5
# BB#4:                                 # %land.rhs.i
	movq	label_value_list(%rip), %rdi
	movq	%rbx, %rsi
	callq	in_expr_list_p
	testq	%r15, %rax
	je	.LBB0_6
.LBB0_5:                                # %if.then1
	movq	56(%rbx), %rax
	movq	$37, %rcx
	movw	%cx, (%rbx)
	movabsq	$4294967208, %rcx       # imm = 0xFFFFFFA8
	movl	%ecx, 40(%rbx)
	movq	%rax, 32(%rbx)
	movq	cfun(%rip), %rsi
	addq	$176, %rsi
	movq	%rbx, %rdi
	callq	remove_node_from_expr_list
	movl	(%rbx), %eax
	jmp	.LBB0_9
.LBB0_6:                                # %if.then13.critedge
	movq	cfun(%rip), %rsi
	addq	$176, %rsi
	movq	%rbx, %rdi
	callq	remove_node_from_expr_list
	movl	(%rbx), %eax
.LBB0_7:                                # %if.then13
	testq	$134217728, %rax        # imm = 0x8000000
	jne	.LBB0_26
# BB#8:                                 # %if.end18
	movq	%rbx, %rdi
	callq	remove_insn
	movl	(%rbx), %eax
	orq	$134217728, %rax        # imm = 0x8000000
	movl	%eax, (%rbx)
.LBB0_9:                                # %if.end22
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$33, %rax
	jne	.LBB0_12
# BB#10:                                # %land.lhs.true
	movq	64(%rbx), %rax
	testq	%rax, %rax
	je	.LBB0_12
# BB#11:                                # %land.lhs.true30
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	je	.LBB0_14
.LBB0_12:                               # %if.else
	movq	$13, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB0_15
# BB#13:                                # %land.lhs.true46
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB0_15
.LBB0_14:                               # %if.then53
	movl	32(%rax), %ecx
	decq	%rcx
	movl	%ecx, 32(%rax)
.LBB0_15:                               # %if.end62
	movzbl	(%rbx), %eax
	cmpq	$33, %rax
	jne	.LBB0_25
# BB#16:                                # %land.lhs.true66
	movq	32(%rbx), %rax
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65534, %rdx            # imm = 0xFFFE
	cmpq	$44, %rdx
	jne	.LBB0_25
# BB#17:                                # %if.then79
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$45, %rcx
	jne	.LBB0_18
# BB#19:                                # %if.then79
	movabsq	$1, %rcx
	jmp	.LBB0_20
.LBB0_18:
	movabsq	$0, %rcx
.LBB0_20:                               # %if.then79
	movq	8(%rax,%rcx,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB0_25
# BB#21:
	movq	$1, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	.align	16, 0x90
.LBB0_22:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rdx,8), %rdi
	movq	8(%rdi), %rdi
	movzbl	(%rdi), %ebx
	cmpq	$37, %rbx
	je	.LBB0_24
# BB#23:                                # %if.then106
                                        #   in Loop: Header=BB0_22 Depth=1
	movl	32(%rdi), %ebx
	decq	%rbx
	movl	%ebx, 32(%rdi)
.LBB0_24:                               # %for.inc
                                        #   in Loop: Header=BB0_22 Depth=1
	movq	%rdx, %rdi
	andq	%rsi, %rdi
	incq	%rdx
	cmpq	%rcx, %rdi
	jne	.LBB0_22
.LBB0_25:                               # %if.end112
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB0_26:                               # %if.then17
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.delete_insn, %rcx
	movq	$139, %rsi
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp10:
	.size	delete_insn, .Ltmp10-delete_insn
	.cfi_endproc

	.globl	delete_insn_chain
	.align	16, 0x90
	.type	delete_insn_chain,@function
delete_insn_chain:                      # @delete_insn_chain
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp16:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp17:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp18:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp19:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp20:
	.cfi_def_cfa_offset 48
.Ltmp21:
	.cfi_offset %rbx, -40
.Ltmp22:
	.cfi_offset %r12, -32
.Ltmp23:
	.cfi_offset %r14, -24
.Ltmp24:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movabsq	$4294967197, %r15       # imm = 0xFFFFFF9D
	movabsq	$4294967216, %r12       # imm = 0xFFFFFFB0
	jmp	.LBB1_1
.LBB1_4:                                #   in Loop: Header=BB1_1 Depth=1
	movq	24(%rbx), %rax
	jmp	.LBB1_6
	.align	16, 0x90
.LBB1_1:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB1_5
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB1_1 Depth=1
	movl	40(%rbx), %eax
	cmpq	%r15, %rax
	je	.LBB1_5
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB1_1 Depth=1
	cmpq	%r12, %rax
	jne	.LBB1_4
	.align	16, 0x90
.LBB1_5:                                # %if.else
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%rbx, %rdi
	callq	delete_insn
.LBB1_6:                                # %if.end
                                        #   in Loop: Header=BB1_1 Depth=1
	cmpq	%r14, %rbx
	movq	%rax, %rbx
	jne	.LBB1_1
# BB#7:                                 # %while.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp25:
	.size	delete_insn_chain, .Ltmp25-delete_insn_chain
	.cfi_endproc

	.globl	create_basic_block_structure
	.align	16, 0x90
	.type	create_basic_block_structure,@function
create_basic_block_structure:           # @create_basic_block_structure
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp32:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp33:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp34:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp35:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp36:
	.cfi_def_cfa_offset 48
.Ltmp37:
	.cfi_offset %rbx, -48
.Ltmp38:
	.cfi_offset %r12, -40
.Ltmp39:
	.cfi_offset %r13, -32
.Ltmp40:
	.cfi_offset %r14, -24
.Ltmp41:
	.cfi_offset %r15, -16
	movq	%rcx, %r13
	movq	%rdx, %rbx
	movq	%rsi, %r15
	movq	%rdi, %r14
	testq	%r13, %r13
	je	.LBB2_10
# BB#1:                                 # %land.lhs.true
	movzbl	3(%r13), %eax
	testq	$64, %rax
	jne	.LBB2_10
# BB#2:                                 # %land.lhs.true2
	movq	32(%r13), %r12
	testq	%r12, %r12
	je	.LBB2_10
# BB#3:                                 # %land.lhs.true4
	cmpq	$0, 80(%r12)
	je	.LBB2_4
.LBB2_10:                               # %if.else20
	callq	alloc_block
	movq	%rax, %r12
	movq	%r15, %rax
	orq	%rbx, %rax
	je	.LBB2_11
# BB#12:                                # %if.else27
	movzbl	(%r15), %eax
	cmpq	$36, %rax
	jne	.LBB2_17
# BB#13:                                # %if.else27
	testq	%rbx, %rbx
	je	.LBB2_17
# BB#14:                                # %if.then33
	movabsq	$4294967216, %rdi       # imm = 0xFFFFFFB0
	movq	%r15, %rsi
	callq	emit_note_after
	movq	%rax, %r13
	cmpq	%rbx, %r15
                                        # kill: RAX<def> R13<kill>
	je	.LBB2_16
# BB#15:                                # %if.then33
	movq	%rbx, %rax
.LBB2_16:                               # %if.then33
	movq	%rax, %rbx
	jmp	.LBB2_20
.LBB2_11:                               # %if.then24
	callq	get_last_insn
	movabsq	$4294967216, %rdi       # imm = 0xFFFFFFB0
	movq	%rax, %rsi
	callq	emit_note_after
	movq	%rax, %r15
	jmp	.LBB2_18
.LBB2_17:                               # %if.else38
	movabsq	$4294967216, %rdi       # imm = 0xFFFFFFB0
	movq	%r15, %rsi
	callq	emit_note_before
	movq	%rax, %r15
	testq	%rbx, %rbx
	jne	.LBB2_19
.LBB2_18:                               # %if.else38
	movq	%r15, %rbx
.LBB2_19:                               # %if.else38
	movq	%r15, %r13
.LBB2_20:                               # %if.end44
	movq	%r12, 32(%r13)
.LBB2_21:                               # %if.end48
	movq	24(%rbx), %rax
	cmpq	%r13, %rax
	je	.LBB2_23
# BB#22:                                # %select.mid
	movq	%rbx, %r13
.LBB2_23:                               # %select.end
	movq	%r15, (%r12)
	movq	%r13, 8(%r12)
	movl	%r14d, 88(%r12)
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rax
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	%r12, 32(%rax,%r14,8)
	movq	basic_block_for_insn(%rip), %rax
	testq	%rax, %rax
	je	.LBB2_29
# BB#24:
	movq	%r12, %rbx
	jmp	.LBB2_25
	.align	16, 0x90
.LBB2_28:                               # %for.inc.i
                                        #   in Loop: Header=BB2_25 Depth=1
	movq	basic_block_for_insn(%rip), %rax
	addq	$24, %rbx
.LBB2_25:                               # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rbx
	movslq	8(%rbx), %r14
	movq	(%rax), %rcx
	cmpq	%rcx, %r14
	jb	.LBB2_27
# BB#26:                                # %if.then.i.i
                                        #   in Loop: Header=BB2_25 Depth=1
	leaq	7(%r14), %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	addq	%r14, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, basic_block_for_insn(%rip)
.LBB2_27:                               # %set_block_for_insn.exit.i
                                        #   in Loop: Header=BB2_25 Depth=1
	movq	%r12, 32(%rax,%r14,8)
	movq	8(%r12), %rax
	cmpq	%rax, %rbx
	jne	.LBB2_28
.LBB2_29:                               # %if.end62
	movq	%r12, 80(%r12)
	movq	%r12, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_4:                                # %if.then
	movzbl	(%r15), %eax
	cmpq	$36, %rax
	jne	.LBB2_6
# BB#5:
	movq	%r15, %rdx
	jmp	.LBB2_7
.LBB2_6:                                # %if.else
	movq	16(%r15), %rdx
	movq	%r13, %r15
.LBB2_7:                                # %if.end
	cmpq	%r13, %rdx
	je	.LBB2_21
# BB#8:                                 # %land.lhs.true13
	movq	24(%rdx), %rax
	cmpq	%r13, %rax
	je	.LBB2_21
# BB#9:                                 # %if.then18
	movq	%r13, %rdi
	movq	%r13, %rsi
	callq	reorder_insns
	jmp	.LBB2_21
.Ltmp42:
	.size	create_basic_block_structure, .Ltmp42-create_basic_block_structure
	.cfi_endproc

	.globl	update_bb_for_insn
	.align	16, 0x90
	.type	update_bb_for_insn,@function
update_bb_for_insn:                     # @update_bb_for_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp47:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp48:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp49:
	.cfi_def_cfa_offset 32
.Ltmp50:
	.cfi_offset %rbx, -32
.Ltmp51:
	.cfi_offset %r14, -24
.Ltmp52:
	.cfi_offset %r15, -16
	movq	basic_block_for_insn(%rip), %rax
	movq	%rdi, %r14
	testq	%rax, %rax
	je	.LBB3_6
# BB#1:                                 # %if.end
	movq	%r14, %rbx
	jmp	.LBB3_2
	.align	16, 0x90
.LBB3_5:                                # %for.inc
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	basic_block_for_insn(%rip), %rax
	addq	$24, %rbx
.LBB3_2:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rbx
	movslq	8(%rbx), %r15
	movq	(%rax), %rcx
	cmpq	%rcx, %r15
	jb	.LBB3_4
# BB#3:                                 # %if.then.i
                                        #   in Loop: Header=BB3_2 Depth=1
	leaq	7(%r15), %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	addq	%r15, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, basic_block_for_insn(%rip)
.LBB3_4:                                # %set_block_for_insn.exit
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	%r14, 32(%rax,%r15,8)
	movq	8(%r14), %rax
	cmpq	%rax, %rbx
	jne	.LBB3_5
.LBB3_6:                                # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp53:
	.size	update_bb_for_insn, .Ltmp53-update_bb_for_insn
	.cfi_endproc

	.globl	create_basic_block
	.align	16, 0x90
	.type	create_basic_block,@function
create_basic_block:                     # @create_basic_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp59:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp60:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp61:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp62:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp63:
	.cfi_def_cfa_offset 48
.Ltmp64:
	.cfi_offset %rbx, -40
.Ltmp65:
	.cfi_offset %r12, -32
.Ltmp66:
	.cfi_offset %r14, -24
.Ltmp67:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movq	$32, %rcx
	movq	%rbx, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movl	n_basic_blocks(%rip), %esi
	movq	basic_block_info(%rip), %rdi
	incq	%rsi
	movl	%esi, n_basic_blocks(%rip)
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	callq	varray_grow
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, basic_block_info(%rip)
	leaq	-1(%rdi), %rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%r12, %rsi
	jle	.LBB4_4
# BB#1:                                 # %for.body.lr.ph
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	jmp	.LBB4_2
	.align	16, 0x90
.LBB4_3:                                # %for.body.for.body_crit_edge
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	basic_block_info(%rip), %rax
	movq	%rsi, %rdi
	decq	%rsi
.LBB4_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	addq	$-2, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	32(%rax,%rdi,8), %rcx
	movq	%rcx, 32(%rax,%rsi,8)
	movl	%esi, 88(%rcx)
	decq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rax
	jg	.LBB4_3
.LBB4_4:                                # %for.end
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	create_basic_block_structure
	movq	$0, 80(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp68:
	.size	create_basic_block, .Ltmp68-create_basic_block
	.cfi_endproc

	.globl	flow_delete_block_noexpunge
	.align	16, 0x90
	.type	flow_delete_block_noexpunge,@function
flow_delete_block_noexpunge:            # @flow_delete_block_noexpunge
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp75:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp76:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp77:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp78:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp79:
	.cfi_def_cfa_offset 48
.Ltmp80:
	.cfi_offset %rbx, -48
.Ltmp81:
	.cfi_offset %r12, -40
.Ltmp82:
	.cfi_offset %r13, -32
.Ltmp83:
	.cfi_offset %r14, -24
.Ltmp84:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	(%r14), %rbx
	movq	8(%r14), %rsi
	movq	%rbx, %rdi
	callq	never_reached_warning
	movzbl	(%rbx), %eax
	cmpq	$36, %rax
	jne	.LBB5_2
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	callq	maybe_remove_eh_handler
.LBB5_2:                                # %if.end
	movq	8(%r14), %r15
	movzbl	(%r15), %eax
	cmpq	$33, %rax
	jne	.LBB5_9
# BB#3:                                 # %land.lhs.true
	movq	64(%r15), %rax
	testq	%rax, %rax
	je	.LBB5_9
# BB#4:                                 # %land.lhs.true7
	movq	24(%rax), %rax
	testq	%rax, %rax
	je	.LBB5_9
# BB#5:                                 # %land.lhs.true12
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB5_9
# BB#6:                                 # %land.lhs.true16
	movq	32(%rax), %rcx
	movl	(%rcx), %ecx
	andq	$65534, %rcx            # imm = 0xFFFE
	cmpq	$44, %rcx
	je	.LBB5_8
# BB#7:                                 # %land.lhs.true16
	movq	%r15, %rax
.LBB5_8:                                # %land.lhs.true16
	movq	%rax, %r15
.LBB5_9:                                # %if.end30
	movq	%r15, %rdi
	callq	next_nonnote_insn
	testq	%rax, %rax
	je	.LBB5_13
# BB#10:                                # %land.lhs.true31
	movzbl	(%rax), %ecx
	cmpq	$35, %rcx
	je	.LBB5_12
# BB#11:                                # %land.lhs.true31
	movq	%r15, %rax
.LBB5_12:                               # %land.lhs.true31
	movq	%rax, %r15
.LBB5_13:                               # %if.end36
	movq	$0, (%r14)
	movabsq	$4294967197, %r12       # imm = 0xFFFFFF9D
	movabsq	$4294967216, %r13       # imm = 0xFFFFFFB0
	jmp	.LBB5_14
.LBB5_17:                               #   in Loop: Header=BB5_14 Depth=1
	movq	24(%rbx), %rax
	jmp	.LBB5_19
	.align	16, 0x90
.LBB5_14:                               # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB5_18
# BB#15:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB5_14 Depth=1
	movl	40(%rbx), %eax
	cmpq	%r12, %rax
	je	.LBB5_18
# BB#16:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB5_14 Depth=1
	cmpq	%r13, %rax
	jne	.LBB5_17
	.align	16, 0x90
.LBB5_18:                               # %if.else.i
                                        #   in Loop: Header=BB5_14 Depth=1
	movq	%rbx, %rdi
	callq	delete_insn
.LBB5_19:                               # %if.end.i
                                        #   in Loop: Header=BB5_14 Depth=1
	cmpq	%r15, %rbx
	movq	%rax, %rbx
	jne	.LBB5_14
# BB#20:                                # %while.cond.preheader
	movq	32(%r14), %rdi
	leaq	32(%r14), %rbx
	jmp	.LBB5_21
	.align	16, 0x90
.LBB5_22:                               # %while.body
                                        #   in Loop: Header=BB5_21 Depth=1
	callq	remove_edge
	movq	(%rbx), %rdi
.LBB5_21:                               # %while.cond.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%rdi, %rdi
	jne	.LBB5_22
	jmp	.LBB5_24
	.align	16, 0x90
.LBB5_23:                               # %while.body42
                                        #   in Loop: Header=BB5_24 Depth=1
	callq	remove_edge
.LBB5_24:                               # %while.body42
                                        # =>This Inner Loop Header: Depth=1
	movq	40(%r14), %rdi
	testq	%rdi, %rdi
	jne	.LBB5_23
# BB#25:                                # %while.end44
	movq	$0, 8(%rbx)
	xorq	%rax, %rax
	movq	$0, (%rbx)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp85:
	.size	flow_delete_block_noexpunge, .Ltmp85-flow_delete_block_noexpunge
	.cfi_endproc

	.globl	flow_delete_block
	.align	16, 0x90
	.type	flow_delete_block,@function
flow_delete_block:                      # @flow_delete_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp88:
	.cfi_def_cfa_offset 16
.Ltmp89:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	flow_delete_block_noexpunge
	movq	%rbx, %rdi
	callq	expunge_block
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp90:
	.size	flow_delete_block, .Ltmp90-flow_delete_block
	.cfi_endproc

	.globl	compute_bb_for_insn
	.align	16, 0x90
	.type	compute_bb_for_insn,@function
compute_bb_for_insn:                    # @compute_bb_for_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp94:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp95:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp96:
	.cfi_def_cfa_offset 32
.Ltmp97:
	.cfi_offset %rbx, -24
.Ltmp98:
	.cfi_offset %r14, -16
	movq	basic_block_for_insn(%rip), %rax
	movq	%rdi, %r14
	testq	%rax, %rax
	je	.LBB7_2
# BB#1:                                 # %if.then2
	movq	%rax, %rdi
	callq	free
	movq	$0, basic_block_for_insn(%rip)
.LBB7_2:                                # %if.end3
	movq	$32, %rcx
	movq	%r14, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movabsq	$.L.str1, %rdx
	movq	$8, %rsi
	callq	varray_init
	movslq	n_basic_blocks(%rip), %r8
	movq	%rax, basic_block_for_insn(%rip)
	testq	%r8, %r8
	jle	.LBB7_10
# BB#3:
	xorq	%r9, %r9
	.align	16, 0x90
.LBB7_4:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_5 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r9,8), %rdx
	movq	8(%rdx), %rdi
	movq	%rdx, %rsi
	jmp	.LBB7_5
	.align	16, 0x90
.LBB7_8:                                # %for.inc
                                        #   in Loop: Header=BB7_5 Depth=2
	addq	$24, %rsi
.LBB7_5:                                # %for.cond7
                                        #   Parent Loop BB7_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rsi), %rsi
	movslq	8(%rsi), %rax
	movq	$32, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rax
	jge	.LBB7_7
# BB#6:                                 # %if.then11
                                        #   in Loop: Header=BB7_5 Depth=2
	movq	basic_block_for_insn(%rip), %rcx
	movq	%rdx, 32(%rcx,%rax,8)
.LBB7_7:                                # %if.end19
                                        #   in Loop: Header=BB7_5 Depth=2
	cmpq	%rdi, %rsi
	jne	.LBB7_8
# BB#9:                                 # %for.inc26
                                        #   in Loop: Header=BB7_4 Depth=1
	incq	%r9
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r8, %rax
	jl	.LBB7_4
.LBB7_10:                               # %for.end27
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp99:
	.size	compute_bb_for_insn, .Ltmp99-compute_bb_for_insn
	.cfi_endproc

	.globl	free_bb_for_insn
	.align	16, 0x90
	.type	free_bb_for_insn,@function
free_bb_for_insn:                       # @free_bb_for_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp101:
	.cfi_def_cfa_offset 16
	movq	basic_block_for_insn(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB8_2
# BB#1:                                 # %if.then2
	callq	free
	movq	$0, basic_block_for_insn(%rip)
.LBB8_2:                                # %if.end3
	movq	$0, basic_block_for_insn(%rip)
	popq	%rax
	retq
.Ltmp102:
	.size	free_bb_for_insn, .Ltmp102-free_bb_for_insn
	.cfi_endproc

	.globl	set_block_for_insn
	.align	16, 0x90
	.type	set_block_for_insn,@function
set_block_for_insn:                     # @set_block_for_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp106:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp107:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp108:
	.cfi_def_cfa_offset 32
.Ltmp109:
	.cfi_offset %rbx, -24
.Ltmp110:
	.cfi_offset %r14, -16
	movq	basic_block_for_insn(%rip), %rax
	movslq	8(%rdi), %rbx
	movq	(%rax), %rcx
	movq	%rsi, %r14
	cmpq	%rcx, %rbx
	jb	.LBB9_2
# BB#1:                                 # %if.then
	leaq	7(%rbx), %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	addq	%rbx, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, basic_block_for_insn(%rip)
.LBB9_2:                                # %if.end
	movq	%r14, 32(%rax,%rbx,8)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp111:
	.size	set_block_for_insn, .Ltmp111-set_block_for_insn
	.cfi_endproc

	.globl	split_block
	.align	16, 0x90
	.type	split_block,@function
split_block:                            # @split_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp118:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp119:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp120:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp121:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp122:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp123:
	.cfi_def_cfa_offset 64
.Ltmp124:
	.cfi_offset %rbx, -48
.Ltmp125:
	.cfi_offset %r12, -40
.Ltmp126:
	.cfi_offset %r13, -32
.Ltmp127:
	.cfi_offset %r14, -24
.Ltmp128:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	8(%r14), %r13
	movq	%rsi, %r12
	xorq	%r15, %r15
	cmpq	%r12, %r13
	je	.LBB10_22
# BB#1:                                 # %if.end
	movl	88(%r14), %r15d
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	24(%r12), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movl	n_basic_blocks(%rip), %esi
	movq	basic_block_info(%rip), %rdi
	incq	%rsi
	movl	%esi, n_basic_blocks(%rip)
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	callq	varray_grow
	movl	n_basic_blocks(%rip), %edx
	movq	%rax, basic_block_info(%rip)
	leaq	-1(%rdx), %rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rbx, %rdi
	jle	.LBB10_5
# BB#2:                                 # %for.body.lr.ph.i
	movq	$32, %rcx
	movq	%rsi, %r8
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	leaq	-2(%rdx), %rdi
	movq	$32, %rcx
	movq	%rdi, %r9
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	$32, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	movq	32(%rax,%r9,8), %r10
	sarq	%cl, %rbx
	cmpq	%rbx, %r9
	movq	%r10, 32(%rax,%r8,8)
	movl	%esi, 88(%r10)
	jle	.LBB10_5
# BB#3:                                 # %for.body.for.body_crit_edge.i.preheader
	decq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%rdx
	.align	16, 0x90
.LBB10_4:                               # %for.body.for.body_crit_edge.i
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	addq	$-2, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	32(%rax,%rsi,8), %rcx
	movq	%rcx, 32(%rax,%rdx,8)
	movq	%rdx, %rsi
	movl	%edx, 88(%rcx)
	decq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	decq	%rdx
	cmpq	%rbx, %rax
	jg	.LBB10_4
.LBB10_5:                               # %create_basic_block.exit
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r13, %rdx
	callq	create_basic_block_structure
	movq	%rax, %rbx
	movq	$0, 80(%rbx)
	movq	96(%r14), %rax
	movq	%rax, 96(%rbx)
	movl	104(%r14), %eax
	movl	%eax, 104(%rbx)
	movl	92(%r14), %eax
	movl	%eax, 92(%rbx)
	movq	%r12, 8(%r14)
	movq	40(%r14), %rax
	movq	%rax, 40(%rbx)
	movq	$0, 40(%r14)
	movq	40(%rbx), %rax
	jmp	.LBB10_7
	.align	16, 0x90
.LBB10_6:                               # %for.body
                                        #   in Loop: Header=BB10_7 Depth=1
	movq	%rbx, 16(%rax)
	movq	8(%rax), %rax
.LBB10_7:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB10_6
# BB#8:                                 # %for.end
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	make_single_succ_edge
	cmpq	$0, 64(%r14)
	movq	%rax, %r15
	je	.LBB10_22
# BB#9:                                 # %if.then11
	movq	flow_obstack+32(%rip), %rcx
	movq	flow_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$23, %rcx
	jg	.LBB10_11
# BB#10:                                # %if.then14
	movabsq	$flow_obstack, %rdi
	movq	$24, %rsi
	callq	_obstack_newchunk
	movq	flow_obstack+24(%rip), %rax
.LBB10_11:                              # %if.end15
	addq	$24, %rax
	movq	%rax, flow_obstack+24(%rip)
	movq	flow_obstack+16(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB10_13
# BB#12:                                # %if.then22
	movzbl	flow_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, flow_obstack+80(%rip)
.LBB10_13:                              # %if.end23
	movslq	flow_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, flow_obstack+24(%rip)
	movq	flow_obstack+32(%rip), %r8
	movq	flow_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%r8, %rax
	subq	%rdx, %rax
	cmpq	%rax, %rsi
	jle	.LBB10_15
# BB#14:                                # %if.then44
	movq	%r8, flow_obstack+24(%rip)
	movq	%r8, %rcx
.LBB10_15:                              # %if.end47
	movq	%rcx, flow_obstack+16(%rip)
	callq	bitmap_initialize
	movq	%rax, 64(%rbx)
	movq	flow_obstack+32(%rip), %rcx
	movq	flow_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$23, %rcx
	jg	.LBB10_17
# BB#16:                                # %if.then67
	movabsq	$flow_obstack, %rdi
	movq	$24, %rsi
	callq	_obstack_newchunk
	movq	flow_obstack+24(%rip), %rax
.LBB10_17:                              # %if.end68
	addq	$24, %rax
	movq	%rax, flow_obstack+24(%rip)
	movq	flow_obstack+16(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB10_19
# BB#18:                                # %if.then81
	movzbl	flow_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, flow_obstack+80(%rip)
.LBB10_19:                              # %if.end86
	movslq	flow_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, flow_obstack+24(%rip)
	movq	flow_obstack+32(%rip), %r8
	movq	flow_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%r8, %rax
	subq	%rdx, %rax
	cmpq	%rax, %rsi
	jle	.LBB10_21
# BB#20:                                # %if.then111
	movq	%r8, flow_obstack+24(%rip)
	movq	%r8, %rcx
.LBB10_21:                              # %if.end114
	movq	%rcx, flow_obstack+16(%rip)
	callq	bitmap_initialize
	movq	%rax, 72(%rbx)
	movq	72(%r14), %rsi
	movq	%rax, %rdi
	callq	bitmap_copy
	movq	64(%rbx), %rdi
	movq	72(%r14), %rsi
	callq	bitmap_copy
	movq	64(%rbx), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rdi
	callq	propagate_block
	movq	72(%r14), %rdi
	movq	64(%rbx), %rsi
	callq	bitmap_copy
.LBB10_22:                              # %return
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp129:
	.size	split_block, .Ltmp129-split_block
	.cfi_endproc

	.globl	merge_blocks_nomove
	.align	16, 0x90
	.type	merge_blocks_nomove,@function
merge_blocks_nomove:                    # @merge_blocks_nomove
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp136:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp137:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp138:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp139:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp140:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp141:
	.cfi_def_cfa_offset 64
.Ltmp142:
	.cfi_offset %rbx, -48
.Ltmp143:
	.cfi_offset %r12, -40
.Ltmp144:
	.cfi_offset %r13, -32
.Ltmp145:
	.cfi_offset %r14, -24
.Ltmp146:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	(%r15), %rax
	movl	(%rax), %edx
	movq	%rdi, %r14
	movq	8(%r15), %rdi
	movq	8(%r14), %rbx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	xorq	%rcx, %rcx
	cmpq	$36, %rsi
	jne	.LBB11_1
# BB#2:                                 # %if.then
	cmpq	%rdi, %rax
	jne	.LBB11_3
# BB#4:                                 # %if.then
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movabsq	$1, %rcx
	jmp	.LBB11_5
.LBB11_1:
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	%rax, %r13
	xorq	%rax, %rax
	jmp	.LBB11_6
.LBB11_3:
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movabsq	$0, %rcx
.LBB11_5:                               # %if.then
	movq	24(%rax), %r13
	movl	(%r13), %edx
.LBB11_6:                               # %if.end4
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$37, %rdx
	jne	.LBB11_7
# BB#8:                                 # %land.lhs.true
	movl	40(%r13), %edx
	movabsq	$4294967216, %rsi       # imm = 0xFFFFFFB0
	cmpq	%rsi, %rdx
	jne	.LBB11_7
# BB#9:                                 # %if.then11
	movq	8(%rsp), %rdx           # 8-byte Reload
	cmpq	%rdx, %r13
	movq	$1, %rsi
	je	.LBB11_11
# BB#10:                                # %if.then11
	movq	%rcx, %rsi
.LBB11_11:                              # %if.then11
	testq	%rax, %rax
	movq	%r13, %rcx
	je	.LBB11_13
# BB#12:                                # %if.then11
	movq	%rax, %rcx
.LBB11_13:                              # %if.then11
	movq	%rcx, %rax
	jmp	.LBB11_14
.LBB11_7:
	movq	%rax, %r13
	movq	%rcx, %rsi
.LBB11_14:                              # %if.end20
	movzbl	(%rbx), %ecx
	cmpq	$33, %rcx
	jne	.LBB11_20
# BB#15:                                # %if.then24
	movq	16(%rbx), %r12
	movzbl	(%r12), %eax
	cmpq	$37, %rax
	jne	.LBB11_23
# BB#16:
	movabsq	$4294967216, %rax       # imm = 0xFFFFFFB0
	movq	%r12, %rcx
	.align	16, 0x90
.LBB11_17:                              # %lor.lhs.false
                                        # =>This Inner Loop Header: Depth=1
	movl	40(%rcx), %edx
	cmpq	%rax, %rdx
	je	.LBB11_23
# BB#18:                                # %lor.lhs.false35
                                        #   in Loop: Header=BB11_17 Depth=1
	movq	(%r14), %rdx
	cmpq	%rdx, %rcx
	je	.LBB11_23
# BB#19:                                # %for.inc
                                        #   in Loop: Header=BB11_17 Depth=1
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$37, %rdx
	je	.LBB11_17
	jmp	.LBB11_23
.LBB11_20:                              # %if.else
	movq	24(%rbx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$35, %rdx
	je	.LBB11_22
# BB#21:                                # %if.else
	movq	%rax, %rcx
.LBB11_22:                              # %if.else
	movq	%rbx, %r12
	movq	%rcx, %rbx
.LBB11_23:                              # %while.cond.preheader
	movq	%rsi, (%rsp)            # 8-byte Spill
	jmp	.LBB11_25
	.align	16, 0x90
.LBB11_24:                              # %while.body
                                        #   in Loop: Header=BB11_25 Depth=1
	callq	remove_edge
.LBB11_25:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	40(%r14), %rdi
	testq	%rdi, %rdi
	jne	.LBB11_24
# BB#26:                                # %while.end
	movq	40(%r15), %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB11_30
# BB#27:
	movq	%rax, %rcx
	.align	16, 0x90
.LBB11_28:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, 16(%rcx)
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB11_28
# BB#29:
	movq	%rax, %rcx
.LBB11_30:                              # %for.end64
	movq	%rcx, 40(%r14)
	movq	$0, 40(%r15)
	movq	$0, 32(%r15)
	movq	72(%r15), %rax
	movq	%rax, 72(%r14)
	movq	%r15, %rdi
	callq	expunge_block
	movabsq	$4294967197, %r15       # imm = 0xFFFFFF9D
	jmp	.LBB11_31
.LBB11_34:                              #   in Loop: Header=BB11_31 Depth=1
	movq	24(%rbx), %rax
	jmp	.LBB11_36
	.align	16, 0x90
.LBB11_31:                              # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB11_35
# BB#32:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB11_31 Depth=1
	movl	40(%rbx), %eax
	cmpq	%r15, %rax
	je	.LBB11_35
# BB#33:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB11_31 Depth=1
	movabsq	$4294967216, %rcx       # imm = 0xFFFFFFB0
	cmpq	%rcx, %rax
	jne	.LBB11_34
	.align	16, 0x90
.LBB11_35:                              # %if.else.i
                                        #   in Loop: Header=BB11_31 Depth=1
	movq	%rbx, %rdi
	callq	delete_insn
.LBB11_36:                              # %if.end.i
                                        #   in Loop: Header=BB11_31 Depth=1
	cmpq	%r13, %rbx
	movq	%rax, %rbx
	jne	.LBB11_31
# BB#37:                                # %delete_insn_chain.exit
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	(%rsp), %rcx            # 8-byte Reload
	testq	%rax, %rcx
	movq	8(%rsp), %r15           # 8-byte Reload
	je	.LBB11_39
# BB#38:
	movq	%r12, %r15
	jmp	.LBB11_47
.LBB11_39:                              # %if.then70
	movq	basic_block_for_insn(%rip), %rax
	testq	%rax, %rax
	jne	.LBB11_40
	jmp	.LBB11_47
	.align	16, 0x90
.LBB11_43:                              # %set_block_for_insn.exit
                                        #   in Loop: Header=BB11_40 Depth=1
	movq	%r14, 32(%rax,%rbx,8)
	movq	24(%r12), %r12
	movq	basic_block_for_insn(%rip), %rax
.LBB11_40:                              # %for.cond73.preheader
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r15, %r12
	je	.LBB11_44
# BB#41:                                # %for.body75
                                        #   in Loop: Header=BB11_40 Depth=1
	movslq	8(%r12), %rbx
	movq	(%rax), %rcx
	cmpq	%rcx, %rbx
	jb	.LBB11_43
# BB#42:                                # %if.then.i
                                        #   in Loop: Header=BB11_40 Depth=1
	leaq	7(%rbx), %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	addq	%rbx, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, basic_block_for_insn(%rip)
	jmp	.LBB11_43
.LBB11_44:                              # %for.end80
	movslq	8(%r15), %rbx
	movq	(%rax), %rcx
	cmpq	%rcx, %rbx
	jb	.LBB11_46
# BB#45:                                # %if.then.i139
	leaq	7(%rbx), %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	addq	%rbx, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, basic_block_for_insn(%rip)
.LBB11_46:                              # %set_block_for_insn.exit144
	movq	%r14, 32(%rax,%rbx,8)
.LBB11_47:                              # %if.end82
	movq	%r15, 8(%r14)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp147:
	.size	merge_blocks_nomove, .Ltmp147-merge_blocks_nomove
	.cfi_endproc

	.globl	block_label
	.align	16, 0x90
	.type	block_label,@function
block_label:                            # @block_label
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp151:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp152:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp153:
	.cfi_def_cfa_offset 32
.Ltmp154:
	.cfi_offset %rbx, -24
.Ltmp155:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	xorq	%rcx, %rcx
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %rbx
	je	.LBB12_6
# BB#1:                                 # %if.end
	movq	(%rbx), %rcx
	movzbl	(%rcx), %eax
	cmpq	$36, %rax
	je	.LBB12_6
# BB#2:                                 # %if.then2
	callq	gen_label_rtx
	movq	(%rbx), %rsi
	movq	%rax, %rdi
	callq	emit_label_before
	movq	%rax, %rcx
	movq	%rcx, (%rbx)
	movq	basic_block_for_insn(%rip), %rax
	testq	%rax, %rax
	je	.LBB12_6
# BB#3:                                 # %if.then6
	movslq	8(%rcx), %r14
	movq	(%rax), %rcx
	cmpq	%rcx, %r14
	jb	.LBB12_5
# BB#4:                                 # %if.then.i
	leaq	7(%r14), %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	addq	%r14, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, basic_block_for_insn(%rip)
.LBB12_5:                               # %set_block_for_insn.exit
	movq	%rbx, 32(%rax,%r14,8)
	movq	(%rbx), %rcx
.LBB12_6:                               # %return
	movq	%rcx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp156:
	.size	block_label, .Ltmp156-block_label
	.cfi_endproc

	.globl	redirect_edge_and_branch
	.align	16, 0x90
	.type	redirect_edge_and_branch,@function
redirect_edge_and_branch:               # @redirect_edge_and_branch
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp163:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp164:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp165:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp166:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp167:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp168:
	.cfi_def_cfa_offset 80
.Ltmp169:
	.cfi_offset %rbx, -48
.Ltmp170:
	.cfi_offset %r12, -40
.Ltmp171:
	.cfi_offset %r13, -32
.Ltmp172:
	.cfi_offset %r14, -24
.Ltmp173:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movzbl	48(%rbx), %eax
	movq	%rsi, %r15
	xorq	%r13, %r13
	testq	$12, %rax
	jne	.LBB13_88
# BB#1:                                 # %if.end
	movq	16(%rbx), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	24(%rbx), %rax
	movq	(%rax), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	40(%rcx), %rax
	movq	8(%rcx), %r14
	jmp	.LBB13_2
	.align	16, 0x90
.LBB13_17:                              # %for.cond.i
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	8(%rax), %rax
.LBB13_2:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB13_3
# BB#18:                                # %for.body.i
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	24(%rax), %rcx
	cmpq	%r15, %rcx
	je	.LBB13_17
# BB#19:                                # %for.body.i
                                        #   in Loop: Header=BB13_2 Depth=1
	cmpq	%rbx, %rax
	je	.LBB13_17
	jmp	.LBB13_20
.LBB13_3:                               # %lor.lhs.false.i
	movq	%r14, %rdi
	callq	onlyjump_p
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB13_20
# BB#4:                                 # %if.end8.i
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB13_20
# BB#5:                                 # %cond.true.i
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB13_7
# BB#6:                                 # %cond.false.i
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB13_7:                               # %cond.end25.i
	testq	%rax, %rax
	je	.LBB13_20
# BB#8:                                 # %lor.lhs.false28.i
	movq	%rax, %rdi
	callq	side_effects_p
	testq	%r12, %rax
	jne	.LBB13_20
# BB#9:                                 # %if.end32.i
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	can_fallthru
	testq	%rax, %rax
	je	.LBB13_37
# BB#10:                                # %if.then34.i
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB13_12
# BB#11:                                # %if.then36.i
	movl	8(%r14), %edx
	movabsq	$.L.str42, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB13_12:                              # %if.end40.i
	movq	(%r15), %rax
	movq	16(%rax), %rbx
	movabsq	$4294967197, %r13       # imm = 0xFFFFFF9D
	movq	$1, %r12
	jmp	.LBB13_13
.LBB13_16:                              #   in Loop: Header=BB13_13 Depth=1
	movq	24(%r14), %rax
	jmp	.LBB13_36
	.align	16, 0x90
.LBB13_13:                              # %while.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %eax
	cmpq	$37, %rax
	jne	.LBB13_35
# BB#14:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB13_13 Depth=1
	movl	40(%r14), %eax
	cmpq	%r13, %rax
	je	.LBB13_35
# BB#15:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB13_13 Depth=1
	movabsq	$4294967216, %rcx       # imm = 0xFFFFFFB0
	cmpq	%rcx, %rax
	jne	.LBB13_16
	.align	16, 0x90
.LBB13_35:                              # %if.else.i.i
                                        #   in Loop: Header=BB13_13 Depth=1
	movq	%r14, %rdi
	callq	delete_insn
.LBB13_36:                              # %if.end.i.i
                                        #   in Loop: Header=BB13_13 Depth=1
	cmpq	%rbx, %r14
	movq	%rax, %r14
	jne	.LBB13_13
	jmp	.LBB13_79
.LBB13_37:                              # %if.else.i
	movq	%r14, %rdi
	callq	simplejump_p
	testq	%r12, %rax
	je	.LBB13_51
# BB#38:                                # %if.then46.i
	movq	24(%rbx), %rax
	cmpq	%r15, %rax
	je	.LBB13_20
# BB#39:                                # %if.end51.i
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB13_41
# BB#40:                                # %if.then53.i
	movl	88(%r15), %r8d
	movl	88(%rax), %ecx
	movl	8(%r14), %edx
	movabsq	$.L.str43, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB13_41:                              # %if.end60.i
	xorq	%r12, %r12
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %r15
	movabsq	$0, %rsi
	je	.LBB13_47
# BB#42:                                # %if.end.i247.i
	movq	(%r15), %rsi
	movzbl	(%rsi), %eax
	cmpq	$36, %rax
	je	.LBB13_47
# BB#43:                                # %if.then2.i.i
	callq	gen_label_rtx
	movq	(%r15), %rsi
	movq	%rax, %rdi
	callq	emit_label_before
	movq	%rax, %rsi
	movq	%rsi, (%r15)
	movq	basic_block_for_insn(%rip), %rax
	testq	%rax, %rax
	je	.LBB13_47
# BB#44:                                # %if.then6.i.i
	movslq	8(%rsi), %rdx
	movq	(%rax), %rcx
	cmpq	%rcx, %rdx
	jb	.LBB13_46
# BB#45:                                # %if.then.i.i.i
	leaq	7(%rdx), %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	addq	%rdx, %rsi
	movq	%rax, %rdi
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	callq	varray_grow
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rax, basic_block_for_insn(%rip)
.LBB13_46:                              # %set_block_for_insn.exit.i.i
	movq	%r15, 32(%rax,%rdx,8)
	movq	(%r15), %rsi
.LBB13_47:                              # %block_label.exit.i
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	redirect_jump
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB13_79
# BB#48:                                # %if.then64.i
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %r15
	je	.LBB13_20
# BB#49:                                # %if.end68.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.try_redirect_by_replacing_jump, %rcx
	movq	$713, %rsi              # imm = 0x2C9
	jmp	.LBB13_50
.LBB13_51:                              # %if.else70.i
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %r15
	je	.LBB13_20
# BB#52:                                # %if.end.i255.i
	movq	(%r15), %rbx
	movzbl	(%rbx), %eax
	cmpq	$36, %rax
	je	.LBB13_57
# BB#53:                                # %if.then2.i259.i
	callq	gen_label_rtx
	movq	(%r15), %rsi
	movq	%rax, %rdi
	callq	emit_label_before
	movq	%rax, %rbx
	movq	%rbx, (%r15)
	movq	basic_block_for_insn(%rip), %rax
	testq	%rax, %rax
	je	.LBB13_57
# BB#54:                                # %if.then6.i265.i
	movslq	8(%rbx), %rbx
	movq	(%rax), %rcx
	cmpq	%rcx, %rbx
	jb	.LBB13_56
# BB#55:                                # %if.then.i.i270.i
	leaq	7(%rbx), %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	addq	%rbx, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, basic_block_for_insn(%rip)
.LBB13_56:                              # %set_block_for_insn.exit.i275.i
	movq	%r15, 32(%rax,%rbx,8)
	movq	(%r15), %rbx
.LBB13_57:                              # %block_label.exit277.i
	movq	%rbx, %rdi
	callq	gen_jump
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	emit_jump_insn_after
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rax
	movq	%rbx, 64(%rax)
	movl	32(%rbx), %eax
	incq	%rax
	movl	%eax, 32(%rbx)
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	movq	%r14, %rbx
	je	.LBB13_59
# BB#58:                                # %if.then90.i
	movq	8(%rcx), %rax
	movl	8(%r14), %edx
	movl	8(%rax), %ecx
	movabsq	$.L.str44, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movq	%r14, %rbx
	jmp	.LBB13_59
.LBB13_20:                              # %if.else
	movq	24(%rbx), %rax
	cmpq	%r15, %rax
	je	.LBB13_88
# BB#21:                                # %if.end9
	movzbl	48(%rbx), %eax
	testq	$1, %rax
	jne	.LBB13_88
# BB#22:                                # %if.else14
	movzbl	(%r14), %eax
	cmpq	$33, %rax
	jne	.LBB13_88
# BB#23:                                # %if.end18
	movq	64(%r14), %rax
	testq	%rax, %rax
	je	.LBB13_107
# BB#24:                                # %land.lhs.true
	movq	24(%rax), %r12
	testq	%r12, %r12
	je	.LBB13_107
# BB#25:                                # %land.lhs.true24
	movzbl	(%r12), %eax
	cmpq	$33, %rax
	jne	.LBB13_107
# BB#26:                                # %land.lhs.true28
	movq	32(%r12), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	jne	.LBB13_107
# BB#27:                                # %if.then41
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %r15
	je	.LBB13_88
# BB#28:                                # %if.end.i
	movq	(%r15), %rdx
	movzbl	(%rdx), %eax
	cmpq	$36, %rax
	je	.LBB13_33
# BB#29:                                # %if.then2.i
	callq	gen_label_rtx
	movq	(%r15), %rsi
	movq	%rax, %rdi
	callq	emit_label_before
	movq	%rax, %rdx
	movq	%rdx, (%r15)
	movq	basic_block_for_insn(%rip), %rax
	testq	%rax, %rax
	je	.LBB13_33
# BB#30:                                # %if.then6.i
	movslq	8(%rdx), %r13
	movq	(%rax), %rcx
	cmpq	%rcx, %r13
	jb	.LBB13_32
# BB#31:                                # %if.then.i.i
	leaq	7(%r13), %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	addq	%r13, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, basic_block_for_insn(%rip)
.LBB13_32:                              # %block_label.exit.thread299
	movq	%r15, 32(%rax,%r13,8)
	movq	(%r15), %rdx
.LBB13_33:                              # %if.end48
	movq	32(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$44, %rcx
	je	.LBB13_34
# BB#89:                                # %if.end48
	addq	$16, %rax
	jmp	.LBB13_90
.LBB13_107:                             # %if.else170
	movq	%r14, %rdi
	callq	computed_jump_p
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	jne	.LBB13_88
# BB#108:                               # %lor.lhs.false173
	movq	%r14, %rdi
	callq	returnjump_p
	testq	%r12, %rax
	jne	.LBB13_88
# BB#109:                               # %if.end177
	movq	64(%r14), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB13_110
# BB#111:                               # %if.end184
	xorq	%rsi, %rsi
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %r15
	je	.LBB13_117
# BB#112:                               # %if.end.i273
	movq	(%r15), %rsi
	movzbl	(%rsi), %eax
	cmpq	$36, %rax
	je	.LBB13_117
# BB#113:                               # %if.then2.i277
	callq	gen_label_rtx
	movq	(%r15), %rsi
	movq	%rax, %rdi
	callq	emit_label_before
	movq	%rax, %rsi
	movq	%rsi, (%r15)
	movq	basic_block_for_insn(%rip), %rax
	testq	%rax, %rax
	je	.LBB13_117
# BB#114:                               # %if.then6.i283
	movslq	8(%rsi), %r13
	movq	(%rax), %rcx
	cmpq	%rcx, %r13
	jb	.LBB13_116
# BB#115:                               # %if.then.i.i288
	leaq	7(%r13), %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	addq	%r13, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, basic_block_for_insn(%rip)
.LBB13_116:                             # %set_block_for_insn.exit.i293
	movq	%r15, 32(%rax,%r13,8)
	movq	(%r15), %rsi
.LBB13_117:                             # %block_label.exit295
	xorq	%rdx, %rdx
	xorq	%r13, %r13
	movq	%r14, %rdi
	callq	redirect_jump
	testq	%r12, %rax
	jne	.LBB13_103
# BB#118:                               # %if.then188
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %r15
	je	.LBB13_88
# BB#119:                               # %if.end192
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.redirect_edge_and_branch, %rcx
	movq	$899, %rsi              # imm = 0x383
	jmp	.LBB13_50
.LBB13_62:                              #   in Loop: Header=BB13_59 Depth=1
	movq	24(%rbx), %rax
	jmp	.LBB13_64
.LBB13_59:                              # %while.body.i282.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB13_63
# BB#60:                                # %land.lhs.true.i287.i
                                        #   in Loop: Header=BB13_59 Depth=1
	movl	40(%rbx), %eax
	movabsq	$4294967197, %rcx       # imm = 0xFFFFFF9D
	cmpq	%rcx, %rax
	je	.LBB13_63
# BB#61:                                # %land.lhs.true.i287.i
                                        #   in Loop: Header=BB13_59 Depth=1
	movabsq	$4294967216, %rcx       # imm = 0xFFFFFFB0
	cmpq	%rcx, %rax
	jne	.LBB13_62
.LBB13_63:                              # %if.else.i289.i
                                        #   in Loop: Header=BB13_59 Depth=1
	movq	%rbx, %rdi
	callq	delete_insn
.LBB13_64:                              # %if.end.i292.i
                                        #   in Loop: Header=BB13_59 Depth=1
	cmpq	%r14, %rbx
	movq	%rax, %rbx
	jne	.LBB13_59
# BB#65:                                # %delete_insn_chain.exit293.i
	movq	64(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB13_76
# BB#66:                                # %land.lhs.true105.i
	movq	24(%rbx), %r14
	testq	%r14, %r14
	je	.LBB13_76
# BB#67:                                # %land.lhs.true111.i
	movzbl	(%r14), %eax
	cmpq	$33, %rax
	jne	.LBB13_76
# BB#68:                                # %land.lhs.true116.i
	movq	32(%r14), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	jne	.LBB13_76
# BB#69:
	movabsq	$4294967197, %r12       # imm = 0xFFFFFF9D
	movabsq	$4294967216, %r13       # imm = 0xFFFFFFB0
	jmp	.LBB13_70
.LBB13_34:
	addq	$8, %rax
.LBB13_90:                              # %if.end48
	movq	(%rax), %rsi
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movslq	(%rsi), %r13
	testq	%r13, %r13
	jle	.LBB13_94
# BB#91:                                # %for.body.lr.ph
	decq	%r13
.LBB13_92:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rsi,%r13,8), %rax
	movq	8(%rax), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB13_93
# BB#120:                               # %if.then75
                                        #   in Loop: Header=BB13_92 Depth=1
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$67, %rdi
	movq	%rdx, %r12
                                        # kill: RDX<def> R12<kill>
	callq	gen_rtx_fmt_u00
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r12, %rdx
	movq	%rax, 8(%rsi,%r13,8)
	movq	24(%rsp), %rcx          # 8-byte Reload
	movl	32(%rcx), %eax
	decq	%rax
	movl	%eax, 32(%rcx)
	movl	32(%rdx), %eax
	incq	%rax
	movl	%eax, 32(%rdx)
.LBB13_93:                              # %for.cond.backedge
                                        #   in Loop: Header=BB13_92 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%r13
	testq	%rax, %rax
	jg	.LBB13_92
.LBB13_94:                              # %for.end
	movq	%rdx, %r13
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB13_103
# BB#95:                                # %cond.true
	movq	32(%r14), %r12
	movzbl	(%r12), %eax
	cmpq	$47, %rax
	je	.LBB13_97
# BB#96:                                # %cond.false
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	single_set_2
	movq	%rax, %r12
.LBB13_97:                              # %cond.end112
	testq	%r12, %r12
	je	.LBB13_103
# BB#98:                                # %land.lhs.true116
	movq	8(%r12), %rax
	movq	global_rtl(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB13_103
# BB#99:                                # %land.lhs.true122
	movq	16(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$72, %rcx
	jne	.LBB13_103
# BB#100:                               # %land.lhs.true130
	movq	24(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$67, %rcx
	jne	.LBB13_103
# BB#101:                               # %land.lhs.true141
	movq	8(%rax), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB13_103
# BB#102:                               # %if.then153
	movq	$67, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %r14
	movq	%r14, %rdx
	callq	gen_rtx_fmt_u00
	movq	16(%r12), %rcx
	movq	%rax, 24(%rcx)
	movq	24(%rsp), %rcx          # 8-byte Reload
	movl	32(%rcx), %eax
	decq	%rax
	movl	%eax, 32(%rcx)
	movl	32(%r14), %eax
	incq	%rax
	movl	%eax, 32(%r14)
.LBB13_103:                             # %if.end194
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB13_105
# BB#104:                               # %if.then196
	movq	16(%rbx), %rax
	movq	24(%rbx), %rcx
	movl	88(%r15), %r8d
	movl	88(%rcx), %ecx
	movl	88(%rax), %edx
	movabsq	$.L.str2, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB13_105:                             # %if.end202
	movq	24(%rbx), %rax
	cmpq	%r15, %rax
	movq	$1, %r13
	je	.LBB13_88
# BB#106:                               # %if.then206
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	redirect_edge_succ_nodup
	jmp	.LBB13_88
.LBB13_73:                              #   in Loop: Header=BB13_70 Depth=1
	movq	24(%rbx), %rax
	jmp	.LBB13_75
.LBB13_70:                              # %while.body.i298.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB13_74
# BB#71:                                # %land.lhs.true.i303.i
                                        #   in Loop: Header=BB13_70 Depth=1
	movl	40(%rbx), %eax
	cmpq	%r12, %rax
	je	.LBB13_74
# BB#72:                                # %land.lhs.true.i303.i
                                        #   in Loop: Header=BB13_70 Depth=1
	cmpq	%r13, %rax
	jne	.LBB13_73
.LBB13_74:                              # %if.else.i305.i
                                        #   in Loop: Header=BB13_70 Depth=1
	movq	%rbx, %rdi
	callq	delete_insn
.LBB13_75:                              # %if.end.i308.i
                                        #   in Loop: Header=BB13_70 Depth=1
	cmpq	%r14, %rbx
	movq	%rax, %rbx
	jne	.LBB13_70
.LBB13_76:                              # %if.end136.i
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdi
	callq	next_nonnote_insn
	testq	%rax, %rax
	je	.LBB13_78
# BB#77:                                # %lor.lhs.false140.i
	movzbl	(%rax), %eax
	xorq	%r12, %r12
	cmpq	$35, %rax
	je	.LBB13_79
.LBB13_78:                              # %if.then145.i
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdi
	callq	emit_barrier_after
	xorq	%r12, %r12
.LBB13_79:                              # %while.cond.preheader.i
	movq	16(%rsp), %r14          # 8-byte Reload
	jmp	.LBB13_81
	.align	16, 0x90
.LBB13_80:                              # %while.body.i
                                        #   in Loop: Header=BB13_81 Depth=1
	movq	%rbx, %rdi
	callq	remove_edge
.LBB13_81:                              # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	40(%r14), %rbx
	cmpq	$0, 8(%rbx)
	jne	.LBB13_80
# BB#82:                                # %while.end.i
	movl	%r12d, 48(%rbx)
	movq	$10000, %rax            # imm = 0x2710
	movl	%eax, 52(%rbx)
	movq	96(%r14), %rax
	movq	%rax, 56(%rbx)
	jmp	.LBB13_83
.LBB13_85:                              # %while.body177.i
                                        #   in Loop: Header=BB13_83 Depth=1
	callq	delete_insn
.LBB13_83:                              # %while.end.i
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rbx), %rax
	movq	8(%rax), %rdi
	movzbl	(%rdi), %eax
	cmpq	$37, %rax
	jne	.LBB13_86
# BB#84:                                # %land.rhs.i
                                        #   in Loop: Header=BB13_83 Depth=1
	movslq	40(%rdi), %rax
	testq	%rax, %rax
	jns	.LBB13_85
.LBB13_86:                              # %while.end181.i
	movq	24(%rbx), %rax
	cmpq	%r15, %rax
	movq	$1, %r13
	je	.LBB13_88
# BB#87:                                # %if.then185.i
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	redirect_edge_succ
.LBB13_88:                              # %return
	movq	%r13, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB13_110:                             # %if.then183
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.redirect_edge_and_branch, %rcx
	movq	$890, %rsi              # imm = 0x37A
.LBB13_50:                              # %if.end68.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp174:
	.size	redirect_edge_and_branch, .Ltmp174-redirect_edge_and_branch
	.cfi_endproc

	.globl	force_nonfallthru
	.align	16, 0x90
	.type	force_nonfallthru,@function
force_nonfallthru:                      # @force_nonfallthru
	.cfi_startproc
# BB#0:                                 # %entry
	movq	24(%rdi), %rsi
	jmp	force_nonfallthru_and_redirect  # TAILCALL
.Ltmp175:
	.size	force_nonfallthru, .Ltmp175-force_nonfallthru
	.cfi_endproc

	.align	16, 0x90
	.type	force_nonfallthru_and_redirect,@function
force_nonfallthru_and_redirect:         # @force_nonfallthru_and_redirect
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp182:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp183:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp184:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp185:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp186:
	.cfi_def_cfa_offset 48
.Ltmp187:
	.cfi_offset %rbx, -48
.Ltmp188:
	.cfi_offset %r12, -40
.Ltmp189:
	.cfi_offset %r13, -32
.Ltmp190:
	.cfi_offset %r14, -24
.Ltmp191:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movl	48(%r15), %eax
	movq	%rsi, %r14
	testq	$2, %rax
	jne	.LBB15_1
# BB#3:                                 # %if.else
	testq	$1, %rax
	je	.LBB15_4
# BB#5:                                 # %if.else5
	movq	16(%r15), %r13
	movabsq	$entry_exit_blocks, %rax
	cmpq	%rax, %r13
	jne	.LBB15_16
# BB#6:                                 # %if.then6
	movq	24(%r15), %rax
	movq	(%rax), %r12
	movl	n_basic_blocks(%rip), %esi
	movq	basic_block_info(%rip), %rdi
	incq	%rsi
	movl	%esi, n_basic_blocks(%rip)
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	callq	varray_grow
	movl	n_basic_blocks(%rip), %edx
	movq	%rax, basic_block_info(%rip)
	leaq	-1(%rdx), %rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	jle	.LBB15_10
# BB#7:                                 # %for.body.lr.ph.i
	movq	$32, %rcx
	movq	%rsi, %r8
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	leaq	-2(%rdx), %rdi
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	32(%rax,%rbx,8), %rcx
	testq	%rbx, %rbx
	movq	%rcx, 32(%rax,%r8,8)
	movl	%esi, 88(%rcx)
	jle	.LBB15_10
# BB#8:                                 # %for.body.for.body_crit_edge.i.preheader
	decq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%rdx
	.align	16, 0x90
.LBB15_9:                               # %for.body.for.body_crit_edge.i
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	addq	$-2, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	32(%rax,%rsi,8), %rcx
	movq	%rcx, 32(%rax,%rdx,8)
	movq	%rdx, %rsi
	movl	%edx, 88(%rcx)
	decq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rdx
	testq	%rax, %rax
	jg	.LBB15_9
.LBB15_10:                              # %create_basic_block.exit
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%rbx, %rbx
	movq	%r12, %rsi
	callq	create_basic_block_structure
	movq	%rax, %rsi
	movq	$0, 80(%rsi)
	movq	%rsi, 16(%r15)
	movq	56(%r15), %rax
	movq	%rax, 96(%rsi)
	movl	104(%rsi), %ecx
	movl	52(%r15), %eax
	imulq	%rcx, %rax
	addq	$5000, %rax             # imm = 0x1388
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
	movl	%edx, 104(%rsi)
	movl	%ebx, 92(%rsi)
	movq	entry_exit_blocks+40(%rip), %rax
	testq	%rax, %rax
	je	.LBB15_15
# BB#11:
	movabsq	$entry_exit_blocks+40, %rcx
	.align	16, 0x90
.LBB15_13:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r15, %rax
	je	.LBB15_14
# BB#12:                                # %for.cond
                                        #   in Loop: Header=BB15_13 Depth=1
	movq	%rax, %rcx
	addq	$8, %rcx
	movq	(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB15_13
	jmp	.LBB15_15
.LBB15_14:                              # %if.then13
	movq	8(%r15), %rax
	movq	%rax, (%rcx)
.LBB15_15:                              # %for.end
	movq	$0, 8(%r15)
	movq	%r15, 40(%rsi)
	movabsq	$entry_exit_blocks, %rdi
	movq	$1, %rdx
	callq	make_single_succ_edge
	movq	16(%r15), %r13
.LBB15_16:                              # %if.end19
	movq	40(%r13), %rax
	xorq	%r12, %r12
	cmpq	$0, 8(%rax)
	je	.LBB15_52
# BB#17:                                # %if.then24
	movq	8(%r13), %rax
	movabsq	$4294967200, %rcx       # imm = 0xFFFFFFA0
	movabsq	$4294967216, %rdx       # imm = 0xFFFFFFB0
	jmp	.LBB15_18
	.align	16, 0x90
.LBB15_22:                              #   in Loop: Header=BB15_18 Depth=1
	movq	%rsi, %rax
.LBB15_18:                              # %for.cond.outer.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB15_19 Depth 2
	movq	%rax, %rsi
.LBB15_19:                              # %for.cond.i
                                        #   Parent Loop BB15_18 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rsi), %rsi
	testq	%rsi, %rsi
	je	.LBB15_24
# BB#20:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB15_19 Depth=2
	movzbl	(%rsi), %edi
	cmpq	$37, %rdi
	jne	.LBB15_24
# BB#21:                                # %land.rhs.i
                                        #   in Loop: Header=BB15_19 Depth=2
	movl	40(%rsi), %edi
	cmpq	%rcx, %rdi
	je	.LBB15_22
# BB#23:                                # %land.rhs.i
                                        #   in Loop: Header=BB15_19 Depth=2
	cmpq	%rdx, %rdi
	jne	.LBB15_19
.LBB15_24:                              # %last_loop_beg_note.exit
	movq	24(%rax), %rax
	xorq	%r12, %r12
	testq	%rax, %rax
	je	.LBB15_33
# BB#25:                                # %land.lhs.true
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB15_26
# BB#27:                                # %land.lhs.true29
	movq	24(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB15_28
# BB#29:                                # %land.lhs.true34
	movzbl	(%rcx), %edx
	cmpq	$33, %rdx
	jne	.LBB15_30
# BB#31:                                # %land.lhs.true41
	movq	32(%rcx), %rdx
	movl	(%rdx), %edx
	andq	$65534, %rdx            # imm = 0xFFFE
	cmpq	$44, %rdx
	movq	%rax, %r12
	jne	.LBB15_33
# BB#32:                                # %if.then60
	movq	24(%rcx), %r12
	jmp	.LBB15_33
.LBB15_26:
	movq	%rax, %r12
	jmp	.LBB15_33
.LBB15_28:
	movq	%rax, %r12
	jmp	.LBB15_33
.LBB15_30:
	movq	%rax, %r12
.LBB15_33:                              # %if.end67
	movl	88(%r13), %r13d
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movl	n_basic_blocks(%rip), %esi
	movq	basic_block_info(%rip), %rdi
	incq	%rsi
	movl	%esi, n_basic_blocks(%rip)
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	callq	varray_grow
	movl	n_basic_blocks(%rip), %edx
	movq	%rax, basic_block_info(%rip)
	leaq	-1(%rdx), %rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rbx, %rdi
	jle	.LBB15_37
# BB#34:                                # %for.body.lr.ph.i348
	movq	$32, %rcx
	movq	%rsi, %r8
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	leaq	-2(%rdx), %rdi
	movq	$32, %rcx
	movq	%rdi, %r9
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	$32, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	movq	32(%rax,%r9,8), %r10
	sarq	%cl, %rbx
	cmpq	%rbx, %r9
	movq	%r10, 32(%rax,%r8,8)
	movl	%esi, 88(%r10)
	jle	.LBB15_37
# BB#35:                                # %for.body.for.body_crit_edge.i364.preheader
	decq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%rdx
	.align	16, 0x90
.LBB15_36:                              # %for.body.for.body_crit_edge.i364
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	addq	$-2, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	32(%rax,%rsi,8), %rcx
	movq	%rcx, 32(%rax,%rdx,8)
	movq	%rdx, %rsi
	movl	%edx, 88(%rcx)
	decq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	decq	%rdx
	cmpq	%rbx, %rax
	jg	.LBB15_36
.LBB15_37:                              # %create_basic_block.exit367
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	create_basic_block_structure
	movq	%rax, %r12
	movq	$0, 80(%r12)
	movq	56(%r15), %rax
	movq	%rax, 96(%r12)
	movq	16(%r15), %rdi
	movl	104(%rdi), %ecx
	movl	52(%r15), %eax
	imulq	%rcx, %rax
	addq	$5000, %rax             # imm = 0x1388
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
	movl	%edx, 104(%r12)
	movl	92(%r14), %eax
	movl	%eax, 92(%r12)
	cmpq	$0, 64(%r14)
	je	.LBB15_51
# BB#38:                                # %if.then83
	movq	flow_obstack+32(%rip), %rcx
	movq	flow_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$23, %rcx
	jg	.LBB15_40
# BB#39:                                # %if.then86
	movabsq	$flow_obstack, %rdi
	movq	$24, %rsi
	callq	_obstack_newchunk
	movq	flow_obstack+24(%rip), %rax
.LBB15_40:                              # %if.end87
	addq	$24, %rax
	movq	%rax, flow_obstack+24(%rip)
	movq	flow_obstack+16(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB15_42
# BB#41:                                # %if.then94
	movzbl	flow_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, flow_obstack+80(%rip)
.LBB15_42:                              # %if.end97
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
	jle	.LBB15_44
# BB#43:                                # %if.then119
	movq	%rbx, flow_obstack+24(%rip)
	movq	%rbx, %rcx
.LBB15_44:                              # %if.end122
	movq	%rcx, flow_obstack+16(%rip)
	callq	bitmap_initialize
	movq	%rax, 64(%r12)
	movq	flow_obstack+32(%rip), %rcx
	movq	flow_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$23, %rcx
	jg	.LBB15_46
# BB#45:                                # %if.then142
	movabsq	$flow_obstack, %rdi
	movq	$24, %rsi
	callq	_obstack_newchunk
	movq	flow_obstack+24(%rip), %rax
.LBB15_46:                              # %if.end143
	addq	$24, %rax
	movq	%rax, flow_obstack+24(%rip)
	movq	flow_obstack+16(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB15_48
# BB#47:                                # %if.then156
	movzbl	flow_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, flow_obstack+80(%rip)
.LBB15_48:                              # %if.end161
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
	jle	.LBB15_50
# BB#49:                                # %if.then186
	movq	%rbx, flow_obstack+24(%rip)
	movq	%rbx, %rcx
.LBB15_50:                              # %if.end189
	movq	%rcx, flow_obstack+16(%rip)
	callq	bitmap_initialize
	movq	%rax, 72(%r12)
	movq	64(%r12), %rdi
	movq	64(%r14), %rsi
	callq	bitmap_copy
	movq	72(%r12), %rdi
	movq	64(%r14), %rsi
	callq	bitmap_copy
	movq	16(%r15), %rdi
.LBB15_51:                              # %if.end198
	movq	$1, %rdx
	movq	%r12, %rsi
	callq	make_edge
	movl	52(%r15), %ecx
	movl	%ecx, 52(%rax)
	movq	56(%r15), %rcx
	movq	%rcx, 56(%rax)
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	redirect_edge_pred
	movq	$10000, %rax            # imm = 0x2710
	movl	%eax, 52(%r15)
	movq	%r12, %r13
.LBB15_52:                              # %if.end208
	movl	48(%r15), %eax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rax, %rcx
	movl	%ecx, 48(%r15)
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %r14
	je	.LBB15_53
# BB#56:                                # %if.end.i
	movq	(%r14), %rbx
	movzbl	(%rbx), %eax
	cmpq	$36, %rax
	je	.LBB15_61
# BB#57:                                # %if.then2.i
	callq	gen_label_rtx
	movq	(%r14), %rsi
	movq	%rax, %rdi
	callq	emit_label_before
	movq	%rax, %rbx
	movq	%rbx, (%r14)
	movq	basic_block_for_insn(%rip), %rax
	testq	%rax, %rax
	je	.LBB15_61
# BB#58:                                # %if.then6.i
	movslq	8(%rbx), %rbx
	movq	(%rax), %rcx
	cmpq	%rcx, %rbx
	jb	.LBB15_60
# BB#59:                                # %if.then.i.i
	leaq	7(%rbx), %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	addq	%rbx, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, basic_block_for_insn(%rip)
.LBB15_60:                              # %set_block_for_insn.exit.i
	movq	%r14, 32(%rax,%rbx,8)
	movq	(%r14), %rbx
.LBB15_61:                              # %block_label.exit
	movq	%rbx, %rdi
	callq	gen_jump
	movq	8(%r13), %rsi
	movq	%rax, %rdi
	callq	emit_jump_insn_after
	movq	8(%r13), %rax
	movq	%rbx, 64(%rax)
	movl	32(%rbx), %eax
	incq	%rax
	movl	%eax, 32(%rbx)
	jmp	.LBB15_62
.LBB15_53:                              # %if.then213
	callq	ix86_can_use_return_insn_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_55
# BB#54:                                # %if.then216
	callq	gen_return
	movq	8(%r13), %rsi
	movq	%rax, %rdi
	callq	emit_jump_insn_after
.LBB15_62:                              # %if.end234
	movq	8(%r13), %rdi
	callq	emit_barrier_after
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	redirect_edge_succ_nodup
	movq	%r12, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB15_1:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.force_nonfallthru_and_redirect, %rcx
	movq	$926, %rsi              # imm = 0x39E
	jmp	.LBB15_2
.LBB15_4:                               # %if.then4
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.force_nonfallthru_and_redirect, %rcx
	movq	$928, %rsi              # imm = 0x3A0
	jmp	.LBB15_2
.LBB15_55:                              # %if.else220
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.force_nonfallthru_and_redirect, %rcx
	movq	$1007, %rsi             # imm = 0x3EF
.LBB15_2:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp192:
	.size	force_nonfallthru_and_redirect, .Ltmp192-force_nonfallthru_and_redirect
	.cfi_endproc

	.globl	redirect_edge_and_branch_force
	.align	16, 0x90
	.type	redirect_edge_and_branch_force,@function
redirect_edge_and_branch_force:         # @redirect_edge_and_branch_force
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp196:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp197:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp198:
	.cfi_def_cfa_offset 32
.Ltmp199:
	.cfi_offset %rbx, -24
.Ltmp200:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
                                        # kill: RSI<def> R14<kill>
	callq	redirect_edge_and_branch
	testq	%rax, %rax
	jne	.LBB16_2
# BB#1:                                 # %lor.lhs.false
	movq	24(%rbx), %rax
	cmpq	%r14, %rax
	je	.LBB16_2
# BB#3:                                 # %if.end
	movq	%rbx, %rdi
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	force_nonfallthru_and_redirect  # TAILCALL
.LBB16_2:                               # %return
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp201:
	.size	redirect_edge_and_branch_force, .Ltmp201-redirect_edge_and_branch_force
	.cfi_endproc

	.globl	tidy_fallthru_edge
	.align	16, 0x90
	.type	tidy_fallthru_edge,@function
tidy_fallthru_edge:                     # @tidy_fallthru_edge
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp208:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp209:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp210:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp211:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp212:
	.cfi_def_cfa_offset 48
.Ltmp213:
	.cfi_offset %rbx, -48
.Ltmp214:
	.cfi_offset %r12, -40
.Ltmp215:
	.cfi_offset %r13, -32
.Ltmp216:
	.cfi_offset %r14, -24
.Ltmp217:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	8(%r12), %rax
	movq	%rdx, %r15
	movq	%rdi, %r14
	movq	%rax, %rdi
	callq	next_real_insn
	movq	(%r15), %rcx
	movq	16(%rcx), %rdi
	movq	%rax, %rbx
	callq	next_real_insn
	cmpq	%rax, %rbx
	jne	.LBB17_17
# BB#1:                                 # %if.end
	movq	8(%r12), %rbx
	movzbl	(%rbx), %eax
	cmpq	$33, %rax
	jne	.LBB17_8
# BB#2:                                 # %land.lhs.true
	movq	%rbx, %rdi
	callq	onlyjump_p
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB17_8
# BB#3:                                 # %land.lhs.true5
	movq	%rbx, %rdi
	callq	any_uncondjump_p
	testq	%r13, %rax
	jne	.LBB17_6
# BB#4:                                 # %lor.lhs.false
	movq	40(%r12), %rax
	cmpq	%r14, %rax
	jne	.LBB17_8
# BB#5:                                 # %land.lhs.true9
	cmpq	$0, 8(%r14)
	jne	.LBB17_8
	.align	16, 0x90
.LBB17_6:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rbx), %rbx
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB17_8
# BB#7:                                 # %land.rhs
                                        #   in Loop: Header=BB17_6 Depth=1
	movslq	40(%rbx), %rax
	testq	%rax, %rax
	jns	.LBB17_6
.LBB17_8:                               # %if.end24
	movq	(%r15), %rax
	movq	16(%rax), %r15
	cmpq	%r15, %rbx
	je	.LBB17_16
# BB#9:                                 # %if.then30
	movq	24(%rbx), %rbx
	movabsq	$4294967197, %r12       # imm = 0xFFFFFF9D
	movabsq	$4294967216, %r13       # imm = 0xFFFFFFB0
	jmp	.LBB17_10
.LBB17_13:                              #   in Loop: Header=BB17_10 Depth=1
	movq	24(%rbx), %rax
	jmp	.LBB17_15
	.align	16, 0x90
.LBB17_10:                              # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB17_14
# BB#11:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB17_10 Depth=1
	movl	40(%rbx), %eax
	cmpq	%r12, %rax
	je	.LBB17_14
# BB#12:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB17_10 Depth=1
	cmpq	%r13, %rax
	jne	.LBB17_13
	.align	16, 0x90
.LBB17_14:                              # %if.else.i
                                        #   in Loop: Header=BB17_10 Depth=1
	movq	%rbx, %rdi
	callq	delete_insn
.LBB17_15:                              # %if.end.i
                                        #   in Loop: Header=BB17_10 Depth=1
	cmpq	%r15, %rbx
	movq	%rax, %rbx
	jne	.LBB17_10
.LBB17_16:                              # %if.end38
	movl	48(%r14), %eax
	orq	$1, %rax
	movl	%eax, 48(%r14)
.LBB17_17:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp218:
	.size	tidy_fallthru_edge, .Ltmp218-tidy_fallthru_edge
	.cfi_endproc

	.globl	tidy_fallthru_edges
	.align	16, 0x90
	.type	tidy_fallthru_edges,@function
tidy_fallthru_edges:                    # @tidy_fallthru_edges
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp225:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp226:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp227:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp228:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp229:
	.cfi_def_cfa_offset 48
.Ltmp230:
	.cfi_offset %rbx, -48
.Ltmp231:
	.cfi_offset %r12, -40
.Ltmp232:
	.cfi_offset %r13, -32
.Ltmp233:
	.cfi_offset %r14, -24
.Ltmp234:
	.cfi_offset %r15, -16
	movslq	n_basic_blocks(%rip), %rax
	cmpq	$2, %rax
	jl	.LBB18_10
# BB#1:
	movq	$2, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB18_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	16(%rax,%rbx,8), %r14
	movq	40(%r14), %r12
	testq	%r12, %r12
	je	.LBB18_9
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB18_2 Depth=1
	movzbl	48(%r12), %ecx
	testq	$14, %rcx
	jne	.LBB18_9
# BB#4:                                 # %land.lhs.true6
                                        #   in Loop: Header=BB18_2 Depth=1
	cmpq	$0, 8(%r12)
	jne	.LBB18_9
# BB#5:                                 # %land.lhs.true8
                                        #   in Loop: Header=BB18_2 Depth=1
	movq	24(%rax,%rbx,8), %r15
	movq	24(%r12), %rax
	cmpq	%r15, %rax
	jne	.LBB18_9
# BB#6:                                 # %land.lhs.true10
                                        #   in Loop: Header=BB18_2 Depth=1
	movq	8(%r14), %rdi
	movzbl	(%rdi), %eax
	cmpq	$33, %rax
	jne	.LBB18_8
# BB#7:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB18_2 Depth=1
	callq	onlyjump_p
	testq	%r13, %rax
	je	.LBB18_9
.LBB18_8:                               # %if.then
                                        #   in Loop: Header=BB18_2 Depth=1
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	tidy_fallthru_edge
	.align	16, 0x90
.LBB18_9:                               # %for.inc
                                        #   in Loop: Header=BB18_2 Depth=1
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB18_2
.LBB18_10:                              # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp235:
	.size	tidy_fallthru_edges, .Ltmp235-tidy_fallthru_edges
	.cfi_endproc

	.globl	split_edge
	.align	16, 0x90
	.type	split_edge,@function
split_edge:                             # @split_edge
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp241:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp242:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp243:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp244:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp245:
	.cfi_def_cfa_offset 48
.Ltmp246:
	.cfi_offset %rbx, -40
.Ltmp247:
	.cfi_offset %r12, -32
.Ltmp248:
	.cfi_offset %r14, -24
.Ltmp249:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movl	48(%r15), %eax
	testq	$2, %rax
	jne	.LBB19_1
# BB#3:                                 # %if.end
	testq	$1, %rax
	jne	.LBB19_9
# BB#4:                                 # %if.then4
	movq	24(%r15), %rax
	movq	32(%rax), %rdi
	jmp	.LBB19_6
	.align	16, 0x90
.LBB19_5:                               # %for.cond
                                        #   in Loop: Header=BB19_6 Depth=1
	movq	(%rdi), %rdi
.LBB19_6:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	testq	%rdi, %rdi
	je	.LBB19_9
# BB#7:                                 # %for.body
                                        #   in Loop: Header=BB19_6 Depth=1
	movzbl	48(%rdi), %eax
	testq	$1, %rax
	je	.LBB19_5
# BB#8:                                 # %if.then11
	movq	24(%rdi), %rsi
	callq	force_nonfallthru_and_redirect
.LBB19_9:                               # %if.end13
	movq	24(%r15), %rdx
	xorq	%r14, %r14
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %rdx
	movq	%rax, %rcx
	je	.LBB19_27
# BB#10:                                # %land.lhs.true
	movq	(%rdx), %r14
	movq	16(%r14), %rsi
	testq	%rsi, %rsi
	je	.LBB19_26
# BB#11:                                # %land.lhs.true18
	movzbl	(%rsi), %ecx
	cmpq	$37, %rcx
	jne	.LBB19_26
# BB#12:                                # %land.lhs.true25
	movl	40(%rsi), %ecx
	movabsq	$4294967200, %r8        # imm = 0xFFFFFFA0
	cmpq	%r8, %rcx
	jne	.LBB19_26
# BB#13:                                # %land.lhs.true34
	movq	16(%r15), %rcx
	movslq	88(%rdx), %rdi
	movslq	88(%rcx), %rbx
	cmpq	%rbx, %rdi
	jg	.LBB19_14
# BB#15:                                # %if.else.i
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	andq	%r9, %rbx
	andq	%r9, %rdi
	cmpq	%rbx, %rdi
	je	.LBB19_26
# BB#16:                                # %if.end6.i
	movq	8(%rdx), %rdi
	movq	(%rcx), %r9
	cmpq	%r9, %rdi
	je	.LBB19_26
# BB#17:
	xorq	%r11, %r11
	movabsq	$4294967201, %r10       # imm = 0xFFFFFFA1
	.align	16, 0x90
.LBB19_18:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdi), %ecx
	cmpq	$37, %rcx
	jne	.LBB19_23
# BB#19:                                # %if.then10.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movl	40(%rdi), %ecx
	cmpq	%r10, %rcx
	jne	.LBB19_20
# BB#22:                                # %if.then18.i
                                        #   in Loop: Header=BB19_18 Depth=1
	decq	%r11
	jmp	.LBB19_23
.LBB19_20:                              # %if.then10.i
                                        #   in Loop: Header=BB19_18 Depth=1
	cmpq	%r8, %rcx
	jne	.LBB19_23
# BB#21:                                # %if.then12.i
                                        #   in Loop: Header=BB19_18 Depth=1
	incq	%r11
	.align	16, 0x90
.LBB19_23:                              # %for.inc.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	24(%rdi), %rdi
	cmpq	%r9, %rdi
	je	.LBB19_25
# BB#24:                                # %for.inc.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	$32, %rcx
	movq	%r11, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	jns	.LBB19_18
.LBB19_25:                              # %back_edge_of_syntactic_loop_p.exit
	movq	$32, %rcx
	shlq	%cl, %r11
	movq	$32, %rcx
	sarq	%cl, %r11
	testq	%r11, %r11
	js	.LBB19_14
.LBB19_26:                              # %if.then45
	movq	%rdx, %rcx
.LBB19_27:                              # %if.end50
	cmpq	%rax, %rcx
	je	.LBB19_28
# BB#29:                                # %if.end50
	addq	$88, %rcx
	jmp	.LBB19_30
.LBB19_28:
	movabsq	$n_basic_blocks, %rcx
.LBB19_30:                              # %if.end50
	movslq	(%rcx), %r12
	movl	n_basic_blocks(%rip), %esi
	movq	basic_block_info(%rip), %rdi
	incq	%rsi
	movl	%esi, n_basic_blocks(%rip)
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	callq	varray_grow
	movl	n_basic_blocks(%rip), %edx
	movq	%rax, basic_block_info(%rip)
	leaq	-1(%rdx), %rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%r12, %rdi
	jle	.LBB19_34
# BB#31:                                # %for.body.lr.ph.i
	movq	$32, %rcx
	movq	%rsi, %r8
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	leaq	-2(%rdx), %rdi
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	32(%rax,%rbx,8), %rcx
	cmpq	%r12, %rbx
	movq	%rcx, 32(%rax,%r8,8)
	movl	%esi, 88(%rcx)
	jle	.LBB19_34
# BB#32:                                # %for.body.for.body_crit_edge.i.preheader
	decq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%rdx
	.align	16, 0x90
.LBB19_33:                              # %for.body.for.body_crit_edge.i
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	addq	$-2, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	32(%rax,%rsi,8), %rcx
	movq	%rcx, 32(%rax,%rdx,8)
	movq	%rdx, %rsi
	movl	%edx, 88(%rcx)
	decq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rdx
	cmpq	%r12, %rax
	jg	.LBB19_33
.LBB19_34:                              # %create_basic_block.exit
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	create_basic_block_structure
	movq	%rax, %r14
	movq	$0, 80(%r14)
	movq	56(%r15), %rax
	movq	%rax, 96(%r14)
	movq	16(%r15), %rax
	movl	104(%rax), %ecx
	movl	52(%r15), %eax
	imulq	%rcx, %rax
	addq	$5000, %rax             # imm = 0x1388
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
	movl	%edx, 104(%r14)
	movq	24(%r15), %rsi
	movl	92(%rsi), %eax
	movl	%eax, 92(%r14)
	cmpq	$0, 64(%rsi)
	je	.LBB19_48
# BB#35:                                # %if.then62
	movq	flow_obstack+32(%rip), %rcx
	movq	flow_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$23, %rcx
	jg	.LBB19_37
# BB#36:                                # %if.then65
	movabsq	$flow_obstack, %rdi
	movq	$24, %rsi
	callq	_obstack_newchunk
	movq	flow_obstack+24(%rip), %rax
.LBB19_37:                              # %if.end66
	addq	$24, %rax
	movq	%rax, flow_obstack+24(%rip)
	movq	flow_obstack+16(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB19_39
# BB#38:                                # %if.then73
	movzbl	flow_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, flow_obstack+80(%rip)
.LBB19_39:                              # %if.end76
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
	jle	.LBB19_41
# BB#40:                                # %if.then98
	movq	%rbx, flow_obstack+24(%rip)
	movq	%rbx, %rcx
.LBB19_41:                              # %if.end101
	movq	%rcx, flow_obstack+16(%rip)
	callq	bitmap_initialize
	movq	%rax, 64(%r14)
	movq	flow_obstack+32(%rip), %rcx
	movq	flow_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$23, %rcx
	jg	.LBB19_43
# BB#42:                                # %if.then121
	movabsq	$flow_obstack, %rdi
	movq	$24, %rsi
	callq	_obstack_newchunk
	movq	flow_obstack+24(%rip), %rax
.LBB19_43:                              # %if.end122
	addq	$24, %rax
	movq	%rax, flow_obstack+24(%rip)
	movq	flow_obstack+16(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB19_45
# BB#44:                                # %if.then135
	movzbl	flow_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, flow_obstack+80(%rip)
.LBB19_45:                              # %if.end140
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
	jle	.LBB19_47
# BB#46:                                # %if.then165
	movq	%rbx, flow_obstack+24(%rip)
	movq	%rbx, %rcx
.LBB19_47:                              # %if.end168
	movq	%rcx, flow_obstack+16(%rip)
	callq	bitmap_initialize
	movq	%rax, 72(%r14)
	movq	24(%r15), %rax
	movq	64(%r14), %rdi
	movq	64(%rax), %rsi
	callq	bitmap_copy
	movq	24(%r15), %rax
	movq	72(%r14), %rdi
	movq	64(%rax), %rsi
	callq	bitmap_copy
	movq	24(%r15), %rsi
.LBB19_48:                              # %if.end179
	movq	$1, %rdx
	movq	%r14, %rdi
	callq	make_single_succ_edge
	movzbl	48(%r15), %eax
	movq	%r15, %rdi
	movq	%r14, %rsi
	testq	$1, %rax
	je	.LBB19_49
# BB#51:                                # %if.else190
	callq	redirect_edge_succ
	jmp	.LBB19_52
.LBB19_49:                              # %if.then186
	callq	redirect_edge_and_branch
	testq	%rax, %rax
	je	.LBB19_50
.LBB19_52:                              # %if.end191
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB19_14:
	movq	%rdx, %rcx
	movq	%rsi, %r14
	jmp	.LBB19_27
.LBB19_1:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.split_edge, %rcx
	movq	$1190, %rsi             # imm = 0x4A6
	jmp	.LBB19_2
.LBB19_50:                              # %if.then188
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.split_edge, %rcx
	movq	$1260, %rsi             # imm = 0x4EC
.LBB19_2:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp250:
	.size	split_edge, .Ltmp250-split_edge
	.cfi_endproc

	.globl	insert_insn_on_edge
	.align	16, 0x90
	.type	insert_insn_on_edge,@function
insert_insn_on_edge:                    # @insert_insn_on_edge
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp254:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp255:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp256:
	.cfi_def_cfa_offset 32
.Ltmp257:
	.cfi_offset %rbx, -24
.Ltmp258:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movzbl	48(%rbx), %eax
	movq	%rdi, %r14
	testq	$2, %rax
	je	.LBB20_3
# BB#1:                                 # %land.lhs.true
	movq	16(%rbx), %rax
	movq	40(%rax), %rax
	cmpq	$0, 8(%rax)
	je	.LBB20_3
# BB#2:                                 # %land.lhs.true2
	movq	24(%rbx), %rax
	movq	32(%rax), %rax
	cmpq	$0, (%rax)
	jne	.LBB20_7
.LBB20_3:                               # %if.end
	movq	32(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB20_4
# BB#5:                                 # %if.else
	callq	push_to_sequence
	jmp	.LBB20_6
.LBB20_4:                               # %if.then4
	callq	start_sequence
.LBB20_6:                               # %if.end6
	movq	%r14, %rdi
	callq	emit_insn
	callq	get_insns
	movq	%rax, 32(%rbx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	end_sequence  # TAILCALL
.LBB20_7:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.insert_insn_on_edge, %rcx
	movq	$1280, %rsi             # imm = 0x500
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp259:
	.size	insert_insn_on_edge, .Ltmp259-insert_insn_on_edge
	.cfi_endproc

	.globl	commit_edge_insertions
	.align	16, 0x90
	.type	commit_edge_insertions,@function
commit_edge_insertions:                 # @commit_edge_insertions
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp266:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp267:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp268:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp269:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp270:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp271:
	.cfi_def_cfa_offset 64
.Ltmp272:
	.cfi_offset %rbx, -48
.Ltmp273:
	.cfi_offset %r12, -40
.Ltmp274:
	.cfi_offset %r13, -32
.Ltmp275:
	.cfi_offset %r14, -24
.Ltmp276:
	.cfi_offset %r15, -16
	xorq	%rsi, %rsi
	movabsq	$entry_exit_blocks, %rax
	jmp	.LBB21_1
	.align	16, 0x90
.LBB21_5:                               # %if.end3
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rax
	movq	basic_block_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %rax
	incq	%rsi
.LBB21_1:                               # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB21_2 Depth 2
                                        #       Child Loop BB21_20 Depth 3
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	40(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB21_4
	.align	16, 0x90
.LBB21_2:                               # %for.body
                                        #   Parent Loop BB21_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB21_20 Depth 3
	movq	32(%rdi), %r15
	movq	8(%rdi), %r13
	testq	%r15, %r15
	je	.LBB21_3
# BB#6:                                 # %if.then
                                        #   in Loop: Header=BB21_2 Depth=2
	movq	$0, 32(%rdi)
	movq	24(%rdi), %r14
	movq	32(%r14), %rax
	cmpq	$0, (%rax)
	jne	.LBB21_15
# BB#7:                                 # %if.then
                                        #   in Loop: Header=BB21_2 Depth=2
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %r14
	je	.LBB21_15
# BB#8:                                 # %if.then.i
                                        #   in Loop: Header=BB21_2 Depth=2
	movq	(%r14), %rbx
	movl	(%rbx), %ecx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$36, %rax
	movq	%rbx, %rax
	jne	.LBB21_10
# BB#9:                                 # %if.then9.i
                                        #   in Loop: Header=BB21_2 Depth=2
	movq	24(%rbx), %rax
	movl	(%rax), %ecx
.LBB21_10:                              # %if.end.i
                                        #   in Loop: Header=BB21_2 Depth=2
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$37, %rcx
	jne	.LBB21_13
# BB#11:                                # %land.lhs.true13.i
                                        #   in Loop: Header=BB21_2 Depth=2
	movl	40(%rax), %ecx
	movabsq	$4294967216, %rdx       # imm = 0xFFFFFFB0
	cmpq	%rdx, %rcx
	jne	.LBB21_13
# BB#12:                                # %if.then17.i
                                        #   in Loop: Header=BB21_2 Depth=2
	movq	24(%rax), %rax
.LBB21_13:                              # %if.end21.i
                                        #   in Loop: Header=BB21_2 Depth=2
	cmpq	%rbx, %rax
	je	.LBB21_22
# BB#14:                                # %if.else.i
                                        #   in Loop: Header=BB21_2 Depth=2
	movq	16(%rax), %rsi
	jmp	.LBB21_27
	.align	16, 0x90
.LBB21_15:                              # %if.else29.i
                                        #   in Loop: Header=BB21_2 Depth=2
	movl	48(%rdi), %eax
	testq	$2, %rax
	jne	.LBB21_26
# BB#16:                                # %land.lhs.true31.i
                                        #   in Loop: Header=BB21_2 Depth=2
	movq	16(%rdi), %r14
	movq	40(%r14), %rcx
	cmpq	$0, 8(%rcx)
	jne	.LBB21_26
# BB#17:                                # %land.lhs.true31.i
                                        #   in Loop: Header=BB21_2 Depth=2
	movabsq	$entry_exit_blocks, %rcx
	cmpq	%rcx, %r14
	je	.LBB21_26
# BB#18:                                # %if.then36.i
                                        #   in Loop: Header=BB21_2 Depth=2
	movq	8(%r14), %rsi
	movzbl	(%rsi), %ecx
	cmpq	$33, %rcx
	jne	.LBB21_24
# BB#19:                                #   in Loop: Header=BB21_2 Depth=2
	movabsq	$4294967200, %rcx       # imm = 0xFFFFFFA0
	.align	16, 0x90
.LBB21_20:                              # %for.cond.i
                                        #   Parent Loop BB21_1 Depth=1
                                        #     Parent Loop BB21_2 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rsi, %rbx
	movq	16(%rbx), %rsi
	movzbl	(%rsi), %eax
	cmpq	$37, %rax
	jne	.LBB21_22
# BB#21:                                # %land.rhs.i
                                        #   in Loop: Header=BB21_20 Depth=3
	movl	40(%rsi), %eax
	cmpq	%rcx, %rax
	je	.LBB21_20
.LBB21_22:                              # %if.end70.i
                                        #   in Loop: Header=BB21_2 Depth=2
	xorq	%rsi, %rsi
	testq	%rbx, %rbx
	je	.LBB21_27
# BB#23:                                # %if.then71.i
                                        #   in Loop: Header=BB21_2 Depth=2
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	emit_insns_before
	movq	%rbx, %rdi
	callq	prev_nonnote_insn
	movq	%rax, %r15
	movq	$1, %r12
	jmp	.LBB21_28
.LBB21_26:                              # %if.else67.i
                                        #   in Loop: Header=BB21_2 Depth=2
	callq	split_edge
	movq	%rax, %r14
	movq	8(%r14), %rsi
.LBB21_27:                              # %if.else74.i
                                        #   in Loop: Header=BB21_2 Depth=2
	movq	%r15, %rdi
	callq	emit_insns_after
	movq	%rax, %r15
	xorq	%r12, %r12
	xorq	%rbx, %rbx
.LBB21_28:                              # %if.end76.i
                                        #   in Loop: Header=BB21_2 Depth=2
	movq	%r15, %rdi
	callq	returnjump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB21_29
# BB#34:                                # %if.else98.i
                                        #   in Loop: Header=BB21_2 Depth=2
	movzbl	(%r15), %eax
	cmpq	$33, %rax
	jne	.LBB21_35
	jmp	.LBB21_38
	.align	16, 0x90
.LBB21_29:                              # %if.then79.i
                                        #   in Loop: Header=BB21_2 Depth=2
	movq	40(%r14), %rax
	movq	24(%rax), %rcx
	movabsq	$entry_exit_blocks+112, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB21_37
# BB#30:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB21_2 Depth=2
	cmpq	$0, 8(%rax)
	jne	.LBB21_37
# BB#31:                                # %lor.lhs.false85.i
                                        #   in Loop: Header=BB21_2 Depth=2
	movl	48(%rax), %ecx
	testq	$1, %rcx
	je	.LBB21_37
# BB#32:                                # %if.end90.i
                                        #   in Loop: Header=BB21_2 Depth=2
	movabsq	$4294967294, %rdx       # imm = 0xFFFFFFFE
	andq	%rdx, %rcx
	movl	%ecx, 48(%rax)
	movq	%r15, %rdi
	callq	emit_barrier_after
	testq	%r12, %r12
	je	.LBB21_35
# BB#33:                                # %if.then95.i
                                        #   in Loop: Header=BB21_2 Depth=2
	movq	%rbx, %rdi
	callq	delete_insn
.LBB21_35:                              # %commit_one_edge_insertion.exit
                                        #   in Loop: Header=BB21_2 Depth=2
	movq	%r14, %rdi
	callq	find_sub_basic_blocks
.LBB21_3:                               # %for.cond.backedge
                                        #   in Loop: Header=BB21_2 Depth=2
	testq	%r13, %r13
	movq	%r13, %rdi
	jne	.LBB21_2
	jmp	.LBB21_4
.LBB21_24:                              # %if.else59.i
                                        #   in Loop: Header=BB21_2 Depth=2
	testq	$1, %rax
	jne	.LBB21_27
	jmp	.LBB21_25
	.align	16, 0x90
.LBB21_4:                               # %for.end
                                        #   in Loop: Header=BB21_1 Depth=1
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB21_5
# BB#36:                                # %while.end
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB21_37:                              # %if.then89.i
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.commit_one_edge_insertion, %rdx
	movq	$1384, %rsi             # imm = 0x568
	callq	fancy_abort
.LBB21_38:                              # %if.then102.i
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.commit_one_edge_insertion, %rdx
	movq	$1393, %rsi             # imm = 0x571
	callq	fancy_abort
.LBB21_25:                              # %if.then63.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.commit_one_edge_insertion, %rcx
	movq	$1350, %rsi             # imm = 0x546
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp277:
	.size	commit_edge_insertions, .Ltmp277-commit_edge_insertions
	.cfi_endproc

	.globl	dump_bb
	.align	16, 0x90
	.type	dump_bb,@function
dump_bb:                                # @dump_bb
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp283:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp284:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp285:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp286:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp287:
	.cfi_def_cfa_offset 48
.Ltmp288:
	.cfi_offset %rbx, -40
.Ltmp289:
	.cfi_offset %r12, -32
.Ltmp290:
	.cfi_offset %r14, -24
.Ltmp291:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	92(%r14), %ecx
	movl	88(%r14), %edx
	movq	%rsi, %r15
	movabsq	$.L.str3, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	fprintf
	movq	96(%r14), %rdx
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	fprintf
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	putc
	movabsq	$.L.str5, %rdi
	movq	$17, %rsi
	movq	$1, %rdx
	movq	%r15, %rcx
	callq	fwrite
	movq	32(%r14), %rbx
	jmp	.LBB22_2
	.align	16, 0x90
.LBB22_1:                               # %for.body
                                        #   in Loop: Header=BB22_2 Depth=1
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	dump_edge_info
	movq	(%rbx), %rbx
.LBB22_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB22_1
# BB#3:                                 # %for.end
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	putc
	movabsq	$.L.str6, %rdi
	movq	$27, %rsi
	movq	$1, %rdx
	movq	%r15, %rcx
	callq	fwrite
	movq	64(%r14), %rdi
	movq	%r15, %rsi
	callq	dump_regset
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	putc
	movq	8(%r14), %rax
	movq	(%r14), %rbx
	movq	24(%rax), %r12
	jmp	.LBB22_5
	.align	16, 0x90
.LBB22_4:                               # %for.body8
                                        #   in Loop: Header=BB22_5 Depth=1
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	print_rtl_single
	movq	24(%rbx), %rbx
.LBB22_5:                               # %for.body8
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r12, %rbx
	jne	.LBB22_4
# BB#6:                                 # %for.end14
	movabsq	$.L.str7, %rdi
	movq	$25, %rsi
	movq	$1, %rdx
	movq	%r15, %rcx
	callq	fwrite
	movq	72(%r14), %rdi
	movq	%r15, %rsi
	callq	dump_regset
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	putc
	movabsq	$.L.str8, %rdi
	movq	$15, %rsi
	movq	$1, %rdx
	movq	%r15, %rcx
	callq	fwrite
	movq	40(%r14), %rbx
	jmp	.LBB22_8
	.align	16, 0x90
.LBB22_7:                               # %for.body20
                                        #   in Loop: Header=BB22_8 Depth=1
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	dump_edge_info
	movq	8(%rbx), %rbx
.LBB22_8:                               # %for.body20
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB22_7
# BB#9:                                 # %for.end22
	movq	$10, %rdi
	movq	%r15, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	putc  # TAILCALL
.Ltmp292:
	.size	dump_bb, .Ltmp292-dump_bb
	.cfi_endproc

	.globl	debug_bb
	.align	16, 0x90
	.type	debug_bb,@function
debug_bb:                               # @debug_bb
	.cfi_startproc
# BB#0:                                 # %entry
	movq	stderr(%rip), %rsi
	jmp	dump_bb  # TAILCALL
.Ltmp293:
	.size	debug_bb, .Ltmp293-debug_bb
	.cfi_endproc

	.globl	debug_bb_n
	.align	16, 0x90
	.type	debug_bb_n,@function
debug_bb_n:                             # @debug_bb_n
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	basic_block_info(%rip), %rax
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	32(%rax,%rdi,8), %rdi
	movq	stderr(%rip), %rsi
	jmp	dump_bb  # TAILCALL
.Ltmp294:
	.size	debug_bb_n, .Ltmp294-debug_bb_n
	.cfi_endproc

	.globl	print_rtl_with_bb
	.align	16, 0x90
	.type	print_rtl_with_bb,@function
print_rtl_with_bb:                      # @print_rtl_with_bb
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
	subq	$16, %rsp
.Ltmp306:
	.cfi_def_cfa_offset 64
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
	movq	%rsi, %r13
	movq	%rdi, %r14
	testq	%r13, %r13
	je	.LBB25_1
# BB#2:                                 # %if.else
	callq	get_max_uid
	movq	%rax, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$8, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$8, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	$4, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movslq	n_basic_blocks(%rip), %rdx
	movq	%rax, %r12
	testq	%rdx, %rdx
	jle	.LBB25_10
# BB#3:                                 # %for.body.lr.ph
	movq	basic_block_info(%rip), %r8
	movq	(%rsp), %rax            # 8-byte Reload
	.align	16, 0x90
.LBB25_5:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB25_6 Depth 2
	movq	24(%r8,%rdx,8), %rdi
	movq	(%rdi), %rcx
	movslq	8(%rcx), %rsi
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rdi, (%rbx,%rsi,8)
	movq	8(%rdi), %rsi
	movslq	8(%rsi), %rbx
	testq	%rcx, %rcx
	movq	%rdi, (%rax,%rbx,8)
	je	.LBB25_4
	.align	16, 0x90
.LBB25_6:                               # %for.body23
                                        #   Parent Loop BB25_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	8(%rcx), %rdi
	movl	(%r12,%rdi,4), %ebx
	testq	%rbx, %rbx
	movq	$1, %rbx
	je	.LBB25_8
# BB#7:                                 # %select.mid
                                        #   in Loop: Header=BB25_6 Depth=2
	movq	$2, %rbx
.LBB25_8:                               # %select.end
                                        #   in Loop: Header=BB25_6 Depth=2
	movl	%ebx, (%r12,%rdi,4)
	cmpq	%rsi, %rcx
	je	.LBB25_4
# BB#9:                                 # %for.inc
                                        #   in Loop: Header=BB25_6 Depth=2
	movq	24(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB25_6
.LBB25_4:                               # %for.cond.loopexit
                                        #   in Loop: Header=BB25_5 Depth=1
	decq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jg	.LBB25_5
	jmp	.LBB25_10
.LBB25_16:                              # %if.then76
                                        #   in Loop: Header=BB25_10 Depth=1
	movabsq	$.L.str11, %rdi
	jmp	.LBB25_18
	.align	16, 0x90
.LBB25_10:                              # %for.body49
                                        # =>This Inner Loop Header: Depth=1
	movslq	8(%r13), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	(%rcx,%rax,8), %rbx
	testq	%rbx, %rbx
	je	.LBB25_12
# BB#11:                                # %if.then58
                                        #   in Loop: Header=BB25_10 Depth=1
	movl	88(%rbx), %edx
	movabsq	$.L.str10, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movq	64(%rbx), %rdi
	movq	%r14, %rsi
	callq	dump_regset
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	putc
	movl	8(%r13), %eax
.LBB25_12:                              # %if.end61
                                        #   in Loop: Header=BB25_10 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	(%r12,%rax,4), %eax
	cmpq	$2, %rax
	jne	.LBB25_13
# BB#17:                                # %if.then86
                                        #   in Loop: Header=BB25_10 Depth=1
	movabsq	$.L.str12, %rdi
.LBB25_18:                              # %if.end89
                                        #   in Loop: Header=BB25_10 Depth=1
	movq	$36, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	jmp	.LBB25_19
	.align	16, 0x90
.LBB25_13:                              # %if.end61
                                        #   in Loop: Header=BB25_10 Depth=1
	testq	%rax, %rax
	jne	.LBB25_19
# BB#14:                                # %land.lhs.true
                                        #   in Loop: Header=BB25_10 Depth=1
	movzbl	(%r13), %eax
	cmpq	$35, %rax
	je	.LBB25_19
# BB#15:                                # %land.lhs.true
                                        #   in Loop: Header=BB25_10 Depth=1
	cmpq	$37, %rax
	jne	.LBB25_16
	.align	16, 0x90
.LBB25_19:                              # %if.end89
                                        #   in Loop: Header=BB25_10 Depth=1
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	print_rtl_single
	movslq	8(%r13), %rcx
	movq	(%rsp), %rdx            # 8-byte Reload
	movq	(%rdx,%rcx,8), %rbx
	movq	%rax, %r15
	testq	%rbx, %rbx
	je	.LBB25_21
# BB#20:                                # %if.then98
                                        #   in Loop: Header=BB25_10 Depth=1
	movl	88(%rbx), %edx
	movabsq	$.L.str13, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movq	72(%rbx), %rdi
	movq	%r14, %rsi
	callq	dump_regset
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	putc
.LBB25_21:                              # %if.end102
                                        #   in Loop: Header=BB25_10 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	je	.LBB25_23
# BB#22:                                # %if.then103
                                        #   in Loop: Header=BB25_10 Depth=1
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	putc
.LBB25_23:                              # %for.inc106
                                        #   in Loop: Header=BB25_10 Depth=1
	movq	24(%r13), %r13
	testq	%r13, %r13
	jne	.LBB25_10
# BB#24:                                # %for.end110
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	free
	movq	(%rsp), %rdi            # 8-byte Reload
	callq	free
	movq	%r12, %rdi
	callq	free
	jmp	.LBB25_25
.LBB25_1:                               # %if.then
	movabsq	$.L.str9, %rdi
	movq	$6, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
.LBB25_25:                              # %if.end111
	movq	cfun(%rip), %rax
	cmpq	$0, 416(%rax)
	je	.LBB25_29
# BB#26:                                # %if.then114
	movabsq	$.L.str14, %rdi
	movq	$35, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movq	cfun(%rip), %rax
	movq	416(%rax), %rbx
	jmp	.LBB25_28
	.align	16, 0x90
.LBB25_27:                              # %for.body120
                                        #   in Loop: Header=BB25_28 Depth=1
	movq	8(%rbx), %rsi
	movq	%r14, %rdi
	callq	print_rtl_single
	movq	16(%rbx), %rbx
.LBB25_28:                              # %for.body120
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB25_27
.LBB25_29:                              # %if.end130
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp312:
	.size	print_rtl_with_bb, .Ltmp312-print_rtl_with_bb
	.cfi_endproc

	.globl	update_br_prob_note
	.align	16, 0x90
	.type	update_br_prob_note,@function
update_br_prob_note:                    # @update_br_prob_note
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp316:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp317:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp318:
	.cfi_def_cfa_offset 32
.Ltmp319:
	.cfi_offset %rbx, -24
.Ltmp320:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	8(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$33, %rax
	jne	.LBB26_8
# BB#1:                                 # %if.end
	movq	$16, %rsi
	xorq	%rdx, %rdx
	callq	find_reg_note
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB26_8
# BB#2:                                 # %lor.lhs.false
	movq	40(%rbx), %rax
	movq	8(%r14), %rdx
	movl	48(%rax), %ecx
	movq	8(%rdx), %rdx
	andq	$1, %rcx
	movq	%rax, %rsi
	je	.LBB26_4
# BB#3:                                 # %cond.true
	movq	8(%rax), %rsi
.LBB26_4:                               # %cond.end
	movslq	52(%rsi), %rsi
	cmpq	%rsi, %rdx
	je	.LBB26_8
# BB#5:                                 # %if.end10
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB26_7
# BB#6:                                 # %cond.true15
	movq	8(%rax), %rax
.LBB26_7:                               # %cond.end20
	movslq	52(%rax), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, 8(%r14)
.LBB26_8:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp321:
	.size	update_br_prob_note, .Ltmp321-update_br_prob_note
	.cfi_endproc

	.globl	verify_flow_info
	.align	16, 0x90
	.type	verify_flow_info,@function
verify_flow_info:                       # @verify_flow_info
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp328:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp329:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp330:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp331:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp332:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp333:
	.cfi_def_cfa_offset 112
.Ltmp334:
	.cfi_offset %rbx, -48
.Ltmp335:
	.cfi_offset %r12, -40
.Ltmp336:
	.cfi_offset %r13, -32
.Ltmp337:
	.cfi_offset %r14, -24
.Ltmp338:
	.cfi_offset %r15, -16
	callq	get_max_uid
	movq	%rax, %r14
	callq	get_insns
	movq	%rax, 16(%rsp)          # 8-byte Spill
	callq	get_last_insn
	movq	%rax, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	$8, %rsi
	movq	%r14, %rdi
	callq	xcalloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, 32(%rsp)          # 8-byte Spill
	addq	$2, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$8, %rsi
	callq	xcalloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, 24(%rsp)          # 8-byte Spill
	addq	$2, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$8, %rsi
	callq	xcalloc
	movslq	n_basic_blocks(%rip), %rdi
	movq	%rax, %r14
	xorq	%rdx, %rdx
	testq	%rdi, %rdi
	jle	.LBB27_37
# BB#1:                                 # %for.body.lr.ph
	xorq	%rdx, %rdx
	movabsq	$0, %r12
	movabsq	$1, %rsi
	jmp	.LBB27_2
.LBB27_19:                              #   in Loop: Header=BB27_2 Depth=1
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movabsq	$1, %rsi
	movq	56(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB27_23
	.align	16, 0x90
.LBB27_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB27_5 Depth 2
                                        #     Child Loop BB27_16 Depth 2
	movq	%rdx, %r8
	movq	basic_block_info(%rip), %rax
	movq	24(%rax,%rdi,8), %r15
	movq	(%r15), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	8(%r15), %rax
	decq	%rdi
	testq	%rbx, %rbx
	movq	%r12, %rcx
	je	.LBB27_4
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB27_2 Depth=1
	movq	%rsi, %rcx
.LBB27_4:                               # %for.body
                                        #   in Loop: Header=BB27_2 Depth=1
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	jmp	.LBB27_5
	.align	16, 0x90
.LBB27_10:                              # %for.inc
                                        #   in Loop: Header=BB27_5 Depth=2
	movq	%rsi, %rcx
.LBB27_5:                               # %for.body
                                        #   Parent Loop BB27_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r12, %rdx
	jne	.LBB27_7
# BB#6:                                 # %for.body
                                        #   in Loop: Header=BB27_5 Depth=2
	movq	%rsi, %rdx
.LBB27_7:                               # %for.body
                                        #   in Loop: Header=BB27_5 Depth=2
	cmpq	%rax, %rbx
	je	.LBB27_11
# BB#8:                                 # %for.body
                                        #   in Loop: Header=BB27_5 Depth=2
	testq	%rdx, %rdx
	jne	.LBB27_11
# BB#9:                                 # %for.inc
                                        #   in Loop: Header=BB27_5 Depth=2
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	movq	%r12, %rcx
	je	.LBB27_5
	jmp	.LBB27_10
	.align	16, 0x90
.LBB27_11:                              # %for.end
                                        #   in Loop: Header=BB27_2 Depth=1
	testq	%rcx, %rcx
	je	.LBB27_12
# BB#13:                                # %for.cond24.preheader
                                        #   in Loop: Header=BB27_2 Depth=1
	testq	%rbx, %rbx
	movq	%r8, %rdx
	jne	.LBB27_15
# BB#14:                                # %for.cond24.preheader.if.then62.loopexit_crit_edge
                                        #   in Loop: Header=BB27_2 Depth=1
	addq	$88, %r15
	jmp	.LBB27_22
	.align	16, 0x90
.LBB27_12:                              # %for.cond24.preheader.thread
                                        #   in Loop: Header=BB27_2 Depth=1
	movl	88(%r15), %edx
	movl	8(%rax), %esi
	xorq	%rax, %rax
	movabsq	$.L.str15, %rdi
	callq	error
	addq	$88, %r15
	jmp	.LBB27_22
	.align	16, 0x90
.LBB27_15:                              # %for.body27.lr.ph
                                        #   in Loop: Header=BB27_2 Depth=1
	leaq	88(%r15), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	32(%rsp), %rdi          # 8-byte Reload
	.align	16, 0x90
.LBB27_16:                              # %for.body27
                                        #   Parent Loop BB27_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	8(%rbx), %rsi
	movq	(%rdi,%rsi,8), %rax
	testq	%rax, %rax
	je	.LBB27_18
# BB#17:                                # %if.then35
                                        #   in Loop: Header=BB27_16 Depth=2
	movl	88(%rax), %ecx
	movq	48(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %edx
	xorq	%rax, %rax
	movq	%r14, %r13
	movq	%rdi, %r14
	movabsq	$.L.str16, %rdi
	callq	error
	movq	%r14, %rdi
	movq	%r13, %r14
	movl	8(%rbx), %esi
	movq	$1, %rdx
.LBB27_18:                              # %if.end46
                                        #   in Loop: Header=BB27_16 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%r15, (%rdi,%rsi,8)
	movq	56(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %rbx
	je	.LBB27_19
# BB#20:                                # %for.inc56
                                        #   in Loop: Header=BB27_16 Depth=2
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB27_16
# BB#21:                                #   in Loop: Header=BB27_2 Depth=1
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movq	48(%rsp), %r15          # 8-byte Reload
	.align	16, 0x90
.LBB27_22:                              # %if.then62
                                        #   in Loop: Header=BB27_2 Depth=1
	movl	(%r15), %edx
	movq	56(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %esi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movabsq	$.L.str17, %rdi
	callq	error
	movq	$1, %rdx
	movabsq	$1, %rsi
.LBB27_23:                              # %for.cond.backedge
                                        #   in Loop: Header=BB27_2 Depth=1
	movq	$32, %rcx
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB27_2
# BB#24:                                # %for.end69
	movslq	n_basic_blocks(%rip), %r12
	testq	%r12, %r12
	jle	.LBB27_37
# BB#25:                                # %for.body74.lr.ph
	movabsq	$4294967216, %rbx       # imm = 0xFFFFFFB0
	.align	16, 0x90
.LBB27_26:                              # %for.body74
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB27_28 Depth 2
                                        #       Child Loop BB27_49 Depth 3
                                        #     Child Loop BB27_56 Depth 2
                                        #     Child Loop BB27_62 Depth 2
                                        #     Child Loop BB27_67 Depth 2
                                        #     Child Loop BB27_90 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	24(%rax,%r12,8), %r15
	movq	40(%r15), %r13
	decq	%r12
	movq	%r12, 40(%rsp)          # 8-byte Spill
	testq	%r13, %r13
	je	.LBB27_55
# BB#27:                                # %for.body82.lr.ph
                                        #   in Loop: Header=BB27_26 Depth=1
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	24(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB27_28
.LBB27_52:                              # %if.then161
                                        #   in Loop: Header=BB27_28 Depth=2
	movl	88(%r15), %esi
	xorq	%rax, %rax
	movabsq	$.L.str22, %rdi
	callq	error
	movq	$13, %rsi
	movq	$1, %rdx
	movq	$1, 56(%rsp)            # 8-byte Folded Spill
	movabsq	$.L.str23, %rdi
	movq	stderr(%rip), %r12
	movq	%r12, %rcx
	callq	fwrite
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	dump_edge_info
	movq	$12, %rsi
	movq	$1, %rdx
	movabsq	$.L.str24, %rdi
	movq	%r12, %rcx
	callq	fwrite
	movq	$1, %rdx
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	dump_edge_info
	movq	$10, %rdi
	movq	%r12, %rsi
	callq	fputc
	jmp	.LBB27_53
	.align	16, 0x90
.LBB27_28:                              # %for.body82
                                        #   Parent Loop BB27_26 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB27_49 Depth 3
	movq	24(%r13), %rax
	movl	88(%rax), %edx
	leaq	2(%rdx), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	(%rbx,%rsi,8), %rcx
	cmpq	%r15, %rcx
	jne	.LBB27_30
# BB#29:                                # %if.then89
                                        #   in Loop: Header=BB27_28 Depth=2
	movq	16(%r13), %rax
	movl	88(%rax), %esi
	xorq	%rax, %rax
	movabsq	$.L.str18, %rdi
	callq	error
	movq	24(%r13), %rax
	movl	88(%rax), %edx
	movq	$1, 56(%rsp)            # 8-byte Folded Spill
.LBB27_30:                              # %if.end93
                                        #   in Loop: Header=BB27_28 Depth=2
	leaq	2(%rdx), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%r15, (%rbx,%rsi,8)
	movzbl	48(%r13), %ecx
	andq	$1, %rcx
	je	.LBB27_32
# BB#31:                                # %if.end93
                                        #   in Loop: Header=BB27_28 Depth=2
	movq	$1, 48(%rsp)            # 8-byte Folded Spill
.LBB27_32:                              # %if.end93
                                        #   in Loop: Header=BB27_28 Depth=2
	testq	%rcx, %rcx
	je	.LBB27_51
# BB#33:                                # %land.lhs.true
                                        #   in Loop: Header=BB27_28 Depth=2
	movq	16(%r13), %r8
	movabsq	$entry_exit_blocks, %rcx
	cmpq	%rcx, %r8
	je	.LBB27_51
# BB#34:                                # %land.lhs.true
                                        #   in Loop: Header=BB27_28 Depth=2
	movabsq	$entry_exit_blocks+112, %rcx
	cmpq	%rcx, %rax
	je	.LBB27_51
# BB#35:                                # %if.then112
                                        #   in Loop: Header=BB27_28 Depth=2
	movq	%r14, %r12
	movq	%rbx, %r14
	movl	88(%r8), %esi
	leaq	1(%rsi), %rdi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%rbx, %rcx
	andq	%rcx, %rdi
	movq	%rdx, %rbx
	andq	%rcx, %rbx
	cmpq	%rbx, %rdi
	jne	.LBB27_36
# BB#48:                                # %if.else
                                        #   in Loop: Header=BB27_28 Depth=2
	movq	8(%r8), %rbx
	movq	(%rax), %rax
	.align	16, 0x90
.LBB27_49:                              # %for.cond130
                                        #   Parent Loop BB27_26 Depth=1
                                        #     Parent Loop BB27_28 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	24(%rbx), %rbx
	cmpq	%rax, %rbx
	je	.LBB27_50
# BB#71:                                # %for.body135
                                        #   in Loop: Header=BB27_49 Depth=3
	movzbl	(%rbx), %ecx
	cmpq	$35, %rcx
	je	.LBB27_73
# BB#72:                                # %lor.lhs.false
                                        #   in Loop: Header=BB27_49 Depth=3
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB27_49
	jmp	.LBB27_73
.LBB27_36:                              # %if.then120
                                        #   in Loop: Header=BB27_28 Depth=2
	xorq	%rax, %rax
	movabsq	$.L.str19, %rdi
	callq	error
	movq	$1, 56(%rsp)            # 8-byte Folded Spill
.LBB27_50:                              #   in Loop: Header=BB27_28 Depth=2
	movq	%r14, %rbx
	movq	%r12, %r14
.LBB27_51:                              # %if.end157
                                        #   in Loop: Header=BB27_28 Depth=2
	movq	16(%r13), %rax
	cmpq	%r15, %rax
	jne	.LBB27_52
.LBB27_53:                              # %if.end166
                                        #   in Loop: Header=BB27_28 Depth=2
	movq	24(%r13), %rax
	movl	88(%rax), %eax
	addq	$2, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%r13, (%r14,%rax,8)
	movq	8(%r13), %r13
	testq	%r13, %r13
	jne	.LBB27_28
# BB#54:                                # %for.end174
                                        #   in Loop: Header=BB27_26 Depth=1
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	48(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	movq	56(%rsp), %rdx          # 8-byte Reload
	movabsq	$4294967216, %rbx       # imm = 0xFFFFFFB0
	jne	.LBB27_61
.LBB27_55:                              # %if.then176
                                        #   in Loop: Header=BB27_26 Depth=1
	movq	8(%r15), %rax
	jmp	.LBB27_56
	.align	16, 0x90
.LBB27_74:                              # %for.inc201
                                        #   in Loop: Header=BB27_56 Depth=2
	movq	24(%rax), %rax
.LBB27_56:                              # %if.then176
                                        #   Parent Loop BB27_26 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rax, %rax
	je	.LBB27_60
# BB#57:                                # %lor.rhs
                                        #   in Loop: Header=BB27_56 Depth=2
	movzbl	(%rax), %ecx
	cmpq	$35, %rcx
	je	.LBB27_61
# BB#58:                                # %lor.rhs
                                        #   in Loop: Header=BB27_56 Depth=2
	cmpq	$37, %rcx
	jne	.LBB27_74
# BB#59:                                # %land.lhs.true192
                                        #   in Loop: Header=BB27_56 Depth=2
	movl	40(%rax), %ecx
	cmpq	%rbx, %rcx
	jne	.LBB27_74
	.align	16, 0x90
.LBB27_60:                              # %if.then198
                                        #   in Loop: Header=BB27_26 Depth=1
	movl	88(%r15), %esi
	xorq	%rax, %rax
	movabsq	$.L.str26, %rdi
	callq	error
	movq	$1, %rdx
.LBB27_61:                              # %if.end206
                                        #   in Loop: Header=BB27_26 Depth=1
	movq	%rdx, %r12
	movq	32(%r15), %rbx
	jmp	.LBB27_62
	.align	16, 0x90
.LBB27_65:                              # %if.end218
                                        #   in Loop: Header=BB27_62 Depth=2
	movl	88(%rax), %eax
	addq	$2, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	subq	%rbx, (%r14,%rax,8)
	movq	(%rbx), %rbx
.LBB27_62:                              # %if.end206
                                        #   Parent Loop BB27_26 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB27_66
# BB#63:                                # %for.body209
                                        #   in Loop: Header=BB27_62 Depth=2
	movq	24(%rbx), %rax
	cmpq	%r15, %rax
	movq	%r15, %rax
	je	.LBB27_65
# BB#64:                                # %if.then213
                                        #   in Loop: Header=BB27_62 Depth=2
	movl	88(%r15), %esi
	xorq	%rax, %rax
	movabsq	$.L.str27, %rdi
	callq	error
	movq	$13, %rsi
	movq	$1, %rdx
	movq	$1, %r12
	movabsq	$.L.str23, %rdi
	movq	stderr(%rip), %r13
	movq	%r13, %rcx
	callq	fwrite
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	dump_edge_info
	movq	$12, %rsi
	movq	$1, %rdx
	movabsq	$.L.str24, %rdi
	movq	%r13, %rcx
	callq	fwrite
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	dump_edge_info
	movq	$10, %rdi
	movq	%r13, %rsi
	callq	fputc
	movq	24(%rbx), %rax
	jmp	.LBB27_65
	.align	16, 0x90
.LBB27_66:                              # %for.end226
                                        #   in Loop: Header=BB27_26 Depth=1
	movq	8(%r15), %rax
	movq	(%r15), %r13
	movq	24(%rax), %rcx
	cmpq	%rcx, %r13
	je	.LBB27_79
	.align	16, 0x90
.LBB27_67:                              # %for.body235
                                        #   Parent Loop BB27_26 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	basic_block_for_insn(%rip), %rax
	testq	%rax, %rax
	je	.LBB27_77
# BB#68:                                # %land.lhs.true237
                                        #   in Loop: Header=BB27_67 Depth=2
	movslq	8(%r13), %rcx
	movq	32(%rax,%rcx,8), %rax
	cmpq	%r15, %rax
	je	.LBB27_77
# BB#69:                                # %if.then247
                                        #   in Loop: Header=BB27_67 Depth=2
	movq	%r13, %rdi
	callq	debug_rtx
	movslq	8(%r13), %rsi
	movq	basic_block_for_insn(%rip), %rax
	movq	32(%rax,%rsi,8), %rax
	movl	88(%r15), %edx
	testq	%rax, %rax
	jne	.LBB27_75
# BB#70:                                # %if.then256
                                        #   in Loop: Header=BB27_67 Depth=2
	xorq	%rax, %rax
	movabsq	$.L.str28, %rdi
	callq	error
	jmp	.LBB27_76
.LBB27_75:                              # %if.else261
                                        #   in Loop: Header=BB27_67 Depth=2
	movl	88(%rax), %ecx
	xorq	%rax, %rax
	movabsq	$.L.str29, %rdi
	callq	error
.LBB27_76:                              # %for.inc276
                                        #   in Loop: Header=BB27_67 Depth=2
	movq	$1, %r12
.LBB27_77:                              # %for.inc276
                                        #   in Loop: Header=BB27_67 Depth=2
	movq	8(%r15), %rax
	movq	24(%r13), %r13
	movq	24(%rax), %rcx
	cmpq	%rcx, %r13
	jne	.LBB27_67
# BB#78:                                # %for.cond228.for.end280_crit_edge
                                        #   in Loop: Header=BB27_26 Depth=1
	movq	(%r15), %r13
.LBB27_79:                              # %for.end280
                                        #   in Loop: Header=BB27_26 Depth=1
	movl	(%r13), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$36, %rdx
	jne	.LBB27_80
# BB#81:                                # %if.then286
                                        #   in Loop: Header=BB27_26 Depth=1
	cmpq	%r13, %rax
	movq	%r12, %rdx
	movq	40(%rsp), %r12          # 8-byte Reload
	jne	.LBB27_83
# BB#82:                                # %if.then290
                                        #   in Loop: Header=BB27_26 Depth=1
	movl	88(%r15), %esi
	xorq	%rax, %rax
	movabsq	$.L.str30, %rdi
	callq	error
	movq	$1, %rdx
.LBB27_83:                              # %if.end292
                                        #   in Loop: Header=BB27_26 Depth=1
	movq	24(%r13), %r13
	movl	(%r13), %ecx
	jmp	.LBB27_84
	.align	16, 0x90
.LBB27_80:                              #   in Loop: Header=BB27_26 Depth=1
	movq	%r12, %rdx
	movq	40(%rsp), %r12          # 8-byte Reload
.LBB27_84:                              # %if.end296
                                        #   in Loop: Header=BB27_26 Depth=1
	movabsq	$4294967216, %rbx       # imm = 0xFFFFFFB0
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$37, %rcx
	jne	.LBB27_87
# BB#85:                                # %land.lhs.true301
                                        #   in Loop: Header=BB27_26 Depth=1
	movl	40(%r13), %eax
	cmpq	%rbx, %rax
	jne	.LBB27_87
# BB#86:                                # %lor.lhs.false307
                                        #   in Loop: Header=BB27_26 Depth=1
	movq	32(%r13), %rax
	cmpq	%r15, %rax
	je	.LBB27_88
	.align	16, 0x90
.LBB27_87:                              # %if.then313
                                        #   in Loop: Header=BB27_26 Depth=1
	movl	88(%r15), %esi
	xorq	%rax, %rax
	movabsq	$.L.str30, %rdi
	callq	error
	movq	$1, %rdx
.LBB27_88:                              # %if.end315
                                        #   in Loop: Header=BB27_26 Depth=1
	movq	8(%r15), %rax
	cmpq	%r13, %rax
	je	.LBB27_89
	.align	16, 0x90
.LBB27_90:                              # %for.cond324
                                        #   Parent Loop BB27_26 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%r13), %r13
	testq	%r13, %r13
	je	.LBB27_89
# BB#91:                                # %for.body326
                                        #   in Loop: Header=BB27_90 Depth=2
	movzbl	(%r13), %ecx
	cmpq	$37, %rcx
	jne	.LBB27_94
# BB#92:                                # %land.lhs.true331
                                        #   in Loop: Header=BB27_90 Depth=2
	movl	40(%r13), %ecx
	cmpq	%rbx, %rcx
	jne	.LBB27_94
# BB#93:                                # %if.then337
                                        #   in Loop: Header=BB27_90 Depth=2
	movl	88(%r15), %edx
	movl	8(%r13), %esi
	xorq	%rax, %rax
	movabsq	$.L.str31, %rdi
	callq	error
	movq	8(%r15), %rax
	movq	$1, %rdx
.LBB27_94:                              # %if.end342
                                        #   in Loop: Header=BB27_90 Depth=2
	cmpq	%rax, %r13
	je	.LBB27_89
# BB#95:                                # %if.end347
                                        #   in Loop: Header=BB27_90 Depth=2
	movzbl	(%r13), %ecx
	cmpq	$36, %rcx
	ja	.LBB27_90
# BB#96:                                # %if.end347
                                        #   in Loop: Header=BB27_90 Depth=2
	movq	%rdx, %rsi
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$111669149696, %rcx     # imm = 0x1A00000000
	testq	%rcx, %rdx
	movq	%rsi, %rdx
	je	.LBB27_90
	jmp	.LBB27_97
	.align	16, 0x90
.LBB27_89:                              # %for.cond71.backedge
                                        #   in Loop: Header=BB27_26 Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB27_26
.LBB27_37:                              # %for.cond375.preheader
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movq	entry_exit_blocks+40(%rip), %rax
	jmp	.LBB27_38
	.align	16, 0x90
.LBB27_98:                              # %for.body377
                                        #   in Loop: Header=BB27_38 Depth=1
	movq	24(%rax), %rcx
	movl	88(%rcx), %edx
	addq	$2, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	%rax, (%r14,%rdx,8)
	movq	8(%rax), %rax
.LBB27_38:                              # %for.cond375.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB27_98
# BB#39:                                # %for.cond387.preheader
	movq	entry_exit_blocks+144(%rip), %rax
	jmp	.LBB27_40
	.align	16, 0x90
.LBB27_99:                              # %for.body389
                                        #   in Loop: Header=BB27_40 Depth=1
	movq	24(%rax), %rcx
	movl	88(%rcx), %edx
	addq	$2, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	subq	%rax, (%r14,%rdx,8)
	movq	(%rax), %rax
.LBB27_40:                              # %for.cond387.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB27_99
# BB#41:                                # %for.cond399.preheader
	movq	%r14, %r12
	movslq	n_basic_blocks(%rip), %rdx
	cmpq	$-2, %rdx
	movq	16(%rsp), %r13          # 8-byte Reload
	jle	.LBB27_46
# BB#42:
	movq	$-1, %rbx
	movabsq	$.L.str34, %r14
	.align	16, 0x90
.LBB27_43:                              # %for.body402
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, 8(%r12,%rbx,8)
	je	.LBB27_45
# BB#44:                                # %if.then407
                                        #   in Loop: Header=BB27_43 Depth=1
	leaq	-1(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	error
	movl	n_basic_blocks(%rip), %edx
	movq	$1, 56(%rsp)            # 8-byte Folded Spill
.LBB27_45:                              # %for.inc409
                                        #   in Loop: Header=BB27_43 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rsi, %rax
	jl	.LBB27_43
.LBB27_46:                              # %for.cond411.preheader
	xorq	%r8, %r8
	testq	%r13, %r13
	movq	32(%rsp), %rdi          # 8-byte Reload
	je	.LBB27_47
# BB#100:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%r12, %rbx
	.align	16, 0x90
.LBB27_101:                             # %for.body413
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r13), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$37, %rcx
	jne	.LBB27_107
# BB#102:                               # %land.lhs.true418
                                        #   in Loop: Header=BB27_101 Depth=1
	movl	40(%r13), %edx
	movabsq	$4294967216, %rsi       # imm = 0xFFFFFFB0
	cmpq	%rsi, %rdx
	jne	.LBB27_107
# BB#103:                               # %if.then424
                                        #   in Loop: Header=BB27_101 Depth=1
	movq	32(%r13), %rdx
	movl	88(%rdx), %edx
	incq	%r15
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %r15
	cmpq	%r15, %rdx
	jne	.LBB27_104
# BB#106:                               # %if.end435
                                        #   in Loop: Header=BB27_101 Depth=1
	incq	48(%rsp)                # 8-byte Folded Spill
	movq	%rdx, %r15
.LBB27_107:                             # %if.end437
                                        #   in Loop: Header=BB27_101 Depth=1
	movq	%rbx, %r12
	movslq	8(%r13), %rdx
	cmpq	$0, (%rdi,%rdx,8)
	jne	.LBB27_116
# BB#108:                               # %if.then444
                                        #   in Loop: Header=BB27_101 Depth=1
	cmpq	$35, %rcx
	je	.LBB27_116
# BB#109:                               # %if.then444
                                        #   in Loop: Header=BB27_101 Depth=1
	cmpq	$37, %rcx
	je	.LBB27_116
# BB#110:                               # %if.then444
                                        #   in Loop: Header=BB27_101 Depth=1
	cmpq	$36, %rcx
	jne	.LBB27_123
# BB#111:                               # %sw.bb447
                                        #   in Loop: Header=BB27_101 Depth=1
	movq	24(%r13), %rcx
	testq	%rcx, %rcx
	je	.LBB27_116
# BB#112:                               # %land.lhs.true452
                                        #   in Loop: Header=BB27_101 Depth=1
	movzbl	(%rcx), %edx
	cmpq	$33, %rdx
	jne	.LBB27_116
# BB#113:                               # %land.lhs.true460
                                        #   in Loop: Header=BB27_101 Depth=1
	movq	32(%rcx), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	je	.LBB27_115
# BB#114:                               # %land.lhs.true460
                                        #   in Loop: Header=BB27_101 Depth=1
	movq	%r13, %rcx
.LBB27_115:                             # %land.lhs.true460
                                        #   in Loop: Header=BB27_101 Depth=1
	movl	(%rcx), %eax
	movq	%rcx, %r13
	.align	16, 0x90
.LBB27_116:                             # %if.end487
                                        #   in Loop: Header=BB27_101 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %ecx
	cmpq	$105, %rcx
	jne	.LBB27_125
# BB#117:                               # %if.end487
                                        #   in Loop: Header=BB27_101 Depth=1
	cmpq	$33, %rax
	jne	.LBB27_125
# BB#118:                               # %land.lhs.true500
                                        #   in Loop: Header=BB27_101 Depth=1
	movq	%rdi, %r14
	movq	%r13, %rdi
	callq	returnjump_p
	movq	%r14, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB27_125
# BB#119:                               # %land.lhs.true503
                                        #   in Loop: Header=BB27_101 Depth=1
	movq	%rdi, %rbx
	movq	%r13, %rdi
	callq	condjump_p
	movq	%rbx, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB27_125
# BB#120:                               # %land.lhs.true506
                                        #   in Loop: Header=BB27_101 Depth=1
	movq	24(%r13), %rax
	testq	%rax, %rax
	je	.LBB27_122
# BB#121:                               # %land.lhs.true511
                                        #   in Loop: Header=BB27_101 Depth=1
	movzbl	(%rax), %eax
	cmpq	$35, %rax
	jne	.LBB27_122
	.align	16, 0x90
.LBB27_125:                             # %for.inc521
                                        #   in Loop: Header=BB27_101 Depth=1
	movq	24(%r13), %r13
	testq	%r13, %r13
	movq	%r12, %rbx
	jne	.LBB27_101
# BB#126:                               # %for.cond411.for.end525_crit_edge
	movl	n_basic_blocks(%rip), %edx
	movq	48(%rsp), %r8           # 8-byte Reload
	jmp	.LBB27_127
.LBB27_47:
	movq	%r12, %rbx
.LBB27_127:                             # %for.end525
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rdx, %rcx
	andq	%rax, %rcx
	movq	%r8, %rsi
	andq	%rax, %rsi
	cmpq	%rcx, %rsi
	jne	.LBB27_131
# BB#128:                               # %if.end529
	movq	56(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	jne	.LBB27_129
# BB#130:                               # %if.end532
	callq	free
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	%rbx, %rdi
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.LBB27_73:                              # %if.then145
	movabsq	$.L.str20, %rdi
	xorq	%rax, %rax
	callq	error
	movabsq	$.L.str21, %rdi
	movabsq	$.L.str, %rdx
	movabsq	$.L__FUNCTION__.verify_flow_info, %r8
	movq	$1717, %rcx             # imm = 0x6B5
	movq	%rbx, %rsi
	callq	_fatal_insn
.LBB27_97:                              # %if.then362
	movl	88(%r15), %ecx
	movabsq	$.L.str32, %rdx
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	callq	error
	movabsq	$.L.str33, %rax
	movabsq	$.L.str, %rdx
	movabsq	$.L__FUNCTION__.verify_flow_info, %rbx
	movq	$1829, %rcx             # imm = 0x725
	jmp	.LBB27_124
.LBB27_104:                             # %if.then434
	movabsq	$.L.str35, %rcx
	jmp	.LBB27_105
.LBB27_123:                             # %sw.default
	movabsq	$.L.str36, %rax
	movabsq	$.L.str, %rdx
	movabsq	$.L__FUNCTION__.verify_flow_info, %rbx
	movq	$1887, %rcx             # imm = 0x75F
	jmp	.LBB27_124
.LBB27_122:                             # %if.then519
	movabsq	$.L.str37, %rax
	movabsq	$.L.str, %rdx
	movabsq	$.L__FUNCTION__.verify_flow_info, %rbx
	movq	$1895, %rcx             # imm = 0x767
.LBB27_124:                             # %sw.default
	movq	%rax, %rdi
	movq	%r13, %rsi
	movq	%rbx, %r8
	callq	_fatal_insn
.LBB27_131:                             # %if.then528
	movabsq	$.L.str38, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%r8, %rsi
	callq	internal_error
.LBB27_129:                             # %if.then531
	movabsq	$.L.str39, %rcx
.LBB27_105:                             # %if.then434
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	internal_error
.Ltmp339:
	.size	verify_flow_info, .Ltmp339-verify_flow_info
	.cfi_endproc

	.globl	purge_dead_edges
	.align	16, 0x90
	.type	purge_dead_edges,@function
purge_dead_edges:                       # @purge_dead_edges
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp346:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp347:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp348:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp349:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp350:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp351:
	.cfi_def_cfa_offset 64
.Ltmp352:
	.cfi_offset %rbx, -48
.Ltmp353:
	.cfi_offset %r12, -40
.Ltmp354:
	.cfi_offset %r13, -32
.Ltmp355:
	.cfi_offset %r14, -24
.Ltmp356:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	8(%r15), %r12
	movzbl	(%r12), %eax
	cmpq	$32, %rax
	jne	.LBB28_6
# BB#1:                                 # %land.lhs.true
	movq	$23, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB28_6
# BB#2:                                 # %if.then
	movq	32(%r12), %rdi
	callq	may_trap_p
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB28_5
# BB#3:                                 # %lor.lhs.false
	movq	%r12, %rdi
	callq	find_reg_equal_equiv_note
	testq	%rax, %rax
	je	.LBB28_6
# BB#4:                                 # %land.lhs.true5
	movq	8(%rax), %rdi
	callq	may_trap_p
	testq	%r14, %rax
	jne	.LBB28_6
.LBB28_5:                               # %if.then11
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	remove_note
.LBB28_6:                               # %if.end12
	movq	8(%r15), %rdi
	callq	can_throw_internal
	xorq	%r13, %r13
	testq	%rax, %rax
	jne	.LBB28_11
# BB#7:                                 # %if.then15
	movq	40(%r15), %rbx
	xorq	%r13, %r13
	jmp	.LBB28_8
.LBB28_10:                              # %if.then18
                                        #   in Loop: Header=BB28_8 Depth=1
	callq	remove_edge
	movq	$1, %r13
	.align	16, 0x90
.LBB28_8:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	testq	%rdi, %rdi
	je	.LBB28_11
# BB#9:                                 # %for.body
                                        #   in Loop: Header=BB28_8 Depth=1
	movzbl	48(%rdi), %eax
	movq	8(%rdi), %rbx
	testq	$8, %rax
	je	.LBB28_8
	jmp	.LBB28_10
.LBB28_11:                              # %if.end20
	movzbl	(%r12), %eax
	cmpq	$33, %rax
	jne	.LBB28_39
# BB#12:                                # %if.then24
	movq	%r15, %r14
	movq	%r13, 8(%rsp)           # 8-byte Spill
	movq	%r12, %rdi
	callq	any_condjump_p
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	jne	.LBB28_15
# BB#13:                                # %land.lhs.true28
	movq	%r12, %rdi
	callq	returnjump_p
	testq	%r15, %rax
	jne	.LBB28_15
# BB#14:                                # %land.lhs.true31
	movq	%r12, %rdi
	callq	simplejump_p
	xorq	%rbx, %rbx
	testq	%r15, %rax
	je	.LBB28_53
.LBB28_15:                              # %if.end35
	movq	%r14, %rax
	movq	40(%rax), %r14
	xorq	%rbx, %rbx
	testq	%r14, %r14
	je	.LBB28_53
# BB#16:                                # %for.body39.lr.ph.lr.ph
	movq	%rax, (%rsp)            # 8-byte Spill
	movabsq	$4294967293, %r13       # imm = 0xFFFFFFFD
	jmp	.LBB28_17
.LBB28_39:                              # %if.end143
	movq	40(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB28_52
# BB#40:
	movq	%rbx, %rax
	.align	16, 0x90
.LBB28_42:                              # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movzbl	48(%rax), %ecx
	testq	$15, %rcx
	je	.LBB28_43
# BB#41:                                # %for.cond145
                                        #   in Loop: Header=BB28_42 Depth=1
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB28_42
	jmp	.LBB28_52
.LBB28_32:                              # %if.end75
                                        #   in Loop: Header=BB28_17 Depth=1
	movq	%rbx, %rdi
	callq	remove_edge
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
	jmp	.LBB28_19
	.align	16, 0x90
.LBB28_17:                              # %for.body39
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rbx
	movl	48(%rbx), %eax
	movq	8(%rbx), %r14
	movq	%rax, %rcx
	andq	%r13, %rcx
	movl	%ecx, 48(%rbx)
	testq	$1, %rax
	je	.LBB28_27
# BB#18:                                # %land.lhs.true46
                                        #   in Loop: Header=BB28_17 Depth=1
	movq	%r12, %rdi
	callq	any_condjump_p
	testq	%r15, %rax
	jne	.LBB28_19
.LBB28_27:                              # %if.else
                                        #   in Loop: Header=BB28_17 Depth=1
	movq	24(%rbx), %rax
	movabsq	$entry_exit_blocks+112, %rcx
	cmpq	%rcx, %rax
	jne	.LBB28_28
# BB#29:                                # %land.lhs.true61
                                        #   in Loop: Header=BB28_17 Depth=1
	movq	%r12, %rdi
	callq	returnjump_p
	testq	%r15, %rax
	jne	.LBB28_19
	jmp	.LBB28_30
	.align	16, 0x90
.LBB28_28:                              # %land.lhs.true51
                                        #   in Loop: Header=BB28_17 Depth=1
	movq	(%rax), %rax
	movq	64(%r12), %rcx
	cmpq	%rcx, %rax
	je	.LBB28_19
.LBB28_30:                              # %if.else65
                                        #   in Loop: Header=BB28_17 Depth=1
	movzbl	48(%rbx), %eax
	testq	$8, %rax
	je	.LBB28_32
# BB#31:                                # %land.lhs.true69
                                        #   in Loop: Header=BB28_17 Depth=1
	movq	%r12, %rdi
	callq	can_throw_internal
	testq	%rax, %rax
	je	.LBB28_32
.LBB28_19:                              # %for.cond37.backedge
                                        #   in Loop: Header=BB28_17 Depth=1
	testq	%r14, %r14
	jne	.LBB28_17
# BB#20:                                # %for.end77
	movq	(%rsp), %r14            # 8-byte Reload
	cmpq	$0, 40(%r14)
	movabsq	$0, %rbx
	je	.LBB28_53
# BB#21:                                # %for.end77
	andq	$1, 8(%rsp)             # 8-byte Folded Spill
	je	.LBB28_53
# BB#22:                                # %if.end83
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB28_24
# BB#23:                                # %if.then85
	movl	88(%r14), %edx
	movabsq	$.L.str40, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB28_24:                              # %if.end87
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	movq	$1, %rbx
	je	.LBB28_53
# BB#25:                                # %if.end91
	movq	40(%r14), %rax
	cmpq	$0, 8(%rax)
	je	.LBB28_26
# BB#33:                                # %if.else99
	movq	$16, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB28_53
# BB#34:                                # %if.end104
	movq	40(%r14), %rcx
	movl	48(%rcx), %edx
	movq	8(%rcx), %r8
	andq	$1, %rdx
	movq	%rcx, %rdi
	je	.LBB28_36
# BB#35:                                # %if.end104
	movq	%r8, %rdi
.LBB28_36:                              # %if.end104
	testq	%rdx, %rdx
	movq	$1, %rbx
	je	.LBB28_38
# BB#37:                                # %if.end104
	movq	%rcx, %r8
.LBB28_38:                              # %if.end104
	movq	8(%rax), %rax
	movl	8(%rax), %eax
	movl	%eax, 52(%rdi)
	movq	$10000, %rsi            # imm = 0x2710
	subq	%rax, %rsi
	movl	%esi, 52(%r8)
	movq	96(%r14), %r10
	movslq	52(%rdi), %rax
	imulq	%r10, %rax
	movabsq	$3777893186295716171, %r9 # imm = 0x346DC5D63886594B
	imulq	%r9
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$11, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	movq	%rdx, 56(%rdi)
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	imulq	%r10, %rsi
	movq	%rsi, %rax
	imulq	%r9
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$11, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	movq	%rdx, 56(%r8)
	jmp	.LBB28_53
.LBB28_43:
	movq	%r15, %r14
	jmp	.LBB28_44
.LBB28_47:                              # %if.then166
                                        #   in Loop: Header=BB28_44 Depth=1
	movq	%rdx, %r14
	callq	remove_edge
	movq	$1, %r13
.LBB28_44:                              # %for.cond159.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB28_45 Depth 2
	movq	%r14, %rdx
	.align	16, 0x90
.LBB28_45:                              # %for.cond159
                                        #   Parent Loop BB28_44 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	testq	%rdi, %rdi
	je	.LBB28_48
# BB#46:                                # %for.body161
                                        #   in Loop: Header=BB28_45 Depth=2
	movzbl	48(%rdi), %eax
	movq	8(%rdi), %rbx
	testq	$1, %rax
	jne	.LBB28_45
	jmp	.LBB28_47
.LBB28_48:                              # %for.end169
	movq	40(%rdx), %rax
	testq	%rax, %rax
	je	.LBB28_54
# BB#49:                                # %lor.lhs.false172
	cmpq	$0, 8(%rax)
	jne	.LBB28_54
# BB#50:                                # %if.end177
	movq	$10000, %rcx            # imm = 0x2710
	movl	%ecx, 52(%rax)
	movq	96(%rdx), %rcx
	movq	%rcx, 56(%rax)
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB28_52
# BB#51:                                # %if.then184
	movl	88(%rdx), %edx
	movabsq	$.L.str41, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB28_52:                              # %if.end187
	andq	$1, %r13
	movq	%r13, %rbx
.LBB28_53:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB28_26:                              # %if.then95
	movq	$10000, %rcx            # imm = 0x2710
	movl	%ecx, 52(%rax)
	movq	96(%r14), %rcx
	movq	%rcx, 56(%rax)
	jmp	.LBB28_53
.LBB28_54:                              # %if.then176
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.purge_dead_edges, %rcx
	movq	$2045, %rsi             # imm = 0x7FD
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp357:
	.size	purge_dead_edges, .Ltmp357-purge_dead_edges
	.cfi_endproc

	.globl	purge_all_dead_edges
	.align	16, 0x90
	.type	purge_all_dead_edges,@function
purge_all_dead_edges:                   # @purge_all_dead_edges
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp364:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp365:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp366:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp367:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp368:
	.cfi_def_cfa_offset 48
.Ltmp369:
	.cfi_offset %rbx, -48
.Ltmp370:
	.cfi_offset %r12, -40
.Ltmp371:
	.cfi_offset %r13, -32
.Ltmp372:
	.cfi_offset %r14, -24
.Ltmp373:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	xorq	%r15, %r15
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %r14
	movabsq	$0, %r12
	je	.LBB29_2
# BB#1:                                 # %if.then
	movl	n_basic_blocks(%rip), %edi
	callq	sbitmap_alloc
	movq	%rax, %r12
	movq	%r12, %rdi
	callq	sbitmap_zero
.LBB29_2:                               # %if.end
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB29_16
# BB#3:                                 # %for.body.lr.ph
	xorq	%r13, %r13
	testq	%rbx, %r14
	movq	%rbx, %r15
	je	.LBB29_8
# BB#4:
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB29_5:                               # %for.body.us
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rbx,8), %rdi
	callq	purge_dead_edges
	testq	%rax, %rax
	je	.LBB29_7
# BB#6:                                 # %if.then6.us
                                        #   in Loop: Header=BB29_5 Depth=1
	movq	$1, %rdx
	movq	%rbx, %rcx
	shlq	%cl, %rdx
	movq	$3, %rcx
	movq	%rbx, %rsi
	shrq	%cl, %rsi
	andq	$536870904, %rsi        # imm = 0x1FFFFFF8
	orq	%rdx, 16(%r12,%rsi)
.LBB29_7:                               # %for.inc.us
                                        #   in Loop: Header=BB29_5 Depth=1
	orq	%rax, %r13
	incq	%rbx
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB29_5
	jmp	.LBB29_10
.LBB29_8:
	movq	$4, %rbx
	.align	16, 0x90
.LBB29_9:                               # %for.inc
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	(%rax,%rbx,8), %rdi
	callq	purge_dead_edges
	orq	%rax, %r13
	movslq	n_basic_blocks(%rip), %rax
	leaq	-3(%rbx), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB29_9
.LBB29_10:                              # %for.end
	movq	%r15, %rbx
	testq	%rbx, %r13
	je	.LBB29_11
# BB#12:                                # %for.end
	movabsq	$1, %r15
	jmp	.LBB29_13
.LBB29_11:
	movabsq	$0, %r15
.LBB29_13:                              # %for.end
	testq	%rbx, %r14
	je	.LBB29_16
# BB#14:                                # %for.end
	testq	%rbx, %r13
	je	.LBB29_16
# BB#15:                                # %if.then14
	movq	$1, %rsi
	movq	$25, %rdx
	movq	%r12, %rdi
	callq	update_life_info
.LBB29_16:                              # %if.end15
	testq	%rbx, %r14
	je	.LBB29_18
# BB#17:                                # %if.then17
	movq	%r12, %rdi
	callq	free
.LBB29_18:                              # %if.end18
	movq	%r15, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp374:
	.size	purge_all_dead_edges, .Ltmp374-purge_all_dead_edges
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"cfgrtl.c"
	.size	.L.str, 9

	.type	.L__FUNCTION__.delete_insn,@object # @__FUNCTION__.delete_insn
.L__FUNCTION__.delete_insn:
	.asciz	"delete_insn"
	.size	.L__FUNCTION__.delete_insn, 12

	.type	basic_block_for_insn,@object # @basic_block_for_insn
	.comm	basic_block_for_insn,8,8
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"basic_block_for_insn"
	.size	.L.str1, 21

	.type	.L__FUNCTION__.redirect_edge_and_branch,@object # @__FUNCTION__.redirect_edge_and_branch
.L__FUNCTION__.redirect_edge_and_branch:
	.asciz	"redirect_edge_and_branch"
	.size	.L__FUNCTION__.redirect_edge_and_branch, 25

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Edge %i->%i redirected to %i\n"
	.size	.L.str2, 30

	.type	.L__FUNCTION__.split_edge,@object # @__FUNCTION__.split_edge
.L__FUNCTION__.split_edge:
	.asciz	"split_edge"
	.size	.L__FUNCTION__.split_edge, 11

	.type	.L__FUNCTION__.insert_insn_on_edge,@object # @__FUNCTION__.insert_insn_on_edge
.L__FUNCTION__.insert_insn_on_edge:
	.asciz	"insert_insn_on_edge"
	.size	.L__FUNCTION__.insert_insn_on_edge, 20

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	";; Basic block %d, loop depth %d, count "
	.size	.L.str3, 41

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"%lld"
	.size	.L.str4, 5

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	";; Predecessors: "
	.size	.L.str5, 18

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	";; Registers live at start:"
	.size	.L.str6, 28

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	";; Registers live at end:"
	.size	.L.str7, 26

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	";; Successors: "
	.size	.L.str8, 16

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"(nil)\n"
	.size	.L.str9, 7

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	";; Start of basic block %d, registers live:"
	.size	.L.str10, 44

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	";; Insn is not within a basic block\n"
	.size	.L.str11, 37

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	";; Insn is in multiple basic blocks\n"
	.size	.L.str12, 37

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	";; End of basic block %d, registers live:\n"
	.size	.L.str13, 43

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"\n;; Insns in epilogue delay list:\n\n"
	.size	.L.str14, 36

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"end insn %d for block %d not found in the insn stream"
	.size	.L.str15, 54

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"insn %d is in multiple basic blocks (%d and %d)"
	.size	.L.str16, 48

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"head insn %d for block %d not found in the insn stream"
	.size	.L.str17, 55

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"verify_flow_info: Duplicate edge %i->%i"
	.size	.L.str18, 40

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"verify_flow_info: Incorrect blocks for fallthru %i->%i"
	.size	.L.str19, 55

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"verify_flow_info: Incorrect fallthru %i->%i"
	.size	.L.str20, 44

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"wrong insn in the fallthru edge"
	.size	.L.str21, 32

	.type	.L__FUNCTION__.verify_flow_info,@object # @__FUNCTION__.verify_flow_info
.L__FUNCTION__.verify_flow_info:
	.asciz	"verify_flow_info"
	.size	.L__FUNCTION__.verify_flow_info, 17

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"verify_flow_info: Basic block %d succ edge is corrupted"
	.size	.L.str22, 56

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"Predecessor: "
	.size	.L.str23, 14

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"\nSuccessor: "
	.size	.L.str24, 13

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"missing barrier after block %i"
	.size	.L.str26, 31

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"basic block %d pred edge is corrupted"
	.size	.L.str27, 38

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"insn %d inside basic block %d but block_for_insn is NULL"
	.size	.L.str28, 57

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"insn %d inside basic block %d but block_for_insn is %i"
	.size	.L.str29, 55

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"NOTE_INSN_BASIC_BLOCK is missing for block %d"
	.size	.L.str30, 46

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"NOTE_INSN_BASIC_BLOCK %d in middle of basic block %d"
	.size	.L.str31, 53

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"in basic block %d:"
	.size	.L.str32, 19

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"flow control insn inside a basic block"
	.size	.L.str33, 39

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"basic block %i edge lists are corrupted"
	.size	.L.str34, 40

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"basic blocks not numbered consecutively"
	.size	.L.str35, 40

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"insn outside basic block"
	.size	.L.str36, 25

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"return not followed by barrier"
	.size	.L.str37, 31

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"number of bb notes in insn chain (%d) != n_basic_blocks (%d)"
	.size	.L.str38, 61

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"verify_flow_info failed"
	.size	.L.str39, 24

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"Purged edges from bb %i\n"
	.size	.L.str40, 25

	.type	.L__FUNCTION__.purge_dead_edges,@object # @__FUNCTION__.purge_dead_edges
.L__FUNCTION__.purge_dead_edges:
	.asciz	"purge_dead_edges"
	.size	.L__FUNCTION__.purge_dead_edges, 17

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"Purged non-fallthru edges from bb %i\n"
	.size	.L.str41, 38

	.type	label_value_list,@object # @label_value_list
	.comm	label_value_list,8,8
	.type	tail_recursion_label_list,@object # @tail_recursion_label_list
	.comm	tail_recursion_label_list,8,8
	.type	.L__FUNCTION__.commit_one_edge_insertion,@object # @__FUNCTION__.commit_one_edge_insertion
.L__FUNCTION__.commit_one_edge_insertion:
	.asciz	"commit_one_edge_insertion"
	.size	.L__FUNCTION__.commit_one_edge_insertion, 26

	.type	.L__FUNCTION__.force_nonfallthru_and_redirect,@object # @__FUNCTION__.force_nonfallthru_and_redirect
.L__FUNCTION__.force_nonfallthru_and_redirect:
	.asciz	"force_nonfallthru_and_redirect"
	.size	.L__FUNCTION__.force_nonfallthru_and_redirect, 31

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"Removing jump %i.\n"
	.size	.L.str42, 19

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"Redirecting jump %i from %i to %i.\n"
	.size	.L.str43, 36

	.type	.L__FUNCTION__.try_redirect_by_replacing_jump,@object # @__FUNCTION__.try_redirect_by_replacing_jump
.L__FUNCTION__.try_redirect_by_replacing_jump:
	.asciz	"try_redirect_by_replacing_jump"
	.size	.L__FUNCTION__.try_redirect_by_replacing_jump, 31

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"Replacing insn %i by jump %i\n"
	.size	.L.str44, 30


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
