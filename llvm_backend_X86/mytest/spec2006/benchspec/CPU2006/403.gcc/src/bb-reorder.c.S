	.file	"bb-reorder.c.bc"
	.text
	.globl	reorder_basic_blocks
	.align	16, 0x90
	.type	reorder_basic_blocks,@function
reorder_basic_blocks:                   # @reorder_basic_blocks
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5:
	.cfi_def_cfa_offset 32
.Ltmp6:
	.cfi_offset %rbx, -24
.Ltmp7:
	.cfi_offset %r14, -16
	movslq	n_basic_blocks(%rip), %rax
	cmpq	$2, %rax
	jl	.LBB0_13
# BB#1:                                 # %if.end
	callq	*targetm+336(%rip)
	testq	%rax, %rax
	je	.LBB0_2
.LBB0_13:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB0_2:                                # %if.end2
	callq	cfg_layout_initialize
	movl	n_basic_blocks(%rip), %r14d
	decq	%r14
	xorq	%rax, %rax
	jmp	.LBB0_3
	.align	16, 0x90
.LBB0_8:                                # %do.cond.thread.us.i
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	%rax, %rsi
	callq	make_reorder_chain_1
.LBB0_3:                                # %for.body.lr.ph.us.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_4 Depth 2
	movq	basic_block_info(%rip), %r8
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB0_4:                                # %for.body.us.i
                                        #   Parent Loop BB0_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	32(%r8,%rbx,8), %rdi
	movq	80(%rdi), %rcx
	movl	24(%rcx), %ecx
	testq	%rcx, %rcx
	je	.LBB0_6
# BB#5:                                 # %select.mid
                                        #   in Loop: Header=BB0_4 Depth=2
	xorq	%rdi, %rdi
.LBB0_6:                                # %select.end
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rsi
	jge	.LBB0_7
# BB#9:                                 # %select.end
                                        #   in Loop: Header=BB0_4 Depth=2
	incq	%rbx
	testq	%rdi, %rdi
	je	.LBB0_4
.LBB0_7:                                # %for.end.us.i
                                        #   in Loop: Header=BB0_3 Depth=1
	testq	%rdi, %rdi
	jne	.LBB0_8
# BB#10:                                # %make_reorder_chain.exit
	movq	80(%rax), %rax
	movq	$0, 16(%rax)
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_12
# BB#11:                                # %if.then3
	callq	dump_flow_info
.LBB0_12:                               # %if.end4
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	cfg_layout_finalize  # TAILCALL
.Ltmp8:
	.size	reorder_basic_blocks, .Ltmp8-reorder_basic_blocks
	.cfi_endproc

	.align	16, 0x90
	.type	make_reorder_chain_1,@function
make_reorder_chain_1:                   # @make_reorder_chain_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp14:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp15:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp16:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp17:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp18:
	.cfi_def_cfa_offset 48
.Ltmp19:
	.cfi_offset %rbx, -40
.Ltmp20:
	.cfi_offset %r12, -32
.Ltmp21:
	.cfi_offset %r14, -24
.Ltmp22:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	testq	%rsi, %rsi
	jne	.LBB1_1
# BB#4:                                 # %if.else
	movl	88(%r14), %eax
	testq	%rax, %rax
	je	.LBB1_5
# BB#49:                                # %if.then9
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.make_reorder_chain_1, %rcx
	movq	$168, %rsi
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB1_1:                                # %restart
	movq	80(%rsi), %rax
	movq	%r14, 16(%rax)
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB1_5
# BB#2:                                 # %land.lhs.true
	movl	88(%rsi), %ecx
	movl	88(%r14), %edx
	leaq	1(%rcx), %rax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	cmpq	%rdx, %rsi
	je	.LBB1_5
# BB#3:                                 # %if.then4
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB1_5:                                # %if.end11
	movq	80(%r14), %rax
	movq	$1, %rcx
	movl	%ecx, 24(%rax)
	cmpq	$0, 40(%r14)
	je	.LBB1_6
# BB#7:                                 # %if.end15
	movq	8(%r14), %rdi
	callq	any_condjump_p
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB1_21
# BB#8:                                 # %land.lhs.true18
	movq	8(%r14), %rdi
	movq	$16, %rsi
	xorq	%rdx, %rdx
	xorq	%r12, %r12
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB1_21
# BB#9:                                 # %if.then22
	movq	8(%rax), %rax
	movq	40(%r14), %rbx
	movq	8(%rax), %rax
	testq	%rbx, %rbx
	je	.LBB1_10
# BB#11:
	movq	%rbx, %rcx
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB1_12:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	48(%rcx), %esi
	testq	$1, %rsi
	jne	.LBB1_13
# BB#14:                                # %if.else31
                                        #   in Loop: Header=BB1_12 Depth=1
	testq	$8, %rsi
	movq	%rcx, %rsi
	je	.LBB1_16
# BB#15:                                # %if.else31
                                        #   in Loop: Header=BB1_12 Depth=1
	movq	%rdx, %rsi
.LBB1_16:                               # %if.else31
                                        #   in Loop: Header=BB1_12 Depth=1
	movq	%rsi, %rdx
	jmp	.LBB1_17
	.align	16, 0x90
.LBB1_13:                               #   in Loop: Header=BB1_12 Depth=1
	movq	%rcx, %r12
.LBB1_17:                               # %for.inc
                                        #   in Loop: Header=BB1_12 Depth=1
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB1_12
	jmp	.LBB1_18
.LBB1_6:
	movq	%r14, %rsi
	jmp	.LBB1_48
.LBB1_10:
	xorq	%rdx, %rdx
.LBB1_18:                               # %if.end39
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$5000, %rax             # imm = 0x1388
	jg	.LBB1_20
# BB#19:                                # %if.end39
	movq	%r12, %rdx
.LBB1_20:                               # %if.end39
	movq	24(%rdx), %rdi
	testq	%rdi, %rdi
	jne	.LBB1_32
.LBB1_21:                               # %for.cond43.preheader
	movq	40(%r14), %r8
	xorq	%rbx, %rbx
	testq	%r8, %r8
	je	.LBB1_35
# BB#22:                                # %for.body45.lr.ph
	xorq	%rbx, %rbx
	movabsq	$0, %r10
	movabsq	$1, %r9
	movq	%r8, %rsi
	.align	16, 0x90
.LBB1_23:                               # %for.body45
                                        # =>This Inner Loop Header: Depth=1
	movl	48(%rsi), %eax
	movq	24(%rsi), %rdi
	testq	$1, %rax
	jne	.LBB1_31
# BB#24:                                # %if.else51
                                        #   in Loop: Header=BB1_23 Depth=1
	movl	88(%r14), %ecx
	movl	88(%rdi), %edx
	incq	%rcx
	andq	%r15, %rcx
	cmpq	%rcx, %rdx
	movq	%r10, %rcx
	jne	.LBB1_26
# BB#25:                                # %if.else51
                                        #   in Loop: Header=BB1_23 Depth=1
	movq	%r9, %rcx
.LBB1_26:                               # %if.else51
                                        #   in Loop: Header=BB1_23 Depth=1
	testq	$12, %rax
	movq	%r10, %rax
	jne	.LBB1_28
# BB#27:                                # %if.else51
                                        #   in Loop: Header=BB1_23 Depth=1
	movq	%r9, %rax
.LBB1_28:                               # %if.else51
                                        #   in Loop: Header=BB1_23 Depth=1
	testq	%rax, %rcx
	jne	.LBB1_30
# BB#29:                                # %if.else51
                                        #   in Loop: Header=BB1_23 Depth=1
	movq	%rbx, %rdi
.LBB1_30:                               # %if.else51
                                        #   in Loop: Header=BB1_23 Depth=1
	movq	8(%rsi), %rsi
	testq	%rsi, %rsi
	movq	%rdi, %rbx
	jne	.LBB1_23
.LBB1_31:
	movq	%r8, %rbx
.LBB1_32:                               # %if.end70
	testq	%rdi, %rdi
	je	.LBB1_35
# BB#33:                                # %if.end70
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %rdi
	je	.LBB1_35
# BB#34:                                # %lor.lhs.false74
	movq	80(%rdi), %rax
	movl	24(%rax), %eax
	testq	%rax, %rax
	je	.LBB1_36
.LBB1_35:                               # %if.then78
	xorq	%rdi, %rdi
.LBB1_36:                               # %for.cond81.preheader
	testq	%rbx, %rbx
	je	.LBB1_47
# BB#37:
	movabsq	$entry_exit_blocks+112, %r15
	.align	16, 0x90
.LBB1_38:                               # %for.body83
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rax
	cmpq	%r15, %rax
	je	.LBB1_46
# BB#39:                                # %land.lhs.true87
                                        #   in Loop: Header=BB1_38 Depth=1
	movq	80(%rax), %rcx
	movl	24(%rcx), %ecx
	testq	%rcx, %rcx
	jne	.LBB1_46
# BB#40:                                # %land.lhs.true92
                                        #   in Loop: Header=BB1_38 Depth=1
	cmpq	$0, 40(%rax)
	je	.LBB1_46
# BB#41:                                # %land.lhs.true96
                                        #   in Loop: Header=BB1_38 Depth=1
	movzbl	48(%rbx), %ecx
	testq	$12, %rcx
	jne	.LBB1_46
# BB#42:                                # %if.then100
                                        #   in Loop: Header=BB1_38 Depth=1
	testq	%rdi, %rdi
	jne	.LBB1_44
# BB#43:                                #   in Loop: Header=BB1_38 Depth=1
	movq	%rax, %rdi
	jmp	.LBB1_46
.LBB1_44:                               # %if.then102
                                        #   in Loop: Header=BB1_38 Depth=1
	movq	%r14, %rsi
	callq	make_reorder_chain_1
	movq	24(%rbx), %rdi
	movq	80(%rdi), %rcx
	movl	24(%rcx), %ecx
	movq	%rax, %r14
	testq	%rcx, %rcx
	je	.LBB1_46
# BB#45:                                # %select.mid
                                        #   in Loop: Header=BB1_38 Depth=1
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB1_46:                               # %for.inc117
                                        #   in Loop: Header=BB1_38 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB1_38
.LBB1_47:                               # %for.end119
	movq	%r14, %rsi
	testq	%rdi, %rdi
	movq	%rdi, %r14
	jne	.LBB1_1
.LBB1_48:                               # %return
	movq	%rsi, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp23:
	.size	make_reorder_chain_1, .Ltmp23-make_reorder_chain_1
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Reordering block %d after %d\n"
	.size	.L.str, 30

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"bb-reorder.c"
	.size	.L.str1, 13

	.type	.L__FUNCTION__.make_reorder_chain_1,@object # @__FUNCTION__.make_reorder_chain_1
.L__FUNCTION__.make_reorder_chain_1:
	.asciz	"make_reorder_chain_1"
	.size	.L__FUNCTION__.make_reorder_chain_1, 21


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
