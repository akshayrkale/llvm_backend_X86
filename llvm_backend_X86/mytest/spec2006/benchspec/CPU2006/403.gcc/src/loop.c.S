	.file	"loop.c.bc"
	.text
	.globl	init_loop
	.align	16, 0x90
	.type	init_loop,@function
init_loop:                              # @init_loop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	movl	word_mode(%rip), %edi
	movq	$58, %rsi
	callq	gen_rtx_REG
	movq	$4, %rsi
	movq	%rax, %rdi
	callq	address_cost
	movl	%eax, reg_address_cost(%rip)
	movq	$1, %rax
	movb	%al, copy_cost(%rip)
	popq	%rax
	retq
.Ltmp2:
	.size	init_loop, .Ltmp2-init_loop
	.cfi_endproc

	.globl	loop_optimize
	.align	16, 0x90
	.type	loop_optimize,@function
loop_optimize:                          # @loop_optimize
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp9:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp10:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp11:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp12:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp13:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp14:
	.cfi_def_cfa_offset 128
.Ltmp15:
	.cfi_offset %rbx, -48
.Ltmp16:
	.cfi_offset %r12, -40
.Ltmp17:
	.cfi_offset %r13, -32
.Ltmp18:
	.cfi_offset %r14, -24
.Ltmp19:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rdi, %r15
	movq	%rsi, loop_dump_stream(%rip)
	callq	init_recog_no_volatile
	callq	max_reg_num
	xorq	%rbx, %rbx
	testq	%r15, %r15
	movl	%eax, max_reg_before_loop(%rip)
	movl	%eax, loop_max_reg(%rip)
	movq	$0, regs_may_share(%rip)
	movl	%ebx, max_loop_num(%rip)
	je	.LBB1_171
# BB#1:
	movabsq	$4294967200, %rdx       # imm = 0xFFFFFFA0
	movq	%r15, %rax
	.align	16, 0x90
.LBB1_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB1_5
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB1_2 Depth=1
	movl	40(%rax), %ecx
	cmpq	%rdx, %rcx
	jne	.LBB1_5
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB1_2 Depth=1
	incq	%rbx
	movl	%ebx, max_loop_num(%rip)
.LBB1_5:                                # %for.inc
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	.LBB1_2
# BB#6:                                 # %for.end
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB1_171
# BB#7:                                 # %if.end6
	callq	get_max_uid
	movl	max_loop_num(%rip), %edx
	movq	$5, %rcx
	shlq	%cl, %rdx
	leaq	1(%rax,%rdx), %rdi
	movl	%edi, max_uid_for_loop(%rip)
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$4, %rsi
	callq	xcalloc
	movslq	max_uid_for_loop(%rip), %rdi
	movq	%rax, uid_luid(%rip)
	movq	$8, %rsi
	callq	xcalloc
	movq	%rax, uid_loop(%rip)
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	$248, %rsi
	callq	xcalloc
	testq	%r15, %r15
	je	.LBB1_172
# BB#8:
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	%r14, 16(%rsp)          # 8-byte Spill
	xorq	%rax, %rax
	movq	%r15, %rcx
	.align	16, 0x90
.LBB1_9:                                # %for.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	movslq	8(%rcx), %rdx
	movslq	max_uid_for_loop(%rip), %rsi
	cmpq	%rsi, %rdx
	jge	.LBB1_15
# BB#10:                                # %if.end.i.i
                                        #   in Loop: Header=BB1_9 Depth=1
	movzbl	(%rcx), %esi
	cmpq	$37, %rsi
	jne	.LBB1_12
# BB#11:                                # %lor.lhs.false.i.i
                                        #   in Loop: Header=BB1_9 Depth=1
	movslq	40(%rcx), %rsi
	testq	%rsi, %rsi
	jg	.LBB1_13
.LBB1_12:                               # %if.then7.i.i
                                        #   in Loop: Header=BB1_9 Depth=1
	movq	uid_luid(%rip), %rsi
	incq	%rax
	jmp	.LBB1_14
.LBB1_13:                               # %if.else.i.i
                                        #   in Loop: Header=BB1_9 Depth=1
	movq	uid_luid(%rip), %rsi
.LBB1_14:                               # %for.inc.i.i
                                        #   in Loop: Header=BB1_9 Depth=1
	movl	%eax, (%rsi,%rdx,4)
.LBB1_15:                               # %for.inc.i.i
                                        #   in Loop: Header=BB1_9 Depth=1
	movq	24(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB1_9
# BB#16:                                # %for.body.i.preheader
	movq	uid_loop(%rip), %rax
	movq	$0, (%rax)
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	$1, %r13
	movabsq	$.L.str33, %r14
	movq	%r15, %rbx
	movq	%r15, 40(%rsp)          # 8-byte Spill
	xorq	%r15, %r15
	.align	16, 0x90
.LBB1_17:                               # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_30 Depth 2
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB1_27
# BB#18:                                # %if.then.i
                                        #   in Loop: Header=BB1_17 Depth=1
	movl	40(%rbx), %eax
	addq	$96, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$3, %rax
	ja	.LBB1_27
# BB#19:                                # %if.then.i
                                        #   in Loop: Header=BB1_17 Depth=1
	jmpq	*.LJTI1_0(,%rax,8)
.LBB1_20:                               # %sw.bb.i
                                        #   in Loop: Header=BB1_17 Depth=1
	movq	$32, %rcx
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$248, %rax, %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	movl	%edx, (%rcx,%rax)
	movq	%rbx, 192(%rcx,%rax)
	movq	%r15, 128(%rcx,%rax)
	leaq	(%rcx,%rax), %r15
	incq	%rdx
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	jmp	.LBB1_27
.LBB1_23:                               # %sw.bb5.i
                                        #   in Loop: Header=BB1_17 Depth=1
	testq	%r15, %r15
	je	.LBB1_24
# BB#26:                                # %if.end.i
                                        #   in Loop: Header=BB1_17 Depth=1
	movq	%rbx, 200(%r15)
	movq	128(%r15), %r15
	jmp	.LBB1_27
.LBB1_21:                               # %sw.bb3.i
                                        #   in Loop: Header=BB1_17 Depth=1
	movq	%rbx, 176(%r15)
	jmp	.LBB1_27
.LBB1_22:                               # %sw.bb4.i
                                        #   in Loop: Header=BB1_17 Depth=1
	movq	%rbx, 168(%r15)
	.align	16, 0x90
.LBB1_27:                               # %if.end9.i
                                        #   in Loop: Header=BB1_17 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB1_33
# BB#28:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB1_17 Depth=1
	movq	$28, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB1_33
# BB#29:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB1_17 Depth=1
	testq	%r15, %r15
	movq	%r15, %r12
	je	.LBB1_33
	.align	16, 0x90
.LBB1_30:                               # %for.body18.i
                                        #   Parent Loop BB1_17 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	%r13d, 156(%r12)
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB1_32
# BB#31:                                # %if.then20.i
                                        #   in Loop: Header=BB1_30 Depth=2
	movq	192(%r12), %rax
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	fprintf
.LBB1_32:                               # %for.inc.i
                                        #   in Loop: Header=BB1_30 Depth=2
	movq	128(%r12), %r12
	testq	%r12, %r12
	jne	.LBB1_30
.LBB1_33:                               # %if.end28.i
                                        #   in Loop: Header=BB1_17 Depth=1
	movslq	8(%rbx), %rax
	movq	uid_loop(%rip), %rcx
	movq	%r15, (%rcx,%rax,8)
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB1_17
	jmp	.LBB1_34
.LBB1_172:                              # %compute_luids.exit.thread.i
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movq	uid_loop(%rip), %rax
	movq	$0, (%rax)
.LBB1_34:                               # %for.end36.i
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	32(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_40
# BB#35:                                # %for.body39.lr.ph.i
	movq	uid_loop(%rip), %rcx
	movq	$1, %rdx
	.align	16, 0x90
.LBB1_36:                               # %for.body39.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_38 Depth 2
	movq	8(%rax), %rsi
	movslq	8(%rsi), %rsi
	movq	(%rcx,%rsi,8), %rsi
	jmp	.LBB1_38
	.align	16, 0x90
.LBB1_37:                               # %for.body.i700.i
                                        #   in Loop: Header=BB1_38 Depth=2
	movl	%edx, 156(%rsi)
	movq	128(%rsi), %rsi
.LBB1_38:                               # %for.body.i700.i
                                        #   Parent Loop BB1_36 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rsi, %rsi
	jne	.LBB1_37
# BB#39:                                # %invalidate_loops_containing_label.exit.i
                                        #   in Loop: Header=BB1_36 Depth=1
	movq	16(%rax), %rax
	testq	%rax, %rax
	jne	.LBB1_36
.LBB1_40:                               # %for.end47.i
	movabsq	$invalidate_loops_containing_label, %rdi
	callq	for_each_eh_label
	movq	40(%rsp), %r15          # 8-byte Reload
	testq	%r15, %r15
	je	.LBB1_130
# BB#41:
	movq	$1, %r12
	movq	%r15, %r13
	.align	16, 0x90
.LBB1_42:                               # %for.body50.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_47 Depth 2
                                        #     Child Loop BB1_56 Depth 2
                                        #     Child Loop BB1_63 Depth 2
                                        #     Child Loop BB1_80 Depth 2
                                        #     Child Loop BB1_82 Depth 2
                                        #     Child Loop BB1_95 Depth 2
                                        #     Child Loop BB1_109 Depth 2
                                        #     Child Loop BB1_118 Depth 2
                                        #     Child Loop BB1_113 Depth 2
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %ecx
	cmpq	$105, %rcx
	jne	.LBB1_129
# BB#43:                                # %if.then57.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movslq	8(%r13), %rcx
	movq	uid_loop(%rip), %rdx
	movq	(%rdx,%rcx,8), %r14
	orq	$2, %rax
	cmpq	$34, %rax
	jne	.LBB1_48
# BB#44:                                # %if.then71.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	$13, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB1_48
# BB#45:                                # %if.then74.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	8(%rax), %rax
	movslq	8(%rax), %rax
	movq	uid_loop(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	jmp	.LBB1_47
	.align	16, 0x90
.LBB1_46:                               # %for.body.i712.i
                                        #   in Loop: Header=BB1_47 Depth=2
	movl	%r12d, 156(%rax)
	movq	128(%rax), %rax
.LBB1_47:                               # %for.body.i712.i
                                        #   Parent Loop BB1_42 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rax, %rax
	jne	.LBB1_46
.LBB1_48:                               # %if.end79.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movzbl	(%r13), %eax
	cmpq	$33, %rax
	jne	.LBB1_129
# BB#49:                                # %if.end85.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	32(%r13), %rdi
	movq	%r14, %rsi
	callq	mark_loop_jump
	testq	%r14, %r14
	je	.LBB1_129
# BB#50:                                # %land.lhs.true90.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	32(%r13), %rax
	movzbl	(%rax), %eax
	cmpq	$51, %rax
	je	.LBB1_54
# BB#51:                                # %lor.lhs.false98.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	%r13, %rdi
	callq	any_uncondjump_p
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB1_129
# BB#52:                                # %land.lhs.true101.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	%r13, %rdi
	callq	onlyjump_p
	testq	%rbx, %rax
	je	.LBB1_129
# BB#53:                                # %land.lhs.true104.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	64(%r13), %rax
	movslq	8(%rax), %rax
	movq	uid_loop(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	cmpq	%r14, %rax
	je	.LBB1_129
.LBB1_54:                               # %land.lhs.true115.i
                                        #   in Loop: Header=BB1_42 Depth=1
	callq	get_max_uid
	movq	$32, %rcx
	movslq	max_uid_for_loop(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jge	.LBB1_129
# BB#55:                                # %if.then119.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	%r13, %rdi
	callq	next_real_insn
	movq	24(%r13), %rcx
	movq	%rcx, 72(%rsp)
	movq	%r13, %r12
	movabsq	$4294967200, %rcx       # imm = 0xFFFFFFA0
	.align	16, 0x90
.LBB1_56:                               # %for.cond127.i
                                        #   Parent Loop BB1_42 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	16(%r12), %r12
	movzbl	(%r12), %edi
	cmpq	$36, %rdi
	je	.LBB1_60
# BB#57:                                # %for.cond127.i
                                        #   in Loop: Header=BB1_56 Depth=2
	cmpq	$33, %rdi
	je	.LBB1_60
# BB#58:                                # %for.cond127.i
                                        #   in Loop: Header=BB1_56 Depth=2
	cmpq	$37, %rdi
	jne	.LBB1_56
# BB#59:                                # %land.lhs.true137.i
                                        #   in Loop: Header=BB1_56 Depth=2
	movl	40(%r12), %edx
	cmpq	%rcx, %rdx
	movq	$37, %rdi
	jne	.LBB1_56
.LBB1_60:                               # %for.end152.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	64(%r13), %rdx
	xorq	%rcx, %rcx
	testq	%rdx, %rdx
	je	.LBB1_66
# BB#61:                                # %if.then157.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movslq	8(%rdx), %rdx
	movq	uid_loop(%rip), %rsi
	movq	(%rsi,%rdx,8), %rdx
	testq	%rdx, %rdx
	je	.LBB1_66
# BB#62:                                # %for.cond168.preheader.i
                                        #   in Loop: Header=BB1_42 Depth=1
	cmpq	%r14, %rdx
	movq	%r14, %rcx
	je	.LBB1_66
	.align	16, 0x90
.LBB1_63:                               # %for.inc175.i
                                        #   Parent Loop BB1_42 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	128(%rdx), %rdx
	testq	%rdx, %rdx
	je	.LBB1_64
# BB#65:                                # %for.inc175.i
                                        #   in Loop: Header=BB1_63 Depth=2
	cmpq	%r14, %rdx
	movq	%rdx, %rcx
	jne	.LBB1_63
	jmp	.LBB1_66
.LBB1_64:                               #   in Loop: Header=BB1_42 Depth=1
	movq	%rdx, %rcx
.LBB1_66:                               # %if.end179.i
                                        #   in Loop: Header=BB1_42 Depth=1
	cmpq	$33, %rdi
	jne	.LBB1_128
# BB#67:                                # %land.lhs.true184.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	64(%r12), %rax
	testq	%rax, %rax
	je	.LBB1_70
# BB#68:                                # %land.lhs.true189.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movslq	8(%rax), %rdx
	movq	uid_loop(%rip), %rsi
	movq	(%rsi,%rdx,8), %rdx
	cmpq	%r14, %rdx
	je	.LBB1_70
# BB#69:                                # %select.mid
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	%r14, %rcx
.LBB1_70:                               # %if.end201.i
                                        #   in Loop: Header=BB1_42 Depth=1
	testq	%rcx, %rcx
	jne	.LBB1_128
# BB#71:                                # %if.end201.i
                                        #   in Loop: Header=BB1_42 Depth=1
	testq	%rax, %rax
	je	.LBB1_128
# BB#72:                                # %land.lhs.true214.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB1_128
# BB#73:                                # %land.lhs.true223.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	%r12, %rdi
	callq	any_condjump_p
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB1_128
# BB#74:                                # %land.lhs.true226.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	%r12, %rdi
	callq	onlyjump_p
	testq	%rbx, %rax
	je	.LBB1_128
# BB#75:                                # %land.lhs.true229.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	64(%r12), %rdi
	callq	next_real_insn
	movq	48(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB1_128
# BB#76:                                # %land.lhs.true236.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	24(%r13), %rsi
	movq	%r12, %rdi
	leaq	72(%rsp), %rdx
	callq	insns_safe_to_move_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_128
# BB#77:                                # %if.then242.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	64(%r13), %rsi
	testq	%rsi, %rsi
	jne	.LBB1_79
# BB#78:                                # %cond.false.i
                                        #   in Loop: Header=BB1_42 Depth=1
	callq	get_last_insn
	movq	%rax, %rsi
.LBB1_79:                               # %cond.end.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movslq	8(%rsi), %rcx
	movq	uid_loop(%rip), %rdx
	movq	72(%rsp), %rax
	movq	(%rdx,%rcx,8), %rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	jmp	.LBB1_80
.LBB1_93:                               # %for.inc271.i
                                        #   in Loop: Header=BB1_80 Depth=2
	movq	24(%rax), %rax
.LBB1_80:                               # %cond.end.i
                                        #   Parent Loop BB1_42 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rax, %rax
	je	.LBB1_81
# BB#90:                                # %land.rhs258.i
                                        #   in Loop: Header=BB1_80 Depth=2
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	je	.LBB1_81
# BB#91:                                # %land.rhs258.i
                                        #   in Loop: Header=BB1_80 Depth=2
	cmpq	$35, %rcx
	jne	.LBB1_93
# BB#92:                                # %if.then269.i
                                        #   in Loop: Header=BB1_80 Depth=2
	movq	%rax, 72(%rsp)
	jmp	.LBB1_93
.LBB1_81:                               # %for.cond276.preheader.i
                                        #   in Loop: Header=BB1_42 Depth=1
	testq	%rsi, %rsi
	movq	%rsi, %rbx
	je	.LBB1_128
.LBB1_82:                               # %for.body278.i
                                        #   Parent Loop BB1_42 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rbx), %eax
	cmpq	$35, %rax
	jne	.LBB1_89
# BB#83:                                # %land.lhs.true283.i
                                        #   in Loop: Header=BB1_82 Depth=2
	movq	%rbx, %rdi
	callq	next_nonnote_insn
	movq	8(%rsp), %rsi           # 8-byte Reload
	testq	%rax, %rax
	je	.LBB1_88
# BB#84:                                # %lor.lhs.false287.i
                                        #   in Loop: Header=BB1_82 Depth=2
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB1_88
# BB#85:                                # %lor.lhs.false292.i
                                        #   in Loop: Header=BB1_82 Depth=2
	movq	%rax, %rdi
	callq	next_nonnote_insn
	movq	8(%rsp), %rsi           # 8-byte Reload
	testq	%rax, %rax
	je	.LBB1_88
# BB#86:                                # %lor.lhs.false296.i
                                        #   in Loop: Header=BB1_82 Depth=2
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB1_88
# BB#87:                                # %lor.lhs.false301.i
                                        #   in Loop: Header=BB1_82 Depth=2
	movq	32(%rax), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	je	.LBB1_89
.LBB1_88:                               # %land.lhs.true317.i
                                        #   in Loop: Header=BB1_82 Depth=2
	movslq	8(%rbx), %rax
	movq	uid_loop(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB1_94
.LBB1_89:                               # %for.inc327.i
                                        #   in Loop: Header=BB1_82 Depth=2
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB1_82
	jmp	.LBB1_95
.LBB1_94:                               # %for.end331.i
                                        #   in Loop: Header=BB1_42 Depth=1
	testq	%rbx, %rbx
	jne	.LBB1_104
.LBB1_95:                               # %for.body337.i
                                        #   Parent Loop BB1_42 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rsi), %eax
	cmpq	$35, %rax
	jne	.LBB1_102
# BB#96:                                # %land.lhs.true342.i
                                        #   in Loop: Header=BB1_95 Depth=2
	movq	%rsi, %rbx
	movq	%rbx, %rdi
	callq	next_nonnote_insn
	movq	%rbx, %rsi
	testq	%rax, %rax
	je	.LBB1_101
# BB#97:                                # %lor.lhs.false346.i
                                        #   in Loop: Header=BB1_95 Depth=2
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB1_101
# BB#98:                                # %lor.lhs.false351.i
                                        #   in Loop: Header=BB1_95 Depth=2
	movq	%rsi, %rbx
	movq	%rax, %rdi
	callq	next_nonnote_insn
	movq	%rbx, %rsi
	testq	%rax, %rax
	je	.LBB1_101
# BB#99:                                # %lor.lhs.false355.i
                                        #   in Loop: Header=BB1_95 Depth=2
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB1_101
# BB#100:                               # %lor.lhs.false360.i
                                        #   in Loop: Header=BB1_95 Depth=2
	movq	32(%rax), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	je	.LBB1_102
.LBB1_101:                              # %land.lhs.true376.i
                                        #   in Loop: Header=BB1_95 Depth=2
	movslq	8(%rsi), %rax
	movq	uid_loop(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB1_103
.LBB1_102:                              # %for.inc386.i
                                        #   in Loop: Header=BB1_95 Depth=2
	movq	24(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB1_95
	jmp	.LBB1_128
.LBB1_103:                              # %if.end391.i
                                        #   in Loop: Header=BB1_42 Depth=1
	testq	%rsi, %rsi
	movq	%rsi, %rbx
	je	.LBB1_128
.LBB1_104:                              # %if.then393.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	64(%r12), %rbx
	movq	%r12, %rdi
	callq	get_label_after
	movq	%rax, 64(%rsp)
	movl	32(%rbx), %ecx
	incq	%rcx
	movl	%ecx, 32(%rbx)
	movq	$1, %rdx
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	invert_jump
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_125
# BB#105:                               # %if.end432.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	%rbx, (%rsp)            # 8-byte Spill
	leaq	64(%rsp), %rdi
	leaq	72(%rsp), %rsi
	callq	squeeze_notes
	testq	%rax, %rax
	jne	.LBB1_106
# BB#107:                               # %if.end435.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	64(%rsp), %rax
	movq	72(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	reorder_insns
	movq	72(%rsp), %rcx
	movq	64(%rsp), %rax
	jmp	.LBB1_109
.LBB1_108:                              # %for.body442.i
                                        #   in Loop: Header=BB1_109 Depth=2
	movslq	8(%rax), %rcx
	movq	uid_loop(%rip), %rdx
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, (%rdx,%rcx,8)
	movq	72(%rsp), %rcx
	movq	24(%rax), %rax
.LBB1_109:                              # %for.body442.i
                                        #   Parent Loop BB1_42 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rcx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_108
# BB#110:                               # %for.end452.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	64(%r13), %rcx
	testq	%rcx, %rcx
	je	.LBB1_122
# BB#111:                               # %if.then457.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	232(%r14), %rsi
	xorq	%rax, %rax
	testq	%rsi, %rsi
	movabsq	$0, %rdx
	je	.LBB1_112
.LBB1_118:                              # %for.body460.i
                                        #   Parent Loop BB1_42 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rsi, %rdi
	movq	8(%rdi), %rax
	cmpq	%rcx, %rax
	je	.LBB1_119
# BB#117:                               # %for.inc486.i
                                        #   in Loop: Header=BB1_118 Depth=2
	movq	16(%rdi), %rsi
	xorq	%rax, %rax
	testq	%rsi, %rsi
	movq	%rdi, %rdx
	jne	.LBB1_118
	jmp	.LBB1_112
.LBB1_119:                              # %if.then469.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movl	(%rdi), %eax
	movabsq	$4026531839, %rcx       # imm = 0xEFFFFFFF
	andq	%rcx, %rax
	movl	%eax, (%rdi)
	movq	16(%rdi), %rax
	testq	%rdx, %rdx
	jne	.LBB1_120
# BB#121:                               # %if.else.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	%rax, 232(%r14)
	movq	$1, %rax
	jmp	.LBB1_112
.LBB1_120:                              # %if.then473.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	%rax, 16(%rdx)
	movq	$1, %rax
.LBB1_112:                              # %for.cond491.preheader.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	48(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %r14
	movq	%r14, %rcx
	je	.LBB1_115
.LBB1_113:                              # %for.body497.i
                                        #   Parent Loop BB1_42 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	240(%rcx), %edx
	decq	%rdx
	movl	%edx, 240(%rcx)
	movq	128(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB1_115
# BB#114:                               # %for.body497.i
                                        #   in Loop: Header=BB1_113 Depth=2
	movq	48(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %rcx
	jne	.LBB1_113
.LBB1_115:                              # %for.end500.i
                                        #   in Loop: Header=BB1_42 Depth=1
	testq	%rax, %rax
	je	.LBB1_116
.LBB1_122:                              # %if.end504.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	32(%r12), %rax
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	mark_loop_jump
	movq	64(%r13), %rax
	testq	%rax, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	(%rsp), %rbx            # 8-byte Reload
	je	.LBB1_125
# BB#123:                               # %land.lhs.true513.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	%rax, %rdi
	movq	%rcx, %r12
	callq	next_real_insn
	movq	%rax, %r14
	movq	%r13, %rdi
	callq	next_real_insn
	movq	%r12, %rcx
	cmpq	%rax, %r14
	jne	.LBB1_125
# BB#124:                               # %if.then521.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	%r13, %rdi
	movq	%rcx, %r14
	callq	delete_related_insns
	movq	%r14, %rcx
.LBB1_125:                              # %if.end524.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movl	32(%rbx), %eax
	movq	24(%rbx), %r14
	decq	%rax
	testq	%rcx, %rax
	movl	%eax, 32(%rbx)
	movq	%rbx, %rax
	jne	.LBB1_127
# BB#126:                               # %if.then534.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	%rax, %rdi
	callq	delete_related_insns
.LBB1_127:                              # %if.end536.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	16(%r14), %r13
.LBB1_128:                              # %for.inc544.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	$1, %r12
	.align	16, 0x90
.LBB1_129:                              # %for.inc544.i
                                        #   in Loop: Header=BB1_42 Depth=1
	movq	24(%r13), %r13
	testq	%r13, %r13
	jne	.LBB1_42
.LBB1_130:                              # %find_and_verify_loops.exit
	movq	$240, %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	xcalloc
	movq	%rax, %r14
	movq	$32, %rcx
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jle	.LBB1_138
# BB#131:                               # %for.body22.preheader
	leaq	-1(%rdi), %r9
	andq	%rbx, %r9
	incq	%r9
	xorq	%rdx, %rdx
	movabsq	$8589934588, %r8        # imm = 0x1FFFFFFFC
	andq	%r9, %r8
	je	.LBB1_135
# BB#132:                               # %vector.body.preheader
	movq	%r9, %r10
	andq	$-4, %r10
	movq	56(%rsp), %r13          # 8-byte Reload
	leaq	160(%r13), %rsi
	xorq	%rdi, %rdi
	movq	%r14, %rbx
	.align	16, 0x90
.LBB1_133:                              # %vector.body
                                        # =>This Inner Loop Header: Depth=1
	leaq	3(%rdi), %r11
	leaq	2(%rdi), %r15
	leaq	1(%rdi), %r12
	imulq	$240, %r12, %rcx
	addq	%r14, %rcx
	imulq	$240, %r15, %rax
	addq	%r14, %rax
	imulq	$240, %r11, %rdx
	addq	%r14, %rdx
	imulq	$248, %r12, %r12
	imulq	$248, %r15, %r15
	imulq	$248, %r11, %r11
	movq	%rbx, (%rsi)
	movq	%rcx, 160(%r13,%r12)
	movq	%rax, 160(%r13,%r15)
	movq	%rdx, 160(%r13,%r11)
	addq	$4, %rdi
	addq	$992, %rsi              # imm = 0x3E0
	addq	$960, %rbx              # imm = 0x3C0
	cmpq	%rdi, %r10
	jne	.LBB1_133
# BB#134:
	movq	%r8, %rdx
	movq	40(%rsp), %r15          # 8-byte Reload
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	32(%rsp), %rdi          # 8-byte Reload
.LBB1_135:                              # %middle.block
	cmpq	%rdx, %r9
	je	.LBB1_138
# BB#136:                               # %for.body22.preheader1
	imulq	$248, %rdx, %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	leaq	160(%rax,%rcx), %rax
	imulq	$240, %rdx, %rcx
	leaq	1(%rdx), %rdx
	addq	%r14, %rcx
	andq	%rbx, %rdi
	.align	16, 0x90
.LBB1_137:                              # %for.body22
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, (%rax)
	movq	%rdx, %rsi
	andq	%rbx, %rsi
	incq	%rdx
	addq	$248, %rax
	addq	$240, %rcx
	cmpq	%rdi, %rsi
	jne	.LBB1_137
.LBB1_138:                              # %for.end27
	movl	max_reg_before_loop(%rip), %esi
	movq	$1, %rdx
	movq	%r15, %rdi
	callq	reg_scan
	callq	init_alias_analysis
	callq	get_max_uid
	movq	$32, %rcx
	movslq	max_uid_for_loop(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jg	.LBB1_139
# BB#140:                               # %if.end32
	callq	get_max_uid
	testq	%r15, %r15
	movl	%eax, max_uid_for_loop(%rip)
	movq	%rbx, %r8
	je	.LBB1_149
# BB#141:
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	.align	16, 0x90
.LBB1_142:                              # %for.body.i121
                                        # =>This Inner Loop Header: Depth=1
	movslq	8(%rsi), %rdi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdi
	jge	.LBB1_148
# BB#143:                               # %if.end.i124
                                        #   in Loop: Header=BB1_142 Depth=1
	movzbl	(%rsi), %eax
	cmpq	$37, %rax
	jne	.LBB1_145
# BB#144:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB1_142 Depth=1
	movslq	40(%rsi), %rax
	testq	%rax, %rax
	jg	.LBB1_146
.LBB1_145:                              # %if.then7.i127
                                        #   in Loop: Header=BB1_142 Depth=1
	movq	uid_luid(%rip), %rax
	incq	%rdx
	jmp	.LBB1_147
.LBB1_146:                              # %if.else.i128
                                        #   in Loop: Header=BB1_142 Depth=1
	movq	uid_luid(%rip), %rax
.LBB1_147:                              # %for.inc.i131
                                        #   in Loop: Header=BB1_142 Depth=1
	movl	%edx, (%rax,%rdi,4)
.LBB1_148:                              # %for.inc.i131
                                        #   in Loop: Header=BB1_142 Depth=1
	movq	24(%rsi), %rsi
	movl	max_uid_for_loop(%rip), %eax
	testq	%rsi, %rsi
	jne	.LBB1_142
.LBB1_149:                              # %for.cond35.preheader
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB1_157
# BB#150:                               # %for.body38.lr.ph
	movq	uid_luid(%rip), %rax
	movq	$1, %rsi
	.align	16, 0x90
.LBB1_151:                              # %for.body38
                                        # =>This Inner Loop Header: Depth=1
	movl	-4(%rax,%rsi,4), %ecx
	movl	%ecx, (%rax)
	movl	max_uid_for_loop(%rip), %edx
	testq	%rcx, %rcx
	jne	.LBB1_152
# BB#173:                               # %for.body38
                                        #   in Loop: Header=BB1_151 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	leaq	1(%rsi), %rbx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdi, %rsi
	movq	%rbx, %rsi
	jl	.LBB1_151
.LBB1_152:                              # %for.cond50.preheader
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB1_157
# BB#153:                               # %for.body53.lr.ph
	movq	uid_luid(%rip), %rbx
	movq	$1, %rsi
	.align	16, 0x90
.LBB1_154:                              # %for.body53
                                        # =>This Inner Loop Header: Depth=1
	movl	-4(%rbx,%rsi,4), %ecx
	testq	%rcx, %rcx
	jne	.LBB1_156
# BB#155:                               # %if.then58
                                        #   in Loop: Header=BB1_154 Depth=1
	movl	-8(%rbx,%rsi,4), %eax
	movl	%eax, -4(%rbx,%rsi,4)
	movl	max_uid_for_loop(%rip), %edx
.LBB1_156:                              # %for.inc64
                                        #   in Loop: Header=BB1_154 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rsi
	cmpq	%rax, %rdi
	jl	.LBB1_154
.LBB1_157:                              # %for.end66
	testq	%r15, %r15
	movq	56(%rsp), %r13          # 8-byte Reload
	je	.LBB1_158
# BB#159:
	movq	16(%rsp), %r12          # 8-byte Reload
	movq	%r8, %rbx
	.align	16, 0x90
.LBB1_160:                              # %for.body.i133
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %rdi
	callq	computed_jump_p
	testq	%rbx, %rax
	movq	$1, %rax
	jne	.LBB1_163
# BB#161:                               # %for.inc.i136
                                        #   in Loop: Header=BB1_160 Depth=1
	movq	24(%r15), %r15
	testq	%r15, %r15
	jne	.LBB1_160
# BB#162:
	xorq	%rax, %rax
	jmp	.LBB1_163
.LBB1_158:
	xorq	%rax, %rax
	movq	16(%rsp), %r12          # 8-byte Reload
.LBB1_163:                              # %indirect_jump_in_function_p.exit
	movslq	max_loop_num(%rip), %r15
	movl	%eax, indirect_jump_in_function(%rip)
	testq	%r15, %r15
	jle	.LBB1_168
# BB#164:                               # %for.body72.lr.ph
	imulq	$248, %r15, %rax
	decq	%r15
	leaq	-248(%rax,%r13), %rbx
	.align	16, 0x90
.LBB1_165:                              # %for.body72
                                        # =>This Inner Loop Header: Depth=1
	movl	156(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB1_167
# BB#166:                               # %land.lhs.true77
                                        #   in Loop: Header=BB1_165 Depth=1
	cmpq	$0, 200(%rbx)
	je	.LBB1_167
# BB#174:                               # %if.then79
                                        #   in Loop: Header=BB1_165 Depth=1
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	scan_loop
.LBB1_167:                              # %for.cond69.backedge
                                        #   in Loop: Header=BB1_165 Depth=1
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%r15
	addq	$-248, %rbx
	testq	%rax, %rax
	jg	.LBB1_165
.LBB1_168:                              # %for.end82
	movl	write_symbols(%rip), %eax
	testq	%rax, %rax
	je	.LBB1_170
# BB#169:                               # %if.then85
	callq	reorder_blocks
.LBB1_170:                              # %if.end86
	callq	end_alias_analysis
	movq	uid_luid(%rip), %rdi
	callq	free
	movq	uid_loop(%rip), %rdi
	callq	free
	movq	%r14, %rdi
	callq	free
	movq	%r13, %rdi
	callq	free
.LBB1_171:                              # %cleanup
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_24:                               # %if.then7.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_and_verify_loops, %rcx
	movq	$2679, %rsi             # imm = 0xA77
	jmp	.LBB1_25
.LBB1_106:                              # %if.then434.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_and_verify_loops, %rcx
	movq	$2892, %rsi             # imm = 0xB4C
	jmp	.LBB1_25
.LBB1_116:                              # %if.then502.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_and_verify_loops, %rcx
	movq	$2928, %rsi             # imm = 0xB70
	jmp	.LBB1_25
.LBB1_139:                              # %if.then31
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.loop_optimize, %rcx
	movq	$518, %rsi              # imm = 0x206
.LBB1_25:                               # %if.then7.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp20:
	.size	loop_optimize, .Ltmp20-loop_optimize
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI1_0:
	.quad	.LBB1_20
	.quad	.LBB1_23
	.quad	.LBB1_21
	.quad	.LBB1_22

	.text
	.align	16, 0x90
	.type	scan_loop,@function
scan_loop:                              # @scan_loop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp27:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp28:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp29:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp30:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp31:
	.cfi_def_cfa_offset 48
	subq	$336, %rsp              # imm = 0x150
.Ltmp32:
	.cfi_def_cfa_offset 384
.Ltmp33:
	.cfi_offset %rbx, -48
.Ltmp34:
	.cfi_offset %r12, -40
.Ltmp35:
	.cfi_offset %r13, -32
.Ltmp36:
	.cfi_offset %r14, -24
.Ltmp37:
	.cfi_offset %r15, -16
	movq	%rdi, %r10
	movq	160(%r10), %rbx
	movq	192(%r10), %r11
	movq	200(%r10), %rcx
	movq	$0, 208(%r10)
	movq	$0, 176(%rbx)
	movq	$0, 168(%rbx)
	movq	24(%r11), %rax
	cmpq	%rcx, %rax
	movq	%rcx, %rdx
	je	.LBB2_9
# BB#1:
	movabsq	$4294967294, %r8        # imm = 0xFFFFFFFE
	movabsq	$4294967200, %r9        # imm = 0xFFFFFFA0
	.align	16, 0x90
.LBB2_2:                                # %land.lhs.true
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %edx
	cmpq	$36, %rdx
	je	.LBB2_8
# BB#3:                                 # %land.lhs.true6
                                        #   in Loop: Header=BB2_2 Depth=1
	movzbl	rtx_class(%rdx), %edi
	cmpq	$105, %rdi
	je	.LBB2_8
# BB#4:                                 # %land.rhs
                                        #   in Loop: Header=BB2_2 Depth=1
	cmpq	$37, %rdx
	jne	.LBB2_5
# BB#7:                                 # %lor.rhs
                                        #   in Loop: Header=BB2_2 Depth=1
	movl	40(%rax), %edx
	andq	%r8, %rdx
	cmpq	%r9, %rdx
	je	.LBB2_8
.LBB2_5:                                # %for.cond.backedge
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	24(%rax), %rax
	cmpq	%rcx, %rax
	jne	.LBB2_2
# BB#6:
	movq	%rcx, %rdx
	jmp	.LBB2_9
.LBB2_8:
	movq	%rax, %rdx
.LBB2_9:                                # %for.end
	movq	%rdx, 208(%rsp)         # 8-byte Spill
	movq	%rdx, 216(%r10)
	movq	24(%rcx), %rax
	leaq	168(%rbx), %r14
	testq	%rax, %rax
	je	.LBB2_11
# BB#10:                                # %if.then
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	%rax, 224(%r10)
	movq	%rcx, %r12
	movq	%rcx, 112(%rsp)         # 8-byte Spill
	movq	%r11, %r13
	movq	%r11, 88(%rsp)          # 8-byte Spill
	movq	%rbx, 128(%rsp)         # 8-byte Spill
	movq	%rbx, %r15
	movq	%r10, %rbx
	jmp	.LBB2_12
.LBB2_11:                               # %if.else
	movq	%r11, 88(%rsp)          # 8-byte Spill
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	%rbx, 128(%rsp)         # 8-byte Spill
	movabsq	$4294967197, %rdi       # imm = 0xFFFFFF9D
	movq	%rcx, %rsi
	movq	%rcx, 112(%rsp)         # 8-byte Spill
	movq	%r10, %rbx
	callq	emit_note_after
	movq	%rax, 224(%rbx)
	movq	160(%rbx), %r15
	movq	192(%rbx), %r13
	movq	200(%rbx), %r12
.LBB2_12:                               # %if.end
	movq	%r12, %rdi
	callq	next_nonnote_insn
	movq	%rax, 216(%rsp)         # 8-byte Spill
	movl	indirect_jump_in_function(%rip), %eax
	movl	%eax, 24(%r15)
	xorq	%rax, %rax
	movl	%eax, 232(%r15)
	movl	%eax, (%r15)
	movq	$0, 16(%r15)
	movq	$0, 8(%r15)
	movq	$1, %rcx
	movl	%ecx, 124(%rbx)
	movl	%eax, 144(%r15)
	movl	%eax, 148(%r15)
	movq	$0, 120(%r15)
	movq	$0, 160(%r15)
	movl	%eax, 136(%r15)
	movl	%eax, 152(%r15)
	movq	%r13, 304(%rsp)
	testq	%r13, %r13
	je	.LBB2_18
# BB#13:
	movq	%r13, %rax
.LBB2_14:                               # %land.rhs.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	je	.LBB2_18
# BB#15:                                # %land.rhs.i
                                        #   in Loop: Header=BB2_14 Depth=1
	cmpq	$34, %rcx
	je	.LBB2_16
# BB#17:                                # %for.inc.i
                                        #   in Loop: Header=BB2_14 Depth=1
	movq	16(%rax), %rax
	testq	%rax, %rax
	movq	%rax, 304(%rsp)
	jne	.LBB2_14
	jmp	.LBB2_18
.LBB2_16:                               # %if.then.i
	movq	$1, %rax
	movl	%eax, 232(%r15)
.LBB2_18:                               # %for.end.i
	movq	%r14, 136(%rsp)         # 8-byte Spill
	movq	%rbx, 160(%rsp)         # 8-byte Spill
	movq	24(%r13), %rdi
	movq	%rdi, 304(%rsp)
	movq	24(%r12), %rax
	cmpq	%rax, %rdi
	je	.LBB2_54
# BB#19:
	movq	%r13, %rbx
	leaq	8(%r15), %rax
	movq	%rax, 200(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	$1, %r14
	.align	16, 0x90
.LBB2_20:                               # %for.body16.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_34 Depth 2
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	addq	$-32, %rcx
	andq	%r13, %rcx
	cmpq	$5, %rcx
	ja	.LBB2_52
# BB#21:                                # %for.body16.i
                                        #   in Loop: Header=BB2_20 Depth=1
	jmpq	*.LJTI2_0(,%rcx,8)
.LBB2_29:                               # %sw.bb51.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movl	20(%r15), %eax
	testq	%rax, %rax
	jne	.LBB2_41
# BB#30:                                # %if.then54.i
                                        #   in Loop: Header=BB2_20 Depth=1
	callq	pc_set
	testq	%rax, %rax
	je	.LBB2_40
# BB#31:                                # %if.then57.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movq	16(%rax), %rax
	movzbl	(%rax), %ecx
	xorq	%rdx, %rdx
	cmpq	$72, %rcx
	jne	.LBB2_33
# BB#32:                                # %if.then64.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movq	16(%rax), %rcx
	movq	24(%rax), %rdx
	movq	%rcx, %rax
.LBB2_33:                               # %do.body.preheader.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movq	global_rtl(%rip), %rcx
	.align	16, 0x90
.LBB2_34:                               # %do.body.i
                                        #   Parent Loop BB2_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %rsi
	movq	%rdx, %rax
	testq	%rsi, %rsi
	je	.LBB2_39
# BB#35:                                # %do.body.i
                                        #   in Loop: Header=BB2_34 Depth=2
	cmpq	%rcx, %rsi
	je	.LBB2_39
# BB#36:                                # %if.then75.i
                                        #   in Loop: Header=BB2_34 Depth=2
	movl	(%rsi), %edx
	movq	%rdx, %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$67, %rdi
	jne	.LBB2_40
# BB#37:                                # %if.else81.i
                                        #   in Loop: Header=BB2_34 Depth=2
	movq	8(%rsi), %rsi
	movq	216(%rsp), %rdi         # 8-byte Reload
	cmpq	%rdi, %rsi
	je	.LBB2_39
# BB#38:                                # %if.else81.i
                                        #   in Loop: Header=BB2_34 Depth=2
	andq	$268435456, %rdx        # imm = 0x10000000
	testq	%r13, %rdx
	jne	.LBB2_40
.LBB2_39:                               # %if.end95.i
                                        #   in Loop: Header=BB2_34 Depth=2
	xorq	%rdx, %rdx
	testq	%rax, %rax
	jne	.LBB2_34
	jmp	.LBB2_41
.LBB2_24:                               # %sw.bb32.i
                                        #   in Loop: Header=BB2_20 Depth=1
	testq	$67108864, %rax         # imm = 0x4000000
	jne	.LBB2_26
# BB#25:                                # %if.then36.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movl	%r14d, 144(%r15)
	movl	%r14d, 8(%r15)
	movl	%r14d, (%r15)
	jmp	.LBB2_50
.LBB2_22:                               # %sw.bb.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movl	40(%rdi), %eax
	movabsq	$4294967200, %rcx       # imm = 0xFFFFFFA0
	cmpq	%rcx, %rax
	jne	.LBB2_52
# BB#23:                                # %if.then22.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movq	160(%rsp), %rcx         # 8-byte Reload
	movl	124(%rcx), %eax
	incq	%rax
	movl	%eax, 124(%rcx)
	jmp	.LBB2_52
.LBB2_26:                               # %if.else39.i
                                        #   in Loop: Header=BB2_20 Depth=1
	callq	pure_call_p
	testq	%r13, %rax
	je	.LBB2_28
# BB#27:                                # %if.then42.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movq	200(%rsp), %rax         # 8-byte Reload
	movl	%r14d, (%rax)
.LBB2_28:                               # %if.end45.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movl	%r14d, (%r15)
	jmp	.LBB2_50
.LBB2_40:                               # %if.else97.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movl	%r14d, 20(%r15)
.LBB2_41:                               # %sw.bb101.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movq	304(%rsp), %rax
	movq	32(%rax), %rdi
	callq	volatile_refs_p
	testq	%r13, %rax
	je	.LBB2_43
# BB#42:                                # %if.then107.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movl	%r14d, 12(%r15)
.LBB2_43:                               # %if.end109.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movq	304(%rsp), %rax
	movzbl	(%rax), %ecx
	movq	32(%rax), %rdi
	cmpq	$33, %rcx
	jne	.LBB2_46
# BB#44:                                # %land.lhs.true113.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movl	(%rdi), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	jne	.LBB2_46
# BB#45:                                # %if.then126.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movl	%r14d, 16(%r15)
.LBB2_46:                               # %if.end128.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movabsq	$note_addr_stored, %rsi
	movq	%r15, %rdx
	callq	note_stores
	cmpq	$0, 160(%r15)
	jne	.LBB2_49
# BB#47:                                # %land.lhs.true134.i
                                        #   in Loop: Header=BB2_20 Depth=1
	cmpq	$0, 120(%r15)
	je	.LBB2_49
# BB#48:                                # %if.then137.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movq	304(%rsp), %rax
	movq	%rax, 160(%r15)
.LBB2_49:                               # %if.end139.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movl	flag_non_call_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB2_52
.LBB2_50:                               # %land.lhs.true141.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movq	304(%rsp), %rdi
	callq	can_throw_internal
	testq	%rax, %rax
	je	.LBB2_52
# BB#51:                                # %if.then143.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movl	%r14d, 20(%r15)
	.align	16, 0x90
.LBB2_52:                               # %for.inc146.i
                                        #   in Loop: Header=BB2_20 Depth=1
	movq	304(%rsp), %rax
	movq	24(%rax), %rdi
	movq	%rdi, 304(%rsp)
	movq	24(%r12), %rax
	cmpq	%rax, %rdi
	jne	.LBB2_20
# BB#53:                                # %for.end150.i
	movq	200(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %eax
	testq	%rax, %rax
	movq	%rbx, %r13
	jne	.LBB2_59
.LBB2_54:                               # %land.lhs.true153.i
	movq	cfun(%rip), %rax
	movzbl	424(%rax), %eax
	testq	$64, %rax
	jne	.LBB2_59
# BB#55:                                # %land.lhs.true158.i
	movl	20(%r15), %eax
	testq	%rax, %rax
	jne	.LBB2_59
# BB#56:                                # %for.cond165.preheader.i
	movq	24(%r13), %rax
	movq	%rax, 304(%rsp)
	movq	24(%r12), %rcx
	cmpq	%rcx, %rax
	je	.LBB2_59
# BB#57:
	movabsq	$insert_loop_mem, %r14
	leaq	304(%rsp), %rbx
	.align	16, 0x90
.LBB2_58:                               # %for.body170.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	for_each_rtx
	movq	304(%rsp), %rax
	movq	24(%rax), %rax
	movq	%rax, 304(%rsp)
	movq	24(%r12), %rcx
	cmpq	%rcx, %rax
	jne	.LBB2_58
.LBB2_59:                               # %if.end177.i
	movl	144(%r15), %eax
	testq	%rax, %rax
	je	.LBB2_61
# BB#60:                                # %if.then180.i
	movq	const_int_rtx+512(%rip), %rsi
	movq	$51, %rdi
	callq	gen_rtx_MEM
	movq	120(%r15), %rcx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 120(%r15)
.LBB2_61:                               # %if.end185.i
	movl	148(%r15), %eax
	testq	%rax, %rax
	je	.LBB2_63
# BB#62:                                # %if.then188.i
	movq	const_int_rtx+512(%rip), %rsi
	movq	$51, %rdi
	callq	gen_rtx_MEM
	movl	(%rax), %ecx
	orq	$67108864, %rcx         # imm = 0x4000000
	movl	%ecx, (%rax)
	movq	120(%r15), %rcx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 120(%r15)
.LBB2_63:                               # %prescan_loop.exit
	movq	128(%rsp), %r13         # 8-byte Reload
	movl	(%r13), %eax
	testq	%rax, %rax
	movq	$1, %rax
	jne	.LBB2_65
# BB#64:                                # %select.mid
	movq	$2, %rax
.LBB2_65:                               # %select.end
	movq	%rax, 192(%rsp)         # 8-byte Spill
	movq	208(%rsp), %r14         # 8-byte Reload
	movzbl	(%r14), %eax
	cmpq	$33, %rax
	movq	160(%rsp), %rbx         # 8-byte Reload
	jne	.LBB2_74
# BB#66:                                # %if.then46
	movq	%r14, %rdi
	callq	any_uncondjump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_74
# BB#67:                                # %land.lhs.true49
	movq	64(%r14), %rax
	testq	%rax, %rax
	je	.LBB2_74
# BB#68:                                # %land.lhs.true55
	movslq	8(%rax), %rax
	movslq	max_uid_for_loop(%rip), %rdx
	cmpq	%rdx, %rax
	jge	.LBB2_74
# BB#69:                                # %cond.true
	movq	88(%rsp), %rcx          # 8-byte Reload
	movslq	8(%rcx), %rcx
	cmpq	%rdx, %rcx
	jge	.LBB2_78
# BB#70:                                # %cond.true87
	movq	uid_luid(%rip), %rsi
	movl	(%rsi,%rax,4), %edi
	movslq	(%rsi,%rcx,4), %r8
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r8, %rax
	jl	.LBB2_74
# BB#71:                                # %cond.true107
	movq	112(%rsp), %rax         # 8-byte Reload
	movslq	8(%rax), %rcx
	cmpq	%rdx, %rcx
	jge	.LBB2_78
# BB#72:                                # %cond.true124
	movslq	(%rsi,%rcx,4), %rax
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rax, %rdi
	jg	.LBB2_74
# BB#73:                                # %if.then135
	movq	216(%rbx), %rdi
	callq	next_label
	movq	%rax, 208(%rbx)
	movq	64(%r14), %rax
	movq	%rax, 216(%rbx)
.LBB2_74:                               # %if.end144
	movq	216(%rbx), %rax
	movslq	8(%rax), %rcx
	movslq	max_uid_for_loop(%rip), %rdx
	cmpq	%rdx, %rcx
	jge	.LBB2_76
# BB#75:                                # %lor.lhs.false
	movzbl	(%rax), %eax
	cmpq	$36, %rax
	jne	.LBB2_76
# BB#80:                                # %if.end167
	movl	136(%r13), %esi
	addq	$16, %rsi
	movq	%rbx, %rdi
	movq	%rbx, 160(%rsp)         # 8-byte Spill
	callq	loop_regs_scan
	movq	%rbx, %r9
	movq	208(%r9), %rax
	testq	%rax, %rax
	jne	.LBB2_82
# BB#81:                                # %cond.false.i
	movq	192(%r9), %rax
.LBB2_82:                               # %for.cond.preheader.i
	movq	200(%r9), %rcx
	xorq	%rbx, %rbx
	cmpq	%rcx, %rax
	je	.LBB2_87
# BB#83:
	movabsq	$0, %rdx
	movabsq	$1, %rsi
	.align	16, 0x90
.LBB2_84:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %edi
	movzbl	rtx_class(%rdi), %edi
	cmpq	$105, %rdi
	movq	%rdx, %rdi
	jne	.LBB2_86
# BB#85:                                # %for.body.i
                                        #   in Loop: Header=BB2_84 Depth=1
	movq	%rsi, %rdi
.LBB2_86:                               # %for.body.i
                                        #   in Loop: Header=BB2_84 Depth=1
	movq	24(%rax), %rax
	addq	%rdi, %rbx
	cmpq	%rcx, %rax
	jne	.LBB2_84
.LBB2_87:                               # %count_insns_in_loop.exit
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB2_90
# BB#88:                                # %if.then171
	movq	112(%rsp), %rax         # 8-byte Reload
	movl	8(%rax), %ecx
	movq	88(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %edx
	movabsq	$.L.str37, %rsi
	xorq	%rax, %rax
	movq	%rbx, %r8
	movq	%r9, %r12
	callq	fprintf
	movq	%r12, %r9
	movq	176(%r9), %rax
	testq	%rax, %rax
	je	.LBB2_90
# BB#89:                                # %if.then180
	movq	loop_dump_stream(%rip), %rdi
	movl	8(%rax), %edx
	movabsq	$.L.str38, %rsi
	xorq	%rax, %rax
	movq	%r9, %r12
	callq	fprintf
	movq	%r12, %r9
.LBB2_90:                               # %if.end187
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	216(%r9), %rcx
	movq	200(%r9), %rdx
	movq	24(%rcx), %rax
	cmpq	%rdx, %rax
	jne	.LBB2_92
# BB#91:                                # %if.then.i2115
	movq	208(%r9), %rax
.LBB2_92:                               # %next_insn_in_loop.exit
	xorq	%r12, %r12
	cmpq	%rcx, %rax
	je	.LBB2_94
# BB#93:                                # %next_insn_in_loop.exit
	movq	%rax, %r12
.LBB2_94:                               # %next_insn_in_loop.exit
	testq	%r12, %r12
	je	.LBB2_324
# BB#95:                                # %for.body193.lr.ph
	movzbl	rtx_class+32(%rip), %edx
	andq	$255, %rdx
	movq	%rdx, 176(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	$0, 208(%rsp)           # 8-byte Folded Spill
	movq	$0, 216(%rsp)           # 8-byte Folded Spill
	movq	$0, 184(%rsp)           # 8-byte Folded Spill
	jmp	.LBB2_96
.LBB2_76:                               # %if.then156
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	movq	112(%rsp), %rax         # 8-byte Reload
	je	.LBB2_854
# BB#77:                                # %if.then158
	movl	8(%rax), %ecx
	movq	88(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %edx
	movabsq	$.L.str36, %rsi
	xorq	%rax, %rax
	callq	fprintf
	jmp	.LBB2_854
.LBB2_258:                              #   in Loop: Header=BB2_96 Depth=1
	movq	128(%rsp), %r13         # 8-byte Reload
	jmp	.LBB2_259
.LBB2_261:                              #   in Loop: Header=BB2_96 Depth=1
	movq	128(%rsp), %r13         # 8-byte Reload
	movq	%r14, %r9
.LBB2_259:                              # %for.inc1543
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	176(%rsp), %rdx         # 8-byte Reload
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB2_319
.LBB2_157:                              # %if.then558
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	192(%rbx), %rax
	movq	$3, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	leaq	(%rdx,%rdx,2), %rcx
	movq	%rcx, 200(%rsp)         # 8-byte Spill
	movq	200(%rsp), %rcx         # 8-byte Reload
	movq	8(%rax,%rcx), %rax
	movq	56(%rax), %r15
	movq	152(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx), %rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	16(%rcx), %rax
	movq	%rcx, %r13
	movq	%rax, %rdi
	callq	copy_rtx
	movq	%r15, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	replace_rtx
	movq	192(%rbx), %rcx
	movq	%rbx, %r15
	movq	200(%rsp), %rdx         # 8-byte Reload
	movq	8(%rcx,%rdx), %rcx
	movq	%rax, 56(%rcx)
	movq	%r12, %rdi
	callq	delete_insn
	movq	%r14, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	movabsq	$0, %rax
	jne	.LBB2_159
# BB#158:                               # %if.then558
                                        #   in Loop: Header=BB2_96 Depth=1
	movabsq	$1, %rax
.LBB2_159:                              # %if.then558
                                        #   in Loop: Header=BB2_96 Depth=1
	leaq	-21(%r14), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	orq	%rax, %rcx
	leaq	-45(%r14), %rax
	andq	%rdx, %rax
	cmpq	$8, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
	orq	%rcx, %rax
	leaq	-29(%r14), %rcx
	andq	%rdx, %rcx
	movq	%rdx, %r10
	cmpq	$8, %rcx
	sbbq	%rsi, %rsi
	andq	$1, %rsi
	orq	%rax, %rsi
	movq	$3, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rdi
	xorq	%r8, %r8
	jmp	.LBB2_160
.LBB2_276:                              # %if.end.i2195.land.rhs.i2186_crit_edge
                                        #   in Loop: Header=BB2_272 Depth=2
	movl	8(%rbx), %edi
.LBB2_272:                              # %land.rhs.i2186
                                        #   Parent Loop BB2_96 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rdi, %r10
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	cmpq	%rdx, %r10
	jge	.LBB2_856
# BB#273:                               # %cond.true.i
                                        #   in Loop: Header=BB2_272 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	200(%rsp), %rcx         # 8-byte Reload
	movslq	(%rcx,%rdi,4), %r10
	movq	$32, %rcx
	movq	%r9, %r11
	shlq	%cl, %r11
	movq	$32, %rcx
	sarq	%cl, %r11
	xorq	%rdi, %rdi
	cmpq	%r11, %r10
	jg	.LBB2_265
# BB#274:                               # %while.body.i2192
                                        #   in Loop: Header=BB2_272 Depth=2
	movzbl	(%rbx), %ecx
	cmpq	$36, %rcx
	je	.LBB2_264
# BB#275:                               # %if.end.i2195
                                        #   in Loop: Header=BB2_272 Depth=2
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB2_276
	jmp	.LBB2_265
.LBB2_264:                              #   in Loop: Header=BB2_96 Depth=1
	movq	$1, %rdi
.LBB2_265:                              # %lor.end1287
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	$2, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	andq	$-111, %rax
	orq	%rdx, %rax
	movb	%al, 44(%rsi)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	208(%rsp), %rdx         # 8-byte Reload
	testq	%rcx, %rdx
	je	.LBB2_277
# BB#266:                               # %lor.end1287
                                        #   in Loop: Header=BB2_96 Depth=1
	xorq	$1, %rdi
	jne	.LBB2_277
# BB#267:                               # %if.then1304
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	16(%r15), %rcx
	movzbl	2(%rcx), %ecx
	jmp	.LBB2_278
.LBB2_183:                              # %for.body685
                                        #   in Loop: Header=BB2_160 Depth=2
	movq	192(%r15), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax,%rdi)
	incq	%r8
	addq	$24, %rdi
.LBB2_160:                              # %for.cond582
                                        #   Parent Loop BB2_96 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rax
	cmpq	$52, %r14
	jg	.LBB2_181
# BB#161:                               # %cond.true585
                                        #   in Loop: Header=BB2_160 Depth=2
	movq	8(%r13), %rax
	movzbl	2(%rax), %eax
	testq	%rsi, %rsi
	je	.LBB2_176
# BB#162:                               # %cond.true609
                                        #   in Loop: Header=BB2_160 Depth=2
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB2_181
# BB#163:                               # %lor.rhs619
                                        #   in Loop: Header=BB2_160 Depth=2
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB2_165
# BB#164:                               # %lor.rhs619
                                        #   in Loop: Header=BB2_160 Depth=2
	movabsq	$1, %rax
.LBB2_165:                              # %lor.rhs619
                                        #   in Loop: Header=BB2_160 Depth=2
	incq	%rax
	jmp	.LBB2_181
.LBB2_176:                              # %cond.false632
                                        #   in Loop: Header=BB2_160 Depth=2
	cmpq	$24, %rax
	jne	.LBB2_177
# BB#179:                               # %cond.true653
                                        #   in Loop: Header=BB2_160 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB2_181
.LBB2_177:                              # %cond.false632
                                        #   in Loop: Header=BB2_160 Depth=2
	cmpq	$18, %rax
	jne	.LBB2_180
# BB#178:                               # %cond.true641
                                        #   in Loop: Header=BB2_160 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB2_181
.LBB2_180:                              # %cond.false657
                                        #   in Loop: Header=BB2_160 Depth=2
	movl	target_flags(%rip), %r9d
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %r9
	andq	$4, %r9
	leaq	3(%rax,%r9), %rax
	addq	$4, %r9
	cqto
	idivq	%r9
.LBB2_181:                              # %cond.end681
                                        #   in Loop: Header=BB2_160 Depth=2
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jl	.LBB2_183
# BB#182:                               #   in Loop: Header=BB2_96 Depth=1
	movq	%r15, %r13
	movq	160(%rsp), %r9          # 8-byte Reload
	jmp	.LBB2_297
.LBB2_277:                              # %if.else1312
                                        #   in Loop: Header=BB2_96 Depth=1
	xorq	%rcx, %rcx
.LBB2_278:                              # %if.end1314
                                        #   in Loop: Header=BB2_96 Depth=1
	movl	%ecx, 48(%rsi)
	movq	%r13, %rdi
	movl	%edi, 36(%rsi)
	movq	reg_n_info(%rip), %rcx
	andq	$148, %rax
	movb	%al, 44(%rsi)
	movq	$0, 56(%rsi)
	movq	32(%rcx,%rdi,8), %rax
	movslq	4(%rax), %rcx
	movq	uid_luid(%rip), %rdx
	movslq	(%rax), %rax
	movl	(%rdx,%rcx,4), %ecx
	movl	(%rdx,%rax,4), %eax
	subq	%rax, %rcx
	movw	%cx, 40(%rsi)
	movq	$1, %rax
	movw	%ax, 42(%rsi)
	movq	%rdi, %rax
	movq	%rdi, %rdx
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	movabsq	$0, %rax
	jne	.LBB2_280
# BB#279:                               # %if.end1314
                                        #   in Loop: Header=BB2_96 Depth=1
	movabsq	$1, %rax
.LBB2_280:                              # %if.end1314
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%rdx, %rbx
	leaq	-21(%rbx), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	orq	%rax, %rcx
	leaq	-45(%rbx), %rax
	andq	%rdx, %rax
	cmpq	$8, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
	orq	%rcx, %rax
	leaq	-29(%rbx), %rcx
	andq	%rdx, %rcx
	movq	%rdx, %r10
	cmpq	$8, %rcx
	sbbq	%rdi, %rdi
	andq	$1, %rdi
	orq	%rax, %rdi
	movq	$3, %rcx
	movq	%rbx, %rax
	movq	%rbx, %r11
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %r15
	xorq	%r9, %r9
	jmp	.LBB2_281
.LBB2_293:                              # %for.body1446
                                        #   in Loop: Header=BB2_281 Depth=2
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	192(%rax), %rax
	movl	%r10d, (%rax,%r15)
	incq	%r9
	addq	$24, %r15
.LBB2_281:                              # %for.cond1338
                                        #   Parent Loop BB2_96 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rax
	cmpq	$52, %r11
	jg	.LBB2_292
# BB#282:                               # %cond.true1341
                                        #   in Loop: Header=BB2_281 Depth=2
	movq	8(%r8), %rax
	movzbl	2(%rax), %eax
	testq	%rdi, %rdi
	je	.LBB2_287
# BB#283:                               # %cond.true1365
                                        #   in Loop: Header=BB2_281 Depth=2
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB2_292
# BB#284:                               # %lor.rhs1376
                                        #   in Loop: Header=BB2_281 Depth=2
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB2_286
# BB#285:                               # %lor.rhs1376
                                        #   in Loop: Header=BB2_281 Depth=2
	movabsq	$1, %rax
.LBB2_286:                              # %lor.rhs1376
                                        #   in Loop: Header=BB2_281 Depth=2
	incq	%rax
	jmp	.LBB2_292
.LBB2_287:                              # %cond.false1390
                                        #   in Loop: Header=BB2_281 Depth=2
	cmpq	$24, %rax
	jne	.LBB2_288
# BB#290:                               # %cond.true1412
                                        #   in Loop: Header=BB2_281 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB2_292
.LBB2_288:                              # %cond.false1390
                                        #   in Loop: Header=BB2_281 Depth=2
	cmpq	$18, %rax
	jne	.LBB2_291
# BB#289:                               # %cond.true1399
                                        #   in Loop: Header=BB2_281 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB2_292
.LBB2_291:                              # %cond.false1416
                                        #   in Loop: Header=BB2_281 Depth=2
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
.LBB2_292:                              # %cond.end1442
                                        #   in Loop: Header=BB2_281 Depth=2
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jl	.LBB2_293
# BB#294:                               # %for.end1454
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	136(%rsp), %rax         # 8-byte Reload
	cmpq	$0, (%rax)
	jne	.LBB2_298
# BB#295:                               # %if.then.i2200
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	%rsi, (%rax)
	movq	128(%rsp), %r13         # 8-byte Reload
	jmp	.LBB2_296
.LBB2_298:                              # %if.else.i2203
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	128(%rsp), %r13         # 8-byte Reload
	movq	176(%r13), %rax
	movq	%rsi, 72(%rax)
.LBB2_296:                              # %for.inc1543
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%rsi, 176(%r13)
	movq	%r14, %r9
.LBB2_297:                              # %for.inc1543
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	176(%rsp), %rdx         # 8-byte Reload
	movq	%r10, %r15
	jmp	.LBB2_319
	.align	16, 0x90
.LBB2_96:                               # %for.body193
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_118 Depth 2
                                        #     Child Loop BB2_125 Depth 2
                                        #     Child Loop BB2_132 Depth 2
                                        #     Child Loop BB2_169 Depth 2
                                        #     Child Loop BB2_160 Depth 2
                                        #     Child Loop BB2_195 Depth 2
                                        #     Child Loop BB2_209 Depth 2
                                        #     Child Loop BB2_229 Depth 2
                                        #       Child Loop BB2_232 Depth 3
                                        #     Child Loop BB2_272 Depth 2
                                        #     Child Loop BB2_281 Depth 2
	movzbl	(%r12), %eax
	cmpq	$32, %rax
	jne	.LBB2_299
# BB#97:                                # %for.body193
                                        #   in Loop: Header=BB2_96 Depth=1
	cmpq	$105, %rdx
	jne	.LBB2_299
# BB#98:                                # %cond.true206
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	32(%r12), %rbx
	movzbl	(%rbx), %eax
	cmpq	$47, %rax
	movq	%r12, %rdi
	jne	.LBB2_100
# BB#99:                                #   in Loop: Header=BB2_96 Depth=1
	movq	%rdi, %r14
	jmp	.LBB2_101
	.align	16, 0x90
.LBB2_100:                              # %cond.false218
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%r13, %r12
	movq	%r15, %r13
	movq	%rdx, %r15
	movq	%rdi, %r14
	movq	%rbx, %rsi
	movq	%r9, %rbx
	callq	single_set_2
	movq	%r15, %rdx
	movq	%r13, %r15
	movq	%r12, %r13
	movq	%rbx, %r9
	movq	%rax, %rbx
.LBB2_101:                              # %cond.end226
                                        #   in Loop: Header=BB2_96 Depth=1
	testq	%rbx, %rbx
	movq	%r14, %r12
	je	.LBB2_299
# BB#102:                               # %land.lhs.true229
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB2_299
# BB#103:                               # %land.lhs.true237
                                        #   in Loop: Header=BB2_96 Depth=1
	movl	8(%rax), %eax
	movq	192(%r13), %rcx
	leaq	(%rax,%rax,2), %rax
	movzbl	16(%rcx,%rax,8), %eax
	testq	%rax, %rax
	jne	.LBB2_299
# BB#104:                               # %if.then246
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	16(%rbx), %rax
	movq	%rax, 200(%rsp)         # 8-byte Spill
	movq	$3, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB2_105
# BB#106:                               # %if.else256
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	movq	%rax, %r14
	testq	%r14, %r14
	jne	.LBB2_108
# BB#107:                               #   in Loop: Header=BB2_96 Depth=1
	movq	$0, 144(%rsp)           # 8-byte Folded Spill
	movq	%rbx, %r13
	xorq	%rax, %rax
	jmp	.LBB2_122
	.align	16, 0x90
.LBB2_299:                              # %if.else1460
                                        #   in Loop: Header=BB2_96 Depth=1
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$67174399, %rcx         # imm = 0x400FFFF
	cmpq	$34, %rcx
	jne	.LBB2_301
# BB#300:                               #   in Loop: Header=BB2_96 Depth=1
	movq	$1, 216(%rsp)           # 8-byte Folded Spill
	jmp	.LBB2_319
	.align	16, 0x90
.LBB2_301:                              # %if.else1471
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$36, %rcx
	jne	.LBB2_303
# BB#302:                               #   in Loop: Header=BB2_96 Depth=1
	movq	$1, 208(%rsp)           # 8-byte Folded Spill
	jmp	.LBB2_319
	.align	16, 0x90
.LBB2_303:                              # %if.else1471
                                        #   in Loop: Header=BB2_96 Depth=1
	cmpq	$33, %rcx
	jne	.LBB2_311
# BB#304:                               # %land.lhs.true1486
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	64(%r12), %rax
	movq	208(%r9), %rcx
	cmpq	%rcx, %rax
	jne	.LBB2_305
# BB#306:                               # %land.lhs.true1493
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%r15, %rbx
	movq	24(%r12), %rax
	movq	24(%rax), %rax
	movq	112(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB2_307
# BB#308:                               # %land.lhs.true1502
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%r13, %r15
	movq	%rdx, %r13
	movq	%r9, %r14
	movq	%r12, %rdi
	callq	any_uncondjump_p
	testq	%rbx, %rax
	jne	.LBB2_310
# BB#309:                               #   in Loop: Header=BB2_96 Depth=1
	movq	$1, 208(%rsp)           # 8-byte Folded Spill
	movq	%r14, %r9
	movq	%r13, %rdx
	movq	%r15, %r13
	movq	%rbx, %r15
	jmp	.LBB2_319
.LBB2_305:                              #   in Loop: Header=BB2_96 Depth=1
	movq	$1, 208(%rsp)           # 8-byte Folded Spill
	jmp	.LBB2_319
.LBB2_105:                              # %if.then252
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%rbx, %r13
	movq	8(%rax), %rax
	movq	%rax, 200(%rsp)         # 8-byte Spill
	xorq	%rax, %rax
	movq	$1, 144(%rsp)           # 8-byte Folded Spill
.LBB2_122:                              # %if.end338
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	160(%rsp), %r9          # 8-byte Reload
	jmp	.LBB2_123
.LBB2_307:                              #   in Loop: Header=BB2_96 Depth=1
	movq	$1, 208(%rsp)           # 8-byte Folded Spill
	movq	%rbx, %r15
	jmp	.LBB2_319
.LBB2_108:                              # %land.lhs.true259
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	8(%r14), %rax
	movzbl	(%rax), %ecx
	xorq	%rdx, %rdx
	cmpq	$133, %rcx
	jg	.LBB2_112
# BB#109:                               # %land.lhs.true259
                                        #   in Loop: Header=BB2_96 Depth=1
	addq	$-54, %rcx
	andq	%r15, %rcx
	cmpq	$14, %rcx
	jbe	.LBB2_111
# BB#110:                               #   in Loop: Header=BB2_96 Depth=1
	xorq	%rdx, %rdx
	jmp	.LBB2_115
.LBB2_310:                              # %land.lhs.true1502.if.else1506_crit_edge
                                        #   in Loop: Header=BB2_96 Depth=1
	movl	(%r12), %eax
	movq	%r14, %r9
	movq	%r13, %rdx
	movq	%r15, %r13
	movq	%rbx, %r15
.LBB2_311:                              # %if.else1506
                                        #   in Loop: Header=BB2_96 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$37, %rax
	jne	.LBB2_319
# BB#312:                               # %if.then1511
                                        #   in Loop: Header=BB2_96 Depth=1
	movl	40(%r12), %eax
	movabsq	$4294967203, %rcx       # imm = 0xFFFFFFA3
	cmpq	%rcx, %rax
	jne	.LBB2_315
# BB#313:                               # %if.then1511
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	184(%rsp), %rcx         # 8-byte Reload
	testq	%r15, %rcx
	jne	.LBB2_315
# BB#314:                               #   in Loop: Header=BB2_96 Depth=1
	movq	$0, 184(%rsp)           # 8-byte Folded Spill
	movq	$0, 216(%rsp)           # 8-byte Folded Spill
	movq	$0, 208(%rsp)           # 8-byte Folded Spill
	jmp	.LBB2_319
.LBB2_315:                              # %if.else1521
                                        #   in Loop: Header=BB2_96 Depth=1
	movabsq	$4294967201, %rcx       # imm = 0xFFFFFFA1
	cmpq	%rcx, %rax
	jne	.LBB2_316
# BB#318:                               # %if.then1535
                                        #   in Loop: Header=BB2_96 Depth=1
	decq	184(%rsp)               # 8-byte Folded Spill
	jmp	.LBB2_319
.LBB2_316:                              # %if.else1521
                                        #   in Loop: Header=BB2_96 Depth=1
	movabsq	$4294967200, %rcx       # imm = 0xFFFFFFA0
	cmpq	%rcx, %rax
	jne	.LBB2_319
# BB#317:                               # %if.then1527
                                        #   in Loop: Header=BB2_96 Depth=1
	incq	184(%rsp)               # 8-byte Folded Spill
	jmp	.LBB2_319
.LBB2_112:                              # %land.lhs.true259
                                        #   in Loop: Header=BB2_96 Depth=1
	cmpq	$134, %rcx
	je	.LBB2_114
# BB#113:                               # %land.lhs.true259
                                        #   in Loop: Header=BB2_96 Depth=1
	cmpq	$140, %rcx
	jne	.LBB2_115
	jmp	.LBB2_114
.LBB2_111:                              # %land.lhs.true259
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	movabsq	$0, %rdx
	je	.LBB2_115
.LBB2_114:                              # %if.then323
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	$1, %rdx
	movq	%rax, 200(%rsp)         # 8-byte Spill
.LBB2_115:                              # %land.lhs.true329
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%rdx, 144(%rsp)         # 8-byte Spill
	movq	%rbx, %r13
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB2_116
# BB#117:                               # %if.then332
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	8(%r14), %rax
	movq	%rax, 200(%rsp)         # 8-byte Spill
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	movq	8(%rax), %rbx
	cmpq	%r12, %rbx
	movq	$0, 224(%rsp)
	leaq	224(%rsp), %r14
	jne	.LBB2_118
.LBB2_116:                              #   in Loop: Header=BB2_96 Depth=1
	movq	160(%rsp), %r9          # 8-byte Reload
	xorq	%rax, %rax
.LBB2_123:                              # %if.end338
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	32(%r12), %r8
	movzbl	(%r8), %ecx
	cmpq	$39, %rcx
	jne	.LBB2_128
# BB#124:                               # %for.cond347.preheader
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	8(%r8), %rdx
	movslq	(%rdx), %rcx
	testq	%rcx, %rcx
	movq	$1, %rbx
	jle	.LBB2_128
	.align	16, 0x90
.LBB2_125:                              # %for.body355
                                        #   Parent Loop BB2_96 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rdx,%rbx,8), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$48, %rcx
	jne	.LBB2_127
# BB#126:                               # %if.then368
                                        #   in Loop: Header=BB2_125 Depth=2
	movq	8(%rdx), %rdx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	movq	%r9, %r14
	callq	gen_rtx_fmt_ee
	movq	%r14, %r9
	movq	32(%r12), %r8
.LBB2_127:                              # %for.inc374
                                        #   in Loop: Header=BB2_125 Depth=2
	movq	8(%r8), %rdx
	movslq	(%rdx), %rsi
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rbx
	cmpq	%rsi, %rdi
	jl	.LBB2_125
.LBB2_128:                              # %if.end376
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	8(%r13), %r14
	movl	8(%r14), %edx
	movq	$32, %rcx
	movq	%rdx, %rax
	movl	max_reg_before_loop(%rip), %esi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rsi, %rdx
	jae	.LBB2_129
# BB#130:                               # %if.else386
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%r13, 152(%rsp)         # 8-byte Spill
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	(%rax), %ecx
	movl	8(%r12), %edx
	cmpq	%rdx, %rcx
	jne	.LBB2_167
# BB#131:                               # %for.body.i2133.preheader
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%r12, %rcx
	.align	16, 0x90
.LBB2_132:                              # %for.body.i2133
                                        #   Parent Loop BB2_96 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rcx), %edx
	addq	$-32, %rdx
	andq	%r15, %rdx
	cmpq	$4, %rdx
	ja	.LBB2_135
# BB#133:                               # %for.body.i2133
                                        #   in Loop: Header=BB2_132 Depth=2
	jmpq	*.LJTI2_1(,%rdx,8)
.LBB2_134:                              # %sw.bb.i2134
                                        #   in Loop: Header=BB2_132 Depth=2
	movl	8(%rcx), %edx
	movl	4(%rax), %esi
	cmpq	%rdx, %rsi
	je	.LBB2_136
.LBB2_135:                              # %for.inc.i2137
                                        #   in Loop: Header=BB2_132 Depth=2
	movq	24(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB2_132
	jmp	.LBB2_136
	.align	16, 0x90
.LBB2_118:                              # %while.body.i
                                        #   Parent Loop BB2_96 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rbx), %eax
	addq	$-32, %rax
	andq	%r15, %rax
	cmpq	$2, %rax
	ja	.LBB2_120
# BB#119:                               # %if.then.i2122
                                        #   in Loop: Header=BB2_118 Depth=2
	movq	32(%rbx), %rdi
	movq	200(%rsp), %rsi         # 8-byte Reload
	movq	%r14, %rdx
	callq	record_excess_regs
.LBB2_120:                              # %if.end.i
                                        #   in Loop: Header=BB2_118 Depth=2
	movq	24(%rbx), %rbx
	cmpq	%r12, %rbx
	jne	.LBB2_118
# BB#121:                               # %while.cond.while.end_crit_edge.i
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	224(%rsp), %rax
	jmp	.LBB2_122
.LBB2_166:                              # %reg_in_basic_block_p.exit
                                        #   in Loop: Header=BB2_96 Depth=1
	movl	4(%rax), %eax
	movl	8(%rcx), %ecx
	cmpq	%rcx, %rax
	je	.LBB2_136
.LBB2_167:                              # %land.lhs.true392
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	208(%rsp), %rax         # 8-byte Reload
	testq	%r15, %rax
	jne	.LBB2_129
# BB#168:                               # %lor.lhs.false394
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	216(%r9), %rbx
	jmp	.LBB2_169
.LBB2_175:                              # %for.inc.i2151
                                        #   in Loop: Header=BB2_169 Depth=2
	movq	24(%rbx), %rbx
.LBB2_169:                              # %lor.lhs.false394
                                        #   Parent Loop BB2_96 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r12, %rbx
	je	.LBB2_136
# BB#170:                               # %for.body.i2146
                                        #   in Loop: Header=BB2_169 Depth=2
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB2_173
# BB#171:                               # %land.lhs.true.i
                                        #   in Loop: Header=BB2_169 Depth=2
	movq	32(%rbx), %rsi
	movq	%r14, %rdi
	movq	%r15, %r13
	movq	%r9, %r15
	callq	reg_overlap_mentioned_p
	movq	%r15, %r9
	movq	%r13, %r15
	testq	%r15, %rax
	jne	.LBB2_172
.LBB2_173:                              # %if.end.i2149
                                        #   in Loop: Header=BB2_169 Depth=2
	movq	200(%r9), %rax
	cmpq	%rax, %rbx
	jne	.LBB2_175
# BB#174:                               # %if.then9.i
                                        #   in Loop: Header=BB2_169 Depth=2
	movq	192(%r9), %rbx
	jmp	.LBB2_175
.LBB2_136:                              # %if.else398
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%r9, %rdi
	movq	200(%rsp), %rsi         # 8-byte Reload
	callq	loop_invariant_p
	movq	%rax, %rbx
	testq	%r15, %rbx
	movq	168(%rsp), %r14         # 8-byte Reload
	je	.LBB2_247
# BB#137:                               # %land.lhs.true401
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	testq	%r14, %r14
	movabsq	$0, %r13
	je	.LBB2_139
# BB#138:                               # %lor.lhs.false404
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	%r14, %rsi
	callq	loop_invariant_p
	movq	%rax, %r13
	testq	%r15, %r13
	je	.LBB2_247
.LBB2_139:                              # %land.lhs.true408
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rsi
	movl	8(%rsi), %eax
	movq	128(%rsp), %rcx         # 8-byte Reload
	movq	192(%rcx), %rcx
	leaq	(%rax,%rax,2), %rax
	movl	(%rcx,%rax,8), %edx
	cmpq	$1, %rdx
	je	.LBB2_141
# BB#140:                               # %lor.lhs.false420
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	%r12, %rcx
	callq	consec_sets_invariant_p
	movq	%rax, 120(%rsp)         # 8-byte Spill
	testq	%r15, %rax
	je	.LBB2_247
.LBB2_141:                              # %land.lhs.true436
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	208(%rsp), %rax         # 8-byte Reload
	movq	216(%rsp), %rcx         # 8-byte Reload
	orq	%rcx, %rax
	testq	%r15, %rax
	je	.LBB2_143
# BB#142:                               # %land.lhs.true440
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	200(%rsp), %rdi         # 8-byte Reload
	callq	may_trap_p
	testq	%r15, %rax
	jne	.LBB2_247
.LBB2_143:                              # %if.then443
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%r13, 64(%rsp)          # 8-byte Spill
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	movq	%r14, %rbx
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movq	128(%rsp), %rcx         # 8-byte Reload
	movl	(%rcx), %ecx
	movslq	8(%rax), %r14
	testq	%rcx, %rcx
	je	.LBB2_184
# BB#144:                               # %land.lhs.true452
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	192(%rax), %rax
	leaq	(%r14,%r14,2), %r13
	movq	8(%rax,%r13,8), %rcx
	testq	%rcx, %rcx
	je	.LBB2_184
# BB#145:                               # %land.lhs.true452
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	const_int_rtx+512(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB2_184
# BB#146:                               # %land.lhs.true465
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	reg_n_info(%rip), %rdx
	movq	32(%rdx,%r14,8), %rdx
	movl	(%rdx), %esi
	movl	8(%r12), %edi
	cmpq	%rdi, %rsi
	jne	.LBB2_184
# BB#147:                               # %land.lhs.true473
                                        #   in Loop: Header=BB2_96 Depth=1
	movl	4(%rdx), %edx
	movl	8(%rcx), %ecx
	cmpq	%rcx, %rdx
	jne	.LBB2_184
# BB#148:                               # %land.lhs.true487
                                        #   in Loop: Header=BB2_96 Depth=1
	movl	(%rax,%r13,8), %eax
	cmpq	$1, %rax
	jne	.LBB2_184
# BB#149:                               # %land.lhs.true494
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	16(%rax), %rdi
	movzbl	(%rdi), %eax
	cmpq	$41, %rax
	je	.LBB2_184
# BB#150:                               # %land.lhs.true502
                                        #   in Loop: Header=BB2_96 Depth=1
	callq	side_effects_p
	testq	%r15, %rax
	jne	.LBB2_184
# BB#151:                               # %land.lhs.true508
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB2_184
# BB#152:                               # %land.lhs.true511
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	16(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB2_154
# BB#153:                               # %land.lhs.true519
                                        #   in Loop: Header=BB2_96 Depth=1
	movl	8(%rax), %ecx
	cmpq	$53, %rcx
	jb	.LBB2_184
.LBB2_154:                              # %land.lhs.true528
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	128(%rsp), %rcx         # 8-byte Reload
	movq	192(%rcx), %rcx
	movq	8(%rcx,%r13,8), %rcx
	movq	%rax, %rdi
	movq	%r12, %rsi
	movq	%rcx, %rdx
	callq	modified_between_p
	testq	%r15, %rax
	jne	.LBB2_184
# BB#155:                               # %land.lhs.true538
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	192(%rax), %rax
	movq	8(%rax,%r13,8), %rax
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	no_labels_between_p
	testq	%r15, %rax
	je	.LBB2_184
# BB#156:                               # %land.lhs.true545
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	192(%rax), %rax
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	8(%rdx), %rcx
	movq	16(%rdx), %rdx
	movq	8(%rax,%r13,8), %rax
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	movq	%rax, %rdx
	callq	validate_replace_rtx
	testq	%r15, %rax
	jne	.LBB2_157
.LBB2_184:                              # %if.end694
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	$80, %rdi
	callq	xmalloc
	movq	$0, 72(%rax)
	movq	%r12, (%rax)
	movq	200(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 8(%rax)
	movq	%rbx, 24(%rax)
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	8(%rdx), %rcx
	movq	%rcx, 16(%rax)
	movzbl	44(%rax), %esi
	movq	%rsi, %rcx
	andq	$253, %rcx
	movb	%cl, 44(%rax)
	movq	8(%rdx), %rcx
	movl	8(%rcx), %ecx
	movq	128(%rsp), %r13         # 8-byte Reload
	movq	192(%r13), %rdx
	leaq	(%rcx,%rcx,2), %rcx
	movl	(%rdx,%rcx,8), %ecx
	decq	%rcx
	movl	%ecx, 32(%rax)
	movq	$0, 64(%rax)
	movq	$5, %rcx
	movq	144(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	andq	$133, %rsi
	orq	%rdx, %rsi
	movb	%sil, 44(%rax)
	movq	%rax, %rbx
	movq	$3, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	movabsq	$0, %rax
	je	.LBB2_186
# BB#185:                               # %if.end694
                                        #   in Loop: Header=BB2_96 Depth=1
	movabsq	$1, %rax
.LBB2_186:                              # %if.end694
                                        #   in Loop: Header=BB2_96 Depth=1
	movzbl	44(%rbx), %edx
	movq	$7, %rcx
	shlq	%cl, %rax
	xorq	%rcx, %rcx
	movl	%ecx, 48(%rbx)
	movl	%r14d, 36(%rbx)
	movq	96(%rsp), %rcx          # 8-byte Reload
	movq	64(%rsp), %rsi          # 8-byte Reload
	orq	%rcx, %rsi
	movq	120(%rsp), %rcx         # 8-byte Reload
	orq	%rcx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$1, %rsi
	movabsq	$0, %rcx
	movq	160(%rsp), %r9          # 8-byte Reload
	jle	.LBB2_188
# BB#187:                               # %if.end694
                                        #   in Loop: Header=BB2_96 Depth=1
	movabsq	$1, %rcx
.LBB2_188:                              # %if.end694
                                        #   in Loop: Header=BB2_96 Depth=1
	andq	$126, %rdx
	orq	%rdx, %rax
	orq	%rcx, %rax
	movb	%al, 44(%rbx)
	movq	200(%r9), %rcx
	movslq	8(%rcx), %rdi
	movslq	max_uid_for_loop(%rip), %rcx
	cmpq	%rcx, %rdi
	jge	.LBB2_855
# BB#189:                               # %cond.true754
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	reg_n_info(%rip), %r8
	movq	32(%r8,%r14,8), %r10
	movq	uid_luid(%rip), %rdx
	movslq	4(%r10), %rsi
	movslq	(%rdx,%rsi,4), %rsi
	movslq	(%rdx,%rdi,4), %rdi
	cmpq	%rdi, %rsi
	movq	$1, %rdi
	jg	.LBB2_193
# BB#190:                               # %lor.rhs766
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	192(%r9), %rsi
	movslq	8(%rsi), %rdi
	cmpq	%rcx, %rdi
	jge	.LBB2_855
# BB#191:                               # %cond.true780
                                        #   in Loop: Header=BB2_96 Depth=1
	movslq	(%r10), %rcx
	movslq	(%rdx,%rcx,4), %rcx
	movslq	(%rdx,%rdi,4), %rsi
	cmpq	%rsi, %rcx
	movabsq	$0, %rdi
	jge	.LBB2_193
# BB#192:                               # %cond.true780
                                        #   in Loop: Header=BB2_96 Depth=1
	movabsq	$1, %rdi
.LBB2_193:                              # %lor.end792
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	$2, %rcx
	shlq	%cl, %rdi
	andq	$251, %rax
	orq	%rdi, %rax
	movb	%al, 44(%rbx)
	movq	$0, 56(%rbx)
	movq	32(%r8,%r14,8), %rax
	movslq	4(%rax), %rcx
	movslq	(%rax), %rax
	movl	(%rdx,%rcx,4), %ecx
	movl	(%rdx,%rax,4), %eax
	subq	%rax, %rcx
	movw	%cx, 40(%rbx)
	movq	192(%r13), %rax
	leaq	(%r14,%r14,2), %rcx
	movzbl	4(%rax,%rcx,8), %eax
	movw	%ax, 42(%rbx)
	movq	%rbx, 104(%rsp)         # 8-byte Spill
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB2_204
# BB#194:                               # %if.then821
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	$6, %rsi
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
	movq	%r12, %rdi
	callq	find_reg_note
	movq	8(%rax), %rax
	jmp	.LBB2_195
.LBB2_247:                              # %if.else1070
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	16(%rax), %rax
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB2_248
# BB#249:                               # %land.lhs.true1076
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	24(%r12), %rdi
	movzbl	(%rdi), %eax
	cmpq	$32, %rax
	movq	160(%rsp), %r9          # 8-byte Reload
	jne	.LBB2_129
# BB#250:                               # %cond.true1095
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	32(%rdi), %r15
	movzbl	(%r15), %eax
	cmpq	$47, %rax
	je	.LBB2_252
# BB#251:                               # %cond.false1113
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%r15, %rsi
	movq	%r9, %rbx
	callq	single_set_2
	movq	%rbx, %r9
	movq	%rax, %r15
.LBB2_252:                              # %cond.end1127
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	176(%rsp), %rdx         # 8-byte Reload
	movq	152(%rsp), %rbx         # 8-byte Reload
	testq	%r15, %r15
	je	.LBB2_253
# BB#254:                               # %land.lhs.true1130
                                        #   in Loop: Header=BB2_96 Depth=1
	movzbl	(%r15), %eax
	cmpq	$47, %rax
	jne	.LBB2_253
# BB#255:                               # %land.lhs.true1135
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	8(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$64, %rcx
	jne	.LBB2_253
# BB#256:                               # %land.lhs.true1143
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$63, %rcx
	jne	.LBB2_253
# BB#257:                               # %land.lhs.true1154
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	8(%rax), %rdi
	movq	8(%rbx), %rax
	cmpq	%rax, %rdi
	jne	.LBB2_258
# BB#260:                               # %land.lhs.true1169
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%r9, %r14
	movq	16(%r15), %rsi
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_261
# BB#262:                               # %if.then1178
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	8(%rbx), %rax
	movslq	8(%rax), %r13
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	192(%rax), %rax
	leaq	(%r13,%r13,2), %rcx
	movl	(%rax,%rcx,8), %eax
	cmpq	$2, %rax
	jne	.LBB2_261
# BB#263:                               # %if.then1192
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	$80, %rdi
	callq	xmalloc
	movq	%rax, %rsi
	movq	$0, 72(%rsi)
	movq	%r12, (%rsi)
	movq	8(%rbx), %rax
	movq	%rbx, %r8
	movq	%rax, 16(%rsi)
	movq	$0, 24(%rsi)
	movzbl	44(%rsi), %eax
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rsi)
	movq	$0, 64(%rsi)
	andq	$-123, %rax
	orq	$16, %rax
	movb	%al, 44(%rsi)
	movslq	8(%r12), %rdi
	movslq	max_uid_for_loop(%rip), %rdx
	cmpq	%rdx, %rdi
	jge	.LBB2_264
# BB#268:                               # %lor.lhs.false1225
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	200(%r14), %rcx
	movslq	8(%rcx), %rbx
	cmpq	%rdx, %rbx
	jge	.LBB2_863
# BB#269:                               # %cond.true1239
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%r13,8), %r9
	movq	uid_luid(%rip), %r10
	movslq	4(%r9), %rcx
	movslq	(%r10,%rcx,4), %rcx
	movslq	(%r10,%rbx,4), %rbx
	cmpq	%rbx, %rcx
	jg	.LBB2_264
# BB#270:                               # %lor.lhs.false1251
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	192(%r14), %rcx
	movslq	8(%rcx), %rbx
	cmpq	%rdx, %rbx
	jge	.LBB2_863
# BB#271:                               # %cond.true1265
                                        #   in Loop: Header=BB2_96 Depth=1
	movslq	(%r9), %rcx
	movq	%r10, 200(%rsp)         # 8-byte Spill
	movl	(%r10,%rcx,4), %r9d
	movslq	(%r10,%rbx,4), %rbx
	movq	$32, %rcx
	movq	%r9, %r10
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	cmpq	%rbx, %r10
	movq	%r12, %rbx
	jl	.LBB2_264
	jmp	.LBB2_272
.LBB2_248:                              #   in Loop: Header=BB2_96 Depth=1
	movq	128(%rsp), %r13         # 8-byte Reload
	jmp	.LBB2_226
.LBB2_253:                              #   in Loop: Header=BB2_96 Depth=1
	movq	128(%rsp), %r13         # 8-byte Reload
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB2_319
.LBB2_202:                              # %for.inc.i2162
                                        #   in Loop: Header=BB2_195 Depth=2
	movq	24(%rax), %rax
.LBB2_195:                              # %if.then821
                                        #   Parent Loop BB2_96 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r12, %rax
	je	.LBB2_203
# BB#196:                               # %for.body.i2157
                                        #   in Loop: Header=BB2_195 Depth=2
	movzbl	(%rax), %ecx
	cmpq	$32, %rcx
	jne	.LBB2_197
# BB#199:                               # %land.lhs.true.i2160
                                        #   in Loop: Header=BB2_195 Depth=2
	movq	32(%rax), %rcx
	movl	(%rcx), %ecx
	andq	$65534, %rcx            # imm = 0xFFFE
	cmpq	$48, %rcx
	movabsq	$0, %rcx
	je	.LBB2_201
# BB#200:                               # %land.lhs.true.i2160
                                        #   in Loop: Header=BB2_195 Depth=2
	movabsq	$1, %rcx
.LBB2_201:                              # %land.lhs.true.i2160
                                        #   in Loop: Header=BB2_195 Depth=2
	addq	%rcx, %rbx
	jmp	.LBB2_202
.LBB2_197:                              # %for.body.i2157
                                        #   in Loop: Header=BB2_195 Depth=2
	cmpq	$34, %rcx
	jne	.LBB2_202
# BB#198:                               # %if.then.i2158
                                        #   in Loop: Header=BB2_195 Depth=2
	addq	$10, %rbx
	movq	24(%rax), %rax
	jmp	.LBB2_195
.LBB2_203:                              # %libcall_benefit.exit
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	104(%rsp), %rcx         # 8-byte Reload
	movzbl	42(%rcx), %eax
	addq	%rbx, %rax
	movw	%ax, 42(%rcx)
.LBB2_204:                              # %for.cond828.preheader
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%r14, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	movabsq	$0, %rax
	movq	152(%rsp), %rbx         # 8-byte Reload
	jne	.LBB2_206
# BB#205:                               # %for.cond828.preheader
                                        #   in Loop: Header=BB2_96 Depth=1
	movabsq	$1, %rax
.LBB2_206:                              # %for.cond828.preheader
                                        #   in Loop: Header=BB2_96 Depth=1
	leaq	-21(%r14), %rcx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	orq	%rax, %rcx
	leaq	-45(%r14), %rax
	andq	%r15, %rax
	cmpq	$8, %rax
	sbbq	%r8, %r8
	andq	$1, %r8
	orq	%rcx, %r8
	leaq	-29(%r14), %rax
	andq	%r15, %rax
	cmpq	$8, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
	cmpq	$0, 144(%rsp)           # 8-byte Folded Reload
	movabsq	$0, %r9
	jne	.LBB2_208
# BB#207:                               # %for.cond828.preheader
                                        #   in Loop: Header=BB2_96 Depth=1
	movabsq	$1, %r9
.LBB2_208:                              # %for.cond828.preheader
                                        #   in Loop: Header=BB2_96 Depth=1
	orq	%rax, %r8
	orq	$-2, %r9
	movq	$3, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rsi
	xorq	%rdi, %rdi
	movq	104(%rsp), %r11         # 8-byte Reload
	jmp	.LBB2_209
	.align	16, 0x90
.LBB2_221:                              # %for.body936
                                        #   in Loop: Header=BB2_209 Depth=2
	movq	192(%r13), %rax
	movl	%r9d, (%rax,%rsi)
	incq	%rdi
	addq	$24, %rsi
.LBB2_209:                              # %for.cond828
                                        #   Parent Loop BB2_96 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rax
	cmpq	$52, %r14
	jg	.LBB2_220
# BB#210:                               # %cond.true831
                                        #   in Loop: Header=BB2_209 Depth=2
	movq	8(%rbx), %rax
	movzbl	2(%rax), %eax
	testq	%r8, %r8
	je	.LBB2_215
# BB#211:                               # %cond.true855
                                        #   in Loop: Header=BB2_209 Depth=2
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB2_220
# BB#212:                               # %lor.rhs866
                                        #   in Loop: Header=BB2_209 Depth=2
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB2_214
# BB#213:                               # %lor.rhs866
                                        #   in Loop: Header=BB2_209 Depth=2
	movabsq	$1, %rax
.LBB2_214:                              # %lor.rhs866
                                        #   in Loop: Header=BB2_209 Depth=2
	incq	%rax
	jmp	.LBB2_220
.LBB2_215:                              # %cond.false880
                                        #   in Loop: Header=BB2_209 Depth=2
	cmpq	$24, %rax
	jne	.LBB2_216
# BB#218:                               # %cond.true902
                                        #   in Loop: Header=BB2_209 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB2_220
.LBB2_216:                              # %cond.false880
                                        #   in Loop: Header=BB2_209 Depth=2
	cmpq	$18, %rax
	jne	.LBB2_219
# BB#217:                               # %cond.true889
                                        #   in Loop: Header=BB2_209 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB2_220
.LBB2_219:                              # %cond.false906
                                        #   in Loop: Header=BB2_209 Depth=2
	movq	%rbx, %r10
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
	movq	%r10, %rbx
	.align	16, 0x90
.LBB2_220:                              # %cond.end932
                                        #   in Loop: Header=BB2_209 Depth=2
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jl	.LBB2_221
# BB#222:                               # %for.end946
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	136(%rsp), %rax         # 8-byte Reload
	cmpq	$0, (%rax)
	jne	.LBB2_224
# BB#223:                               # %if.then.i2165
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%r11, (%rax)
	jmp	.LBB2_225
.LBB2_224:                              # %if.else.i
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	176(%r13), %rax
	movq	%r11, 72(%rax)
.LBB2_225:                              # %loop_movables_add.exit
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%r11, 176(%r13)
	movslq	32(%r11), %rax
	testq	%rax, %rax
	jle	.LBB2_226
# BB#227:                               # %if.then950
                                        #   in Loop: Header=BB2_96 Depth=1
	movzbl	44(%r11), %eax
	leaq	(%rax,%rax), %rcx
	andq	$64, %rcx
	andq	$191, %rax
	orq	%rcx, %rax
	movb	%al, 44(%r11)
	movq	%r12, %rdi
	movq	%r11, %rbx
	callq	next_nonnote_insn
	movq	%rbx, %rcx
	movslq	32(%rcx), %r14
	movq	%rax, %rbx
	movq	%rcx, %r13
	testq	%r14, %r14
	jle	.LBB2_234
# BB#228:                               # %for.body.lr.ph.i2168
                                        #   in Loop: Header=BB2_96 Depth=1
	movl	(%rbx), %eax
.LBB2_229:                              # %for.body.i2171
                                        #   Parent Loop BB2_96 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_232 Depth 3
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$37, %rax
	je	.LBB2_232
# BB#230:                               # %land.lhs.true.i2174
                                        #   in Loop: Header=BB2_229 Depth=2
	movq	$7, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB2_232
# BB#231:                               # %if.then.i2177
                                        #   in Loop: Header=BB2_229 Depth=2
	movq	8(%rax), %rbx
	.align	16, 0x90
.LBB2_232:                              # %do.body.i2179
                                        #   Parent Loop BB2_96 Depth=1
                                        #     Parent Loop BB2_229 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	24(%rbx), %rbx
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$37, %rcx
	je	.LBB2_232
# BB#233:                               # %for.inc.i2181
                                        #   in Loop: Header=BB2_229 Depth=2
	decq	%r14
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jg	.LBB2_229
.LBB2_234:                              # %skip_consec_insns.exit
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%rbx, %rdi
	callq	prev_nonnote_insn
	movq	%rax, %r12
	movq	$3, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB2_235
# BB#236:                               # %if.else977
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB2_240
# BB#237:                               # %land.lhs.true980
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$133, %rcx
	jg	.LBB2_241
# BB#238:                               # %land.lhs.true980
                                        #   in Loop: Header=BB2_96 Depth=1
	addq	$-54, %rcx
	andq	%r15, %rcx
	cmpq	$14, %rcx
	ja	.LBB2_240
# BB#239:                               # %land.lhs.true980
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB2_243
	jmp	.LBB2_240
.LBB2_235:                              # %if.then968
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	8(%rax), %rax
.LBB2_243:                              # %if.then1044
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%rax, 8(%r13)
	movzbl	44(%r13), %eax
	orq	$32, %rax
	jmp	.LBB2_244
.LBB2_241:                              # %land.lhs.true980
                                        #   in Loop: Header=BB2_96 Depth=1
	cmpq	$134, %rcx
	je	.LBB2_243
# BB#242:                               # %land.lhs.true980
                                        #   in Loop: Header=BB2_96 Depth=1
	cmpq	$140, %rcx
	je	.LBB2_243
.LBB2_240:                              # %if.else1053
                                        #   in Loop: Header=BB2_96 Depth=1
	movzbl	44(%r13), %eax
	andq	$223, %rax
.LBB2_244:                              # %if.end1058
                                        #   in Loop: Header=BB2_96 Depth=1
	movb	%al, 44(%r13)
	movq	%r13, %rbx
	movq	$3, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	movabsq	$0, %rax
	movq	128(%rsp), %r13         # 8-byte Reload
	je	.LBB2_246
# BB#245:                               # %if.end1058
                                        #   in Loop: Header=BB2_96 Depth=1
	movabsq	$1, %rax
.LBB2_246:                              # %if.end1058
                                        #   in Loop: Header=BB2_96 Depth=1
	movzbl	44(%rbx), %edx
	movq	$7, %rcx
	shlq	%cl, %rax
	andq	$127, %rdx
	orq	%rax, %rdx
	movb	%dl, 44(%rbx)
.LBB2_226:                              #   in Loop: Header=BB2_96 Depth=1
	movq	160(%rsp), %r9          # 8-byte Reload
	movq	176(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB2_319
.LBB2_172:                              #   in Loop: Header=BB2_96 Depth=1
	movq	$0, 208(%rsp)           # 8-byte Folded Spill
.LBB2_129:                              #   in Loop: Header=BB2_96 Depth=1
	movq	128(%rsp), %r13         # 8-byte Reload
	movq	176(%rsp), %rdx         # 8-byte Reload
	.align	16, 0x90
.LBB2_319:                              # %for.inc1543
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	24(%r12), %rax
	movq	200(%r9), %rcx
	cmpq	%rcx, %rax
	jne	.LBB2_321
# BB#320:                               # %if.then.i2212
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	208(%r9), %rax
.LBB2_321:                              # %next_insn_in_loop.exit2217
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	216(%r9), %rcx
	xorq	%r12, %r12
	cmpq	%rcx, %rax
	je	.LBB2_323
# BB#322:                               # %select.mid364
                                        #   in Loop: Header=BB2_96 Depth=1
	movq	%rax, %r12
.LBB2_323:                              # %select.end363
                                        #   in Loop: Header=BB2_96 Depth=1
	testq	%r12, %r12
	jne	.LBB2_96
.LBB2_324:                              # %for.end1545
	movq	136(%rsp), %r14         # 8-byte Reload
	movq	(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB2_348
	.align	16, 0x90
.LBB2_325:                              # %for.body.i2222
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_327 Depth 2
                                        #       Child Loop BB2_329 Depth 3
	movq	(%rbx), %rdi
	movq	$6, %rsi
	xorq	%rdx, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB2_334
# BB#326:                               # %if.then.i2224
                                        #   in Loop: Header=BB2_325 Depth=1
	movq	8(%rax), %rax
	movq	(%rbx), %rcx
	jmp	.LBB2_327
	.align	16, 0x90
.LBB2_333:                              # %for.inc13.i
                                        #   in Loop: Header=BB2_327 Depth=2
	movq	24(%rax), %rax
.LBB2_327:                              # %if.then.i2224
                                        #   Parent Loop BB2_325 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_329 Depth 3
	cmpq	%rcx, %rax
	je	.LBB2_334
# BB#328:                               # %for.cond7.preheader.i
                                        #   in Loop: Header=BB2_327 Depth=2
	movq	(%r14), %rdx
	cmpq	%rbx, %rdx
	je	.LBB2_333
	.align	16, 0x90
.LBB2_329:                              # %for.body9.i
                                        #   Parent Loop BB2_325 Depth=1
                                        #     Parent Loop BB2_327 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rdx), %rcx
	cmpq	%rax, %rcx
	jne	.LBB2_331
# BB#330:                               # %if.then12.i
                                        #   in Loop: Header=BB2_329 Depth=3
	movzbl	44(%rdx), %ecx
	orq	$8, %rcx
	movb	%cl, 44(%rdx)
.LBB2_331:                              # %for.inc.i2229
                                        #   in Loop: Header=BB2_329 Depth=3
	movq	72(%rdx), %rdx
	cmpq	%rbx, %rdx
	jne	.LBB2_329
# BB#332:                               # %for.inc13.i.loopexit
                                        #   in Loop: Header=BB2_327 Depth=2
	movq	(%rbx), %rcx
	jmp	.LBB2_333
	.align	16, 0x90
.LBB2_334:                              # %for.inc19.i
                                        #   in Loop: Header=BB2_325 Depth=1
	movq	72(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB2_325
# BB#335:                               # %ignore_some_movables.exit
	movq	(%r14), %rax
	testq	%rax, %rax
	je	.LBB2_348
# BB#336:
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB2_337:                              # %for.body.i2234
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_340 Depth 2
	movzbl	44(%rax), %edx
	testq	$24, %rdx
	jne	.LBB2_347
# BB#338:                               # %if.then.i2235
                                        #   in Loop: Header=BB2_337 Depth=1
	movq	72(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB2_348
# BB#339:                               # %for.body10.lr.ph.i
                                        #   in Loop: Header=BB2_337 Depth=1
	movslq	36(%rax), %rsi
	movq	reg_n_info(%rip), %rdi
	movq	32(%rdi,%rsi,8), %rsi
	movl	4(%rsi), %ebx
	.align	16, 0x90
.LBB2_340:                              # %for.body10.i
                                        #   Parent Loop BB2_337 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rdx), %rdi
	movl	8(%rdi), %edi
	movq	%rbx, %rsi
	andq	%rcx, %rsi
	cmpq	%rsi, %rdi
	jne	.LBB2_342
# BB#341:                               # %land.lhs.true12.i
                                        #   in Loop: Header=BB2_340 Depth=2
	movzbl	44(%rdx), %esi
	testq	$8, %rsi
	je	.LBB2_343
.LBB2_342:                              # %for.cond8.backedge.i
                                        #   in Loop: Header=BB2_340 Depth=2
	movq	72(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB2_340
	jmp	.LBB2_347
.LBB2_343:                              # %land.lhs.true22.i
                                        #   in Loop: Header=BB2_337 Depth=1
	movq	8(%rdx), %rsi
	movq	16(%rax), %rdi
	cmpq	%rdi, %rsi
	jne	.LBB2_346
# BB#344:                               # %land.lhs.true24.i
                                        #   in Loop: Header=BB2_337 Depth=1
	movl	32(%rdx), %esi
	testq	%rsi, %rsi
	je	.LBB2_347
# BB#345:                               # %land.lhs.true24.i
                                        #   in Loop: Header=BB2_337 Depth=1
	testq	%rdx, %rdx
	je	.LBB2_347
.LBB2_346:                              # %if.then29.i
                                        #   in Loop: Header=BB2_337 Depth=1
	movq	%rax, 64(%rdx)
	movzbl	40(%rdx), %esi
	movzbl	40(%rax), %edi
	addq	%rsi, %rdi
	movw	%di, 40(%rax)
	movzbl	42(%rdx), %edx
	movzbl	42(%rax), %esi
	addq	%rdx, %rsi
	movw	%si, 42(%rax)
	.align	16, 0x90
.LBB2_347:                              # %for.inc40.i
                                        #   in Loop: Header=BB2_337 Depth=1
	movq	72(%rax), %rax
	testq	%rax, %rax
	jne	.LBB2_337
.LBB2_348:                              # %force_movables.exit
	movslq	184(%r13), %rdi
	callq	xmalloc
	movq	168(%r13), %rbx
	movq	%r13, %r14
	movq	%rax, 216(%rsp)         # 8-byte Spill
	testq	%rbx, %rbx
	je	.LBB2_356
# BB#349:                               # %for.body.lr.ph.i2249
	movq	%rbx, %r12
	movq	216(%rsp), %r15         # 8-byte Reload
	.align	16, 0x90
.LBB2_350:                              # %for.body.i2251
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_354 Depth 2
	cmpq	$0, 56(%r12)
	jne	.LBB2_355
# BB#351:                               # %land.lhs.true.i2254
                                        #   in Loop: Header=BB2_350 Depth=1
	movl	36(%r12), %r13d
	movq	192(%r14), %rax
	leaq	(%r13,%r13,2), %rcx
	movl	4(%rax,%rcx,8), %eax
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	cmpq	$1, %rax
	jne	.LBB2_355
# BB#352:                               # %land.lhs.true4.i
                                        #   in Loop: Header=BB2_350 Depth=1
	movzbl	44(%r12), %eax
	testq	$16, %rax
	jne	.LBB2_355
# BB#353:                               # %if.then.i2260
                                        #   in Loop: Header=BB2_350 Depth=1
	movslq	184(%r14), %rdx
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	memset
	movq	$1, %rax
	movb	%al, (%r15,%r13)
	movq	%r12, %rax
	.align	16, 0x90
.LBB2_354:                              # %for.cond12.i
                                        #   Parent Loop BB2_350 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	72(%rax), %rax
	testq	%rax, %rax
	jne	.LBB2_354
	.align	16, 0x90
.LBB2_355:                              # %for.inc16.i
                                        #   in Loop: Header=BB2_350 Depth=1
	movq	72(%r12), %r12
	testq	%r12, %r12
	jne	.LBB2_350
.LBB2_356:                              # %for.end18.i
	movl	class_narrowest_mode+4(%rip), %r15d
	testq	%r15, %r15
	je	.LBB2_376
# BB#357:
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	jmp	.LBB2_358
	.align	16, 0x90
.LBB2_375:                              # %for.inc142.for.cond24.preheader_crit_edge.i
                                        #   in Loop: Header=BB2_358 Depth=1
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rbx
.LBB2_358:                              # %for.cond24.preheader.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_359 Depth 2
                                        #       Child Loop BB2_365 Depth 3
	xorq	%rsi, %rsi
	testq	%rbx, %rbx
	je	.LBB2_374
	.align	16, 0x90
.LBB2_359:                              # %for.body26.i
                                        #   Parent Loop BB2_358 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_365 Depth 3
	movzbl	44(%rbx), %eax
	andq	$20, %rax
	cmpq	$16, %rax
	jne	.LBB2_372
# BB#360:                               # %land.lhs.true39.i
                                        #   in Loop: Header=BB2_359 Depth=2
	movq	(%rbx), %rax
	movq	24(%rax), %rax
	movq	32(%rax), %rax
	movq	16(%rax), %rax
	movzbl	2(%rax), %eax
	movq	%r15, %rcx
	andq	%r8, %rcx
	cmpq	%rax, %rcx
	jne	.LBB2_372
# BB#361:                               # %if.then52.i
                                        #   in Loop: Header=BB2_359 Depth=2
	testq	%rsi, %rsi
	movq	%rbx, %rcx
	je	.LBB2_373
# BB#362:                               # %if.end69.i
                                        #   in Loop: Header=BB2_359 Depth=2
	movq	16(%rbx), %rax
	movq	16(%rsi), %rcx
	movl	(%rax), %eax
	movl	(%rcx), %ecx
	xorq	%rax, %rcx
	testq	$16711680, %rcx         # imm = 0xFF0000
	jne	.LBB2_372
# BB#363:                               # %for.cond82.preheader.i
                                        #   in Loop: Header=BB2_359 Depth=2
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rdi
	cmpq	%rbx, %rdi
	je	.LBB2_371
# BB#364:                               #   in Loop: Header=BB2_359 Depth=2
	movl	36(%rbx), %eax
	movq	reg_n_info(%rip), %r9
	movq	32(%r9,%rax,8), %rax
	movslq	(%rax), %rcx
	movq	uid_luid(%rip), %r12
	movslq	4(%rax), %rax
	movl	(%r12,%rcx,4), %r10d
	movl	(%r12,%rax,4), %r11d
	.align	16, 0x90
.LBB2_365:                              # %for.body85.i
                                        #   Parent Loop BB2_358 Depth=1
                                        #     Parent Loop BB2_359 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	cmpq	%rsi, %rdi
	je	.LBB2_368
# BB#366:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB2_365 Depth=3
	movzbl	44(%rdi), %eax
	testq	$16, %rax
	je	.LBB2_370
# BB#367:                               # %land.lhs.true94.i
                                        #   in Loop: Header=BB2_365 Depth=3
	movq	56(%rdi), %rax
	cmpq	%rsi, %rax
	jne	.LBB2_370
	.align	16, 0x90
.LBB2_368:                              # %if.then98.i
                                        #   in Loop: Header=BB2_365 Depth=3
	movl	36(%rdi), %eax
	movq	32(%r9,%rax,8), %rax
	movslq	(%rax), %rcx
	movslq	(%r12,%rcx,4), %r13
	movq	$32, %rcx
	movq	%r11, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %r13
	jg	.LBB2_370
# BB#369:                               # %lor.lhs.false109.i
                                        #   in Loop: Header=BB2_365 Depth=3
	movslq	4(%rax), %rax
	movslq	(%r12,%rax,4), %rax
	movq	$32, %rcx
	movq	%r10, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB2_372
.LBB2_370:                              # %for.inc123.i
                                        #   in Loop: Header=BB2_365 Depth=3
	movq	72(%rdi), %rdi
	cmpq	%rbx, %rdi
	jne	.LBB2_365
.LBB2_371:                              # %for.end125.i
                                        #   in Loop: Header=BB2_359 Depth=2
	movzbl	40(%rbx), %eax
	movzbl	40(%rsi), %ecx
	addq	%rax, %rcx
	movw	%cx, 40(%rsi)
	movzbl	42(%rbx), %eax
	movzbl	42(%rsi), %ecx
	addq	%rax, %rcx
	movw	%cx, 42(%rsi)
	movzbl	44(%rbx), %eax
	orq	$8, %rax
	movb	%al, 44(%rbx)
	movq	%rsi, 56(%rbx)
	.align	16, 0x90
.LBB2_372:                              # %for.inc139.i
                                        #   in Loop: Header=BB2_359 Depth=2
	movq	%rsi, %rcx
.LBB2_373:                              # %for.inc139.i
                                        #   in Loop: Header=BB2_359 Depth=2
	movq	72(%rbx), %rbx
	testq	%rbx, %rbx
	movq	%rcx, %rsi
	jne	.LBB2_359
.LBB2_374:                              # %for.inc142.i
                                        #   in Loop: Header=BB2_358 Depth=1
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movzbl	mode_wider_mode(%r15), %r15d
	testq	%r15, %r15
	jne	.LBB2_375
.LBB2_376:                              # %combine_movables.exit
	movq	216(%rsp), %rdi         # 8-byte Reload
	callq	free
	movl	optimize_size(%rip), %eax
	testq	%rax, %rax
	jne	.LBB2_615
# BB#377:                               # %if.then1547
	movq	160(%rsp), %rax         # 8-byte Reload
	movq	160(%rax), %r15
	movq	%r15, 96(%rsp)          # 8-byte Spill
	movq	192(%rax), %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movslq	184(%r15), %rbx
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	200(%rax), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	$8, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	%rax, 200(%rsp)         # 8-byte Spill
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	136(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %r12
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	$0, 176(%rsp)           # 8-byte Folded Spill
	testq	%r12, %r12
	je	.LBB2_603
# BB#378:                               # %for.body.lr.ph.i2285
	movl	n_non_fixed_regs(%rip), %eax
	movq	%rax, %rcx
	incq	%rcx
	movq	192(%rsp), %rax         # 8-byte Reload
	imulq	%rcx, %rax
	movq	%rax, 192(%rsp)         # 8-byte Spill
	movabsq	$.L.str92, %r13
	movq	$0, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB2_379
.LBB2_596:                              # %if.then1015.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	$13, %rsi
	movq	$1, %rdx
	movabsq	$.L.str104, %rdi
	callq	fwrite
	jmp	.LBB2_567
	.align	16, 0x90
.LBB2_379:                              # %for.body.i2286
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_421 Depth 2
                                        #     Child Loop BB2_431 Depth 2
                                        #       Child Loop BB2_442 Depth 3
                                        #       Child Loop BB2_444 Depth 3
                                        #     Child Loop BB2_439 Depth 2
                                        #       Child Loop BB2_464 Depth 3
                                        #         Child Loop BB2_467 Depth 4
                                        #       Child Loop BB2_535 Depth 3
                                        #     Child Loop BB2_544 Depth 2
                                        #     Child Loop BB2_572 Depth 2
                                        #       Child Loop BB2_580 Depth 3
	movq	%r12, 216(%rsp)         # 8-byte Spill
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	jne	.LBB2_381
# BB#380:                               # %for.body.if.end64_crit_edge.i
                                        #   in Loop: Header=BB2_379 Depth=1
	leaq	44(%r12), %rbx
	jmp	.LBB2_397
	.align	16, 0x90
.LBB2_381:                              # %if.then.i2294
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	(%r12), %rax
	movswq	40(%r12), %r8
	movl	36(%r12), %ecx
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movq	%r13, %rsi
	callq	fprintf
	movslq	32(%r12), %rdx
	testq	%rdx, %rdx
	jle	.LBB2_383
# BB#382:                               # %if.then8.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	loop_dump_stream(%rip), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str93, %rsi
	callq	fprintf
.LBB2_383:                              # %if.end.i2298
                                        #   in Loop: Header=BB2_379 Depth=1
	movzbl	44(%r12), %eax
	leaq	44(%r12), %rbx
	testq	$1, %rax
	je	.LBB2_385
# BB#384:                               # %if.then12.i2299
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	loop_dump_stream(%rip), %rcx
	movq	$5, %rsi
	movq	$1, %rdx
	movabsq	$.L.str94, %rdi
	callq	fwrite
	movzbl	(%rbx), %eax
.LBB2_385:                              # %if.end14.i
                                        #   in Loop: Header=BB2_379 Depth=1
	testq	$2, %rax
	je	.LBB2_387
# BB#386:                               # %if.then19.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	loop_dump_stream(%rip), %rcx
	movq	$6, %rsi
	movq	$1, %rdx
	movabsq	$.L.str95, %rdi
	callq	fwrite
	movzbl	(%rbx), %eax
.LBB2_387:                              # %if.end21.i
                                        #   in Loop: Header=BB2_379 Depth=1
	testq	$4, %rax
	je	.LBB2_389
# BB#388:                               # %if.then27.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	loop_dump_stream(%rip), %rcx
	movq	$7, %rsi
	movq	$1, %rdx
	movabsq	$.L.str96, %rdi
	callq	fwrite
	movzbl	(%rbx), %eax
.LBB2_389:                              # %if.end29.i
                                        #   in Loop: Header=BB2_379 Depth=1
	testq	$8, %rax
	je	.LBB2_391
# BB#390:                               # %if.then35.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	loop_dump_stream(%rip), %rcx
	movq	$5, %rsi
	movq	$1, %rdx
	movabsq	$.L.str97, %rdi
	callq	fwrite
	movzbl	(%rbx), %eax
.LBB2_391:                              # %if.end37.i
                                        #   in Loop: Header=BB2_379 Depth=1
	testq	$32, %rax
	je	.LBB2_393
# BB#392:                               # %if.then43.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	loop_dump_stream(%rip), %rcx
	movq	$10, %rsi
	movq	$1, %rdx
	movabsq	$.L.str98, %rdi
	callq	fwrite
.LBB2_393:                              # %if.end45.i2303
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	56(%r12), %rax
	testq	%rax, %rax
	je	.LBB2_395
# BB#394:                               # %if.then47.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	(%rax), %rax
	movq	loop_dump_stream(%rip), %rdi
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str99, %rsi
	callq	fprintf
.LBB2_395:                              # %if.end54.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	64(%r12), %rax
	testq	%rax, %rax
	je	.LBB2_397
# BB#396:                               # %if.then56.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	(%rax), %rax
	movq	loop_dump_stream(%rip), %rdi
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str100, %rsi
	callq	fprintf
.LBB2_397:                              # %if.end64.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movzbl	(%rbx), %eax
	testq	$8, %rax
	jne	.LBB2_597
# BB#398:                               # %land.lhs.true.i2305
                                        #   in Loop: Header=BB2_379 Depth=1
	testq	$1, %rax
	je	.LBB2_404
# BB#399:                               # %lor.lhs.false.i2308
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	8(%r12), %rsi
	movq	160(%rsp), %rdi         # 8-byte Reload
	callq	loop_invariant_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB2_597
# BB#400:                               # %land.lhs.true79.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	24(%r12), %rsi
	testq	%rsi, %rsi
	je	.LBB2_402
# BB#401:                               # %lor.lhs.false82.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	160(%rsp), %rdi         # 8-byte Reload
	callq	loop_invariant_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB2_597
.LBB2_402:                              # %land.lhs.true87.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movl	32(%r12), %edx
	testq	%rdx, %rdx
	je	.LBB2_404
# BB#403:                               # %lor.lhs.false91.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	(%r12), %rcx
	movq	16(%r12), %rsi
	incq	%rdx
	movq	160(%rsp), %rdi         # 8-byte Reload
	callq	consec_sets_invariant_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB2_597
	.align	16, 0x90
.LBB2_404:                              # %land.lhs.true97.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	64(%r12), %rax
	testq	%rax, %rax
	je	.LBB2_406
# BB#405:                               # %lor.lhs.false100.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movzbl	44(%rax), %eax
	testq	$8, %rax
	jne	.LBB2_406
	.align	16, 0x90
.LBB2_597:                              # %if.else1019.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB2_602
# BB#598:                               # %land.lhs.true1021.i
                                        #   in Loop: Header=BB2_379 Depth=1
	cmpq	$0, 56(%r12)
	jne	.LBB2_600
# BB#599:                               # %if.then1024.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	$8, %rsi
	movq	$1, %rdx
	movabsq	$.L.str105, %rdi
	callq	fwrite
	jmp	.LBB2_600
	.align	16, 0x90
.LBB2_406:                              # %if.then108.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	%rbx, 208(%rsp)         # 8-byte Spill
	movswq	42(%r12), %rbx
	movq	(%r12), %r13
	movq	loop_dump_stream(%rip), %rdi
	movslq	36(%r12), %r10
	movq	%r10, 152(%rsp)         # 8-byte Spill
	xorq	%rcx, %rcx
	testq	%rdi, %rdi
	je	.LBB2_408
# BB#407:                               # %if.then116.i
                                        #   in Loop: Header=BB2_379 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str101, %rsi
	movq	%rbx, %rdx
	movq	%rbx, %r14
	movq	%r10, %rbx
	callq	fprintf
	movq	%rbx, %r10
	movq	%r14, %rbx
	movq	loop_dump_stream(%rip), %rcx
.LBB2_408:                              # %if.end118.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	192(%r15), %rax
	leaq	(%r10,%r10,2), %rdx
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	movzbl	17(%rax,%rdx,8), %eax
	testq	%rax, %rax
	je	.LBB2_411
# BB#409:                               # %if.end118.i
                                        #   in Loop: Header=BB2_379 Depth=1
	testq	%rcx, %rcx
	je	.LBB2_411
# BB#410:                               # %if.then124.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	$27, %rsi
	movq	$1, %rdx
	movabsq	$.L.str102, %rdi
	movq	%r10, %r14
	callq	fwrite
	movq	%r14, %r10
.LBB2_411:                              # %if.end126.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax,%r10), %eax
	testq	%rax, %rax
	jne	.LBB2_419
# BB#412:                               # %if.end126.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movl	flag_move_all_movables(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_419
# BB#413:                               # %lor.lhs.false133.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	192(%rsp), %rax         # 8-byte Reload
	imulq	%rax, %rbx
	movq	192(%r15), %rax
	movswq	40(%r12), %rdx
	imulq	%rbx, %rdx
	movq	$32, %rcx
	movq	104(%rsp), %rsi         # 8-byte Reload
	movzbl	17(%rax,%rsi,8), %esi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rsi, %rsi
	movabsq	$0, %rcx
	je	.LBB2_415
# BB#414:                               # %lor.lhs.false133.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movabsq	$1, %rcx
.LBB2_415:                              # %lor.lhs.false133.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	72(%rsp), %rsi          # 8-byte Reload
	shlq	%cl, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jge	.LBB2_419
# BB#416:                               # %lor.lhs.false147.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	64(%r12), %rcx
	testq	%rcx, %rcx
	je	.LBB2_594
# BB#417:                               # %land.lhs.true150.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movzbl	44(%rcx), %edx
	testq	$8, %rdx
	je	.LBB2_594
# BB#418:                               # %land.lhs.true158.i2317
                                        #   in Loop: Header=BB2_379 Depth=1
	movl	36(%rcx), %ecx
	leaq	(%rcx,%rcx,2), %rcx
	movl	4(%rax,%rcx,8), %eax
	cmpq	$1, %rax
	jne	.LBB2_594
.LBB2_419:                              # %if.then166.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	208(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	testq	$16, %rax
	je	.LBB2_429
# BB#420:                               # %land.lhs.true172.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	56(%r12), %rcx
	testq	%rcx, %rcx
	jne	.LBB2_421
.LBB2_429:                              # %if.else.i2322
                                        #   in Loop: Header=BB2_379 Depth=1
	movl	32(%r12), %r14d
	testq	$32, %rax
	jne	.LBB2_430
# BB#437:                               # %for.cond316.preheader.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB2_539
# BB#438:                               # %for.body319.lr.ph.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	$0, 144(%rsp)           # 8-byte Folded Spill
	jmp	.LBB2_439
	.align	16, 0x90
.LBB2_421:                              # %for.cond176.for.cond176_crit_edge.i
                                        #   Parent Loop BB2_379 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, %rbx
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.LBB2_421
# BB#422:                               # %for.end.i2321
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	%r10, 152(%rsp)         # 8-byte Spill
	movq	(%r12), %rax
	movq	(%rbx), %rcx
	movq	32(%rax), %rax
	movq	32(%rcx), %rcx
	movq	8(%rax), %rdi
	movq	8(%rcx), %rsi
	callq	gen_move_insn
	movq	160(%rsp), %rcx         # 8-byte Reload
	movq	192(%rcx), %rsi
	testq	%rsi, %rsi
	je	.LBB2_423
# BB#424:                               # %loop_insn_hoist.exit.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	%r12, %r13
	movq	%rax, %rdi
	callq	emit_insn_before
	movq	(%r13), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, %r14
	movq	%rcx, 56(%r14)
	movq	(%r13), %rax
	movq	(%rbx), %rcx
	movq	32(%rax), %rax
	movq	32(%rcx), %rcx
	movq	8(%rax), %rbx
	movq	8(%rcx), %rdx
	movq	regs_may_share(%rip), %rcx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, regs_may_share(%rip)
	movq	(%r13), %rdi
	callq	delete_insn
	movq	176(%rsp), %rax         # 8-byte Reload
	testq	%rax, %rax
	movq	%r14, %rbx
	je	.LBB2_426
# BB#425:                               # %loop_insn_hoist.exit.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	%rax, %rbx
.LBB2_426:                              # %loop_insn_hoist.exit.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB2_427
# BB#428:                               # %if.then226.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movl	8(%r14), %edx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	callq	fprintf
.LBB2_427:                              #   in Loop: Header=BB2_379 Depth=1
	movq	%rbx, 176(%rsp)         # 8-byte Spill
	movq	152(%rsp), %r10         # 8-byte Reload
	jmp	.LBB2_540
.LBB2_430:                              # %for.cond241.preheader.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	%r10, 152(%rsp)         # 8-byte Spill
	movq	%r12, %rbx
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB2_448
	.align	16, 0x90
.LBB2_431:                              # %for.body244.i
                                        #   Parent Loop BB2_379 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_442 Depth 3
                                        #       Child Loop BB2_444 Depth 3
	movzbl	(%r13), %eax
	cmpq	$37, %rax
	je	.LBB2_434
# BB#432:                               # %land.lhs.true249.i
                                        #   in Loop: Header=BB2_431 Depth=2
	movq	$7, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB2_434
# BB#433:                               # %if.then252.i
                                        #   in Loop: Header=BB2_431 Depth=2
	movq	8(%rax), %r13
.LBB2_434:                              # %if.end256.i
                                        #   in Loop: Header=BB2_431 Depth=2
	movzbl	(%r13), %eax
	cmpq	$37, %rax
	je	.LBB2_443
# BB#435:                               # %land.lhs.true261.i
                                        #   in Loop: Header=BB2_431 Depth=2
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB2_443
# BB#436:                               # %if.then264.i
                                        #   in Loop: Header=BB2_431 Depth=2
	movq	8(%rax), %rax
	jmp	.LBB2_442
	.align	16, 0x90
.LBB2_441:                              # %while.body.i2323
                                        #   in Loop: Header=BB2_442 Depth=3
	movq	%rax, %rdi
	callq	delete_insn
.LBB2_442:                              # %while.body.i2323
                                        #   Parent Loop BB2_379 Depth=1
                                        #     Parent Loop BB2_431 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	cmpq	%r13, %rax
	jne	.LBB2_441
.LBB2_443:                              # %if.end271.i
                                        #   in Loop: Header=BB2_431 Depth=2
	movq	%r13, %rdi
	callq	delete_insn
	xorq	%rdx, %rdx
	testq	%rax, %rax
	je	.LBB2_447
	.align	16, 0x90
.LBB2_444:                              # %land.rhs.i2325
                                        #   Parent Loop BB2_379 Depth=1
                                        #     Parent Loop BB2_431 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB2_445
# BB#446:                               # %while.body279.i
                                        #   in Loop: Header=BB2_444 Depth=3
	movq	24(%rax), %rax
	testq	%rax, %rax
	movq	%rax, 24(%r13)
	jne	.LBB2_444
	jmp	.LBB2_447
	.align	16, 0x90
.LBB2_445:                              #   in Loop: Header=BB2_431 Depth=2
	movq	%rax, %rdx
.LBB2_447:                              # %for.inc287.i
                                        #   in Loop: Header=BB2_431 Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%r14
	testq	%rax, %rax
	movq	%rdx, %r13
	jg	.LBB2_431
.LBB2_448:                              # %for.end288.i
                                        #   in Loop: Header=BB2_379 Depth=1
	callq	start_sequence
	movq	8(%rbx), %rsi
	movq	16(%rbx), %rdi
	callq	emit_move_insn
	callq	get_insns
	movq	%rax, %r14
	callq	gen_sequence
	movq	%rax, %r12
	callq	end_sequence
	movq	8(%rbx), %rdi
	movq	%rbx, %r13
	movq	%r14, %rsi
	callq	add_label_notes
	movq	160(%rsp), %rax         # 8-byte Reload
	movq	192(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB2_449
# BB#450:                               # %loop_insn_hoist.exit1391.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	%r12, %rdi
	callq	emit_insn_before
	movq	%rax, %rbx
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB2_454
# BB#451:                               # %if.then298.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	208(%rsp), %rax         # 8-byte Reload
	movsbq	(%rax), %rax
	testq	%rax, %rax
	movq	$3, %rsi
	js	.LBB2_453
# BB#452:                               # %select.mid431
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	$4, %rsi
.LBB2_453:                              # %select.end430
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	8(%r13), %rdx
	movq	%rbx, %rdi
	callq	set_unique_reg_note
.LBB2_454:                              # %if.end306.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB2_456
# BB#455:                               # %if.then308.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movl	8(%rbx), %edx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	callq	fprintf
.LBB2_456:                              # %if.end313.i
                                        #   in Loop: Header=BB2_379 Depth=1
	addq	$-3, 192(%rsp)          # 8-byte Folded Spill
	movq	152(%rsp), %r10         # 8-byte Reload
	jmp	.LBB2_540
.LBB2_594:                              # %if.else1013.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	jne	.LBB2_596
# BB#595:                               #   in Loop: Header=BB2_379 Depth=1
	movabsq	$.L.str92, %rbx
	movq	%rbx, %r13
	jmp	.LBB2_602
.LBB2_512:                              # %if.then548.i
                                        #   in Loop: Header=BB2_439 Depth=2
	callq	start_sequence
	movq	8(%r12), %rsi
	movq	16(%r12), %rdi
	callq	emit_move_insn
	callq	get_insns
	movq	%rax, %r15
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	8(%r12), %rdi
	movq	%r15, %rsi
	callq	add_label_notes
	movq	160(%rsp), %rax         # 8-byte Reload
	movq	192(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB2_513
# BB#514:                               # %loop_insn_hoist.exit1407.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	%rbx, %rdi
	callq	emit_insn_before
	movq	%rax, %rbx
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB2_522
# BB#515:                               # %if.then559.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	208(%rsp), %rax         # 8-byte Reload
	movsbq	(%rax), %rax
	testq	%rax, %rax
	movq	$3, %rsi
	js	.LBB2_517
# BB#516:                               # %select.mid466
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	$4, %rsi
.LBB2_517:                              # %select.end465
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	8(%r12), %rdx
	movq	%rbx, %rdi
	callq	set_unique_reg_note
	jmp	.LBB2_522
	.align	16, 0x90
.LBB2_439:                              # %for.body319.i
                                        #   Parent Loop BB2_379 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_464 Depth 3
                                        #         Child Loop BB2_467 Depth 4
                                        #       Child Loop BB2_535 Depth 3
	movzbl	(%r13), %eax
	cmpq	$37, %rax
	jne	.LBB2_457
# BB#440:                               #   in Loop: Header=BB2_439 Depth=2
	movq	176(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB2_459
	.align	16, 0x90
.LBB2_457:                              # %land.lhs.true326.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	$7, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	movq	176(%rsp), %rbx         # 8-byte Reload
	je	.LBB2_459
# BB#458:                               # %if.then329.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	8(%rax), %r13
.LBB2_459:                              # %if.end333.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movzbl	(%r13), %eax
	cmpq	$37, %rax
	jne	.LBB2_461
# BB#460:                               #   in Loop: Header=BB2_439 Depth=2
	movq	%rbx, %rcx
	jmp	.LBB2_499
	.align	16, 0x90
.LBB2_461:                              # %land.lhs.true338.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB2_463
# BB#462:                               #   in Loop: Header=BB2_439 Depth=2
	movq	%rbx, %rcx
	jmp	.LBB2_499
.LBB2_463:                              # %if.then341.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	%r14, 48(%rsp)          # 8-byte Spill
	movq	8(%rax), %r15
	xorq	%rcx, %rcx
	cmpq	%r13, %r15
	movabsq	$0, %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movabsq	$0, %r14
	je	.LBB2_497
	.align	16, 0x90
.LBB2_464:                              # %for.body348.i
                                        #   Parent Loop BB2_379 Depth=1
                                        #     Parent Loop BB2_439 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB2_467 Depth 4
	movzbl	(%r15), %eax
	cmpq	$37, %rax
	jne	.LBB2_466
# BB#465:                               # %for.body348.for.inc480_crit_edge.i
                                        #   in Loop: Header=BB2_464 Depth=3
	addq	$24, %r15
	jmp	.LBB2_496
	.align	16, 0x90
.LBB2_466:                              # %if.end354.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movq	%rcx, %r12
	movq	24(%r15), %rax
	movq	32(%r15), %rbx
	cmpq	%r13, %rax
	movq	%r13, %rcx
	je	.LBB2_474
	.align	16, 0x90
.LBB2_467:                              # %for.body364.i
                                        #   Parent Loop BB2_379 Depth=1
                                        #     Parent Loop BB2_439 Depth=2
                                        #       Parent Loop BB2_464 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	je	.LBB2_472
# BB#468:                               # %for.body364.i
                                        #   in Loop: Header=BB2_467 Depth=4
	cmpq	$32, %rcx
	jne	.LBB2_469
# BB#470:                               # %land.lhs.true369.i
                                        #   in Loop: Header=BB2_467 Depth=4
	movq	32(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$48, %rcx
	jne	.LBB2_471
.LBB2_472:                              # %for.cond361.backedge.i
                                        #   in Loop: Header=BB2_467 Depth=4
	movq	24(%rax), %rax
	cmpq	%r13, %rax
	jne	.LBB2_467
# BB#473:                               #   in Loop: Header=BB2_464 Depth=3
	movq	%r13, %rcx
	jmp	.LBB2_474
.LBB2_469:                              #   in Loop: Header=BB2_464 Depth=3
	movq	%rax, %rcx
	jmp	.LBB2_474
.LBB2_471:                              #   in Loop: Header=BB2_464 Depth=3
	movq	%rax, %rcx
	.align	16, 0x90
.LBB2_474:                              # %for.end388.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movzbl	(%rcx), %eax
	cmpq	$34, %rax
	jne	.LBB2_481
# BB#475:                               # %land.lhs.true393.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movzbl	(%rbx), %eax
	cmpq	$47, %rax
	jne	.LBB2_481
# BB#476:                               # %land.lhs.true398.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movq	8(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB2_481
# BB#477:                               # %land.lhs.true406.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB2_481
# BB#478:                               # %if.then409.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movq	16(%rbx), %rcx
	movq	%rcx, 120(%rsp)         # 8-byte Spill
	movzbl	(%rcx), %ecx
	cmpq	$61, %rcx
	je	.LBB2_480
# BB#479:                               # %if.then417.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movq	8(%rbx), %rcx
	movq	%rcx, 120(%rsp)         # 8-byte Spill
.LBB2_480:                              # %if.end421.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movq	8(%rax), %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	%r15, %r12
	.align	16, 0x90
.LBB2_481:                              # %if.end425.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movzbl	(%r15), %eax
	cmpq	$34, %rax
	jne	.LBB2_485
# BB#482:                               # %if.end425.i
                                        #   in Loop: Header=BB2_464 Depth=3
	cmpq	$0, 144(%rsp)           # 8-byte Folded Reload
	je	.LBB2_485
# BB#483:                               # %land.lhs.true433.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	reg_referenced_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_485
# BB#484:                               # %if.then436.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	144(%rsp), %rsi         # 8-byte Reload
	callq	gen_move_insn
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	emit_insn_after
.LBB2_485:                              # %if.end439.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movzbl	(%r15), %eax
	movq	160(%rsp), %rcx         # 8-byte Reload
	movq	192(%rcx), %rsi
	cmpq	$34, %rax
	leaq	24(%r15), %rax
	movq	%rax, 184(%rsp)         # 8-byte Spill
	jne	.LBB2_488
# BB#486:                               # %if.then444.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movq	%rbx, %rdi
	callq	emit_call_insn_before
	movq	64(%r15), %rdi
	movq	%rax, %rbx
	testq	%rdi, %rdi
	je	.LBB2_491
# BB#487:                               # %if.then450.i
                                        #   in Loop: Header=BB2_464 Depth=3
	callq	copy_rtx
	movq	%rax, 64(%rbx)
	jmp	.LBB2_491
	.align	16, 0x90
.LBB2_488:                              # %if.else459.i
                                        #   in Loop: Header=BB2_464 Depth=3
	testq	%rsi, %rsi
	je	.LBB2_489
# BB#490:                               # %loop_insn_hoist.exit1396.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movq	%rbx, %rdi
	callq	emit_insn_before
	movq	%rax, %rbx
.LBB2_491:                              # %if.end461.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movq	%r12, %rcx
	testq	%r14, %r14
	movq	%r14, %rax
	movq	%rbx, %r14
	je	.LBB2_493
# BB#492:                               # %if.end461.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movq	%rax, %r14
.LBB2_493:                              # %if.end461.i
                                        #   in Loop: Header=BB2_464 Depth=3
	cmpq	%rcx, %r15
	movq	%rbx, %r12
	je	.LBB2_495
# BB#494:                               # %if.end461.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movq	%rcx, %r12
.LBB2_495:                              # %if.end461.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movq	56(%r15), %rax
	movq	%rax, 56(%rbx)
	movq	$0, 56(%r15)
	movq	%r15, %rdi
	callq	delete_insn
	movq	%r12, %rcx
	movq	216(%rsp), %r12         # 8-byte Reload
	movq	184(%rsp), %r15         # 8-byte Reload
.LBB2_496:                              # %for.inc480.i
                                        #   in Loop: Header=BB2_464 Depth=3
	movq	(%r15), %r15
	cmpq	%r13, %r15
	jne	.LBB2_464
.LBB2_497:                              # %for.end484.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	%r14, 144(%rsp)         # 8-byte Spill
	movq	176(%rsp), %rax         # 8-byte Reload
	testq	%rax, %rax
	movq	%r14, %rcx
	movq	48(%rsp), %r14          # 8-byte Reload
	je	.LBB2_499
# BB#498:                               # %for.end484.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	%rax, %rcx
	.align	16, 0x90
.LBB2_499:                              # %if.end489.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movl	48(%r12), %eax
	testq	%rax, %rax
	jne	.LBB2_500
# BB#507:                               # %if.else515.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	%rcx, 184(%rsp)         # 8-byte Spill
	movzbl	(%r13), %eax
	cmpq	$34, %rax
	movq	160(%rsp), %rdx         # 8-byte Reload
	jne	.LBB2_510
# BB#508:                               # %if.then520.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	32(%r13), %rdi
	movq	192(%rdx), %rsi
	callq	emit_call_insn_before
	movq	64(%r13), %rdi
	movq	%rax, %rbx
	testq	%rdi, %rdi
	je	.LBB2_522
# BB#509:                               # %if.then529.i
                                        #   in Loop: Header=BB2_439 Depth=2
	callq	copy_rtx
	movq	%rax, 64(%rbx)
	jmp	.LBB2_522
	.align	16, 0x90
.LBB2_500:                              # %if.then492.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	%rcx, 184(%rsp)         # 8-byte Spill
	movq	16(%r12), %rbx
	callq	start_sequence
	movslq	48(%r12), %rax
	movzbl	mode_bitsize(%rax,%rax), %ecx
	movzbl	2(%rbx), %r15d
	movq	$1, %rsi
	shlq	%cl, %rsi
	decq	%rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$83, %rsi
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	%r15, %rdi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	movq	%rbx, %r8
	callq	expand_simple_binop
	testq	%rax, %rax
	je	.LBB2_501
# BB#502:                               # %if.end507.i
                                        #   in Loop: Header=BB2_439 Depth=2
	cmpq	%rbx, %rax
	je	.LBB2_504
# BB#503:                               # %if.then510.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB2_504:                              # %if.end512.i
                                        #   in Loop: Header=BB2_439 Depth=2
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	160(%rsp), %rax         # 8-byte Reload
	movq	192(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB2_505
# BB#506:                               # %loop_insn_hoist.exit1401.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	%rbx, %rdi
	jmp	.LBB2_521
.LBB2_510:                              # %if.else538.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movl	32(%r12), %eax
	movq	%r14, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	%rax, %rcx
	jne	.LBB2_518
# BB#511:                               # %land.lhs.true542.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	208(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	testq	$64, %rax
	jne	.LBB2_512
.LBB2_518:                              # %if.else569.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	192(%rdx), %rsi
	testq	%rsi, %rsi
	je	.LBB2_519
# BB#520:                               # %loop_insn_hoist.exit1412.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	32(%r13), %rdi
.LBB2_521:                              # %if.end576.i
                                        #   in Loop: Header=BB2_439 Depth=2
	callq	emit_insn_before
	movq	%rax, %rbx
.LBB2_522:                              # %if.end576.i
                                        #   in Loop: Header=BB2_439 Depth=2
	cmpq	$0, 56(%rbx)
	jne	.LBB2_526
# BB#523:                               # %if.then582.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	56(%r13), %rax
	movq	%rax, 56(%rbx)
	movq	$0, 56(%r13)
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB2_526
# BB#524:                               # %land.lhs.true594.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	8(%r15), %rsi
	movq	160(%rsp), %rdi         # 8-byte Reload
	callq	loop_invariant_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_526
# BB#525:                               # %if.then600.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	remove_note
	.align	16, 0x90
.LBB2_526:                              # %if.end602.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	184(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rcx
	movq	%rbx, %rax
	je	.LBB2_528
# BB#527:                               # %if.end602.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	%rcx, %rax
.LBB2_528:                              # %if.end602.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	%rax, 176(%rsp)         # 8-byte Spill
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	movq	96(%rsp), %r15          # 8-byte Reload
	je	.LBB2_530
# BB#529:                               # %if.then608.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movl	8(%rbx), %edx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	callq	fprintf
.LBB2_530:                              # %if.end613.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB2_532
# BB#531:                               # %if.then616.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	%rdi, 8(%rax)
	movq	$7, %rsi
	xorq	%rdx, %rdx
	callq	find_reg_note
	movq	%rbx, 8(%rax)
.LBB2_532:                              # %if.end624.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	%r13, %rdi
	callq	delete_insn
	movq	24(%r13), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	jne	.LBB2_534
# BB#533:                               #   in Loop: Header=BB2_439 Depth=2
	movq	152(%rsp), %r10         # 8-byte Reload
	jmp	.LBB2_538
.LBB2_534:                              # %land.rhs631.lr.ph.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	152(%rsp), %r10         # 8-byte Reload
	.align	16, 0x90
.LBB2_535:                              # %land.rhs631.i
                                        #   Parent Loop BB2_379 Depth=1
                                        #     Parent Loop BB2_439 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rcx), %edx
	cmpq	$37, %rdx
	jne	.LBB2_536
# BB#537:                               # %while.body637.i
                                        #   in Loop: Header=BB2_535 Depth=3
	movq	24(%rcx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, 24(%r13)
	jne	.LBB2_535
	jmp	.LBB2_538
.LBB2_536:                              #   in Loop: Header=BB2_439 Depth=2
	movq	%rcx, %rax
.LBB2_538:                              # %for.inc645.i
                                        #   in Loop: Header=BB2_439 Depth=2
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%r14
	testq	%rdx, %rdx
	movq	%rax, %r13
	jg	.LBB2_439
.LBB2_539:                              # %for.end647.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	%r12, %r13
	addq	$-3, 192(%rsp)          # 8-byte Folded Spill
.LBB2_540:                              # %if.end650.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	$1, %rcx
	movb	%cl, (%rax,%r10)
	movq	192(%r15), %rax
	movq	104(%rsp), %rdx         # 8-byte Reload
	movb	%cl, 17(%rax,%rdx,8)
	movq	208(%rsp), %rdx         # 8-byte Reload
	movzbl	(%rdx), %ecx
	testq	$16, %rcx
	jne	.LBB2_559
# BB#541:                               # %for.cond664.preheader.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	%rdx, %r9
	movq	%r10, %rcx
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	movabsq	$0, %rcx
	movabsq	$0, %rsi
	jne	.LBB2_543
# BB#542:                               # %for.cond664.preheader.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movabsq	$1, %rcx
.LBB2_543:                              # %for.cond664.preheader.i
                                        #   in Loop: Header=BB2_379 Depth=1
	leaq	-21(%r10), %rdx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rdx
	cmpq	$8, %rdx
	sbbq	%rdx, %rdx
	andq	$1, %rdx
	orq	%rcx, %rdx
	leaq	-45(%r10), %rcx
	andq	%rdi, %rcx
	cmpq	$8, %rcx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	orq	%rdx, %rcx
	leaq	-29(%r10), %rdx
	andq	%rdi, %rdx
	cmpq	$8, %rdx
	sbbq	%r8, %r8
	andq	$1, %r8
	orq	%rcx, %r8
	leaq	(%r10,%r10,2), %rcx
	leaq	(%rax,%rcx,8), %rdi
	xorq	%rbx, %rbx
	jmp	.LBB2_544
	.align	16, 0x90
.LBB2_557:                              # %for.body757.i
                                        #   in Loop: Header=BB2_544 Depth=2
	movl	%esi, (%rdi)
	incq	%rbx
	addq	$24, %rdi
.LBB2_544:                              # %for.cond664.i
                                        #   Parent Loop BB2_379 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rax
	cmpq	$52, %r10
	jg	.LBB2_556
# BB#545:                               # %cond.true667.i
                                        #   in Loop: Header=BB2_544 Depth=2
	movq	16(%r13), %rax
	movzbl	2(%rax), %eax
	testq	%r8, %r8
	je	.LBB2_550
# BB#546:                               # %cond.true691.i
                                        #   in Loop: Header=BB2_544 Depth=2
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB2_556
# BB#547:                               # %lor.rhs.i
                                        #   in Loop: Header=BB2_544 Depth=2
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB2_549
# BB#548:                               # %lor.rhs.i
                                        #   in Loop: Header=BB2_544 Depth=2
	movabsq	$1, %rax
.LBB2_549:                              # %lor.rhs.i
                                        #   in Loop: Header=BB2_544 Depth=2
	incq	%rax
	jmp	.LBB2_556
	.align	16, 0x90
.LBB2_550:                              # %cond.false709.i
                                        #   in Loop: Header=BB2_544 Depth=2
	cmpq	$24, %rax
	jne	.LBB2_551
# BB#553:                               # %cond.true726.i
                                        #   in Loop: Header=BB2_544 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB2_556
.LBB2_551:                              # %cond.false709.i
                                        #   in Loop: Header=BB2_544 Depth=2
	cmpq	$18, %rax
	jne	.LBB2_554
# BB#552:                               # %cond.true716.i
                                        #   in Loop: Header=BB2_544 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB2_555
.LBB2_554:                              # %cond.false730.i
                                        #   in Loop: Header=BB2_544 Depth=2
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB2_555:                              # %cond.end753.i
                                        #   in Loop: Header=BB2_544 Depth=2
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB2_556:                              # %cond.end753.i
                                        #   in Loop: Header=BB2_544 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jl	.LBB2_557
# BB#558:                               # %if.end764.loopexit.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	%r9, %rdx
	movzbl	(%rdx), %ecx
.LBB2_559:                              # %if.end764.i
                                        #   in Loop: Header=BB2_379 Depth=1
	orq	$8, %rcx
	movb	%cl, (%rdx)
	movq	64(%rsp), %rax          # 8-byte Reload
	movslq	8(%rax), %rcx
	movslq	max_uid_for_loop(%rip), %rsi
	cmpq	%rsi, %rcx
	jge	.LBB2_857
# BB#560:                               # %cond.true778.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r10,8), %rax
	movq	uid_luid(%rip), %rdx
	movslq	(%rax), %rdi
	movslq	(%rdx,%rdi,4), %rdi
	movslq	(%rdx,%rcx,4), %rbx
	cmpq	%rbx, %rdi
	jle	.LBB2_562
# BB#561:                               # %if.then789.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movl	%ecx, (%rax)
	movl	max_uid_for_loop(%rip), %esi
.LBB2_562:                              # %if.end798.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	56(%rsp), %rcx          # 8-byte Reload
	movslq	8(%rcx), %rdi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdi
	jge	.LBB2_858
# BB#563:                               # %cond.true810.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movslq	4(%rax), %rcx
	movslq	(%rdx,%rcx,4), %rcx
	movslq	(%rdx,%rdi,4), %rdx
	cmpq	%rdx, %rcx
	jge	.LBB2_565
# BB#564:                               # %if.then821.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movl	%edi, 4(%rax)
.LBB2_565:                              # %if.end830.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	208(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	testq	$16, %rax
	jne	.LBB2_566
# BB#568:                               # %for.cond839.preheader.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB2_566
# BB#569:                               # %for.body841.lr.ph.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	%r10, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	movabsq	$0, %rax
	jne	.LBB2_571
# BB#570:                               # %for.body841.lr.ph.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movabsq	$1, %rax
.LBB2_571:                              # %for.body841.lr.ph.i
                                        #   in Loop: Header=BB2_379 Depth=1
	leaq	-21(%r10), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	orq	%rax, %rcx
	leaq	-45(%r10), %rax
	andq	%rdx, %rax
	cmpq	$8, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
	orq	%rcx, %rax
	leaq	-29(%r10), %rcx
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	sbbq	%r14, %r14
	andq	$1, %r14
	orq	%rax, %r14
	.align	16, 0x90
.LBB2_572:                              # %for.body841.i
                                        #   Parent Loop BB2_379 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_580 Depth 3
	movq	56(%rbx), %rax
	cmpq	%r13, %rax
	jne	.LBB2_593
# BB#573:                               # %if.then845.i
                                        #   in Loop: Header=BB2_572 Depth=2
	movq	16(%r13), %rsi
	movq	16(%rbx), %rax
	movzbl	2(%rsi), %ecx
	movzbl	2(%rax), %edi
	movq	%r10, %r12
	cmpq	%rdi, %rcx
	jne	.LBB2_575
# BB#574:                               # %if.then857.i
                                        #   in Loop: Header=BB2_572 Depth=2
	movl	36(%rbx), %eax
	movq	200(%rsp), %rcx         # 8-byte Reload
	movq	%rsi, (%rcx,%rax,8)
	jmp	.LBB2_576
	.align	16, 0x90
.LBB2_575:                              # %if.else862.i
                                        #   in Loop: Header=BB2_572 Depth=2
	callq	gen_lowpart_common
	movl	36(%rbx), %ecx
	movq	200(%rsp), %rdx         # 8-byte Reload
	movq	%rax, (%rdx,%rcx,8)
.LBB2_576:                              # %if.end872.i
                                        #   in Loop: Header=BB2_572 Depth=2
	movzbl	44(%rbx), %eax
	orq	$8, %rax
	movb	%al, 44(%rbx)
	movq	(%rbx), %rdi
	movq	$6, %rsi
	xorq	%rdx, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB2_577
# BB#578:                               # %if.else885.i
                                        #   in Loop: Header=BB2_572 Depth=2
	movq	(%rbx), %rdi
	callq	delete_insn
	jmp	.LBB2_579
	.align	16, 0x90
.LBB2_577:                              # %if.then880.i
                                        #   in Loop: Header=BB2_572 Depth=2
	movq	8(%rax), %rdi
	movq	(%rbx), %rsi
	callq	delete_insn_chain
.LBB2_579:                              # %if.end888.i
                                        #   in Loop: Header=BB2_572 Depth=2
	movl	36(%rbx), %r8d
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	$1, %rcx
	movb	%cl, (%rax,%r8)
	movq	208(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	xorq	%rdi, %rdi
	testq	$16, %rax
	movabsq	$0, %rsi
	movq	%r12, %r10
	jne	.LBB2_593
	jmp	.LBB2_580
	.align	16, 0x90
.LBB2_592:                              # %for.body997.i
                                        #   in Loop: Header=BB2_580 Depth=3
	leaq	(%rdi,%r8), %rax
	movq	192(%r15), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	leaq	(%rax,%rax,2), %rax
	movl	%esi, (%rcx,%rax,8)
	incq	%rdi
.LBB2_580:                              # %for.cond900.i
                                        #   Parent Loop BB2_379 Depth=1
                                        #     Parent Loop BB2_572 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$1, %rax
	cmpq	$52, %r10
	jg	.LBB2_591
# BB#581:                               # %cond.true903.i
                                        #   in Loop: Header=BB2_580 Depth=3
	movq	16(%rbx), %rax
	movzbl	2(%rax), %eax
	testq	%r14, %r14
	je	.LBB2_586
# BB#582:                               # %cond.true927.i
                                        #   in Loop: Header=BB2_580 Depth=3
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB2_591
# BB#583:                               # %lor.rhs936.i
                                        #   in Loop: Header=BB2_580 Depth=3
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB2_585
# BB#584:                               # %lor.rhs936.i
                                        #   in Loop: Header=BB2_580 Depth=3
	movabsq	$1, %rax
.LBB2_585:                              # %lor.rhs936.i
                                        #   in Loop: Header=BB2_580 Depth=3
	incq	%rax
	jmp	.LBB2_591
	.align	16, 0x90
.LBB2_586:                              # %cond.false947.i
                                        #   in Loop: Header=BB2_580 Depth=3
	cmpq	$24, %rax
	jne	.LBB2_587
# BB#589:                               # %cond.true965.i
                                        #   in Loop: Header=BB2_580 Depth=3
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB2_591
.LBB2_587:                              # %cond.false947.i
                                        #   in Loop: Header=BB2_580 Depth=3
	cmpq	$18, %rax
	jne	.LBB2_590
# BB#588:                               # %cond.true954.i
                                        #   in Loop: Header=BB2_580 Depth=3
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB2_591
.LBB2_590:                              # %cond.false969.i
                                        #   in Loop: Header=BB2_580 Depth=3
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB2_591:                              # %cond.end993.i
                                        #   in Loop: Header=BB2_580 Depth=3
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jl	.LBB2_592
.LBB2_593:                              # %for.inc1009.i
                                        #   in Loop: Header=BB2_572 Depth=2
	movq	72(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB2_572
.LBB2_566:                              #   in Loop: Header=BB2_379 Depth=1
	movq	%r13, %r12
.LBB2_567:                              # %if.end1027.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movabsq	$.L.str92, %rbx
	movq	%rbx, %r13
.LBB2_600:                              # %if.end1027.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	loop_dump_stream(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB2_602
# BB#601:                               # %if.then1029.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	$10, %rdi
	callq	fputc
.LBB2_602:                              # %for.inc1032.i
                                        #   in Loop: Header=BB2_379 Depth=1
	movq	72(%r12), %r12
	testq	%r12, %r12
	jne	.LBB2_379
.LBB2_603:                              # %for.end1034.i
	movq	176(%rsp), %rax         # 8-byte Reload
	testq	%rax, %rax
	je	.LBB2_605
# BB#604:                               # %for.end1034.i
	movq	%rax, 64(%rsp)          # 8-byte Spill
.LBB2_605:                              # %for.end1034.i
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	56(%rsp), %r13          # 8-byte Reload
	cmpq	%r13, %rbx
	movq	40(%rsp), %r12          # 8-byte Reload
	je	.LBB2_610
# BB#606:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	200(%rsp), %r15         # 8-byte Reload
	.align	16, 0x90
.LBB2_607:                              # %for.body1042.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	addq	$-32, %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	ja	.LBB2_609
# BB#608:                               # %if.then1057.i
                                        #   in Loop: Header=BB2_607 Depth=1
	movq	32(%rbx), %rdi
	xorq	%rcx, %rcx
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	replace_regs
	movq	56(%rbx), %rdi
	xorq	%rcx, %rcx
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	replace_regs
	movl	%r14d, 40(%rbx)
.LBB2_609:                              # %for.inc1070.i
                                        #   in Loop: Header=BB2_607 Depth=1
	movq	24(%rbx), %rbx
	cmpq	%r13, %rbx
	jne	.LBB2_607
.LBB2_610:                              # %move_movables.exit
	movq	200(%rsp), %rdi         # 8-byte Reload
	callq	free
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	free
	callq	max_reg_num
	movq	$32, %rcx
	movq	128(%rsp), %r14         # 8-byte Reload
	movslq	184(%r14), %r8
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r8, %rax
	movq	160(%rsp), %rdi         # 8-byte Reload
	jle	.LBB2_616
# BB#611:                               # %if.then1551
	xorq	%rsi, %rsi
	callq	loop_regs_scan
	movq	88(%rsp), %rax          # 8-byte Reload
	.align	16, 0x90
.LBB2_612:                              # %for.cond1552
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rdi
	movq	16(%rdi), %rax
	testq	%rax, %rax
	je	.LBB2_614
# BB#613:                               # %land.rhs1557
                                        #   in Loop: Header=BB2_612 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB2_612
.LBB2_614:                              # %for.end1571
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	24(%rax), %rsi
	movl	loop_max_reg(%rip), %edx
	callq	reg_scan_update
	callq	max_reg_num
	movl	%eax, loop_max_reg(%rip)
.LBB2_615:                              # %for.cond1578.preheaderthread-pre-split
	movl	184(%r14), %r8d
	movq	160(%rsp), %rdi         # 8-byte Reload
.LBB2_616:                              # %for.cond1578.preheader
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB2_621
# BB#617:                               # %for.body1582.lr.ph
	movq	192(%r14), %rax
	addq	$4, %rax
	movq	$1, %rbx
	.align	16, 0x90
.LBB2_618:                              # %for.body1582
                                        # =>This Inner Loop Header: Depth=1
	movslq	-4(%rax), %rcx
	testq	%rcx, %rcx
	jns	.LBB2_620
# BB#619:                               # %if.then1589
                                        #   in Loop: Header=BB2_618 Depth=1
	movl	(%rax), %ecx
	movl	%ecx, -4(%rax)
.LBB2_620:                              # %for.inc1599
                                        #   in Loop: Header=BB2_618 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	addq	$24, %rax
	cmpq	%rdx, %rsi
	jl	.LBB2_618
.LBB2_621:                              # %for.end1601
	movq	%r14, 128(%rsp)         # 8-byte Spill
	movq	160(%rdi), %r14
	movq	%rdi, %rbx
	callq	max_reg_num
	movq	%rbx, %rsi
	movl	136(%r14), %ecx
	movq	%rax, 72(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	je	.LBB2_838
# BB#622:                               # %if.end.i2342
	movq	200(%rsi), %rdi
	movq	%rsi, %rbx
	callq	next_nonnote_insn
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	testq	%rax, %rax
	je	.LBB2_626
# BB#623:                               # %land.lhs.true.i2346
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	je	.LBB2_625
# BB#624:                               # %land.lhs.true.i2346
	xorq	%rax, %rax
.LBB2_625:                              # %land.lhs.true.i2346
	movq	%rax, 64(%rsp)          # 8-byte Spill
.LBB2_626:                              # %if.end6.i
	movq	%rbx, %rsi
	movq	216(%rsi), %rdx
	movq	200(%rsi), %rax
	movq	24(%rdx), %rcx
	cmpq	%rax, %rcx
	jne	.LBB2_628
# BB#627:                               # %if.then.i.i
	movq	208(%rsi), %rcx
.LBB2_628:                              # %next_insn_in_loop.exit.i
	cmpq	%rdx, %rcx
	movabsq	$0, %rdi
	je	.LBB2_630
# BB#629:                               # %next_insn_in_loop.exit.i
	movq	%rcx, %rdi
.LBB2_630:                              # %next_insn_in_loop.exit.i
	testq	%rdi, %rdi
	movq	%rdi, 296(%rsp)
	movq	%r14, %r13
	movq	%r13, 120(%rsp)         # 8-byte Spill
	je	.LBB2_631
# BB#632:
	movq	$0, 104(%rsp)           # 8-byte Folded Spill
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	xorq	%r15, %r15
	.align	16, 0x90
.LBB2_633:                              # %for.body.i2350
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdi), %ecx
	cmpq	$36, %rcx
	je	.LBB2_652
# BB#634:                               # %for.body.i2350
                                        #   in Loop: Header=BB2_633 Depth=1
	cmpq	$33, %rcx
	jne	.LBB2_651
# BB#635:                               # %land.lhs.true20.i
                                        #   in Loop: Header=BB2_633 Depth=1
	movq	64(%rdi), %rcx
	movq	208(%rsi), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB2_639
# BB#636:                               # %land.lhs.true22.i2354
                                        #   in Loop: Header=BB2_633 Depth=1
	movq	24(%rdi), %rdx
	movq	24(%rdx), %rdx
	cmpq	%rax, %rdx
	jne	.LBB2_639
# BB#637:                               # %land.lhs.true31.i
                                        #   in Loop: Header=BB2_633 Depth=1
	movq	%rsi, %rbx
	callq	any_uncondjump_p
	movq	%rbx, %rsi
	testq	%r14, %rax
	jne	.LBB2_651
# BB#638:                               # %land.lhs.true31.if.then34_crit_edge.i
                                        #   in Loop: Header=BB2_633 Depth=1
	movq	296(%rsp), %rdi
	movq	64(%rdi), %rcx
	.align	16, 0x90
.LBB2_639:                              # %if.then34.i
                                        #   in Loop: Header=BB2_633 Depth=1
	testq	%rcx, %rcx
	je	.LBB2_838
# BB#640:                               # %lor.lhs.false.i2355
                                        #   in Loop: Header=BB2_633 Depth=1
	movq	64(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %rcx
	je	.LBB2_646
# BB#641:                               # %land.lhs.true43.i
                                        #   in Loop: Header=BB2_633 Depth=1
	movslq	8(%rcx), %rcx
	movslq	max_uid_for_loop(%rip), %r8
	cmpq	%r8, %rcx
	jge	.LBB2_838
# BB#642:                               # %cond.true.i2359
                                        #   in Loop: Header=BB2_633 Depth=1
	movq	192(%rsi), %rdx
	movslq	8(%rdx), %rbx
	cmpq	%r8, %rbx
	jge	.LBB2_859
# BB#643:                               # %cond.true69.i
                                        #   in Loop: Header=BB2_633 Depth=1
	movq	uid_luid(%rip), %r9
	movl	(%r9,%rcx,4), %eax
	movslq	(%r9,%rbx,4), %rbx
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rbx, %rdx
	jl	.LBB2_838
# BB#644:                               # %cond.true88.i
                                        #   in Loop: Header=BB2_633 Depth=1
	movq	200(%rsi), %rcx
	movslq	8(%rcx), %rcx
	cmpq	%r8, %rcx
	jge	.LBB2_860
# BB#645:                               # %cond.true105.i
                                        #   in Loop: Header=BB2_633 Depth=1
	movslq	(%r9,%rcx,4), %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jg	.LBB2_838
.LBB2_646:                              # %if.end117.i
                                        #   in Loop: Header=BB2_633 Depth=1
	movq	%rsi, %rbx
	callq	any_condjump_p
	andq	%r14, %rax
	movq	$1, %rcx
	je	.LBB2_648
# BB#647:                               # %if.end117.i
                                        #   in Loop: Header=BB2_633 Depth=1
	movq	$1, %r15
.LBB2_648:                              # %if.end117.i
                                        #   in Loop: Header=BB2_633 Depth=1
	testq	%rax, %rax
	movq	%rbx, %rsi
	je	.LBB2_650
# BB#649:                               # %if.end117.i
                                        #   in Loop: Header=BB2_633 Depth=1
	movq	104(%rsp), %rcx         # 8-byte Reload
.LBB2_650:                              # %if.end117.i
                                        #   in Loop: Header=BB2_633 Depth=1
	movq	%rcx, 104(%rsp)         # 8-byte Spill
	jmp	.LBB2_653
	.align	16, 0x90
.LBB2_651:                              # %if.else123.i
                                        #   in Loop: Header=BB2_633 Depth=1
	testq	%r14, %r15
	je	.LBB2_653
.LBB2_652:                              # %if.else123.i
                                        #   in Loop: Header=BB2_633 Depth=1
	movq	$1, 104(%rsp)           # 8-byte Folded Spill
.LBB2_653:                              # %for.inc.i2360
                                        #   in Loop: Header=BB2_633 Depth=1
	movq	296(%rsp), %rax
	movq	24(%rax), %rcx
	movq	200(%rsi), %rax
	cmpq	%rax, %rcx
	jne	.LBB2_655
# BB#654:                               # %if.then.i989.i
                                        #   in Loop: Header=BB2_633 Depth=1
	movq	208(%rsi), %rcx
.LBB2_655:                              # %next_insn_in_loop.exit994.i
                                        #   in Loop: Header=BB2_633 Depth=1
	movq	216(%rsi), %rdx
	xorq	%rdi, %rdi
	cmpq	%rdx, %rcx
	je	.LBB2_657
# BB#656:                               # %select.mid554
                                        #   in Loop: Header=BB2_633 Depth=1
	movq	%rcx, %rdi
.LBB2_657:                              # %select.end553
                                        #   in Loop: Header=BB2_633 Depth=1
	movq	%rdi, 296(%rsp)
	testq	%rdi, %rdi
	jne	.LBB2_633
	jmp	.LBB2_658
	.align	16, 0x90
.LBB2_489:                              # %tailrecurse.us.i1394.i
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB2_489
	.align	16, 0x90
.LBB2_505:                              # %tailrecurse.us.i1399.i
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB2_505
	.align	16, 0x90
.LBB2_519:                              # %tailrecurse.us.i1410.i
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB2_519
	.align	16, 0x90
.LBB2_449:                              # %tailrecurse.us.i1389.i
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB2_449
	.align	16, 0x90
.LBB2_423:                              # %tailrecurse.us.i.i
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB2_423
	.align	16, 0x90
.LBB2_513:                              # %tailrecurse.us.i1405.i
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB2_513
.LBB2_631:
	movq	$0, 104(%rsp)           # 8-byte Folded Spill
.LBB2_658:                              # %for.end.i2362
	movq	192(%rsi), %rax
	movq	%rsi, %rbx
	.align	16, 0x90
.LBB2_659:                              # %for.cond131.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %r14
	movq	%r14, 296(%rsp)
	movq	16(%r14), %rax
	testq	%rax, %rax
	je	.LBB2_661
# BB#660:                               # %land.rhs.i2363
                                        #   in Loop: Header=BB2_659 Depth=1
	movzbl	(%r14), %ecx
	cmpq	$36, %rcx
	jne	.LBB2_659
.LBB2_661:                              # %for.end144.i
	callq	cselib_init
	movq	192(%rbx), %rax
	cmpq	%rax, %r14
	je	.LBB2_664
# BB#662:
	movq	%r14, %rdi
	.align	16, 0x90
.LBB2_663:                              # %for.body148.i
                                        # =>This Inner Loop Header: Depth=1
	callq	cselib_process_insn
	movq	296(%rsp), %rax
	movq	24(%rax), %rdi
	movq	%rdi, 296(%rsp)
	movq	192(%rbx), %rax
	cmpq	%rax, %rdi
	jne	.LBB2_663
.LBB2_664:                              # %for.cond154.preheader.i
	movq	%r14, 40(%rsp)          # 8-byte Spill
	movq	%rbx, 160(%rsp)         # 8-byte Spill
	movslq	136(%r13), %rax
	testq	%rax, %rax
	jle	.LBB2_837
# BB#665:                               # %for.body157.lr.ph.i
	xorq	%rbx, %rbx
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB2_666:                              # %for.body157.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_671 Depth 2
                                        #     Child Loop BB2_685 Depth 2
                                        #     Child Loop BB2_698 Depth 2
                                        #     Child Loop BB2_728 Depth 2
                                        #     Child Loop BB2_755 Depth 2
                                        #     Child Loop BB2_771 Depth 2
                                        #       Child Loop BB2_773 Depth 3
                                        #       Child Loop BB2_777 Depth 3
                                        #     Child Loop BB2_784 Depth 2
                                        #       Child Loop BB2_785 Depth 3
                                        #         Child Loop BB2_787 Depth 4
                                        #           Child Loop BB2_794 Depth 5
	movq	128(%r13), %rax
	leaq	(%rbx,%rbx,2), %r14
	movq	(%rax,%r14,8), %rcx
	movq	%rcx, 216(%rsp)         # 8-byte Spill
	movzbl	3(%rcx), %ecx
	testq	$8, %rcx
	jne	.LBB2_669
# BB#667:                               # %lor.lhs.false164.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	216(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rsi
	movq	160(%rsp), %rdi         # 8-byte Reload
	callq	loop_invariant_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	je	.LBB2_670
# BB#668:                               # %lor.lhs.false164.if.then170_crit_edge.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	128(%r13), %rax
.LBB2_669:                              # %if.then170.i
                                        #   in Loop: Header=BB2_666 Depth=1
	xorq	%rcx, %rcx
	movl	%ecx, 16(%rax,%r14,8)
.LBB2_670:                              # %if.end174.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	movq	120(%r13), %rbx
	movq	%r13, %r15
	xorq	%r12, %r12
	testq	%rbx, %rbx
	je	.LBB2_689
	.align	16, 0x90
.LBB2_671:                              # %while.body.i2367
                                        #   Parent Loop BB2_666 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx), %rsi
	movq	216(%rsp), %rdi         # 8-byte Reload
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rax
	jne	.LBB2_674
# BB#672:                               # %if.else182.i
                                        #   in Loop: Header=BB2_671 Depth=2
	movq	8(%rbx), %rdi
	xorq	%rsi, %rsi
	movq	216(%rsp), %rdx         # 8-byte Reload
	movabsq	$rtx_varies_p, %rcx
	callq	true_dependence
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_861
# BB#673:                               #   in Loop: Header=BB2_671 Depth=2
	movq	%r12, %rax
.LBB2_674:                              # %if.end194.i
                                        #   in Loop: Header=BB2_671 Depth=2
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	movq	%rax, %r12
	jne	.LBB2_671
	jmp	.LBB2_675
.LBB2_861:                              # %if.then188.i2369
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	128(%r15), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 16(%rax,%r14,8)
	movq	%r12, %rax
.LBB2_675:                              # %while.end.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movl	flag_float_store(%rip), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	movabsq	$0, %r12
	je	.LBB2_677
# BB#676:                               # %while.end.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movabsq	$1, %r12
.LBB2_677:                              # %while.end.i
                                        #   in Loop: Header=BB2_666 Depth=1
	testq	%rcx, %rcx
	je	.LBB2_681
# BB#678:                               # %while.end.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_681
# BB#679:                               # %land.lhs.true201.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	216(%rsp), %rcx         # 8-byte Reload
	movzbl	2(%rcx), %ecx
	movl	mode_class(,%rcx,4), %ecx
	cmpq	$2, %rcx
	jne	.LBB2_681
# BB#680:                               # %if.then208.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	128(%r15), %rcx
	xorq	%rdx, %rdx
	movl	%edx, 16(%rcx,%r14,8)
.LBB2_681:                              # %if.end213.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	128(%r15), %rcx
	movl	16(%rcx,%r14,8), %ecx
	testq	%rcx, %rcx
	je	.LBB2_689
# BB#682:                               # %if.end213.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_689
# BB#683:                               # %for.cond222.preheader.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movslq	136(%r15), %rax
	testq	%rax, %rax
	movq	$1, %r12
	jle	.LBB2_689
# BB#684:                               # %for.body225.i.preheader
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	$1, %rbx
	xorq	%r13, %r13
	.align	16, 0x90
.LBB2_685:                              # %for.body225.i
                                        #   Parent Loop BB2_666 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	96(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%rdx, %rsi
	andq	%rsi, %rcx
	leaq	-1(%rbx), %rdx
	andq	%rsi, %rdx
	cmpq	%rcx, %rdx
	je	.LBB2_688
# BB#686:                               # %if.else228.i
                                        #   in Loop: Header=BB2_685 Depth=2
	movq	128(%r15), %rax
	movq	(%rax,%r13), %rdx
	xorq	%rsi, %rsi
	movq	216(%rsp), %rdi         # 8-byte Reload
	movabsq	$rtx_varies_p, %rcx
	callq	true_dependence
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_862
# BB#687:                               # %if.else228.for.inc242_crit_edge.i
                                        #   in Loop: Header=BB2_685 Depth=2
	movl	136(%r15), %eax
.LBB2_688:                              # %for.inc242.i
                                        #   in Loop: Header=BB2_685 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	addq	$24, %r13
	cmpq	%rsi, %rdx
	movq	$1, %r12
	jl	.LBB2_685
	jmp	.LBB2_689
.LBB2_862:                              # %if.then235.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	128(%r15), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 16(%rax,%r14,8)
	movq	$1, %r12
	.align	16, 0x90
.LBB2_689:                              # %if.end244.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	104(%rsp), %rax         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%r15, %r13
	je	.LBB2_692
# BB#690:                               # %land.lhs.true246.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	216(%rsp), %rdi         # 8-byte Reload
	callq	may_trap_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_692
# BB#691:                               # %if.then249.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	128(%r13), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 16(%rax,%r14,8)
.LBB2_692:                              # %if.end254.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	128(%r13), %rax
	movl	16(%rax,%r14,8), %eax
	testq	%rax, %rax
	je	.LBB2_827
# BB#693:                               # %if.end261.i
                                        #   in Loop: Header=BB2_666 Depth=1
	leaq	272(%rsp), %rdi
	callq	bitmap_initialize
	leaq	248(%rsp), %rdi
	callq	bitmap_initialize
	movq	216(%rsp), %rax         # 8-byte Reload
	movzbl	2(%rax), %edi
	callq	gen_reg_rtx
	movq	%rax, 184(%rsp)         # 8-byte Spill
	movl	(%rax), %ecx
	orq	$134217728, %rcx        # imm = 0x8000000
	movl	%ecx, (%rax)
	movq	128(%r13), %rcx
	movq	%rax, 8(%rcx,%r14,8)
	movq	160(%rsp), %r13         # 8-byte Reload
	movq	216(%r13), %rcx
	movq	200(%r13), %rdx
	movq	24(%rcx), %rax
	cmpq	%rdx, %rax
	jne	.LBB2_695
# BB#694:                               # %if.then.i1000.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	208(%r13), %rax
.LBB2_695:                              # %next_insn_in_loop.exit1005.i
                                        #   in Loop: Header=BB2_666 Depth=1
	xorq	%r15, %r15
	cmpq	%rcx, %rax
	movabsq	$0, %rdi
	je	.LBB2_697
# BB#696:                               # %next_insn_in_loop.exit1005.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	%rax, %rdi
.LBB2_697:                              # %next_insn_in_loop.exit1005.i
                                        #   in Loop: Header=BB2_666 Depth=1
	testq	%rdi, %rdi
	movq	%rdi, 296(%rsp)
	je	.LBB2_724
	.align	16, 0x90
.LBB2_698:                              # %for.body278.i
                                        #   Parent Loop BB2_666 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB2_716
# BB#699:                               # %cond.true293.i
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	32(%rdi), %rbx
	movzbl	(%rbx), %eax
	cmpq	$47, %rax
	je	.LBB2_701
# BB#700:                               # %cond.false305.i
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	%rbx, %rsi
	callq	single_set_2
	movq	%rax, %rbx
.LBB2_701:                              # %cond.end313.i
                                        #   in Loop: Header=BB2_698 Depth=2
	testq	%rbx, %rbx
	je	.LBB2_715
# BB#702:                               # %cond.end313.i
                                        #   in Loop: Header=BB2_698 Depth=2
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	jne	.LBB2_715
# BB#703:                               # %land.lhs.true318.i
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB2_709
# BB#704:                               # %land.lhs.true326.i2381
                                        #   in Loop: Header=BB2_698 Depth=2
	movl	8(%rax), %eax
	cmpq	$53, %rax
	jb	.LBB2_709
# BB#705:                               # %land.lhs.true326.i2381
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	72(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jae	.LBB2_709
# BB#706:                               # %land.lhs.true343.i
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	192(%rcx), %rcx
	leaq	(%rax,%rax,2), %rax
	movl	4(%rcx,%rax,8), %eax
	cmpq	$1, %rax
	jne	.LBB2_709
# BB#707:                               # %land.lhs.true354.i
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	16(%rbx), %rdi
	movq	216(%rsp), %rsi         # 8-byte Reload
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_709
# BB#708:                               # %if.then360.i
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	8(%rbx), %rax
	movl	8(%rax), %esi
	leaq	272(%rsp), %rdi
	callq	bitmap_set_bit
	.align	16, 0x90
.LBB2_709:                              # %land.lhs.true371.i
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	16(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB2_715
# BB#710:                               # %land.lhs.true379.i
                                        #   in Loop: Header=BB2_698 Depth=2
	movl	8(%rax), %eax
	cmpq	$53, %rax
	jb	.LBB2_715
# BB#711:                               # %land.lhs.true379.i
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	72(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jae	.LBB2_715
# BB#712:                               # %land.lhs.true397.i
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	192(%rcx), %rcx
	leaq	(%rax,%rax,2), %rax
	movl	4(%rcx,%rax,8), %eax
	cmpq	$1, %rax
	jne	.LBB2_715
# BB#713:                               # %land.lhs.true410.i
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	8(%rbx), %rdi
	movq	216(%rsp), %rsi         # 8-byte Reload
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_715
# BB#714:                               # %if.then416.i
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	16(%rbx), %rax
	movl	8(%rax), %esi
	leaq	248(%rsp), %rdi
	callq	bitmap_set_bit
	.align	16, 0x90
.LBB2_715:                              # %if.end423.i
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	128(%rax), %rax
	movq	296(%rsp), %rcx
	movq	8(%rax,%r14,8), %rdx
	movq	(%rax,%r14,8), %rax
	movq	%rcx, 328(%rsp)
	movq	%rcx, 320(%rsp)
	movq	%rdx, 312(%rsp)
	movq	%rax, 304(%rsp)
	leaq	328(%rsp), %rdi
	movabsq	$replace_loop_mem, %rsi
	leaq	304(%rsp), %rdx
	callq	for_each_rtx
	movq	296(%rsp), %rdi
	movl	(%rdi), %eax
.LBB2_716:                              # %if.end432.i
                                        #   in Loop: Header=BB2_698 Depth=2
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$33, %rax
	je	.LBB2_718
# BB#717:                               # %if.end432.i
                                        #   in Loop: Header=BB2_698 Depth=2
	cmpq	$36, %rax
	jne	.LBB2_719
.LBB2_718:                              # %if.then442.i
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	$1, %r15
.LBB2_719:                              # %for.inc444.i
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	24(%rdi), %rax
	movq	200(%r13), %rcx
	cmpq	%rcx, %rax
	jne	.LBB2_721
# BB#720:                               # %if.then.i1011.i
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	208(%r13), %rax
.LBB2_721:                              # %next_insn_in_loop.exit1016.i
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	216(%r13), %rcx
	xorq	%rdi, %rdi
	cmpq	%rcx, %rax
	je	.LBB2_723
# BB#722:                               # %select.mid649
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	%rax, %rdi
.LBB2_723:                              # %select.end648
                                        #   in Loop: Header=BB2_698 Depth=2
	movq	%rdi, 296(%rsp)
	testq	%rdi, %rdi
	jne	.LBB2_698
.LBB2_724:                              # %for.end446.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	%r15, 104(%rsp)         # 8-byte Spill
	movq	%r13, 160(%rsp)         # 8-byte Spill
	callq	apply_change_group
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_726
# BB#725:                               # %if.then449.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	120(%rsp), %r13         # 8-byte Reload
	movq	128(%r13), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 16(%rax,%r14,8)
	jmp	.LBB2_827
.LBB2_726:                              # %if.else454.i
                                        #   in Loop: Header=BB2_666 Depth=1
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	$0, 208(%rsp)           # 8-byte Folded Spill
	movq	216(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	cselib_lookup
	testq	%rax, %rax
	movq	%rbx, %r14
	je	.LBB2_750
# BB#727:                               # %if.then459.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	16(%rax), %rax
	xorq	%r13, %r13
	testq	%rax, %rax
	movabsq	$0, %r15
	movq	216(%rsp), %r14         # 8-byte Reload
	movabsq	$0, %rcx
	movq	%rcx, 208(%rsp)         # 8-byte Spill
	je	.LBB2_750
	.align	16, 0x90
.LBB2_728:                              # %for.body462.i
                                        #   Parent Loop BB2_666 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rax), %rdx
	movzbl	(%rdx), %esi
	cmpq	$133, %rsi
	jg	.LBB2_732
# BB#729:                               # %for.body462.i
                                        #   in Loop: Header=BB2_728 Depth=2
	leaq	-54(%rsi), %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$14, %rcx
	ja	.LBB2_736
# BB#730:                               # %for.body462.i
                                        #   in Loop: Header=BB2_728 Depth=2
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	$24599, %rdi            # imm = 0x6017
	je	.LBB2_736
# BB#731:                               #   in Loop: Header=BB2_728 Depth=2
	movq	%rax, %r15
	jmp	.LBB2_740
	.align	16, 0x90
.LBB2_732:                              # %for.body462.i
                                        #   in Loop: Header=BB2_728 Depth=2
	cmpq	$134, %rsi
	jne	.LBB2_734
# BB#733:                               #   in Loop: Header=BB2_728 Depth=2
	movq	%rax, %r15
	jmp	.LBB2_740
	.align	16, 0x90
.LBB2_734:                              # %for.body462.i
                                        #   in Loop: Header=BB2_728 Depth=2
	cmpq	$140, %rsi
	jne	.LBB2_736
# BB#735:                               #   in Loop: Header=BB2_728 Depth=2
	movq	%rax, %r15
	jmp	.LBB2_740
	.align	16, 0x90
.LBB2_736:                              # %if.else510.i
                                        #   in Loop: Header=BB2_728 Depth=2
	cmpq	$61, %rsi
	jne	.LBB2_740
# BB#737:                               # %land.lhs.true516.i
                                        #   in Loop: Header=BB2_728 Depth=2
	movl	8(%rdx), %ecx
	cmpq	$52, %rcx
	movq	%rax, %rcx
	ja	.LBB2_739
# BB#738:                               # %select.mid654
                                        #   in Loop: Header=BB2_728 Depth=2
	movq	%r13, %rcx
.LBB2_739:                              # %select.end653
                                        #   in Loop: Header=BB2_728 Depth=2
	movq	%rcx, %r13
.LBB2_740:                              # %for.inc526.i
                                        #   in Loop: Header=BB2_728 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB2_728
# BB#741:                               # %for.end527.i
                                        #   in Loop: Header=BB2_666 Depth=1
	testq	%r13, %r13
	jne	.LBB2_743
# BB#742:                               #   in Loop: Header=BB2_666 Depth=1
	movq	%r15, 208(%rsp)         # 8-byte Spill
	movq	%r15, %r13
	jmp	.LBB2_748
.LBB2_743:                              # %if.else530.i
                                        #   in Loop: Header=BB2_666 Depth=1
	testq	%r15, %r15
	jne	.LBB2_745
# BB#744:                               #   in Loop: Header=BB2_666 Depth=1
	movq	$0, 208(%rsp)           # 8-byte Folded Spill
	movq	%r13, %r15
	jmp	.LBB2_749
.LBB2_745:                              # %land.lhs.true532.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	8(%r15), %rdi
	movq	$47, %rsi
	callq	rtx_cost
	movq	%rax, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	8(%r13), %rdi
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$47, %rsi
	callq	rtx_cost
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rbx
	movq	%r15, %rax
	jg	.LBB2_747
# BB#746:                               # %land.lhs.true532.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movabsq	$0, %rax
.LBB2_747:                              # %land.lhs.true532.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	%rax, 208(%rsp)         # 8-byte Spill
	jle	.LBB2_749
.LBB2_748:                              # %if.end541.i
                                        #   in Loop: Header=BB2_666 Depth=1
	testq	%r13, %r13
	movq	%r13, %r15
	movq	216(%rsp), %r14         # 8-byte Reload
	je	.LBB2_750
.LBB2_749:                              # %if.then543.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	8(%r15), %rdi
	callq	copy_rtx
	movq	%rax, %r14
.LBB2_750:                              # %if.end547.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	184(%rsp), %rdi         # 8-byte Reload
	movq	%r14, %rsi
	callq	gen_move_insn
	movq	160(%rsp), %rcx         # 8-byte Reload
	movq	192(%rcx), %rsi
	testq	%rsi, %rsi
	movq	48(%rsp), %r13          # 8-byte Reload
	je	.LBB2_751
# BB#752:                               # %loop_insn_hoist.exit.i2393
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	%rax, %rdi
	callq	emit_insn_before
	movzbl	(%r14), %ecx
	movq	%rax, %r15
	cmpq	$61, %rcx
	jne	.LBB2_758
# BB#753:                               # %for.cond555.preheader.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 296(%rsp)
	movq	160(%rsp), %rax         # 8-byte Reload
	movq	192(%rax), %rax
	cmpq	%rax, %rcx
	je	.LBB2_758
# BB#754:                               # %for.body559.lr.ph.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movl	8(%r14), %r8d
	movq	reg_n_info(%rip), %rdx
	movq	40(%rsp), %rsi          # 8-byte Reload
	.align	16, 0x90
.LBB2_755:                              # %for.body559.i
                                        #   Parent Loop BB2_666 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	32(%rdx,%r8,8), %rdi
	movl	4(%rdi), %ebx
	movl	8(%rsi), %ecx
	cmpq	%rcx, %rbx
	je	.LBB2_756
# BB#757:                               # %for.inc584.i
                                        #   in Loop: Header=BB2_755 Depth=2
	movq	24(%rsi), %rsi
	cmpq	%rax, %rsi
	movq	%rsi, 296(%rsp)
	jne	.LBB2_755
	jmp	.LBB2_758
.LBB2_756:                              # %if.then571.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movl	8(%r15), %eax
	movl	%eax, 4(%rdi)
.LBB2_758:                              # %if.end589.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	208(%rsp), %rax         # 8-byte Reload
	testq	%rax, %rax
	je	.LBB2_760
# BB#759:                               # %if.then591.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	8(%rax), %rdi
	callq	copy_rtx
	movq	$4, %rsi
	movq	%r15, %rdi
	movq	%rax, %rdx
	callq	set_unique_reg_note
.LBB2_760:                              # %if.end595.i
                                        #   in Loop: Header=BB2_666 Depth=1
	testq	%r12, %r12
	je	.LBB2_764
# BB#761:                               # %if.then597.i
                                        #   in Loop: Header=BB2_666 Depth=1
	testq	%r13, %r13
	jne	.LBB2_763
# BB#762:                               # %if.then600.i2396
                                        #   in Loop: Header=BB2_666 Depth=1
	callq	gen_label_rtx
	movq	160(%rsp), %rcx         # 8-byte Reload
	movq	200(%rcx), %rsi
	movq	%rax, %r13
	movq	%r13, %rdi
	callq	emit_label_after
.LBB2_763:                              # %if.end604.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	216(%rsp), %rdi         # 8-byte Reload
	callq	copy_rtx
	movq	%rax, %rdi
	movq	184(%rsp), %rsi         # 8-byte Reload
	callq	gen_move_insn
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	emit_insn_after
.LBB2_764:                              # %if.end608.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB2_768
# BB#765:                               # %if.then610.i
                                        #   in Loop: Header=BB2_666 Depth=1
	testq	%r12, %r12
	movabsq	$.L.str86, %rcx
	jne	.LBB2_767
# BB#766:                               # %if.then610.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movabsq	$.L.str87, %rcx
.LBB2_767:                              # %if.then610.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	184(%rsp), %rax         # 8-byte Reload
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str85, %rsi
	callq	fprintf
	movq	loop_dump_stream(%rip), %rdi
	movq	216(%rsp), %rsi         # 8-byte Reload
	callq	print_rtl
	movq	loop_dump_stream(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
.LBB2_768:                              # %do.body.i2397
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movq	272(%rsp), %r14
	testq	%r14, %r14
	je	.LBB2_781
# BB#769:                               # %for.cond641.preheader.lr.ph.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movl	16(%r14), %eax
	testq	%rax, %rax
	movq	$53, %r15
	je	.LBB2_771
# BB#770:                               # %select.mid674
                                        #   in Loop: Header=BB2_666 Depth=1
	xorq	%r15, %r15
	.align	16, 0x90
.LBB2_771:                              # %for.cond641.preheader.i
                                        #   Parent Loop BB2_666 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_773 Depth 3
                                        #       Child Loop BB2_777 Depth 3
	movq	24(%r14), %r12
	testq	%r12, %r12
	je	.LBB2_776
# BB#772:                               # %for.body653.preheader.i
                                        #   in Loop: Header=BB2_771 Depth=2
	movq	%r15, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r13
	.align	16, 0x90
.LBB2_773:                              # %for.body653.i
                                        #   Parent Loop BB2_666 Depth=1
                                        #     Parent Loop BB2_771 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$1, %rbx
	movq	%r13, %rcx
	shlq	%cl, %rbx
	testq	%rbx, %r12
	je	.LBB2_775
# BB#774:                               # %if.then656.i
                                        #   in Loop: Header=BB2_773 Depth=3
	movl	16(%r14), %edx
	notq	%rbx
	movq	$7, %rcx
	shlq	%cl, %rdx
	addq	%r13, %rdx
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	184(%rsp), %rsi         # 8-byte Reload
	callq	try_copy_prop
	andq	%rbx, %r12
	je	.LBB2_776
.LBB2_775:                              # %for.inc666.i
                                        #   in Loop: Header=BB2_773 Depth=3
	incq	%r15
	movq	%r15, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	incq	%r13
	cmpq	$64, %rax
	jb	.LBB2_773
.LBB2_776:                              # %if.end669.i
                                        #   in Loop: Header=BB2_771 Depth=2
	movq	32(%r14), %r15
	xorq	%r13, %r13
	testq	%r15, %r15
	movabsq	$0, %r12
	je	.LBB2_780
	.align	16, 0x90
.LBB2_777:                              # %for.body653.1.i
                                        #   Parent Loop BB2_666 Depth=1
                                        #     Parent Loop BB2_771 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$1, %rbx
	movq	%r13, %rcx
	shlq	%cl, %rbx
	testq	%rbx, %r15
	je	.LBB2_779
# BB#778:                               # %if.then656.1.i
                                        #   in Loop: Header=BB2_777 Depth=3
	movl	16(%r14), %eax
	notq	%rbx
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	64(%r13,%rax), %rdx
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	184(%rsp), %rsi         # 8-byte Reload
	callq	try_copy_prop
	andq	%rbx, %r15
	je	.LBB2_780
.LBB2_779:                              # %for.inc666.1.i
                                        #   in Loop: Header=BB2_777 Depth=3
	incq	%r12
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	incq	%r13
	cmpq	$64, %rax
	jb	.LBB2_777
.LBB2_780:                              # %if.end669.1.i
                                        #   in Loop: Header=BB2_771 Depth=2
	movq	(%r14), %r14
	xorq	%r15, %r15
	testq	%r14, %r14
	jne	.LBB2_771
.LBB2_781:                              # %do.end.i
                                        #   in Loop: Header=BB2_666 Depth=1
	leaq	272(%rsp), %rdi
	callq	bitmap_clear
	movq	248(%rsp), %rax
	movq	%rax, 176(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	movq	160(%rsp), %r15         # 8-byte Reload
	je	.LBB2_826
# BB#782:                               # %for.cond705.preheader.lr.ph.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	176(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	testq	%rax, %rax
	movq	$53, %rsi
	je	.LBB2_784
# BB#783:                               # %select.mid679
                                        #   in Loop: Header=BB2_666 Depth=1
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB2_784:                              # %for.cond705.preheader.i
                                        #   Parent Loop BB2_666 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_785 Depth 3
                                        #         Child Loop BB2_787 Depth 4
                                        #           Child Loop BB2_794 Depth 5
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB2_785:                              # %for.body708.i
                                        #   Parent Loop BB2_666 Depth=1
                                        #     Parent Loop BB2_784 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB2_787 Depth 4
                                        #           Child Loop BB2_794 Depth 5
	movq	%rcx, 144(%rsp)         # 8-byte Spill
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	24(%rax,%rcx,8), %rdx
	testq	%rdx, %rdx
	je	.LBB2_824
# BB#786:                               # %for.body719.preheader.i
                                        #   in Loop: Header=BB2_785 Depth=3
	movq	$6, %rcx
	movq	144(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	%rsi, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r13
	.align	16, 0x90
.LBB2_787:                              # %for.body719.i
                                        #   Parent Loop BB2_666 Depth=1
                                        #     Parent Loop BB2_784 Depth=2
                                        #       Parent Loop BB2_785 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB2_794 Depth 5
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	testq	%rax, %rdx
	je	.LBB2_823
# BB#788:                               # %if.then726.i
                                        #   in Loop: Header=BB2_787 Depth=4
	movq	%rsi, 192(%rsp)         # 8-byte Spill
	movq	%rax, 200(%rsp)         # 8-byte Spill
	movq	%rdx, 208(%rsp)         # 8-byte Spill
	movq	216(%r15), %rsi
	movq	176(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	200(%r15), %rdi
	movq	24(%rsi), %rdx
	movq	184(%rsp), %rcx         # 8-byte Reload
	movl	8(%rcx), %ecx
	movq	%rcx, 216(%rsp)         # 8-byte Spill
	movq	$7, %rcx
	shlq	%cl, %rax
	cmpq	%rdi, %rdx
	jne	.LBB2_790
# BB#789:                               # %if.then.i.i.i
                                        #   in Loop: Header=BB2_787 Depth=4
	movq	208(%r15), %rdx
.LBB2_790:                              # %next_insn_in_loop.exit.i.i
                                        #   in Loop: Header=BB2_787 Depth=4
	xorq	%r12, %r12
	cmpq	%rsi, %rdx
	je	.LBB2_792
# BB#791:                               # %next_insn_in_loop.exit.i.i
                                        #   in Loop: Header=BB2_787 Depth=4
	movq	%rdx, %r12
.LBB2_792:                              # %next_insn_in_loop.exit.i.i
                                        #   in Loop: Header=BB2_787 Depth=4
	testq	%r12, %r12
	je	.LBB2_822
# BB#793:                               #   in Loop: Header=BB2_787 Depth=4
	movq	152(%rsp), %rcx         # 8-byte Reload
	leaq	(%r13,%rcx), %rcx
	addq	%rax, %rcx
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB2_794:                              # %for.body.i.i
                                        #   Parent Loop BB2_666 Depth=1
                                        #     Parent Loop BB2_784 Depth=2
                                        #       Parent Loop BB2_785 Depth=3
                                        #         Parent Loop BB2_787 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB2_802
# BB#795:                               # %cond.true.i.i
                                        #   in Loop: Header=BB2_794 Depth=5
	movq	32(%r12), %r14
	movzbl	(%r14), %eax
	cmpq	$47, %rax
	je	.LBB2_797
# BB#796:                               # %cond.false.i.i
                                        #   in Loop: Header=BB2_794 Depth=5
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	single_set_2
	movq	%rax, %r14
.LBB2_797:                              # %cond.end26.i.i
                                        #   in Loop: Header=BB2_794 Depth=5
	testq	%r14, %r14
	je	.LBB2_802
# BB#798:                               # %land.lhs.true28.i.i
                                        #   in Loop: Header=BB2_794 Depth=5
	movq	8(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB2_802
# BB#799:                               # %land.lhs.true36.i.i
                                        #   in Loop: Header=BB2_794 Depth=5
	movl	8(%rax), %eax
	movq	216(%rsp), %rcx         # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jne	.LBB2_802
# BB#800:                               # %land.lhs.true45.i.i
                                        #   in Loop: Header=BB2_794 Depth=5
	movq	16(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB2_802
# BB#801:                               # %land.lhs.true53.i.i
                                        #   in Loop: Header=BB2_794 Depth=5
	movl	8(%rax), %eax
	movq	168(%rsp), %rbx         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rbx
	cmpq	%rbx, %rax
	je	.LBB2_807
	.align	16, 0x90
.LBB2_802:                              # %for.inc.i.i
                                        #   in Loop: Header=BB2_794 Depth=5
	movq	24(%r12), %rax
	movq	200(%r15), %rcx
	cmpq	%rcx, %rax
	jne	.LBB2_804
# BB#803:                               # %if.then.i219.i.i
                                        #   in Loop: Header=BB2_794 Depth=5
	movq	208(%r15), %rax
.LBB2_804:                              # %next_insn_in_loop.exit224.i.i
                                        #   in Loop: Header=BB2_794 Depth=5
	movq	216(%r15), %rcx
	xorq	%r12, %r12
	cmpq	%rcx, %rax
	je	.LBB2_806
# BB#805:                               # %select.mid705
                                        #   in Loop: Header=BB2_794 Depth=5
	movq	%rax, %r12
.LBB2_806:                              # %select.end704
                                        #   in Loop: Header=BB2_794 Depth=5
	testq	%r12, %r12
	jne	.LBB2_794
	jmp	.LBB2_822
.LBB2_807:                              # %if.then65.i.i
                                        #   in Loop: Header=BB2_787 Depth=4
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB2_822
# BB#808:                               # %land.lhs.true76.i.i
                                        #   in Loop: Header=BB2_787 Depth=4
	movq	16(%r12), %rdi
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB2_822
# BB#809:                               # %cond.true84.i.i
                                        #   in Loop: Header=BB2_787 Depth=4
	movq	32(%rdi), %rsi
	movzbl	(%rsi), %eax
	cmpq	$47, %rax
	jne	.LBB2_811
# BB#810:                               #   in Loop: Header=BB2_787 Depth=4
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	jmp	.LBB2_812
.LBB2_811:                              # %cond.false96.i.i
                                        #   in Loop: Header=BB2_787 Depth=4
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	callq	single_set_2
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	%rax, %rsi
.LBB2_812:                              # %cond.end104.i.i
                                        #   in Loop: Header=BB2_787 Depth=4
	testq	%rsi, %rsi
	movq	56(%rsp), %rdi          # 8-byte Reload
	je	.LBB2_822
# BB#813:                               # %land.lhs.true107.i.i
                                        #   in Loop: Header=BB2_787 Depth=4
	movq	8(%rsi), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB2_822
# BB#814:                               # %land.lhs.true115.i.i
                                        #   in Loop: Header=BB2_787 Depth=4
	movl	8(%rax), %eax
	cmpq	%rbx, %rax
	jne	.LBB2_822
# BB#815:                               # %if.then124.i.i
                                        #   in Loop: Header=BB2_787 Depth=4
	leaq	8(%r14), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	addq	$16, %r14
	addq	$8, %rsi
	movq	$1, %rcx
	movq	184(%rsp), %rdx         # 8-byte Reload
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	%rdx, %rbx
	callq	validate_change
	movq	(%r14), %rdx
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	validate_change
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	validate_change
	callq	apply_change_group
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	32(%rsp), %rdx          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_822
# BB#816:                               # %if.then142.i.i
                                        #   in Loop: Header=BB2_787 Depth=4
	movq	%rdx, %r14
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	jne	.LBB2_818
# BB#817:                               # %if.then142.if.end151_crit_edge.i.i
                                        #   in Loop: Header=BB2_787 Depth=4
	addq	$8, %rsi
	jmp	.LBB2_819
.LBB2_818:                              # %if.then144.i.i
                                        #   in Loop: Header=BB2_787 Depth=4
	movl	8(%rsi), %r9d
	movl	8(%r12), %ecx
	addq	$8, %rsi
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	xorq	%rax, %rax
	movabsq	$.L.str88, %rsi
	movq	168(%rsp), %rdx         # 8-byte Reload
	movq	216(%rsp), %r8          # 8-byte Reload
	callq	fprintf
	movq	56(%rsp), %rsi          # 8-byte Reload
.LBB2_819:                              # %if.end151.i.i
                                        #   in Loop: Header=BB2_787 Depth=4
	movq	%r15, %rdi
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r14,8), %rax
	movl	(%rax), %ecx
	movl	(%rsi), %edx
	cmpq	%rdx, %rcx
	jne	.LBB2_821
# BB#820:                               # %if.then159.i.i
                                        #   in Loop: Header=BB2_787 Depth=4
	movl	8(%r12), %ecx
	movl	%ecx, (%rax)
.LBB2_821:                              # %if.end168.i.i
                                        #   in Loop: Header=BB2_787 Depth=4
	movq	%rdi, %r15
	movq	184(%rsp), %rsi         # 8-byte Reload
	movq	168(%rsp), %rdx         # 8-byte Reload
	callq	try_copy_prop
	.align	16, 0x90
.LBB2_822:                              # %try_swap_copy_prop.exit.i
                                        #   in Loop: Header=BB2_787 Depth=4
	movq	200(%rsp), %rax         # 8-byte Reload
	notq	%rax
	movq	208(%rsp), %rdx         # 8-byte Reload
	andq	%rax, %rdx
	movq	192(%rsp), %rsi         # 8-byte Reload
	je	.LBB2_824
.LBB2_823:                              # %for.inc739.i
                                        #   in Loop: Header=BB2_787 Depth=4
	incq	%rsi
	movq	%rsi, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	incq	%r13
	cmpq	$64, %rax
	jb	.LBB2_787
.LBB2_824:                              # %if.end742.i
                                        #   in Loop: Header=BB2_785 Depth=3
	movq	144(%rsp), %rcx         # 8-byte Reload
	incq	%rcx
	xorq	%rsi, %rsi
	cmpq	$2, %rcx
	jne	.LBB2_785
# BB#825:                               # %for.end745.i
                                        #   in Loop: Header=BB2_784 Depth=2
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 176(%rsp)         # 8-byte Spill
	xorq	%rsi, %rsi
	testq	%rax, %rax
	jne	.LBB2_784
.LBB2_826:                              # %do.end749.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	%r15, 160(%rsp)         # 8-byte Spill
	leaq	248(%rsp), %rdi
	callq	bitmap_clear
	movq	120(%rsp), %r13         # 8-byte Reload
	.align	16, 0x90
.LBB2_827:                              # %for.inc751.i
                                        #   in Loop: Header=BB2_666 Depth=1
	movq	96(%rsp), %rbx          # 8-byte Reload
	incq	%rbx
	movslq	136(%r13), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB2_666
# BB#828:                               # %for.end753.i
	movq	48(%rsp), %r12          # 8-byte Reload
	testq	%r12, %r12
	je	.LBB2_837
# BB#829:                               # %for.end753.i
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB2_837
# BB#830:                               # %if.then759.i
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, 232(%rsp)
	movq	%r12, 240(%rsp)
	movq	160(%rsp), %rcx         # 8-byte Reload
	movq	192(%rcx), %rax
	movq	%rax, 296(%rsp)
	movq	200(%rcx), %rcx
	cmpq	%rcx, %rax
	je	.LBB2_837
# BB#831:                               # %for.body765.lr.ph.i
	movabsq	$replace_label, %r14
	leaq	296(%rsp), %r15
	leaq	232(%rsp), %rbx
.LBB2_832:                              # %for.body765.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	for_each_rtx
	movq	296(%rsp), %rax
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB2_833
# BB#834:                               # %land.lhs.true771.i
                                        #   in Loop: Header=BB2_832 Depth=1
	movq	64(%rax), %rcx
	movq	64(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %rcx
	movq	160(%rsp), %rcx         # 8-byte Reload
	jne	.LBB2_836
# BB#835:                               # %if.then777.i
                                        #   in Loop: Header=BB2_832 Depth=1
	movq	%r12, 64(%rax)
	movq	296(%rsp), %rax
	jmp	.LBB2_836
.LBB2_833:                              #   in Loop: Header=BB2_832 Depth=1
	movq	160(%rsp), %rcx         # 8-byte Reload
.LBB2_836:                              # %for.inc782.i
                                        #   in Loop: Header=BB2_832 Depth=1
	movq	24(%rax), %rax
	movq	%rax, 296(%rsp)
	movq	200(%rcx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB2_832
.LBB2_837:                              # %if.end787.i
	callq	cselib_finish
	movq	160(%rsp), %rsi         # 8-byte Reload
.LBB2_838:                              # %load_mems.exit
	movq	%rsi, %r12
	callq	max_reg_num
	movq	$32, %rcx
	movq	128(%rsp), %r15         # 8-byte Reload
	movslq	184(%r15), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jle	.LBB2_840
# BB#839:                               # %if.then1606
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	loop_regs_scan
.LBB2_840:                              # %for.cond1608
	movq	88(%rsp), %rcx          # 8-byte Reload
	.align	16, 0x90
.LBB2_841:                              # %for.cond1608
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rbx
	movq	16(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB2_843
# BB#842:                               # %land.rhs1613
                                        #   in Loop: Header=BB2_841 Depth=1
	movzbl	(%rcx), %eax
	cmpq	$36, %rax
	jne	.LBB2_841
.LBB2_843:                              # %for.end1627
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	24(%rax), %r14
	movl	loop_max_reg(%rip), %edx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	reg_scan_update
	callq	max_reg_num
	movl	flag_strength_reduce(%rip), %ecx
	movl	%eax, loop_max_reg(%rip)
	testq	%rcx, %rcx
	je	.LBB2_851
# BB#844:                               # %if.then1633
	testq	%r14, %r14
	je	.LBB2_850
# BB#845:                               # %land.lhs.true1635
	movzbl	(%r14), %eax
	cmpq	$36, %rax
	movq	%r12, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	jne	.LBB2_847
# BB#846:                               # %if.then1640
	movl	32(%r14), %eax
	incq	%rax
	movl	%eax, 32(%r14)
.LBB2_847:                              # %land.lhs.true1648
	callq	strength_reduce
	movl	loop_max_reg(%rip), %edx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	reg_scan_update
	callq	max_reg_num
	movl	%eax, loop_max_reg(%rip)
	movzbl	(%r14), %eax
	cmpq	$36, %rax
	jne	.LBB2_851
# BB#848:                               # %land.lhs.true1653
	movl	32(%r14), %eax
	decq	%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movl	%eax, 32(%r14)
	jne	.LBB2_851
# BB#849:                               # %if.then1660
	movq	%r14, %rdi
	callq	delete_related_insns
	jmp	.LBB2_851
	.align	16, 0x90
.LBB2_751:                              # %tailrecurse.us.i.i2391
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB2_751
.LBB2_850:                              # %if.end1663.critedge
	movq	%r12, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	strength_reduce
	movl	loop_max_reg(%rip), %edx
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	reg_scan_update
	callq	max_reg_num
	movl	%eax, loop_max_reg(%rip)
.LBB2_851:                              # %if.end1663
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB2_853
	.align	16, 0x90
.LBB2_852:                              # %for.body.i2405
                                        # =>This Inner Loop Header: Depth=1
	movq	72(%rdi), %rbx
	callq	free
	testq	%rbx, %rbx
	movq	%rbx, %rdi
	jne	.LBB2_852
.LBB2_853:                              # %loop_movables_free.exit
	movq	192(%r15), %rdi
	callq	free
	movq	$0, 192(%r15)
	xorq	%rax, %rax
	movl	%eax, 184(%r15)
.LBB2_854:                              # %return
	addq	$336, %rsp              # imm = 0x150
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_78:                               # %cond.false93
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.scan_loop, %rcx
	movq	$707, %rsi              # imm = 0x2C3
	jmp	.LBB2_79
.LBB2_856:                              # %cond.false.i2189
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.labels_in_range_p, %rcx
	movq	$3120, %rsi             # imm = 0xC30
	jmp	.LBB2_79
.LBB2_855:                              # %cond.false761
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.scan_loop, %rcx
	movq	$931, %rsi              # imm = 0x3A3
	jmp	.LBB2_79
.LBB2_863:                              # %cond.false1246
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.scan_loop, %rcx
	movq	$1027, %rsi             # imm = 0x403
	jmp	.LBB2_79
.LBB2_501:                              # %if.then506.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.move_movables, %rcx
	movq	$2081, %rsi             # imm = 0x821
	jmp	.LBB2_79
.LBB2_857:                              # %cond.false784.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.move_movables, %rcx
	movq	$2191, %rsi             # imm = 0x88F
	jmp	.LBB2_79
.LBB2_858:                              # %cond.false816.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.move_movables, %rcx
	movq	$2196, %rsi             # imm = 0x894
	jmp	.LBB2_79
.LBB2_859:                              # %cond.false76.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.load_mems, %rcx
	movq	$9637, %rsi             # imm = 0x25A5
	jmp	.LBB2_79
.LBB2_860:                              # %cond.false112.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.load_mems, %rcx
	movq	$9638, %rsi             # imm = 0x25A6
.LBB2_79:                               # %cond.false93
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp38:
	.size	scan_loop, .Ltmp38-scan_loop
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI2_0:
	.quad	.LBB2_41
	.quad	.LBB2_29
	.quad	.LBB2_24
	.quad	.LBB2_52
	.quad	.LBB2_52
	.quad	.LBB2_22
.LJTI2_1:
	.quad	.LBB2_134
	.quad	.LBB2_166
	.quad	.LBB2_134
	.quad	.LBB2_167
	.quad	.LBB2_167

	.text
	.globl	record_excess_regs
	.align	16, 0x90
	.type	record_excess_regs,@function
record_excess_regs:                     # @record_excess_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp45:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp46:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp47:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp48:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp49:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp50:
	.cfi_def_cfa_offset 64
.Ltmp51:
	.cfi_offset %rbx, -48
.Ltmp52:
	.cfi_offset %r12, -40
.Ltmp53:
	.cfi_offset %r13, -32
.Ltmp54:
	.cfi_offset %r14, -24
.Ltmp55:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movzbl	(%r15), %eax
	movq	%rdx, %rbx
	movq	%rsi, %r12
	leaq	-54(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$15, %rcx
	ja	.LBB3_6
# BB#1:                                 # %entry
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$57395, %rdx            # imm = 0xE033
	jne	.LBB3_14
# BB#2:                                 # %entry
	cmpq	$7, %rcx
	jne	.LBB3_6
# BB#3:                                 # %sw.bb1
	movl	8(%r15), %eax
	cmpq	$53, %rax
	jb	.LBB3_14
# BB#4:                                 # %land.lhs.true
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB3_14
# BB#5:                                 # %if.then
	movq	(%rbx), %rcx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, (%rbx)
	jmp	.LBB3_14
.LBB3_6:                                # %sw.epilog
	movzbl	rtx_length(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB3_14
# BB#7:                                 # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	.align	16, 0x90
.LBB3_8:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_11 Depth 2
	movq	%rdx, %r13
	movq	(%rsp), %rax            # 8-byte Reload
	movsbq	-1(%rax,%r13), %rax
	leaq	-1(%r13), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$101, %rax
	jne	.LBB3_9
# BB#12:                                # %sw.bb25
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	(%r15,%r13,8), %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	record_excess_regs
	jmp	.LBB3_13
	.align	16, 0x90
.LBB3_9:                                # %for.body
                                        #   in Loop: Header=BB3_8 Depth=1
	cmpq	$69, %rax
	jne	.LBB3_13
# BB#10:                                # %for.cond12.preheader
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	(%r15,%r13,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r14
	jle	.LBB3_13
	.align	16, 0x90
.LBB3_11:                               # %for.body18
                                        #   Parent Loop BB3_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r14,8), %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	record_excess_regs
	movq	(%r15,%r13,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r14
	cmpq	%rdx, %rsi
	jl	.LBB3_11
	.align	16, 0x90
.LBB3_13:                               # %for.cond.backedge
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	$32, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB3_8
.LBB3_14:                               # %for.end31
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp56:
	.size	record_excess_regs, .Ltmp56-record_excess_regs
	.cfi_endproc

	.globl	libcall_other_reg
	.align	16, 0x90
	.type	libcall_other_reg,@function
libcall_other_reg:                      # @libcall_other_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp63:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp64:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp65:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp66:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp67:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp68:
	.cfi_def_cfa_offset 64
.Ltmp69:
	.cfi_offset %rbx, -48
.Ltmp70:
	.cfi_offset %r12, -40
.Ltmp71:
	.cfi_offset %r13, -32
.Ltmp72:
	.cfi_offset %r14, -24
.Ltmp73:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r12
	movq	$6, %rsi
	xorq	%rdx, %rdx
	xorq	%r15, %r15
                                        # kill: RDI<def> R12<kill>
	callq	find_reg_note
	movq	8(%rax), %rbx
	cmpq	%r12, %rbx
	movq	$0, 8(%rsp)
	je	.LBB4_6
# BB#1:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	leaq	8(%rsp), %r15
	.align	16, 0x90
.LBB4_2:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	addq	$-32, %rax
	andq	%r13, %rax
	cmpq	$2, %rax
	ja	.LBB4_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	32(%rbx), %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	record_excess_regs
.LBB4_4:                                # %if.end
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	24(%rbx), %rbx
	cmpq	%r12, %rbx
	jne	.LBB4_2
# BB#5:                                 # %while.cond.while.end_crit_edge
	movq	8(%rsp), %r15
.LBB4_6:                                # %while.end
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp74:
	.size	libcall_other_reg, .Ltmp74-libcall_other_reg
	.cfi_endproc

	.globl	loop_invariant_p
	.align	16, 0x90
	.type	loop_invariant_p,@function
loop_invariant_p:                       # @loop_invariant_p
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
	subq	$32, %rsp
.Ltmp86:
	.cfi_def_cfa_offset 80
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
	movq	%rsi, %r15
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	testq	%r15, %r15
	movq	$1, %rbx
	je	.LBB5_5
# BB#1:                                 # %if.end
	movl	(%r15), %ecx
	movq	%rcx, %r14
	andq	$65535, %r14            # imm = 0xFFFF
	cmpq	$53, %r14
	jg	.LBB5_6
# BB#2:                                 # %if.end
	cmpq	$41, %r14
	jne	.LBB5_3
# BB#28:                                # %sw.bb64
	xorq	%rbx, %rbx
	testq	$134217728, %rcx        # imm = 0x8000000
	jne	.LBB5_5
	jmp	.LBB5_29
.LBB5_6:                                # %if.end
	leaq	-54(%r14), %rdx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rdx
	cmpq	$15, %rdx
	ja	.LBB5_29
# BB#7:                                 # %if.end
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	160(%rax), %rax
	jmpq	*.LJTI5_0(,%rdx,8)
.LBB5_11:                               # %sw.bb6
	movq	global_rtl+24(%rip), %rcx
	cmpq	%r15, %rcx
	je	.LBB5_15
# BB#12:                                # %sw.bb6
	movq	global_rtl+32(%rip), %rcx
	cmpq	%r15, %rcx
	je	.LBB5_15
# BB#13:                                # %sw.bb6
	movq	global_rtl+40(%rip), %rcx
	cmpq	%r15, %rcx
	je	.LBB5_15
# BB#14:                                # %sw.bb6
	movq	pic_offset_table_rtx(%rip), %rcx
	cmpq	%r15, %rcx
	jne	.LBB5_16
.LBB5_15:                               # %land.lhs.true
	movq	cfun(%rip), %rcx
	movzbl	425(%rcx), %ecx
	testq	$2, %rcx
	je	.LBB5_5
.LBB5_16:                               # %if.end17
	movl	(%rax), %edx
	movl	8(%r15), %ecx
	testq	%rdx, %rdx
	je	.LBB5_19
# BB#17:                                # %if.end17
	cmpq	$52, %rcx
	ja	.LBB5_19
# BB#18:                                # %land.lhs.true22
	movzbl	call_used_regs(%rcx), %edx
	xorq	%rbx, %rbx
	testq	%rdx, %rdx
	jne	.LBB5_5
.LBB5_19:                               # %if.end29
	movq	192(%rax), %rax
	leaq	(%rcx,%rcx,2), %rcx
	movslq	(%rax,%rcx,8), %rax
	testq	%rax, %rax
	movq	$2, %rbx
	js	.LBB5_5
# BB#20:                                # %if.end38
	testq	%r13, %rax
	jne	.LBB5_21
# BB#22:                                # %if.end38
	movabsq	$1, %rbx
	jmp	.LBB5_5
.LBB5_3:                                # %if.end
	cmpq	$43, %r14
	jne	.LBB5_29
.LBB5_4:                                # %sw.bb5
	xorq	%rbx, %rbx
	jmp	.LBB5_5
.LBB5_23:                               # %sw.bb48
	xorq	%rbx, %rbx
	testq	$134217728, %rcx        # imm = 0x8000000
	jne	.LBB5_5
# BB#24:                                # %if.end54
	movq	120(%rax), %r12
	testq	%r12, %r12
	je	.LBB5_29
# BB#25:
	xorq	%rbx, %rbx
.LBB5_26:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r12), %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movabsq	$rtx_varies_p, %rcx
	callq	true_dependence
	testq	%r13, %rax
	jne	.LBB5_5
# BB#27:                                # %if.end60
                                        #   in Loop: Header=BB5_26 Depth=1
	movq	16(%r12), %r12
	testq	%r12, %r12
	jne	.LBB5_26
.LBB5_29:                               # %sw.epilog
	movzbl	rtx_length(%r14), %r12d
	xorq	%r13, %r13
	testq	%r12, %r12
	je	.LBB5_39
# BB#30:                                # %for.body.lr.ph.lr.ph
	movq	rtx_format(,%r14,8), %r14
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
.LBB5_31:                               # %for.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_32 Depth 2
                                        #     Child Loop BB5_41 Depth 2
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	decq	%r12
.LBB5_32:                               # %for.body
                                        #   Parent Loop BB5_31 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r14,%r12), %eax
	cmpq	$101, %rax
	jne	.LBB5_33
# BB#36:                                # %if.then83
                                        #   in Loop: Header=BB5_32 Depth=2
	movq	8(%r15,%r12,8), %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	loop_invariant_p
	xorq	%rbx, %rbx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB5_5
# BB#37:                                # %if.then83
                                        #   in Loop: Header=BB5_32 Depth=2
	movq	$1, %r13
	andq	%rcx, %rax
	cmpq	$2, %rax
	je	.LBB5_38
.LBB5_34:                               # %for.cond.backedge
                                        #   in Loop: Header=BB5_32 Depth=2
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%r12
	testq	%rax, %rax
	jg	.LBB5_32
	jmp	.LBB5_35
.LBB5_33:                               # %for.body
                                        #   in Loop: Header=BB5_32 Depth=2
	cmpq	$69, %rax
	jne	.LBB5_34
# BB#40:                                # %for.cond104.preheader
                                        #   in Loop: Header=BB5_31 Depth=1
	movq	8(%r15,%r12,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %rdi
	movq	16(%rsp), %r13          # 8-byte Reload
	jle	.LBB5_38
.LBB5_41:                               # %for.body110
                                        #   Parent Loop BB5_31 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%rdi,8), %rsi
	movq	%rdi, %r14
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	loop_invariant_p
	xorq	%rbx, %rbx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB5_5
# BB#42:                                # %for.body110
                                        #   in Loop: Header=BB5_41 Depth=2
	andq	%rcx, %rax
	cmpq	$2, %rax
	jne	.LBB5_44
# BB#43:                                # %if.then125
                                        #   in Loop: Header=BB5_41 Depth=2
	movq	$1, %r13
.LBB5_44:                               # %for.inc
                                        #   in Loop: Header=BB5_41 Depth=2
	movq	8(%r15,%r12,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r14, %rdi
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rdi
	cmpq	%rdx, %rsi
	movq	8(%rsp), %r14           # 8-byte Reload
	jl	.LBB5_41
.LBB5_38:                               # %for.cond.outer.backedge
                                        #   in Loop: Header=BB5_31 Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r13, 16(%rsp)          # 8-byte Spill
	jg	.LBB5_31
	jmp	.LBB5_39
.LBB5_8:                                # %sw.bb3
	movl	flag_unroll_loops(%rip), %eax
	testq	%rax, %rax
	jne	.LBB5_9
# BB#10:                                # %sw.bb3
	movabsq	$1, %rbx
	jmp	.LBB5_5
.LBB5_9:
	movabsq	$0, %rbx
	jmp	.LBB5_5
.LBB5_35:
	movq	16(%rsp), %r13          # 8-byte Reload
.LBB5_39:                               # %for.end130
	incq	%r13
	movq	%r13, %rbx
.LBB5_5:                                # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB5_21:
	movabsq	$0, %rbx
	jmp	.LBB5_5
.Ltmp92:
	.size	loop_invariant_p, .Ltmp92-loop_invariant_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI5_0:
	.quad	.LBB5_5
	.quad	.LBB5_5
	.quad	.LBB5_29
	.quad	.LBB5_29
	.quad	.LBB5_5
	.quad	.LBB5_4
	.quad	.LBB5_29
	.quad	.LBB5_11
	.quad	.LBB5_29
	.quad	.LBB5_29
	.quad	.LBB5_29
	.quad	.LBB5_29
	.quad	.LBB5_23
	.quad	.LBB5_8
	.quad	.LBB5_5
	.quad	.LBB5_4

	.text
	.globl	for_each_insn_in_loop
	.align	16, 0x90
	.type	for_each_insn_in_loop,@function
for_each_insn_in_loop:                  # @for_each_insn_in_loop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp99:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp100:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp101:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp102:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp103:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp104:
	.cfi_def_cfa_offset 96
.Ltmp105:
	.cfi_offset %rbx, -48
.Ltmp106:
	.cfi_offset %r12, -40
.Ltmp107:
	.cfi_offset %r13, -32
.Ltmp108:
	.cfi_offset %r14, -24
.Ltmp109:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	216(%r12), %rdi
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	callq	prev_nonnote_insn
	movq	192(%r12), %rdi
	movq	%rax, %rbx
	callq	prev_nonnote_insn
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	cmpq	%rax, %rbx
	je	.LBB6_2
# BB#1:                                 # %if.then
	movq	216(%r12), %rsi
	movq	%r12, %rdi
	callq	back_branch_in_range_p
	movq	%rax, 40(%rsp)          # 8-byte Spill
.LBB6_2:                                # %if.end
	movq	216(%r12), %rcx
	movq	200(%r12), %rdx
	movq	24(%rcx), %rax
	cmpq	%rdx, %rax
	jne	.LBB6_4
# BB#3:                                 # %if.then.i
	movq	208(%r12), %rax
.LBB6_4:                                # %next_insn_in_loop.exit
	xorq	%rsi, %rsi
	cmpq	%rcx, %rax
	je	.LBB6_6
# BB#5:                                 # %next_insn_in_loop.exit
	movq	%rax, %rsi
.LBB6_6:                                # %next_insn_in_loop.exit
	testq	%rsi, %rsi
	je	.LBB6_75
# BB#7:                                 # %for.body.lr.ph
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	xorq	%r15, %r15
	.align	16, 0x90
.LBB6_8:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_11 Depth 2
                                        #       Child Loop BB6_25 Depth 3
                                        #     Child Loop BB6_48 Depth 2
                                        #     Child Loop BB6_80 Depth 2
	movq	%r12, %rdi
	movq	%r15, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	*24(%rsp)               # 8-byte Folded Reload
	movq	%rax, %r13
	movzbl	(%r13), %eax
	cmpq	$36, %rax
	jne	.LBB6_38
# BB#9:                                 # %while.body.preheader
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	24(%r13), %r14
	movq	216(%r12), %r9
	cmpq	%r9, %r14
	jne	.LBB6_11
# BB#10:                                #   in Loop: Header=BB6_8 Depth=1
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	jmp	.LBB6_38
	.align	16, 0x90
.LBB6_11:                               # %if.end13
                                        #   Parent Loop BB6_8 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB6_25 Depth 3
	movq	200(%r12), %rax
	cmpq	%rax, %r14
	jne	.LBB6_16
# BB#12:                                # %if.then15
                                        #   in Loop: Header=BB6_11 Depth=2
	movq	208(%r12), %r14
	testq	%r14, %r14
	je	.LBB6_13
# BB#14:                                # %if.then15
                                        #   in Loop: Header=BB6_11 Depth=2
	cmpq	%r9, %r14
	je	.LBB6_15
.LBB6_16:                               # %if.end24
                                        #   in Loop: Header=BB6_11 Depth=2
	movzbl	(%r14), %eax
	cmpq	$33, %rax
	jne	.LBB6_17
# BB#19:                                # %land.lhs.true
                                        #   in Loop: Header=BB6_11 Depth=2
	movq	32(%r14), %rax
	movzbl	(%rax), %eax
	cmpq	$51, %rax
	je	.LBB6_17
# BB#20:                                # %land.lhs.true34
                                        #   in Loop: Header=BB6_11 Depth=2
	movq	%r14, %rdi
	callq	any_condjump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, 40(%rsp)            # 8-byte Folded Spill
	je	.LBB6_38
# BB#21:                                # %lor.lhs.false
                                        #   in Loop: Header=BB6_11 Depth=2
	movq	64(%r14), %r10
	movq	216(%r12), %r9
	testq	%r10, %r10
	je	.LBB6_17
# BB#22:                                # %land.lhs.true40
                                        #   in Loop: Header=BB6_11 Depth=2
	cmpq	%r9, %r10
	jne	.LBB6_24
# BB#23:                                #   in Loop: Header=BB6_11 Depth=2
	movq	%r10, %r9
	.align	16, 0x90
.LBB6_17:                               # %while.body.backedge
                                        #   in Loop: Header=BB6_11 Depth=2
	movq	24(%r14), %r14
	cmpq	%r9, %r14
	jne	.LBB6_11
	jmp	.LBB6_18
.LBB6_24:                               # %land.lhs.true46
                                        #   in Loop: Header=BB6_11 Depth=2
	movl	max_uid_for_loop(%rip), %r8d
	movq	%r13, %rdi
	movq	%r10, %r11
	jmp	.LBB6_25
.LBB6_36:                               # %if.then56.i290
                                        #   in Loop: Header=BB6_25 Depth=3
	movq	24(%r11), %r11
	.align	16, 0x90
.LBB6_25:                               # %for.cond.i256
                                        #   Parent Loop BB6_8 Depth=1
                                        #     Parent Loop BB6_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	cmpq	%r13, %r11
	je	.LBB6_38
# BB#26:                                # %for.cond.i256
                                        #   in Loop: Header=BB6_25 Depth=3
	testq	%rdi, %rdi
	je	.LBB6_38
# BB#27:                                # %if.end.i259
                                        #   in Loop: Header=BB6_25 Depth=3
	cmpq	%r10, %rdi
	je	.LBB6_17
# BB#28:                                # %if.end.i259
                                        #   in Loop: Header=BB6_25 Depth=3
	testq	%r11, %r11
	je	.LBB6_17
# BB#29:                                # %if.end5.i263
                                        #   in Loop: Header=BB6_25 Depth=3
	movslq	8(%rdi), %rsi
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rsi
	jge	.LBB6_30
# BB#31:                                # %land.lhs.true.i265
                                        #   in Loop: Header=BB6_25 Depth=3
	movslq	8(%r11), %rbx
	movq	$32, %rcx
	movq	%r8, %rdx
	movl	(%rdi), %eax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	%rdx, %rbx
	jge	.LBB6_33
# BB#32:                                # %land.lhs.true.i265
                                        #   in Loop: Header=BB6_25 Depth=3
	cmpq	$37, %rax
	jne	.LBB6_37
.LBB6_33:                               # %lor.lhs.false42.i281
                                        #   in Loop: Header=BB6_25 Depth=3
	cmpq	$37, %rax
	jne	.LBB6_35
	jmp	.LBB6_34
	.align	16, 0x90
.LBB6_30:                               # %if.end5.i263.if.then47.i284_crit_edge
                                        #   in Loop: Header=BB6_25 Depth=3
	movl	8(%r11), %ebx
.LBB6_34:                               # %if.then47.i284
                                        #   in Loop: Header=BB6_25 Depth=3
	movq	24(%rdi), %rdi
.LBB6_35:                               # %if.end50.i287
                                        #   in Loop: Header=BB6_25 Depth=3
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rax, %rbx
	jl	.LBB6_25
	jmp	.LBB6_36
.LBB6_37:                               # %loop_insn_first_p.exit292
                                        #   in Loop: Header=BB6_11 Depth=2
	movq	uid_luid(%rip), %rax
	movslq	(%rax,%rsi,4), %rcx
	movslq	(%rax,%rbx,4), %rax
	cmpq	%rax, %rcx
	jle	.LBB6_17
	jmp	.LBB6_38
.LBB6_18:                               #   in Loop: Header=BB6_8 Depth=1
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	jmp	.LBB6_38
.LBB6_13:                               #   in Loop: Header=BB6_8 Depth=1
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	jmp	.LBB6_38
.LBB6_15:                               #   in Loop: Header=BB6_8 Depth=1
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB6_38:                               # %if.end54
                                        #   in Loop: Header=BB6_8 Depth=1
	movzbl	(%r13), %eax
	cmpq	$33, %rax
	jne	.LBB6_52
# BB#39:                                # %land.lhs.true58
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	64(%r13), %rax
	movq	208(%r12), %rcx
	cmpq	%rcx, %rax
	jne	.LBB6_45
# BB#40:                                # %land.lhs.true64
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	24(%r13), %rax
	movq	24(%rax), %rdx
	movq	200(%r12), %rcx
	cmpq	%rcx, %rdx
	jne	.LBB6_43
# BB#41:                                # %land.lhs.true73
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	%r13, %rdi
	callq	any_uncondjump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB6_52
# BB#42:                                # %land.lhs.true73.lor.lhs.false76_crit_edge
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	24(%r13), %rax
	movq	200(%r12), %rcx
.LBB6_43:                               # %lor.lhs.false76
                                        #   in Loop: Header=BB6_8 Depth=1
	cmpq	%rcx, %rax
	jne	.LBB6_45
# BB#44:                                # %land.lhs.true82
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	%r13, %rdi
	callq	any_condjump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB6_52
	.align	16, 0x90
.LBB6_45:                               # %if.then85
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	232(%r12), %rax
	testq	%rax, %rax
	movq	$1, %r14
	jne	.LBB6_47
# BB#46:                                #   in Loop: Header=BB6_8 Depth=1
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movl	(%r13), %eax
	jmp	.LBB6_64
	.align	16, 0x90
.LBB6_52:                               # %if.else104
                                        #   in Loop: Header=BB6_8 Depth=1
	movl	(%r13), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$37, %rcx
	jne	.LBB6_53
# BB#54:                                # %if.then108
                                        #   in Loop: Header=BB6_8 Depth=1
	movl	40(%r13), %ecx
	movabsq	$4294967203, %rdx       # imm = 0xFFFFFFA3
	cmpq	%rdx, %rcx
	jne	.LBB6_55
# BB#58:                                # %land.lhs.true117
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	16(%rsp), %r14          # 8-byte Reload
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %r14
	jne	.LBB6_63
	jmp	.LBB6_64
	.align	16, 0x90
.LBB6_53:                               #   in Loop: Header=BB6_8 Depth=1
	movq	%r15, %r14
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	jmp	.LBB6_64
.LBB6_47:                               # %for.body88.lr.ph
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	64(%r13), %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB6_48:                               # %for.body88
                                        #   Parent Loop BB6_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rax), %rdx
	cmpq	%rcx, %rdx
	je	.LBB6_49
# BB#50:                                # %for.inc
                                        #   in Loop: Header=BB6_48 Depth=2
	movq	16(%rax), %rax
	testq	%rax, %rax
	jne	.LBB6_48
# BB#51:                                # %for.end
                                        #   in Loop: Header=BB6_8 Depth=1
	movl	(%r13), %eax
	jmp	.LBB6_64
.LBB6_55:                               # %lor.lhs.false112
                                        #   in Loop: Header=BB6_8 Depth=1
	movabsq	$4294967202, %rdx       # imm = 0xFFFFFFA2
	cmpq	%rdx, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	jne	.LBB6_59
# BB#56:                                # %lor.lhs.false112
                                        #   in Loop: Header=BB6_8 Depth=1
	xorq	%r14, %r14
	movq	16(%rsp), %rdx          # 8-byte Reload
	testq	%rsi, %rdx
	jne	.LBB6_59
# BB#57:                                #   in Loop: Header=BB6_8 Depth=1
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	jmp	.LBB6_64
.LBB6_49:                               #   in Loop: Header=BB6_8 Depth=1
	movq	%r15, %r14
	movl	(%r13), %eax
	jmp	.LBB6_64
.LBB6_59:                               # %if.else120
                                        #   in Loop: Header=BB6_8 Depth=1
	movabsq	$4294967201, %rdx       # imm = 0xFFFFFFA1
	cmpq	%rdx, %rcx
	jne	.LBB6_60
# BB#62:                                # %if.then131
                                        #   in Loop: Header=BB6_8 Depth=1
	decq	16(%rsp)                # 8-byte Folded Spill
	jmp	.LBB6_63
.LBB6_60:                               # %if.else120
                                        #   in Loop: Header=BB6_8 Depth=1
	movabsq	$4294967200, %rdx       # imm = 0xFFFFFFA0
	cmpq	%rdx, %rcx
	jne	.LBB6_63
# BB#61:                                # %if.then125
                                        #   in Loop: Header=BB6_8 Depth=1
	incq	16(%rsp)                # 8-byte Folded Spill
.LBB6_63:                               # %if.end136
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	%r15, %r14
	.align	16, 0x90
.LBB6_64:                               # %if.end136
                                        #   in Loop: Header=BB6_8 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$33, %rax
	jne	.LBB6_66
# BB#65:                                # %land.lhs.true140
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	192(%r12), %rcx
	movq	64(%r13), %rdx
	movq	24(%rcx), %rcx
	cmpq	%rcx, %rdx
	movq	$1, %rcx
	je	.LBB6_67
.LBB6_66:                               # %select.mid
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	32(%rsp), %rcx          # 8-byte Reload
.LBB6_67:                               # %if.end150
                                        #   in Loop: Header=BB6_8 Depth=1
	testq	%rsi, %rcx
	jne	.LBB6_68
# BB#76:                                # %if.end150
                                        #   in Loop: Header=BB6_8 Depth=1
	testq	%rsi, %r14
	je	.LBB6_68
# BB#77:                                # %if.end150
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	cmpq	$36, %rax
	jne	.LBB6_69
# BB#78:                                # %land.lhs.true158
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	200(%r12), %rsi
	movq	%r13, %rdi
	callq	no_labels_between_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_69
# BB#79:                                # %land.lhs.true162
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	176(%r12), %r8
	movl	max_uid_for_loop(%rip), %r9d
	movq	%r13, %rsi
	movq	%r8, %r10
	jmp	.LBB6_80
	.align	16, 0x90
.LBB6_68:                               #   in Loop: Header=BB6_8 Depth=1
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	jmp	.LBB6_69
.LBB6_92:                               # %if.then56.i
                                        #   in Loop: Header=BB6_80 Depth=2
	movq	24(%r10), %r10
	.align	16, 0x90
.LBB6_80:                               # %for.cond.i
                                        #   Parent Loop BB6_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r13, %r10
	je	.LBB6_69
# BB#81:                                # %for.cond.i
                                        #   in Loop: Header=BB6_80 Depth=2
	testq	%rsi, %rsi
	je	.LBB6_69
# BB#82:                                # %if.end.i
                                        #   in Loop: Header=BB6_80 Depth=2
	xorq	%r15, %r15
	cmpq	%r8, %rsi
	je	.LBB6_70
# BB#83:                                # %if.end.i
                                        #   in Loop: Header=BB6_80 Depth=2
	testq	%r10, %r10
	je	.LBB6_70
# BB#84:                                # %if.end5.i
                                        #   in Loop: Header=BB6_80 Depth=2
	movslq	8(%rsi), %rax
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB6_85
# BB#86:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB6_80 Depth=2
	movslq	8(%r10), %rbx
	movq	$32, %rcx
	movq	%r9, %rdi
	movl	(%rsi), %edx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	%rdi, %rbx
	jge	.LBB6_89
# BB#87:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB6_80 Depth=2
	cmpq	$37, %rdx
	jne	.LBB6_88
.LBB6_89:                               # %lor.lhs.false42.i
                                        #   in Loop: Header=BB6_80 Depth=2
	cmpq	$37, %rdx
	jne	.LBB6_91
	jmp	.LBB6_90
	.align	16, 0x90
.LBB6_85:                               # %if.end5.i.if.then47.i_crit_edge
                                        #   in Loop: Header=BB6_80 Depth=2
	movl	8(%r10), %ebx
.LBB6_90:                               # %if.then47.i
                                        #   in Loop: Header=BB6_80 Depth=2
	movq	24(%rsi), %rsi
.LBB6_91:                               # %if.end50.i
                                        #   in Loop: Header=BB6_80 Depth=2
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rax, %rbx
	jl	.LBB6_80
	jmp	.LBB6_92
.LBB6_88:                               # %cond.true26.i
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	uid_luid(%rip), %rcx
	movslq	(%rcx,%rax,4), %rax
	movslq	(%rcx,%rbx,4), %rcx
	xorq	%r15, %r15
	cmpq	%rcx, %rax
	jle	.LBB6_70
	.align	16, 0x90
.LBB6_69:                               # %for.inc167
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	%r14, %r15
.LBB6_70:                               # %for.inc167
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	24(%r13), %rax
	movq	200(%r12), %rcx
	cmpq	%rcx, %rax
	jne	.LBB6_72
# BB#71:                                # %if.then.i236
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	208(%r12), %rax
.LBB6_72:                               # %next_insn_in_loop.exit241
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	216(%r12), %rcx
	xorq	%rsi, %rsi
	cmpq	%rcx, %rax
	je	.LBB6_74
# BB#73:                                # %select.mid70
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	%rax, %rsi
.LBB6_74:                               # %select.end69
                                        #   in Loop: Header=BB6_8 Depth=1
	testq	%rsi, %rsi
	jne	.LBB6_8
.LBB6_75:                               # %for.end169
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp110:
	.size	for_each_insn_in_loop, .Ltmp110-for_each_insn_in_loop
	.cfi_endproc

	.globl	loop_insn_first_p
	.align	16, 0x90
	.type	loop_insn_first_p,@function
loop_insn_first_p:                      # @loop_insn_first_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp113:
	.cfi_def_cfa_offset 16
.Ltmp114:
	.cfi_offset %rbx, -16
	movl	max_uid_for_loop(%rip), %r8d
	movq	%rdi, %rbx
	movq	%rsi, %r9
	jmp	.LBB7_1
.LBB7_16:                               # %if.then56
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	24(%r9), %r9
	.align	16, 0x90
.LBB7_1:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rax, %rax
	cmpq	%rdi, %r9
	je	.LBB7_11
# BB#2:                                 # %for.cond
                                        #   in Loop: Header=BB7_1 Depth=1
	testq	%rbx, %rbx
	je	.LBB7_11
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB7_1 Depth=1
	cmpq	%rsi, %rbx
	movq	$1, %rax
	je	.LBB7_11
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB7_1 Depth=1
	testq	%r9, %r9
	je	.LBB7_11
# BB#5:                                 # %if.end5
                                        #   in Loop: Header=BB7_1 Depth=1
	movslq	8(%rbx), %r11
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r11
	jge	.LBB7_14
# BB#6:                                 # %land.lhs.true
                                        #   in Loop: Header=BB7_1 Depth=1
	movslq	8(%r9), %r10
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r10
	jge	.LBB7_13
# BB#7:                                 # %land.lhs.true11
                                        #   in Loop: Header=BB7_1 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB7_8
# BB#12:                                # %if.end36
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r11
	jge	.LBB7_14
.LBB7_13:                               # %lor.lhs.false42
                                        #   in Loop: Header=BB7_1 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB7_15
.LBB7_14:                               # %if.then47
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	24(%rbx), %rbx
.LBB7_15:                               # %if.end50
                                        #   in Loop: Header=BB7_1 Depth=1
	movslq	8(%r9), %rax
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB7_1
	jmp	.LBB7_16
.LBB7_8:                                # %cond.true26
	movq	uid_luid(%rip), %rax
	movslq	(%rax,%r11,4), %rcx
	movslq	(%rax,%r10,4), %rax
	cmpq	%rax, %rcx
	jg	.LBB7_9
# BB#10:                                # %cond.true26
	movabsq	$1, %rax
.LBB7_11:                               # %return
	popq	%rbx
	retq
.LBB7_9:
	movabsq	$0, %rax
	popq	%rbx
	retq
.Ltmp115:
	.size	loop_insn_first_p, .Ltmp115-loop_insn_first_p
	.cfi_endproc

	.globl	express_from
	.align	16, 0x90
	.type	express_from,@function
express_from:                           # @express_from
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp122:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp123:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp124:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp125:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp126:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp127:
	.cfi_def_cfa_offset 64
.Ltmp128:
	.cfi_offset %rbx, -48
.Ltmp129:
	.cfi_offset %r12, -40
.Ltmp130:
	.cfi_offset %r13, -32
.Ltmp131:
	.cfi_offset %r14, -24
.Ltmp132:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movq	64(%r13), %rdi
	movq	%rsi, %r14
	movzbl	(%rdi), %eax
	movq	64(%r14), %rsi
	cmpq	$54, %rax
	jne	.LBB8_5
# BB#1:                                 # %land.lhs.true
	movzbl	(%rsi), %eax
	cmpq	$54, %rax
	jne	.LBB8_5
# BB#2:                                 # %if.then
	movq	const_int_rtx+512(%rip), %rax
	xorq	%r15, %r15
	cmpq	%rax, %rdi
	je	.LBB8_16
# BB#3:                                 # %lor.lhs.false
	movq	8(%rsi), %rcx
	movq	8(%rdi), %rsi
	movq	%rcx, %rax
	cqto
	idivq	%rsi
	testq	%rdx, %rdx
	jne	.LBB8_16
# BB#4:                                 # %if.end
	movq	%rcx, %rax
	cqto
	idivq	%rsi
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r12
	jmp	.LBB8_7
.LBB8_5:                                # %if.else
	callq	rtx_equal_p
	xorq	%r15, %r15
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB8_16
# BB#6:                                 # %if.then25
	movq	const_int_rtx+520(%rip), %r12
.LBB8_7:                                # %if.end28
	movq	72(%r13), %rdi
	movq	72(%r14), %rsi
	movq	%r12, %rdx
	callq	express_from_1
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB8_14
# BB#8:                                 # %if.then32
	movq	8(%r12), %rax
	xorq	%r15, %r15
	cmpq	$2, %rax
	jl	.LBB8_16
# BB#9:                                 # %if.then37
	movq	72(%r13), %rdx
	movl	(%rdx), %ebx
	movq	%rbx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$78, %rcx
	jne	.LBB8_12
# BB#10:                                # %land.lhs.true42
	movq	16(%rdx), %rcx
	movzbl	(%rcx), %esi
	cmpq	$54, %rsi
	jne	.LBB8_12
# BB#11:                                # %if.then48
	movq	8(%rcx), %rsi
	imulq	%rax, %rsi
	movq	8(%rdx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$78, %rdi
	movq	%rbx, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rax, %rcx
	jmp	.LBB8_13
.LBB8_12:                               # %if.else65
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movq	$78, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rcx
.LBB8_13:                               # %if.end74
	callq	gen_rtx_fmt_ee
	movq	72(%r14), %rsi
	movq	const_int_rtx+520(%rip), %rdx
	movq	%rax, %rdi
	callq	express_from_1
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB8_16
.LBB8_14:                               # %if.end77
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %r12
	je	.LBB8_15
# BB#17:                                # %if.else80
	movq	const_int_rtx+520(%rip), %rax
	cmpq	%rax, %r12
	je	.LBB8_18
# BB#19:                                # %if.else83
	movq	32(%r13), %rdx
	movl	48(%r14), %esi
	movq	$78, %rdi
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r15
	jmp	.LBB8_20
.LBB8_15:
	movq	%rbx, %r15
	jmp	.LBB8_16
.LBB8_18:                               # %if.then82
	movq	32(%r13), %r15
.LBB8_20:                               # %if.end87
	cmpq	%rcx, %rbx
	je	.LBB8_16
# BB#21:                                # %if.else90
	movzbl	(%rbx), %eax
	cmpq	$75, %rax
	jne	.LBB8_22
# BB#23:                                # %land.lhs.true94
	movq	16(%rbx), %r12
	movzbl	(%r12), %eax
	cmpq	$133, %rax
	jg	.LBB8_28
# BB#24:                                # %land.lhs.true94
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	jbe	.LBB8_26
# BB#25:
	movq	%rbx, %r12
	jmp	.LBB8_32
.LBB8_16:                               # %return
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB8_22:
	movq	%rbx, %r12
	jmp	.LBB8_32
.LBB8_28:                               # %land.lhs.true94
	cmpq	$134, %rax
	je	.LBB8_31
# BB#29:                                # %land.lhs.true94
	cmpq	$140, %rax
	je	.LBB8_31
# BB#30:
	movq	%rbx, %r12
	jmp	.LBB8_32
.LBB8_26:                               # %land.lhs.true94
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	je	.LBB8_27
.LBB8_31:                               # %if.then150
	movq	8(%rbx), %rcx
	movl	48(%r14), %esi
	movq	$75, %rdi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
.LBB8_32:                               # %if.end159
	movl	48(%r14), %esi
	movq	$75, %rdi
	movq	%r15, %rdx
	movq	%r12, %rcx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.LBB8_27:
	movq	%rbx, %r12
	jmp	.LBB8_32
.Ltmp133:
	.size	express_from, .Ltmp133-express_from
	.cfi_endproc

	.align	16, 0x90
	.type	express_from_1,@function
express_from_1:                         # @express_from_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp140:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp141:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp142:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp143:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp144:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp145:
	.cfi_def_cfa_offset 64
.Ltmp146:
	.cfi_offset %rbx, -48
.Ltmp147:
	.cfi_offset %r12, -40
.Ltmp148:
	.cfi_offset %r13, -32
.Ltmp149:
	.cfi_offset %r14, -24
.Ltmp150:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rax
	movq	%rsi, %rbx
	movq	%rdi, %r14
	cmpq	%rdx, %rax
	je	.LBB9_1
# BB#2:                                 # %if.end
	movl	(%r14), %eax
	movq	const_int_rtx+520(%rip), %rdi
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	xorq	%rsi, %rsi
	cmpq	%rdx, %rdi
	je	.LBB9_4
# BB#3:                                 # %if.end
	cmpq	$54, %rcx
	jne	.LBB9_42
.LBB9_4:                                # %while.cond.preheader
	movq	%rdx, (%rsp)            # 8-byte Spill
	cmpq	$75, %rcx
	jne	.LBB9_5
	.align	16, 0x90
.LBB9_6:                                # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %ecx
	cmpq	$75, %rcx
	jne	.LBB9_5
# BB#7:                                 # %while.body
                                        #   in Loop: Header=BB9_6 Depth=1
	movq	8(%r14), %rax
	movzbl	(%rax), %ecx
	movq	16(%r14), %r15
	cmpq	$75, %rcx
	movq	%rax, %rcx
	je	.LBB9_9
# BB#8:                                 # %while.body
                                        #   in Loop: Header=BB9_6 Depth=1
	movq	%r15, %rcx
.LBB9_9:                                # %while.body
                                        #   in Loop: Header=BB9_6 Depth=1
	je	.LBB9_11
# BB#10:                                # %while.body
                                        #   in Loop: Header=BB9_6 Depth=1
	movq	%rax, %r15
.LBB9_11:                               # %while.body
                                        #   in Loop: Header=BB9_6 Depth=1
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	8(%rbx), %rdx
	movzbl	(%rdx), %ecx
	movq	16(%rbx), %rax
	cmpq	$75, %rcx
	movq	%rax, %r12
	je	.LBB9_13
# BB#12:                                # %while.body
                                        #   in Loop: Header=BB9_6 Depth=1
	movq	%rdx, %r12
.LBB9_13:                               # %while.body
                                        #   in Loop: Header=BB9_6 Depth=1
	je	.LBB9_15
# BB#14:                                # %while.body
                                        #   in Loop: Header=BB9_6 Depth=1
	movq	%rax, %rdx
.LBB9_15:                               # %while.body
                                        #   in Loop: Header=BB9_6 Depth=1
	movq	%rdx, %r13
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%r13, %rdx
	testq	%rcx, %rax
	jne	.LBB9_16
# BB#24:                                # %if.else
                                        #   in Loop: Header=BB9_6 Depth=1
	movzbl	(%rdx), %eax
	cmpq	$75, %rax
	je	.LBB9_27
# BB#25:                                # %land.lhs.true34
                                        #   in Loop: Header=BB9_6 Depth=1
	movq	%r15, %rdi
	movq	%rdx, %rsi
	movq	%rdx, %r13
	callq	rtx_equal_p
	movq	%r13, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB9_26
.LBB9_27:                               # %if.else38
                                        #   in Loop: Header=BB9_6 Depth=1
	movq	8(%rsp), %rdi           # 8-byte Reload
	movzbl	(%rdi), %eax
	cmpq	$75, %rax
	je	.LBB9_29
# BB#28:                                # %land.lhs.true42
                                        #   in Loop: Header=BB9_6 Depth=1
	movq	%r12, %rsi
	movq	%rdx, %r13
	callq	rtx_equal_p
	movq	%r13, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB9_17
	jmp	.LBB9_29
.LBB9_26:                               #   in Loop: Header=BB9_6 Depth=1
	movq	%r12, %rdx
	.align	16, 0x90
.LBB9_16:                               # %while.cond.backedge
                                        #   in Loop: Header=BB9_6 Depth=1
	movq	8(%rsp), %r15           # 8-byte Reload
.LBB9_17:                               # %while.cond.backedge
                                        #   in Loop: Header=BB9_6 Depth=1
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$75, %rcx
	movq	%r15, %r14
	movq	%rdx, %rbx
	je	.LBB9_6
	jmp	.LBB9_18
.LBB9_1:
	movq	%rbx, %rsi
	movq	%rsi, %rax
	jmp	.LBB9_50
.LBB9_5:
	movq	%r14, %r15
	movq	%rbx, %rdx
.LBB9_18:                               # %while.end
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	jne	.LBB9_19
# BB#33:                                # %if.then95
	movq	8(%r15), %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	movq	8(%rcx), %rsi
	negq	%rax
	imulq	%rax, %rsi
	movq	%rdx, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	plus_constant_wide  # TAILCALL
.LBB9_19:                               # %while.end
	cmpq	$75, %rcx
	jne	.LBB9_34
# BB#20:                                # %if.then60
	movq	8(%r15), %r14
	movq	16(%r15), %rbx
	movq	%rbx, %rdi
	movq	%rdx, %rsi
	movq	%rdx, %r12
	callq	rtx_equal_p
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB9_31
# BB#21:
	movq	%r14, %rbx
	movq	(%rsp), %rcx            # 8-byte Reload
	jmp	.LBB9_22
.LBB9_34:                               # %if.else105
	leaq	-67(%rcx), %rsi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rsi
	cmpq	$2, %rsi
	jb	.LBB9_45
# BB#35:                                # %lor.lhs.false112
	cmpq	$133, %rcx
	jg	.LBB9_43
# BB#36:                                # %lor.lhs.false112
	cmpq	$58, %rcx
	ja	.LBB9_38
# BB#37:                                # %lor.lhs.false112
	movq	$1, %rsi
	shlq	%cl, %rsi
	movabsq	$414331165718085632, %rcx # imm = 0x5C0000000000000
	testq	%rcx, %rsi
	jne	.LBB9_45
	jmp	.LBB9_38
.LBB9_29:                               # %if.else46
	movq	%r14, %rdi
	movq	%rdx, %rsi
	movq	(%rsp), %rdx            # 8-byte Reload
	callq	express_from_1
	testq	%rax, %rax
	movabsq	$0, %rsi
	je	.LBB9_42
# BB#30:                                # %if.end50
	movzbl	2(%rbx), %esi
	movq	$75, %rdi
	movq	%r12, %rdx
	movq	%rax, %rcx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.LBB9_31:                               # %if.else74
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	rtx_equal_p
	testq	%r15, %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	je	.LBB9_32
.LBB9_22:                               # %if.end79
	movzbl	(%rbx), %eax
	cmpq	$54, %rax
	movabsq	$0, %rsi
	jne	.LBB9_42
# BB#23:                                # %if.end84
	movq	8(%rbx), %rax
	movq	8(%rcx), %rsi
	negq	%rax
	imulq	%rax, %rsi
	xorq	%rdi, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_CONST_INT  # TAILCALL
.LBB9_43:                               # %lor.lhs.false112
	cmpq	$134, %rcx
	je	.LBB9_45
# BB#44:                                # %lor.lhs.false112
	cmpq	$140, %rcx
	jne	.LBB9_38
.LBB9_45:                               # %if.then136
	movzbl	2(%rdx), %esi
	movq	$16, %rcx
	shrq	%cl, %rax
	testq	%rsi, %rsi
	jne	.LBB9_47
# BB#46:
	andq	$255, %rax
	movq	%rax, %rsi
.LBB9_47:                               # %if.then136
	movq	$76, %rdi
	movq	%r15, %rcx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	simplify_gen_binary  # TAILCALL
.LBB9_38:                               # %if.else148
	movzbl	(%rdx), %eax
	cmpq	$75, %rax
	jne	.LBB9_48
# BB#39:                                # %if.then152
	movq	8(%rdx), %rsi
	movq	%r15, %rdi
	movq	%rdx, %r14
	callq	rtx_equal_p
	movq	%r14, %rcx
	movq	16(%rcx), %rsi
	testq	%rbx, %rax
	jne	.LBB9_42
# BB#40:                                # %if.else162
	movq	%rcx, %r14
	movq	%r15, %rdi
	callq	rtx_equal_p
	testq	%rbx, %rax
	movabsq	$0, %rsi
	je	.LBB9_42
# BB#41:                                # %if.then168
	movq	8(%r14), %rsi
.LBB9_42:                               # %return
	movq	%rsi, %rax
.LBB9_50:                               # %if.else173
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB9_32:
	xorq	%rsi, %rsi
	movq	%rsi, %rax
	jmp	.LBB9_50
.LBB9_48:                               # %if.else173
	movq	%r15, %rdi
	movq	%rdx, %rsi
	callq	rtx_equal_p
	andq	%rbx, %rax
	je	.LBB9_50
# BB#49:                                # %if.else173
	movq	const_int_rtx+512(%rip), %rax
	jmp	.LBB9_50
.Ltmp151:
	.size	express_from_1, .Ltmp151-express_from_1
	.cfi_endproc

	.globl	extend_value_for_giv
	.align	16, 0x90
	.type	extend_value_for_giv,@function
extend_value_for_giv:                   # @extend_value_for_giv
	.cfi_startproc
# BB#0:                                 # %entry
	movq	120(%rdi), %rdx
	movq	%rsi, %rax
	testq	%rdx, %rdx
	je	.LBB10_8
# BB#1:                                 # %if.end
	movl	(%rax), %esi
	movq	%rsi, %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$133, %rdi
	jg	.LBB10_5
# BB#2:                                 # %if.end
	addq	$-54, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$14, %rcx
	ja	.LBB10_4
# BB#3:                                 # %if.end
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	$24599, %rdi            # imm = 0x6017
	jne	.LBB10_7
	jmp	.LBB10_4
.LBB10_5:                               # %if.end
	cmpq	$134, %rdi
	je	.LBB10_7
# BB#6:                                 # %if.end
	cmpq	$140, %rdi
	jne	.LBB10_4
.LBB10_7:                               # %land.lhs.true
	testq	$16711680, %rsi         # imm = 0xFF0000
	je	.LBB10_8
.LBB10_4:                               # %if.end32
	movl	(%rdx), %esi
	movq	%rsi, %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%rax, %rdx
	jmp	gen_rtx_fmt_e  # TAILCALL
.LBB10_8:                               # %return
	retq
.Ltmp152:
	.size	extend_value_for_giv, .Ltmp152-extend_value_for_giv
	.cfi_endproc

	.globl	loop_iv_add_mult_emit_before
	.align	16, 0x90
	.type	loop_iv_add_mult_emit_before,@function
loop_iv_add_mult_emit_before:           # @loop_iv_add_mult_emit_before
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp159:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp160:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp161:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp162:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp163:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp164:
	.cfi_def_cfa_offset 64
.Ltmp165:
	.cfi_offset %rbx, -48
.Ltmp166:
	.cfi_offset %r12, -40
.Ltmp167:
	.cfi_offset %r13, -32
.Ltmp168:
	.cfi_offset %r14, -24
.Ltmp169:
	.cfi_offset %r15, -16
	movq	64(%rsp), %rax
	movq	%r8, %rbx
	movq	%rcx, %r13
	movq	%rdx, %r15
	movq	%rsi, %r12
	testq	%rax, %rax
	je	.LBB11_4
# BB#1:                                 # %if.end
	movq	%r12, %rdi
	callq	copy_rtx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%r13, %rdi
	callq	copy_rtx
	movq	%rax, %r14
	callq	start_sequence
	movzbl	2(%rbx), %r8d
	movq	$1, %r9
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rbx, %rsi
	movq	(%rsp), %rdx            # 8-byte Reload
	movq	%r14, %rcx
	callq	expand_mult_add
	cmpq	%rbx, %rax
	je	.LBB11_3
# BB#2:                                 # %if.then.i
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB11_3:                               # %loop_insn_emit_before.exit
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
	movq	%r13, %rdi
	movq	64(%rsp), %rbx
	movq	%rbx, %rsi
	callq	update_reg_last_use
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	update_reg_last_use
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	update_reg_last_use
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	emit_insn_before
	movq	%r14, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	loop_regs_update  # TAILCALL
.LBB11_4:                               # %if.then
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	%r13, %rcx
	movq	%rbx, %r8
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	loop_iv_add_mult_hoist  # TAILCALL
.Ltmp170:
	.size	loop_iv_add_mult_emit_before, .Ltmp170-loop_iv_add_mult_emit_before
	.cfi_endproc

	.globl	loop_iv_add_mult_hoist
	.align	16, 0x90
	.type	loop_iv_add_mult_hoist,@function
loop_iv_add_mult_hoist:                 # @loop_iv_add_mult_hoist
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp177:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp178:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp179:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp180:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp181:
	.cfi_def_cfa_offset 48
.Ltmp182:
	.cfi_offset %rbx, -48
.Ltmp183:
	.cfi_offset %r12, -40
.Ltmp184:
	.cfi_offset %r13, -32
.Ltmp185:
	.cfi_offset %r14, -24
.Ltmp186:
	.cfi_offset %r15, -16
	movq	%r8, %rbx
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rdi, %r14
	movq	%rsi, %rdi
	callq	copy_rtx
	movq	%rax, %r13
	movq	%r12, %rdi
	callq	copy_rtx
	movq	%rax, %r12
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, %r15
	callq	start_sequence
	movzbl	2(%rbx), %r8d
	movq	$1, %r9
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	callq	expand_mult_add
	cmpq	%rbx, %rax
	je	.LBB12_2
# BB#1:                                 # %if.then.i
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB12_2:                               # %gen_add_mult.exit
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	192(%r14), %rsi
	testq	%rsi, %rsi
	je	.LBB12_3
# BB#4:                                 # %loop_insn_hoist.exit
	movq	%rbx, %rdi
	callq	emit_insn_before
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	loop_regs_update  # TAILCALL
	.align	16, 0x90
.LBB12_3:                               # %tailrecurse.us.i
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB12_3
.Ltmp187:
	.size	loop_iv_add_mult_hoist, .Ltmp187-loop_iv_add_mult_hoist
	.cfi_endproc

	.align	16, 0x90
	.type	update_reg_last_use,@function
update_reg_last_use:                    # @update_reg_last_use
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp194:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp195:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp196:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp197:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp198:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp199:
	.cfi_def_cfa_offset 64
.Ltmp200:
	.cfi_offset %rbx, -48
.Ltmp201:
	.cfi_offset %r12, -40
.Ltmp202:
	.cfi_offset %r13, -32
.Ltmp203:
	.cfi_offset %r14, -24
.Ltmp204:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movzbl	(%r13), %eax
	movq	%rsi, %r14
	cmpq	$61, %rax
	jne	.LBB13_5
# BB#1:                                 # %land.lhs.true
	movl	8(%r13), %edx
	movl	max_reg_before_loop(%rip), %ecx
	cmpq	%rcx, %rdx
	jae	.LBB13_5
# BB#2:                                 # %land.lhs.true2
	movslq	8(%r14), %rcx
	movslq	max_uid_for_loop(%rip), %rsi
	cmpq	%rsi, %rcx
	jge	.LBB13_5
# BB#3:                                 # %land.lhs.true6
	movq	reg_n_info(%rip), %rsi
	movq	32(%rsi,%rdx,8), %rdx
	movq	uid_luid(%rip), %rsi
	movslq	4(%rdx), %rdi
	movslq	(%rsi,%rdi,4), %rdi
	movslq	(%rsi,%rcx,4), %rsi
	cmpq	%rsi, %rdi
	jge	.LBB13_5
# BB#4:                                 # %if.then
	movl	%ecx, 4(%rdx)
	jmp	.LBB13_10
.LBB13_5:                               # %if.else
	movzbl	rtx_length(%rax), %r15d
	testq	%r15, %r15
	je	.LBB13_10
# BB#6:                                 # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB13_7:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_13 Depth 2
	movq	%r15, %rbx
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	-1(%rax,%rbx), %eax
	leaq	-1(%rbx), %r15
	cmpq	$69, %rax
	jne	.LBB13_8
# BB#12:                                # %if.then59
                                        #   in Loop: Header=BB13_7 Depth=1
	movq	(%r13,%rbx,8), %rax
	movslq	(%rax), %r12
	testq	%r12, %r12
	jle	.LBB13_9
	jmp	.LBB13_13
	.align	16, 0x90
.LBB13_14:                              # %for.body67.for.body67_crit_edge
                                        #   in Loop: Header=BB13_13 Depth=2
	movq	(%r13,%rbx,8), %rax
.LBB13_13:                              # %for.body67.lr.ph
                                        #   Parent Loop BB13_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%r12
	movq	8(%rax,%r12,8), %rdi
	movq	%r14, %rsi
	callq	update_reg_last_use
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_14
	jmp	.LBB13_9
	.align	16, 0x90
.LBB13_8:                               # %for.body
                                        #   in Loop: Header=BB13_7 Depth=1
	cmpq	$101, %rax
	jne	.LBB13_9
# BB#11:                                # %if.then49
                                        #   in Loop: Header=BB13_7 Depth=1
	movq	(%r13,%rbx,8), %rdi
	movq	%r14, %rsi
	callq	update_reg_last_use
.LBB13_9:                               # %for.cond.backedge
                                        #   in Loop: Header=BB13_7 Depth=1
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_7
.LBB13_10:                              # %if.end78
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp205:
	.size	update_reg_last_use, .Ltmp205-update_reg_last_use
	.cfi_endproc

	.globl	loop_insn_emit_before
	.align	16, 0x90
	.type	loop_insn_emit_before,@function
loop_insn_emit_before:                  # @loop_insn_emit_before
	.cfi_startproc
# BB#0:                                 # %entry
	testq	%rdx, %rdx
	je	.LBB14_1
# BB#2:                                 # %if.end
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	jmp	emit_insn_before  # TAILCALL
.LBB14_1:                               # %if.then
	movq	%rcx, %rsi
	jmp	loop_insn_hoist  # TAILCALL
.Ltmp206:
	.size	loop_insn_emit_before, .Ltmp206-loop_insn_emit_before
	.cfi_endproc

	.align	16, 0x90
	.type	loop_regs_update,@function
loop_regs_update:                       # @loop_regs_update
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp210:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp211:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp212:
	.cfi_def_cfa_offset 32
.Ltmp213:
	.cfi_offset %rbx, -24
.Ltmp214:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movzbl	(%r14), %eax
	cmpq	$47, %rax
	jne	.LBB15_1
# BB#11:                                # %land.lhs.true72
	movq	8(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB15_12
# BB#13:                                # %if.then80
	movq	16(%r14), %rsi
	movl	8(%rax), %edi
	xorq	%rdx, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	record_base_value  # TAILCALL
.LBB15_1:                               # %entry
	cmpq	$24, %rax
	jne	.LBB15_12
# BB#2:                                 # %for.cond.preheader
	movq	8(%r14), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB15_12
# BB#3:
	movq	$1, %rbx
	.align	16, 0x90
.LBB15_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %rdi
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB15_10
# BB#5:                                 # %cond.true
                                        #   in Loop: Header=BB15_4 Depth=1
	movq	32(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB15_7
# BB#6:                                 # %cond.false
                                        #   in Loop: Header=BB15_4 Depth=1
	movq	%rax, %rsi
	callq	single_set_2
.LBB15_7:                               # %cond.end50
                                        #   in Loop: Header=BB15_4 Depth=1
	testq	%rax, %rax
	je	.LBB15_10
# BB#8:                                 # %land.lhs.true
                                        #   in Loop: Header=BB15_4 Depth=1
	movq	8(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB15_10
# BB#9:                                 # %if.then59
                                        #   in Loop: Header=BB15_4 Depth=1
	movq	16(%rax), %rsi
	movl	8(%rcx), %edi
	xorq	%rdx, %rdx
	callq	record_base_value
	.align	16, 0x90
.LBB15_10:                              # %for.inc
                                        #   in Loop: Header=BB15_4 Depth=1
	movq	8(%r14), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB15_4
.LBB15_12:                              # %if.end91
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp215:
	.size	loop_regs_update, .Ltmp215-loop_regs_update
	.cfi_endproc

	.globl	loop_iv_add_mult_sink
	.align	16, 0x90
	.type	loop_iv_add_mult_sink,@function
loop_iv_add_mult_sink:                  # @loop_iv_add_mult_sink
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp222:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp223:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp224:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp225:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp226:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp227:
	.cfi_def_cfa_offset 80
.Ltmp228:
	.cfi_offset %rbx, -48
.Ltmp229:
	.cfi_offset %r12, -40
.Ltmp230:
	.cfi_offset %r13, -32
.Ltmp231:
	.cfi_offset %r14, -24
.Ltmp232:
	.cfi_offset %r15, -16
	movq	%r8, %r13
	movq	%rcx, %r15
	movq	%rdx, %rbx
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rdi, %r14
	movq	%rsi, %rdi
	callq	copy_rtx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r15, %rbx
	callq	copy_rtx
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	%rax, %r12
	callq	start_sequence
	movzbl	2(%r13), %r8d
	movq	$1, %r9
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r13, %rsi
	movq	%r15, %rdx
	movq	%r12, %rcx
	callq	expand_mult_add
	cmpq	%r13, %rax
	je	.LBB16_2
# BB#1:                                 # %if.then.i
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB16_2:                               # %gen_add_mult.exit
	callq	gen_sequence
	movq	%rax, %r13
	callq	end_sequence
	movq	224(%r14), %rsi
	movq	%rbx, %rdi
	callq	update_reg_last_use
	movq	224(%r14), %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	update_reg_last_use
	movq	224(%r14), %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	update_reg_last_use
	movq	224(%r14), %rsi
	testq	%rsi, %rsi
	jne	.LBB16_5
# BB#3:                                 # %if.then.i.i
	movq	192(%r14), %rsi
	testq	%rsi, %rsi
	je	.LBB16_4
.LBB16_5:                               # %if.end.i.i
	movq	%r13, %rdi
	callq	emit_insn_before
	movq	%r13, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	loop_regs_update  # TAILCALL
	.align	16, 0x90
.LBB16_4:                               # %tailrecurse.us.i.i
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB16_4
.Ltmp233:
	.size	loop_iv_add_mult_sink, .Ltmp233-loop_iv_add_mult_sink
	.cfi_endproc

	.globl	loop_insn_sink
	.align	16, 0x90
	.type	loop_insn_sink,@function
loop_insn_sink:                         # @loop_insn_sink
	.cfi_startproc
# BB#0:                                 # %entry
	movq	224(%rdi), %rax
	testq	%rax, %rax
	jne	.LBB17_3
# BB#1:                                 # %if.then.i
	movq	192(%rdi), %rax
	testq	%rax, %rax
	je	.LBB17_2
.LBB17_3:                               # %if.end.i
	movq	%rsi, %rdi
	movq	%rax, %rsi
	jmp	emit_insn_before  # TAILCALL
	.align	16, 0x90
.LBB17_2:                               # %tailrecurse.us.i
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB17_2
.Ltmp234:
	.size	loop_insn_sink, .Ltmp234-loop_insn_sink
	.cfi_endproc

	.globl	loop_insn_hoist
	.align	16, 0x90
	.type	loop_insn_hoist,@function
loop_insn_hoist:                        # @loop_insn_hoist
	.cfi_startproc
# BB#0:                                 # %entry
	movq	192(%rdi), %rax
	testq	%rax, %rax
	je	.LBB18_1
# BB#2:                                 # %if.end.i
	movq	%rsi, %rdi
	movq	%rax, %rsi
	jmp	emit_insn_before  # TAILCALL
	.align	16, 0x90
.LBB18_1:                               # %tailrecurse.us
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB18_1
.Ltmp235:
	.size	loop_insn_hoist, .Ltmp235-loop_insn_hoist
	.cfi_endproc

	.globl	canonicalize_condition
	.align	16, 0x90
	.type	canonicalize_condition,@function
canonicalize_condition:                 # @canonicalize_condition
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp242:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp243:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp244:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp245:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp246:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp247:
	.cfi_def_cfa_offset 112
.Ltmp248:
	.cfi_offset %rbx, -48
.Ltmp249:
	.cfi_offset %r12, -40
.Ltmp250:
	.cfi_offset %r13, -32
.Ltmp251:
	.cfi_offset %r14, -24
.Ltmp252:
	.cfi_offset %r15, -16
	movq	%r8, 48(%rsp)           # 8-byte Spill
	movq	%rcx, %r14
	movq	%r14, 40(%rsp)          # 8-byte Spill
	movq	%rdi, %r12
	movl	(%rsi), %edi
	movq	$16, %rcx
	movq	%rdi, %rbx
	movq	8(%rsi), %r13
	movq	16(%rsi), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	shrq	%cl, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	je	.LBB19_1
# BB#2:                                 # %if.then
	movq	%rsi, %rdi
	movq	%r12, %rsi
	callq	reversed_comparison_code
	movq	%rax, %rdi
	jmp	.LBB19_3
.LBB19_1:
	andq	$65535, %rdi            # imm = 0xFFFF
.LBB19_3:                               # %if.end
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdi
	je	.LBB19_78
# BB#4:                                 # %if.end7
	andq	$255, %rbx
	testq	%r14, %r14
	je	.LBB19_6
# BB#5:                                 # %if.then9
	movq	%r12, (%r14)
.LBB19_6:                               # %while.cond.preheader
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	jmp	.LBB19_7
.LBB19_51:                              # %if.then229
                                        #   in Loop: Header=BB19_7 Depth=1
	movq	%r12, (%r14)
	.align	16, 0x90
.LBB19_7:                               # %while.cond.outer.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_8 Depth 2
	movq	$32, %rcx
	movq	%rdi, %rax
	movq	%rdi, %rbx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	rtx_class(%rax), %r15d
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	jmp	.LBB19_8
.LBB19_13:                              # %if.then27
                                        #   in Loop: Header=BB19_8 Depth=2
	movq	8(%r13), %rax
	movq	16(%r13), %rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	%rax, %r13
	.align	16, 0x90
.LBB19_8:                               # %while.cond
                                        #   Parent Loop BB19_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rax
	andq	$255, %rax
	cmpq	$60, %rax
	jne	.LBB19_55
# BB#9:                                 # %land.lhs.true
                                        #   in Loop: Header=BB19_8 Depth=2
	movl	(%r13), %eax
	movq	$13, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$2040, %rdx             # imm = 0x7F8
	movq	const_tiny_rtx(%rdx), %rcx
	cmpq	%rcx, 56(%rsp)          # 8-byte Folded Reload
	jne	.LBB19_55
# BB#10:                                # %land.lhs.true
                                        #   in Loop: Header=BB19_8 Depth=2
	movq	48(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %r13
	je	.LBB19_55
# BB#11:                                # %while.body
                                        #   in Loop: Header=BB19_8 Depth=2
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB19_12
# BB#14:                                # %if.end40
                                        #   in Loop: Header=BB19_8 Depth=2
	movq	%r12, %rdi
	callq	prev_nonnote_insn
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB19_55
# BB#15:                                # %lor.lhs.false
                                        #   in Loop: Header=BB19_8 Depth=2
	movzbl	(%r12), %eax
	cmpq	$32, %rax
	jne	.LBB19_55
# BB#16:                                # %if.end49
                                        #   in Loop: Header=BB19_8 Depth=2
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	set_of
	movq	%rax, %r14
	testq	%r14, %r14
	jne	.LBB19_17
# BB#54:                                # %if.else195
                                        #   in Loop: Header=BB19_8 Depth=2
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	reg_set_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB19_8
	jmp	.LBB19_55
.LBB19_17:                              # %land.lhs.true52
                                        #   in Loop: Header=BB19_8 Depth=2
	movzbl	(%r14), %eax
	cmpq	$47, %rax
	jne	.LBB19_55
# BB#18:                                # %lor.lhs.false57
                                        #   in Loop: Header=BB19_8 Depth=2
	movq	8(%r14), %rdi
	movq	%r13, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB19_55
# BB#19:                                # %if.then66
                                        #   in Loop: Header=BB19_8 Depth=2
	movq	16(%r14), %r9
	movq	8(%r14), %rcx
	movzbl	(%r9), %r8d
	movzbl	2(%rcx), %ecx
	cmpq	$74, %r8
	movabsq	$0, %rax
	je	.LBB19_26
# BB#20:                                # %lor.lhs.false80
                                        #   in Loop: Header=BB19_8 Depth=2
	leaq	-102(%rbx), %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	$5, %rdx
	ja	.LBB19_55
# BB#21:                                # %lor.lhs.false80
                                        #   in Loop: Header=BB19_8 Depth=2
	movq	%rbx, %rdi
	jmpq	*.LJTI19_0(,%rdx,8)
.LBB19_22:                              # %land.lhs.true86
                                        #   in Loop: Header=BB19_8 Depth=2
	movl	mode_class(,%rcx,4), %edx
	cmpq	$1, %rdx
	movq	$107, %rdi
	jne	.LBB19_56
# BB#23:                                # %land.lhs.true91
                                        #   in Loop: Header=BB19_8 Depth=2
	movzbl	mode_bitsize(%rcx,%rcx), %edx
	cmpq	$64, %rdx
	ja	.LBB19_33
# BB#24:                                # %land.lhs.true97
                                        #   in Loop: Header=BB19_8 Depth=2
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	addq	%rsi, %rdx
	testq	%rsi, %rdx
	jne	.LBB19_33
.LBB19_25:                              # %land.lhs.true102
                                        #   in Loop: Header=BB19_8 Depth=2
	movzbl	rtx_class(%r8), %edx
	cmpq	$60, %rdx
	jne	.LBB19_33
.LBB19_26:                              # %land.lhs.true113
                                        #   in Loop: Header=BB19_8 Depth=2
	movq	32(%rsp), %rdx          # 8-byte Reload
	movl	mode_class(,%rdx,4), %edx
	cmpq	$4, %rdx
	movq	%rax, %rdx
	jne	.LBB19_28
# BB#27:                                # %land.lhs.true113
                                        #   in Loop: Header=BB19_8 Depth=2
	movabsq	$1, %rdx
.LBB19_28:                              # %land.lhs.true113
                                        #   in Loop: Header=BB19_8 Depth=2
	movl	mode_class(,%rcx,4), %esi
	cmpq	$4, %rsi
	movq	%rax, %rsi
	jne	.LBB19_30
# BB#29:                                # %land.lhs.true113
                                        #   in Loop: Header=BB19_8 Depth=2
	movabsq	$1, %rsi
.LBB19_30:                              # %land.lhs.true113
                                        #   in Loop: Header=BB19_8 Depth=2
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	je	.LBB19_45
# BB#31:                                # %land.lhs.true113
                                        #   in Loop: Header=BB19_8 Depth=2
	xorq	%rsi, %rdx
	xorq	$1, %rdx
	jne	.LBB19_45
# BB#32:                                # %land.lhs.true113
                                        #   in Loop: Header=BB19_8 Depth=2
	testq	%rcx, %rcx
	je	.LBB19_45
.LBB19_33:                              # %if.else134
                                        #   in Loop: Header=BB19_8 Depth=2
	movq	%rbx, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	$103, %rdx
	je	.LBB19_38
# BB#34:                                # %if.else134
                                        #   in Loop: Header=BB19_8 Depth=2
	cmpq	$104, %rdx
	jne	.LBB19_55
.LBB19_35:                              # %land.lhs.true140
                                        #   in Loop: Header=BB19_8 Depth=2
	movzbl	mode_bitsize(%rcx,%rcx), %edx
	cmpq	$64, %rdx
	movq	$104, %rdi
	ja	.LBB19_56
# BB#36:                                # %land.lhs.true146
                                        #   in Loop: Header=BB19_8 Depth=2
	movl	mode_class(,%rcx,4), %esi
	cmpq	$1, %rsi
	jne	.LBB19_55
# BB#37:                                # %land.lhs.true151
                                        #   in Loop: Header=BB19_8 Depth=2
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	addq	%rsi, %rdx
	testq	%rsi, %rdx
	jne	.LBB19_55
.LBB19_38:                              # %land.lhs.true160
                                        #   in Loop: Header=BB19_8 Depth=2
	movzbl	rtx_class(%r8), %edx
	cmpq	$60, %rdx
	jne	.LBB19_55
# BB#39:                                # %land.lhs.true171
                                        #   in Loop: Header=BB19_8 Depth=2
	movq	32(%rsp), %rdx          # 8-byte Reload
	movl	mode_class(,%rdx,4), %edx
	cmpq	$4, %rdx
	movq	%rax, %rdx
	jne	.LBB19_41
# BB#40:                                # %land.lhs.true171
                                        #   in Loop: Header=BB19_8 Depth=2
	movabsq	$1, %rdx
.LBB19_41:                              # %land.lhs.true171
                                        #   in Loop: Header=BB19_8 Depth=2
	movl	mode_class(,%rcx,4), %esi
	cmpq	$4, %rsi
	movq	%rax, %rsi
	jne	.LBB19_43
# BB#42:                                # %land.lhs.true171
                                        #   in Loop: Header=BB19_8 Depth=2
	movabsq	$1, %rsi
.LBB19_43:                              # %land.lhs.true171
                                        #   in Loop: Header=BB19_8 Depth=2
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	je	.LBB19_44
# BB#52:                                # %land.lhs.true171
                                        #   in Loop: Header=BB19_8 Depth=2
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	xorq	%rsi, %rdx
	xorq	$1, %rdx
	jne	.LBB19_45
# BB#53:                                # %land.lhs.true171
                                        #   in Loop: Header=BB19_8 Depth=2
	testq	%rcx, %rcx
	je	.LBB19_45
	jmp	.LBB19_55
.LBB19_12:                              # %while.body
                                        #   in Loop: Header=BB19_8 Depth=2
	cmpq	$74, %rax
	jne	.LBB19_55
	jmp	.LBB19_13
.LBB19_44:                              #   in Loop: Header=BB19_8 Depth=2
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
.LBB19_45:                              # %if.end200
                                        #   in Loop: Header=BB19_8 Depth=2
	testq	%r9, %r9
	je	.LBB19_8
# BB#46:                                # %if.then202
                                        #   in Loop: Header=BB19_7 Depth=1
	movzbl	rtx_class(%r8), %ecx
	cmpq	$60, %rcx
	movq	40(%rsp), %r14          # 8-byte Reload
	je	.LBB19_48
# BB#47:                                # %select.mid
                                        #   in Loop: Header=BB19_7 Depth=1
	movq	%rbx, %r8
.LBB19_48:                              # %select.end
                                        #   in Loop: Header=BB19_7 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	24(%rsp), %rax          # 8-byte Reload
	testq	%rcx, %rax
	movq	%r8, %rdi
	je	.LBB19_50
# BB#49:                                # %if.then215
                                        #   in Loop: Header=BB19_7 Depth=1
	movq	%r9, %rdi
	movq	%r12, %rsi
	movq	%r9, %rbx
	callq	reversed_comparison_code
	movq	%rbx, %r9
	movq	%rax, %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdi
	je	.LBB19_78
.LBB19_50:                              # %if.end221
                                        #   in Loop: Header=BB19_7 Depth=1
	movq	8(%r9), %r13
	movq	16(%r9), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	testq	%r14, %r14
	je	.LBB19_7
	jmp	.LBB19_51
.LBB19_55:
	movq	%rbx, %rdi
.LBB19_56:                              # %while.end
	movl	(%r13), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$133, %rcx
	jg	.LBB19_60
# BB#57:                                # %while.end
	addq	$-54, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB19_58
# BB#59:                                # %while.end
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB19_62
	jmp	.LBB19_58
.LBB19_60:                              # %while.end
	cmpq	$134, %rcx
	je	.LBB19_62
# BB#61:                                # %while.end
	cmpq	$140, %rcx
	jne	.LBB19_58
.LBB19_62:                              # %if.then271
	callq	swap_condition
	movq	56(%rsp), %rcx          # 8-byte Reload
	movl	(%rcx), %edx
	movq	%rax, %rdi
	movq	%r13, %rax
	jmp	.LBB19_63
.LBB19_58:
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%r13, 56(%rsp)          # 8-byte Spill
.LBB19_63:                              # %if.end273
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movl	mode_class(,%rdx,4), %ecx
	cmpq	$4, %rcx
	jne	.LBB19_64
.LBB19_78:                              # %return
	xorq	%rax, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB19_64:                              # %if.end282
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB19_77
# BB#65:                                # %if.end282
	testq	%rdx, %rdx
	je	.LBB19_77
# BB#66:                                # %land.lhs.true293
	movzbl	mode_bitsize(%rdx,%rdx), %ecx
	cmpq	$64, %rcx
	ja	.LBB19_77
# BB#67:                                # %if.then302
	leaq	-104(%rdi), %rbx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rbx
	cmpq	$6, %rbx
	ja	.LBB19_77
# BB#68:                                # %if.then302
	movq	8(%rax), %rsi
	movq	mode_mask_array(,%rdx,8), %rdx
	jmpq	*.LJTI19_1(,%rbx,8)
.LBB19_71:                              # %sw.bb315
	andq	%rsi, %rdx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	addq	%rdi, %rcx
	movq	$1, %rdi
	shlq	%cl, %rdi
	cmpq	%rdi, %rdx
	movq	$104, %rdi
	je	.LBB19_77
# BB#72:                                # %if.then328
	decq	%rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$105, %rdi
	jmp	.LBB19_77
.LBB19_69:                              # %sw.bb
	movq	$1, %rcx
	shrq	%cl, %rdx
	movq	$106, %rdi
	cmpq	%rdx, %rsi
	je	.LBB19_77
# BB#70:                                # %if.then312
	incq	%rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$107, %rdi
	jmp	.LBB19_77
.LBB19_75:                              # %sw.bb339
	movq	$108, %rdi
	testq	%rsi, %rsi
	je	.LBB19_77
# BB#76:                                # %if.then342
	decq	%rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$109, %rdi
	jmp	.LBB19_77
.LBB19_73:                              # %sw.bb332
	movq	$110, %rdi
	cmpq	%rdx, %rsi
	jae	.LBB19_77
# BB#74:                                # %if.then335
	incq	%rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$111, %rdi
.LBB19_77:                              # %if.end346
	xorq	%rsi, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp253:
	.size	canonicalize_condition, .Ltmp253-canonicalize_condition
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI19_0:
	.quad	.LBB19_25
	.quad	.LBB19_38
	.quad	.LBB19_35
	.quad	.LBB19_56
	.quad	.LBB19_56
	.quad	.LBB19_22
.LJTI19_1:
	.quad	.LBB19_71
	.quad	.LBB19_77
	.quad	.LBB19_69
	.quad	.LBB19_77
	.quad	.LBB19_75
	.quad	.LBB19_77
	.quad	.LBB19_73

	.text
	.globl	get_condition
	.align	16, 0x90
	.type	get_condition,@function
get_condition:                          # @get_condition
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp257:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp258:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp259:
	.cfi_def_cfa_offset 32
.Ltmp260:
	.cfi_offset %rbx, -24
.Ltmp261:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movzbl	(%rbx), %eax
	movq	%rsi, %r14
	cmpq	$33, %rax
	jne	.LBB20_7
# BB#1:                                 # %lor.lhs.false
	movq	%rbx, %rdi
	callq	any_condjump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB20_7
# BB#2:                                 # %if.end
	movq	%rbx, %rdi
	callq	pc_set
	movq	16(%rax), %rcx
	movq	24(%rcx), %rax
	movzbl	(%rax), %edi
	movq	8(%rcx), %rsi
	xorq	%rdx, %rdx
	cmpq	$67, %rdi
	jne	.LBB20_6
# BB#3:                                 # %land.rhs
	movq	8(%rax), %rax
	movq	64(%rbx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB20_4
# BB#5:                                 # %land.rhs
	movabsq	$1, %rdx
	jmp	.LBB20_6
.LBB20_7:                               # %return
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB20_4:
	movabsq	$0, %rdx
.LBB20_6:                               # %land.end
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	%r14, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	canonicalize_condition  # TAILCALL
.Ltmp262:
	.size	get_condition, .Ltmp262-get_condition
	.cfi_endproc

	.globl	get_condition_for_loop
	.align	16, 0x90
	.type	get_condition_for_loop,@function
get_condition_for_loop:                 # @get_condition_for_loop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp268:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp269:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp270:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp271:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp272:
	.cfi_def_cfa_offset 48
.Ltmp273:
	.cfi_offset %rbx, -40
.Ltmp274:
	.cfi_offset %r12, -32
.Ltmp275:
	.cfi_offset %r14, -24
.Ltmp276:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movzbl	(%rbx), %eax
	movq	%rdi, %r14
	xorq	%r15, %r15
	cmpq	$33, %rax
	jne	.LBB21_10
# BB#1:                                 # %lor.lhs.false.i
	movq	%rbx, %rdi
	callq	any_condjump_p
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB21_10
# BB#2:                                 # %if.end.i
	movq	%rbx, %rdi
	callq	pc_set
	movq	16(%rax), %rcx
	movq	24(%rcx), %rax
	movzbl	(%rax), %edx
	movq	8(%rcx), %rsi
	xorq	%r15, %r15
	cmpq	$67, %rdx
	movabsq	$0, %rdx
	jne	.LBB21_6
# BB#3:                                 # %land.rhs.i
	movq	8(%rax), %rax
	movq	64(%rbx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB21_4
# BB#5:                                 # %land.rhs.i
	movabsq	$1, %rdx
	jmp	.LBB21_6
.LBB21_4:
	movabsq	$0, %rdx
.LBB21_6:                               # %get_condition.exit
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rdi
	callq	canonicalize_condition
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB21_10
# BB#7:                                 # %lor.lhs.false
	movq	8(%rbx), %rsi
	movq	%r14, %rdi
	callq	loop_invariant_p
	testq	%r12, %rax
	je	.LBB21_9
# BB#8:                                 # %lor.lhs.false2
	movq	16(%rbx), %rsi
	movq	%r14, %rdi
	callq	loop_invariant_p
	testq	%r12, %rax
	je	.LBB21_11
.LBB21_9:
	movq	%rbx, %r15
.LBB21_10:                              # %return
	movq	%r15, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB21_11:                              # %if.end
	movzbl	(%rbx), %edi
	callq	swap_condition
	movq	8(%rbx), %rcx
	movq	16(%rbx), %rdx
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp277:
	.size	get_condition_for_loop, .Ltmp277-get_condition_for_loop
	.cfi_endproc

	.globl	debug_ivs
	.align	16, 0x90
	.type	debug_ivs,@function
debug_ivs:                              # @debug_ivs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp281:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp282:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp283:
	.cfi_def_cfa_offset 32
.Ltmp284:
	.cfi_offset %rbx, -24
.Ltmp285:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB22_8
# BB#1:                                 # %entry
	movq	stderr(%rip), %r14
	testq	%r14, %r14
	je	.LBB22_8
# BB#2:                                 # %if.end.i
	movq	160(%rbx), %rax
	movq	224(%rax), %rax
	xorq	%rcx, %rcx
	jmp	.LBB22_4
	.align	16, 0x90
.LBB22_3:                               # %for.body.i
                                        #   in Loop: Header=BB22_4 Depth=1
	movq	64(%rax), %rax
	incq	%rcx
.LBB22_4:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB22_3
# BB#5:                                 # %for.end.i
	movl	(%rbx), %edx
	movabsq	$.L.str32, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movq	160(%rbx), %rax
	movq	224(%rax), %rbx
	jmp	.LBB22_7
	.align	16, 0x90
.LBB22_6:                               # %for.body8.i
                                        #   in Loop: Header=BB22_7 Depth=1
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	loop_iv_class_dump
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	64(%rbx), %rbx
.LBB22_7:                               # %for.body8.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB22_6
.LBB22_8:                               # %loop_ivs_dump.exit
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp286:
	.size	debug_ivs, .Ltmp286-debug_ivs
	.cfi_endproc

	.globl	debug_iv_class
	.align	16, 0x90
	.type	debug_iv_class,@function
debug_iv_class:                         # @debug_iv_class
	.cfi_startproc
# BB#0:                                 # %entry
	movq	stderr(%rip), %rsi
	jmp	loop_iv_class_dump  # TAILCALL
.Ltmp287:
	.size	debug_iv_class, .Ltmp287-debug_iv_class
	.cfi_endproc

	.align	16, 0x90
	.type	loop_iv_class_dump,@function
loop_iv_class_dump:                     # @loop_iv_class_dump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp294:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp295:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp296:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp297:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp298:
	.cfi_def_cfa_offset 48
.Ltmp299:
	.cfi_offset %rbx, -48
.Ltmp300:
	.cfi_offset %r12, -40
.Ltmp301:
	.cfi_offset %r13, -32
.Ltmp302:
	.cfi_offset %r14, -24
.Ltmp303:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r14
	testq	%r14, %r14
	je	.LBB24_21
# BB#1:                                 # %entry
	testq	%r15, %r15
	je	.LBB24_21
# BB#2:                                 # %if.end
	movl	32(%r14), %ecx
	movl	(%r14), %edx
	movabsq	$.L.str24, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	fprintf
	movq	72(%r14), %rax
	movl	8(%rax), %edx
	movabsq	$.L.str25, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	fprintf
	cmpq	$0, 40(%r14)
	je	.LBB24_4
# BB#3:                                 # %if.then4
	movabsq	$.L.str26, %rdi
	movq	$12, %rsi
	movq	$1, %rdx
	movq	%r15, %rcx
	callq	fwrite
	movq	40(%r14), %rsi
	movq	%r15, %rdi
	callq	print_simple_rtl
.LBB24_4:                               # %if.end7
	cmpq	$0, 48(%r14)
	je	.LBB24_6
# BB#5:                                 # %if.then9
	movabsq	$.L.str27, %rdi
	movq	$13, %rsi
	movq	$1, %rdx
	movq	%r15, %rcx
	callq	fwrite
	movq	48(%r14), %rsi
	movq	%r15, %rdi
	callq	print_simple_rtl
.LBB24_6:                               # %if.end12
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	fputc
	cmpq	$0, 56(%r14)
	je	.LBB24_8
# BB#7:                                 # %if.then15
	movabsq	$.L.str28, %rdi
	movq	$12, %rsi
	movq	$1, %rdx
	movq	%r15, %rcx
	callq	fwrite
	movq	56(%r14), %rsi
	movq	%r15, %rdi
	callq	print_simple_rtl
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	fputc
.LBB24_8:                               # %if.end19
	movq	%r14, %rdi
	callq	biv_total_increment
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB24_10
# BB#9:                                 # %if.then22
	movabsq	$.L.str29, %rdi
	movq	$18, %rsi
	movq	$1, %rdx
	movq	%r15, %rcx
	callq	fwrite
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	print_simple_rtl
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	fputc
.LBB24_10:                              # %if.end25
	movq	8(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB24_13
# BB#11:
	xorq	%r13, %r13
	movabsq	$.L.str30, %r12
	.align	16, 0x90
.LBB24_12:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rax
	movl	8(%rax), %ecx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	fprintf
	movq	72(%rbx), %rsi
	movq	%r15, %rdi
	callq	print_simple_rtl
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	fputc
	movq	128(%rbx), %rbx
	incq	%r13
	testq	%rbx, %rbx
	jne	.LBB24_12
.LBB24_13:                              # %for.end
	movq	24(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB24_21
# BB#14:
	xorq	%r12, %r12
	movabsq	$.L.str31, %r14
	.align	16, 0x90
.LBB24_15:                              # %for.body34
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rax
	movl	80(%rbx), %r8d
	movl	8(%rax), %ecx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	fprintf
	movl	24(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB24_17
# BB#16:                                # %if.then40
                                        #   in Loop: Header=BB24_15 Depth=1
	movq	56(%rbx), %rsi
	jmp	.LBB24_20
	.align	16, 0x90
.LBB24_17:                              # %if.else
                                        #   in Loop: Header=BB24_15 Depth=1
	movq	(%rbx), %rdi
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	xorq	%rsi, %rsi
	cmpq	$105, %rax
	jne	.LBB24_20
# BB#18:                                # %cond.true
                                        #   in Loop: Header=BB24_15 Depth=1
	movq	32(%rdi), %rsi
	movzbl	(%rsi), %eax
	cmpq	$47, %rax
	je	.LBB24_20
# BB#19:                                # %cond.false
                                        #   in Loop: Header=BB24_15 Depth=1
	callq	single_set_2
	movq	%rax, %rsi
	.align	16, 0x90
.LBB24_20:                              # %cond.end64
                                        #   in Loop: Header=BB24_15 Depth=1
	movq	%r15, %rdi
	callq	print_simple_rtl
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	fputc
	movq	128(%rbx), %rbx
	incq	%r12
	testq	%rbx, %rbx
	jne	.LBB24_15
.LBB24_21:                              # %for.end71
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp304:
	.size	loop_iv_class_dump, .Ltmp304-loop_iv_class_dump
	.cfi_endproc

	.globl	debug_biv
	.align	16, 0x90
	.type	debug_biv,@function
debug_biv:                              # @debug_biv
	.cfi_startproc
# BB#0:                                 # %entry
	movq	stderr(%rip), %rsi
	movq	$1, %rdx
	jmp	loop_biv_dump  # TAILCALL
.Ltmp305:
	.size	debug_biv, .Ltmp305-debug_biv
	.cfi_endproc

	.align	16, 0x90
	.type	loop_biv_dump,@function
loop_biv_dump:                          # @loop_biv_dump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp310:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp311:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp312:
	.cfi_def_cfa_offset 32
.Ltmp313:
	.cfi_offset %rbx, -32
.Ltmp314:
	.cfi_offset %r14, -24
.Ltmp315:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB26_6
# BB#1:                                 # %entry
	testq	%r14, %r14
	je	.LBB26_6
# BB#2:                                 # %if.end
	movq	(%rbx), %rax
	movq	32(%rbx), %rdx
	movl	8(%rax), %ecx
	movl	8(%rdx), %edx
	movabsq	$.L.str22, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movabsq	$.L.str23, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movq	72(%rbx), %rsi
	movq	%r14, %rdi
	callq	print_simple_rtl
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	je	.LBB26_5
# BB#3:                                 # %land.lhs.true
	cmpq	$0, 88(%rbx)
	je	.LBB26_5
# BB#4:                                 # %if.then7
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	fputc
	movabsq	$.L.str21, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movq	88(%rbx), %rsi
	movq	%r14, %rdi
	callq	print_simple_rtl
.LBB26_5:                               # %if.end11
	movq	$10, %rdi
	movq	%r14, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.LBB26_6:                               # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp316:
	.size	loop_biv_dump, .Ltmp316-loop_biv_dump
	.cfi_endproc

	.globl	debug_giv
	.align	16, 0x90
	.type	debug_giv,@function
debug_giv:                              # @debug_giv
	.cfi_startproc
# BB#0:                                 # %entry
	movq	stderr(%rip), %rsi
	movq	$1, %rdx
	jmp	loop_giv_dump  # TAILCALL
.Ltmp317:
	.size	debug_giv, .Ltmp317-debug_giv
	.cfi_endproc

	.align	16, 0x90
	.type	loop_giv_dump,@function
loop_giv_dump:                          # @loop_giv_dump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp322:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp323:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp324:
	.cfi_def_cfa_offset 32
.Ltmp325:
	.cfi_offset %rbx, -32
.Ltmp326:
	.cfi_offset %r14, -24
.Ltmp327:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r14
	testq	%r14, %r14
	je	.LBB28_22
# BB#1:                                 # %entry
	testq	%rbx, %rbx
	je	.LBB28_22
# BB#2:                                 # %if.end
	movl	24(%r14), %eax
	cmpq	$1, %rax
	jne	.LBB28_4
# BB#3:                                 # %if.then2
	movq	(%r14), %rax
	movq	32(%r14), %rdx
	movl	8(%rax), %ecx
	movl	8(%rdx), %edx
	movabsq	$.L.str10, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fprintf
	jmp	.LBB28_5
.LBB28_22:                              # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB28_4:                               # %if.else
	movq	(%r14), %rax
	movl	8(%rax), %edx
	movabsq	$.L.str11, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fprintf
.LBB28_5:                               # %if.end10
	movq	16(%r14), %rax
	movl	80(%r14), %ecx
	movl	8(%rax), %edx
	movabsq	$.L.str12, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fprintf
	movl	104(%r14), %edx
	movabsq	$.L.str13, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fprintf
	movzbl	100(%r14), %eax
	testq	$1, %rax
	je	.LBB28_7
# BB#6:                                 # %if.then17
	movabsq	$.L.str14, %rdi
	movq	$12, %rsi
	movq	$1, %rdx
	movq	%rbx, %rcx
	callq	fwrite
	movzbl	100(%r14), %eax
.LBB28_7:                               # %if.end19
	testq	$2048, %rax             # imm = 0x800
	je	.LBB28_9
# BB#8:                                 # %if.then24
	movabsq	$.L.str15, %rdi
	movq	$5, %rsi
	movq	$1, %rdx
	movq	%rbx, %rcx
	callq	fwrite
.LBB28_9:                               # %if.end26
	movq	120(%r14), %rax
	testq	%rax, %rax
	je	.LBB28_15
# BB#10:                                # %if.then28
	movzbl	(%rax), %eax
	cmpq	$122, %rax
	je	.LBB28_20
# BB#11:                                # %if.then28
	cmpq	$121, %rax
	jne	.LBB28_12
# BB#19:                                # %sw.bb33
	movabsq	$.L.str17, %rdi
	jmp	.LBB28_14
.LBB28_20:                              # %sw.bb35
	movabsq	$.L.str18, %rdi
	jmp	.LBB28_14
.LBB28_12:                              # %if.then28
	cmpq	$120, %rax
	jne	.LBB28_21
# BB#13:                                # %sw.bb
	movabsq	$.L.str16, %rdi
.LBB28_14:                              # %if.end37
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%rbx, %rcx
	callq	fwrite
.LBB28_15:                              # %if.end37
	movq	$10, %rdi
	movq	%rbx, %rsi
	callq	fputc
	movabsq	$.L.str19, %rdi
	movq	$6, %rsi
	movq	$1, %rdx
	movq	%rbx, %rcx
	callq	fwrite
	movq	64(%r14), %rsi
	movq	%rbx, %rdi
	callq	print_simple_rtl
	movq	$10, %rdi
	movq	%rbx, %rsi
	callq	fputc
	movabsq	$.L.str20, %rdi
	movq	$6, %rsi
	movq	$1, %rdx
	movq	%rbx, %rcx
	callq	fwrite
	movq	72(%r14), %rsi
	movq	%rbx, %rdi
	callq	print_simple_rtl
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	je	.LBB28_18
# BB#16:                                # %land.lhs.true
	cmpq	$0, 88(%r14)
	je	.LBB28_18
# BB#17:                                # %if.then44
	movq	$10, %rdi
	movq	%rbx, %rsi
	callq	fputc
	movabsq	$.L.str21, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%rbx, %rcx
	callq	fwrite
	movq	88(%r14), %rsi
	movq	%rbx, %rdi
	callq	print_simple_rtl
.LBB28_18:                              # %if.end48
	movq	$10, %rdi
	movq	%rbx, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.LBB28_21:                              # %sw.default
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.loop_giv_dump, %rcx
	movq	$10533, %rsi            # imm = 0x2925
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp328:
	.size	loop_giv_dump, .Ltmp328-loop_giv_dump
	.cfi_endproc

	.globl	debug_loop
	.align	16, 0x90
	.type	debug_loop,@function
debug_loop:                             # @debug_loop
	.cfi_startproc
# BB#0:                                 # %entry
	movq	stderr(%rip), %rsi
	movabsq	$loop_dump_aux, %rdx
	movq	$1, %rcx
	jmp	flow_loop_dump  # TAILCALL
.Ltmp329:
	.size	debug_loop, .Ltmp329-debug_loop
	.cfi_endproc

	.align	16, 0x90
	.type	loop_dump_aux,@function
loop_dump_aux:                          # @loop_dump_aux
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp336:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp337:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp338:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp339:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp340:
	.cfi_def_cfa_offset 48
	subq	$112, %rsp
.Ltmp341:
	.cfi_def_cfa_offset 160
.Ltmp342:
	.cfi_offset %rbx, -48
.Ltmp343:
	.cfi_offset %r12, -40
.Ltmp344:
	.cfi_offset %r13, -32
.Ltmp345:
	.cfi_offset %r14, -24
.Ltmp346:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r13
	testq	%r13, %r13
	je	.LBB30_104
# BB#1:                                 # %entry
	testq	%r14, %r14
	je	.LBB30_104
# BB#2:                                 # %if.end
	movq	48(%r13), %rax
	movq	(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB30_5
# BB#3:                                 # %lor.lhs.false3
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB30_5
# BB#4:                                 # %lor.lhs.false9
	movl	40(%rax), %ecx
	movabsq	$4294967200, %rdx       # imm = 0xFFFFFFA0
	cmpq	%rdx, %rcx
	je	.LBB30_6
.LBB30_5:                               # %if.then18
	movl	8(%rax), %edx
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
.LBB30_6:                               # %if.end27
	movq	56(%r13), %rax
	movq	8(%rax), %rax
	movq	24(%rax), %rax
	testq	%rax, %rax
	je	.LBB30_9
# BB#7:                                 # %lor.lhs.false32
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB30_9
# BB#8:                                 # %lor.lhs.false41
	movl	40(%rax), %ecx
	movabsq	$4294967201, %rdx       # imm = 0xFFFFFFA1
	cmpq	%rdx, %rcx
	je	.LBB30_10
.LBB30_9:                               # %if.then51
	movl	8(%rax), %edx
	movabsq	$.L.str2, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
.LBB30_10:                              # %if.end61
	movq	192(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB30_104
# BB#11:                                # %cond.true
	movzbl	(%rdi), %eax
	cmpq	$37, %rax
	jne	.LBB30_16
# BB#12:                                # %cond.true70
	callq	next_nonnote_insn
	testq	%rax, %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	je	.LBB30_18
# BB#13:                                # %cond.true74
	movq	192(%r13), %rdi
	callq	next_nonnote_insn
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	cmpq	$0, 32(%rcx,%rax,8)
	je	.LBB30_18
# BB#14:                                # %cond.true82
	movq	192(%r13), %rdi
	callq	next_nonnote_insn
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	jmp	.LBB30_15
.LBB30_16:                              # %cond.true98
	movslq	8(%rdi), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	testq	%rax, %rax
	je	.LBB30_17
.LBB30_15:                              # %cond.true108
	movl	88(%rax), %ebx
	jmp	.LBB30_18
.LBB30_17:
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB30_18:                              # %cond.end128
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	movq	192(%r13), %rax
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%rax, %rax
	movq	%r12, %rcx
	je	.LBB30_20
# BB#19:                                # %cond.true132
	movl	8(%rax), %ecx
.LBB30_20:                              # %cond.end138
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	movq	176(%r13), %rdi
	testq	%rdi, %rdi
	movq	%r12, %r8
	movq	%r12, %rbx
	movq	%r12, %r9
	movq	%r12, %rax
	je	.LBB30_42
# BB#21:                                # %cond.true141
	movzbl	(%rdi), %eax
	cmpq	$37, %rax
	jne	.LBB30_26
# BB#22:                                # %cond.true146
	callq	next_nonnote_insn
	testq	%rax, %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	je	.LBB30_28
# BB#23:                                # %cond.true150
	movq	176(%r13), %rdi
	callq	next_nonnote_insn
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	cmpq	$0, 32(%rcx,%rax,8)
	je	.LBB30_28
# BB#24:                                # %cond.true161
	movq	176(%r13), %rdi
	callq	next_nonnote_insn
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	jmp	.LBB30_25
.LBB30_26:                              # %cond.true182
	movslq	8(%rdi), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	testq	%rax, %rax
	je	.LBB30_27
.LBB30_25:                              # %cond.true192
	movl	88(%rax), %ebx
	jmp	.LBB30_28
.LBB30_27:
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB30_28:                              # %cond.end212
	movq	176(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB30_29
# BB#30:                                # %cond.true226
	movzbl	(%rdi), %eax
	movslq	8(%rdi), %r9
	cmpq	$37, %rax
	jne	.LBB30_35
# BB#31:                                # %cond.true231
	movq	%rbx, 104(%rsp)         # 8-byte Spill
	movq	%r9, %rbx
	callq	next_nonnote_insn
	testq	%rax, %rax
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	je	.LBB30_32
# BB#33:                                # %cond.true235
	movq	176(%r13), %rdi
	movq	%r8, %r15
	callq	next_nonnote_insn
	movq	%r15, %r8
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	cmpq	$0, 32(%rcx,%rax,8)
	movq	%rbx, %r9
	movq	104(%rsp), %rbx         # 8-byte Reload
	je	.LBB30_39
# BB#34:                                # %cond.true246
	movq	176(%r13), %rdi
	movq	%r9, %r15
	callq	next_nonnote_insn
	movq	%r15, %r9
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	88(%rax), %r8d
	jmp	.LBB30_39
.LBB30_29:
	movq	%r12, %r8
	movq	%r12, %r9
	movq	%r12, %rax
	jmp	.LBB30_42
.LBB30_35:                              # %cond.true267
	movq	%rbx, %r15
	movq	basic_block_for_insn(%rip), %rax
	movq	32(%rax,%r9,8), %rax
	testq	%rax, %rax
	je	.LBB30_36
# BB#37:                                # %cond.true277
	movl	88(%rax), %r8d
	jmp	.LBB30_38
.LBB30_32:
	movq	%rbx, %r9
	movq	104(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB30_39
.LBB30_36:
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
.LBB30_38:                              # %cond.end297
	movq	%r15, %rbx
.LBB30_39:                              # %cond.end297
	movq	176(%r13), %rax
	testq	%rax, %rax
	je	.LBB30_40
# BB#41:                                # %cond.true301
	movl	8(%rax), %eax
	jmp	.LBB30_42
.LBB30_40:
	movq	%r12, %rax
.LBB30_42:                              # %cond.end307
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	168(%r13), %rdi
	testq	%rdi, %rdi
	movq	%r12, %rsi
	movq	%r12, %rdx
	je	.LBB30_54
# BB#43:                                # %cond.true310
	movzbl	(%rdi), %eax
	cmpq	$37, %rax
	jne	.LBB30_48
# BB#44:                                # %cond.true315
	movq	%rbx, 104(%rsp)         # 8-byte Spill
	movq	%r8, %r15
	movq	%r9, %rbx
	callq	next_nonnote_insn
	testq	%rax, %rax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	je	.LBB30_50
# BB#45:                                # %cond.true319
	movq	168(%r13), %rdi
	callq	next_nonnote_insn
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	cmpq	$0, 32(%rcx,%rax,8)
	je	.LBB30_50
# BB#46:                                # %cond.true330
	movq	168(%r13), %rdi
	callq	next_nonnote_insn
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	jmp	.LBB30_47
.LBB30_48:                              # %cond.true351
	movq	%rbx, 104(%rsp)         # 8-byte Spill
	movslq	8(%rdi), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movq	%r8, %r15
	movq	%r9, %rbx
	testq	%rax, %rax
	je	.LBB30_49
.LBB30_47:                              # %cond.end381
	movl	88(%rax), %esi
	jmp	.LBB30_50
.LBB30_49:
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
.LBB30_50:                              # %cond.end381
	movq	168(%r13), %rax
	testq	%rax, %rax
	movq	%rbx, %r9
	movq	%r15, %r8
	je	.LBB30_51
# BB#52:                                # %cond.true385
	movl	8(%rax), %edx
	jmp	.LBB30_53
.LBB30_51:
	movq	%r12, %rdx
.LBB30_53:                              # %cond.end391
	movq	104(%rsp), %rbx         # 8-byte Reload
.LBB30_54:                              # %cond.end391
	movq	%rbx, 104(%rsp)         # 8-byte Spill
	movq	200(%r13), %rdi
	testq	%rdi, %rdi
	movq	%r12, %r15
	movq	%r12, %rax
	je	.LBB30_67
# BB#55:                                # %cond.true395
	movzbl	(%rdi), %eax
	cmpq	$37, %rax
	jne	.LBB30_60
# BB#56:                                # %cond.true400
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	%rsi, %rbx
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	%r9, 64(%rsp)           # 8-byte Spill
	callq	next_nonnote_insn
	testq	%rax, %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	je	.LBB30_57
# BB#58:                                # %cond.true404
	movq	200(%r13), %rdi
	callq	next_nonnote_insn
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	cmpq	$0, 32(%rcx,%rax,8)
	movq	64(%rsp), %r9           # 8-byte Reload
	movq	56(%rsp), %r8           # 8-byte Reload
	je	.LBB30_64
# BB#59:                                # %cond.true415
	movq	200(%r13), %rdi
	movq	%r8, %r15
	callq	next_nonnote_insn
	movq	%r15, %r8
	movq	64(%rsp), %r9           # 8-byte Reload
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	jmp	.LBB30_63
.LBB30_60:                              # %cond.true436
	movslq	8(%rdi), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	testq	%rax, %rax
	je	.LBB30_61
# BB#62:                                # %cond.true446
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	%rsi, %rbx
.LBB30_63:                              # %cond.end466
	movl	88(%rax), %r15d
	jmp	.LBB30_64
.LBB30_57:
	movq	64(%rsp), %r9           # 8-byte Reload
	movq	56(%rsp), %r8           # 8-byte Reload
	jmp	.LBB30_64
.LBB30_61:
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	%rsi, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
.LBB30_64:                              # %cond.end466
	movq	200(%r13), %rax
	testq	%rax, %rax
	movq	%rbx, %rsi
	movq	72(%rsp), %rdx          # 8-byte Reload
	je	.LBB30_65
# BB#66:                                # %cond.true470
	movl	8(%rax), %eax
	jmp	.LBB30_67
.LBB30_65:
	movq	%r12, %rax
.LBB30_67:                              # %cond.end476
	movq	%rax, 40(%rsp)
	movq	%r15, 32(%rsp)
	movq	%rdx, 24(%rsp)
	movq	%rsi, 16(%rsp)
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%rsp)
	movabsq	$.L.str3, %rsi
	xorq	%rax, %rax
	movq	%r8, (%rsp)
	movq	%r14, %rdi
	movq	96(%rsp), %rdx          # 8-byte Reload
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	104(%rsp), %r8          # 8-byte Reload
	callq	fprintf
	movq	208(%r13), %rdi
	testq	%rdi, %rdi
	movq	%r12, %rdx
	movq	%r12, %r15
	je	.LBB30_78
# BB#68:                                # %cond.true480
	movzbl	(%rdi), %eax
	cmpq	$37, %rax
	jne	.LBB30_73
# BB#69:                                # %cond.true485
	callq	next_nonnote_insn
	testq	%rax, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	je	.LBB30_75
# BB#70:                                # %cond.true489
	movq	208(%r13), %rdi
	movq	%rdx, %rbx
	callq	next_nonnote_insn
	movq	%rbx, %rdx
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	cmpq	$0, 32(%rcx,%rax,8)
	je	.LBB30_75
# BB#71:                                # %cond.true500
	movq	208(%r13), %rdi
	callq	next_nonnote_insn
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	jmp	.LBB30_72
.LBB30_73:                              # %cond.true521
	movslq	8(%rdi), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	testq	%rax, %rax
	je	.LBB30_74
.LBB30_72:                              # %cond.true531
	movl	88(%rax), %edx
	jmp	.LBB30_75
.LBB30_74:
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
.LBB30_75:                              # %cond.end551
	movq	208(%r13), %rax
	testq	%rax, %rax
	je	.LBB30_76
# BB#77:                                # %cond.true555
	movl	8(%rax), %r15d
	jmp	.LBB30_78
.LBB30_76:
	movq	%r12, %r15
.LBB30_78:                              # %cond.end561
	movq	216(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB30_79
# BB#80:                                # %cond.true564
	movzbl	(%rdi), %eax
	cmpq	$37, %rax
	jne	.LBB30_86
# BB#81:                                # %cond.true569
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	callq	next_nonnote_insn
	testq	%rax, %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	je	.LBB30_82
# BB#83:                                # %cond.true573
	movq	216(%r13), %rdi
	callq	next_nonnote_insn
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	cmpq	$0, 32(%rcx,%rax,8)
	movq	104(%rsp), %rdx         # 8-byte Reload
	je	.LBB30_88
# BB#84:                                # %cond.true584
	movq	216(%r13), %rdi
	movq	%rdx, %rbx
	callq	next_nonnote_insn
	movq	%rbx, %rdx
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	jmp	.LBB30_85
.LBB30_79:
	movq	%r12, %rbx
	jmp	.LBB30_90
.LBB30_86:                              # %cond.true605
	movslq	8(%rdi), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	testq	%rax, %rax
	je	.LBB30_87
.LBB30_85:                              # %cond.true615
	movl	88(%rax), %ebx
	jmp	.LBB30_88
.LBB30_82:
	movq	104(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB30_88
.LBB30_87:
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB30_88:                              # %cond.end635
	movq	216(%r13), %rax
	testq	%rax, %rax
	je	.LBB30_90
# BB#89:                                # %cond.true639
	movl	8(%rax), %r12d
.LBB30_90:                              # %cond.end645
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rcx
	movq	%rbx, %r8
	movq	%r12, %r9
	callq	fprintf
	movl	240(%r13), %edx
	movabsq	$.L.str5, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movl	240(%r13), %eax
	testq	%rax, %rax
	je	.LBB30_96
# BB#91:                                # %if.then651
	movabsq	$.L.str6, %rdi
	movq	$9, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movq	232(%r13), %rbx
	testq	%rbx, %rbx
	je	.LBB30_96
# BB#92:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movabsq	$.L.str7, %r15
	.align	16, 0x90
.LBB30_93:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	testq	%rax, %rax
	movq	%r12, %rdx
	je	.LBB30_95
# BB#94:                                # %cond.true658
                                        #   in Loop: Header=BB30_93 Depth=1
	movl	8(%rax), %edx
.LBB30_95:                              # %cond.end666
                                        #   in Loop: Header=BB30_93 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	fprintf
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB30_93
.LBB30_96:                              # %if.end672
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	176(%r13), %rdi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rdi, %rdi
	movq	%rbx, %rax
	je	.LBB30_103
# BB#97:                                # %cond.true676
	movzbl	(%rdi), %eax
	cmpq	$37, %rax
	jne	.LBB30_101
# BB#98:                                # %cond.true681
	callq	next_nonnote_insn
	testq	%rax, %rax
	movq	%rbx, %rax
	je	.LBB30_103
# BB#99:                                # %cond.true685
	movq	176(%r13), %rdi
	callq	next_nonnote_insn
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	cmpq	$0, 32(%rcx,%rax,8)
	movq	%rbx, %rax
	je	.LBB30_103
# BB#100:                               # %cond.true696
	movq	176(%r13), %rdi
	callq	next_nonnote_insn
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	88(%rax), %eax
	jmp	.LBB30_103
.LBB30_101:                             # %cond.true717
	movslq	8(%rdi), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rcx
	testq	%rcx, %rcx
	movq	%rbx, %rax
	je	.LBB30_103
# BB#102:                               # %cond.true727
	movl	88(%rcx), %eax
.LBB30_103:                             # %cond.end747
	movq	16(%r13), %rcx
	movl	88(%rcx), %ecx
	andq	%rbx, %rax
	cmpq	%rcx, %rax
	jne	.LBB30_105
.LBB30_104:                             # %if.end754
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB30_105:                             # %if.then751
	movabsq	$.L.str9, %rdi
	movq	$42, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fwrite  # TAILCALL
.Ltmp347:
	.size	loop_dump_aux, .Ltmp347-loop_dump_aux
	.cfi_endproc

	.globl	debug_loops
	.align	16, 0x90
	.type	debug_loops,@function
debug_loops:                            # @debug_loops
	.cfi_startproc
# BB#0:                                 # %entry
	movq	stderr(%rip), %rsi
	movabsq	$loop_dump_aux, %rdx
	movq	$1, %rcx
	jmp	flow_loops_dump  # TAILCALL
.Ltmp348:
	.size	debug_loops, .Ltmp348-debug_loops
	.cfi_endproc

	.align	16, 0x90
	.type	invalidate_loops_containing_label,@function
invalidate_loops_containing_label:      # @invalidate_loops_containing_label
	.cfi_startproc
# BB#0:                                 # %entry
	movslq	8(%rdi), %rax
	movq	uid_loop(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	testq	%rax, %rax
	je	.LBB32_3
# BB#1:
	movq	$1, %rcx
	.align	16, 0x90
.LBB32_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	%ecx, 156(%rax)
	movq	128(%rax), %rax
	testq	%rax, %rax
	jne	.LBB32_2
.LBB32_3:                               # %for.end
	retq
.Ltmp349:
	.size	invalidate_loops_containing_label, .Ltmp349-invalidate_loops_containing_label
	.cfi_endproc

	.align	16, 0x90
	.type	mark_loop_jump,@function
mark_loop_jump:                         # @mark_loop_jump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp355:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp356:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp357:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp358:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp359:
	.cfi_def_cfa_offset 48
.Ltmp360:
	.cfi_offset %rbx, -40
.Ltmp361:
	.cfi_offset %r12, -32
.Ltmp362:
	.cfi_offset %r14, -24
.Ltmp363:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB33_1
	.align	16, 0x90
.LBB33_12:                              # %sw.bb9
                                        #   in Loop: Header=BB33_1 Depth=1
	addq	$16, %r14
	movq	(%r14), %r14
.LBB33_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$134, %rcx
	jg	.LBB33_43
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB33_1 Depth=1
	cmpq	$119, %rcx
	jg	.LBB33_13
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB33_1 Depth=1
	addq	$-39, %rcx
	andq	%r15, %rcx
	cmpq	$39, %rcx
	ja	.LBB33_44
# BB#4:                                 # %tailrecurse
                                        #   in Loop: Header=BB33_1 Depth=1
	jmpq	*.LJTI33_0(,%rcx,8)
.LBB33_11:                              # %sw.bb2
                                        #   in Loop: Header=BB33_1 Depth=1
	movq	8(%r14), %rdi
	movq	%rbx, %rsi
	callq	mark_loop_jump
	jmp	.LBB33_12
	.align	16, 0x90
.LBB33_43:                              # %tailrecurse
                                        #   in Loop: Header=BB33_1 Depth=1
	cmpq	$135, %rcx
	je	.LBB33_12
	jmp	.LBB33_44
	.align	16, 0x90
.LBB33_13:                              # %tailrecurse
                                        #   in Loop: Header=BB33_1 Depth=1
	addq	$-120, %rcx
	andq	%r15, %rcx
	cmpq	$2, %rcx
	jae	.LBB33_44
.LBB33_14:                              # %sw.bb1
                                        #   in Loop: Header=BB33_1 Depth=1
	addq	$8, %r14
	movq	(%r14), %r14
	jmp	.LBB33_1
.LBB33_41:                              # %sw.bb71
                                        #   in Loop: Header=BB33_1 Depth=1
	movq	8(%r14), %rax
	movq	global_rtl(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB33_12
	jmp	.LBB33_50
.LBB33_42:                              # %sw.bb81
                                        #   in Loop: Header=BB33_1 Depth=1
	movq	16(%r14), %rdi
	movq	%rbx, %rsi
	callq	mark_loop_jump
	addq	$24, %r14
	movq	(%r14), %r14
	jmp	.LBB33_1
.LBB33_44:                              # %sw.default
	testq	%rbx, %rbx
	je	.LBB33_50
# BB#45:
	movq	$1, %r15
	movabsq	$.L.str35, %r14
	.align	16, 0x90
.LBB33_46:                              # %for.body123
                                        # =>This Inner Loop Header: Depth=1
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB33_49
# BB#47:                                # %land.lhs.true125
                                        #   in Loop: Header=BB33_46 Depth=1
	movl	156(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB33_49
# BB#48:                                # %if.then128
                                        #   in Loop: Header=BB33_46 Depth=1
	movq	192(%rbx), %rax
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	fprintf
.LBB33_49:                              # %if.end134
                                        #   in Loop: Header=BB33_46 Depth=1
	movl	%r15d, 156(%rbx)
	movq	128(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB33_46
	jmp	.LBB33_50
.LBB33_8:                               # %for.cond89.preheader
	movq	8(%r14), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB33_50
# BB#9:
	movq	$1, %r15
	.align	16, 0x90
.LBB33_10:                              # %for.body93
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%r15,8), %rdi
	movq	%rbx, %rsi
	callq	mark_loop_jump
	movq	8(%r14), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r15
	cmpq	%rdx, %rsi
	jl	.LBB33_10
	jmp	.LBB33_50
.LBB33_5:                               # %for.cond103.preheader
	movq	16(%r14), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB33_50
# BB#6:
	movq	$1, %r15
	.align	16, 0x90
.LBB33_7:                               # %for.body109
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%r15,8), %rdi
	movq	%rbx, %rsi
	callq	mark_loop_jump
	movq	16(%r14), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r15
	cmpq	%rdx, %rsi
	jl	.LBB33_7
	jmp	.LBB33_50
.LBB33_15:                              # %sw.bb17
	movq	8(%r14), %rcx
	movslq	8(%rcx), %rcx
	movq	uid_loop(%rip), %rdx
	movq	(%rdx,%rcx,8), %r15
	xorq	%rcx, %rcx
	testq	%r15, %r15
	je	.LBB33_20
# BB#16:                                # %for.cond.preheader
	cmpq	%rbx, %r15
	movq	%rbx, %rcx
	je	.LBB33_20
# BB#17:
	movq	%r15, %rcx
	.align	16, 0x90
.LBB33_18:                              # %for.inc
                                        # =>This Inner Loop Header: Depth=1
	movq	128(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB33_20
# BB#19:                                # %for.inc
                                        #   in Loop: Header=BB33_18 Depth=1
	cmpq	%rbx, %rcx
	jne	.LBB33_18
.LBB33_20:                              # %if.end26
	testq	%rbx, %rbx
	je	.LBB33_26
# BB#21:                                # %if.end26
	testq	%rcx, %rcx
	jne	.LBB33_26
# BB#22:                                # %if.then29
	orq	$268435456, %rax        # imm = 0x10000000
	movl	%eax, (%r14)
	movq	232(%rbx), %rax
	movq	%rax, 16(%r14)
	movq	%r14, 232(%rbx)
	cmpq	%rbx, %r15
	je	.LBB33_26
# BB#23:
	movq	%rbx, %rax
	.align	16, 0x90
.LBB33_24:                              # %for.body39
                                        # =>This Inner Loop Header: Depth=1
	movl	240(%rax), %ecx
	incq	%rcx
	movl	%ecx, 240(%rax)
	movq	128(%rax), %rax
	testq	%rax, %rax
	je	.LBB33_26
# BB#25:                                # %for.body39
                                        #   in Loop: Header=BB33_24 Depth=1
	cmpq	%r15, %rax
	jne	.LBB33_24
.LBB33_26:                              # %if.end43
	testq	%r15, %r15
	je	.LBB33_50
# BB#27:                                # %for.body49.preheader
	testq	%rbx, %rbx
	je	.LBB33_28
# BB#33:
	movq	$1, %r12
	movabsq	$.L.str34, %r14
.LBB33_34:                              # %for.body52.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB33_35 Depth 2
	movq	%rbx, %rax
	.align	16, 0x90
.LBB33_35:                              # %for.body52
                                        #   Parent Loop BB33_34 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%rax, %r15
	je	.LBB33_50
# BB#36:                                # %for.inc56
                                        #   in Loop: Header=BB33_35 Depth=2
	movq	128(%rax), %rax
	testq	%rax, %rax
	jne	.LBB33_35
# BB#37:                                # %for.end58
                                        #   in Loop: Header=BB33_34 Depth=1
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB33_40
# BB#38:                                # %land.lhs.true60
                                        #   in Loop: Header=BB33_34 Depth=1
	movl	156(%r15), %eax
	testq	%rax, %rax
	jne	.LBB33_40
# BB#39:                                # %if.then62
                                        #   in Loop: Header=BB33_34 Depth=1
	movq	192(%r15), %rax
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	fprintf
.LBB33_40:                              # %if.end66
                                        #   in Loop: Header=BB33_34 Depth=1
	movl	%r12d, 156(%r15)
	movq	128(%r15), %r15
	testq	%r15, %r15
	jne	.LBB33_34
	jmp	.LBB33_50
.LBB33_28:
	movq	$1, %rbx
	movabsq	$.L.str34, %r14
.LBB33_29:                              # %for.end58.us
                                        # =>This Inner Loop Header: Depth=1
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB33_32
# BB#30:                                # %land.lhs.true60.us
                                        #   in Loop: Header=BB33_29 Depth=1
	movl	156(%r15), %eax
	testq	%rax, %rax
	jne	.LBB33_32
# BB#31:                                # %if.then62.us
                                        #   in Loop: Header=BB33_29 Depth=1
	movq	192(%r15), %rax
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	fprintf
.LBB33_32:                              # %if.end66.us
                                        #   in Loop: Header=BB33_29 Depth=1
	movl	%ebx, 156(%r15)
	movq	128(%r15), %r15
	testq	%r15, %r15
	jne	.LBB33_29
.LBB33_50:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp364:
	.size	mark_loop_jump, .Ltmp364-mark_loop_jump
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI33_0:
	.quad	.LBB33_8
	.quad	.LBB33_44
	.quad	.LBB33_44
	.quad	.LBB33_44
	.quad	.LBB33_44
	.quad	.LBB33_8
	.quad	.LBB33_5
	.quad	.LBB33_44
	.quad	.LBB33_41
	.quad	.LBB33_50
	.quad	.LBB33_50
	.quad	.LBB33_44
	.quad	.LBB33_50
	.quad	.LBB33_44
	.quad	.LBB33_44
	.quad	.LBB33_50
	.quad	.LBB33_50
	.quad	.LBB33_44
	.quad	.LBB33_44
	.quad	.LBB33_14
	.quad	.LBB33_50
	.quad	.LBB33_44
	.quad	.LBB33_50
	.quad	.LBB33_44
	.quad	.LBB33_44
	.quad	.LBB33_44
	.quad	.LBB33_44
	.quad	.LBB33_50
	.quad	.LBB33_15
	.quad	.LBB33_44
	.quad	.LBB33_44
	.quad	.LBB33_44
	.quad	.LBB33_44
	.quad	.LBB33_42
	.quad	.LBB33_44
	.quad	.LBB33_44
	.quad	.LBB33_11
	.quad	.LBB33_11
	.quad	.LBB33_44
	.quad	.LBB33_11

	.text
	.align	16, 0x90
	.type	loop_regs_scan,@function
loop_regs_scan:                         # @loop_regs_scan
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp371:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp372:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp373:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp374:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp375:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp376:
	.cfi_def_cfa_offset 64
.Ltmp377:
	.cfi_offset %rbx, -48
.Ltmp378:
	.cfi_offset %r12, -40
.Ltmp379:
	.cfi_offset %r13, -32
.Ltmp380:
	.cfi_offset %r14, -24
.Ltmp381:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	160(%r14), %rbx
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movslq	184(%rbx), %r15
	movq	%rsi, %r12
	callq	max_reg_num
	movq	$32, %rcx
	movq	%rax, %rdx
	movl	%eax, 184(%rbx)
	movslq	188(%rbx), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rsi, %rdx
	jl	.LBB34_2
# BB#1:                                 # %if.then
	addq	%r12, %rax
	movq	8(%rsp), %rbx           # 8-byte Reload
	movl	%eax, 188(%rbx)
	movq	192(%rbx), %rdi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rsi
	callq	xrealloc
	movq	%rax, 192(%rbx)
	leaq	(%r15,%r15,2), %rcx
	movl	188(%rbx), %edx
	leaq	(%rax,%rcx,8), %rdi
	subq	%r15, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$3, %rcx
	shlq	%cl, %rdx
	leaq	(%rdx,%rdx,2), %rdx
	xorq	%rsi, %rsi
	callq	memset
.LBB34_2:                               # %for.cond.preheader
	testq	%r15, %r15
	jle	.LBB34_5
# BB#3:                                 # %for.body.lr.ph
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	192(%rax), %rax
	addq	$16, %rax
	movq	$1, %rcx
	xorq	%rdx, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	%r15, %rdi
	andq	%rsi, %rdi
	.align	16, 0x90
.LBB34_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -16(%rax)
	movb	%dl, (%rax)
	movq	$0, -8(%rax)
	movq	%rcx, %rbx
	andq	%rsi, %rbx
	incq	%rcx
	addq	$24, %rax
	cmpq	%rdi, %rbx
	jne	.LBB34_4
.LBB34_5:                               # %for.end
	movq	%r15, (%rsp)            # 8-byte Spill
	movq	8(%rsp), %rax           # 8-byte Reload
	leaq	184(%rax), %r12
	movslq	(%r12), %rdi
	movq	$8, %rsi
	callq	xcalloc
	movq	208(%r14), %r13
	movq	%rax, %r15
	testq	%r13, %r13
	jne	.LBB34_7
# BB#6:                                 # %cond.false
	movq	192(%r14), %r13
	jmp	.LBB34_7
	.align	16, 0x90
.LBB34_18:                              # %for.inc110
                                        #   in Loop: Header=BB34_7 Depth=1
	movq	24(%r13), %r13
.LBB34_7:                               # %for.cond26.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB34_30 Depth 2
	movq	200(%r14), %rax
	cmpq	%rax, %r13
	je	.LBB34_19
# BB#8:                                 # %for.body29
                                        #   in Loop: Header=BB34_7 Depth=1
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB34_15
# BB#9:                                 # %if.then35
                                        #   in Loop: Header=BB34_7 Depth=1
	movq	32(%r13), %rdx
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	find_single_use_in_loop
	movq	56(%r13), %rdx
	testq	%rdx, %rdx
	je	.LBB34_11
# BB#10:                                # %if.then41
                                        #   in Loop: Header=BB34_7 Depth=1
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	find_single_use_in_loop
.LBB34_11:                              # %if.end45
                                        #   in Loop: Header=BB34_7 Depth=1
	movq	32(%r13), %rdx
	movzbl	(%rdx), %eax
	cmpq	$39, %rax
	je	.LBB34_29
# BB#12:                                # %if.end45
                                        #   in Loop: Header=BB34_7 Depth=1
	cmpq	$47, %rax
	je	.LBB34_14
# BB#13:                                # %if.end45
                                        #   in Loop: Header=BB34_7 Depth=1
	cmpq	$49, %rax
	jne	.LBB34_15
.LBB34_14:                              # %if.then60
                                        #   in Loop: Header=BB34_7 Depth=1
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	%r15, %rcx
	callq	count_one_set
	jmp	.LBB34_15
.LBB34_29:                              # %if.then71
                                        #   in Loop: Header=BB34_7 Depth=1
	movq	8(%rdx), %rax
	movslq	(%rax), %rbx
	testq	%rbx, %rbx
	jle	.LBB34_15
	jmp	.LBB34_30
	.align	16, 0x90
.LBB34_31:                              # %for.body82.for.body82_crit_edge
                                        #   in Loop: Header=BB34_30 Depth=2
	movq	32(%r13), %rax
	movq	8(%rax), %rax
.LBB34_30:                              # %for.body82.lr.ph
                                        #   Parent Loop BB34_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%rbx
	movq	8(%rax,%rbx,8), %rdx
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	%r15, %rcx
	callq	count_one_set
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB34_31
	.align	16, 0x90
.LBB34_15:                              # %if.end95
                                        #   in Loop: Header=BB34_7 Depth=1
	movzbl	(%r13), %eax
	cmpq	$33, %rax
	je	.LBB34_17
# BB#16:                                # %if.end95
                                        #   in Loop: Header=BB34_7 Depth=1
	cmpq	$36, %rax
	jne	.LBB34_18
.LBB34_17:                              # %if.then105
                                        #   in Loop: Header=BB34_7 Depth=1
	movslq	(%r12), %rdx
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	memset
	jmp	.LBB34_18
.LBB34_19:                              # %for.end114
	movq	160(%r14), %rax
	movl	(%rax), %eax
	testq	%rax, %rax
	je	.LBB34_25
# BB#20:                                # %for.cond118.preheader
	xorq	%r14, %r14
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB34_21:                              # %for.body121
                                        # =>This Inner Loop Header: Depth=1
	movq	regs_invalidated_by_call(%rip), %rax
	movq	$1, %rdx
	movq	%rbx, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB34_24
# BB#22:                                # %land.lhs.true
                                        #   in Loop: Header=BB34_21 Depth=1
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_REG
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	rtx_varies_p
	testq	%r13, %rax
	je	.LBB34_24
# BB#23:                                # %if.then129
                                        #   in Loop: Header=BB34_21 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	192(%rax), %rax
	movq	$1, %rcx
	movb	%cl, 16(%rax,%r14)
	movl	%ecx, (%rax,%r14)
.LBB34_24:                              # %for.inc139
                                        #   in Loop: Header=BB34_21 Depth=1
	incq	%rbx
	addq	$24, %r14
	cmpq	$53, %rbx
	jne	.LBB34_21
.LBB34_25:                              # %for.cond143.preheader
	movslq	(%r12), %rax
	movq	(%rsp), %rdx            # 8-byte Reload
	cmpq	%rax, %rdx
	jge	.LBB34_28
# BB#26:                                # %for.body147.lr.ph
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	192(%rcx), %rcx
	leaq	(%rdx,%rdx,2), %rsi
	leaq	1(%rdx), %rdx
	leaq	4(%rcx,%rsi,8), %rsi
	.align	16, 0x90
.LBB34_27:                              # %for.body147
                                        # =>This Inner Loop Header: Depth=1
	movl	-4(%rsi), %ecx
	movl	%ecx, (%rsi)
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rdx
	addq	$24, %rsi
	cmpq	%rax, %rdi
	jl	.LBB34_27
.LBB34_28:                              # %for.end157
	movq	%r15, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.Ltmp382:
	.size	loop_regs_scan, .Ltmp382-loop_regs_scan
	.cfi_endproc

	.align	16, 0x90
	.type	consec_sets_invariant_p,@function
consec_sets_invariant_p:                # @consec_sets_invariant_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp389:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp390:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp391:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp392:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp393:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp394:
	.cfi_def_cfa_offset 96
.Ltmp395:
	.cfi_offset %rbx, -48
.Ltmp396:
	.cfi_offset %r12, -40
.Ltmp397:
	.cfi_offset %r13, -32
.Ltmp398:
	.cfi_offset %r14, -24
.Ltmp399:
	.cfi_offset %r15, -16
	movq	%rcx, %rbx
	movq	%rdx, %r14
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%r14, %rax
	andq	%rcx, %rax
	xorq	%r15, %r15
	cmpq	$127, %rax
	je	.LBB35_31
# BB#1:                                 # %if.end
	movq	160(%rdi), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movl	8(%rsi), %r13d
	movq	%r13, 32(%rsp)          # 8-byte Spill
	movq	192(%rax), %rax
	leaq	(%r13,%r13,2), %rdx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	leaq	(%rax,%rdx,8), %rcx
	movl	(%rax,%rdx,8), %eax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%r15, %r15
	movl	%r15d, (%rcx)
	jmp	.LBB35_2
.LBB35_18:                              # %if.end147.us.thread
                                        #   in Loop: Header=BB35_2 Depth=1
	orq	$1, %r15
	movq	32(%rsp), %r13          # 8-byte Reload
	.align	16, 0x90
.LBB35_2:                               # %while.cond.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB35_3 Depth 2
	decq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB35_30
	.align	16, 0x90
.LBB35_3:                               # %while.body.us
                                        #   Parent Loop BB35_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rbx), %rbx
	movzbl	(%rbx), %r12d
	cmpq	$32, %r12
	jne	.LBB35_28
# BB#4:                                 # %land.lhs.true.us
                                        #   in Loop: Header=BB35_3 Depth=2
	movq	$7, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB35_6
# BB#5:                                 # %if.then11.us
                                        #   in Loop: Header=BB35_3 Depth=2
	movq	8(%rax), %rbx
.LBB35_6:                               # %land.lhs.true17.us
                                        #   in Loop: Header=BB35_3 Depth=2
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB35_29
# BB#7:                                 # %cond.true.us
                                        #   in Loop: Header=BB35_3 Depth=2
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB35_9
# BB#8:                                 # %cond.false.us
                                        #   in Loop: Header=BB35_3 Depth=2
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB35_9:                               # %cond.end40.us
                                        #   in Loop: Header=BB35_3 Depth=2
	testq	%rax, %rax
	je	.LBB35_28
# BB#10:                                # %land.lhs.true43.us
                                        #   in Loop: Header=BB35_3 Depth=2
	movq	8(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB35_28
# BB#11:                                # %land.lhs.true51.us
                                        #   in Loop: Header=BB35_3 Depth=2
	movl	8(%rcx), %ecx
	cmpq	%r13, %rcx
	jne	.LBB35_28
# BB#12:                                # %if.then60.us
                                        #   in Loop: Header=BB35_3 Depth=2
	movq	16(%rax), %rsi
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	loop_invariant_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB35_32
# BB#13:                                # %if.else.us
                                        #   in Loop: Header=BB35_3 Depth=2
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	%rax, %r13
	testq	%r13, %r13
	jne	.LBB35_15
# BB#14:                                #   in Loop: Header=BB35_3 Depth=2
	movq	32(%rsp), %r13          # 8-byte Reload
	jmp	.LBB35_28
.LBB35_15:                              # %if.then70.us
                                        #   in Loop: Header=BB35_3 Depth=2
	movq	8(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$133, %rcx
	jg	.LBB35_19
# BB#16:                                # %if.then70.us
                                        #   in Loop: Header=BB35_3 Depth=2
	addq	$-54, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB35_21
# BB#17:                                # %if.then70.us
                                        #   in Loop: Header=BB35_3 Depth=2
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	je	.LBB35_21
	jmp	.LBB35_18
.LBB35_19:                              # %if.then70.us
                                        #   in Loop: Header=BB35_3 Depth=2
	cmpq	$134, %rcx
	je	.LBB35_18
# BB#20:                                # %if.then70.us
                                        #   in Loop: Header=BB35_3 Depth=2
	cmpq	$140, %rcx
	je	.LBB35_18
.LBB35_21:                              # %lor.rhs.us
                                        #   in Loop: Header=BB35_3 Depth=2
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB35_23
# BB#22:                                #   in Loop: Header=BB35_3 Depth=2
	movq	32(%rsp), %r13          # 8-byte Reload
	jmp	.LBB35_28
.LBB35_23:                              # %if.end147.us
                                        #   in Loop: Header=BB35_3 Depth=2
	movq	8(%r13), %rsi
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	loop_invariant_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movabsq	$0, %rcx
	je	.LBB35_25
# BB#24:                                # %if.end147.us
                                        #   in Loop: Header=BB35_3 Depth=2
	movabsq	$1, %rcx
.LBB35_25:                              # %if.end147.us
                                        #   in Loop: Header=BB35_3 Depth=2
	movq	32(%rsp), %r13          # 8-byte Reload
	jne	.LBB35_27
# BB#26:                                # %if.end147.us
                                        #   in Loop: Header=BB35_3 Depth=2
	xorq	%rcx, %rcx
.LBB35_27:                              # %if.end147.us
                                        #   in Loop: Header=BB35_3 Depth=2
	orq	%rcx, %r15
	testq	%rax, %rax
	jne	.LBB35_2
	.align	16, 0x90
.LBB35_28:                              # %if.else151.us
                                        #   in Loop: Header=BB35_3 Depth=2
	cmpq	$37, %r12
	je	.LBB35_3
	jmp	.LBB35_29
.LBB35_32:                              # %if.end147.thread212
                                        #   in Loop: Header=BB35_2 Depth=1
	orq	%rax, %r15
	jmp	.LBB35_2
.LBB35_29:                              # %if.then154
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	192(%rax), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	8(%rsp), %rdx           # 8-byte Reload
	movl	%edx, (%rax,%rcx,8)
	xorq	%r15, %r15
.LBB35_31:                              # %return
	movq	%r15, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB35_30:                              # %while.end
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	192(%rax), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	8(%rsp), %rdx           # 8-byte Reload
	movl	%edx, (%rax,%rcx,8)
	andq	$2, %r15
	orq	$1, %r15
	jmp	.LBB35_31
.Ltmp400:
	.size	consec_sets_invariant_p, .Ltmp400-consec_sets_invariant_p
	.cfi_endproc

	.align	16, 0x90
	.type	strength_reduce,@function
strength_reduce:                        # @strength_reduce
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp407:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp408:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp409:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp410:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp411:
	.cfi_def_cfa_offset 48
	subq	$5824, %rsp             # imm = 0x16C0
.Ltmp412:
	.cfi_def_cfa_offset 5872
.Ltmp413:
	.cfi_offset %rbx, -48
.Ltmp414:
	.cfi_offset %r12, -40
.Ltmp415:
	.cfi_offset %r13, -32
.Ltmp416:
	.cfi_offset %r14, -24
.Ltmp417:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	160(%rbx), %r14
	movl	(%r14), %eax
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movq	$1, 128(%rsp)           # 8-byte Folded Spill
	jne	.LBB36_2
# BB#1:                                 # %select.mid
	movq	$2, 128(%rsp)           # 8-byte Folded Spill
.LBB36_2:                               # %select.end
	movl	word_mode(%rip), %edi
	movq	$58, %rsi
	callq	gen_rtx_REG
	movq	208(%rbx), %rcx
	movq	%rax, 144(%rsp)         # 8-byte Spill
	testq	%rcx, %rcx
	jne	.LBB36_4
# BB#3:                                 # %cond.false.i
	movq	192(%rbx), %rcx
.LBB36_4:                               # %for.cond.preheader.i
	movq	200(%rbx), %rax
	xorq	%rsi, %rsi
	cmpq	%rax, %rcx
	je	.LBB36_9
# BB#5:
	movabsq	$0, %rdx
	movabsq	$1, %r8
	.align	16, 0x90
.LBB36_6:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rcx), %edi
	movzbl	rtx_class(%rdi), %edi
	cmpq	$105, %rdi
	movq	%rdx, %rdi
	jne	.LBB36_8
# BB#7:                                 # %for.body.i
                                        #   in Loop: Header=BB36_6 Depth=1
	movq	%r8, %rdi
.LBB36_8:                               # %for.body.i
                                        #   in Loop: Header=BB36_6 Depth=1
	movq	24(%rcx), %rcx
	addq	%rdi, %rsi
	cmpq	%rax, %rcx
	jne	.LBB36_6
.LBB36_9:                               # %count_insns_in_loop.exit
	movq	%rsi, 136(%rsp)         # 8-byte Spill
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_reg_rtx
	movl	max_reg_before_loop(%rip), %edi
	movq	%rax, addr_placeholder(%rip)
	movq	%r14, 104(%rsp)         # 8-byte Spill
	movl	%edi, 216(%r14)
	movq	$16, %rsi
	callq	xcalloc
	movq	%rax, 208(%r14)
	movq	%rbx, %rdi
	movq	%rdi, 176(%rsp)         # 8-byte Spill
	movq	160(%rdi), %r12
	movq	$0, 224(%r12)
	movabsq	$check_insn_for_bivs, %rsi
	callq	for_each_insn_in_loop
	movq	224(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB36_23
# BB#10:                                # %for.body.lr.ph.i
	leaq	224(%r12), %r13
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	$2, %r15
	jmp	.LBB36_11
.LBB36_20:                              # %if.else.i
                                        #   in Loop: Header=BB36_11 Depth=1
	movq	loop_dump_stream(%rip), %rdi
	leaq	64(%rbx), %r13
	testq	%rdi, %rdi
	je	.LBB36_22
# BB#21:                                # %if.then34.i
                                        #   in Loop: Header=BB36_11 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str84, %rsi
	callq	fprintf
	jmp	.LBB36_22
	.align	16, 0x90
.LBB36_11:                              # %for.body.i402
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rbx), %edx
	movq	208(%r12), %rax
	movq	$4, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movl	(%rax,%rsi), %ecx
	cmpq	$1, %rcx
	jne	.LBB36_14
# BB#12:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB36_11 Depth=1
	movq	192(%r12), %rsi
	leaq	(%rdx,%rdx,2), %rdi
	movl	4(%rsi,%rdi,8), %esi
	movl	4(%rbx), %edi
	cmpq	%rdi, %rsi
	jne	.LBB36_14
# BB#13:                                # %lor.lhs.false10.i
                                        #   in Loop: Header=BB36_11 Depth=1
	movzbl	88(%rbx), %esi
	testq	$1, %rsi
	jne	.LBB36_20
	.align	16, 0x90
.LBB36_14:                              # %if.then.i
                                        #   in Loop: Header=BB36_11 Depth=1
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB36_19
# BB#15:                                # %if.then13.i
                                        #   in Loop: Header=BB36_11 Depth=1
	cmpq	$1, %rcx
	movabsq	$.L.str81, %rcx
	jne	.LBB36_18
# BB#16:                                # %cond.false.i405
                                        #   in Loop: Header=BB36_11 Depth=1
	movzbl	88(%rbx), %eax
	testq	$1, %rax
	movabsq	$.L.str82, %rcx
	je	.LBB36_18
# BB#17:                                # %cond.false.i405
                                        #   in Loop: Header=BB36_11 Depth=1
	movabsq	$.L.str83, %rcx
.LBB36_18:                              # %cond.end.i
                                        #   in Loop: Header=BB36_11 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str80, %rsi
	callq	fprintf
	movl	(%rbx), %edx
	movq	208(%r12), %rax
.LBB36_19:                              # %if.end.i
                                        #   in Loop: Header=BB36_11 Depth=1
	andq	%r14, %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	%r15d, (%rax,%rdx)
	movq	64(%rbx), %rax
	movq	%rax, (%r13)
.LBB36_22:                              # %for.inc.i
                                        #   in Loop: Header=BB36_11 Depth=1
	movq	64(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB36_11
.LBB36_23:                              # %loop_bivs_find.exit
	movq	104(%rsp), %rax         # 8-byte Reload
	cmpq	$0, 224(%rax)
	je	.LBB36_24
# BB#32:                                # %if.end16
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	192(%rax), %rbx
	movq	160(%rax), %r14
	movq	%rax, %r13
	testq	%rbx, %rbx
	je	.LBB36_59
# BB#33:                                # %land.rhs.lr.ph.i
	addq	$208, %r14
	.align	16, 0x90
.LBB36_34:                              # %land.rhs.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$36, %rax
	je	.LBB36_58
# BB#35:                                # %for.body.i440
                                        #   in Loop: Header=BB36_34 Depth=1
	movq	%rbx, note_insn(%rip)
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB36_37
# BB#36:                                # %if.then9.i
                                        #   in Loop: Header=BB36_34 Depth=1
	movq	32(%rbx), %rdi
	movabsq	$record_initial, %rsi
	movq	%r14, %rdx
	callq	note_stores
	movl	(%rbx), %eax
.LBB36_37:                              # %if.end11.i
                                        #   in Loop: Header=BB36_34 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$33, %rax
	jne	.LBB36_57
# BB#38:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB36_34 Depth=1
	movq	64(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB36_57
# BB#39:                                # %land.lhs.true21.i
                                        #   in Loop: Header=BB36_34 Depth=1
	callq	next_real_insn
	movq	200(%r13), %rdi
	movq	%rax, %r15
	callq	next_real_insn
	cmpq	%rax, %r15
	jne	.LBB36_57
# BB#40:                                # %land.lhs.true28.i
                                        #   in Loop: Header=BB36_34 Depth=1
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	get_condition_for_loop
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB36_57
# BB#41:                                # %land.lhs.true32.i
                                        #   in Loop: Header=BB36_34 Depth=1
	movq	8(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB36_57
# BB#42:                                # %land.lhs.true40.i
                                        #   in Loop: Header=BB36_34 Depth=1
	movl	8(%rax), %edx
	movl	max_reg_before_loop(%rip), %eax
	cmpq	%rax, %rdx
	jae	.LBB36_57
# BB#43:                                # %land.lhs.true48.i
                                        #   in Loop: Header=BB36_34 Depth=1
	movq	(%r14), %rsi
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	8(%rsi,%rdx), %r15
	testq	%r15, %r15
	je	.LBB36_57
# BB#44:                                # %land.lhs.true59.i
                                        #   in Loop: Header=BB36_34 Depth=1
	movq	16(%r12), %rdi
	movzbl	(%rdi), %ecx
	cmpq	$133, %rcx
	jg	.LBB36_51
# BB#45:                                # %land.lhs.true59.i
                                        #   in Loop: Header=BB36_34 Depth=1
	addq	$-54, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$14, %rcx
	ja	.LBB36_57
# BB#46:                                # %land.lhs.true59.i
                                        #   in Loop: Header=BB36_34 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB36_53
# BB#47:                                # %land.lhs.true59.i
                                        #   in Loop: Header=BB36_34 Depth=1
	cmpq	$7, %rcx
	jne	.LBB36_57
# BB#48:                                # %lor.lhs.false31.i.i
                                        #   in Loop: Header=BB36_34 Depth=1
	movl	8(%rdi), %ecx
	cmpq	%rax, %rcx
	jae	.LBB36_57
# BB#49:                                # %lor.lhs.false31.i.i
                                        #   in Loop: Header=BB36_34 Depth=1
	cmpq	$53, %rcx
	jb	.LBB36_57
# BB#50:                                # %valid_initial_value_p.exit.i
                                        #   in Loop: Header=BB36_34 Depth=1
	movq	192(%r13), %rdx
	movq	%rbx, %rsi
	callq	reg_set_between_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB36_53
	jmp	.LBB36_57
.LBB36_51:                              # %land.lhs.true59.i
                                        #   in Loop: Header=BB36_34 Depth=1
	cmpq	$134, %rcx
	je	.LBB36_53
# BB#52:                                # %land.lhs.true59.i
                                        #   in Loop: Header=BB36_34 Depth=1
	cmpq	$140, %rcx
	jne	.LBB36_57
.LBB36_53:                              # %land.lhs.true66.i
                                        #   in Loop: Header=BB36_34 Depth=1
	cmpq	$0, 72(%r15)
	jne	.LBB36_57
# BB#54:                                # %if.then69.i
                                        #   in Loop: Header=BB36_34 Depth=1
	movzbl	(%r12), %eax
	cmpq	$102, %rax
	jne	.LBB36_56
# BB#55:                                # %if.then74.i
                                        #   in Loop: Header=BB36_34 Depth=1
	movq	%rbx, 72(%r15)
	movq	8(%r12), %rdx
	movq	16(%r12), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, 80(%r15)
	jmp	.LBB36_57
.LBB36_56:                              # %if.else.i443
                                        #   in Loop: Header=BB36_34 Depth=1
	movq	%r12, 48(%r15)
	.align	16, 0x90
.LBB36_57:                              # %for.inc.i445
                                        #   in Loop: Header=BB36_34 Depth=1
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB36_34
.LBB36_58:                              # %loop_bivs_init_find.exit.loopexit
	movq	160(%r13), %r14
.LBB36_59:                              # %loop_bivs_init_find.exit
	movq	224(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB36_94
# BB#60:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movabsq	$.L.str78, %r12
	.align	16, 0x90
.LBB36_61:                              # %for.body.i452
                                        # =>This Inner Loop Header: Depth=1
	movq	72(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB36_93
# BB#62:                                # %if.end.i455
                                        #   in Loop: Header=BB36_61 Depth=1
	movq	$4, %rsi
	xorq	%rdx, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB36_68
# BB#63:                                # %land.lhs.true.i460
                                        #   in Loop: Header=BB36_61 Depth=1
	movq	8(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$133, %rcx
	jg	.LBB36_66
# BB#64:                                # %land.lhs.true.i460
                                        #   in Loop: Header=BB36_61 Depth=1
	addq	$-54, %rcx
	andq	%r14, %rcx
	cmpq	$14, %rcx
	ja	.LBB36_68
# BB#65:                                # %land.lhs.true.i460
                                        #   in Loop: Header=BB36_61 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB36_75
	jmp	.LBB36_68
.LBB36_66:                              # %land.lhs.true.i460
                                        #   in Loop: Header=BB36_61 Depth=1
	cmpq	$134, %rcx
	je	.LBB36_75
# BB#67:                                # %land.lhs.true.i460
                                        #   in Loop: Header=BB36_61 Depth=1
	cmpq	$140, %rcx
	je	.LBB36_75
	.align	16, 0x90
.LBB36_68:                              # %lor.lhs.false53.i
                                        #   in Loop: Header=BB36_61 Depth=1
	movq	72(%rbx), %rdi
	movq	$3, %rsi
	xorq	%rdx, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB36_72
# BB#69:                                # %land.lhs.true57.i
                                        #   in Loop: Header=BB36_61 Depth=1
	movq	8(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$133, %rcx
	jg	.LBB36_73
# BB#70:                                # %land.lhs.true57.i
                                        #   in Loop: Header=BB36_61 Depth=1
	addq	$-54, %rcx
	andq	%r14, %rcx
	cmpq	$14, %rcx
	ja	.LBB36_72
# BB#71:                                # %land.lhs.true57.i
                                        #   in Loop: Header=BB36_61 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB36_75
	jmp	.LBB36_72
.LBB36_73:                              # %land.lhs.true57.i
                                        #   in Loop: Header=BB36_61 Depth=1
	cmpq	$134, %rcx
	je	.LBB36_75
# BB#74:                                # %land.lhs.true57.i
                                        #   in Loop: Header=BB36_61 Depth=1
	cmpq	$140, %rcx
	jne	.LBB36_72
	.align	16, 0x90
.LBB36_75:                              # %if.then113.i
                                        #   in Loop: Header=BB36_61 Depth=1
	addq	$8, %rax
	jmp	.LBB36_76
	.align	16, 0x90
.LBB36_72:                              # %if.else.i462
                                        #   in Loop: Header=BB36_61 Depth=1
	movq	80(%rbx), %rax
	addq	$16, %rax
.LBB36_76:                              # %if.end120.i
                                        #   in Loop: Header=BB36_61 Depth=1
	movq	loop_dump_stream(%rip), %rdi
	movq	(%rax), %r15
	testq	%rdi, %rdi
	je	.LBB36_78
# BB#77:                                # %if.then122.i
                                        #   in Loop: Header=BB36_61 Depth=1
	movq	72(%rbx), %rax
	movl	(%rbx), %edx
	movl	8(%rax), %ecx
	xorq	%rax, %rax
	movq	%r12, %rsi
	callq	fprintf
.LBB36_78:                              # %if.end127.i
                                        #   in Loop: Header=BB36_61 Depth=1
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	(%rbx), %ecx
	movq	80(%rax), %rax
	movq	(%rax,%rcx,8), %rcx
	movl	(%r15), %eax
	movzbl	2(%rcx), %esi
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	cmpq	%rsi, %rdx
	je	.LBB36_80
# BB#79:                                # %if.end127.i
                                        #   in Loop: Header=BB36_61 Depth=1
	testq	%rdx, %rdx
	jne	.LBB36_87
.LBB36_80:                              # %land.lhs.true141.i
                                        #   in Loop: Header=BB36_61 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$133, %rax
	jg	.LBB36_89
# BB#81:                                # %land.lhs.true141.i
                                        #   in Loop: Header=BB36_61 Depth=1
	addq	$-54, %rax
	andq	%r14, %rax
	cmpq	$14, %rax
	ja	.LBB36_87
# BB#82:                                # %land.lhs.true141.i
                                        #   in Loop: Header=BB36_61 Depth=1
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB36_91
# BB#83:                                # %land.lhs.true141.i
                                        #   in Loop: Header=BB36_61 Depth=1
	cmpq	$7, %rax
	jne	.LBB36_87
# BB#84:                                # %lor.lhs.false31.i.i471
                                        #   in Loop: Header=BB36_61 Depth=1
	movl	8(%r15), %eax
	movl	max_reg_before_loop(%rip), %ecx
	cmpq	%rcx, %rax
	jae	.LBB36_87
# BB#85:                                # %lor.lhs.false31.i.i471
                                        #   in Loop: Header=BB36_61 Depth=1
	cmpq	$53, %rax
	jb	.LBB36_87
# BB#86:                                # %valid_initial_value_p.exit.i474
                                        #   in Loop: Header=BB36_61 Depth=1
	movq	72(%rbx), %rsi
	movq	192(%r13), %rdx
	movq	%r15, %rdi
	callq	reg_set_between_p
	testq	%r14, %rax
	je	.LBB36_91
	jmp	.LBB36_87
.LBB36_89:                              # %land.lhs.true141.i
                                        #   in Loop: Header=BB36_61 Depth=1
	cmpq	$134, %rax
	je	.LBB36_91
# BB#90:                                # %land.lhs.true141.i
                                        #   in Loop: Header=BB36_61 Depth=1
	cmpq	$140, %rax
	jne	.LBB36_87
.LBB36_91:                              # %if.then146.i
                                        #   in Loop: Header=BB36_61 Depth=1
	movq	%r15, 40(%rbx)
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB36_93
# BB#92:                                # %if.then148.i
                                        #   in Loop: Header=BB36_61 Depth=1
	movq	%r15, %rsi
	callq	print_simple_rtl
	movq	loop_dump_stream(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	jmp	.LBB36_93
	.align	16, 0x90
.LBB36_87:                              # %if.else151.i
                                        #   in Loop: Header=BB36_61 Depth=1
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB36_93
# BB#88:                                # %if.then153.i
                                        #   in Loop: Header=BB36_61 Depth=1
	movq	$11, %rsi
	movq	$1, %rdx
	movabsq	$.L.str79, %rdi
	callq	fwrite
	.align	16, 0x90
.LBB36_93:                              # %for.inc.i477
                                        #   in Loop: Header=BB36_61 Depth=1
	movq	64(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB36_61
.LBB36_94:                              # %loop_bivs_check.exit
	movabsq	$check_insn_for_givs, %rsi
	movq	%r13, %rbx
	movq	%rbx, 176(%rsp)         # 8-byte Spill
	movq	%rbx, %rdi
	callq	for_each_insn_in_loop
	movq	%rbx, %rdi
	callq	loop_iterations
	testq	$4, 80(%rsp)            # 8-byte Folded Reload
	je	.LBB36_203
# BB#95:                                # %if.then20
	movl	x86_prefetch_sse(%rip), %ecx
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	160(%rax), %rax
	testq	%rcx, %rcx
	jne	.LBB36_97
# BB#96:                                # %lor.lhs.false.i482
	movzbl	target_flags+2(%rip), %ecx
	testq	$16, %rcx
	je	.LBB36_203
.LBB36_97:                              # %if.end.i484
	movl	(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB36_101
# BB#98:                                # %if.then4.i
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB36_203
# BB#99:                                # %if.then6.i
	movabsq	$.L.str62, %rdi
	movq	$36, %rsi
	jmp	.LBB36_100
.LBB36_24:                              # %if.then
	testq	$1, 80(%rsp)            # 8-byte Folded Reload
	movq	176(%rsp), %rdi         # 8-byte Reload
	je	.LBB36_26
# BB#25:                                # %if.then15
	xorq	%rdx, %rdx
	movq	%rdi, %rbx
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	unroll_loop
	movq	%rbx, %rdi
.LBB36_26:                              # %if.end
	movq	160(%rdi), %rax
	movq	208(%rax), %rdi
	movq	224(%rax), %r15
	callq	free
	testq	%r15, %r15
	je	.LBB36_554
	.align	16, 0x90
.LBB36_27:                              # %while.body.i416
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB36_28 Depth 2
                                        #     Child Loop BB36_30 Depth 2
	movq	8(%r15), %rdi
	movq	64(%r15), %r14
	testq	%rdi, %rdi
	je	.LBB36_29
	.align	16, 0x90
.LBB36_28:                              # %for.body.i420
                                        #   Parent Loop BB36_27 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	128(%rdi), %rbx
	callq	free
	testq	%rbx, %rbx
	movq	%rbx, %rdi
	jne	.LBB36_28
.LBB36_29:                              # %for.end.i423
                                        #   in Loop: Header=BB36_27 Depth=1
	movq	24(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB36_31
	.align	16, 0x90
.LBB36_30:                              # %for.body6.i427
                                        #   Parent Loop BB36_27 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	128(%rdi), %rbx
	callq	free
	testq	%rbx, %rbx
	movq	%rbx, %rdi
	jne	.LBB36_30
.LBB36_31:                              # %for.end9.i429
                                        #   in Loop: Header=BB36_27 Depth=1
	movq	%r15, %rdi
	callq	free
	testq	%r14, %r14
	movq	%r14, %r15
	jne	.LBB36_27
	jmp	.LBB36_554
.LBB36_101:                             # %if.end8.i
	movq	96(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB36_105
# BB#102:                               # %if.end8.i
	cmpq	$32, %rcx
	ja	.LBB36_105
# BB#103:                               # %if.then14.i
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB36_203
# BB#104:                               # %if.then16.i
	movabsq	$.L.str63, %rdi
	movq	$50, %rsi
.LBB36_100:                             # %if.end21
	movq	$1, %rdx
	callq	fwrite
.LBB36_203:                             # %if.end21
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	160(%rdi), %r15
	movq	224(%r15), %rax
	testq	%rax, %rax
	je	.LBB36_308
# BB#204:
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB36_205:                             # %for.body.i513
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB36_206 Depth 2
                                        #       Child Loop BB36_213 Depth 3
                                        #       Child Loop BB36_218 Depth 3
                                        #         Child Loop BB36_219 Depth 4
                                        #           Child Loop BB36_220 Depth 5
                                        #       Child Loop BB36_236 Depth 3
                                        #         Child Loop BB36_243 Depth 4
                                        #         Child Loop BB36_258 Depth 4
                                        #         Child Loop BB36_271 Depth 4
                                        #         Child Loop BB36_285 Depth 4
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	24(%rax), %r12
	jmp	.LBB36_206
	.align	16, 0x90
.LBB36_305:                             # %for.inc.i522
                                        #   in Loop: Header=BB36_206 Depth=2
	movq	128(%r12), %r12
.LBB36_206:                             # %for.body.i513
                                        #   Parent Loop BB36_205 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB36_213 Depth 3
                                        #       Child Loop BB36_218 Depth 3
                                        #         Child Loop BB36_219 Depth 4
                                        #           Child Loop BB36_220 Depth 5
                                        #       Child Loop BB36_236 Depth 3
                                        #         Child Loop BB36_243 Depth 4
                                        #         Child Loop BB36_258 Depth 4
                                        #         Child Loop BB36_271 Depth 4
                                        #         Child Loop BB36_285 Depth 4
	testq	%r12, %r12
	je	.LBB36_306
# BB#207:                               # %for.body4.i
                                        #   in Loop: Header=BB36_206 Depth=2
	movzbl	100(%r12), %eax
	testq	$3, %rax
	jne	.LBB36_305
# BB#208:                               # %if.then.i517
                                        #   in Loop: Header=BB36_206 Depth=2
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	%r12, %rsi
	movq	%r9, %rbx
	callq	final_giv_value
	movq	%rbx, %r9
	testq	%rax, %rax
	je	.LBB36_302
# BB#209:                               # %land.lhs.true.i.i
                                        #   in Loop: Header=BB36_206 Depth=2
	movzbl	100(%r12), %ecx
	testq	$16, %rcx
	jne	.LBB36_210
# BB#211:                               # %lor.lhs.false.i.i
                                        #   in Loop: Header=BB36_206 Depth=2
	movq	(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB36_302
# BB#212:                               # %land.lhs.true.lr.ph.i.i.i
                                        #   in Loop: Header=BB36_206 Depth=2
	movq	32(%r12), %r10
	movq	reg_n_info(%rip), %rdx
	leaq	32(%r12), %rsi
	movq	%rsi, 160(%rsp)         # 8-byte Spill
	movq	%rbx, %rsi
.LBB36_213:                             # %land.lhs.true.i.i.i
                                        #   Parent Loop BB36_205 Depth=1
                                        #     Parent Loop BB36_206 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rsi), %edi
	cmpq	$33, %rdi
	je	.LBB36_302
# BB#214:                               # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB36_213 Depth=3
	cmpq	$36, %rdi
	je	.LBB36_302
# BB#215:                               # %for.body.i.i.i
                                        #   in Loop: Header=BB36_213 Depth=3
	movl	8(%r10), %edi
	movq	32(%rdx,%rdi,8), %rdi
	movl	4(%rdi), %r8d
	movl	8(%rsi), %edi
	cmpq	%rdi, %r8
	je	.LBB36_216
# BB#770:                               # %for.inc.i.i.i
                                        #   in Loop: Header=BB36_213 Depth=3
	movq	24(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB36_213
	jmp	.LBB36_302
.LBB36_210:                             # %land.lhs.true.if.then_crit_edge.i.i
                                        #   in Loop: Header=BB36_206 Depth=2
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	(%r12), %rbx
	leaq	32(%r12), %rax
	movq	%rax, 160(%rsp)         # 8-byte Spill
.LBB36_217:                             # %if.then.i.i
                                        #   in Loop: Header=BB36_206 Depth=2
	orq	$1, %rcx
	movw	%cx, 100(%r12)
	xorq	%rdi, %rdi
	xorq	%r14, %r14
	movq	176(%rsp), %rsi         # 8-byte Reload
.LBB36_218:                             # %while.body.outer.outer.i.i
                                        #   Parent Loop BB36_205 Depth=1
                                        #     Parent Loop BB36_206 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB36_219 Depth 4
                                        #           Child Loop BB36_220 Depth 5
	movq	%rbx, 168(%rsp)         # 8-byte Spill
.LBB36_219:                             # %while.body.outer.i.i
                                        #   Parent Loop BB36_205 Depth=1
                                        #     Parent Loop BB36_206 Depth=2
                                        #       Parent Loop BB36_218 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB36_220 Depth 5
	movq	200(%rsi), %rax
	movq	(%r12), %rcx
	.align	16, 0x90
.LBB36_220:                             # %while.body.i.i
                                        #   Parent Loop BB36_205 Depth=1
                                        #     Parent Loop BB36_206 Depth=2
                                        #       Parent Loop BB36_218 Depth=3
                                        #         Parent Loop BB36_219 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	24(%rbx), %rbx
	cmpq	%rax, %rbx
	jne	.LBB36_222
# BB#221:                               # %if.then12.i.i
                                        #   in Loop: Header=BB36_220 Depth=5
	movq	192(%rsi), %rdx
	movq	24(%rdx), %rbx
	movq	$1, %r14
.LBB36_222:                             # %if.end.i.i
                                        #   in Loop: Header=BB36_220 Depth=5
	cmpq	%rcx, %rbx
	je	.LBB36_232
# BB#223:                               # %if.end19.i.i
                                        #   in Loop: Header=BB36_220 Depth=5
	movzbl	(%rbx), %edx
	addq	$-32, %rdx
	andq	%r9, %rdx
	cmpq	$2, %rdx
	ja	.LBB36_220
# BB#224:                               # %if.then31.i.i
                                        #   in Loop: Header=BB36_219 Depth=4
	testq	%r9, %rdi
	movq	%r9, %r15
	jne	.LBB36_225
# BB#226:                               # %land.lhs.true33.i.i
                                        #   in Loop: Header=BB36_219 Depth=4
	movq	16(%r12), %rdi
	movq	32(%rbx), %rsi
	callq	reg_set_p
	testq	%r15, %rax
	movabsq	$0, %rax
	je	.LBB36_228
# BB#227:                               # %land.lhs.true33.i.i
                                        #   in Loop: Header=BB36_219 Depth=4
	movabsq	$1, %rax
.LBB36_228:                             # %land.lhs.true33.i.i
                                        #   in Loop: Header=BB36_219 Depth=4
	movq	%rax, %r13
	jmp	.LBB36_229
	.align	16, 0x90
.LBB36_225:                             # %if.then31.if.end41_crit_edge.i.i
                                        #   in Loop: Header=BB36_219 Depth=4
	movq	%rdi, %r13
.LBB36_229:                             # %if.end41.i.i
                                        #   in Loop: Header=BB36_219 Depth=4
	leaq	32(%rbx), %rax
	movq	160(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rdi
	movq	(%rax), %rsi
	callq	reg_mentioned_p
	movq	%r15, %r9
	testq	%r9, %rax
	movq	176(%rsp), %rsi         # 8-byte Reload
	movq	%r13, %rdi
	je	.LBB36_219
# BB#230:                               # %if.then48.i.i
                                        #   in Loop: Header=BB36_218 Depth=3
	movq	%rdi, %rax
	orq	%r14, %rax
	testq	%r9, %rax
	je	.LBB36_218
# BB#231:                               # %if.then52.i.i
                                        #   in Loop: Header=BB36_206 Depth=2
	movzbl	100(%r12), %r15d
	andq	$-4, %r15
	orq	$2, %r15
	movw	%r15w, 100(%r12)
	jmp	.LBB36_233
.LBB36_232:                             # %while.end.loopexit.i.i
                                        #   in Loop: Header=BB36_206 Depth=2
	movzbl	100(%r12), %r15d
.LBB36_233:                             # %while.end.i.i
                                        #   in Loop: Header=BB36_206 Depth=2
	testq	$1, %r15
	jne	.LBB36_235
# BB#234:                               #   in Loop: Header=BB36_206 Depth=2
	movq	152(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB36_300
.LBB36_235:                             # %if.then67.i.i
                                        #   in Loop: Header=BB36_206 Depth=2
	movq	(%r12), %r8
	movq	200(%rsi), %rax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movl	max_uid_for_loop(%rip), %r10d
	movq	uid_luid(%rip), %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	%r8, %r11
	.align	16, 0x90
.LBB36_236:                             # %while.body69.i.i
                                        #   Parent Loop BB36_205 Depth=1
                                        #     Parent Loop BB36_206 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB36_243 Depth 4
                                        #         Child Loop BB36_258 Depth 4
                                        #         Child Loop BB36_271 Depth 4
                                        #         Child Loop BB36_285 Depth 4
	movq	24(%r11), %r11
	movq	160(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r11
	jne	.LBB36_238
# BB#237:                               # %if.then75.i.i
                                        #   in Loop: Header=BB36_236 Depth=3
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	192(%rax), %rax
	movq	24(%rax), %r11
.LBB36_238:                             # %if.end80.i.i
                                        #   in Loop: Header=BB36_236 Depth=3
	movq	168(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r11
	je	.LBB36_239
# BB#240:                               # %if.end83.i.i
                                        #   in Loop: Header=BB36_236 Depth=3
	movzbl	(%r11), %eax
	cmpq	$33, %rax
	jne	.LBB36_236
# BB#241:                               # %land.lhs.true87.i.i
                                        #   in Loop: Header=BB36_236 Depth=3
	movq	64(%r11), %r9
	testq	%r9, %r9
	je	.LBB36_236
# BB#242:                               # %land.lhs.true92.i.i
                                        #   in Loop: Header=BB36_236 Depth=3
	cmpq	$0, 56(%r9)
	movq	%r9, %rsi
	movq	%r8, %r13
	je	.LBB36_236
	jmp	.LBB36_243
.LBB36_255:                             # %if.then56.i368.i.i
                                        #   in Loop: Header=BB36_243 Depth=4
	movq	24(%r13), %r13
	.align	16, 0x90
.LBB36_243:                             # %for.cond.i334.i.i
                                        #   Parent Loop BB36_205 Depth=1
                                        #     Parent Loop BB36_206 Depth=2
                                        #       Parent Loop BB36_236 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	cmpq	%r9, %r13
	je	.LBB36_244
# BB#245:                               # %for.cond.i334.i.i
                                        #   in Loop: Header=BB36_243 Depth=4
	testq	%rsi, %rsi
	je	.LBB36_244
# BB#246:                               # %if.end.i337.i.i
                                        #   in Loop: Header=BB36_243 Depth=4
	cmpq	%r8, %rsi
	je	.LBB36_257
# BB#247:                               # %if.end.i337.i.i
                                        #   in Loop: Header=BB36_243 Depth=4
	testq	%r13, %r13
	je	.LBB36_257
# BB#248:                               # %if.end5.i341.i.i
                                        #   in Loop: Header=BB36_243 Depth=4
	movslq	8(%rsi), %rdx
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jge	.LBB36_249
# BB#250:                               # %land.lhs.true.i343.i.i
                                        #   in Loop: Header=BB36_243 Depth=4
	movslq	8(%r13), %rax
	movq	$32, %rcx
	movq	%r10, %rbx
	shlq	%cl, %rbx
	movzbl	(%rsi), %edi
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rax
	jge	.LBB36_252
# BB#251:                               # %land.lhs.true.i343.i.i
                                        #   in Loop: Header=BB36_243 Depth=4
	cmpq	$37, %rdi
	jne	.LBB36_256
.LBB36_252:                             # %lor.lhs.false42.i359.i.i
                                        #   in Loop: Header=BB36_243 Depth=4
	cmpq	$37, %rdi
	jne	.LBB36_254
	jmp	.LBB36_253
	.align	16, 0x90
.LBB36_249:                             # %if.end5.i341.if.then47.i362_crit_edge.i.i
                                        #   in Loop: Header=BB36_243 Depth=4
	movl	8(%r13), %eax
.LBB36_253:                             # %if.then47.i362.i.i
                                        #   in Loop: Header=BB36_243 Depth=4
	movq	24(%rsi), %rsi
.LBB36_254:                             # %if.end50.i365.i.i
                                        #   in Loop: Header=BB36_243 Depth=4
	movq	$32, %rcx
	movq	%r10, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB36_243
	jmp	.LBB36_255
.LBB36_256:                             # %loop_insn_first_p.exit370.i.i
                                        #   in Loop: Header=BB36_236 Depth=3
	movq	112(%rsp), %rsi         # 8-byte Reload
	movslq	(%rsi,%rdx,4), %rcx
	movslq	(%rsi,%rax,4), %rax
	cmpq	%rax, %rcx
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%r9, %r14
	jg	.LBB36_271
.LBB36_257:                             # %land.lhs.true106.i.i
                                        #   in Loop: Header=BB36_236 Depth=3
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	192(%rax), %r13
	movq	%r13, %rbx
	movq	%r9, %rax
	jmp	.LBB36_258
.LBB36_269:                             # %if.then56.i319.i.i
                                        #   in Loop: Header=BB36_258 Depth=4
	movq	24(%rax), %rax
	.align	16, 0x90
.LBB36_258:                             # %for.cond.i285.i.i
                                        #   Parent Loop BB36_205 Depth=1
                                        #     Parent Loop BB36_206 Depth=2
                                        #       Parent Loop BB36_236 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	cmpq	%r13, %rax
	je	.LBB36_244
# BB#259:                               # %for.cond.i285.i.i
                                        #   in Loop: Header=BB36_258 Depth=4
	testq	%rbx, %rbx
	je	.LBB36_244
# BB#260:                               # %if.end.i288.i.i
                                        #   in Loop: Header=BB36_258 Depth=4
	cmpq	%r9, %rbx
	je	.LBB36_298
# BB#261:                               # %if.end.i288.i.i
                                        #   in Loop: Header=BB36_258 Depth=4
	testq	%rax, %rax
	je	.LBB36_298
# BB#262:                               # %if.end5.i292.i.i
                                        #   in Loop: Header=BB36_258 Depth=4
	movslq	8(%rbx), %rdx
	movq	$32, %rcx
	movq	%r10, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jge	.LBB36_263
# BB#264:                               # %land.lhs.true.i294.i.i
                                        #   in Loop: Header=BB36_258 Depth=4
	movslq	8(%rax), %rsi
	movq	$32, %rcx
	movq	%r10, %r14
	shlq	%cl, %r14
	movzbl	(%rbx), %edi
	movq	$32, %rcx
	sarq	%cl, %r14
	cmpq	%r14, %rsi
	jge	.LBB36_266
# BB#265:                               # %land.lhs.true.i294.i.i
                                        #   in Loop: Header=BB36_258 Depth=4
	cmpq	$37, %rdi
	jne	.LBB36_270
.LBB36_266:                             # %lor.lhs.false42.i310.i.i
                                        #   in Loop: Header=BB36_258 Depth=4
	cmpq	$37, %rdi
	jne	.LBB36_268
	jmp	.LBB36_267
	.align	16, 0x90
.LBB36_263:                             # %if.end5.i292.if.then47.i313_crit_edge.i.i
                                        #   in Loop: Header=BB36_258 Depth=4
	movl	8(%rax), %esi
.LBB36_267:                             # %if.then47.i313.i.i
                                        #   in Loop: Header=BB36_258 Depth=4
	movq	24(%rbx), %rbx
.LBB36_268:                             # %if.end50.i316.i.i
                                        #   in Loop: Header=BB36_258 Depth=4
	movq	$32, %rcx
	movq	%r10, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB36_258
	jmp	.LBB36_269
.LBB36_244:                             #   in Loop: Header=BB36_236 Depth=3
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%r9, %r14
	jmp	.LBB36_271
.LBB36_270:                             # %loop_insn_first_p.exit321.i.i
                                        #   in Loop: Header=BB36_236 Depth=3
	movq	112(%rsp), %rcx         # 8-byte Reload
	movslq	(%rcx,%rdx,4), %rax
	movslq	(%rcx,%rsi,4), %rcx
	cmpq	%rcx, %rax
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%r9, %r14
	jg	.LBB36_271
	jmp	.LBB36_298
.LBB36_283:                             # %if.then56.i270.i.i
                                        #   in Loop: Header=BB36_271 Depth=4
	movq	24(%r14), %r14
	.align	16, 0x90
.LBB36_271:                             # %for.cond.i236.i.i
                                        #   Parent Loop BB36_205 Depth=1
                                        #     Parent Loop BB36_206 Depth=2
                                        #       Parent Loop BB36_236 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	168(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r14
	je	.LBB36_236
# BB#272:                               # %for.cond.i236.i.i
                                        #   in Loop: Header=BB36_271 Depth=4
	testq	%rsi, %rsi
	je	.LBB36_236
# BB#273:                               # %if.end.i239.i.i
                                        #   in Loop: Header=BB36_271 Depth=4
	cmpq	%r9, %rsi
	je	.LBB36_274
# BB#275:                               # %if.end.i239.i.i
                                        #   in Loop: Header=BB36_271 Depth=4
	testq	%r14, %r14
	je	.LBB36_274
# BB#276:                               # %if.end5.i243.i.i
                                        #   in Loop: Header=BB36_271 Depth=4
	movslq	8(%rsi), %rdx
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jge	.LBB36_277
# BB#278:                               # %land.lhs.true.i245.i.i
                                        #   in Loop: Header=BB36_271 Depth=4
	movslq	8(%r14), %rax
	movq	$32, %rcx
	movq	%r10, %rbx
	shlq	%cl, %rbx
	movzbl	(%rsi), %edi
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rax
	jge	.LBB36_280
# BB#279:                               # %land.lhs.true.i245.i.i
                                        #   in Loop: Header=BB36_271 Depth=4
	cmpq	$37, %rdi
	jne	.LBB36_284
.LBB36_280:                             # %lor.lhs.false42.i261.i.i
                                        #   in Loop: Header=BB36_271 Depth=4
	cmpq	$37, %rdi
	jne	.LBB36_282
	jmp	.LBB36_281
	.align	16, 0x90
.LBB36_277:                             # %if.end5.i243.if.then47.i264_crit_edge.i.i
                                        #   in Loop: Header=BB36_271 Depth=4
	movl	8(%r14), %eax
.LBB36_281:                             # %if.then47.i264.i.i
                                        #   in Loop: Header=BB36_271 Depth=4
	movq	24(%rsi), %rsi
.LBB36_282:                             # %if.end50.i267.i.i
                                        #   in Loop: Header=BB36_271 Depth=4
	movq	$32, %rcx
	movq	%r10, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB36_271
	jmp	.LBB36_283
.LBB36_274:                             #   in Loop: Header=BB36_236 Depth=3
	movq	%r9, %rsi
	movq	160(%rsp), %r14         # 8-byte Reload
	jmp	.LBB36_285
.LBB36_284:                             # %loop_insn_first_p.exit272.i.i
                                        #   in Loop: Header=BB36_236 Depth=3
	movq	112(%rsp), %rsi         # 8-byte Reload
	movslq	(%rsi,%rdx,4), %rcx
	movslq	(%rsi,%rax,4), %rax
	cmpq	%rax, %rcx
	movq	%r9, %rsi
	movq	160(%rsp), %r14         # 8-byte Reload
	jg	.LBB36_236
	jmp	.LBB36_285
.LBB36_296:                             # %if.then56.i.i.i
                                        #   in Loop: Header=BB36_285 Depth=4
	movq	24(%r14), %r14
	.align	16, 0x90
.LBB36_285:                             # %for.cond.i.i.i
                                        #   Parent Loop BB36_205 Depth=1
                                        #     Parent Loop BB36_206 Depth=2
                                        #       Parent Loop BB36_236 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	cmpq	%r9, %r14
	je	.LBB36_236
# BB#286:                               # %for.cond.i.i.i
                                        #   in Loop: Header=BB36_285 Depth=4
	testq	%rsi, %rsi
	je	.LBB36_236
# BB#287:                               # %if.end.i.i.i
                                        #   in Loop: Header=BB36_285 Depth=4
	movq	160(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %rsi
	je	.LBB36_298
# BB#288:                               # %if.end.i.i.i
                                        #   in Loop: Header=BB36_285 Depth=4
	testq	%r14, %r14
	je	.LBB36_298
# BB#289:                               # %if.end5.i.i.i
                                        #   in Loop: Header=BB36_285 Depth=4
	movslq	8(%rsi), %rdx
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jge	.LBB36_290
# BB#291:                               # %land.lhs.true.i218.i.i
                                        #   in Loop: Header=BB36_285 Depth=4
	movslq	8(%r14), %rax
	movq	$32, %rcx
	movq	%r10, %rbx
	shlq	%cl, %rbx
	movzbl	(%rsi), %edi
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rax
	jge	.LBB36_293
# BB#292:                               # %land.lhs.true.i218.i.i
                                        #   in Loop: Header=BB36_285 Depth=4
	cmpq	$37, %rdi
	jne	.LBB36_297
.LBB36_293:                             # %lor.lhs.false42.i.i.i
                                        #   in Loop: Header=BB36_285 Depth=4
	cmpq	$37, %rdi
	jne	.LBB36_295
	jmp	.LBB36_294
	.align	16, 0x90
.LBB36_290:                             # %if.end5.i.if.then47.i_crit_edge.i.i
                                        #   in Loop: Header=BB36_285 Depth=4
	movl	8(%r14), %eax
.LBB36_294:                             # %if.then47.i.i.i
                                        #   in Loop: Header=BB36_285 Depth=4
	movq	24(%rsi), %rsi
.LBB36_295:                             # %if.end50.i.i.i
                                        #   in Loop: Header=BB36_285 Depth=4
	movq	$32, %rcx
	movq	%r10, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB36_285
	jmp	.LBB36_296
.LBB36_297:                             # %loop_insn_first_p.exit.i.i
                                        #   in Loop: Header=BB36_236 Depth=3
	movq	112(%rsp), %rsi         # 8-byte Reload
	movslq	(%rsi,%rdx,4), %rcx
	movslq	(%rsi,%rax,4), %rax
	cmpq	%rax, %rcx
	jg	.LBB36_236
.LBB36_298:                             # %if.then126.i.i
                                        #   in Loop: Header=BB36_206 Depth=2
	andq	$-4, %r15
	orq	$2, %r15
	movw	%r15w, 100(%r12)
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	152(%rsp), %rbx         # 8-byte Reload
	je	.LBB36_300
# BB#299:                               # %if.then135.i.i
                                        #   in Loop: Header=BB36_206 Depth=2
	movq	$35, %rsi
	movq	$1, %rdx
	movabsq	$.L.str60, %rdi
	movq	%r9, %r14
	callq	fwrite
	movq	%r14, %r9
	movzbl	100(%r12), %r15d
	jmp	.LBB36_300
.LBB36_239:                             #   in Loop: Header=BB36_206 Depth=2
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	152(%rsp), %rbx         # 8-byte Reload
.LBB36_300:                             # %if.end140.i.i
                                        #   in Loop: Header=BB36_206 Depth=2
	testq	$1, %r15
	je	.LBB36_302
# BB#301:                               # %if.then146.i.i
                                        #   in Loop: Header=BB36_206 Depth=2
	movq	%rbx, 88(%r12)
	.align	16, 0x90
.LBB36_302:                             # %if.end149.i.i
                                        #   in Loop: Header=BB36_206 Depth=2
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB36_305
# BB#303:                               # %land.lhs.true151.i.i
                                        #   in Loop: Header=BB36_206 Depth=2
	movzbl	100(%r12), %eax
	testq	$1, %rax
	je	.LBB36_305
# BB#304:                               # %if.then157.i.i
                                        #   in Loop: Header=BB36_206 Depth=2
	movq	(%r12), %rax
	movq	32(%r12), %rcx
	movl	8(%rcx), %ecx
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str61, %rsi
	movq	%r9, %rbx
	callq	fprintf
	movq	%rbx, %r9
	jmp	.LBB36_305
.LBB36_216:                             #   in Loop: Header=BB36_206 Depth=2
	movq	%rax, 152(%rsp)         # 8-byte Spill
	jmp	.LBB36_217
	.align	16, 0x90
.LBB36_306:                             # %for.inc10.i
                                        #   in Loop: Header=BB36_205 Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	64(%rax), %rax
	testq	%rax, %rax
	jne	.LBB36_205
# BB#307:                               # %loop_givs_check.exit.loopexit
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	160(%rdi), %r15
.LBB36_308:                             # %loop_givs_check.exit
	movq	%rdi, 176(%rsp)         # 8-byte Spill
	movq	200(%rdi), %r13
	movq	192(%rdi), %rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	16(%r13), %r12
	movq	%r12, %rsi
	callq	get_condition_for_loop
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB36_512
# BB#309:                               # %if.end.i534
	movq	%r12, %rdi
	callq	onlyjump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB36_512
# BB#310:                               # %if.end7.i
	movzbl	(%r12), %eax
	cmpq	$33, %rax
	jne	.LBB36_317
# BB#311:                               # %lor.lhs.false.i.i540
	movq	%r12, %rdi
	callq	any_condjump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB36_317
# BB#312:                               # %if.end.i.i543
	movq	%r12, %rdi
	callq	pc_set
	movq	16(%rax), %rcx
	movq	24(%rcx), %rax
	movzbl	(%rax), %edi
	movq	8(%rcx), %rsi
	xorq	%rdx, %rdx
	cmpq	$67, %rdi
	jne	.LBB36_316
# BB#313:                               # %land.rhs.i.i
	movq	8(%rax), %rax
	movq	64(%r12), %rcx
	cmpq	%rcx, %rax
	jne	.LBB36_314
# BB#315:                               # %land.rhs.i.i
	movabsq	$1, %rdx
	jmp	.LBB36_316
.LBB36_105:                             # %if.end19.i
	movq	224(%rax), %rax
	xorq	%r15, %r15
	testq	%rax, %rax
	movabsq	$0, %r13
	movabsq	$0, %rbx
	movabsq	$0, %rcx
	je	.LBB36_167
# BB#106:                               # %for.body.lr.ph.i487
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	xorq	%r13, %r13
	.align	16, 0x90
.LBB36_107:                             # %for.body.i488
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB36_109 Depth 2
                                        #     Child Loop BB36_124 Depth 2
                                        #       Child Loop BB36_136 Depth 3
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	8(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB36_145
# BB#108:                               # %while.body.lr.ph.i
                                        #   in Loop: Header=BB36_107 Depth=1
	movq	72(%rbx), %rax
	movzbl	(%rax), %eax
	xorq	%r15, %r15
	movq	%rbx, %rcx
	.align	16, 0x90
.LBB36_109:                             # %while.body.i491
                                        #   Parent Loop BB36_107 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	$54, %rax
	jne	.LBB36_110
# BB#116:                               # %if.end33.i
                                        #   in Loop: Header=BB36_109 Depth=2
	movzbl	100(%rbx), %edx
	testq	$32, %rdx
	jne	.LBB36_117
# BB#120:                               # %if.end52.i
                                        #   in Loop: Header=BB36_109 Depth=2
	movq	72(%rcx), %rdx
	movq	8(%rdx), %rdx
	movq	128(%rcx), %rcx
	andq	%rdi, %r15
	addq	%rdx, %r15
	testq	%rcx, %rcx
	jne	.LBB36_109
# BB#121:                               # %while.end.i
                                        #   in Loop: Header=BB36_107 Depth=1
	testq	%rdi, %r15
	je	.LBB36_145
# BB#122:                               # %if.end61.i
                                        #   in Loop: Header=BB36_107 Depth=1
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	24(%rax), %r12
	testq	%r12, %r12
	je	.LBB36_145
# BB#123:                               # %for.body64.lr.ph.i
                                        #   in Loop: Header=BB36_107 Depth=1
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	jmp	.LBB36_124
.LBB36_110:                             # %if.then24.i
                                        #   in Loop: Header=BB36_107 Depth=1
	movq	%rdi, %r14
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	jne	.LBB36_114
# BB#111:                               #   in Loop: Header=BB36_107 Depth=1
	movq	%r14, %rdi
	jmp	.LBB36_145
.LBB36_117:                             # %if.then37.i
                                        #   in Loop: Header=BB36_107 Depth=1
	movq	%rdi, %r14
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	jne	.LBB36_119
# BB#118:                               #   in Loop: Header=BB36_107 Depth=1
	movq	%r14, %rdi
	jmp	.LBB36_145
.LBB36_114:                             # %if.then26.i
                                        #   in Loop: Header=BB36_107 Depth=1
	movq	(%rbx), %rax
	movq	16(%rbx), %rdx
	movl	8(%rax), %ecx
	movl	8(%rdx), %edx
	xorq	%rax, %rax
	movabsq	$.L.str64, %rsi
	jmp	.LBB36_115
.LBB36_119:                             # %if.then39.i
                                        #   in Loop: Header=BB36_107 Depth=1
	movq	(%rbx), %rax
	movq	16(%rbx), %rdx
	movl	8(%rax), %ecx
	movl	8(%rdx), %edx
	xorq	%rax, %rax
	movabsq	$.L.str65, %rsi
.LBB36_115:                             # %for.inc250.i
                                        #   in Loop: Header=BB36_107 Depth=1
	callq	fprintf
	movq	loop_dump_stream(%rip), %rdi
	movq	72(%rbx), %rsi
	callq	print_rtl
	movq	loop_dump_stream(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	movq	%r14, %rdi
	jmp	.LBB36_145
.LBB36_140:                             # %if.then152.i
                                        #   in Loop: Header=BB36_124 Depth=2
	movl	192(%rsp), %eax
	movzbl	(%r13), %ecx
	orq	%rcx, %rax
	andq	$1, %rax
	andq	$254, %rcx
	orq	%rax, %rcx
	movb	%cl, (%r13)
	movl	-8(%r13), %eax
	movq	112(%rsp), %rcx         # 8-byte Reload
	addq	%rcx, %rax
	movl	%eax, -8(%r13)
	movq	%rdx, -24(%r13)
	movq	%r12, -40(%r13)
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	%rax, -48(%r13)
	movq	$32, %rcx
	movq	72(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$56, %rax, %rax
	movq	152(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 224(%rsp,%rax)
	jmp	.LBB36_144
.LBB36_143:                             # %if.then188.i
                                        #   in Loop: Header=BB36_124 Depth=2
	movl	192(%rsp), %eax
	movzbl	(%r13), %ecx
	orq	%rcx, %rax
	andq	$1, %rax
	andq	$254, %rcx
	orq	%rax, %rcx
	movb	%cl, (%r13)
	movl	-8(%r13), %eax
	movq	112(%rsp), %rcx         # 8-byte Reload
	addq	%rcx, %rax
	movl	%eax, -8(%r13)
	movq	72(%rsp), %r13          # 8-byte Reload
	jmp	.LBB36_144
	.align	16, 0x90
.LBB36_124:                             # %for.body64.i
                                        #   Parent Loop BB36_107 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB36_136 Depth 3
	movq	%rdi, %r14
	movl	24(%r12), %eax
	testq	%rax, %rax
	jne	.LBB36_129
# BB#125:                               # %lor.lhs.false70.i
                                        #   in Loop: Header=BB36_124 Depth=2
	movq	64(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB36_129
# BB#126:                               # %lor.lhs.false75.i
                                        #   in Loop: Header=BB36_124 Depth=2
	movq	8(%rax), %rax
	imulq	%r15, %rax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	cmpq	$4096, %rax             # imm = 0x1000
	ja	.LBB36_129
# BB#127:                               # %lor.lhs.false86.i
                                        #   in Loop: Header=BB36_124 Depth=2
	movslq	48(%r12), %rbx
	movq	72(%r12), %rsi
	movq	176(%rsp), %rdi         # 8-byte Reload
	callq	loop_invariant_p
	testq	%r14, %rax
	je	.LBB36_129
# BB#128:                               # %lor.lhs.false90.i
                                        #   in Loop: Header=BB36_124 Depth=2
	movzbl	100(%r12), %eax
	testq	$32, %rax
	jne	.LBB36_129
# BB#132:                               # %if.end106.i
                                        #   in Loop: Header=BB36_124 Depth=2
	movzbl	mode_size(%rbx), %ebx
	movq	72(%r12), %rdi
	callq	copy_rtx
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	40(%rcx), %rdi
	movq	%rax, 152(%rsp)         # 8-byte Spill
	callq	copy_rtx
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$75, %rdi
	movq	%rax, %rdx
	movq	152(%rsp), %rcx         # 8-byte Reload
	callq	simplify_gen_binary
	movq	%rax, 200(%rsp)
	leaq	200(%rsp), %rdi
	callq	remove_constant_addition
	xorq	%rcx, %rcx
	movl	%ecx, 192(%rsp)
	movq	40(%r12), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, 184(%rsp)
	movzbl	100(%r12), %ecx
	testq	$16, %rcx
	je	.LBB36_134
# BB#133:                               # %if.then121.i
                                        #   in Loop: Header=BB36_124 Depth=2
	movq	(%r12), %rcx
	movq	32(%rcx), %rdi
	movabsq	$check_store, %rsi
	movq	%rbx, 112(%rsp)         # 8-byte Spill
	movq	%rax, %rbx
	leaq	184(%rsp), %rdx
	callq	note_stores
	movq	%rbx, %rax
	movq	112(%rsp), %rbx         # 8-byte Reload
.LBB36_134:                             # %for.cond126.preheader.i
                                        #   in Loop: Header=BB36_124 Depth=2
	movq	$32, %rcx
	movq	%r13, %rdx
	movq	%r13, 72(%rsp)          # 8-byte Spill
	shlq	%cl, %rdx
	movq	$32, %rcx
	movq	200(%rsp), %rsi
	movq	%rsi, 152(%rsp)         # 8-byte Spill
	sarq	%cl, %rdx
	addq	%rbx, %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%rbx, 112(%rsp)         # 8-byte Spill
	testq	%rdx, %rdx
	jle	.LBB36_147
# BB#135:                               #   in Loop: Header=BB36_124 Depth=2
	movq	$1, %rbx
	leaq	256(%rsp), %r13
	.align	16, 0x90
.LBB36_136:                             # %for.body129.i
                                        #   Parent Loop BB36_107 Depth=1
                                        #     Parent Loop BB36_124 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	-32(%r13), %rsi
	movq	152(%rsp), %rdi         # 8-byte Reload
	callq	rtx_equal_for_prefetch_p
	movq	%r14, %rdi
	testq	%rdi, %rax
	je	.LBB36_146
# BB#137:                               # %land.lhs.true134.i
                                        #   in Loop: Header=BB36_136 Depth=3
	movq	-16(%r13), %rax
	cmpq	%rax, 160(%rsp)         # 8-byte Folded Reload
	jne	.LBB36_146
# BB#138:                               # %if.then140.i
                                        #   in Loop: Header=BB36_136 Depth=3
	movq	-24(%r13), %rax
	movq	120(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, %rcx
	subq	%rax, %rcx
	jl	.LBB36_141
# BB#139:                               # %if.then140.i
                                        #   in Loop: Header=BB36_136 Depth=3
	cmpq	$4095, %rcx             # imm = 0xFFF
	jle	.LBB36_140
.LBB36_141:                             # %if.end175.i
                                        #   in Loop: Header=BB36_136 Depth=3
	subq	%rdx, %rax
	jle	.LBB36_146
# BB#142:                               # %if.end175.i
                                        #   in Loop: Header=BB36_136 Depth=3
	cmpq	$4095, %rax             # imm = 0xFFF
	jle	.LBB36_143
	.align	16, 0x90
.LBB36_146:                             # %for.inc.i502
                                        #   in Loop: Header=BB36_136 Depth=3
	movq	%rdi, %r14
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	72(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	addq	$56, %r13
	cmpq	%rdx, %rax
	jl	.LBB36_136
.LBB36_147:                             # %if.then209.i
                                        #   in Loop: Header=BB36_124 Depth=2
	movq	$32, %rcx
	movq	72(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$56, %rax, %rcx
	movq	%r12, 216(%rsp,%rcx)
	movq	168(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, 208(%rsp,%rcx)
	movq	120(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, 232(%rsp,%rcx)
	movq	160(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, 240(%rsp,%rcx)
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, 224(%rsp,%rcx)
	movzbl	192(%rsp), %edx
	movzbl	256(%rsp,%rcx), %esi
	andq	$1, %rdx
	andq	$254, %rsi
	orq	%rdx, %rsi
	cmpq	$99, %rax
	movb	%sil, 256(%rsp,%rcx)
	movq	112(%rsp), %rax         # 8-byte Reload
	movl	%eax, 248(%rsp,%rcx)
	jge	.LBB36_149
# BB#148:                               #   in Loop: Header=BB36_124 Depth=2
	incq	%r13
	movq	%r14, %rdi
	jmp	.LBB36_144
	.align	16, 0x90
.LBB36_129:                             # %if.then97.i
                                        #   in Loop: Header=BB36_124 Depth=2
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB36_131
# BB#130:                               # %if.then99.i
                                        #   in Loop: Header=BB36_124 Depth=2
	movq	(%r12), %rax
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str66, %rsi
	callq	fprintf
.LBB36_131:                             # %for.inc247.i
                                        #   in Loop: Header=BB36_124 Depth=2
	movq	%r14, %rdi
.LBB36_144:                             # %for.inc247.i
                                        #   in Loop: Header=BB36_124 Depth=2
	movq	128(%r12), %r12
	testq	%r12, %r12
	jne	.LBB36_124
	.align	16, 0x90
.LBB36_145:                             # %for.inc250.i
                                        #   in Loop: Header=BB36_107 Depth=1
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	64(%rax), %rax
	testq	%rax, %rax
	jne	.LBB36_107
# BB#112:                               # %for.cond252.preheader.i
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%r15, %r15
	testq	%rax, %rax
	jle	.LBB36_113
# BB#151:                               # %for.body255.lr.ph.i
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	160(%rax), %r10
	movq	ix86_cost(%rip), %r11
	xorq	%r12, %r12
	movq	$1, %rbx
	xorq	%r8, %r8
	leaq	256(%rsp), %rsi
	.align	16, 0x90
.LBB36_152:                             # %for.body255.i
                                        # =>This Inner Loop Header: Depth=1
	movq	96(%r10), %rcx
	movq	-16(%rsi), %r15
	testq	%rcx, %rcx
	je	.LBB36_155
# BB#153:                               # %land.lhs.true259.i
                                        #   in Loop: Header=BB36_152 Depth=1
	xorq	%rdx, %rdx
	movq	%rdi, %rax
	idivq	%r15
	cmpq	%rcx, %rax
	jae	.LBB36_154
.LBB36_155:                             # %if.else.i505
                                        #   in Loop: Header=BB36_152 Depth=1
	movl	%edi, -4(%rsi)
	movq	%rdi, %rcx
	movq	%rdi, %r9
	jmp	.LBB36_156
.LBB36_154:                             # %if.then267.i
                                        #   in Loop: Header=BB36_152 Depth=1
	movq	%rdi, %r9
	imulq	%r15, %rcx
	movl	%ecx, -4(%rsi)
.LBB36_156:                             # %if.end280.i
                                        #   in Loop: Header=BB36_152 Depth=1
	movl	-8(%rsi), %eax
	andq	$16777215, %rax         # imm = 0xFFFFFF
	shlq	$8, %rax
	xorq	%rdx, %rdx
	idivq	%r15
	cmpq	$221, %rax
	jl	.LBB36_161
# BB#157:                               # %land.lhs.true292.i
                                        #   in Loop: Header=BB36_152 Depth=1
	movl	152(%r11), %edi
	andq	%r9, %rcx
	xorq	%rdx, %rdx
	movq	%rcx, %rax
	divq	%rdi
	cmpq	$2, %rax
	jb	.LBB36_161
# BB#158:                               # %if.then299.i
                                        #   in Loop: Header=BB36_152 Depth=1
	movzbl	(%rsi), %edi
	orq	$4, %rdi
	movb	%dil, (%rsi)
	movl	152(%r11), %r14d
	xorq	%rdx, %rdx
	movq	%rcx, %rax
	divq	%r14
	cmpq	$6, %rax
	movabsq	$0, %rax
	jbe	.LBB36_160
# BB#159:                               # %if.then299.i
                                        #   in Loop: Header=BB36_152 Depth=1
	movabsq	$1, %rax
.LBB36_160:                             # %if.then299.i
                                        #   in Loop: Header=BB36_152 Depth=1
	addq	%rax, %rax
	andq	$-3, %rdi
	orq	%rax, %rdi
	jmp	.LBB36_162
	.align	16, 0x90
.LBB36_161:                             # %if.else319.i
                                        #   in Loop: Header=BB36_152 Depth=1
	movzbl	(%rsi), %edi
	andq	$-7, %rdi
.LBB36_162:                             # %if.end330.i
                                        #   in Loop: Header=BB36_152 Depth=1
	movb	%dil, (%rsi)
	testq	$2, %rdi
	je	.LBB36_165
# BB#163:                               # %if.then339.i
                                        #   in Loop: Header=BB36_152 Depth=1
	movslq	152(%r11), %rcx
	leaq	-1(%r15,%rcx), %rax
	cqto
	idivq	%rcx
	andq	%r9, %r12
	addq	%rax, %r12
	testq	$1, %rdi
	je	.LBB36_165
# BB#164:                               # %if.then360.i
                                        #   in Loop: Header=BB36_152 Depth=1
	andq	%r9, %r8
	addq	%rax, %r8
.LBB36_165:                             # %for.inc376.i
                                        #   in Loop: Header=BB36_152 Depth=1
	movq	%r9, %rdi
	movq	%rbx, %rax
	andq	%rdi, %rax
	movq	%r13, %rcx
	andq	%rdi, %rcx
	incq	%rbx
	addq	$56, %rsi
	cmpq	%rcx, %rax
	movq	$1, %r15
	jne	.LBB36_152
# BB#166:
	movq	%r8, %rcx
	movq	%r12, %rbx
	jmp	.LBB36_167
.LBB36_314:
	movabsq	$0, %rdx
.LBB36_316:                             # %land.end.i.i
	leaq	5816(%rsp), %rcx
	xorq	%r8, %r8
	movq	%r12, %rdi
	callq	canonicalize_condition
.LBB36_317:                             # %get_condition.exit.i
	movq	5816(%rsp), %rbx
	cmpq	%r12, %rbx
	movq	$1, %rax
	movq	%r12, %rdi
	je	.LBB36_320
# BB#318:                               # %if.else.i545
	movq	%r12, %rdi
	callq	prev_nonnote_insn
	cmpq	%rax, %rbx
	jne	.LBB36_512
# BB#319:                               # %if.else.if.end16_crit_edge.i
	movq	5816(%rsp), %rdi
	movq	$2, %rax
.LBB36_320:                             # %if.end16.i
	movq	%rax, 160(%rsp)         # 8-byte Spill
	callq	prev_nonnote_insn
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	176(%rcx), %rcx
	cmpq	%rcx, %rax
	je	.LBB36_322
# BB#321:                               # %if.then19.i
	movzbl	(%rax), %eax
	cmpq	$33, %rax
	je	.LBB36_512
.LBB36_322:                             # %if.end23.i
	movq	224(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB36_512
# BB#323:                               # %for.body.lr.ph.i550
	leaq	184(%r15), %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB36_324:                             # %for.body.i552
                                        # =>This Inner Loop Header: Depth=1
	movl	4(%rbx), %eax
	cmpq	$1, %rax
	jne	.LBB36_328
# BB#325:                               # %land.lhs.true.i555
                                        #   in Loop: Header=BB36_324 Depth=1
	movq	8(%rbx), %rax
	movzbl	100(%rax), %ecx
	testq	$32, %rcx
	jne	.LBB36_328
# BB#326:                               # %land.lhs.true29.i
                                        #   in Loop: Header=BB36_324 Depth=1
	movq	32(%rax), %rcx
	movq	8(%r14), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB36_328
# BB#327:                               # %land.lhs.true35.i
                                        #   in Loop: Header=BB36_324 Depth=1
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	(%rbx), %edx
	movq	80(%rcx), %rcx
	movq	(%rcx,%rdx,8), %rdi
	movq	(%rax), %rsi
	movq	5816(%rsp), %rdx
	callq	reg_used_between_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB36_329
	.align	16, 0x90
.LBB36_328:                             # %for.inc.i563
                                        #   in Loop: Header=BB36_324 Depth=1
	movq	64(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB36_324
	jmp	.LBB36_512
.LBB36_113:
	xorq	%rbx, %rbx
	xorq	%rcx, %rcx
.LBB36_167:                             # %for.end378.i
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB36_178
# BB#168:                               # %for.cond381.preheader.i
	movq	%rbx, 160(%rsp)         # 8-byte Spill
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	movq	%r13, 72(%rsp)          # 8-byte Spill
	testq	%r15, %r15
	je	.LBB36_177
# BB#169:                               # %for.body384.i.preheader
	leaq	256(%rsp), %rbx
	movq	$1, %r13
	movabsq	$.L.str70, %r14
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB36_170:                             # %for.body384.i
                                        # =>This Inner Loop Header: Depth=1
	movq	-40(%rbx), %rax
	movq	(%rax), %rax
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str68, %rsi
	callq	fprintf
	movq	loop_dump_stream(%rip), %rdi
	movq	-32(%rbx), %rsi
	callq	print_rtl
	movq	loop_dump_stream(%rip), %rcx
	movq	$8, %rsi
	movq	$1, %rdx
	movabsq	$.L.str69, %rdi
	callq	fwrite
	movq	loop_dump_stream(%rip), %rdi
	movq	-24(%rbx), %rdx
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	fprintf
	movq	loop_dump_stream(%rip), %rcx
	movq	$9, %rsi
	movq	$1, %rdx
	movabsq	$.L.str71, %rdi
	callq	fwrite
	movq	loop_dump_stream(%rip), %rdi
	movq	-16(%rbx), %rdx
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	fprintf
	movl	-8(%rbx), %eax
	movq	loop_dump_stream(%rip), %rdi
	imulq	$100, %rax, %rax
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rcx, %rax
	xorq	%rdx, %rdx
	idivq	-16(%rbx)
	movq	%rax, %rdx
	movzbl	(%rbx), %eax
	testq	$1, %rax
	movabsq	$.L.str73, %r8
	jne	.LBB36_172
# BB#171:                               # %for.body384.i
                                        #   in Loop: Header=BB36_170 Depth=1
	movabsq	$.L.str74, %r8
.LBB36_172:                             # %for.body384.i
                                        #   in Loop: Header=BB36_170 Depth=1
	movq	%rax, %rsi
	andq	$2, %rsi
	movq	$1, %rcx
	shrq	%cl, %rsi
	testq	%rsi, %rsi
	movabsq	$.L.str75, %r9
	jne	.LBB36_174
# BB#173:                               # %for.body384.i
                                        #   in Loop: Header=BB36_170 Depth=1
	movabsq	$.L.str76, %r9
.LBB36_174:                             # %for.body384.i
                                        #   in Loop: Header=BB36_170 Depth=1
	andq	$4, %rax
	movq	$2, %rcx
	shrq	%cl, %rax
	testq	%rax, %rax
	movabsq	$.L.str75, %rsi
	jne	.LBB36_176
# BB#175:                               # %for.body384.i
                                        #   in Loop: Header=BB36_170 Depth=1
	movabsq	$.L.str76, %rsi
.LBB36_176:                             # %for.body384.i
                                        #   in Loop: Header=BB36_170 Depth=1
	movl	-4(%rbx), %ecx
	xorq	%rax, %rax
	movq	%rsi, (%rsp)
	movabsq	$.L.str72, %rsi
	callq	fprintf
	movq	loop_dump_stream(%rip), %rdi
	movq	%r13, %rax
	andq	%r12, %rax
	movq	72(%rsp), %rcx          # 8-byte Reload
	andq	%r12, %rcx
	incq	%r13
	addq	$56, %rbx
	cmpq	%rcx, %rax
	jne	.LBB36_170
.LBB36_177:                             # %for.end448.i
	movabsq	$.L.str77, %rsi
	xorq	%rax, %rax
	movq	160(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdx
	movq	168(%rsp), %rcx         # 8-byte Reload
	callq	fprintf
	movq	72(%rsp), %r13          # 8-byte Reload
.LBB36_178:                             # %if.end450.i
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rbx
	je	.LBB36_203
# BB#179:                               # %if.end453.i
	movq	ix86_cost(%rip), %rax
	movslq	156(%rax), %rax
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cqto
	idivq	%rbx
	movq	%rax, 64(%rsp)          # 8-byte Spill
	testq	%rsi, %rax
	je	.LBB36_203
# BB#180:                               # %if.end453.i
	xorq	$1, %r15
	jne	.LBB36_203
# BB#181:                               # %for.body461.lr.ph.i
	movq	64(%rsp), %rax          # 8-byte Reload
	andq	%rsi, %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	xorq	%rdx, %rdx
	andq	%rsi, %r13
	movq	%r13, 72(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB36_182:                             # %for.body461.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB36_186 Depth 2
                                        #       Child Loop BB36_192 Depth 3
                                        #     Child Loop BB36_199 Depth 2
	imulq	$56, %rdx, %rdi
	movzbl	256(%rsp,%rdi), %ecx
	leaq	256(%rsp,%rdi), %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	testq	$2, %rcx
	jne	.LBB36_184
# BB#183:                               #   in Loop: Header=BB36_182 Depth=1
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	jmp	.LBB36_195
	.align	16, 0x90
.LBB36_184:                             # %for.cond471.preheader.i
                                        #   in Loop: Header=BB36_182 Depth=1
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movq	ix86_cost(%rip), %rax
	movq	240(%rsp,%rdi), %rdx
	movslq	152(%rax), %rsi
	leaq	-1(%rdx,%rsi), %rax
	cqto
	idivq	%rsi
	testq	%rax, %rax
	jle	.LBB36_195
# BB#185:                               # %for.body485.lr.ph.i
                                        #   in Loop: Header=BB36_182 Depth=1
	leaq	240(%rsp,%rdi), %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	leaq	216(%rsp,%rdi), %r13
	movq	%rdi, 88(%rsp)          # 8-byte Spill
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB36_186:                             # %for.body485.i
                                        #   Parent Loop BB36_182 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB36_192 Depth 3
	movq	%rbx, 168(%rsp)         # 8-byte Spill
	movq	(%r13), %rax
	movq	40(%rax), %rax
	movq	(%rax), %rdi
	callq	copy_rtx
	movq	(%r13), %rcx
	movq	ix86_cost(%rip), %rdx
	movq	(%rcx), %r12
	movl	152(%rdx), %ecx
	movq	16(%r12), %rdx
	movq	%rdx, 160(%rsp)         # 8-byte Spill
	movq	%rax, %r14
	movq	112(%rsp), %rax         # 8-byte Reload
	leaq	(%rbx,%rax), %r15
	imulq	%rcx, %r15
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	offsettable_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB36_187
# BB#188:                               # %if.else511.i
                                        #   in Loop: Header=BB36_186 Depth=2
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rbx
	movq	const_int_rtx+520(%rip), %rax
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_CONST_INT
	xorq	%r9, %r9
	movq	%r12, (%rsp)
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	%r14, %rsi
	movq	96(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	movq	%rbx, %r8
	callq	loop_iv_add_mult_emit_before
	jmp	.LBB36_189
	.align	16, 0x90
.LBB36_187:                             # %if.then508.i
                                        #   in Loop: Header=BB36_186 Depth=2
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	plus_constant_wide
	movq	%rax, %rbx
.LBB36_189:                             # %if.end518.i
                                        #   in Loop: Header=BB36_186 Depth=2
	callq	start_sequence
	movq	insn_data+52664(%rip), %rax
	movzbl	16(%rax), %esi
	movq	%rbx, %rdi
	callq	*(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB36_191
# BB#190:                               # %if.then526.i
                                        #   in Loop: Header=BB36_186 Depth=2
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %rbx
.LBB36_191:                             # %if.end531.i
                                        #   in Loop: Header=BB36_186 Depth=2
	movq	152(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %esi
	andq	$1, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	gen_prefetch
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	emit_insn_before
	movq	160(%rsp), %rax         # 8-byte Reload
	movq	24(%rax), %rsi
	cmpq	%r12, %rsi
	movq	176(%rsp), %rbx         # 8-byte Reload
	je	.LBB36_193
	.align	16, 0x90
.LBB36_192:                             # %while.body551.i
                                        #   Parent Loop BB36_182 Depth=1
                                        #     Parent Loop BB36_186 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%r13), %rax
	movzbl	100(%rax), %eax
	movq	$4, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	$5, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movq	%rbx, %rdi
	movq	%rax, %rcx
	callq	check_insn_for_givs
	movq	24(%rax), %rsi
	cmpq	%r12, %rsi
	jne	.LBB36_192
.LBB36_193:                             # %for.inc573.i
                                        #   in Loop: Header=BB36_186 Depth=2
	movq	ix86_cost(%rip), %rax
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rcx
	movslq	152(%rax), %rsi
	leaq	-1(%rcx,%rsi), %rax
	movq	168(%rsp), %rbx         # 8-byte Reload
	incq	%rbx
	cqto
	idivq	%rsi
	cmpq	%rax, %rbx
	jl	.LBB36_186
# BB#194:                               # %for.cond471.if.end576.loopexit_crit_edge.i
                                        #   in Loop: Header=BB36_182 Depth=1
	movq	152(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %ecx
	movq	88(%rsp), %rdi          # 8-byte Reload
.LBB36_195:                             # %if.end576.i
                                        #   in Loop: Header=BB36_182 Depth=1
	testq	$4, %rcx
	je	.LBB36_202
# BB#196:                               # %for.cond587.preheader.i
                                        #   in Loop: Header=BB36_182 Depth=1
	testq	$2, %rcx
	je	.LBB36_198
# BB#197:                               # %for.cond587.preheader.i
                                        #   in Loop: Header=BB36_182 Depth=1
	movq	$32, %rcx
	movq	64(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB36_202
.LBB36_198:                             # %land.rhs.lr.ph.i507
                                        #   in Loop: Header=BB36_182 Depth=1
	leaq	252(%rsp,%rdi), %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	leaq	208(%rsp,%rdi), %rax
	movq	%rax, 96(%rsp)          # 8-byte Spill
	leaq	216(%rsp,%rdi), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB36_199:                             # %land.rhs.i508
                                        #   Parent Loop BB36_182 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	ix86_cost(%rip), %rax
	movl	152(%rax), %eax
	imulq	%rsi, %rax
	movq	$32, %rcx
	movq	120(%rsp), %rdx         # 8-byte Reload
	movslq	(%rdx), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jge	.LBB36_202
# BB#200:                               # %for.body606.i
                                        #   in Loop: Header=BB36_199 Depth=2
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%rsi, 168(%rsp)         # 8-byte Spill
	callq	gen_reg_rtx
	movq	%rax, %r14
	movq	176(%rsp), %r13         # 8-byte Reload
	movq	192(%r13), %rax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	movq	ix86_cost(%rip), %rax
	andq	$1, %rbx
	orq	$4, %rbx
	movq	88(%rsp), %r12          # 8-byte Reload
	movq	(%r12), %rcx
	movl	152(%rax), %esi
	movq	72(%rcx), %r15
	movq	168(%rsp), %rax         # 8-byte Reload
	imulq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	simplify_gen_binary
	movq	96(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rcx
	movq	(%r12), %rdx
	movq	40(%rcx), %rsi
	movq	64(%rdx), %rdx
	xorq	%r9, %r9
	movq	160(%rsp), %r12         # 8-byte Reload
	movq	%r12, (%rsp)
	movq	%r13, %rdi
	movq	%rax, %rcx
	movq	%r14, %r8
	callq	loop_iv_add_mult_emit_before
	movq	152(%rsp), %rbx         # 8-byte Reload
	movzbl	(%rbx), %esi
	andq	$1, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r15
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	gen_rtx_CONST_INT
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	gen_prefetch
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	emit_insn_before
	movq	168(%rsp), %rsi         # 8-byte Reload
	movzbl	(%rbx), %eax
	incq	%rsi
	testq	$2, %rax
	je	.LBB36_199
# BB#201:                               # %for.body606.i
                                        #   in Loop: Header=BB36_199 Depth=2
	movq	$32, %rcx
	movq	64(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB36_199
	.align	16, 0x90
.LBB36_202:                             # %for.inc648.i
                                        #   in Loop: Header=BB36_182 Depth=1
	movq	56(%rsp), %rdx          # 8-byte Reload
	incq	%rdx
	movq	%rdx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	72(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB36_182
	jmp	.LBB36_203
.LBB36_329:                             # %if.end44.i
	movzbl	(%r14), %eax
	cmpq	$102, %rax
	jne	.LBB36_330
# BB#350:                               # %land.lhs.true65.i
	movq	16(%r14), %rax
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB36_333
	jmp	.LBB36_351
.LBB36_330:                             # %if.end44.i
	cmpq	$105, %rax
	jne	.LBB36_333
# BB#331:                               # %land.lhs.true48.i564
	movq	16(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB36_333
# BB#332:                               # %land.lhs.true55.i
	movq	8(%rax), %rax
	cmpq	$-1, %rax
	jne	.LBB36_333
.LBB36_351:                             # %land.lhs.true70.i
	movq	8(%rbx), %rax
	movq	72(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB36_333
# BB#352:                               # %land.lhs.true75.i
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	js	.LBB36_353
.LBB36_333:                             # %if.else187.i
	movq	8(%rbx), %rax
	movq	72(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB36_512
# BB#334:                               # %land.lhs.true193.i
	cmpq	$0, 8(%rax)
	jle	.LBB36_512
# BB#335:                               # %if.then200.i
	movl	16(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB36_379
# BB#336:                               # %land.lhs.true202.i
	movq	176(%rsp), %rax         # 8-byte Reload
	movl	240(%rax), %eax
	testq	%rax, %rax
	jne	.LBB36_379
# BB#337:                               # %land.lhs.true204.i
	movl	20(%r15), %eax
	testq	%rax, %rax
	jne	.LBB36_379
# BB#338:                               # %if.then206.i
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	(%rbx), %ecx
	movq	80(%rax), %rax
	movq	(%rax,%rcx,8), %r12
	cmpq	%r13, 168(%rsp)         # 8-byte Folded Reload
	je	.LBB36_372
# BB#339:                               # %for.body214.lr.ph.i
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	%rax, 120(%rsp)         # 8-byte Spill
.LBB36_340:                             # %for.body214.i
                                        # =>This Inner Loop Header: Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB36_371
# BB#341:                               # %cond.true.i
                                        #   in Loop: Header=BB36_340 Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB36_343
# BB#342:                               # %cond.false.i574
                                        #   in Loop: Header=BB36_340 Depth=1
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	single_set_2
.LBB36_343:                             # %cond.end245.i
                                        #   in Loop: Header=BB36_340 Depth=1
	testq	%rax, %rax
	je	.LBB36_346
# BB#344:                               # %land.lhs.true248.i
                                        #   in Loop: Header=BB36_340 Depth=1
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB36_346
# BB#345:                               # %land.lhs.true256.i
                                        #   in Loop: Header=BB36_340 Depth=1
	movl	8(%rax), %eax
	movl	(%rbx), %ecx
	cmpq	%rcx, %rax
	je	.LBB36_371
.LBB36_346:                             # %if.else266.i
                                        #   in Loop: Header=BB36_340 Depth=1
	movq	%r13, %rdi
	callq	prev_nonnote_insn
	movq	%rax, %rdi
	callq	prev_nonnote_insn
	cmpq	%rax, 120(%rsp)         # 8-byte Folded Reload
	je	.LBB36_348
# BB#347:                               # %lor.lhs.false271.i
                                        #   in Loop: Header=BB36_340 Depth=1
	movq	%r13, %rdi
	callq	prev_nonnote_insn
	cmpq	%rax, 120(%rsp)         # 8-byte Folded Reload
	jne	.LBB36_370
.LBB36_348:                             # %land.lhs.true275.i
                                        #   in Loop: Header=BB36_340 Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rax
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB36_349
.LBB36_370:                             # %if.else288.i
                                        #   in Loop: Header=BB36_340 Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rax
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	reg_mentioned_p
	xorq	%rdx, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB36_371
	jmp	.LBB36_373
.LBB36_349:                             # %if.then281.i
                                        #   in Loop: Header=BB36_340 Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rax
	movq	%rax, %rdi
	movabsq	$note_set_pseudo_multiple_uses, %rax
	movq	%rax, %rsi
	movq	152(%rsp), %rdx         # 8-byte Reload
	callq	note_stores
	movl	200(%r15), %eax
	xorq	%rdx, %rdx
	testq	%rax, %rax
	jne	.LBB36_373
.LBB36_371:                             # %for.inc299.i
                                        #   in Loop: Header=BB36_340 Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	24(%rax), %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	cmpq	%r13, %rax
	jne	.LBB36_340
.LBB36_372:
	movq	$1, %rdx
.LBB36_373:                             # %for.end303.i
	movq	%rdx, 152(%rsp)         # 8-byte Spill
	movq	224(%r15), %rax
	jmp	.LBB36_374
.LBB36_149:                             # %if.then240.i
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB36_203
# BB#150:                               # %if.then242.i
	movabsq	$.L.str67, %rdi
	movq	$39, %rsi
	jmp	.LBB36_100
.LBB36_353:                             # %if.then82.i
	movq	40(%rbx), %rax
	movzbl	(%rax), %edx
	cmpq	$54, %rdx
	jne	.LBB36_354
# BB#365:                               # %land.lhs.true86.i
	movq	8(%rax), %rax
	testq	%rax, %rax
	jle	.LBB36_354
# BB#366:                               # %land.lhs.true92.i
	cqto
	idivq	%rcx
	movq	%rdx, %rax
	testq	%rax, %rax
	je	.LBB36_367
.LBB36_354:                             # %for.cond120.preheader.i
	cmpq	$0, 168(%rsp)           # 8-byte Folded Reload
	je	.LBB36_512
# BB#355:
	movq	168(%rsp), %r14         # 8-byte Reload
.LBB36_356:                             # %for.body122.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %eax
	cmpq	$36, %rax
	je	.LBB36_512
# BB#357:                               # %for.body122.i
                                        #   in Loop: Header=BB36_356 Depth=1
	cmpq	$33, %rax
	jne	.LBB36_364
# BB#358:                               # %if.end132.i
                                        #   in Loop: Header=BB36_356 Depth=1
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	%r14, %rsi
	callq	get_condition_for_loop
	testq	%rax, %rax
	je	.LBB36_364
# BB#359:                               # %land.lhs.true135.i
                                        #   in Loop: Header=BB36_356 Depth=1
	movq	8(%rbx), %rdx
	movq	8(%rax), %rcx
	movq	32(%rdx), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB36_364
# BB#360:                               # %land.lhs.true142.i
                                        #   in Loop: Header=BB36_356 Depth=1
	movzbl	(%rax), %edx
	cmpq	$107, %rdx
	jne	.LBB36_364
# BB#361:                               # %land.lhs.true146.i
                                        #   in Loop: Header=BB36_356 Depth=1
	movq	16(%rax), %rax
	movq	const_int_rtx+512(%rip), %rdx
	cmpq	%rdx, %rax
	jne	.LBB36_364
# BB#362:                               # %land.lhs.true151.i
                                        #   in Loop: Header=BB36_356 Depth=1
	movq	%rcx, %rdi
	movq	%r14, %rsi
	movq	168(%rsp), %rdx         # 8-byte Reload
	callq	reg_set_between_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB36_364
# BB#363:                               # %land.lhs.true156.i
                                        #   in Loop: Header=BB36_356 Depth=1
	movq	8(%rbx), %rax
	movq	72(%rax), %rax
	movq	8(%rax), %rax
	cmpq	$-1, %rax
	je	.LBB36_367
.LBB36_364:                             # %for.inc182.i
                                        #   in Loop: Header=BB36_356 Depth=1
	movq	16(%r14), %r14
	testq	%r14, %r14
	jne	.LBB36_356
	jmp	.LBB36_512
.LBB36_377:                             # %for.inc318.i
                                        #   in Loop: Header=BB36_374 Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	64(%rax), %rax
.LBB36_374:                             # %for.end303.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, 120(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	je	.LBB36_378
# BB#375:                               # %for.body307.i
                                        #   in Loop: Header=BB36_374 Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	80(%rax), %rax
	testq	%rax, %rax
	je	.LBB36_377
# BB#376:                               # %land.lhs.true309.i
                                        #   in Loop: Header=BB36_374 Depth=1
	movq	16(%rax), %rax
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	reg_mentioned_p
	movq	152(%rsp), %rdx         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB36_377
	jmp	.LBB36_379
.LBB36_367:                             # %if.then103.i
	movq	$8, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB36_369
# BB#368:                               # %if.then106.i
	movq	8(%rbx), %rax
	movq	32(%rax), %rax
	movq	56(%r12), %rcx
	movq	$3, %rdi
	movq	$8, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%r12)
.LBB36_369:                             # %if.end116.i
	movzbl	88(%rbx), %eax
	orq	$4, %rax
	movb	%al, 88(%rbx)
	jmp	.LBB36_512
.LBB36_378:                             # %if.end321.i
	movq	$1, %rax
	testq	%rdx, %rdx
	movq	$1, %r12
	jne	.LBB36_393
.LBB36_379:                             # %if.else324.i
	movslq	152(%r15), %rax
	cmpq	$1, %rax
	jg	.LBB36_512
# BB#380:                               # %for.cond328.preheader.i
	movq	168(%rsp), %rcx         # 8-byte Reload
	cmpq	%r13, %rcx
	je	.LBB36_387
# BB#381:
	movq	$0, 152(%rsp)           # 8-byte Folded Spill
	movq	%rcx, %r12
.LBB36_382:                             # %for.body331.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB36_384
# BB#383:                               # %if.then339.i578
                                        #   in Loop: Header=BB36_382 Depth=1
	movq	32(%r12), %rsi
	movq	176(%rsp), %rdi         # 8-byte Reload
	callq	count_nonfixed_reads
	addq	%rax, 152(%rsp)         # 8-byte Folded Spill
.LBB36_384:                             # %for.inc345.i
                                        #   in Loop: Header=BB36_382 Depth=1
	movq	24(%r12), %r12
	cmpq	%r13, %r12
	jne	.LBB36_382
# BB#385:                               # %if.end379.i
	movl	152(%r15), %eax
	cmpq	$1, %rax
	je	.LBB36_386
# BB#391:                               # %if.end379.i
	movabsq	$1, %rax
	jmp	.LBB36_392
.LBB36_387:                             # %if.end379.thread.i
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	je	.LBB36_388
# BB#389:                               # %if.end379.thread.i
	movabsq	$1, %rax
	jmp	.LBB36_390
.LBB36_386:
	movabsq	$0, %rax
.LBB36_392:                             # %if.end379.i
	movq	$32, %rcx
	movq	152(%rsp), %rsi         # 8-byte Reload
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdx, %rdx
	cmpq	$1, %rsi
	movabsq	$0, %r12
	jg	.LBB36_512
	jmp	.LBB36_393
.LBB36_388:
	movabsq	$0, %rax
.LBB36_390:                             # %if.end379.thread.i
	xorq	%rdx, %rdx
	xorq	%r12, %r12
.LBB36_393:                             # %land.lhs.true382.i
	movl	8(%r15), %ecx
	testq	%rcx, %rcx
	jne	.LBB36_405
# BB#394:                               # %land.lhs.true384.i
	movl	12(%r15), %ecx
	testq	%rcx, %rcx
	jne	.LBB36_405
# BB#395:                               # %land.lhs.true384.i
	testq	%rax, %rax
	je	.LBB36_405
# BB#396:                               # %land.lhs.true388.i
	movq	%rdx, %r10
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	160(%rax), %rcx
	movl	16(%rbx), %eax
	movl	4(%rbx), %edi
	movq	168(%rcx), %r8
	movl	152(%r15), %esi
	xorq	%rdx, %rdx
	testq	%r8, %r8
	je	.LBB36_399
# BB#397:
	movq	$3, %rcx
.LBB36_398:                             # %for.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	44(%r8), %r9d
	shrq	%cl, %r9
	movq	72(%r8), %r8
	notq	%r9
	andq	$1, %r9
	addq	%r9, %rdx
	testq	%r8, %r8
	jne	.LBB36_398
.LBB36_399:                             # %num_unmoved_movables.exit.i
	movq	160(%rsp), %rcx         # 8-byte Reload
	addq	%rcx, %rax
	addq	%rdi, %rax
	addq	%rsi, %rax
	addq	%rdx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	andq	%rdx, %rax
	movq	136(%rsp), %rcx         # 8-byte Reload
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	movq	%r10, %rdx
	jne	.LBB36_405
# BB#400:                               # %land.lhs.true399.i
	movq	224(%r15), %rax
	cmpq	%rax, %rbx
	je	.LBB36_401
.LBB36_405:                             # %lor.lhs.false407.i
	testq	%r12, %r12
	movq	$1, %r12
	je	.LBB36_512
	jmp	.LBB36_406
.LBB36_401:                             # %land.lhs.true403.i
	movq	%rdx, %rcx
	cmpq	$0, 64(%rbx)
	jne	.LBB36_402
# BB#403:                               # %land.lhs.true403.i
	movabsq	$1, %rax
	jmp	.LBB36_404
.LBB36_402:
	movabsq	$0, %rax
.LBB36_404:                             # %land.lhs.true403.i
	orq	%r12, %rax
	movq	%rcx, %rdx
	je	.LBB36_512
.LBB36_406:                             # %if.then409.i
	movq	%rdx, 152(%rsp)         # 8-byte Spill
	movq	loop_dump_stream(%rip), %rax
	testq	%rax, %rax
	je	.LBB36_408
# BB#407:                               # %if.then411.i
	movabsq	$.L.str57, %rcx
	movq	$17, %rsi
	movq	$1, %rdx
	movq	%rcx, %rdi
	movq	%rax, %rcx
	callq	fwrite
.LBB36_408:                             # %land.lhs.true415.i
	movzbl	(%r14), %eax
	cmpq	$106, %rax
	jne	.LBB36_409
# BB#410:                               # %land.lhs.true415.i
	movabsq	$1, %rcx
	jmp	.LBB36_411
.LBB36_409:
	movabsq	$0, %rcx
.LBB36_411:                             # %land.lhs.true415.i
	cmpq	$107, %rax
	je	.LBB36_413
# BB#412:                               # %land.lhs.true415.i
	andq	%r12, %rcx
	je	.LBB36_512
.LBB36_413:                             # %if.then427.i
	movq	8(%rbx), %rax
	movq	16(%r14), %rdx
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	72(%rax), %rcx
	movzbl	2(%rdx), %eax
	movq	8(%rcx), %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB36_415
# BB#414:                               # %if.then444.i
	movq	8(%r14), %rax
	movzbl	2(%rax), %eax
.LBB36_415:                             # %if.end461.i
	movzbl	mode_bitsize(%rax,%rax), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	leaq	(%rax,%rcx), %rcx
	movq	$1, %rdx
	shlq	%cl, %rdx
	movq	%rdx, 112(%rsp)         # 8-byte Spill
	cmpq	$64, %rax
	jbe	.LBB36_417
# BB#416:
	movabsq	$-9223372036854775808, %rax # imm = 0x8000000000000000
	movq	%rax, 112(%rsp)         # 8-byte Spill
.LBB36_417:                             # %if.end461.i
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	loop_invariant_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	152(%rsp), %rax         # 8-byte Reload
	je	.LBB36_512
# BB#418:                               # %if.end470.i
	movq	%rax, %rdi
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB36_419
# BB#427:                               # %if.end479.i
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rsi
	movq	40(%rbx), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	testq	%r12, %r12
	je	.LBB36_428
# BB#429:                               # %land.lhs.true487.i
	movq	56(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB36_430
# BB#431:                               # %if.then492.i
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	64(%rsp), %r8           # 8-byte Reload
	leaq	-1(%r8,%rsi), %r9
	subq	%rax, %r9
	xorq	%rdx, %rdx
	movq	%r9, %rax
	divq	%r8
	movq	%rdx, %rax
	movq	const_int_rtx+512(%rip), %rdx
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	subq	%rax, %r9
	movq	%r9, 48(%rsp)           # 8-byte Spill
	jmp	.LBB36_432
.LBB36_419:                             # %if.end502.i.thread
	movq	40(%rbx), %rsi
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	movq	const_int_rtx+512(%rip), %rax
	leaq	40(%rbx), %rdx
	movq	%rdx, 96(%rsp)          # 8-byte Spill
	cmpq	%rax, %rsi
	jne	.LBB36_420
# BB#421:                               # %if.end502.i.thread
	movabsq	$1, %rax
	jmp	.LBB36_422
.LBB36_428:
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	jmp	.LBB36_432
.LBB36_430:
	movq	%rsi, 48(%rsp)          # 8-byte Spill
.LBB36_432:                             # %if.end502.i
	movq	const_int_rtx+512(%rip), %rdx
	leaq	40(%rbx), %rax
	movq	%rax, 96(%rsp)          # 8-byte Spill
	xorq	%rax, %rax
	cmpq	%rdx, 56(%rsp)          # 8-byte Folded Reload
	jne	.LBB36_423
# BB#433:                               # %land.lhs.true510.i
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	64(%rsp), %rdx          # 8-byte Reload
	subq	%rdx, %rax
	xorq	%rax, %rsi
	movq	112(%rsp), %rax         # 8-byte Reload
	testq	%rax, %rsi
	movq	$1, %rax
	jne	.LBB36_423
# BB#434:                               # %if.end531.thread.i
	movzbl	(%r14), %eax
	cmpq	$106, %rax
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, 32(%rsp)          # 8-byte Spill
	je	.LBB36_436
# BB#435:                               # %if.end531.thread.i
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
.LBB36_436:                             # %if.end531.thread.i
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, %rsi
	movq	32(%rsp), %rdx          # 8-byte Reload
	subq	%rdx, %rsi
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	$1, 112(%rsp)           # 8-byte Folded Spill
	movq	$104, 88(%rsp)          # 8-byte Folded Spill
	movq	%rax, 8(%rsp)           # 8-byte Spill
	jmp	.LBB36_445
.LBB36_420:
	movabsq	$0, %rax
.LBB36_422:                             # %if.end502.i.thread
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
.LBB36_423:                             # %if.else517.i
	cmpq	$1, 64(%rsp)            # 8-byte Folded Reload
	movq	%rdi, %rdx
	jne	.LBB36_512
# BB#424:                               # %land.lhs.true520.i
	movq	%rdx, %rsi
	movq	176(%rsp), %rdx         # 8-byte Reload
	cmpq	$0, 168(%rdx)
	movq	%rsi, %rdx
	je	.LBB36_512
# BB#425:                               # %land.lhs.true522.i
	movq	%rdx, %rsi
	movl	4(%rbx), %edx
	testq	%rdx, %rdx
	jne	.LBB36_426
# BB#437:                               # %land.lhs.true522.i
	movabsq	$1, %rdx
	jmp	.LBB36_438
.LBB36_426:
	movabsq	$0, %rdx
.LBB36_438:                             # %land.lhs.true522.i
	orq	%r12, %rdx
	cmpq	$1, %rdx
	movq	%rsi, %rdx
	jne	.LBB36_512
# BB#439:                               # %if.end531.i582
	movq	%rdx, %rdi
	movzbl	(%r14), %edx
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	cmpq	$106, %rdx
	jne	.LBB36_440
# BB#441:                               # %if.end531.i582
	movabsq	$1, %rdx
	jmp	.LBB36_442
.LBB36_440:
	movabsq	$0, %rdx
.LBB36_442:                             # %if.end531.i582
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	$-1, 16(%rsp)           # 8-byte Folded Spill
	je	.LBB36_444
# BB#443:                               # %if.end531.i582
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
.LBB36_444:                             # %if.end531.i582
	movq	$102, 88(%rsp)          # 8-byte Folded Spill
	testq	%rax, %rax
	movabsq	$0, %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	je	.LBB36_447
.LBB36_445:                             # %land.lhs.true541.i
	cmpq	$54, %rcx
	jne	.LBB36_447
# BB#446:                               # %if.then546.i
	xorq	%rdx, %rdx
	movq	48(%rsp), %rax          # 8-byte Reload
	divq	64(%rsp)                # 8-byte Folded Reload
	movq	%rdx, %rax
	testq	%rax, %rax
	jne	.LBB36_512
	jmp	.LBB36_450
.LBB36_447:                             # %if.else552.i
	testq	%rdi, %rdi
	je	.LBB36_512
# BB#448:                               # %if.else552.i
	cmpq	$1, 64(%rsp)            # 8-byte Folded Reload
	jne	.LBB36_512
# BB#449:                               # %if.end559.i
	cmpq	$54, %rcx
	jne	.LBB36_451
.LBB36_450:                             # %land.lhs.true564.i
	movq	56(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB36_451
# BB#452:                               # %if.then569.i
	xorq	%rdi, %rdi
	movq	48(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rsi
	callq	gen_rtx_CONST_INT
	movq	96(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, 72(%rsp)          # 8-byte Spill
	addq	%r12, %rcx
	xorq	%rdi, %rdi
	movq	%rcx, %rsi
	callq	gen_rtx_CONST_INT
	jmp	.LBB36_453
.LBB36_451:
	movq	72(%rsp), %rax          # 8-byte Reload
.LBB36_453:                             # %if.end577.i
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, (%rax)
	movq	8(%rbx), %rax
	movq	32(%rax), %r12
	movq	16(%r13), %rax
	movq	%rax, %rdi
	callq	condjump_label
	movq	8(%rbx), %rcx
	movq	72(%rcx), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, 152(%rsp)         # 8-byte Spill
	negq	%rcx
	xorq	%rdi, %rdi
	movq	%rcx, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, 24(%rsp)          # 8-byte Spill
	cmpq	%rcx, 56(%rsp)          # 8-byte Folded Reload
	jne	.LBB36_458
# BB#454:                               # %land.lhs.true594.i
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB36_458
# BB#455:                               # %if.then599.i
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	16(%rsp), %rcx          # 8-byte Reload
	subq	%rcx, %rax
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	gen_move_insn
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	192(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB36_456
# BB#457:                               # %loop_insn_hoist.exit.i
	movq	%rax, %rdi
	jmp	.LBB36_469
.LBB36_458:                             # %if.else604.i
	movq	56(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB36_464
# BB#459:                               # %if.then609.i
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movzbl	2(%r12), %ecx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, %rax
	negq	%rax
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	gen_add3_insn
	movq	%rax, 32(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB36_512
# BB#460:                               # %if.end623.i
	movq	$75, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	gen_rtx_fmt_ee
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	192(%rcx), %rcx
	movq	%rax, 56(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	je	.LBB36_461
# BB#462:                               # %loop_insn_hoist.exit14.i
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	callq	emit_insn_before
	movzbl	(%r14), %eax
	cmpq	$106, %rax
	jne	.LBB36_470
# BB#463:                               # %if.then630.i
	xorq	%rdi, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 40(%rsp)          # 8-byte Spill
	jmp	.LBB36_470
.LBB36_456:                             # %tailrecurse.us.i.i
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB36_456
.LBB36_464:                             # %if.else634.i
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, 8(%rsp)           # 8-byte Folded Reload
	jne	.LBB36_512
# BB#465:                               # %if.then636.i
	movq	%r12, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	56(%rsp), %rdx          # 8-byte Reload
	callq	gen_sub3_insn
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB36_512
# BB#466:                               # %if.end645.i
	movq	$16, %rcx
	movl	(%r12), %eax
	shrq	%cl, %rax
	andq	$255, %rax
	movq	$76, %rdi
	movq	%rax, %rsi
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	gen_rtx_fmt_ee
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	192(%rcx), %rcx
	movq	%rax, 56(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	je	.LBB36_467
# BB#468:                               # %loop_insn_hoist.exit19.i
	movq	%r14, %rdi
.LBB36_469:                             # %if.end651.i
	movq	%rcx, %rsi
	callq	emit_insn_before
.LBB36_470:                             # %if.end651.i
	callq	start_sequence
	movq	%r12, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	expand_inc
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
	movq	8(%rbx), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB36_473
# BB#471:                               # %if.then.i.i588
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	192(%rax), %rax
	testq	%rax, %rax
	je	.LBB36_472
.LBB36_473:                             # %if.end.i23.i
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_insn_before
	movq	%rax, %r14
	movq	8(%rbx), %rax
	movq	(%rax), %rax
	movq	%rax, %rdi
	callq	delete_insn
	movq	8(%rbx), %rax
	movq	%r14, (%rax)
	movq	96(%rsp), %rcx          # 8-byte Reload
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, (%rcx)
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 72(%rax)
	movq	%r12, 32(%r15)
	movq	%r12, 56(%r15)
	movq	const_int_rtx+512(%rip), %rax
	movq	%rax, 48(%r15)
	movq	const_int_rtx+512(%rip), %rax
	movq	%rax, 64(%r15)
	movq	const_int_rtx+512(%rip), %rax
	movq	%rax, 40(%r15)
	movq	88(%rsp), %rax          # 8-byte Reload
	movl	%eax, 88(%r15)
	movq	%rcx, 80(%r15)
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movl	32(%rax), %ecx
	incq	%rcx
	movl	%ecx, 32(%rax)
	movl	(%rbx), %eax
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movq	5816(%rsp), %rcx
	movl	4(%rax), %edx
	movl	8(%rcx), %ecx
	cmpq	%rcx, %rdx
	jne	.LBB36_476
# BB#474:                               # %lor.lhs.false681.i
	movq	72(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB36_476
# BB#475:                               # %lor.lhs.false683.i
	movl	(%rax), %eax
	movl	8(%rcx), %ecx
	cmpq	%rcx, %rax
	je	.LBB36_480
.LBB36_476:                             # %if.then695.i
	movq	%r12, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	gen_move_insn
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	224(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB36_479
# BB#477:                               # %if.then.i.i.i
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	192(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB36_478
.LBB36_479:                             # %if.end.i.i.i592
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	emit_insn_before
.LBB36_480:                             # %if.end698.i
	movq	16(%r13), %rax
	movq	%rax, %rdi
	callq	delete_related_insns
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	160(%rsp), %rcx         # 8-byte Reload
	andq	%rax, %rcx
	cmpq	$2, %rcx
	jne	.LBB36_482
# BB#481:                               # %if.then705.i
	movq	5816(%rsp), %rax
	movq	%rax, %rdi
	callq	delete_related_insns
.LBB36_482:                             # %if.end707.i
	callq	start_sequence
	movq	const_int_rtx+512(%rip), %rax
	movq	152(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx), %rcx
	movzbl	2(%r12), %r8d
	movq	%rcx, (%rsp)
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	xorq	%r14, %r14
	movq	%r12, %rdi
	movq	%rax, %rsi
	movq	88(%rsp), %rdx          # 8-byte Reload
	callq	emit_cmp_and_jump_insns
	callq	gen_sequence
	movq	%rax, 160(%rsp)         # 8-byte Spill
	callq	end_sequence
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	%r13, %rsi
	callq	emit_jump_insn_before
	movq	16(%r13), %rax
	testq	%rax, %rax
	movabsq	$0, %rcx
	je	.LBB36_487
# BB#483:
	xorq	%r14, %r14
.LBB36_484:                             # %land.rhs721.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	je	.LBB36_486
# BB#485:                               # %for.inc728.i
                                        #   in Loop: Header=BB36_484 Depth=1
	movq	16(%rax), %rax
	testq	%rax, %rax
	movabsq	$0, %rcx
	jne	.LBB36_484
	jmp	.LBB36_487
.LBB36_461:                             # %tailrecurse.us.i12.i
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB36_461
.LBB36_467:                             # %tailrecurse.us.i17.i
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB36_467
.LBB36_472:                             # %tailrecurse.us.i29.i
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB36_472
.LBB36_478:                             # %tailrecurse.us.i.i.i
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB36_478
.LBB36_486:                             # %if.then734.i
	movq	152(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx), %rcx
	movq	%rcx, 64(%rax)
	movq	$1, %r14
	movq	%rax, %rcx
.LBB36_487:                             # %if.end741.i
	cmpq	$0, 112(%rsp)           # 8-byte Folded Reload
	je	.LBB36_491
# BB#488:                               # %if.then743.i
	testq	%r14, %r14
	je	.LBB36_490
# BB#489:                               # %if.then745.i
	movq	%rcx, %r14
	movq	56(%r14), %rax
	movq	$3, %rdi
	movq	$8, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%r14)
.LBB36_490:                             # %if.end753.i
	movzbl	88(%rbx), %eax
	orq	$4, %rax
	movb	%al, 88(%rbx)
.LBB36_491:                             # %for.cond759.preheader.i
	cmpq	%r13, 168(%rsp)         # 8-byte Folded Reload
	je	.LBB36_508
.LBB36_492:                             # %for.body762.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB36_502 Depth 2
                                        #       Child Loop BB36_503 Depth 3
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB36_507
# BB#493:                               # %cond.true779.i
                                        #   in Loop: Header=BB36_492 Depth=1
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB36_495
# BB#494:                               # %cond.false791.i
                                        #   in Loop: Header=BB36_492 Depth=1
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	single_set_2
.LBB36_495:                             # %cond.end799.i
                                        #   in Loop: Header=BB36_492 Depth=1
	testq	%rax, %rax
	je	.LBB36_500
# BB#496:                               # %lor.lhs.false802.i
                                        #   in Loop: Header=BB36_492 Depth=1
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB36_500
# BB#497:                               # %lor.lhs.false810.i
                                        #   in Loop: Header=BB36_492 Depth=1
	movl	8(%rax), %eax
	movl	216(%r15), %ecx
	cmpq	%rcx, %rax
	jae	.LBB36_500
# BB#498:                               # %lor.lhs.false821.i
                                        #   in Loop: Header=BB36_492 Depth=1
	movq	208(%r15), %rdx
	movq	$4, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movl	(%rdx,%rsi), %ecx
	cmpq	$3, %rcx
	jne	.LBB36_500
# BB#499:                               # %lor.lhs.false833.i
                                        #   in Loop: Header=BB36_492 Depth=1
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	8(%rdx,%rax), %rax
	movq	8(%rbx), %rcx
	movq	16(%rax), %rax
	movq	16(%rcx), %rcx
	cmpq	%rcx, %rax
	je	.LBB36_507
.LBB36_500:                             # %if.then847.i
                                        #   in Loop: Header=BB36_492 Depth=1
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	56(%rax), %rax
	testq	%rax, %rax
	je	.LBB36_507
# BB#501:                               #   in Loop: Header=BB36_492 Depth=1
	movq	168(%rsp), %rcx         # 8-byte Reload
	leaq	56(%rcx), %r14
.LBB36_502:                             # %for.body853.lr.ph.i
                                        #   Parent Loop BB36_492 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB36_503 Depth 3
	movq	%rax, %rcx
.LBB36_503:                             # %for.body853.i
                                        #   Parent Loop BB36_492 Depth=1
                                        #     Parent Loop BB36_502 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rcx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB36_506
# BB#504:                               # %land.lhs.true859.i
                                        #   in Loop: Header=BB36_503 Depth=3
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	(%rbx), %edx
	movq	80(%rax), %rax
	movq	(%rax,%rdx,8), %rax
	movq	8(%rcx), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	reg_mentioned_p
	movq	(%r14), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB36_506
# BB#505:                               # %if.then870.i
                                        #   in Loop: Header=BB36_503 Depth=3
	movq	16(%rcx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, (%r14)
	jne	.LBB36_503
	jmp	.LBB36_507
.LBB36_506:                             # %if.else874.i
                                        #   in Loop: Header=BB36_502 Depth=2
	movq	16(%rcx), %rax
	addq	$16, %rcx
	testq	%rax, %rax
	movq	%rcx, %r14
	jne	.LBB36_502
.LBB36_507:                             # %for.inc882.i
                                        #   in Loop: Header=BB36_492 Depth=1
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	24(%rax), %rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
	cmpq	%r13, %rax
	jne	.LBB36_492
.LBB36_508:                             # %for.end886.i
	movzbl	88(%rbx), %eax
	orq	$8, %rax
	movb	%al, 88(%rbx)
	movq	loop_dump_stream(%rip), %rax
	testq	%rax, %rax
	je	.LBB36_512
# BB#509:                               # %if.then891.i
	movabsq	$.L.str58, %rcx
	movq	$13, %rsi
	movq	$1, %rdx
	movq	%rcx, %rdi
	movq	%rax, %rcx
	callq	fwrite
	movzbl	88(%rbx), %ecx
	movq	loop_dump_stream(%rip), %rax
	testq	$4, %rcx
	je	.LBB36_511
# BB#510:                               # %if.then899.i
	movabsq	$.L.str59, %rcx
	movq	$22, %rsi
	movq	$1, %rdx
	movq	%rcx, %rdi
	movq	%rax, %rcx
	callq	fwrite
	jmp	.LBB36_512
.LBB36_511:                             # %if.else901.i
	movq	$10, %rdi
	movq	%rax, %rsi
	callq	fputc
.LBB36_512:                             # %check_dbra_loop.exit
	movq	104(%rsp), %rbx         # 8-byte Reload
	movslq	216(%rbx), %rdi
	movq	%rdi, 88(%rsp)          # 8-byte Spill
	movq	$8, %rsi
	callq	xcalloc
	movq	224(%rbx), %r12
	movq	%rax, 112(%rsp)         # 8-byte Spill
	testq	%r12, %r12
	movq	136(%rsp), %rbx         # 8-byte Reload
	je	.LBB36_522
# BB#513:
	movl	n_non_fixed_regs(%rip), %eax
	movq	%rax, %rcx
	addq	$3, %rcx
	movq	104(%rsp), %rax         # 8-byte Reload
	leaq	184(%rax), %rax
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	128(%rsp), %rax         # 8-byte Reload
	imulq	%rcx, %rax
	movq	%rax, 128(%rsp)         # 8-byte Spill
	jmp	.LBB36_514
.LBB36_574:                             # %cond.false42.i
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	152(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rdi
	movabsq	$0, %r13
	jbe	.LBB36_578
.LBB36_575:                             # %land.lhs.true45.i
                                        #   in Loop: Header=BB36_514 Depth=1
	testq	%r8, %r8
	movabsq	$0, %rcx
	js	.LBB36_577
# BB#576:                               # %land.lhs.true45.i
                                        #   in Loop: Header=BB36_514 Depth=1
	movabsq	$1, %rcx
.LBB36_577:                             # %land.lhs.true45.i
                                        #   in Loop: Header=BB36_514 Depth=1
	cmpq	%rdi, %rdx
	sbbq	%r13, %r13
	andq	%rcx, %r13
	notq	%r13
	andq	$1, %r13
.LBB36_578:                             # %if.end52.i642
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	%rax, %rcx
	notq	%rcx
	cmpq	%rcx, 152(%rsp)         # 8-byte Folded Reload
	jl	.LBB36_585
# BB#579:                               # %land.lhs.true57.i643
                                        #   in Loop: Header=BB36_514 Depth=1
	testq	%r8, %r8
	jns	.LBB36_582
# BB#580:                               # %cond.true59.i
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	152(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %rdi
	jge	.LBB36_585
# BB#581:                               # %cond.true59.i
                                        #   in Loop: Header=BB36_514 Depth=1
	cmpq	%rcx, %rdi
	jge	.LBB36_584
	jmp	.LBB36_585
.LBB36_582:                             # %cond.false62.i
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	152(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rdi
	jle	.LBB36_585
# BB#583:                               # %cond.false62.i
                                        #   in Loop: Header=BB36_514 Depth=1
	cmpq	%rax, %rdi
	jg	.LBB36_585
.LBB36_584:                             # %if.then75.i
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	$1, 160(%rsp)           # 8-byte Folded Spill
	movq	$1, %r15
	jmp	.LBB36_585
	.align	16, 0x90
.LBB36_514:                             # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB36_587 Depth 2
                                        #     Child Loop BB36_612 Depth 2
                                        #       Child Loop BB36_617 Depth 3
                                        #       Child Loop BB36_631 Depth 3
                                        #         Child Loop BB36_645 Depth 4
                                        #     Child Loop BB36_659 Depth 2
                                        #       Child Loop BB36_665 Depth 3
                                        #     Child Loop BB36_675 Depth 2
                                        #       Child Loop BB36_680 Depth 3
                                        #     Child Loop BB36_696 Depth 2
                                        #     Child Loop BB36_745 Depth 2
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	200(%rax), %rax
	movslq	8(%rax), %rsi
	movslq	max_uid_for_loop(%rip), %rax
	cmpq	%rax, %rsi
	jge	.LBB36_555
# BB#515:                               # %cond.true.i605
                                        #   in Loop: Header=BB36_514 Depth=1
	movl	(%r12), %ecx
	movq	reg_n_info(%rip), %rdx
	movq	32(%rdx,%rcx,8), %rdx
	movq	uid_luid(%rip), %rcx
	movslq	4(%rdx), %rdi
	movslq	(%rcx,%rdi,4), %rdi
	movslq	(%rcx,%rsi,4), %rsi
	cmpq	%rsi, %rdi
	jge	.LBB36_520
# BB#516:                               # %land.lhs.true.i609
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	72(%r12), %rsi
	testq	%rsi, %rsi
	je	.LBB36_520
# BB#517:                               # %land.lhs.true11.i
                                        #   in Loop: Header=BB36_514 Depth=1
	movslq	8(%rsi), %rsi
	cmpq	%rax, %rsi
	jge	.LBB36_520
# BB#518:                               # %cond.true30.i
                                        #   in Loop: Header=BB36_514 Depth=1
	movslq	(%rdx), %rax
	movslq	(%rcx,%rax,4), %rax
	movslq	(%rcx,%rsi,4), %rcx
	cmpq	%rcx, %rax
	jl	.LBB36_520
# BB#519:                               # %land.lhs.true41.i
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	8(%r12), %rax
	movq	80(%r12), %rcx
	movq	32(%rax), %rdi
	movq	16(%rcx), %rsi
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB36_521
	.align	16, 0x90
.LBB36_520:                             # %lor.lhs.false.i617
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	%r12, %rsi
	callq	final_biv_value
	testq	%rax, %rax
	movq	%rax, 56(%r12)
	jne	.LBB36_521
# BB#557:                               # %if.end.i619
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	movq	$16, %rbx
	je	.LBB36_559
# BB#558:                               # %if.then49.i
                                        #   in Loop: Header=BB36_514 Depth=1
	movl	(%r12), %edx
	xorq	%rax, %rax
	movabsq	$.L.str55, %rsi
	callq	fprintf
	movl	(%r12), %eax
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movq	loop_dump_stream(%rip), %rdi
	movl	4(%rax), %ecx
	movl	(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str56, %rsi
	callq	fprintf
	jmp	.LBB36_559
	.align	16, 0x90
.LBB36_521:                             # %if.then.i618
                                        #   in Loop: Header=BB36_514 Depth=1
	xorq	%rdx, %rdx
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	%r12, %rsi
	movq	128(%rsp), %rcx         # 8-byte Reload
	movq	%rbx, %r8
	callq	maybe_eliminate_biv
	andq	$1, %rax
	leaq	16(%rax,%rax), %rbx
.LBB36_559:                             # %loop_biv_eliminable_p.exit
                                        #   in Loop: Header=BB36_514 Depth=1
	movzbl	88(%r12), %eax
	andq	$237, %rax
	orq	%rbx, %rax
	movb	%al, 88(%r12)
	movq	global_rtl(%rip), %rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	$0, 152(%rsp)           # 8-byte Folded Spill
	movq	104(%rsp), %rax         # 8-byte Reload
	cmpq	$0, 96(%rax)
	je	.LBB36_560
# BB#561:                               # %land.lhs.true.i629
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	40(%r12), %rax
	movq	$0, 152(%rsp)           # 8-byte Folded Spill
	testq	%rax, %rax
	je	.LBB36_560
# BB#562:                               # %land.lhs.true1.i
                                        #   in Loop: Header=BB36_514 Depth=1
	movzbl	(%rax), %eax
	movq	$0, 152(%rsp)           # 8-byte Folded Spill
	cmpq	$54, %rax
	movabsq	$0, %rdi
	movabsq	$0, %rax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movabsq	$0, %r15
	movabsq	$0, %r13
	jne	.LBB36_585
# BB#563:                               # %land.lhs.true6.i
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	8(%r12), %rax
	movq	16(%rax), %rax
	movzbl	2(%rax), %ebx
	movq	%r12, %rdi
	callq	biv_total_increment
	testq	%rax, %rax
	jne	.LBB36_565
# BB#564:                               #   in Loop: Header=BB36_514 Depth=1
	movq	$0, 168(%rsp)           # 8-byte Folded Spill
	movq	$0, 152(%rsp)           # 8-byte Folded Spill
	jmp	.LBB36_560
.LBB36_565:                             # %land.lhs.true8.i
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movzbl	(%rax), %eax
	movq	$0, 152(%rsp)           # 8-byte Folded Spill
	cmpq	$54, %rax
	jne	.LBB36_560
# BB#566:                               # %land.lhs.true12.i
                                        #   in Loop: Header=BB36_514 Depth=1
	movzbl	mode_bitsize(%rbx,%rbx), %eax
	movq	$0, 152(%rsp)           # 8-byte Folded Spill
	cmpq	$64, %rax
	ja	.LBB36_560
# BB#567:                               # %if.then.i639
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %r8
	movq	40(%r12), %rax
	movq	8(%rax), %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	$63, %rcx
	movq	%r8, %rax
	sarq	%cl, %rax
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	96(%rcx), %rdi
	leaq	(%r8,%rax), %rcx
	xorq	%rax, %rcx
	movq	%rcx, %rsi
	imulq	%rdi, %rsi
	xorq	%rdx, %rdx
	movq	%rsi, %rax
	divq	%rdi
	xorq	%rdi, %rdi
	cmpq	%rcx, %rax
	movq	$1, 160(%rsp)           # 8-byte Folded Spill
	jne	.LBB36_568
# BB#569:                               # %if.then30.i
                                        #   in Loop: Header=BB36_514 Depth=1
	testq	%r8, %r8
	jns	.LBB36_571
# BB#570:                               #   in Loop: Header=BB36_514 Depth=1
	negq	%rsi
.LBB36_571:                             # %if.then30.i
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	%rsi, %rdi
	movq	152(%rsp), %rsi         # 8-byte Reload
	addq	%rsi, %rdi
	movq	mode_mask_array(,%rbx,8), %rdx
	sbbq	%rbx, %rbx
	movq	$1, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	xorq	%r15, %r15
	testq	%rsi, %rsi
	movabsq	$0, %r13
	js	.LBB36_578
# BB#572:                               # %land.lhs.true37.i
                                        #   in Loop: Header=BB36_514 Depth=1
	testq	%r8, %r8
	jns	.LBB36_574
# BB#573:                               # %cond.true39.i
                                        #   in Loop: Header=BB36_514 Depth=1
	andq	$1, %rbx
	movabsq	$0, %r13
	jne	.LBB36_575
	jmp	.LBB36_578
	.align	16, 0x90
.LBB36_560:                             #   in Loop: Header=BB36_514 Depth=1
	xorq	%rdi, %rdi
	movq	$0, 160(%rsp)           # 8-byte Folded Spill
.LBB36_568:                             #   in Loop: Header=BB36_514 Depth=1
	xorq	%r15, %r15
	xorq	%r13, %r13
.LBB36_585:                             # %if.end78.i
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	%rdi, 120(%rsp)         # 8-byte Spill
	movq	24(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB36_611
# BB#586:                               # %for.body.lr.ph.i649
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	%r15, %r14
	andq	%r13, %r14
	.align	16, 0x90
.LBB36_587:                             # %for.body.i650
                                        #   Parent Loop BB36_514 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	120(%rbx), %rax
	testq	%rax, %rax
	je	.LBB36_610
# BB#588:                               # %if.then81.i
                                        #   in Loop: Header=BB36_587 Depth=2
	movl	(%rax), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$120, %rcx
	movq	%r15, %rdx
	je	.LBB36_596
# BB#589:                               # %if.then81.i
                                        #   in Loop: Header=BB36_587 Depth=2
	cmpq	$122, %rcx
	jne	.LBB36_590
# BB#592:                               # %sw.bb86.i
                                        #   in Loop: Header=BB36_587 Depth=2
	testq	%r14, %r14
	je	.LBB36_599
# BB#593:                               # %if.then90.i
                                        #   in Loop: Header=BB36_587 Depth=2
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$2040, %rax             # imm = 0x7F8
	movq	mode_mask_array(%rax), %rax
	movq	$1, %rcx
	shrq	%cl, %rax
	cmpq	%rax, 152(%rsp)         # 8-byte Folded Reload
	ja	.LBB36_599
# BB#594:                               # %if.then90.i
                                        #   in Loop: Header=BB36_587 Depth=2
	cmpq	%rax, 120(%rsp)         # 8-byte Folded Reload
	ja	.LBB36_599
# BB#595:                               # %sw.epilog.thread196.i
                                        #   in Loop: Header=BB36_587 Depth=2
	movq	loop_dump_stream(%rip), %rdi
	jmp	.LBB36_597
.LBB36_590:                             # %if.then81.i
                                        #   in Loop: Header=BB36_587 Depth=2
	cmpq	$121, %rcx
	movq	%r13, %rdx
	jne	.LBB36_591
.LBB36_596:                             # %sw.epilog.i
                                        #   in Loop: Header=BB36_587 Depth=2
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdx, %rdx
	je	.LBB36_600
.LBB36_597:                             # %if.then107.i
                                        #   in Loop: Header=BB36_587 Depth=2
	testq	%rdi, %rdi
	je	.LBB36_610
# BB#598:                               # %if.then109.i
                                        #   in Loop: Header=BB36_587 Depth=2
	movq	(%rbx), %rax
	movl	(%r12), %ecx
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str50, %rsi
	callq	fprintf
	jmp	.LBB36_610
.LBB36_599:                             # %sw.epilog.thread.i
                                        #   in Loop: Header=BB36_587 Depth=2
	movq	loop_dump_stream(%rip), %rdi
.LBB36_600:                             # %if.else.i657
                                        #   in Loop: Header=BB36_587 Depth=2
	testq	%rdi, %rdi
	je	.LBB36_609
# BB#601:                               # %if.then115.i
                                        #   in Loop: Header=BB36_587 Depth=2
	cmpq	$0, 160(%rsp)           # 8-byte Folded Reload
	jne	.LBB36_602
# BB#603:                               # %if.else118.i
                                        #   in Loop: Header=BB36_587 Depth=2
	movq	global_rtl(%rip), %rax
	movq	168(%rsp), %rcx         # 8-byte Reload
	cmpq	%rax, %rcx
	movq	%rcx, %rax
	jne	.LBB36_605
# BB#604:                               # %if.then121.i659
                                        #   in Loop: Header=BB36_587 Depth=2
	movq	%r12, %rdi
	callq	biv_total_increment
	movq	loop_dump_stream(%rip), %rdi
.LBB36_605:                             # %if.end123.i
                                        #   in Loop: Header=BB36_587 Depth=2
	movq	%rax, %rcx
	movq	const_int_rtx+520(%rip), %rax
	cmpq	%rax, %rcx
	movq	%rcx, %rax
	movabsq	$.L.str52, %rcx
	je	.LBB36_607
# BB#606:                               # %select.mid408
                                        #   in Loop: Header=BB36_587 Depth=2
	movabsq	$.L.str53, %rcx
.LBB36_607:                             # %select.end407
                                        #   in Loop: Header=BB36_587 Depth=2
	movq	%rax, 168(%rsp)         # 8-byte Spill
	jmp	.LBB36_608
.LBB36_602:                             #   in Loop: Header=BB36_587 Depth=2
	movabsq	$.L.str51, %rcx
.LBB36_608:                             # %if.end129.i
                                        #   in Loop: Header=BB36_587 Depth=2
	movq	(%rbx), %rax
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str54, %rsi
	callq	fprintf
.LBB36_609:                             # %if.end135.i
                                        #   in Loop: Header=BB36_587 Depth=2
	movzbl	100(%rbx), %eax
	orq	$4, %rax
	movw	%ax, 100(%rbx)
	movzbl	88(%r12), %eax
	andq	$239, %rax
	movb	%al, 88(%r12)
.LBB36_610:                             # %for.inc.i664
                                        #   in Loop: Header=BB36_587 Depth=2
	movq	128(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB36_587
.LBB36_611:                             # %check_ext_dependent_givs.exit
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	combine_givs
	movq	24(%r12), %r13
	testq	%r13, %r13
	je	.LBB36_750
	jmp	.LBB36_612
.LBB36_671:                             # %if.then76
                                        #   in Loop: Header=BB36_612 Depth=2
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB36_655
# BB#672:                               # %if.then78
                                        #   in Loop: Header=BB36_612 Depth=2
	movq	(%r13), %rax
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str40, %rsi
	callq	fprintf
	jmp	.LBB36_655
	.align	16, 0x90
.LBB36_612:                             # %for.body34
                                        #   Parent Loop BB36_514 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB36_617 Depth 3
                                        #       Child Loop BB36_631 Depth 3
                                        #         Child Loop BB36_645 Depth 4
	movzbl	100(%r13), %eax
	testq	$4, %rax
	jne	.LBB36_657
# BB#613:                               # %lor.lhs.false
                                        #   in Loop: Header=BB36_612 Depth=2
	cmpq	$0, 136(%r13)
	jne	.LBB36_657
# BB#614:                               # %if.end40
                                        #   in Loop: Header=BB36_612 Depth=2
	movl	48(%r13), %eax
	movl	80(%r13), %ecx
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	movq	%r12, %rcx
	movq	144(%rsp), %r12         # 8-byte Reload
	movb	%al, 2(%r12)
	movq	8(%rcx), %rax
	movq	%rcx, %r15
	movq	72(%rax), %rbx
	movq	64(%r13), %r14
	callq	start_sequence
	movzbl	2(%r12), %r8d
	movq	$1, %r9
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	%r12, %rcx
	callq	expand_mult_add
	cmpq	%r12, %rax
	je	.LBB36_616
# BB#615:                               # %if.then.i.i680
                                        #   in Loop: Header=BB36_612 Depth=2
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB36_616:                             # %if.end.i.i681
                                        #   in Loop: Header=BB36_612 Depth=2
	callq	get_last_insn
	movq	%rax, %rbx
	xorq	%r14, %r14
	jmp	.LBB36_617
	.align	16, 0x90
.LBB36_623:                             # %if.end29.i.i
                                        #   in Loop: Header=BB36_617 Depth=3
	movq	16(%rbx), %rbx
.LBB36_617:                             # %if.end.i.i681
                                        #   Parent Loop BB36_514 Depth=1
                                        #     Parent Loop BB36_612 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB36_624
# BB#618:                               # %while.body.i.i683
                                        #   in Loop: Header=BB36_617 Depth=3
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB36_623
# BB#619:                               # %cond.true.i.i
                                        #   in Loop: Header=BB36_617 Depth=3
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB36_621
# BB#620:                               # %cond.false.i.i
                                        #   in Loop: Header=BB36_617 Depth=3
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB36_621:                             # %cond.end21.i.i
                                        #   in Loop: Header=BB36_617 Depth=3
	testq	%rax, %rax
	je	.LBB36_623
# BB#622:                               # %if.then24.i.i
                                        #   in Loop: Header=BB36_617 Depth=3
	movq	16(%rax), %rdi
	movq	$47, %rsi
	callq	rtx_cost
	addq	%rax, %r14
	jmp	.LBB36_623
	.align	16, 0x90
.LBB36_624:                             # %iv_add_mult_cost.exit.i
                                        #   in Loop: Header=BB36_612 Depth=2
	callq	end_sequence
	movzbl	100(%r13), %eax
	testq	$1, %rax
	movq	%r15, %r12
	jne	.LBB36_628
# BB#625:                               # %land.lhs.true.i691
                                        #   in Loop: Header=BB36_612 Depth=2
	movzbl	88(%r12), %ecx
	testq	$2, %rcx
	jne	.LBB36_628
# BB#626:                               # %land.lhs.true8.i695
                                        #   in Loop: Header=BB36_612 Depth=2
	movq	32(%r13), %rcx
	movzbl	3(%rcx), %ecx
	testq	$8, %rcx
	je	.LBB36_628
# BB#627:                               # %if.then.i697
                                        #   in Loop: Header=BB36_612 Depth=2
	movzbl	copy_cost(%rip), %edx
	movq	$2, %rcx
	shlq	%cl, %rdx
	subq	%rdx, 168(%rsp)         # 8-byte Folded Spill
	.align	16, 0x90
.LBB36_628:                             # %loop_giv_reduce_benefit.exit
                                        #   in Loop: Header=BB36_612 Depth=2
	movl	flag_reduce_all_givs(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB36_630
# BB#629:                               # %land.lhs.true
                                        #   in Loop: Header=BB36_612 Depth=2
	movl	4(%r12), %ecx
	movl	104(%r13), %ebx
	imulq	%r14, %rcx
	movq	168(%rsp), %rdx         # 8-byte Reload
	subq	%rcx, %rdx
	movq	128(%rsp), %rcx         # 8-byte Reload
	imulq	%rcx, %rbx
	imulq	%rdx, %rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	136(%rsp), %rsi         # 8-byte Reload
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jge	.LBB36_630
# BB#652:                               # %land.lhs.true46
                                        #   in Loop: Header=BB36_612 Depth=2
	movzbl	88(%r12), %ecx
	testq	$8, %rcx
	jne	.LBB36_630
# BB#653:                               # %if.then52
                                        #   in Loop: Header=BB36_612 Depth=2
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB36_656
# BB#654:                               # %if.then54
                                        #   in Loop: Header=BB36_612 Depth=2
	movq	(%r13), %rax
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str39, %rsi
	movq	%rbx, %rcx
	movq	136(%rsp), %r8          # 8-byte Reload
	callq	fprintf
.LBB36_655:                             # %if.end59
                                        #   in Loop: Header=BB36_612 Depth=2
	movzbl	100(%r13), %eax
.LBB36_656:                             # %if.end59
                                        #   in Loop: Header=BB36_612 Depth=2
	orq	$4, %rax
	movw	%ax, 100(%r13)
	movzbl	88(%r12), %eax
	andq	$239, %rax
	movb	%al, 88(%r12)
	jmp	.LBB36_657
	.align	16, 0x90
.LBB36_630:                             # %for.cond67.preheader
                                        #   in Loop: Header=BB36_612 Depth=2
	movq	8(%r12), %rbx
	jmp	.LBB36_631
	.align	16, 0x90
.LBB36_651:                             # %for.inc
                                        #   in Loop: Header=BB36_631 Depth=3
	movq	128(%rbx), %rbx
.LBB36_631:                             # %for.cond67.preheader
                                        #   Parent Loop BB36_514 Depth=1
                                        #     Parent Loop BB36_612 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB36_645 Depth 4
	testq	%rbx, %rbx
	je	.LBB36_657
# BB#632:                               # %for.body69
                                        #   in Loop: Header=BB36_631 Depth=3
	movq	64(%rbx), %rax
	movq	const_int_rtx+520(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB36_651
# BB#633:                               # %land.lhs.true72
                                        #   in Loop: Header=BB36_631 Depth=3
	movq	72(%rbx), %r15
	movzbl	(%r15), %ecx
	movq	64(%r13), %rax
	cmpq	$54, %rcx
	movq	%rax, %r14
	je	.LBB36_635
# BB#634:                               # %land.lhs.true72
                                        #   in Loop: Header=BB36_631 Depth=3
	movq	%r15, %r14
.LBB36_635:                             # %land.lhs.true72
                                        #   in Loop: Header=BB36_631 Depth=3
	je	.LBB36_637
# BB#636:                               # %land.lhs.true72
                                        #   in Loop: Header=BB36_631 Depth=3
	movq	%rax, %r15
.LBB36_637:                             # %land.lhs.true72
                                        #   in Loop: Header=BB36_631 Depth=3
	movzbl	(%r14), %eax
	cmpq	$54, %rax
	je	.LBB36_651
# BB#638:                               # %if.end6.i
                                        #   in Loop: Header=BB36_631 Depth=3
	movzbl	(%r15), %eax
	cmpq	$54, %rax
	jne	.LBB36_671
# BB#639:                               # %if.end11.i711
                                        #   in Loop: Header=BB36_631 Depth=3
	callq	start_sequence
	movzbl	2(%r14), %edi
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	expand_mult
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
	movzbl	(%r14), %eax
	cmpq	$47, %rax
	je	.LBB36_670
# BB#640:                               # %if.end11.i711
                                        #   in Loop: Header=BB36_631 Depth=3
	cmpq	$39, %rax
	jne	.LBB36_641
# BB#669:                               # %land.lhs.true108.i
                                        #   in Loop: Header=BB36_631 Depth=3
	movq	8(%r14), %rax
	movq	8(%rax), %r14
	movzbl	(%r14), %eax
	cmpq	$47, %rax
	jne	.LBB36_651
.LBB36_670:                             # %product_cheap_p.exit
                                        #   in Loop: Header=BB36_631 Depth=3
	movq	16(%r14), %rax
	movzbl	(%rax), %eax
	cmpq	$78, %rax
	jne	.LBB36_651
	jmp	.LBB36_671
.LBB36_641:                             # %if.end11.i711
                                        #   in Loop: Header=BB36_631 Depth=3
	cmpq	$24, %rax
	jne	.LBB36_651
# BB#642:                               # %if.then18.i
                                        #   in Loop: Header=BB36_631 Depth=3
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.LBB36_651
# BB#643:                               # %if.else.i714
                                        #   in Loop: Header=BB36_631 Depth=3
	movslq	(%rax), %rdx
	cmpq	$3, %rdx
	jg	.LBB36_671
# BB#644:                               # %for.cond.preheader.i715
                                        #   in Loop: Header=BB36_631 Depth=3
	movq	$1, %rsi
	testq	%rdx, %rdx
	jle	.LBB36_651
	.align	16, 0x90
.LBB36_645:                             # %for.body.i718
                                        #   Parent Loop BB36_514 Depth=1
                                        #     Parent Loop BB36_612 Depth=2
                                        #       Parent Loop BB36_631 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	(%rax,%rsi,8), %rcx
	movzbl	(%rcx), %edi
	cmpq	$32, %rdi
	jne	.LBB36_671
# BB#646:                               # %lor.lhs.false.i720
                                        #   in Loop: Header=BB36_645 Depth=4
	movq	32(%rcx), %rcx
	movzbl	(%rcx), %edi
	cmpq	$39, %rdi
	jne	.LBB36_648
# BB#647:                               # %land.lhs.true61.i
                                        #   in Loop: Header=BB36_645 Depth=4
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rcx
	movzbl	(%rcx), %edi
.LBB36_648:                             # %land.lhs.true61.i
                                        #   in Loop: Header=BB36_645 Depth=4
	cmpq	$47, %rdi
	jne	.LBB36_650
# BB#649:                               # %land.lhs.true73.i
                                        #   in Loop: Header=BB36_645 Depth=4
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$78, %rcx
	je	.LBB36_671
.LBB36_650:                             # %for.inc.i727
                                        #   in Loop: Header=BB36_645 Depth=4
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rsi
	cmpq	%rdx, %rdi
	jl	.LBB36_645
	jmp	.LBB36_651
	.align	16, 0x90
.LBB36_657:                             # %for.inc94
                                        #   in Loop: Header=BB36_612 Depth=2
	movq	128(%r13), %r13
	testq	%r13, %r13
	jne	.LBB36_612
# BB#658:                               # %for.end96
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	24(%r12), %rax
	testq	%rax, %rax
	je	.LBB36_750
	.align	16, 0x90
.LBB36_659:                             # %for.body.i738
                                        #   Parent Loop BB36_514 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB36_665 Depth 3
	movzbl	100(%rax), %ecx
	testq	$4, %rcx
	jne	.LBB36_673
# BB#660:                               # %lor.lhs.false.i740
                                        #   in Loop: Header=BB36_659 Depth=2
	movq	136(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB36_662
# BB#661:                               # %land.lhs.true.i741
                                        #   in Loop: Header=BB36_659 Depth=2
	movzbl	100(%rdx), %edx
	testq	$4, %rdx
	jne	.LBB36_673
.LBB36_662:                             # %if.end.i744
                                        #   in Loop: Header=BB36_659 Depth=2
	movl	24(%rax), %edx
	cmpq	$1, %rdx
	jne	.LBB36_673
# BB#663:                               # %land.lhs.true10.i
                                        #   in Loop: Header=BB36_659 Depth=2
	movq	32(%rax), %rdx
	movl	8(%rdx), %esi
	movq	reg_n_info(%rip), %rdi
	movq	(%rax), %rbx
	movq	32(%rdi,%rsi,8), %rdi
	movl	(%rdi), %esi
	movl	8(%rbx), %ebx
	cmpq	%rbx, %rsi
	jne	.LBB36_673
# BB#664:                               # %for.cond17.preheader.i
                                        #   in Loop: Header=BB36_659 Depth=2
	movq	24(%r12), %rsi
	testq	%rsi, %rsi
	je	.LBB36_673
	jmp	.LBB36_665
	.align	16, 0x90
.LBB36_668:                             # %for.inc.for.body19_crit_edge.i
                                        #   in Loop: Header=BB36_665 Depth=3
	movq	reg_n_info(%rip), %rdi
	movl	8(%rdx), %ebx
	movq	32(%rdi,%rbx,8), %rdi
.LBB36_665:                             # %for.body19.i
                                        #   Parent Loop BB36_514 Depth=1
                                        #     Parent Loop BB36_659 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rsi), %rbx
	movl	4(%rdi), %edi
	movl	8(%rbx), %ebx
	cmpq	%rbx, %rdi
	jne	.LBB36_667
# BB#666:                               # %if.then33.i
                                        #   in Loop: Header=BB36_665 Depth=3
	orq	$128, %rcx
	movw	%cx, 100(%rax)
.LBB36_667:                             # %for.inc.i759
                                        #   in Loop: Header=BB36_665 Depth=3
	movq	128(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB36_668
	.align	16, 0x90
.LBB36_673:                             # %for.inc38.i
                                        #   in Loop: Header=BB36_659 Depth=2
	movq	128(%rax), %rax
	testq	%rax, %rax
	jne	.LBB36_659
# BB#674:                               # %loop_givs_dead_check.exit
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	24(%r12), %r14
	testq	%r14, %r14
	je	.LBB36_750
	.align	16, 0x90
.LBB36_675:                             # %for.body.i771
                                        #   Parent Loop BB36_514 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB36_680 Depth 3
	movzbl	100(%r14), %eax
	testq	$4, %rax
	jne	.LBB36_694
# BB#676:                               # %land.lhs.true.i774
                                        #   in Loop: Header=BB36_675 Depth=2
	cmpq	$0, 136(%r14)
	jne	.LBB36_694
# BB#677:                               # %if.then.i776
                                        #   in Loop: Header=BB36_675 Depth=2
	cmpq	$0, 8(%r14)
	jne	.LBB36_679
# BB#678:                               # %if.then3.i
                                        #   in Loop: Header=BB36_675 Depth=2
	movl	48(%r14), %edi
	callq	gen_reg_rtx
	movq	%rax, 8(%r14)
.LBB36_679:                             # %if.end.i779
                                        #   in Loop: Header=BB36_675 Depth=2
	movq	8(%r12), %rbx
	jmp	.LBB36_680
	.align	16, 0x90
.LBB36_684:                             # %for.inc.i787
                                        #   in Loop: Header=BB36_680 Depth=3
	callq	loop_iv_add_mult_emit_before
	movq	128(%rbx), %rbx
.LBB36_680:                             # %if.end.i779
                                        #   Parent Loop BB36_514 Depth=1
                                        #     Parent Loop BB36_675 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB36_685
# BB#681:                               # %if.then9.i783
                                        #   in Loop: Header=BB36_680 Depth=3
	movq	(%rbx), %rax
	movq	64(%rbx), %rcx
	movq	const_int_rtx+520(%rip), %rdi
	movq	72(%rbx), %rsi
	movq	64(%r14), %rdx
	cmpq	%rdi, %rcx
	jne	.LBB36_683
# BB#682:                               # %if.then18.i784
                                        #   in Loop: Header=BB36_680 Depth=3
	movq	8(%r14), %rcx
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	%rcx, %r8
	jmp	.LBB36_684
	.align	16, 0x90
.LBB36_683:                             # %if.else22.i
                                        #   in Loop: Header=BB36_680 Depth=3
	movq	8(%r14), %r8
	movq	72(%r14), %rcx
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	176(%rsp), %rdi         # 8-byte Reload
	jmp	.LBB36_684
	.align	16, 0x90
.LBB36_685:                             # %for.end.i789
                                        #   in Loop: Header=BB36_675 Depth=2
	movq	120(%r14), %rdx
	movq	40(%r12), %rax
	testq	%rdx, %rdx
	je	.LBB36_693
# BB#686:                               # %if.end.i.i792
                                        #   in Loop: Header=BB36_675 Depth=2
	movl	(%rax), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$133, %rcx
	jg	.LBB36_689
# BB#687:                               # %if.end.i.i792
                                        #   in Loop: Header=BB36_675 Depth=2
	addq	$-54, %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$14, %rcx
	ja	.LBB36_692
# BB#688:                               # %if.end.i.i792
                                        #   in Loop: Header=BB36_675 Depth=2
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	$24599, %rdi            # imm = 0x6017
	jne	.LBB36_691
	jmp	.LBB36_692
.LBB36_689:                             # %if.end.i.i792
                                        #   in Loop: Header=BB36_675 Depth=2
	cmpq	$134, %rcx
	je	.LBB36_691
# BB#690:                               # %if.end.i.i792
                                        #   in Loop: Header=BB36_675 Depth=2
	cmpq	$140, %rcx
	jne	.LBB36_692
.LBB36_691:                             # %land.lhs.true.i.i793
                                        #   in Loop: Header=BB36_675 Depth=2
	testq	$16711680, %rsi         # imm = 0xFF0000
	je	.LBB36_693
.LBB36_692:                             # %if.end32.i.i
                                        #   in Loop: Header=BB36_675 Depth=2
	movl	(%rdx), %esi
	movq	%rsi, %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
.LBB36_693:                             # %extend_value_for_giv.exit.i
                                        #   in Loop: Header=BB36_675 Depth=2
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	8(%r14), %r8
	movq	64(%r14), %rdx
	movq	72(%r14), %rcx
	movq	%rax, %rsi
	callq	loop_iv_add_mult_hoist
.LBB36_694:                             # %for.inc33.i
                                        #   in Loop: Header=BB36_675 Depth=2
	movq	128(%r14), %r14
	testq	%r14, %r14
	jne	.LBB36_675
# BB#695:                               # %loop_givs_reduce.exit
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	24(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB36_750
	.align	16, 0x90
.LBB36_696:                             # %for.body.i803
                                        #   Parent Loop BB36_514 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	136(%rbx), %rax
	testq	%rax, %rax
	je	.LBB36_699
# BB#697:                               # %land.lhs.true.i808
                                        #   in Loop: Header=BB36_696 Depth=2
	movzbl	100(%rax), %ecx
	testq	$4, %rcx
	je	.LBB36_699
# BB#698:                               # %if.then.i812
                                        #   in Loop: Header=BB36_696 Depth=2
	movzbl	100(%rbx), %ecx
	orq	$4, %rcx
	movw	%cx, 100(%rbx)
.LBB36_699:                             # %if.end.i815
                                        #   in Loop: Header=BB36_696 Depth=2
	movzbl	100(%rbx), %ecx
	testq	$4, %rcx
	jne	.LBB36_743
# BB#700:                               # %if.end14.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	8(%rbx), %rdi
	testq	%rax, %rax
	je	.LBB36_702
# BB#701:                               # %if.then17.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	8(%rax), %rdx
	movq	32(%rax), %rsi
	callq	replace_rtx
	movq	%rax, %rdi
	movq	%rdi, 8(%rbx)
.LBB36_702:                             # %if.end22.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB36_717
# BB#703:                               # %land.lhs.true26.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movl	24(%rbx), %eax
	cmpq	$1, %rax
	jne	.LBB36_706
# BB#704:                               # %land.lhs.true28.i821
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	32(%rbx), %rcx
	movslq	(%rcx), %rdx
	testq	%rdx, %rdx
	jns	.LBB36_706
# BB#705:                               # %if.then33.i827
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	8(%rcx), %ecx
	movq	64(%rax), %rax
	movzbl	(%rax,%rcx), %esi
	jmp	.LBB36_716
.LBB36_706:                             # %land.lhs.true42.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	16(%rbx), %rcx
	movslq	(%rcx), %rdx
	testq	%rdx, %rdx
	jns	.LBB36_711
# BB#707:                               # %if.then46.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	8(%rcx), %ecx
	movq	64(%rax), %rax
	movzbl	(%rax,%rcx), %esi
	testq	%rsi, %rsi
	je	.LBB36_715
# BB#708:                               # %lor.lhs.false.i834
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	72(%rbx), %rax
	jmp	.LBB36_709
.LBB36_711:                             # %land.lhs.true78.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	72(%rbx), %rcx
	movl	(%rcx), %edx
	movabsq	$2147549183, %rsi       # imm = 0x8000FFFF
	andq	%rsi, %rdx
	movabsq	$2147483709, %rsi       # imm = 0x8000003D
	cmpq	%rsi, %rdx
	jne	.LBB36_714
# BB#712:                               # %if.then89.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	8(%rcx), %ecx
	movq	64(%rax), %rax
	movzbl	(%rax,%rcx), %esi
	testq	%rsi, %rsi
	je	.LBB36_715
# BB#713:                               # %lor.lhs.false102.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	64(%rbx), %rax
.LBB36_709:                             # %lor.lhs.false.i834
                                        #   in Loop: Header=BB36_696 Depth=2
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB36_715
# BB#710:                               # %lor.lhs.false62.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	8(%rax), %rax
	movq	$3, %rcx
	movq	%rsi, %r8
	shrq	%cl, %r8
	cqto
	idivq	%r8
	testq	%rdx, %rdx
	jne	.LBB36_715
	jmp	.LBB36_716
.LBB36_714:                             # %land.lhs.true126.i
                                        #   in Loop: Header=BB36_696 Depth=2
	testq	%rax, %rax
	jne	.LBB36_717
.LBB36_715:                             # %if.then130.i
                                        #   in Loop: Header=BB36_696 Depth=2
	xorq	%rsi, %rsi
.LBB36_716:                             # %if.end135.i841
                                        #   in Loop: Header=BB36_696 Depth=2
	callq	mark_reg_pointer
.LBB36_717:                             # %if.end135.i841
                                        #   in Loop: Header=BB36_696 Depth=2
	movl	24(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB36_719
# BB#718:                               # %if.then139.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	40(%rbx), %rsi
	movq	(%rbx), %rdi
	movq	8(%rbx), %rdx
	xorq	%rcx, %rcx
	callq	validate_change
	jmp	.LBB36_722
	.align	16, 0x90
.LBB36_719:                             # %if.else142.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movzbl	100(%rbx), %eax
	testq	$1, %rax
	jne	.LBB36_720
# BB#721:                               # %if.else155.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	32(%rbx), %rdi
	movq	(%rbx), %r14
	movq	8(%rbx), %rsi
	callq	gen_move_insn
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	emit_insn_after
	jmp	.LBB36_722
.LBB36_720:                             # %if.then147.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	32(%rbx), %rax
	movq	8(%rbx), %rcx
	movl	8(%rax), %eax
	movq	112(%rsp), %rdx         # 8-byte Reload
	movq	%rcx, (%rdx,%rax,8)
.LBB36_722:                             # %if.end162.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movzbl	88(%r12), %eax
	testq	$8, %rax
	je	.LBB36_733
# BB#723:                               # %land.lhs.true168.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movzbl	100(%rbx), %eax
	testq	$1, %rax
	jne	.LBB36_733
# BB#724:                               # %if.then174.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	120(%rbx), %rdx
	movq	40(%r12), %rax
	testq	%rdx, %rdx
	je	.LBB36_732
# BB#725:                               # %if.end.i.i851
                                        #   in Loop: Header=BB36_696 Depth=2
	movl	(%rax), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$133, %rcx
	jg	.LBB36_728
# BB#726:                               # %if.end.i.i851
                                        #   in Loop: Header=BB36_696 Depth=2
	addq	$-54, %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$14, %rcx
	ja	.LBB36_731
# BB#727:                               # %if.end.i.i851
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	$24599, %rdi            # imm = 0x6017
	jne	.LBB36_730
	jmp	.LBB36_731
	.align	16, 0x90
.LBB36_733:                             # %if.else179.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	88(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB36_741
# BB#734:                               # %if.then181.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	32(%rbx), %rdi
	callq	gen_move_insn
	movq	176(%rsp), %rdx         # 8-byte Reload
	movl	240(%rdx), %ecx
	testq	%rcx, %rcx
	jne	.LBB36_735
# BB#737:                               # %if.else.i.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	%rdx, %rcx
	movq	224(%rcx), %rsi
	testq	%rsi, %rsi
	jne	.LBB36_740
# BB#738:                               # %if.then.i.i.i.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	192(%rcx), %rsi
	testq	%rsi, %rsi
	jne	.LBB36_740
	jmp	.LBB36_739
.LBB36_735:                             # %if.then.i.i864
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	192(%rdx), %rsi
	testq	%rsi, %rsi
	je	.LBB36_736
.LBB36_740:                             # %if.end.i.i.i.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	%rax, %rdi
	callq	emit_insn_before
	jmp	.LBB36_741
.LBB36_728:                             # %if.end.i.i851
                                        #   in Loop: Header=BB36_696 Depth=2
	cmpq	$134, %rcx
	je	.LBB36_730
# BB#729:                               # %if.end.i.i851
                                        #   in Loop: Header=BB36_696 Depth=2
	cmpq	$140, %rcx
	jne	.LBB36_731
.LBB36_730:                             # %land.lhs.true.i.i854
                                        #   in Loop: Header=BB36_696 Depth=2
	testq	$16711680, %rsi         # imm = 0xFF0000
	je	.LBB36_732
.LBB36_731:                             # %if.end32.i.i859
                                        #   in Loop: Header=BB36_696 Depth=2
	movl	(%rdx), %esi
	movq	%rsi, %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
.LBB36_732:                             # %extend_value_for_giv.exit.i861
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	32(%rbx), %r8
	movq	64(%rbx), %rdx
	movq	72(%rbx), %rcx
	movq	%rax, %rsi
	callq	loop_iv_add_mult_sink
.LBB36_741:                             # %if.end187.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB36_743
# BB#742:                               # %if.then189.i
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	(%rbx), %rax
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str46, %rsi
	callq	fprintf
	movq	loop_dump_stream(%rip), %rdi
	movq	8(%rbx), %rsi
	callq	print_simple_rtl
	movq	loop_dump_stream(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
.LBB36_743:                             # %for.inc.i873
                                        #   in Loop: Header=BB36_696 Depth=2
	movq	128(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB36_696
# BB#744:                               # %for.cond98.preheader
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	24(%r12), %rax
	jmp	.LBB36_745
	.align	16, 0x90
.LBB36_749:                             # %for.inc115
                                        #   in Loop: Header=BB36_745 Depth=2
	movq	128(%rax), %rax
.LBB36_745:                             # %for.cond98.preheader
                                        #   Parent Loop BB36_514 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rax, %rax
	je	.LBB36_750
# BB#746:                               # %for.body100
                                        #   in Loop: Header=BB36_745 Depth=2
	movzbl	100(%rax), %ecx
	testq	$128, %rcx
	jne	.LBB36_749
# BB#747:                               # %land.lhs.true106
                                        #   in Loop: Header=BB36_745 Depth=2
	movq	136(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB36_749
# BB#748:                               # %if.then109
                                        #   in Loop: Header=BB36_745 Depth=2
	movzbl	100(%rcx), %edx
	andq	$65407, %rdx            # imm = 0xFF7F
	movw	%dx, 100(%rcx)
	jmp	.LBB36_749
	.align	16, 0x90
.LBB36_750:                             # %for.end117
                                        #   in Loop: Header=BB36_514 Depth=1
	movzbl	88(%r12), %eax
	andq	$18, %rax
	cmpq	$18, %rax
	movq	136(%rsp), %rbx         # 8-byte Reload
	jne	.LBB36_763
# BB#751:                               # %land.lhs.true132
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	$1, %rdx
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	%r12, %rsi
	movq	128(%rsp), %rcx         # 8-byte Reload
	movq	%rbx, %r8
	callq	maybe_eliminate_biv
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB36_752
.LBB36_763:                             # %if.else154
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.LBB36_769
# BB#764:                               # %land.lhs.true157
                                        #   in Loop: Header=BB36_514 Depth=1
	movzbl	88(%r12), %eax
	testq	$8, %rax
	jne	.LBB36_769
# BB#765:                               # %if.then164
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	8(%r12), %rax
	movq	32(%rax), %rdi
	callq	gen_move_insn
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	224(%rcx), %rsi
	testq	%rsi, %rsi
	jne	.LBB36_768
# BB#766:                               # %if.then.i.i895
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	192(%rcx), %rsi
	testq	%rsi, %rsi
	je	.LBB36_767
.LBB36_768:                             # %if.end.i.i900
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	%rax, %rdi
	callq	emit_insn_before
	jmp	.LBB36_769
	.align	16, 0x90
.LBB36_752:                             # %if.then135
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.LBB36_761
# BB#753:                               # %land.lhs.true137
                                        #   in Loop: Header=BB36_514 Depth=1
	movzbl	88(%r12), %eax
	testq	$8, %rax
	jne	.LBB36_761
# BB#754:                               # %if.then144
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	8(%r12), %rax
	movq	32(%rax), %rdi
	callq	gen_move_insn
	movq	176(%rsp), %rdx         # 8-byte Reload
	movl	240(%rdx), %ecx
	testq	%rcx, %rcx
	jne	.LBB36_755
# BB#757:                               # %if.else.i884
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	%rdx, %rcx
	movq	224(%rcx), %rsi
	testq	%rsi, %rsi
	jne	.LBB36_760
# BB#758:                               # %if.then.i.i.i886
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	192(%rcx), %rsi
	testq	%rsi, %rsi
	jne	.LBB36_760
	jmp	.LBB36_759
.LBB36_755:                             # %if.then.i879
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	192(%rdx), %rsi
	testq	%rsi, %rsi
	je	.LBB36_756
.LBB36_760:                             # %if.end.i.i.i890
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	%rax, %rdi
	callq	emit_insn_before
.LBB36_761:                             # %if.end149
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB36_769
# BB#762:                               # %if.then151
                                        #   in Loop: Header=BB36_514 Depth=1
	movl	(%r12), %edx
	xorq	%rax, %rax
	movabsq	$.L.str41, %rsi
	callq	fprintf
	.align	16, 0x90
.LBB36_769:                             # %for.inc172
                                        #   in Loop: Header=BB36_514 Depth=1
	movq	64(%r12), %r12
	testq	%r12, %r12
	jne	.LBB36_514
.LBB36_522:                             # %for.cond174.preheader
	movq	176(%rsp), %rsi         # 8-byte Reload
	movq	192(%rsi), %rbx
	movq	200(%rsi), %rax
	cmpq	%rax, %rbx
	je	.LBB36_527
# BB#523:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	112(%rsp), %r12         # 8-byte Reload
	movq	88(%rsp), %r13          # 8-byte Reload
	.align	16, 0x90
.LBB36_524:                             # %for.body177
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %ecx
	addq	$-32, %rcx
	andq	%r14, %rcx
	cmpq	$2, %rcx
	ja	.LBB36_526
# BB#525:                               # %if.then192
                                        #   in Loop: Header=BB36_524 Depth=1
	movq	32(%rbx), %rdi
	xorq	%rcx, %rcx
	movq	%rsi, %r15
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	replace_regs
	movq	56(%rbx), %rdi
	xorq	%rcx, %rcx
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	replace_regs
	movq	%r15, %rsi
	movl	%r14d, 40(%rbx)
	movq	200(%rsi), %rax
.LBB36_526:                             # %for.inc204
                                        #   in Loop: Header=BB36_524 Depth=1
	movq	24(%rbx), %rbx
	cmpq	%rax, %rbx
	jne	.LBB36_524
.LBB36_527:                             # %for.end208
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	96(%rax), %rdx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	je	.LBB36_531
# BB#528:                               # %if.then211
	movq	%rsi, %rbx
	movq	136(%rsp), %rax         # 8-byte Reload
	leaq	-2(%rax), %rsi
	imulq	%rdx, %rsi
	decq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	xorq	%rax, %rax
	testq	%rdi, %rdi
	js	.LBB36_530
# BB#529:                               # %if.then211
	movq	%rsi, %rax
.LBB36_530:                             # %if.then211
	movq	%rbx, %rsi
.LBB36_531:                             # %if.end221
	testq	$1, 80(%rsp)            # 8-byte Folded Reload
	jne	.LBB36_535
# BB#532:                               # %lor.lhs.false224
	testq	$8, 80(%rsp)            # 8-byte Folded Reload
	jne	.LBB36_536
# BB#533:                               # %lor.lhs.false224
	testq	%rdx, %rdx
	je	.LBB36_536
# BB#534:                               # %lor.lhs.false224
	movq	$32, %rcx
	movq	136(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jg	.LBB36_536
.LBB36_535:                             # %if.then234
	movq	$1, %rdx
	movq	%rsi, %rdi
	movq	%rsi, %rbx
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	unroll_loop
	movq	%rbx, %rsi
.LBB36_536:                             # %if.end235
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	jne	.LBB36_541
# BB#537:                               # %land.lhs.true238
	movl	ix86_cpu(%rip), %ecx
	movl	x86_use_loop(%rip), %eax
	movq	$1, %rdx
	shlq	%cl, %rdx
	andq	%rax, %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	je	.LBB36_541
# BB#538:                               # %land.lhs.true241
	testq	$2, 80(%rsp)            # 8-byte Folded Reload
	je	.LBB36_541
# BB#539:                               # %land.lhs.true241
	movl	flag_branch_on_count_reg(%rip), %ecx
	testq	%rax, %rcx
	je	.LBB36_541
# BB#540:                               # %if.then246
	movq	%rsi, %rdi
	movq	%rsi, %rbx
	callq	doloop_optimize
	movq	%rbx, %rsi
.LBB36_541:                             # %if.end248
	movq	%rsi, %rbx
	testq	$2, 80(%rsp)            # 8-byte Folded Reload
	je	.LBB36_544
# BB#542:                               # %land.lhs.true251
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	96(%rcx), %rax
	movl	104(%rcx), %ecx
	xorq	%rdx, %rdx
	divq	%rcx
	movq	%rax, %rsi
	cmpq	$2, %rsi
	jb	.LBB36_544
# BB#543:                               # %if.then256
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	movq	200(%rbx), %rax
	sarq	%cl, %rsi
	movq	16(%rax), %rdi
	movq	$10000, %rcx            # imm = 0x2710
	movq	$10000, %rax            # imm = 0x2710
	xorq	%rdx, %rdx
	idivq	%rsi
	subq	%rax, %rcx
	movq	$5, %rsi
	movq	%rcx, %rdx
	callq	predict_insn
.LBB36_544:                             # %if.end268
	movq	loop_dump_stream(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB36_546
# BB#545:                               # %if.then270
	movq	$10, %rdi
	callq	fputc
.LBB36_546:                             # %if.end272
	movq	160(%rbx), %rax
	movq	208(%rax), %rdi
	movq	224(%rax), %r15
	callq	free
	testq	%r15, %r15
	je	.LBB36_552
	.align	16, 0x90
.LBB36_547:                             # %while.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB36_548 Depth 2
                                        #     Child Loop BB36_550 Depth 2
	movq	8(%r15), %rdi
	movq	64(%r15), %r14
	testq	%rdi, %rdi
	je	.LBB36_549
	.align	16, 0x90
.LBB36_548:                             # %for.body.i398
                                        #   Parent Loop BB36_547 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	128(%rdi), %rbx
	callq	free
	testq	%rbx, %rbx
	movq	%rbx, %rdi
	jne	.LBB36_548
.LBB36_549:                             # %for.end.i
                                        #   in Loop: Header=BB36_547 Depth=1
	movq	24(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB36_551
	.align	16, 0x90
.LBB36_550:                             # %for.body6.i
                                        #   Parent Loop BB36_547 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	128(%rdi), %rbx
	callq	free
	testq	%rbx, %rbx
	movq	%rbx, %rdi
	jne	.LBB36_550
.LBB36_551:                             # %for.end9.i
                                        #   in Loop: Header=BB36_547 Depth=1
	movq	%r15, %rdi
	callq	free
	testq	%r14, %r14
	movq	%r14, %r15
	jne	.LBB36_547
.LBB36_552:                             # %loop_ivs_free.exit
	movq	112(%rsp), %rdi         # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB36_554
# BB#553:                               # %if.then274
	callq	free
.LBB36_554:                             # %if.end275
	addq	$5824, %rsp             # imm = 0x16C0
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
	.align	16, 0x90
.LBB36_736:                             # %tailrecurse.us.i.i.i865
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB36_736
	.align	16, 0x90
.LBB36_767:                             # %tailrecurse.us.i.i896
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB36_767
	.align	16, 0x90
.LBB36_739:                             # %tailrecurse.us.i.i.i.i
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB36_739
	.align	16, 0x90
.LBB36_756:                             # %tailrecurse.us.i.i880
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB36_756
	.align	16, 0x90
.LBB36_759:                             # %tailrecurse.us.i.i.i887
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB36_759
.LBB36_555:                             # %cond.false.i606
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.loop_biv_eliminable_p, %rcx
	movq	$4530, %rsi             # imm = 0x11B2
	jmp	.LBB36_556
.LBB36_591:                             # %sw.default.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.check_ext_dependent_givs, %rcx
	movq	$7362, %rsi             # imm = 0x1CC2
.LBB36_556:                             # %cond.false.i606
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp418:
	.size	strength_reduce, .Ltmp418-strength_reduce
	.cfi_endproc

	.align	16, 0x90
	.type	combine_givs,@function
combine_givs:                           # @combine_givs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp422:
	.cfi_def_cfa_offset 16
.Ltmp423:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp424:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$88, %rsp
.Ltmp425:
	.cfi_offset %rbx, -56
.Ltmp426:
	.cfi_offset %r12, -48
.Ltmp427:
	.cfi_offset %r13, -40
.Ltmp428:
	.cfi_offset %r14, -32
.Ltmp429:
	.cfi_offset %r15, -24
	movq	24(%rsi), %rax
	movq	%rdi, -128(%rbp)        # 8-byte Spill
	testq	%rax, %rax
	je	.LBB37_73
# BB#1:
	xorq	%r12, %r12
	movq	%rax, %rdx
	movq	$2, %rcx
	.align	16, 0x90
.LBB37_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, %rsi
	movzbl	100(%rdx), %r12d
	shrq	%cl, %r12
	movq	128(%rdx), %rdx
	notq	%r12
	andq	$1, %r12
	addq	%rsi, %r12
	testq	%rdx, %rdx
	jne	.LBB37_2
# BB#3:                                 # %for.end
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$3, %rcx
	movq	%rbx, %r15
	shlq	%cl, %r15
	leaq	15(,%rbx,8), %rcx
	andq	$-16, %rcx
	movq	%rsp, %r14
	subq	%rcx, %r14
	movq	%r14, %rsp
	testq	%rax, %rax
	je	.LBB37_8
# BB#4:
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB37_5:                               # %for.body5
                                        # =>This Inner Loop Header: Depth=1
	movzbl	100(%rax), %ecx
	testq	$4, %rcx
	jne	.LBB37_7
# BB#6:                                 # %if.then12
                                        #   in Loop: Header=BB37_5 Depth=1
	movq	$32, %rcx
	leaq	1(%rdx), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rax, (%r14,%rdx,8)
	movq	%rsi, %rdx
.LBB37_7:                               # %for.inc15
                                        #   in Loop: Header=BB37_5 Depth=1
	movq	128(%rax), %rax
	testq	%rax, %rax
	jne	.LBB37_5
	jmp	.LBB37_8
.LBB37_73:                              # %for.end.thread
	xorq	%r15, %r15
	leaq	-48(%rbp), %r14
	xorq	%rbx, %rbx
	xorq	%r12, %r12
.LBB37_8:                               # %for.end17
	movq	%r12, -56(%rbp)         # 8-byte Spill
	movq	%r14, -112(%rbp)        # 8-byte Spill
	movq	$8, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	%rax, -64(%rbp)         # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	xcalloc
	movq	%rax, %r10
	movq	%r10, -72(%rbp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r14, %rsi
	jle	.LBB37_33
# BB#9:                                 # %for.body25.lr.ph
	xorq	%r8, %r8
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	xorq	%r9, %r9
	.align	16, 0x90
.LBB37_10:                              # %for.body25
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB37_14 Depth 2
	movq	(%rsi,%r9,8), %rbx
	movq	$32, %rcx
	movq	%r8, %rax
	movq	-64(%rbp), %rdx         # 8-byte Reload
	movl	%r9d, (%rdx,%r9,8)
	movl	24(%rbx), %edx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %rdx
	jne	.LBB37_13
# BB#11:                                # %land.lhs.true
                                        #   in Loop: Header=BB37_10 Depth=1
	movq	32(%rbx), %rcx
	movl	8(%rcx), %ecx
	movq	-128(%rbp), %rdx        # 8-byte Reload
	movq	8(%rdx), %rdx
	leaq	(%rcx,%rcx,2), %rcx
	movq	8(%rdx,%rcx,8), %rcx
	testq	%rcx, %rcx
	je	.LBB37_13
# BB#12:                                # %land.lhs.true
                                        #   in Loop: Header=BB37_10 Depth=1
	movq	const_int_rtx+512(%rip), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB37_32
	.align	16, 0x90
.LBB37_13:                              # %for.body51.lr.ph
                                        #   in Loop: Header=BB37_10 Depth=1
	movq	%r9, -104(%rbp)         # 8-byte Spill
	movq	%r8, -96(%rbp)          # 8-byte Spill
	movzbl	100(%rbx), %edx
	movq	$11, %rcx
	shrq	%cl, %rdx
	movl	80(%rbx), %ecx
	leaq	(%r10,%rax,8), %rax
	movq	%rax, -88(%rbp)         # 8-byte Spill
	andq	$1, %rdx
	addq	%rdx, %rcx
	movq	%rcx, -80(%rbp)         # 8-byte Spill
	movq	$1, %r15
	.align	16, 0x90
.LBB37_14:                              # %for.body51
                                        #   Parent Loop BB37_10 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-8(%rsi,%r15,8), %r13
	cmpq	%r13, %rbx
	je	.LBB37_30
# BB#15:                                # %land.lhs.true56
                                        #   in Loop: Header=BB37_14 Depth=2
	movslq	48(%rbx), %rax
	movslq	48(%r13), %rcx
	movzbl	mode_size(%rax), %eax
	movzbl	mode_size(%rcx), %ecx
	cmpq	%rcx, %rax
	jb	.LBB37_30
# BB#16:                                # %if.end.i
                                        #   in Loop: Header=BB37_14 Depth=2
	movq	%rsi, %r12
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	express_from
	movq	%rax, %r14
	testq	%r14, %r14
	jne	.LBB37_18
# BB#17:                                #   in Loop: Header=BB37_14 Depth=2
	movq	%r12, %rsi
	movq	-56(%rbp), %r12         # 8-byte Reload
	movq	-72(%rbp), %r10         # 8-byte Reload
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	jmp	.LBB37_30
.LBB37_18:                              # %if.end9.i
                                        #   in Loop: Header=BB37_14 Depth=2
	movl	48(%rbx), %eax
	movl	48(%r13), %edi
	cmpq	%rdi, %rax
	movq	%r14, %rcx
	je	.LBB37_20
# BB#19:                                # %if.then14.i
                                        #   in Loop: Header=BB37_14 Depth=2
	movq	%r14, %rsi
	callq	gen_lowpart
	movq	%rax, %rcx
.LBB37_20:                              # %if.end17.i
                                        #   in Loop: Header=BB37_14 Depth=2
	movq	32(%rbx), %rax
	cmpq	%rax, %r14
	movq	%r12, %rsi
	jne	.LBB37_23
# BB#21:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB37_14 Depth=2
	movl	24(%rbx), %eax
	cmpq	$1, %rax
	je	.LBB37_28
# BB#22:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB37_14 Depth=2
	movl	24(%r13), %eax
	testq	%rax, %rax
	jne	.LBB37_23
.LBB37_28:                              # %combine_givs_p.exit
                                        #   in Loop: Header=BB37_14 Depth=2
	testq	%rcx, %rcx
	movq	-56(%rbp), %r12         # 8-byte Reload
	movq	-72(%rbp), %r10         # 8-byte Reload
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	je	.LBB37_30
	jmp	.LBB37_29
.LBB37_23:                              # %if.end26.i
                                        #   in Loop: Header=BB37_14 Depth=2
	testq	%rcx, %rcx
	movq	-56(%rbp), %r12         # 8-byte Reload
	movq	-72(%rbp), %r10         # 8-byte Reload
	jne	.LBB37_25
# BB#24:                                #   in Loop: Header=BB37_14 Depth=2
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	jmp	.LBB37_30
.LBB37_25:                              # %land.lhs.true29.i
                                        #   in Loop: Header=BB37_14 Depth=2
	movl	24(%r13), %eax
	testq	%rax, %rax
	jne	.LBB37_26
# BB#27:                                # %land.lhs.true33.i
                                        #   in Loop: Header=BB37_14 Depth=2
	movq	56(%r13), %rax
	movzbl	2(%rax), %edi
	movq	%rcx, %rsi
	movq	%r10, %r14
	movq	%rcx, -120(%rbp)        # 8-byte Spill
	callq	memory_address_p
	movq	-120(%rbp), %rcx        # 8-byte Reload
	movq	%r14, %r10
	movq	-112(%rbp), %rsi        # 8-byte Reload
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	testq	%rdi, %rax
	je	.LBB37_30
.LBB37_29:                              # %if.then60
                                        #   in Loop: Header=BB37_14 Depth=2
	movq	-88(%rbp), %rax         # 8-byte Reload
	movq	%rcx, -8(%rax,%r15,8)
	movl	80(%r13), %eax
	movq	-80(%rbp), %rcx         # 8-byte Reload
	leaq	3(%rcx,%rax), %rcx
	movq	%rcx, -80(%rbp)         # 8-byte Spill
	jmp	.LBB37_30
.LBB37_26:                              #   in Loop: Header=BB37_14 Depth=2
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB37_30:                              # %for.inc69
                                        #   in Loop: Header=BB37_14 Depth=2
	movq	%r15, %rax
	andq	%rdi, %rax
	movq	%r12, %rcx
	andq	%rdi, %rcx
	incq	%r15
	cmpq	%rcx, %rax
	jne	.LBB37_14
# BB#31:                                # %for.end71
                                        #   in Loop: Header=BB37_10 Depth=1
	movq	-64(%rbp), %rax         # 8-byte Reload
	movq	-104(%rbp), %r9         # 8-byte Reload
	movq	-80(%rbp), %rcx         # 8-byte Reload
	movl	%ecx, 4(%rax,%r9,8)
	movq	-96(%rbp), %r8          # 8-byte Reload
.LBB37_32:                              # %for.inc74
                                        #   in Loop: Header=BB37_10 Depth=1
	incq	%r9
	movq	%r9, %rax
	andq	%rdi, %rax
	movq	%r12, %rcx
	andq	%rdi, %rcx
	addq	%r12, %r8
	cmpq	%rcx, %rax
	jne	.LBB37_10
	jmp	.LBB37_33
.LBB37_70:                              # %for.end250
                                        #   in Loop: Header=BB37_33 Depth=1
	movl	80(%r15), %eax
	movq	-88(%rbp), %rcx         # 8-byte Reload
	addq	%rcx, %rax
	movl	%eax, 80(%r15)
	.align	16, 0x90
.LBB37_33:                              # %restart
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB37_36 Depth 2
                                        #     Child Loop BB37_43 Depth 2
                                        #       Child Loop BB37_46 Depth 3
                                        #         Child Loop BB37_57 Depth 4
                                        #     Child Loop BB37_67 Depth 2
	movq	%r10, %r13
	movq	%rsi, %r15
	movabsq	$cmp_combine_givs_stats, %rcx
	movq	$8, %rdx
	movq	-64(%rbp), %rdi         # 8-byte Reload
	movq	%r12, %rsi
	callq	specqsort
	movq	loop_dump_stream(%rip), %rax
	testq	%rax, %rax
	je	.LBB37_41
# BB#34:                                # %if.then78
                                        #   in Loop: Header=BB37_33 Depth=1
	movq	$32, %rcx
	movq	%r12, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movabsq	$.L.str47, %rdi
	movq	$27, %rsi
	movq	$1, %rdx
	movq	$1, %rbx
	movq	%rax, %rcx
	callq	fwrite
	testq	%r14, %r14
	movq	%r15, %r8
	jle	.LBB37_40
# BB#35:                                #   in Loop: Header=BB37_33 Depth=1
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%r12, %r15
	andq	%rdx, %r15
	.align	16, 0x90
.LBB37_36:                              # %for.body83
                                        #   Parent Loop BB37_33 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-64(%rbp), %rax         # 8-byte Reload
	movslq	-8(%rax,%rbx,8), %rax
	movq	(%r8,%rax,8), %rax
	movl	96(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB37_39
# BB#37:                                # %land.lhs.true90
                                        #   in Loop: Header=BB37_36 Depth=2
	cmpq	$0, 136(%rax)
	jne	.LBB37_39
# BB#38:                                # %if.then92
                                        #   in Loop: Header=BB37_36 Depth=2
	movq	(%rax), %rax
	movq	loop_dump_stream(%rip), %rdi
	movq	-64(%rbp), %rcx         # 8-byte Reload
	movl	-4(%rcx,%rbx,8), %ecx
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str48, %rsi
	movq	%r12, %r14
	movq	%r8, %r12
	callq	fprintf
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%r12, %r8
	movq	%r14, %r12
.LBB37_39:                              # %for.inc105
                                        #   in Loop: Header=BB37_36 Depth=2
	movq	%rbx, %rax
	andq	%rdx, %rax
	incq	%rbx
	cmpq	%r15, %rax
	jne	.LBB37_36
.LBB37_40:                              # %for.end107
                                        #   in Loop: Header=BB37_33 Depth=1
	movq	%r8, %r15
	movq	loop_dump_stream(%rip), %rsi
	movq	$10, %rdi
	callq	putc
.LBB37_41:                              # %for.cond110.preheader
                                        #   in Loop: Header=BB37_33 Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r15, %rsi
	jle	.LBB37_72
# BB#42:                                #   in Loop: Header=BB37_33 Depth=1
	xorq	%rdi, %rdi
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB37_43:                              # %for.body113
                                        #   Parent Loop BB37_33 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB37_46 Depth 3
                                        #         Child Loop BB37_57 Depth 4
	movq	-64(%rbp), %rax         # 8-byte Reload
	movslq	(%rax,%rdi,8), %rcx
	movq	(%rsi,%rcx,8), %r15
	movl	96(%r15), %eax
	testq	%rax, %rax
	jne	.LBB37_71
# BB#44:                                # %lor.lhs.false
                                        #   in Loop: Header=BB37_43 Depth=2
	cmpq	$0, 136(%r15)
	jne	.LBB37_71
# BB#45:                                # %for.body128.lr.ph
                                        #   in Loop: Header=BB37_43 Depth=2
	movq	%rdi, -104(%rbp)        # 8-byte Spill
	movq	%rcx, %rdx
	movq	%rcx, -96(%rbp)         # 8-byte Spill
	imulq	%r12, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, -80(%rbp)         # 8-byte Spill
	xorq	%r14, %r14
	movq	$0, -88(%rbp)           # 8-byte Folded Spill
	movq	%r13, %rbx
	.align	16, 0x90
.LBB37_46:                              # %for.body128
                                        #   Parent Loop BB37_33 Depth=1
                                        #     Parent Loop BB37_43 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB37_57 Depth 4
	movq	(%rsi,%r14,8), %r13
	cmpq	%r13, %r15
	je	.LBB37_64
# BB#47:                                # %land.lhs.true133
                                        #   in Loop: Header=BB37_46 Depth=3
	leaq	(%r14,%rdx), %rax
	movq	(%rbx,%rax,8), %rdi
	testq	%rdi, %rdi
	je	.LBB37_64
# BB#48:                                # %land.lhs.true139
                                        #   in Loop: Header=BB37_46 Depth=3
	cmpq	$0, 136(%r13)
	jne	.LBB37_64
# BB#49:                                # %land.lhs.true142
                                        #   in Loop: Header=BB37_46 Depth=3
	movl	96(%r13), %eax
	testq	%rax, %rax
	jne	.LBB37_63
# BB#50:                                # %if.then145
                                        #   in Loop: Header=BB37_46 Depth=3
	movq	%rdi, 8(%r13)
	movq	%r15, 136(%r13)
	movl	24(%r13), %ecx
	movl	80(%r13), %eax
	testq	%rcx, %rcx
	jne	.LBB37_52
# BB#51:                                # %if.then154
                                        #   in Loop: Header=BB37_46 Depth=3
	movq	56(%r13), %rcx
	movl	reg_address_cost(%rip), %r12d
	movzbl	2(%rcx), %esi
	addq	%rax, %r12
	callq	address_cost
	subq	%rax, %r12
	movl	%r12d, 80(%r13)
	movq	%r12, %rax
.LBB37_52:                              # %if.end163
                                        #   in Loop: Header=BB37_46 Depth=3
	movl	96(%r15), %ecx
	incq	%rcx
	movl	%ecx, 96(%r15)
	movl	104(%r13), %ecx
	movl	104(%r15), %r9d
	addq	%rcx, %r9
	movl	%r9d, 104(%r15)
	movzbl	100(%r13), %ecx
	addq	%rax, -88(%rbp)         # 8-byte Folded Spill
	testq	$1, %rcx
	movq	-56(%rbp), %r12         # 8-byte Reload
	jne	.LBB37_53
# BB#54:                                # %land.lhs.true174
                                        #   in Loop: Header=BB37_46 Depth=3
	movq	32(%r13), %rcx
	movzbl	3(%rcx), %ecx
	testq	$8, %rcx
	movq	-64(%rbp), %rsi         # 8-byte Reload
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	je	.LBB37_56
# BB#55:                                # %if.then180
                                        #   in Loop: Header=BB37_46 Depth=3
	movzbl	copy_cost(%rip), %edx
	movq	$2, %rcx
	shlq	%cl, %rdx
	subq	%rdx, -88(%rbp)         # 8-byte Folded Spill
	jmp	.LBB37_56
.LBB37_53:                              #   in Loop: Header=BB37_46 Depth=3
	movq	-64(%rbp), %rsi         # 8-byte Reload
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
.LBB37_56:                              # %for.body186.lr.ph
                                        #   in Loop: Header=BB37_46 Depth=3
	movq	$1, %rdx
	.align	16, 0x90
.LBB37_57:                              # %for.body186
                                        #   Parent Loop BB37_33 Depth=1
                                        #     Parent Loop BB37_43 Depth=2
                                        #       Parent Loop BB37_46 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movl	-8(%rsi,%rdx,8), %edi
	imulq	%r12, %rdi
	addq	%r14, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$0, (%rbx,%rdi,8)
	je	.LBB37_59
# BB#58:                                # %if.then195
                                        #   in Loop: Header=BB37_57 Depth=4
	movl	-4(%rsi,%rdx,8), %ecx
	movq	$-3, %rdi
	subq	%rax, %rdi
	addq	%rcx, %rdi
	movl	%edi, -4(%rsi,%rdx,8)
.LBB37_59:                              # %for.inc203
                                        #   in Loop: Header=BB37_57 Depth=4
	movq	%rdx, %rcx
	andq	%r8, %rcx
	movq	%r12, %rdi
	andq	%r8, %rdi
	incq	%rdx
	cmpq	%rdi, %rcx
	jne	.LBB37_57
# BB#60:                                # %for.end205
                                        #   in Loop: Header=BB37_46 Depth=3
	movq	%rsi, -64(%rbp)         # 8-byte Spill
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB37_62
# BB#61:                                # %if.then207
                                        #   in Loop: Header=BB37_46 Depth=3
	movq	(%r13), %rax
	movq	(%r15), %rcx
	movq	%r8, %r13
	movl	80(%r15), %r8d
	movl	8(%rcx), %ecx
	movl	8(%rax), %edx
	subq	$16, %rsp
	xorq	%rax, %rax
	movq	%r9, (%rsp)
	movabsq	$.L.str49, %rsi
	movq	-88(%rbp), %r9          # 8-byte Reload
	callq	fprintf
	movq	%r13, %r8
	addq	$16, %rsp
.LBB37_62:                              # %for.inc221
                                        #   in Loop: Header=BB37_46 Depth=3
	movq	-112(%rbp), %rsi        # 8-byte Reload
.LBB37_63:                              # %for.inc221
                                        #   in Loop: Header=BB37_46 Depth=3
	movq	-80(%rbp), %rdx         # 8-byte Reload
	.align	16, 0x90
.LBB37_64:                              # %for.inc221
                                        #   in Loop: Header=BB37_46 Depth=3
	incq	%r14
	movq	%r14, %rcx
	andq	%r8, %rcx
	movq	%r12, %rax
	andq	%r8, %rax
	cmpq	%rax, %rcx
	jne	.LBB37_46
# BB#65:                                # %for.end223
                                        #   in Loop: Header=BB37_43 Depth=2
	movq	%rbx, %r13
	movl	96(%r15), %ecx
	testq	%rcx, %rcx
	movq	-96(%rbp), %r11         # 8-byte Reload
	movq	-104(%rbp), %rdi        # 8-byte Reload
	jne	.LBB37_66
.LBB37_71:                              # %for.inc254
                                        #   in Loop: Header=BB37_43 Depth=2
	incq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB37_43
	jmp	.LBB37_72
.LBB37_66:                              # %for.body230.lr.ph
                                        #   in Loop: Header=BB37_33 Depth=1
	movq	$1, %rdx
	movq	%r13, %r10
	.align	16, 0x90
.LBB37_67:                              # %for.body230
                                        #   Parent Loop BB37_33 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-64(%rbp), %rcx         # 8-byte Reload
	movl	-8(%rcx,%rdx,8), %edi
	imulq	%r12, %rdi
	addq	%r11, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$0, (%r10,%rdi,8)
	je	.LBB37_69
# BB#68:                                # %if.then240
                                        #   in Loop: Header=BB37_67 Depth=2
	movl	80(%r15), %ecx
	movq	-64(%rbp), %rbx         # 8-byte Reload
	movl	-4(%rbx,%rdx,8), %r9d
	movq	$-3, %rdi
	subq	%rcx, %rdi
	addq	%r9, %rdi
	movl	%edi, -4(%rbx,%rdx,8)
.LBB37_69:                              # %for.inc248
                                        #   in Loop: Header=BB37_67 Depth=2
	movq	%rdx, %rcx
	andq	%r8, %rcx
	incq	%rdx
	cmpq	%rax, %rcx
	jne	.LBB37_67
	jmp	.LBB37_70
.LBB37_72:                              # %for.end256
	movq	-64(%rbp), %rdi         # 8-byte Reload
	callq	free
	movq	%r13, %rdi
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	jmp	free  # TAILCALL
.Ltmp430:
	.size	combine_givs, .Ltmp430-combine_givs
	.cfi_endproc

	.align	16, 0x90
	.type	maybe_eliminate_biv,@function
maybe_eliminate_biv:                    # @maybe_eliminate_biv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp437:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp438:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp439:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp440:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp441:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp442:
	.cfi_def_cfa_offset 112
.Ltmp443:
	.cfi_offset %rbx, -48
.Ltmp444:
	.cfi_offset %r12, -40
.Ltmp445:
	.cfi_offset %r13, -32
.Ltmp446:
	.cfi_offset %r14, -24
.Ltmp447:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	192(%r14), %rbx
	movq	200(%r14), %rax
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	cmpq	%rax, %rbx
	je	.LBB38_1
# BB#2:                                 # %for.body.lr.ph
	movq	8(%rsi), %rax
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	160(%r14), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	32(%rax), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB38_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %r13
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	48(%rsp), %rdx          # 8-byte Reload
	movl	(%rbx), %r14d
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%r14, %r12
	andq	$65535, %r12            # imm = 0xFFFF
	cmpq	%rax, %rdx
	movabsq	$0, %r15
	jge	.LBB38_5
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB38_3 Depth=1
	movq	%rbx, %r15
.LBB38_5:                               # %for.body
                                        #   in Loop: Header=BB38_3 Depth=1
	movzbl	rtx_class(%r12), %eax
	cmpq	$105, %rax
	jne	.LBB38_18
# BB#6:                                 # %if.then
                                        #   in Loop: Header=BB38_3 Depth=1
	movq	$7, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB38_18
# BB#7:                                 # %if.then5
                                        #   in Loop: Header=BB38_3 Depth=1
	movq	8(%rax), %rdi
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB38_18
# BB#8:                                 # %cond.true14
                                        #   in Loop: Header=BB38_3 Depth=1
	movq	32(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB38_10
# BB#9:                                 #   in Loop: Header=BB38_3 Depth=1
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	jmp	.LBB38_11
.LBB38_10:                              # %cond.false26
                                        #   in Loop: Header=BB38_3 Depth=1
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	%rax, %rsi
	callq	single_set_2
.LBB38_11:                              # %cond.end34
                                        #   in Loop: Header=BB38_3 Depth=1
	testq	%rax, %rax
	je	.LBB38_18
# BB#12:                                # %land.lhs.true
                                        #   in Loop: Header=BB38_3 Depth=1
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB38_18
# BB#13:                                # %if.then44
                                        #   in Loop: Header=BB38_3 Depth=1
	movl	8(%rax), %eax
	movq	8(%rsp), %rdx           # 8-byte Reload
	movl	216(%rdx), %ecx
	cmpq	%rcx, %rax
	jae	.LBB38_18
# BB#14:                                # %land.lhs.true52
                                        #   in Loop: Header=BB38_3 Depth=1
	movq	208(%rdx), %rdx
	movq	$4, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movl	(%rdx,%rsi), %ecx
	cmpq	$3, %rcx
	jne	.LBB38_18
# BB#15:                                # %land.lhs.true57
                                        #   in Loop: Header=BB38_3 Depth=1
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	8(%rdx,%rax), %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	movq	16(%rax), %rax
	movq	16(%rcx), %rcx
	cmpq	%rcx, %rax
	movq	16(%rsp), %rax          # 8-byte Reload
	je	.LBB38_17
# BB#16:                                # %select.mid
                                        #   in Loop: Header=BB38_3 Depth=1
	movq	%rbx, %rax
.LBB38_17:                              # %select.end
                                        #   in Loop: Header=BB38_3 Depth=1
	movq	%rax, %rbx
	.align	16, 0x90
.LBB38_18:                              # %if.end68
                                        #   in Loop: Header=BB38_3 Depth=1
	andq	$65534, %r14            # imm = 0xFFFE
	cmpq	$32, %r14
	movq	%r13, %r14
	je	.LBB38_20
# BB#19:                                # %if.end68
                                        #   in Loop: Header=BB38_3 Depth=1
	cmpq	$34, %r12
	jne	.LBB38_24
.LBB38_20:                              # %land.lhs.true76
                                        #   in Loop: Header=BB38_3 Depth=1
	movq	32(%rbx), %rsi
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB38_24
# BB#21:                                # %land.lhs.true82
                                        #   in Loop: Header=BB38_3 Depth=1
	movq	32(%rbx), %rsi
	movq	%r14, %rdi
	movq	%rbx, %rdx
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	24(%rsp), %r8           # 8-byte Reload
	movq	%r15, %r9
	callq	maybe_eliminate_biv_1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB38_22
.LBB38_24:                              # %for.inc
                                        #   in Loop: Header=BB38_3 Depth=1
	movq	24(%rbx), %rbx
	movq	200(%r14), %rax
	cmpq	%rax, %rbx
	jne	.LBB38_3
	jmp	.LBB38_25
.LBB38_1:
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	jmp	.LBB38_25
.LBB38_22:                              # %if.then88
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB38_25
# BB#23:                                # %if.then90
	movl	8(%rbx), %ecx
	movq	32(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %edx
	movabsq	$.L.str42, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB38_25:                              # %for.end
	movq	200(%r14), %rax
	xorq	%r14, %r14
	cmpq	%rax, %rbx
	jne	.LBB38_31
# BB#26:                                # %if.then103
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	movq	$1, %r14
	je	.LBB38_31
# BB#27:                                # %if.then105
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	24(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	jne	.LBB38_28
# BB#29:                                # %if.then105
	movabsq	$.L.str45, %rcx
	jmp	.LBB38_30
.LBB38_28:
	movabsq	$.L.str44, %rcx
.LBB38_30:                              # %if.then105
	movq	32(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %edx
	movabsq	$.L.str43, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB38_31:                              # %return
	movq	%r14, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp448:
	.size	maybe_eliminate_biv, .Ltmp448-maybe_eliminate_biv
	.cfi_endproc

	.align	16, 0x90
	.type	maybe_eliminate_biv_1,@function
maybe_eliminate_biv_1:                  # @maybe_eliminate_biv_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp455:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp456:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp457:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp458:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp459:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp460:
	.cfi_def_cfa_offset 128
.Ltmp461:
	.cfi_offset %rbx, -48
.Ltmp462:
	.cfi_offset %r12, -40
.Ltmp463:
	.cfi_offset %r13, -32
.Ltmp464:
	.cfi_offset %r14, -24
.Ltmp465:
	.cfi_offset %r15, -16
	movq	8(%rcx), %rax
	movq	32(%rax), %rbx
	movq	%rsi, %r14
	movzbl	(%r14), %eax
	movzbl	2(%rbx), %r10d
	movq	%r9, 40(%rsp)           # 8-byte Spill
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	cmpq	$101, %rax
	jg	.LBB39_15
# BB#1:                                 # %entry
	cmpq	$60, %rax
	jg	.LBB39_9
# BB#2:                                 # %entry
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	cmpq	$47, %rax
	jne	.LBB39_115
# BB#3:                                 # %sw.bb3
	movq	8(%r14), %rdx
	cmpq	%rbx, %rdx
	movq	$1, %r15
	je	.LBB39_126
# BB#4:                                 # %if.end6
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	24(%rcx), %rcx
	jmp	.LBB39_5
.LBB39_15:                              # %entry
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	leaq	-102(%rax), %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rdx, %rsi
	cmpq	$10, %rsi
	jae	.LBB39_115
.LBB39_16:                              # %sw.bb15
	movq	8(%r14), %rsi
	movq	16(%r14), %rcx
	cmpq	%rbx, %rsi
	movq	$1, %rdx
	je	.LBB39_17
# BB#18:                                # %if.else
	xorq	%rdx, %rdx
	cmpq	%rbx, %rcx
	jne	.LBB39_115
	jmp	.LBB39_19
.LBB39_9:                               # %entry
	cmpq	$61, %rax
	je	.LBB39_12
# BB#10:                                # %entry
	cmpq	$66, %rax
	jne	.LBB39_11
# BB#110:                               # %sw.bb342
	movq	24(%rcx), %rdi
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	testq	%rdi, %rdi
	je	.LBB39_115
# BB#111:                               # %for.body346.lr.ph
	leaq	8(%r14), %rdx
	movq	$1, %r15
	.align	16, 0x90
.LBB39_112:                             # %for.body346
                                        # =>This Inner Loop Header: Depth=1
	movl	24(%rdi), %esi
	testq	%rsi, %rsi
	jne	.LBB39_114
# BB#113:                               # %land.lhs.true349
                                        #   in Loop: Header=BB39_112 Depth=1
	movq	40(%rdi), %rsi
	cmpq	%rdx, %rsi
	je	.LBB39_126
.LBB39_114:                             # %for.inc356
                                        #   in Loop: Header=BB39_112 Depth=1
	movq	128(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.LBB39_112
	jmp	.LBB39_115
.LBB39_17:
	movq	%rcx, %rsi
.LBB39_19:                              # %if.end34
	movzbl	(%rsi), %ecx
	cmpq	$133, %rcx
	jg	.LBB39_25
# BB#20:                                # %if.end34
	addq	$-54, %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	$14, %rcx
	jbe	.LBB39_22
# BB#21:
	xorq	%r15, %r15
	jmp	.LBB39_126
.LBB39_25:                              # %if.end34
	movq	%r10, 32(%rsp)          # 8-byte Spill
	cmpq	$134, %rcx
	je	.LBB39_28
# BB#26:                                # %if.end34
	cmpq	$140, %rcx
	je	.LBB39_28
# BB#27:
	xorq	%r15, %r15
	jmp	.LBB39_126
.LBB39_8:                               # %for.inc
                                        #   in Loop: Header=BB39_5 Depth=1
	movq	128(%rcx), %rcx
.LBB39_5:                               # %if.end6
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB39_115
# BB#6:                                 # %for.body
                                        #   in Loop: Header=BB39_5 Depth=1
	movl	24(%rcx), %esi
	cmpq	$1, %rsi
	jne	.LBB39_8
# BB#7:                                 # %land.lhs.true
                                        #   in Loop: Header=BB39_5 Depth=1
	movq	32(%rcx), %rsi
	cmpq	%rsi, %rdx
	jne	.LBB39_8
	jmp	.LBB39_126
.LBB39_12:                              # %sw.bb
	cmpq	%r14, %rbx
	je	.LBB39_13
# BB#14:                                # %sw.bb
	movabsq	$1, %r15
	jmp	.LBB39_126
.LBB39_11:                              # %entry
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	cmpq	$74, %rax
	je	.LBB39_16
.LBB39_115:                             # %sw.epilog
	movzbl	rtx_length(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB39_116
# BB#117:                               # %for.body367.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB39_118:                             # %for.body367
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB39_124 Depth 2
	movq	%rdx, %r13
	movq	24(%rsp), %rax          # 8-byte Reload
	movsbq	-1(%rax,%r13), %rax
	leaq	-1(%r13), %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	andq	%rbx, %rax
	cmpq	$69, %rax
	jne	.LBB39_119
# BB#123:                               # %sw.bb380
                                        #   in Loop: Header=BB39_118 Depth=1
	movq	(%r14,%r13,8), %rax
	movslq	(%rax), %r12
	.align	16, 0x90
.LBB39_124:                             # %for.cond385
                                        #   Parent Loop BB39_118 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB39_121
# BB#125:                               # %for.body388
                                        #   in Loop: Header=BB39_124 Depth=2
	movq	(%r14,%r13,8), %rax
	movq	(%rax,%r12,8), %rsi
	decq	%r12
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	56(%rsp), %r8           # 8-byte Reload
	movq	40(%rsp), %r9           # 8-byte Reload
	callq	maybe_eliminate_biv_1
	xorq	%r15, %r15
	testq	%rbx, %rax
	jne	.LBB39_124
	jmp	.LBB39_126
	.align	16, 0x90
.LBB39_119:                             # %for.body367
                                        #   in Loop: Header=BB39_118 Depth=1
	cmpq	$101, %rax
	jne	.LBB39_121
# BB#120:                               # %sw.bb371
                                        #   in Loop: Header=BB39_118 Depth=1
	movq	(%r14,%r13,8), %rsi
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	56(%rsp), %r8           # 8-byte Reload
	movq	40(%rsp), %r9           # 8-byte Reload
	callq	maybe_eliminate_biv_1
	xorq	%r15, %r15
	testq	%rbx, %rax
	je	.LBB39_126
.LBB39_121:                             # %for.cond364.backedge
                                        #   in Loop: Header=BB39_118 Depth=1
	movq	$32, %rcx
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB39_118
# BB#122:
	movq	$1, %r15
	jmp	.LBB39_126
.LBB39_116:
	movq	$1, %r15
	jmp	.LBB39_126
.LBB39_22:                              # %if.end34
	movq	%r10, 32(%rsp)          # 8-byte Spill
	movq	$1, %rax
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	$24599, %rdi            # imm = 0x6017
	je	.LBB39_23
.LBB39_28:                              # %if.then65
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	24(%rax), %r13
	leaq	24(%rax), %r12
	testq	%r13, %r13
	je	.LBB39_103
# BB#29:                                # %for.body69.lr.ph
	movq	%rdx, %rax
	xorq	$1, %rax
	leaq	8(%r14,%rax,8), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	leaq	8(%r14,%rdx,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$1, %r15
	.align	16, 0x90
.LBB39_30:                              # %for.body69
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB39_40 Depth 2
                                        #     Child Loop BB39_54 Depth 2
                                        #     Child Loop BB39_68 Depth 2
                                        #     Child Loop BB39_82 Depth 2
	movq	64(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB39_102
# BB#31:                                # %land.lhs.true73
                                        #   in Loop: Header=BB39_30 Depth=1
	cmpq	$0, 8(%rax)
	jle	.LBB39_102
# BB#32:                                # %land.lhs.true78
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	72(%r13), %rax
	movl	(%rax), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	leaq	-67(%rcx), %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	$2, %rdx
	jb	.LBB39_36
# BB#33:                                # %land.lhs.true78
                                        #   in Loop: Header=BB39_30 Depth=1
	cmpq	$58, %rcx
	je	.LBB39_36
# BB#34:                                # %land.lhs.true78
                                        #   in Loop: Header=BB39_30 Depth=1
	cmpq	$61, %rcx
	jne	.LBB39_102
# BB#35:                                # %land.lhs.true97
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB39_102
	.align	16, 0x90
.LBB39_36:                              # %land.lhs.true102
                                        #   in Loop: Header=BB39_30 Depth=1
	movzbl	100(%r13), %eax
	movq	%rax, %rcx
	andq	$140, %rcx
	cmpq	$8, %rcx
	jne	.LBB39_102
# BB#37:                                # %land.lhs.true119
                                        #   in Loop: Header=BB39_30 Depth=1
	movl	48(%r13), %ecx
	movq	32(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %rcx
	jne	.LBB39_102
# BB#38:                                # %if.then122
                                        #   in Loop: Header=BB39_30 Depth=1
	testq	$256, %rax              # imm = 0x100
	je	.LBB39_95
# BB#39:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %r8
	movq	(%r13), %r9
	movl	max_uid_for_loop(%rip), %r11d
	movq	%r9, %rdi
	movq	72(%rsp), %r10          # 8-byte Reload
	jmp	.LBB39_40
.LBB39_51:                              # %if.then56.i.i
                                        #   in Loop: Header=BB39_40 Depth=2
	movq	24(%r10), %r10
	.align	16, 0x90
.LBB39_40:                              # %for.cond.i.i
                                        #   Parent Loop BB39_30 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r9, %r10
	je	.LBB39_67
# BB#41:                                # %for.cond.i.i
                                        #   in Loop: Header=BB39_40 Depth=2
	testq	%rdi, %rdi
	je	.LBB39_67
# BB#42:                                # %if.end.i.i
                                        #   in Loop: Header=BB39_40 Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %rdi
	je	.LBB39_53
# BB#43:                                # %if.end.i.i
                                        #   in Loop: Header=BB39_40 Depth=2
	testq	%r10, %r10
	je	.LBB39_53
# BB#44:                                # %if.end5.i.i
                                        #   in Loop: Header=BB39_40 Depth=2
	movslq	8(%rdi), %rax
	movq	$32, %rcx
	movq	%r11, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB39_45
# BB#46:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB39_40 Depth=2
	movslq	8(%r10), %rsi
	movq	$32, %rcx
	movq	%r11, %rbx
	shlq	%cl, %rbx
	movzbl	(%rdi), %edx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rsi
	jge	.LBB39_48
# BB#47:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB39_40 Depth=2
	cmpq	$37, %rdx
	jne	.LBB39_52
.LBB39_48:                              # %lor.lhs.false42.i.i
                                        #   in Loop: Header=BB39_40 Depth=2
	cmpq	$37, %rdx
	jne	.LBB39_50
	jmp	.LBB39_49
	.align	16, 0x90
.LBB39_45:                              # %if.end5.i.if.then47.i_crit_edge.i
                                        #   in Loop: Header=BB39_40 Depth=2
	movl	8(%r10), %esi
.LBB39_49:                              # %if.then47.i.i
                                        #   in Loop: Header=BB39_40 Depth=2
	movq	24(%rdi), %rdi
.LBB39_50:                              # %if.end50.i.i
                                        #   in Loop: Header=BB39_40 Depth=2
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rax, %rsi
	jl	.LBB39_40
	jmp	.LBB39_51
.LBB39_52:                              # %loop_insn_first_p.exit.i
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	uid_luid(%rip), %rcx
	movslq	(%rcx,%rax,4), %rax
	movslq	(%rcx,%rsi,4), %rcx
	cmpq	%rcx, %rax
	jg	.LBB39_67
.LBB39_53:                              # %land.lhs.true3.i
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	(%r8), %r10
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	%r10, %r14
	jmp	.LBB39_54
.LBB39_65:                              # %if.then56.i164.i
                                        #   in Loop: Header=BB39_54 Depth=2
	movq	24(%r14), %r14
	.align	16, 0x90
.LBB39_54:                              # %for.cond.i130.i
                                        #   Parent Loop BB39_30 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r14
	je	.LBB39_67
# BB#55:                                # %for.cond.i130.i
                                        #   in Loop: Header=BB39_54 Depth=2
	testq	%rbx, %rbx
	je	.LBB39_67
# BB#56:                                # %if.end.i133.i
                                        #   in Loop: Header=BB39_54 Depth=2
	cmpq	%r10, %rbx
	je	.LBB39_102
# BB#57:                                # %if.end.i133.i
                                        #   in Loop: Header=BB39_54 Depth=2
	testq	%r14, %r14
	je	.LBB39_102
# BB#58:                                # %if.end5.i137.i
                                        #   in Loop: Header=BB39_54 Depth=2
	movslq	8(%rbx), %rsi
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rsi
	jge	.LBB39_59
# BB#60:                                # %land.lhs.true.i139.i
                                        #   in Loop: Header=BB39_54 Depth=2
	movslq	8(%r14), %rax
	movq	$32, %rcx
	movq	%r11, %rdi
	shlq	%cl, %rdi
	movzbl	(%rbx), %edx
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rax
	jge	.LBB39_62
# BB#61:                                # %land.lhs.true.i139.i
                                        #   in Loop: Header=BB39_54 Depth=2
	cmpq	$37, %rdx
	jne	.LBB39_66
.LBB39_62:                              # %lor.lhs.false42.i155.i
                                        #   in Loop: Header=BB39_54 Depth=2
	cmpq	$37, %rdx
	jne	.LBB39_64
	jmp	.LBB39_63
	.align	16, 0x90
.LBB39_59:                              # %if.end5.i137.if.then47.i158_crit_edge.i
                                        #   in Loop: Header=BB39_54 Depth=2
	movl	8(%r14), %eax
.LBB39_63:                              # %if.then47.i158.i
                                        #   in Loop: Header=BB39_54 Depth=2
	movq	24(%rbx), %rbx
.LBB39_64:                              # %if.end50.i161.i
                                        #   in Loop: Header=BB39_54 Depth=2
	movq	$32, %rcx
	movq	%r11, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB39_54
	jmp	.LBB39_65
.LBB39_66:                              # %loop_insn_first_p.exit166.i
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	uid_luid(%rip), %rcx
	movslq	(%rcx,%rsi,4), %rdx
	movslq	(%rcx,%rax,4), %rax
	cmpq	%rax, %rdx
	jle	.LBB39_102
.LBB39_67:                              # %lor.lhs.false.i
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	(%r8), %r8
	movq	%r8, %rdi
	movq	72(%rsp), %r10          # 8-byte Reload
	jmp	.LBB39_68
.LBB39_80:                              # %if.then56.i115.i
                                        #   in Loop: Header=BB39_68 Depth=2
	movq	24(%r10), %r10
	.align	16, 0x90
.LBB39_68:                              # %for.cond.i81.i
                                        #   Parent Loop BB39_30 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r8, %r10
	je	.LBB39_95
# BB#69:                                # %for.cond.i81.i
                                        #   in Loop: Header=BB39_68 Depth=2
	testq	%rdi, %rdi
	je	.LBB39_95
# BB#70:                                # %if.end.i84.i
                                        #   in Loop: Header=BB39_68 Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %rdi
	je	.LBB39_71
# BB#72:                                # %if.end.i84.i
                                        #   in Loop: Header=BB39_68 Depth=2
	testq	%r10, %r10
	je	.LBB39_71
# BB#73:                                # %if.end5.i88.i
                                        #   in Loop: Header=BB39_68 Depth=2
	movslq	8(%rdi), %rdx
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jge	.LBB39_74
# BB#75:                                # %land.lhs.true.i90.i
                                        #   in Loop: Header=BB39_68 Depth=2
	movslq	8(%r10), %rax
	movq	$32, %rcx
	movq	%r11, %rbx
	shlq	%cl, %rbx
	movzbl	(%rdi), %esi
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rax
	jge	.LBB39_77
# BB#76:                                # %land.lhs.true.i90.i
                                        #   in Loop: Header=BB39_68 Depth=2
	cmpq	$37, %rsi
	jne	.LBB39_81
.LBB39_77:                              # %lor.lhs.false42.i106.i
                                        #   in Loop: Header=BB39_68 Depth=2
	cmpq	$37, %rsi
	jne	.LBB39_79
	jmp	.LBB39_78
	.align	16, 0x90
.LBB39_74:                              # %if.end5.i88.if.then47.i109_crit_edge.i
                                        #   in Loop: Header=BB39_68 Depth=2
	movl	8(%r10), %eax
.LBB39_78:                              # %if.then47.i109.i
                                        #   in Loop: Header=BB39_68 Depth=2
	movq	24(%rdi), %rdi
.LBB39_79:                              # %if.end50.i112.i
                                        #   in Loop: Header=BB39_68 Depth=2
	movq	$32, %rcx
	movq	%r11, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB39_68
	jmp	.LBB39_80
.LBB39_71:                              #   in Loop: Header=BB39_30 Depth=1
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	%r9, %r8
	jmp	.LBB39_82
.LBB39_81:                              # %loop_insn_first_p.exit117.i
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	uid_luid(%rip), %rcx
	movslq	(%rcx,%rdx,4), %rdx
	movslq	(%rcx,%rax,4), %rax
	cmpq	%rax, %rdx
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	%r9, %r8
	jg	.LBB39_95
	jmp	.LBB39_82
.LBB39_93:                              # %if.then56.i66.i
                                        #   in Loop: Header=BB39_82 Depth=2
	movq	24(%r8), %r8
	.align	16, 0x90
.LBB39_82:                              # %for.cond.i32.i
                                        #   Parent Loop BB39_30 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r8
	je	.LBB39_95
# BB#83:                                # %for.cond.i32.i
                                        #   in Loop: Header=BB39_82 Depth=2
	testq	%rsi, %rsi
	je	.LBB39_95
# BB#84:                                # %if.end.i35.i
                                        #   in Loop: Header=BB39_82 Depth=2
	cmpq	%r9, %rsi
	je	.LBB39_102
# BB#85:                                # %if.end.i35.i
                                        #   in Loop: Header=BB39_82 Depth=2
	testq	%r8, %r8
	je	.LBB39_102
# BB#86:                                # %if.end5.i39.i
                                        #   in Loop: Header=BB39_82 Depth=2
	movslq	8(%rsi), %rdx
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jge	.LBB39_87
# BB#88:                                # %land.lhs.true.i41.i
                                        #   in Loop: Header=BB39_82 Depth=2
	movslq	8(%r8), %rax
	movq	$32, %rcx
	movq	%r11, %rdi
	shlq	%cl, %rdi
	movzbl	(%rsi), %ebx
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rax
	jge	.LBB39_90
# BB#89:                                # %land.lhs.true.i41.i
                                        #   in Loop: Header=BB39_82 Depth=2
	cmpq	$37, %rbx
	jne	.LBB39_94
.LBB39_90:                              # %lor.lhs.false42.i57.i
                                        #   in Loop: Header=BB39_82 Depth=2
	cmpq	$37, %rbx
	jne	.LBB39_92
	jmp	.LBB39_91
.LBB39_87:                              # %if.end5.i39.if.then47.i60_crit_edge.i
                                        #   in Loop: Header=BB39_82 Depth=2
	movl	8(%r8), %eax
.LBB39_91:                              # %if.then47.i60.i
                                        #   in Loop: Header=BB39_82 Depth=2
	movq	24(%rsi), %rsi
.LBB39_92:                              # %if.end50.i63.i
                                        #   in Loop: Header=BB39_82 Depth=2
	movq	$32, %rcx
	movq	%r11, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB39_82
	jmp	.LBB39_93
.LBB39_94:                              # %loop_insn_first_p.exit68.i
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	uid_luid(%rip), %rcx
	movslq	(%rcx,%rdx,4), %rdx
	movslq	(%rcx,%rax,4), %rax
	cmpq	%rax, %rdx
	jle	.LBB39_102
.LBB39_95:                              # %if.end126
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB39_126
# BB#96:                                # %if.end129
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	8(%r13), %rdx
	movq	$1, %rcx
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	validate_change
	movq	24(%rsp), %rsi          # 8-byte Reload
	movzbl	(%rsi), %eax
	cmpq	$54, %rax
	jne	.LBB39_100
# BB#97:                                # %land.lhs.true137
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	64(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB39_100
# BB#98:                                # %land.lhs.true142
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	72(%r13), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB39_100
# BB#99:                                # %if.then147
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	8(%rsi), %rdx
	movq	8(%rax), %rax
	movq	8(%rcx), %rsi
	imulq	%rdx, %rax
	addq	%rax, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$1, %rcx
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%rax, %rdx
	jmp	.LBB39_101
.LBB39_100:                             # %if.else165
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rsi, %rbx
	callq	gen_reg_rtx
	movq	64(%r13), %rdx
	movq	72(%r13), %rcx
	movq	%rax, %r14
	xorq	%r9, %r9
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r14, %r8
	callq	loop_iv_add_mult_emit_before
	movq	$1, %rcx
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r14, %rdx
.LBB39_101:                             # %if.end174
                                        #   in Loop: Header=BB39_30 Depth=1
	callq	validate_change
	callq	apply_change_group
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB39_126
	.align	16, 0x90
.LBB39_102:                             # %for.inc180
                                        #   in Loop: Header=BB39_30 Depth=1
	movq	128(%r13), %r13
	testq	%r13, %r13
	jne	.LBB39_30
	.align	16, 0x90
.LBB39_103:                             # %for.cond184
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r12), %rax
	testq	%rax, %rax
	leaq	128(%rax), %r12
	jne	.LBB39_103
# BB#104:
	xorq	%r15, %r15
	jmp	.LBB39_126
.LBB39_13:
	movabsq	$0, %r15
.LBB39_126:                             # %return
	movq	%r15, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB39_23:                              # %if.end34
	shlq	%cl, %rax
	testq	$4224, %rax             # imm = 0x1080
	je	.LBB39_24
# BB#105:                               # %if.then263
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	loop_invariant_p
	andq	%rbx, %rax
	cmpq	$1, %rax
	jne	.LBB39_106
# BB#107:                               # %if.then266
	movq	64(%rsp), %rax          # 8-byte Reload
	addq	$24, %rax
	.align	16, 0x90
.LBB39_108:                             # %for.cond268
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	leaq	128(%rax), %rax
	jne	.LBB39_108
# BB#109:
	xorq	%r15, %r15
	jmp	.LBB39_126
.LBB39_24:
	xorq	%r15, %r15
	jmp	.LBB39_126
.LBB39_106:
	xorq	%r15, %r15
	jmp	.LBB39_126
.Ltmp466:
	.size	maybe_eliminate_biv_1, .Ltmp466-maybe_eliminate_biv_1
	.cfi_endproc

	.align	16, 0x90
	.type	cmp_combine_givs_stats,@function
cmp_combine_givs_stats:                 # @cmp_combine_givs_stats
	.cfi_startproc
# BB#0:                                 # %entry
	movl	4(%rsi), %eax
	movl	4(%rdi), %ecx
	subq	%rcx, %rax
	jne	.LBB40_2
# BB#1:                                 # %if.then
	movl	(%rdi), %eax
	movl	(%rsi), %ecx
	subq	%rcx, %rax
.LBB40_2:                               # %if.end
	retq
.Ltmp467:
	.size	cmp_combine_givs_stats, .Ltmp467-cmp_combine_givs_stats
	.cfi_endproc

	.align	16, 0x90
	.type	note_set_pseudo_multiple_uses,@function
note_set_pseudo_multiple_uses:          # @note_set_pseudo_multiple_uses
	.cfi_startproc
# BB#0:                                 # %entry
	testq	%rdi, %rdi
	je	.LBB41_11
# BB#1:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB41_2
	.align	16, 0x90
.LBB41_5:                               # %while.body
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	8(%rdi), %rdi
.LBB41_2:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdi), %ecx
	leaq	-63(%rcx), %rsi
	andq	%rax, %rsi
	cmpq	$2, %rsi
	jb	.LBB41_5
# BB#3:                                 # %while.cond
                                        #   in Loop: Header=BB41_2 Depth=1
	cmpq	$61, %rcx
	je	.LBB41_6
# BB#4:                                 # %while.cond
                                        #   in Loop: Header=BB41_2 Depth=1
	addq	$-132, %rcx
	andq	%rax, %rcx
	cmpq	$1, %rcx
	jbe	.LBB41_5
	jmp	.LBB41_11
.LBB41_6:                               # %lor.lhs.false15
	movl	8(%rdi), %eax
	cmpq	$53, %rax
	jb	.LBB41_11
# BB#7:                                 # %if.end20
	movl	max_reg_before_loop(%rip), %ecx
	cmpq	%rcx, %rax
	jae	.LBB41_10
# BB#8:                                 # %lor.lhs.false25
	movq	8(%rdx), %rcx
	leaq	(%rax,%rax,2), %rax
	movq	8(%rcx,%rax,8), %rax
	testq	%rax, %rax
	je	.LBB41_10
# BB#9:                                 # %lor.lhs.false25
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB41_11
.LBB41_10:                              # %if.then39
	movq	$1, %rax
	movl	%eax, 16(%rdx)
.LBB41_11:                              # %if.end40
	retq
.Ltmp468:
	.size	note_set_pseudo_multiple_uses, .Ltmp468-note_set_pseudo_multiple_uses
	.cfi_endproc

	.align	16, 0x90
	.type	count_nonfixed_reads,@function
count_nonfixed_reads:                   # @count_nonfixed_reads
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp475:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp476:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp477:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp478:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp479:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp480:
	.cfi_def_cfa_offset 80
.Ltmp481:
	.cfi_offset %rbx, -48
.Ltmp482:
	.cfi_offset %r12, -40
.Ltmp483:
	.cfi_offset %r13, -32
.Ltmp484:
	.cfi_offset %r14, -24
.Ltmp485:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%r15, 24(%rsp)          # 8-byte Spill
	xorq	%r14, %r14
	testq	%r15, %r15
	je	.LBB42_15
# BB#1:                                 # %if.end
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movzbl	(%r15), %eax
	leaq	-54(%rax), %rcx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rcx
	cmpq	$15, %rcx
	ja	.LBB42_8
# BB#2:                                 # %if.end
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$57523, %rdx            # imm = 0xE0B3
	jne	.LBB42_15
# BB#3:                                 # %if.end
	cmpq	$12, %rcx
	jne	.LBB42_8
# BB#4:                                 # %sw.bb1
	movq	8(%r15), %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	loop_invariant_p
	andq	%r12, %rax
	cmpq	$1, %rax
	je	.LBB42_5
# BB#6:                                 # %sw.bb1
	movabsq	$1, %rbx
	jmp	.LBB42_7
.LBB42_8:                               # %sw.epilog
	movzbl	rtx_length(%rax), %r12d
	testq	%r12, %r12
	je	.LBB42_15
# BB#9:                                 # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rbx
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	xorq	%r14, %r14
	.align	16, 0x90
.LBB42_10:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB42_18 Depth 2
	movzbl	-1(%rbx,%r12), %ecx
	cmpq	$101, %rcx
	jne	.LBB42_12
# BB#11:                                # %if.then18
                                        #   in Loop: Header=BB42_10 Depth=1
	movq	(%r15,%r12,8), %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	count_nonfixed_reads
	movzbl	-1(%rbx,%r12), %ecx
	addq	%rax, %r14
.LBB42_12:                              # %if.end25
                                        #   in Loop: Header=BB42_10 Depth=1
	andq	$255, %rcx
	cmpq	$69, %rcx
	jne	.LBB42_13
# BB#17:                                # %for.cond32.preheader
                                        #   in Loop: Header=BB42_10 Depth=1
	movq	(%r15,%r12,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r13
	movq	%r14, %rbx
	movq	16(%rsp), %r15          # 8-byte Reload
	jle	.LBB42_14
	.align	16, 0x90
.LBB42_18:                              # %for.body38
                                        #   Parent Loop BB42_10 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r13,8), %rsi
	movq	%r15, %rdi
	callq	count_nonfixed_reads
	movq	%rax, %r14
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r12,8), %rax
	addq	%rbx, %r14
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r13
	cmpq	%rdx, %rsi
	movq	%r14, %rbx
	jl	.LBB42_18
	jmp	.LBB42_14
	.align	16, 0x90
.LBB42_13:                              #   in Loop: Header=BB42_10 Depth=1
	movq	16(%rsp), %r15          # 8-byte Reload
.LBB42_14:                              # %for.cond.backedge
                                        #   in Loop: Header=BB42_10 Depth=1
	movq	%r15, 16(%rsp)          # 8-byte Spill
	decq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	8(%rsp), %rbx           # 8-byte Reload
	jg	.LBB42_10
.LBB42_15:                              # %return
	movq	%r14, %rax
.LBB42_16:                              # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB42_5:
	movabsq	$0, %rbx
.LBB42_7:                               # %sw.bb1
	movq	8(%r15), %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	count_nonfixed_reads
	addq	%rbx, %rax
	jmp	.LBB42_16
.Ltmp486:
	.size	count_nonfixed_reads, .Ltmp486-count_nonfixed_reads
	.cfi_endproc

	.align	16, 0x90
	.type	remove_constant_addition,@function
remove_constant_addition:               # @remove_constant_addition
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp491:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp492:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp493:
	.cfi_def_cfa_offset 32
.Ltmp494:
	.cfi_offset %rbx, -32
.Ltmp495:
	.cfi_offset %r14, -24
.Ltmp496:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	(%r15), %rbx
	movzbl	(%rbx), %ecx
	cmpq	$54, %rcx
	je	.LBB43_10
# BB#1:                                 # %entry
	cmpq	$75, %rcx
	jne	.LBB43_2
# BB#7:                                 # %if.then49
	leaq	8(%rbx), %rdi
	callq	remove_constant_addition
	movq	%rax, %r14
	leaq	16(%rbx), %rdi
	callq	remove_constant_addition
	movq	8(%rbx), %rcx
	movq	const_int_rtx+512(%rip), %rsi
	movq	16(%rbx), %rdx
	addq	%r14, %rax
	cmpq	%rsi, %rcx
	je	.LBB43_8
# BB#9:                                 # %if.else66
	cmpq	%rsi, %rdx
	jne	.LBB43_12
	jmp	.LBB43_11
.LBB43_10:                              # %if.then42
	movq	const_int_rtx+512(%rip), %rcx
	movq	8(%rbx), %rax
.LBB43_11:                              # %if.then71
	movq	%rcx, (%r15)
	jmp	.LBB43_12
.LBB43_2:                               # %entry
	xorq	%rax, %rax
	cmpq	$58, %rcx
	jne	.LBB43_12
# BB#3:                                 # %if.then
	movq	8(%rbx), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$75, %rcx
	jne	.LBB43_12
# BB#4:                                 # %land.lhs.true
	movq	8(%rdx), %rcx
	movzbl	(%rcx), %esi
	cmpq	$68, %rsi
	jne	.LBB43_12
# BB#5:                                 # %land.lhs.true13
	movq	16(%rdx), %rdx
	movzbl	(%rdx), %edx
	cmpq	$54, %rdx
	jne	.LBB43_12
# BB#6:                                 # %if.then23
	movq	%rcx, (%r15)
	movq	8(%rbx), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	jmp	.LBB43_12
.LBB43_8:                               # %if.then62
	movq	%rdx, (%r15)
.LBB43_12:                              # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp497:
	.size	remove_constant_addition, .Ltmp497-remove_constant_addition
	.cfi_endproc

	.align	16, 0x90
	.type	check_store,@function
check_store:                            # @check_store
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp500:
	.cfi_def_cfa_offset 16
.Ltmp501:
	.cfi_offset %rbx, -16
	movzbl	(%rdi), %eax
	movq	%rdx, %rbx
	cmpq	$66, %rax
	jne	.LBB44_3
# BB#1:                                 # %land.lhs.true
	movq	(%rbx), %rax
	movq	8(%rdi), %rsi
	movq	%rax, %rdi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB44_3
# BB#2:                                 # %if.then
	movq	$1, %rax
	movl	%eax, 8(%rbx)
.LBB44_3:                               # %if.end
	popq	%rbx
	retq
.Ltmp502:
	.size	check_store, .Ltmp502-check_store
	.cfi_endproc

	.align	16, 0x90
	.type	rtx_equal_for_prefetch_p,@function
rtx_equal_for_prefetch_p:               # @rtx_equal_for_prefetch_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp509:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp510:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp511:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp512:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp513:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp514:
	.cfi_def_cfa_offset 64
.Ltmp515:
	.cfi_offset %rbx, -48
.Ltmp516:
	.cfi_offset %r12, -40
.Ltmp517:
	.cfi_offset %r13, -32
.Ltmp518:
	.cfi_offset %r14, -24
.Ltmp519:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %rbx
	cmpq	%r15, %rbx
	movq	$1, %rcx
	je	.LBB45_35
# BB#1:                                 # %if.end
	movzbl	(%rbx), %eax
	movzbl	(%r15), %ecx
	cmpq	%rcx, %rax
	jne	.LBB45_34
# BB#2:                                 # %if.end5
	movzbl	rtx_class(%rax), %ecx
	cmpq	$99, %rcx
	jne	.LBB45_9
# BB#3:                                 # %if.then10
	movq	8(%rbx), %rdi
	movq	8(%r15), %rsi
	callq	rtx_equal_for_prefetch_p
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB45_5
# BB#4:                                 # %land.lhs.true
	movq	16(%rbx), %rdi
	movq	16(%r15), %rsi
	callq	rtx_equal_for_prefetch_p
	testq	%r12, %rax
	movq	$1, %rcx
	jne	.LBB45_35
.LBB45_5:
	leaq	16(%r15), %rax
	movq	8(%rbx), %rdi
	movq	(%rax), %rsi
	callq	rtx_equal_for_prefetch_p
	xorq	%rcx, %rcx
	testq	%r12, %rax
	je	.LBB45_35
# BB#6:                                 # %land.rhs
	movq	16(%rbx), %rdi
	movq	8(%r15), %rsi
	callq	rtx_equal_for_prefetch_p
	testq	%r12, %rax
	je	.LBB45_7
# BB#8:                                 # %land.rhs
	movabsq	$1, %rcx
	jmp	.LBB45_35
.LBB45_9:                               # %if.end39
	movzbl	rtx_length(%rax), %r14d
	testq	%r14, %r14
	je	.LBB45_10
# BB#11:                                # %for.body.lr.ph
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rbx
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB45_12:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB45_31 Depth 2
	movq	%r14, %r12
	movq	(%rsp), %rax            # 8-byte Reload
	movsbq	-1(%rax,%r12), %rax
	leaq	-1(%r12), %r14
	cmpq	$68, %rax
	jg	.LBB45_16
# BB#13:                                # %for.body
                                        #   in Loop: Header=BB45_12 Depth=1
	andq	%r13, %rax
	cmpq	$48, %rax
	je	.LBB45_14
	jmp	.LBB45_36
	.align	16, 0x90
.LBB45_16:                              # %for.body
                                        #   in Loop: Header=BB45_12 Depth=1
	cmpq	$100, %rax
	jg	.LBB45_20
# BB#17:                                # %for.body
                                        #   in Loop: Header=BB45_12 Depth=1
	andq	%r13, %rax
	cmpq	$69, %rax
	jne	.LBB45_36
# BB#18:                                # %sw.bb73
                                        #   in Loop: Header=BB45_12 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax,%r12,8), %rax
	movq	(%rbx,%r12,8), %rdx
	movl	(%rax), %esi
	movl	(%rdx), %ecx
	cmpq	%rcx, %rsi
	jne	.LBB45_34
# BB#19:                                # %for.cond86.preheader
                                        #   in Loop: Header=BB45_12 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$1, %r15
	testq	%rsi, %rsi
	jle	.LBB45_14
	jmp	.LBB45_31
	.align	16, 0x90
.LBB45_30:                              # %for.cond86.for.body94_crit_edge
                                        #   in Loop: Header=BB45_31 Depth=2
	movq	(%rbx,%r12,8), %rdx
	incq	%r15
.LBB45_31:                              # %for.body94
                                        #   Parent Loop BB45_12 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r15,8), %rdi
	movq	(%rdx,%r15,8), %rsi
	callq	rtx_equal_for_prefetch_p
	testq	%r13, %rax
	je	.LBB45_34
# BB#29:                                # %for.cond86
                                        #   in Loop: Header=BB45_31 Depth=2
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax,%r12,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB45_30
	jmp	.LBB45_14
	.align	16, 0x90
.LBB45_20:                              # %for.body
                                        #   in Loop: Header=BB45_12 Depth=1
	cmpq	$114, %rax
	jg	.LBB45_25
# BB#21:                                # %for.body
                                        #   in Loop: Header=BB45_12 Depth=1
	andq	%r13, %rax
	cmpq	$101, %rax
	jne	.LBB45_22
# BB#32:                                # %sw.bb113
                                        #   in Loop: Header=BB45_12 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax,%r12,8), %rdi
	movq	(%rbx,%r12,8), %rsi
	callq	rtx_equal_for_prefetch_p
	testq	%r13, %rax
	jne	.LBB45_14
	jmp	.LBB45_34
.LBB45_25:                              # %for.body
                                        #   in Loop: Header=BB45_12 Depth=1
	andq	%r13, %rax
	cmpq	$115, %rax
	je	.LBB45_33
# BB#26:                                # %for.body
                                        #   in Loop: Header=BB45_12 Depth=1
	cmpq	$117, %rax
	je	.LBB45_14
# BB#27:                                # %for.body
                                        #   in Loop: Header=BB45_12 Depth=1
	cmpq	$119, %rax
	jne	.LBB45_36
# BB#28:                                # %sw.bb
                                        #   in Loop: Header=BB45_12 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax,%r12,8), %rax
	movq	(%rbx,%r12,8), %rcx
	jmp	.LBB45_24
.LBB45_22:                              # %for.body
                                        #   in Loop: Header=BB45_12 Depth=1
	cmpq	$105, %rax
	jne	.LBB45_36
# BB#23:                                # %sw.bb61
                                        #   in Loop: Header=BB45_12 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	(%rax,%r12,8), %eax
	movl	(%rbx,%r12,8), %ecx
.LBB45_24:                              # %sw.bb61
                                        #   in Loop: Header=BB45_12 Depth=1
	cmpq	%rcx, %rax
	je	.LBB45_14
	jmp	.LBB45_34
.LBB45_33:                              # %sw.bb127
                                        #   in Loop: Header=BB45_12 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax,%r12,8), %rdi
	movq	(%rbx,%r12,8), %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB45_34
	.align	16, 0x90
.LBB45_14:                              # %for.cond.backedge
                                        #   in Loop: Header=BB45_12 Depth=1
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB45_12
# BB#15:
	movq	$1, %rcx
	jmp	.LBB45_35
.LBB45_34:
	xorq	%rcx, %rcx
.LBB45_35:                              # %return
	movq	%rcx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB45_10:
	movq	$1, %rcx
	jmp	.LBB45_35
.LBB45_7:
	movabsq	$0, %rcx
	jmp	.LBB45_35
.LBB45_36:                              # %sw.default
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.rtx_equal_for_prefetch_p, %rcx
	movq	$3688, %rsi             # imm = 0xE68
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp520:
	.size	rtx_equal_for_prefetch_p, .Ltmp520-rtx_equal_for_prefetch_p
	.cfi_endproc

	.align	16, 0x90
	.type	check_insn_for_givs,@function
check_insn_for_givs:                    # @check_insn_for_givs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp527:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp528:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp529:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp530:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp531:
	.cfi_def_cfa_offset 48
	subq	$160, %rsp
.Ltmp532:
	.cfi_def_cfa_offset 208
.Ltmp533:
	.cfi_offset %rbx, -48
.Ltmp534:
	.cfi_offset %r12, -40
.Ltmp535:
	.cfi_offset %r13, -32
.Ltmp536:
	.cfi_offset %r14, -24
.Ltmp537:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movzbl	(%rbx), %eax
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rdi, 88(%rsp)          # 8-byte Spill
	cmpq	$32, %rax
	jne	.LBB46_30
# BB#1:                                 # %entry
	movzbl	rtx_class+32(%rip), %eax
	andq	$255, %rax
	cmpq	$105, %rax
	jne	.LBB46_30
# BB#2:                                 # %cond.true
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	160(%rdx), %r14
	cmpq	$47, %rcx
	je	.LBB46_4
# BB#3:                                 # %cond.false
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB46_4:                               # %cond.end19
	testq	%rax, %rax
	je	.LBB46_30
# BB#5:                                 # %land.lhs.true21
	movq	8(%rax), %r13
	movzbl	(%r13), %ecx
	cmpq	$61, %rcx
	jne	.LBB46_30
# BB#6:                                 # %land.lhs.true29
	movl	8(%r13), %ecx
	movq	192(%r14), %rdx
	leaq	(%rcx,%rcx,2), %rsi
	movzbl	16(%rdx,%rsi,8), %edx
	testq	%rdx, %rdx
	jne	.LBB46_30
# BB#7:                                 # %if.then
	cmpq	$53, %rcx
	jb	.LBB46_63
# BB#8:                                 # %if.end
	movq	%r14, 80(%rsp)          # 8-byte Spill
	movq	16(%rax), %rsi
	leaq	108(%rsp), %r14
	movq	%r14, 8(%rsp)
	movq	$0, 16(%rsp)
	leaq	136(%rsp), %rdx
	leaq	128(%rsp), %rcx
	leaq	120(%rsp), %r8
	leaq	112(%rsp), %r9
	movq	$0, (%rsp)
	movq	88(%rsp), %rdi          # 8-byte Reload
	callq	general_induction_var
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB46_11
# BB#9:                                 # %lor.lhs.false
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB46_30
# BB#10:                                # %land.lhs.true54
	movq	8(%rax), %rsi
	movq	%r14, 8(%rsp)
	movq	$0, 16(%rsp)
	leaq	136(%rsp), %rdx
	leaq	128(%rsp), %rcx
	leaq	120(%rsp), %r8
	leaq	112(%rsp), %r9
	movq	$0, (%rsp)
	movq	88(%rsp), %rdi          # 8-byte Reload
	callq	general_induction_var
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB46_30
.LBB46_11:                              # %land.lhs.true60
	movl	8(%r13), %eax
	movl	max_reg_before_loop(%rip), %ecx
	cmpq	%rcx, %rax
	movq	80(%rsp), %rdx          # 8-byte Reload
	jae	.LBB46_30
# BB#12:                                # %land.lhs.true66
	movq	136(%rsp), %rcx
	cmpq	%rcx, %r13
	je	.LBB46_30
# BB#13:                                # %land.lhs.true69
	movq	192(%rdx), %rdx
	leaq	(%rax,%rax,2), %rax
	movl	4(%rdx,%rax,8), %eax
	cmpq	$1, %rax
	je	.LBB46_15
# BB#14:                                # %lor.lhs.false78
	movl	108(%rsp), %esi
	leaq	96(%rsp), %rax
	movq	%rax, 16(%rsp)
	leaq	112(%rsp), %rax
	movq	%rax, 8(%rsp)
	leaq	128(%rsp), %r9
	leaq	120(%rsp), %rax
	movq	%rax, (%rsp)
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	%r13, %r8
	callq	consec_sets_giv
	movq	72(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	movl	%eax, 108(%rsp)
	je	.LBB46_30
.LBB46_15:                              # %if.then81
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	$168, %rdi
	callq	xmalloc
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	$6, %rsi
	xorq	%rdx, %rdx
	xorq	%r14, %r14
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB46_16
# BB#17:                                # %if.then85
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	8(%rax), %rax
	cmpq	%rbx, %rax
	je	.LBB46_26
# BB#18:
	movabsq	$0, %rcx
	movabsq	$1, %rdx
.LBB46_19:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %esi
	cmpq	$32, %rsi
	jne	.LBB46_20
# BB#22:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB46_19 Depth=1
	movq	32(%rax), %rsi
	movl	(%rsi), %esi
	andq	$65534, %rsi            # imm = 0xFFFE
	cmpq	$48, %rsi
	movq	%rcx, %rsi
	je	.LBB46_24
# BB#23:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB46_19 Depth=1
	movq	%rdx, %rsi
.LBB46_24:                              # %land.lhs.true.i
                                        #   in Loop: Header=BB46_19 Depth=1
	addq	%rsi, %r14
	jmp	.LBB46_25
.LBB46_20:                              # %for.body.i
                                        #   in Loop: Header=BB46_19 Depth=1
	cmpq	$34, %rsi
	jne	.LBB46_25
# BB#21:                                # %if.then.i
                                        #   in Loop: Header=BB46_19 Depth=1
	addq	$10, %r14
.LBB46_25:                              # %for.inc.i
                                        #   in Loop: Header=BB46_19 Depth=1
	movq	24(%rax), %rax
	cmpq	%rbx, %rax
	jne	.LBB46_19
.LBB46_26:                              # %libcall_benefit.exit
	movl	108(%rsp), %eax
	addq	%r14, %rax
	movl	%eax, 108(%rsp)
	jmp	.LBB46_27
.LBB46_16:                              # %if.then81.if.end87_crit_edge
	movl	108(%rsp), %eax
.LBB46_27:                              # %if.end87
	movq	80(%rsp), %rdx          # 8-byte Reload
	movq	72(%rsp), %rcx          # 8-byte Reload
	movl	8(%r13), %esi
	movq	192(%rdx), %rdx
	leaq	(%rsi,%rsi,2), %rsi
	movl	4(%rdx,%rsi,8), %edx
	cmpq	$1, %rdx
	je	.LBB46_29
# BB#28:                                # %select.mid
	movq	96(%rsp), %rbx
.LBB46_29:                              # %select.end
	movq	120(%rsp), %r9
	movq	128(%rsp), %rsi
	movq	112(%rsp), %rdx
	movq	%r15, 40(%rsp)
	movq	%r12, 32(%rsp)
	movq	%rax, 16(%rsp)
	movq	%rdx, 8(%rsp)
	movq	%rsi, (%rsp)
	movq	$0, 48(%rsp)
	movq	$1, 24(%rsp)
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r13, %r8
	callq	record_giv
.LBB46_30:                              # %if.end100
	movl	(%rbx), %ecx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$32, %rax
	jne	.LBB46_32
# BB#31:                                # %if.then105
	movq	32(%rbx), %rsi
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r12, %rcx
	movq	%r15, %r8
	callq	find_mem_givs
	movl	(%rbx), %ecx
.LBB46_32:                              # %if.end109
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$36, %rcx
	ja	.LBB46_63
# BB#33:                                # %if.end109
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$81604378624, %rcx      # imm = 0x1300000000
	testq	%rcx, %rax
	je	.LBB46_63
# BB#34:                                # %if.then124
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	160(%rax), %rax
	movq	224(%rax), %r13
	testq	%r13, %r13
	je	.LBB46_63
# BB#35:
	leaq	156(%rsp), %r12
	.align	16, 0x90
.LBB46_36:                              # %for.body.i164
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB46_37 Depth 2
                                        #       Child Loop BB46_42 Depth 3
	movq	8(%r13), %r14
	jmp	.LBB46_37
	.align	16, 0x90
.LBB46_61:                              # %for.inc97.i
                                        #   in Loop: Header=BB46_37 Depth=2
	movq	128(%r14), %r14
.LBB46_37:                              # %for.body.i164
                                        #   Parent Loop BB46_36 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB46_42 Depth 3
	testq	%r14, %r14
	je	.LBB46_62
# BB#38:                                # %for.body5.i
                                        #   in Loop: Header=BB46_37 Depth=2
	movzbl	(%rbx), %eax
	cmpq	$33, %rax
	je	.LBB46_41
# BB#39:                                # %for.body5.i
                                        #   in Loop: Header=BB46_37 Depth=2
	cmpq	$36, %rax
	je	.LBB46_41
# BB#40:                                # %lor.lhs.false9.i
                                        #   in Loop: Header=BB46_37 Depth=2
	movq	(%r14), %rax
	cmpq	%rbx, %rax
	jne	.LBB46_61
	.align	16, 0x90
.LBB46_41:                              # %if.then.i167
                                        #   in Loop: Header=BB46_37 Depth=2
	movq	24(%r13), %r15
	jmp	.LBB46_42
	.align	16, 0x90
.LBB46_60:                              # %for.inc.i169
                                        #   in Loop: Header=BB46_42 Depth=3
	movq	128(%r15), %r15
.LBB46_42:                              # %if.then.i167
                                        #   Parent Loop BB46_36 Depth=1
                                        #     Parent Loop BB46_37 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r15, %r15
	je	.LBB46_61
# BB#43:                                # %for.body14.i
                                        #   in Loop: Header=BB46_42 Depth=3
	movzbl	100(%r15), %eax
	testq	$64, %rax
	jne	.LBB46_60
# BB#44:                                # %if.end.i
                                        #   in Loop: Header=BB46_42 Depth=3
	movzbl	(%rbx), %edx
	cmpq	$36, %rdx
	jne	.LBB46_46
# BB#45:                                # %if.end.i
                                        #   in Loop: Header=BB46_42 Depth=3
	movq	%rax, %rcx
	andq	$8, %rcx
	testq	$65535, %rcx            # imm = 0xFFFF
	je	.LBB46_59
.LBB46_46:                              # %if.else.i
                                        #   in Loop: Header=BB46_42 Depth=3
	movq	64(%r15), %rcx
	movq	const_int_rtx+512(%rip), %rsi
	cmpq	%rsi, %rcx
	je	.LBB46_60
# BB#47:                                # %if.else.i
                                        #   in Loop: Header=BB46_42 Depth=3
	movq	%rax, %rsi
	andq	$1, %rsi
	jne	.LBB46_60
# BB#48:                                # %if.else38.i
                                        #   in Loop: Header=BB46_42 Depth=3
	movq	(%r14), %rsi
	cmpq	%rbx, %rsi
	jne	.LBB46_55
# BB#49:                                # %if.then41.i
                                        #   in Loop: Header=BB46_42 Depth=3
	movq	64(%r14), %rax
	movq	const_int_rtx+520(%rip), %rdx
	cmpq	%rdx, %rax
	jne	.LBB46_54
# BB#50:                                # %if.end47.i
                                        #   in Loop: Header=BB46_42 Depth=3
	movq	72(%r14), %rdx
	movl	48(%r15), %esi
	movq	$78, %rdi
	callq	gen_rtx_fmt_ee
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	leaq	144(%rsp), %rdx
	movq	%r12, %rcx
	callq	simplify_giv_expr
	testq	%rax, %rax
	je	.LBB46_54
# BB#51:                                # %land.lhs.true49.i
                                        #   in Loop: Header=BB46_42 Depth=3
	movq	112(%r15), %rcx
	testq	%rcx, %rcx
	je	.LBB46_53
# BB#52:                                # %if.end56.i
                                        #   in Loop: Header=BB46_42 Depth=3
	movl	48(%r15), %esi
	movq	$75, %rdi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	leaq	144(%rsp), %rdx
	movq	%r12, %rcx
	callq	simplify_giv_expr
	testq	%rax, %rax
	jne	.LBB46_53
.LBB46_54:                              # %if.else60.i
                                        #   in Loop: Header=BB46_42 Depth=3
	movzbl	100(%r15), %eax
	jmp	.LBB46_59
.LBB46_55:                              # %if.else66.i
                                        #   in Loop: Header=BB46_42 Depth=3
	cmpq	$36, %rdx
	jne	.LBB46_57
# BB#56:                                # %land.lhs.true70.i
                                        #   in Loop: Header=BB46_42 Depth=3
	movzbl	100(%r14), %ecx
	testq	$8, %rcx
	je	.LBB46_59
	jmp	.LBB46_60
.LBB46_57:                              # %lor.lhs.false77.i
                                        #   in Loop: Header=BB46_42 Depth=3
	cmpq	$33, %rdx
	jne	.LBB46_60
# BB#58:                                # %land.lhs.true81.i
                                        #   in Loop: Header=BB46_42 Depth=3
	movzbl	100(%r14), %ecx
	testq	$32, %rcx
	je	.LBB46_60
.LBB46_59:                              # %if.then87.i
                                        #   in Loop: Header=BB46_42 Depth=3
	orq	$64, %rax
	movw	%ax, 100(%r15)
	jmp	.LBB46_60
.LBB46_53:                              # %if.then58.i
                                        #   in Loop: Header=BB46_42 Depth=3
	movq	%rax, 112(%r15)
	movq	128(%r15), %r15
	jmp	.LBB46_42
	.align	16, 0x90
.LBB46_62:                              # %for.inc100.i
                                        #   in Loop: Header=BB46_36 Depth=1
	movq	64(%r13), %r13
	testq	%r13, %r13
	jne	.LBB46_36
.LBB46_63:                              # %return
	movq	%rbx, %rax
	addq	$160, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp538:
	.size	check_insn_for_givs, .Ltmp538-check_insn_for_givs
	.cfi_endproc

	.align	16, 0x90
	.type	general_induction_var,@function
general_induction_var:                  # @general_induction_var
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp545:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp546:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp547:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp548:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp549:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp550:
	.cfi_def_cfa_offset 80
.Ltmp551:
	.cfi_offset %rbx, -48
.Ltmp552:
	.cfi_offset %r12, -40
.Ltmp553:
	.cfi_offset %r13, -32
.Ltmp554:
	.cfi_offset %r14, -24
.Ltmp555:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	160(%r15), %r13
	movq	%r9, %r12
	movq	%r8, 24(%rsp)           # 8-byte Spill
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rsi, %rbx
                                        # kill: RDI<def> R15<kill>
                                        # kill: RSI<def> RBX<kill>
	callq	loop_invariant_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	xorq	%r14, %r14
	cmpq	$1, %rax
	je	.LBB47_27
# BB#1:                                 # %if.end
	movq	88(%rsp), %rax
	xorq	%r14, %r14
	movl	%r14d, (%rax)
	movq	$0, (%r12)
	movq	%r15, %rdi
	movq	%rax, %r15
	movq	%rbx, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	callq	simplify_giv_expr
	testq	%rax, %rax
	je	.LBB47_27
# BB#2:                                 # %if.end5
	movq	96(%rsp), %rsi
	movzbl	(%rax), %edx
	movq	80(%rsp), %r8
	cmpq	$53, %rdx
	jg	.LBB47_5
# BB#3:                                 # %if.end5
	cmpq	$48, %rdx
	je	.LBB47_7
	jmp	.LBB47_4
.LBB47_5:                               # %if.end5
	cmpq	$60, %rdx
	jg	.LBB47_8
# BB#6:                                 # %if.end5
	cmpq	$54, %rdx
	jne	.LBB47_4
.LBB47_7:                               # %sw.bb
	movq	224(%r13), %rdx
	movq	8(%rdx), %rdx
	movq	32(%rdx), %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rdx, (%rcx)
	movq	const_int_rtx+512(%rip), %rdx
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rdx, (%rdi)
	movq	16(%rsp), %r9           # 8-byte Reload
	jmp	.LBB47_19
.LBB47_8:                               # %if.end5
	cmpq	$61, %rdx
	je	.LBB47_12
# BB#9:                                 # %if.end5
	cmpq	$75, %rdx
	jne	.LBB47_10
# BB#15:                                # %sw.bb7
	movq	8(%rax), %rdx
	movzbl	(%rdx), %edi
	cmpq	$78, %rdi
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	16(%rsp), %r9           # 8-byte Reload
	jne	.LBB47_17
# BB#16:                                # %if.then11
	movq	8(%rdx), %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rdx, (%rcx)
	movq	8(%rax), %rdx
	addq	$16, %rdx
	jmp	.LBB47_18
.LBB47_12:                              # %sw.bb6
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rax, (%rcx)
	movq	const_int_rtx+520(%rip), %rax
	jmp	.LBB47_13
.LBB47_10:                              # %if.end5
	cmpq	$78, %rdx
	jne	.LBB47_4
# BB#11:                                # %sw.bb31
	movq	8(%rax), %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rdx, (%rcx)
	movq	16(%rax), %rax
.LBB47_13:                              # %sw.epilog
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rax, (%rdi)
	movq	const_int_rtx+512(%rip), %rax
	movq	16(%rsp), %r9           # 8-byte Reload
	jmp	.LBB47_19
.LBB47_17:                              # %if.else
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rdx, (%rcx)
	movabsq	$const_int_rtx+520, %rdx
.LBB47_18:                              # %if.end27
	movq	(%rdx), %rdx
	movq	%rdx, (%rdi)
	movq	16(%rax), %rax
.LBB47_19:                              # %sw.epilog
	movq	%rax, (%r9)
	movzbl	(%rax), %edx
	cmpq	$48, %rdx
	jne	.LBB47_21
# BB#20:                                # %if.then41
	movq	8(%rax), %rax
	movq	%rax, (%r9)
.LBB47_21:                              # %if.end45
	movq	(%rdi), %rax
	movzbl	(%rax), %edx
	cmpq	$48, %rdx
	jne	.LBB47_23
# BB#22:                                # %if.then49
	movq	8(%rax), %rax
	movq	%rax, (%rdi)
.LBB47_23:                              # %if.end53
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r8
	je	.LBB47_25
# BB#24:                                # %if.then54
	movq	%rbx, %rdi
	callq	address_cost
	movl	reg_address_cost(%rip), %edx
	movl	(%r15), %ecx
	subq	%rdx, %rax
	jmp	.LBB47_26
.LBB47_25:                              # %if.else56
	movq	$47, %rsi
	movq	%rbx, %rdi
	callq	rtx_cost
	movl	(%r15), %ecx
.LBB47_26:                              # %if.end59
	addq	%rax, %rcx
	movl	%ecx, (%r15)
	movq	$1, %r14
.LBB47_27:                              # %return
	movq	%r14, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB47_4:                               # %sw.default
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.general_induction_var, %rcx
	movq	$6350, %rsi             # imm = 0x18CE
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp556:
	.size	general_induction_var, .Ltmp556-general_induction_var
	.cfi_endproc

	.align	16, 0x90
	.type	consec_sets_giv,@function
consec_sets_giv:                        # @consec_sets_giv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp560:
	.cfi_def_cfa_offset 16
.Ltmp561:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp562:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$56, %rsp
.Ltmp563:
	.cfi_offset %rbx, -56
.Ltmp564:
	.cfi_offset %r12, -48
.Ltmp565:
	.cfi_offset %r13, -40
.Ltmp566:
	.cfi_offset %r14, -32
.Ltmp567:
	.cfi_offset %r15, -24
	movq	%rcx, %r10
	movq	%r10, -48(%rbp)
	movq	160(%rdi), %r12
	movq	%r8, %r15
	movl	8(%r15), %r8d
	movq	208(%r12), %rax
	movq	$4, %rcx
	movq	%r8, %rbx
	shlq	%cl, %rbx
	movl	(%rax,%rbx), %ecx
	movq	%rdx, %r13
	xorq	%rax, %rax
	testq	%rcx, %rcx
	jne	.LBB48_46
# BB#1:                                 # %if.end
	movq	%rdi, -80(%rbp)         # 8-byte Spill
	movq	16(%rbp), %rcx
	movq	%rsp, %rax
	leaq	-176(%rax), %rdi
	movq	%rdi, -72(%rbp)         # 8-byte Spill
	movq	%rdi, %rsp
	movq	%r10, -160(%rax)
	movq	(%rcx), %rcx
	movq	%rcx, -112(%rax)
	movq	(%r9), %rcx
	movq	%rcx, -104(%rax)
	movl	%esi, -96(%rax)
	xorq	%rbx, %rbx
	movw	%bx, -76(%rax)
	movq	$0, -56(%rax)
	movq	$0, -64(%rax)
	movq	208(%r12), %rax
	movq	$4, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$3, %rcx
	movl	%ecx, (%rax,%rdx)
	movq	%rdi, 8(%rax,%rdx)
	movq	192(%r12), %rax
	leaq	(%r8,%r8,2), %rcx
	movl	4(%rax,%rcx,8), %eax
	decq	%rax
	movq	%rax, -64(%rbp)         # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB48_31
# BB#2:
	movq	%r9, %rbx
	jmp	.LBB48_3
	.align	16, 0x90
.LBB48_42:                              # %land.lhs.true150
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB48_29
	jmp	.LBB48_45
	.align	16, 0x90
.LBB48_43:                              # %land.lhs.true150
                                        #   in Loop: Header=BB48_3 Depth=1
	cmpq	$134, %rcx
	je	.LBB48_29
# BB#44:                                # %land.lhs.true150
                                        #   in Loop: Header=BB48_3 Depth=1
	cmpq	$140, %rcx
	jne	.LBB48_45
	.align	16, 0x90
.LBB48_29:                              # %while.cond.backedge
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	$32, %rcx
	movq	-64(%rbp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB48_3
	jmp	.LBB48_30
.LBB48_18:                              # %if.then101
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	$6, %rsi
	xorq	%rdx, %rdx
	xorq	%r14, %r14
	movq	%r13, %rdi
	callq	find_reg_note
	movq	8(%rax), %rax
	jmp	.LBB48_19
.LBB48_26:                              # %for.inc.i
                                        #   in Loop: Header=BB48_19 Depth=2
	movq	24(%rax), %rax
.LBB48_19:                              # %if.then101
                                        #   Parent Loop BB48_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r13, %rax
	je	.LBB48_27
# BB#20:                                # %for.body.i
                                        #   in Loop: Header=BB48_19 Depth=2
	movzbl	(%rax), %ecx
	cmpq	$32, %rcx
	jne	.LBB48_21
# BB#23:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB48_19 Depth=2
	movq	32(%rax), %rcx
	movl	(%rcx), %ecx
	andq	$65534, %rcx            # imm = 0xFFFE
	cmpq	$48, %rcx
	movabsq	$0, %rcx
	je	.LBB48_25
# BB#24:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB48_19 Depth=2
	movabsq	$1, %rcx
.LBB48_25:                              # %land.lhs.true.i
                                        #   in Loop: Header=BB48_19 Depth=2
	addq	%rcx, %r14
	jmp	.LBB48_26
.LBB48_21:                              # %for.body.i
                                        #   in Loop: Header=BB48_19 Depth=2
	cmpq	$34, %rcx
	jne	.LBB48_26
# BB#22:                                # %if.then.i
                                        #   in Loop: Header=BB48_19 Depth=2
	addq	$10, %r14
	movq	24(%rax), %rax
	jmp	.LBB48_19
.LBB48_27:                              # %libcall_benefit.exit
                                        #   in Loop: Header=BB48_3 Depth=1
	movl	-52(%rbp), %eax
	addq	%r14, %rax
	movl	%eax, -52(%rbp)
.LBB48_28:                              # %if.end103
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	16(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	-72(%rbp), %rdx         # 8-byte Reload
	movq	%rcx, 64(%rdx)
	movq	(%rbx), %rcx
	movq	%rcx, 72(%rdx)
	movl	80(%rdx), %ecx
	decq	-64(%rbp)               # 8-byte Folded Spill
	addq	%rax, %rcx
	movl	%ecx, 80(%rdx)
	jmp	.LBB48_29
	.align	16, 0x90
.LBB48_3:                               # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB48_19 Depth 2
	movq	24(%r13), %r13
	movzbl	(%r13), %r14d
	cmpq	$32, %r14
	jne	.LBB48_32
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	$7, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB48_6
# BB#5:                                 # %if.then34
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	8(%rax), %r13
.LBB48_6:                               # %land.lhs.true40
                                        #   in Loop: Header=BB48_3 Depth=1
	movl	(%r13), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB48_35
# BB#7:                                 # %cond.true
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	32(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB48_9
# BB#8:                                 # %cond.false
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB48_9:                               # %cond.end63
                                        #   in Loop: Header=BB48_3 Depth=1
	testq	%rax, %rax
	je	.LBB48_32
# BB#10:                                # %land.lhs.true66
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	8(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB48_32
# BB#11:                                # %land.lhs.true66
                                        #   in Loop: Header=BB48_3 Depth=1
	cmpq	%r15, %rcx
	jne	.LBB48_32
# BB#12:                                # %land.lhs.true80
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	16(%rax), %rsi
	subq	$32, %rsp
	leaq	-52(%rbp), %rax
	movq	%rax, 8(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, (%rsp)
	movq	-80(%rbp), %rdi         # 8-byte Reload
	leaq	-48(%rbp), %rdx
	movq	%rbx, %rcx
	movq	16(%rbp), %r8
	movq	24(%rbp), %r9
	callq	general_induction_var
	addq	$32, %rsp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB48_15
# BB#13:                                # %lor.lhs.false
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB48_32
# BB#14:                                # %land.lhs.true88
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	8(%rax), %rsi
	subq	$32, %rsp
	leaq	-52(%rbp), %rax
	movq	%rax, 8(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, (%rsp)
	movq	-80(%rbp), %rdi         # 8-byte Reload
	leaq	-48(%rbp), %rdx
	movq	%rbx, %rcx
	movq	16(%rbp), %r8
	movq	24(%rbp), %r9
	callq	general_induction_var
	addq	$32, %rsp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB48_32
.LBB48_15:                              # %land.lhs.true94
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	-48(%rbp), %rax
	movq	-72(%rbp), %rcx         # 8-byte Reload
	movq	16(%rcx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB48_32
# BB#16:                                # %if.then98
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB48_18
# BB#17:                                # %if.then98.if.end103_crit_edge
                                        #   in Loop: Header=BB48_3 Depth=1
	movl	-52(%rbp), %eax
	jmp	.LBB48_28
	.align	16, 0x90
.LBB48_32:                              # %if.else
                                        #   in Loop: Header=BB48_3 Depth=1
	cmpq	$37, %r14
	je	.LBB48_29
# BB#33:                                # %if.then110
                                        #   in Loop: Header=BB48_3 Depth=1
	cmpq	$32, %r14
	jne	.LBB48_45
# BB#34:                                # %if.then110.land.lhs.true113_crit_edge
                                        #   in Loop: Header=BB48_3 Depth=1
	movl	(%r13), %eax
.LBB48_35:                              # %land.lhs.true113
                                        #   in Loop: Header=BB48_3 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB48_45
# BB#36:                                # %cond.true121
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	32(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB48_38
# BB#37:                                # %cond.false133
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB48_38:                              # %cond.end141
                                        #   in Loop: Header=BB48_3 Depth=1
	testq	%rax, %rax
	je	.LBB48_45
# BB#39:                                # %land.lhs.true144
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	8(%rax), %rcx
	cmpq	%r15, %rcx
	je	.LBB48_45
# BB#40:                                # %land.lhs.true150
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	16(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$133, %rcx
	jg	.LBB48_43
# BB#41:                                # %land.lhs.true150
                                        #   in Loop: Header=BB48_3 Depth=1
	addq	$-54, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	jbe	.LBB48_42
.LBB48_45:                              # %if.end215
	movl	8(%r15), %edx
	movq	208(%r12), %rsi
	movq	$4, %rcx
	shlq	%cl, %rdx
	xorq	%rax, %rax
	movl	%eax, (%rsi,%rdx)
	jmp	.LBB48_46
.LBB48_30:                              # %while.cond.while.end_crit_edge
	movl	8(%r15), %r8d
	movq	-72(%rbp), %rax         # 8-byte Reload
	movl	80(%rax), %esi
	xorq	%rbx, %rbx
.LBB48_31:                              # %while.end
	movq	32(%rbp), %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%r8, %rdx
	movq	208(%r12), %rdi
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	%ebx, (%rdi,%rdx)
	movq	%r13, (%rax)
	movq	%rsi, %rax
.LBB48_46:                              # %return
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp568:
	.size	consec_sets_giv, .Ltmp568-consec_sets_giv
	.cfi_endproc

	.align	16, 0x90
	.type	record_giv,@function
record_giv:                             # @record_giv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp575:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp576:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp577:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp578:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp579:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp580:
	.cfi_def_cfa_offset 96
.Ltmp581:
	.cfi_offset %rbx, -48
.Ltmp582:
	.cfi_offset %r12, -40
.Ltmp583:
	.cfi_offset %r13, -32
.Ltmp584:
	.cfi_offset %r14, -24
.Ltmp585:
	.cfi_offset %r15, -16
	movzbl	(%rdx), %eax
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movzbl	rtx_class(%rax), %ebx
	movq	160(%rdi), %r15
	movq	%r9, 32(%rsp)           # 8-byte Spill
	movq	%r8, %r14
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	%rsi, %r12
	xorq	%rax, %rax
	cmpq	$105, %rbx
	jne	.LBB49_3
# BB#1:                                 # %cond.true
	movq	32(%rdx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB49_3
# BB#2:                                 # %cond.false
	movq	%rdx, %rdi
	movq	%rax, %rsi
	movq	%rdx, %rbx
	callq	single_set_2
	movq	%rbx, %rdx
.LBB49_3:                               # %cond.end16
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rdx, %r13
	movq	96(%rsp), %rbx
	movq	%rbx, %rdi
	callq	simplify_rtx
	testq	%rax, %rax
	je	.LBB49_4
# BB#5:                                 # %land.lhs.true
	movzbl	(%rbx), %ecx
	cmpq	$78, %rcx
	movq	%r13, %r10
	jne	.LBB49_7
# BB#6:                                 # %land.lhs.true23
	movzbl	(%rax), %ecx
	cmpq	$87, %rcx
	je	.LBB49_8
.LBB49_7:                               # %if.then
	movq	%rax, %rbx
	jmp	.LBB49_8
.LBB49_4:
	movq	%r13, %r10
.LBB49_8:                               # %if.end
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	144(%rsp), %rax
	movq	136(%rsp), %rsi
	movq	128(%rsp), %r8
	movq	120(%rsp), %rdx
	movq	112(%rsp), %r9
	movq	104(%rsp), %rcx
	addq	$208, %r15
	movq	%r15, %r11
	movq	%r10, (%r12)
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, 16(%r12)
	movl	%edx, 24(%r12)
	movq	%r14, 32(%r12)
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, 64(%r12)
	movq	%rbx, 72(%r12)
	movq	%rcx, 120(%r12)
	movl	%r9d, 80(%r12)
	movq	%rax, 40(%r12)
	movzbl	100(%r12), %edi
	movq	$5, %rcx
	shlq	%cl, %rsi
	andq	$32, %rsi
	andq	$-225, %rdi
	orq	%rsi, %rdi
	movq	%rdi, %rcx
	andq	$63547, %rcx            # imm = 0xF83B
	xorq	%rsi, %rsi
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rdx
	movl	%esi, 96(%r12)
	movq	$0, 112(%r12)
	movq	$0, 136(%r12)
	movq	$0, 8(%r12)
	movq	$0, 88(%r12)
	movq	$0, 152(%r12)
	movw	%cx, 100(%r12)
	je	.LBB49_9
# BB#10:                                # %if.else
	testq	%r15, %r8
	jne	.LBB49_11
# BB#12:                                # %if.else
	movabsq	$1, %rsi
	jmp	.LBB49_13
.LBB49_9:                               # %if.then52
	andq	$-1997, %rdi            # imm = 0xFFFFFFFFFFFFF833
	orq	$8, %rdi
	jmp	.LBB49_14
.LBB49_11:
	movabsq	$0, %rsi
.LBB49_13:                              # %if.else
	movq	$3, %rcx
	shlq	%cl, %rsi
	andq	$-1997, %rdi            # imm = 0xFFFFFFFFFFFFF833
	orq	%rsi, %rdi
.LBB49_14:                              # %if.end64
	testq	%r15, %r8
	je	.LBB49_15
# BB#16:                                # %if.end64
	movabsq	$1, %rsi
	jmp	.LBB49_17
.LBB49_15:
	movabsq	$0, %rsi
.LBB49_17:                              # %if.end64
	movq	$4, %rcx
	shlq	%cl, %rsi
	andq	$-17, %rdi
	orq	%rsi, %rdi
	xorq	$16, %rdi
	testq	%r15, %rdx
	movw	%di, 100(%r12)
	je	.LBB49_18
# BB#19:                                # %if.else79
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	8(%rax), %rax
	movzbl	2(%rax), %eax
	movl	%eax, 48(%r12)
	movl	8(%r14), %eax
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movslq	4(%rax), %rcx
	movq	uid_luid(%rip), %rsi
	movslq	(%rax), %rax
	movl	(%rsi,%rcx,4), %ecx
	movl	(%rsi,%rax,4), %eax
	subq	%rax, %rcx
	movl	%ecx, 104(%r12)
	jne	.LBB49_21
# BB#20:                                # %if.then106
	orq	$4, %rdi
	movw	%di, 100(%r12)
.LBB49_21:                              # %if.end111
	movl	8(%r14), %eax
	movq	(%r11), %rsi
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	$3, %rcx
	movl	%ecx, (%rsi,%rax)
	movq	%r12, 8(%rsi,%rax)
	jmp	.LBB49_22
.LBB49_18:                              # %if.then76
	movq	(%rax), %rax
	movzbl	2(%rax), %eax
	movl	%eax, 48(%r12)
	movq	$1, %rax
	movl	%eax, 104(%r12)
.LBB49_22:                              # %if.end124
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %eax
	movq	(%r11), %rsi
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	8(%rsi,%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB49_27
# BB#23:                                # %if.then133
	movq	24(%rdi), %rax
	movq	%rax, 128(%r12)
	movq	%r12, 24(%rdi)
	movq	%rdx, %rax
	andq	%r15, %rax
	cmpq	$1, %rax
	jne	.LBB49_25
# BB#24:                                # %if.then137
	movl	16(%rdi), %eax
	incq	%rax
	movl	%eax, 16(%rdi)
.LBB49_25:                              # %if.end138
	movl	32(%rdi), %eax
	addq	%r9, %rax
	testq	%r15, %rdx
	movl	%eax, 32(%rdi)
	je	.LBB49_26
# BB#29:                                # %if.else147
	movl	8(%r14), %eax
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	(%rax), %ecx
	movl	8(%r10), %edx
	cmpq	%rdx, %rcx
	jne	.LBB49_39
# BB#30:                                # %land.lhs.true160
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	200(%rcx), %rcx
	movslq	8(%rcx), %rcx
	movslq	max_uid_for_loop(%rip), %rdx
	cmpq	%rdx, %rcx
	jge	.LBB49_71
# BB#31:                                # %cond.true176
	movq	uid_luid(%rip), %rdx
	movslq	4(%rax), %rax
	movslq	(%rdx,%rax,4), %rsi
	movslq	(%rdx,%rcx,4), %rcx
	cmpq	%rcx, %rsi
	jge	.LBB49_39
# BB#32:                                # %land.lhs.true188
	testq	%r15, %r8
	je	.LBB49_40
# BB#33:                                # %lor.lhs.false
	testq	%r10, %r10
	je	.LBB49_39
# BB#34:
	andq	%r15, %rax
.LBB49_35:                              # %land.lhs.true.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r10), %ecx
	cmpq	$33, %rcx
	je	.LBB49_39
# BB#36:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB49_35 Depth=1
	cmpq	$36, %rcx
	je	.LBB49_39
# BB#37:                                # %for.body.i
                                        #   in Loop: Header=BB49_35 Depth=1
	movl	8(%r10), %ecx
	cmpq	%rcx, %rax
	je	.LBB49_40
# BB#38:                                # %for.inc.i
                                        #   in Loop: Header=BB49_35 Depth=1
	movq	24(%r10), %r10
	testq	%r10, %r10
	jne	.LBB49_35
.LBB49_39:                              # %if.else296
	movzbl	100(%r12), %eax
	andq	$-4, %rax
	movw	%ax, 100(%r12)
	jmp	.LBB49_53
.LBB49_26:                              # %if.then143
	movzbl	100(%r12), %eax
	orq	$1, %rax
	movw	%ax, 100(%r12)
.LBB49_53:                              # %if.end304
	movq	%rax, %rdx
	orq	$2048, %rdx             # imm = 0x800
	movw	%dx, 100(%r12)
	movq	const_int_rtx+512(%rip), %rcx
	movq	24(%rsp), %rdi          # 8-byte Reload
	cmpq	%rcx, %rdi
	je	.LBB49_60
# BB#54:                                # %if.else311
	movzbl	(%rdi), %ecx
	cmpq	$133, %rcx
	jg	.LBB49_57
# BB#55:                                # %if.else311
	addq	$-54, %rcx
	andq	%r15, %rcx
	cmpq	$14, %rcx
	ja	.LBB49_60
# BB#56:                                # %if.else311
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	jne	.LBB49_59
	jmp	.LBB49_60
.LBB49_57:                              # %if.else311
	cmpq	$134, %rcx
	je	.LBB49_59
# BB#58:                                # %if.else311
	cmpq	$140, %rcx
	jne	.LBB49_60
.LBB49_59:                              # %if.then351
	andq	$63487, %rax            # imm = 0xF7FF
	movw	%ax, 100(%r12)
	movq	%rax, %rdx
.LBB49_60:                              # %if.end356
	movzbl	(%rdi), %eax
	cmpq	$75, %rax
	jne	.LBB49_69
.LBB49_61:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rax
	movq	8(%rax), %rdi
	movzbl	(%rdi), %ecx
	cmpq	$75, %rcx
	je	.LBB49_61
# BB#62:                                # %if.else373
                                        #   in Loop: Header=BB49_61 Depth=1
	movq	16(%rax), %rdi
	movzbl	(%rdi), %ecx
	cmpq	$133, %rcx
	jg	.LBB49_66
# BB#63:                                # %if.else373
                                        #   in Loop: Header=BB49_61 Depth=1
	addq	$-54, %rcx
	andq	%r15, %rcx
	cmpq	$21, %rcx
	ja	.LBB49_69
# BB#64:                                # %if.else373
                                        #   in Loop: Header=BB49_61 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB49_68
# BB#65:                                # %if.else373
                                        #   in Loop: Header=BB49_61 Depth=1
	cmpq	$21, %rcx
	je	.LBB49_61
	jmp	.LBB49_69
.LBB49_66:                              # %if.else373
	cmpq	$134, %rcx
	je	.LBB49_68
# BB#67:                                # %if.else373
	cmpq	$140, %rcx
	jne	.LBB49_69
.LBB49_68:                              # %if.then451
	andq	$63487, %rdx            # imm = 0xF7FF
	movw	%dx, 100(%r12)
.LBB49_69:                              # %if.end456
	movq	loop_dump_stream(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB49_70
# BB#72:                                # %if.then458
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	loop_giv_dump  # TAILCALL
.LBB49_70:                              # %if.end459
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB49_40:                              # %if.then192
	movq	%r14, %r13
	movzbl	100(%r12), %r8d
	movq	%r8, %rax
	orq	$1, %rax
	movw	%ax, 100(%r12)
	movq	8(%rdi), %rsi
	testq	%rsi, %rsi
	je	.LBB49_47
# BB#41:                                # %for.body.lr.ph
	movl	max_uid_for_loop(%rip), %r9d
	movq	uid_luid(%rip), %r11
	movq	reg_n_info(%rip), %r10
	.align	16, 0x90
.LBB49_42:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsi), %rcx
	movslq	8(%rcx), %rbx
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rbx
	jge	.LBB49_45
# BB#43:                                # %cond.true211
                                        #   in Loop: Header=BB49_42 Depth=1
	movl	8(%r13), %ecx
	movq	32(%r10,%rcx,8), %rdx
	movslq	(%rdx), %rcx
	movslq	(%r11,%rbx,4), %rbx
	movslq	(%r11,%rcx,4), %rcx
	cmpq	%rcx, %rbx
	jl	.LBB49_46
# BB#44:                                # %cond.true240
                                        #   in Loop: Header=BB49_42 Depth=1
	movslq	4(%rdx), %rcx
	movslq	(%r11,%rcx,4), %rcx
	cmpq	%rcx, %rbx
	jle	.LBB49_45
.LBB49_46:                              # %for.inc
                                        #   in Loop: Header=BB49_42 Depth=1
	movq	128(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB49_42
	jmp	.LBB49_47
.LBB49_45:                              # %if.then262
	andq	$-4, %r8
	orq	$2, %r8
	movw	%r8w, 100(%r12)
	movq	%r8, %rax
.LBB49_47:                              # %for.end
	testq	$1, %rax
	je	.LBB49_53
# BB#48:                                # %for.cond277.preheader
	movq	8(%rdi), %rbx
	testq	%rbx, %rbx
	je	.LBB49_53
	.align	16, 0x90
.LBB49_50:                              # %for.body279
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	back_branch_in_range_p
	testq	%r15, %rax
	jne	.LBB49_51
# BB#49:                                # %for.cond277
                                        #   in Loop: Header=BB49_50 Depth=1
	movq	128(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB49_50
# BB#52:                                # %for.cond277.if.end304.loopexit_crit_edge
	movzbl	100(%r12), %eax
	jmp	.LBB49_53
.LBB49_51:                              # %if.then283
	movzbl	100(%r12), %eax
	andq	$-4, %rax
	orq	$2, %rax
	movw	%ax, 100(%r12)
	jmp	.LBB49_53
.LBB49_27:                              # %if.else139
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.record_giv, %rcx
	movq	$5713, %rsi             # imm = 0x1651
	jmp	.LBB49_28
.LBB49_71:                              # %cond.false183
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.record_giv, %rcx
	movq	$5732, %rsi             # imm = 0x1664
.LBB49_28:                              # %if.else139
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp586:
	.size	record_giv, .Ltmp586-record_giv
	.cfi_endproc

	.align	16, 0x90
	.type	find_mem_givs,@function
find_mem_givs:                          # @find_mem_givs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp593:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp594:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp595:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp596:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp597:
	.cfi_def_cfa_offset 48
	subq	$128, %rsp
.Ltmp598:
	.cfi_def_cfa_offset 176
.Ltmp599:
	.cfi_offset %rbx, -48
.Ltmp600:
	.cfi_offset %r12, -40
.Ltmp601:
	.cfi_offset %r13, -32
.Ltmp602:
	.cfi_offset %r14, -24
.Ltmp603:
	.cfi_offset %r15, -16
	movq	%rsi, %r9
	testq	%r9, %r9
	je	.LBB50_6
# BB#1:                                 # %if.end
	movq	%r8, %r14
	movq	%rcx, %r12
	movq	%rdx, %rbx
	movl	(%r9), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	leaq	-44(%rdx), %rcx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rcx
	cmpq	$25, %rcx
	ja	.LBB50_7
# BB#2:                                 # %if.end
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$58903603, %rsi         # imm = 0x382CC33
	movq	%rbx, %r15
	movq	%r12, %rbx
	jne	.LBB50_6
# BB#3:                                 # %if.end
	movq	%rbx, %r12
	movq	%r15, %rbx
	cmpq	$22, %rcx
	jne	.LBB50_7
# BB#4:                                 # %sw.bb1
	movq	8(%r9), %rsi
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, 16(%rsp)
	leaq	92(%rsp), %rax
	movq	%rax, 8(%rsp)
	leaq	120(%rsp), %rdx
	leaq	112(%rsp), %rcx
	movq	%r9, %r15
	leaq	104(%rsp), %r8
	leaq	96(%rsp), %r9
	movq	$1, (%rsp)
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	callq	general_induction_var
	movq	%r15, %rcx
	testq	%r13, %rax
	movq	%rbx, %r15
	movq	%r12, %rbx
	movq	%r14, %rax
	je	.LBB50_6
# BB#5:                                 # %if.then4
	leaq	8(%rcx), %r14
	movq	$168, %rdi
	movq	%rcx, %r13
	movq	%rax, %r12
	callq	xmalloc
	movq	120(%rsp), %rcx
	movq	addr_placeholder(%rip), %r8
	movq	104(%rsp), %r9
	movq	112(%rsp), %rdx
	movq	96(%rsp), %rsi
	movl	92(%rsp), %edi
	movq	%r14, 48(%rsp)
	movq	%r12, 40(%rsp)
	movq	%rbx, 32(%rsp)
	movq	%rdi, 16(%rsp)
	movq	%rsi, 8(%rsp)
	movq	%rdx, (%rsp)
	movq	$0, 24(%rsp)
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	movq	%rax, %rbx
	movq	%r15, %rdx
	callq	record_giv
	movq	%r13, 56(%rbx)
	jmp	.LBB50_6
.LBB50_7:                               # %sw.epilog
	movq	%r14, 72(%rsp)          # 8-byte Spill
	movq	%rbx, 80(%rsp)          # 8-byte Spill
	movq	%rdi, %r13
	movzbl	rtx_length(%rdx), %edi
	testq	%rdi, %rdi
	movq	%r12, %rbx
	je	.LBB50_6
# BB#8:                                 # %for.body.lr.ph
	movq	rtx_format(,%rdx,8), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB50_9:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB50_14 Depth 2
	movq	%rdi, %r14
	movq	56(%rsp), %rax          # 8-byte Reload
	movzbl	-1(%rax,%r14), %eax
	leaq	-1(%r14), %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB50_10
# BB#13:                                # %for.cond31.preheader
                                        #   in Loop: Header=BB50_9 Depth=1
	movq	(%r9,%r14,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r15
	jle	.LBB50_11
	.align	16, 0x90
.LBB50_14:                              # %for.body37
                                        #   Parent Loop BB50_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r15,8), %rsi
	movq	%r13, %rdi
	movq	80(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, %rcx
	movq	%r9, %r12
	movq	72(%rsp), %r8           # 8-byte Reload
	callq	find_mem_givs
	movq	%r12, %r9
	movq	(%r9,%r14,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r15
	cmpq	%rdx, %rsi
	jl	.LBB50_14
	jmp	.LBB50_11
	.align	16, 0x90
.LBB50_10:                              # %for.body
                                        #   in Loop: Header=BB50_9 Depth=1
	cmpq	$101, %rax
	jne	.LBB50_11
# BB#12:                                # %if.then20
                                        #   in Loop: Header=BB50_9 Depth=1
	movq	(%r9,%r14,8), %rsi
	movq	%r13, %rdi
	movq	80(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, %rcx
	movq	%r9, %r14
	movq	72(%rsp), %r8           # 8-byte Reload
	callq	find_mem_givs
	movq	%r14, %r9
.LBB50_11:                              # %for.cond.backedge
                                        #   in Loop: Header=BB50_9 Depth=1
	movq	$32, %rcx
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB50_9
.LBB50_6:                               # %for.end47
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp604:
	.size	find_mem_givs, .Ltmp604-find_mem_givs
	.cfi_endproc

	.align	16, 0x90
	.type	simplify_giv_expr,@function
simplify_giv_expr:                      # @simplify_giv_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp611:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp612:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp613:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp614:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp615:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp616:
	.cfi_def_cfa_offset 80
.Ltmp617:
	.cfi_offset %rbx, -48
.Ltmp618:
	.cfi_offset %r12, -40
.Ltmp619:
	.cfi_offset %r13, -32
.Ltmp620:
	.cfi_offset %r14, -24
.Ltmp621:
	.cfi_offset %r15, -16
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, %r14
	movq	%rdi, (%rsp)            # 8-byte Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB51_1
	.align	16, 0x90
.LBB51_143:                             # %if.then445
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	16(%rax), %r14
	movq	%r13, %rdi
.LBB51_1:                               # %tailrecurse
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB51_140 Depth 2
	movl	(%r14), %edx
	movq	160(%rdi), %rax
	movq	%rdi, %r13
	movq	$16, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	je	.LBB51_4
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	andq	%r15, %rcx
	movl	mode_class(,%rcx,4), %esi
	xorq	%r12, %r12
	cmpq	$1, %rsi
	jne	.LBB51_177
# BB#3:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB51_1 Depth=1
	movzbl	mode_bitsize(%rcx,%rcx), %ecx
	cmpq	$64, %rcx
	ja	.LBB51_177
.LBB51_4:                               # %if.end
                                        #   in Loop: Header=BB51_1 Depth=1
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$119, %rdx
	jg	.LBB51_121
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$60, %rdx
	jle	.LBB51_6
# BB#8:                                 # %if.end
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$74, %rdx
	jg	.LBB51_13
# BB#9:                                 # %if.end
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$61, %rdx
	jne	.LBB51_162
# BB#10:                                # %sw.bb378
                                        #   in Loop: Header=BB51_1 Depth=1
	movl	8(%r14), %edx
	movl	max_reg_before_loop(%rip), %ecx
	xorq	%r12, %r12
	cmpq	%rcx, %rdx
	jae	.LBB51_177
# BB#11:                                # %if.end384
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	208(%rax), %rsi
	movq	$4, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movl	(%rsi,%rdi), %ecx
	cmpq	$1, %rcx
	je	.LBB51_12
# BB#127:                               # %if.end384
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$3, %rcx
	movq	%r13, %rbx
	movq	24(%rsp), %r8           # 8-byte Reload
	jne	.LBB51_137
	jmp	.LBB51_128
	.align	16, 0x90
.LBB51_121:                             # %if.end
                                        #   in Loop: Header=BB51_1 Depth=1
	addq	$-120, %rdx
	andq	%r15, %rdx
	cmpq	$3, %rdx
	jae	.LBB51_162
# BB#122:                               # %sw.bb355
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	24(%rsp), %r8           # 8-byte Reload
	cmpq	$0, (%r8)
	movq	%r13, %rbx
	jne	.LBB51_137
# BB#123:                               # %if.then358
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	8(%r14), %rsi
	movq	%rbx, %rdi
	movq	%r8, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%r8, %r12
	callq	simplify_giv_expr
	movq	%r12, %r8
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB51_137
# BB#124:                               # %land.lhs.true364
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$0, (%r8)
	jne	.LBB51_137
# BB#125:                               # %land.lhs.true367
                                        #   in Loop: Header=BB51_1 Depth=1
	movzbl	(%r12), %eax
	cmpq	$61, %rax
	je	.LBB51_126
	.align	16, 0x90
.LBB51_137:                             # %do_default
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	%r8, 24(%rsp)           # 8-byte Spill
	movq	%rbx, %rdi
	movq	%rbx, %r13
	movq	%r14, %rsi
	callq	loop_invariant_p
	andq	%r15, %rax
	cmpq	$1, %rax
	jne	.LBB51_162
# BB#138:                               # %if.then437
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	160(%r13), %rax
	movq	168(%rax), %rbx
	jmp	.LBB51_140
	.align	16, 0x90
.LBB51_139:                             # %for.cond
                                        #   in Loop: Header=BB51_140 Depth=2
	movq	72(%rbx), %rbx
.LBB51_140:                             # %for.cond
                                        #   Parent Loop BB51_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB51_162
# BB#141:                               # %for.body
                                        #   in Loop: Header=BB51_140 Depth=2
	movq	16(%rbx), %rsi
	movq	%r14, %rdi
	callq	rtx_equal_p
	testq	%r15, %rax
	je	.LBB51_139
# BB#142:                               # %if.then443
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	56(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB51_143
	jmp	.LBB51_144
.LBB51_13:                              # %if.end
                                        #   in Loop: Header=BB51_1 Depth=1
	addq	$-75, %rdx
	andq	%r15, %rdx
	cmpq	$12, %rdx
	ja	.LBB51_162
# BB#14:                                # %if.end
                                        #   in Loop: Header=BB51_1 Depth=1
	jmpq	*.LJTI51_0(,%rdx,8)
.LBB51_15:                              # %sw.bb
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	8(%r14), %rsi
	movq	%r13, %rdi
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdx
	movq	8(%rsp), %r12           # 8-byte Reload
	movq	%r12, %rcx
	callq	simplify_giv_expr
	movq	16(%r14), %rsi
	movq	%rax, %r14
	movq	%r13, %rdi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	callq	simplify_giv_expr
	xorq	%r12, %r12
	testq	%r14, %r14
	je	.LBB51_177
# BB#16:                                # %sw.bb
                                        #   in Loop: Header=BB51_1 Depth=1
	testq	%rax, %rax
	je	.LBB51_177
# BB#17:                                # %if.end22
                                        #   in Loop: Header=BB51_1 Depth=1
	movzbl	(%r14), %ecx
	cmpq	$48, %rcx
	jne	.LBB51_18
# BB#20:                                # %land.lhs.true37
                                        #   in Loop: Header=BB51_1 Depth=1
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$48, %rdx
	jne	.LBB51_23
# BB#21:                                #   in Loop: Header=BB51_1 Depth=1
	movq	%rax, %r8
	jmp	.LBB51_27
.LBB51_79:                              # %sw.bb180
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	8(%r14), %r12
	movq	16(%r14), %rdx
	movq	const_int_rtx+504(%rip), %rcx
	movq	$78, %rdi
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	jmp	.LBB51_117
.LBB51_119:                             # %sw.bb341
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	8(%r14), %rdx
	movq	const_int_rtx+504(%rip), %rcx
	movq	$78, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB51_118
.LBB51_80:                              # %sw.bb190
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	8(%r14), %rsi
	movq	%r13, %rbx
	movq	%rbx, %rdi
	movq	24(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rdx
	movq	8(%rsp), %r13           # 8-byte Reload
	movq	%r13, %rcx
	callq	simplify_giv_expr
	movq	16(%r14), %rsi
	movq	%rax, %r14
	movq	%rbx, %rdi
	movq	%r12, %rdx
	movq	%r13, %rcx
	callq	simplify_giv_expr
	xorq	%r12, %r12
	testq	%r14, %r14
	je	.LBB51_177
# BB#81:                                # %sw.bb190
                                        #   in Loop: Header=BB51_1 Depth=1
	testq	%rax, %rax
	je	.LBB51_177
# BB#82:                                # %if.end205
                                        #   in Loop: Header=BB51_1 Depth=1
	movzbl	(%r14), %ecx
	cmpq	$48, %rcx
	je	.LBB51_85
# BB#83:                                # %if.end205
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$54, %rcx
	jne	.LBB51_84
.LBB51_85:                              # %land.lhs.true215
                                        #   in Loop: Header=BB51_1 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	movq	%rax, %r13
	je	.LBB51_87
# BB#86:                                # %land.lhs.true215
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	%r14, %r13
.LBB51_87:                              # %land.lhs.true215
                                        #   in Loop: Header=BB51_1 Depth=1
	je	.LBB51_89
# BB#88:                                # %land.lhs.true215
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	%rax, %r14
	jmp	.LBB51_89
.LBB51_120:                             # %sw.bb347
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	8(%r14), %rdx
	movq	$77, %rdi
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	gen_rtx_fmt_e
	movq	const_int_rtx+520(%rip), %rcx
	movq	$76, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	jmp	.LBB51_118
.LBB51_115:                             # %sw.bb319
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	16(%r14), %rax
	movzbl	(%rax), %ecx
	xorq	%r12, %r12
	cmpq	$54, %rcx
	jne	.LBB51_177
# BB#116:                               # %if.end328
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	8(%rax), %rcx
	movq	8(%r14), %rbx
	movq	$1, %rsi
	shlq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$78, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
.LBB51_117:                             # %tailrecurse
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	%rax, %rcx
.LBB51_118:                             # %tailrecurse
                                        #   in Loop: Header=BB51_1 Depth=1
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	%r13, %rdi
	jmp	.LBB51_1
.LBB51_18:                              # %if.end22
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$54, %rcx
	jne	.LBB51_19
# BB#22:                                # %land.lhs.true32.lor.lhs.false42_crit_edge
                                        #   in Loop: Header=BB51_1 Depth=1
	movl	(%rax), %ecx
.LBB51_23:                              # %lor.lhs.false42
                                        #   in Loop: Header=BB51_1 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	movq	%rax, %r8
	je	.LBB51_25
# BB#24:                                # %lor.lhs.false42
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	%r14, %r8
.LBB51_25:                              # %lor.lhs.false42
                                        #   in Loop: Header=BB51_1 Depth=1
	je	.LBB51_27
# BB#26:                                # %lor.lhs.false42
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	%rax, %r14
	jmp	.LBB51_27
.LBB51_84:                              #   in Loop: Header=BB51_1 Depth=1
	movq	%rax, %r13
.LBB51_89:                              # %if.end221
                                        #   in Loop: Header=BB51_1 Depth=1
	movzbl	(%r13), %eax
	cmpq	$48, %rax
	je	.LBB51_91
# BB#90:                                # %if.end221
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$54, %rax
	jne	.LBB51_177
.LBB51_91:                              # %if.end232
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %r13
	je	.LBB51_92
# BB#93:                                # %if.else236
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	const_int_rtx+520(%rip), %rcx
	cmpq	%rcx, %r13
	je	.LBB51_94
# BB#95:                                # %if.end241
                                        #   in Loop: Header=BB51_1 Depth=1
	movzbl	(%r14), %ecx
	cmpq	$53, %rcx
	jg	.LBB51_103
# BB#96:                                # %if.end241
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$48, %rcx
	jne	.LBB51_114
# BB#97:                                # %sw.bb253
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$54, %rax
	jne	.LBB51_177
# BB#98:                                # %if.end259
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	8(%r14), %rbx
	movzbl	(%rbx), %eax
	cmpq	$75, %rax
	jne	.LBB51_99
# BB#110:                               # %if.then282
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	8(%rbx), %rdx
	movq	$78, %rdi
	movq	16(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	movq	%r13, %rcx
	callq	gen_rtx_fmt_ee
	movq	16(%rbx), %rdx
	movq	%rax, %rbx
	movq	$78, %rdi
	movq	%r14, %rsi
	movq	%r13, %rcx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	jmp	.LBB51_60
.LBB51_103:                             # %if.end241
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$60, %rcx
	jle	.LBB51_104
# BB#106:                               # %if.end241
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$78, %rcx
	je	.LBB51_112
# BB#107:                               # %if.end241
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$75, %rcx
	jne	.LBB51_108
# BB#113:                               # %sw.bb307
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	8(%r14), %rdx
	movq	$78, %rdi
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r13, %rcx
	callq	gen_rtx_fmt_ee
	movq	16(%r14), %rdx
	movq	%rax, %r14
	movq	$78, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rcx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	%rbx, %rsi
	jmp	.LBB51_59
.LBB51_19:                              #   in Loop: Header=BB51_1 Depth=1
	movq	%rax, %r8
.LBB51_27:                              # %if.end48
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%rax, %r8
	movq	16(%rsp), %r13          # 8-byte Reload
	je	.LBB51_28
# BB#29:                                # %if.else
                                        #   in Loop: Header=BB51_1 Depth=1
	movl	(%r8), %ecx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$48, %rax
	je	.LBB51_31
# BB#30:                                # %if.else
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$54, %rax
	jne	.LBB51_64
.LBB51_31:                              # %if.then61
                                        #   in Loop: Header=BB51_1 Depth=1
	movzbl	(%r14), %ecx
	cmpq	$53, %rcx
	jle	.LBB51_32
# BB#34:                                # %if.then61
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$60, %rcx
	jle	.LBB51_35
# BB#44:                                # %if.then61
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$61, %rcx
	je	.LBB51_47
# BB#45:                                # %if.then61
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$75, %rcx
	jne	.LBB51_46
# BB#58:                                # %sw.bb107
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	%r14, %rax
	movq	8(%rax), %r14
	movq	16(%rax), %rdx
	movq	$75, %rdi
	movq	%r13, %rsi
	movq	%r8, %rcx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	%r13, %rsi
.LBB51_59:                              # %tailrecurse
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	%r14, %rdx
	jmp	.LBB51_60
.LBB51_64:                              # %if.end118
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	%r14, %rbx
	movq	%r8, %r14
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	jne	.LBB51_65
# BB#66:                                # %if.then123
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	const_int_rtx+520(%rip), %rcx
	movq	$78, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	%r14, %rdx
	movl	(%rdx), %ecx
	movq	%rax, %r14
	jmp	.LBB51_67
.LBB51_99:                              # %if.end259
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$78, %rax
	jne	.LBB51_111
# BB#100:                               # %if.then267
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	8(%rbx), %r12
	movq	16(%rbx), %rdx
	jmp	.LBB51_101
.LBB51_65:                              #   in Loop: Header=BB51_1 Depth=1
	movq	%r14, %rdx
	movq	%rbx, %r14
.LBB51_67:                              # %if.end125
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB51_69
# BB#68:                                # %if.then130
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	const_int_rtx+520(%rip), %rcx
	movq	$78, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdx
	movl	(%rdx), %ecx
.LBB51_69:                              # %if.end132
                                        #   in Loop: Header=BB51_1 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$78, %rcx
	movq	%r14, %rbx
	je	.LBB51_71
# BB#70:                                # %if.end132
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	%rdx, %rbx
.LBB51_71:                              # %if.end132
                                        #   in Loop: Header=BB51_1 Depth=1
	je	.LBB51_73
# BB#72:                                # %if.end132
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	%r14, %rdx
.LBB51_73:                              # %if.end132
                                        #   in Loop: Header=BB51_1 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$75, %rax
	jne	.LBB51_75
# BB#74:                                # %if.then143
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	8(%rbx), %rcx
	movq	$75, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_fmt_ee
	movq	16(%rbx), %rcx
	movq	$75, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	jmp	.LBB51_61
.LBB51_75:                              # %if.end153
                                        #   in Loop: Header=BB51_1 Depth=1
	movzbl	(%rdx), %ecx
	cmpq	$78, %rcx
	jne	.LBB51_177
# BB#76:                                # %if.end153
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$78, %rax
	jne	.LBB51_177
# BB#77:                                # %if.end164
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	%rdx, %rdi
	movq	%rbx, %rsi
	movq	%rdx, %r14
	callq	rtx_equal_p
	testq	%r15, %rax
	je	.LBB51_177
# BB#78:                                # %if.end167
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	8(%r14), %r12
	movq	16(%r14), %rdx
	movq	16(%rbx), %rcx
	movq	$75, %rdi
	movq	%r13, %r14
	movq	%r14, %rsi
	callq	gen_rtx_fmt_ee
	movq	$78, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	jmp	.LBB51_60
.LBB51_112:                             # %sw.bb297
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	8(%r14), %r12
	movq	16(%r14), %rdx
.LBB51_101:                             # %tailrecurse
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	$78, %rdi
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r13, %rcx
	callq	gen_rtx_fmt_ee
	movq	$78, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
.LBB51_60:                              # %tailrecurse
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	%rax, %rcx
.LBB51_61:                              # %tailrecurse
                                        #   in Loop: Header=BB51_1 Depth=1
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	(%rsp), %rdi            # 8-byte Reload
	jmp	.LBB51_1
.LBB51_6:                               # %if.end
	cmpq	$48, %rdx
	jne	.LBB51_162
# BB#7:
	movq	%r14, %r12
	jmp	.LBB51_177
.LBB51_12:
	movq	%r14, %r12
	jmp	.LBB51_177
.LBB51_144:                             # %if.end449
	movl	32(%rbx), %eax
	movq	(%rbx), %rdi
	testq	%rax, %rax
	je	.LBB51_148
	.align	16, 0x90
.LBB51_145:                             # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rdi), %rdi
	decq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jg	.LBB51_145
# BB#146:                               # %do.end
	movq	$4, %rsi
	xorq	%rdx, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB51_162
# BB#147:                               # %if.then461
	addq	$8, %rax
	jmp	.LBB51_153
.LBB51_32:                              # %if.then61
	cmpq	$48, %rcx
	jne	.LBB51_62
# BB#33:                                # %if.then69
	movq	8(%r14), %r14
	jmp	.LBB51_36
.LBB51_104:                             # %if.end241
	cmpq	$54, %rcx
	jne	.LBB51_114
# BB#105:                               # %sw.bb246
	movq	8(%r14), %rax
	movq	8(%r13), %rsi
	imulq	%rax, %rsi
	xorq	%rdi, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_CONST_INT  # TAILCALL
.LBB51_35:                              # %if.then61
	cmpq	$54, %rcx
	jne	.LBB51_62
.LBB51_36:                              # %if.end73
	cmpq	$48, %rax
	jne	.LBB51_38
# BB#37:                                # %if.then78
	movq	8(%r8), %r8
.LBB51_38:                              # %if.end82
	movzbl	(%r14), %eax
	cmpq	$54, %rax
	movq	%r14, %rbx
	je	.LBB51_40
# BB#39:                                # %if.end82
	movq	%r8, %rbx
.LBB51_40:                              # %if.end82
	je	.LBB51_42
# BB#41:                                # %if.end82
	movq	%r14, %r8
.LBB51_42:                              # %if.end82
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	jne	.LBB51_49
	jmp	.LBB51_43
	.align	16, 0x90
.LBB51_54:                              # %if.end.i
                                        #   in Loop: Header=BB51_49 Depth=1
	movq	%rax, %r8
	movq	16(%rbx), %rbx
	movl	(%rbx), %eax
.LBB51_49:                              # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$54, %rax
	je	.LBB51_43
# BB#50:                                # %while.cond.i
                                        #   in Loop: Header=BB51_49 Depth=1
	cmpq	$75, %rax
	jne	.LBB51_55
# BB#51:                                # %while.body.i
                                        #   in Loop: Header=BB51_49 Depth=1
	movq	8(%rbx), %rcx
	movzbl	(%rcx), %eax
	cmpq	$54, %rax
	jne	.LBB51_53
# BB#52:                                # %if.then.i
                                        #   in Loop: Header=BB51_49 Depth=1
	movq	%r8, %rdi
	movq	%rcx, %rsi
	callq	sge_plus_constant
	jmp	.LBB51_54
.LBB51_53:                              # %if.else.i
                                        #   in Loop: Header=BB51_49 Depth=1
	movq	$75, %rdi
	movq	%r13, %rsi
	movq	%r8, %rdx
	callq	gen_rtx_fmt_ee
	jmp	.LBB51_54
.LBB51_43:                              # %if.then93
	movq	%r8, %rdi
	movq	%rbx, %rsi
	callq	sge_plus_constant
	jmp	.LBB51_56
.LBB51_148:                             # %if.else466
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB51_162
# BB#149:                               # %cond.true
	movq	32(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB51_151
# BB#150:                               # %cond.false
	movq	%rax, %rsi
	callq	single_set_2
.LBB51_151:                             # %cond.end495
	testq	%rax, %rax
	je	.LBB51_162
# BB#152:                               # %if.then498
	addq	$16, %rax
.LBB51_153:                             # %if.end503
	movq	(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB51_162
# BB#154:                               # %if.then505
	movzbl	(%rsi), %ecx
	addq	$-54, %rcx
	andq	%r15, %rcx
	cmpq	$33, %rcx
	ja	.LBB51_162
# BB#155:                               # %if.then505
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$8608825345, %rdx       # imm = 0x201204001
	testq	%rdx, %rax
	je	.LBB51_156
# BB#178:                               # %if.then530
	movq	%r13, %rdi
.LBB51_161:                             # %if.then570
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	8(%rsp), %rcx           # 8-byte Reload
	callq	simplify_giv_expr
	movq	%rax, %r12
	testq	%r12, %r12
	jne	.LBB51_177
	jmp	.LBB51_162
.LBB51_55:                              # %if.else13.i
	movq	$75, %rdi
	movq	%r13, %rsi
	movq	%r8, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
.LBB51_56:                              # %if.end97
	movq	%rax, %r12
	movzbl	(%r12), %eax
	cmpq	$54, %rax
	je	.LBB51_177
# BB#57:                                # %if.then102
	movq	$48, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	jmp	.LBB51_175
.LBB51_126:                             # %if.then372
	movzbl	(%r14), %edi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r12, %rdx
	movq	%r8, %rbx
	callq	gen_rtx_fmt_e
	movq	%rax, (%rbx)
	jmp	.LBB51_177
.LBB51_108:                             # %if.end241
	cmpq	$61, %rcx
	jne	.LBB51_114
# BB#109:                               # %sw.bb244
	movq	$78, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	movq	%r13, %rcx
	jmp	.LBB51_48
.LBB51_46:                              # %if.then61
	cmpq	$78, %rcx
	jne	.LBB51_62
.LBB51_47:                              # %sw.bb105
	movq	$75, %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	movq	%r8, %rcx
.LBB51_48:                              # %sw.bb105
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.LBB51_128:                             # %sw.bb392
	movq	%rbx, %r14
	movq	192(%rax), %rax
	movq	$4, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	leaq	(%rdx,%rdx,2), %rcx
	movq	8(%rax,%rcx,8), %rax
	movq	8(%rsi,%rdi), %rbx
	testq	%rax, %rax
	je	.LBB51_131
# BB#129:                               # %sw.bb392
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB51_131
# BB#130:                               # %if.then408
	movl	80(%rbx), %eax
	movq	8(%rsp), %rdx           # 8-byte Reload
	movl	(%rdx), %ecx
	addq	%rax, %rcx
	movl	%ecx, (%rdx)
.LBB51_131:                             # %if.end410
	movq	%r8, %r13
	movzbl	100(%rbx), %eax
	testq	$64, %rax
	jne	.LBB51_177
# BB#132:                               # %if.end416
	movq	16(%rbx), %rdx
	movq	64(%rbx), %rcx
	movq	$78, %rdi
	movq	16(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	callq	gen_rtx_fmt_ee
	movq	72(%rbx), %rcx
	movq	$75, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	112(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB51_134
# BB#133:                               # %if.then420
	movq	$76, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
.LBB51_134:                             # %if.end423
	movq	%r13, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rdx, %r14
	callq	simplify_giv_expr
	movq	%r14, %rdx
	cmpq	$0, (%rdx)
	movq	120(%rbx), %rcx
	movq	%rax, %r12
	je	.LBB51_176
# BB#135:                               # %if.then426
	testq	%rcx, %rcx
	je	.LBB51_177
# BB#136:                               # %if.then426
	xorq	%r12, %r12
	jmp	.LBB51_177
.LBB51_92:
	movq	%r13, %r12
	jmp	.LBB51_177
.LBB51_28:
	movq	%r14, %r12
	jmp	.LBB51_177
.LBB51_94:
	movq	%r14, %r12
	jmp	.LBB51_177
.LBB51_111:                             # %if.end294
	movq	$78, %rdi
	movq	16(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	%r13, %rcx
	callq	gen_rtx_fmt_ee
	movq	$48, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	jmp	.LBB51_175
.LBB51_156:                             # %if.then505
	cmpq	$4, %rcx
	jne	.LBB51_162
# BB#157:                               # %land.lhs.true540
	movq	8(%rsi), %rax
	movzbl	(%rax), %ecx
	cmpq	$75, %rcx
	jne	.LBB51_162
# BB#158:                               # %land.lhs.true548
	movq	8(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$68, %rcx
	jne	.LBB51_162
# BB#159:                               # %land.lhs.true559
	movq	16(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$54, %rcx
	jne	.LBB51_162
# BB#160:                               # %if.then570
	movq	%r13, %rdi
	movq	%rax, %rsi
	jmp	.LBB51_161
.LBB51_162:                             # %sw.default583
	movzbl	(%r14), %eax
	cmpq	$48, %rax
	jne	.LBB51_164
# BB#163:                               # %if.then588
	movq	8(%r14), %r14
.LBB51_164:                             # %if.end592
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	loop_invariant_p
	andq	%r15, %rax
	xorq	%r12, %r12
	cmpq	$1, %rax
	jne	.LBB51_177
# BB#165:                               # %if.then596
	movzbl	(%r14), %eax
	cmpq	$54, %rax
	jne	.LBB51_167
# BB#166:
	movq	%r14, %r12
.LBB51_177:                             # %return
	movq	%r12, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB51_167:                             # %if.then596
	cmpq	$58, %rax
	jne	.LBB51_168
# BB#169:                               # %land.lhs.true607
	movq	8(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$75, %rcx
	movq	16(%rsp), %rsi          # 8-byte Reload
	jne	.LBB51_174
# BB#170:                               # %land.lhs.true615
	movq	8(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$68, %rcx
	jne	.LBB51_174
# BB#171:                               # %land.lhs.true626
	movq	16(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$54, %rcx
	je	.LBB51_173
# BB#172:                               # %land.lhs.true626
	movq	%r14, %rax
.LBB51_173:                             # %land.lhs.true626
	movq	%rax, %r14
	jmp	.LBB51_174
.LBB51_168:
	movq	16(%rsp), %rsi          # 8-byte Reload
.LBB51_174:                             # %if.end641
	movq	$48, %rdi
	movq	%r14, %rdx
.LBB51_175:                             # %if.end641
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_e  # TAILCALL
.LBB51_176:                             # %if.else430
	movq	%rcx, (%rdx)
	jmp	.LBB51_177
.LBB51_114:                             # %sw.default318
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.simplify_giv_expr, %rcx
	movq	$6614, %rsi             # imm = 0x19D6
	jmp	.LBB51_63
.LBB51_62:                              # %sw.default
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.simplify_giv_expr, %rcx
	movq	$6473, %rsi             # imm = 0x1949
.LBB51_63:                              # %sw.default
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp622:
	.size	simplify_giv_expr, .Ltmp622-simplify_giv_expr
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI51_0:
	.quad	.LBB51_15
	.quad	.LBB51_79
	.quad	.LBB51_119
	.quad	.LBB51_80
	.quad	.LBB51_162
	.quad	.LBB51_162
	.quad	.LBB51_162
	.quad	.LBB51_162
	.quad	.LBB51_162
	.quad	.LBB51_162
	.quad	.LBB51_162
	.quad	.LBB51_120
	.quad	.LBB51_115

	.text
	.align	16, 0x90
	.type	sge_plus_constant,@function
sge_plus_constant:                      # @sge_plus_constant
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp626:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp627:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp628:
	.cfi_def_cfa_offset 32
.Ltmp629:
	.cfi_offset %rbx, -24
.Ltmp630:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movl	(%r14), %ebx
	movq	%rsi, %rax
	movq	%rbx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$75, %rcx
	jne	.LBB52_1
# BB#4:                                 # %if.else11
	movq	16(%r14), %rdi
	movzbl	(%rdi), %esi
	cmpq	$54, %rsi
	jne	.LBB52_6
# BB#5:                                 # %if.then17
	movq	$16, %rcx
	movq	8(%rdi), %rdx
	movq	8(%rax), %rsi
	movq	8(%r14), %r14
	shrq	%cl, %rbx
	andq	$255, %rbx
	addq	%rdx, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	jmp	.LBB52_3
.LBB52_1:                               # %entry
	cmpq	$54, %rcx
	jne	.LBB52_2
# BB#12:                                # %if.then
	movq	8(%r14), %rcx
	movq	8(%rax), %rsi
	addq	%rcx, %rsi
	xorq	%rdi, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_CONST_INT  # TAILCALL
.LBB52_6:                               # %if.else36
	movq	8(%r14), %r8
	movzbl	(%r8), %edx
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	cmpq	$75, %rdx
	je	.LBB52_8
# BB#7:                                 # %if.else36
	cmpq	$75, %rsi
	jne	.LBB52_8
# BB#9:                                 # %if.else61
	movq	%rax, %rsi
	callq	sge_plus_constant
	movq	8(%r14), %rcx
	jmp	.LBB52_10
.LBB52_2:                               # %if.then7
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
.LBB52_3:                               # %if.then7
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	jmp	.LBB52_11
.LBB52_8:                               # %if.then49
	movq	%r8, %rdi
	movq	%rax, %rsi
	callq	sge_plus_constant
	movq	16(%r14), %rcx
.LBB52_10:                              # %if.else61
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
.LBB52_11:                              # %if.else61
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp631:
	.size	sge_plus_constant, .Ltmp631-sge_plus_constant
	.cfi_endproc

	.align	16, 0x90
	.type	record_initial,@function
record_initial:                         # @record_initial
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB53_5
# BB#1:                                 # %lor.lhs.false
	movl	8(%rdi), %eax
	movl	8(%rdx), %ecx
	cmpq	%rcx, %rax
	jae	.LBB53_5
# BB#2:                                 # %lor.lhs.false2
	movq	(%rdx), %rdx
	movq	$4, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movl	(%rdx,%rdi), %ecx
	cmpq	$1, %rcx
	jne	.LBB53_5
# BB#3:                                 # %if.end
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	8(%rdx,%rax), %rax
	cmpq	$0, 72(%rax)
	jne	.LBB53_5
# BB#4:                                 # %if.then15
	movq	note_insn(%rip), %rcx
	movq	%rcx, 72(%rax)
	movq	%rsi, 80(%rax)
.LBB53_5:                               # %if.end17
	retq
.Ltmp632:
	.size	record_initial, .Ltmp632-record_initial
	.cfi_endproc

	.align	16, 0x90
	.type	check_insn_for_bivs,@function
check_insn_for_bivs:                    # @check_insn_for_bivs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp639:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp640:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp641:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp642:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp643:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp644:
	.cfi_def_cfa_offset 112
.Ltmp645:
	.cfi_offset %rbx, -48
.Ltmp646:
	.cfi_offset %r12, -40
.Ltmp647:
	.cfi_offset %r13, -32
.Ltmp648:
	.cfi_offset %r14, -24
.Ltmp649:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movzbl	(%r12), %eax
	movq	%rcx, %r15
	movq	%rdx, %rbx
	movq	%rdi, %r13
	cmpq	$32, %rax
	jne	.LBB54_84
# BB#1:                                 # %entry
	movzbl	rtx_class+32(%rip), %eax
	andq	$255, %rax
	cmpq	$105, %rax
	jne	.LBB54_84
# BB#2:                                 # %cond.true
	movq	32(%r12), %rax
	movzbl	(%rax), %ecx
	movq	160(%r13), %r14
	cmpq	$47, %rcx
	je	.LBB54_4
# BB#3:                                 # %cond.false
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB54_4:                               # %cond.end19
	testq	%rax, %rax
	je	.LBB54_84
# BB#5:                                 # %land.lhs.true21
	movq	8(%rax), %r8
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movzbl	(%r8), %ecx
	cmpq	$61, %rcx
	jne	.LBB54_84
# BB#6:                                 # %if.then
	movl	8(%r8), %edx
	movl	max_reg_before_loop(%rip), %ecx
	cmpq	%rcx, %rdx
	jae	.LBB54_84
# BB#7:                                 # %if.then
	cmpq	$53, %rdx
	jb	.LBB54_84
# BB#8:                                 # %land.lhs.true42
	movq	208(%r14), %rsi
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	(%rsi,%rdx), %ecx
	cmpq	$2, %rcx
	je	.LBB54_84
# BB#9:                                 # %if.then50
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movq	%r14, 48(%rsp)          # 8-byte Spill
	movq	16(%rax), %rcx
	movl	(%rcx), %eax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movabsq	$4294967200, %rdi       # imm = 0xFFFFFFA0
	movq	%rax, %rsi
	movq	%r12, %r14
	jmp	.LBB54_10
.LBB54_71:                              # %if.then360.i
                                        #   in Loop: Header=BB54_10 Depth=1
	movq	8(%rdx), %rcx
	movl	(%rax), %esi
	movl	(%rcx), %eax
.LBB54_10:                              # %tailrecurse.outer.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB54_11 Depth 2
                                        #     Child Loop BB54_56 Depth 2
                                        #     Child Loop BB54_29 Depth 2
                                        #       Child Loop BB54_40 Depth 3
	movq	%rcx, %rdx
	jmp	.LBB54_11
.LBB54_27:                              # %tailrecurse.backedge.i
                                        #   in Loop: Header=BB54_11 Depth=2
	movq	8(%rdx), %rdx
	movl	(%rdx), %eax
	movq	%rax, %rsi
.LBB54_11:                              # %tailrecurse.i
                                        #   Parent Loop BB54_10 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$119, %rax
	jg	.LBB54_26
# BB#12:                                # %tailrecurse.i
                                        #   in Loop: Header=BB54_11 Depth=2
	movq	%rsi, %r15
	cmpq	$88, %rax
	jg	.LBB54_55
# BB#13:                                # %tailrecurse.i
                                        #   in Loop: Header=BB54_11 Depth=2
	cmpq	$74, %rax
	jg	.LBB54_16
# BB#14:                                # %tailrecurse.i
                                        #   in Loop: Header=BB54_11 Depth=2
	addq	$-54, %rax
	andq	%rbx, %rax
	cmpq	$14, %rax
	ja	.LBB54_81
# BB#15:                                # %tailrecurse.i
                                        #   in Loop: Header=BB54_11 Depth=2
	jmpq	*.LJTI54_0(,%rax,8)
.LBB54_26:                              # %tailrecurse.i
                                        #   in Loop: Header=BB54_11 Depth=2
	cmpq	$120, %rax
	je	.LBB54_27
	jmp	.LBB54_81
.LBB54_55:                              # %tailrecurse.i
                                        #   in Loop: Header=BB54_10 Depth=1
	cmpq	$89, %rax
	jne	.LBB54_81
.LBB54_56:                              # %for.cond.i
                                        #   Parent Loop BB54_10 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	16(%r14), %r14
	xorq	%r15, %r15
	testq	%r14, %r14
	je	.LBB54_57
# BB#58:                                # %land.lhs.true261.i
                                        #   in Loop: Header=BB54_56 Depth=2
	movzbl	(%r14), %eax
	cmpq	$37, %rax
	jne	.LBB54_60
# BB#59:                                # %land.rhs266.i
                                        #   in Loop: Header=BB54_56 Depth=2
	movl	40(%r14), %eax
	cmpq	%rdi, %rax
	movq	$37, %rax
	jne	.LBB54_56
.LBB54_60:                              # %if.then277.i
                                        #   in Loop: Header=BB54_10 Depth=1
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB54_64
# BB#61:                                # %cond.true285.i
                                        #   in Loop: Header=BB54_10 Depth=1
	movq	%rdx, %rbx
	movq	32(%r14), %r15
	movzbl	(%r15), %eax
	cmpq	$47, %rax
	je	.LBB54_63
# BB#62:                                # %cond.false297.i
                                        #   in Loop: Header=BB54_10 Depth=1
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r8, %r15
	callq	single_set_2
	movq	%r15, %r8
	movq	%rax, %r15
.LBB54_63:                              # %if.end307.i
                                        #   in Loop: Header=BB54_10 Depth=1
	movq	%rbx, %rdx
	jmp	.LBB54_64
.LBB54_57:                              #   in Loop: Header=BB54_10 Depth=1
	xorq	%r14, %r14
.LBB54_64:                              # %if.end307.i
                                        #   in Loop: Header=BB54_10 Depth=1
	movq	8(%rdx), %rsi
	movq	%r8, %rdi
	movq	%rdx, %rbx
	callq	rtx_equal_p
	movq	%rbx, %rdx
	movq	56(%rsp), %r8           # 8-byte Reload
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	movabsq	$4294967200, %r9        # imm = 0xFFFFFFA0
	movq	%r9, %rdi
	jne	.LBB54_81
# BB#65:                                # %if.end307.i
                                        #   in Loop: Header=BB54_10 Depth=1
	testq	%r15, %r15
	je	.LBB54_81
# BB#66:                                # %land.lhs.true315.i
                                        #   in Loop: Header=BB54_10 Depth=1
	movq	8(%r15), %rax
	movq	8(%rdx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB54_81
# BB#67:                                # %land.lhs.true324.i
                                        #   in Loop: Header=BB54_10 Depth=1
	movq	16(%rdx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB54_81
# BB#68:                                # %land.lhs.true332.i
                                        #   in Loop: Header=BB54_10 Depth=1
	cmpq	$0, 8(%rcx)
	js	.LBB54_81
# BB#69:                                # %land.lhs.true340.i
                                        #   in Loop: Header=BB54_10 Depth=1
	movq	16(%r15), %rdx
	movzbl	(%rdx), %esi
	cmpq	$87, %rsi
	jne	.LBB54_81
# BB#70:                                # %land.lhs.true348.i
                                        #   in Loop: Header=BB54_10 Depth=1
	movq	16(%rdx), %rsi
	cmpq	%rsi, %rcx
	je	.LBB54_71
	jmp	.LBB54_81
.LBB54_28:                              # %sw.bb79.i
                                        #   in Loop: Header=BB54_10 Depth=1
	movq	%r8, %rdi
	movq	%rdx, %rsi
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	callq	rtx_equal_p
	movabsq	$4294967292, %r10       # imm = 0xFFFFFFFC
	movabsq	$4294967200, %rdi       # imm = 0xFFFFFFA0
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	56(%rsp), %r8           # 8-byte Reload
	testq	%rbx, %rax
	jne	.LBB54_81
.LBB54_29:                              # %do.body.preheader.i
                                        #   Parent Loop BB54_10 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB54_40 Depth 3
	movq	16(%r14), %r14
	testq	%r14, %r14
	je	.LBB54_43
# BB#30:                                # %land.lhs.true88.i
                                        #   in Loop: Header=BB54_29 Depth=2
	movzbl	(%r14), %eax
	cmpq	$37, %rax
	jne	.LBB54_32
# BB#31:                                # %land.rhs.i
                                        #   in Loop: Header=BB54_29 Depth=2
	movl	40(%r14), %ecx
	cmpq	%rdi, %rcx
	jne	.LBB54_29
.LBB54_32:                              # %if.end97.i
                                        #   in Loop: Header=BB54_29 Depth=2
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB54_43
# BB#33:                                # %cond.true.i
                                        #   in Loop: Header=BB54_29 Depth=2
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB54_35
# BB#34:                                # %cond.false.i
                                        #   in Loop: Header=BB54_29 Depth=2
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	single_set_2
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	movq	%rcx, %r10
	movabsq	$4294967200, %r9        # imm = 0xFFFFFFA0
	movq	%r9, %rdi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	56(%rsp), %r8           # 8-byte Reload
.LBB54_35:                              # %cond.end119.i
                                        #   in Loop: Header=BB54_29 Depth=2
	testq	%rax, %rax
	je	.LBB54_43
# BB#36:                                # %if.end124.i
                                        #   in Loop: Header=BB54_29 Depth=2
	movq	8(%rax), %r11
	cmpq	%rdx, %r11
	je	.LBB54_52
# BB#37:                                # %lor.lhs.false130.i
                                        #   in Loop: Header=BB54_29 Depth=2
	movl	(%r11), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB54_40
# BB#38:                                # %land.lhs.true135.i
                                        #   in Loop: Header=BB54_29 Depth=2
	movq	%r8, %rdi
	movq	$16, %rcx
	movq	%rsi, %r8
	shrq	%cl, %r8
	movl	target_flags(%rip), %ebx
	andq	$255, %r8
	movzbl	mode_size(%r8), %r9d
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	addq	$4, %rbx
	andq	%r10, %rbx
	cmpq	%rbx, %r9
	ja	.LBB54_39
# BB#50:                                # %land.lhs.true146.i
                                        #   in Loop: Header=BB54_29 Depth=2
	movl	mode_class(,%r8,4), %ecx
	cmpq	$1, %rcx
	jne	.LBB54_39
# BB#51:                                # %land.lhs.true154.i
                                        #   in Loop: Header=BB54_29 Depth=2
	movq	8(%r11), %rcx
	cmpq	%rdx, %rcx
	movq	%rdi, %r8
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rbx
	movabsq	$4294967200, %rcx       # imm = 0xFFFFFFA0
	movq	%rcx, %rdi
	jne	.LBB54_40
	jmp	.LBB54_52
.LBB54_39:                              #   in Loop: Header=BB54_29 Depth=2
	movq	%rdi, %r8
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, %rbx
	movabsq	$4294967200, %rax       # imm = 0xFFFFFFA0
	movq	%rax, %rdi
	jmp	.LBB54_40
.LBB54_54:                              # %while.body206.i
                                        #   in Loop: Header=BB54_40 Depth=3
	movq	8(%r11), %r11
	movl	(%r11), %esi
.LBB54_40:                              # %while.cond187.i
                                        #   Parent Loop BB54_10 Depth=1
                                        #     Parent Loop BB54_29 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	andq	$65535, %rsi            # imm = 0xFFFF
	leaq	-63(%rsi), %rax
	andq	%rbx, %rax
	cmpq	$2, %rax
	jb	.LBB54_54
# BB#41:                                # %while.cond187.i
                                        #   in Loop: Header=BB54_40 Depth=3
	addq	$-132, %rsi
	andq	%rbx, %rsi
	cmpq	$2, %rsi
	jb	.LBB54_54
# BB#42:                                # %lor.rhs.i
                                        #   in Loop: Header=BB54_29 Depth=2
	cmpq	%rdx, %r11
	jne	.LBB54_29
	jmp	.LBB54_43
.LBB54_52:                              # %if.then160.i
                                        #   in Loop: Header=BB54_10 Depth=1
	movq	16(%rax), %rcx
	movl	(%rcx), %eax
	testq	$16711680, %rax         # imm = 0xFF0000
	movq	%rax, %rsi
	jne	.LBB54_10
# BB#53:                                # %cond.true172.i
                                        #   in Loop: Header=BB54_10 Depth=1
	movl	(%rdx), %esi
	jmp	.LBB54_10
.LBB54_16:                              # %tailrecurse.i
	cmpq	$75, %rax
	jne	.LBB54_81
# BB#17:                                # %sw.bb.i
	movq	8(%rdx), %rdi
	movq	%r8, %rsi
	movq	%r8, %r14
	movq	%rdx, %r15
	callq	rtx_equal_p
	movq	%r15, %r10
	movq	%rbx, %r9
	testq	%r9, %rax
	jne	.LBB54_20
# BB#18:                                # %lor.lhs.false.i
	leaq	8(%r10), %rsi
	movq	(%rsi), %rax
	movl	(%rax), %ecx
	andq	$268500991, %rcx        # imm = 0x1000FFFF
	cmpq	$268435519, %rcx        # imm = 0x1000003F
	jne	.LBB54_21
# BB#19:                                # %land.lhs.true12.i
	movq	8(%rax), %rax
	cmpq	%r14, %rax
	je	.LBB54_20
.LBB54_21:                              # %if.else.i
	movq	16(%r10), %rdi
	movq	%r14, %rbx
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%rbx, %rsi
	movq	%r9, %r14
	movq	%r10, %r15
	callq	rtx_equal_p
	movq	%r15, %r10
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%r14, %r9
	movq	%rbx, %r8
	testq	%r9, %rax
	jne	.LBB54_24
# BB#22:                                # %lor.lhs.false28.i
	movq	16(%r10), %rax
	movl	(%rax), %ecx
	andq	$268500991, %rcx        # imm = 0x1000FFFF
	cmpq	$268435519, %rcx        # imm = 0x1000003F
	jne	.LBB54_81
# BB#23:                                # %land.lhs.true43.i
	movq	8(%rax), %rax
	cmpq	%r8, %rax
	jne	.LBB54_81
	jmp	.LBB54_24
.LBB54_20:                              # %if.then.i
	leaq	16(%r10), %rdx
	movq	%r14, %r8
.LBB54_24:                              # %if.end56.i
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	(%rdx), %rsi
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	movq	%r8, %rbx
	movq	%r9, %r15
	movq	%r10, %r14
	callq	loop_invariant_p
	movq	%r14, %rdx
	movq	%r15, %rcx
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %r8
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB54_81
# BB#25:                                # %if.end60.i
	movq	%rsi, %r15
	movzbl	2(%rdx), %esi
	movzbl	2(%r8), %edi
	xorq	%rcx, %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%r8, %rbx
	callq	convert_modes
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movabsq	$const_int_rtx+520, %rax
.LBB54_48:                              # %if.then61
	movq	(%rax), %r14
	movq	$168, %rdi
	callq	xmalloc
	movq	%r14, %r8
	movq	%rax, %r14
	movq	160(%r13), %r13
	movq	%r12, (%r14)
	movq	%rbx, 16(%r14)
	movq	%rbx, 32(%r14)
	movq	%r8, 64(%r14)
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, 72(%r14)
	movq	$0, 120(%r14)
	movq	%r15, 40(%r14)
	movzbl	2(%rbx), %eax
	movl	%eax, 48(%r14)
	movq	40(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB54_49
# BB#72:                                # %if.then61
	movabsq	$1, %rax
	jmp	.LBB54_73
.LBB54_49:
	movabsq	$0, %rax
.LBB54_73:                              # %if.then61
	movq	32(%rsp), %rdi          # 8-byte Reload
	movzbl	100(%r14), %edx
	movq	$3, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	andq	$65479, %rdx            # imm = 0xFFC7
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	$5, %rcx
	shlq	%cl, %rdi
	andq	$32, %rdi
	orq	%rsi, %rax
	orq	%rdi, %rax
	orq	%rdx, %rax
	movw	%ax, 100(%r14)
	movl	8(%rbx), %eax
	movq	208(%r13), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	8(%rdx,%rax), %rax
	testq	%rax, %rax
	jne	.LBB54_75
# BB#74:                                # %if.then.i112
	movq	$96, %rdi
	movq	%r8, %r15
	callq	xmalloc
	movq	%r15, %r8
	movl	8(%rbx), %edx
	movl	%edx, (%rax)
	movq	$0, 24(%rax)
	movq	$0, 12(%rax)
	movq	$0, 4(%rax)
	movq	%rbx, 40(%rax)
	movq	$0, 56(%rax)
	movq	$0, 48(%rax)
	movq	$0, 80(%rax)
	movq	$0, 72(%rax)
	movzbl	88(%rax), %ecx
	andq	$240, %rcx
	movb	%cl, 88(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	movq	224(%r13), %rcx
	movq	%rcx, 64(%rax)
	movq	%rax, 224(%r13)
	movq	208(%r13), %rsi
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	%rax, 8(%rsi,%rdx)
.LBB54_75:                              # %if.end.i
	movq	8(%rax), %rcx
	movq	%rcx, 128(%r14)
	movq	%r14, 8(%rax)
	movl	4(%rax), %ecx
	incq	%rcx
	movl	%ecx, 4(%rax)
	movq	const_int_rtx+520(%rip), %rcx
	cmpq	%r8, %rcx
	jne	.LBB54_77
# BB#76:                                # %if.then49.i
	movzbl	88(%rax), %ecx
	orq	$1, %rcx
	movb	%cl, 88(%rax)
.LBB54_77:                              # %if.end54.i
	movq	loop_dump_stream(%rip), %r15
	testq	%r15, %r15
	je	.LBB54_80
# BB#78:                                # %if.end54.i
	testq	%r14, %r14
	je	.LBB54_80
# BB#79:                                # %if.end.i.i
	movq	(%r14), %rax
	movq	32(%r14), %rdx
	movl	8(%rax), %ecx
	movl	8(%rdx), %edx
	movabsq	$.L.str22, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	fprintf
	movabsq	$.L.str23, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r15, %rcx
	callq	fwrite
	movq	72(%r14), %rsi
	movq	%r15, %rdi
	callq	print_simple_rtl
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	fputc
.LBB54_80:                              # %record_biv.exit
	movl	8(%rbx), %eax
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	208(%rcx), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	$1, %rcx
	jmp	.LBB54_83
.LBB54_43:                              # %sw.bb215.i
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%r13, %rdi
	movq	%rdx, %rsi
	movq	%r8, %r14
	callq	loop_invariant_p
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%r14, %r8
	andq	%rbx, %rax
	cmpq	$1, %rax
	jne	.LBB54_81
.LBB54_44:                              # %sw.bb221.i
	movl	124(%r13), %eax
	movq	$16, %rcx
	shrq	%cl, %r15
	cmpq	$1, %rax
	jne	.LBB54_81
# BB#45:                                # %land.lhs.true224.i
	movzbl	2(%r8), %edi
	andq	$255, %r15
	movl	mode_class(,%r15,4), %eax
	movq	%r15, %rsi
	movl	mode_class(,%rdi,4), %ecx
	cmpq	%rcx, %rax
	jne	.LBB54_81
# BB#46:                                # %land.lhs.true224.i
	cmpq	$4, %rax
	je	.LBB54_81
# BB#47:                                # %if.then239.i
	xorq	%rcx, %rcx
	xorq	%r15, %r15
	movq	%r8, %rbx
	callq	convert_modes
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movabsq	$const_int_rtx+512, %rax
	jmp	.LBB54_48
.LBB54_81:                              # %if.else
	movl	8(%r8), %eax
	movq	48(%rsp), %rdx          # 8-byte Reload
	movl	216(%rdx), %ecx
	cmpq	%rcx, %rax
	jae	.LBB54_84
# BB#82:                                # %if.then75
	movq	208(%rdx), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	$2, %rcx
.LBB54_83:                              # %if.end85
	movl	%ecx, (%rdx,%rax)
.LBB54_84:                              # %if.end85
	movq	%r12, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp650:
	.size	check_insn_for_bivs, .Ltmp650-check_insn_for_bivs
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI54_0:
	.quad	.LBB54_44
	.quad	.LBB54_81
	.quad	.LBB54_81
	.quad	.LBB54_81
	.quad	.LBB54_44
	.quad	.LBB54_81
	.quad	.LBB54_81
	.quad	.LBB54_28
	.quad	.LBB54_81
	.quad	.LBB54_27
	.quad	.LBB54_81
	.quad	.LBB54_81
	.quad	.LBB54_43
	.quad	.LBB54_81
	.quad	.LBB54_44

	.text
	.align	16, 0x90
	.type	try_copy_prop,@function
try_copy_prop:                          # @try_copy_prop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp657:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp658:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp659:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp660:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp661:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp662:
	.cfi_def_cfa_offset 144
.Ltmp663:
	.cfi_offset %rbx, -48
.Ltmp664:
	.cfi_offset %r12, -40
.Ltmp665:
	.cfi_offset %r13, -32
.Ltmp666:
	.cfi_offset %r14, -24
.Ltmp667:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	%rdi, %r13
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	%rdx, %r14
	movq	216(%r13), %rcx
	movq	80(%rax), %rdx
	andq	%rdi, %r14
	movq	200(%r13), %rdi
	movq	24(%rcx), %rax
	movq	(%rdx,%r14,8), %rdx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	cmpq	%rdi, %rax
	jne	.LBB55_2
# BB#1:                                 # %if.then.i
	movq	208(%r13), %rax
.LBB55_2:                               # %next_insn_in_loop.exit
	xorq	%rbx, %rbx
	cmpq	%rcx, %rax
	je	.LBB55_4
# BB#3:                                 # %next_insn_in_loop.exit
	movq	%rax, %rbx
.LBB55_4:                               # %next_insn_in_loop.exit
	testq	%rbx, %rbx
	je	.LBB55_36
# BB#5:                                 # %for.body.lr.ph
	movq	(%rsp), %rcx            # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	xorq	%r12, %r12
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB55_6:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$36, %rax
	jne	.LBB55_8
# BB#7:                                 # %for.body
                                        #   in Loop: Header=BB55_6 Depth=1
	testq	%r12, %r12
	jne	.LBB55_35
.LBB55_8:                               # %if.end
                                        #   in Loop: Header=BB55_6 Depth=1
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB55_26
# BB#9:                                 # %cond.true
                                        #   in Loop: Header=BB55_6 Depth=1
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB55_11
# BB#10:                                # %cond.false
                                        #   in Loop: Header=BB55_6 Depth=1
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB55_11:                              # %cond.end31
                                        #   in Loop: Header=BB55_6 Depth=1
	testq	%rax, %rax
	je	.LBB55_20
# BB#12:                                # %land.lhs.true34
                                        #   in Loop: Header=BB55_6 Depth=1
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB55_20
# BB#13:                                # %land.lhs.true42
                                        #   in Loop: Header=BB55_6 Depth=1
	movl	8(%rax), %eax
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB55_20
# BB#14:                                # %if.then50
                                        #   in Loop: Header=BB55_6 Depth=1
	testq	%r12, %r12
	jne	.LBB55_15
# BB#17:                                # %if.end53
                                        #   in Loop: Header=BB55_6 Depth=1
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r14,8), %rax
	movl	(%rax), %eax
	movl	8(%rbx), %ecx
	cmpq	%rcx, %rax
	movq	$1, %rax
	je	.LBB55_19
# BB#18:                                # %select.mid
                                        #   in Loop: Header=BB55_6 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
.LBB55_19:                              # %select.end
                                        #   in Loop: Header=BB55_6 Depth=1
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%rbx, %r12
	.align	16, 0x90
.LBB55_20:                              # %if.end62
                                        #   in Loop: Header=BB55_6 Depth=1
	testq	%r12, %r12
	je	.LBB55_26
# BB#21:                                # %if.end62
                                        #   in Loop: Header=BB55_6 Depth=1
	cmpq	%r12, %rbx
	je	.LBB55_26
# BB#22:                                # %if.then67
                                        #   in Loop: Header=BB55_6 Depth=1
	movq	%rbx, 88(%rsp)
	movq	%rbx, 80(%rsp)
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, 64(%rsp)
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rax, 72(%rsp)
	leaq	88(%rsp), %rdi
	movabsq	$replace_loop_reg, %rsi
	leaq	64(%rsp), %rdx
	callq	for_each_rtx
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r14,8), %rax
	movl	4(%rax), %eax
	movl	8(%rbx), %ecx
	cmpq	%rcx, %rax
	movq	$1, %r15
	je	.LBB55_24
# BB#23:                                # %select.mid10
                                        #   in Loop: Header=BB55_6 Depth=1
	movq	32(%rsp), %r15          # 8-byte Reload
.LBB55_24:                              # %select.end9
                                        #   in Loop: Header=BB55_6 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rax, 56(%rsp)
	xorq	%rax, %rax
	movl	%eax, 48(%rsp)
	movq	32(%rbx), %rdi
	movabsq	$note_reg_stored, %rsi
	leaq	48(%rsp), %rdx
	callq	note_stores
	movl	48(%rsp), %eax
	testq	%rax, %rax
	jne	.LBB55_31
# BB#25:                                #   in Loop: Header=BB55_6 Depth=1
	movq	%r15, 32(%rsp)          # 8-byte Spill
.LBB55_26:                              # %for.inc
                                        #   in Loop: Header=BB55_6 Depth=1
	movq	24(%rbx), %rax
	movq	200(%r13), %rcx
	cmpq	%rcx, %rax
	jne	.LBB55_28
# BB#27:                                # %if.then.i174
                                        #   in Loop: Header=BB55_6 Depth=1
	movq	208(%r13), %rax
.LBB55_28:                              # %next_insn_in_loop.exit180
                                        #   in Loop: Header=BB55_6 Depth=1
	movq	216(%r13), %rcx
	xorq	%rbx, %rbx
	cmpq	%rcx, %rax
	je	.LBB55_30
# BB#29:                                # %select.mid21
                                        #   in Loop: Header=BB55_6 Depth=1
	movq	%rax, %rbx
.LBB55_30:                              # %select.end20
                                        #   in Loop: Header=BB55_6 Depth=1
	testq	%rbx, %rbx
	jne	.LBB55_6
	jmp	.LBB55_35
.LBB55_31:                              # %if.then85
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB55_34
# BB#32:                                # %land.lhs.true88
	movq	8(%r14), %rsi
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB55_34
# BB#33:                                # %if.then94
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	remove_note
.LBB55_34:                              # %for.end
	movq	%r15, 32(%rsp)          # 8-byte Spill
.LBB55_35:                              # %for.end
	testq	%r12, %r12
	je	.LBB55_36
# BB#37:                                # %if.end101
	callq	apply_change_group
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB55_50
# BB#38:                                # %if.then104
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB55_40
# BB#39:                                # %if.then106
	movabsq	$.L.str89, %rsi
	xorq	%rax, %rax
	movq	(%rsp), %rdx            # 8-byte Reload
	callq	fprintf
.LBB55_40:                              # %if.end108
	movq	16(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	je	.LBB55_48
# BB#41:                                # %if.end108
	movq	32(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	je	.LBB55_48
# BB#42:                                # %if.then112
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	movq	%r12, %rbx
	je	.LBB55_44
# BB#43:                                # %if.then115
	movq	8(%rax), %rbx
	jmp	.LBB55_44
	.align	16, 0x90
.LBB55_47:                              # %if.end3.i
                                        #   in Loop: Header=BB55_44 Depth=1
	movq	24(%rbx), %rbx
.LBB55_44:                              # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB55_46
# BB#45:                                # %if.then.i165
                                        #   in Loop: Header=BB55_44 Depth=1
	movl	8(%rbx), %edx
	movabsq	$.L.str91, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB55_46:                              # %if.end.i
                                        #   in Loop: Header=BB55_44 Depth=1
	movq	%rbx, %rdi
	callq	delete_insn
	cmpq	%r12, %rbx
	jne	.LBB55_47
.LBB55_48:                              # %if.end120
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB55_50
# BB#49:                                # %if.then122
	movabsq	$.L.str90, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
.LBB55_50:                              # %if.end125
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB55_15:                              # %if.then52
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.try_copy_prop, %rcx
	movq	$9988, %rsi             # imm = 0x2704
	jmp	.LBB55_16
.LBB55_36:                              # %if.then100
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.try_copy_prop, %rcx
	movq	$10022, %rsi            # imm = 0x2726
.LBB55_16:                              # %if.then52
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp668:
	.size	try_copy_prop, .Ltmp668-try_copy_prop
	.cfi_endproc

	.align	16, 0x90
	.type	replace_label,@function
replace_label:                          # @replace_label
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	testq	%rax, %rax
	je	.LBB56_4
# BB#1:                                 # %if.end
	movzbl	(%rax), %ecx
	cmpq	$67, %rcx
	jne	.LBB56_4
# BB#2:                                 # %if.end3
	movq	(%rsi), %rcx
	movq	8(%rax), %rdx
	cmpq	%rcx, %rdx
	jne	.LBB56_4
# BB#3:                                 # %if.end6
	movq	8(%rsi), %rdx
	movq	%rdx, 8(%rax)
	movl	32(%rdx), %eax
	incq	%rax
	movl	%eax, 32(%rdx)
	movl	32(%rcx), %eax
	decq	%rax
	movl	%eax, 32(%rcx)
.LBB56_4:                               # %return
	xorq	%rax, %rax
	retq
.Ltmp669:
	.size	replace_label, .Ltmp669-replace_label
	.cfi_endproc

	.align	16, 0x90
	.type	note_reg_stored,@function
note_reg_stored:                        # @note_reg_stored
	.cfi_startproc
# BB#0:                                 # %entry
	movq	8(%rdx), %rax
	cmpq	%rdi, %rax
	jne	.LBB57_2
# BB#1:                                 # %if.then
	movq	$1, %rax
	movl	%eax, (%rdx)
.LBB57_2:                               # %if.end
	retq
.Ltmp670:
	.size	note_reg_stored, .Ltmp670-note_reg_stored
	.cfi_endproc

	.align	16, 0x90
	.type	replace_loop_reg,@function
replace_loop_reg:                       # @replace_loop_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp672:
	.cfi_def_cfa_offset 16
	movq	%rdi, %rax
	movq	(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB58_3
# BB#1:                                 # %if.end
	movq	(%rsi), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB58_3
# BB#2:                                 # %if.then2
	movq	8(%rsi), %rdx
	movq	16(%rsi), %rdi
	movq	$1, %rcx
	movq	%rax, %rsi
	callq	validate_change
.LBB58_3:                               # %return
	xorq	%rax, %rax
	popq	%rdx
	retq
.Ltmp673:
	.size	replace_loop_reg, .Ltmp673-replace_loop_reg
	.cfi_endproc

	.align	16, 0x90
	.type	replace_loop_mem,@function
replace_loop_mem:                       # @replace_loop_mem
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp678:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp679:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp680:
	.cfi_def_cfa_offset 32
.Ltmp681:
	.cfi_offset %rbx, -32
.Ltmp682:
	.cfi_offset %r14, -24
.Ltmp683:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rax
	movq	%rsi, %r15
	xorq	%r14, %r14
	testq	%rax, %rax
	je	.LBB59_6
# BB#1:                                 # %if.end
	movzbl	(%rax), %ecx
	cmpq	$55, %rcx
	jne	.LBB59_3
# BB#2:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jmp	.LBB59_6
.LBB59_3:                               # %if.end
	cmpq	$66, %rcx
	jne	.LBB59_6
# BB#4:                                 # %sw.epilog
	movq	(%r15), %rdi
	movq	%rax, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB59_6
# BB#5:                                 # %if.end2
	movq	8(%r15), %rdx
	movq	16(%r15), %rdi
	movq	$1, %rcx
	movq	%rbx, %rsi
	callq	validate_change
.LBB59_6:                               # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp684:
	.size	replace_loop_mem, .Ltmp684-replace_loop_mem
	.cfi_endproc

	.align	16, 0x90
	.type	add_label_notes,@function
add_label_notes:                        # @add_label_notes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp691:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp692:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp693:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp694:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp695:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp696:
	.cfi_def_cfa_offset 64
.Ltmp697:
	.cfi_offset %rbx, -48
.Ltmp698:
	.cfi_offset %r12, -40
.Ltmp699:
	.cfi_offset %r13, -32
.Ltmp700:
	.cfi_offset %r14, -24
.Ltmp701:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movl	(%r15), %r12d
	movq	%rsi, %r14
	movq	%r12, %rax
	andq	$134283263, %rax        # imm = 0x800FFFF
	cmpq	$67, %rax
	jne	.LBB60_7
# BB#1:                                 # %entry
	testq	%r14, %r14
	je	.LBB60_7
# BB#2:                                 # %for.body.lr.ph
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%r14, %rbx
	.align	16, 0x90
.LBB60_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r15), %rdi
	movq	%rbx, %rsi
	callq	reg_mentioned_p
	testq	%r13, %rax
	je	.LBB60_6
# BB#4:                                 # %if.then5
                                        #   in Loop: Header=BB60_3 Depth=1
	movq	8(%r15), %rdx
	movq	56(%rbx), %rcx
	movq	$4, %rdi
	movq	$13, %rsi
	callq	gen_rtx_fmt_ue
	movq	%rax, 56(%rbx)
	movq	8(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB60_6
# BB#5:                                 # %if.then22
                                        #   in Loop: Header=BB60_3 Depth=1
	movl	32(%rax), %ecx
	incq	%rcx
	movl	%ecx, 32(%rax)
.LBB60_6:                               # %for.inc
                                        #   in Loop: Header=BB60_3 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB60_3
.LBB60_7:                               # %if.end32
	andq	$65535, %r12            # imm = 0xFFFF
	movzbl	rtx_length(%r12), %r13d
	testq	%r13, %r13
	je	.LBB60_12
# BB#8:                                 # %for.body39.lr.ph
	movq	rtx_format(,%r12,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB60_9:                               # %for.body39
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB60_15 Depth 2
	movq	%r13, %rbx
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	-1(%rax,%rbx), %eax
	leaq	-1(%rbx), %r13
	cmpq	$69, %rax
	jne	.LBB60_10
# BB#14:                                # %if.then55
                                        #   in Loop: Header=BB60_9 Depth=1
	movq	(%r15,%rbx,8), %rax
	movslq	(%rax), %r12
	testq	%r12, %r12
	jle	.LBB60_11
	jmp	.LBB60_15
	.align	16, 0x90
.LBB60_16:                              # %for.body63.for.body63_crit_edge
                                        #   in Loop: Header=BB60_15 Depth=2
	movq	(%r15,%rbx,8), %rax
.LBB60_15:                              # %for.body63.lr.ph
                                        #   Parent Loop BB60_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%r12
	movq	8(%rax,%r12,8), %rdi
	movq	%r14, %rsi
	callq	add_label_notes
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB60_16
	jmp	.LBB60_11
	.align	16, 0x90
.LBB60_10:                              # %for.body39
                                        #   in Loop: Header=BB60_9 Depth=1
	cmpq	$101, %rax
	jne	.LBB60_11
# BB#13:                                # %if.then45
                                        #   in Loop: Header=BB60_9 Depth=1
	movq	(%r15,%rbx,8), %rdi
	movq	%r14, %rsi
	callq	add_label_notes
.LBB60_11:                              # %for.cond36.backedge
                                        #   in Loop: Header=BB60_9 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB60_9
.LBB60_12:                              # %for.end76
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp702:
	.size	add_label_notes, .Ltmp702-add_label_notes
	.cfi_endproc

	.align	16, 0x90
	.type	find_single_use_in_loop,@function
find_single_use_in_loop:                # @find_single_use_in_loop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp709:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp710:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp711:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp712:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp713:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp714:
	.cfi_def_cfa_offset 64
.Ltmp715:
	.cfi_offset %rbx, -48
.Ltmp716:
	.cfi_offset %r12, -40
.Ltmp717:
	.cfi_offset %r13, -32
.Ltmp718:
	.cfi_offset %r14, -24
.Ltmp719:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movq	%rsi, %r14
	movq	%rdi, %r15
	jmp	.LBB61_1
	.align	16, 0x90
.LBB61_12:                              # %if.end
                                        #   in Loop: Header=BB61_1 Depth=1
	movq	16(%rbx), %rbx
.LBB61_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$47, %rax
	jne	.LBB61_2
# BB#10:                                # %if.then21
                                        #   in Loop: Header=BB61_1 Depth=1
	movq	8(%rbx), %rdx
	movzbl	(%rdx), %eax
	cmpq	$61, %rax
	je	.LBB61_12
# BB#11:                                # %if.then27
                                        #   in Loop: Header=BB61_1 Depth=1
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	find_single_use_in_loop
	jmp	.LBB61_12
.LBB61_2:                               # %tailrecurse
	cmpq	$61, %rax
	jne	.LBB61_13
# BB#3:                                 # %if.then
	movl	8(%rbx), %ecx
	movq	8(%r15), %rax
	leaq	(%rcx,%rcx,2), %rcx
	movq	8(%rax,%rcx,8), %rbx
	testq	%rbx, %rbx
	movabsq	$0, %rdx
	movabsq	$1, %rdi
	movq	%rdx, %rsi
	jne	.LBB61_5
# BB#4:                                 # %if.then
	movq	%rdi, %rsi
.LBB61_5:                               # %if.then
	cmpq	%r14, %rbx
	jne	.LBB61_7
# BB#6:                                 # %if.then
	movq	%rdi, %rdx
.LBB61_7:                               # %if.then
	orq	%rdx, %rsi
	jne	.LBB61_9
# BB#8:                                 # %if.then
	movq	const_int_rtx+512(%rip), %r14
.LBB61_9:                               # %if.then
	movq	%r14, 8(%rax,%rcx,8)
	jmp	.LBB61_20
.LBB61_13:                              # %if.else34
	movzbl	rtx_length(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB61_20
# BB#14:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	.align	16, 0x90
.LBB61_15:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB61_22 Depth 2
	movq	%rdx, %r12
	movq	(%rsp), %rax            # 8-byte Reload
	movzbl	-1(%rax,%r12), %eax
	leaq	-1(%r12), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB61_16
# BB#21:                                # %if.then62
                                        #   in Loop: Header=BB61_15 Depth=1
	movq	(%rbx,%r12,8), %rax
	movslq	(%rax), %r13
	testq	%r13, %r13
	jle	.LBB61_19
	jmp	.LBB61_22
	.align	16, 0x90
.LBB61_23:                              # %for.body70.for.body70_crit_edge
                                        #   in Loop: Header=BB61_22 Depth=2
	movq	(%rbx,%r12,8), %rax
.LBB61_22:                              # %for.body70.lr.ph
                                        #   Parent Loop BB61_15 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%r13
	movq	8(%rax,%r13,8), %rdx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	find_single_use_in_loop
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB61_23
	jmp	.LBB61_19
	.align	16, 0x90
.LBB61_16:                              # %for.body
                                        #   in Loop: Header=BB61_15 Depth=1
	cmpq	$101, %rax
	jne	.LBB61_19
# BB#17:                                # %land.lhs.true44
                                        #   in Loop: Header=BB61_15 Depth=1
	movq	(%rbx,%r12,8), %rdx
	testq	%rdx, %rdx
	je	.LBB61_19
# BB#18:                                # %if.then51
                                        #   in Loop: Header=BB61_15 Depth=1
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	find_single_use_in_loop
	.align	16, 0x90
.LBB61_19:                              # %for.cond.backedge
                                        #   in Loop: Header=BB61_15 Depth=1
	movq	$32, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB61_15
.LBB61_20:                              # %if.end83
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp720:
	.size	find_single_use_in_loop, .Ltmp720-find_single_use_in_loop
	.cfi_endproc

	.align	16, 0x90
	.type	count_one_set,@function
count_one_set:                          # @count_one_set
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp727:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp728:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp729:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp730:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp731:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp732:
	.cfi_def_cfa_offset 96
.Ltmp733:
	.cfi_offset %rbx, -48
.Ltmp734:
	.cfi_offset %r12, -40
.Ltmp735:
	.cfi_offset %r13, -32
.Ltmp736:
	.cfi_offset %r14, -24
.Ltmp737:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movl	(%rbx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$49, %rdx
	jne	.LBB62_3
# BB#1:                                 # %land.lhs.true
	movq	8(%rbx), %r8
	movzbl	(%r8), %edx
	cmpq	$61, %rdx
	jne	.LBB62_3
# BB#2:                                 # %if.then
	movl	8(%r8), %eax
	movq	8(%rdi), %r8
	leaq	(%rax,%rax,2), %rax
	movq	$1, %rdx
	movb	%dl, 16(%r8,%rax,8)
	movl	(%rbx), %eax
.LBB62_3:                               # %if.end
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$47, %rax
	jne	.LBB62_5
# BB#4:
	movq	%rcx, %r9
	movq	%rsi, %r8
	jmp	.LBB62_6
.LBB62_5:                               # %if.end
	cmpq	$49, %rax
	movq	%rcx, %r9
	movq	%rsi, %r8
	jne	.LBB62_36
	.align	16, 0x90
.LBB62_6:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rbx
	movzbl	(%rbx), %eax
	leaq	-63(%rax), %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$2, %rcx
	jb	.LBB62_6
# BB#7:                                 # %while.cond
                                        #   in Loop: Header=BB62_6 Depth=1
	leaq	-132(%rax), %rcx
	andq	%rsi, %rcx
	cmpq	$2, %rcx
	jb	.LBB62_6
# BB#8:                                 # %while.cond
	cmpq	$61, %rax
	jne	.LBB62_36
# BB#9:                                 # %if.then40
	movslq	8(%rbx), %r10
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%r10, %rax
	cmpq	$8, %rax
	movabsq	$0, %rax
	jne	.LBB62_11
# BB#10:                                # %if.then40
	movabsq	$1, %rax
.LBB62_11:                              # %if.then40
	leaq	-21(%r10), %rcx
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	orq	%rax, %rcx
	leaq	-45(%r10), %rax
	andq	%rsi, %rax
	cmpq	$8, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
	orq	%rcx, %rax
	leaq	-29(%r10), %rcx
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	orq	%rax, %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	leaq	(%r9,%r10,8), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	$3, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %r11
	xorq	%r15, %r15
	jmp	.LBB62_12
	.align	16, 0x90
.LBB62_35:                              # %if.end154
                                        #   in Loop: Header=BB62_12 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%r8, (%rax,%r15,8)
	incq	%r15
	addq	$24, %r11
	movq	%r12, %rsi
.LBB62_12:                              # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$52, %r10
	movq	$1, %rax
	jg	.LBB62_24
# BB#13:                                # %cond.true
                                        #   in Loop: Header=BB62_12 Depth=1
	movzbl	2(%rbx), %eax
	cmpq	$0, 16(%rsp)            # 8-byte Folded Reload
	je	.LBB62_18
# BB#14:                                # %cond.true60
                                        #   in Loop: Header=BB62_12 Depth=1
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB62_24
# BB#15:                                # %lor.rhs66
                                        #   in Loop: Header=BB62_12 Depth=1
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB62_17
# BB#16:                                # %lor.rhs66
                                        #   in Loop: Header=BB62_12 Depth=1
	movabsq	$1, %rax
.LBB62_17:                              # %lor.rhs66
                                        #   in Loop: Header=BB62_12 Depth=1
	incq	%rax
	jmp	.LBB62_24
	.align	16, 0x90
.LBB62_18:                              # %cond.false
                                        #   in Loop: Header=BB62_12 Depth=1
	movq	%rsi, %r14
	cmpq	$24, %rax
	jne	.LBB62_19
# BB#21:                                # %cond.true85
                                        #   in Loop: Header=BB62_12 Depth=1
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB62_23
.LBB62_19:                              # %cond.false
                                        #   in Loop: Header=BB62_12 Depth=1
	cmpq	$18, %rax
	jne	.LBB62_22
# BB#20:                                # %cond.true78
                                        #   in Loop: Header=BB62_12 Depth=1
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB62_23
.LBB62_22:                              # %cond.false89
                                        #   in Loop: Header=BB62_12 Depth=1
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB62_23:                              # %cond.end107
                                        #   in Loop: Header=BB62_12 Depth=1
	movq	%r14, %rsi
.LBB62_24:                              # %cond.end107
                                        #   in Loop: Header=BB62_12 Depth=1
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jge	.LBB62_36
# BB#25:                                # %for.body
                                        #   in Loop: Header=BB62_12 Depth=1
	movq	8(%rdi), %rax
	leaq	(%r10,%r10,2), %rcx
	movslq	(%rax,%rcx,8), %rcx
	testq	%rcx, %rcx
	jle	.LBB62_28
# BB#26:                                # %for.body
                                        #   in Loop: Header=BB62_12 Depth=1
	testq	%r9, %r9
	jne	.LBB62_28
# BB#27:                                # %if.then119
                                        #   in Loop: Header=BB62_12 Depth=1
	movq	$1, %rcx
	movb	%cl, 16(%rax,%r11)
.LBB62_28:                              # %if.end125
                                        #   in Loop: Header=BB62_12 Depth=1
	movq	%rsi, %r12
	movq	(%r9,%r10,8), %rsi
	testq	%rsi, %rsi
	je	.LBB62_33
# BB#29:                                # %land.lhs.true130
                                        #   in Loop: Header=BB62_12 Depth=1
	movq	%r15, 24(%rsp)          # 8-byte Spill
	movq	%r11, %r15
	movq	%rdi, %r14
	movq	%r9, 32(%rsp)           # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r8, %rdx
	movq	%r8, %r13
	callq	reg_used_between_p
	movq	%r14, %rdi
	testq	%r12, %rax
	jne	.LBB62_31
# BB#30:                                # %land.lhs.true130.if.end140_crit_edge
                                        #   in Loop: Header=BB62_12 Depth=1
	movq	8(%rdi), %rax
	movq	32(%rsp), %r9           # 8-byte Reload
	movq	%r13, %r8
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	%r15, %r11
	jmp	.LBB62_32
	.align	16, 0x90
.LBB62_31:                              # %if.then134
                                        #   in Loop: Header=BB62_12 Depth=1
	movq	8(%rdi), %rax
	movq	%r15, %r11
	movq	$1, %rcx
	movb	%cl, 16(%rax,%r11)
	movq	32(%rsp), %r9           # 8-byte Reload
	movq	%r13, %r8
	movq	8(%rsp), %r10           # 8-byte Reload
.LBB62_32:                              # %if.end140
                                        #   in Loop: Header=BB62_12 Depth=1
	movq	24(%rsp), %r15          # 8-byte Reload
.LBB62_33:                              # %if.end140
                                        #   in Loop: Header=BB62_12 Depth=1
	movslq	(%rax,%r11), %rcx
	cmpq	$126, %rcx
	jg	.LBB62_35
# BB#34:                                # %if.then148
                                        #   in Loop: Header=BB62_12 Depth=1
	incq	%rcx
	movl	%ecx, (%rax,%r11)
	jmp	.LBB62_35
.LBB62_36:                              # %if.end160
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp738:
	.size	count_one_set, .Ltmp738-count_one_set
	.cfi_endproc

	.align	16, 0x90
	.type	note_addr_stored,@function
note_addr_stored:                       # @note_addr_stored
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp741:
	.cfi_def_cfa_offset 16
.Ltmp742:
	.cfi_offset %rbx, -16
	movq	%rdx, %rbx
	movq	%rdi, %rax
	testq	%rax, %rax
	je	.LBB63_7
# BB#1:                                 # %lor.lhs.false
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB63_7
# BB#2:                                 # %if.end
	movl	152(%rbx), %ecx
	incq	%rcx
	movl	%ecx, 152(%rbx)
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$16711680, %rdx         # imm = 0xFF0000
	cmpq	$3342336, %rdx          # imm = 0x330000
	jne	.LBB63_6
# BB#3:                                 # %if.then5
	movq	$1, %rax
	testq	$67108864, %rcx         # imm = 0x4000000
	je	.LBB63_5
# BB#4:                                 # %if.then9
	movl	%eax, 148(%rbx)
	popq	%rbx
	retq
.LBB63_6:                               # %if.end11
	movq	120(%rbx), %rcx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 120(%rbx)
.LBB63_7:                               # %return
	popq	%rbx
	retq
.LBB63_5:                               # %if.else
	movl	%eax, 144(%rbx)
	popq	%rbx
	retq
.Ltmp743:
	.size	note_addr_stored, .Ltmp743-note_addr_stored
	.cfi_endproc

	.align	16, 0x90
	.type	insert_loop_mem,@function
insert_loop_mem:                        # @insert_loop_mem
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp750:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp751:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp752:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp753:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp754:
	.cfi_def_cfa_offset 48
.Ltmp755:
	.cfi_offset %rbx, -48
.Ltmp756:
	.cfi_offset %r12, -40
.Ltmp757:
	.cfi_offset %r13, -32
.Ltmp758:
	.cfi_offset %r14, -24
.Ltmp759:
	.cfi_offset %r15, -16
	movq	(%rdi), %r15
	movq	%rsi, %r14
	xorq	%rax, %rax
	testq	%r15, %r15
	je	.LBB64_22
# BB#1:                                 # %if.end
	movzbl	(%r15), %ecx
	cmpq	$48, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jg	.LBB64_3
# BB#2:                                 # %if.end
	cmpq	$3, %rcx
	jne	.LBB64_21
	jmp	.LBB64_22
.LBB64_3:                               # %if.end
	cmpq	$49, %rcx
	je	.LBB64_22
# BB#4:                                 # %if.end
	cmpq	$55, %rcx
	je	.LBB64_22
# BB#5:                                 # %if.end
	cmpq	$66, %rcx
	jne	.LBB64_21
# BB#6:                                 # %for.cond.preheader
	movslq	136(%r14), %rdx
	testq	%rdx, %rdx
	jle	.LBB64_12
# BB#7:                                 # %for.body.lr.ph
	xorq	%r12, %r12
	movq	$1, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB64_9:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	128(%r14), %rax
	movq	(%rax,%r12), %rsi
	movq	%r15, %rdi
	callq	rtx_equal_p
	testq	%r13, %rax
	jne	.LBB64_10
# BB#8:                                 # %for.cond
                                        #   in Loop: Header=BB64_9 Depth=1
	movslq	136(%r14), %rdx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$24, %r12
	incq	%rbx
	cmpq	%rdx, %rax
	jl	.LBB64_9
.LBB64_12:                              # %for.end
	movl	140(%r14), %ecx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rdx, %rsi
	andq	%rax, %rsi
	cmpq	%rcx, %rsi
	jne	.LBB64_13
# BB#14:                                # %if.then24
	testq	%rax, %rdx
	movq	$32, %rax
	je	.LBB64_16
# BB#15:                                # %if.then24
	addq	%rdx, %rdx
	movq	%rdx, %rax
.LBB64_16:                              # %if.then24
	movl	%eax, 140(%r14)
	movq	128(%r14), %rdi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rsi
	callq	xrealloc
	movq	%rax, 128(%r14)
	movl	136(%r14), %edx
	jmp	.LBB64_17
.LBB64_13:                              # %for.end.if.end36_crit_edge
	movq	128(%r14), %rax
.LBB64_17:                              # %if.end36
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	leaq	(%rsi,%rsi,2), %rcx
	movq	%r15, (%rax,%rcx,8)
	movl	(%r15), %esi
	andq	$16711680, %rsi         # imm = 0xFF0000
	cmpq	$3342336, %rsi          # imm = 0x330000
	je	.LBB64_18
# BB#19:                                # %if.end36
	movabsq	$1, %rsi
	jmp	.LBB64_20
.LBB64_18:
	movabsq	$0, %rsi
.LBB64_20:                              # %if.end36
	movl	%esi, 16(%rax,%rcx,8)
	movq	$0, 8(%rax,%rcx,8)
	incq	%rdx
	movl	%edx, 136(%r14)
.LBB64_21:                              # %return
	xorq	%rax, %rax
.LBB64_22:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB64_10:                              # %if.then5
	movq	128(%r14), %rcx
	movq	(%rcx,%r12), %rax
	movl	(%r15), %edx
	movl	(%rax), %esi
	xorq	%rdx, %rsi
	xorq	%rax, %rax
	testq	$16711680, %rsi         # imm = 0xFF0000
	je	.LBB64_22
# BB#11:                                # %if.then16
	xorq	%rax, %rax
	movl	%eax, 16(%rcx,%r12)
	jmp	.LBB64_22
.Ltmp760:
	.size	insert_loop_mem, .Ltmp760-insert_loop_mem
	.cfi_endproc

	.type	reg_address_cost,@object # @reg_address_cost
	.local	reg_address_cost
	.comm	reg_address_cost,4,4
	.type	copy_cost,@object       # @copy_cost
	.local	copy_cost
	.comm	copy_cost,1,1
	.type	loop_dump_stream,@object # @loop_dump_stream
	.comm	loop_dump_stream,8,8
	.type	max_reg_before_loop,@object # @max_reg_before_loop
	.comm	max_reg_before_loop,4,4
	.type	loop_max_reg,@object    # @loop_max_reg
	.local	loop_max_reg
	.comm	loop_max_reg,4,4
	.type	max_loop_num,@object    # @max_loop_num
	.local	max_loop_num
	.comm	max_loop_num,4,4
	.type	max_uid_for_loop,@object # @max_uid_for_loop
	.comm	max_uid_for_loop,4,4
	.type	uid_luid,@object        # @uid_luid
	.comm	uid_luid,8,8
	.type	uid_loop,@object        # @uid_loop
	.comm	uid_loop,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"loop.c"
	.size	.L.str, 7

	.type	.L__FUNCTION__.loop_optimize,@object # @__FUNCTION__.loop_optimize
.L__FUNCTION__.loop_optimize:
	.asciz	"loop_optimize"
	.size	.L__FUNCTION__.loop_optimize, 14

	.type	indirect_jump_in_function,@object # @indirect_jump_in_function
	.local	indirect_jump_in_function
	.comm	indirect_jump_in_function,4,4
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	";;  No NOTE_INSN_LOOP_BEG at %d\n"
	.size	.L.str1, 33

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	";;  No NOTE_INSN_LOOP_END at %d\n"
	.size	.L.str2, 33

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	";;  start %d (%d), cont dom %d (%d), cont %d (%d), vtop %d (%d), end %d (%d)\n"
	.size	.L.str3, 78

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	";;  top %d (%d), scan start %d (%d)\n"
	.size	.L.str4, 37

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	";;  exit_count %d"
	.size	.L.str5, 18

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	", labels:"
	.size	.L.str6, 10

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	" %d "
	.size	.L.str7, 5

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	";;  NOTE_INSN_LOOP_CONT not in loop latch\n"
	.size	.L.str9, 43

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"Giv %d: insn %d"
	.size	.L.str10, 16

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"Dest address: insn %d"
	.size	.L.str11, 22

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	" src reg %d benefit %d"
	.size	.L.str12, 23

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	" lifetime %d"
	.size	.L.str13, 13

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	" replaceable"
	.size	.L.str14, 13

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	" ncav"
	.size	.L.str15, 6

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	" ext se"
	.size	.L.str16, 8

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	" ext ze"
	.size	.L.str17, 8

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	" ext tr"
	.size	.L.str18, 8

	.type	.L__FUNCTION__.loop_giv_dump,@object # @__FUNCTION__.loop_giv_dump
.L__FUNCTION__.loop_giv_dump:
	.asciz	"loop_giv_dump"
	.size	.L__FUNCTION__.loop_giv_dump, 14

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	" mult "
	.size	.L.str19, 7

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	" add  "
	.size	.L.str20, 7

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	" final "
	.size	.L.str21, 8

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"Biv %d: insn %d"
	.size	.L.str22, 16

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	" const "
	.size	.L.str23, 8

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"IV class for reg %d, benefit %d\n"
	.size	.L.str24, 33

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	" Init insn %d"
	.size	.L.str25, 14

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	", init val: "
	.size	.L.str26, 13

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	", init test: "
	.size	.L.str27, 14

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	" Final val: "
	.size	.L.str28, 13

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	" Total increment: "
	.size	.L.str29, 19

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	" Inc%d: insn %d, incr: "
	.size	.L.str30, 24

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	" Giv%d: insn %d, benefit %d, "
	.size	.L.str31, 30

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"Loop %d: %d IV classes\n"
	.size	.L.str32, 24

	.type	.L__FUNCTION__.find_and_verify_loops,@object # @__FUNCTION__.find_and_verify_loops
.L__FUNCTION__.find_and_verify_loops:
	.asciz	"find_and_verify_loops"
	.size	.L__FUNCTION__.find_and_verify_loops, 22

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"\nLoop at %d ignored due to setjmp.\n"
	.size	.L.str33, 36

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"\nLoop at %d ignored due to multiple entry points.\n"
	.size	.L.str34, 51

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"\nLoop at %d ignored due to unknown exit jump.\n"
	.size	.L.str35, 47

	.type	.L__FUNCTION__.scan_loop,@object # @__FUNCTION__.scan_loop
.L__FUNCTION__.scan_loop:
	.asciz	"scan_loop"
	.size	.L__FUNCTION__.scan_loop, 10

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"\nLoop from %d to %d is phony.\n\n"
	.size	.L.str36, 32

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"\nLoop from %d to %d: %d real insns.\n"
	.size	.L.str37, 37

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"Continue at insn %d.\n"
	.size	.L.str38, 22

	.type	addr_placeholder,@object # @addr_placeholder
	.local	addr_placeholder
	.comm	addr_placeholder,8,8
	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"giv of insn %d not worth while, %d vs %d.\n"
	.size	.L.str39, 43

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"giv of insn %d: would need a multiply.\n"
	.size	.L.str40, 40

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"Reg %d: biv eliminated\n"
	.size	.L.str41, 24

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"Cannot eliminate biv %d: biv used in insn %d.\n"
	.size	.L.str42, 47

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"biv %d %s eliminated.\n"
	.size	.L.str43, 23

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"was"
	.size	.L.str44, 4

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"can be"
	.size	.L.str45, 7

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"giv at %d reduced to "
	.size	.L.str46, 22

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"Sorted combine statistics:\n"
	.size	.L.str47, 28

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	" {%d, %d}"
	.size	.L.str48, 10

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"giv at %d combined with giv at %d; new benefit %d + %d, lifetime %d\n"
	.size	.L.str49, 69

	.type	.L__FUNCTION__.check_ext_dependent_givs,@object # @__FUNCTION__.check_ext_dependent_givs
.L__FUNCTION__.check_ext_dependent_givs:
	.asciz	"check_ext_dependent_givs"
	.size	.L__FUNCTION__.check_ext_dependent_givs, 25

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"Verified ext dependent giv at %d of reg %d\n"
	.size	.L.str50, 44

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"biv iteration values overflowed"
	.size	.L.str51, 32

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"biv iteration info incomplete; incr by 1"
	.size	.L.str52, 41

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"biv iteration info incomplete"
	.size	.L.str53, 30

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"Failed ext dependent giv at %d, %s\n"
	.size	.L.str54, 36

	.type	.L__FUNCTION__.loop_biv_eliminable_p,@object # @__FUNCTION__.loop_biv_eliminable_p
.L__FUNCTION__.loop_biv_eliminable_p:
	.asciz	"loop_biv_eliminable_p"
	.size	.L__FUNCTION__.loop_biv_eliminable_p, 22

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"Cannot eliminate biv %d.\n"
	.size	.L.str55, 26

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"First use: insn %d, last use: insn %d.\n"
	.size	.L.str56, 40

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"Can reverse loop\n"
	.size	.L.str57, 18

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"Reversed loop"
	.size	.L.str58, 14

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	" and added reg_nonneg\n"
	.size	.L.str59, 23

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"Found branch outside giv lifetime.\n"
	.size	.L.str60, 36

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"Insn %d: giv reg %d final_value replaceable\n"
	.size	.L.str61, 45

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"Prefetch: ignoring loop - has call.\n"
	.size	.L.str62, 37

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"Prefetch: ignoring loop - not enought iterations.\n"
	.size	.L.str63, 51

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"Prefetch: biv %i ignored: non-constant addition at insn %i:"
	.size	.L.str64, 60

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"Prefetch: biv %i ignored: maybe_multiple at insn %i:"
	.size	.L.str65, 53

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"Prefetch: Ignoring giv at %i\n"
	.size	.L.str66, 30

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"Maximal number of prefetches exceeded.\n"
	.size	.L.str67, 40

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"Prefetch insn %i address: "
	.size	.L.str68, 27

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	" Index: "
	.size	.L.str69, 9

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"%lld"
	.size	.L.str70, 5

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	" stride: "
	.size	.L.str71, 10

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	" density: %i%% total_bytes: %u%sin loop: %s before: %s\n"
	.size	.L.str72, 56

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	" read/write "
	.size	.L.str73, 13

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	" read only "
	.size	.L.str74, 12

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"yes"
	.size	.L.str75, 4

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"no"
	.size	.L.str76, 3

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"Real prefetches needed: %i (write: %i)\n"
	.size	.L.str77, 40

	.type	.L__FUNCTION__.simplify_giv_expr,@object # @__FUNCTION__.simplify_giv_expr
.L__FUNCTION__.simplify_giv_expr:
	.asciz	"simplify_giv_expr"
	.size	.L__FUNCTION__.simplify_giv_expr, 18

	.type	.L__FUNCTION__.record_giv,@object # @__FUNCTION__.record_giv
.L__FUNCTION__.record_giv:
	.asciz	"record_giv"
	.size	.L__FUNCTION__.record_giv, 11

	.type	.L__FUNCTION__.general_induction_var,@object # @__FUNCTION__.general_induction_var
.L__FUNCTION__.general_induction_var:
	.asciz	"general_induction_var"
	.size	.L__FUNCTION__.general_induction_var, 22

	.type	.L__FUNCTION__.rtx_equal_for_prefetch_p,@object # @__FUNCTION__.rtx_equal_for_prefetch_p
.L__FUNCTION__.rtx_equal_for_prefetch_p:
	.asciz	"rtx_equal_for_prefetch_p"
	.size	.L__FUNCTION__.rtx_equal_for_prefetch_p, 25

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"Biv %d: initialized at insn %d: initial value "
	.size	.L.str78, 47

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"is complex\n"
	.size	.L.str79, 12

	.type	note_insn,@object       # @note_insn
	.local	note_insn
	.comm	note_insn,8,8
	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"Biv %d: discarded, %s\n"
	.size	.L.str80, 23

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"not induction variable"
	.size	.L.str81, 23

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"never incremented"
	.size	.L.str82, 18

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"count error"
	.size	.L.str83, 12

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"Biv %d: verified\n"
	.size	.L.str84, 18

	.type	.L__FUNCTION__.load_mems,@object # @__FUNCTION__.load_mems
.L__FUNCTION__.load_mems:
	.asciz	"load_mems"
	.size	.L__FUNCTION__.load_mems, 10

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"Hoisted regno %d %s from "
	.size	.L.str85, 26

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"r/w"
	.size	.L.str86, 4

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"r/o"
	.size	.L.str87, 4

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"  Swapped set of reg %d at %d with reg %d at %d.\n"
	.size	.L.str88, 50

	.type	.L__FUNCTION__.try_copy_prop,@object # @__FUNCTION__.try_copy_prop
.L__FUNCTION__.try_copy_prop:
	.asciz	"try_copy_prop"
	.size	.L__FUNCTION__.try_copy_prop, 14

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"  Replaced reg %d"
	.size	.L.str89, 18

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	".\n"
	.size	.L.str90, 3

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	", deleting init_insn (%d)"
	.size	.L.str91, 26

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"Insn %d: regno %d (life %d), "
	.size	.L.str92, 30

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	"consec %d, "
	.size	.L.str93, 12

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"cond "
	.size	.L.str94, 6

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	"force "
	.size	.L.str95, 7

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"global "
	.size	.L.str96, 8

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"done "
	.size	.L.str97, 6

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"move-insn "
	.size	.L.str98, 11

	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	"matches %d "
	.size	.L.str99, 12

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"forces %d "
	.size	.L.str100, 11

	.type	.L.str101,@object       # @.str101
.L.str101:
	.asciz	"savings %d "
	.size	.L.str101, 12

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"halved since already moved "
	.size	.L.str102, 28

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	" moved to %d"
	.size	.L.str103, 13

	.type	.L__FUNCTION__.move_movables,@object # @__FUNCTION__.move_movables
.L__FUNCTION__.move_movables:
	.asciz	"move_movables"
	.size	.L__FUNCTION__.move_movables, 14

	.type	.L.str104,@object       # @.str104
.L.str104:
	.asciz	"not desirable"
	.size	.L.str104, 14

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	"not safe"
	.size	.L.str105, 9

	.type	.L__FUNCTION__.labels_in_range_p,@object # @__FUNCTION__.labels_in_range_p
.L__FUNCTION__.labels_in_range_p:
	.asciz	"labels_in_range_p"
	.size	.L__FUNCTION__.labels_in_range_p, 18


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
