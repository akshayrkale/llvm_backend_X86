	.file	"expr.c.bc"
	.text
	.globl	init_expr_once
	.align	16, 0x90
	.type	init_expr_once,@function
init_expr_once:                         # @init_expr_once
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
	subq	$32, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 80
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
	callq	start_sequence
	movq	global_rtl+16(%rip), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_MEM
	movq	global_rtl+24(%rip), %rsi
	movq	%rax, 16(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	callq	gen_rtx_MEM
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$47, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rax, %r12
	movq	32(%r12), %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	xorq	%r13, %r13
	.align	16, 0x90
.LBB0_1:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_3 Depth 2
	xorq	%rax, %rax
	movb	%al, direct_store(%r13)
	movb	%al, direct_load(%r13)
	movq	16(%rsp), %rsi          # 8-byte Reload
	movl	(%rsi), %eax
	movq	$16, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movabsq	$4278255615, %rcx       # imm = 0xFF00FFFF
	movq	%rcx, %rdi
	andq	%rdi, %rax
	orq	%rdx, %rax
	movl	%eax, (%rsi)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movl	(%rcx), %eax
	andq	%rdi, %rax
	orq	%rdx, %rax
	movl	%eax, (%rcx)
	testq	%r15, %r13
	je	.LBB0_15
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r13, %rax
	andq	%r15, %rax
	cmpq	$51, %rax
	movabsq	$0, %r14
	je	.LBB0_15
	.align	16, 0x90
.LBB0_3:                                # %land.rhs
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	direct_load(%r13), %eax
	testq	%rax, %rax
	je	.LBB0_5
# BB#4:                                 # %lor.rhs
                                        #   in Loop: Header=BB0_3 Depth=2
	movzbl	direct_store(%r13), %eax
	testq	%rax, %rax
	jne	.LBB0_15
.LBB0_5:                                # %for.body25
                                        #   in Loop: Header=BB0_3 Depth=2
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	ix86_hard_regno_mode_ok
	testq	%r15, %rax
	je	.LBB0_14
# BB#6:                                 # %if.end
                                        #   in Loop: Header=BB0_3 Depth=2
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, 16(%rbx)
	movq	%r15, 8(%rbx)
	movq	%rbx, %rdi
	movq	%r12, %rsi
	leaq	28(%rsp), %rdx
	callq	recog
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB0_8
# BB#7:                                 # %if.then38
                                        #   in Loop: Header=BB0_3 Depth=2
	movq	$1, %rax
	movb	%al, direct_load(%r13)
.LBB0_8:                                # %if.end41
                                        #   in Loop: Header=BB0_3 Depth=2
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, 16(%rbx)
	movq	%r15, 8(%rbx)
	movq	%rbx, %rdi
	movq	%r12, %rsi
	leaq	28(%rsp), %rdx
	callq	recog
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB0_10
# BB#9:                                 # %if.then51
                                        #   in Loop: Header=BB0_3 Depth=2
	movq	$1, %rax
	movb	%al, direct_load(%r13)
.LBB0_10:                               # %if.end54
                                        #   in Loop: Header=BB0_3 Depth=2
	movq	%r15, 16(%rbx)
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%rbx)
	movq	%rbx, %rdi
	movq	%r12, %rsi
	leaq	28(%rsp), %rdx
	callq	recog
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB0_12
# BB#11:                                # %if.then64
                                        #   in Loop: Header=BB0_3 Depth=2
	movq	$1, %rax
	movb	%al, direct_store(%r13)
.LBB0_12:                               # %if.end67
                                        #   in Loop: Header=BB0_3 Depth=2
	movq	%r15, 16(%rbx)
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, 8(%rbx)
	movq	%rbx, %rdi
	movq	%r12, %rsi
	leaq	28(%rsp), %rdx
	callq	recog
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	js	.LBB0_14
# BB#13:                                # %if.then77
                                        #   in Loop: Header=BB0_3 Depth=2
	movq	$1, %rax
	movb	%al, direct_store(%r13)
.LBB0_14:                               # %for.inc
                                        #   in Loop: Header=BB0_3 Depth=2
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jl	.LBB0_3
	.align	16, 0x90
.LBB0_15:                               # %for.inc82
                                        #   in Loop: Header=BB0_1 Depth=1
	incq	%r13
	cmpq	$59, %r13
	jne	.LBB0_1
# BB#16:                                # %for.end83
	callq	end_sequence
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	init_expr_once, .Ltmp17-init_expr_once
	.cfi_endproc

	.globl	init_expr
	.align	16, 0x90
	.type	init_expr,@function
init_expr:                              # @init_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp19:
	.cfi_def_cfa_offset 16
	movq	$48, %rdi
	callq	xmalloc
	movq	cfun(%rip), %rcx
	movq	%rax, 16(%rcx)
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	$0, 40(%rax)
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movl	%ecx, 8(%rax)
	movl	%ecx, 4(%rax)
	movq	$0, 16(%rax)
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	$0, 24(%rax)
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	$0, 32(%rax)
	popq	%rax
	retq
.Ltmp20:
	.size	init_expr, .Ltmp20-init_expr
	.cfi_endproc

	.globl	mark_expr_status
	.align	16, 0x90
	.type	mark_expr_status,@function
mark_expr_status:                       # @mark_expr_status
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp24:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp25:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp26:
	.cfi_def_cfa_offset 32
.Ltmp27:
	.cfi_offset %rbx, -24
.Ltmp28:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB2_9
# BB#1:                                 # %do.body
	movq	16(%rbx), %r14
	testq	%r14, %r14
	je	.LBB2_4
# BB#2:                                 # %land.lhs.true
	movq	%r14, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_4
# BB#3:                                 # %if.then2
	movq	%r14, %rdi
	callq	ggc_mark_rtx_children
.LBB2_4:                                # %do.body4
	movq	24(%rbx), %r14
	testq	%r14, %r14
	je	.LBB2_7
# BB#5:                                 # %land.lhs.true7
	movq	%r14, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_7
# BB#6:                                 # %if.then10
	movq	%r14, %rdi
	callq	ggc_mark_rtx_children
.LBB2_7:                                # %do.body13
	movq	32(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB2_9
# BB#8:                                 # %land.lhs.true16
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_10
.LBB2_9:                                # %do.end21
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB2_10:                               # %if.then19
	movq	%rbx, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	ggc_mark_rtx_children  # TAILCALL
.Ltmp29:
	.size	mark_expr_status, .Ltmp29-mark_expr_status
	.cfi_endproc

	.globl	free_expr_status
	.align	16, 0x90
	.type	free_expr_status,@function
free_expr_status:                       # @free_expr_status
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp32:
	.cfi_def_cfa_offset 16
.Ltmp33:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	16(%rbx), %rdi
	callq	free
	movq	$0, 16(%rbx)
	popq	%rbx
	retq
.Ltmp34:
	.size	free_expr_status, .Ltmp34-free_expr_status
	.cfi_endproc

	.globl	finish_expr_for_function
	.align	16, 0x90
	.type	finish_expr_for_function,@function
finish_expr_for_function:               # @finish_expr_for_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp36:
	.cfi_def_cfa_offset 16
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	cmpq	$0, 40(%rax)
	jne	.LBB4_2
# BB#1:                                 # %if.end
	popq	%rax
	retq
.LBB4_2:                                # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.finish_expr_for_function, %rcx
	movq	$313, %rsi              # imm = 0x139
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp37:
	.size	finish_expr_for_function, .Ltmp37-finish_expr_for_function
	.cfi_endproc

	.globl	protect_from_queue
	.align	16, 0x90
	.type	protect_from_queue,@function
protect_from_queue:                     # @protect_from_queue
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp43:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp44:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp45:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp46:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp47:
	.cfi_def_cfa_offset 48
.Ltmp48:
	.cfi_offset %rbx, -40
.Ltmp49:
	.cfi_offset %r12, -32
.Ltmp50:
	.cfi_offset %r14, -24
.Ltmp51:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	(%r14), %ecx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$71, %rax
	jne	.LBB5_1
# BB#22:                                # %if.end84
	cmpq	$0, 16(%r14)
	je	.LBB5_26
# BB#23:                                # %if.end94
	movq	24(%r14), %rbx
	testq	%rbx, %rbx
	jne	.LBB5_25
# BB#24:                                # %if.end103
	movq	8(%r14), %rax
	movzbl	2(%rax), %edi
	callq	gen_reg_rtx
	movq	%rax, 24(%r14)
	movq	8(%r14), %rsi
	movq	%rax, %rdi
	callq	gen_move_insn
	movq	16(%r14), %rsi
	movq	%rax, %rdi
	callq	emit_insn_before
	movq	24(%r14), %rbx
	jmp	.LBB5_25
.LBB5_1:                                # %if.then
	cmpq	$66, %rax
	je	.LBB5_2
# BB#3:                                 # %if.then
	movabsq	$1, %rdx
	jmp	.LBB5_4
.LBB5_2:
	movabsq	$0, %rdx
.LBB5_4:                                # %if.then
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$3342336, %rcx          # imm = 0x330000
	je	.LBB5_11
# BB#5:                                 # %if.then
	testq	%rdx, %rdx
	jne	.LBB5_11
# BB#6:                                 # %land.lhs.true5
	movq	8(%r14), %r12
	movzbl	(%r12), %ecx
	cmpq	$71, %rcx
	jne	.LBB5_11
# BB#7:                                 # %land.lhs.true5
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rsi
	jne	.LBB5_11
# BB#8:                                 # %if.then10
	movq	8(%r12), %rsi
	movq	%r14, %rdi
	callq	replace_equiv_address_nv
	cmpq	$0, 16(%r12)
	movq	%rax, %r15
	je	.LBB5_10
# BB#9:                                 # %if.then21
	movzbl	2(%r14), %edi
	callq	gen_reg_rtx
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	gen_move_insn
	movq	16(%r12), %rsi
	movq	%rax, %rdi
	callq	emit_insn_before
	jmp	.LBB5_25
.LBB5_11:                               # %if.end36
	cmpq	$66, %rax
	jne	.LBB5_15
# BB#12:                                # %if.then38
	movq	8(%r14), %rdi
	xorq	%rsi, %rsi
	callq	protect_from_queue
	movq	8(%r14), %rcx
	movq	%rax, %r15
	cmpq	%rcx, %r15
	je	.LBB5_13
# BB#14:                                # %if.then47
	movq	%r14, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%r15, 8(%rbx)
	jmp	.LBB5_25
.LBB5_26:                               # %if.then89
	movq	8(%r14), %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	copy_to_reg  # TAILCALL
.LBB5_15:                               # %if.else
	cmpq	$75, %rax
	je	.LBB5_18
# BB#16:                                # %if.else
	cmpq	$78, %rax
	jne	.LBB5_17
.LBB5_18:                               # %if.then55
	movq	8(%r14), %rdi
	xorq	%rsi, %rsi
	callq	protect_from_queue
	movq	16(%r14), %rdi
	movq	%rax, %r15
	xorq	%rsi, %rsi
	callq	protect_from_queue
	movq	8(%r14), %rcx
	movq	%rax, %r12
	cmpq	%rcx, %r15
	jne	.LBB5_21
# BB#19:                                # %lor.lhs.false68
	movq	16(%r14), %rax
	cmpq	%rax, %r12
	je	.LBB5_20
.LBB5_21:                               # %if.then73
	movq	%r14, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%r15, 8(%rbx)
	movq	%r12, 16(%rbx)
	jmp	.LBB5_25
.LBB5_13:
	movq	%r14, %rbx
	jmp	.LBB5_25
.LBB5_10:                               # %if.end
	movq	8(%r15), %rdi
	callq	copy_to_reg
	movq	%r15, %rdi
	movq	%rax, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	replace_equiv_address  # TAILCALL
.LBB5_17:
	movq	%r14, %rbx
	jmp	.LBB5_25
.LBB5_20:
	movq	%r14, %rbx
.LBB5_25:                               # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp52:
	.size	protect_from_queue, .Ltmp52-protect_from_queue
	.cfi_endproc

	.globl	queued_subexp_p
	.align	16, 0x90
	.type	queued_subexp_p,@function
queued_subexp_p:                        # @queued_subexp_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp56:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp57:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp58:
	.cfi_def_cfa_offset 32
.Ltmp59:
	.cfi_offset %rbx, -24
.Ltmp60:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	xorq	%rax, %rax
	jmp	.LBB6_1
	.align	16, 0x90
.LBB6_3:                                # %sw.bb1
                                        #   in Loop: Header=BB6_1 Depth=1
	movq	8(%rbx), %rbx
.LBB6_1:                                # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %ecx
	cmpq	$70, %rcx
	jg	.LBB6_4
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB6_1 Depth=1
	cmpq	$66, %rcx
	je	.LBB6_3
	jmp	.LBB6_12
.LBB6_4:                                # %tailrecurse
	leaq	-75(%rcx), %rdx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rdx
	cmpq	$2, %rdx
	jb	.LBB6_7
# BB#5:                                 # %tailrecurse
	cmpq	$71, %rcx
	jne	.LBB6_6
# BB#11:                                # %return.loopexit
	movq	$1, %rax
	jmp	.LBB6_12
.LBB6_6:                                # %tailrecurse
	cmpq	$78, %rcx
	jne	.LBB6_12
.LBB6_7:                                # %sw.bb2
	movq	8(%rbx), %rdi
	callq	queued_subexp_p
	testq	%r14, %rax
	movq	$1, %rax
	jne	.LBB6_12
# BB#8:                                 # %lor.rhs
	movq	16(%rbx), %rdi
	callq	queued_subexp_p
	testq	%r14, %rax
	je	.LBB6_9
# BB#10:                                # %lor.rhs
	movabsq	$1, %rax
	jmp	.LBB6_12
.LBB6_9:
	movabsq	$0, %rax
.LBB6_12:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp61:
	.size	queued_subexp_p, .Ltmp61-queued_subexp_p
	.cfi_endproc

	.globl	emit_queue
	.align	16, 0x90
	.type	emit_queue,@function
emit_queue:                             # @emit_queue
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp64:
	.cfi_def_cfa_offset 16
.Ltmp65:
	.cfi_offset %rbx, -16
	jmp	.LBB7_1
	.align	16, 0x90
.LBB7_5:                                # %if.end
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rbx), %rcx
	movq	16(%rax), %rax
	movq	%rcx, 40(%rax)
.LBB7_1:                                # %entry
                                        # =>This Inner Loop Header: Depth=1
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB7_6
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$24, %rax
	jne	.LBB7_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	8(%rdi), %rax
	movq	8(%rax), %rax
	movq	%rax, 16(%rbx)
	callq	emit_insn
	jmp	.LBB7_5
	.align	16, 0x90
.LBB7_4:                                # %if.else
                                        #   in Loop: Header=BB7_1 Depth=1
	callq	emit_insn
	movq	%rax, 16(%rbx)
	jmp	.LBB7_5
.LBB7_6:                                # %while.end
	popq	%rbx
	retq
.Ltmp66:
	.size	emit_queue, .Ltmp66-emit_queue
	.cfi_endproc

	.globl	convert_move
	.align	16, 0x90
	.type	convert_move,@function
convert_move:                           # @convert_move
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp73:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp74:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp75:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp76:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp77:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp78:
	.cfi_def_cfa_offset 96
.Ltmp79:
	.cfi_offset %rbx, -48
.Ltmp80:
	.cfi_offset %r12, -40
.Ltmp81:
	.cfi_offset %r13, -32
.Ltmp82:
	.cfi_offset %r14, -24
.Ltmp83:
	.cfi_offset %r15, -16
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rsi, %rax
	jmp	.LBB8_1
	.align	16, 0x90
.LBB8_135:                              # %if.end443
                                        #   in Loop: Header=BB8_1 Depth=1
	movl	word_mode(%rip), %edi
	callq	gen_lowpart
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movq	32(%rsp), %rdi          # 8-byte Reload
.LBB8_1:                                # %tailrecurse
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_176 Depth 2
	movabsq	$0, %rcx
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movzbl	2(%rdi), %r15d
	movl	mode_class(,%r15,4), %edx
	movzbl	2(%rax), %r14d
	cmpq	$2, %rdx
	movq	%rcx, %rbx
	jne	.LBB8_3
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB8_1 Depth=1
	movabsq	$1, %rbx
.LBB8_3:                                # %tailrecurse
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movl	mode_class(,%r14,4), %eax
	cmpq	$2, %rax
	movq	%rcx, %r12
	jne	.LBB8_5
# BB#4:                                 # %tailrecurse
                                        #   in Loop: Header=BB8_1 Depth=1
	movabsq	$1, %r12
.LBB8_5:                                # %tailrecurse
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	$1, %rsi
	callq	protect_from_queue
	movq	%rax, %r13
	xorq	%rsi, %rsi
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	xorq	%r12, %rbx
	jne	.LBB8_6
# BB#8:                                 # %if.end
                                        #   in Loop: Header=BB8_1 Depth=1
	movl	(%rax), %esi
	movq	%rsi, %rcx
	andq	$268500991, %rcx        # imm = 0x1000FFFF
	cmpq	$268435519, %rcx        # imm = 0x1000003F
	movq	%r14, %rdi
	jne	.LBB8_9
# BB#10:                                # %land.lhs.true19
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	8(%rax), %rcx
	movzbl	2(%rcx), %ecx
	movzbl	mode_size(%rcx), %ecx
	movzbl	mode_size(%r15), %edx
	cmpq	%rdx, %rcx
	movq	%r13, %r8
	jae	.LBB8_12
# BB#11:                                #   in Loop: Header=BB8_1 Depth=1
	movq	%r15, %r10
	jmp	.LBB8_15
	.align	16, 0x90
.LBB8_9:                                #   in Loop: Header=BB8_1 Depth=1
	movq	%r13, %r8
	movq	%r15, %r10
	jmp	.LBB8_15
	.align	16, 0x90
.LBB8_12:                               # %land.lhs.true32
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	$26, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	movq	40(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rcx, %rsi
	jne	.LBB8_13
# BB#14:                                # %if.then38
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	%r15, %rbx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movq	%r8, %r14
	callq	gen_lowpart
	movq	%rbx, %r10
	movq	%r14, %r8
	movq	%r10, %rdi
	jmp	.LBB8_15
.LBB8_13:                               #   in Loop: Header=BB8_1 Depth=1
	movq	%r15, %r10
	.align	16, 0x90
.LBB8_15:                               # %if.end40
                                        #   in Loop: Header=BB8_1 Depth=1
	movl	(%r8), %edx
	movq	%rdx, %rcx
	andq	$268500991, %rcx        # imm = 0x1000FFFF
	cmpq	$268435519, %rcx        # imm = 0x1000003F
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	24(%rsp), %rsi          # 8-byte Reload
	je	.LBB8_16
# BB#17:                                # %if.end51
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	%rdi, %r10
	je	.LBB8_28
# BB#18:                                # %lor.lhs.false
                                        #   in Loop: Header=BB8_1 Depth=1
	testq	%rdi, %rdi
	jne	.LBB8_22
# BB#19:                                # %land.lhs.true56
                                        #   in Loop: Header=BB8_1 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$133, %rcx
	jg	.LBB8_26
# BB#20:                                # %land.lhs.true56
                                        #   in Loop: Header=BB8_1 Depth=1
	addq	$-54, %rcx
	andq	%r15, %rcx
	cmpq	$14, %rcx
	ja	.LBB8_22
# BB#21:                                # %land.lhs.true56
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	$1, %rbx
	shlq	%cl, %rbx
	testq	$24599, %rbx            # imm = 0x6017
	je	.LBB8_22
	jmp	.LBB8_28
	.align	16, 0x90
.LBB8_26:                               # %land.lhs.true56
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$134, %rcx
	je	.LBB8_28
# BB#27:                                # %land.lhs.true56
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$140, %rcx
	je	.LBB8_28
	.align	16, 0x90
.LBB8_22:                               # %if.end98
                                        #   in Loop: Header=BB8_1 Depth=1
	leaq	-7(%rsi), %rbx
	andq	%r15, %rbx
	cmpq	$2, %rbx
	jb	.LBB8_24
# BB#23:                                # %lor.lhs.false108
                                        #   in Loop: Header=BB8_1 Depth=1
	movl	mode_class(,%rdi,4), %ecx
	addq	$-7, %rcx
	andq	%r15, %rcx
	cmpq	$1, %rcx
	jbe	.LBB8_24
# BB#33:                                # %if.end153
                                        #   in Loop: Header=BB8_1 Depth=1
	movzbl	mode_bitsize(%rdi,%rdi), %ebx
	movq	%rdi, %r13
	movzbl	mode_bitsize(%r10,%r10), %r14d
	movq	%rax, %r12
	cmpq	$2, %rsi
	je	.LBB8_34
# BB#91:                                # %if.end258
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	%r8, 32(%rsp)           # 8-byte Spill
	movl	target_flags(%rip), %edx
	andq	$65535, %rbx            # imm = 0xFFFF
	cmpq	%r14, %rbx
	jae	.LBB8_125
# BB#92:                                # %land.lhs.true267
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	%rdx, %rax
	andq	$33554432, %rax         # imm = 0x2000000
	movq	$20, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	addq	$32, %rsi
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rcx, %rsi
	cmpq	%rsi, %r14
	ja	.LBB8_93
.LBB8_125:                              # %if.end392
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movq	%rdx, %rax
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rcx, %rax
	cmpq	%rax, %rbx
	jbe	.LBB8_136
# BB#126:                               # %if.end392
                                        #   in Loop: Header=BB8_1 Depth=1
	andq	%r15, %rdx
	cmpq	%rdx, %r14
	ja	.LBB8_136
# BB#127:                               # %if.then410
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	%r12, %rsi
	movl	(%rsi), %eax
	movq	%rax, %rcx
	andq	$134283263, %rcx        # imm = 0x800FFFF
	cmpq	$66, %rcx
	jne	.LBB8_128
# BB#129:                               # %land.lhs.true420
                                        #   in Loop: Header=BB8_1 Depth=1
	movzbl	direct_load(%r10), %ecx
	testq	%rcx, %rcx
	movq	%r13, %rbx
	je	.LBB8_133
# BB#130:                               # %land.lhs.true425
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	8(%rsi), %rdi
	movq	%rsi, %r14
	callq	mode_dependent_address_p
	testq	%r15, %rax
	jne	.LBB8_132
# BB#131:                               #   in Loop: Header=BB8_1 Depth=1
	movq	%r14, %rsi
	jmp	.LBB8_135
	.align	16, 0x90
.LBB8_136:                              # %if.end445
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$8, %r10
	movq	%r12, %rax
	je	.LBB8_137
# BB#140:                               # %if.end454
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	%r13, %r12
	cmpq	$8, %r12
	jne	.LBB8_143
# BB#141:                               # %if.then457
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$2, %r10
	movq	%r10, %rbx
	je	.LBB8_220
# BB#142:                               # %if.then460
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	$2, %rdi
	xorq	%rsi, %rsi
	movq	$2, %r12
	movq	%rax, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	convert_modes
	movq	%rbx, %r10
.LBB8_143:                              # %if.end464
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$10, %r10
	je	.LBB8_144
# BB#147:                               # %if.end473
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$10, %r12
	jne	.LBB8_150
# BB#148:                               # %if.then476
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$4, %r10
	movq	%r10, %rbx
	je	.LBB8_221
# BB#149:                               # %if.then479
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	$4, %rdi
	xorq	%rsi, %rsi
	movq	$4, %r12
	movq	%rax, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	convert_modes
	movq	%rbx, %r10
.LBB8_150:                              # %if.end483
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$11, %r10
	je	.LBB8_151
# BB#154:                               # %if.end492
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$11, %r12
	jne	.LBB8_157
# BB#155:                               # %if.then495
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$5, %r10
	movq	%r10, %rbx
	je	.LBB8_222
# BB#156:                               # %if.then498
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	$5, %rdi
	xorq	%rsi, %rsi
	movq	$5, %r12
	movq	%rax, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	convert_modes
	movq	%rbx, %r10
.LBB8_157:                              # %if.end502
                                        #   in Loop: Header=BB8_1 Depth=1
	movzbl	mode_bitsize(%r12,%r12), %ecx
	cmpq	%rcx, %r14
	jb	.LBB8_158
# BB#173:                               # %if.end565
                                        #   in Loop: Header=BB8_1 Depth=1
	jbe	.LBB8_191
# BB#174:                               # %if.then574
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%r10, %rdi
	movq	%r12, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%r10, %rbx
	callq	can_extend_p
	movq	%rax, %r13
                                        # kill: RAX<def> R13<kill>
	andq	%r15, %rax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB8_184
# BB#175:                               # %for.cond585.preheader
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	%rbx, %r13
	movq	%r12, %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	movq	%rax, %rbx
                                        # kill: R12<def> RAX<kill>
	je	.LBB8_182
	.align	16, 0x90
.LBB8_176:                              # %for.body588
                                        #   Parent Loop BB8_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	can_extend_p
	andq	%r15, %rax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB8_178
# BB#177:                               # %lor.lhs.false592
                                        #   in Loop: Header=BB8_176 Depth=2
	movzbl	mode_size(%r13), %edx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %ecx
	cmpq	%rcx, %rdx
	jae	.LBB8_180
.LBB8_178:                              # %land.lhs.true601
                                        #   in Loop: Header=BB8_176 Depth=2
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	can_extend_p
	andq	%r15, %rax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB8_190
# BB#179:                               # %land.lhs.true601.for.inc608_crit_edge
                                        #   in Loop: Header=BB8_176 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, %rax
.LBB8_180:                              # %for.inc608
                                        #   in Loop: Header=BB8_176 Depth=2
	movzbl	mode_wider_mode(%rax), %ebx
	testq	%rbx, %rbx
	jne	.LBB8_176
	jmp	.LBB8_181
.LBB8_128:                              #   in Loop: Header=BB8_1 Depth=1
	movq	%r13, %rbx
	jmp	.LBB8_133
.LBB8_191:                              # %if.end631
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$5, %r12
	jne	.LBB8_196
# BB#192:                               # %if.end631
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$4, %r10
	je	.LBB8_193
.LBB8_196:                              # %if.end639
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$5, %r12
	jne	.LBB8_198
# BB#197:                               # %if.end639
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$3, %r10
	je	.LBB8_193
.LBB8_198:                              # %if.end647
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$5, %r12
	jne	.LBB8_200
# BB#199:                               # %if.end647
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$2, %r10
	jne	.LBB8_200
.LBB8_193:                              # %if.then637
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	$5, %rdi
.LBB8_194:                              # %tailrecurse
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	%rax, %rsi
	callq	force_reg
	movq	32(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB8_1
.LBB8_190:                              # %if.then605
                                        #   in Loop: Header=BB8_1 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	convert_modes
	movq	32(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB8_1
.LBB8_132:                              # %land.lhs.true425.lor.lhs.false431_crit_edge
                                        #   in Loop: Header=BB8_1 Depth=1
	movl	(%r14), %eax
	movq	%r14, %rsi
	.align	16, 0x90
.LBB8_133:                              # %lor.lhs.false431
                                        #   in Loop: Header=BB8_1 Depth=1
	orq	$2, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	je	.LBB8_135
# BB#134:                               # %if.then441
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	%rbx, %rdi
	callq	force_reg
	movq	%rax, %rsi
	jmp	.LBB8_135
.LBB8_200:                              # %if.end655
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$4, %r12
	jne	.LBB8_203
# BB#201:                               # %if.end655
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$3, %r10
	je	.LBB8_202
.LBB8_203:                              # %if.end663
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$4, %r12
	jne	.LBB8_205
# BB#204:                               # %if.end663
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$2, %r10
	jne	.LBB8_205
.LBB8_202:                              # %if.then661
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	$4, %rdi
	jmp	.LBB8_194
.LBB8_205:                              # %if.end671
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$3, %r12
	jne	.LBB8_208
# BB#206:                               # %if.end671
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$2, %r10
	jne	.LBB8_208
# BB#207:                               # %if.then677
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	$3, %rdi
	jmp	.LBB8_194
.LBB8_208:                              # %if.end679
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$6, %r12
	jne	.LBB8_211
# BB#209:                               # %if.end679
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$5, %r10
	je	.LBB8_210
.LBB8_211:                              # %if.end687
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$6, %r12
	jne	.LBB8_213
# BB#212:                               # %if.end687
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$4, %r10
	je	.LBB8_210
.LBB8_213:                              # %if.end695
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$6, %r12
	jne	.LBB8_215
# BB#214:                               # %if.end695
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$3, %r10
	je	.LBB8_210
.LBB8_215:                              # %if.end703
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$6, %r12
	jne	.LBB8_217
# BB#216:                               # %if.end703
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$2, %r10
	jne	.LBB8_217
.LBB8_210:                              # %if.then685
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	$6, %rdi
	jmp	.LBB8_194
.LBB8_28:                               # %if.then96
	movq	%r8, %rdi
	movq	%rax, %rsi
	jmp	.LBB8_32
.LBB8_181:
	movq	%r12, %rcx
.LBB8_182:                              # %for.end612
	movq	16(%rsp), %rax          # 8-byte Reload
	subq	%rax, %r14
	xorq	%rsi, %rsi
	movq	%rcx, %rbx
	movq	%r14, %rdi
	callq	build_int_2_wide
	movq	%rax, %r14
	movq	%rbx, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	force_reg
	movq	%r13, %r15
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	gen_lowpart
	movq	$82, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %r8
	movq	40(%rsp), %r12          # 8-byte Reload
	movq	%r12, %r9
	callq	expand_shift
	movq	$83, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	movq	%rbx, %r8
	movq	%r12, %r9
	callq	expand_shift
	movq	%rbx, %rdi
	cmpq	%rdi, %rax
	je	.LBB8_218
# BB#183:                               # %if.then628
	movq	%rax, %rsi
	jmp	.LBB8_32
.LBB8_24:                               # %if.then118
	movzbl	mode_bitsize(%rdi,%rdi), %ecx
	movzbl	mode_bitsize(%r10,%r10), %esi
	cmpq	%rsi, %rcx
	jne	.LBB8_25
# BB#29:                                # %if.end128
	cmpq	$1, %rbx
	ja	.LBB8_31
# BB#30:                                # %if.then138
	movzbl	2(%rax), %edx
	xorq	%rcx, %rcx
	movq	%r10, %rdi
	movq	%rax, %rsi
	movq	%r8, %rbx
	callq	simplify_gen_subreg
	movq	%rbx, %rdi
	movq	%rax, %rsi
	jmp	.LBB8_32
.LBB8_218:                              # %return
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB8_31:                               # %if.else
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	xorq	%rcx, %rcx
	movq	%r8, %rsi
	movq	%rax, %rbx
	callq	simplify_gen_subreg
	movq	%rbx, %rsi
	movq	%rax, %rdi
	jmp	.LBB8_32
.LBB8_34:                               # %if.then155
	andq	$65535, %rbx            # imm = 0xFFFF
	cmpq	%r14, %rbx
	jae	.LBB8_37
# BB#35:                                # %if.then164
	xorq	%rdx, %rdx
	movq	%r10, %rdi
	movq	%r13, %rsi
	movq	%r8, %rbx
	movq	%r10, %r14
	callq	can_extend_p
	movq	%r14, %r10
	movq	%rbx, %r8
	movq	%rax, %rcx
	andq	%r15, %rcx
	cmpq	$1317, %rcx             # imm = 0x525
	jne	.LBB8_36
.LBB8_37:                               # %if.end170
	movl	target_flags(%rip), %ecx
	movq	%rcx, %rax
	andq	$1, %rax
	je	.LBB8_38
# BB#45:                                # %land.lhs.true176
	cmpq	$16, %r13
	movq	%r12, %rdx
	movq	%r13, %rdi
	je	.LBB8_46
	jmp	.LBB8_40
.LBB8_93:                               # %if.then276
	movq	40(%rsp), %rcx          # 8-byte Reload
	testq	%r15, %rcx
	je	.LBB8_94
# BB#95:                                # %if.then276
	movabsq	$1, %rsi
	jmp	.LBB8_96
.LBB8_158:                              # %if.then511
	movl	(%rax), %edx
	movq	%rdx, %rcx
	andq	$134283263, %rcx        # imm = 0x800FFFF
	cmpq	$66, %rcx
	jne	.LBB8_163
# BB#159:                               # %land.lhs.true521
	movzbl	direct_load(%r10), %ecx
	testq	%rcx, %rcx
	je	.LBB8_163
# BB#160:                               # %land.lhs.true526
	movq	%r10, %rbx
	movq	8(%rax), %rdi
	movq	%rax, %r14
	callq	mode_dependent_address_p
	testq	%r15, %rax
	je	.LBB8_161
# BB#162:                               # %land.lhs.true526.lor.lhs.false532_crit_edge
	movl	(%r14), %edx
	movq	%r14, %rax
	movq	%rbx, %r10
.LBB8_163:                              # %lor.lhs.false532
	orq	$2, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$63, %rdx
	je	.LBB8_166
# BB#164:                               # %if.then542
	movq	%r12, %rdi
	movq	%rax, %rsi
	movq	%r10, %rbx
	callq	force_reg
	jmp	.LBB8_165
.LBB8_38:                               # %lor.lhs.false172
	testq	$262144, %rcx           # imm = 0x40000
	movq	%r12, %rdx
	movq	%r13, %rdi
	je	.LBB8_40
# BB#39:                                # %lor.lhs.false172
	cmpq	$16, %rdi
	jne	.LBB8_40
.LBB8_46:                               # %land.lhs.true176
	cmpq	$15, %r10
	jne	.LBB8_40
# BB#47:                                # %if.then182
	movq	$909, %rdi              # imm = 0x38D
	jmp	.LBB8_48
.LBB8_40:                               # %if.end183
	andq	$33554432, %rcx         # imm = 0x2000000
	jne	.LBB8_50
# BB#41:                                # %land.lhs.true186
	testq	%r15, %rax
	je	.LBB8_50
# BB#42:                                # %land.lhs.true186
	cmpq	$17, %rdi
	jne	.LBB8_50
# BB#43:                                # %land.lhs.true186
	cmpq	$15, %r10
	jne	.LBB8_50
# BB#44:                                # %if.then195
	movq	$913, %rdi              # imm = 0x391
	jmp	.LBB8_48
.LBB8_50:                               # %if.end196
	testq	%r15, %rax
	movabsq	$0, %rsi
	movabsq	$1, %r9
	movq	%rsi, %rbx
	je	.LBB8_52
# BB#51:                                # %if.end196
	movq	%r9, %rbx
.LBB8_52:                               # %if.end196
	cmpq	$18, %rdi
	jne	.LBB8_54
# BB#53:                                # %if.end196
	movq	%r9, %rsi
.LBB8_54:                               # %if.end196
	andq	%rsi, %rbx
	je	.LBB8_57
# BB#55:                                # %if.end196
	cmpq	$15, %r10
	jne	.LBB8_57
# BB#56:                                # %if.then205
	movq	$916, %rdi              # imm = 0x394
	jmp	.LBB8_48
.LBB8_36:                               # %if.then168
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	%r8, %rsi
	movq	%r12, %rdx
	jmp	.LBB8_49
.LBB8_94:
	movabsq	$0, %rsi
.LBB8_96:                               # %if.then276
	movzbl	mode_size(%r10), %edx
	movabsq	$4286578688, %rcx       # imm = 0xFF800000
	andq	%rcx, %rax
	movq	$23, %rcx
	shrq	%cl, %rax
	leaq	4(%rax), %rcx
	orq	$3, %rax
	addq	%rdx, %rax
	cqto
	idivq	%rcx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	orq	$120, %rsi
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%r10, %rdi
	movq	%r10, 24(%rsp)          # 8-byte Spill
	movq	%r13, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	can_extend_p
	movq	%rax, %r14
                                        # kill: RAX<def> R14<kill>
	andq	%r15, %rax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB8_97
# BB#104:                               # %if.else301
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rcx, %rax
	cmpq	%rax, %rbx
	jae	.LBB8_109
# BB#105:                               # %land.lhs.true310
	movl	word_mode(%rip), %esi
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	can_extend_p
	movq	%rax, %r14
                                        # kill: RAX<def> R14<kill>
	andq	%r15, %rax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB8_106
.LBB8_109:                              # %if.end326
	callq	start_sequence
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %r14
	movq	%r14, %rsi
	callq	reg_overlap_mentioned_p
	testq	%r15, %rax
	je	.LBB8_110
# BB#111:                               # %if.then329
	movq	%r13, %r12
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	force_reg
	movq	%rax, %rdx
	jmp	.LBB8_112
.LBB8_97:                               # %if.then290
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	jle	.LBB8_98
# BB#99:                                # %land.lhs.true293
	movq	%r12, %rdx
	movzbl	(%rdx), %eax
	cmpq	$63, %rax
	jne	.LBB8_101
# BB#100:                               # %if.then298
	movq	%r13, %rdi
	movq	%rdx, %rsi
	callq	force_reg
	movq	%rax, %rdx
.LBB8_101:                              # %if.end300
	movq	32(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB8_102
.LBB8_184:                              # %if.then578
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%r15, %rax
	movq	24(%rsp), %rdx          # 8-byte Reload
	je	.LBB8_185
# BB#186:                               # %if.then578
	movabsq	$1, %r14
	jmp	.LBB8_187
.LBB8_57:                               # %if.end206
	testq	%r15, %rcx
	jne	.LBB8_62
# BB#58:                                # %land.lhs.true209
	testq	%r15, %rax
	je	.LBB8_62
# BB#59:                                # %land.lhs.true209
	cmpq	$17, %rdi
	jne	.LBB8_62
# BB#60:                                # %land.lhs.true209
	cmpq	$16, %r10
	jne	.LBB8_62
# BB#61:                                # %if.then218
	movq	$919, %rdi              # imm = 0x397
	jmp	.LBB8_48
.LBB8_62:                               # %if.end219
	movq	%rdi, %r13
	cmpq	$16, %r10
	jne	.LBB8_63
# BB#64:                                # %if.end219
	movabsq	$1, %rax
	jmp	.LBB8_65
.LBB8_185:
	movabsq	$0, %r14
.LBB8_187:                              # %if.then578
	movl	flag_force_mem(%rip), %eax
	orq	$120, %r14
	testq	%rax, %rax
	je	.LBB8_189
# BB#188:                               # %if.then580
	movq	%rdx, %rdi
	callq	force_not_mem
	movq	%rax, %rdx
.LBB8_189:                              # %if.end582
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%r13, %rdi
	movq	%r14, %rcx
	jmp	.LBB8_49
.LBB8_98:
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%r12, %rdx
.LBB8_102:                              # %if.end300
	movq	%r14, %rdi
	movq	16(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB8_49
.LBB8_110:
	movq	%r14, %rdx
	movq	%r13, %r12
.LBB8_112:                              # %if.end331
	movq	24(%rsp), %r13          # 8-byte Reload
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rcx, %rax
	cmpq	%rax, %rbx
	jae	.LBB8_114
# BB#113:
	movl	word_mode(%rip), %r12d
.LBB8_114:                              # %if.end331
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rcx
	callq	convert_modes
	movq	%rax, %r14
	movq	%r12, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	gen_lowpart
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	emit_move_insn
	testq	%r15, %rbx
	je	.LBB8_116
# BB#115:                               # %if.then347
	movq	const_int_rtx+512(%rip), %r14
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	jmp	.LBB8_117
.LBB8_116:                              # %if.else348
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movzbl	mode_bitsize(%rbx,%rbx), %edi
	decq	%rdi
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	$83, %rdi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	expand_shift
	movl	word_mode(%rip), %edi
	xorq	%rsi, %rsi
	movq	$1, %rcx
	movq	%rax, %rdx
	callq	convert_modes
	movq	%rax, %r14
	movq	%rbx, %r12
.LBB8_117:                              # %if.end368
	movq	8(%rsp), %r15           # 8-byte Reload
	movzbl	mode_size(%r12), %eax
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	xorq	%rdx, %rdx
	idivq	%rsi
	movq	%rax, %rbx
	jmp	.LBB8_118
.LBB8_123:                              # %for.inc
                                        #   in Loop: Header=BB8_118 Depth=1
	incq	%rbx
.LBB8_118:                              # %if.end368
                                        # =>This Inner Loop Header: Depth=1
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
	cmpq	%rdx, %rax
	jge	.LBB8_124
# BB#119:                               # %for.body
                                        #   in Loop: Header=BB8_118 Depth=1
	movq	$1, %rdx
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r13, %rcx
	callq	operand_subword
	testq	%rax, %rax
	je	.LBB8_120
# BB#121:                               # %if.end382
                                        #   in Loop: Header=BB8_118 Depth=1
	cmpq	%rax, %r14
	je	.LBB8_123
# BB#122:                               # %if.then385
                                        #   in Loop: Header=BB8_118 Depth=1
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	emit_move_insn
	jmp	.LBB8_123
.LBB8_124:                              # %for.end
	callq	get_insns
	movq	%rax, %r14
	callq	end_sequence
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%rax, %r8
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_no_conflict_block  # TAILCALL
.LBB8_106:                              # %if.then314
	movq	32(%rsp), %rcx          # 8-byte Reload
	movzbl	(%rcx), %eax
	cmpq	$61, %rax
	jne	.LBB8_108
# BB#107:                               # %if.then319
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%rcx, %rbx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rbx, %rcx
.LBB8_108:                              # %if.end322
	movl	word_mode(%rip), %edi
	movq	%rcx, %rsi
	movq	%rcx, %rbx
	callq	gen_lowpart
	movq	%rax, %rdi
	movq	%r12, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	convert_move
	movl	word_mode(%rip), %edi
	movq	%rbx, %rsi
	callq	gen_lowpart
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	movq	16(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB8_49
.LBB8_63:
	movabsq	$0, %rax
.LBB8_65:                               # %if.end219
	andq	%rax, %rbx
	cmpq	$1, %rbx
	jne	.LBB8_67
# BB#66:                                # %if.then228
	movq	$922, %rdi              # imm = 0x39A
.LBB8_48:                               # %if.then182
	xorq	%rcx, %rcx
	movq	%r8, %rsi
.LBB8_49:                               # %if.then182
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_unop_insn  # TAILCALL
.LBB8_67:                               # %if.end229
	movq	%rdx, %r14
	movq	%r8, %r12
	leaq	-15(%r13), %rax
	andq	%r15, %rax
	cmpq	$3, %rax
	ja	.LBB8_219
# BB#68:                                # %if.end229
	jmpq	*.LJTI8_0(,%rax,8)
.LBB8_69:                               # %sw.bb
	cmpq	$16, %r10
	je	.LBB8_70
# BB#71:                                # %sw.bb
	cmpq	$18, %r10
	jne	.LBB8_72
# BB#74:                                # %sw.bb232
	movq	%r10, %r15
	movabsq	$libfunc_table+16, %rax
	jmp	.LBB8_89
.LBB8_161:
	movq	%r14, %rax
.LBB8_165:                              # %if.end544
	movq	%rbx, %r10
.LBB8_166:                              # %if.end544
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB8_172
# BB#167:                               # %land.lhs.true549
	movl	8(%rax), %edi
	cmpq	$52, %rdi
	ja	.LBB8_172
# BB#168:                               # %land.lhs.true554
	movq	%rax, %rbx
	movq	%r10, %rsi
	movq	%r10, %r14
	callq	ix86_hard_regno_mode_ok
	testq	%r15, %rax
	je	.LBB8_170
# BB#169:
	movq	%rbx, %rax
	jmp	.LBB8_171
.LBB8_170:                              # %if.then560
	movq	%rbx, %rdi
	callq	copy_to_reg
.LBB8_171:                              # %if.end562
	movq	%r14, %r10
.LBB8_172:                              # %if.end562
	movq	%r10, %rdi
	movq	%rax, %rsi
	callq	gen_lowpart
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
.LBB8_32:                               # %if.end147
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_move_insn  # TAILCALL
.LBB8_75:                               # %sw.bb233
	cmpq	$15, %r10
	je	.LBB8_76
# BB#77:                                # %sw.bb233
	cmpq	$18, %r10
	jne	.LBB8_78
# BB#80:                                # %sw.bb236
	movq	%r10, %r15
	movabsq	$libfunc_table+32, %rax
	jmp	.LBB8_89
.LBB8_81:                               # %sw.bb239
	cmpq	$15, %r10
	jne	.LBB8_83
# BB#82:
	movq	%r10, %r15
	movabsq	$libfunc_table+48, %rax
	jmp	.LBB8_89
.LBB8_85:                               # %sw.bb244
	cmpq	$15, %r10
	jne	.LBB8_87
# BB#86:
	movq	%r10, %r15
	movabsq	$libfunc_table+56, %rax
	jmp	.LBB8_89
.LBB8_83:                               # %sw.bb239
	cmpq	$16, %r10
	movq	%r10, %r15
	jne	.LBB8_219
# BB#84:                                # %sw.bb241
	movabsq	$libfunc_table+64, %rax
	jmp	.LBB8_89
.LBB8_87:                               # %sw.bb244
	cmpq	$16, %r10
	movq	%r10, %r15
	jne	.LBB8_219
# BB#88:                                # %sw.bb246
	movabsq	$libfunc_table+72, %rax
	jmp	.LBB8_89
.LBB8_70:
	movq	%r10, %r15
	movabsq	$libfunc_table, %rax
	jmp	.LBB8_89
.LBB8_72:                               # %sw.bb
	cmpq	$17, %r10
	movq	%r10, %r15
	jne	.LBB8_219
# BB#73:                                # %sw.bb231
	movabsq	$libfunc_table+8, %rax
	jmp	.LBB8_89
.LBB8_76:
	movq	%r10, %r15
	movabsq	$libfunc_table+40, %rax
	jmp	.LBB8_89
.LBB8_78:                               # %sw.bb233
	cmpq	$17, %r10
	movq	%r10, %r15
	jne	.LBB8_219
# BB#79:                                # %sw.bb235
	movabsq	$libfunc_table+24, %rax
.LBB8_89:                               # %sw.epilog250
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB8_219
# BB#90:                                # %if.end254
	callq	start_sequence
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	$1, %r8
	xorq	%rax, %rax
	movq	%r13, (%rsp)
	movq	%rbx, %rdi
	movq	%r15, %rbx
	movq	%rbx, %rcx
	movq	%r14, %r13
	movq	%r13, %r9
	callq	emit_library_call_value
	movq	%rax, %r14
	callq	get_insns
	movq	%rax, %r15
	callq	end_sequence
	movq	$124, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_e
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_libcall_block  # TAILCALL
.LBB8_219:                              # %if.then253
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.convert_move, %rdx
	movq	$781, %rsi              # imm = 0x30D
	callq	fancy_abort
.LBB8_6:                                # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.convert_move, %rcx
	movq	$502, %rsi              # imm = 0x1F6
	jmp	.LBB8_7
.LBB8_16:                               # %if.then50
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.convert_move, %rcx
	movq	$515, %rsi              # imm = 0x203
	jmp	.LBB8_7
.LBB8_137:                              # %if.then448
	cmpq	$2, %r13
	je	.LBB8_139
# BB#138:                               # %if.then451
	movq	$2, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	convert_modes
.LBB8_139:                              # %if.end453
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.convert_move, %rcx
	movq	$928, %rsi              # imm = 0x3A0
	jmp	.LBB8_7
.LBB8_220:                              # %if.else462
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.convert_move, %rcx
	movq	$947, %rsi              # imm = 0x3B3
	jmp	.LBB8_7
.LBB8_144:                              # %if.then467
	cmpq	$4, %r12
	je	.LBB8_146
# BB#145:                               # %if.then470
	movq	$4, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	convert_modes
.LBB8_146:                              # %if.end472
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.convert_move, %rcx
	movq	$963, %rsi              # imm = 0x3C3
	jmp	.LBB8_7
.LBB8_221:                              # %if.else481
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.convert_move, %rcx
	movq	$989, %rsi              # imm = 0x3DD
	jmp	.LBB8_7
.LBB8_151:                              # %if.then486
	cmpq	$5, %r12
	je	.LBB8_153
# BB#152:                               # %if.then489
	movq	$5, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	convert_modes
.LBB8_153:                              # %if.end491
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.convert_move, %rcx
	movq	$1005, %rsi             # imm = 0x3ED
	jmp	.LBB8_7
.LBB8_222:                              # %if.else500
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.convert_move, %rcx
	movq	$1024, %rsi             # imm = 0x400
	jmp	.LBB8_7
.LBB8_217:                              # %if.end725
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.convert_move, %rcx
	movq	$1243, %rsi             # imm = 0x4DB
	jmp	.LBB8_7
.LBB8_25:                               # %if.then127
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.convert_move, %rcx
	movq	$527, %rsi              # imm = 0x20F
	jmp	.LBB8_7
.LBB8_120:                              # %if.then381
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.convert_move, %rcx
	movq	$886, %rsi              # imm = 0x376
.LBB8_7:                                # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp84:
	.size	convert_move, .Ltmp84-convert_move
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI8_0:
	.quad	.LBB8_69
	.quad	.LBB8_75
	.quad	.LBB8_81
	.quad	.LBB8_85

	.text
	.globl	emit_move_insn
	.align	16, 0x90
	.type	emit_move_insn,@function
emit_move_insn:                         # @emit_move_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp90:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp91:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp92:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp93:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp94:
	.cfi_def_cfa_offset 48
.Ltmp95:
	.cfi_offset %rbx, -40
.Ltmp96:
	.cfi_offset %r12, -32
.Ltmp97:
	.cfi_offset %r14, -24
.Ltmp98:
	.cfi_offset %r15, -16
	movzbl	2(%rdi), %ebx
	movq	%rsi, %r15
	movq	$1, %rsi
	callq	protect_from_queue
	movq	%rax, %r14
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	protect_from_queue
	movq	%rax, %r12
	cmpq	$51, %rbx
	je	.LBB9_17
# BB#1:                                 # %lor.lhs.false
	movzbl	2(%r12), %eax
	cmpq	%rbx, %rax
	je	.LBB9_3
# BB#2:                                 # %lor.lhs.false
	testq	%rax, %rax
	jne	.LBB9_17
.LBB9_3:                                # %if.end
	movl	(%r14), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB9_10
# BB#4:                                 # %land.lhs.true53
	movq	8(%r14), %rsi
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	callq	memory_address_p
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	jne	.LBB9_6
# BB#5:                                 # %land.lhs.true58
	movzbl	2(%r14), %esi
	movq	%r14, %rdi
	callq	push_operand
	testq	%r15, %rax
	je	.LBB9_9
.LBB9_6:                                # %lor.lhs.false64
	movl	flag_force_addr(%rip), %eax
	testq	%rax, %rax
	je	.LBB9_10
# BB#7:                                 # %land.lhs.true66
	movq	8(%r14), %rax
	movzbl	(%rax), %ecx
	addq	$-54, %rcx
	andq	%r15, %rcx
	cmpq	$14, %rcx
	ja	.LBB9_10
# BB#8:                                 # %land.lhs.true66
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24595, %rax            # imm = 0x6013
	je	.LBB9_10
.LBB9_9:                                # %if.then101
	movq	%r14, %rdi
	callq	validize_mem
	movq	%rax, %r14
.LBB9_10:                               # %if.end103
	movl	(%r12), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB9_16
# BB#11:                                # %land.lhs.true107
	movq	8(%r12), %rsi
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	callq	memory_address_p
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB9_15
# BB#12:                                # %lor.lhs.false116
	movl	flag_force_addr(%rip), %eax
	testq	%rax, %rax
	je	.LBB9_16
# BB#13:                                # %land.lhs.true118
	movq	8(%r12), %rax
	movzbl	(%rax), %ecx
	addq	$-54, %rcx
	andq	%rdx, %rcx
	cmpq	$14, %rcx
	ja	.LBB9_16
# BB#14:                                # %land.lhs.true118
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24595, %rax            # imm = 0x6013
	je	.LBB9_16
.LBB9_15:                               # %if.then153
	movq	%r12, %rdi
	callq	validize_mem
	movq	%rax, %r12
.LBB9_16:                               # %if.end158
	movq	%r14, %rdi
	movq	%r12, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	emit_move_insn_1  # TAILCALL
.LBB9_17:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.emit_move_insn, %rcx
	movq	$2769, %rsi             # imm = 0xAD1
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp99:
	.size	emit_move_insn, .Ltmp99-emit_move_insn
	.cfi_endproc

	.globl	convert_to_mode
	.align	16, 0x90
	.type	convert_to_mode,@function
convert_to_mode:                        # @convert_to_mode
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, %rax
	xorq	%rsi, %rsi
	movq	%rdx, %rcx
	movq	%rax, %rdx
	jmp	convert_modes  # TAILCALL
.Ltmp100:
	.size	convert_to_mode, .Ltmp100-convert_to_mode
	.cfi_endproc

	.globl	convert_modes
	.align	16, 0x90
	.type	convert_modes,@function
convert_modes:                          # @convert_modes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp107:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp108:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp109:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp110:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp111:
	.cfi_def_cfa_offset 48
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
	movq	%rdx, %r13
	movl	(%r13), %edx
	movq	%rcx, %r15
	movq	%rsi, %r12
	movq	%rdi, %r14
	movq	%rdx, %rax
	andq	$268500991, %rax        # imm = 0x1000FFFF
	cmpq	$268435519, %rax        # imm = 0x1000003F
	jne	.LBB11_4
# BB#1:                                 # %land.lhs.true3
	movq	8(%r13), %rax
	movzbl	2(%rax), %eax
	movzbl	mode_size(%rax), %eax
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	mode_size(%rsi), %ecx
	cmpq	%rcx, %rax
	jb	.LBB11_4
# BB#2:                                 # %land.lhs.true13
	movq	$26, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	andq	$1, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%r15, %rcx
	cmpq	%rcx, %rax
	jne	.LBB11_4
# BB#3:                                 # %if.then
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	gen_lowpart
	movq	%rax, %r13
	movl	(%r13), %edx
.LBB11_4:                               # %if.end
	movq	$16, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	andq	$255, %rax
	je	.LBB11_6
# BB#5:                                 # %if.end
	movq	%rax, %r12
.LBB11_6:                               # %if.end
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r12, %rcx
	andq	%rax, %rcx
	movq	%r14, %rsi
	andq	%rax, %rsi
	cmpq	%rsi, %rcx
	je	.LBB11_34
# BB#7:                                 # %if.end32
	testq	%rax, %r15
	je	.LBB11_16
# BB#8:                                 # %land.lhs.true34
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	mode_class(,%rsi,4), %ecx
	cmpq	$1, %rcx
	jne	.LBB11_16
# BB#9:                                 # %land.lhs.true39
	movzbl	mode_bitsize(%rsi,%rsi), %ecx
	cmpq	$128, %rcx
	jne	.LBB11_16
# BB#10:                                # %land.lhs.true39
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	jne	.LBB11_16
# BB#11:                                # %land.lhs.true50
	movq	8(%r13), %rdi
	testq	%rdi, %rdi
	js	.LBB11_12
.LBB11_16:                              # %if.end73
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	movq	$32, %rcx
	movq	%r14, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$54, %rsi
	jne	.LBB11_18
# BB#17:                                # %land.lhs.true78
	movzbl	mode_bitsize(%rdi,%rdi), %ecx
	cmpq	$65, %rcx
	jb	.LBB11_25
.LBB11_18:                              # %lor.lhs.false
	movl	mode_class(,%rdi,4), %ecx
	cmpq	$1, %rcx
	jne	.LBB11_33
# BB#19:                                # %land.lhs.true88
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movl	mode_class(,%rbx,4), %ecx
	cmpq	$1, %rcx
	jne	.LBB11_33
# BB#20:                                # %land.lhs.true93
	cmpq	$55, %rsi
	je	.LBB11_25
# BB#21:                                # %lor.lhs.false98
	movzbl	mode_size(%rdi), %ecx
	movzbl	mode_size(%rbx), %ebx
	cmpq	%rbx, %rcx
	ja	.LBB11_33
# BB#22:                                # %land.lhs.true107
	andq	$134283263, %rdx        # imm = 0x800FFFF
	cmpq	$66, %rdx
	jne	.LBB11_24
# BB#23:                                # %land.lhs.true117
	movzbl	direct_load(%rdi), %ecx
	testq	%rcx, %rcx
	jne	.LBB11_25
.LBB11_24:                              # %lor.lhs.false122
	cmpq	$61, %rsi
	jne	.LBB11_33
.LBB11_25:                              # %if.then127
	cmpq	$54, %rsi
	jne	.LBB11_32
# BB#26:                                # %if.then127
	testq	%rax, %r12
	je	.LBB11_32
# BB#27:                                # %land.lhs.true135
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %edx
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movzbl	mode_size(%r12), %ecx
	cmpq	%rcx, %rdx
	jbe	.LBB11_32
# BB#28:                                # %if.then144
	movzbl	mode_bitsize(%r12,%r12), %edx
	movq	8(%r13), %rbx
	movq	$1, %rsi
	movq	$1, %rdi
	movq	%rdx, %rcx
	shlq	%cl, %rdi
	decq	%rdi
	andq	%rbx, %rdi
	testq	%rax, %r15
	jne	.LBB11_31
# BB#29:                                # %land.lhs.true158
	addq	%rdx, %rax
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rsi, %rdi
	je	.LBB11_31
# BB#30:                                # %if.then164
	movq	$-1, %rax
	movq	%rdx, %rcx
	shlq	%cl, %rax
	orq	%rax, %rdi
.LBB11_31:                              # %if.end167
	movq	%r14, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_CONST_INT  # TAILCALL
.LBB11_33:                              # %if.end172
	movq	%r14, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	convert_move
	movq	%rbx, %r13
.LBB11_34:                              # %return
	movq	%r13, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB11_32:                              # %if.end170
	movq	%r14, %rdi
	movq	%r13, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_lowpart  # TAILCALL
.LBB11_12:                              # %if.then55
	testq	%rax, %r12
	je	.LBB11_15
# BB#13:                                # %land.lhs.true61
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movzbl	mode_bitsize(%r12,%r12), %ecx
	cmpq	$63, %rcx
	ja	.LBB11_15
# BB#14:                                # %if.then67
	movq	$1, %rax
	shlq	%cl, %rax
	decq	%rax
	andq	%rax, %rdi
.LBB11_15:                              # %if.end71
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	immed_double_const  # TAILCALL
.Ltmp117:
	.size	convert_modes, .Ltmp117-convert_modes
	.cfi_endproc

	.globl	move_by_pieces
	.align	16, 0x90
	.type	move_by_pieces,@function
move_by_pieces:                         # @move_by_pieces
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
	subq	$112, %rsp
.Ltmp129:
	.cfi_def_cfa_offset 160
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
	movq	%rdx, %r13
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movl	target_flags(%rip), %r15d
	movq	$23, %rcx
	shrq	%cl, %r15
	xorq	%r12, %r12
	testq	%rdi, %rdi
	movq	8(%rsi), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movabsq	$0, %rdx
	movabsq	$0, %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	je	.LBB12_6
# BB#1:                                 # %if.then
	movq	8(%rdi), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movl	(%rax), %ecx
	movq	%rcx, %rax
	andq	$65532, %rax            # imm = 0xFFFC
	andq	$65533, %rcx            # imm = 0xFFFD
	cmpq	$96, %rcx
	movabsq	$0, %rdx
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movabsq	$1, %rcx
	je	.LBB12_3
# BB#2:                                 # %if.then
	movq	%rcx, %rdx
.LBB12_3:                               # %if.then
	cmpq	$96, %rax
	je	.LBB12_5
# BB#4:                                 # %if.then
	movq	%rcx, 72(%rsp)          # 8-byte Spill
.LBB12_5:                               # %if.then
	movq	%rdi, 64(%rsp)          # 8-byte Spill
.LBB12_6:                               # %if.end
	movq	56(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %eax
	andq	$4, %r15
	andq	$65532, %rax            # imm = 0xFFFC
	movq	%rax, 96(%rsp)          # 8-byte Spill
	testq	%rdx, %rdx
	movabsq	$0, %r14
	jne	.LBB12_8
# BB#7:                                 # %if.end
	movq	%r13, %r14
.LBB12_8:                               # %if.end
	addq	$5, %r15
	cmpq	$96, 96(%rsp)           # 8-byte Folded Reload
	je	.LBB12_9
# BB#10:                                # %if.end
	movabsq	$1, %rax
	jmp	.LBB12_11
.LBB12_9:
	movabsq	$0, %rax
.LBB12_11:                              # %if.end
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	jne	.LBB12_13
# BB#12:                                # %if.end
	testq	%rax, %rax
	je	.LBB12_50
.LBB12_13:                              # %land.lhs.true58
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movl	class_narrowest_mode+4(%rip), %r13d
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	testq	%r13, %r13
	je	.LBB12_14
# BB#15:
	movq	$17, %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 104(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB12_16:                              # %for.body.lr.ph.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB12_17 Depth 2
	movq	%r13, %rbx
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB12_17:                              # %for.body.i
                                        #   Parent Loop BB12_16 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rax, %rcx
	movq	%rbx, %rdi
	jb	.LBB12_19
# BB#18:                                # %for.body.i
                                        #   in Loop: Header=BB12_17 Depth=2
	movq	%rsi, %rdi
.LBB12_19:                              # %for.body.i
                                        #   in Loop: Header=BB12_17 Depth=2
	movzbl	mode_wider_mode(%rdx), %ebx
	testq	%rbx, %rbx
	movq	%rdi, %rsi
	jne	.LBB12_17
# BB#20:                                # %for.end.i
                                        #   in Loop: Header=BB12_16 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdi
	je	.LBB12_25
# BB#21:                                # %if.end10.i
                                        #   in Loop: Header=BB12_16 Depth=1
	movq	$32, %rcx
	movq	%rdi, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	movq	optab_table+240(%rip), %rax
	sarq	%cl, %r12
	movq	$4, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB12_24
# BB#22:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB12_16 Depth=1
	callq	get_mode_alignment
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$128, %rax
	ja	.LBB12_24
# BB#23:                                # %if.then17.i
                                        #   in Loop: Header=BB12_16 Depth=1
	movzbl	mode_size(%r12), %ecx
	xorq	%rdx, %rdx
	movq	104(%rsp), %rax         # 8-byte Reload
	divq	%rcx
	addq	%rax, 88(%rsp)          # 8-byte Folded Spill
	movq	%rdx, 104(%rsp)         # 8-byte Spill
.LBB12_24:                              # %if.end24.i
                                        #   in Loop: Header=BB12_16 Depth=1
	movzbl	mode_size(%r12), %eax
	cmpq	$1, %rax
	ja	.LBB12_16
	jmp	.LBB12_25
.LBB12_14:
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	%rax, 104(%rsp)         # 8-byte Spill
.LBB12_25:                              # %while.end.i
	cmpq	$0, 104(%rsp)           # 8-byte Folded Reload
	jne	.LBB12_26
# BB#28:                                # %move_by_pieces_ninsns.exit
	xorq	%r12, %r12
	cmpq	$3, 88(%rsp)            # 8-byte Folded Reload
	jb	.LBB12_29
# BB#30:                                # %if.then60
	xorq	%r12, %r12
	testq	%r13, %r13
	je	.LBB12_35
# BB#31:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	.align	16, 0x90
.LBB12_32:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rax, %rcx
	jb	.LBB12_34
# BB#33:                                # %for.body
                                        #   in Loop: Header=BB12_32 Depth=1
	movq	%r12, %r13
.LBB12_34:                              # %for.body
                                        #   in Loop: Header=BB12_32 Depth=1
	movq	%r13, %r12
	movzbl	mode_wider_mode(%rdx), %r13d
	testq	%r13, %r13
	jne	.LBB12_32
.LBB12_35:                              # %for.end
	cmpq	$96, 96(%rsp)           # 8-byte Folded Reload
	jne	.LBB12_37
# BB#36:
	movq	48(%rsp), %r13          # 8-byte Reload
	jmp	.LBB12_43
.LBB12_29:
	movq	48(%rsp), %r13          # 8-byte Reload
	jmp	.LBB12_50
.LBB12_37:                              # %land.lhs.true72
	movq	56(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$133, %rax
	jg	.LBB12_40
# BB#38:                                # %land.lhs.true72
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	movq	48(%rsp), %r13          # 8-byte Reload
	ja	.LBB12_43
# BB#39:                                # %land.lhs.true72
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB12_42
	jmp	.LBB12_43
.LBB12_40:                              # %land.lhs.true72
	cmpq	$134, %rax
	movq	48(%rsp), %r13          # 8-byte Reload
	je	.LBB12_42
# BB#41:                                # %land.lhs.true72
	cmpq	$140, %rax
	jne	.LBB12_43
.LBB12_42:                              # %if.then112
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	copy_addr_to_reg
	movq	%rax, 56(%rsp)          # 8-byte Spill
.LBB12_43:                              # %if.end115
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	je	.LBB12_50
# BB#44:                                # %land.lhs.true118
	movq	16(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$133, %rax
	jg	.LBB12_47
# BB#45:                                # %land.lhs.true118
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB12_50
# BB#46:                                # %land.lhs.true118
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB12_49
	jmp	.LBB12_50
.LBB12_47:                              # %land.lhs.true118
	cmpq	$134, %rax
	je	.LBB12_49
# BB#48:                                # %land.lhs.true118
	cmpq	$140, %rax
	jne	.LBB12_50
.LBB12_49:                              # %if.then158
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	copy_addr_to_reg
	movq	%rax, 16(%rsp)          # 8-byte Spill
.LBB12_50:                              # %while.cond.preheader
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%r15, %rax
	andq	%rbx, %rax
	cmpq	$1, %rax
	je	.LBB12_78
	.align	16, 0x90
.LBB12_51:                              # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB12_52 Depth 2
                                        #     Child Loop BB12_65 Depth 2
                                        #     Child Loop BB12_68 Depth 2
	movl	class_narrowest_mode+4(%rip), %eax
	testq	%rbx, %rax
	je	.LBB12_55
	.align	16, 0x90
.LBB12_52:                              # %for.body168
                                        #   Parent Loop BB12_51 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %ecx
	movq	%r15, %rsi
	andq	%rbx, %rsi
	cmpq	%rsi, %rcx
	jb	.LBB12_54
# BB#53:                                # %for.body168
                                        #   in Loop: Header=BB12_52 Depth=2
	movq	%r12, %rax
.LBB12_54:                              # %for.body168
                                        #   in Loop: Header=BB12_52 Depth=2
	movq	%rax, %r12
	movzbl	mode_wider_mode(%rdx), %eax
	testq	%rax, %rax
	jne	.LBB12_52
.LBB12_55:                              # %for.end180
                                        #   in Loop: Header=BB12_51 Depth=1
	testq	%rbx, %r12
	je	.LBB12_78
# BB#56:                                # %if.end184
                                        #   in Loop: Header=BB12_51 Depth=1
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movq	optab_table+240(%rip), %rax
	sarq	%cl, %rdx
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	$4, %rcx
	shlq	%cl, %rdx
	movslq	8(%rax,%rdx), %r15
	cmpq	$1317, %r15             # imm = 0x525
	je	.LBB12_77
# BB#57:                                # %land.lhs.true189
                                        #   in Loop: Header=BB12_51 Depth=1
	movq	%r12, %rdi
	callq	get_mode_alignment
	andq	%rbx, %rax
	cmpq	$128, %rax
	ja	.LBB12_77
# BB#58:                                # %if.then193
                                        #   in Loop: Header=BB12_51 Depth=1
	movq	48(%rsp), %rax          # 8-byte Reload
	movzbl	mode_size(%rax), %ebx
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	cmpq	%rbx, %r13
	jb	.LBB12_77
# BB#59:                                # %while.body.i.preheader
                                        #   in Loop: Header=BB12_51 Depth=1
	movq	$0, 104(%rsp)           # 8-byte Folded Spill
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%rax, %rax
	jne	.LBB12_61
# BB#60:                                # %while.body.i.preheader
                                        #   in Loop: Header=BB12_51 Depth=1
	movq	%rbx, 104(%rsp)         # 8-byte Spill
.LBB12_61:                              # %while.body.i.preheader
                                        #   in Loop: Header=BB12_51 Depth=1
	movq	%rax, 88(%rsp)          # 8-byte Spill
	je	.LBB12_63
# BB#62:                                # %while.body.i.preheader
                                        #   in Loop: Header=BB12_51 Depth=1
	movq	%rbx, 88(%rsp)          # 8-byte Spill
.LBB12_63:                              # %while.body.i.preheader
                                        #   in Loop: Header=BB12_51 Depth=1
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	jne	.LBB12_64
# BB#67:                                # %if.end14.i.us.preheader
                                        #   in Loop: Header=BB12_51 Depth=1
	movq	104(%rsp), %r15         # 8-byte Reload
	negq	%r15
	.align	16, 0x90
.LBB12_68:                              # %if.end14.i.us
                                        #   Parent Loop BB12_51 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	(%r15,%r14), %rax
	movq	104(%rsp), %rcx         # 8-byte Reload
	subq	%rcx, %r14
	cmpq	$96, 96(%rsp)           # 8-byte Folded Reload
	jne	.LBB12_69
# BB#70:                                # %if.then16.i.us
                                        #   in Loop: Header=BB12_68 Depth=2
	movq	$1, %r8
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	adjust_automodify_address_1
	jmp	.LBB12_71
	.align	16, 0x90
.LBB12_69:                              # %if.else19.i.us
                                        #   in Loop: Header=BB12_68 Depth=2
	movq	$1, %rcx
	movq	$1, %r8
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	adjust_address_1
.LBB12_71:                              # %if.end30.i.us
                                        #   in Loop: Header=BB12_68 Depth=2
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_single_push_insn
	movq	88(%rsp), %rax          # 8-byte Reload
	addq	%rax, %r14
	subq	%rbx, %r13
	cmpq	%rbx, %r13
	jae	.LBB12_68
	jmp	.LBB12_77
.LBB12_64:                              # %if.then5.i.preheader
                                        #   in Loop: Header=BB12_51 Depth=1
	leaq	(%r15,%r15,4), %rax
	movq	insn_data+16(,%rax,8), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	104(%rsp), %rax         # 8-byte Reload
	negq	%rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB12_65:                              # %if.then5.i
                                        #   Parent Loop BB12_51 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r13, %rbx
	movq	24(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r14), %r15
	movq	104(%rsp), %rax         # 8-byte Reload
	subq	%rax, %r14
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	je	.LBB12_66
# BB#72:                                # %if.else.i
                                        #   in Loop: Header=BB12_65 Depth=2
	movq	$1, %rcx
	movq	$1, %r8
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	adjust_address_1
	jmp	.LBB12_73
	.align	16, 0x90
.LBB12_66:                              # %if.then7.i
                                        #   in Loop: Header=BB12_65 Depth=2
	movq	$1, %r8
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%r15, %rcx
	callq	adjust_automodify_address_1
.LBB12_73:                              # %if.end14.i
                                        #   in Loop: Header=BB12_65 Depth=2
	movq	%rax, %r13
	cmpq	$96, 96(%rsp)           # 8-byte Folded Reload
	jne	.LBB12_75
# BB#74:                                # %if.then16.i
                                        #   in Loop: Header=BB12_65 Depth=2
	movq	$1, %r8
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	%r15, %rcx
	callq	adjust_automodify_address_1
	jmp	.LBB12_76
	.align	16, 0x90
.LBB12_75:                              # %if.else19.i
                                        #   in Loop: Header=BB12_65 Depth=2
	movq	$1, %rcx
	movq	$1, %r8
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	adjust_address_1
.LBB12_76:                              # %if.end30.i
                                        #   in Loop: Header=BB12_65 Depth=2
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rcx, %rsi
	callq	*32(%rsp)               # 8-byte Folded Reload
	movq	%rax, %rdi
	callq	emit_insn
	movq	88(%rsp), %rax          # 8-byte Reload
	addq	%rax, %r14
	movq	%rbx, %r13
	movq	40(%rsp), %rax          # 8-byte Reload
	subq	%rax, %r13
	cmpq	%rax, %r13
	jae	.LBB12_65
	.align	16, 0x90
.LBB12_77:                              # %if.end196
                                        #   in Loop: Header=BB12_51 Depth=1
	movq	48(%rsp), %rax          # 8-byte Reload
	movzbl	mode_size(%rax), %r15d
	cmpq	$1, %r15
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	ja	.LBB12_51
.LBB12_78:                              # %while.end
	testq	%r13, %r13
	jne	.LBB12_79
# BB#80:                                # %if.end204
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB12_26:                              # %if.then28.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.move_by_pieces_ninsns, %rcx
	movq	$1542, %rsi             # imm = 0x606
	jmp	.LBB12_27
.LBB12_79:                              # %if.then203
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.move_by_pieces, %rcx
	movq	$1503, %rsi             # imm = 0x5DF
.LBB12_27:                              # %if.then28.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp135:
	.size	move_by_pieces, .Ltmp135-move_by_pieces
	.cfi_endproc

	.globl	emit_block_move
	.align	16, 0x90
	.type	emit_block_move,@function
emit_block_move:                        # @emit_block_move
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
	subq	$32, %rsp
.Ltmp147:
	.cfi_def_cfa_offset 80
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
	movq	16(%rdi), %rax
	movq	%rdx, %r14
	movq	%rsi, %r12
	testq	%rax, %rax
	movq	$8, %rdx
	movq	$8, %rsi
	je	.LBB13_2
# BB#1:                                 # %cond.true
	movl	32(%rax), %esi
.LBB13_2:                               # %cond.end
	movq	16(%r12), %rcx
	testq	%rcx, %rcx
	je	.LBB13_4
# BB#3:                                 # %cond.true10
	movl	32(%rcx), %edx
.LBB13_4:                               # %cond.end16
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rdx
	andq	%rbx, %rsi
	movq	$8, %r15
	cmpq	%rdx, %rsi
	jae	.LBB13_7
# BB#5:                                 # %cond.true19
	testq	%rax, %rax
	je	.LBB13_9
# BB#6:                                 # %cond.true24
	movl	32(%rax), %r15d
	jmp	.LBB13_9
.LBB13_7:                               # %cond.false32
	testq	%rcx, %rcx
	je	.LBB13_9
# BB#8:                                 # %cond.true37
	movl	32(%rcx), %r15d
.LBB13_9:                               # %cond.end45
	movl	(%rdi), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$3342336, %rax          # imm = 0x330000
	jne	.LBB13_10
# BB#12:                                # %if.end
	movl	(%r12), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$3342336, %rax          # imm = 0x330000
	jne	.LBB13_13
# BB#14:                                # %if.end53
	movq	$1, %rsi
	callq	protect_from_queue
	movq	%rax, %rbx
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	protect_from_queue
	movq	%rax, %r12
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	protect_from_queue
	movzbl	(%rbx), %ecx
	cmpq	$66, %rcx
	jne	.LBB13_15
# BB#16:                                # %if.end60
	movzbl	(%r12), %ecx
	cmpq	$66, %rcx
	jne	.LBB13_17
# BB#18:                                # %if.end65
	testq	%rax, %rax
	je	.LBB13_19
# BB#20:                                # %if.end68
	movq	%r12, 16(%rsp)          # 8-byte Spill
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB13_21
# BB#39:                                # %land.lhs.true
	movl	class_narrowest_mode+4(%rip), %r13d
	movq	8(%rax), %r14
	xorq	%r12, %r12
	testq	%r13, %r13
	je	.LBB13_40
# BB#41:
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$17, %rax
	.align	16, 0x90
.LBB13_42:                              # %for.body.lr.ph.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_43 Depth 2
	movq	%r13, %rbx
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB13_43:                              # %for.body.i
                                        #   Parent Loop BB13_42 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rax, %rcx
	movq	%rbx, %rdi
	jb	.LBB13_45
# BB#44:                                # %for.body.i
                                        #   in Loop: Header=BB13_43 Depth=2
	movq	%rsi, %rdi
.LBB13_45:                              # %for.body.i
                                        #   in Loop: Header=BB13_43 Depth=2
	movzbl	mode_wider_mode(%rdx), %ebx
	testq	%rbx, %rbx
	movq	%rdi, %rsi
	jne	.LBB13_43
# BB#46:                                # %for.end.i
                                        #   in Loop: Header=BB13_42 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdi
	je	.LBB13_51
# BB#47:                                # %if.end10.i
                                        #   in Loop: Header=BB13_42 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	movq	optab_table+240(%rip), %rax
	sarq	%cl, %rbx
	movq	$4, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB13_50
# BB#48:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB13_42 Depth=1
	callq	get_mode_alignment
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$128, %rax
	ja	.LBB13_50
# BB#49:                                # %if.then17.i
                                        #   in Loop: Header=BB13_42 Depth=1
	movzbl	mode_size(%rbx), %ecx
	xorq	%rdx, %rdx
	movq	%r14, %rax
	divq	%rcx
	addq	%rax, %r12
	movq	%rdx, %r14
.LBB13_50:                              # %if.end24.i
                                        #   in Loop: Header=BB13_42 Depth=1
	movzbl	mode_size(%rbx), %eax
	cmpq	$1, %rax
	ja	.LBB13_42
	jmp	.LBB13_51
.LBB13_21:                              # %if.end68.if.else_crit_edge
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movl	class_narrowest_mode+4(%rip), %r13d
	jmp	.LBB13_22
.LBB13_40:
	movq	%rax, 24(%rsp)          # 8-byte Spill
.LBB13_51:                              # %while.end.i
	testq	%r14, %r14
	jne	.LBB13_52
# BB#53:                                # %move_by_pieces_ninsns.exit
	movl	optimize_size(%rip), %eax
	testq	%rax, %rax
	movq	$3, %rax
	jne	.LBB13_55
# BB#54:                                # %cond.false76
	movq	ix86_cost(%rip), %rax
	movl	40(%rax), %eax
.LBB13_55:                              # %cond.end77
	cmpq	%rax, %r12
	jae	.LBB13_22
# BB#56:                                # %if.then81
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdx
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	move_by_pieces
	xorq	%rbx, %rbx
	jmp	.LBB13_63
.LBB13_22:                              # %if.else
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rax, %r15
	movq	$3, %rcx
	shrq	%cl, %r15
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	$1, %rax
	movl	%eax, volatile_ok(%rip)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	je	.LBB13_59
# BB#23:                                # %for.body.lr.ph
	movabsq	$4294967264, %r14       # imm = 0xFFFFFFE0
	.align	16, 0x90
.LBB13_24:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movslq	movstr_optab(,%rbx,4), %rax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB13_58
# BB#25:                                # %land.lhs.true92
                                        #   in Loop: Header=BB13_24 Depth=1
	movq	24(%rsp), %rcx          # 8-byte Reload
	movzbl	(%rcx), %ecx
	cmpq	$54, %rcx
	jne	.LBB13_27
# BB#26:                                # %land.lhs.true97
                                        #   in Loop: Header=BB13_24 Depth=1
	movq	mode_mask_array(,%rbx,8), %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rsi
	movq	$1, %rcx
	shrq	%cl, %rdx
	cmpq	%rdx, %rsi
	jbe	.LBB13_28
.LBB13_27:                              # %lor.lhs.false
                                        #   in Loop: Header=BB13_24 Depth=1
	movl	target_flags(%rip), %edx
	movzbl	mode_bitsize(%rbx,%rbx), %esi
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	andq	%r14, %rdx
	cmpq	%rdx, %rsi
	jb	.LBB13_58
.LBB13_28:                              # %land.lhs.true112
                                        #   in Loop: Header=BB13_24 Depth=1
	leaq	(%rax,%rax,4), %r12
	movq	insn_data+24(,%r12,8), %r15
	movq	(%r15), %rax
	testq	%rax, %rax
	je	.LBB13_30
# BB#29:                                # %lor.lhs.false118
                                        #   in Loop: Header=BB13_24 Depth=1
	movq	$51, %rsi
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	*%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_58
.LBB13_30:                              # %land.lhs.true121
                                        #   in Loop: Header=BB13_24 Depth=1
	movq	24(%r15), %rax
	testq	%rax, %rax
	je	.LBB13_32
# BB#31:                                # %lor.lhs.false129
                                        #   in Loop: Header=BB13_24 Depth=1
	movq	$51, %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	*%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_58
.LBB13_32:                              # %land.lhs.true132
                                        #   in Loop: Header=BB13_24 Depth=1
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.LBB13_34
# BB#33:                                # %lor.lhs.false140
                                        #   in Loop: Header=BB13_24 Depth=1
	xorq	%rsi, %rsi
	movq	(%rsp), %rdi            # 8-byte Reload
	callq	*%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_58
.LBB13_34:                              # %if.then143
                                        #   in Loop: Header=BB13_24 Depth=1
	callq	get_last_insn
	movq	%rax, %r14
	xorq	%rsi, %rsi
	movq	$1, %rcx
	movq	%r13, %rdi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	convert_modes
	movq	48(%r15), %rcx
	movq	%rax, %r15
	testq	%rcx, %rcx
	je	.LBB13_37
# BB#35:                                # %land.lhs.true153
                                        #   in Loop: Header=BB13_24 Depth=1
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	*%rcx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB13_37
# BB#36:                                # %if.then156
                                        #   in Loop: Header=BB13_24 Depth=1
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %r15
.LBB13_37:                              # %if.end158
                                        #   in Loop: Header=BB13_24 Depth=1
	movq	16(%rsp), %r13          # 8-byte Reload
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r13, %rsi
	movq	%r15, %rdx
	movq	(%rsp), %rcx            # 8-byte Reload
	callq	*insn_data+16(,%r12,8)
	testq	%rax, %rax
	jne	.LBB13_38
# BB#57:                                # %if.else165
                                        #   in Loop: Header=BB13_24 Depth=1
	movq	%r13, 16(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	callq	delete_insns_since
	movabsq	$4294967264, %r14       # imm = 0xFFFFFFE0
	.align	16, 0x90
.LBB13_58:                              # %for.inc
                                        #   in Loop: Header=BB13_24 Depth=1
	movzbl	mode_wider_mode(%rbx), %r13d
	testq	%r13, %r13
	jne	.LBB13_24
.LBB13_59:                              # %for.end
	movl	target_flags(%rip), %edi
	xorq	%rax, %rax
	movl	%eax, volatile_ok(%rip)
	movq	$25, %rcx
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	8(%rax), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	copy_to_mode_reg
	movl	target_flags(%rip), %edi
	movq	%rax, %r13
	movq	$25, %rcx
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	copy_to_mode_reg
	movq	sizetype_tab(%rip), %rcx
	movl	60(%rcx), %edi
	movq	%rax, %r14
	movq	$9, %rcx
	shrq	%cl, %rdi
	andq	$127, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	copy_to_mode_reg
	cmpq	$0, emit_block_move.fn(%rip)
	movq	%rax, %r15
	jne	.LBB13_61
# BB#60:                                # %if.then191
	movabsq	$.L.str1, %rdi
	callq	get_identifier
	movq	global_trees+216(%rip), %rdi
	movq	%rax, emit_block_move.fn(%rip)
	movabsq	$emit_block_move.fn, %rbx
	callq	build_pointer_type
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	build_function_type
	movq	emit_block_move.fn(%rip), %rsi
	movq	$30, %rdi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, emit_block_move.fn(%rip)
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	ggc_add_tree_root
	movq	emit_block_move.fn(%rip), %rax
	orq	$256, 48(%rax)          # imm = 0x100
	movq	emit_block_move.fn(%rip), %rax
	movl	16(%rax), %ecx
	orq	$524288, %rcx           # imm = 0x80000
	movl	%ecx, 16(%rax)
	movq	emit_block_move.fn(%rip), %rax
	orq	$4194304, 48(%rax)      # imm = 0x400000
	movq	emit_block_move.fn(%rip), %rax
	movl	16(%rax), %ecx
	orq	$131072, %rcx           # imm = 0x20000
	movl	%ecx, 16(%rax)
	movq	emit_block_move.fn(%rip), %rdi
	xorq	%rsi, %rsi
	callq	make_decl_rtl
	movq	emit_block_move.fn(%rip), %rdi
	callq	assemble_external
.LBB13_61:                              # %if.end209
	movq	global_trees+216(%rip), %rdi
	callq	build_pointer_type
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	make_tree
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	build_tree_list
	movq	global_trees+216(%rip), %rdi
	movq	%rax, %rbx
	callq	build_pointer_type
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	make_tree
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	build_tree_list
	movq	%rax, (%rbx)
	movq	sizetype_tab(%rip), %rdi
	movq	%r15, %rsi
	callq	make_tree
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	build_tree_list
	movq	(%rbx), %rcx
	movq	%rax, (%rcx)
	movq	(%rbx), %rax
	movq	(%rax), %rax
	movq	$0, (%rax)
	movq	emit_block_move.fn(%rip), %rax
	movq	8(%rax), %rdi
	callq	build_pointer_type
	movq	emit_block_move.fn(%rip), %rdx
	movq	$121, %rdi
	movq	%rax, %rsi
	callq	build1
	movq	emit_block_move.fn(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rsi
	movq	%rax, %rcx
	movq	$53, %rdi
	xorq	%r8, %r8
	xorq	%rax, %rax
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	build
	movl	16(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movzbl	3(%r13), %ecx
	movq	%rax, %rbx
	testq	$4, %rcx
	je	.LBB13_63
# BB#62:                                # %if.then247
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
.LBB13_63:                              # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB13_38:                              # %if.then163
	movq	%rax, %rdi
	callq	emit_insn
	xorq	%rbx, %rbx
	movl	%ebx, volatile_ok(%rip)
	jmp	.LBB13_63
.LBB13_10:                              # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.emit_block_move, %rcx
	movq	$1634, %rsi             # imm = 0x662
	jmp	.LBB13_11
.LBB13_13:                              # %if.then52
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.emit_block_move, %rcx
	movq	$1637, %rsi             # imm = 0x665
	jmp	.LBB13_11
.LBB13_15:                              # %if.then59
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.emit_block_move, %rcx
	movq	$1644, %rsi             # imm = 0x66C
	jmp	.LBB13_11
.LBB13_17:                              # %if.then64
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.emit_block_move, %rcx
	movq	$1646, %rsi             # imm = 0x66E
	jmp	.LBB13_11
.LBB13_19:                              # %if.then67
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.emit_block_move, %rcx
	movq	$1648, %rsi             # imm = 0x670
	jmp	.LBB13_11
.LBB13_52:                              # %if.then28.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.move_by_pieces_ninsns, %rcx
	movq	$1542, %rsi             # imm = 0x606
.LBB13_11:                              # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp153:
	.size	emit_block_move, .Ltmp153-emit_block_move
	.cfi_endproc

	.globl	expand_expr
	.align	16, 0x90
	.type	expand_expr,@function
expand_expr:                            # @expand_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp160:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp161:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp162:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp163:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp164:
	.cfi_def_cfa_offset 48
	subq	$192, %rsp
.Ltmp165:
	.cfi_def_cfa_offset 240
.Ltmp166:
	.cfi_offset %rbx, -48
.Ltmp167:
	.cfi_offset %r12, -40
.Ltmp168:
	.cfi_offset %r13, -32
.Ltmp169:
	.cfi_offset %r14, -24
.Ltmp170:
	.cfi_offset %r15, -16
	movq	%rdi, %r10
	movq	8(%r10), %rdi
	movl	16(%rdi), %eax
	movq	%rcx, %r14
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, 188(%rsp)
	movzbl	16(%r10), %r9d
	movq	%rdx, %rbx
	movq	%rsi, %r8
	testq	%r9, %r9
	je	.LBB14_2
# BB#1:                                 # %lor.lhs.false
	movzbl	16(%rdi), %eax
	testq	%rax, %rax
	je	.LBB14_2
# BB#5:                                 # %if.end18
	movl	60(%rdi), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	xorq	%r13, %r13
	testq	%r8, %r8
	je	.LBB14_11
# BB#6:                                 # %lor.lhs.false.i
	movl	(%r8), %eax
	andq	$67174399, %rax         # imm = 0x400FFFF
	cmpq	$61, %rax
	jne	.LBB14_11
# BB#7:                                 # %lor.lhs.false5.i
	movl	8(%r8), %eax
	cmpq	$53, %rax
	jb	.LBB14_11
# BB#8:                                 # %lor.lhs.false7.i
	movq	%r9, 112(%rsp)          # 8-byte Spill
	movq	%rbx, 80(%rsp)          # 8-byte Spill
	movq	%r14, %r12
	movq	%rdi, %r14
	movq	%rdx, %r15
	movq	%r10, %rbx
	movq	%r8, %r13
	callq	preserve_subexpressions_p
	movq	%r13, %r8
	xorq	%r13, %r13
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB14_10
# BB#9:                                 # %lor.lhs.false7.i
	movq	%r8, %r13
.LBB14_10:                              # %lor.lhs.false7.i
	movq	%rbx, %r10
	movq	%r15, %rdx
	movq	%r14, %rdi
	movq	%r12, %r14
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	112(%rsp), %r9          # 8-byte Reload
.LBB14_11:                              # %get_subtarget.exit
	movq	const_int_rtx+512(%rip), %rsi
	andq	$127, %rdx
	cmpq	%r8, %rsi
	movq	$1, %r15
	je	.LBB14_19
# BB#12:                                # %lor.rhs
	leaq	-114(%r9), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$4, %rcx
	jb	.LBB14_15
# BB#13:                                # %lor.rhs
	cmpq	$51, %r9
	je	.LBB14_15
# BB#14:                                # %lor.rhs
	xorq	%r15, %r15
	cmpq	$122, %r9
	jne	.LBB14_48
.LBB14_15:                              # %lor.end
	movzbl	16(%rdi), %ecx
	cmpq	$5, %rcx
	jne	.LBB14_16
# BB#17:                                # %lor.end
	movabsq	$1, %r15
	jmp	.LBB14_18
.LBB14_2:                               # %if.then
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	const_tiny_rtx(,%rbx,8), %rax
	testq	%rax, %rax
	jne	.LBB14_4
# BB#3:
	movq	const_int_rtx+512(%rip), %rax
	jmp	.LBB14_4
.LBB14_16:
	movabsq	$0, %r15
.LBB14_18:                              # %lor.end
	jne	.LBB14_48
.LBB14_19:                              # %if.then41
	movl	16(%r10), %eax
	testq	$256, %rax              # imm = 0x100
	je	.LBB14_28
# BB#20:                                # %if.end48
	testq	$2048, %rax             # imm = 0x800
	je	.LBB14_29
# BB#21:                                # %land.lhs.true
	andq	$255, %rax
	cmpq	$30, %rax
	je	.LBB14_29
# BB#22:                                # %land.lhs.true
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	je	.LBB14_29
# BB#23:                                # %land.lhs.true
	movq	%rdx, %rcx
	andq	%rax, %rcx
	cmpq	$51, %rcx
	je	.LBB14_29
# BB#24:                                # %land.lhs.true
	andq	%r14, %rax
	cmpq	$2, %rax
	je	.LBB14_29
# BB#25:                                # %if.then65
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%r10, %rdi
	movq	%r14, %rcx
	callq	expand_expr
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB14_27
# BB#26:                                # %if.then70
	movq	%rax, %rdi
	callq	copy_to_reg
.LBB14_27:                              # %if.end72
	movq	const_int_rtx+512(%rip), %rsi
.LBB14_28:                              # %return
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_48:                              # %if.end158
	movq	%r14, 112(%rsp)         # 8-byte Spill
	movl	cse_not_expected(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB14_49
# BB#50:                                # %if.end158
	andq	%rdx, %rax
	cmpq	$51, %rax
	je	.LBB14_49
# BB#51:                                # %if.end158
	xorq	%r12, %r12
	movq	%rdi, 104(%rsp)         # 8-byte Spill
	testq	%r8, %r8
	je	.LBB14_52
# BB#53:                                # %land.lhs.true165
	movq	%rdx, 96(%rsp)          # 8-byte Spill
	movzbl	(%r8), %ecx
	cmpq	$61, %rcx
	jne	.LBB14_56
# BB#54:                                # %lor.lhs.false170
	movl	8(%r8), %ecx
	cmpq	$52, %rcx
	ja	.LBB14_55
.LBB14_56:                              # %land.lhs.true174
	cmpq	$46, %r9
	movq	%r13, 88(%rsp)          # 8-byte Spill
	jne	.LBB14_45
# BB#57:                                # %land.lhs.true177
	movl	target_flags(%rip), %edx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	addq	$4, %rdx
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	ja	.LBB14_59
# BB#58:                                # %land.lhs.true177
	movq	%r13, %r8
.LBB14_59:                              # %land.lhs.true177
	movq	%r8, %r12
	movq	96(%rsp), %rcx          # 8-byte Reload
.LBB14_60:                              # %if.else1295
	movl	16(%r10), %eax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB14_300
# BB#61:                                # %land.lhs.true1302
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rcx
	cmpq	$51, %rcx
	jne	.LBB14_65
# BB#62:                                # %land.lhs.true1305
	testq	%r12, %r12
	je	.LBB14_303
# BB#63:                                # %land.lhs.true1308
	movq	$1, %rdx
	movq	%r12, %rdi
	movq	%r10, %rsi
	movq	%r10, %rbx
	callq	safe_from_p
	movq	%rbx, %r10
	testq	%r15, %rax
	je	.LBB14_303
# BB#64:                                # %land.lhs.true1308.lor.lhs.false1311_crit_edge
	movl	16(%r10), %eax
.LBB14_65:                              # %lor.lhs.false1311
	testq	$1024, %rax             # imm = 0x400
	jne	.LBB14_303
# BB#66:                                # %lor.lhs.false1317
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	40(%rax), %rdi
	movq	$1, %rsi
	movq	%r10, %rbx
	callq	host_integerp
	movq	%rbx, %r10
	testq	%r15, %rax
	je	.LBB14_300
# BB#67:                                # %land.lhs.true1321
	movq	%r12, 88(%rsp)          # 8-byte Spill
	movq	%r10, 72(%rsp)          # 8-byte Spill
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	40(%rax), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movl	class_narrowest_mode+4(%rip), %r13d
	movq	%rax, %r14
	movq	$0, 96(%rsp)            # 8-byte Folded Spill
	testq	%r13, %r13
	je	.LBB14_78
# BB#68:
	movq	$17, %rax
.LBB14_69:                              # %for.body.lr.ph.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB14_70 Depth 2
	movq	%r13, %rbx
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB14_70:                              # %for.body.i
                                        #   Parent Loop BB14_69 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rax, %rcx
	movq	%rbx, %rdi
	jb	.LBB14_72
# BB#71:                                # %for.body.i
                                        #   in Loop: Header=BB14_70 Depth=2
	movq	%rsi, %rdi
.LBB14_72:                              # %for.body.i
                                        #   in Loop: Header=BB14_70 Depth=2
	movzbl	mode_wider_mode(%rdx), %ebx
	testq	%rbx, %rbx
	movq	%rdi, %rsi
	jne	.LBB14_70
# BB#73:                                # %for.end.i
                                        #   in Loop: Header=BB14_69 Depth=1
	testq	%r15, %rdi
	je	.LBB14_78
# BB#74:                                # %if.end10.i
                                        #   in Loop: Header=BB14_69 Depth=1
	movq	$32, %rcx
	movq	%rdi, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	movq	optab_table+240(%rip), %rax
	sarq	%cl, %r12
	movq	$4, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB14_77
# BB#75:                                # %land.lhs.true.i8426
                                        #   in Loop: Header=BB14_69 Depth=1
	callq	get_mode_alignment
	andq	%r15, %rax
	cmpq	$128, %rax
	ja	.LBB14_77
# BB#76:                                # %if.then17.i
                                        #   in Loop: Header=BB14_69 Depth=1
	movzbl	mode_size(%r12), %ecx
	xorq	%rdx, %rdx
	movq	%r14, %rax
	divq	%rcx
	addq	%rax, 96(%rsp)          # 8-byte Folded Spill
	movq	%rdx, %r14
.LBB14_77:                              # %if.end24.i
                                        #   in Loop: Header=BB14_69 Depth=1
	movzbl	mode_size(%r12), %eax
	cmpq	$1, %rax
	ja	.LBB14_69
.LBB14_78:                              # %while.end.i8431
	testq	%r14, %r14
	movq	72(%rsp), %r10          # 8-byte Reload
	jne	.LBB14_79
# BB#296:                               # %move_by_pieces_ninsns.exit
	movl	optimize_size(%rip), %eax
	testq	%rax, %rax
	movq	$3, %rax
	movq	88(%rsp), %r12          # 8-byte Reload
	jne	.LBB14_298
# BB#297:                               # %cond.false1329
	movq	ix86_cost(%rip), %rax
	movl	40(%rax), %eax
.LBB14_298:                             # %cond.end1330
	cmpq	%rax, 96(%rsp)          # 8-byte Folded Reload
	jb	.LBB14_300
# BB#299:                               # %land.lhs.true1335
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	mostly_zeros_p
	movq	%rbx, %r10
	testq	%r15, %rax
	je	.LBB14_303
.LBB14_300:                             # %lor.lhs.false1338
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	112(%rsp), %rax         # 8-byte Reload
	andq	%r14, %rax
	cmpq	$3, %rax
	jne	.LBB14_305
# BB#301:                               # %land.lhs.true1341
	movzbl	17(%r10), %eax
	testq	$2, %rax
	je	.LBB14_305
# BB#302:                               # %if.then1347.thread
	movq	$1, %rsi
	movq	%r10, %rdi
	callq	output_constant_def
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_29:                              # %if.end73
	movq	%rdi, 104(%rsp)         # 8-byte Spill
	movzbl	tree_code_type(%r9), %eax
	cmpq	$49, %rax
	je	.LBB14_33
# BB#30:                                # %if.end73
	cmpq	$39, %r9
	je	.LBB14_33
# BB#31:                                # %if.end73
	cmpq	$41, %r9
	je	.LBB14_33
# BB#32:                                # %if.end73
	cmpq	$42, %r9
	je	.LBB14_33
# BB#34:                                # %if.else
	movq	%rdx, 96(%rsp)          # 8-byte Spill
	cmpq	$50, %rax
	je	.LBB14_38
# BB#35:                                # %lor.lhs.false96
	cmpq	$60, %rax
	je	.LBB14_38
# BB#36:                                # %lor.lhs.false96
	cmpq	$43, %r9
	je	.LBB14_38
# BB#37:                                # %lor.lhs.false96
	cmpq	$44, %r9
	je	.LBB14_38
# BB#41:                                # %if.else117
	leaq	-91(%r9), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$1, %rcx
	ja	.LBB14_43
# BB#42:                                # %land.lhs.true123
	movq	40(%r10), %rax
	movzbl	17(%rax), %eax
	testq	$1, %rax
	je	.LBB14_33
.LBB14_43:                              # %if.else138
	cmpq	$40, %r9
	jne	.LBB14_44
# BB#47:                                # %if.then141
	movq	32(%r10), %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rcx
	movq	%r10, %rbx
	callq	expand_expr
	movq	40(%rbx), %rdi
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rcx
	callq	expand_expr
	movq	48(%rbx), %rdi
	jmp	.LBB14_39
.LBB14_33:                              # %if.then87
	movq	32(%r10), %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rcx
.LBB14_383:                             # %return
	callq	expand_expr
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_38:                              # %if.then108
	movq	32(%r10), %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rcx
	movq	%r10, %rbx
	callq	expand_expr
	movq	40(%rbx), %rdi
.LBB14_39:                              # %return
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rcx
.LBB14_40:                              # %return
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_49:
	xorq	%r12, %r12
	movq	%rdi, 104(%rsp)         # 8-byte Spill
.LBB14_52:
	movq	%rdx, 96(%rsp)          # 8-byte Spill
	movq	%r8, 88(%rsp)           # 8-byte Spill
	jmp	.LBB14_45
.LBB14_55:
	movq	%r8, 88(%rsp)           # 8-byte Spill
	jmp	.LBB14_45
.LBB14_303:                             # %if.then1347
	movq	$1, %rsi
	movq	%r10, %rdi
	callq	output_constant_def
	movq	%rax, %rsi
	movq	112(%rsp), %rax         # 8-byte Reload
	decq	%rax
	andq	%r15, %rax
	cmpq	$3, %rax
	jb	.LBB14_28
# BB#304:                               # %if.then1357
	movq	%rsi, %rdi
	callq	validize_mem
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_305:                             # %if.else1360
	testq	%r12, %r12
	je	.LBB14_308
# BB#306:                               # %lor.lhs.false1363
	movq	$1, %rdx
	movq	%r12, %rdi
	movq	%r10, %rsi
	movq	%r10, %rbx
	callq	safe_from_p
	movq	%rbx, %r10
	testq	%r14, %rax
	je	.LBB14_308
# BB#307:                               # %lor.lhs.false1366
	movzbl	(%r12), %eax
	cmpq	$39, %rax
	jne	.LBB14_313
.LBB14_308:                             # %if.then1371
	movq	104(%rsp), %rdi         # 8-byte Reload
	movl	16(%rdi), %esi
	movq	$12, %rcx
	movq	%rsi, %r8
	shrq	%cl, %r8
	movq	$10, %rcx
	movq	%rsi, %rdx
	shrq	%cl, %rdx
	movl	60(%rdi), %eax
	movq	$19, %rcx
	shrq	%cl, %rax
	andq	$255, %rsi
	xorq	%r9, %r9
	cmpq	$20, %rsi
	jne	.LBB14_312
# BB#309:                               # %land.rhs1398
	cmpq	$0, 8(%rdi)
	je	.LBB14_310
# BB#311:                               # %land.rhs1398
	movabsq	$1, %r9
	jmp	.LBB14_312
.LBB14_44:
	movq	%r14, 112(%rsp)         # 8-byte Spill
	movq	$1, %r12
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
.LBB14_45:                              # %if.end185
	leaq	-25(%r9), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$121, %rax
	jbe	.LBB14_46
.LBB14_1201:                            # %sw.default
	movq	%r10, %rdi
.LBB14_571:                             # %return
	movq	%r8, %rsi
	movq	%rbx, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	*lang_expand_expr(%rip)
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
.LBB14_4:                               # %if.then
	addq	$192, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB14_310:
	movabsq	$0, %r9
.LBB14_312:                             # %land.end1402
	andq	$1, %r8
	andq	$2, %rdx
	orq	%r8, %rdx
	andq	$4, %rax
	orq	%rdx, %rax
	movq	$3, %rcx
	movl	16(%r10), %esi
	shlq	%cl, %r9
	orq	%rax, %r9
	movq	$12, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	%r9, %rsi
	movq	%r10, %rbx
	callq	build_qualified_type
	movl	16(%rbx), %edx
	movq	$10, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	xorq	%rsi, %rsi
	movq	$1, %rcx
	movq	%rax, %rdi
	callq	assign_temp
	movq	%rbx, %r10
	movq	%rax, %r12
.LBB14_313:                             # %if.end1420
	movq	8(%r10), %rdi
	movq	%r10, %rbx
	callq	int_size_in_bytes
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%rax, %rcx
	callq	store_constructor
	movq	%r12, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_46:                              # %if.end185
	jmpq	*.LJTI14_0(,%rax,8)
.LBB14_843:                             # %sw.bb4739
	movq	%r9, 112(%rsp)          # 8-byte Spill
	movq	%r8, %r13
	movq	%r10, %r14
	testq	%rcx, %rbx
	movq	%rbx, %r15
	jne	.LBB14_845
# BB#844:                               # %sw.bb4739
	movq	96(%rsp), %r15          # 8-byte Reload
.LBB14_845:                             # %sw.bb4739
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	callq	do_store_flag
	movq	%rax, %rsi
	testq	%rsi, %rsi
	jne	.LBB14_28
# BB#846:                               # %if.end4750
	cmpq	$102, 112(%rsp)         # 8-byte Folded Reload
	movq	%r14, %r10
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, %rcx
	movq	%r13, %rdx
	jne	.LBB14_861
# BB#847:                               # %land.lhs.true4753
	movq	40(%r10), %rax
	movq	%rax, %rdi
	movq	%r10, %r14
	movq	%rcx, %r13
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	callq	integer_zerop
	movq	64(%rsp), %r8           # 8-byte Reload
	movq	%r13, %rcx
	movq	%r14, %r10
	testq	%rcx, %rax
	je	.LBB14_861
# BB#848:                               # %land.lhs.true4753
	testq	%r8, %r8
	je	.LBB14_861
# BB#849:                               # %land.lhs.true4761
	movl	(%r8), %eax
	movq	%rcx, %rdx
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	movq	%rdx, %rcx
	jne	.LBB14_861
# BB#850:                               # %land.lhs.true4766
	movq	32(%r10), %rsi
	movq	8(%rsi), %rdx
	movq	%rcx, %rdi
	movq	$16, %rcx
	movl	60(%rdx), %edx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	$9, %rcx
	shrq	%cl, %rdx
	movq	%rdi, %rcx
	andq	$127, %rdx
	cmpq	%rdx, %rax
	jne	.LBB14_861
# BB#851:                               # %if.then4782
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rsi, %rdi
	movq	%r8, %rsi
	movq	%r8, %rbx
	callq	expand_expr
	movl	(%rax), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	jne	.LBB14_856
# BB#852:                               # %if.then4791
	cmpq	$0, 8(%rax)
	je	.LBB14_855
# BB#853:                               # %if.then4797
	movq	const_int_rtx+520(%rip), %rax
	jmp	.LBB14_854
.LBB14_79:                              # %if.then28.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.move_by_pieces_ninsns, %rcx
	movq	$1542, %rsi             # imm = 0x606
	jmp	.LBB14_104
.LBB14_202:                             # %sw.bb853
	movq	%rcx, %r14
	movq	32(%r10), %rax
	movq	40(%r10), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	movq	96(%rsp), %rdx          # 8-byte Reload
	movq	%r10, %rbx
	callq	immed_double_const
	movl	16(%rbx), %ecx
	movq	%rax, %rdx
	testq	$262144, %rcx           # imm = 0x40000
	je	.LBB14_203
# BB#204:                               # %sw.bb853
	movq	112(%rsp), %rax         # 8-byte Reload
	andq	%r14, %rax
	cmpq	$3, %rax
	je	.LBB14_205
# BB#206:                               # %if.then867
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rdx, %rsi
	callq	force_reg
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_208:                             # %sw.bb873
	movq	%r10, %rdi
	callq	immed_real_const
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_209:                             # %sw.bb875
	movq	%rcx, %r14
	movq	24(%r10), %rax
	movq	%r10, %rbx
	testq	%rax, %rax
	movq	%rax, %r15
	jne	.LBB14_211
# BB#210:                               # %if.then878
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	output_constant_def
	movq	24(%rbx), %r15
.LBB14_211:                             # %if.end880
	movzbl	(%r15), %eax
	cmpq	$66, %rax
	jne	.LBB14_219
# BB#212:                               # %if.end880
	movq	112(%rsp), %rax         # 8-byte Reload
	decq	%rax
	andq	%r14, %rax
	cmpq	$3, %rax
	jb	.LBB14_219
# BB#213:                               # %land.lhs.true896
	movq	8(%r15), %rax
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	memory_address_p
	testq	%r14, %rax
	je	.LBB14_214
# BB#215:                               # %lor.lhs.false904
	movl	flag_force_addr(%rip), %eax
	movq	24(%rbx), %r15
	testq	%rax, %rax
	je	.LBB14_219
# BB#216:                               # %land.lhs.true906
	movq	8(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB14_217
.LBB14_219:                             # %if.end926
	movq	%r15, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_80:                              # %sw.bb
	movq	%rcx, %r14
	movq	%r10, %rdi
	movq	%r10, %r15
	callq	decl_function_context
	movq	current_function_decl(%rip), %rcx
	movq	%rax, %rbx
	cmpq	%rcx, %rbx
	je	.LBB14_84
# BB#81:                                # %land.lhs.true189
	movq	inline_function_decl(%rip), %rax
	cmpq	%rax, %rbx
	je	.LBB14_84
# BB#82:                                # %land.lhs.true189
	testq	%rbx, %rbx
	je	.LBB14_84
# BB#83:                                # %if.then195
	movq	%rbx, %rdi
	callq	find_function_data
	movq	%rax, %r14
	movq	%r15, %rdi
	callq	label_rtx
	movq	16(%r14), %rcx
	movq	32(%rcx), %rcx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	16(%r14), %rcx
	jmp	.LBB14_86
.LBB14_207:                             # %sw.bb870
	movq	104(%r10), %rax
	jmp	.LBB14_887
.LBB14_92:                              # %sw.bb233
	cmpq	$0, 144(%r10)
	jne	.LBB14_95
# BB#93:                                # %cond.end
	xorq	%rsi, %rsi
	movq	%r10, %rdi
	movq	%r10, %rbx
	movq	%rcx, %r14
	callq	make_decl_rtl
	movq	%r14, %rcx
	movq	%rbx, %r10
	cmpq	$0, 144(%r10)
	je	.LBB14_94
.LBB14_95:                              # %sw.bb246
	cmpq	$0, 40(%r10)
	jne	.LBB14_101
# BB#96:                                # %land.lhs.true250
	movq	8(%r10), %rax
	cmpq	$0, 32(%rax)
	je	.LBB14_101
# BB#97:                                # %land.lhs.true257
	movzbl	18(%r10), %eax
	testq	$4, %rax
	jne	.LBB14_99
# BB#98:                                # %lor.lhs.false263
	movzbl	49(%r10), %eax
	testq	$1, %rax
	je	.LBB14_101
.LBB14_99:                              # %if.then269
	movq	144(%r10), %r14
	xorq	%rsi, %rsi
	movq	%r10, %rdi
	movq	%r10, %rbx
	movq	%rcx, %r15
	callq	layout_decl
	movq	%r15, %rcx
	movq	%rbx, %r10
	testq	%r14, %r14
	je	.LBB14_101
# BB#100:                               # %if.then291
	movl	48(%r10), %eax
	movb	%al, 2(%r14)
	movq	$0, 144(%r10)
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%r10, %rsi
	movq	%r10, %rbx
	movq	%rcx, %r15
	callq	set_mem_attributes
	movq	%r15, %rcx
	movq	%rbx, %r10
	movq	%r14, 144(%r10)
.LBB14_101:                             # %sw.bb306
	movq	%rcx, %r15
	cmpq	$0, 144(%r10)
	movq	%r10, %rcx
	jne	.LBB14_105
# BB#102:                               # %cond.end316
	xorq	%rsi, %rsi
	movq	%rcx, %rbx
	movq	%rbx, %rdi
	callq	make_decl_rtl
	cmpq	$0, 144(%rbx)
	movq	%rbx, %rcx
	je	.LBB14_103
.LBB14_105:                             # %if.end321
	movzbl	18(%rcx), %eax
	testq	$1, %rax
	jne	.LBB14_107
# BB#106:                               # %if.then327
	movq	%rcx, %rbx
	movq	%rbx, %rdi
	callq	assemble_external
	movl	16(%rbx), %eax
	orq	$65536, %rax            # imm = 0x10000
	movl	%eax, 16(%rbx)
	movq	%rbx, %rcx
.LBB14_107:                             # %if.end333
	movq	%rcx, %rbx
	movq	%rbx, %rdi
	callq	decl_function_context
	movq	144(%rbx), %rcx
	testq	%rax, %rax
	je	.LBB14_151
# BB#108:                               # %if.end333
	movq	current_function_decl(%rip), %rdx
	cmpq	%rdx, %rax
	je	.LBB14_151
# BB#109:                               # %if.end333
	movq	inline_function_decl(%rip), %rdx
	cmpq	%rdx, %rax
	je	.LBB14_151
# BB#110:                               # %land.lhs.true343
	testq	%rcx, %rcx
	jne	.LBB14_112
# BB#111:                               # %cond.false350
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
.LBB14_112:                             # %cond.end353
	movzbl	(%rcx), %eax
	cmpq	$66, %rax
	jne	.LBB14_137
# BB#113:                               # %land.lhs.true359
	testq	%rcx, %rcx
	jne	.LBB14_115
# BB#114:                               # %cond.false366
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
.LBB14_115:                             # %cond.end369
	movq	8(%rcx), %rax
	movl	(%rax), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$67, %rdx
	je	.LBB14_151
# BB#116:                               # %lor.lhs.false378
	testq	%rcx, %rcx
	jne	.LBB14_118
# BB#117:                               # %cond.false385
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
	movq	8(%rcx), %rax
	movl	(%rax), %eax
.LBB14_118:                             # %cond.end388
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$68, %rdx
	je	.LBB14_151
# BB#119:                               # %lor.lhs.false397
	testq	%rcx, %rcx
	jne	.LBB14_121
# BB#120:                               # %cond.false404
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
	movq	8(%rcx), %rax
	movl	(%rax), %eax
.LBB14_121:                             # %cond.end407
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$54, %rdx
	je	.LBB14_151
# BB#122:                               # %lor.lhs.false416
	testq	%rcx, %rcx
	jne	.LBB14_124
# BB#123:                               # %cond.false423
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
	movq	8(%rcx), %rax
	movl	(%rax), %eax
.LBB14_124:                             # %cond.end426
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$55, %rdx
	je	.LBB14_151
# BB#125:                               # %lor.lhs.false435
	testq	%rcx, %rcx
	jne	.LBB14_127
# BB#126:                               # %cond.false442
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
	movq	8(%rcx), %rax
	movl	(%rax), %eax
.LBB14_127:                             # %cond.end445
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$58, %rdx
	je	.LBB14_151
# BB#128:                               # %lor.lhs.false454
	testq	%rcx, %rcx
	jne	.LBB14_130
# BB#129:                               # %cond.false461
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
	movq	8(%rcx), %rax
	movl	(%rax), %eax
.LBB14_130:                             # %cond.end464
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$134, %rdx
	je	.LBB14_151
# BB#131:                               # %lor.lhs.false473
	testq	%rcx, %rcx
	jne	.LBB14_133
# BB#132:                               # %cond.false480
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
	movq	8(%rcx), %rax
	movl	(%rax), %eax
.LBB14_133:                             # %cond.end483
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$56, %rdx
	je	.LBB14_151
# BB#134:                               # %lor.lhs.false492
	testq	%rcx, %rcx
	jne	.LBB14_136
# BB#135:                               # %cond.false499
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
	movq	8(%rcx), %rax
	movl	(%rax), %eax
.LBB14_136:                             # %cond.end502
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$140, %rax
	jne	.LBB14_137
.LBB14_151:                             # %if.else581
	testq	%rcx, %rcx
	jne	.LBB14_153
# BB#152:                               # %cond.false588
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
.LBB14_153:                             # %cond.end591
	movzbl	(%rcx), %eax
	cmpq	$66, %rax
	jne	.LBB14_160
# BB#154:                               # %land.lhs.true597
	testq	%rcx, %rcx
	jne	.LBB14_156
# BB#155:                               # %cond.false604
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
.LBB14_156:                             # %cond.end607
	movq	8(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB14_160
# BB#157:                               # %if.then616
	testq	%rcx, %rcx
	jne	.LBB14_159
# BB#158:                               # %cond.false623
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
.LBB14_159:                             # %cond.end626
	movq	%rcx, %rdi
	callq	validize_mem
	jmp	.LBB14_177
.LBB14_314:                             # %sw.bb1424
	movq	%rcx, %r13
	movq	32(%r10), %r14
	movq	%r10, %rbx
	leaq	168(%rsp), %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	string_constant
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB14_324
# BB#315:                               # %land.lhs.true1430
	movzbl	16(%r15), %eax
	cmpq	$29, %rax
	jne	.LBB14_324
# BB#316:                               # %land.lhs.true1437
	movq	168(%rsp), %r12
	movzbl	16(%r12), %eax
	cmpq	$25, %rax
	jne	.LBB14_324
# BB#317:                               # %land.lhs.true1444
	movslq	32(%r15), %rax
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB14_324
# BB#318:                               # %land.lhs.true1450
	movq	96(%rsp), %rax          # 8-byte Reload
	andq	%r13, %rax
	movl	mode_class(,%rax,4), %ecx
	cmpq	$1, %rcx
	jne	.LBB14_324
# BB#319:                               # %land.lhs.true1455
	movzbl	mode_size(%rax), %eax
	cmpq	$1, %rax
	jne	.LBB14_324
# BB#320:                               # %land.lhs.true1455
	movq	112(%rsp), %rax         # 8-byte Reload
	andq	%r13, %rax
	cmpq	$4, %rax
	je	.LBB14_324
# BB#321:                               # %if.then1464
	movq	32(%r12), %rax
	movq	40(%r15), %rcx
	jmp	.LBB14_322
.LBB14_532:                             # %sw.bb2612
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$7155, %rsi             # imm = 0x1BF3
	jmp	.LBB14_104
.LBB14_328:                             # %sw.bb1487
	movq	%r9, %r13
	movq	%rbx, %r14
	movq	32(%r10), %r12
	movq	%r10, %rbx
	movq	8(%r12), %rax
	movzbl	16(%rax), %ecx
	cmpq	$18, %rcx
	jne	.LBB14_329
# BB#330:                               # %if.end1500
	movq	24(%rax), %rax
	testq	%rax, %rax
	je	.LBB14_331
# BB#332:                               # %cond.true1508
	addq	$104, %rax
	jmp	.LBB14_333
.LBB14_523:                             # %sw.bb2538
	movq	%r10, %r14
	callq	get_last_insn
	movq	32(%r14), %rcx
	movq	%r14, %r12
	movq	%rax, %r15
	movq	%rcx, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	expand_expr
	movq	%rax, %rcx
	movl	(%rcx), %eax
	movq	%rcx, %r13
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB14_528
# BB#524:                               # %if.then2548
	callq	get_last_insn
	movq	%rax, %rbx
	cmpq	%r15, %rbx
	je	.LBB14_525
# BB#526:                               # %if.end2553
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	je	.LBB14_529
# BB#527:                               # %if.then2561
	movq	%rbx, %rdi
	callq	prev_nonnote_insn
	movq	%rax, %rbx
	jmp	.LBB14_529
.LBB14_293:                             # %sw.bb1284
	testq	%r12, %r12
	movq	96(%rsp), %rcx          # 8-byte Reload
	movq	88(%rsp), %r12          # 8-byte Reload
	je	.LBB14_60
# BB#294:                               # %if.then1286
	movq	40(%r10), %rbx
	testq	%rbx, %rbx
	je	.LBB14_28
.LBB14_295:                             # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	(%rbx), %rbx
	movq	const_int_rtx+512(%rip), %rsi
	testq	%rbx, %rbx
	jne	.LBB14_295
	jmp	.LBB14_28
.LBB14_879:                             # %sw.bb4882
	movq	32(%r10), %rax
	movq	%r10, %r14
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	jmp	.LBB14_880
.LBB14_1095:                            # %sw.bb5942
	movq	%rcx, %r13
	movq	32(%r10), %rbx
	movq	40(%r10), %r14
	testq	%r12, %r12
	je	.LBB14_1116
# BB#1096:                              # %land.lhs.true5952
	movzbl	16(%rbx), %eax
	cmpq	$39, %rax
	jne	.LBB14_1116
# BB#1097:                              # %land.lhs.true5959
	movzbl	16(%r14), %eax
	cmpq	$86, %rax
	je	.LBB14_1099
# BB#1098:                              # %land.lhs.true5959
	cmpq	$88, %rax
	jne	.LBB14_1116
.LBB14_1099:                            # %land.lhs.true5973
	movq	32(%r14), %rax
	cmpq	%rbx, %rax
	jne	.LBB14_1116
# BB#1100:                              # %land.lhs.true5979
	movq	40(%r14), %rax
	movzbl	16(%rax), %eax
	cmpq	$39, %rax
	jne	.LBB14_1116
# BB#1101:                              # %land.lhs.true5989
	movq	40(%rbx), %rax
	movq	40(%rax), %rax
	movq	%rax, %rdi
	callq	integer_onep
	testq	%r13, %rax
	je	.LBB14_1116
# BB#1102:                              # %land.lhs.true5997
	movq	40(%r14), %rax
	movq	40(%rax), %rax
	movq	40(%rax), %rax
	movq	%rax, %rdi
	callq	integer_onep
	testq	%r13, %rax
	je	.LBB14_1116
# BB#1103:                              # %if.then6008
	callq	gen_label_rtx
	movzbl	16(%r14), %edx
	movq	40(%r14), %rcx
	movq	%rax, %r15
	cmpq	$86, %rdx
                                        # kill: RAX<def> R15<kill>
	je	.LBB14_1105
# BB#1104:                              # %if.then6008
	xorq	%rax, %rax
.LBB14_1105:                            # %if.then6008
	cmpq	$88, %rdx
	movq	%r15, %rdx
	je	.LBB14_1107
# BB#1106:                              # %if.then6008
	xorq	%rdx, %rdx
.LBB14_1107:                            # %if.then6008
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	do_jump
	movzbl	16(%r14), %ecx
	movq	8(%r14), %rax
	cmpq	$86, %rcx
	je	.LBB14_1108
# BB#1109:                              # %if.then6008
	movabsq	$global_trees+88, %rcx
	jmp	.LBB14_1110
.LBB14_1094:                            # %sw.bb5930
	movq	32(%r10), %rax
	movq	40(%r10), %rcx
	xorq	$1, %r15
	movq	%rax, %rdi
	movq	%rcx, %rsi
	jmp	.LBB14_1117
.LBB14_1077:                            # %sw.bb5808
	movq	%r8, %rdx
	movq	32(%r10), %r14
	movq	%r10, %r15
	movl	16(%r14), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$34, %rcx
	jne	.LBB14_1078
# BB#1079:                              # %if.end5820
	testq	%r12, %r12
	jne	.LBB14_1081
# BB#1080:                              # %if.end5820
	movq	%rdx, 88(%rsp)          # 8-byte Spill
.LBB14_1081:                            # %if.end5820
	movq	%rax, %rcx
	orq	$65536, %rcx            # imm = 0x10000
	movl	%ecx, 16(%r14)
	movq	144(%r14), %rcx
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB14_1082
# BB#1089:                              # %if.else5882
	testq	%rcx, %rcx
	jne	.LBB14_1083
# BB#1090:                              # %if.else5906
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 144(%r14)
	xorq	%rbx, %rbx
	testq	$1024, %rax             # imm = 0x400
	je	.LBB14_1092
# BB#1091:                              # %if.then5915
	movq	%r14, %rdi
	callq	put_var_into_stack
.LBB14_1092:                            # %if.end5918
	movq	88(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB14_1093
.LBB14_888:                             # %sw.bb4896
	movq	%r8, 64(%rsp)           # 8-byte Spill
	movq	%rbx, %r14
	movq	40(%r10), %rax
	movq	%r10, %rbx
	movzbl	16(%rax), %ecx
	cmpq	$115, %rcx
	jne	.LBB14_900
# BB#889:                               # %land.lhs.true4906
	movq	48(%rbx), %rcx
	movzbl	16(%rcx), %edx
	cmpq	$115, %rdx
	jne	.LBB14_900
# BB#890:                               # %land.lhs.true4916
	movq	32(%rax), %r15
	movq	32(%rcx), %r13
	movq	8(%r15), %rax
	movq	8(%r13), %rcx
	cmpq	%rcx, %rax
	jne	.LBB14_900
# BB#891:                               # %if.then4935
	movzbl	16(%r15), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$50, %rax
	jne	.LBB14_893
# BB#892:                               # %land.lhs.true4957
	movq	32(%r15), %rax
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	operand_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB14_899
.LBB14_893:                             # %lor.lhs.false4963
	movzbl	16(%r13), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$50, %rax
	jne	.LBB14_895
# BB#894:                               # %land.lhs.true4973
	movq	32(%r13), %rax
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	operand_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB14_899
.LBB14_895:                             # %lor.lhs.false4979
	movzbl	16(%r15), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$49, %rax
	jne	.LBB14_897
# BB#896:                               # %land.lhs.true4989
	movq	32(%r15), %rax
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	operand_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB14_899
.LBB14_897:                             # %lor.lhs.false4995
	movzbl	16(%r13), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$49, %rax
	jne	.LBB14_900
# BB#898:                               # %land.lhs.true5005
	movq	32(%r13), %rax
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	operand_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_900
.LBB14_899:                             # %if.then5011
	movq	8(%r15), %rcx
	movq	32(%rbx), %rdx
	movq	$51, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%r15, %rcx
	movq	%r13, %r8
	callq	build
	movq	$115, %rdi
	movq	104(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	build1
	movq	%rax, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	jmp	.LBB14_382
.LBB14_280:                             # %sw.bb1217
	movq	%rbx, %r14
	movq	32(%r10), %r15
	movq	$2, %rdi
	xorq	%rsi, %rsi
	movq	%r10, %rbx
	callq	expand_start_bindings_and_block
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.LBB14_283
# BB#281:                               # %land.lhs.true1226
	movzbl	18(%rax), %ecx
	testq	$1, %rcx
	jne	.LBB14_283
# BB#282:                               # %if.then1236
	movq	%rax, %rdi
	callq	insert_block
	jmp	.LBB14_283
.LBB14_567:                             # %sw.bb2822
	movq	%rcx, %rsi
	movq	32(%r10), %rax
	movq	%r10, %rdx
	movzbl	16(%rax), %ecx
	cmpq	$121, %rcx
	jne	.LBB14_573
# BB#568:                               # %land.lhs.true2832
	movq	32(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$30, %rcx
	jne	.LBB14_573
# BB#569:                               # %land.lhs.true2845
	movq	48(%rax), %rax
	movq	$29, %rcx
	shrq	%cl, %rax
	andq	$3, %rax
	jne	.LBB14_570
.LBB14_573:                             # %if.end2878
	movq	%rdx, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	callq	expand_call
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_561:                             # %sw.bb2788
	movq	%rbx, %rcx
	movq	48(%r10), %rsi
	movq	%r10, %rax
	testq	%rsi, %rsi
	jne	.LBB14_28
# BB#562:                               # %if.then2794
	movq	%rax, %rbx
	movq	32(%rbx), %rax
	movq	%rax, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%rcx, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	expand_expr
	movq	%rax, 48(%rbx)
	movq	40(%rbx), %rax
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	expand_decl_cleanup
	movq	$0, 40(%rbx)
	movq	48(%rbx), %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_563:                             # %sw.bb2813
	movq	$2, %rdi
	xorq	%rsi, %rsi
	movq	%r10, %r14
	callq	expand_start_bindings_and_block
	movq	cfun(%rip), %rax
	movl	328(%rax), %ecx
	movl	%ecx, 336(%rax)
	movq	32(%r14), %rax
	movq	%rax, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	expand_expr
	testq	%r12, %r12
	jne	.LBB14_565
# BB#564:                               # %if.then2819
	movq	%rax, %rdi
	callq	force_not_mem
.LBB14_565:                             # %if.end2821
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	preserve_temp_slots
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	jmp	.LBB14_566
.LBB14_261:                             # %sw.bb1140
	movq	%r8, %r14
	movq	placeholder_list(%rip), %r15
	movq	$0, 176(%rsp)
	leaq	176(%rsp), %rax
	movq	%r10, %rdi
	movq	%rax, %rsi
	callq	find_placeholder
	testq	%rax, %rax
	je	.LBB14_262
# BB#263:                               # %if.end1145
	movq	176(%rsp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, placeholder_list(%rip)
	movq	%rax, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	expand_expr
	movq	%rax, %rsi
	movq	%r15, placeholder_list(%rip)
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_264:                             # %sw.bb1148
	movq	40(%r10), %rax
	movq	placeholder_list(%rip), %rcx
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	movq	%rcx, %rdx
	movq	%r10, %r14
	movq	%r8, %r15
	callq	tree_cons
	movq	%rax, placeholder_list(%rip)
	movq	32(%r14), %rax
	movq	%rax, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	expand_expr
	movq	placeholder_list(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rax, %rsi
	movq	%rcx, placeholder_list(%rip)
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_711:                             # %sw.bb3941
	movq	%r10, %r12
	movabsq	$4294967293, %rax       # imm = 0xFFFFFFFD
	movq	112(%rsp), %rcx         # 8-byte Reload
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB14_719
# BB#712:                               # %land.lhs.true3947
	movq	32(%r12), %rax
	movq	%rax, %rdi
	callq	really_constant_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_719
# BB#713:                               # %land.lhs.true3953
	movq	40(%r12), %rax
	movq	%rax, %rdi
	callq	really_constant_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_719
# BB#714:                               # %if.then3959
	movq	%r12, %rbx
	movq	32(%rbx), %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	112(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rcx
	callq	expand_expr
	movq	40(%rbx), %rcx
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rcx, %rdi
	movq	%r14, %rcx
	callq	expand_expr
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB14_716
# BB#715:                               # %if.then3974
	movq	8(%rax), %rax
	negq	%rax
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	plus_constant_wide
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_729:                             # %sw.bb4031
	movq	32(%r10), %rax
	movq	%r10, 72(%rsp)          # 8-byte Spill
	movzbl	16(%rax), %ecx
	cmpq	$25, %rcx
	jne	.LBB14_731
# BB#730:                               # %if.then4041
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	40(%rdx), %rcx
	movq	%rcx, 32(%rdx)
	movq	%rax, 40(%rdx)
.LBB14_731:                             # %if.end4054
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	112(%rsp), %rcx         # 8-byte Reload
	andq	%rax, %rcx
	cmpq	$1, %rcx
	jne	.LBB14_739
# BB#732:                               # %if.end4054
	movl	ptr_mode(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	andq	%rdx, %rax
	movq	96(%rsp), %rcx          # 8-byte Reload
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	jne	.LBB14_739
# BB#733:                               # %land.lhs.true4060
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_739
# BB#734:                               # %if.then4066
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	expand_expr
	movq	%rax, %rbx
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	je	.LBB14_736
# BB#735:                               # %if.then4075
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	force_operand
	movq	%rax, %rbx
	movl	(%rbx), %eax
.LBB14_736:                             # %if.end4077
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	je	.LBB14_738
# BB#737:                               # %if.then4082
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %rbx
.LBB14_738:                             # %if.end4084
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	$78, %rdi
	jmp	.LBB14_717
.LBB14_777:                             # %if.end185.sw.bb4454_crit_edge8607
	movq	%r9, %r15
	movq	%rcx, %r14
	leaq	40(%r10), %rax
	movq	%r10, %rbx
	movq	(%rax), %rax
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	safe_from_p
	andq	%r14, %rax
	je	.LBB14_779
# BB#778:                               # %sw.bb4454
	movq	%r13, %rax
.LBB14_779:                             # %sw.bb4454
	movq	32(%rbx), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rsi, %rdi
	movq	%rax, %rsi
	callq	expand_expr
	movq	40(%rbx), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movl	188(%rsp), %ecx
	movq	$1, %rdi
	jmp	.LBB14_770
.LBB14_771:                             # %sw.bb4423
	movq	%rbx, 80(%rsp)          # 8-byte Spill
	movq	%r10, %r12
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB14_1202
# BB#772:                               # %sw.bb4423
	movl	flag_unsafe_math_optimizations(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_1202
# BB#773:                               # %sw.bb4423
	movl	optimize_size(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB14_1202
# BB#774:                               # %land.lhs.true4429
	movq	104(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	jne	.LBB14_1202
# BB#775:                               # %land.lhs.true4436
	movq	32(%r12), %rax
	movq	%rax, %rdi
	callq	real_onep
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_776
.LBB14_1202:                            # %if.end4453
	movq	optab_table+80(%rip), %rbx
	jmp	.LBB14_1203
.LBB14_767:                             # %if.end185.sw.bb4406_crit_edge8586
	movq	%r9, %r15
	movq	%rcx, %r14
	leaq	40(%r10), %rax
	movq	%r10, %rbx
	movq	(%rax), %rax
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	safe_from_p
	andq	%r14, %rax
	je	.LBB14_769
# BB#768:                               # %sw.bb4406
	movq	%r13, %rax
.LBB14_769:                             # %sw.bb4406
	movq	32(%rbx), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rsi, %rdi
	movq	%rax, %rsi
	callq	expand_expr
	movq	40(%rbx), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movl	188(%rsp), %ecx
	xorq	%rdi, %rdi
.LBB14_770:                             # %return
	movq	%rcx, (%rsp)
	movq	%r15, %rsi
	movq	96(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, %rcx
	movq	%rax, %r8
	movq	88(%rsp), %r9           # 8-byte Reload
	callq	expand_divmod
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_781:                             # %sw.bb4472
	movq	32(%r10), %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	%rax, %rbx
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	jne	.LBB14_783
# BB#782:                               # %if.then4479
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB14_783:                             # %if.end4481
	movl	188(%rsp), %eax
	movq	88(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	expand_fix
	movq	%r14, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_780:                             # %sw.bb4471
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$7833, %rsi             # imm = 0x1E99
	jmp	.LBB14_104
.LBB14_784:                             # %sw.bb4482
	movq	32(%r10), %rax
	movq	%r10, %r14
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	%rax, %rbx
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	jne	.LBB14_786
# BB#785:                               # %if.then4489
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB14_786:                             # %if.end4491
	movzbl	2(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB14_788
# BB#787:                               # %if.then4497
	movq	32(%r14), %rax
	movq	8(%rax), %rax
	movl	60(%rax), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %rbx
.LBB14_788:                             # %if.end4509
	movq	32(%r14), %rax
	movq	8(%rax), %rax
	movl	16(%rax), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movq	88(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	expand_float
	movq	%r14, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_789:                             # %sw.bb4520
	movq	%rcx, %r14
	movq	32(%r10), %rax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	expand_expr
	movl	188(%rsp), %ecx
	testq	%rcx, %rcx
	jne	.LBB14_793
# BB#790:                               # %sw.bb4520
	movl	flag_trapv(%rip), %ecx
	testq	%r14, %rcx
	je	.LBB14_793
# BB#791:                               # %land.lhs.true4528
	movq	96(%rsp), %rcx          # 8-byte Reload
	movq	%r14, %rdx
	andq	%rcx, %rdx
	movl	mode_class(,%rdx,4), %ecx
	cmpq	$1, %rcx
	jne	.LBB14_793
# BB#792:
	movabsq	$optab_table+264, %rcx
	jmp	.LBB14_794
.LBB14_801:                             # %sw.bb4572
	movq	%r9, %r13
	movq	%r10, %rbx
	testq	%r8, %r8
	movq	%r8, %r12
	je	.LBB14_807
# BB#802:                               # %lor.lhs.false4575
	movq	40(%rbx), %rax
	movq	$1, %rdx
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	safe_from_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_807
# BB#803:                               # %lor.lhs.false4581
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$134283263, %rcx        # imm = 0x800FFFF
	cmpq	$134217794, %rcx        # imm = 0x8000042
	je	.LBB14_807
# BB#804:                               # %lor.lhs.false4591
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movq	96(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	%rcx, %rdx
	jne	.LBB14_807
# BB#805:                               # %lor.lhs.false4597
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB14_808
# BB#806:                               # %land.lhs.true4602
	movl	8(%r12), %eax
	cmpq	$52, %rax
	ja	.LBB14_808
.LBB14_807:                             # %if.then4608
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, %r12
.LBB14_808:                             # %if.end4610
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	40(%rbx), %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	32(%rbx), %rsi
	movq	%rax, %r14
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rsi, %rdi
	movq	%r12, %rsi
	callq	expand_expr
	movq	104(%rsp), %rcx         # 8-byte Reload
	movzbl	17(%rcx), %ecx
	movq	%rax, %r15
	testq	$32, %rcx
	je	.LBB14_812
# BB#809:                               # %cond.true4625
	cmpq	$78, %r13
	je	.LBB14_810
# BB#811:                               # %cond.true4625
	movabsq	$optab_table+232, %rax
	jmp	.LBB14_815
.LBB14_796:                             # %sw.bb4542
	movq	32(%r10), %rax
	movq	%r10, %rbx
	xorq	%rdx, %rdx
	movq	%rcx, %r14
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	expand_expr
	movq	96(%rsp), %rcx          # 8-byte Reload
	andq	%r14, %rcx
	movl	mode_class(,%rcx,4), %ecx
	movq	%rax, %r15
	addq	$-5, %rcx
	andq	%r14, %rcx
	cmpq	$1, %rcx
	ja	.LBB14_798
# BB#797:                               # %if.then4556
	movl	188(%rsp), %eax
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	expand_complex_abs
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_834:                             # %sw.bb4710
	movq	32(%r10), %rax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	expand_expr
	movq	optab_table+296(%rip), %rcx
	movq	$1, %r8
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	88(%rsp), %rcx          # 8-byte Reload
	callq	expand_unop
	movq	%rax, %rsi
	testq	%rsi, %rsi
	jne	.LBB14_28
# BB#835:                               # %if.then4718
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$7950, %rsi             # imm = 0x1F0E
	jmp	.LBB14_104
.LBB14_839:                             # %if.end185.sw.bb4723_crit_edge8624
	movq	%r9, %r15
	movq	%rcx, %r14
	leaq	40(%r10), %rax
	movq	%r10, %rbx
	movq	(%rax), %rax
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	safe_from_p
	andq	%r14, %rax
	je	.LBB14_841
# BB#840:                               # %sw.bb4723
	movq	%r13, %rax
.LBB14_841:                             # %sw.bb4723
	movq	32(%rbx), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rsi, %rdi
	movq	%rax, %rsi
	callq	expand_expr
	movq	40(%rbx), %rcx
	movl	188(%rsp), %ebx
	movq	%r15, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	88(%rsp), %r8           # 8-byte Reload
	movq	%rbx, %r9
	jmp	.LBB14_842
.LBB14_837:                             # %sw.bb4721
	movq	%r10, %r12
	movq	optab_table+152(%rip), %rbx
	jmp	.LBB14_1203
.LBB14_838:                             # %sw.bb4722
	movq	%r10, %r12
	movq	optab_table+160(%rip), %rbx
	jmp	.LBB14_1203
.LBB14_836:                             # %sw.bb4720
	movq	%r10, %r12
	movq	optab_table+144(%rip), %rbx
	jmp	.LBB14_1203
.LBB14_832:                             # %sw.bb4700
	movq	32(%r10), %rax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	expand_expr
	movq	optab_table+288(%rip), %rcx
	movq	$1, %r8
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	88(%rsp), %rcx          # 8-byte Reload
	callq	expand_unop
	movq	%rax, %rsi
	testq	%rsi, %rsi
	jne	.LBB14_28
# BB#833:                               # %if.then4708
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$7943, %rsi             # imm = 0x1F07
	jmp	.LBB14_104
.LBB14_877:                             # %sw.bb4872
	movq	32(%r10), %rax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	88(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	callq	expand_expr
	movq	optab_table+160(%rip), %rcx
	movq	const_int_rtx+520(%rip), %rbx
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	movq	%r14, %r8
	callq	expand_binop
	movq	%rax, %rsi
	testq	%rsi, %rsi
	jne	.LBB14_28
# BB#878:                               # %if.then4880
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$8081, %rsi             # imm = 0x1F91
	jmp	.LBB14_104
.LBB14_533:                             # %sw.bb2613
	movq	%rbx, %r14
	movq	32(%r10), %r13
	movq	40(%r10), %rbx
	movq	8(%r13), %rax
	movq	8(%rbx), %rcx
	movq	24(%rax), %rax
	movl	16(%rcx), %r15d
	movq	104(%rax), %rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	112(%rax), %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	$13, %rcx
	shrq	%cl, %r15
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	callq	expand_expr
	movq	%rax, %r12
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	expand_expr
	movq	%rax, 72(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	expand_expr
	movq	%rax, 64(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	callq	expand_expr
	movq	8(%rbx), %rcx
	movl	60(%rcx), %r13d
	movq	104(%rsp), %rcx         # 8-byte Reload
	movzbl	16(%rcx), %edx
	movq	8(%rax), %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	$9, %rcx
	shrq	%cl, %r13
	cmpq	$25, %rdx
	jne	.LBB14_536
# BB#534:                               # %land.lhs.true2659
	movq	56(%rsp), %rax          # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB14_536
# BB#535:                               # %land.lhs.true2666
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	tree_int_cst_lt
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB14_542
.LBB14_536:                             # %lor.lhs.false2669
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	jne	.LBB14_539
# BB#537:                               # %land.lhs.true2676
	movq	56(%rsp), %rax          # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB14_539
# BB#538:                               # %land.lhs.true2683
	movq	%rbx, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	tree_int_cst_lt
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB14_542
.LBB14_539:                             # %lor.lhs.false2686
	movq	104(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB14_543
# BB#540:                               # %land.lhs.true2693
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	jne	.LBB14_543
# BB#541:                               # %land.lhs.true2700
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	tree_int_cst_lt
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_543
.LBB14_542:                             # %if.then2703
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_574:                             # %sw.bb2880
	movq	%rbx, %rdx
	movq	%rcx, %r13
	movq	32(%r10), %rax
	movq	%r10, %r12
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB14_28
# BB#575:                               # %if.end2887
	movq	104(%rsp), %rcx         # 8-byte Reload
	movzbl	16(%rcx), %ecx
	movq	8(%rax), %r15
	cmpq	$21, %rcx
	jne	.LBB14_589
# BB#576:                               # %if.then2894
	movq	96(%rsp), %rcx          # 8-byte Reload
	andq	%r13, %rcx
	cmpq	$51, %rcx
	movq	%rcx, %rbx
	jne	.LBB14_580
# BB#577:                               # %land.lhs.true2902
	movl	60(%r15), %ecx
	andq	$65024, %rcx            # imm = 0xFE00
	cmpq	$26112, %rcx            # imm = 0x6600
	jne	.LBB14_580
# BB#578:                               # %if.then2910
	movq	%rax, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	expand_expr
	movq	%rax, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	jmp	.LBB14_579
.LBB14_612:                             # %sw.bb3129
	movq	%rcx, %r15
	movq	32(%r10), %rax
	movq	%r10, %r14
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	movq	96(%rsp), %rdx          # 8-byte Reload
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	expand_expr
	movq	%rax, %rcx
	movq	104(%rsp), %rax         # 8-byte Reload
	movl	60(%rax), %eax
	movl	(%rcx), %edx
	movq	%rcx, %r12
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	movq	$16, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	cmpq	%rsi, %rax
	je	.LBB14_626
# BB#613:                               # %if.else3145
	movq	%rax, %rcx
	andq	%r15, %rcx
	cmpq	$51, %rcx
	je	.LBB14_618
# BB#614:                               # %if.else3145
	cmpq	$51, %rsi
	je	.LBB14_618
# BB#615:                               # %land.lhs.true3159
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rcx), %ebx
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	addq	$4, %rdi
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rdi, %rcx
	cmpq	%rcx, %rbx
	ja	.LBB14_618
# BB#616:                               # %land.lhs.true3173
	movzbl	mode_size(%rsi), %ecx
	andq	%r15, %rdi
	cmpq	%rdi, %rcx
	jbe	.LBB14_617
.LBB14_618:                             # %if.else3192
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$66, %rdx
	je	.LBB14_626
# BB#619:                               # %if.then3197
	movzbl	17(%r14), %eax
	testq	$4, %rax
	jne	.LBB14_620
# BB#621:                               # %if.end3211
	movq	32(%r14), %rax
	movq	8(%rax), %rax
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB14_622
# BB#623:                               # %lor.lhs.false3214
	movl	60(%rax), %esi
	movq	88(%rsp), %rcx          # 8-byte Reload
	movzbl	2(%rcx), %edx
	movq	$9, %rcx
	movq	%rsi, %rdi
	shrq	%cl, %rdi
	andq	$127, %rdi
	cmpq	%rdi, %rdx
	je	.LBB14_625
	jmp	.LBB14_624
.LBB14_223:                             # %sw.bb946
	movq	%rcx, %r12
	movq	%r10, %rdi
	movq	%r10, %r15
	callq	decl_function_context
	testq	%rax, %rax
	jne	.LBB14_225
# BB#224:                               # %if.then950
	movq	current_function_decl(%rip), %rcx
	movq	%rcx, 40(%r15)
.LBB14_225:                             # %if.end954
	movq	current_function_decl(%rip), %rcx
	cmpq	%rcx, %rax
	movabsq	$0, %rcx
	movabsq	$1, %rsi
	movq	%rcx, %rdx
	jne	.LBB14_227
# BB#226:                               # %if.end954
	movq	%rsi, %rdx
.LBB14_227:                             # %if.end954
	movq	inline_function_decl(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB14_229
# BB#228:                               # %if.end954
	movq	%rsi, %rcx
.LBB14_229:                             # %if.end954
	xorq	%rsi, %rsi
	orq	%rcx, %rdx
	jne	.LBB14_231
# BB#230:                               # %if.end954
	movq	%rax, %rsi
.LBB14_231:                             # %if.end954
	testq	%rsi, %rsi
	je	.LBB14_239
# BB#232:                               # %if.then963
	movq	%rsi, %rdi
	callq	find_function_data
	movq	48(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB14_237
# BB#233:                               # %land.lhs.true969
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB14_236
# BB#234:                               # %if.end978
	movq	%r15, %r14
	movq	%r14, %rdi
	callq	put_var_into_stack
	movq	48(%r14), %rbx
	movq	%r14, %r15
	testq	%rbx, %rbx
	je	.LBB14_237
# BB#235:                               # %if.end978.lor.lhs.false981_crit_edge
	movl	(%rbx), %eax
.LBB14_236:                             # %lor.lhs.false981
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB14_237
# BB#238:                               # %if.end987
	movq	8(%rbx), %rax
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	fix_lexical_addr
	movq	%rbx, %rdi
	jmp	.LBB14_218
.LBB14_260:                             # %sw.bb1127
	movq	32(%r10), %rax
	movq	%rax, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	%r10, %rbx
	callq	expand_expr
	movq	32(%rbx), %rcx
	movq	%rax, %r14
	movq	%rcx, %rdi
	callq	unsave_expr_now
	movq	%r14, %rsi
	movq	%rax, 32(%rbx)
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_288:                             # %sw.bb1257
	movq	32(%r10), %rax
	movq	%r10, %rbx
	testq	%rax, %rax
	je	.LBB14_292
# BB#289:                               # %if.then1262
	cmpq	%rsi, %rax
	je	.LBB14_290
# BB#291:                               # %if.end1269
	movq	%rax, %rdi
	callq	emit_insns
	movq	const_int_rtx+512(%rip), %rax
	movq	%rax, 32(%rbx)
.LBB14_292:                             # %if.end1277
	movq	40(%rbx), %rax
	movq	%rax, %rdi
	callq	preserve_rtl_expr_result
	movq	%rbx, %rdi
	callq	free_temps_for_rtl_expr
	movq	40(%rbx), %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_1124:                            # %sw.bb6073
	movq	%rcx, %r13
	movq	32(%r10), %rcx
	movq	%r10, %rbx
	movzbl	16(%rcx), %eax
	cmpq	$30, %rax
	jne	.LBB14_1129
# BB#1125:                              # %land.lhs.true6083
	movq	%rcx, %rdi
	callq	decl_function_context
	movq	32(%rbx), %rcx
	testq	%rax, %rax
	je	.LBB14_1129
# BB#1126:                              # %land.lhs.true6090
	movzbl	49(%rcx), %eax
	testq	$4, %rax
	jne	.LBB14_1129
# BB#1127:                              # %land.lhs.true6101
	movzbl	18(%rbx), %eax
	testq	$4, %rax
	je	.LBB14_1128
.LBB14_1129:                            # %if.else6114
	movzbl	16(%rcx), %eax
	testq	%rax, %rax
	je	.LBB14_1130
# BB#1131:                              # %if.else6125
	cmpq	$0, cfun(%rip)
	jne	.LBB14_1135
# BB#1132:                              # %land.lhs.true6128
	cmpq	$46, %rax
	je	.LBB14_1134
# BB#1133:                              # %lor.lhs.false6138
	movzbl	tree_code_type(%rax), %eax
	cmpq	$99, %rax
	jne	.LBB14_1135
.LBB14_1134:                            # %if.then6151
	xorq	%rsi, %rsi
	movq	%rcx, %rdi
	callq	output_constant_def
	movq	8(%rax), %rdx
	jmp	.LBB14_1166
.LBB14_1175:                            # %sw.bb6356
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$8704, %rsi             # imm = 0x2200
	jmp	.LBB14_104
.LBB14_1200:                            # %sw.bb6521
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$8853, %rsi             # imm = 0x2295
	jmp	.LBB14_104
.LBB14_1176:                            # %sw.bb6357
	movq	8(%r10), %rax
	movq	8(%rax), %rax
	movl	60(%rax), %r15d
	movq	32(%r10), %rax
	movq	$9, %rcx
	shrq	%cl, %r15
	andq	$127, %r15
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	%r10, %rbx
	callq	expand_expr
	movq	40(%rbx), %rdi
	movq	%rbx, %r12
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%rax, %r14
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	jne	.LBB14_1178
# BB#1177:                              # %if.then6377
	movq	8(%r12), %rax
	movl	60(%rax), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	movq	%rax, %rdi
	callq	gen_reg_rtx
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB14_1178:                            # %if.end6386
	callq	start_sequence
	movq	%r15, %rdi
	movq	88(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rsi
	callq	gen_realpart
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	gen_imagpart
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	emit_move_insn
	callq	get_insns
	movq	%rax, %r15
	callq	end_sequence
	movzbl	(%r12), %eax
	cmpq	$65, %rax
	jne	.LBB14_1179
# BB#1180:                              # %if.else6398
	movq	%r15, %rdi
	callq	emit_insns
	movq	%r12, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_1183:                            # %sw.bb6413
	movq	%rcx, %r12
	movq	8(%r10), %rax
	movq	8(%rax), %rax
	movl	60(%rax), %r14d
	movq	32(%r10), %rax
	movq	$9, %rcx
	shrq	%cl, %r14
	andq	$127, %r14
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	%rax, %rbx
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	jne	.LBB14_1185
# BB#1184:                              # %if.then6429
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB14_1185:                            # %if.end6431
	callq	start_sequence
	movq	%r14, %rdi
	movq	88(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rsi
	callq	gen_realpart
	movq	%rax, %r15
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	gen_realpart
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	gen_imagpart
	movl	188(%rsp), %ecx
	movq	%rax, %r15
	testq	%rcx, %rcx
	jne	.LBB14_1189
# BB#1186:                              # %if.end6431
	movl	flag_trapv(%rip), %eax
	testq	%r12, %rax
	je	.LBB14_1189
# BB#1187:                              # %land.lhs.true6439
	movq	%r12, %rax
	andq	%r14, %rax
	movl	mode_class(,%rax,4), %eax
	cmpq	$1, %rax
	jne	.LBB14_1189
# BB#1188:
	movabsq	$optab_table+264, %rax
	jmp	.LBB14_1190
.LBB14_1181:                            # %sw.bb6401
	movq	32(%r10), %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	gen_realpart
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_1182:                            # %sw.bb6407
	movq	32(%r10), %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	gen_imagpart
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_1121:                            # %sw.bb6066
	xorq	%rsi, %rsi
	movq	%r10, %rdi
	jmp	.LBB14_1122
.LBB14_1123:                            # %sw.bb6068
	movq	%r15, %rax
	xorq	$1, %rax
	movq	%r10, %rdi
	movq	%rax, %rsi
.LBB14_1122:                            # %return
	movq	%r15, %rdx
	callq	expand_increment
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_1198:                            # %sw.bb6514
	movq	32(%r10), %rax
	movq	%rax, %rdi
	movq	104(%rsp), %rsi         # 8-byte Reload
	callq	expand_builtin_va_arg
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_1195:                            # %sw.bb6465
	movq	40(%r10), %r14
	movq	%r10, %rbx
	callq	expand_eh_region_start
	movq	32(%rbx), %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	expand_eh_region_end_cleanup
	movq	%rbx, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_1196:                            # %sw.bb6473
	movq	32(%r10), %r14
	movq	40(%r10), %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	callq	gen_label_rtx
	movq	%rax, %r12
	callq	gen_label_rtx
	movl	target_flags(%rip), %edx
	movq	%rax, %r15
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	orq	$4, %rdx
	movq	%rdx, %rdi
	callq	gen_reg_rtx
	movq	global_trees+216(%rip), %rcx
	movq	%rax, %r13
	movq	$136, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%r12, %rdx
	movq	%r13, %rcx
	callq	build
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	96(%rsp), %rax          # 8-byte Reload
	movl	16(%rax), %eax
	orq	$256, %rax              # imm = 0x100
	movq	96(%rsp), %rcx          # 8-byte Reload
	movl	%eax, 16(%rcx)
	movq	$2, %rdi
	xorq	%rsi, %rsi
	callq	expand_start_bindings_and_block
	movq	cfun(%rip), %rax
	movl	328(%rax), %ecx
	movl	%ecx, 336(%rax)
	xorq	%rdi, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	callq	expand_decl_cleanup
	movq	%r14, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	expand_expr
	movq	%rax, %r14
	movq	%r14, %rdi
	callq	preserve_temp_slots
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	expand_end_bindings
	movq	%r15, %rdi
	callq	emit_jump
	movq	%r12, %rdi
	callq	emit_label
	movq	const_int_rtx+512(%rip), %rax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	expand_expr
	movq	%r13, %rdi
	callq	emit_indirect_jump
	movq	%r15, %rdi
	callq	emit_label
	movq	%r14, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_1197:                            # %sw.bb6499
	movq	32(%r10), %r12
	movq	40(%r10), %r14
	callq	gen_label_rtx
	movl	target_flags(%rip), %edx
	movq	%rax, %r15
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	orq	$4, %rdx
	movq	$67, %rdi
	movq	%rdx, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_u00
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	%r12, %rdi
	callq	emit_jump
	jmp	.LBB14_1114
.LBB14_265:                             # %sw.bb1159
	movq	32(%r10), %rax
	movzbl	16(%rax), %ecx
	cmpq	$31, %rcx
	je	.LBB14_266
# BB#267:                               # %if.else1173
	movq	%rax, %rdi
	callq	expand_computed_goto
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_1118:                            # %sw.bb6055
	movq	32(%r10), %rax
	testq	%rax, %rax
	je	.LBB14_1119
# BB#1120:                              # %if.else6061
	movq	%rax, %rdi
	callq	expand_return
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_268:                             # %sw.bb1178
	movq	32(%r10), %rax
	movq	%rax, %rdi
	callq	invert_truthvalue
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	expand_exit_loop_if_false
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_279:                             # %sw.bb1212
	movq	%r10, %rbx
	callq	push_temp_slots
	movq	$1, %rdi
	callq	expand_start_loop
	movq	32(%rbx), %rax
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	callq	expand_expr_stmt_value
	callq	expand_end_loop
	callq	pop_temp_slots
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_269:                             # %sw.bb1184
	movq	40(%r10), %rax
	movq	%r10, %rbx
	testq	%rax, %rax
	je	.LBB14_271
# BB#270:                               # %if.then1189
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	callq	expand_expr_stmt_value
.LBB14_271:                             # %if.end1193
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB14_275
# BB#272:                               # %if.then.i8437
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB14_274
# BB#273:                               # %if.then3.i8443
	xorq	%rdi, %rdi
	movq	%rcx, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	adjust_stack
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
.LBB14_274:                             # %if.end.i8445
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.LBB14_275:                             # %do_pending_stack_adjust.exit8446
	movq	32(%rbx), %rax
	movq	%rax, %rdi
	callq	label_rtx
	movq	%rax, %rdi
	jmp	.LBB14_1115
.LBB14_276:                             # %sw.bb1199
	cmpq	$0, 40(%r10)
	movq	%r10, %rbx
	je	.LBB14_278
# BB#277:                               # %if.then1204
	movabsq	$.L.str5, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	sorry
.LBB14_278:                             # %if.end1205
	movq	32(%rbx), %rax
	movq	32(%rax), %rax
.LBB14_266:                             # %if.then1169
	movq	%rax, %rdi
	callq	expand_goto
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_220:                             # %sw.bb929
	movq	%rbx, %r14
	movq	40(%r10), %rax
	movq	32(%rax), %rax
	movq	input_filename(%rip), %r12
	movq	%rax, input_filename(%rip)
	movslq	24(%r10), %rdx
	movl	lineno(%rip), %r15d
	movq	$12, %rcx
	sarq	%cl, %rdx
	movl	%edx, lineno(%rip)
	movzbl	18(%r10), %ecx
	movq	%r10, %rbx
	testq	$8, %rcx
	je	.LBB14_222
# BB#221:                               # %if.then939
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	emit_line_note
.LBB14_222:                             # %if.end941
	movq	32(%rbx), %rax
	movq	%rax, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	expand_expr
	movq	%rax, %rsi
	movq	%r12, input_filename(%rip)
	movl	%r15d, lineno(%rip)
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_1199:                            # %sw.bb6519
	movq	cfun(%rip), %rax
	movq	%rax, %rdi
	callq	get_exception_pointer
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_861:                             # %sw.bb4829
	movq	%rcx, %r14
	movq	%r10, %r15
	testq	%r12, %r12
	jne	.LBB14_870
# BB#862:                               # %land.lhs.true4831
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB14_867
# BB#863:                               # %lor.lhs.false4834
	movq	$1, %rdx
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	safe_from_p
	testq	%r14, %rax
	je	.LBB14_867
# BB#864:                               # %lor.lhs.false4837
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	jne	.LBB14_870
# BB#865:                               # %land.lhs.true4839
	movq	88(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB14_871
# BB#866:                               # %land.lhs.true4844
	movq	88(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %eax
	cmpq	$52, %rax
	ja	.LBB14_870
.LBB14_867:                             # %if.then4850
	testq	%r14, %rbx
	jne	.LBB14_869
# BB#868:                               # %if.then4850
	movq	96(%rsp), %rbx          # 8-byte Reload
.LBB14_869:                             # %if.then4850
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB14_870:                             # %if.end4858
	xorq	%rbx, %rbx
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	movabsq	$0, %r13
	je	.LBB14_872
.LBB14_871:                             # %if.then4860
	movq	88(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	callq	emit_clr_insn
	movq	$1, %rbx
	movq	%r14, %r13
.LBB14_872:                             # %if.end4861
	callq	gen_label_rtx
	movq	%rax, %r14
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	do_jump
	testq	%rbx, %rbx
	je	.LBB14_874
# BB#873:                               # %if.then4864
	movq	%r13, %rdi
	callq	emit_0_to_1_insn
.LBB14_874:                             # %if.end4865
	movq	%r14, %rdi
	callq	emit_label
	testq	%r12, %r12
	je	.LBB14_876
# BB#875:
	movq	const_int_rtx+512(%rip), %r13
.LBB14_876:                             # %if.end4865
	movq	%r13, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_856:                             # %if.end4802
	cmpq	%rbx, %rax
	je	.LBB14_860
# BB#857:                               # %if.then4805
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	je	.LBB14_859
# BB#858:                               # %if.then4805
	movq	%rdx, %r15
.LBB14_859:                             # %if.then4805
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %rbx
.LBB14_860:                             # %if.end4821
	callq	gen_label_rtx
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rbx, %r15
	movzbl	2(%r15), %ebx
	movl	188(%rsp), %r9d
	movq	%rax, %r14
	movq	$103, %rdx
	xorq	%rcx, %rcx
	movq	%r14, (%rsp)
	movq	%r15, %rdi
	movq	%rbx, %r8
	callq	emit_cmp_and_jump_insns
	movq	const_int_rtx+520(%rip), %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	%r14, %rdi
	callq	emit_label
	movq	%r15, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_855:                             # %if.else4799
	movq	const_int_rtx+512(%rip), %rax
.LBB14_854:                             # %return
	movq	88(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	%rbx, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_203:
	movq	%rdx, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_205:
	movq	%rdx, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_214:                             # %land.lhs.true896.if.then916_crit_edge
	movq	24(%rbx), %rcx
	movq	8(%rcx), %rax
	movq	%rcx, %r15
.LBB14_217:                             # %if.then916
	movq	%rax, %rdi
	callq	copy_rtx
	movq	%r15, %rdi
.LBB14_218:                             # %return
	movq	%rax, %rsi
	callq	replace_equiv_address
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_84:                              # %if.else201
	movq	112(%rsp), %rax         # 8-byte Reload
	andq	%r14, %rax
	cmpq	$3, %rax
	jne	.LBB14_87
# BB#85:                                # %if.then204
	movq	%r15, %rdi
	callq	label_rtx
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	32(%rcx), %rcx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rcx
.LBB14_86:                              # %if.end212
	movq	%rax, 32(%rcx)
.LBB14_87:                              # %if.end212
	movl	target_flags(%rip), %r14d
	movq	$25, %rcx
	shrq	%cl, %r14
	andq	$1, %r14
	orq	$4, %r14
	movq	%r15, %rdi
	callq	label_rtx
	movq	$67, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_u00
	movq	$2, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	current_function_decl(%rip), %rcx
	movq	%rax, %rdx
	cmpq	%rcx, %rbx
	je	.LBB14_88
# BB#89:                                # %land.lhs.true221
	movq	inline_function_decl(%rip), %rax
	movq	%rdx, %rsi
	cmpq	%rax, %rbx
	je	.LBB14_28
# BB#90:                                # %land.lhs.true221
	testq	%rbx, %rbx
	je	.LBB14_28
# BB#91:                                # %if.then227
	movq	8(%rsi), %rax
	movl	(%rax), %ecx
	orq	$134217728, %rcx        # imm = 0x8000000
	movl	%ecx, (%rax)
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_94:                              # %if.then242
	movq	%rcx, %r14
	movabsq	$.L.str4, %rcx
	xorq	%rax, %rax
	movq	%r10, %rdi
	movq	%rcx, %rsi
	callq	error_with_decl
	movq	96(%rsp), %rax          # 8-byte Reload
	andq	%r14, %rax
	movq	const_tiny_rtx(,%rax,8), %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_103:                             # %if.then320
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$6239, %rsi             # imm = 0x185F
	jmp	.LBB14_104
.LBB14_137:                             # %if.then511
	orq	$512, 48(%rbx)          # imm = 0x200
	movq	current_function_decl(%rip), %rax
	movzbl	49(%rax), %eax
	testq	$4, %rax
	jne	.LBB14_138
# BB#139:                               # %if.end523
	movq	%rbx, %rdi
	callq	mark_addressable
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB14_141
# BB#140:                               # %cond.false531
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB14_141:                             # %cond.end534
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB14_142
# BB#143:                               # %if.end541
	testq	%rax, %rax
	jne	.LBB14_145
# BB#144:                               # %cond.false548
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB14_145:                             # %cond.end551
	movq	8(%rax), %r14
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	jne	.LBB14_147
# BB#146:                               # %if.then560
	movq	8(%r14), %rax
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	fix_lexical_addr
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	replace_equiv_address
	jmp	.LBB14_148
.LBB14_160:                             # %if.else629
	testq	%rcx, %rcx
	jne	.LBB14_162
# BB#161:                               # %cond.false636
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
.LBB14_162:                             # %cond.end639
	movzbl	(%rcx), %eax
	cmpq	$66, %rax
	jne	.LBB14_183
# BB#163:                               # %cond.end639
	movq	112(%rsp), %rax         # 8-byte Reload
	decq	%rax
	andq	%r15, %rax
	cmpq	$3, %rax
	jb	.LBB14_183
# BB#164:                               # %land.lhs.true654
	movzbl	48(%rbx), %r14d
	testq	%rcx, %rcx
	jne	.LBB14_166
# BB#165:                               # %cond.false666
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
.LBB14_166:                             # %cond.end669
	movq	8(%rcx), %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	memory_address_p
	testq	%r15, %rax
	je	.LBB14_167
# BB#168:                               # %lor.lhs.false676
	movl	flag_force_addr(%rip), %eax
	testq	%rax, %rax
	je	.LBB14_183
# BB#169:                               # %land.lhs.true678
	movq	144(%rbx), %r14
	testq	%r14, %r14
	jne	.LBB14_171
# BB#170:                               # %cond.false685
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %r14
.LBB14_171:                             # %cond.end688
	movq	8(%r14), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	je	.LBB14_183
	jmp	.LBB14_172
.LBB14_324:                             # %if.end1473
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%r14, %rdi
	callq	expand_expr
	movq	96(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	memory_address
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%rax, %r14
	movq	%rbx, %rsi
	callq	set_mem_attributes
	movq	112(%rsp), %rax         # 8-byte Reload
	andq	%r13, %rax
	cmpq	$4, %rax
	jne	.LBB14_325
# BB#326:                               # %land.lhs.true1479
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	readonly_fields_p
	movq	%r14, %rsi
	testq	%r13, %rax
	je	.LBB14_28
# BB#327:                               # %if.then1482
	movl	(%rsi), %eax
	orq	$67108864, %rax         # imm = 0x4000000
	movl	%eax, (%rsi)
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_329:                             # %if.then1499
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$6703, %rsi             # imm = 0x1A2F
	jmp	.LBB14_104
.LBB14_331:
	movabsq	$global_trees+88, %rax
.LBB14_333:                             # %cond.end1511
	movq	(%rax), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	sizetype_tab(%rip), %rax
	movq	40(%rbx), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	convert
	movq	%rax, %r15
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB14_335
# BB#334:                               # %if.then1520
	movq	sizetype_tab(%rip), %rax
	movq	%rax, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	convert
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	size_diffop
	movq	%rax, %r15
.LBB14_335:                             # %if.end1523
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	movq	112(%rsp), %rax         # 8-byte Reload
	andq	%rax, %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	cmpq	$2, %rcx
	movq	%rbx, %r10
	movq	%r14, %rbx
	movq	%r13, %r9
	je	.LBB14_367
# BB#336:                               # %land.lhs.true1529
	movq	%r9, %r13
	movq	%rbx, %r14
	movq	%r10, 72(%rsp)          # 8-byte Spill
	movl	16(%r12), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$29, %rcx
	jne	.LBB14_343
# BB#337:                               # %land.lhs.true1536
	movzbl	16(%r15), %eax
	cmpq	$25, %rax
	jne	.LBB14_342
# BB#338:                               # %land.lhs.true1543
	movslq	32(%r12), %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB14_342
# BB#339:                               # %land.lhs.true1550
	movq	96(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movl	mode_class(,%rax,4), %ecx
	cmpq	$1, %rcx
	jne	.LBB14_342
# BB#340:                               # %land.lhs.true1555
	movzbl	mode_size(%rax), %eax
	cmpq	$1, %rax
	jne	.LBB14_342
# BB#341:                               # %if.then1561
	movq	32(%r15), %rax
	movq	40(%r12), %rcx
.LBB14_322:                             # %return
	movsbq	(%rcx,%rax), %rax
	movq	%rax, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB14_323
.LBB14_528:                             # %if.else2564
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r14
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	emit_move_insn
	movq	%rax, %rbx
	movq	%r14, %r13
.LBB14_529:                             # %if.end2570
	movq	40(%r12), %rax
	movq	144(%rax), %rcx
	testq	%rcx, %rcx
	jne	.LBB14_531
# BB#530:                               # %cond.false2583
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	make_decl_rtl
	movq	40(%r12), %rax
	movq	144(%rax), %rcx
.LBB14_531:                             # %cond.end2592
	movq	8(%rcx), %r14
	movq	48(%r12), %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	plus_constant_wide
	movq	8(%rax), %rax
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$30, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
	movq	%r13, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_525:                             # %if.then2552
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$7128, %rsi             # imm = 0x1BD8
	jmp	.LBB14_104
.LBB14_884:                             # %if.end.i8402
                                        #   in Loop: Header=BB14_880 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rbx), %rcx
	movq	16(%rax), %rax
	movq	%rcx, 40(%rax)
.LBB14_880:                             # %sw.bb4882
                                        # =>This Inner Loop Header: Depth=1
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB14_885
# BB#881:                               # %while.body.i8385
                                        #   in Loop: Header=BB14_880 Depth=1
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$24, %rcx
	jne	.LBB14_883
# BB#882:                               # %if.then.i8391
                                        #   in Loop: Header=BB14_880 Depth=1
	movq	8(%rax), %rcx
	movq	8(%rcx), %rcx
	movq	%rcx, 16(%rbx)
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB14_884
.LBB14_883:                             # %if.else.i8394
                                        #   in Loop: Header=BB14_880 Depth=1
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rax, 16(%rbx)
	jmp	.LBB14_884
.LBB14_1116:                            # %if.end6048
	xorq	$1, %r15
	movq	%rbx, %rdi
	movq	%r14, %rsi
.LBB14_1117:                            # %return
	movq	%r15, %rdx
	callq	expand_assignment
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_1108:
	movabsq	$global_trees+96, %rcx
.LBB14_1110:                            # %if.then6008
	movq	(%rcx), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	convert
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	expand_assignment
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB14_1114
# BB#1111:                              # %if.then.i
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB14_1113
# BB#1112:                              # %if.then3.i
	xorq	%rdi, %rdi
	movq	%rcx, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	adjust_stack
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
.LBB14_1113:                            # %if.end.i
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.LBB14_1114:                            # %do_pending_stack_adjust.exit
	movq	%r15, %rdi
.LBB14_1115:                            # %return
	callq	emit_label
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_1078:                            # %if.then5819
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$8399, %rsi             # imm = 0x20CF
	jmp	.LBB14_104
.LBB14_1082:                            # %if.then5831
	testq	%rcx, %rcx
	je	.LBB14_1084
.LBB14_1083:                            # %cond.end5846
	xorq	%rbx, %rbx
	cmpq	$0, 40(%r15)
	movq	%rcx, %rsi
	jne	.LBB14_1093
	jmp	.LBB14_28
.LBB14_900:                             # %if.end5021
	movq	40(%rbx), %rax
	movq	%rax, %rdi
	callq	integer_onep
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_909
# BB#901:                               # %land.lhs.true5027
	movq	48(%rbx), %rax
	movq	%rax, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_909
# BB#902:                               # %land.lhs.true5033
	movq	32(%rbx), %rax
	movzbl	16(%rax), %ecx
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$60, %rcx
	jne	.LBB14_909
# BB#903:                               # %if.then5046
	testq	%r12, %r12
	je	.LBB14_905
# BB#904:                               # %if.then5048
	movq	const_int_rtx+512(%rip), %rcx
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	movq	112(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB14_40
.LBB14_286:                             # %if.end1247
                                        #   in Loop: Header=BB14_283 Depth=1
	movq	%r15, %rdi
	callq	expand_decl_init
	movq	(%r15), %r15
.LBB14_283:                             # %while.cond.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%r15, %r15
	je	.LBB14_287
# BB#284:                               # %while.body
                                        #   in Loop: Header=BB14_283 Depth=1
	cmpq	$0, 144(%r15)
	jne	.LBB14_286
# BB#285:                               # %if.then1246
                                        #   in Loop: Header=BB14_283 Depth=1
	movq	%r15, %rdi
	callq	expand_decl
	jmp	.LBB14_286
.LBB14_570:                             # %land.lhs.true2845
	andq	%rsi, %rax
	movq	%rdx, %rdi
	cmpq	$1, %rax
	je	.LBB14_571
# BB#572:                               # %if.else2876
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%r13, %rdx
	movq	%rbx, %rcx
	movq	%r15, %r8
	callq	expand_builtin
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_262:                             # %if.then1144
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$6510, %rsi             # imm = 0x196E
	jmp	.LBB14_104
.LBB14_719:                             # %if.end3982
	movq	40(%r12), %rax
	movzbl	16(%rax), %ecx
	cmpq	$25, %rcx
	jne	.LBB14_723
# BB#720:                               # %if.then3992
	movq	$77, %rdi
	movq	104(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movzbl	17(%rbx), %edx
	movq	%rbx, %rsi
	movq	%rax, %rcx
	testq	$32, %rdx
	jne	.LBB14_723
# BB#721:                               # %lor.lhs.false4004
	movzbl	18(%rcx), %eax
	testq	$8, %rax
	je	.LBB14_722
.LBB14_723:                             # %if.end4018
	movl	188(%rsp), %eax
	testq	%rax, %rax
	jne	.LBB14_727
# BB#724:                               # %if.end4018
	movl	flag_trapv(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_727
# BB#725:                               # %land.lhs.true4022
	movq	96(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movl	mode_class(,%rax,4), %eax
	cmpq	$1, %rax
	jne	.LBB14_727
# BB#726:
	movabsq	$optab_table+24, %rax
	movq	(%rax), %rbx
	jmp	.LBB14_1203
.LBB14_716:                             # %if.else3980
	movq	$76, %rdi
.LBB14_717:                             # %return
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%rax, %rcx
	jmp	.LBB14_718
.LBB14_739:                             # %if.end4091
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rax
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	safe_from_p
	movq	%rax, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r14
	je	.LBB14_741
# BB#740:                               # %if.end4091
	movq	%r13, %r14
.LBB14_741:                             # %if.end4091
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$115, %rcx
	jne	.LBB14_750
# BB#742:                               # %land.lhs.true4108
	movq	104(%rsp), %rcx         # 8-byte Reload
	movzbl	16(%rcx), %ecx
	cmpq	$6, %rcx
	jne	.LBB14_750
# BB#743:                               # %land.lhs.true4115
	movq	32(%rax), %rcx
	movq	8(%rax), %rdx
	movq	8(%rcx), %rcx
	movl	60(%rcx), %esi
	movl	60(%rdx), %edx
	andq	$511, %rsi              # imm = 0x1FF
	andq	$511, %rdx              # imm = 0x1FF
	cmpq	%rdx, %rsi
	jae	.LBB14_750
# BB#744:                               # %land.lhs.true4138
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rax
	movzbl	16(%rax), %edx
	cmpq	$25, %rdx
	jne	.LBB14_748
# BB#745:                               # %land.lhs.true4148
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	int_fits_type_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_748
# BB#746:                               # %land.lhs.true4162
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rax
	movq	8(%rax), %rcx
	movl	60(%rcx), %edx
	movq	$8, %rcx
	shrq	%cl, %rdx
	andq	$254, %rdx
	movzbl	mode_bitsize(%rdx), %ecx
	cmpq	$64, %rcx
	ja	.LBB14_753
# BB#747:                               # %lor.lhs.false4178
	movq	32(%rax), %rax
	movq	%rax, %rdi
	callq	exact_log2_wide
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB14_753
.LBB14_748:                             # %lor.lhs.false4188
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rcx
	movzbl	16(%rcx), %eax
	cmpq	$115, %rax
	jne	.LBB14_749
# BB#751:                               # %land.lhs.true4198
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	32(%rcx), %rcx
	movq	32(%rax), %rsi
	movq	8(%rcx), %rdx
	movq	8(%rsi), %rcx
	movl	60(%rdx), %esi
	movl	60(%rcx), %edi
	xorq	%rsi, %rdi
	testq	$511, %rdi              # imm = 0x1FF
	jne	.LBB14_750
# BB#752:                               # %land.lhs.true4225
	movl	16(%rdx), %edx
	movl	16(%rcx), %ecx
	xorq	%rdx, %rcx
	testq	$8192, %rcx             # imm = 0x2000
	jne	.LBB14_750
.LBB14_753:                             # %if.then4254
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	32(%rax), %rdi
	movq	8(%rdi), %rdx
	movl	60(%rdx), %r15d
	movq	$9, %rcx
	shrq	%cl, %r15
	movzbl	17(%rdx), %edx
	andq	$32, %rdx
	movq	$5, %rcx
	shrq	%cl, %rdx
	movq	optab_table+64(%rip), %rcx
	movq	optab_table+72(%rip), %rbx
	andq	$127, %r15
	testq	%rdx, %rdx
	movq	%rcx, %r12
	jne	.LBB14_755
# BB#754:                               # %if.then4254
	movq	%rbx, %r12
.LBB14_755:                             # %if.then4254
	jne	.LBB14_757
# BB#756:                               # %if.then4254
	movq	%rcx, %rbx
.LBB14_757:                             # %if.then4254
	movzbl	mode_wider_mode(%r15), %ecx
	movq	96(%rsp), %rdx          # 8-byte Reload
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	%rcx, %rdx
	jne	.LBB14_750
# BB#758:                               # %if.then4309
	movq	$4, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movl	8(%rbx,%rsi), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	jne	.LBB14_759
# BB#761:                               # %if.else4345
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	8(%r12,%rdx), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	je	.LBB14_750
# BB#762:                               # %if.else4345
	movl	word_mode(%rip), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	andq	%r15, %rdx
	cmpq	%rcx, %rdx
	jne	.LBB14_750
# BB#763:                               # %if.then4355
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	40(%rcx), %rdi
	movzbl	16(%rdi), %ecx
	movq	%rax, %rbx
	cmpq	$25, %rcx
	jne	.LBB14_765
# BB#764:                               # %if.then4372
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movl	188(%rsp), %ecx
	movq	%r15, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	convert_modes
	jmp	.LBB14_766
.LBB14_776:                             # %if.then4442
	movq	%r12, %r15
	movq	32(%r15), %rbx
	movq	dconst1+16(%rip), %rax
	movq	%rax, 16(%rsp)
	movq	dconst1+8(%rip), %rax
	movq	%rax, 8(%rsp)
	movq	dconst1(%rip), %rax
	movq	%rax, (%rsp)
	movq	104(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdi
	callq	build_real
	movq	40(%r15), %rcx
	movq	%rax, %rdx
	movq	$70, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	build
	movq	%rax, %rcx
	movq	$61, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	build
	movl	188(%rsp), %ecx
	movq	%rax, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	80(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB14_383
.LBB14_793:                             # %cond.false4534
	movabsq	$optab_table+256, %rcx
.LBB14_794:                             # %cond.end4535
	movq	(%rcx), %rcx
	xorq	%r8, %r8
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	88(%rsp), %rcx          # 8-byte Reload
	callq	expand_unop
	movq	%rax, %rsi
	testq	%rsi, %rsi
	jne	.LBB14_28
# BB#795:                               # %if.then4540
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$7862, %rsi             # imm = 0x1EB6
	jmp	.LBB14_104
.LBB14_812:                             # %cond.false4632
	cmpq	$78, %r13
	je	.LBB14_813
# BB#814:                               # %cond.false4632
	movabsq	$optab_table+216, %rax
	jmp	.LBB14_815
.LBB14_810:
	movabsq	$optab_table+224, %rax
	jmp	.LBB14_815
.LBB14_798:                             # %if.end4558
	movq	104(%rsp), %rax         # 8-byte Reload
	movzbl	17(%rax), %eax
	testq	$32, %rax
	je	.LBB14_800
# BB#799:
	movq	%r15, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_543:                             # %if.end2704
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	jne	.LBB14_547
# BB#544:                               # %if.then2707
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	jne	.LBB14_546
# BB#545:                               # %if.then2707
	movq	96(%rsp), %r14          # 8-byte Reload
.LBB14_546:                             # %if.then2707
	movq	%r14, %rdi
	callq	gen_reg_rtx
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB14_547:                             # %if.end2715
	andq	$1, %r15
	andq	$127, %r13
	callq	gen_label_rtx
	movq	%rax, %r14
	callq	gen_label_rtx
	movl	(%r12), %r8d
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$54, %rax
	jne	.LBB14_549
# BB#548:                               # %land.lhs.true2722
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	je	.LBB14_550
.LBB14_549:                             # %if.then2727
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	movq	$107, %rdx
	xorq	%rcx, %rcx
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%r12, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %r9
	callq	emit_cmp_and_jump_insns
	movl	(%r12), %r8d
.LBB14_550:                             # %if.end2731
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$54, %rax
	jne	.LBB14_552
# BB#551:                               # %land.lhs.true2736
	movq	64(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	je	.LBB14_553
.LBB14_552:                             # %if.then2741
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	movq	$105, %rdx
	xorq	%rcx, %rcx
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%r12, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %r9
	callq	emit_cmp_and_jump_insns
.LBB14_553:                             # %if.end2745
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB14_555
# BB#554:                               # %if.then2750
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	andq	$-257, %rax             # imm = 0xFFFFFFFFFFFFFEFF
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	jmp	.LBB14_556
.LBB14_589:                             # %if.end2982
	movl	60(%r15), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	movq	96(%rsp), %rbx          # 8-byte Reload
	andq	%r13, %rbx
	cmpq	%rdx, %rbx
	jne	.LBB14_593
# BB#590:                               # %if.then2995
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	expand_expr
	movq	32(%r12), %rcx
	movq	8(%rcx), %rcx
	movl	16(%rcx), %edx
	movq	%rax, %rsi
	movl	188(%rsp), %eax
	movq	$13, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	cmpq	%rax, %rdx
	je	.LBB14_28
# BB#591:                               # %land.lhs.true3012
	movl	(%rsi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB14_28
# BB#592:                               # %if.then3017
	movabsq	$4026531839, %rcx       # imm = 0xEFFFFFFF
	andq	%rcx, %rax
	movl	%eax, (%rsi)
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_580:                             # %if.end2917
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	jne	.LBB14_582
# BB#581:                               # %if.then2920
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	assign_temp
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB14_582:                             # %if.end2922
	movq	88(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB14_583
# BB#585:                               # %if.then2943
	movq	32(%r12), %rax
	movq	8(%rax), %rax
	movq	%rax, %rdi
	callq	int_size_in_bytes
	movzbl	mode_bitsize(%rbx,%rbx), %esi
	movq	$3, %rcx
	shlq	%cl, %rax
	cmpq	%rsi, %rax
	jge	.LBB14_587
# BB#586:                               # %cond.true2956
	movq	32(%r12), %rax
	movq	8(%rax), %rax
	movq	%rax, %rdi
	callq	int_size_in_bytes
	movq	%rax, %rsi
	movq	$3, %rcx
	shlq	%cl, %rsi
.LBB14_587:                             # %cond.end2968
	movl	60(%r15), %eax
	movq	32(%r12), %rbx
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 8(%rsp)
	movq	$0, 16(%rsp)
	xorq	%rdx, %rdx
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	88(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	movq	%rax, %rcx
	movq	%rbx, %r8
	callq	store_field
	movq	%r14, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_617:                             # %if.then3185
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	gen_lowpart
	movq	%rax, %r12
	jmp	.LBB14_626
.LBB14_620:                             # %if.then3210
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$7418, %rsi             # imm = 0x1CFA
	jmp	.LBB14_104
.LBB14_622:                             # %if.end3211.if.then3225_crit_edge
	movl	60(%rax), %esi
.LBB14_624:                             # %if.then3225
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	movzbl	mode_size(%rsi), %ecx
	xorq	%rdx, %rdx
	movq	%rsi, %rdi
	movq	%rcx, %rsi
	movq	%rax, %rcx
	callq	assign_stack_temp_for_type
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB14_625:                             # %if.end3240
	movq	88(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	emit_move_insn
	movq	%rbx, %r12
.LBB14_626:                             # %if.end3244
	movzbl	(%r12), %eax
	cmpq	$66, %rax
	jne	.LBB14_627
# BB#628:                               # %if.then3249
	movq	%r12, %rdi
	callq	copy_rtx
	movq	104(%rsp), %rcx         # 8-byte Reload
	movzbl	18(%rcx), %ecx
	movq	%rax, %r14
	testq	$2, %rcx
	je	.LBB14_637
# BB#629:                               # %if.then3256
	movq	16(%r14), %rax
	testq	%rax, %rax
	movq	$8, %rdx
	je	.LBB14_631
# BB#630:                               # %cond.true3262
	movl	32(%rax), %edx
.LBB14_631:                             # %cond.end3268
	movq	104(%rsp), %rcx         # 8-byte Reload
	movl	64(%rcx), %ecx
	andq	%r15, %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB14_633
# BB#632:                               # %cond.end3268
	movq	$8, %rcx
.LBB14_633:                             # %cond.end3268
	jbe	.LBB14_636
# BB#634:                               # %cond.end3268
	testq	%rax, %rax
	je	.LBB14_636
# BB#635:                               # %cond.true3280
	movl	32(%rax), %ecx
.LBB14_636:                             # %cond.end3291
	movq	%r14, %rdi
	movq	%rcx, %rsi
	callq	set_mem_align
.LBB14_637:                             # %if.end3394
	movq	104(%rsp), %rax         # 8-byte Reload
	movl	60(%rax), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	adjust_address_1
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_239:                             # %if.end993
	movq	48(%r15), %rax
	testq	%rax, %rax
	movq	%rax, %rsi
	jne	.LBB14_257
# BB#240:                               # %if.then999
	movq	96(%rsp), %rax          # 8-byte Reload
	testq	%r12, %rax
	je	.LBB14_241
# BB#242:                               # %if.else1003
	movq	104(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %edi
	movq	$10, %rcx
	movq	%rdi, %rdx
	shrq	%cl, %rdx
	movl	60(%rax), %eax
	movq	$19, %rcx
	shrq	%cl, %rax
	andq	$255, %rdi
	xorq	%rsi, %rsi
	cmpq	$20, %rdi
	jne	.LBB14_246
# BB#243:                               # %land.rhs1026
	movq	104(%rsp), %rcx         # 8-byte Reload
	cmpq	$0, 8(%rcx)
	je	.LBB14_244
# BB#245:                               # %land.rhs1026
	movabsq	$1, %rsi
	jmp	.LBB14_246
.LBB14_237:                             # %if.then986
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$6437, %rsi             # imm = 0x1925
	jmp	.LBB14_104
.LBB14_290:                             # %if.then1268
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$6603, %rsi             # imm = 0x19CB
	jmp	.LBB14_104
.LBB14_1128:                            # %if.then6108
	movq	%rcx, %rdi
	callq	trampoline_address
	movq	%rax, %rdi
	jmp	.LBB14_1165
.LBB14_1130:                            # %if.then6124
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_1135:                            # %if.else6159
	movq	%r12, %rdx
	testq	%rdx, %rdx
	movq	%rdx, %rax
	movq	%rdx, %r14
	je	.LBB14_1137
# BB#1136:                              # %if.else6159
	movq	const_int_rtx+512(%rip), %rax
.LBB14_1137:                            # %if.else6159
	movq	112(%rsp), %rdx         # 8-byte Reload
	andq	%r13, %rdx
	cmpq	$3, %rdx
	jne	.LBB14_1138
# BB#1139:                              # %if.else6159
	movabsq	$1, %r8
	jmp	.LBB14_1140
.LBB14_1179:                            # %if.then6396
	xorq	%r8, %r8
	movq	%r15, %rdi
	movq	%r12, %r15
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	emit_no_conflict_block
	movq	%r15, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_1189:                            # %cond.false6445
	movabsq	$optab_table+256, %rax
.LBB14_1190:                            # %cond.end6446
	movq	(%rax), %r12
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	gen_imagpart
	xorq	%r8, %r8
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	callq	expand_unop
	cmpq	%r15, %rax
	je	.LBB14_1192
# BB#1191:                              # %if.then6452
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB14_1192:                            # %if.end6454
	callq	get_insns
	movq	%rax, %r14
	callq	end_sequence
	movzbl	(%r13), %eax
	cmpq	$65, %rax
	jne	.LBB14_1193
# BB#1194:                              # %if.else6462
	movq	%r14, %rdi
	callq	emit_insns
	movq	%r13, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_1119:                            # %if.then6060
	callq	expand_null_return
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_88:
	movq	%rdx, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_138:                             # %if.then522
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$6272, %rsi             # imm = 0x1880
	jmp	.LBB14_104
.LBB14_142:                             # %if.then540
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$6275, %rsi             # imm = 0x1883
	jmp	.LBB14_104
.LBB14_147:                             # %if.else566
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	fix_lexical_addr
.LBB14_148:                             # %if.end568
	movq	%rax, %r14
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB14_150
# BB#149:                               # %cond.false575
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB14_150:                             # %cond.end578
	movq	%rax, %rdi
	movq	%r14, %rsi
	jmp	.LBB14_176
.LBB14_167:                             # %cond.end669.if.then697_crit_edge
	movq	144(%rbx), %r14
.LBB14_172:                             # %if.then697
	testq	%r14, %r14
	movq	%r14, %rax
	jne	.LBB14_175
# BB#173:                               # %cond.end707
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %r14
	testq	%r14, %r14
	movq	%r14, %rax
	jne	.LBB14_175
# BB#174:                               # %cond.false715
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	xorq	%r14, %r14
.LBB14_175:                             # %cond.end718
	movq	8(%rax), %rax
	movq	%rax, %rdi
	callq	copy_rtx
	movq	%r14, %rdi
	movq	%rax, %rsi
.LBB14_176:                             # %if.end727
	callq	replace_equiv_address
.LBB14_177:                             # %if.end727
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB14_183
# BB#178:                               # %if.then730
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	jne	.LBB14_179
# BB#180:                               # %land.lhs.true735
	movq	8(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB14_181
# BB#182:                               # %if.then743
	movl	56(%rbx), %ecx
	andq	$16777215, %rcx         # imm = 0xFFFFFF
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	mark_reg_pointer
	movq	%r14, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_325:
	movq	%r14, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_342:                             # %if.end1571.land.lhs.true1577_crit_edge
	movl	16(%r12), %eax
.LBB14_343:                             # %land.lhs.true1577
	andq	$511, %rax              # imm = 0x1FF
	cmpq	$46, %rax
	jne	.LBB14_358
# BB#344:                               # %land.lhs.true1591
	movzbl	16(%r15), %eax
	cmpq	$25, %rax
	jne	.LBB14_358
# BB#345:                               # %land.lhs.true1598
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	40(%rax), %rax
	movq	%rax, %rdi
	callq	list_length
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB14_346
.LBB14_358:                             # %if.else1640
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	movq	72(%rsp), %r10          # 8-byte Reload
	movq	%r14, %rbx
	movq	%r13, %r9
	jle	.LBB14_367
# BB#359:                               # %if.else1640
	cmpq	$2, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB14_367
# BB#360:                               # %land.lhs.true1649
	movl	16(%r12), %eax
	andq	$4607, %rax             # imm = 0x11FF
	cmpq	$4130, %rax             # imm = 0x1022
	jne	.LBB14_367
# BB#361:                               # %land.lhs.true1670
	movq	104(%r12), %r12
	testq	%r12, %r12
	je	.LBB14_367
# BB#362:                               # %land.lhs.true1674
	movzbl	16(%r12), %eax
	testq	%rax, %rax
	je	.LBB14_367
# BB#363:                               # %if.then1683
	movzbl	16(%r15), %ecx
	cmpq	$25, %rcx
	jne	.LBB14_367
# BB#364:                               # %if.then1690
	movq	%r9, %r13
	movq	%rbx, %r14
	movq	%r10, %rbx
	cmpq	$29, %rax
	jne	.LBB14_365
# BB#384:                               # %land.lhs.true1740
	movslq	32(%r12), %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rbx, %r10
	movq	%r14, %rbx
	movq	%r13, %r9
	jns	.LBB14_367
# BB#385:                               # %if.then1747
	movq	8(%r12), %rax
	movq	8(%rax), %rax
	movl	60(%rax), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	movl	mode_class(,%rax,4), %ecx
	cmpq	$1, %rcx
	jne	.LBB14_367
# BB#386:                               # %land.lhs.true1763
	movzbl	mode_size(%rax), %ecx
	cmpq	$1, %rcx
	jne	.LBB14_367
# BB#387:                               # %if.then1769
	movq	32(%r15), %rcx
	movq	40(%r12), %rdx
	movsbq	(%rdx,%rcx), %rcx
	movq	%rcx, %rdi
	movq	%rax, %rsi
.LBB14_323:                             # %return
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_885:                             # %emit_queue.exit8404
	movq	40(%r14), %rax
	testq	%r12, %r12
	je	.LBB14_887
# BB#886:
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rcx, 88(%rsp)          # 8-byte Spill
.LBB14_887:                             # %emit_queue.exit8404
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB14_383
.LBB14_1084:                            # %if.else5855
	movq	$2, %rsi
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	assign_temp
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	preserve_temp_slots
	movq	%rbx, 144(%r14)
	movq	%rbx, %r12
	movzbl	17(%r14), %eax
	testq	$4, %rax
	je	.LBB14_1086
# BB#1085:                              # %if.then5865
	movq	%r14, %rdi
	callq	put_var_into_stack
.LBB14_1086:                            # %if.end5866
	movq	48(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB14_1088
# BB#1087:
	movq	%r12, %rsi
	jmp	.LBB14_1093
.LBB14_909:                             # %if.end5070
	movq	40(%rbx), %rax
	movzbl	16(%rax), %ecx
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$50, %rcx
	jne	.LBB14_913
# BB#910:                               # %land.lhs.true5083
	movq	48(%rbx), %rcx
	movq	32(%rax), %rax
	xorq	%rdx, %rdx
	xorq	%r15, %r15
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	operand_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_913
# BB#911:                               # %if.then5095
	movq	%rbx, %rax
	movq	40(%rax), %r14
	movq	48(%rax), %r13
	jmp	.LBB14_923
.LBB14_905:                             # %if.end5053
	movq	%rax, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	expand_expr
	movq	%rax, %rcx
	movzbl	2(%rcx), %eax
	movq	%rcx, %r14
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	andq	%rbx, %rdx
	movq	%rbx, %rcx
	cmpq	%rdx, %rax
	jne	.LBB14_907
# BB#906:
	movq	%r14, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_287:                             # %while.end
	movq	40(%rbx), %rax
	movq	%rax, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	expand_expr
	movq	32(%rbx), %rcx
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rcx, %rdi
.LBB14_566:                             # %return
	callq	expand_end_bindings
	movq	%rbx, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_722:                             # %if.else4012
	movq	32(%r12), %rdx
	movq	$59, %rdi
	xorq	%rax, %rax
	callq	build
	movq	%rax, %r10
.LBB14_638:                             # %plus_expr
	movq	%r10, %r12
	movl	188(%rsp), %eax
	testq	%rax, %rax
	jne	.LBB14_642
# BB#639:                               # %plus_expr
	movl	flag_trapv(%rip), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB14_642
# BB#640:                               # %land.lhs.true3406
	movq	96(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	movl	mode_class(,%rcx,4), %ecx
	cmpq	$1, %rcx
	jne	.LBB14_642
# BB#641:
	movabsq	$optab_table+8, %rdx
	jmp	.LBB14_643
.LBB14_727:                             # %cond.false4028
	movabsq	$optab_table+16, %rax
	movq	(%rax), %rbx
	jmp	.LBB14_1203
.LBB14_749:                             # %lor.lhs.false4188.if.end4396_crit_edge
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
.LBB14_750:                             # %if.end4396
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	expand_expr
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	40(%rcx), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movl	188(%rsp), %r8d
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%rax, %rdx
	movq	88(%rsp), %rcx          # 8-byte Reload
	callq	expand_mult
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_759:                             # %if.then4314
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	40(%rcx), %rdi
	movzbl	16(%rdi), %ecx
	movq	%rax, %r15
	cmpq	$25, %rcx
	je	.LBB14_1206
# BB#760:                               # %if.else4336
	movq	32(%rdi), %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	jmp	.LBB14_1207
.LBB14_765:                             # %if.else4378
	movq	32(%rdi), %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
.LBB14_766:                             # %if.end4386
	movq	%rax, %r14
	movl	188(%rsp), %eax
	movq	$3, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	movq	88(%rsp), %r8           # 8-byte Reload
	movq	%rax, %r9
	callq	expand_binop
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	gen_highpart
	movq	%rax, %r12
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	gen_highpart
	movl	188(%rsp), %r9d
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	movq	%rax, %r8
	callq	expand_mult_highpart_adjust
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	gen_highpart
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
	movq	%r13, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_813:
	movabsq	$optab_table+208, %rax
.LBB14_815:                             # %cond.false4632
	movq	(%rax), %rax
	movl	188(%rsp), %ebx
	movq	$2, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	movq	%r12, %r8
	movq	%rbx, %r9
	callq	expand_binop
	movq	%rax, %rsi
	testq	%rsi, %rsi
	jne	.LBB14_28
# BB#816:                               # %if.end4645
	movq	%r12, %rcx
	movzbl	(%rcx), %eax
	cmpq	$66, %rax
	movq	%rcx, %rbx
	jne	.LBB14_818
# BB#817:                               # %if.then4650
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, %rbx
.LBB14_818:                             # %if.end4652
	cmpq	%r15, %rbx
	je	.LBB14_820
# BB#819:                               # %if.then4655
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	emit_move_insn
.LBB14_820:                             # %if.end4657
	callq	gen_label_rtx
	movq	96(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	movl	mode_class(,%rcx,4), %ecx
	movq	%rax, %r15
	cmpq	$1, %rcx
	jne	.LBB14_826
# BB#821:                               # %land.lhs.true4663
	movq	$104, %rdi
	xorq	%rdx, %rdx
	movq	96(%rsp), %rsi          # 8-byte Reload
	callq	can_compare_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_822
.LBB14_826:                             # %if.else4682
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rax
	movq	8(%rax), %rax
	movl	16(%rax), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	cmpq	$79, %r13
	je	.LBB14_827
# BB#828:                               # %if.else4682
	movabsq	$1, %rcx
	jmp	.LBB14_829
.LBB14_800:                             # %if.end4566
	movq	32(%rbx), %rax
	movl	188(%rsp), %ebx
	movq	$1, %rdx
	movq	88(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	safe_from_p
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	callq	expand_abs
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_555:                             # %if.else2755
	movq	optab_table+144(%rip), %rax
	movq	%rax, 96(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$-257, %rsi             # imm = 0xFFFFFFFFFFFFFEFF
	callq	gen_rtx_CONST_INT
	xorq	%r8, %r8
	movq	$3, (%rsp)
	movq	%r13, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	movq	%r15, %r9
	callq	expand_binop
.LBB14_556:                             # %if.end2758
	movq	optab_table+16(%rip), %rcx
	xorq	%r8, %r8
	movq	$3, (%rsp)
	movq	%r13, %rdi
	movq	%rcx, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	movq	%r15, %r9
	callq	expand_binop
	movq	%rax, 96(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	xorq	%rdi, %rdi
	movq	$62, %rsi
	xorq	%r9, %r9
	movq	%r15, (%rsp)
	movq	%r13, %rdx
	movq	96(%rsp), %rcx          # 8-byte Reload
	movq	%rax, %r8
	callq	expand_divmod
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	$1, %rdi
	movq	$66, %rsi
	xorq	%r9, %r9
	movq	%r15, (%rsp)
	movq	%r13, %rdx
	movq	%r12, %rcx
	movq	%rax, %r8
	callq	expand_divmod
	movq	optab_table(%rip), %rcx
	movl	byte_mode(%rip), %r12d
	movq	%rax, 80(%rsp)          # 8-byte Spill
	xorq	%r8, %r8
	movq	$3, (%rsp)
	movq	%r13, %rdi
	movq	%rcx, %rsi
	movq	96(%rsp), %rdx          # 8-byte Reload
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	%r15, %r9
	callq	expand_binop
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	memory_address
	movl	byte_mode(%rip), %r15d
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	8(%rbx), %rcx
	movq	%rax, %rbx
	movq	%rcx, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	make_tree
	movq	$83, %rdi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	expand_shift
	movl	byte_mode(%rip), %ecx
	movq	optab_table+144(%rip), %rdx
	movq	88(%rsp), %rdi          # 8-byte Reload
	movzbl	2(%rdi), %esi
	movq	const_int_rtx+520(%rip), %rbx
	cmpq	%rcx, %rsi
	movq	%rdi, %r8
	movq	%rdi, %r15
	je	.LBB14_558
# BB#557:                               # %if.end2758
	xorq	%r8, %r8
.LBB14_558:                             # %if.end2758
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	expand_binop
	cmpq	%r15, %rax
	je	.LBB14_560
# BB#559:                               # %if.then2783
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	convert_move
.LBB14_560:                             # %if.end2784
	movq	%r14, %rdi
	callq	emit_jump
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	emit_label
	movq	const_int_rtx+512(%rip), %rax
	movq	%r15, %rbx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	%r14, %rdi
	jmp	.LBB14_831
.LBB14_593:                             # %if.end3021
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	movq	96(%rsp), %rdx          # 8-byte Reload
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	expand_expr
	movq	%rax, %rcx
	movl	(%rcx), %eax
	movq	%rcx, %r15
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	cmpq	%rbx, %rdx
	jne	.LBB14_595
# BB#594:
	movq	%r15, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_583:                             # %if.end2922
	cmpq	$66, %rax
	jne	.LBB14_588
# BB#584:                               # %if.then2927
	movl	60(%r15), %eax
	movq	32(%r12), %rbx
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	88(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	adjust_address_1
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	store_expr
	movq	%r14, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_627:
	movq	%r12, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_241:                             # %if.then1002
	movq	const_int_rtx+512(%rip), %r14
	jmp	.LBB14_247
.LBB14_244:
	movabsq	$0, %rsi
.LBB14_246:                             # %land.end1030
	andq	$2, %rdx
	andq	$4, %rax
	movq	$3, %rcx
	shlq	%cl, %rsi
	orq	%rdx, %rax
	orq	%rsi, %rax
	orq	$1, %rax
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	build_qualified_type
	movq	$3, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	assign_temp
	movq	%rax, %r14
.LBB14_247:                             # %if.end1036
	movq	%r14, 48(%r15)
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	jne	.LBB14_250
# BB#248:                               # %land.lhs.true1041
	movzbl	(%r14), %eax
	cmpq	$61, %rax
	jne	.LBB14_250
# BB#249:                               # %if.then1046
	movq	cfun(%rip), %rax
	movq	208(%rax), %rax
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	cfun(%rip), %rcx
	movq	%rax, 208(%rcx)
.LBB14_250:                             # %if.end1049
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB14_253
# BB#251:                               # %land.lhs.true1054
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	96(%rsp), %rcx          # 8-byte Reload
	andq	%r12, %rcx
	cmpq	%rcx, %rax
	je	.LBB14_253
# BB#252:                               # %if.then1060
	movq	48(%r15), %rax
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	gen_lowpart_SUBREG
	movq	%rax, %r14
	movl	(%r14), %eax
	orq	$268435456, %rax        # imm = 0x10000000
	movl	%eax, (%r14)
	movl	188(%rsp), %ecx
	andq	$1, %rcx
	shlq	$26, %rcx
	movabsq	$4227858431, %rdx       # imm = 0xFBFFFFFF
	andq	%rax, %rdx
	orq	%rcx, %rdx
	movl	%edx, (%r14)
.LBB14_253:                             # %if.end1073
	movq	const_int_rtx+512(%rip), %rcx
	movq	32(%r15), %rax
	xorq	%rdx, %rdx
	cmpq	%rcx, %r14
	je	.LBB14_254
# BB#255:                               # %if.else1081
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	store_expr
	jmp	.LBB14_256
.LBB14_1138:
	movabsq	$0, %r8
.LBB14_1140:                            # %if.else6159
	orq	$2, %r8
	xorq	%rdx, %rdx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	movq	%r8, %rcx
	callq	expand_expr
	testq	%r14, %r14
	je	.LBB14_1142
# BB#1141:
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_1193:                            # %if.then6460
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%r14, %rdi
	movq	%r13, %r14
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	emit_no_conflict_block
	movq	%r14, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_183:                             # %if.end751
	movq	144(%rbx), %rax
	testq	%rax, %rax
	movq	%rax, %rsi
	jne	.LBB14_185
# BB#184:                               # %cond.false758
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rsi
.LBB14_185:                             # %cond.end761
	movl	(%rsi), %r14d
	movq	%r14, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB14_200
# BB#186:                               # %land.lhs.true767
	testq	%rsi, %rsi
	jne	.LBB14_188
# BB#187:                               # %cond.false774
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movl	(%rax), %r14d
	movq	%rax, %rsi
.LBB14_188:                             # %cond.end777
	movq	48(%rbx), %rax
	movq	$16, %rcx
	movq	%r14, %rdx
	shrq	%cl, %rdx
	xorq	%rax, %rdx
	testq	$255, %rdx
	je	.LBB14_200
# BB#189:                               # %if.then789
	testq	%rsi, %rsi
	jne	.LBB14_191
# BB#190:                               # %cond.false796
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
	movq	48(%rbx), %rax
	movl	(%rcx), %r14d
.LBB14_191:                             # %cond.end799
	movzbl	16(%rbx), %edx
	movq	$16, %rcx
	shrq	%cl, %r14
	andq	$255, %r14
	cmpq	$36, %rdx
	jne	.LBB14_192
# BB#193:                               # %cond.end799
	movabsq	$1, %rcx
	jmp	.LBB14_194
.LBB14_179:
	movq	%r14, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_181:
	movq	%r14, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_346:                             # %if.then1610
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	40(%rax), %rax
	movq	32(%r15), %rsi
	testq	%rax, %rax
	je	.LBB14_347
# BB#348:                               # %if.then1610
	movabsq	$1, %rcx
	jmp	.LBB14_349
.LBB14_365:                             # %if.then1690
	cmpq	$46, %rax
	movq	%rbx, %r10
	movq	%r14, %rbx
	movq	%r13, %r9
	jne	.LBB14_367
# BB#366:                               # %if.then1699
	movq	40(%r12), %r12
	testq	%r12, %r12
	je	.LBB14_367
.LBB14_379:                             # %land.rhs1706
                                        # =>This Inner Loop Header: Depth=1
	movq	%r9, %r13
	movq	%rbx, %r14
	movq	%r10, %rbx
	movq	24(%r12), %rax
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	tree_int_cst_equal
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB14_380
# BB#378:                               # %for.cond1704
                                        #   in Loop: Header=BB14_379 Depth=1
	movq	(%r12), %r12
	testq	%r12, %r12
	movq	%rbx, %r10
	movq	%r14, %rbx
	movq	%r13, %r9
	jne	.LBB14_379
	jmp	.LBB14_367
.LBB14_1088:                            # %if.then5872
	movq	%r14, %rdi
	callq	maybe_build_cleanup
	movq	%rax, %rbx
	movq	%rbx, 48(%r15)
	movq	%r12, %rsi
.LBB14_1093:                            # %if.end5918
	movq	%r15, %rcx
	movq	40(%rcx), %rax
	movq	%rax, 56(%rcx)
	movq	$0, 40(%rcx)
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%rsi, %r14
                                        # kill: RSI<def> R14<kill>
	callq	store_expr
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	expand_decl_cleanup
	movq	%r14, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_913:                             # %if.else5102
	movq	48(%rbx), %rax
	movzbl	16(%rax), %ecx
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$50, %rcx
	jne	.LBB14_916
# BB#914:                               # %land.lhs.true5115
	movq	40(%rbx), %rcx
	movq	32(%rax), %rax
	xorq	%rdx, %rdx
	xorq	%r15, %r15
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	operand_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_916
# BB#915:                               # %if.then5127
	movq	%rbx, %rax
	movq	40(%rax), %r13
	movq	48(%rax), %r14
	jmp	.LBB14_923
.LBB14_907:                             # %if.end5064
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	jne	.LBB14_522
# BB#908:                               # %if.then5067
	movq	%rcx, %rdi
	jmp	.LBB14_521
.LBB14_642:                             # %cond.false3412
	movabsq	$optab_table, %rdx
.LBB14_643:                             # %cond.end3413
	movq	32(%r12), %rcx
	movzbl	16(%rcx), %esi
	movq	(%rdx), %rbx
	cmpq	$59, %rsi
	jne	.LBB14_650
# BB#644:                               # %land.lhs.true3424
	movq	40(%rcx), %rdx
	movzbl	16(%rdx), %edx
	cmpq	$25, %rdx
	jne	.LBB14_650
# BB#645:                               # %land.lhs.true3437
	movq	40(%r12), %rdx
	movzbl	16(%rdx), %esi
	cmpq	$120, %rsi
	jne	.LBB14_650
# BB#646:                               # %land.lhs.true3447
	movq	40(%rdx), %rsi
	movq	global_rtl+24(%rip), %rdi
	cmpq	%rdi, %rsi
	je	.LBB14_649
# BB#647:                               # %land.lhs.true3447
	movq	global_rtl+16(%rip), %rdi
	cmpq	%rdi, %rsi
	je	.LBB14_649
# BB#648:                               # %land.lhs.true3447
	movq	global_rtl+40(%rip), %rdi
	cmpq	%rdi, %rsi
	jne	.LBB14_650
.LBB14_649:                             # %if.then3474
	movq	32(%rcx), %rsi
	movq	%rsi, 40(%r12)
	movq	%rdx, 32(%rcx)
.LBB14_650:                             # %if.end3493
	movabsq	$4294967293, %rcx       # imm = 0xFFFFFFFD
	movq	112(%rsp), %rdx         # 8-byte Reload
	andq	%rdx, %rcx
	cmpq	$1, %rcx
	je	.LBB14_654
# BB#651:                               # %lor.lhs.false3499
	movl	ptr_mode(%rip), %ecx
	movq	96(%rsp), %rdx          # 8-byte Reload
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	%rcx, %rdx
	jne	.LBB14_674
# BB#652:                               # %land.lhs.true3502
	testq	%rax, %rax
	jne	.LBB14_654
# BB#653:                               # %land.lhs.true3502
	movl	flag_trapv(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB14_674
.LBB14_654:                             # %if.then3506
	movq	32(%r12), %rax
	movl	16(%rax), %ecx
	movq	%rcx, %rdx
	andq	$255, %rdx
	cmpq	$25, %rdx
	jne	.LBB14_659
# BB#655:                               # %land.lhs.true3516
	movq	96(%rsp), %rdx          # 8-byte Reload
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	movzbl	mode_bitsize(%rdx,%rdx), %edx
	cmpq	$64, %rdx
	ja	.LBB14_659
# BB#656:                               # %land.lhs.true3522
	movq	40(%r12), %rsi
	movzbl	17(%rsi), %edx
	testq	$2, %rdx
	je	.LBB14_659
# BB#657:                               # %if.then3532
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%rsi, %rdi
	movq	%r13, %rsi
	callq	expand_expr
	movq	%r12, %rdx
	movq	40(%rdx), %rcx
	movq	8(%rcx), %rcx
	movq	32(%rdx), %rdx
	movl	60(%rcx), %ebx
	movq	%rax, %r14
	movq	32(%rdx), %rax
	movq	$9, %rcx
	shrq	%cl, %rbx
	andq	$127, %rbx
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	movq	%rbx, %rdx
	callq	immed_double_const
	movq	8(%rax), %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	plus_constant_wide
	movq	%rax, %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	orq	$2, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	$3, %rax
	jne	.LBB14_673
# BB#658:
	movq	%rcx, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_822:                             # %if.then4666
	movq	104(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	%r8, %r8
	cmpq	$79, %r13
	jne	.LBB14_825
# BB#823:                               # %if.then4669
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	jmp	.LBB14_824
.LBB14_827:
	movabsq	$0, %rcx
.LBB14_829:                             # %if.else4682
	leaq	104(%rcx,%rcx), %rcx
	movq	%r15, 8(%rsp)
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	movq	96(%rsp), %r8           # 8-byte Reload
	callq	do_compare_rtx_and_jump
	jmp	.LBB14_830
.LBB14_595:                             # %if.end3032
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$133, %rax
	jg	.LBB14_602
# BB#596:                               # %if.end3032
	addq	$-54, %rax
	andq	%r13, %rax
	cmpq	$14, %rax
	ja	.LBB14_598
# BB#597:                               # %if.end3032
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB14_604
	jmp	.LBB14_598
.LBB14_588:                             # %if.else2979
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$7344, %rsi             # imm = 0x1CB0
	jmp	.LBB14_104
.LBB14_254:                             # %if.then1076
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	expand_expr
.LBB14_256:                             # %if.end1086
	movq	%r15, %rcx
	movl	16(%rcx), %eax
	orq	$65536, %rax            # imm = 0x10000
	movl	%eax, 16(%rcx)
	movq	48(%rcx), %rsi
                                        # kill: R15<def> RCX<kill>
.LBB14_257:                             # %if.end1092
	movl	(%rsi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB14_28
# BB#258:                               # %land.lhs.true1100
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	96(%rsp), %rcx          # 8-byte Reload
	movq	%r12, %rdx
	andq	%rcx, %rdx
	cmpq	%rdx, %rax
	je	.LBB14_28
# BB#259:                               # %if.then1109
	leaq	188(%rsp), %rax
	xorq	%rcx, %rcx
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	promote_mode
	movq	48(%r15), %rax
	movq	%rbx, %rdi
	jmp	.LBB14_199
.LBB14_1142:                            # %if.end6177
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	protect_from_queue
	movq	%rax, %r14
	movzbl	(%r14), %eax
	cmpq	$53, %rax
	jg	.LBB14_1145
# BB#1143:                              # %if.end6177
	cmpq	$39, %rax
	je	.LBB14_1155
# BB#1144:
	movq	%r14, %r15
	jmp	.LBB14_1160
.LBB14_200:                             # %if.end841
	testq	%rsi, %rsi
	jne	.LBB14_28
# BB#201:                               # %cond.false848
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_192:
	movabsq	$0, %rcx
.LBB14_194:                             # %cond.end799
	andq	$255, %rax
	leaq	188(%rsp), %rdx
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	promote_mode
	andq	%r15, %rax
	cmpq	%rax, %r14
	jne	.LBB14_195
# BB#196:                               # %if.end820
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB14_198
# BB#197:                               # %cond.false827
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB14_198:                             # %cond.end830
	movq	96(%rsp), %rdi          # 8-byte Reload
.LBB14_199:                             # %return
	movq	%rax, %rsi
	callq	gen_lowpart_SUBREG
	movq	%rax, %rsi
	movl	(%rsi), %eax
	orq	$268435456, %rax        # imm = 0x10000000
	movl	%eax, (%rsi)
	movl	188(%rsp), %ecx
	andq	$1, %rcx
	shlq	$26, %rcx
	movabsq	$4227858431, %rdx       # imm = 0xFBFFFFFF
	andq	%rax, %rdx
	orq	%rcx, %rdx
	movl	%edx, (%rsi)
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_347:
	movabsq	$0, %rcx
.LBB14_349:                             # %if.then1610
	je	.LBB14_356
# BB#350:                               # %if.then1610
	testq	%rsi, %rsi
	je	.LBB14_356
# BB#351:                               # %for.inc1629.preheader
	movq	$1, %rdx
	subq	%rsi, %rdx
	movabsq	$0, %rsi
	movabsq	$1, %rdi
.LBB14_352:                             # %for.inc1629
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	leaq	1(%rdx), %rbx
	movq	%rsi, %rcx
	je	.LBB14_354
# BB#353:                               # %for.inc1629
                                        #   in Loop: Header=BB14_352 Depth=1
	movq	%rdi, %rcx
.LBB14_354:                             # %for.inc1629
                                        #   in Loop: Header=BB14_352 Depth=1
	je	.LBB14_356
# BB#355:                               # %for.inc1629
                                        #   in Loop: Header=BB14_352 Depth=1
	testq	%rdx, %rdx
	movq	%rbx, %rdx
	jne	.LBB14_352
.LBB14_356:                             # %for.end1632
	testq	%rcx, %rcx
	movq	72(%rsp), %r10          # 8-byte Reload
	movq	%r14, %rbx
	movq	%r13, %r9
	je	.LBB14_367
# BB#357:                               # %if.then1634
	movq	32(%rax), %rax
	jmp	.LBB14_381
.LBB14_380:                             # %land.lhs.true1718
	movq	32(%r12), %rax
	movzbl	17(%rax), %ecx
	testq	$1, %rcx
	movq	%rbx, %r10
	movq	%r14, %rbx
	movq	%r13, %r9
	je	.LBB14_381
.LBB14_367:                             # %sw.bb1785
	movq	%rbx, 80(%rsp)          # 8-byte Spill
	movq	%r10, %rsi
	cmpq	$39, %r9
	jne	.LBB14_392
# BB#368:                               # %land.lhs.true1788
	movq	32(%rsi), %rax
	movzbl	16(%rax), %ecx
	cmpq	$46, %rcx
	jne	.LBB14_392
# BB#369:                               # %land.lhs.true1798
	cmpq	$0, 24(%rax)
	jne	.LBB14_392
# BB#370:                               # %if.then1806
	movq	40(%rax), %r15
	testq	%r15, %r15
	je	.LBB14_392
# BB#371:                               # %for.body1816.lr.ph
	movq	40(%rsi), %rax
.LBB14_372:                             # %for.body1816
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%r15), %rcx
	cmpq	%rax, %rcx
	jne	.LBB14_391
# BB#373:                               # %land.lhs.true1824
                                        #   in Loop: Header=BB14_372 Depth=1
	movq	48(%rcx), %rcx
	testq	$4096, %rcx             # imm = 0x1000
	je	.LBB14_376
# BB#374:                               # %lor.lhs.false1833
                                        #   in Loop: Header=BB14_372 Depth=1
	andq	$255, %rcx
	movl	mode_class(,%rcx,4), %edx
	cmpq	$1, %rdx
	jne	.LBB14_391
# BB#375:                               # %land.lhs.true1845
                                        #   in Loop: Header=BB14_372 Depth=1
	movzbl	mode_bitsize(%rcx,%rcx), %ecx
	cmpq	$64, %rcx
	jbe	.LBB14_376
.LBB14_391:                             # %for.inc1912
                                        #   in Loop: Header=BB14_372 Depth=1
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB14_372
.LBB14_392:                             # %if.end1916
	xorq	%r13, %r13
	movl	%r13d, 132(%rsp)
	leaq	132(%rsp), %rax
	leaq	152(%rsp), %rcx
	leaq	144(%rsp), %rdx
	leaq	136(%rsp), %rbx
	leaq	164(%rsp), %r8
	leaq	188(%rsp), %r9
	movq	%rax, (%rsp)
	movq	%rsi, %r15
	movq	%r15, 72(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	movq	%rcx, %rsi
	movq	%rbx, %rcx
	callq	get_inner_reference
	movq	%rax, %r12
	cmpq	%r15, %r12
	je	.LBB14_393
# BB#394:                               # %if.end1922
	movq	8(%r12), %rax
	movzbl	16(%rax), %ecx
	cmpq	$21, %rcx
	jne	.LBB14_397
# BB#395:                               # %land.lhs.true1931
	movq	32(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	movq	88(%rsp), %r13          # 8-byte Reload
	jne	.LBB14_397
# BB#396:                               # %land.lhs.true1931
	xorq	%r13, %r13
.LBB14_397:                             # %cond.end1944
	movabsq	$4294967294, %r15       # imm = 0xFFFFFFFE
	movq	112(%rsp), %rcx         # 8-byte Reload
	andq	%rcx, %r15
	cmpq	$2, %r15
	movq	%rcx, %rax
	movq	%rcx, %r14
	je	.LBB14_399
# BB#398:                               # %cond.end1944
	xorq	%rax, %rax
.LBB14_399:                             # %cond.end1944
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	%rax, %rcx
	callq	expand_expr
	movq	%rax, %r13
	movzbl	(%r13), %eax
	cmpq	$133, %rax
	jg	.LBB14_405
# BB#400:                               # %cond.end1944
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$14, %rax
	ja	.LBB14_407
# BB#401:                               # %cond.end1944
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB14_402
	jmp	.LBB14_407
.LBB14_916:                             # %if.else5134
	movq	40(%rbx), %rax
	movzbl	16(%rax), %ecx
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$49, %rcx
	jne	.LBB14_919
# BB#917:                               # %land.lhs.true5147
	movq	48(%rbx), %rcx
	movq	32(%rax), %rax
	xorq	%rdx, %rdx
	xorq	%r14, %r14
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	operand_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_919
# BB#918:                               # %if.then5159
	movq	%rbx, %rax
	movq	40(%rax), %r15
	movq	48(%rax), %r13
	jmp	.LBB14_923
.LBB14_659:                             # %if.else3566
	movq	40(%r12), %rdx
	movzbl	16(%rdx), %edx
	cmpq	$25, %rdx
	jne	.LBB14_674
# BB#660:                               # %land.lhs.true3576
	movq	96(%rsp), %rdx          # 8-byte Reload
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	movzbl	mode_bitsize(%rdx,%rdx), %edx
	cmpq	$32, %rdx
	ja	.LBB14_674
# BB#661:                               # %land.lhs.true3576
	andq	$512, %rcx              # imm = 0x200
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB14_674
# BB#662:                               # %if.then3592
	movq	112(%rsp), %rcx         # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$3, %rcx
	jne	.LBB14_663
# BB#664:                               # %if.then3592
	movabsq	$1, %rcx
	jmp	.LBB14_665
.LBB14_825:                             # %if.else4675
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
.LBB14_824:                             # %if.end4697
	movq	%r15, %r9
	callq	do_jump_by_parts_greater_rtx
.LBB14_830:                             # %if.end4697
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	emit_move_insn
	movq	%r15, %rdi
.LBB14_831:                             # %return
	callq	emit_label
	movq	%rbx, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_602:                             # %if.end3032
	cmpq	$134, %rax
	je	.LBB14_604
# BB#603:                               # %if.end3032
	cmpq	$140, %rax
	jne	.LBB14_598
.LBB14_604:                             # %if.then3072
	movq	32(%r12), %rax
	movq	8(%rax), %rax
	movl	60(%rax), %ebx
	movq	$9, %rcx
	shrq	%cl, %rbx
	andq	$127, %rbx
	movq	112(%rsp), %rcx         # 8-byte Reload
	andq	%r13, %rcx
	cmpq	$3, %rcx
	jne	.LBB14_606
# BB#605:                               # %if.then3085
	movq	96(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	subreg_lowpart_offset
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	simplify_gen_subreg
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_1145:                            # %if.end6177
	cmpq	$133, %rax
	jg	.LBB14_1151
# BB#1146:                              # %if.end6177
	addq	$-54, %rax
	andq	%r13, %rax
	cmpq	$16, %rax
	jbe	.LBB14_1148
# BB#1147:
	movq	%r14, %r15
	jmp	.LBB14_1160
.LBB14_195:                             # %if.then819
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$6332, %rsi             # imm = 0x18BC
	jmp	.LBB14_104
.LBB14_381:                             # %if.then1727
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
.LBB14_382:                             # %return
	movq	112(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB14_383
.LBB14_376:                             # %if.then1858
	movq	32(%r15), %rax
	movq	%rax, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	80(%rsp), %rdx          # 8-byte Reload
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	expand_expr
	movq	24(%r15), %rcx
	movzbl	49(%rcx), %edx
	movq	%rax, %r15
	testq	$16, %rdx
	je	.LBB14_377
# BB#388:                               # %if.then1871
	movq	8(%rcx), %rdx
	movq	40(%rcx), %rax
	movl	60(%rdx), %ebx
	movq	32(%rax), %rax
	movzbl	17(%rdx), %edx
	movq	$9, %rcx
	shrq	%cl, %rbx
	andq	$127, %rbx
	testq	$32, %rdx
	je	.LBB14_390
# BB#389:                               # %if.then1898
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	decq	%rdx
	xorq	%rdi, %rdi
	movq	%rdx, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	movq	88(%rsp), %rcx          # 8-byte Reload
	callq	expand_and
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_393:                             # %if.then1921
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$6877, %rsi             # imm = 0x1ADD
	jmp	.LBB14_104
.LBB14_405:                             # %cond.end1944
	cmpq	$134, %rax
	je	.LBB14_402
# BB#406:                               # %cond.end1944
	cmpq	$140, %rax
	jne	.LBB14_407
.LBB14_402:                             # %if.then1995
	movq	8(%r12), %rax
	movl	60(%rax), %eax
	movq	$9, %rcx
	movq	136(%rsp), %rdx
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	shrq	%cl, %rax
	andq	$127, %rax
	cmpq	$51, %rax
	je	.LBB14_408
# BB#403:                               # %if.then1995
	cmpq	$0, 48(%rsp)            # 8-byte Folded Reload
	jne	.LBB14_408
# BB#404:                               # %if.end2015.thread
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	force_reg
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	jmp	.LBB14_434
.LBB14_919:                             # %if.else5166
	movq	48(%rbx), %rax
	movzbl	16(%rax), %ecx
	movzbl	tree_code_type(%rcx), %ecx
	xorq	%r14, %r14
	cmpq	$49, %rcx
	movabsq	$0, %r15
	movabsq	$0, %r13
	jne	.LBB14_924
# BB#920:                               # %land.lhs.true5179
	movq	40(%rbx), %rcx
	movq	32(%rax), %rax
	xorq	%rdx, %rdx
	xorq	%r14, %r14
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	operand_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_921
# BB#922:                               # %if.then5191
	movq	%rbx, %rax
	movq	40(%rax), %r13
	movq	48(%rax), %r15
.LBB14_923:                             # %if.end5201
	movq	%rax, %rbx
	jmp	.LBB14_924
.LBB14_674:                             # %if.end3684
	movq	112(%rsp), %rax         # 8-byte Reload
	orq	$2, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$3, %rax
	jne	.LBB14_1203
# BB#675:                               # %lor.lhs.false3690
	movl	ptr_mode(%rip), %eax
	movq	96(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	jne	.LBB14_1203
# BB#676:                               # %if.end3694
	movq	40(%r12), %rax
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	safe_from_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	je	.LBB14_678
# BB#677:                               # %if.end3694
	movq	%r13, %rax
.LBB14_678:                             # %if.end3694
	movq	%r12, %rbx
	movq	32(%rbx), %rcx
	xorq	%rdx, %rdx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	movq	112(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rcx
	callq	expand_expr
	movq	40(%rbx), %rcx
	movq	%rax, %r15
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rcx, %rdi
	movq	%r14, %rcx
	callq	expand_expr
	movq	%rax, %r14
	jmp	.LBB14_679
.LBB14_663:
	movabsq	$0, %rcx
.LBB14_665:                             # %if.then3592
	leaq	1(%rcx,%rcx), %rcx
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	expand_expr
	movq	%rax, %r15
	movzbl	(%r15), %eax
	cmpq	$133, %rax
	jg	.LBB14_670
# BB#666:                               # %if.then3592
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$14, %rax
	ja	.LBB14_672
# BB#667:                               # %if.then3592
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB14_668
	jmp	.LBB14_672
.LBB14_598:                             # %if.end3095
	movq	112(%rsp), %rax         # 8-byte Reload
	andq	%r13, %rax
	cmpq	$3, %rax
	jne	.LBB14_607
# BB#599:                               # %if.then3098
	movl	188(%rsp), %eax
	testq	%rax, %rax
	movq	$121, %rax
	jne	.LBB14_601
# BB#600:                               # %select.mid
	movq	$120, %rax
.LBB14_601:                             # %select.end
	movq	%rax, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_606:                             # %if.else3088
	movl	16(%rax), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	jmp	.LBB14_609
.LBB14_1151:                            # %if.end6177
	cmpq	$134, %rax
	je	.LBB14_1154
# BB#1152:                              # %if.end6177
	cmpq	$140, %rax
	je	.LBB14_1154
# BB#1153:
	movq	%r14, %r15
	jmp	.LBB14_1160
.LBB14_1148:                            # %if.end6177
	movq	$1, %rdx
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	je	.LBB14_1149
.LBB14_1154:                            # %if.then6218
	movq	32(%rbx), %rax
	movq	8(%rax), %rax
	movl	60(%rax), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	force_const_mem
	movq	%rax, %r15
	jmp	.LBB14_1160
.LBB14_377:
	movq	%r15, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_390:                             # %if.else1901
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	movzbl	mode_bitsize(%rcx,%rcx), %ecx
	subq	%rax, %rcx
	xorq	%rsi, %rsi
	movq	%rcx, %rdi
	callq	build_int_2_wide
	movq	%rax, %r14
	movq	$82, %rdi
	xorq	%r9, %r9
	movq	%rbx, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	movq	88(%rsp), %r15          # 8-byte Reload
	movq	%r15, %r8
	callq	expand_shift
	movq	$83, %rdi
	xorq	%r9, %r9
	movq	%rbx, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	movq	%r15, %r8
.LBB14_842:                             # %return
	callq	expand_shift
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_407:                             # %cond.end1944.if.end2015_crit_edge
	movq	136(%rsp), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	%r13, 56(%rsp)          # 8-byte Spill
	jmp	.LBB14_409
.LBB14_408:                             # %if.else2011
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	force_const_mem
	movq	%rax, %rdi
	callq	validize_mem
	movq	%rax, 56(%rsp)          # 8-byte Spill
.LBB14_409:                             # %if.end2015
	cmpq	$0, 48(%rsp)            # 8-byte Folded Reload
	je	.LBB14_410
# BB#411:                               # %if.then2018
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	$1, %rbx
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	expand_expr
	movq	56(%rsp), %rcx          # 8-byte Reload
	movzbl	(%rcx), %ecx
	movq	%rax, 40(%rsp)          # 8-byte Spill
	addq	$-61, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$9, %rcx
	ja	.LBB14_420
# BB#412:                               # %if.then2018
	shlq	%cl, %rbx
	testq	$533, %rbx              # imm = 0x215
	je	.LBB14_420
# BB#413:                               # %if.then2039
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$118, %rcx
	jne	.LBB14_415
# BB#414:                               # %if.then2049
	movq	%rax, %rdi
	callq	put_var_into_stack
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	48(%rax), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	jmp	.LBB14_420
.LBB14_921:
	xorq	%r15, %r15
	xorq	%r13, %r13
.LBB14_924:                             # %if.end5201
	testq	%r12, %r12
	je	.LBB14_930
# BB#925:                               # %if.end5275.thread
	testq	%r13, %r13
	movabsq	$0, %rcx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	movabsq	$1, %rax
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	je	.LBB14_927
# BB#926:                               # %if.end5275.thread
	movq	%rax, 48(%rsp)          # 8-byte Spill
.LBB14_927:                             # %if.end5275.thread
	testq	%r14, %r14
	je	.LBB14_929
# BB#928:                               # %if.end5275.thread
	movq	%rax, 80(%rsp)          # 8-byte Spill
.LBB14_929:                             # %if.end5275.thread
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB14_999
.LBB14_1203:                            # %binop
	movq	40(%r12), %rax
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	safe_from_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	je	.LBB14_1205
# BB#1204:                              # %binop
	movq	%r13, %rax
.LBB14_1205:                            # %binop
	movq	%r12, %r14
	movq	32(%r14), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rsi, %rdi
	movq	%rax, %rsi
	callq	expand_expr
	movq	40(%r14), %rdi
	movq	%rax, %r15
.LBB14_1206:                            # %binop2
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
.LBB14_1207:                            # %binop2
	callq	expand_expr
	movq	%rax, %r14
	jmp	.LBB14_1208
.LBB14_670:                             # %if.then3592
	cmpq	$134, %rax
	je	.LBB14_668
# BB#671:                               # %if.then3592
	cmpq	$140, %rax
	jne	.LBB14_672
.LBB14_668:                             # %if.end3652
	movq	%r12, %rcx
	movq	32(%rcx), %rax
	movq	8(%rax), %rax
	movq	40(%rcx), %rcx
	movl	60(%rax), %eax
	movq	32(%rcx), %rdx
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	xorq	%rsi, %rsi
	movq	%rdx, %rdi
	movq	%rax, %rdx
	callq	immed_double_const
	movq	8(%rax), %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	plus_constant_wide
	movq	%rax, %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	orq	$2, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	$3, %rax
	jne	.LBB14_673
# BB#669:
	movq	%rcx, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_607:                             # %if.end3102
	movq	32(%r12), %rax
	movq	8(%rax), %rax
	movl	16(%rax), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB14_608
# BB#610:                               # %if.else3117
	movq	88(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r15, %rsi
	jmp	.LBB14_611
.LBB14_1149:                            # %if.end6177
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$68224, %rdx            # imm = 0x10A80
	je	.LBB14_1150
.LBB14_1155:                            # %if.then6255
	movq	32(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$118, %rcx
	jne	.LBB14_1157
# BB#1156:                              # %if.then6265
	movq	%rax, %rdi
	callq	put_var_into_stack
	movq	32(%rbx), %rax
	movq	48(%rax), %r15
	jmp	.LBB14_1160
.LBB14_410:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	jmp	.LBB14_434
.LBB14_415:                             # %if.else2059
	movq	8(%r12), %rax
	movl	16(%rax), %ebx
	movq	$10, %rcx
	movq	%rbx, %rsi
	shrq	%cl, %rsi
	movl	60(%rax), %edx
	movq	$19, %rcx
	shrq	%cl, %rdx
	andq	$255, %rbx
	xorq	%rdi, %rdi
	cmpq	$20, %rbx
	jne	.LBB14_419
# BB#416:                               # %land.rhs2096
	cmpq	$0, 8(%rax)
	je	.LBB14_417
# BB#418:                               # %land.rhs2096
	movabsq	$1, %rdi
	jmp	.LBB14_419
.LBB14_930:                             # %if.else5204
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB14_949
# BB#931:                               # %land.lhs.true5206
	movq	32(%rbx), %rax
	movq	$1, %rdx
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	safe_from_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB14_943
# BB#932:                               # %lor.lhs.false5212
	testq	%r13, %r13
	je	.LBB14_949
# BB#933:                               # %land.lhs.true5214
	movq	64(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB14_949
# BB#934:                               # %land.lhs.true5219
	movq	64(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %eax
	cmpq	$53, %rax
	jb	.LBB14_949
# BB#935:                               # %land.lhs.true5225
	movq	global_trees(%rip), %rcx
	movq	%r13, %rdx
.LBB14_936:                             # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rdx), %eax
	movq	%rdx, %r8
	movq	%rax, %rdx
	andq	$255, %rdx
	addq	$-114, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	$2, %rdx
	ja	.LBB14_939
# BB#937:                               # %land.lhs.true.i
                                        #   in Loop: Header=BB14_936 Depth=1
	movq	32(%r8), %rdx
	cmpq	%rcx, %rdx
	je	.LBB14_939
# BB#938:                               # %land.rhs.i
                                        #   in Loop: Header=BB14_936 Depth=1
	movq	8(%r8), %rsi
	movq	8(%rdx), %rdi
	movl	60(%rsi), %esi
	movl	60(%rdi), %edi
	xorq	%rsi, %rdi
	testq	$65024, %rdi            # imm = 0xFE00
	je	.LBB14_936
.LBB14_939:                             # %while.end.i
	andq	$254, %rax
	xorq	%rcx, %rcx
	cmpq	$34, %rax
	jne	.LBB14_942
# BB#940:                               # %sw.bb.i
	movq	144(%r8), %rcx
	testq	%rcx, %rcx
	jne	.LBB14_942
# BB#941:                               # %cond.false.i
	xorq	%rsi, %rsi
	movq	%r8, 112(%rsp)          # 8-byte Spill
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	make_decl_rtl
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	144(%rax), %rcx
.LBB14_942:                             # %var_rtx.exit
	movq	64(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %rcx
	jne	.LBB14_949
.LBB14_943:                             # %land.lhs.true5229
	movq	64(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %eax
	movq	96(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jne	.LBB14_949
# BB#944:                               # %land.lhs.true5235
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	can_conditionally_move_p
	movq	64(%rsp), %rcx          # 8-byte Reload
	movl	(%rcx), %ecx
	andq	$65535, %rcx            # imm = 0xFFFF
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB14_947
# BB#945:                               # %land.lhs.true5235
	cmpq	$61, %rcx
	je	.LBB14_947
# BB#946:                               # %lor.lhs.false5243
	movq	104(%rsp), %rax         # 8-byte Reload
	movzbl	17(%rax), %eax
	testq	$4, %rax
	je	.LBB14_949
.LBB14_947:                             # %land.lhs.true5250
	cmpq	$66, %rcx
	jne	.LBB14_952
# BB#948:                               # %lor.lhs.false5255
	movq	104(%rsp), %rax         # 8-byte Reload
	movzbl	17(%rax), %eax
	testq	$4, %rax
	jne	.LBB14_952
.LBB14_949:                             # %if.else5263
	movq	104(%rsp), %rax         # 8-byte Reload
	movzbl	17(%rax), %eax
	testq	$4, %rax
	jne	.LBB14_950
# BB#951:                               # %if.else5271
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	assign_temp
	movq	%rax, 64(%rsp)          # 8-byte Spill
.LBB14_952:                             # %if.end5275
	testq	%r13, %r13
	movabsq	$0, %rcx
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	movabsq	$1, %rax
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	je	.LBB14_954
# BB#953:                               # %if.end5275
	movq	%rax, 48(%rsp)          # 8-byte Spill
.LBB14_954:                             # %if.end5275
	testq	%r14, %r14
	movabsq	$0, %rcx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	je	.LBB14_956
# BB#955:                               # %if.end5275
	movq	%rax, 80(%rsp)          # 8-byte Spill
.LBB14_956:                             # %if.end5275
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB14_958
# BB#957:                               # %if.end5275
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB14_958:                             # %if.end5275
	je	.LBB14_998
# BB#959:                               # %if.end5275
	testq	%r13, %r13
	je	.LBB14_998
# BB#960:                               # %if.end5275
	testq	%r14, %r14
	je	.LBB14_998
# BB#961:                               # %land.lhs.true5281
	movzbl	16(%r14), %eax
	movq	$1, 80(%rsp)            # 8-byte Folded Spill
	addq	$-59, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$28, %rax
	ja	.LBB14_997
# BB#962:                               # %land.lhs.true5281
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$402653187, %rdx        # imm = 0x18000003
	je	.LBB14_997
# BB#963:                               # %land.lhs.true5309
	movslq	ix86_branch_cost(%rip), %rcx
	movq	40(%r14), %rax
	movq	%rax, %rdi
	cmpq	$3, %rcx
	jl	.LBB14_965
# BB#964:                               # %cond.true5312
	callq	integer_pow2p
	jmp	.LBB14_966
.LBB14_672:                             # %if.then3640
	movq	40(%r12), %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	112(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rcx
	callq	expand_expr
	movq	%rax, %r14
	orq	$2, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r12
	cmpq	$3, %r12
	jne	.LBB14_1208
.LBB14_679:                             # %both_summands
	movzbl	(%r15), %eax
	cmpq	$75, %rax
	jne	.LBB14_680
# BB#681:                               # %land.lhs.true3714
	movq	16(%r15), %rax
	movzbl	(%rax), %eax
	cmpq	$133, %rax
	jg	.LBB14_684
# BB#682:                               # %land.lhs.true3714
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$14, %rax
	ja	.LBB14_680
# BB#683:                               # %land.lhs.true3714
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB14_686
	jmp	.LBB14_680
.LBB14_673:                             # %if.then3563
	movq	%rcx, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	force_operand
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_608:                             # %if.then3105
	xorq	%rsi, %rsi
	movq	96(%rsp), %rdi          # 8-byte Reload
.LBB14_609:                             # %return
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	convert_modes
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_1150:
	movq	%r14, %r15
	jmp	.LBB14_1160
.LBB14_1157:                            # %if.else6275
	movq	8(%rax), %r12
	movq	$1, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	%r12, %rdi
	callq	assign_temp
	movzbl	(%r14), %ecx
	movq	%rax, %r15
	cmpq	$39, %rcx
	jne	.LBB14_1159
# BB#1158:                              # %if.then6288
	movq	%r12, %rdi
	callq	int_size_in_bytes
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	emit_group_store
	jmp	.LBB14_1160
.LBB14_417:
	movabsq	$0, %rdi
.LBB14_419:                             # %land.end2102
	andq	$2, %rsi
	andq	$4, %rdx
	movq	$3, %rcx
	shlq	%cl, %rdi
	orq	%rsi, %rdx
	orq	%rdi, %rdx
	orq	$1, %rdx
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	build_qualified_type
	movq	$1, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	%rax, %rdi
	callq	assign_temp
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	emit_move_insn
	movq	%rbx, 56(%rsp)          # 8-byte Spill
.LBB14_420:                             # %if.end2111
	movq	56(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %ecx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB14_421
# BB#422:                               # %if.end2117
	movq	40(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %edx
	movl	ptr_mode(%rip), %eax
	cmpq	%rax, %rdx
	je	.LBB14_424
# BB#423:                               # %if.then2123
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	convert_modes
	movq	56(%rsp), %rcx          # 8-byte Reload
	movl	(%rcx), %ecx
	movq	%rax, 40(%rsp)          # 8-byte Spill
.LBB14_424:                             # %if.end2125
	andq	$16777215, %rcx         # imm = 0xFFFFFF
	cmpq	$3342402, %rcx          # imm = 0x330042
	jne	.LBB14_433
# BB#425:                               # %land.lhs.true2136
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movl	(%rax), %eax
	testq	$16711680, %rax         # imm = 0xFF0000
	je	.LBB14_433
# BB#426:                               # %land.lhs.true2136
	movq	152(%rsp), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB14_433
# BB#427:                               # %land.lhs.true2148
	movq	144(%rsp), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	cqto
	idivq	32(%rsp)                # 8-byte Folded Reload
	movq	%rdx, %rax
	testq	%rax, %rax
	jne	.LBB14_433
# BB#428:                               # %land.lhs.true2151
	movl	164(%rsp), %ebx
	movq	%rbx, %rdi
	callq	get_mode_alignment
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	32(%rsp), %rax          # 8-byte Reload
	cqto
	idivq	%rcx
	movq	%rdx, %rax
	testq	%rax, %rax
	jne	.LBB14_433
# BB#429:                               # %land.lhs.true2157
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rax
	testq	%rax, %rax
	movq	$8, 32(%rsp)            # 8-byte Folded Spill
	je	.LBB14_431
# BB#430:                               # %cond.true2162
	movl	32(%rax), %eax
	movq	%rax, 32(%rsp)          # 8-byte Spill
.LBB14_431:                             # %cond.end2168
	movq	%rbx, %rdi
	callq	get_mode_alignment
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	32(%rsp), %rdx          # 8-byte Reload
	andq	%rcx, %rdx
	cmpq	%rax, %rdx
	jne	.LBB14_433
# BB#432:                               # %if.then2173
	movq	$63, %rcx
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	sarq	%cl, %rax
	movq	$61, %rcx
	shrq	%cl, %rax
	addq	%rdx, %rax
	movq	$3, %rcx
	sarq	%cl, %rax
	movq	$1, %rcx
	movq	$1, %r8
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	adjust_address_1
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	$0, 144(%rsp)
.LBB14_433:                             # %if.end2175
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	highest_pow2_factor
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	offset_address
	movq	%rax, 56(%rsp)          # 8-byte Spill
.LBB14_434:                             # %if.end2178
	movq	56(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$66, %rax
	jne	.LBB14_454
# BB#435:                               # %if.end2178
	movq	144(%rsp), %rax
	testq	%rax, %rax
	jne	.LBB14_454
# BB#436:                               # %if.end2178
	cmpq	$0, 48(%rsp)            # 8-byte Folded Reload
	je	.LBB14_454
# BB#437:
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	jmp	.LBB14_438
.LBB14_950:                             # %if.then5270
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$8197, %rsi             # imm = 0x2005
	jmp	.LBB14_104
.LBB14_965:                             # %cond.false5318
	callq	integer_onep
.LBB14_966:                             # %cond.false5318
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, 80(%rsp)            # 8-byte Folded Spill
	je	.LBB14_997
# BB#967:                               # %land.lhs.true5324
	movq	32(%rbx), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movzbl	16(%rax), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$60, %rax
	movq	$1, 80(%rsp)            # 8-byte Folded Spill
	jne	.LBB14_997
# BB#968:                               # %if.then5337
	movzbl	16(%r14), %eax
	cmpq	$60, %rax
	jne	.LBB14_969
# BB#974:                               # %cond.true5367
	movl	flag_trapv(%rip), %eax
	testq	%rax, %rax
	je	.LBB14_977
# BB#975:                               # %land.lhs.true5369
	movq	8(%r14), %rax
	movzbl	17(%rax), %eax
	testq	$32, %rax
	je	.LBB14_976
.LBB14_977:                             # %cond.false5379
	movq	optab_table+16(%rip), %rax
	jmp	.LBB14_982
.LBB14_1208:                            # %binop2
	movl	188(%rsp), %eax
	movq	$3, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	movq	88(%rsp), %r8           # 8-byte Reload
	movq	%rax, %r9
	callq	expand_binop
	movq	%rax, %rsi
	testq	%rsi, %rsi
	jne	.LBB14_28
# BB#1209:                              # %if.then6541
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$8870, %rsi             # imm = 0x22A6
	jmp	.LBB14_104
.LBB14_684:                             # %land.lhs.true3714
	cmpq	$134, %rax
	je	.LBB14_686
# BB#685:                               # %land.lhs.true3714
	cmpq	$140, %rax
	jne	.LBB14_680
.LBB14_686:                             # %if.then3778
	movq	%r15, %rbx
	jmp	.LBB14_687
.LBB14_1159:                            # %if.else6291
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	emit_move_insn
.LBB14_1160:                            # %if.end6296
	movzbl	(%r15), %eax
	cmpq	$66, %rax
	jne	.LBB14_1161
# BB#1162:                              # %if.end6302
	movq	%r15, %rdi
	callq	mark_temp_addr_taken
	movq	8(%r15), %rdx
	movabsq	$4294967293, %rax       # imm = 0xFFFFFFFD
	movq	112(%rsp), %rcx         # 8-byte Reload
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB14_1164
# BB#1163:
	movq	%rdx, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_421:                             # %if.then2116
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$6938, %rsi             # imm = 0x1B1A
	jmp	.LBB14_104
.LBB14_441:                             # %while.body.i8412
                                        #   in Loop: Header=BB14_438 Depth=1
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
.LBB14_438:                             # %while.cond.i8409
                                        # =>This Inner Loop Header: Depth=1
	movq	48(%rsp), %rax          # 8-byte Reload
	movzbl	16(%rax), %eax
	leaq	-114(%rax), %rcx
	andq	%rdx, %rcx
	cmpq	$3, %rcx
	jb	.LBB14_441
# BB#439:                               # %while.cond.i8409
                                        #   in Loop: Header=BB14_438 Depth=1
	cmpq	$88, %rax
	je	.LBB14_442
# BB#440:                               # %while.cond.i8409
                                        #   in Loop: Header=BB14_438 Depth=1
	cmpq	$58, %rax
	je	.LBB14_441
	jmp	.LBB14_454
.LBB14_969:                             # %if.then5337
	cmpq	$59, %rax
	jne	.LBB14_978
# BB#970:                               # %cond.true5345
	movl	flag_trapv(%rip), %eax
	testq	%rax, %rax
	je	.LBB14_973
# BB#971:                               # %land.lhs.true5347
	movq	8(%r14), %rax
	movzbl	17(%rax), %eax
	testq	$32, %rax
	je	.LBB14_972
.LBB14_973:                             # %cond.false5357
	movq	optab_table(%rip), %rax
	jmp	.LBB14_982
.LBB14_976:                             # %cond.true5378
	movq	optab_table+24(%rip), %rax
	jmp	.LBB14_982
.LBB14_680:
	movq	%r14, %rbx
	movq	%r15, %r14
.LBB14_687:                             # %if.end3779
	movzbl	(%rbx), %eax
	cmpq	$75, %rax
	jne	.LBB14_701
# BB#688:                               # %land.lhs.true3784
	movq	16(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$133, %rax
	jg	.LBB14_691
# BB#689:                               # %land.lhs.true3784
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$14, %rax
	ja	.LBB14_701
# BB#690:                               # %land.lhs.true3784
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB14_693
	jmp	.LBB14_701
.LBB14_1161:                            # %if.then6301
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$8635, %rsi             # imm = 0x21BB
	jmp	.LBB14_104
.LBB14_1164:                            # %if.end6312
	movq	%rdx, %rdi
.LBB14_1165:                            # %if.end6319
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	force_operand
	movq	%rax, %rdx
.LBB14_1166:                            # %if.end6319
	movl	flag_force_addr(%rip), %eax
	testq	%rax, %rax
	je	.LBB14_1172
# BB#1167:                              # %land.lhs.true6321
	movzbl	(%rdx), %eax
	cmpq	$61, %rax
	je	.LBB14_1172
# BB#1168:                              # %land.lhs.true6321
	movq	112(%rsp), %rax         # 8-byte Reload
	andq	%r13, %rax
	cmpq	$2, %rax
	je	.LBB14_1172
# BB#1169:                              # %land.lhs.true6321
	cmpq	$3, %rax
	je	.LBB14_1172
# BB#1170:                              # %land.lhs.true6321
	cmpq	$1, %rax
	je	.LBB14_1172
# BB#1171:                              # %if.then6335
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	$4, %rax
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	force_reg
	movq	%rax, %rdx
.LBB14_1172:                            # %if.end6340
	movl	(%rdx), %eax
	andq	$134283263, %rax        # imm = 0x800FFFF
	cmpq	$61, %rax
	jne	.LBB14_1173
# BB#1174:                              # %if.then6350
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movl	64(%rax), %eax
	movq	%rdx, %rbx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	mark_reg_pointer
	movq	%rbx, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_442:                             # %lor.lhs.false23.i
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_454
# BB#443:                               # %lor.lhs.false27.i
	movq	48(%rsp), %rcx          # 8-byte Reload
	addq	$32, %rcx
	movq	8(%rcx), %rax
	movq	%rcx, %rbx
	movq	$128, %rsi
	movq	%rax, %rdi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB14_454
# BB#444:                               # %lor.lhs.false33.i
	movq	8(%rbx), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	leaq	1(%rax), %rax
	movq	%rax, %rdi
	callq	exact_log2_wide
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
.LBB14_445:                             # %while.cond44.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rax
	movzbl	16(%rax), %ecx
	leaq	-114(%rcx), %rdx
	andq	%rsi, %rdx
	leaq	32(%rax), %rbx
	cmpq	$3, %rdx
	jb	.LBB14_445
# BB#446:                               # %lor.rhs56.i
	cmpq	$77, %rcx
	jne	.LBB14_454
# BB#447:
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
.LBB14_448:                             # %while.cond78.i
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax), %rax
	movzbl	16(%rax), %ecx
	leaq	-114(%rcx), %rdx
	andq	%rsi, %rdx
	cmpq	$3, %rdx
	jb	.LBB14_448
# BB#449:                               # %lor.rhs90.i
	cmpq	$121, %rcx
	jne	.LBB14_454
# BB#450:                               # %land.rhs.i8416
	movq	32(%rax), %rax
	cmpq	%r12, %rax
	je	.LBB14_453
# BB#451:                               # %lor.rhs111.i
	movzbl	16(%rax), %ecx
	cmpq	$57, %rcx
	jne	.LBB14_454
# BB#452:                               # %land.rhs120.i
	movq	8(%rax), %rax
	movq	8(%r12), %rcx
	cmpq	%rcx, %rax
	jne	.LBB14_454
.LBB14_453:                             # %if.then2192
	movq	$128, %rsi
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	set_mem_align
.LBB14_454:                             # %if.end2193
	movq	56(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB14_460
# BB#455:                               # %if.end2193
	movl	132(%rsp), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB14_460
# BB#456:                               # %if.end2193
	movq	%rax, %rcx
	andq	$134217728, %rcx        # imm = 0x8000000
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	jne	.LBB14_460
# BB#457:                               # %if.then2205
	cmpq	%r13, 56(%rsp)          # 8-byte Folded Reload
	jne	.LBB14_459
# BB#458:                               # %if.then2208
	movq	%r13, %rdi
	callq	copy_rtx
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movl	(%rax), %eax
.LBB14_459:                             # %if.end2210
	orq	$134217728, %rax        # imm = 0x8000000
	movq	56(%rsp), %rcx          # 8-byte Reload
	movl	%eax, (%rcx)
.LBB14_460:                             # %if.end2214
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$65, %rdx
	jne	.LBB14_464
# BB#461:                               # %if.then2219
	cmpq	$0, 144(%rsp)
	movq	56(%rsp), %rsi          # 8-byte Reload
	jne	.LBB14_463
# BB#462:                               # %lor.lhs.false2222
	movq	152(%rsp), %rdx
	movq	$15, %rcx
	shrq	%cl, %rax
	andq	$510, %rax              # imm = 0x1FE
	movzbl	mode_bitsize(%rax), %eax
	cmpq	%rax, %rdx
	je	.LBB14_28
.LBB14_463:                             # %if.then2231
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$6987, %rsi             # imm = 0x1B4B
	jmp	.LBB14_104
.LBB14_978:                             # %cond.false5382
	cmpq	$86, %rax
	je	.LBB14_979
# BB#980:                               # %cond.false5382
	movabsq	$optab_table+160, %rax
	jmp	.LBB14_981
.LBB14_972:                             # %cond.true5356
	movq	optab_table+8(%rip), %rax
	jmp	.LBB14_982
.LBB14_691:                             # %land.lhs.true3784
	cmpq	$134, %rax
	je	.LBB14_693
# BB#692:                               # %land.lhs.true3784
	cmpq	$140, %rax
	jne	.LBB14_701
.LBB14_693:                             # %if.then3848
	movq	const_int_rtx+512(%rip), %rax
	movq	%rax, 120(%rsp)
	movq	8(%rbx), %rax
	movq	$75, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	simplify_binary_operation
	testq	%rax, %rax
	jne	.LBB14_698
# BB#694:                               # %if.else3856
	movzbl	(%r14), %ecx
	movq	8(%rbx), %rax
	movq	$75, %rdi
	cmpq	$78, %rcx
	jne	.LBB14_696
# BB#695:                               # %if.then3861
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	movq	%rax, %rcx
	jmp	.LBB14_697
.LBB14_1173:
	movq	%rdx, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_464:                             # %if.end2233
	movl	164(%rsp), %esi
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rsi, %rsi
	je	.LBB14_476
# BB#465:                               # %if.end2233
	cmpq	$61, %rdx
	je	.LBB14_476
# BB#466:                               # %if.end2233
	cmpq	$63, %rdx
	je	.LBB14_476
# BB#467:                               # %lor.lhs.false2246
	movl	164(%rsp), %ecx
	cmpq	$51, %rcx
	je	.LBB14_473
# BB#468:                               # %land.lhs.true2249
	movzbl	direct_load(%rax), %eax
	testq	%rax, %rax
	jne	.LBB14_473
# BB#469:                               # %land.lhs.true2253
	movq	96(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	je	.LBB14_473
# BB#470:                               # %land.lhs.true2258
	cmpq	$6, %rax
	je	.LBB14_473
# BB#471:                               # %land.lhs.true2258
	movq	%r14, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$2, %rax
	je	.LBB14_473
# BB#472:                               # %land.lhs.true2258
	cmpq	$3, %rax
	jne	.LBB14_476
.LBB14_473:                             # %lor.lhs.false2269
	movq	152(%rsp), %rax
	testq	%rax, %rax
	js	.LBB14_498
# BB#474:                               # %land.lhs.true2290
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	movq	32(%rcx), %rcx
	movzbl	16(%rcx), %edx
	cmpq	$25, %rdx
	jne	.LBB14_498
# BB#475:                               # %land.lhs.true2301
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_498
.LBB14_476:                             # %if.then2309
	movq	96(%rsp), %r14          # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r14
	cmpq	$51, %r14
	jne	.LBB14_490
# BB#477:                               # %land.lhs.true2312
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB14_481
# BB#478:                               # %land.lhs.true2315
	movq	56(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$66, %rax
	jne	.LBB14_481
# BB#479:                               # %land.lhs.true2320
	movq	88(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$66, %rax
	jne	.LBB14_481
# BB#480:                               # %land.lhs.true2325
	movzbl	144(%rsp), %eax
	testq	$7, %rax
	je	.LBB14_482
.LBB14_481:                             # %if.end2332
	movq	152(%rsp), %rax
	movq	$1, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	callq	mode_for_size
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$51, %rcx
	jne	.LBB14_490
.LBB14_482:                             # %if.then2335
	movq	56(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$66, %rax
	jne	.LBB14_486
# BB#483:                               # %lor.lhs.false2340
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB14_485
# BB#484:                               # %land.lhs.true2343
	movq	88(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$66, %rax
	jne	.LBB14_486
.LBB14_485:                             # %lor.lhs.false2348
	movq	144(%rsp), %rax
	testq	$7, %rax
	jne	.LBB14_486
# BB#487:                               # %if.end2353
	movq	$63, %rcx
	movq	%rax, %rdx
	sarq	%cl, %rdx
	movq	$61, %rcx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	xorq	%rsi, %rsi
	movq	$1, %rcx
	movq	$1, %r8
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	adjust_address_1
	movq	%rax, %r14
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	jne	.LBB14_489
# BB#488:                               # %if.then2358
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	assign_temp
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB14_489:                             # %if.end2360
	movq	152(%rsp), %rax
	leaq	7(%rax), %rdx
	movq	$63, %rcx
	sarq	%cl, %rdx
	movq	$61, %rcx
	shrq	%cl, %rdx
	leaq	7(%rax,%rdx), %rax
	movq	$3, %rcx
	sarq	%cl, %rax
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	88(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	emit_block_move
	movq	%rbx, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_979:
	movabsq	$optab_table+152, %rax
.LBB14_981:                             # %cond.false5382
	movq	(%rax), %rax
.LBB14_982:                             # %cond.end5395
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	40(%rbx), %rax
	cmpq	%rax, %r13
	jne	.LBB14_984
# BB#983:                               # %if.then5402
	movq	88(%rsp), %rdi          # 8-byte Reload
	callq	invert_truthvalue
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	%rax, 32(%rbx)
.LBB14_984:                             # %if.end5410
	movq	$1, %rdx
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	safe_from_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	je	.LBB14_986
# BB#985:                               # %if.end5410
	movq	64(%rsp), %rax          # 8-byte Reload
.LBB14_986:                             # %if.end5410
	movslq	ix86_branch_cost(%rip), %rcx
	cmpq	$2, %rcx
	jge	.LBB14_987
# BB#988:                               # %if.end5410
	movabsq	$1, %rcx
	jmp	.LBB14_989
.LBB14_696:                             # %if.else3866
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%r14, %rcx
.LBB14_697:                             # %if.end3872
	callq	gen_rtx_fmt_ee
.LBB14_698:                             # %if.end3872
	leaq	120(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	eliminate_constant_term
	movq	120(%rsp), %rcx
	movq	16(%rbx), %r8
	movq	%rax, %r14
	movq	$75, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rcx, %rdx
	movq	%r8, %rcx
	callq	simplify_binary_operation
	testq	%rax, %rax
	jne	.LBB14_700
# BB#699:                               # %if.else3881
	movq	120(%rsp), %rax
	movq	16(%rbx), %rcx
	movq	$75, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
.LBB14_700:                             # %if.end3887
	movq	%rax, %rbx
.LBB14_701:                             # %if.end3887
	movzbl	(%r14), %eax
	cmpq	$133, %rax
	jg	.LBB14_705
# BB#702:                               # %if.end3887
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$14, %rax
	ja	.LBB14_707
# BB#703:                               # %if.end3887
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB14_704
	jmp	.LBB14_707
.LBB14_498:                             # %if.end2451
	movq	96(%rsp), %rbx          # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	cmpq	$51, %rbx
	jne	.LBB14_500
# BB#499:                               # %if.then2454
	movq	$51, %rax
	movl	%eax, 164(%rsp)
	movq	$51, 48(%rsp)           # 8-byte Folded Spill
.LBB14_500:                             # %if.end2455
	movq	144(%rsp), %rdx
	movq	%r14, %rsi
	decq	%rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	movq	$63, %rcx
	movq	%rdx, %rax
	sarq	%cl, %rax
	movq	$61, %rcx
	shrq	%cl, %rax
	addq	%rdx, %rax
	movq	$3, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rsi
	ja	.LBB14_502
# BB#501:                               # %if.then2464
	xorq	%rcx, %rcx
	jmp	.LBB14_503
.LBB14_490:                             # %if.end2365
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	validize_mem
	movq	%rax, %rbx
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB14_495
# BB#491:                               # %land.lhs.true2371
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB14_495
# BB#492:                               # %if.then2379
	movq	16(%rbx), %rdx
	testq	%rdx, %rdx
	movq	$8, %rcx
	je	.LBB14_494
# BB#493:                               # %cond.true2388
	movl	32(%rdx), %ecx
.LBB14_494:                             # %cond.end2394
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	mark_reg_pointer
.LBB14_495:                             # %if.end2396
	movq	152(%rsp), %r15
	movq	144(%rsp), %r13
	movq	8(%r12), %rax
	movl	188(%rsp), %r12d
	movq	%rax, %rdi
	callq	int_size_in_bytes
	movq	%rax, 8(%rsp)
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	movq	%r12, %rcx
	movq	88(%rsp), %r8           # 8-byte Reload
	movq	%rax, %r9
	movq	%rax, %rbx
	callq	extract_bit_field
	movq	%rax, %r15
	cmpq	$51, %r14
	jne	.LBB14_496
# BB#497:                               # %if.then2441
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	type_for_mode
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	build_qualified_type
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	%rax, %rdi
	callq	assign_temp
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	emit_move_insn
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	$51, %rax
	movb	%al, 2(%rbx)
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
.LBB14_579:                             # %return
	callq	set_mem_attributes
	movq	%rbx, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_486:                             # %if.then2352
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_expr, %rcx
	movq	$7035, %rsi             # imm = 0x1B7B
.LBB14_104:                             # %if.then320
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB14_987:
	movabsq	$0, %rcx
.LBB14_989:                             # %if.end5410
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	movq	96(%rsp), %rdx          # 8-byte Reload
	callq	do_store_flag
	movq	%rax, 88(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB14_995
# BB#990:                               # %land.lhs.true5425
	movq	40(%r14), %rax
	movq	%rax, %rdi
	callq	integer_onep
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB14_994
# BB#991:                               # %if.end5445
	movq	40(%r14), %rax
	movq	%rax, %rdi
	callq	tree_log2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	build_int_2_wide
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	$1, %rdx
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	safe_from_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	je	.LBB14_993
# BB#992:                               # %if.end5445
	movq	64(%rsp), %rax          # 8-byte Reload
.LBB14_993:                             # %if.end5445
	movq	$82, %rdi
	xorq	%r9, %r9
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	%rax, %r8
	callq	expand_shift
	movq	%rax, 88(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB14_995
.LBB14_994:                             # %if.then5447
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	callq	expand_expr
	movl	188(%rsp), %ebx
	movq	$3, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	64(%rsp), %r8           # 8-byte Reload
	movq	%rbx, %r9
	callq	expand_binop
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_705:                             # %if.end3887
	cmpq	$134, %rax
	je	.LBB14_704
# BB#706:                               # %if.end3887
	cmpq	$140, %rax
	je	.LBB14_704
.LBB14_707:                             # %lor.lhs.false3927
	movzbl	(%rbx), %eax
	cmpq	$78, %rax
	jne	.LBB14_708
.LBB14_704:                             # %if.then3932
	movq	%r14, %r15
	jmp	.LBB14_709
.LBB14_502:                             # %if.else2467
	movq	$1, %rcx
.LBB14_503:                             # %if.end2470
	movq	$1, %r8
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	adjust_address_1
	cmpq	%r13, %rax
	jne	.LBB14_505
# BB#504:                               # %if.then2473
	movq	%r13, %rdi
	callq	copy_rtx
.LBB14_505:                             # %if.end2475
	xorq	%rdx, %rdx
	movq	%rax, %r14
	movq	%r14, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	set_mem_attributes
	movq	8(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB14_509
# BB#506:                               # %if.then2483
	movq	16(%r14), %rdx
	testq	%rdx, %rdx
	movq	$8, %rcx
	je	.LBB14_508
# BB#507:                               # %cond.true2492
	movl	32(%rdx), %ecx
.LBB14_508:                             # %cond.end2498
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	mark_reg_pointer
.LBB14_509:                             # %if.end2500
	movl	132(%rsp), %eax
	movq	%r14, %rdx
	movl	(%rdx), %ecx
	andq	$1, %rax
	shlq	$27, %rax
	orq	%rcx, %rax
	movl	%eax, (%rdx)
                                        # kill: R14<def> RDX<kill>
	movl	164(%rsp), %eax
	cmpq	%rax, %rbx
	je	.LBB14_510
# BB#511:                               # %if.end2500
	cmpq	$51, %rax
	je	.LBB14_512
# BB#513:                               # %if.end2500
	movq	80(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	je	.LBB14_514
# BB#515:                               # %if.end2500
	cmpq	$2, %r15
	je	.LBB14_516
# BB#517:                               # %if.else2525
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	jne	.LBB14_522
# BB#518:                               # %if.then2528
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	80(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	jne	.LBB14_520
# BB#519:                               # %if.then2528
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	%rax, 80(%rsp)          # 8-byte Spill
.LBB14_520:                             # %if.then2528
	movq	80(%rsp), %rdi          # 8-byte Reload
.LBB14_521:                             # %if.end2537
	callq	gen_reg_rtx
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB14_522:                             # %if.end2537
	movl	188(%rsp), %eax
	movq	88(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r14, %rsi
.LBB14_611:                             # %return
	movq	%rax, %rdx
	callq	convert_move
	movq	%rbx, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_496:
	movq	%r15, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_995:                             # %if.else5450
	movq	40(%rbx), %rax
	movq	$1, 80(%rsp)            # 8-byte Folded Spill
	cmpq	%rax, %r13
	jne	.LBB14_997
# BB#996:                               # %if.then5456
	movq	32(%rbx), %rax
	movq	%rax, %rdi
	callq	invert_truthvalue
	movq	%rax, 32(%rbx)
.LBB14_997:                             # %if.end5466
	movq	$1, 48(%rsp)            # 8-byte Folded Spill
	movq	$1, 88(%rsp)            # 8-byte Folded Spill
.LBB14_998:                             # %if.end5466
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, 56(%rsp)          # 8-byte Spill
.LBB14_999:                             # %if.end5466
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB14_1003
# BB#1000:                              # %if.then.i8363
	movslq	(%rax), %rdx
	testq	%rdx, %rdx
	movabsq	$0, %rcx
	je	.LBB14_1002
# BB#1001:                              # %if.then3.i8369
	xorq	%rdi, %rdi
	movq	%rdx, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	adjust_stack
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
.LBB14_1002:                            # %if.end.i8371
	xorq	%rdx, %rdx
	movl	%edx, (%rax)
.LBB14_1003:                            # %do_pending_stack_adjust.exit8372
	incq	%rcx
	movl	%ecx, 4(%rax)
	callq	gen_label_rtx
	movq	%rax, 112(%rsp)         # 8-byte Spill
	cmpq	$0, 48(%rsp)            # 8-byte Folded Reload
	je	.LBB14_1028
# BB#1004:                              # %land.lhs.true5471
	movq	32(%rbx), %rax
	movzbl	17(%rax), %eax
	testq	$1, %rax
	je	.LBB14_1005
.LBB14_1028:                            # %if.else5566
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB14_1051
# BB#1029:                              # %land.lhs.true5568
	movq	32(%rbx), %rax
	movl	16(%rax), %ecx
	movq	%rcx, %rdx
	andq	$255, %rdx
	movzbl	tree_code_type(%rdx), %edx
	cmpq	$60, %rdx
	jne	.LBB14_1041
# BB#1030:                              # %land.lhs.true5581
	movq	40(%rax), %rax
	movq	%rax, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_1040
# BB#1031:                              # %land.lhs.true5590
	movq	%rbx, %rdx
	movq	32(%rdx), %rax
	movq	40(%rdx), %rcx
                                        # kill: RBX<def> RDX<kill>
	movq	32(%rax), %rax
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	operand_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_1040
# BB#1032:                              # %land.lhs.true5602
	movq	32(%rbx), %rax
	movzbl	17(%rax), %eax
	testq	$1, %rax
	je	.LBB14_1034
# BB#1033:                              # %lor.lhs.false5612
	movq	40(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$118, %rax
	jne	.LBB14_1040
.LBB14_1034:                            # %land.lhs.true5622
	movq	48(%rbx), %rax
	movq	$1, %rdx
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	safe_from_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_1040
# BB#1035:                              # %if.then5628
	movq	56(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB14_1038
# BB#1036:                              # %land.lhs.true5633
	movq	56(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %eax
	cmpq	$52, %rax
	ja	.LBB14_1038
# BB#1037:                              # %if.then5639
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, 56(%rsp)          # 8-byte Spill
.LBB14_1038:                            # %if.end5641
	movq	40(%rbx), %rax
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	56(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	callq	store_expr
	movq	32(%rbx), %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	movq	112(%rsp), %rdx         # 8-byte Reload
	callq	do_jump
	callq	start_cleanup_deferral
	movq	48(%rbx), %rax
	jmp	.LBB14_1039
.LBB14_708:
	movq	%rbx, %r15
	movq	%r14, %rbx
.LBB14_709:                             # %if.end3933
	movq	$75, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r15, %rcx
	callq	simplify_binary_operation
	movq	%rax, %rsi
	testq	%rsi, %rsi
	jne	.LBB14_28
# BB#710:                               # %cond.false3937
	movq	$75, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r15, %rcx
.LBB14_718:                             # %return
	callq	gen_rtx_fmt_ee
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	jmp	.LBB14_4
.LBB14_510:
	movq	%r14, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_512:
	movq	%r14, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_514:
	movq	%r14, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_516:
	movq	%r14, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_4
.LBB14_1005:                            # %if.then5481
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB14_1012
# BB#1006:                              # %if.then5484
	cmpq	$0, 80(%rsp)            # 8-byte Folded Reload
	je	.LBB14_1008
# BB#1007:                              # %land.lhs.true5486
	movq	40(%r14), %rax
	movq	$1, %rdx
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	safe_from_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_1010
.LBB14_1008:                            # %lor.lhs.false5492
	movq	56(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB14_1011
# BB#1009:                              # %land.lhs.true5497
	movq	56(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %eax
	cmpq	$52, %rax
	ja	.LBB14_1011
.LBB14_1010:                            # %if.then5503
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, 56(%rsp)          # 8-byte Spill
.LBB14_1011:                            # %if.end5505
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	store_expr
	jmp	.LBB14_1015
.LBB14_1040:                            # %if.else5653.land.lhs.true5655_crit_edge
	movq	32(%rbx), %rax
	movl	16(%rax), %ecx
.LBB14_1041:                            # %land.lhs.true5655
	andq	$255, %rcx
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$60, %rcx
	jne	.LBB14_1051
# BB#1042:                              # %land.lhs.true5668
	movq	40(%rax), %rax
	movq	%rax, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_1051
# BB#1043:                              # %land.lhs.true5677
	movq	%rbx, %rdx
	movq	32(%rdx), %rax
	movq	48(%rdx), %rcx
                                        # kill: RBX<def> RDX<kill>
	movq	32(%rax), %rax
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	operand_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_1051
# BB#1044:                              # %land.lhs.true5689
	movq	32(%rbx), %rax
	movzbl	17(%rax), %eax
	testq	$1, %rax
	je	.LBB14_1046
# BB#1045:                              # %lor.lhs.false5699
	movq	48(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$118, %rax
	jne	.LBB14_1051
.LBB14_1046:                            # %land.lhs.true5709
	movq	40(%rbx), %rax
	movq	$1, %rdx
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	safe_from_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_1051
# BB#1047:                              # %if.then5715
	movq	56(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB14_1050
# BB#1048:                              # %land.lhs.true5720
	movq	56(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %eax
	cmpq	$52, %rax
	ja	.LBB14_1050
# BB#1049:                              # %if.then5726
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, 56(%rsp)          # 8-byte Spill
.LBB14_1050:                            # %if.end5728
	movq	48(%rbx), %rax
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	56(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	callq	store_expr
	movq	32(%rbx), %rax
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	112(%rsp), %rsi         # 8-byte Reload
	callq	do_jump
	callq	start_cleanup_deferral
	movq	40(%rbx), %rax
.LBB14_1039:                            # %if.end5803
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	store_expr
	jmp	.LBB14_1070
.LBB14_1012:                            # %if.else5507
	movq	%r12, %rcx
	testq	%rcx, %rcx
	movq	%rcx, %rax
                                        # kill: R12<def> RCX<kill>
	je	.LBB14_1014
# BB#1013:                              # %if.else5507
	movq	const_int_rtx+512(%rip), %rax
.LBB14_1014:                            # %if.else5507
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	expand_expr
.LBB14_1015:                            # %if.end5514
	movq	%rbx, %rax
	movq	40(%rax), %rcx
	movq	32(%rax), %rax
	cmpq	%rcx, %r13
	je	.LBB14_1016
# BB#1017:                              # %if.else5524
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	112(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB14_1018
.LBB14_1051:                            # %if.else5740
	callq	gen_label_rtx
	movq	%rbx, %r14
	movq	32(%r14), %rcx
	movq	%rax, %rbx
	xorq	%rdx, %rdx
	movq	%rcx, %rdi
	movq	112(%rsp), %rsi         # 8-byte Reload
	callq	do_jump
	callq	start_cleanup_deferral
	movq	40(%r14), %rax
	movq	%r14, %r15
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB14_1054
# BB#1052:                              # %land.lhs.true5747
	movq	8(%rax), %rcx
	movq	global_trees+216(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB14_1054
# BB#1053:                              # %if.then5755
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	store_expr
	jmp	.LBB14_1057
.LBB14_1016:                            # %if.then5520
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	movq	112(%rsp), %rdx         # 8-byte Reload
.LBB14_1018:                            # %if.end5528
	callq	do_jump
	callq	start_cleanup_deferral
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	jne	.LBB14_1019
# BB#1020:                              # %if.end5528
	movabsq	$1, %rax
	jmp	.LBB14_1021
.LBB14_1054:                            # %if.else5760
	movq	%r12, %rcx
	testq	%rcx, %rcx
	movq	%rcx, %rsi
                                        # kill: R12<def> RCX<kill>
	je	.LBB14_1056
# BB#1055:                              # %if.else5760
	movq	const_int_rtx+512(%rip), %rsi
.LBB14_1056:                            # %if.else5760
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
.LBB14_1057:                            # %if.end5770
	callq	end_cleanup_deferral
	jmp	.LBB14_1058
.LBB14_1019:
	movabsq	$0, %rax
.LBB14_1021:                            # %if.end5528
	movq	80(%rsp), %rcx          # 8-byte Reload
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB14_1025
# BB#1022:                              # %if.then5533
	movq	40(%r14), %rax
	testq	%r12, %r12
	je	.LBB14_1024
# BB#1023:                              # %if.then5533
	movq	const_int_rtx+512(%rip), %r12
.LBB14_1024:                            # %if.then5533
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	expand_expr
	jmp	.LBB14_1070
.LBB14_1062:                            # %if.end.i8356
                                        #   in Loop: Header=BB14_1058 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%r14), %rcx
	movq	16(%rax), %rax
	movq	%rcx, 40(%rax)
.LBB14_1058:                            # %if.end5770
                                        # =>This Inner Loop Header: Depth=1
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %r14
	testq	%r14, %r14
	je	.LBB14_1063
# BB#1059:                              # %while.body.i8339
                                        #   in Loop: Header=BB14_1058 Depth=1
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$24, %rcx
	jne	.LBB14_1061
# BB#1060:                              # %if.then.i8345
                                        #   in Loop: Header=BB14_1058 Depth=1
	movq	8(%rax), %rcx
	movq	8(%rcx), %rcx
	movq	%rcx, 16(%r14)
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB14_1062
.LBB14_1061:                            # %if.else.i8348
                                        #   in Loop: Header=BB14_1058 Depth=1
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rax, 16(%r14)
	jmp	.LBB14_1062
.LBB14_1025:                            # %if.else5543
	cmpq	$0, 80(%rsp)            # 8-byte Folded Reload
	je	.LBB14_1027
# BB#1026:                              # %if.then5545
	movzbl	16(%r14), %ebx
	movq	104(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rdi
	movq	56(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rsi
	callq	make_tree
	movq	40(%r14), %rcx
	movq	%rax, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	build
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	store_expr
	jmp	.LBB14_1070
.LBB14_1063:                            # %emit_queue.exit8357
	movq	%rbx, %rdi
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	emit_label
	callq	start_cleanup_deferral
	movq	48(%r15), %rax
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB14_1066
# BB#1064:                              # %land.lhs.true5777
	movq	8(%rax), %rcx
	movq	global_trees+216(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB14_1066
# BB#1065:                              # %if.then5785
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	store_expr
	jmp	.LBB14_1069
.LBB14_1027:                            # %if.else5556
	movzbl	16(%r15), %ebx
	movq	104(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdi
	movq	56(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	callq	make_tree
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	build1
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	store_expr
	jmp	.LBB14_1070
.LBB14_1066:                            # %if.else5790
	testq	%r12, %r12
	je	.LBB14_1068
# BB#1067:                              # %if.else5790
	movq	const_int_rtx+512(%rip), %r12
.LBB14_1068:                            # %if.else5790
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	expand_expr
.LBB14_1069:                            # %if.end5803
	movq	%rbx, 112(%rsp)         # 8-byte Spill
.LBB14_1070:                            # %if.end5803
	callq	end_cleanup_deferral
	jmp	.LBB14_1071
.LBB14_1075:                            # %if.end.i8330
                                        #   in Loop: Header=BB14_1071 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rbx), %rcx
	movq	16(%rax), %rax
	movq	%rcx, 40(%rax)
.LBB14_1071:                            # %if.end5803
                                        # =>This Inner Loop Header: Depth=1
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB14_1076
# BB#1072:                              # %while.body.i
                                        #   in Loop: Header=BB14_1071 Depth=1
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$24, %rcx
	jne	.LBB14_1074
# BB#1073:                              # %if.then.i8328
                                        #   in Loop: Header=BB14_1071 Depth=1
	movq	8(%rax), %rcx
	movq	8(%rcx), %rcx
	movq	%rcx, 16(%rbx)
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB14_1075
.LBB14_1074:                            # %if.else.i
                                        #   in Loop: Header=BB14_1071 Depth=1
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rax, 16(%rbx)
	jmp	.LBB14_1075
.LBB14_1076:                            # %emit_queue.exit
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	emit_label
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	decq	%rcx
	movl	%ecx, 4(%rax)
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rax
	jmp	.LBB14_4
.Ltmp171:
	.size	expand_expr, .Ltmp171-expand_expr
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI14_0:
	.quad	.LBB14_202
	.quad	.LBB14_208
	.quad	.LBB14_209
	.quad	.LBB14_1201
	.quad	.LBB14_209
	.quad	.LBB14_101
	.quad	.LBB14_80
	.quad	.LBB14_207
	.quad	.LBB14_1201
	.quad	.LBB14_95
	.quad	.LBB14_92
	.quad	.LBB14_101
	.quad	.LBB14_1201
	.quad	.LBB14_1201
	.quad	.LBB14_367
	.quad	.LBB14_367
	.quad	.LBB14_314
	.quad	.LBB14_532
	.quad	.LBB14_328
	.quad	.LBB14_367
	.quad	.LBB14_523
	.quad	.LBB14_293
	.quad	.LBB14_879
	.quad	.LBB14_1095
	.quad	.LBB14_1094
	.quad	.LBB14_1077
	.quad	.LBB14_888
	.quad	.LBB14_280
	.quad	.LBB14_567
	.quad	.LBB14_1201
	.quad	.LBB14_561
	.quad	.LBB14_563
	.quad	.LBB14_261
	.quad	.LBB14_264
	.quad	.LBB14_638
	.quad	.LBB14_711
	.quad	.LBB14_729
	.quad	.LBB14_767
	.quad	.LBB14_767
	.quad	.LBB14_767
	.quad	.LBB14_767
	.quad	.LBB14_777
	.quad	.LBB14_777
	.quad	.LBB14_777
	.quad	.LBB14_777
	.quad	.LBB14_771
	.quad	.LBB14_767
	.quad	.LBB14_781
	.quad	.LBB14_780
	.quad	.LBB14_780
	.quad	.LBB14_780
	.quad	.LBB14_784
	.quad	.LBB14_789
	.quad	.LBB14_801
	.quad	.LBB14_801
	.quad	.LBB14_796
	.quad	.LBB14_834
	.quad	.LBB14_839
	.quad	.LBB14_839
	.quad	.LBB14_839
	.quad	.LBB14_839
	.quad	.LBB14_837
	.quad	.LBB14_838
	.quad	.LBB14_836
	.quad	.LBB14_1201
	.quad	.LBB14_832
	.quad	.LBB14_861
	.quad	.LBB14_861
	.quad	.LBB14_836
	.quad	.LBB14_837
	.quad	.LBB14_838
	.quad	.LBB14_877
	.quad	.LBB14_843
	.quad	.LBB14_843
	.quad	.LBB14_843
	.quad	.LBB14_843
	.quad	.LBB14_843
	.quad	.LBB14_843
	.quad	.LBB14_843
	.quad	.LBB14_843
	.quad	.LBB14_843
	.quad	.LBB14_843
	.quad	.LBB14_843
	.quad	.LBB14_843
	.quad	.LBB14_843
	.quad	.LBB14_533
	.quad	.LBB14_1201
	.quad	.LBB14_1201
	.quad	.LBB14_1201
	.quad	.LBB14_574
	.quad	.LBB14_574
	.quad	.LBB14_574
	.quad	.LBB14_612
	.quad	.LBB14_223
	.quad	.LBB14_260
	.quad	.LBB14_288
	.quad	.LBB14_1124
	.quad	.LBB14_574
	.quad	.LBB14_1175
	.quad	.LBB14_1200
	.quad	.LBB14_1176
	.quad	.LBB14_1183
	.quad	.LBB14_1181
	.quad	.LBB14_1182
	.quad	.LBB14_1121
	.quad	.LBB14_1121
	.quad	.LBB14_1123
	.quad	.LBB14_1123
	.quad	.LBB14_1198
	.quad	.LBB14_1195
	.quad	.LBB14_1196
	.quad	.LBB14_1197
	.quad	.LBB14_1201
	.quad	.LBB14_265
	.quad	.LBB14_1118
	.quad	.LBB14_268
	.quad	.LBB14_279
	.quad	.LBB14_269
	.quad	.LBB14_276
	.quad	.LBB14_220
	.quad	.LBB14_1201
	.quad	.LBB14_1199

	.text
	.globl	move_block_to_reg
	.align	16, 0x90
	.type	move_block_to_reg,@function
move_block_to_reg:                      # @move_block_to_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp178:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp179:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp180:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp181:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp182:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp183:
	.cfi_def_cfa_offset 64
.Ltmp184:
	.cfi_offset %rbx, -48
.Ltmp185:
	.cfi_offset %r12, -40
.Ltmp186:
	.cfi_offset %r13, -32
.Ltmp187:
	.cfi_offset %r14, -24
.Ltmp188:
	.cfi_offset %r15, -16
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rdx, %r13
	movq	%rsi, (%rsp)            # 8-byte Spill
	movq	%rdi, %r12
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB15_3
# BB#1:
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %r13
	.align	16, 0x90
.LBB15_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	word_mode(%rip), %edi
	leaq	(%rbx,%r12), %rsi
	callq	gen_rtx_REG
	movq	%rax, %r14
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%rbx, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	operand_subword_force
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	incq	%rbx
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	%r13, %rax
	jne	.LBB15_2
.LBB15_3:                               # %for.end
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp189:
	.size	move_block_to_reg, .Ltmp189-move_block_to_reg
	.cfi_endproc

	.globl	move_block_from_reg
	.align	16, 0x90
	.type	move_block_from_reg,@function
move_block_from_reg:                    # @move_block_from_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp196:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp197:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp198:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp199:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp200:
	.cfi_def_cfa_offset 48
.Ltmp201:
	.cfi_offset %rbx, -48
.Ltmp202:
	.cfi_offset %r12, -40
.Ltmp203:
	.cfi_offset %r13, -32
.Ltmp204:
	.cfi_offset %r14, -24
.Ltmp205:
	.cfi_offset %r15, -16
	movq	%rcx, %rax
	movq	%rdx, %r12
	movq	%rsi, %r15
	movq	%rdi, %r14
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %r12
	je	.LBB16_9
# BB#1:                                 # %if.end
	movl	target_flags(%rip), %edx
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	addq	$4, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB16_2
# BB#6:                                 # %land.lhs.true
	movq	$3, %rcx
	shlq	%cl, %rax
	movq	$1, %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	mode_for_size
	movq	%rax, %r13
	andq	%r13, %rbx
	cmpq	$51, %rbx
	jne	.LBB16_7
.LBB16_2:                               # %for.cond.preheader
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB16_9
# BB#3:
	xorq	%r13, %r13
	.align	16, 0x90
.LBB16_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rdx
	movq	$51, %rcx
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	operand_subword
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB16_5
# BB#8:                                 # %if.end34
                                        #   in Loop: Header=BB16_4 Depth=1
	movl	word_mode(%rip), %edi
	leaq	(%r13,%r14), %rsi
	callq	gen_rtx_REG
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB16_4
.LBB16_9:                               # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB16_7:                               # %if.then3
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	adjust_address_1
	movq	%rax, %r15
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_REG
	movq	%r15, %rdi
	movq	%rax, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_move_insn  # TAILCALL
.LBB16_5:                               # %if.then33
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.move_block_from_reg, %rcx
	movq	$1928, %rsi             # imm = 0x788
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp206:
	.size	move_block_from_reg, .Ltmp206-move_block_from_reg
	.cfi_endproc

	.globl	emit_group_load
	.align	16, 0x90
	.type	emit_group_load,@function
emit_group_load:                        # @emit_group_load
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp210:
	.cfi_def_cfa_offset 16
.Ltmp211:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp212:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$104, %rsp
.Ltmp213:
	.cfi_offset %rbx, -56
.Ltmp214:
	.cfi_offset %r12, -48
.Ltmp215:
	.cfi_offset %r13, -40
.Ltmp216:
	.cfi_offset %r14, -32
.Ltmp217:
	.cfi_offset %r15, -24
	movq	%rdi, %r14
	movq	%r14, -104(%rbp)        # 8-byte Spill
	movzbl	(%r14), %eax
	movq	%rdx, -96(%rbp)         # 8-byte Spill
	cmpq	$39, %rax
	jne	.LBB17_1
# BB#3:                                 # %if.end
	movq	8(%r14), %rax
	movq	8(%rax), %rcx
	movq	8(%rcx), %rdi
	testq	%rdi, %rdi
	jne	.LBB17_4
# BB#5:                                 # %if.end
	movabsq	$1, %r15
	jmp	.LBB17_6
.LBB17_4:
	movabsq	$0, %r15
.LBB17_6:                               # %if.end
	movq	%r15, -112(%rbp)        # 8-byte Spill
	movslq	(%rax), %rcx
	movq	%rsp, %rbx
	leaq	15(,%rcx,8), %rdx
	andq	$-16, %rdx
	subq	%rdx, %rbx
	movq	%rbx, -80(%rbp)         # 8-byte Spill
	movq	%rbx, %rsp
	cmpq	%rcx, %r15
	jge	.LBB17_7
# BB#8:                                 # %for.body.lr.ph
	testq	%rdi, %rdi
	movq	%rdi, -128(%rbp)        # 8-byte Spill
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, %rbx
	movq	%rsi, %r12
	movq	%r12, -120(%rbp)        # 8-byte Spill
	jne	.LBB17_10
# BB#9:                                 # %for.body.lr.ph
	movq	%rdx, %rbx
.LBB17_10:                              # %for.body.lr.ph
	movq	$32, %rcx
	movq	-96(%rbp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, -88(%rbp)         # 8-byte Spill
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	%r15, %rdi
	jmp	.LBB17_11
.LBB17_7:
	movq	%rbx, -80(%rbp)         # 8-byte Spill
	movq	%rdi, -128(%rbp)        # 8-byte Spill
	jmp	.LBB17_67
.LBB17_42:                              # %if.then169.lor.lhs.false184_crit_edge
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	8(%r15), %rax
	movl	(%rax), %eax
.LBB17_48:                              # %lor.lhs.false184
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %eax
	cmpq	%rax, -72(%rbp)         # 8-byte Folded Reload
	jne	.LBB17_60
# BB#49:                                # %land.lhs.true196
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	16(%r15), %rax
	movzbl	2(%rax), %eax
	movzbl	mode_size(%rax), %eax
	movq	%r13, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	jne	.LBB17_60
.LBB17_50:                              # %if.then208
                                        #   in Loop: Header=BB17_11 Depth=1
	cmpq	$0, -72(%rbp)           # 8-byte Folded Reload
	movabsq	$0, %rax
	je	.LBB17_52
# BB#51:                                # %if.then208
                                        #   in Loop: Header=BB17_11 Depth=1
	movabsq	$1, %rax
.LBB17_52:                              # %if.then208
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	8(%r15,%rax,8), %rdi
	movq	%rdi, (%r12,%rbx)
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$133, %rcx
	jg	.LBB17_57
# BB#53:                                # %if.then208
                                        #   in Loop: Header=BB17_11 Depth=1
	addq	$-54, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$14, %rcx
	movq	-104(%rbp), %r14        # 8-byte Reload
	movq	-112(%rbp), %r15        # 8-byte Reload
	ja	.LBB17_59
# BB#54:                                # %if.then208
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB17_66
# BB#55:                                # %if.then208
                                        #   in Loop: Header=BB17_11 Depth=1
	cmpq	$7, %rcx
	jne	.LBB17_59
# BB#56:                                # %lor.lhs.false279
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	-48(%rbp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB17_59
	jmp	.LBB17_66
.LBB17_60:                              # %if.else297
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	%rbx, %r14
	cmpq	$0, -72(%rbp)           # 8-byte Folded Reload
	jne	.LBB17_62
# BB#61:                                # %if.then300
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movzbl	mode_size(%rdi), %esi
	xorq	%rdx, %rdx
	callq	assign_stack_temp
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	emit_move_insn
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%rbx, %rdi
	movq	-48(%rbp), %rsi         # 8-byte Reload
	callq	adjust_address_1
	movq	%r14, %rbx
	movq	%rax, (%r12,%rbx)
	jmp	.LBB17_65
.LBB17_57:                              # %if.then208
                                        #   in Loop: Header=BB17_11 Depth=1
	cmpq	$134, %rcx
	movq	-104(%rbp), %r14        # 8-byte Reload
	movq	-112(%rbp), %r15        # 8-byte Reload
	je	.LBB17_66
# BB#58:                                # %if.then208
                                        #   in Loop: Header=BB17_11 Depth=1
	cmpq	$140, %rcx
	je	.LBB17_66
.LBB17_59:                              # %if.then287
                                        #   in Loop: Header=BB17_11 Depth=1
	andq	$536870911, %r13        # imm = 0x1FFFFFFF
	shlq	$3, %r13
	subq	$16, %rsp
	movq	-88(%rbp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	xorq	%rdx, %rdx
	movq	$1, %rcx
	xorq	%r8, %r8
	movq	-48(%rbp), %r9          # 8-byte Reload
	movq	%r9, (%rsp)
	movq	%r13, %rsi
	callq	extract_bit_field
	addq	$16, %rsp
	movq	-80(%rbp), %rcx         # 8-byte Reload
	movq	%rax, (%rcx,%rbx)
	jmp	.LBB17_66
	.align	16, 0x90
.LBB17_11:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, -64(%rbp)         # 8-byte Spill
	movq	8(%rax,%rbx), %rax
	movq	8(%rax), %rcx
	movzbl	2(%rcx), %esi
	movq	%rsi, -48(%rbp)         # 8-byte Spill
	movq	16(%rax), %rax
	movq	$32, %rcx
	movq	-96(%rbp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movzbl	mode_size(%rsi), %r14d
	movq	8(%rax), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movq	%r14, %r13
	js	.LBB17_15
# BB#12:                                # %land.lhs.true
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	%r14, %rax
	addq	%rsi, %rax
	movq	-88(%rbp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	movq	%r14, %r13
	jle	.LBB17_15
# BB#13:                                # %if.then45
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	-88(%rbp), %r13         # 8-byte Reload
	subq	%rsi, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	je	.LBB17_14
.LBB17_15:                              # %if.end58
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	%rsi, -72(%rbp)         # 8-byte Spill
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$133, %rcx
	jg	.LBB17_20
# BB#16:                                # %if.end58
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	%rbx, -56(%rbp)         # 8-byte Spill
	addq	$-54, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$14, %rcx
	ja	.LBB17_22
# BB#17:                                # %if.end58
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB17_23
# BB#18:                                # %if.end58
                                        #   in Loop: Header=BB17_11 Depth=1
	cmpq	$12, %rcx
	jne	.LBB17_22
# BB#19:                                #   in Loop: Header=BB17_11 Depth=1
	movq	%r12, %r15
	jmp	.LBB17_29
	.align	16, 0x90
.LBB17_20:                              # %if.end58
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	%rbx, -56(%rbp)         # 8-byte Spill
	cmpq	$134, %rcx
	je	.LBB17_23
# BB#21:                                # %if.end58
                                        #   in Loop: Header=BB17_11 Depth=1
	cmpq	$140, %rcx
	jne	.LBB17_22
.LBB17_23:                              # %lor.lhs.false102
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	$16, %rcx
	movq	%rax, %rdi
	shrq	%cl, %rdi
	andq	$255, %rdi
	movq	-48(%rbp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rdi
	je	.LBB17_24
# BB#25:                                # %lor.lhs.false102
                                        #   in Loop: Header=BB17_11 Depth=1
	testq	%rdi, %rdi
	movq	%r12, %r15
	jne	.LBB17_26
	jmp	.LBB17_29
	.align	16, 0x90
.LBB17_22:                              # %if.end58.if.then114_crit_edge
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rdi
.LBB17_26:                              # %if.then114
                                        #   in Loop: Header=BB17_11 Depth=1
	testq	%rdi, %rdi
	jne	.LBB17_28
# BB#27:                                # %if.then120
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	-48(%rbp), %rdi         # 8-byte Reload
.LBB17_28:                              # %if.else121
                                        #   in Loop: Header=BB17_11 Depth=1
	callq	gen_reg_rtx
	movq	%rax, %r15
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	emit_move_insn
	movl	(%r15), %eax
	jmp	.LBB17_29
.LBB17_24:                              #   in Loop: Header=BB17_11 Depth=1
	movq	%r12, %r15
	.align	16, 0x90
.LBB17_29:                              # %if.end128
                                        #   in Loop: Header=BB17_11 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB17_36
# BB#30:                                # %land.lhs.true133
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	16(%r15), %rax
	testq	%rax, %rax
	movq	$8, %rbx
	je	.LBB17_32
# BB#31:                                # %cond.true
                                        #   in Loop: Header=BB17_11 Depth=1
	movl	32(%rax), %ebx
.LBB17_32:                              # %cond.end
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	-48(%rbp), %rdi         # 8-byte Reload
	callq	get_mode_alignment
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	andq	%rcx, %rbx
	cmpq	%rax, %rbx
	jb	.LBB17_36
# BB#33:                                # %land.lhs.true144
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	$3, %rcx
	movq	-72(%rbp), %r12         # 8-byte Reload
	shlq	%cl, %r12
	movq	-48(%rbp), %rdi         # 8-byte Reload
	callq	get_mode_alignment
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	%r12, %rax
	cqto
	idivq	%rcx
	testq	%rdx, %rdx
	jne	.LBB17_36
# BB#34:                                # %land.lhs.true144
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%r14, %rax
	jne	.LBB17_36
# BB#35:                                # %if.then156
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	-48(%rbp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r12
	movq	-56(%rbp), %r14         # 8-byte Reload
	movq	-80(%rbp), %rax         # 8-byte Reload
	movq	%r12, (%rax,%r14)
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	-72(%rbp), %rdx         # 8-byte Reload
	callq	adjust_address_1
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	%r14, %rbx
	jmp	.LBB17_65
	.align	16, 0x90
.LBB17_36:                              # %if.else164
                                        #   in Loop: Header=BB17_11 Depth=1
	movl	(%r15), %edi
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$133, %rcx
	jg	.LBB17_43
# BB#37:                                # %if.else164
                                        #   in Loop: Header=BB17_11 Depth=1
	addq	$-54, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	movq	-80(%rbp), %r12         # 8-byte Reload
	movq	-56(%rbp), %rbx         # 8-byte Reload
	ja	.LBB17_45
# BB#38:                                # %if.else164
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB17_64
# BB#39:                                # %if.else164
                                        #   in Loop: Header=BB17_11 Depth=1
	cmpq	$7, %rcx
	jne	.LBB17_40
# BB#63:                                # %land.lhs.true363
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movq	-48(%rbp), %rax         # 8-byte Reload
	cmpq	%rax, %rdi
	jne	.LBB17_45
	jmp	.LBB17_64
	.align	16, 0x90
.LBB17_43:                              # %if.else164
                                        #   in Loop: Header=BB17_11 Depth=1
	cmpq	$134, %rcx
	movq	-80(%rbp), %r12         # 8-byte Reload
	movq	-56(%rbp), %rbx         # 8-byte Reload
	je	.LBB17_64
# BB#44:                                # %if.else164
                                        #   in Loop: Header=BB17_11 Depth=1
	cmpq	$140, %rcx
	jne	.LBB17_45
.LBB17_64:                              # %if.then369
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	%r15, (%r12,%rbx)
	jmp	.LBB17_65
.LBB17_40:                              # %if.else164
                                        #   in Loop: Header=BB17_11 Depth=1
	cmpq	$11, %rcx
	jne	.LBB17_45
# BB#41:                                # %if.then169
                                        #   in Loop: Header=BB17_11 Depth=1
	cmpq	$0, -72(%rbp)           # 8-byte Folded Reload
	jne	.LBB17_42
# BB#47:                                # %land.lhs.true172
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	8(%r15), %rax
	movl	(%rax), %eax
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movzbl	mode_size(%rdx), %ecx
	movq	%r13, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	%rcx, %rdx
	je	.LBB17_50
	jmp	.LBB17_48
	.align	16, 0x90
.LBB17_45:                              # %if.else372
                                        #   in Loop: Header=BB17_11 Depth=1
	andq	$536870911, %r13        # imm = 0x1FFFFFFF
	shlq	$3, %r13
	movq	$3, %rcx
	movq	-72(%rbp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	subq	$16, %rsp
	movq	-88(%rbp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$1, %rcx
	xorq	%r8, %r8
	movq	-48(%rbp), %r9          # 8-byte Reload
	movq	%r9, (%rsp)
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	extract_bit_field
	addq	$16, %rsp
	movq	%rax, (%r12,%rbx)
.LBB17_65:                              # %for.inc
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	-104(%rbp), %r14        # 8-byte Reload
	movq	-112(%rbp), %r15        # 8-byte Reload
.LBB17_66:                              # %for.inc
                                        #   in Loop: Header=BB17_11 Depth=1
	movq	8(%r14), %rax
	movq	-64(%rbp), %rdi         # 8-byte Reload
	incq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movslq	(%rax), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$8, %rbx
	cmpq	%rsi, %rdx
	movq	-120(%rbp), %r12        # 8-byte Reload
	jl	.LBB17_11
.LBB17_67:                              # %for.end
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	40(%rcx), %rbx
	testq	%rbx, %rbx
	je	.LBB17_73
	.align	16, 0x90
.LBB17_68:                              # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$24, %rax
	jne	.LBB17_70
# BB#69:                                # %if.then.i
                                        #   in Loop: Header=BB17_68 Depth=1
	movq	8(%rdi), %rax
	movq	8(%rax), %rax
	movq	%rax, 16(%rbx)
	callq	emit_insn
	jmp	.LBB17_71
	.align	16, 0x90
.LBB17_70:                              # %if.else.i
                                        #   in Loop: Header=BB17_68 Depth=1
	callq	emit_insn
	movq	%rax, 16(%rbx)
.LBB17_71:                              # %if.end.i
                                        #   in Loop: Header=BB17_68 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rbx), %rcx
	movq	16(%rax), %rax
	movq	%rcx, 40(%rax)
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB17_68
# BB#72:                                # %for.cond383.preheader.loopexit
	movq	8(%r14), %rax
.LBB17_73:                              # %for.cond383.preheader
	movslq	(%rax), %rcx
	cmpq	%rcx, %r15
	movq	-128(%rbp), %rcx        # 8-byte Reload
	movq	-80(%rbp), %r12         # 8-byte Reload
	jge	.LBB17_79
# BB#74:                                # %for.body390.lr.ph
	testq	%rcx, %rcx
	jne	.LBB17_75
# BB#76:                                # %for.body390.lr.ph
	movabsq	$1, %rbx
	jmp	.LBB17_77
.LBB17_75:
	movabsq	$0, %rbx
.LBB17_77:                              # %for.body390.lr.ph
	movq	$3, %rcx
	shlq	%cl, %rbx
	.align	16, 0x90
.LBB17_78:                              # %for.body390
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax,%rbx), %rax
	movq	8(%rax), %rdi
	movq	(%r12,%rbx), %rsi
	callq	emit_move_insn
	movq	8(%r14), %rax
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movslq	(%rax), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$8, %rbx
	cmpq	%rsi, %rdx
	jl	.LBB17_78
.LBB17_79:                              # %for.end405
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB17_1:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.emit_group_load, %rcx
	movq	$1953, %rsi             # imm = 0x7A1
	jmp	.LBB17_2
.LBB17_62:                              # %if.else315
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.emit_group_load, %rcx
	movq	$2028, %rsi             # imm = 0x7EC
	jmp	.LBB17_2
.LBB17_14:                              # %if.then56
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.emit_group_load, %rcx
	movq	$1978, %rsi             # imm = 0x7BA
.LBB17_2:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp218:
	.size	emit_group_load, .Ltmp218-emit_group_load
	.cfi_endproc

	.globl	emit_group_store
	.align	16, 0x90
	.type	emit_group_store,@function
emit_group_store:                       # @emit_group_store
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp222:
	.cfi_def_cfa_offset 16
.Ltmp223:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp224:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$88, %rsp
.Ltmp225:
	.cfi_offset %rbx, -56
.Ltmp226:
	.cfi_offset %r12, -48
.Ltmp227:
	.cfi_offset %r13, -40
.Ltmp228:
	.cfi_offset %r14, -32
.Ltmp229:
	.cfi_offset %r15, -24
	movq	%rsi, %r15
	movzbl	(%r15), %eax
	movq	%rdx, -88(%rbp)         # 8-byte Spill
	movq	%rdi, -120(%rbp)        # 8-byte Spill
	cmpq	$39, %rax
	jne	.LBB18_1
# BB#3:                                 # %if.end
	movq	8(%r15), %rax
	movq	8(%rax), %rcx
	movq	8(%rcx), %rcx
	movq	%rcx, -56(%rbp)         # 8-byte Spill
	testq	%rcx, %rcx
	jne	.LBB18_4
# BB#5:                                 # %if.end
	movabsq	$1, %rsi
	jmp	.LBB18_6
.LBB18_4:
	movabsq	$0, %rsi
.LBB18_6:                               # %if.end
	movq	%rsi, -48(%rbp)         # 8-byte Spill
	movslq	(%rax), %rcx
	movq	%rsp, %r14
	leaq	15(,%rcx,8), %rdx
	andq	$-16, %rdx
	subq	%rdx, %r14
	movq	%r14, %rsp
	cmpq	%rcx, %rsi
	jge	.LBB18_12
# BB#7:                                 # %for.body.lr.ph
	cmpq	$0, -56(%rbp)           # 8-byte Folded Reload
	jne	.LBB18_8
# BB#9:                                 # %for.body.lr.ph
	movabsq	$1, %r12
	jmp	.LBB18_10
.LBB18_8:
	movabsq	$0, %r12
.LBB18_10:                              # %for.body.lr.ph
	movq	-48(%rbp), %rdx         # 8-byte Reload
	movq	%rdx, -48(%rbp)         # 8-byte Spill
	movq	$3, %rcx
	shlq	%cl, %r12
	movq	%rdx, %rbx
	.align	16, 0x90
.LBB18_11:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax,%r12), %rax
	movq	%r15, %r13
	movq	8(%rax), %r15
	movzbl	2(%r15), %edi
	callq	gen_reg_rtx
	movq	%rax, (%r14,%r12)
	movq	%rax, %rdi
	movq	%r15, %rsi
	movq	%r13, %r15
	callq	emit_move_insn
	movq	8(%r15), %rax
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movslq	(%rax), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$8, %r12
	cmpq	%rsi, %rdx
	jl	.LBB18_11
.LBB18_12:                              # %for.end
	movq	%r14, %r12
	jmp	.LBB18_13
	.align	16, 0x90
.LBB18_17:                              # %if.end.i
                                        #   in Loop: Header=BB18_13 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rbx), %rcx
	movq	16(%rax), %rax
	movq	%rcx, 40(%rax)
.LBB18_13:                              # %for.end
                                        # =>This Inner Loop Header: Depth=1
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB18_18
# BB#14:                                # %while.body.i
                                        #   in Loop: Header=BB18_13 Depth=1
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$24, %rax
	jne	.LBB18_16
# BB#15:                                # %if.then.i
                                        #   in Loop: Header=BB18_13 Depth=1
	movq	8(%rdi), %rax
	movq	8(%rax), %rax
	movq	%rax, 16(%rbx)
	callq	emit_insn
	jmp	.LBB18_17
	.align	16, 0x90
.LBB18_16:                              # %if.else.i
                                        #   in Loop: Header=BB18_13 Depth=1
	callq	emit_insn
	movq	%rax, 16(%rbx)
	jmp	.LBB18_17
.LBB18_18:                              # %emit_queue.exit
	movq	-120(%rbp), %r13        # 8-byte Reload
	movl	(%r13), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-65(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$2, %rcx
	movq	%r13, %r14
	jb	.LBB18_23
# BB#19:                                # %emit_queue.exit
	cmpq	$39, %rax
	jne	.LBB18_22
# BB#20:                                # %if.then34
	movq	-120(%rbp), %rdi        # 8-byte Reload
	movq	%r15, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB18_51
# BB#21:                                # %if.end38
	movq	$32, %rcx
	movq	%r15, %r12
	movq	-88(%rbp), %r15         # 8-byte Reload
	movq	%r15, %rsi
	movq	-120(%rbp), %r14        # 8-byte Reload
	movzbl	2(%r14), %edi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdx, %rdx
	callq	assign_stack_temp
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	emit_group_store
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	jmp	emit_group_load  # TAILCALL
.LBB18_22:                              # %if.then53
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	callq	gen_reg_rtx
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rax, %r14
	movq	%r14, %rdi
	callq	emit_move_insn
.LBB18_23:                              # %if.end60
	movq	%r14, -104(%rbp)        # 8-byte Spill
	movq	8(%r15), %rax
	movq	%r15, -96(%rbp)         # 8-byte Spill
	movslq	(%rax), %rcx
	movq	-48(%rbp), %rdi         # 8-byte Reload
	cmpq	%rcx, %rdi
	movq	%r12, %rbx
	movq	%rbx, -112(%rbp)        # 8-byte Spill
	jge	.LBB18_45
# BB#24:                                # %for.body68.lr.ph
	cmpq	$0, -56(%rbp)           # 8-byte Folded Reload
	jne	.LBB18_25
# BB#26:                                # %for.body68.lr.ph
	movabsq	$1, %r15
	jmp	.LBB18_27
.LBB18_25:
	movabsq	$0, %r15
.LBB18_27:                              # %for.body68.lr.ph
	movq	$32, %rcx
	movq	-88(%rbp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, -64(%rbp)         # 8-byte Spill
	movq	$3, %rcx
	shlq	%cl, %r15
	.align	16, 0x90
.LBB18_28:                              # %for.body68
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, -48(%rbp)         # 8-byte Spill
	movq	(%rbx,%r15), %rcx
	movq	%rcx, -56(%rbp)         # 8-byte Spill
	movq	8(%rax,%r15), %rax
	movzbl	2(%rcx), %r9d
	movq	16(%rax), %rax
	movq	$32, %rcx
	movq	-88(%rbp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movzbl	mode_size(%r9), %esi
	movq	8(%rax), %r12
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movq	%rsi, %r13
	js	.LBB18_31
# BB#29:                                # %land.lhs.true90
                                        #   in Loop: Header=BB18_28 Depth=1
	movq	%rsi, %rax
	addq	%r12, %rax
	movq	-64(%rbp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	movq	%rsi, %r13
	jle	.LBB18_31
# BB#30:                                # %if.then95
                                        #   in Loop: Header=BB18_28 Depth=1
	movq	-64(%rbp), %r13         # 8-byte Reload
	subq	%r12, %r13
.LBB18_31:                              # %if.end98
                                        #   in Loop: Header=BB18_28 Depth=1
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$65, %rcx
	jne	.LBB18_32
# BB#33:                                # %if.then103
                                        #   in Loop: Header=BB18_28 Depth=1
	movq	%rsi, -80(%rbp)         # 8-byte Spill
	movq	8(%r14), %rdi
	movl	(%rdi), %eax
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movzbl	mode_size(%rdx), %ecx
	movq	%r13, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	addq	%r12, %rdx
	cmpq	%rcx, %rdx
	jle	.LBB18_36
# BB#34:                                # %if.else121
                                        #   in Loop: Header=BB18_28 Depth=1
	subq	%rcx, %r12
	jl	.LBB18_52
# BB#35:                                # %if.then133
                                        #   in Loop: Header=BB18_28 Depth=1
	movq	16(%r14), %rdi
	movl	(%rdi), %eax
	jmp	.LBB18_36
	.align	16, 0x90
.LBB18_32:                              #   in Loop: Header=BB18_28 Depth=1
	movq	%rsi, -80(%rbp)         # 8-byte Spill
	movq	%r14, %rdi
.LBB18_36:                              # %if.end150
                                        #   in Loop: Header=BB18_28 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB18_43
# BB#37:                                # %land.lhs.true155
                                        #   in Loop: Header=BB18_28 Depth=1
	movq	16(%rdi), %rax
	movq	%rdi, -72(%rbp)         # 8-byte Spill
	testq	%rax, %rax
	movq	$8, %rbx
	je	.LBB18_39
# BB#38:                                # %cond.true
                                        #   in Loop: Header=BB18_28 Depth=1
	movl	32(%rax), %ebx
.LBB18_39:                              # %cond.end
                                        #   in Loop: Header=BB18_28 Depth=1
	movq	%r9, %rdi
	movq	%r9, %r14
	callq	get_mode_alignment
	movq	%r14, %r9
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	andq	%rcx, %rbx
	cmpq	%rax, %rbx
	movq	-112(%rbp), %rbx        # 8-byte Reload
	movq	-72(%rbp), %rdi         # 8-byte Reload
	jb	.LBB18_43
# BB#40:                                # %land.lhs.true166
                                        #   in Loop: Header=BB18_28 Depth=1
	movq	$3, %rcx
	movq	%r12, %r14
	shlq	%cl, %r14
	movq	%r9, %rdi
	movq	%r9, %rbx
	callq	get_mode_alignment
	movq	-72(%rbp), %rdi         # 8-byte Reload
	movq	%rbx, %r9
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	%r14, %rax
	movq	-112(%rbp), %rbx        # 8-byte Reload
	cqto
	idivq	%rcx
	testq	%rdx, %rdx
	jne	.LBB18_43
# BB#41:                                # %land.lhs.true166
                                        #   in Loop: Header=BB18_28 Depth=1
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	-80(%rbp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB18_43
# BB#42:                                # %if.then178
                                        #   in Loop: Header=BB18_28 Depth=1
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r9, %rsi
	movq	%r12, %rdx
	callq	adjust_address_1
	movq	%rax, %rdi
	movq	-56(%rbp), %rsi         # 8-byte Reload
	callq	emit_move_insn
	jmp	.LBB18_44
	.align	16, 0x90
.LBB18_43:                              # %if.else183
                                        #   in Loop: Header=BB18_28 Depth=1
	andq	$536870911, %r13        # imm = 0x1FFFFFFF
	shlq	$3, %r13
	movq	$3, %rcx
	shlq	%cl, %r12
	movq	%r13, %rsi
	movq	%r12, %rdx
	movq	%r9, %rcx
	movq	-56(%rbp), %r8          # 8-byte Reload
	movq	-64(%rbp), %r9          # 8-byte Reload
	callq	store_bit_field
.LBB18_44:                              # %for.inc192
                                        #   in Loop: Header=BB18_28 Depth=1
	movq	-96(%rbp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movq	-48(%rbp), %rdi         # 8-byte Reload
	incq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movslq	(%rax), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$8, %r15
	cmpq	%rsi, %rdx
	movq	-104(%rbp), %r14        # 8-byte Reload
	jl	.LBB18_28
	jmp	.LBB18_45
	.align	16, 0x90
.LBB18_49:                              # %if.end.i295
                                        #   in Loop: Header=BB18_45 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rbx), %rcx
	movq	16(%rax), %rax
	movq	%rcx, 40(%rax)
.LBB18_45:                              # %for.end194
                                        # =>This Inner Loop Header: Depth=1
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB18_50
# BB#46:                                # %while.body.i278
                                        #   in Loop: Header=BB18_45 Depth=1
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$24, %rax
	jne	.LBB18_48
# BB#47:                                # %if.then.i284
                                        #   in Loop: Header=BB18_45 Depth=1
	movq	8(%rdi), %rax
	movq	8(%rax), %rax
	movq	%rax, 16(%rbx)
	callq	emit_insn
	jmp	.LBB18_49
	.align	16, 0x90
.LBB18_48:                              # %if.else.i287
                                        #   in Loop: Header=BB18_45 Depth=1
	callq	emit_insn
	movq	%rax, 16(%rbx)
	jmp	.LBB18_49
.LBB18_50:                              # %emit_queue.exit296
	movzbl	(%r14), %eax
	cmpq	$61, %rax
	jne	.LBB18_51
# BB#53:                                # %if.then199
	movq	-120(%rbp), %rdi        # 8-byte Reload
	movq	%r14, %rsi
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	jmp	emit_move_insn  # TAILCALL
.LBB18_51:                              # %if.end201
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB18_1:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.emit_group_store, %rcx
	movq	$2063, %rsi             # imm = 0x80F
	jmp	.LBB18_2
.LBB18_52:                              # %if.else147
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.emit_group_store, %rcx
	movq	$2142, %rsi             # imm = 0x85E
.LBB18_2:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp230:
	.size	emit_group_store, .Ltmp230-emit_group_store
	.cfi_endproc

	.globl	copy_blkmode_from_reg
	.align	16, 0x90
	.type	copy_blkmode_from_reg,@function
copy_blkmode_from_reg:                  # @copy_blkmode_from_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp237:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp238:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp239:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp240:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp241:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp242:
	.cfi_def_cfa_offset 112
.Ltmp243:
	.cfi_offset %rbx, -48
.Ltmp244:
	.cfi_offset %r12, -40
.Ltmp245:
	.cfi_offset %r13, -32
.Ltmp246:
	.cfi_offset %r14, -24
.Ltmp247:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	callq	int_size_in_bytes
	movl	target_flags(%rip), %edx
	movq	%rax, %r14
	movl	64(%rbx), %r12d
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movabsq	$4294967264, %r15       # imm = 0xFFFFFFE0
	movq	%rdx, %rax
	andq	%r15, %rax
	cmpq	%rax, %r12
	jb	.LBB19_2
# BB#1:                                 # %entry
	movq	%rdx, %r12
.LBB19_2:                               # %entry
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	jne	.LBB19_8
# BB#3:                                 # %if.then
	movl	16(%rbx), %edi
	movq	$10, %rcx
	movq	%rdi, %rax
	shrq	%cl, %rax
	movl	60(%rbx), %esi
	movq	$19, %rcx
	shrq	%cl, %rsi
	andq	$255, %rdi
	xorq	%rdx, %rdx
	cmpq	$20, %rdi
	jne	.LBB19_7
# BB#4:                                 # %land.rhs
	cmpq	$0, 8(%rbx)
	je	.LBB19_5
# BB#6:                                 # %land.rhs
	movabsq	$1, %rdx
	jmp	.LBB19_7
.LBB19_5:
	movabsq	$0, %rdx
.LBB19_7:                               # %land.end
	andq	$2, %rax
	andq	$4, %rsi
	movq	$3, %rcx
	shlq	%cl, %rdx
	orq	%rax, %rsi
	orq	%rdx, %rsi
	orq	$1, %rsi
	movq	%rbx, %rdi
	callq	build_qualified_type
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	%rax, %rdi
	callq	assign_temp
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%rax, %rdi
	callq	preserve_temp_slots
.LBB19_8:                               # %if.end
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %eax
	cmpq	$51, %rax
	je	.LBB19_11
# BB#9:                                 # %land.lhs.true
	movl	target_flags(%rip), %edx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	addq	$4, %rdx
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jae	.LBB19_11
# BB#10:                                # %if.then48
	movl	16(%rbx), %eax
	movl	word_mode(%rip), %edi
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	%rsi, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	convert_modes
	movq	%rax, 24(%rsp)          # 8-byte Spill
.LBB19_11:                              # %if.end54
	movq	$3, %rcx
	shlq	%cl, %r14
	movq	%r14, 40(%rsp)          # 8-byte Spill
	testq	%r14, %r14
	movq	%r15, %r14
	je	.LBB19_19
# BB#12:                                # %for.body.lr.ph
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r12
	xorq	%rbx, %rbx
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB19_13:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	target_flags(%rip), %edx
	movq	$20, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	andq	%r14, %rsi
	leaq	-1(%rsi), %rax
	testq	%rbx, %rax
	movq	%r14, %r13
	je	.LBB19_15
# BB#14:                                # %for.body
                                        #   in Loop: Header=BB19_13 Depth=1
	testq	%rbx, %rbx
	jne	.LBB19_16
.LBB19_15:                              # %if.then66
                                        #   in Loop: Header=BB19_13 Depth=1
	xorq	%rdx, %rdx
	movq	%rbx, %rax
	divq	%rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	movzbl	2(%rdi), %edx
	movq	%rax, %rsi
	callq	operand_subword_force
	movl	target_flags(%rip), %edx
	movq	%rax, 48(%rsp)          # 8-byte Spill
.LBB19_16:                              # %if.end76
                                        #   in Loop: Header=BB19_13 Depth=1
	movq	$20, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	andq	%r13, %rsi
	leaq	-1(%rsi), %rax
	testq	%rbx, %rax
	jne	.LBB19_18
# BB#17:                                # %if.then84
                                        #   in Loop: Header=BB19_13 Depth=1
	xorq	%rdx, %rdx
	movq	%rbx, %rax
	divq	%rsi
	movq	$1, %rdx
	movq	$51, %rcx
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	operand_subword
	movl	target_flags(%rip), %edx
	movq	%rax, 56(%rsp)          # 8-byte Spill
.LBB19_18:                              # %if.end92
                                        #   in Loop: Header=BB19_13 Depth=1
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movl	word_mode(%rip), %r15d
	andq	%r13, %rdx
	movq	%r13, %r14
	leaq	-1(%rdx), %r13
	andq	%rbx, %r13
	movq	%r15, (%rsp)
	movq	$1, %rcx
	xorq	%r8, %r8
	movq	%rdx, 8(%rsp)
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	%r13, %rdx
	movq	%r15, %r9
	callq	extract_bit_field
	movl	target_flags(%rip), %r9d
	movq	$20, %rcx
	shrq	%cl, %r9
	andq	$32, %r9
	addq	$32, %r9
	andq	%r14, %r9
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	%r13, %rdx
	movq	%r15, %rcx
	movq	%rax, %r8
	callq	store_bit_field
	addq	%r12, %rbx
	movq	40(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %rbx
	jb	.LBB19_13
.LBB19_19:                              # %for.end
	movq	32(%rsp), %rax          # 8-byte Reload
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp248:
	.size	copy_blkmode_from_reg, .Ltmp248-copy_blkmode_from_reg
	.cfi_endproc

	.globl	use_reg
	.align	16, 0x90
	.type	use_reg,@function
use_reg:                                # @use_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp251:
	.cfi_def_cfa_offset 16
.Ltmp252:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movzbl	(%rax), %ecx
	movq	%rdi, %rbx
	cmpq	$61, %rcx
	jne	.LBB20_3
# BB#1:                                 # %lor.lhs.false
	movl	8(%rax), %ecx
	cmpq	$53, %rcx
	jae	.LBB20_3
# BB#2:                                 # %if.end
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	(%rbx), %rcx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, (%rbx)
	popq	%rbx
	retq
.LBB20_3:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.use_reg, %rcx
	movq	$2261, %rsi             # imm = 0x8D5
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp253:
	.size	use_reg, .Ltmp253-use_reg
	.cfi_endproc

	.globl	use_regs
	.align	16, 0x90
	.type	use_regs,@function
use_regs:                               # @use_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp260:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp261:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp262:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp263:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp264:
	.cfi_def_cfa_offset 48
.Ltmp265:
	.cfi_offset %rbx, -48
.Ltmp266:
	.cfi_offset %r12, -40
.Ltmp267:
	.cfi_offset %r13, -32
.Ltmp268:
	.cfi_offset %r14, -24
.Ltmp269:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r13
	leaq	(%r14,%r15), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jg	.LBB21_6
# BB#1:                                 # %for.cond.preheader
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB21_9
# BB#2:                                 # %for.body.lr.ph
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	leaq	reg_raw_mode(,%r15,4), %r12
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB21_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r12,%rbx,4), %edi
	leaq	(%r15,%rbx), %rsi
	callq	gen_rtx_REG
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB21_5
# BB#4:                                 # %lor.lhs.false.i
                                        #   in Loop: Header=BB21_3 Depth=1
	movl	8(%rax), %ecx
	cmpq	$53, %rcx
	jae	.LBB21_5
# BB#8:                                 # %use_reg.exit
                                        #   in Loop: Header=BB21_3 Depth=1
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	(%r13), %rcx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, (%r13)
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB21_3
.LBB21_9:                               # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB21_6:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.use_regs, %rcx
	movq	$2280, %rsi             # imm = 0x8E8
	jmp	.LBB21_7
.LBB21_5:                               # %if.then.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.use_reg, %rcx
	movq	$2261, %rsi             # imm = 0x8D5
.LBB21_7:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp270:
	.size	use_regs, .Ltmp270-use_regs
	.cfi_endproc

	.globl	use_group_regs
	.align	16, 0x90
	.type	use_group_regs,@function
use_group_regs:                         # @use_group_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp275:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp276:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp277:
	.cfi_def_cfa_offset 32
.Ltmp278:
	.cfi_offset %rbx, -32
.Ltmp279:
	.cfi_offset %r14, -24
.Ltmp280:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	8(%r14), %rax
	movslq	(%rax), %rcx
	movq	%rdi, %r15
	testq	%rcx, %rcx
	jle	.LBB22_7
# BB#1:
	movq	$1, %rbx
	.align	16, 0x90
.LBB22_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %rcx
	movq	8(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB22_6
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB22_2 Depth=1
	movzbl	(%rdx), %ecx
	cmpq	$61, %rcx
	jne	.LBB22_6
# BB#4:                                 # %lor.lhs.false.i
                                        #   in Loop: Header=BB22_2 Depth=1
	movl	8(%rdx), %eax
	cmpq	$53, %rax
	jae	.LBB22_8
# BB#5:                                 # %use_reg.exit
                                        #   in Loop: Header=BB22_2 Depth=1
	movq	$48, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	movq	(%r15), %rcx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, (%r15)
	movq	8(%r14), %rax
.LBB22_6:                               # %for.inc
                                        #   in Loop: Header=BB22_2 Depth=1
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB22_2
.LBB22_7:                               # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB22_8:                               # %if.then.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.use_reg, %rcx
	movq	$2261, %rsi             # imm = 0x8D5
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp281:
	.size	use_group_regs, .Ltmp281-use_group_regs
	.cfi_endproc

	.globl	can_store_by_pieces
	.align	16, 0x90
	.type	can_store_by_pieces,@function
can_store_by_pieces:                    # @can_store_by_pieces
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp288:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp289:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp290:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp291:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp292:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp293:
	.cfi_def_cfa_offset 80
.Ltmp294:
	.cfi_offset %rbx, -48
.Ltmp295:
	.cfi_offset %r12, -40
.Ltmp296:
	.cfi_offset %r13, -32
.Ltmp297:
	.cfi_offset %r14, -24
.Ltmp298:
	.cfi_offset %r15, -16
	movl	class_narrowest_mode+4(%rip), %eax
	movq	%rdi, %r12
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	testq	%rax, %rax
	je	.LBB23_1
# BB#2:
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rdx, %r14
	movq	$17, %rax
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%r12, %r15
	.align	16, 0x90
.LBB23_3:                               # %for.body.lr.ph.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB23_4 Depth 2
	movl	class_narrowest_mode+4(%rip), %ebx
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB23_4:                               # %for.body.i
                                        #   Parent Loop BB23_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rax, %rcx
	movq	%rbx, %rdi
	jb	.LBB23_6
# BB#5:                                 # %for.body.i
                                        #   in Loop: Header=BB23_4 Depth=2
	movq	%rsi, %rdi
.LBB23_6:                               # %for.body.i
                                        #   in Loop: Header=BB23_4 Depth=2
	movzbl	mode_wider_mode(%rdx), %ebx
	testq	%rbx, %rbx
	movq	%rdi, %rsi
	jne	.LBB23_4
# BB#7:                                 # %for.end.i
                                        #   in Loop: Header=BB23_3 Depth=1
	testq	%r13, %rdi
	je	.LBB23_12
# BB#8:                                 # %if.end10.i
                                        #   in Loop: Header=BB23_3 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	movq	optab_table+240(%rip), %rax
	sarq	%cl, %rbx
	movq	$4, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB23_11
# BB#9:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB23_3 Depth=1
	callq	get_mode_alignment
	andq	%r13, %rax
	cmpq	$128, %rax
	ja	.LBB23_11
# BB#10:                                # %if.then17.i
                                        #   in Loop: Header=BB23_3 Depth=1
	movzbl	mode_size(%rbx), %ecx
	xorq	%rdx, %rdx
	movq	%r15, %rax
	divq	%rcx
	addq	%rax, 24(%rsp)          # 8-byte Folded Spill
	movq	%rdx, %r15
.LBB23_11:                              # %if.end24.i
                                        #   in Loop: Header=BB23_3 Depth=1
	movzbl	mode_size(%rbx), %eax
	cmpq	$1, %rax
	ja	.LBB23_3
	jmp	.LBB23_12
.LBB23_1:
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rdx, %r14
	movq	%r12, %r15
.LBB23_12:                              # %while.end.i
	testq	%r15, %r15
	jne	.LBB23_13
# BB#15:                                # %move_by_pieces_ninsns.exit
	movl	optimize_size(%rip), %eax
	testq	%rax, %rax
	movq	$3, %rcx
	jne	.LBB23_17
# BB#16:                                # %cond.false
	movq	ix86_cost(%rip), %rax
	movl	40(%rax), %ecx
.LBB23_17:                              # %cond.end
	movq	%r12, 16(%rsp)          # 8-byte Spill
	xorq	%rax, %rax
	cmpq	%rcx, 24(%rsp)          # 8-byte Folded Reload
	jae	.LBB23_33
# BB#18:                                # %for.body
	movl	target_flags(%rip), %eax
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	addq	$5, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	je	.LBB23_31
# BB#19:                                # %while.body.lr.ph
	xorq	%rbx, %rbx
	xorq	%r13, %r13
	.align	16, 0x90
.LBB23_20:                              # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB23_21 Depth 2
                                        #     Child Loop BB23_29 Depth 2
	movl	class_narrowest_mode+4(%rip), %ecx
	testq	%rcx, %rcx
	movq	%rcx, %rdx
	je	.LBB23_24
	.align	16, 0x90
.LBB23_21:                              # %for.body12
                                        #   Parent Loop BB23_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	mode_size(%rsi), %ecx
	cmpq	%rax, %rcx
	jb	.LBB23_23
# BB#22:                                # %for.body12
                                        #   in Loop: Header=BB23_21 Depth=2
	movq	%rbx, %rdx
.LBB23_23:                              # %for.body12
                                        #   in Loop: Header=BB23_21 Depth=2
	movq	%rdx, %rbx
	movzbl	mode_wider_mode(%rsi), %edx
	testq	%rdx, %rdx
	jne	.LBB23_21
.LBB23_24:                              # %for.end
                                        #   in Loop: Header=BB23_20 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB23_31
# BB#25:                                # %if.end24
                                        #   in Loop: Header=BB23_20 Depth=1
	movq	$32, %rcx
	movq	%rbx, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	movq	optab_table+240(%rip), %rax
	sarq	%cl, %r15
	movq	$4, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB23_26
# BB#27:                                # %land.lhs.true
                                        #   in Loop: Header=BB23_20 Depth=1
	movq	%rbx, %rdi
	callq	get_mode_alignment
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$128, %rax
	ja	.LBB23_26
# BB#28:                                # %if.then32
                                        #   in Loop: Header=BB23_20 Depth=1
	movq	%r15, (%rsp)            # 8-byte Spill
	movzbl	mode_size(%r15), %r15d
	movq	16(%rsp), %r12          # 8-byte Reload
	cmpq	%r15, %r12
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	8(%rsp), %r14           # 8-byte Reload
	jb	.LBB23_30
	.align	16, 0x90
.LBB23_29:                              # %while.body40
                                        #   Parent Loop BB23_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	*%r14
	addq	%r15, %r13
	subq	%r15, %r12
	cmpq	%r15, %r12
	jae	.LBB23_29
	jmp	.LBB23_30
	.align	16, 0x90
.LBB23_26:                              #   in Loop: Header=BB23_20 Depth=1
	movq	%r15, (%rsp)            # 8-byte Spill
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	16(%rsp), %r12          # 8-byte Reload
.LBB23_30:                              # %if.end53
                                        #   in Loop: Header=BB23_20 Depth=1
	movq	%r12, 16(%rsp)          # 8-byte Spill
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	(%rsp), %rax            # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	cmpq	$1, %rax
	ja	.LBB23_20
.LBB23_31:                              # %while.end57
	movq	$1, %rax
	cmpq	$0, 16(%rsp)            # 8-byte Folded Reload
	jne	.LBB23_32
.LBB23_33:                              # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB23_13:                              # %if.then28.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.move_by_pieces_ninsns, %rcx
	movq	$1542, %rsi             # imm = 0x606
	jmp	.LBB23_14
.LBB23_32:                              # %if.then60
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.can_store_by_pieces, %rcx
	movq	$2378, %rsi             # imm = 0x94A
.LBB23_14:                              # %if.then28.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp299:
	.size	can_store_by_pieces, .Ltmp299-can_store_by_pieces
	.cfi_endproc

	.globl	store_by_pieces
	.align	16, 0x90
	.type	store_by_pieces,@function
store_by_pieces:                        # @store_by_pieces
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp306:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp307:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp308:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp309:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp310:
	.cfi_def_cfa_offset 48
	subq	$112, %rsp
.Ltmp311:
	.cfi_def_cfa_offset 160
.Ltmp312:
	.cfi_offset %rbx, -48
.Ltmp313:
	.cfi_offset %r12, -40
.Ltmp314:
	.cfi_offset %r13, -32
.Ltmp315:
	.cfi_offset %r14, -24
.Ltmp316:
	.cfi_offset %r15, -16
	movl	class_narrowest_mode+4(%rip), %r15d
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	testq	%r15, %r15
	je	.LBB24_1
# BB#2:
	movq	$17, %rax
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%rsi, %r14
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB24_3:                               # %for.body.lr.ph.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB24_4 Depth 2
	movq	%r15, %rbx
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB24_4:                               # %for.body.i
                                        #   Parent Loop BB24_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rax, %rcx
	movq	%rbx, %rdi
	jb	.LBB24_6
# BB#5:                                 # %for.body.i
                                        #   in Loop: Header=BB24_4 Depth=2
	movq	%rsi, %rdi
.LBB24_6:                               # %for.body.i
                                        #   in Loop: Header=BB24_4 Depth=2
	movzbl	mode_wider_mode(%rdx), %ebx
	testq	%rbx, %rbx
	movq	%rdi, %rsi
	jne	.LBB24_4
# BB#7:                                 # %for.end.i
                                        #   in Loop: Header=BB24_3 Depth=1
	testq	%r12, %rdi
	je	.LBB24_12
# BB#8:                                 # %if.end10.i
                                        #   in Loop: Header=BB24_3 Depth=1
	movq	$32, %rcx
	movq	%rdi, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	movq	optab_table+240(%rip), %rax
	sarq	%cl, %r13
	movq	$4, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB24_11
# BB#9:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB24_3 Depth=1
	callq	get_mode_alignment
	andq	%r12, %rax
	cmpq	$128, %rax
	ja	.LBB24_11
# BB#10:                                # %if.then17.i
                                        #   in Loop: Header=BB24_3 Depth=1
	movzbl	mode_size(%r13), %ecx
	xorq	%rdx, %rdx
	movq	%r14, %rax
	divq	%rcx
	addq	%rax, 40(%rsp)          # 8-byte Folded Spill
	movq	%rdx, %r14
.LBB24_11:                              # %if.end24.i
                                        #   in Loop: Header=BB24_3 Depth=1
	movzbl	mode_size(%r13), %eax
	cmpq	$1, %rax
	ja	.LBB24_3
	jmp	.LBB24_12
.LBB24_1:
	movq	%rsi, %r14
	movq	%rsi, 32(%rsp)          # 8-byte Spill
.LBB24_12:                              # %while.end.i
	testq	%r14, %r14
	jne	.LBB24_13
# BB#15:                                # %move_by_pieces_ninsns.exit
	movl	optimize_size(%rip), %eax
	testq	%rax, %rax
	movq	$3, %rax
	jne	.LBB24_17
# BB#16:                                # %cond.false
	movq	ix86_cost(%rip), %rax
	movl	40(%rax), %eax
.LBB24_17:                              # %cond.end
	cmpq	%rax, 40(%rsp)          # 8-byte Folded Reload
	jae	.LBB24_18
# BB#19:                                # %if.end
	movq	$1, %rsi
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	protect_from_queue
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 88(%rsp)
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 96(%rsp)
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 72(%rsp)
	movq	%rax, 48(%rsp)
	leaq	48(%rsp), %rdi
	callq	store_by_pieces_1
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB24_13:                              # %if.then28.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.move_by_pieces_ninsns, %rcx
	movq	$1542, %rsi             # imm = 0x606
	jmp	.LBB24_14
.LBB24_18:                              # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.store_by_pieces, %rcx
	movq	$2400, %rsi             # imm = 0x960
.LBB24_14:                              # %if.then28.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp317:
	.size	store_by_pieces, .Ltmp317-store_by_pieces
	.cfi_endproc

	.align	16, 0x90
	.type	store_by_pieces_1,@function
store_by_pieces_1:                      # @store_by_pieces_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp324:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp325:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp326:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp327:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp328:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp329:
	.cfi_def_cfa_offset 80
.Ltmp330:
	.cfi_offset %rbx, -48
.Ltmp331:
	.cfi_offset %r12, -40
.Ltmp332:
	.cfi_offset %r13, -32
.Ltmp333:
	.cfi_offset %r14, -24
.Ltmp334:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	(%r14), %rax
	movq	8(%rax), %rbx
	movl	target_flags(%rip), %r13d
	movq	$23, %rcx
	shrq	%cl, %r13
	andq	$4, %r13
	addq	$5, %r13
	movq	$0, 32(%r14)
	movq	%rbx, 8(%r14)
	movl	(%rbx), %eax
	andq	$65532, %rax            # imm = 0xFFFC
	cmpq	$96, %rax
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, %rsi
	jne	.LBB25_2
# BB#1:                                 # %entry
	movq	%rdx, %rsi
.LBB25_2:                               # %entry
	movl	%esi, 16(%r14)
	xorq	%rsi, %rsi
	movl	%esi, 20(%r14)
	movl	(%rbx), %esi
	movq	%r13, %r15
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %r15
	andq	$65533, %rsi            # imm = 0xFFFD
	cmpq	$96, %rsi
	jne	.LBB25_4
# BB#3:                                 # %entry
	movq	%rdx, %rcx
.LBB25_4:                               # %entry
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movl	%ecx, 56(%r14)
	jne	.LBB25_6
# BB#5:                                 # %if.then
	movq	24(%r14), %rcx
	movq	%rcx, 32(%r14)
.LBB25_6:                               # %if.end
	xorq	%rbx, %rbx
	cmpq	$96, %rax
	je	.LBB25_34
# BB#7:                                 # %land.lhs.true
	movl	class_narrowest_mode+4(%rip), %r12d
	movq	24(%r14), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	testq	%r12, %r12
	je	.LBB25_18
# BB#8:
	movq	$17, %rax
	.align	16, 0x90
.LBB25_9:                               # %for.body.lr.ph.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB25_10 Depth 2
	movq	%r12, %rbx
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB25_10:                              # %for.body.i
                                        #   Parent Loop BB25_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rax, %rcx
	movq	%rbx, %rdi
	jb	.LBB25_12
# BB#11:                                # %for.body.i
                                        #   in Loop: Header=BB25_10 Depth=2
	movq	%rsi, %rdi
.LBB25_12:                              # %for.body.i
                                        #   in Loop: Header=BB25_10 Depth=2
	movzbl	mode_wider_mode(%rdx), %ebx
	testq	%rbx, %rbx
	movq	%rdi, %rsi
	jne	.LBB25_10
# BB#13:                                # %for.end.i
                                        #   in Loop: Header=BB25_9 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdi
	je	.LBB25_18
# BB#14:                                # %if.end10.i
                                        #   in Loop: Header=BB25_9 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	movq	optab_table+240(%rip), %rax
	sarq	%cl, %rbx
	movq	$4, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB25_17
# BB#15:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB25_9 Depth=1
	callq	get_mode_alignment
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$128, %rax
	ja	.LBB25_17
# BB#16:                                # %if.then17.i
                                        #   in Loop: Header=BB25_9 Depth=1
	movzbl	mode_size(%rbx), %ecx
	xorq	%rdx, %rdx
	movq	24(%rsp), %rax          # 8-byte Reload
	divq	%rcx
	addq	%rax, 16(%rsp)          # 8-byte Folded Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
.LBB25_17:                              # %if.end24.i
                                        #   in Loop: Header=BB25_9 Depth=1
	movzbl	mode_size(%rbx), %eax
	cmpq	$1, %rax
	ja	.LBB25_9
.LBB25_18:                              # %while.end.i
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	jne	.LBB25_19
# BB#21:                                # %move_by_pieces_ninsns.exit
	xorq	%rbx, %rbx
	cmpq	$3, 16(%rsp)            # 8-byte Folded Reload
	jb	.LBB25_34
# BB#22:                                # %if.then35
	xorq	%rbx, %rbx
	jmp	.LBB25_23
	.align	16, 0x90
.LBB25_26:                              # %for.body
                                        #   in Loop: Header=BB25_23 Depth=1
	movq	%r12, %rbx
	movzbl	mode_wider_mode(%rax), %r12d
.LBB25_23:                              # %if.then35
                                        # =>This Inner Loop Header: Depth=1
	testq	%r12, %r12
	je	.LBB25_27
# BB#24:                                # %for.body
                                        #   in Loop: Header=BB25_23 Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %ecx
	cmpq	%r15, %rcx
	jb	.LBB25_26
# BB#25:                                # %for.body
                                        #   in Loop: Header=BB25_23 Depth=1
	movq	%rbx, %r12
	jmp	.LBB25_26
.LBB25_27:                              # %for.end
	movl	16(%r14), %eax
	testq	%rax, %rax
	jne	.LBB25_34
# BB#28:                                # %land.lhs.true49
	movq	8(%rsp), %rdi           # 8-byte Reload
	movzbl	(%rdi), %ecx
	cmpq	$133, %rcx
	jg	.LBB25_31
# BB#29:                                # %land.lhs.true49
	addq	$-54, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB25_34
# BB#30:                                # %land.lhs.true49
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB25_33
	jmp	.LBB25_34
.LBB25_31:                              # %land.lhs.true49
	cmpq	$134, %rcx
	je	.LBB25_33
# BB#32:                                # %land.lhs.true49
	cmpq	$140, %rcx
	jne	.LBB25_34
.LBB25_33:                              # %if.then89
	callq	copy_addr_to_reg
	movq	%rax, 8(%r14)
.LBB25_34:                              # %while.cond.preheader
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r13
	cmpq	$1, %r13
	jne	.LBB25_36
# BB#35:                                # %while.cond.preheader.while.end_crit_edge
	addq	$24, %r14
	jmp	.LBB25_56
.LBB25_36:                              # %while.body.lr.ph
	leaq	24(%r14), %r13
	.align	16, 0x90
.LBB25_37:                              # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB25_38 Depth 2
                                        #     Child Loop BB25_46 Depth 2
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	class_narrowest_mode+4(%rip), %ecx
	testq	%rax, %rcx
	movq	%rcx, %rax
	je	.LBB25_41
	.align	16, 0x90
.LBB25_38:                              # %for.body99
                                        #   Parent Loop BB25_37 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%r15, %rcx
	jb	.LBB25_40
# BB#39:                                # %for.body99
                                        #   in Loop: Header=BB25_38 Depth=2
	movq	%rbx, %rax
.LBB25_40:                              # %for.body99
                                        #   in Loop: Header=BB25_38 Depth=2
	movq	%rax, %rbx
	movzbl	mode_wider_mode(%rdx), %eax
	testq	%rax, %rax
	jne	.LBB25_38
.LBB25_41:                              # %for.end111
                                        #   in Loop: Header=BB25_37 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB25_55
# BB#42:                                # %if.end115
                                        #   in Loop: Header=BB25_37 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movq	optab_table+240(%rip), %rax
	sarq	%cl, %rdx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	$4, %rcx
	shlq	%cl, %rdx
	movslq	8(%rax,%rdx), %r15
	cmpq	$1317, %r15             # imm = 0x525
	je	.LBB25_54
# BB#43:                                # %land.lhs.true120
                                        #   in Loop: Header=BB25_37 Depth=1
	movq	%rbx, %rdi
	callq	get_mode_alignment
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$128, %rax
	ja	.LBB25_54
# BB#44:                                # %if.then124
                                        #   in Loop: Header=BB25_37 Depth=1
	movq	(%r13), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movzbl	mode_size(%rcx), %r12d
	cmpq	%r12, %rax
	jb	.LBB25_54
# BB#45:                                # %while.body.lr.ph.i
                                        #   in Loop: Header=BB25_37 Depth=1
	leaq	(%r15,%r15,4), %rax
	movq	insn_data+16(,%rax,8), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movl	56(%r14), %eax
	.align	16, 0x90
.LBB25_46:                              # %while.body.i
                                        #   Parent Loop BB25_37 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB25_48
# BB#47:                                # %if.then.i
                                        #   in Loop: Header=BB25_46 Depth=2
	subq	%r12, 32(%r14)
.LBB25_48:                              # %if.end.i
                                        #   in Loop: Header=BB25_46 Depth=2
	movl	16(%r14), %eax
	movq	(%r14), %rdi
	testq	%rax, %rax
	jne	.LBB25_49
# BB#50:                                # %if.else.i
                                        #   in Loop: Header=BB25_46 Depth=2
	movq	32(%r14), %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%rbx, %rsi
	callq	adjust_address_1
	jmp	.LBB25_51
	.align	16, 0x90
.LBB25_49:                              # %if.then5.i
                                        #   in Loop: Header=BB25_46 Depth=2
	movq	8(%r14), %rdx
	movq	32(%r14), %rcx
	movq	$1, %r8
	movq	%rbx, %rsi
	callq	adjust_automodify_address_1
.LBB25_51:                              # %if.end10.i6
                                        #   in Loop: Header=BB25_46 Depth=2
	movq	%rax, %r15
	movq	32(%r14), %rsi
	movq	48(%r14), %rdi
	movq	%rbx, %rdx
	callq	*40(%r14)
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rcx, %rsi
	callq	*24(%rsp)               # 8-byte Folded Reload
	movq	%rax, %rdi
	callq	emit_insn
	movl	56(%r14), %eax
	testq	%rax, %rax
	jne	.LBB25_53
# BB#52:                                # %if.then17.i8
                                        #   in Loop: Header=BB25_46 Depth=2
	addq	%r12, 32(%r14)
.LBB25_53:                              # %if.end20.i
                                        #   in Loop: Header=BB25_46 Depth=2
	movq	(%r13), %rcx
	subq	%r12, %rcx
	cmpq	%r12, %rcx
	movq	%rcx, (%r13)
	jae	.LBB25_46
	.align	16, 0x90
.LBB25_54:                              # %if.end127
                                        #   in Loop: Header=BB25_37 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	movzbl	mode_size(%rax), %r15d
	cmpq	$1, %r15
	ja	.LBB25_37
.LBB25_55:
	movq	%r13, %r14
.LBB25_56:                              # %while.end
	cmpq	$0, (%r14)
	jne	.LBB25_57
# BB#58:                                # %if.end135
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB25_19:                              # %if.then28.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.move_by_pieces_ninsns, %rcx
	movq	$1542, %rsi             # imm = 0x606
	jmp	.LBB25_20
.LBB25_57:                              # %if.then134
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.store_by_pieces_1, %rcx
	movq	$2524, %rsi             # imm = 0x9DC
.LBB25_20:                              # %if.then28.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp335:
	.size	store_by_pieces_1, .Ltmp335-store_by_pieces_1
	.cfi_endproc

	.globl	clear_storage
	.align	16, 0x90
	.type	clear_storage,@function
clear_storage:                          # @clear_storage
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp342:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp343:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp344:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp345:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp346:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp347:
	.cfi_def_cfa_offset 144
.Ltmp348:
	.cfi_offset %rbx, -48
.Ltmp349:
	.cfi_offset %r12, -40
.Ltmp350:
	.cfi_offset %r13, -32
.Ltmp351:
	.cfi_offset %r14, -24
.Ltmp352:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	(%rbx), %edi
	movq	%rsi, %r15
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB26_3
# BB#1:                                 # %cond.true
	movq	16(%rbx), %rcx
	testq	%rcx, %rcx
	movq	$8, %rax
	je	.LBB26_4
# BB#2:                                 # %cond.true3
	movl	32(%rcx), %eax
	jmp	.LBB26_4
.LBB26_3:                               # %cond.false8
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	callq	get_mode_alignment
	movl	(%rbx), %edi
.LBB26_4:                               # %cond.end11
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	cmpq	$51, %rdi
	je	.LBB26_8
# BB#5:                                 # %land.lhs.true
	movzbl	(%r15), %eax
	cmpq	$54, %rax
	jne	.LBB26_8
# BB#6:                                 # %land.lhs.true20
	movzbl	mode_size(%rdi), %eax
	movl	8(%r15), %ecx
	cmpq	%rcx, %rax
	jne	.LBB26_8
# BB#7:                                 # %if.then
	movq	const_tiny_rtx(,%rdi,8), %rsi
	movq	%rbx, %rdi
	callq	emit_move_insn
	xorq	%rbx, %rbx
	jmp	.LBB26_46
.LBB26_8:                               # %if.else
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	protect_from_queue
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	movq	%r15, %rdi
	callq	protect_from_queue
	movq	%rax, %r13
	movzbl	(%r13), %eax
	cmpq	$54, %rax
	jne	.LBB26_9
# BB#24:                                # %land.lhs.true42
	movl	class_narrowest_mode+4(%rip), %r12d
	movq	%r13, (%rsp)            # 8-byte Spill
	movq	8(%r13), %r13
	testq	%r12, %r12
	je	.LBB26_35
# BB#25:
	movq	$17, %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB26_26:                              # %for.body.lr.ph.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB26_27 Depth 2
	movq	%r12, %rbx
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB26_27:                              # %for.body.i
                                        #   Parent Loop BB26_26 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rax, %rcx
	movq	%rbx, %rdi
	jb	.LBB26_29
# BB#28:                                # %for.body.i
                                        #   in Loop: Header=BB26_27 Depth=2
	movq	%rsi, %rdi
.LBB26_29:                              # %for.body.i
                                        #   in Loop: Header=BB26_27 Depth=2
	movzbl	mode_wider_mode(%rdx), %ebx
	testq	%rbx, %rbx
	movq	%rdi, %rsi
	jne	.LBB26_27
# BB#30:                                # %for.end.i
                                        #   in Loop: Header=BB26_26 Depth=1
	testq	%r15, %rdi
	je	.LBB26_35
# BB#31:                                # %if.end10.i
                                        #   in Loop: Header=BB26_26 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	movq	optab_table+240(%rip), %rax
	sarq	%cl, %rbx
	movq	$4, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB26_34
# BB#32:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB26_26 Depth=1
	callq	get_mode_alignment
	andq	%r15, %rax
	cmpq	$128, %rax
	ja	.LBB26_34
# BB#33:                                # %if.then17.i
                                        #   in Loop: Header=BB26_26 Depth=1
	movzbl	mode_size(%rbx), %ecx
	xorq	%rdx, %rdx
	movq	%r13, %rax
	divq	%rcx
	addq	%rax, %r14
	movq	%rdx, %r13
.LBB26_34:                              # %if.end24.i
                                        #   in Loop: Header=BB26_26 Depth=1
	movzbl	mode_size(%rbx), %eax
	cmpq	$1, %rax
	ja	.LBB26_26
.LBB26_35:                              # %while.end.i
	testq	%r13, %r13
	jne	.LBB26_47
# BB#36:                                # %move_by_pieces_ninsns.exit
	movl	optimize_size(%rip), %eax
	testq	%rax, %rax
	movq	$3, %rax
	movq	(%rsp), %r13            # 8-byte Reload
	jne	.LBB26_38
# BB#37:                                # %cond.false48
	movq	ix86_cost(%rip), %rax
	movl	40(%rax), %eax
.LBB26_38:                              # %cond.end49
	cmpq	%rax, %r14
	jae	.LBB26_10
# BB#39:                                # %if.then54
	movq	8(%r13), %rax
	movq	$clear_by_pieces_1, 72(%rsp)
	movq	$0, 80(%rsp)
	movq	%rax, 56(%rsp)
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, 32(%rsp)
	leaq	32(%rsp), %rdi
	callq	store_by_pieces_1
	xorq	%rbx, %rbx
	jmp	.LBB26_46
.LBB26_9:                               # %if.else.if.else58_crit_edge
	movl	class_narrowest_mode+4(%rip), %r12d
.LBB26_10:                              # %if.else58
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	movq	16(%rsp), %rsi          # 8-byte Reload
	andq	%rax, %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	je	.LBB26_42
	.align	16, 0x90
.LBB26_11:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movslq	clrstr_optab(,%rbx,4), %rax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB26_41
# BB#12:                                # %land.lhs.true67
                                        #   in Loop: Header=BB26_11 Depth=1
	movzbl	(%r13), %ecx
	cmpq	$54, %rcx
	jne	.LBB26_14
# BB#13:                                # %land.lhs.true72
                                        #   in Loop: Header=BB26_11 Depth=1
	movq	mode_mask_array(,%rbx,8), %rdx
	movq	8(%r13), %rsi
	movq	$1, %rcx
	shrq	%cl, %rdx
	cmpq	%rdx, %rsi
	jbe	.LBB26_15
.LBB26_14:                              # %lor.lhs.false
                                        #   in Loop: Header=BB26_11 Depth=1
	movl	target_flags(%rip), %edx
	movzbl	mode_bitsize(%rbx,%rbx), %esi
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rcx, %rdx
	cmpq	%rdx, %rsi
	jb	.LBB26_41
.LBB26_15:                              # %land.lhs.true87
                                        #   in Loop: Header=BB26_11 Depth=1
	leaq	(%rax,%rax,4), %r15
	movq	insn_data+24(,%r15,8), %r14
	movq	(%r14), %rax
	testq	%rax, %rax
	je	.LBB26_17
# BB#16:                                # %lor.lhs.false93
                                        #   in Loop: Header=BB26_11 Depth=1
	movq	$51, %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	*%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB26_41
.LBB26_17:                              # %land.lhs.true96
                                        #   in Loop: Header=BB26_11 Depth=1
	movq	48(%r14), %rax
	testq	%rax, %rax
	je	.LBB26_19
# BB#18:                                # %lor.lhs.false104
                                        #   in Loop: Header=BB26_11 Depth=1
	xorq	%rsi, %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	*%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB26_41
.LBB26_19:                              # %if.then107
                                        #   in Loop: Header=BB26_11 Depth=1
	callq	get_last_insn
	movq	%r13, %rdx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%rsi, %rsi
	movq	$1, %rcx
	movq	%r12, %rdi
                                        # kill: R13<def> RDX<kill>
	callq	convert_modes
	movq	24(%r14), %rcx
	movq	%rax, %r14
	testq	%rcx, %rcx
	je	.LBB26_22
# BB#20:                                # %land.lhs.true117
                                        #   in Loop: Header=BB26_11 Depth=1
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	*%rcx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB26_22
# BB#21:                                # %if.then120
                                        #   in Loop: Header=BB26_11 Depth=1
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %r14
.LBB26_22:                              # %if.end
                                        #   in Loop: Header=BB26_11 Depth=1
	xorq	%rax, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	16(%rsp), %rdx          # 8-byte Reload
	callq	*insn_data+16(,%r15,8)
	testq	%rax, %rax
	jne	.LBB26_23
# BB#40:                                # %if.else128
                                        #   in Loop: Header=BB26_11 Depth=1
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	delete_insns_since
	.align	16, 0x90
.LBB26_41:                              # %for.inc
                                        #   in Loop: Header=BB26_11 Depth=1
	movzbl	mode_wider_mode(%rbx), %r12d
	testq	%r12, %r12
	jne	.LBB26_11
.LBB26_42:                              # %for.end
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	copy_to_mode_reg
	movq	sizetype_tab(%rip), %rcx
	movl	60(%rcx), %edi
	movq	%rax, %r12
	movq	$9, %rcx
	shrq	%cl, %rdi
	andq	$127, %rdi
	movq	%r13, %rsi
	callq	copy_to_mode_reg
	cmpq	$0, clear_storage.fn(%rip)
	movq	%rax, %r14
	jne	.LBB26_44
# BB#43:                                # %if.then147
	movabsq	$.L.str2, %rdi
	callq	get_identifier
	movq	global_trees+216(%rip), %rdi
	movq	%rax, clear_storage.fn(%rip)
	movabsq	$clear_storage.fn, %rbx
	callq	build_pointer_type
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	build_function_type
	movq	clear_storage.fn(%rip), %rsi
	movq	$30, %rdi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, clear_storage.fn(%rip)
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	ggc_add_tree_root
	movq	clear_storage.fn(%rip), %rax
	orq	$256, 48(%rax)          # imm = 0x100
	movq	clear_storage.fn(%rip), %rax
	movl	16(%rax), %ecx
	orq	$524288, %rcx           # imm = 0x80000
	movl	%ecx, 16(%rax)
	movq	clear_storage.fn(%rip), %rax
	orq	$4194304, 48(%rax)      # imm = 0x400000
	movq	clear_storage.fn(%rip), %rax
	movl	16(%rax), %ecx
	orq	$131072, %rcx           # imm = 0x20000
	movl	%ecx, 16(%rax)
	movq	clear_storage.fn(%rip), %rdi
	xorq	%rsi, %rsi
	callq	make_decl_rtl
	movq	clear_storage.fn(%rip), %rdi
	callq	assemble_external
.LBB26_44:                              # %if.end165
	movq	global_trees+216(%rip), %rdi
	callq	build_pointer_type
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	make_tree
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	build_tree_list
	movq	integer_types+40(%rip), %rdi
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rax, %rbx
	callq	make_tree
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	build_tree_list
	movq	%rax, (%rbx)
	movq	sizetype_tab(%rip), %rdi
	movq	%r14, %rsi
	callq	make_tree
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	build_tree_list
	movq	(%rbx), %rcx
	movq	%rax, (%rcx)
	movq	(%rbx), %rax
	movq	(%rax), %rax
	movq	$0, (%rax)
	movq	clear_storage.fn(%rip), %rax
	movq	8(%rax), %rdi
	callq	build_pointer_type
	movq	clear_storage.fn(%rip), %rdx
	movq	$121, %rdi
	movq	%rax, %rsi
	callq	build1
	movq	clear_storage.fn(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rsi
	movq	%rax, %rcx
	movq	$53, %rdi
	xorq	%r8, %r8
	xorq	%rax, %rax
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	build
	movl	16(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movzbl	3(%r12), %ecx
	movq	%rax, %rbx
	testq	$4, %rcx
	je	.LBB26_46
# BB#45:                                # %if.then202
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
.LBB26_46:                              # %return
	movq	%rbx, %rax
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB26_23:                              # %if.then126
	movq	%rax, %rdi
	callq	emit_insn
	xorq	%rbx, %rbx
	jmp	.LBB26_46
.LBB26_47:                              # %if.then28.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.move_by_pieces_ninsns, %rcx
	movq	$1542, %rsi             # imm = 0x606
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp353:
	.size	clear_storage, .Ltmp353-clear_storage
	.cfi_endproc

	.globl	emit_move_insn_1
	.align	16, 0x90
	.type	emit_move_insn_1,@function
emit_move_insn_1:                       # @emit_move_insn_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp360:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp361:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp362:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp363:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp364:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp365:
	.cfi_def_cfa_offset 96
.Ltmp366:
	.cfi_offset %rbx, -48
.Ltmp367:
	.cfi_offset %r12, -40
.Ltmp368:
	.cfi_offset %r13, -32
.Ltmp369:
	.cfi_offset %r14, -24
.Ltmp370:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movzbl	2(%r15), %eax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%rsi, %r12
	cmpq	$58, %rax
	ja	.LBB27_6
# BB#1:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
.LBB27_2:                               # %if.end.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB27_3 Depth 2
	movq	%r15, 24(%rsp)          # 8-byte Spill
	movq	%r12, %rsi
.LBB27_3:                               # %if.end
                                        #   Parent Loop BB27_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	40(%rsp), %r13          # 8-byte Reload
	movq	optab_table+240(%rip), %rax
	andq	%r14, %r13
	movq	$4, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movslq	8(%rax,%rdx), %rax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB27_4
# BB#8:                                 # %if.else
                                        #   in Loop: Header=BB27_3 Depth=2
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movl	mode_class(,%r13,4), %ebx
	leaq	-5(%rbx), %rax
	andq	%r14, %rax
	cmpq	$1, %rax
	ja	.LBB27_46
# BB#9:                                 # %land.lhs.true
                                        #   in Loop: Header=BB27_3 Depth=2
	andq	%r14, %rbx
	cmpq	$5, %rbx
	movabsq	$0, %rsi
	jne	.LBB27_11
# BB#10:                                # %land.lhs.true
                                        #   in Loop: Header=BB27_3 Depth=2
	movabsq	$1, %rsi
.LBB27_11:                              # %land.lhs.true
                                        #   in Loop: Header=BB27_3 Depth=2
	movzbl	mode_unit_size(%r13), %edi
	movq	$3, %rcx
	shlq	%cl, %rdi
	xorq	$1, %rsi
	incq	%rsi
	xorq	%rdx, %rdx
	callq	mode_for_size
	movq	%rax, %rcx
	andq	%r14, %rcx
	cmpq	$51, %rcx
	je	.LBB27_46
# BB#12:                                # %land.lhs.true21
                                        #   in Loop: Header=BB27_3 Depth=2
	movq	$32, %rcx
	movq	%rax, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	movq	optab_table+240(%rip), %rsi
	sarq	%cl, %r12
	movq	$4, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movl	8(%rsi,%rdx), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	je	.LBB27_46
# BB#13:                                # %if.then28
                                        #   in Loop: Header=BB27_3 Depth=2
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movzbl	2(%r15), %esi
	movq	%r15, %rdi
	callq	push_operand
	movl	target_flags(%rip), %edx
	testq	%r14, %rax
	jne	.LBB27_14
# BB#25:                                # %if.else132
                                        #   in Loop: Header=BB27_3 Depth=2
	movzbl	mode_bitsize(%r13,%r13), %edi
	movq	$19, %rcx
	shrq	%cl, %rdx
	andq	$64, %rdx
	addq	$64, %rdx
	movabsq	$4294967232, %rax       # imm = 0xFFFFFFC0
	andq	%rax, %rdx
	cmpq	%rdx, %rdi
	jae	.LBB27_37
# BB#26:                                # %land.lhs.true142
                                        #   in Loop: Header=BB27_3 Depth=2
	movl	reload_in_progress(%rip), %eax
	movl	reload_completed(%rip), %ecx
	orq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	jne	.LBB27_37
# BB#27:                                # %if.then145
                                        #   in Loop: Header=BB27_3 Depth=2
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %ecx
	xorq	%rax, %rax
	cmpq	$61, %rcx
	movabsq	$0, %r15
	jne	.LBB27_29
# BB#28:                                # %land.rhs
                                        #   in Loop: Header=BB27_3 Depth=2
	movq	24(%rsp), %rcx          # 8-byte Reload
	movl	8(%rcx), %ecx
	cmpq	$53, %rcx
	sbbq	%r15, %r15
	andq	$1, %r15
.LBB27_29:                              # %land.end
                                        #   in Loop: Header=BB27_3 Depth=2
	movq	32(%rsp), %rdx          # 8-byte Reload
	movzbl	(%rdx), %ecx
	cmpq	$61, %rcx
	jne	.LBB27_31
# BB#30:                                # %land.rhs158
                                        #   in Loop: Header=BB27_3 Depth=2
	movl	8(%rdx), %eax
	cmpq	$53, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
.LBB27_31:                              # %land.end164
                                        #   in Loop: Header=BB27_3 Depth=2
	orq	%r15, %rax
	cmpq	$1, %rax
	jne	.LBB27_37
# BB#32:                                # %if.then169
                                        #   in Loop: Header=BB27_3 Depth=2
	cmpq	$6, %rbx
	movabsq	$0, %rsi
	jne	.LBB27_34
# BB#33:                                # %if.then169
                                        #   in Loop: Header=BB27_3 Depth=2
	movabsq	$1, %rsi
.LBB27_34:                              # %if.then169
                                        #   in Loop: Header=BB27_3 Depth=2
	incq	%rsi
	movq	$1, %rdx
	callq	mode_for_size
	movq	%rax, %r14
                                        # kill: RAX<def> R14<kill>
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$51, %rax
	je	.LBB27_37
# BB#35:                                # %if.then179
                                        #   in Loop: Header=BB27_3 Depth=2
	movzbl	mode_size(%r13), %esi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	assign_stack_temp
	movq	%rax, %r12
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r12, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	adjust_address_1
	movq	cfun(%rip), %rcx
	movq	%rax, %rbx
	movq	$.L.str3, 136(%rcx)
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	testq	%r15, %r15
	jne	.LBB27_36
# BB#5:                                 # %if.else190
                                        #   in Loop: Header=BB27_3 Depth=2
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_SUBREG
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn_1
	movq	24(%rsp), %r15          # 8-byte Reload
	movzbl	2(%r15), %eax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	cmpq	$58, %rax
	movq	%rbx, %rsi
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jbe	.LBB27_3
	jmp	.LBB27_6
.LBB27_36:                              # %if.then186
                                        #   in Loop: Header=BB27_2 Depth=1
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_SUBREG
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	emit_move_insn_1
	movzbl	2(%r15), %eax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	cmpq	$58, %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jbe	.LBB27_2
.LBB27_6:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.emit_move_insn_1, %rcx
	movq	$2819, %rsi             # imm = 0xB03
	jmp	.LBB27_7
.LBB27_37:                              # %if.end197
	movq	16(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdi
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	gen_realpart
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	movq	32(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	callq	gen_realpart
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	gen_imagpart
	movq	%rax, %r14
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	gen_imagpart
	movq	%rax, %r13
	cmpq	%r15, %rbx
	movq	%r14, %r15
	movq	40(%rsp), %r14          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	je	.LBB27_43
# BB#38:                                # %if.end197
	movl	reload_in_progress(%rip), %eax
	testq	%rcx, %rax
	jne	.LBB27_43
# BB#39:                                # %if.end197
	movl	reload_completed(%rip), %eax
	testq	%rcx, %rax
	jne	.LBB27_43
# BB#40:                                # %land.lhs.true208
	movzbl	(%r14), %eax
	cmpq	$63, %rax
	je	.LBB27_42
# BB#41:                                # %lor.lhs.false213
	movzbl	(%r15), %eax
	cmpq	$63, %rax
	jne	.LBB27_43
.LBB27_42:                              # %if.then218
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
.LBB27_43:                              # %if.end221
	movq	optab_table+240(%rip), %rax
	movq	$4, %rcx
	shlq	%cl, %r12
	movslq	8(%rax,%r12), %rax
	leaq	(%rax,%rax,4), %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	*insn_data+16(,%rcx,8)
	movq	%rax, %rdi
	callq	emit_insn
	movq	optab_table+240(%rip), %rax
	movslq	8(%rax,%r12), %rax
	leaq	(%rax,%rax,4), %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	*insn_data+16(,%rcx,8)
.LBB27_44:                              # %if.end241
	movq	%rax, %rdi
	callq	emit_insn
.LBB27_45:                              # %if.end241
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	get_last_insn  # TAILCALL
.LBB27_46:                              # %if.else243
	movl	target_flags(%rip), %eax
	movzbl	mode_size(%r13), %edx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	addq	$4, %rax
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rax, %rcx
	cmpq	%rcx, %rdx
	jbe	.LBB27_101
# BB#47:                                # %if.then252
	movzbl	2(%r15), %esi
	movq	%r15, %rdi
	callq	push_operand
	testq	%r14, %rax
	je	.LBB27_60
# BB#48:                                # %if.then258
	movl	target_flags(%rip), %eax
	movq	%rax, %rbx
	andq	$33554432, %rbx         # imm = 0x2000000
	movzbl	2(%r15), %edx
	movq	$25, %rcx
	movq	optab_table+16(%rip), %r13
	movq	global_rtl+16(%rip), %r12
	movzbl	mode_size(%rdx), %esi
	shrq	%cl, %rbx
	orq	$4, %rbx
	andq	$33554432, %rax         # imm = 0x2000000
	je	.LBB27_50
# BB#49:                                # %cond.true265
	addq	$7, %rsi
	andq	$504, %rsi              # imm = 0x1F8
	jmp	.LBB27_51
.LBB27_4:                               # %if.then4
	leaq	(%rax,%rax,4), %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	*insn_data+16(,%rcx,8)
	movq	%rax, %rdi
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_insn  # TAILCALL
.LBB27_14:                              # %land.lhs.true33
	movzbl	mode_size(%r12), %ecx
	andq	$33554432, %rdx         # imm = 0x2000000
	je	.LBB27_16
# BB#15:                                # %cond.true
	leaq	7(%rcx), %rax
	andq	$504, %rax              # imm = 0x1F8
	jmp	.LBB27_17
.LBB27_50:                              # %cond.false274
	incq	%rsi
	andq	$510, %rsi              # imm = 0x1FE
.LBB27_51:                              # %cond.end283
	andq	%r14, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	global_rtl+16(%rip), %r8
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	expand_binop
	movq	global_rtl+16(%rip), %rdi
	cmpq	%rdi, %rax
	je	.LBB27_53
# BB#52:                                # %if.then290
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB27_53:                              # %if.end292
	movq	8(%r15), %rax
	movzbl	(%rax), %eax
	cmpq	$98, %rax
	jne	.LBB27_54
# BB#56:                                # %if.then315
	movzbl	2(%r15), %eax
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	movq	global_rtl+16(%rip), %r12
	movzbl	mode_size(%rax), %esi
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	jmp	.LBB27_57
.LBB27_54:                              # %if.end292
	cmpq	$99, %rax
	jne	.LBB27_58
# BB#55:                                # %if.then300
	movzbl	2(%r15), %eax
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	movzbl	mode_size(%rax), %esi
	movq	global_rtl+16(%rip), %r12
	andq	$1, %rbx
	orq	$4, %rbx
	negq	%rsi
.LBB27_57:                              # %if.end329
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdx
	jmp	.LBB27_59
.LBB27_58:                              # %if.else327
	movq	global_rtl+16(%rip), %rdx
.LBB27_59:                              # %if.end329
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	change_address
	movq	%rax, %r15
.LBB27_60:                              # %if.end331
	movl	reload_in_progress(%rip), %eax
	testq	%rax, %rax
	je	.LBB27_71
# BB#61:                                # %land.lhs.true333
	movzbl	(%r15), %eax
	cmpq	$66, %rax
	jne	.LBB27_64
# BB#62:                                # %land.lhs.true338
	leaq	8(%r15), %rdi
	callq	find_replacement
	movq	8(%r15), %rcx
	cmpq	%rcx, %rax
	je	.LBB27_64
# BB#63:                                # %if.then348
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	replace_equiv_address_nv
	movq	%rax, %r15
.LBB27_64:                              # %if.end350
	movq	32(%rsp), %rcx          # 8-byte Reload
	movl	reload_in_progress(%rip), %eax
	testq	%rax, %rax
	je	.LBB27_65
# BB#66:                                # %land.lhs.true352
	movzbl	(%rcx), %eax
	cmpq	$66, %rax
	jne	.LBB27_67
# BB#68:                                # %land.lhs.true357
	leaq	8(%rcx), %rdi
	movq	%rcx, %rbx
	callq	find_replacement
	movq	%rbx, %rdi
	movq	8(%rdi), %rcx
	cmpq	%rcx, %rax
	je	.LBB27_69
# BB#70:                                # %if.then367
	movq	%rax, %rsi
	callq	replace_equiv_address_nv
	movq	%rax, 32(%rsp)          # 8-byte Spill
	jmp	.LBB27_71
.LBB27_65:
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	jmp	.LBB27_71
.LBB27_67:
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	jmp	.LBB27_71
.LBB27_16:                              # %cond.false
	leaq	1(%rcx), %rax
	andq	$510, %rax              # imm = 0x1FE
.LBB27_17:                              # %cond.end
	andq	%r14, %rax
	cmpq	%rcx, %rax
	jne	.LBB27_18
# BB#24:                                # %if.then105
	movq	optab_table+240(%rip), %rax
	movq	$4, %rcx
	shlq	%cl, %r12
	movslq	8(%rax,%r12), %rax
	movq	8(%r15), %rsi
	leaq	(%rax,%rax,4), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	16(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdi
	callq	gen_rtx_MEM
	movq	%rax, %r14
	movq	%r13, %rdi
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	gen_imagpart
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rcx, %rsi
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	*insn_data+16(,%rcx,8)
	movq	%rax, %rdi
	callq	emit_insn
	movq	optab_table+240(%rip), %rax
	movslq	8(%rax,%r12), %rax
	movq	8(%r15), %rsi
	leaq	(%rax,%rax,4), %r15
	movq	%r13, %rdi
	callq	gen_rtx_MEM
	movq	%rax, %r14
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	gen_realpart
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rcx, %rsi
	callq	*insn_data+16(,%r15,8)
	jmp	.LBB27_44
.LBB27_18:                              # %if.then50
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	%rdx, %r12
	andq	%r14, %r12
	movzbl	2(%r15), %eax
	movq	$25, %rcx
	movq	%r14, %rdi
	movq	optab_table+16(%rip), %r14
	movq	global_rtl+16(%rip), %rbx
	movzbl	mode_size(%rax), %esi
	shrq	%cl, %r12
	orq	$4, %r12
	andq	%rdi, %rdx
	movq	%rdi, %rax
	je	.LBB27_20
# BB#19:                                # %cond.true56
	addq	$7, %rsi
	andq	$504, %rsi              # imm = 0x1F8
	jmp	.LBB27_21
.LBB27_69:
	movq	%rdi, 32(%rsp)          # 8-byte Spill
.LBB27_71:                              # %if.end369
	movq	%r15, 24(%rsp)          # 8-byte Spill
	callq	start_sequence
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	movq	16(%rsp), %rax          # 8-byte Reload
	leaq	3(%rsi,%rax), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	jle	.LBB27_72
# BB#73:
	xorq	%r14, %r14
	.align	16, 0x90
.LBB27_74:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rcx
	callq	operand_subword
	movq	%rax, %r12
	movq	$1, %rdx
	movq	32(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rcx
	callq	operand_subword
	testq	%rax, %rax
	jne	.LBB27_82
# BB#75:                                # %land.lhs.true387
                                        #   in Loop: Header=BB27_74 Depth=1
	movzbl	(%r13), %ecx
	cmpq	$133, %rcx
	movq	%r13, %rsi
	jg	.LBB27_79
# BB#76:                                # %land.lhs.true387
                                        #   in Loop: Header=BB27_74 Depth=1
	addq	$-54, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB27_78
# BB#77:                                # %land.lhs.true387
                                        #   in Loop: Header=BB27_74 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB27_81
	jmp	.LBB27_78
	.align	16, 0x90
.LBB27_79:                              # %land.lhs.true387
                                        #   in Loop: Header=BB27_74 Depth=1
	cmpq	$134, %rcx
	je	.LBB27_81
# BB#80:                                # %land.lhs.true387
                                        #   in Loop: Header=BB27_74 Depth=1
	cmpq	$140, %rcx
	jne	.LBB27_78
.LBB27_81:                              # %if.then427
                                        #   in Loop: Header=BB27_74 Depth=1
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	callq	force_const_mem
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rcx
	callq	operand_subword
	jmp	.LBB27_82
	.align	16, 0x90
.LBB27_78:                              # %if.then433
                                        #   in Loop: Header=BB27_74 Depth=1
	movq	%rsi, %rdi
	movq	%rbx, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	operand_subword_force
.LBB27_82:                              # %if.end436
                                        #   in Loop: Header=BB27_74 Depth=1
	testq	%r12, %r12
	je	.LBB27_84
# BB#83:                                # %if.end436
                                        #   in Loop: Header=BB27_74 Depth=1
	testq	%rax, %rax
	je	.LBB27_84
# BB#85:                                # %if.end443
                                        #   in Loop: Header=BB27_74 Depth=1
	movzbl	(%r12), %ecx
	cmpq	$63, %rcx
	movabsq	$0, %rcx
	movq	24(%rsp), %r15          # 8-byte Reload
	jne	.LBB27_87
# BB#86:                                # %if.end443
                                        #   in Loop: Header=BB27_74 Depth=1
	movabsq	$1, %rcx
.LBB27_87:                              # %if.end443
                                        #   in Loop: Header=BB27_74 Depth=1
	orq	%rcx, %r14
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	%rax, %r12
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movl	target_flags(%rip), %edi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	movq	16(%rsp), %rax          # 8-byte Reload
	leaq	3(%rdi,%rax), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rsi
	jl	.LBB27_74
# BB#88:                                # %for.cond.for.end_crit_edge
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	je	.LBB27_89
# BB#90:                                # %for.cond.for.end_crit_edge
	movabsq	$1, %rbx
	jmp	.LBB27_91
.LBB27_72:
	xorq	%r12, %r12
	jmp	.LBB27_91
.LBB27_89:
	movabsq	$0, %rbx
.LBB27_91:                              # %for.end
	callq	gen_sequence
	movq	%rax, %r13
	callq	end_sequence
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r15
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	je	.LBB27_93
# BB#92:                                # %for.end
	movq	%rdx, %rcx
.LBB27_93:                              # %for.end
	movl	reload_in_progress(%rip), %esi
	testq	%rsi, %rsi
	movq	%rax, %rsi
	jne	.LBB27_95
# BB#94:                                # %for.end
	movq	%rdx, %rsi
.LBB27_95:                              # %for.end
	movl	reload_completed(%rip), %edi
	testq	%rdi, %rdi
	jne	.LBB27_97
# BB#96:                                # %for.end
	movq	%rdx, %rax
.LBB27_97:                              # %for.end
	testq	%rbx, %rbx
	je	.LBB27_100
# BB#98:                                # %for.end
	andq	%rsi, %rcx
	andq	%rax, %rcx
	je	.LBB27_100
# BB#99:                                # %if.then460
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
.LBB27_100:                             # %return
	movq	%r13, %rdi
	callq	emit_insn
	movq	%r12, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB27_20:                              # %cond.false65
	incq	%rsi
	andq	$510, %rsi              # imm = 0x1FE
.LBB27_21:                              # %cond.end74
	andq	%rax, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	global_rtl+16(%rip), %r8
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	expand_binop
	movq	global_rtl+16(%rip), %rdi
	movq	%rax, %r14
	cmpq	%rdi, %r14
	je	.LBB27_23
# BB#22:                                # %if.then81
	movq	%r14, %rsi
	callq	emit_move_insn
	movq	global_rtl+16(%rip), %r14
.LBB27_23:                              # %if.end83
	movl	target_flags(%rip), %r12d
	movq	$25, %rcx
	shrq	%cl, %r12
	andq	$1, %r12
	orq	$4, %r12
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%r15, %rdi
	movq	16(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	change_address
	movq	%rax, %r14
	movq	%r13, %rdi
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	gen_realpart
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movl	target_flags(%rip), %r12d
	movq	$25, %rcx
	movq	global_rtl+16(%rip), %r14
	shrq	%cl, %r12
	andq	$1, %r12
	orq	$4, %r12
	xorq	%rdi, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	change_address
	movq	%rax, %r14
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	gen_imagpart
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	jmp	.LBB27_45
.LBB27_101:                             # %if.else465
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.emit_move_insn_1, %rcx
	movq	$3101, %rsi             # imm = 0xC1D
	jmp	.LBB27_7
.LBB27_84:                              # %if.then442
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.emit_move_insn_1, %rcx
	movq	$3077, %rsi             # imm = 0xC05
.LBB27_7:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp371:
	.size	emit_move_insn_1, .Ltmp371-emit_move_insn_1
	.cfi_endproc

	.globl	push_block
	.align	16, 0x90
	.type	push_block,@function
push_block:                             # @push_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp378:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp379:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp380:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp381:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp382:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp383:
	.cfi_def_cfa_offset 64
.Ltmp384:
	.cfi_offset %rbx, -48
.Ltmp385:
	.cfi_offset %r12, -40
.Ltmp386:
	.cfi_offset %r13, -32
.Ltmp387:
	.cfi_offset %r14, -24
.Ltmp388:
	.cfi_offset %r15, -16
	movl	target_flags(%rip), %eax
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %rdx
	movq	$25, %rcx
	movl	ptr_mode(%rip), %esi
	shrq	%cl, %rax
	andq	$1, %rax
	orq	$4, %rax
	movq	$1, %rcx
	movq	%rax, %rdi
	callq	convert_modes
	movzbl	(%rax), %edx
	cmpq	$133, %rdx
	jg	.LBB28_8
# BB#1:                                 # %entry
	leaq	-54(%rdx), %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	ja	.LBB28_3
# BB#2:                                 # %entry
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	jne	.LBB28_10
	jmp	.LBB28_3
.LBB28_8:                               # %entry
	cmpq	$134, %rdx
	je	.LBB28_10
# BB#9:                                 # %entry
	cmpq	$140, %rdx
	jne	.LBB28_3
.LBB28_10:                              # %if.then
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rax, %rdi
	callq	plus_constant_wide
	jmp	.LBB28_11
.LBB28_3:                               # %if.else
	cmpq	$61, %rdx
	jne	.LBB28_5
# BB#4:                                 # %if.else
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rbx
	jne	.LBB28_5
.LBB28_11:                              # %if.then35
	movq	%rax, %rdi
	jmp	.LBB28_12
.LBB28_5:                               # %if.else36
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%rax, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB28_7
# BB#6:                                 # %if.then43
	movl	target_flags(%rip), %r12d
	movq	$25, %rcx
	shrq	%cl, %r12
	andq	$1, %r12
	orq	$4, %r12
	movq	optab_table(%rip), %r13
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	movq	%r15, %r8
	callq	expand_binop
	movq	%rax, %r15
.LBB28_7:                               # %if.end
	movq	%r15, %rdi
.LBB28_12:                              # %if.end51
	callq	anti_adjust_stack
	movq	global_rtl+72(%rip), %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB28_15
# BB#13:                                # %if.end51
	testq	%rax, %r14
	je	.LBB28_15
# BB#14:                                # %if.then56
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rsi, %rdi
	movq	%rbx, %rsi
	callq	plus_constant_wide
	movq	%rax, %rsi
.LBB28_15:                              # %if.end59
	movl	class_narrowest_mode+4(%rip), %edi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	memory_address  # TAILCALL
.Ltmp389:
	.size	push_block, .Ltmp389-push_block
	.cfi_endproc

	.globl	emit_push_insn
	.align	16, 0x90
	.type	emit_push_insn,@function
emit_push_insn:                         # @emit_push_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp396:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp397:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp398:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp399:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp400:
	.cfi_def_cfa_offset 48
	subq	$128, %rsp
.Ltmp401:
	.cfi_def_cfa_offset 176
.Ltmp402:
	.cfi_offset %rbx, -48
.Ltmp403:
	.cfi_offset %r12, -40
.Ltmp404:
	.cfi_offset %r13, -32
.Ltmp405:
	.cfi_offset %r14, -24
.Ltmp406:
	.cfi_offset %r15, -16
	movq	%r9, %r12
	movq	%r8, 120(%rsp)          # 8-byte Spill
	movq	%rcx, %r15
	movq	%rdx, 96(%rsp)          # 8-byte Spill
	movq	%rsi, %r14
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%r14, %rbx
	andq	%r13, %rbx
	xorq	%rsi, %rsi
	callq	protect_from_queue
	movq	%rax, %r9
	cmpq	$51, %rbx
	jne	.LBB29_94
# BB#1:                                 # %if.then
	movq	%r14, 112(%rsp)         # 8-byte Spill
	movl	target_flags(%rip), %eax
	andq	$33554432, %rax         # imm = 0x2000000
	movq	$23, %rcx
	movq	%rax, %r14
	shrq	%cl, %r14
	testq	%r15, %r15
	je	.LBB29_2
# BB#4:                                 # %if.end
	addq	$4, %r14
	movq	%r12, 80(%rsp)          # 8-byte Spill
	imulq	%r12, %r14
	movabsq	$4293918720, %rcx       # imm = 0xFFF00000
	andq	%rcx, %rax
	movq	$20, %rcx
	shrq	%cl, %rax
	leaq	32(%rax), %rdx
	movq	$60, %rcx
	shrq	%cl, %rdx
	andq	$7, %rdx
	leaq	32(%rax,%rdx), %rsi
	movq	$3, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%rsi
	subq	%rdx, %r14
	movq	%r13, %rdi
	testq	%rdi, %r12
	movq	%r9, 88(%rsp)           # 8-byte Spill
	je	.LBB29_6
# BB#5:                                 # %if.then7
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$51, %rsi
	movq	$1, %rcx
	movq	$1, %r8
	movq	%rdi, %r12
	movq	%r9, %rdi
	movq	%r9, %rbx
	callq	adjust_address_1
	movq	%rbx, %r9
	movq	%r12, %rdi
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB29_6:                               # %if.end9
	movq	208(%rsp), %r12
	andq	%rdi, %r12
	je	.LBB29_8
# BB#7:                                 # %if.end9
	movq	%r14, %r12
.LBB29_8:                               # %if.end9
	movq	%r9, 104(%rsp)          # 8-byte Spill
	movq	192(%rsp), %r8
	testq	%r8, %r8
	movq	%r15, %r13
	jne	.LBB29_36
# BB#9:                                 # %land.lhs.true
	movzbl	target_flags+1(%rip), %eax
	testq	$24, %rax
	jne	.LBB29_36
# BB#10:                                # %land.lhs.true20
	movzbl	(%r13), %eax
	cmpq	$54, %rax
	jne	.LBB29_36
# BB#11:                                # %land.lhs.true20
	testq	%rdi, %r12
	jne	.LBB29_36
# BB#12:                                # %land.lhs.true26
	movq	%r12, 64(%rsp)          # 8-byte Spill
	movq	%r13, 72(%rsp)          # 8-byte Spill
	movl	8(%r13), %r13d
	movq	%rdi, %rax
	movl	class_narrowest_mode+4(%rip), %r12d
	subq	%r14, %r13
	andq	%rax, %r13
	movq	%rax, %r15
	testq	%r12, %r12
	je	.LBB29_13
# BB#14:
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	$17, %rax
	.align	16, 0x90
.LBB29_15:                              # %for.body.lr.ph.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB29_16 Depth 2
	movq	%r12, %rbx
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB29_16:                              # %for.body.i
                                        #   Parent Loop BB29_15 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rax, %rcx
	movq	%rbx, %rdi
	jb	.LBB29_18
# BB#17:                                # %for.body.i
                                        #   in Loop: Header=BB29_16 Depth=2
	movq	%rsi, %rdi
.LBB29_18:                              # %for.body.i
                                        #   in Loop: Header=BB29_16 Depth=2
	movzbl	mode_wider_mode(%rdx), %ebx
	testq	%rbx, %rbx
	movq	%rdi, %rsi
	jne	.LBB29_16
# BB#19:                                # %for.end.i
                                        #   in Loop: Header=BB29_15 Depth=1
	testq	%r15, %rdi
	je	.LBB29_24
# BB#20:                                # %if.end10.i
                                        #   in Loop: Header=BB29_15 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	movq	optab_table+240(%rip), %rax
	sarq	%cl, %rbx
	movq	$4, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB29_23
# BB#21:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB29_15 Depth=1
	callq	get_mode_alignment
	andq	%r15, %rax
	cmpq	$128, %rax
	ja	.LBB29_23
# BB#22:                                # %if.then17.i
                                        #   in Loop: Header=BB29_15 Depth=1
	movzbl	mode_size(%rbx), %ecx
	xorq	%rdx, %rdx
	movq	%r13, %rax
	divq	%rcx
	addq	%rax, 56(%rsp)          # 8-byte Folded Spill
	movq	%rdx, %r13
.LBB29_23:                              # %if.end24.i
                                        #   in Loop: Header=BB29_15 Depth=1
	movzbl	mode_size(%rbx), %eax
	cmpq	$1, %rax
	ja	.LBB29_15
	jmp	.LBB29_24
.LBB29_94:                              # %if.else287
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r12, %r15
	jle	.LBB29_108
# BB#95:                                # %if.then290
	movq	$32, %rcx
	movq	%r14, %rax
	movq	%r14, 112(%rsp)         # 8-byte Spill
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %eax
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	xorq	%rdx, %rdx
	idivq	%rsi
	movq	%rax, %r14
	movq	200(%rsp), %rax
	movq	8(%rax), %rbx
	movq	184(%rsp), %rsi
	testq	%r13, %rsi
	movq	192(%rsp), %rax
	je	.LBB29_98
# BB#96:                                # %if.then290
	testq	%rax, %rax
	jne	.LBB29_98
# BB#97:                                # %if.then323
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	movq	%r9, %r12
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	anti_adjust_stack
	movq	%r12, %r9
.LBB29_98:                              # %if.end326
	movq	208(%rsp), %rsi
	movq	%rsi, %rax
                                        # kill: RSI<def> RAX<kill>
	andq	%r13, %rsi
	je	.LBB29_100
# BB#99:                                # %if.end326
	movq	%r15, %rsi
.LBB29_100:                             # %if.end326
	movl	(%r9), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB29_105
# BB#101:                               # %land.lhs.true386
	movq	%rsi, %r13
	movl	8(%r9), %ecx
	cmpq	$52, %rcx
	ja	.LBB29_104
# BB#102:                               # %land.lhs.true391
	movq	$14, %rcx
	shrq	%cl, %rax
	andq	$1020, %rax             # imm = 0x3FC
	movl	mode_class(%rax), %eax
	cmpq	$1, %rax
	je	.LBB29_104
# BB#103:                               # %if.then399
	movq	%r9, %rdi
	callq	copy_to_reg
	movq	%rax, %r9
.LBB29_104:                             # %if.end401
	movq	%r13, %rsi
.LBB29_105:                             # %if.end401
	movq	%r9, 104(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jle	.LBB29_120
# BB#106:                               # %if.then410.lr.ph
	subq	%r15, %rsi
	movq	%rsi, 88(%rsp)          # 8-byte Spill
	movq	%r15, %rax
                                        # kill: R15<def> RAX<kill>
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB29_107:                             # %if.then410
                                        # =>This Inner Loop Header: Depth=1
	leaq	-1(%r14), %r12
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	%r12, %rsi
	movq	112(%rsp), %rdx         # 8-byte Reload
	callq	operand_subword_force
	movq	%rax, %r13
	movq	88(%rsp), %rax          # 8-byte Reload
	leaq	-1(%rax,%r14), %rax
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	imulq	%rax, %rsi
	movq	96(%rsp), %rax          # 8-byte Reload
	addq	%rax, %rsi
	movq	$32, %rcx
	movl	word_mode(%rip), %r14d
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	216(%rsp), %rcx
	movq	%rcx, 40(%rsp)
	movq	208(%rsp), %rcx
	movq	%rcx, 32(%rsp)
	movq	%rax, 24(%rsp)
	movq	192(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	$0, 8(%rsp)
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	120(%rsp), %r8          # 8-byte Reload
	callq	emit_push_insn
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rbx
	movq	%r12, %r14
	jg	.LBB29_107
	jmp	.LBB29_120
.LBB29_108:                             # %if.else424
	movq	184(%rsp), %rsi
	testq	%r13, %rsi
	movq	192(%rsp), %rdx
	je	.LBB29_111
# BB#109:                               # %if.else424
	testq	%rdx, %rdx
	jne	.LBB29_111
# BB#110:                               # %if.then436
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	movq	%rdx, %r12
	movq	%r9, %rbx
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	anti_adjust_stack
	movq	%rbx, %r9
	movq	%r12, %rdx
.LBB29_111:                             # %if.end439
	testq	%rdx, %rdx
	movq	200(%rsp), %rbx
	jne	.LBB29_114
# BB#112:                               # %land.lhs.true442
	movzbl	target_flags+1(%rip), %eax
	testq	$24, %rax
	je	.LBB29_113
.LBB29_114:                             # %if.else449
	movq	%r9, %r12
	movzbl	(%rbx), %eax
	cmpq	$54, %rax
	jne	.LBB29_116
# BB#115:                               # %if.then454
	movq	8(%rbx), %rsi
	movq	%rdx, %rdi
	callq	plus_constant_wide
	jmp	.LBB29_117
.LBB29_116:                             # %if.else460
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$75, %rdi
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
.LBB29_117:                             # %if.end466
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	memory_address
	movq	%r14, %rdi
	movq	%r14, 112(%rsp)         # 8-byte Spill
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	96(%rsp), %rsi          # 8-byte Reload
	testq	%rsi, %rsi
	je	.LBB29_119
# BB#118:                               # %if.then470
	movq	$1, %rdx
	movq	%rbx, %rdi
	callq	set_mem_attributes
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	set_mem_alias_set
.LBB29_119:                             # %if.end471
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%rsi, 104(%rsp)         # 8-byte Spill
	callq	emit_move_insn
	jmp	.LBB29_120
.LBB29_113:                             # %if.then448
	movq	%r14, %rdi
	movq	%r14, 112(%rsp)         # 8-byte Spill
	movq	%r9, %rsi
	movq	%r9, 104(%rsp)          # 8-byte Spill
	movq	96(%rsp), %rdx          # 8-byte Reload
	callq	emit_single_push_insn
	jmp	.LBB29_120
.LBB29_13:
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
.LBB29_24:                              # %while.end.i
	testq	%r13, %r13
	jne	.LBB29_25
# BB#26:                                # %move_by_pieces_ninsns.exit
	movl	optimize_size(%rip), %eax
	testq	%rax, %rax
	movq	$3, %rax
	movq	%r15, %rdi
	movq	72(%rsp), %r13          # 8-byte Reload
	movq	64(%rsp), %r12          # 8-byte Reload
	jne	.LBB29_28
# BB#27:                                # %cond.false33
	movq	ix86_cost(%rip), %rax
	movl	40(%rax), %eax
.LBB29_28:                              # %cond.end34
	cmpq	%rax, 56(%rsp)          # 8-byte Folded Reload
	movq	192(%rsp), %r15
	movq	%r15, %r8
	jae	.LBB29_36
# BB#29:                                # %land.lhs.true39
	movzbl	target_flags+3(%rip), %eax
	movq	8(%r13), %rdx
	testq	$2, %rax
	je	.LBB29_31
# BB#30:                                # %cond.true42
	leaq	7(%rdx), %rax
	andq	$-8, %rax
	jmp	.LBB29_32
.LBB29_31:                              # %cond.false47
	leaq	1(%rdx), %rax
	andq	$-2, %rax
.LBB29_32:                              # %cond.end53
	cmpq	%rdx, %rax
	jne	.LBB29_36
# BB#33:                                # %if.then60
	movq	184(%rsp), %rsi
	testq	%rdi, %rsi
	je	.LBB29_35
# BB#34:                                # %if.then71
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	anti_adjust_stack
	movq	8(%r13), %rdx
.LBB29_35:                              # %if.end74
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	subq	%r14, %rdx
	xorq	%rdi, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	move_by_pieces
	movq	176(%rsp), %rdi
	movq	80(%rsp), %r15          # 8-byte Reload
	jmp	.LBB29_121
.LBB29_36:                              # %if.else
	movq	80(%rsp), %rax          # 8-byte Reload
	testq	%rdi, %rax
	je	.LBB29_37
# BB#38:                                # %if.then82
	movq	%r8, %r15
	movl	(%r13), %ebx
	movq	%rbx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$54, %rax
	jne	.LBB29_40
# BB#39:                                # %if.then87
	movq	8(%r13), %rsi
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	subq	%r14, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	jmp	.LBB29_41
.LBB29_37:
	movq	200(%rsp), %rbx
	jmp	.LBB29_42
.LBB29_40:                              # %if.else94
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movq	%r12, 64(%rsp)          # 8-byte Spill
	movq	%r13, 72(%rsp)          # 8-byte Spill
	movq	optab_table+16(%rip), %r13
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_CONST_INT
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	expand_binop
.LBB29_41:                              # %if.end101
	movq	%rax, %r13
	movq	200(%rsp), %rbx
	movq	%r15, %r8
.LBB29_42:                              # %if.end101
	movq	%r13, 72(%rsp)          # 8-byte Spill
	testq	%r8, %r8
	je	.LBB29_43
# BB#44:                                # %if.else107
	movzbl	(%rbx), %eax
	cmpq	$54, %rax
	jne	.LBB29_46
# BB#45:                                # %if.then112
	movq	$32, %rcx
	movq	8(%rbx), %rsi
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	addq	%r12, %rsi
	movq	%r8, %rdi
	jmp	.LBB29_47
.LBB29_43:                              # %if.then103
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movq	184(%rsp), %rsi
	callq	push_block
	jmp	.LBB29_48
.LBB29_46:                              # %if.else120
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$75, %rdi
	movq	%r8, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	%rax, %rdi
	movq	%r12, %rsi
.LBB29_47:                              # %if.end129
	callq	plus_constant_wide
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	memory_address
.LBB29_48:                              # %if.end129
	movq	%rax, %r14
	movq	$51, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	96(%rsp), %rsi          # 8-byte Reload
	testq	%rsi, %rsi
	je	.LBB29_50
# BB#49:                                # %if.then133
	movq	$1, %rdx
	movq	%rbx, %rdi
	callq	set_mem_attributes
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	set_mem_alias_set
	jmp	.LBB29_51
.LBB29_50:                              # %if.else134
	movq	%rbx, %rdi
	movq	120(%rsp), %rsi         # 8-byte Reload
	callq	set_mem_align
.LBB29_51:                              # %if.end135
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	movq	%r14, 64(%rsp)          # 8-byte Spill
	movzbl	(%r13), %eax
	cmpq	$54, %rax
	jne	.LBB29_52
# BB#69:                                # %land.lhs.true140
	movl	class_narrowest_mode+4(%rip), %r12d
	movl	8(%r13), %r14d
	xorq	%r15, %r15
	testq	%r12, %r12
	je	.LBB29_70
# BB#71:
	movq	80(%rsp), %r13          # 8-byte Reload
	movq	$17, %rax
	.align	16, 0x90
.LBB29_72:                              # %for.body.lr.ph.i671
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB29_73 Depth 2
	movq	%r12, %rbx
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB29_73:                              # %for.body.i682
                                        #   Parent Loop BB29_72 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rax, %rcx
	movq	%rbx, %rdi
	jb	.LBB29_75
# BB#74:                                # %for.body.i682
                                        #   in Loop: Header=BB29_73 Depth=2
	movq	%rsi, %rdi
.LBB29_75:                              # %for.body.i682
                                        #   in Loop: Header=BB29_73 Depth=2
	movzbl	mode_wider_mode(%rdx), %ebx
	testq	%rbx, %rbx
	movq	%rdi, %rsi
	jne	.LBB29_73
# BB#76:                                # %for.end.i684
                                        #   in Loop: Header=BB29_72 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdi
	je	.LBB29_81
# BB#77:                                # %if.end10.i688
                                        #   in Loop: Header=BB29_72 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	movq	optab_table+240(%rip), %rax
	sarq	%cl, %rbx
	movq	$4, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB29_80
# BB#78:                                # %land.lhs.true.i691
                                        #   in Loop: Header=BB29_72 Depth=1
	callq	get_mode_alignment
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$128, %rax
	ja	.LBB29_80
# BB#79:                                # %if.then17.i697
                                        #   in Loop: Header=BB29_72 Depth=1
	movzbl	mode_size(%rbx), %ecx
	xorq	%rdx, %rdx
	movq	%r14, %rax
	divq	%rcx
	addq	%rax, %r15
	movq	%rdx, %r14
.LBB29_80:                              # %if.end24.i703
                                        #   in Loop: Header=BB29_72 Depth=1
	movzbl	mode_size(%rbx), %eax
	cmpq	$1, %rax
	ja	.LBB29_72
	jmp	.LBB29_81
.LBB29_52:                              # %if.end135.if.else160_crit_edge
	movl	class_narrowest_mode+4(%rip), %r12d
	movq	80(%rsp), %r15          # 8-byte Reload
	movq	120(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB29_53
.LBB29_70:
	movq	80(%rsp), %r13          # 8-byte Reload
.LBB29_81:                              # %while.end.i707
	testq	%r14, %r14
	movq	%r13, %rcx
	jne	.LBB29_25
# BB#82:                                # %move_by_pieces_ninsns.exit709
	movl	optimize_size(%rip), %eax
	testq	%rax, %rax
	movq	$3, %rax
	movq	72(%rsp), %r13          # 8-byte Reload
	movq	120(%rsp), %rsi         # 8-byte Reload
	jne	.LBB29_84
# BB#83:                                # %cond.false149
	movq	ix86_cost(%rip), %rax
	movl	40(%rax), %eax
.LBB29_84:                              # %cond.end151
	cmpq	%rax, %r15
	movq	%rcx, %r15
	jae	.LBB29_53
# BB#85:                                # %if.then156
	movq	8(%r13), %rdx
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	move_by_pieces
	jmp	.LBB29_120
.LBB29_53:                              # %if.else160
	movq	%r15, 80(%rsp)          # 8-byte Spill
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rax, %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	je	.LBB29_88
	.align	16, 0x90
.LBB29_54:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movslq	movstr_optab(,%rbx,4), %rax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB29_87
# BB#55:                                # %land.lhs.true170
                                        #   in Loop: Header=BB29_54 Depth=1
	movzbl	(%r13), %ecx
	cmpq	$54, %rcx
	jne	.LBB29_57
# BB#56:                                # %land.lhs.true175
                                        #   in Loop: Header=BB29_54 Depth=1
	movq	mode_mask_array(,%rbx,8), %rdx
	movq	8(%r13), %rsi
	movq	$1, %rcx
	shrq	%cl, %rdx
	cmpq	%rdx, %rsi
	jbe	.LBB29_58
.LBB29_57:                              # %lor.lhs.false
                                        #   in Loop: Header=BB29_54 Depth=1
	movl	target_flags(%rip), %edx
	movzbl	mode_bitsize(%rbx,%rbx), %esi
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rcx, %rdx
	cmpq	%rdx, %rsi
	jb	.LBB29_87
.LBB29_58:                              # %land.lhs.true191
                                        #   in Loop: Header=BB29_54 Depth=1
	leaq	(%rax,%rax,4), %r14
	movq	insn_data+24(,%r14,8), %r15
	movq	(%r15), %rax
	testq	%rax, %rax
	je	.LBB29_60
# BB#59:                                # %lor.lhs.false196
                                        #   in Loop: Header=BB29_54 Depth=1
	movq	$51, %rsi
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	*%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB29_87
.LBB29_60:                              # %land.lhs.true199
                                        #   in Loop: Header=BB29_54 Depth=1
	movq	24(%r15), %rax
	testq	%rax, %rax
	je	.LBB29_62
# BB#61:                                # %lor.lhs.false206
                                        #   in Loop: Header=BB29_54 Depth=1
	movq	$51, %rsi
	movq	88(%rsp), %rdi          # 8-byte Reload
	callq	*%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB29_87
.LBB29_62:                              # %land.lhs.true209
                                        #   in Loop: Header=BB29_54 Depth=1
	movq	72(%r15), %rax
	testq	%rax, %rax
	je	.LBB29_64
# BB#63:                                # %lor.lhs.false216
                                        #   in Loop: Header=BB29_54 Depth=1
	xorq	%rsi, %rsi
	movq	120(%rsp), %rdi         # 8-byte Reload
	callq	*%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB29_87
.LBB29_64:                              # %if.then219
                                        #   in Loop: Header=BB29_54 Depth=1
	xorq	%rsi, %rsi
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%r13, %rdx
	callq	convert_modes
	movq	%rax, %r13
	callq	get_last_insn
	movq	48(%r15), %rcx
	movq	%rax, %r15
	testq	%rcx, %rcx
	je	.LBB29_67
# BB#65:                                # %land.lhs.true229
                                        #   in Loop: Header=BB29_54 Depth=1
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	*%rcx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB29_67
# BB#66:                                # %if.then232
                                        #   in Loop: Header=BB29_54 Depth=1
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %r13
.LBB29_67:                              # %if.end234
                                        #   in Loop: Header=BB29_54 Depth=1
	movq	96(%rsp), %r12          # 8-byte Reload
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%r13, %rdx
	movq	120(%rsp), %rcx         # 8-byte Reload
	callq	*insn_data+16(,%r14,8)
	testq	%rax, %rax
	jne	.LBB29_68
# BB#86:                                # %if.else241
                                        #   in Loop: Header=BB29_54 Depth=1
	movq	%r12, 96(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	callq	delete_insns_since
	movq	72(%rsp), %r13          # 8-byte Reload
	.align	16, 0x90
.LBB29_87:                              # %for.inc
                                        #   in Loop: Header=BB29_54 Depth=1
	movzbl	mode_wider_mode(%rbx), %r12d
	testq	%r12, %r12
	jne	.LBB29_54
.LBB29_88:                              # %if.end247
	movzbl	target_flags+1(%rip), %eax
	testq	$16, %rax
	je	.LBB29_90
# BB#89:
	movq	80(%rsp), %r12          # 8-byte Reload
	jmp	.LBB29_93
.LBB29_90:                              # %if.then250
	movq	global_rtl+64(%rip), %rdi
	movq	64(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	callq	reg_mentioned_p
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	movq	80(%rsp), %r12          # 8-byte Reload
	jne	.LBB29_92
# BB#91:                                # %lor.lhs.false253
	movq	global_rtl+72(%rip), %rdi
	movq	%r14, %rsi
	callq	reg_mentioned_p
	testq	%rbx, %rax
	je	.LBB29_93
.LBB29_92:                              # %if.then256
	movq	%r14, %rdi
	callq	copy_to_reg
	movq	%rax, 64(%rsp)          # 8-byte Spill
.LBB29_93:                              # %if.end259
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	incq	%rcx
	movl	%ecx, 4(%rax)
	movq	libfunc_table+88(%rip), %r14
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	movq	sizetype_tab(%rip), %rax
	orq	$4, %rbx
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %r15
	movl	60(%rax), %edi
	movq	$9, %rcx
	movl	16(%rax), %eax
	shrq	%cl, %rdi
	andq	$127, %rdi
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	convert_modes
	movq	sizetype_tab(%rip), %rcx
	movl	60(%rcx), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	movq	%rdx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movq	%rbx, 8(%rsp)
	movabsq	$0, %rsi
	movabsq	$0, %rdx
	movq	$3, %rcx
	xorq	%rax, %rax
	movq	%r15, (%rsp)
	movq	%r14, %rdi
	movq	64(%rsp), %r8           # 8-byte Reload
	movq	%rbx, %r9
	callq	emit_library_call
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	decq	%rcx
	movl	%ecx, 4(%rax)
	movq	176(%rsp), %rdi
	movq	%r12, %r15
	jmp	.LBB29_121
.LBB29_68:                              # %if.then239
	movq	%rax, %rdi
	callq	emit_insn
	movq	80(%rsp), %r15          # 8-byte Reload
.LBB29_120:                             # %ret
	movq	176(%rsp), %rdi
.LBB29_121:                             # %ret
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	112(%rsp), %rax         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	104(%rsp), %rdx         # 8-byte Reload
	jle	.LBB29_127
# BB#122:                               # %ret
	testq	%rdi, %rdi
	je	.LBB29_127
# BB#123:                               # %if.then481
	movq	%r15, %r8
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	movzbl	(%rdi), %eax
	cmpq	$39, %rax
	jne	.LBB29_125
# BB#124:                               # %if.then486
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	callq	emit_group_load
	jmp	.LBB29_127
.LBB29_125:                             # %for.body.i713.preheader
	movl	8(%rdi), %eax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	xorq	%rbx, %rbx
	movq	%r8, %r13
	andq	%rdx, %r13
	movq	%rcx, %r12
	movq	%rsi, %r15
	.align	16, 0x90
.LBB29_126:                             # %for.body.i713
                                        # =>This Inner Loop Header: Depth=1
	movl	word_mode(%rip), %edi
	movq	120(%rsp), %rax         # 8-byte Reload
	leaq	(%rbx,%rax), %rsi
	callq	gen_rtx_REG
	movq	%rax, %r14
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	callq	operand_subword_force
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	incq	%rbx
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%r13, %rax
	jne	.LBB29_126
.LBB29_127:                             # %if.end492
	movq	216(%rsp), %rdi
	testq	%rdi, %rdi
	je	.LBB29_129
# BB#128:                               # %if.end492
	movq	192(%rsp), %rax
	testq	%rax, %rax
	jne	.LBB29_129
# BB#130:                               # %if.then508
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	anti_adjust_stack  # TAILCALL
.LBB29_129:                             # %if.end509
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB29_2:                               # %if.then5
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.emit_push_insn, %rcx
	movq	$3301, %rsi             # imm = 0xCE5
	jmp	.LBB29_3
.LBB29_25:                              # %if.then28.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.move_by_pieces_ninsns, %rcx
	movq	$1542, %rsi             # imm = 0x606
.LBB29_3:                               # %if.then5
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp407:
	.size	emit_push_insn, .Ltmp407-emit_push_insn
	.cfi_endproc

	.align	16, 0x90
	.type	emit_single_push_insn,@function
emit_single_push_insn:                  # @emit_single_push_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp414:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp415:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp416:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp417:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp418:
	.cfi_def_cfa_offset 48
.Ltmp419:
	.cfi_offset %rbx, -48
.Ltmp420:
	.cfi_offset %r12, -40
.Ltmp421:
	.cfi_offset %r13, -32
.Ltmp422:
	.cfi_offset %r14, -24
.Ltmp423:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	$32, %rcx
                                        # kill: RDI<def> R12<kill>
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movzbl	target_flags+3(%rip), %ecx
	movzbl	mode_size(%rdi), %eax
	movq	%rdx, %r15
	movq	%rsi, %r14
	testq	$2, %rcx
	je	.LBB30_2
# BB#1:                                 # %cond.true9
	leaq	7(%rax), %rsi
	andq	$504, %rsi              # imm = 0x1F8
	jmp	.LBB30_3
.LBB30_2:                               # %cond.false15
	leaq	1(%rax), %rsi
	andq	$510, %rsi              # imm = 0x1FE
.LBB30_3:                               # %cond.end21
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rcx
	movl	8(%rcx), %edx
	addq	%rsi, %rdx
	movl	%edx, 8(%rcx)
	movq	optab_table+384(%rip), %rdx
	movq	$4, %rcx
	shlq	%cl, %rdi
	movslq	8(%rdx,%rdi), %rcx
	cmpq	$1317, %rcx             # imm = 0x525
	jne	.LBB30_4
# BB#8:                                 # %if.end38
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	target_flags(%rip), %ebx
	andq	%rcx, %rsi
	movq	$25, %rcx
	shrq	%cl, %rbx
	movq	global_rtl+16(%rip), %r13
	andq	$1, %rbx
	orq	$4, %rbx
	cmpq	%rsi, %rax
	jne	.LBB30_10
# BB#9:                                 # %if.then44
	movq	$96, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_e
	jmp	.LBB30_11
.LBB30_4:                               # %if.then
	leaq	(%rcx,%rcx,4), %rbx
	movq	insn_data+24(,%rbx,8), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB30_7
# BB#5:                                 # %land.lhs.true
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	*%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB30_7
# BB#6:                                 # %if.then32
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	force_reg
	movq	%rax, %r14
.LBB30_7:                               # %if.end
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	*insn_data+16(,%rbx,8)
	movq	%rax, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_insn  # TAILCALL
.LBB30_10:                              # %if.else
	negq	%rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	movq	global_rtl+16(%rip), %rdx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$100, %rdi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
.LBB30_11:                              # %if.end59
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	testq	%r15, %r15
	je	.LBB30_14
# BB#12:                                # %if.then63
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	set_mem_attributes
	movl	flag_optimize_sibling_calls(%rip), %eax
	testq	%rax, %rax
	je	.LBB30_14
# BB#13:                                # %if.then65
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	set_mem_alias_set
.LBB30_14:                              # %if.end67
	movq	%rbx, %rdi
	movq	%r14, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_move_insn  # TAILCALL
.Ltmp424:
	.size	emit_single_push_insn, .Ltmp424-emit_single_push_insn
	.cfi_endproc

	.globl	expand_assignment
	.align	16, 0x90
	.type	expand_assignment,@function
expand_assignment:                      # @expand_assignment
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp431:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp432:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp433:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp434:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp435:
	.cfi_def_cfa_offset 48
	subq	$128, %rsp
.Ltmp436:
	.cfi_def_cfa_offset 176
.Ltmp437:
	.cfi_offset %rbx, -48
.Ltmp438:
	.cfi_offset %r12, -40
.Ltmp439:
	.cfi_offset %r13, -32
.Ltmp440:
	.cfi_offset %r14, -24
.Ltmp441:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movzbl	16(%rbx), %ecx
	movq	%rdx, %r15
	movq	%rsi, %r12
	cmpq	$44, %rcx
	ja	.LBB31_57
# BB#1:                                 # %entry
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$28037546508288, %rdx   # imm = 0x198000000000
	testq	%rdx, %rax
	je	.LBB31_2
# BB#6:                                 # %if.then23
	movq	%r12, 64(%rsp)          # 8-byte Spill
	xorq	%rax, %rax
	movl	%eax, 88(%rsp)
	callq	push_temp_slots
	leaq	88(%rsp), %rax
	leaq	112(%rsp), %rsi
	leaq	104(%rsp), %rdx
	leaq	96(%rsp), %rcx
	leaq	124(%rsp), %r8
	leaq	92(%rsp), %r9
	movq	%rax, (%rsp)
	movq	%rbx, %rdi
	callq	get_inner_reference
	movl	124(%rsp), %ecx
	testq	%rcx, %rcx
	jne	.LBB31_7
# BB#8:                                 # %if.then23
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r15
	je	.LBB31_9
# BB#10:                                # %if.then27
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	%rax, %rdi
	callq	stabilize_reference
	jmp	.LBB31_11
.LBB31_2:                               # %entry
	testq	%rcx, %rcx
	jne	.LBB31_57
# BB#3:                                 # %if.then
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	callq	expand_expr
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r15
	je	.LBB31_5
# BB#4:                                 # %if.then
	movq	%rax, %r15
.LBB31_5:                               # %if.then
	movq	%r15, %rbx
	jmp	.LBB31_87
.LBB31_57:                              # %if.end256
	movzbl	16(%r12), %eax
	cmpq	$53, %rax
	jne	.LBB31_72
# BB#58:                                # %land.lhs.true263
	movq	%r12, %rdi
	callq	aggregate_value_p
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	jne	.LBB31_72
# BB#59:                                # %land.lhs.true266
	movq	8(%r12), %rax
	movq	32(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB31_72
# BB#60:                                # %land.lhs.true276
	movl	16(%rbx), %eax
	andq	$254, %rax
	cmpq	$34, %rax
	jne	.LBB31_64
# BB#61:                                # %land.lhs.true290
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB31_63
# BB#62:                                # %cond.false295
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB31_63:                              # %cond.end298
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB31_64
.LBB31_72:                              # %if.then340
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	$4, %rcx
	movq	%rbx, %rdi
	callq	expand_expr
	movzbl	16(%rbx), %ecx
	movq	%rax, %r13
	cmpq	$36, %rcx
	jne	.LBB31_85
# BB#73:                                # %land.lhs.true349
	movzbl	(%r13), %eax
	cmpq	$39, %rax
	je	.LBB31_75
# BB#74:                                # %land.lhs.true349
	cmpq	$61, %rax
	jne	.LBB31_81
.LBB31_75:                              # %if.then359
	callq	push_temp_slots
	movzbl	2(%r13), %edx
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	callq	expand_expr
	movzbl	(%r13), %ecx
	movq	%rax, %r14
	cmpq	$39, %rcx
	jne	.LBB31_77
# BB#76:                                # %if.then368
	movq	8(%r12), %rdi
	callq	int_size_in_bytes
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	emit_group_load
	jmp	.LBB31_78
.LBB31_7:
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	jmp	.LBB31_11
.LBB31_9:
	movq	%rbx, 72(%rsp)          # 8-byte Spill
.LBB31_11:                              # %if.end29
	movq	%rax, 56(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	96(%rsp), %r14
	movq	%rax, %r13
	testq	%r14, %r14
	movq	%r13, %r12
	je	.LBB31_28
# BB#12:                                # %if.then32
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%r14, %rdi
	callq	expand_expr
	movl	(%r13), %ecx
	movq	%rax, %r12
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB31_88
# BB#13:                                # %if.end38
	movzbl	2(%r12), %eax
	movl	ptr_mode(%rip), %edi
	cmpq	%rdi, %rax
	jne	.LBB31_15
# BB#14:
	movq	%r15, 80(%rsp)          # 8-byte Spill
	jmp	.LBB31_16
.LBB31_15:                              # %if.then42
	movq	%r15, 80(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%r12, %rdx
	callq	convert_modes
	movl	(%r13), %ecx
	movq	%rax, %r12
.LBB31_16:                              # %if.end44
	andq	$16777215, %rcx         # imm = 0xFFFFFF
	cmpq	$3342402, %rcx          # imm = 0x330042
	movq	%r13, %r15
	jne	.LBB31_25
# BB#17:                                # %land.lhs.true53
	movq	8(%r13), %rax
	movl	(%rax), %eax
	testq	$16711680, %rax         # imm = 0xFF0000
	movq	%r13, %r15
	je	.LBB31_25
# BB#18:                                # %land.lhs.true53
	movq	112(%rsp), %rbx
	testq	%rbx, %rbx
	movq	%r13, %r15
	jle	.LBB31_25
# BB#19:                                # %land.lhs.true60
	movq	104(%rsp), %rcx
	movq	%rcx, %rax
	cqto
	idivq	%rbx
	testq	%rdx, %rdx
	movq	%r13, %r15
	jne	.LBB31_25
# BB#20:                                # %land.lhs.true62
	movl	124(%rsp), %edi
	movq	%rcx, %r15
	callq	get_mode_alignment
	movq	%r15, %rsi
	movq	%rax, %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	movq	%rbx, %rax
	cqto
	idivq	%rcx
	testq	%rdx, %rdx
	movq	%r13, %r15
	jne	.LBB31_25
# BB#21:                                # %land.lhs.true67
	movq	%rdi, %r15
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	movq	16(%r13), %rax
	testq	%rax, %rax
	movq	$8, %rbx
	je	.LBB31_23
# BB#22:                                # %cond.true72
	movl	32(%rax), %ebx
.LBB31_23:                              # %cond.end77
	movl	124(%rsp), %edi
	callq	get_mode_alignment
	andq	%r15, %rax
	andq	%r15, %rbx
	cmpq	%rax, %rbx
	movq	%r13, %r15
	movq	48(%rsp), %rax          # 8-byte Reload
	jne	.LBB31_25
# BB#24:                                # %if.then82
	movq	$63, %rcx
	movq	%rax, %rdx
	sarq	%cl, %rdx
	movq	$61, %rcx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r13, %rdi
	movl	124(%rsp), %esi
	callq	adjust_address_1
	movq	%rax, %r15
	movq	$0, 104(%rsp)
.LBB31_25:                              # %if.end84
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rbx
	movq	%r14, %rdi
	callq	highest_pow2_factor
	movl	64(%rbx), %edx
	movq	$3, %rcx
	shrq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB31_27
# BB#26:                                # %if.end84
	movq	%rdx, %rax
.LBB31_27:                              # %if.end84
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	offset_address
	movq	%rax, %r12
	movq	80(%rsp), %r15          # 8-byte Reload
.LBB31_28:                              # %if.end88
	movzbl	(%r12), %eax
	cmpq	$66, %rax
	jne	.LBB31_38
# BB#29:                                # %if.then93
	movq	16(%r12), %rax
	xorq	%r14, %r14
	testq	%rax, %rax
	movabsq	$0, %rbx
	je	.LBB31_31
# BB#30:                                # %cond.false100
	movq	8(%rax), %rbx
.LBB31_31:                              # %cond.end104
	movq	%r12, %rdi
	callq	shallow_copy_rtx
	movq	%rax, %r12
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	set_mem_attributes
	movq	16(%r12), %rax
	testq	%rax, %rax
	je	.LBB31_33
# BB#32:                                # %cond.false113
	movq	8(%rax), %r14
.LBB31_33:                              # %cond.end118
	cmpq	%rbx, %r14
	je	.LBB31_38
# BB#34:                                # %cond.end118
	testq	%rax, %rax
	je	.LBB31_38
# BB#35:                                # %cond.false129
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB31_38
# BB#36:                                # %land.lhs.true135
	movq	104(%rsp), %rdx
	leaq	7(%rdx), %rcx
	cmpq	$15, %rcx
	jb	.LBB31_38
# BB#37:                                # %if.then139
	movq	$63, %rcx
	movq	%rdx, %rdi
	sarq	%cl, %rdi
	movq	$61, %rcx
	shrq	%cl, %rdi
	movq	8(%rax), %rsi
	addq	%rdx, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	subq	%rdi, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	set_mem_offset
.LBB31_38:                              # %if.end158
	movl	88(%rsp), %eax
	testq	%rax, %rax
	movq	72(%rsp), %r14          # 8-byte Reload
	je	.LBB31_43
# BB#39:                                # %land.lhs.true160
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB31_43
# BB#40:                                # %if.then165
	cmpq	%r13, %r12
	jne	.LBB31_42
# BB#41:                                # %if.then168
	movq	%r13, %rdi
	callq	copy_rtx
	movq	%rax, %r12
	movl	(%r12), %eax
.LBB31_42:                              # %if.end170
	orq	$134217728, %rax        # imm = 0x8000000
	movl	%eax, (%r12)
.LBB31_43:                              # %if.end173
	movzbl	16(%r14), %eax
	cmpq	$39, %rax
	jne	.LBB31_48
# BB#44:                                # %land.lhs.true180
	movq	40(%r14), %rax
	movzbl	17(%rax), %eax
	testq	$16, %rax
	je	.LBB31_48
# BB#45:                                # %if.then187
	cmpq	%r13, %r12
	jne	.LBB31_47
# BB#46:                                # %if.then190
	movq	%r13, %rdi
	callq	copy_rtx
	movq	%rax, %r12
.LBB31_47:                              # %if.end192
	movl	(%r12), %eax
	orq	$67108864, %rax         # imm = 0x4000000
	movl	%eax, (%r12)
.LBB31_48:                              # %if.end196
	movzbl	(%r12), %eax
	cmpq	$66, %rax
	jne	.LBB31_53
# BB#49:                                # %land.lhs.true201
	movq	%r14, %rdi
	callq	can_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB31_53
# BB#50:                                # %if.then204
	cmpq	%r13, %r12
	jne	.LBB31_52
# BB#51:                                # %if.then207
	movq	%r13, %rdi
	callq	copy_rtx
	movq	%rax, %r12
.LBB31_52:                              # %if.end209
	movl	(%r12), %eax
	orq	$16777216, %rax         # imm = 0x1000000
	movl	%eax, (%r12)
.LBB31_53:                              # %if.end213
	xorq	%rbx, %rbx
	movq	112(%rsp), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	104(%rsp), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	movq	%r15, 80(%rsp)          # 8-byte Spill
	movabsq	$0, %r15
	je	.LBB31_55
# BB#54:                                # %cond.true215
	movq	8(%r14), %rax
	movl	60(%rax), %r15d
	movq	$9, %rcx
	shrq	%cl, %r15
	andq	$127, %r15
.LBB31_55:                              # %cond.end223
	movq	%r14, %r13
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %r14
	movq	%r13, %rdi
	callq	get_alias_set
	movq	%rax, 16(%rsp)
	movq	%r14, 8(%rsp)
	movl	92(%rsp), %eax
	movq	%rax, (%rsp)
	movq	%r12, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	40(%rsp), %rdx          # 8-byte Reload
	movl	124(%rsp), %ecx
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	%r12, %r8
	movq	%r15, %r9
	callq	store_field
	movq	%rax, %r14
	movq	%r14, %rdi
	callq	preserve_temp_slots
	callq	free_temp_slots
	callq	pop_temp_slots
	movq	80(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB31_87
# BB#56:                                # %cond.true231
	movq	8(%r13), %rax
	movl	60(%rax), %edi
	movq	$9, %rcx
	movq	8(%r12), %rdx
	shrq	%cl, %rdi
	andq	$127, %rdi
	movl	60(%rdx), %esi
	movq	$9, %rcx
	movl	16(%rax), %eax
	shrq	%cl, %rsi
	andq	$127, %rsi
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	convert_modes  # TAILCALL
.LBB31_77:                              # %if.else373
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	emit_move_insn
.LBB31_78:                              # %if.end375
	movq	%r13, %rdi
	callq	preserve_temp_slots
	callq	free_temp_slots
	callq	pop_temp_slots
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r15
	je	.LBB31_80
# BB#79:                                # %if.end375
	movq	%r13, %r15
.LBB31_80:                              # %if.end375
	movq	%r15, %rbx
	jmp	.LBB31_87
.LBB31_81:                              # %land.lhs.true388
	movzbl	16(%r12), %eax
	cmpq	$41, %rax
	jne	.LBB31_85
# BB#82:                                # %land.lhs.true395
	movq	cfun(%rip), %rax
	movl	424(%rax), %eax
	andq	$3, %rax
	cmpq	$1, %rax
	jne	.LBB31_85
# BB#83:                                # %if.then404
	callq	push_temp_slots
	movq	%r12, %rdi
	callq	expr_size
	movq	%r12, %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	libfunc_table+96(%rip), %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	%r15, 80(%rsp)          # 8-byte Spill
	movq	8(%r13), %r15
	movl	target_flags(%rip), %r12d
	movq	$25, %rcx
	shrq	%cl, %r12
	andq	$1, %r12
	movq	sizetype_tab(%rip), %rdx
	orq	$4, %r12
	movq	8(%rax), %r14
	movl	60(%rdx), %edi
	movq	$9, %rcx
	movl	16(%rdx), %eax
	shrq	%cl, %rdi
	andq	$127, %rdi
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	convert_modes
	movq	sizetype_tab(%rip), %rcx
	movl	60(%rcx), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	movq	%rdx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movq	%r12, 8(%rsp)
	movabsq	$0, %rsi
	movabsq	$0, %rdx
	movq	$3, %rcx
	xorq	%rax, %rax
	movq	%r14, (%rsp)
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %r8
	movq	80(%rsp), %r14          # 8-byte Reload
	movq	%r12, %r9
	callq	emit_library_call
	movq	%r13, %rdi
	callq	preserve_temp_slots
	callq	free_temp_slots
	callq	pop_temp_slots
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r14
	je	.LBB31_86
# BB#84:                                # %if.then404
	movq	%r13, %r14
	jmp	.LBB31_86
.LBB31_85:                              # %if.end440
	callq	push_temp_slots
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	store_expr
	movq	%rax, %r14
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%r15, %rbx
	movq	%r14, %rdi
	callq	preserve_temp_slots
	callq	free_temp_slots
	callq	pop_temp_slots
	testq	%rbx, %rbx
	je	.LBB31_87
.LBB31_86:                              # %if.end440
	movq	%r14, %rbx
	jmp	.LBB31_87
.LBB31_64:                              # %if.then304
	movq	%r15, 80(%rsp)          # 8-byte Spill
	callq	push_temp_slots
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	callq	expand_expr
	movq	%rbx, %rdi
	movq	%rax, %r14
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	$4, %rcx
	callq	expand_expr
	movq	%rax, %r15
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$39, %rcx
	jne	.LBB31_66
# BB#65:                                # %if.then315
	movq	8(%r12), %rdi
	callq	int_size_in_bytes
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	emit_group_load
	jmp	.LBB31_69
.LBB31_66:                              # %if.else
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$3342336, %rax          # imm = 0x330000
	jne	.LBB31_68
# BB#67:                                # %if.then325
	movq	%r12, %rdi
	callq	expr_size
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	emit_block_move
	jmp	.LBB31_69
.LBB31_68:                              # %if.else328
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	emit_move_insn
.LBB31_69:                              # %if.end331
	movq	%r15, %rdi
	callq	preserve_temp_slots
	callq	free_temp_slots
	callq	pop_temp_slots
	movq	80(%rsp), %r12          # 8-byte Reload
	andq	%r13, %r12
	je	.LBB31_71
# BB#70:                                # %if.end331
	movq	%r15, %r12
.LBB31_71:                              # %if.end331
	movq	%r12, %rbx
.LBB31_87:                              # %return
	movq	%rbx, %rax
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB31_88:                              # %if.then37
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_assignment, %rcx
	movq	$3686, %rsi             # imm = 0xE66
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp442:
	.size	expand_assignment, .Ltmp442-expand_assignment
	.cfi_endproc

	.globl	get_inner_reference
	.align	16, 0x90
	.type	get_inner_reference,@function
get_inner_reference:                    # @get_inner_reference
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp449:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp450:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp451:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp452:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp453:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp454:
	.cfi_def_cfa_offset 112
.Ltmp455:
	.cfi_offset %rbx, -48
.Ltmp456:
	.cfi_offset %r12, -40
.Ltmp457:
	.cfi_offset %r13, -32
.Ltmp458:
	.cfi_offset %r14, -24
.Ltmp459:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	$0, 56(%rsp)
	movl	16(%r12), %eax
	movq	global_trees+120(%rip), %r13
	movq	global_trees+136(%rip), %rdi
	movq	%r8, 32(%rsp)           # 8-byte Spill
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, %r14
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$40, %rcx
	jne	.LBB32_1
# BB#8:                                 # %if.then29
	movq	40(%r12), %rbx
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, (%r9)
	xorq	%rdx, %rdx
	jmp	.LBB32_9
.LBB32_1:                               # %entry
	cmpq	$39, %rcx
	jne	.LBB32_5
# BB#2:                                 # %if.then
	movq	40(%r12), %rax
	movq	48(%rax), %rcx
	movq	40(%rax), %rbx
	xorq	%rdx, %rdx
	testq	$4096, %rcx             # imm = 0x1000
	jne	.LBB32_4
# BB#3:                                 # %if.then8
	andq	$255, %rcx
	movq	%rcx, %rdx
.LBB32_4:                               # %if.end
	movl	16(%rax), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, (%r9)
	jmp	.LBB32_9
.LBB32_5:                               # %if.else38
	movq	8(%r12), %rax
	movl	60(%rax), %esi
	movq	$9, %rcx
	shrq	%cl, %rsi
	movl	16(%rax), %edx
	andq	$127, %rsi
	movq	$13, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	cmpq	$51, %rsi
	movl	%edx, (%r9)
	jne	.LBB32_7
# BB#6:                                 # %if.then53
	movq	32(%rax), %rbx
	movq	$51, %rdx
.LBB32_9:                               # %if.end62
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	testq	%rbx, %rbx
	je	.LBB32_10
# BB#11:                                # %if.then65
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB32_12
# BB#13:                                # %if.else68
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	tree_low_cst
	jmp	.LBB32_14
.LBB32_10:
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	jmp	.LBB32_15
.LBB32_12:                              # %if.then67
	movq	$-1, (%r14)
	movq	$51, 40(%rsp)           # 8-byte Folded Spill
	jmp	.LBB32_15
.LBB32_7:                               # %if.end62.thread
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rsi, %rax
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movzbl	mode_bitsize(%rax,%rax), %eax
.LBB32_14:                              # %while.body.preheader
	movq	%rax, (%r14)
.LBB32_15:                              # %while.body.preheader
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	jmp	.LBB32_16
	.align	16, 0x90
.LBB32_51:                              # %if.end257
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	32(%r14), %r12
.LBB32_16:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, %r14
	movzbl	16(%r14), %eax
	cmpq	$113, %rax
	jg	.LBB32_22
# BB#17:                                # %while.body
                                        #   in Loop: Header=BB32_16 Depth=1
	cmpq	$56, %rax
	jg	.LBB32_46
# BB#18:                                # %while.body
                                        #   in Loop: Header=BB32_16 Depth=1
	leaq	-43(%rax), %rcx
	andq	%rdx, %rcx
	cmpq	$2, %rcx
	jae	.LBB32_19
# BB#34:                                # %if.then126
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	%r13, 8(%rsp)           # 8-byte Spill
	movq	32(%r14), %rbx
	movq	8(%rbx), %rax
	movq	24(%rax), %rcx
	movq	40(%r14), %r13
	testq	%rcx, %rcx
	jne	.LBB32_36
# BB#35:                                # %cond.end.thread
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	8(%rax), %rax
	movq	40(%rax), %r12
	jmp	.LBB32_39
	.align	16, 0x90
.LBB32_22:                              # %while.body
                                        #   in Loop: Header=BB32_16 Depth=1
	leaq	-114(%rax), %rcx
	andq	%rdx, %rcx
	cmpq	$2, %rcx
	jae	.LBB32_23
# BB#48:                                # %land.lhs.true225
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	32(%r14), %rax
	movq	8(%r14), %rcx
	movq	8(%rax), %rax
	movl	60(%rcx), %ecx
	movl	60(%rax), %eax
	xorq	%rcx, %rax
	testq	$65024, %rax            # imm = 0xFE00
	je	.LBB32_49
	jmp	.LBB32_24
	.align	16, 0x90
.LBB32_46:                              # %while.body
                                        #   in Loop: Header=BB32_16 Depth=1
	cmpq	$57, %rax
	jne	.LBB32_24
# BB#47:                                # %if.then190
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	%r14, %rdi
	leaq	56(%rsp), %rsi
	movq	%rdx, %rbx
	callq	find_placeholder
	movq	%rbx, %rdx
	movq	%rax, %r12
	testq	%r12, %r12
	jne	.LBB32_16
	jmp	.LBB32_24
	.align	16, 0x90
.LBB32_23:                              # %while.body
                                        #   in Loop: Header=BB32_16 Depth=1
	addq	$-116, %rax
	andq	%rdx, %rax
	cmpq	$2, %rax
	jb	.LBB32_49
	jmp	.LBB32_24
.LBB32_19:                              # %while.body
                                        #   in Loop: Header=BB32_16 Depth=1
	cmpq	$39, %rax
	jne	.LBB32_20
# BB#29:                                # %if.then90
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	40(%r14), %r15
	movq	88(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB32_24
# BB#30:                                # %if.else98
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	%rdx, %r12
	movzbl	17(%rbx), %eax
	testq	$2, %rax
	jne	.LBB32_33
# BB#31:                                # %land.lhs.true
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	%rbx, %rdi
	callq	contains_placeholder_p
	testq	%r12, %rax
	je	.LBB32_33
# BB#32:                                # %if.then106
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	sizetype_tab(%rip), %rsi
	movq	$58, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	build
	movq	%rax, %rbx
.LBB32_33:                              # %if.end109
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	$59, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	size_binop
	movq	160(%r15), %rdx
	movq	%rax, %r13
	movq	$59, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	size_binop
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	%r12, %rdx
	jmp	.LBB32_49
.LBB32_36:                              # %cond.end
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	8(%rax), %rax
	movq	104(%rcx), %r15
	movq	40(%rax), %r12
	testq	%r15, %r15
	je	.LBB32_39
# BB#37:                                # %land.lhs.true145
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	%r15, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB32_39
# BB#38:                                # %if.then148
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	8(%r13), %rsi
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	%r13, %rdx
	movq	%r15, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r13
.LBB32_39:                              # %if.end153
                                        #   in Loop: Header=BB32_16 Depth=1
	movzbl	17(%r13), %eax
	testq	$2, %rax
	jne	.LBB32_42
# BB#40:                                # %land.lhs.true160
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	%r13, %rdi
	callq	contains_placeholder_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB32_42
# BB#41:                                # %if.then163
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	8(%r13), %rsi
	movq	$58, %rdi
	xorq	%rax, %rax
	movq	%r13, %rdx
	movq	%r14, %rcx
	callq	build
	movq	%rax, %r13
.LBB32_42:                              # %if.end167
                                        #   in Loop: Header=BB32_16 Depth=1
	movzbl	17(%r12), %eax
	testq	$2, %rax
	jne	.LBB32_45
# BB#43:                                # %land.lhs.true174
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	%r12, %rdi
	callq	contains_placeholder_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB32_45
# BB#44:                                # %if.then177
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	sizetype_tab(%rip), %rsi
	movq	$58, %rdi
	xorq	%rax, %rax
	movq	%r12, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	%rax, %r12
.LBB32_45:                              # %if.end179
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	sizetype_tab(%rip), %rdi
	movq	%r13, %rsi
	callq	convert
	movq	$61, %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	callq	size_binop
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, %r13
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	jmp	.LBB32_49
.LBB32_20:                              # %while.body
                                        #   in Loop: Header=BB32_16 Depth=1
	cmpq	$40, %rax
	jne	.LBB32_24
# BB#21:                                # %if.then78
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	%rdx, %rbx
	movq	48(%r14), %rdx
	movq	$59, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	size_binop
	movq	%rbx, %rdx
	movq	%rax, 48(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB32_49:                              # %if.end250
                                        #   in Loop: Header=BB32_16 Depth=1
	movzbl	17(%r14), %eax
	testq	$8, %rax
	je	.LBB32_51
# BB#50:                                # %if.then256
                                        #   in Loop: Header=BB32_16 Depth=1
	movq	112(%rsp), %rax
	movq	$1, %rcx
	movl	%ecx, (%rax)
	jmp	.LBB32_51
.LBB32_24:                              # %while.end
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	movq	%rdx, %rbx
	callq	host_integerp
	testq	%rbx, %rax
	movq	%rbx, %r12
	movq	48(%rsp), %rbx          # 8-byte Reload
	je	.LBB32_52
# BB#25:                                # %land.lhs.true263
	movq	sizetype_tab+24(%rip), %rdi
	movq	%r13, %rsi
	callq	convert
	movq	global_trees+152(%rip), %rdx
	movq	$61, %rdi
	movq	%rax, %rsi
	callq	size_binop
	testq	%rax, %rax
	je	.LBB32_52
# BB#26:                                # %land.lhs.true268
	movq	$59, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	size_binop
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB32_52
# BB#27:                                # %land.lhs.true272
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	host_integerp
	testq	%r12, %rax
	je	.LBB32_52
# BB#28:                                # %if.then275
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	tree_low_cst
	xorq	%r13, %r13
	jmp	.LBB32_53
.LBB32_52:                              # %if.else277
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	tree_low_cst
.LBB32_53:                              # %if.end279
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%r13, (%rax)
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	movl	%ecx, (%rax)
	movq	%r14, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp460:
	.size	get_inner_reference, .Ltmp460-get_inner_reference
	.cfi_endproc

	.align	16, 0x90
	.type	store_field,@function
store_field:                            # @store_field
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp467:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp468:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp469:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp470:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp471:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp472:
	.cfi_def_cfa_offset 112
.Ltmp473:
	.cfi_offset %rbx, -48
.Ltmp474:
	.cfi_offset %r12, -40
.Ltmp475:
	.cfi_offset %r13, -32
.Ltmp476:
	.cfi_offset %r14, -24
.Ltmp477:
	.cfi_offset %r15, -16
	movq	%r8, %r12
	movzbl	16(%r12), %eax
	movq	%rcx, %r14
	movq	%rsi, %r13
	movq	%rdi, %r10
	testq	%rax, %rax
	je	.LBB33_40
# BB#1:                                 # %if.end
	testq	%r13, %r13
	je	.LBB33_70
# BB#2:                                 # %if.else
	movq	%r9, 48(%rsp)           # 8-byte Spill
	xorq	%r15, %r15
	cmpq	$63, %r13
	ja	.LBB33_4
# BB#3:                                 # %if.then5
	movq	$1, %r15
	movq	%r13, %rcx
	shlq	%cl, %r15
	decq	%r15
.LBB33_4:                               # %if.end7
	movq	120(%rsp), %rdi
	movl	(%r10), %eax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	%r14, %rbx
	andq	%rsi, %rbx
	cmpq	$51, %rbx
	jne	.LBB33_13
# BB#5:                                 # %land.lhs.true9
	movq	%rax, %rcx
	orq	$2, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB33_13
# BB#6:                                 # %if.then16
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movq	%r10, %r15
	movl	16(%rdi), %eax
	movq	$10, %rcx
	movq	%rax, %rbx
	shrq	%cl, %rbx
	movl	60(%rdi), %esi
	movq	$19, %rcx
	shrq	%cl, %rsi
	andq	$255, %rax
	xorq	%rdx, %rdx
	cmpq	$20, %rax
	jne	.LBB33_10
# BB#7:                                 # %land.rhs
	cmpq	$0, 8(%rdi)
	je	.LBB33_8
# BB#9:                                 # %land.rhs
	movabsq	$1, %rdx
	jmp	.LBB33_10
.LBB33_13:                              # %if.end54
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$65, %rax
	jne	.LBB33_18
# BB#14:                                # %if.then59
	testq	%rdx, %rdx
	jne	.LBB33_15
# BB#17:                                # %if.end63
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%r10, %rsi
.LBB33_69:                              # %if.end338
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	store_expr  # TAILCALL
.LBB33_70:                              # %if.then2
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	expand_expr  # TAILCALL
.LBB33_18:                              # %if.end65
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	cmpq	$51, %rbx
	je	.LBB33_23
# BB#19:                                # %if.end65
	movq	%r10, 32(%rsp)          # 8-byte Spill
	testq	%rsi, %r14
	jne	.LBB33_21
# BB#20:                                # %if.then134.thread
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	callq	expand_expr
	movq	%rax, 40(%rsp)          # 8-byte Spill
	jmp	.LBB33_28
.LBB33_8:
	movabsq	$0, %rdx
.LBB33_10:                              # %land.end
	andq	$2, %rbx
	andq	$4, %rsi
	movq	$3, %rcx
	shlq	%cl, %rdx
	orq	%rbx, %rsi
	orq	%rdx, %rsi
	orq	$1, %rsi
	callq	build_qualified_type
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	%rax, %rdi
	callq	assign_temp
	movq	%rax, %r14
	movq	$51, %rsi
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r14, %rdi
	callq	adjust_address_1
	movq	%r15, %rbx
	movzbl	2(%rbx), %ecx
	movzbl	mode_bitsize(%rcx,%rcx), %ecx
	movq	%rax, %r15
	cmpq	%r13, %rcx
	je	.LBB33_12
# BB#11:                                # %if.then49
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
.LBB33_12:                              # %if.end51
	movq	128(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	120(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	$51, %rcx
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %r8
	callq	store_field
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	emit_move_insn
	movq	%r15, %rax
	jmp	.LBB33_42
.LBB33_21:                              # %land.lhs.true71
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	direct_store(%rdx), %ecx
	testq	%rcx, %rcx
	movq	32(%rsp), %r10          # 8-byte Reload
	jne	.LBB33_23
# BB#22:                                # %land.lhs.true75
	movl	mode_class(,%rdx,4), %ecx
	addq	$-5, %rcx
	andq	%rsi, %rcx
	cmpq	$1, %rcx
	ja	.LBB33_27
.LBB33_23:                              # %lor.lhs.false85
	orq	$2, %rax
	cmpq	$63, %rax
	je	.LBB33_27
# BB#24:                                # %lor.lhs.false95
	testq	%r13, %r13
	js	.LBB33_51
# BB#25:                                # %land.lhs.true116
	movq	8(%r12), %rax
	movq	32(%rax), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB33_51
# BB#26:                                # %land.lhs.true126
	movq	%r13, %rsi
	movq	%r10, 32(%rsp)          # 8-byte Spill
	callq	compare_tree_int
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	32(%rsp), %r10          # 8-byte Reload
	testq	%rsi, %rax
	je	.LBB33_51
.LBB33_27:                              # %if.then134
	movq	%r10, 32(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	callq	expand_expr
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	jne	.LBB33_29
.LBB33_28:
	movq	48(%rsp), %r12          # 8-byte Reload
	movq	32(%rsp), %r9           # 8-byte Reload
	jmp	.LBB33_33
.LBB33_29:                              # %if.then134
	cmpq	$51, %rbx
	movq	32(%rsp), %r9           # 8-byte Reload
	je	.LBB33_32
# BB#30:                                # %land.lhs.true141
	movq	8(%r12), %rax
	movl	60(%rax), %esi
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	cmpq	%rbx, %rsi
	je	.LBB33_32
# BB#31:                                # %if.then151
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%r9, %rbx
	callq	convert_modes
	movq	%rbx, %r9
	movq	%rax, 40(%rsp)          # 8-byte Spill
.LBB33_32:                              # %if.end160
	movq	48(%rsp), %r12          # 8-byte Reload
.LBB33_33:                              # %if.end160
	movl	(%r9), %eax
	movq	%rax, %rcx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$3342336, %rcx          # imm = 0x330000
	jne	.LBB33_43
# BB#34:                                # %land.lhs.true166
	movq	40(%rsp), %rcx          # 8-byte Reload
	movl	(%rcx), %ecx
	movq	%rcx, %rdx
	andq	$16711680, %rdx         # imm = 0xFF0000
	cmpq	$3342336, %rdx          # imm = 0x330000
	jne	.LBB33_43
# BB#35:                                # %if.then172
	movq	%r12, %r14
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	movq	56(%rsp), %rdx          # 8-byte Reload
	jne	.LBB33_38
# BB#36:                                # %if.then172
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB33_38
# BB#37:                                # %if.then172
	movq	%rdx, %rax
	andq	$7, %rax
	jne	.LBB33_38
# BB#39:                                # %if.end187
	movq	$63, %rcx
	movq	%rdx, %rax
                                        # kill: RDX<def> RAX<kill>
	sarq	%cl, %rdx
	movq	$61, %rcx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	xorq	%rsi, %rsi
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r9, %rdi
	callq	adjust_address_1
	movq	%rax, %rbx
	leaq	7(%r13), %rax
	movq	$63, %rcx
	sarq	%cl, %rax
	movq	$61, %rcx
	shrq	%cl, %rax
	leaq	7(%r13,%rax), %rsi
	movq	$3, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rbx, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	emit_block_move
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	jne	.LBB33_41
	jmp	.LBB33_40
.LBB33_43:                              # %if.end199
	movq	120(%rsp), %rdi
	movq	%r9, %rbx
	callq	int_size_in_bytes
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	%r14, %rcx
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	%rax, %r9
	callq	store_bit_field
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r12
	je	.LBB33_40
# BB#44:                                # %if.then204
	testq	%r15, %r15
	je	.LBB33_50
# BB#45:                                # %land.lhs.true207
	movl	(%rbx), %eax
	andq	$134283263, %rax        # imm = 0x800FFFF
	cmpq	$134217794, %rax        # imm = 0x8000042
	jne	.LBB33_46
.LBB33_50:                              # %if.end238
	movq	120(%rsp), %rdi
	callq	int_size_in_bytes
	movq	%rax, 8(%rsp)
	xorq	%r8, %r8
	movq	$0, (%rsp)
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	112(%rsp), %rcx
	movq	%r12, %r9
	callq	extract_bit_field
	jmp	.LBB33_42
.LBB33_40:                              # %if.end241
	movq	const_int_rtx+512(%rip), %rbx
.LBB33_41:                              # %return
	movq	%rbx, %rax
.LBB33_42:                              # %return
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB33_51:                              # %if.else242
	movq	48(%rsp), %r15          # 8-byte Reload
	testq	%rsi, %r15
	je	.LBB33_59
# BB#52:                                # %land.lhs.true247
	movq	8(%r10), %rdi
	movzbl	(%rdi), %ecx
	addq	$-54, %rcx
	andq	%rsi, %rcx
	cmpq	$21, %rcx
	ja	.LBB33_58
# BB#53:                                # %land.lhs.true247
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24723, %rax            # imm = 0x6093
	jne	.LBB33_59
# BB#54:                                # %land.lhs.true247
	cmpq	$21, %rcx
	jne	.LBB33_58
# BB#55:                                # %land.lhs.true282
	movq	16(%rdi), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB33_58
# BB#56:                                # %land.lhs.true290
	movq	8(%rdi), %rax
	movq	global_rtl+48(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB33_59
# BB#57:                                # %land.lhs.true290
	movq	global_rtl+56(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB33_59
.LBB33_58:                              # %if.then302
	movq	%r10, %rbx
	movq	%rsi, %r13
	callq	copy_to_reg
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	replace_equiv_address
	movq	%r13, %rsi
	movq	%rbx, %r10
.LBB33_59:                              # %if.end305
	movq	%rsi, %r13
	movq	$63, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, %rdx
	sarq	%cl, %rdx
	movq	$61, %rcx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r10, %rdi
	movq	%r14, %rsi
	movq	%r10, %rbx
	callq	adjust_address_1
	movq	%rbx, %rdi
	movq	%rax, %r14
	cmpq	%rdi, %r14
	jne	.LBB33_61
# BB#60:                                # %if.then310
	callq	copy_rtx
	movq	%rax, %r14
.LBB33_61:                              # %do.body
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$1879048191, %rcx       # imm = 0x6FFFFFFF
	orq	$268435456, %rcx        # imm = 0x10000000
	movl	%ecx, (%r14)
	testq	$16777216, %rax         # imm = 0x1000000
	jne	.LBB33_65
# BB#62:                                # %land.lhs.true321
	movq	16(%r14), %rax
	testq	%rax, %rax
	je	.LBB33_65
# BB#63:                                # %cond.false328
	cmpq	$0, (%rax)
	je	.LBB33_65
# BB#64:                                # %if.then336
	movq	$32, %rcx
	movq	128(%rsp), %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%r14, %rdi
	callq	set_mem_alias_set
.LBB33_65:                              # %if.end338
	testq	%r13, %r15
	je	.LBB33_66
# BB#67:                                # %if.end338
	movabsq	$1, %rdx
	jmp	.LBB33_68
.LBB33_46:                              # %if.then217
	movq	40(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %eax
	testq	%rax, %rax
	je	.LBB33_48
# BB#47:                                # %if.then217
	movq	%rax, %r12
.LBB33_48:                              # %if.then217
	movq	112(%rsp), %rax
	testq	%rcx, %rax
	je	.LBB33_49
# BB#71:                                # %if.then226
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	expand_and  # TAILCALL
.LBB33_66:
	movabsq	$0, %rdx
.LBB33_68:                              # %if.end338
	movq	%r12, %rdi
	movq	%r14, %rsi
	jmp	.LBB33_69
.LBB33_49:                              # %if.end230
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %edi
	subq	%r13, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%r12, %rbx
	movq	%rax, %r14
	movq	$82, %rdi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rbx, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%r14, %rcx
	callq	expand_shift
	movq	$83, %rdi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rbx, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	expand_shift  # TAILCALL
.LBB33_15:                              # %if.then62
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.store_field, %rcx
	movq	$5092, %rsi             # imm = 0x13E4
	jmp	.LBB33_16
.LBB33_38:                              # %if.then186
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.store_field, %rcx
	movq	$5145, %rsi             # imm = 0x1419
.LBB33_16:                              # %if.then62
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp478:
	.size	store_field, .Ltmp478-store_field
	.cfi_endproc

	.globl	store_expr
	.align	16, 0x90
	.type	store_expr,@function
store_expr:                             # @store_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp485:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp486:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp487:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp488:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp489:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp490:
	.cfi_def_cfa_offset 96
.Ltmp491:
	.cfi_offset %rbx, -48
.Ltmp492:
	.cfi_offset %r12, -40
.Ltmp493:
	.cfi_offset %r13, -32
.Ltmp494:
	.cfi_offset %r14, -24
.Ltmp495:
	.cfi_offset %r15, -16
	movq	%rdx, %r13
	movq	%rsi, %r15
	movq	%rdi, %r14
	jmp	.LBB34_1
	.align	16, 0x90
.LBB34_9:                               # %emit_queue.exit
                                        #   in Loop: Header=BB34_1 Depth=1
	movq	40(%r14), %r14
.LBB34_1:                               # %tailrecurse
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB34_4 Depth 2
	movzbl	16(%r14), %eax
	cmpq	$51, %rax
	je	.LBB34_10
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB34_1 Depth=1
	cmpq	$47, %rax
	jne	.LBB34_36
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB34_1 Depth=1
	movq	32(%r14), %rdi
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	jmp	.LBB34_4
	.align	16, 0x90
.LBB34_8:                               # %if.end.i
                                        #   in Loop: Header=BB34_4 Depth=2
	movq	cfun(%rip), %rax
	movq	40(%rbx), %rcx
	movq	16(%rax), %rax
	movq	%rcx, 40(%rax)
.LBB34_4:                               # %if.then
                                        #   Parent Loop BB34_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB34_9
# BB#5:                                 # %while.body.i
                                        #   in Loop: Header=BB34_4 Depth=2
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$24, %rax
	jne	.LBB34_7
# BB#6:                                 # %if.then.i
                                        #   in Loop: Header=BB34_4 Depth=2
	movq	8(%rdi), %rax
	movq	8(%rax), %rax
	movq	%rax, 16(%rbx)
	callq	emit_insn
	jmp	.LBB34_8
	.align	16, 0x90
.LBB34_7:                               # %if.else.i
                                        #   in Loop: Header=BB34_4 Depth=2
	callq	emit_insn
	movq	%rax, 16(%rbx)
	jmp	.LBB34_8
.LBB34_10:                              # %land.lhs.true
	movl	(%r15), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$3342336, %rax          # imm = 0x330000
	jne	.LBB34_36
# BB#11:                                # %if.then14
	movq	%r13, 40(%rsp)          # 8-byte Spill
	callq	gen_label_rtx
	movq	%rax, %r13
	callq	gen_label_rtx
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	40(%rcx), %rbx
	movq	%rax, %r12
	jmp	.LBB34_12
.LBB34_36:                              # %if.else36
	movq	%r15, %rdi
	callq	queued_subexp_p
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB34_46
# BB#37:                                # %if.then39
	movzbl	2(%r15), %edx
	testq	%rdx, %rdx
	je	.LBB34_40
# BB#38:                                # %if.then39
	cmpq	$51, %rdx
	jne	.LBB34_39
.LBB34_40:                              # %if.else58
	movq	%r13, 40(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	jmp	.LBB34_41
	.align	16, 0x90
.LBB34_16:                              # %if.end.i956
                                        #   in Loop: Header=BB34_12 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rbx), %rcx
	movq	16(%rax), %rax
	movq	%rcx, 40(%rax)
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rbx
.LBB34_12:                              # %if.then14
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB34_17
# BB#13:                                # %while.body.i939
                                        #   in Loop: Header=BB34_12 Depth=1
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$24, %rax
	jne	.LBB34_15
# BB#14:                                # %if.then.i945
                                        #   in Loop: Header=BB34_12 Depth=1
	movq	8(%rdi), %rax
	movq	8(%rax), %rax
	movq	%rax, 16(%rbx)
	callq	emit_insn
	jmp	.LBB34_16
	.align	16, 0x90
.LBB34_15:                              # %if.else.i948
                                        #   in Loop: Header=BB34_12 Depth=1
	callq	emit_insn
	movq	%rax, 16(%rbx)
	jmp	.LBB34_16
.LBB34_46:                              # %if.else71
	movl	(%r15), %edx
	testq	%rbx, %r13
	je	.LBB34_57
# BB#47:                                # %if.else71
	movq	%rdx, %rax
	andq	$134283263, %rax        # imm = 0x800FFFF
	andq	%rbx, %rax
	cmpq	$66, %rax
	jne	.LBB34_57
# BB#48:                                # %land.lhs.true82
	movq	$16, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$51, %rax
	jne	.LBB34_49
.LBB34_57:                              # %if.else115
	movq	%rdx, %rax
	andq	$268500991, %rax        # imm = 0x1000FFFF
	cmpq	$268435519, %rax        # imm = 0x1000003F
	jne	.LBB34_88
# BB#58:                                # %if.then124
	xorq	%rsi, %rsi
	testq	%rbx, %r13
	jne	.LBB34_68
# BB#59:                                # %land.lhs.true126
	movq	%rbx, %r12
	movq	8(%r14), %rax
	movl	16(%rax), %ebx
	movq	%rbx, %rcx
	andq	$255, %rcx
	cmpq	$12, %rcx
	jbe	.LBB34_61
# BB#60:
	movq	%r12, %rbx
	jmp	.LBB34_68
.LBB34_17:                              # %emit_queue.exit957
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	protect_from_queue
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rcx
	movl	4(%rcx), %edx
	movq	%rax, %r15
	testq	%rdx, %rdx
	jne	.LBB34_21
# BB#18:                                # %if.then.i960
	movslq	(%rcx), %rsi
	xorq	%rbx, %rbx
	testq	%rsi, %rsi
	movabsq	$0, %rdx
	je	.LBB34_20
# BB#19:                                # %if.then3.i
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	adjust_stack
	movq	cfun(%rip), %rax
	movq	16(%rax), %rcx
	movl	4(%rcx), %edx
.LBB34_20:                              # %if.end.i962
	movl	%ebx, (%rcx)
.LBB34_21:                              # %do_pending_stack_adjust.exit
	incq	%rdx
	movl	%edx, 4(%rcx)
	movq	32(%r14), %rdi
	xorq	%rdx, %rdx
	movq	%r13, %rsi
	callq	do_jump
	callq	start_cleanup_deferral
	movq	40(%r14), %rdi
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	callq	store_expr
	callq	end_cleanup_deferral
	jmp	.LBB34_22
	.align	16, 0x90
.LBB34_26:                              # %if.end.i988
                                        #   in Loop: Header=BB34_22 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rbx), %rcx
	movq	16(%rax), %rax
	movq	%rcx, 40(%rax)
.LBB34_22:                              # %do_pending_stack_adjust.exit
                                        # =>This Inner Loop Header: Depth=1
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB34_27
# BB#23:                                # %while.body.i971
                                        #   in Loop: Header=BB34_22 Depth=1
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$24, %rax
	jne	.LBB34_25
# BB#24:                                # %if.then.i977
                                        #   in Loop: Header=BB34_22 Depth=1
	movq	8(%rdi), %rax
	movq	8(%rax), %rax
	movq	%rax, 16(%rbx)
	callq	emit_insn
	jmp	.LBB34_26
	.align	16, 0x90
.LBB34_25:                              # %if.else.i980
                                        #   in Loop: Header=BB34_22 Depth=1
	callq	emit_insn
	movq	%rax, 16(%rbx)
	jmp	.LBB34_26
.LBB34_27:                              # %emit_queue.exit989
	movq	%r12, %rdi
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	movq	%r13, %rdi
	callq	emit_label
	callq	start_cleanup_deferral
	movq	48(%r14), %rdi
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	callq	store_expr
	callq	end_cleanup_deferral
	jmp	.LBB34_28
	.align	16, 0x90
.LBB34_32:                              # %if.end.i929
                                        #   in Loop: Header=BB34_28 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rbx), %rcx
	movq	16(%rax), %rax
	movq	%rcx, 40(%rax)
.LBB34_28:                              # %emit_queue.exit989
                                        # =>This Inner Loop Header: Depth=1
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB34_33
# BB#29:                                # %while.body.i912
                                        #   in Loop: Header=BB34_28 Depth=1
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$24, %rax
	jne	.LBB34_31
# BB#30:                                # %if.then.i918
                                        #   in Loop: Header=BB34_28 Depth=1
	movq	8(%rdi), %rax
	movq	8(%rax), %rax
	movq	%rax, 16(%rbx)
	callq	emit_insn
	jmp	.LBB34_32
	.align	16, 0x90
.LBB34_31:                              # %if.else.i921
                                        #   in Loop: Header=BB34_28 Depth=1
	callq	emit_insn
	movq	%rax, 16(%rbx)
	jmp	.LBB34_32
.LBB34_33:                              # %emit_queue.exit930
	movq	%r12, %rdi
	callq	emit_label
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	decq	%rcx
	movl	%ecx, 4(%rax)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	40(%rsp), %rcx          # 8-byte Reload
	andq	%rax, %rcx
	je	.LBB34_35
# BB#34:                                # %emit_queue.exit930
	movq	%r15, %rcx
.LBB34_35:                              # %emit_queue.exit930
	movq	%rcx, %rax
	jmp	.LBB34_153
.LBB34_39:                              # %if.then49
	movq	%r13, 40(%rsp)          # 8-byte Spill
	movq	%rdx, %rdi
	callq	gen_reg_rtx
	movzbl	2(%r15), %edx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rax, %rsi
.LBB34_41:                              # %if.end
	callq	expand_expr
	movq	%rax, %r12
	movzbl	3(%r15), %eax
	testq	$8, %rax
	movabsq	$0, %rax
	movabsq	$1, %rcx
	movq	%rax, %r13
	jne	.LBB34_43
# BB#42:                                # %if.end
	movq	%rcx, %r13
.LBB34_43:                              # %if.end
	movq	40(%rsp), %rdx          # 8-byte Reload
	testq	%rbx, %rdx
	je	.LBB34_45
# BB#44:                                # %if.end
	movq	%rcx, %rax
.LBB34_45:                              # %if.end
	andq	%rax, %r13
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
.LBB34_104:                             # %if.end403
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$133, %rcx
	jg	.LBB34_107
# BB#105:                               # %if.end403
	addq	$-54, %rcx
	andq	%rbx, %rcx
	cmpq	$14, %rcx
	ja	.LBB34_113
# BB#106:                               # %if.end403
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB34_109
	jmp	.LBB34_113
.LBB34_107:                             # %if.end403
	cmpq	$134, %rcx
	je	.LBB34_109
# BB#108:                               # %if.end403
	cmpq	$140, %rcx
	jne	.LBB34_113
.LBB34_109:                             # %land.lhs.true435
	testq	$16711680, %rax         # imm = 0xFF0000
	jne	.LBB34_113
# BB#110:                               # %land.lhs.true440
	movzbl	16(%r14), %eax
	testq	%rax, %rax
	je	.LBB34_113
# BB#111:                               # %land.lhs.true446
	movq	8(%r14), %rax
	movl	60(%rax), %esi
	movzbl	2(%r15), %edi
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	cmpq	%rsi, %rdi
	je	.LBB34_113
# BB#112:                               # %if.then458
	movl	16(%rax), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	convert_modes
	movq	%rax, %r12
.LBB34_113:                             # %if.end477
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	rtx_equal_p
	testq	%rbx, %rax
	je	.LBB34_118
# BB#114:                               # %lor.lhs.false480
	cmpq	%r15, %r12
	je	.LBB34_115
# BB#116:                               # %land.lhs.true482
	movq	%r12, %rdi
	callq	side_effects_p
	testq	%rbx, %rax
	jne	.LBB34_118
# BB#117:                               # %lor.lhs.false485
	movq	%r15, %rdi
	callq	side_effects_p
	testq	%rbx, %rax
	je	.LBB34_144
.LBB34_118:                             # %land.lhs.true488
	movzbl	16(%r14), %eax
	testq	%rax, %rax
	je	.LBB34_144
# BB#119:                               # %land.lhs.true488
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	jne	.LBB34_144
# BB#120:                               # %land.lhs.true496
	movzbl	tree_code_type(%rax), %eax
	cmpq	$100, %rax
	jne	.LBB34_122
# BB#121:                               # %lor.lhs.false504
	movq	144(%r14), %rax
	cmpq	%r15, %rax
	je	.LBB34_144
.LBB34_122:                             # %if.then524
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	protect_from_queue
	movq	%rax, %r15
	movl	(%r12), %eax
	movl	(%r15), %edx
	movq	$16, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	$16, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	andq	$255, %rdi
	cmpq	%rdi, %rsi
	je	.LBB34_128
# BB#123:                               # %if.then524
	testq	%rsi, %rsi
	je	.LBB34_128
# BB#124:                               # %if.then540
	movq	8(%r14), %rax
	movl	16(%rax), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	testq	%r13, %r13
	je	.LBB34_127
# BB#125:                               # %if.then549
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	convert_modes
	movq	%rax, %r12
	jmp	.LBB34_126
.LBB34_115:
	movq	%r15, %r12
	jmp	.LBB34_144
.LBB34_88:                              # %if.else337
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	xorq	%rcx, %rcx
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	expand_expr
	movl	(%r15), %ecx
	movq	%rax, %r12
	testq	%r15, %r15
	je	.LBB34_98
# BB#89:                                # %if.else337
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB34_98
# BB#90:                                # %land.lhs.true347
	movl	8(%r15), %eax
	cmpq	$53, %rax
	jb	.LBB34_99
# BB#91:                                # %land.lhs.true347
	andq	$134283263, %rcx        # imm = 0x800FFFF
	andq	%rbx, %rcx
	jmp	.LBB34_92
.LBB34_128:                             # %if.else557
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$3342336, %rax          # imm = 0x330000
	jne	.LBB34_139
# BB#129:                               # %land.lhs.true563
	movzbl	16(%r14), %ecx
	cmpq	$29, %rcx
	jne	.LBB34_139
# BB#130:                               # %if.then570
	movq	%r14, %rdi
	callq	expr_size
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB34_132
# BB#131:                               # %land.lhs.true576
	movq	8(%rax), %rdx
	movslq	32(%r14), %rcx
	cmpq	%rcx, %rdx
	jl	.LBB34_143
.LBB34_132:
	addq	$32, %r14
	movq	sizetype_tab(%rip), %rdi
	movq	%rax, %rsi
	movq	%rax, 32(%rsp)          # 8-byte Spill
	callq	make_tree
	movslq	(%r14), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	$78, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, %r14
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	expand_expr
	movl	ptr_mode(%rip), %edi
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%rax, %rdx
	callq	convert_modes
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	emit_block_move
	movzbl	(%rbx), %eax
	cmpq	$54, %rax
	jne	.LBB34_134
# BB#133:                               # %if.then598
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	movq	8(%rbx), %rsi
	negq	%rsi
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	plus_constant_wide
	movq	8(%rbx), %rdx
	movq	%rax, %rbx
	movq	$51, %rsi
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r15, %rdi
	callq	adjust_address_1
	movq	%rax, %r15
	jmp	.LBB34_135
.LBB34_49:                              # %if.then87
	movq	%r13, 40(%rsp)          # 8-byte Spill
	xorq	%rcx, %rcx
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	expand_expr
	movq	%rax, %r12
	movzbl	2(%r12), %eax
	testq	%rax, %rax
	movq	$1, %r13
	je	.LBB34_104
# BB#50:                                # %if.then87
	cmpq	$51, %rax
	je	.LBB34_104
# BB#51:                                # %if.then101
	cmpq	%r15, %r12
	je	.LBB34_55
# BB#52:                                # %lor.lhs.false
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	rtx_equal_p
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	testq	%rbx, %rax
	je	.LBB34_56
# BB#53:                                # %land.lhs.true105
	movq	%r12, %rdi
	callq	side_effects_p
	testq	%rbx, %rax
	jne	.LBB34_56
# BB#54:                                # %land.lhs.true108
	movq	%r15, %rdi
	callq	side_effects_p
	testq	%rbx, %rax
	jne	.LBB34_56
.LBB34_55:                              # %if.then111
	movq	$1, 32(%rsp)            # 8-byte Folded Spill
.LBB34_56:                              # %if.end112
	movq	%r12, %rdi
	callq	copy_to_reg
	movq	%rax, %r12
	jmp	.LBB34_104
.LBB34_139:                             # %if.else632
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$39, %rdx
	jne	.LBB34_141
# BB#140:                               # %if.then637
	movq	8(%r14), %rdi
	callq	int_size_in_bytes
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	emit_group_load
	jmp	.LBB34_144
.LBB34_98:                              # %land.lhs.true351
	andq	$134283263, %rcx        # imm = 0x800FFFF
.LBB34_92:                              # %land.lhs.true347
	cmpq	$134217794, %rcx        # imm = 0x8000042
	je	.LBB34_99
# BB#93:                                # %land.lhs.true360
	movq	%r13, 40(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	rtx_equal_p
	testq	%rbx, %rax
	jne	.LBB34_103
# BB#94:                                # %land.lhs.true363
	movzbl	(%r12), %eax
	cmpq	$133, %rax
	jg	.LBB34_100
# BB#95:                                # %land.lhs.true363
	leaq	-54(%rax), %rcx
	andq	%rbx, %rcx
	cmpq	$14, %rcx
	ja	.LBB34_101
# BB#96:                                # %land.lhs.true363
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB34_97
	jmp	.LBB34_101
.LBB34_99:
	movq	%r13, 40(%rsp)          # 8-byte Spill
.LBB34_103:
	xorq	%r13, %r13
	jmp	.LBB34_104
.LBB34_127:                             # %if.else555
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	convert_move
	jmp	.LBB34_144
.LBB34_141:                             # %if.else642
	andq	%rbx, %rax
	cmpq	$3342336, %rax          # imm = 0x330000
	jne	.LBB34_126
# BB#142:                               # %if.then648
	movq	%r14, %rdi
	callq	expr_size
.LBB34_143:                             # %if.then582
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	emit_block_move
	jmp	.LBB34_144
.LBB34_126:                             # %if.else651
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	emit_move_insn
.LBB34_144:                             # %if.end657
	xorq	%rax, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	testq	%rbx, %rcx
	je	.LBB34_153
# BB#145:                               # %if.else660
	testq	%r13, %r13
	je	.LBB34_149
# BB#146:                               # %land.lhs.true662
	movzbl	(%r12), %eax
	cmpq	$66, %rax
	jne	.LBB34_148
# BB#147:                               # %land.lhs.true662
	movq	%r15, %r12
.LBB34_148:                             # %land.lhs.true662
	movq	%r12, %rax
	jne	.LBB34_153
.LBB34_149:                             # %land.lhs.true670
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$3342336, %rcx          # imm = 0x330000
	je	.LBB34_152
# BB#150:                               # %land.lhs.true676
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB34_154
# BB#151:                               # %land.lhs.true681
	movl	8(%r15), %eax
	cmpq	$53, %rax
	jae	.LBB34_154
.LBB34_152:
	movq	%r15, %rax
	jmp	.LBB34_153
.LBB34_154:                             # %if.then687
	movq	%r15, %rdi
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	copy_to_reg  # TAILCALL
.LBB34_61:                              # %land.lhs.true126
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	$7232, %rdi             # imm = 0x1C40
	je	.LBB34_62
# BB#63:                                # %land.lhs.true157
	cmpq	$0, 8(%rax)
	je	.LBB34_65
# BB#64:
	movq	%r12, %rbx
	jmp	.LBB34_68
.LBB34_100:                             # %land.lhs.true363
	cmpq	$134, %rax
	je	.LBB34_97
.LBB34_101:                             # %lor.lhs.false391
	cmpq	$140, %rax
	je	.LBB34_97
# BB#102:                               # %lor.lhs.false391
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	je	.LBB34_103
.LBB34_97:                              # %if.then397
	movq	$1, %r13
	jmp	.LBB34_104
.LBB34_134:                             # %if.else608
	movq	sizetype_tab(%rip), %rax
	movl	60(%rax), %edi
	movq	optab_table+16(%rip), %rsi
	movq	$9, %rcx
	shrq	%cl, %rdi
	andq	$127, %rdi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, %rcx
	callq	expand_binop
	movq	%r14, %rdi
	movq	%rax, %r14
	callq	highest_pow2_factor
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	offset_address
	movq	%rax, %r15
	callq	gen_label_rtx
	movq	const_int_rtx+512(%rip), %rsi
	movzbl	2(%r14), %r8d
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$107, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	movq	%r14, %rbx
	callq	emit_cmp_and_jump_insns
.LBB34_135:                             # %if.end621
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%rax, %rbx
	movq	%rbx, %rsi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	je	.LBB34_137
# BB#136:                               # %if.then624
	movq	%r15, %rdi
	callq	clear_storage
.LBB34_137:                             # %if.end626
	movq	24(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB34_144
# BB#138:                               # %if.then628
	callq	emit_label
	jmp	.LBB34_144
.LBB34_65:                              # %if.then163
	movq	$13, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	movq	$26, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	andq	$1, %rdi
	cmpq	%rdi, %rbx
	je	.LBB34_67
# BB#66:                                # %if.then174
	movq	%rax, %rsi
	callq	signed_or_unsigned_type
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	convert
	movl	(%r15), %edx
	movq	%rax, %r14
.LBB34_67:                              # %if.end182
	movq	%r12, %rbx
	movq	8(%r15), %rax
	movzbl	2(%rax), %edi
	movq	$26, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%rdx, %rsi
	callq	type_for_mode
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	convert
	movq	8(%r15), %rsi
	movq	%rax, %r14
	jmp	.LBB34_68
.LBB34_62:
	movq	%r12, %rbx
.LBB34_68:                              # %if.end195
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	expand_expr
	movq	%rax, %r12
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB34_73
# BB#69:                                # %if.end195
	testq	%rbx, %r13
	je	.LBB34_73
# BB#70:                                # %land.lhs.true202
	testq	$134217728, %rax        # imm = 0x8000000
	jne	.LBB34_72
# BB#71:                                # %lor.lhs.false207
	movq	8(%r15), %rdi
	movq	8(%r12), %rsi
	callq	reg_mentioned_p
	testq	%rbx, %rax
	je	.LBB34_73
.LBB34_72:                              # %if.then216
	movq	%r12, %rdi
	callq	copy_to_reg
	movq	%rax, %r12
.LBB34_73:                              # %if.end218
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$133, %rcx
	jg	.LBB34_76
# BB#74:                                # %if.end218
	addq	$-54, %rcx
	andq	%rbx, %rcx
	cmpq	$14, %rcx
	ja	.LBB34_80
# BB#75:                                # %if.end218
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB34_78
	jmp	.LBB34_80
.LBB34_76:                              # %if.end218
	cmpq	$134, %rcx
	je	.LBB34_78
# BB#77:                                # %if.end218
	cmpq	$140, %rcx
	jne	.LBB34_80
.LBB34_78:                              # %land.lhs.true250
	testq	$16711680, %rax         # imm = 0xFF0000
	jne	.LBB34_80
# BB#79:                                # %if.then255
	movl	(%r15), %eax
	movq	8(%r14), %rdx
	movq	$16, %rcx
	movq	%rax, %rdi
	shrq	%cl, %rdi
	andq	$255, %rdi
	movl	60(%rdx), %esi
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	movq	$26, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	convert_modes
	movq	8(%r15), %rdx
	movq	%rbx, %r8
	movl	(%r15), %ebx
	movq	$16, %rcx
	movq	%rbx, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	movzbl	2(%rdx), %edi
	movq	$26, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	movq	%rax, %rdx
	movq	%rbx, %rcx
	movq	%r8, %rbx
	callq	convert_modes
	movq	%rax, %r12
.LBB34_80:                              # %if.end282
	movl	(%r15), %edx
	movq	8(%r15), %rdi
	movq	$26, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%r12, %rsi
	callq	convert_move
	testq	%rbx, %r13
	je	.LBB34_85
# BB#81:                                # %land.lhs.true290
	movl	(%r15), %eax
	movzbl	2(%r12), %edx
	movq	$16, %rcx
	movq	%rax, %rdi
	shrq	%cl, %rdi
	andq	$255, %rdi
	cmpq	%rdi, %rdx
	je	.LBB34_85
# BB#82:                                # %if.then298
	testq	%rdx, %rdx
	je	.LBB34_84
# BB#83:                                # %if.then303
	movq	%r12, %rsi
	callq	gen_lowpart_SUBREG
	movq	%rax, %r12
	movl	(%r12), %eax
	orq	$268435456, %rax        # imm = 0x10000000
	movl	%eax, (%r12)
	movl	(%r15), %ecx
	andq	$67108864, %rcx         # imm = 0x4000000
	movabsq	$4227858431, %rdx       # imm = 0xFBFFFFFF
	andq	%rax, %rdx
	orq	%rcx, %rdx
	movl	%edx, (%r12)
	jmp	.LBB34_85
.LBB34_84:                              # %if.else316
	movq	8(%r15), %rcx
	movzbl	2(%rcx), %esi
	movq	$26, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	convert_modes
	movq	%rax, %r12
.LBB34_85:                              # %if.end331
	andq	%rbx, %r13
	je	.LBB34_87
# BB#86:                                # %if.end331
	movq	%r12, %r13
.LBB34_87:                              # %if.end331
	movq	%r13, %rax
.LBB34_153:                             # %return
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp496:
	.size	store_expr, .Ltmp496-store_expr
	.cfi_endproc

	.globl	do_pending_stack_adjust
	.align	16, 0x90
	.type	do_pending_stack_adjust,@function
do_pending_stack_adjust:                # @do_pending_stack_adjust
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp498:
	.cfi_def_cfa_offset 16
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB35_4
# BB#1:                                 # %if.then
	movslq	(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB35_3
# BB#2:                                 # %if.then3
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	adjust_stack
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
.LBB35_3:                               # %if.end
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.LBB35_4:                               # %if.end8
	popq	%rax
	retq
.Ltmp499:
	.size	do_pending_stack_adjust, .Ltmp499-do_pending_stack_adjust
	.cfi_endproc

	.globl	jumpifnot
	.align	16, 0x90
	.type	jumpifnot,@function
jumpifnot:                              # @jumpifnot
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rdx, %rdx
	jmp	do_jump  # TAILCALL
.Ltmp500:
	.size	jumpifnot, .Ltmp500-jumpifnot
	.cfi_endproc

	.align	16, 0x90
	.type	highest_pow2_factor,@function
highest_pow2_factor:                    # @highest_pow2_factor
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp505:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp506:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp507:
	.cfi_def_cfa_offset 32
.Ltmp508:
	.cfi_offset %rbx, -32
.Ltmp509:
	.cfi_offset %r14, -24
.Ltmp510:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	$1, %r14
	jmp	.LBB37_1
	.align	16, 0x90
.LBB37_13:                              # %sw.bb51
                                        #   in Loop: Header=BB37_1 Depth=1
	addq	$32, %rbx
	movq	(%rbx), %rbx
.LBB37_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rbx), %ecx
	movq	%rcx, %rax
	andq	$255, %rax
	cmpq	$113, %rax
	jg	.LBB37_11
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB37_1 Depth=1
	cmpq	$46, %rax
	jle	.LBB37_3
# BB#8:                                 # %tailrecurse
                                        #   in Loop: Header=BB37_1 Depth=1
	cmpq	$77, %rax
	jg	.LBB37_14
# BB#9:                                 # %tailrecurse
                                        #   in Loop: Header=BB37_1 Depth=1
	addq	$-47, %rax
	andq	%r15, %rax
	cmpq	$18, %rax
	ja	.LBB37_7
# BB#10:                                # %tailrecurse
                                        #   in Loop: Header=BB37_1 Depth=1
	jmpq	*.LJTI37_0(,%rax,8)
.LBB37_24:                              # %sw.bb56
                                        #   in Loop: Header=BB37_1 Depth=1
	addq	$40, %rbx
	movq	(%rbx), %rbx
	jmp	.LBB37_1
	.align	16, 0x90
.LBB37_11:                              # %tailrecurse
                                        #   in Loop: Header=BB37_1 Depth=1
	addq	$-114, %rax
	andq	%r15, %rax
	cmpq	$4, %rax
	ja	.LBB37_7
# BB#12:                                # %tailrecurse
                                        #   in Loop: Header=BB37_1 Depth=1
	cmpq	$3, %rax
	jne	.LBB37_13
	jmp	.LBB37_7
.LBB37_3:                               # %tailrecurse
	cmpq	$25, %rax
	jne	.LBB37_7
# BB#4:                                 # %sw.bb
	testq	$262144, %rcx           # imm = 0x40000
	movq	$128, %r14
	jne	.LBB37_7
# BB#5:                                 # %if.else
	movq	32(%rbx), %rax
	movq	%rax, %r14
	negq	%r14
	andq	%rax, %r14
	jne	.LBB37_7
# BB#6:                                 # %if.else
	movq	$128, %r14
	jmp	.LBB37_7
.LBB37_14:                              # %tailrecurse
	addq	$-78, %rax
	andq	%r15, %rax
	cmpq	$2, %rax
	jae	.LBB37_7
.LBB37_15:                              # %sw.bb6
	movq	32(%rbx), %rdi
	callq	highest_pow2_factor
	movq	40(%rbx), %rdi
.LBB37_16:                              # %sw.bb6
	movq	%rax, %rbx
	callq	highest_pow2_factor
	cmpq	%rax, %rbx
	jle	.LBB37_18
	jmp	.LBB37_17
.LBB37_21:                              # %sw.bb25
	movq	40(%rbx), %rdi
	callq	integer_pow2p
	testq	%r15, %rax
	je	.LBB37_7
# BB#22:                                # %land.lhs.true
	movq	40(%rbx), %rdi
	movq	$1, %rsi
	movq	$1, %r14
	callq	host_integerp
	testq	%r15, %rax
	je	.LBB37_7
# BB#23:                                # %if.then36
	movq	32(%rbx), %rdi
	callq	highest_pow2_factor
	movq	40(%rbx), %rdi
	movq	%rax, %r14
	movq	$1, %rsi
	movq	$1, %rbx
	callq	tree_low_cst
	movq	%rax, %rcx
	movq	%r14, %rax
	cqto
	idivq	%rcx
	testq	%rax, %rax
	jle	.LBB37_18
.LBB37_17:                              # %sw.bb6
	movq	%rax, %rbx
.LBB37_18:                              # %sw.bb6
	movq	%rbx, %rax
	jmp	.LBB37_19
.LBB37_7:                               # %return
	movq	%r14, %rax
.LBB37_19:                              # %sw.bb6
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB37_25:                              # %sw.bb61
	movq	40(%rbx), %rdi
	callq	highest_pow2_factor
	movq	48(%rbx), %rdi
	jmp	.LBB37_16
.LBB37_20:                              # %sw.bb16
	movq	32(%rbx), %rdi
	callq	highest_pow2_factor
	movq	40(%rbx), %rdi
	movq	%rax, %rbx
	callq	highest_pow2_factor
	imulq	%rbx, %rax
	jmp	.LBB37_19
.Ltmp511:
	.size	highest_pow2_factor, .Ltmp511-highest_pow2_factor
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI37_0:
	.quad	.LBB37_24
	.quad	.LBB37_7
	.quad	.LBB37_7
	.quad	.LBB37_7
	.quad	.LBB37_25
	.quad	.LBB37_7
	.quad	.LBB37_7
	.quad	.LBB37_7
	.quad	.LBB37_7
	.quad	.LBB37_7
	.quad	.LBB37_7
	.quad	.LBB37_13
	.quad	.LBB37_15
	.quad	.LBB37_15
	.quad	.LBB37_20
	.quad	.LBB37_21
	.quad	.LBB37_21
	.quad	.LBB37_21
	.quad	.LBB37_21

	.text
	.globl	find_placeholder
	.align	16, 0x90
	.type	find_placeholder,@function
find_placeholder:                       # @find_placeholder
	.cfi_startproc
# BB#0:                                 # %entry
	movq	8(%rdi), %rax
	testq	%rsi, %rsi
	je	.LBB38_2
# BB#1:                                 # %land.lhs.true
	movq	(%rsi), %rcx
	testq	%rcx, %rcx
	jne	.LBB38_3
.LBB38_2:
	movabsq	$placeholder_list, %rcx
.LBB38_3:                               # %for.cond.preheader
	movq	(%rcx), %r8
	testq	%r8, %r8
	je	.LBB38_30
# BB#4:                                 # %for.body.lr.ph
	movq	128(%rax), %r9
.LBB38_5:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB38_7 Depth 2
                                        #     Child Loop BB38_15 Depth 2
	movq	24(%r8), %rdx
	testq	%rdx, %rdx
	je	.LBB38_29
# BB#6:                                 # %for.body7.lr.ph
                                        #   in Loop: Header=BB38_5 Depth=1
	movq	%rdx, %rax
	.align	16, 0x90
.LBB38_7:                               # %for.body7
                                        #   Parent Loop BB38_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rax), %rdi
	movq	128(%rdi), %rdi
	cmpq	%r9, %rdi
	je	.LBB38_8
# BB#11:                                # %for.inc
                                        #   in Loop: Header=BB38_7 Depth=2
	movzbl	16(%rax), %edi
	cmpq	$47, %rdi
	je	.LBB38_13
# BB#12:                                # %for.inc
                                        #   in Loop: Header=BB38_7 Depth=2
	cmpq	$51, %rdi
	jne	.LBB38_21
.LBB38_13:                              # %cond.true23
                                        #   in Loop: Header=BB38_7 Depth=2
	addq	$40, %rax
	jmp	.LBB38_14
	.align	16, 0x90
.LBB38_21:                              # %cond.false25
                                        #   in Loop: Header=BB38_7 Depth=2
	movzbl	tree_code_type(%rdi), %edi
	leaq	-49(%rdi), %rcx
	andq	$255, %rcx
	cmpq	$2, %rcx
	jb	.LBB38_24
# BB#22:                                # %cond.false25
                                        #   in Loop: Header=BB38_7 Depth=2
	andq	$255, %rdi
	cmpq	$101, %rdi
	je	.LBB38_24
# BB#23:                                # %cond.false25
                                        #   in Loop: Header=BB38_7 Depth=2
	cmpq	$114, %rdi
	jne	.LBB38_15
.LBB38_24:                              # %cond.true63
                                        #   in Loop: Header=BB38_7 Depth=2
	addq	$32, %rax
.LBB38_14:                              # %for.cond5.backedge
                                        #   in Loop: Header=BB38_7 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB38_7
	jmp	.LBB38_15
	.align	16, 0x90
.LBB38_27:                              # %cond.true123
                                        #   in Loop: Header=BB38_15 Depth=2
	addq	$40, %rdx
	movq	(%rdx), %rdx
.LBB38_15:                              # %for.end
                                        #   Parent Loop BB38_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rdx, %rdx
	je	.LBB38_29
# BB#16:                                # %for.body77
                                        #   in Loop: Header=BB38_15 Depth=2
	movq	8(%rdx), %rax
	movl	16(%rax), %ecx
	orq	$2, %rcx
	andq	$255, %rcx
	cmpq	$15, %rcx
	jne	.LBB38_25
# BB#17:                                # %land.lhs.true95
                                        #   in Loop: Header=BB38_15 Depth=2
	movq	8(%rax), %rax
	movq	128(%rax), %rax
	cmpq	%r9, %rax
	je	.LBB38_18
.LBB38_25:                              # %for.inc109
                                        #   in Loop: Header=BB38_15 Depth=2
	movzbl	16(%rdx), %eax
	cmpq	$47, %rax
	je	.LBB38_27
# BB#26:                                # %for.inc109
                                        #   in Loop: Header=BB38_15 Depth=2
	cmpq	$51, %rax
	je	.LBB38_27
# BB#31:                                # %cond.false127
                                        #   in Loop: Header=BB38_15 Depth=2
	movzbl	tree_code_type(%rax), %eax
	leaq	-49(%rax), %rcx
	andq	$255, %rcx
	cmpq	$2, %rcx
	jb	.LBB38_34
# BB#32:                                # %cond.false127
                                        #   in Loop: Header=BB38_15 Depth=2
	andq	$255, %rax
	cmpq	$101, %rax
	je	.LBB38_34
# BB#33:                                # %cond.false127
                                        #   in Loop: Header=BB38_15 Depth=2
	cmpq	$114, %rax
	jne	.LBB38_29
.LBB38_34:                              # %cond.true167
                                        #   in Loop: Header=BB38_15 Depth=2
	addq	$32, %rdx
	movq	(%rdx), %rdx
	jmp	.LBB38_15
	.align	16, 0x90
.LBB38_29:                              # %for.inc177
                                        #   in Loop: Header=BB38_5 Depth=1
	movq	(%r8), %r8
	testq	%r8, %r8
	jne	.LBB38_5
.LBB38_30:
	xorq	%rax, %rax
	retq
.LBB38_8:                               # %if.then
	testq	%rsi, %rsi
	je	.LBB38_10
# BB#9:                                 # %if.then14
	movq	%r8, (%rsi)
.LBB38_10:                              # %return
	retq
.LBB38_18:                              # %if.then104
	testq	%rsi, %rsi
	je	.LBB38_20
# BB#19:                                # %if.then106
	movq	%r8, (%rsi)
.LBB38_20:                              # %if.end107
	movq	$41, %rdi
	movq	%r9, %rsi
	jmp	build1  # TAILCALL
.Ltmp512:
	.size	find_placeholder, .Ltmp512-find_placeholder
	.cfi_endproc

	.globl	handled_component_p
	.align	16, 0x90
	.type	handled_component_p,@function
handled_component_p:                    # @handled_component_p
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	16(%rdi), %ecx
	cmpq	$113, %rcx
	movq	$1, %rax
	jg	.LBB39_4
# BB#1:                                 # %entry
	cmpq	$44, %rcx
	ja	.LBB39_3
# BB#2:                                 # %entry
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$28037546508288, %rcx   # imm = 0x198000000000
	testq	%rcx, %rdx
	jne	.LBB39_9
	jmp	.LBB39_3
.LBB39_4:                               # %entry
	leaq	-116(%rcx), %rsi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rsi
	cmpq	$2, %rsi
	jb	.LBB39_9
# BB#5:                                 # %entry
	addq	$-114, %rcx
	andq	%rdx, %rcx
	cmpq	$1, %rcx
	ja	.LBB39_3
# BB#6:                                 # %sw.bb1
	movq	32(%rdi), %rax
	movq	8(%rdi), %rcx
	movq	8(%rax), %rax
	movl	60(%rcx), %ecx
	movl	60(%rax), %eax
	xorq	%rcx, %rax
	testq	$65024, %rax            # imm = 0xFE00
	jne	.LBB39_7
# BB#8:                                 # %sw.bb1
	movabsq	$1, %rax
.LBB39_9:                               # %return
	retq
.LBB39_3:                               # %sw.default
	xorq	%rax, %rax
	retq
.LBB39_7:
	movabsq	$0, %rax
	retq
.Ltmp513:
	.size	handled_component_p, .Ltmp513-handled_component_p
	.cfi_endproc

	.globl	force_operand
	.align	16, 0x90
	.type	force_operand,@function
force_operand:                          # @force_operand
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp520:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp521:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp522:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp523:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp524:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp525:
	.cfi_def_cfa_offset 80
.Ltmp526:
	.cfi_offset %rbx, -48
.Ltmp527:
	.cfi_offset %r12, -40
.Ltmp528:
	.cfi_offset %r13, -32
.Ltmp529:
	.cfi_offset %r14, -24
.Ltmp530:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r12
	xorq	%r15, %r15
	testq	%r14, %r14
	je	.LBB40_5
# BB#1:                                 # %lor.lhs.false.i
	movl	(%r14), %eax
	andq	$67174399, %rax         # imm = 0x400FFFF
	cmpq	$61, %rax
	jne	.LBB40_5
# BB#2:                                 # %lor.lhs.false5.i
	movl	8(%r14), %eax
	cmpq	$53, %rax
	jb	.LBB40_5
# BB#3:                                 # %lor.lhs.false7.i
	callq	preserve_subexpressions_p
	xorq	%r15, %r15
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB40_5
# BB#4:                                 # %lor.lhs.false7.i
	movq	%r14, %r15
.LBB40_5:                               # %get_subtarget.exit
	movl	(%r12), %r13d
	movq	%r13, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-75(%rax), %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$1, %rcx
	ja	.LBB40_12
# BB#6:                                 # %land.lhs.true
	movq	8(%r12), %rcx
	movq	pic_offset_table_rtx(%rip), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB40_12
# BB#7:                                 # %land.lhs.true7
	movq	16(%r12), %rcx
	movzbl	(%rcx), %ecx
	addq	$-58, %rcx
	andq	%rsi, %rcx
	cmpq	$10, %rcx
	ja	.LBB40_12
# BB#8:                                 # %land.lhs.true7
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$1537, %rdx             # imm = 0x601
	je	.LBB40_12
# BB#9:                                 # %if.then
	testq	%r15, %r15
	jne	.LBB40_11
# BB#10:                                # %if.then28
	movq	$16, %rcx
	shrq	%cl, %r13
	andq	$255, %r13
	movq	%r13, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
.LBB40_11:                              # %if.end
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	emit_move_insn
	movq	%r15, %r12
	jmp	.LBB40_52
.LBB40_12:                              # %if.end33
	cmpq	$74, %rax
	jle	.LBB40_49
# BB#13:                                # %if.end33
	cmpq	$75, %rax
	je	.LBB40_14
# BB#15:                                # %if.end33
	cmpq	$78, %rax
	jne	.LBB40_16
# BB#24:                                # %if.then46
	movq	16(%r12), %r13
	movzbl	(%r13), %ecx
	cmpq	$133, %rcx
	jg	.LBB40_29
# BB#25:                                # %if.then46
	addq	$-54, %rcx
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	ja	.LBB40_31
# BB#26:                                # %if.then46
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB40_32
# BB#27:                                # %if.then46
	cmpq	$7, %rcx
	jne	.LBB40_31
# BB#28:                                # %land.lhs.true85
	cmpq	%r15, %r13
	jne	.LBB40_32
	jmp	.LBB40_31
.LBB40_14:
	movabsq	$optab_table, %rcx
	jmp	.LBB40_18
.LBB40_16:                              # %if.end33
	cmpq	$76, %rax
	jne	.LBB40_52
# BB#17:                                # %if.then41
	movabsq	$optab_table+16, %rcx
.LBB40_18:                              # %if.end100
	movq	(%rcx), %rbx
	testq	%rbx, %rbx
	je	.LBB40_49
# BB#19:                                # %if.then102
	movq	16(%r12), %rsi
	movzbl	(%rsi), %eax
	cmpq	$133, %rax
	jg	.LBB40_33
# BB#20:                                # %if.then102
	leaq	-54(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$14, %rcx
	ja	.LBB40_35
# BB#21:                                # %if.then102
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB40_36
# BB#22:                                # %if.then102
	cmpq	$7, %rcx
	jne	.LBB40_35
# BB#23:                                # %land.lhs.true141
	cmpq	%r15, %rsi
	jne	.LBB40_36
	jmp	.LBB40_35
.LBB40_49:                              # %if.end232
	cmpq	$63, %rax
	jne	.LBB40_52
# BB#50:                                # %land.lhs.true236
	movq	8(%r12), %rdi
	movl	(%rdi), %ebx
	movq	%rbx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB40_52
# BB#51:                                # %land.lhs.true243
	movq	$16, %rcx
	shrq	%cl, %r13
	andq	$255, %r13
	movzbl	mode_size(%r13), %eax
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movzbl	mode_size(%rbx), %ecx
	cmpq	%rcx, %rax
	jbe	.LBB40_52
# BB#54:                                # %if.then259
	xorq	%rsi, %rsi
	callq	force_operand
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	force_reg
	movq	8(%r12), %rdx
	movl	16(%r12), %ecx
	movzbl	2(%rdx), %edx
	movq	%r13, %rdi
	movq	%rax, %rsi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	simplify_gen_subreg  # TAILCALL
.LBB40_52:                              # %return
	movq	%r12, %rax
	jmp	.LBB40_53
.LBB40_29:                              # %if.then46
	cmpq	$134, %rcx
	je	.LBB40_32
# BB#30:                                # %if.then46
	cmpq	$140, %rcx
	je	.LBB40_32
.LBB40_31:                              # %if.then87
	xorq	%r15, %r15
.LBB40_32:                              # %if.end88
	movq	8(%r12), %rdi
	movq	%r15, %rsi
	callq	force_operand
	movzbl	2(%r12), %ebx
	movq	%rax, %r15
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	force_operand
	movq	$1, %r8
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	expand_mult  # TAILCALL
.LBB40_33:                              # %if.then102
	cmpq	$134, %rax
	je	.LBB40_36
# BB#34:                                # %if.then102
	cmpq	$140, %rax
	je	.LBB40_36
.LBB40_35:                              # %if.then143
	xorq	%r15, %r15
.LBB40_36:                              # %if.end144
	movq	optab_table+16(%rip), %rcx
	cmpq	%rcx, %rbx
	jne	.LBB40_37
# BB#38:                                # %if.end144
	cmpq	$54, %rax
	jne	.LBB40_39
# BB#40:                                # %if.then150
	movq	optab_table(%rip), %rbx
	movq	$16, %rcx
	shrq	%cl, %r13
	andq	$255, %r13
	movq	%r13, %rdi
	callq	negate_rtx
	movq	%rax, 24(%rsp)          # 8-byte Spill
	jmp	.LBB40_41
.LBB40_37:
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	jmp	.LBB40_41
.LBB40_39:
	movq	%rsi, 24(%rsp)          # 8-byte Spill
.LBB40_41:                              # %if.end155
	movq	optab_table(%rip), %rax
	cmpq	%rax, %rbx
	jne	.LBB40_47
# BB#42:                                # %land.lhs.true157
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB40_47
# BB#43:                                # %land.lhs.true161
	movq	8(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$75, %rcx
	jne	.LBB40_47
# BB#44:                                # %land.lhs.true168
	movq	8(%rax), %rdx
	movzbl	(%rdx), %eax
	cmpq	$61, %rax
	jne	.LBB40_47
# BB#45:                                # %land.lhs.true178
	movl	8(%rdx), %eax
	addq	$-53, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$4, %rax
	ja	.LBB40_47
# BB#46:                                # %if.then199
	movzbl	2(%r12), %edi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rsi
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%r15, %r8
	callq	expand_binop
	movq	8(%r12), %rcx
	movq	16(%rcx), %rdi
	movzbl	2(%r12), %r15d
	movq	%rax, %r12
	xorq	%rsi, %rsi
	callq	force_operand
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	jmp	.LBB40_48
.LBB40_47:                              # %if.end222
	movq	8(%r12), %rdi
	movq	%rbx, %r13
	movq	%r15, %rsi
	callq	force_operand
	movzbl	2(%r12), %ebx
	movq	%rax, %r15
	xorq	%rsi, %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	force_operand
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
.LBB40_48:                              # %if.end222
	movq	%rax, %rcx
	movq	%r14, %r8
	callq	expand_binop
.LBB40_53:                              # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp531:
	.size	force_operand, .Ltmp531-force_operand
	.cfi_endproc

	.globl	safe_from_p
	.align	16, 0x90
	.type	safe_from_p,@function
safe_from_p:                            # @safe_from_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp538:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp539:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp540:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp541:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp542:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp543:
	.cfi_def_cfa_offset 64
.Ltmp544:
	.cfi_offset %rbx, -48
.Ltmp545:
	.cfi_offset %r12, -40
.Ltmp546:
	.cfi_offset %r13, -32
.Ltmp547:
	.cfi_offset %r14, -24
.Ltmp548:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r14
	testq	%r14, %r14
	je	.LBB41_1
# BB#2:                                 # %lor.lhs.false.lr.ph
	movq	global_rtl+72(%rip), %r8
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	je	.LBB41_3
# BB#4:                                 # %lor.lhs.false.lr.ph
	movabsq	$1, %rcx
	jmp	.LBB41_5
.LBB41_1:
	movq	$1, %r13
	jmp	.LBB41_112
.LBB41_3:
	movabsq	$0, %rcx
.LBB41_5:                               # %lor.lhs.false.lr.ph
	xorq	%r13, %r13
	jmp	.LBB41_6
	.align	16, 0x90
.LBB41_38:                              # %if.end86
                                        #   in Loop: Header=BB41_6 Depth=1
	cmpq	$113, %rdi
	jg	.LBB41_42
# BB#39:                                # %if.end86
                                        #   in Loop: Header=BB41_6 Depth=1
	andq	%rbx, %rdi
	cmpq	$101, %rdi
	je	.LBB41_56
	jmp	.LBB41_40
.LBB41_42:                              # %if.end86
                                        #   in Loop: Header=BB41_6 Depth=1
	andq	%rbx, %rdi
	cmpq	$114, %rdi
	jne	.LBB41_43
.LBB41_56:                              # %sw.bb156
                                        #   in Loop: Header=BB41_6 Depth=1
	cmpq	$117, %rcx
	jg	.LBB41_65
# BB#57:                                # %sw.bb156
                                        #   in Loop: Header=BB41_6 Depth=1
	cmpq	$51, %rcx
	jle	.LBB41_58
# BB#63:                                # %sw.bb156
                                        #   in Loop: Header=BB41_6 Depth=1
	addq	$-52, %rcx
	andq	%rbx, %rcx
	cmpq	$4, %rcx
	ja	.LBB41_94
# BB#64:                                # %sw.bb156
                                        #   in Loop: Header=BB41_6 Depth=1
	jmpq	*.LJTI41_0(,%rcx,8)
.LBB41_90:                              # %sw.bb322
                                        #   in Loop: Header=BB41_6 Depth=1
	addq	$40, %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB41_54
.LBB41_83:                              # %sw.bb286
                                        #   in Loop: Header=BB41_6 Depth=1
	addq	$32, %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB41_54
	.align	16, 0x90
.LBB41_6:                               # %lor.lhs.false
                                        # =>This Inner Loop Header: Depth=1
	testq	$1, %rcx
	je	.LBB41_15
# BB#7:                                 # %land.lhs.true
                                        #   in Loop: Header=BB41_6 Depth=1
	movq	8(%r15), %rdx
	testq	%rdx, %rdx
	je	.LBB41_15
# BB#8:                                 # %land.lhs.true2
                                        #   in Loop: Header=BB41_6 Depth=1
	movq	32(%rdx), %rsi
	testq	%rsi, %rsi
	je	.LBB41_15
# BB#9:                                 # %land.lhs.true7
                                        #   in Loop: Header=BB41_6 Depth=1
	movzbl	16(%rsi), %esi
	cmpq	$25, %rsi
	je	.LBB41_15
# BB#10:                                # %land.lhs.true14
                                        #   in Loop: Header=BB41_6 Depth=1
	movzbl	16(%rdx), %esi
	cmpq	$18, %rsi
	jne	.LBB41_13
# BB#11:                                # %lor.lhs.false22
                                        #   in Loop: Header=BB41_6 Depth=1
	movq	112(%rdx), %rdx
	testq	%rdx, %rdx
	je	.LBB41_13
# BB#12:                                # %lor.lhs.false27
                                        #   in Loop: Header=BB41_6 Depth=1
	movzbl	16(%rdx), %edx
	cmpq	$25, %rdx
	je	.LBB41_15
.LBB41_13:                              # %land.lhs.true37
                                        #   in Loop: Header=BB41_6 Depth=1
	movl	(%r14), %edx
	andq	$16711680, %rdx         # imm = 0xFF0000
	cmpq	$3342336, %rdx          # imm = 0x330000
	je	.LBB41_14
	.align	16, 0x90
.LBB41_15:                              # %lor.lhs.false41
                                        #   in Loop: Header=BB41_6 Depth=1
	movl	(%r14), %edx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$63, %rsi
	je	.LBB41_23
# BB#16:                                # %lor.lhs.false41
                                        #   in Loop: Header=BB41_6 Depth=1
	cmpq	$66, %rsi
	jne	.LBB41_25
# BB#17:                                # %land.lhs.true45
                                        #   in Loop: Header=BB41_6 Depth=1
	movq	8(%r14), %rdi
	cmpq	%r8, %rdi
	je	.LBB41_18
# BB#19:                                # %lor.lhs.false47
                                        #   in Loop: Header=BB41_6 Depth=1
	movzbl	(%rdi), %ebx
	cmpq	$75, %rbx
	jne	.LBB41_22
# BB#20:                                # %land.lhs.true54
                                        #   in Loop: Header=BB41_6 Depth=1
	movq	8(%rdi), %rdi
	cmpq	%r8, %rdi
	je	.LBB41_21
.LBB41_22:                              # %if.end
                                        #   in Loop: Header=BB41_6 Depth=1
	cmpq	$63, %rsi
	jne	.LBB41_25
	.align	16, 0x90
.LBB41_23:                              # %if.then65
                                        #   in Loop: Header=BB41_6 Depth=1
	movq	8(%r14), %r14
	movl	(%r14), %edx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$61, %rsi
	jne	.LBB41_25
# BB#24:                                # %land.lhs.true72
                                        #   in Loop: Header=BB41_6 Depth=1
	movl	8(%r14), %esi
	cmpq	$53, %rsi
	jb	.LBB41_112
.LBB41_25:                              # %if.end78
                                        #   in Loop: Header=BB41_6 Depth=1
	testq	$1, %rcx
	jne	.LBB41_26
# BB#29:                                # %if.end86
                                        #   in Loop: Header=BB41_6 Depth=1
	movl	16(%r15), %esi
	movq	%rsi, %rcx
	andq	$255, %rcx
	movsbq	tree_code_type(%rcx), %rdi
	cmpq	$59, %rdi
	jg	.LBB41_36
# BB#30:                                # %if.end86
                                        #   in Loop: Header=BB41_6 Depth=1
	andq	%rax, %rdi
	cmpq	$49, %rdi
	jne	.LBB41_31
# BB#53:                                # %sw.bb138
                                        #   in Loop: Header=BB41_6 Depth=1
	addq	$32, %r15
.LBB41_54:                              # %tailrecurse.backedge
                                        #   in Loop: Header=BB41_6 Depth=1
	movq	(%r15), %r15
	xorq	%rcx, %rcx
	testq	%r14, %r14
	jne	.LBB41_6
	jmp	.LBB41_55
	.align	16, 0x90
.LBB41_36:                              # %if.end86
                                        #   in Loop: Header=BB41_6 Depth=1
	cmpq	$99, %rdi
	movq	%rax, %rbx
	jg	.LBB41_38
# BB#37:                                # %if.end86
	andq	%rbx, %rdi
	cmpq	$60, %rdi
.LBB41_32:                              # %if.end86
	movq	$1, %r13
	jne	.LBB41_112
# BB#33:                                # %sw.bb142
	movq	32(%r15), %rsi
	xorq	%rdx, %rdx
	xorq	%r13, %r13
	movq	%r14, %rdi
	callq	safe_from_p
	testq	%rbx, %rax
	je	.LBB41_112
# BB#34:                                # %land.rhs148
	movq	40(%r15), %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	safe_from_p
	testq	%rbx, %rax
	jne	.LBB41_111
# BB#35:
	movabsq	$0, %r13
	jmp	.LBB41_112
.LBB41_65:                              # %sw.bb156
	cmpq	$118, %rcx
	je	.LBB41_84
# BB#66:                                # %sw.bb156
	cmpq	$120, %rcx
	jne	.LBB41_67
# BB#81:                                # %sw.bb272
	xorq	%r13, %r13
	cmpq	$0, 32(%r15)
	jne	.LBB41_112
# BB#82:                                # %if.end278
	leaq	32(%r15), %rax
	addq	$8, %rax
.LBB41_93:                              # %sw.epilog
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB41_103
	jmp	.LBB41_94
.LBB41_58:                              # %sw.bb156
	cmpq	$41, %rcx
	jne	.LBB41_94
# BB#59:                                # %sw.bb232
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$66, %rdx
	jne	.LBB41_94
# BB#60:                                # %land.lhs.true237
	movq	16(%r14), %rax
	xorq	%r13, %r13
	testq	%rax, %rax
	movabsq	$0, %r12
	je	.LBB41_62
# BB#61:                                # %cond.false243
	movq	(%rax), %r12
.LBB41_62:                              # %cond.end247
	movq	%r15, %rdi
	callq	get_alias_set
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	alias_sets_conflict_p
	testq	%rbx, %rax
	jne	.LBB41_112
	jmp	.LBB41_94
.LBB41_84:                              # %sw.bb291
	movq	48(%r15), %rbx
	testq	%rbx, %rbx
	jne	.LBB41_103
# BB#85:                                # %if.end297
	testq	$1048576, %rsi          # imm = 0x100000
	je	.LBB41_87
# BB#86:
	movq	$1, %r13
	jmp	.LBB41_112
.LBB41_67:                              # %sw.bb156
	cmpq	$121, %rcx
	jne	.LBB41_94
# BB#68:                                # %sw.bb161
	movq	32(%r15), %rdi
	callq	staticp
	testq	%rbx, %rax
	je	.LBB41_70
# BB#69:
	movq	$1, %r13
	jmp	.LBB41_112
.LBB41_26:                              # %if.then80
	movq	$0, safe_from_p.save_expr_list(%rip)
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	safe_from_p
	movq	safe_from_p.save_expr_list(%rip), %rcx
	movq	%rax, %r13
	testq	%rcx, %rcx
	je	.LBB41_112
# BB#27:
	movabsq	$4293918719, %rax       # imm = 0xFFEFFFFF
	.align	16, 0x90
.LBB41_28:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rcx), %rdx
	movl	16(%rdx), %esi
	andq	%rax, %rsi
	movl	%esi, 16(%rdx)
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB41_28
	jmp	.LBB41_112
.LBB41_55:
	movq	$1, %r13
	jmp	.LBB41_112
.LBB41_31:                              # %if.end86
	movq	%rax, %rbx
	cmpq	$50, %rdi
	jmp	.LBB41_32
.LBB41_18:
	movq	$1, %r13
	jmp	.LBB41_112
.LBB41_70:                              # %lor.lhs.false167
	movzbl	18(%r15), %eax
	testq	$4, %rax
	je	.LBB41_72
# BB#71:
	movq	$1, %r13
	jmp	.LBB41_112
.LBB41_87:                              # %if.end305
	orq	$1048576, %rsi          # imm = 0x100000
	movl	%esi, 16(%r15)
	movq	32(%r15), %rsi
	xorq	%rdx, %rdx
	xorq	%r13, %r13
	movq	%r14, %rdi
	callq	safe_from_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB41_88
# BB#89:                                # %if.end320
	movq	safe_from_p.save_expr_list(%rip), %rdx
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	tree_cons
	movq	%rax, safe_from_p.save_expr_list(%rip)
	movq	$1, %r13
	jmp	.LBB41_112
.LBB41_21:
	movq	$1, %r13
	jmp	.LBB41_112
.LBB41_40:                              # %if.end86
	cmpq	$100, %rdi
	jne	.LBB41_41
# BB#102:                               # %sw.bb
	movq	144(%r15), %rbx
	testq	%rbx, %rbx
	movq	$1, %r13
	je	.LBB41_112
.LBB41_103:                             # %if.then370
	movzbl	(%rbx), %eax
	cmpq	$63, %rax
	jne	.LBB41_106
# BB#104:                               # %if.then375
	movq	8(%rbx), %rbx
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	jne	.LBB41_106
# BB#105:                               # %land.lhs.true383
	movl	8(%rbx), %eax
	xorq	%r13, %r13
	cmpq	$53, %rax
	jb	.LBB41_112
.LBB41_106:                             # %if.end391
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	rtx_equal_p
	xorq	%r13, %r13
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	jne	.LBB41_112
# BB#107:                               # %lor.rhs394
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	movq	$1, %r13
	jne	.LBB41_112
# BB#108:                               # %land.lhs.true399
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB41_112
# BB#109:                               # %land.rhs404
	movabsq	$rtx_addr_varies_p, %rcx
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	true_dependence
	testq	%r15, %rax
	je	.LBB41_111
# BB#110:
	movabsq	$0, %r13
	jmp	.LBB41_112
.LBB41_43:                              # %if.end86
	cmpq	$120, %rdi
	jne	.LBB41_44
# BB#45:                                # %sw.bb106
	testq	%rcx, %rcx
	movq	$1, %r13
	je	.LBB41_112
# BB#46:                                # %sw.bb106
	cmpq	$2, %rcx
	jne	.LBB41_52
# BB#47:                                # %if.then113
	movq	32(%r15), %rsi
	testq	%rsi, %rsi
	je	.LBB41_49
# BB#48:                                # %lor.lhs.false117
	xorq	%rdx, %rdx
	xorq	%r13, %r13
	movq	%r14, %rdi
	callq	safe_from_p
	testq	%rbx, %rax
	je	.LBB41_112
.LBB41_49:                              # %land.rhs
	movq	(%r15), %rsi
	testq	%rsi, %rsi
	movq	$1, %r13
	je	.LBB41_112
# BB#50:                                # %lor.rhs
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	safe_from_p
	testq	%rbx, %rax
	je	.LBB41_51
.LBB41_111:                             # %land.rhs404
	movabsq	$1, %r13
	jmp	.LBB41_112
.LBB41_72:                              # %lor.lhs.false173
	movq	32(%r15), %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	safe_from_p
	testq	%rbx, %rax
	je	.LBB41_74
# BB#73:
	movq	$1, %r13
	jmp	.LBB41_112
.LBB41_88:                              # %if.then315
	movl	16(%r15), %eax
	movabsq	$4293918719, %rcx       # imm = 0xFFEFFFFF
	andq	%rax, %rcx
	movl	%ecx, 16(%r15)
	jmp	.LBB41_112
.LBB41_14:
	movq	$1, %r13
	jmp	.LBB41_112
.LBB41_41:
	movq	$1, %r13
	jmp	.LBB41_112
.LBB41_44:
	movq	$1, %r13
	jmp	.LBB41_112
.LBB41_74:                              # %if.end180
	movq	32(%r15), %r15
	movzbl	16(%r15), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$100, %rax
	jne	.LBB41_94
# BB#75:                                # %if.then193
	movq	144(%r15), %rax
	xorq	%r13, %r13
	testq	%rax, %rax
	je	.LBB41_112
# BB#76:                                # %cond.end208
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB41_112
# BB#77:                                # %cond.end225
	addq	$8, %rax
	jmp	.LBB41_93
.LBB41_52:                              # %if.else137
	xorq	%r13, %r13
	jmp	.LBB41_112
.LBB41_78:                              # %sw.bb254
	xorq	%r13, %r13
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$66, %rdx
	je	.LBB41_112
# BB#79:                                # %sw.bb254
	cmpq	$61, %rdx
	jne	.LBB41_94
# BB#80:                                # %land.lhs.true259
	movl	8(%r14), %eax
	cmpq	$53, %rax
	jb	.LBB41_112
.LBB41_94:                              # %if.end330
	movzbl	16(%r15), %edi
	callq	first_rtl_op
	movq	%rax, %rbx
	movq	$32, %rcx
                                        # kill: RAX<def> RBX<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB41_99
# BB#95:
	movq	$4, %r12
	xorq	%r13, %r13
	.align	16, 0x90
.LBB41_96:                              # %for.body339
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r15,%r12,8), %rsi
	testq	%rsi, %rsi
	je	.LBB41_98
# BB#97:                                # %land.lhs.true346
                                        #   in Loop: Header=BB41_96 Depth=1
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	safe_from_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB41_112
.LBB41_98:                              # %for.inc355
                                        #   in Loop: Header=BB41_96 Depth=1
	leaq	-3(%r12), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r12
	cmpq	%rdx, %rax
	jl	.LBB41_96
.LBB41_99:                              # %for.end356
	movzbl	16(%r15), %eax
	cmpq	$147, %rax
	jb	.LBB41_101
# BB#100:                               # %land.lhs.true363
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	*lang_hooks+80(%rip)
	xorq	%r13, %r13
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB41_112
.LBB41_101:                             # %sw.epilog368
	movq	$1, %r13
.LBB41_112:                             # %return
	movq	%r13, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB41_91:                              # %sw.bb327
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.safe_from_p, %rcx
	movq	$5724, %rsi             # imm = 0x165C
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB41_92:                              # %sw.bb282
	leaq	48(%r15), %rax
	jmp	.LBB41_93
.LBB41_51:
	movabsq	$0, %r13
	jmp	.LBB41_112
.Ltmp549:
	.size	safe_from_p, .Ltmp549-safe_from_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI41_0:
	.quad	.LBB41_90
	.quad	.LBB41_78
	.quad	.LBB41_91
	.quad	.LBB41_92
	.quad	.LBB41_83

	.text
	.align	16, 0x90
	.type	mostly_zeros_p,@function
mostly_zeros_p:                         # @mostly_zeros_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp556:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp557:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp558:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp559:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp560:
	.cfi_def_cfa_offset 48
.Ltmp561:
	.cfi_offset %rbx, -48
.Ltmp562:
	.cfi_offset %r12, -40
.Ltmp563:
	.cfi_offset %r13, -32
.Ltmp564:
	.cfi_offset %r14, -24
.Ltmp565:
	.cfi_offset %r15, -16
	movzbl	16(%rdi), %eax
	cmpq	$46, %rax
	jne	.LBB42_16
# BB#1:                                 # %if.then
	movq	8(%rdi), %rax
	movq	40(%rdi), %rbx
	testq	%rax, %rax
	je	.LBB42_2
# BB#4:                                 # %land.lhs.true
	movzbl	16(%rax), %eax
	cmpq	$19, %rax
	jne	.LBB42_2
# BB#5:                                 # %if.then10
	testq	%rbx, %rbx
	je	.LBB42_14
# BB#6:
	movabsq	$0, %rax
	jmp	.LBB42_15
.LBB42_16:                              # %if.end21
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	is_zeros_p  # TAILCALL
.LBB42_2:                               # %for.cond.preheader
	xorq	%r14, %r14
	testq	%rbx, %rbx
	je	.LBB42_3
# BB#7:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movabsq	$0, %r13
	xorq	%r15, %r15
	.align	16, 0x90
.LBB42_8:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rdi
	callq	mostly_zeros_p
	testq	%r12, %rax
	movq	%r13, %rax
	je	.LBB42_10
# BB#9:                                 # %for.body
                                        #   in Loop: Header=BB42_8 Depth=1
	movabsq	$1, %rax
.LBB42_10:                              # %for.body
                                        #   in Loop: Header=BB42_8 Depth=1
	movq	(%rbx), %rbx
	addq	%rax, %r14
	incq	%r15
	testq	%rbx, %rbx
	jne	.LBB42_8
# BB#11:                                # %for.cond.for.end_crit_edge
	movq	$2, %rcx
	shlq	%cl, %r14
	leaq	(%r15,%r15,2), %rax
	jmp	.LBB42_12
.LBB42_3:
	xorq	%rax, %rax
.LBB42_12:                              # %for.end
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	cmpq	%rax, %r14
	jl	.LBB42_13
.LBB42_14:                              # %for.end
	movabsq	$1, %rax
	jmp	.LBB42_15
.LBB42_13:
	movabsq	$0, %rax
.LBB42_15:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp566:
	.size	mostly_zeros_p, .Ltmp566-mostly_zeros_p
	.cfi_endproc

	.align	16, 0x90
	.type	store_constructor,@function
store_constructor:                      # @store_constructor
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp570:
	.cfi_def_cfa_offset 16
.Ltmp571:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp572:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$136, %rsp
.Ltmp573:
	.cfi_offset %rbx, -56
.Ltmp574:
	.cfi_offset %r12, -48
.Ltmp575:
	.cfi_offset %r13, -40
.Ltmp576:
	.cfi_offset %r14, -32
.Ltmp577:
	.cfi_offset %r15, -24
	movq	%rdi, %rbx
	movq	8(%rbx), %r13
	movzbl	16(%r13), %eax
	movq	%rcx, %r8
	movq	%rdx, -64(%rbp)         # 8-byte Spill
	movq	%rsi, %r15
	leaq	-9(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$13, %rcx
	ja	.LBB43_204
# BB#1:                                 # %entry
	jmpq	*.LJTI43_0(,%rcx,8)
.LBB43_55:                              # %if.then234
	movq	%rbx, -112(%rbp)        # 8-byte Spill
	movq	8(%r13), %rcx
	movq	%rcx, -80(%rbp)         # 8-byte Spill
	movq	24(%r13), %r12
	cmpq	$9, %rax
	jne	.LBB43_56
# BB#57:                                # %if.then245
	movq	%r8, -72(%rbp)          # 8-byte Spill
	movq	%rdx, %r14
	movq	%r15, -56(%rbp)         # 8-byte Spill
	movq	24(%r12), %rax
	movq	8(%rax), %rax
	movq	24(%rax), %r12
	jmp	.LBB43_58
.LBB43_2:                               # %lor.lhs.false19
	cmpq	$22, %rax
	jne	.LBB43_4
# BB#3:                                 # %lor.lhs.false19
	movq	-64(%rbp), %rax         # 8-byte Reload
	testq	%rdx, %rax
	je	.LBB43_10
	jmp	.LBB43_4
.LBB43_150:                             # %if.then700
	movq	40(%rbx), %rax
	movq	%rbx, -112(%rbp)        # 8-byte Spill
	movq	%r13, %rdi
	movq	%r13, %r14
	movq	%rax, %r13
	movq	%r8, %rbx
	callq	int_size_in_bytes
	movq	%rbx, %rsi
	movq	%rax, %r12
	testq	%r13, %r13
	jne	.LBB43_154
# BB#151:                               # %if.then700
	testq	%rsi, %rsi
	jle	.LBB43_154
# BB#152:                               # %if.then714
	movq	%r15, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	-64(%rbp), %rcx         # 8-byte Reload
	testq	%rax, %rcx
	jne	.LBB43_149
# BB#153:                               # %if.then716
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	clear_storage
	jmp	.LBB43_149
.LBB43_9:                               # %land.lhs.true
	movq	$1, -104(%rbp)          # 8-byte Folded Spill
	movq	-64(%rbp), %rax         # 8-byte Reload
	testq	%rdx, %rax
	jne	.LBB43_21
.LBB43_10:                              # %land.lhs.true25
	cmpq	$0, 40(%rbx)
	je	.LBB43_11
.LBB43_4:                               # %if.else
	movq	$1, -104(%rbp)          # 8-byte Folded Spill
	movq	-64(%rbp), %rax         # 8-byte Reload
	testq	%rdx, %rax
	jne	.LBB43_21
# BB#5:                                 # %land.lhs.true31
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB43_14
# BB#6:                                 # %land.lhs.true35
	movzbl	18(%rbx), %ecx
	testq	$4, %rcx
	je	.LBB43_14
# BB#7:                                 # %land.lhs.true40
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	target_flags(%rip), %edx
	movzbl	mode_size(%rax), %esi
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	addq	$4, %rdx
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rdx, %rcx
	cmpq	%rcx, %rsi
	jbe	.LBB43_8
.LBB43_14:                              # %if.else55
	testq	%r8, %r8
	jle	.LBB43_20
# BB#15:                                # %land.lhs.true60
	movq	40(%rbx), %rdi
	movq	%r8, -72(%rbp)          # 8-byte Spill
	callq	list_length
	movq	%rax, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, %r12
	andq	%r12, %r14
	movq	%r13, %rdi
	callq	fields_length
	movq	-72(%rbp), %rsi         # 8-byte Reload
	andq	%r12, %rax
	cmpq	%rax, %r14
	jne	.LBB43_17
# BB#16:                                # %lor.lhs.false68
	movq	%rbx, %rdi
	movq	%rsi, %r14
	callq	mostly_zeros_p
	movq	%r14, %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB43_20
.LBB43_17:                              # %land.lhs.true71
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB43_19
# BB#18:                                # %lor.lhs.false76
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %eax
	cmpq	%rsi, %rax
	jne	.LBB43_20
.LBB43_19:                              # %if.then85
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	jmp	.LBB43_12
.LBB43_56:
	movq	%r8, -72(%rbp)          # 8-byte Spill
	movq	%rdx, %r14
	movq	%r15, -56(%rbp)         # 8-byte Spill
.LBB43_58:                              # %if.end254
	movq	104(%r12), %rdi
	xorq	%r15, %r15
	testq	%rdi, %rdi
	movq	$1, %rdx
	movabsq	$0, %rcx
	je	.LBB43_63
# BB#59:                                # %land.lhs.true257
	xorq	%r15, %r15
	cmpq	$0, 112(%r12)
	movq	$1, %rdx
	movabsq	$0, %rcx
	je	.LBB43_63
# BB#60:                                # %land.lhs.true260
	xorq	%rsi, %rsi
	xorq	%r15, %r15
	callq	host_integerp
	testq	%r14, %rax
	movq	$1, %rdx
	movabsq	$0, %rcx
	je	.LBB43_63
# BB#61:                                # %land.end
	movq	112(%r12), %rdi
	xorq	%rsi, %rsi
	xorq	%r15, %r15
	callq	host_integerp
	testq	%r14, %rax
	movq	$1, %rdx
	movabsq	$0, %rcx
	je	.LBB43_63
# BB#62:                                # %if.then270
	movq	104(%r12), %rdi
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	112(%r12), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	%rbx, %rcx
	movq	%rax, %rdx
	incq	%rdx
	movq	$1, %r15
.LBB43_63:                              # %if.end277
	movq	$1, %rbx
	movq	%rcx, -128(%rbp)        # 8-byte Spill
	movq	%r15, -160(%rbp)        # 8-byte Spill
	movq	%r12, -120(%rbp)        # 8-byte Spill
	movq	%r13, -96(%rbp)         # 8-byte Spill
	movq	%r14, %rdi
	movq	-64(%rbp), %rax         # 8-byte Reload
	testq	%rdi, %rax
	jne	.LBB43_87
# BB#64:                                # %lor.lhs.false279
	movq	%rdx, -136(%rbp)        # 8-byte Spill
	movq	-56(%rbp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB43_66
# BB#65:                                # %land.lhs.true284
	movq	-112(%rbp), %rax        # 8-byte Reload
	movzbl	18(%rax), %eax
	testq	$4, %rax
	jne	.LBB43_87
.LBB43_66:                              # %if.else292
	movq	-112(%rbp), %rax        # 8-byte Reload
	movq	40(%rax), %r12
	movq	-160(%rbp), %rbx        # 8-byte Reload
	xorq	$1, %rbx
	xorq	%r15, %r15
	testq	%r12, %r12
	je	.LBB43_67
# BB#68:                                # %if.else292
	testq	%rbx, %rbx
	jne	.LBB43_69
# BB#70:
	movq	%rbx, -144(%rbp)        # 8-byte Spill
	xorq	%r14, %r14
	.align	16, 0x90
.LBB43_71:                              # %for.body306
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %r13
	movq	24(%r12), %rax
	testq	%rax, %rax
	movq	$1, %rbx
	je	.LBB43_76
# BB#72:                                # %land.lhs.true311
                                        #   in Loop: Header=BB43_71 Depth=1
	movzbl	16(%rax), %ecx
	cmpq	$113, %rcx
	movq	$1, %rbx
	jne	.LBB43_76
# BB#73:                                # %if.then318
                                        #   in Loop: Header=BB43_71 Depth=1
	movq	32(%rax), %rdi
	movq	%rdi, -104(%rbp)        # 8-byte Spill
	movq	40(%rax), %rax
	movq	%rax, -88(%rbp)         # 8-byte Spill
	movq	$1, %rsi
	movq	$1, %rbx
	callq	host_integerp
	movq	%r13, %rdi
	testq	%rdi, %rax
	je	.LBB43_87
# BB#74:                                # %lor.lhs.false327
                                        #   in Loop: Header=BB43_71 Depth=1
	movq	$1, %rsi
	movq	$1, %rbx
	movq	%rdi, %r13
	movq	-88(%rbp), %rdi         # 8-byte Reload
	callq	host_integerp
	movq	%r13, %rdi
	testq	%rdi, %rax
	je	.LBB43_87
# BB#75:                                # %if.end331
                                        #   in Loop: Header=BB43_71 Depth=1
	movq	%rdi, %r13
	movq	$1, %rsi
	movq	-88(%rbp), %rdi         # 8-byte Reload
	callq	tree_low_cst
	movq	%rax, %rbx
	movq	$1, %rsi
	movq	-104(%rbp), %rdi        # 8-byte Reload
	callq	tree_low_cst
	incq	%rbx
	subq	%rax, %rbx
.LBB43_76:                              # %if.end335
                                        #   in Loop: Header=BB43_71 Depth=1
	movq	32(%r12), %rdi
	callq	mostly_zeros_p
	movq	%r13, %rdi
	andq	%rdi, %rax
	je	.LBB43_78
# BB#77:                                # %if.end335
                                        #   in Loop: Header=BB43_71 Depth=1
	movq	%rbx, %rax
.LBB43_78:                              # %if.end335
                                        #   in Loop: Header=BB43_71 Depth=1
	movq	(%r12), %r12
	addq	%rbx, %r15
	addq	%rax, %r14
	testq	%r12, %r12
	jne	.LBB43_71
# BB#79:                                # %for.cond297.for.end347_crit_edge
	movq	$2, %rcx
	shlq	%cl, %r14
	movq	-144(%rbp), %rbx        # 8-byte Reload
	jmp	.LBB43_80
.LBB43_11:                              # %if.then28
	movq	%rbx, %rdi
	callq	expr_size
.LBB43_12:                              # %if.end94
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	clear_storage
.LBB43_13:                              # %if.end94
	movq	$1, -104(%rbp)          # 8-byte Folded Spill
	movq	$1, -64(%rbp)           # 8-byte Folded Spill
	jmp	.LBB43_21
.LBB43_154:                             # %if.end720
	movq	24(%r14), %rbx
	movq	sizetype_tab(%rip), %rdi
	movq	104(%rbx), %rsi
	callq	convert
	movq	sizetype_tab(%rip), %rdi
	movq	112(%rbx), %rsi
	movq	%rax, %rbx
	movq	%rbx, -96(%rbp)         # 8-byte Spill
	callq	convert
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	size_diffop
	movq	%rax, %rbx
	movq	$1, %rdi
	movq	$1, %rsi
	callq	size_int_wide
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, -104(%rbp)        # 8-byte Spill
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	movl	(%r15), %ecx
	movq	%rax, %rbx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$3342336, %rcx          # imm = 0x330000
	jne	.LBB43_158
# BB#155:                               # %lor.lhs.false736
	movl	target_flags(%rip), %eax
	movq	$19, %rcx
	shrq	%cl, %rax
	andq	$64, %rax
	addq	$64, %rax
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rax, %rcx
	cmpq	%rcx, %rbx
	jbe	.LBB43_158
# BB#156:                               # %lor.lhs.false744
	cmpq	$32, %r12
	ja	.LBB43_174
# BB#157:                               # %land.lhs.true747
	cmpq	$0, (%r13)
	je	.LBB43_174
.LBB43_158:                             # %if.then752
	movq	%r15, -56(%rbp)         # 8-byte Spill
	movq	-112(%rbp), %r14        # 8-byte Reload
	movq	8(%r14), %rax
	movl	64(%rax), %r13d
	movq	$1, %rsi
	movq	$1, %rdx
	movq	%r13, %rdi
	callq	mode_for_size
	movq	%rax, -120(%rbp)        # 8-byte Spill
	movq	%rsp, %r12
	leaq	15(%rbx), %rax
	andq	$-16, %rax
	subq	%rax, %r12
	movq	%r12, %rsp
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	get_set_constructor_bits
	movq	%rax, -72(%rbp)         # 8-byte Spill
	movq	$3, %rcx
	movq	%r13, %rax
	shrq	%cl, %rax
	movq	%rax, -88(%rbp)         # 8-byte Spill
	xorq	%r14, %r14
	movq	$0, -80(%rbp)           # 8-byte Folded Spill
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	jmp	.LBB43_159
	.align	16, 0x90
.LBB43_173:                             # %if.end804
                                        #   in Loop: Header=BB43_159 Depth=1
	movq	-88(%rbp), %rax         # 8-byte Reload
	addq	%rax, -80(%rbp)         # 8-byte Folded Spill
.LBB43_159:                             # %for.cond761.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB43_160 Depth 2
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	.align	16, 0x90
.LBB43_160:                             # %for.cond761
                                        #   Parent Loop BB43_159 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r14, %rcx
	andq	%rdi, %rcx
	movzbl	(%r12,%rcx), %ecx
	testq	%rcx, %rcx
	je	.LBB43_162
# BB#161:                               # %if.then765
                                        #   in Loop: Header=BB43_160 Depth=2
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	orq	%rdx, %rsi
.LBB43_162:                             # %if.end767
                                        #   in Loop: Header=BB43_160 Depth=2
	incq	%rax
	movq	%rax, %rcx
	incq	%r14
	andq	%rdi, %rcx
	movq	%r14, %r15
	andq	%rdi, %r15
	cmpq	%r13, %rcx
	jae	.LBB43_164
# BB#163:                               # %if.end767
                                        #   in Loop: Header=BB43_160 Depth=2
	cmpq	%rbx, %r15
	jne	.LBB43_160
.LBB43_164:                             # %if.then776
                                        #   in Loop: Header=BB43_159 Depth=1
	testq	%rsi, %rsi
	jne	.LBB43_166
# BB#165:                               # %if.then776
                                        #   in Loop: Header=BB43_159 Depth=1
	movq	-64(%rbp), %rax         # 8-byte Reload
	testq	%rdi, %rax
	jne	.LBB43_169
.LBB43_166:                             # %if.then781
                                        #   in Loop: Header=BB43_159 Depth=1
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	-56(%rbp), %rdi         # 8-byte Reload
	movzbl	(%rdi), %ecx
	movq	%rax, %rdx
	cmpq	$66, %rcx
	jne	.LBB43_171
# BB#167:                               # %if.then788
                                        #   in Loop: Header=BB43_159 Depth=1
	movq	%rdx, -112(%rbp)        # 8-byte Spill
	movq	-80(%rbp), %rdx         # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	-120(%rbp), %rsi        # 8-byte Reload
	callq	adjust_address_1
	movq	-112(%rbp), %rdx        # 8-byte Reload
	movq	%rax, %rdi
	jmp	.LBB43_168
	.align	16, 0x90
.LBB43_171:                             # %if.else791
                                        #   in Loop: Header=BB43_159 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	-80(%rbp), %rcx         # 8-byte Reload
	testq	%rax, %rcx
	jne	.LBB43_172
.LBB43_168:                             # %if.end797
                                        #   in Loop: Header=BB43_159 Depth=1
	movq	%rdx, %rsi
	callq	emit_move_insn
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
.LBB43_169:                             # %if.end799
                                        #   in Loop: Header=BB43_159 Depth=1
	cmpq	%rbx, %r15
	jne	.LBB43_173
# BB#170:
	movq	-72(%rbp), %r13         # 8-byte Reload
	jmp	.LBB43_185
.LBB43_204:                             # %if.else979
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.store_constructor, %rdx
	movq	$5009, %rsi             # imm = 0x1391
	callq	fancy_abort
.LBB43_20:                              # %if.then91
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	$0, -104(%rbp)          # 8-byte Folded Spill
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	$0, -64(%rbp)           # 8-byte Folded Spill
.LBB43_21:                              # %if.end94
	movq	%rbx, -112(%rbp)        # 8-byte Spill
	movq	%r13, -96(%rbp)         # 8-byte Spill
	movq	%r15, %r14
	movq	40(%rbx), %r12
	jmp	.LBB43_22
	.align	16, 0x90
.LBB43_54:                              # %for.inc
                                        #   in Loop: Header=BB43_22 Depth=1
	movq	(%r12), %r12
.LBB43_22:                              # %if.end94
                                        # =>This Inner Loop Header: Depth=1
	testq	%r12, %r12
	je	.LBB43_149
# BB#23:                                # %for.body
                                        #   in Loop: Header=BB43_22 Depth=1
	movq	24(%r12), %r13
	testq	%r13, %r13
	je	.LBB43_54
# BB#24:                                # %if.end104
                                        #   in Loop: Header=BB43_22 Depth=1
	movq	32(%r12), %rbx
	cmpq	$0, -104(%rbp)          # 8-byte Folded Reload
	je	.LBB43_26
# BB#25:                                # %land.lhs.true106
                                        #   in Loop: Header=BB43_22 Depth=1
	movq	%rbx, %rdi
	callq	is_zeros_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB43_54
.LBB43_26:                              # %if.end110
                                        #   in Loop: Header=BB43_22 Depth=1
	movq	%rbx, -56(%rbp)         # 8-byte Spill
	movq	40(%r13), %rdi
	movq	$1, %rsi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$-1, %rax
	je	.LBB43_28
# BB#27:                                # %if.then114
                                        #   in Loop: Header=BB43_22 Depth=1
	movq	40(%r13), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
.LBB43_28:                              # %if.end119
                                        #   in Loop: Header=BB43_22 Depth=1
	movq	%rax, -72(%rbp)         # 8-byte Spill
	movq	48(%r13), %rax
	movq	88(%r13), %r15
	testq	$4096, %rax             # imm = 0x1000
	je	.LBB43_29
# BB#30:                                # %if.end119
                                        #   in Loop: Header=BB43_22 Depth=1
	movq	$0, -80(%rbp)           # 8-byte Folded Spill
	jmp	.LBB43_31
.LBB43_29:                              #   in Loop: Header=BB43_22 Depth=1
	andq	$255, %rax
	movq	%rax, -80(%rbp)         # 8-byte Spill
.LBB43_31:                              # %if.end119
                                        #   in Loop: Header=BB43_22 Depth=1
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB43_34
# BB#32:                                # %land.lhs.true139
                                        #   in Loop: Header=BB43_22 Depth=1
	movq	%r13, %rdi
	callq	bit_position
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	testq	%rbx, %rax
	jne	.LBB43_33
.LBB43_34:                              # %if.end148
                                        #   in Loop: Header=BB43_22 Depth=1
	movq	160(%r13), %rdi
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	%rax, -88(%rbp)         # 8-byte Spill
	testq	%r15, %r15
	je	.LBB43_35
# BB#36:                                # %if.then150
                                        #   in Loop: Header=BB43_22 Depth=1
	movq	%r15, %rdi
	callq	contains_placeholder_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB43_38
# BB#37:                                #   in Loop: Header=BB43_22 Depth=1
	movq	%r14, %rbx
	jmp	.LBB43_39
.LBB43_33:                              # %if.end148.thread
                                        #   in Loop: Header=BB43_22 Depth=1
	movq	%r13, %rdi
	callq	int_bit_position
	movq	%rax, -88(%rbp)         # 8-byte Spill
.LBB43_35:                              #   in Loop: Header=BB43_22 Depth=1
	movq	%r14, %r15
	movq	%r15, %rbx
	jmp	.LBB43_45
.LBB43_38:                              # %if.then153
                                        #   in Loop: Header=BB43_22 Depth=1
	movq	sizetype_tab(%rip), %rax
	movq	%rax, -120(%rbp)        # 8-byte Spill
	movq	-112(%rbp), %rax        # 8-byte Reload
	movq	8(%rax), %rdi
	movq	%r14, %rbx
	movq	%rbx, %rsi
	callq	make_tree
	movq	%rax, %rcx
	movq	$58, %rdi
	xorq	%rax, %rax
	movq	-120(%rbp), %rsi        # 8-byte Reload
	movq	%r15, %rdx
	callq	build
	movq	%rax, %r15
.LBB43_39:                              # %if.end158
                                        #   in Loop: Header=BB43_22 Depth=1
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	callq	expand_expr
	movzbl	(%rbx), %ecx
	movq	%rbx, %r14
	movq	%rax, %rbx
	cmpq	$66, %rcx
	jne	.LBB43_40
# BB#42:                                # %if.end165
                                        #   in Loop: Header=BB43_22 Depth=1
	movzbl	2(%rbx), %eax
	movl	ptr_mode(%rip), %edi
	cmpq	%rdi, %rax
	je	.LBB43_44
# BB#43:                                # %if.then171
                                        #   in Loop: Header=BB43_22 Depth=1
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%rbx, %rdx
	callq	convert_modes
	movq	%rax, %rbx
.LBB43_44:                              # %if.end173
                                        #   in Loop: Header=BB43_22 Depth=1
	movq	%r15, %rdi
	callq	highest_pow2_factor
	movq	%r14, %r15
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	offset_address
	movq	%rax, %rbx
.LBB43_45:                              # %if.end176
                                        #   in Loop: Header=BB43_22 Depth=1
	movzbl	17(%r13), %ecx
	movl	(%rbx), %eax
	testq	$16, %rcx
	je	.LBB43_49
# BB#46:                                # %if.then182
                                        #   in Loop: Header=BB43_22 Depth=1
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB43_48
# BB#47:                                # %if.then187
                                        #   in Loop: Header=BB43_22 Depth=1
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movl	(%rbx), %eax
.LBB43_48:                              # %if.end189
                                        #   in Loop: Header=BB43_22 Depth=1
	orq	$67108864, %rax         # imm = 0x4000000
	movl	%eax, (%rbx)
.LBB43_49:                              # %if.end192
                                        #   in Loop: Header=BB43_22 Depth=1
	andq	$16842751, %rax         # imm = 0x100FFFF
	cmpq	$66, %rax
	jne	.LBB43_50
# BB#51:                                # %land.lhs.true202
                                        #   in Loop: Header=BB43_22 Depth=1
	movzbl	52(%r13), %eax
	movq	%r15, %r14
	testq	$4, %rax
	je	.LBB43_53
# BB#52:                                # %if.then209
                                        #   in Loop: Header=BB43_22 Depth=1
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movl	(%rbx), %eax
	orq	$16777216, %rax         # imm = 0x1000000
	movl	%eax, (%rbx)
	jmp	.LBB43_53
.LBB43_50:                              #   in Loop: Header=BB43_22 Depth=1
	movq	%r15, %r14
.LBB43_53:                              # %if.end214
                                        #   in Loop: Header=BB43_22 Depth=1
	movq	8(%r13), %rdi
	callq	get_alias_set
	subq	$16, %rsp
	movq	%rax, 8(%rsp)
	movq	-64(%rbp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%rbx, %rdi
	movq	-72(%rbp), %rsi         # 8-byte Reload
	movq	-88(%rbp), %rdx         # 8-byte Reload
	movq	-80(%rbp), %rcx         # 8-byte Reload
	movq	-56(%rbp), %r8          # 8-byte Reload
	movq	-96(%rbp), %r9          # 8-byte Reload
	callq	store_constructor_field
	addq	$16, %rsp
	jmp	.LBB43_54
.LBB43_67:
	xorq	%r14, %r14
	jmp	.LBB43_80
.LBB43_69:
	xorq	%r14, %r14
.LBB43_80:                              # %for.end347
	movq	-160(%rbp), %rax        # 8-byte Reload
	testq	%rax, %rax
	movq	-136(%rbp), %rcx        # 8-byte Reload
	je	.LBB43_87
# BB#81:                                # %land.lhs.true349
	movq	-128(%rbp), %rax        # 8-byte Reload
	subq	%rax, %rcx
	cmpq	%rcx, %r15
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	jl	.LBB43_83
# BB#82:                                # %land.lhs.true349
	movq	%rdx, %rcx
.LBB43_83:                              # %land.lhs.true349
	leaq	(%r15,%r15,2), %rsi
	cmpq	%rsi, %r14
	jge	.LBB43_85
# BB#84:                                # %land.lhs.true349
	movq	%rdx, %rax
.LBB43_85:                              # %land.lhs.true349
	testq	%rax, %rcx
	jne	.LBB43_87
# BB#86:                                # %land.lhs.true349
	movq	$1, %rbx
.LBB43_87:                              # %if.end360
	testq	%rdi, %rbx
	movq	-72(%rbp), %rsi         # 8-byte Reload
	je	.LBB43_93
# BB#88:                                # %if.end360
	testq	%rsi, %rsi
	jle	.LBB43_93
# BB#89:                                # %if.then365
	movq	$1, -88(%rbp)           # 8-byte Folded Spill
	movq	-64(%rbp), %rax         # 8-byte Reload
	testq	%rdi, %rax
	movq	-56(%rbp), %r14         # 8-byte Reload
	jne	.LBB43_96
# BB#90:                                # %if.then367
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	movq	-112(%rbp), %rbx        # 8-byte Reload
	jne	.LBB43_92
# BB#91:                                # %if.then372
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$2040, %rax             # imm = 0x7F8
	movq	const_tiny_rtx(%rax), %rsi
	movq	%r14, %rdi
	callq	emit_move_insn
	jmp	.LBB43_97
.LBB43_93:                              # %if.else384
	movq	-56(%rbp), %rdx         # 8-byte Reload
	movzbl	(%rdx), %eax
	cmpq	$61, %rax
	jne	.LBB43_95
# BB#94:                                # %if.then389
	movq	$49, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
.LBB43_95:                              # %if.end393
	movq	-64(%rbp), %rax         # 8-byte Reload
	movq	%rax, -88(%rbp)         # 8-byte Spill
.LBB43_96:                              # %if.end393
	movq	-112(%rbp), %rbx        # 8-byte Reload
.LBB43_97:                              # %if.end393
	movq	40(%rbx), %rcx
	testq	%rcx, %rcx
	movq	-80(%rbp), %r13         # 8-byte Reload
	je	.LBB43_149
# BB#98:                                # %for.body399.lr.ph
	movq	-128(%rbp), %rax        # 8-byte Reload
	notq	%rax
	movq	%rax, -168(%rbp)        # 8-byte Spill
	movq	$0, -104(%rbp)          # 8-byte Folded Spill
	.align	16, 0x90
.LBB43_99:                              # %for.body399
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB43_116 Depth 2
                                        #     Child Loop BB43_124 Depth 2
	movq	%rcx, -72(%rbp)         # 8-byte Spill
	movq	24(%rcx), %r12
	movq	32(%rcx), %rbx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	-88(%rbp), %rax         # 8-byte Reload
	testq	%rdx, %rax
	je	.LBB43_101
# BB#100:                               # %land.lhs.true411
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	%rdx, %r14
	movq	%rbx, %rdi
	callq	is_zeros_p
	movq	%r14, %rdx
	testq	%rdx, %rax
	jne	.LBB43_148
.LBB43_101:                             # %if.end415
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	%rbx, -64(%rbp)         # 8-byte Spill
	movl	16(%r13), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, -44(%rbp)
	movl	60(%r13), %r14d
	movq	$9, %rcx
	shrq	%cl, %r14
	andq	$127, %r14
	cmpq	$51, %r14
	jne	.LBB43_104
# BB#102:                               # %if.then428
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	%rdx, %r15
	movq	32(%r13), %rdi
	movq	$1, %rsi
	callq	host_integerp
	testq	%r15, %rax
	movq	$-1, %rbx
	je	.LBB43_105
# BB#103:                               # %cond.true
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	32(%r13), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, %rbx
	jmp	.LBB43_105
	.align	16, 0x90
.LBB43_104:                             # %if.else437
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	%r14, %rax
	andq	%rdx, %rax
	movzbl	mode_bitsize(%rax,%rax), %ebx
.LBB43_105:                             # %if.end441
                                        #   in Loop: Header=BB43_99 Depth=1
	testq	%r12, %r12
	je	.LBB43_134
# BB#106:                               # %land.lhs.true444
                                        #   in Loop: Header=BB43_99 Depth=1
	movzbl	16(%r12), %eax
	cmpq	$113, %rax
	jne	.LBB43_133
# BB#107:                               # %if.then451
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	%rbx, -112(%rbp)        # 8-byte Spill
	movq	32(%r12), %rax
	movq	%rax, -136(%rbp)        # 8-byte Spill
	movq	40(%r12), %rbx
	movq	-160(%rbp), %rax        # 8-byte Reload
	testq	%rax, %rax
	je	.LBB43_121
# BB#108:                               # %land.lhs.true462
                                        #   in Loop: Header=BB43_99 Depth=1
	xorq	%rsi, %rsi
	movq	-136(%rbp), %rdi        # 8-byte Reload
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB43_121
# BB#109:                               # %land.lhs.true465
                                        #   in Loop: Header=BB43_99 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB43_121
# BB#110:                               # %land.lhs.true468
                                        #   in Loop: Header=BB43_99 Depth=1
	xorq	%rsi, %rsi
	movq	-136(%rbp), %rdi        # 8-byte Reload
	callq	tree_low_cst
	movq	%rax, %r15
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	tree_low_cst
	movq	-56(%rbp), %rcx         # 8-byte Reload
	movzbl	(%rcx), %ecx
	movq	%rax, %rdx
	cmpq	$66, %rcx
	jne	.LBB43_114
# BB#111:                               # %land.lhs.true468
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	%rdx, %r12
	subq	%r15, %r12
	incq	%r12
	cmpq	$3, %r12
	jl	.LBB43_114
# BB#112:                               # %lor.rhs
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	-80(%rbp), %rax         # 8-byte Reload
	movq	32(%rax), %rdi
	movq	$1, %rsi
	movq	%rdx, %r13
	callq	host_integerp
	movq	%r13, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB43_121
# BB#113:                               # %land.rhs484
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	-80(%rbp), %rax         # 8-byte Reload
	movq	32(%rax), %rdi
	movq	$1, %rsi
	movq	%rdx, %r13
	callq	tree_low_cst
	movq	%r13, %rdx
	imulq	%r12, %rax
	cmpq	$320, %rax              # imm = 0x140
	jg	.LBB43_121
.LBB43_114:                             # %if.then493
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	%r15, %rax
	movq	-128(%rbp), %rcx        # 8-byte Reload
	subq	%rcx, %rax
	subq	%rcx, %rdx
	cmpq	%rdx, %rax
	jg	.LBB43_148
# BB#115:                               # %for.body499.preheader
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	-168(%rbp), %rax        # 8-byte Reload
	addq	%rax, %r15
	.align	16, 0x90
.LBB43_116:                             # %for.body499
                                        #   Parent Loop BB43_99 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rdx, %r13
	movq	-80(%rbp), %rax         # 8-byte Reload
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	-56(%rbp), %rbx         # 8-byte Reload
	movl	(%rbx), %ecx
	movq	%rax, %r12
	andq	$16842751, %rcx         # imm = 0x100FFFF
	cmpq	$66, %rcx
	jne	.LBB43_120
# BB#117:                               # %land.lhs.true513
                                        #   in Loop: Header=BB43_116 Depth=2
	movq	-96(%rbp), %rcx         # 8-byte Reload
	movzbl	16(%rcx), %eax
	cmpq	$18, %rax
	jne	.LBB43_120
# BB#118:                               # %land.lhs.true520
                                        #   in Loop: Header=BB43_116 Depth=2
	movzbl	62(%rcx), %eax
	testq	$8, %rax
	je	.LBB43_120
# BB#119:                               # %if.then526
                                        #   in Loop: Header=BB43_116 Depth=2
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movl	(%rbx), %eax
	orq	$16777216, %rax         # imm = 0x1000000
	movl	%eax, (%rbx)
	.align	16, 0x90
.LBB43_120:                             # %if.end531
                                        #   in Loop: Header=BB43_116 Depth=2
	movq	%rbx, -56(%rbp)         # 8-byte Spill
	incq	%r15
	imulq	%r15, %r12
	movq	-80(%rbp), %rdi         # 8-byte Reload
	callq	get_alias_set
	subq	$16, %rsp
	movq	%rax, 8(%rsp)
	movq	-88(%rbp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%rbx, %rdi
	movq	-112(%rbp), %rsi        # 8-byte Reload
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	-64(%rbp), %r8          # 8-byte Reload
	movq	-96(%rbp), %r9          # 8-byte Reload
	callq	store_constructor_field
	addq	$16, %rsp
	movq	%r13, %rdx
	cmpq	%rdx, %r15
	jl	.LBB43_116
	jmp	.LBB43_148
.LBB43_133:                             # %land.lhs.true606
                                        #   in Loop: Header=BB43_99 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB43_137
	.align	16, 0x90
.LBB43_134:                             # %lor.lhs.false609
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	32(%r13), %rdi
	movq	$1, %rsi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB43_140
# BB#135:                               # %if.then614
                                        #   in Loop: Header=BB43_99 Depth=1
	testq	%r12, %r12
	jne	.LBB43_137
# BB#136:                               # %if.then618
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	$1, %rdi
	movq	$1, %rsi
	callq	size_int_wide
	movq	%rax, %r12
.LBB43_137:                             # %if.end620
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	%r13, %rbx
	movq	-128(%rbp), %rax        # 8-byte Reload
	testq	%rax, %rax
	je	.LBB43_139
# BB#138:                               # %if.then622
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	sizetype_tab+8(%rip), %r15
	movq	-120(%rbp), %rax        # 8-byte Reload
	movq	104(%rax), %rdx
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %r12
.LBB43_139:                             # %if.end628
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	sizetype_tab+8(%rip), %rdi
	movq	40(%rbx), %rsi
	callq	convert
	movq	$61, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	callq	expand_expr
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	highest_pow2_factor
	movq	-56(%rbp), %rdi         # 8-byte Reload
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	offset_address
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	adjust_address_1
	xorq	%rdx, %rdx
	movq	-64(%rbp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	store_expr
	jmp	.LBB43_148
	.align	16, 0x90
.LBB43_140:                             # %if.else638
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	%rbx, -112(%rbp)        # 8-byte Spill
	testq	%r12, %r12
	je	.LBB43_142
# BB#141:                               # %if.then641
                                        #   in Loop: Header=BB43_99 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	tree_low_cst
	movq	32(%r13), %rdi
	movq	%rax, %rbx
	movq	-128(%rbp), %rax        # 8-byte Reload
	subq	%rax, %rbx
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, %r15
	imulq	%rbx, %r15
	jmp	.LBB43_143
.LBB43_121:                             # %if.else536
                                        #   in Loop: Header=BB43_99 Depth=1
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%rbx, -144(%rbp)        # 8-byte Spill
	callq	expand_expr
	callq	gen_label_rtx
	callq	gen_label_rtx
	movq	-120(%rbp), %rbx        # 8-byte Reload
	movl	16(%rbx), %edx
	movq	%rax, -152(%rbp)        # 8-byte Spill
	movq	$13, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movl	%edx, -44(%rbp)
	movq	$34, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	build_decl
	movq	%rax, %r12
	movzbl	48(%r12), %esi
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	leaq	-44(%rbp), %rdx
	callq	promote_mode
	movq	%rax, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
	movq	%r15, 144(%r12)
	movq	-64(%rbp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$118, %rax
	jne	.LBB43_129
# BB#122:                               # %land.lhs.true560
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	-64(%rbp), %rax         # 8-byte Reload
	cmpq	$0, 48(%rax)
	jne	.LBB43_129
# BB#123:                               # %if.then566
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	-64(%rbp), %rdi         # 8-byte Reload
	callq	expand_expr
	jmp	.LBB43_124
.LBB43_142:                             # %if.else648
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	32(%r13), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, %r15
	movq	-104(%rbp), %rax        # 8-byte Reload
	imulq	%rax, %r15
.LBB43_143:                             # %if.end654
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	-56(%rbp), %rax         # 8-byte Reload
	movl	(%rax), %eax
	andq	$16842751, %rax         # imm = 0x100FFFF
	cmpq	$66, %rax
	movq	-96(%rbp), %rbx         # 8-byte Reload
	jne	.LBB43_147
# BB#144:                               # %land.lhs.true664
                                        #   in Loop: Header=BB43_99 Depth=1
	movzbl	16(%rbx), %eax
	cmpq	$18, %rax
	jne	.LBB43_147
# BB#145:                               # %land.lhs.true671
                                        #   in Loop: Header=BB43_99 Depth=1
	movzbl	62(%rbx), %eax
	testq	$8, %rax
	je	.LBB43_147
# BB#146:                               # %if.then678
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	-56(%rbp), %rdi         # 8-byte Reload
	callq	copy_rtx
	movq	%rax, -56(%rbp)         # 8-byte Spill
	movl	(%rax), %ecx
	orq	$16777216, %rcx         # imm = 0x1000000
	movl	%ecx, (%rax)
	.align	16, 0x90
.LBB43_147:                             # %if.end683
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	-80(%rbp), %rdi         # 8-byte Reload
	callq	get_alias_set
	subq	$16, %rsp
	movq	%rax, 8(%rsp)
	movq	-88(%rbp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	-56(%rbp), %rdi         # 8-byte Reload
	movq	-112(%rbp), %rsi        # 8-byte Reload
	movq	%r15, %rdx
	movq	%r14, %rcx
	movq	-64(%rbp), %r8          # 8-byte Reload
	movq	%rbx, %r9
	callq	store_constructor_field
	addq	$16, %rsp
	jmp	.LBB43_148
	.align	16, 0x90
.LBB43_128:                             # %if.end.i
                                        #   in Loop: Header=BB43_124 Depth=2
	movq	cfun(%rip), %rax
	movq	40(%rbx), %rcx
	movq	16(%rax), %rax
	movq	%rcx, 40(%rax)
.LBB43_124:                             # %if.then566
                                        #   Parent Loop BB43_99 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB43_129
# BB#125:                               # %while.body.i
                                        #   in Loop: Header=BB43_124 Depth=2
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$24, %rax
	jne	.LBB43_127
# BB#126:                               # %if.then.i
                                        #   in Loop: Header=BB43_124 Depth=2
	movq	8(%rdi), %rax
	movq	8(%rax), %rax
	movq	%rax, 16(%rbx)
	callq	emit_insn
	jmp	.LBB43_128
.LBB43_127:                             # %if.else.i
                                        #   in Loop: Header=BB43_124 Depth=2
	callq	emit_insn
	movq	%rax, 16(%rbx)
	jmp	.LBB43_128
.LBB43_129:                             # %if.end568
                                        #   in Loop: Header=BB43_99 Depth=1
	xorq	%rdx, %rdx
	movq	-136(%rbp), %rdi        # 8-byte Reload
	movq	%r15, %rsi
	callq	store_expr
	xorq	%rdi, %rdi
	callq	expand_start_loop
	movq	sizetype_tab+8(%rip), %rbx
	movq	8(%r12), %rsi
	movq	-120(%rbp), %rcx        # 8-byte Reload
	movq	104(%rcx), %rcx
	movq	%rax, %r13
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	%r12, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	sizetype_tab+8(%rip), %rdi
	movq	-80(%rbp), %rcx         # 8-byte Reload
	movq	40(%rcx), %rsi
	movq	%rax, %rbx
	callq	convert
	movq	$61, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	callq	expand_expr
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	highest_pow2_factor
	movq	-56(%rbp), %rdi         # 8-byte Reload
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	offset_address
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	adjust_address_1
	movq	-64(%rbp), %rdi         # 8-byte Reload
	movzbl	16(%rdi), %ecx
	cmpq	$46, %rcx
	jne	.LBB43_131
# BB#130:                               # %if.then591
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	$63, %rcx
	movq	%r13, %rdx
	movq	-112(%rbp), %r13        # 8-byte Reload
	movq	%r13, %rbx
	sarq	%cl, %rbx
	movq	$61, %rcx
	shrq	%cl, %rbx
	addq	%r13, %rbx
	movq	%rdx, %r13
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%rax, %rsi
	movq	-88(%rbp), %rdx         # 8-byte Reload
	movq	%rbx, %rcx
	callq	store_constructor
	jmp	.LBB43_132
.LBB43_131:                             # %if.else592
                                        #   in Loop: Header=BB43_99 Depth=1
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	store_expr
.LBB43_132:                             # %if.end594
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	integer_types+40(%rip), %rsi
	movq	$97, %rdi
	xorq	%rax, %rax
	movq	%r12, %rdx
	movq	-144(%rbp), %rcx        # 8-byte Reload
	callq	build
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	expand_exit_loop_if_false
	movq	8(%r12), %rsi
	movq	global_trees+96(%rip), %rcx
	movq	$130, %rdi
	xorq	%rax, %rax
	movq	%r12, %rdx
	callq	build
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	expand_increment
	callq	expand_end_loop
	movq	-152(%rbp), %rdi        # 8-byte Reload
	callq	emit_label
	.align	16, 0x90
.LBB43_148:                             # %for.inc688
                                        #   in Loop: Header=BB43_99 Depth=1
	movq	-72(%rbp), %rcx         # 8-byte Reload
	movq	(%rcx), %rcx
	incq	-104(%rbp)              # 8-byte Folded Spill
	testq	%rcx, %rcx
	movq	-80(%rbp), %r13         # 8-byte Reload
	jne	.LBB43_99
	jmp	.LBB43_149
.LBB43_92:                              # %if.else379
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	clear_storage
	jmp	.LBB43_97
.LBB43_8:                               # %if.then48
	movq	const_tiny_rtx(,%rax,8), %rsi
	movq	%r15, %rdi
	callq	emit_move_insn
	jmp	.LBB43_13
.LBB43_174:                             # %if.else809
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	-64(%rbp), %rcx         # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB43_176
# BB#175:
	movq	%r15, -56(%rbp)         # 8-byte Spill
	jmp	.LBB43_185
.LBB43_176:                             # %if.then811
	cmpq	$0, (%r13)
	jne	.LBB43_184
# BB#177:                               # %lor.lhs.false816
	cmpq	$0, 24(%r13)
	je	.LBB43_178
# BB#180:                               # %cond.false824
	movq	32(%r13), %rdi
	xorq	%rsi, %rsi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB43_184
# BB#181:                               # %lor.lhs.false829
	movq	24(%r13), %rdi
	xorq	%rsi, %rsi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB43_184
# BB#182:                               # %lor.lhs.false834
	movq	32(%r13), %rdi
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	24(%r13), %rdi
	movq	%rax, %r12
	xorq	%rsi, %rsi
	callq	tree_low_cst
	incq	%r12
	subq	%rax, %r12
	cmpq	%rbx, %r12
	jne	.LBB43_184
# BB#183:
	movq	%r15, -56(%rbp)         # 8-byte Spill
	jmp	.LBB43_185
.LBB43_178:                             # %cond.true821
	cmpq	$1, %rbx
	jne	.LBB43_184
# BB#179:
	movq	%r15, -56(%rbp)         # 8-byte Spill
	jmp	.LBB43_185
.LBB43_184:                             # %if.then845
	movq	-112(%rbp), %rdi        # 8-byte Reload
	callq	expr_size
	movq	%r15, %rdi
	movq	%r15, -56(%rbp)         # 8-byte Spill
	movq	%rax, %rsi
	callq	clear_storage
	jmp	.LBB43_185
.LBB43_202:                             # %for.inc975
                                        #   in Loop: Header=BB43_185 Depth=1
	movq	%rbx, -56(%rbp)         # 8-byte Spill
	movq	-72(%rbp), %r13         # 8-byte Reload
	movq	(%r13), %r13
.LBB43_185:                             # %for.cond851.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%r13, %r13
	je	.LBB43_149
# BB#186:                               # %for.body854
                                        #   in Loop: Header=BB43_185 Depth=1
	movq	24(%r13), %rbx
	movq	32(%r13), %r12
	xorq	%rsi, %rsi
	movq	$66, %rdx
	movq	$2, %rcx
	movq	-104(%rbp), %rdi        # 8-byte Reload
	callq	expand_expr
	movq	%rax, -64(%rbp)         # 8-byte Spill
	testq	%rbx, %rbx
	movq	-96(%rbp), %r15         # 8-byte Reload
	jne	.LBB43_187
# BB#188:                               # %if.then862
                                        #   in Loop: Header=BB43_185 Depth=1
	movq	%r13, -72(%rbp)         # 8-byte Spill
	movq	%r12, %rdi
	callq	save_expr
	movq	%rax, %rbx
	movq	%rbx, %r12
	jmp	.LBB43_189
.LBB43_187:                             #   in Loop: Header=BB43_185 Depth=1
	movq	%r13, -72(%rbp)         # 8-byte Spill
.LBB43_189:                             # %if.end864
                                        #   in Loop: Header=BB43_185 Depth=1
	movq	sizetype_tab(%rip), %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	sizetype_tab(%rip), %rdi
	movq	%rax, %r14
	movq	%r12, %rsi
	callq	convert
	movq	%rax, %r12
	movq	%r15, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	-56(%rbp), %rbx         # 8-byte Reload
	jne	.LBB43_191
# BB#190:                               # %if.then869
                                        #   in Loop: Header=BB43_185 Depth=1
	movq	$60, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	size_binop
	movq	%rax, %r14
	movq	$60, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	size_binop
	movq	%rax, %r12
.LBB43_191:                             # %if.end872
                                        #   in Loop: Header=BB43_185 Depth=1
	xorq	%rsi, %rsi
	movq	$66, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	callq	expand_expr
	movq	%rax, -80(%rbp)         # 8-byte Spill
	xorq	%rsi, %rsi
	movq	$66, %rdx
	movq	$2, %rcx
	movq	%r12, %rdi
	callq	expand_expr
	movl	(%rbx), %edi
	movq	%rax, %r15
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	movq	%rbx, %r13
	je	.LBB43_194
# BB#192:                               # %if.end872
                                        #   in Loop: Header=BB43_185 Depth=1
	cmpq	$61, %rax
	jne	.LBB43_203
# BB#193:                               # %if.then879
                                        #   in Loop: Header=BB43_185 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	xorq	%rsi, %rsi
	callq	type_for_mode
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	build_qualified_type
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	%rax, %rdi
	callq	assign_temp
	movq	%rax, %r13
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
.LBB43_194:                             # %if.end895
                                        #   in Loop: Header=BB43_185 Depth=1
	movzbl	16(%r14), %eax
	cmpq	$25, %rax
	jne	.LBB43_199
# BB#195:                               # %land.lhs.true902
                                        #   in Loop: Header=BB43_185 Depth=1
	movzbl	16(%r12), %eax
	cmpq	$25, %rax
	jne	.LBB43_199
# BB#196:                               # %land.lhs.true909
                                        #   in Loop: Header=BB43_185 Depth=1
	movq	32(%r14), %r14
	testq	$7, %r14
	jne	.LBB43_199
# BB#197:                               # %land.lhs.true913
                                        #   in Loop: Header=BB43_185 Depth=1
	movq	32(%r12), %r12
	incq	%r12
	testq	$7, %r12
	jne	.LBB43_199
# BB#198:                               # %if.then921
                                        #   in Loop: Header=BB43_185 Depth=1
	movq	libfunc_table+128(%rip), %rax
	movq	%rax, -64(%rbp)         # 8-byte Spill
	movq	8(%r13), %rdi
	movq	$63, %rcx
	movq	%r14, %rsi
	sarq	%cl, %rsi
	movq	$61, %rcx
	shrq	%cl, %rsi
	addq	%r14, %rsi
	movq	$3, %rcx
	sarq	%cl, %rsi
	callq	plus_constant_wide
	movq	%rax, -80(%rbp)         # 8-byte Spill
	movl	target_flags(%rip), %r15d
	movq	$25, %rcx
	shrq	%cl, %r15
	andq	$1, %r15
	movq	integer_types+40(%rip), %rax
	orq	$4, %r15
	movq	const_int_rtx+504(%rip), %rcx
	movq	%rcx, -88(%rbp)         # 8-byte Spill
	movl	60(%rax), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	movq	%rax, -112(%rbp)        # 8-byte Spill
	subq	%r14, %r12
	movq	$63, %rcx
	movq	%r12, %rsi
	sarq	%cl, %rsi
	movq	$61, %rcx
	shrq	%cl, %rsi
	addq	%r12, %rsi
	movq	$3, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	sizetype_tab(%rip), %rcx
	movl	60(%rcx), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	subq	$32, %rsp
	movq	%rdx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movq	-112(%rbp), %rax        # 8-byte Reload
	movq	%rax, 8(%rsp)
	movabsq	$0, %rsi
	movabsq	$0, %rdx
	movq	$3, %rcx
	xorq	%rax, %rax
	movq	-88(%rbp), %rdi         # 8-byte Reload
	movq	%rdi, (%rsp)
	movq	-64(%rbp), %rdi         # 8-byte Reload
	movq	-80(%rbp), %r8          # 8-byte Reload
	movq	%r15, %r9
	callq	emit_library_call
	addq	$32, %rsp
	jmp	.LBB43_200
.LBB43_199:                             # %if.else941
                                        #   in Loop: Header=BB43_185 Depth=1
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$68, %rdi
	movabsq	$.L.str6, %rdx
	callq	gen_rtx_fmt_s
	movq	%rax, %r10
	movq	8(%r13), %r8
	movq	sizetype_tab(%rip), %rax
	movl	target_flags(%rip), %r9d
	movq	$25, %rcx
	shrq	%cl, %r9
	movl	60(%rax), %eax
	andq	$1, %r9
	orq	$4, %r9
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	subq	$48, %rsp
	movq	%rax, 40(%rsp)
	movq	%r15, 32(%rsp)
	movq	%rax, 24(%rsp)
	movq	-80(%rbp), %rcx         # 8-byte Reload
	movq	%rcx, 16(%rsp)
	movq	%rax, 8(%rsp)
	movabsq	$0, %rsi
	movabsq	$0, %rdx
	movq	$4, %rcx
	xorq	%rax, %rax
	movq	-64(%rbp), %rdi         # 8-byte Reload
	movq	%rdi, (%rsp)
	movq	%r10, %rdi
	callq	emit_library_call
	addq	$48, %rsp
.LBB43_200:                             # %if.end967
                                        #   in Loop: Header=BB43_185 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	jne	.LBB43_202
# BB#201:                               # %if.then972
                                        #   in Loop: Header=BB43_185 Depth=1
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	emit_move_insn
	jmp	.LBB43_202
.LBB43_149:                             # %if.end982
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB43_172:                             # %if.else795
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.store_constructor, %rcx
	movq	$4905, %rsi             # imm = 0x1329
	jmp	.LBB43_41
.LBB43_40:                              # %if.then164
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.store_constructor, %rcx
	movq	$4498, %rsi             # imm = 0x1192
	jmp	.LBB43_41
.LBB43_203:                             # %if.else893
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.store_constructor, %rcx
	movq	$4975, %rsi             # imm = 0x136F
.LBB43_41:                              # %if.then164
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp578:
	.size	store_constructor, .Ltmp578-store_constructor
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI43_0:
	.quad	.LBB43_55
	.quad	.LBB43_204
	.quad	.LBB43_204
	.quad	.LBB43_204
	.quad	.LBB43_204
	.quad	.LBB43_204
	.quad	.LBB43_204
	.quad	.LBB43_204
	.quad	.LBB43_204
	.quad	.LBB43_55
	.quad	.LBB43_150
	.quad	.LBB43_2
	.quad	.LBB43_9
	.quad	.LBB43_2

	.text
	.globl	string_constant
	.align	16, 0x90
	.type	string_constant,@function
string_constant:                        # @string_constant
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp583:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp584:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp585:
	.cfi_def_cfa_offset 32
.Ltmp586:
	.cfi_offset %rbx, -32
.Ltmp587:
	.cfi_offset %r14, -24
.Ltmp588:
	.cfi_offset %r15, -16
	movq	global_trees(%rip), %rcx
	movq	%rsi, %r14
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB44_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rdi), %edx
	leaq	-114(%rdx), %rax
	andq	%r8, %rax
	cmpq	$2, %rax
	ja	.LBB44_4
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB44_1 Depth=1
	movq	32(%rdi), %rdx
	xorq	%rax, %rax
	cmpq	%rcx, %rdx
	je	.LBB44_21
# BB#3:                                 # %land.rhs
                                        #   in Loop: Header=BB44_1 Depth=1
	movq	8(%rdi), %rsi
	movq	8(%rdx), %rdi
	movl	60(%rsi), %esi
	movl	60(%rdi), %edi
	xorq	%rsi, %rdi
	testq	$65024, %rdi            # imm = 0xFE00
	movq	%rdx, %rdi
	je	.LBB44_1
	jmp	.LBB44_21
.LBB44_4:                               # %while.end
	xorq	%rax, %rax
	cmpq	$59, %rdx
	jne	.LBB44_5
# BB#8:                                 # %if.then53
	movq	32(%rdi), %rdx
	movq	40(%rdi), %rdi
	.align	16, 0x90
.LBB44_9:                               # %while.cond60
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %r15
	movzbl	16(%r15), %r9d
	leaq	-114(%r9), %rdx
	andq	%r8, %rdx
	cmpq	$2, %rdx
	ja	.LBB44_12
# BB#10:                                # %land.lhs.true78
                                        #   in Loop: Header=BB44_9 Depth=1
	movq	32(%r15), %rdx
	cmpq	%rcx, %rdx
	je	.LBB44_12
# BB#11:                                # %land.rhs83
                                        #   in Loop: Header=BB44_9 Depth=1
	movq	8(%r15), %rbx
	movq	8(%rdx), %rsi
	movl	60(%rbx), %ebx
	movl	60(%rsi), %esi
	xorq	%rbx, %rsi
	testq	$65024, %rsi            # imm = 0xFE00
	je	.LBB44_9
	.align	16, 0x90
.LBB44_12:                              # %while.cond108
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rbx
	movzbl	16(%rbx), %edx
	leaq	-114(%rdx), %rsi
	andq	%r8, %rsi
	cmpq	$2, %rsi
	ja	.LBB44_15
# BB#13:                                # %land.lhs.true126
                                        #   in Loop: Header=BB44_12 Depth=1
	movq	32(%rbx), %rdi
	cmpq	%rcx, %rdi
	je	.LBB44_15
# BB#14:                                # %land.rhs131
                                        #   in Loop: Header=BB44_12 Depth=1
	movq	8(%rbx), %r10
	movq	8(%rdi), %rsi
	movl	60(%r10), %r10d
	movl	60(%rsi), %esi
	xorq	%r10, %rsi
	testq	$65024, %rsi            # imm = 0xFE00
	je	.LBB44_12
.LBB44_15:                              # %while.end155
	cmpq	$121, %r9
	jne	.LBB44_18
# BB#16:                                # %land.lhs.true161
	movq	32(%r15), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$29, %rcx
	jne	.LBB44_18
# BB#17:                                # %if.then170
	movq	sizetype_tab(%rip), %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	%rax, (%r14)
	movq	32(%r15), %rax
	jmp	.LBB44_21
.LBB44_5:                               # %while.end
	cmpq	$121, %rdx
	jne	.LBB44_21
# BB#6:                                 # %land.lhs.true36
	movq	32(%rdi), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$29, %rcx
	jne	.LBB44_21
# BB#7:                                 # %if.then
	movq	global_trees+120(%rip), %rax
	movq	%rax, (%r14)
	movq	32(%rdi), %rax
	jmp	.LBB44_21
.LBB44_18:                              # %if.else174
	cmpq	$121, %rdx
	jne	.LBB44_21
# BB#19:                                # %land.lhs.true180
	movq	32(%rbx), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$29, %rcx
	jne	.LBB44_21
# BB#20:                                # %if.then189
	movq	sizetype_tab(%rip), %rdi
	movq	%r15, %rsi
	callq	convert
	movq	%rax, (%r14)
	movq	32(%rbx), %rax
.LBB44_21:                              # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp589:
	.size	string_constant, .Ltmp589-string_constant
	.cfi_endproc

	.globl	do_jump_by_parts_greater_rtx
	.align	16, 0x90
	.type	do_jump_by_parts_greater_rtx,@function
do_jump_by_parts_greater_rtx:           # @do_jump_by_parts_greater_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp596:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp597:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp598:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp599:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp600:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp601:
	.cfi_def_cfa_offset 144
.Ltmp602:
	.cfi_offset %rbx, -48
.Ltmp603:
	.cfi_offset %r12, -40
.Ltmp604:
	.cfi_offset %r13, -32
.Ltmp605:
	.cfi_offset %r14, -24
.Ltmp606:
	.cfi_offset %r15, -16
	movq	%r9, 72(%rsp)           # 8-byte Spill
	movq	%r8, %r12
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	%rsi, %rbx
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %eax
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	xorq	%rcx, %rcx
	xorq	%rdx, %rdx
	idivq	%rsi
	movq	%rax, 80(%rsp)          # 8-byte Spill
	testq	%r9, %r9
	je	.LBB45_2
# BB#1:                                 # %entry
	testq	%r12, %r12
	jne	.LBB45_3
.LBB45_2:                               # %if.then
	callq	gen_label_rtx
	movq	%rax, %rcx
.LBB45_3:                               # %if.end
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	jne	.LBB45_5
# BB#4:                                 # %if.end
	movq	%rcx, 72(%rsp)          # 8-byte Spill
.LBB45_5:                               # %if.end
	testq	%r12, %r12
	jne	.LBB45_7
# BB#6:                                 # %if.end
	movq	%rcx, %r12
.LBB45_7:                               # %if.end
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%r12, 64(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	80(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB45_14
# BB#8:                                 # %for.body.lr.ph
	movq	80(%rsp), %rax          # 8-byte Reload
	leaq	-1(%rax), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%r15, %r15
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB45_9:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rdx, %rbx
	movabsq	$0, %rax
	je	.LBB45_11
# BB#10:                                # %for.body
                                        #   in Loop: Header=BB45_9 Depth=1
	movabsq	$1, %rax
.LBB45_11:                              # %for.body
                                        #   in Loop: Header=BB45_9 Depth=1
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r15, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	24(%rsp), %rbx          # 8-byte Reload
	subq	%r15, %rbx
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	32(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdx
	callq	operand_subword_force
	movq	%rax, %r14
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	operand_subword_force
	movq	%rax, %r13
	testq	%r12, %r12
	movabsq	$0, %rcx
	jle	.LBB45_13
# BB#12:                                # %for.body
                                        #   in Loop: Header=BB45_9 Depth=1
	movabsq	$1, %rcx
.LBB45_13:                              # %for.body
                                        #   in Loop: Header=BB45_9 Depth=1
	movl	word_mode(%rip), %r8d
	movq	88(%rsp), %rax          # 8-byte Reload
	orq	%rax, %rcx
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$105, %rdx
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	do_compare_rtx_and_jump
	movl	word_mode(%rip), %r8d
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	%r12, 8(%rsp)
	movq	$102, %rdx
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r14, %rdi
	movq	%r13, %rsi
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rcx
	callq	do_compare_rtx_and_jump
	incq	%r15
	movq	%r15, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	movq	80(%rsp), %rcx          # 8-byte Reload
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jne	.LBB45_9
.LBB45_14:                              # %for.end
	testq	%r12, %r12
	je	.LBB45_16
# BB#15:                                # %if.then19
	movq	%r12, %rdi
	callq	emit_jump
.LBB45_16:                              # %if.end20
	movq	16(%rsp), %rdi          # 8-byte Reload
	addq	$96, %rsp
	testq	%rdi, %rdi
	je	.LBB45_17
# BB#18:                                # %if.then22
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_label  # TAILCALL
.LBB45_17:                              # %if.end24
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp607:
	.size	do_jump_by_parts_greater_rtx, .Ltmp607-do_jump_by_parts_greater_rtx
	.cfi_endproc

	.globl	do_compare_rtx_and_jump
	.align	16, 0x90
	.type	do_compare_rtx_and_jump,@function
do_compare_rtx_and_jump:                # @do_compare_rtx_and_jump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp614:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp615:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp616:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp617:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp618:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp619:
	.cfi_def_cfa_offset 96
.Ltmp620:
	.cfi_offset %rbx, -48
.Ltmp621:
	.cfi_offset %r12, -40
.Ltmp622:
	.cfi_offset %r13, -32
.Ltmp623:
	.cfi_offset %r14, -24
.Ltmp624:
	.cfi_offset %r15, -16
	movq	104(%rsp), %rax
	movq	96(%rsp), %r13
	movq	%r9, 8(%rsp)            # 8-byte Spill
	movq	%r8, %rbx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%rdi, %r12
	testq	%rax, %rax
	je	.LBB46_2
# BB#1:
	movq	%r13, 24(%rsp)          # 8-byte Spill
	movq	%rax, %r13
	jmp	.LBB46_6
.LBB46_2:                               # %land.lhs.true
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	mode_class(,%rax,4), %ecx
	cmpq	$8, %rcx
	ja	.LBB46_5
# BB#3:                                 # %land.lhs.true
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$324, %rax              # imm = 0x144
	je	.LBB46_5
# BB#4:
	movq	%r13, 24(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	jmp	.LBB46_6
.LBB46_5:                               # %if.then
	movq	%r15, %rdi
	callq	reverse_condition
	movq	%rax, %r15
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
.LBB46_6:                               # %if.end
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	swap_commutative_operands_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB46_7
# BB#8:                                 # %if.then10
	movq	%r15, %rdi
	callq	swap_condition
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%r12, %r15
	jmp	.LBB46_9
.LBB46_7:
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movq	%r14, %r15
	movq	%r12, %r14
.LBB46_9:                               # %if.end12
	movl	flag_force_mem(%rip), %eax
	testq	%rax, %rax
	je	.LBB46_11
# BB#10:                                # %if.then14
	movq	%r14, %rdi
	callq	force_not_mem
	movq	%rax, %r14
	movq	%r15, %rdi
	callq	force_not_mem
	movq	%rax, %r15
.LBB46_11:                              # %if.end17
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB46_15
# BB#12:                                # %if.then.i
	movslq	(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB46_14
# BB#13:                                # %if.then3.i
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	adjust_stack
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
.LBB46_14:                              # %if.end.i
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.LBB46_15:                              # %do_pending_stack_adjust.exit
	movzbl	(%r14), %eax
	cmpq	$54, %rax
	jne	.LBB46_23
# BB#16:                                # %land.lhs.true19
	movzbl	(%r15), %eax
	cmpq	$54, %rax
	jne	.LBB46_23
# BB#17:                                # %land.lhs.true23
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	simplify_relational_operation
	testq	%rax, %rax
	je	.LBB46_23
# BB#18:                                # %if.then26
	movq	const_true_rtx(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB46_19
# BB#21:                                # %if.else
	movq	24(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	jne	.LBB46_22
	jmp	.LBB46_28
.LBB46_23:                              # %if.end36
	xorq	%rbx, %rbx
	testq	%r13, %r13
	jne	.LBB46_25
# BB#24:                                # %if.then38
	movq	$1, %rbx
	callq	gen_label_rtx
	movq	%rax, %r13
.LBB46_25:                              # %if.end40
	movq	%r13, (%rsp)
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	16(%rsp), %r9           # 8-byte Reload
	callq	emit_cmp_and_jump_insns
	movq	24(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB46_27
# BB#26:                                # %if.then42
	callq	emit_jump
.LBB46_27:                              # %if.end43
	testq	%rbx, %rbx
	je	.LBB46_28
# BB#29:                                # %if.then45
	movq	%r13, %rdi
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_label  # TAILCALL
.LBB46_19:                              # %if.then28
	testq	%r13, %r13
	je	.LBB46_28
# BB#20:                                # %if.then30
	movq	%r13, %rdi
.LBB46_22:                              # %if.then33
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_jump  # TAILCALL
.LBB46_28:                              # %if.end47
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp625:
	.size	do_compare_rtx_and_jump, .Ltmp625-do_compare_rtx_and_jump
	.cfi_endproc

	.align	16, 0x90
	.type	do_store_flag,@function
do_store_flag:                          # @do_store_flag
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp632:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp633:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp634:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp635:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp636:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp637:
	.cfi_def_cfa_offset 128
.Ltmp638:
	.cfi_offset %rbx, -48
.Ltmp639:
	.cfi_offset %r12, -40
.Ltmp640:
	.cfi_offset %r13, -32
.Ltmp641:
	.cfi_offset %r14, -24
.Ltmp642:
	.cfi_offset %r15, -16
	movzbl	16(%rdi), %eax
	xorq	%r8, %r8
	cmpq	$96, %rax
	jne	.LBB47_2
# BB#1:                                 # %if.then
	movq	32(%rdi), %rdi
	movq	$1, %r8
.LBB47_2:                               # %if.end
	movq	32(%rdi), %rax
	movq	global_trees(%rip), %r9
	cmpq	%r9, %rax
	je	.LBB47_4
# BB#3:                                 # %if.end
	movq	40(%rdi), %r10
	cmpq	%r9, %r10
	je	.LBB47_4
# BB#5:                                 # %if.end12
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	movq	8(%rax), %rdx
	movl	60(%rdx), %ebx
	movq	$9, %rcx
	movq	%rbx, %r15
	shrq	%cl, %r15
	movl	16(%rdx), %esi
	andq	$127, %r15
	movq	$13, %rcx
	shrq	%cl, %rsi
	xorq	%r11, %r11
	cmpq	$51, %r15
	je	.LBB47_129
# BB#6:
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%r8, 32(%rsp)           # 8-byte Spill
	andq	$1, %rsi
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB47_7:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %r14
	movzbl	16(%r14), %ecx
	addq	$-114, %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	ja	.LBB47_10
# BB#8:                                 # %land.lhs.true
                                        #   in Loop: Header=BB47_7 Depth=1
	movq	32(%r14), %rax
	cmpq	%r9, %rax
	je	.LBB47_10
# BB#9:                                 # %land.rhs
                                        #   in Loop: Header=BB47_7 Depth=1
	movq	8(%rax), %rcx
	movl	60(%rcx), %ecx
	xorq	%rcx, %rbx
	testq	$65024, %rbx            # imm = 0xFE00
	movq	%rcx, %rbx
	je	.LBB47_7
.LBB47_10:
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB47_11:                              # %while.cond68
                                        # =>This Inner Loop Header: Depth=1
	movq	%r10, %r13
	movzbl	16(%r13), %ecx
	addq	$-114, %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	ja	.LBB47_14
# BB#12:                                # %land.lhs.true86
                                        #   in Loop: Header=BB47_11 Depth=1
	movq	32(%r13), %r10
	cmpq	%r9, %r10
	je	.LBB47_14
# BB#13:                                # %land.rhs91
                                        #   in Loop: Header=BB47_11 Depth=1
	movq	8(%r13), %rcx
	movq	8(%r10), %rsi
	movl	60(%rcx), %ecx
	movl	60(%rsi), %esi
	xorq	%rcx, %rsi
	testq	$65024, %rsi            # imm = 0xFE00
	je	.LBB47_11
.LBB47_14:                              # %while.end115
	movzbl	16(%rdi), %eax
	addq	$-97, %rax
	andq	%r12, %rax
	cmpq	$12, %rax
	ja	.LBB47_37
# BB#15:                                # %while.end115
	movq	$103, %rcx
	jmpq	*.LJTI47_0(,%rax,8)
.LBB47_16:                              # %sw.bb121
	movq	%r13, %rdi
	callq	integer_onep
	testq	%r12, %rax
	je	.LBB47_18
# BB#17:                                # %if.then122
	movq	global_trees+88(%rip), %r13
	movq	48(%rsp), %rax          # 8-byte Reload
	leaq	106(,%rax,4), %rcx
	jmp	.LBB47_39
.LBB47_4:                               # %if.then11
	movq	const_int_rtx+512(%rip), %r11
.LBB47_129:                             # %return
	movq	%r11, %rax
.LBB47_130:                             # %return
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB47_19:                              # %sw.bb127
	movq	48(%rsp), %rcx          # 8-byte Reload
	testq	%rcx, %rcx
	jne	.LBB47_22
# BB#20:                                # %land.lhs.true129
	movq	%r13, %rdi
	movq	%rcx, %rbx
	callq	integer_all_onesp
	movq	%rbx, %rcx
	testq	%r12, %rax
	je	.LBB47_22
# BB#21:                                # %if.then132
	movq	global_trees+88(%rip), %r13
	movq	$107, %rcx
	jmp	.LBB47_39
.LBB47_23:                              # %sw.bb137
	movq	48(%rsp), %rcx          # 8-byte Reload
	testq	%rcx, %rcx
	jne	.LBB47_26
# BB#24:                                # %land.lhs.true139
	movq	%r13, %rdi
	movq	%rcx, %rbx
	callq	integer_all_onesp
	movq	%rbx, %rcx
	testq	%r12, %rax
	je	.LBB47_26
# BB#25:                                # %if.then142
	movq	global_trees+88(%rip), %r13
	movq	$104, %rcx
	jmp	.LBB47_39
.LBB47_27:                              # %sw.bb147
	movq	%r13, %rdi
	callq	integer_onep
	testq	%r12, %rax
	je	.LBB47_29
# BB#28:                                # %if.then150
	movq	global_trees+88(%rip), %r13
	movq	48(%rsp), %rax          # 8-byte Reload
	leaq	105(,%rax,4), %rcx
	jmp	.LBB47_39
.LBB47_38:                              # %sw.bb120
	movq	$102, %rcx
	jmp	.LBB47_39
.LBB47_30:                              # %sw.bb157
	movq	$112, %rcx
	jmp	.LBB47_39
.LBB47_31:                              # %sw.bb158
	movq	$113, %rcx
	jmp	.LBB47_39
.LBB47_32:                              # %sw.bb159
	movq	$118, %rcx
	jmp	.LBB47_39
.LBB47_33:                              # %sw.bb160
	movq	$117, %rcx
	jmp	.LBB47_39
.LBB47_34:                              # %sw.bb161
	movq	$116, %rcx
	jmp	.LBB47_39
.LBB47_35:                              # %sw.bb162
	movq	$115, %rcx
	jmp	.LBB47_39
.LBB47_36:                              # %sw.bb163
	movq	$114, %rcx
	jmp	.LBB47_39
.LBB47_37:                              # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.do_store_flag, %rdx
	movq	$10281, %rsi            # imm = 0x2829
	callq	fancy_abort
.LBB47_22:                              # %if.else133
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	leaq	106(,%rcx,4), %rcx
	jmp	.LBB47_39
.LBB47_26:                              # %if.else143
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	leaq	105(,%rcx,4), %rcx
	jmp	.LBB47_39
.LBB47_18:                              # %if.else
	movq	48(%rsp), %rax          # 8-byte Reload
	leaq	107(,%rax,4), %rcx
	jmp	.LBB47_39
.LBB47_29:                              # %if.else153
	movq	48(%rsp), %rax          # 8-byte Reload
	leaq	104(,%rax,4), %rcx
.LBB47_39:                              # %sw.epilog
	movzbl	16(%r14), %eax
	addq	$-25, %rax
	andq	%r12, %rax
	cmpq	$1, %rax
	ja	.LBB47_40
# BB#41:                                # %if.then175
	movq	%rcx, %rdi
	callq	swap_condition
	movq	%rax, %rcx
	movq	%r14, 40(%rsp)          # 8-byte Spill
	jmp	.LBB47_42
.LBB47_40:
	movq	%r13, 40(%rsp)          # 8-byte Spill
	movq	%r14, %r13
.LBB47_42:                              # %if.end177
	movabsq	$4294967294, %r14       # imm = 0xFFFFFFFE
	andq	%rcx, %r14
	cmpq	$102, %r14
	jne	.LBB47_76
# BB#43:                                # %land.lhs.true181
	movzbl	16(%r13), %eax
	cmpq	$88, %rax
	jne	.LBB47_76
# BB#44:                                # %land.lhs.true187
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rbx
	callq	integer_zerop
	movq	%rbx, %rcx
	testq	%r12, %rax
	je	.LBB47_76
# BB#45:                                # %land.lhs.true190
	movq	40(%r13), %rdi
	movq	%rcx, %rbx
	callq	integer_pow2p
	movq	%rbx, %rcx
	testq	%r12, %rax
	je	.LBB47_76
# BB#46:                                # %if.then196
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	32(%r13), %r14
	movq	40(%r13), %rdi
	callq	tree_log2
	movzbl	16(%r14), %ecx
	movq	%rax, %r13
	cmpq	$83, %rcx
	jne	.LBB47_52
# BB#47:                                # %land.lhs.true209
	movq	40(%r14), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB47_52
# BB#48:                                # %land.lhs.true218
	cmpq	$0, 40(%rdi)
	jne	.LBB47_52
# BB#49:                                # %land.lhs.true224
	movq	24(%rsp), %rax          # 8-byte Reload
	movl	60(%rax), %eax
	andq	$511, %rax              # imm = 0x1FF
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB47_52
# BB#50:                                # %land.lhs.true229
	movq	%r13, %rsi
	subq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB47_52
# BB#51:                                # %if.then240
	movq	40(%r14), %rax
	movq	32(%r14), %r14
	movq	32(%rax), %rax
	andq	%r12, %r13
	addq	%rax, %r13
.LBB47_52:                              # %if.end251
	testq	%rbx, %rbx
	je	.LBB47_58
# BB#53:                                # %lor.lhs.false.i
	movl	(%rbx), %eax
	andq	$67174399, %rax         # imm = 0x400FFFF
	cmpq	$61, %rax
	jne	.LBB47_58
# BB#54:                                # %lor.lhs.false5.i
	movl	8(%rbx), %eax
	cmpq	$53, %rax
	jb	.LBB47_58
# BB#55:                                # %lor.lhs.false7.i
	callq	preserve_subexpressions_p
	testq	%r12, %rax
	jne	.LBB47_58
# BB#56:                                # %lor.lhs.false262
	movzbl	2(%rbx), %eax
	movq	%r15, %rcx
	andq	%r12, %rcx
	cmpq	%rcx, %rax
	jne	.LBB47_58
# BB#57:                                # %lor.lhs.false268
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	safe_from_p
	testq	%r12, %rax
	jne	.LBB47_59
.LBB47_58:                              # %if.then271
	xorq	%rbx, %rbx
.LBB47_59:                              # %if.end272
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	expand_expr
	movq	%rax, %r11
	testq	%r12, %r13
	je	.LBB47_60
# BB#61:                                # %if.then276
	movq	$32, %rcx
	movq	%r13, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	xorq	%rsi, %rsi
	movq	%r11, %r14
	callq	size_int_wide
	movq	$83, %rdi
	movq	$1, %r9
	movq	%r15, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	movq	%rbx, %r8
	movq	%rbx, %r14
	callq	expand_shift
	movq	%rax, %r11
	jmp	.LBB47_62
.LBB47_76:                              # %if.end311
	movq	$2, %rdx
	movq	%rcx, %rdi
	movq	%rcx, %rbx
	movq	%r15, %rsi
	callq	can_compare_p
	testq	%r12, %rax
	je	.LBB47_77
# BB#78:                                # %if.end315
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movslq	setcc_gen_code(,%rax,4), %rax
	cmpq	$1317, %rax             # imm = 0x525
	movabsq	$0, %r11
	je	.LBB47_81
# BB#79:                                # %lor.lhs.false319
	movq	64(%rsp), %rcx          # 8-byte Reload
	testq	%r12, %rcx
	je	.LBB47_94
# BB#80:                                # %land.lhs.true321
	leaq	(%rax,%rax,4), %rax
	movq	insn_data+24(,%rax,8), %rax
	movzbl	16(%rax), %eax
	movq	56(%rsp), %rcx          # 8-byte Reload
	andq	%r12, %rcx
	cmpq	%rcx, %rax
	je	.LBB47_94
.LBB47_81:                              # %if.then329
	movq	%rbx, %rax
	andq	%r12, %rax
	cmpq	$107, %rax
	je	.LBB47_84
# BB#82:                                # %lor.lhs.false335
	movq	64(%rsp), %rcx          # 8-byte Reload
	testq	%r12, %rcx
	jne	.LBB47_85
# BB#83:                                # %lor.lhs.false335
	cmpq	$104, %rax
	jne	.LBB47_85
.LBB47_84:                              # %land.lhs.true340
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	integer_zerop
	xorq	%r11, %r11
	testq	%r12, %rax
	jne	.LBB47_94
.LBB47_85:                              # %if.else344
	movslq	ix86_branch_cost(%rip), %rcx
	movq	%rbx, %rdi
	cmpq	$102, %r14
	je	.LBB47_86
# BB#87:                                # %if.else344
	movabsq	$1, %rax
	jmp	.LBB47_88
.LBB47_77:
	xorq	%r11, %r11
	jmp	.LBB47_129
.LBB47_86:
	movabsq	$0, %rax
.LBB47_88:                              # %if.else344
	testq	%rcx, %rcx
	js	.LBB47_129
# BB#89:                                # %if.else344
	movq	64(%rsp), %rcx          # 8-byte Reload
	testq	%r12, %rcx
	jne	.LBB47_129
# BB#90:                                # %if.else344
	testq	%rax, %rax
	jne	.LBB47_129
# BB#91:                                # %land.lhs.true355
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	je	.LBB47_129
# BB#92:                                # %land.lhs.true362
	movq	%r15, %rax
	movq	optab_table+272(%rip), %rdx
	andq	%r12, %rax
	movq	$4, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movl	8(%rdx,%rsi), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	movq	%rdi, %rbx
	jne	.LBB47_94
# BB#93:                                # %lor.lhs.false367
	movq	optab_table+296(%rip), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	8(%rdx,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB47_129
.LBB47_94:                              # %if.end378
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rbx          # 8-byte Reload
	testq	%rbx, %rbx
	je	.LBB47_100
# BB#95:                                # %lor.lhs.false.i644
	movl	(%rbx), %eax
	andq	$67174399, %rax         # imm = 0x400FFFF
	cmpq	$61, %rax
	jne	.LBB47_100
# BB#96:                                # %lor.lhs.false5.i648
	movl	8(%rbx), %eax
	cmpq	$53, %rax
	jb	.LBB47_100
# BB#97:                                # %lor.lhs.false7.i652
	callq	preserve_subexpressions_p
	testq	%r12, %rax
	jne	.LBB47_100
# BB#98:                                # %lor.lhs.false381
	movzbl	2(%rbx), %eax
	movq	%r15, %rcx
	andq	%r12, %rcx
	cmpq	%rcx, %rax
	jne	.LBB47_100
# BB#99:                                # %lor.lhs.false387
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	safe_from_p
	testq	%r12, %rax
	movq	%rbx, %rsi
	jne	.LBB47_101
.LBB47_100:                             # %if.then390
	xorq	%rsi, %rsi
.LBB47_101:                             # %if.end391
	movq	%r15, 64(%rsp)          # 8-byte Spill
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	callq	expand_expr
	movq	%rax, %r14
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	expand_expr
	movq	%rax, %r13
	testq	%rbx, %rbx
	jne	.LBB47_103
# BB#102:                               # %if.then396
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, %rbx
.LBB47_103:                             # %if.end398
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	callq	queued_subexp_p
	testq	%r12, %rax
	movq	%r14, %r15
	je	.LBB47_105
# BB#104:                               # %cond.true
	movq	%r14, %rdi
	callq	copy_rtx
	movq	%rax, %r15
.LBB47_105:                             # %cond.end
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	%r13, %rbx
	movq	%rbx, %rdi
	callq	queued_subexp_p
	testq	%r12, %rax
	movq	%rbx, %rcx
	je	.LBB47_107
# BB#106:                               # %cond.true405
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	%rax, %rcx
.LBB47_107:                             # %cond.end408
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	$1, (%rsp)
	movq	72(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdi
	movq	16(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %r8
	movq	48(%rsp), %r9           # 8-byte Reload
	callq	emit_store_flag
	movq	%rax, %r11
	testq	%r11, %r11
	je	.LBB47_110
# BB#108:                               # %if.then412
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	je	.LBB47_129
# BB#109:                               # %if.then414
	movq	optab_table+160(%rip), %rsi
	movq	const_int_rtx+520(%rip), %rcx
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r11, %rdx
	movq	%r11, %r8
	callq	expand_binop
	jmp	.LBB47_130
.LBB47_110:                             # %if.end417
	movzbl	(%r13), %eax
	cmpq	$61, %rax
	movq	24(%rsp), %r15          # 8-byte Reload
	jne	.LBB47_114
# BB#111:                               # %lor.lhs.false422
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	reg_mentioned_p
	testq	%r12, %rax
	jne	.LBB47_114
# BB#112:                               # %lor.lhs.false425
	movq	%r13, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	reg_mentioned_p
	testq	%r12, %rax
	je	.LBB47_113
.LBB47_114:                             # %if.then428
	movzbl	2(%r13), %edi
	callq	gen_reg_rtx
	movq	%rax, %rdi
.LBB47_115:                             # %if.end433
	movq	32(%rsp), %r12          # 8-byte Reload
	testq	%r12, %r12
	jne	.LBB47_116
# BB#117:                               # %if.end433
	movabsq	$const_int_rtx+520, %rax
	jmp	.LBB47_118
.LBB47_116:
	movabsq	$const_int_rtx+512, %rax
.LBB47_118:                             # %if.end433
	movq	(%rax), %rsi
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	callq	emit_move_insn
	xorq	%r9, %r9
	movq	%r15, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, %r8
	callq	compare_from_rtx
	movzbl	(%rax), %ebx
	cmpq	$54, %rbx
	jne	.LBB47_124
# BB#119:                               # %if.then445
	movq	const_int_rtx+512(%rip), %r11
	cmpq	%r11, %rax
	je	.LBB47_120
# BB#121:                               # %if.then445
	movabsq	$1, %rax
	jmp	.LBB47_122
.LBB47_124:                             # %if.end459
	callq	gen_label_rtx
	movq	bcc_gen_fctn(,%rbx,8), %rcx
	movq	%rax, %r14
	testq	%rcx, %rcx
	je	.LBB47_131
# BB#125:                               # %if.end468
	movq	%r14, %rdi
	callq	*%rcx
	movq	%rax, %rdi
	callq	emit_jump_insn
	testq	%r12, %r12
	jne	.LBB47_126
# BB#127:                               # %if.end468
	movabsq	$const_int_rtx+512, %rax
	jmp	.LBB47_128
.LBB47_120:
	movabsq	$0, %rax
.LBB47_122:                             # %if.then445
	movq	%rax, %rcx
	orq	%r12, %rcx
	xorq	$1, %rcx
	andq	%r12, %rax
	orq	%rcx, %rax
	jne	.LBB47_129
# BB#123:                               # %if.then445
	movq	const_int_rtx+520(%rip), %r11
	jmp	.LBB47_129
.LBB47_126:
	movabsq	$const_int_rtx+520, %rax
.LBB47_128:                             # %if.end468
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	(%rax), %rsi
	movq	%rdi, %rbx
	callq	emit_move_insn
	movq	%r14, %rdi
	callq	emit_label
	movq	%rbx, %r11
	jmp	.LBB47_129
.LBB47_60:
	movq	%rbx, %r14
.LBB47_62:                              # %if.end280
	movzbl	2(%r11), %eax
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, %rcx
	andq	%r12, %rcx
	cmpq	%rcx, %rax
	jne	.LBB47_64
# BB#63:
	movq	%rdi, %rbx
	jmp	.LBB47_65
.LBB47_64:                              # %if.then286
	xorq	%rsi, %rsi
	movq	$1, %rcx
	movq	%rdi, %rbx
	movq	%r11, %rdx
	callq	convert_modes
	movq	%rax, %r11
.LBB47_65:                              # %if.end288
	movq	16(%rsp), %rsi          # 8-byte Reload
	andq	%r12, %rsi
	cmpq	$102, %rsi
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	jne	.LBB47_67
# BB#66:                                # %if.end288
	movq	%rdx, %rcx
.LBB47_67:                              # %if.end288
	cmpq	$103, %rsi
	movq	%rax, %rsi
	jne	.LBB47_69
# BB#68:                                # %if.end288
	movq	%rdx, %rsi
.LBB47_69:                              # %if.end288
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	jne	.LBB47_71
# BB#70:                                # %if.end288
	movq	%rdx, %rax
.LBB47_71:                              # %if.end288
	movq	32(%rsp), %rdx          # 8-byte Reload
	testq	%rdx, %rcx
	jne	.LBB47_73
# BB#72:                                # %if.end288
	andq	%rax, %rsi
	je	.LBB47_74
.LBB47_73:                              # %if.then298
	movq	optab_table+160(%rip), %rsi
	movq	const_int_rtx+520(%rip), %rcx
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdi
	movq	%r11, %rdx
	movq	%r14, %r8
	callq	expand_binop
	movq	%rax, %r11
.LBB47_74:                              # %if.end300
	movq	24(%rsp), %rax          # 8-byte Reload
	movl	60(%rax), %eax
	andq	$511, %rax              # imm = 0x1FF
	decq	%rax
	andq	%r12, %rax
	andq	%r12, %r13
	cmpq	%rax, %r13
	je	.LBB47_129
# BB#75:                                # %if.then308
	movq	const_int_rtx+520(%rip), %rdx
	movq	%rbx, %rdi
	movq	%r11, %rsi
	movq	%r14, %rcx
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	expand_and  # TAILCALL
.LBB47_113:
	movq	%r13, %rdi
	jmp	.LBB47_115
.LBB47_131:                             # %if.then467
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.do_store_flag, %rcx
	movq	$10431, %rsi            # imm = 0x28BF
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp643:
	.size	do_store_flag, .Ltmp643-do_store_flag
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI47_0:
	.quad	.LBB47_16
	.quad	.LBB47_19
	.quad	.LBB47_23
	.quad	.LBB47_27
	.quad	.LBB47_39
	.quad	.LBB47_38
	.quad	.LBB47_30
	.quad	.LBB47_31
	.quad	.LBB47_32
	.quad	.LBB47_33
	.quad	.LBB47_34
	.quad	.LBB47_35
	.quad	.LBB47_36

	.text
	.globl	jumpif
	.align	16, 0x90
	.type	jumpif,@function
jumpif:                                 # @jumpif
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	jmp	do_jump  # TAILCALL
.Ltmp644:
	.size	jumpif, .Ltmp644-jumpif
	.cfi_endproc

	.globl	do_jump
	.align	16, 0x90
	.type	do_jump,@function
do_jump:                                # @do_jump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp651:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp652:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp653:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp654:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp655:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp656:
	.cfi_def_cfa_offset 144
.Ltmp657:
	.cfi_offset %rbx, -48
.Ltmp658:
	.cfi_offset %r12, -40
.Ltmp659:
	.cfi_offset %r13, -32
.Ltmp660:
	.cfi_offset %r14, -24
.Ltmp661:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	%rdi, %r13
	movq	40(%rax), %rbx
	movzbl	16(%r13), %r15d
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rsi, %r14
	jmp	.LBB49_1
	.align	16, 0x90
.LBB49_5:                               # %if.end.i
                                        #   in Loop: Header=BB49_1 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rbx), %rcx
	movq	16(%rax), %rax
	movq	%rcx, 40(%rax)
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rbx
.LBB49_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB49_6
# BB#2:                                 # %while.body.i
                                        #   in Loop: Header=BB49_1 Depth=1
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$24, %rax
	jne	.LBB49_4
# BB#3:                                 # %if.then.i
                                        #   in Loop: Header=BB49_1 Depth=1
	movq	8(%rdi), %rax
	movq	8(%rax), %rax
	movq	%rax, 16(%rbx)
	callq	emit_insn
	jmp	.LBB49_5
	.align	16, 0x90
.LBB49_4:                               # %if.else.i
                                        #   in Loop: Header=BB49_1 Depth=1
	callq	emit_insn
	movq	%rax, 16(%rbx)
	jmp	.LBB49_5
.LBB49_6:                               # %emit_queue.exit
	movq	%r14, 48(%rsp)          # 8-byte Spill
	cmpq	$24, %r15
	jg	.LBB49_8
# BB#7:                                 # %emit_queue.exit
	testq	%r15, %r15
	jne	.LBB49_133
	jmp	.LBB49_155
.LBB49_8:                               # %emit_queue.exit
	cmpq	$38, %r15
	jg	.LBB49_14
# BB#9:                                 # %emit_queue.exit
	cmpq	$25, %r15
	jne	.LBB49_133
# BB#10:                                # %sw.bb
	movq	%r13, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	48(%rsp), %rax          # 8-byte Reload
	jne	.LBB49_12
	jmp	.LBB49_11
.LBB49_14:                              # %emit_queue.exit
	addq	$-39, %r15
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %r15
	cmpq	$83, %r15
	ja	.LBB49_133
# BB#15:                                # %emit_queue.exit
	movq	$112, %rbx
	movq	$113, %r14
	movq	$97, %rax
	movq	$118, %r12
	jmpq	*.LJTI49_0(,%r15,8)
.LBB49_16:                              # %emit_queue.exit.sw.bb55_crit_edge
	movq	32(%r13), %rdi
	jmp	.LBB49_21
.LBB49_51:                              # %sw.bb162
	xorq	%rax, %rax
	movl	%eax, 60(%rsp)
	leaq	60(%rsp), %rax
	leaq	88(%rsp), %rsi
	leaq	80(%rsp), %rdx
	leaq	64(%rsp), %rcx
	leaq	72(%rsp), %r8
	leaq	76(%rsp), %r9
	movq	%rax, (%rsp)
	movq	%r13, %rdi
	callq	get_inner_reference
	movq	88(%rsp), %rbx
	movl	76(%rsp), %esi
	movq	%rbx, %rdi
	callq	type_for_size
	testq	%rax, %rax
	je	.LBB49_133
# BB#52:                                # %sw.bb162
	testq	%rbx, %rbx
	jns	.LBB49_30
	jmp	.LBB49_133
.LBB49_40:                              # %sw.bb154
	callq	push_temp_slots
	movq	32(%r13), %rdi
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	xorq	%rdi, %rdi
	callq	preserve_temp_slots
	callq	free_temp_slots
	callq	pop_temp_slots
	jmp	.LBB49_41
.LBB49_45:                              # %if.end.i1016
                                        #   in Loop: Header=BB49_41 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rbx), %rcx
	movq	16(%rax), %rax
	movq	%rcx, 40(%rax)
.LBB49_41:                              # %sw.bb154
                                        # =>This Inner Loop Header: Depth=1
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB49_46
# BB#42:                                # %while.body.i999
                                        #   in Loop: Header=BB49_41 Depth=1
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$24, %rax
	jne	.LBB49_44
# BB#43:                                # %if.then.i1005
                                        #   in Loop: Header=BB49_41 Depth=1
	movq	8(%rdi), %rax
	movq	8(%rax), %rax
	movq	%rax, 16(%rbx)
	callq	emit_insn
	jmp	.LBB49_45
.LBB49_44:                              # %if.else.i1008
                                        #   in Loop: Header=BB49_41 Depth=1
	callq	emit_insn
	movq	%rax, 16(%rbx)
	jmp	.LBB49_45
.LBB49_53:                              # %sw.bb200
	movq	40(%r13), %rdi
	movq	%rdx, %rbx
	callq	integer_onep
	movq	%rbx, %rcx
	testq	%rcx, %rax
	je	.LBB49_56
# BB#54:                                # %land.lhs.true206
	movq	48(%r13), %rdi
	movq	%rcx, %rbx
	callq	integer_zerop
	movq	%rbx, %rcx
	testq	%rcx, %rax
	jne	.LBB49_55
.LBB49_56:                              # %if.else
	movq	40(%r13), %rdi
	movq	%rcx, %rbx
	callq	integer_zerop
	movq	%rbx, %rcx
	testq	%rcx, %rax
	je	.LBB49_58
# BB#57:                                # %land.lhs.true221
	movq	48(%r13), %rdi
	movq	%rcx, %rbx
	callq	integer_onep
	testq	%rbx, %rax
	jne	.LBB49_78
.LBB49_58:                              # %if.else231
	callq	gen_label_rtx
	movq	%rax, %r14
	callq	gen_label_rtx
	movq	32(%r13), %rdi
	movq	%rax, %r12
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	do_jump
	callq	start_cleanup_deferral
	movq	40(%r13), %rdi
	movq	48(%rsp), %r15          # 8-byte Reload
	testq	%r15, %r15
	jne	.LBB49_60
# BB#59:                                # %if.else231
	movq	%r12, %r15
.LBB49_60:                              # %if.else231
	movq	40(%rsp), %rdx          # 8-byte Reload
	testq	%rdx, %rdx
	jne	.LBB49_62
# BB#61:                                # %if.else231
	movq	%r12, %rdx
.LBB49_62:                              # %if.else231
	movq	%r15, %rsi
	movq	%rdx, %rbx
	callq	do_jump
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB49_66
# BB#63:                                # %if.then.i1028
	movslq	(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB49_65
# BB#64:                                # %if.then3.i1034
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	adjust_stack
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
.LBB49_65:                              # %if.end.i1036
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.LBB49_66:                              # %do_pending_stack_adjust.exit1037
	movq	%r14, %rdi
	callq	emit_label
	movq	48(%r13), %rdi
	movq	%r15, %rsi
	jmp	.LBB49_67
.LBB49_126:                             # %sw.bb613
	movq	32(%r13), %rax
	movzbl	16(%rax), %ecx
	cmpq	$121, %rcx
	jne	.LBB49_133
# BB#127:                               # %if.then623
	movq	32(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$30, %rcx
	jne	.LBB49_133
# BB#128:                               # %land.lhs.true639
	movzbl	51(%rax), %ecx
	testq	$96, %rcx
	je	.LBB49_133
# BB#129:                               # %land.lhs.true645
	movl	56(%rax), %eax
	cmpq	$94, %rax
	jne	.LBB49_133
# BB#130:                               # %land.lhs.true645
	movq	40(%r13), %rax
	testq	%rax, %rax
	je	.LBB49_133
# BB#131:                               # %land.lhs.true652
	cmpq	$0, (%rax)
	je	.LBB49_133
# BB#132:                               # %if.then657
	movq	%r13, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	expand_builtin_expect_jump
	testq	%rax, %rax
	je	.LBB49_133
# BB#156:                               # %if.then661
	movq	%rax, %rdi
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_insn  # TAILCALL
.LBB49_154:                             # %sw.bb59
	movq	40(%r13), %rdi
	movq	placeholder_list(%rip), %rdx
	xorq	%rsi, %rsi
	callq	tree_cons
	movq	%rax, placeholder_list(%rip)
	movq	32(%r13), %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	do_jump
	movq	placeholder_list(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, placeholder_list(%rip)
	jmp	.LBB49_155
.LBB49_24:                              # %sw.bb68
	movq	8(%r13), %rsi
	movq	32(%r13), %rdx
	movq	40(%r13), %rcx
	movq	$102, %rdi
	xorq	%rax, %rax
	callq	build
	movq	$102, %rsi
	movq	$102, %rdx
	movq	%rax, %rdi
	jmp	.LBB49_85
.LBB49_25:                              # %sw.bb78
	movq	40(%r13), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB49_133
# BB#26:                                # %land.lhs.true
	movq	8(%r13), %rax
	movl	60(%rax), %eax
	andq	$511, %rax              # imm = 0x1FF
	cmpq	$64, %rax
	ja	.LBB49_133
# BB#27:                                # %land.lhs.true94
	movq	%rdx, %rbx
	callq	tree_floor_log2
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB49_133
# BB#28:                                # %land.lhs.true100
	incq	%rax
	movq	$1, %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	mode_for_size
	andq	%rax, %rbx
	cmpq	$51, %rbx
	je	.LBB49_133
# BB#29:                                # %land.lhs.true103
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	type_for_mode
	testq	%rax, %rax
	je	.LBB49_133
.LBB49_30:                              # %land.lhs.true106
	movq	8(%r13), %rcx
	movl	60(%rax), %edx
	movl	60(%rcx), %ecx
	movq	%rdx, %rsi
	andq	$511, %rsi              # imm = 0x1FF
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rcx, %rsi
	jae	.LBB49_133
# BB#31:                                # %land.lhs.true118
	movq	optab_table+328(%rip), %rsi
	movq	$5, %rcx
	shrq	%cl, %rdx
	andq	$2032, %rdx             # imm = 0x7F0
	movl	8(%rsi,%rdx), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	je	.LBB49_133
# BB#32:                                # %if.then125
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	convert
	movq	%rax, %rdi
	jmp	.LBB49_21
.LBB49_34:                              # %sw.bb132
	xorq	%r12, %r12
	movq	48(%rsp), %rbx          # 8-byte Reload
	testq	%rbx, %rbx
	jne	.LBB49_36
# BB#35:                                # %if.then134
	callq	gen_label_rtx
	movq	%rax, %r12
	movq	%r12, %rbx
.LBB49_36:                              # %if.end136
	movq	32(%r13), %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	do_jump
	callq	start_cleanup_deferral
	movq	40(%r13), %rdi
	movq	%rbx, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB49_68
.LBB49_37:                              # %sw.bb143
	xorq	%r12, %r12
	movq	40(%rsp), %rdx          # 8-byte Reload
	testq	%rdx, %rdx
	jne	.LBB49_39
# BB#38:                                # %if.then145
	callq	gen_label_rtx
	movq	%rax, %r12
	movq	%r12, %rdx
.LBB49_39:                              # %if.end147
	movq	32(%r13), %rdi
	xorq	%rsi, %rsi
	movq	%rdx, %rbx
	callq	do_jump
	callq	start_cleanup_deferral
	movq	40(%r13), %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
.LBB49_67:                              # %sw.epilog
	movq	%rbx, %rdx
.LBB49_68:                              # %sw.epilog
	callq	do_jump
	callq	end_cleanup_deferral
	testq	%r12, %r12
	je	.LBB49_155
# BB#69:                                # %if.then738
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB49_73
# BB#70:                                # %if.then.i1085
	movslq	(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB49_72
# BB#71:                                # %if.then3.i1091
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	adjust_stack
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
.LBB49_72:                              # %if.end.i1093
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.LBB49_73:                              # %do_pending_stack_adjust.exit1094
	movq	%r12, %rdi
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_label  # TAILCALL
.LBB49_94:                              # %sw.bb459
	movq	%rdx, %rbx
	movq	32(%r13), %rax
	movq	8(%rax), %rax
	movl	60(%rax), %esi
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	movl	mode_class(,%rsi,4), %eax
	cmpq	$1, %rax
	jne	.LBB49_99
# BB#95:                                # %land.lhs.true474
	movq	$107, %rdi
	xorq	%rdx, %rdx
	callq	can_compare_p
	testq	%rbx, %rax
	je	.LBB49_96
.LBB49_99:                              # %if.else478
	movq	$107, %rsi
	movq	$111, %rdx
	jmp	.LBB49_84
.LBB49_100:                             # %sw.bb480
	movq	%rdx, %rbx
	movq	32(%r13), %rax
	movq	8(%rax), %rax
	movl	60(%rax), %esi
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	movl	mode_class(,%rsi,4), %eax
	cmpq	$1, %rax
	jne	.LBB49_104
# BB#101:                               # %land.lhs.true495
	movq	$106, %rdi
	xorq	%rdx, %rdx
	callq	can_compare_p
	testq	%rbx, %rax
	je	.LBB49_102
.LBB49_104:                             # %if.else499
	movq	$106, %rsi
	movq	$110, %rdx
	jmp	.LBB49_84
.LBB49_105:                             # %sw.bb501
	movq	%rdx, %rbx
	movq	32(%r13), %rax
	movq	8(%rax), %rax
	movl	60(%rax), %esi
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	movl	mode_class(,%rsi,4), %eax
	cmpq	$1, %rax
	jne	.LBB49_108
# BB#106:                               # %land.lhs.true516
	movq	$105, %rdi
	xorq	%rdx, %rdx
	callq	can_compare_p
	testq	%rbx, %rax
	je	.LBB49_107
.LBB49_108:                             # %if.else520
	movq	$105, %rsi
	movq	$109, %rdx
	jmp	.LBB49_84
.LBB49_109:                             # %sw.bb522
	movq	%rdx, %rbx
	movq	32(%r13), %rax
	movq	8(%rax), %rax
	movl	60(%rax), %esi
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	movl	mode_class(,%rsi,4), %eax
	cmpq	$1, %rax
	jne	.LBB49_112
# BB#110:                               # %land.lhs.true537
	movq	$104, %rdi
	xorq	%rdx, %rdx
	callq	can_compare_p
	testq	%rbx, %rax
	je	.LBB49_111
.LBB49_112:                             # %if.else541
	movq	$104, %rsi
	movq	$108, %rdx
.LBB49_84:                              # %if.else357
	movq	%r13, %rdi
	jmp	.LBB49_85
.LBB49_74:                              # %sw.bb266
	movq	32(%r13), %rdi
	movq	8(%rdi), %r14
	movl	60(%r14), %eax
	movq	$7, %rcx
	shrq	%cl, %rax
	andq	$508, %rax              # imm = 0x1FC
	movl	mode_class(%rax), %eax
	addq	$-5, %rax
	andq	%rdx, %rax
	cmpq	$1, %rax
	ja	.LBB49_77
# BB#75:                                # %if.then291
	callq	save_expr
	movq	40(%r13), %rdi
	movq	%rax, %rbx
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	callq	save_expr
	movq	8(%r13), %r12
	movq	8(%r14), %rsi
	movq	%rax, %r15
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movq	$127, %rdi
	movq	%rbx, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	8(%r14), %rsi
	movq	%rax, %rbx
	movq	$127, %rdi
	movq	%r15, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	movq	$101, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	8(%r13), %r13
	movq	8(%r14), %rsi
	movq	%rax, %r15
	movq	$128, %rdi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	8(%r14), %rsi
	movq	%rax, %rbx
	movq	$128, %rdi
	movq	32(%rsp), %rdx          # 8-byte Reload
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	movq	$101, %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	movq	$91, %rdi
	jmp	.LBB49_76
.LBB49_87:                              # %sw.bb361
	movq	32(%r13), %rdi
	movq	8(%rdi), %r14
	movl	60(%r14), %eax
	movq	$7, %rcx
	shrq	%cl, %rax
	andq	$508, %rax              # imm = 0x1FC
	movl	mode_class(%rax), %eax
	addq	$-5, %rax
	andq	%rdx, %rax
	cmpq	$1, %rax
	ja	.LBB49_89
# BB#88:                                # %if.then387
	callq	save_expr
	movq	40(%r13), %rdi
	movq	%rax, %rbx
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	callq	save_expr
	movq	8(%r13), %r12
	movq	8(%r14), %rsi
	movq	%rax, %r15
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movq	$127, %rdi
	movq	%rbx, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	8(%r14), %rsi
	movq	%rax, %rbx
	movq	$127, %rdi
	movq	%r15, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	movq	$102, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	8(%r13), %r13
	movq	8(%r14), %rsi
	movq	%rax, %r15
	movq	$128, %rdi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	8(%r14), %rsi
	movq	%rax, %rbx
	movq	$128, %rdi
	movq	32(%rsp), %rdx          # 8-byte Reload
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	movq	$102, %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	movq	$92, %rdi
.LBB49_76:                              # %if.then291
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rdi
	jmp	.LBB49_21
.LBB49_113:                             # %if.else548
	movq	$113, %rbx
	movq	$112, %r14
.LBB49_114:                             # %if.end549
	movq	32(%r13), %rax
	movq	8(%rax), %rax
	movl	60(%rax), %r12d
	movq	$9, %rcx
	shrq	%cl, %r12
	andq	$127, %r12
	movq	%rdx, %r15
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	can_compare_p
	movq	%r15, %rcx
	testq	%rcx, %rax
	jne	.LBB49_117
# BB#115:                               # %land.lhs.true562
	movq	%rcx, %r15
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	can_compare_p
	movq	%r15, %rcx
	testq	%rcx, %rax
	jne	.LBB49_118
# BB#116:                               # %land.lhs.true562
	andq	%r14, %rcx
	cmpq	$112, %rcx
	je	.LBB49_118
.LBB49_117:                             # %if.then571
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	%rbx, %rdx
	jmp	.LBB49_85
.LBB49_119:                             # %sw.bb575
	movq	$98, %rax
	movq	$117, %r12
	jmp	.LBB49_123
.LBB49_120:                             # %sw.bb576
	movq	$99, %rax
	movq	$116, %r12
	jmp	.LBB49_123
.LBB49_121:                             # %sw.bb577
	movq	$100, %rax
	movq	$115, %r12
	jmp	.LBB49_123
.LBB49_122:                             # %sw.bb578
	movq	$101, %rax
	movq	$114, %r12
.LBB49_123:                             # %unordered_bcc
	movq	%rax, %r15
	movq	32(%r13), %rax
	movq	8(%rax), %rax
	movl	60(%rax), %esi
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	movq	%rdx, %rbx
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	can_compare_p
	testq	%rbx, %rax
	je	.LBB49_125
# BB#124:                               # %if.then591
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%r12, %rdx
.LBB49_85:                              # %if.else357
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	40(%rsp), %r8           # 8-byte Reload
.LBB49_86:                              # %if.else357
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	do_compare_and_jump  # TAILCALL
.LBB49_125:                             # %if.else592
	movq	32(%r13), %rdi
	callq	save_expr
	movq	40(%r13), %rdi
	movq	%rax, %r14
	callq	save_expr
	movq	8(%r13), %rsi
	movq	%rax, %rbx
	movq	$103, %rdi
	xorq	%rax, %rax
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	8(%r13), %rsi
	movq	%rax, %r12
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	8(%r13), %rsi
	movq	%rax, %rcx
	movq	$92, %rdi
	xorq	%rax, %rax
	movq	%r12, %rdx
	callq	build
	movq	%rax, %rdi
	jmp	.LBB49_21
.LBB49_17:                              # %emit_queue.exit.sw.bb37_crit_edge
	movq	32(%r13), %rdi
	jmp	.LBB49_20
.LBB49_18:                              # %sw.bb3
	movq	32(%r13), %rdi
	movzbl	16(%rdi), %ecx
	cmpq	$44, %rcx
	ja	.LBB49_20
# BB#19:                                # %sw.bb3
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$28037546508288, %rcx   # imm = 0x198000000000
	testq	%rcx, %rax
	jne	.LBB49_133
.LBB49_20:                              # %sw.bb37
	movq	8(%r13), %rax
	movq	8(%rdi), %rcx
	movl	60(%rax), %eax
	movl	60(%rcx), %ecx
	andq	$511, %rax              # imm = 0x1FF
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rcx, %rax
	jae	.LBB49_21
.LBB49_133:                             # %normal
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	callq	expand_expr
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rcx
	movl	4(%rcx), %edx
	movq	%rax, %r14
	testq	%rdx, %rdx
	jne	.LBB49_137
# BB#134:                               # %if.then.i1043
	movslq	(%rcx), %rsi
	testq	%rsi, %rsi
	je	.LBB49_136
# BB#135:                               # %if.then3.i1049
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	adjust_stack
	movq	cfun(%rip), %rax
	movq	16(%rax), %rcx
.LBB49_136:                             # %if.end.i1051
	xorq	%rax, %rax
	movl	%eax, (%rcx)
.LBB49_137:                             # %do_pending_stack_adjust.exit1052
	movq	40(%rcx), %rbx
	jmp	.LBB49_138
	.align	16, 0x90
.LBB49_142:                             # %if.end.i1078
                                        #   in Loop: Header=BB49_138 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rbx), %rcx
	movq	16(%rax), %rax
	movq	%rcx, 40(%rax)
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rbx
.LBB49_138:                             # %do_pending_stack_adjust.exit1052
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB49_143
# BB#139:                               # %while.body.i1061
                                        #   in Loop: Header=BB49_138 Depth=1
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$24, %rax
	jne	.LBB49_141
# BB#140:                               # %if.then.i1067
                                        #   in Loop: Header=BB49_138 Depth=1
	movq	8(%rdi), %rax
	movq	8(%rax), %rax
	movq	%rax, 16(%rbx)
	callq	emit_insn
	jmp	.LBB49_142
	.align	16, 0x90
.LBB49_141:                             # %if.else.i1070
                                        #   in Loop: Header=BB49_138 Depth=1
	callq	emit_insn
	movq	%rax, 16(%rbx)
	jmp	.LBB49_142
.LBB49_143:                             # %emit_queue.exit1079
	movl	(%r14), %r8d
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$54, %rax
	je	.LBB49_147
# BB#144:                               # %emit_queue.exit1079
	cmpq	$67, %rax
	je	.LBB49_147
# BB#145:                               # %emit_queue.exit1079
	cmpq	$55, %rax
	jne	.LBB49_148
# BB#146:                               # %land.lhs.true676
	testq	$16711680, %r8          # imm = 0xFF0000
	je	.LBB49_147
.LBB49_148:                             # %if.else697
	movq	$16, %rcx
	movq	%r8, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	movl	mode_class(,%rsi,4), %eax
	cmpq	$1, %rax
	jne	.LBB49_151
# BB#149:                               # %land.lhs.true705
	movq	$102, %rdi
	xorq	%rdx, %rdx
	callq	can_compare_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB49_157
# BB#150:                               # %land.lhs.true705.if.else712_crit_edge
	movl	(%r14), %r8d
.LBB49_151:                             # %if.else712
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	je	.LBB49_153
# BB#152:                               # %if.then718
	movq	8(%r13), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	16(%rax), %eax
	andq	%r8, %rcx
	movq	const_tiny_rtx(,%rcx,8), %rsi
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 8(%rsp)
	movq	$102, %rdx
	xorq	%r9, %r9
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, (%rsp)
	movq	%r14, %rdi
	movq	%rax, %rcx
	callq	do_compare_rtx_and_jump
	jmp	.LBB49_155
.LBB49_147:                             # %if.then687
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%rax, %r14
	movq	48(%rsp), %rax          # 8-byte Reload
	je	.LBB49_12
.LBB49_11:                              # %sw.bb
	movq	40(%rsp), %rax          # 8-byte Reload
.LBB49_12:                              # %sw.bb
	testq	%rax, %rax
	movq	%rax, %rdi
	je	.LBB49_155
# BB#13:                                # %if.then
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_jump  # TAILCALL
.LBB49_155:                             # %if.end740
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB49_157:                             # %if.then711
	movq	%r14, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	do_jump_by_parts_equality_rtx  # TAILCALL
.LBB49_46:                              # %emit_queue.exit1017
	movl	4(%rax), %ecx
	testq	%rcx, %rcx
	movq	48(%rsp), %rbx          # 8-byte Reload
	jne	.LBB49_50
# BB#47:                                # %if.then.i1020
	movslq	(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB49_49
# BB#48:                                # %if.then3.i
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	adjust_stack
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
.LBB49_49:                              # %if.end.i1022
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.LBB49_50:                              # %do_pending_stack_adjust.exit
	movq	40(%r13), %rdi
	movq	%rbx, %rsi
	jmp	.LBB49_22
.LBB49_77:                              # %if.else328
	movq	40(%r13), %rdi
	movq	%rdx, %rbx
	callq	integer_zerop
	movq	%rbx, %rcx
	testq	%rcx, %rax
	je	.LBB49_79
.LBB49_78:                              # %sw.bb128
	movq	32(%r13), %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB49_23
.LBB49_89:                              # %if.else426
	movq	40(%r13), %rdi
	movq	%rdx, %rbx
	callq	integer_zerop
	movq	%rbx, %rcx
	testq	%rcx, %rax
	je	.LBB49_90
.LBB49_55:                              # %if.then212
	movq	32(%r13), %rdi
.LBB49_21:                              # %sw.bb55
	movq	48(%rsp), %rsi          # 8-byte Reload
.LBB49_22:                              # %sw.bb55
	movq	40(%rsp), %rdx          # 8-byte Reload
.LBB49_23:                              # %sw.bb55
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	do_jump  # TAILCALL
.LBB49_118:                             # %if.else572
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%r14, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	48(%rsp), %r8           # 8-byte Reload
	jmp	.LBB49_86
.LBB49_96:                              # %if.then477
	movq	40(%r13), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	32(%r13), %rdi
	jmp	.LBB49_97
.LBB49_102:                             # %if.then498
	movq	32(%r13), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	40(%r13), %rdi
	jmp	.LBB49_103
.LBB49_107:                             # %if.then519
	movq	32(%r13), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	40(%r13), %rdi
.LBB49_97:                              # %if.then477
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	32(%r13), %rcx
	movq	8(%rcx), %rdx
	movl	60(%rdx), %edi
	movq	$9, %rcx
	movl	16(%rdx), %esi
	shrq	%cl, %rdi
	andq	$127, %rdi
	movq	$13, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	40(%rsp), %r9           # 8-byte Reload
	jmp	.LBB49_98
.LBB49_111:                             # %if.then540
	movq	40(%r13), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	32(%r13), %rdi
.LBB49_103:                             # %if.then498
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	32(%r13), %rcx
	movq	8(%rcx), %rdx
	movl	60(%rdx), %edi
	movq	$9, %rcx
	movl	16(%rdx), %esi
	shrq	%cl, %rdi
	andq	$127, %rdi
	movq	$13, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	48(%rsp), %r9           # 8-byte Reload
.LBB49_98:                              # %if.then477
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	do_jump_by_parts_greater_rtx  # TAILCALL
.LBB49_79:                              # %if.else338
	movq	%rcx, %rbx
	movl	60(%r14), %esi
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	movl	mode_class(,%rsi,4), %eax
	cmpq	$1, %rax
	jne	.LBB49_83
# BB#80:                                # %land.lhs.true348
	movq	$103, %rdi
	xorq	%rdx, %rdx
	callq	can_compare_p
	testq	%rbx, %rax
	je	.LBB49_81
.LBB49_83:                              # %if.else357
	movq	$103, %rsi
	movq	$103, %rdx
	jmp	.LBB49_84
.LBB49_90:                              # %if.else436
	movq	%rcx, %rbx
	movl	60(%r14), %esi
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	movl	mode_class(,%rsi,4), %eax
	cmpq	$1, %rax
	jne	.LBB49_93
# BB#91:                                # %land.lhs.true446
	movq	$102, %rdi
	xorq	%rdx, %rdx
	callq	can_compare_p
	testq	%rbx, %rax
	je	.LBB49_92
.LBB49_93:                              # %if.else455
	movq	$102, %rsi
	movq	$102, %rdx
	jmp	.LBB49_84
.LBB49_81:                              # %if.then356
	movq	%r13, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	40(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB49_82
.LBB49_92:                              # %if.then454
	movq	%r13, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
.LBB49_82:                              # %if.then356
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	do_jump_by_parts_equality  # TAILCALL
.LBB49_153:                             # %if.else733
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.do_jump, %rcx
	movq	$9720, %rsi             # imm = 0x25F8
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp662:
	.size	do_jump, .Ltmp662-do_jump
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI49_0:
	.quad	.LBB49_51
	.quad	.LBB49_51
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_51
	.quad	.LBB49_51
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_40
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_53
	.quad	.LBB49_133
	.quad	.LBB49_126
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_154
	.quad	.LBB49_133
	.quad	.LBB49_24
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_16
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_16
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_16
	.quad	.LBB49_16
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_25
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_34
	.quad	.LBB49_37
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_78
	.quad	.LBB49_94
	.quad	.LBB49_100
	.quad	.LBB49_105
	.quad	.LBB49_109
	.quad	.LBB49_74
	.quad	.LBB49_87
	.quad	.LBB49_114
	.quad	.LBB49_113
	.quad	.LBB49_123
	.quad	.LBB49_119
	.quad	.LBB49_120
	.quad	.LBB49_121
	.quad	.LBB49_122
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_17
	.quad	.LBB49_18
	.quad	.LBB49_16
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_133
	.quad	.LBB49_16

	.text
	.align	16, 0x90
	.type	expand_increment,@function
expand_increment:                       # @expand_increment
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp669:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp670:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp671:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp672:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp673:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp674:
	.cfi_def_cfa_offset 144
.Ltmp675:
	.cfi_offset %rbx, -48
.Ltmp676:
	.cfi_offset %r12, -40
.Ltmp677:
	.cfi_offset %r13, -32
.Ltmp678:
	.cfi_offset %r14, -24
.Ltmp679:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	8(%r12), %rax
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	%rsi, %r13
	movq	32(%r12), %rbx
	movl	60(%rax), %r14d
	movq	$9, %rcx
	shrq	%cl, %r14
	movq	optab_table(%rip), %rdx
	andq	$127, %r14
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %r13
	je	.LBB50_7
# BB#1:                                 # %lor.lhs.false
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$40, %rcx
	je	.LBB50_7
# BB#2:                                 # %lor.lhs.false
	cmpq	$39, %rcx
	jne	.LBB50_3
# BB#4:                                 # %land.lhs.true
	movq	32(%rbx), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$41, %rcx
	jne	.LBB50_7
# BB#5:                                 # %lor.lhs.false21
	movq	40(%rbx), %rcx
	movzbl	49(%rcx), %ecx
	testq	$16, %rcx
	je	.LBB50_6
.LBB50_7:                               # %if.then
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	callq	stabilize_reference
	movq	%rax, %rbx
	movl	16(%rbx), %eax
.LBB50_8:                               # %if.end
	movq	%r12, 88(%rsp)          # 8-byte Spill
	andq	$255, %rax
	addq	$-129, %rax
	andq	%r15, %rax
	cmpq	$1, %rax
	ja	.LBB50_9
# BB#10:                                # %if.then40
	movq	%r14, 56(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	callq	save_expr
	movq	%rax, %rbx
	jmp	.LBB50_11
.LBB50_9:
	movq	%r14, 56(%rsp)          # 8-byte Spill
.LBB50_11:                              # %if.end42
	callq	get_last_insn
	movq	%rax, %r14
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movq	%rbx, %rdi
	callq	expand_expr
	movq	%rax, %r12
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$268500991, %rcx        # imm = 0x1000FFFF
	cmpq	$268435519, %rcx        # imm = 0x1000003F
	jne	.LBB50_15
# BB#12:                                # %if.then53
	testq	%r15, %r13
	je	.LBB50_13
# BB#14:                                # %if.then55
	movq	8(%r12), %rdi
	callq	copy_to_reg
	movq	%rax, 8(%r12)
	jmp	.LBB50_24
.LBB50_15:                              # %if.else62
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB50_24
# BB#16:                                # %land.lhs.true66
	movq	$15, %rcx
	shrq	%cl, %rax
	andq	$510, %rax              # imm = 0x1FE
	movl	target_flags(%rip), %edx
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movzbl	mode_bitsize(%rax), %eax
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rdx, %rcx
	testq	%r15, %r13
	jne	.LBB50_17
# BB#18:                                # %land.lhs.true66
	movabsq	$1, %rdx
	jmp	.LBB50_19
.LBB50_13:
	movq	$1, 40(%rsp)            # 8-byte Folded Spill
	jmp	.LBB50_24
.LBB50_3:
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	jmp	.LBB50_8
.LBB50_17:
	movabsq	$0, %rdx
.LBB50_19:                              # %land.lhs.true66
	cmpq	%rcx, %rax
	sbbq	%rsi, %rsi
	andq	$1, %rsi
	cmpq	%rcx, %rax
	jae	.LBB50_20
# BB#21:                                # %land.lhs.true66
	testq	%rdx, %rdx
	jne	.LBB50_22
# BB#23:                                # %if.then76
	movq	%r12, %rdi
	callq	copy_to_reg
	movq	%rax, %r12
	jmp	.LBB50_24
.LBB50_20:
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	jmp	.LBB50_24
.LBB50_22:
	movq	%rsi, 40(%rsp)          # 8-byte Spill
.LBB50_24:                              # %if.end81
	movl	(%r12), %eax
	orq	$2, %rax
	xorq	%rcx, %rcx
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB50_28
# BB#25:                                # %land.rhs
	callq	get_last_insn
	cmpq	%rax, %r14
	je	.LBB50_26
# BB#27:                                # %land.rhs
	movabsq	$1, %rcx
	jmp	.LBB50_28
.LBB50_26:
	movabsq	$0, %rcx
.LBB50_28:                              # %land.end
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	88(%rsp), %r14          # 8-byte Reload
	movq	40(%r14), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movl	16(%r14), %ecx
	movq	%rax, %rsi
	orq	$2, %rcx
	andq	$255, %rcx
	cmpq	$131, %rcx
	jne	.LBB50_30
# BB#29:                                # %if.end112.thread
	movq	optab_table+16(%rip), %rax
.LBB50_31:                              # %land.lhs.true115
	movzbl	(%rsi), %ecx
	cmpq	$54, %rcx
	jne	.LBB50_32
# BB#33:                                # %if.then120
	movq	8(%rsi), %rsi
	negq	%rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	optab_table(%rip), %rdx
	movq	%rax, %rsi
	jmp	.LBB50_34
.LBB50_32:
	movq	%rax, %rdx
	jmp	.LBB50_34
.LBB50_30:                              # %if.end112
	movq	optab_table+16(%rip), %rax
	movq	48(%rsp), %rdx          # 8-byte Reload
	cmpq	%rax, %rdx
	je	.LBB50_31
.LBB50_34:                              # %if.end124
	movl	flag_trapv(%rip), %eax
	testq	%rax, %rax
	je	.LBB50_35
# BB#36:                                # %land.lhs.true126
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movzbl	17(%rax), %eax
	testq	$32, %rax
	je	.LBB50_38
# BB#37:
	movq	%r12, 80(%rsp)          # 8-byte Spill
	jmp	.LBB50_41
.LBB50_35:
	movq	%r12, 80(%rsp)          # 8-byte Spill
	jmp	.LBB50_41
.LBB50_38:                              # %if.then134
	movq	optab_table(%rip), %rax
	cmpq	%rax, %rdx
	je	.LBB50_39
# BB#40:                                # %select.mid
	movq	%r12, 80(%rsp)          # 8-byte Spill
	movq	optab_table+24(%rip), %rdx
	jmp	.LBB50_41
.LBB50_39:
	movq	%r12, 80(%rsp)          # 8-byte Spill
	movq	optab_table+8(%rip), %rdx
.LBB50_41:                              # %if.end138
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	xorq	%r14, %r14
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %r13
	jne	.LBB50_51
# BB#42:                                # %if.then140
	movq	%r13, 24(%rsp)          # 8-byte Spill
	movq	56(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rax
	andq	%r12, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movslq	8(%rdx,%rax), %rax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB50_44
# BB#43:
	movq	24(%rsp), %r13          # 8-byte Reload
	jmp	.LBB50_51
.LBB50_44:                              # %land.lhs.true145
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	leaq	(%rax,%rax,4), %rax
	movq	insn_data+24(,%rax,8), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	80(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	*(%rax)
	testq	%r12, %rax
	je	.LBB50_45
# BB#46:                                # %land.lhs.true151
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	16(%rsp), %rax          # 8-byte Reload
	callq	*24(%rax)
	testq	%r12, %rax
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	je	.LBB50_51
# BB#47:                                # %land.lhs.true159
	movq	%rdx, %r15
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	16(%rsp), %rax          # 8-byte Reload
	callq	*48(%rax)
	testq	%r12, %rax
	je	.LBB50_48
# BB#49:                                # %land.lhs.true159
	movabsq	$1, %r14
	jmp	.LBB50_50
.LBB50_45:
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB50_51
.LBB50_48:
	movabsq	$0, %r14
.LBB50_50:                              # %land.lhs.true159
	movq	%r15, %rdx
.LBB50_51:                              # %if.end169
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	movq	%r12, %r15
	jne	.LBB50_54
# BB#52:                                # %lor.lhs.false171
	orq	%r13, %r14
	testq	%r15, %r14
	je	.LBB50_54
# BB#53:                                # %lor.lhs.false171
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	jne	.LBB50_54
# BB#69:                                # %if.end228
	testq	%r15, %r13
	je	.LBB50_87
# BB#70:                                # %if.then230
	movq	56(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rax
	andq	%r15, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movslq	8(%rdx,%rax), %rax
	movq	%rdx, %r13
	cmpq	$1317, %rax             # imm = 0x525
	movq	%r15, %rbx
	movq	80(%rsp), %r14          # 8-byte Reload
	je	.LBB50_86
# BB#71:                                # %land.lhs.true237
	leaq	(%rax,%rax,4), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	insn_data+24(,%rax,8), %r15
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	*(%r15)
	testq	%rbx, %rax
	je	.LBB50_78
# BB#72:                                # %land.lhs.true245
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	*24(%r15)
	testq	%rbx, %rax
	je	.LBB50_78
# BB#73:                                # %if.then253
	movq	32(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	*48(%r15)
	testq	%rbx, %rax
	je	.LBB50_75
# BB#74:
	movq	%r13, %rdx
	jmp	.LBB50_76
.LBB50_54:                              # %if.then177
	movq	88(%rsp), %rcx          # 8-byte Reload
	movzbl	16(%rcx), %eax
	cmpq	$131, %rax
	movq	$60, %rdi
	je	.LBB50_59
# BB#55:                                # %lor.rhs
	cmpq	$129, %rax
	jne	.LBB50_56
# BB#57:                                # %lor.rhs
	movabsq	$1, %rdi
	jmp	.LBB50_58
.LBB50_56:
	movabsq	$0, %rdi
.LBB50_58:                              # %lor.rhs
	addq	$59, %rdi
.LBB50_59:                              # %lor.end
	movq	8(%rcx), %rsi
	movq	40(%rcx), %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	build
	movl	16(%rbx), %ecx
	andq	$254, %rcx
	cmpq	$114, %rcx
	jne	.LBB50_60
# BB#61:
	movq	80(%rsp), %r14          # 8-byte Reload
	.align	16, 0x90
.LBB50_62:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rdi
	movq	%rax, %rsi
	callq	convert
	movq	32(%rbx), %rbx
	movl	16(%rbx), %ecx
	andq	$254, %rcx
	cmpq	$114, %rcx
	je	.LBB50_62
	jmp	.LBB50_63
.LBB50_60:
	movq	80(%rsp), %r14          # 8-byte Reload
.LBB50_63:                              # %while.end
	movq	64(%rsp), %rcx          # 8-byte Reload
	orq	%r13, %rcx
	testq	%r15, %rcx
	jne	.LBB50_64
# BB#65:                                # %while.end
	movabsq	$1, %rdx
	jmp	.LBB50_66
.LBB50_64:
	movabsq	$0, %rdx
.LBB50_66:                              # %while.end
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	expand_assignment
	testq	%r15, %r13
	jne	.LBB50_68
# BB#67:                                # %while.end
	movq	%rax, %r14
.LBB50_68:                              # %while.end
	movq	%r14, %rbx
.LBB50_90:                              # %return
	movq	%rbx, %rax
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB50_87:                              # %if.else325
	movq	%rdx, %r13
	movq	80(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%r14, %rdx
	movq	56(%rsp), %r12          # 8-byte Reload
	jmp	.LBB50_88
.LBB50_78:                              # %land.lhs.true271
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	jne	.LBB50_86
# BB#79:                                # %if.then276
	movq	8(%r14), %rdi
	movq	%r12, %rsi
	callq	general_operand
	testq	%rbx, %rax
	je	.LBB50_81
# BB#80:                                # %cond.true282
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	8(%r14), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	force_reg
	jmp	.LBB50_82
.LBB50_86:                              # %if.then323
	movq	%r14, %rdi
	callq	copy_to_reg
	movq	%rax, %rdx
	movq	%rdx, %rbx
.LBB50_88:                              # %if.end327
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movl	16(%rax), %r9d
	movq	$13, %rcx
	shrq	%cl, %r9
	andq	$1, %r9
	movq	$3, (%rsp)
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	%r14, %r8
	callq	expand_binop
	cmpq	%r14, %rax
	je	.LBB50_90
# BB#89:                                # %if.then338
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	jmp	.LBB50_90
.LBB50_6:
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	jmp	.LBB50_8
.LBB50_75:                              # %if.then261
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	force_reg
	movq	%rax, %rdx
.LBB50_76:                              # %if.end263
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r14, %rsi
	jmp	.LBB50_77
.LBB50_81:                              # %cond.false290
	movq	8(%r14), %rdi
	callq	copy_to_reg
.LBB50_82:                              # %cond.end295
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	replace_equiv_address
	movq	%rax, %r12
	movzbl	2(%r12), %edi
	movq	%r12, %rsi
	callq	force_reg
	movq	%rax, %r14
	movq	32(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdi
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	*48(%r15)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB50_84
# BB#83:
	movq	%r13, %rbx
	jmp	.LBB50_85
.LBB50_84:                              # %if.then310
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	force_reg
	movq	%rax, %rbx
.LBB50_85:                              # %if.end312
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	gen_move_insn
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	40(%rcx), %rdx
	movzbl	2(%r12), %esi
	movq	$71, %rdi
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%rdx, (%rsp)
	movq	%r12, %rdx
	movq	%rax, %r9
	callq	gen_rtx_fmt_eeeee
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	%rax, 40(%rcx)
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
.LBB50_77:                              # %return
	movq	72(%rsp), %rcx          # 8-byte Reload
	callq	*insn_data+16(,%rcx,8)
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	40(%rcx), %rdx
	movzbl	2(%r14), %esi
	movq	$71, %rdi
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%rdx, (%rsp)
	movq	%r14, %rdx
	movq	%rax, %r9
	callq	gen_rtx_fmt_eeeee
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	%rax, 40(%rcx)
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rbx
	jmp	.LBB50_90
.Ltmp680:
	.size	expand_increment, .Ltmp680-expand_increment
	.cfi_endproc

	.globl	init_pending_stack_adjust
	.align	16, 0x90
	.type	init_pending_stack_adjust,@function
init_pending_stack_adjust:              # @init_pending_stack_adjust
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	retq
.Ltmp681:
	.size	init_pending_stack_adjust, .Ltmp681-init_pending_stack_adjust
	.cfi_endproc

	.globl	clear_pending_stack_adjust
	.align	16, 0x90
	.type	clear_pending_stack_adjust,@function
clear_pending_stack_adjust:             # @clear_pending_stack_adjust
	.cfi_startproc
# BB#0:                                 # %entry
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	jle	.LBB52_9
# BB#1:                                 # %land.lhs.true
	movl	flag_omit_frame_pointer(%rip), %eax
	testq	%rax, %rax
	je	.LBB52_3
# BB#2:                                 # %lor.lhs.false
	movq	cfun(%rip), %rax
	movzbl	424(%rax), %eax
	testq	$64, %rax
	je	.LBB52_9
.LBB52_3:                               # %land.lhs.true2
	movq	current_function_decl(%rip), %rax
	movzbl	49(%rax), %eax
	movl	flag_no_inline(%rip), %edx
	andq	$8, %rax
	movq	$3, %rcx
	shrq	%cl, %rax
	testq	%rdx, %rdx
	jne	.LBB52_4
# BB#5:                                 # %land.lhs.true2
	movabsq	$1, %rcx
	jmp	.LBB52_6
.LBB52_4:
	movabsq	$0, %rcx
.LBB52_6:                               # %land.lhs.true2
	testq	%rax, %rcx
	jne	.LBB52_9
# BB#7:                                 # %land.lhs.true2
	movl	flag_inline_functions(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB52_9
# BB#8:                                 # %if.then
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	(%rax), %ecx
	movl	8(%rax), %edx
	subq	%rcx, %rdx
	movl	%edx, 8(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.LBB52_9:                               # %if.end
	retq
.Ltmp682:
	.size	clear_pending_stack_adjust, .Ltmp682-clear_pending_stack_adjust
	.cfi_endproc

	.align	16, 0x90
	.type	do_compare_and_jump,@function
do_compare_and_jump:                    # @do_compare_and_jump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp689:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp690:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp691:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp692:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp693:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp694:
	.cfi_def_cfa_offset 112
.Ltmp695:
	.cfi_offset %rbx, -48
.Ltmp696:
	.cfi_offset %r12, -40
.Ltmp697:
	.cfi_offset %r13, -32
.Ltmp698:
	.cfi_offset %r14, -24
.Ltmp699:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movq	32(%r13), %rdi
	movq	%r8, %r14
	movq	%rcx, %rbx
	movq	%rdx, %r12
	movq	%rsi, %r15
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	32(%r13), %rcx
	movzbl	16(%rcx), %ecx
	movq	%rax, 56(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	je	.LBB53_18
# BB#1:                                 # %if.end
	movq	40(%r13), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	40(%r13), %rdx
	movzbl	16(%rdx), %esi
	testq	%rsi, %rsi
	je	.LBB53_18
# BB#2:                                 # %if.end19
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%r14, 32(%rsp)          # 8-byte Spill
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	32(%r13), %rcx
	movq	8(%rcx), %rax
	movl	60(%rax), %r14d
	movzbl	16(%rcx), %edi
	movq	$9, %rcx
	shrq	%cl, %r14
	andq	$127, %r14
	cmpq	$25, %rdi
	jne	.LBB53_3
# BB#4:                                 # %land.lhs.true
	movq	%r12, 48(%rsp)          # 8-byte Spill
	cmpq	$25, %rsi
	jne	.LBB53_5
# BB#6:                                 # %lor.lhs.false
	movq	8(%rdx), %rdx
	movl	60(%rdx), %ebx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%r14, %rcx
	movzbl	mode_bitsize(%rcx,%rcx), %edi
	movq	$8, %rcx
	movq	%rbx, %rsi
	shrq	%cl, %rsi
	andq	$254, %rsi
	movzbl	mode_bitsize(%rsi), %ecx
	cmpq	%rcx, %rdi
	jbe	.LBB53_8
	jmp	.LBB53_7
.LBB53_3:
	movq	%r12, 48(%rsp)          # 8-byte Spill
	jmp	.LBB53_8
.LBB53_5:                               # %land.lhs.true.if.then61_crit_edge
	movq	8(%rdx), %rdx
	movl	60(%rdx), %ebx
.LBB53_7:                               # %if.then61
	movq	$9, %rcx
	shrq	%cl, %rbx
	andq	$127, %rbx
	movq	%rbx, %r14
	movq	%rdx, %rax
.LBB53_8:                               # %if.end72
	movl	16(%rax), %r12d
	movq	$13, %rcx
	shrq	%cl, %r12
	andq	$1, %r12
	jne	.LBB53_10
# BB#9:                                 # %if.end72
	movq	%r15, 48(%rsp)          # 8-byte Spill
	jmp	.LBB53_10
	.align	16, 0x90
.LBB53_14:                              # %if.end.i
                                        #   in Loop: Header=BB53_10 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rbx), %rcx
	movq	16(%rax), %rax
	movq	%rcx, 40(%rax)
.LBB53_10:                              # %if.end72
                                        # =>This Inner Loop Header: Depth=1
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB53_15
# BB#11:                                # %while.body.i
                                        #   in Loop: Header=BB53_10 Depth=1
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$24, %rax
	jne	.LBB53_13
# BB#12:                                # %if.then.i
                                        #   in Loop: Header=BB53_10 Depth=1
	movq	8(%rdi), %rax
	movq	8(%rax), %rax
	movq	%rax, 16(%rbx)
	callq	emit_insn
	jmp	.LBB53_14
	.align	16, 0x90
.LBB53_13:                              # %if.else.i
                                        #   in Loop: Header=BB53_10 Depth=1
	callq	emit_insn
	movq	%rax, 16(%rbx)
	jmp	.LBB53_14
.LBB53_15:                              # %emit_queue.exit
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	xorq	%r9, %r9
	cmpq	$51, %rax
	movq	48(%rsp), %rbx          # 8-byte Reload
	jne	.LBB53_17
# BB#16:                                # %cond.true79
	movq	32(%r13), %rdi
	callq	expr_size
	movq	%rax, %r9
.LBB53_17:                              # %cond.end85
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r12, %rcx
	movq	%r14, %r8
	callq	do_compare_rtx_and_jump
.LBB53_18:                              # %return
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp700:
	.size	do_compare_and_jump, .Ltmp700-do_compare_and_jump
	.cfi_endproc

	.align	16, 0x90
	.type	do_jump_by_parts_equality,@function
do_jump_by_parts_equality:              # @do_jump_by_parts_equality
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp707:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp708:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp709:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp710:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp711:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp712:
	.cfi_def_cfa_offset 112
.Ltmp713:
	.cfi_offset %rbx, -48
.Ltmp714:
	.cfi_offset %r12, -40
.Ltmp715:
	.cfi_offset %r13, -32
.Ltmp716:
	.cfi_offset %r14, -24
.Ltmp717:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	32(%rbx), %rdi
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	%rsi, %r12
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r14, %r14
	callq	expand_expr
	movq	40(%rbx), %rdi
	movq	%rax, 16(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	32(%rbx), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, %r13
	movl	60(%rcx), %edi
	movq	$9, %rcx
	shrq	%cl, %rdi
	andq	$127, %rdi
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rdi), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	xorq	%rdx, %rdx
	idivq	%rsi
	movq	%rax, %rsi
	testq	%r12, %r12
	jne	.LBB54_2
# BB#1:                                 # %if.then
	movq	%rsi, %r14
	movq	%rdi, %r15
	callq	gen_label_rtx
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rax, %r12
	movq	%r12, %r14
.LBB54_2:                               # %if.end
	movq	%r12, 56(%rsp)          # 8-byte Spill
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	16(%rsp), %r15          # 8-byte Reload
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movq	%rdi, %rdx
	jle	.LBB54_5
# BB#3:                                 # %for.body.lr.ph
	xorq	%r12, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB54_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rdx, %r13
	callq	operand_subword_force
	movq	%rax, %r14
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	operand_subword_force
	movq	8(%rbx), %rcx
	movq	%rbx, %r10
	movl	16(%rcx), %ebx
	movl	word_mode(%rip), %r8d
	movq	$13, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, (%rsp)
	movq	$103, %rdx
	xorq	%r9, %r9
	movq	$0, 8(%rsp)
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rcx
	movq	%r10, %rbx
	callq	do_compare_rtx_and_jump
	movq	40(%rsp), %rcx          # 8-byte Reload
	incq	%r12
	movq	%r12, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	%rcx, %rax
	movq	%r13, %rdx
	jne	.LBB54_4
.LBB54_5:                               # %for.end
	movq	32(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	movq	24(%rsp), %rbx          # 8-byte Reload
	je	.LBB54_7
# BB#6:                                 # %if.then24
	callq	emit_jump
.LBB54_7:                               # %if.end25
	testq	%rbx, %rbx
	je	.LBB54_8
# BB#9:                                 # %if.then27
	movq	%rbx, %rdi
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_label  # TAILCALL
.LBB54_8:                               # %if.end29
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp718:
	.size	do_jump_by_parts_equality, .Ltmp718-do_jump_by_parts_equality
	.cfi_endproc

	.globl	do_jump_by_parts_equality_rtx
	.align	16, 0x90
	.type	do_jump_by_parts_equality_rtx,@function
do_jump_by_parts_equality_rtx:          # @do_jump_by_parts_equality_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp725:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp726:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp727:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp728:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp729:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp730:
	.cfi_def_cfa_offset 96
.Ltmp731:
	.cfi_offset %rbx, -48
.Ltmp732:
	.cfi_offset %r12, -40
.Ltmp733:
	.cfi_offset %r13, -32
.Ltmp734:
	.cfi_offset %r14, -24
.Ltmp735:
	.cfi_offset %r15, -16
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	%rsi, %r15
	movq	%rdi, %r13
	movzbl	2(%r13), %eax
	movzbl	mode_size(%rax), %eax
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	xorq	%rdx, %rdx
	idivq	%rsi
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rax, %rbx
	movl	word_mode(%rip), %edi
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	callq	gen_reg_rtx
	movzbl	2(%r13), %edx
	movq	%rax, %r14
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	operand_subword_force
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	testq	%r14, %r14
	je	.LBB55_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	jmp	.LBB55_3
.LBB55_1:
	movabsq	$0, %rax
.LBB55_3:                               # %entry
	movq	%r15, 24(%rsp)          # 8-byte Spill
	cmpq	$2, %rbx
	jl	.LBB55_10
# BB#4:                                 # %entry
	testq	%r14, %r14
	je	.LBB55_10
# BB#5:
	movq	$1, %rbx
	.align	16, 0x90
.LBB55_6:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	optab_table+152(%rip), %r15
	movzbl	2(%r13), %edx
	movl	word_mode(%rip), %r12d
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	operand_subword_force
	movq	$1, %r9
	movq	$2, (%rsp)
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	movq	%r14, %r8
	callq	expand_binop
	movq	%rax, %r14
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	40(%rsp), %rsi          # 8-byte Reload
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%r14, %r14
	movabsq	$0, %rax
	je	.LBB55_8
# BB#7:                                 # %for.body
                                        #   in Loop: Header=BB55_6 Depth=1
	movabsq	$1, %rax
.LBB55_8:                               # %for.body
                                        #   in Loop: Header=BB55_6 Depth=1
	cmpq	%rsi, %rdx
	jge	.LBB55_10
# BB#9:                                 # %for.body
                                        #   in Loop: Header=BB55_6 Depth=1
	testq	%r14, %r14
	jne	.LBB55_6
.LBB55_10:                              # %for.end
	testq	%rax, %rax
	je	.LBB55_12
# BB#11:                                # %if.then
	movq	const_int_rtx+512(%rip), %rsi
	movl	word_mode(%rip), %r8d
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$103, %rdx
	movq	$1, %rcx
	xorq	%r9, %r9
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	callq	do_compare_rtx_and_jump
	jmp	.LBB55_20
.LBB55_12:                              # %if.end
	xorq	%r14, %r14
	movq	24(%rsp), %rbx          # 8-byte Reload
	testq	%rbx, %rbx
	movabsq	$0, %rax
	movq	40(%rsp), %rdx          # 8-byte Reload
	jne	.LBB55_14
# BB#13:                                # %if.then17
	movq	%rdx, %rbx
	callq	gen_label_rtx
	movq	%rbx, %rdx
	movq	%rax, %rbx
                                        # kill: RAX<def> RBX<kill>
.LBB55_14:                              # %if.end19
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB55_17
# BB#15:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rdx
	.align	16, 0x90
.LBB55_16:                              # %for.body23
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %r15
	movzbl	2(%r13), %edx
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	operand_subword_force
	movq	const_int_rtx+512(%rip), %rsi
	movl	word_mode(%rip), %r8d
	movq	%rbx, (%rsp)
	movq	$103, %rdx
	movq	$1, %rcx
	xorq	%r9, %r9
	movq	$0, 8(%rsp)
	movq	%rax, %rdi
	callq	do_compare_rtx_and_jump
	movq	%r15, %rdx
	incq	%r14
	movq	%r14, %rax
	andq	%r12, %rax
	cmpq	%rdx, %rax
	jne	.LBB55_16
.LBB55_17:                              # %for.end30
	movq	32(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB55_19
# BB#18:                                # %if.then32
	callq	emit_jump
.LBB55_19:                              # %if.end33
	movq	24(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB55_20
# BB#21:                                # %if.then35
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_label  # TAILCALL
.LBB55_20:                              # %if.end37
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp736:
	.size	do_jump_by_parts_equality_rtx, .Ltmp736-do_jump_by_parts_equality_rtx
	.cfi_endproc

	.globl	compare_from_rtx
	.align	16, 0x90
	.type	compare_from_rtx,@function
compare_from_rtx:                       # @compare_from_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp743:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp744:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp745:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp746:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp747:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp748:
	.cfi_def_cfa_offset 64
.Ltmp749:
	.cfi_offset %rbx, -48
.Ltmp750:
	.cfi_offset %r12, -40
.Ltmp751:
	.cfi_offset %r13, -32
.Ltmp752:
	.cfi_offset %r14, -24
.Ltmp753:
	.cfi_offset %r15, -16
	movq	%r9, 8(%rsp)            # 8-byte Spill
	movq	%r8, %r12
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	%rdx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r15
                                        # kill: RDI<def> R15<kill>
                                        # kill: RSI<def> RBX<kill>
	callq	swap_commutative_operands_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB56_1
# BB#2:                                 # %if.then
	movq	%r13, %rdi
	callq	swap_condition
	movq	%rax, %r13
	movq	%r15, %r14
	jmp	.LBB56_3
.LBB56_1:
	movq	%rbx, %r14
	movq	%r15, %rbx
.LBB56_3:                               # %if.end
	movl	flag_force_mem(%rip), %eax
	testq	%rax, %rax
	je	.LBB56_5
# BB#4:                                 # %if.then3
	movq	%rbx, %rdi
	callq	force_not_mem
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	force_not_mem
	movq	%rax, %r14
.LBB56_5:                               # %if.end6
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB56_9
# BB#6:                                 # %if.then.i
	movslq	(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB56_8
# BB#7:                                 # %if.then3.i
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	adjust_stack
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
.LBB56_8:                               # %if.end.i
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.LBB56_9:                               # %do_pending_stack_adjust.exit
	movzbl	(%rbx), %eax
	cmpq	$54, %rax
	jne	.LBB56_13
# BB#10:                                # %land.lhs.true
	movzbl	(%r14), %eax
	cmpq	$54, %rax
	jne	.LBB56_13
# BB#11:                                # %land.lhs.true10
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	simplify_relational_operation
	testq	%rax, %rax
	je	.LBB56_13
# BB#12:                                # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB56_13:                              # %if.end14
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%r12, %r8
	movq	(%rsp), %r9             # 8-byte Reload
	callq	emit_cmp_insn
	movq	global_rtl+8(%rip), %rdx
	movq	const_int_rtx+512(%rip), %rcx
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp754:
	.size	compare_from_rtx, .Ltmp754-compare_from_rtx
	.cfi_endproc

	.globl	case_values_threshold
	.align	16, 0x90
	.type	case_values_threshold,@function
case_values_threshold:                  # @case_values_threshold
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$5, %rax
	retq
.Ltmp755:
	.size	case_values_threshold, .Ltmp755-case_values_threshold
	.cfi_endproc

	.globl	try_casesi
	.align	16, 0x90
	.type	try_casesi,@function
try_casesi:                             # @try_casesi
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp756:
	.size	try_casesi, .Ltmp756-try_casesi
	.cfi_endproc

	.globl	try_tablejump
	.align	16, 0x90
	.type	try_tablejump,@function
try_tablejump:                          # @try_tablejump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp763:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp764:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp765:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp766:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp767:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp768:
	.cfi_def_cfa_offset 80
.Ltmp769:
	.cfi_offset %rbx, -48
.Ltmp770:
	.cfi_offset %r12, -40
.Ltmp771:
	.cfi_offset %r13, -32
.Ltmp772:
	.cfi_offset %r14, -24
.Ltmp773:
	.cfi_offset %r15, -16
	movq	%r9, 16(%rsp)           # 8-byte Spill
	movq	%r8, 24(%rsp)           # 8-byte Spill
	movq	%rcx, %r13
	movq	%rdx, %rbx
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	convert
	movq	%rax, %r12
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	%rax, %rcx
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	40(%rcx), %rbx
	movq	%rax, %r12
	jmp	.LBB59_1
	.align	16, 0x90
.LBB59_5:                               # %if.end.i
                                        #   in Loop: Header=BB59_1 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rbx), %rcx
	movq	16(%rax), %rax
	movq	%rcx, 40(%rax)
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rbx
.LBB59_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB59_6
# BB#2:                                 # %while.body.i
                                        #   in Loop: Header=BB59_1 Depth=1
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$24, %rax
	jne	.LBB59_4
# BB#3:                                 # %if.then.i
                                        #   in Loop: Header=BB59_1 Depth=1
	movq	8(%rdi), %rax
	movq	8(%rax), %rax
	movq	%rax, 16(%rbx)
	callq	emit_insn
	jmp	.LBB59_5
	.align	16, 0x90
.LBB59_4:                               # %if.else.i
                                        #   in Loop: Header=BB59_1 Depth=1
	callq	emit_insn
	movq	%rax, 16(%rbx)
	jmp	.LBB59_5
.LBB59_6:                               # %emit_queue.exit
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	protect_from_queue
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rcx
	movl	4(%rcx), %edx
	movq	%rax, %r12
	testq	%rdx, %rdx
	jne	.LBB59_10
# BB#7:                                 # %if.then.i35
	movslq	(%rcx), %rsi
	testq	%rsi, %rsi
	je	.LBB59_9
# BB#8:                                 # %if.then3.i
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	adjust_stack
	movq	cfun(%rip), %rax
	movq	16(%rax), %rcx
.LBB59_9:                               # %if.end.i37
	xorq	%rax, %rax
	movl	%eax, (%rcx)
.LBB59_10:                              # %do_pending_stack_adjust.exit
	movq	8(%r13), %rax
	movl	60(%r14), %r14d
	movq	$9, %rcx
	movl	60(%rax), %r15d
	shrq	%cl, %r14
	andq	$127, %r14
	movq	$9, %rcx
	shrq	%cl, %r15
	andq	$127, %r15
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	callq	expand_expr
	movq	8(%r13), %rcx
	movl	16(%rcx), %ebx
	movq	$13, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	convert_modes
	movq	$109, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r9
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, (%rsp)
	movq	%r12, %rdi
	movq	%rax, %rsi
	movq	%r14, %r8
	callq	emit_cmp_and_jump_insns
	movl	target_flags(%rip), %edx
	movq	$25, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	cmpq	%r14, %rdi
	je	.LBB59_12
# BB#11:                                # %if.then.i40
	xorq	%rsi, %rsi
	movq	$1, %rcx
	movq	%r12, %rdx
	callq	convert_modes
	movl	target_flags(%rip), %edx
	movq	%rax, %r12
.LBB59_12:                              # %if.end.i41
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	%rdx, %rbx
	andq	$33554432, %rbx         # imm = 0x2000000
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$33554432, %rdx         # imm = 0x2000000
	movq	$4, %r15
	movq	$4, %rax
	je	.LBB59_15
# BB#13:                                # %lor.rhs.i
	movl	flag_pic(%rip), %eax
	testq	%rax, %rax
	movq	$4, %rax
	jne	.LBB59_15
# BB#14:                                # %select.mid
	movq	$5, %rax
.LBB59_15:                              # %lor.end.i
	movzbl	mode_size(%rax), %esi
	orq	$4, %rbx
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$78, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movl	target_flags(%rip), %esi
	movq	%rax, %r14
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$67, %rdi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_u00
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movzbl	target_flags+3(%rip), %ecx
	testq	$2, %rcx
	je	.LBB59_18
# BB#16:                                # %lor.rhs23.i
	movl	flag_pic(%rip), %ecx
	testq	%rcx, %rcx
	movq	$4, %r15
	jne	.LBB59_18
# BB#17:                                # %select.mid4
	movq	$5, %r15
.LBB59_18:                              # %lor.end25.i
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	memory_address_noforce
	movzbl	target_flags+3(%rip), %ecx
	movq	%rax, %r15
	movq	$4, %r12
	testq	$2, %rcx
	movq	$4, %rdi
	je	.LBB59_21
# BB#19:                                # %lor.rhs30.i
	movl	flag_pic(%rip), %eax
	testq	%rax, %rax
	movq	$4, %rdi
	jne	.LBB59_21
# BB#20:                                # %select.mid6
	movq	$5, %rdi
.LBB59_21:                              # %lor.end32.i
	callq	gen_reg_rtx
	movzbl	target_flags+3(%rip), %ecx
	movq	%rax, %rbx
	testq	$2, %rcx
	je	.LBB59_24
# BB#22:                                # %lor.rhs37.i
	movl	flag_pic(%rip), %eax
	testq	%rax, %rax
	movq	$4, %r12
	jne	.LBB59_24
# BB#23:                                # %select.mid8
	movq	$5, %r12
.LBB59_24:                              # %lor.end39.i
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_MEM
	movl	(%rax), %ecx
	orq	$67108864, %rcx         # imm = 0x4000000
	movl	%ecx, (%rax)
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert_move
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	gen_tablejump
	movq	%rax, %rdi
	callq	emit_jump_insn
	movl	flag_pic(%rip), %eax
	testq	%rax, %rax
	jne	.LBB59_26
# BB#25:                                # %if.then45.i
	callq	emit_barrier
.LBB59_26:                              # %do_tablejump.exit
	movq	$1, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp774:
	.size	try_tablejump, .Ltmp774-try_tablejump
	.cfi_endproc

	.align	16, 0x90
	.type	is_zeros_p,@function
is_zeros_p:                             # @is_zeros_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp779:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp780:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp781:
	.cfi_def_cfa_offset 32
.Ltmp782:
	.cfi_offset %rbx, -32
.Ltmp783:
	.cfi_offset %r14, -24
.Ltmp784:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	xorq	%r14, %r14
	jmp	.LBB60_1
	.align	16, 0x90
.LBB60_11:                              # %sw.bb
                                        #   in Loop: Header=BB60_1 Depth=1
	movq	32(%rbx), %rbx
.LBB60_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx), %eax
	cmpq	$113, %rax
	jle	.LBB60_2
# BB#10:                                # %tailrecurse
                                        #   in Loop: Header=BB60_1 Depth=1
	addq	$-114, %rax
	andq	%r15, %rax
	cmpq	$3, %rax
	jbe	.LBB60_11
	jmp	.LBB60_31
.LBB60_2:                               # %tailrecurse
	cmpq	$45, %rax
	jg	.LBB60_5
# BB#3:                                 # %tailrecurse
	addq	$-25, %rax
	andq	%r15, %rax
	cmpq	$3, %rax
	ja	.LBB60_31
# BB#4:                                 # %tailrecurse
	jmpq	*.LJTI60_0(,%rax,8)
.LBB60_12:                              # %sw.bb2
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	integer_zerop  # TAILCALL
.LBB60_5:                               # %tailrecurse
	cmpq	$46, %rax
	jne	.LBB60_31
# BB#6:                                 # %sw.bb18
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.LBB60_26
# BB#7:                                 # %land.lhs.true
	movzbl	16(%rax), %eax
	cmpq	$19, %rax
	jne	.LBB60_26
# BB#8:                                 # %if.then27
	cmpq	$0, 40(%rbx)
	jne	.LBB60_9
# BB#25:                                # %if.then27
	movabsq	$1, %r14
	jmp	.LBB60_31
.LBB60_26:                              # %if.end32
	movq	40(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB60_27
	.align	16, 0x90
.LBB60_30:                              # %for.body38
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rdi
	callq	is_zeros_p
	testq	%r15, %rax
	je	.LBB60_31
# BB#28:                                # %for.cond36
                                        #   in Loop: Header=BB60_30 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB60_30
# BB#29:
	movq	$1, %r14
	jmp	.LBB60_31
.LBB60_17:                              # %sw.bb9
	addq	$32, %rbx
	movabsq	$dconst0, %rsi
	movq	$24, %rdx
	movq	%rbx, %rdi
	callq	memcmp
	testq	%r15, %rax
	jne	.LBB60_18
# BB#19:                                # %sw.bb9
	movabsq	$1, %r14
	jmp	.LBB60_31
.LBB60_13:                              # %sw.bb4
	movq	32(%rbx), %rdi
	callq	is_zeros_p
	xorq	%r14, %r14
	testq	%r15, %rax
	je	.LBB60_31
# BB#14:                                # %land.rhs
	movq	40(%rbx), %rdi
	callq	is_zeros_p
	testq	%r15, %rax
	je	.LBB60_15
# BB#16:                                # %land.rhs
	movabsq	$1, %r14
	jmp	.LBB60_31
.LBB60_20:                              # %sw.bb13
	movq	32(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB60_21
	.align	16, 0x90
.LBB60_22:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rdi
	callq	is_zeros_p
	testq	%r15, %rax
	je	.LBB60_31
# BB#23:                                # %for.cond
                                        #   in Loop: Header=BB60_22 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB60_22
# BB#24:
	movq	$1, %r14
	jmp	.LBB60_31
.LBB60_27:
	movq	$1, %r14
	jmp	.LBB60_31
.LBB60_18:
	movabsq	$0, %r14
	jmp	.LBB60_31
.LBB60_21:
	movq	$1, %r14
	jmp	.LBB60_31
.LBB60_9:
	movabsq	$0, %r14
	jmp	.LBB60_31
.LBB60_15:
	movabsq	$0, %r14
.LBB60_31:                              # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp785:
	.size	is_zeros_p, .Ltmp785-is_zeros_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI60_0:
	.quad	.LBB60_12
	.quad	.LBB60_17
	.quad	.LBB60_13
	.quad	.LBB60_20

	.text
	.align	16, 0x90
	.type	store_constructor_field,@function
store_constructor_field:                # @store_constructor_field
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp792:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp793:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp794:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp795:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp796:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp797:
	.cfi_def_cfa_offset 80
.Ltmp798:
	.cfi_offset %rbx, -48
.Ltmp799:
	.cfi_offset %r12, -40
.Ltmp800:
	.cfi_offset %r13, -32
.Ltmp801:
	.cfi_offset %r14, -24
.Ltmp802:
	.cfi_offset %r15, -16
	movq	%r8, %r15
	movzbl	16(%r15), %eax
	movq	88(%rsp), %r12
	movq	%rdx, %rbx
	movq	%rsi, %r14
	movq	%rdi, %r13
	cmpq	$46, %rax
	jne	.LBB61_14
# BB#1:                                 # %entry
	movq	%rbx, %rax
	andq	$7, %rax
	jne	.LBB61_14
# BB#2:                                 # %land.lhs.true2
	movl	(%r13), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	testq	%rbx, %rbx
	je	.LBB61_4
# BB#3:                                 # %land.lhs.true2
	cmpq	$66, %rax
	je	.LBB61_4
.LBB61_14:                              # %if.else
	movq	%r12, 16(%rsp)
	movq	%r9, 8(%rsp)
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	%r15, %r8
	callq	store_field
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB61_4:                               # %if.then
	cmpq	$66, %rax
	jne	.LBB61_9
# BB#5:                                 # %if.then10
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	cmpq	$51, %rdi
	movq	$51, %rsi
	je	.LBB61_8
# BB#6:                                 # %lor.rhs
	callq	get_mode_alignment
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	%rbx, %rax
	cqto
	idivq	%rcx
	movq	%rdx, %rsi
	testq	%rsi, %rsi
	je	.LBB61_8
# BB#7:                                 # %lor.rhs
	movq	$51, %rsi
.LBB61_8:                               # %lor.end
	movq	$63, %rcx
	movq	%rbx, %rdx
	sarq	%cl, %rdx
	movq	$61, %rcx
	shrq	%cl, %rdx
	addq	%rbx, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r13, %rdi
	callq	adjust_address_1
	movq	%rax, %r13
	movl	(%r13), %edi
.LBB61_9:                               # %if.end
	andq	$16842751, %rdi         # imm = 0x100FFFF
	cmpq	$66, %rdi
	jne	.LBB61_13
# BB#10:                                # %land.lhs.true29
	movq	16(%r13), %rax
	testq	%rax, %rax
	je	.LBB61_13
# BB#11:                                # %cond.false
	cmpq	$0, (%rax)
	je	.LBB61_13
# BB#12:                                # %if.then38
	movq	%r13, %rdi
	callq	copy_rtx
	movq	%rax, %r13
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	set_mem_alias_set
.LBB61_13:                              # %if.end41
	movq	80(%rsp), %rdx
	movq	$3, %rcx
	shrq	%cl, %r14
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%r14, %rcx
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	store_constructor  # TAILCALL
.Ltmp803:
	.size	store_constructor_field, .Ltmp803-store_constructor_field
	.cfi_endproc

	.align	16, 0x90
	.type	clear_by_pieces_1,@function
clear_by_pieces_1:                      # @clear_by_pieces_1
	.cfi_startproc
# BB#0:                                 # %entry
	movq	const_int_rtx+512(%rip), %rax
	retq
.Ltmp804:
	.size	clear_by_pieces_1, .Ltmp804-clear_by_pieces_1
	.cfi_endproc

	.type	direct_store,@object    # @direct_store
	.local	direct_store
	.comm	direct_store,59,16
	.type	direct_load,@object     # @direct_load
	.local	direct_load
	.comm	direct_load,59,16
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"expr.c"
	.size	.L.str, 7

	.type	.L__FUNCTION__.finish_expr_for_function,@object # @__FUNCTION__.finish_expr_for_function
.L__FUNCTION__.finish_expr_for_function:
	.asciz	"finish_expr_for_function"
	.size	.L__FUNCTION__.finish_expr_for_function, 25

	.type	.L__FUNCTION__.convert_move,@object # @__FUNCTION__.convert_move
.L__FUNCTION__.convert_move:
	.asciz	"convert_move"
	.size	.L__FUNCTION__.convert_move, 13

	.type	.L__FUNCTION__.move_by_pieces,@object # @__FUNCTION__.move_by_pieces
.L__FUNCTION__.move_by_pieces:
	.asciz	"move_by_pieces"
	.size	.L__FUNCTION__.move_by_pieces, 15

	.type	emit_block_move.fn,@object # @emit_block_move.fn
	.local	emit_block_move.fn
	.comm	emit_block_move.fn,8,8
	.type	.L__FUNCTION__.emit_block_move,@object # @__FUNCTION__.emit_block_move
.L__FUNCTION__.emit_block_move:
	.asciz	"emit_block_move"
	.size	.L__FUNCTION__.emit_block_move, 16

	.type	movstr_optab,@object    # @movstr_optab
	.comm	movstr_optab,236,16
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"memcpy"
	.size	.L.str1, 7

	.type	.L__FUNCTION__.move_block_from_reg,@object # @__FUNCTION__.move_block_from_reg
.L__FUNCTION__.move_block_from_reg:
	.asciz	"move_block_from_reg"
	.size	.L__FUNCTION__.move_block_from_reg, 20

	.type	.L__FUNCTION__.emit_group_load,@object # @__FUNCTION__.emit_group_load
.L__FUNCTION__.emit_group_load:
	.asciz	"emit_group_load"
	.size	.L__FUNCTION__.emit_group_load, 16

	.type	.L__FUNCTION__.emit_group_store,@object # @__FUNCTION__.emit_group_store
.L__FUNCTION__.emit_group_store:
	.asciz	"emit_group_store"
	.size	.L__FUNCTION__.emit_group_store, 17

	.type	.L__FUNCTION__.use_reg,@object # @__FUNCTION__.use_reg
.L__FUNCTION__.use_reg:
	.asciz	"use_reg"
	.size	.L__FUNCTION__.use_reg, 8

	.type	.L__FUNCTION__.use_regs,@object # @__FUNCTION__.use_regs
.L__FUNCTION__.use_regs:
	.asciz	"use_regs"
	.size	.L__FUNCTION__.use_regs, 9

	.type	.L__FUNCTION__.can_store_by_pieces,@object # @__FUNCTION__.can_store_by_pieces
.L__FUNCTION__.can_store_by_pieces:
	.asciz	"can_store_by_pieces"
	.size	.L__FUNCTION__.can_store_by_pieces, 20

	.type	.L__FUNCTION__.store_by_pieces,@object # @__FUNCTION__.store_by_pieces
.L__FUNCTION__.store_by_pieces:
	.asciz	"store_by_pieces"
	.size	.L__FUNCTION__.store_by_pieces, 16

	.type	clear_storage.fn,@object # @clear_storage.fn
	.local	clear_storage.fn
	.comm	clear_storage.fn,8,8
	.type	clrstr_optab,@object    # @clrstr_optab
	.comm	clrstr_optab,236,16
	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"memset"
	.size	.L.str2, 7

	.type	.L__FUNCTION__.emit_move_insn,@object # @__FUNCTION__.emit_move_insn
.L__FUNCTION__.emit_move_insn:
	.asciz	"emit_move_insn"
	.size	.L__FUNCTION__.emit_move_insn, 15

	.type	.L__FUNCTION__.emit_move_insn_1,@object # @__FUNCTION__.emit_move_insn_1
.L__FUNCTION__.emit_move_insn_1:
	.asciz	"emit_move_insn_1"
	.size	.L__FUNCTION__.emit_move_insn_1, 17

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"function using short complex types cannot be inline"
	.size	.L.str3, 52

	.type	.L__FUNCTION__.emit_push_insn,@object # @__FUNCTION__.emit_push_insn
.L__FUNCTION__.emit_push_insn:
	.asciz	"emit_push_insn"
	.size	.L__FUNCTION__.emit_push_insn, 15

	.type	.L__FUNCTION__.expand_assignment,@object # @__FUNCTION__.expand_assignment
.L__FUNCTION__.expand_assignment:
	.asciz	"expand_assignment"
	.size	.L__FUNCTION__.expand_assignment, 18

	.type	safe_from_p.save_expr_list,@object # @safe_from_p.save_expr_list
	.local	safe_from_p.save_expr_list
	.comm	safe_from_p.save_expr_list,8,8
	.type	.L__FUNCTION__.safe_from_p,@object # @__FUNCTION__.safe_from_p
.L__FUNCTION__.safe_from_p:
	.asciz	"safe_from_p"
	.size	.L__FUNCTION__.safe_from_p, 12

	.type	placeholder_list,@object # @placeholder_list
	.local	placeholder_list
	.comm	placeholder_list,8,8
	.type	cse_not_expected,@object # @cse_not_expected
	.comm	cse_not_expected,4,4
	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"prior parameter's size depends on `%s'"
	.size	.L.str4, 39

	.type	.L__FUNCTION__.expand_expr,@object # @__FUNCTION__.expand_expr
.L__FUNCTION__.expand_expr:
	.asciz	"expand_expr"
	.size	.L__FUNCTION__.expand_expr, 12

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"returned value in block_exit_expr"
	.size	.L.str5, 34

	.type	.L__FUNCTION__.do_jump,@object # @__FUNCTION__.do_jump
.L__FUNCTION__.do_jump:
	.asciz	"do_jump"
	.size	.L__FUNCTION__.do_jump, 8

	.type	.L__FUNCTION__.do_store_flag,@object # @__FUNCTION__.do_store_flag
.L__FUNCTION__.do_store_flag:
	.asciz	"do_store_flag"
	.size	.L__FUNCTION__.do_store_flag, 14

	.type	.L__FUNCTION__.store_constructor,@object # @__FUNCTION__.store_constructor
.L__FUNCTION__.store_constructor:
	.asciz	"store_constructor"
	.size	.L__FUNCTION__.store_constructor, 18

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"__setbits"
	.size	.L.str6, 10

	.type	.L__FUNCTION__.store_field,@object # @__FUNCTION__.store_field
.L__FUNCTION__.store_field:
	.asciz	"store_field"
	.size	.L__FUNCTION__.store_field, 12

	.type	.L__FUNCTION__.store_by_pieces_1,@object # @__FUNCTION__.store_by_pieces_1
.L__FUNCTION__.store_by_pieces_1:
	.asciz	"store_by_pieces_1"
	.size	.L__FUNCTION__.store_by_pieces_1, 18

	.type	.L__FUNCTION__.move_by_pieces_ninsns,@object # @__FUNCTION__.move_by_pieces_ninsns
.L__FUNCTION__.move_by_pieces_ninsns:
	.asciz	"move_by_pieces_ninsns"
	.size	.L__FUNCTION__.move_by_pieces_ninsns, 22


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
