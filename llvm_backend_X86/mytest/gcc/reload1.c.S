	.file	"reload1.c.bc"
	.text
	.globl	init_reload
	.align	16, 0x90
	.type	init_reload,@function
init_reload:                            # @init_reload
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp7:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp8:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp9:
	.cfi_def_cfa_offset 48
.Ltmp10:
	.cfi_offset %rbx, -40
.Ltmp11:
	.cfi_offset %r12, -32
.Ltmp12:
	.cfi_offset %r14, -24
.Ltmp13:
	.cfi_offset %r15, -16
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movq	$58, %rsi
	movq	%rbx, %rdi
	callq	gen_rtx_REG
	movq	%rax, %r15
	xorq	%rdi, %rdi
	movq	$4, %rsi
	xorq	%r14, %r14
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movb	%r14b, spill_indirect_levels(%rip)
	movq	$2, %rdi
	movq	%rbx, %rsi
	callq	memory_address_p
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	jmp	.LBB0_2
	.align	16, 0x90
.LBB0_1:                                # %while.body
                                        #   in Loop: Header=BB0_2 Depth=1
	movzbl	spill_indirect_levels(%rip), %eax
	movl	target_flags(%rip), %edi
	incq	%rax
	movb	%al, spill_indirect_levels(%rip)
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$2, %rdi
	movq	%rbx, %rsi
	callq	memory_address_p
.LBB0_2:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%r12, %rax
	jne	.LBB0_1
# BB#3:                                 # %while.end
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movabsq	$.L.str, %rdx
	movq	$68, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_fmt_s
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$2, %rdi
	movq	%rax, %rsi
	callq	memory_address_p
	movb	%al, indirect_symref_ok(%rip)
	.align	16, 0x90
.LBB0_5:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movq	$6, %rsi
	movq	%rbx, %rdi
	callq	gen_rtx_REG
	movl	target_flags(%rip), %edi
	movq	%rax, %r15
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_REG
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$4, %rsi
	movq	%rax, %rdi
	callq	plus_constant_wide
	movq	$2, %rdi
	movq	%rax, %rsi
	callq	memory_address_p
	testq	%r12, %rax
	jne	.LBB0_6
# BB#4:                                 # %for.cond
                                        #   in Loop: Header=BB0_5 Depth=1
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	jle	.LBB0_5
	jmp	.LBB0_7
.LBB0_6:                                # %if.then
	movq	$1, %rax
	movb	%al, double_reg_address_ok(%rip)
.LBB0_7:                                # %for.end
	movabsq	$reload_obstack, %rdi
	callq	gcc_obstack_init
	movq	reload_obstack+32(%rip), %rax
	movq	reload_obstack+24(%rip), %rcx
	cmpq	%rcx, %rax
	jns	.LBB0_9
# BB#8:                                 # %if.then45
	movabsq	$reload_obstack, %rdi
	xorq	%rsi, %rsi
	callq	_obstack_newchunk
	movq	reload_obstack+24(%rip), %rcx
.LBB0_9:                                # %if.end46
	movq	reload_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB0_11
# BB#10:                                # %if.then53
	movzbl	reload_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, reload_obstack+80(%rip)
.LBB0_11:                               # %if.end54
	movslq	reload_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, reload_obstack+24(%rip)
	movq	reload_obstack+32(%rip), %rbx
	movq	reload_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB0_13
# BB#12:                                # %if.then75
	movq	%rbx, reload_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB0_13:                               # %if.end78
	movq	%rdx, reload_obstack+16(%rip)
	movq	%rax, reload_startobj(%rip)
	movabsq	$spilled_pseudos, %rdi
	callq	bitmap_initialize
	movabsq	$pseudos_counted, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	bitmap_initialize  # TAILCALL
.Ltmp14:
	.size	init_reload, .Ltmp14-init_reload
	.cfi_endproc

	.globl	new_insn_chain
	.align	16, 0x90
	.type	new_insn_chain,@function
new_insn_chain:                         # @new_insn_chain
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp17:
	.cfi_def_cfa_offset 16
.Ltmp18:
	.cfi_offset %rbx, -16
	movq	unused_insn_chains(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB1_1
# BB#8:                                 # %if.else
	movq	(%rbx), %rax
	movq	%rax, unused_insn_chains(%rip)
	jmp	.LBB1_9
.LBB1_1:                                # %if.then
	movq	reload_obstack+32(%rip), %rcx
	movq	reload_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$263, %rcx              # imm = 0x107
	jg	.LBB1_3
# BB#2:                                 # %if.then3
	movabsq	$reload_obstack, %rdi
	movq	$264, %rsi              # imm = 0x108
	callq	_obstack_newchunk
	movq	reload_obstack+24(%rip), %rax
.LBB1_3:                                # %if.end
	addq	$264, %rax              # imm = 0x108
	movq	%rax, reload_obstack+24(%rip)
	movq	reload_obstack+16(%rip), %rbx
	cmpq	%rbx, %rax
	jne	.LBB1_5
# BB#4:                                 # %if.then10
	movzbl	reload_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, reload_obstack+80(%rip)
.LBB1_5:                                # %if.end11
	movslq	reload_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, reload_obstack+24(%rip)
	movq	reload_obstack+32(%rip), %rax
	movq	reload_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rax, %rdi
	subq	%rdx, %rdi
	cmpq	%rdi, %rsi
	jle	.LBB1_7
# BB#6:                                 # %if.then31
	movq	%rax, reload_obstack+24(%rip)
	movq	%rax, %rcx
.LBB1_7:                                # %if.end34
	movq	%rcx, reload_obstack+16(%rip)
	leaq	40(%rbx), %rdi
	callq	bitmap_initialize
	movq	%rbx, %rdi
	addq	$64, %rdi
	callq	bitmap_initialize
.LBB1_9:                                # %if.end39
	movzbl	262(%rbx), %eax
	andq	$240, %rax
	movb	%al, 262(%rbx)
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp19:
	.size	new_insn_chain, .Ltmp19-new_insn_chain
	.cfi_endproc

	.globl	compute_use_by_pseudos
	.align	16, 0x90
	.type	compute_use_by_pseudos,@function
compute_use_by_pseudos:                 # @compute_use_by_pseudos
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp26:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp27:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp28:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp29:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp30:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp31:
	.cfi_def_cfa_offset 64
.Ltmp32:
	.cfi_offset %rbx, -48
.Ltmp33:
	.cfi_offset %r12, -40
.Ltmp34:
	.cfi_offset %r13, -32
.Ltmp35:
	.cfi_offset %r14, -24
.Ltmp36:
	.cfi_offset %r15, -16
	movq	(%rsi), %r8
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	testq	%r8, %r8
	je	.LBB2_31
# BB#1:                                 # %for.cond6.preheader.lr.ph
	movl	16(%r8), %eax
	testq	%rax, %rax
	movq	$53, %r9
	je	.LBB2_3
# BB#2:                                 # %select.mid
	xorq	%r9, %r9
.LBB2_3:                                # %select.end
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB2_4:                                # %for.cond6.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_5 Depth 2
                                        #       Child Loop BB2_7 Depth 3
                                        #         Child Loop BB2_25 Depth 4
	xorq	%r15, %r15
	.align	16, 0x90
.LBB2_5:                                # %for.body8
                                        #   Parent Loop BB2_4 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_7 Depth 3
                                        #         Child Loop BB2_25 Depth 4
	movq	24(%r8,%r15,8), %r13
	testq	%r13, %r13
	je	.LBB2_29
# BB#6:                                 # %for.body13.preheader
                                        #   in Loop: Header=BB2_5 Depth=2
	movq	$6, %rcx
	movq	%r15, %rdi
	shlq	%cl, %rdi
	movq	%r9, %rsi
	andq	%r10, %rsi
	.align	16, 0x90
.LBB2_7:                                # %for.body13
                                        #   Parent Loop BB2_4 Depth=1
                                        #     Parent Loop BB2_5 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB2_25 Depth 4
	movq	$1, %rax
	movq	%rsi, %rcx
	shlq	%cl, %rax
	testq	%r13, %rax
	je	.LBB2_28
# BB#8:                                 # %if.then15
                                        #   in Loop: Header=BB2_7 Depth=3
	movl	16(%r8), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	movq	reg_renumber(%rip), %rbx
	leaq	(%rsi,%rdi), %rcx
	addq	%rdx, %rcx
	andq	%r10, %rcx
	movswq	(%rbx,%rcx,2), %rbx
	notq	%rax
	andq	%rax, %r13
	testq	%rbx, %rbx
	jns	.LBB2_11
# BB#9:                                 # %if.then24
                                        #   in Loop: Header=BB2_7 Depth=3
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	jne	.LBB2_27
	jmp	.LBB2_10
	.align	16, 0x90
.LBB2_11:                               # %if.else
                                        #   in Loop: Header=BB2_7 Depth=3
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	(%rax,%rcx,8), %rax
	movzbl	2(%rax), %eax
	movq	%rbx, %rcx
	andq	$65528, %rcx            # imm = 0xFFF8
	cmpq	$8, %rcx
	je	.LBB2_15
# BB#12:                                # %if.else
                                        #   in Loop: Header=BB2_7 Depth=3
	leaq	-21(%rbx), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB2_15
# BB#13:                                # %if.else
                                        #   in Loop: Header=BB2_7 Depth=3
	leaq	-45(%rbx), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB2_15
# BB#14:                                # %if.else
                                        #   in Loop: Header=BB2_7 Depth=3
	leaq	-29(%rbx), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$7, %rcx
	jbe	.LBB2_15
# BB#19:                                # %cond.false
                                        #   in Loop: Header=BB2_7 Depth=3
	cmpq	$24, %rax
	jne	.LBB2_20
# BB#22:                                # %cond.true89
                                        #   in Loop: Header=BB2_7 Depth=3
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB2_24
.LBB2_15:                               # %cond.true
                                        #   in Loop: Header=BB2_7 Depth=3
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB2_24
# BB#16:                                # %lor.rhs
                                        #   in Loop: Header=BB2_7 Depth=3
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB2_18
# BB#17:                                # %lor.rhs
                                        #   in Loop: Header=BB2_7 Depth=3
	movabsq	$1, %rax
.LBB2_18:                               # %lor.rhs
                                        #   in Loop: Header=BB2_7 Depth=3
	incq	%rax
	jmp	.LBB2_24
.LBB2_20:                               # %cond.false
                                        #   in Loop: Header=BB2_7 Depth=3
	cmpq	$18, %rax
	jne	.LBB2_23
# BB#21:                                # %cond.true75
                                        #   in Loop: Header=BB2_7 Depth=3
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB2_24
.LBB2_23:                               # %while.cond116.preheader
                                        #   in Loop: Header=BB2_7 Depth=3
	movl	target_flags(%rip), %r14d
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %r14
	andq	$4, %r14
	leaq	3(%rax,%r14), %rax
	addq	$4, %r14
	cqto
	idivq	%r14
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB2_27
	.align	16, 0x90
.LBB2_24:                               # %while.body119.lr.ph
                                        #   in Loop: Header=BB2_7 Depth=3
	movq	%r8, %r12
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	(%rcx), %r14
	.align	16, 0x90
.LBB2_25:                               # %while.body119
                                        #   Parent Loop BB2_4 Depth=1
                                        #     Parent Loop BB2_5 Depth=2
                                        #       Parent Loop BB2_7 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	%r14, %r11
	leaq	-1(%rax,%rbx), %rdx
	leaq	-1(%rax), %r8
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %r14
	movq	%rdx, %rcx
	shlq	%cl, %r14
	orq	%r11, %r14
	testq	%rax, %rax
	movq	%r8, %rax
	jg	.LBB2_25
# BB#26:                                # %while.cond116.if.end124.loopexit_crit_edge
                                        #   in Loop: Header=BB2_7 Depth=3
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%r14, (%rax)
	movq	%r12, %r8
.LBB2_27:                               # %if.end124
                                        #   in Loop: Header=BB2_7 Depth=3
	testq	%r13, %r13
	je	.LBB2_29
.LBB2_28:                               # %for.inc
                                        #   in Loop: Header=BB2_7 Depth=3
	incq	%r9
	movq	%r9, %rax
	andq	%r10, %rax
	incq	%rsi
	cmpq	$64, %rax
	jb	.LBB2_7
.LBB2_29:                               # %if.end130
                                        #   in Loop: Header=BB2_5 Depth=2
	incq	%r15
	movq	%r15, %rax
	andq	%r10, %rax
	xorq	%r9, %r9
	cmpq	$2, %rax
	jb	.LBB2_5
# BB#30:                                # %for.end133
                                        #   in Loop: Header=BB2_4 Depth=1
	movq	(%r8), %r8
	xorq	%r9, %r9
	testq	%r8, %r8
	jne	.LBB2_4
.LBB2_31:                               # %do.end
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_10:                               # %if.then25
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.compute_use_by_pseudos, %rdx
	movq	$573, %rsi              # imm = 0x23D
	callq	fancy_abort
.Ltmp37:
	.size	compute_use_by_pseudos, .Ltmp37-compute_use_by_pseudos
	.cfi_endproc

	.globl	reload
	.align	16, 0x90
	.type	reload,@function
reload:                                 # @reload
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
	subq	$528, %rsp              # imm = 0x210
.Ltmp49:
	.cfi_def_cfa_offset 576
.Ltmp50:
	.cfi_offset %rbx, -48
.Ltmp51:
	.cfi_offset %r12, -40
.Ltmp52:
	.cfi_offset %r13, -32
.Ltmp53:
	.cfi_offset %r14, -24
.Ltmp54:
	.cfi_offset %r15, -16
	movq	%rsi, 64(%rsp)          # 8-byte Spill
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	callq	init_recog
	movq	reload_obstack+32(%rip), %rax
	movq	reload_obstack+24(%rip), %rcx
	xorq	%rdx, %rdx
	movb	%dl, failure(%rip)
	cmpq	%rcx, %rax
	jns	.LBB3_2
# BB#1:                                 # %if.then
	movabsq	$reload_obstack, %rdi
	xorq	%rsi, %rsi
	callq	_obstack_newchunk
	movq	reload_obstack+24(%rip), %rcx
.LBB3_2:                                # %if.end
	movq	reload_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB3_4
# BB#3:                                 # %if.then8
	movzbl	reload_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, reload_obstack+80(%rip)
.LBB3_4:                                # %if.end9
	movslq	reload_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, reload_obstack+24(%rip)
	movq	reload_obstack+32(%rip), %rbx
	movq	reload_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB3_6
# BB#5:                                 # %if.then29
	movq	%rbx, reload_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB3_6:                                # %if.end32
	movq	%rdx, reload_obstack+16(%rip)
	movq	%rax, reload_firstobj(%rip)
	movabsq	$4294967197, %rsi       # imm = 0xFFFFFF9D
	xorq	%rdi, %rdi
	callq	emit_note
	callq	get_max_uid
	movl	%eax, reload_first_uid(%rip)
	callq	clear_secondary_mem
	movabsq	$spill_stack_slot, %rdi
	xorq	%rsi, %rsi
	movq	$424, %rdx              # imm = 0x1A8
	callq	memset
	movabsq	$spill_stack_slot_width, %rdi
	xorq	%rsi, %rsi
	movq	$212, %rdx
	callq	memset
	callq	init_save_areas
	movslq	max_regno(%rip), %rdi
	cmpq	$54, %rdi
	jl	.LBB3_9
# BB#7:
	movq	$53, %rbx
	.align	16, 0x90
.LBB3_8:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	callq	mark_home_live
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	movslq	max_regno(%rip), %rdi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdi, %rax
	jl	.LBB3_8
.LBB3_9:                                # %for.end
	movq	cfun(%rip), %rax
	movzbl	425(%rax), %eax
	testq	$1, %rax
	je	.LBB3_15
# BB#10:
	movq	$-53, %rax
	movq	$1, %rcx
	.align	16, 0x90
.LBB3_11:                               # %for.body45
                                        # =>This Inner Loop Header: Depth=1
	movzbl	call_used_regs+53(%rax), %edx
	testq	%rdx, %rdx
	jne	.LBB3_14
# BB#12:                                # %land.lhs.true
                                        #   in Loop: Header=BB3_11 Depth=1
	movzbl	fixed_regs+53(%rax), %edx
	testq	%rdx, %rdx
	jne	.LBB3_14
# BB#13:                                # %if.then50
                                        #   in Loop: Header=BB3_11 Depth=1
	movb	%cl, regs_ever_live+53(%rax)
.LBB3_14:                               # %for.inc54
                                        #   in Loop: Header=BB3_11 Depth=1
	incq	%rax
	jne	.LBB3_11
.LBB3_15:                               # %if.end57
	movq	$8, %rsi
	callq	xcalloc
	movslq	max_regno(%rip), %rdi
	movq	%rax, reg_equiv_constant(%rip)
	movq	$8, %rsi
	callq	xcalloc
	movslq	max_regno(%rip), %rdi
	movq	%rax, reg_equiv_mem(%rip)
	movq	$8, %rsi
	callq	xcalloc
	movslq	max_regno(%rip), %rdi
	movq	%rax, reg_equiv_init(%rip)
	movq	$8, %rsi
	callq	xcalloc
	movslq	max_regno(%rip), %rdi
	movq	%rax, reg_equiv_address(%rip)
	movq	$4, %rsi
	callq	xcalloc
	movslq	max_regno(%rip), %rdi
	movq	%rax, reg_max_ref_width(%rip)
	movq	$2, %rsi
	callq	xcalloc
	movslq	max_regno(%rip), %rbx
	movq	reg_renumber(%rip), %rsi
	movq	%rax, reg_old_renumber(%rip)
	leaq	(%rbx,%rbx), %rdx
	movq	%rax, %rdi
	callq	memcpy
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	%rbx, %rdi
	callq	xmalloc
	movslq	max_regno(%rip), %rdi
	movq	%rax, pseudo_forbidden_regs(%rip)
	movq	$8, %rsi
	callq	xcalloc
	movq	%rax, pseudo_previous_regs(%rip)
	movq	$0, bad_spill_regs_global(%rip)
	xorq	%rax, %rax
	movl	%eax, num_eliminable_invariants(%rip)
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB3_64
# BB#16:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	$1, %r13
	movq	56(%rsp), %rbx          # 8-byte Reload
	.align	16, 0x90
.LBB3_17:                               # %for.body78
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_27 Depth 2
	movl	(%rbx), %ecx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB3_18
# BB#19:                                # %cond.true
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	32(%rbx), %r14
	movzbl	(%r14), %eax
	cmpq	$47, %rax
	je	.LBB3_21
# BB#20:                                # %cond.false
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	single_set_2
	movl	(%rbx), %ecx
	movq	%rax, %r14
	jmp	.LBB3_21
	.align	16, 0x90
.LBB3_18:                               #   in Loop: Header=BB3_17 Depth=1
	xorq	%r14, %r14
.LBB3_21:                               # %cond.end101
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB3_25
# BB#22:                                # %land.lhs.true110
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	32(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	jne	.LBB3_25
# BB#23:                                # %land.lhs.true110
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	%rcx, %rax
	andq	$16711680, %rax         # imm = 0xFF0000
	testq	%r12, %rax
	movq	%r12, %rax
                                        # kill: R12<def> RAX<kill>
	je	.LBB3_25
# BB#24:                                # %if.then124
                                        #   in Loop: Header=BB3_17 Depth=1
	movabsq	$4278255615, %rax       # imm = 0xFF00FFFF
	andq	%rax, %rcx
	movl	%ecx, (%rbx)
.LBB3_25:                               # %if.end127
                                        #   in Loop: Header=BB3_17 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$34, %rcx
	jne	.LBB3_30
# BB#26:                                # %land.lhs.true132
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	$28, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	movq	$-53, %rax
	je	.LBB3_30
	.align	16, 0x90
.LBB3_27:                               # %for.body139
                                        #   Parent Loop BB3_17 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	call_used_regs+53(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB3_29
# BB#28:                                # %if.then143
                                        #   in Loop: Header=BB3_27 Depth=2
	movb	%r13b, regs_ever_live+53(%rax)
.LBB3_29:                               # %for.inc147
                                        #   in Loop: Header=BB3_27 Depth=2
	incq	%rax
	jne	.LBB3_27
.LBB3_30:                               # %if.end150
                                        #   in Loop: Header=BB3_17 Depth=1
	testq	%r14, %r14
	je	.LBB3_61
# BB#31:                                # %land.lhs.true153
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	8(%r14), %rdi
	movzbl	(%rdi), %eax
	cmpq	$66, %rax
	jne	.LBB3_32
# BB#57:                                # %land.lhs.true350
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	16(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB3_61
# BB#58:                                # %land.lhs.true358
                                        #   in Loop: Header=BB3_17 Depth=1
	movl	8(%rax), %eax
	movq	reg_equiv_memory_loc(%rip), %rcx
	movq	(%rcx,%rax,8), %rsi
	testq	%rsi, %rsi
	je	.LBB3_61
# BB#59:                                # %land.lhs.true368
                                        #   in Loop: Header=BB3_17 Depth=1
	callq	rtx_equal_p
	testq	%r12, %rax
	je	.LBB3_61
# BB#60:                                # %if.then382
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	16(%r14), %rax
	movl	8(%rax), %eax
	movq	reg_equiv_init(%rip), %rcx
	movq	(%rcx,%rax,8), %rcx
	movq	$4, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ue
	movq	16(%r14), %rcx
	movl	8(%rcx), %ecx
	movq	reg_equiv_init(%rip), %rdx
	movq	%rax, (%rdx,%rcx,8)
	jmp	.LBB3_61
	.align	16, 0x90
.LBB3_32:                               # %land.lhs.true153
                                        #   in Loop: Header=BB3_17 Depth=1
	cmpq	$61, %rax
	jne	.LBB3_61
# BB#33:                                # %if.then161
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	$3, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB3_61
# BB#34:                                # %land.lhs.true165
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	8(%r15), %rdi
	callq	function_invariant_p
	testq	%r12, %rax
	je	.LBB3_45
# BB#35:                                # %land.lhs.true165
                                        #   in Loop: Header=BB3_17 Depth=1
	movl	flag_pic(%rip), %eax
	testq	%r12, %rax
	je	.LBB3_45
# BB#36:                                # %lor.lhs.false172
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	8(%r15), %rdi
	movzbl	(%rdi), %eax
	cmpq	$133, %rax
	jg	.LBB3_43
# BB#37:                                # %lor.lhs.false172
                                        #   in Loop: Header=BB3_17 Depth=1
	leaq	-54(%rax), %rcx
	andq	%r12, %rcx
	cmpq	$3, %rcx
	jb	.LBB3_45
# BB#38:                                # %lor.lhs.false172
                                        #   in Loop: Header=BB3_17 Depth=1
	leaq	-67(%rax), %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	jb	.LBB3_42
# BB#39:                                # %lor.lhs.false172
                                        #   in Loop: Header=BB3_17 Depth=1
	cmpq	$58, %rax
	jne	.LBB3_61
# BB#40:                                # %land.lhs.true260
                                        #   in Loop: Header=BB3_17 Depth=1
	callq	symbolic_reference_mentioned_p
	testq	%r12, %rax
	je	.LBB3_45
# BB#41:                                # %land.lhs.true260.lor.lhs.false266_crit_edge
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	8(%r15), %rdi
.LBB3_42:                               # %lor.lhs.false266
                                        #   in Loop: Header=BB3_17 Depth=1
	callq	legitimate_pic_address_disp_p
	testq	%r12, %rax
	jne	.LBB3_45
	jmp	.LBB3_61
.LBB3_43:                               # %lor.lhs.false172
                                        #   in Loop: Header=BB3_17 Depth=1
	cmpq	$134, %rax
	je	.LBB3_45
# BB#44:                                # %lor.lhs.false172
                                        #   in Loop: Header=BB3_17 Depth=1
	cmpq	$140, %rax
	jne	.LBB3_61
.LBB3_45:                               # %if.then272
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	8(%r14), %rax
	movslq	8(%rax), %rax
	cmpq	$58, %rax
	jl	.LBB3_61
# BB#46:                                # %if.then284
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	8(%r15), %r15
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	memory_operand
	movq	%r15, %rdi
	testq	%r12, %rax
	jne	.LBB3_47
# BB#48:                                # %if.else
                                        #   in Loop: Header=BB3_17 Depth=1
	callq	function_invariant_p
	testq	%r12, %rax
	je	.LBB3_63
# BB#49:                                # %if.then293
                                        #   in Loop: Header=BB3_17 Depth=1
	movzbl	(%r15), %eax
	cmpq	$75, %rax
	jne	.LBB3_51
# BB#50:                                # %if.then298
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	%r15, %rdi
	callq	copy_rtx
	movq	reg_equiv_constant(%rip), %rcx
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%rax, (%rcx,%rdx,8)
	movq	%rdx, %rcx
	jmp	.LBB3_53
.LBB3_47:                               # %if.then287
                                        #   in Loop: Header=BB3_17 Depth=1
	callq	copy_rtx
	movq	reg_equiv_memory_loc(%rip), %rcx
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%rax, (%rcx,%rdx,8)
	movq	%rdx, %rcx
	jmp	.LBB3_54
.LBB3_51:                               # %if.else303
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	global_rtl+24(%rip), %rcx
	movq	reg_equiv_constant(%rip), %rdx
	movq	global_rtl+40(%rip), %rax
	cmpq	%rcx, %r15
	movq	152(%rsp), %rcx         # 8-byte Reload
	movq	%r15, (%rdx,%rcx,8)
	je	.LBB3_53
# BB#52:                                # %if.else303
                                        #   in Loop: Header=BB3_17 Depth=1
	cmpq	%rax, %r15
	jne	.LBB3_54
.LBB3_53:                               # %if.then309
                                        #   in Loop: Header=BB3_17 Depth=1
	movl	num_eliminable_invariants(%rip), %eax
	incq	%rax
	movl	%eax, num_eliminable_invariants(%rip)
.LBB3_54:                               # %if.end320
                                        #   in Loop: Header=BB3_17 Depth=1
	movzbl	(%r15), %eax
	cmpq	$66, %rax
	jne	.LBB3_56
# BB#55:                                # %lor.lhs.false325
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	16(%r14), %rdi
	movq	%r15, %rsi
	movq	%rcx, %r14
	callq	rtx_equal_p
	movq	%r14, %rcx
	testq	%r12, %rax
	je	.LBB3_61
.LBB3_56:                               # %if.then331
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	reg_equiv_init(%rip), %rax
	movq	%rcx, %r14
	movq	(%rax,%r14,8), %rcx
	movq	$4, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ue
	movq	reg_equiv_init(%rip), %rcx
	movq	%rax, (%rcx,%r14,8)
	.align	16, 0x90
.LBB3_61:                               # %if.end401
                                        #   in Loop: Header=BB3_17 Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB3_63
# BB#62:                                # %if.then409
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	32(%rbx), %rdi
	callq	scan_paradoxical_subregs
.LBB3_63:                               # %for.inc414
                                        #   in Loop: Header=BB3_17 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB3_17
.LBB3_64:                               # %for.end418
	movq	reg_eliminate(%rip), %rbx
	testq	%rbx, %rbx
	jne	.LBB3_66
# BB#65:                                # %if.then.i
	movq	$48, %rdi
	movq	$4, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movq	%rbx, reg_eliminate(%rip)
.LBB3_66:                               # %if.end.i
	movl	flag_omit_frame_pointer(%rip), %eax
	testq	%rax, %rax
	movq	$1, %rax
	je	.LBB3_72
# BB#67:                                # %lor.lhs.false.i
	movq	cfun(%rip), %rcx
	movzbl	424(%rcx), %ecx
	testq	$64, %rcx
	jne	.LBB3_72
# BB#68:                                # %lor.rhs.i
	callq	ix86_frame_pointer_required
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB3_69
# BB#70:                                # %lor.rhs.i
	movabsq	$1, %rax
	jmp	.LBB3_71
.LBB3_69:
	movabsq	$0, %rax
.LBB3_71:                               # %lor.rhs.i
	movq	reg_eliminate(%rip), %rbx
.LBB3_72:                               # %for.body.lr.ph.i
	movl	%eax, frame_pointer_needed(%rip)
	xorq	%rax, %rax
	movl	%eax, num_eliminable(%rip)
	leaq	192(%rbx), %r9
	movabsq	$reg_eliminate_1, %rcx
	movabsq	$0, %rax
	movabsq	$1, %r8
	movq	%rbx, %rdi
	.align	16, 0x90
.LBB3_73:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rcx), %edx
	movl	%edx, (%rdi)
	movl	4(%rcx), %edx
	cmpq	$7, %rdx
	movl	%edx, 4(%rdi)
	movq	%rax, %rdx
	jne	.LBB3_75
# BB#74:                                # %for.body.i
                                        #   in Loop: Header=BB3_73 Depth=1
	movq	%r8, %rdx
.LBB3_75:                               # %for.body.i
                                        #   in Loop: Header=BB3_73 Depth=1
	movl	frame_pointer_needed(%rip), %esi
	testq	%rsi, %rsi
	movq	%rax, %rsi
	je	.LBB3_77
# BB#76:                                # %for.body.i
                                        #   in Loop: Header=BB3_73 Depth=1
	movq	%r8, %rsi
.LBB3_77:                               # %for.body.i
                                        #   in Loop: Header=BB3_73 Depth=1
	andq	%rsi, %rdx
	xorq	$1, %rdx
	movl	%edx, 16(%rdi)
	movl	%edx, 12(%rdi)
	addq	$48, %rdi
	addq	$8, %rcx
	cmpq	%r9, %rdi
	jb	.LBB3_73
	.align	16, 0x90
.LBB3_78:                               # %for.body19.i
                                        # =>This Inner Loop Header: Depth=1
	movl	12(%rbx), %eax
	movl	num_eliminable(%rip), %ecx
	addq	%rax, %rcx
	movl	target_flags(%rip), %edi
	movl	%ecx, num_eliminable(%rip)
	movq	$25, %rcx
	movl	(%rbx), %esi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_rtx_REG
	movq	%rax, 32(%rbx)
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movl	4(%rbx), %esi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_rtx_REG
	movq	%rax, 40(%rbx)
	movq	reg_eliminate(%rip), %rax
	addq	$48, %rbx
	addq	$192, %rax
	cmpq	%rax, %rbx
	jb	.LBB3_78
# BB#79:                                # %init_elim_table.exit
	callq	max_label_num
	movq	%rax, %rbx
	callq	get_first_label_num
	subq	%rax, %rbx
	movl	%ebx, num_labels(%rip)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, %rdi
	callq	xmalloc
	movslq	num_labels(%rip), %rdi
	movq	%rax, %rbx
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	$4, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, %r14
	movq	%r14, 24(%rsp)          # 8-byte Spill
	callq	get_first_label_num
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rbx, %rcx
	subq	%rax, %rcx
	movq	%rcx, offsets_known_at(%rip)
	callq	get_first_label_num
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	movslq	max_regno(%rip), %rdx
	shlq	%cl, %rax
	movq	%r14, %rcx
	subq	%rax, %rcx
	movq	%rcx, offsets_at(%rip)
	cmpq	$58, %rdx
	jle	.LBB3_82
# BB#80:
	movq	$58, %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB3_81:                               # %for.body437
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	alter_reg
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	movslq	max_regno(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB3_81
.LBB3_82:                               # %for.cond441.preheader
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB3_89
# BB#83:                                # %for.cond441.preheader
	movl	num_eliminable(%rip), %eax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB3_89
# BB#84:
	movabsq	$mark_not_eliminable, %r14
	movq	56(%rsp), %rbx          # 8-byte Reload
	.align	16, 0x90
.LBB3_85:                               # %for.body444
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %ecx
	addq	$-32, %rcx
	andq	%r15, %rcx
	cmpq	$2, %rcx
	ja	.LBB3_87
# BB#86:                                # %if.then459
                                        #   in Loop: Header=BB3_85 Depth=1
	movq	32(%rbx), %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	note_stores
	movl	num_eliminable(%rip), %eax
.LBB3_87:                               # %for.inc464
                                        #   in Loop: Header=BB3_85 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB3_89
# BB#88:                                # %for.inc464
                                        #   in Loop: Header=BB3_85 Depth=1
	testq	%r15, %rax
	jne	.LBB3_85
.LBB3_89:                               # %for.end468
	movq	reload_insn_chain(%rip), %rax
	testq	%rax, %rax
	je	.LBB3_157
# BB#90:                                # %for.body.lr.ph.i1612
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB3_91:                               # %for.body.i1616
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_95 Depth 2
                                        #     Child Loop BB3_112 Depth 2
                                        #       Child Loop BB3_145 Depth 3
                                        #         Child Loop BB3_148 Depth 4
                                        #       Child Loop BB3_113 Depth 3
                                        #         Child Loop BB3_116 Depth 4
                                        #     Child Loop BB3_153 Depth 2
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	32(%rax), %rax
	movzbl	(%rax), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB3_156
# BB#92:                                # %lor.lhs.false.i1618
                                        #   in Loop: Header=BB3_91 Depth=1
	movq	32(%rax), %rdi
	callq	asm_noperands
	movq	%rax, %r14
	movq	$32, %rcx
                                        # kill: RAX<def> R14<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB3_156
# BB#93:                                # %if.end.i1619
                                        #   in Loop: Header=BB3_91 Depth=1
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rax
	movq	32(%rax), %rdi
	movzbl	(%rdi), %eax
	cmpq	$39, %rax
	jne	.LBB3_156
# BB#94:                                # %for.cond17.preheader.i
                                        #   in Loop: Header=BB3_91 Depth=1
	movq	8(%rdi), %r8
	movslq	(%r8), %rax
	xorq	%r12, %r12
	testq	%rax, %rax
	movq	$1, %rsi
	movabsq	$0, %r10
	movabsq	$4294967288, %r9        # imm = 0xFFFFFFF8
	jle	.LBB3_100
	.align	16, 0x90
.LBB3_95:                               # %for.body22.i
                                        #   Parent Loop BB3_91 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r8,%rsi,8), %rcx
	movzbl	(%rcx), %ebx
	cmpq	$49, %rbx
	jne	.LBB3_99
# BB#96:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB3_95 Depth=2
	movq	8(%rcx), %rcx
	movzbl	(%rcx), %ebx
	cmpq	$61, %rbx
	jne	.LBB3_99
# BB#97:                                # %land.lhs.true39.i
                                        #   in Loop: Header=BB3_95 Depth=2
	movl	8(%rcx), %ecx
	movq	%rcx, %rbx
	andq	%r9, %rbx
	cmpq	$8, %rbx
	jne	.LBB3_99
# BB#98:                                # %if.then56.i
                                        #   in Loop: Header=BB3_95 Depth=2
	movq	$1, %rbx
	shlq	%cl, %rbx
	orq	%rbx, %r10
	.align	16, 0x90
.LBB3_99:                               # %for.inc.i
                                        #   in Loop: Header=BB3_95 Depth=2
	movq	%rsi, %rcx
	andq	%r13, %rcx
	movq	%rax, %rdx
	andq	%r13, %rdx
	incq	%rsi
	cmpq	%rdx, %rcx
	jne	.LBB3_95
.LBB3_100:                              # %for.end.i
                                        #   in Loop: Header=BB3_91 Depth=1
	movq	%r10, 128(%rsp)         # 8-byte Spill
	movabsq	$recog_data, %rsi
	movabsq	$recog_data+240, %rdx
	leaq	288(%rsp), %rcx
	leaq	160(%rsp), %r8
	callq	decode_asm_operands
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$17626545782785, %rdx   # imm = 0x100800000001
	jle	.LBB3_152
# BB#101:                               # %for.body69.lr.ph.i
                                        #   in Loop: Header=BB3_91 Depth=1
	movl	target_flags(%rip), %r15d
	testq	$327680, %r15           # imm = 0x50000
	movabsq	$0, %rax
	je	.LBB3_103
# BB#102:                               # %for.body69.lr.ph.i
                                        #   in Loop: Header=BB3_91 Depth=1
	movabsq	$1, %rax
.LBB3_103:                              # %for.body69.lr.ph.i
                                        #   in Loop: Header=BB3_91 Depth=1
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	%r15, %rsi
	andq	$262144, %rsi           # imm = 0x40000
	movq	$14, %rcx
	shrq	%cl, %rsi
	movq	%rsi, 112(%rsp)         # 8-byte Spill
	movq	%r15, %rsi
	andq	$16384, %rsi            # imm = 0x4000
	movq	$14, %rcx
	shrq	%cl, %rsi
	movq	%rsi, 120(%rsp)         # 8-byte Spill
	testq	%rsi, %rsi
	je	.LBB3_105
# BB#104:                               # %for.body69.lr.ph.i
                                        #   in Loop: Header=BB3_91 Depth=1
	movq	$17, 120(%rsp)          # 8-byte Folded Spill
.LBB3_105:                              # %for.body69.lr.ph.i
                                        #   in Loop: Header=BB3_91 Depth=1
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	%r15, %rax
	andq	$1, %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	%r15, %rsi
	andq	$32, %rsi
	movq	$5, %rcx
	shrq	%cl, %rsi
	testq	%rsi, %rsi
	movq	%rsi, %rdi
	je	.LBB3_107
# BB#106:                               # %for.body69.lr.ph.i
                                        #   in Loop: Header=BB3_91 Depth=1
	movq	$15, %rdi
.LBB3_107:                              # %for.body69.lr.ph.i
                                        #   in Loop: Header=BB3_91 Depth=1
	movq	%rsi, 144(%rsp)         # 8-byte Spill
	movq	%rdx, %rbx
	movq	%r14, %r11
	je	.LBB3_109
# BB#108:                               # %for.body69.lr.ph.i
                                        #   in Loop: Header=BB3_91 Depth=1
	movq	$13, 144(%rsp)          # 8-byte Folded Spill
.LBB3_109:                              # %for.body69.lr.ph.i
                                        #   in Loop: Header=BB3_91 Depth=1
	je	.LBB3_111
# BB#110:                               # %for.body69.lr.ph.i
                                        #   in Loop: Header=BB3_91 Depth=1
	movq	$14, %rsi
.LBB3_111:                              # %for.body69.lr.ph.i
                                        #   in Loop: Header=BB3_91 Depth=1
	andq	$33554432, %r15         # imm = 0x2000000
	movq	$23, %rcx
	shrq	%cl, %r15
	orq	$8, %r15
	xorq	%r8, %r8
	xorq	%r12, %r12
	.align	16, 0x90
.LBB3_112:                              # %for.body69.i
                                        #   Parent Loop BB3_91 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_145 Depth 3
                                        #         Child Loop BB3_148 Depth 4
                                        #       Child Loop BB3_113 Depth 3
                                        #         Child Loop BB3_116 Depth 4
	movq	288(%rsp,%r8,8), %r9
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	movabsq	$0, %r14
	movabsq	$0, %r10
	jne	.LBB3_145
	jmp	.LBB3_113
	.align	16, 0x90
.LBB3_143:                              # %cond.end257.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	imulq	$100, %r14, %rax
	movl	reg_class_subunion(%rax,%rdx,4), %r14d
.LBB3_113:                              # %for.cond72.us.i
                                        #   Parent Loop BB3_91 Depth=1
                                        #     Parent Loop BB3_112 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB3_116 Depth 4
	movzbl	(%r9), %eax
	incq	%r9
	cmpq	$44, %rax
	ja	.LBB3_119
# BB#114:                               # %for.cond72.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	%rbx, %rdx
	je	.LBB3_119
# BB#115:                               # %if.then84.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	reg_class_contents(,%r14,8), %rcx
	cmpq	$35, %rax
	jne	.LBB3_118
.LBB3_116:                              # %do.body.us.i
                                        #   Parent Loop BB3_91 Depth=1
                                        #     Parent Loop BB3_112 Depth=2
                                        #       Parent Loop BB3_113 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movzbl	(%r9), %eax
	incq	%r9
	testq	%rax, %rax
	je	.LBB3_118
# BB#117:                               # %do.body.us.i
                                        #   in Loop: Header=BB3_116 Depth=4
	cmpq	$44, %rax
	jne	.LBB3_116
	.align	16, 0x90
.LBB3_118:                              # %if.end99.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	orq	%rcx, %r12
	testq	%rax, %rax
	movabsq	$0, %r14
	jne	.LBB3_113
	jmp	.LBB3_151
	.align	16, 0x90
.LBB3_119:                              # %if.end105.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	addq	$-33, %rdx
	andq	%r13, %rdx
	cmpq	$82, %rdx
	ja	.LBB3_122
# BB#120:                               # %if.end105.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	jmpq	*.LJTI3_1(,%rdx,8)
.LBB3_121:                              # %sw.bb107.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	imulq	$100, %r14, %rax
	movl	reg_class_subunion+48(%rax), %r14d
	jmp	.LBB3_113
	.align	16, 0x90
.LBB3_122:                              # %sw.default.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	movq	$12, %rdx
	cmpq	$114, %rax
	je	.LBB3_143
# BB#123:                               # %cond.false.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	movq	$11, %rdx
	cmpq	$82, %rax
	je	.LBB3_143
# BB#124:                               # %cond.false.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	cmpq	$113, %rax
	movq	%r15, %rdx
	je	.LBB3_143
# BB#125:                               # %cond.false127.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	cmpq	$101, %rdx
	jg	.LBB3_127
# BB#126:                               # %cond.false127.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	movq	$8, %rdx
	cmpq	$81, %rax
	jne	.LBB3_130
	jmp	.LBB3_143
.LBB3_127:                              # %cond.false127.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	cmpq	$102, %rax
	movq	%rdi, %rdx
	je	.LBB3_143
# BB#128:                               # %cond.false127.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	cmpq	$116, %rax
	movq	144(%rsp), %rdx         # 8-byte Reload
	je	.LBB3_143
# BB#129:                               # %cond.false127.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	cmpq	$117, %rax
	movq	%rsi, %rdx
	je	.LBB3_143
.LBB3_130:                              # %cond.false166.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	movq	$1, %rdx
	cmpq	$97, %rax
	je	.LBB3_143
# BB#131:                               # %cond.false171.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	movq	$4, %rdx
	cmpq	$98, %rax
	je	.LBB3_143
# BB#132:                               # %cond.false176.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	movq	$3, %rdx
	cmpq	$99, %rax
	je	.LBB3_143
# BB#133:                               # %cond.false181.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	cmpq	$119, %rdx
	jg	.LBB3_136
# BB#134:                               # %cond.false181.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	cmpq	$89, %rax
	movq	112(%rsp), %rdx         # 8-byte Reload
	je	.LBB3_143
# BB#135:                               # %cond.false181.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	movq	$2, %rdx
	cmpq	$100, %rax
	jne	.LBB3_138
	jmp	.LBB3_143
.LBB3_136:                              # %cond.false181.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	cmpq	$120, %rax
	movq	104(%rsp), %rdx         # 8-byte Reload
	je	.LBB3_143
# BB#137:                               # %cond.false181.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	cmpq	$121, %rax
	movq	120(%rsp), %rdx         # 8-byte Reload
	je	.LBB3_143
.LBB3_138:                              # %cond.false213.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	movq	$7, %rdx
	cmpq	$65, %rax
	je	.LBB3_143
# BB#139:                               # %cond.false218.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	movq	$6, %rdx
	cmpq	$68, %rax
	je	.LBB3_143
# BB#140:                               # %cond.false223.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	cmpq	$83, %rax
	movabsq	$0, %rax
	jne	.LBB3_142
# BB#141:                               # %cond.false223.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	movabsq	$1, %rax
.LBB3_142:                              # %cond.false223.us.i
                                        #   in Loop: Header=BB3_113 Depth=3
	leaq	(%rax,%rax,4), %rdx
	jmp	.LBB3_143
	.align	16, 0x90
.LBB3_199:                              # %cond.end257.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$32, %rcx
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	imulq	$100, %r10, %rax
	movl	reg_class_subunion(%rax,%rdx,4), %r10d
	jmp	.LBB3_145
.LBB3_144:                              # %sw.bb111.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$32, %rcx
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	imulq	$100, %r10, %rax
	movl	reg_class_subunion+48(%rax), %r10d
	.align	16, 0x90
.LBB3_145:                              # %for.cond72.i
                                        #   Parent Loop BB3_91 Depth=1
                                        #     Parent Loop BB3_112 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB3_148 Depth 4
	movzbl	(%r9), %eax
	incq	%r9
	cmpq	$44, %rax
	ja	.LBB3_173
# BB#146:                               # %for.cond72.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	%rbx, %rdx
	je	.LBB3_173
# BB#147:                               # %if.then84.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$32, %rcx
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	movq	reg_class_contents(,%r10,8), %rcx
	cmpq	$35, %rax
	jne	.LBB3_150
.LBB3_148:                              # %do.body.i
                                        #   Parent Loop BB3_91 Depth=1
                                        #     Parent Loop BB3_112 Depth=2
                                        #       Parent Loop BB3_145 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movzbl	(%r9), %eax
	incq	%r9
	testq	%rax, %rax
	je	.LBB3_150
# BB#149:                               # %do.body.i
                                        #   in Loop: Header=BB3_148 Depth=4
	cmpq	$44, %rax
	jne	.LBB3_148
	.align	16, 0x90
.LBB3_150:                              # %if.end99.i
                                        #   in Loop: Header=BB3_145 Depth=3
	orq	%rcx, %r12
	testq	%rax, %rax
	movabsq	$0, %r10
	jne	.LBB3_145
	jmp	.LBB3_151
	.align	16, 0x90
.LBB3_173:                              # %if.end105.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	addq	$-33, %rdx
	andq	%r13, %rdx
	cmpq	$82, %rdx
	ja	.LBB3_175
# BB#174:                               # %if.end105.i
                                        #   in Loop: Header=BB3_145 Depth=3
	jmpq	*.LJTI3_0(,%rdx,8)
	.align	16, 0x90
.LBB3_175:                              # %sw.default.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$12, %rdx
	cmpq	$114, %rax
	je	.LBB3_199
# BB#176:                               # %cond.false.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$11, %rdx
	cmpq	$82, %rax
	je	.LBB3_199
# BB#177:                               # %cond.false.i
                                        #   in Loop: Header=BB3_145 Depth=3
	cmpq	$113, %rax
	movq	%r15, %rdx
	je	.LBB3_199
# BB#178:                               # %cond.false127.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	cmpq	$101, %rdx
	jg	.LBB3_186
# BB#179:                               # %cond.false127.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$8, %rdx
	cmpq	$81, %rax
	je	.LBB3_199
	jmp	.LBB3_180
.LBB3_186:                              # %cond.false127.i
                                        #   in Loop: Header=BB3_145 Depth=3
	cmpq	$117, %rax
	je	.LBB3_191
# BB#187:                               # %cond.false127.i
                                        #   in Loop: Header=BB3_145 Depth=3
	cmpq	$116, %rax
	jne	.LBB3_188
# BB#190:                               # %cond.true146.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$13, %rdx
	jmp	.LBB3_199
.LBB3_191:                              # %cond.true158.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$14, %rdx
	jmp	.LBB3_199
.LBB3_188:                              # %cond.false127.i
                                        #   in Loop: Header=BB3_145 Depth=3
	cmpq	$102, %rax
	jne	.LBB3_180
# BB#189:                               # %cond.true136.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$15, %rdx
	jmp	.LBB3_199
.LBB3_180:                              # %cond.false166.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$1, %rdx
	cmpq	$97, %rax
	je	.LBB3_199
# BB#181:                               # %cond.false171.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$4, %rdx
	cmpq	$98, %rax
	je	.LBB3_199
# BB#182:                               # %cond.false176.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$3, %rdx
	cmpq	$99, %rax
	je	.LBB3_199
# BB#183:                               # %cond.false181.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	cmpq	$119, %rdx
	jg	.LBB3_192
# BB#184:                               # %cond.false181.i
                                        #   in Loop: Header=BB3_145 Depth=3
	cmpq	$89, %rax
	movq	112(%rsp), %rdx         # 8-byte Reload
	je	.LBB3_199
# BB#185:                               # %cond.false181.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$2, %rdx
	cmpq	$100, %rax
	jne	.LBB3_194
	jmp	.LBB3_199
.LBB3_192:                              # %cond.false181.i
                                        #   in Loop: Header=BB3_145 Depth=3
	cmpq	$121, %rax
	movq	120(%rsp), %rdx         # 8-byte Reload
	je	.LBB3_199
# BB#193:                               # %cond.false181.i
                                        #   in Loop: Header=BB3_145 Depth=3
	cmpq	$120, %rax
	movq	104(%rsp), %rdx         # 8-byte Reload
	je	.LBB3_199
.LBB3_194:                              # %cond.false213.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$7, %rdx
	cmpq	$65, %rax
	je	.LBB3_199
# BB#195:                               # %cond.false218.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movq	$6, %rdx
	cmpq	$68, %rax
	je	.LBB3_199
# BB#196:                               # %cond.false223.i
                                        #   in Loop: Header=BB3_145 Depth=3
	cmpq	$83, %rax
	movabsq	$0, %rax
	jne	.LBB3_198
# BB#197:                               # %cond.false223.i
                                        #   in Loop: Header=BB3_145 Depth=3
	movabsq	$1, %rax
.LBB3_198:                              # %cond.false223.i
                                        #   in Loop: Header=BB3_145 Depth=3
	leaq	(%rax,%rax,4), %rdx
	jmp	.LBB3_199
	.align	16, 0x90
.LBB3_151:                              # %for.inc264.i
                                        #   in Loop: Header=BB3_112 Depth=2
	incq	%r8
	movq	%r8, %rax
	andq	%r13, %rax
	movq	%r11, %rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	jne	.LBB3_112
.LBB3_152:                              # %for.end266.i
                                        #   in Loop: Header=BB3_91 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	andq	%rax, %r12
	movq	152(%rsp), %rax         # 8-byte Reload
	leaq	40(%rax), %rbx
	leaq	64(%rax), %r14
	xorq	%r15, %r15
	.align	16, 0x90
.LBB3_153:                              # %for.body271.i
                                        #   Parent Loop BB3_91 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rax
	movq	%r15, %rcx
	shlq	%cl, %rax
	testq	%rax, %r12
	je	.LBB3_155
# BB#154:                               # %if.then276.i
                                        #   in Loop: Header=BB3_153 Depth=2
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	bitmap_clear_bit
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	bitmap_clear_bit
.LBB3_155:                              # %for.inc278.i
                                        #   in Loop: Header=BB3_153 Depth=2
	incq	%r15
	cmpq	$53, %r15
	jne	.LBB3_153
	.align	16, 0x90
.LBB3_156:                              # %for.inc281.i
                                        #   in Loop: Header=BB3_91 Depth=1
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB3_91
.LBB3_157:                              # %for.body473.lr.ph
	movq	$0, insns_need_reload(%rip)
	xorq	%rcx, %rcx
	movq	reg_eliminate(%rip), %rax
	movb	%cl, something_needs_elimination(%rip)
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movl	%edx, last_spill_reg(%rip)
	movq	$0, used_spill_regs(%rip)
	movq	%rax, %rbx
	.align	16, 0x90
.LBB3_158:                              # %for.body473
                                        # =>This Inner Loop Header: Depth=1
	movl	12(%rbx), %ecx
	testq	%rcx, %rcx
	jne	.LBB3_160
# BB#159:                               # %if.then475
                                        #   in Loop: Header=BB3_158 Depth=1
	movl	(%rbx), %edi
	movq	%rdx, %r14
	callq	spill_hard_reg
	movq	%r14, %rdx
	movq	reg_eliminate(%rip), %rax
.LBB3_160:                              # %for.inc477
                                        #   in Loop: Header=BB3_158 Depth=1
	addq	$48, %rbx
	leaq	192(%rax), %rcx
	cmpq	%rcx, %rbx
	jb	.LBB3_158
# BB#161:                               # %for.end478
	movq	%rdx, %r12
	movl	frame_pointer_needed(%rip), %eax
	testq	%rax, %rax
	je	.LBB3_163
# BB#162:                               # %if.then480
	movq	$6, %rdi
	callq	spill_hard_reg
.LBB3_163:                              # %if.end481
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	finish_spills
	movq	$1, %rax
	xorq	%r13, %r13
	movl	%eax, reload_in_progress(%rip)
	jmp	.LBB3_164
.LBB3_516:                              # %if.else868
                                        #   in Loop: Header=BB3_164 Depth=1
	movq	%r10, %r12
	movabsq	$reload_obstack, %rdi
	callq	obstack_free
	.align	16, 0x90
.LBB3_164:                              # %for.cond483
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_167 Depth 2
                                        #     Child Loop BB3_169 Depth 2
                                        #     Child Loop BB3_201 Depth 2
                                        #     Child Loop BB3_240 Depth 2
                                        #       Child Loop BB3_241 Depth 3
                                        #         Child Loop BB3_274 Depth 4
                                        #     Child Loop BB3_301 Depth 2
                                        #     Child Loop BB3_308 Depth 2
                                        #       Child Loop BB3_311 Depth 3
                                        #       Child Loop BB3_317 Depth 3
                                        #     Child Loop BB3_323 Depth 2
                                        #     Child Loop BB3_340 Depth 2
                                        #     Child Loop BB3_344 Depth 2
                                        #       Child Loop BB3_346 Depth 3
                                        #       Child Loop BB3_368 Depth 3
                                        #         Child Loop BB3_370 Depth 4
                                        #         Child Loop BB3_374 Depth 4
                                        #       Child Loop BB3_381 Depth 3
                                        #         Child Loop BB3_383 Depth 4
                                        #         Child Loop BB3_387 Depth 4
                                        #       Child Loop BB3_393 Depth 3
                                        #         Child Loop BB3_402 Depth 4
                                        #           Child Loop BB3_408 Depth 5
                                        #         Child Loop BB3_400 Depth 4
                                        #           Child Loop BB3_424 Depth 5
                                        #         Child Loop BB3_462 Depth 4
                                        #           Child Loop BB3_464 Depth 5
                                        #           Child Loop BB3_468 Depth 5
                                        #         Child Loop BB3_475 Depth 4
                                        #           Child Loop BB3_477 Depth 5
                                        #           Child Loop BB3_481 Depth 5
                                        #         Child Loop BB3_486 Depth 4
                                        #     Child Loop BB3_503 Depth 2
                                        #       Child Loop BB3_505 Depth 3
	movq	cfun(%rip), %rax
	movl	400(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB3_166
# BB#165:                               # %if.then488
                                        #   in Loop: Header=BB3_164 Depth=1
	movq	$51, %rdi
	xorq	%rsi, %rsi
	callq	assign_stack_local
.LBB3_166:                              # %if.end491
                                        #   in Loop: Header=BB3_164 Depth=1
	callq	get_frame_size
	movq	reg_eliminate(%rip), %rbx
	movq	%rax, %r15
	.align	16, 0x90
.LBB3_167:                              # %for.body.i1630
                                        #   Parent Loop BB3_164 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	4(%rbx), %esi
	movl	(%rbx), %edi
	callq	ix86_initial_elimination_offset
	movl	%eax, 8(%rbx)
	movl	%eax, 20(%rbx)
	movl	%eax, 24(%rbx)
	movq	reg_eliminate(%rip), %rax
	addq	$192, %rax
	addq	$48, %rbx
	cmpq	%rax, %rbx
	jb	.LBB3_167
# BB#168:                               # %set_initial_elim_offsets.exit
                                        #   in Loop: Header=BB3_164 Depth=1
	movl	%r13d, num_not_at_initial_offset(%rip)
	callq	get_first_label_num
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	offsets_known_at(%rip), %rdi
	movslq	num_labels(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rax, %rdi
	xorq	%rsi, %rsi
	callq	memset
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	32(%rax), %rbx
	jmp	.LBB3_169
	.align	16, 0x90
.LBB3_172:                              # %for.inc.i1640
                                        #   in Loop: Header=BB3_169 Depth=2
	movq	16(%rbx), %rbx
.LBB3_169:                              # %set_initial_elim_offsets.exit
                                        #   Parent Loop BB3_164 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB3_200
# BB#170:                               # %for.body.i1637
                                        #   in Loop: Header=BB3_169 Depth=2
	movq	8(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB3_172
# BB#171:                               # %if.then.i1638
                                        #   in Loop: Header=BB3_169 Depth=2
	xorq	%rsi, %rsi
	movq	$1, %rdx
	callq	set_label_offsets
	jmp	.LBB3_172
	.align	16, 0x90
.LBB3_200:                              # %for.cond493.preheader
                                        #   in Loop: Header=BB3_164 Depth=1
	movslq	max_regno(%rip), %rax
	cmpq	$54, %rax
	movq	$53, %rbx
	jl	.LBB3_221
	.align	16, 0x90
.LBB3_201:                              # %for.body496
                                        #   Parent Loop BB3_164 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%rbx,2), %rax
	testq	%rax, %rax
	jns	.LBB3_220
# BB#202:                               # %land.lhs.true502
                                        #   in Loop: Header=BB3_201 Depth=2
	movq	reg_equiv_memory_loc(%rip), %rax
	movq	(%rax,%rbx,8), %rdi
	testq	%rdi, %rdi
	je	.LBB3_220
# BB#203:                               # %if.then506
                                        #   in Loop: Header=BB3_201 Depth=2
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	eliminate_regs
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	80(%rcx), %rcx
	movq	(%rcx,%rbx,8), %rcx
	movq	%rax, %r14
	movq	8(%r14), %rsi
	movzbl	2(%rcx), %edi
	callq	strict_memory_address_p
	movq	%r12, %rsi
	testq	%rsi, %rax
	jne	.LBB3_204
# BB#205:                               # %if.else527
                                        #   in Loop: Header=BB3_201 Depth=2
	movq	8(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$133, %rcx
	jg	.LBB3_215
# BB#206:                               # %if.else527
                                        #   in Loop: Header=BB3_201 Depth=2
	addq	$-54, %rcx
	andq	%rsi, %rcx
	cmpq	$21, %rcx
	ja	.LBB3_219
# BB#207:                               # %if.else527
                                        #   in Loop: Header=BB3_201 Depth=2
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB3_217
# BB#208:                               # %if.else527
                                        #   in Loop: Header=BB3_201 Depth=2
	cmpq	$7, %rcx
	jne	.LBB3_209
# BB#218:                               # %land.lhs.true599
                                        #   in Loop: Header=BB3_201 Depth=2
	movl	8(%rax), %ecx
	cmpq	$53, %rcx
	jb	.LBB3_217
	jmp	.LBB3_219
.LBB3_204:                              # %if.then522
                                        #   in Loop: Header=BB3_201 Depth=2
	movq	%rsi, %r12
	movq	reg_equiv_mem(%rip), %rax
	movq	%r14, (%rax,%rbx,8)
	movq	reg_equiv_address(%rip), %rax
	movq	$0, (%rax,%rbx,8)
	jmp	.LBB3_220
.LBB3_209:                              # %if.else527
                                        #   in Loop: Header=BB3_201 Depth=2
	cmpq	$21, %rcx
	jne	.LBB3_219
# BB#210:                               # %land.lhs.true616
                                        #   in Loop: Header=BB3_201 Depth=2
	movq	8(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB3_219
# BB#211:                               # %land.lhs.true627
                                        #   in Loop: Header=BB3_201 Depth=2
	movl	8(%rcx), %ecx
	cmpq	$52, %rcx
	ja	.LBB3_219
# BB#212:                               # %land.lhs.true639
                                        #   in Loop: Header=BB3_201 Depth=2
	movq	16(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$133, %rcx
	jg	.LBB3_215
# BB#213:                               # %land.lhs.true639
                                        #   in Loop: Header=BB3_201 Depth=2
	addq	$-54, %rcx
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	ja	.LBB3_219
# BB#214:                               # %land.lhs.true639
                                        #   in Loop: Header=BB3_201 Depth=2
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB3_217
	jmp	.LBB3_219
.LBB3_215:                              # %land.lhs.true639
                                        #   in Loop: Header=BB3_201 Depth=2
	cmpq	$134, %rcx
	je	.LBB3_217
# BB#216:                               # %land.lhs.true639
                                        #   in Loop: Header=BB3_201 Depth=2
	cmpq	$140, %rcx
	jne	.LBB3_219
.LBB3_217:                              # %if.then727
                                        #   in Loop: Header=BB3_201 Depth=2
	movq	%rsi, %r12
	movq	reg_equiv_address(%rip), %rcx
	movq	%rax, (%rcx,%rbx,8)
	movq	reg_equiv_mem(%rip), %rax
	movq	$0, (%rax,%rbx,8)
	jmp	.LBB3_220
.LBB3_219:                              # %if.else735
                                        #   in Loop: Header=BB3_201 Depth=2
	movq	reg_equiv_memory_loc(%rip), %rax
	movq	$0, (%rax,%rbx,8)
	movq	reg_equiv_init(%rip), %rax
	movq	$0, (%rax,%rbx,8)
	movq	%rbx, %rdi
	movq	%rsi, %r12
	callq	alter_reg
	.align	16, 0x90
.LBB3_220:                              # %for.inc743
                                        #   in Loop: Header=BB3_201 Depth=2
	incq	%rbx
	movslq	max_regno(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB3_201
.LBB3_221:                              # %for.end745
                                        #   in Loop: Header=BB3_164 Depth=1
	movl	caller_save_needed(%rip), %eax
	testq	%rax, %rax
	je	.LBB3_223
# BB#222:                               # %if.then747
                                        #   in Loop: Header=BB3_164 Depth=1
	callq	setup_save_areas
.LBB3_223:                              # %if.end748
                                        #   in Loop: Header=BB3_164 Depth=1
	callq	get_frame_size
	cmpq	%rax, %r15
	jne	.LBB3_164
# BB#224:                               # %if.end753
                                        #   in Loop: Header=BB3_164 Depth=1
	movl	caller_save_needed(%rip), %eax
	testq	%rax, %rax
	jne	.LBB3_226
# BB#225:                               # %if.end753.if.end821_crit_edge
                                        #   in Loop: Header=BB3_164 Depth=1
	movq	reload_obstack+32(%rip), %rbx
	movq	reload_obstack+24(%rip), %rax
	jmp	.LBB3_233
	.align	16, 0x90
.LBB3_226:                              # %if.then755
                                        #   in Loop: Header=BB3_164 Depth=1
	callq	save_call_clobbered_regs
	movq	reload_obstack+32(%rip), %rax
	movq	reload_obstack+24(%rip), %rdx
	cmpq	%rdx, %rax
	jns	.LBB3_228
# BB#227:                               # %if.then770
                                        #   in Loop: Header=BB3_164 Depth=1
	xorq	%rsi, %rsi
	movabsq	$reload_obstack, %rdi
	callq	_obstack_newchunk
	movq	reload_obstack+24(%rip), %rdx
.LBB3_228:                              # %if.end771
                                        #   in Loop: Header=BB3_164 Depth=1
	movq	reload_obstack+16(%rip), %rcx
	cmpq	%rcx, %rdx
	jne	.LBB3_230
# BB#229:                               # %if.then784
                                        #   in Loop: Header=BB3_164 Depth=1
	movzbl	reload_obstack+80(%rip), %eax
	orq	$2, %rax
	movb	%al, reload_obstack+80(%rip)
.LBB3_230:                              # %if.end789
                                        #   in Loop: Header=BB3_164 Depth=1
	movslq	reload_obstack+48(%rip), %rax
	addq	%rax, %rdx
	notq	%rax
	andq	%rdx, %rax
	movq	%rax, reload_obstack+24(%rip)
	movq	reload_obstack+32(%rip), %rbx
	movq	reload_obstack+8(%rip), %rsi
	movq	%rax, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rdx
	subq	%rsi, %rdx
	cmpq	%rdx, %rdi
	jle	.LBB3_232
# BB#231:                               # %if.then814
                                        #   in Loop: Header=BB3_164 Depth=1
	movq	%rbx, reload_obstack+24(%rip)
	movq	%rbx, %rax
.LBB3_232:                              # %if.end817
                                        #   in Loop: Header=BB3_164 Depth=1
	movq	%rax, reload_obstack+16(%rip)
	movq	%rcx, reload_firstobj(%rip)
.LBB3_233:                              # %if.end821
                                        #   in Loop: Header=BB3_164 Depth=1
	movb	%r13b, something_needs_elimination(%rip)
	cmpq	%rax, %rbx
	jns	.LBB3_235
# BB#234:                               # %if.then.i1644
                                        #   in Loop: Header=BB3_164 Depth=1
	xorq	%rsi, %rsi
	movabsq	$reload_obstack, %rdi
	callq	_obstack_newchunk
	movq	reload_obstack+24(%rip), %rax
.LBB3_235:                              # %if.end.i1645
                                        #   in Loop: Header=BB3_164 Depth=1
	movq	reload_obstack+16(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB3_237
# BB#236:                               # %if.then8.i
                                        #   in Loop: Header=BB3_164 Depth=1
	movzbl	reload_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, reload_obstack+80(%rip)
.LBB3_237:                              # %if.end9.i
                                        #   in Loop: Header=BB3_164 Depth=1
	movslq	reload_obstack+48(%rip), %rdx
	addq	%rdx, %rax
	notq	%rdx
	andq	%rax, %rdx
	movq	%rdx, reload_obstack+24(%rip)
	movq	reload_obstack+32(%rip), %rbx
	movq	reload_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB3_239
# BB#238:                               # %if.then29.i
                                        #   in Loop: Header=BB3_164 Depth=1
	movq	%rbx, reload_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB3_239:                              # %if.end32.i
                                        #   in Loop: Header=BB3_164 Depth=1
	movq	%r15, 136(%rsp)         # 8-byte Spill
	movq	reload_insn_chain(%rip), %r15
	movq	%rdx, reload_obstack+16(%rip)
	movq	%rcx, reload_insn_firstobj(%rip)
	testq	%r15, %r15
	movabsq	$insns_need_reload, %rax
	movq	%rax, %rcx
	movq	%rax, %r14
	je	.LBB3_300
.LBB3_240:                              # %for.body.lr.ph.i1649
                                        #   Parent Loop BB3_164 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_241 Depth 3
                                        #         Child Loop BB3_274 Depth 4
	movq	%rcx, 120(%rsp)         # 8-byte Spill
	movq	%r12, %rbx
	movq	%r15, %r12
	jmp	.LBB3_241
.LBB3_267:                              # %if.then217.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	152(%rsp), %r12         # 8-byte Reload
	movq	%r12, (%rax)
.LBB3_269:                              # %if.end220.i
                                        #   in Loop: Header=BB3_241 Depth=3
	testq	%r12, %r12
	je	.LBB3_271
# BB#270:                               # %if.then222.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	8(%r14), %rax
	movq	%rax, 8(%r12)
.LBB3_271:                              # %if.end225.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	unused_insn_chains(%rip), %rax
	movq	%rax, (%r14)
	movq	%r14, unused_insn_chains(%rip)
	jmp	.LBB3_253
	.align	16, 0x90
.LBB3_241:                              # %for.body.i1650
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_240 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB3_274 Depth 4
	movq	%r12, %r14
	movq	(%r14), %r12
	movq	32(%r14), %r15
	movl	%r13d, 96(%r14)
	movzbl	262(%r14), %eax
	andq	$248, %rax
	movb	%al, 262(%r14)
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$33, %rcx
	je	.LBB3_245
# BB#242:                               # %for.body.i1650
                                        #   in Loop: Header=BB3_241 Depth=3
	cmpq	$36, %rcx
	je	.LBB3_245
# BB#243:                               # %lor.lhs.false55.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB3_246
# BB#244:                               # %land.lhs.true.i1653
                                        #   in Loop: Header=BB3_241 Depth=3
	cmpq	$0, 56(%r15)
	je	.LBB3_246
	.align	16, 0x90
.LBB3_245:                              # %if.then64.i
                                        #   in Loop: Header=BB3_241 Depth=3
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%r15, %rsi
	callq	set_label_offsets
	movl	(%r15), %eax
.LBB3_246:                              # %if.end65.i
                                        #   in Loop: Header=BB3_241 Depth=3
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB3_253
# BB#247:                               # %if.then73.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	32(%r15), %r13
	movq	56(%r15), %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movzbl	(%r13), %eax
	cmpq	$47, %rax
	movq	%r13, %rax
	je	.LBB3_249
# BB#248:                               # %cond.false.i1655
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	single_set_2
.LBB3_249:                              # %cond.end110.i
                                        #   in Loop: Header=BB3_241 Depth=3
	testq	%rax, %rax
	je	.LBB3_255
# BB#250:                               # %land.lhs.true112.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB3_255
# BB#251:                               # %land.lhs.true120.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movl	8(%rax), %eax
	movq	reg_renumber(%rip), %rcx
	movswq	(%rcx,%rax,2), %rcx
	testq	%rcx, %rcx
	jns	.LBB3_255
# BB#252:                               # %land.lhs.true131.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	reg_equiv_constant(%rip), %rcx
	cmpq	$0, (%rcx,%rax,8)
	jne	.LBB3_253
	.align	16, 0x90
.LBB3_255:                              # %if.end142.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	%r12, 152(%rsp)         # 8-byte Spill
	movl	num_eliminable(%rip), %eax
	movl	num_eliminable_invariants(%rip), %ecx
	orq	%rax, %rcx
	xorq	%r12, %r12
	testq	%rbx, %rcx
	je	.LBB3_257
# BB#256:                               # %if.then146.i
                                        #   in Loop: Header=BB3_241 Depth=3
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	eliminate_regs_in_insn
	movq	%rax, %r12
.LBB3_257:                              # %if.end148.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movsbq	spill_indirect_levels(%rip), %rdx
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	movq	64(%rsp), %rcx          # 8-byte Reload
	movabsq	$spill_reg_order, %r8
	callq	find_reloads
	movl	flag_expensive_optimizations(%rip), %ecx
	movq	%rax, %rdx
	testq	%rcx, %rcx
	je	.LBB3_272
# BB#258:                               # %if.end148.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movl	n_reloads(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB3_272
# BB#259:                               # %if.then155.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movzbl	(%r15), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB3_272
# BB#260:                               # %cond.true165.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	32(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB3_262
# BB#261:                               # %cond.false177.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	%r15, %rdi
	movq	%rdx, 128(%rsp)         # 8-byte Spill
	movq	%rax, %rsi
	callq	single_set_2
	movq	128(%rsp), %rdx         # 8-byte Reload
.LBB3_262:                              # %cond.end185.i
                                        #   in Loop: Header=BB3_241 Depth=3
	testq	%rax, %rax
	je	.LBB3_272
# BB#263:                               # %land.lhs.true188.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	8(%rax), %rcx
	movq	16(%rax), %rax
	cmpq	%rcx, %rax
	jne	.LBB3_272
# BB#264:                               # %land.lhs.true197.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB3_272
# BB#265:                               # %land.lhs.true205.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movl	8(%rax), %eax
	cmpq	$53, %rax
	jb	.LBB3_272
# BB#266:                               # %if.then214.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	%r15, %rdi
	callq	delete_insn
	movq	8(%r14), %rax
	testq	%rax, %rax
	jne	.LBB3_267
# BB#268:                               # %if.else.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	152(%rsp), %r12         # 8-byte Reload
	movq	%r12, reload_insn_chain(%rip)
	jmp	.LBB3_269
	.align	16, 0x90
.LBB3_272:                              # %if.end228.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	%rdx, %rdi
	movl	num_eliminable(%rip), %eax
	testq	%rax, %rax
	je	.LBB3_278
# BB#273:                               # %if.then230.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	reg_eliminate(%rip), %rax
	xorq	%rcx, %rcx
	movl	%ecx, num_not_at_initial_offset(%rip)
	leaq	192(%rax), %rcx
	.align	16, 0x90
.LBB3_274:                              # %for.body.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_240 Depth=2
                                        #       Parent Loop BB3_241 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movl	20(%rax), %edx
	movl	%edx, 24(%rax)
	movl	12(%rax), %esi
	testq	%rsi, %rsi
	je	.LBB3_277
# BB#275:                               # %land.lhs.true.i.i
                                        #   in Loop: Header=BB3_274 Depth=4
	movl	8(%rax), %esi
	andq	%rbx, %rdx
	cmpq	%rsi, %rdx
	je	.LBB3_277
# BB#276:                               # %if.then.i.i
                                        #   in Loop: Header=BB3_274 Depth=4
	movl	num_not_at_initial_offset(%rip), %edx
	incq	%rdx
	movl	%edx, num_not_at_initial_offset(%rip)
.LBB3_277:                              # %for.inc.i.i
                                        #   in Loop: Header=BB3_274 Depth=4
	addq	$48, %rax
	cmpq	%rcx, %rax
	jb	.LBB3_274
.LBB3_278:                              # %if.end231.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movzbl	262(%r14), %eax
	movq	$2, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	andq	$4, %rdx
	andq	$-5, %rax
	orq	%rdx, %rax
	movb	%al, 262(%r14)
	movslq	n_reloads(%rip), %rcx
	testq	%rcx, %rcx
	movabsq	$0, %rcx
	jle	.LBB3_280
# BB#279:                               # %if.end231.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movabsq	$1, %rcx
.LBB3_280:                              # %if.end231.i
                                        #   in Loop: Header=BB3_241 Depth=3
	andq	$252, %rax
	leaq	(%rdi,%rdi), %rdx
	andq	$2, %rdx
	orq	%rcx, %rdx
	orq	%rax, %rdx
	testq	%rbx, %r12
	movb	%dl, 262(%r14)
	je	.LBB3_286
# BB#281:                               # %if.then252.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	%rdi, %r12
	movq	reload_insn_firstobj(%rip), %rsi
	movq	reload_obstack+8(%rip), %rax
	cmpq	%rax, %rsi
	jbe	.LBB3_284
# BB#282:                               # %if.then252.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	reload_obstack+32(%rip), %rax
	cmpq	%rax, %rsi
	jae	.LBB3_284
# BB#283:                               # %if.then263.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	%rsi, reload_obstack+16(%rip)
	movq	%rsi, reload_obstack+24(%rip)
	jmp	.LBB3_285
.LBB3_284:                              # %if.else266.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movabsq	$reload_obstack, %rdi
	callq	obstack_free
.LBB3_285:                              # %if.end267.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movq	%r13, 32(%r15)
	movl	40(%r15), %eax
	movl	%eax, 40(%r15)
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	%rax, 56(%r15)
	movq	$1, %rax
	movb	%al, something_needs_elimination(%rip)
	movq	%r12, %rdi
.LBB3_286:                              # %if.end277.i
                                        #   in Loop: Header=BB3_241 Depth=3
	movl	something_needs_operands_changed(%rip), %ecx
	movl	n_reloads(%rip), %eax
	orq	%rdi, %rcx
	movl	%ecx, something_needs_operands_changed(%rip)
	testq	%rax, %rax
	movq	152(%rsp), %r12         # 8-byte Reload
	jne	.LBB3_287
.LBB3_253:                              # %for.cond.backedge.i
                                        #   in Loop: Header=BB3_241 Depth=3
	testq	%r12, %r12
	movabsq	$0, %r13
	jne	.LBB3_241
	jmp	.LBB3_254
	.align	16, 0x90
.LBB3_287:                              # %if.then280.i
                                        #   in Loop: Header=BB3_240 Depth=2
	movq	%r12, %r15
	movq	%rbx, %r12
	movl	%eax, 96(%r14)
	movl	n_reloads(%rip), %eax
	imulq	$104, %rax, %rsi
	movq	reload_obstack+32(%rip), %rdx
	movq	reload_obstack+24(%rip), %rax
	subq	%rax, %rdx
	movq	$32, %rcx
	movq	%rsi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rdx
	jge	.LBB3_289
# BB#288:                               # %if.then.i370.i
                                        #   in Loop: Header=BB3_240 Depth=2
	movabsq	$reload_obstack, %rdi
	callq	_obstack_newchunk
	movq	reload_obstack+24(%rip), %rax
.LBB3_289:                              # %if.end.i.i
                                        #   in Loop: Header=BB3_240 Depth=2
	addq	%rbx, %rax
	movq	%rax, reload_obstack+24(%rip)
	movq	reload_obstack+16(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB3_291
# BB#290:                               # %if.then10.i.i
                                        #   in Loop: Header=BB3_240 Depth=2
	movzbl	reload_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, reload_obstack+80(%rip)
.LBB3_291:                              # %if.end11.i.i
                                        #   in Loop: Header=BB3_240 Depth=2
	movslq	reload_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, reload_obstack+24(%rip)
	movq	reload_obstack+32(%rip), %rbx
	movq	reload_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rbx, %rax
	subq	%rdx, %rax
	cmpq	%rax, %rsi
	movabsq	$0, %r13
	jle	.LBB3_293
# BB#292:                               # %if.then31.i.i
                                        #   in Loop: Header=BB3_240 Depth=2
	movq	%rbx, reload_obstack+24(%rip)
	movq	%rbx, %rcx
.LBB3_293:                              # %if.end34.i.i
                                        #   in Loop: Header=BB3_240 Depth=2
	movq	%rcx, reload_obstack+16(%rip)
	movq	%rdi, 88(%r14)
	movslq	n_reloads(%rip), %rax
	imulq	$104, %rax, %rdx
	movabsq	$rld, %rsi
	callq	memcpy
	movq	reload_obstack+32(%rip), %rax
	movq	reload_obstack+24(%rip), %rcx
	cmpq	%rcx, %rax
	jns	.LBB3_295
# BB#294:                               # %if.then55.i.i
                                        #   in Loop: Header=BB3_240 Depth=2
	xorq	%rsi, %rsi
	movabsq	$reload_obstack, %rdi
	callq	_obstack_newchunk
	movq	reload_obstack+24(%rip), %rcx
.LBB3_295:                              # %if.end56.i.i
                                        #   in Loop: Header=BB3_240 Depth=2
	movq	reload_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB3_297
# BB#296:                               # %if.then69.i.i
                                        #   in Loop: Header=BB3_240 Depth=2
	movzbl	reload_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, reload_obstack+80(%rip)
.LBB3_297:                              # %if.end74.i.i
                                        #   in Loop: Header=BB3_240 Depth=2
	movslq	reload_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, reload_obstack+24(%rip)
	movq	reload_obstack+32(%rip), %rbx
	movq	reload_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB3_299
# BB#298:                               # %if.then99.i.i
                                        #   in Loop: Header=BB3_240 Depth=2
	movq	%rbx, reload_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB3_299:                              # %copy_reloads.exit.i
                                        #   in Loop: Header=BB3_240 Depth=2
	movq	%rdx, reload_obstack+16(%rip)
	movq	%rax, reload_insn_firstobj(%rip)
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	%r14, (%rax)
	addq	$16, %r14
	testq	%r15, %r15
	movq	%r14, %rcx
	jne	.LBB3_240
	jmp	.LBB3_300
	.align	16, 0x90
.LBB3_254:                              #   in Loop: Header=BB3_164 Depth=1
	movq	%rbx, %r12
	movq	120(%rsp), %r14         # 8-byte Reload
.LBB3_300:                              # %calculate_needs_all_insns.exit
                                        #   in Loop: Header=BB3_164 Depth=1
	movq	$0, (%r14)
	movabsq	$spilled_pseudos, %rdi
	callq	bitmap_clear
	callq	get_frame_size
	movl	frame_pointer_needed(%rip), %r14d
	movq	reg_eliminate(%rip), %rbx
	movq	%rax, %r15
	movq	%r12, %r10
	.align	16, 0x90
.LBB3_301:                              # %for.body.i1666
                                        #   Parent Loop BB3_164 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rbx), %eax
	cmpq	$6, %rax
	jne	.LBB3_303
# BB#302:                               # %land.lhs.true.i1669
                                        #   in Loop: Header=BB3_301 Depth=2
	movq	%r10, %r12
	callq	ix86_frame_pointer_required
	movq	%r12, %r10
	testq	%r10, %rax
	jne	.LBB3_305
.LBB3_303:                              # %lor.lhs.false.i1671
                                        #   in Loop: Header=BB3_301 Depth=2
	movl	4(%rbx), %eax
	cmpq	$7, %rax
	jne	.LBB3_306
# BB#304:                               # %lor.lhs.false.i1671
                                        #   in Loop: Header=BB3_301 Depth=2
	movl	frame_pointer_needed(%rip), %eax
	testq	%r10, %rax
	je	.LBB3_306
.LBB3_305:                              # %if.then.i1673
                                        #   in Loop: Header=BB3_301 Depth=2
	movl	%r13d, 12(%rbx)
.LBB3_306:                              # %for.inc.i1677
                                        #   in Loop: Header=BB3_301 Depth=2
	movq	reg_eliminate(%rip), %r9
	addq	$48, %rbx
	leaq	192(%r9), %rdx
	cmpq	%rdx, %rbx
	jb	.LBB3_301
# BB#307:                               #   in Loop: Header=BB3_164 Depth=1
	movq	%r9, %rcx
	.align	16, 0x90
.LBB3_308:                              # %for.body7.i
                                        #   Parent Loop BB3_164 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_311 Depth 3
                                        #       Child Loop BB3_317 Depth 3
	movl	12(%rcx), %esi
	testq	%rsi, %rsi
	jne	.LBB3_321
# BB#309:                               # %land.lhs.true10.i
                                        #   in Loop: Header=BB3_308 Depth=2
	movl	16(%rcx), %esi
	testq	%rsi, %rsi
	je	.LBB3_321
# BB#310:                               # %for.body16.lr.ph.i
                                        #   in Loop: Header=BB3_308 Depth=2
	movl	(%rcx), %r8d
	movq	%r9, %rsi
	.align	16, 0x90
.LBB3_311:                              # %for.body16.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_308 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rsi), %ebx
	movq	%r8, %rdi
	andq	%r10, %rdi
	cmpq	%rdi, %rbx
	jne	.LBB3_314
# BB#312:                               # %land.lhs.true20.i
                                        #   in Loop: Header=BB3_311 Depth=3
	movl	12(%rsi), %edi
	testq	%rdi, %rdi
	jne	.LBB3_313
.LBB3_314:                              # %for.inc26.i
                                        #   in Loop: Header=BB3_311 Depth=3
	addq	$48, %rsi
	cmpq	%rdx, %rsi
	jb	.LBB3_311
# BB#315:                               #   in Loop: Header=BB3_308 Depth=2
	movq	%r10, %rax
	jmp	.LBB3_316
.LBB3_313:                              # %if.then23.i
                                        #   in Loop: Header=BB3_308 Depth=2
	movl	4(%rsi), %eax
.LBB3_316:                              # %for.body32.lr.ph.i
                                        #   in Loop: Header=BB3_308 Depth=2
	movq	%r9, %rdi
	.align	16, 0x90
.LBB3_317:                              # %for.body32.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_308 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rdi), %ebx
	movq	%rax, %rsi
	andq	%r10, %rsi
	cmpq	%rsi, %rbx
	jne	.LBB3_320
# BB#318:                               # %land.lhs.true35.i
                                        #   in Loop: Header=BB3_317 Depth=3
	movl	4(%rdi), %esi
	movl	4(%rcx), %ebx
	cmpq	%rbx, %rsi
	jne	.LBB3_320
# BB#319:                               # %if.then39.i
                                        #   in Loop: Header=BB3_317 Depth=3
	movl	%r13d, 12(%rdi)
.LBB3_320:                              # %for.inc42.i
                                        #   in Loop: Header=BB3_317 Depth=3
	addq	$48, %rdi
	cmpq	%rdx, %rdi
	jb	.LBB3_317
.LBB3_321:                              # %for.inc46.i
                                        #   in Loop: Header=BB3_308 Depth=2
	addq	$48, %rcx
	cmpq	%rdx, %rcx
	jb	.LBB3_308
# BB#322:                               # %for.body52.lr.ph.i
                                        #   in Loop: Header=BB3_164 Depth=1
	movq	$1, %rax
	movl	%eax, frame_pointer_needed(%rip)
	xorq	%r12, %r12
	.align	16, 0x90
.LBB3_323:                              # %for.body52.i
                                        #   Parent Loop BB3_164 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	12(%r9), %ecx
	testq	%rcx, %rcx
	je	.LBB3_328
# BB#324:                               # %land.lhs.true55.i
                                        #   in Loop: Header=BB3_323 Depth=2
	movl	(%r9), %eax
	cmpq	$20, %rax
	jne	.LBB3_327
# BB#325:                               # %land.lhs.true58.i
                                        #   in Loop: Header=BB3_323 Depth=2
	movl	4(%r9), %eax
	cmpq	$6, %rax
	je	.LBB3_327
# BB#326:                               # %if.then61.i
                                        #   in Loop: Header=BB3_323 Depth=2
	movl	%r13d, frame_pointer_needed(%rip)
	movl	12(%r9), %ecx
.LBB3_327:                              # %if.end62.i
                                        #   in Loop: Header=BB3_323 Depth=2
	testq	%r10, %rcx
	jne	.LBB3_330
.LBB3_328:                              # %land.lhs.true65.i
                                        #   in Loop: Header=BB3_323 Depth=2
	movl	16(%r9), %eax
	testq	%rax, %rax
	je	.LBB3_330
# BB#329:                               # %if.then68.i
                                        #   in Loop: Header=BB3_323 Depth=2
	movl	%r13d, 16(%r9)
	movl	(%r9), %ecx
	movl	num_eliminable(%rip), %eax
	movq	$1, %rsi
	shlq	%cl, %rsi
	orq	%rsi, %r12
	decq	%rax
	movl	%eax, num_eliminable(%rip)
.LBB3_330:                              # %for.inc72.i
                                        #   in Loop: Header=BB3_323 Depth=2
	addq	$48, %r9
	cmpq	%rdx, %r9
	jb	.LBB3_323
# BB#331:                               # %for.end74.i
                                        #   in Loop: Header=BB3_164 Depth=1
	cmpq	%r15, 136(%rsp)         # 8-byte Folded Reload
	movabsq	$0, %r15
	movq	%r15, %rdx
	je	.LBB3_333
# BB#332:                               # %for.end74.i
                                        #   in Loop: Header=BB3_164 Depth=1
	movabsq	$1, %rax
	movq	%rax, %rdx
.LBB3_333:                              # %for.end74.i
                                        #   in Loop: Header=BB3_164 Depth=1
	movl	frame_pointer_needed(%rip), %eax
	testq	%rax, %rax
	movq	%r15, %rax
	jne	.LBB3_335
# BB#334:                               # %for.end74.i
                                        #   in Loop: Header=BB3_164 Depth=1
	movabsq	$1, %rax
.LBB3_335:                              # %for.end74.i
                                        #   in Loop: Header=BB3_164 Depth=1
	testq	%r10, %r14
	movq	%r15, %rcx
	je	.LBB3_337
# BB#336:                               # %for.end74.i
                                        #   in Loop: Header=BB3_164 Depth=1
	movabsq	$1, %rcx
.LBB3_337:                              # %for.end74.i
                                        #   in Loop: Header=BB3_164 Depth=1
	orq	%rcx, %rax
	jne	.LBB3_339
# BB#338:                               # %for.end74.i
                                        #   in Loop: Header=BB3_164 Depth=1
	orq	$64, %r12
.LBB3_339:                              # %for.end74.i
                                        #   in Loop: Header=BB3_164 Depth=1
	xorq	%rbx, %rbx
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB3_340:                              # %for.body831
                                        #   Parent Loop BB3_164 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	testq	%r12, %rax
	je	.LBB3_342
# BB#341:                               # %if.then834
                                        #   in Loop: Header=BB3_340 Depth=2
	movq	%rbx, %rdi
	movq	%r10, %r14
	callq	spill_hard_reg
	movq	%r14, %r10
	movq	$1, %rsi
	movq	$1, %rdx
.LBB3_342:                              # %for.inc836
                                        #   in Loop: Header=BB3_340 Depth=2
	incq	%rbx
	cmpq	$53, %rbx
	jne	.LBB3_340
# BB#343:                               # %for.end838
                                        #   in Loop: Header=BB3_164 Depth=1
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	insns_need_reload(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB3_495
	jmp	.LBB3_344
.LBB3_489:                              # %if.then142.i.i
                                        #   in Loop: Header=BB3_344 Depth=2
	movq	%r9, %r12
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %r15
	movq	32(%r15), %rdi
	movq	88(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %r14d
	callq	asm_noperands
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	spill_failure.reg_class_names(,%r14,8), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB3_612
# BB#490:                               # %spill_failure.exit.i.i
                                        #   in Loop: Header=BB3_344 Depth=2
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str38, %rsi
	callq	error_for_asm
	movq	$1, %rax
	movb	%al, failure(%rip)
	xorq	%r13, %r13
	movq	96(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB3_494
	.align	16, 0x90
.LBB3_344:                              # %for.body.i1689
                                        #   Parent Loop BB3_164 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_346 Depth 3
                                        #       Child Loop BB3_368 Depth 3
                                        #         Child Loop BB3_370 Depth 4
                                        #         Child Loop BB3_374 Depth 4
                                        #       Child Loop BB3_381 Depth 3
                                        #         Child Loop BB3_383 Depth 4
                                        #         Child Loop BB3_387 Depth 4
                                        #       Child Loop BB3_393 Depth 3
                                        #         Child Loop BB3_402 Depth 4
                                        #           Child Loop BB3_408 Depth 5
                                        #         Child Loop BB3_400 Depth 4
                                        #           Child Loop BB3_424 Depth 5
                                        #         Child Loop BB3_462 Depth 4
                                        #           Child Loop BB3_464 Depth 5
                                        #           Child Loop BB3_468 Depth 5
                                        #         Child Loop BB3_475 Depth 4
                                        #           Child Loop BB3_477 Depth 5
                                        #           Child Loop BB3_481 Depth 5
                                        #         Child Loop BB3_486 Depth 4
	movslq	96(%rbx), %r14
	movq	88(%rbx), %rsi
	movq	%rbx, %r12
	testq	%r14, %r14
	jle	.LBB3_363
# BB#345:                               # %for.body.i.i1691.preheader
                                        #   in Loop: Header=BB3_344 Depth=2
	leaq	64(%rsi), %rdi
	movq	$1, %rbx
	.align	16, 0x90
.LBB3_346:                              # %for.body.i.i1691
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rdi), %rax
	leaq	-1(%rbx), %r8
	testq	%rax, %rax
	jne	.LBB3_347
# BB#361:                               # %if.else.i.i
                                        #   in Loop: Header=BB3_346 Depth=3
	movl	%r10d, -8(%rdi)
	jmp	.LBB3_362
	.align	16, 0x90
.LBB3_347:                              # %if.then.i.i1693
                                        #   in Loop: Header=BB3_346 Depth=3
	movl	8(%rax), %ecx
	movl	%ecx, -8(%rdi)
	movzbl	2(%rax), %r11d
	movq	%rcx, %rdx
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rax, %rdx
	cmpq	$8, %rdx
	je	.LBB3_351
# BB#348:                               # %if.then.i.i1693
                                        #   in Loop: Header=BB3_346 Depth=3
	leaq	-21(%rcx), %rdx
	andq	%r10, %rdx
	cmpq	$8, %rdx
	jb	.LBB3_351
# BB#349:                               # %if.then.i.i1693
                                        #   in Loop: Header=BB3_346 Depth=3
	leaq	-45(%rcx), %rdx
	andq	%r10, %rdx
	cmpq	$8, %rdx
	jb	.LBB3_351
# BB#350:                               # %if.then.i.i1693
                                        #   in Loop: Header=BB3_346 Depth=3
	addq	$-29, %rcx
	andq	%r10, %rcx
	cmpq	$7, %rcx
	jbe	.LBB3_351
# BB#355:                               # %cond.false.i.i
                                        #   in Loop: Header=BB3_346 Depth=3
	cmpq	$24, %r11
	jne	.LBB3_356
# BB#358:                               # %cond.true60.i.i
                                        #   in Loop: Header=BB3_346 Depth=3
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	movl	%eax, -32(%rdi)
	jmp	.LBB3_362
	.align	16, 0x90
.LBB3_351:                              # %cond.true.i.i
                                        #   in Loop: Header=BB3_346 Depth=3
	movl	mode_class(,%r11,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB3_360
# BB#352:                               # %lor.rhs.i.i
                                        #   in Loop: Header=BB3_346 Depth=3
	cmpq	$6, %rcx
	movq	%r15, %rax
	jne	.LBB3_354
# BB#353:                               # %lor.rhs.i.i
                                        #   in Loop: Header=BB3_346 Depth=3
	movabsq	$1, %rax
.LBB3_354:                              # %lor.rhs.i.i
                                        #   in Loop: Header=BB3_346 Depth=3
	incq	%rax
	movl	%eax, -32(%rdi)
	jmp	.LBB3_362
.LBB3_356:                              # %cond.false.i.i
                                        #   in Loop: Header=BB3_346 Depth=3
	cmpq	$18, %r11
	jne	.LBB3_359
# BB#357:                               # %cond.true48.i.i
                                        #   in Loop: Header=BB3_346 Depth=3
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	movl	%eax, -32(%rdi)
	jmp	.LBB3_362
.LBB3_359:                              # %cond.false64.i.i
                                        #   in Loop: Header=BB3_346 Depth=3
	movl	target_flags(%rip), %r9d
	movzbl	mode_size(%r11), %eax
	movq	$23, %rcx
	shrq	%cl, %r9
	andq	$4, %r9
	leaq	3(%rax,%r9), %rax
	addq	$4, %r9
	cqto
	idivq	%r9
.LBB3_360:                              # %cond.end83.i.i
                                        #   in Loop: Header=BB3_346 Depth=3
	movl	%eax, -32(%rdi)
	.align	16, 0x90
.LBB3_362:                              # %if.end.i.i1699
                                        #   in Loop: Header=BB3_346 Depth=3
	movw	%r8w, reload_order-2(%rbx,%rbx)
	movq	%rbx, %rax
	andq	%r10, %rax
	movq	%r14, %rcx
	andq	%r10, %rcx
	incq	%rbx
	addq	$104, %rdi
	cmpq	%rcx, %rax
	jne	.LBB3_346
.LBB3_363:                              # %for.end.i.i
                                        #   in Loop: Header=BB3_344 Depth=2
	movq	%r10, %rbx
	movl	%r14d, n_reloads(%rip)
	imulq	$104, %r14, %rdx
	movabsq	$rld, %rdi
	callq	memcpy
	movq	rtl_dump_file(%rip), %rdi
	movq	$0, used_spill_regs_local(%rip)
	testq	%rdi, %rdi
	je	.LBB3_365
# BB#364:                               # %if.then99.i.i1701
                                        #   in Loop: Header=BB3_344 Depth=2
	movq	32(%r12), %rax
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str12, %rsi
	callq	fprintf
	movl	n_reloads(%rip), %r14d
.LBB3_365:                              # %if.end102.i.i
                                        #   in Loop: Header=BB3_344 Depth=2
	movq	$2, %rdx
	movabsq	$reload_order, %rdi
	movq	%r14, %rsi
	movabsq	$reload_reg_class_lower, %rcx
	callq	specqsort
	movq	fixed_reg_set(%rip), %rax
	movq	%rax, bad_spill_regs(%rip)
	xorq	%rsi, %rsi
	movq	$212, %rdx
	movabsq	$spill_cost, %rdi
	callq	memset
	xorq	%rsi, %rsi
	movq	$212, %rdx
	movabsq	$spill_add_cost, %rdi
	callq	memset
	movq	$0, 288(%rsp)
	movq	%r12, 96(%rsp)          # 8-byte Spill
	leaq	40(%r12), %rsi
	leaq	288(%rsp), %rdi
	callq	reg_set_to_hard_reg_set
	movq	$0, 160(%rsp)
	leaq	64(%r12), %rsi
	movq	%rsi, 144(%rsp)         # 8-byte Spill
	leaq	160(%rsp), %rdi
	callq	reg_set_to_hard_reg_set
	movq	288(%rsp), %rax
	movq	bad_spill_regs(%rip), %rcx
	movq	160(%rsp), %rdx
	orq	%rax, %rcx
	orq	%rcx, %rdx
	movq	%rdx, bad_spill_regs(%rip)
	movabsq	$pseudos_counted, %rdi
	callq	bitmap_clear
	movq	40(%r12), %rcx
	testq	%rcx, %rcx
	movq	%rbx, %rdx
	je	.LBB3_378
# BB#366:                               # %for.cond11.preheader.lr.ph.i.i.i
                                        #   in Loop: Header=BB3_344 Depth=2
	movl	16(%rcx), %eax
	testq	%rax, %rax
	movq	$53, %r15
	je	.LBB3_368
# BB#367:                               # %select.mid
                                        #   in Loop: Header=BB3_344 Depth=2
	xorq	%r15, %r15
	.align	16, 0x90
.LBB3_368:                              # %for.cond11.preheader.i.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB3_370 Depth 4
                                        #         Child Loop BB3_374 Depth 4
	movq	%rcx, 152(%rsp)         # 8-byte Spill
	movq	24(%rcx), %r13
	testq	%r13, %r13
	je	.LBB3_373
# BB#369:                               # %for.body18.preheader.i.i.i
                                        #   in Loop: Header=BB3_368 Depth=3
	movq	%r15, %rbx
	andq	%rdx, %rbx
	.align	16, 0x90
.LBB3_370:                              # %for.body18.i.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        #       Parent Loop BB3_368 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	$1, %r14
	movq	%rbx, %rcx
	shlq	%cl, %r14
	testq	%r13, %r14
	je	.LBB3_372
# BB#371:                               # %if.then20.i.i.i
                                        #   in Loop: Header=BB3_370 Depth=4
	movq	152(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %edi
	notq	%r14
	movq	$7, %rcx
	shlq	%cl, %rdi
	addq	%rbx, %rdi
	movq	%rdx, %r12
	callq	count_pseudo
	movq	%r12, %rdx
	andq	%r14, %r13
	je	.LBB3_373
.LBB3_372:                              # %for.inc.i.i.i
                                        #   in Loop: Header=BB3_370 Depth=4
	incq	%r15
	movq	%r15, %rax
	andq	%rdx, %rax
	incq	%rbx
	cmpq	$64, %rax
	jb	.LBB3_370
.LBB3_373:                              # %if.end29.i.i.i
                                        #   in Loop: Header=BB3_368 Depth=3
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %r15
	xorq	%r14, %r14
	testq	%r15, %r15
	movabsq	$0, %r13
	je	.LBB3_377
	.align	16, 0x90
.LBB3_374:                              # %for.body18.1.i.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        #       Parent Loop BB3_368 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	$1, %rbx
	movq	%r14, %rcx
	shlq	%cl, %rbx
	testq	%r15, %rbx
	je	.LBB3_376
# BB#375:                               # %if.then20.1.i.i.i
                                        #   in Loop: Header=BB3_374 Depth=4
	movq	152(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	notq	%rbx
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	64(%r14,%rax), %rdi
	movq	%rdx, %r12
	callq	count_pseudo
	movq	%r12, %rdx
	andq	%rbx, %r15
	je	.LBB3_377
.LBB3_376:                              # %for.inc.1.i.i.i
                                        #   in Loop: Header=BB3_374 Depth=4
	incq	%r13
	movq	%r13, %rax
	andq	%rdx, %rax
	incq	%r14
	cmpq	$64, %rax
	jb	.LBB3_374
.LBB3_377:                              # %if.end29.1.i.i.i
                                        #   in Loop: Header=BB3_368 Depth=3
	movq	152(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rcx
	xorq	%r15, %r15
	testq	%rcx, %rcx
	jne	.LBB3_368
.LBB3_378:                              # %do.body37.i.i.i
                                        #   in Loop: Header=BB3_344 Depth=2
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB3_391
# BB#379:                               # %for.cond62.preheader.lr.ph.i.i.i
                                        #   in Loop: Header=BB3_344 Depth=2
	movl	16(%rcx), %eax
	testq	%rax, %rax
	movq	$53, %r15
	je	.LBB3_381
# BB#380:                               # %select.mid212
                                        #   in Loop: Header=BB3_344 Depth=2
	xorq	%r15, %r15
	.align	16, 0x90
.LBB3_381:                              # %for.cond62.preheader.i.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB3_383 Depth 4
                                        #         Child Loop BB3_387 Depth 4
	movq	%rcx, 152(%rsp)         # 8-byte Spill
	movq	24(%rcx), %r12
	testq	%r12, %r12
	je	.LBB3_386
# BB#382:                               # %for.body73.preheader.i.i.i
                                        #   in Loop: Header=BB3_381 Depth=3
	movq	%r15, %r13
	andq	%rdx, %r13
	.align	16, 0x90
.LBB3_383:                              # %for.body73.i.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        #       Parent Loop BB3_381 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	$1, %rbx
	movq	%r13, %rcx
	shlq	%cl, %rbx
	testq	%rbx, %r12
	je	.LBB3_385
# BB#384:                               # %if.then79.i.i.i
                                        #   in Loop: Header=BB3_383 Depth=4
	movq	152(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %edi
	notq	%rbx
	movq	$7, %rcx
	shlq	%cl, %rdi
	addq	%r13, %rdi
	movq	%rdx, %r14
	callq	count_pseudo
	movq	%r14, %rdx
	andq	%rbx, %r12
	je	.LBB3_386
.LBB3_385:                              # %for.inc91.i.i.i
                                        #   in Loop: Header=BB3_383 Depth=4
	incq	%r15
	movq	%r15, %rax
	andq	%rdx, %rax
	incq	%r13
	cmpq	$64, %rax
	jb	.LBB3_383
.LBB3_386:                              # %if.end94.i.i.i
                                        #   in Loop: Header=BB3_381 Depth=3
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %r15
	xorq	%r13, %r13
	testq	%r15, %r15
	movabsq	$0, %r12
	je	.LBB3_390
	.align	16, 0x90
.LBB3_387:                              # %for.body73.1.i.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        #       Parent Loop BB3_381 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	$1, %rbx
	movq	%r13, %rcx
	shlq	%cl, %rbx
	testq	%rbx, %r15
	je	.LBB3_389
# BB#388:                               # %if.then79.1.i.i.i
                                        #   in Loop: Header=BB3_387 Depth=4
	movq	152(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	notq	%rbx
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	64(%r13,%rax), %rdi
	movq	%rdx, %r14
	callq	count_pseudo
	movq	%r14, %rdx
	andq	%rbx, %r15
	je	.LBB3_390
.LBB3_389:                              # %for.inc91.1.i.i.i
                                        #   in Loop: Header=BB3_387 Depth=4
	incq	%r12
	movq	%r12, %rax
	andq	%rdx, %rax
	incq	%r13
	cmpq	$64, %rax
	jb	.LBB3_387
.LBB3_390:                              # %if.end94.1.i.i.i
                                        #   in Loop: Header=BB3_381 Depth=3
	movq	152(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rcx
	xorq	%r15, %r15
	testq	%rcx, %rcx
	jne	.LBB3_381
.LBB3_391:                              # %order_regs_for_reload.exit.i.i
                                        #   in Loop: Header=BB3_344 Depth=2
	movabsq	$pseudos_counted, %rdi
	movq	%rdx, %rbx
	callq	bitmap_clear
	movq	%rbx, %r9
	movslq	n_reloads(%rip), %rax
	testq	%rax, %rax
	jle	.LBB3_493
# BB#392:                               # %for.body106.lr.ph.i.i
                                        #   in Loop: Header=BB3_344 Depth=2
	movq	$0, 128(%rsp)           # 8-byte Folded Spill
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB3_393:                              # %for.body106.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB3_402 Depth 4
                                        #           Child Loop BB3_408 Depth 5
                                        #         Child Loop BB3_400 Depth 4
                                        #           Child Loop BB3_424 Depth 5
                                        #         Child Loop BB3_462 Depth 4
                                        #           Child Loop BB3_464 Depth 5
                                        #           Child Loop BB3_468 Depth 5
                                        #         Child Loop BB3_475 Depth 4
                                        #           Child Loop BB3_477 Depth 5
                                        #           Child Loop BB3_481 Depth 5
                                        #         Child Loop BB3_486 Depth 4
	movswq	reload_order(%rdi,%rdi), %rdx
	imulq	$104, %rdx, %rsi
	cmpq	$0, rld+8(%rsi)
	jne	.LBB3_396
# BB#394:                               # %lor.lhs.false114.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	imulq	$104, %rdx, %rax
	cmpq	$0, rld(%rax)
	jne	.LBB3_396
# BB#395:                               # %lor.lhs.false119.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	movzbl	rld+96(%rax), %eax
	testq	$4, %rax
	je	.LBB3_492
	.align	16, 0x90
.LBB3_396:                              # %land.lhs.true126.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	imulq	$104, %rdx, %rcx
	movzbl	rld+96(%rcx), %eax
	testq	$1, %rax
	jne	.LBB3_492
# BB#397:                               # %land.lhs.true133.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	movl	rld+56(%rcx), %eax
	cmpq	%r9, %rax
	jne	.LBB3_492
# BB#398:                               # %if.then139.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	movq	%rsi, 144(%rsp)         # 8-byte Spill
	movq	%rdx, 136(%rsp)         # 8-byte Spill
	movq	bad_spill_regs(%rip), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	bad_spill_regs_global(%rip), %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movl	rld+16(%rcx), %eax
	movq	%rcx, 104(%rsp)         # 8-byte Spill
	movq	reg_class_contents(,%rax,8), %r10
	movq	%r10, 152(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rdi, %rax
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%r13, %r13
	testq	%rax, %rax
	movabsq	$0, %r12
	movabsq	$0, %rbx
	jle	.LBB3_399
	.align	16, 0x90
.LBB3_402:                              # %for.body.i.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        #       Parent Loop BB3_393 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB3_408 Depth 5
	movq	%r9, %r15
	movswq	reload_order(%rbx,%rbx), %rdi
	imulq	$104, %rdi, %r14
	movslq	rld+56(%r14), %rax
	testq	%rax, %rax
	js	.LBB3_403
# BB#404:                               # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB3_402 Depth=4
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	reloads_conflict
	movq	%r15, %r9
	testq	%r9, %rax
	jne	.LBB3_406
# BB#405:                               #   in Loop: Header=BB3_402 Depth=4
	movq	152(%rsp), %r10         # 8-byte Reload
	jmp	.LBB3_409
	.align	16, 0x90
.LBB3_403:                              #   in Loop: Header=BB3_402 Depth=4
	movq	%r15, %r9
	jmp	.LBB3_409
	.align	16, 0x90
.LBB3_406:                              # %for.cond12.preheader.i.i.i
                                        #   in Loop: Header=BB3_402 Depth=4
	movl	rld+32(%r14), %eax
	testq	%rax, %rax
	movq	152(%rsp), %r10         # 8-byte Reload
	je	.LBB3_409
# BB#407:                               # %for.body17.lr.ph.i.i.i
                                        #   in Loop: Header=BB3_402 Depth=4
	leaq	rld+56(%r14), %rcx
	movl	(%rcx), %edx
	movq	$1, %rsi
	.align	16, 0x90
.LBB3_408:                              # %for.body17.i.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        #       Parent Loop BB3_393 Depth=3
                                        #         Parent Loop BB3_402 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	leaq	-1(%rsi,%rdx), %rcx
	movq	$1, %rdi
	shlq	%cl, %rdi
	orq	%rdi, %r12
	movq	%rsi, %rcx
	andq	%r9, %rcx
	incq	%rsi
	cmpq	%rax, %rcx
	jne	.LBB3_408
	.align	16, 0x90
.LBB3_409:                              # %for.inc22.i.i.i
                                        #   in Loop: Header=BB3_402 Depth=4
	incq	%rbx
	movq	%rbx, %rax
	andq	%r9, %rax
	movq	128(%rsp), %rcx         # 8-byte Reload
	andq	%r9, %rcx
	cmpq	%rcx, %rax
	jne	.LBB3_402
.LBB3_399:                              # %for.cond25.preheader.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	movq	144(%rsp), %rax         # 8-byte Reload
	leaq	rld+8(%rax), %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	104(%rsp), %rax         # 8-byte Reload
	leaq	rld+56(%rax), %rcx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	movq	112(%rsp), %rdx         # 8-byte Reload
	movq	88(%rsp), %rcx          # 8-byte Reload
	orq	%rcx, %rdx
	leaq	rld+16(%rax), %rcx
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	notq	%r10
	orq	%rdx, %r10
	orq	%r12, %r10
	leaq	rld+28(%rax), %r12
	leaq	rld(%rax), %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	$2147483647, 144(%rsp)  # 8-byte Folded Spill
                                        # imm = 0x7FFFFFFF
	movq	%r9, 152(%rsp)          # 8-byte Spill
	movabsq	$4294967288, %r15       # imm = 0xFFFFFFF8
	.align	16, 0x90
.LBB3_400:                              # %for.body28.i.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        #       Parent Loop BB3_393 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB3_424 Depth 5
	movq	%r9, %rbx
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	testq	%r10, %rax
	jne	.LBB3_401
# BB#410:                               # %land.lhs.true38.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movq	%r10, %r14
	movl	(%r12), %esi
	movq	%r13, %rdi
	callq	ix86_hard_regno_mode_ok
	movq	%rbx, %r9
	testq	%r9, %rax
	jne	.LBB3_412
# BB#411:                               #   in Loop: Header=BB3_400 Depth=4
	movq	%r14, %r10
	jmp	.LBB3_442
	.align	16, 0x90
.LBB3_401:                              #   in Loop: Header=BB3_400 Depth=4
	movq	%rbx, %r9
	jmp	.LBB3_442
	.align	16, 0x90
.LBB3_412:                              # %if.then41.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movl	spill_cost(,%r13,4), %esi
	movslq	(%r12), %rax
	movq	%r13, %rcx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	movq	%r14, %r10
	je	.LBB3_416
# BB#413:                               # %if.then41.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	leaq	-21(%r13), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB3_416
# BB#414:                               # %if.then41.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	leaq	-45(%r13), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB3_416
# BB#415:                               # %if.then41.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	leaq	-29(%r13), %rcx
	andq	%r9, %rcx
	cmpq	$7, %rcx
	jbe	.LBB3_416
# BB#417:                               # %cond.false.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movq	%rax, %rcx
	andq	%r9, %rcx
	cmpq	$24, %rcx
	jne	.LBB3_418
# BB#420:                               # %cond.true87.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB3_422
.LBB3_416:                              # %cond.true.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movl	mode_class(,%rax,4), %eax
	addq	$-5, %rax
	andq	%r9, %rax
	cmpq	$2, %rax
	movq	$2, %rax
	jb	.LBB3_423
	jmp	.LBB3_428
.LBB3_418:                              # %cond.false.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	cmpq	$18, %rcx
	jne	.LBB3_421
# BB#419:                               # %cond.true79.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB3_422
.LBB3_421:                              # %cond.false91.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
.LBB3_422:                              # %cond.end106.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movq	%rax, %rcx
	andq	%r9, %rcx
	cmpq	$2, %rcx
	jb	.LBB3_428
.LBB3_423:                              # %for.body111.lr.ph.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movq	$1, %r8
	movq	$2, %rdi
	.align	16, 0x90
.LBB3_424:                              # %for.body111.i.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        #       Parent Loop BB3_393 Depth=3
                                        #         Parent Loop BB3_400 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	leaq	-1(%r13,%rdi), %rcx
	movq	%rcx, %rbx
	andq	%r9, %rbx
	movl	spill_add_cost(,%rbx,4), %ebx
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%r10, %rdx
	je	.LBB3_426
# BB#425:                               # %for.body111.i.i.i
                                        #   in Loop: Header=BB3_424 Depth=5
	xorq	%r8, %r8
.LBB3_426:                              # %for.body111.i.i.i
                                        #   in Loop: Header=BB3_424 Depth=5
	addq	%rbx, %rsi
	movq	%rdi, %rcx
	andq	%r9, %rcx
	movq	%rax, %rdx
	andq	%r9, %rdx
	incq	%rdi
	cmpq	%rdx, %rcx
	jne	.LBB3_424
# BB#427:                               # %for.end131.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	testq	%r9, %r8
	je	.LBB3_442
.LBB3_428:                              # %if.end134.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB3_433
# BB#429:                               # %land.lhs.true136.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB3_433
# BB#430:                               # %land.lhs.true140.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movl	8(%rax), %eax
	movq	%r13, %rcx
	andq	%r9, %rcx
	cmpq	%rcx, %rax
	movabsq	$0, %rax
	jne	.LBB3_432
# BB#431:                               # %land.lhs.true140.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movabsq	$1, %rax
.LBB3_432:                              # %land.lhs.true140.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	subq	%rax, %rsi
.LBB3_433:                              # %if.end146.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB3_438
# BB#434:                               # %land.lhs.true148.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB3_438
# BB#435:                               # %land.lhs.true154.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movl	8(%rax), %eax
	movq	%r13, %rcx
	andq	%r9, %rcx
	cmpq	%rcx, %rax
	movabsq	$0, %rax
	jne	.LBB3_437
# BB#436:                               # %land.lhs.true154.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movabsq	$1, %rax
.LBB3_437:                              # %land.lhs.true154.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	subq	%rax, %rsi
.LBB3_438:                              # %if.end163.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movq	$32, %rcx
	movq	144(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB3_441
# BB#439:                               # %lor.lhs.false166.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movq	144(%rsp), %rax         # 8-byte Reload
	andq	%r9, %rax
	movq	%rsi, %rcx
	andq	%r9, %rcx
	cmpq	%rax, %rcx
	jne	.LBB3_442
# BB#440:                               # %land.lhs.true169.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movslq	inv_reg_alloc_order(,%r13,4), %rax
	movq	$32, %rcx
	movq	152(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movslq	inv_reg_alloc_order(,%rdx,4), %rcx
	cmpq	%rcx, %rax
	jge	.LBB3_442
.LBB3_441:                              # %if.then176.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	movq	%r13, 152(%rsp)         # 8-byte Spill
	movq	%rsi, 144(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB3_442:                              # %for.inc179.i.i.i
                                        #   in Loop: Header=BB3_400 Depth=4
	incq	%r13
	cmpq	$53, %r13
	jne	.LBB3_400
# BB#443:                               # %for.end181.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	movq	152(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rax
	andq	%r9, %rax
	cmpq	%r9, %rax
	je	.LBB3_489
# BB#444:                               # %if.end185.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB3_446
# BB#445:                               # %if.then187.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	xorq	%rax, %rax
	movabsq	$.L.str41, %rsi
	movq	%rbx, %rdx
	movq	136(%rsp), %rcx         # 8-byte Reload
	movq	%r9, %r13
	callq	fprintf
	movq	%r13, %r9
.LBB3_446:                              # %if.end189.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	movslq	(%r12), %rax
	movq	%rbx, %rcx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	je	.LBB3_450
# BB#447:                               # %if.end189.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	leaq	-21(%rbx), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB3_450
# BB#448:                               # %if.end189.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	leaq	-45(%rbx), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB3_450
# BB#449:                               # %if.end189.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	leaq	-29(%rbx), %rcx
	andq	%r9, %rcx
	cmpq	$7, %rcx
	jbe	.LBB3_450
# BB#454:                               # %cond.false227.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	movq	%rax, %rcx
	andq	%r9, %rcx
	cmpq	$24, %rcx
	jne	.LBB3_455
# BB#457:                               # %cond.true239.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB3_459
.LBB3_450:                              # %cond.true213.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB3_459
# BB#451:                               # %lor.rhs219.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB3_453
# BB#452:                               # %lor.rhs219.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	movabsq	$1, %rax
.LBB3_453:                              # %lor.rhs219.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	incq	%rax
	jmp	.LBB3_459
.LBB3_455:                              # %cond.false227.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	cmpq	$18, %rcx
	jne	.LBB3_458
# BB#456:                               # %cond.true231.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB3_459
.LBB3_458:                              # %cond.false243.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
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
.LBB3_459:                              # %cond.end261.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	movq	104(%rsp), %rcx         # 8-byte Reload
	movl	%eax, rld+32(%rcx)
	movq	80(%rsp), %rax          # 8-byte Reload
	movl	%ebx, (%rax)
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rdx
	leaq	rld+32(%rcx), %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	testq	%rdx, %rdx
	je	.LBB3_472
# BB#460:                               # %for.cond281.preheader.lr.ph.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	movl	16(%rdx), %eax
	testq	%rax, %rax
	movq	$53, %r15
	je	.LBB3_462
# BB#461:                               # %select.mid228
                                        #   in Loop: Header=BB3_393 Depth=3
	xorq	%r15, %r15
	.align	16, 0x90
.LBB3_462:                              # %for.cond281.preheader.i.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        #       Parent Loop BB3_393 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB3_464 Depth 5
                                        #           Child Loop BB3_468 Depth 5
	movq	%rdx, 136(%rsp)         # 8-byte Spill
	movq	24(%rdx), %r14
	testq	%r14, %r14
	je	.LBB3_467
# BB#463:                               # %for.body293.preheader.i.i.i
                                        #   in Loop: Header=BB3_462 Depth=4
	movq	%r15, %rbx
	andq	%r9, %rbx
	.align	16, 0x90
.LBB3_464:                              # %for.body293.i.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        #       Parent Loop BB3_393 Depth=3
                                        #         Parent Loop BB3_462 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	$1, %r12
	movq	%rbx, %rcx
	shlq	%cl, %r12
	testq	%r12, %r14
	je	.LBB3_466
# BB#465:                               # %if.then299.i.i.i
                                        #   in Loop: Header=BB3_464 Depth=5
	movq	136(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %edx
	movq	$7, %rcx
	movq	144(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %esi
	shlq	%cl, %rdx
	notq	%r12
	addq	%rbx, %rdx
	movq	152(%rsp), %rdi         # 8-byte Reload
	movq	%r9, %r13
	callq	count_spilled_pseudo
	movq	%r13, %r9
	andq	%r12, %r14
	je	.LBB3_467
.LBB3_466:                              # %for.inc312.i.i.i
                                        #   in Loop: Header=BB3_464 Depth=5
	incq	%r15
	movq	%r15, %rax
	andq	%r9, %rax
	incq	%rbx
	cmpq	$64, %rax
	jb	.LBB3_464
.LBB3_467:                              # %if.end315.i.i.i
                                        #   in Loop: Header=BB3_462 Depth=4
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %r14
	xorq	%r12, %r12
	testq	%r14, %r14
	movabsq	$0, %r15
	je	.LBB3_471
	.align	16, 0x90
.LBB3_468:                              # %for.body293.1.i.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        #       Parent Loop BB3_393 Depth=3
                                        #         Parent Loop BB3_462 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	$1, %rbx
	movq	%r12, %rcx
	shlq	%cl, %rbx
	testq	%rbx, %r14
	je	.LBB3_470
# BB#469:                               # %if.then299.1.i.i.i
                                        #   in Loop: Header=BB3_468 Depth=5
	movq	136(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	$7, %rcx
	movq	144(%rsp), %rdx         # 8-byte Reload
	movl	(%rdx), %esi
	shlq	%cl, %rax
	notq	%rbx
	leaq	64(%r12,%rax), %rdx
	movq	152(%rsp), %rdi         # 8-byte Reload
	movq	%r9, %r13
	callq	count_spilled_pseudo
	movq	%r13, %r9
	andq	%rbx, %r14
	je	.LBB3_471
.LBB3_470:                              # %for.inc312.1.i.i.i
                                        #   in Loop: Header=BB3_468 Depth=5
	incq	%r15
	movq	%r15, %rax
	andq	%r9, %rax
	incq	%r12
	cmpq	$64, %rax
	jb	.LBB3_468
.LBB3_471:                              # %if.end315.1.i.i.i
                                        #   in Loop: Header=BB3_462 Depth=4
	movq	136(%rsp), %rdx         # 8-byte Reload
	movq	(%rdx), %rdx
	xorq	%r15, %r15
	testq	%rdx, %rdx
	jne	.LBB3_462
.LBB3_472:                              # %do.body322.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	64(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB3_485
# BB#473:                               # %for.cond351.preheader.lr.ph.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	movl	16(%rcx), %eax
	testq	%rax, %rax
	movq	$53, %r15
	je	.LBB3_475
# BB#474:                               # %select.mid236
                                        #   in Loop: Header=BB3_393 Depth=3
	xorq	%r15, %r15
	.align	16, 0x90
.LBB3_475:                              # %for.cond351.preheader.i.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        #       Parent Loop BB3_393 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB3_477 Depth 5
                                        #           Child Loop BB3_481 Depth 5
	movq	%rcx, 136(%rsp)         # 8-byte Spill
	movq	24(%rcx), %r14
	testq	%r14, %r14
	je	.LBB3_480
# BB#476:                               # %for.body365.preheader.i.i.i
                                        #   in Loop: Header=BB3_475 Depth=4
	movq	%r15, %r12
	andq	%r9, %r12
	.align	16, 0x90
.LBB3_477:                              # %for.body365.i.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        #       Parent Loop BB3_393 Depth=3
                                        #         Parent Loop BB3_475 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	$1, %rbx
	movq	%r12, %rcx
	shlq	%cl, %rbx
	testq	%rbx, %r14
	je	.LBB3_479
# BB#478:                               # %if.then372.i.i.i
                                        #   in Loop: Header=BB3_477 Depth=5
	movq	136(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %edx
	movq	$7, %rcx
	movq	144(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %esi
	shlq	%cl, %rdx
	notq	%rbx
	addq	%r12, %rdx
	movq	152(%rsp), %rdi         # 8-byte Reload
	movq	%r9, %r13
	callq	count_spilled_pseudo
	movq	%r13, %r9
	andq	%rbx, %r14
	je	.LBB3_480
.LBB3_479:                              # %for.inc386.i.i.i
                                        #   in Loop: Header=BB3_477 Depth=5
	incq	%r15
	movq	%r15, %rax
	andq	%r9, %rax
	incq	%r12
	cmpq	$64, %rax
	jb	.LBB3_477
.LBB3_480:                              # %if.end389.i.i.i
                                        #   in Loop: Header=BB3_475 Depth=4
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %r14
	xorq	%r12, %r12
	testq	%r14, %r14
	movabsq	$0, %r15
	je	.LBB3_484
	.align	16, 0x90
.LBB3_481:                              # %for.body365.1.i.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        #       Parent Loop BB3_393 Depth=3
                                        #         Parent Loop BB3_475 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	$1, %rbx
	movq	%r12, %rcx
	shlq	%cl, %rbx
	testq	%rbx, %r14
	je	.LBB3_483
# BB#482:                               # %if.then372.1.i.i.i
                                        #   in Loop: Header=BB3_481 Depth=5
	movq	136(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	$7, %rcx
	movq	144(%rsp), %rdx         # 8-byte Reload
	movl	(%rdx), %esi
	shlq	%cl, %rax
	notq	%rbx
	leaq	64(%r12,%rax), %rdx
	movq	152(%rsp), %rdi         # 8-byte Reload
	movq	%r9, %r13
	callq	count_spilled_pseudo
	movq	%r13, %r9
	andq	%rbx, %r14
	je	.LBB3_484
.LBB3_483:                              # %for.inc386.1.i.i.i
                                        #   in Loop: Header=BB3_481 Depth=5
	incq	%r15
	movq	%r15, %rax
	andq	%r9, %rax
	incq	%r12
	cmpq	$64, %rax
	jb	.LBB3_481
.LBB3_484:                              # %if.end389.1.i.i.i
                                        #   in Loop: Header=BB3_475 Depth=4
	movq	136(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rcx
	xorq	%r15, %r15
	testq	%rcx, %rcx
	jne	.LBB3_475
.LBB3_485:                              # %for.cond397.preheader.i.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	movq	144(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %eax
	testq	%rax, %rax
	movq	$1, %rdx
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	152(%rsp), %rbx         # 8-byte Reload
	je	.LBB3_492
	.align	16, 0x90
.LBB3_486:                              # %for.body401.i.i.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_344 Depth=2
                                        #       Parent Loop BB3_393 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	leaq	-1(%rdx,%rbx), %rcx
	andq	%r9, %rcx
	movl	spill_cost(,%rcx,4), %esi
	testq	%rsi, %rsi
	jne	.LBB3_488
# BB#487:                               # %lor.lhs.false407.i.i.i
                                        #   in Loop: Header=BB3_486 Depth=4
	movl	spill_add_cost(,%rcx,4), %esi
	testq	%rsi, %rsi
	jne	.LBB3_488
# BB#491:                               # %if.end414.i.i.i
                                        #   in Loop: Header=BB3_486 Depth=4
	movq	$1, %rsi
	shlq	%cl, %rsi
	orq	%rsi, used_spill_regs_local(%rip)
	movq	%rdx, %rcx
	andq	%r9, %rcx
	incq	%rdx
	cmpq	%rax, %rcx
	jb	.LBB3_486
	.align	16, 0x90
.LBB3_492:                              # %for.inc148.i.i
                                        #   in Loop: Header=BB3_393 Depth=3
	incq	%rdi
	incq	128(%rsp)               # 8-byte Folded Spill
	movslq	n_reloads(%rip), %rax
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB3_393
.LBB3_493:                              # %for.end150.i.i
                                        #   in Loop: Header=BB3_344 Depth=2
	movq	%r9, %r12
	movq	used_spill_regs_local(%rip), %rcx
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	%rcx, 104(%rbx)
	movq	used_spill_regs_local(%rip), %rcx
	orq	%rcx, used_spill_regs(%rip)
	movq	88(%rbx), %rdi
	imulq	$104, %rax, %rdx
	movabsq	$rld, %rsi
	callq	memcpy
	xorq	%r13, %r13
.LBB3_494:                              # %find_reload_regs.exit.i
                                        #   in Loop: Header=BB3_344 Depth=2
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	movq	%r12, %r10
	movabsq	$0, %r15
	jne	.LBB3_344
.LBB3_495:                              # %select_reload_regs.exit
                                        #   in Loop: Header=BB3_164 Depth=1
	movzbl	failure(%rip), %eax
	testq	%rax, %rax
	jne	.LBB3_549
# BB#496:                               # %if.end841
                                        #   in Loop: Header=BB3_164 Depth=1
	cmpq	$0, insns_need_reload(%rip)
	jne	.LBB3_499
# BB#497:                               # %if.end841
                                        #   in Loop: Header=BB3_164 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%r10, %rax
	jne	.LBB3_499
# BB#498:                               #   in Loop: Header=BB3_164 Depth=1
	movq	48(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB3_500
.LBB3_499:                              # %if.then846
                                        #   in Loop: Header=BB3_164 Depth=1
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r10, %rbx
	callq	finish_spills
	movq	%rbx, %r10
	movq	48(%rsp), %rcx          # 8-byte Reload
	orq	%rax, %rcx
.LBB3_500:                              # %if.end848
                                        #   in Loop: Header=BB3_164 Depth=1
	testq	%r10, %rcx
	je	.LBB3_517
# BB#501:                               # %if.end851
                                        #   in Loop: Header=BB3_164 Depth=1
	movl	caller_save_needed(%rip), %eax
	testq	%rax, %rax
	je	.LBB3_513
# BB#502:                               # %if.then853
                                        #   in Loop: Header=BB3_164 Depth=1
	movq	reload_insn_chain(%rip), %r14
.LBB3_503:                              # %while.cond.loopexit.i
                                        #   Parent Loop BB3_164 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_505 Depth 3
	testq	%r14, %r14
	je	.LBB3_513
# BB#504:                               # %while.cond1.preheader.i
                                        #   in Loop: Header=BB3_503 Depth=2
	testq	%r14, %r14
	movq	%r14, %rbx
	je	.LBB3_513
	.align	16, 0x90
.LBB3_505:                              # %land.rhs.i
                                        #   Parent Loop BB3_164 Depth=1
                                        #     Parent Loop BB3_503 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	262(%rbx), %eax
	movq	(%rbx), %r14
	testq	$8, %rax
	je	.LBB3_503
# BB#506:                               # %while.body3.i
                                        #   in Loop: Header=BB3_505 Depth=3
	movq	%r15, %r12
	movq	%r10, %r15
	movq	reload_insn_chain(%rip), %rax
	movq	32(%rbx), %rdi
	cmpq	%rax, %rbx
	jne	.LBB3_508
# BB#507:                               # %if.then.i1713
                                        #   in Loop: Header=BB3_505 Depth=3
	movq	%r14, reload_insn_chain(%rip)
.LBB3_508:                              # %if.end.i1715
                                        #   in Loop: Header=BB3_505 Depth=3
	callq	delete_insn
	testq	%r14, %r14
	je	.LBB3_510
# BB#509:                               # %if.then8.i1716
                                        #   in Loop: Header=BB3_505 Depth=3
	movq	8(%rbx), %rax
	movq	%rax, 8(%r14)
.LBB3_510:                              # %if.end10.i
                                        #   in Loop: Header=BB3_505 Depth=3
	movq	8(%rbx), %rax
	testq	%rax, %rax
	movq	%r15, %r10
	je	.LBB3_512
# BB#511:                               # %if.then13.i
                                        #   in Loop: Header=BB3_505 Depth=3
	movq	%r14, (%rax)
.LBB3_512:                              # %if.end16.i
                                        #   in Loop: Header=BB3_505 Depth=3
	movq	unused_insn_chains(%rip), %rax
	testq	%r14, %r14
	movq	%rax, (%rbx)
	movq	%rbx, unused_insn_chains(%rip)
	movq	%r14, %rbx
	movq	%r12, %r15
	jne	.LBB3_505
.LBB3_513:                              # %if.end854
                                        #   in Loop: Header=BB3_164 Depth=1
	movq	reload_firstobj(%rip), %rsi
	movq	reload_obstack+8(%rip), %rax
	cmpq	%rax, %rsi
	jbe	.LBB3_516
# BB#514:                               # %if.end854
                                        #   in Loop: Header=BB3_164 Depth=1
	movq	reload_obstack+32(%rip), %rax
	cmpq	%rax, %rsi
	jae	.LBB3_516
# BB#515:                               # %if.then865
                                        #   in Loop: Header=BB3_164 Depth=1
	movq	%r10, %r12
	movq	%rsi, reload_obstack+16(%rip)
	movq	%rsi, reload_obstack+24(%rip)
	jmp	.LBB3_164
.LBB3_517:                              # %for.end870
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	64(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	je	.LBB3_522
# BB#518:                               # %for.body877.lr.ph
	movq	reg_eliminate(%rip), %rax
	movq	%rax, %rbx
	.align	16, 0x90
.LBB3_519:                              # %for.body877
                                        # =>This Inner Loop Header: Depth=1
	movl	12(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB3_521
# BB#520:                               # %if.then880
                                        #   in Loop: Header=BB3_519 Depth=1
	movl	4(%rbx), %esi
	movl	(%rbx), %edi
	callq	mark_elimination
	movq	reg_eliminate(%rip), %rax
.LBB3_521:                              # %for.inc883
                                        #   in Loop: Header=BB3_519 Depth=1
	addq	$48, %rbx
	leaq	192(%rax), %rcx
	cmpq	%rcx, %rbx
	jb	.LBB3_519
.LBB3_522:                              # %for.cond887.preheader
	movslq	max_regno(%rip), %rax
	cmpq	$53, %rax
	jle	.LBB3_534
# BB#523:
	movq	$53, %r15
	movq	$37, %r12
	.align	16, 0x90
.LBB3_524:                              # %for.body890
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_526 Depth 2
	movq	reg_renumber(%rip), %rcx
	movswq	(%rcx,%r15,2), %rcx
	testq	%rcx, %rcx
	jns	.LBB3_533
# BB#525:                               # %land.lhs.true896
                                        #   in Loop: Header=BB3_524 Depth=1
	movq	reg_equiv_init(%rip), %rcx
	movq	(%rcx,%r15,8), %rbx
	testq	%rbx, %rbx
	je	.LBB3_533
	.align	16, 0x90
.LBB3_526:                              # %for.body907
                                        #   Parent Loop BB3_524 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx), %r13
	movzbl	(%r13), %eax
	cmpq	$37, %rax
	je	.LBB3_531
# BB#527:                               # %lor.lhs.false916
                                        #   in Loop: Header=BB3_526 Depth=2
	movq	%r13, %rdi
	callq	can_throw_internal
	testq	%rax, %rax
	jne	.LBB3_531
# BB#528:                               # %if.else920
                                        #   in Loop: Header=BB3_526 Depth=2
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	(%rax,%r15,8), %rdi
	movq	32(%r13), %rsi
	callq	reg_set_p
	testq	%r14, %rax
	jne	.LBB3_529
# BB#530:                               # %if.else931
                                        #   in Loop: Header=BB3_526 Depth=2
	movw	%r12w, (%r13)
	movq	$0, 32(%r13)
	movabsq	$4294967197, %rax       # imm = 0xFFFFFF9D
	movl	%eax, 40(%r13)
	jmp	.LBB3_531
.LBB3_529:                              # %if.then930
                                        #   in Loop: Header=BB3_526 Depth=2
	movq	%r13, %rdi
	callq	delete_dead_insn
	.align	16, 0x90
.LBB3_531:                              # %for.inc941
                                        #   in Loop: Header=BB3_526 Depth=2
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB3_526
# BB#532:                               # %for.inc947.loopexit
                                        #   in Loop: Header=BB3_524 Depth=1
	movl	max_regno(%rip), %eax
.LBB3_533:                              # %for.inc947
                                        #   in Loop: Header=BB3_524 Depth=1
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB3_524
.LBB3_534:                              # %for.end949
	cmpq	$0, insns_need_reload(%rip)
	jne	.LBB3_537
# BB#535:                               # %for.end949
	movzbl	something_needs_elimination(%rip), %eax
	testq	$1, %rax
	jne	.LBB3_537
# BB#536:                               # %for.end949
	movl	something_needs_operands_changed(%rip), %eax
	testq	%r14, %rax
	je	.LBB3_545
.LBB3_537:                              # %if.then956
	callq	get_frame_size
	movq	%rax, %rbx
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	reload_as_needed
	callq	get_frame_size
	cmpq	%rax, %rbx
	jne	.LBB3_538
# BB#540:                               # %if.end963
	movl	num_eliminable(%rip), %eax
	testq	%rax, %rax
	je	.LBB3_545
# BB#541:                               # %if.then965
	movq	reg_eliminate(%rip), %rbx
	.align	16, 0x90
.LBB3_543:                              # %for.body.i1727
                                        # =>This Inner Loop Header: Depth=1
	movl	4(%rbx), %esi
	movl	(%rbx), %edi
	callq	ix86_initial_elimination_offset
	movl	8(%rbx), %ecx
	andq	%r14, %rax
	cmpq	%rcx, %rax
	jne	.LBB3_544
# BB#542:                               # %for.cond.i
                                        #   in Loop: Header=BB3_543 Depth=1
	movq	reg_eliminate(%rip), %rax
	addq	$192, %rax
	addq	$48, %rbx
	cmpq	%rax, %rbx
	jb	.LBB3_543
.LBB3_545:                              # %if.end967
	movl	frame_pointer_needed(%rip), %eax
	testq	%rax, %rax
	jne	.LBB3_549
# BB#546:                               # %if.end967
	movl	n_basic_blocks(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB3_549
# BB#547:
	movq	$4, %rbx
	.align	16, 0x90
.LBB3_548:                              # %for.body973
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	(%rax,%rbx,8), %rax
	movq	64(%rax), %rdi
	movq	$6, %rsi
	callq	bitmap_clear_bit
	movslq	n_basic_blocks(%rip), %rax
	leaq	-3(%rbx), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB3_548
.LBB3_549:                              # %failed
	movabsq	$spilled_pseudos, %rdi
	callq	bitmap_clear
	movslq	max_regno(%rip), %rcx
	xorq	%rax, %rax
	movl	%eax, reload_in_progress(%rip)
	cmpq	$54, %rcx
	jl	.LBB3_563
# BB#550:
	movabsq	$4278124610, %rdx       # imm = 0xFEFF0042
	.align	16, 0x90
.LBB3_551:                              # %for.body983
                                        # =>This Inner Loop Header: Depth=1
	movq	reg_equiv_mem(%rip), %rcx
	movq	424(%rcx,%rax,8), %rcx
	xorq	%rsi, %rsi
	testq	%rcx, %rcx
	je	.LBB3_553
# BB#552:                               # %if.then988
                                        #   in Loop: Header=BB3_551 Depth=1
	movq	8(%rcx), %rsi
.LBB3_553:                              # %if.end994
                                        #   in Loop: Header=BB3_551 Depth=1
	movq	reg_equiv_address(%rip), %rdi
	movq	424(%rdi,%rax,8), %rdi
	testq	%rdi, %rdi
	je	.LBB3_555
# BB#554:                               # %if.end994
                                        #   in Loop: Header=BB3_551 Depth=1
	movq	%rdi, %rsi
.LBB3_555:                              # %if.end994
                                        #   in Loop: Header=BB3_551 Depth=1
	testq	%rsi, %rsi
	je	.LBB3_562
# BB#556:                               # %if.then1003
                                        #   in Loop: Header=BB3_551 Depth=1
	movq	reg_renumber(%rip), %rdi
	movswq	106(%rdi,%rax,2), %rdi
	testq	%rdi, %rdi
	jns	.LBB3_560
# BB#557:                               # %if.then1009
                                        #   in Loop: Header=BB3_551 Depth=1
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	80(%rcx), %rcx
	movq	424(%rcx,%rax,8), %rcx
	movl	(%rcx), %edi
	andq	$-65536, %rdi           # imm = 0xFFFFFFFFFFFF0000
	orq	$66, %rdi
	movq	%rsi, 8(%rcx)
	movq	%rdi, %rbx
	andq	$-134283198, %rbx       # imm = 0xFFFFFFFFF7FF0042
	movl	%ebx, (%rcx)
	movq	reg_equiv_memory_loc(%rip), %rsi
	movq	424(%rsi,%rax,8), %rsi
	testq	%rsi, %rsi
	jne	.LBB3_558
# BB#559:                               # %if.else1081
                                        #   in Loop: Header=BB3_551 Depth=1
	andq	$1677656130, %rdi       # imm = 0x63FF0042
	movl	%edi, (%rcx)
	movq	$0, 16(%rcx)
	jmp	.LBB3_562
	.align	16, 0x90
.LBB3_560:                              # %if.else1092
                                        #   in Loop: Header=BB3_551 Depth=1
	testq	%rcx, %rcx
	je	.LBB3_562
# BB#561:                               # %if.then1096
                                        #   in Loop: Header=BB3_551 Depth=1
	movq	%rsi, 8(%rcx)
	jmp	.LBB3_562
.LBB3_558:                              # %if.then1026
                                        #   in Loop: Header=BB3_551 Depth=1
	movl	(%rsi), %esi
	andq	$134217728, %rsi        # imm = 0x8000000
	orq	%rbx, %rsi
	movl	%esi, (%rcx)
	movq	reg_equiv_memory_loc(%rip), %rdi
	movq	424(%rdi,%rax,8), %rdi
	movl	(%rdi), %edi
	andq	$268435456, %rdi        # imm = 0x10000000
	andq	$-268500926, %rsi       # imm = 0xFFFFFFFFEFFF0042
	orq	%rdi, %rsi
	movl	%esi, (%rcx)
	movq	reg_equiv_memory_loc(%rip), %rdi
	movq	424(%rdi,%rax,8), %rdi
	movl	(%rdi), %edi
	andq	$-2147483648, %rdi      # imm = 0xFFFFFFFF80000000
	andq	$2147418178, %rsi       # imm = 0x7FFF0042
	orq	%rdi, %rsi
	movl	%esi, (%rcx)
	movq	reg_equiv_memory_loc(%rip), %rdi
	movq	424(%rdi,%rax,8), %rdi
	movl	(%rdi), %edi
	andq	$67108864, %rdi         # imm = 0x4000000
	andq	$-67174334, %rsi        # imm = 0xFFFFFFFFFBFF0042
	orq	%rdi, %rsi
	movl	%esi, (%rcx)
	movq	reg_equiv_memory_loc(%rip), %rdi
	movq	424(%rdi,%rax,8), %rdi
	movl	(%rdi), %edi
	andq	$16777216, %rdi         # imm = 0x1000000
	andq	%rdx, %rsi
	orq	%rdi, %rsi
	movl	%esi, (%rcx)
	movq	reg_equiv_memory_loc(%rip), %rsi
	movq	424(%rsi,%rax,8), %rsi
	movq	16(%rsi), %rsi
	movq	%rsi, 16(%rcx)
	.align	16, 0x90
.LBB3_562:                              # %for.inc1105
                                        #   in Loop: Header=BB3_551 Depth=1
	movslq	max_regno(%rip), %rsi
	leaq	54(%rax), %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rax
	cmpq	%rsi, %rdi
	jl	.LBB3_551
.LBB3_563:                              # %for.end1107
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	movq	$1, %rax
	movl	%eax, reload_completed(%rip)
	je	.LBB3_584
# BB#564:
	movq	56(%rsp), %rbx          # 8-byte Reload
	.align	16, 0x90
.LBB3_565:                              # %for.body1110
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_577 Depth 2
                                        #       Child Loop BB3_578 Depth 3
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %ecx
	cmpq	$105, %rcx
	jne	.LBB3_583
# BB#566:                               # %if.then1118
                                        #   in Loop: Header=BB3_565 Depth=1
	cmpq	$34, %rax
	jne	.LBB3_568
# BB#567:                               # %if.then1124
                                        #   in Loop: Header=BB3_565 Depth=1
	movq	64(%rbx), %rdx
	leaq	64(%rbx), %rdi
	xorq	%rsi, %rsi
	callq	replace_pseudos_in_call_usage
.LBB3_568:                              # %if.end1131
                                        #   in Loop: Header=BB3_565 Depth=1
	movq	32(%rbx), %rax
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$48, %rdx
	jne	.LBB3_572
# BB#569:                               # %land.lhs.true1139
                                        #   in Loop: Header=BB3_565 Depth=1
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$131072, %rax           # imm = 0x20000
	je	.LBB3_574
# BB#570:                               # %lor.lhs.false1145
                                        #   in Loop: Header=BB3_565 Depth=1
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB3_574
# BB#571:                               # %lor.lhs.false1145.lor.lhs.false1148_crit_edge
                                        #   in Loop: Header=BB3_565 Depth=1
	movq	32(%rbx), %rax
	movl	(%rax), %ecx
	.align	16, 0x90
.LBB3_572:                              # %lor.lhs.false1148
                                        #   in Loop: Header=BB3_565 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$49, %rcx
	jne	.LBB3_575
# BB#573:                               # %land.lhs.true1156
                                        #   in Loop: Header=BB3_565 Depth=1
	movq	8(%rax), %rax
	movl	(%rax), %eax
	andq	$1073807359, %rax       # imm = 0x4000FFFF
	cmpq	$1073741885, %rax       # imm = 0x4000003D
	jne	.LBB3_574
.LBB3_575:                              # %if.end1180
                                        #   in Loop: Header=BB3_565 Depth=1
	movq	56(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB3_582
# BB#576:                               #   in Loop: Header=BB3_565 Depth=1
	leaq	56(%rbx), %rax
	.align	16, 0x90
.LBB3_577:                              # %while.body.lr.ph
                                        #   Parent Loop BB3_565 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_578 Depth 3
	movq	%rcx, %rdx
.LBB3_578:                              # %while.body
                                        #   Parent Loop BB3_565 Depth=1
                                        #     Parent Loop BB3_577 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	2(%rdx), %ecx
	cmpq	$10, %rcx
	ja	.LBB3_581
# BB#579:                               # %while.body
                                        #   in Loop: Header=BB3_578 Depth=3
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$1222, %rsi             # imm = 0x4C6
	je	.LBB3_581
# BB#580:                               # %if.then1215
                                        #   in Loop: Header=BB3_578 Depth=3
	movq	16(%rdx), %rdx
	testq	%rdx, %rdx
	movq	%rdx, (%rax)
	jne	.LBB3_578
	jmp	.LBB3_582
	.align	16, 0x90
.LBB3_581:                              # %if.else1219
                                        #   in Loop: Header=BB3_577 Depth=2
	movq	16(%rdx), %rcx
	addq	$16, %rdx
	testq	%rcx, %rcx
	movq	%rdx, %rax
	jne	.LBB3_577
	.align	16, 0x90
.LBB3_582:                              # %while.end
                                        #   in Loop: Header=BB3_565 Depth=1
	movq	%rbx, %rdi
	callq	cleanup_subreg_operands
	jmp	.LBB3_583
	.align	16, 0x90
.LBB3_574:                              # %if.then1178
                                        #   in Loop: Header=BB3_565 Depth=1
	movq	%rbx, %rdi
	callq	delete_insn
.LBB3_583:                              # %for.inc1225
                                        #   in Loop: Header=BB3_565 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB3_565
.LBB3_584:                              # %for.end1229
	movl	flag_stack_check(%rip), %eax
	testq	%rax, %rax
	je	.LBB3_595
# BB#585:                               # %if.then1231
	callq	get_frame_size
	movl	target_flags(%rip), %edx
	movq	$21, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	andq	$16, %rdi
	addq	$16, %rdi
	movabsq	$4294967280, %rsi       # imm = 0xFFFFFFF0
	andq	%rdi, %rsi
	addq	%rax, %rsi
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	leaq	4(%rdx), %rcx
	movabsq	$4294967292, %rax       # imm = 0xFFFFFFFC
	andq	%rcx, %rax
	movq	$-53, %rcx
	.align	16, 0x90
.LBB3_586:                              # %for.body1243
                                        # =>This Inner Loop Header: Depth=1
	movzbl	regs_ever_live+53(%rcx), %edi
	testq	%rdi, %rdi
	je	.LBB3_591
# BB#587:                               # %land.lhs.true1248
                                        #   in Loop: Header=BB3_586 Depth=1
	movzbl	fixed_regs+53(%rcx), %edi
	testq	%rdi, %rdi
	jne	.LBB3_591
# BB#588:                               # %land.lhs.true1252
                                        #   in Loop: Header=BB3_586 Depth=1
	movzbl	call_used_regs+53(%rcx), %ebx
	xorq	%rdi, %rdi
	testq	%rbx, %rbx
	je	.LBB3_590
# BB#589:                               # %select.mid267
                                        #   in Loop: Header=BB3_586 Depth=1
	movq	%rax, %rdi
.LBB3_590:                              # %select.end266
                                        #   in Loop: Header=BB3_586 Depth=1
	addq	%rdi, %rsi
.LBB3_591:                              # %for.inc1264
                                        #   in Loop: Header=BB3_586 Depth=1
	incq	%rcx
	jne	.LBB3_586
# BB#592:                               # %for.end1266
	movq	$4092, %rax             # imm = 0xFFC
	subq	%rdx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rcx, %rsi
	jle	.LBB3_595
# BB#593:                               # %if.then1274
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	callq	warning
	movzbl	reload.verbose_warned(%rip), %eax
	testq	%rax, %rax
	jne	.LBB3_595
# BB#594:                               # %if.then1276
	movabsq	$.L.str3, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	$1, %rax
	movb	%al, reload.verbose_warned(%rip)
.LBB3_595:                              # %if.end1279
	movq	reg_equiv_constant(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB3_597
# BB#596:                               # %if.then1281
	callq	free
.LBB3_597:                              # %if.end1282
	movq	reg_equiv_memory_loc(%rip), %rdi
	movq	$0, reg_equiv_constant(%rip)
	testq	%rdi, %rdi
	je	.LBB3_599
# BB#598:                               # %if.then1284
	callq	free
.LBB3_599:                              # %if.end1285
	movq	$0, reg_equiv_memory_loc(%rip)
	movq	32(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB3_601
# BB#600:                               # %if.then1287
	callq	free
.LBB3_601:                              # %if.end1288
	movq	24(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB3_603
# BB#602:                               # %if.then1290
	callq	free
.LBB3_603:                              # %if.end1291
	movq	reg_equiv_mem(%rip), %rdi
	callq	free
	movq	reg_equiv_init(%rip), %rdi
	callq	free
	movq	reg_equiv_address(%rip), %rdi
	callq	free
	movq	reg_max_ref_width(%rip), %rdi
	callq	free
	movq	reg_old_renumber(%rip), %rdi
	callq	free
	movq	pseudo_previous_regs(%rip), %rdi
	callq	free
	movq	pseudo_forbidden_regs(%rip), %rdi
	callq	free
	movslq	n_spills(%rip), %rax
	movq	$0, used_spill_regs(%rip)
	testq	%rax, %rax
	jle	.LBB3_607
# BB#604:
	xorq	%rdx, %rdx
	movq	$1, %rsi
	.align	16, 0x90
.LBB3_605:                              # %for.body1295
                                        # =>This Inner Loop Header: Depth=1
	movswq	spill_regs-2(%rsi,%rsi), %rcx
	movq	%rdx, %rdi
	movq	$1, %rdx
	shlq	%cl, %rdx
	orq	%rdi, %rdx
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rsi
	cmpq	%rax, %rdi
	jl	.LBB3_605
# BB#606:                               # %for.cond1292.for.end1304_crit_edge
	movq	%rdx, used_spill_regs(%rip)
.LBB3_607:                              # %for.end1304
	movq	reload_startobj(%rip), %rsi
	movq	reload_obstack+8(%rip), %rax
	cmpq	%rax, %rsi
	jbe	.LBB3_610
# BB#608:                               # %for.end1304
	movq	reload_obstack+32(%rip), %rax
	cmpq	%rax, %rsi
	jae	.LBB3_610
# BB#609:                               # %if.then1316
	movq	%rsi, reload_obstack+16(%rip)
	movq	%rsi, reload_obstack+24(%rip)
	jmp	.LBB3_611
.LBB3_610:                              # %if.else1319
	movabsq	$reload_obstack, %rdi
	callq	obstack_free
.LBB3_611:                              # %if.end1320
	movq	$0, unused_insn_chains(%rip)
	callq	fixup_abnormal_edges
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	unshare_all_rtl_again
	movzbl	failure(%rip), %eax
	addq	$528, %rsp              # imm = 0x210
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB3_488:                              # %if.then413.i.i.i
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.find_reg, %rdx
	movq	$1778, %rsi             # imm = 0x6F2
	callq	fancy_abort
.LBB3_612:                              # %if.else.i.i.i
	movabsq	$.L.str39, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	callq	error
	movabsq	$.L.str40, %rax
	movabsq	$.L.str1, %rdx
	movabsq	$.L__FUNCTION__.spill_failure, %r8
	movq	$1910, %rcx             # imm = 0x776
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	_fatal_insn
.LBB3_538:                              # %if.then962
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.reload, %rcx
	movq	$1113, %rsi             # imm = 0x459
	jmp	.LBB3_539
.LBB3_544:                              # %if.then.i1728
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.verify_initial_elim_offsets, %rcx
	movq	$3339, %rsi             # imm = 0xD0B
.LBB3_539:                              # %if.then962
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp55:
	.size	reload, .Ltmp55-reload
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI3_0:
	.quad	.LBB3_145
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_145
	.quad	.LBB3_175
	.quad	.LBB3_145
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_144
	.quad	.LBB3_175
	.quad	.LBB3_145
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_175
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_145
	.quad	.LBB3_144
	.quad	.LBB3_175
	.quad	.LBB3_144
	.quad	.LBB3_145
.LJTI3_1:
	.quad	.LBB3_113
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_113
	.quad	.LBB3_122
	.quad	.LBB3_113
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_121
	.quad	.LBB3_122
	.quad	.LBB3_113
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_122
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_113
	.quad	.LBB3_121
	.quad	.LBB3_122
	.quad	.LBB3_121
	.quad	.LBB3_113

	.text
	.globl	mark_home_live
	.align	16, 0x90
	.type	mark_home_live,@function
mark_home_live:                         # @mark_home_live
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp57:
	.cfi_def_cfa_offset 16
	movq	$32, %rcx
	movq	reg_renumber(%rip), %rax
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movswq	(%rax,%rdi,2), %rsi
	testq	%rsi, %rsi
	js	.LBB4_18
# BB#1:                                 # %if.end
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	(%rax,%rdi,8), %rax
	movzbl	2(%rax), %eax
	movq	%rsi, %rcx
	andq	$65528, %rcx            # imm = 0xFFF8
	cmpq	$8, %rcx
	je	.LBB4_5
# BB#2:                                 # %if.end
	leaq	-21(%rsi), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB4_5
# BB#3:                                 # %if.end
	leaq	-45(%rsi), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB4_5
# BB#4:                                 # %if.end
	leaq	-29(%rsi), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$7, %rcx
	jbe	.LBB4_5
# BB#10:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB4_11
# BB#13:                                # %cond.true61
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB4_15
.LBB4_5:                                # %cond.true
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB4_15
# BB#6:                                 # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB4_7
# BB#8:                                 # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB4_15
.LBB4_11:                               # %cond.false
	cmpq	$18, %rax
	jne	.LBB4_14
# BB#12:                                # %cond.true49
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB4_15
.LBB4_7:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB4_15
.LBB4_14:                               # %cond.end85
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB4_18
.LBB4_15:                               # %while.body.lr.ph
	addq	%rsi, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	1(%rsi), %rcx
	cmpq	%rcx, %rdx
	leaq	regs_ever_live(%rsi), %rdi
	jge	.LBB4_17
# BB#16:                                # %while.body.lr.ph
	movq	%rcx, %rax
.LBB4_17:                               # %while.body.lr.ph
	decq	%rax
	subq	%rsi, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	incq	%rdx
	movq	$1, %rsi
	callq	memset
.LBB4_18:                               # %while.end
	popq	%rax
	retq
.Ltmp58:
	.size	mark_home_live, .Ltmp58-mark_home_live
	.cfi_endproc

	.align	16, 0x90
	.type	scan_paradoxical_subregs,@function
scan_paradoxical_subregs:               # @scan_paradoxical_subregs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp65:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp66:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp67:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp68:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp69:
	.cfi_def_cfa_offset 48
.Ltmp70:
	.cfi_offset %rbx, -48
.Ltmp71:
	.cfi_offset %r12, -40
.Ltmp72:
	.cfi_offset %r13, -32
.Ltmp73:
	.cfi_offset %r14, -24
.Ltmp74:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movl	(%r12), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	leaq	-48(%rdx), %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$21, %rcx
	ja	.LBB5_7
# BB#1:                                 # %entry
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$3681731, %rsi          # imm = 0x382DC3
	jne	.LBB5_6
# BB#2:                                 # %entry
	cmpq	$15, %rcx
	jne	.LBB5_7
# BB#3:                                 # %sw.bb2
	movq	8(%r12), %rdx
	movl	(%rdx), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB5_6
# BB#4:                                 # %land.lhs.true
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %eax
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movzbl	mode_size(%rsi), %ecx
	cmpq	%rcx, %rax
	jbe	.LBB5_6
# BB#5:                                 # %if.then
	movl	8(%rdx), %ecx
	movq	reg_max_ref_width(%rip), %rdx
	movl	%eax, (%rdx,%rcx,4)
	jmp	.LBB5_6
.LBB5_7:                                # %sw.epilog
	movzbl	rtx_length(%rdx), %r14d
	testq	%r14, %r14
	je	.LBB5_6
# BB#8:                                 # %for.body.lr.ph
	movq	rtx_format(,%rdx,8), %r15
	.align	16, 0x90
.LBB5_9:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_14 Depth 2
	movq	%r14, %r13
	movzbl	-1(%r15,%r13), %eax
	leaq	-1(%r13), %r14
	cmpq	$69, %rax
	jne	.LBB5_10
# BB#13:                                # %if.then54
                                        #   in Loop: Header=BB5_9 Depth=1
	movq	(%r12,%r13,8), %rax
	movslq	(%rax), %rbx
	testq	%rbx, %rbx
	jle	.LBB5_11
	jmp	.LBB5_14
	.align	16, 0x90
.LBB5_15:                               # %for.body62.for.body62_crit_edge
                                        #   in Loop: Header=BB5_14 Depth=2
	movq	(%r12,%r13,8), %rax
.LBB5_14:                               # %for.body62.lr.ph
                                        #   Parent Loop BB5_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%rbx
	movq	8(%rax,%rbx,8), %rdi
	callq	scan_paradoxical_subregs
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB5_15
	jmp	.LBB5_11
	.align	16, 0x90
.LBB5_10:                               # %for.body
                                        #   in Loop: Header=BB5_9 Depth=1
	cmpq	$101, %rax
	jne	.LBB5_11
# BB#12:                                # %if.then44
                                        #   in Loop: Header=BB5_9 Depth=1
	movq	(%r12,%r13,8), %rdi
	callq	scan_paradoxical_subregs
.LBB5_11:                               # %for.cond.backedge
                                        #   in Loop: Header=BB5_9 Depth=1
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB5_9
.LBB5_6:                                # %for.end73
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp75:
	.size	scan_paradoxical_subregs, .Ltmp75-scan_paradoxical_subregs
	.cfi_endproc

	.align	16, 0x90
	.type	alter_reg,@function
alter_reg:                              # @alter_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp82:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp83:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp84:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp85:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp86:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp87:
	.cfi_def_cfa_offset 64
.Ltmp88:
	.cfi_offset %rbx, -48
.Ltmp89:
	.cfi_offset %r12, -40
.Ltmp90:
	.cfi_offset %r13, -32
.Ltmp91:
	.cfi_offset %r14, -24
.Ltmp92:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	%rdi, %r13
	movq	$32, %rcx
	movq	%r13, %rbx
	movq	80(%rax), %rax
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	(%rax,%rbx,8), %rax
	movq	%rsi, %r14
	testq	%rax, %rax
	je	.LBB6_43
# BB#1:                                 # %if.end
	movl	(%rax), %edi
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB6_43
# BB#2:                                 # %if.end7
	movq	reg_renumber(%rip), %rcx
	movswq	(%rcx,%rbx,2), %rcx
	testq	%rcx, %rcx
	jns	.LBB6_4
# BB#3:                                 # %if.end7
	movq	%r13, %rcx
.LBB6_4:                                # %if.end7
	movl	%ecx, 8(%rax)
	jns	.LBB6_43
# BB#5:                                 # %land.lhs.true
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%rbx,8), %rax
	movslq	16(%rax), %rax
	testq	%rax, %rax
	jle	.LBB6_43
# BB#6:                                 # %land.lhs.true29
	movq	reg_equiv_constant(%rip), %rax
	cmpq	$0, (%rax,%rbx,8)
	jne	.LBB6_43
# BB#7:                                 # %land.lhs.true34
	movq	reg_equiv_memory_loc(%rip), %rax
	cmpq	$0, (%rax,%rbx,8)
	jne	.LBB6_43
# BB#8:                                 # %if.then39
	movq	$16, %rcx
	movq	reg_max_ref_width(%rip), %rax
	shrq	%cl, %rdi
	andq	$255, %rdi
	movzbl	mode_size(%rdi), %r9d
	movl	(%rax,%rbx,4), %eax
	cmpq	%rax, %r9
	movq	%r9, %rsi
	ja	.LBB6_10
# BB#9:                                 # %if.then39
	movq	%rax, %rsi
.LBB6_10:                               # %if.then39
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%r14, %rdx
	andq	%r8, %rdx
	cmpq	%r8, %rdx
	je	.LBB6_11
# BB#14:                                # %if.else
	movq	$32, %rcx
	movq	%r14, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	spill_stack_slot(,%r15,8), %r12
	testq	%r12, %r12
	je	.LBB6_15
# BB#16:                                # %land.lhs.true87
	movl	spill_stack_slot_width(,%r15,4), %eax
	movq	%rsi, %rcx
	andq	%r8, %rcx
	cmpq	%rcx, %rax
	jb	.LBB6_21
# BB#17:                                # %land.lhs.true92
	movzbl	2(%r12), %eax
	movzbl	mode_size(%rax), %eax
	cmpq	%r9, %rax
	jae	.LBB6_18
# BB#19:                                # %if.else106
	testq	%r12, %r12
	je	.LBB6_20
.LBB6_21:                               # %if.then116
	movzbl	2(%r12), %r10d
	movzbl	mode_size(%r10), %eax
	cmpq	%r9, %rax
	ja	.LBB6_23
# BB#22:                                # %select.mid
	movq	%rdi, %r10
.LBB6_23:                               # %select.end
	movl	spill_stack_slot_width(,%r15,4), %edi
	cmpq	%rcx, %rdi
	ja	.LBB6_25
# BB#24:                                # %select.end
	movq	%rsi, %rdi
.LBB6_25:                               # %select.end
	movq	%rdi, %rsi
	jmp	.LBB6_26
.LBB6_11:                               # %if.then61
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	andq	%r8, %rsi
	cmpq	%rsi, %r9
	movq	$-1, %rdx
	jne	.LBB6_13
# BB#12:                                # %if.then61
	xorq	%rdx, %rdx
.LBB6_13:                               # %if.then61
	callq	assign_stack_local
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	80(%rcx), %rcx
	movq	(%rcx,%rbx,8), %rcx
	movq	%rax, %r12
	movl	(%rcx), %eax
	movl	(%r12), %ecx
	andq	$67108864, %rax         # imm = 0x4000000
	movabsq	$4227858431, %rdx       # imm = 0xFBFFFFFF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, (%r12)
	callq	new_alias_set
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	set_mem_alias_set
	jmp	.LBB6_34
.LBB6_15:
	movq	%rdi, %r10
.LBB6_26:                               # %if.end142
	movq	%rsi, (%rsp)            # 8-byte Spill
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	andq	%r8, %rsi
	cmpq	%rsi, %r9
	movq	$-1, %rdx
	jne	.LBB6_28
# BB#27:                                # %if.end142
	xorq	%rdx, %rdx
.LBB6_28:                               # %if.end142
	movq	%r10, %rdi
	callq	assign_stack_local
	movq	spill_stack_slot(,%r15,8), %rcx
	movq	%rax, %r12
	testq	%rcx, %rcx
	je	.LBB6_32
# BB#29:                                # %if.then151
	movq	16(%rcx), %rax
	xorq	%rsi, %rsi
	testq	%rax, %rax
	je	.LBB6_31
# BB#30:                                # %cond.false159
	movq	(%rax), %rsi
.LBB6_31:                               # %cond.end165
	movq	%r12, %rdi
	jmp	.LBB6_33
.LBB6_32:                               # %if.else167
	callq	new_alias_set
	movq	%r12, %rdi
	movq	%rax, %rsi
.LBB6_33:                               # %if.end169
	callq	set_mem_alias_set
	movq	%r12, spill_stack_slot(,%r15,8)
	movq	(%rsp), %rax            # 8-byte Reload
	movl	%eax, spill_stack_slot_width(,%r15,4)
	jmp	.LBB6_34
.LBB6_18:
	movq	%rdx, 8(%rsp)           # 8-byte Spill
.LBB6_34:                               # %if.end175
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	(%rax,%rbx,8), %rax
	movzbl	2(%rax), %esi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r12, %rdi
	callq	adjust_address_1
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	72(%rcx), %rcx
	movq	(%rcx,%rbx,8), %rsi
	movq	%rax, %r15
	testq	%rsi, %rsi
	je	.LBB6_42
# BB#35:                                # %if.then189
	movq	144(%rsi), %rax
	testq	%rax, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	je	.LBB6_42
# BB#36:                                # %land.lhs.true229
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB6_42
# BB#37:                                # %land.lhs.true234
	movl	8(%rax), %eax
	andq	%rdx, %r13
	cmpq	%r13, %rax
	jne	.LBB6_42
# BB#38:                                # %if.then240
	cmpq	%rdx, 8(%rsp)           # 8-byte Folded Reload
	je	.LBB6_41
# BB#39:                                # %land.lhs.true243
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	spill_stack_slot(,%r14,8), %rax
	cmpq	%r15, %rax
	jne	.LBB6_41
# BB#40:                                # %if.then248
	movq	%r15, %rdi
	callq	copy_rtx
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	72(%rcx), %rcx
	movq	(%rcx,%rbx,8), %rsi
	movq	%rax, %r15
.LBB6_41:                               # %if.end250
	movq	%r15, %rdi
	callq	set_mem_expr
.LBB6_42:                               # %if.end256
	movq	reg_equiv_memory_loc(%rip), %rax
	movq	%r15, (%rax,%rbx,8)
.LBB6_43:                               # %if.end259
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB6_20:
	movq	%rdi, %r10
	jmp	.LBB6_26
.Ltmp93:
	.size	alter_reg, .Ltmp93-alter_reg
	.cfi_endproc

	.align	16, 0x90
	.type	mark_not_eliminable,@function
mark_not_eliminable:                    # @mark_not_eliminable
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %eax
	cmpq	$63, %rax
	jne	.LBB7_2
# BB#1:                                 # %if.then
	movq	8(%rdi), %rdi
.LBB7_2:                                # %if.end
	movq	global_rtl+32(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB7_12
# BB#3:                                 # %for.cond.preheader
	movq	reg_eliminate(%rip), %rax
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB7_4:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	12(%rax,%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB7_11
# BB#5:                                 # %land.lhs.true
                                        #   in Loop: Header=BB7_4 Depth=1
	movq	40(%rax,%rcx), %rdx
	cmpq	%rdx, %rdi
	jne	.LBB7_11
# BB#6:                                 # %land.lhs.true11
                                        #   in Loop: Header=BB7_4 Depth=1
	movzbl	(%rsi), %edx
	cmpq	$47, %rdx
	jne	.LBB7_10
# BB#7:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB7_4 Depth=1
	movq	16(%rsi), %r8
	movzbl	(%r8), %edx
	cmpq	$75, %rdx
	jne	.LBB7_10
# BB#8:                                 # %lor.lhs.false23
                                        #   in Loop: Header=BB7_4 Depth=1
	movq	8(%r8), %rdx
	cmpq	%rdi, %rdx
	jne	.LBB7_10
# BB#9:                                 # %lor.lhs.false32
                                        #   in Loop: Header=BB7_4 Depth=1
	movq	16(%r8), %rdx
	movzbl	(%rdx), %edx
	cmpq	$54, %rdx
	je	.LBB7_11
	.align	16, 0x90
.LBB7_10:                               # %if.then43
                                        #   in Loop: Header=BB7_4 Depth=1
	movq	$0, 12(%rax,%rcx)
	movl	num_eliminable(%rip), %edx
	decq	%rdx
	movl	%edx, num_eliminable(%rip)
.LBB7_11:                               # %for.inc
                                        #   in Loop: Header=BB7_4 Depth=1
	addq	$48, %rcx
	cmpq	$192, %rcx
	jne	.LBB7_4
.LBB7_12:                               # %for.end
	retq
.Ltmp94:
	.size	mark_not_eliminable, .Ltmp94-mark_not_eliminable
	.cfi_endproc

	.align	16, 0x90
	.type	spill_hard_reg,@function
spill_hard_reg:                         # @spill_hard_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp101:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp102:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp103:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp104:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp105:
	.cfi_def_cfa_offset 48
.Ltmp106:
	.cfi_offset %rbx, -48
.Ltmp107:
	.cfi_offset %r12, -40
.Ltmp108:
	.cfi_offset %r13, -32
.Ltmp109:
	.cfi_offset %r14, -24
.Ltmp110:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%r14, %rax
	movq	$1, %rdx
	movq	%r14, %rcx
	shlq	%cl, %rdx
	orq	%rdx, bad_spill_regs_global(%rip)
	movslq	max_regno(%rip), %r8
	andq	%r12, %rax
	movq	$1, %rcx
	movb	%cl, regs_ever_live(%rax)
	cmpq	$54, %r8
	jl	.LBB8_20
# BB#1:
	movq	$53, %rbx
	movabsq	$0, %r13
	movabsq	$spilled_pseudos, %r15
	.align	16, 0x90
.LBB8_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%rbx,2), %rdi
	testq	%rdi, %rdi
	js	.LBB8_19
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	%r14, %rsi
	andq	%r12, %rsi
	movq	%rdi, %rax
	andq	%r12, %rax
	cmpq	%rsi, %rax
	ja	.LBB8_19
# BB#4:                                 # %land.lhs.true10
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	(%rax,%rbx,8), %rax
	movzbl	2(%rax), %eax
	movq	%rdi, %rcx
	andq	$65528, %rcx            # imm = 0xFFF8
	cmpq	$8, %rcx
	je	.LBB8_8
# BB#5:                                 # %land.lhs.true10
                                        #   in Loop: Header=BB8_2 Depth=1
	leaq	-21(%rdi), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB8_8
# BB#6:                                 # %land.lhs.true10
                                        #   in Loop: Header=BB8_2 Depth=1
	leaq	-45(%rdi), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB8_8
# BB#7:                                 # %land.lhs.true10
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	%rdi, %rcx
	addq	$-29, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$7, %rcx
	jbe	.LBB8_8
# BB#12:                                # %cond.false
                                        #   in Loop: Header=BB8_2 Depth=1
	cmpq	$24, %rax
	jne	.LBB8_13
# BB#15:                                # %cond.true99
                                        #   in Loop: Header=BB8_2 Depth=1
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB8_17
	.align	16, 0x90
.LBB8_8:                                # %cond.true
                                        #   in Loop: Header=BB8_2 Depth=1
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB8_17
# BB#9:                                 # %lor.rhs
                                        #   in Loop: Header=BB8_2 Depth=1
	cmpq	$6, %rcx
	movq	%r13, %rax
	jne	.LBB8_11
# BB#10:                                # %lor.rhs
                                        #   in Loop: Header=BB8_2 Depth=1
	movabsq	$1, %rax
.LBB8_11:                               # %lor.rhs
                                        #   in Loop: Header=BB8_2 Depth=1
	incq	%rax
	jmp	.LBB8_17
.LBB8_13:                               # %cond.false
                                        #   in Loop: Header=BB8_2 Depth=1
	cmpq	$18, %rax
	jne	.LBB8_16
# BB#14:                                # %cond.true86
                                        #   in Loop: Header=BB8_2 Depth=1
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB8_17
.LBB8_16:                               # %cond.false103
                                        #   in Loop: Header=BB8_2 Depth=1
	movl	target_flags(%rip), %r9d
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %r9
	andq	$4, %r9
	leaq	3(%rax,%r9), %rax
	addq	$4, %r9
	cqto
	idivq	%r9
	.align	16, 0x90
.LBB8_17:                               # %cond.end123
                                        #   in Loop: Header=BB8_2 Depth=1
	addq	%rdi, %rax
	andq	%r12, %rax
	cmpq	%rsi, %rax
	jbe	.LBB8_19
# BB#18:                                # %if.then128
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	bitmap_set_bit
	movl	max_regno(%rip), %r8d
.LBB8_19:                               # %for.inc
                                        #   in Loop: Header=BB8_2 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB8_2
.LBB8_20:                               # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp111:
	.size	spill_hard_reg, .Ltmp111-spill_hard_reg
	.cfi_endproc

	.align	16, 0x90
	.type	finish_spills,@function
finish_spills:                          # @finish_spills
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
	subq	$32, %rsp
.Ltmp123:
	.cfi_def_cfa_offset 80
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
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movl	num_eliminable(%rip), %eax
	movq	used_spill_regs(%rip), %r10
	xorq	%rsi, %rsi
	movl	%esi, n_spills(%rip)
	movq	$1, %r8
	movq	$65535, %r9             # imm = 0xFFFF
	testq	%rax, %rax
	je	.LBB9_1
# BB#6:
	xorq	%rax, %rax
	xorq	%r15, %r15
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB9_7:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %r10
	jne	.LBB9_8
# BB#11:                                # %if.else
                                        #   in Loop: Header=BB9_7 Depth=1
	movw	%r9w, spill_reg_order(%rax,%rax)
	movq	%rsi, %rdx
	jmp	.LBB9_12
	.align	16, 0x90
.LBB9_8:                                # %if.end
                                        #   in Loop: Header=BB9_7 Depth=1
	movw	%si, spill_reg_order(%rax,%rax)
	leaq	1(%rsi), %rdx
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	regs_ever_live(%rax), %ecx
	movl	%edx, n_spills(%rip)
	movw	%bx, spill_regs(%rsi,%rsi)
	testq	%rcx, %rcx
	movq	$1, %rcx
	je	.LBB9_10
# BB#9:                                 # %select.mid
                                        #   in Loop: Header=BB9_7 Depth=1
	movq	%r15, %rcx
.LBB9_10:                               # %select.end
                                        #   in Loop: Header=BB9_7 Depth=1
	movb	%r8b, regs_ever_live(%rax)
	movq	%rcx, %r15
.LBB9_12:                               # %for.inc
                                        #   in Loop: Header=BB9_7 Depth=1
	incq	%rax
	incq	%rbx
	cmpq	$53, %rax
	movq	%rdx, %rsi
	jne	.LBB9_7
	jmp	.LBB9_13
.LBB9_1:
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	xorq	%r15, %r15
	.align	16, 0x90
.LBB9_2:                                # %for.body.us
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %r10
	jne	.LBB9_3
# BB#4:                                 # %if.else.us
                                        #   in Loop: Header=BB9_2 Depth=1
	movw	%r9w, spill_reg_order(%rax,%rax)
	jmp	.LBB9_5
	.align	16, 0x90
.LBB9_3:                                # %if.end.us
                                        #   in Loop: Header=BB9_2 Depth=1
	movw	%si, spill_reg_order(%rax,%rax)
	leaq	1(%rsi), %rdx
	movl	%edx, n_spills(%rip)
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movw	%bx, spill_regs(%rsi,%rsi)
	movb	%r8b, regs_ever_live(%rax)
	movq	%rdx, %rsi
.LBB9_5:                                # %for.inc.us
                                        #   in Loop: Header=BB9_2 Depth=1
	incq	%rax
	incq	%rbx
	cmpq	$53, %rax
	jne	.LBB9_2
.LBB9_13:                               # %do.body
	movq	spilled_pseudos(%rip), %r9
	testq	%r9, %r9
	je	.LBB9_26
# BB#14:                                # %for.cond31.preheader.lr.ph
	movl	16(%r9), %eax
	testq	%rax, %rax
	movq	$53, %rbx
	je	.LBB9_16
# BB#15:                                # %select.mid23
	xorq	%rbx, %rbx
.LBB9_16:                               # %select.end22
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB9_17:                               # %for.cond31.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_18 Depth 2
                                        #       Child Loop BB9_20 Depth 3
	movq	reg_renumber(%rip), %r13
	movq	pseudo_previous_regs(%rip), %r11
	xorq	%r14, %r14
	.align	16, 0x90
.LBB9_18:                               # %for.body34
                                        #   Parent Loop BB9_17 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB9_20 Depth 3
	movq	24(%r9,%r14,8), %rsi
	testq	%rsi, %rsi
	je	.LBB9_24
# BB#19:                                # %for.body43.preheader
                                        #   in Loop: Header=BB9_18 Depth=2
	movq	$6, %rcx
	movq	%r14, %r12
	shlq	%cl, %r12
	movq	%rbx, %rax
	andq	%r10, %rax
	.align	16, 0x90
.LBB9_20:                               # %for.body43
                                        #   Parent Loop BB9_17 Depth=1
                                        #     Parent Loop BB9_18 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	%rsi, %rdx
	je	.LBB9_23
# BB#21:                                # %if.then49
                                        #   in Loop: Header=BB9_20 Depth=3
	movl	16(%r9), %edi
	movq	$7, %rcx
	shlq	%cl, %rdi
	leaq	(%rax,%r12), %r8
	addq	%rdi, %r8
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movswq	(%r13,%r8,2), %rcx
	testq	%rcx, %rcx
	js	.LBB9_79
# BB#22:                                # %if.end60
                                        #   in Loop: Header=BB9_20 Depth=3
	notq	%rdx
	andq	%rdx, %rsi
	movq	$1, %rdx
	shlq	%cl, %rdx
	orq	%rdx, (%r11,%r8,8)
	movq	$1, %r15
	movq	$65535, %rcx            # imm = 0xFFFF
	movw	%cx, (%r13,%r8,2)
	testq	%rsi, %rsi
	je	.LBB9_24
.LBB9_23:                               # %for.inc75
                                        #   in Loop: Header=BB9_20 Depth=3
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r10, %rcx
	incq	%rax
	cmpq	$64, %rcx
	jb	.LBB9_20
.LBB9_24:                               # %if.end78
                                        #   in Loop: Header=BB9_18 Depth=2
	incq	%r14
	movq	%r14, %rax
	andq	%r10, %rax
	xorq	%rbx, %rbx
	cmpq	$2, %rax
	jb	.LBB9_18
# BB#25:                                # %for.end81
                                        #   in Loop: Header=BB9_17 Depth=1
	movq	(%r9), %r9
	xorq	%rbx, %rbx
	testq	%r9, %r9
	jne	.LBB9_17
.LBB9_26:                               # %do.end
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%r14, %rax
	je	.LBB9_35
# BB#27:                                # %if.then86
	movslq	max_regno(%rip), %rdx
	movq	pseudo_forbidden_regs(%rip), %rdi
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movq	insns_need_reload(%rip), %r8
	jmp	.LBB9_28
	.align	16, 0x90
.LBB9_68:                               # %for.inc242
                                        #   in Loop: Header=BB9_28 Depth=1
	movq	16(%r8), %r8
.LBB9_28:                               # %if.then86
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_44 Depth 2
                                        #       Child Loop BB9_46 Depth 3
                                        #       Child Loop BB9_50 Depth 3
                                        #     Child Loop BB9_58 Depth 2
                                        #       Child Loop BB9_60 Depth 3
                                        #       Child Loop BB9_64 Depth 3
	testq	%r8, %r8
	je	.LBB9_29
# BB#40:                                # %do.body92
                                        #   in Loop: Header=BB9_28 Depth=1
	movq	40(%r8), %r10
	testq	%r10, %r10
	je	.LBB9_54
# BB#41:                                # %for.cond120.preheader.lr.ph
                                        #   in Loop: Header=BB9_28 Depth=1
	movl	16(%r10), %eax
	testq	%rax, %rax
	movq	$53, %rbx
	je	.LBB9_43
# BB#42:                                # %select.mid32
                                        #   in Loop: Header=BB9_28 Depth=1
	xorq	%rbx, %rbx
.LBB9_43:                               # %select.end31
                                        #   in Loop: Header=BB9_28 Depth=1
	movq	pseudo_forbidden_regs(%rip), %r9
	.align	16, 0x90
.LBB9_44:                               # %for.cond120.preheader
                                        #   Parent Loop BB9_28 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB9_46 Depth 3
                                        #       Child Loop BB9_50 Depth 3
	movq	24(%r10), %rdx
	testq	%rdx, %rdx
	je	.LBB9_49
# BB#45:                                # %for.body134.preheader
                                        #   in Loop: Header=BB9_44 Depth=2
	movq	%rbx, %rax
	andq	%r14, %rax
	.align	16, 0x90
.LBB9_46:                               # %for.body134
                                        #   Parent Loop BB9_28 Depth=1
                                        #     Parent Loop BB9_44 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	testq	%rdx, %rdi
	je	.LBB9_48
# BB#47:                                # %if.then141
                                        #   in Loop: Header=BB9_46 Depth=3
	movl	16(%r10), %esi
	movq	$7, %rcx
	shlq	%cl, %rsi
	addq	%rax, %rsi
	movq	$32, %rcx
	movq	104(%r8), %r11
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	orq	%r11, (%r9,%rsi,8)
	notq	%rdi
	andq	%rdi, %rdx
	je	.LBB9_49
.LBB9_48:                               # %for.inc154
                                        #   in Loop: Header=BB9_46 Depth=3
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r14, %rcx
	incq	%rax
	cmpq	$64, %rcx
	jb	.LBB9_46
.LBB9_49:                               # %if.end157
                                        #   in Loop: Header=BB9_44 Depth=2
	movq	32(%r10), %rdx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	movabsq	$0, %rbx
	je	.LBB9_53
	.align	16, 0x90
.LBB9_50:                               # %for.body134.1
                                        #   Parent Loop BB9_28 Depth=1
                                        #     Parent Loop BB9_44 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	testq	%rdx, %rdi
	je	.LBB9_52
# BB#51:                                # %if.then141.1
                                        #   in Loop: Header=BB9_50 Depth=3
	movl	16(%r10), %esi
	movq	$7, %rcx
	shlq	%cl, %rsi
	leaq	64(%rax,%rsi), %rsi
	movq	$32, %rcx
	movq	104(%r8), %r11
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	orq	%r11, (%r9,%rsi,8)
	notq	%rdi
	andq	%rdi, %rdx
	je	.LBB9_53
.LBB9_52:                               # %for.inc154.1
                                        #   in Loop: Header=BB9_50 Depth=3
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r14, %rcx
	incq	%rax
	cmpq	$64, %rcx
	jb	.LBB9_50
.LBB9_53:                               # %if.end157.1
                                        #   in Loop: Header=BB9_44 Depth=2
	movq	(%r10), %r10
	xorq	%rbx, %rbx
	testq	%r10, %r10
	jne	.LBB9_44
.LBB9_54:                               # %do.body165
                                        #   in Loop: Header=BB9_28 Depth=1
	movq	64(%r8), %r10
	testq	%r10, %r10
	je	.LBB9_68
# BB#55:                                # %for.cond194.preheader.lr.ph
                                        #   in Loop: Header=BB9_28 Depth=1
	movl	16(%r10), %eax
	testq	%rax, %rax
	movq	$53, %rbx
	je	.LBB9_57
# BB#56:                                # %select.mid40
                                        #   in Loop: Header=BB9_28 Depth=1
	xorq	%rbx, %rbx
.LBB9_57:                               # %select.end39
                                        #   in Loop: Header=BB9_28 Depth=1
	movq	pseudo_forbidden_regs(%rip), %r9
	.align	16, 0x90
.LBB9_58:                               # %for.cond194.preheader
                                        #   Parent Loop BB9_28 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB9_60 Depth 3
                                        #       Child Loop BB9_64 Depth 3
	movq	24(%r10), %rdx
	testq	%rdx, %rdx
	je	.LBB9_63
# BB#59:                                # %for.body208.preheader
                                        #   in Loop: Header=BB9_58 Depth=2
	movq	%rbx, %rax
	andq	%r14, %rax
	.align	16, 0x90
.LBB9_60:                               # %for.body208
                                        #   Parent Loop BB9_28 Depth=1
                                        #     Parent Loop BB9_58 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	testq	%rdx, %rdi
	je	.LBB9_62
# BB#61:                                # %if.then215
                                        #   in Loop: Header=BB9_60 Depth=3
	movl	16(%r10), %esi
	movq	$7, %rcx
	shlq	%cl, %rsi
	addq	%rax, %rsi
	movq	$32, %rcx
	movq	104(%r8), %r11
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	orq	%r11, (%r9,%rsi,8)
	notq	%rdi
	andq	%rdi, %rdx
	je	.LBB9_63
.LBB9_62:                               # %for.inc231
                                        #   in Loop: Header=BB9_60 Depth=3
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r14, %rcx
	incq	%rax
	cmpq	$64, %rcx
	jb	.LBB9_60
.LBB9_63:                               # %if.end234
                                        #   in Loop: Header=BB9_58 Depth=2
	movq	32(%r10), %rdx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	movabsq	$0, %rbx
	je	.LBB9_67
	.align	16, 0x90
.LBB9_64:                               # %for.body208.1
                                        #   Parent Loop BB9_28 Depth=1
                                        #     Parent Loop BB9_58 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	testq	%rdx, %rdi
	je	.LBB9_66
# BB#65:                                # %if.then215.1
                                        #   in Loop: Header=BB9_64 Depth=3
	movl	16(%r10), %esi
	movq	$7, %rcx
	shlq	%cl, %rsi
	leaq	64(%rax,%rsi), %rsi
	movq	$32, %rcx
	movq	104(%r8), %r11
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	orq	%r11, (%r9,%rsi,8)
	notq	%rdi
	andq	%rdi, %rdx
	je	.LBB9_67
.LBB9_66:                               # %for.inc231.1
                                        #   in Loop: Header=BB9_64 Depth=3
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r14, %rcx
	incq	%rax
	cmpq	$64, %rcx
	jb	.LBB9_64
.LBB9_67:                               # %if.end234.1
                                        #   in Loop: Header=BB9_58 Depth=2
	movq	(%r10), %r10
	xorq	%rbx, %rbx
	testq	%r10, %r10
	jne	.LBB9_58
	jmp	.LBB9_68
.LBB9_29:                               # %for.cond244.preheader
	movslq	max_regno(%rip), %rax
	cmpq	$53, %rax
	jle	.LBB9_35
# BB#30:
	movq	$53, %r14
	movabsq	$spilled_pseudos, %rbx
	.align	16, 0x90
.LBB9_31:                               # %for.body247
                                        # =>This Inner Loop Header: Depth=1
	movq	reg_old_renumber(%rip), %rax
	movq	reg_renumber(%rip), %rcx
	movzbl	(%rax,%r14,2), %eax
	movzbl	(%rcx,%r14,2), %ecx
	cmpq	%rcx, %rax
	je	.LBB9_34
# BB#32:                                # %if.then256
                                        #   in Loop: Header=BB9_31 Depth=1
	movq	pseudo_forbidden_regs(%rip), %rax
	movq	pseudo_previous_regs(%rip), %rcx
	movq	bad_spill_regs_global(%rip), %rdx
	movq	(%rax,%r14,8), %rax
	movq	(%rcx,%r14,8), %rsi
	orq	%rdx, %rax
	orq	%rax, %rsi
	movq	%r14, %rdi
	callq	retry_global_alloc
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%r14,2), %rax
	testq	%rax, %rax
	js	.LBB9_34
# BB#33:                                # %if.then268
                                        #   in Loop: Header=BB9_31 Depth=1
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	bitmap_clear_bit
.LBB9_34:                               # %for.inc271
                                        #   in Loop: Header=BB9_31 Depth=1
	incq	%r14
	movslq	max_regno(%rip), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB9_31
.LBB9_35:                               # %for.cond275.preheader
	movq	reload_insn_chain(%rip), %r12
	testq	%r12, %r12
	je	.LBB9_70
# BB#36:
	leaq	24(%rsp), %r13
	.align	16, 0x90
.LBB9_37:                               # %for.body277
                                        # =>This Inner Loop Header: Depth=1
	leaq	40(%r12), %r14
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%r14, %rsi
	movabsq	$spilled_pseudos, %rbx
	movq	%rbx, %rdx
	callq	bitmap_operation
	leaq	64(%r12), %rdi
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	$1, %rcx
	movq	%rdi, %rsi
	movq	%rbx, %rdx
	callq	bitmap_operation
	movzbl	262(%r12), %eax
	testq	$1, %rax
	je	.LBB9_69
# BB#38:                                # %do.body285
                                        #   in Loop: Header=BB9_37 Depth=1
	movq	$0, 24(%rsp)
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	reg_set_to_hard_reg_set
	movq	$0, 16(%rsp)
	leaq	16(%rsp), %rdi
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rsi
	callq	reg_set_to_hard_reg_set
	movq	16(%rsp), %rax
	orq	%rax, 24(%rsp)
	movq	104(%r12), %rax
	movq	%rax, 16(%rsp)
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	compute_use_by_pseudos
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	compute_use_by_pseudos
	movq	24(%rsp), %rax
	movq	%rax, %rcx
	notq	%rcx
	movq	%rcx, 104(%r12)
	movq	used_spill_regs(%rip), %rdx
	andq	%rdx, %rcx
	movq	%rcx, 104(%r12)
	movq	16(%rsp), %rcx
	notq	%rdx
	orq	%rax, %rdx
	testq	%rdx, %rcx
	jne	.LBB9_39
.LBB9_69:                               # %for.inc307
                                        #   in Loop: Header=BB9_37 Depth=1
	movq	(%r12), %r12
	testq	%r12, %r12
	jne	.LBB9_37
.LBB9_70:                               # %for.cond310.preheader
	movslq	max_regno(%rip), %rax
	cmpq	$53, %rax
	jle	.LBB9_78
# BB#71:
	movq	$53, %r14
	movabsq	$.L.str10, %r13
	movabsq	$.L.str11, %r12
	.align	16, 0x90
.LBB9_72:                               # %for.body313
                                        # =>This Inner Loop Header: Depth=1
	movq	reg_renumber(%rip), %rax
	movq	reg_old_renumber(%rip), %rcx
	movzbl	(%rax,%r14,2), %ebx
	movzbl	(%rcx,%r14,2), %esi
	cmpq	%rbx, %rsi
	je	.LBB9_77
# BB#73:                                # %if.end323
                                        #   in Loop: Header=BB9_72 Depth=1
	movq	$48, %rcx
	shlq	%cl, %rsi
	movq	$48, %rcx
	sarq	%cl, %rsi
	movq	%r14, %rdi
	callq	alter_reg
	movq	reg_old_renumber(%rip), %rax
	movw	%bx, (%rax,%r14,2)
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB9_77
# BB#74:                                # %if.then331
                                        #   in Loop: Header=BB9_72 Depth=1
	cmpq	$65535, %rbx            # imm = 0xFFFF
	jne	.LBB9_76
# BB#75:                                # %if.then334
                                        #   in Loop: Header=BB9_72 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	fprintf
	jmp	.LBB9_77
.LBB9_76:                               # %if.else336
                                        #   in Loop: Header=BB9_72 Depth=1
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%r14,2), %rcx
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r14, %rdx
	callq	fprintf
	.align	16, 0x90
.LBB9_77:                               # %for.inc343
                                        #   in Loop: Header=BB9_72 Depth=1
	incq	%r14
	movslq	max_regno(%rip), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB9_72
.LBB9_78:                               # %for.end345
	movq	%r15, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB9_79:                               # %if.then59
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.finish_spills, %rdx
	movq	$3642, %rsi             # imm = 0xE3A
	callq	fancy_abort
.LBB9_39:                               # %if.end305
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.finish_spills, %rcx
	movq	$3721, %rsi             # imm = 0xE89
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp129:
	.size	finish_spills, .Ltmp129-finish_spills
	.cfi_endproc

	.globl	eliminate_regs
	.align	16, 0x90
	.type	eliminate_regs,@function
eliminate_regs:                         # @eliminate_regs
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
	subq	$80, %rsp
.Ltmp141:
	.cfi_def_cfa_offset 128
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
	movq	current_function_decl(%rip), %rax
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	%rsi, %r12
	movq	%rdi, %r8
	testq	%rax, %rax
	je	.LBB10_143
# BB#1:
	movq	%r8, %rbx
.LBB10_2:                               # %if.end.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB10_3 Depth 2
	movq	%r12, 64(%rsp)          # 8-byte Spill
.LBB10_3:                               # %if.end
                                        #   Parent Loop BB10_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rbx), %esi
	movq	%rsi, %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	leaq	-3(%rdi), %rcx
	movq	%rdi, %r13
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$128, %rcx
	ja	.LBB10_121
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB10_3 Depth=2
	jmpq	*.LJTI10_0(,%rcx,8)
.LBB10_90:                              # %sw.bb458
                                        #   in Loop: Header=BB10_3 Depth=2
	movq	%rdx, %r13
	movq	%r12, %r15
	movq	8(%r8), %rax
	movq	%r8, %r14
	testq	%rax, %rax
	je	.LBB10_91
# BB#92:                                # %if.then463
                                        #   in Loop: Header=BB10_3 Depth=2
	movq	%rax, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	eliminate_regs
	movq	8(%r14), %rcx
	cmpq	%rcx, %rax
	je	.LBB10_91
# BB#93:                                # %if.then473
                                        #   in Loop: Header=BB10_3 Depth=2
	movzbl	2(%rbx), %ecx
	movq	16(%r14), %rbx
	cmpq	$1, %rcx
	jne	.LBB10_95
# BB#94:                                # %if.then479
                                        #   in Loop: Header=BB10_3 Depth=2
	xorq	%r8, %r8
	testq	%rbx, %rbx
	movq	%r15, %r12
	movq	%r13, %rdx
	jne	.LBB10_30
	jmp	.LBB10_143
.LBB10_7:                               # %sw.bb9
                                        #   in Loop: Header=BB10_3 Depth=2
	movq	%rdx, %rdi
	movq	%r12, %rdx
	movslq	8(%r8), %rax
	movq	%r8, %rsi
	cmpq	$52, %rax
	jle	.LBB10_8
# BB#15:                                # %if.else
                                        #   in Loop: Header=BB10_3 Depth=2
	movq	reg_renumber(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB10_16
# BB#17:                                # %land.lhs.true22
                                        #   in Loop: Header=BB10_3 Depth=2
	movswq	(%rcx,%rax,2), %rcx
	testq	%rcx, %rcx
	jns	.LBB10_18
# BB#19:                                # %land.lhs.true22
                                        #   in Loop: Header=BB10_3 Depth=2
	movq	reg_equiv_constant(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB10_20
# BB#21:                                # %land.lhs.true29
                                        #   in Loop: Header=BB10_3 Depth=2
	movq	(%rcx,%rax,8), %rax
	testq	%rax, %rax
	je	.LBB10_22
# BB#23:                                # %land.lhs.true33
                                        #   in Loop: Header=BB10_3 Depth=2
	movzbl	(%rax), %ecx
	cmpq	$133, %rcx
	jg	.LBB10_26
# BB#24:                                # %land.lhs.true33
                                        #   in Loop: Header=BB10_3 Depth=2
	addq	$-54, %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	$14, %rcx
	movq	%rsi, %r8
	movq	%rdx, %r12
	movq	%rdi, %rsi
	ja	.LBB10_29
# BB#25:                                # %land.lhs.true33
                                        #   in Loop: Header=BB10_3 Depth=2
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	je	.LBB10_29
	jmp	.LBB10_143
.LBB10_26:                              # %land.lhs.true33
                                        #   in Loop: Header=BB10_3 Depth=2
	cmpq	$134, %rcx
	je	.LBB10_27
# BB#28:                                # %land.lhs.true33
                                        #   in Loop: Header=BB10_3 Depth=2
	cmpq	$140, %rcx
	movq	%rsi, %r8
	movq	%rdx, %r12
	movq	%rdi, %rsi
	je	.LBB10_143
.LBB10_29:                              # %if.then88
                                        #   in Loop: Header=BB10_3 Depth=2
	movq	%rax, %rdi
	movq	%rsi, %rbx
	callq	copy_rtx
	movq	%rbx, %rdx
	movq	%rax, %rbx
.LBB10_30:                              # %tailrecurse.backedge
                                        #   in Loop: Header=BB10_3 Depth=2
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	movq	%rbx, %r8
	jne	.LBB10_3
	jmp	.LBB10_143
.LBB10_115:                             # %sw.bb641
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	8(%r8), %rdi
	movq	%r8, %rbx
	movzbl	(%rdi), %ecx
	cmpq	$70, %rcx
	jne	.LBB10_117
# BB#116:                               # %if.then649
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	8(%rdi), %rbx
	xorq	%r12, %r12
	testq	%rax, %rax
	movq	%rbx, %r8
	jne	.LBB10_2
	jmp	.LBB10_143
.LBB10_121:                             # %sw.epilog
	movzbl	rtx_length(%r13), %eax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB10_143
# BB#122:                               # %for.body691.lr.ph
	movq	rtx_format(,%r13,8), %rsi
	movq	%r13, 32(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	leaq	8(,%rax,8), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB10_123:                             # %for.body691
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB10_127 Depth 2
	movzbl	(%rsi), %eax
	cmpq	$101, %rax
	jne	.LBB10_124
# BB#129:                               # %if.then695
                                        #   in Loop: Header=BB10_123 Depth=1
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	8(%r8,%r13,8), %rdi
	movq	%r12, %rsi
	movq	%rdx, %r15
	movq	%r8, %rbx
	callq	eliminate_regs
	movq	%rbx, %r8
	movq	8(%r8,%r13,8), %rcx
	movq	%rax, %rbx
	cmpq	%rcx, %rbx
	je	.LBB10_134
# BB#130:                               # %if.then695
                                        #   in Loop: Header=BB10_123 Depth=1
	movq	%r12, 64(%rsp)          # 8-byte Spill
	movq	%r8, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	56(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	jne	.LBB10_131
# BB#132:                               # %if.then709
                                        #   in Loop: Header=BB10_123 Depth=1
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	rtx_alloc
	movq	%rax, %r12
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	memcpy
	movq	$1, 56(%rsp)            # 8-byte Folded Spill
	movq	%r12, %r8
	jmp	.LBB10_133
	.align	16, 0x90
.LBB10_124:                             # %for.body691
                                        #   in Loop: Header=BB10_123 Depth=1
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB10_142
# BB#125:                               # %for.cond725.preheader
                                        #   in Loop: Header=BB10_123 Depth=1
	movq	8(%r8,%r13,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB10_142
# BB#126:                               #   in Loop: Header=BB10_123 Depth=1
	movq	$1, %r15
	leaq	8(%r8,%r13,8), %rbx
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB10_127:                             # %for.body731
                                        #   Parent Loop BB10_123 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r8, %r14
	movq	(%rax,%r15,8), %rdi
	movq	%r12, %rsi
	callq	eliminate_regs
	movq	(%rbx), %rsi
	movq	(%rsi,%r15,8), %rcx
	movq	%rax, %r12
	cmpq	%rcx, %r12
	je	.LBB10_128
# BB#135:                               # %for.body731
                                        #   in Loop: Header=BB10_127 Depth=2
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	72(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	jne	.LBB10_136
# BB#137:                               # %if.then750
                                        #   in Loop: Header=BB10_127 Depth=2
	movl	(%rsi), %edi
	addq	$8, %rsi
	callq	gen_rtvec_v
	movq	56(%rsp), %rcx          # 8-byte Reload
	testq	%rbx, %rcx
	jne	.LBB10_138
# BB#139:                               # %if.then763
                                        #   in Loop: Header=BB10_127 Depth=2
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rax, 72(%rsp)          # 8-byte Spill
	callq	rtx_alloc
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	memcpy
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	$1, %rcx
	movq	%rbx, %r8
	jmp	.LBB10_140
	.align	16, 0x90
.LBB10_128:                             #   in Loop: Header=BB10_127 Depth=2
	movq	%r14, %r8
	jmp	.LBB10_141
	.align	16, 0x90
.LBB10_136:                             #   in Loop: Header=BB10_127 Depth=2
	movq	%r14, %r8
	jmp	.LBB10_141
	.align	16, 0x90
.LBB10_138:                             #   in Loop: Header=BB10_127 Depth=2
	movq	%r14, %r8
.LBB10_140:                             # %if.end771
                                        #   in Loop: Header=BB10_127 Depth=2
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	%rax, 8(%r8,%r13,8)
	movq	$1, 72(%rsp)            # 8-byte Folded Spill
.LBB10_141:                             # %if.end776
                                        #   in Loop: Header=BB10_127 Depth=2
	movq	8(%r8,%r13,8), %rax
	movq	%r12, (%rax,%r15,8)
	movq	8(%r8,%r13,8), %rax
	leaq	8(%r8,%r13,8), %rbx
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r15
	cmpq	%rdx, %rsi
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	jl	.LBB10_127
	jmp	.LBB10_142
.LBB10_131:                             #   in Loop: Header=BB10_123 Depth=1
	movq	%r14, %r8
.LBB10_133:                             # %if.end715
                                        #   in Loop: Header=BB10_123 Depth=1
	movq	64(%rsp), %r12          # 8-byte Reload
.LBB10_134:                             # %if.end715
                                        #   in Loop: Header=BB10_123 Depth=1
	movq	%r15, %rdx
	movq	%rbx, 8(%r8,%r13,8)
.LBB10_142:                             # %for.inc788
                                        #   in Loop: Header=BB10_123 Depth=1
	incq	%r13
	movq	40(%rsp), %rsi          # 8-byte Reload
	incq	%rsi
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jl	.LBB10_123
	jmp	.LBB10_143
.LBB10_82:                              # %sw.bb425.loopexit1026
	movq	8(%r8), %rdi
	movq	%r8, %r15
	jmp	.LBB10_83
.LBB10_102:                             # %sw.bb529
	movq	8(%r8), %rdi
	movq	%r12, %rsi
	movq	%r8, %r14
	callq	eliminate_regs
	movq	%r14, %r8
	movq	8(%r8), %rcx
	cmpq	%rcx, %rax
	je	.LBB10_143
# BB#103:                               # %if.then539
	movzbl	2(%rbx), %esi
	movq	%r13, %rdi
	jmp	.LBB10_104
.LBB10_120:                             # %sw.bb682
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.eliminate_regs, %rcx
	movq	$2609, %rsi             # imm = 0xA31
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB10_118:                             # %sw.bb666
	movq	8(%r8), %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	movq	%r8, %r14
	callq	eliminate_regs
	movq	%r14, %r8
	movq	8(%r8), %rcx
	cmpq	%rcx, %rax
	je	.LBB10_143
# BB#119:                               # %if.then676
	movzbl	2(%rbx), %ecx
	movq	$48, %rdi
	movq	%rcx, %rsi
.LBB10_104:                             # %if.then539
	movq	%rax, %rdx
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_e  # TAILCALL
.LBB10_105:                             # %sw.bb545
	movq	%rdx, %r10
	movq	%r12, %r9
	movq	8(%r8), %rax
	movq	%r8, %r14
	movl	(%rax), %edi
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB10_110
# BB#106:                               # %land.lhs.true553
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movzbl	mode_size(%rsi), %edx
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movzbl	mode_size(%rdi), %ecx
	cmpq	%rcx, %rdx
	ja	.LBB10_110
# BB#107:                               # %land.lhs.true553
	movq	reg_equiv_memory_loc(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB10_110
# BB#108:                               # %land.lhs.true574
	movl	8(%rax), %edx
	cmpq	$0, (%rcx,%rdx,8)
	je	.LBB10_110
# BB#109:
	movq	%r14, %r8
	jmp	.LBB10_143
.LBB10_5:                               # %sw.bb1
	movq	8(%r8), %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	movq	%r8, %rbx
	callq	eliminate_regs
	movq	%rbx, %r8
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB10_143
# BB#6:                                 # %if.then4
	movq	8(%rax), %r8
	jmp	.LBB10_143
.LBB10_31:                              # %sw.bb95
	movq	%rdx, %r10
	movq	%r12, %r9
	movq	8(%r8), %rdx
	movq	%r8, %r15
	movzbl	(%rdx), %eax
	cmpq	$61, %rax
	jne	.LBB10_36
# BB#32:                                # %land.lhs.true103
	movl	8(%rdx), %eax
	cmpq	$52, %rax
	ja	.LBB10_36
# BB#33:                                # %land.lhs.true112
	movq	16(%r15), %rax
	movzbl	(%rax), %edi
	cmpq	$133, %rdi
	jg	.LBB10_46
# BB#34:                                # %land.lhs.true112
	leaq	-54(%rdi), %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	ja	.LBB10_36
# BB#35:                                # %land.lhs.true112
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	jne	.LBB10_48
	jmp	.LBB10_36
.LBB10_69:                              # %sw.bb346
	movq	%rdx, %r9
	movq	%r12, %rsi
	movq	8(%r8), %rdi
	movq	%r8, %r15
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	movq	$78, %r13
	jne	.LBB10_70
# BB#71:                                # %land.lhs.true354
	movl	8(%rdi), %eax
	cmpq	$52, %rax
	ja	.LBB10_70
# BB#72:                                # %land.lhs.true363
	movq	16(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB10_70
# BB#73:                                # %for.body376.lr.ph
	movq	reg_eliminate(%rip), %r14
	leaq	192(%r14), %rcx
.LBB10_74:                              # %for.body376
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r14), %rdx
	cmpq	%rdi, %rdx
	jne	.LBB10_81
# BB#75:                                # %land.lhs.true383
                                        #   in Loop: Header=BB10_74 Depth=1
	movl	12(%r14), %edx
	testq	%rdx, %rdx
	jne	.LBB10_76
.LBB10_81:                              # %for.inc421
                                        #   in Loop: Header=BB10_74 Depth=1
	addq	$48, %r14
	cmpq	%rcx, %r14
	jb	.LBB10_74
.LBB10_70:
	movq	%rsi, %r12
	movq	%r9, %rdx
.LBB10_83:                              # %sw.bb425
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	%r12, %rsi
	callq	eliminate_regs
	movq	16(%r15), %rdi
	movq	%rax, %r14
	xorq	%rax, %rax
	testq	%rdi, %rdi
	movq	%r15, %r8
	je	.LBB10_85
# BB#84:                                # %cond.true
	movq	%r12, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%r8, %r15
	callq	eliminate_regs
	movq	%r15, %r8
.LBB10_85:                              # %cond.end
	movq	8(%r8), %rcx
	cmpq	%rcx, %r14
	jne	.LBB10_87
# BB#86:                                # %lor.lhs.false446
	movq	16(%r8), %rcx
	cmpq	%rcx, %rax
	je	.LBB10_143
.LBB10_87:                              # %if.then452
	movzbl	2(%rbx), %esi
	movq	%r13, %rdi
	jmp	.LBB10_88
.LBB10_91:
	movq	%r14, %r8
	jmp	.LBB10_96
.LBB10_95:                              # %if.end492
	movq	$3, %rdi
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r8
.LBB10_96:                              # %sw.bb502
	movq	%r15, %r12
	movq	%r13, %rdx
.LBB10_97:                              # %sw.bb502
	movq	%r12, %rcx
	movq	16(%r8), %rax
	movq	%r8, %rbx
	testq	%rax, %rax
	je	.LBB10_98
# BB#99:                                # %if.then507
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	eliminate_regs
	movq	16(%rbx), %rcx
	cmpq	%rcx, %rax
	je	.LBB10_100
# BB#101:                               # %if.then517
	movq	%rbx, %rcx
	movl	(%rcx), %edx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	movq	8(%rcx), %rbx
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movq	%rsi, %rdi
	movq	%rdx, %rsi
	movq	%rbx, %rdx
	jmp	.LBB10_89
.LBB10_8:                               # %for.body.lr.ph
	movq	reg_eliminate(%rip), %rax
	leaq	192(%rax), %rcx
.LBB10_9:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax), %rdx
	cmpq	%rsi, %rdx
	jne	.LBB10_13
# BB#10:                                # %land.lhs.true
                                        #   in Loop: Header=BB10_9 Depth=1
	movl	12(%rax), %edx
	testq	%rdx, %rdx
	jne	.LBB10_11
.LBB10_13:                              # %for.inc
                                        #   in Loop: Header=BB10_9 Depth=1
	addq	$48, %rax
	cmpq	%rcx, %rax
	jb	.LBB10_9
# BB#14:
	movq	%rsi, %r8
	jmp	.LBB10_143
.LBB10_16:
	movq	%rsi, %r8
	jmp	.LBB10_143
.LBB10_18:
	movq	%rsi, %r8
	jmp	.LBB10_143
.LBB10_20:
	movq	%rsi, %r8
	jmp	.LBB10_143
.LBB10_22:
	movq	%rsi, %r8
	jmp	.LBB10_143
.LBB10_27:
	movq	%rsi, %r8
	jmp	.LBB10_143
.LBB10_117:                             # %if.end657
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	callq	eliminate_regs
	movq	%rbx, %rdi
	movq	%rax, %rsi
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	replace_equiv_address_nv  # TAILCALL
.LBB10_110:                             # %if.end594
	movq	%rax, %rdi
	movq	%r9, %rsi
	movq	%r10, %rdx
	callq	eliminate_regs
	movq	8(%r14), %rcx
	cmpq	%rcx, %rax
	je	.LBB10_111
# BB#112:                               # %if.then600
	movl	(%rax), %esi
	movzbl	2(%rbx), %edx
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB10_114
# BB#113:                               # %land.lhs.true617
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movzbl	mode_size(%rdx), %ecx
	movzbl	mode_size(%rsi), %esi
	cmpq	%rsi, %rcx
	jbe	.LBB10_144
.LBB10_114:                             # %if.else632
	movl	16(%r14), %ecx
	movq	%rdx, %rdi
	movq	%rax, %rsi
	movq	%rcx, %rdx
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_SUBREG  # TAILCALL
.LBB10_46:                              # %land.lhs.true112
	cmpq	$134, %rdi
	je	.LBB10_48
# BB#47:                                # %land.lhs.true112
	cmpq	$140, %rdi
	jne	.LBB10_36
.LBB10_48:                              # %for.body181.lr.ph
	movq	reg_eliminate(%rip), %rsi
	leaq	192(%rsi), %rcx
.LBB10_49:                              # %for.body181
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rsi), %rbx
	cmpq	%rdx, %rbx
	jne	.LBB10_56
# BB#50:                                # %land.lhs.true188
                                        #   in Loop: Header=BB10_49 Depth=1
	movl	12(%rsi), %ebx
	testq	%rbx, %rbx
	jne	.LBB10_51
.LBB10_56:                              # %for.inc225
                                        #   in Loop: Header=BB10_49 Depth=1
	addq	$48, %rsi
	cmpq	%rcx, %rsi
	jb	.LBB10_49
# BB#57:
	movq	%r15, %r8
	jmp	.LBB10_143
.LBB10_76:                              # %if.then386
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rsi
	jne	.LBB10_80
# BB#77:                                # %land.lhs.true388
	testq	%r9, %r9
	je	.LBB10_79
# BB#78:                                # %land.lhs.true391
	movzbl	(%r9), %ecx
	addq	$-3, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$2, %rcx
	jb	.LBB10_80
.LBB10_79:                              # %if.then401
	movq	$1, %rcx
	movl	%ecx, 28(%r14)
.LBB10_80:                              # %if.end402
	movl	target_flags(%rip), %edx
	movq	$25, %rcx
	movq	40(%r14), %rbx
	shrq	%cl, %rdx
	andq	$1, %rdx
	orq	$4, %rdx
	movq	$78, %rdi
	movq	%rdx, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	16(%r15), %rcx
	movslq	24(%r14), %rdx
	movq	8(%rcx), %rcx
	imulq	%rdx, %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	jmp	.LBB10_12
.LBB10_98:
	movq	%rbx, %r8
	jmp	.LBB10_143
.LBB10_100:
	movq	%rbx, %r8
	jmp	.LBB10_143
.LBB10_11:                              # %if.then18
	movq	40(%rax), %rcx
	movslq	24(%rax), %rax
	movq	%rcx, %rdi
	movq	%rax, %rsi
.LBB10_12:                              # %if.then18
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	plus_constant_wide  # TAILCALL
.LBB10_111:
	movq	%r14, %r8
	jmp	.LBB10_143
.LBB10_144:                             # %if.then623
	movl	16(%r14), %ebx
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%rax, %rdi
	movq	%rdx, %rsi
	movq	%rbx, %rdx
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	adjust_address_1  # TAILCALL
.LBB10_36:                              # %if.end228
	movq	%rdx, %rdi
	movq	%r9, %r12
	movq	%r12, %rsi
	movq	%r10, %r13
	movq	%r13, %rdx
	callq	eliminate_regs
	movq	16(%r15), %rcx
	movq	%rax, %r14
	movq	%rcx, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	eliminate_regs
	movq	reg_renumber(%rip), %rcx
	movq	%rax, %r9
	movq	%r15, %r8
	testq	%rcx, %rcx
	je	.LBB10_143
# BB#37:                                # %land.lhs.true238
	movq	8(%r8), %rdx
	cmpq	%rdx, %r14
	movq	%r12, %rax
	jne	.LBB10_39
# BB#38:                                # %lor.lhs.false244
	movq	16(%r8), %rdx
	cmpq	%rdx, %r9
	je	.LBB10_143
.LBB10_39:                              # %if.then250
	movq	%rax, %r15
	movl	(%r9), %esi
	movzbl	(%r14), %edx
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$75, %rdx
	jne	.LBB10_58
# BB#40:                                # %if.then250
	cmpq	$61, %rsi
	jne	.LBB10_58
# BB#41:                                # %land.lhs.true260
	movl	8(%r9), %edi
	cmpq	$53, %rdi
	jb	.LBB10_58
# BB#42:                                # %land.lhs.true266
	movswq	(%rcx,%rdi,2), %rax
	testq	%rax, %rax
	jns	.LBB10_58
# BB#43:                                # %land.lhs.true266
	movq	reg_equiv_constant(%rip), %r8
	testq	%r8, %r8
	je	.LBB10_58
# BB#44:                                # %land.lhs.true278
	movq	(%r8,%rdi,8), %rdi
	testq	%rdi, %rdi
	je	.LBB10_58
# BB#45:
	movq	%rdi, %r9
	jmp	.LBB10_64
.LBB10_51:                              # %if.then191
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r9
	je	.LBB10_55
# BB#52:                                # %if.then191
	cmpq	$54, %rdi
	jne	.LBB10_55
# BB#53:                                # %land.lhs.true202
	movl	24(%rsi), %edx
	movq	8(%rax), %rdi
	negq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rdi
	jne	.LBB10_55
# BB#54:                                # %if.then212
	movq	40(%rsi), %r8
	jmp	.LBB10_143
.LBB10_58:                              # %if.else292
	cmpq	$75, %rsi
	jne	.LBB10_64
# BB#59:                                # %if.else292
	cmpq	$61, %rdx
	jne	.LBB10_64
# BB#60:                                # %land.lhs.true302
	movl	8(%r14), %edx
	cmpq	$53, %rdx
	jb	.LBB10_64
# BB#61:                                # %land.lhs.true308
	movswq	(%rcx,%rdx,2), %rax
	testq	%rax, %rax
	jns	.LBB10_64
# BB#62:                                # %land.lhs.true317
	movq	reg_equiv_constant(%rip), %rax
	movq	(%rax,%rdx,8), %rcx
	testq	%rcx, %rcx
	je	.LBB10_64
# BB#63:                                # %land.lhs.true317
	movq	%rcx, %r14
.LBB10_64:                              # %if.end332
	movq	%r14, %rdi
	movq	%r9, %rsi
	callq	form_sum
	movq	%rax, %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	je	.LBB10_66
# BB#65:
	movq	%rdx, %r8
	jmp	.LBB10_143
.LBB10_55:                              # %if.else214
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	movq	40(%rsi), %r14
	movslq	24(%rsi), %rdx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	plus_constant_wide
	movq	$75, %rdi
	movq	%rbx, %rsi
.LBB10_88:                              # %if.then452
	movq	%r14, %rdx
	jmp	.LBB10_89
.LBB10_66:                              # %land.lhs.true335
	movzbl	(%rdx), %eax
	cmpq	$75, %rax
	jne	.LBB10_68
# BB#67:
	movq	%rdx, %r8
.LBB10_143:                             # %return
	movq	%r8, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB10_68:                              # %if.then340
	movq	const_int_rtx+512(%rip), %rax
	movzbl	2(%rbx), %ecx
	movq	$75, %rdi
	movq	%rcx, %rsi
.LBB10_89:                              # %if.then452
	movq	%rax, %rcx
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp147:
	.size	eliminate_regs, .Ltmp147-eliminate_regs
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI10_0:
	.quad	.LBB10_90
	.quad	.LBB10_97
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_143
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_143
	.quad	.LBB10_120
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_143
	.quad	.LBB10_143
	.quad	.LBB10_121
	.quad	.LBB10_120
	.quad	.LBB10_118
	.quad	.LBB10_120
	.quad	.LBB10_82
	.quad	.LBB10_143
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_143
	.quad	.LBB10_143
	.quad	.LBB10_143
	.quad	.LBB10_121
	.quad	.LBB10_143
	.quad	.LBB10_143
	.quad	.LBB10_121
	.quad	.LBB10_7
	.quad	.LBB10_121
	.quad	.LBB10_105
	.quad	.LBB10_102
	.quad	.LBB10_121
	.quad	.LBB10_115
	.quad	.LBB10_121
	.quad	.LBB10_143
	.quad	.LBB10_143
	.quad	.LBB10_5
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_82
	.quad	.LBB10_31
	.quad	.LBB10_82
	.quad	.LBB10_102
	.quad	.LBB10_69
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_102
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_102
	.quad	.LBB10_102
	.quad	.LBB10_102
	.quad	.LBB10_102
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_82
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_121
	.quad	.LBB10_102
	.quad	.LBB10_102
	.quad	.LBB10_102
	.quad	.LBB10_102
	.quad	.LBB10_102
	.quad	.LBB10_102
	.quad	.LBB10_102
	.quad	.LBB10_102
	.quad	.LBB10_102
	.quad	.LBB10_102
	.quad	.LBB10_102
	.quad	.LBB10_102

	.text
	.align	16, 0x90
	.type	delete_dead_insn,@function
delete_dead_insn:                       # @delete_dead_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp153:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp154:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp155:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp156:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp157:
	.cfi_def_cfa_offset 48
.Ltmp158:
	.cfi_offset %rbx, -40
.Ltmp159:
	.cfi_offset %r12, -32
.Ltmp160:
	.cfi_offset %r14, -24
.Ltmp161:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	prev_real_insn
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB11_7
# BB#1:                                 # %land.lhs.true
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB11_7
# BB#2:                                 # %land.lhs.true1
	movq	8(%rax), %r15
	movzbl	(%r15), %eax
	cmpq	$61, %rax
	jne	.LBB11_7
# BB#3:                                 # %land.lhs.true11
	movq	32(%rbx), %rsi
	movq	%r15, %rdi
	callq	reg_mentioned_p
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB11_7
# BB#4:                                 # %land.lhs.true17
	movl	8(%r15), %edx
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	find_regno_note
	testq	%rax, %rax
	je	.LBB11_7
# BB#5:                                 # %land.lhs.true22
	movq	32(%r14), %rax
	movq	16(%rax), %rdi
	callq	side_effects_p
	testq	%r12, %rax
	jne	.LBB11_7
# BB#6:                                 # %if.then
	movq	%r14, %rdi
	callq	delete_dead_insn
.LBB11_7:                               # %if.end
	movq	$37, %rax
	movw	%ax, (%rbx)
	movabsq	$4294967197, %rax       # imm = 0xFFFFFF9D
	movl	%eax, 40(%rbx)
	movq	$0, 32(%rbx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp162:
	.size	delete_dead_insn, .Ltmp162-delete_dead_insn
	.cfi_endproc

	.align	16, 0x90
	.type	reload_as_needed,@function
reload_as_needed:                       # @reload_as_needed
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp169:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp170:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp171:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp172:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp173:
	.cfi_def_cfa_offset 48
	subq	$1696, %rsp             # imm = 0x6A0
.Ltmp174:
	.cfi_def_cfa_offset 1744
.Ltmp175:
	.cfi_offset %rbx, -48
.Ltmp176:
	.cfi_offset %r12, -40
.Ltmp177:
	.cfi_offset %r13, -32
.Ltmp178:
	.cfi_offset %r14, -24
.Ltmp179:
	.cfi_offset %r15, -16
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	movabsq	$spill_reg_rtx, %rdi
	xorq	%rsi, %rsi
	movq	$424, %rdx              # imm = 0x1A8
	callq	memset
	movabsq	$spill_reg_store, %rdi
	xorq	%rsi, %rsi
	movq	$424, %rdx              # imm = 0x1A8
	callq	memset
	movslq	max_regno(%rip), %rdi
	movq	$8, %rsi
	callq	xcalloc
	movslq	max_regno(%rip), %rdi
	movq	%rax, reg_last_reload_reg(%rip)
	callq	xmalloc
	movq	reg_eliminate(%rip), %rbx
	movq	%rax, reg_has_output_reload(%rip)
	movq	$0, reg_reloaded_valid(%rip)
	.align	16, 0x90
.LBB12_1:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movl	4(%rbx), %esi
	movl	(%rbx), %edi
	callq	ix86_initial_elimination_offset
	movl	%eax, 8(%rbx)
	movl	%eax, 20(%rbx)
	movl	%eax, 24(%rbx)
	movq	reg_eliminate(%rip), %rax
	addq	$192, %rax
	addq	$48, %rbx
	cmpq	%rax, %rbx
	jb	.LBB12_1
# BB#2:                                 # %set_initial_elim_offsets.exit
	movq	reload_insn_chain(%rip), %r14
	xorq	%rax, %rax
	movl	%eax, num_not_at_initial_offset(%rip)
	testq	%r14, %r14
	je	.LBB12_988
# BB#3:                                 # %for.body.lr.ph
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB12_4:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB12_17 Depth 2
                                        #     Child Loop BB12_41 Depth 2
                                        #     Child Loop BB12_52 Depth 2
                                        #       Child Loop BB12_53 Depth 3
                                        #       Child Loop BB12_60 Depth 3
                                        #       Child Loop BB12_66 Depth 3
                                        #         Child Loop BB12_113 Depth 4
                                        #         Child Loop BB12_121 Depth 4
                                        #         Child Loop BB12_126 Depth 4
                                        #         Child Loop BB12_145 Depth 4
                                        #         Child Loop BB12_190 Depth 4
                                        #         Child Loop BB12_216 Depth 4
                                        #       Child Loop BB12_221 Depth 3
                                        #         Child Loop BB12_232 Depth 4
                                        #           Child Loop BB12_233 Depth 5
                                        #             Child Loop BB12_266 Depth 6
                                        #     Child Loop BB12_283 Depth 2
                                        #       Child Loop BB12_288 Depth 3
                                        #     Child Loop BB12_299 Depth 2
                                        #       Child Loop BB12_300 Depth 3
                                        #     Child Loop BB12_323 Depth 2
                                        #     Child Loop BB12_327 Depth 2
                                        #     Child Loop BB12_338 Depth 2
                                        #       Child Loop BB12_356 Depth 3
                                        #       Child Loop BB12_372 Depth 3
                                        #     Child Loop BB12_381 Depth 2
                                        #       Child Loop BB12_390 Depth 3
                                        #       Child Loop BB12_417 Depth 3
                                        #       Child Loop BB12_427 Depth 3
                                        #     Child Loop BB12_445 Depth 2
                                        #       Child Loop BB12_494 Depth 3
                                        #       Child Loop BB12_558 Depth 3
                                        #       Child Loop BB12_563 Depth 3
                                        #       Child Loop BB12_588 Depth 3
                                        #       Child Loop BB12_649 Depth 3
                                        #       Child Loop BB12_744 Depth 3
                                        #       Child Loop BB12_780 Depth 3
                                        #     Child Loop BB12_787 Depth 2
                                        #     Child Loop BB12_992 Depth 2
                                        #     Child Loop BB12_791 Depth 2
                                        #       Child Loop BB12_818 Depth 3
                                        #       Child Loop BB12_825 Depth 3
                                        #       Child Loop BB12_866 Depth 3
                                        #       Child Loop BB12_874 Depth 3
                                        #       Child Loop BB12_890 Depth 3
                                        #       Child Loop BB12_910 Depth 3
                                        #       Child Loop BB12_923 Depth 3
                                        #       Child Loop BB12_925 Depth 3
                                        #       Child Loop BB12_927 Depth 3
                                        #       Child Loop BB12_974 Depth 3
                                        #       Child Loop BB12_932 Depth 3
                                        #       Child Loop BB12_955 Depth 3
                                        #         Child Loop BB12_956 Depth 4
                                        #     Child Loop BB12_978 Depth 2
                                        #     Child Loop BB12_26 Depth 2
                                        #     Child Loop BB12_31 Depth 2
	movq	32(%r14), %r12
	movzbl	(%r12), %eax
	cmpq	$36, %rax
	jne	.LBB12_9
# BB#5:                                 # %if.then
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	%r12, %rdi
	movq	%r8, %rbx
	callq	set_offsets_for_label
	movq	%rbx, %r8
	jmp	.LBB12_6
	.align	16, 0x90
.LBB12_9:                               # %if.else
                                        #   in Loop: Header=BB12_4 Depth=1
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB12_6
# BB#10:                                # %if.then11
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	32(%r12), %r15
	movl	(%r15), %eax
	movq	24(%r12), %r13
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$48, %rax
	jne	.LBB12_13
# BB#11:                                # %land.lhs.true
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	8(%r15), %rax
	movl	(%rax), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB12_13
# BB#12:                                # %if.then39
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	8(%rax), %rdi
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	xorq	%rdx, %rdx
	movq	%r8, %rbx
	callq	eliminate_regs
	movq	%rbx, %r8
	movq	32(%r12), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, 8(%rcx)
.LBB12_13:                              # %if.end
                                        #   in Loop: Header=BB12_4 Depth=1
	movl	num_eliminable(%rip), %eax
	movl	num_eliminable_invariants(%rip), %ecx
	orq	%rax, %rcx
	testq	%r8, %rcx
	je	.LBB12_21
# BB#14:                                # %land.lhs.true70
                                        #   in Loop: Header=BB12_4 Depth=1
	movzbl	262(%r14), %eax
	testq	$4, %rax
	je	.LBB12_21
# BB#15:                                # %if.then75
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	$1, %rsi
	movq	%r12, %rdi
	movq	%r8, %rbx
	callq	eliminate_regs_in_insn
	movq	%rbx, %r8
	movzbl	(%r12), %eax
	cmpq	$37, %rax
	jne	.LBB12_21
# BB#16:                                # %if.then81
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	reg_eliminate(%rip), %rax
	xorq	%rcx, %rcx
	movl	%ecx, num_not_at_initial_offset(%rip)
	leaq	192(%rax), %rcx
	.align	16, 0x90
.LBB12_17:                              # %for.body.i268
                                        #   Parent Loop BB12_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	20(%rax), %edx
	movl	%edx, 24(%rax)
	movl	12(%rax), %esi
	testq	%rsi, %rsi
	je	.LBB12_20
# BB#18:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB12_17 Depth=2
	movl	8(%rax), %esi
	andq	%r8, %rdx
	cmpq	%rsi, %rdx
	je	.LBB12_20
# BB#19:                                # %if.then.i
                                        #   in Loop: Header=BB12_17 Depth=2
	movl	num_not_at_initial_offset(%rip), %edx
	incq	%rdx
	movl	%edx, num_not_at_initial_offset(%rip)
.LBB12_20:                              # %for.inc.i
                                        #   in Loop: Header=BB12_17 Depth=2
	addq	$48, %rax
	cmpq	%rcx, %rax
	jb	.LBB12_17
	jmp	.LBB12_987
	.align	16, 0x90
.LBB12_21:                              # %if.end83
                                        #   in Loop: Header=BB12_4 Depth=1
	movzbl	262(%r14), %eax
	testq	$7, %rax
	jne	.LBB12_36
# BB#22:                                # %if.end106.thread
                                        #   in Loop: Header=BB12_4 Depth=1
	xorq	%rax, %rax
	movl	%eax, n_reloads(%rip)
	jmp	.LBB12_23
.LBB12_36:                              # %if.end106
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	reg_has_output_reload(%rip), %rdi
	movslq	max_regno(%rip), %rdx
	xorq	%rsi, %rsi
	movq	%r8, %rbx
	callq	memset
	movsbq	spill_indirect_levels(%rip), %rdx
	movq	$0, reg_is_output_reload(%rip)
	movq	$1, %rsi
	movq	%r12, %rdi
	movq	56(%rsp), %rcx          # 8-byte Reload
	movabsq	$spill_reg_order, %r8
	callq	find_reloads
	movq	%rbx, %r8
	movslq	n_reloads(%rip), %rbx
	testq	%rbx, %rbx
	jle	.LBB12_23
# BB#37:                                # %for.body.lr.ph.i294
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movq	16(%r12), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	24(%r12), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	32(%r14), %rax
	cmpq	$1, %rbx
	jg	.LBB12_38
# BB#39:                                # %for.body.lr.ph.i294
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%r8, %r15
	movq	$4, %rdx
	jmp	.LBB12_40
.LBB12_38:                              #   in Loop: Header=BB12_4 Depth=1
	movq	%rax, 120(%rsp)         # 8-byte Spill
	leaq	-1(%rbx), %rax
	andq	%r8, %rax
	movq	%r8, %r15
	leaq	4(,%rax,4), %rdx
.LBB12_40:                              # %for.body.lr.ph.i294
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	$255, %rsi
	movabsq	$reload_spill_index, %rdi
	callq	memset
	xorq	%rax, %rax
	movq	$1, %rdi
	movabsq	$rld+64, %rdx
	xorq	%r8, %r8
	movq	%r15, %r9
	.align	16, 0x90
.LBB12_41:                              # %for.body.i297
                                        #   Parent Loop BB12_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	-32(%rdx), %esi
	movw	%ax, reload_order(%rax,%rax)
	cmpq	$2, %rsi
	jb	.LBB12_45
# BB#42:                                # %if.then.i298
                                        #   in Loop: Header=BB12_41 Depth=2
	movq	%rdi, %rcx
	andq	%r9, %rcx
	cmpq	%rcx, %rsi
	ja	.LBB12_44
# BB#43:                                # %if.then.i298
                                        #   in Loop: Header=BB12_41 Depth=2
	movq	%rdi, %rsi
.LBB12_44:                              # %if.then.i298
                                        #   in Loop: Header=BB12_41 Depth=2
	movq	$32, %rcx
	movslq	-48(%rdx), %rdi
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	imulq	$100, %rdi, %rcx
	movl	reg_class_superunion(%rcx,%r8,4), %r8d
	movq	%rsi, %rdi
.LBB12_45:                              # %if.end.i
                                        #   in Loop: Header=BB12_41 Depth=2
	movq	(%rdx), %rcx
	movq	%rcx, 240(%rsp,%rax,8)
	incq	%rax
	movq	%rax, %rcx
	andq	%r9, %rcx
	movq	%rbx, %rsi
	andq	%r9, %rsi
	addq	$104, %rdx
	cmpq	%rsi, %rcx
	jne	.LBB12_41
# BB#46:                                # %for.end.i303
                                        #   in Loop: Header=BB12_4 Depth=1
	cmpq	$2, %rbx
	jl	.LBB12_47
# BB#48:                                # %if.then28.i
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	%r13, 72(%rsp)          # 8-byte Spill
	movq	%r12, 80(%rsp)          # 8-byte Spill
	movq	$2, %rdx
	movabsq	$reload_order, %rdi
	movq	%rbx, %rsi
	movabsq	$reload_reg_class_lower, %rcx
	movq	%r9, %r15
	movq	%r8, %rbx
	callq	specqsort
	movq	%rbx, %r8
	movq	%r15, %r9
	movl	n_reloads(%rip), %ebx
	jmp	.LBB12_49
.LBB12_47:                              #   in Loop: Header=BB12_4 Depth=1
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	%r13, 72(%rsp)          # 8-byte Spill
	movq	%r12, 80(%rsp)          # 8-byte Spill
.LBB12_49:                              # %if.end29.i
                                        #   in Loop: Header=BB12_4 Depth=1
	movslq	optimize(%rip), %rax
	testq	%rax, %rax
	movabsq	$0, %rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
	jle	.LBB12_51
# BB#50:                                # %if.end29.i
                                        #   in Loop: Header=BB12_4 Depth=1
	movabsq	$1, %rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
.LBB12_51:                              # %if.end29.i
                                        #   in Loop: Header=BB12_4 Depth=1
	leaq	40(%r14), %rax
	movq	%rax, 96(%rsp)          # 8-byte Spill
	leaq	64(%r14), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	%r14, 136(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB12_52:                              # %for.cond32.i
                                        #   Parent Loop BB12_4 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB12_53 Depth 3
                                        #       Child Loop BB12_60 Depth 3
                                        #       Child Loop BB12_66 Depth 3
                                        #         Child Loop BB12_113 Depth 4
                                        #         Child Loop BB12_121 Depth 4
                                        #         Child Loop BB12_126 Depth 4
                                        #         Child Loop BB12_145 Depth 4
                                        #         Child Loop BB12_190 Depth 4
                                        #         Child Loop BB12_216 Depth 4
                                        #       Child Loop BB12_221 Depth 3
                                        #         Child Loop BB12_232 Depth 4
                                        #           Child Loop BB12_233 Depth 5
                                        #             Child Loop BB12_266 Depth 6
	movq	%r8, 104(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rax
	testq	%rdx, %rdx
	movabsq	$rld+64, %rcx
	jle	.LBB12_54
	.align	16, 0x90
.LBB12_53:                              # %for.body.i.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_52 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	232(%rsp,%rax,8), %rdx
	movq	%rdx, (%rcx)
	movq	%rax, %rdx
	andq	%r9, %rdx
	movq	%rbx, %rsi
	andq	%r9, %rsi
	incq	%rax
	addq	$104, %rcx
	cmpq	%rsi, %rdx
	jne	.LBB12_53
.LBB12_54:                              # %for.end.i.i
                                        #   in Loop: Header=BB12_52 Depth=2
	movq	%r9, %r12
	xorq	%rsi, %rsi
	movq	$180, %rdx
	movabsq	$reload_inherited, %rdi
	callq	memset
	xorq	%rsi, %rsi
	movq	$1440, %rdx             # imm = 0x5A0
	movabsq	$reload_inheritance_insn, %rdi
	callq	memset
	xorq	%rsi, %rsi
	movq	$1440, %rdx             # imm = 0x5A0
	movabsq	$reload_override_in, %rdi
	callq	memset
	movq	$0, reload_reg_used(%rip)
	movq	$0, reload_reg_used_at_all(%rip)
	movq	$0, reload_reg_used_in_op_addr(%rip)
	movq	$0, reload_reg_used_in_op_addr_reload(%rip)
	movq	$0, reload_reg_used_in_insn(%rip)
	movq	$0, reload_reg_used_in_other_addr(%rip)
	movq	$0, reg_used_in_insn(%rip)
	movq	$0, 1688(%rsp)
	leaq	1688(%rsp), %rbx
	movq	%rbx, %rdi
	movq	96(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	callq	reg_set_to_hard_reg_set
	movq	1688(%rsp), %rax
	orq	%rax, reg_used_in_insn(%rip)
	movq	$0, 1688(%rsp)
	movq	%rbx, %rdi
	movq	88(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	callq	reg_set_to_hard_reg_set
	movq	1688(%rsp), %rax
	orq	%rax, reg_used_in_insn(%rip)
	movabsq	$reg_used_in_insn, %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	compute_use_by_pseudos
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	compute_use_by_pseudos
	movslq	reload_n_operands(%rip), %rax
	testq	%rax, %rax
	movq	136(%rsp), %r14         # 8-byte Reload
	jle	.LBB12_59
# BB#55:                                # %for.body10.lr.ph.i.i
                                        #   in Loop: Header=BB12_52 Depth=2
	cmpq	$1, %rax
	jg	.LBB12_56
# BB#57:                                # %for.body10.lr.ph.i.i
                                        #   in Loop: Header=BB12_52 Depth=2
	movq	$8, %rbx
	jmp	.LBB12_58
	.align	16, 0x90
.LBB12_56:                              #   in Loop: Header=BB12_52 Depth=2
	decq	%rax
	andq	%r12, %rax
	leaq	8(,%rax,8), %rbx
.LBB12_58:                              # %for.body10.lr.ph.i.i
                                        #   in Loop: Header=BB12_52 Depth=2
	xorq	%rsi, %rsi
	movabsq	$reload_reg_used_in_output, %rdi
	movq	%rbx, %rdx
	callq	memset
	xorq	%rsi, %rsi
	movabsq	$reload_reg_used_in_input, %rdi
	movq	%rbx, %rdx
	callq	memset
	xorq	%rsi, %rsi
	movabsq	$reload_reg_used_in_input_addr, %rdi
	movq	%rbx, %rdx
	callq	memset
	xorq	%rsi, %rsi
	movabsq	$reload_reg_used_in_inpaddr_addr, %rdi
	movq	%rbx, %rdx
	callq	memset
	xorq	%rsi, %rsi
	movabsq	$reload_reg_used_in_output_addr, %rdi
	movq	%rbx, %rdx
	callq	memset
	xorq	%rsi, %rsi
	movabsq	$reload_reg_used_in_outaddr_addr, %rdi
	movq	%rbx, %rdx
	callq	memset
.LBB12_59:                              # %for.end25.i.i
                                        #   in Loop: Header=BB12_52 Depth=2
	movq	104(%r14), %rcx
	notq	%rcx
	movslq	n_reloads(%rip), %rax
	movq	%rcx, reload_reg_unavailable(%rip)
	movq	$0, reload_reg_used_for_inherit(%rip)
	testq	%rax, %rax
	movq	$1, %r14
	movabsq	$rld+92, %rbx
	jle	.LBB12_63
	.align	16, 0x90
.LBB12_60:                              # %for.body28.i.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_52 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	-28(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB12_62
# BB#61:                                # %if.then.i.i310
                                        #   in Loop: Header=BB12_60 Depth=3
	movl	-64(%rbx), %ecx
	movl	(%rbx), %edx
	movl	-20(%rbx), %esi
	movl	8(%rdi), %edi
	callq	mark_reload_reg_in_use
	movl	n_reloads(%rip), %eax
.LBB12_62:                              # %for.inc42.i.i
                                        #   in Loop: Header=BB12_60 Depth=3
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r14
	addq	$104, %rbx
	cmpq	%rsi, %rdx
	jl	.LBB12_60
.LBB12_63:                              # %choose_reload_regs_init.exit.i
                                        #   in Loop: Header=BB12_52 Depth=2
	movq	$32, %rcx
	movq	168(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB12_278
# BB#64:                                # %for.cond36.preheader.i
                                        #   in Loop: Header=BB12_52 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	testq	%rax, %rax
	jle	.LBB12_277
# BB#65:                                # %for.body39.lr.ph.i
                                        #   in Loop: Header=BB12_52 Depth=2
	xorq	%r15, %r15
	movq	%r12, %r9
	.align	16, 0x90
.LBB12_66:                              # %for.body39.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_52 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB12_113 Depth 4
                                        #         Child Loop BB12_121 Depth 4
                                        #         Child Loop BB12_126 Depth 4
                                        #         Child Loop BB12_145 Depth 4
                                        #         Child Loop BB12_190 Depth 4
                                        #         Child Loop BB12_216 Depth 4
	movswq	reload_order(%r15,%r15), %rax
	movq	%rax, 224(%rsp)         # 8-byte Spill
	imulq	$104, %rax, %r13
	cmpq	$0, rld+8(%r13)
	movq	rld(%r13), %rdi
	leaq	rld+8(%r13), %rax
	movq	%rax, 232(%rsp)         # 8-byte Spill
	leaq	rld(%r13), %r8
	jne	.LBB12_69
# BB#67:                                # %land.lhs.true.i315
                                        #   in Loop: Header=BB12_66 Depth=3
	testq	%rdi, %rdi
	jne	.LBB12_70
# BB#68:                                # %land.lhs.true51.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movzbl	rld+96(%r13), %eax
	testq	$4, %rax
	jne	.LBB12_75
	jmp	.LBB12_219
	.align	16, 0x90
.LBB12_69:                              # %if.end55.i
                                        #   in Loop: Header=BB12_66 Depth=3
	testq	%rdi, %rdi
	je	.LBB12_75
.LBB12_70:                              # %land.lhs.true61.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	rld+64(%r13), %rsi
	testq	%rsi, %rsi
	je	.LBB12_75
# BB#71:                                # %land.lhs.true67.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%r9, %r14
	movq	%r8, %rbx
	callq	rtx_equal_p
	movq	%rbx, %rdx
	movq	%r14, %r9
	testq	%r9, %rax
	jne	.LBB12_219
# BB#72:                                # %lor.lhs.false.i320
                                        #   in Loop: Header=BB12_66 Depth=3
	leaq	rld+64(%r13), %rax
	movq	232(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rdi
	movq	(%rax), %rsi
	movq	%r9, %r14
	movq	%rdx, %rbx
	callq	rtx_equal_p
	movq	%rbx, %r8
	movq	%r14, %r9
	testq	%r9, %rax
	je	.LBB12_75
# BB#73:                                # %land.lhs.true83.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	(%r8), %rdi
	movzbl	(%rdi), %eax
	cmpq	$66, %rax
	je	.LBB12_75
# BB#74:                                # %land.lhs.true91.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%r9, %r14
	movq	%r8, %rbx
	callq	true_regnum
	movq	%rbx, %r8
	movq	%r14, %r9
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jl	.LBB12_219
	.align	16, 0x90
.LBB12_75:                              # %if.end99.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	168(%rsp), %rax         # 8-byte Reload
	testq	%r9, %rax
	je	.LBB12_219
# BB#76:                                # %if.then101.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	(%r8), %rax
	testq	%rax, %rax
	je	.LBB12_160
# BB#77:                                # %if.else.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movl	(%rax), %ebx
	movq	%rbx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB12_79
# BB#78:                                # %if.then115.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movl	8(%rax), %eax
	xorq	%rdx, %rdx
	jmp	.LBB12_85
.LBB12_79:                              # %if.else126.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	rld+40(%r13), %rdi
	movl	(%rdi), %ebx
	movq	%rbx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB12_80
# BB#82:                                # %land.lhs.true154.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	8(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB12_149
# BB#83:                                # %if.then164.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movslq	8(%rax), %rsi
	movl	16(%rdi), %edx
	cmpq	$52, %rsi
	jg	.LBB12_86
# BB#84:                                # %if.then182.i
                                        #   in Loop: Header=BB12_66 Depth=3
	leaq	rld+40(%r13), %rax
	movq	%rax, 208(%rsp)         # 8-byte Spill
	movq	%r9, %rbx
	movq	%r8, %r12
	movq	%rdx, %r14
	callq	subreg_regno
	movq	%r14, %rdx
	movq	%r12, %r8
	movq	%rbx, %r9
	movq	208(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rcx
	movl	(%rcx), %ebx
	jmp	.LBB12_85
.LBB12_80:                              # %if.else126.i
                                        #   in Loop: Header=BB12_66 Depth=3
	cmpq	$61, %rax
	jne	.LBB12_149
# BB#81:                                # %if.then133.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movl	8(%rdi), %eax
	xorq	%rdx, %rdx
.LBB12_85:                              # %if.end197.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	movq	%rax, %rsi
	js	.LBB12_149
.LBB12_86:                              # %land.lhs.true200.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	$16, %rcx
	shrq	%cl, %rbx
	movq	$32, %rcx
	movq	%rsi, %rdi
	movq	reg_last_reload_reg(%rip), %rax
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%rdi, 208(%rsp)         # 8-byte Spill
	movq	(%rax,%rdi,8), %rax
	testq	%rax, %rax
	je	.LBB12_149
# BB#87:                                # %if.then205.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%rsi, 192(%rsp)         # 8-byte Spill
	movq	%r8, 216(%rsp)          # 8-byte Spill
	movslq	rld+16(%r13), %rcx
	movq	%rcx, 176(%rsp)         # 8-byte Spill
	movl	8(%rax), %edi
	movq	%rdi, 184(%rsp)         # 8-byte Spill
	movzbl	2(%rax), %esi
	movq	%rax, 200(%rsp)         # 8-byte Spill
	andq	$255, %rbx
	movq	%rbx, %rcx
	movq	%r9, %r12
	movq	%rdx, %r14
	callq	subreg_regno_offset
	movq	%r14, %rdx
	movq	%r12, %r9
	movq	%rax, %r12
	movq	184(%rsp), %rax         # 8-byte Reload
	addq	%rax, %r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 184(%rsp)         # 8-byte Spill
	testq	%r9, %rdx
	movq	%rbx, %rax
	je	.LBB12_89
# BB#88:                                # %if.else224.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movzbl	mode_size(%rbx), %edi
	addq	%rdx, %rdi
	movl	mode_class(,%rbx,4), %esi
	movq	%r9, %r14
	callq	smallest_mode_for_size
	movq	%r14, %r9
.LBB12_89:                              # %if.end232.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	200(%rsp), %r10         # 8-byte Reload
	movzbl	2(%r10), %ecx
	movzbl	mode_size(%rcx), %edx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %eax
	cmpq	%rax, %rdx
	movq	216(%rsp), %r8          # 8-byte Reload
	movq	192(%rsp), %r14         # 8-byte Reload
	jb	.LBB12_149
# BB#90:                                # %land.lhs.true244.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	184(%rsp), %rax         # 8-byte Reload
	movl	reg_reloaded_contents(,%rax,4), %eax
	andq	%r9, %r14
	cmpq	%r14, %rax
	jne	.LBB12_149
# BB#91:                                # %land.lhs.true249.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	reg_reloaded_valid(%rip), %rax
	movq	$1, %rdx
	movq	%r12, %rcx
	shlq	%cl, %rdx
	movq	%rdx, 192(%rsp)         # 8-byte Spill
	testq	%rdx, %rax
	je	.LBB12_149
# BB#92:                                # %land.lhs.true251.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movl	rld+28(%r13), %esi
	movq	%r12, %rdi
	movq	%r12, 152(%rsp)         # 8-byte Spill
	movq	%r8, %r12
	movq	%rbx, 160(%rsp)         # 8-byte Spill
	movq	%r10, %rbx
	callq	ix86_hard_regno_mode_ok
	movq	160(%rsp), %r10         # 8-byte Reload
	movq	%r12, %r8
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	testq	%r9, %rax
	je	.LBB12_149
# BB#93:                                # %land.lhs.true257.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	reg_class_contents(,%rax,8), %rax
	movq	192(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rax
	jne	.LBB12_96
# BB#94:                                # %lor.lhs.false264.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	184(%rsp), %rax         # 8-byte Reload
	movl	regclass_map(,%rax,4), %esi
	movq	%rsi, 144(%rsp)         # 8-byte Spill
	movq	%r10, %rdi
	movq	176(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rdx
	callq	ix86_register_move_cost
	movq	%rax, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$1, %rdx
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	%r12, %rsi
	callq	ix86_memory_move_cost
	movq	160(%rsp), %rdx         # 8-byte Reload
	movq	216(%rsp), %r8          # 8-byte Reload
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rbx
	jge	.LBB12_149
# BB#95:                                # %land.lhs.true269.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	$1, %rcx
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	176(%rsp), %rsi         # 8-byte Reload
	movq	%rdx, %r12
	callq	ix86_secondary_memory_needed
	movq	200(%rsp), %rbx         # 8-byte Reload
	movq	%r12, %r10
	movq	216(%rsp), %r8          # 8-byte Reload
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	testq	%r9, %rax
	jne	.LBB12_149
.LBB12_96:                              # %land.lhs.true272.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movl	rld+32(%r13), %eax
	movq	48(%rsp), %rcx          # 8-byte Reload
	andq	%r9, %rcx
	cmpq	%rcx, %rax
	je	.LBB12_98
# BB#97:                                # %lor.lhs.false278.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	reg_class_contents(,%rax,8), %rax
	movq	192(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rax
	jne	.LBB12_149
.LBB12_98:                              # %land.lhs.true285.i
                                        #   in Loop: Header=BB12_66 Depth=3
	leaq	rld+28(%r13), %rax
	movq	%rax, 176(%rsp)         # 8-byte Spill
	movq	%r8, 216(%rsp)          # 8-byte Spill
	movq	216(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %r8
	movq	const_int_rtx+512(%rip), %r9
	movl	(%rax), %esi
	movl	rld+92(%r13), %ecx
	movl	rld+72(%r13), %edx
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	$1, 8(%rsp)
	movq	152(%rsp), %rdi         # 8-byte Reload
	movq	%r10, %r12
	callq	free_for_value_p
	movq	%rbx, %r10
	movq	%r12, %rdi
	movq	216(%rsp), %r8          # 8-byte Reload
	movq	152(%rsp), %r12         # 8-byte Reload
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	testq	%r9, %rax
	je	.LBB12_149
# BB#99:                                # %if.then298.i
                                        #   in Loop: Header=BB12_66 Depth=3
	leaq	rld+16(%r13), %rsi
	leaq	rld+72(%r13), %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	%r12, %rcx
	movq	176(%rsp), %rax         # 8-byte Reload
	movslq	(%rax), %rax
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	leaq	rld+92(%r13), %rcx
	movq	%rcx, 128(%rsp)         # 8-byte Spill
	je	.LBB12_103
# BB#100:                               # %if.then298.i
                                        #   in Loop: Header=BB12_66 Depth=3
	leaq	-21(%r12), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_103
# BB#101:                               # %if.then298.i
                                        #   in Loop: Header=BB12_66 Depth=3
	leaq	-45(%r12), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_103
# BB#102:                               # %if.then298.i
                                        #   in Loop: Header=BB12_66 Depth=3
	leaq	-29(%r12), %rcx
	andq	%r9, %rcx
	cmpq	$7, %rcx
	jbe	.LBB12_103
# BB#104:                               # %cond.false338.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%rax, %rcx
	andq	%r9, %rcx
	cmpq	$24, %rcx
	jne	.LBB12_105
# BB#107:                               # %cond.true354.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%rsi, 112(%rsp)         # 8-byte Spill
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB12_110
.LBB12_103:                             # %cond.true322.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%rsi, 112(%rsp)         # 8-byte Spill
	movl	mode_class(,%rax,4), %eax
	addq	$-5, %rax
	andq	%r9, %rax
	cmpq	$2, %rax
	movq	$1, %rbx
	movq	$2, %rax
	jb	.LBB12_111
	jmp	.LBB12_117
.LBB12_105:                             # %cond.false338.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%r12, %rbx
	cmpq	$18, %rcx
	jne	.LBB12_108
# BB#106:                               # %cond.true344.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%rsi, 112(%rsp)         # 8-byte Spill
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB12_109
.LBB12_108:                             # %cond.false358.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%rsi, 112(%rsp)         # 8-byte Spill
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB12_109:                             # %cond.end376.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%rbx, %r12
.LBB12_110:                             # %cond.end376.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rsi
	cmpq	$2, %rdx
	jl	.LBB12_116
.LBB12_111:                             # %for.body381.lr.ph.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%r10, 200(%rsp)         # 8-byte Spill
	movq	%r8, %rbx
	movq	%r9, %r11
	movq	reg_reloaded_valid(%rip), %r8
	movq	$32, %rcx
	movq	%r12, %r10
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	leaq	reg_reloaded_contents(,%r10,4), %r9
	movq	$1, %rsi
	.align	16, 0x90
.LBB12_113:                             # %for.body381.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_52 Depth=2
                                        #       Parent Loop BB12_66 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movl	(%r9,%rsi,4), %ecx
	cmpq	%r14, %rcx
	jne	.LBB12_115
# BB#114:                               # %lor.lhs.false387.i
                                        #   in Loop: Header=BB12_113 Depth=4
	leaq	(%r10,%rsi), %rcx
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%r8, %rdx
	je	.LBB12_115
# BB#112:                               # %for.cond378.i
                                        #   in Loop: Header=BB12_113 Depth=4
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rdx
	jl	.LBB12_113
.LBB12_115:                             # %for.body381.for.end397_crit_edge.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%r11, %r9
	movq	%rbx, %r8
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	200(%rsp), %r10         # 8-byte Reload
.LBB12_116:                             # %for.end397.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%rax, %rcx
	andq	%r9, %rcx
	andq	%r9, %rsi
	cmpq	%rcx, %rsi
	movq	%rax, %rbx
	jne	.LBB12_149
.LBB12_117:                             # %if.then400.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%r8, 216(%rsp)          # 8-byte Spill
	movzbl	2(%r10), %eax
	cmpq	%rdi, %rax
	je	.LBB12_119
# BB#118:                               # %cond.false407.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%r12, %rsi
	movq	%rdi, %r14
	callq	gen_rtx_REG
	movq	%r14, %rdi
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	%rax, %r10
.LBB12_119:                             # %cond.end409.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%r10, 200(%rsp)         # 8-byte Spill
	movq	%rdi, 160(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %r14
	jle	.LBB12_124
# BB#120:                               # %for.body414.lr.ph.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	112(%rsp), %rax         # 8-byte Reload
	movslq	(%rax), %rax
	movq	reg_class_contents(,%rax,8), %rax
	xorq	%rsi, %rsi
	movq	$1, %rdx
	.align	16, 0x90
.LBB12_121:                             # %for.body414.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_52 Depth=2
                                        #       Parent Loop BB12_66 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	leaq	-1(%rdx,%r12), %rcx
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%rax, %rdi
	movabsq	$0, %r14
	jne	.LBB12_123
# BB#122:                               # %for.body414.i
                                        #   in Loop: Header=BB12_121 Depth=4
	movabsq	$1, %r14
.LBB12_123:                             # %for.body414.i
                                        #   in Loop: Header=BB12_121 Depth=4
	orq	%rsi, %r14
	movq	%rdx, %rcx
	andq	%r9, %rcx
	movq	%rbx, %rsi
	andq	%r9, %rsi
	incq	%rdx
	cmpq	%rsi, %rcx
	movq	%r14, %rsi
	jne	.LBB12_121
.LBB12_124:                             # %for.cond428.preheader.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movslq	n_earlyclobbers(%rip), %rax
	testq	%rax, %rax
	movabsq	$0, %r12
	jle	.LBB12_128
	.align	16, 0x90
.LBB12_126:                             # %for.body431.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_52 Depth=2
                                        #       Parent Loop BB12_66 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	reg_last_reload_reg(%rip), %rax
	movq	208(%rsp), %rcx         # 8-byte Reload
	movq	(%rax,%rcx,8), %rdi
	movq	reload_earlyclobbers(,%r12,8), %rsi
	callq	reg_overlap_mentioned_for_reload_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB12_127
# BB#125:                               # %for.cond428.i
                                        #   in Loop: Header=BB12_126 Depth=4
	incq	%r12
	movslq	n_earlyclobbers(%rip), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB12_126
	jmp	.LBB12_128
.LBB12_127:                             # %for.body431.for.end442_crit_edge.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movl	n_earlyclobbers(%rip), %eax
.LBB12_128:                             # %for.end442.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	andq	%r9, %rax
	andq	%r9, %r12
	cmpq	%rax, %r12
	movq	152(%rsp), %r12         # 8-byte Reload
	jne	.LBB12_147
# BB#129:                               # %lor.lhs.false445.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	216(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r8
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r9
	movq	128(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %ecx
	movq	144(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %edx
	movq	176(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %esi
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	$1, 8(%rsp)
	movq	%r12, %rdi
	callq	free_for_value_p
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	testq	%r9, %rax
	je	.LBB12_147
# BB#130:                               # %lor.lhs.false463.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	reg_used_in_insn(%rip), %rax
	movq	192(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rax
	je	.LBB12_133
# BB#131:                               # %land.lhs.true468.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	232(%rsp), %rax         # 8-byte Reload
	cmpq	$0, (%rax)
	je	.LBB12_133
# BB#132:                               # %land.lhs.true473.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	reg_reloaded_dead(%rip), %rax
	movq	192(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rax
	je	.LBB12_147
.LBB12_133:                             # %lor.lhs.false478.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%r12, %rax
	andq	%r9, %rax
	cmpq	$6, %rax
	jne	.LBB12_136
# BB#134:                               # %lor.lhs.false478.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movl	frame_pointer_needed(%rip), %eax
	testq	%r9, %rax
	je	.LBB12_136
# BB#135:                               # %land.lhs.true483.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	232(%rsp), %rax         # 8-byte Reload
	cmpq	$0, (%rax)
	jne	.LBB12_147
.LBB12_136:                             # %lor.lhs.false488.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	176(%rsp), %rax         # 8-byte Reload
	movslq	(%rax), %rax
	movzbl	mode_size(%rax), %ecx
	movq	160(%rsp), %rdx         # 8-byte Reload
	movzbl	mode_size(%rdx), %edx
	cmpq	%rdx, %rcx
	ja	.LBB12_147
# BB#137:                               # %lor.lhs.false488.i
                                        #   in Loop: Header=BB12_66 Depth=3
	testq	%r9, %r14
	jne	.LBB12_147
# BB#138:                               # %lor.lhs.false502.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	232(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rdi
	testq	%rdi, %rdi
	leaq	rld+64(%r13), %rcx
	jne	.LBB12_140
# BB#139:                               #   in Loop: Header=BB12_66 Depth=3
	movq	%rcx, 208(%rsp)         # 8-byte Spill
	jmp	.LBB12_143
.LBB12_140:                             # %land.lhs.true507.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	(%rcx), %rsi
	movq	%rcx, 208(%rsp)         # 8-byte Spill
	testq	%rsi, %rsi
	je	.LBB12_143
# BB#141:                               # %land.lhs.true512.i
                                        #   in Loop: Header=BB12_66 Depth=3
	callq	rtx_equal_p
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	testq	%r9, %rax
	jne	.LBB12_147
# BB#142:                               # %land.lhs.true512.if.else536_crit_edge.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	176(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %eax
.LBB12_143:                             # %if.else536.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	$32, %rcx
	movq	%rbx, %r14
	movq	128(%rsp), %rdx         # 8-byte Reload
	movl	(%rdx), %edx
	movq	144(%rsp), %rsi         # 8-byte Reload
	movl	(%rsi), %esi
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	%r12, %rdi
	movq	%rax, %rcx
	callq	mark_reload_reg_in_use
	movq	%rbx, %rdi
	testq	%r14, %r14
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	reg_reloaded_insn(,%rax,8), %rax
	movq	200(%rsp), %rcx         # 8-byte Reload
	movq	208(%rsp), %rdx         # 8-byte Reload
	movq	%rcx, (%rdx)
	movq	$1, %rcx
	movq	224(%rsp), %rdx         # 8-byte Reload
	movb	%cl, reload_inherited(%rdx)
	movq	%rax, reload_inheritance_insn(,%rdx,8)
	movl	%r12d, reload_spill_index(,%rdx,4)
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	216(%rsp), %r8          # 8-byte Reload
	jle	.LBB12_149
# BB#144:                               # %for.body561.lr.ph.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	reload_reg_used_for_inherit(%rip), %rdx
	movq	$1, %rax
.LBB12_145:                             # %for.body561.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_52 Depth=2
                                        #       Parent Loop BB12_66 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	%rdx, %rsi
	leaq	-1(%rax,%r12), %rcx
	movq	$1, %rdx
	shlq	%cl, %rdx
	orq	%rsi, %rdx
	movq	%rax, %rcx
	andq	%r9, %rcx
	movq	%rdi, %rsi
	andq	%r9, %rsi
	incq	%rax
	cmpq	%rsi, %rcx
	jne	.LBB12_145
# BB#146:                               # %for.cond558.if.end573.loopexit_crit_edge.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%rdx, reload_reg_used_for_inherit(%rip)
	jmp	.LBB12_149
.LBB12_147:                             # %if.then521.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movzbl	rld+96(%r13), %eax
	testq	$1, %rax
	movq	216(%rsp), %r8          # 8-byte Reload
	movq	200(%rsp), %r12         # 8-byte Reload
	jne	.LBB12_149
# BB#148:                               # %if.then528.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	reg_reloaded_insn(,%rax,8), %rax
	movq	224(%rsp), %rcx         # 8-byte Reload
	movq	%r12, reload_override_in(,%rcx,8)
	movq	%rax, reload_inheritance_insn(,%rcx,8)
	.align	16, 0x90
.LBB12_149:                             # %land.lhs.true575.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	(%r8), %rdi
	testq	%rdi, %rdi
	je	.LBB12_160
# BB#150:                               # %land.lhs.true581.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	224(%rsp), %rax         # 8-byte Reload
	movzbl	reload_inherited(%rax), %eax
	testq	%rax, %rax
	jne	.LBB12_219
# BB#151:                               # %land.lhs.true585.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	232(%rsp), %rax         # 8-byte Reload
	cmpq	$0, (%rax)
	jne	.LBB12_159
# BB#152:                               # %land.lhs.true591.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movzbl	(%rdi), %ecx
	cmpq	$133, %rcx
	jg	.LBB12_155
# BB#153:                               # %land.lhs.true591.i
                                        #   in Loop: Header=BB12_66 Depth=3
	addq	$-54, %rcx
	andq	%r9, %rcx
	cmpq	$21, %rcx
	ja	.LBB12_159
# BB#154:                               # %land.lhs.true591.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$2125975, %rax          # imm = 0x207097
	jne	.LBB12_157
	jmp	.LBB12_159
.LBB12_155:                             # %land.lhs.true591.i
                                        #   in Loop: Header=BB12_66 Depth=3
	cmpq	$134, %rcx
	je	.LBB12_157
# BB#156:                               # %land.lhs.true591.i
                                        #   in Loop: Header=BB12_66 Depth=3
	cmpq	$140, %rcx
	jne	.LBB12_159
.LBB12_157:                             # %land.lhs.true679.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movl	rld+32(%r13), %eax
	movq	48(%rsp), %rcx          # 8-byte Reload
	andq	%r9, %rcx
	cmpq	%rcx, %rax
	je	.LBB12_171
# BB#158:                               # %lor.lhs.false685.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movl	rld+16(%r13), %edi
	movq	104(%rsp), %rsi         # 8-byte Reload
	movq	%r9, %r14
	movq	%r8, %rbx
	callq	reg_classes_intersect_p
	movq	%rbx, %r8
	movq	%r14, %r9
	movq	(%r8), %rdi
	testq	%r9, %rax
	je	.LBB12_171
.LBB12_159:                             # %land.lhs.true697.i
                                        #   in Loop: Header=BB12_66 Depth=3
	testq	%rdi, %rdi
	jne	.LBB12_219
	.align	16, 0x90
.LBB12_160:                             # %land.lhs.true703.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	232(%rsp), %rax         # 8-byte Reload
	cmpq	$0, (%rax)
	je	.LBB12_219
# BB#161:                               # %if.then709.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	120(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB12_219
# BB#162:                               # %cond.true717.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	32(%rcx), %rbx
	movzbl	(%rbx), %eax
	cmpq	$47, %rax
	je	.LBB12_164
# BB#163:                               # %cond.false729.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	movq	%r9, %r14
	movq	%r8, %rbx
	callq	single_set_2
	movq	%rbx, %r8
	movq	%r14, %r9
	movq	%rax, %rbx
.LBB12_164:                             # %cond.end737.i
                                        #   in Loop: Header=BB12_66 Depth=3
	testq	%rbx, %rbx
	je	.LBB12_219
# BB#165:                               # %land.lhs.true740.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rdi
	movq	8(%rbx), %rsi
	movq	%r9, %r14
	movq	%r8, %r12
	callq	rtx_equal_p
	movq	%r12, %r8
	movq	%r14, %r9
	testq	%r9, %rax
	je	.LBB12_219
# BB#166:                               # %land.lhs.true749.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	16(%rbx), %rdi
	movzbl	(%rdi), %ecx
	cmpq	$133, %rcx
	jg	.LBB12_169
# BB#167:                               # %land.lhs.true749.i
                                        #   in Loop: Header=BB12_66 Depth=3
	addq	$-54, %rcx
	andq	%r9, %rcx
	cmpq	$14, %rcx
	ja	.LBB12_219
# BB#168:                               # %land.lhs.true749.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB12_171
	jmp	.LBB12_219
.LBB12_169:                             # %land.lhs.true749.i
                                        #   in Loop: Header=BB12_66 Depth=3
	cmpq	$134, %rcx
	je	.LBB12_171
# BB#170:                               # %land.lhs.true749.i
                                        #   in Loop: Header=BB12_66 Depth=3
	cmpq	$140, %rcx
	jne	.LBB12_219
.LBB12_171:                             # %if.end819.i
                                        #   in Loop: Header=BB12_66 Depth=3
	testq	%rdi, %rdi
	je	.LBB12_219
# BB#172:                               # %if.then821.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movl	rld+16(%r13), %edx
	movl	rld+28(%r13), %eax
	movq	%r8, %r14
	xorq	%r8, %r8
	movq	%r9, %rbx
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	%rbx, %rcx
	callq	find_equiv_reg
	movq	%r14, %rdx
	movq	%rbx, %r9
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB12_219
# BB#173:                               # %if.then832.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movzbl	(%r12), %eax
	leaq	rld+16(%r13), %rcx
	movq	%rcx, 200(%rsp)         # 8-byte Spill
	leaq	rld+28(%r13), %rcx
	cmpq	$63, %rax
	jne	.LBB12_174
# BB#178:                               # %if.then846.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%r12, %rdi
	movq	%rdx, %r12
	movq	%rcx, %r14
	movq	%r14, 208(%rsp)         # 8-byte Spill
	callq	subreg_regno
	movl	(%r14), %edi
	movq	%rax, %rbx
	movq	%rbx, %rsi
	callq	gen_rtx_REG
	movq	%rbx, %rdi
	movq	%r12, %rdx
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	%rax, %r12
	jmp	.LBB12_179
.LBB12_174:                             # %if.then832.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%rcx, 208(%rsp)         # 8-byte Spill
	cmpq	$61, %rax
	jne	.LBB12_176
# BB#175:                               # %if.then837.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movl	8(%r12), %edi
.LBB12_179:                             # %if.end855.i
                                        #   in Loop: Header=BB12_66 Depth=3
	testq	%r12, %r12
	je	.LBB12_219
# BB#180:                               # %land.lhs.true858.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	reload_reg_used_at_all(%rip), %rax
	movq	$1, %rsi
	movq	%rdi, %rcx
	shlq	%cl, %rsi
	movq	%rsi, 192(%rsp)         # 8-byte Spill
	testq	%rsi, %rax
	je	.LBB12_182
# BB#181:                               # %land.lhs.true863.i340
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	(%rdx), %r8
	movq	%r9, %r14
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r9
	movq	208(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %esi
	movl	rld+92(%r13), %ecx
	movl	rld+72(%r13), %edx
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	$1, 8(%rsp)
	movq	%rdi, %rbx
	callq	free_for_value_p
	movq	%rbx, %rdi
	movq	%r14, %r9
	testq	%r9, %rax
	je	.LBB12_219
.LBB12_182:                             # %lor.lhs.false881.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	200(%rsp), %rax         # 8-byte Reload
	movslq	(%rax), %rax
	movq	reg_class_contents(,%rax,8), %rax
	movq	192(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rax
	je	.LBB12_219
# BB#183:                               # %land.lhs.true895.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	208(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %esi
	movq	%r9, %r14
	movq	%rdi, %rbx
	callq	ix86_hard_regno_mode_ok
	movq	%rbx, %rcx
	movq	%r14, %r9
	testq	%r9, %rax
	je	.LBB12_219
# BB#184:                               # %for.cond906.preheader.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%rcx, 232(%rsp)         # 8-byte Spill
	movq	%r9, %r14
	movslq	n_earlyclobbers(%rip), %rax
	testq	%rax, %rax
	movq	$1, %rbx
	jle	.LBB12_185
	.align	16, 0x90
.LBB12_190:                             # %for.body909.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_52 Depth=2
                                        #       Parent Loop BB12_66 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	reload_earlyclobbers-8(,%rbx,8), %rsi
	movq	%r12, %rdi
	callq	reg_overlap_mentioned_for_reload_p
	movq	%r14, %r9
	testq	%r9, %rax
	jne	.LBB12_191
# BB#189:                               # %for.cond906.i
                                        #   in Loop: Header=BB12_190 Depth=4
	movq	%r9, %r14
	movslq	n_earlyclobbers(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB12_190
.LBB12_185:                             # %if.then933.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	208(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %edx
	xorq	%rcx, %rcx
	movq	232(%rsp), %rdi         # 8-byte Reload
	movq	120(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	regno_clobbered_p
	movq	%r14, %r9
	testq	%r9, %rax
	jne	.LBB12_186
# BB#193:                               # %if.else955.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	208(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %edx
	movq	$1, %rcx
	movq	232(%rsp), %rdi         # 8-byte Reload
	movq	%rdi, %r14
	movq	%rbx, %rsi
	movq	%r9, %rbx
	callq	regno_clobbered_p
	movq	%rbx, %r9
	testq	%r9, %rax
	je	.LBB12_197
# BB#194:                               # %if.then961.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movl	rld+92(%r13), %ecx
	cmpq	$10, %rcx
	ja	.LBB12_219
# BB#195:                               # %if.then961.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$1433, %rax             # imm = 0x599
	jne	.LBB12_197
# BB#196:                               # %if.then961.i
                                        #   in Loop: Header=BB12_66 Depth=3
	cmpq	$9, %rcx
	je	.LBB12_191
	jmp	.LBB12_219
.LBB12_186:                             # %if.then939.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movl	rld+92(%r13), %ecx
	cmpq	$10, %rcx
	movq	232(%rsp), %rax         # 8-byte Reload
	ja	.LBB12_219
# BB#187:                               # %if.then939.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%rax, %r14
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$1304, %rax             # imm = 0x518
	je	.LBB12_188
.LBB12_197:                             # %land.lhs.true985.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%r14, %rdi
	movq	%rdi, %rax
	andq	%r9, %rax
	cmpq	$6, %rax
	jne	.LBB12_199
# BB#198:                               # %land.lhs.true985.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movl	frame_pointer_needed(%rip), %eax
	testq	%r9, %rax
	jne	.LBB12_219
.LBB12_199:                             # %if.then990.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	208(%rsp), %rax         # 8-byte Reload
	movslq	(%rax), %rax
	movq	%rdi, %rcx
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	je	.LBB12_203
# BB#200:                               # %if.then990.i
                                        #   in Loop: Header=BB12_66 Depth=3
	leaq	-21(%rdi), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_203
# BB#201:                               # %if.then990.i
                                        #   in Loop: Header=BB12_66 Depth=3
	leaq	-45(%rdi), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_203
# BB#202:                               # %if.then990.i
                                        #   in Loop: Header=BB12_66 Depth=3
	leaq	-29(%rdi), %rcx
	andq	%r9, %rcx
	cmpq	$7, %rcx
	jbe	.LBB12_203
# BB#207:                               # %cond.false1033.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%rax, %rcx
	andq	%r9, %rcx
	cmpq	$24, %rcx
	jne	.LBB12_208
# BB#210:                               # %cond.true1049.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movzbl	target_flags+3(%rip), %edx
	andq	$2, %rdx
	xorq	$6, %rdx
	movq	$1, %rbx
	jmp	.LBB12_212
.LBB12_203:                             # %cond.true1015.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %rdx
	movq	$1, %rbx
	je	.LBB12_212
# BB#204:                               # %lor.rhs1023.i
                                        #   in Loop: Header=BB12_66 Depth=3
	cmpq	$6, %rax
	movabsq	$0, %rax
	movq	%rax, %rdx
	jne	.LBB12_206
# BB#205:                               # %lor.rhs1023.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movabsq	$1, %rax
	movq	%rax, %rdx
.LBB12_206:                             # %lor.rhs1023.i
                                        #   in Loop: Header=BB12_66 Depth=3
	incq	%rdx
	jmp	.LBB12_212
.LBB12_188:                             # %if.then939.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$641, %rax              # imm = 0x281
	je	.LBB12_219
.LBB12_191:                             # %if.then914.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movzbl	rld+96(%r13), %eax
	testq	$1, %rax
	jne	.LBB12_219
# BB#192:                               # %if.then922.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	%r12, reload_override_in(,%rax,8)
	jmp	.LBB12_219
.LBB12_208:                             # %cond.false1033.i
                                        #   in Loop: Header=BB12_66 Depth=3
	cmpq	$18, %rcx
	movq	$1, %rbx
	jne	.LBB12_211
# BB#209:                               # %cond.true1039.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movl	target_flags(%rip), %edx
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	xorq	$3, %rdx
	jmp	.LBB12_212
.LBB12_211:                             # %cond.false1053.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %rdx
.LBB12_212:                             # %cond.end1073.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	%rdx, 216(%rsp)         # 8-byte Spill
	movq	reg_reloaded_valid(%rip), %rax
	movq	%r12, rld+64(%r13)
	movq	224(%rsp), %rcx         # 8-byte Reload
	movb	%bl, reload_inherited(%rcx)
	movq	192(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rax
	jne	.LBB12_214
# BB#213:                               # %if.then1085.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$0, spill_reg_store(,%rax,8)
.LBB12_214:                             # %for.cond1089.preheader.i
                                        #   in Loop: Header=BB12_66 Depth=3
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB12_219
# BB#215:                               # %for.body1092.lr.ph.i
                                        #   in Loop: Header=BB12_66 Depth=3
	leaq	rld+72(%r13), %rax
	movq	%rax, 224(%rsp)         # 8-byte Spill
	leaq	rld+92(%r13), %rax
	movq	%rax, 200(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 192(%rsp)         # 8-byte Spill
	leaq	spill_reg_order(%rax,%rax), %rsi
	xorq	%rbx, %rbx
.LBB12_216:                             # %for.body1092.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_52 Depth=2
                                        #       Parent Loop BB12_66 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movswq	(%rsi,%rbx,2), %rax
	testq	%rax, %rax
	js	.LBB12_218
# BB#217:                               # %if.then1099.i
                                        #   in Loop: Header=BB12_216 Depth=4
	movq	208(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %ecx
	movq	200(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %edx
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	%rsi, %r12
	movl	(%rax), %esi
	movq	192(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%rbx), %rax
	movq	%rax, 232(%rsp)         # 8-byte Spill
	movq	%r9, %r13
	movq	%rdi, %r14
	callq	mark_reload_reg_in_use
	movq	%r12, %rsi
	movq	216(%rsp), %rdx         # 8-byte Reload
	movq	%r14, %rdi
	movq	%r13, %r9
	movq	$1, %rax
	movq	232(%rsp), %rcx         # 8-byte Reload
	shlq	%cl, %rax
	orq	%rax, reload_reg_used_for_inherit(%rip)
.LBB12_218:                             # %for.inc1114.i
                                        #   in Loop: Header=BB12_216 Depth=4
	incq	%rbx
	movq	%rbx, %rax
	andq	%r9, %rax
	movq	%rdx, %rcx
	andq	%r9, %rcx
	cmpq	%rcx, %rax
	jne	.LBB12_216
	.align	16, 0x90
.LBB12_219:                             # %for.inc1135.i
                                        #   in Loop: Header=BB12_66 Depth=3
	incq	%r15
	movslq	n_reloads(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB12_66
# BB#220:                               # %for.cond1138.preheader.i
                                        #   in Loop: Header=BB12_52 Depth=2
	movq	%r9, %r12
	xorq	%rsi, %rsi
	testq	%rax, %rax
	jle	.LBB12_277
	.align	16, 0x90
.LBB12_221:                             # %for.body1141.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_52 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB12_232 Depth 4
                                        #           Child Loop BB12_233 Depth 5
                                        #             Child Loop BB12_266 Depth 6
	movq	%rsi, 128(%rsp)         # 8-byte Spill
	movswq	reload_order(%rsi,%rsi), %rcx
	movq	%rcx, 160(%rsp)         # 8-byte Spill
	imulq	$104, %rcx, %rcx
	cmpq	$0, rld+8(%rcx)
	jne	.LBB12_224
# BB#222:                               # %land.lhs.true1151.i
                                        #   in Loop: Header=BB12_221 Depth=3
	cmpq	$0, rld(%rcx)
	jne	.LBB12_224
# BB#223:                               # %land.lhs.true1157.i
                                        #   in Loop: Header=BB12_221 Depth=3
	movzbl	rld+96(%rcx), %edx
	testq	$4, %rdx
	je	.LBB12_276
	.align	16, 0x90
.LBB12_224:                             # %if.end1167.i
                                        #   in Loop: Header=BB12_221 Depth=3
	cmpq	$0, rld+64(%rcx)
	jne	.LBB12_276
# BB#225:                               # %lor.lhs.false1173.i
                                        #   in Loop: Header=BB12_221 Depth=3
	movzbl	rld+96(%rcx), %edx
	testq	$1, %rdx
	jne	.LBB12_276
# BB#226:                               # %if.end1182.i
                                        #   in Loop: Header=BB12_221 Depth=3
	movl	rld+32(%rcx), %edx
	decq	%rax
	andq	%r12, %rax
	cmpq	$1, %rdx
	movabsq	$0, %rdx
	movq	%rdx, 152(%rsp)         # 8-byte Spill
	jbe	.LBB12_228
# BB#227:                               # %if.end1182.i
                                        #   in Loop: Header=BB12_221 Depth=3
	movabsq	$1, %rdx
	movq	%rdx, 152(%rsp)         # 8-byte Spill
.LBB12_228:                             # %if.end1182.i
                                        #   in Loop: Header=BB12_221 Depth=3
	movq	%rsi, %rdx
	andq	%r12, %rdx
	cmpq	%rax, %rdx
	movabsq	$0, %rax
	je	.LBB12_230
# BB#229:                               # %if.end1182.i
                                        #   in Loop: Header=BB12_221 Depth=3
	movabsq	$1, %rax
.LBB12_230:                             # %if.end1182.i
                                        #   in Loop: Header=BB12_221 Depth=3
	leaq	rld+8(%rcx), %rdx
	movq	%rdx, 144(%rsp)         # 8-byte Spill
	leaq	rld+32(%rcx), %rdx
	movq	%rdx, 112(%rsp)         # 8-byte Spill
	leaq	rld(%rcx), %rdx
	movq	%rdx, 200(%rsp)         # 8-byte Spill
	andq	%rax, 152(%rsp)         # 8-byte Folded Spill
	leaq	rld+16(%rcx), %rax
	movq	%rax, 208(%rsp)         # 8-byte Spill
	leaq	rld+72(%rcx), %rax
	movq	%rax, 224(%rsp)         # 8-byte Spill
	movl	n_spills(%rip), %eax
	leaq	rld+92(%rcx), %rdx
	movq	%rdx, 216(%rsp)         # 8-byte Spill
	leaq	rld+28(%rcx), %rcx
	movq	%rcx, 192(%rsp)         # 8-byte Spill
	movq	$0, 184(%rsp)           # 8-byte Folded Spill
	.align	16, 0x90
.LBB12_232:                             # %for.body.i2158.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_52 Depth=2
                                        #       Parent Loop BB12_221 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB12_233 Depth 5
                                        #             Child Loop BB12_266 Depth 6
	movl	last_spill_reg(%rip), %r15d
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%r14, %r14
	testq	%rdx, %rdx
	jle	.LBB12_273
	.align	16, 0x90
.LBB12_233:                             # %for.body4.i.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_52 Depth=2
                                        #       Parent Loop BB12_221 Depth=3
                                        #         Parent Loop BB12_232 Depth=4
                                        # =>        This Loop Header: Depth=5
                                        #             Child Loop BB12_266 Depth 6
	movq	208(%rsp), %rcx         # 8-byte Reload
	movslq	(%rcx), %rcx
	movq	%rcx, 232(%rsp)         # 8-byte Spill
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	movabsq	$0, %rcx
	jl	.LBB12_235
# BB#234:                               # %for.body4.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movq	%rax, %rcx
.LBB12_235:                             # %for.body4.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	subq	%rcx, %r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movswq	spill_regs(%rax,%rax), %r13
	movq	216(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %edx
	movq	224(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %esi
	movq	%r13, %rdi
	callq	reload_reg_free_p
	movq	%r12, %r9
	testq	%r9, %rax
	jne	.LBB12_236
# BB#237:                               # %lor.lhs.false.i.i347
                                        #   in Loop: Header=BB12_233 Depth=5
	movq	200(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r8
	testq	%r8, %r8
	je	.LBB12_271
# BB#238:                               # %land.lhs.true.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movq	reload_reg_used(%rip), %rax
	movq	$1, %rbx
	movq	%r13, %rcx
	shlq	%cl, %rbx
	testq	%rbx, %rax
	jne	.LBB12_271
# BB#239:                               # %land.lhs.true20.i.i354
                                        #   in Loop: Header=BB12_233 Depth=5
	movq	%r9, %r12
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r9
	movq	216(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %ecx
	movq	224(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %edx
	movq	192(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %esi
	movq	160(%rsp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	$1, 8(%rsp)
	movq	%r13, %rdi
	callq	free_for_value_p
	movq	%r12, %r9
	testq	%r9, %rax
	je	.LBB12_271
	jmp	.LBB12_240
	.align	16, 0x90
.LBB12_236:                             # %for.body4.land.lhs.true36_crit_edge.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movq	$1, %rbx
	movq	%r13, %rcx
	shlq	%cl, %rbx
.LBB12_240:                             # %land.lhs.true36.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	reg_class_contents(,%rax,8), %rax
	testq	%rbx, %rax
	je	.LBB12_271
# BB#241:                               # %land.lhs.true43.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movq	192(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %esi
	movq	%r13, %rdi
	movq	%r9, %r12
	callq	ix86_hard_regno_mode_ok
	movq	%r12, %r9
	testq	%r9, %rax
	je	.LBB12_271
# BB#242:                               # %land.lhs.true49.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movq	184(%rsp), %rax         # 8-byte Reload
	testq	%r9, %rax
	jne	.LBB12_245
# BB#243:                               # %lor.lhs.false51.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movq	reload_reg_used_at_all(%rip), %rax
	testq	%rbx, %rax
	je	.LBB12_271
# BB#244:                               # %land.lhs.true56.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movq	reload_reg_used_for_inherit(%rip), %rax
	testq	%rbx, %rax
	jne	.LBB12_271
.LBB12_245:                             # %if.then61.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movq	192(%rsp), %rax         # 8-byte Reload
	movslq	(%rax), %rax
	movq	%r13, %rcx
	andq	$65528, %rcx            # imm = 0xFFF8
	cmpq	$8, %rcx
	je	.LBB12_249
# BB#246:                               # %if.then61.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	leaq	-21(%r13), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB12_249
# BB#247:                               # %if.then61.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	leaq	-45(%r13), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB12_249
# BB#248:                               # %if.then61.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movq	%r13, %rcx
	addq	$-29, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$7, %rcx
	jbe	.LBB12_249
# BB#253:                               # %cond.false.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movq	%rax, %rcx
	andq	%r9, %rcx
	cmpq	$24, %rcx
	jne	.LBB12_254
# BB#256:                               # %cond.true114.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movzbl	target_flags+3(%rip), %ebx
	andq	$2, %rbx
	xorq	$6, %rbx
	jmp	.LBB12_258
.LBB12_249:                             # %cond.true.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %rbx
	je	.LBB12_258
# BB#250:                               # %lor.rhs.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	cmpq	$6, %rax
	movabsq	$0, %rbx
	jne	.LBB12_252
# BB#251:                               # %lor.rhs.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movabsq	$1, %rbx
.LBB12_252:                             # %lor.rhs.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	incq	%rbx
	jmp	.LBB12_258
.LBB12_254:                             # %cond.false.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	cmpq	$18, %rcx
	jne	.LBB12_257
# BB#255:                               # %cond.true104.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	xorq	$3, %rbx
	jmp	.LBB12_258
.LBB12_257:                             # %cond.false118.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
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
.LBB12_258:                             # %cond.end135.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	cmpq	$0, 152(%rsp)           # 8-byte Folded Reload
	je	.LBB12_260
# BB#259:                               # %if.then138.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movq	112(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %ebx
.LBB12_260:                             # %if.end142.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movq	%rbx, %r12
	andq	%r9, %r12
	cmpq	$1, %r12
	jne	.LBB12_261
# BB#263:                               # %if.then145.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	cmpq	$0, 152(%rsp)           # 8-byte Folded Reload
	jne	.LBB12_271
	jmp	.LBB12_264
.LBB12_261:                             # %while.cond.preheader.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB12_271
# BB#262:                               # %while.body.lr.ph.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	leaq	-1(%r13), %rax
	movq	%rax, 176(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB12_266:                             # %while.body.i.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_52 Depth=2
                                        #       Parent Loop BB12_221 Depth=3
                                        #         Parent Loop BB12_232 Depth=4
                                        #           Parent Loop BB12_233 Depth=5
                                        # =>          This Inner Loop Header: Depth=6
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	reg_class_contents(,%rax,8), %rdx
	movq	176(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%r12), %rax
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rdx, %rsi
	movq	%r12, %rcx
	je	.LBB12_269
# BB#267:                               # %land.lhs.true160.i.i
                                        #   in Loop: Header=BB12_266 Depth=6
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movswq	spill_reg_order(%rdx,%rdx), %rcx
	testq	%rcx, %rcx
	movq	%r12, %rcx
	js	.LBB12_269
# BB#268:                               # %land.lhs.true166.i.i
                                        #   in Loop: Header=BB12_266 Depth=6
	movq	216(%rsp), %rcx         # 8-byte Reload
	movl	(%rcx), %edx
	movq	224(%rsp), %rcx         # 8-byte Reload
	movl	(%rcx), %esi
	movq	%rax, %rdi
	movq	%r9, %r13
	callq	reload_reg_free_p
	movq	%r13, %r9
	testq	%r9, %rax
	movq	%r12, %rcx
	je	.LBB12_269
# BB#265:                               # %while.cond.i.i
                                        #   in Loop: Header=BB12_266 Depth=6
	decq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%r12
	cmpq	$2, %rax
	movq	%rbx, %rcx
	jge	.LBB12_266
.LBB12_269:                             # %while.end.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	andq	%r9, %rcx
	cmpq	$1, %rcx
	je	.LBB12_270
	.align	16, 0x90
.LBB12_271:                             # %for.inc.i.i
                                        #   in Loop: Header=BB12_233 Depth=5
	movq	%r9, %r12
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rdx
	movslq	n_spills(%rip), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB12_233
	jmp	.LBB12_272
.LBB12_264:                             #   in Loop: Header=BB12_232 Depth=4
	movq	%r9, %r12
	jmp	.LBB12_272
.LBB12_270:                             #   in Loop: Header=BB12_232 Depth=4
	movq	%r9, %r12
	.align	16, 0x90
.LBB12_272:                             # %for.end.i2161.loopexit.i
                                        #   in Loop: Header=BB12_232 Depth=4
	movl	n_spills(%rip), %eax
	movq	128(%rsp), %rsi         # 8-byte Reload
.LBB12_273:                             # %for.end.i2161.i
                                        #   in Loop: Header=BB12_232 Depth=4
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	cmpq	%rdx, %r14
	jl	.LBB12_274
# BB#231:                               # %for.cond.i.i
                                        #   in Loop: Header=BB12_232 Depth=4
	movq	184(%rsp), %rdx         # 8-byte Reload
	incq	%rdx
	movq	%rdx, 184(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$1, %rdx
	jle	.LBB12_232
	jmp	.LBB12_277
.LBB12_274:                             # %allocate_reload_reg.exit.i
                                        #   in Loop: Header=BB12_221 Depth=3
	movq	%r15, %rdi
	movq	%rsi, %rbx
	movq	160(%rsp), %rsi         # 8-byte Reload
	callq	set_reload_reg
	movq	%rbx, %rsi
	testq	%r12, %rax
	je	.LBB12_277
# BB#275:                               # %allocate_reload_reg.exit.for.inc1190_crit_edge.i
                                        #   in Loop: Header=BB12_221 Depth=3
	movl	n_reloads(%rip), %eax
	.align	16, 0x90
.LBB12_276:                             # %for.inc1190.i
                                        #   in Loop: Header=BB12_221 Depth=3
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rsi, %rdi
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	movq	%rdi, %rsi
	jl	.LBB12_221
	.align	16, 0x90
.LBB12_277:                             # %for.end1192.i
                                        #   in Loop: Header=BB12_52 Depth=2
	movl	n_reloads(%rip), %ebx
	movq	%rsi, %rax
	movq	%r12, %r9
	andq	%r9, %rax
	decq	168(%rsp)               # 8-byte Folded Spill
	cmpq	%rbx, %rax
	movq	104(%rsp), %r8          # 8-byte Reload
	jne	.LBB12_52
	jmp	.LBB12_298
.LBB12_278:                             # %if.then1200.critedge.i
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	136(%rsp), %rcx         # 8-byte Reload
	movl	96(%rcx), %ecx
	movq	%rax, %rdx
	movq	%r12, %r9
	andq	%r9, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB12_281
# BB#279:                               # %for.cond1206.preheader.i
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB12_280
# BB#282:                               # %for.body1209.lr.ph.i
                                        #   in Loop: Header=BB12_4 Depth=1
	xorq	%r14, %r14
	movq	%rax, %rsi
	.align	16, 0x90
.LBB12_283:                             # %for.body1209.i
                                        #   Parent Loop BB12_4 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB12_288 Depth 3
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	88(%rax), %rcx
	imulq	$104, %r14, %r15
	movslq	56(%rcx,%r15), %rax
	testq	%rax, %rax
	js	.LBB12_297
# BB#284:                               # %lor.lhs.false1215.i
                                        #   in Loop: Header=BB12_283 Depth=2
	cmpq	$0, 64(%rcx,%r15)
	jne	.LBB12_297
# BB#285:                               # %if.end1223.i
                                        #   in Loop: Header=BB12_283 Depth=2
	movl	92(%rcx,%r15), %ecx
	movl	rld+92(%r15), %edx
	cmpq	%rdx, %rcx
	jne	.LBB12_989
# BB#286:                               # %for.cond1235.preheader.i
                                        #   in Loop: Header=BB12_283 Depth=2
	movslq	n_spills(%rip), %rcx
	testq	%rcx, %rcx
	jle	.LBB12_297
# BB#287:                               # %for.body1238.lr.ph.i
                                        #   in Loop: Header=BB12_283 Depth=2
	leaq	rld(%r15), %rcx
	movq	%rcx, 232(%rsp)         # 8-byte Spill
	leaq	rld+64(%r15), %rcx
	movq	%rcx, 224(%rsp)         # 8-byte Spill
	leaq	rld+96(%r15), %rcx
	movq	%rcx, 216(%rsp)         # 8-byte Spill
	movq	$1, %r12
	jmp	.LBB12_288
	.align	16, 0x90
.LBB12_295:                             # %for.inc1255.for.body1238_crit_edge.i
                                        #   in Loop: Header=BB12_288 Depth=3
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	88(%rax), %rax
	movl	56(%rax,%r15), %eax
	incq	%r12
.LBB12_288:                             # %for.body1238.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_283 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movswq	spill_regs-2(%r12,%r12), %rcx
	andq	%r9, %rcx
	andq	%r9, %rax
	cmpq	%rax, %rcx
	jne	.LBB12_294
# BB#289:                               # %if.then1248.i
                                        #   in Loop: Header=BB12_288 Depth=3
	leaq	-1(%r12), %rdi
	movq	%r14, %rsi
	movq	%r9, %rbx
	callq	set_reload_reg
	movq	%rbx, %r9
	testq	%r9, %rax
	jne	.LBB12_294
# BB#290:                               # %if.then1251.i
                                        #   in Loop: Header=BB12_288 Depth=3
	movq	%r9, %r13
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rbx
	movq	32(%rbx), %rdi
	callq	asm_noperands
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB12_291
# BB#293:                               # %failed_reload.exit.i
                                        #   in Loop: Header=BB12_288 Depth=3
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str9, %rsi
	callq	error_for_asm
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	$0, (%rax)
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movq	216(%rsp), %rcx         # 8-byte Reload
	movzbl	(%rcx), %eax
	orq	$5, %rax
	movb	%al, (%rcx)
	movq	%r13, %r9
.LBB12_294:                             # %for.inc1255.i
                                        #   in Loop: Header=BB12_288 Depth=3
	movslq	n_spills(%rip), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB12_295
# BB#296:                               # %for.cond1235.for.inc1258.loopexit_crit_edge.i
                                        #   in Loop: Header=BB12_283 Depth=2
	movl	n_reloads(%rip), %esi
	.align	16, 0x90
.LBB12_297:                             # %for.inc1258.i
                                        #   in Loop: Header=BB12_283 Depth=2
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB12_283
	jmp	.LBB12_298
.LBB12_280:                             #   in Loop: Header=BB12_4 Depth=1
	movq	%rax, %rsi
.LBB12_298:                             # %if.end1261.i
                                        #   in Loop: Header=BB12_4 Depth=1
	movslq	flag_expensive_optimizations(%rip), %r14
	testq	%r14, %r14
	js	.LBB12_309
	.align	16, 0x90
.LBB12_299:                             # %for.cond1266.preheader.i
                                        #   Parent Loop BB12_4 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB12_300 Depth 3
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %r15
	testq	%rax, %rax
	jle	.LBB12_322
	.align	16, 0x90
.LBB12_300:                             # %for.body1269.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_299 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movswq	reload_order-2(%r15,%r15), %rbx
	movzbl	reload_inherited(%rbx), %eax
	testq	%rax, %rax
	je	.LBB12_302
# BB#301:                               # %land.lhs.true1278.i
                                        #   in Loop: Header=BB12_300 Depth=3
	imulq	$104, %rbx, %rax
	movq	rld+64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB12_304
.LBB12_302:                             # %if.else1287.i
                                        #   in Loop: Header=BB12_300 Depth=3
	movq	reload_override_in(,%rbx,8), %rdi
	testq	%rdi, %rdi
	je	.LBB12_321
# BB#303:                               # %land.lhs.true1291.i
                                        #   in Loop: Header=BB12_300 Depth=3
	movl	(%rdi), %eax
	orq	$2, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB12_321
.LBB12_304:                             # %if.end1310.i
                                        #   in Loop: Header=BB12_300 Depth=3
	movq	%r9, %r13
	callq	true_regnum
	imulq	$104, %rbx, %r12
	movl	rld+28(%r12), %esi
	movl	rld+72(%r12), %edx
	movl	rld+92(%r12), %ecx
	movzbl	reload_inherited(%rbx), %edi
	movq	rld(%r12), %r8
	testq	%rdi, %rdi
	movabsq	$const_int_rtx+512, %rdi
	je	.LBB12_306
# BB#305:                               # %cond.true1328.i
                                        #   in Loop: Header=BB12_300 Depth=3
	leaq	rld+8(%r12), %rdi
.LBB12_306:                             # %cond.end1333.i
                                        #   in Loop: Header=BB12_300 Depth=3
	movq	(%rdi), %r9
	movq	%rbx, (%rsp)
	movq	$1, 8(%rsp)
	movq	%rax, %rdi
	callq	free_for_value_p
	movq	%r13, %r9
	testq	%r9, %rax
	jne	.LBB12_311
# BB#307:                               # %if.then1337.i
                                        #   in Loop: Header=BB12_300 Depth=3
	testq	%r9, %r14
	jne	.LBB12_321
# BB#308:                               # %if.end1340.i
                                        #   in Loop: Header=BB12_300 Depth=3
	xorq	%r14, %r14
	movb	%r14b, reload_inherited(%rbx)
	movq	$0, reload_override_in(,%rbx,8)
	jmp	.LBB12_321
	.align	16, 0x90
.LBB12_311:                             # %if.else1345.i
                                        #   in Loop: Header=BB12_300 Depth=3
	leaq	rld(%r12), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB12_321
# BB#312:                               # %land.lhs.true1350.i
                                        #   in Loop: Header=BB12_300 Depth=3
	movq	%r9, %rbx
	movq	rld+8(%r12), %rax
	cmpq	%rdi, %rax
	jne	.LBB12_314
# BB#313:                               #   in Loop: Header=BB12_300 Depth=3
	movq	%rbx, %r9
	jmp	.LBB12_321
.LBB12_314:                             # %land.lhs.true1359.i
                                        #   in Loop: Header=BB12_300 Depth=3
	callq	remove_address_replacements
	movq	%rbx, %r9
	testq	%r9, %rax
	movabsq	$0, %rax
	je	.LBB12_316
# BB#315:                               # %land.lhs.true1359.i
                                        #   in Loop: Header=BB12_300 Depth=3
	movabsq	$1, %rax
.LBB12_316:                             # %land.lhs.true1359.i
                                        #   in Loop: Header=BB12_300 Depth=3
	testq	%r9, %r14
	movabsq	$0, %rcx
	je	.LBB12_318
# BB#317:                               # %land.lhs.true1359.i
                                        #   in Loop: Header=BB12_300 Depth=3
	movabsq	$1, %rcx
.LBB12_318:                             # %land.lhs.true1359.i
                                        #   in Loop: Header=BB12_300 Depth=3
	testq	%rcx, %rax
	movq	$2, %rax
	jne	.LBB12_320
# BB#319:                               # %land.lhs.true1359.i
                                        #   in Loop: Header=BB12_300 Depth=3
	movq	%r14, %rax
.LBB12_320:                             # %land.lhs.true1359.i
                                        #   in Loop: Header=BB12_300 Depth=3
	movq	%rax, %r14
	.align	16, 0x90
.LBB12_321:                             # %for.inc1370.i
                                        #   in Loop: Header=BB12_300 Depth=3
	movslq	n_reloads(%rip), %rsi
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r15
	cmpq	%rsi, %rax
	jl	.LBB12_300
.LBB12_322:                             # %for.inc1373.i
                                        #   in Loop: Header=BB12_299 Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%r14
	testq	%rax, %rax
	jg	.LBB12_299
.LBB12_309:                             # %for.cond1376.preheader.i
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rdx
	movq	%rsi, %rdi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rax
	testq	%rdx, %rdx
	movabsq	$rld, %rcx
	jle	.LBB12_310
	.align	16, 0x90
.LBB12_323:                             # %for.body1379.i
                                        #   Parent Loop BB12_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	reload_override_in-8(,%rax,8), %rdx
	testq	%rdx, %rdx
	je	.LBB12_325
# BB#324:                               # %if.then1383.i
                                        #   in Loop: Header=BB12_323 Depth=2
	movq	%rdx, (%rcx)
.LBB12_325:                             # %for.inc1390.i
                                        #   in Loop: Header=BB12_323 Depth=2
	movq	%rax, %rdx
	andq	%r9, %rdx
	movq	%rdi, %rsi
	andq	%r9, %rsi
	incq	%rax
	addq	$104, %rcx
	cmpq	%rsi, %rdx
	movq	$1, %rbx
	jne	.LBB12_323
# BB#326:                               #   in Loop: Header=BB12_4 Depth=1
	movabsq	$rld+96, %r14
	movq	%rdi, %rsi
	.align	16, 0x90
.LBB12_327:                             # %for.body1396.i
                                        #   Parent Loop BB12_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-32(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB12_336
# BB#328:                               # %land.lhs.true1402.i
                                        #   in Loop: Header=BB12_327 Depth=2
	movzbl	(%r14), %eax
	testq	$1, %rax
	je	.LBB12_330
# BB#329:                               # %land.lhs.true1410.i
                                        #   in Loop: Header=BB12_327 Depth=2
	movzbl	reload_inherited-1(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB12_333
.LBB12_330:                             # %lor.lhs.false1414.i
                                        #   in Loop: Header=BB12_327 Depth=2
	cmpq	$0, -96(%r14)
	jne	.LBB12_336
# BB#331:                               # %land.lhs.true1420.i
                                        #   in Loop: Header=BB12_327 Depth=2
	cmpq	$0, -88(%r14)
	jne	.LBB12_336
# BB#332:                               # %land.lhs.true1420.i
                                        #   in Loop: Header=BB12_327 Depth=2
	andq	$4, %rax
	testq	$255, %rax
	jne	.LBB12_336
.LBB12_333:                             # %if.then1435.i
                                        #   in Loop: Header=BB12_327 Depth=2
	movq	%r9, %r15
	callq	true_regnum
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movswq	spill_reg_order(%rdx,%rdx), %rcx
	testq	%rcx, %rcx
	js	.LBB12_335
# BB#334:                               # %if.then1446.i
                                        #   in Loop: Header=BB12_327 Depth=2
	movl	-68(%r14), %ecx
	movl	-4(%r14), %edx
	movl	-24(%r14), %esi
	movq	%rax, %rdi
	callq	clear_reload_reg_in_use
.LBB12_335:                             # %if.end1456.i
                                        #   in Loop: Header=BB12_327 Depth=2
	movl	n_reloads(%rip), %esi
	movq	$0, -32(%r14)
	movq	%r15, %r9
	movl	%r9d, reload_spill_index-4(,%rbx,4)
.LBB12_336:                             # %for.inc1463.i
                                        #   in Loop: Header=BB12_327 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	addq	$104, %r14
	cmpq	%rdx, %rax
	jl	.LBB12_327
# BB#337:                               # %for.cond1466.preheader.i
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%r11, %r11
	testq	%rax, %rax
	movq	$1, %r14
	jle	.LBB12_380
	.align	16, 0x90
.LBB12_338:                             # %for.body1469.i
                                        #   Parent Loop BB12_4 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB12_356 Depth 3
                                        #       Child Loop BB12_372 Depth 3
	movswq	reload_order(%r11,%r11), %rax
	imulq	$104, %rax, %r10
	movq	rld+48(%r10), %rcx
	testq	%rcx, %rcx
	je	.LBB12_379
# BB#339:                               # %land.lhs.true1480.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB12_379
# BB#340:                               # %land.lhs.true1488.i
                                        #   in Loop: Header=BB12_338 Depth=2
	cmpq	$0, rld+64(%r10)
	je	.LBB12_379
# BB#341:                               # %if.then1494.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movslq	8(%rcx), %rbx
	movl	reload_spill_index(,%rax,4), %r8d
	cmpq	$52, %rbx
	movq	$1, %rax
	jg	.LBB12_355
# BB#342:                               # %if.then1504.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movslq	rld+28(%r10), %rax
	movq	%rbx, %rcx
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	je	.LBB12_346
# BB#343:                               # %if.then1504.i
                                        #   in Loop: Header=BB12_338 Depth=2
	leaq	-21(%rbx), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_346
# BB#344:                               # %if.then1504.i
                                        #   in Loop: Header=BB12_338 Depth=2
	leaq	-45(%rbx), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_346
# BB#345:                               # %if.then1504.i
                                        #   in Loop: Header=BB12_338 Depth=2
	leaq	-29(%rbx), %rcx
	andq	%r9, %rcx
	cmpq	$7, %rcx
	jbe	.LBB12_346
# BB#350:                               # %cond.false1546.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movq	%rax, %rcx
	andq	%r9, %rcx
	cmpq	$24, %rcx
	jne	.LBB12_351
# BB#353:                               # %cond.true1562.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB12_355
.LBB12_346:                             # %cond.true1528.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB12_355
# BB#347:                               # %lor.rhs1536.i
                                        #   in Loop: Header=BB12_338 Depth=2
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB12_349
# BB#348:                               # %lor.rhs1536.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movabsq	$1, %rax
.LBB12_349:                             # %lor.rhs1536.i
                                        #   in Loop: Header=BB12_338 Depth=2
	incq	%rax
	jmp	.LBB12_355
.LBB12_351:                             # %cond.false1546.i
                                        #   in Loop: Header=BB12_338 Depth=2
	cmpq	$18, %rcx
	jne	.LBB12_354
# BB#352:                               # %cond.true1552.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB12_355
.LBB12_354:                             # %while.cond.preheader.i360
                                        #   in Loop: Header=BB12_338 Depth=2
	movq	%r9, %rsi
	movl	target_flags(%rip), %r9d
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %r9
	andq	$4, %r9
	leaq	3(%rax,%r9), %rax
	addq	$4, %r9
	cqto
	idivq	%r9
	movq	%rsi, %r9
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB12_357
.LBB12_355:                             # %while.body.lr.ph.i361
                                        #   in Loop: Header=BB12_338 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rax
	.align	16, 0x90
.LBB12_356:                             # %while.body.i364
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_338 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	reg_has_output_reload(%rip), %rcx
	addq	%rbx, %rcx
	movb	%r14b, (%rax,%rcx)
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%rax
	testq	%rdx, %rdx
	jg	.LBB12_356
.LBB12_357:                             # %while.end.i365
                                        #   in Loop: Header=BB12_338 Depth=2
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB12_374
# BB#358:                               # %if.then1597.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movslq	rld+28(%r10), %rax
	movq	%r8, %rcx
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	je	.LBB12_362
# BB#359:                               # %if.then1597.i
                                        #   in Loop: Header=BB12_338 Depth=2
	leaq	-21(%r8), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_362
# BB#360:                               # %if.then1597.i
                                        #   in Loop: Header=BB12_338 Depth=2
	leaq	-45(%r8), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_362
# BB#361:                               # %if.then1597.i
                                        #   in Loop: Header=BB12_338 Depth=2
	leaq	-29(%r8), %rcx
	andq	%r9, %rcx
	cmpq	$7, %rcx
	jbe	.LBB12_362
# BB#366:                               # %cond.false1639.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movq	%rax, %rcx
	andq	%r9, %rcx
	cmpq	$24, %rcx
	jne	.LBB12_367
# BB#369:                               # %cond.true1655.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB12_371
.LBB12_362:                             # %cond.true1621.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB12_371
# BB#363:                               # %lor.rhs1629.i
                                        #   in Loop: Header=BB12_338 Depth=2
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB12_365
# BB#364:                               # %lor.rhs1629.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movabsq	$1, %rax
.LBB12_365:                             # %lor.rhs1629.i
                                        #   in Loop: Header=BB12_338 Depth=2
	incq	%rax
	jmp	.LBB12_371
.LBB12_367:                             # %cond.false1639.i
                                        #   in Loop: Header=BB12_338 Depth=2
	cmpq	$18, %rcx
	jne	.LBB12_370
# BB#368:                               # %cond.true1645.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB12_371
.LBB12_370:                             # %while.cond1681.preheader.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB12_374
.LBB12_371:                             # %while.body1685.lr.ph.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movq	reg_is_output_reload(%rip), %rbx
	.align	16, 0x90
.LBB12_372:                             # %while.body1685.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_338 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rbx, %rdi
	leaq	-1(%rax,%r8), %rdx
	leaq	-1(%rax), %rax
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$1, %rbx
	movq	%rdx, %rcx
	shlq	%cl, %rbx
	orq	%rdi, %rbx
	testq	%rsi, %rsi
	jg	.LBB12_372
# BB#373:                               # %while.cond1681.if.end1691.loopexit_crit_edge.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movq	%rbx, reg_is_output_reload(%rip)
.LBB12_374:                             # %if.end1691.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movl	rld+92(%r10), %eax
	cmpq	$1, %rax
	je	.LBB12_378
# BB#375:                               # %if.end1691.i
                                        #   in Loop: Header=BB12_338 Depth=2
	cmpq	$9, %rax
	je	.LBB12_378
# BB#376:                               # %if.end1691.i
                                        #   in Loop: Header=BB12_338 Depth=2
	cmpq	$2, %rax
	jne	.LBB12_377
.LBB12_378:                             # %if.end1691.for.inc1712_crit_edge.i
                                        #   in Loop: Header=BB12_338 Depth=2
	movl	n_reloads(%rip), %esi
	.align	16, 0x90
.LBB12_379:                             # %for.inc1712.i
                                        #   in Loop: Header=BB12_338 Depth=2
	incq	%r11
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB12_338
	jmp	.LBB12_380
.LBB12_310:                             #   in Loop: Header=BB12_4 Depth=1
	movq	%rdi, %rsi
.LBB12_380:                             # %choose_reload_regs.exit
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	%r9, %r13
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%r14, %r14
	testq	%rax, %rax
	jle	.LBB12_437
	.align	16, 0x90
.LBB12_381:                             # %for.body.i371
                                        #   Parent Loop BB12_4 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB12_390 Depth 3
                                        #       Child Loop BB12_417 Depth 3
                                        #       Child Loop BB12_427 Depth 3
	imulq	$104, %r14, %rbx
	cmpq	$0, rld(%rbx)
	je	.LBB12_436
# BB#382:                               # %lor.lhs.false.i373
                                        #   in Loop: Header=BB12_381 Depth=2
	movl	rld+92(%rbx), %eax
	cmpq	$9, %rax
	je	.LBB12_436
# BB#383:                               # %lor.lhs.false5.i
                                        #   in Loop: Header=BB12_381 Depth=2
	cmpq	$0, rld+8(%rbx)
	jne	.LBB12_436
# BB#384:                               # %lor.lhs.false9.i
                                        #   in Loop: Header=BB12_381 Depth=2
	movq	rld+64(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB12_436
# BB#385:                               # %lor.lhs.false13.i
                                        #   in Loop: Header=BB12_381 Depth=2
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	reg_set_p
	movl	n_reloads(%rip), %esi
	movq	%r13, %r8
	testq	%r8, %rax
	jne	.LBB12_386
# BB#387:                               # %for.cond17.preheader.i
                                        #   in Loop: Header=BB12_381 Depth=2
	leaq	rld(%rbx), %rax
	movq	%rax, 200(%rsp)         # 8-byte Spill
	leaq	rld+92(%rbx), %rax
	movq	%rax, 224(%rsp)         # 8-byte Spill
	leaq	rld+64(%rbx), %rax
	movq	%rax, 232(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB12_388
# BB#389:                               # %for.body19.lr.ph.i
                                        #   in Loop: Header=BB12_381 Depth=2
	leaq	rld+72(%rbx), %rax
	movq	%rax, 184(%rsp)         # 8-byte Spill
	movq	$0, 216(%rsp)           # 8-byte Folded Spill
	movq	$30, 208(%rsp)          # 8-byte Folded Spill
	movq	$1, %r15
	movabsq	$rld+92, %r13
	movq	%r8, 192(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB12_390:                             # %for.body19.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_381 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r14, %rax
	leaq	-1(%r15), %r12
	andq	%r8, %rax
	movq	%r12, %rcx
	andq	%r8, %rcx
	cmpq	%rcx, %rax
	je	.LBB12_410
# BB#391:                               # %lor.lhs.false21.i
                                        #   in Loop: Header=BB12_390 Depth=3
	movq	%r8, %rbx
	movq	-28(%r13), %rdi
	testq	%rdi, %rdi
	jne	.LBB12_393
# BB#392:                               #   in Loop: Header=BB12_390 Depth=3
	movq	%rbx, %r8
	jmp	.LBB12_410
	.align	16, 0x90
.LBB12_393:                             # %lor.lhs.false26.i
                                        #   in Loop: Header=BB12_390 Depth=3
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rsi
	callq	reg_overlap_mentioned_p
	movq	%rbx, %r8
	testq	%r8, %rax
	je	.LBB12_410
# BB#394:                               # %if.end36.i379
                                        #   in Loop: Header=BB12_390 Depth=3
	movl	(%r13), %eax
	movq	%r8, %rbx
	cmpq	$3, %rax
	jne	.LBB12_398
# BB#395:                               # %land.lhs.true.i381
                                        #   in Loop: Header=BB12_390 Depth=3
	movslq	-20(%r13), %rax
	movq	$32, %rcx
	movq	192(%rsp), %rsi         # 8-byte Reload
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB12_397
# BB#396:                               # %land.lhs.true.i381
                                        #   in Loop: Header=BB12_390 Depth=3
	movq	%rsi, %rax
.LBB12_397:                             # %land.lhs.true.i381
                                        #   in Loop: Header=BB12_390 Depth=3
	movq	%rax, 192(%rsp)         # 8-byte Spill
.LBB12_398:                             # %if.end48.i
                                        #   in Loop: Header=BB12_390 Depth=3
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rdi
	movq	-28(%r13), %rsi
	callq	rtx_equal_p
	movq	%rbx, %r8
	testq	%r8, %rax
	je	.LBB12_402
# BB#399:                               # %lor.lhs.false57.i
                                        #   in Loop: Header=BB12_390 Depth=3
	cmpq	$0, -84(%r13)
	jne	.LBB12_402
# BB#400:                               # %lor.lhs.false62.i
                                        #   in Loop: Header=BB12_390 Depth=3
	movq	-92(%r13), %rsi
	testq	%rsi, %rsi
	je	.LBB12_402
# BB#401:                               # %lor.lhs.false67.i
                                        #   in Loop: Header=BB12_390 Depth=3
	movq	200(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rdi
	movq	%r8, %rbx
	callq	rtx_equal_p
	movq	%rbx, %r8
	testq	%r8, %rax
	jne	.LBB12_410
.LBB12_402:                             # %if.then76.i
                                        #   in Loop: Header=BB12_390 Depth=3
	movl	(%r13), %eax
	testq	%rax, %rax
	jne	.LBB12_411
# BB#403:                               # %lor.lhs.false81.i
                                        #   in Loop: Header=BB12_390 Depth=3
	movq	224(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %eax
	cmpq	$10, %rax
	jne	.LBB12_404
# BB#406:                               # %land.lhs.true94.if.end100_crit_edge.i
                                        #   in Loop: Header=BB12_390 Depth=3
	movl	-20(%r13), %eax
	jmp	.LBB12_407
.LBB12_404:                             # %lor.lhs.false81.i
                                        #   in Loop: Header=BB12_390 Depth=3
	cmpq	$3, %rax
	jne	.LBB12_411
# BB#405:                               # %lor.lhs.false86.i
                                        #   in Loop: Header=BB12_390 Depth=3
	movq	184(%rsp), %rax         # 8-byte Reload
	movslq	(%rax), %rcx
	movslq	-20(%r13), %rax
	cmpq	%rax, %rcx
	jg	.LBB12_411
.LBB12_407:                             # %if.end100.i
                                        #   in Loop: Header=BB12_390 Depth=3
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	208(%rsp), %rdi         # 8-byte Reload
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jg	.LBB12_409
# BB#408:                               # %if.end100.i
                                        #   in Loop: Header=BB12_390 Depth=3
	movq	%rdi, %rax
.LBB12_409:                             # %if.end100.i
                                        #   in Loop: Header=BB12_390 Depth=3
	movq	$1, 216(%rsp)           # 8-byte Folded Spill
	movq	%rax, 208(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB12_410:                             # %for.inc.i385
                                        #   in Loop: Header=BB12_390 Depth=3
	movslq	n_reloads(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%r15, %r12
	incq	%r15
	addq	$104, %r13
	cmpq	%rax, %rdx
	jl	.LBB12_390
.LBB12_411:                             # %for.end.i388.loopexit
                                        #   in Loop: Header=BB12_381 Depth=2
	movl	n_reloads(%rip), %esi
	movq	208(%rsp), %rax         # 8-byte Reload
	movq	192(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB12_412
.LBB12_386:                             #   in Loop: Header=BB12_381 Depth=2
	movq	%r8, %r13
	jmp	.LBB12_436
.LBB12_388:                             #   in Loop: Header=BB12_381 Depth=2
	movq	$30, %rax
	movq	$0, 216(%rsp)           # 8-byte Folded Spill
	xorq	%r12, %r12
	movq	%r8, %rdx
.LBB12_412:                             # %for.end.i388
                                        #   in Loop: Header=BB12_381 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jg	.LBB12_413
# BB#414:                               # %for.end.i388
                                        #   in Loop: Header=BB12_381 Depth=2
	movq	%rsi, %rax
	andq	%r8, %rax
	andq	%r8, %r12
	movq	%r8, %r13
	cmpq	%rax, %r12
	jne	.LBB12_436
# BB#415:                               # %for.end.i388
                                        #   in Loop: Header=BB12_381 Depth=2
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB12_436
# BB#416:                               # %for.body117.lr.ph.i
                                        #   in Loop: Header=BB12_381 Depth=2
	movq	$1, %rbx
	movabsq	$rld+92, %r12
	movq	%r13, %rdx
	.align	16, 0x90
.LBB12_417:                             # %for.body117.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_381 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r14, %rax
	leaq	-1(%rbx), %r15
	andq	%rdx, %rax
	movq	%r15, %rcx
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	je	.LBB12_424
# BB#418:                               # %land.lhs.true119.i
                                        #   in Loop: Header=BB12_417 Depth=3
	movq	-28(%r12), %rsi
	testq	%rsi, %rsi
	je	.LBB12_424
# BB#419:                               # %land.lhs.true124.i
                                        #   in Loop: Header=BB12_417 Depth=3
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rdi
	movq	%rdx, %r13
	callq	rtx_equal_p
	movq	%r13, %rdx
	testq	%rdx, %rax
	je	.LBB12_424
# BB#420:                               # %land.lhs.true133.i
                                        #   in Loop: Header=BB12_417 Depth=3
	movq	216(%rsp), %rax         # 8-byte Reload
	testq	%rdx, %rax
	je	.LBB12_423
# BB#421:                               # %lor.lhs.false135.i
                                        #   in Loop: Header=BB12_417 Depth=3
	movl	(%r12), %eax
	cmpq	$3, %rax
	je	.LBB12_423
# BB#422:                               # %lor.lhs.false135.i
                                        #   in Loop: Header=BB12_417 Depth=3
	cmpq	$10, %rax
	jne	.LBB12_424
.LBB12_423:                             # %if.then145.i
                                        #   in Loop: Header=BB12_417 Depth=3
	movq	$9, %rax
	movq	224(%rsp), %rcx         # 8-byte Reload
	movl	%eax, (%rcx)
	movq	$0, -92(%r12)
	movl	%edx, reload_spill_index-4(,%rbx,4)
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rdx, %r15
	callq	transfer_replacements
	movq	%r15, %rdx
	.align	16, 0x90
.LBB12_424:                             # %for.inc155.i
                                        #   in Loop: Header=BB12_417 Depth=3
	movslq	n_reloads(%rip), %rsi
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rbx
	addq	$104, %r12
	cmpq	%rsi, %rax
	jl	.LBB12_417
# BB#425:                               # %for.end157.i
                                        #   in Loop: Header=BB12_381 Depth=2
	movq	%rdx, %r13
	movq	224(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %eax
	cmpq	$9, %rax
	jne	.LBB12_436
# BB#426:                               # %for.end157.i
                                        #   in Loop: Header=BB12_381 Depth=2
	movq	$1, %r15
	testq	%rsi, %rsi
	movabsq	$rld, %rbx
	jle	.LBB12_436
	.align	16, 0x90
.LBB12_427:                             # %for.body165.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_381 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB12_435
# BB#428:                               # %land.lhs.true170.i
                                        #   in Loop: Header=BB12_427 Depth=3
	movl	92(%rbx), %eax
	cmpq	$9, %rax
	je	.LBB12_435
# BB#429:                               # %land.lhs.true175.i
                                        #   in Loop: Header=BB12_427 Depth=3
	movq	200(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rsi
	callq	reg_overlap_mentioned_for_reload_p
	testq	%r13, %rax
	je	.LBB12_435
# BB#430:                               # %if.then184.i
                                        #   in Loop: Header=BB12_427 Depth=3
	movl	92(%rbx), %ecx
	cmpq	$3, %rcx
	movq	$10, %rax
	je	.LBB12_434
# BB#431:                               # %lor.rhs.i392
                                        #   in Loop: Header=BB12_427 Depth=3
	cmpq	$4, %rcx
	movabsq	$0, %rax
	jne	.LBB12_433
# BB#432:                               # %lor.rhs.i392
                                        #   in Loop: Header=BB12_427 Depth=3
	movabsq	$1, %rax
.LBB12_433:                             # %lor.rhs.i392
                                        #   in Loop: Header=BB12_427 Depth=3
	addq	$9, %rax
.LBB12_434:                             # %lor.end.i
                                        #   in Loop: Header=BB12_427 Depth=3
	movl	%eax, 92(%rbx)
	.align	16, 0x90
.LBB12_435:                             # %for.inc197.i
                                        #   in Loop: Header=BB12_427 Depth=3
	movslq	n_reloads(%rip), %rsi
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r15
	addq	$104, %rbx
	cmpq	%rsi, %rax
	jl	.LBB12_427
	jmp	.LBB12_436
.LBB12_413:                             #   in Loop: Header=BB12_381 Depth=2
	movq	%r8, %r13
	.align	16, 0x90
.LBB12_436:                             # %for.inc202.i
                                        #   in Loop: Header=BB12_381 Depth=2
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB12_381
.LBB12_437:                             # %merge_assigned_reloads.exit
                                        #   in Loop: Header=BB12_4 Depth=1
	movslq	reload_n_operands(%rip), %rax
	movq	136(%rsp), %r14         # 8-byte Reload
	movq	32(%r14), %rcx
	movq	%rcx, 176(%rsp)         # 8-byte Spill
	movq	$0, reg_reloaded_died(%rip)
	testq	%rax, %rax
	jle	.LBB12_442
# BB#438:                               # %for.body.lr.ph.i
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	%rsi, %r15
	cmpq	$1, %rax
	jg	.LBB12_439
# BB#440:                               # %for.body.lr.ph.i
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	$8, %rbx
	jmp	.LBB12_441
.LBB12_439:                             #   in Loop: Header=BB12_4 Depth=1
	decq	%rax
	andq	%r13, %rax
	leaq	8(,%rax,8), %rbx
.LBB12_441:                             # %for.body.lr.ph.i
                                        #   in Loop: Header=BB12_4 Depth=1
	xorq	%rsi, %rsi
	movabsq	$other_output_reload_insns, %rdi
	movq	%rbx, %rdx
	callq	memset
	xorq	%rsi, %rsi
	movabsq	$outaddr_address_reload_insns, %rdi
	movq	%rbx, %rdx
	callq	memset
	xorq	%rsi, %rsi
	movabsq	$output_address_reload_insns, %rdi
	movq	%rbx, %rdx
	callq	memset
	xorq	%rsi, %rsi
	movabsq	$output_reload_insns, %rdi
	movq	%rbx, %rdx
	callq	memset
	xorq	%rsi, %rsi
	movabsq	$inpaddr_address_reload_insns, %rdi
	movq	%rbx, %rdx
	callq	memset
	xorq	%rsi, %rsi
	movabsq	$input_address_reload_insns, %rdi
	movq	%rbx, %rdx
	callq	memset
	xorq	%rsi, %rsi
	movabsq	$input_reload_insns, %rdi
	movq	%rbx, %rdx
	callq	memset
	movq	%r15, %rsi
.LBB12_442:                             # %for.end.i
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	$0, other_input_address_reload_insns(%rip)
	movq	rtl_dump_file(%rip), %rdi
	movq	$0, other_input_reload_insns(%rip)
	movq	$0, operand_reload_insns(%rip)
	movq	$0, other_operand_reload_insns(%rip)
	testq	%rdi, %rdi
	je	.LBB12_444
# BB#443:                               # %if.then.i290
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	176(%rsp), %rax         # 8-byte Reload
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str5, %rsi
	callq	fprintf
	movq	rtl_dump_file(%rip), %rdi
	callq	debug_reload_to_stream
	movl	n_reloads(%rip), %esi
.LBB12_444:                             # %for.cond15.preheader.i
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%r15, %r15
	testq	%rsi, %rsi
	movq	%r13, %r8
	jle	.LBB12_786
	jmp	.LBB12_445
.LBB12_717:                             #   in Loop: Header=BB12_445 Depth=2
	movq	%r8, %r12
	movq	%rbx, %r13
	movq	%r14, %rbx
.LBB12_732:                             # %if.then141.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	160(%rsp), %rdi         # 8-byte Reload
	jmp	.LBB12_733
.LBB12_719:                             # %if.then60.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r8, %r12
	movq	%rbx, %r13
	leaq	(%rax,%rax,4), %rcx
	xorq	%rax, %rax
	movq	152(%rsp), %rdi         # 8-byte Reload
	movq	%r14, %rsi
	movq	%r9, %rdx
	jmp	.LBB12_720
.LBB12_724:                             # %if.then82.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rcx, 200(%rsp)         # 8-byte Spill
	movslq	rld+80(%r13), %rcx
	movl	(%rsi), %eax
	imulq	$104, %rcx, %rcx
	movq	rld+64(%rcx), %rcx
	movq	%rcx, 160(%rsp)         # 8-byte Spill
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB12_725
# BB#726:                               # %land.lhs.true92.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rdx, %r13
	movq	8(%rsi), %rdx
	movq	%rsi, 152(%rsp)         # 8-byte Spill
	movq	$16, %rcx
	movzbl	2(%rdx), %edi
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %eax
	movzbl	mode_size(%rdi), %ecx
	cmpq	%rcx, %rax
	jbe	.LBB12_730
# BB#727:                               # %land.lhs.true109.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r8, %r12
	movq	%r9, %rsi
	movq	%r9, %rbx
	callq	gen_lowpart_common
	testq	%rax, %rax
	jne	.LBB12_729
# BB#728:                               #   in Loop: Header=BB12_445 Depth=2
	movq	%r12, %r8
	movq	%rbx, %r9
	jmp	.LBB12_730
.LBB12_725:                             #   in Loop: Header=BB12_445 Depth=2
	movq	%rsi, 152(%rsp)         # 8-byte Spill
	movq	%rdx, %r13
	jmp	.LBB12_730
.LBB12_729:                             # %if.then119.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	152(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx), %rcx
	movq	%rcx, 152(%rsp)         # 8-byte Spill
	movq	%rax, %r9
	movq	%r12, %r8
.LBB12_730:                             # %if.end123.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	192(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %ecx
	movl	rld+72(%r8), %edx
	movq	%r8, %r12
	movq	%r9, %rdi
	movq	%r14, %rsi
	movq	%r9, %rbx
	callq	gen_reload
	movq	200(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%rax,4), %rcx
	xorq	%rax, %rax
	movq	152(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	movq	160(%rsp), %rdx         # 8-byte Reload
.LBB12_720:                             # %if.end200.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	callq	*insn_data+16(,%rcx,8)
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB12_743
.LBB12_634:                             #   in Loop: Header=BB12_445 Depth=2
	movq	%r14, %r15
	jmp	.LBB12_646
	.align	16, 0x90
.LBB12_445:                             # %for.body17.i
                                        #   Parent Loop BB12_4 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB12_494 Depth 3
                                        #       Child Loop BB12_558 Depth 3
                                        #       Child Loop BB12_563 Depth 3
                                        #       Child Loop BB12_588 Depth 3
                                        #       Child Loop BB12_649 Depth 3
                                        #       Child Loop BB12_744 Depth 3
                                        #       Child Loop BB12_780 Depth 3
	imulq	$104, %r15, %r9
	movq	rld+64(%r9), %rdi
	testq	%rdi, %rdi
	je	.LBB12_448
# BB#446:                               # %land.lhs.true.i291
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%rdi), %eax
	cmpq	$52, %rax
	ja	.LBB12_448
# BB#447:                               # %if.then27.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	$0, new_spill_reg_store(,%rax,8)
.LBB12_448:                             # %if.end36.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	rld(%r9), %rax
	movq	32(%r14), %r13
	leaq	rld+64(%r9), %rcx
	movq	%rcx, 232(%rsp)         # 8-byte Spill
	leaq	rld(%r9), %rbx
	testq	%rax, %rax
	je	.LBB12_655
# BB#449:                               # %cond.end.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movzbl	(%rax), %eax
	leaq	rld+40(%r9), %rcx
	cmpq	$66, %rax
	movq	%rcx, %rax
	je	.LBB12_451
# BB#450:                               # %cond.end.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rbx, %rax
.LBB12_451:                             # %cond.end.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	(%rax), %r12
	testq	%r12, %r12
	je	.LBB12_655
# BB#452:                               # %land.lhs.true5.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movzbl	reload_inherited(%r15), %eax
	testq	%rax, %rax
	je	.LBB12_455
# BB#453:                               # %lor.lhs.false.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	cmpq	$0, rld+8(%r9)
	je	.LBB12_655
# BB#454:                               # %land.lhs.true8.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	cmpq	$0, rld+48(%r9)
	jne	.LBB12_655
.LBB12_455:                             # %land.lhs.true10.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r12, %rsi
	movq	%rbx, 224(%rsp)         # 8-byte Spill
	movq	%r13, 208(%rsp)         # 8-byte Spill
	movq	%r15, 200(%rsp)         # 8-byte Spill
	movq	%r8, %r15
	movq	%r9, %rbx
	movq	%rcx, %r13
	callq	rtx_equal_p
	movq	%r12, %rdi
	movq	%r13, %rcx
	movq	%rbx, %r9
	movq	%r15, %r8
	movq	200(%rsp), %r15         # 8-byte Reload
	movq	208(%rsp), %r13         # 8-byte Reload
	movq	224(%rsp), %rbx         # 8-byte Reload
	testq	%r8, %rax
	jne	.LBB12_655
# BB#456:                               # %land.lhs.true12.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB12_655
# BB#457:                               # %if.then.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rcx, 152(%rsp)         # 8-byte Spill
	movl	(%rdi), %eax
	movq	32(%r14), %rcx
	movq	%rcx, 184(%rsp)         # 8-byte Spill
	movq	$16, %rcx
	movq	%rax, %r12
	shrq	%cl, %r12
	andq	$255, %r12
	jne	.LBB12_459
# BB#458:                               # %if.then.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	rld+20(%r9), %r12d
.LBB12_459:                             # %if.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r9, 216(%rsp)          # 8-byte Spill
	movl	optimize(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB12_473
# BB#460:                               # %land.lhs.true3.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	je	.LBB12_464
# BB#461:                               # %land.lhs.true3.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	cmpq	$61, %rax
	jne	.LBB12_473
# BB#462:                               # %land.lhs.true10.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%rdi), %eax
	cmpq	$53, %rax
	jb	.LBB12_473
# BB#463:                               # %land.lhs.true12.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	reg_renumber(%rip), %rcx
	movswq	(%rcx,%rax,2), %rax
	testq	%rax, %rax
	jns	.LBB12_473
.LBB12_464:                             # %if.end20.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	$24, %rdx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%r12, (%rsp)
	movq	%rdi, %r14
	movq	%rsi, %r13
	movq	184(%rsp), %rsi         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	callq	find_equiv_reg
	movq	%r13, %rsi
	movq	%r14, %rdi
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB12_473
# BB#465:                               # %if.then22.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rsi, 128(%rsp)         # 8-byte Spill
	movq	%rdi, %r13
	movq	%r14, %rdi
	callq	true_regnum
	movq	(%rbx), %r8
	movq	const_int_rtx+512(%rip), %r9
	movq	216(%rsp), %rsi         # 8-byte Reload
	movl	rld+92(%rsi), %ecx
	movl	rld+72(%rsi), %edx
	movl	rld+28(%rsi), %esi
	movq	%rax, %rbx
	movq	%r15, (%rsp)
	movq	$0, 8(%rsp)
	movq	%rbx, %rdi
	callq	free_for_value_p
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	je	.LBB12_467
# BB#466:                               # %if.then22.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r14, %rcx
.LBB12_467:                             # %if.then22.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	testq	%rcx, %rcx
	movq	%r13, %rdi
	movq	128(%rsp), %rsi         # 8-byte Reload
	je	.LBB12_473
# BB#468:                               # %land.lhs.true31.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rdi, 160(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rbx
	movl	regclass_map(,%rbx,4), %ebx
	movq	216(%rsp), %rax         # 8-byte Reload
	movl	rld+16(%rax), %edx
	cmpq	%rdx, %rbx
	movq	%rbx, %rsi
	je	.LBB12_471
# BB#469:                               # %land.lhs.true36.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	leaq	rld+16(%rax), %r13
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%rbx, 144(%rsp)         # 8-byte Spill
	callq	ix86_register_move_cost
	movq	%rax, %r14
	movq	$32, %rcx
	movl	(%r13), %esi
	movq	%r13, %rbx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	$1, %rdx
	movq	%r12, %rdi
	callq	ix86_memory_move_cost
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r14
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
	jge	.LBB12_473
# BB#470:                               # %land.lhs.true36.lor.lhs.false45_crit_edge.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	(%rbx), %esi
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	216(%rsp), %rax         # 8-byte Reload
	movq	144(%rsp), %rbx         # 8-byte Reload
.LBB12_471:                             # %lor.lhs.false45.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rax, 216(%rsp)         # 8-byte Spill
	movq	$1, %rcx
	movq	%rbx, %rdi
	movq	%r12, %rdx
	callq	ix86_secondary_memory_needed
	testq	%r14, %rax
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
	jne	.LBB12_473
# BB#472:                               #   in Loop: Header=BB12_445 Depth=2
	movq	%rdi, 144(%rsp)         # 8-byte Spill
	movq	168(%rsp), %rdi         # 8-byte Reload
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	jmp	.LBB12_478
.LBB12_473:                             # %land.lhs.true56.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	xorq	%r14, %r14
	cmpq	$0, reload_override_in(,%r15,8)
	jne	.LBB12_475
# BB#474:                               #   in Loop: Header=BB12_445 Depth=2
	movq	%rdi, 144(%rsp)         # 8-byte Spill
	movq	%rdi, %rbx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	jmp	.LBB12_483
.LBB12_475:                             # %land.lhs.true60.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rcx
	movzbl	(%rcx), %eax
	cmpq	$61, %rax
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	je	.LBB12_477
# BB#476:                               # %land.lhs.true60.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rdi, %rcx
.LBB12_477:                             # %land.lhs.true60.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rcx, 144(%rsp)         # 8-byte Spill
	movq	%rcx, %rbx
	jne	.LBB12_483
.LBB12_478:                             # %if.else.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB12_480
# BB#479:                               #   in Loop: Header=BB12_445 Depth=2
	movq	%rdi, %r14
	jmp	.LBB12_482
.LBB12_480:                             # %if.else76.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	xorq	%r14, %r14
	cmpq	$63, %rax
	jne	.LBB12_482
# BB#481:                               # %if.then81.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	8(%rdi), %r14
.LBB12_482:                             # %if.end86.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rdi, %rbx
.LBB12_483:                             # %if.end86.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB12_491
# BB#484:                               # %land.lhs.true88.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	jne	.LBB12_491
# BB#485:                               # %land.lhs.true93.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%rbx), %eax
	cmpq	$52, %rax
	ja	.LBB12_491
# BB#486:                               # %land.lhs.true99.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	cmpq	$0, spill_reg_store(,%rax,8)
	je	.LBB12_491
# BB#487:                               # %land.lhs.true106.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	144(%rsp), %rcx         # 8-byte Reload
	movzbl	(%rcx), %ecx
	cmpq	$61, %rcx
	jne	.LBB12_491
# BB#488:                               # %land.lhs.true111.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rsi, %r13
	movq	spill_reg_stored_to(,%rax,8), %rsi
	movq	184(%rsp), %rdi         # 8-byte Reload
	movq	%r12, 192(%rsp)         # 8-byte Spill
	movq	%r8, %r12
	callq	dead_or_set_p
	movq	%r13, %rsi
	movq	%r12, %r8
	movq	192(%rsp), %r12         # 8-byte Reload
	testq	%r8, %rax
	jne	.LBB12_490
# BB#489:                               # %lor.lhs.false119.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%rbx), %eax
	movq	spill_reg_stored_to(,%rax,8), %rdi
	movq	216(%rsp), %rax         # 8-byte Reload
	movq	%rsi, %r13
	movq	rld+48(%rax), %rsi
	movq	%r8, %r12
	callq	rtx_equal_p
	movq	%r13, %rsi
	movq	%r12, %r8
	movq	192(%rsp), %r12         # 8-byte Reload
	testq	%r8, %rax
	je	.LBB12_491
.LBB12_490:                             # %if.then127.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r12, 192(%rsp)         # 8-byte Spill
	movl	8(%rbx), %edx
	movq	184(%rsp), %rdi         # 8-byte Reload
	movq	%rsi, %r13
	movq	%r15, %rsi
	movq	%r8, %r12
	callq	delete_output_reload
	movq	%r13, %rsi
	movq	%r12, %r8
	movq	192(%rsp), %r12         # 8-byte Reload
.LBB12_491:                             # %if.end131.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r14, 168(%rsp)         # 8-byte Spill
	movzbl	2(%rsi), %eax
	movq	%r12, %rdi
                                        # kill: R12<def> RDI<kill>
	andq	%r8, %r12
	cmpq	%r12, %rax
	jne	.LBB12_493
# BB#492:                               #   in Loop: Header=BB12_445 Depth=2
	movq	%rdi, 192(%rsp)         # 8-byte Spill
	jmp	.LBB12_494
.LBB12_493:                             # %if.then137.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%rsi), %esi
	movq	%rdi, 192(%rsp)         # 8-byte Spill
	movq	%r8, %r14
	callq	gen_rtx_REG
	movq	%r14, %r8
	movq	%rax, %rsi
	jmp	.LBB12_494
.LBB12_496:                             # %while.body.i.i.i
                                        #   in Loop: Header=BB12_494 Depth=3
	movq	8(%rbx), %rbx
.LBB12_494:                             # %while.cond.preheader.i.i.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_445 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB12_497
# BB#495:                               # %land.rhs.i.i.i
                                        #   in Loop: Header=BB12_494 Depth=3
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	cmpq	%r12, %rdx
	jne	.LBB12_496
.LBB12_497:                             # %while.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	je	.LBB12_498
# BB#499:                               # %while.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	andq	%r8, %rax
	cmpq	%rax, %r12
	je	.LBB12_500
# BB#501:                               # %if.then166.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rsi, %r12
	movq	192(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	gen_lowpart_SUBREG
	movq	%rax, %rbx
	jmp	.LBB12_502
.LBB12_498:                             #   in Loop: Header=BB12_445 Depth=2
	movq	%rsi, %r12
	jmp	.LBB12_502
.LBB12_500:                             #   in Loop: Header=BB12_445 Depth=2
	movq	%rsi, %r12
.LBB12_502:                             # %if.end168.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	216(%rsp), %rdx         # 8-byte Reload
	movl	rld+92(%rdx), %eax
	cmpq	$10, %rax
	ja	.LBB12_511
# BB#503:                               # %if.end168.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movabsq	$other_input_reload_insns, %rcx
	jmpq	*.LJTI12_0(,%rax,8)
.LBB12_512:                             # %sw.bb170.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movslq	rld+72(%rdx), %rax
	leaq	input_reload_insns(,%rax,8), %rcx
	jmp	.LBB12_513
.LBB12_504:                             # %sw.bb174.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movslq	rld+72(%rdx), %rax
	leaq	input_address_reload_insns(,%rax,8), %rcx
	jmp	.LBB12_513
.LBB12_505:                             # %sw.bb178.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movslq	rld+72(%rdx), %rax
	leaq	inpaddr_address_reload_insns(,%rax,8), %rcx
	jmp	.LBB12_513
.LBB12_506:                             # %sw.bb182.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movslq	rld+72(%rdx), %rax
	leaq	output_address_reload_insns(,%rax,8), %rcx
	jmp	.LBB12_513
.LBB12_507:                             # %sw.bb186.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movslq	rld+72(%rdx), %rax
	leaq	outaddr_address_reload_insns(,%rax,8), %rcx
	jmp	.LBB12_513
.LBB12_508:                             # %sw.bb190.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movabsq	$operand_reload_insns, %rax
	movq	%rax, %rcx
	jmp	.LBB12_513
.LBB12_509:                             # %sw.bb191.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movabsq	$other_operand_reload_insns, %rax
	movq	%rax, %rcx
	jmp	.LBB12_513
.LBB12_510:                             # %sw.bb192.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movabsq	$other_input_address_reload_insns, %rax
	movq	%rax, %rcx
.LBB12_513:                             # %sw.epilog.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rcx, 160(%rsp)         # 8-byte Spill
	movq	%rbx, 192(%rsp)         # 8-byte Spill
	movq	(%rcx), %rdi
	movq	%rdx, %rbx
	callq	push_to_sequence
	cmpq	$0, rld+8(%rbx)
	leaq	rld+8(%rbx), %r14
	je	.LBB12_553
# BB#514:                               # %land.lhs.true194.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	cmpq	$0, rld+48(%rbx)
	jne	.LBB12_553
# BB#515:                               # %if.then197.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movslq	rld+76(%rbx), %rax
	testq	%rax, %rax
	movq	152(%rsp), %rcx         # 8-byte Reload
	jns	.LBB12_516
# BB#517:                               # %if.end201.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movzbl	reload_inherited(%r15), %eax
	testq	%rax, %rax
	movq	%r12, %r13
	movq	192(%rsp), %r8          # 8-byte Reload
	je	.LBB12_519
# BB#518:                               # %select.mid
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r13, %r8
.LBB12_519:                             # %select.end
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB12_527
# BB#520:                               # %land.lhs.true212.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movzbl	(%r8), %eax
	cmpq	$61, %rax
	jne	.LBB12_527
# BB#521:                               # %land.lhs.true217.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%r8), %eax
	cmpq	$52, %rax
	ja	.LBB12_527
# BB#522:                               # %land.lhs.true223.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	cmpq	$0, spill_reg_store(,%rax,8)
	je	.LBB12_527
# BB#523:                               # %land.lhs.true230.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	(%rcx), %rcx
	movq	8(%rcx), %r12
	movzbl	(%r12), %ecx
	cmpq	$61, %rcx
	jne	.LBB12_527
# BB#524:                               # %land.lhs.true235.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	spill_reg_stored_to(,%rax,8), %rsi
	movq	184(%rsp), %rdi         # 8-byte Reload
	movq	%r8, %rbx
	callq	dead_or_set_p
	movq	%rbx, %r8
	movq	216(%rsp), %rbx         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB12_526
# BB#525:                               # %lor.lhs.false243.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%r8), %eax
	movq	spill_reg_stored_to(,%rax,8), %rdi
	movq	%r12, %rsi
	movq	%r8, %r12
	callq	rtx_equal_p
	movq	%r12, %r8
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB12_527
.LBB12_526:                             # %if.then251.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%r8), %edx
	movq	184(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rsi
	movq	%r8, %r12
	callq	delete_output_reload
	movq	%r12, %r8
.LBB12_527:                             # %if.end255.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r13, 128(%rsp)         # 8-byte Spill
	movq	(%r14), %r14
	movl	rld+36(%rbx), %esi
	movq	8(%r14), %rdx
	movl	(%r14), %r12d
	cmpq	%r14, %r8
	movq	%rdx, %rdi
	je	.LBB12_529
# BB#528:                               # %if.end255.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r8, %rdi
.LBB12_529:                             # %if.end255.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movzbl	(%rdx), %eax
	cmpq	$61, %rax
	movq	%r12, %rax
	jne	.LBB12_531
# BB#530:                               # %if.then.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%rdx), %eax
	movq	reg_last_reload_reg(%rip), %rcx
	movq	$0, (%rcx,%rax,8)
	movl	(%r14), %eax
.LBB12_531:                             # %if.end.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rdi, %r13
	movq	%r8, 192(%rsp)          # 8-byte Spill
	andq	$65534, %r12            # imm = 0xFFFE
	orq	$2, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	movq	%rdx, %rbx
	cmpq	$98, %rax
	jne	.LBB12_533
# BB#532:                               # %if.then20.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	negq	%rsi
.LBB12_533:                             # %if.end21.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rsi, 144(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, 152(%rsp)         # 8-byte Spill
	cmpq	$98, %r12
	movabsq	$0, %rax
	je	.LBB12_535
# BB#534:                               # %if.end21.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movabsq	$1, %rax
.LBB12_535:                             # %if.end21.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	128(%rsp), %rcx         # 8-byte Reload
	movq	%r13, %rdx
	cmpq	%rcx, %rdx
	je	.LBB12_538
# BB#536:                               # %if.end21.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	testq	%rax, %rax
	jne	.LBB12_538
# BB#537:                               # %if.then24.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rdx, %r13
	movq	%r13, %rsi
	callq	gen_move_insn
	movq	%rax, %rdi
	callq	emit_insn
	movq	%r13, %rdx
.LBB12_538:                             # %if.end27.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	192(%rsp), %rax         # 8-byte Reload
	cmpq	%r14, %rax
	movq	%rbx, %r13
	jne	.LBB12_546
# BB#539:                               # %if.then30.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rdx, %rbx
	callq	get_last_insn
	movzbl	2(%r13), %esi
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	$75, %rdi
	movq	%r13, %rdx
	movq	152(%rsp), %rcx         # 8-byte Reload
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rax, %r14
	movslq	40(%r14), %rax
	testq	%rax, %rax
	jns	.LBB12_541
# BB#540:                               # %cond.end47.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r14, %rdi
	callq	recog_memoized_1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB12_545
.LBB12_541:                             # %if.then51.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r14, %rdi
	callq	extract_insn
	movq	$1, %rdi
	callq	constrain_operands
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB12_542
.LBB12_545:                             # %if.end61.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	120(%rsp), %rdi         # 8-byte Reload
	callq	delete_insns_since
	movq	192(%rsp), %rax         # 8-byte Reload
	movq	%rbx, %rdx
.LBB12_546:                             # %if.end62.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rdx, %rsi
	cmpq	$98, %r12
	jne	.LBB12_547
# BB#550:                               # %if.else.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rax, 192(%rsp)         # 8-byte Spill
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	152(%rsp), %rsi         # 8-byte Reload
	callq	gen_add2_insn
	movq	%rax, %rdi
	callq	emit_insn
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	gen_move_insn
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rax, %r14
	movq	144(%rsp), %rsi         # 8-byte Reload
	negq	%rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	gen_add2_insn
	jmp	.LBB12_551
.LBB12_553:                             # %if.else263.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	optimize(%rip), %eax
	leaq	rld+92(%rbx), %r13
	testq	%rax, %rax
	je	.LBB12_599
# BB#554:                               # %land.lhs.true265.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	144(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB12_599
# BB#555:                               # %land.lhs.true270.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	144(%rsp), %rax         # 8-byte Reload
	movl	8(%rax), %eax
	cmpq	$53, %rax
	jb	.LBB12_599
# BB#556:                               # %land.lhs.true276.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	184(%rsp), %rdi         # 8-byte Reload
	movq	144(%rsp), %rsi         # 8-byte Reload
	callq	dead_or_set_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB12_599
# BB#557:                               # %land.lhs.true279.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movslq	n_reloads(%rip), %rax
	testq	%rax, %rax
	movq	$1, %rbx
	jle	.LBB12_562
.LBB12_558:                             # %for.body.i.i.i.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_445 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	reload_override_in-8(,%rbx,8), %rsi
	testq	%rsi, %rsi
	je	.LBB12_561
# BB#559:                               # %land.lhs.true.i.i.i.i
                                        #   in Loop: Header=BB12_558 Depth=3
	movq	%r12, %rdi
	callq	reg_overlap_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB12_599
# BB#560:                               # %land.lhs.true.for.inc_crit_edge.i.i.i.i
                                        #   in Loop: Header=BB12_558 Depth=3
	movl	n_reloads(%rip), %eax
.LBB12_561:                             # %for.inc.i.i.i.i
                                        #   in Loop: Header=BB12_558 Depth=3
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
	cmpq	%rsi, %rdx
	jl	.LBB12_558
.LBB12_562:                             # %land.lhs.true282.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	(%r14), %r9
	movl	(%r13), %ecx
	movl	8(%r12), %edi
	movq	216(%rsp), %rax         # 8-byte Reload
	movl	rld+72(%rax), %edx
	movl	rld+28(%rax), %esi
	movq	%r15, (%rsp)
	movq	$0, 8(%rsp)
	movq	144(%rsp), %r8          # 8-byte Reload
	callq	free_for_value_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	184(%rsp), %rbx         # 8-byte Reload
	je	.LBB12_599
.LBB12_563:                             # %while.cond296.i.i.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_445 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB12_599
# BB#564:                               # %land.rhs298.i.i.i
                                        #   in Loop: Header=BB12_563 Depth=3
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	je	.LBB12_563
# BB#565:                               # %land.rhs298.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	cmpq	$32, %rax
	jne	.LBB12_599
# BB#566:                               # %land.lhs.true315.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$47, %rax
	jne	.LBB12_599
# BB#567:                               # %land.lhs.true323.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	8(%rdi), %rax
	movq	144(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB12_599
# BB#568:                               # %land.lhs.true332.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	callq	asm_noperands
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB12_599
# BB#569:                               # %land.lhs.true339.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movslq	40(%rbx), %rax
	testq	%rax, %rax
	jns	.LBB12_571
# BB#570:                               # %cond.false.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rbx, %rdi
	callq	recog_memoized_1
.LBB12_571:                             # %cond.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rax,%rax,4), %rax
	movq	insn_data+24(,%rax,8), %rax
	movq	8(%rax), %r14
	movq	%r12, %rdi
	callq	true_regnum
	movl	target_flags(%rip), %edx
	movq	%rax, %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	testq	$327680, %rdx           # imm = 0x50000
	movq	%rdx, %rsi
	movabsq	$0, %rax
	movq	%rax, %rdx
	je	.LBB12_573
# BB#572:                               # %cond.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movabsq	$1, %rax
	movq	%rax, %rdx
.LBB12_573:                             # %cond.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	%rsi, %rax
                                        # kill: RSI<def> RAX<kill>
	andq	$262144, %rsi           # imm = 0x40000
	movq	$14, %rcx
	shrq	%cl, %rsi
	movq	%rax, %rdi
	andq	$16384, %rdi            # imm = 0x4000
	movq	$14, %rcx
	shrq	%cl, %rdi
	testq	%rdi, %rdi
	je	.LBB12_575
# BB#574:                               # %cond.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	$17, %rdi
.LBB12_575:                             # %cond.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rax, %r8
	andq	$32, %r8
	movq	$5, %rcx
	shrq	%cl, %r8
	testq	%r8, %r8
	movq	%r8, %r9
	je	.LBB12_577
# BB#576:                               # %cond.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	$15, %r9
.LBB12_577:                             # %cond.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r8, %r10
	je	.LBB12_579
# BB#578:                               # %cond.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	$13, %r10
.LBB12_579:                             # %cond.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	je	.LBB12_581
# BB#580:                               # %cond.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	$14, %r8
.LBB12_581:                             # %cond.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movl	target_flags(%rip), %eax
	movq	%rax, %r11
	andq	$33554432, %r11         # imm = 0x2000000
	movq	reg_class_contents+96(%rip), %rax
	movq	%rax, 128(%rsp)         # 8-byte Spill
	movq	$23, %rcx
	shrq	%cl, %r11
	orq	$8, %r11
	movq	152(%rsp), %rax         # 8-byte Reload
	andq	%rax, 128(%rsp)         # 8-byte Folded Spill
	andq	$1, 120(%rsp)           # 8-byte Folded Spill
	movq	120(%rsp), %rax         # 8-byte Reload
	je	.LBB12_583
# BB#582:                               # %cond.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	$15, %r9
.LBB12_583:                             # %cond.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	je	.LBB12_585
# BB#584:                               # %cond.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	$13, %r10
.LBB12_585:                             # %cond.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	testq	%rax, %rax
	je	.LBB12_587
# BB#586:                               # %cond.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	$14, %r8
.LBB12_587:                             # %cond.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	jmp	.LBB12_588
.LBB12_547:                             # %if.then64.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r13, %r14
	movq	128(%rsp), %rbx         # 8-byte Reload
	cmpq	%rbx, %rax
	movq	%rax, 192(%rsp)         # 8-byte Spill
	movq	208(%rsp), %r13         # 8-byte Reload
	je	.LBB12_549
# BB#548:                               # %if.then67.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rbx, %rdi
	callq	gen_move_insn
	movq	%rax, %rdi
	callq	emit_insn
.LBB12_549:                             # %if.end70.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rbx, %rdi
	movq	152(%rsp), %rsi         # 8-byte Reload
	callq	gen_add2_insn
	movq	%rax, %rdi
	callq	emit_insn
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	gen_move_insn
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rax, %r14
	jmp	.LBB12_552
.LBB12_542:                             # %if.then54.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	cmpq	$98, %r12
	jne	.LBB12_544
# BB#543:                               #   in Loop: Header=BB12_445 Depth=2
	movq	208(%rsp), %r13         # 8-byte Reload
	movq	128(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB12_552
.LBB12_544:                             # %if.then56.i.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	gen_move_insn
.LBB12_551:                             # %inc_for_reload.exit.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rax, %rdi
	callq	emit_insn
	movq	208(%rsp), %r13         # 8-byte Reload
.LBB12_552:                             # %inc_for_reload.exit.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%rbx), %eax
	movq	%r14, new_spill_reg_store(,%rax,8)
	jmp	.LBB12_647
.LBB12_608:                             # %sw.bb3.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	movq	$1, 120(%rsp)           # 8-byte Folded Spill
.LBB12_588:                             # %while.body.i.i.i.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_445 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movsbq	(%r14), %rcx
	incq	%r14
	cmpq	$43, %rcx
	jg	.LBB12_595
# BB#589:                               # %while.body.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	jne	.LBB12_590
	jmp	.LBB12_627
.LBB12_595:                             # %while.body.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	movq	%rcx, 200(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, 200(%rsp)         # 8-byte Folded Spill
	cmpq	$114, 200(%rsp)         # 8-byte Folded Reload
	movq	200(%rsp), %rax         # 8-byte Reload
	je	.LBB12_607
# BB#596:                               # %while.body.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	cmpq	$103, %rax
	jne	.LBB12_597
.LBB12_607:                             # %sw.bb3.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	cmpq	$0, 128(%rsp)           # 8-byte Folded Reload
	je	.LBB12_588
	jmp	.LBB12_608
.LBB12_597:                             # %while.body.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	cmpq	$44, %rax
	jne	.LBB12_590
# BB#598:                               # %sw.bb1.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	120(%rsp), %rcx         # 8-byte Reload
	testq	%rax, %rcx
	movabsq	$0, %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	jne	.LBB12_588
	jmp	.LBB12_599
.LBB12_590:                             # %sw.default.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	movq	%rcx, %rax
	andq	$255, %rax
	movq	$12, 200(%rsp)          # 8-byte Folded Spill
	cmpq	$114, %rax
	je	.LBB12_625
# BB#591:                               # %cond.false.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	movq	$11, 200(%rsp)          # 8-byte Folded Spill
	cmpq	$82, %rax
	je	.LBB12_625
# BB#592:                               # %cond.false.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	cmpq	$113, %rax
	movq	%r11, 200(%rsp)         # 8-byte Spill
	je	.LBB12_625
# BB#593:                               # %cond.false17.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	cmpq	$101, %rcx
	jg	.LBB12_609
# BB#594:                               # %cond.false17.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	movq	$8, 200(%rsp)           # 8-byte Folded Spill
	cmpq	$81, %rax
	jne	.LBB12_612
	jmp	.LBB12_625
.LBB12_609:                             # %cond.false17.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	cmpq	$117, %rax
	movq	%r8, 200(%rsp)          # 8-byte Spill
	je	.LBB12_625
# BB#610:                               # %cond.false17.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	cmpq	$116, %rax
	movq	%r10, 200(%rsp)         # 8-byte Spill
	je	.LBB12_625
# BB#611:                               # %cond.false17.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	cmpq	$102, %rax
	movq	%r9, 200(%rsp)          # 8-byte Spill
	je	.LBB12_625
.LBB12_612:                             # %cond.false52.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	movq	$1, 200(%rsp)           # 8-byte Folded Spill
	cmpq	$97, %rax
	je	.LBB12_625
# BB#613:                               # %cond.false56.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	movq	$4, 200(%rsp)           # 8-byte Folded Spill
	cmpq	$98, %rax
	je	.LBB12_625
# BB#614:                               # %cond.false60.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	movq	$3, 200(%rsp)           # 8-byte Folded Spill
	cmpq	$99, %rax
	je	.LBB12_625
# BB#615:                               # %cond.false64.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	cmpq	$119, %rcx
	jg	.LBB12_618
# BB#616:                               # %cond.false64.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	cmpq	$89, %rax
	movq	%rsi, 200(%rsp)         # 8-byte Spill
	je	.LBB12_625
# BB#617:                               # %cond.false64.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	movq	$2, 200(%rsp)           # 8-byte Folded Spill
	cmpq	$100, %rax
	jne	.LBB12_620
	jmp	.LBB12_625
.LBB12_618:                             # %cond.false64.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	cmpq	$121, %rax
	movq	%rdi, 200(%rsp)         # 8-byte Spill
	je	.LBB12_625
# BB#619:                               # %cond.false64.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	cmpq	$120, %rax
	movq	%rdx, 200(%rsp)         # 8-byte Spill
	je	.LBB12_625
.LBB12_620:                             # %cond.false92.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	movq	$7, 200(%rsp)           # 8-byte Folded Spill
	cmpq	$65, %rax
	je	.LBB12_625
# BB#621:                               # %cond.false96.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	movq	$6, 200(%rsp)           # 8-byte Folded Spill
	cmpq	$68, %rax
	je	.LBB12_625
# BB#622:                               # %cond.false100.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	cmpq	$83, %rax
	movabsq	$0, %rax
	jne	.LBB12_624
# BB#623:                               # %cond.false100.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	movabsq	$1, %rax
.LBB12_624:                             # %cond.false100.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	leaq	(%rax,%rax,4), %rax
	movq	%rax, 200(%rsp)         # 8-byte Spill
.LBB12_625:                             # %cond.end133.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	movq	200(%rsp), %rax         # 8-byte Reload
	movq	reg_class_contents(,%rax,8), %rax
	movq	152(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rax
	je	.LBB12_588
# BB#626:                               # %cond.end133.i.i.i.i
                                        #   in Loop: Header=BB12_588 Depth=3
	movq	$1, 120(%rsp)           # 8-byte Folded Spill
	jmp	.LBB12_588
.LBB12_627:                             # %constraint_accepts_reg_p.exit.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	120(%rsp), %rcx         # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB12_599
# BB#628:                               # %land.lhs.true353.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rax
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	144(%rsp), %rsi         # 8-byte Reload
	callq	count_occurrences
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB12_599
# BB#629:                               # %land.lhs.true360.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	32(%rbx), %rax
	movq	16(%rax), %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB12_599
# BB#630:                               # %if.then369.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r15, %r14
	movq	32(%rbx), %rax
	movq	%r12, 8(%rax)
	movq	32(%rbx), %rax
	movq	16(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB12_633
# BB#631:                               # %land.lhs.true386.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%rax), %eax
	cmpq	$52, %rax
	ja	.LBB12_633
# BB#632:                               # %if.then398.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	$0, spill_reg_store(,%rax,8)
	movq	32(%rbx), %rax
	movq	16(%rax), %rax
	movl	8(%rax), %eax
	movq	$0, spill_reg_stored_to(,%rax,8)
.LBB12_633:                             # %if.end421.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	144(%rsp), %rax         # 8-byte Reload
	movl	8(%rax), %eax
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rcx
	movl	24(%rcx), %edx
	cmpq	$1, %rdx
	jne	.LBB12_634
# BB#635:                               # %land.lhs.true429.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	12(%rcx), %ecx
	cmpq	$1, %rcx
	jne	.LBB12_634
# BB#636:                               # %if.then439.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	232(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rcx
	movzbl	8(%rcx), %ecx
	movq	reg_renumber(%rip), %rdx
	movw	%cx, (%rdx,%rax,2)
	movq	%rax, %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, %rsi
	callq	alter_reg
	movq	%r14, %r15
	jmp	.LBB12_646
.LBB12_599:                             # %land.lhs.true458.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r12, %rdi
	movq	192(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	jne	.LBB12_646
# BB#600:                               # %if.then461.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movzbl	(%rbx), %ecx
	cmpq	$133, %rcx
	movq	%rbx, %rdi
	jg	.LBB12_637
# BB#601:                               # %if.then461.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	addq	$-54, %rcx
	andq	%rdx, %rcx
	cmpq	$14, %rcx
	movq	%rdi, %rsi
	ja	.LBB12_645
# BB#602:                               # %if.then461.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB12_639
# BB#603:                               # %if.then461.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	cmpq	$7, %rcx
	jne	.LBB12_604
# BB#640:                               # %land.lhs.true466.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%rdi), %eax
	jmp	.LBB12_641
.LBB12_637:                             # %if.then461.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	cmpq	$134, %rcx
	je	.LBB12_639
# BB#638:                               # %if.then461.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	cmpq	$140, %rcx
	movq	%rdi, %rsi
	jne	.LBB12_645
.LBB12_639:                             # %land.lhs.true572.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%r12), %eax
	movl	regclass_map(,%rax,4), %esi
	movq	%rdx, %r14
	movq	%rdi, %rbx
	callq	ix86_preferred_reload_class
	movq	%rbx, %rdi
	movq	%r14, %rbx
	testq	%rbx, %rax
	movq	%rdi, %rsi
	je	.LBB12_644
	jmp	.LBB12_645
.LBB12_604:                             # %if.then461.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	cmpq	$9, %rcx
	movq	%rdi, %rsi
	jne	.LBB12_645
# BB#605:                               # %land.lhs.true493.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	8(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	movq	%rdi, %rsi
	jne	.LBB12_645
# BB#606:                               # %land.lhs.true501.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%rax), %eax
.LBB12_641:                             # %land.lhs.true466.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	cmpq	$53, %rax
	movq	%rdi, %rsi
	jb	.LBB12_645
# BB#642:                               # %land.lhs.true472.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	reg_equiv_memory_loc(%rip), %rcx
	cmpq	$0, (%rcx,%rax,8)
	jne	.LBB12_644
# BB#643:                               # %lor.lhs.false480.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	reg_equiv_constant(%rip), %rcx
	cmpq	$0, (%rcx,%rax,8)
	movq	%rdi, %rsi
	je	.LBB12_645
.LBB12_644:                             # %if.then581.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rsi
.LBB12_645:                             # %if.end583.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rdi, 192(%rsp)         # 8-byte Spill
	movl	(%r13), %ecx
	movq	216(%rsp), %rax         # 8-byte Reload
	movl	rld+72(%rax), %edx
	movq	%r12, %rdi
	callq	gen_reload
.LBB12_646:                             # %if.end587.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	208(%rsp), %r13         # 8-byte Reload
.LBB12_647:                             # %if.end587.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	flag_non_call_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB12_653
# BB#648:                               # %if.then589.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	callq	get_insns
	movq	%rax, %rbx
	movq	$23, %rsi
	xorq	%rdx, %rdx
	movq	184(%rsp), %rdi         # 8-byte Reload
	callq	find_reg_note
	movq	%rax, %r14
	testq	%r14, %r14
	jne	.LBB12_649
	jmp	.LBB12_653
.LBB12_652:                             # %for.inc.i766.i.i.i
                                        #   in Loop: Header=BB12_649 Depth=3
	movq	24(%rbx), %rbx
.LBB12_649:                             # %if.then589.i.i.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_445 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB12_653
# BB#650:                               # %for.body.i765.i.i.i
                                        #   in Loop: Header=BB12_649 Depth=3
	movq	32(%rbx), %rdi
	callq	may_trap_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB12_652
# BB#651:                               # %if.then3.i.i.i.i
                                        #   in Loop: Header=BB12_649 Depth=3
	movq	8(%r14), %rdx
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$23, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
	jmp	.LBB12_652
.LBB12_653:                             # %if.end591.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	callq	get_insns
	movq	160(%rsp), %rcx         # 8-byte Reload
	movq	%rax, (%rcx)
	callq	end_sequence
	cmpq	$0, 168(%rsp)           # 8-byte Folded Reload
	movq	136(%rsp), %r14         # 8-byte Reload
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	216(%rsp), %r9          # 8-byte Reload
	movq	224(%rsp), %rbx         # 8-byte Reload
	je	.LBB12_655
# BB#654:                               # %if.then594.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	192(%rsp), %rax         # 8-byte Reload
	movq	%rax, reload_override_in(,%r15,8)
	.align	16, 0x90
.LBB12_655:                             # %if.end.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB12_672
# BB#656:                               # %land.lhs.true16.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movzbl	reload_inherited(%r15), %ecx
	testq	%rcx, %rcx
	je	.LBB12_665
# BB#657:                               # %land.lhs.true20.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB12_663
# BB#658:                               # %land.lhs.true23.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movzbl	(%rsi), %ecx
	cmpq	$66, %rcx
	jne	.LBB12_663
# BB#659:                               # %land.lhs.true29.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	rld+40(%r9), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$66, %rcx
	jne	.LBB12_663
# BB#660:                               # %land.lhs.true35.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movslq	reload_spill_index(,%r15,4), %rcx
	testq	%rcx, %rcx
	js	.LBB12_663
# BB#661:                               # %land.lhs.true40.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rbx, 224(%rsp)         # 8-byte Spill
	movq	reg_reloaded_valid(%rip), %rdx
	andq	%r8, %rcx
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%rdi, %rdx
	je	.LBB12_663
# BB#662:                               # %if.then44.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	32(%r13), %rdi
	xorq	%rdx, %rdx
	movq	%r8, %rbx
	movq	%r15, %r12
	movq	%r9, %r15
	callq	count_occurrences
	movq	%r15, %r9
	movq	%r12, %r15
	movq	%rbx, %r8
	movq	cfun(%rip), %rax
	movslq	reload_spill_index(,%r15,4), %rcx
	movq	24(%rax), %rax
	movslq	reg_reloaded_contents(,%rcx,4), %rcx
	movq	80(%rax), %rax
	movq	(%rax,%rcx,8), %rcx
	movl	optimize(%rip), %eax
	movq	224(%rsp), %rdx         # 8-byte Reload
	movq	%rcx, (%rdx)
	.align	16, 0x90
.LBB12_663:                             # %if.end58.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	testq	%r8, %rax
	je	.LBB12_672
# BB#664:                               # %land.lhs.true60.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movzbl	reload_inherited(%r15), %eax
	testq	%rax, %rax
	jne	.LBB12_666
	.align	16, 0x90
.LBB12_665:                             # %lor.lhs.false65.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	cmpq	$0, reload_override_in(,%r15,8)
	je	.LBB12_672
.LBB12_666:                             # %land.lhs.true69.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB12_672
# BB#667:                               # %land.lhs.true72.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB12_672
# BB#668:                               # %land.lhs.true78.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%rax), %eax
	cmpq	$0, spill_reg_store(,%rax,8)
	je	.LBB12_672
# BB#669:                               # %land.lhs.true86.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	spill_reg_stored_to(,%rax,8), %rsi
	movq	%r13, %rdi
	movq	%r15, %r12
	movq	%r8, %r15
	movq	%r9, %rbx
	callq	dead_or_set_p
	movq	%rbx, %r9
	movq	%r15, %r8
	movq	%r12, %r15
	movq	%r13, %rcx
	testq	%r8, %rax
	jne	.LBB12_671
# BB#670:                               # %lor.lhs.false95.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movl	8(%rax), %eax
	movq	spill_reg_stored_to(,%rax,8), %rdi
	movq	rld+48(%r9), %rsi
	movq	%rcx, %r13
	movq	%r15, %r12
	movq	%r8, %r15
	movq	%r9, %rbx
	callq	rtx_equal_p
	movq	%rbx, %r9
	movq	%r15, %r8
	movq	%r12, %r15
	movq	%r13, %rcx
	testq	%r8, %rax
	je	.LBB12_672
.LBB12_671:                             # %if.then105.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movl	8(%rax), %edx
	movq	%rcx, %rdi
	movq	%r15, %rsi
	movq	%r15, %r12
	movq	%r8, %r15
	movq	%r9, %rbx
	callq	delete_output_reload
	movq	%rbx, %r9
	movq	%r15, %r8
	movq	%r12, %r15
	.align	16, 0x90
.LBB12_672:                             # %do_input_reload.exit.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	rld+48(%r9), %rbx
	movq	32(%r14), %rax
	movq	%rax, 224(%rsp)         # 8-byte Spill
	leaq	rld+48(%r9), %r10
	testq	%rbx, %rbx
	je	.LBB12_684
# BB#673:                               # %do_input_reload.exit.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	optimize(%rip), %eax
	testq	%r8, %rax
	je	.LBB12_684
# BB#674:                               # %land.lhs.true3.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	jne	.LBB12_683
# BB#675:                               # %land.lhs.true4.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	rld+40(%r9), %rdi
	movq	%rbx, %rsi
	movq	%r8, %r12
	movq	%r15, %r14
	movq	%r9, %r15
	movq	%r10, %r13
	callq	rtx_equal_p
	movq	%r13, %r10
	movq	%r15, %r9
	movq	%r14, %r15
	movq	%r12, %r8
	testq	%r8, %rax
	jne	.LBB12_683
# BB#676:                               # %land.lhs.true6.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%rbx), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$53, %rax
	jb	.LBB12_683
# BB#677:                               # %land.lhs.true8.i1623.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	reg_last_reload_reg(%rip), %rcx
	cmpq	$0, (%rcx,%rax,8)
	je	.LBB12_683
# BB#678:                               # %if.then.i1630.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	(%rcx,%rdx,8), %rcx
	movl	8(%rcx), %r14d
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movq	reg_reloaded_valid(%rip), %rsi
	sarq	%cl, %rdx
	movq	$1, %rdi
	movq	%r14, %rcx
	shlq	%cl, %rdi
	testq	%rsi, %rdi
	je	.LBB12_683
# BB#679:                               # %land.lhs.true23.i1631.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	reg_reloaded_contents(,%rdx,4), %ecx
	cmpq	%rax, %rcx
	jne	.LBB12_683
# BB#680:                               # %land.lhs.true27.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	cmpq	$0, spill_reg_store(,%rdx,8)
	je	.LBB12_683
# BB#681:                               # %land.lhs.true31.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	spill_reg_stored_to(,%rdx,8), %rsi
	movq	%rbx, %rdi
	movq	%r15, %r12
	movq	%r8, %r15
	movq	%r9, %rbx
	movq	%r10, %r13
	callq	rtx_equal_p
	movq	%r13, %r10
	movq	%rbx, %r9
	movq	%r15, %r8
	movq	%r12, %r15
	testq	%r8, %rax
	je	.LBB12_683
# BB#682:                               # %if.then36.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rsi
	movq	%r14, %rdx
	movq	%r15, %r12
	movq	%r8, %r15
	movq	%r9, %rbx
	movq	%r10, %r13
	callq	delete_output_reload
	movq	%r13, %r10
	movq	%rbx, %r9
	movq	%r15, %r8
	movq	%r12, %r15
	.align	16, 0x90
.LBB12_683:                             # %if.end37thread-pre-split.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	(%r10), %rbx
	movq	136(%rsp), %r14         # 8-byte Reload
.LBB12_684:                             # %if.end37.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	testq	%rbx, %rbx
	je	.LBB12_785
# BB#685:                               # %lor.lhs.false.i1634.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	cmpq	%rbx, %rax
	je	.LBB12_785
# BB#686:                               # %lor.lhs.false.i1634.i
                                        #   in Loop: Header=BB12_445 Depth=2
	testq	%rax, %rax
	je	.LBB12_785
# BB#687:                               # %if.end45.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	(%rbx), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	addq	$-61, %rax
	andq	%r8, %rax
	cmpq	$1, %rax
	ja	.LBB12_690
# BB#688:                               # %land.lhs.true53.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r10, %r13
	movq	%r9, 216(%rsp)          # 8-byte Spill
	movq	%r8, %r12
	movq	$10, %rsi
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB12_990
# BB#689:                               # %land.lhs.true53.if.else_crit_edge.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	(%rbx), %edi
	movq	%r12, %r8
	movq	216(%rsp), %r9          # 8-byte Reload
	movq	%r13, %r10
.LBB12_690:                             # %if.else.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB12_691
# BB#692:                               # %land.lhs.true63.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	8(%rbx), %rdx
	movzbl	(%rdx), %eax
	cmpq	$61, %rax
	movq	%r9, %rcx
	jne	.LBB12_695
# BB#693:                               # %land.lhs.true70.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r10, %r13
	movq	%r15, %r12
	movq	%rcx, %r15
	movq	$10, %rsi
	movq	224(%rsp), %rdi         # 8-byte Reload
	callq	find_reg_note
	movl	(%rbx), %edi
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB12_991
# BB#694:                               #   in Loop: Header=BB12_445 Depth=2
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%r15, %rcx
	movq	%r12, %r15
	movq	%r13, %r10
	jmp	.LBB12_695
.LBB12_691:                             #   in Loop: Header=BB12_445 Depth=2
	movq	%r9, %rcx
.LBB12_695:                             # %if.else84.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$62, %rdi
	je	.LBB12_785
# BB#696:                               # %if.end91.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	224(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$33, %rax
	je	.LBB12_697
# BB#698:                               # %if.end96.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%r10, 184(%rsp)         # 8-byte Spill
	movq	%r8, %r12
	movq	rld+8(%rcx), %rbx
	movq	32(%r14), %rax
	movq	%rax, 224(%rsp)         # 8-byte Spill
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r13
	movl	rld+92(%rcx), %eax
	movzbl	2(%rbx), %r14d
	leaq	rld+8(%rcx), %rdx
	movq	%rdx, 168(%rsp)         # 8-byte Spill
	leaq	rld+92(%rcx), %rdx
	movq	%rdx, 192(%rsp)         # 8-byte Spill
	cmpq	$9, %rax
	jne	.LBB12_700
# BB#699:                               # %if.then.i.i1643.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rcx, 216(%rsp)         # 8-byte Spill
	callq	start_sequence
	jmp	.LBB12_701
.LBB12_990:                             # %if.then56.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	232(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rcx
	movq	%rcx, 8(%rax)
	movq	%r12, %r8
	jmp	.LBB12_785
.LBB12_700:                             # %if.else.i.i1647.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movslq	rld+72(%rcx), %rax
	movq	%rcx, 216(%rsp)         # 8-byte Spill
	movq	output_reload_insns(,%rax,8), %rdi
	callq	push_to_sequence
.LBB12_701:                             # %if.end.i.i1648.i
                                        #   in Loop: Header=BB12_445 Depth=2
	testq	%r14, %r14
	movq	%rbx, %rdi
	jne	.LBB12_705
# BB#702:                               # %if.then3.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	224(%rsp), %rbx         # 8-byte Reload
	movq	32(%rbx), %rdi
	callq	asm_noperands
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB12_703
# BB#704:                               # %if.end7.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str7, %rsi
	callq	error_for_asm
	movl	word_mode(%rip), %r14d
	movl	8(%r13), %esi
	movq	%r14, %rdi
	callq	gen_rtx_REG
	movq	%rax, %rdi
.LBB12_705:                             # %if.end11.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movzbl	2(%r13), %eax
	movq	%r13, %rcx
	movq	%r14, %r13
	movq	%r12, %rbx
	andq	%rbx, %r13
	cmpq	%r13, %rax
	jne	.LBB12_707
# BB#706:                               #   in Loop: Header=BB12_445 Depth=2
	movq	%rcx, %r14
	jmp	.LBB12_708
.LBB12_707:                             # %if.then16.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%rcx), %esi
	movq	%rdi, %r12
	movq	%r14, %rdi
	callq	gen_rtx_REG
	movq	%r12, %rdi
	movq	%rax, %r14
.LBB12_708:                             # %if.end21.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	216(%rsp), %r8          # 8-byte Reload
	movslq	rld+80(%r8), %rax
	leaq	rld+80(%r8), %rcx
	movq	%rcx, 208(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	js	.LBB12_709
# BB#710:                               # %if.then23.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movzbl	(%rdi), %ecx
	cmpq	$61, %rcx
	movq	%rdi, %rdx
	jne	.LBB12_714
# BB#711:                               # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%rdi), %ecx
	cmpq	$53, %rcx
	movq	%rdi, %rdx
	jb	.LBB12_714
# BB#712:                               # %land.lhs.true31.i.i1653.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	reg_equiv_mem(%rip), %rdx
	movq	(%rdx,%rcx,8), %rcx
	testq	%rcx, %rcx
	movq	%rdi, %rdx
	je	.LBB12_714
# BB#713:                               # %land.lhs.true31.i.i1653.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rcx, %rdx
.LBB12_714:                             # %if.end44.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	rld+16(%r8), %ecx
	addq	$-10, %rcx
	andq	%rbx, %rcx
	cmpq	$2, %rcx
	ja	.LBB12_709
# BB#715:                               # %land.lhs.true51.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rdx, 152(%rsp)         # 8-byte Spill
	movl	target_flags(%rip), %ecx
	testq	$33554432, %rcx         # imm = 0x2000000
	jne	.LBB12_709
# BB#716:                               # %land.lhs.true51.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rdi, 160(%rsp)         # 8-byte Spill
	cmpq	$2, %r13
	jne	.LBB12_717
# BB#718:                               # %if.then54.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	imulq	$104, %rax, %r13
	movslq	rld+88(%r8), %rax
	movq	rld+64(%r13), %r9
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB12_719
# BB#721:                               # %if.else66.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	(%r9), %eax
	movslq	rld+88(%r13), %rcx
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$131072, %rax           # imm = 0x20000
	je	.LBB12_723
# BB#722:                               # %if.then75.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	8(%r9), %esi
	movq	$2, %rdi
	movq	%r15, 200(%rsp)         # 8-byte Spill
	movq	%r8, %r15
	movq	%rcx, %r12
	callq	gen_rtx_REG
	movq	%r12, %rcx
	movq	%r15, %r8
	movq	200(%rsp), %r15         # 8-byte Reload
	movq	%rax, %r9
.LBB12_723:                             # %if.end80.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rbx, %rdx
	movq	152(%rsp), %rsi         # 8-byte Reload
	movq	%rcx, %rax
	andq	%rdx, %rax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB12_724
# BB#731:                               # %if.else132.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rdx, %r13
	movq	192(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %ecx
	movl	rld+72(%r8), %edx
	movq	%r8, %r12
	movq	%r9, %rdi
	movq	%r14, %rsi
	movq	%r9, %rbx
	callq	gen_reload
	jmp	.LBB12_732
.LBB12_709:                             #   in Loop: Header=BB12_445 Depth=2
	movq	%r8, %r12
	movq	%rbx, %r13
	movq	%r14, %rbx
.LBB12_733:                             # %if.then141.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	flag_expensive_optimizations(%rip), %eax
	testq	%rax, %rax
	je	.LBB12_742
# BB#734:                               # %lor.lhs.false143.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB12_742
# BB#735:                               # %lor.lhs.false148.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	224(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB12_742
# BB#736:                               # %cond.true.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	224(%rsp), %rcx         # 8-byte Reload
	movq	32(%rcx), %r14
	movzbl	(%r14), %eax
	cmpq	$47, %rax
	je	.LBB12_738
# BB#737:                               # %cond.false.i.i1660.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rdi, 160(%rsp)         # 8-byte Spill
	movq	%rcx, %rdi
	movq	%r14, %rsi
	callq	single_set_2
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %r14
.LBB12_738:                             # %cond.end173.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	testq	%r14, %r14
	je	.LBB12_742
# BB#739:                               # %lor.lhs.false176.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	8(%r14), %rsi
	movq	%rdi, 160(%rsp)         # 8-byte Spill
	movq	160(%rsp), %rdi         # 8-byte Reload
	callq	rtx_equal_p
	movq	160(%rsp), %rdi         # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB12_742
# BB#740:                               # %lor.lhs.false182.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	16(%r14), %rsi
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	reg_mentioned_p
	movq	%r14, %rdi
	testq	%r13, %rax
	je	.LBB12_742
# BB#741:                               # %lor.lhs.false188.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	%rdi, %r14
	movl	8(%r14), %edi
	movl	rld+28(%r12), %edx
	xorq	%rcx, %rcx
	movq	224(%rsp), %rsi         # 8-byte Reload
	callq	regno_clobbered_p
	movq	%r14, %rdi
	testq	%r13, %rax
	jne	.LBB12_743
.LBB12_742:                             # %if.then195.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	192(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %ecx
	movl	rld+72(%r12), %edx
	movq	%rbx, %rsi
	callq	gen_reload
.LBB12_743:                             # %if.end200.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	callq	get_insns
	movq	%rax, %r14
	jmp	.LBB12_744
	.align	16, 0x90
.LBB12_774:                             # %for.inc.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movq	24(%r14), %r14
.LBB12_744:                             # %if.end200.i.i.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_445 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r14, %r14
	je	.LBB12_775
# BB#745:                               # %for.body.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB12_774
# BB#746:                               # %if.then210.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movq	32(%r14), %rbx
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movabsq	$forget_old_reloads_1, %rsi
	callq	note_stores
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rdi
	movq	%rbx, %rsi
	callq	reg_mentioned_p
	testq	%r13, %rax
	je	.LBB12_774
# BB#747:                               # %if.then217.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movq	224(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB12_755
# BB#748:                               # %cond.true226.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movq	224(%rsp), %rdx         # 8-byte Reload
	movq	32(%rdx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB12_750
# BB#749:                               # %cond.false238.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB12_750:                             # %cond.end246.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movslq	reload_spill_index(,%r15,4), %rcx
	testq	%rcx, %rcx
	jns	.LBB12_755
# BB#751:                               # %cond.end246.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	testq	%rax, %rax
	je	.LBB12_755
# BB#752:                               # %land.lhs.true254.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movq	16(%rax), %rax
	movq	232(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB12_755
# BB#753:                               # %if.then261.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movl	8(%rax), %ecx
	movl	%ecx, reload_spill_index(,%r15,4)
	movq	$1, %rbx
	shlq	%cl, %rbx
	orq	%rbx, reg_is_output_reload(%rip)
	movq	$1, %rsi
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	%rcx, %rdx
	callq	find_regno_note
	testq	%rax, %rax
	je	.LBB12_755
# BB#754:                               # %if.then272.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	orq	%rbx, reg_reloaded_died(%rip)
.LBB12_755:                             # %if.end277.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movl	8(%rax), %eax
	cmpq	$52, %rax
	ja	.LBB12_774
# BB#756:                               # %if.then284.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %ecx
	movq	208(%rsp), %rax         # 8-byte Reload
	movslq	(%rax), %rbx
	xorq	%rax, %rax
	cmpq	$105, %rcx
	jne	.LBB12_759
# BB#757:                               # %cond.true293.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB12_759
# BB#758:                               # %cond.false305.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB12_759:                             # %cond.end313.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	testq	%rbx, %rbx
	js	.LBB12_761
# BB#760:                               # %cond.end313.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	testq	%rax, %rax
	je	.LBB12_774
.LBB12_761:                             # %if.else321.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	testq	%rbx, %rbx
	js	.LBB12_762
# BB#764:                               # %land.lhs.true324.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movq	16(%rax), %rdx
	movq	232(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rcx
	cmpq	%rcx, %rdx
	jne	.LBB12_763
# BB#765:                               # %land.lhs.true331.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movq	8(%rax), %rsi
	imulq	$104, %rbx, %rax
	movq	rld+64(%rax), %rcx
	cmpq	%rcx, %rsi
	movq	%rdx, %rcx
	jne	.LBB12_763
# BB#766:                               # %if.then340.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rcx
	movq	24(%r14), %rbx
	movq	%rcx, rld+8(%rax)
	movq	184(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rcx
	movq	%rcx, rld+48(%rax)
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB12_774
# BB#767:                               # %cond.true361.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB12_769
# BB#768:                               #   in Loop: Header=BB12_744 Depth=3
	movq	%rsi, 200(%rsp)         # 8-byte Spill
	jmp	.LBB12_770
.LBB12_762:                             # %if.else321.if.else410_crit_edge.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rcx
.LBB12_763:                             # %if.else410.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movl	8(%rcx), %eax
	movq	%r14, new_spill_reg_store(,%rax,8)
	movq	24(%r14), %r14
	jmp	.LBB12_744
.LBB12_769:                             # %cond.false373.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movq	%rsi, 200(%rsp)         # 8-byte Spill
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB12_770:                             # %cond.end381.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	testq	%rax, %rax
	movq	200(%rsp), %rcx         # 8-byte Reload
	je	.LBB12_774
# BB#771:                               # %land.lhs.true384.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movq	16(%rax), %rax
	cmpq	%rcx, %rax
	jne	.LBB12_774
# BB#772:                               # %land.lhs.true390.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movl	8(%rcx), %ecx
	cmpq	$0, new_spill_reg_store(,%rcx,8)
	jne	.LBB12_774
# BB#773:                               # %if.then397.i.i.i
                                        #   in Loop: Header=BB12_744 Depth=3
	movq	$1, %rax
	shlq	%cl, %rax
	orq	%rax, reg_is_output_reload(%rip)
	movq	%rbx, new_spill_reg_store(,%rcx,8)
	jmp	.LBB12_774
.LBB12_775:                             # %for.end.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	192(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %eax
	cmpq	$9, %rax
	jne	.LBB12_777
# BB#776:                               # %if.then428.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movslq	rld+72(%r12), %rax
	movq	other_output_reload_insns(,%rax,8), %rdi
	callq	emit_insns
	callq	get_insns
	movslq	rld+72(%r12), %rcx
	movq	%rax, other_output_reload_insns(,%rcx,8)
	jmp	.LBB12_778
.LBB12_777:                             # %if.else437.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	callq	get_insns
	movslq	rld+72(%r12), %rcx
	movq	%rax, output_reload_insns(,%rcx,8)
.LBB12_778:                             # %if.end442.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movl	flag_non_call_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB12_784
# BB#779:                               # %if.then444.i.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	callq	get_insns
	movq	%rax, %rbx
	movq	$23, %rsi
	xorq	%rdx, %rdx
	movq	224(%rsp), %rdi         # 8-byte Reload
	callq	find_reg_note
	movq	%rax, %r14
	testq	%r14, %r14
	jne	.LBB12_780
	jmp	.LBB12_784
	.align	16, 0x90
.LBB12_783:                             # %for.inc.i.i.i1686.i
                                        #   in Loop: Header=BB12_780 Depth=3
	movq	24(%rbx), %rbx
.LBB12_780:                             # %if.then444.i.i.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_445 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB12_784
# BB#781:                               # %for.body.i.i.i1677.i
                                        #   in Loop: Header=BB12_780 Depth=3
	movq	32(%rbx), %rdi
	callq	may_trap_p
	testq	%r13, %rax
	je	.LBB12_783
# BB#782:                               # %if.then3.i.i.i1682.i
                                        #   in Loop: Header=BB12_780 Depth=3
	movq	8(%r14), %rdx
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$23, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
	jmp	.LBB12_783
.LBB12_784:                             # %emit_output_reload_insns.exit.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	callq	end_sequence
	movq	136(%rsp), %r14         # 8-byte Reload
	movq	%r13, %r8
	jmp	.LBB12_785
.LBB12_991:                             # %if.then76.i.i
                                        #   in Loop: Header=BB12_445 Depth=2
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rsi
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	callq	gen_lowpart_common
	movq	%rax, 8(%rbx)
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%r12, %r15
	.align	16, 0x90
.LBB12_785:                             # %do_output_reload.exit.i
                                        #   in Loop: Header=BB12_445 Depth=2
	incq	%r15
	movslq	n_reloads(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB12_445
.LBB12_786:                             # %for.end41.i
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	other_input_address_reload_insns(%rip), %rdi
	movq	176(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rsi
	callq	emit_insns_before
	movq	other_input_reload_insns(%rip), %rdi
	movq	%r13, %rsi
	callq	emit_insns_before
	movslq	reload_n_operands(%rip), %rax
	testq	%rax, %rax
	movq	$1, %rbx
	jle	.LBB12_788
	.align	16, 0x90
.LBB12_787:                             # %for.body46.i
                                        #   Parent Loop BB12_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	inpaddr_address_reload_insns-8(,%rbx,8), %rdi
	movq	%r13, %rsi
	callq	emit_insns_before
	movq	input_address_reload_insns-8(,%rbx,8), %rdi
	movq	%r13, %rsi
	callq	emit_insns_before
	movq	input_reload_insns-8(,%rbx,8), %rdi
	movq	%r13, %rsi
	callq	emit_insns_before
	movslq	reload_n_operands(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB12_787
.LBB12_788:                             # %for.end58.i
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	other_operand_reload_insns(%rip), %rdi
	movq	%r13, %rsi
	callq	emit_insns_before
	movq	operand_reload_insns(%rip), %rdi
	movq	%r13, %rsi
	callq	emit_insns_before
	movslq	reload_n_operands(%rip), %rax
	testq	%rax, %rax
	movq	$1, %rbx
	movq	80(%rsp), %r12          # 8-byte Reload
	movq	%r13, %r15
	movq	72(%rsp), %r13          # 8-byte Reload
	jle	.LBB12_789
	.align	16, 0x90
.LBB12_992:                             # %for.body63.i
                                        #   Parent Loop BB12_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	outaddr_address_reload_insns-8(,%rbx,8), %rdi
	movq	%r15, %rsi
	callq	emit_insns_after
	movq	output_address_reload_insns-8(,%rbx,8), %rdi
	movq	%rax, %rsi
	callq	emit_insns_after
	movq	output_reload_insns-8(,%rbx,8), %rdi
	movq	%rax, %rsi
	callq	emit_insns_after
	movq	other_output_reload_insns-8(,%rbx,8), %rdi
	movq	%rax, %rsi
	callq	emit_insns_after
	movslq	reload_n_operands(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB12_992
.LBB12_789:                             # %for.cond79.preheader.i
                                        #   in Loop: Header=BB12_4 Depth=1
	movslq	n_reloads(%rip), %rax
	testq	%rax, %rax
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	jle	.LBB12_976
# BB#790:                               # %for.body81.lr.ph.i
                                        #   in Loop: Header=BB12_4 Depth=1
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB12_791:                             # %for.body81.i
                                        #   Parent Loop BB12_4 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB12_818 Depth 3
                                        #       Child Loop BB12_825 Depth 3
                                        #       Child Loop BB12_866 Depth 3
                                        #       Child Loop BB12_874 Depth 3
                                        #       Child Loop BB12_890 Depth 3
                                        #       Child Loop BB12_910 Depth 3
                                        #       Child Loop BB12_923 Depth 3
                                        #       Child Loop BB12_925 Depth 3
                                        #       Child Loop BB12_927 Depth 3
                                        #       Child Loop BB12_974 Depth 3
                                        #       Child Loop BB12_932 Depth 3
                                        #       Child Loop BB12_955 Depth 3
                                        #         Child Loop BB12_956 Depth 4
	movswq	reload_order(%rbx,%rbx), %rsi
	imulq	$104, %rsi, %r8
	movq	rld+40(%r8), %rax
	movslq	reload_spill_index(,%rsi,4), %r12
	movq	%r12, 200(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	je	.LBB12_802
# BB#792:                               # %land.lhs.true90.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movzbl	reload_inherited(%rsi), %ecx
	testq	%rcx, %rcx
	jne	.LBB12_802
# BB#793:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB12_791 Depth=2
	cmpq	$0, reload_override_in(,%rsi,8)
	jne	.LBB12_802
# BB#794:                               # %if.then98.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$63, %rdx
	jne	.LBB12_796
# BB#795:                               # %if.then104.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	8(%rax), %rax
	movl	(%rax), %ecx
.LBB12_796:                             # %if.end107.i
                                        #   in Loop: Header=BB12_791 Depth=2
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB12_802
# BB#797:                               # %land.lhs.true112.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	8(%rax), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$53, %rax
	jb	.LBB12_802
# BB#798:                               # %land.lhs.true118.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	reg_has_output_reload(%rip), %rcx
	movzbl	(%rcx,%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB12_802
# BB#799:                               # %if.then125.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	reg_last_reload_reg(%rip), %rcx
	movq	(%rcx,%rdx,8), %rcx
	testq	%rcx, %rcx
	je	.LBB12_802
# BB#800:                               # %if.then132.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movslq	8(%rcx), %rcx
	movl	reg_reloaded_contents(,%rcx,4), %edx
	cmpq	%rax, %rdx
	jne	.LBB12_802
# BB#801:                               # %if.then142.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	$0, spill_reg_store(,%rcx,8)
	.align	16, 0x90
.LBB12_802:                             # %if.end148.i
                                        #   in Loop: Header=BB12_791 Depth=2
	testq	%r12, %r12
	js	.LBB12_877
# BB#803:                               # %land.lhs.true151.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%rbx, 192(%rsp)         # 8-byte Spill
	movq	rld+64(%r8), %rax
	movq	%r8, 208(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB12_876
# BB#804:                               # %if.then157.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	208(%rsp), %rbx         # 8-byte Reload
	leaq	rld+40(%rbx), %rcx
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	movzbl	2(%rax), %eax
	leaq	rld+64(%rbx), %rcx
	movq	%rcx, 184(%rsp)         # 8-byte Spill
	movq	%r12, %rcx
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	je	.LBB12_808
# BB#805:                               # %if.then157.i
                                        #   in Loop: Header=BB12_791 Depth=2
	leaq	-21(%r12), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_808
# BB#806:                               # %if.then157.i
                                        #   in Loop: Header=BB12_791 Depth=2
	leaq	-45(%r12), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_808
# BB#807:                               # %if.then157.i
                                        #   in Loop: Header=BB12_791 Depth=2
	leaq	-29(%r12), %rcx
	andq	%r9, %rcx
	cmpq	$7, %rcx
	jbe	.LBB12_808
# BB#812:                               # %cond.false.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%rsi, 160(%rsp)         # 8-byte Spill
	cmpq	$24, %rax
	jne	.LBB12_813
# BB#815:                               # %cond.true220.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movzbl	target_flags+3(%rip), %r11d
	andq	$2, %r11
	xorq	$6, %r11
	jmp	.LBB12_817
.LBB12_808:                             # %cond.true.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%rsi, 160(%rsp)         # 8-byte Spill
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %r11
	je	.LBB12_817
# BB#809:                               # %lor.rhs.i
                                        #   in Loop: Header=BB12_791 Depth=2
	cmpq	$6, %rax
	movabsq	$0, %rax
	movq	%rax, %r11
	jne	.LBB12_811
# BB#810:                               # %lor.rhs.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movabsq	$1, %rax
	movq	%rax, %r11
.LBB12_811:                             # %lor.rhs.i
                                        #   in Loop: Header=BB12_791 Depth=2
	incq	%r11
	jmp	.LBB12_817
.LBB12_813:                             # %cond.false.i
                                        #   in Loop: Header=BB12_791 Depth=2
	cmpq	$18, %rax
	jne	.LBB12_816
# BB#814:                               # %cond.true208.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	target_flags(%rip), %r11d
	movq	$25, %rcx
	shrq	%cl, %r11
	andq	$1, %r11
	xorq	$3, %r11
	jmp	.LBB12_817
.LBB12_816:                             # %cond.end243.i
                                        #   in Loop: Header=BB12_791 Depth=2
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
	movq	$32, %rcx
                                        # kill: RAX<def> R11<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%r8, %r8
	testq	%rax, %rax
	jle	.LBB12_827
	.align	16, 0x90
.LBB12_817:                             # %for.body248.lr.ph.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%r12, %r14
	leaq	rld+72(%rbx), %rax
	movq	%rax, 232(%rsp)         # 8-byte Spill
	leaq	rld+92(%rbx), %rax
	movq	%rax, 224(%rsp)         # 8-byte Spill
	movq	$1, %r12
	xorq	%r13, %r13
	movq	$0, 216(%rsp)           # 8-byte Folded Spill
	.align	16, 0x90
.LBB12_818:                             # %for.body248.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_791 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r11, %r15
	movq	224(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %edx
	movq	232(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %esi
	leaq	(%r13,%r14), %rdi
	movq	%r9, %rbx
	callq	reload_reg_reaches_end_p
	movq	%rbx, %r9
	andq	%r9, %rax
	movabsq	$0, %rcx
	je	.LBB12_820
# BB#819:                               # %for.body248.i
                                        #   in Loop: Header=BB12_818 Depth=3
	movq	%r12, %rcx
.LBB12_820:                             # %for.body248.i
                                        #   in Loop: Header=BB12_818 Depth=3
	testq	%rax, %rax
	movq	%r15, %r11
	je	.LBB12_822
# BB#821:                               # %for.body248.i
                                        #   in Loop: Header=BB12_818 Depth=3
	movq	$1, 216(%rsp)           # 8-byte Folded Spill
.LBB12_822:                             # %for.body248.i
                                        #   in Loop: Header=BB12_818 Depth=3
	incq	%r13
	movq	%r13, %rax
	andq	%r9, %rax
	movq	%r11, %r15
	andq	%r9, %r15
	cmpq	%r15, %rax
	movq	%rcx, %r12
	jne	.LBB12_818
# BB#823:                               # %for.end260.i
                                        #   in Loop: Header=BB12_791 Depth=2
	testq	%r9, %rcx
	jne	.LBB12_824
# BB#926:                               # %if.else826.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	$1, %rbx
	movq	216(%rsp), %rax         # 8-byte Reload
	testq	%r9, %rax
	jmp	.LBB12_927
.LBB12_824:                             # %for.body266.lr.ph.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	reg_reloaded_valid(%rip), %rdx
	movq	$1, %rax
	movq	%r14, %r12
	.align	16, 0x90
.LBB12_825:                             # %for.body266.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_791 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rdx, %rsi
	leaq	-1(%rax,%r12), %rcx
	movq	$1, %rdx
	shlq	%cl, %rdx
	notq	%rdx
	andq	%rsi, %rdx
	movq	%rax, %rcx
	andq	%r9, %rcx
	incq	%rax
	cmpq	%r15, %rcx
	jne	.LBB12_825
# BB#826:                               # %for.cond263.for.end271_crit_edge.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%rdx, reg_reloaded_valid(%rip)
	movq	$1, %r8
	movq	208(%rsp), %rbx         # 8-byte Reload
.LBB12_827:                             # %for.end271.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	rld+8(%rbx), %rsi
	leaq	rld+8(%rbx), %r10
	testq	%rsi, %rsi
	jne	.LBB12_844
# BB#828:                               # %for.end271.if.else511_crit_edge.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	rld+48(%rbx), %rsi
	jmp	.LBB12_829
	.align	16, 0x90
.LBB12_930:                             # %for.inc849.i
                                        #   in Loop: Header=BB12_927 Depth=3
	movq	%rbx, %rax
	andq	%r9, %rax
	incq	%rbx
	cmpq	%r15, %rax
.LBB12_927:                             # %if.else826.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_791 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r14, %r12
	je	.LBB12_876
# BB#928:                               # %for.body832.i
                                        #   in Loop: Header=BB12_927 Depth=3
	movq	224(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %edx
	movq	232(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %esi
	leaq	-1(%rbx,%r12), %r13
	movq	%r12, %r14
	movq	%r13, %rdi
	movq	%r9, %r12
	callq	reload_reg_reaches_end_p
	movq	%r12, %r9
	testq	%r9, %rax
	je	.LBB12_930
# BB#929:                               # %if.then842.i
                                        #   in Loop: Header=BB12_927 Depth=3
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	notq	%rax
	andq	%rax, reg_reloaded_valid(%rip)
	jmp	.LBB12_930
.LBB12_844:                             # %land.lhs.true276.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	je	.LBB12_848
# BB#845:                               # %lor.lhs.false284.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	rld+48(%rbx), %rsi
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	jne	.LBB12_829
# BB#846:                               # %cond.false304.i
                                        #   in Loop: Header=BB12_791 Depth=2
	testq	%rsi, %rsi
	jne	.LBB12_848
# BB#847:                               # %cond.false313.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	8(%rax), %rsi
.LBB12_848:                             # %cond.end322.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%r8, 224(%rsp)          # 8-byte Spill
	movslq	8(%rsi), %r10
	cmpq	$53, %r10
	jl	.LBB12_850
# BB#849:                               # %cond.end322.cond.end430_crit_edge.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%rbx, 208(%rsp)         # 8-byte Spill
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r8
	movq	$1, %rdi
	jmp	.LBB12_863
.LBB12_829:                             # %if.else511.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%rbx, 208(%rsp)         # 8-byte Spill
	testq	%rsi, %rsi
	jne	.LBB12_876
# BB#830:                               # %land.lhs.true517.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	208(%rsp), %rax         # 8-byte Reload
	movq	rld(%rax), %rax
	testq	%rax, %rax
	je	.LBB12_876
# BB#831:                               # %land.lhs.true522.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movzbl	(%rax), %ecx
	movq	208(%rsp), %rdx         # 8-byte Reload
	leaq	rld+48(%rdx), %rbx
	leaq	rld(%rdx), %rdx
	cmpq	$61, %rcx
	jne	.LBB12_834
# BB#832:                               # %land.lhs.true530.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	8(%rax), %eax
	cmpq	$53, %rax
	jb	.LBB12_834
# BB#833:                               # %land.lhs.true539.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	reg_has_output_reload(%rip), %rcx
	movzbl	(%rcx,%rax), %eax
	testq	%rax, %rax
	je	.LBB12_836
.LBB12_834:                             # %lor.lhs.false549.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB12_876
# BB#835:                               # %land.lhs.true557.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	8(%rax), %eax
	movq	reg_has_output_reload(%rip), %rcx
	movzbl	(%rcx,%rax), %eax
	testq	%rax, %rax
	jne	.LBB12_876
.LBB12_836:                             # %land.lhs.true567.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rdi
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rsi
	movq	%r8, %r14
	movq	%r10, 232(%rsp)         # 8-byte Spill
	movq	%rbx, 216(%rsp)         # 8-byte Spill
	movq	%r9, %r15
	movq	%r11, %rbx
	movq	%rdx, %r13
	callq	reg_set_p
	movq	%r13, %rcx
	movq	%rbx, %r11
	movq	%r15, %r9
	movq	216(%rsp), %r10         # 8-byte Reload
	movq	232(%rsp), %rbx         # 8-byte Reload
	movq	%r14, %r13
	movq	160(%rsp), %rdi         # 8-byte Reload
	testq	%r9, %rax
	jne	.LBB12_876
# BB#837:                               # %if.then576.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	(%rcx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB12_839
# BB#838:                               # %land.lhs.true586.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	8(%rax), %r14d
	cmpq	$52, %r14
	ja	.LBB12_842
.LBB12_839:                             # %if.else602.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	je	.LBB12_841
# BB#840:                               # %if.else617.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	8(%rax), %rax
.LBB12_841:                             # %if.end628.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	8(%rax), %r14d
.LBB12_842:                             # %if.end628.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jl	.LBB12_891
# BB#843:                               # %cond.end731.thread1692.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	reg_last_reload_reg(%rip), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rax, (%rsi,%rdx,8)
	movq	$1, %rsi
	jmp	.LBB12_914
.LBB12_850:                             # %cond.false331.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r8
	movzbl	2(%r8), %eax
	movq	%r10, %rcx
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	je	.LBB12_854
# BB#851:                               # %cond.false331.i
                                        #   in Loop: Header=BB12_791 Depth=2
	leaq	-21(%r10), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_854
# BB#852:                               # %cond.false331.i
                                        #   in Loop: Header=BB12_791 Depth=2
	leaq	-45(%r10), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_854
# BB#853:                               # %cond.false331.i
                                        #   in Loop: Header=BB12_791 Depth=2
	leaq	-29(%r10), %rcx
	andq	%r9, %rcx
	cmpq	$7, %rcx
	jbe	.LBB12_854
# BB#858:                               # %cond.false379.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%rbx, 208(%rsp)         # 8-byte Spill
	cmpq	$24, %rax
	jne	.LBB12_859
# BB#861:                               # %cond.true401.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movzbl	target_flags+3(%rip), %edi
	andq	$2, %rdi
	xorq	$6, %rdi
	jmp	.LBB12_863
.LBB12_854:                             # %cond.true355.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%rbx, 208(%rsp)         # 8-byte Spill
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %rdi
	je	.LBB12_863
# BB#855:                               # %lor.rhs366.i
                                        #   in Loop: Header=BB12_791 Depth=2
	cmpq	$6, %rax
	movabsq	$0, %r14
	movq	%r14, %rdi
	jne	.LBB12_857
# BB#856:                               # %lor.rhs366.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movabsq	$1, %r14
	movq	%r14, %rdi
.LBB12_857:                             # %lor.rhs366.i
                                        #   in Loop: Header=BB12_791 Depth=2
	incq	%rdi
	jmp	.LBB12_863
.LBB12_859:                             # %cond.false379.i
                                        #   in Loop: Header=BB12_791 Depth=2
	cmpq	$18, %rax
	jne	.LBB12_862
# BB#860:                               # %cond.true388.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	xorq	$3, %rdi
	jmp	.LBB12_863
.LBB12_862:                             # %cond.false405.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
	movq	%rax, %rdi
.LBB12_863:                             # %cond.end430.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%rdi, 232(%rsp)         # 8-byte Spill
	movq	new_spill_reg_store(,%r12,8), %rax
	movq	reg_last_reload_reg(%rip), %rcx
	movq	%rax, spill_reg_store(,%r12,8)
	movq	%rsi, spill_reg_stored_to(,%r12,8)
	movq	%r12, 200(%rsp)         # 8-byte Spill
	movq	%r8, (%rcx,%r10,8)
	cmpq	$52, %r10
	movq	%r10, 216(%rsp)         # 8-byte Spill
	jg	.LBB12_869
# BB#864:                               # %cond.end430.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB12_869
# BB#865:                               # %for.body449.lr.ph.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	$3, %rcx
	movq	216(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$1, %rbx
	.align	16, 0x90
.LBB12_866:                             # %for.body449.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_791 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rdi, %r12
	andq	%r9, %r12
	movq	%r11, %rcx
	andq	%r9, %rcx
	xorq	%rax, %rax
	cmpq	%r12, %rcx
	jne	.LBB12_868
# BB#867:                               # %cond.true452.i
                                        #   in Loop: Header=BB12_866 Depth=3
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movl	8(%rax), %esi
	addq	%rbx, %rsi
	movq	%rsi, %rax
	andq	%r9, %rax
	movl	reg_raw_mode(,%rax,4), %edi
	movq	%r9, %r13
	movq	%r11, %r14
	movq	%rdx, %r15
	callq	gen_rtx_REG
	movq	%r15, %rdx
	movq	232(%rsp), %rdi         # 8-byte Reload
	movq	%r14, %r11
	movq	%r13, %r9
.LBB12_868:                             # %cond.end471.i
                                        #   in Loop: Header=BB12_866 Depth=3
	movq	reg_last_reload_reg(%rip), %rcx
	addq	%rdx, %rcx
	movq	%rax, (%rcx,%rbx,8)
	incq	%rbx
	movq	%rbx, %rax
	andq	%r9, %rax
	cmpq	%r12, %rax
	jne	.LBB12_866
.LBB12_869:                             # %for.cond480.preheader.i
                                        #   in Loop: Header=BB12_791 Depth=2
	cmpq	$0, 224(%rsp)           # 8-byte Folded Reload
	movq	200(%rsp), %r12         # 8-byte Reload
	je	.LBB12_876
# BB#870:                               # %for.body483.lr.ph.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	216(%rsp), %r14         # 8-byte Reload
	cmpq	$53, %r14
	movabsq	$0, %rax
	jge	.LBB12_872
# BB#871:                               # %for.body483.lr.ph.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movabsq	$1, %rax
.LBB12_872:                             # %for.body483.lr.ph.i
                                        #   in Loop: Header=BB12_791 Depth=2
	andq	%r9, %rdi
	movq	reg_reloaded_dead(%rip), %rdx
	movq	reg_reloaded_valid(%rip), %rsi
	andq	%r9, %r11
	cmpq	%rdi, %r11
	movq	$1, %rdi
	jne	.LBB12_873
# BB#889:                               # %for.body483.lr.ph.i
                                        #   in Loop: Header=BB12_791 Depth=2
	testq	%rax, %rax
	movq	176(%rsp), %r10         # 8-byte Reload
	je	.LBB12_874
	.align	16, 0x90
.LBB12_890:                             # %for.body483.us.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_791 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1(%rdi,%r12), %rax
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	movq	%rbx, %rcx
	notq	%rcx
	andq	%rcx, %rdx
	leaq	-1(%rdi,%r14), %r8
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	%r8d, reg_reloaded_contents(,%rax,4)
	movq	%r10, reg_reloaded_insn(,%rax,8)
	orq	%rbx, %rsi
	movq	%rdi, %rax
	andq	%r9, %rax
	incq	%rdi
	cmpq	%r11, %rax
	jne	.LBB12_890
	jmp	.LBB12_875
.LBB12_873:                             #   in Loop: Header=BB12_791 Depth=2
	movq	176(%rsp), %r10         # 8-byte Reload
	.align	16, 0x90
.LBB12_874:                             # %for.body483.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_791 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1(%rdi,%r12), %rax
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	movq	%rbx, %rcx
	notq	%rcx
	andq	%rcx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	%r14d, reg_reloaded_contents(,%rax,4)
	movq	%r10, reg_reloaded_insn(,%rax,8)
	orq	%rbx, %rsi
	movq	%rdi, %rax
	andq	%r9, %rax
	incq	%rdi
	cmpq	%r11, %rax
	jne	.LBB12_874
	jmp	.LBB12_875
.LBB12_891:                             # %cond.false632.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r8
	movzbl	2(%r8), %eax
	movq	%r14, %rcx
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	je	.LBB12_895
# BB#892:                               # %cond.false632.i
                                        #   in Loop: Header=BB12_791 Depth=2
	leaq	-21(%r14), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_895
# BB#893:                               # %cond.false632.i
                                        #   in Loop: Header=BB12_791 Depth=2
	leaq	-45(%r14), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_895
# BB#894:                               # %cond.false632.i
                                        #   in Loop: Header=BB12_791 Depth=2
	leaq	-29(%r14), %rcx
	andq	%r9, %rcx
	cmpq	$7, %rcx
	jbe	.LBB12_895
# BB#900:                               # %cond.false680.i
                                        #   in Loop: Header=BB12_791 Depth=2
	cmpq	$24, %rax
	jne	.LBB12_901
# BB#905:                               # %cond.true702.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movzbl	target_flags+3(%rip), %esi
	andq	$2, %rsi
	xorq	$6, %rsi
	jmp	.LBB12_906
.LBB12_895:                             # %cond.true656.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	jne	.LBB12_897
# BB#896:                               # %for.cond741.preheader.i.thread
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%r13, 224(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%r14, %rdx
	movq	%r14, 152(%rsp)         # 8-byte Spill
	shlq	%cl, %rdx
	movq	reg_last_reload_reg(%rip), %rax
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%r8, (%rax,%rdx,8)
	movq	$2, %rsi
	jmp	.LBB12_909
.LBB12_901:                             # %cond.false680.i
                                        #   in Loop: Header=BB12_791 Depth=2
	cmpq	$18, %rax
	jne	.LBB12_903
# BB#902:                               # %cond.true689.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	xorq	$3, %rsi
	jmp	.LBB12_904
.LBB12_897:                             # %lor.rhs667.i
                                        #   in Loop: Header=BB12_791 Depth=2
	cmpq	$6, %rax
	movabsq	$0, %rax
	movq	%rax, %rsi
	movq	232(%rsp), %rbx         # 8-byte Reload
	movq	216(%rsp), %r10         # 8-byte Reload
	jne	.LBB12_899
# BB#898:                               # %lor.rhs667.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movabsq	$1, %rax
	movq	%rax, %rsi
.LBB12_899:                             # %lor.rhs667.i
                                        #   in Loop: Header=BB12_791 Depth=2
	incq	%rsi
.LBB12_906:                             # %cond.end731.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	reg_last_reload_reg(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	movq	%r8, (%rdx,%rax,8)
	jg	.LBB12_914
	jmp	.LBB12_907
.LBB12_903:                             # %cond.false706.i
                                        #   in Loop: Header=BB12_791 Depth=2
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
.LBB12_904:                             # %cond.end731.thread.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	movq	reg_last_reload_reg(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%r8, (%rdx,%rax,8)
.LBB12_907:                             # %for.cond741.preheader.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB12_914
# BB#908:                               # %for.cond741.preheader.i.for.body744.lr.ph.i_crit_edge
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%r10, 216(%rsp)         # 8-byte Spill
	movq	%rbx, 232(%rsp)         # 8-byte Spill
	movq	%r13, 224(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%r14, %rdx
	movq	%r14, 152(%rsp)         # 8-byte Spill
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
.LBB12_909:                             # %for.body744.lr.ph.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%rsi, 168(%rsp)         # 8-byte Spill
	movq	$3, %rcx
	shlq	%cl, %rdx
	movq	$1, %rbx
.LBB12_910:                             # %for.body744.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_791 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rsi, %r15
	andq	%r9, %r15
	movq	%r11, %rcx
	andq	%r9, %rcx
	xorq	%rax, %rax
	cmpq	%r15, %rcx
	jne	.LBB12_912
# BB#911:                               # %cond.true747.i
                                        #   in Loop: Header=BB12_910 Depth=3
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movl	8(%rax), %esi
	addq	%rbx, %rsi
	movq	%rsi, %rax
	andq	%r9, %rax
	movl	reg_raw_mode(,%rax,4), %edi
	movq	%r9, %r12
	movq	%r11, %r14
	movq	%rdx, %r13
	callq	gen_rtx_REG
	movq	%r13, %rdx
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%r14, %r11
	movq	%r12, %r9
	movq	200(%rsp), %r12         # 8-byte Reload
	movq	160(%rsp), %rdi         # 8-byte Reload
.LBB12_912:                             # %cond.end766.i
                                        #   in Loop: Header=BB12_910 Depth=3
	movq	reg_last_reload_reg(%rip), %rcx
	addq	%rdx, %rcx
	movq	%rax, (%rcx,%rbx,8)
	incq	%rbx
	movq	%rbx, %rax
	andq	%r9, %rax
	cmpq	%r15, %rax
	jne	.LBB12_910
# BB#913:                               #   in Loop: Header=BB12_791 Depth=2
	movq	224(%rsp), %r13         # 8-byte Reload
	movq	152(%rsp), %r14         # 8-byte Reload
	movq	232(%rsp), %rbx         # 8-byte Reload
	movq	216(%rsp), %r10         # 8-byte Reload
.LBB12_914:                             # %if.end774.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movzbl	reload_inherited(%rdi), %eax
	testq	%rax, %rax
	je	.LBB12_917
# BB#915:                               # %lor.lhs.false778.i
                                        #   in Loop: Header=BB12_791 Depth=2
	cmpq	$0, (%rbx)
	je	.LBB12_918
# BB#916:                               # %land.lhs.true783.i
                                        #   in Loop: Header=BB12_791 Depth=2
	cmpq	$0, (%r10)
	jne	.LBB12_918
.LBB12_917:                             # %if.then788.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	$0, spill_reg_store(,%r12,8)
.LBB12_918:                             # %for.cond792.preheader.i
                                        #   in Loop: Header=BB12_791 Depth=2
	testq	%r13, %r13
	je	.LBB12_876
# BB#919:                               # %for.body795.lr.ph.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	movabsq	$0, %rax
	jge	.LBB12_921
# BB#920:                               # %for.body795.lr.ph.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movabsq	$1, %rax
.LBB12_921:                             # %for.body795.lr.ph.i
                                        #   in Loop: Header=BB12_791 Depth=2
	andq	%r9, %rsi
	movq	reg_reloaded_dead(%rip), %rdx
	movq	%rsi, %rcx
	movq	reg_reloaded_valid(%rip), %rsi
	andq	%r9, %r11
	cmpq	%rcx, %r11
	movq	$1, %rdi
	jne	.LBB12_922
# BB#924:                               # %for.body795.lr.ph.i
                                        #   in Loop: Header=BB12_791 Depth=2
	testq	%rax, %rax
	movq	176(%rsp), %r10         # 8-byte Reload
	je	.LBB12_923
.LBB12_925:                             # %for.body795.us.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_791 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1(%rdi,%r12), %rax
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	movq	%rbx, %rcx
	notq	%rcx
	andq	%rcx, %rdx
	leaq	-1(%rdi,%r14), %r8
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	%r8d, reg_reloaded_contents(,%rax,4)
	movq	%r10, reg_reloaded_insn(,%rax,8)
	orq	%rbx, %rsi
	movq	%rdi, %rax
	andq	%r9, %rax
	incq	%rdi
	cmpq	%r11, %rax
	jne	.LBB12_925
	jmp	.LBB12_875
.LBB12_922:                             #   in Loop: Header=BB12_791 Depth=2
	movq	176(%rsp), %r10         # 8-byte Reload
.LBB12_923:                             # %for.body795.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_791 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1(%rdi,%r12), %rax
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	movq	%rbx, %rcx
	notq	%rcx
	andq	%rcx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	%r14d, reg_reloaded_contents(,%rax,4)
	movq	%r10, reg_reloaded_insn(,%rax,8)
	orq	%rbx, %rsi
	movq	%rdi, %rax
	andq	%r9, %rax
	incq	%rdi
	cmpq	%r11, %rax
	jne	.LBB12_923
.LBB12_875:                             # %for.cond480.if.end854.loopexit1705_crit_edge.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%r10, 176(%rsp)         # 8-byte Spill
	movq	%rdx, reg_reloaded_dead(%rip)
	movq	%rsi, reg_reloaded_valid(%rip)
	.align	16, 0x90
.LBB12_876:                             # %if.end854.i
                                        #   in Loop: Header=BB12_791 Depth=2
	testq	%r12, %r12
	movq	136(%rsp), %r14         # 8-byte Reload
	movq	192(%rsp), %rbx         # 8-byte Reload
	movq	208(%rsp), %r8          # 8-byte Reload
	jns	.LBB12_975
.LBB12_877:                             # %land.lhs.true857.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	rld+8(%r8), %r15
	testq	%r15, %r15
	je	.LBB12_975
# BB#878:                               # %land.lhs.true863.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	je	.LBB12_881
# BB#879:                               # %land.lhs.true863.i
                                        #   in Loop: Header=BB12_791 Depth=2
	cmpq	$66, %rcx
	jne	.LBB12_975
# BB#880:                               # %land.lhs.true879.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	rld+48(%r8), %r15
	movzbl	(%r15), %ecx
	cmpq	$61, %rcx
	jne	.LBB12_975
	.align	16, 0x90
.LBB12_881:                             # %cond.end904.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movslq	8(%r15), %r13
	cmpq	$53, %r13
	jl	.LBB12_960
# BB#882:                               # %if.then912.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	reg_last_reload_reg(%rip), %rax
	movq	$0, (%rax,%r13,8)
	movq	rld+64(%r8), %r10
	testq	%r10, %r10
	jne	.LBB12_993
# BB#883:                               # %if.then919.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	176(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB12_975
# BB#884:                               # %cond.true927.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	32(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB12_886
# BB#885:                               # %cond.false939.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%rax, %rsi
	movq	%rbx, 192(%rsp)         # 8-byte Spill
	movq	%r9, %rbx
	movq	%r8, %r12
	callq	single_set_2
	movq	%r12, %r8
	movq	%rbx, %r9
	movq	192(%rsp), %rbx         # 8-byte Reload
.LBB12_886:                             # %cond.end947.i
                                        #   in Loop: Header=BB12_791 Depth=2
	testq	%rax, %rax
	je	.LBB12_975
# BB#887:                               # %land.lhs.true950.i
                                        #   in Loop: Header=BB12_791 Depth=2
	leaq	rld+8(%r8), %rcx
	movq	8(%rax), %rdx
	movq	(%rcx), %rcx
	cmpq	%rcx, %rdx
	jne	.LBB12_975
# BB#888:                               # %if.then959.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movslq	n_reloads(%rip), %rdx
	movq	16(%rax), %r10
	testq	%rdx, %rdx
	movq	$1, %rax
	movabsq	$rld+64, %rsi
	jle	.LBB12_934
	.align	16, 0x90
.LBB12_932:                             # %for.body967.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_791 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	-64(%rsi), %rcx
	cmpq	%r10, %rcx
	je	.LBB12_933
# BB#931:                               # %for.cond964.i
                                        #   in Loop: Header=BB12_932 Depth=3
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	addq	$104, %rsi
	incq	%rax
	cmpq	%rdx, %rdi
	jl	.LBB12_932
	jmp	.LBB12_934
.LBB12_960:                             # %if.else1136.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%r13, %rdx
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB12_964
# BB#961:                               # %if.else1136.i
                                        #   in Loop: Header=BB12_791 Depth=2
	leaq	-21(%r13), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_964
# BB#962:                               # %if.else1136.i
                                        #   in Loop: Header=BB12_791 Depth=2
	leaq	-45(%r13), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_964
# BB#963:                               # %if.else1136.i
                                        #   in Loop: Header=BB12_791 Depth=2
	leaq	-29(%r13), %rcx
	andq	%r9, %rcx
	cmpq	$7, %rcx
	jbe	.LBB12_964
# BB#968:                               # %cond.false1184.i
                                        #   in Loop: Header=BB12_791 Depth=2
	cmpq	$24, %rax
	jne	.LBB12_969
# BB#971:                               # %cond.true1206.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB12_973
.LBB12_993:                             # %if.end988.thread1699.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	8(%r10), %eax
	movq	new_spill_reg_store(,%rax,8), %rax
	movq	%rax, 232(%rsp)         # 8-byte Spill
.LBB12_935:                             # %land.lhs.true990.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movzbl	(%r10), %eax
	cmpq	$61, %rax
	jne	.LBB12_975
# BB#936:                               # %land.lhs.true995.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	8(%r10), %r11d
	movq	$32, %rcx
	movq	%r11, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	%r12, 200(%rsp)         # 8-byte Spill
	cmpq	$52, %r11
	ja	.LBB12_975
# BB#937:                               # %if.then1001.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movslq	rld+28(%r8), %rax
	movq	%r11, %rcx
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	je	.LBB12_941
# BB#938:                               # %if.then1001.i
                                        #   in Loop: Header=BB12_791 Depth=2
	leaq	-21(%r11), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_941
# BB#939:                               # %if.then1001.i
                                        #   in Loop: Header=BB12_791 Depth=2
	leaq	-45(%r11), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_941
# BB#940:                               # %if.then1001.i
                                        #   in Loop: Header=BB12_791 Depth=2
	leaq	-29(%r11), %rcx
	andq	%r9, %rcx
	cmpq	$7, %rcx
	jbe	.LBB12_941
# BB#945:                               # %cond.false1046.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%rax, %rcx
	andq	%r9, %rcx
	cmpq	$24, %rcx
	jne	.LBB12_946
# BB#948:                               # %cond.true1062.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%r10, 184(%rsp)         # 8-byte Spill
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB12_950
.LBB12_964:                             # %cond.true1160.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB12_973
# BB#965:                               # %lor.rhs1171.i
                                        #   in Loop: Header=BB12_791 Depth=2
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB12_967
# BB#966:                               # %lor.rhs1171.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movabsq	$1, %rax
.LBB12_967:                             # %lor.rhs1171.i
                                        #   in Loop: Header=BB12_791 Depth=2
	incq	%rax
	jmp	.LBB12_973
.LBB12_969:                             # %cond.false1184.i
                                        #   in Loop: Header=BB12_791 Depth=2
	cmpq	$18, %rax
	jne	.LBB12_972
# BB#970:                               # %cond.true1193.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB12_973
.LBB12_972:                             # %while.cond1235.preheader.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB12_975
.LBB12_973:                             # %while.body1239.lr.ph.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rax
	movq	$3, %rcx
	shlq	%cl, %r13
	.align	16, 0x90
.LBB12_974:                             # %while.body1239.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_791 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	reg_last_reload_reg(%rip), %rcx
	addq	%r13, %rcx
	movq	$0, (%rcx,%rax,8)
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%rax
	testq	%rdx, %rdx
	jg	.LBB12_974
	jmp	.LBB12_975
.LBB12_941:                             # %cond.true1029.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%r10, 184(%rsp)         # 8-byte Spill
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB12_950
# BB#942:                               # %lor.rhs1036.i
                                        #   in Loop: Header=BB12_791 Depth=2
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB12_944
# BB#943:                               # %lor.rhs1036.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movabsq	$1, %rax
.LBB12_944:                             # %lor.rhs1036.i
                                        #   in Loop: Header=BB12_791 Depth=2
	incq	%rax
	jmp	.LBB12_950
.LBB12_946:                             # %cond.false1046.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%r10, 184(%rsp)         # 8-byte Spill
	cmpq	$18, %rcx
	jne	.LBB12_949
# BB#947:                               # %cond.true1052.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB12_950
.LBB12_949:                             # %cond.false1066.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB12_950:                             # %cond.end1086.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%r11, 216(%rsp)         # 8-byte Spill
	movq	%rax, 208(%rsp)         # 8-byte Spill
	movq	%rbx, 192(%rsp)         # 8-byte Spill
	movq	$1, %rsi
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	%r11, %rdx
	callq	find_regno_note
	movq	%rax, 224(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB12_953
# BB#951:                               # %cond.end1086.i
                                        #   in Loop: Header=BB12_791 Depth=2
	cmpq	$0, 232(%rsp)           # 8-byte Folded Reload
	je	.LBB12_953
# BB#952:                               # %if.then1092.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	$1, %rsi
	movq	232(%rsp), %rdi         # 8-byte Reload
	movq	216(%rsp), %rdx         # 8-byte Reload
	callq	find_regno_note
	movq	%rax, 224(%rsp)         # 8-byte Spill
.LBB12_953:                             # %while.cond.preheader.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	$32, %rcx
	movq	208(%rsp), %rsi         # 8-byte Reload
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB12_959
# BB#954:                               # %while.body.lr.ph.lr.ph.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	$1, %rax
	movq	reg_reloaded_dead(%rip), %rbx
	movq	reg_reloaded_valid(%rip), %r11
	movq	reg_is_output_reload(%rip), %rdi
	movq	216(%rsp), %rcx         # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 216(%rsp)         # 8-byte Spill
	notq	%rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
	leaq	reg_reloaded_contents-4(,%r12,4), %r8
	movq	%r8, 160(%rsp)          # 8-byte Spill
.LBB12_955:                             # %while.body.lr.ph.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_791 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB12_956 Depth 4
	movq	%rsi, 208(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	-1(%r12,%rdx), %rax
	leaq	(%r12,%rdx), %rcx
	leaq	-8(,%rcx,8), %r14
	leaq	(%r8,%rdx,4), %r9
	leaq	-1(%rdx), %r8
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB12_956:                             # %while.body.i
                                        #   Parent Loop BB12_4 Depth=1
                                        #     Parent Loop BB12_791 Depth=2
                                        #       Parent Loop BB12_955 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	%rbx, %r12
	movq	%rdi, %r10
	leaq	(%rax,%rdx), %rcx
	movq	$1, %rdi
	shlq	%cl, %rdi
	movq	%rdi, %rbx
	notq	%rbx
	andq	%r12, %rbx
	orq	%rdi, %r11
	orq	%r10, %rdi
	cmpq	$0, 224(%rsp)           # 8-byte Folded Reload
	movq	232(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, spill_reg_store(%r14,%rdx,8)
	movq	%r15, spill_reg_stored_to(%r14,%rdx,8)
	movl	%r13d, (%r9,%rdx,4)
	movq	%rcx, reg_reloaded_insn(%r14,%rdx,8)
	je	.LBB12_994
# BB#957:                               # %if.then1123.i
                                        #   in Loop: Header=BB12_956 Depth=4
	leaq	(%r8,%rdx), %rsi
	movq	216(%rsp), %rcx         # 8-byte Reload
	orq	%rcx, reg_reloaded_died(%rip)
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	decq	%rdx
	testq	%rsi, %rsi
	jg	.LBB12_956
	jmp	.LBB12_958
.LBB12_994:                             # %if.else1127.i
                                        #   in Loop: Header=BB12_955 Depth=3
	movq	$32, %rcx
	movq	208(%rsp), %rsi         # 8-byte Reload
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	leaq	-1(%rsi,%rdx), %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	%rbx, reg_reloaded_dead(%rip)
	movq	%r11, reg_reloaded_valid(%rip)
	movq	%rdi, reg_is_output_reload(%rip)
	movq	168(%rsp), %rdx         # 8-byte Reload
	andq	%rdx, reg_reloaded_died(%rip)
	sarq	%cl, %rax
	movq	$0, 224(%rsp)           # 8-byte Folded Spill
	testq	%rax, %rax
	movq	136(%rsp), %r14         # 8-byte Reload
	movq	200(%rsp), %r12         # 8-byte Reload
	movq	160(%rsp), %r8          # 8-byte Reload
	jg	.LBB12_955
	jmp	.LBB12_959
.LBB12_958:                             # %while.cond.while.end_crit_edge.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	%rbx, reg_reloaded_dead(%rip)
	movq	%r11, reg_reloaded_valid(%rip)
	movq	%rdi, reg_is_output_reload(%rip)
	movq	136(%rsp), %r14         # 8-byte Reload
.LBB12_959:                             # %while.end.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	reg_last_reload_reg(%rip), %rax
	movq	184(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, (%rax,%r13,8)
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	192(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB12_975
.LBB12_933:                             # %if.then973.i
                                        #   in Loop: Header=BB12_791 Depth=2
	movq	(%rsi), %r10
.LBB12_934:                             # %if.end988.i
                                        #   in Loop: Header=BB12_791 Depth=2
	testq	%r10, %r10
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	%rax, 232(%rsp)         # 8-byte Spill
	jne	.LBB12_935
	.align	16, 0x90
.LBB12_975:                             # %for.inc1246.i
                                        #   in Loop: Header=BB12_791 Depth=2
	incq	%rbx
	movslq	n_reloads(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	80(%rsp), %r12          # 8-byte Reload
	movq	72(%rsp), %r13          # 8-byte Reload
	jl	.LBB12_791
.LBB12_976:                             # %emit_reload_insns.exit
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	reg_reloaded_died(%rip), %rax
	orq	%rax, reg_reloaded_dead(%rip)
	movq	%r12, %rdi
	movq	%r9, %rbx
	callq	subst_reloads
	movq	32(%r12), %rdi
	callq	asm_noperands
	movq	%rbx, %r8
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	40(%rsp), %r15          # 8-byte Reload
	js	.LBB12_23
# BB#977:                               # %for.cond126.preheader
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	24(%rax), %rbx
	jmp	.LBB12_978
.LBB12_980:                             # %for.cond126.backedge
                                        #   in Loop: Header=BB12_978 Depth=2
	movq	24(%rbx), %rbx
.LBB12_978:                             # %for.cond126.preheader
                                        #   Parent Loop BB12_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	64(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %rbx
	je	.LBB12_23
# BB#979:                               # %for.body129
                                        #   in Loop: Header=BB12_978 Depth=2
	cmpq	%r12, %rbx
	je	.LBB12_980
# BB#981:                               # %land.lhs.true132
                                        #   in Loop: Header=BB12_978 Depth=2
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB12_980
# BB#982:                               # %land.lhs.true140
                                        #   in Loop: Header=BB12_978 Depth=2
	movslq	40(%rbx), %rax
	testq	%rax, %rax
	jns	.LBB12_984
# BB#983:                               # %cond.end
                                        #   in Loop: Header=BB12_978 Depth=2
	movq	%rbx, %rdi
	movq	%r8, %r15
	callq	recog_memoized_1
	movq	%r15, %r8
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB12_985
.LBB12_984:                             # %lor.lhs.false151
                                        #   in Loop: Header=BB12_978 Depth=2
	movq	%rbx, %rdi
	movq	%r8, %r15
	callq	extract_insn
	movq	$1, %rdi
	callq	constrain_operands
	movq	%r15, %r8
	movq	40(%rsp), %r15          # 8-byte Reload
	testq	%r8, %rax
	jne	.LBB12_980
.LBB12_985:                             # %if.then154
                                        #   in Loop: Header=BB12_978 Depth=2
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str4, %rsi
	callq	error_for_asm
	movq	%rbx, %rdi
	callq	delete_insn
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	24(%rbx), %rbx
	jmp	.LBB12_978
.LBB12_23:                              # %if.end161
                                        #   in Loop: Header=BB12_4 Depth=1
	movl	num_eliminable(%rip), %eax
	testq	%rax, %rax
	je	.LBB12_30
# BB#24:                                # %land.lhs.true163
                                        #   in Loop: Header=BB12_4 Depth=1
	movzbl	262(%r14), %eax
	testq	$4, %rax
	je	.LBB12_30
# BB#25:                                # %if.then170
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	reg_eliminate(%rip), %rax
	xorq	%rcx, %rcx
	movl	%ecx, num_not_at_initial_offset(%rip)
	leaq	192(%rax), %rcx
	.align	16, 0x90
.LBB12_26:                              # %for.body.i278
                                        #   Parent Loop BB12_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	20(%rax), %edx
	movl	%edx, 24(%rax)
	movl	12(%rax), %esi
	testq	%rsi, %rsi
	je	.LBB12_29
# BB#27:                                # %land.lhs.true.i281
                                        #   in Loop: Header=BB12_26 Depth=2
	movl	8(%rax), %esi
	andq	%r8, %rdx
	cmpq	%rsi, %rdx
	je	.LBB12_29
# BB#28:                                # %if.then.i283
                                        #   in Loop: Header=BB12_26 Depth=2
	movl	num_not_at_initial_offset(%rip), %edx
	incq	%rdx
	movl	%edx, num_not_at_initial_offset(%rip)
.LBB12_29:                              # %for.inc.i286
                                        #   in Loop: Header=BB12_26 Depth=2
	addq	$48, %rax
	cmpq	%rcx, %rax
	jb	.LBB12_26
.LBB12_30:                              # %if.end171
                                        #   in Loop: Header=BB12_4 Depth=1
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movabsq	$forget_old_reloads_1, %rsi
	movq	%r8, %rbx
	callq	note_stores
	movq	%rbx, %r8
	movq	24(%r12), %rbx
	jmp	.LBB12_31
	.align	16, 0x90
.LBB12_35:                              # %for.inc196
                                        #   in Loop: Header=BB12_31 Depth=2
	movq	24(%rbx), %rbx
.LBB12_31:                              # %if.end171
                                        #   Parent Loop BB12_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r13, %rbx
	je	.LBB12_6
# BB#32:                                # %for.body178
                                        #   in Loop: Header=BB12_31 Depth=2
	movzbl	(%rbx), %eax
	cmpq	$32, %rax
	jne	.LBB12_35
# BB#33:                                # %land.lhs.true183
                                        #   in Loop: Header=BB12_31 Depth=2
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$49, %rax
	jne	.LBB12_35
# BB#34:                                # %if.then191
                                        #   in Loop: Header=BB12_31 Depth=2
	xorq	%rdx, %rdx
	movabsq	$forget_old_reloads_1, %rsi
	movq	%r8, %r15
	callq	note_stores
	movq	%r15, %r8
	jmp	.LBB12_35
	.align	16, 0x90
.LBB12_6:                               # %if.end202
                                        #   in Loop: Header=BB12_4 Depth=1
	movzbl	(%r12), %eax
	cmpq	$34, %rax
	jne	.LBB12_7
# BB#986:                               # %if.then213
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	call_used_reg_set(%rip), %rax
	notq	%rax
	andq	%rax, reg_reloaded_valid(%rip)
	jmp	.LBB12_987
	.align	16, 0x90
.LBB12_7:                               # %if.end202
                                        #   in Loop: Header=BB12_4 Depth=1
	cmpq	$36, %rax
	jne	.LBB12_987
# BB#8:                                 # %if.then207
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	$0, reg_reloaded_valid(%rip)
.LBB12_987:                             # %for.inc216
                                        #   in Loop: Header=BB12_4 Depth=1
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB12_4
.LBB12_988:                             # %for.end218
	movq	reg_last_reload_reg(%rip), %rdi
	callq	free
	movq	reg_has_output_reload(%rip), %rdi
	callq	free
	addq	$1696, %rsp             # imm = 0x6A0
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB12_511:                             # %sw.default.i.i.i
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.emit_input_reload_insns, %rdx
	movq	$6330, %rsi             # imm = 0x18BA
	callq	fancy_abort
.LBB12_176:                             # %if.else852.i
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.choose_reload_regs, %rcx
	movq	$5670, %rsi             # imm = 0x1626
	jmp	.LBB12_177
.LBB12_281:                             # %if.then1204.i
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.choose_reload_regs, %rcx
	movq	$5862, %rsi             # imm = 0x16E6
	jmp	.LBB12_177
.LBB12_989:                             # %if.then1233.i
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.choose_reload_regs, %rcx
	movq	$5869, %rsi             # imm = 0x16ED
	jmp	.LBB12_177
.LBB12_291:                             # %if.then.i2165.i
	movabsq	$.L.str8, %rax
	movabsq	$.L.str1, %rdx
	movabsq	$.L__FUNCTION__.failed_reload, %r8
	movq	$5051, %rcx             # imm = 0x13BB
	jmp	.LBB12_292
.LBB12_377:                             # %if.then1709.i
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.choose_reload_regs, %rcx
	movq	$5985, %rsi             # imm = 0x1761
	jmp	.LBB12_177
.LBB12_516:                             # %if.then200.i.i.i
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.emit_input_reload_insns, %rcx
	movq	$6342, %rsi             # imm = 0x18C6
	jmp	.LBB12_177
.LBB12_697:                             # %if.then95.i.i
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.do_output_reload, %rcx
	movq	$6967, %rsi             # imm = 0x1B37
.LBB12_177:                             # %if.else852.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB12_703:                             # %if.then6.i.i.i
	movabsq	$.L.str6, %rax
	movabsq	$.L.str1, %rdx
	movabsq	$.L__FUNCTION__.emit_output_reload_insns, %r8
	movq	$6651, %rcx             # imm = 0x19FB
.LBB12_292:                             # %if.then.i2165.i
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	_fatal_insn
.Ltmp180:
	.size	reload_as_needed, .Ltmp180-reload_as_needed
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI12_0:
	.quad	.LBB12_512
	.quad	.LBB12_511
	.quad	.LBB12_511
	.quad	.LBB12_504
	.quad	.LBB12_505
	.quad	.LBB12_506
	.quad	.LBB12_507
	.quad	.LBB12_508
	.quad	.LBB12_509
	.quad	.LBB12_513
	.quad	.LBB12_510

	.text
	.align	16, 0x90
	.type	replace_pseudos_in_call_usage,@function
replace_pseudos_in_call_usage:          # @replace_pseudos_in_call_usage
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp187:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp188:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp189:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp190:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp191:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp192:
	.cfi_def_cfa_offset 80
.Ltmp193:
	.cfi_offset %rbx, -48
.Ltmp194:
	.cfi_offset %r12, -40
.Ltmp195:
	.cfi_offset %r13, -32
.Ltmp196:
	.cfi_offset %r14, -24
.Ltmp197:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	(%rbx), %r12
	movq	%rdx, %r14
	movq	%rsi, %r15
	jmp	.LBB13_1
	.align	16, 0x90
.LBB13_6:                               # %if.then6
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	%r12, (%rbx)
.LBB13_1:                               # %entry
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_2 Depth 2
	testq	%r12, %r12
	je	.LBB13_26
.LBB13_2:                               # %if.end
                                        #   Parent Loop BB13_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB13_3
# BB#17:                                # %if.then55
                                        #   in Loop: Header=BB13_2 Depth=2
	movq	$16, %rcx
	movq	%r12, %rbx
	movq	8(%r12), %r12
	shrq	%cl, %rax
	andq	$255, %rax
	addq	$8, %rbx
	testq	%r12, %r12
	movq	%rax, %r15
	jne	.LBB13_2
	jmp	.LBB13_26
	.align	16, 0x90
.LBB13_3:                               # %if.end
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$61, %rcx
	jne	.LBB13_18
# BB#4:                                 # %if.then1
                                        #   in Loop: Header=BB13_1 Depth=1
	movl	8(%r12), %r13d
	cmpq	$53, %r13
	jb	.LBB13_26
# BB#5:                                 # %if.end4
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	eliminate_regs
	movq	(%rbx), %rcx
	movq	%rax, %r12
	cmpq	%rcx, %r12
	jne	.LBB13_6
# BB#7:                                 # %if.end7
	movq	reg_equiv_constant(%rip), %rax
	movq	(%rax,%r13,8), %rax
	testq	%rax, %rax
	je	.LBB13_9
# BB#8:                                 # %if.then10
	movq	%rax, (%rbx)
	jmp	.LBB13_26
.LBB13_18:                              # %if.end62
	movzbl	rtx_length(%rcx), %eax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	testq	%rax, %rax
	je	.LBB13_26
# BB#19:                                # %for.body.lr.ph
	movq	rtx_format(,%rcx,8), %rdi
	xorq	%r13, %r13
	.align	16, 0x90
.LBB13_20:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_24 Depth 2
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movzbl	(%rdi), %eax
	cmpq	$101, %rax
	jne	.LBB13_21
# BB#27:                                # %if.then72
                                        #   in Loop: Header=BB13_20 Depth=1
	leaq	8(%r12,%r13,8), %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	replace_pseudos_in_call_usage
	movq	%r14, %rdx
	movq	%r15, %rsi
	jmp	.LBB13_25
	.align	16, 0x90
.LBB13_21:                              # %for.body
                                        #   in Loop: Header=BB13_20 Depth=1
	cmpq	$69, %rax
	movq	%r14, %rdx
	movq	%r15, %rsi
	jne	.LBB13_25
# BB#22:                                # %for.cond82.preheader
                                        #   in Loop: Header=BB13_20 Depth=1
	movq	8(%r12,%r13,8), %rdi
	movslq	(%rdi), %rax
	testq	%rax, %rax
	jle	.LBB13_25
# BB#23:                                #   in Loop: Header=BB13_20 Depth=1
	movq	$8, %r14
	movq	$1, %r15
	.align	16, 0x90
.LBB13_24:                              # %for.body88
                                        #   Parent Loop BB13_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r12, 24(%rsp)          # 8-byte Spill
	addq	%r14, %rdi
	movq	%r13, %r12
	movq	%rdx, %r13
	movq	%rsi, %rbx
	callq	replace_pseudos_in_call_usage
	movq	%rbx, %rsi
	movq	%r13, %rdx
	movq	%r12, %r13
	movq	24(%rsp), %r12          # 8-byte Reload
	movq	8(%r12,%r13,8), %rdi
	movslq	(%rdi), %rax
	movq	$32, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	incq	%r15
	addq	$8, %r14
	cmpq	%rax, %rbx
	jl	.LBB13_24
	.align	16, 0x90
.LBB13_25:                              # %for.inc97
                                        #   in Loop: Header=BB13_20 Depth=1
	incq	%r13
	movq	16(%rsp), %rdi          # 8-byte Reload
	incq	%rdi
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	%rcx, %rax
	movq	%rdx, %r14
	movq	%rsi, %r15
	jl	.LBB13_20
	jmp	.LBB13_26
.LBB13_9:                               # %if.else
	movq	reg_equiv_mem(%rip), %rax
	movq	(%rax,%r13,8), %rax
	testq	%rax, %rax
	je	.LBB13_11
# BB#10:                                # %if.then16
	movq	%rax, (%rbx)
	jmp	.LBB13_26
.LBB13_11:                              # %if.else19
	movq	reg_equiv_address(%rip), %rax
	movq	(%rax,%r13,8), %rsi
	testq	%rsi, %rsi
	je	.LBB13_13
# BB#12:                                # %if.then23
	movzbl	2(%r12), %edi
	callq	gen_rtx_MEM
	movq	%rax, (%rbx)
	jmp	.LBB13_26
.LBB13_13:                              # %if.else29
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	(%rax,%r13,8), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB13_15
# BB#14:                                # %lor.lhs.false
	movl	8(%rax), %ecx
	cmpq	%r13, %rcx
	je	.LBB13_16
.LBB13_15:                              # %if.then43
	movq	%rax, (%rbx)
.LBB13_26:                              # %for.end99
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB13_16:                              # %if.else48
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.replace_pseudos_in_call_usage, %rcx
	movq	$627, %rsi              # imm = 0x273
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp198:
	.size	replace_pseudos_in_call_usage, .Ltmp198-replace_pseudos_in_call_usage
	.cfi_endproc

	.globl	fixup_abnormal_edges
	.align	16, 0x90
	.type	fixup_abnormal_edges,@function
fixup_abnormal_edges:                   # @fixup_abnormal_edges
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp205:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp206:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp207:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp208:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp209:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp210:
	.cfi_def_cfa_offset 64
.Ltmp211:
	.cfi_offset %rbx, -48
.Ltmp212:
	.cfi_offset %r12, -40
.Ltmp213:
	.cfi_offset %r13, -32
.Ltmp214:
	.cfi_offset %r14, -24
.Ltmp215:
	.cfi_offset %r15, -16
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB14_30
# BB#1:
	xorq	%r15, %r15
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	$0, (%rsp)              # 8-byte Folded Spill
	.align	16, 0x90
.LBB14_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB14_4 Depth 2
                                        #     Child Loop BB14_11 Depth 2
                                        #     Child Loop BB14_14 Depth 2
                                        #     Child Loop BB14_24 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r15,8), %r14
	movq	40(%r14), %rax
	jmp	.LBB14_4
	.align	16, 0x90
.LBB14_3:                               # %for.cond2
                                        #   in Loop: Header=BB14_4 Depth=2
	movq	8(%rax), %rax
.LBB14_4:                               # %for.cond2
                                        #   Parent Loop BB14_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rax, %rax
	je	.LBB14_28
# BB#5:                                 # %for.body3
                                        #   in Loop: Header=BB14_4 Depth=2
	movl	48(%rax), %ecx
	testq	$4, %rcx
	jne	.LBB14_7
# BB#6:                                 # %for.body3
                                        #   in Loop: Header=BB14_4 Depth=2
	andq	$10, %rcx
	andq	%r13, %rcx
	cmpq	$10, %rcx
	jne	.LBB14_3
.LBB14_7:                               # %land.lhs.true
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%r14), %rdi
	movzbl	(%rdi), %eax
	cmpq	$34, %rax
	je	.LBB14_28
# BB#8:                                 # %land.lhs.true12
                                        #   in Loop: Header=BB14_2 Depth=1
	callq	can_throw_internal
	testq	%rax, %rax
	jne	.LBB14_28
# BB#9:                                 # %if.then14
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%r14), %rbx
	movq	40(%r14), %rax
	movq	24(%rbx), %r12
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	jmp	.LBB14_11
	.align	16, 0x90
.LBB14_10:                              # %for.cond19
                                        #   in Loop: Header=BB14_11 Depth=2
	movq	8(%rax), %rax
.LBB14_11:                              # %for.cond19
                                        #   Parent Loop BB14_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rax, %rax
	je	.LBB14_14
# BB#12:                                # %for.body21
                                        #   in Loop: Header=BB14_11 Depth=2
	movzbl	48(%rax), %ecx
	testq	$1, %rcx
	je	.LBB14_10
# BB#13:                                #   in Loop: Header=BB14_2 Depth=1
	movq	%rax, 8(%rsp)           # 8-byte Spill
	jmp	.LBB14_14
	.align	16, 0x90
.LBB14_18:                              # %while.body
                                        #   in Loop: Header=BB14_14 Depth=2
	movq	16(%rbx), %rbx
.LBB14_14:                              # %while.cond
                                        #   Parent Loop BB14_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rbx), %eax
	cmpq	$32, %rax
	je	.LBB14_16
# BB#15:                                # %while.cond
                                        #   in Loop: Header=BB14_14 Depth=2
	cmpq	$37, %rax
	jne	.LBB14_19
.LBB14_16:                              # %land.lhs.true36
                                        #   in Loop: Header=BB14_14 Depth=2
	movq	%rbx, %rdi
	callq	can_throw_internal
	testq	%rax, %rax
	jne	.LBB14_19
# BB#17:                                # %land.rhs
                                        #   in Loop: Header=BB14_14 Depth=2
	movq	(%r14), %rax
	cmpq	%rax, %rbx
	jne	.LBB14_18
.LBB14_19:                              # %while.end
                                        #   in Loop: Header=BB14_2 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	je	.LBB14_21
# BB#20:                                # %land.lhs.true45
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	can_throw_internal
	testq	%rax, %rax
	je	.LBB14_31
.LBB14_21:                              # %if.end48
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, 8(%r14)
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB14_22
# BB#23:                                # %if.end48
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	$1, (%rsp)              # 8-byte Folded Spill
	cmpq	%r12, %rbx
	je	.LBB14_28
	.align	16, 0x90
.LBB14_24:                              # %while.body58
                                        #   Parent Loop BB14_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	movq	24(%rbx), %r14
	cmpq	$105, %rax
	jne	.LBB14_26
# BB#25:                                # %if.then68
                                        #   in Loop: Header=BB14_24 Depth=2
	movq	%rbx, %rdi
	callq	delete_insn
	movl	(%rbx), %eax
	movabsq	$4160749567, %rcx       # imm = 0xF7FFFFFF
	andq	%rcx, %rax
	movl	%eax, (%rbx)
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	gen_rtvec
	movq	$24, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	insert_insn_on_edge
.LBB14_26:                              # %while.cond53.backedge
                                        #   in Loop: Header=BB14_24 Depth=2
	testq	%r14, %r14
	je	.LBB14_28
# BB#27:                                # %while.cond53.backedge
                                        #   in Loop: Header=BB14_24 Depth=2
	cmpq	%r12, %r14
	movq	%r14, %rbx
	jne	.LBB14_24
	jmp	.LBB14_28
.LBB14_22:                              #   in Loop: Header=BB14_2 Depth=1
	movq	$1, (%rsp)              # 8-byte Folded Spill
	.align	16, 0x90
.LBB14_28:                              # %for.inc77
                                        #   in Loop: Header=BB14_2 Depth=1
	incq	%r15
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB14_2
# BB#29:                                # %for.end78
	testq	$1, (%rsp)              # 8-byte Folded Reload
	je	.LBB14_30
# BB#32:                                # %if.then80
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	commit_edge_insertions  # TAILCALL
.LBB14_30:                              # %if.end81
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB14_31:                              # %if.then47
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.fixup_abnormal_edges, %rcx
	movq	$9527, %rsi             # imm = 0x2537
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp216:
	.size	fixup_abnormal_edges, .Ltmp216-fixup_abnormal_edges
	.cfi_endproc

	.globl	reloads_conflict
	.align	16, 0x90
	.type	reloads_conflict,@function
reloads_conflict:                       # @reloads_conflict
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp218:
	.cfi_def_cfa_offset 16
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	imulq	$104, %rsi, %rsi
	movl	rld+92(%rsi), %ecx
	cmpq	$9, %rcx
	movq	$1, %rax
	je	.LBB15_56
# BB#1:                                 # %if.end
	imulq	$104, %rdi, %rdx
	movl	rld+92(%rdx), %edi
	cmpq	$10, %rdi
	ja	.LBB15_57
# BB#2:                                 # %if.end
	movl	rld+72(%rdx), %edx
	movl	rld+72(%rsi), %esi
	jmpq	*.LJTI15_0(,%rdi,8)
.LBB15_3:                               # %sw.bb
	cmpq	$8, %rcx
	ja	.LBB15_5
# BB#4:                                 # %sw.bb
	movq	$1, %rax
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	$389, %rdi              # imm = 0x185
	jne	.LBB15_56
.LBB15_5:                               # %lor.rhs
	addq	$-3, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$2, %rax
	sbbq	%rax, %rax
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jle	.LBB15_6
# BB#7:                                 # %lor.rhs
	movabsq	$1, %rcx
	andq	%rcx, %rax
	popq	%rdx
	retq
.LBB15_44:                              # %sw.bb76
	leaq	-1(%rcx), %rax
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rax
	cmpq	$2, %rax
	movq	$1, %rax
	jb	.LBB15_56
# BB#45:                                # %lor.rhs80
	addq	$-5, %rcx
	andq	%rdi, %rcx
	cmpq	$2, %rcx
	sbbq	%rax, %rax
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB15_46
# BB#47:                                # %lor.rhs80
	movabsq	$1, %rcx
	andq	%rcx, %rax
	popq	%rdx
	retq
.LBB15_49:                              # %sw.bb89
	cmpq	$3, %rcx
	sbbq	%rax, %rax
	andq	$1, %rax
	cmpq	$7, %rcx
	jne	.LBB15_50
# BB#51:                                # %sw.bb89
	movabsq	$1, %rcx
	orq	%rcx, %rax
	popq	%rdx
	retq
.LBB15_9:                               # %sw.bb19
	cmpq	$3, %rcx
	jne	.LBB15_11
# BB#10:                                # %sw.bb19
	movq	$1, %rax
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	%rsi, %r8
	andq	%rdi, %r8
	andq	%rdx, %rdi
	cmpq	%r8, %rdi
	je	.LBB15_56
.LBB15_11:                              # %lor.rhs22
	testq	%rcx, %rcx
	movabsq	$0, %rdi
	movabsq	$1, %r8
	movq	%rdi, %rax
	jne	.LBB15_13
# BB#12:                                # %lor.rhs22
	movq	%r8, %rax
.LBB15_13:                              # %lor.rhs22
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jge	.LBB15_15
# BB#14:                                # %lor.rhs22
	movq	%r8, %rdi
.LBB15_15:                              # %lor.rhs22
	andq	%rdi, %rax
	popq	%rdx
	retq
.LBB15_16:                              # %sw.bb29
	cmpq	$4, %rcx
	jne	.LBB15_18
# BB#17:                                # %sw.bb29
	movq	$1, %rax
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	%rsi, %r8
	andq	%rdi, %r8
	andq	%rdx, %rdi
	cmpq	%r8, %rdi
	je	.LBB15_56
.LBB15_18:                              # %lor.rhs33
	testq	%rcx, %rcx
	movabsq	$0, %rdi
	movabsq	$1, %r8
	movq	%rdi, %rax
	jne	.LBB15_20
# BB#19:                                # %lor.rhs33
	movq	%r8, %rax
.LBB15_20:                              # %lor.rhs33
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jge	.LBB15_22
# BB#21:                                # %lor.rhs33
	movq	%r8, %rdi
.LBB15_22:                              # %lor.rhs33
	andq	%rdi, %rax
	popq	%rdx
	retq
.LBB15_23:                              # %sw.bb40
	cmpq	$5, %rcx
	jne	.LBB15_25
# BB#24:                                # %sw.bb40
	movq	$1, %rax
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	%rdx, %r8
	andq	%rdi, %r8
	andq	%rsi, %rdi
	cmpq	%r8, %rdi
	je	.LBB15_56
.LBB15_25:                              # %lor.rhs44
	cmpq	$1, %rcx
	movabsq	$0, %rdi
	movabsq	$1, %r8
	movq	%rdi, %rax
	jne	.LBB15_27
# BB#26:                                # %lor.rhs44
	movq	%r8, %rax
.LBB15_27:                              # %lor.rhs44
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jg	.LBB15_29
# BB#28:                                # %lor.rhs44
	movq	%r8, %rdi
.LBB15_29:                              # %lor.rhs44
	andq	%rdi, %rax
	popq	%rdx
	retq
.LBB15_30:                              # %sw.bb51
	cmpq	$6, %rcx
	jne	.LBB15_32
# BB#31:                                # %sw.bb51
	movq	$1, %rax
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	%rdx, %r8
	andq	%rdi, %r8
	andq	%rsi, %rdi
	cmpq	%r8, %rdi
	je	.LBB15_56
.LBB15_32:                              # %lor.rhs55
	cmpq	$1, %rcx
	movabsq	$0, %rdi
	movabsq	$1, %r8
	movq	%rdi, %rax
	jne	.LBB15_34
# BB#33:                                # %lor.rhs55
	movq	%r8, %rax
.LBB15_34:                              # %lor.rhs55
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jg	.LBB15_36
# BB#35:                                # %lor.rhs55
	movq	%r8, %rdi
.LBB15_36:                              # %lor.rhs55
	andq	%rdi, %rax
	popq	%rdx
	retq
.LBB15_37:                              # %sw.bb62
	movabsq	$4294967293, %rax       # imm = 0xFFFFFFFD
	testq	%rax, %rcx
	movabsq	$0, %rdx
	movabsq	$1, %rsi
	movq	%rdx, %rax
	jne	.LBB15_39
# BB#38:                                # %sw.bb62
	movq	%rsi, %rax
.LBB15_39:                              # %sw.bb62
	cmpq	$7, %rcx
	jne	.LBB15_41
# BB#40:                                # %sw.bb62
	movq	%rsi, %rdx
.LBB15_41:                              # %sw.bb62
	orq	%rdx, %rax
	popq	%rdx
	retq
.LBB15_42:                              # %sw.bb70
	movabsq	$4294967287, %rax       # imm = 0xFFFFFFF7
	testq	%rax, %rcx
	je	.LBB15_55
# BB#43:
	movabsq	$0, %rax
	popq	%rdx
	retq
.LBB15_53:                              # %sw.bb99
	cmpq	$10, %rcx
	jne	.LBB15_54
.LBB15_55:                              # %sw.bb99
	movabsq	$1, %rax
.LBB15_56:                              # %return
	popq	%rdx
	retq
.LBB15_57:                              # %sw.default
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.reloads_conflict, %rdx
	movq	$4739, %rsi             # imm = 0x1283
	callq	fancy_abort
.LBB15_50:
	movabsq	$0, %rcx
	orq	%rcx, %rax
	popq	%rdx
	retq
.LBB15_54:
	movabsq	$0, %rax
	popq	%rdx
	retq
.LBB15_6:
	movabsq	$0, %rcx
	andq	%rcx, %rax
	popq	%rdx
	retq
.LBB15_46:
	movabsq	$0, %rcx
	andq	%rcx, %rax
	popq	%rdx
	retq
.Ltmp219:
	.size	reloads_conflict, .Ltmp219-reloads_conflict
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI15_0:
	.quad	.LBB15_3
	.quad	.LBB15_44
	.quad	.LBB15_49
	.quad	.LBB15_9
	.quad	.LBB15_16
	.quad	.LBB15_23
	.quad	.LBB15_30
	.quad	.LBB15_37
	.quad	.LBB15_42
	.quad	.LBB15_56
	.quad	.LBB15_53

	.text
	.globl	deallocate_reload_reg
	.align	16, 0x90
	.type	deallocate_reload_reg,@function
deallocate_reload_reg:                  # @deallocate_reload_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp224:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp225:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp226:
	.cfi_def_cfa_offset 32
.Ltmp227:
	.cfi_offset %rbx, -32
.Ltmp228:
	.cfi_offset %r14, -24
.Ltmp229:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	imulq	$104, %rbx, %r14
	movq	rld+64(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB16_4
# BB#1:                                 # %if.end
	leaq	rld+64(%r14), %r15
	callq	true_regnum
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movswq	spill_reg_order(%rdx,%rdx), %rcx
	movq	$0, (%r15)
	testq	%rcx, %rcx
	js	.LBB16_3
# BB#2:                                 # %if.then10
	movl	rld+28(%r14), %ecx
	movl	rld+92(%r14), %edx
	movl	rld+72(%r14), %esi
	movq	%rax, %rdi
	callq	clear_reload_reg_in_use
.LBB16_3:                               # %if.end17
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, reload_spill_index(,%rbx,4)
.LBB16_4:                               # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp230:
	.size	deallocate_reload_reg, .Ltmp230-deallocate_reload_reg
	.cfi_endproc

	.align	16, 0x90
	.type	clear_reload_reg_in_use,@function
clear_reload_reg_in_use:                # @clear_reload_reg_in_use
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
	subq	$48, %rsp
.Ltmp242:
	.cfi_def_cfa_offset 96
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
	movq	%rcx, %rax
	movq	%rdx, %r15
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	movq	%rdi, %rcx
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	je	.LBB17_4
# BB#1:                                 # %entry
	leaq	-21(%rdi), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB17_4
# BB#2:                                 # %entry
	leaq	-45(%rdi), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB17_4
# BB#3:                                 # %entry
	movq	%rdi, %rdx
	addq	$-29, %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB17_4
# BB#9:                                 # %cond.false
	andq	%rax, %rcx
	cmpq	$24, %rcx
	jne	.LBB17_10
# BB#12:                                # %cond.true22
	movzbl	target_flags+3(%rip), %ebx
	andq	$2, %rbx
	xorq	$6, %rbx
	jmp	.LBB17_14
.LBB17_4:                               # %cond.true
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %rbx
	je	.LBB17_14
# BB#5:                                 # %lor.rhs
	cmpq	$6, %rax
	jne	.LBB17_6
# BB#7:                                 # %lor.rhs
	movabsq	$1, %rbx
	incq	%rbx
	jmp	.LBB17_14
.LBB17_10:                              # %cond.false
	cmpq	$18, %rcx
	jne	.LBB17_13
# BB#11:                                # %cond.true18
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	xorq	$3, %rbx
	jmp	.LBB17_14
.LBB17_6:
	movabsq	$0, %rbx
	incq	%rbx
	jmp	.LBB17_14
.LBB17_13:                              # %cond.false26
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movl	target_flags(%rip), %ebx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
	movq	%rax, %rbx
.LBB17_14:                              # %cond.end38
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %r15
	cmpq	$10, %r15
	ja	.LBB17_23
# BB#15:                                # %cond.end38
	movabsq	$reload_reg_used_in_op_addr_reload, %r9
	movq	$1, %rcx
	movabsq	$reload_reg_used, %rax
	jmpq	*.LJTI17_0(,%r15,8)
.LBB17_20:                              # %sw.bb55
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	leaq	reload_reg_used_in_input(,%rsi,8), %rax
	jmp	.LBB17_36
.LBB17_21:                              # %sw.bb58
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	leaq	reload_reg_used_in_output(,%rsi,8), %rax
	jmp	.LBB17_36
.LBB17_22:                              # %sw.bb61
	movabsq	$reload_reg_used_in_insn, %rax
	jmp	.LBB17_36
.LBB17_35:                              # %sw.bb40
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	leaq	reload_reg_used_in_input_addr(,%rsi,8), %rax
	jmp	.LBB17_36
.LBB17_24:                              # %sw.bb43
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	reload_reg_used_in_inpaddr_addr(,%rax,8), %r9
	jmp	.LBB17_25
.LBB17_16:                              # %sw.bb46
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	leaq	reload_reg_used_in_output_addr(,%rsi,8), %rax
	jmp	.LBB17_36
.LBB17_17:                              # %sw.bb49
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	reload_reg_used_in_outaddr_addr(,%rax,8), %r9
.LBB17_25:                              # %sw.epilog.thread
	xorq	%rcx, %rcx
	jmp	.LBB17_26
.LBB17_18:                              # %sw.bb52
	movabsq	$reload_reg_used_in_op_addr, %rax
.LBB17_36:                              # %sw.epilog
	addq	%rdi, %rbx
	movq	%rax, %r9
	jmp	.LBB17_57
.LBB17_19:                              # %sw.bb54
	movabsq	$reload_reg_used_in_other_addr, %r9
.LBB17_26:                              # %sw.epilog.thread
	movq	%r9, 24(%rsp)           # 8-byte Spill
	movslq	n_reloads(%rip), %r13
	addq	%rdi, %rbx
	testq	%r13, %r13
	jle	.LBB17_57
# BB#27:
	movq	%rcx, 32(%rsp)          # 8-byte Spill
.LBB17_28:                              # %for.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB17_29 Depth 2
	movq	%rdi, %r8
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	imulq	$104, %r13, %rax
	leaq	rld-12(%rax), %r14
	decq	%r13
	movq	32(%rsp), %rdx          # 8-byte Reload
	.align	16, 0x90
.LBB17_29:                              # %for.body
                                        #   Parent Loop BB17_28 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%r14), %eax
	cmpq	%r15, %rax
	jne	.LBB17_33
# BB#30:                                # %land.lhs.true73
                                        #   in Loop: Header=BB17_29 Depth=2
	testq	%rdx, %rdx
	jne	.LBB17_32
# BB#31:                                # %lor.lhs.false75
                                        #   in Loop: Header=BB17_29 Depth=2
	movl	-20(%r14), %eax
	movq	%rsi, %rcx
	andq	%r12, %rcx
	cmpq	%rcx, %rax
	jne	.LBB17_33
.LBB17_32:                              # %land.lhs.true81
                                        #   in Loop: Header=BB17_29 Depth=2
	movq	-28(%r14), %rdi
	testq	%rdi, %rdi
	jne	.LBB17_37
.LBB17_33:                              # %for.cond.backedge
                                        #   in Loop: Header=BB17_29 Depth=2
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$-104, %r14
	decq	%r13
	testq	%rax, %rax
	jg	.LBB17_29
	jmp	.LBB17_34
.LBB17_37:                              # %if.then85
                                        #   in Loop: Header=BB17_28 Depth=1
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	callq	true_regnum
	movslq	-64(%r14), %rcx
	movq	%rax, %rbx
                                        # kill: RAX<def> RBX<kill>
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rax
	cmpq	$8, %rax
	je	.LBB17_41
# BB#38:                                # %if.then85
                                        #   in Loop: Header=BB17_28 Depth=1
	leaq	-21(%rbx), %rax
	andq	%r12, %rax
	cmpq	$8, %rax
	jb	.LBB17_41
# BB#39:                                # %if.then85
                                        #   in Loop: Header=BB17_28 Depth=1
	leaq	-45(%rbx), %rax
	andq	%r12, %rax
	cmpq	$8, %rax
	jb	.LBB17_41
# BB#40:                                # %if.then85
                                        #   in Loop: Header=BB17_28 Depth=1
	movq	%rbx, %rax
	addq	$-29, %rax
	andq	%r12, %rax
	cmpq	$7, %rax
	jbe	.LBB17_41
# BB#45:                                # %cond.false130
                                        #   in Loop: Header=BB17_28 Depth=1
	movq	%rcx, %rax
	andq	%r12, %rax
	cmpq	$24, %rax
	movq	8(%rsp), %r10           # 8-byte Reload
	jne	.LBB17_46
# BB#48:                                # %cond.true146
                                        #   in Loop: Header=BB17_28 Depth=1
	movzbl	target_flags+3(%rip), %edi
	andq	$2, %rdi
	xorq	$6, %rdi
	jmp	.LBB17_50
.LBB17_41:                              # %cond.true112
                                        #   in Loop: Header=BB17_28 Depth=1
	movl	mode_class(,%rcx,4), %eax
	cmpq	$5, %rax
	movq	$2, %rdi
	movq	8(%rsp), %r10           # 8-byte Reload
	je	.LBB17_50
# BB#42:                                # %lor.rhs120
                                        #   in Loop: Header=BB17_28 Depth=1
	cmpq	$6, %rax
	movabsq	$0, %rdi
	jne	.LBB17_44
# BB#43:                                # %lor.rhs120
                                        #   in Loop: Header=BB17_28 Depth=1
	movabsq	$1, %rdi
.LBB17_44:                              # %lor.rhs120
                                        #   in Loop: Header=BB17_28 Depth=1
	incq	%rdi
	jmp	.LBB17_50
.LBB17_46:                              # %cond.false130
                                        #   in Loop: Header=BB17_28 Depth=1
	cmpq	$18, %rax
	jne	.LBB17_49
# BB#47:                                # %cond.true136
                                        #   in Loop: Header=BB17_28 Depth=1
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	xorq	$3, %rdi
	jmp	.LBB17_50
.LBB17_49:                              # %cond.false150
                                        #   in Loop: Header=BB17_28 Depth=1
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rcx), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	movq	%rax, %rdi
.LBB17_50:                              # %cond.end170
                                        #   in Loop: Header=BB17_28 Depth=1
	movq	$32, %rcx
	movq	%r13, %r8
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	addq	%rbx, %rdi
	movq	%rdi, %rdx
	andq	%r12, %rdx
	movq	%rbx, %rcx
	andq	%r12, %rcx
	movq	%r10, %rax
	andq	%r12, %rax
	cmpq	%rax, %rcx
	movabsq	$0, %r9
	movq	16(%rsp), %rsi          # 8-byte Reload
	ja	.LBB17_52
# BB#51:                                # %cond.end170
                                        #   in Loop: Header=BB17_28 Depth=1
	movabsq	$1, %r9
.LBB17_52:                              # %cond.end170
                                        #   in Loop: Header=BB17_28 Depth=1
	cmpq	%rdx, %rax
	sbbq	%rax, %rax
	testq	%r9, %rax
	jne	.LBB17_54
# BB#53:                                # %cond.end170
                                        #   in Loop: Header=BB17_28 Depth=1
	movq	%r10, %rdi
.LBB17_54:                              # %cond.end170
                                        #   in Loop: Header=BB17_28 Depth=1
	movq	%rdi, %rax
	andq	%r12, %rax
	cmpq	%rcx, %rax
	sbbq	%rax, %rax
	movq	%rsi, %rdx
	andq	%r12, %rdx
	cmpq	%rdx, %rcx
	sbbq	%rcx, %rcx
	andq	%rax, %rcx
	testq	$1, %rcx
	movq	24(%rsp), %r9           # 8-byte Reload
	jne	.LBB17_56
# BB#55:                                # %cond.end170
                                        #   in Loop: Header=BB17_28 Depth=1
	movq	%rsi, %rbx
.LBB17_56:                              # %cond.end170
                                        #   in Loop: Header=BB17_28 Depth=1
	testq	%r8, %r8
	movq	40(%rsp), %rsi          # 8-byte Reload
	jg	.LBB17_28
	jmp	.LBB17_57
.LBB17_34:
	movq	%r8, %rdi
.LBB17_57:                              # %if.end187
	andq	%r12, %rbx
	movq	%rdi, %rcx
	andq	%r12, %rcx
	cmpq	%rbx, %rcx
	jae	.LBB17_61
# BB#58:                                # %for.body191.lr.ph
	movq	(%r9), %rax
	.align	16, 0x90
.LBB17_59:                              # %for.body191
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rdx
	movq	$1, %rax
	shlq	%cl, %rax
	notq	%rax
	andq	%rdx, %rax
	incq	%rdi
	movq	%rdi, %rdx
	andq	%r12, %rdx
	incq	%rcx
	cmpq	%rbx, %rdx
	jne	.LBB17_59
# BB#60:                                # %for.cond188.for.end194_crit_edge
	movq	%rax, (%r9)
.LBB17_61:                              # %for.end194
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB17_23:                              # %sw.default
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.clear_reload_reg_in_use, %rdx
	movq	$4328, %rsi             # imm = 0x10E8
	callq	fancy_abort
.Ltmp248:
	.size	clear_reload_reg_in_use, .Ltmp248-clear_reload_reg_in_use
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI17_0:
	.quad	.LBB17_20
	.quad	.LBB17_21
	.quad	.LBB17_22
	.quad	.LBB17_35
	.quad	.LBB17_24
	.quad	.LBB17_16
	.quad	.LBB17_17
	.quad	.LBB17_18
	.quad	.LBB17_26
	.quad	.LBB17_36
	.quad	.LBB17_19

	.text
	.globl	gen_reload
	.align	16, 0x90
	.type	gen_reload,@function
gen_reload:                             # @gen_reload
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp255:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp256:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp257:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp258:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp259:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp260:
	.cfi_def_cfa_offset 96
.Ltmp261:
	.cfi_offset %rbx, -48
.Ltmp262:
	.cfi_offset %r12, -40
.Ltmp263:
	.cfi_offset %r13, -32
.Ltmp264:
	.cfi_offset %r14, -24
.Ltmp265:
	.cfi_offset %r15, -16
	movq	%rcx, %r13
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r15
	callq	get_last_insn
	movl	(%rbx), %edx
	movq	%rax, %r14
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB18_4
# BB#1:                                 # %land.lhs.true
	movq	8(%rbx), %rax
	movq	$16, %rcx
	movzbl	2(%rax), %edi
	shrq	%cl, %rdx
	andq	$255, %rdx
	movzbl	mode_size(%rdx), %eax
	movzbl	mode_size(%rdi), %ecx
	cmpq	%rcx, %rax
	jbe	.LBB18_4
# BB#2:                                 # %land.lhs.true12
	movq	%r15, %rsi
	callq	gen_lowpart_common
	testq	%rax, %rax
	je	.LBB18_4
# BB#3:                                 # %if.then
	movq	8(%rbx), %rbx
	movq	%rax, %r15
	jmp	.LBB18_8
.LBB18_4:                               # %if.else
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB18_8
# BB#5:                                 # %land.lhs.true29
	movq	8(%r15), %rdx
	movq	$16, %rcx
	movzbl	2(%rdx), %edi
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %eax
	movzbl	mode_size(%rdi), %ecx
	cmpq	%rcx, %rax
	jbe	.LBB18_8
# BB#6:                                 # %land.lhs.true47
	movq	%rbx, %rsi
	callq	gen_lowpart_common
	testq	%rax, %rax
	je	.LBB18_8
# BB#7:                                 # %if.then57
	movq	8(%r15), %r15
	movq	%rax, %rbx
.LBB18_8:                               # %if.end61
	movl	(%rbx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	je	.LBB18_16
# BB#9:                                 # %if.end61
	cmpq	$75, %rdx
	jne	.LBB18_21
# BB#10:                                # %land.lhs.true66
	movq	8(%rbx), %rcx
	movzbl	(%rcx), %ecx
	addq	$-61, %rcx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %rcx
	cmpq	$5, %rcx
	ja	.LBB18_21
# BB#11:                                # %land.lhs.true66
	movq	%r12, 40(%rsp)          # 8-byte Spill
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$37, %rsi
	je	.LBB18_21
# BB#12:                                # %land.lhs.true89
	movq	%r13, 32(%rsp)          # 8-byte Spill
	movq	16(%rbx), %rcx
	movzbl	(%rcx), %ecx
	leaq	8(%rbx), %rdi
	leaq	16(%rbx), %r13
	cmpq	$133, %rcx
	jg	.LBB18_24
# BB#13:                                # %land.lhs.true89
	addq	$-54, %rcx
	andq	%r8, %rcx
	cmpq	$14, %rcx
	ja	.LBB18_15
# BB#14:                                # %land.lhs.true89
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$29335, %rsi            # imm = 0x7297
	jne	.LBB18_26
	jmp	.LBB18_15
.LBB18_24:                              # %land.lhs.true89
	cmpq	$134, %rcx
	je	.LBB18_26
# BB#25:                                # %land.lhs.true89
	cmpq	$140, %rcx
	jne	.LBB18_15
.LBB18_26:                              # %if.then177
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	callq	find_replacement
	movq	%rax, %r12
	movq	%r13, %rdi
	callq	find_replacement
	movq	(%r13), %rcx
	movzbl	(%rcx), %edx
	movq	%rax, %r13
	cmpq	$61, %rdx
	jne	.LBB18_27
# BB#28:                                # %land.lhs.true194
	movl	8(%r15), %eax
	movl	8(%rcx), %edx
	cmpq	%rdx, %rax
	movq	%r13, %rdx
	je	.LBB18_30
# BB#29:                                # %land.lhs.true194
	movq	%r12, %rdx
.LBB18_30:                              # %land.lhs.true194
	movq	24(%rsp), %rax          # 8-byte Reload
	je	.LBB18_32
# BB#31:                                # %land.lhs.true194
	movq	%r13, %r12
.LBB18_32:                              # %land.lhs.true194
	movq	%r12, %r13
	jmp	.LBB18_33
.LBB18_15:                              # %if.else367
	cmpq	$61, %rdx
	movq	32(%rsp), %r13          # 8-byte Reload
	movq	40(%rsp), %r12          # 8-byte Reload
	jne	.LBB18_21
.LBB18_16:                              # %land.lhs.true372
	movl	8(%rbx), %esi
	cmpq	$52, %rsi
	ja	.LBB18_21
# BB#17:                                # %land.lhs.true378
	movl	(%r15), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB18_21
# BB#18:                                # %land.lhs.true383
	movl	8(%r15), %r8d
	cmpq	$52, %r8
	ja	.LBB18_21
# BB#19:                                # %land.lhs.true389
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movl	regclass_map(,%rsi,4), %edi
	movl	regclass_map(,%r8,4), %esi
	movq	$1, %rcx
	callq	ix86_secondary_memory_needed
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB18_20
# BB#60:                                # %if.then405
	movzbl	2(%r15), %esi
	movq	%rbx, %rdi
	movq	%r12, %rdx
	movq	%r12, 40(%rsp)          # 8-byte Spill
	movq	%r13, %rcx
	callq	get_secondary_mem
	movq	%rax, %r12
	movl	(%r12), %edi
	movzbl	2(%r15), %edx
	movq	$16, %rcx
	movq	%rdi, %rax
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	%rdx, %rax
	je	.LBB18_62
# BB#61:                                # %if.then418
	movl	8(%r15), %esi
	movq	%rax, %rdi
	callq	gen_rtx_REG
	movl	(%r12), %edi
	movq	%rax, %r15
.LBB18_62:                              # %if.end426
	movzbl	2(%rbx), %eax
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	cmpq	%rax, %rdi
	je	.LBB18_64
# BB#63:                                # %if.then435
	movl	8(%rbx), %esi
	callq	gen_rtx_REG
	movq	%rax, %rbx
.LBB18_64:                              # %if.end443
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r13, %rcx
	callq	gen_reload
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	movq	%r13, %rcx
	callq	gen_reload
	jmp	.LBB18_67
.LBB18_27:
	movq	%r12, %rdx
	movq	24(%rsp), %rax          # 8-byte Reload
.LBB18_33:                              # %if.end206
	movq	(%rax), %rax
	cmpq	%rax, %rdx
	jne	.LBB18_36
# BB#34:                                # %if.end206
	cmpq	%rcx, %r13
	jne	.LBB18_36
# BB#35:
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	jmp	.LBB18_37
.LBB18_36:                              # %if.then218
	movzbl	2(%rbx), %esi
	movq	$75, %rdi
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%r13, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
.LBB18_37:                              # %if.end223
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rax, %r12
	movslq	40(%r12), %rax
	testq	%rax, %rax
	jns	.LBB18_39
# BB#38:                                # %cond.end
	movq	%r12, %rdi
	callq	recog_memoized_1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB18_40
.LBB18_39:                              # %if.then236
	movq	%r12, %rdi
	callq	extract_insn
	movq	$1, %rdi
	callq	constrain_operands
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB18_69
.LBB18_40:                              # %if.end240
	movq	%r14, %rdi
	callq	delete_insns_since
	movzbl	2(%r15), %eax
	movq	optab_table(%rip), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movzbl	(%r13), %ecx
	movq	%r13, 16(%rsp)          # 8-byte Spill
	movslq	8(%rdx,%rax), %rax
	cmpq	$133, %rcx
	jg	.LBB18_52
# BB#41:                                # %if.end240
	addq	$-54, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	movq	40(%rsp), %r12          # 8-byte Reload
	ja	.LBB18_54
# BB#42:                                # %if.end240
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$29207, %rdx            # imm = 0x7217
	jne	.LBB18_45
# BB#43:                                # %if.end240
	cmpq	$7, %rcx
	jne	.LBB18_54
# BB#44:                                # %land.lhs.true300
	movq	16(%rsp), %rcx          # 8-byte Reload
	movl	8(%rcx), %ecx
	cmpq	$52, %rcx
	jbe	.LBB18_54
	jmp	.LBB18_45
.LBB18_20:                              # %land.lhs.true389.if.else446_crit_edge
	movl	(%rbx), %eax
.LBB18_21:                              # %if.else446
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %ecx
	cmpq	$111, %rcx
	je	.LBB18_23
# BB#22:                                # %if.else446
	cmpq	$63, %rax
	je	.LBB18_23
# BB#65:                                # %if.else462
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	jmp	.LBB18_66
.LBB18_23:                              # %if.then459
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	gen_move_insn
.LBB18_66:                              # %if.end467
	movq	%rax, %rdi
	callq	emit_insn
.LBB18_67:                              # %if.end467
	testq	%r14, %r14
	je	.LBB18_70
# BB#68:                                # %cond.true469
	movq	24(%r14), %r12
.LBB18_69:                              # %return
	movq	%r12, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB18_70:                              # %cond.false473
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	get_insns  # TAILCALL
.LBB18_52:                              # %if.end240
	cmpq	$134, %rcx
	movq	40(%rsp), %r12          # 8-byte Reload
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	je	.LBB18_45
# BB#53:                                # %if.end240
	cmpq	$140, %rcx
	je	.LBB18_45
.LBB18_54:                              # %lor.lhs.false306
	movq	%rax, %rcx
	andq	%rsi, %rcx
	cmpq	$1317, %rcx             # imm = 0x525
	movq	%rsi, %r13
	movq	24(%rsp), %rcx          # 8-byte Reload
	jne	.LBB18_57
# BB#55:
	movq	%rcx, %r13
	jmp	.LBB18_56
.LBB18_57:                              # %land.lhs.true309
	leaq	(%rax,%rax,4), %rax
	movq	insn_data+24(,%rax,8), %rax
	movzbl	64(%rax), %esi
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	*48(%rax)
	testq	%r13, %rax
	je	.LBB18_45
# BB#58:
	movq	24(%rsp), %r13          # 8-byte Reload
.LBB18_56:                              # %if.end321
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, 24(%rsp)          # 8-byte Spill
	jmp	.LBB18_46
.LBB18_45:                              # %if.then320
	movq	16(%rsp), %r13          # 8-byte Reload
.LBB18_46:                              # %if.end321
	movq	%r13, 16(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	movq	32(%rsp), %rcx          # 8-byte Reload
	callq	gen_reload
	movq	%r13, %rdi
	movq	24(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rsi
	callq	rtx_equal_p
	movq	%r12, %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB18_48
# BB#47:                                # %if.end321
	movq	%r15, %rsi
.LBB18_48:                              # %if.end321
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	callq	gen_add2_insn
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rax, %r12
	movslq	40(%r12), %rax
	testq	%rax, %rax
	jns	.LBB18_50
# BB#49:                                # %cond.end340
	movq	%r12, %rdi
	callq	recog_memoized_1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB18_59
.LBB18_50:                              # %if.then344
	movq	%r12, %rdi
	callq	extract_insn
	movq	$1, %rdi
	callq	constrain_operands
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB18_59
# BB#51:                                # %if.then347
	movq	56(%r12), %rcx
	movq	$3, %rdi
	movq	$3, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%r12)
	jmp	.LBB18_69
.LBB18_59:                              # %if.end356
	movq	%r14, %rdi
	callq	delete_insns_since
	movq	%r15, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	32(%rsp), %rcx          # 8-byte Reload
	callq	gen_reload
	movq	%r15, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	gen_add2_insn
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rax, %r15
	movq	56(%r15), %rcx
	movq	$3, %rdi
	movq	$3, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%r15)
	jmp	.LBB18_67
.Ltmp266:
	.size	gen_reload, .Ltmp266-gen_reload
	.cfi_endproc

	.globl	reload_cse_regs
	.align	16, 0x90
	.type	reload_cse_regs,@function
reload_cse_regs:                        # @reload_cse_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp273:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp274:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp275:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp276:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp277:
	.cfi_def_cfa_offset 48
	subq	$160, %rsp
.Ltmp278:
	.cfi_def_cfa_offset 208
.Ltmp279:
	.cfi_offset %rbx, -48
.Ltmp280:
	.cfi_offset %r12, -40
.Ltmp281:
	.cfi_offset %r13, -32
.Ltmp282:
	.cfi_offset %r14, -24
.Ltmp283:
	.cfi_offset %r15, -16
	movq	%rdi, 128(%rsp)         # 8-byte Spill
	callq	reload_cse_regs_1
	movzbl	double_reg_address_ok(%rip), %eax
	testq	%rax, %rax
	movq	$52, %r15
	jne	.LBB19_139
# BB#1:                                 # %for.cond.preheader.i
	movq	reg_class_contents+80(%rip), %rax
	xorq	%rcx, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%rdx, %rdi
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB19_2:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%rax, %rsi
	je	.LBB19_6
# BB#3:                                 # %if.then3.i
                                        #   in Loop: Header=BB19_2 Depth=1
	movq	%rdi, %rsi
	andq	%rdx, %rsi
	cmpq	%rdx, %rsi
	movq	%rcx, %rsi
	je	.LBB19_5
# BB#4:                                 # %if.then3.i
                                        #   in Loop: Header=BB19_2 Depth=1
	movq	%rdi, %rsi
.LBB19_5:                               # %if.then3.i
                                        #   in Loop: Header=BB19_2 Depth=1
	movq	%rcx, %rbx
	movq	%rsi, %rdi
.LBB19_6:                               # %for.inc.i
                                        #   in Loop: Header=BB19_2 Depth=1
	incq	%rcx
	cmpq	$53, %rcx
	jne	.LBB19_2
# BB#7:                                 # %for.end.i
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rdi, %rax
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	je	.LBB19_139
# BB#8:                                 # %if.end12.i
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movq	%rdi, 96(%rsp)          # 8-byte Spill
	callq	get_first_label_num
	movq	%rax, %rbx
	movq	%rbx, 112(%rsp)         # 8-byte Spill
	callq	max_label_num
	subq	%rbx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	movq	%rax, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rbx
	movq	%rax, 120(%rsp)         # 8-byte Spill
	xorq	%r13, %r13
	testq	%rbx, %rbx
	movabsq	$0, %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	jle	.LBB19_12
# BB#9:                                 # %for.body20.lr.ph.i
	decq	%rbx
	movq	$0, 136(%rsp)           # 8-byte Folded Spill
	leaq	152(%rsp), %r12
	.align	16, 0x90
.LBB19_10:                              # %for.body20.i
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rbx,8), %rax
	movq	(%rax), %r14
	movzbl	(%r14), %ecx
	cmpq	$36, %rcx
	jne	.LBB19_11
# BB#210:                               # %do.body.i
                                        #   in Loop: Header=BB19_10 Depth=1
	movq	$0, 152(%rsp)
	movq	64(%rax), %rsi
	movq	%r12, %rdi
	callq	reg_set_to_hard_reg_set
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rbx,8), %rax
	movq	64(%rax), %rsi
	movq	%r12, %rdi
	callq	compute_use_by_pseudos
	movl	48(%r14), %eax
	movq	152(%rsp), %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	%rdx, (%rcx,%rax,8)
	orq	%rdx, 136(%rsp)         # 8-byte Folded Spill
.LBB19_11:                              # %for.cond17.backedge.i
                                        #   in Loop: Header=BB19_10 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB19_10
.LBB19_12:                              # %for.end39.i
	movl	%r13d, reload_combine_ruid(%rip)
	movq	$-53, %rax
	movabsq	$reg_state+112, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB19_13:                              # %for.body43.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	fixed_regs+53(%rax), %edx
	movl	%r13d, (%rcx)
	testq	%rdx, %rdx
	movq	$6, %rdx
	je	.LBB19_15
# BB#14:                                # %select.mid
                                        #   in Loop: Header=BB19_13 Depth=1
	movq	%rsi, %rdx
.LBB19_15:                              # %select.end
                                        #   in Loop: Header=BB19_13 Depth=1
	movl	%edx, -16(%rcx)
	addq	$120, %rcx
	incq	%rax
	jne	.LBB19_13
# BB#16:                                # %for.end58.i
	callq	get_last_insn
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB19_138
# BB#17:                                # %for.body62.lr.ph.i
	movq	$32, %rcx
	movq	96(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	imulq	$30, %rax, %rax
	leaq	reg_state+112(,%rax,4), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	$0, 104(%rsp)           # 8-byte Folded Spill
	movabsq	$fixed_regs, %rdi
	movq	$6, %r13
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB19_18:                              # %for.body62.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_21 Depth 2
                                        #     Child Loop BB19_78 Depth 2
                                        #     Child Loop BB19_102 Depth 2
                                        #     Child Loop BB19_108 Depth 2
                                        #       Child Loop BB19_109 Depth 3
                                        #     Child Loop BB19_39 Depth 2
                                        #     Child Loop BB19_43 Depth 2
                                        #       Child Loop BB19_118 Depth 3
                                        #     Child Loop BB19_129 Depth 2
                                        #     Child Loop BB19_133 Depth 2
	movl	(%r14), %eax
	movq	%rax, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	movq	$-6360, %rcx            # imm = 0xFFFFFFFFFFFFE728
	cmpq	$35, %rsi
	movq	%rdi, %rdx
	jne	.LBB19_19
	.align	16, 0x90
.LBB19_21:                              # %for.body77.i
                                        #   Parent Loop BB19_18 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rdx), %eax
	testq	%rax, %rax
	jne	.LBB19_23
# BB#22:                                # %if.then81.i
                                        #   in Loop: Header=BB19_21 Depth=2
	movl	%r13d, reg_state+6456(%rcx)
.LBB19_23:                              # %for.inc86.i
                                        #   in Loop: Header=BB19_21 Depth=2
	incq	%rdx
	addq	$120, %rcx
	jne	.LBB19_21
# BB#24:                                # %if.end90.loopexit.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movl	(%r14), %eax
	jmp	.LBB19_25
	.align	16, 0x90
.LBB19_19:                              # %for.body62.i
                                        #   in Loop: Header=BB19_18 Depth=1
	cmpq	$36, %rsi
	jne	.LBB19_25
# BB#20:                                # %if.then67.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movl	reload_combine_ruid(%rip), %ecx
	movq	%rcx, 104(%rsp)         # 8-byte Spill
.LBB19_25:                              # %if.end90.i
                                        #   in Loop: Header=BB19_18 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB19_137
# BB#26:                                # %if.end99.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movl	reload_combine_ruid(%rip), %eax
	incq	%rax
	movl	%eax, reload_combine_ruid(%rip)
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	leaq	32(%r14), %rcx
	movq	%rcx, 144(%rsp)         # 8-byte Spill
	cmpq	$105, %rax
	jne	.LBB19_36
# BB#27:                                # %cond.true.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rbx
	movzbl	(%rbx), %eax
	cmpq	$47, %rax
	je	.LBB19_29
# BB#28:                                # %cond.false.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	single_set_2
	movq	%rax, %rbx
.LBB19_29:                              # %cond.end123.i
                                        #   in Loop: Header=BB19_18 Depth=1
	testq	%rbx, %rbx
	je	.LBB19_36
# BB#30:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	8(%rbx), %rsi
	movl	(%rsi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB19_36
# BB#31:                                # %land.lhs.true134.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movl	8(%rsi), %edx
	movq	%rdx, %rdi
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rdi
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdi
	je	.LBB19_35
# BB#32:                                # %land.lhs.true134.i
                                        #   in Loop: Header=BB19_18 Depth=1
	leaq	-21(%rdx), %rcx
	andq	%r12, %rcx
	cmpq	$8, %rcx
	jb	.LBB19_35
# BB#33:                                # %land.lhs.true134.i
                                        #   in Loop: Header=BB19_18 Depth=1
	leaq	-45(%rdx), %rcx
	andq	%r12, %rcx
	cmpq	$8, %rcx
	jb	.LBB19_35
# BB#34:                                # %land.lhs.true134.i
                                        #   in Loop: Header=BB19_18 Depth=1
	addq	$-29, %rdx
	andq	%r12, %rdx
	cmpq	$7, %rdx
	jbe	.LBB19_35
# BB#53:                                # %cond.false225.i
                                        #   in Loop: Header=BB19_18 Depth=1
	cmpq	$24, %rax
	jne	.LBB19_54
# BB#56:                                # %cond.true247.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB19_58
.LBB19_35:                              # %cond.true204.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movl	mode_class(,%rax,4), %eax
	addq	$-5, %rax
	andq	%r12, %rax
	cmpq	$2, %rax
	jae	.LBB19_59
	jmp	.LBB19_36
.LBB19_54:                              # %cond.false225.i
                                        #   in Loop: Header=BB19_18 Depth=1
	cmpq	$18, %rax
	jne	.LBB19_57
# BB#55:                                # %cond.true234.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB19_58
.LBB19_57:                              # %cond.false251.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
.LBB19_58:                              # %cond.end272.i
                                        #   in Loop: Header=BB19_18 Depth=1
	andq	%r12, %rax
	cmpq	$1, %rax
	jne	.LBB19_36
.LBB19_59:                              # %land.lhs.true276.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	16(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$75, %rcx
	jne	.LBB19_36
# BB#60:                                # %land.lhs.true284.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	16(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$61, %rcx
	jne	.LBB19_36
# BB#61:                                # %land.lhs.true295.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	8(%rax), %rdi
	callq	rtx_equal_p
	testq	%r12, %rax
	je	.LBB19_36
# BB#62:                                # %land.lhs.true307.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	8(%rbx), %rsi
	movl	8(%rsi), %eax
	imulq	$120, %rax, %rax
	movslq	reg_state+116(%rax), %rax
	movq	$32, %rcx
	movq	104(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB19_36
# BB#63:                                # %if.then318.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	%rsi, 64(%rsp)          # 8-byte Spill
	movq	16(%rbx), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	16(%rax), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	callq	prev_nonnote_insn
	movq	%rax, %rbx
	xorq	%rax, %rax
	testq	%rbx, %rbx
	je	.LBB19_67
# BB#64:                                # %cond.true330.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movzbl	(%rbx), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	movq	64(%rsp), %rsi          # 8-byte Reload
	jne	.LBB19_68
# BB#65:                                # %cond.true338.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB19_68
# BB#66:                                # %cond.false350.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB19_67:                              # %cond.end361.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	64(%rsp), %rsi          # 8-byte Reload
.LBB19_68:                              # %cond.end361.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movl	8(%rsi), %ecx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	reg_class_contents+80(%rip), %r8
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%r8, %rdx
	jne	.LBB19_69
# BB#70:                                # %lor.lhs.false370.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %ecx
	movq	$1, %rax
	shlq	%cl, %rax
	andq	%r8, %rax
	movq	%rsi, %rdi
	jne	.LBB19_72
# BB#71:                                # %lor.lhs.false370.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movabsq	$0, %rdi
.LBB19_72:                              # %lor.lhs.false370.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	%rsi, 64(%rsp)          # 8-byte Spill
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	88(%rsp), %rdx          # 8-byte Reload
	jne	.LBB19_74
# BB#73:                                # %lor.lhs.false370.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
.LBB19_74:                              # %lor.lhs.false370.i
                                        #   in Loop: Header=BB19_18 Depth=1
	testq	%rax, %rax
	jne	.LBB19_75
# BB#76:                                # %lor.lhs.false370.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	96(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	56(%rsp), %rax          # 8-byte Reload
	jg	.LBB19_92
# BB#77:                                # %for.body383.lr.ph.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	64(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %eax
	imulq	$120, %rax, %rax
	movl	target_flags(%rip), %ebx
	leaq	reg_state+116(%rax), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	$23, %rcx
	movq	%rbx, %r10
	shrq	%cl, %r10
	andq	$4, %r10
	movq	$25, %rcx
	movq	%rbx, %r11
	shrq	%cl, %r11
	andq	$1, %r11
	xorq	$3, %r11
	movq	$24, %rcx
	shrq	%cl, %rbx
	andq	$2, %rbx
	xorq	$6, %rbx
	leaq	4(%r10), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	orq	$3, %r10
	movq	72(%rsp), %r9           # 8-byte Reload
	movq	80(%rsp), %rsi          # 8-byte Reload
.LBB19_78:                              # %for.body383.i
                                        #   Parent Loop BB19_18 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rax
	movq	%rsi, %rcx
	shlq	%cl, %rax
	testq	%r8, %rax
	je	.LBB19_86
# BB#79:                                # %land.lhs.true388.i
                                        #   in Loop: Header=BB19_78 Depth=2
	movl	-16(%r9), %eax
	cmpq	$6, %rax
	jne	.LBB19_86
# BB#80:                                # %land.lhs.true394.i
                                        #   in Loop: Header=BB19_78 Depth=2
	movslq	(%r9), %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	movslq	(%rcx), %rcx
	cmpq	%rcx, %rax
	jg	.LBB19_86
# BB#81:                                # %land.lhs.true403.i
                                        #   in Loop: Header=BB19_78 Depth=2
	movq	64(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %edi
	movq	%rsi, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	je	.LBB19_85
# BB#82:                                # %land.lhs.true403.i
                                        #   in Loop: Header=BB19_78 Depth=2
	leaq	-21(%rsi), %rax
	andq	%r12, %rax
	cmpq	$8, %rax
	jb	.LBB19_85
# BB#83:                                # %land.lhs.true403.i
                                        #   in Loop: Header=BB19_78 Depth=2
	leaq	-45(%rsi), %rax
	andq	%r12, %rax
	cmpq	$8, %rax
	jb	.LBB19_85
# BB#84:                                # %land.lhs.true403.i
                                        #   in Loop: Header=BB19_78 Depth=2
	leaq	-29(%rsi), %rax
	andq	%r12, %rax
	cmpq	$7, %rax
	jbe	.LBB19_85
# BB#87:                                # %cond.false445.i
                                        #   in Loop: Header=BB19_78 Depth=2
	cmpq	$18, %rdi
	movq	%r11, %rax
	je	.LBB19_90
# BB#88:                                # %cond.false445.i
                                        #   in Loop: Header=BB19_78 Depth=2
	cmpq	$24, %rdi
	movq	%rbx, %rax
	je	.LBB19_90
# BB#89:                                # %cond.false465.i
                                        #   in Loop: Header=BB19_78 Depth=2
	movzbl	mode_size(%rdi), %eax
	addq	%r10, %rax
	cqto
	idivq	16(%rsp)                # 8-byte Folded Reload
.LBB19_90:                              # %cond.end485.i
                                        #   in Loop: Header=BB19_78 Depth=2
	andq	%r12, %rax
	cmpq	$1, %rax
	jne	.LBB19_86
	jmp	.LBB19_91
.LBB19_85:                              # %cond.true427.i
                                        #   in Loop: Header=BB19_78 Depth=2
	movl	mode_class(,%rdi,4), %eax
	addq	$-5, %rax
	andq	%r12, %rax
	cmpq	$2, %rax
	jae	.LBB19_91
.LBB19_86:                              # %for.inc499.i
                                        #   in Loop: Header=BB19_78 Depth=2
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rsi
	movq	$32, %rcx
	movq	88(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$120, %r9
	xorq	%rdi, %rdi
	cmpq	%rdx, %rax
	movabsq	$0, %rax
	jl	.LBB19_78
	jmp	.LBB19_92
.LBB19_69:                              #   in Loop: Header=BB19_18 Depth=1
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, %rdi
	movq	%rsi, 64(%rsp)          # 8-byte Spill
	movq	56(%rsp), %rax          # 8-byte Reload
	jmp	.LBB19_92
.LBB19_75:                              #   in Loop: Header=BB19_18 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	jmp	.LBB19_92
.LBB19_91:                              # %if.then489.i
                                        #   in Loop: Header=BB19_18 Depth=1
	callq	gen_rtx_REG
	movq	64(%rsp), %rcx          # 8-byte Reload
	movzbl	2(%rcx), %esi
	movq	%rax, %rbx
	movq	$75, %rdi
	movq	%rbx, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	gen_rtx_fmt_ee
	movq	%rbx, %rdi
.LBB19_92:                              # %if.end502.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	48(%rsp), %rcx          # 8-byte Reload
	testq	%rcx, %rcx
	movq	64(%rsp), %rsi          # 8-byte Reload
	je	.LBB19_36
# BB#93:                                # %land.lhs.true505.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	16(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB19_36
# BB#94:                                # %land.lhs.true513.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	8(%rcx), %rdi
	movq	%rcx, %rbx
	callq	rtx_equal_p
	movq	%rbx, %rdx
	testq	%r12, %rax
	je	.LBB19_36
# BB#95:                                # %land.lhs.true519.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	imulq	$120, %rax, %rdi
	movslq	reg_state+96(%rdi), %rax
	testq	%rax, %rax
	js	.LBB19_36
# BB#96:                                # %land.lhs.true525.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %eax
	imulq	$120, %rax, %rax
	movslq	reg_state+112(%rax), %rax
	movslq	reg_state+116(%rdi), %rcx
	cmpq	%rcx, %rax
	jg	.LBB19_36
# BB#97:                                # %land.lhs.true525.i
                                        #   in Loop: Header=BB19_18 Depth=1
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB19_36
# BB#98:                                # %if.then540.i
                                        #   in Loop: Header=BB19_18 Depth=1
	leaq	8(%rdx), %rsi
	leaq	reg_state+96(%rdi), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$1, %rcx
	movq	%rdi, %rbx
	movq	%rbx, (%rsp)            # 8-byte Spill
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	callq	validate_change
	movq	reg_state+104(%rbx), %rax
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB19_100
# BB#99:                                # %if.then550.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	48(%rsp), %rbx          # 8-byte Reload
	addq	$16, %rbx
	movq	(%rbx), %rcx
	movq	8(%rcx), %rcx
	movq	8(%rax), %rsi
	addq	%rcx, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$1, %rcx
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	validate_change
.LBB19_100:                             # %if.end568.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movslq	(%rax), %rdx
	cmpq	$5, %rdx
	jg	.LBB19_103
# BB#101:                               # %for.body575.lr.ph.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%rax,4), %rax
	leaq	(%rax,%rax,2), %rax
	leaq	(%rax,%rdx,2), %rax
	incq	%rdx
	leaq	reg_state+8(,%rax,8), %rbx
.LBB19_102:                             # %for.body575.i
                                        #   Parent Loop BB19_18 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	-8(%rbx), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	(%rbx), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	copy_rtx
	movq	$1, %rcx
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	validate_change
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rdx
	addq	$16, %rbx
	cmpq	$6, %rax
	jl	.LBB19_102
.LBB19_103:                             # %for.end590.i
                                        #   in Loop: Header=BB19_18 Depth=1
	callq	apply_change_group
	testq	%r12, %rax
	movq	(%rsp), %rax            # 8-byte Reload
	jne	.LBB19_104
	.align	16, 0x90
.LBB19_36:                              # %if.end640.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	32(%r14), %rdi
	xorq	%rdx, %rdx
	movabsq	$reload_combine_note_store, %rsi
	callq	note_stores
	movzbl	(%r14), %eax
	cmpq	$33, %rax
	jne	.LBB19_37
# BB#123:                               # %land.lhs.true810.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$51, %rax
	je	.LBB19_132
# BB#124:                               # %if.then818.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	%r14, %rdi
	callq	condjump_p
	testq	%r12, %rax
	jne	.LBB19_126
# BB#125:                               # %lor.lhs.false822.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	%r14, %rdi
	callq	condjump_in_parallel_p
	testq	%r12, %rax
	movq	136(%rsp), %rdx         # 8-byte Reload
	je	.LBB19_128
.LBB19_126:                             # %land.lhs.true825.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	64(%r14), %rax
	testq	%rax, %rax
	movq	136(%rsp), %rdx         # 8-byte Reload
	je	.LBB19_128
# BB#127:                               # %if.then830.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movl	48(%rax), %eax
	movq	112(%rsp), %rcx         # 8-byte Reload
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx,%rax,8), %rdx
.LBB19_128:                             # %if.end841.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	$52, %rax
	movabsq	$reg_state+6336, %rsi
	.align	16, 0x90
.LBB19_129:                             # %for.body845.i
                                        #   Parent Loop BB19_18 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	testq	%rdx, %rdi
	je	.LBB19_131
# BB#130:                               # %if.then850.i
                                        #   in Loop: Header=BB19_129 Depth=2
	movl	%r12d, (%rsi)
.LBB19_131:                             # %for.inc855.i
                                        #   in Loop: Header=BB19_129 Depth=2
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	decq	%rax
	addq	$-120, %rsi
	testq	%rdi, %rdi
	jg	.LBB19_129
	jmp	.LBB19_132
	.align	16, 0x90
.LBB19_37:                              # %if.end640.i
                                        #   in Loop: Header=BB19_18 Depth=1
	cmpq	$34, %rax
	jne	.LBB19_132
# BB#38:                                # %for.cond649.preheader.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movl	reload_combine_ruid(%rip), %r8d
	movq	$-53, %rax
	movabsq	$reg_state+112, %rcx
	.align	16, 0x90
.LBB19_39:                              # %for.body652.i
                                        #   Parent Loop BB19_18 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	call_used_regs+53(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB19_41
# BB#40:                                # %if.then656.i
                                        #   in Loop: Header=BB19_39 Depth=2
	movl	%r13d, -16(%rcx)
	movl	%r8d, (%rcx)
.LBB19_41:                              # %for.inc664.i
                                        #   in Loop: Header=BB19_39 Depth=2
	addq	$120, %rcx
	incq	%rax
	jne	.LBB19_39
# BB#42:                                # %for.end666.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	64(%r14), %rdi
	jmp	.LBB19_43
	.align	16, 0x90
.LBB19_122:                             # %for.inc800.i
                                        #   in Loop: Header=BB19_43 Depth=2
	movq	16(%rdi), %rdi
.LBB19_43:                              # %for.end666.i
                                        #   Parent Loop BB19_18 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB19_118 Depth 3
	testq	%rdi, %rdi
	je	.LBB19_132
# BB#44:                                # %for.body672.i
                                        #   in Loop: Header=BB19_43 Depth=2
	movq	8(%rdi), %rbx
	movq	8(%rbx), %rcx
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB19_122
# BB#45:                                # %if.then683.i
                                        #   in Loop: Header=BB19_43 Depth=2
	movl	8(%rcx), %esi
	movq	%rsi, %rdx
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB19_49
# BB#46:                                # %if.then683.i
                                        #   in Loop: Header=BB19_43 Depth=2
	leaq	-21(%rsi), %rcx
	andq	%r12, %rcx
	cmpq	$8, %rcx
	jb	.LBB19_49
# BB#47:                                # %if.then683.i
                                        #   in Loop: Header=BB19_43 Depth=2
	leaq	-45(%rsi), %rcx
	andq	%r12, %rcx
	cmpq	$8, %rcx
	jb	.LBB19_49
# BB#48:                                # %if.then683.i
                                        #   in Loop: Header=BB19_43 Depth=2
	leaq	-29(%rsi), %rcx
	andq	%r12, %rcx
	cmpq	$7, %rcx
	jbe	.LBB19_49
# BB#112:                               # %cond.false729.i
                                        #   in Loop: Header=BB19_43 Depth=2
	cmpq	$24, %rax
	jne	.LBB19_113
# BB#115:                               # %cond.true745.i
                                        #   in Loop: Header=BB19_43 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB19_117
.LBB19_49:                              # %cond.true711.i
                                        #   in Loop: Header=BB19_43 Depth=2
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB19_117
# BB#50:                                # %lor.rhs719.i
                                        #   in Loop: Header=BB19_43 Depth=2
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB19_52
# BB#51:                                # %lor.rhs719.i
                                        #   in Loop: Header=BB19_43 Depth=2
	movabsq	$1, %rax
.LBB19_52:                              # %lor.rhs719.i
                                        #   in Loop: Header=BB19_43 Depth=2
	incq	%rax
	jmp	.LBB19_117
.LBB19_113:                             # %cond.false729.i
                                        #   in Loop: Header=BB19_43 Depth=2
	cmpq	$18, %rax
	jne	.LBB19_116
# BB#114:                               # %cond.true735.i
                                        #   in Loop: Header=BB19_43 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB19_117
.LBB19_116:                             # %cond.false749.i
                                        #   in Loop: Header=BB19_43 Depth=2
	movl	target_flags(%rip), %r9d
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %r9
	andq	$4, %r9
	leaq	3(%rax,%r9), %rax
	addq	$4, %r9
	cqto
	idivq	%r9
	.align	16, 0x90
.LBB19_117:                             # %cond.end769.i
                                        #   in Loop: Header=BB19_43 Depth=2
	leaq	-1(%rsi,%rax), %rax
	andq	%r12, %rax
	cmpq	%rax, %rsi
	ja	.LBB19_122
	.align	16, 0x90
.LBB19_118:                             # %for.body776.i
                                        #   Parent Loop BB19_18 Depth=1
                                        #     Parent Loop BB19_43 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rbx), %edx
	movq	%rsi, %rcx
	andq	%r12, %rcx
	imulq	$120, %rcx, %rcx
	cmpq	$49, %rdx
	leaq	reg_state+96(%rcx), %rdx
	jne	.LBB19_120
# BB#119:                               # %if.then784.i
                                        #   in Loop: Header=BB19_118 Depth=3
	movl	%r13d, (%rdx)
	movl	%r8d, reg_state+112(%rcx)
	jmp	.LBB19_121
	.align	16, 0x90
.LBB19_120:                             # %if.else791.i
                                        #   in Loop: Header=BB19_118 Depth=3
	movl	%r12d, (%rdx)
.LBB19_121:                             # %for.inc796.i
                                        #   in Loop: Header=BB19_118 Depth=3
	incq	%rsi
	movq	%rsi, %rcx
	andq	%r12, %rcx
	cmpq	%rax, %rcx
	jbe	.LBB19_118
	jmp	.LBB19_122
	.align	16, 0x90
.LBB19_132:                             # %if.end859.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	%r14, %rsi
	callq	reload_combine_note_use
	movq	56(%r14), %rax
	testq	%rax, %rax
	movabsq	$fixed_regs, %rdi
	je	.LBB19_137
	.align	16, 0x90
.LBB19_133:                             # %for.body868.i
                                        #   Parent Loop BB19_18 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rax), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$131072, %rcx           # imm = 0x20000
	jne	.LBB19_136
# BB#134:                               # %land.lhs.true874.i
                                        #   in Loop: Header=BB19_133 Depth=2
	movq	8(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB19_136
# BB#135:                               # %if.then882.i
                                        #   in Loop: Header=BB19_133 Depth=2
	movslq	8(%rcx), %rcx
	movl	reload_combine_ruid(%rip), %edx
	imulq	$120, %rcx, %rcx
	movl	%edx, reg_state+112(%rcx)
	movl	%r12d, reg_state+96(%rcx)
.LBB19_136:                             # %for.inc897.i
                                        #   in Loop: Header=BB19_133 Depth=2
	movq	16(%rax), %rax
	testq	%rax, %rax
	jne	.LBB19_133
	jmp	.LBB19_137
.LBB19_104:                             # %if.then593.i
                                        #   in Loop: Header=BB19_18 Depth=1
	leaq	reg_state+104(%rax), %rbx
	movq	%r14, %rdi
	callq	delete_insn
	movq	(%rbx), %rax
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB19_111
# BB#105:                               # %if.then600.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	56(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB19_111
# BB#106:                               #   in Loop: Header=BB19_18 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	leaq	56(%rax), %rax
.LBB19_108:                             # %for.body606.lr.ph.i
                                        #   Parent Loop BB19_18 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB19_109 Depth 3
	movq	%rcx, %rdx
.LBB19_109:                             # %for.body606.i
                                        #   Parent Loop BB19_18 Depth=1
                                        #     Parent Loop BB19_108 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	2(%rdx), %esi
	movq	16(%rdx), %rcx
	addq	$-3, %rsi
	andq	%r12, %rsi
	cmpq	$1, %rsi
	ja	.LBB19_107
# BB#110:                               # %if.then618.i
                                        #   in Loop: Header=BB19_109 Depth=3
	movq	%rcx, (%rax)
	testq	%rcx, %rcx
	movq	%rcx, %rdx
	jne	.LBB19_109
	jmp	.LBB19_111
.LBB19_107:                             # %for.cond604.outer.loopexit.i
                                        #   in Loop: Header=BB19_108 Depth=2
	addq	$16, %rdx
	testq	%rcx, %rcx
	movq	%rdx, %rax
	jne	.LBB19_108
.LBB19_111:                             # %if.end628.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	%r13d, (%rax)
	movq	16(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %eax
	movl	reload_combine_ruid(%rip), %ecx
	imulq	$120, %rax, %rax
	movl	%ecx, reg_state+112(%rax)
	movabsq	$fixed_regs, %rdi
	.align	16, 0x90
.LBB19_137:                             # %for.inc902.i
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	16(%r14), %r14
	testq	%r14, %r14
	jne	.LBB19_18
.LBB19_138:                             # %for.end906.i
	movq	120(%rsp), %rdi         # 8-byte Reload
	callq	free
	.align	16, 0x90
.LBB19_139:                             # %for.body.i4
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rax, %rax
	movl	%eax, reg_set_luid(,%r15,4)
	movq	$32, %rcx
	leaq	-1(%r15), %rdx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	testq	%r15, %r15
	movq	%rdx, %r15
	jg	.LBB19_139
# BB#140:                               # %for.end.i5
	movl	%eax, move2add_last_label_luid(%rip)
	movq	$2, %rax
	movl	%eax, move2add_luid(%rip)
	cmpq	$0, 128(%rsp)           # 8-byte Folded Reload
	je	.LBB19_207
# BB#141:
	xorq	%r13, %r13
	movq	128(%rsp), %rbx         # 8-byte Reload
	.align	16, 0x90
.LBB19_142:                             # %for.body2.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_196 Depth 2
                                        #     Child Loop BB19_203 Depth 2
	movzbl	(%rbx), %ecx
	cmpq	$36, %rcx
	jne	.LBB19_144
# BB#143:                               # %if.then.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movl	%eax, move2add_last_label_luid(%rip)
	incq	%rax
	movl	%eax, move2add_luid(%rip)
	jmp	.LBB19_206
	.align	16, 0x90
.LBB19_144:                             # %if.end.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movzbl	rtx_class(%rcx), %eax
	cmpq	$105, %rax
	jne	.LBB19_206
# BB#145:                               # %if.end11.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	32(%rbx), %r14
	movzbl	(%r14), %eax
	cmpq	$47, %rax
	jne	.LBB19_195
# BB#146:                               # %land.lhs.true.i9
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	8(%r14), %r12
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB19_195
# BB#147:                               # %if.then24.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movslq	8(%r12), %r15
	movslq	reg_set_luid(,%r15,4), %rdx
	movslq	move2add_last_label_luid(%rip), %rcx
	cmpq	%rcx, %rdx
	jle	.LBB19_195
# BB#148:                               # %land.lhs.true37.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	$16, %rcx
	movslq	reg_mode(,%r15,4), %rsi
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %ecx
	movzbl	mode_size(%rsi), %esi
	cmpq	%rsi, %rcx
	ja	.LBB19_195
# BB#149:                               # %if.then63.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	16(%r14), %r8
	movzbl	(%r8), %esi
	cmpq	$61, %rsi
	jne	.LBB19_150
# BB#165:                               # %land.lhs.true121.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movl	8(%r8), %eax
	movl	reg_set_luid(,%rax,4), %esi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rdx
	cmpq	%rsi, %rdx
	jne	.LBB19_195
# BB#166:                               # %land.lhs.true131.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movl	reg_base_reg(,%r15,4), %edx
	movl	reg_base_reg(,%rax,4), %esi
	cmpq	%rsi, %rdx
	jne	.LBB19_195
# BB#167:                               # %land.lhs.true141.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movslq	reg_mode(,%rax,4), %rax
	movzbl	mode_size(%rax), %eax
	cmpq	%rax, %rcx
	ja	.LBB19_195
# BB#168:                               # %if.then175.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	%rbx, %rdi
	movq	%r8, %r14
	callq	next_nonnote_insn
	movq	%r14, %rdx
	testq	%rax, %rax
	je	.LBB19_195
# BB#169:                               # %if.then178.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movzbl	(%rax), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB19_195
# BB#170:                               # %cond.true.i15
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	32(%rax), %rsi
	movzbl	(%rsi), %ecx
	cmpq	$47, %rcx
	jne	.LBB19_172
# BB#171:                               #   in Loop: Header=BB19_142 Depth=1
	movq	%rax, %r14
	jmp	.LBB19_173
.LBB19_150:                             # %if.then63.i
                                        #   in Loop: Header=BB19_142 Depth=1
	cmpq	$54, %rsi
	jne	.LBB19_195
# BB#151:                               # %land.lhs.true68.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movslq	reg_base_reg(,%r15,4), %rcx
	testq	%rcx, %rcx
	jns	.LBB19_195
# BB#152:                               # %if.then73.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	8(%r8), %rcx
	movq	reg_offset(,%r15,8), %rdx
	movq	mode_mask_array(,%rax,8), %rsi
	movzbl	mode_bitsize(%rax,%rax), %eax
	subq	%rdx, %rcx
	andq	%rcx, %rsi
	testq	%rax, %rax
	je	.LBB19_153
# BB#154:                               # %if.then73.i
                                        #   in Loop: Header=BB19_142 Depth=1
	cmpq	$63, %rax
	ja	.LBB19_155
# BB#156:                               # %land.lhs.true6.i.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	%r8, 144(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	leaq	(%rax,%rcx), %rcx
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rsi, %rdx
	je	.LBB19_158
# BB#157:                               # %if.then.i.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	$-1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	jmp	.LBB19_158
.LBB19_153:                             #   in Loop: Header=BB19_142 Depth=1
	movq	%r8, 144(%rsp)          # 8-byte Spill
	jmp	.LBB19_158
.LBB19_155:                             #   in Loop: Header=BB19_142 Depth=1
	movq	%r8, 144(%rsp)          # 8-byte Spill
.LBB19_158:                             # %sext_for_mode.exit.i
                                        #   in Loop: Header=BB19_142 Depth=1
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB19_160
# BB#159:                               # %if.then84.i
                                        #   in Loop: Header=BB19_142 Depth=1
	addq	$16, %r14
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	jmp	.LBB19_163
.LBB19_160:                             # %if.else.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	$75, %rsi
	movq	%rax, %rdi
	movq	%rax, 136(%rsp)         # 8-byte Spill
	callq	rtx_cost
	movq	%rax, %r14
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	$47, %rsi
	movq	144(%rsp), %rdi         # 8-byte Reload
	callq	rtx_cost
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r14
	jge	.LBB19_164
# BB#161:                               # %land.lhs.true93.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	%r12, %rdi
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	have_add2_insn
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB19_164
# BB#162:                               # %if.then96.i
                                        #   in Loop: Header=BB19_142 Depth=1
	leaq	32(%rbx), %r14
	movq	%r12, %rdi
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	gen_add2_insn
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
.LBB19_163:                             # %if.end103.i
                                        #   in Loop: Header=BB19_142 Depth=1
	callq	validate_change
.LBB19_164:                             # %if.end103.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movl	move2add_luid(%rip), %eax
	movl	%eax, reg_set_luid(,%r15,4)
	movzbl	2(%r12), %eax
	movl	%eax, reg_mode(,%r15,4)
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movq	%rax, reg_offset(,%r15,8)
	jmp	.LBB19_206
.LBB19_172:                             # %cond.false.i16
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	%rax, %rdi
	movq	%rdi, %r14
	movq	%rdx, 144(%rsp)         # 8-byte Spill
	callq	single_set_2
	movq	144(%rsp), %rdx         # 8-byte Reload
	movq	%rax, %rsi
.LBB19_173:                             # %if.end204.i
                                        #   in Loop: Header=BB19_142 Depth=1
	testq	%rsi, %rsi
	movq	%r14, %rdi
	je	.LBB19_195
# BB#174:                               # %land.lhs.true206.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	8(%rsi), %rax
	cmpq	%r12, %rax
	jne	.LBB19_195
# BB#175:                               # %land.lhs.true212.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	16(%rsi), %rax
	movzbl	(%rax), %ecx
	cmpq	$75, %rcx
	jne	.LBB19_195
# BB#176:                               # %land.lhs.true220.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	8(%rax), %rcx
	cmpq	%r12, %rcx
	jne	.LBB19_195
# BB#177:                               # %land.lhs.true229.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	16(%rax), %r10
	movzbl	(%r10), %eax
	cmpq	$54, %rax
	jne	.LBB19_195
# BB#178:                               # %if.then240.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movl	8(%rdx), %eax
	movq	8(%r10), %rcx
	movzbl	2(%r12), %edx
	movq	reg_offset(,%rax,8), %rax
	movq	reg_offset(,%r15,8), %r9
	addq	%rcx, %rax
	movq	mode_mask_array(,%rdx,8), %r8
	movzbl	mode_bitsize(%rdx,%rdx), %edx
	movq	%rax, %rcx
	subq	%r9, %rcx
	andq	%rcx, %r8
	testq	%rdx, %rdx
	je	.LBB19_179
# BB#180:                               # %if.then240.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	%r10, 120(%rsp)         # 8-byte Spill
	movq	%rsi, %r14
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	%rdi, 136(%rsp)         # 8-byte Spill
	cmpq	$63, %rdx
	ja	.LBB19_183
# BB#181:                               # %land.lhs.true6.i503.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	leaq	(%rdx,%rcx), %rcx
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%r8, %rsi
	je	.LBB19_183
# BB#182:                               # %if.then.i506.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	$-1, %rsi
	movq	%rdx, %rcx
	shlq	%cl, %rsi
	orq	%rsi, %r8
	jmp	.LBB19_183
	.align	16, 0x90
.LBB19_195:                             # %if.end309.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	56(%rbx), %rax
	jmp	.LBB19_196
	.align	16, 0x90
.LBB19_201:                             # %for.inc344.i
                                        #   in Loop: Header=BB19_196 Depth=2
	movq	16(%rax), %rax
.LBB19_196:                             # %if.end309.i
                                        #   Parent Loop BB19_142 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rax, %rax
	je	.LBB19_202
# BB#197:                               # %for.body315.i
                                        #   in Loop: Header=BB19_196 Depth=2
	movl	(%rax), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$131072, %rcx           # imm = 0x20000
	jne	.LBB19_201
# BB#198:                               # %land.lhs.true321.i
                                        #   in Loop: Header=BB19_196 Depth=2
	movq	8(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB19_201
# BB#199:                               # %if.then329.i
                                        #   in Loop: Header=BB19_196 Depth=2
	movslq	8(%rcx), %rcx
	cmpq	$52, %rcx
	jg	.LBB19_201
# BB#200:                               # %if.then339.i
                                        #   in Loop: Header=BB19_196 Depth=2
	movl	%r13d, reg_set_luid(,%rcx,4)
	jmp	.LBB19_201
	.align	16, 0x90
.LBB19_202:                             # %for.end348.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	32(%rbx), %rdi
	xorq	%rdx, %rdx
	movabsq	$move2add_note_store, %rsi
	callq	note_stores
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	movq	$52, %rax
	jne	.LBB19_206
	.align	16, 0x90
.LBB19_203:                             # %for.body360.i
                                        #   Parent Loop BB19_142 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	call_used_regs(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB19_205
# BB#204:                               # %if.then364.i
                                        #   in Loop: Header=BB19_203 Depth=2
	movl	%r13d, reg_set_luid(,%rax,4)
.LBB19_205:                             # %for.inc368.i
                                        #   in Loop: Header=BB19_203 Depth=2
	movq	$32, %rcx
	leaq	-1(%rax), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rdx, %rax
	jg	.LBB19_203
	jmp	.LBB19_206
.LBB19_179:                             #   in Loop: Header=BB19_142 Depth=1
	movq	%r10, 120(%rsp)         # 8-byte Spill
	movq	%rsi, %r14
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	%rdi, 136(%rsp)         # 8-byte Spill
.LBB19_183:                             # %sext_for_mode.exit508.i
                                        #   in Loop: Header=BB19_142 Depth=1
	xorq	%rdi, %rdi
	movq	%r8, %rsi
	callq	gen_rtx_CONST_INT
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, 112(%rsp)         # 8-byte Spill
	cmpq	%rcx, %rax
	jne	.LBB19_185
# BB#184:                               # %if.then267.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	%r14, %rax
	addq	$16, %rax
	xorq	%rcx, %rcx
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	movq	%r12, %rdx
	jmp	.LBB19_188
.LBB19_185:                             # %if.else272.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	$75, %rsi
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	rtx_cost
	movq	%rax, %r14
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	$47, %rsi
	movq	120(%rsp), %rdi         # 8-byte Reload
	callq	rtx_cost
	addq	$4, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r14
	jge	.LBB19_190
# BB#186:                               # %land.lhs.true278.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	%r12, %rdi
	movq	112(%rsp), %rsi         # 8-byte Reload
	callq	have_add2_insn
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB19_190
# BB#187:                               # %if.then281.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	136(%rsp), %rax         # 8-byte Reload
	leaq	32(%rax), %r14
	movq	%r12, %rdi
	movq	112(%rsp), %rsi         # 8-byte Reload
	callq	gen_add2_insn
	xorq	%rcx, %rcx
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	%r14, %rsi
	movq	%rax, %rdx
.LBB19_188:                             # %if.end288.i
                                        #   in Loop: Header=BB19_142 Depth=1
	callq	validate_change
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB19_190
# BB#189:                               # %if.then290.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	%rbx, %rdi
	callq	delete_insn
.LBB19_190:                             # %if.end292.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movzbl	2(%r12), %eax
	movl	%eax, reg_mode(,%r15,4)
	movzbl	2(%r12), %ecx
	movq	mode_mask_array(,%rcx,8), %rax
	movzbl	mode_bitsize(%rcx,%rcx), %edx
	movq	144(%rsp), %rcx         # 8-byte Reload
	andq	%rcx, %rax
	testq	%rdx, %rdx
	je	.LBB19_194
# BB#191:                               # %if.end292.i
                                        #   in Loop: Header=BB19_142 Depth=1
	cmpq	$63, %rdx
	ja	.LBB19_194
# BB#192:                               # %land.lhs.true6.i484.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	leaq	(%rdx,%rcx), %rcx
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%rax, %rsi
	je	.LBB19_194
# BB#193:                               # %if.then.i487.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	$-1, %rsi
	movq	%rdx, %rcx
	shlq	%cl, %rsi
	orq	%rsi, %rax
.LBB19_194:                             # %sext_for_mode.exit489.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movq	%rax, reg_offset(,%r15,8)
	movq	136(%rsp), %rbx         # 8-byte Reload
	.align	16, 0x90
.LBB19_206:                             # %for.inc372.i
                                        #   in Loop: Header=BB19_142 Depth=1
	movl	move2add_luid(%rip), %eax
	movq	24(%rbx), %rbx
	incq	%rax
	testq	%rbx, %rbx
	movl	%eax, move2add_luid(%rip)
	jne	.LBB19_142
.LBB19_207:                             # %reload_cse_move2add.exit
	movl	flag_expensive_optimizations(%rip), %eax
	testq	%rax, %rax
	je	.LBB19_209
# BB#208:                               # %if.then
	movq	128(%rsp), %rdi         # 8-byte Reload
	callq	reload_cse_regs_1
.LBB19_209:                             # %if.end
	addq	$160, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp284:
	.size	reload_cse_regs, .Ltmp284-reload_cse_regs
	.cfi_endproc

	.align	16, 0x90
	.type	reload_cse_regs_1,@function
reload_cse_regs_1:                      # @reload_cse_regs_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp291:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp292:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp293:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp294:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp295:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp296:
	.cfi_def_cfa_offset 64
.Ltmp297:
	.cfi_offset %rbx, -48
.Ltmp298:
	.cfi_offset %r12, -40
.Ltmp299:
	.cfi_offset %r13, -32
.Ltmp300:
	.cfi_offset %r14, -24
.Ltmp301:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	cselib_init
	callq	init_alias_analysis
	testq	%rbx, %rbx
	je	.LBB20_36
# BB#1:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB20_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB20_16 Depth 2
                                        #       Child Loop BB20_18 Depth 3
                                        #     Child Loop BB20_27 Depth 2
                                        #       Child Loop BB20_28 Depth 3
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB20_35
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	32(%rbx), %r13
	movzbl	(%r13), %eax
	cmpq	$39, %rax
	jne	.LBB20_4
# BB#15:                                # %if.then24.i
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	8(%r13), %rax
	movl	(%rax), %eax
	xorq	%rcx, %rcx
.LBB20_16:                              # %for.cond.outer.i
                                        #   Parent Loop BB20_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB20_18 Depth 3
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB20_25
# BB#17:                                # %for.body.lr.ph.i
                                        #   in Loop: Header=BB20_16 Depth=2
	movq	$32, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	decq	%rbx
	movq	%rax, %r12
	.align	16, 0x90
.LBB20_18:                              # %for.body.i
                                        #   Parent Loop BB20_2 Depth=1
                                        #     Parent Loop BB20_16 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%r13), %rax
	movq	8(%rax,%rbx,8), %r14
	movzbl	(%r14), %eax
	leaq	-1(%r12), %r15
	cmpq	$47, %rax
	jne	.LBB20_19
# BB#21:                                # %if.then37.i
                                        #   in Loop: Header=BB20_18 Depth=3
	movq	8(%r14), %rdi
	movq	16(%r14), %rsi
	callq	rtx_equal_for_cselib_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB20_24
# BB#22:                                # %if.end41.i
                                        #   in Loop: Header=BB20_18 Depth=3
	movq	8(%r14), %rcx
	movzbl	3(%rcx), %eax
	testq	$64, %rax
	je	.LBB20_20
	jmp	.LBB20_23
	.align	16, 0x90
.LBB20_19:                              # %for.body.i
                                        #   in Loop: Header=BB20_18 Depth=3
	cmpq	$49, %rax
	jne	.LBB20_24
.LBB20_20:                              # %for.cond.backedge.i
                                        #   in Loop: Header=BB20_18 Depth=3
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	movq	%r15, %r12
	jg	.LBB20_18
	jmp	.LBB20_25
.LBB20_23:                              # %if.then49.i
                                        #   in Loop: Header=BB20_16 Depth=2
	cmpq	$0, (%rsp)              # 8-byte Folded Reload
	movq	%rbx, %rax
	je	.LBB20_16
.LBB20_24:                              # %for.end.i
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	testq	%r12, %r12
	jg	.LBB20_26
.LBB20_25:                              # %if.then65.i
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	8(%rsp), %rbx           # 8-byte Reload
	movslq	8(%rbx), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movq	8(%rax), %r14
	movq	(%rsp), %rdx            # 8-byte Reload
	testq	%rdx, %rdx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jne	.LBB20_8
	jmp	.LBB20_9
	.align	16, 0x90
.LBB20_4:                               # %if.then
                                        #   in Loop: Header=BB20_2 Depth=1
	cmpq	$47, %rax
	jne	.LBB20_35
# BB#5:                                 # %if.then.i
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	reload_cse_simplify_set
	testq	%r15, %rax
	jne	.LBB20_12
# BB#6:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	8(%r13), %rdi
	movq	16(%r13), %rsi
	callq	rtx_equal_for_cselib_p
	testq	%r15, %rax
	je	.LBB20_14
# BB#7:                                 # %if.then3.i
                                        #   in Loop: Header=BB20_2 Depth=1
	movslq	8(%rbx), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	8(%r13), %rsi
	movq	32(%rcx,%rax,8), %rax
	movzbl	3(%rsi), %edx
	movq	$57, %rcx
	shlq	%cl, %rdx
	movq	8(%rax), %r14
	movq	$63, %rcx
	sarq	%cl, %rdx
	andq	%rsi, %rdx
	jne	.LBB20_8
.LBB20_9:                               # %if.else.i.i
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	%rbx, %rdi
	callq	delete_insn
	jmp	.LBB20_10
.LBB20_8:                               # %if.then.i.i
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	$48, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	movq	%rax, 32(%rbx)
	movl	%r15d, 40(%rbx)
	movq	$0, 56(%rbx)
.LBB20_10:                              # %if.end.i.i
                                        #   in Loop: Header=BB20_2 Depth=1
	cmpq	%rbx, %r14
	jne	.LBB20_35
# BB#11:                                # %if.then12.i.i
                                        #   in Loop: Header=BB20_2 Depth=1
	movslq	8(%rbx), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rdi
	callq	purge_dead_edges
	jmp	.LBB20_35
.LBB20_26:                              # %if.end66.i
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	8(%r13), %rax
	movl	(%rax), %ebx
	xorq	%r15, %r15
	jmp	.LBB20_27
	.align	16, 0x90
.LBB20_30:                              # %if.then84.i
                                        #   in Loop: Header=BB20_27 Depth=2
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	reload_cse_simplify_set
	addq	%rax, %r15
.LBB20_27:                              # %for.cond72.outer.i
                                        #   Parent Loop BB20_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB20_28 Depth 3
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	.align	16, 0x90
.LBB20_28:                              # %for.cond72.i
                                        #   Parent Loop BB20_2 Depth=1
                                        #     Parent Loop BB20_27 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB20_31
# BB#29:                                # %for.body74.i
                                        #   in Loop: Header=BB20_28 Depth=3
	movq	8(%r13), %rax
	movq	(%rax,%rbx,8), %rdi
	movzbl	(%rdi), %eax
	decq	%rbx
	cmpq	$47, %rax
	jne	.LBB20_28
	jmp	.LBB20_30
.LBB20_31:                              # %for.end96.i
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	testq	%r15, %r15
	jle	.LBB20_33
# BB#32:                                # %if.then98.i
                                        #   in Loop: Header=BB20_2 Depth=1
	callq	apply_change_group
	movq	8(%rsp), %rbx           # 8-byte Reload
	jmp	.LBB20_34
.LBB20_12:                              # %if.end14.i
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB20_14
# BB#13:                                # %if.then16.i
                                        #   in Loop: Header=BB20_2 Depth=1
	callq	apply_change_group
	jmp	.LBB20_35
.LBB20_14:                              # %if.else.i
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	%rbx, %rdi
	callq	reload_cse_simplify_operands
	jmp	.LBB20_35
.LBB20_33:                              # %if.else100.i
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rdi
	callq	reload_cse_simplify_operands
.LBB20_34:                              # %if.end
                                        #   in Loop: Header=BB20_2 Depth=1
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB20_35:                              # %if.end
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	%rbx, %rdi
	callq	cselib_process_insn
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB20_2
.LBB20_36:                              # %for.end
	callq	end_alias_analysis
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	cselib_finish  # TAILCALL
.Ltmp302:
	.size	reload_cse_regs_1, .Ltmp302-reload_cse_regs_1
	.cfi_endproc

	.align	16, 0x90
	.type	move2add_note_store,@function
move2add_note_store:                    # @move2add_note_store
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp307:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp308:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp309:
	.cfi_def_cfa_offset 32
.Ltmp310:
	.cfi_offset %rbx, -32
.Ltmp311:
	.cfi_offset %r14, -24
.Ltmp312:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	(%rbx), %edi
	movq	%rsi, %r14
	movq	$16, %rcx
	movq	%rdi, %r15
	shrq	%cl, %r15
	andq	$255, %r15
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	xorq	%rax, %rax
	cmpq	$63, %rcx
	jne	.LBB21_2
# BB#1:                                 # %if.then
	movq	8(%rbx), %rax
	movl	16(%rbx), %edx
	movzbl	2(%rax), %esi
	movl	8(%rax), %edi
	movq	%r15, %rcx
	callq	subreg_regno_offset
	movq	8(%rbx), %rbx
	movl	(%rbx), %edi
.LBB21_2:                               # %if.end
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB21_3
# BB#6:                                 # %if.end55
	movl	8(%rbx), %ecx
	leaq	(%rcx,%rax), %r8
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%r8, %rdx
	cmpq	$8, %rdx
	jne	.LBB21_7
# BB#8:                                 # %if.end55
	movabsq	$1, %rdx
	jmp	.LBB21_9
.LBB21_3:                               # %if.end
	cmpq	$66, %rcx
	jne	.LBB21_60
# BB#4:                                 # %if.then23
	movq	8(%rbx), %rax
	movl	(%rax), %ecx
	andq	$65532, %rcx            # imm = 0xFFFC
	cmpq	$96, %rcx
	jne	.LBB21_60
# BB#5:                                 # %if.then41
	movq	8(%rax), %rax
	movl	8(%rax), %eax
	xorq	%rcx, %rcx
	movl	%ecx, reg_set_luid(,%rax,4)
	jmp	.LBB21_60
.LBB21_7:
	movabsq	$0, %rdx
.LBB21_9:                               # %if.end55
	leaq	-21(%rcx,%rax), %rbx
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	andq	%r10, %rbx
	cmpq	$8, %rbx
	sbbq	%rbx, %rbx
	andq	$1, %rbx
	orq	%rdx, %rbx
	leaq	-45(%rcx,%rax), %rdx
	andq	%r10, %rdx
	cmpq	$8, %rdx
	sbbq	%rdx, %rdx
	andq	$1, %rdx
	orq	%rbx, %rdx
	leaq	-29(%rcx,%rax), %rax
	andq	%r10, %rax
	cmpq	$8, %rax
	sbbq	%rbx, %rbx
	andq	$1, %rbx
	orq	%rdx, %rbx
	je	.LBB21_11
# BB#10:                                # %cond.true
	movl	mode_class(,%r15,4), %ecx
	leaq	-5(%rcx), %rax
	andq	%r10, %rax
	cmpq	$2, %rax
	jae	.LBB21_17
	jmp	.LBB21_48
.LBB21_11:                              # %cond.false
	cmpq	$24, %r15
	jne	.LBB21_12
# BB#14:                                # %cond.true84
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB21_16
.LBB21_12:                              # %cond.false
	cmpq	$18, %r15
	jne	.LBB21_15
# BB#13:                                # %cond.true80
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB21_16
.LBB21_15:                              # %cond.false88
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%r15), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB21_16:                              # %cond.end101
	andq	%r10, %rax
	cmpq	$1, %rax
	jne	.LBB21_46
.LBB21_17:                              # %land.lhs.true105
	movzbl	(%r14), %eax
	cmpq	$47, %rax
	jne	.LBB21_46
# BB#18:                                # %land.lhs.true110
	movq	8(%r14), %rax
	movzbl	(%rax), %eax
	leaq	-132(%rax), %rcx
	andq	%r10, %rcx
	cmpq	$2, %rcx
	jb	.LBB21_46
# BB#19:                                # %land.lhs.true110
	cmpq	$64, %rax
	jne	.LBB21_20
.LBB21_46:                              # %if.else381
	testq	$1, %rbx
	je	.LBB21_53
# BB#47:                                # %if.else381.cond.true405_crit_edge
	movl	mode_class(,%r15,4), %ecx
.LBB21_48:                              # %cond.true405
	andq	%r10, %rcx
	movq	$2, %rax
	cmpq	$5, %rcx
	je	.LBB21_58
# BB#49:                                # %lor.rhs410
	cmpq	$6, %rcx
	jne	.LBB21_50
# BB#51:                                # %lor.rhs410
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB21_58
.LBB21_53:                              # %cond.false417
	cmpq	$24, %r15
	jne	.LBB21_54
# BB#56:                                # %cond.true427
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB21_58
.LBB21_54:                              # %cond.false417
	cmpq	$18, %r15
	jne	.LBB21_57
# BB#55:                                # %cond.true420
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB21_58
.LBB21_50:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB21_58
.LBB21_57:                              # %cond.false431
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%r15), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB21_58:                              # %cond.end448
	leaq	(%rax,%r8), %rcx
	andq	%r10, %r8
	andq	%r10, %rcx
	cmpq	%rcx, %r8
	jae	.LBB21_60
# BB#59:                                # %for.body.lr.ph
	leaq	reg_set_luid(,%r8,4), %rdi
	decq	%rax
	andq	%r10, %rax
	leaq	4(,%rax,4), %rdx
	xorq	%rsi, %rsi
	callq	memset
.LBB21_60:                              # %if.end455
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB21_20:                              # %if.then134
	movq	16(%r14), %rdx
	movzbl	(%rdx), %eax
	movq	$16, %rcx
	shrq	%cl, %rdi
	cmpq	$54, %rax
	je	.LBB21_37
# BB#21:                                # %if.then134
	andq	$255, %rdi
	xorq	%r9, %r9
	cmpq	$61, %rax
	je	.LBB21_34
# BB#22:                                # %if.then134
	cmpq	$75, %rax
	jne	.LBB21_38
# BB#23:                                # %sw.bb
	movq	8(%rdx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB21_38
# BB#24:                                # %if.then150
	movq	16(%rdx), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$61, %rcx
	jne	.LBB21_25
# BB#27:                                # %land.lhs.true174
	movl	8(%rdx), %r9d
	movslq	reg_set_luid(,%r9,4), %rsi
	movslq	move2add_last_label_luid(%rip), %rbx
	cmpq	%rbx, %rsi
	jle	.LBB21_38
# BB#28:                                # %land.lhs.true185
	movslq	reg_mode(,%r9,4), %rsi
	movzbl	mode_size(%rdi), %ecx
	movzbl	mode_size(%rsi), %esi
	cmpq	%rsi, %rcx
	ja	.LBB21_38
# BB#29:                                # %if.then219
	movslq	reg_base_reg(,%r9,4), %rcx
	testq	%rcx, %rcx
	js	.LBB21_30
# BB#31:                                # %if.else239
	movl	8(%rax), %eax
	movslq	reg_set_luid(,%rax,4), %rcx
	cmpq	%rbx, %rcx
	jle	.LBB21_38
# BB#32:                                # %land.lhs.true281
	movslq	reg_base_reg(,%rax,4), %rcx
	testq	%rcx, %rcx
	jns	.LBB21_38
# BB#33:                                # %if.then289
	movq	reg_offset(,%rax,8), %r9
.LBB21_34:                              # %sw.epilog
	movq	%rdx, %rax
.LBB21_35:                              # %sw.epilog
	movslq	8(%rax), %rdx
	movslq	reg_set_luid(,%rdx,4), %rax
	movslq	move2add_last_label_luid(%rip), %rcx
	cmpq	%rcx, %rax
	jle	.LBB21_36
# BB#39:                                # %if.else340
	movslq	reg_mode(,%rdx,4), %rcx
	movzbl	mode_size(%rdi), %esi
	movzbl	mode_size(%rcx), %ecx
	cmpq	%rcx, %rsi
	ja	.LBB21_38
# BB#40:                                # %if.else340.if.end364_crit_edge
	movl	reg_base_reg(,%rdx,4), %esi
	movq	reg_offset(,%rdx,8), %rcx
	movq	%rsi, %rdx
	jmp	.LBB21_41
.LBB21_37:                              # %sw.bb306
	andq	%r10, %r8
	movl	%r10d, reg_base_reg(,%r8,4)
	movl	move2add_last_label_luid(%rip), %eax
	movq	8(%rdx), %rcx
	movq	%rcx, reg_offset(,%r8,8)
	incq	%rax
	movl	%eax, reg_set_luid(,%r8,4)
	movl	%r15d, reg_mode(,%r8,4)
	jmp	.LBB21_60
.LBB21_36:                              # %if.then331
	movl	%edx, reg_base_reg(,%rdx,4)
	movl	move2add_luid(%rip), %eax
	movq	$0, reg_offset(,%rdx,8)
	movl	%eax, reg_set_luid(,%rdx,4)
	movl	%r15d, reg_mode(,%rdx,4)
	xorq	%rcx, %rcx
.LBB21_41:                              # %if.end364
	andq	%r10, %r8
	movl	%r15d, reg_mode(,%r8,4)
	movl	%eax, reg_set_luid(,%r8,4)
	movq	mode_mask_array(,%rdi,8), %rbx
	movzbl	mode_bitsize(%rdi,%rdi), %eax
	movl	%edx, reg_base_reg(,%r8,4)
	addq	%r9, %rcx
	andq	%rcx, %rbx
	testq	%rax, %rax
	je	.LBB21_45
# BB#42:                                # %if.end364
	cmpq	$63, %rax
	ja	.LBB21_45
# BB#43:                                # %land.lhs.true6.i
	addq	%rax, %r10
	movq	$1, %rdx
	movq	%r10, %rcx
	shlq	%cl, %rdx
	testq	%rbx, %rdx
	je	.LBB21_45
# BB#44:                                # %if.then.i
	movq	$-1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rbx
.LBB21_45:                              # %sext_for_mode.exit
	movq	%rbx, reg_offset(,%r8,8)
	jmp	.LBB21_60
.LBB21_25:                              # %if.then150
	cmpq	$54, %rcx
	jne	.LBB21_38
# BB#26:                                # %if.then161
	movq	8(%rdx), %r9
	jmp	.LBB21_35
.LBB21_38:                              # %invalidate
	andq	%r10, %r8
	xorq	%rax, %rax
	movl	%eax, reg_set_luid(,%r8,4)
	jmp	.LBB21_60
.LBB21_30:                              # %if.then230
	movq	reg_offset(,%r9,8), %r9
	jmp	.LBB21_35
.Ltmp313:
	.size	move2add_note_store, .Ltmp313-move2add_note_store
	.cfi_endproc

	.align	16, 0x90
	.type	reload_combine_note_store,@function
reload_combine_note_store:              # @reload_combine_note_store
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp318:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp319:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp320:
	.cfi_def_cfa_offset 32
.Ltmp321:
	.cfi_offset %rbx, -32
.Ltmp322:
	.cfi_offset %r14, -24
.Ltmp323:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	(%rbx), %edx
	movq	%rsi, %r14
	movq	$16, %rcx
	movq	%rdx, %r15
	shrq	%cl, %r15
	andq	$255, %r15
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	xorq	%rsi, %rsi
	cmpq	$63, %rax
	jne	.LBB22_2
# BB#1:                                 # %if.then
	movq	8(%rbx), %rax
	movl	16(%rbx), %edx
	movzbl	2(%rax), %esi
	movl	8(%rax), %edi
	movq	%r15, %rcx
	callq	subreg_regno_offset
	movq	8(%rbx), %rbx
	movl	(%rbx), %edx
	movq	%rax, %rsi
.LBB22_2:                               # %if.end
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB22_40
# BB#3:                                 # %if.end24
	movl	8(%rbx), %r8d
	movzbl	(%r14), %eax
	leaq	(%r8,%rsi), %r9
	cmpq	$47, %rax
	jne	.LBB22_6
# BB#4:                                 # %lor.lhs.false
	movq	8(%r14), %rax
	movzbl	(%rax), %ecx
	leaq	-132(%rcx), %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	cmpq	$2, %rdx
	jb	.LBB22_6
# BB#5:                                 # %lor.lhs.false
	cmpq	$64, %rcx
	jne	.LBB22_23
.LBB22_6:                               # %if.then51
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%r9, %rax
	cmpq	$8, %rax
	je	.LBB22_10
# BB#7:                                 # %if.then51
	leaq	-21(%r9), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB22_10
# BB#8:                                 # %if.then51
	leaq	-45(%r9), %rcx
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB22_10
# BB#9:                                 # %if.then51
	leaq	-29(%r9), %rcx
	andq	%rax, %rcx
	cmpq	$7, %rcx
	jbe	.LBB22_10
# BB#15:                                # %cond.false
	cmpq	$24, %r15
	jne	.LBB22_16
# BB#18:                                # %cond.true76
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB22_20
.LBB22_10:                              # %cond.true
	movl	mode_class(,%r15,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB22_20
# BB#11:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB22_12
# BB#13:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB22_20
.LBB22_16:                              # %cond.false
	cmpq	$18, %r15
	jne	.LBB22_19
# BB#17:                                # %cond.true72
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB22_20
.LBB22_12:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB22_20
.LBB22_19:                              # %cond.false80
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%r15), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
.LBB22_20:                              # %cond.end93
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	-1(%r9,%rax), %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rdx, %rbx
	jl	.LBB22_40
# BB#21:                                # %for.body.lr.ph
	movl	reload_combine_ruid(%rip), %r10d
	addq	%rsi, %rax
	leaq	-1(%r8,%rax), %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	imulq	$120, %rdi, %rcx
	leaq	reg_state+112(%rcx), %rsi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB22_22:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	%ebx, -16(%rsi)
	movl	%r10d, (%rsi)
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%rdi
	addq	$-120, %rsi
	cmpq	%rdx, %rax
	jg	.LBB22_22
	jmp	.LBB22_40
.LBB22_23:                              # %if.else
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%r9, %rcx
	cmpq	$8, %rcx
	je	.LBB22_27
# BB#24:                                # %if.else
	leaq	-21(%r9), %rcx
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB22_27
# BB#25:                                # %if.else
	leaq	-45(%r9), %rcx
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB22_27
# BB#26:                                # %if.else
	leaq	-29(%r9), %rcx
	andq	%rax, %rcx
	cmpq	$7, %rcx
	jbe	.LBB22_27
# BB#32:                                # %cond.false138
	cmpq	$24, %r15
	jne	.LBB22_33
# BB#35:                                # %cond.true148
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB22_37
.LBB22_27:                              # %cond.true126
	movl	mode_class(,%r15,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB22_37
# BB#28:                                # %lor.rhs131
	cmpq	$6, %rcx
	jne	.LBB22_29
# BB#30:                                # %lor.rhs131
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB22_37
.LBB22_33:                              # %cond.false138
	cmpq	$18, %r15
	jne	.LBB22_36
# BB#34:                                # %cond.true141
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB22_37
.LBB22_36:                              # %cond.false152
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%r15), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	jmp	.LBB22_37
.LBB22_29:
	movabsq	$0, %rax
	incq	%rax
.LBB22_37:                              # %cond.end169
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	-1(%r9,%rax), %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdx, %rdi
	jl	.LBB22_40
# BB#38:                                # %for.body176.lr.ph
	movl	reload_combine_ruid(%rip), %r10d
	addq	%rsi, %rax
	leaq	-1(%r8,%rax), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$120, %rax, %rcx
	leaq	reg_state+112(%rcx), %rsi
	movq	$6, %rbx
	.align	16, 0x90
.LBB22_39:                              # %for.body176
                                        # =>This Inner Loop Header: Depth=1
	movl	%r10d, (%rsi)
	movl	%ebx, -16(%rsi)
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%rax
	addq	$-120, %rsi
	cmpq	%rdx, %rdi
	jg	.LBB22_39
.LBB22_40:                              # %if.end186
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp324:
	.size	reload_combine_note_store, .Ltmp324-reload_combine_note_store
	.cfi_endproc

	.align	16, 0x90
	.type	reload_combine_note_use,@function
reload_combine_note_use:                # @reload_combine_note_use
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp331:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp332:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp333:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp334:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp335:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp336:
	.cfi_def_cfa_offset 64
.Ltmp337:
	.cfi_offset %rbx, -48
.Ltmp338:
	.cfi_offset %r12, -40
.Ltmp339:
	.cfi_offset %r13, -32
.Ltmp340:
	.cfi_offset %r14, -24
.Ltmp341:
	.cfi_offset %r15, -16
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %r14
	jmp	.LBB23_1
	.align	16, 0x90
.LBB23_11:                              # %if.then
                                        #   in Loop: Header=BB23_1 Depth=1
	addq	$16, %r13
	movq	%r13, %r14
.LBB23_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r14), %r13
	movl	(%r13), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$74, %rcx
	jg	.LBB23_29
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$60, %rcx
	jg	.LBB23_37
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$47, %rcx
	jne	.LBB23_4
# BB#10:                                # %sw.bb
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%r13), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	movq	$47, %rcx
	je	.LBB23_11
	jmp	.LBB23_32
.LBB23_29:                              # %tailrecurse
	cmpq	$75, %rcx
	jne	.LBB23_32
# BB#30:                                # %sw.bb108
	movq	8(%r13), %rdx
	movl	(%rdx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	movq	$75, %rcx
	jne	.LBB23_32
# BB#31:                                # %lor.lhs.false116
	movq	16(%r13), %rdi
	movzbl	(%rdi), %esi
	cmpq	$54, %rsi
	je	.LBB23_39
	jmp	.LBB23_32
.LBB23_37:                              # %tailrecurse
	cmpq	$61, %rcx
	jne	.LBB23_32
# BB#38:                                # %sw.bb132.loopexit
	movq	const_int_rtx+512(%rip), %rdi
	movq	%r13, %rdx
.LBB23_39:                              # %sw.bb132
	movslq	8(%rdx), %rsi
	cmpq	$53, %rsi
	jge	.LBB23_40
# BB#41:                                # %if.end142
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rsi, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB23_45
# BB#42:                                # %if.end142
	leaq	-21(%rsi), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB23_45
# BB#43:                                # %if.end142
	leaq	-45(%rsi), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB23_45
# BB#44:                                # %if.end142
	leaq	-29(%rsi), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB23_45
# BB#51:                                # %cond.false184
	cmpq	$24, %rax
	jne	.LBB23_52
# BB#54:                                # %cond.true200
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB23_56
.LBB23_45:                              # %cond.true166
	movl	mode_class(,%rax,4), %eax
	addq	$-5, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$2, %rcx
	movq	$2, %rax
	jb	.LBB23_57
	jmp	.LBB23_46
.LBB23_52:                              # %cond.false184
	cmpq	$18, %rax
	jne	.LBB23_55
# BB#53:                                # %cond.true190
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB23_56
.LBB23_55:                              # %cond.false204
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
.LBB23_56:                              # %cond.end224
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$2, %rdx
	jl	.LBB23_46
.LBB23_57:                              # %while.body233.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rax, %rsi
	leaq	-1(%rax), %rax
	imulq	$120, %rsi, %rcx
	leaq	reg_state-24(%rcx), %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB23_58:                              # %while.body233
                                        # =>This Inner Loop Header: Depth=1
	movl	%esi, (%rdx)
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	decq	%rax
	addq	$-120, %rdx
	testq	%rdi, %rdi
	jg	.LBB23_58
	jmp	.LBB23_50
.LBB23_46:                              # %if.end239
	imulq	$120, %rsi, %r12
	movslq	reg_state+96(%r12), %rax
	testq	%rax, %rax
	leaq	-1(%rax), %rbx
	movl	%ebx, reg_state+96(%r12)
	jle	.LBB23_50
# BB#47:                                # %if.end247
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	$5, %rax
	leaq	reg_state+104(%r12), %rax
	jne	.LBB23_48
# BB#59:                                # %if.else
	movl	reload_combine_ruid(%rip), %ecx
	movq	%rdi, (%rax)
	movl	%ecx, reg_state+116(%r12)
	jmp	.LBB23_60
.LBB23_48:                              # %if.then250
	movq	(%rax), %rsi
	callq	rtx_equal_p
	testq	%r15, %rax
	je	.LBB23_49
.LBB23_60:                              # %if.end265
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$4, %rcx
	shlq	%cl, %rbx
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, reg_state(%r12,%rbx)
	movq	%r14, reg_state+8(%r12,%rbx)
	jmp	.LBB23_50
.LBB23_49:                              # %if.then255
	leaq	reg_state+96(%r12), %rax
	movl	%r15d, (%rax)
	jmp	.LBB23_50
.LBB23_4:                               # %tailrecurse
	cmpq	$48, %rcx
	jne	.LBB23_5
# BB#12:                                # %sw.bb6
	movq	8(%r13), %rdx
	movl	(%rdx), %eax
	movq	%rax, %rcx
	andq	$1073807359, %rcx       # imm = 0x4000FFFF
	cmpq	$1073741885, %rcx       # imm = 0x4000003D
	movq	$48, %rcx
	jne	.LBB23_32
# BB#13:                                # %if.then18
	movl	8(%rdx), %edx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movabsq	$4294967288, %rdi       # imm = 0xFFFFFFF8
	andq	%rdx, %rdi
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdi
	je	.LBB23_17
# BB#14:                                # %if.then18
	leaq	-21(%rdx), %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdi
	cmpq	$8, %rdi
	jb	.LBB23_17
# BB#15:                                # %if.then18
	leaq	-45(%rdx), %rdi
	andq	%rcx, %rdi
	cmpq	$8, %rdi
	jb	.LBB23_17
# BB#16:                                # %if.then18
	addq	$-29, %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB23_17
# BB#22:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB23_23
# BB#25:                                # %cond.true61
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB23_27
.LBB23_5:                               # %tailrecurse
	cmpq	$49, %rcx
	jne	.LBB23_32
# BB#6:                                 # %sw.bb88
	movq	8(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	movq	$49, %rcx
	jne	.LBB23_32
# BB#7:                                 # %if.then96
	movl	8(%rax), %eax
	cmpq	$53, %rax
	jb	.LBB23_50
# BB#8:                                 # %if.then105
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.reload_combine_note_use, %rcx
	movq	$8994, %rsi             # imm = 0x2322
	jmp	.LBB23_9
.LBB23_32:                              # %sw.epilog
	movzbl	rtx_length(%rcx), %r14d
	testq	%r14, %r14
	je	.LBB23_50
# BB#33:                                # %for.body.lr.ph
	movq	rtx_format(,%rcx,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	.align	16, 0x90
.LBB23_34:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB23_64 Depth 2
	movq	%r14, %rbx
	movq	(%rsp), %rax            # 8-byte Reload
	movzbl	-1(%rax,%rbx), %eax
	leaq	-1(%rbx), %r14
	cmpq	$69, %rax
	jne	.LBB23_35
# BB#62:                                # %if.then300
                                        #   in Loop: Header=BB23_34 Depth=1
	movq	(%r13,%rbx,8), %rdi
	movslq	(%rdi), %r12
	testq	%r12, %r12
	jle	.LBB23_36
# BB#63:                                # %for.body308.lr.ph
                                        #   in Loop: Header=BB23_34 Depth=1
	movq	$3, %rcx
	leaq	-1(%r12), %r15
	shlq	%cl, %r12
	jmp	.LBB23_64
	.align	16, 0x90
.LBB23_65:                              # %for.body308.for.body308_crit_edge
                                        #   in Loop: Header=BB23_64 Depth=2
	movq	(%r13,%rbx,8), %rdi
	decq	%r15
	addq	$-8, %r12
.LBB23_64:                              # %for.body308
                                        #   Parent Loop BB23_34 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	addq	%r12, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	reload_combine_note_use
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB23_65
	jmp	.LBB23_36
	.align	16, 0x90
.LBB23_35:                              # %for.body
                                        #   in Loop: Header=BB23_34 Depth=1
	cmpq	$101, %rax
	jne	.LBB23_36
# BB#61:                                # %if.then289
                                        #   in Loop: Header=BB23_34 Depth=1
	leaq	8(%r13,%r14,8), %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	reload_combine_note_use
.LBB23_36:                              # %for.cond.backedge
                                        #   in Loop: Header=BB23_34 Depth=1
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB23_34
	jmp	.LBB23_50
.LBB23_17:                              # %cond.true
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB23_27
# BB#18:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB23_19
# BB#20:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB23_27
.LBB23_23:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB23_26
# BB#24:                                # %cond.true53
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB23_27
.LBB23_19:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB23_27
.LBB23_26:                              # %while.cond.preheader
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB23_50
.LBB23_27:                              # %while.body.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rax, %rsi
	leaq	-1(%rax), %rax
	imulq	$120, %rsi, %rcx
	leaq	reg_state-24(%rcx), %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
.LBB23_28:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movl	%esi, (%rdx)
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	decq	%rax
	addq	$-120, %rdx
	testq	%rdi, %rdi
	jg	.LBB23_28
.LBB23_50:                              # %for.end320
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB23_40:                              # %if.then141
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.reload_combine_note_use, %rcx
	movq	$9015, %rsi             # imm = 0x2337
.LBB23_9:                               # %if.then105
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp342:
	.size	reload_combine_note_use, .Ltmp342-reload_combine_note_use
	.cfi_endproc

	.align	16, 0x90
	.type	reload_cse_simplify_set,@function
reload_cse_simplify_set:                # @reload_cse_simplify_set
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp349:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp350:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp351:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp352:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp353:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp354:
	.cfi_def_cfa_offset 80
.Ltmp355:
	.cfi_offset %rbx, -48
.Ltmp356:
	.cfi_offset %r12, -40
.Ltmp357:
	.cfi_offset %r13, -32
.Ltmp358:
	.cfi_offset %r14, -24
.Ltmp359:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	8(%r14), %rdi
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	callq	true_regnum
	movq	%rax, %rbx
	movq	$32, %rcx
                                        # kill: RAX<def> RBX<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%r12, %r12
	testq	%rax, %rax
	js	.LBB24_30
# BB#1:                                 # %if.end
	movq	16(%r14), %r15
	movq	%r15, %rdi
	callq	side_effects_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB24_30
# BB#2:                                 # %lor.lhs.false
	movq	%r15, %rdi
	callq	true_regnum
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB24_30
# BB#3:                                 # %if.end8
	movq	$32, %rcx
	shlq	%cl, %rbx
	movl	(%r15), %edi
	movq	$32, %rcx
	sarq	%cl, %rbx
	movl	regclass_map(,%rbx,4), %eax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	leaq	16(%r14), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$133, %rcx
	jg	.LBB24_9
# BB#4:                                 # %if.end8
	addq	$-54, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB24_11
# BB#5:                                 # %if.end8
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB24_11
# BB#6:                                 # %if.end8
	cmpq	$7, %rcx
	jne	.LBB24_7
# BB#31:                                # %if.then52
	movl	8(%r15), %eax
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movl	regclass_map(,%rax,4), %esi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	ix86_register_move_cost
	jmp	.LBB24_12
.LBB24_9:                               # %if.end8
	cmpq	$134, %rcx
	je	.LBB24_11
# BB#10:                                # %if.end8
	cmpq	$140, %rcx
.LBB24_11:                              # %if.else61
	movq	$47, %rsi
	movq	%r15, %rdi
	callq	rtx_cost
.LBB24_12:                              # %if.end65
	movq	%rax, %r13
	movq	8(%r14), %rax
	movzbl	2(%rax), %esi
	xorq	%rdx, %rdx
	xorq	%r12, %r12
	movq	%r15, %rdi
	callq	cselib_lookup
	testq	%rax, %rax
	je	.LBB24_30
# BB#13:                                # %if.end75
	movq	16(%rax), %rbx
	xorq	%r12, %r12
	jmp	.LBB24_14
.LBB24_29:                              # %for.inc
                                        #   in Loop: Header=BB24_14 Depth=1
	movq	(%rbx), %rbx
.LBB24_14:                              # %if.end75
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB24_30
# BB#15:                                # %for.body
                                        #   in Loop: Header=BB24_14 Depth=1
	movq	8(%rbx), %r15
	movl	(%r15), %edi
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$133, %rcx
	jg	.LBB24_18
# BB#16:                                # %for.body
                                        #   in Loop: Header=BB24_14 Depth=1
	addq	$-54, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB24_22
# BB#17:                                # %for.body
                                        #   in Loop: Header=BB24_14 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB24_20
	jmp	.LBB24_22
.LBB24_18:                              # %for.body
                                        #   in Loop: Header=BB24_14 Depth=1
	cmpq	$134, %rcx
	je	.LBB24_20
# BB#19:                                # %for.body
                                        #   in Loop: Header=BB24_14 Depth=1
	cmpq	$140, %rcx
	jne	.LBB24_22
.LBB24_20:                              # %land.lhs.true
                                        #   in Loop: Header=BB24_14 Depth=1
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	references_value_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB24_21
# BB#32:                                # %if.then110
                                        #   in Loop: Header=BB24_14 Depth=1
	movq	$47, %rsi
	movq	%r15, %rdi
	callq	rtx_cost
	jmp	.LBB24_24
.LBB24_21:                              # %land.lhs.true.if.else112_crit_edge
                                        #   in Loop: Header=BB24_14 Depth=1
	movl	(%r15), %edi
.LBB24_22:                              # %if.else112
                                        #   in Loop: Header=BB24_14 Depth=1
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB24_29
# BB#23:                                # %if.then116
                                        #   in Loop: Header=BB24_14 Depth=1
	movl	8(%r15), %eax
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movl	regclass_map(,%rax,4), %esi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	ix86_register_move_cost
.LBB24_24:                              # %if.end128
                                        #   in Loop: Header=BB24_14 Depth=1
	movq	%rax, %r14
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB24_28
# BB#25:                                # %lor.lhs.false130
                                        #   in Loop: Header=BB24_14 Depth=1
	movq	%r13, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	movq	%r14, %rcx
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	jne	.LBB24_29
# BB#26:                                # %land.lhs.true132
                                        #   in Loop: Header=BB24_14 Depth=1
	movzbl	(%r15), %eax
	cmpq	$61, %rax
	jne	.LBB24_29
# BB#27:                                # %land.lhs.true136
                                        #   in Loop: Header=BB24_14 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	je	.LBB24_29
.LBB24_28:                              # %if.then143
                                        #   in Loop: Header=BB24_14 Depth=1
	movq	%r15, %rdi
	callq	copy_rtx
	movq	$1, %rcx
	movq	$1, %r12
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	validate_change
	movq	%r14, %r13
	jmp	.LBB24_29
.LBB24_30:                              # %return
	movq	%r12, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB24_7:                               # %if.end8
	cmpq	$12, %rcx
	jne	.LBB24_11
# BB#8:                                 # %if.then11
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movq	$1, %rdx
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	ix86_memory_move_cost
	jmp	.LBB24_12
.Ltmp360:
	.size	reload_cse_simplify_set, .Ltmp360-reload_cse_simplify_set
	.cfi_endproc

	.align	16, 0x90
	.type	reload_cse_simplify_operands,@function
reload_cse_simplify_operands:           # @reload_cse_simplify_operands
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp364:
	.cfi_def_cfa_offset 16
.Ltmp365:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp366:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$600, %rsp              # imm = 0x258
.Ltmp367:
	.cfi_offset %rbx, -56
.Ltmp368:
	.cfi_offset %r12, -48
.Ltmp369:
	.cfi_offset %r13, -40
.Ltmp370:
	.cfi_offset %r14, -32
.Ltmp371:
	.cfi_offset %r15, -24
	movq	%rdi, %r12
	movq	%r12, -624(%rbp)        # 8-byte Spill
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_REG
	movq	%rax, -576(%rbp)        # 8-byte Spill
	movq	%r12, %rdi
	callq	extract_insn
	movzbl	recog_data+998(%rip), %eax
	testq	%rax, %rax
	je	.LBB25_124
# BB#1:                                 # %entry
	movzbl	recog_data+996(%rip), %eax
	testq	$255, %rax
	je	.LBB25_124
# BB#2:                                 # %if.end
	movq	$1, %rdi
	callq	constrain_operands
	testq	%rbx, %rax
	movq	%rbx, %r14
	je	.LBB25_125
# BB#3:                                 # %if.end7
	movsbq	recog_data+998(%rip), %r15
	movq	$2, %rcx
	movq	%rsp, %rdi
	leaq	15(,%r15,4), %rax
	shlq	%cl, %r15
	andq	$-16, %rax
	subq	%rax, %rdi
	movq	%rdi, -536(%rbp)        # 8-byte Spill
	movq	%rdi, %rsp
	movq	%rsp, %r13
	subq	%rax, %r13
	movq	%r13, -544(%rbp)        # 8-byte Spill
	movq	%r13, %rsp
	movq	%rsp, %rcx
	subq	%rax, %rcx
	movq	%rcx, -616(%rbp)        # 8-byte Spill
	movq	%rcx, %rsp
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%r15, %rdx
	callq	memset
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	movq	%r15, %rdx
	callq	memset
	movl	recog_data+996(%rip), %r15d
	movq	$56, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	movq	$16, %rcx
	movq	%r15, %rdx
	shrq	%cl, %rdx
	testq	%rax, %rax
	jle	.LBB25_25
	.align	16, 0x90
.LBB25_4:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB25_21 Depth 2
	movq	recog_data(,%rbx,8), %rdi
	movq	$0, -288(%rbp,%rbx,8)
	movzbl	(%rdi), %ecx
	cmpq	$53, %rcx
	jg	.LBB25_12
# BB#5:                                 # %for.body
                                        #   in Loop: Header=BB25_4 Depth=1
	cmpq	$36, %rcx
	jne	.LBB25_18
	jmp	.LBB25_6
	.align	16, 0x90
.LBB25_12:                              # %for.body
                                        #   in Loop: Header=BB25_4 Depth=1
	cmpq	$133, %rcx
	jg	.LBB25_16
# BB#13:                                # %for.body
                                        #   in Loop: Header=BB25_4 Depth=1
	addq	$-54, %rcx
	andq	%r14, %rcx
	cmpq	$14, %rcx
	ja	.LBB25_18
# BB#14:                                # %for.body
                                        #   in Loop: Header=BB25_4 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB25_15
	jmp	.LBB25_18
	.align	16, 0x90
.LBB25_16:                              # %for.body
                                        #   in Loop: Header=BB25_4 Depth=1
	cmpq	$134, %rcx
	je	.LBB25_15
# BB#17:                                # %for.body
                                        #   in Loop: Header=BB25_4 Depth=1
	cmpq	$140, %rcx
	jne	.LBB25_18
.LBB25_15:                              # %land.lhs.true
                                        #   in Loop: Header=BB25_4 Depth=1
	movl	recog_data+720(,%rbx,4), %esi
	testq	%rsi, %rsi
	je	.LBB25_6
	jmp	.LBB25_19
	.align	16, 0x90
.LBB25_18:                              # %for.body.if.end85_crit_edge
                                        #   in Loop: Header=BB25_4 Depth=1
	movl	recog_data+720(,%rbx,4), %esi
.LBB25_19:                              # %if.end85
                                        #   in Loop: Header=BB25_4 Depth=1
	xorq	%rdx, %rdx
	callq	cselib_lookup
	testq	%rax, %rax
	je	.LBB25_6
# BB#20:                                # %if.end93
                                        #   in Loop: Header=BB25_4 Depth=1
	movq	16(%rax), %rax
	xorq	%rdx, %rdx
	jmp	.LBB25_21
	.align	16, 0x90
.LBB25_24:                              # %for.inc
                                        #   in Loop: Header=BB25_21 Depth=2
	movq	(%rax), %rax
.LBB25_21:                              # %if.end93
                                        #   Parent Loop BB25_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rax, %rax
	je	.LBB25_6
# BB#22:                                # %for.body96
                                        #   in Loop: Header=BB25_21 Depth=2
	movq	8(%rax), %rcx
	movzbl	(%rcx), %esi
	cmpq	$61, %rsi
	jne	.LBB25_24
# BB#23:                                # %if.then101
                                        #   in Loop: Header=BB25_21 Depth=2
	movl	8(%rcx), %ecx
	movq	$1, %rsi
	shlq	%cl, %rsi
	orq	%rsi, %rdx
	movq	%rdx, -288(%rbp,%rbx,8)
	jmp	.LBB25_24
	.align	16, 0x90
.LBB25_6:                               # %for.inc107
                                        #   in Loop: Header=BB25_4 Depth=1
	movl	recog_data+996(%rip), %r15d
	incq	%rbx
	movq	$56, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$16, %rcx
	movq	%r15, %rdx
	shrq	%cl, %rdx
	cmpq	%rax, %rsi
	jl	.LBB25_4
# BB#7:                                 # %for.cond109.preheader
	movq	$56, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB25_25
# BB#8:                                 # %for.body113.lr.ph
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB25_9:                               # %for.body113
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB25_30 Depth 2
                                        #       Child Loop BB25_32 Depth 3
                                        #     Child Loop BB25_43 Depth 2
                                        #       Child Loop BB25_45 Depth 3
	movq	$56, %rcx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	leaq	15(,%rdx,4), %rax
	andq	$-16, %rax
	movq	%rsp, %rcx
	subq	%rax, %rcx
	movq	%rcx, -560(%rbp)        # 8-byte Spill
	movq	%rcx, %rsp
	testq	%rdx, %rdx
	movq	%rcx, -528(%rbp,%rbx,8)
	jle	.LBB25_29
# BB#10:                                # %for.body122.lr.ph
                                        #   in Loop: Header=BB25_9 Depth=1
	movsbq	recog_data+998(%rip), %rax
	cmpq	$1, %rax
	jg	.LBB25_11
# BB#27:                                # %for.body122.lr.ph
                                        #   in Loop: Header=BB25_9 Depth=1
	movq	$4, %rdx
	jmp	.LBB25_28
	.align	16, 0x90
.LBB25_11:                              #   in Loop: Header=BB25_9 Depth=1
	movabsq	$17179869180, %rcx      # imm = 0x3FFFFFFFC
	leaq	(%rcx,%rax,4), %rdx
	andq	%rcx, %rdx
	addq	$4, %rdx
.LBB25_28:                              # %for.body122.lr.ph
                                        #   in Loop: Header=BB25_9 Depth=1
	movq	$255, %rsi
	movq	-560(%rbp), %rdi        # 8-byte Reload
	callq	memset
.LBB25_29:                              # %for.end129
                                        #   in Loop: Header=BB25_9 Depth=1
	movq	recog_data+480(,%rbx,8), %rcx
	movq	%rcx, -608(%rbp)        # 8-byte Spill
	movzbl	(%rcx), %edx
	movl	recog_data+720(,%rbx,4), %eax
	movq	%rax, -584(%rbp)        # 8-byte Spill
	xorq	%rax, %rax
	jmp	.LBB25_30
.LBB25_35:                              # %if.then142
                                        #   in Loop: Header=BB25_30 Depth=2
	movzbl	(%rcx), %edx
	incq	%rax
	.align	16, 0x90
.LBB25_30:                              # %for.end129
                                        #   Parent Loop BB25_9 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB25_32 Depth 3
	testq	%rdx, %rdx
	je	.LBB25_39
# BB#31:                                # %while.body.lr.ph
                                        #   in Loop: Header=BB25_30 Depth=2
	incq	%rcx
	.align	16, 0x90
.LBB25_32:                              # %while.body
                                        #   Parent Loop BB25_9 Depth=1
                                        #     Parent Loop BB25_30 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	andq	$255, %rdx
	cmpq	$33, %rdx
	je	.LBB25_126
# BB#33:                                # %while.body
                                        #   in Loop: Header=BB25_32 Depth=3
	cmpq	$63, %rdx
	jne	.LBB25_34
# BB#36:                                # %if.then147
                                        #   in Loop: Header=BB25_32 Depth=3
	movq	-536(%rbp), %rsi        # 8-byte Reload
	movl	(%rsi,%rax,4), %edx
	addq	$3, %rdx
	jmp	.LBB25_37
	.align	16, 0x90
.LBB25_126:                             # %if.then154
                                        #   in Loop: Header=BB25_32 Depth=3
	movq	-536(%rbp), %rsi        # 8-byte Reload
	movl	(%rsi,%rax,4), %edx
	addq	$300, %rdx              # imm = 0x12C
.LBB25_37:                              # %while.cond.backedge
                                        #   in Loop: Header=BB25_32 Depth=3
	movl	%edx, (%rsi,%rax,4)
.LBB25_38:                              # %while.cond.backedge
                                        #   in Loop: Header=BB25_32 Depth=3
	movzbl	(%rcx), %edx
	incq	%rcx
	testq	%rdx, %rdx
	jne	.LBB25_32
	jmp	.LBB25_39
.LBB25_34:                              # %while.body
                                        #   in Loop: Header=BB25_32 Depth=3
	cmpq	$44, %rdx
	jne	.LBB25_38
	jmp	.LBB25_35
	.align	16, 0x90
.LBB25_39:                              # %while.end
                                        #   in Loop: Header=BB25_9 Depth=1
	movq	recog_data(,%rbx,8), %rdi
	movq	%rbx, -592(%rbp)        # 8-byte Spill
	callq	true_regnum
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB25_99
# BB#40:                                # %lor.lhs.false166
                                        #   in Loop: Header=BB25_9 Depth=1
	movq	-608(%rbp), %rax        # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$43, %rax
	je	.LBB25_99
# BB#41:                                # %lor.lhs.false166
                                        #   in Loop: Header=BB25_9 Depth=1
	cmpq	$61, %rax
	je	.LBB25_99
# BB#42:                                # %for.cond182.preheader
                                        #   in Loop: Header=BB25_9 Depth=1
	movq	$16, %rcx
	movq	-584(%rbp), %rdx        # 8-byte Reload
	movq	-592(%rbp), %rax        # 8-byte Reload
	movq	-288(%rbp,%rax,8), %rax
	movq	%rax, -600(%rbp)        # 8-byte Spill
	shlq	%cl, %rdx
	andq	$16711680, %rdx         # imm = 0xFF0000
	movq	%rdx, -632(%rbp)        # 8-byte Spill
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB25_43:                              # %for.body185
                                        #   Parent Loop BB25_9 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB25_45 Depth 3
	movq	%rcx, -568(%rbp)        # 8-byte Spill
	movq	$1, %rax
	shlq	%cl, %rax
	movq	-600(%rbp), %rcx        # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB25_98
# BB#44:                                # %if.end192
                                        #   in Loop: Header=BB25_43 Depth=2
	movq	-576(%rbp), %rcx        # 8-byte Reload
	movq	-568(%rbp), %rax        # 8-byte Reload
	movl	%eax, 8(%rcx)
	movl	(%rcx), %eax
	movabsq	$4278255615, %rdx       # imm = 0xFF00FFFF
	andq	%rdx, %rax
	movq	-632(%rbp), %rdx        # 8-byte Reload
	orq	%rdx, %rax
	movl	%eax, (%rcx)
	xorq	%r12, %r12
	movq	-608(%rbp), %rbx        # 8-byte Reload
	movq	$0, -552(%rbp)          # 8-byte Folded Spill
	.align	16, 0x90
.LBB25_45:                              # %for.cond200
                                        #   Parent Loop BB25_9 Depth=1
                                        #     Parent Loop BB25_43 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movsbq	(%rbx), %r13
	incq	%rbx
	cmpq	$32, %r13
	jg	.LBB25_51
# BB#46:                                # %for.cond200
                                        #   in Loop: Header=BB25_45 Depth=3
	testq	%r14, %r13
	je	.LBB25_91
	jmp	.LBB25_47
	.align	16, 0x90
.LBB25_51:                              # %for.cond200
                                        #   in Loop: Header=BB25_45 Depth=3
	leaq	-33(%r13), %rax
	andq	%r14, %rax
	cmpq	$82, %rax
	ja	.LBB25_47
# BB#52:                                # %for.cond200
                                        #   in Loop: Header=BB25_45 Depth=3
	jmpq	*.LJTI25_0(,%rax,8)
.LBB25_53:                              # %sw.bb204
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	imulq	$100, %r12, %rax
	movl	reg_class_subunion+48(%rax), %r12d
	jmp	.LBB25_97
	.align	16, 0x90
.LBB25_47:                              # %sw.default
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	%r13, %rcx
	andq	$255, %rcx
	movq	$12, %rax
	cmpq	$114, %rcx
	je	.LBB25_90
# BB#48:                                # %cond.false
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	$11, %rax
	cmpq	$82, %rcx
	je	.LBB25_90
# BB#49:                                # %cond.false
                                        #   in Loop: Header=BB25_45 Depth=3
	cmpq	$113, %rcx
	jne	.LBB25_54
# BB#50:                                # %cond.true219
                                        #   in Loop: Header=BB25_45 Depth=3
	movl	target_flags(%rip), %eax
	andq	$33554432, %rax         # imm = 0x2000000
	movq	$23, %rcx
	shrq	%cl, %rax
	orq	$8, %rax
	jmp	.LBB25_90
	.align	16, 0x90
.LBB25_91:                              # %sw.bb358
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	$32, %rcx
	movq	-552(%rbp), %r15        # 8-byte Reload
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	-560(%rbp), %rax        # 8-byte Reload
	movl	(%rax,%r15,4), %eax
	cmpq	%r14, %rax
	jne	.LBB25_96
# BB#92:                                # %land.lhs.true365
                                        #   in Loop: Header=BB25_45 Depth=3
	xorq	%rdx, %rdx
	movq	-576(%rbp), %rdi        # 8-byte Reload
	movq	%r12, %rsi
	movq	-584(%rbp), %rcx        # 8-byte Reload
	callq	reg_fits_class_p
	testq	%r14, %rax
	je	.LBB25_96
# BB#93:                                # %land.lhs.true368
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	-592(%rbp), %rax        # 8-byte Reload
	movq	recog_data(,%rax,8), %rdi
	movzbl	(%rdi), %eax
	cmpq	$54, %rax
	jne	.LBB25_95
# BB#94:                                # %lor.lhs.false375
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	$47, %rsi
	callq	rtx_cost
	movq	%rax, %r14
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	$47, %rsi
	movq	-576(%rbp), %rdi        # 8-byte Reload
	callq	rtx_cost
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r14
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jle	.LBB25_96
.LBB25_95:                              # %if.then382
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	-544(%rbp), %rcx        # 8-byte Reload
	movl	(%rcx,%r15,4), %eax
	incq	%rax
	movl	%eax, (%rcx,%r15,4)
	movq	-560(%rbp), %rax        # 8-byte Reload
	movq	-568(%rbp), %rcx        # 8-byte Reload
	movl	%ecx, (%rax,%r15,4)
	.align	16, 0x90
.LBB25_96:                              # %if.end390
                                        #   in Loop: Header=BB25_45 Depth=3
	incq	-552(%rbp)              # 8-byte Folded Spill
	jmp	.LBB25_97
.LBB25_54:                              # %cond.false222
                                        #   in Loop: Header=BB25_45 Depth=3
	cmpq	$101, %r13
	jg	.LBB25_67
# BB#55:                                # %cond.false222
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	$8, %rax
	cmpq	$81, %rcx
	je	.LBB25_90
	jmp	.LBB25_56
.LBB25_67:                              # %cond.false222
                                        #   in Loop: Header=BB25_45 Depth=3
	cmpq	$117, %rcx
	je	.LBB25_78
# BB#68:                                # %cond.false222
                                        #   in Loop: Header=BB25_45 Depth=3
	cmpq	$116, %rcx
	jne	.LBB25_69
# BB#74:                                # %cond.true241
                                        #   in Loop: Header=BB25_45 Depth=3
	movl	target_flags(%rip), %edx
	testq	$1, %rdx
	movq	$13, %rax
	jne	.LBB25_90
# BB#75:                                # %lor.rhs244
                                        #   in Loop: Header=BB25_45 Depth=3
	andq	$32, %rdx
	movq	$5, %rcx
	shrq	%cl, %rdx
	testq	%rdx, %rdx
	je	.LBB25_77
# BB#76:                                # %lor.rhs244
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	$13, %rdx
.LBB25_77:                              # %lor.rhs244
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	%rdx, %rax
	jmp	.LBB25_90
.LBB25_78:                              # %cond.true253
                                        #   in Loop: Header=BB25_45 Depth=3
	movl	target_flags(%rip), %edx
	testq	$1, %rdx
	movq	$14, %rax
	jne	.LBB25_90
# BB#79:                                # %lor.rhs256
                                        #   in Loop: Header=BB25_45 Depth=3
	andq	$32, %rdx
	movq	$5, %rcx
	shrq	%cl, %rdx
	testq	%rdx, %rdx
	je	.LBB25_81
# BB#80:                                # %lor.rhs256
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	$14, %rdx
.LBB25_81:                              # %lor.rhs256
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	%rdx, %rax
	jmp	.LBB25_90
.LBB25_69:                              # %cond.false222
                                        #   in Loop: Header=BB25_45 Depth=3
	cmpq	$102, %rcx
	jne	.LBB25_56
# BB#70:                                # %cond.true231
                                        #   in Loop: Header=BB25_45 Depth=3
	movl	target_flags(%rip), %edx
	testq	$1, %rdx
	movq	$15, %rax
	jne	.LBB25_90
# BB#71:                                # %lor.rhs
                                        #   in Loop: Header=BB25_45 Depth=3
	andq	$32, %rdx
	movq	$5, %rcx
	shrq	%cl, %rdx
	testq	%rdx, %rdx
	je	.LBB25_73
# BB#72:                                # %lor.rhs
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	$15, %rdx
.LBB25_73:                              # %lor.rhs
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	%rdx, %rax
	jmp	.LBB25_90
.LBB25_56:                              # %cond.false261
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	$1, %rax
	cmpq	$97, %rcx
	je	.LBB25_90
# BB#57:                                # %cond.false266
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	$4, %rax
	cmpq	$98, %rcx
	je	.LBB25_90
# BB#58:                                # %cond.false271
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	$3, %rax
	cmpq	$99, %rcx
	je	.LBB25_90
# BB#59:                                # %cond.false276
                                        #   in Loop: Header=BB25_45 Depth=3
	cmpq	$119, %r13
	jg	.LBB25_82
# BB#60:                                # %cond.false276
                                        #   in Loop: Header=BB25_45 Depth=3
	cmpq	$89, %rcx
	jne	.LBB25_61
# BB#87:                                # %cond.true294
                                        #   in Loop: Header=BB25_45 Depth=3
	movl	target_flags(%rip), %eax
	andq	$262144, %rax           # imm = 0x40000
	movq	$14, %rcx
	shrq	%cl, %rax
	jmp	.LBB25_90
.LBB25_82:                              # %cond.false276
                                        #   in Loop: Header=BB25_45 Depth=3
	cmpq	$121, %rcx
	jne	.LBB25_83
# BB#88:                                # %cond.true303
                                        #   in Loop: Header=BB25_45 Depth=3
	movzbl	target_flags+1(%rip), %eax
	andq	$64, %rax
	movq	$6, %rcx
	shrq	%cl, %rax
	testq	%rax, %rax
	je	.LBB25_90
# BB#89:                                # %cond.true303
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	$17, %rax
	jmp	.LBB25_90
.LBB25_61:                              # %cond.false276
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	$2, %rax
	cmpq	$100, %rcx
	je	.LBB25_90
	jmp	.LBB25_62
.LBB25_83:                              # %cond.false276
                                        #   in Loop: Header=BB25_45 Depth=3
	cmpq	$120, %rcx
	jne	.LBB25_62
# BB#84:                                # %cond.true285
                                        #   in Loop: Header=BB25_45 Depth=3
	movzbl	target_flags+2(%rip), %eax
	testq	$5, %rax
	movabsq	$0, %rax
	je	.LBB25_86
# BB#85:                                # %cond.true285
                                        #   in Loop: Header=BB25_45 Depth=3
	movabsq	$1, %rax
.LBB25_86:                              # %cond.true285
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	$4, %rcx
	shlq	%cl, %rax
	jmp	.LBB25_90
.LBB25_62:                              # %cond.false308
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	$7, %rax
	cmpq	$65, %rcx
	je	.LBB25_90
# BB#63:                                # %cond.false313
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	$6, %rax
	cmpq	$68, %rcx
	je	.LBB25_90
# BB#64:                                # %cond.false318
                                        #   in Loop: Header=BB25_45 Depth=3
	cmpq	$83, %rcx
	movabsq	$0, %rax
	jne	.LBB25_66
# BB#65:                                # %cond.false318
                                        #   in Loop: Header=BB25_45 Depth=3
	movabsq	$1, %rax
.LBB25_66:                              # %cond.false318
                                        #   in Loop: Header=BB25_45 Depth=3
	leaq	(%rax,%rax,4), %rax
	.align	16, 0x90
.LBB25_90:                              # %cond.end352
                                        #   in Loop: Header=BB25_45 Depth=3
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	imulq	$100, %r12, %rcx
	movl	reg_class_subunion(%rcx,%rax,4), %r12d
.LBB25_97:                              # %sw.epilog
                                        #   in Loop: Header=BB25_45 Depth=3
	testq	$255, %r13
	jne	.LBB25_45
.LBB25_98:                              # %for.inc398
                                        #   in Loop: Header=BB25_43 Depth=2
	movq	-568(%rbp), %rcx        # 8-byte Reload
	incq	%rcx
	cmpq	$53, %rcx
	jne	.LBB25_43
	.align	16, 0x90
.LBB25_99:                              # %for.inc401
                                        #   in Loop: Header=BB25_9 Depth=1
	movl	recog_data+996(%rip), %r15d
	movq	-592(%rbp), %rbx        # 8-byte Reload
	incq	%rbx
	movq	$56, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$16, %rcx
	movq	%r15, %rdx
	shrq	%cl, %rdx
	cmpq	%rax, %rsi
	movq	-624(%rbp), %r12        # 8-byte Reload
	jl	.LBB25_9
.LBB25_25:                              # %for.cond404.preheader
	movq	%r12, -624(%rbp)        # 8-byte Spill
	movq	$56, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB25_26
# BB#100:                               # %for.body408.lr.ph
	movslq	which_alternative(%rip), %rax
	movq	-536(%rbp), %rcx        # 8-byte Reload
	movl	(%rcx,%rax,4), %ebx
	movq	$56, %rcx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	xorq	%rdi, %rdi
	xorq	%r12, %r12
	.align	16, 0x90
.LBB25_101:                             # %for.body408
                                        # =>This Inner Loop Header: Depth=1
	movq	-536(%rbp), %rax        # 8-byte Reload
	movslq	(%rax,%rdi,4), %rax
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rax
	jg	.LBB25_103
# BB#102:                               # %if.then415
                                        #   in Loop: Header=BB25_101 Depth=1
	movq	$32, %rcx
	leaq	1(%r12), %rax
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	-616(%rbp), %rcx        # 8-byte Reload
	movl	%edi, (%rcx,%r12,4)
	movq	%rax, %r12
.LBB25_103:                             # %for.inc420
                                        #   in Loop: Header=BB25_101 Depth=1
	incq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB25_101
# BB#104:                               # %for.end422
	movb	%r12b, recog_data+998(%rip)
	movq	$56, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	decq	%rax
	testq	%rax, %rax
	movq	-616(%rbp), %rdx        # 8-byte Reload
	jle	.LBB25_114
# BB#105:                               # %for.body428.lr.ph
	movq	$56, %rcx
	shlq	%cl, %r12
	movq	$56, %rcx
	sarq	%cl, %r12
	leaq	-1(%r12), %rax
	movq	%rax, -576(%rbp)        # 8-byte Spill
	movq	$1, -568(%rbp)          # 8-byte Folded Spill
	xorq	%r10, %r10
	.align	16, 0x90
.LBB25_106:                             # %for.body428
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB25_108 Depth 2
	movslq	(%rdx,%r10,4), %rax
	movq	%rax, -552(%rbp)        # 8-byte Spill
	leaq	1(%r10), %rax
	movq	%rax, -560(%rbp)        # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r12, %rax
	movq	%r10, %r13
	jge	.LBB25_113
# BB#107:                               # %for.body442.lr.ph
                                        #   in Loop: Header=BB25_106 Depth=1
	movq	-544(%rbp), %rax        # 8-byte Reload
	movq	-552(%rbp), %rcx        # 8-byte Reload
	movl	(%rax,%rcx,4), %r11d
	movq	-536(%rbp), %rax        # 8-byte Reload
	movl	(%rax,%rcx,4), %r8d
	movq	-568(%rbp), %rbx        # 8-byte Reload
	movq	%r10, %r13
	.align	16, 0x90
.LBB25_108:                             # %for.body442
                                        #   Parent Loop BB25_106 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	(%rdx,%rbx,4), %rdi
	movq	-536(%rbp), %rax        # 8-byte Reload
	movslq	(%rax,%rdi,4), %rsi
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	-544(%rbp), %rcx        # 8-byte Reload
	movl	(%rcx,%rdi,4), %r9d
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rsi
	jl	.LBB25_111
# BB#109:                               # %lor.lhs.false455
                                        #   in Loop: Header=BB25_108 Depth=2
	movq	%r8, %rax
	andq	%r14, %rax
	movq	%rsi, %rcx
	andq	%r14, %rcx
	cmpq	%rax, %rcx
	jne	.LBB25_112
# BB#110:                               # %lor.lhs.false455
                                        #   in Loop: Header=BB25_108 Depth=2
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r9, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rax, %rdi
	jge	.LBB25_112
.LBB25_111:                             # %if.then461
                                        #   in Loop: Header=BB25_108 Depth=2
	movq	%rbx, %r13
	movq	%rsi, %r8
	movq	%r9, %r11
.LBB25_112:                             # %for.inc463
                                        #   in Loop: Header=BB25_108 Depth=2
	incq	%rbx
	movq	%rbx, %rax
	andq	%r14, %rax
	movq	%r12, %rcx
	andq	%r14, %rcx
	cmpq	%rcx, %rax
	jne	.LBB25_108
.LBB25_113:                             # %for.end465
                                        #   in Loop: Header=BB25_106 Depth=1
	movq	$32, %rcx
	movq	-560(%rbp), %rdi        # 8-byte Reload
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movl	(%rdx,%r13,4), %ecx
	incq	-568(%rbp)              # 8-byte Folded Spill
	movq	-576(%rbp), %rsi        # 8-byte Reload
	cmpq	%rsi, %rax
	movq	-552(%rbp), %rax        # 8-byte Reload
	movl	%eax, (%rdx,%r13,4)
	movl	%ecx, (%rdx,%r10,4)
	movq	%rdi, %r10
	jl	.LBB25_106
	jmp	.LBB25_114
.LBB25_26:                              # %for.end422.thread
	xorq	%rax, %rax
	movb	%al, recog_data+998(%rip)
	movq	-616(%rbp), %rdx        # 8-byte Reload
.LBB25_114:                             # %for.end476
	movq	$56, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movslq	(%rdx), %r12
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	-624(%rbp), %r13        # 8-byte Reload
	jle	.LBB25_119
# BB#115:                               # %for.body482.lr.ph
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB25_116:                             # %for.body482
                                        # =>This Inner Loop Header: Depth=1
	movq	-528(%rbp,%rbx,8), %rax
	movl	(%rax,%r12,4), %esi
	cmpq	%r14, %rsi
	je	.LBB25_118
# BB#117:                               # %if.end494
                                        #   in Loop: Header=BB25_116 Depth=1
	movq	%r14, %r15
	movq	recog_data+240(,%rbx,8), %r14
	movl	recog_data+720(,%rbx,4), %edi
	callq	gen_rtx_REG
	movq	$1, %rcx
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%r15, %r14
	movq	%rax, %rdx
	callq	validate_change
	movzbl	recog_data+996(%rip), %r15d
.LBB25_118:                             # %for.inc503
                                        #   in Loop: Header=BB25_116 Depth=1
	incq	%rbx
	movq	$56, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB25_116
.LBB25_119:                             # %for.end505
	movq	$48, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB25_123
# BB#120:                               # %for.body511.lr.ph
	movq	$48, %rcx
	shlq	%cl, %r15
	movq	$56, %rcx
	sarq	%cl, %r15
	decq	%r15
	.align	16, 0x90
.LBB25_121:                             # %for.body511
                                        # =>This Inner Loop Header: Depth=1
	movsbq	recog_data+992(%r15), %rax
	movq	-528(%rbp,%rax,8), %rcx
	movl	(%rcx,%r12,4), %esi
	cmpq	%r14, %rsi
	je	.LBB25_122
# BB#127:                               # %if.end527
                                        #   in Loop: Header=BB25_121 Depth=1
	movq	%r14, %rbx
	movq	recog_data+960(,%r15,8), %r14
	movl	recog_data+720(,%rax,4), %edi
	callq	gen_rtx_REG
	movq	$1, %rcx
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%rbx, %r14
	movq	%rax, %rdx
	callq	validate_change
.LBB25_122:                             # %for.cond508.backedge
                                        #   in Loop: Header=BB25_121 Depth=1
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%r15
	testq	%rax, %rax
	jg	.LBB25_121
.LBB25_123:                             # %for.end537
	callq	apply_change_group
.LBB25_124:                             # %cleanup
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB25_125:                             # %if.then6
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.reload_cse_simplify_operands, %rcx
	movq	$8371, %rdx             # imm = 0x20B3
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	_fatal_insn_not_found
.Ltmp372:
	.size	reload_cse_simplify_operands, .Ltmp372-reload_cse_simplify_operands
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI25_0:
	.quad	.LBB25_97
	.quad	.LBB25_47
	.quad	.LBB25_97
	.quad	.LBB25_47
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_91
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_97
	.quad	.LBB25_47
	.quad	.LBB25_97
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_53
	.quad	.LBB25_47
	.quad	.LBB25_97
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_47
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_97
	.quad	.LBB25_47
	.quad	.LBB25_53
	.quad	.LBB25_97

	.text
	.align	16, 0x90
	.type	set_offsets_for_label,@function
set_offsets_for_label:                  # @set_offsets_for_label
	.cfi_startproc
# BB#0:                                 # %entry
	movslq	48(%rdi), %rdx
	xorq	%rax, %rax
	movl	%eax, num_not_at_initial_offset(%rip)
	movq	offsets_at(%rip), %rsi
	movq	reg_eliminate(%rip), %rax
	movq	$4, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movl	(%rsi,%rdi), %ecx
	movl	%ecx, 24(%rax)
	movl	%ecx, 20(%rax)
	movl	12(%rax), %edi
	testq	%rdi, %rdi
	je	.LBB26_3
# BB#1:                                 # %land.lhs.true
	movl	8(%rax), %r8d
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	%r8, %rcx
	je	.LBB26_3
# BB#2:                                 # %if.then
	movl	num_not_at_initial_offset(%rip), %ecx
	incq	%rcx
	movl	%ecx, num_not_at_initial_offset(%rip)
.LBB26_3:                               # %for.inc
	movq	$4, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movl	4(%rsi,%rdi), %ecx
	movl	%ecx, 72(%rax)
	movl	%ecx, 68(%rax)
	movl	60(%rax), %edi
	testq	%rdi, %rdi
	je	.LBB26_6
# BB#4:                                 # %land.lhs.true.1
	movl	56(%rax), %r8d
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	%r8, %rcx
	je	.LBB26_6
# BB#5:                                 # %if.then.1
	movl	num_not_at_initial_offset(%rip), %ecx
	incq	%rcx
	movl	%ecx, num_not_at_initial_offset(%rip)
.LBB26_6:                               # %for.inc.1
	movq	$4, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movl	8(%rsi,%rdi), %ecx
	movl	%ecx, 120(%rax)
	movl	%ecx, 116(%rax)
	movl	108(%rax), %edi
	testq	%rdi, %rdi
	je	.LBB26_9
# BB#7:                                 # %land.lhs.true.2
	movl	104(%rax), %r8d
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	%r8, %rcx
	je	.LBB26_9
# BB#8:                                 # %if.then.2
	movl	num_not_at_initial_offset(%rip), %ecx
	incq	%rcx
	movl	%ecx, num_not_at_initial_offset(%rip)
.LBB26_9:                               # %for.inc.2
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	12(%rsi,%rdx), %ecx
	movl	%ecx, 168(%rax)
	movl	%ecx, 164(%rax)
	movl	156(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB26_12
# BB#10:                                # %land.lhs.true.3
	movl	152(%rax), %eax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	je	.LBB26_12
# BB#11:                                # %if.then.3
	movl	num_not_at_initial_offset(%rip), %eax
	incq	%rax
	movl	%eax, num_not_at_initial_offset(%rip)
.LBB26_12:                              # %for.inc.3
	retq
.Ltmp373:
	.size	set_offsets_for_label, .Ltmp373-set_offsets_for_label
	.cfi_endproc

	.align	16, 0x90
	.type	eliminate_regs_in_insn,@function
eliminate_regs_in_insn:                 # @eliminate_regs_in_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp380:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp381:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp382:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp383:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp384:
	.cfi_def_cfa_offset 48
	subq	$560, %rsp              # imm = 0x230
.Ltmp385:
	.cfi_def_cfa_offset 608
.Ltmp386:
	.cfi_offset %rbx, -48
.Ltmp387:
	.cfi_offset %r12, -40
.Ltmp388:
	.cfi_offset %r13, -32
.Ltmp389:
	.cfi_offset %r14, -24
.Ltmp390:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movslq	40(%rbx), %r15
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	testq	%r15, %r15
	jns	.LBB27_2
# BB#1:                                 # %cond.false
	movq	%rbx, %rdi
	callq	recog_memoized_1
	movq	%rax, %r15
.LBB27_2:                               # %cond.end
	movq	32(%rbx), %r14
	movq	%r14, %rdi
	callq	asm_noperands
	movzbl	(%rbx), %ecx
	movzbl	rtx_class(%rcx), %ecx
	movq	%rax, 64(%rsp)          # 8-byte Spill
	xorq	%rax, %rax
	cmpq	$105, %rcx
	jne	.LBB27_5
# BB#3:                                 # %cond.true12
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB27_5
# BB#4:                                 # %cond.false24
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB27_5:                               # %cond.end32
	movq	64(%rsp), %rdx          # 8-byte Reload
	andq	%r15, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB27_6
# BB#9:                                 # %if.end75
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB27_67
# BB#10:                                # %land.lhs.true78
	movq	8(%rax), %rdx
	movzbl	(%rdx), %edi
	cmpq	$61, %rdi
	jne	.LBB27_67
# BB#11:                                # %land.lhs.true86
	movl	8(%rdx), %ecx
	cmpq	$52, %rcx
	ja	.LBB27_46
# BB#12:                                # %for.body.lr.ph
	movq	%r14, %r12
	movq	reg_eliminate(%rip), %r15
	leaq	192(%r15), %rcx
	.align	16, 0x90
.LBB27_13:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r15), %rsi
	cmpq	%rdx, %rsi
	jne	.LBB27_44
# BB#14:                                # %land.lhs.true103
                                        #   in Loop: Header=BB27_13 Depth=1
	movl	12(%r15), %esi
	testq	%rsi, %rsi
	jne	.LBB27_15
.LBB27_44:                              # %for.inc
                                        #   in Loop: Header=BB27_13 Depth=1
	addq	$48, %r15
	cmpq	%rcx, %r15
	jb	.LBB27_13
# BB#45:                                # %land.lhs.true273
	cmpq	$61, %rdi
	movq	%r12, %r14
	jne	.LBB27_67
.LBB27_46:                              # %land.lhs.true281
	movq	16(%rax), %rcx
	movzbl	(%rcx), %esi
	cmpq	$75, %rsi
	jne	.LBB27_67
# BB#47:                                # %land.lhs.true289
	movq	8(%rcx), %r8
	movzbl	(%r8), %esi
	cmpq	$61, %rsi
	jne	.LBB27_67
# BB#48:                                # %land.lhs.true300
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %esi
	cmpq	$54, %rsi
	jne	.LBB27_67
# BB#49:                                # %land.lhs.true311
	movl	8(%r8), %esi
	cmpq	$52, %rsi
	ja	.LBB27_67
# BB#50:                                # %for.body345.lr.ph
	movq	reg_eliminate(%rip), %rbx
	movq	8(%rcx), %rcx
	leaq	192(%rbx), %rsi
.LBB27_51:                              # %for.body345
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rdi
	cmpq	%r8, %rdi
	jne	.LBB27_66
# BB#52:                                # %land.lhs.true349
                                        #   in Loop: Header=BB27_51 Depth=1
	movl	12(%rbx), %edi
	testq	%rdi, %rdi
	jne	.LBB27_53
.LBB27_66:                              # %for.inc466
                                        #   in Loop: Header=BB27_51 Depth=1
	addq	$48, %rbx
	cmpq	%rsi, %rbx
	jb	.LBB27_51
.LBB27_67:                              # %if.end469
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%r14, 24(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	%r14, %rdi
	callq	elimination_effects
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	extract_insn
	movzbl	recog_data+996(%rip), %eax
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movabsq	$0, %rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	jle	.LBB27_88
# BB#68:                                # %for.body474.lr.ph
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	40(%rsp), %r15          # 8-byte Reload
	andq	%rax, %r15
	je	.LBB27_70
# BB#69:                                # %for.body474.lr.ph
	movq	32(%rsp), %r15          # 8-byte Reload
.LBB27_70:                              # %for.body474.lr.ph
	movq	$32, %rcx
	movq	8(%rsp), %rax           # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rax,%rax,4), %rax
	leaq	insn_data+24(,%rax,8), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	xorq	%r14, %r14
	xorq	%r13, %r13
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB27_71:                              # %for.body474
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB27_76 Depth 2
	movq	$32, %rcx
	movq	64(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	recog_data(,%r13,8), %rbx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rbx, 80(%rsp,%r13,8)
	movq	%rbx, 320(%rsp,%r13,8)
	jns	.LBB27_73
# BB#72:                                # %lor.lhs.false484
                                        #   in Loop: Header=BB27_71 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	leaq	(%r13,%r13,2), %rcx
	movzbl	19(%rax,%rcx,8), %eax
	testq	%rax, %rax
	je	.LBB27_87
.LBB27_73:                              # %if.then491
                                        #   in Loop: Header=BB27_71 Depth=1
	movl	recog_data+840(,%r13,4), %eax
	testq	%rax, %rax
	je	.LBB27_80
# BB#74:                                # %land.lhs.true496
                                        #   in Loop: Header=BB27_71 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	jne	.LBB27_80
# BB#75:                                # %for.body508.lr.ph
                                        #   in Loop: Header=BB27_71 Depth=1
	movq	reg_eliminate(%rip), %rax
	leaq	192(%rax), %rcx
	.align	16, 0x90
.LBB27_76:                              # %for.body508
                                        #   Parent Loop BB27_71 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	32(%rax), %rdx
	cmpq	%rbx, %rdx
	jne	.LBB27_79
# BB#77:                                # %land.lhs.true514
                                        #   in Loop: Header=BB27_76 Depth=2
	movl	12(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB27_79
# BB#78:                                # %if.then517
                                        #   in Loop: Header=BB27_76 Depth=2
	movl	%r14d, 12(%rax)
.LBB27_79:                              # %for.inc520
                                        #   in Loop: Header=BB27_76 Depth=2
	addq	$48, %rax
	cmpq	%rcx, %rax
	jb	.LBB27_76
.LBB27_80:                              # %if.end523
                                        #   in Loop: Header=BB27_71 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	eliminate_regs
	cmpq	%rbx, %rax
	movq	%rax, 320(%rsp,%r13,8)
	je	.LBB27_82
# BB#81:                                # %if.end523
                                        #   in Loop: Header=BB27_71 Depth=1
	movq	$1, 48(%rsp)            # 8-byte Folded Spill
.LBB27_82:                              # %if.end523
                                        #   in Loop: Header=BB27_71 Depth=1
	movq	recog_data+240(,%r13,8), %rcx
	movq	$0, (%rcx)
	movl	recog_data+840(,%r13,4), %ecx
	testq	%rcx, %rcx
	je	.LBB27_87
# BB#83:                                # %land.lhs.true548
                                        #   in Loop: Header=BB27_71 Depth=1
	movzbl	(%rbx), %ecx
	cmpq	$61, %rcx
	jne	.LBB27_87
# BB#84:                                # %land.lhs.true555
                                        #   in Loop: Header=BB27_71 Depth=1
	movzbl	(%rax), %eax
	cmpq	$66, %rax
	jne	.LBB27_87
# BB#85:                                # %land.lhs.true555
                                        #   in Loop: Header=BB27_71 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB27_87
# BB#86:                                # %if.then564
                                        #   in Loop: Header=BB27_71 Depth=1
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	emit_insn_after
	.align	16, 0x90
.LBB27_87:                              # %for.inc571
                                        #   in Loop: Header=BB27_71 Depth=1
	movzbl	recog_data+996(%rip), %eax
	incq	%r13
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB27_71
.LBB27_88:                              # %for.cond573.preheader
	andq	$65280, %rax            # imm = 0xFF00
	movq	$8, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movq	$48, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB27_90
	.align	16, 0x90
.LBB27_89:                              # %for.body577
                                        # =>This Inner Loop Header: Depth=1
	movsbq	recog_data+992(%r12), %rax
	movq	recog_data+240(,%rax,8), %rax
	movq	(%rax), %rax
	movq	recog_data+960(,%r12,8), %rcx
	movq	%rax, (%rcx)
	incq	%r12
	movq	$56, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rax, %rsi
	jl	.LBB27_89
.LBB27_90:                              # %for.end587
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	check_eliminable_occurrences
	movzbl	recog_data+996(%rip), %eax
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movq	32(%rsp), %r12          # 8-byte Reload
	movq	48(%rsp), %r15          # 8-byte Reload
	movq	16(%rsp), %r13          # 8-byte Reload
	jle	.LBB27_93
# BB#91:
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB27_92:                              # %for.body592
                                        # =>This Inner Loop Header: Depth=1
	movq	320(%rsp,%rdx,8), %rax
	movq	recog_data+240(,%rdx,8), %rcx
	movq	%rax, (%rcx)
	movzbl	recog_data+996(%rip), %eax
	incq	%rdx
	movq	$56, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rsi, %rdi
	jl	.LBB27_92
.LBB27_93:                              # %for.cond600.preheader
	andq	$65280, %rax            # imm = 0xFF00
	movq	$8, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movq	$48, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB27_96
# BB#94:
	xorq	%rax, %rax
	.align	16, 0x90
.LBB27_95:                              # %for.body604
                                        # =>This Inner Loop Header: Depth=1
	movsbq	recog_data+992(%rax), %rcx
	movq	320(%rsp,%rcx,8), %rcx
	movq	recog_data+960(,%rax,8), %rsi
	movq	%rcx, (%rsi)
	incq	%rax
	movq	$56, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rsi, %rdi
	jl	.LBB27_95
.LBB27_96:                              # %for.end614
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %r15
	je	.LBB27_115
# BB#97:                                # %if.then616
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	jne	.LBB27_100
# BB#98:                                # %if.then618
	movq	%rbx, %rdi
	callq	copy_insn
	movq	56(%r12), %rdi
	movq	%rax, %rbx
	testq	%rdi, %rdi
	je	.LBB27_100
# BB#99:                                # %if.then624
	callq	copy_insn_1
	movq	%rax, 56(%r12)
.LBB27_100:                             # %if.end633
	movq	$32, %rcx
	movq	64(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rbx, 32(%r12)
	jns	.LBB27_115
# BB#101:                               # %if.end633
	testq	%r13, %r13
	je	.LBB27_115
# BB#102:                               # %land.lhs.true641
	movq	16(%r13), %rcx
	movzbl	(%rcx), %eax
	cmpq	$61, %rax
	jne	.LBB27_105
# BB#103:                               # %land.lhs.true649
	movzbl	(%rbx), %edx
	cmpq	$47, %rdx
	jne	.LBB27_113
# BB#104:                               # %lor.lhs.false654
	movq	16(%rbx), %rdx
	movzbl	(%rdx), %edx
	cmpq	$61, %rdx
	jne	.LBB27_113
.LBB27_105:                             # %land.lhs.true665
	cmpq	$66, %rax
	jne	.LBB27_107
# BB#106:                               # %land.lhs.true665
	movq	recog_data+8(%rip), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB27_113
.LBB27_107:                             # %lor.lhs.false679
	movq	8(%r13), %rsi
	movzbl	(%rsi), %ecx
	movq	recog_data(%rip), %rdi
	cmpq	$66, %rcx
	movabsq	$0, %rcx
	movabsq	$1, %r8
	movq	%rcx, %rdx
	jne	.LBB27_109
# BB#108:                               # %lor.lhs.false679
	movq	%r8, %rdx
.LBB27_109:                             # %lor.lhs.false679
	cmpq	%rdi, %rsi
	je	.LBB27_111
# BB#110:                               # %lor.lhs.false679
	movq	%r8, %rcx
.LBB27_111:                             # %lor.lhs.false679
	testq	%rdx, %rcx
	jne	.LBB27_113
# BB#112:                               # %lor.lhs.false679
	cmpq	$75, %rax
	jne	.LBB27_115
.LBB27_113:                             # %if.then701
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	recog
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB27_115
# BB#114:                               # %if.then708
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	%eax, 40(%r12)
.LBB27_115:                             # %if.end714
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	jne	.LBB27_122
# BB#116:                               # %for.cond717.preheader
	movzbl	recog_data+996(%rip), %eax
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB27_119
# BB#117:
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB27_118:                             # %for.body721
                                        # =>This Inner Loop Header: Depth=1
	movq	80(%rsp,%rdx,8), %rax
	movq	recog_data+240(,%rdx,8), %rcx
	movq	%rax, (%rcx)
	movzbl	recog_data+996(%rip), %eax
	incq	%rdx
	movq	$56, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rsi, %rdi
	jl	.LBB27_118
.LBB27_119:                             # %for.cond729.preheader
	andq	$65280, %rax            # imm = 0xFF00
	movq	$8, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movq	$48, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB27_122
# BB#120:
	xorq	%rax, %rax
	.align	16, 0x90
.LBB27_121:                             # %for.body733
                                        # =>This Inner Loop Header: Depth=1
	movsbq	recog_data+992(%rax), %rcx
	movq	80(%rsp,%rcx,8), %rcx
	movq	recog_data+960(,%rax,8), %rsi
	movq	%rcx, (%rsi)
	incq	%rax
	movq	$56, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rsi, %rdi
	jl	.LBB27_121
.LBB27_122:                             # %for.body749.lr.ph
	movq	reg_eliminate(%rip), %rax
	xorq	%rcx, %rcx
	leaq	192(%rax), %rdx
	.align	16, 0x90
.LBB27_123:                             # %for.body749
                                        # =>This Inner Loop Header: Depth=1
	movl	24(%rax), %esi
	movl	20(%rax), %edi
	cmpq	%rdi, %rsi
	je	.LBB27_126
# BB#124:                               # %land.lhs.true753
                                        #   in Loop: Header=BB27_123 Depth=1
	movl	28(%rax), %ebx
	testq	%rbx, %rbx
	je	.LBB27_126
# BB#125:                               # %if.then755
                                        #   in Loop: Header=BB27_123 Depth=1
	movl	%ecx, 12(%rax)
.LBB27_126:                             # %if.end757
                                        #   in Loop: Header=BB27_123 Depth=1
	cmpq	%rdi, %rsi
	movl	%ecx, 28(%rax)
	je	.LBB27_128
# BB#127:                               # %if.end757
                                        #   in Loop: Header=BB27_123 Depth=1
	movq	$1, %r15
.LBB27_128:                             # %if.end757
                                        #   in Loop: Header=BB27_123 Depth=1
	addq	$48, %rax
	cmpq	%rdx, %rax
	jb	.LBB27_123
# BB#129:                               # %done
	xorq	%rax, %rax
	testq	%r14, %r15
	jne	.LBB27_130
	jmp	.LBB27_133
.LBB27_6:                               # %if.then
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$49, %rcx
	ja	.LBB27_8
# BB#7:                                 # %if.then
	xorq	%rax, %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$898300999892992, %rcx  # imm = 0x3310000000000
	testq	%rcx, %rdx
	jne	.LBB27_133
.LBB27_8:                               # %if.end
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.eliminate_regs_in_insn, %rdx
	movq	$2939, %rsi             # imm = 0xB7B
	callq	fancy_abort
.LBB27_15:                              # %if.then105
	movl	(%r15), %ecx
	cmpq	$20, %rcx
	jne	.LBB27_42
# BB#16:                                # %land.lhs.true108
	movl	4(%r15), %ecx
	cmpq	$6, %rcx
	jne	.LBB27_42
# BB#17:                                # %if.then111
	movq	16(%rax), %r14
	movq	40(%r15), %rax
	xorq	%rbx, %rbx
	cmpq	%rax, %r14
	je	.LBB27_32
# BB#18:
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	32(%rsp), %rdi          # 8-byte Reload
.LBB27_20:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %ecx
	cmpq	$75, %rcx
	jne	.LBB27_24
# BB#21:                                # %land.lhs.true121
                                        #   in Loop: Header=BB27_20 Depth=1
	movq	16(%r14), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB27_24
# BB#22:                                # %if.then129
                                        #   in Loop: Header=BB27_20 Depth=1
	movq	8(%rcx), %rcx
	movq	8(%r14), %r14
	andq	%rsi, %rbx
	addq	%rcx, %rbx
	cmpq	%rax, %r14
	jne	.LBB27_20
	jmp	.LBB27_23
.LBB27_24:                              # %if.else
                                        #   in Loop: Header=BB27_20 Depth=1
	callq	prev_nonnote_insn
	testq	%rax, %rax
	je	.LBB27_31
# BB#25:                                # %land.lhs.true143
                                        #   in Loop: Header=BB27_20 Depth=1
	movzbl	(%rax), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB27_31
# BB#26:                                # %cond.true151
                                        #   in Loop: Header=BB27_20 Depth=1
	movq	32(%rax), %r13
	movzbl	(%r13), %ecx
	cmpq	$47, %rcx
	jne	.LBB27_28
# BB#27:                                #   in Loop: Header=BB27_20 Depth=1
	movq	%rax, 64(%rsp)          # 8-byte Spill
	jmp	.LBB27_29
.LBB27_28:                              # %cond.false163
                                        #   in Loop: Header=BB27_20 Depth=1
	movq	%rax, %rdi
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	movq	%r13, %rsi
	callq	single_set_2
	movq	%rax, %r13
.LBB27_29:                              # %cond.end171
                                        #   in Loop: Header=BB27_20 Depth=1
	testq	%r13, %r13
	je	.LBB27_31
# BB#30:                                # %land.lhs.true175
                                        #   in Loop: Header=BB27_20 Depth=1
	movq	8(%r13), %rdi
	movq	%r14, %rsi
	callq	rtx_equal_p
	movq	64(%rsp), %rdi          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB27_31
# BB#19:                                # %while.cond.outer
                                        #   in Loop: Header=BB27_20 Depth=1
	movq	16(%r13), %r14
	movq	40(%r15), %rax
	cmpq	%rax, %r14
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	jne	.LBB27_20
	jmp	.LBB27_32
.LBB27_23:
	movq	%rax, %r14
.LBB27_32:                              # %if.then191
	movl	20(%r15), %eax
	subq	%rax, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	plus_constant_wide
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%rcx, %rax
	jne	.LBB27_35
# BB#33:                                # %if.then197
	movq	%r12, %rdi
	callq	copy_insn
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	56(%rbx), %rdi
	movq	%rax, %r12
	testq	%rdi, %rdi
	je	.LBB27_35
# BB#34:                                # %if.then203
	callq	copy_insn_1
	movq	%rax, 56(%rbx)
.LBB27_35:                              # %if.end212
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	%r12, %rsi
	movq	%rsi, 32(%rbx)
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	xorq	%r13, %r13
	cmpq	$105, %rax
	jne	.LBB27_39
# BB#36:                                # %cond.true223
	movzbl	(%rsi), %eax
	cmpq	$47, %rax
	jne	.LBB27_38
# BB#37:
	movq	%rsi, %r13
	jmp	.LBB27_39
.LBB27_53:                              # %if.then352
	movl	20(%rbx), %r15d
	addq	%rcx, %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	je	.LBB27_54
# BB#58:                                # %if.else399
	movq	40(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	movq	32(%rsp), %r12          # 8-byte Reload
	jne	.LBB27_61
# BB#59:                                # %if.then401
	movq	%r14, %rdi
	callq	copy_insn
	movq	56(%r12), %rdi
	movq	%rax, %r14
	testq	%rdi, %rdi
	je	.LBB27_61
# BB#60:                                # %if.then407
	callq	copy_insn_1
	movq	%rax, 56(%r12)
.LBB27_61:                              # %if.end416
	movq	%r14, %rsi
	movq	%rsi, 32(%r12)
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	xorq	%r14, %r14
	cmpq	$105, %rax
	jne	.LBB27_65
# BB#62:                                # %cond.true427
	movzbl	(%rsi), %eax
	cmpq	$47, %rax
	jne	.LBB27_64
# BB#63:
	movq	%rsi, %r14
	jmp	.LBB27_65
.LBB27_38:                              # %cond.false235
	movq	%rbx, %rdi
	callq	single_set_2
	movq	%rax, %r13
.LBB27_39:                              # %cond.end243
	leaq	8(%r13), %r14
	addq	$16, %r13
	movq	$1, %rcx
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rdx
	callq	validate_change
	movq	40(%r15), %rdx
	movq	$1, %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	validate_change
	callq	apply_change_group
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB27_40
# BB#41:                                # %if.then256
	movq	%r12, (%r13)
	movq	40(%r15), %rax
	movq	%rax, (%r14)
.LBB27_40:
	movq	%rbx, %r12
	movq	$1, %r15
	jmp	.LBB27_130
.LBB27_54:                              # %if.then357
	movq	40(%rbx), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	callq	gen_rtx_fmt_ee
	movq	32(%rsp), %r12          # 8-byte Reload
	movq	%rax, 32(%r12)
	movl	%ebx, 76(%rsp)
	leaq	76(%rsp), %rdx
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	recog
	movl	%eax, 40(%r12)
	movl	76(%rsp), %edi
	testq	%rdi, %rdi
	je	.LBB27_56
# BB#55:                                # %if.then374
	incq	%rdi
	callq	rtvec_alloc
	movq	32(%r12), %rcx
	movq	%rcx, 8(%rax)
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, 32(%r12)
	movl	40(%r12), %esi
	movq	%rax, %rdi
	callq	add_clobbers
	movl	40(%r12), %eax
.LBB27_56:                              # %if.end391
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %r15
	testq	%rax, %rax
	jns	.LBB27_130
# BB#57:                                # %if.then397
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.eliminate_regs_in_insn, %rcx
	movq	$3071, %rsi             # imm = 0xBFF
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB27_31:                              # %while.end
	movq	40(%r15), %rax
	cmpq	%rax, %r14
	movq	%rax, %r14
	je	.LBB27_32
.LBB27_42:                              # %if.end266
	movq	$1, %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	40(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	movq	32(%rsp), %r12          # 8-byte Reload
	je	.LBB27_130
# BB#43:                                # %if.then268
	movq	%r12, %rdi
	callq	delete_dead_insn
	movq	$1, %rax
	jmp	.LBB27_133
.LBB27_64:                              # %cond.false439
	movq	%r12, %rdi
	callq	single_set_2
	movq	%rax, %r14
.LBB27_65:                              # %cond.end447
	movq	40(%rbx), %rax
	movq	16(%r14), %rcx
	movq	%rax, 8(%rcx)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_CONST_INT
	movq	16(%r14), %rcx
	movq	%rax, 16(%rcx)
	movq	$1, %r15
.LBB27_130:                             # %land.lhs.true769
	movq	56(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB27_132
# BB#131:                               # %if.then775
	xorq	%rsi, %rsi
	movq	%rdi, %rdx
	callq	eliminate_regs
	movq	%rax, 56(%r12)
.LBB27_132:                             # %cleanup
	movq	%r15, %rax
.LBB27_133:                             # %cleanup
	addq	$560, %rsp              # imm = 0x230
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp391:
	.size	eliminate_regs_in_insn, .Ltmp391-eliminate_regs_in_insn
	.cfi_endproc

	.align	16, 0x90
	.type	forget_old_reloads_1,@function
forget_old_reloads_1:                   # @forget_old_reloads_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp395:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp396:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp397:
	.cfi_def_cfa_offset 32
.Ltmp398:
	.cfi_offset %rbx, -24
.Ltmp399:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	xorq	%r14, %r14
	jmp	.LBB28_1
	.align	16, 0x90
.LBB28_3:                               # %while.body
                                        #   in Loop: Header=BB28_1 Depth=1
	movq	8(%rbx), %rax
	movl	16(%rbx), %edx
	movzbl	2(%rax), %esi
	movl	8(%rax), %edi
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	movq	%r8, %rcx
	callq	subreg_regno_offset
	movq	8(%rbx), %rbx
	addq	%rax, %r14
.LBB28_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rbx), %r8d
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	je	.LBB28_11
# BB#2:                                 # %while.cond
                                        #   in Loop: Header=BB28_1 Depth=1
	cmpq	$63, %rax
	je	.LBB28_3
	jmp	.LBB28_10
.LBB28_11:                              # %if.end
	movl	8(%rbx), %r10d
	addq	%r14, %r10
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	movq	%r10, %rax
	andq	%r11, %rax
	cmpq	$52, %rax
	movq	$1, %rax
	ja	.LBB28_5
# BB#12:                                # %if.else
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%r10, %rax
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	cmpq	$8, %rax
	je	.LBB28_16
# BB#13:                                # %if.else
	leaq	-21(%r10), %rax
	andq	%r11, %rax
	cmpq	$8, %rax
	jb	.LBB28_16
# BB#14:                                # %if.else
	leaq	-45(%r10), %rax
	andq	%r11, %rax
	cmpq	$8, %rax
	jb	.LBB28_16
# BB#15:                                # %if.else
	leaq	-29(%r10), %rax
	andq	%r11, %rax
	cmpq	$7, %rax
	jbe	.LBB28_16
# BB#21:                                # %cond.false
	cmpq	$24, %r8
	jne	.LBB28_22
# BB#24:                                # %cond.true61
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB28_26
.LBB28_16:                              # %cond.true
	movl	mode_class(,%r8,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB28_26
# BB#17:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB28_18
# BB#19:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB28_26
.LBB28_22:                              # %cond.false
	cmpq	$18, %r8
	jne	.LBB28_25
# BB#23:                                # %cond.true54
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB28_26
.LBB28_18:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB28_26
.LBB28_25:                              # %cond.end81
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%r8), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	testq	%r11, %rax
	je	.LBB28_10
.LBB28_26:                              # %for.body.lr.ph
	movl	n_reloads(%rip), %r8d
	movq	reg_is_output_reload(%rip), %r9
	movq	$1, %rdx
	movq	%rax, %rbx
	andq	%r11, %rbx
	.align	16, 0x90
.LBB28_27:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	leaq	-1(%rdx,%r10), %rcx
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%r11, %r8
	je	.LBB28_29
# BB#28:                                # %for.body
                                        #   in Loop: Header=BB28_27 Depth=1
	movq	%r9, %rdi
	andq	%rsi, %rdi
	jne	.LBB28_30
.LBB28_29:                              # %if.then91
                                        #   in Loop: Header=BB28_27 Depth=1
	notq	%rsi
	andq	%rsi, reg_reloaded_valid(%rip)
	andq	%r11, %rcx
	movq	$0, spill_reg_store(,%rcx,8)
.LBB28_30:                              # %for.inc
                                        #   in Loop: Header=BB28_27 Depth=1
	movq	%rdx, %rcx
	andq	%r11, %rcx
	incq	%rdx
	cmpq	%rbx, %rcx
	jne	.LBB28_27
# BB#4:                                 # %while.cond101.preheader
	testq	%r11, %rax
	je	.LBB28_10
.LBB28_5:                               # %while.body104.lr.ph
	movl	n_reloads(%rip), %ecx
	.align	16, 0x90
.LBB28_6:                               # %while.body104
                                        # =>This Inner Loop Header: Depth=1
	leaq	-1(%rax,%r10), %rdx
	decq	%rax
	andq	%r11, %rdx
	testq	%r11, %rcx
	je	.LBB28_8
# BB#7:                                 # %lor.lhs.false107
                                        #   in Loop: Header=BB28_6 Depth=1
	movq	reg_has_output_reload(%rip), %rsi
	movzbl	(%rsi,%rdx), %esi
	testq	%rsi, %rsi
	jne	.LBB28_9
.LBB28_8:                               # %if.then114
                                        #   in Loop: Header=BB28_6 Depth=1
	movq	reg_last_reload_reg(%rip), %rsi
	movq	$0, (%rsi,%rdx,8)
.LBB28_9:                               # %while.cond101.backedge
                                        #   in Loop: Header=BB28_6 Depth=1
	testq	%r11, %rax
	jne	.LBB28_6
.LBB28_10:                              # %while.end119
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp400:
	.size	forget_old_reloads_1, .Ltmp400-forget_old_reloads_1
	.cfi_endproc

	.align	16, 0x90
	.type	reload_reg_reaches_end_p,@function
reload_reg_reaches_end_p:               # @reload_reg_reaches_end_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp402:
	.cfi_def_cfa_offset 16
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$10, %rcx
	ja	.LBB29_66
# BB#1:                                 # %entry
	movq	$1, %rax
	jmpq	*.LJTI29_0(,%rcx,8)
.LBB29_11:                              # %for.cond49.preheader
	movslq	reload_n_operands(%rip), %r8
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %rdx
	movq	%rdi, %rcx
	shlq	%cl, %rdx
	cmpq	%rax, %r8
	jle	.LBB29_15
# BB#12:                                # %for.body51.preheader
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rdi
	.align	16, 0x90
.LBB29_13:                              # %for.body51
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_reg_used_in_input-8(,%rdi,8), %rcx
	xorq	%rax, %rax
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#14:                                # %for.cond49
                                        #   in Loop: Header=BB29_13 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rdi
	cmpq	%r8, %rax
	jl	.LBB29_13
.LBB29_15:                              # %for.cond63.preheader
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rsi
	.align	16, 0x90
.LBB29_16:                              # %for.cond63
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r8, %rax
	jge	.LBB29_17
# BB#32:                                # %for.body65
                                        #   in Loop: Header=BB29_16 Depth=1
	movq	reload_reg_used_in_input_addr(,%rsi,8), %rcx
	xorq	%rax, %rax
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#33:                                # %lor.lhs.false72
                                        #   in Loop: Header=BB29_16 Depth=1
	movq	reload_reg_used_in_inpaddr_addr(,%rsi,8), %rcx
	incq	%rsi
	testq	%rdx, %rcx
	je	.LBB29_16
	jmp	.LBB29_65
.LBB29_44:                              # %for.cond137.preheader
	movl	reload_n_operands(%rip), %r8d
	movq	$1, %r9
	movq	%rdi, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rsi
	.align	16, 0x90
.LBB29_45:                              # %for.cond137
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB29_8
# BB#46:                                # %for.body139
                                        #   in Loop: Header=BB29_45 Depth=1
	movq	reload_reg_used_in_input_addr(,%rsi,8), %rcx
	xorq	%rax, %rax
	testq	%r9, %rcx
	jne	.LBB29_65
# BB#47:                                # %lor.lhs.false146
                                        #   in Loop: Header=BB29_45 Depth=1
	movq	reload_reg_used_in_inpaddr_addr(,%rsi,8), %rcx
	testq	%r9, %rcx
	jne	.LBB29_65
# BB#48:                                # %lor.lhs.false153
                                        #   in Loop: Header=BB29_45 Depth=1
	movq	reload_reg_used_in_input(,%rsi,8), %rcx
	incq	%rsi
	testq	%r9, %rcx
	je	.LBB29_45
	jmp	.LBB29_65
.LBB29_59:                              # %sw.bb246
	movl	reload_n_operands(%rip), %esi
.LBB29_60:                              # %sw.bb247
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB29_65
# BB#61:                                # %for.body250.lr.ph
	movq	$1, %rdx
	movq	$1, %r8
	movq	%rdi, %rcx
	shlq	%cl, %r8
	.align	16, 0x90
.LBB29_63:                              # %for.body250
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_reg_used_in_output_addr-8(,%rdx,8), %rcx
	xorq	%rax, %rax
	testq	%r8, %rcx
	jne	.LBB29_65
# BB#64:                                # %lor.lhs.false257
                                        #   in Loop: Header=BB29_63 Depth=1
	movq	reload_reg_used_in_outaddr_addr-8(,%rdx,8), %rcx
	testq	%r8, %rcx
	jne	.LBB29_65
# BB#62:                                # %for.cond248
                                        #   in Loop: Header=BB29_63 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rdx
	cmpq	%rdi, %rax
	movq	$1, %rax
	jl	.LBB29_63
	jmp	.LBB29_65
.LBB29_7:                               # %entry.for.cond166.preheader_crit_edge
	movl	reload_n_operands(%rip), %r8d
.LBB29_8:                               # %for.cond166.preheader
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %rsi
	movq	$1, %rdx
	movq	%rdi, %rcx
	shlq	%cl, %rdx
	testq	%rax, %rax
	jle	.LBB29_9
	.align	16, 0x90
.LBB29_50:                              # %for.body168
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_reg_used_in_output_addr-8(,%rsi,8), %rcx
	xorq	%rax, %rax
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#51:                                # %lor.lhs.false175
                                        #   in Loop: Header=BB29_50 Depth=1
	movq	reload_reg_used_in_outaddr_addr-8(,%rsi,8), %rcx
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#52:                                # %lor.lhs.false182
                                        #   in Loop: Header=BB29_50 Depth=1
	movq	reload_reg_used_in_output-8(,%rsi,8), %rcx
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#49:                                # %for.cond166
                                        #   in Loop: Header=BB29_50 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r8, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rsi
	cmpq	%rdi, %rax
	jl	.LBB29_50
.LBB29_9:                               # %for.end193
	movq	reload_reg_used(%rip), %rax
	testq	%rdx, %rax
	jne	.LBB29_10
# BB#53:                                # %for.end193
	movabsq	$1, %rax
	popq	%rdx
	retq
.LBB29_2:                               # %for.cond200.preheader
	movslq	reload_n_operands(%rip), %r8
	movq	$1, %rsi
	movq	$1, %rdx
	movq	%rdi, %rcx
	shlq	%cl, %rdx
	testq	%r8, %r8
	jle	.LBB29_3
	.align	16, 0x90
.LBB29_55:                              # %for.body202
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_reg_used_in_output_addr-8(,%rsi,8), %rcx
	xorq	%rax, %rax
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#56:                                # %lor.lhs.false209
                                        #   in Loop: Header=BB29_55 Depth=1
	movq	reload_reg_used_in_outaddr_addr-8(,%rsi,8), %rcx
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#57:                                # %lor.lhs.false216
                                        #   in Loop: Header=BB29_55 Depth=1
	movq	reload_reg_used_in_output-8(,%rsi,8), %rcx
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#54:                                # %for.cond200
                                        #   in Loop: Header=BB29_55 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rsi
	cmpq	%r8, %rax
	jl	.LBB29_55
.LBB29_3:                               # %for.end227
	movq	reload_reg_used_in_op_addr(%rip), %rcx
	xorq	%rax, %rax
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#4:                                 # %land.lhs.true232
	movq	reload_reg_used_in_insn(%rip), %rcx
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#5:                                 # %land.rhs237
	movq	reload_reg_used(%rip), %rax
	testq	%rdx, %rax
	jne	.LBB29_6
# BB#58:                                # %land.rhs237
	movabsq	$1, %rax
	popq	%rdx
	retq
.LBB29_19:                              # %for.cond.preheader
	movslq	reload_n_operands(%rip), %r8
	movq	$1, %rsi
	movq	$1, %rdx
	movq	%rdi, %rcx
	shlq	%cl, %rdx
	testq	%r8, %r8
	jle	.LBB29_20
	.align	16, 0x90
.LBB29_25:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_reg_used_in_output_addr-8(,%rsi,8), %rcx
	xorq	%rax, %rax
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#26:                                # %lor.lhs.false
                                        #   in Loop: Header=BB29_25 Depth=1
	movq	reload_reg_used_in_outaddr_addr-8(,%rsi,8), %rcx
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#27:                                # %lor.lhs.false8
                                        #   in Loop: Header=BB29_25 Depth=1
	movq	reload_reg_used_in_output-8(,%rsi,8), %rcx
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#28:                                # %lor.lhs.false15
                                        #   in Loop: Header=BB29_25 Depth=1
	movq	reload_reg_used_in_input_addr-8(,%rsi,8), %rcx
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#29:                                # %lor.lhs.false22
                                        #   in Loop: Header=BB29_25 Depth=1
	movq	reload_reg_used_in_inpaddr_addr-8(,%rsi,8), %rcx
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#30:                                # %lor.lhs.false29
                                        #   in Loop: Header=BB29_25 Depth=1
	movq	reload_reg_used_in_input-8(,%rsi,8), %rcx
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#24:                                # %for.cond
                                        #   in Loop: Header=BB29_25 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rsi
	cmpq	%r8, %rax
	jl	.LBB29_25
.LBB29_20:                              # %for.end
	movq	reload_reg_used_in_op_addr(%rip), %rcx
	xorq	%rax, %rax
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#21:                                # %land.lhs.true
	movq	reload_reg_used_in_insn(%rip), %rcx
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#22:                                # %land.rhs
	movq	reload_reg_used(%rip), %rax
	testq	%rdx, %rax
	jne	.LBB29_23
# BB#31:                                # %land.rhs
	movabsq	$1, %rax
	popq	%rdx
	retq
.LBB29_66:                              # %sw.epilog
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.reload_reg_reaches_end_p, %rdx
	movq	$4663, %rsi             # imm = 0x1237
	callq	fancy_abort
.LBB29_17:                              # %for.cond84.preheader
	testq	%r8, %r8
	jle	.LBB29_38
# BB#18:
	movq	$1, %rsi
	.align	16, 0x90
.LBB29_35:                              # %for.body86
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_reg_used_in_output_addr-8(,%rsi,8), %rcx
	xorq	%rax, %rax
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#36:                                # %lor.lhs.false93
                                        #   in Loop: Header=BB29_35 Depth=1
	movq	reload_reg_used_in_outaddr_addr-8(,%rsi,8), %rcx
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#37:                                # %lor.lhs.false100
                                        #   in Loop: Header=BB29_35 Depth=1
	movq	reload_reg_used_in_output-8(,%rsi,8), %rcx
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#34:                                # %for.cond84
                                        #   in Loop: Header=BB29_35 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rsi
	cmpq	%r8, %rax
	jl	.LBB29_35
.LBB29_38:                              # %for.end111
	movq	reload_reg_used_in_op_addr_reload(%rip), %rcx
	xorq	%rax, %rax
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#39:                                # %if.end117
	movq	reload_reg_used_in_op_addr(%rip), %rcx
	xorq	%rax, %rax
	testq	%rdx, %rcx
	jne	.LBB29_65
# BB#40:                                # %land.lhs.true122
	movq	reload_reg_used_in_insn(%rip), %rcx
	testq	%rdx, %rcx
	je	.LBB29_41
.LBB29_65:                              # %return
	popq	%rdx
	retq
.LBB29_10:
	movabsq	$0, %rax
	popq	%rdx
	retq
.LBB29_6:
	movabsq	$0, %rax
	popq	%rdx
	retq
.LBB29_23:
	movabsq	$0, %rax
	popq	%rdx
	retq
.LBB29_41:                              # %land.rhs127
	movq	reload_reg_used(%rip), %rax
	testq	%rdx, %rax
	jne	.LBB29_42
# BB#43:                                # %land.rhs127
	movabsq	$1, %rax
	popq	%rdx
	retq
.LBB29_42:
	movabsq	$0, %rax
	popq	%rdx
	retq
.Ltmp403:
	.size	reload_reg_reaches_end_p, .Ltmp403-reload_reg_reaches_end_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI29_0:
	.quad	.LBB29_44
	.quad	.LBB29_60
	.quad	.LBB29_59
	.quad	.LBB29_11
	.quad	.LBB29_11
	.quad	.LBB29_60
	.quad	.LBB29_60
	.quad	.LBB29_7
	.quad	.LBB29_2
	.quad	.LBB29_65
	.quad	.LBB29_19

	.text
	.align	16, 0x90
	.type	delete_output_reload,@function
delete_output_reload:                   # @delete_output_reload
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp410:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp411:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp412:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp413:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp414:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp415:
	.cfi_def_cfa_offset 112
.Ltmp416:
	.cfi_offset %rbx, -48
.Ltmp417:
	.cfi_offset %r12, -40
.Ltmp418:
	.cfi_offset %r13, -32
.Ltmp419:
	.cfi_offset %r14, -24
.Ltmp420:
	.cfi_offset %r15, -16
	movq	%rdx, %r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	spill_reg_stored_to(,%rax,8), %r12
	movzbl	(%r12), %ecx
	movq	spill_reg_store(,%rax,8), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	leaq	8(%r12), %rdx
	cmpq	$63, %rcx
	jne	.LBB30_2
	.align	16, 0x90
.LBB30_1:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdx), %r12
	movzbl	(%r12), %eax
	leaq	8(%r12), %rdx
	cmpq	$63, %rax
	je	.LBB30_1
.LBB30_2:                               # %while.end
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movl	(%rdx), %eax
	movq	reg_equiv_memory_loc(%rip), %rcx
	movslq	n_reloads(%rip), %r14
	movq	(%rcx,%rax,8), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	xorq	%rax, %rax
	testq	%r14, %r14
	jle	.LBB30_3
.LBB30_4:                               # %for.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB30_5 Depth 2
                                        #       Child Loop BB30_11 Depth 3
                                        #     Child Loop BB30_30 Depth 2
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	%r14, %rbx
	.align	16, 0x90
.LBB30_5:                               # %for.body
                                        #   Parent Loop BB30_4 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB30_11 Depth 3
	leaq	-1(%rbx), %r14
	imulq	$104, %r14, %r15
	movq	rld(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB30_13
# BB#6:                                 # %if.end
                                        #   in Loop: Header=BB30_5 Depth=2
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	je	.LBB30_8
# BB#7:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB30_5 Depth=2
	cmpq	$0, reload_override_in-8(,%rbx,8)
	je	.LBB30_9
.LBB30_8:                               # %if.then17
                                        #   in Loop: Header=BB30_5 Depth=2
	movq	rld+40(%r15), %rdi
	movl	(%rdi), %eax
.LBB30_9:                               # %while.cond21.preheader
                                        #   in Loop: Header=BB30_5 Depth=2
	andq	$65535, %rax            # imm = 0xFFFF
	jmp	.LBB30_11
	.align	16, 0x90
.LBB30_10:                              # %while.body25
                                        #   in Loop: Header=BB30_11 Depth=3
	movq	8(%rdi), %rdi
	movzbl	(%rdi), %eax
.LBB30_11:                              # %while.body25
                                        #   Parent Loop BB30_4 Depth=1
                                        #     Parent Loop BB30_5 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	cmpq	$63, %rax
	je	.LBB30_10
# BB#12:                                # %while.end29
                                        #   in Loop: Header=BB30_5 Depth=2
	movq	%r12, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB30_15
.LBB30_13:                              # %for.cond.backedge
                                        #   in Loop: Header=BB30_5 Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r14, %rbx
	jg	.LBB30_5
	jmp	.LBB30_14
	.align	16, 0x90
.LBB30_15:                              # %if.then31
                                        #   in Loop: Header=BB30_4 Depth=1
	movzbl	reload_inherited(%r14), %eax
	testq	%rax, %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	jne	.LBB30_18
# BB#16:                                # %lor.lhs.false35
                                        #   in Loop: Header=BB30_4 Depth=1
	cmpq	$0, reload_override_in-8(,%rbx,8)
	jne	.LBB30_18
# BB#17:                                # %lor.lhs.false35
                                        #   in Loop: Header=BB30_4 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%rdx, %rsi
	andq	%rsi, %rax
	movq	%rcx, %rdx
	movq	%r14, %rcx
	andq	%rsi, %rcx
	cmpq	%rax, %rcx
	movq	%rdx, %rcx
	jne	.LBB30_86
.LBB30_18:                              # %if.then42
                                        #   in Loop: Header=BB30_4 Depth=1
	movq	rld+48(%r15), %rdi
	movq	24(%rsp), %rbx          # 8-byte Reload
	leaq	1(%rbx), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	testq	%rdi, %rdi
	jne	.LBB30_28
# BB#19:                                #   in Loop: Header=BB30_4 Depth=1
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	jmp	.LBB30_20
.LBB30_28:                              # %while.cond48.preheader
                                        #   in Loop: Header=BB30_4 Depth=1
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	jmp	.LBB30_30
	.align	16, 0x90
.LBB30_29:                              # %while.body53
                                        #   in Loop: Header=BB30_30 Depth=2
	movq	8(%rdi), %rdi
.LBB30_30:                              # %while.body53
                                        #   Parent Loop BB30_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rdi), %eax
	cmpq	$63, %rax
	je	.LBB30_29
# BB#31:                                # %while.end57
                                        #   in Loop: Header=BB30_4 Depth=1
	movq	%r12, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB30_20
# BB#32:                                # %while.end57
                                        #   in Loop: Header=BB30_4 Depth=1
	addq	$2, %rbx
	movq	%rbx, 56(%rsp)          # 8-byte Spill
.LBB30_20:                              # %for.cond.outer.backedge
                                        #   in Loop: Header=BB30_4 Depth=1
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	56(%rsp), %rax          # 8-byte Reload
	jg	.LBB30_4
	jmp	.LBB30_21
.LBB30_14:
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, 56(%rsp)          # 8-byte Spill
	jmp	.LBB30_21
.LBB30_3:
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
.LBB30_21:                              # %for.end
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	32(%rbx), %rdi
	xorq	%rdx, %rdx
	movq	%r12, %rsi
	callq	count_occurrences
	movq	%rax, %r14
	movq	40(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB30_23
# BB#22:                                # %if.then70
	movq	32(%rbx), %r15
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	eliminate_regs
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	count_occurrences
	addq	%rax, %r14
.LBB30_23:                              # %if.end76
	movq	%r14, 40(%rsp)          # 8-byte Spill
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
	movq	%rbx, %rcx
	jg	.LBB30_86
# BB#24:                                # %for.cond84.preheader
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	24(%rax), %rbx
	cmpq	%rcx, %rbx
	je	.LBB30_43
# BB#25:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB30_26:                              # %for.body87
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	addq	$-32, %rax
	andq	%r15, %rax
	cmpq	$4, %rax
	ja	.LBB30_33
# BB#27:                                # %for.body87
                                        #   in Loop: Header=BB30_26 Depth=1
	jmpq	*.LJTI30_0(,%rax,8)
.LBB30_34:                              # %land.lhs.true
                                        #   in Loop: Header=BB30_26 Depth=1
	movq	32(%rbx), %rsi
	movq	%r12, %rdi
	movq	%rcx, %r14
	callq	reg_mentioned_p
	movq	%r14, %rcx
	testq	%r15, %rax
	jne	.LBB30_35
	.align	16, 0x90
.LBB30_33:                              # %for.cond84.backedge
                                        #   in Loop: Header=BB30_26 Depth=1
	movq	24(%rbx), %rbx
	cmpq	%rcx, %rbx
	jne	.LBB30_26
	jmp	.LBB30_43
.LBB30_35:                              # %while.cond114.preheader
	movzbl	(%rbx), %eax
	cmpq	$32, %rax
	jne	.LBB30_41
# BB#36:
	movabsq	$0, %r14
	.align	16, 0x90
.LBB30_37:                              # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$48, %rcx
	jne	.LBB30_41
# BB#38:                                # %while.body126
                                        #   in Loop: Header=BB30_37 Depth=1
	movq	8(%rax), %rsi
	movq	%r12, %rdi
	callq	rtx_equal_p
	testq	%r15, %rax
	movq	%r14, %rax
	je	.LBB30_40
# BB#39:                                # %while.body126
                                        #   in Loop: Header=BB30_37 Depth=1
	movabsq	$1, %rax
.LBB30_40:                              # %while.body126
                                        #   in Loop: Header=BB30_37 Depth=1
	movq	24(%rbx), %rbx
	movzbl	(%rbx), %ecx
	addq	%rax, 40(%rsp)          # 8-byte Folded Spill
	cmpq	$32, %rcx
	je	.LBB30_37
.LBB30_41:                              # %while.end140
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	40(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	48(%rsp), %rcx          # 8-byte Reload
	jg	.LBB30_86
# BB#42:                                # %while.end140
	cmpq	%rcx, %rbx
	jne	.LBB30_86
.LBB30_43:                              # %for.end153
	movq	%rcx, %r14
	movzbl	2(%r12), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%r13, %rcx
	cmpq	$8, %rcx
	je	.LBB30_47
# BB#44:                                # %for.end153
	leaq	-21(%r13), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB30_47
# BB#45:                                # %for.end153
	leaq	-45(%r13), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB30_47
# BB#46:                                # %for.end153
	leaq	-29(%r13), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB30_47
# BB#52:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB30_53
# BB#55:                                # %cond.true204
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	movq	8(%rsp), %rbx           # 8-byte Reload
	jmp	.LBB30_57
.LBB30_86:                              # %if.end441
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB30_47:                              # %cond.true
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	movq	8(%rsp), %rbx           # 8-byte Reload
	je	.LBB30_57
# BB#48:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB30_49
# BB#50:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB30_57
.LBB30_53:                              # %cond.false
	cmpq	$18, %rax
	movq	8(%rsp), %rbx           # 8-byte Reload
	jne	.LBB30_56
# BB#54:                                # %cond.true195
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB30_57
.LBB30_49:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB30_57
.LBB30_56:                              # %for.cond228.preheader
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB30_59
.LBB30_57:                              # %for.body232.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	addq	%rax, %r13
	leaq	-1(%rax), %rax
	movq	$3, %rcx
	shlq	%cl, %r13
	.align	16, 0x90
.LBB30_58:                              # %for.body232
                                        # =>This Inner Loop Header: Depth=1
	movq	$0, spill_reg_store-8(%r13)
	movq	$0, spill_reg_stored_to-8(%r13)
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%rax
	addq	$-8, %r13
	testq	%rdx, %rdx
	jg	.LBB30_58
.LBB30_59:                              # %for.end239
	movq	$32, %rcx
	movq	16(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$104, %rax, %r15
	movq	rld+8(%r15), %rax
	movq	rld(%r15), %rcx
	cmpq	%rcx, %rax
	je	.LBB30_87
# BB#60:                                # %land.lhs.true247
	movl	(%rbx), %edx
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%rdx,8), %rax
	movl	24(%rax), %ecx
	cmpq	$1, %rcx
	jne	.LBB30_87
# BB#61:                                # %land.lhs.true256
	movl	12(%rax), %ecx
	cmpq	$1, %rcx
	jne	.LBB30_87
# BB#62:                                # %land.lhs.true266
	movslq	36(%rax), %rax
	testq	%rax, %rax
	js	.LBB30_87
# BB#63:                                # %land.lhs.true276
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	find_regno_note
	movq	%rbx, %rcx
	testq	%rax, %rax
	je	.LBB30_87
# BB#64:                                # %if.then282
	movq	%r14, %rax
	movq	16(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB30_83
# BB#65:
	movq	%rax, %r14
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB30_66:                              # %for.body288
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB30_71
# BB#67:                                # %cond.true296
                                        #   in Loop: Header=BB30_66 Depth=1
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB30_69
# BB#68:                                # %cond.false308
                                        #   in Loop: Header=BB30_66 Depth=1
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB30_69:                              # %cond.end316
                                        #   in Loop: Header=BB30_66 Depth=1
	testq	%rax, %rax
	je	.LBB30_71
# BB#70:                                # %land.lhs.true320
                                        #   in Loop: Header=BB30_66 Depth=1
	movq	8(%rax), %rax
	cmpq	%r12, %rax
	je	.LBB30_85
	.align	16, 0x90
.LBB30_71:                              # %if.end327
                                        #   in Loop: Header=BB30_66 Depth=1
	movzbl	(%rbx), %eax
	addq	$-32, %rax
	andq	%r13, %rax
	cmpq	$4, %rax
	ja	.LBB30_85
# BB#72:                                # %if.end327
                                        #   in Loop: Header=BB30_66 Depth=1
	jmpq	*.LJTI30_1(,%rax,8)
.LBB30_84:                              # %land.lhs.true348
                                        #   in Loop: Header=BB30_66 Depth=1
	movq	32(%rbx), %rsi
	movq	%r12, %rdi
	callq	reg_mentioned_p
	testq	%r13, %rax
	jne	.LBB30_87
	.align	16, 0x90
.LBB30_85:                              # %for.inc357
                                        #   in Loop: Header=BB30_66 Depth=1
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB30_66
.LBB30_73:                              # %for.cond365.preheader
	movq	%r14, %rsi
	movq	16(%rsi), %rbx
	testq	%rbx, %rbx
	movq	8(%rsp), %rcx           # 8-byte Reload
	je	.LBB30_83
.LBB30_74:                              # %for.body367
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB30_80
# BB#75:                                # %cond.true376
                                        #   in Loop: Header=BB30_74 Depth=1
	movq	%rcx, %r13
	movq	%rsi, %r14
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB30_77
# BB#76:                                # %cond.false388
                                        #   in Loop: Header=BB30_74 Depth=1
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB30_77:                              # %cond.end396
                                        #   in Loop: Header=BB30_74 Depth=1
	movq	%r14, %rsi
	testq	%rax, %rax
	movq	%r13, %rcx
	je	.LBB30_80
# BB#78:                                # %land.lhs.true400
                                        #   in Loop: Header=BB30_74 Depth=1
	movq	8(%rax), %rax
	cmpq	%r12, %rax
	jne	.LBB30_80
# BB#79:                                # %if.then406
                                        #   in Loop: Header=BB30_74 Depth=1
	movq	%rbx, %rdi
	movq	%rcx, %r13
	movq	%rsi, %r14
	callq	delete_address_reloads
	movq	%rbx, %rdi
	callq	delete_insn
	movq	%r14, %rsi
	movq	%r13, %rcx
.LBB30_80:                              # %if.end408
                                        #   in Loop: Header=BB30_74 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$33, %rax
	je	.LBB30_83
# BB#81:                                # %if.end408
                                        #   in Loop: Header=BB30_74 Depth=1
	cmpq	$36, %rax
	je	.LBB30_83
# BB#82:                                # %for.inc420
                                        #   in Loop: Header=BB30_74 Depth=1
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB30_74
.LBB30_83:                              # %for.end424
	movq	rld+64(%r15), %rax
	movzbl	8(%rax), %eax
	movl	(%rcx), %edi
	movq	reg_renumber(%rip), %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movw	%ax, (%rcx,%rdi,2)
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	alter_reg  # TAILCALL
.LBB30_87:                              # %if.then354
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	delete_address_reloads
	movq	%rbx, %rdi
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	delete_insn  # TAILCALL
.Ltmp421:
	.size	delete_output_reload, .Ltmp421-delete_output_reload
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI30_0:
	.quad	.LBB30_34
	.quad	.LBB30_86
	.quad	.LBB30_34
	.quad	.LBB30_33
	.quad	.LBB30_86
.LJTI30_1:
	.quad	.LBB30_84
	.quad	.LBB30_73
	.quad	.LBB30_84
	.quad	.LBB30_85
	.quad	.LBB30_73

	.text
	.align	16, 0x90
	.type	delete_address_reloads,@function
delete_address_reloads:                 # @delete_address_reloads
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp428:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp429:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp430:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp431:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp432:
	.cfi_def_cfa_offset 48
.Ltmp433:
	.cfi_offset %rbx, -48
.Ltmp434:
	.cfi_offset %r12, -40
.Ltmp435:
	.cfi_offset %r13, -32
.Ltmp436:
	.cfi_offset %r14, -24
.Ltmp437:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	movq	%rsi, %r14
	cmpq	$105, %rax
	jne	.LBB31_6
# BB#1:                                 # %cond.true
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB31_3
# BB#2:                                 # %cond.false
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB31_3:                               # %cond.end15
	testq	%rax, %rax
	je	.LBB31_6
# BB#4:                                 # %if.then
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB31_6
# BB#5:                                 # %if.then25
	movq	8(%rax), %rsi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	delete_address_reloads_1
.LBB31_6:                               # %if.end29
	movq	16(%rbx), %r13
	testq	%r13, %r13
	je	.LBB31_24
# BB#7:                                 # %if.end29
	movq	24(%rbx), %r14
	testq	%r14, %r14
	je	.LBB31_24
# BB#8:                                 # %if.end39
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	xorq	%r15, %r15
	cmpq	$105, %rax
	jne	.LBB31_11
# BB#9:                                 # %cond.true47
	movq	32(%r14), %r15
	movzbl	(%r15), %eax
	cmpq	$47, %rax
	je	.LBB31_11
# BB#10:                                # %cond.false59
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	single_set_2
	movq	%rax, %r15
.LBB31_11:                              # %cond.end67
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB31_24
# BB#12:                                # %cond.true76
	movq	32(%r13), %r12
	movzbl	(%r12), %eax
	cmpq	$47, %rax
	je	.LBB31_14
# BB#13:                                # %cond.false88
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	single_set_2
	movq	%rax, %r12
.LBB31_14:                              # %cond.end96
	testq	%r15, %r15
	je	.LBB31_24
# BB#15:                                # %cond.end96
	testq	%r12, %r12
	je	.LBB31_24
# BB#16:                                # %lor.lhs.false101
	movq	16(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$75, %rcx
	jne	.LBB31_24
# BB#17:                                # %lor.lhs.false109
	movq	16(%r12), %rcx
	movzbl	(%rcx), %edx
	cmpq	$75, %rdx
	jne	.LBB31_24
# BB#18:                                # %lor.lhs.false117
	movq	16(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB31_24
# BB#19:                                # %lor.lhs.false128
	movq	16(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB31_24
# BB#20:                                # %if.end140
	movq	8(%r15), %rbx
	movq	8(%r12), %rsi
	movq	%rbx, %rdi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB31_24
# BB#21:                                # %lor.lhs.false149
	movq	16(%r15), %rax
	movq	8(%rax), %rsi
	movq	%rbx, %rdi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB31_24
# BB#22:                                # %lor.lhs.false158
	movq	16(%r12), %rax
	movq	8(%rax), %rsi
	movq	%rbx, %rdi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB31_24
# BB#23:                                # %lor.lhs.false167
	movq	16(%r15), %rax
	movq	16(%r12), %rcx
	movq	16(%rax), %rax
	movq	16(%rcx), %rcx
	movq	8(%rax), %rax
	movq	8(%rcx), %rcx
	addq	%rax, %rcx
	jne	.LBB31_24
# BB#25:                                # %if.end188
	movq	%r13, %rdi
	callq	delete_related_insns
	movq	%r14, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	delete_related_insns  # TAILCALL
.LBB31_24:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp438:
	.size	delete_address_reloads, .Ltmp438-delete_address_reloads
	.cfi_endproc

	.align	16, 0x90
	.type	delete_address_reloads_1,@function
delete_address_reloads_1:               # @delete_address_reloads_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp445:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp446:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp447:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp448:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp449:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp450:
	.cfi_def_cfa_offset 64
.Ltmp451:
	.cfi_offset %rbx, -48
.Ltmp452:
	.cfi_offset %r12, -40
.Ltmp453:
	.cfi_offset %r13, -32
.Ltmp454:
	.cfi_offset %r14, -24
.Ltmp455:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movzbl	(%r12), %eax
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %rbx
	cmpq	$61, %rax
	jne	.LBB32_1
# BB#11:                                # %if.end37
	movl	8(%r12), %eax
	movswq	spill_reg_order(%rax,%rax), %rax
	testq	%rax, %rax
	js	.LBB32_6
# BB#12:                                # %for.cond50.preheader
	movq	16(%rbx), %r14
	testq	%r14, %r14
	je	.LBB32_6
# BB#13:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
.LBB32_14:                              # %for.body51
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %eax
	cmpq	$33, %rax
	je	.LBB32_6
# BB#15:                                # %for.body51
                                        #   in Loop: Header=BB32_14 Depth=1
	cmpq	$36, %rax
	je	.LBB32_6
# BB#16:                                # %if.end59
                                        #   in Loop: Header=BB32_14 Depth=1
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB32_19
# BB#17:                                # %if.end66
                                        #   in Loop: Header=BB32_14 Depth=1
	movq	32(%r14), %rsi
	movq	%r12, %rdi
	callq	reg_set_p
	testq	%r15, %rax
	jne	.LBB32_20
# BB#18:                                # %if.end72
                                        #   in Loop: Header=BB32_14 Depth=1
	movq	32(%r14), %rsi
	movq	%r12, %rdi
	callq	reg_referenced_p
	testq	%r15, %rax
	jne	.LBB32_6
.LBB32_19:                              # %for.cond50.backedge
                                        #   in Loop: Header=BB32_14 Depth=1
	movq	16(%r14), %r14
	testq	%r14, %r14
	jne	.LBB32_14
	jmp	.LBB32_6
.LBB32_1:                               # %if.then
	movzbl	rtx_length(%rax), %r15d
	testq	%r15, %r15
	je	.LBB32_6
# BB#2:                                 # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	.align	16, 0x90
.LBB32_3:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB32_9 Depth 2
	movq	%r15, %r14
	movq	(%rsp), %rax            # 8-byte Reload
	movzbl	-1(%rax,%r14), %eax
	leaq	-1(%r14), %r15
	cmpq	$69, %rax
	jne	.LBB32_4
# BB#8:                                 # %if.then18
                                        #   in Loop: Header=BB32_3 Depth=1
	movq	(%r12,%r14,8), %rax
	movslq	(%rax), %r13
	testq	%r13, %r13
	jle	.LBB32_5
	jmp	.LBB32_9
	.align	16, 0x90
.LBB32_10:                              # %for.body26.for.body26_crit_edge
                                        #   in Loop: Header=BB32_9 Depth=2
	movq	(%r12,%r14,8), %rax
.LBB32_9:                               # %for.body26.lr.ph
                                        #   Parent Loop BB32_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%r13
	movq	8(%rax,%r13,8), %rsi
	movq	%rbx, %rdi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	delete_address_reloads_1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB32_10
	jmp	.LBB32_5
	.align	16, 0x90
.LBB32_4:                               # %for.body
                                        #   in Loop: Header=BB32_3 Depth=1
	cmpq	$101, %rax
	jne	.LBB32_5
# BB#7:                                 # %if.then10
                                        #   in Loop: Header=BB32_3 Depth=1
	movq	(%r12,%r14,8), %rsi
	movq	%rbx, %rdi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	delete_address_reloads_1
.LBB32_5:                               # %for.cond.backedge
                                        #   in Loop: Header=BB32_3 Depth=1
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB32_3
	jmp	.LBB32_6
.LBB32_20:                              # %lor.lhs.false86
	movslq	8(%r14), %rax
	movslq	reload_first_uid(%rip), %rcx
	cmpq	%rcx, %rax
	jl	.LBB32_6
# BB#21:                                # %if.end92
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB32_6
# BB#22:                                # %cond.true
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB32_24
# BB#23:                                # %cond.false
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB32_24:                              # %cond.end116
	testq	%rax, %rax
	je	.LBB32_6
# BB#25:                                # %if.end120
	movq	8(%rax), %r13
	movzbl	(%r13), %ecx
	cmpq	$61, %rcx
	jne	.LBB32_6
# BB#26:                                # %lor.lhs.false128
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%rax, %r12
	callq	rtx_equal_p
	movq	%r12, %rcx
	testq	%r15, %rax
	je	.LBB32_6
# BB#27:                                # %if.end132
	movq	%rcx, %r12
	movq	32(%rbx), %rsi
	movq	%r13, %rdi
                                        # kill: R13<def> RDI<kill>
	jmp	.LBB32_28
.LBB32_50:                              # %if.end251
                                        #   in Loop: Header=BB32_28 Depth=1
	movq	32(%rbx), %rax
	movq	%r13, %rdi
	movq	%rax, %rsi
.LBB32_28:                              # %if.end132
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB32_29 Depth 2
                                        #     Child Loop BB32_46 Depth 2
	callq	reg_set_p
	testq	%r15, %rax
	jne	.LBB32_52
.LBB32_29:                              # %for.cond142.preheader
                                        #   Parent Loop BB32_28 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB32_52
# BB#30:                                # %for.body144
                                        #   in Loop: Header=BB32_29 Depth=2
	movzbl	(%rbx), %eax
	cmpq	$36, %rax
	je	.LBB32_52
# BB#31:                                # %if.end150
                                        #   in Loop: Header=BB32_29 Depth=2
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB32_29
# BB#32:                                # %if.end159
                                        #   in Loop: Header=BB32_28 Depth=1
	movq	32(%rbx), %rax
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	reg_referenced_p
	testq	%r15, %rax
	jne	.LBB32_33
# BB#43:                                # %if.end216
                                        #   in Loop: Header=BB32_28 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$33, %rax
	je	.LBB32_52
# BB#44:                                # %if.end222
                                        #   in Loop: Header=BB32_28 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	cmpq	%rax, %rbx
	jne	.LBB32_50
# BB#45:                                # %if.then225
                                        #   in Loop: Header=BB32_28 Depth=1
	movslq	n_reloads(%rip), %rax
	imulq	$104, %rax, %rcx
	leaq	rld-40(%rcx), %rdx
	movq	%r13, %rsi
	movq	%r12, %rcx
.LBB32_46:                              # %for.cond227
                                        #   Parent Loop BB32_28 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, %r12
	movq	%rsi, %r13
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jle	.LBB32_50
# BB#47:                                # %for.body230
                                        #   in Loop: Header=BB32_46 Depth=2
	movq	(%rdx), %rcx
	movq	%r13, %rsi
	cmpq	%rsi, %rcx
	movq	%r12, %rcx
	jne	.LBB32_49
# BB#48:                                # %land.lhs.true236
                                        #   in Loop: Header=BB32_46 Depth=2
	movq	%rcx, %rdi
	movzbl	reload_inherited-1(%rax), %ecx
	testq	%rcx, %rcx
	movq	%rdi, %rcx
	jne	.LBB32_6
.LBB32_49:                              # %lor.lhs.false241
                                        #   in Loop: Header=BB32_46 Depth=2
	movq	%rcx, %rdi
	movq	reload_override_in-8(,%rax,8), %rcx
	decq	%rax
	addq	$-104, %rdx
	cmpq	%rsi, %rcx
	movq	%rdi, %rcx
	jne	.LBB32_46
	jmp	.LBB32_6
.LBB32_33:                              # %if.then165
	movq	8(%rsp), %rax           # 8-byte Reload
	cmpq	%rax, %rbx
	movq	%r13, %rdi
	movq	%r12, %rcx
	jne	.LBB32_6
# BB#34:                                # %if.then168
	movslq	n_reloads(%rip), %rax
	movq	%rcx, %rsi
	imulq	$104, %rax, %rcx
	leaq	rld-40(%rcx), %rdx
	movq	%rsi, %rcx
	movq	%rax, %rsi
.LBB32_35:                              # %for.cond170
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %r8
	movq	%rdi, %rbx
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	jle	.LBB32_39
# BB#36:                                # %for.body173
                                        #   in Loop: Header=BB32_35 Depth=1
	movq	(%rdx), %rcx
	movq	%rbx, %rdi
	cmpq	%rdi, %rcx
	movq	%r8, %rcx
	jne	.LBB32_38
# BB#37:                                # %land.lhs.true
                                        #   in Loop: Header=BB32_35 Depth=1
	movq	%rcx, %rbx
	movzbl	reload_inherited-1(%rsi), %ecx
	testq	%rcx, %rcx
	movq	%rbx, %rcx
	jne	.LBB32_6
.LBB32_38:                              # %lor.lhs.false182
                                        #   in Loop: Header=BB32_35 Depth=1
	movq	%rcx, %rbx
	movq	reload_override_in-8(,%rsi,8), %rcx
	decq	%rsi
	addq	$-104, %rdx
	cmpq	%rdi, %rcx
	movq	%rbx, %rcx
	jne	.LBB32_35
	jmp	.LBB32_6
.LBB32_39:                              # %for.end191
	testq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r8, %rcx
	jle	.LBB32_6
# BB#40:                                # %for.body196.preheader
	movq	%rcx, %rdi
	imulq	$104, %rax, %rcx
	leaq	rld-40(%rcx), %rdx
	movq	%rdi, %rcx
	leaq	-1(%rax), %rax
.LBB32_41:                              # %for.body196
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %r12
	movq	%rsi, %r13
	cmpq	$0, -64(%rdx)
	je	.LBB32_42
# BB#51:                                # %land.lhs.true200
                                        #   in Loop: Header=BB32_41 Depth=1
	movq	(%rdx), %rcx
	cmpq	%r13, %rcx
	je	.LBB32_52
.LBB32_42:                              # %for.cond193.backedge
                                        #   in Loop: Header=BB32_41 Depth=1
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	$-104, %rdx
	decq	%rax
	testq	%rsi, %rsi
	movq	%r13, %rsi
	movq	%r12, %rcx
	jg	.LBB32_41
.LBB32_6:                               # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB32_52:                              # %if.end264
	movq	16(%r12), %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	delete_address_reloads_1
	movl	8(%r13), %eax
	movl	%r15d, reg_reloaded_contents(,%rax,4)
	movq	%r14, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	delete_insn  # TAILCALL
.Ltmp456:
	.size	delete_address_reloads_1, .Ltmp456-delete_address_reloads_1
	.cfi_endproc

	.align	16, 0x90
	.type	free_for_value_p,@function
free_for_value_p:                       # @free_for_value_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp463:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp464:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp465:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp466:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp467:
	.cfi_def_cfa_offset 48
	subq	$160, %rsp
.Ltmp468:
	.cfi_def_cfa_offset 208
.Ltmp469:
	.cfi_offset %rbx, -48
.Ltmp470:
	.cfi_offset %r12, -40
.Ltmp471:
	.cfi_offset %r13, -32
.Ltmp472:
	.cfi_offset %r14, -24
.Ltmp473:
	.cfi_offset %r15, -16
	movq	%r9, 120(%rsp)          # 8-byte Spill
	movq	%r8, 88(%rsp)           # 8-byte Spill
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	%rdx, %r8
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	movq	%rdi, %rax
	andq	%rcx, %rax
	cmpq	$8, %rax
	je	.LBB33_4
# BB#1:                                 # %entry
	leaq	-21(%rdi), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB33_4
# BB#2:                                 # %entry
	leaq	-45(%rdi), %rcx
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB33_4
# BB#3:                                 # %entry
	movq	%rdi, %rcx
	addq	$-29, %rcx
	andq	%rax, %rcx
	cmpq	$7, %rcx
	jbe	.LBB33_4
# BB#9:                                 # %cond.false
	andq	%rsi, %rax
	cmpq	$24, %rax
	jne	.LBB33_10
# BB#12:                                # %cond.true22
	movq	%rdi, 128(%rsp)         # 8-byte Spill
	movzbl	target_flags+3(%rip), %esi
	andq	$2, %rsi
	xorq	$6, %rsi
	jmp	.LBB33_14
.LBB33_4:                               # %cond.true
	movq	%rdi, 128(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	mode_class(,%rsi,4), %eax
	cmpq	$5, %rax
	movq	$2, %rsi
	je	.LBB33_14
# BB#5:                                 # %lor.rhs
	cmpq	$6, %rax
	jne	.LBB33_6
# BB#7:                                 # %lor.rhs
	movabsq	$1, %rsi
	incq	%rsi
	jmp	.LBB33_14
.LBB33_10:                              # %cond.false
	movq	%rdi, 128(%rsp)         # 8-byte Spill
	cmpq	$18, %rax
	jne	.LBB33_13
# BB#11:                                # %cond.true18
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	xorq	$3, %rsi
	jmp	.LBB33_14
.LBB33_6:
	movabsq	$0, %rsi
	incq	%rsi
	jmp	.LBB33_14
.LBB33_13:                              # %while.cond.preheader
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	movl	target_flags(%rip), %edi
	sarq	%cl, %rsi
	movzbl	mode_size(%rsi), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	movq	%rax, %rsi
	movq	$32, %rcx
                                        # kill: RAX<def> RSI<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %rbx
	jle	.LBB33_109
.LBB33_14:                              # %while.body.lr.ph
	movq	$32, %rcx
	movq	208(%rsp), %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$104, %rax, %rax
	leaq	rld+48(%rax), %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	leaq	rld+8(%rax), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	%r8, %rbx
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	leaq	2(,%rbx,4), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	leaq	3(,%rbx,4), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	leaq	4(,%rbx,4), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	leaq	124(%rbx), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	leaq	125(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	72(%rsp), %rax          # 8-byte Reload
	andq	%r8, %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
.LBB33_15:                              # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB33_36 Depth 2
	movq	128(%rsp), %rax         # 8-byte Reload
	leaq	-1(%rsi,%rax), %rcx
	movq	%rcx, 152(%rsp)         # 8-byte Spill
	movq	reload_reg_unavailable(%rip), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	xorq	%rbx, %rbx
	testq	%rdx, %rax
	jne	.LBB33_109
# BB#16:                                # %if.end.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movq	const_int_rtx+512(%rip), %rax
	movq	$0, 136(%rsp)           # 8-byte Folded Spill
	movq	120(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB33_18
# BB#17:                                # %if.end.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 136(%rsp)         # 8-byte Spill
.LBB33_18:                              # %if.end.i
                                        #   in Loop: Header=BB33_15 Depth=1
	decq	%rsi
	movq	%rsi, 144(%rsp)         # 8-byte Spill
	cmpq	$10, 112(%rsp)          # 8-byte Folded Reload
	ja	.LBB33_23
# BB#19:                                # %if.end.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movq	$121, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	jmpq	*.LJTI33_0(,%rcx,8)
.LBB33_26:                              # %sw.bb11.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movq	120(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	movq	24(%rsp), %rdx          # 8-byte Reload
	je	.LBB33_33
# BB#27:                                # %sw.bb11.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movq	$123, %rdx
	jmp	.LBB33_33
.LBB33_25:                              # %sw.bb8.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movq	32(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB33_33
.LBB33_24:                              # %sw.bb7.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movq	40(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB33_33
.LBB33_32:                              # %sw.bb22.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movq	8(%rsp), %rdx           # 8-byte Reload
	jmp	.LBB33_33
.LBB33_31:                              # %sw.bb20.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movq	16(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB33_33
.LBB33_28:                              # %sw.bb17.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movq	120(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	movabsq	$0, %rax
	movq	%rax, %rdx
	je	.LBB33_30
# BB#29:                                # %sw.bb17.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movabsq	$1, %rax
	movq	%rax, %rdx
.LBB33_30:                              # %sw.bb17.i
                                        #   in Loop: Header=BB33_15 Depth=1
	orq	$122, %rdx
	jmp	.LBB33_33
.LBB33_22:                              # %sw.bb4.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movq	120(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	movq	$1, %rdx
	je	.LBB33_33
	.align	16, 0x90
.LBB33_23:                              # %sw.default.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movq	$155, %rdx
.LBB33_33:                              # %sw.epilog.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	movslq	n_reloads(%rip), %rax
	testq	%rax, %rax
	jle	.LBB33_34
# BB#35:                                # %for.body.lr.ph.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	$1, %r14
	movabsq	$rld, %r12
	jmp	.LBB33_36
.LBB33_20:                              # %sw.bb.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movq	120(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	movabsq	$0, %rax
	movq	%rax, %rdx
	je	.LBB33_33
# BB#21:                                # %sw.bb.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movabsq	$1, %rax
	movq	%rax, %rdx
	jmp	.LBB33_33
.LBB33_72:                              #   in Loop: Header=BB33_36 Depth=2
	xorq	%rbx, %rbx
	leaq	3(,%rax,4), %rdi
	jmp	.LBB33_97
	.align	16, 0x90
.LBB33_36:                              # %for.body.i
                                        #   Parent Loop BB33_15 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	64(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB33_105
# BB#37:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	jne	.LBB33_105
# BB#38:                                # %land.lhs.true27.i
                                        #   in Loop: Header=BB33_36 Depth=2
	leaq	-1(%r14), %r13
	movq	%rbx, %rdi
	movq	%r8, %r15
	callq	true_regnum
	movq	%r15, %r8
	movl	8(%rbx), %edx
	movq	152(%rsp), %rsi         # 8-byte Reload
	movzbl	2(%rbx), %ecx
	subq	%rax, %rsi
	movq	%rdx, %rax
	movabsq	$4294967288, %rdi       # imm = 0xFFFFFFF8
	andq	%rdi, %rax
	cmpq	$8, %rax
	je	.LBB33_42
# BB#39:                                # %land.lhs.true27.i
                                        #   in Loop: Header=BB33_36 Depth=2
	leaq	-21(%rdx), %rax
	andq	%r8, %rax
	cmpq	$8, %rax
	jb	.LBB33_42
# BB#40:                                # %land.lhs.true27.i
                                        #   in Loop: Header=BB33_36 Depth=2
	leaq	-45(%rdx), %rax
	andq	%r8, %rax
	cmpq	$8, %rax
	jb	.LBB33_42
# BB#41:                                # %land.lhs.true27.i
                                        #   in Loop: Header=BB33_36 Depth=2
	addq	$-29, %rdx
	andq	%r8, %rdx
	cmpq	$7, %rdx
	jbe	.LBB33_42
# BB#46:                                # %cond.false77.i
                                        #   in Loop: Header=BB33_36 Depth=2
	cmpq	$24, %rcx
	jne	.LBB33_47
# BB#49:                                # %cond.true91.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB33_51
.LBB33_42:                              # %cond.true64.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movl	mode_class(,%rcx,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB33_51
# BB#43:                                # %lor.rhs.i
                                        #   in Loop: Header=BB33_36 Depth=2
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB33_45
# BB#44:                                # %lor.rhs.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movabsq	$1, %rax
.LBB33_45:                              # %lor.rhs.i
                                        #   in Loop: Header=BB33_36 Depth=2
	incq	%rax
	jmp	.LBB33_51
.LBB33_47:                              # %cond.false77.i
                                        #   in Loop: Header=BB33_36 Depth=2
	cmpq	$18, %rcx
	jne	.LBB33_50
# BB#48:                                # %cond.true82.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB33_51
.LBB33_50:                              # %cond.false95.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rcx), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	.align	16, 0x90
.LBB33_51:                              # %cond.end113.i
                                        #   in Loop: Header=BB33_36 Depth=2
	decq	%rax
	andq	%r8, %rax
	andq	%r8, %rsi
	cmpq	%rax, %rsi
	ja	.LBB33_52
# BB#53:                                # %cond.end113.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	208(%rsp), %r15
	andq	%r8, %r15
	andq	%r8, %r13
	cmpq	%r15, %r13
	je	.LBB33_54
# BB#55:                                # %if.then121.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	(%r12), %r13
	movq	%rbx, %rdi
	movq	%r8, %rbx
	callq	true_regnum
	movq	%rbx, %r8
	andq	%r8, %rax
	movq	128(%rsp), %rcx         # 8-byte Reload
	andq	%r8, %rcx
	cmpq	%rcx, %rax
	je	.LBB33_57
# BB#56:                                # %if.then121.i
                                        #   in Loop: Header=BB33_36 Depth=2
	xorq	%r13, %r13
.LBB33_57:                              # %if.then121.i
                                        #   in Loop: Header=BB33_36 Depth=2
	testq	%r13, %r13
	je	.LBB33_61
# BB#58:                                # %lor.lhs.false130.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	%r13, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%r8, %rbx
	callq	rtx_equal_p
	movq	%rbx, %r8
	testq	%r8, %rax
	je	.LBB33_61
# BB#59:                                # %lor.lhs.false133.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	8(%r12), %rax
	movq	136(%rsp), %rcx         # 8-byte Reload
	orq	%rcx, %rax
	je	.LBB33_60
.LBB33_61:                              # %if.then140.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	%r13, 96(%rsp)          # 8-byte Spill
	movl	92(%r12), %eax
	xorq	%rbx, %rbx
	cmpq	$10, %rax
	ja	.LBB33_109
# BB#62:                                # %if.then140.i
                                        #   in Loop: Header=BB33_36 Depth=2
	xorq	%rdi, %rdi
	movq	144(%rsp), %rsi         # 8-byte Reload
	jmpq	*.LJTI33_1(,%rax,8)
.LBB33_95:                              # %sw.bb205.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movl	72(%r12), %eax
	leaq	4(,%rax,4), %rdi
	jmp	.LBB33_96
.LBB33_52:                              #   in Loop: Header=BB33_36 Depth=2
	movq	144(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB33_105
.LBB33_54:                              #   in Loop: Header=BB33_36 Depth=2
	movq	144(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB33_105
.LBB33_60:                              #   in Loop: Header=BB33_36 Depth=2
	movq	144(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB33_105
.LBB33_82:                              # %sw.bb229.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	$124, %rdi
	jmp	.LBB33_97
.LBB33_81:                              # %sw.bb228.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	$123, %rdi
	jmp	.LBB33_97
.LBB33_70:                              # %sw.bb182.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movl	72(%r12), %eax
	movq	72(%rsp), %rcx          # 8-byte Reload
	testq	%r8, %rcx
	jne	.LBB33_75
# BB#71:                                # %sw.bb182.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	56(%rsp), %rcx          # 8-byte Reload
	andq	%r8, %rcx
	movq	%rax, %rdx
	andq	%r8, %rdx
	cmpq	%rcx, %rdx
	jne	.LBB33_72
# BB#73:                                # %sw.bb182.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	216(%rsp), %rcx
	testq	%r8, %rcx
	movabsq	$0, %rbx
	je	.LBB33_75
# BB#74:                                # %land.lhs.true193.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	48(%rsp), %rax          # 8-byte Reload
	cmpq	$0, (%rax)
	movq	56(%rsp), %rax          # 8-byte Reload
	je	.LBB33_105
.LBB33_75:                              # %if.end199.i
                                        #   in Loop: Header=BB33_36 Depth=2
	leaq	3(,%rax,4), %rdi
	jmp	.LBB33_97
.LBB33_63:                              # %sw.bb144.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
	testq	%r8, %rax
	jne	.LBB33_64
# BB#66:                                # %land.lhs.true162.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movl	72(%r12), %eax
	movq	56(%rsp), %rcx          # 8-byte Reload
	andq	%r8, %rcx
	cmpq	%rcx, %rax
	je	.LBB33_67
	jmp	.LBB33_69
.LBB33_88:                              # %sw.bb250.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movl	72(%r12), %edi
	addq	$125, %rdi
	jmp	.LBB33_97
.LBB33_83:                              # %sw.bb230.i
                                        #   in Loop: Header=BB33_36 Depth=2
	cmpq	$5, 112(%rsp)           # 8-byte Folded Reload
	jne	.LBB33_87
# BB#84:                                # %land.lhs.true233.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	%r14, %rax
	andq	%r8, %rax
	cmpq	%r15, %rax
	jne	.LBB33_87
# BB#85:                                # %land.lhs.true233.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	216(%rsp), %rax
	testq	%r8, %rax
	je	.LBB33_87
# BB#86:                                # %land.lhs.true239.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	48(%rsp), %rax          # 8-byte Reload
	cmpq	$0, (%rax)
	je	.LBB33_105
.LBB33_87:                              # %if.end245.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movl	72(%r12), %edi
	addq	$124, %rdi
	jmp	.LBB33_97
.LBB33_80:                              # %sw.bb227.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	$122, %rdi
.LBB33_96:                              # %sw.epilog276.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	$1, 80(%rsp)            # 8-byte Folded Spill
	jmp	.LBB33_97
.LBB33_76:                              # %sw.bb211.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	$121, %rdi
	cmpq	$7, 112(%rsp)           # 8-byte Folded Reload
	jne	.LBB33_97
# BB#77:                                # %land.lhs.true214.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	%r14, %rax
	andq	%r8, %rax
	cmpq	%r15, %rax
	jne	.LBB33_97
# BB#78:                                # %land.lhs.true214.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	216(%rsp), %rax
	testq	%r8, %rax
	je	.LBB33_97
# BB#79:                                # %land.lhs.true220.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	48(%rsp), %rax          # 8-byte Reload
	cmpq	$0, (%rax)
	jne	.LBB33_97
	jmp	.LBB33_105
.LBB33_89:                              # %sw.bb255.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	%r8, %r13
	movq	%rsi, %r15
	cmpq	$0, (%r12)
	je	.LBB33_91
# BB#90:                                # %lor.lhs.false260.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	rtx_equal_p
	testq	%r13, %rax
	jne	.LBB33_91
# BB#94:                                # %if.end271.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	$1, %rdi
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	movq	%r15, %rsi
	movq	%r13, %r8
	je	.LBB33_97
	jmp	.LBB33_109
.LBB33_91:                              # %if.then263.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	8(%r12), %rdi
	callq	earlyclobber_operand_p
	movq	%r13, %r8
	testq	%r8, %rax
	movabsq	$0, %rax
	movq	%rax, %rdi
	jne	.LBB33_93
# BB#92:                                # %if.then263.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movabsq	$1, %rax
	movq	%rax, %rdi
.LBB33_93:                              # %if.then263.i
                                        #   in Loop: Header=BB33_36 Depth=2
	addq	$123, %rdi
	movq	%r15, %rsi
	jmp	.LBB33_97
.LBB33_64:                              # %sw.bb144.i
                                        #   in Loop: Header=BB33_36 Depth=2
	cmpq	$3, 112(%rsp)           # 8-byte Folded Reload
	jne	.LBB33_69
# BB#65:                                # %land.lhs.true147.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	%r14, %rax
	andq	%r8, %rax
	cmpq	%r15, %rax
	jne	.LBB33_69
.LBB33_67:                              # %land.lhs.true162.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	216(%rsp), %rax
	testq	%r8, %rax
	je	.LBB33_69
# BB#68:                                # %land.lhs.true170.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	48(%rsp), %rax          # 8-byte Reload
	cmpq	$0, (%rax)
	je	.LBB33_105
.LBB33_69:                              # %if.end176.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movl	72(%r12), %eax
	leaq	2(,%rax,4), %rdi
.LBB33_97:                              # %sw.epilog276.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	104(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB33_102
# BB#98:                                # %land.lhs.true279.i
                                        #   in Loop: Header=BB33_36 Depth=2
	cmpq	$0, (%r12)
	je	.LBB33_109
# BB#99:                                # %lor.lhs.false284.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	%rdi, %r13
	movq	%r8, %rbx
	movq	%rsi, %r15
	cmpq	$0, 8(%r12)
	jne	.LBB33_100
# BB#101:                               # %lor.lhs.false289.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	rtx_equal_p
	movq	%rbx, %r8
	testq	%r8, %rax
	movq	%r15, %rsi
	movq	%r13, %rdi
	movabsq	$0, %rbx
	je	.LBB33_109
.LBB33_102:                             # %lor.lhs.false292.i
                                        #   in Loop: Header=BB33_36 Depth=2
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	je	.LBB33_105
# BB#103:                               # %land.lhs.true294.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	64(%rsp), %rax          # 8-byte Reload
	cmpq	$0, (%rax)
	je	.LBB33_105
# BB#104:                               # %land.lhs.true294.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$122, %rdi
	jg	.LBB33_109
	.align	16, 0x90
.LBB33_105:                             # %for.inc.i
                                        #   in Loop: Header=BB33_36 Depth=2
	movslq	n_reloads(%rip), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r14
	addq	$104, %r12
	cmpq	%rax, %rdx
	jl	.LBB33_36
# BB#106:                               # %for.end.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movq	80(%rsp), %rax          # 8-byte Reload
	testq	%r8, %rax
	je	.LBB33_34
# BB#107:                               # %for.end.i
                                        #   in Loop: Header=BB33_15 Depth=1
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	je	.LBB33_34
# BB#108:                               # %land.lhs.true308.i
                                        #   in Loop: Header=BB33_15 Depth=1
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	%rsi, %r14
	movq	%r8, %rbx
	callq	earlyclobber_operand_p
	movq	%rbx, %r8
	movq	%r14, %rsi
	xorq	%rbx, %rbx
	testq	%r8, %rax
	jne	.LBB33_109
	.align	16, 0x90
.LBB33_34:                              # %while.cond.backedge
                                        #   in Loop: Header=BB33_15 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %rbx
	testq	%rax, %rax
	jg	.LBB33_15
	jmp	.LBB33_109
.LBB33_100:
	xorq	%rbx, %rbx
.LBB33_109:                             # %return
	movq	%rbx, %rax
	addq	$160, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp474:
	.size	free_for_value_p, .Ltmp474-free_for_value_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI33_0:
	.quad	.LBB33_26
	.quad	.LBB33_23
	.quad	.LBB33_23
	.quad	.LBB33_25
	.quad	.LBB33_24
	.quad	.LBB33_32
	.quad	.LBB33_31
	.quad	.LBB33_28
	.quad	.LBB33_33
	.quad	.LBB33_22
	.quad	.LBB33_20
.LJTI33_1:
	.quad	.LBB33_95
	.quad	.LBB33_82
	.quad	.LBB33_81
	.quad	.LBB33_70
	.quad	.LBB33_63
	.quad	.LBB33_88
	.quad	.LBB33_83
	.quad	.LBB33_80
	.quad	.LBB33_76
	.quad	.LBB33_89
	.quad	.LBB33_97

	.text
	.align	16, 0x90
	.type	reload_reg_class_lower,@function
reload_reg_class_lower:                 # @reload_reg_class_lower
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp478:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp479:
	.cfi_def_cfa_offset 24
.Ltmp480:
	.cfi_offset %rbx, -24
.Ltmp481:
	.cfi_offset %r14, -16
	movswq	(%rdi), %rcx
	movswq	(%rsi), %r11
	imulq	$104, %rcx, %rdi
	movzbl	rld+96(%rdi), %eax
	imulq	$104, %r11, %rsi
	movzbl	rld+96(%rsi), %edx
	andq	$1, %rax
	andq	$1, %rdx
	subq	%rdx, %rax
	jne	.LBB34_15
# BB#1:                                 # %if.end
	movslq	rld+16(%rsi), %r9
	movl	reg_class_size(,%r9,4), %r8d
	cmpq	$1, %r8
	movabsq	$0, %rdx
	movabsq	$1, %r14
	movq	%rdx, %rax
	jne	.LBB34_3
# BB#2:                                 # %if.end
	movq	%r14, %rax
.LBB34_3:                               # %if.end
	movslq	rld+16(%rdi), %r10
	movl	reg_class_size(,%r10,4), %ebx
	cmpq	$1, %rbx
	jne	.LBB34_5
# BB#4:                                 # %if.end
	movq	%r14, %rdx
.LBB34_5:                               # %if.end
	xorq	%rdx, %rax
	je	.LBB34_11
# BB#6:                                 # %if.then25
	cmpq	$1, %rbx
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	jne	.LBB34_8
# BB#7:                                 # %if.then25
	movq	%rdx, %rcx
.LBB34_8:                               # %if.then25
	cmpq	$1, %r8
	jne	.LBB34_10
# BB#9:                                 # %if.then25
	movq	%rdx, %rax
.LBB34_10:                              # %if.then25
	subq	%rcx, %rax
	jmp	.LBB34_15
.LBB34_11:                              # %if.end26
	movl	rld+32(%rsi), %eax
	movl	rld+32(%rdi), %edx
	subq	%rdx, %rax
	jne	.LBB34_15
# BB#12:                                # %if.end36
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r9, %rdx
	andq	%rax, %rdx
	andq	%r10, %rax
	cmpq	%rdx, %rax
	jne	.LBB34_13
# BB#14:                                # %if.end47
	subq	%r11, %rcx
	movq	%rcx, %rax
	jmp	.LBB34_15
.LBB34_13:                              # %if.then46
	subq	%r9, %r10
	movq	%r10, %rax
.LBB34_15:                              # %return
	popq	%rbx
	popq	%r14
	retq
.Ltmp482:
	.size	reload_reg_class_lower, .Ltmp482-reload_reg_class_lower
	.cfi_endproc

	.align	16, 0x90
	.type	mark_reload_reg_in_use,@function
mark_reload_reg_in_use:                 # @mark_reload_reg_in_use
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rcx, %rax
	movq	%rdx, %r8
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rdi, %rcx
	cmpq	$8, %rcx
	je	.LBB35_4
# BB#1:                                 # %entry
	leaq	-21(%rdi), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB35_4
# BB#2:                                 # %entry
	leaq	-45(%rdi), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB35_4
# BB#3:                                 # %entry
	movq	%rdi, %rdx
	addq	$-29, %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB35_4
# BB#9:                                 # %cond.false
	andq	%rax, %rcx
	cmpq	$24, %rcx
	jne	.LBB35_10
# BB#12:                                # %cond.true22
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB35_14
.LBB35_4:                               # %cond.true
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB35_14
# BB#5:                                 # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB35_6
# BB#7:                                 # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB35_14
.LBB35_10:                              # %cond.false
	cmpq	$18, %rcx
	jne	.LBB35_13
# BB#11:                                # %cond.true18
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB35_14
.LBB35_6:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB35_14
.LBB35_13:                              # %cond.false26
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movl	target_flags(%rip), %r9d
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %r9
	andq	$4, %r9
	leaq	3(%rax,%r9), %rax
	addq	$4, %r9
	cqto
	idivq	%r9
.LBB35_14:                              # %cond.end38
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	%rdi, %r11
	andq	%r9, %r11
	addq	%rdi, %rax
	andq	%r9, %rax
	cmpq	%r11, %rax
	jbe	.LBB35_32
# BB#15:                                # %for.body.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	reload_reg_used_at_all(%rip), %r10
	movq	$32, %rcx
	sarq	%cl, %rsi
	andq	%r9, %r8
	jmp	.LBB35_16
	.align	16, 0x90
.LBB35_17:                              # %for.body
                                        #   in Loop: Header=BB35_16 Depth=1
	jmpq	*.LJTI35_0(,%r8,8)
.LBB35_27:                              # %sw.bb79
                                        #   in Loop: Header=BB35_16 Depth=1
	movq	$1, %rdx
	movq	%r11, %rcx
	shlq	%cl, %rdx
	orq	%rdx, reload_reg_used_in_input(,%rsi,8)
	jmp	.LBB35_30
.LBB35_28:                              # %sw.bb85
                                        #   in Loop: Header=BB35_16 Depth=1
	movq	$1, %rdx
	movq	%r11, %rcx
	shlq	%cl, %rdx
	orq	%rdx, reload_reg_used_in_output(,%rsi,8)
	jmp	.LBB35_30
.LBB35_29:                              # %sw.bb91
                                        #   in Loop: Header=BB35_16 Depth=1
	movq	$1, %rdx
	movq	%r11, %rcx
	shlq	%cl, %rdx
	orq	%rdx, reload_reg_used_in_insn(%rip)
	jmp	.LBB35_30
.LBB35_20:                              # %sw.bb43
                                        #   in Loop: Header=BB35_16 Depth=1
	movq	$1, %rdx
	movq	%r11, %rcx
	shlq	%cl, %rdx
	orq	%rdx, reload_reg_used_in_input_addr(,%rsi,8)
	jmp	.LBB35_30
.LBB35_21:                              # %sw.bb49
                                        #   in Loop: Header=BB35_16 Depth=1
	movq	$1, %rdx
	movq	%r11, %rcx
	shlq	%cl, %rdx
	orq	%rdx, reload_reg_used_in_inpaddr_addr(,%rsi,8)
	jmp	.LBB35_30
.LBB35_22:                              # %sw.bb55
                                        #   in Loop: Header=BB35_16 Depth=1
	movq	$1, %rdx
	movq	%r11, %rcx
	shlq	%cl, %rdx
	orq	%rdx, reload_reg_used_in_output_addr(,%rsi,8)
	jmp	.LBB35_30
.LBB35_23:                              # %sw.bb61
                                        #   in Loop: Header=BB35_16 Depth=1
	movq	$1, %rdx
	movq	%r11, %rcx
	shlq	%cl, %rdx
	orq	%rdx, reload_reg_used_in_outaddr_addr(,%rsi,8)
	jmp	.LBB35_30
.LBB35_24:                              # %sw.bb67
                                        #   in Loop: Header=BB35_16 Depth=1
	movq	$1, %rdx
	movq	%r11, %rcx
	shlq	%cl, %rdx
	orq	%rdx, reload_reg_used_in_op_addr(%rip)
	jmp	.LBB35_30
.LBB35_25:                              # %sw.bb71
                                        #   in Loop: Header=BB35_16 Depth=1
	movq	$1, %rdx
	movq	%r11, %rcx
	shlq	%cl, %rdx
	orq	%rdx, reload_reg_used_in_op_addr_reload(%rip)
	jmp	.LBB35_30
.LBB35_19:                              # %sw.bb
                                        #   in Loop: Header=BB35_16 Depth=1
	movq	$1, %rdx
	movq	%r11, %rcx
	shlq	%cl, %rdx
	orq	%rdx, reload_reg_used(%rip)
	jmp	.LBB35_30
.LBB35_26:                              # %sw.bb75
                                        #   in Loop: Header=BB35_16 Depth=1
	movq	$1, %rdx
	movq	%r11, %rcx
	shlq	%cl, %rdx
	orq	%rdx, reload_reg_used_in_other_addr(%rip)
	jmp	.LBB35_30
	.align	16, 0x90
.LBB35_16:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$10, %r8
	jbe	.LBB35_17
# BB#18:                                # %for.body.sw.epilog_crit_edge
                                        #   in Loop: Header=BB35_16 Depth=1
	movq	$1, %rdx
	movq	%r11, %rcx
	shlq	%cl, %rdx
.LBB35_30:                              # %sw.epilog
                                        #   in Loop: Header=BB35_16 Depth=1
	orq	%rdx, %r10
	incq	%rdi
	movq	%rdi, %rcx
	andq	%r9, %rcx
	incq	%r11
	cmpq	%rax, %rcx
	jne	.LBB35_16
# BB#31:                                # %for.cond.for.end_crit_edge
	movq	%r10, reload_reg_used_at_all(%rip)
.LBB35_32:                              # %for.end
	retq
.Ltmp483:
	.size	mark_reload_reg_in_use, .Ltmp483-mark_reload_reg_in_use
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI35_0:
	.quad	.LBB35_27
	.quad	.LBB35_28
	.quad	.LBB35_29
	.quad	.LBB35_20
	.quad	.LBB35_21
	.quad	.LBB35_22
	.quad	.LBB35_23
	.quad	.LBB35_24
	.quad	.LBB35_25
	.quad	.LBB35_19
	.quad	.LBB35_26

	.text
	.align	16, 0x90
	.type	set_reload_reg,@function
set_reload_reg:                         # @set_reload_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp490:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp491:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp492:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp493:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp494:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp495:
	.cfi_def_cfa_offset 64
.Ltmp496:
	.cfi_offset %rbx, -48
.Ltmp497:
	.cfi_offset %r12, -40
.Ltmp498:
	.cfi_offset %r13, -32
.Ltmp499:
	.cfi_offset %r14, -24
.Ltmp500:
	.cfi_offset %r15, -16
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	$32, %rcx
	movq	%rdi, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	spill_reg_rtx(,%r14,8), %r15
	movq	%rsi, %rbx
	testq	%r15, %r15
	je	.LBB36_1
# BB#2:                                 # %lor.lhs.false
	movzbl	2(%r15), %eax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	imulq	$104, %rdx, %rcx
	movl	rld+28(%rcx), %edi
	cmpq	%rdi, %rax
	je	.LBB36_4
	jmp	.LBB36_3
.LBB36_1:                               # %entry.if.then_crit_edge
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$104, %rax, %rax
	movl	rld+28(%rax), %edi
.LBB36_3:                               # %if.then
	movswq	spill_regs(%r14,%r14), %rsi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	%r15, spill_reg_rtx(,%r14,8)
.LBB36_4:                               # %if.end
	movq	%r15, %rdi
	callq	true_regnum
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	imulq	$104, %rbx, %r12
	movl	rld+28(%r12), %esi
	movq	%rax, %r13
	movq	%r13, %rdi
	callq	ix86_hard_regno_mode_ok
	xorq	%rcx, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB36_11
# BB#5:                                 # %if.then16
	movq	rld(%r12), %rax
	testq	%rax, %rax
	je	.LBB36_8
# BB#6:                                 # %if.end27
	movzbl	2(%rax), %esi
	testq	%rsi, %rsi
	je	.LBB36_8
# BB#7:                                 # %land.lhs.true35
	movq	%r13, %rdi
	callq	ix86_hard_regno_mode_ok
	xorq	%rcx, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB36_11
.LBB36_8:                               # %if.then38
	movq	rld+8(%r12), %rax
	testq	%rax, %rax
	je	.LBB36_10
# BB#9:                                 # %land.lhs.true43
	movzbl	2(%rax), %esi
	movq	%r13, %rdi
	callq	ix86_hard_regno_mode_ok
	xorq	%rcx, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB36_11
.LBB36_10:                              # %if.then52
	leaq	rld+28(%r12), %rax
	movswq	spill_regs(%r14,%r14), %rdi
	movl	(%rax), %ecx
	movl	rld+92(%r12), %edx
	movl	rld+72(%r12), %esi
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	%eax, last_spill_reg(%rip)
	callq	mark_reload_reg_in_use
	movswq	spill_regs(%r14,%r14), %rax
	movq	%r15, rld+64(%r12)
	movl	%eax, reload_spill_index(,%rbx,4)
	movq	$1, %rcx
.LBB36_11:                              # %return
	movq	%rcx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp501:
	.size	set_reload_reg, .Ltmp501-set_reload_reg
	.cfi_endproc

	.align	16, 0x90
	.type	reload_reg_free_p,@function
reload_reg_free_p:                      # @reload_reg_free_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp503:
	.cfi_def_cfa_offset 16
	movq	reload_reg_used(%rip), %r9
	movq	$1, %r8
	movq	%rdi, %rcx
	shlq	%cl, %r8
	xorq	%rax, %rax
	testq	%r8, %r9
	jne	.LBB37_34
# BB#1:                                 # %lor.lhs.false
	movq	reload_reg_unavailable(%rip), %rcx
	testq	%r8, %rcx
	jne	.LBB37_34
# BB#2:                                 # %if.end
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$10, %rdx
	ja	.LBB37_92
# BB#3:                                 # %if.end
	jmpq	*.LJTI37_0(,%rdx,8)
.LBB37_23:                              # %sw.bb62
	movq	reload_reg_used_in_insn(%rip), %rax
	testq	%r8, %rax
	jne	.LBB37_34
# BB#24:                                # %lor.lhs.false67
	movq	reload_reg_used_in_op_addr(%rip), %rax
	testq	%r8, %rax
	jne	.LBB37_34
# BB#25:                                # %if.end73
	movq	reload_reg_used_in_op_addr_reload(%rip), %rax
	testq	%r8, %rax
	jne	.LBB37_34
# BB#26:                                # %for.cond80.preheader
	movslq	reload_n_operands(%rip), %rdx
	testq	%rdx, %rdx
	jle	.LBB37_30
# BB#27:
	movq	$1, %rax
.LBB37_28:                              # %for.body82
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_reg_used_in_input-8(,%rax,8), %rcx
	testq	%r8, %rcx
	jne	.LBB37_34
# BB#29:                                # %for.cond80
                                        #   in Loop: Header=BB37_28 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rax
	cmpq	%rdx, %rdi
	jl	.LBB37_28
.LBB37_30:                              # %for.cond94.preheader
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rsi
.LBB37_31:                              # %for.cond94
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	movq	$1, %rax
	jge	.LBB37_60
# BB#32:                                # %for.body96
                                        #   in Loop: Header=BB37_31 Depth=1
	movq	reload_reg_used_in_input_addr(,%rsi,8), %rax
	testq	%r8, %rax
	jne	.LBB37_34
# BB#33:                                # %lor.lhs.false103
                                        #   in Loop: Header=BB37_31 Depth=1
	movq	reload_reg_used_in_inpaddr_addr(,%rsi,8), %rax
	incq	%rsi
	testq	%r8, %rax
	je	.LBB37_31
	jmp	.LBB37_34
.LBB37_72:                              # %sw.bb257
	movq	reload_reg_used_in_insn(%rip), %rax
	testq	%r8, %rax
	jne	.LBB37_34
# BB#73:                                # %for.cond264.preheader
	movslq	reload_n_operands(%rip), %rdx
	testq	%rdx, %rdx
	jle	.LBB37_77
# BB#74:
	movq	$1, %rax
.LBB37_75:                              # %for.body266
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_reg_used_in_output-8(,%rax,8), %rcx
	testq	%r8, %rcx
	jne	.LBB37_34
# BB#76:                                # %for.cond264
                                        #   in Loop: Header=BB37_75 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rax
	cmpq	%rdx, %rdi
	jl	.LBB37_75
.LBB37_77:                              # %for.cond278.preheader
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$1, %rax
	cmpq	%rdi, %rdx
	jle	.LBB37_60
# BB#78:                                # %for.body280.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rsi
.LBB37_80:                              # %for.body280
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_reg_used_in_output_addr-8(,%rsi,8), %rcx
	testq	%r8, %rcx
	jne	.LBB37_34
# BB#81:                                # %lor.lhs.false287
                                        #   in Loop: Header=BB37_80 Depth=1
	movq	reload_reg_used_in_outaddr_addr-8(,%rsi,8), %rcx
	testq	%r8, %rcx
	jne	.LBB37_34
# BB#79:                                # %for.cond278
                                        #   in Loop: Header=BB37_80 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rsi
	cmpq	%rdx, %rdi
	jl	.LBB37_80
	jmp	.LBB37_60
.LBB37_4:                               # %for.cond300.preheader
	movslq	reload_n_operands(%rip), %rax
	testq	%rax, %rax
	jle	.LBB37_85
# BB#5:
	movq	$1, %rdx
.LBB37_83:                              # %for.body302
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_reg_used_in_input-8(,%rdx,8), %rcx
	testq	%r8, %rcx
	jne	.LBB37_34
# BB#84:                                # %lor.lhs.false309
                                        #   in Loop: Header=BB37_83 Depth=1
	movq	reload_reg_used_in_output-8(,%rdx,8), %rcx
	testq	%r8, %rcx
	jne	.LBB37_34
# BB#82:                                # %for.cond300
                                        #   in Loop: Header=BB37_83 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rdx
	cmpq	%rax, %rsi
	jl	.LBB37_83
.LBB37_85:                              # %for.end320
	movq	reload_reg_used_in_insn(%rip), %rcx
	xorq	%rax, %rax
	testq	%r8, %rcx
	jne	.LBB37_60
# BB#86:                                # %land.rhs325
	movq	reload_reg_used_in_op_addr(%rip), %rax
	testq	%r8, %rax
	jne	.LBB37_87
# BB#88:                                # %land.rhs325
	movabsq	$1, %rax
	popq	%rdx
	retq
.LBB37_35:                              # %sw.bb115
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	reload_reg_used_in_input_addr(,%rax,8), %rcx
	testq	%r8, %rcx
	jne	.LBB37_34
# BB#36:                                # %lor.lhs.false122
	movq	reload_reg_used_in_inpaddr_addr(,%rax,8), %rax
	testq	%r8, %rax
	jne	.LBB37_34
# BB#37:                                # %for.cond131.preheader
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rax
	testq	%rdx, %rdx
	jle	.LBB37_60
# BB#38:
	movq	$1, %rdx
.LBB37_40:                              # %for.body133
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_reg_used_in_input-8(,%rdx,8), %rcx
	testq	%r8, %rcx
	jne	.LBB37_34
# BB#39:                                # %for.cond131
                                        #   in Loop: Header=BB37_40 Depth=1
	movq	$32, %rcx
	movq	%rdx, %r9
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rdx
	cmpq	%rdi, %r9
	jl	.LBB37_40
	jmp	.LBB37_60
.LBB37_41:                              # %sw.bb145
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	reload_reg_used_in_inpaddr_addr(,%rax,8), %rax
	testq	%r8, %rax
	jne	.LBB37_34
# BB#42:                                # %for.cond154.preheader
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rax
	testq	%rdx, %rdx
	jle	.LBB37_60
# BB#43:
	movq	$1, %rdx
.LBB37_45:                              # %for.body156
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_reg_used_in_input-8(,%rdx,8), %rcx
	testq	%r8, %rcx
	jne	.LBB37_34
# BB#44:                                # %for.cond154
                                        #   in Loop: Header=BB37_45 Depth=1
	movq	$32, %rcx
	movq	%rdx, %r9
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rdx
	cmpq	%rdi, %r9
	jl	.LBB37_45
	jmp	.LBB37_60
.LBB37_46:                              # %sw.bb168
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	reload_reg_used_in_output_addr(,%rax,8), %rax
	testq	%r8, %rax
	movabsq	$0, %rax
	jne	.LBB37_60
# BB#47:                                # %for.cond177.preheader
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB37_48
# BB#51:
	xorq	%rdx, %rdx
.LBB37_52:                              # %for.body179
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_reg_used_in_output(,%rdx,8), %rcx
	xorq	%rax, %rax
	testq	%r8, %rcx
	jne	.LBB37_60
# BB#49:                                # %for.cond177
                                        #   in Loop: Header=BB37_52 Depth=1
	movq	$32, %rcx
	leaq	1(%rdx), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rdx
	movq	%rax, %rdx
	jl	.LBB37_52
# BB#50:
	movq	$1, %rax
	popq	%rdx
	retq
.LBB37_53:                              # %sw.bb191
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	reload_reg_used_in_outaddr_addr(,%rax,8), %rax
	testq	%r8, %rax
	movabsq	$0, %rax
	jne	.LBB37_60
# BB#54:                                # %for.cond200.preheader
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB37_55
# BB#58:
	xorq	%rdx, %rdx
.LBB37_59:                              # %for.body202
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_reg_used_in_output(,%rdx,8), %rcx
	xorq	%rax, %rax
	testq	%r8, %rcx
	jne	.LBB37_60
# BB#56:                                # %for.cond200
                                        #   in Loop: Header=BB37_59 Depth=1
	movq	$32, %rcx
	leaq	1(%rdx), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rdx
	movq	%rax, %rdx
	jl	.LBB37_59
# BB#57:
	movq	$1, %rax
	popq	%rdx
	retq
.LBB37_8:                               # %for.cond215.preheader
	movslq	reload_n_operands(%rip), %rax
	testq	%rax, %rax
	jle	.LBB37_63
# BB#9:
	movq	$1, %rdx
.LBB37_62:                              # %for.body217
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_reg_used_in_input-8(,%rdx,8), %rcx
	testq	%r8, %rcx
	jne	.LBB37_34
# BB#61:                                # %for.cond215
                                        #   in Loop: Header=BB37_62 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rdx
	cmpq	%rax, %rsi
	jl	.LBB37_62
.LBB37_63:                              # %for.end228
	movq	reload_reg_used_in_insn(%rip), %rcx
	xorq	%rax, %rax
	testq	%r8, %rcx
	jne	.LBB37_60
# BB#64:                                # %land.rhs
	movq	reload_reg_used_in_op_addr(%rip), %rax
	testq	%r8, %rax
	jne	.LBB37_65
# BB#66:                                # %land.rhs
	movabsq	$1, %rax
	popq	%rdx
	retq
.LBB37_6:                               # %for.cond238.preheader
	movslq	reload_n_operands(%rip), %rax
	testq	%rax, %rax
	jle	.LBB37_69
# BB#7:
	movq	$1, %rdx
.LBB37_68:                              # %for.body240
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_reg_used_in_input-8(,%rdx,8), %rcx
	testq	%r8, %rcx
	jne	.LBB37_34
# BB#67:                                # %for.cond238
                                        #   in Loop: Header=BB37_68 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rdx
	cmpq	%rax, %rsi
	jl	.LBB37_68
.LBB37_69:                              # %for.end251
	movq	reload_reg_used_in_op_addr_reload(%rip), %rax
	testq	%r8, %rax
	jne	.LBB37_70
# BB#71:                                # %for.end251
	movabsq	$1, %rax
	popq	%rdx
	retq
.LBB37_34:
	xorq	%rax, %rax
	popq	%rdx
	retq
.LBB37_10:                              # %sw.bb
	movq	reload_reg_used_in_other_addr(%rip), %rcx
	testq	%r8, %rcx
	jne	.LBB37_60
# BB#11:                                # %lor.lhs.false9
	movq	reload_reg_used_in_op_addr(%rip), %rcx
	testq	%r8, %rcx
	jne	.LBB37_60
# BB#12:                                # %lor.lhs.false14
	movq	reload_reg_used_in_insn(%rip), %rcx
	testq	%r8, %rcx
	jne	.LBB37_60
# BB#13:                                # %for.cond.preheader
	movslq	reload_n_operands(%rip), %rdx
	testq	%rdx, %rdx
	movq	$1, %rsi
	jle	.LBB37_14
.LBB37_17:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_reg_used_in_input_addr-8(,%rsi,8), %rcx
	testq	%r8, %rcx
	jne	.LBB37_60
# BB#18:                                # %lor.lhs.false25
                                        #   in Loop: Header=BB37_17 Depth=1
	movq	reload_reg_used_in_inpaddr_addr-8(,%rsi,8), %rcx
	testq	%r8, %rcx
	jne	.LBB37_60
# BB#19:                                # %lor.lhs.false32
                                        #   in Loop: Header=BB37_17 Depth=1
	movq	reload_reg_used_in_output_addr-8(,%rsi,8), %rcx
	testq	%r8, %rcx
	jne	.LBB37_60
# BB#20:                                # %lor.lhs.false39
                                        #   in Loop: Header=BB37_17 Depth=1
	movq	reload_reg_used_in_outaddr_addr-8(,%rsi,8), %rcx
	testq	%r8, %rcx
	jne	.LBB37_60
# BB#21:                                # %lor.lhs.false46
                                        #   in Loop: Header=BB37_17 Depth=1
	movq	reload_reg_used_in_input-8(,%rsi,8), %rcx
	testq	%r8, %rcx
	jne	.LBB37_60
# BB#22:                                # %lor.lhs.false53
                                        #   in Loop: Header=BB37_17 Depth=1
	movq	reload_reg_used_in_output-8(,%rsi,8), %rcx
	testq	%r8, %rcx
	jne	.LBB37_60
# BB#15:                                # %for.cond
                                        #   in Loop: Header=BB37_17 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rsi
	cmpq	%rdx, %rdi
	jl	.LBB37_17
# BB#16:
	movq	$1, %rax
	popq	%rdx
	retq
.LBB37_60:                              # %return
	popq	%rdx
	retq
.LBB37_89:                              # %sw.bb334
	movq	reload_reg_used_in_other_addr(%rip), %rax
	testq	%r8, %rax
	jne	.LBB37_90
# BB#91:                                # %sw.bb334
	movabsq	$1, %rax
	popq	%rdx
	retq
.LBB37_92:                              # %sw.epilog
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.reload_reg_free_p, %rdx
	movq	$4527, %rsi             # imm = 0x11AF
	callq	fancy_abort
.LBB37_90:
	movabsq	$0, %rax
	popq	%rdx
	retq
.LBB37_70:
	movabsq	$0, %rax
	popq	%rdx
	retq
.LBB37_48:
	movq	$1, %rax
	popq	%rdx
	retq
.LBB37_55:
	movq	$1, %rax
	popq	%rdx
	retq
.LBB37_65:
	movabsq	$0, %rax
	popq	%rdx
	retq
.LBB37_87:
	movabsq	$0, %rax
	popq	%rdx
	retq
.LBB37_14:
	movq	$1, %rax
	popq	%rdx
	retq
.Ltmp504:
	.size	reload_reg_free_p, .Ltmp504-reload_reg_free_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI37_0:
	.quad	.LBB37_23
	.quad	.LBB37_72
	.quad	.LBB37_4
	.quad	.LBB37_35
	.quad	.LBB37_41
	.quad	.LBB37_46
	.quad	.LBB37_53
	.quad	.LBB37_8
	.quad	.LBB37_6
	.quad	.LBB37_10
	.quad	.LBB37_89

	.text
	.align	16, 0x90
	.type	elimination_effects,@function
elimination_effects:                    # @elimination_effects
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp511:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp512:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp513:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp514:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp515:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp516:
	.cfi_def_cfa_offset 64
.Ltmp517:
	.cfi_offset %rbx, -48
.Ltmp518:
	.cfi_offset %r12, -40
.Ltmp519:
	.cfi_offset %r13, -32
.Ltmp520:
	.cfi_offset %r14, -24
.Ltmp521:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	xorq	%rbx, %rbx
	jmp	.LBB38_1
.LBB38_67:                              # %if.end381
                                        #   in Loop: Header=BB38_1 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %r12
	movq	%rdx, %rdi
	.align	16, 0x90
.LBB38_1:                               # %tailrecurse.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB38_2 Depth 2
                                        #       Child Loop BB38_19 Depth 3
                                        #       Child Loop BB38_50 Depth 3
                                        #       Child Loop BB38_46 Depth 3
                                        #     Child Loop BB38_56 Depth 2
	movq	$32, %rcx
	movq	%r12, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	jmp	.LBB38_2
.LBB38_40:                              # %sw.bb206
                                        #   in Loop: Header=BB38_2 Depth=2
	addq	$8, %rdi
	movq	(%rdi), %rdi
	.align	16, 0x90
.LBB38_2:                               # %tailrecurse
                                        #   Parent Loop BB38_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB38_19 Depth 3
                                        #       Child Loop BB38_50 Depth 3
                                        #       Child Loop BB38_46 Depth 3
	movl	(%rdi), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	leaq	-36(%rdx), %rcx
	andq	%r15, %rcx
	cmpq	$95, %rcx
	ja	.LBB38_68
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB38_2 Depth=2
	jmpq	*.LJTI38_0(,%rcx,8)
.LBB38_18:                              # %for.body33.lr.ph
                                        #   in Loop: Header=BB38_2 Depth=2
	movq	reg_eliminate(%rip), %r9
	movq	8(%rdi), %r11
	movq	%rdi, %r14
	movq	%rax, %r8
	andq	$65533, %r8             # imm = 0xFFFD
	andq	$65534, %rax            # imm = 0xFFFE
	movq	%r9, %rsi
.LBB38_19:                              # %for.body33
                                        #   Parent Loop BB38_1 Depth=1
                                        #     Parent Loop BB38_2 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	40(%rsi), %rcx
	cmpq	%r11, %rcx
	jne	.LBB38_38
# BB#20:                                # %if.then38
                                        #   in Loop: Header=BB38_19 Depth=3
	movq	global_rtl+16(%rip), %rdi
	movzbl	mode_size(%r13), %ecx
	cmpq	%rdi, %r11
	jne	.LBB38_24
# BB#21:                                # %if.then45
                                        #   in Loop: Header=BB38_19 Depth=3
	movzbl	target_flags+3(%rip), %edi
	testq	$2, %rdi
	jne	.LBB38_22
# BB#23:                                # %cond.false
                                        #   in Loop: Header=BB38_19 Depth=3
	incq	%rcx
	andq	$510, %rcx              # imm = 0x1FE
	jmp	.LBB38_24
.LBB38_22:                              # %cond.true
                                        #   in Loop: Header=BB38_19 Depth=3
	addq	$7, %rcx
	andq	$504, %rcx              # imm = 0x1F8
.LBB38_24:                              # %if.end50
                                        #   in Loop: Header=BB38_19 Depth=3
	cmpq	$97, %r8
	je	.LBB38_36
# BB#25:                                # %if.end50
                                        #   in Loop: Header=BB38_19 Depth=3
	cmpq	$96, %r8
	jne	.LBB38_27
# BB#26:                                # %if.then55
                                        #   in Loop: Header=BB38_19 Depth=3
	movl	20(%rsi), %edi
	addq	%rcx, %rdi
	jmp	.LBB38_37
.LBB38_27:                              # %if.else65
                                        #   in Loop: Header=BB38_19 Depth=3
	cmpq	$100, %rax
	jne	.LBB38_38
# BB#28:                                # %land.lhs.true71
                                        #   in Loop: Header=BB38_19 Depth=3
	movq	16(%r14), %rcx
	movzbl	(%rcx), %edi
	cmpq	$75, %rdi
	jne	.LBB38_38
# BB#29:                                # %land.lhs.true79
                                        #   in Loop: Header=BB38_19 Depth=3
	movq	8(%rcx), %rdi
	cmpq	%rdi, %r11
	jne	.LBB38_38
# BB#30:                                # %land.lhs.true91
                                        #   in Loop: Header=BB38_19 Depth=3
	movq	16(%rcx), %r10
	movzbl	(%r10), %ecx
	cmpq	$133, %rcx
	jg	.LBB38_33
# BB#31:                                # %land.lhs.true91
                                        #   in Loop: Header=BB38_19 Depth=3
	addq	$-54, %rcx
	andq	%r15, %rcx
	cmpq	$14, %rcx
	ja	.LBB38_38
# BB#32:                                # %land.lhs.true91
                                        #   in Loop: Header=BB38_19 Depth=3
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	$24599, %rdi            # imm = 0x6017
	jne	.LBB38_35
	jmp	.LBB38_38
.LBB38_33:                              # %land.lhs.true91
                                        #   in Loop: Header=BB38_19 Depth=3
	cmpq	$134, %rcx
	je	.LBB38_35
# BB#34:                                # %land.lhs.true91
                                        #   in Loop: Header=BB38_19 Depth=3
	cmpq	$140, %rcx
	jne	.LBB38_38
.LBB38_35:                              # %if.then179
                                        #   in Loop: Header=BB38_19 Depth=3
	movq	8(%r10), %rcx
.LBB38_36:                              # %for.inc196
                                        #   in Loop: Header=BB38_19 Depth=3
	movl	20(%rsi), %edi
	subq	%rcx, %rdi
.LBB38_37:                              # %for.inc196
                                        #   in Loop: Header=BB38_19 Depth=3
	movl	%edi, 20(%rsi)
.LBB38_38:                              # %for.inc196
                                        #   in Loop: Header=BB38_19 Depth=3
	addq	$48, %rsi
	leaq	192(%r9), %rcx
	cmpq	%rcx, %rsi
	jb	.LBB38_19
# BB#39:                                # %for.end198
                                        #   in Loop: Header=BB38_2 Depth=2
	cmpq	$100, %rax
	movq	%r14, %rdi
	jne	.LBB38_40
	jmp	.LBB38_68
.LBB38_45:                              # %for.body259.lr.ph
                                        #   in Loop: Header=BB38_2 Depth=2
	movq	reg_eliminate(%rip), %rax
	movq	8(%rdi), %rcx
	addq	$8, %rdi
	leaq	192(%rax), %rdx
.LBB38_46:                              # %for.body259
                                        #   Parent Loop BB38_1 Depth=1
                                        #     Parent Loop BB38_2 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	32(%rax), %rsi
	cmpq	%rcx, %rsi
	jne	.LBB38_48
# BB#47:                                # %if.then266
                                        #   in Loop: Header=BB38_46 Depth=3
	movl	%ebx, 12(%rax)
.LBB38_48:                              # %for.inc269
                                        #   in Loop: Header=BB38_46 Depth=3
	addq	$48, %rax
	cmpq	%rdx, %rax
	jb	.LBB38_46
	jmp	.LBB38_53
.LBB38_49:                              # %for.body280.lr.ph
                                        #   in Loop: Header=BB38_2 Depth=2
	movq	reg_eliminate(%rip), %rax
	movq	8(%rdi), %rcx
	addq	$8, %rdi
	leaq	192(%rax), %rdx
.LBB38_50:                              # %for.body280
                                        #   Parent Loop BB38_1 Depth=1
                                        #     Parent Loop BB38_2 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	40(%rax), %rsi
	cmpq	%rcx, %rsi
	jne	.LBB38_52
# BB#51:                                # %if.then287
                                        #   in Loop: Header=BB38_50 Depth=3
	movl	%ebx, 12(%rax)
.LBB38_52:                              # %for.inc290
                                        #   in Loop: Header=BB38_50 Depth=3
	addq	$48, %rax
	cmpq	%rdx, %rax
	jb	.LBB38_50
	jmp	.LBB38_53
.LBB38_6:                               # %sw.bb2
                                        #   in Loop: Header=BB38_2 Depth=2
	movslq	8(%rdi), %r14
	movq	%rdi, %rdx
	cmpq	$52, %r14
	jle	.LBB38_7
# BB#13:                                # %if.else
                                        #   in Loop: Header=BB38_2 Depth=2
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%r14,2), %rax
	testq	%rax, %rax
	jns	.LBB38_75
# BB#14:                                # %if.else
                                        #   in Loop: Header=BB38_2 Depth=2
	movq	reg_equiv_constant(%rip), %rax
	testq	%rax, %rax
	je	.LBB38_75
# BB#15:                                # %land.lhs.true15
                                        #   in Loop: Header=BB38_2 Depth=2
	movq	(%rax,%r14,8), %rax
	testq	%rax, %rax
	je	.LBB38_75
# BB#16:                                # %land.lhs.true19
                                        #   in Loop: Header=BB38_2 Depth=2
	movq	%rax, %rdi
	callq	function_invariant_p
	testq	%r15, %rax
	jne	.LBB38_75
# BB#17:                                # %if.then23
                                        #   in Loop: Header=BB38_2 Depth=2
	movq	reg_equiv_constant(%rip), %rax
	leaq	(%rax,%r14,8), %rdi
	movq	(%rdi), %rdi
	jmp	.LBB38_2
.LBB38_41:                              # %sw.bb210
                                        #   in Loop: Header=BB38_2 Depth=2
	movq	8(%rdi), %rdx
	movl	(%rdx), %esi
	addq	$8, %rdi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB38_53
# BB#42:                                # %land.lhs.true218
                                        #   in Loop: Header=BB38_2 Depth=2
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %eax
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movzbl	mode_size(%rsi), %ecx
	cmpq	%rcx, %rax
	ja	.LBB38_53
# BB#43:                                # %land.lhs.true218
                                        #   in Loop: Header=BB38_2 Depth=2
	movq	reg_equiv_memory_loc(%rip), %rax
	testq	%rax, %rax
	je	.LBB38_53
# BB#44:                                # %land.lhs.true238
                                        #   in Loop: Header=BB38_2 Depth=2
	movl	8(%rdx), %ecx
	cmpq	$0, (%rax,%rcx,8)
	jne	.LBB38_75
.LBB38_53:                              # %tailrecurse.backedge
                                        #   in Loop: Header=BB38_2 Depth=2
	movq	(%rdi), %rdi
	jmp	.LBB38_2
.LBB38_54:                              # %sw.bb296
                                        #   in Loop: Header=BB38_1 Depth=1
	movq	8(%rdi), %rax
	movq	%rdi, %r14
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB38_64
# BB#55:                                # %for.body309.lr.ph
                                        #   in Loop: Header=BB38_1 Depth=1
	movq	reg_eliminate(%rip), %rcx
	movq	%rcx, %rdx
.LBB38_56:                              # %for.body309
                                        #   Parent Loop BB38_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	40(%rdx), %rsi
	cmpq	%rax, %rsi
	jne	.LBB38_63
# BB#57:                                # %for.body309
                                        #   in Loop: Header=BB38_56 Depth=2
	movq	global_rtl+32(%rip), %rsi
	cmpq	%rsi, %rax
	je	.LBB38_63
# BB#58:                                # %if.then322
                                        #   in Loop: Header=BB38_56 Depth=2
	movq	16(%r14), %rsi
	movzbl	(%rsi), %edi
	cmpq	$75, %rdi
	jne	.LBB38_62
# BB#59:                                # %land.lhs.true330
                                        #   in Loop: Header=BB38_56 Depth=2
	movq	8(%rsi), %rdi
	cmpq	%rax, %rdi
	jne	.LBB38_62
# BB#60:                                # %land.lhs.true339
                                        #   in Loop: Header=BB38_56 Depth=2
	movq	16(%rsi), %rsi
	movzbl	(%rsi), %edi
	cmpq	$54, %rdi
	jne	.LBB38_62
# BB#61:                                # %if.then347
                                        #   in Loop: Header=BB38_56 Depth=2
	movq	8(%rsi), %rsi
	movl	20(%rdx), %edi
	subq	%rsi, %rdi
	movl	%edi, 20(%rdx)
	jmp	.LBB38_63
.LBB38_62:                              # %if.else358
                                        #   in Loop: Header=BB38_56 Depth=2
	movl	%ebx, 12(%rdx)
.LBB38_63:                              # %for.inc362
                                        #   in Loop: Header=BB38_56 Depth=2
	addq	$48, %rdx
	leaq	192(%rcx), %rsi
	cmpq	%rsi, %rdx
	jb	.LBB38_56
.LBB38_64:                              # %if.end365
                                        #   in Loop: Header=BB38_1 Depth=1
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	%rax, %rdi
	callq	elimination_effects
	movq	%r14, %rdi
	movq	16(%rdi), %rdi
	jmp	.LBB38_1
.LBB38_65:                              # %sw.bb372
                                        #   in Loop: Header=BB38_1 Depth=1
	movq	8(%rdi), %rdi
	movzbl	(%rdi), %ecx
	movq	%rdi, %rdx
	cmpq	$70, %rcx
	jne	.LBB38_67
# BB#66:                                # %if.then380
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.elimination_effects, %rcx
	movq	$2839, %rsi             # imm = 0xB17
	jmp	.LBB38_5
.LBB38_68:                              # %sw.epilog
	movzbl	rtx_length(%rdx), %eax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	testq	%rax, %rax
	je	.LBB38_75
# BB#69:                                # %for.body396.lr.ph
	movq	rtx_format(,%rdx,8), %r13
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB38_70:                              # %for.body396
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB38_73 Depth 2
	movzbl	(%r13), %eax
	cmpq	$101, %rax
	jne	.LBB38_71
# BB#76:                                # %if.then400
                                        #   in Loop: Header=BB38_70 Depth=1
	movq	%rdi, %r14
	movq	8(%r14,%rbx,8), %rdi
	movq	%r12, %rsi
	callq	elimination_effects
	movq	%r14, %rdi
	jmp	.LBB38_74
	.align	16, 0x90
.LBB38_71:                              # %for.body396
                                        #   in Loop: Header=BB38_70 Depth=1
	cmpq	$69, %rax
	jne	.LBB38_74
# BB#72:                                # %for.cond410.preheader
                                        #   in Loop: Header=BB38_70 Depth=1
	movq	8(%rdi,%rbx,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r14
	jle	.LBB38_74
	.align	16, 0x90
.LBB38_73:                              # %for.body416
                                        #   Parent Loop BB38_70 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rdi, %r15
	movq	(%rax,%r14,8), %rdi
	movq	%r12, %rsi
	callq	elimination_effects
	movq	%r15, %rdi
	movq	8(%rdi,%rbx,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r14
	cmpq	%rdx, %rsi
	jl	.LBB38_73
	.align	16, 0x90
.LBB38_74:                              # %for.inc427
                                        #   in Loop: Header=BB38_70 Depth=1
	incq	%rbx
	incq	%r13
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	%rcx, %rax
	jl	.LBB38_70
	jmp	.LBB38_75
.LBB38_4:                               # %sw.bb1
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.elimination_effects, %rcx
	movq	$2698, %rsi             # imm = 0xA8A
.LBB38_5:                               # %sw.bb1
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB38_7:                               # %for.body.lr.ph
	movq	reg_eliminate(%rip), %rax
	leaq	192(%rax), %rcx
	movq	%rdx, %rsi
.LBB38_8:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax), %rdx
	cmpq	%rsi, %rdx
	jne	.LBB38_12
# BB#9:                                 # %land.lhs.true
                                        #   in Loop: Header=BB38_8 Depth=1
	movl	12(%rax), %edx
	testq	%rdx, %rdx
	jne	.LBB38_10
.LBB38_12:                              # %for.inc
                                        #   in Loop: Header=BB38_8 Depth=1
	addq	$48, %rax
	cmpq	%rcx, %rax
	jb	.LBB38_8
	jmp	.LBB38_75
.LBB38_10:                              # %if.then6
	testq	%r15, %r12
	jne	.LBB38_75
# BB#11:                                # %if.then8
	movq	$1, %rcx
	movl	%ecx, 28(%rax)
.LBB38_75:                              # %for.end430
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp522:
	.size	elimination_effects, .Ltmp522-elimination_effects
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI38_0:
	.quad	.LBB38_75
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_75
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_75
	.quad	.LBB38_75
	.quad	.LBB38_68
	.quad	.LBB38_54
	.quad	.LBB38_45
	.quad	.LBB38_49
	.quad	.LBB38_68
	.quad	.LBB38_75
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_75
	.quad	.LBB38_75
	.quad	.LBB38_75
	.quad	.LBB38_68
	.quad	.LBB38_75
	.quad	.LBB38_75
	.quad	.LBB38_68
	.quad	.LBB38_6
	.quad	.LBB38_68
	.quad	.LBB38_41
	.quad	.LBB38_40
	.quad	.LBB38_68
	.quad	.LBB38_65
	.quad	.LBB38_68
	.quad	.LBB38_75
	.quad	.LBB38_75
	.quad	.LBB38_4
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_40
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_40
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_18
	.quad	.LBB38_18
	.quad	.LBB38_18
	.quad	.LBB38_18
	.quad	.LBB38_18
	.quad	.LBB38_18
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_68
	.quad	.LBB38_40
	.quad	.LBB38_40
	.quad	.LBB38_40
	.quad	.LBB38_40
	.quad	.LBB38_40
	.quad	.LBB38_40
	.quad	.LBB38_40
	.quad	.LBB38_40
	.quad	.LBB38_40
	.quad	.LBB38_40
	.quad	.LBB38_40
	.quad	.LBB38_40

	.text
	.align	16, 0x90
	.type	check_eliminable_occurrences,@function
check_eliminable_occurrences:           # @check_eliminable_occurrences
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp529:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp530:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp531:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp532:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp533:
	.cfi_def_cfa_offset 48
.Ltmp534:
	.cfi_offset %rbx, -48
.Ltmp535:
	.cfi_offset %r12, -40
.Ltmp536:
	.cfi_offset %r13, -32
.Ltmp537:
	.cfi_offset %r14, -24
.Ltmp538:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	testq	%r15, %r15
	je	.LBB39_15
# BB#1:                                 # %if.end
	movzbl	(%r15), %eax
	cmpq	$61, %rax
	jne	.LBB39_8
# BB#2:                                 # %land.lhs.true
	movl	8(%r15), %ecx
	cmpq	$52, %rcx
	ja	.LBB39_8
# BB#3:                                 # %for.body.lr.ph
	movq	reg_eliminate(%rip), %rax
	xorq	%rcx, %rcx
	leaq	192(%rax), %rdx
	.align	16, 0x90
.LBB39_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax), %rsi
	cmpq	%r15, %rsi
	jne	.LBB39_7
# BB#5:                                 # %land.lhs.true7
                                        #   in Loop: Header=BB39_4 Depth=1
	movl	12(%rax), %esi
	testq	%rsi, %rsi
	je	.LBB39_7
# BB#6:                                 # %if.then8
                                        #   in Loop: Header=BB39_4 Depth=1
	movl	%ecx, 12(%rax)
.LBB39_7:                               # %for.inc
                                        #   in Loop: Header=BB39_4 Depth=1
	addq	$48, %rax
	cmpq	%rdx, %rax
	jb	.LBB39_4
	jmp	.LBB39_15
.LBB39_8:                               # %if.end11
	movzbl	rtx_length(%rax), %r14d
	testq	%r14, %r14
	je	.LBB39_15
# BB#9:                                 # %for.body18.lr.ph
	movq	rtx_format(,%rax,8), %r12
	xorq	%r13, %r13
	.align	16, 0x90
.LBB39_10:                              # %for.body18
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB39_13 Depth 2
	movzbl	(%r12), %eax
	cmpq	$101, %rax
	jne	.LBB39_11
# BB#16:                                # %if.then22
                                        #   in Loop: Header=BB39_10 Depth=1
	movq	8(%r15,%r13,8), %rdi
	callq	check_eliminable_occurrences
	jmp	.LBB39_14
	.align	16, 0x90
.LBB39_11:                              # %for.body18
                                        #   in Loop: Header=BB39_10 Depth=1
	cmpq	$69, %rax
	jne	.LBB39_14
# BB#12:                                # %for.cond30.preheader
                                        #   in Loop: Header=BB39_10 Depth=1
	movq	8(%r15,%r13,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %rbx
	jle	.LBB39_14
	.align	16, 0x90
.LBB39_13:                              # %for.body36
                                        #   Parent Loop BB39_10 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%rbx,8), %rdi
	callq	check_eliminable_occurrences
	movq	8(%r15,%r13,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB39_13
	.align	16, 0x90
.LBB39_14:                              # %for.inc47
                                        #   in Loop: Header=BB39_10 Depth=1
	incq	%r13
	incq	%r12
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r14, %rax
	jl	.LBB39_10
.LBB39_15:                              # %for.end50
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp539:
	.size	check_eliminable_occurrences, .Ltmp539-check_eliminable_occurrences
	.cfi_endproc

	.align	16, 0x90
	.type	set_label_offsets,@function
set_label_offsets:                      # @set_label_offsets
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp546:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp547:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp548:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp549:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp550:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp551:
	.cfi_def_cfa_offset 64
.Ltmp552:
	.cfi_offset %rbx, -48
.Ltmp553:
	.cfi_offset %r12, -40
.Ltmp554:
	.cfi_offset %r13, -32
.Ltmp555:
	.cfi_offset %r14, -24
.Ltmp556:
	.cfi_offset %r15, -16
	movq	%rdx, %r13
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	jmp	.LBB40_1
	.align	16, 0x90
.LBB40_44:                              # %tailrecurse.backedge
                                        #   in Loop: Header=BB40_1 Depth=1
	movq	8(%rax), %rbx
.LBB40_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rbx), %ecx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jg	.LBB40_10
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB40_1 Depth=1
	leaq	-32(%rax), %rcx
	andq	%r8, %rcx
	cmpq	$15, %rcx
	ja	.LBB40_56
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB40_1 Depth=1
	jmpq	*.LJTI40_0(,%rcx,8)
.LBB40_38:                              # %sw.bb119
                                        #   in Loop: Header=BB40_1 Depth=1
	movq	8(%rbx), %rax
	movq	global_rtl(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB40_56
# BB#39:                                # %if.end126
                                        #   in Loop: Header=BB40_1 Depth=1
	movq	16(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$58, %rcx
	jle	.LBB40_40
# BB#41:                                # %if.end126
                                        #   in Loop: Header=BB40_1 Depth=1
	cmpq	$59, %rcx
	je	.LBB40_56
# BB#42:                                # %if.end126
                                        #   in Loop: Header=BB40_1 Depth=1
	cmpq	$72, %rcx
	jne	.LBB40_43
# BB#45:                                # %sw.bb140
                                        #   in Loop: Header=BB40_1 Depth=1
	movq	16(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$51, %rdx
	je	.LBB40_49
# BB#46:                                # %sw.bb140
                                        #   in Loop: Header=BB40_1 Depth=1
	cmpq	$59, %rdx
	je	.LBB40_49
# BB#47:                                # %sw.bb140
                                        #   in Loop: Header=BB40_1 Depth=1
	cmpq	$67, %rdx
	jne	.LBB40_52
# BB#48:                                # %if.then151
                                        #   in Loop: Header=BB40_1 Depth=1
	movq	8(%rcx), %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	movq	%r8, %r12
	callq	set_label_offsets
	movq	%r12, %r8
	movq	16(%rbx), %rax
.LBB40_49:                              # %if.end167
                                        #   in Loop: Header=BB40_1 Depth=1
	movq	24(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$67, %rcx
	je	.LBB40_44
	jmp	.LBB40_50
.LBB40_43:                              # %if.end126
                                        #   in Loop: Header=BB40_1 Depth=1
	cmpq	$67, %rcx
	je	.LBB40_44
	jmp	.LBB40_52
.LBB40_10:                              # %tailrecurse
	cmpq	$67, %rax
	jne	.LBB40_56
# BB#11:                                # %sw.bb
	testq	$134217728, %rcx        # imm = 0x8000000
	jne	.LBB40_56
# BB#12:                                # %if.end
	movq	8(%rbx), %rbx
.LBB40_13:                              # %sw.bb3
	movslq	48(%rbx), %rdi
	movq	offsets_known_at(%rip), %rax
	movzbl	(%rax,%rdi), %ecx
	testq	%rcx, %rcx
	je	.LBB40_14
# BB#16:                                # %if.else
	cmpq	%r15, %rbx
	jne	.LBB40_17
# BB#24:                                # %land.lhs.true
	movq	%r15, %rdi
	movq	%r8, %r12
	callq	prev_nonnote_insn
	movq	%r12, %r8
	testq	%rax, %rax
	je	.LBB40_17
# BB#25:                                # %land.lhs.true31
	movzbl	(%rax), %eax
	cmpq	$35, %rax
	jne	.LBB40_17
# BB#26:                                # %if.then36
	movq	%r15, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	set_offsets_for_label  # TAILCALL
.LBB40_40:                              # %if.end126
	cmpq	$51, %rcx
	jne	.LBB40_52
	jmp	.LBB40_56
.LBB40_17:                              # %for.cond38.preheader
	movslq	48(%rbx), %rsi
	movq	offsets_at(%rip), %rdx
	movq	$4, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	reg_eliminate(%rip), %rax
	movl	(%rdx,%rdi), %ecx
	testq	%r8, %r13
	je	.LBB40_18
# BB#19:                                # %for.cond38.preheader
	leaq	8(%rax), %rdi
	jmp	.LBB40_20
.LBB40_14:                              # %for.cond.preheader
	movq	reg_eliminate(%rip), %rdx
	testq	%r8, %r13
	je	.LBB40_57
# BB#15:                                # %cond.true.3
	movl	8(%rdx), %r8d
	movq	offsets_at(%rip), %rsi
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	%r8d, (%rsi,%rdi)
	movslq	48(%rbx), %rdi
	movl	56(%rdx), %r8d
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	%r8d, 4(%rsi,%rdi)
	movslq	48(%rbx), %rdi
	movl	104(%rdx), %r8d
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	%r8d, 8(%rsi,%rdi)
	addq	$152, %rdx
	jmp	.LBB40_58
.LBB40_18:
	leaq	20(%rax), %rdi
.LBB40_20:                              # %for.cond38.preheader
	movl	(%rdi), %edi
	cmpq	%rdi, %rcx
	je	.LBB40_22
# BB#21:                                # %if.then63
	xorq	%rcx, %rcx
	movl	%ecx, 12(%rax)
	movl	48(%rbx), %esi
.LBB40_22:                              # %for.inc67
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	4(%rdx,%rdi), %ecx
	testq	%r8, %r13
	je	.LBB40_23
# BB#27:                                # %for.inc67
	leaq	56(%rax), %rdi
	jmp	.LBB40_28
.LBB40_23:
	leaq	68(%rax), %rdi
.LBB40_28:                              # %for.inc67
	movl	(%rdi), %edi
	cmpq	%rdi, %rcx
	je	.LBB40_30
# BB#29:                                # %if.then63.1
	xorq	%rcx, %rcx
	movl	%ecx, 60(%rax)
	movl	48(%rbx), %esi
.LBB40_30:                              # %for.inc67.1
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	8(%rdx,%rdi), %ecx
	testq	%r8, %r13
	je	.LBB40_31
# BB#59:                                # %for.inc67.1
	leaq	104(%rax), %rdi
	jmp	.LBB40_60
.LBB40_31:
	leaq	116(%rax), %rdi
.LBB40_60:                              # %for.inc67.1
	movl	(%rdi), %edi
	cmpq	%rdi, %rcx
	je	.LBB40_62
# BB#61:                                # %if.then63.2
	xorq	%rcx, %rcx
	movl	%ecx, 108(%rax)
	movl	48(%rbx), %esi
.LBB40_62:                              # %for.inc67.2
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movl	12(%rdx,%rsi), %ecx
	testq	%r8, %r13
	je	.LBB40_63
# BB#64:                                # %for.inc67.2
	leaq	152(%rax), %rdx
	jmp	.LBB40_65
.LBB40_63:
	leaq	164(%rax), %rdx
.LBB40_65:                              # %for.inc67.2
	movl	(%rdx), %edx
	cmpq	%rdx, %rcx
	je	.LBB40_56
# BB#66:                                # %if.then63.3
	xorq	%rcx, %rcx
	movl	%ecx, 156(%rax)
	jmp	.LBB40_56
.LBB40_57:                              # %cond.false.3
	movl	20(%rdx), %r8d
	movq	offsets_at(%rip), %rsi
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	%r8d, (%rsi,%rdi)
	movslq	48(%rbx), %rdi
	movl	68(%rdx), %r8d
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	%r8d, 4(%rsi,%rdi)
	movslq	48(%rbx), %rdi
	movl	116(%rdx), %r8d
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	%r8d, 8(%rsi,%rdi)
	addq	$164, %rdx
.LBB40_58:                              # %cond.end.3
	movslq	48(%rbx), %rdi
	movl	(%rdx), %edx
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	%edx, 12(%rsi,%rdi)
	movslq	48(%rbx), %rcx
	movq	$1, %rdx
	movb	%dl, (%rax,%rcx)
	jmp	.LBB40_56
.LBB40_4:                               # %for.cond99.preheader
	cmpq	$45, %rax
	jne	.LBB40_5
# BB#6:                                 # %for.cond99.preheader
	movabsq	$1, %rsi
	jmp	.LBB40_7
.LBB40_5:
	movabsq	$0, %rsi
.LBB40_7:                               # %for.cond99.preheader
	movq	8(%rbx,%rsi,8), %rax
	movl	(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB40_56
# BB#8:
	xorq	%r12, %r12
	.align	16, 0x90
.LBB40_9:                               # %for.body107
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	%r12, %rcx
	andq	%r8, %rcx
	movq	8(%rax,%rcx,8), %rdi
	movq	%rsi, %r14
	movq	%r15, %rsi
	movq	%r13, %rdx
	movq	%r15, %rbx
	movq	%r13, %r15
	movq	%r8, %r13
	callq	set_label_offsets
	movq	%r14, %rsi
	movq	%r13, %r8
	movq	%r15, %r13
	movq	%rbx, %r15
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	8(%rbx,%rsi,8), %rax
	movl	(%rax), %ecx
	incq	%r12
	movq	%r12, %rdx
	andq	%r8, %rdx
	cmpq	%rcx, %rdx
	jb	.LBB40_9
	jmp	.LBB40_56
.LBB40_32:                              # %sw.bb72
	movq	32(%r15), %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	set_label_offsets
.LBB40_33:                              # %sw.bb76
	movq	56(%rbx), %rbx
	jmp	.LBB40_34
.LBB40_37:                              # %for.inc93
                                        #   in Loop: Header=BB40_34 Depth=1
	movq	16(%rbx), %rbx
.LBB40_34:                              # %sw.bb76
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB40_56
# BB#35:                                # %for.body82
                                        #   in Loop: Header=BB40_34 Depth=1
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$851968, %rax           # imm = 0xD0000
	jne	.LBB40_37
# BB#36:                                # %if.then88
                                        #   in Loop: Header=BB40_34 Depth=1
	movq	8(%rbx), %rdi
	movq	$1, %rdx
	movq	%r15, %rsi
	callq	set_label_offsets
	jmp	.LBB40_37
.LBB40_50:                              # %if.end167
	cmpq	$51, %rcx
	je	.LBB40_56
# BB#51:                                # %if.end167
	cmpq	$59, %rcx
	je	.LBB40_56
.LBB40_52:                              # %for.body199.lr.ph
	movq	reg_eliminate(%rip), %rax
	xorq	%rcx, %rcx
	leaq	192(%rax), %rdx
	.align	16, 0x90
.LBB40_53:                              # %for.body199
                                        # =>This Inner Loop Header: Depth=1
	movl	20(%rax), %esi
	movl	8(%rax), %edi
	cmpq	%rdi, %rsi
	je	.LBB40_55
# BB#54:                                # %if.then204
                                        #   in Loop: Header=BB40_53 Depth=1
	movl	%ecx, 12(%rax)
.LBB40_55:                              # %for.inc207
                                        #   in Loop: Header=BB40_53 Depth=1
	addq	$48, %rax
	cmpq	%rdx, %rax
	jb	.LBB40_53
.LBB40_56:                              # %sw.epilog210
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp557:
	.size	set_label_offsets, .Ltmp557-set_label_offsets
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI40_0:
	.quad	.LBB40_33
	.quad	.LBB40_32
	.quad	.LBB40_33
	.quad	.LBB40_56
	.quad	.LBB40_13
	.quad	.LBB40_56
	.quad	.LBB40_56
	.quad	.LBB40_4
	.quad	.LBB40_56
	.quad	.LBB40_56
	.quad	.LBB40_56
	.quad	.LBB40_56
	.quad	.LBB40_4
	.quad	.LBB40_4
	.quad	.LBB40_56
	.quad	.LBB40_38

	.text
	.align	16, 0x90
	.type	count_spilled_pseudo,@function
count_spilled_pseudo:                   # @count_spilled_pseudo
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp564:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp565:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp566:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp567:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp568:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp569:
	.cfi_def_cfa_offset 64
.Ltmp570:
	.cfi_offset %rbx, -48
.Ltmp571:
	.cfi_offset %r12, -40
.Ltmp572:
	.cfi_offset %r13, -32
.Ltmp573:
	.cfi_offset %r14, -24
.Ltmp574:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	24(%rax), %rax
	movq	$32, %rcx
	movq	%rdx, %r14
	shlq	%cl, %r14
	movq	reg_renumber(%rip), %rsi
	movq	80(%rax), %rax
	movq	$32, %rcx
	sarq	%cl, %r14
	movzbl	(%rsi,%r14,2), %esi
	movq	(%rax,%r14,8), %rax
	movq	$48, %rcx
	movq	%rsi, %r13
	shlq	%cl, %r13
	movq	$48, %rcx
	movzbl	2(%rax), %eax
	sarq	%cl, %r13
	movq	%rsi, %rcx
	andq	$65528, %rcx            # imm = 0xFFF8
	cmpq	$8, %rcx
	je	.LBB41_4
# BB#1:                                 # %entry
	leaq	-21(%rsi), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB41_4
# BB#2:                                 # %entry
	leaq	-45(%rsi), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB41_4
# BB#3:                                 # %entry
	addq	$-29, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$7, %rsi
	jbe	.LBB41_4
# BB#9:                                 # %cond.false
	cmpq	$24, %rax
	jne	.LBB41_10
# BB#12:                                # %cond.true59
	movzbl	target_flags+3(%rip), %r12d
	andq	$2, %r12
	xorq	$6, %r12
	jmp	.LBB41_15
.LBB41_4:                               # %cond.true
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %r12
	je	.LBB41_15
# BB#5:                                 # %lor.rhs
	cmpq	$6, %rax
	jne	.LBB41_6
# BB#7:                                 # %lor.rhs
	movabsq	$1, %r12
	incq	%r12
	jmp	.LBB41_15
.LBB41_10:                              # %cond.false
	movq	%rdx, %rdi
	cmpq	$18, %rax
	jne	.LBB41_13
# BB#11:                                # %cond.true47
	movl	target_flags(%rip), %r12d
	movq	$25, %rcx
	shrq	%cl, %r12
	andq	$1, %r12
	xorq	$3, %r12
	jmp	.LBB41_14
.LBB41_6:
	movabsq	$0, %r12
	incq	%r12
	jmp	.LBB41_15
.LBB41_13:                              # %cond.false63
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %r12
.LBB41_14:                              # %cond.end83
	movq	%rdi, %rdx
.LBB41_15:                              # %cond.end83
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movabsq	$spilled_pseudos, %rdi
	movq	%rdx, %rsi
	callq	bitmap_bit_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB41_21
# BB#16:                                # %cond.end83
	addq	%r15, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%r13, %rbx
	jle	.LBB41_21
# BB#17:                                # %cond.end83
	leaq	(%r12,%r13), %rax
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r15, %rax
	jle	.LBB41_21
# BB#18:                                # %if.end
	movabsq	$spilled_pseudos, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	bitmap_set_bit
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r14,8), %rax
	movl	20(%rax), %ecx
	movl	spill_add_cost(,%r13,4), %edx
	subq	%rcx, %rdx
	movl	%edx, spill_add_cost(,%r13,4)
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB41_21
# BB#19:                                # %while.body.lr.ph
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	addq	%r12, %r13
	leaq	-1(%r12), %rdx
	leaq	spill_cost-4(,%r13,4), %rsi
	.align	16, 0x90
.LBB41_20:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movl	20(%rax), %ecx
	movl	(%rsi), %edi
	subq	%rcx, %rdi
	movl	%edi, (%rsi)
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	decq	%rdx
	addq	$-4, %rsi
	testq	%rdi, %rdi
	jg	.LBB41_20
.LBB41_21:                              # %while.end
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp575:
	.size	count_spilled_pseudo, .Ltmp575-count_spilled_pseudo
	.cfi_endproc

	.align	16, 0x90
	.type	count_pseudo,@function
count_pseudo:                           # @count_pseudo
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp582:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp583:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp584:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp585:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp586:
	.cfi_def_cfa_offset 48
.Ltmp587:
	.cfi_offset %rbx, -48
.Ltmp588:
	.cfi_offset %r12, -40
.Ltmp589:
	.cfi_offset %r13, -32
.Ltmp590:
	.cfi_offset %r14, -24
.Ltmp591:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	reg_n_info(%rip), %rax
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	32(%rax,%rbx,8), %rax
	movq	reg_renumber(%rip), %rcx
	movl	20(%rax), %r14d
	movswq	(%rcx,%rbx,2), %r15
	movabsq	$pseudos_counted, %rdi
	movq	%r12, %rsi
	callq	bitmap_bit_p
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	jne	.LBB42_19
# BB#1:                                 # %lor.lhs.false
	movabsq	$spilled_pseudos, %rdi
	movq	%r12, %rsi
	callq	bitmap_bit_p
	testq	%r13, %rax
	jne	.LBB42_19
# BB#2:                                 # %if.end
	movabsq	$pseudos_counted, %rdi
	movq	%r12, %rsi
	callq	bitmap_set_bit
	testq	%r15, %r15
	js	.LBB42_20
# BB#3:                                 # %if.end9
	movl	spill_add_cost(,%r15,4), %eax
	movq	cfun(%rip), %rcx
	addq	%r14, %rax
	movl	%eax, spill_add_cost(,%r15,4)
	movq	24(%rcx), %rax
	movq	80(%rax), %rax
	movq	(%rax,%rbx,8), %rax
	movzbl	2(%rax), %eax
	movq	%r15, %rcx
	andq	$65528, %rcx            # imm = 0xFFF8
	cmpq	$8, %rcx
	je	.LBB42_7
# BB#4:                                 # %if.end9
	leaq	-21(%r15), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB42_7
# BB#5:                                 # %if.end9
	leaq	-45(%r15), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB42_7
# BB#6:                                 # %if.end9
	leaq	-29(%r15), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$7, %rcx
	jbe	.LBB42_7
# BB#12:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB42_13
# BB#15:                                # %cond.true73
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB42_17
.LBB42_7:                               # %cond.true
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB42_17
# BB#8:                                 # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB42_9
# BB#10:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB42_17
.LBB42_13:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB42_16
# BB#14:                                # %cond.true60
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB42_17
.LBB42_9:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB42_17
.LBB42_16:                              # %while.cond.preheader
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB42_19
.LBB42_17:                              # %while.body.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rax, %r15
	leaq	-1(%rax), %rax
	leaq	spill_cost-4(,%r15,4), %rdx
	.align	16, 0x90
.LBB42_18:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rdx), %ecx
	addq	%r14, %rcx
	movl	%ecx, (%rdx)
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	decq	%rax
	addq	$-4, %rdx
	testq	%rsi, %rsi
	jg	.LBB42_18
.LBB42_19:                              # %while.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB42_20:                              # %if.then8
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.count_pseudo, %rcx
	movq	$1599, %rsi             # imm = 0x63F
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp592:
	.size	count_pseudo, .Ltmp592-count_pseudo
	.cfi_endproc

	.type	reload_in_progress,@object # @reload_in_progress
	.bss
	.globl	reload_in_progress
	.align	4
reload_in_progress:
	.long	0                       # 0x0
	.size	reload_in_progress, 4

	.type	spill_indirect_levels,@object # @spill_indirect_levels
	.local	spill_indirect_levels
	.comm	spill_indirect_levels,1,1
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"foo"
	.size	.L.str, 4

	.type	indirect_symref_ok,@object # @indirect_symref_ok
	.comm	indirect_symref_ok,1,1
	.type	double_reg_address_ok,@object # @double_reg_address_ok
	.comm	double_reg_address_ok,1,1
	.type	reload_obstack,@object  # @reload_obstack
	.comm	reload_obstack,88,8
	.type	reload_startobj,@object # @reload_startobj
	.comm	reload_startobj,8,8
	.type	spilled_pseudos,@object # @spilled_pseudos
	.local	spilled_pseudos
	.comm	spilled_pseudos,24,8
	.type	pseudos_counted,@object # @pseudos_counted
	.local	pseudos_counted
	.comm	pseudos_counted,24,8
	.type	unused_insn_chains,@object # @unused_insn_chains
	.local	unused_insn_chains
	.comm	unused_insn_chains,8,8
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"reload1.c"
	.size	.L.str1, 10

	.type	.L__FUNCTION__.compute_use_by_pseudos,@object # @__FUNCTION__.compute_use_by_pseudos
.L__FUNCTION__.compute_use_by_pseudos:
	.asciz	"compute_use_by_pseudos"
	.size	.L__FUNCTION__.compute_use_by_pseudos, 23

	.type	failure,@object         # @failure
	.local	failure
	.comm	failure,1,1
	.type	reload_firstobj,@object # @reload_firstobj
	.comm	reload_firstobj,8,8
	.type	reload_first_uid,@object # @reload_first_uid
	.comm	reload_first_uid,4,4
	.type	spill_stack_slot,@object # @spill_stack_slot
	.local	spill_stack_slot
	.comm	spill_stack_slot,424,16
	.type	spill_stack_slot_width,@object # @spill_stack_slot_width
	.local	spill_stack_slot_width
	.comm	spill_stack_slot_width,212,16
	.type	reg_equiv_constant,@object # @reg_equiv_constant
	.comm	reg_equiv_constant,8,8
	.type	reg_equiv_mem,@object   # @reg_equiv_mem
	.comm	reg_equiv_mem,8,8
	.type	reg_equiv_init,@object  # @reg_equiv_init
	.local	reg_equiv_init
	.comm	reg_equiv_init,8,8
	.type	reg_equiv_address,@object # @reg_equiv_address
	.comm	reg_equiv_address,8,8
	.type	reg_max_ref_width,@object # @reg_max_ref_width
	.local	reg_max_ref_width
	.comm	reg_max_ref_width,8,8
	.type	reg_old_renumber,@object # @reg_old_renumber
	.local	reg_old_renumber
	.comm	reg_old_renumber,8,8
	.type	pseudo_forbidden_regs,@object # @pseudo_forbidden_regs
	.local	pseudo_forbidden_regs
	.comm	pseudo_forbidden_regs,8,8
	.type	pseudo_previous_regs,@object # @pseudo_previous_regs
	.local	pseudo_previous_regs
	.comm	pseudo_previous_regs,8,8
	.type	bad_spill_regs_global,@object # @bad_spill_regs_global
	.local	bad_spill_regs_global
	.comm	bad_spill_regs_global,8,8
	.type	num_eliminable_invariants,@object # @num_eliminable_invariants
	.local	num_eliminable_invariants
	.comm	num_eliminable_invariants,4,4
	.type	reg_equiv_memory_loc,@object # @reg_equiv_memory_loc
	.comm	reg_equiv_memory_loc,8,8
	.type	num_labels,@object      # @num_labels
	.local	num_labels
	.comm	num_labels,4,4
	.type	offsets_known_at,@object # @offsets_known_at
	.local	offsets_known_at
	.comm	offsets_known_at,8,8
	.type	offsets_at,@object      # @offsets_at
	.local	offsets_at
	.comm	offsets_at,8,8
	.type	num_eliminable,@object  # @num_eliminable
	.local	num_eliminable
	.comm	num_eliminable,4,4
	.type	insns_need_reload,@object # @insns_need_reload
	.local	insns_need_reload
	.comm	insns_need_reload,8,8
	.type	something_needs_elimination,@object # @something_needs_elimination
	.local	something_needs_elimination
	.comm	something_needs_elimination,1,1
	.type	last_spill_reg,@object  # @last_spill_reg
	.local	last_spill_reg
	.comm	last_spill_reg,4,4
	.type	used_spill_regs,@object # @used_spill_regs
	.local	used_spill_regs
	.comm	used_spill_regs,8,8
	.type	reg_eliminate,@object   # @reg_eliminate
	.local	reg_eliminate
	.comm	reg_eliminate,8,8
	.type	caller_save_needed,@object # @caller_save_needed
	.comm	caller_save_needed,4,4
	.type	something_needs_operands_changed,@object # @something_needs_operands_changed
	.comm	something_needs_operands_changed,4,4
	.type	.L__FUNCTION__.reload,@object # @__FUNCTION__.reload
.L__FUNCTION__.reload:
	.asciz	"reload"
	.size	.L__FUNCTION__.reload, 7

	.type	reload.verbose_warned,@object # @reload.verbose_warned
	.local	reload.verbose_warned
	.comm	reload.verbose_warned,1,1
	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"frame size too large for reliable stack checking"
	.size	.L.str2, 49

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"try reducing the number of local variables"
	.size	.L.str3, 43

	.type	n_spills,@object        # @n_spills
	.local	n_spills
	.comm	n_spills,4,4
	.type	spill_regs,@object      # @spill_regs
	.local	spill_regs
	.comm	spill_regs,106,16
	.type	.L__FUNCTION__.eliminate_regs,@object # @__FUNCTION__.eliminate_regs
.L__FUNCTION__.eliminate_regs:
	.asciz	"eliminate_regs"
	.size	.L__FUNCTION__.eliminate_regs, 15

	.type	.L__FUNCTION__.reloads_conflict,@object # @__FUNCTION__.reloads_conflict
.L__FUNCTION__.reloads_conflict:
	.asciz	"reloads_conflict"
	.size	.L__FUNCTION__.reloads_conflict, 17

	.type	spill_reg_order,@object # @spill_reg_order
	.local	spill_reg_order
	.comm	spill_reg_order,106,16
	.type	reload_spill_index,@object # @reload_spill_index
	.comm	reload_spill_index,720,16
	.type	.L__FUNCTION__.fixup_abnormal_edges,@object # @__FUNCTION__.fixup_abnormal_edges
.L__FUNCTION__.fixup_abnormal_edges:
	.asciz	"fixup_abnormal_edges"
	.size	.L__FUNCTION__.fixup_abnormal_edges, 21

	.type	reload_in_optab,@object # @reload_in_optab
	.comm	reload_in_optab,236,16
	.type	reload_out_optab,@object # @reload_out_optab
	.comm	reload_out_optab,236,16
	.type	reload_insn_chain,@object # @reload_insn_chain
	.comm	reload_insn_chain,8,8
	.type	num_not_at_initial_offset,@object # @num_not_at_initial_offset
	.comm	num_not_at_initial_offset,4,4
	.type	reload_inherited,@object # @reload_inherited
	.comm	reload_inherited,180,16
	.type	reload_inheritance_insn,@object # @reload_inheritance_insn
	.comm	reload_inheritance_insn,1440,16
	.type	reload_override_in,@object # @reload_override_in
	.comm	reload_override_in,1440,16
	.type	reg_set_luid,@object    # @reg_set_luid
	.local	reg_set_luid
	.comm	reg_set_luid,212,16
	.type	move2add_last_label_luid,@object # @move2add_last_label_luid
	.local	move2add_last_label_luid
	.comm	move2add_last_label_luid,4,4
	.type	move2add_luid,@object   # @move2add_luid
	.local	move2add_luid
	.comm	move2add_luid,4,4
	.type	reg_mode,@object        # @reg_mode
	.local	reg_mode
	.comm	reg_mode,212,16
	.type	reg_base_reg,@object    # @reg_base_reg
	.local	reg_base_reg
	.comm	reg_base_reg,212,16
	.type	reg_offset,@object      # @reg_offset
	.local	reg_offset
	.comm	reg_offset,424,16
	.type	reload_combine_ruid,@object # @reload_combine_ruid
	.local	reload_combine_ruid
	.comm	reload_combine_ruid,4,4
	.type	reg_state,@object       # @reg_state
	.local	reg_state
	.comm	reg_state,6360,16
	.type	.L__FUNCTION__.reload_combine_note_use,@object # @__FUNCTION__.reload_combine_note_use
.L__FUNCTION__.reload_combine_note_use:
	.asciz	"reload_combine_note_use"
	.size	.L__FUNCTION__.reload_combine_note_use, 24

	.type	.L__FUNCTION__.reload_cse_simplify_operands,@object # @__FUNCTION__.reload_cse_simplify_operands
.L__FUNCTION__.reload_cse_simplify_operands:
	.asciz	"reload_cse_simplify_operands"
	.size	.L__FUNCTION__.reload_cse_simplify_operands, 29

	.type	reload_reg_used,@object # @reload_reg_used
	.local	reload_reg_used
	.comm	reload_reg_used,8,8
	.type	reload_reg_used_in_input_addr,@object # @reload_reg_used_in_input_addr
	.local	reload_reg_used_in_input_addr
	.comm	reload_reg_used_in_input_addr,240,16
	.type	reload_reg_used_in_inpaddr_addr,@object # @reload_reg_used_in_inpaddr_addr
	.local	reload_reg_used_in_inpaddr_addr
	.comm	reload_reg_used_in_inpaddr_addr,240,16
	.type	reload_reg_used_in_output_addr,@object # @reload_reg_used_in_output_addr
	.local	reload_reg_used_in_output_addr
	.comm	reload_reg_used_in_output_addr,240,16
	.type	reload_reg_used_in_outaddr_addr,@object # @reload_reg_used_in_outaddr_addr
	.local	reload_reg_used_in_outaddr_addr
	.comm	reload_reg_used_in_outaddr_addr,240,16
	.type	reload_reg_used_in_op_addr,@object # @reload_reg_used_in_op_addr
	.local	reload_reg_used_in_op_addr
	.comm	reload_reg_used_in_op_addr,8,8
	.type	reload_reg_used_in_op_addr_reload,@object # @reload_reg_used_in_op_addr_reload
	.local	reload_reg_used_in_op_addr_reload
	.comm	reload_reg_used_in_op_addr_reload,8,8
	.type	reload_reg_used_in_other_addr,@object # @reload_reg_used_in_other_addr
	.local	reload_reg_used_in_other_addr
	.comm	reload_reg_used_in_other_addr,8,8
	.type	reload_reg_used_in_input,@object # @reload_reg_used_in_input
	.local	reload_reg_used_in_input
	.comm	reload_reg_used_in_input,240,16
	.type	reload_reg_used_in_output,@object # @reload_reg_used_in_output
	.local	reload_reg_used_in_output
	.comm	reload_reg_used_in_output,240,16
	.type	reload_reg_used_in_insn,@object # @reload_reg_used_in_insn
	.local	reload_reg_used_in_insn
	.comm	reload_reg_used_in_insn,8,8
	.type	.L__FUNCTION__.clear_reload_reg_in_use,@object # @__FUNCTION__.clear_reload_reg_in_use
.L__FUNCTION__.clear_reload_reg_in_use:
	.asciz	"clear_reload_reg_in_use"
	.size	.L__FUNCTION__.clear_reload_reg_in_use, 24

	.type	spill_reg_rtx,@object   # @spill_reg_rtx
	.local	spill_reg_rtx
	.comm	spill_reg_rtx,424,16
	.type	spill_reg_store,@object # @spill_reg_store
	.local	spill_reg_store
	.comm	spill_reg_store,424,16
	.type	reg_last_reload_reg,@object # @reg_last_reload_reg
	.local	reg_last_reload_reg
	.comm	reg_last_reload_reg,8,8
	.type	reg_has_output_reload,@object # @reg_has_output_reload
	.local	reg_has_output_reload
	.comm	reg_has_output_reload,8,8
	.type	reg_reloaded_valid,@object # @reg_reloaded_valid
	.local	reg_reloaded_valid
	.comm	reg_reloaded_valid,8,8
	.type	reg_is_output_reload,@object # @reg_is_output_reload
	.local	reg_is_output_reload
	.comm	reg_is_output_reload,8,8
	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"`asm' operand requires impossible reload"
	.size	.L.str4, 41

	.type	reg_reloaded_died,@object # @reg_reloaded_died
	.local	reg_reloaded_died
	.comm	reg_reloaded_died,8,8
	.type	other_output_reload_insns,@object # @other_output_reload_insns
	.local	other_output_reload_insns
	.comm	other_output_reload_insns,240,16
	.type	outaddr_address_reload_insns,@object # @outaddr_address_reload_insns
	.local	outaddr_address_reload_insns
	.comm	outaddr_address_reload_insns,240,16
	.type	output_address_reload_insns,@object # @output_address_reload_insns
	.local	output_address_reload_insns
	.comm	output_address_reload_insns,240,16
	.type	output_reload_insns,@object # @output_reload_insns
	.local	output_reload_insns
	.comm	output_reload_insns,240,16
	.type	inpaddr_address_reload_insns,@object # @inpaddr_address_reload_insns
	.local	inpaddr_address_reload_insns
	.comm	inpaddr_address_reload_insns,240,16
	.type	input_address_reload_insns,@object # @input_address_reload_insns
	.local	input_address_reload_insns
	.comm	input_address_reload_insns,240,16
	.type	input_reload_insns,@object # @input_reload_insns
	.local	input_reload_insns
	.comm	input_reload_insns,240,16
	.type	other_input_address_reload_insns,@object # @other_input_address_reload_insns
	.local	other_input_address_reload_insns
	.comm	other_input_address_reload_insns,8,8
	.type	other_input_reload_insns,@object # @other_input_reload_insns
	.local	other_input_reload_insns
	.comm	other_input_reload_insns,8,8
	.type	operand_reload_insns,@object # @operand_reload_insns
	.local	operand_reload_insns
	.comm	operand_reload_insns,8,8
	.type	other_operand_reload_insns,@object # @other_operand_reload_insns
	.local	other_operand_reload_insns
	.comm	other_operand_reload_insns,8,8
	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"\nReloads for insn # %d\n"
	.size	.L.str5, 24

	.type	new_spill_reg_store,@object # @new_spill_reg_store
	.local	new_spill_reg_store
	.comm	new_spill_reg_store,424,16
	.type	reload_order,@object    # @reload_order
	.local	reload_order
	.comm	reload_order,360,16
	.type	reg_reloaded_contents,@object # @reg_reloaded_contents
	.local	reg_reloaded_contents
	.comm	reg_reloaded_contents,212,16
	.type	spill_reg_stored_to,@object # @spill_reg_stored_to
	.local	spill_reg_stored_to
	.comm	spill_reg_stored_to,424,16
	.type	reg_reloaded_dead,@object # @reg_reloaded_dead
	.local	reg_reloaded_dead
	.comm	reg_reloaded_dead,8,8
	.type	reg_reloaded_insn,@object # @reg_reloaded_insn
	.local	reg_reloaded_insn
	.comm	reg_reloaded_insn,424,16
	.type	.L__FUNCTION__.reload_reg_reaches_end_p,@object # @__FUNCTION__.reload_reg_reaches_end_p
.L__FUNCTION__.reload_reg_reaches_end_p:
	.asciz	"reload_reg_reaches_end_p"
	.size	.L__FUNCTION__.reload_reg_reaches_end_p, 25

	.type	.L__FUNCTION__.do_output_reload,@object # @__FUNCTION__.do_output_reload
.L__FUNCTION__.do_output_reload:
	.asciz	"do_output_reload"
	.size	.L__FUNCTION__.do_output_reload, 17

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"VOIDmode on an output"
	.size	.L.str6, 22

	.type	.L__FUNCTION__.emit_output_reload_insns,@object # @__FUNCTION__.emit_output_reload_insns
.L__FUNCTION__.emit_output_reload_insns:
	.asciz	"emit_output_reload_insns"
	.size	.L__FUNCTION__.emit_output_reload_insns, 25

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"output operand is constant in `asm'"
	.size	.L.str7, 36

	.type	.L__FUNCTION__.emit_input_reload_insns,@object # @__FUNCTION__.emit_input_reload_insns
.L__FUNCTION__.emit_input_reload_insns:
	.asciz	"emit_input_reload_insns"
	.size	.L__FUNCTION__.emit_input_reload_insns, 24

	.type	reload_reg_unavailable,@object # @reload_reg_unavailable
	.local	reload_reg_unavailable
	.comm	reload_reg_unavailable,8,8
	.type	reg_used_in_insn,@object # @reg_used_in_insn
	.local	reg_used_in_insn
	.comm	reg_used_in_insn,8,8
	.type	reload_reg_used_for_inherit,@object # @reload_reg_used_for_inherit
	.local	reload_reg_used_for_inherit
	.comm	reload_reg_used_for_inherit,8,8
	.type	.L__FUNCTION__.choose_reload_regs,@object # @__FUNCTION__.choose_reload_regs
.L__FUNCTION__.choose_reload_regs:
	.asciz	"choose_reload_regs"
	.size	.L__FUNCTION__.choose_reload_regs, 19

	.type	reload_reg_used_at_all,@object # @reload_reg_used_at_all
	.local	reload_reg_used_at_all
	.comm	reload_reg_used_at_all,8,8
	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"could not find a spill register"
	.size	.L.str8, 32

	.type	.L__FUNCTION__.failed_reload,@object # @__FUNCTION__.failed_reload
.L__FUNCTION__.failed_reload:
	.asciz	"failed_reload"
	.size	.L__FUNCTION__.failed_reload, 14

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"`asm' operand constraint incompatible with operand size"
	.size	.L.str9, 56

	.type	.L__FUNCTION__.reload_reg_free_p,@object # @__FUNCTION__.reload_reg_free_p
.L__FUNCTION__.reload_reg_free_p:
	.asciz	"reload_reg_free_p"
	.size	.L__FUNCTION__.reload_reg_free_p, 18

	.type	.L__FUNCTION__.eliminate_regs_in_insn,@object # @__FUNCTION__.eliminate_regs_in_insn
.L__FUNCTION__.eliminate_regs_in_insn:
	.asciz	"eliminate_regs_in_insn"
	.size	.L__FUNCTION__.eliminate_regs_in_insn, 23

	.type	.L__FUNCTION__.elimination_effects,@object # @__FUNCTION__.elimination_effects
.L__FUNCTION__.elimination_effects:
	.asciz	"elimination_effects"
	.size	.L__FUNCTION__.elimination_effects, 20

	.type	.L__FUNCTION__.finish_spills,@object # @__FUNCTION__.finish_spills
.L__FUNCTION__.finish_spills:
	.asciz	"finish_spills"
	.size	.L__FUNCTION__.finish_spills, 14

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	" Register %d now on stack.\n\n"
	.size	.L.str10, 29

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	" Register %d now in %d.\n\n"
	.size	.L.str11, 26

	.type	reg_eliminate_1,@object # @reg_eliminate_1
	.section	.rodata,"a",@progbits
	.align	16
reg_eliminate_1:
	.long	16                      # 0x10
	.long	7                       # 0x7
	.long	16                      # 0x10
	.long	6                       # 0x6
	.long	20                      # 0x14
	.long	7                       # 0x7
	.long	20                      # 0x14
	.long	6                       # 0x6
	.size	reg_eliminate_1, 32

	.type	.L__FUNCTION__.verify_initial_elim_offsets,@object # @__FUNCTION__.verify_initial_elim_offsets
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__FUNCTION__.verify_initial_elim_offsets:
	.asciz	"verify_initial_elim_offsets"
	.size	.L__FUNCTION__.verify_initial_elim_offsets, 28

	.type	used_spill_regs_local,@object # @used_spill_regs_local
	.local	used_spill_regs_local
	.comm	used_spill_regs_local,8,8
	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"Spilling for insn %d.\n"
	.size	.L.str12, 23

	.type	spill_failure.reg_class_names,@object # @spill_failure.reg_class_names
	.section	.rodata,"a",@progbits
	.align	16
spill_failure.reg_class_names:
	.quad	.L.str13
	.quad	.L.str14
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
	.size	spill_failure.reg_class_names, 200

	.type	.L.str13,@object        # @.str13
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str13:
	.asciz	"NO_REGS"
	.size	.L.str13, 8

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"AREG"
	.size	.L.str14, 5

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"DREG"
	.size	.L.str15, 5

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"CREG"
	.size	.L.str16, 5

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"BREG"
	.size	.L.str17, 5

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"SIREG"
	.size	.L.str18, 6

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"DIREG"
	.size	.L.str19, 6

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"AD_REGS"
	.size	.L.str20, 8

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"Q_REGS"
	.size	.L.str21, 7

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"NON_Q_REGS"
	.size	.L.str22, 11

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"INDEX_REGS"
	.size	.L.str23, 11

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"LEGACY_REGS"
	.size	.L.str24, 12

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"GENERAL_REGS"
	.size	.L.str25, 13

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"FP_TOP_REG"
	.size	.L.str26, 11

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"FP_SECOND_REG"
	.size	.L.str27, 14

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"FLOAT_REGS"
	.size	.L.str28, 11

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"SSE_REGS"
	.size	.L.str29, 9

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"MMX_REGS"
	.size	.L.str30, 9

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"FP_TOP_SSE_REGS"
	.size	.L.str31, 16

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"FP_SECOND_SSE_REGS"
	.size	.L.str32, 19

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"FLOAT_SSE_REGS"
	.size	.L.str33, 15

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"FLOAT_INT_REGS"
	.size	.L.str34, 15

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"INT_SSE_REGS"
	.size	.L.str35, 13

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"FLOAT_INT_SSE_REGS"
	.size	.L.str36, 19

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"ALL_REGS"
	.size	.L.str37, 9

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"can't find a register in class `%s' while reloading `asm'"
	.size	.L.str38, 58

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"unable to find a register to spill in class `%s'"
	.size	.L.str39, 49

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"this is the insn:"
	.size	.L.str40, 18

	.type	.L__FUNCTION__.spill_failure,@object # @__FUNCTION__.spill_failure
.L__FUNCTION__.spill_failure:
	.asciz	"spill_failure"
	.size	.L__FUNCTION__.spill_failure, 14

	.type	bad_spill_regs,@object  # @bad_spill_regs
	.local	bad_spill_regs
	.comm	bad_spill_regs,8,8
	.type	spill_cost,@object      # @spill_cost
	.local	spill_cost
	.comm	spill_cost,212,16
	.type	spill_add_cost,@object  # @spill_add_cost
	.local	spill_add_cost
	.comm	spill_add_cost,212,16
	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"Using reg %d for reload %d\n"
	.size	.L.str41, 28

	.type	.L__FUNCTION__.find_reg,@object # @__FUNCTION__.find_reg
.L__FUNCTION__.find_reg:
	.asciz	"find_reg"
	.size	.L__FUNCTION__.find_reg, 9

	.type	.L__FUNCTION__.count_pseudo,@object # @__FUNCTION__.count_pseudo
.L__FUNCTION__.count_pseudo:
	.asciz	"count_pseudo"
	.size	.L__FUNCTION__.count_pseudo, 13

	.type	reload_insn_firstobj,@object # @reload_insn_firstobj
	.local	reload_insn_firstobj
	.comm	reload_insn_firstobj,8,8
	.type	.L__FUNCTION__.replace_pseudos_in_call_usage,@object # @__FUNCTION__.replace_pseudos_in_call_usage
.L__FUNCTION__.replace_pseudos_in_call_usage:
	.asciz	"replace_pseudos_in_call_usage"
	.size	.L__FUNCTION__.replace_pseudos_in_call_usage, 30


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
