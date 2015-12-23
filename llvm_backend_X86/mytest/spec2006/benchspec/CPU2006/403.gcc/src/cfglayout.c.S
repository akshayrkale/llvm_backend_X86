	.file	"cfglayout.c.bc"
	.text
	.globl	scope_to_insns_initialize
	.align	16, 0x90
	.type	scope_to_insns_initialize,@function
scope_to_insns_initialize:              # @scope_to_insns_initialize
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
	callq	get_max_uid
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$.L.str, %rdx
	movq	$8, %rsi
	movq	%rax, %rdi
	callq	varray_init
	movq	%rax, insn_scopes(%rip)
	callq	get_insns
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB0_6
# BB#1:
	xorq	%r12, %r12
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movabsq	$4294967199, %r13       # imm = 0xFFFFFF9F
	jmp	.LBB0_2
.LBB0_10:                               # %sw.bb
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	32(%rbx), %r12
	jmp	.LBB0_11
	.align	16, 0x90
.LBB0_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rbx
	movq	24(%rbx), %r14
	movq	%rbx, %rdi
	callq	active_insn_p
	testq	%r15, %rax
	je	.LBB0_7
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	32(%rbx), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	jne	.LBB0_4
.LBB0_7:                                # %if.else
                                        #   in Loop: Header=BB0_2 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB0_5
# BB#8:                                 # %if.then24
                                        #   in Loop: Header=BB0_2 Depth=1
	movl	40(%rbx), %eax
	cmpq	%r13, %rax
	jne	.LBB0_9
# BB#12:                                # %sw.bb31
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	48(%r12), %r12
.LBB0_11:                               # %for.cond.outer.backedge
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	%rbx, %rdi
	callq	delete_insn
	jmp	.LBB0_5
	.align	16, 0x90
.LBB0_4:                                # %if.then
                                        #   in Loop: Header=BB0_2 Depth=1
	movslq	8(%rbx), %rax
	movq	insn_scopes(%rip), %rcx
	movq	%r12, 32(%rcx,%rax,8)
	jmp	.LBB0_5
.LBB0_9:                                # %if.then24
                                        #   in Loop: Header=BB0_2 Depth=1
	movabsq	$4294967198, %rcx       # imm = 0xFFFFFF9E
	cmpq	%rcx, %rax
	je	.LBB0_10
	.align	16, 0x90
.LBB0_5:                                # %for.cond.backedge
                                        #   in Loop: Header=BB0_2 Depth=1
	testq	%r14, %r14
	jne	.LBB0_2
.LBB0_6:                                # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp16:
	.size	scope_to_insns_initialize, .Ltmp16-scope_to_insns_initialize
	.cfi_endproc

	.globl	scope_to_insns_finalize
	.align	16, 0x90
	.type	scope_to_insns_finalize,@function
scope_to_insns_finalize:                # @scope_to_insns_finalize
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp23:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp24:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp25:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp26:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp27:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp28:
	.cfi_def_cfa_offset 64
.Ltmp29:
	.cfi_offset %rbx, -48
.Ltmp30:
	.cfi_offset %r12, -40
.Ltmp31:
	.cfi_offset %r13, -32
.Ltmp32:
	.cfi_offset %r14, -24
.Ltmp33:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	48(%rax), %rax
	movq	104(%rax), %r12
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	set_block_levels
	callq	get_insns
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB1_14
# BB#1:
	movabsq	$4294967199, %rdi       # imm = 0xFFFFFF9F
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB1_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_5 Depth 2
                                        #       Child Loop BB1_6 Depth 3
                                        #     Child Loop BB1_21 Depth 2
                                        #     Child Loop BB1_11 Depth 2
	movq	insn_scopes(%rip), %rax
	movslq	8(%r14), %rcx
	movq	(%rax), %rdx
	cmpq	%rdx, %rcx
	jae	.LBB1_13
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	32(%rax,%rcx,8), %rax
	testq	%rax, %rax
	je	.LBB1_13
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB1_2 Depth=1
	cmpq	%r12, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rax, %r15
	movq	%r12, %rax
	je	.LBB1_13
.LBB1_5:                                # %while.body.lr.ph.i
                                        #   Parent Loop BB1_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB1_6 Depth 3
	testq	%rax, %rax
	je	.LBB1_39
	.align	16, 0x90
.LBB1_6:                                # %while.body.i
                                        #   Parent Loop BB1_2 Depth=1
                                        #     Parent Loop BB1_5 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r15, %r15
	je	.LBB1_39
# BB#7:                                 # %if.end.i
                                        #   in Loop: Header=BB1_6 Depth=3
	movl	24(%rax), %edx
	movl	24(%r15), %esi
	movq	$2, %rcx
	shrq	%cl, %rdx
	movq	$2, %rcx
	shrq	%cl, %rsi
	cmpq	%rsi, %rdx
	ja	.LBB1_8
# BB#18:                                # %if.else.i
                                        #   in Loop: Header=BB1_6 Depth=3
	movq	48(%r15), %r15
	andq	%r13, %rsi
	andq	%r13, %rdx
	cmpq	%rsi, %rdx
	jae	.LBB1_8
# BB#19:                                # %if.then19.i
                                        #   in Loop: Header=BB1_6 Depth=3
	cmpq	%r15, %rax
	jne	.LBB1_6
	jmp	.LBB1_20
	.align	16, 0x90
.LBB1_8:                                # %while.cond.outer.backedge.i
                                        #   in Loop: Header=BB1_5 Depth=2
	movq	48(%rax), %rax
	cmpq	%r15, %rax
	jne	.LBB1_5
	jmp	.LBB1_9
	.align	16, 0x90
.LBB1_20:                               #   in Loop: Header=BB1_2 Depth=1
	movq	%rax, %r15
.LBB1_9:                                # %while.cond29.preheader.i
                                        #   in Loop: Header=BB1_2 Depth=1
	cmpq	%r12, %r15
	movq	%rdi, %rbx
	je	.LBB1_10
	.align	16, 0x90
.LBB1_21:                               # %while.body31.i
                                        #   Parent Loop BB1_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	emit_note_before
	movq	%r12, 32(%rax)
	movq	48(%r12), %r12
	cmpq	%r15, %r12
	jne	.LBB1_21
# BB#22:                                #   in Loop: Header=BB1_2 Depth=1
	movq	%r15, %r12
.LBB1_10:                               # %while.cond35.preheader.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	%rcx, %r12
	movq	%r14, %rax
	movq	%rcx, %rbx
	movabsq	$4294967198, %r15       # imm = 0xFFFFFF9E
	je	.LBB1_12
	.align	16, 0x90
.LBB1_11:                               # %while.body37.i
                                        #   Parent Loop BB1_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_note_before
	movq	%rbx, 32(%rax)
	movq	48(%rbx), %rbx
	cmpq	%r12, %rbx
	jne	.LBB1_11
.LBB1_12:                               #   in Loop: Header=BB1_2 Depth=1
	movq	8(%rsp), %r12           # 8-byte Reload
	movabsq	$4294967199, %rdi       # imm = 0xFFFFFF9F
.LBB1_13:                               # %for.inc
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	24(%r14), %r14
	testq	%r14, %r14
	jne	.LBB1_2
.LBB1_14:                               # %do.body
	movq	insn_scopes(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB1_16
# BB#15:                                # %if.then17
	callq	free
	movq	$0, insn_scopes(%rip)
.LBB1_16:                               # %do.end
	movabsq	$4294967197, %rsi       # imm = 0xFFFFFF9D
	xorq	%rdi, %rdi
	callq	emit_note
	movq	cfun(%rip), %rcx
	movq	48(%rcx), %rcx
	movq	104(%rcx), %rbx
	movq	%rax, %r14
	cmpq	%rbx, %r12
	je	.LBB1_27
# BB#17:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rbx, %r13
	movq	%r12, %rdx
.LBB1_33:                               # %while.body.lr.ph.i40
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_34 Depth 2
	testq	%rdx, %rdx
	je	.LBB1_35
	.align	16, 0x90
.LBB1_34:                               # %while.body.i46
                                        #   Parent Loop BB1_33 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r13, %r13
	je	.LBB1_35
# BB#31:                                # %if.end.i54
                                        #   in Loop: Header=BB1_34 Depth=2
	movl	24(%rdx), %esi
	movl	24(%r13), %edi
	movq	$2, %rcx
	shrq	%cl, %rsi
	movq	$2, %rcx
	shrq	%cl, %rdi
	cmpq	%rdi, %rsi
	ja	.LBB1_32
# BB#36:                                # %if.else.i63
                                        #   in Loop: Header=BB1_34 Depth=2
	movq	48(%r13), %r13
	andq	%rax, %rdi
	andq	%rax, %rsi
	cmpq	%rdi, %rsi
	jae	.LBB1_32
# BB#37:                                # %if.then19.i65
                                        #   in Loop: Header=BB1_34 Depth=2
	cmpq	%r13, %rdx
	jne	.LBB1_34
	jmp	.LBB1_38
	.align	16, 0x90
.LBB1_32:                               # %while.cond.outer.backedge.i60
                                        #   in Loop: Header=BB1_33 Depth=1
	movq	48(%rdx), %rdx
	cmpq	%r13, %rdx
	jne	.LBB1_33
	jmp	.LBB1_23
.LBB1_38:
	movq	%rdx, %r13
.LBB1_23:                               # %while.cond29.preheader.i43
	cmpq	%r12, %r13
	je	.LBB1_27
# BB#24:
	movabsq	$4294967199, %r15       # imm = 0xFFFFFF9F
	.align	16, 0x90
.LBB1_25:                               # %while.body31.i74
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	emit_note_before
	movq	%r12, 32(%rax)
	movq	48(%r12), %r12
	cmpq	%r13, %r12
	jne	.LBB1_25
# BB#26:
	movq	%r13, %r12
.LBB1_27:                               # %while.cond35.preheader.i68
	cmpq	%rbx, %r12
	je	.LBB1_30
# BB#28:
	movabsq	$4294967198, %r15       # imm = 0xFFFFFF9E
	movq	%r14, %rax
	.align	16, 0x90
.LBB1_29:                               # %while.body37.i81
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_note_before
	movq	%rbx, 32(%rax)
	movq	48(%rbx), %rbx
	cmpq	%r12, %rbx
	jne	.LBB1_29
.LBB1_30:                               # %change_scope.exit82
	movq	%r14, %rdi
	callq	delete_insn
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	reorder_blocks  # TAILCALL
.LBB1_39:                               # %if.then.i
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.change_scope, %rdx
	movq	$256, %rsi              # imm = 0x100
	callq	fancy_abort
.LBB1_35:                               # %if.then.i47
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.change_scope, %rcx
	movq	$256, %rsi              # imm = 0x100
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp34:
	.size	scope_to_insns_finalize, .Ltmp34-scope_to_insns_finalize
	.cfi_endproc

	.align	16, 0x90
	.type	set_block_levels,@function
set_block_levels:                       # @set_block_levels
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp39:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp40:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp41:
	.cfi_def_cfa_offset 32
.Ltmp42:
	.cfi_offset %rbx, -32
.Ltmp43:
	.cfi_offset %r14, -24
.Ltmp44:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB2_3
# BB#1:                                 # %while.body.lr.ph
	movq	$2, %rcx
	movq	%r14, %r15
	shlq	%cl, %r15
	incq	%r14
	.align	16, 0x90
.LBB2_2:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movl	24(%rbx), %eax
	andq	$3, %rax
	orq	%r15, %rax
	movl	%eax, 24(%rbx)
	movq	40(%rbx), %rdi
	movq	%r14, %rsi
	callq	set_block_levels
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB2_2
.LBB2_3:                                # %while.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp45:
	.size	set_block_levels, .Ltmp45-set_block_levels
	.cfi_endproc

	.globl	verify_insn_chain
	.align	16, 0x90
	.type	verify_insn_chain,@function
verify_insn_chain:                      # @verify_insn_chain
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp49:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp50:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp51:
	.cfi_def_cfa_offset 32
.Ltmp52:
	.cfi_offset %rbx, -24
.Ltmp53:
	.cfi_offset %r14, -16
	callq	get_insns
	xorq	%rcx, %rcx
	testq	%rax, %rax
	movq	$1, %r14
	je	.LBB3_1
	.align	16, 0x90
.LBB3_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rbx
	movq	16(%rbx), %rax
	cmpq	%rcx, %rax
	jne	.LBB3_3
# BB#5:                                 # %for.inc
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	24(%rbx), %rax
	incq	%r14
	testq	%rax, %rax
	movq	%rbx, %rcx
	jne	.LBB3_2
	jmp	.LBB3_6
.LBB3_1:
	xorq	%rbx, %rbx
.LBB3_6:                                # %for.end
	callq	get_last_insn
	cmpq	%rax, %rbx
	jne	.LBB3_7
# BB#8:                                 # %if.end8
	callq	get_last_insn
	testq	%rax, %rax
	movq	$1, %rcx
	je	.LBB3_13
# BB#9:
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB3_10:                               # %for.body12
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rax), %rsi
	cmpq	%rdx, %rsi
	jne	.LBB3_11
# BB#12:                                # %for.inc19
                                        #   in Loop: Header=BB3_10 Depth=1
	movq	16(%rax), %rsi
	movq	%rax, %rdx
	incq	%rcx
	testq	%rsi, %rsi
	movq	%rsi, %rax
	jne	.LBB3_10
.LBB3_13:                               # %for.end24
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	andq	%rax, %r14
	cmpq	%rcx, %r14
	jne	.LBB3_14
# BB#15:                                # %if.end27
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB3_3:                                # %if.then
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.verify_insn_chain, %rcx
	movq	$519, %rsi              # imm = 0x207
	jmp	.LBB3_4
.LBB3_7:                                # %if.then7
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.verify_insn_chain, %rcx
	movq	$522, %rsi              # imm = 0x20A
	jmp	.LBB3_4
.LBB3_11:                               # %if.then17
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.verify_insn_chain, %rcx
	movq	$528, %rsi              # imm = 0x210
	jmp	.LBB3_4
.LBB3_14:                               # %if.then26
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.verify_insn_chain, %rcx
	movq	$531, %rsi              # imm = 0x213
.LBB3_4:                                # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp54:
	.size	verify_insn_chain, .Ltmp54-verify_insn_chain
	.cfi_endproc

	.globl	cfg_layout_initialize
	.align	16, 0x90
	.type	cfg_layout_initialize,@function
cfg_layout_initialize:                  # @cfg_layout_initialize
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
	subq	$16, %rsp
.Ltmp66:
	.cfi_def_cfa_offset 64
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
	movq	$32, %rdi
	callq	alloc_aux_for_blocks
	callq	scope_to_insns_initialize
	callq	get_insns
	movslq	n_basic_blocks(%rip), %r8
	testq	%r8, %r8
	jle	.LBB4_21
# BB#1:
	xorq	%rdi, %rdi
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movabsq	$4294967199, %r12       # imm = 0xFFFFFF9F
	movabsq	$4294967201, %rsi       # imm = 0xFFFFFFA1
	.align	16, 0x90
.LBB4_2:                                # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_5 Depth 2
                                        #       Child Loop BB4_6 Depth 3
                                        #     Child Loop BB4_17 Depth 2
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rdi,8), %r15
	movq	80(%r15), %r9
	movq	%rax, (%r9)
	movl	88(%r15), %eax
	incq	%rax
	movq	%rax, %rdx
	andq	%r14, %rdx
	movq	%r8, %rdi
	andq	%r14, %rdi
	xorq	%rcx, %rcx
	cmpq	%rdi, %rdx
	je	.LBB4_4
# BB#3:                                 # %if.then.i.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %rax
	movq	(%rax), %rcx
.LBB4_4:                                # %if.end.i.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	8(%r15), %rax
	movq	%rax, %rdi
	.align	16, 0x90
.LBB4_5:                                # %for.cond.outer.i.i
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB4_6 Depth 3
	movq	%rdi, %rbx
.LBB4_6:                                # %for.cond.i.i
                                        #   Parent Loop BB4_2 Depth=1
                                        #     Parent Loop BB4_5 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	24(%rdi), %rdi
	testq	%rdi, %rdi
	je	.LBB4_16
# BB#7:                                 # %for.cond.i.i
                                        #   in Loop: Header=BB4_6 Depth=3
	cmpq	%rcx, %rdi
	je	.LBB4_16
# BB#8:                                 # %if.end8.i.i
                                        #   in Loop: Header=BB4_6 Depth=3
	movzbl	(%rdi), %edx
	cmpq	$36, %rdx
	je	.LBB4_13
# BB#9:                                 # %if.end8.i.i
                                        #   in Loop: Header=BB4_6 Depth=3
	cmpq	$35, %rdx
	je	.LBB4_5
# BB#10:                                # %if.end8.i.i
                                        #   in Loop: Header=BB4_6 Depth=3
	cmpq	$37, %rdx
	jne	.LBB4_16
# BB#11:                                # %sw.bb9.i.i
                                        #   in Loop: Header=BB4_6 Depth=3
	movl	40(%rdi), %edx
	cmpq	%r12, %rdx
	je	.LBB4_5
# BB#12:                                # %sw.bb9.i.i
                                        #   in Loop: Header=BB4_6 Depth=3
	cmpq	%rsi, %rdx
	jne	.LBB4_6
	jmp	.LBB4_5
.LBB4_13:                               # %sw.bb14.i.i
                                        #   in Loop: Header=BB4_5 Depth=2
	movq	24(%rdi), %rdi
	testq	%rdi, %rdi
	je	.LBB4_16
# BB#14:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB4_5 Depth=2
	movzbl	(%rdi), %edx
	cmpq	$33, %rdx
	jne	.LBB4_16
# BB#15:                                # %land.lhs.true24.i.i
                                        #   in Loop: Header=BB4_5 Depth=2
	movq	32(%rdi), %rdx
	movl	(%rdx), %edx
	andq	$65534, %rdx            # imm = 0xFFFE
	cmpq	$44, %rdx
	je	.LBB4_5
	.align	16, 0x90
.LBB4_16:                               # %for.cond49.preheader.i.i
                                        #   in Loop: Header=BB4_2 Depth=1
	cmpq	%rax, %rbx
	movq	%rbx, %r13
	je	.LBB4_20
	.align	16, 0x90
.LBB4_17:                               # %for.body52.i.i
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r13, %rdi
	movzbl	(%rdi), %ecx
	movq	16(%rdi), %r13
	cmpq	$37, %rcx
	jne	.LBB4_18
# BB#22:                                # %if.then59.i.i
                                        #   in Loop: Header=BB4_17 Depth=2
	movl	40(%rdi), %ecx
	addq	$99, %rcx
	andq	%r14, %rcx
	cmpq	$11, %rcx
	ja	.LBB4_24
# BB#23:                                # %if.then59.i.i
                                        #   in Loop: Header=BB4_17 Depth=2
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$2069, %rdx             # imm = 0x815
	jne	.LBB4_18
	.align	16, 0x90
.LBB4_24:                               # %sw.default64.i.i
                                        #   in Loop: Header=BB4_17 Depth=2
	movq	%rdi, %rsi
	movq	%rbx, %rdx
	callq	reorder_insns
	movq	8(%r15), %rax
.LBB4_18:                               # %for.cond49.backedge.i.i
                                        #   in Loop: Header=BB4_17 Depth=2
	cmpq	%rax, %r13
	jne	.LBB4_17
# BB#19:                                # %skip_insns_after_block.exit.loopexit.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	80(%r15), %r9
	movl	n_basic_blocks(%rip), %r8d
	movq	%rbx, %rax
.LBB4_20:                               # %skip_insns_after_block.exit.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%rax, 8(%r9)
	movq	24(%rax), %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	incq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r8, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	movabsq	$4294967201, %rsi       # imm = 0xFFFFFFA1
	jl	.LBB4_2
.LBB4_21:                               # %record_effective_endpoints.exit
	movq	%rax, function_tail_eff_head(%rip)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp72:
	.size	cfg_layout_initialize, .Ltmp72-cfg_layout_initialize
	.cfi_endproc

	.globl	cfg_layout_finalize
	.align	16, 0x90
	.type	cfg_layout_finalize,@function
cfg_layout_finalize:                    # @cfg_layout_finalize
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
	movq	entry_exit_blocks+144(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB5_12
# BB#1:
	xorq	%rax, %rax
	.align	16, 0x90
.LBB5_2:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	48(%rcx), %edx
	testq	$1, %rdx
	je	.LBB5_4
# BB#3:                                 # %if.then.i
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	16(%rcx), %rax
.LBB5_4:                                # %for.inc.i
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB5_2
# BB#5:                                 # %for.end.i
	testq	%rax, %rax
	je	.LBB5_12
# BB#6:                                 # %land.lhs.true.i
	movq	80(%rax), %rcx
	movq	16(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB5_12
# BB#7:                                 # %if.then4.i
	movq	basic_block_info(%rip), %rsi
	movq	32(%rsi), %rsi
	.align	16, 0x90
.LBB5_8:                                # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	80(%rsi), %rdi
	movq	16(%rdi), %rsi
	cmpq	%rax, %rsi
	jne	.LBB5_8
# BB#9:                                 # %while.cond14.while.cond14_crit_edge.i.lr.ph
	movq	%rdx, 16(%rdi)
	.align	16, 0x90
.LBB5_10:                               # %while.cond14.while.cond14_crit_edge.i
                                        # =>This Inner Loop Header: Depth=1
	movq	80(%rdx), %rsi
	movq	16(%rsi), %rdx
	testq	%rdx, %rdx
	jne	.LBB5_10
# BB#11:                                # %while.end21.i
	movq	%rax, 16(%rsi)
	movq	$0, 16(%rcx)
.LBB5_12:                               # %fixup_fallthru_exit_predecessor.exit
	movq	basic_block_info(%rip), %rax
	movq	32(%rax), %rax
	movq	80(%rax), %rax
	movq	16(%rax), %rdx
	testq	%rdx, %rdx
	movq	$1, %rcx
	je	.LBB5_14
	.align	16, 0x90
.LBB5_13:                               # %for.body.i5
                                        # =>This Inner Loop Header: Depth=1
	movq	80(%rdx), %rsi
	movq	8(%rax), %rax
	movq	(%rsi), %rsi
	movq	%rsi, 24(%rax)
	movq	%rax, 16(%rsi)
	movq	80(%rdx), %rax
	movq	16(%rax), %rdx
	incq	%rcx
	testq	%rdx, %rdx
	jne	.LBB5_13
.LBB5_14:                               # %for.end.i6
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movl	n_basic_blocks(%rip), %edx
	andq	%rsi, %rdx
	andq	%rsi, %rcx
	cmpq	%rdx, %rcx
	jne	.LBB5_15
# BB#17:                                # %if.end.i
	movq	8(%rax), %rax
	movq	function_tail_eff_head(%rip), %rcx
	movq	%rcx, 24(%rax)
	movq	function_tail_eff_head(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB5_19
# BB#18:                                # %if.then16.i
	movq	%rax, 16(%rcx)
	.align	16, 0x90
.LBB5_19:                               # %while.cond.i9
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rdi
	movq	24(%rdi), %rax
	testq	%rax, %rax
	jne	.LBB5_19
# BB#20:                                # %while.end.i10
	movq	%rsi, %rbx
	callq	set_last_insn
	movq	basic_block_info(%rip), %rax
	movq	32(%rax), %r15
	xorq	%r12, %r12
	testq	%r15, %r15
	je	.LBB5_65
# BB#21:
	movq	%rbx, %r13
	.align	16, 0x90
.LBB5_22:                               # %for.body33.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_23 Depth 2
	movq	40(%r15), %rax
	testq	%rax, %rax
	movabsq	$0, %r14
	movabsq	$0, %rbx
	je	.LBB5_63
	.align	16, 0x90
.LBB5_23:                               # %for.body40.i
                                        #   Parent Loop BB5_22 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	48(%rax), %ecx
	testq	$1, %rcx
	jne	.LBB5_24
# BB#25:                                # %if.else.i
                                        #   in Loop: Header=BB5_23 Depth=2
	testq	$8, %rcx
	movq	%rax, %rcx
	je	.LBB5_27
# BB#26:                                # %if.else.i
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	%r14, %rcx
.LBB5_27:                               # %if.else.i
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	%rcx, %r14
	jmp	.LBB5_28
	.align	16, 0x90
.LBB5_24:                               #   in Loop: Header=BB5_23 Depth=2
	movq	%rax, %rbx
.LBB5_28:                               # %for.inc49.i
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB5_23
# BB#29:                                # %for.end50.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	8(%r15), %r12
	movzbl	(%r12), %eax
	cmpq	$33, %rax
	jne	.LBB5_57
# BB#30:                                # %if.then52.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	%r12, %rdi
	callq	any_condjump_p
	testq	%r13, %rax
	jne	.LBB5_31
# BB#54:                                # %if.else101.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	%r12, %rdi
	callq	returnjump_p
	testq	%r13, %rax
	jne	.LBB5_63
# BB#55:                                # %if.else101.i
                                        #   in Loop: Header=BB5_22 Depth=1
	testq	%rbx, %rbx
	je	.LBB5_63
	jmp	.LBB5_56
	.align	16, 0x90
.LBB5_57:                               # %if.else110.i
                                        #   in Loop: Header=BB5_22 Depth=1
	testq	%rbx, %rbx
	je	.LBB5_63
# BB#58:                                # %if.end113.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	80(%r15), %rax
	movq	16(%rax), %rcx
	movq	24(%rbx), %rax
	cmpq	%rax, %rcx
	je	.LBB5_63
# BB#59:                                # %if.end119.i
                                        #   in Loop: Header=BB5_22 Depth=1
	testq	%rcx, %rcx
	jne	.LBB5_61
# BB#60:                                # %if.end119.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movabsq	$entry_exit_blocks+112, %rcx
	cmpq	%rcx, %rax
	je	.LBB5_63
	jmp	.LBB5_61
.LBB5_31:                               # %if.then54.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	80(%r15), %rax
	movq	16(%rax), %rax
	movq	24(%rbx), %r13
	cmpq	%r13, %rax
	jne	.LBB5_33
# BB#32:                                #   in Loop: Header=BB5_22 Depth=1
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jmp	.LBB5_63
.LBB5_33:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB5_22 Depth=1
	testq	%rax, %rax
	jne	.LBB5_36
# BB#34:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movabsq	$entry_exit_blocks+112, %rcx
	cmpq	%rcx, %r13
	jne	.LBB5_36
# BB#35:                                #   in Loop: Header=BB5_22 Depth=1
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jmp	.LBB5_63
.LBB5_36:                               # %if.end64.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	24(%r14), %rcx
	cmpq	%rcx, %rax
	jne	.LBB5_37
# BB#47:                                # %if.else89.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	(%r13), %rsi
	movzbl	(%rsi), %eax
	cmpq	$36, %rax
	je	.LBB5_52
# BB#48:                                # %if.then.i257.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB5_50
# BB#49:                                # %if.then1.i260.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movl	88(%r13), %edx
	xorq	%rax, %rax
	movabsq	$.L.str6, %rsi
	callq	fprintf
.LBB5_50:                               # %if.end.i267.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	%r13, %rdi
	callq	block_label
	movq	80(%r13), %rcx
	movq	(%rcx), %rdx
	movq	(%r13), %rdi
	movq	16(%rdx), %rdx
	movq	%rax, %rsi
	cmpq	%rdx, %rdi
	jne	.LBB5_52
# BB#51:                                # %if.then5.i268.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	%rsi, (%rcx)
.LBB5_52:                               # %label_for_bb.exit270.i
                                        #   in Loop: Header=BB5_22 Depth=1
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	invert_jump
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB5_61
# BB#53:                                # %if.then94.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movl	48(%rbx), %eax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rcx, %rax
	movl	%eax, 48(%rbx)
	movl	48(%r14), %eax
	orq	$1, %rax
	movl	%eax, 48(%r14)
	movq	%r15, %rdi
	callq	update_br_prob_note
	jmp	.LBB5_63
.LBB5_37:                               # %if.then69.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	$16, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	je	.LBB5_61
# BB#38:                                # %land.lhs.true72.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	cmpq	$4999, %rax             # imm = 0x1387
	jg	.LBB5_39
# BB#40:                                # %land.lhs.true79.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	24(%rbx), %r13
	movq	(%r13), %rsi
	movzbl	(%rsi), %eax
	cmpq	$36, %rax
	je	.LBB5_45
# BB#41:                                # %if.then.i.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB5_43
# BB#42:                                # %if.then1.i.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movl	88(%r13), %edx
	xorq	%rax, %rax
	movabsq	$.L.str6, %rsi
	callq	fprintf
.LBB5_43:                               # %if.end.i.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	%r13, %rdi
	callq	block_label
	movq	80(%r13), %rcx
	movq	(%rcx), %rdx
	movq	(%r13), %rdi
	movq	16(%rdx), %rdx
	movq	%rax, %rsi
	cmpq	%rdx, %rdi
	jne	.LBB5_45
# BB#44:                                # %if.then5.i.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	%rsi, (%rcx)
.LBB5_45:                               # %label_for_bb.exit.i
                                        #   in Loop: Header=BB5_22 Depth=1
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	invert_jump
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB5_61
# BB#46:                                # %if.then84.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movl	48(%rbx), %eax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rcx, %rax
	movl	%eax, 48(%rbx)
	movl	48(%r14), %eax
	orq	$1, %rax
	movl	%eax, 48(%r14)
	movq	%r15, %rdi
	callq	update_br_prob_note
	movq	%r14, %rbx
	jmp	.LBB5_61
.LBB5_39:                               #   in Loop: Header=BB5_22 Depth=1
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB5_61:                               # %if.end128.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	%rbx, %rdi
	callq	force_nonfallthru
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB5_63
# BB#62:                                # %if.then131.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	$32, %rsi
	movq	%rbx, %rdi
	callq	alloc_aux_for_block
	movq	(%rbx), %rax
	movq	80(%rbx), %rcx
	movq	%rax, (%rcx)
	movq	8(%rbx), %rax
	movq	24(%rax), %rax
	movq	%rax, 8(%rcx)
	movq	$1, %rax
	movl	%eax, 24(%rcx)
	movq	80(%r15), %rax
	movq	16(%rax), %rdx
	movq	%rdx, 16(%rcx)
	movq	%rbx, 16(%rax)
	movq	%rbx, %r15
	.align	16, 0x90
.LBB5_63:                               # %for.inc148.i
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	80(%r15), %rax
	movq	16(%rax), %r15
	testq	%r15, %r15
	jne	.LBB5_22
# BB#64:                                # %for.cond31.for.end151_crit_edge.i
	movq	basic_block_info(%rip), %rax
	movq	32(%rax), %r12
.LBB5_65:                               # %for.end151.i
	movq	rtl_dump_file(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB5_67
# BB#66:                                # %if.then156.i
	movabsq	$.L.str2, %rdi
	movq	$20, %rsi
	movq	$1, %rdx
	callq	fwrite
.LBB5_67:                               # %for.cond159.preheader.i
	testq	%r12, %r12
	je	.LBB5_74
# BB#68:
	xorq	%rbx, %rbx
	movabsq	$.L.str4, %r15
	movabsq	$.L.str5, %r13
	movabsq	$.L.str3, %r14
	.align	16, 0x90
.LBB5_69:                               # %for.body161.i
                                        # =>This Inner Loop Header: Depth=1
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB5_73
# BB#70:                                # %if.then163.i
                                        #   in Loop: Header=BB5_69 Depth=1
	movslq	88(%r12), %r8
	movq	$32, %rcx
	movl	n_basic_blocks(%rip), %eax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r8
	movq	%r15, %rcx
	jge	.LBB5_72
# BB#71:                                # %if.then163.i
                                        #   in Loop: Header=BB5_69 Depth=1
	movq	%r13, %rcx
.LBB5_72:                               # %if.then163.i
                                        #   in Loop: Header=BB5_69 Depth=1
	movl	104(%r12), %r9d
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	fprintf
.LBB5_73:                               # %if.end168.i
                                        #   in Loop: Header=BB5_69 Depth=1
	movl	%ebx, 88(%r12)
	movq	basic_block_info(%rip), %rax
	movq	%r12, 32(%rax,%rbx,8)
	movq	80(%r12), %rax
	movq	16(%rax), %r12
	incq	%rbx
	testq	%r12, %r12
	jne	.LBB5_69
.LBB5_74:                               # %fixup_reorder_chain.exit
	callq	scope_to_insns_finalize
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free_aux_for_blocks  # TAILCALL
.LBB5_15:                               # %if.then.i7
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.fixup_reorder_chain, %rcx
	movq	$353, %rsi              # imm = 0x161
	jmp	.LBB5_16
.LBB5_56:                               # %if.end108.i
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.fixup_reorder_chain, %rcx
	movq	$447, %rsi              # imm = 0x1BF
.LBB5_16:                               # %if.then.i7
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp89:
	.size	cfg_layout_finalize, .Ltmp89-cfg_layout_finalize
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"insn scopes"
	.size	.L.str, 12

	.type	insn_scopes,@object     # @insn_scopes
	.local	insn_scopes
	.comm	insn_scopes,8,8
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"cfglayout.c"
	.size	.L.str1, 12

	.type	.L__FUNCTION__.verify_insn_chain,@object # @__FUNCTION__.verify_insn_chain
.L__FUNCTION__.verify_insn_chain:
	.asciz	"verify_insn_chain"
	.size	.L__FUNCTION__.verify_insn_chain, 18

	.type	.L__FUNCTION__.fixup_reorder_chain,@object # @__FUNCTION__.fixup_reorder_chain
.L__FUNCTION__.fixup_reorder_chain:
	.asciz	"fixup_reorder_chain"
	.size	.L__FUNCTION__.fixup_reorder_chain, 20

	.type	function_tail_eff_head,@object # @function_tail_eff_head
	.local	function_tail_eff_head
	.comm	function_tail_eff_head,8,8
	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Reordered sequence:\n"
	.size	.L.str2, 21

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	" %i %sbb %i freq %i\n"
	.size	.L.str3, 21

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"compensation "
	.size	.L.str4, 14

	.type	.L.str5,@object         # @.str5
.L.str5:
	.zero	1
	.size	.L.str5, 1

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"Emitting label for block %d\n"
	.size	.L.str6, 29

	.type	.L__FUNCTION__.change_scope,@object # @__FUNCTION__.change_scope
.L__FUNCTION__.change_scope:
	.asciz	"change_scope"
	.size	.L__FUNCTION__.change_scope, 13


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
