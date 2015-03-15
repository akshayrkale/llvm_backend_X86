	.file	"cfganal.c.bc"
	.text
	.globl	forwarder_block_p
	.align	16, 0x90
	.type	forwarder_block_p,@function
forwarder_block_p:                      # @forwarder_block_p
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
	movq	%rdi, %r12
	xorq	%r14, %r14
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %r12
	je	.LBB0_22
# BB#1:                                 # %entry
	movabsq	$entry_exit_blocks, %rax
	cmpq	%rax, %r12
	je	.LBB0_22
# BB#2:                                 # %lor.lhs.false2
	movq	40(%r12), %rax
	testq	%rax, %rax
	je	.LBB0_22
# BB#3:                                 # %lor.lhs.false3
	cmpq	$0, 8(%rax)
	jne	.LBB0_22
# BB#4:                                 # %if.end
	movq	(%r12), %rbx
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %ecx
	movq	8(%r12), %r15
	cmpq	$105, %rcx
	jne	.LBB0_5
# BB#6:                                 # %if.end
	movabsq	$1, %rcx
	jmp	.LBB0_7
.LBB0_5:
	movabsq	$0, %rcx
.LBB0_7:                                # %if.end
	cmpq	%r15, %rbx
	je	.LBB0_8
# BB#9:
	movabsq	$0, %r13
	.align	16, 0x90
.LBB0_10:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	testq	$1, %rcx
	je	.LBB0_13
# BB#11:                                # %land.lhs.true
                                        #   in Loop: Header=BB0_10 Depth=1
	movq	%rbx, %rdi
	callq	active_insn_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_22
# BB#12:                                # %land.lhs.true.for.inc_crit_edge
                                        #   in Loop: Header=BB0_10 Depth=1
	movq	8(%r12), %r15
.LBB0_13:                               # %for.inc
                                        #   in Loop: Header=BB0_10 Depth=1
	movq	24(%rbx), %rbx
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %ecx
	cmpq	$105, %rcx
	movq	%r13, %rcx
	jne	.LBB0_15
# BB#14:                                # %for.inc
                                        #   in Loop: Header=BB0_10 Depth=1
	movabsq	$1, %rcx
.LBB0_15:                               # %for.inc
                                        #   in Loop: Header=BB0_10 Depth=1
	cmpq	%r15, %rbx
	jne	.LBB0_10
	jmp	.LBB0_16
.LBB0_8:
	movq	%rbx, %r15
.LBB0_16:                               # %for.end
	movq	$1, %r14
	testq	%rcx, %rcx
	je	.LBB0_22
# BB#17:                                # %lor.lhs.false20
	cmpq	$33, %rax
	jne	.LBB0_19
# BB#18:                                # %land.lhs.true25
	movq	%r15, %rdi
	callq	simplejump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_22
.LBB0_19:                               # %lor.rhs
	movq	%r15, %rdi
	callq	active_insn_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_20
# BB#21:                                # %lor.rhs
	movabsq	$1, %r14
	jmp	.LBB0_22
.LBB0_20:
	movabsq	$0, %r14
.LBB0_22:                               # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp16:
	.size	forwarder_block_p, .Ltmp16-forwarder_block_p
	.cfi_endproc

	.globl	can_fallthru
	.align	16, 0x90
	.type	can_fallthru,@function
can_fallthru:                           # @can_fallthru
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
	movq	8(%rdi), %r14
	movl	88(%rdi), %eax
	movq	(%rsi), %rbx
	movl	88(%rsi), %ecx
	incq	%rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	cmpq	%rcx, %rax
	jne	.LBB1_3
# BB#1:                                 # %land.lhs.true
	movq	%rbx, %rdi
	callq	active_insn_p
	testq	%r15, %rax
	jne	.LBB1_3
# BB#2:                                 # %if.then
	movq	%rbx, %rdi
	callq	next_active_insn
	movq	%rax, %rbx
.LBB1_3:                                # %if.end
	movq	%r14, %rdi
	callq	next_active_insn
	cmpq	%rbx, %rax
	jne	.LBB1_4
# BB#5:                                 # %if.end
	movabsq	$1, %rax
	jmp	.LBB1_6
.LBB1_4:
	movabsq	$0, %rax
.LBB1_6:                                # %if.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp27:
	.size	can_fallthru, .Ltmp27-can_fallthru
	.cfi_endproc

	.globl	mark_dfs_back_edges
	.align	16, 0x90
	.type	mark_dfs_back_edges,@function
mark_dfs_back_edges:                    # @mark_dfs_back_edges
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp34:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp35:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp36:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp37:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp38:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp39:
	.cfi_def_cfa_offset 80
.Ltmp40:
	.cfi_offset %rbx, -48
.Ltmp41:
	.cfi_offset %r12, -40
.Ltmp42:
	.cfi_offset %r13, -32
.Ltmp43:
	.cfi_offset %r14, -24
.Ltmp44:
	.cfi_offset %r15, -16
	movslq	n_basic_blocks(%rip), %rdi
	movq	$4, %rsi
	callq	xcalloc
	movslq	n_basic_blocks(%rip), %rdi
	movq	%rax, %r15
	movq	$4, %rsi
	callq	xcalloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, 16(%rsp)          # 8-byte Spill
	incq	%rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, %rbx
	callq	sbitmap_alloc
	movq	%rax, %r12
	movq	%r12, %rdi
	callq	sbitmap_zero
	movq	entry_exit_blocks+40(%rip), %rax
	movq	%rax, (%rbx)
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	movq	$1, %r14
	movq	$1, %rdx
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
.LBB2_1:                                # %while.body.lr.ph.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_2 Depth 2
                                        #       Child Loop BB2_3 Depth 3
	movq	%r14, %r8
.LBB2_2:                                # %while.body.lr.ph
                                        #   Parent Loop BB2_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_3 Depth 3
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	.align	16, 0x90
.LBB2_3:                                # %while.body
                                        #   Parent Loop BB2_1 Depth=1
                                        #     Parent Loop BB2_2 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	-8(%rbx,%r8,8), %r10
	movl	48(%r10), %r9d
	movq	24(%r10), %rdi
	movq	%r9, %rax
	movabsq	$4294967263, %rcx       # imm = 0xFFFFFFDF
	andq	%rcx, %rax
	movabsq	$entry_exit_blocks+112, %rcx
	cmpq	%rcx, %rdi
	movl	%eax, 48(%r10)
	je	.LBB2_4
# BB#7:                                 # %land.lhs.true
                                        #   in Loop: Header=BB2_3 Depth=3
	movl	88(%rdi), %r11d
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$6, %rcx
	movq	%r11, %rsi
	shrq	%cl, %rsi
	movq	16(%r12,%rsi,8), %r13
	movq	$1, %r14
	movq	%r11, %rcx
	shlq	%cl, %r14
	testq	%r13, %r14
	jne	.LBB2_11
# BB#8:                                 # %if.then
                                        #   in Loop: Header=BB2_3 Depth=3
	orq	%r13, %r14
	movq	%r14, 16(%r12,%rsi,8)
	movl	%edx, (%r15,%rax,4)
	movq	40(%rdi), %rcx
	incq	%rdx
	testq	%rcx, %rcx
	je	.LBB2_10
# BB#9:                                 # %if.then29
                                        #   in Loop: Header=BB2_3 Depth=3
	movq	%rcx, (%rbx,%r8,8)
	incq	%r8
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r8
	jne	.LBB2_3
	jmp	.LBB2_22
	.align	16, 0x90
.LBB2_10:                               # %if.else
                                        #   in Loop: Header=BB2_2 Depth=2
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r8
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	movl	%esi, (%rcx,%rax,4)
	leaq	1(%rsi), %rsi
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	jne	.LBB2_2
	jmp	.LBB2_22
	.align	16, 0x90
.LBB2_4:                                # %if.else38.thread
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	16(%r10), %rsi
	movabsq	$entry_exit_blocks, %rax
	cmpq	%rax, %rsi
	movabsq	$0, %rax
	movq	%rax, %rdi
	je	.LBB2_6
# BB#5:                                 # %if.else38.thread
                                        #   in Loop: Header=BB2_1 Depth=1
	movabsq	$1, %rax
	movq	%rax, %rdi
.LBB2_6:                                # %if.else38.thread
                                        #   in Loop: Header=BB2_1 Depth=1
	leaq	-1(%r8), %r14
	jmp	.LBB2_17
	.align	16, 0x90
.LBB2_11:                               # %if.else38
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	16(%r10), %rsi
	leaq	-1(%r8), %r14
	movabsq	$entry_exit_blocks, %rcx
	cmpq	%rcx, %rsi
	jne	.LBB2_14
# BB#12:                                # %if.end62.thread
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	8(%r10), %rcx
	jmp	.LBB2_13
.LBB2_14:                               # %land.lhs.true44
                                        #   in Loop: Header=BB2_1 Depth=1
	movslq	88(%rsi), %rcx
	movslq	(%r15,%rcx,4), %r11
	movslq	(%r15,%rax,4), %rcx
	cmpq	%rcx, %r11
	movq	$1, %rdi
	jl	.LBB2_17
# BB#15:                                # %land.lhs.true53
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	movl	(%rcx,%rax,4), %eax
	testq	%rax, %rax
	jne	.LBB2_17
# BB#16:                                # %if.then59
                                        #   in Loop: Header=BB2_1 Depth=1
	orq	$32, %r9
	movl	%r9d, 48(%r10)
	movq	$1, %rdi
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
	.align	16, 0x90
.LBB2_17:                               # %if.end62
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	8(%r10), %rcx
	testq	%rcx, %rcx
	jne	.LBB2_13
# BB#18:                                # %if.end62
                                        #   in Loop: Header=BB2_1 Depth=1
	testq	%rdi, %rdi
	je	.LBB2_13
# BB#19:                                # %if.then67
                                        #   in Loop: Header=BB2_1 Depth=1
	movslq	88(%rsi), %rax
	testq	%rcx, %rcx
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdi          # 8-byte Reload
	movl	%edi, (%rsi,%rax,4)
	leaq	1(%rdi), %rdi
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	je	.LBB2_21
	jmp	.LBB2_20
	.align	16, 0x90
.LBB2_13:                               # %if.end72
                                        #   in Loop: Header=BB2_1 Depth=1
	testq	%rcx, %rcx
	je	.LBB2_21
.LBB2_20:                               # %if.then75
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%rcx, -8(%rbx,%r8,8)
	movq	%r8, %r14
.LBB2_21:                               # %while.cond.outer.backedge
                                        #   in Loop: Header=BB2_1 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	jne	.LBB2_1
.LBB2_22:                               # %while.end
	movq	%r15, %rdi
	callq	free
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	%rbx, %rdi
	callq	free
	movq	%r12, %rdi
	callq	free
	movq	8(%rsp), %rax           # 8-byte Reload
	andq	$1, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp45:
	.size	mark_dfs_back_edges, .Ltmp45-mark_dfs_back_edges
	.cfi_endproc

	.globl	flow_call_edges_add
	.align	16, 0x90
	.type	flow_call_edges_add,@function
flow_call_edges_add:                    # @flow_call_edges_add
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp52:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp53:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp54:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp55:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp56:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp57:
	.cfi_def_cfa_offset 80
.Ltmp58:
	.cfi_offset %rbx, -48
.Ltmp59:
	.cfi_offset %r12, -40
.Ltmp60:
	.cfi_offset %r13, -32
.Ltmp61:
	.cfi_offset %r14, -24
.Ltmp62:
	.cfi_offset %r15, -16
	movslq	n_basic_blocks(%rip), %rax
	movq	%rdi, %rbx
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	$3, %rcx
	shlq	%cl, %rax
	movq	%rax, %rdi
	callq	xmalloc
	movq	%rax, 24(%rsp)          # 8-byte Spill
	testq	%rbx, %rbx
	je	.LBB3_1
# BB#7:                                 # %do.body
	movl	4(%rbx), %eax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%r15, %r15
	testq	%rax, %rax
	je	.LBB3_45
# BB#8:                                 # %for.body8.lr.ph
	movq	basic_block_info(%rip), %r9
	movl	n_basic_blocks(%rip), %r10d
	decq	%r10
	xorq	%r11, %r11
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %r10
	xorq	%r14, %r14
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB3_9:                                # %for.body8
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_11 Depth 2
	movq	16(%rbx,%r11,8), %rsi
	testq	%rsi, %rsi
	je	.LBB3_16
# BB#10:                                # %for.body17.preheader
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	$6, %rcx
	movq	%r11, %r12
	shlq	%cl, %r12
	movq	$1, %rdi
	movq	%rdx, %r13
	.align	16, 0x90
.LBB3_11:                               # %for.body17
                                        #   Parent Loop BB3_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%rdi), %rax
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	testq	%rsi, %rbx
	je	.LBB3_15
# BB#12:                                # %if.then20
                                        #   in Loop: Header=BB3_11 Depth=2
	notq	%rbx
	addq	%r12, %rax
	movq	%rax, %rdx
	andq	%r15, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r14, %r8
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	cmpq	%r10, %rdx
	movq	$1, %rdx
	je	.LBB3_14
# BB#13:                                # %if.then20
                                        #   in Loop: Header=BB3_11 Depth=2
	movq	%r13, %rdx
.LBB3_14:                               # %if.then20
                                        #   in Loop: Header=BB3_11 Depth=2
	movq	32(%r9,%rax,8), %rax
	incq	%r14
	andq	%rbx, %rsi
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx,%r8,8)
	movq	%rdx, %r13
	je	.LBB3_16
.LBB3_15:                               # %for.inc38
                                        #   in Loop: Header=BB3_11 Depth=2
	movq	%rdi, %rax
	andq	%r15, %rax
	incq	%rdi
	cmpq	$64, %rax
	movq	%r13, %rdx
	jb	.LBB3_11
.LBB3_16:                               # %for.inc42
                                        #   in Loop: Header=BB3_9 Depth=1
	incq	%r11
	movq	%r11, %rax
	andq	%r15, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	%rcx, %rax
	movq	16(%rsp), %rbx          # 8-byte Reload
	jne	.LBB3_9
# BB#17:                                # %if.end45
	testq	$1, %rdx
	je	.LBB3_30
# BB#18:                                # %if.end45.if.then47_crit_edge
	movl	n_basic_blocks(%rip), %ebx
	jmp	.LBB3_19
.LBB3_1:                                # %for.cond.preheader
	movslq	n_basic_blocks(%rip), %rbx
	testq	%rbx, %rbx
	jle	.LBB3_2
# BB#3:                                 # %for.body.lr.ph
	movq	basic_block_info(%rip), %rsi
	cmpq	$1, %rbx
	jg	.LBB3_4
# BB#5:                                 # %for.body.lr.ph
	xorq	%r14, %r14
	jmp	.LBB3_6
.LBB3_2:
	xorq	%r14, %r14
	jmp	.LBB3_19
.LBB3_4:
	leaq	-1(%rbx), %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%rax, %r14
.LBB3_6:                                # %for.body.lr.ph
	addq	$32, %rsi
	leaq	8(,%r14,8), %rdx
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	memcpy
	incq	%r14
.LBB3_19:                               # %if.then47
	decq	%rbx
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rax
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	32(%rax,%rbx,8), %r15
	movq	(%r15), %rax
	movq	8(%r15), %rbx
	jmp	.LBB3_20
	.align	16, 0x90
.LBB3_22:                               # %while.body
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	16(%rbx), %rbx
	movq	(%r15), %rax
.LBB3_20:                               # %if.then47
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rax, %rbx
	je	.LBB3_23
# BB#21:                                # %land.rhs
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	%rbx, %rdi
	callq	keep_with_call_p
	testq	%rax, %rax
	jne	.LBB3_22
.LBB3_23:                               # %while.end
	movq	%rbx, %rdi
	callq	need_fake_edge_p
	testq	%rax, %rax
	je	.LBB3_30
# BB#24:                                # %if.then60
	movq	40(%r15), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB3_29
# BB#25:
	movabsq	$entry_exit_blocks+112, %rcx
	.align	16, 0x90
.LBB3_27:                               # %for.body63
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rax), %rdx
	cmpq	%rcx, %rdx
	je	.LBB3_28
# BB#26:                                # %for.cond61
                                        #   in Loop: Header=BB3_27 Depth=1
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB3_27
	jmp	.LBB3_29
.LBB3_28:
	movq	%rax, %rbx
.LBB3_29:                               # %for.end69
	movq	const_int_rtx+512(%rip), %rdx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	insert_insn_on_edge
	callq	commit_edge_insertions
.LBB3_30:                               # %for.cond73.preheader
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%r15, %r15
	testq	%rax, %rax
	jle	.LBB3_45
# BB#31:
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	.align	16, 0x90
.LBB3_32:                               # %for.body76
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_33 Depth 2
                                        #       Child Loop BB3_36 Depth 3
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%rsi,8), %r13
	movq	8(%r13), %rbx
	.align	16, 0x90
.LBB3_33:                               # %for.cond82
                                        #   Parent Loop BB3_32 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_36 Depth 3
	movq	16(%rbx), %r12
	movq	%rbx, %rdi
	callq	need_fake_edge_p
	testq	%rax, %rax
	je	.LBB3_41
# BB#34:                                # %if.then87
                                        #   in Loop: Header=BB3_33 Depth=2
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	movq	%rbx, %r15
	jne	.LBB3_38
# BB#35:                                # %while.cond92.preheader
                                        #   in Loop: Header=BB3_33 Depth=2
	movq	8(%r13), %rax
	cmpq	%rax, %rbx
	movq	%rbx, %r15
	je	.LBB3_38
	.align	16, 0x90
.LBB3_36:                               # %land.rhs96
                                        #   Parent Loop BB3_32 Depth=1
                                        #     Parent Loop BB3_33 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	24(%r15), %rdi
	callq	keep_with_call_p
	testq	%rax, %rax
	je	.LBB3_38
# BB#37:                                # %while.body103
                                        #   in Loop: Header=BB3_36 Depth=3
	movq	24(%r15), %r15
	movq	8(%r13), %rax
	cmpq	%rax, %r15
	jne	.LBB3_36
	.align	16, 0x90
.LBB3_38:                               # %if.end108
                                        #   in Loop: Header=BB3_33 Depth=2
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	split_block
	testq	%rax, %rax
	movabsq	$0, %rax
	je	.LBB3_40
# BB#39:                                # %if.end108
                                        #   in Loop: Header=BB3_33 Depth=2
	movabsq	$1, %rax
.LBB3_40:                               # %if.end108
                                        #   in Loop: Header=BB3_33 Depth=2
	addq	%rax, %r14
	movq	$16, %rdx
	movq	%r13, %rdi
	movabsq	$entry_exit_blocks+112, %rsi
	callq	make_edge
.LBB3_41:                               # %if.end115
                                        #   in Loop: Header=BB3_33 Depth=2
	movq	(%r13), %rax
	cmpq	%rax, %rbx
	movq	%r12, %rbx
	jne	.LBB3_33
# BB#42:                                # %for.inc123
                                        #   in Loop: Header=BB3_32 Depth=1
	movq	16(%rsp), %rsi          # 8-byte Reload
	incq	%rsi
	movq	%rsi, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	andq	%rdx, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jne	.LBB3_32
# BB#43:                                # %for.end125
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	movabsq	$0, %r15
	je	.LBB3_45
# BB#44:                                # %if.then127
	callq	verify_flow_info
	movq	%r14, %r15
.LBB3_45:                               # %if.end128
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	%r15, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp63:
	.size	flow_call_edges_add, .Ltmp63-flow_call_edges_add
	.cfi_endproc

	.align	16, 0x90
	.type	keep_with_call_p,@function
keep_with_call_p:                       # @keep_with_call_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp66:
	.cfi_def_cfa_offset 16
.Ltmp67:
	.cfi_offset %rbx, -16
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB4_11
# BB#1:                                 # %cond.true
	movq	32(%rdi), %rbx
	movzbl	(%rbx), %eax
	cmpq	$47, %rax
	je	.LBB4_3
# BB#2:                                 # %cond.false
	movq	%rbx, %rsi
	callq	single_set_2
	movq	%rax, %rbx
.LBB4_3:                                # %cond.end22
	testq	%rbx, %rbx
	je	.LBB4_11
# BB#4:                                 # %if.then
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB4_7
# BB#5:                                 # %land.lhs.true33
	movl	8(%rax), %eax
	movzbl	fixed_regs(%rax), %eax
	testq	%rax, %rax
	je	.LBB4_7
# BB#6:                                 # %land.lhs.true42
	movq	16(%rbx), %rdi
	xorq	%rsi, %rsi
	callq	general_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rax
	jne	.LBB4_12
.LBB4_7:                                # %if.end
	movq	16(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB4_11
# BB#8:                                 # %land.lhs.true56
	movl	8(%rax), %edi
	callq	ix86_function_value_regno_p
	testq	%rax, %rax
	je	.LBB4_11
# BB#9:                                 # %land.lhs.true65
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB4_11
# BB#10:                                # %land.lhs.true73
	movl	8(%rax), %eax
	cmpq	$52, %rax
	movq	$1, %rax
	ja	.LBB4_12
.LBB4_11:                               # %if.end84
	xorq	%rax, %rax
.LBB4_12:                               # %return
	popq	%rbx
	retq
.Ltmp68:
	.size	keep_with_call_p, .Ltmp68-keep_with_call_p
	.cfi_endproc

	.align	16, 0x90
	.type	need_fake_edge_p,@function
need_fake_edge_p:                       # @need_fake_edge_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp72:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp73:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp74:
	.cfi_def_cfa_offset 32
.Ltmp75:
	.cfi_offset %rbx, -24
.Ltmp76:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	xorq	%rbx, %rbx
	cmpq	$105, %rcx
	jne	.LBB5_12
# BB#1:                                 # %if.end
	andq	$16842751, %rax         # imm = 0x100FFFF
	cmpq	$34, %rax
	jne	.LBB5_5
# BB#2:                                 # %land.lhs.true8
	movq	$26, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB5_5
# BB#3:                                 # %land.lhs.true10
	movq	$29, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB5_5
# BB#4:                                 # %land.lhs.true13
	movzbl	3(%r14), %eax
	movq	$1, %rbx
	testq	$4, %rax
	je	.LBB5_12
.LBB5_5:                                # %if.end19
	movq	32(%r14), %rcx
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$134283263, %rdx        # imm = 0x800FFFF
	cmpq	$134217769, %rdx        # imm = 0x8000029
	movq	$1, %rbx
	je	.LBB5_12
# BB#6:                                 # %lor.lhs.false
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$39, %rax
	jne	.LBB5_9
# BB#7:                                 # %land.lhs.true40
	movq	%r14, %rdi
	callq	asm_noperands
	movq	32(%r14), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	%rdx, %rax
	je	.LBB5_9
# BB#8:                                 # %land.lhs.true44
	movq	8(%rcx), %rax
	movq	8(%rax), %rax
	movzbl	3(%rax), %eax
	testq	$8, %rax
	jne	.LBB5_12
.LBB5_9:                                # %lor.rhs
	movzbl	(%rcx), %eax
	cmpq	$40, %rax
	jne	.LBB5_10
# BB#11:                                # %lor.rhs
	movabsq	$1, %rbx
	jmp	.LBB5_12
.LBB5_10:
	movabsq	$0, %rbx
.LBB5_12:                               # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp77:
	.size	need_fake_edge_p, .Ltmp77-need_fake_edge_p
	.cfi_endproc

	.globl	find_unreachable_blocks
	.align	16, 0x90
	.type	find_unreachable_blocks,@function
find_unreachable_blocks:                # @find_unreachable_blocks
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp80:
	.cfi_def_cfa_offset 16
.Ltmp81:
	.cfi_offset %rbx, -16
	movslq	n_basic_blocks(%rip), %rbx
	movq	$3, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	callq	xmalloc
	testq	%rbx, %rbx
	jle	.LBB6_3
# BB#1:                                 # %for.body.lr.ph
	movq	$4, %rcx
	movq	basic_block_info(%rip), %rdx
	movabsq	$4294967294, %r8        # imm = 0xFFFFFFFE
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	andq	%r9, %rbx
	.align	16, 0x90
.LBB6_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdx,%rcx,8), %rsi
	movl	108(%rsi), %edi
	andq	%r8, %rdi
	movl	%edi, 108(%rsi)
	leaq	-3(%rcx), %rsi
	andq	%r9, %rsi
	incq	%rcx
	cmpq	%rbx, %rsi
	jne	.LBB6_2
.LBB6_3:                                # %for.cond2.preheader
	movq	entry_exit_blocks+40(%rip), %rdx
	testq	%rdx, %rdx
	je	.LBB6_14
# BB#4:
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB6_5:                                # %for.body3
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rdx), %rsi
	movq	%rsi, (%rax,%rcx)
	movq	24(%rdx), %rsi
	movl	108(%rsi), %edi
	orq	$1, %rdi
	movl	%edi, 108(%rsi)
	movq	8(%rdx), %rdx
	addq	$8, %rcx
	testq	%rdx, %rdx
	jne	.LBB6_5
# BB#6:                                 # %while.cond.preheader
	testq	%rcx, %rcx
	je	.LBB6_14
# BB#7:
	addq	%rax, %rcx
	.align	16, 0x90
.LBB6_9:                                # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_13 Depth 2
	movq	-8(%rcx), %rdx
	movq	40(%rdx), %rdx
	addq	$-8, %rcx
	jmp	.LBB6_13
	.align	16, 0x90
.LBB6_12:                               # %for.inc23
                                        #   in Loop: Header=BB6_13 Depth=2
	movq	8(%rdx), %rdx
.LBB6_13:                               # %for.inc23
                                        #   Parent Loop BB6_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rdx, %rdx
	je	.LBB6_8
# BB#10:                                # %for.body13
                                        #   in Loop: Header=BB6_13 Depth=2
	movq	24(%rdx), %rsi
	movzbl	108(%rsi), %edi
	testq	$1, %rdi
	jne	.LBB6_12
# BB#11:                                # %if.then
                                        #   in Loop: Header=BB6_13 Depth=2
	movq	%rsi, (%rcx)
	movq	24(%rdx), %rsi
	movl	108(%rsi), %edi
	addq	$8, %rcx
	orq	$1, %rdi
	movl	%edi, 108(%rsi)
	jmp	.LBB6_12
	.align	16, 0x90
.LBB6_8:                                # %while.cond.loopexit
                                        #   in Loop: Header=BB6_9 Depth=1
	cmpq	%rax, %rcx
	jne	.LBB6_9
.LBB6_14:                               # %while.end
	movq	%rax, %rdi
	popq	%rbx
	jmp	free  # TAILCALL
.Ltmp82:
	.size	find_unreachable_blocks, .Ltmp82-find_unreachable_blocks
	.cfi_endproc

	.globl	create_edge_list
	.align	16, 0x90
	.type	create_edge_list,@function
create_edge_list:                       # @create_edge_list
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp87:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp88:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp89:
	.cfi_def_cfa_offset 32
.Ltmp90:
	.cfi_offset %rbx, -32
.Ltmp91:
	.cfi_offset %r14, -24
.Ltmp92:
	.cfi_offset %r15, -16
	movslq	n_basic_blocks(%rip), %r15
	xorq	%rbx, %rbx
	testq	%r15, %r15
	jle	.LBB7_17
# BB#1:                                 # %for.body.lr.ph
	movq	basic_block_info(%rip), %rax
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB7_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_15 Depth 2
	movq	32(%rax,%rdx,8), %rcx
	movq	40(%rcx), %rcx
	jmp	.LBB7_15
	.align	16, 0x90
.LBB7_14:                               # %for.body3
                                        #   in Loop: Header=BB7_15 Depth=2
	movq	8(%rcx), %rcx
	incq	%rbx
.LBB7_15:                               # %for.body3
                                        #   Parent Loop BB7_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rcx, %rcx
	jne	.LBB7_14
# BB#16:                                # %for.inc4
                                        #   in Loop: Header=BB7_2 Depth=1
	incq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%r15, %rsi
	jl	.LBB7_2
.LBB7_17:                               # %for.cond7.preheader
	movq	entry_exit_blocks+40(%rip), %rax
	jmp	.LBB7_4
	.align	16, 0x90
.LBB7_3:                                # %for.body9
                                        #   in Loop: Header=BB7_4 Depth=1
	movq	8(%rax), %rax
	incq	%rbx
.LBB7_4:                                # %for.body9
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB7_3
# BB#5:                                 # %for.end13
	addq	$2, %r15
	movq	$16, %rdi
	callq	xmalloc
	movq	%rax, %r14
	movl	%r15d, (%r14)
	movl	%ebx, 4(%r14)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	%rbx, %rdi
	callq	xmalloc
	movq	entry_exit_blocks+40(%rip), %rcx
	movq	%rax, 8(%r14)
	xorq	%rdx, %rdx
	jmp	.LBB7_6
	.align	16, 0x90
.LBB7_18:                               # %for.body18
                                        #   in Loop: Header=BB7_6 Depth=1
	movq	%rcx, (%rax,%rdx,8)
	movq	8(%rcx), %rcx
	incq	%rdx
.LBB7_6:                                # %for.end13
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	jne	.LBB7_18
# BB#7:                                 # %for.cond26.preheader
	movslq	n_basic_blocks(%rip), %r8
	testq	%r8, %r8
	jle	.LBB7_13
# BB#8:
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB7_9:                                # %for.body29
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_11 Depth 2
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rsi,8), %rcx
	movq	40(%rcx), %rbx
	testq	%rbx, %rbx
	je	.LBB7_12
# BB#10:                                # %for.body38.lr.ph
                                        #   in Loop: Header=BB7_9 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	.align	16, 0x90
.LBB7_11:                               # %for.body38
                                        #   Parent Loop BB7_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, (%rax,%rdx,8)
	movq	8(%rbx), %rbx
	incq	%rdx
	testq	%rbx, %rbx
	jne	.LBB7_11
.LBB7_12:                               # %for.inc46
                                        #   in Loop: Header=BB7_9 Depth=1
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%r8, %rdi
	jl	.LBB7_9
.LBB7_13:                               # %for.end48
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp93:
	.size	create_edge_list, .Ltmp93-create_edge_list
	.cfi_endproc

	.globl	free_edge_list
	.align	16, 0x90
	.type	free_edge_list,@function
free_edge_list:                         # @free_edge_list
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp96:
	.cfi_def_cfa_offset 16
.Ltmp97:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB8_1
# BB#2:                                 # %if.then
	movq	8(%rbx), %rdi
	callq	free
	movq	%rbx, %rdi
	popq	%rbx
	jmp	free  # TAILCALL
.LBB8_1:                                # %if.end
	popq	%rbx
	retq
.Ltmp98:
	.size	free_edge_list, .Ltmp98-free_edge_list
	.cfi_endproc

	.globl	print_edge_list
	.align	16, 0x90
	.type	print_edge_list,@function
print_edge_list:                        # @print_edge_list
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp105:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp106:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp107:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp108:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp109:
	.cfi_def_cfa_offset 48
.Ltmp110:
	.cfi_offset %rbx, -48
.Ltmp111:
	.cfi_offset %r12, -40
.Ltmp112:
	.cfi_offset %r13, -32
.Ltmp113:
	.cfi_offset %r14, -24
.Ltmp114:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movl	(%r14), %edx
	movl	4(%r14), %ecx
	movq	%rdi, %r15
	addq	$-2, %rdx
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
                                        # kill: RDI<def> R15<kill>
	callq	fprintf
	movslq	4(%r14), %rax
	testq	%rax, %rax
	jle	.LBB9_9
# BB#1:                                 # %for.body.lr.ph
	movabsq	$.L.str1, %r12
	movabsq	$entry_exit_blocks+112, %r13
	.align	16, 0x90
.LBB9_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movq	8(%r14), %rax
	movq	(%rax,%rbx,8), %rax
	movq	16(%rax), %rax
	movabsq	$entry_exit_blocks, %rcx
	cmpq	%rcx, %rax
	jne	.LBB9_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB9_2 Depth=1
	movq	$6, %rsi
	movq	$1, %rdx
	movabsq	$.L.str2, %rdi
	movq	%r15, %rcx
	callq	fwrite
	jmp	.LBB9_5
	.align	16, 0x90
.LBB9_4:                                # %if.else
                                        #   in Loop: Header=BB9_2 Depth=1
	movl	88(%rax), %edx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str3, %rsi
	callq	fprintf
.LBB9_5:                                # %if.end
                                        #   in Loop: Header=BB9_2 Depth=1
	movq	8(%r14), %rax
	movq	(%rax,%rbx,8), %rax
	movq	24(%rax), %rax
	cmpq	%r13, %rax
	jne	.LBB9_7
# BB#6:                                 # %if.then14
                                        #   in Loop: Header=BB9_2 Depth=1
	movq	$6, %rsi
	movq	$1, %rdx
	movabsq	$.L.str4, %rdi
	movq	%r15, %rcx
	callq	fwrite
	jmp	.LBB9_8
	.align	16, 0x90
.LBB9_7:                                # %if.else16
                                        #   in Loop: Header=BB9_2 Depth=1
	movl	88(%rax), %edx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str5, %rsi
	callq	fprintf
.LBB9_8:                                # %for.inc
                                        #   in Loop: Header=BB9_2 Depth=1
	incq	%rbx
	movslq	4(%r14), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB9_2
.LBB9_9:                                # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp115:
	.size	print_edge_list, .Ltmp115-print_edge_list
	.cfi_endproc

	.globl	verify_edge_list
	.align	16, 0x90
	.type	verify_edge_list,@function
verify_edge_list:                       # @verify_edge_list
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
	movslq	n_basic_blocks(%rip), %rax
	movq	%rsi, %r10
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	testq	%rax, %rax
	jle	.LBB10_10
# BB#1:                                 # %for.body.lr.ph
	xorq	%rsi, %rsi
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB10_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB10_3 Depth 2
                                        #       Child Loop BB10_6 Depth 3
	movq	%rsi, (%rsp)            # 8-byte Spill
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rsi,8), %rax
	movq	40(%rax), %r14
	jmp	.LBB10_3
	.align	16, 0x90
.LBB10_24:                              # %for.inc
                                        #   in Loop: Header=BB10_3 Depth=2
	movq	8(%r14), %r14
.LBB10_3:                               # %for.body
                                        #   Parent Loop BB10_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB10_6 Depth 3
	testq	%r14, %r14
	je	.LBB10_9
# BB#4:                                 # %for.body4
                                        #   in Loop: Header=BB10_3 Depth=2
	movq	16(%r14), %rax
	movq	24(%r14), %rdx
	movl	88(%rax), %r9d
	movslq	4(%r10), %rdi
	movl	88(%rdx), %r12d
	testq	%rdi, %rdi
	jle	.LBB10_18
# BB#5:                                 # %for.body.lr.ph.i
                                        #   in Loop: Header=BB10_3 Depth=2
	movq	8(%r10), %rsi
	xorq	%r13, %r13
	.align	16, 0x90
.LBB10_6:                               # %for.body.i
                                        #   Parent Loop BB10_2 Depth=1
                                        #     Parent Loop BB10_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rsi,%r13,8), %rcx
	movq	16(%rcx), %rbx
	cmpq	%rax, %rbx
	jne	.LBB10_8
# BB#7:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB10_6 Depth=3
	movq	24(%rcx), %rcx
	cmpq	%rdx, %rcx
	je	.LBB10_17
.LBB10_8:                               # %for.inc.i
                                        #   in Loop: Header=BB10_6 Depth=3
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rdi, %rbx
	jl	.LBB10_6
	jmp	.LBB10_18
.LBB10_17:                              # %find_edge_index.exit
                                        #   in Loop: Header=BB10_3 Depth=2
	movq	%r13, %rax
	andq	%r15, %rax
	cmpq	%r15, %rax
	jne	.LBB10_19
	.align	16, 0x90
.LBB10_18:                              # %if.then
                                        #   in Loop: Header=BB10_3 Depth=2
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	movabsq	$.L.str6, %rsi
	movq	%r9, %rdx
	movq	%r12, %rcx
	movq	%r10, %rbx
	callq	fprintf
.LBB10_23:                              # %for.inc
                                        #   in Loop: Header=BB10_3 Depth=2
	movq	%rbx, %r10
	jmp	.LBB10_24
.LBB10_19:                              # %if.end
                                        #   in Loop: Header=BB10_3 Depth=2
	movq	(%rsi,%r13,8), %rax
	movq	16(%rax), %rcx
	movl	88(%rcx), %r8d
	movq	%r9, %rcx
	andq	%r15, %rcx
	cmpq	%rcx, %r8
	je	.LBB10_21
# BB#20:                                # %if.then16
                                        #   in Loop: Header=BB10_3 Depth=2
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	movabsq	$.L.str7, %rsi
	movq	%r13, %rdx
	movq	%r9, %rcx
	movq	%r10, %rbx
	callq	fprintf
	movq	%rbx, %r10
	movq	8(%r10), %rax
	movq	(%rax,%r13,8), %rax
.LBB10_21:                              # %if.end23
                                        #   in Loop: Header=BB10_3 Depth=2
	movq	24(%rax), %rax
	movl	88(%rax), %r8d
	movq	%r12, %rax
	andq	%r15, %rax
	cmpq	%rax, %r8
	je	.LBB10_24
# BB#22:                                # %if.then30
                                        #   in Loop: Header=BB10_3 Depth=2
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	movabsq	$.L.str8, %rsi
	movq	%r13, %rdx
	movq	%r12, %rcx
	movq	%r10, %rbx
	callq	fprintf
	jmp	.LBB10_23
	.align	16, 0x90
.LBB10_9:                               # %for.inc38
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	(%rsp), %rsi            # 8-byte Reload
	incq	%rsi
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB10_2
.LBB10_10:                              # %for.cond40.preheader
	movq	entry_exit_blocks+40(%rip), %r14
	testq	%r14, %r14
	je	.LBB10_26
# BB#11:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB10_12
.LBB10_33:                              # %if.end53
                                        #   in Loop: Header=BB10_12 Depth=1
	movq	(%rsi,%r13,8), %rax
	movq	16(%rax), %rcx
	movl	88(%rcx), %r8d
	movq	%r9, %rcx
	andq	%r15, %rcx
	cmpq	%rcx, %r8
	je	.LBB10_35
# BB#34:                                # %if.then60
                                        #   in Loop: Header=BB10_12 Depth=1
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	movabsq	$.L.str7, %rsi
	movq	%r13, %rdx
	movq	%r9, %rcx
	movq	%r10, %rbx
	callq	fprintf
	movq	%rbx, %r10
	movq	8(%r10), %rax
	movq	(%rax,%r13,8), %rax
.LBB10_35:                              # %if.end67
                                        #   in Loop: Header=BB10_12 Depth=1
	movq	24(%rax), %rax
	movl	88(%rax), %r8d
	movq	%r12, %rax
	andq	%r15, %rax
	cmpq	%rax, %r8
	je	.LBB10_38
# BB#36:                                # %if.then74
                                        #   in Loop: Header=BB10_12 Depth=1
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	movabsq	$.L.str8, %rsi
	movq	%r13, %rdx
	movq	%r12, %rcx
	movq	%r10, %rbx
	callq	fprintf
	jmp	.LBB10_37
	.align	16, 0x90
.LBB10_12:                              # %for.body42
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB10_14 Depth 2
	movq	16(%r14), %rax
	movq	24(%r14), %rdx
	movl	88(%rax), %r9d
	movslq	4(%r10), %rdi
	movl	88(%rdx), %r12d
	testq	%rdi, %rdi
	jle	.LBB10_32
# BB#13:                                # %for.body.lr.ph.i581
                                        #   in Loop: Header=BB10_12 Depth=1
	movq	8(%r10), %rsi
	xorq	%r13, %r13
	.align	16, 0x90
.LBB10_14:                              # %for.body.i586
                                        #   Parent Loop BB10_12 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rsi,%r13,8), %rcx
	movq	16(%rcx), %rbx
	cmpq	%rax, %rbx
	jne	.LBB10_16
# BB#15:                                # %land.lhs.true.i589
                                        #   in Loop: Header=BB10_14 Depth=2
	movq	24(%rcx), %rcx
	cmpq	%rdx, %rcx
	je	.LBB10_31
.LBB10_16:                              # %for.inc.i592
                                        #   in Loop: Header=BB10_14 Depth=2
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rdi, %rbx
	jl	.LBB10_14
	jmp	.LBB10_32
.LBB10_31:                              # %find_edge_index.exit595
                                        #   in Loop: Header=BB10_12 Depth=1
	movq	%r13, %rax
	andq	%r15, %rax
	cmpq	%r15, %rax
	jne	.LBB10_33
	.align	16, 0x90
.LBB10_32:                              # %if.then51
                                        #   in Loop: Header=BB10_12 Depth=1
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	movabsq	$.L.str6, %rsi
	movq	%r9, %rdx
	movq	%r12, %rcx
	movq	%r10, %rbx
	callq	fprintf
.LBB10_37:                              # %for.inc82
                                        #   in Loop: Header=BB10_12 Depth=1
	movq	%rbx, %r10
.LBB10_38:                              # %for.inc82
                                        #   in Loop: Header=BB10_12 Depth=1
	movq	8(%r14), %r14
	testq	%r14, %r14
	jne	.LBB10_12
# BB#25:                                # %for.cond40.for.cond85.preheader_crit_edge
	movl	n_basic_blocks(%rip), %eax
.LBB10_26:                              # %for.cond85.preheader
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB10_141
# BB#27:                                # %for.cond88.preheader.lr.ph
	xorq	%r14, %r14
	.align	16, 0x90
.LBB10_28:                              # %for.cond88.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB10_29 Depth 2
                                        #       Child Loop BB10_49 Depth 3
                                        #       Child Loop BB10_54 Depth 3
                                        #       Child Loop BB10_57 Depth 3
                                        #       Child Loop BB10_67 Depth 3
                                        #       Child Loop BB10_72 Depth 3
	movq	%r10, %r15
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movabsq	$0, %r13
	jle	.LBB10_78
	.align	16, 0x90
.LBB10_29:                              # %for.body90
                                        #   Parent Loop BB10_28 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB10_49 Depth 3
                                        #       Child Loop BB10_54 Depth 3
                                        #       Child Loop BB10_57 Depth 3
                                        #       Child Loop BB10_67 Depth 3
                                        #       Child Loop BB10_72 Depth 3
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%r14,8), %r10
	movq	40(%r10), %rsi
	movq	32(%rcx,%r13,8), %r9
	testq	%rsi, %rsi
	jne	.LBB10_49
# BB#30:                                #   in Loop: Header=BB10_29 Depth=2
	xorq	%rcx, %rcx
	jmp	.LBB10_50
	.align	16, 0x90
.LBB10_49:                              # %for.body102
                                        #   Parent Loop BB10_28 Depth=1
                                        #     Parent Loop BB10_29 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	24(%rsi), %rcx
	cmpq	%r9, %rcx
	movq	$1, %rcx
	je	.LBB10_50
# BB#47:                                # %for.cond100
                                        #   in Loop: Header=BB10_49 Depth=3
	movq	8(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB10_49
# BB#48:                                #   in Loop: Header=BB10_29 Depth=2
	xorq	%rcx, %rcx
.LBB10_50:                              # %for.end109
                                        #   in Loop: Header=BB10_29 Depth=2
	movq	32(%r9), %rsi
	testq	%rsi, %rsi
	jne	.LBB10_54
# BB#51:                                #   in Loop: Header=BB10_29 Depth=2
	movq	%rcx, %r12
	jmp	.LBB10_55
	.align	16, 0x90
.LBB10_54:                              # %for.body113
                                        #   Parent Loop BB10_28 Depth=1
                                        #     Parent Loop BB10_29 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	16(%rsi), %rdi
	cmpq	%r10, %rdi
	movq	$1, %r12
	je	.LBB10_55
# BB#52:                                # %for.cond111
                                        #   in Loop: Header=BB10_54 Depth=3
	movq	(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB10_54
# BB#53:                                #   in Loop: Header=BB10_29 Depth=2
	movq	%rcx, %r12
.LBB10_55:                              # %for.end119
                                        #   in Loop: Header=BB10_29 Depth=2
	movslq	4(%r15), %rax
	testq	%rax, %rax
	movq	$1, %r8
	jle	.LBB10_62
# BB#56:                                # %for.body.lr.ph.i563
                                        #   in Loop: Header=BB10_29 Depth=2
	movq	8(%r15), %rdi
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB10_57:                              # %for.body.i568
                                        #   Parent Loop BB10_28 Depth=1
                                        #     Parent Loop BB10_29 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rdi,%rbx,8), %rcx
	movq	16(%rcx), %rdx
	cmpq	%r10, %rdx
	jne	.LBB10_59
# BB#58:                                # %land.lhs.true.i571
                                        #   in Loop: Header=BB10_57 Depth=3
	movq	24(%rcx), %rcx
	cmpq	%r9, %rcx
	je	.LBB10_60
.LBB10_59:                              # %for.inc.i574
                                        #   in Loop: Header=BB10_57 Depth=3
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB10_57
	jmp	.LBB10_62
.LBB10_60:                              # %land.lhs.true.return_crit_edge.i575
                                        #   in Loop: Header=BB10_29 Depth=2
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rbx
	cmpq	%rcx, %rbx
	movabsq	$0, %r8
	jne	.LBB10_62
# BB#61:                                # %land.lhs.true.return_crit_edge.i575
                                        #   in Loop: Header=BB10_29 Depth=2
	movabsq	$1, %r8
	.align	16, 0x90
.LBB10_62:                              # %find_edge_index.exit577
                                        #   in Loop: Header=BB10_29 Depth=2
	testq	%r8, %r8
	je	.LBB10_65
# BB#63:                                # %find_edge_index.exit577
                                        #   in Loop: Header=BB10_29 Depth=2
	testq	%r12, %r12
	je	.LBB10_65
# BB#64:                                # %if.then131
                                        #   in Loop: Header=BB10_29 Depth=2
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	movabsq	$.L.str9, %rsi
	movq	%r14, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%r14,8), %r10
	movq	32(%rcx,%r13,8), %r9
	movl	4(%r15), %eax
.LBB10_65:                              # %if.end133
                                        #   in Loop: Header=BB10_29 Depth=2
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB10_77
# BB#66:                                # %for.body.lr.ph.i545
                                        #   in Loop: Header=BB10_29 Depth=2
	movq	8(%r15), %rdi
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB10_67:                              # %for.body.i550
                                        #   Parent Loop BB10_28 Depth=1
                                        #     Parent Loop BB10_29 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rdi,%rbx,8), %rcx
	movq	16(%rcx), %rdx
	cmpq	%r10, %rdx
	jne	.LBB10_69
# BB#68:                                # %land.lhs.true.i553
                                        #   in Loop: Header=BB10_67 Depth=3
	movq	24(%rcx), %rcx
	cmpq	%r9, %rcx
	je	.LBB10_70
.LBB10_69:                              # %for.inc.i556
                                        #   in Loop: Header=BB10_67 Depth=3
	incq	%rbx
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
	cmpq	%rsi, %rdx
	jl	.LBB10_67
	jmp	.LBB10_77
.LBB10_70:                              # %find_edge_index.exit559
                                        #   in Loop: Header=BB10_29 Depth=2
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rbx
	cmpq	%rcx, %rbx
	je	.LBB10_77
# BB#71:                                # %find_edge_index.exit559
                                        #   in Loop: Header=BB10_29 Depth=2
	xorq	%r8, %r8
	testq	%r12, %r12
	jne	.LBB10_77
	.align	16, 0x90
.LBB10_72:                              # %for.body.i532
                                        #   Parent Loop BB10_28 Depth=1
                                        #     Parent Loop BB10_29 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rdi,%r8,8), %rcx
	movq	16(%rcx), %rdx
	cmpq	%r10, %rdx
	jne	.LBB10_74
# BB#73:                                # %land.lhs.true.i535
                                        #   in Loop: Header=BB10_72 Depth=3
	movq	24(%rcx), %rcx
	cmpq	%r9, %rcx
	je	.LBB10_76
.LBB10_74:                              # %for.inc.i538
                                        #   in Loop: Header=BB10_72 Depth=3
	incq	%r8
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB10_72
# BB#75:                                #   in Loop: Header=BB10_29 Depth=2
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
.LBB10_76:                              # %find_edge_index.exit541
                                        #   in Loop: Header=BB10_29 Depth=2
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	movabsq	$.L.str10, %rsi
	movq	%r14, %rdx
	movq	%r13, %rcx
	callq	fprintf
	.align	16, 0x90
.LBB10_77:                              # %for.inc158
                                        #   in Loop: Header=BB10_29 Depth=2
	incq	%r13
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB10_29
.LBB10_78:                              # %for.inc161
                                        #   in Loop: Header=BB10_28 Depth=1
	incq	%r14
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
	cmpq	%rsi, %rdx
	movq	%r15, %r10
	jl	.LBB10_28
# BB#39:                                # %for.cond164.preheader
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB10_141
# BB#40:                                # %for.body166.lr.ph
	xorq	%r14, %r14
	movabsq	$entry_exit_blocks, %r13
	.align	16, 0x90
.LBB10_41:                              # %for.body166
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB10_42 Depth 2
                                        #     Child Loop BB10_89 Depth 2
                                        #     Child Loop BB10_93 Depth 2
                                        #     Child Loop BB10_103 Depth 2
                                        #     Child Loop BB10_108 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	entry_exit_blocks+40(%rip), %rdx
	movq	32(%rax,%r14,8), %r9
	testq	%rdx, %rdx
	movabsq	$0, %rcx
	je	.LBB10_45
	.align	16, 0x90
.LBB10_42:                              # %for.body177
                                        #   Parent Loop BB10_41 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rdx), %rcx
	cmpq	%r9, %rcx
	movq	$1, %rcx
	je	.LBB10_45
# BB#43:                                # %for.cond175
                                        #   in Loop: Header=BB10_42 Depth=2
	movq	8(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB10_42
# BB#44:                                #   in Loop: Header=BB10_41 Depth=1
	xorq	%rcx, %rcx
.LBB10_45:                              # %for.end184
                                        #   in Loop: Header=BB10_41 Depth=1
	movq	32(%r9), %rdx
	testq	%rdx, %rdx
	jne	.LBB10_89
# BB#46:                                #   in Loop: Header=BB10_41 Depth=1
	movq	%rcx, %r12
	jmp	.LBB10_90
	.align	16, 0x90
.LBB10_89:                              # %for.body188
                                        #   Parent Loop BB10_41 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	16(%rdx), %rsi
	cmpq	%r13, %rsi
	movq	$1, %r12
	je	.LBB10_90
# BB#87:                                # %for.cond186
                                        #   in Loop: Header=BB10_89 Depth=2
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB10_89
# BB#88:                                #   in Loop: Header=BB10_41 Depth=1
	movq	%rcx, %r12
.LBB10_90:                              # %for.end195
                                        #   in Loop: Header=BB10_41 Depth=1
	movq	%r10, %rax
	movslq	4(%rax), %r10
	testq	%r10, %r10
	movq	$1, %r8
	jle	.LBB10_91
# BB#92:                                # %for.body.lr.ph.i509
                                        #   in Loop: Header=BB10_41 Depth=1
	movq	8(%rax), %rbx
	movq	%rax, %r15
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB10_93:                              # %for.body.i514
                                        #   Parent Loop BB10_41 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rbx,%rdi,8), %rcx
	movq	16(%rcx), %rsi
	cmpq	%r13, %rsi
	jne	.LBB10_95
# BB#94:                                # %land.lhs.true.i517
                                        #   in Loop: Header=BB10_93 Depth=2
	movq	24(%rcx), %rcx
	cmpq	%r9, %rcx
	je	.LBB10_96
.LBB10_95:                              # %for.inc.i520
                                        #   in Loop: Header=BB10_93 Depth=2
	incq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%r10, %rsi
	jl	.LBB10_93
	jmp	.LBB10_98
	.align	16, 0x90
.LBB10_91:                              #   in Loop: Header=BB10_41 Depth=1
	movq	%rax, %r15
.LBB10_98:                              # %find_edge_index.exit523
                                        #   in Loop: Header=BB10_41 Depth=1
	testq	%r8, %r8
	je	.LBB10_101
# BB#99:                                # %find_edge_index.exit523
                                        #   in Loop: Header=BB10_41 Depth=1
	testq	%r12, %r12
	je	.LBB10_101
# BB#100:                               # %if.then204
                                        #   in Loop: Header=BB10_41 Depth=1
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	movabsq	$.L.str11, %rsi
	movq	%r14, %rdx
	callq	fprintf
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r14,8), %r9
	movl	4(%r15), %r10d
.LBB10_101:                             # %if.end206
                                        #   in Loop: Header=BB10_41 Depth=1
	movq	$32, %rcx
	movq	%r10, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jle	.LBB10_113
# BB#102:                               # %for.body.lr.ph.i491
                                        #   in Loop: Header=BB10_41 Depth=1
	movq	8(%r15), %rsi
	xorq	%rax, %rax
	.align	16, 0x90
.LBB10_103:                             # %for.body.i496
                                        #   Parent Loop BB10_41 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rsi,%rax,8), %rcx
	movq	16(%rcx), %rbx
	cmpq	%r13, %rbx
	jne	.LBB10_105
# BB#104:                               # %land.lhs.true.i499
                                        #   in Loop: Header=BB10_103 Depth=2
	movq	24(%rcx), %rcx
	cmpq	%r9, %rcx
	je	.LBB10_106
.LBB10_105:                             # %for.inc.i502
                                        #   in Loop: Header=BB10_103 Depth=2
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%r10, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rdi
	jl	.LBB10_103
	jmp	.LBB10_113
.LBB10_96:                              # %land.lhs.true.return_crit_edge.i521
                                        #   in Loop: Header=BB10_41 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdi
	cmpq	%rax, %rdi
	movabsq	$0, %r8
	jne	.LBB10_98
# BB#97:                                # %land.lhs.true.return_crit_edge.i521
                                        #   in Loop: Header=BB10_41 Depth=1
	movabsq	$1, %r8
	jmp	.LBB10_98
.LBB10_106:                             # %find_edge_index.exit505
                                        #   in Loop: Header=BB10_41 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	je	.LBB10_113
# BB#107:                               # %find_edge_index.exit505
                                        #   in Loop: Header=BB10_41 Depth=1
	xorq	%rbx, %rbx
	testq	%r12, %r12
	jne	.LBB10_113
	.align	16, 0x90
.LBB10_108:                             # %for.body.i478
                                        #   Parent Loop BB10_41 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rsi,%rbx,8), %rcx
	movq	16(%rcx), %rax
	cmpq	%r13, %rax
	jne	.LBB10_110
# BB#109:                               # %land.lhs.true.i481
                                        #   in Loop: Header=BB10_108 Depth=2
	movq	24(%rcx), %rax
	cmpq	%r9, %rax
	je	.LBB10_112
.LBB10_110:                             # %for.inc.i484
                                        #   in Loop: Header=BB10_108 Depth=2
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r10, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB10_108
# BB#111:                               #   in Loop: Header=BB10_41 Depth=1
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB10_112:                             # %find_edge_index.exit487
                                        #   in Loop: Header=BB10_41 Depth=1
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	movabsq	$.L.str12, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	fprintf
	.align	16, 0x90
.LBB10_113:                             # %for.inc223
                                        #   in Loop: Header=BB10_41 Depth=1
	incq	%r14
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	%r15, %r10
	jl	.LBB10_41
# BB#79:                                # %for.cond226.preheader
	testq	%rax, %rax
	jle	.LBB10_141
# BB#80:                                # %for.body228.lr.ph
	xorq	%r14, %r14
	movabsq	$entry_exit_blocks+112, %r13
	.align	16, 0x90
.LBB10_81:                              # %for.body228
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB10_82 Depth 2
                                        #     Child Loop BB10_116 Depth 2
                                        #     Child Loop BB10_119 Depth 2
                                        #     Child Loop BB10_129 Depth 2
                                        #     Child Loop BB10_134 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r14,8), %r9
	movq	40(%r9), %rdx
	testq	%rdx, %rdx
	movabsq	$0, %rcx
	je	.LBB10_85
	.align	16, 0x90
.LBB10_82:                              # %for.body239
                                        #   Parent Loop BB10_81 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rdx), %rcx
	cmpq	%r13, %rcx
	movq	$1, %rcx
	je	.LBB10_85
# BB#83:                                # %for.cond237
                                        #   in Loop: Header=BB10_82 Depth=2
	movq	8(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB10_82
# BB#84:                                #   in Loop: Header=BB10_81 Depth=1
	xorq	%rcx, %rcx
.LBB10_85:                              # %for.end246
                                        #   in Loop: Header=BB10_81 Depth=1
	movq	entry_exit_blocks+144(%rip), %rdx
	testq	%rdx, %rdx
	jne	.LBB10_116
# BB#86:                                #   in Loop: Header=BB10_81 Depth=1
	movq	%rcx, %r12
	jmp	.LBB10_117
	.align	16, 0x90
.LBB10_116:                             # %for.body250
                                        #   Parent Loop BB10_81 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	16(%rdx), %rsi
	cmpq	%r9, %rsi
	movq	$1, %r12
	je	.LBB10_117
# BB#114:                               # %for.cond248
                                        #   in Loop: Header=BB10_116 Depth=2
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB10_116
# BB#115:                               #   in Loop: Header=BB10_81 Depth=1
	movq	%rcx, %r12
.LBB10_117:                             # %for.end257
                                        #   in Loop: Header=BB10_81 Depth=1
	movslq	4(%r10), %rax
	testq	%rax, %rax
	movq	$1, %r8
	jle	.LBB10_124
# BB#118:                               # %for.body.lr.ph.i455
                                        #   in Loop: Header=BB10_81 Depth=1
	movq	8(%r10), %rbx
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB10_119:                             # %for.body.i460
                                        #   Parent Loop BB10_81 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rbx,%rdi,8), %rcx
	movq	16(%rcx), %rsi
	cmpq	%r9, %rsi
	jne	.LBB10_121
# BB#120:                               # %land.lhs.true.i463
                                        #   in Loop: Header=BB10_119 Depth=2
	movq	24(%rcx), %rcx
	cmpq	%r13, %rcx
	je	.LBB10_122
.LBB10_121:                             # %for.inc.i466
                                        #   in Loop: Header=BB10_119 Depth=2
	incq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rax, %rsi
	jl	.LBB10_119
	jmp	.LBB10_124
.LBB10_122:                             # %land.lhs.true.return_crit_edge.i467
                                        #   in Loop: Header=BB10_81 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdi
	cmpq	%rcx, %rdi
	movabsq	$0, %r8
	jne	.LBB10_124
# BB#123:                               # %land.lhs.true.return_crit_edge.i467
                                        #   in Loop: Header=BB10_81 Depth=1
	movabsq	$1, %r8
	.align	16, 0x90
.LBB10_124:                             # %find_edge_index.exit469
                                        #   in Loop: Header=BB10_81 Depth=1
	testq	%r8, %r8
	je	.LBB10_127
# BB#125:                               # %find_edge_index.exit469
                                        #   in Loop: Header=BB10_81 Depth=1
	testq	%r12, %r12
	je	.LBB10_127
# BB#126:                               # %if.then266
                                        #   in Loop: Header=BB10_81 Depth=1
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	movabsq	$.L.str13, %rsi
	movq	%r14, %rdx
	movq	%r10, %rbx
	callq	fprintf
	movq	%rbx, %r10
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r14,8), %r9
	movl	4(%r10), %eax
.LBB10_127:                             # %if.end268
                                        #   in Loop: Header=BB10_81 Depth=1
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jle	.LBB10_140
# BB#128:                               # %for.body.lr.ph.i437
                                        #   in Loop: Header=BB10_81 Depth=1
	movq	8(%r10), %r8
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB10_129:                             # %for.body.i442
                                        #   Parent Loop BB10_81 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r8,%rsi,8), %rcx
	movq	16(%rcx), %rbx
	cmpq	%r9, %rbx
	jne	.LBB10_131
# BB#130:                               # %land.lhs.true.i445
                                        #   in Loop: Header=BB10_129 Depth=2
	movq	24(%rcx), %rcx
	cmpq	%r13, %rcx
	je	.LBB10_132
.LBB10_131:                             # %for.inc.i448
                                        #   in Loop: Header=BB10_129 Depth=2
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rdi
	jl	.LBB10_129
	jmp	.LBB10_140
.LBB10_132:                             # %find_edge_index.exit451
                                        #   in Loop: Header=BB10_81 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	cmpq	%rcx, %rsi
	je	.LBB10_140
# BB#133:                               # %find_edge_index.exit451
                                        #   in Loop: Header=BB10_81 Depth=1
	xorq	%rbx, %rbx
	testq	%r12, %r12
	jne	.LBB10_140
	.align	16, 0x90
.LBB10_134:                             # %for.body.i424
                                        #   Parent Loop BB10_81 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r8,%rbx,8), %rcx
	movq	16(%rcx), %rdx
	cmpq	%r9, %rdx
	jne	.LBB10_136
# BB#135:                               # %land.lhs.true.i427
                                        #   in Loop: Header=BB10_134 Depth=2
	movq	24(%rcx), %rcx
	cmpq	%r13, %rcx
	je	.LBB10_138
.LBB10_136:                             # %for.inc.i430
                                        #   in Loop: Header=BB10_134 Depth=2
	incq	%rbx
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
	cmpq	%rsi, %rdx
	jl	.LBB10_134
# BB#137:                               #   in Loop: Header=BB10_81 Depth=1
	movq	%r10, %r15
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB10_139
.LBB10_138:                             # %land.lhs.true.return_crit_edge.i431
                                        #   in Loop: Header=BB10_81 Depth=1
	movq	%r10, %r15
.LBB10_139:                             # %find_edge_index.exit433
                                        #   in Loop: Header=BB10_81 Depth=1
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	movabsq	$.L.str14, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	fprintf
	movq	%r15, %r10
	.align	16, 0x90
.LBB10_140:                             # %for.inc285
                                        #   in Loop: Header=BB10_81 Depth=1
	incq	%r14
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB10_81
.LBB10_141:                             # %for.end287
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp133:
	.size	verify_edge_list, .Ltmp133-verify_edge_list
	.cfi_endproc

	.globl	find_edge_index
	.align	16, 0x90
	.type	find_edge_index,@function
find_edge_index:                        # @find_edge_index
	.cfi_startproc
# BB#0:                                 # %entry
	movslq	4(%rdi), %r8
	testq	%r8, %r8
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jle	.LBB11_6
# BB#1:                                 # %for.body.lr.ph
	movq	8(%rdi), %r9
	xorq	%r11, %r11
	.align	16, 0x90
.LBB11_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r9,%r11,8), %r10
	movq	16(%r10), %rcx
	cmpq	%rsi, %rcx
	jne	.LBB11_4
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB11_2 Depth=1
	movq	24(%r10), %rcx
	cmpq	%rdx, %rcx
	je	.LBB11_5
.LBB11_4:                               # %for.inc
                                        #   in Loop: Header=BB11_2 Depth=1
	incq	%r11
	movq	$32, %rcx
	movq	%r11, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%r8, %rdi
	jl	.LBB11_2
	jmp	.LBB11_6
.LBB11_5:                               # %land.lhs.true.return_crit_edge
	movq	%r11, %rax
.LBB11_6:                               # %return
	retq
.Ltmp134:
	.size	find_edge_index, .Ltmp134-find_edge_index
	.cfi_endproc

	.globl	flow_nodes_print
	.align	16, 0x90
	.type	flow_nodes_print,@function
flow_nodes_print:                       # @flow_nodes_print
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp141:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp142:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp143:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp144:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp145:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp146:
	.cfi_def_cfa_offset 80
.Ltmp147:
	.cfi_offset %rbx, -48
.Ltmp148:
	.cfi_offset %r12, -40
.Ltmp149:
	.cfi_offset %r13, -32
.Ltmp150:
	.cfi_offset %r14, -24
.Ltmp151:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	%rdi, %rcx
	testq	%rbx, %rbx
	je	.LBB12_9
# BB#1:                                 # %if.end
	movabsq	$.L.str15, %rsi
	xorq	%rax, %rax
	xorq	%r15, %r15
	movq	%rdx, %rdi
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rcx, %rdx
	callq	fprintf
	movl	4(%rbx), %eax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	testq	%rax, %rax
	je	.LBB12_8
	.align	16, 0x90
.LBB12_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB12_4 Depth 2
	movq	16(%rbx,%r15,8), %r14
	testq	%r14, %r14
	je	.LBB12_7
# BB#3:                                 # %for.body5.preheader
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	$6, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	$1, %r12
	.align	16, 0x90
.LBB12_4:                               # %for.body5
                                        #   Parent Loop BB12_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%r12), %rcx
	movq	$1, %r13
	shlq	%cl, %r13
	testq	%r14, %r13
	je	.LBB12_6
# BB#5:                                 # %if.then7
                                        #   in Loop: Header=BB12_4 Depth=2
	notq	%r13
	addq	%rbx, %rcx
	xorq	%rax, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str16, %rsi
	movq	%rcx, %rdx
	callq	fprintf
	andq	%r13, %r14
	je	.LBB12_7
.LBB12_6:                               # %for.inc
                                        #   in Loop: Header=BB12_4 Depth=2
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	incq	%r12
	cmpq	$64, %rax
	jb	.LBB12_4
.LBB12_7:                               # %for.inc15
                                        #   in Loop: Header=BB12_2 Depth=1
	incq	%r15
	movq	%r15, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	%rcx, %rax
	movq	16(%rsp), %rbx          # 8-byte Reload
	jne	.LBB12_2
.LBB12_8:                               # %do.end
	movabsq	$.L.str17, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fwrite  # TAILCALL
.LBB12_9:                               # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp152:
	.size	flow_nodes_print, .Ltmp152-flow_nodes_print
	.cfi_endproc

	.globl	flow_edge_list_print
	.align	16, 0x90
	.type	flow_edge_list_print,@function
flow_edge_list_print:                   # @flow_edge_list_print
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
.Ltmp164:
	.cfi_offset %rbx, -48
.Ltmp165:
	.cfi_offset %r12, -40
.Ltmp166:
	.cfi_offset %r13, -32
.Ltmp167:
	.cfi_offset %r14, -24
.Ltmp168:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r13
	movq	%rsi, %r15
	movq	%rdi, %rcx
	testq	%r15, %r15
	je	.LBB13_5
# BB#1:                                 # %if.end
	movabsq	$.L.str15, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rcx, %rdx
	callq	fprintf
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB13_4
# BB#2:
	movq	$1, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %r13
	.align	16, 0x90
.LBB13_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r15,%rbx,8), %rax
	movq	16(%rax), %rdx
	movq	24(%rax), %rax
	movl	88(%rax), %ecx
	movl	88(%rdx), %edx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str18, %rsi
	callq	fprintf
	movq	%rbx, %rax
	andq	%r12, %rax
	incq	%rbx
	cmpq	%r13, %rax
	jne	.LBB13_3
.LBB13_4:                               # %for.end
	movabsq	$.L.str17, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fwrite  # TAILCALL
.LBB13_5:                               # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp169:
	.size	flow_edge_list_print, .Ltmp169-flow_edge_list_print
	.cfi_endproc

	.globl	remove_fake_edges
	.align	16, 0x90
	.type	remove_fake_edges,@function
remove_fake_edges:                      # @remove_fake_edges
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp173:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp174:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp175:
	.cfi_def_cfa_offset 32
.Ltmp176:
	.cfi_offset %rbx, -24
.Ltmp177:
	.cfi_offset %r14, -16
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB14_8
# BB#1:
	xorq	%r14, %r14
	.align	16, 0x90
.LBB14_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB14_3 Depth 2
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%r14,8), %rcx
	movq	40(%rcx), %rdi
	testq	%rdi, %rdi
	je	.LBB14_7
	.align	16, 0x90
.LBB14_3:                               # %for.body.i
                                        #   Parent Loop BB14_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	48(%rdi), %eax
	movq	8(%rdi), %rbx
	testq	$16, %rax
	je	.LBB14_5
# BB#4:                                 # %if.then.i
                                        #   in Loop: Header=BB14_3 Depth=2
	callq	remove_edge
.LBB14_5:                               # %for.cond.backedge.i
                                        #   in Loop: Header=BB14_3 Depth=2
	testq	%rbx, %rbx
	movq	%rbx, %rdi
	jne	.LBB14_3
# BB#6:                                 # %remove_fake_successors.exit.loopexit
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	n_basic_blocks(%rip), %eax
.LBB14_7:                               # %remove_fake_successors.exit
                                        #   in Loop: Header=BB14_2 Depth=1
	incq	%r14
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
	cmpq	%rsi, %rdx
	jl	.LBB14_2
.LBB14_8:                               # %for.end
	movq	entry_exit_blocks+40(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB14_12
	.align	16, 0x90
.LBB14_9:                               # %for.body.i10
                                        # =>This Inner Loop Header: Depth=1
	movzbl	48(%rdi), %eax
	movq	8(%rdi), %rbx
	testq	$16, %rax
	je	.LBB14_11
# BB#10:                                # %if.then.i11
                                        #   in Loop: Header=BB14_9 Depth=1
	callq	remove_edge
.LBB14_11:                              # %for.cond.backedge.i13
                                        #   in Loop: Header=BB14_9 Depth=1
	testq	%rbx, %rbx
	movq	%rbx, %rdi
	jne	.LBB14_9
.LBB14_12:                              # %remove_fake_successors.exit14
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp178:
	.size	remove_fake_edges, .Ltmp178-remove_fake_edges
	.cfi_endproc

	.globl	add_noreturn_fake_exit_edges
	.align	16, 0x90
	.type	add_noreturn_fake_exit_edges,@function
add_noreturn_fake_exit_edges:           # @add_noreturn_fake_exit_edges
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp182:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp183:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp184:
	.cfi_def_cfa_offset 32
.Ltmp185:
	.cfi_offset %rbx, -24
.Ltmp186:
	.cfi_offset %r14, -16
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB15_5
# BB#1:
	movq	$4, %rbx
	movabsq	$entry_exit_blocks+112, %r14
	.align	16, 0x90
.LBB15_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	(%rcx,%rbx,8), %rdi
	cmpq	$0, 40(%rdi)
	jne	.LBB15_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	$16, %rdx
	movq	%r14, %rsi
	callq	make_single_succ_edge
	movl	n_basic_blocks(%rip), %eax
.LBB15_4:                               # %for.inc
                                        #   in Loop: Header=BB15_2 Depth=1
	leaq	-3(%rbx), %rdx
	movq	$32, %rcx
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
	jl	.LBB15_2
.LBB15_5:                               # %for.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp187:
	.size	add_noreturn_fake_exit_edges, .Ltmp187-add_noreturn_fake_exit_edges
	.cfi_endproc

	.globl	connect_infinite_loops_to_exit
	.align	16, 0x90
	.type	connect_infinite_loops_to_exit,@function
connect_infinite_loops_to_exit:         # @connect_infinite_loops_to_exit
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
.Ltmp199:
	.cfi_offset %rbx, -48
.Ltmp200:
	.cfi_offset %r12, -40
.Ltmp201:
	.cfi_offset %r13, -32
.Ltmp202:
	.cfi_offset %r14, -24
.Ltmp203:
	.cfi_offset %r15, -16
	movl	n_basic_blocks(%rip), %edi
	addq	$2, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, %r14
	addq	$2, %rdi
	callq	sbitmap_alloc
	movq	%rax, %r15
	movq	%r15, %rdi
	callq	sbitmap_zero
	movl	entry_exit_blocks+200(%rip), %ecx
	movq	$entry_exit_blocks+112, (%r14)
	addq	$2, %rcx
	movabsq	$4294967232, %r13       # imm = 0xFFFFFFC0
	movq	%rcx, %rax
	andq	%r13, %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	movq	$3, %rcx
	shrq	%cl, %rax
	orq	%rdx, 16(%r15,%rax)
	movq	$1, %r8
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	jmp	.LBB16_2
	.align	16, 0x90
.LBB16_11:                              # %if.end
                                        #   in Loop: Header=BB16_2 Depth=1
	movq	$16, %rdx
	movq	%r12, %rdi
	movabsq	$entry_exit_blocks+112, %rsi
	callq	make_edge
	movq	%r12, (%r14)
	movl	88(%r12), %ecx
	addq	$2, %rcx
	movq	%rcx, %rax
	andq	%r13, %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	movq	$3, %rcx
	shrq	%cl, %rax
	orq	%rdx, 16(%r15,%rax)
	movq	$1, %r8
	movq	%rbx, %r10
	jmp	.LBB16_2
	.align	16, 0x90
.LBB16_1:                               # %while.cond.loopexit.i
                                        #   in Loop: Header=BB16_2 Depth=1
	testq	%r10, %r8
	jne	.LBB16_2
# BB#7:                                 # %while.end.i
                                        #   in Loop: Header=BB16_2 Depth=1
	movl	n_basic_blocks(%rip), %eax
	addq	$2, %rax
	.align	16, 0x90
.LBB16_8:                               # %for.cond10.i
                                        #   Parent Loop BB16_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %rdx
	movq	$32, %rcx
                                        # kill: RAX<def> RDX<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB16_12
# BB#9:                                 # %for.body13.i
                                        #   in Loop: Header=BB16_8 Depth=2
	leaq	-1(%rdx), %rax
	movq	%rax, %rsi
	andq	%r13, %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	movq	16(%r15,%rsi), %rsi
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	testq	%rdi, %rsi
	jne	.LBB16_8
# BB#10:                                # %flow_dfs_compute_reverse_execute.exit
                                        #   in Loop: Header=BB16_2 Depth=1
	movq	%r10, %rbx
	addq	$-3, %rdx
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	32(%rax,%rdx,8), %r12
	testq	%r12, %r12
	jne	.LBB16_11
	jmp	.LBB16_12
	.align	16, 0x90
.LBB16_2:                               # %while.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB16_6 Depth 2
                                        #     Child Loop BB16_8 Depth 2
	decq	%r8
	movq	%r8, %rax
	andq	%r10, %rax
	movq	(%r14,%rax,8), %rax
	movq	32(%rax), %rsi
	jmp	.LBB16_6
	.align	16, 0x90
.LBB16_5:                               # %for.inc.i
                                        #   in Loop: Header=BB16_6 Depth=2
	movq	(%rsi), %rsi
.LBB16_6:                               # %for.inc.i
                                        #   Parent Loop BB16_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rsi, %rsi
	je	.LBB16_1
# BB#3:                                 # %for.body.i
                                        #   in Loop: Header=BB16_6 Depth=2
	movq	16(%rsi), %r9
	movl	88(%r9), %ebx
	addq	$2, %rbx
	movq	%rbx, %rax
	andq	%r13, %rax
	movq	$6, %rcx
	shrq	%cl, %rax
	movq	16(%r15,%rax,8), %rdx
	movq	$1, %rdi
	movq	%rbx, %rcx
	shlq	%cl, %rdi
	testq	%rdx, %rdi
	jne	.LBB16_5
# BB#4:                                 # %if.then.i
                                        #   in Loop: Header=BB16_6 Depth=2
	leaq	1(%r8), %rcx
	andq	%r10, %r8
	movq	%r9, (%r14,%r8,8)
	orq	%rdi, %rdx
	movq	%rdx, 16(%r15,%rax,8)
	movq	%rcx, %r8
	jmp	.LBB16_5
.LBB16_12:                              # %while.end
	movq	%r14, %rdi
	callq	free
	movq	%r15, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.Ltmp204:
	.size	connect_infinite_loops_to_exit, .Ltmp204-connect_infinite_loops_to_exit
	.cfi_endproc

	.globl	flow_reverse_top_sort_order_compute
	.align	16, 0x90
	.type	flow_reverse_top_sort_order_compute,@function
flow_reverse_top_sort_order_compute:    # @flow_reverse_top_sort_order_compute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp211:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp212:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp213:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp214:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp215:
	.cfi_def_cfa_offset 48
.Ltmp216:
	.cfi_offset %rbx, -48
.Ltmp217:
	.cfi_offset %r12, -40
.Ltmp218:
	.cfi_offset %r13, -32
.Ltmp219:
	.cfi_offset %r14, -24
.Ltmp220:
	.cfi_offset %r15, -16
	movl	n_basic_blocks(%rip), %eax
	movq	%rdi, %r14
	incq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	movq	%rax, %rdi
	callq	xmalloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, %r12
	callq	sbitmap_alloc
	movq	%rax, %r15
	movq	%r15, %rdi
	callq	sbitmap_zero
	movq	entry_exit_blocks+40(%rip), %rax
	movq	%rax, (%r12)
	xorq	%r9, %r9
	movq	$1, %rdx
	movabsq	$entry_exit_blocks+112, %r10
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
.LBB17_1:                               # %while.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB17_2 Depth 2
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	.align	16, 0x90
.LBB17_2:                               # %while.body
                                        #   Parent Loop BB17_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-8(%r12,%rdx,8), %rsi
	movq	24(%rsi), %rdi
	cmpq	%r10, %rdi
	je	.LBB17_7
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB17_2 Depth=2
	movl	88(%rdi), %r13d
	movq	$6, %rcx
	movq	%r13, %rax
	shrq	%cl, %rax
	movq	16(%r15,%rax,8), %rbx
	movq	$1, %r8
	movq	%r13, %rcx
	shlq	%cl, %r8
	testq	%rbx, %r8
	jne	.LBB17_7
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB17_2 Depth=2
	orq	%rbx, %r8
	movq	%r8, 16(%r15,%rax,8)
	movq	40(%rdi), %rax
	testq	%rax, %rax
	je	.LBB17_6
# BB#5:                                 # %if.then20
                                        #   in Loop: Header=BB17_2 Depth=2
	movq	%rax, (%r12,%rdx,8)
	incq	%rdx
	testq	%r11, %rdx
	jne	.LBB17_2
	jmp	.LBB17_14
	.align	16, 0x90
.LBB17_7:                               # %if.else29
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	8(%rsi), %rax
	testq	%rax, %rax
	jne	.LBB17_10
# BB#8:                                 # %if.else29
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	16(%rsi), %rcx
	movabsq	$entry_exit_blocks, %rsi
	cmpq	%rsi, %rcx
	je	.LBB17_10
# BB#9:                                 # %if.then34
                                        #   in Loop: Header=BB17_1 Depth=1
	movl	88(%rcx), %esi
	movq	$32, %rcx
	leaq	1(%r9), %rdi
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movl	%esi, (%r14,%r9,4)
	movq	%rdi, %r9
.LBB17_10:                              # %if.end39
                                        #   in Loop: Header=BB17_1 Depth=1
	testq	%rax, %rax
	jne	.LBB17_12
# BB#11:                                #   in Loop: Header=BB17_1 Depth=1
	decq	%rdx
	jmp	.LBB17_13
	.align	16, 0x90
.LBB17_12:                              # %if.then42
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	%rax, -8(%r12,%rdx,8)
	jmp	.LBB17_13
	.align	16, 0x90
.LBB17_6:                               # %if.else
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	$32, %rcx
	leaq	1(%r9), %rax
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movl	%r13d, (%r14,%r9,4)
	movq	%rax, %r9
.LBB17_13:                              # %while.cond.outer.backedge
                                        #   in Loop: Header=BB17_1 Depth=1
	testq	%r11, %rdx
	jne	.LBB17_1
.LBB17_14:                              # %while.end
	movq	%r12, %rdi
	callq	free
	movq	%r15, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.Ltmp221:
	.size	flow_reverse_top_sort_order_compute, .Ltmp221-flow_reverse_top_sort_order_compute
	.cfi_endproc

	.globl	flow_depth_first_order_compute
	.align	16, 0x90
	.type	flow_depth_first_order_compute,@function
flow_depth_first_order_compute:         # @flow_depth_first_order_compute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp228:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp229:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp230:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp231:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp232:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp233:
	.cfi_def_cfa_offset 64
.Ltmp234:
	.cfi_offset %rbx, -48
.Ltmp235:
	.cfi_offset %r12, -40
.Ltmp236:
	.cfi_offset %r13, -32
.Ltmp237:
	.cfi_offset %r14, -24
.Ltmp238:
	.cfi_offset %r15, -16
	movl	n_basic_blocks(%rip), %eax
	movq	%rsi, %r15
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	leaq	-1(%rax), %rcx
	movq	%rcx, (%rsp)            # 8-byte Spill
	incq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	movq	%rax, %rdi
	callq	xmalloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, %r12
	callq	sbitmap_alloc
	movq	%rax, %r13
	movq	%r13, %rdi
	callq	sbitmap_zero
	movq	entry_exit_blocks+40(%rip), %rax
	movq	%rax, (%r12)
	movq	$1, %r9
	xorq	%r14, %r14
	jmp	.LBB18_1
	.align	16, 0x90
.LBB18_20:                              # %if.then54
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	%rax, -8(%r12,%r11,8)
	movq	%r11, %r9
.LBB18_1:                               # %while.cond.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB18_2 Depth 2
                                        #       Child Loop BB18_5 Depth 3
                                        #       Child Loop BB18_9 Depth 3
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	%r9, %r11
	jmp	.LBB18_2
	.align	16, 0x90
.LBB18_8:                               # %if.then27
                                        #   in Loop: Header=BB18_2 Depth=2
	movq	%rax, (%r12,%r11,8)
	incq	%r11
.LBB18_2:                               # %while.cond.outer107
                                        #   Parent Loop BB18_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB18_5 Depth 3
                                        #       Child Loop BB18_9 Depth 3
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r11
	je	.LBB18_21
# BB#3:                                 # %while.cond.outer107.while.cond.outer107.split_crit_edge
                                        #   in Loop: Header=BB18_2 Depth=2
	leaq	-1(%r11), %r9
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	jne	.LBB18_4
	.align	16, 0x90
.LBB18_9:                               # %while.body.us131
                                        #   Parent Loop BB18_1 Depth=1
                                        #     Parent Loop BB18_2 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	-8(%r12,%r11,8), %rdx
	movq	24(%rdx), %rsi
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %rsi
	je	.LBB18_15
# BB#10:                                # %land.lhs.true.us134
                                        #   in Loop: Header=BB18_9 Depth=3
	movl	88(%rsi), %r8d
	movq	$6, %rcx
	movq	%r8, %rax
	shrq	%cl, %rax
	movq	16(%r13,%rax,8), %rdi
	movq	$1, %rbx
	movq	%r8, %rcx
	shlq	%cl, %rbx
	testq	%rdi, %rbx
	jne	.LBB18_15
# BB#11:                                # %if.end.us148
                                        #   in Loop: Header=BB18_9 Depth=3
	orq	%rdi, %rbx
	movq	%rbx, 16(%r13,%rax,8)
	movq	40(%rsi), %rax
	incq	%r14
	testq	%rax, %rax
	jne	.LBB18_8
# BB#12:                                # %if.else.us152
                                        #   in Loop: Header=BB18_9 Depth=3
	testq	%r15, %r15
	je	.LBB18_9
	jmp	.LBB18_14
	.align	16, 0x90
.LBB18_4:                               # %while.cond.outer107.split.while.cond.outer107.split.split_crit_edge
                                        #   in Loop: Header=BB18_2 Depth=2
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	.align	16, 0x90
.LBB18_5:                               # %while.body
                                        #   Parent Loop BB18_1 Depth=1
                                        #     Parent Loop BB18_2 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	-8(%r12,%r11,8), %rdx
	movq	24(%rdx), %rsi
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %rsi
	je	.LBB18_15
# BB#6:                                 # %land.lhs.true
                                        #   in Loop: Header=BB18_5 Depth=3
	movl	88(%rsi), %r8d
	movq	$6, %rcx
	movq	%r8, %rax
	shrq	%cl, %rax
	movq	16(%r13,%rax,8), %rdi
	movq	$1, %r10
	movq	%r8, %rcx
	shlq	%cl, %r10
	testq	%rdi, %r10
	jne	.LBB18_15
# BB#7:                                 # %if.end
                                        #   in Loop: Header=BB18_5 Depth=3
	orq	%rdi, %r10
	movq	%r10, 16(%r13,%rax,8)
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	%r8d, (%rax,%r14,4)
	movq	40(%rsi), %rax
	incq	%r14
	testq	%rax, %rax
	jne	.LBB18_8
# BB#13:                                # %if.else
                                        #   in Loop: Header=BB18_5 Depth=3
	testq	%r15, %r15
	je	.LBB18_5
.LBB18_14:                              # %if.then33.us-lcssa.us-lcssa
                                        #   in Loop: Header=BB18_1 Depth=1
	addq	$88, %rsi
	movl	(%rsi), %eax
	movq	$32, %rcx
	movq	(%rsp), %rsi            # 8-byte Reload
	leaq	-1(%rsi), %rdx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	%eax, (%r15,%rsi,4)
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	%r11, %r9
	jmp	.LBB18_1
	.align	16, 0x90
.LBB18_15:                              # %if.else39
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	8(%rdx), %rax
	testq	%rax, %rax
	jne	.LBB18_19
# BB#16:                                # %if.else39
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	16(%rdx), %rcx
	movabsq	$entry_exit_blocks, %rdx
	cmpq	%rdx, %rcx
	je	.LBB18_19
# BB#17:                                # %if.else39
                                        #   in Loop: Header=BB18_1 Depth=1
	testq	%r15, %r15
	je	.LBB18_19
# BB#18:                                # %if.then46
                                        #   in Loop: Header=BB18_1 Depth=1
	movl	88(%rcx), %edx
	movq	$32, %rcx
	movq	(%rsp), %rdi            # 8-byte Reload
	leaq	-1(%rdi), %rsi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movl	%edx, (%r15,%rdi,4)
	movq	%rsi, (%rsp)            # 8-byte Spill
.LBB18_19:                              # %if.end51
                                        #   in Loop: Header=BB18_1 Depth=1
	testq	%rax, %rax
	je	.LBB18_1
	jmp	.LBB18_20
.LBB18_21:                              # %while.end
	movq	%r12, %rdi
	callq	free
	movq	%r13, %rdi
	callq	free
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jg	.LBB18_22
# BB#24:                                # %if.end66
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB18_25
# BB#26:                                # %if.end70
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB18_22:                              # %if.then65
	movabsq	$.L.str19, %rax
	movabsq	$.L__FUNCTION__.flow_depth_first_order_compute, %rcx
	movq	$969, %rsi              # imm = 0x3C9
	jmp	.LBB18_23
.LBB18_25:                              # %if.then69
	movabsq	$.L.str19, %rax
	movabsq	$.L__FUNCTION__.flow_depth_first_order_compute, %rcx
	movq	$973, %rsi              # imm = 0x3CD
.LBB18_23:                              # %if.then65
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp239:
	.size	flow_depth_first_order_compute, .Ltmp239-flow_depth_first_order_compute
	.cfi_endproc

	.globl	flow_preorder_transversal_compute
	.align	16, 0x90
	.type	flow_preorder_transversal_compute,@function
flow_preorder_transversal_compute:      # @flow_preorder_transversal_compute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp246:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp247:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp248:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp249:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp250:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp251:
	.cfi_def_cfa_offset 96
.Ltmp252:
	.cfi_offset %rbx, -48
.Ltmp253:
	.cfi_offset %r12, -40
.Ltmp254:
	.cfi_offset %r13, -32
.Ltmp255:
	.cfi_offset %r14, -24
.Ltmp256:
	.cfi_offset %r15, -16
	movl	n_basic_blocks(%rip), %eax
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	incq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	movq	%rax, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rdi
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	$24, %rsi
	callq	xcalloc
	movslq	n_basic_blocks(%rip), %rdi
	movq	%rax, %rbx
	testq	%rdi, %rdi
	jle	.LBB19_7
# BB#1:
	xorq	%r14, %r14
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB19_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_3 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r14,8), %rax
	movq	40(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movabsq	$0, %rdx
	je	.LBB19_6
	.align	16, 0x90
.LBB19_3:                               # %for.body5
                                        #   Parent Loop BB19_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rcx), %rcx
	incq	%rdx
	testq	%rcx, %rcx
	jne	.LBB19_3
# BB#4:                                 # %for.end
                                        #   in Loop: Header=BB19_2 Depth=1
	xorq	%rax, %rax
	testq	%r13, %rdx
	je	.LBB19_6
# BB#5:                                 # %cond.true
                                        #   in Loop: Header=BB19_2 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$8, %rsi
	movq	%rdx, %rdi
	callq	xcalloc
	movl	n_basic_blocks(%rip), %edi
.LBB19_6:                               # %cond.end
                                        #   in Loop: Header=BB19_2 Depth=1
	leaq	(%r14,%r14,2), %rcx
	movq	%rax, 8(%rbx,%rcx,8)
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB19_2
.LBB19_7:                               # %for.end14
	callq	sbitmap_alloc
	movq	%rax, %r12
	movq	%r12, %rdi
	callq	sbitmap_zero
	movq	entry_exit_blocks+40(%rip), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
	movq	$1, %rax
	movabsq	$entry_exit_blocks+112, %r9
	movabsq	$entry_exit_blocks, %r11
	.align	16, 0x90
.LBB19_8:                               # %while.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_9 Depth 2
	movq	%rax, 24(%rsp)          # 8-byte Spill
	leaq	-1(%rax), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%rdx,8), %r10
	jmp	.LBB19_9
.LBB19_22:                              # %if.then70
                                        #   in Loop: Header=BB19_9 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	%r10, (%rax,%rcx,8)
	.align	16, 0x90
.LBB19_9:                               # %while.body
                                        #   Parent Loop BB19_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%r10), %rsi
	cmpq	%r9, %rsi
	je	.LBB19_20
# BB#10:                                # %land.lhs.true
                                        #   in Loop: Header=BB19_9 Depth=2
	movl	88(%rsi), %r13d
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$6, %rcx
	movq	%r13, %r14
	shrq	%cl, %r14
	movq	16(%r12,%r14,8), %r15
	movq	$1, %r8
	movq	%r13, %rcx
	shlq	%cl, %r8
	testq	%r15, %r8
	jne	.LBB19_20
# BB#11:                                # %if.then
                                        #   in Loop: Header=BB19_9 Depth=2
	movq	16(%r10), %rcx
	orq	%r15, %r8
	movq	%r8, 16(%r12,%r14,8)
	cmpq	%r11, %rcx
	je	.LBB19_13
# BB#12:                                # %if.then40
                                        #   in Loop: Header=BB19_9 Depth=2
	movslq	88(%rcx), %rcx
	leaq	(%rcx,%rcx,2), %rcx
	movq	%rbx, %rdi
	movl	(%rdi,%rcx,8), %r8d
	leaq	(%rax,%rax,2), %rax
	leaq	1(%r8), %rbx
	movl	%ebx, (%rdi,%rcx,8)
	movq	8(%rdi,%rcx,8), %rbx
	leaq	(%rdi,%rax,8), %rdx
	leaq	(%rdi,%rcx,8), %rcx
	movq	%rdx, (%rbx,%r8,8)
	movq	%rcx, 16(%rdi,%rax,8)
	movq	%rdi, %rbx
.LBB19_13:                              # %if.end
                                        #   in Loop: Header=BB19_9 Depth=2
	movq	40(%rsi), %rax
	testq	%rax, %rax
	je	.LBB19_9
	jmp	.LBB19_14
.LBB19_20:                              # %if.else
                                        #   in Loop: Header=BB19_9 Depth=2
	movq	8(%r10), %r10
	testq	%r10, %r10
	jne	.LBB19_22
# BB#21:                                #   in Loop: Header=BB19_8 Depth=1
	movq	16(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB19_15
	.align	16, 0x90
.LBB19_14:                              # %if.then62
                                        #   in Loop: Header=BB19_8 Depth=1
	movq	$32, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	leaq	1(%rdx), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx,%rdx,8)
.LBB19_15:                              # %while.cond.outer.backedge
                                        #   in Loop: Header=BB19_8 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	movq	%rsi, %rax
	jne	.LBB19_8
# BB#16:                                # %while.end
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	%r12, %rdi
	callq	free
	xorq	%rax, %rax
	movq	8(%rsp), %rdx           # 8-byte Reload
	movl	%eax, (%rdx)
	testq	%rbx, %rbx
	je	.LBB19_24
# BB#17:                                # %while.body84.lr.ph.lr.ph
	movq	$1, %rax
	movabsq	$-6148914691236517205, %r8 # imm = 0xAAAAAAAAAAAAAAAB
	movq	%rbx, %rsi
	.align	16, 0x90
.LBB19_18:                              # %while.body84
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rsi), %ecx
	testq	%rcx, %rcx
	jne	.LBB19_19
# BB#23:                                # %if.else97
                                        #   in Loop: Header=BB19_18 Depth=1
	movq	16(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB19_18
	jmp	.LBB19_24
.LBB19_19:                              # %if.then87
                                        #   in Loop: Header=BB19_18 Depth=1
	decq	%rcx
	movl	%ecx, (%rsi)
	movq	8(%rsi), %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	movq	(%rsi,%rcx,8), %rsi
	movq	%rsi, %rdi
	subq	%rbx, %rdi
	movq	$3, %rcx
	shrq	%cl, %rdi
	imulq	%r8, %rdi
	testq	%rsi, %rsi
	movl	%edi, (%rdx,%rax,4)
	leaq	1(%rax), %rax
	jne	.LBB19_18
.LBB19_24:                              # %for.cond101.preheader
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB19_29
# BB#25:                                # %for.body104.preheader
	leaq	8(%rbx), %r14
	movq	$1, %r15
	.align	16, 0x90
.LBB19_26:                              # %for.body104
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB19_28
# BB#27:                                # %if.then109
                                        #   in Loop: Header=BB19_26 Depth=1
	callq	free
	movl	n_basic_blocks(%rip), %eax
.LBB19_28:                              # %for.inc114
                                        #   in Loop: Header=BB19_26 Depth=1
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
	incq	%r15
	addq	$24, %r14
	cmpq	%rsi, %rdx
	jl	.LBB19_26
.LBB19_29:                              # %for.end116
	movq	%rbx, %rdi
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.Ltmp257:
	.size	flow_preorder_transversal_compute, .Ltmp257-flow_preorder_transversal_compute
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Compressed edge list, %d BBs + entry & exit, and %d edges\n"
	.size	.L.str, 59

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	" %-4d - edge("
	.size	.L.str1, 14

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"entry,"
	.size	.L.str2, 7

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"%d,"
	.size	.L.str3, 4

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"exit)\n"
	.size	.L.str4, 7

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"%d)\n"
	.size	.L.str5, 5

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"*p* No index for edge from %d to %d\n"
	.size	.L.str6, 37

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"*p* Pred for index %d should be %d not %d\n"
	.size	.L.str7, 43

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"*p* Succ for index %d should be %d not %d\n"
	.size	.L.str8, 43

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"*** Edge (%d, %d) appears to not have an index\n"
	.size	.L.str9, 48

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"*** Edge (%d, %d) has index %d, but there is no edge\n"
	.size	.L.str10, 54

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"*** Edge (entry, %d) appears to not have an index\n"
	.size	.L.str11, 51

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"*** Edge (entry, %d) has index %d, but no edge exists\n"
	.size	.L.str12, 55

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"*** Edge (%d, exit) appears to not have an index\n"
	.size	.L.str13, 50

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"*** Edge (%d, exit) has index %d, but no edge exists\n"
	.size	.L.str14, 54

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"%s { "
	.size	.L.str15, 6

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"%d "
	.size	.L.str16, 4

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"}\n"
	.size	.L.str17, 3

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"%d->%d "
	.size	.L.str18, 8

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"cfganal.c"
	.size	.L.str19, 10

	.type	.L__FUNCTION__.flow_depth_first_order_compute,@object # @__FUNCTION__.flow_depth_first_order_compute
.L__FUNCTION__.flow_depth_first_order_compute:
	.asciz	"flow_depth_first_order_compute"
	.size	.L__FUNCTION__.flow_depth_first_order_compute, 31


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
