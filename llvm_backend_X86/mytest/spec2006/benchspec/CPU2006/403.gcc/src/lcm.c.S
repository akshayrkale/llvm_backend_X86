	.file	"lcm.c.bc"
	.text
	.globl	pre_edge_lcm
	.align	16, 0x90
	.type	pre_edge_lcm,@function
pre_edge_lcm:                           # @pre_edge_lcm
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
	subq	$96, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 144
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
	movq	%r9, %r13
	movq	%r13, 32(%rsp)          # 8-byte Spill
	movq	%r8, 24(%rsp)           # 8-byte Spill
	movq	%rcx, %r15
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	movq	%rsi, %r14
	movq	%r14, 8(%rsp)           # 8-byte Spill
	callq	create_edge_list
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movl	4(%rax), %ebx
	movq	%rbx, (%rsp)            # 8-byte Spill
	movl	n_basic_blocks(%rip), %edi
	movq	%r14, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, %r12
	movq	%r14, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	movq	%r12, %rcx
	callq	compute_available
	movq	%r12, %rdi
	callq	free
	movl	n_basic_blocks(%rip), %edi
	movq	%r14, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, %r15
	movq	%r15, 80(%rsp)          # 8-byte Spill
	movq	%r14, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	compute_antinout_edge
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	sbitmap_vector_alloc
	movq	16(%rsp), %r15          # 8-byte Reload
	movslq	4(%r15), %rbx
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	callq	sbitmap_alloc
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	callq	sbitmap_alloc
	movq	%r15, %r12
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rbx, %rbx
	jle	.LBB0_9
# BB#1:                                 # %for.body.lr.ph.i
	movq	$1, %r15
	movabsq	$entry_exit_blocks, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_2:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r12), %rax
	movq	-8(%rax,%r15,8), %rax
	movq	16(%rax), %r14
	movq	24(%rax), %rax
	cmpq	%rbx, %r14
	jne	.LBB0_4
# BB#3:                                 # %if.then.i
                                        #   in Loop: Header=BB0_2 Depth=1
	movslq	88(%rax), %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	-8(%rcx,%r15,8), %rdi
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %rsi
	callq	sbitmap_copy
	jmp	.LBB0_8
	.align	16, 0x90
.LBB0_4:                                # %if.else.i
                                        #   in Loop: Header=BB0_2 Depth=1
	movslq	88(%rax), %rax
	cmpq	$-2, %rax
	jne	.LBB0_6
# BB#5:                                 # %if.then13.i
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	-8(%rax,%r15,8), %rdi
	callq	sbitmap_zero
	jmp	.LBB0_7
	.align	16, 0x90
.LBB0_6:                                # %if.else16.i
                                        #   in Loop: Header=BB0_2 Depth=1
	movslq	88(%r14), %rcx
	movq	80(%rsp), %rdx          # 8-byte Reload
	movq	(%rdx,%rax,8), %rsi
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%rcx,8), %rdx
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	sbitmap_difference
	movslq	88(%r14), %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %rsi
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	sbitmap_not
	movslq	88(%r14), %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	-8(%rcx,%r15,8), %rdi
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %rdx
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rcx
	callq	sbitmap_a_and_b_or_c
.LBB0_7:                                # %for.inc.i
                                        #   in Loop: Header=BB0_2 Depth=1
	movabsq	$entry_exit_blocks, %rbx
.LBB0_8:                                # %for.inc.i
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	%r15, %rax
	andq	%r13, %rax
	movq	88(%rsp), %rcx          # 8-byte Reload
	andq	%r13, %rcx
	incq	%r15
	cmpq	%rcx, %rax
	jne	.LBB0_2
.LBB0_9:                                # %compute_earliest.exit
	movq	%r12, 16(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %r14
	callq	free
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	80(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, %r12
	incq	%rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, %rbx
	movl	n_basic_blocks(%rip), %edi
	movslq	4(%r14), %r14
	incq	%rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, 32(%rsp)          # 8-byte Spill
	testq	%r14, %r14
	jle	.LBB0_12
# BB#10:                                # %for.body.lr.ph.i47
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	xorq	%rcx, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_11:                               # %for.body.i51
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rcx,8), %rsi
	movq	%rcx, 40(%rsi)
	incq	%rcx
	movq	%rcx, %rsi
	andq	%rdx, %rsi
	movq	%r14, %rdi
	andq	%rdx, %rdi
	cmpq	%rdi, %rsi
	jne	.LBB0_11
.LBB0_12:                               # %for.end.i
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	sbitmap_vector_ones
	movq	entry_exit_blocks+40(%rip), %rbx
	testq	%rbx, %rbx
	movq	56(%rsp), %r13          # 8-byte Reload
	je	.LBB0_13
	.align	16, 0x90
.LBB0_45:                               # %for.body5.i
                                        # =>This Inner Loop Header: Depth=1
	movq	40(%rbx), %rax
	movq	(%r12,%rax,8), %rdi
	movq	(%r13,%rax,8), %rsi
	callq	sbitmap_copy
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_45
.LBB0_13:                               # %for.cond12.preheader.i
	movq	%r12, %r15
	movslq	n_basic_blocks(%rip), %rbx
	testq	%rbx, %rbx
	movq	32(%rsp), %rdi          # 8-byte Reload
	jle	.LBB0_16
# BB#14:
	xorq	%rax, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rbx, %rdx
	andq	%rcx, %rdx
	.align	16, 0x90
.LBB0_15:                               # %for.body15.i
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rsi
	movq	32(%rsi,%rax,8), %rsi
	movq	%rsi, (%rdi,%rax,8)
	incq	%rax
	movq	%rsi, 80(%rsi)
	movq	%rax, %rsi
	andq	%rcx, %rsi
	cmpq	%rdx, %rsi
	jne	.LBB0_15
.LBB0_16:                               # %for.end22.i
	xorq	%rax, %rax
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rbx
	je	.LBB0_33
# BB#17:
	movq	32(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%rbx,8), %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	%rax, %rcx
	movq	%rax, 72(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB0_19:                               # %while.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_23 Depth 2
                                        #     Child Loop BB0_25 Depth 2
	movq	%rbx, 80(%rsp)          # 8-byte Spill
	movq	(%rcx), %r13
	leaq	8(%rcx), %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	movq	$0, 80(%r13)
	jb	.LBB0_21
# BB#20:                                # %while.body.i
                                        #   in Loop: Header=BB0_19 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
.LBB0_21:                               # %while.body.i
                                        #   in Loop: Header=BB0_19 Depth=1
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movslq	88(%r13), %r14
	movq	88(%rsp), %rbx          # 8-byte Reload
	movq	(%rbx,%r14,8), %rdi
	callq	sbitmap_ones
	movq	%r13, 40(%rsp)          # 8-byte Spill
	movq	32(%r13), %r13
	jmp	.LBB0_23
	.align	16, 0x90
.LBB0_22:                               # %for.body36.i
                                        #   in Loop: Header=BB0_23 Depth=2
	movq	40(%r13), %rax
	movq	(%rbx,%r14,8), %rdi
	movq	(%r15,%rax,8), %rdx
	movq	%rdi, %rsi
	callq	sbitmap_a_and_b
	movq	(%r13), %r13
.LBB0_23:                               # %for.body36.i
                                        #   Parent Loop BB0_19 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r13, %r13
	jne	.LBB0_22
# BB#24:                                # %for.end45.i
                                        #   in Loop: Header=BB0_19 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rbx
	decq	80(%rsp)                # 8-byte Folded Spill
	testq	%rbx, %rbx
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	56(%rsp), %r13          # 8-byte Reload
	je	.LBB0_18
	.align	16, 0x90
.LBB0_25:                               # %for.body49.i
                                        #   Parent Loop BB0_19 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	16(%rbx), %rax
	movq	40(%rbx), %rcx
	movslq	88(%rax), %rax
	movq	(%r15,%rcx,8), %rdi
	movq	(%r13,%rcx,8), %rsi
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %rdx
	movq	(%r14,%rax,8), %rcx
	callq	sbitmap_union_of_diff
	testq	%r12, %rax
	je	.LBB0_31
# BB#26:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB0_25 Depth=2
	movq	24(%rbx), %rax
	movabsq	$entry_exit_blocks+112, %rcx
	cmpq	%rcx, %rax
	je	.LBB0_31
# BB#27:                                # %land.lhs.true65.i
                                        #   in Loop: Header=BB0_25 Depth=2
	cmpq	$0, 80(%rax)
	jne	.LBB0_31
# BB#28:                                # %if.then70.i
                                        #   in Loop: Header=BB0_25 Depth=2
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	%rax, (%rdx)
	movq	24(%rbx), %rcx
	leaq	8(%rdx), %rax
	movq	64(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %rax
	movq	%rbx, 80(%rcx)
	jb	.LBB0_30
# BB#29:                                # %if.then70.i
                                        #   in Loop: Header=BB0_25 Depth=2
	movq	32(%rsp), %rax          # 8-byte Reload
.LBB0_30:                               # %if.then70.i
                                        #   in Loop: Header=BB0_25 Depth=2
	incq	80(%rsp)                # 8-byte Folded Spill
	movq	%rax, 72(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB0_31:                               # %for.inc81.i
                                        #   in Loop: Header=BB0_25 Depth=2
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_25
.LBB0_18:                               # %while.cond.loopexit.i
                                        #   in Loop: Header=BB0_19 Depth=1
	movq	80(%rsp), %rbx          # 8-byte Reload
	testq	%r12, %rbx
	movq	48(%rsp), %rcx          # 8-byte Reload
	jne	.LBB0_19
# BB#32:                                # %while.cond.while.end_crit_edge.i
	movslq	n_basic_blocks(%rip), %rax
.LBB0_33:                               # %while.end.i
	movq	88(%rsp), %r14          # 8-byte Reload
	movq	(%r14,%rax,8), %rdi
	callq	sbitmap_ones
	movq	entry_exit_blocks+144(%rip), %rbx
	testq	%rbx, %rbx
	movq	%r15, %r12
	movq	%r14, %r15
	je	.LBB0_35
	.align	16, 0x90
.LBB0_34:                               # %for.body89.i
                                        # =>This Inner Loop Header: Depth=1
	movslq	n_basic_blocks(%rip), %rax
	movq	40(%rbx), %rcx
	movq	(%r15,%rax,8), %rdi
	movq	(%r12,%rcx,8), %rdx
	movq	%rdi, %rsi
	callq	sbitmap_a_and_b
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_34
.LBB0_35:                               # %compute_laterin.exit
	movq	144(%rsp), %r14
	callq	clear_aux_for_edges
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	%r13, %rdi
	callq	free
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, (%r14)
	movl	n_basic_blocks(%rip), %edi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, %r13
	movq	152(%rsp), %rax
	movq	%r13, (%rax)
	movslq	n_basic_blocks(%rip), %rax
	movq	(%r14), %rcx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movq	24(%rsp), %r14          # 8-byte Reload
	jle	.LBB0_38
# BB#36:
	movq	$1, %rbx
	.align	16, 0x90
.LBB0_37:                               # %for.body.i59
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r13,%rbx,8), %rdi
	movq	-8(%r14,%rbx,8), %rsi
	movq	-8(%r15,%rbx,8), %rdx
	callq	sbitmap_difference
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB0_37
.LBB0_38:                               # %for.cond5.preheader.i
	movq	16(%rsp), %rsi          # 8-byte Reload
	movslq	4(%rsi), %rax
	testq	%rax, %rax
	movq	80(%rsp), %r14          # 8-byte Reload
	jle	.LBB0_44
# BB#39:                                # %for.body7.lr.ph.i
	movq	$1, %rbx
	.align	16, 0x90
.LBB0_40:                               # %for.body7.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rsi), %rax
	movq	%rsi, %r13
	movq	-8(%rax,%rbx,8), %rax
	movq	24(%rax), %rax
	movq	-8(%r14,%rbx,8), %rdi
	movq	-8(%r12,%rbx,8), %rsi
	movabsq	$entry_exit_blocks+112, %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_42
# BB#41:                                # %if.then.i63
                                        #   in Loop: Header=BB0_40 Depth=1
	movslq	n_basic_blocks(%rip), %rax
	jmp	.LBB0_43
	.align	16, 0x90
.LBB0_42:                               # %if.else.i67
                                        #   in Loop: Header=BB0_40 Depth=1
	movslq	88(%rax), %rax
.LBB0_43:                               # %for.inc23.i
                                        #   in Loop: Header=BB0_40 Depth=1
	movq	(%r15,%rax,8), %rdx
	callq	sbitmap_difference
	movq	%r13, %rsi
	movslq	4(%rsi), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB0_40
.LBB0_44:                               # %compute_insert_delete.exit
	movq	%r15, %rdi
	movq	%rsi, %rbx
	callq	free
	movq	%r12, %rdi
	callq	free
	movq	%rbx, %rax
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	pre_edge_lcm, .Ltmp17-pre_edge_lcm
	.cfi_endproc

	.globl	compute_available
	.align	16, 0x90
	.type	compute_available,@function
compute_available:                      # @compute_available
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp24:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp25:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp26:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp27:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp28:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp29:
	.cfi_def_cfa_offset 112
.Ltmp30:
	.cfi_offset %rbx, -48
.Ltmp31:
	.cfi_offset %r12, -40
.Ltmp32:
	.cfi_offset %r13, -32
.Ltmp33:
	.cfi_offset %r14, -24
.Ltmp34:
	.cfi_offset %r15, -16
	movslq	n_basic_blocks(%rip), %rax
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	%rdx, %r14
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	$3, %rcx
	shlq	%cl, %rax
	movq	%rax, %rdi
	callq	xmalloc
	movl	n_basic_blocks(%rip), %esi
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	sbitmap_vector_ones
	movslq	n_basic_blocks(%rip), %rsi
	testq	%rsi, %rsi
	jle	.LBB1_3
# BB#1:
	xorq	%rax, %rax
	.align	16, 0x90
.LBB1_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rcx
	movq	basic_block_info(%rip), %rdx
	movq	%rcx, (%rbx,%rax,8)
	movq	32(%rdx,%rax,8), %rcx
	movq	%rcx, 80(%rcx)
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rsi, %rdx
	jl	.LBB1_2
.LBB1_3:                                # %for.end
	movq	%r14, %r12
	movq	entry_exit_blocks+40(%rip), %rax
	leaq	(%rbx,%rsi,8), %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	jmp	.LBB1_5
	.align	16, 0x90
.LBB1_4:                                # %for.body14
                                        #   in Loop: Header=BB1_5 Depth=1
	movq	24(%rax), %rcx
	movq	$entry_exit_blocks, 80(%rcx)
	movq	8(%rax), %rax
.LBB1_5:                                # %for.body14
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB1_4
# BB#6:
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	%rbx, %rcx
	jmp	.LBB1_7
	.align	16, 0x90
.LBB1_25:                               #   in Loop: Header=BB1_7 Depth=1
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	%r8, %rcx
.LBB1_7:                                # %while.cond.outer.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_8 Depth 2
                                        #     Child Loop BB1_17 Depth 2
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB1_8:                                # %while.cond
                                        #   Parent Loop BB1_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rdx, %rsi
	je	.LBB1_26
# BB#9:                                 # %while.body
                                        #   in Loop: Header=BB1_8 Depth=2
	movq	(%rcx), %rbx
	movslq	88(%rbx), %r15
	leaq	8(%rcx), %r14
	movq	40(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r14
	movq	56(%rsp), %rcx          # 8-byte Reload
	jb	.LBB1_11
# BB#10:                                # %while.body
                                        #   in Loop: Header=BB1_8 Depth=2
	movq	16(%rsp), %r14          # 8-byte Reload
.LBB1_11:                               # %while.body
                                        #   in Loop: Header=BB1_8 Depth=2
	movq	%r14, 48(%rsp)          # 8-byte Spill
	movq	80(%rbx), %rax
	decq	%rsi
	movq	%rsi, %r14
	movabsq	$entry_exit_blocks, %rdx
	cmpq	%rdx, %rax
	jne	.LBB1_13
# BB#12:                                # %if.then25
                                        #   in Loop: Header=BB1_8 Depth=2
	movq	(%rcx,%r15,8), %rdi
	movq	%rcx, %r13
	callq	sbitmap_zero
	jmp	.LBB1_14
	.align	16, 0x90
.LBB1_13:                               # %if.else
                                        #   in Loop: Header=BB1_8 Depth=2
	movq	$0, 80(%rbx)
	movq	(%rcx,%r15,8), %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	%rcx, %r13
	callq	sbitmap_intersection_of_preds
.LBB1_14:                               # %if.end31
                                        #   in Loop: Header=BB1_8 Depth=2
	movq	%r13, 56(%rsp)          # 8-byte Spill
	movq	(%r12,%r15,8), %rdi
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r15,8), %rsi
	movq	(%r13,%r15,8), %rdx
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r15,8), %rcx
	callq	sbitmap_union_of_diff
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	movq	%r14, %rsi
	movq	48(%rsp), %rcx          # 8-byte Reload
	je	.LBB1_8
# BB#15:                                # %if.then42
                                        #   in Loop: Header=BB1_8 Depth=2
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.LBB1_8
# BB#16:                                #   in Loop: Header=BB1_7 Depth=1
	movq	%rcx, %r8
	movabsq	$entry_exit_blocks+112, %rcx
	movq	8(%rsp), %rbx           # 8-byte Reload
	.align	16, 0x90
.LBB1_17:                               # %for.body45
                                        #   Parent Loop BB1_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rsi, %rdi
	movq	24(%rax), %rdx
	cmpq	$0, 80(%rdx)
	jne	.LBB1_18
# BB#19:                                # %for.body45
                                        #   in Loop: Header=BB1_17 Depth=2
	cmpq	%rcx, %rdx
	je	.LBB1_20
# BB#21:                                # %if.then52
                                        #   in Loop: Header=BB1_17 Depth=2
	movq	%rdx, (%rbx)
	movq	24(%rax), %rsi
	leaq	8(%rbx), %rdx
	movq	40(%rsp), %rbx          # 8-byte Reload
	cmpq	%rbx, %rdx
	movq	%rax, 80(%rsi)
	jb	.LBB1_23
# BB#22:                                # %if.then52
                                        #   in Loop: Header=BB1_17 Depth=2
	movq	16(%rsp), %rdx          # 8-byte Reload
.LBB1_23:                               # %if.then52
                                        #   in Loop: Header=BB1_17 Depth=2
	movq	%rdi, %rsi
	incq	%rsi
	movq	%rdx, %rbx
	jmp	.LBB1_24
	.align	16, 0x90
.LBB1_18:                               #   in Loop: Header=BB1_17 Depth=2
	movq	%rdi, %rsi
	jmp	.LBB1_24
	.align	16, 0x90
.LBB1_20:                               #   in Loop: Header=BB1_17 Depth=2
	movq	%rdi, %rsi
.LBB1_24:                               # %for.inc63
                                        #   in Loop: Header=BB1_17 Depth=2
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB1_17
	jmp	.LBB1_25
.LBB1_26:                               # %while.end
	callq	clear_aux_for_edges
	callq	clear_aux_for_blocks
	movq	16(%rsp), %rdi          # 8-byte Reload
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.Ltmp35:
	.size	compute_available, .Ltmp35-compute_available
	.cfi_endproc

	.align	16, 0x90
	.type	compute_antinout_edge,@function
compute_antinout_edge:                  # @compute_antinout_edge
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp42:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp43:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp44:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp45:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp46:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp47:
	.cfi_def_cfa_offset 112
.Ltmp48:
	.cfi_offset %rbx, -48
.Ltmp49:
	.cfi_offset %r12, -40
.Ltmp50:
	.cfi_offset %r13, -32
.Ltmp51:
	.cfi_offset %r14, -24
.Ltmp52:
	.cfi_offset %r15, -16
	movslq	n_basic_blocks(%rip), %rax
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	%rdx, %r14
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	$3, %rcx
	shlq	%cl, %rax
	movq	%rax, %rdi
	callq	xmalloc
	movl	n_basic_blocks(%rip), %esi
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	sbitmap_vector_ones
	movslq	n_basic_blocks(%rip), %rdi
	testq	%rdi, %rdi
	jle	.LBB2_3
# BB#1:                                 # %for.body.preheader
	leaq	-1(%rdi), %rax
	movq	%rbx, %rdx
	.align	16, 0x90
.LBB2_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rcx
	movq	%rcx, (%rdx)
	addq	$8, %rdx
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rcx
	movq	%rcx, 80(%rcx)
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	decq	%rax
	testq	%rsi, %rsi
	jg	.LBB2_2
.LBB2_3:                                # %for.end
	movq	%r14, %r15
	movq	entry_exit_blocks+144(%rip), %rax
	movq	%rdi, %rdx
	leaq	(%rbx,%rdx,8), %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	jmp	.LBB2_5
	.align	16, 0x90
.LBB2_4:                                # %for.body14
                                        #   in Loop: Header=BB2_5 Depth=1
	movq	16(%rax), %rcx
	movq	$entry_exit_blocks+112, 80(%rcx)
	movq	(%rax), %rax
.LBB2_5:                                # %for.body14
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB2_4
# BB#6:
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	%rbx, %rcx
	jmp	.LBB2_7
	.align	16, 0x90
.LBB2_23:                               #   in Loop: Header=BB2_7 Depth=1
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %rdx
	movq	%r8, %rcx
.LBB2_7:                                # %while.cond.outer.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_8 Depth 2
                                        #     Child Loop BB2_17 Depth 2
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB2_8:                                # %while.cond
                                        #   Parent Loop BB2_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rsi, %rdx
	je	.LBB2_24
# BB#9:                                 # %while.body
                                        #   in Loop: Header=BB2_8 Depth=2
	movq	(%rcx), %rbx
	movslq	88(%rbx), %r13
	leaq	8(%rcx), %r14
	movq	40(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r14
	movq	56(%rsp), %rcx          # 8-byte Reload
	jb	.LBB2_11
# BB#10:                                # %while.body
                                        #   in Loop: Header=BB2_8 Depth=2
	movq	16(%rsp), %r14          # 8-byte Reload
.LBB2_11:                               # %while.body
                                        #   in Loop: Header=BB2_8 Depth=2
	movq	%r14, 48(%rsp)          # 8-byte Spill
	movq	80(%rbx), %rax
	decq	%rdx
	movq	%rdx, %r14
	movabsq	$entry_exit_blocks+112, %rdx
	cmpq	%rdx, %rax
	jne	.LBB2_13
# BB#12:                                # %if.then26
                                        #   in Loop: Header=BB2_8 Depth=2
	movq	(%rcx,%r13,8), %rdi
	movq	%rcx, %r12
	callq	sbitmap_zero
	jmp	.LBB2_14
	.align	16, 0x90
.LBB2_13:                               # %if.else
                                        #   in Loop: Header=BB2_8 Depth=2
	movq	$0, 80(%rbx)
	movq	(%rcx,%r13,8), %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	movq	%rcx, %r12
	callq	sbitmap_intersection_of_succs
.LBB2_14:                               # %if.end32
                                        #   in Loop: Header=BB2_8 Depth=2
	movq	%r12, 56(%rsp)          # 8-byte Spill
	movq	(%r15,%r13,8), %rdi
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r13,8), %rsi
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r13,8), %rdx
	movq	(%r12,%r13,8), %rcx
	callq	sbitmap_a_or_b_and_c
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rax
	movq	%r14, %rdx
	movq	48(%rsp), %rcx          # 8-byte Reload
	je	.LBB2_8
# BB#15:                                # %if.then43
                                        #   in Loop: Header=BB2_8 Depth=2
	movq	32(%rbx), %rax
	testq	%rax, %rax
	je	.LBB2_8
# BB#16:                                #   in Loop: Header=BB2_7 Depth=1
	movq	%rcx, %r8
	movq	%rdx, %rdi
	movabsq	$entry_exit_blocks, %rcx
	movq	8(%rsp), %rbx           # 8-byte Reload
	.align	16, 0x90
.LBB2_17:                               # %for.body46
                                        #   Parent Loop BB2_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	16(%rax), %rdx
	cmpq	$0, 80(%rdx)
	jne	.LBB2_22
# BB#18:                                # %for.body46
                                        #   in Loop: Header=BB2_17 Depth=2
	cmpq	%rcx, %rdx
	je	.LBB2_22
# BB#19:                                # %if.then53
                                        #   in Loop: Header=BB2_17 Depth=2
	movq	%rdx, (%rbx)
	movq	16(%rax), %rsi
	leaq	8(%rbx), %rdx
	movq	40(%rsp), %rbx          # 8-byte Reload
	cmpq	%rbx, %rdx
	movq	%rax, 80(%rsi)
	jb	.LBB2_21
# BB#20:                                # %if.then53
                                        #   in Loop: Header=BB2_17 Depth=2
	movq	16(%rsp), %rdx          # 8-byte Reload
.LBB2_21:                               # %if.then53
                                        #   in Loop: Header=BB2_17 Depth=2
	incq	%rdi
	movq	%rdx, %rbx
.LBB2_22:                               # %for.inc63
                                        #   in Loop: Header=BB2_17 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB2_17
	jmp	.LBB2_23
.LBB2_24:                               # %while.end
	callq	clear_aux_for_edges
	callq	clear_aux_for_blocks
	movq	16(%rsp), %rdi          # 8-byte Reload
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.Ltmp53:
	.size	compute_antinout_edge, .Ltmp53-compute_antinout_edge
	.cfi_endproc

	.globl	pre_edge_rev_lcm
	.align	16, 0x90
	.type	pre_edge_rev_lcm,@function
pre_edge_rev_lcm:                       # @pre_edge_rev_lcm
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp60:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp61:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp62:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp63:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp64:
	.cfi_def_cfa_offset 48
	subq	$112, %rsp
.Ltmp65:
	.cfi_def_cfa_offset 160
.Ltmp66:
	.cfi_offset %rbx, -48
.Ltmp67:
	.cfi_offset %r12, -40
.Ltmp68:
	.cfi_offset %r13, -32
.Ltmp69:
	.cfi_offset %r14, -24
.Ltmp70:
	.cfi_offset %r15, -16
	movq	%r9, 72(%rsp)           # 8-byte Spill
	movq	%r8, 104(%rsp)          # 8-byte Spill
	movq	%rcx, %rbx
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movq	%rdx, 96(%rsp)          # 8-byte Spill
	movq	%rsi, %r12
	movq	%r12, 32(%rsp)          # 8-byte Spill
	callq	create_edge_list
	movq	%rax, %r13
	movl	4(%r13), %eax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movl	n_basic_blocks(%rip), %edi
	movq	%r12, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, %r15
	movq	%r15, 56(%rsp)          # 8-byte Spill
	movq	%r12, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %esi
	movq	%rax, %r14
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rdi
	callq	sbitmap_vector_zero
	movl	n_basic_blocks(%rip), %esi
	movq	%r14, %rdi
	callq	sbitmap_vector_zero
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	compute_antinout_edge
	movl	n_basic_blocks(%rip), %edi
	movq	%r12, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, %r14
	movq	%r14, 96(%rsp)          # 8-byte Spill
	movq	%r12, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	compute_available
	movl	4(%r13), %edi
	movq	%r12, %rsi
	callq	sbitmap_vector_alloc
	movslq	4(%r13), %rbx
	movq	%rbx, 104(%rsp)         # 8-byte Spill
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	callq	sbitmap_alloc
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	callq	sbitmap_alloc
	movq	%r13, %r12
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rbx, %rbx
	jle	.LBB3_9
# BB#1:                                 # %for.body.lr.ph.i
	movq	$1, %r15
	movabsq	$entry_exit_blocks+112, %r13
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB3_2:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r12), %rax
	movq	-8(%rax,%r15,8), %rax
	movq	24(%rax), %rbx
	movq	16(%rax), %rax
	cmpq	%r13, %rbx
	jne	.LBB3_4
# BB#3:                                 # %if.then.i
                                        #   in Loop: Header=BB3_2 Depth=1
	movslq	88(%rax), %rax
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	-8(%rcx,%r15,8), %rdi
	movq	96(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %rsi
	callq	sbitmap_copy
	jmp	.LBB3_8
	.align	16, 0x90
.LBB3_4:                                # %if.else.i
                                        #   in Loop: Header=BB3_2 Depth=1
	movabsq	$entry_exit_blocks, %rcx
	cmpq	%rcx, %rax
	jne	.LBB3_6
# BB#5:                                 # %if.then12.i
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	-8(%rax,%r15,8), %rdi
	callq	sbitmap_zero
	jmp	.LBB3_7
	.align	16, 0x90
.LBB3_6:                                # %if.else15.i
                                        #   in Loop: Header=BB3_2 Depth=1
	movslq	88(%rax), %rax
	movslq	88(%rbx), %rcx
	movq	96(%rsp), %rdx          # 8-byte Reload
	movq	(%rdx,%rax,8), %rsi
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%rcx,8), %rdx
	movq	80(%rsp), %rdi          # 8-byte Reload
	callq	sbitmap_difference
	movslq	88(%rbx), %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %rsi
	movq	40(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdi
	callq	sbitmap_not
	movslq	88(%rbx), %rax
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	-8(%rcx,%r15,8), %rdi
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %rdx
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	%r13, %rcx
	callq	sbitmap_a_and_b_or_c
.LBB3_7:                                # %for.inc.i
                                        #   in Loop: Header=BB3_2 Depth=1
	movabsq	$entry_exit_blocks+112, %r13
.LBB3_8:                                # %for.inc.i
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	%r15, %rax
	andq	%r14, %rax
	movq	104(%rsp), %rcx         # 8-byte Reload
	andq	%r14, %rcx
	incq	%r15
	cmpq	%rcx, %rax
	jne	.LBB3_2
.LBB3_9:                                # %compute_farthest.exit
	movq	%r12, 16(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %r14
	callq	free
	movq	80(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	free
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, %r12
	incq	%rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, %r15
	movl	n_basic_blocks(%rip), %edi
	movslq	4(%r14), %rbx
	incq	%rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, %r14
	movq	%r14, 72(%rsp)          # 8-byte Spill
	testq	%rbx, %rbx
	jle	.LBB3_12
# BB#10:                                # %for.body.lr.ph.i49
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	xorq	%rcx, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB3_11:                               # %for.body.i53
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rcx,8), %rsi
	movq	%rcx, 40(%rsi)
	incq	%rcx
	movq	%rcx, %rsi
	andq	%rdx, %rsi
	movq	%rbx, %rdi
	andq	%rdx, %rdi
	cmpq	%rdi, %rsi
	jne	.LBB3_11
.LBB3_12:                               # %for.end.i
	movq	%r15, 104(%rsp)         # 8-byte Spill
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_ones
	movq	entry_exit_blocks+144(%rip), %rbx
	testq	%rbx, %rbx
	movq	88(%rsp), %r13          # 8-byte Reload
	je	.LBB3_13
	.align	16, 0x90
.LBB3_44:                               # %for.body5.i
                                        # =>This Inner Loop Header: Depth=1
	movq	40(%rbx), %rax
	movq	(%r12,%rax,8), %rdi
	movq	(%r13,%rax,8), %rsi
	callq	sbitmap_copy
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB3_44
.LBB3_13:                               # %for.cond12.preheader.i
	movq	%r12, %r15
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB3_32
# BB#14:                                # %for.body15.lr.ph.i
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	cmpq	$1, %rax
	jg	.LBB3_15
# BB#16:                                # %for.body15.lr.ph.i
	movq	$8, %rdi
	jmp	.LBB3_17
.LBB3_15:
	leaq	-1(%rax), %rcx
	andq	%r12, %rcx
	leaq	8(,%rcx,8), %rdi
.LBB3_17:                               # %for.body15.lr.ph.i
	xorq	%rcx, %rcx
	movq	%rax, %rdx
	andq	%r12, %rdx
	.align	16, 0x90
.LBB3_18:                               # %for.body15.i
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rsi
	movq	32(%rsi,%rcx,8), %rsi
	movq	%rsi, (%r14,%rcx,8)
	incq	%rcx
	movq	%rsi, 80(%rsi)
	movq	%rcx, %rsi
	andq	%r12, %rsi
	cmpq	%rdx, %rsi
	jne	.LBB3_18
# BB#19:                                # %while.cond.preheader.i
	testq	%rdi, %rdi
	je	.LBB3_32
# BB#20:                                # %while.body.i.preheader
	addq	%r14, %rdi
	.align	16, 0x90
.LBB3_22:                               # %while.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_24 Depth 2
                                        #     Child Loop BB3_26 Depth 2
	movq	%rdi, 96(%rsp)          # 8-byte Spill
	movq	-8(%rdi), %r13
	movq	%r13, 80(%rsp)          # 8-byte Spill
	movq	$0, 80(%r13)
	movslq	88(%r13), %rbx
	movq	104(%rsp), %r14         # 8-byte Reload
	movq	(%r14,%rbx,8), %rdi
	callq	sbitmap_ones
	movq	40(%r13), %r13
	jmp	.LBB3_24
	.align	16, 0x90
.LBB3_23:                               # %for.body33.i
                                        #   in Loop: Header=BB3_24 Depth=2
	movq	40(%r13), %rax
	movq	(%r14,%rbx,8), %rdi
	movq	(%r15,%rax,8), %rdx
	movq	%rdi, %rsi
	callq	sbitmap_a_and_b
	movq	8(%r13), %r13
.LBB3_24:                               # %for.body33.i
                                        #   Parent Loop BB3_22 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r13, %r13
	jne	.LBB3_23
# BB#25:                                # %for.end42.i
                                        #   in Loop: Header=BB3_22 Depth=1
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %r14
	addq	$-8, 96(%rsp)           # 8-byte Folded Spill
	testq	%r14, %r14
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	88(%rsp), %r13          # 8-byte Reload
	je	.LBB3_21
	.align	16, 0x90
.LBB3_26:                               # %for.body46.i
                                        #   Parent Loop BB3_22 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%r14), %rax
	movq	40(%r14), %rcx
	movslq	88(%rax), %rax
	movq	(%r15,%rcx,8), %rdi
	movq	(%r13,%rcx,8), %rsi
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx,%rax,8), %rdx
	movq	(%rbx,%rax,8), %rcx
	callq	sbitmap_union_of_diff
	testq	%r12, %rax
	je	.LBB3_30
# BB#27:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB3_26 Depth=2
	movq	16(%r14), %rax
	movabsq	$entry_exit_blocks, %rcx
	cmpq	%rcx, %rax
	je	.LBB3_30
# BB#28:                                # %land.lhs.true62.i
                                        #   in Loop: Header=BB3_26 Depth=2
	cmpq	$0, 80(%rax)
	jne	.LBB3_30
# BB#29:                                # %if.then.i62
                                        #   in Loop: Header=BB3_26 Depth=2
	movq	96(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
	movq	16(%r14), %rax
	addq	$8, %rcx
	movq	%rcx, 96(%rsp)          # 8-byte Spill
	movq	%r14, 80(%rax)
	.align	16, 0x90
.LBB3_30:                               # %for.inc71.i
                                        #   in Loop: Header=BB3_26 Depth=2
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB3_26
.LBB3_21:                               # %while.cond.loopexit.i
                                        #   in Loop: Header=BB3_22 Depth=1
	movq	72(%rsp), %r14          # 8-byte Reload
	movq	96(%rsp), %rdi          # 8-byte Reload
	cmpq	%r14, %rdi
	jne	.LBB3_22
# BB#31:                                # %while.cond.while.end_crit_edge.i
	movl	n_basic_blocks(%rip), %eax
.LBB3_32:                               # %while.end.i
	movq	%r14, 72(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	104(%rsp), %r14         # 8-byte Reload
	movq	(%r14,%rax,8), %rdi
	callq	sbitmap_ones
	movq	entry_exit_blocks+40(%rip), %rbx
	testq	%rbx, %rbx
	movq	%r15, %r12
	movq	%r14, %r15
	je	.LBB3_34
	.align	16, 0x90
.LBB3_33:                               # %for.body79.i
                                        # =>This Inner Loop Header: Depth=1
	movslq	n_basic_blocks(%rip), %rax
	movq	40(%rbx), %rcx
	movq	(%r15,%rax,8), %rdi
	movq	(%r12,%rcx,8), %rdx
	movq	%rdi, %rsi
	callq	sbitmap_a_and_b
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB3_33
.LBB3_34:                               # %compute_nearerout.exit
	movq	160(%rsp), %r14
	callq	clear_aux_for_edges
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	%r13, %rdi
	callq	free
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, (%r14)
	movl	n_basic_blocks(%rip), %edi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, %r13
	movq	168(%rsp), %rax
	movq	%r13, (%rax)
	movslq	n_basic_blocks(%rip), %rax
	movq	(%r14), %rcx
	movq	%rcx, 96(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movq	64(%rsp), %r14          # 8-byte Reload
	jle	.LBB3_37
# BB#35:
	movq	$1, %rbx
	.align	16, 0x90
.LBB3_36:                               # %for.body.i65
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r13,%rbx,8), %rdi
	movq	-8(%r14,%rbx,8), %rsi
	movq	-8(%r15,%rbx,8), %rdx
	callq	sbitmap_difference
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB3_36
.LBB3_37:                               # %for.cond5.preheader.i
	movq	16(%rsp), %rsi          # 8-byte Reload
	movslq	4(%rsi), %rax
	testq	%rax, %rax
	movq	96(%rsp), %r14          # 8-byte Reload
	jle	.LBB3_43
# BB#38:                                # %for.body7.lr.ph.i
	movq	$1, %rbx
	.align	16, 0x90
.LBB3_39:                               # %for.body7.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rsi), %rax
	movq	%rsi, %r13
	movq	-8(%rax,%rbx,8), %rax
	movq	16(%rax), %rax
	movq	-8(%r14,%rbx,8), %rdi
	movq	-8(%r12,%rbx,8), %rsi
	movabsq	$entry_exit_blocks, %rcx
	cmpq	%rcx, %rax
	jne	.LBB3_41
# BB#40:                                # %if.then.i70
                                        #   in Loop: Header=BB3_39 Depth=1
	movslq	n_basic_blocks(%rip), %rax
	jmp	.LBB3_42
	.align	16, 0x90
.LBB3_41:                               # %if.else.i72
                                        #   in Loop: Header=BB3_39 Depth=1
	movslq	88(%rax), %rax
.LBB3_42:                               # %for.inc23.i
                                        #   in Loop: Header=BB3_39 Depth=1
	movq	(%r15,%rax,8), %rdx
	callq	sbitmap_difference
	movq	%r13, %rsi
	movslq	4(%rsi), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB3_39
.LBB3_43:                               # %compute_rev_insert_delete.exit
	movq	%r15, %rdi
	movq	%rsi, %rbx
	callq	free
	movq	%r12, %rdi
	callq	free
	movq	%rbx, %rax
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp71:
	.size	pre_edge_rev_lcm, .Ltmp71-pre_edge_rev_lcm
	.cfi_endproc

	.globl	optimize_mode_switching
	.align	16, 0x90
	.type	optimize_mode_switching,@function
optimize_mode_switching:                # @optimize_mode_switching
	.cfi_startproc
# BB#0:                                 # %for.body17
	pushq	%r15
.Ltmp78:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp79:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp80:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp81:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp82:
	.cfi_def_cfa_offset 48
	subq	$112, %rsp
.Ltmp83:
	.cfi_def_cfa_offset 160
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
	movslq	n_basic_blocks(%rip), %rdi
	movq	$16, %rsi
	callq	xcalloc
	movq	%rax, %r15
	movq	%r15, 56(%rsp)          # 8-byte Spill
	movl	n_basic_blocks(%rip), %edi
	movq	%r15, 96(%rsp)
	xorq	%r13, %r13
	movl	%r13d, 108(%rsp)
	movq	$1, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, antic(%rip)
	movq	$1, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, transp(%rip)
	movq	$1, %rsi
	callq	sbitmap_vector_alloc
	movq	transp(%rip), %rdi
	movl	n_basic_blocks(%rip), %esi
	movq	%rax, comp(%rip)
	callq	sbitmap_vector_ones
	movslq	n_basic_blocks(%rip), %rdi
	testq	%rdi, %rdi
	jle	.LBB4_38
# BB#1:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB4_2:                                # %for.body28
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_5 Depth 2
                                        #       Child Loop BB4_20 Depth 3
                                        #       Child Loop BB4_25 Depth 3
                                        #       Child Loop BB4_28 Depth 3
                                        #     Child Loop BB4_34 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	$0, 88(%rsp)
	movq	32(%rax,%r13,8), %rax
	movq	64(%rax), %rsi
	leaq	88(%rsp), %rdi
	callq	reg_set_to_hard_reg_set
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r13,8), %rax
	movq	(%rax), %rbx
	movq	$4, %rcx
	testq	%rbx, %rbx
	jne	.LBB4_4
# BB#3:                                 # %for.end154.thread
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$2, %rcx
	movl	%ecx, 8(%r15,%rax)
	xorq	%rbx, %rbx
	jmp	.LBB4_32
	.align	16, 0x90
.LBB4_4:                                # %land.rhs.lr.ph
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%r13, %rdx
	shlq	%cl, %rdx
	addq	%r15, %rdx
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	$2, %r12
	jmp	.LBB4_5
	.align	16, 0x90
.LBB4_93:                               # %for.inc150.land.rhs_crit_edge
                                        #   in Loop: Header=BB4_5 Depth=2
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r13,8), %rax
.LBB4_5:                                # %land.rhs
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB4_20 Depth 3
                                        #       Child Loop BB4_25 Depth 3
                                        #       Child Loop BB4_28 Depth 3
	movq	8(%rax), %rax
	movq	24(%rax), %rax
	cmpq	%rax, %rbx
	je	.LBB4_31
# BB#6:                                 # %for.body46
                                        #   in Loop: Header=BB4_5 Depth=2
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %ecx
	cmpq	$105, %rcx
	jne	.LBB4_29
# BB#7:                                 # %if.then52
                                        #   in Loop: Header=BB4_5 Depth=2
	movq	$1, %r15
	cmpq	$34, %rax
	je	.LBB4_16
# BB#8:                                 # %if.then52
                                        #   in Loop: Header=BB4_5 Depth=2
	cmpq	$32, %rax
	jne	.LBB4_11
# BB#9:                                 # %land.lhs.true
                                        #   in Loop: Header=BB4_5 Depth=2
	movq	32(%rbx), %rdi
	callq	asm_noperands
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB4_16
# BB#10:                                # %lor.lhs.false67
                                        #   in Loop: Header=BB4_5 Depth=2
	movq	32(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$40, %rax
	je	.LBB4_16
.LBB4_11:                               # %cond.false
                                        #   in Loop: Header=BB4_5 Depth=2
	movslq	40(%rbx), %rax
	testq	%rax, %rax
	jns	.LBB4_13
# BB#12:                                # %cond.end
                                        #   in Loop: Header=BB4_5 Depth=2
	movq	%rbx, %rdi
	callq	recog_memoized_1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB4_23
.LBB4_13:                               # %lor.rhs
                                        #   in Loop: Header=BB4_5 Depth=2
	movq	%rbx, %rdi
	callq	get_attr_type
	andq	%r14, %rax
	cmpq	$35, %rax
	movabsq	$0, %r15
	je	.LBB4_15
# BB#14:                                # %lor.rhs
                                        #   in Loop: Header=BB4_5 Depth=2
	movabsq	$1, %r15
.LBB4_15:                               # %lor.rhs
                                        #   in Loop: Header=BB4_5 Depth=2
	addq	%r15, %r15
.LBB4_16:                               # %cond.end91
                                        #   in Loop: Header=BB4_5 Depth=2
	movq	%r15, %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	je	.LBB4_23
# BB#17:                                # %cond.end91
                                        #   in Loop: Header=BB4_5 Depth=2
	movq	%r12, %rcx
	andq	%r14, %rcx
	cmpq	%rcx, %rax
	je	.LBB4_23
# BB#18:                                # %if.then98
                                        #   in Loop: Header=BB4_5 Depth=2
	movq	88(%rsp), %r12
	movq	$40, %rdi
	callq	xmalloc
	movl	%r15d, (%rax)
	movq	%rbx, 8(%rax)
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rcx
	movl	%r13d, 16(%rax)
	movq	$0, 24(%rax)
	movq	%r12, 32(%rax)
	testq	%rcx, %rcx
	jne	.LBB4_20
# BB#19:                                # %if.then.i
                                        #   in Loop: Header=BB4_5 Depth=2
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
	jmp	.LBB4_22
	.align	16, 0x90
.LBB4_20:                               # %while.cond.i
                                        #   Parent Loop BB4_2 Depth=1
                                        #     Parent Loop BB4_5 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rcx, %rdx
	movq	24(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB4_20
# BB#21:                                # %while.end.i
                                        #   in Loop: Header=BB4_5 Depth=2
	movq	%rax, 24(%rdx)
.LBB4_22:                               # %add_seginfo.exit
                                        #   in Loop: Header=BB4_5 Depth=2
	movq	transp(%rip), %rax
	movq	(%rax,%r13,8), %rax
	andq	$-2, 16(%rax)
	movq	%r15, %r12
.LBB4_23:                               # %if.end104
                                        #   in Loop: Header=BB4_5 Depth=2
	movq	56(%rbx), %rax
	jmp	.LBB4_25
	.align	16, 0x90
.LBB4_24:                               # %for.body110
                                        #   in Loop: Header=BB4_25 Depth=3
	movq	16(%rax), %rax
.LBB4_25:                               # %for.body110
                                        #   Parent Loop BB4_2 Depth=1
                                        #     Parent Loop BB4_5 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rax, %rax
	jne	.LBB4_24
# BB#26:                                # %for.end124
                                        #   in Loop: Header=BB4_5 Depth=2
	movq	32(%rbx), %rdi
	movabsq	$reg_becomes_live, %rsi
	leaq	88(%rsp), %rdx
	callq	note_stores
	movq	56(%rbx), %rax
	jmp	.LBB4_28
	.align	16, 0x90
.LBB4_27:                               # %for.body133
                                        #   in Loop: Header=BB4_28 Depth=3
	movq	16(%rax), %rax
.LBB4_28:                               # %for.body133
                                        #   Parent Loop BB4_2 Depth=1
                                        #     Parent Loop BB4_5 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rax, %rax
	jne	.LBB4_27
.LBB4_29:                               # %for.inc150
                                        #   in Loop: Header=BB4_5 Depth=2
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB4_93
# BB#30:                                #   in Loop: Header=BB4_2 Depth=1
	xorq	%rbx, %rbx
.LBB4_31:                               # %for.end154
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	$4, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	movl	%r12d, 8(%rcx,%rax)
	andq	%r14, %r12
	cmpq	$2, %r12
	movq	%rcx, %r15
	jne	.LBB4_37
.LBB4_32:                               # %if.then159
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%r14, %r12
	movq	88(%rsp), %r14
	movq	$40, %rdi
	callq	xmalloc
	movq	$2, %rcx
	movl	%ecx, (%rax)
	movq	%rbx, 8(%rax)
	movq	$4, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	(%r15,%rdx), %rcx
	movl	%r13d, 16(%rax)
	movq	$0, 24(%rax)
	movq	%r14, 32(%rax)
	testq	%rcx, %rcx
	jne	.LBB4_34
# BB#33:                                # %if.then.i617
                                        #   in Loop: Header=BB4_2 Depth=1
	addq	%r15, %rdx
	movq	%rax, (%rdx)
	jmp	.LBB4_36
	.align	16, 0x90
.LBB4_34:                               # %while.cond.i621
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, %rdx
	movq	24(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB4_34
# BB#35:                                # %while.end.i622
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%rax, 24(%rdx)
.LBB4_36:                               # %for.inc164
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%r12, %r14
.LBB4_37:                               # %for.inc164
                                        #   in Loop: Header=BB4_2 Depth=1
	incq	%r13
	movslq	n_basic_blocks(%rip), %rdi
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdi, %rax
	jl	.LBB4_2
.LBB4_38:                               # %for.body174.lr.ph
	movq	$1, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, %r14
	movq	%r14, 32(%rsp)          # 8-byte Spill
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	xorq	%r15, %r15
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB4_39:                               # %for.body179.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_41 Depth 2
                                        #     Child Loop BB4_48 Depth 2
                                        #     Child Loop BB4_50 Depth 2
                                        #       Child Loop BB4_51 Depth 3
                                        #     Child Loop BB4_69 Depth 2
	movq	antic(%rip), %rdi
	movl	n_basic_blocks(%rip), %esi
	callq	sbitmap_vector_zero
	movq	comp(%rip), %rdi
	movl	n_basic_blocks(%rip), %esi
	callq	sbitmap_vector_zero
	movslq	n_basic_blocks(%rip), %rbx
	movq	antic(%rip), %r8
	movq	comp(%rip), %rcx
	testq	%rbx, %rbx
	movl	%r15d, 84(%rsp)
	jle	.LBB4_46
# BB#40:                                # %for.body188.lr.ph.us
                                        #   in Loop: Header=BB4_39 Depth=1
	movq	96(%rsp), %rdx
	addq	$8, %rdx
	movq	$1, %rsi
	.align	16, 0x90
.LBB4_41:                               # %for.body188.us
                                        #   Parent Loop BB4_39 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-8(%rdx), %rdi
	movl	(%rdi), %eax
	movq	%r15, %rdi
	andq	%r13, %rdi
	cmpq	%rdi, %rax
	jne	.LBB4_43
# BB#42:                                # %if.then194.us
                                        #   in Loop: Header=BB4_41 Depth=2
	movq	-8(%r8,%rsi,8), %rax
	orq	$1, 16(%rax)
.LBB4_43:                               # %if.end204.us
                                        #   in Loop: Header=BB4_41 Depth=2
	movl	(%rdx), %eax
	cmpq	%rdi, %rax
	jne	.LBB4_45
# BB#44:                                # %if.then210.us
                                        #   in Loop: Header=BB4_41 Depth=2
	movq	-8(%rcx,%rsi,8), %rax
	orq	$1, 16(%rax)
.LBB4_45:                               # %for.inc222.us
                                        #   in Loop: Header=BB4_41 Depth=2
	movq	%rsi, %rax
	andq	%r13, %rax
	movq	%rbx, %rdi
	andq	%r13, %rdi
	incq	%rsi
	addq	$16, %rdx
	cmpq	%rdi, %rax
	jne	.LBB4_41
.LBB4_46:                               # %for.end227
                                        #   in Loop: Header=BB4_39 Depth=1
	movq	%r15, 56(%rsp)          # 8-byte Spill
	testq	%rbx, %rbx
	jle	.LBB4_49
# BB#47:                                # %for.body232.lr.ph
                                        #   in Loop: Header=BB4_39 Depth=1
	decq	%rbx
	.align	16, 0x90
.LBB4_48:                               # %for.body232
                                        #   Parent Loop BB4_39 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	transp(%rip), %rax
	movq	(%r14,%rbx,8), %rdi
	movq	(%rax,%rbx,8), %rsi
	callq	sbitmap_not
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB4_48
.LBB4_49:                               # %for.body245
                                        #   in Loop: Header=BB4_39 Depth=1
	movq	transp(%rip), %rdx
	movq	comp(%rip), %rcx
	movq	antic(%rip), %r8
	movq	$delete, 8(%rsp)
	movq	$1, %rsi
	movq	$insert, (%rsp)
	movq	%r14, %r9
	callq	pre_edge_lcm
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movslq	4(%rax), %r15
	movslq	108(%rsp), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	testq	%r15, %r15
	jle	.LBB4_67
.LBB4_50:                               # %for.body255.lr.ph
                                        #   Parent Loop BB4_39 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB4_51 Depth 3
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	decq	%r15
	.align	16, 0x90
.LBB4_51:                               # %for.body255
                                        #   Parent Loop BB4_39 Depth=1
                                        #     Parent Loop BB4_50 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	(%rax,%r15,8), %r13
	movq	$0, 40(%r13)
	movq	insert(%rip), %rax
	movq	(%rax,%r15,8), %rax
	movzbl	16(%rax), %eax
	testq	$1, %rax
	jne	.LBB4_53
# BB#52:                                # %for.body255.for.cond252.backedge_crit_edge
                                        #   in Loop: Header=BB4_51 Depth=3
	movq	%r15, %rax
	jmp	.LBB4_57
	.align	16, 0x90
.LBB4_53:                               # %if.end270
                                        #   in Loop: Header=BB4_51 Depth=3
	movq	$1, 40(%r13)
	movq	16(%r13), %r14
	movl	84(%rsp), %r12d
	movq	$0, 72(%rsp)
	movq	72(%r14), %rsi
	leaq	72(%rsp), %rdi
	callq	reg_set_to_hard_reg_set
	callq	start_sequence
	testq	%r12, %r12
	jne	.LBB4_55
# BB#54:                                # %cond.true278
                                        #   in Loop: Header=BB4_51 Depth=3
	movq	$3, %rdi
	movq	$1, %rsi
	callq	assign_386_stack_local
	movq	%rax, %rbx
	movq	$3, %rdi
	movq	$2, %rsi
	callq	assign_386_stack_local
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_i387_cw_initialization
.LBB4_55:                               # %cond.end282
                                        #   in Loop: Header=BB4_51 Depth=3
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movzbl	(%rbx), %eax
	cmpq	$24, %rax
	jne	.LBB4_58
# BB#56:                                # %land.lhs.true289
                                        #   in Loop: Header=BB4_51 Depth=3
	movq	8(%rbx), %rax
	movl	(%rax), %eax
	testq	%rax, %rax
	movq	%r15, %rax
	jne	.LBB4_58
.LBB4_57:                               # %for.cond252.backedge
                                        #   in Loop: Header=BB4_51 Depth=3
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%r15
	testq	%rax, %rax
	jg	.LBB4_51
	jmp	.LBB4_66
	.align	16, 0x90
.LBB4_58:                               # %if.end294
                                        #   in Loop: Header=BB4_50 Depth=2
	movzbl	48(%r13), %eax
	testq	$2, %rax
	jne	.LBB4_59
# BB#65:                                # %if.else334
                                        #   in Loop: Header=BB4_50 Depth=2
	movq	$32, %rcx
	movq	%r15, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	insert_insn_on_edge
	testq	%r14, %r14
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	jg	.LBB4_50
	jmp	.LBB4_66
.LBB4_59:                               # %if.then297
                                        #   in Loop: Header=BB4_50 Depth=2
	movq	8(%r14), %rsi
	movzbl	(%rsi), %eax
	cmpq	$32, %rax
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jne	.LBB4_60
# BB#63:                                # %if.then311
                                        #   in Loop: Header=BB4_50 Depth=2
	movq	%rbx, %rdi
	callq	emit_insn_after
	jmp	.LBB4_64
.LBB4_60:                               # %if.then297
                                        #   in Loop: Header=BB4_50 Depth=2
	cmpq	$33, %rax
	jne	.LBB4_62
# BB#61:                                # %if.then303
                                        #   in Loop: Header=BB4_50 Depth=2
	movq	%rbx, %rdi
	callq	emit_insn_before
.LBB4_64:                               # %if.end316
                                        #   in Loop: Header=BB4_50 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movslq	88(%r14), %rdx
	movq	$32, %rcx
	movq	96(%rsp), %rsi
	sarq	%cl, %rax
	movq	$4, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movl	%r12d, 8(%rsi,%rdi)
	movq	transp(%rip), %rcx
	movq	(%rcx,%rdx,8), %rcx
	andq	$-2, 16(%rcx)
	testq	%rax, %rax
	movq	$1, 40(%rsp)            # 8-byte Folded Spill
	movq	48(%rsp), %rax          # 8-byte Reload
	jg	.LBB4_50
	jmp	.LBB4_67
	.align	16, 0x90
.LBB4_66:                               #   in Loop: Header=BB4_39 Depth=1
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	48(%rsp), %rax          # 8-byte Reload
.LBB4_67:                               # %for.end338
                                        #   in Loop: Header=BB4_39 Depth=1
	movslq	n_basic_blocks(%rip), %rbx
	testq	%rbx, %rbx
	jle	.LBB4_71
# BB#68:                                # %for.body343.lr.ph
                                        #   in Loop: Header=BB4_39 Depth=1
	movl	optimize_mode_switching.num_modes(,%rax,4), %r14d
	movq	$4, %rcx
	movq	%rbx, %r15
	shlq	%cl, %r15
	addq	$-16, %r15
	decq	%rbx
	.align	16, 0x90
.LBB4_69:                               # %for.body343
                                        #   Parent Loop BB4_39 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	delete(%rip), %rax
	movq	(%rax,%rbx,8), %rax
	movzbl	16(%rax), %eax
	testq	$1, %rax
	je	.LBB4_70
# BB#94:                                # %if.then355
                                        #   in Loop: Header=BB4_69 Depth=2
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rbx,8), %rdi
	xorq	%rsi, %rsi
	callq	make_preds_opaque
	movq	96(%rsp), %rax
	movq	(%rax,%r15), %rax
	movl	%r14d, (%rax)
.LBB4_70:                               # %for.cond340.backedge
                                        #   in Loop: Header=BB4_69 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$-16, %r15
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB4_69
.LBB4_71:                               # %for.end372
                                        #   in Loop: Header=BB4_39 Depth=1
	callq	clear_aux_for_edges
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	free_edge_list
	movq	56(%rsp), %r15          # 8-byte Reload
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	movq	32(%rsp), %r14          # 8-byte Reload
	jl	.LBB4_39
# BB#72:                                # %for.body380
	movslq	n_basic_blocks(%rip), %r15
	testq	%r15, %r15
	jle	.LBB4_88
# BB#73:
	movslq	108(%rsp), %rax
	movl	optimize_mode_switching.num_modes(,%rax,4), %r12d
	andq	%r13, %r12
	.align	16, 0x90
.LBB4_75:                               # %for.body390
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_76 Depth 2
	movq	96(%rsp), %rax
	decq	%r15
	movq	$4, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	(%rax,%rdx), %r13
	jmp	.LBB4_76
.LBB4_87:                               # %if.end446
                                        #   in Loop: Header=BB4_76 Depth=2
	movq	%r14, %rdi
	callq	free
	.align	16, 0x90
.LBB4_76:                               # %for.cond397
                                        #   Parent Loop BB4_75 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r13, %r14
	testq	%r14, %r14
	je	.LBB4_74
# BB#77:                                # %for.body399
                                        #   in Loop: Header=BB4_76 Depth=2
	movl	(%r14), %eax
	movq	24(%r14), %r13
	cmpq	%r12, %rax
	je	.LBB4_87
# BB#78:                                # %if.then404
                                        #   in Loop: Header=BB4_76 Depth=2
	callq	start_sequence
	movl	(%r14), %eax
	testq	%rax, %rax
	jne	.LBB4_80
# BB#79:                                # %cond.true409
                                        #   in Loop: Header=BB4_76 Depth=2
	movq	$3, %rdi
	movq	$1, %rsi
	callq	assign_386_stack_local
	movq	%rax, %rbx
	movq	$3, %rdi
	movq	$2, %rsi
	callq	assign_386_stack_local
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_i387_cw_initialization
.LBB4_80:                               # %cond.end413
                                        #   in Loop: Header=BB4_76 Depth=2
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movzbl	(%rbx), %eax
	cmpq	$24, %rax
	jne	.LBB4_82
# BB#81:                                # %land.lhs.true420
                                        #   in Loop: Header=BB4_76 Depth=2
	movq	8(%rbx), %rax
	movl	(%rax), %eax
	testq	%rax, %rax
	je	.LBB4_76
.LBB4_82:                               # %if.end427
                                        #   in Loop: Header=BB4_76 Depth=2
	movq	8(%r14), %rsi
	movzbl	(%rsi), %eax
	cmpq	$37, %rax
	jne	.LBB4_85
# BB#83:                                # %land.lhs.true432
                                        #   in Loop: Header=BB4_76 Depth=2
	movl	40(%rsi), %eax
	movabsq	$4294967216, %rcx       # imm = 0xFFFFFFB0
	cmpq	%rcx, %rax
	jne	.LBB4_85
# BB#84:                                # %if.then439
                                        #   in Loop: Header=BB4_76 Depth=2
	movq	%rbx, %rdi
	callq	emit_insn_after
	jmp	.LBB4_86
.LBB4_85:                               # %if.else442
                                        #   in Loop: Header=BB4_76 Depth=2
	movq	%rbx, %rdi
	callq	emit_insn_before
.LBB4_86:                               # %if.end446
                                        #   in Loop: Header=BB4_76 Depth=2
	movq	$1, 40(%rsp)            # 8-byte Folded Spill
	jmp	.LBB4_87
	.align	16, 0x90
.LBB4_74:                               # %for.cond387.loopexit
                                        #   in Loop: Header=BB4_75 Depth=1
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB4_75
.LBB4_88:                               # %for.end456
	movq	96(%rsp), %rdi
	callq	free
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	antic(%rip), %rdi
	callq	free
	movq	transp(%rip), %rdi
	callq	free
	movq	comp(%rip), %rdi
	callq	free
	movq	delete(%rip), %rdi
	callq	free
	movq	insert(%rip), %rdi
	callq	free
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	24(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB4_90
# BB#89:                                # %if.then458
	callq	commit_edge_insertions
	jmp	.LBB4_91
.LBB4_90:                               # %land.lhs.true461.critedge
	xorq	%rax, %rax
	testq	$1, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB4_92
.LBB4_91:                               # %if.end464
	callq	allocate_reg_life_data
	xorq	%rdi, %rdi
	movq	$2, %rsi
	movq	$29, %rdx
	callq	update_life_info
	movq	$1, %rax
.LBB4_92:                               # %return
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB4_62:                               # %if.else314
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.optimize_mode_switching, %rcx
	movq	$1269, %rsi             # imm = 0x4F5
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp89:
	.size	optimize_mode_switching, .Ltmp89-optimize_mode_switching
	.cfi_endproc

	.align	16, 0x90
	.type	reg_becomes_live,@function
reg_becomes_live:                       # @reg_becomes_live
	.cfi_startproc
# BB#0:                                 # %entry
	movl	(%rdi), %eax
	movq	%rdx, %r8
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB5_2
# BB#1:                                 # %if.then
	movq	8(%rdi), %rdi
	movl	(%rdi), %eax
.LBB5_2:                                # %if.end
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB5_21
# BB#3:                                 # %if.end5
	movslq	8(%rdi), %r9
	cmpq	$52, %r9
	jg	.LBB5_21
# BB#4:                                 # %if.then9
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%r9, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB5_8
# BB#5:                                 # %if.then9
	leaq	-21(%r9), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB5_8
# BB#6:                                 # %if.then9
	leaq	-45(%r9), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB5_8
# BB#7:                                 # %if.then9
	leaq	-29(%r9), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB5_8
# BB#13:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB5_14
# BB#16:                                # %cond.true44
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB5_18
.LBB5_8:                                # %cond.true
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB5_18
# BB#9:                                 # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB5_10
# BB#11:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB5_18
.LBB5_14:                               # %cond.false
	cmpq	$18, %rax
	jne	.LBB5_17
# BB#15:                                # %cond.true37
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB5_18
.LBB5_10:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB5_18
.LBB5_17:                               # %for.cond.preheader
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
	jle	.LBB5_21
.LBB5_18:                               # %for.body.lr.ph
	movq	(%r8), %rsi
	.align	16, 0x90
.LBB5_19:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %r10
	leaq	-1(%rax,%r9), %rdx
	leaq	-1(%rax), %rax
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$1, %rsi
	movq	%rdx, %rcx
	shlq	%cl, %rsi
	orq	%r10, %rsi
	testq	%rdi, %rdi
	jg	.LBB5_19
# BB#20:                                # %for.cond.if.end69.loopexit_crit_edge
	movq	%rsi, (%r8)
.LBB5_21:                               # %if.end69
	retq
.Ltmp90:
	.size	reg_becomes_live, .Ltmp90-reg_becomes_live
	.cfi_endproc

	.align	16, 0x90
	.type	make_preds_opaque,@function
make_preds_opaque:                      # @make_preds_opaque
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp97:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp98:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp99:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp100:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp101:
	.cfi_def_cfa_offset 48
.Ltmp102:
	.cfi_offset %rbx, -48
.Ltmp103:
	.cfi_offset %r12, -40
.Ltmp104:
	.cfi_offset %r13, -32
.Ltmp105:
	.cfi_offset %r14, -24
.Ltmp106:
	.cfi_offset %r15, -16
	movq	32(%rdi), %rbx
	movq	%rsi, %r14
	testq	%rbx, %rbx
	je	.LBB6_6
# BB#1:                                 # %for.body.lr.ph
	movabsq	$4294967232, %r15       # imm = 0xFFFFFFC0
	andq	%r14, %r15
	movq	$6, %rcx
	shrq	%cl, %r15
	movq	$1, %r12
	movq	%r14, %rcx
	shlq	%cl, %r12
	movq	%r12, %r13
	notq	%r13
	.align	16, 0x90
.LBB6_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, 40(%rbx)
	jne	.LBB6_5
# BB#3:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB6_2 Depth=1
	movq	16(%rbx), %rdi
	movslq	88(%rdi), %rax
	movq	transp(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	movq	16(%rax,%r15,8), %rcx
	testq	%r12, %rcx
	je	.LBB6_5
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB6_2 Depth=1
	andq	%r13, %rcx
	movq	%rcx, 16(%rax,%r15,8)
	movq	%r14, %rsi
	callq	make_preds_opaque
.LBB6_5:                                # %for.inc
                                        #   in Loop: Header=BB6_2 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB6_2
.LBB6_6:                                # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp107:
	.size	make_preds_opaque, .Ltmp107-make_preds_opaque
	.cfi_endproc

	.type	optimize_mode_switching.num_modes,@object # @optimize_mode_switching.num_modes
	.section	.rodata.cst4,"aM",@progbits,4
	.align	4
optimize_mode_switching.num_modes:
	.long	2                       # 0x2
	.size	optimize_mode_switching.num_modes, 4

	.type	antic,@object           # @antic
	.local	antic
	.comm	antic,8,8
	.type	transp,@object          # @transp
	.local	transp
	.comm	transp,8,8
	.type	comp,@object            # @comp
	.local	comp
	.comm	comp,8,8
	.type	insert,@object          # @insert
	.local	insert
	.comm	insert,8,8
	.type	delete,@object          # @delete
	.local	delete
	.comm	delete,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"lcm.c"
	.size	.L.str, 6

	.type	.L__FUNCTION__.optimize_mode_switching,@object # @__FUNCTION__.optimize_mode_switching
.L__FUNCTION__.optimize_mode_switching:
	.asciz	"optimize_mode_switching"
	.size	.L__FUNCTION__.optimize_mode_switching, 24


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
