	.file	"cfgloop.c.bc"
	.text
	.globl	flow_loop_dump
	.align	16, 0x90
	.type	flow_loop_dump,@function
flow_loop_dump:                         # @flow_loop_dump
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
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB0_28
# BB#1:                                 # %lor.lhs.false
	cmpq	$0, 8(%rbx)
	je	.LBB0_28
# BB#2:                                 # %if.end
	movq	48(%rbx), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB0_11
# BB#3:                                 # %land.lhs.true
	movq	56(%rbx), %rcx
	movq	8(%rcx), %rsi
	testq	%rsi, %rsi
	je	.LBB0_11
# BB#4:                                 # %if.then4
	movl	(%rbx), %edx
	movl	8(%rax), %ecx
	movl	152(%rbx), %eax
	movl	8(%rsi), %r8d
	testq	%rax, %rax
	je	.LBB0_6
# BB#5:
	movabsq	$.L.str1, %r9
	jmp	.LBB0_7
.LBB0_11:                               # %if.else
	movl	152(%rbx), %eax
	movl	(%rbx), %edx
	testq	%rax, %rax
	je	.LBB0_13
# BB#12:
	movabsq	$.L.str1, %rcx
	jmp	.LBB0_14
.LBB0_13:                               # %select.mid5
	movabsq	$.L.str2, %rcx
.LBB0_14:                               # %select.end4
	movl	156(%rbx), %eax
	testq	%rax, %rax
	je	.LBB0_16
# BB#15:
	movabsq	$.L.str3, %r8
	jmp	.LBB0_17
.LBB0_16:                               # %select.mid15
	movabsq	$.L.str2, %r8
.LBB0_17:                               # %select.end14
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	fprintf
	jmp	.LBB0_18
.LBB0_6:                                # %select.mid
	movabsq	$.L.str2, %r9
.LBB0_7:                                # %select.end
	movl	156(%rbx), %eax
	testq	%rax, %rax
	je	.LBB0_9
# BB#8:
	movabsq	$.L.str3, %rdi
	jmp	.LBB0_10
.LBB0_9:                                # %select.mid13
	movabsq	$.L.str2, %rdi
.LBB0_10:                               # %select.end12
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	movq	%rdi, (%rsp)
	movq	%r12, %rdi
	callq	fprintf
.LBB0_18:                               # %if.end23
	movq	8(%rbx), %rax
	movq	16(%rbx), %rcx
	movl	88(%rax), %edx
	movq	24(%rbx), %rax
	movl	88(%rcx), %ecx
	testq	%rax, %rax
	je	.LBB0_19
# BB#20:                                # %cond.true
	movl	88(%rax), %r8d
	jmp	.LBB0_21
.LBB0_19:
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
.LBB0_21:                               # %cond.end
	movq	48(%rbx), %rax
	movq	56(%rbx), %rsi
	movl	88(%rax), %r9d
	movl	88(%rsi), %edi
	movabsq	$.L.str5, %rsi
	xorq	%rax, %rax
	movq	%rdi, (%rsp)
	movq	%r12, %rdi
	callq	fprintf
	movl	120(%rbx), %edx
	movq	128(%rbx), %rax
	movl	124(%rbx), %ecx
	testq	%rax, %rax
	movq	$-1, %r8
	je	.LBB0_23
# BB#22:                                # %cond.true36
	movslq	(%rax), %r8
.LBB0_23:                               # %cond.end40
	movabsq	$.L.str6, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	fprintf
	movq	32(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB0_25
# BB#24:                                # %if.then44
	movl	40(%rbx), %edx
	movabsq	$.L.str7, %rdi
	movq	%r12, %rcx
	callq	flow_edge_list_print
.LBB0_25:                               # %if.end46
	movq	80(%rbx), %rsi
	movl	88(%rbx), %edx
	movabsq	$.L.str8, %rdi
	movq	%r12, %rcx
	callq	flow_edge_list_print
	movl	72(%rbx), %edx
	movabsq	$.L.str9, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	fprintf
	movq	64(%rbx), %rsi
	movabsq	$.L.str10, %rdi
	movq	%r12, %rdx
	callq	flow_nodes_print
	movq	96(%rbx), %rsi
	movl	104(%rbx), %edx
	movabsq	$.L.str11, %rdi
	movq	%r12, %rcx
	callq	flow_edge_list_print
	movq	112(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB0_27
# BB#26:                                # %if.then49
	movabsq	$.L.str12, %rdi
	movq	%r12, %rdx
	callq	flow_nodes_print
.LBB0_27:                               # %if.end51
	testq	%r15, %r15
	je	.LBB0_28
# BB#29:                                # %if.then53
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmpq	*%r15  # TAILCALL
.LBB0_28:                               # %if.end54
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp14:
	.size	flow_loop_dump, .Ltmp14-flow_loop_dump
	.cfi_endproc

	.globl	flow_loops_dump
	.align	16, 0x90
	.type	flow_loops_dump,@function
flow_loops_dump:                        # @flow_loops_dump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp21:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp22:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp23:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp24:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp25:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp26:
	.cfi_def_cfa_offset 128
.Ltmp27:
	.cfi_offset %rbx, -48
.Ltmp28:
	.cfi_offset %r12, -40
.Ltmp29:
	.cfi_offset %r13, -32
.Ltmp30:
	.cfi_offset %r14, -24
.Ltmp31:
	.cfi_offset %r15, -16
	movl	(%rdi), %eax
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, %r12
	movq	%r12, 56(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movq	%rax, %rdx
	je	.LBB1_36
# BB#1:                                 # %entry
	testq	%r12, %r12
	je	.LBB1_36
# BB#2:                                 # %if.end
	movl	4(%rdi), %ecx
	movabsq	$.L.str13, %rsi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%rdi, %r14
	movq	%r14, 32(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	movq	%rdx, %r15
	movq	%r15, 8(%rsp)           # 8-byte Spill
                                        # kill: RDX<def> R15<kill>
	callq	fprintf
	movq	%r14, %r8
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB1_17
# BB#3:                                 # %for.body.lr.ph
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB1_4:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_7 Depth 2
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	8(%r8), %r14
	movq	%r12, %rsi
	imulq	$248, %rdx, %r12
	leaq	(%r14,%r12), %rdi
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	movq	%r8, %r15
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	flow_loop_dump
	movq	%r15, %r8
	movl	152(%r14,%r12), %eax
	testq	%rax, %rax
	je	.LBB1_16
# BB#5:                                 # %for.cond4.preheader
                                        #   in Loop: Header=BB1_4 Depth=1
	movq	$32, %rcx
	movq	64(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB1_16
# BB#6:                                 # %for.body6.lr.ph
                                        #   in Loop: Header=BB1_4 Depth=1
	leaq	8(%r14,%r12), %r15
	leaq	72(%r14,%r12), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	$1, %r12
	xorq	%r14, %r14
	.align	16, 0x90
.LBB1_7:                                # %for.body6
                                        #   Parent Loop BB1_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%r8), %rax
	movq	(%r15), %rcx
	movq	8(%rax,%r14), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB1_15
# BB#8:                                 # %if.then12
                                        #   in Loop: Header=BB1_7 Depth=2
	movq	40(%rsp), %rcx          # 8-byte Reload
	movslq	(%rcx), %rcx
	movslq	72(%rax,%r14), %rdx
	addq	%r14, %rax
	cmpq	%rdx, %rcx
	leaq	-1(%r12), %rbx
	movq	72(%rsp), %rcx          # 8-byte Reload
	jl	.LBB1_10
# BB#9:                                 # %if.then12
                                        #   in Loop: Header=BB1_7 Depth=2
	movq	%rax, %rcx
.LBB1_10:                               # %if.then12
                                        #   in Loop: Header=BB1_7 Depth=2
	jl	.LBB1_12
# BB#11:                                # %if.then12
                                        #   in Loop: Header=BB1_7 Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
.LBB1_12:                               # %if.then12
                                        #   in Loop: Header=BB1_7 Depth=2
	movq	64(%rcx), %rsi
	movq	64(%rax), %rdi
	callq	sbitmap_a_subset_b_p
	movq	(%r15), %rcx
	testq	%r13, %rax
	movabsq	$.L.str15, %r9
	je	.LBB1_14
# BB#13:                                # %if.then12
                                        #   in Loop: Header=BB1_7 Depth=2
	movabsq	$.L.str16, %r9
.LBB1_14:                               # %if.then12
                                        #   in Loop: Header=BB1_7 Depth=2
	movl	88(%rcx), %edx
	xorq	%rax, %rax
	movq	56(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str14, %rsi
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, %r8
	callq	fprintf
	movq	32(%rsp), %r8           # 8-byte Reload
	movq	48(%rsp), %rbx          # 8-byte Reload
.LBB1_15:                               # %for.inc
                                        #   in Loop: Header=BB1_7 Depth=2
	movq	%r12, %rax
	andq	%r13, %rax
	movq	%rbx, %rcx
	andq	%r13, %rcx
	incq	%r12
	addq	$248, %r14
	cmpq	%rcx, %rax
	jne	.LBB1_7
.LBB1_16:                               # %for.inc29
                                        #   in Loop: Header=BB1_4 Depth=1
	movq	64(%rsp), %rdx          # 8-byte Reload
	incq	%rdx
	incq	%rbx
	movq	%rdx, %rax
	andq	%r13, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	%rcx, %rax
	movq	56(%rsp), %r12          # 8-byte Reload
	jne	.LBB1_4
.LBB1_17:                               # %for.end31
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	24(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB1_36
# BB#18:                                # %if.then33
	movl	(%r8), %eax
	testq	%rax, %rax
	je	.LBB1_36
# BB#19:                                # %lor.lhs.false2.i
	cmpq	$0, 24(%r8)
	je	.LBB1_36
# BB#20:                                # %for.cond.preheader.i
	movq	%r8, %r13
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB1_26
# BB#21:
	xorq	%r14, %r14
	movabsq	$.L.str19, %r15
	.align	16, 0x90
.LBB1_22:                               # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_24 Depth 2
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str18, %rsi
	movq	%r14, %rdx
	callq	fprintf
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r14,8), %rax
	movq	40(%rax), %rbx
	jmp	.LBB1_24
	.align	16, 0x90
.LBB1_23:                               # %for.body7.i
                                        #   in Loop: Header=BB1_24 Depth=2
	movq	24(%rbx), %rax
	movl	88(%rax), %edx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	fprintf
	movq	8(%rbx), %rbx
.LBB1_24:                               # %for.body7.i
                                        #   Parent Loop BB1_22 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	jne	.LBB1_23
# BB#25:                                # %for.end.i
                                        #   in Loop: Header=BB1_22 Depth=1
	movq	24(%r13), %rax
	movq	(%rax,%r14,8), %rsi
	movabsq	$.L.str20, %rdi
	movq	%r12, %rdx
	callq	flow_nodes_print
	incq	%r14
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB1_22
.LBB1_26:                               # %for.end14.i
	movq	%r13, %r15
	cmpq	$0, 32(%r15)
	je	.LBB1_31
# BB#27:                                # %if.then17.i
	movabsq	$.L.str21, %rdi
	movq	$14, %rsi
	movq	$1, %rdx
	movq	$1, %rbx
	movq	%r12, %rcx
	callq	fwrite
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB1_30
# BB#28:
	movabsq	$.L.str19, %r14
	.align	16, 0x90
.LBB1_29:                               # %for.body21.i
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r15), %rax
	movl	-4(%rax,%rbx,4), %edx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	fprintf
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB1_29
.LBB1_30:                               # %for.end29.i
	movq	$10, %rdi
	movq	%r12, %rsi
	callq	fputc
.LBB1_31:                               # %if.end31.i
	cmpq	$0, 40(%r15)
	je	.LBB1_36
# BB#32:                                # %if.then34.i
	movabsq	$.L.str23, %rdi
	movq	$13, %rsi
	movq	$1, %rdx
	movq	$1, %rbx
	movq	%r12, %rcx
	callq	fwrite
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB1_35
# BB#33:
	movabsq	$.L.str19, %r14
	.align	16, 0x90
.LBB1_34:                               # %for.body38.i
                                        # =>This Inner Loop Header: Depth=1
	movq	40(%r15), %rax
	movl	-4(%rax,%rbx,4), %edx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	fprintf
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB1_34
.LBB1_35:                               # %for.end46.i
	movq	$10, %rdi
	movq	%r12, %rsi
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.LBB1_36:                               # %if.end34
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp32:
	.size	flow_loops_dump, .Ltmp32-flow_loops_dump
	.cfi_endproc

	.globl	flow_loops_free
	.align	16, 0x90
	.type	flow_loops_free,@function
flow_loops_free:                        # @flow_loops_free
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp38:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp39:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp40:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp41:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp42:
	.cfi_def_cfa_offset 48
.Ltmp43:
	.cfi_offset %rbx, -40
.Ltmp44:
	.cfi_offset %r12, -32
.Ltmp45:
	.cfi_offset %r14, -24
.Ltmp46:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	8(%r14), %r12
	testq	%r12, %r12
	je	.LBB2_20
# BB#1:                                 # %if.then
	movl	(%r14), %eax
	testq	%rax, %rax
	je	.LBB2_21
# BB#2:                                 # %for.cond.preheader
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB2_15
# BB#3:
	movq	$1, %r15
	movq	$112, %rbx
	.align	16, 0x90
.LBB2_4:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-80(%r12,%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB2_6
# BB#5:                                 # %if.then6
                                        #   in Loop: Header=BB2_4 Depth=1
	callq	free
.LBB2_6:                                # %if.end8
                                        #   in Loop: Header=BB2_4 Depth=1
	movq	-48(%r12,%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB2_8
# BB#7:                                 # %if.then10
                                        #   in Loop: Header=BB2_4 Depth=1
	callq	free
.LBB2_8:                                # %if.end12
                                        #   in Loop: Header=BB2_4 Depth=1
	movq	-32(%r12,%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB2_10
# BB#9:                                 # %if.then14
                                        #   in Loop: Header=BB2_4 Depth=1
	callq	free
.LBB2_10:                               # %if.end16
                                        #   in Loop: Header=BB2_4 Depth=1
	movq	-16(%r12,%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB2_12
# BB#11:                                # %if.then18
                                        #   in Loop: Header=BB2_4 Depth=1
	callq	free
.LBB2_12:                               # %if.end20
                                        #   in Loop: Header=BB2_4 Depth=1
	movq	(%r12,%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB2_14
# BB#13:                                # %if.then22
                                        #   in Loop: Header=BB2_4 Depth=1
	callq	free
.LBB2_14:                               # %for.inc
                                        #   in Loop: Header=BB2_4 Depth=1
	movslq	(%r14), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	8(%r14), %r12
	incq	%r15
	addq	$248, %rbx
	cmpq	%rax, %rdx
	jl	.LBB2_4
.LBB2_15:                               # %for.end
	movq	%r12, %rdi
	callq	free
	movq	$0, 8(%r14)
	movq	24(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB2_17
# BB#16:                                # %if.then28
	callq	free
.LBB2_17:                               # %if.end31
	movq	32(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB2_19
# BB#18:                                # %if.then34
	callq	free
.LBB2_19:                               # %if.end37
	movq	48(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB2_20
# BB#22:                                # %if.then39
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.LBB2_20:                               # %if.end42
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB2_21:                               # %if.then2
	movabsq	$.L.str17, %rax
	movabsq	$.L__FUNCTION__.flow_loops_free, %rcx
	movq	$209, %rsi
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp47:
	.size	flow_loops_free, .Ltmp47-flow_loops_free
	.cfi_endproc

	.globl	flow_loop_scan
	.align	16, 0x90
	.type	flow_loop_scan,@function
flow_loop_scan:                         # @flow_loop_scan
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp54:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp55:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp56:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp57:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp58:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp59:
	.cfi_def_cfa_offset 80
.Ltmp60:
	.cfi_offset %rbx, -48
.Ltmp61:
	.cfi_offset %r12, -40
.Ltmp62:
	.cfi_offset %r13, -32
.Ltmp63:
	.cfi_offset %r14, -24
.Ltmp64:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	%rdi, %r14
	testq	$16, %rdx
	je	.LBB3_3
# BB#1:                                 # %land.lhs.true
	cmpq	$0, 96(%r12)
	jne	.LBB3_3
# BB#2:
	orq	$8, %rdx
.LBB3_3:                                # %if.end
	testq	$4, %rdx
	je	.LBB3_20
# BB#4:                                 # %if.then4
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	8(%r12), %r13
	movq	64(%r12), %r15
	movq	$0, 80(%r12)
	movq	32(%r13), %rax
	testq	%rax, %rax
	je	.LBB3_11
# BB#5:
	xorq	%rdi, %rdi
	movabsq	$entry_exit_blocks, %rbx
	.align	16, 0x90
.LBB3_6:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rax), %rcx
	cmpq	%rbx, %rcx
	je	.LBB3_8
# BB#7:                                 # %lor.lhs.false.i
                                        #   in Loop: Header=BB3_6 Depth=1
	movl	88(%rcx), %ecx
	movq	%rcx, %rdx
	shrq	$6, %rdx
	movq	16(%r15,%rdx,8), %rdx
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%rdx, %rsi
	jne	.LBB3_9
.LBB3_8:                                # %if.then.i
                                        #   in Loop: Header=BB3_6 Depth=1
	incq	%rdi
.LBB3_9:                                # %for.inc.i
                                        #   in Loop: Header=BB3_6 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB3_6
# BB#10:                                # %for.end.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdi
	je	.LBB3_11
# BB#13:                                # %if.end6.i
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, 80(%r12)
	movq	32(%r13), %rdx
	xorq	%rax, %rax
	jmp	.LBB3_14
	.align	16, 0x90
.LBB3_18:                               # %for.inc32.i
                                        #   in Loop: Header=BB3_14 Depth=1
	movq	(%rdx), %rdx
.LBB3_14:                               # %if.end6.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rdx, %rdx
	je	.LBB3_19
# BB#15:                                # %for.body10.i
                                        #   in Loop: Header=BB3_14 Depth=1
	movq	16(%rdx), %rcx
	cmpq	%rbx, %rcx
	je	.LBB3_17
# BB#16:                                # %lor.lhs.false15.i
                                        #   in Loop: Header=BB3_14 Depth=1
	movl	88(%rcx), %ecx
	movq	%rcx, %rsi
	shrq	$6, %rsi
	movq	16(%r15,%rsi,8), %rsi
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%rsi, %rdi
	jne	.LBB3_18
.LBB3_17:                               # %if.then27.i
                                        #   in Loop: Header=BB3_14 Depth=1
	movq	$32, %rcx
	leaq	1(%rax), %rsi
	movq	80(%r12), %rdi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, (%rdi,%rax,8)
	movq	%rsi, %rax
	jmp	.LBB3_18
.LBB3_19:                               # %flow_loop_entry_edges_find.exit
	movl	%eax, 88(%r12)
	movq	16(%rsp), %rdx          # 8-byte Reload
.LBB3_20:                               # %if.end5
	testq	$8, %rdx
	je	.LBB3_50
# BB#21:                                # %if.then8
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movq	64(%r12), %r13
	movq	$0, 96(%r12)
	movl	4(%r13), %eax
	xorq	%rdx, %rdx
	testq	%rax, %rax
	je	.LBB3_49
# BB#22:                                # %for.body.lr.ph.i
	xorq	%r10, %r10
	movq	basic_block_info(%rip), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movabsq	$entry_exit_blocks+112, %r15
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB3_23:                               # %for.body.i75
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_25 Depth 2
                                        #       Child Loop BB3_27 Depth 3
	movq	16(%r13,%r10,8), %rsi
	testq	%rsi, %rsi
	je	.LBB3_34
# BB#24:                                # %for.body4.preheader.i
                                        #   in Loop: Header=BB3_23 Depth=1
	movq	$6, %rcx
	movq	%r10, %r11
	shlq	%cl, %r11
	xorq	%rax, %rax
	.align	16, 0x90
.LBB3_25:                               # %for.body4.i
                                        #   Parent Loop BB3_23 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_27 Depth 3
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	%rsi, %rdx
	je	.LBB3_33
# BB#26:                                # %if.then6.i
                                        #   in Loop: Header=BB3_25 Depth=2
	movq	%rbx, %r9
	leaq	(%rax,%r11), %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	32(%rcx,%rbx,8), %rcx
	movq	40(%rcx), %r14
	jmp	.LBB3_27
	.align	16, 0x90
.LBB3_31:                               # %for.inc.i85
                                        #   in Loop: Header=BB3_27 Depth=3
	movq	8(%r14), %r14
.LBB3_27:                               # %if.then6.i
                                        #   Parent Loop BB3_23 Depth=1
                                        #     Parent Loop BB3_25 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r14, %r14
	je	.LBB3_32
# BB#28:                                # %for.body11.i
                                        #   in Loop: Header=BB3_27 Depth=3
	movq	24(%r14), %rcx
	cmpq	%r15, %rcx
	je	.LBB3_30
# BB#29:                                # %lor.lhs.false.i81
                                        #   in Loop: Header=BB3_27 Depth=3
	movl	88(%rcx), %ecx
	movq	%rcx, %rbx
	shrq	$6, %rbx
	movq	16(%r13,%rbx,8), %rbx
	movq	$1, %r8
	shlq	%cl, %r8
	testq	%rbx, %r8
	jne	.LBB3_31
.LBB3_30:                               # %if.then21.i
                                        #   in Loop: Header=BB3_27 Depth=3
	incq	%rdi
	jmp	.LBB3_31
	.align	16, 0x90
.LBB3_32:                               # %for.end.i86
                                        #   in Loop: Header=BB3_25 Depth=2
	notq	%rdx
	andq	%rdx, %rsi
	movq	%r9, %rbx
	je	.LBB3_34
.LBB3_33:                               # %for.inc26.i
                                        #   in Loop: Header=BB3_25 Depth=2
	incq	%rax
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	$64, %rcx
	jb	.LBB3_25
.LBB3_34:                               # %for.inc30.i
                                        #   in Loop: Header=BB3_23 Depth=1
	incq	%r10
	movq	%r10, %rax
	andq	%rbx, %rax
	movl	4(%r13), %ecx
	cmpq	%rcx, %rax
	jne	.LBB3_23
# BB#35:                                # %do.end.i
	xorq	%rdx, %rdx
	testq	%rbx, %rdi
	je	.LBB3_49
# BB#36:                                # %if.end35.i
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, 96(%r12)
	movl	4(%r13), %eax
	xorq	%r9, %r9
	xorq	%rdx, %rdx
	testq	%rax, %rax
	je	.LBB3_49
	.align	16, 0x90
.LBB3_37:                               # %for.body48.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_39 Depth 2
                                        #       Child Loop BB3_41 Depth 3
	movq	16(%r13,%r9,8), %r11
	testq	%r11, %r11
	je	.LBB3_48
# BB#38:                                # %for.body58.preheader.i
                                        #   in Loop: Header=BB3_37 Depth=1
	movq	$6, %rcx
	movq	%r9, %r10
	shlq	%cl, %r10
	xorq	%rax, %rax
	.align	16, 0x90
.LBB3_39:                               # %for.body58.i
                                        #   Parent Loop BB3_37 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_41 Depth 3
	movq	$1, %r14
	movq	%rax, %rcx
	shlq	%cl, %r14
	testq	%r14, %r11
	je	.LBB3_47
# BB#40:                                # %if.then65.i
                                        #   in Loop: Header=BB3_39 Depth=2
	movq	%rbx, %r8
	leaq	(%rax,%r10), %rsi
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rbx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	32(%rbx,%rsi,8), %rcx
	movq	40(%rcx), %rsi
	jmp	.LBB3_41
	.align	16, 0x90
.LBB3_45:                               # %for.inc99.i
                                        #   in Loop: Header=BB3_41 Depth=3
	movq	8(%rsi), %rsi
.LBB3_41:                               # %if.then65.i
                                        #   Parent Loop BB3_37 Depth=1
                                        #     Parent Loop BB3_39 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rsi, %rsi
	je	.LBB3_46
# BB#42:                                # %for.body77.i
                                        #   in Loop: Header=BB3_41 Depth=3
	movq	24(%rsi), %rcx
	cmpq	%r15, %rcx
	je	.LBB3_44
# BB#43:                                # %lor.lhs.false82.i
                                        #   in Loop: Header=BB3_41 Depth=3
	movl	88(%rcx), %ecx
	movq	%rcx, %rbx
	shrq	$6, %rbx
	movq	16(%r13,%rbx,8), %rbx
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%rbx, %rdi
	jne	.LBB3_45
.LBB3_44:                               # %if.then94.i
                                        #   in Loop: Header=BB3_41 Depth=3
	movq	$32, %rcx
	leaq	1(%rdx), %rdi
	movq	96(%r12), %rbx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rsi, (%rbx,%rdx,8)
	movq	%rdi, %rdx
	jmp	.LBB3_45
	.align	16, 0x90
.LBB3_46:                               # %for.end101.i
                                        #   in Loop: Header=BB3_39 Depth=2
	notq	%r14
	andq	%r14, %r11
	movq	%r8, %rbx
	je	.LBB3_48
.LBB3_47:                               # %for.inc107.i
                                        #   in Loop: Header=BB3_39 Depth=2
	incq	%rax
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	$64, %rcx
	jb	.LBB3_39
.LBB3_48:                               # %for.inc111.i
                                        #   in Loop: Header=BB3_37 Depth=1
	incq	%r9
	movq	%r9, %rax
	andq	%rbx, %rax
	movl	4(%r13), %ecx
	cmpq	%rcx, %rax
	jne	.LBB3_37
.LBB3_49:                               # %flow_loop_exit_edges_find.exit
	movl	%edx, 104(%r12)
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
.LBB3_50:                               # %if.end12
	testq	$16, %rdx
	je	.LBB3_56
# BB#51:                                # %if.then15
	movq	%rdx, %r15
	movl	n_basic_blocks(%rip), %edi
	callq	sbitmap_alloc
	movq	%rax, 112(%r12)
	movq	64(%r12), %rsi
	movq	%rax, %rdi
	callq	sbitmap_copy
	movslq	104(%r12), %rax
	testq	%rax, %rax
	jle	.LBB3_54
# BB#52:                                # %for.body.lr.ph
	movq	$1, %rbx
	.align	16, 0x90
.LBB3_53:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	96(%r12), %rax
	movq	-8(%rax,%rbx,8), %rax
	movq	16(%rax), %rax
	movslq	88(%rax), %rax
	movq	24(%r14), %rcx
	movq	112(%r12), %rdi
	movq	(%rcx,%rax,8), %rdx
	movq	%rdi, %rsi
	callq	sbitmap_a_and_b
	movslq	104(%r12), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB3_53
.LBB3_54:                               # %for.end
	movq	8(%r12), %rax
	movl	88(%rax), %ecx
	movq	112(%r12), %rax
	movq	%rcx, %rdx
	shrq	$6, %rdx
	movq	16(%rax,%rdx,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rax, %rdx
	movq	%r15, %rdx
	je	.LBB3_55
.LBB3_56:                               # %if.end37
	testq	$2, %rdx
	je	.LBB3_75
# BB#57:                                # %if.then40
	movq	8(%r12), %r11
	movq	32(%r11), %rsi
	xorq	%r8, %r8
	testq	%rsi, %rsi
	movabsq	$0, %rcx
	je	.LBB3_65
# BB#58:                                # %for.body.lr.ph.i92
	movq	24(%r14), %r10
	xorq	%rbx, %rbx
	movabsq	$entry_exit_blocks, %r9
	.align	16, 0x90
.LBB3_59:                               # %for.body.i94
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rsi), %rax
	cmpq	%r9, %rax
	jne	.LBB3_61
# BB#60:                                #   in Loop: Header=BB3_59 Depth=1
	movq	%rbx, %rax
	jmp	.LBB3_64
	.align	16, 0x90
.LBB3_61:                               # %land.lhs.true.i
                                        #   in Loop: Header=BB3_59 Depth=1
	movslq	88(%rax), %rdi
	movl	88(%r11), %ecx
	movq	(%r10,%rdi,8), %rdi
	movq	%rcx, %rdx
	shrq	$6, %rdx
	movq	16(%rdi,%rdx,8), %rdx
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%rdi, %rdx
	jne	.LBB3_62
# BB#63:                                # %if.then.i100
                                        #   in Loop: Header=BB3_59 Depth=1
	xorq	%rcx, %rcx
	testq	%rbx, %rbx
	je	.LBB3_64
	jmp	.LBB3_65
	.align	16, 0x90
.LBB3_62:                               #   in Loop: Header=BB3_59 Depth=1
	movq	%rbx, %rax
.LBB3_64:                               # %for.inc.i104
                                        #   in Loop: Header=BB3_59 Depth=1
	movq	(%rsi), %rsi
	testq	%rsi, %rsi
	movq	%rax, %rbx
	movq	%rax, %rcx
	jne	.LBB3_59
.LBB3_65:                               # %flow_loop_pre_header_find.exit
	movq	%rcx, 24(%r12)
	movl	%r8d, 40(%r12)
	movl	88(%r12), %eax
	cmpq	$1, %rax
	jne	.LBB3_75
# BB#66:                                # %if.end.i
	movq	80(%r12), %rax
	movq	(%rax), %rax
	movq	16(%rax), %rcx
	movabsq	$entry_exit_blocks, %rax
	cmpq	%rax, %rcx
	je	.LBB3_75
# BB#67:                                # %for.cond.preheader.i
	movq	32(%rcx), %rdx
	movq	16(%rdx), %rcx
	cmpq	%rax, %rcx
	movq	$1, %rbx
	je	.LBB3_70
	.align	16, 0x90
.LBB3_69:                               # %land.rhs.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, (%rdx)
	jne	.LBB3_70
# BB#68:                                # %for.cond.i
                                        #   in Loop: Header=BB3_69 Depth=1
	movq	32(%rcx), %rdx
	movq	16(%rdx), %rcx
	incq	%rbx
	cmpq	%rax, %rcx
	jne	.LBB3_69
.LBB3_70:                               # %for.end.i116
	movq	$32, %rcx
	movq	%rbx, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	$3, %rcx
	movq	%r14, %rdi
	shlq	%cl, %rdi
	callq	xmalloc
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rbx
	movq	%rax, 32(%r12)
	movl	%ebx, 40(%r12)
	je	.LBB3_75
# BB#71:                                # %for.body14.lr.ph.i
	movq	80(%r12), %rcx
	movq	(%rcx), %rsi
	movq	%rsi, -8(%rax,%r14,8)
	decq	%r14
	testq	%rdx, %r14
	je	.LBB3_75
# BB#72:                                # %for.body14.for.body14_crit_edge.i.preheader
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	$-2, %rbx
	jmp	.LBB3_73
	.align	16, 0x90
.LBB3_74:                               # %for.body14.for.body14_crit_edge.i.for.body14.for.body14_crit_edge.i_crit_edge
                                        #   in Loop: Header=BB3_73 Depth=1
	movq	32(%r12), %rax
	decq	%rbx
.LBB3_73:                               # %for.body14.for.body14_crit_edge.i
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rsi), %rcx
	movq	32(%rcx), %rsi
	movq	%rsi, (%rax,%rbx,8)
	testq	%rdx, %rbx
	jne	.LBB3_74
.LBB3_75:                               # %if.end45
	movq	$1, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB3_11:                               # %if.then5.i
	movabsq	$.L.str17, %rax
	movabsq	$.L__FUNCTION__.flow_loop_entry_edges_find, %rcx
	movq	$267, %rsi              # imm = 0x10B
	jmp	.LBB3_12
.LBB3_55:                               # %if.then35
	movabsq	$.L.str17, %rax
	movabsq	$.L__FUNCTION__.flow_loop_scan, %rcx
	movq	$611, %rsi              # imm = 0x263
.LBB3_12:                               # %if.then5.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp65:
	.size	flow_loop_scan, .Ltmp65-flow_loop_scan
	.cfi_endproc

	.globl	flow_loops_find
	.align	16, 0x90
	.type	flow_loops_find,@function
flow_loops_find:                        # @flow_loops_find
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp72:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp73:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp74:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp75:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp76:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp77:
	.cfi_def_cfa_offset 128
.Ltmp78:
	.cfi_offset %rbx, -48
.Ltmp79:
	.cfi_offset %r12, -40
.Ltmp80:
	.cfi_offset %r13, -32
.Ltmp81:
	.cfi_offset %r14, -24
.Ltmp82:
	.cfi_offset %r15, -16
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rdi, %rcx
	testq	$1, %rsi
	je	.LBB4_1
# BB#3:                                 # %if.end
	movq	$0, 48(%rcx)
	movq	$0, 40(%rcx)
	movq	$0, 32(%rcx)
	movq	$0, 24(%rcx)
	movq	$0, 16(%rcx)
	movq	$0, 8(%rcx)
	movq	$0, (%rcx)
	movl	n_basic_blocks(%rip), %edi
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB4_69
# BB#4:                                 # %if.end2
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	%rdi, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, %r15
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	xorq	%r14, %r14
	movq	%r15, %rsi
	callq	calculate_dominance_info
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB4_62
# BB#5:                                 # %for.body.lr.ph
	xorq	%r8, %r8
	movq	basic_block_info(%rip), %r9
	movabsq	$entry_exit_blocks, %r12
	movabsq	$0, %r11
	movabsq	$1, %r10
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB4_6:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_9 Depth 2
	movq	32(%r9,%rsi,8), %rax
	movl	%r8d, 92(%rax)
	movq	32(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB4_14
# BB#7:                                 # %for.body6.lr.ph
                                        #   in Loop: Header=BB4_6 Depth=1
	movl	88(%rax), %r14d
	movq	%rsi, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r13
	movq	$6, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	$1, %rdx
	movq	%rsi, %rcx
	shlq	%cl, %rdx
	cmpq	%r14, %r13
	jne	.LBB4_70
# BB#8:                                 #   in Loop: Header=BB4_6 Depth=1
	andq	$67108863, %rax         # imm = 0x3FFFFFF
	.align	16, 0x90
.LBB4_9:                                # %if.end9
                                        #   Parent Loop BB4_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	16(%rdi), %rcx
	cmpq	%r12, %rcx
	je	.LBB4_13
# BB#10:                                # %land.lhs.true
                                        #   in Loop: Header=BB4_9 Depth=2
	movslq	88(%rcx), %rcx
	movq	(%r15,%rcx,8), %rcx
	movq	16(%rcx,%rax,8), %rcx
	testq	%rdx, %rcx
	movq	%r11, %rcx
	je	.LBB4_12
# BB#11:                                # %land.lhs.true
                                        #   in Loop: Header=BB4_9 Depth=2
	movq	%r10, %rcx
.LBB4_12:                               # %land.lhs.true
                                        #   in Loop: Header=BB4_9 Depth=2
	addq	%rcx, %rbx
.LBB4_13:                               # %for.inc
                                        #   in Loop: Header=BB4_9 Depth=2
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.LBB4_9
.LBB4_14:                               # %for.inc20
                                        #   in Loop: Header=BB4_6 Depth=1
	incq	%rsi
	movslq	n_basic_blocks(%rip), %rdi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdi, %rax
	jl	.LBB4_6
# BB#15:                                # %for.end22
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	movabsq	$0, %r14
	je	.LBB4_62
# BB#16:                                # %if.then24
	movq	$2, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rdi
	movq	%rax, %r13
	movq	%r13, 64(%rsp)          # 8-byte Spill
	movq	$2, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, %r14
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	flow_depth_first_order_compute
	movq	72(%rsp), %r13          # 8-byte Reload
	movq	%r15, 24(%r13)
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, 32(%r13)
	movq	%r14, 40(%r13)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$248, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	%rax, 8(%r13)
	movl	n_basic_blocks(%rip), %edi
	callq	sbitmap_alloc
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rax, %rdi
	callq	sbitmap_zero
	movl	n_basic_blocks(%rip), %edi
	callq	sbitmap_alloc
	movq	%rax, 48(%r13)
	movq	%rax, %rdi
	callq	sbitmap_zero
	movslq	n_basic_blocks(%rip), %r8
	movq	$0, (%rsp)              # 8-byte Folded Spill
	testq	%r8, %r8
	movabsq	$0, %rbx
	jle	.LBB4_24
# BB#17:                                # %for.body43.lr.ph
	xorq	%r11, %r11
	movabsq	$entry_exit_blocks+112, %r9
	movq	%r13, %r10
	.align	16, 0x90
.LBB4_18:                               # %for.body43
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_19 Depth 2
	movslq	-4(%r14,%r8,4), %rcx
	movq	basic_block_info(%rip), %rsi
	movq	32(%rsi,%rcx,8), %rsi
	movq	40(%rsi), %rdi
	jmp	.LBB4_19
	.align	16, 0x90
.LBB4_30:                               # %for.inc78
                                        #   in Loop: Header=BB4_19 Depth=2
	movq	8(%rdi), %rdi
.LBB4_19:                               # %for.body43
                                        #   Parent Loop BB4_18 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rdi, %rdi
	je	.LBB4_20
# BB#27:                                # %for.body53
                                        #   in Loop: Header=BB4_19 Depth=2
	movq	24(%rdi), %rbx
	cmpq	%r9, %rbx
	je	.LBB4_30
# BB#28:                                # %land.lhs.true57
                                        #   in Loop: Header=BB4_19 Depth=2
	movslq	88(%rsi), %rax
	movl	88(%rbx), %ecx
	movq	(%r15,%rax,8), %rax
	movq	%rcx, %rdx
	shrq	$6, %rdx
	movq	16(%rax,%rdx,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB4_30
# BB#29:                                # %if.then72
                                        #   in Loop: Header=BB4_19 Depth=2
	movq	8(%r10), %rax
	movq	$32, %rcx
	movq	%r11, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	imulq	$248, %rdx, %rcx
	movq	%rbx, 8(%rax,%rcx)
	movq	%rsi, 16(%rax,%rcx)
	movl	%r11d, (%rax,%rcx)
	incq	%r11
	jmp	.LBB4_30
	.align	16, 0x90
.LBB4_20:                               # %for.cond40.loopexit
                                        #   in Loop: Header=BB4_18 Depth=1
	decq	%r8
	movq	$32, %rcx
	movq	%r8, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jg	.LBB4_18
# BB#21:                                # %for.cond82.preheader
	movq	$32, %rcx
	movq	%r11, %rax
	movq	%r11, 24(%rsp)          # 8-byte Spill
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB4_22
# BB#37:
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB4_38:                               # %for.body85
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_43 Depth 2
                                        #       Child Loop BB4_48 Depth 3
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	8(%r10), %r15
	imulq	$248, %rdx, %r14
	movq	8(%r15,%r14), %rax
	movl	88(%rax), %eax
	movq	$6, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	16(%rbx,%rdx,8), %rdi
	leaq	8(%r15,%r14), %rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rdi, %rsi
	je	.LBB4_40
# BB#39:                                # %if.then103
                                        #   in Loop: Header=BB4_38 Depth=1
	movq	48(%r10), %rax
	orq	%rsi, 16(%rax,%rdx,8)
	movq	16(%rbx,%rdx,8), %rdi
.LBB4_40:                               # %if.end115
                                        #   in Loop: Header=BB4_38 Depth=1
	orq	%rdi, %rsi
	movq	%rsi, 16(%rbx,%rdx,8)
	movl	n_basic_blocks(%rip), %edi
	callq	sbitmap_alloc
	movq	%rax, %r13
	movq	%r14, 48(%rsp)          # 8-byte Spill
	movq	%r13, 64(%r15,%r14)
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rbx
	movslq	n_basic_blocks(%rip), %rdi
	movq	16(%r15,%r14), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%r15, 56(%rsp)          # 8-byte Spill
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, %r14
	movq	%r13, %rdi
	callq	sbitmap_zero
	movq	40(%rsp), %r8           # 8-byte Reload
	movl	88(%rbx), %ecx
	movq	$1, %rax
	shlq	%cl, %rax
	shrq	$6, %rcx
	orq	%rax, 16(%r13,%rcx,8)
	movl	92(%rbx), %eax
	incq	%rax
	movl	%eax, 92(%rbx)
	movl	88(%r8), %eax
	movq	$6, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movq	16(%r13,%rdx,8), %rsi
	movq	$1, %r15
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	testq	%rsi, %rdi
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	jne	.LBB4_31
# BB#41:                                # %while.body.lr.ph.i
                                        #   in Loop: Header=BB4_38 Depth=1
	orq	%rsi, %rdi
	movq	%rdi, 16(%r13,%rdx,8)
	movl	92(%r8), %eax
	incq	%rax
	movl	%eax, 92(%r8)
	movq	%r8, (%r14)
	movq	$2, %r15
	movq	$1, %r8
	.align	16, 0x90
.LBB4_43:                               # %while.body.i
                                        #   Parent Loop BB4_38 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB4_48 Depth 3
	decq	%r8
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%r14,%rax,8), %rax
	movq	32(%rax), %rsi
	jmp	.LBB4_48
	.align	16, 0x90
.LBB4_47:                               # %for.inc.i
                                        #   in Loop: Header=BB4_48 Depth=3
	movq	(%rsi), %rsi
.LBB4_48:                               # %for.inc.i
                                        #   Parent Loop BB4_38 Depth=1
                                        #     Parent Loop BB4_43 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rsi, %rsi
	je	.LBB4_42
# BB#44:                                # %for.body.i
                                        #   in Loop: Header=BB4_48 Depth=3
	movq	16(%rsi), %rdi
	cmpq	%r12, %rdi
	je	.LBB4_47
# BB#45:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB4_48 Depth=3
	movl	88(%rdi), %r9d
	movq	$6, %rcx
	movq	%r9, %rax
	shrq	%cl, %rax
	movq	16(%r13,%rax,8), %rdx
	movq	$1, %rbx
	movq	%r9, %rcx
	shlq	%cl, %rbx
	testq	%rdx, %rbx
	jne	.LBB4_47
# BB#46:                                # %if.then43.i
                                        #   in Loop: Header=BB4_48 Depth=3
	orq	%rdx, %rbx
	movq	%rbx, 16(%r13,%rax,8)
	movl	92(%rdi), %eax
	incq	%rax
	movl	%eax, 92(%rdi)
	incq	%r15
	movq	$32, %rcx
	leaq	1(%r8), %rax
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movq	%rdi, (%r14,%r8,8)
	movq	%rax, %r8
	jmp	.LBB4_47
	.align	16, 0x90
.LBB4_42:                               # %while.cond.loopexit.i
                                        #   in Loop: Header=BB4_43 Depth=2
	testq	%r10, %r8
	jne	.LBB4_43
.LBB4_31:                               # %flow_loop_nodes_find.exit
                                        #   in Loop: Header=BB4_38 Depth=1
	movq	56(%rsp), %r13          # 8-byte Reload
	movq	48(%rsp), %rbx          # 8-byte Reload
	leaq	(%r13,%rbx), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	leaq	64(%r13,%rbx), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	movq	%r10, %r14
	callq	free
	movl	%r15d, 72(%r13,%rbx)
	movq	32(%rsp), %r15          # 8-byte Reload
	movq	(%r15), %rdi
	callq	sbitmap_first_set_bit
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %rax
	movq	%rax, 48(%r13,%rbx)
	movq	(%r15), %rdi
	callq	sbitmap_last_set_bit
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %rax
	movq	%rax, 56(%r13,%rbx)
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	callq	flow_loop_scan
	movq	%r14, %r11
	movq	%rbx, %r10
	movq	64(%rsp), %rdx          # 8-byte Reload
	incq	%rdx
	movq	%rdx, %rcx
	andq	%r11, %rcx
	movq	24(%rsp), %rax          # 8-byte Reload
	andq	%r11, %rax
	cmpq	%rax, %rcx
	jne	.LBB4_38
# BB#32:                                # %for.cond149.preheader
	movq	$32, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB4_22
# BB#33:                                # %for.body152.lr.ph
	movq	8(%r10), %rdx
	movq	48(%r10), %r9
	movq	%r10, %r13
	addq	$152, %rdx
	movq	$1, %r8
	movq	$1, %rbx
	.align	16, 0x90
.LBB4_34:                               # %for.body152
                                        # =>This Inner Loop Header: Depth=1
	movq	-144(%rdx), %rcx
	movl	88(%rcx), %ecx
	movq	%rcx, %rdi
	shrq	$6, %rdi
	movq	16(%r9,%rdi,8), %rdi
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%rdi, %rsi
	je	.LBB4_36
# BB#35:                                # %if.then173
                                        #   in Loop: Header=BB4_34 Depth=1
	movl	%r8d, (%rdx)
.LBB4_36:                               # %for.inc178
                                        #   in Loop: Header=BB4_34 Depth=1
	movq	%rbx, %rcx
	andq	%r11, %rcx
	incq	%rbx
	addq	$248, %rdx
	cmpq	%rax, %rcx
	jne	.LBB4_34
	jmp	.LBB4_23
.LBB4_62:                               # %if.end181.thread
	movq	%r15, %rdi
	callq	free
	movq	72(%rsp), %rdx          # 8-byte Reload
	movl	%r14d, (%rdx)
	movq	$0, (%rsp)              # 8-byte Folded Spill
	jmp	.LBB4_63
.LBB4_22:
	movq	%r10, %r13
.LBB4_23:                               # %if.end181
	movq	24(%rsp), %rbx          # 8-byte Reload
.LBB4_24:                               # %if.end181
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	free
	movq	%r13, %rdx
	movl	%ebx, (%rdx)
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rbx
	movq	%rbx, %rdi
	je	.LBB4_63
# BB#25:                                # %if.end.i
	movq	8(%rdx), %r15
	movq	%r15, 16(%rdx)
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	movq	$0, 144(%r15)
	movq	$0, 136(%r15)
	movq	$0, 128(%r15)
	jl	.LBB4_26
# BB#49:
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	%rsi, %r14
	movq	$1, %rdi
	jmp	.LBB4_50
	.align	16, 0x90
.LBB4_61:                               # %flow_loop_tree_node_add.exit.for.body_crit_edge.i
                                        #   in Loop: Header=BB4_50 Depth=1
	movq	8(%rdx), %r15
.LBB4_50:                               # %for.body.i272
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_55 Depth 2
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	leaq	-1(%rdi), %rax
	imulq	$248, %rax, %r13
	imulq	$248, %rdi, %r12
	movq	64(%r15,%r13), %rsi
	movq	64(%r15,%r12), %rdi
	leaq	(%r15,%r13), %rbx
	leaq	(%r15,%r12), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	callq	sbitmap_a_subset_b_p
	testq	%r14, %rax
	jne	.LBB4_53
# BB#51:                                # %for.cond.preheader.i.i
                                        #   in Loop: Header=BB4_50 Depth=1
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%r12, 48(%rsp)          # 8-byte Spill
	movq	128(%r15,%r13), %rax
	testq	%rax, %rax
	jne	.LBB4_54
# BB#52:                                #   in Loop: Header=BB4_50 Depth=1
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movq	%rsi, %r12
	jmp	.LBB4_58
	.align	16, 0x90
.LBB4_53:                               # %if.then.i.i
                                        #   in Loop: Header=BB4_50 Depth=1
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, 136(%r15,%r13)
	movq	%rbx, 128(%r15,%r12)
	jmp	.LBB4_59
	.align	16, 0x90
.LBB4_54:                               #   in Loop: Header=BB4_50 Depth=1
	movq	%r15, %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	48(%rsp), %rdx          # 8-byte Reload
	leaq	64(%rcx,%rdx), %r15
	leaq	128(%rcx,%r13), %r13
	movq	%rsi, %r12
	.align	16, 0x90
.LBB4_55:                               # %for.body.i.i
                                        #   Parent Loop BB4_50 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	64(%rax), %rsi
	movq	(%r15), %rdi
	callq	sbitmap_a_subset_b_p
	testq	%r14, %rax
	jne	.LBB4_56
# BB#57:                                # %for.inc.i.i
                                        #   in Loop: Header=BB4_55 Depth=2
	movq	(%r13), %rbx
	movq	128(%rbx), %rax
	leaq	128(%rbx), %r13
	testq	%rax, %rax
	jne	.LBB4_55
.LBB4_58:                               # %for.end.i.i
                                        #   in Loop: Header=BB4_50 Depth=1
	movq	%r12, 144(%rbx)
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	$0, 128(%rax,%rcx)
	jmp	.LBB4_59
.LBB4_56:                               # %if.then6.i.i
                                        #   in Loop: Header=BB4_50 Depth=1
	movq	%r12, 144(%rbx)
	movq	(%r13), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%rax, 128(%rcx,%rdx)
	.align	16, 0x90
.LBB4_59:                               # %flow_loop_tree_node_add.exit.i
                                        #   in Loop: Header=BB4_50 Depth=1
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	56(%rsp), %rdi          # 8-byte Reload
	incq	%rdi
	movq	%rdi, %rax
	andq	%r14, %rax
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rcx
	andq	%r14, %rcx
	cmpq	%rcx, %rax
	jne	.LBB4_61
# BB#60:
	movq	%rsi, (%rsp)            # 8-byte Spill
	jmp	.LBB4_63
.LBB4_26:
	movq	%rdi, (%rsp)            # 8-byte Spill
.LBB4_63:                               # %flow_loops_tree_build.exit
	movq	16(%rdx), %rbx
	movq	%rdx, %r15
	xorq	%r14, %r14
	jmp	.LBB4_64
	.align	16, 0x90
.LBB4_67:                               # %for.body.i277
                                        #   in Loop: Header=BB4_64 Depth=1
	movq	144(%rbx), %rbx
.LBB4_64:                               # %flow_loops_tree_build.exit
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB4_68
# BB#65:                                # %for.body.i277
                                        #   in Loop: Header=BB4_64 Depth=1
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	flow_loop_level_compute
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jge	.LBB4_67
# BB#66:                                # %for.body.i277
                                        #   in Loop: Header=BB4_64 Depth=1
	movq	%rax, %r14
	jmp	.LBB4_67
.LBB4_68:                               # %flow_loops_level_compute.exit
	movl	%r14d, 4(%r15)
	movq	(%rsp), %rax            # 8-byte Reload
.LBB4_69:                               # %return
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB4_1:                                # %if.then
	movabsq	$.L.str17, %rax
	movabsq	$.L__FUNCTION__.flow_loops_find, %rcx
	movq	$651, %rsi              # imm = 0x28B
	jmp	.LBB4_2
.LBB4_70:                               # %if.then8.split
	movabsq	$.L.str17, %rax
	movabsq	$.L__FUNCTION__.flow_loops_find, %rcx
	movq	$688, %rsi              # imm = 0x2B0
.LBB4_2:                                # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp83:
	.size	flow_loops_find, .Ltmp83-flow_loops_find
	.cfi_endproc

	.globl	flow_loops_update
	.align	16, 0x90
	.type	flow_loops_update,@function
flow_loops_update:                      # @flow_loops_update
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp87:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp88:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp89:
	.cfi_def_cfa_offset 32
.Ltmp90:
	.cfi_offset %rbx, -24
.Ltmp91:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	cmpq	$0, 8(%rbx)
	movq	%rsi, %r14
	je	.LBB5_2
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	callq	flow_loops_free
.LBB5_2:                                # %if.end
	movq	%rbx, %rdi
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	flow_loops_find  # TAILCALL
.Ltmp92:
	.size	flow_loops_update, .Ltmp92-flow_loops_update
	.cfi_endproc

	.globl	flow_loop_outside_edge_p
	.align	16, 0x90
	.type	flow_loop_outside_edge_p,@function
flow_loop_outside_edge_p:               # @flow_loop_outside_edge_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp94:
	.cfi_def_cfa_offset 16
	movq	24(%rsi), %rax
	movq	8(%rdi), %rcx
	cmpq	%rcx, %rax
	jne	.LBB6_6
# BB#1:                                 # %if.end
	movq	16(%rsi), %rcx
	movabsq	$entry_exit_blocks, %rax
	cmpq	%rax, %rcx
	movq	$1, %rax
	je	.LBB6_5
# BB#2:                                 # %lor.rhs
	movl	88(%rcx), %ecx
	movq	64(%rdi), %rax
	movq	%rcx, %rdx
	shrq	$6, %rdx
	movq	16(%rax,%rdx,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rax, %rdx
	jne	.LBB6_3
# BB#4:                                 # %lor.rhs
	movabsq	$1, %rax
.LBB6_5:                                # %lor.end
	popq	%rdx
	retq
.LBB6_3:
	movabsq	$0, %rax
	popq	%rdx
	retq
.LBB6_6:                                # %if.then
	movabsq	$.L.str17, %rax
	movabsq	$.L__FUNCTION__.flow_loop_outside_edge_p, %rcx
	movq	$832, %rsi              # imm = 0x340
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp95:
	.size	flow_loop_outside_edge_p, .Ltmp95-flow_loop_outside_edge_p
	.cfi_endproc

	.align	16, 0x90
	.type	flow_loop_level_compute,@function
flow_loop_level_compute:                # @flow_loop_level_compute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp102:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp103:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp104:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp105:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp106:
	.cfi_def_cfa_offset 48
.Ltmp107:
	.cfi_offset %rbx, -48
.Ltmp108:
	.cfi_offset %r12, -40
.Ltmp109:
	.cfi_offset %r13, -32
.Ltmp110:
	.cfi_offset %r14, -24
.Ltmp111:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	xorq	%rax, %rax
	testq	%r15, %r15
	je	.LBB7_7
# BB#1:                                 # %if.end
	movq	136(%r15), %rbx
	testq	%rbx, %rbx
	movq	$1, %rax
	je	.LBB7_6
# BB#2:                                 # %for.body.lr.ph
	leaq	1(%r14), %r12
	movq	$1, %r13
	.align	16, 0x90
.LBB7_3:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	flow_loop_level_compute
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jge	.LBB7_5
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	%r13, %rax
.LBB7_5:                                # %for.body
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	144(%rbx), %rbx
	testq	%rbx, %rbx
	movq	%rax, %r13
	jne	.LBB7_3
.LBB7_6:                                # %for.end
	movl	%eax, 124(%r15)
	movl	%r14d, 120(%r15)
.LBB7_7:                                # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp112:
	.size	flow_loop_level_compute, .Ltmp112-flow_loop_level_compute
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	";;\n;; Loop %d (%d to %d):%s%s\n"
	.size	.L.str, 31

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	" shared"
	.size	.L.str1, 8

	.type	.L.str2,@object         # @.str2
.L.str2:
	.zero	1
	.size	.L.str2, 1

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	" invalid"
	.size	.L.str3, 9

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	";;\n;; Loop %d:%s%s\n"
	.size	.L.str4, 20

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	";;  header %d, latch %d, pre-header %d, first %d, last %d\n"
	.size	.L.str5, 59

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	";;  depth %d, level %d, outer %ld\n"
	.size	.L.str6, 35

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	";;  pre-header edges"
	.size	.L.str7, 21

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	";;  entry edges"
	.size	.L.str8, 16

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	";;  %d"
	.size	.L.str9, 7

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	" nodes"
	.size	.L.str10, 7

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	";;  exit edges"
	.size	.L.str11, 15

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	";;  exit doms"
	.size	.L.str12, 14

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	";; %d loops found, %d levels\n"
	.size	.L.str13, 30

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	";; loop header %d shared by loops %d, %d %s\n"
	.size	.L.str14, 45

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"disjoint"
	.size	.L.str15, 9

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"nested"
	.size	.L.str16, 7

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"cfgloop.c"
	.size	.L.str17, 10

	.type	.L__FUNCTION__.flow_loops_free,@object # @__FUNCTION__.flow_loops_free
.L__FUNCTION__.flow_loops_free:
	.asciz	"flow_loops_free"
	.size	.L__FUNCTION__.flow_loops_free, 16

	.type	.L__FUNCTION__.flow_loop_scan,@object # @__FUNCTION__.flow_loop_scan
.L__FUNCTION__.flow_loop_scan:
	.asciz	"flow_loop_scan"
	.size	.L__FUNCTION__.flow_loop_scan, 15

	.type	.L__FUNCTION__.flow_loops_find,@object # @__FUNCTION__.flow_loops_find
.L__FUNCTION__.flow_loops_find:
	.asciz	"flow_loops_find"
	.size	.L__FUNCTION__.flow_loops_find, 16

	.type	.L__FUNCTION__.flow_loop_outside_edge_p,@object # @__FUNCTION__.flow_loop_outside_edge_p
.L__FUNCTION__.flow_loop_outside_edge_p:
	.asciz	"flow_loop_outside_edge_p"
	.size	.L__FUNCTION__.flow_loop_outside_edge_p, 25

	.type	.L__FUNCTION__.flow_loop_entry_edges_find,@object # @__FUNCTION__.flow_loop_entry_edges_find
.L__FUNCTION__.flow_loop_entry_edges_find:
	.asciz	"flow_loop_entry_edges_find"
	.size	.L__FUNCTION__.flow_loop_entry_edges_find, 27

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	";; %d succs { "
	.size	.L.str18, 15

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"%d "
	.size	.L.str19, 4

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"} dom"
	.size	.L.str20, 6

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	";; DFS order: "
	.size	.L.str21, 15

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	";; RC order: "
	.size	.L.str23, 14


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
