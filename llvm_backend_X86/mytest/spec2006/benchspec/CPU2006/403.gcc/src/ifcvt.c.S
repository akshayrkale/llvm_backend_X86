	.file	"ifcvt.c.bc"
	.text
	.globl	if_convert
	.align	16, 0x90
	.type	if_convert,@function
if_convert:                             # @if_convert
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
	xorq	%rax, %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rdi
	movl	%eax, num_possible_if_blocks(%rip)
	movl	%eax, num_updated_if_blocks(%rip)
	movl	%eax, num_removed_blocks(%rip)
	je	.LBB0_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	jmp	.LBB0_3
.LBB0_1:
	movabsq	$0, %rax
.LBB0_3:                                # %entry
	movb	%al, life_data_ok(%rip)
	movq	$1, %rdi
	movq	$1, %rbx
	callq	free_basic_block_vars
	movzbl	life_data_ok(%rip), %eax
	movq	$0, post_dominators(%rip)
	testq	%rax, %rax
	je	.LBB0_5
# BB#4:                                 # %if.then
	movl	n_basic_blocks(%rip), %edi
	movq	%rdi, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, post_dominators(%rip)
	xorq	%rdi, %rdi
	movq	$1, %rdx
	movq	%rax, %rsi
	callq	calculate_dominance_info
.LBB0_5:                                # %for.cond.preheader
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_14
# BB#6:
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB0_7:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rdx,4), %rcx
	movq	%rdx, 80(%rcx)
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	addq	$2, %rdx
	cmpq	%rax, %rsi
	jl	.LBB0_7
# BB#8:                                 # %for.cond2.preheader
	testq	%rax, %rax
	jle	.LBB0_14
# BB#9:
	xorq	%r15, %r15
	.align	16, 0x90
.LBB0_10:                               # %for.body5
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%r15, %rax
	movq	basic_block_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %rbx
	movq	%rbx, %rdi
	callq	find_if_header
	testq	%r14, %rax
	jne	.LBB0_11
# BB#12:                                # %if.else
                                        #   in Loop: Header=BB0_10 Depth=1
	incq	%r15
	jmp	.LBB0_13
	.align	16, 0x90
.LBB0_11:                               # %if.then13
                                        #   in Loop: Header=BB0_10 Depth=1
	movl	88(%rbx), %r15d
.LBB0_13:                               # %for.cond2.backedge
                                        #   in Loop: Header=BB0_10 Depth=1
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB0_10
.LBB0_14:                               # %for.end16
	movq	post_dominators(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_16
# BB#15:                                # %if.then18
	callq	free
.LBB0_16:                               # %if.end19
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_18
# BB#17:                                # %if.then21
	callq	fflush
.LBB0_18:                               # %if.end23
	movl	num_removed_blocks(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_28
# BB#19:                                # %if.end23
	movzbl	life_data_ok(%rip), %eax
	testq	$255, %rax
	je	.LBB0_28
# BB#20:                                # %if.then27
	movl	n_basic_blocks(%rip), %edi
	callq	sbitmap_alloc
	movq	%rax, %r15
	movq	%r15, %rdi
	callq	sbitmap_zero
	movslq	max_regno(%rip), %r14
	callq	max_reg_num
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r14
	jge	.LBB0_22
# BB#21:                                # %if.then32
	callq	max_reg_num
	movl	%eax, max_regno(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	allocate_reg_info
.LBB0_22:                               # %for.cond36.preheader
	movslq	n_basic_blocks(%rip), %rdx
	testq	%rdx, %rdx
	jle	.LBB0_27
# BB#23:                                # %for.body39.lr.ph
	movq	basic_block_info(%rip), %rsi
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB0_24:                               # %for.body39
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rsi,%rbx,8), %rcx
	movzbl	80(%rcx), %ecx
	testq	$1, %rcx
	je	.LBB0_26
# BB#25:                                # %if.then46
                                        #   in Loop: Header=BB0_24 Depth=1
	movq	$1, %rdi
	movq	%rbx, %rcx
	shlq	%cl, %rdi
	movq	$3, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	andq	$536870904, %rax        # imm = 0x1FFFFFF8
	orq	%rdi, 16(%r15,%rax)
.LBB0_26:                               # %for.inc51
                                        #   in Loop: Header=BB0_24 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB0_24
.LBB0_27:                               # %for.end53
	callq	clear_aux_for_blocks
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	count_or_remove_death_notes
	movq	$1, %rsi
	movq	$25, %rdx
	movq	%r15, %rdi
	callq	update_life_info
	movq	%r15, %rdi
	callq	free
	jmp	.LBB0_29
.LBB0_28:                               # %if.else55
	callq	clear_aux_for_blocks
.LBB0_29:                               # %if.end56
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_31
# BB#30:                                # %if.end56
	movl	num_possible_if_blocks(%rip), %edx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_31
# BB#32:                                # %if.then61
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movq	rtl_dump_file(%rip), %rdi
	movl	num_updated_if_blocks(%rip), %edx
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movq	rtl_dump_file(%rip), %rdi
	movl	num_removed_blocks(%rip), %edx
	movabsq	$.L.str2, %rsi
	xorq	%rax, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	fprintf  # TAILCALL
.LBB0_31:                               # %if.end65
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	if_convert, .Ltmp10-if_convert
	.cfi_endproc

	.align	16, 0x90
	.type	find_if_header,@function
find_if_header:                         # @find_if_header
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp17:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp18:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp19:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp20:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp21:
	.cfi_def_cfa_offset 48
	subq	$224, %rsp
.Ltmp22:
	.cfi_def_cfa_offset 272
.Ltmp23:
	.cfi_offset %rbx, -48
.Ltmp24:
	.cfi_offset %r12, -40
.Ltmp25:
	.cfi_offset %r13, -32
.Ltmp26:
	.cfi_offset %r14, -24
.Ltmp27:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	40(%r14), %r13
	xorq	%rbx, %rbx
	testq	%r13, %r13
	je	.LBB1_508
# BB#1:                                 # %lor.lhs.false
	movq	8(%r13), %rax
	testq	%rax, %rax
	je	.LBB1_508
# BB#2:                                 # %lor.lhs.false2
	cmpq	$0, 8(%rax)
	jne	.LBB1_508
# BB#3:                                 # %if.end
	movl	48(%r13), %ecx
	testq	$14, %rcx
	jne	.LBB1_508
# BB#4:                                 # %lor.lhs.false5
	movl	48(%rax), %edx
	testq	$14, %rdx
	jne	.LBB1_508
# BB#5:                                 # %if.end10
	testq	$1, %rcx
	je	.LBB1_7
# BB#6:
	movq	%r13, %r8
	movq	%rax, %r13
	jmp	.LBB1_8
.LBB1_7:                                # %if.else
	testq	$1, %rdx
	movq	%rax, %r8
	je	.LBB1_508
.LBB1_8:                                # %if.end21
	movq	24(%r8), %rsi
	movq	32(%rsi), %rax
	cmpq	$0, (%rax)
	jne	.LBB1_421
# BB#9:                                 # %if.end.i
	movq	40(%rsi), %rax
	movq	24(%r13), %r12
	testq	%rax, %rax
	je	.LBB1_14
# BB#10:                                # %land.lhs.true.i
	cmpq	$0, 8(%rax)
	jne	.LBB1_421
# BB#11:                                # %lor.lhs.false.i
	movzbl	48(%rax), %ecx
	testq	$14, %rcx
	jne	.LBB1_421
# BB#12:                                # %if.else27.i
	movq	24(%rax), %r9
	cmpq	%r12, %r9
	je	.LBB1_13
# BB#24:                                # %if.else31.i
	movq	40(%r12), %rax
	testq	%rax, %rax
	je	.LBB1_421
# BB#25:                                # %land.lhs.true33.i
	movq	24(%rax), %rcx
	cmpq	%rcx, %r9
	jne	.LBB1_421
# BB#26:                                # %land.lhs.true37.i
	movq	32(%r12), %rcx
	cmpq	$0, (%rcx)
	jne	.LBB1_421
# BB#27:                                # %land.lhs.true41.i
	cmpq	$0, 8(%rax)
	jne	.LBB1_421
# BB#28:                                # %land.lhs.true44.i
	movzbl	48(%rax), %eax
	testq	$14, %rax
	movq	%r12, %r15
	jne	.LBB1_421
	jmp	.LBB1_29
.LBB1_14:                               # %if.then8.i
	movq	32(%r12), %rax
	cmpq	$0, (%rax)
	jne	.LBB1_421
# BB#15:                                # %if.then12.i
	movq	8(%rsi), %rdi
	xorq	%r15, %r15
	testq	%rdi, %rdi
	je	.LBB1_16
.LBB1_17:                               # %land.lhs.true14.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdi), %eax
	cmpq	$33, %rax
	je	.LBB1_420
# BB#18:                                # %land.lhs.true14.i
                                        #   in Loop: Header=BB1_17 Depth=1
	cmpq	$37, %rax
	jne	.LBB1_19
# BB#20:                                # %land.rhs.i
                                        #   in Loop: Header=BB1_17 Depth=1
	movq	(%rsi), %rax
	cmpq	%rax, %rdi
	je	.LBB1_21
# BB#22:                                # %while.body.i
                                        #   in Loop: Header=BB1_17 Depth=1
	movq	16(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.LBB1_17
# BB#23:
	movq	%r12, %r9
	jmp	.LBB1_29
.LBB1_16:
	movq	%r12, %r9
	jmp	.LBB1_29
.LBB1_13:
	xorq	%r15, %r15
	movq	%r12, %r9
	jmp	.LBB1_29
.LBB1_21:
	movq	%r12, %r9
	jmp	.LBB1_29
.LBB1_420:                              # %land.lhs.true22.i
	movq	%rsi, 104(%rsp)         # 8-byte Spill
	movq	%r8, 128(%rsp)          # 8-byte Spill
	callq	simplejump_p
	movq	128(%rsp), %r8          # 8-byte Reload
	movq	104(%rsp), %rsi         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%r12, %r9
	jne	.LBB1_29
	jmp	.LBB1_421
.LBB1_19:
	movq	%r12, %r9
.LBB1_29:                               # %if.end53.i
	movl	num_possible_if_blocks(%rip), %eax
	movq	rtl_dump_file(%rip), %rdi
	incq	%rax
	movl	%eax, num_possible_if_blocks(%rip)
	testq	%rdi, %rdi
	je	.LBB1_33
# BB#30:                                # %if.then55.i
	movq	%r8, 128(%rsp)          # 8-byte Spill
	movl	88(%r14), %edx
	movl	88(%rsi), %ecx
	testq	%r15, %r15
	movq	%rsi, %r12
	je	.LBB1_32
# BB#31:                                # %if.end68.thread.i
	movq	%r9, 120(%rsp)          # 8-byte Spill
	movl	88(%r9), %r9d
	movl	88(%r15), %r8d
	leaq	88(%r15), %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	%r15, 112(%rsp)         # 8-byte Spill
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movq	104(%rsp), %rcx         # 8-byte Reload
	movl	88(%r12), %eax
	movq	%r12, %rsi
	movq	128(%rsp), %r8          # 8-byte Reload
	jmp	.LBB1_36
.LBB1_33:                               # %if.end68.i
	movl	88(%rsi), %eax
	xorq	%r11, %r11
	testq	%r15, %r15
	je	.LBB1_34
# BB#35:                                # %if.end68.land.lhs.true71_crit_edge.i
	movq	%r9, 120(%rsp)          # 8-byte Spill
	leaq	88(%r15), %rcx
	movq	%r15, 112(%rsp)         # 8-byte Spill
.LBB1_36:                               # %land.lhs.true71.i
	incq	%rax
	movl	(%rcx), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	cmpq	%rcx, %rdx
	movq	$1, %r11
	movq	120(%rsp), %r9          # 8-byte Reload
	jne	.LBB1_421
	jmp	.LBB1_37
.LBB1_32:                               # %if.end68.thread5.i
	movq	%r15, 112(%rsp)         # 8-byte Spill
	movl	88(%r9), %r8d
	movabsq	$.L.str9, %rsi
	xorq	%rax, %rax
	movq	%r9, %r15
	callq	fprintf
	xorq	%r11, %r11
	movq	%r15, %r9
	movl	88(%r12), %eax
	movq	%r12, %rsi
	movq	128(%rsp), %r8          # 8-byte Reload
	jmp	.LBB1_37
.LBB1_34:
	movq	%r15, 112(%rsp)         # 8-byte Spill
.LBB1_37:                               # %if.end76.i
	movq	%r8, %r10
	incq	%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	88(%r9), %edx
	andq	%rcx, %rax
	cmpq	%rdx, %rax
	movabsq	$0, %rcx
	movabsq	$1, %r8
	movq	%rcx, %rax
	jne	.LBB1_39
# BB#38:                                # %if.end76.i
	movq	%r8, %rax
.LBB1_39:                               # %if.end76.i
	movabsq	$4294967294, %rdi       # imm = 0xFFFFFFFE
	cmpq	%rdi, %rdx
	jne	.LBB1_41
# BB#40:                                # %if.end76.i
	movq	%r8, %rcx
.LBB1_41:                               # %if.end76.i
	orq	%rcx, %rax
	movq	%rsi, %rcx
	movq	%r9, %rdx
	jne	.LBB1_43
# BB#42:                                # %if.end76.i
	xorq	%rdx, %rdx
.LBB1_43:                               # %if.end76.i
	orq	%r11, %rax
	movq	%r10, %r8
	je	.LBB1_421
# BB#44:                                # %if.end76.i
	movl	reload_completed(%rip), %eax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rax
	jne	.LBB1_421
# BB#45:                                # %land.lhs.true.i.i
	movq	8(%r14), %rdi
	movq	%rdi, 96(%rsp)          # 8-byte Spill
	leaq	200(%rsp), %rsi
	movq	%rcx, 104(%rsp)         # 8-byte Spill
	movq	%rdx, %r12
	movq	%r8, 128(%rsp)          # 8-byte Spill
	movq	%r11, %r15
	callq	noce_get_condition
	movq	%r15, %rsi
	movq	128(%rsp), %r8          # 8-byte Reload
	movq	%r12, %rdx
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	%rax, 80(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB1_421
# BB#46:                                # %if.end.i.i.i
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, 88(%rsp)          # 8-byte Spill
	callq	onlyjump_p
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	128(%rsp), %r8          # 8-byte Reload
	movq	%r12, %rdx
	movq	%r15, %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	testq	%rdi, %rax
	je	.LBB1_421
# BB#47:                                # %if.end4.i.i.i
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movl	(%rax), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$3342336, %rax          # imm = 0x330000
	je	.LBB1_421
# BB#48:                                # %if.end6.i.i.i
	movq	96(%rsp), %r9           # 8-byte Reload
	movq	%rsi, %rdi
	movq	%r8, %rsi
	movq	(%rcx), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movzbl	(%rax), %eax
	cmpq	$36, %rax
	jne	.LBB1_51
# BB#49:                                # %if.then.i.i.i.i
	movq	8(%rcx), %rax
	cmpq	%rax, 72(%rsp)          # 8-byte Folded Reload
	movq	%rdx, %rax
	movq	%rsi, %r8
	movq	%rdi, %rdx
	movq	%r9, %rsi
	je	.LBB1_421
.LBB1_50:                               # %if.end.i.i.i.i
	movq	%rsi, %r9
	movq	%rdx, %rdi
	movq	%r8, %rsi
	movq	%rax, %rdx
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	24(%rax), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
.LBB1_51:                               # %while.cond.i.i.i.i
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$33, %rax
	movq	%rsi, %r8
	movq	%rdi, %rsi
	movq	%r9, %rdi
	je	.LBB1_421
# BB#52:                                # %while.cond.i.i.i.i
	movq	%rdi, %r10
	movq	%rsi, %r9
	movq	%r8, %rdi
	movq	%rdx, %rsi
	cmpq	$37, %rax
	jne	.LBB1_54
# BB#53:                                # %while.body.i.i.i.i
	movq	8(%rcx), %rax
	cmpq	%rax, 72(%rsp)          # 8-byte Folded Reload
	movq	%rsi, %rax
	movq	%rdi, %r8
	movq	%r9, %rdx
	movq	%r10, %rsi
	jne	.LBB1_50
	jmp	.LBB1_421
.LBB1_54:                               # %first_active_insn.exit.i.i.i
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	movq	%rcx, %rdx
	movq	%rsi, %rcx
	movq	%rdi, %r8
	movq	%r9, %rsi
	movq	%r10, %rdi
	je	.LBB1_421
# BB#55:                                # %lor.lhs.false.i.i.i
	movq	%rdi, %r11
	movq	%rsi, %r10
	movq	%r8, %r9
	movq	%rcx, %r8
	movq	8(%rdx), %rcx
	movq	%rdx, %rdi
	movq	72(%rsp), %rdx          # 8-byte Reload
.LBB1_56:                               # %do.body.i.i.i.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rcx, %rdx
	movq	%rdi, %rsi
	movq	%r8, %rdi
	movq	%r9, %r8
	movq	%r10, %r9
	movq	%r11, %r10
	je	.LBB1_59
# BB#57:                                # %if.end.i433.i.i.i
                                        #   in Loop: Header=BB1_56 Depth=1
	movq	%r10, %r11
	movq	%r9, %r10
	movq	%r8, %r9
	movq	%rdi, %r8
	movq	%rsi, %rdi
	movq	24(%rdx), %rdx
	movzbl	(%rdx), %esi
	cmpq	$37, %rsi
	je	.LBB1_56
# BB#58:                                # %if.end.i433.i.i.i
	cmpq	$33, %rsi
	movq	%rdi, %rsi
	movq	%r8, %rdi
	movq	%r9, %r8
	movq	%r10, %r9
	movq	%r11, %r10
	jne	.LBB1_421
.LBB1_59:                               # %lor.lhs.false11.i.i.i
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB1_421
# BB#60:                                # %cond.true.i.i.i
	movq	%r10, 96(%rsp)          # 8-byte Spill
	movq	%r9, 88(%rsp)           # 8-byte Spill
	movq	%r8, 128(%rsp)          # 8-byte Spill
	movq	%rdi, %r12
	movq	%rsi, %r15
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB1_62
# BB#61:                                # %cond.false.i.i.i
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	single_set_2
.LBB1_62:                               # %cond.end34.i.i.i
	testq	%rax, %rax
	movq	%r15, %rcx
	movq	%r12, %rdx
	movq	128(%rsp), %r8          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	je	.LBB1_421
# BB#63:                                # %if.end39.i.i.i
	movq	%rdx, 120(%rsp)         # 8-byte Spill
	movq	%rcx, %r15
	movq	8(%rax), %rcx
	movq	%rcx, 104(%rsp)         # 8-byte Spill
	movq	16(%rax), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	testq	%rsi, %rsi
	je	.LBB1_80
# BB#64:                                # %if.then47.i.i.i
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r12
	movzbl	(%r12), %eax
	cmpq	$36, %rax
	jne	.LBB1_67
# BB#65:                                # %if.then.i451.i.i.i
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	cmpq	%rax, %r12
	movq	%r15, %rax
	movq	128(%rsp), %r8          # 8-byte Reload
	jne	.LBB1_66
	jmp	.LBB1_421
.LBB1_80:                               # %if.else.i.i.i
	movq	200(%rsp), %rax
	movq	%rax, %rdi
	callq	prev_nonnote_insn
	movq	%rax, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	testq	%r12, %r12
	je	.LBB1_81
# BB#82:                                # %lor.lhs.false96.i.i.i
	movzbl	(%r12), %eax
	cmpq	$32, %rax
	jne	.LBB1_81
# BB#83:                                # %lor.lhs.false96.i.i.i
	movzbl	rtx_class+32(%rip), %eax
	andq	$255, %rax
	cmpq	$105, %rax
	jne	.LBB1_81
# BB#84:                                # %cond.true109.i.i.i
	movq	32(%r12), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movzbl	(%rax), %eax
	cmpq	$47, %rax
	je	.LBB1_86
# BB#85:                                # %cond.false121.i.i.i
	movq	%r12, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	single_set_2
	movq	%rax, 56(%rsp)          # 8-byte Spill
.LBB1_86:                               # %cond.end129.i.i.i
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB1_81
# BB#87:                                # %lor.lhs.false133.i.i.i
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_81
# BB#88:                                # %lor.lhs.false139.i.i.i
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB1_81
# BB#89:                                # %lor.lhs.false142.i.i.i
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_90
.LBB1_81:
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%r15, %rdx
	movq	128(%rsp), %r8          # 8-byte Reload
	jmp	.LBB1_92
.LBB1_90:                               # %lor.lhs.false145.i.i.i
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rax
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%r15, %rdx
	movq	128(%rsp), %r8          # 8-byte Reload
	jne	.LBB1_92
	jmp	.LBB1_91
.LBB1_66:                               # %if.end.i454.i.i.i
	movq	%rax, %r15
	movq	24(%r12), %r12
.LBB1_67:                               # %while.cond.i460.i.i.i
	movzbl	(%r12), %eax
	cmpq	$33, %rax
	movq	%r15, %rcx
	movq	120(%rsp), %rdx         # 8-byte Reload
	movq	128(%rsp), %r8          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	96(%rsp), %rdi          # 8-byte Reload
	je	.LBB1_421
# BB#68:                                # %while.cond.i460.i.i.i
	movq	%rdi, %r9
	movq	%rsi, %rdi
	movq	%r8, %rsi
	cmpq	$37, %rax
	jne	.LBB1_70
# BB#69:                                # %while.body.i462.i.i.i
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	cmpq	%rax, %r12
	movq	%rcx, %rax
	movq	%rsi, %r8
	jne	.LBB1_66
	jmp	.LBB1_421
.LBB1_70:                               # %first_active_insn.exit468.i.i.i
	testq	%r12, %r12
	movq	%rsi, %r8
	movq	%rdi, %rsi
	movq	%r9, %rdi
	je	.LBB1_421
# BB#71:                                # %lor.lhs.false50.i.i.i
	movq	%rdi, %r11
	movq	%rsi, %r10
	movq	%r8, %r9
	movq	%rdx, %r8
	movq	%rcx, %rdi
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx), %rcx
	movq	%r12, %rdx
.LBB1_72:                               # %do.body.i472.i.i.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rcx, %rdx
	movq	%rdi, %rsi
	movq	%r8, %rdi
	movq	%r9, %r8
	movq	%r10, %r9
	movq	%r11, %r10
	je	.LBB1_75
# BB#73:                                # %if.end.i478.i.i.i
                                        #   in Loop: Header=BB1_72 Depth=1
	movq	%r10, %r11
	movq	%r9, %r10
	movq	%r8, %r9
	movq	%rdi, %r8
	movq	%rsi, %rdi
	movq	24(%rdx), %rdx
	movzbl	(%rdx), %esi
	cmpq	$37, %rsi
	je	.LBB1_72
# BB#74:                                # %if.end.i478.i.i.i
	cmpq	$33, %rsi
	movq	%rdi, %rsi
	movq	%r9, %r8
	jne	.LBB1_421
.LBB1_75:                               # %lor.lhs.false53.i.i.i
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB1_421
# BB#76:                                # %cond.true61.i.i.i
	movq	%r8, 128(%rsp)          # 8-byte Spill
	movq	%rsi, %r15
	movq	32(%r12), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movzbl	(%rax), %eax
	cmpq	$47, %rax
	je	.LBB1_78
# BB#77:                                # %cond.false73.i.i.i
	movq	%r12, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	single_set_2
	movq	%rax, 56(%rsp)          # 8-byte Spill
.LBB1_78:                               # %cond.end81.i.i.i
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	movq	%r15, %rcx
	movq	128(%rsp), %r8          # 8-byte Reload
	je	.LBB1_421
# BB#79:                                # %lor.lhs.false85.i.i.i
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	movq	%rcx, %r15
	callq	rtx_equal_p
	movq	128(%rsp), %r8          # 8-byte Reload
	movq	%r15, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_421
.LBB1_91:                               # %cond.true155.i.i.i
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%r12, 64(%rsp)          # 8-byte Spill
.LBB1_92:                               # %cond.end160.i.i.i
	movq	%r8, 128(%rsp)          # 8-byte Spill
	movq	%rdx, %r15
	movq	200(%rsp), %r12
	cmpq	%r12, 96(%rsp)          # 8-byte Folded Reload
	je	.LBB1_103
# BB#93:
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	%rax, 56(%rsp)          # 8-byte Spill
.LBB1_94:                               # %for.body.i.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB1_97
# BB#95:                                # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB1_94 Depth=1
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%r15, %rax
	movq	128(%rsp), %r8          # 8-byte Reload
	jne	.LBB1_421
# BB#96:                                # %land.lhs.true.for.inc_crit_edge.i.i.i
                                        #   in Loop: Header=BB1_94 Depth=1
	movq	%rax, %r15
	movq	200(%rsp), %r12
.LBB1_97:                               # %for.inc.i.i.i
                                        #   in Loop: Header=BB1_94 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	cmpq	%r12, %rax
	jne	.LBB1_94
	jmp	.LBB1_98
.LBB1_102:                              # %for.inc199.i.i.i
                                        #   in Loop: Header=BB1_98 Depth=1
	movq	%rcx, %r15
	movq	24(%r12), %r12
.LBB1_98:                               # %for.cond180.preheader.i.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	96(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r12
	je	.LBB1_103
# BB#99:                                # %for.body183.i.i.i
                                        #   in Loop: Header=BB1_98 Depth=1
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	movq	%r15, %rcx
	jne	.LBB1_102
# BB#100:                               # %land.lhs.true191.i.i.i
                                        #   in Loop: Header=BB1_98 Depth=1
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	%rcx, %r15
	callq	modified_in_p
	movq	128(%rsp), %r8          # 8-byte Reload
	movq	%r15, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	jne	.LBB1_421
# BB#101:                               # %lor.lhs.false194.i.i.i
                                        #   in Loop: Header=BB1_98 Depth=1
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	%rcx, %r15
	callq	modified_in_p
	movq	128(%rsp), %r8          # 8-byte Reload
	movq	%r15, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB1_102
	jmp	.LBB1_421
.LBB1_103:                              # %for.end203.i.i.i
	movq	104(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB1_105
# BB#104:                               # %lor.lhs.false208.i.i.i
	movq	104(%rsp), %rsi         # 8-byte Reload
	movl	8(%rsi), %edx
	cmpq	$52, %rdx
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	ja	.LBB1_109
.LBB1_105:                              # %if.then213.i.i.i
	movl	no_new_pseudos(%rip), %edx
	testq	%rdx, %rdx
	movq	%r15, %rdx
	movq	128(%rsp), %r8          # 8-byte Reload
	jne	.LBB1_421
# BB#106:                               # %if.end216.i.i.i
	movq	%rdx, %r15
	cmpq	$64, %rcx
	jne	.LBB1_108
# BB#107:                               # %cond.true221.i.i.i
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movl	(%rax), %eax
.LBB1_108:                              # %cond.end226.i.i.i
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rdi
	callq	gen_reg_rtx
	movq	%rax, 56(%rsp)          # 8-byte Spill
.LBB1_109:                              # %if.end232.i.i.i
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	noce_operand_ok
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%r15, %rax
	movq	128(%rsp), %r8          # 8-byte Reload
	je	.LBB1_421
# BB#110:                               # %lor.lhs.false235.i.i.i
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %r15
	movq	%r8, %r12
	callq	noce_operand_ok
	movq	%r12, %r8
	movq	%r15, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB1_421
# BB#111:                               # %if.end239.i.i.i
	movq	%rcx, %r15
	movq	%r14, 136(%rsp)
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	%rax, 192(%rsp)
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	%rax, 184(%rsp)
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	%rax, 144(%rsp)
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, 152(%rsp)
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, 160(%rsp)
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	%rax, 168(%rsp)
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 176(%rsp)
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_125
# BB#112:                               # %if.then250.i.i.i
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB1_113
# BB#114:                               # %if.then250.i.i.i
	movabsq	$1, %rax
	jmp	.LBB1_115
.LBB1_125:                              # %if.end283.i.i.i
	movq	176(%rsp), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB1_128
# BB#126:                               # %land.lhs.true.i.i.i.i
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	jne	.LBB1_128
# BB#127:                               # %land.lhs.true3.i.i.i.i
	movq	168(%rsp), %rcx
	movq	const_int_rtx+512(%rip), %rdx
	xorq	%r12, %r12
	cmpq	%rdx, %rcx
	je	.LBB1_132
.LBB1_128:                              # %if.else.i497.i.i.i
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_142
# BB#129:                               # %land.lhs.true7.i.i.i.i
	movq	168(%rsp), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB1_142
# BB#130:                               # %land.lhs.true12.i.i.i.i
	movq	8(%rax), %rax
	cmpq	$1, %rax
	jne	.LBB1_142
# BB#131:                               # %land.lhs.true18.i.i.i.i
	movq	184(%rsp), %rax
	movq	192(%rsp), %rcx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	reversed_comparison_code
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %r12
	je	.LBB1_142
.LBB1_132:                              # %if.end22.i.i.i.i
	callq	start_sequence
	movq	160(%rsp), %rax
	leaq	136(%rsp), %rdx
	xorq	%rcx, %rcx
	movq	%rdx, %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	callq	noce_emit_store_flag
	testq	%rax, %rax
	jne	.LBB1_133
# BB#141:                               # %if.else33.i.i.i.i
	callq	end_sequence
.LBB1_142:                              # %if.end287.i.i.i
	movl	no_new_pseudos(%rip), %eax
	testq	%rax, %rax
	jne	.LBB1_146
# BB#143:                               # %if.end.i535.i.i.i
	movq	160(%rsp), %rax
	movzbl	2(%rax), %eax
	movl	mode_class(,%rax,4), %eax
	movq	%rax, %rcx
	orq	$4, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$6, %rcx
	jne	.LBB1_144
# BB#150:                               # %land.lhs.true.i537.i.i.i
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	jne	.LBB1_151
	jmp	.LBB1_146
.LBB1_113:
	movabsq	$0, %rax
.LBB1_115:                              # %if.then250.i.i.i
	movq	88(%rsp), %rcx          # 8-byte Reload
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB1_121
# BB#116:                               # %land.lhs.true256.i.i.i
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	cmpq	%rax, 64(%rsp)          # 8-byte Folded Reload
	jne	.LBB1_118
# BB#117:                               # %if.then260.i.i.i
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rax
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	%rax, 8(%rcx)
.LBB1_118:                              # %if.end265.i.i.i
	movq	200(%rsp), %rax
	movq	16(%rax), %rax
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	reorder_insns
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB1_120
# BB#119:                               # %if.then273.i.i.i
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	remove_note
	jmp	.LBB1_120
.LBB1_144:                              # %lor.lhs.false8.i.i.i.i
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$8, %rax
	jne	.LBB1_151
# BB#145:                               # %lor.lhs.false8.i.i.i.i
	movl	flag_unsafe_math_optimizations(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_146
.LBB1_151:                              # %if.end18.i.i.i.i
	movq	168(%rsp), %rax
	leaq	136(%rsp), %rcx
	leaq	208(%rsp), %rdx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	noce_get_alt_condition
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB1_146
# BB#152:                               # %if.end21.i.i.i.i
	movzbl	(%r12), %eax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	8(%r12), %rax
	movq	168(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	rtx_equal_p
	movq	16(%r12), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB1_154
# BB#153:                               # %if.then28.i.i.i.i
	movq	176(%rsp), %rax
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB1_157
	jmp	.LBB1_146
.LBB1_121:                              # %if.else275.i.i.i
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB1_120
# BB#122:                               # %land.lhs.true277.i.i.i
	movq	104(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rdi
	callq	side_effects_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%r12, 56(%rsp)          # 8-byte Spill
	movq	%r15, %rax
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	128(%rsp), %r8          # 8-byte Reload
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	96(%rsp), %rsi          # 8-byte Reload
	jne	.LBB1_421
	jmp	.LBB1_123
.LBB1_154:                              # %if.else.i543.i.i.i
	movq	168(%rsp), %rax
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_146
# BB#155:                               # %if.then42.i.i.i.i
	movq	8(%r12), %rax
	movq	176(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_146
# BB#156:                               # %if.end50.i.i.i.i
	movq	80(%rsp), %rdi          # 8-byte Reload
	callq	swap_condition
	movq	%rax, 80(%rsp)          # 8-byte Spill
.LBB1_157:                              # %if.end54.i.i.i.i
	movq	80(%rsp), %rcx          # 8-byte Reload
	addq	$-104, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	movq	%rcx, %rax
	ja	.LBB1_146
# BB#158:                               # %if.end54.i.i.i.i
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	$93, 48(%rsp)           # 8-byte Folded Spill
	jmpq	*.LJTI1_0(,%rax,8)
.LBB1_159:                              # %sw.bb55.i.i.i.i
	movq	$92, 48(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_162
.LBB1_120:                              # %success.thread.i.i.i
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	delete_insn
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	%rax, 56(%rsp)          # 8-byte Spill
	jmp	.LBB1_414
.LBB1_161:                              # %sw.bb57.i.i.i.i
	movq	$1, 80(%rsp)            # 8-byte Folded Spill
	movq	$94, 48(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_162
.LBB1_160:                              # %sw.bb56.i.i.i.i
	movq	$1, 80(%rsp)            # 8-byte Folded Spill
	movq	$95, 48(%rsp)           # 8-byte Folded Spill
.LBB1_162:                              # %sw.epilog.i.i.i.i
	callq	start_sequence
	movq	160(%rsp), %rax
	movq	168(%rsp), %rcx
	movq	176(%rsp), %r8
	movzbl	2(%rax), %edx
	movq	$2, (%rsp)
	movq	%rdx, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rcx, %rdx
	movq	%r8, %rcx
	movq	%rax, %r8
	movq	80(%rsp), %r9           # 8-byte Reload
	callq	expand_simple_binop
	testq	%rax, %rax
	je	.LBB1_163
# BB#164:                               # %if.end68.i.i.i.i
	movq	160(%rsp), %rcx
	cmpq	%rcx, %rax
	je	.LBB1_168
# BB#165:                               # %if.then71.i.i.i.i
	movzbl	(%rcx), %edx
	cmpq	$64, %rdx
	jne	.LBB1_166
# BB#167:                               # %if.end.i.i571.i.i.i
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rdx
	movzbl	2(%rcx), %r8d
	movzbl	2(%rdx), %esi
	movl	16(%rcx), %r9d
	movzbl	mode_bitsize(%r8,%r8), %r10d
	movq	$35, %rcx
	movzbl	mode_bitsize(%rsi,%rsi), %r11d
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	%rdx, %rdi
	movq	%r10, %rsi
	movq	%r9, %rdx
	movq	%r8, %rcx
	movq	%rax, %r8
	movq	%r11, %r9
	callq	store_bit_field
	jmp	.LBB1_168
.LBB1_163:                              # %if.then67.i.i.i.i
	callq	end_sequence
	jmp	.LBB1_146
.LBB1_166:                              # %if.then.i.i549.i.i.i
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB1_168:                              # %if.end73.i.i.i.i
	callq	get_insns
	movq	%rax, 80(%rsp)          # 8-byte Spill
	callq	end_sequence
	cmpq	$0, 80(%rsp)            # 8-byte Folded Reload
	movq	80(%rsp), %rdx          # 8-byte Reload
	je	.LBB1_172
# BB#169:
	movq	%rdx, %rax
.LBB1_170:                              # %while.body.i.i.i.i.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	je	.LBB1_146
# BB#171:                               # %if.end.i112.i.i.i.i
                                        #   in Loop: Header=BB1_170 Depth=1
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	.LBB1_170
.LBB1_172:                              # %noce_try_minmax.exit.i.i.i
	movq	184(%rsp), %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	emit_insns_before
	movq	208(%rsp), %rax
	movq	%r12, 192(%rsp)
	jmp	.LBB1_173
.LBB1_146:                              # %if.end291.i.i.i
	movl	no_new_pseudos(%rip), %eax
	testq	%rax, %rax
	jne	.LBB1_208
# BB#147:                               # %if.end.i582.i.i.i
	movq	168(%rsp), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movzbl	(%rax), %eax
	movq	176(%rsp), %rcx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	cmpq	$77, %rax
	jne	.LBB1_175
# BB#148:                               # %land.lhs.true.i586.i.i.i
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	%rax, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	rtx_equal_p
	xorq	%r12, %r12
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_175
# BB#149:
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	%rax, 48(%rsp)          # 8-byte Spill
	jmp	.LBB1_177
.LBB1_175:                              # %if.else.i587.i.i.i
	movq	80(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$77, %rax
	jne	.LBB1_208
# BB#176:                               # %land.lhs.true8.i589.i.i.i
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	%rax, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %r12
	je	.LBB1_208
.LBB1_177:                              # %if.end17.i.i.i.i
	leaq	136(%rsp), %rax
	leaq	208(%rsp), %rcx
	movq	%rax, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rcx, %rdx
	callq	noce_get_alt_condition
	testq	%rax, %rax
	je	.LBB1_208
# BB#178:                               # %if.end21.i592.i.i.i
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	%rax, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	rtx_equal_p
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx), %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB1_181
# BB#179:                               # %if.else31.i.i.i.i
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_208
# BB#180:                               # %if.then37.i.i.i.i
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
.LBB1_181:                              # %if.end43.i.i.i.i
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB1_190
# BB#182:                               # %for.cond.preheader.i.i.i.i
	movq	136(%rsp), %rax
	movq	208(%rsp), %rdx
	jmp	.LBB1_183
.LBB1_188:                              # %for.inc.i.i.i.i
                                        #   in Loop: Header=BB1_183 Depth=1
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	16(%rdx), %rdx
.LBB1_183:                              # %for.cond.preheader.i.i.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	(%rax), %rcx
	cmpq	%rcx, %rdx
	je	.LBB1_208
# BB#184:                               # %for.body.i.i.i.i
                                        #   in Loop: Header=BB1_183 Depth=1
	movq	32(%rsp), %rcx          # 8-byte Reload
	movzbl	(%rcx), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB1_188
# BB#185:                               # %land.lhs.true54.i.i.i.i
                                        #   in Loop: Header=BB1_183 Depth=1
	movq	$4, %rsi
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB1_189
# BB#186:                               # %lor.lhs.false.i595.i.i.i
                                        #   in Loop: Header=BB1_183 Depth=1
	movq	$3, %rsi
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB1_189
# BB#187:                               # %lor.lhs.false.for.inc_crit_edge.i.i.i.i
                                        #   in Loop: Header=BB1_183 Depth=1
	movq	136(%rsp), %rax
	jmp	.LBB1_188
.LBB1_189:                              # %if.end66.i.i.i.i
	movq	8(%rax), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movl	(%rax), %eax
.LBB1_190:                              # %if.end70.i.i.i.i
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB1_193
# BB#191:                               # %land.lhs.true75.i.i.i.i
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movl	(%rax), %ecx
	andq	$67174399, %rcx         # imm = 0x400FFFF
	cmpq	$67108932, %rcx         # imm = 0x4000044
	jne	.LBB1_193
# BB#192:                               # %if.then90.i.i.i.i
	movq	%rax, %rdi
	callq	get_pool_constant
	movq	%rax, 40(%rsp)          # 8-byte Spill
.LBB1_193:                              # %if.end95.i.i.i.i
	movq	const_int_rtx+504(%rip), %rax
	cmpq	%rax, 40(%rsp)          # 8-byte Folded Reload
	jne	.LBB1_195
# BB#194:                               # %land.lhs.true98.i.i.i.i
	movq	80(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$105, %rcx
	je	.LBB1_199
.LBB1_195:                              # %if.else104.i.i.i.i
	movq	const_int_rtx+520(%rip), %rax
	cmpq	%rax, 40(%rsp)          # 8-byte Folded Reload
	jne	.LBB1_197
# BB#196:                               # %land.lhs.true107.i.i.i.i
	movq	80(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$107, %rcx
	je	.LBB1_199
.LBB1_197:                              # %if.else113.i.i.i.i
	movq	48(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %eax
	movq	const_tiny_rtx(,%rax,8), %rax
	cmpq	%rax, 40(%rsp)          # 8-byte Folded Reload
	jne	.LBB1_208
# BB#198:                               # %if.else113.if.end124_crit_edge.i.i.i.i
	movq	80(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %eax
.LBB1_199:                              # %if.end124.i.i.i.i
	andq	$65535, %rax            # imm = 0xFFFF
	addq	$-104, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$14, %rax
	ja	.LBB1_208
# BB#200:                               # %if.end124.i.i.i.i
	movq	$1, %rdx
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	$6147, %rsi             # imm = 0x1803
	jne	.LBB1_203
# BB#201:                               # %if.end124.i.i.i.i
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$24588, %rdx            # imm = 0x600C
	je	.LBB1_208
# BB#202:                               # %sw.bb.i.i.i.i
	xorq	$1, %r12
.LBB1_203:                              # %sw.epilog.i604.i.i.i
	callq	start_sequence
	movq	160(%rsp), %rax
	movzbl	2(%rax), %ecx
	movq	$129, %rsi
	xorq	%r8, %r8
	movq	%rcx, %rdi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	expand_simple_unop
	testq	%rax, %rax
	je	.LBB1_206
# BB#204:                               # %sw.epilog.i604.i.i.i
	testq	%r12, %r12
	je	.LBB1_206
# BB#205:                               # %if.then137.i.i.i.i
	movq	160(%rsp), %rcx
	movzbl	2(%rax), %edx
	movq	$77, %rsi
	xorq	%r8, %r8
	movq	%rdx, %rdi
	movq	%rax, %rdx
	callq	expand_simple_unop
.LBB1_206:                              # %if.end143.i.i.i.i
	testq	%rax, %rax
	je	.LBB1_207
# BB#211:                               # %if.end146.i.i.i.i
	movq	160(%rsp), %rcx
	cmpq	%rcx, %rax
	je	.LBB1_215
# BB#212:                               # %if.then150.i.i.i.i
	movzbl	(%rcx), %edx
	cmpq	$64, %rdx
	jne	.LBB1_213
# BB#214:                               # %if.end.i.i631.i.i.i
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rdx
	movzbl	2(%rcx), %r8d
	movzbl	2(%rdx), %esi
	movl	16(%rcx), %r9d
	movzbl	mode_bitsize(%r8,%r8), %r10d
	movq	$35, %rcx
	movzbl	mode_bitsize(%rsi,%rsi), %r11d
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	%rdx, %rdi
	movq	%r10, %rsi
	movq	%r9, %rdx
	movq	%r8, %rcx
	movq	%rax, %r8
	movq	%r11, %r9
	callq	store_bit_field
	jmp	.LBB1_215
.LBB1_207:                              # %if.then145.i.i.i.i
	callq	end_sequence
	jmp	.LBB1_208
.LBB1_213:                              # %if.then.i.i609.i.i.i
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB1_215:                              # %if.end152.i.i.i.i
	callq	get_insns
	movq	%rax, %r12
	callq	end_sequence
	testq	%r12, %r12
	je	.LBB1_219
# BB#216:
	movq	%r12, %rax
.LBB1_217:                              # %while.body.i.i634.i.i.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	je	.LBB1_208
# BB#218:                               # %if.end.i219.i.i.i.i
                                        #   in Loop: Header=BB1_217 Depth=1
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	.LBB1_217
.LBB1_219:                              # %noce_try_abs.exit.i.i.i
	movq	184(%rsp), %rax
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_insns_before
	movq	208(%rsp), %rax
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 192(%rsp)
.LBB1_173:                              # %success.i.i.i
	movq	%rax, 200(%rsp)
	jmp	.LBB1_174
.LBB1_208:                              # %if.end295.i.i.i
	movq	168(%rsp), %rax
	movzbl	(%rax), %ecx
	cmpq	$133, %rcx
	jg	.LBB1_220
# BB#209:                               # %if.end295.i.i.i
	addq	$-54, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$14, %rdx
	ja	.LBB1_222
# BB#210:                               # %if.end295.i.i.i
	movq	$1, %rsi
	movq	%rdx, %rcx
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	jne	.LBB1_223
	jmp	.LBB1_222
.LBB1_220:                              # %if.end295.i.i.i
	cmpq	$134, %rcx
	je	.LBB1_223
# BB#221:                               # %if.end295.i.i.i
	cmpq	$140, %rcx
	je	.LBB1_223
.LBB1_222:                              # %lor.lhs.false35.i.i.i.i
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	register_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_231
.LBB1_223:                              # %land.lhs.true.i645.i.i.i
	movq	176(%rsp), %rax
	movzbl	(%rax), %ecx
	cmpq	$133, %rcx
	jg	.LBB1_226
# BB#224:                               # %land.lhs.true.i645.i.i.i
	addq	$-54, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$14, %rcx
	ja	.LBB1_228
# BB#225:                               # %land.lhs.true.i645.i.i.i
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB1_229
	jmp	.LBB1_228
.LBB1_226:                              # %land.lhs.true.i645.i.i.i
	cmpq	$134, %rcx
	je	.LBB1_229
# BB#227:                               # %land.lhs.true.i645.i.i.i
	cmpq	$140, %rcx
	je	.LBB1_229
.LBB1_228:                              # %lor.lhs.false75.i.i.i.i
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	register_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_231
.LBB1_229:                              # %if.then.i651.i.i.i
	callq	start_sequence
	movq	192(%rsp), %rax
	movq	8(%rax), %rcx
	movq	16(%rax), %r8
	movq	160(%rsp), %rdx
	movq	168(%rsp), %r9
	movq	176(%rsp), %rsi
	movzbl	(%rax), %eax
	leaq	136(%rsp), %rdi
	movq	%rsi, (%rsp)
	movq	%rdx, %rsi
	movq	%rax, %rdx
	callq	noce_emit_cmove
	testq	%rax, %rax
	je	.LBB1_230
.LBB1_133:                              # %if.then24.i.i.i.i
	movq	160(%rsp), %rcx
	cmpq	%rcx, %rax
	je	.LBB1_140
# BB#134:                               # %if.then27.i.i.i.i
	movzbl	(%rcx), %edx
	cmpq	$64, %rdx
	je	.LBB1_138
# BB#135:                               # %if.then.i.i505.i.i.i
	movq	%rcx, %rdi
	jmp	.LBB1_136
.LBB1_230:                              # %if.else.i682.i.i.i
	callq	end_sequence
.LBB1_231:                              # %if.end299.i.i.i
	movl	no_new_pseudos(%rip), %eax
	testq	%rax, %rax
	jne	.LBB1_288
# BB#232:                               # %land.lhs.true.i689.i.i.i
	movq	168(%rsp), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB1_288
# BB#233:                               # %land.lhs.true4.i.i.i.i
	movq	176(%rsp), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB1_288
# BB#234:                               # %if.then.i699.i.i.i
	movq	160(%rsp), %rdx
	movq	8(%rcx), %rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	8(%rax), %rsi
	movzbl	2(%rdx), %eax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	%rcx, %rax
	subq	%rsi, %rax
	movq	%rsi, %r9
	movabsq	$0, %rsi
	movabsq	$1, %r8
	movq	%rsi, %rdx
	jle	.LBB1_236
# BB#235:                               # %if.then.i699.i.i.i
	movq	%r8, %rdx
.LBB1_236:                              # %if.then.i699.i.i.i
	testq	%rax, %rax
	movq	%rsi, %rdi
	jle	.LBB1_238
# BB#237:                               # %if.then.i699.i.i.i
	movq	%r8, %rdi
.LBB1_238:                              # %if.then.i699.i.i.i
	movq	$63, %rcx
	movq	%r9, %r11
	movq	%r11, 40(%rsp)          # 8-byte Spill
                                        # kill: R9<def> R11<kill>
	shrq	%cl, %r9
	movq	$63, %rcx
	movq	48(%rsp), %r10          # 8-byte Reload
	shrq	%cl, %r10
	testq	%r11, %r11
	jns	.LBB1_240
# BB#239:                               # %if.then.i699.i.i.i
	movq	%r8, %rsi
.LBB1_240:                              # %if.then.i699.i.i.i
	cmpq	%r10, %r9
	je	.LBB1_242
# BB#241:                               # %if.then.i699.i.i.i
	movq	%rsi, %rdx
.LBB1_242:                              # %if.then.i699.i.i.i
	xorq	%rdx, %rdi
	jne	.LBB1_288
# BB#243:                               # %if.end.i704.i.i.i
	movq	%rax, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	trunc_int_for_mode
	movq	184(%rsp), %rcx
	movq	192(%rsp), %rdx
	movq	%rax, %r12
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	callq	reversed_comparison_code
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	cmpq	$-1, %r12
	je	.LBB1_244
# BB#246:                               # %if.end.i704.i.i.i
	cmpq	$1, %r12
	jne	.LBB1_247
.LBB1_244:
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	jmp	.LBB1_245
.LBB1_247:                              # %if.else.i705.i.i.i
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	jne	.LBB1_250
# BB#248:                               # %land.lhs.true41.i.i.i.i
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	exact_log2_wide
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_250
# BB#249:
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	jmp	.LBB1_261
.LBB1_250:                              # %if.else46.i.i.i.i
	cmpq	$0, 48(%rsp)            # 8-byte Folded Reload
	je	.LBB1_251
# BB#266:                               # %if.else56.i.i.i.i
	movslq	ix86_branch_cost(%rip), %rax
	cmpq	$1, %rax
	jle	.LBB1_267
# BB#268:                               # %if.else56.i.i.i.i
	movabsq	$1, %rcx
	jmp	.LBB1_269
.LBB1_251:                              # %land.lhs.true49.i.i.i.i
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	exact_log2_wide
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_253
# BB#252:                               # %land.lhs.true49.i.i.i.i
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	24(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	jne	.LBB1_260
.LBB1_253:                              # %if.else56.thread.i.i.i.i
	movslq	ix86_branch_cost(%rip), %rax
	cmpq	$1, %rax
	jle	.LBB1_254
# BB#255:                               # %if.else56.thread.i.i.i.i
	movabsq	$1, %rcx
	jmp	.LBB1_256
.LBB1_267:
	movabsq	$0, %rcx
.LBB1_269:                              # %if.else56.i.i.i.i
	cmpq	$-1, 48(%rsp)           # 8-byte Folded Reload
	jne	.LBB1_256
# BB#270:                               # %if.else56.i.i.i.i
	movq	$-1, 16(%rsp)           # 8-byte Folded Spill
	cmpq	$1, %rax
	jle	.LBB1_256
# BB#271:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, 8(%rsp)           # 8-byte Spill
	jmp	.LBB1_272
.LBB1_254:
	movabsq	$0, %rcx
.LBB1_256:                              # %if.else63.i.i.i.i
	cmpq	$-1, 40(%rsp)           # 8-byte Folded Reload
	jne	.LBB1_273
# BB#257:                               # %if.else63.i.i.i.i
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	24(%rsp), %rsi          # 8-byte Reload
	testq	%rdx, %rsi
	je	.LBB1_273
# BB#258:                               # %if.else63.i.i.i.i
	testq	%rcx, %rcx
	je	.LBB1_273
# BB#259:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB1_260:                              # %if.then88.i.i.i.i
	negq	%r12
	movq	%r12, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	trunc_int_for_mode
	movq	%rax, %r12
	movq	$1, 32(%rsp)            # 8-byte Folded Spill
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rax, 16(%rsp)          # 8-byte Spill
	jmp	.LBB1_261
.LBB1_273:                              # %if.else72.i.i.i.i
	cmpq	$2, %rax
	jle	.LBB1_288
# BB#274:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB1_245:                              # %if.end91.i.i.i.i
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	%rax, 16(%rsp)          # 8-byte Spill
.LBB1_272:                              # %if.end91.i.i.i.i
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rax, 48(%rsp)          # 8-byte Spill
.LBB1_261:                              # %if.end91.i.i.i.i
	callq	start_sequence
	movq	160(%rsp), %rax
	leaq	136(%rsp), %rcx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	8(%rsp), %rcx           # 8-byte Reload
	callq	noce_emit_store_flag
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB1_287
# BB#262:                               # %if.end96.i.i.i.i
	cmpq	$-1, %r12
	je	.LBB1_264
# BB#263:                               # %if.end96.i.i.i.i
	cmpq	$1, %r12
	jne	.LBB1_277
.LBB1_264:                              # %if.then102.i.i.i.i
	cmpq	$1, %r12
	jne	.LBB1_265
# BB#275:                               # %if.then102.i.i.i.i
	movabsq	$1, %r12
	jmp	.LBB1_276
.LBB1_277:                              # %if.else109.i.i.i.i
	cmpq	$0, 48(%rsp)            # 8-byte Folded Reload
	jne	.LBB1_280
# BB#278:                               # %land.lhs.true112.i.i.i.i
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	exact_log2_wide
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB1_280
# BB#279:                               # %if.then117.i.i.i.i
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	160(%rsp), %r8
	movq	$87, %rsi
	jmp	.LBB1_282
.LBB1_265:
	movabsq	$0, %r12
.LBB1_276:                              # %if.then102.i.i.i.i
	xorq	$1, %r12
	addq	$75, %r12
	xorq	%rdi, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_CONST_INT
	movq	160(%rsp), %r8
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	%rax, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB1_286
.LBB1_280:                              # %if.else121.i.i.i.i
	cmpq	$-1, 16(%rsp)           # 8-byte Folded Reload
	je	.LBB1_281
# BB#283:                               # %if.else128.i.i.i.i
	xorq	%rdi, %rdi
	movq	%r12, %rsi
	callq	gen_rtx_CONST_INT
	movq	160(%rsp), %r8
	movq	$83, %rsi
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	expand_simple_binop
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB1_287
# BB#284:                               # %if.then133.i.i.i.i
	xorq	%rdi, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_CONST_INT
	movq	160(%rsp), %r8
	movq	$75, %rsi
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rdx
	jmp	.LBB1_285
.LBB1_281:                              # %if.then124.i.i.i.i
	xorq	%rdi, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_CONST_INT
	movq	160(%rsp), %r8
	movq	$84, %rsi
.LBB1_282:                              # %if.end140.i711.i.i.i
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	40(%rsp), %rdx          # 8-byte Reload
.LBB1_285:                              # %if.end140.i711.i.i.i
	movq	%rax, %rcx
.LBB1_286:                              # %if.end140.i711.i.i.i
	callq	expand_simple_binop
	testq	%rax, %rax
	je	.LBB1_287
# BB#298:                               # %if.end143.i713.i.i.i
	movq	160(%rsp), %rcx
	cmpq	%rcx, %rax
	je	.LBB1_302
# BB#299:                               # %if.then147.i.i.i.i
	movzbl	(%rcx), %edx
	cmpq	$64, %rdx
	jne	.LBB1_300
# BB#301:                               # %if.end.i.i740.i.i.i
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rdx
	movzbl	2(%rcx), %r8d
	movzbl	2(%rdx), %esi
	movl	16(%rcx), %r9d
	movzbl	mode_bitsize(%r8,%r8), %r10d
	movq	$35, %rcx
	movzbl	mode_bitsize(%rsi,%rsi), %r11d
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	%rdx, %rdi
	movq	%r10, %rsi
	movq	%r9, %rdx
	movq	%r8, %rcx
	movq	%rax, %r8
	movq	%r11, %r9
	callq	store_bit_field
	jmp	.LBB1_302
.LBB1_287:                              # %if.then142.i712.i.i.i
	callq	end_sequence
	jmp	.LBB1_288
.LBB1_300:                              # %if.then.i.i718.i.i.i
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB1_302:                              # %if.end149.i.i.i.i
	callq	get_insns
	movq	%rax, %r12
	callq	end_sequence
	testq	%r12, %r12
	je	.LBB1_306
# BB#303:
	movq	%r12, %rax
.LBB1_304:                              # %while.body.i.i743.i.i.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	je	.LBB1_288
# BB#305:                               # %if.end.i228.i.i.i.i
                                        #   in Loop: Header=BB1_304 Depth=1
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	.LBB1_304
	jmp	.LBB1_306
.LBB1_288:                              # %if.end303.i.i.i
	movl	no_new_pseudos(%rip), %eax
	testq	%rax, %rax
	jne	.LBB1_322
# BB#289:                               # %if.end303.i.i.i
	movl	ix86_branch_cost(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB1_322
# BB#290:                               # %land.lhs.true1.i.i.i.i
	movq	160(%rsp), %rcx
	movq	176(%rsp), %rax
	cmpq	%rcx, %rax
	jne	.LBB1_322
# BB#291:                               # %land.lhs.true3.i757.i.i.i
	movq	168(%rsp), %rcx
	movzbl	(%rcx), %edx
	cmpq	$75, %rdx
	jne	.LBB1_322
# BB#292:                               # %land.lhs.true5.i.i.i.i
	movq	16(%rcx), %rdx
	movq	const_int_rtx+520(%rip), %rsi
	cmpq	%rsi, %rdx
	je	.LBB1_294
# BB#293:                               # %land.lhs.true5.i.i.i.i
	movq	const_int_rtx+504(%rip), %rsi
	cmpq	%rsi, %rdx
	jne	.LBB1_322
.LBB1_294:                              # %land.lhs.true13.i.i.i.i
	movq	8(%rcx), %rcx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_322
# BB#295:                               # %land.lhs.true20.i.i.i.i
	movq	184(%rsp), %rax
	movq	192(%rsp), %rcx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	reversed_comparison_code
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_322
# BB#296:                               # %if.then.i770.i.i.i
	movq	168(%rsp), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	cmpq	$1, %rax
	jne	.LBB1_308
# BB#297:
	xorq	%r12, %r12
	jmp	.LBB1_311
.LBB1_308:                              # %if.then.i770.i.i.i
	cmpq	$-1, %rax
	jne	.LBB1_310
# BB#309:                               # %if.then39.i.i.i.i
	movq	$1, %r12
	jmp	.LBB1_311
.LBB1_310:                              # %if.else40.i.i.i.i
	xorq	%r12, %r12
	movq	%rax, 80(%rsp)          # 8-byte Spill
.LBB1_311:                              # %if.end48.i.i.i.i
	callq	start_sequence
	movq	160(%rsp), %rax
	movzbl	2(%rax), %eax
	movq	%rax, %rdi
	callq	gen_reg_rtx
	leaq	136(%rsp), %rcx
	movq	$1, %rdx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	movq	80(%rsp), %rcx          # 8-byte Reload
	callq	noce_emit_store_flag
	testq	%rax, %rax
	je	.LBB1_321
# BB#312:                               # %if.end65.i.i.i.i
	movq	160(%rsp), %rdx
	movzbl	2(%rdx), %ecx
	addq	$75, %r12
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	%rcx, %rdi
	movq	%r12, %rsi
	movq	%rax, %rcx
	movq	%rdx, %r8
	callq	expand_simple_binop
	testq	%rax, %rax
	je	.LBB1_321
# BB#313:                               # %if.then67.i775.i.i.i
	movq	160(%rsp), %rcx
	cmpq	%rcx, %rax
	je	.LBB1_317
# BB#314:                               # %if.then71.i779.i.i.i
	movzbl	(%rcx), %edx
	cmpq	$64, %rdx
	jne	.LBB1_315
# BB#316:                               # %if.end.i.i803.i.i.i
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rdx
	movzbl	2(%rcx), %r8d
	movzbl	2(%rdx), %esi
	movl	16(%rcx), %r9d
	movzbl	mode_bitsize(%r8,%r8), %r10d
	movq	$35, %rcx
	movzbl	mode_bitsize(%rsi,%rsi), %r11d
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	%rdx, %rdi
	movq	%r10, %rsi
	movq	%r9, %rdx
	movq	%r8, %rcx
	movq	%rax, %r8
	movq	%r11, %r9
	callq	store_bit_field
	jmp	.LBB1_317
.LBB1_321:                              # %if.end81.i.i.i.i
	callq	end_sequence
	jmp	.LBB1_322
.LBB1_315:                              # %if.then.i.i781.i.i.i
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB1_317:                              # %if.end73.i806.i.i.i
	callq	get_insns
	movq	%rax, %r12
	callq	end_sequence
	testq	%r12, %r12
	je	.LBB1_306
# BB#318:
	movq	%r12, %rax
.LBB1_319:                              # %while.body.i.i808.i.i.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	je	.LBB1_322
# BB#320:                               # %if.end.i113.i.i.i.i
                                        #   in Loop: Header=BB1_319 Depth=1
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	.LBB1_319
	jmp	.LBB1_306
.LBB1_322:                              # %if.end307.i.i.i
	movl	no_new_pseudos(%rip), %eax
	testq	%rax, %rax
	jne	.LBB1_343
# BB#323:                               # %if.end307.i.i.i
	movl	ix86_branch_cost(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB1_343
# BB#324:                               # %land.lhs.true1.i817.i.i.i
	movq	168(%rsp), %rax
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_326
# BB#325:                               # %land.lhs.true3.i821.i.i.i
	movq	160(%rsp), %rax
	movq	176(%rsp), %rcx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	rtx_equal_p
	xorq	%r12, %r12
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB1_332
.LBB1_326:                              # %lor.lhs.false.i826.i.i.i
	movq	184(%rsp), %rax
	movq	192(%rsp), %rcx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	reversed_comparison_code
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	je	.LBB1_327
# BB#328:                               # %lor.lhs.false.i826.i.i.i
	movabsq	$1, %r12
	jmp	.LBB1_329
.LBB1_327:
	movabsq	$0, %r12
.LBB1_329:                              # %lor.lhs.false.i826.i.i.i
	testq	%rax, %rax
	je	.LBB1_343
# BB#330:                               # %land.lhs.true7.i827.i.i.i
	movq	176(%rsp), %rax
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_343
# BB#331:                               # %land.lhs.true11.i.i.i.i
	movq	160(%rsp), %rax
	movq	168(%rsp), %rcx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_343
.LBB1_332:                              # %if.then.i834.i.i.i
	callq	start_sequence
	movq	160(%rsp), %rax
	movzbl	2(%rax), %eax
	movq	%rax, %rdi
	callq	gen_reg_rtx
	leaq	136(%rsp), %rcx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%rcx, %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	movq	%r8, %rcx
	callq	noce_emit_store_flag
	testq	%rax, %rax
	je	.LBB1_342
# BB#333:                               # %if.end.i836.i.i.i
	movq	160(%rsp), %rdx
	movzbl	2(%rdx), %ecx
	movq	$83, %rsi
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	%rcx, %rdi
	movq	%rax, %rcx
	movq	%rdx, %r8
	callq	expand_simple_binop
	testq	%rax, %rax
	je	.LBB1_342
# BB#334:                               # %if.then29.i.i.i.i
	movq	160(%rsp), %rcx
	cmpq	%rcx, %rax
	je	.LBB1_338
# BB#335:                               # %if.then33.i840.i.i.i
	movzbl	(%rcx), %edx
	cmpq	$64, %rdx
	jne	.LBB1_336
# BB#337:                               # %if.end.i.i864.i.i.i
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rdx
	movzbl	2(%rcx), %r8d
	movzbl	2(%rdx), %esi
	movl	16(%rcx), %r9d
	movzbl	mode_bitsize(%r8,%r8), %r10d
	movq	$35, %rcx
	movzbl	mode_bitsize(%rsi,%rsi), %r11d
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	%rdx, %rdi
	movq	%r10, %rsi
	movq	%r9, %rdx
	movq	%r8, %rcx
	movq	%rax, %r8
	movq	%r11, %r9
	callq	store_bit_field
	jmp	.LBB1_338
.LBB1_342:                              # %if.end43.i870.i.i.i
	callq	end_sequence
	jmp	.LBB1_343
.LBB1_336:                              # %if.then.i.i842.i.i.i
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB1_338:                              # %if.end35.i.i.i.i
	callq	get_insns
	movq	%rax, %r12
	callq	end_sequence
	testq	%r12, %r12
	je	.LBB1_306
# BB#339:
	movq	%r12, %rax
.LBB1_340:                              # %while.body.i.i867.i.i.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	je	.LBB1_343
# BB#341:                               # %if.end.i70.i.i.i.i
                                        #   in Loop: Header=BB1_340 Depth=1
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	.LBB1_340
.LBB1_306:                              # %noce_try_store_flag_constants.exit.i.i.i
	movq	184(%rsp), %rax
	movq	%r12, %rdi
	jmp	.LBB1_307
.LBB1_343:                              # %if.end311.i.i.i
	movq	160(%rsp), %rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	168(%rsp), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movl	no_new_pseudos(%rip), %eax
	movq	176(%rsp), %rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB1_349
# BB#344:                               # %if.end311.i.i.i
	movl	cse_not_expected(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_349
# BB#345:                               # %land.lhs.true8.i.i.i.i
	movq	16(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$66, %rax
	jne	.LBB1_349
# BB#346:                               # %land.lhs.true9.i.i.i.i
	movq	48(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$66, %rax
	jne	.LBB1_349
# BB#347:                               # %land.lhs.true9.i.i.i.i
	movl	ix86_branch_cost(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$5, %rax
	jl	.LBB1_349
# BB#348:                               # %if.then.i490.i.i.i
	movl	target_flags(%rip), %eax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	$4, %rax
	movq	%rax, %rdi
	callq	gen_reg_rtx
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	$1, 40(%rsp)            # 8-byte Folded Spill
	movq	%r15, %rcx
	movq	128(%rsp), %r8          # 8-byte Reload
	jmp	.LBB1_351
.LBB1_349:                              # %if.else.i.i.i.i
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	may_trap_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%r15, %rax
	movq	128(%rsp), %r8          # 8-byte Reload
	jne	.LBB1_421
# BB#350:                               # %lor.lhs.false.i.i.i.i
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %r15
	movq	%r8, %r12
	callq	may_trap_p
	movq	%r12, %r8
	movq	%r15, %rcx
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	jne	.LBB1_421
.LBB1_351:                              # %if.end24.i.i.i.i
	movq	%r8, 128(%rsp)          # 8-byte Spill
	movq	%rcx, %r15
	movq	192(%rsp), %rax
	movzbl	(%rax), %ecx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	144(%rsp), %r12
	movq	152(%rsp), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	184(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	reversed_comparison_code
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_352
# BB#353:                               # %if.then33.i.i.i.i
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB1_355
# BB#354:                               # %if.else38.i.i.i.i
	movq	48(%rsp), %rcx          # 8-byte Reload
	movzbl	2(%rcx), %eax
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	general_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_352
.LBB1_355:                              # %if.then47.i.i.i.i
	movq	184(%rsp), %rax
	movq	192(%rsp), %rcx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	reversed_comparison_code
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, 24(%rsp)          # 8-byte Spill
	jmp	.LBB1_356
.LBB1_352:
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	24(%rsp), %r12          # 8-byte Reload
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, 48(%rsp)          # 8-byte Spill
.LBB1_356:                              # %if.end52.i.i.i.i
	callq	start_sequence
	movq	48(%rsp), %rcx          # 8-byte Reload
	movzbl	2(%rcx), %eax
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	general_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB1_366
# BB#357:                               # %if.then58.i.i.i.i
	movl	no_new_pseudos(%rip), %eax
	testq	%rax, %rax
	jne	.LBB1_410
# BB#358:                               # %if.end62.i.i.i.i
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB1_360
# BB#359:                               # %if.then64.i.i.i.i
	movq	48(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %eax
	movq	%rax, %rdi
	callq	gen_reg_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	48(%rsp), %rcx          # 8-byte Reload
	callq	gen_rtx_fmt_ee
	jmp	.LBB1_364
.LBB1_360:                              # %if.else71.i.i.i.i
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	je	.LBB1_410
# BB#361:                               # %if.else74.i.i.i.i
	movq	48(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %eax
	movq	%rax, %rdi
	callq	gen_reg_rtx
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	copy_rtx
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	32(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB1_363
# BB#362:                               # %cond.false.i.i.i.i
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	single_set_2
.LBB1_363:                              # %cond.end102.i.i.i.i
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 8(%rax)
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
.LBB1_364:                              # %if.end112.i.i.i.i
	movq	%rax, %rdi
	callq	emit_insn
	movslq	40(%rax), %rcx
	testq	%rcx, %rcx
	jns	.LBB1_366
# BB#365:                               # %cond.end123.i.i.i.i
	movq	%rax, %rdi
	callq	recog_memoized_1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_410
.LBB1_366:                              # %if.end129.i.i.i.i
	movq	24(%rsp), %rcx          # 8-byte Reload
	movzbl	2(%rcx), %eax
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	general_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB1_376
# BB#367:                               # %if.then135.i.i.i.i
	movl	no_new_pseudos(%rip), %eax
	testq	%rax, %rax
	jne	.LBB1_410
# BB#368:                               # %if.end140.i.i.i.i
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB1_370
# BB#369:                               # %if.then142.i.i.i.i
	movq	24(%rsp), %r12          # 8-byte Reload
	movzbl	2(%r12), %eax
	movq	%rax, %rdi
	callq	gen_reg_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	jmp	.LBB1_374
.LBB1_370:                              # %if.else149.i.i.i.i
	testq	%r12, %r12
	je	.LBB1_410
# BB#371:                               # %if.else152.i.i.i.i
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %eax
	movq	%rax, %rdi
	callq	gen_reg_rtx
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	callq	copy_rtx
	movq	%rax, %r12
	movq	32(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB1_373
# BB#372:                               # %cond.false177.i.i.i.i
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB1_373:                              # %cond.end185.i.i.i.i
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 8(%rax)
	movq	32(%r12), %rax
.LBB1_374:                              # %if.end195.i.i.i.i
	movq	%rax, %rdi
	callq	emit_insn
	movslq	40(%rax), %rcx
	testq	%rcx, %rcx
	jns	.LBB1_376
# BB#375:                               # %cond.end207.i.i.i.i
	movq	%rax, %rdi
	callq	recog_memoized_1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_410
.LBB1_376:                              # %if.end213.i.i.i.i
	movq	192(%rsp), %rax
	movq	8(%rax), %rcx
	movq	16(%rax), %rax
	leaq	136(%rsp), %rdx
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, (%rsp)
	movq	%rdx, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %r8
	movq	48(%rsp), %r9           # 8-byte Reload
	callq	noce_emit_cmove
	testq	%rax, %rax
	je	.LBB1_410
# BB#377:                               # %if.end225.i.i.i.i
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB1_407
# BB#378:                               # %if.then227.i.i.i.i
	movq	160(%rsp), %rcx
	movzbl	2(%rcx), %ecx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	168(%rsp), %rdx
	movl	(%rdx), %esi
	movq	%rax, %rbx
	testq	$134217728, %rsi        # imm = 0x8000000
	jne	.LBB1_380
# BB#379:                               # %lor.lhs.false238.i.i.i.i
	movq	176(%rsp), %rax
	movzbl	3(%rax), %eax
	testq	$8, %rax
	je	.LBB1_381
.LBB1_380:                              # %if.then244.i.i.i.i
	movl	(%rbx), %eax
	orq	$134217728, %rax        # imm = 0x8000000
	movl	%eax, (%rbx)
	movq	168(%rsp), %rdx
	movl	(%rdx), %esi
.LBB1_381:                              # %if.end247.i.i.i.i
	testq	$268435456, %rsi        # imm = 0x10000000
	je	.LBB1_384
# BB#382:                               # %land.lhs.true253.i.i.i.i
	movq	176(%rsp), %rax
	movzbl	3(%rax), %eax
	testq	$16, %rax
	je	.LBB1_384
# BB#383:                               # %if.then259.i.i.i.i
	movl	(%rbx), %eax
	orq	$268435456, %rax        # imm = 0x10000000
	movl	%eax, (%rbx)
	movq	168(%rsp), %rdx
	movl	(%rdx), %esi
.LBB1_384:                              # %if.end263.i.i.i.i
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jns	.LBB1_387
# BB#385:                               # %land.lhs.true268.i.i.i.i
	movq	176(%rsp), %rax
	movslq	(%rax), %rax
	testq	%rax, %rax
	jns	.LBB1_387
# BB#386:                               # %if.then273.i.i.i.i
	movl	(%rbx), %eax
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	orq	%rax, %rcx
	movl	%ecx, (%rbx)
	movq	168(%rsp), %rdx
.LBB1_387:                              # %if.end277.i.i.i.i
	movq	16(%rdx), %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	movabsq	$0, %rdx
	je	.LBB1_389
# BB#388:                               # %cond.false284.i.i.i.i
	movq	(%rax), %rdx
.LBB1_389:                              # %cond.end289.i.i.i.i
	movq	176(%rsp), %rsi
	movq	16(%rsi), %rsi
	testq	%rsi, %rsi
	je	.LBB1_391
# BB#390:                               # %cond.false298.i.i.i.i
	movq	(%rsi), %rcx
.LBB1_391:                              # %cond.end304.i.i.i.i
	cmpq	%rcx, %rdx
	jne	.LBB1_395
# BB#392:                               # %if.then308.i.i.i.i
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB1_394
# BB#393:                               # %cond.false316.i.i.i.i
	movq	(%rax), %rcx
.LBB1_394:                              # %cond.end322.i.i.i.i
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	callq	set_mem_alias_set
	movq	168(%rsp), %rax
	movq	16(%rax), %rax
.LBB1_395:                              # %if.end324.i.i.i.i
	movq	$8, %rdx
	testq	%rax, %rax
	movq	$8, %rsi
	je	.LBB1_397
# BB#396:                               # %cond.true331.i.i.i.i
	movl	32(%rax), %esi
.LBB1_397:                              # %cond.end337.i.i.i.i
	movq	176(%rsp), %rcx
	movq	16(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB1_399
# BB#398:                               # %cond.true345.i.i.i.i
	movl	32(%rcx), %edx
.LBB1_399:                              # %cond.end352.i.i.i.i
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rdx
	andq	%rdi, %rsi
	cmpq	%rdx, %rsi
	jae	.LBB1_402
# BB#400:                               # %cond.true356.i.i.i.i
	movq	$8, %rdx
	testq	%rax, %rax
	je	.LBB1_404
# BB#401:                               # %cond.true363.i.i.i.i
	movl	32(%rax), %edx
	jmp	.LBB1_404
.LBB1_410:                              # %end_seq_and_fail.i.i.i.i
	callq	end_sequence
	movq	128(%rsp), %r8          # 8-byte Reload
.LBB1_421:                              # %if.end24
	movq	24(%r8), %r12
	movq	24(%r13), %rsi
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %r12
	je	.LBB1_436
# BB#422:                               # %if.end.i.i
	cmpq	$0, 40(%r12)
	jne	.LBB1_436
# BB#423:                               # %if.end2.i.i
	movq	(%r12), %rcx
	movzbl	(%rcx), %edx
	cmpq	$36, %rdx
	jne	.LBB1_427
# BB#424:                               # %if.then.i.i.i
	movq	8(%r12), %rdx
	xorq	%r15, %r15
	jmp	.LBB1_425
.LBB1_407:                              # %if.else391.i.i.i.i
	movq	80(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB1_140
# BB#408:                               # %if.then394.i.i.i.i
	movq	80(%rsp), %rcx          # 8-byte Reload
	movzbl	(%rcx), %ecx
	cmpq	$64, %rcx
	jne	.LBB1_409
# BB#137:                               # %if.end.i522.i.i.i.i
	movq	80(%rsp), %rcx          # 8-byte Reload
.LBB1_138:                              # %if.end.i.i527.i.i.i
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rdx
	movzbl	2(%rcx), %ebx
	movzbl	2(%rdx), %esi
	movl	16(%rcx), %r8d
	movzbl	mode_bitsize(%rbx,%rbx), %r9d
	movq	$35, %rcx
	movzbl	mode_bitsize(%rsi,%rsi), %r10d
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movq	%rdx, %rdi
	movq	%r9, %rsi
	movq	%r8, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r8
	jmp	.LBB1_139
.LBB1_402:                              # %cond.false372.i.i.i.i
	movq	$8, %rdx
	testq	%rcx, %rcx
	je	.LBB1_404
# BB#403:                               # %cond.true379.i.i.i.i
	movl	32(%rcx), %edx
.LBB1_404:                              # %cond.end388.i.i.i.i
	movq	%rbx, %rdi
	movq	%rdx, %rsi
	callq	set_mem_align
	movq	160(%rsp), %rax
	movzbl	(%rax), %ecx
	cmpq	$64, %rcx
	jne	.LBB1_405
# BB#406:                               # %if.end.i.i.i.i.i
	movq	8(%rax), %rax
	movq	8(%rax), %rdx
	movzbl	2(%rax), %r8d
	movzbl	2(%rdx), %esi
	movl	16(%rax), %eax
	movzbl	mode_bitsize(%r8,%r8), %r9d
	movq	$35, %rcx
	movzbl	mode_bitsize(%rsi,%rsi), %r10d
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%r9, %rsi
	movq	%rax, %rdx
	movq	%r8, %rcx
	movq	%rbx, %r8
.LBB1_139:                              # %noce_try_store_flag.exit.i.i.i
	movq	%r10, %r9
	callq	store_bit_field
	jmp	.LBB1_140
.LBB1_409:                              # %if.then.i500.i.i.i.i
	movq	80(%rsp), %rdi          # 8-byte Reload
.LBB1_136:                              # %noce_try_store_flag.exit.i.i.i
	movq	%rax, %rsi
	callq	emit_move_insn
	jmp	.LBB1_140
.LBB1_405:                              # %if.then.i.i.i.i.i
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
.LBB1_140:                              # %noce_try_store_flag.exit.i.i.i
	callq	get_insns
	movq	%rax, %rbx
	callq	end_sequence
	movq	184(%rsp), %rax
	movq	%rbx, %rdi
.LBB1_307:                              # %success.i.i.i
	movq	%rax, %rsi
	callq	emit_insns_before
.LBB1_174:                              # %success.i.i.i
	movq	%r15, %rax
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	96(%rsp), %rsi          # 8-byte Reload
.LBB1_123:                              # %success.i.i.i
	movq	%rsi, 96(%rsp)          # 8-byte Spill
	movq	%rdx, %rbx
	movq	%rcx, 120(%rsp)         # 8-byte Spill
	movq	%rax, %r15
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	delete_insn
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB1_124
# BB#411:                               # %success.i.i.i
	movabsq	$1, %rax
	jmp	.LBB1_412
.LBB1_124:
	movabsq	$0, %rax
.LBB1_412:                              # %success.i.i.i
	andq	%rbx, %rax
	cmpq	$1, %rax
	jne	.LBB1_414
# BB#413:                               # %if.then320.i.i.i
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	delete_insn
.LBB1_414:                              # %if.end322.i.i.i
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	delete_insn
	movq	56(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, 104(%rsp)         # 8-byte Folded Reload
	je	.LBB1_419
# BB#415:                               # %if.then326.i.i.i
	callq	start_sequence
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	copy_rtx
	movzbl	(%rax), %ecx
	cmpq	$64, %rcx
	jne	.LBB1_416
# BB#417:                               # %if.end.i444.i.i.i
	movq	8(%rax), %rax
	movq	8(%rax), %rdx
	movzbl	2(%rax), %ebx
	movzbl	2(%rdx), %esi
	movl	16(%rax), %eax
	movzbl	mode_bitsize(%rbx,%rbx), %r8d
	movq	$35, %rcx
	movzbl	mode_bitsize(%rsi,%rsi), %r9d
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%r8, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	movq	56(%rsp), %r8           # 8-byte Reload
	callq	store_bit_field
	jmp	.LBB1_418
.LBB1_416:                              # %if.then.i438.i.i.i
	movq	%rax, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	emit_move_insn
.LBB1_418:                              # %noce_emit_move_insn.exit.i.i.i
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	8(%r14), %rax
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_insn_after
.LBB1_419:                              # %find_if_block.exit
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	112(%rsp), %rdx         # 8-byte Reload
	movq	120(%rsp), %rcx         # 8-byte Reload
	callq	merge_if_block
	jmp	.LBB1_506
.LBB1_425:                              # %if.then.i.i.i
	cmpq	%rdx, %rcx
	je	.LBB1_431
# BB#426:                               # %if.end.i.i.i73
	movq	24(%rcx), %rcx
.LBB1_427:                              # %while.cond.i.i.i
	movzbl	(%rcx), %edx
	xorq	%r15, %r15
	cmpq	$33, %rdx
	je	.LBB1_431
# BB#428:                               # %while.cond.i.i.i
	cmpq	$37, %rdx
	jne	.LBB1_430
# BB#429:                               # %while.body.i.i.i
	movq	8(%r12), %rdx
	jmp	.LBB1_425
.LBB1_430:                              # %first_active_insn.exit.i.i.loopexit
	movq	%rcx, %r15
.LBB1_431:                              # %first_active_insn.exit.i.i
	movq	8(%r12), %rcx
	cmpq	%rcx, %r15
	jne	.LBB1_436
# BB#432:                               # %land.lhs.true.i.i74
	movq	32(%r15), %rcx
	movzbl	(%rcx), %edx
	cmpq	$52, %rdx
	jne	.LBB1_436
# BB#433:                               # %land.lhs.true5.i.i
	movq	8(%rcx), %rcx
	movq	const_true_rtx(%rip), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB1_436
# BB#434:                               # %land.lhs.true5.i.i
	testq	%r15, %r15
	je	.LBB1_436
# BB#435:
	movq	%rsi, 104(%rsp)         # 8-byte Spill
	movq	%r12, %rsi
	jmp	.LBB1_450
.LBB1_436:                              # %if.else.i
	cmpq	%rax, %rsi
	je	.LBB1_467
# BB#437:                               # %if.end.i109.i
	cmpq	$0, 40(%rsi)
	jne	.LBB1_467
# BB#438:                               # %if.end2.i115.i
	movq	(%rsi), %rax
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB1_442
# BB#439:                               # %if.then.i.i117.i
	movq	8(%rsi), %rcx
	xorq	%r15, %r15
.LBB1_440:                              # %if.then.i.i117.i
	cmpq	%rcx, %rax
	je	.LBB1_446
# BB#441:                               # %if.end.i.i120.i
	movq	24(%rax), %rax
.LBB1_442:                              # %while.cond.i.i124.i
	movzbl	(%rax), %ecx
	xorq	%r15, %r15
	cmpq	$33, %rcx
	je	.LBB1_446
# BB#443:                               # %while.cond.i.i124.i
	cmpq	$37, %rcx
	jne	.LBB1_445
# BB#444:                               # %while.body.i.i126.i
	movq	8(%rsi), %rcx
	jmp	.LBB1_440
.LBB1_445:                              # %first_active_insn.exit.i133.i.loopexit
	movq	%rax, %r15
.LBB1_446:                              # %first_active_insn.exit.i133.i
	movq	8(%rsi), %rax
	cmpq	%rax, %r15
	jne	.LBB1_467
# BB#447:                               # %land.lhs.true.i139.i
	movq	32(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$52, %rcx
	jne	.LBB1_467
# BB#448:                               # %land.lhs.true5.i144.i
	movq	8(%rax), %rax
	movq	const_true_rtx(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_467
# BB#449:                               # %land.lhs.true5.i144.i
	testq	%r15, %r15
	movq	%r12, 104(%rsp)         # 8-byte Spill
	je	.LBB1_467
.LBB1_450:                              # %if.end6.i
	movq	%rsi, 120(%rsp)         # 8-byte Spill
	movq	%r8, 128(%rsp)          # 8-byte Spill
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB1_452
# BB#451:                               # %if.then7.i
	movq	120(%rsp), %rax         # 8-byte Reload
	movl	88(%rax), %ecx
	movl	88(%r14), %edx
	movabsq	$.L.str6, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB1_452:                              # %if.end10.i
	movq	8(%r14), %rdi
	movq	%rdi, 112(%rsp)         # 8-byte Spill
	leaq	216(%rsp), %rsi
	callq	noce_get_condition
	movq	%rax, 96(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movq	128(%rsp), %r8          # 8-byte Reload
	je	.LBB1_467
# BB#453:                               # %if.end14.i
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	onlyjump_p
	movq	120(%rsp), %rdx         # 8-byte Reload
	movq	128(%rsp), %r8          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_467
# BB#454:                               # %if.end18.i
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movl	(%rax), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$3342336, %rcx          # imm = 0x330000
	je	.LBB1_467
# BB#455:                               # %if.end21.i
	cmpq	%rdx, %r12
	je	.LBB1_457
# BB#456:
	movq	96(%rsp), %rcx          # 8-byte Reload
	movzbl	(%rcx), %ecx
	jmp	.LBB1_459
.LBB1_457:                              # %if.then25.i
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	%rax, %rsi
	callq	reversed_comparison_code
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	movq	128(%rsp), %r8          # 8-byte Reload
	je	.LBB1_467
# BB#458:                               # %if.then25.if.end30_crit_edge.i
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
.LBB1_459:                              # %if.end30.i
	movq	32(%r15), %rdx
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	16(%rsi), %r8
	movq	16(%rdx), %r9
	movq	%rcx, %rdi
	movq	%rax, %rsi
	movq	%r8, %rdx
	movq	%r9, %rcx
	callq	gen_cond_trap
	testq	%rax, %rax
	movq	128(%rsp), %r8          # 8-byte Reload
	movq	120(%rsp), %rdx         # 8-byte Reload
	je	.LBB1_467
# BB#460:                               # %if.end46.i
	movq	%r8, %rbx
	movq	216(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	movq	%rdx, %r15
	callq	emit_insn_before
	cmpq	%r15, %r12
	je	.LBB1_462
# BB#461:                               # %if.end46.i
	movq	%r13, %rbx
.LBB1_462:                              # %if.end46.i
	movq	%rbx, %rdi
	callq	remove_edge
	cmpq	$0, 32(%r15)
	jne	.LBB1_464
# BB#463:                               # %if.then51.i
	movq	%r15, %rdi
	callq	flow_delete_block
	movl	num_removed_blocks(%rip), %eax
	incq	%rax
	movl	%eax, num_removed_blocks(%rip)
.LBB1_464:                              # %if.end53.i85
	movl	88(%r14), %eax
	movq	104(%rsp), %rcx         # 8-byte Reload
	movl	88(%rcx), %ecx
	incq	%rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	%rcx, %rax
	jne	.LBB1_466
# BB#465:                               # %if.then57.i
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	delete_insn
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	104(%rsp), %rcx         # 8-byte Reload
	callq	merge_if_block
	jmp	.LBB1_506
.LBB1_467:                              # %if.end28
	cmpq	$0, post_dominators(%rip)
	je	.LBB1_508
# BB#468:                               # %if.then30
	movq	24(%r8), %r15
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.LBB1_486
# BB#469:                               # %lor.lhs.false.i91
	cmpq	$0, 8(%rax)
	jne	.LBB1_486
# BB#470:                               # %if.end.i94
	movzbl	48(%rax), %eax
	testq	$15, %rax
	jne	.LBB1_486
# BB#471:                               # %if.end4.i
	movq	32(%r15), %rax
	cmpq	$0, (%rax)
	jne	.LBB1_486
# BB#472:                               # %if.end7.i
	movq	24(%r13), %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%r15, %rdi
	movq	%r8, %r12
	callq	forwarder_block_p
	movq	%r12, %r8
	testq	%rax, %rax
	jne	.LBB1_486
# BB#473:                               # %if.end9.i
	movq	%r8, 128(%rsp)          # 8-byte Spill
	movl	num_possible_if_blocks(%rip), %eax
	movq	rtl_dump_file(%rip), %rdi
	incq	%rax
	movl	%eax, num_possible_if_blocks(%rip)
	testq	%rdi, %rdi
	je	.LBB1_475
# BB#474:                               # %if.then11.i
	movl	88(%r15), %ecx
	movl	88(%r14), %edx
	movabsq	$.L.str5, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB1_475:                              # %if.end14.i100
	movq	8(%r15), %rcx
	xorq	%rax, %rax
	movq	%r15, %rdx
	jmp	.LBB1_476
.LBB1_466:                              # %if.else59.i
	movq	112(%rsp), %r14         # 8-byte Reload
	movq	64(%r14), %rbx
	movq	%rbx, %rdi
	callq	gen_jump
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	emit_jump_insn_after
	movl	32(%rbx), %ecx
	incq	%rcx
	movl	%ecx, 32(%rbx)
	movq	%rbx, 64(%rax)
	movq	%rax, %rdi
	callq	emit_barrier_after
	movq	%r14, %rdi
	callq	delete_insn
	jmp	.LBB1_506
.LBB1_479:                              # %if.end6.i.i
                                        #   in Loop: Header=BB1_476 Depth=1
	addq	$24, %rdx
.LBB1_476:                              # %while.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdx), %rdx
	movl	(%rdx), %esi
	orq	$2, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$34, %rsi
	jne	.LBB1_478
# BB#477:                               # %if.then.i.i
                                        #   in Loop: Header=BB1_476 Depth=1
	incq	%rax
.LBB1_478:                              # %if.end.i.i104
                                        #   in Loop: Header=BB1_476 Depth=1
	cmpq	%rcx, %rdx
	jne	.LBB1_479
# BB#480:                               # %count_bb_insns.exit.i
	movslq	ix86_branch_cost(%rip), %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	movq	128(%rsp), %r8          # 8-byte Reload
	jg	.LBB1_486
# BB#481:                               # %if.end18.i108
	movq	40(%r15), %rax
	movq	24(%rax), %rax
	movq	$1, %r8
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	120(%rsp), %rdx         # 8-byte Reload
	movq	%rax, %rcx
	callq	dead_or_predicable
	movq	128(%rsp), %r8          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_486
# BB#482:                               # %if.end24.i
	orq	$1, 80(%r14)
	movq	72(%r14), %rax
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	64(%rcx), %rdx
	movq	72(%r15), %rbx
	movq	$2, %rcx
	movq	%rax, %rdi
	movq	%rdx, %rsi
	movq	%rbx, %rdx
	callq	bitmap_operation
	movq	40(%r14), %rax
	movzbl	48(%rax), %ecx
	testq	$1, %rcx
	jne	.LBB1_484
# BB#483:                               # %cond.false.i
	movq	8(%rax), %rax
.LBB1_484:                              # %cond.end.i
	movq	%rax, %rdi
	movq	120(%rsp), %rsi         # 8-byte Reload
	callq	redirect_edge_and_branch_force
	testq	%rax, %rax
	je	.LBB1_505
# BB#485:                               # %if.then37.i
	movq	80(%r15), %rcx
	movq	%rcx, 80(%rax)
	orq	$1, 80(%r15)
	jmp	.LBB1_505
.LBB1_486:                              # %if.end34
	movq	24(%r13), %r15
	movq	40(%r15), %r13
	testq	%r13, %r13
	je	.LBB1_508
# BB#487:                               # %lor.lhs.false.i115
	cmpq	$0, 8(%r13)
	jne	.LBB1_508
# BB#488:                               # %if.end.i119
	movzbl	48(%r13), %eax
	testq	$14, %rax
	jne	.LBB1_508
# BB#489:                               # %if.end4.i123
	movq	32(%r15), %rax
	cmpq	$0, (%rax)
	jne	.LBB1_508
# BB#490:                               # %if.end7.i125
	movq	24(%r8), %r12
	movslq	88(%r12), %rax
	testq	%rax, %rax
	js	.LBB1_508
# BB#491:                               # %if.end10.i128
	movq	8(%r14), %rdi
	movq	$16, %rsi
	xorq	%rdx, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB1_493
# BB#492:                               # %land.lhs.true.i131
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	cmpq	$4999, %rax             # imm = 0x1387
	jg	.LBB1_495
.LBB1_493:                              # %if.else.i132
	movq	24(%r13), %rax
	movslq	88(%rax), %rcx
	testq	%rcx, %rcx
	js	.LBB1_495
# BB#494:                               # %lor.lhs.false19.i
	movq	80(%r12), %rcx
	movq	post_dominators(%rip), %rdx
	movq	80(%rax), %rax
	movabsq	$4611686018427387902, %rsi # imm = 0x3FFFFFFFFFFFFFFE
	andq	%rcx, %rsi
	movq	(%rdx,%rsi,4), %rdx
	movq	$1, %rsi
	movq	$1, %rcx
	movq	%rax, %rdi
	shrq	$7, %rax
	movq	16(%rdx,%rax,8), %rax
	shrq	%cl, %rdi
	movq	%rdi, %rcx
	shlq	%cl, %rsi
	xorq	%rbx, %rbx
	testq	%rsi, %rax
	je	.LBB1_508
.LBB1_495:                              # %if.end34.i
	movl	num_possible_if_blocks(%rip), %eax
	movq	rtl_dump_file(%rip), %rcx
	incq	%rax
	movl	%eax, num_possible_if_blocks(%rip)
	testq	%rcx, %rcx
	je	.LBB1_497
# BB#496:                               # %if.then36.i
	movl	88(%r15), %ebx
	movl	88(%r14), %edx
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rbx, %rcx
	callq	fprintf
.LBB1_497:                              # %if.end40.i
	movq	8(%r12), %rcx
	xorq	%rax, %rax
	movq	%r12, %rdx
	jmp	.LBB1_498
.LBB1_501:                              # %if.end6.i.i150
                                        #   in Loop: Header=BB1_498 Depth=1
	addq	$24, %rdx
.LBB1_498:                              # %while.body.i.i142
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdx), %rdx
	movl	(%rdx), %esi
	orq	$2, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$34, %rsi
	jne	.LBB1_500
# BB#499:                               # %if.then.i.i144
                                        #   in Loop: Header=BB1_498 Depth=1
	incq	%rax
.LBB1_500:                              # %if.end.i.i147
                                        #   in Loop: Header=BB1_498 Depth=1
	cmpq	%rcx, %rdx
	jne	.LBB1_501
# BB#502:                               # %count_bb_insns.exit.i151
	movslq	ix86_branch_cost(%rip), %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rbx, %rbx
	cmpq	%rdx, %rax
	jg	.LBB1_508
# BB#503:                               # %if.end44.i
	movq	24(%r13), %rax
	xorq	%r8, %r8
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	dead_or_predicable
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_508
# BB#504:                               # %find_if_case_2.exit
	orq	$1, 80(%r14)
	movq	72(%r14), %rax
	movq	64(%r12), %rdx
	movq	72(%r15), %rbx
	movq	$2, %rcx
	movq	%rax, %rdi
	movq	%rdx, %rsi
	movq	%rbx, %rdx
	callq	bitmap_operation
.LBB1_505:                              # %success
	movq	%r15, %rdi
	callq	flow_delete_block
	movl	num_removed_blocks(%rip), %eax
	movl	num_updated_if_blocks(%rip), %ecx
	incq	%rax
	movl	%eax, num_removed_blocks(%rip)
	incq	%rcx
	movl	%ecx, num_updated_if_blocks(%rip)
.LBB1_506:                              # %success
	movq	rtl_dump_file(%rip), %rax
	testq	%rax, %rax
	movq	$1, %rbx
	je	.LBB1_508
# BB#507:                               # %if.then41
	movabsq	$.L.str3, %rcx
	movq	$22, %rsi
	movq	$1, %rdx
	movq	$1, %rbx
	movq	%rcx, %rdi
	movq	%rax, %rcx
	callq	fwrite
.LBB1_508:                              # %return
	movq	%rbx, %rax
	addq	$224, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp28:
	.size	find_if_header, .Ltmp28-find_if_header
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI1_0:
	.quad	.LBB1_159
	.quad	.LBB1_159
	.quad	.LBB1_162
	.quad	.LBB1_162
	.quad	.LBB1_161
	.quad	.LBB1_161
	.quad	.LBB1_160
	.quad	.LBB1_160
	.quad	.LBB1_146
	.quad	.LBB1_146
	.quad	.LBB1_146
	.quad	.LBB1_159
	.quad	.LBB1_159
	.quad	.LBB1_162
	.quad	.LBB1_162

	.text
	.align	16, 0x90
	.type	dead_or_predicable,@function
dead_or_predicable:                     # @dead_or_predicable
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp35:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp36:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp37:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp38:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp39:
	.cfi_def_cfa_offset 48
	subq	$208, %rsp
.Ltmp40:
	.cfi_def_cfa_offset 256
.Ltmp41:
	.cfi_offset %rbx, -48
.Ltmp42:
	.cfi_offset %r12, -40
.Ltmp43:
	.cfi_offset %r13, -32
.Ltmp44:
	.cfi_offset %r14, -24
.Ltmp45:
	.cfi_offset %r15, -16
	movq	(%rsi), %rbx
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	8(%rdi), %r9
	movq	%rbx, 200(%rsp)
	movq	8(%rsi), %rax
	movq	%rax, 192(%rsp)
	movl	(%rbx), %edi
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$36, %rcx
	jne	.LBB2_2
# BB#1:                                 # %if.then
	movq	24(%rbx), %rbx
	movq	%rbx, 200(%rsp)
	movl	(%rbx), %edi
.LBB2_2:                                # %if.end
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$37, %rdi
	jne	.LBB2_3
# BB#4:                                 # %if.then7
	cmpq	%rax, %rbx
	je	.LBB2_5
# BB#6:                                 # %if.end10
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	%r9, 64(%rsp)           # 8-byte Spill
	movq	24(%rbx), %rbx
	movq	%rbx, 200(%rsp)
	jmp	.LBB2_7
.LBB2_3:
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	%r9, 64(%rsp)           # 8-byte Spill
.LBB2_7:                                # %if.end14
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB2_11
# BB#8:                                 # %if.then18
	cmpq	%rax, %rbx
	je	.LBB2_9
# BB#10:                                # %if.end21
	movq	16(%rax), %rax
	movq	%rax, 192(%rsp)
.LBB2_11:                               # %for.cond.preheader
	movzbl	(%rbx), %ecx
	cmpq	$34, %rcx
	jne	.LBB2_13
# BB#12:
	xorq	%r12, %r12
	jmp	.LBB2_110
.LBB2_13:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movabsq	$find_memory, %r14
	xorq	%r12, %r12
	.align	16, 0x90
.LBB2_14:                               # %if.end35
                                        # =>This Inner Loop Header: Depth=1
	andq	%r13, %rcx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB2_19
# BB#15:                                # %if.then41
                                        #   in Loop: Header=BB2_14 Depth=1
	movq	32(%rbx), %rdi
	callq	may_trap_p
	testq	%r13, %rax
	jne	.LBB2_16
# BB#17:                                # %if.end46
                                        #   in Loop: Header=BB2_14 Depth=1
	leaq	32(%rbx), %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	for_each_rtx
	testq	%r13, %rax
	jne	.LBB2_110
# BB#18:                                # %if.end46.if.end54_crit_edge
                                        #   in Loop: Header=BB2_14 Depth=1
	movq	192(%rsp), %rax
.LBB2_19:                               # %if.end54
                                        #   in Loop: Header=BB2_14 Depth=1
	cmpq	%rax, %rbx
	je	.LBB2_22
# BB#20:                                # %for.inc
                                        #   in Loop: Header=BB2_14 Depth=1
	movq	24(%rbx), %rbx
	movzbl	(%rbx), %ecx
	cmpq	$34, %rcx
	jne	.LBB2_14
# BB#21:
	xorq	%r12, %r12
	jmp	.LBB2_110
.LBB2_5:                                # %if.then9
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	%r9, 64(%rsp)           # 8-byte Spill
.LBB2_9:                                # %if.then20
	movq	$0, 192(%rsp)
	movq	$0, 200(%rsp)
.LBB2_80:                               # %no_body
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	64(%rbx), %r13
	xorq	%r12, %r12
	movq	80(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, 72(%rsp)          # 8-byte Folded Reload
	movabsq	$0, %r15
	je	.LBB2_85
# BB#81:                                # %if.then345
	movq	80(%rsp), %rdi          # 8-byte Reload
	callq	block_label
	movq	%rax, %r15
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r15, %rsi
	testq	%r14, %rax
	je	.LBB2_83
# BB#82:                                # %cond.true
	callq	invert_jump_1
	jmp	.LBB2_84
.LBB2_22:                               # %for.end
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	any_condjump_p
	testq	%r13, %rax
	je	.LBB2_23
# BB#24:                                # %if.end65
	leaq	184(%rsp), %rsi
	movq	%rbx, %rdi
	callq	noce_get_condition
	testq	%rax, %rax
	movabsq	$0, %r12
	je	.LBB2_110
# BB#25:                                # %if.end69
	leaq	136(%rsp), %rdi
	callq	bitmap_initialize
	movq	%rax, %r15
	movq	%r15, 8(%rsp)           # 8-byte Spill
	leaq	160(%rsp), %rdi
	callq	bitmap_initialize
	movq	%rax, %rbx
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	leaq	112(%rsp), %rdi
	callq	bitmap_initialize
	movq	%rax, %r12
	leaq	88(%rsp), %rdi
	callq	bitmap_initialize
	movq	%rax, 16(%rsp)          # 8-byte Spill
	xorq	%r8, %r8
	xorq	%r14, %r14
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	%rbx, %rcx
	callq	propagate_block
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	jne	.LBB2_47
# BB#26:                                # %do.body
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %r8
	xorq	%r14, %r14
	jmp	.LBB2_27
.LBB2_83:                               # %cond.false
	callq	redirect_jump_1
.LBB2_84:                               # %cond.false
	testq	%r14, %rax
	je	.LBB2_109
.LBB2_85:                               # %if.end354
	callq	apply_change_group
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB2_110
# BB#86:                                # %if.end358
	movq	80(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, 72(%rsp)          # 8-byte Folded Reload
	je	.LBB2_100
# BB#87:                                # %if.then361
	testq	%r13, %r13
	je	.LBB2_89
# BB#88:                                # %if.then363
	movl	32(%r13), %eax
	decq	%rax
	movl	%eax, 32(%r13)
.LBB2_89:                               # %if.end366
	testq	%r15, %r15
	je	.LBB2_91
# BB#90:                                # %if.then368
	movl	32(%r15), %eax
	incq	%rax
	movl	%eax, 32(%r15)
.LBB2_91:                               # %if.end373
	movq	%r15, 64(%rbx)
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	je	.LBB2_93
# BB#92:                                # %if.then378
	movq	%rbx, %rdi
	callq	invert_br_probabilities
.LBB2_93:                               # %if.end379
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rdi
	movzbl	48(%rdi), %eax
	testq	$1, %rax
	je	.LBB2_95
# BB#94:                                # %cond.true382
	movq	8(%rdi), %rdi
.LBB2_95:                               # %cond.end
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	redirect_edge_succ
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	je	.LBB2_100
# BB#96:                                # %if.then388
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rax
	movzbl	48(%rax), %ecx
	testq	$1, %rcx
	je	.LBB2_98
# BB#97:                                # %cond.end473
	movq	8(%rax), %rcx
	movq	56(%rax), %rdx
	movq	56(%rcx), %rsi
	movq	%rdx, 56(%rcx)
	movq	%rsi, 56(%rax)
	movq	8(%rax), %rdx
	movl	52(%rax), %esi
	movl	52(%rdx), %ecx
	movl	%esi, 52(%rdx)
	jmp	.LBB2_99
.LBB2_109:                              # %cancel
	xorq	%rdi, %rdi
	xorq	%r12, %r12
	callq	cancel_changes
	jmp	.LBB2_110
.LBB2_16:
	xorq	%r12, %r12
	jmp	.LBB2_110
.LBB2_23:
	xorq	%r12, %r12
	jmp	.LBB2_110
.LBB2_46:                               # %if.end136.1
                                        #   in Loop: Header=BB2_27 Depth=1
	movq	(%r8), %r8
.LBB2_27:                               # %do.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_29 Depth 2
                                        #     Child Loop BB2_38 Depth 2
	testq	%r8, %r8
	je	.LBB2_47
# BB#28:                                # %for.cond95.preheader
                                        #   in Loop: Header=BB2_27 Depth=1
	movq	24(%r8), %rsi
	testq	%rsi, %rsi
	movq	$1, %rdi
	je	.LBB2_37
	.align	16, 0x90
.LBB2_29:                               # %for.body108
                                        #   Parent Loop BB2_27 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%rdi), %rax
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	testq	%rsi, %rbx
	je	.LBB2_36
# BB#30:                                # %if.then112
                                        #   in Loop: Header=BB2_29 Depth=2
	movl	16(%r8), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	addq	%rdx, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$52, %rdx
	jg	.LBB2_35
# BB#31:                                # %land.lhs.true119
                                        #   in Loop: Header=BB2_29 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	fixed_regs(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB2_35
# BB#32:                                # %land.lhs.true123
                                        #   in Loop: Header=BB2_29 Depth=2
	movzbl	global_regs(%rax), %eax
	testq	%rax, %rax
	movq	$1, %rax
	je	.LBB2_34
# BB#33:                                # %select.mid
                                        #   in Loop: Header=BB2_29 Depth=2
	movq	%r14, %rax
.LBB2_34:                               # %select.end
                                        #   in Loop: Header=BB2_29 Depth=2
	movq	%rax, %r14
.LBB2_35:                               # %if.end128
                                        #   in Loop: Header=BB2_29 Depth=2
	notq	%rbx
	andq	%rbx, %rsi
	je	.LBB2_37
.LBB2_36:                               # %for.inc134
                                        #   in Loop: Header=BB2_29 Depth=2
	movq	%rdi, %rax
	andq	%r13, %rax
	incq	%rdi
	cmpq	$64, %rax
	jb	.LBB2_29
.LBB2_37:                               # %if.end136
                                        #   in Loop: Header=BB2_27 Depth=1
	movq	32(%r8), %rsi
	xorq	%rax, %rax
	testq	%rsi, %rsi
	je	.LBB2_46
	.align	16, 0x90
.LBB2_38:                               # %for.body108.1
                                        #   Parent Loop BB2_27 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	testq	%rsi, %rdi
	je	.LBB2_45
# BB#39:                                # %if.then112.1
                                        #   in Loop: Header=BB2_38 Depth=2
	movl	16(%r8), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	leaq	64(%rax,%rdx), %rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$52, %rdx
	jg	.LBB2_44
# BB#40:                                # %land.lhs.true119.1
                                        #   in Loop: Header=BB2_38 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movzbl	fixed_regs(%rbx), %ecx
	testq	%rcx, %rcx
	jne	.LBB2_44
# BB#41:                                # %land.lhs.true123.1
                                        #   in Loop: Header=BB2_38 Depth=2
	movzbl	global_regs(%rbx), %ecx
	testq	%rcx, %rcx
	movq	$1, %rcx
	je	.LBB2_43
# BB#42:                                # %select.mid33
                                        #   in Loop: Header=BB2_38 Depth=2
	movq	%r14, %rcx
.LBB2_43:                               # %select.end32
                                        #   in Loop: Header=BB2_38 Depth=2
	movq	%rcx, %r14
.LBB2_44:                               # %if.end128.1
                                        #   in Loop: Header=BB2_38 Depth=2
	notq	%rdi
	andq	%rdi, %rsi
	je	.LBB2_46
.LBB2_45:                               # %for.inc134.1
                                        #   in Loop: Header=BB2_38 Depth=2
	incq	%rax
	movq	%rax, %rcx
	andq	%r13, %rcx
	cmpq	$64, %rcx
	jb	.LBB2_38
	jmp	.LBB2_46
.LBB2_47:                               # %if.end143
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	64(%rax), %rsi
	movq	%r12, %rbx
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	callq	bitmap_copy
	xorq	%r8, %r8
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rcx
	movq	%rdx, %r12
	callq	init_propagate_block_info
	movq	%rax, %r15
	movq	64(%rsp), %rbx          # 8-byte Reload
	.align	16, 0x90
.LBB2_48:                               # %for.cond145
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	propagate_one_insn
	movq	184(%rsp), %rcx
	cmpq	%rcx, %rbx
	movq	%rax, %rbx
	jne	.LBB2_48
# BB#49:                                # %for.end152
	movq	%r15, %rdi
	callq	free_propagate_block_info
	movq	$2, %rcx
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	bitmap_operation
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	movq	32(%rsp), %rdx          # 8-byte Reload
	callq	bitmap_operation
	movq	(%rbx), %rax
	jmp	.LBB2_50
.LBB2_59:                               # %if.end227.1
                                        #   in Loop: Header=BB2_50 Depth=1
	movq	(%rax), %rax
.LBB2_50:                               # %for.end152
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_52 Depth 2
                                        #     Child Loop BB2_56 Depth 2
	testq	%rax, %rax
	je	.LBB2_60
# BB#51:                                # %for.cond188.preheader
                                        #   in Loop: Header=BB2_50 Depth=1
	movq	24(%rax), %rdx
	testq	%rdx, %rdx
	movq	$1, %rsi
	je	.LBB2_55
	.align	16, 0x90
.LBB2_52:                               # %for.body203
                                        #   Parent Loop BB2_50 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%rsi), %rcx
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%rdx, %rdi
	je	.LBB2_54
# BB#53:                                # %if.then211
                                        #   in Loop: Header=BB2_52 Depth=2
	notq	%rdi
	andq	%rdi, %rdx
	movq	$1, %r14
	je	.LBB2_55
.LBB2_54:                               # %for.inc224
                                        #   in Loop: Header=BB2_52 Depth=2
	movq	%rsi, %rcx
	andq	%r13, %rcx
	incq	%rsi
	cmpq	$64, %rcx
	jb	.LBB2_52
.LBB2_55:                               # %if.end227
                                        #   in Loop: Header=BB2_50 Depth=1
	movq	32(%rax), %rdx
	testq	%rdx, %rdx
	movq	$1, %rsi
	je	.LBB2_59
	.align	16, 0x90
.LBB2_56:                               # %for.body203.1
                                        #   Parent Loop BB2_50 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%rsi), %rcx
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%rdx, %rdi
	je	.LBB2_58
# BB#57:                                # %if.then211.1
                                        #   in Loop: Header=BB2_56 Depth=2
	notq	%rdi
	andq	%rdi, %rdx
	movq	$1, %r14
	je	.LBB2_59
.LBB2_58:                               # %for.inc224.1
                                        #   in Loop: Header=BB2_56 Depth=2
	movq	%rsi, %rcx
	andq	%r13, %rcx
	incq	%rsi
	cmpq	$64, %rcx
	jb	.LBB2_56
	jmp	.LBB2_59
.LBB2_60:                               # %do.end234
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	64(%rax), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	bitmap_operation
	movq	(%rbx), %rax
	jmp	.LBB2_61
.LBB2_70:                               # %if.end309.1
                                        #   in Loop: Header=BB2_61 Depth=1
	movq	(%rax), %rax
.LBB2_61:                               # %do.end234
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_63 Depth 2
                                        #     Child Loop BB2_67 Depth 2
	testq	%rax, %rax
	je	.LBB2_71
# BB#62:                                # %for.cond270.preheader
                                        #   in Loop: Header=BB2_61 Depth=1
	movq	24(%rax), %rdx
	testq	%rdx, %rdx
	movq	$1, %rsi
	je	.LBB2_66
	.align	16, 0x90
.LBB2_63:                               # %for.body285
                                        #   Parent Loop BB2_61 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%rsi), %rcx
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%rdx, %rdi
	je	.LBB2_65
# BB#64:                                # %if.then293
                                        #   in Loop: Header=BB2_63 Depth=2
	notq	%rdi
	andq	%rdi, %rdx
	movq	$1, %r14
	je	.LBB2_66
.LBB2_65:                               # %for.inc306
                                        #   in Loop: Header=BB2_63 Depth=2
	movq	%rsi, %rcx
	andq	%r13, %rcx
	incq	%rsi
	cmpq	$64, %rcx
	jb	.LBB2_63
.LBB2_66:                               # %if.end309
                                        #   in Loop: Header=BB2_61 Depth=1
	movq	32(%rax), %rdx
	testq	%rdx, %rdx
	movq	$1, %rsi
	je	.LBB2_70
	.align	16, 0x90
.LBB2_67:                               # %for.body285.1
                                        #   Parent Loop BB2_61 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%rsi), %rcx
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%rdx, %rdi
	je	.LBB2_69
# BB#68:                                # %if.then293.1
                                        #   in Loop: Header=BB2_67 Depth=2
	notq	%rdi
	andq	%rdi, %rdx
	movq	$1, %r14
	je	.LBB2_70
.LBB2_69:                               # %for.inc306.1
                                        #   in Loop: Header=BB2_67 Depth=2
	movq	%rsi, %rcx
	andq	%r13, %rcx
	incq	%rsi
	cmpq	$64, %rcx
	jb	.LBB2_67
	jmp	.LBB2_70
.LBB2_71:                               # %do.body317
	movq	%rbx, %rdi
	testq	%rdi, %rdi
	je	.LBB2_73
# BB#72:                                # %if.then319
	callq	bitmap_clear
.LBB2_73:                               # %do.body322
	movq	32(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB2_75
# BB#74:                                # %if.then324
	callq	bitmap_clear
.LBB2_75:                               # %do.body327
	movq	24(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB2_77
# BB#76:                                # %if.then329
	callq	bitmap_clear
.LBB2_77:                               # %do.body332
	movq	%r12, %rdi
	testq	%rdi, %rdi
	je	.LBB2_79
# BB#78:                                # %if.then334
	callq	bitmap_clear
.LBB2_79:                               # %do.end336
	xorq	%r12, %r12
	testq	%r13, %r14
	jne	.LBB2_110
	jmp	.LBB2_80
.LBB2_98:                               # %cond.false483
	movq	8(%rax), %rcx
	movq	56(%rcx), %rcx
	movq	56(%rax), %rdx
	movq	%rcx, 56(%rax)
	movq	8(%rax), %rcx
	movq	%rdx, 56(%rcx)
	movq	8(%rax), %rcx
	movl	52(%rcx), %edx
	movl	52(%rax), %ecx
	movl	%edx, 52(%rax)
	movq	8(%rax), %rax
.LBB2_99:                               # %cond.end486
	movl	%ecx, 52(%rax)
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	update_br_prob_note
.LBB2_100:                              # %if.end490
	movq	200(%rsp), %rax
	testq	%rax, %rax
	je	.LBB2_105
# BB#101:                               # %if.then493
	movq	192(%rsp), %rcx
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	8(%rsi), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB2_103
# BB#102:                               # %if.then497
	movq	16(%rax), %rax
	movq	%rax, 8(%rsi)
.LBB2_103:                              # %if.end502
	leaq	200(%rsp), %rdi
	leaq	192(%rsp), %rsi
	callq	squeeze_notes
	movq	$1, %r12
	testq	%rax, %rax
	jne	.LBB2_110
# BB#104:                               # %if.end505
	movq	184(%rsp), %rax
	movq	200(%rsp), %rdi
	movq	192(%rsp), %rsi
	movq	16(%rax), %rdx
	callq	reorder_insns
.LBB2_105:                              # %if.end509
	movq	$1, %r12
	movq	80(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, 72(%rsp)          # 8-byte Folded Reload
	jne	.LBB2_110
# BB#106:                               # %if.then512
	movq	%rbx, %rdi
	callq	delete_insn
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rdi
	movzbl	48(%rdi), %eax
	testq	$1, %rax
	je	.LBB2_108
# BB#107:                               # %cond.true518
	movq	8(%rdi), %rdi
.LBB2_108:                              # %cond.end523
	callq	remove_edge
.LBB2_110:                              # %return
	movq	%r12, %rax
	addq	$208, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp46:
	.size	dead_or_predicable, .Ltmp46-dead_or_predicable
	.cfi_endproc

	.align	16, 0x90
	.type	find_memory,@function
find_memory:                            # @find_memory
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	movzbl	(%rax), %eax
	cmpq	$66, %rax
	jne	.LBB3_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB3_1:
	movabsq	$0, %rax
	retq
.Ltmp47:
	.size	find_memory, .Ltmp47-find_memory
	.cfi_endproc

	.align	16, 0x90
	.type	noce_get_condition,@function
noce_get_condition:                     # @noce_get_condition
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
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	any_condjump_p
	xorq	%r15, %r15
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB4_25
# BB#1:                                 # %if.end
	movq	%rbx, %rdi
	callq	pc_set
	movq	16(%rax), %rax
	movq	24(%rax), %rcx
	movzbl	(%rcx), %esi
	xorq	%rdx, %rdx
	cmpq	$67, %rsi
	jne	.LBB4_5
# BB#2:                                 # %land.rhs
	movq	8(%rcx), %rcx
	movq	64(%rbx), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB4_3
# BB#4:                                 # %land.rhs
	movabsq	$1, %rdx
	jmp	.LBB4_5
.LBB4_3:
	movabsq	$0, %rdx
.LBB4_5:                                # %land.end
	movq	8(%rax), %rsi
	movq	8(%rsi), %r14
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB4_10
# BB#6:                                 # %land.lhs.true
	movq	$14, %rcx
	shrq	%cl, %rax
	andq	$1020, %rax             # imm = 0x3FC
	movl	mode_class(%rax), %eax
	cmpq	$1, %rax
	jne	.LBB4_10
# BB#7:                                 # %if.then36
	movq	%rbx, (%r12)
	testq	%rdx, %rdx
	je	.LBB4_8
# BB#9:                                 # %if.then38
	movzbl	(%rsi), %edi
	movq	%rsi, %rbx
	callq	reverse_condition
	movq	16(%rbx), %rcx
	movzbl	2(%rbx), %esi
	movq	%rax, %rdi
	movq	%r14, %rdx
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.LBB4_10:                               # %if.end50
	xorq	%r8, %r8
	xorq	%r15, %r15
	movq	%rbx, %rdi
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%r12, %rcx
	callq	canonicalize_condition
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB4_25
# BB#11:                                # %for.cond.preheader
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	(%r12), %r12
	jmp	.LBB4_12
	.align	16, 0x90
.LBB4_15:                               # %for.inc
                                        #   in Loop: Header=BB4_12 Depth=1
	movq	24(%r12), %r12
.LBB4_12:                               # %for.cond.preheader
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rbx, %r12
	je	.LBB4_24
# BB#13:                                # %for.body
                                        #   in Loop: Header=BB4_12 Depth=1
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB4_15
# BB#14:                                # %land.lhs.true65
                                        #   in Loop: Header=BB4_12 Depth=1
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	modified_in_p
	testq	%r13, %rax
	je	.LBB4_15
# BB#16:                                # %if.end76
	movq	8(%r14), %r8
	movl	(%r8), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	jne	.LBB4_25
# BB#17:                                # %lor.lhs.false
	movq	$14, %rcx
	shrq	%cl, %rax
	andq	$1020, %rax             # imm = 0x3FC
	movl	mode_class(%rax), %eax
	cmpq	$1, %rax
	jne	.LBB4_25
# BB#18:                                # %if.end92
	movq	%rbx, %rdi
	movq	8(%rsp), %r12           # 8-byte Reload
	movq	%r12, %rcx
	callq	canonicalize_condition
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB4_25
# BB#19:                                # %for.cond99.preheader
	movq	(%r12), %r12
	jmp	.LBB4_20
.LBB4_8:
	movq	%rsi, %r15
	jmp	.LBB4_25
.LBB4_23:                               # %for.inc115
                                        #   in Loop: Header=BB4_20 Depth=1
	movq	24(%r12), %r12
.LBB4_20:                               # %for.cond99.preheader
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rbx, %r12
	je	.LBB4_24
# BB#21:                                # %for.body102
                                        #   in Loop: Header=BB4_20 Depth=1
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB4_23
# BB#22:                                # %land.lhs.true110
                                        #   in Loop: Header=BB4_20 Depth=1
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	modified_in_p
	testq	%r13, %rax
	je	.LBB4_23
	jmp	.LBB4_25
.LBB4_24:
	movq	%r14, %r15
.LBB4_25:                               # %return
	movq	%r15, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp65:
	.size	noce_get_condition, .Ltmp65-noce_get_condition
	.cfi_endproc

	.align	16, 0x90
	.type	merge_if_block,@function
merge_if_block:                         # @merge_if_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp71:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp72:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp73:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp74:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp75:
	.cfi_def_cfa_offset 48
.Ltmp76:
	.cfi_offset %rbx, -40
.Ltmp77:
	.cfi_offset %r12, -32
.Ltmp78:
	.cfi_offset %r14, -24
.Ltmp79:
	.cfi_offset %r15, -16
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r14
	testq	%rbx, %rbx
	je	.LBB5_4
# BB#1:                                 # %if.then
	movzbl	life_data_ok(%rip), %eax
	testq	%rax, %rax
	je	.LBB5_3
# BB#2:                                 # %if.then2
	movq	72(%r14), %rdi
	movq	72(%rbx), %rsi
	callq	bitmap_copy
.LBB5_3:                                # %if.end
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	merge_blocks_nomove
	movl	num_removed_blocks(%rip), %eax
	incq	%rax
	movl	%eax, num_removed_blocks(%rip)
.LBB5_4:                                # %if.end4
	testq	%r12, %r12
	je	.LBB5_6
# BB#5:                                 # %if.then6
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	merge_blocks_nomove
	movl	num_removed_blocks(%rip), %eax
	incq	%rax
	movl	%eax, num_removed_blocks(%rip)
.LBB5_6:                                # %if.end8
	testq	%r15, %r15
	je	.LBB5_7
# BB#21:                                # %if.else58
	movq	32(%r15), %rax
	testq	%rax, %rax
	je	.LBB5_23
# BB#22:                                # %lor.lhs.false
	cmpq	$0, (%rax)
	jne	.LBB5_27
.LBB5_23:                               # %land.lhs.true62
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %r15
	je	.LBB5_27
# BB#24:                                # %if.then64
	movzbl	life_data_ok(%rip), %eax
	testq	%rax, %rax
	je	.LBB5_26
# BB#25:                                # %if.then66
	movq	72(%r14), %rdi
	movq	72(%r15), %rsi
	callq	bitmap_copy
.LBB5_26:                               # %if.end69
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	merge_blocks_nomove
	movl	num_removed_blocks(%rip), %eax
	incq	%rax
	movl	%eax, num_removed_blocks(%rip)
	jmp	.LBB5_32
.LBB5_7:                                # %if.then10
	movq	40(%r14), %rax
	movq	8(%r14), %rbx
	testq	%rax, %rax
	je	.LBB5_8
# BB#14:                                # %if.else30
	movl	(%rbx), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$33, %rdx
	je	.LBB5_32
# BB#15:                                # %if.else35
	cmpq	$34, %rdx
	jne	.LBB5_18
# BB#16:                                # %if.else35
	movq	24(%rax), %rdx
	movabsq	$entry_exit_blocks+112, %rsi
	cmpq	%rsi, %rdx
	jne	.LBB5_18
# BB#17:                                # %if.else35
	andq	$16777216, %rcx         # imm = 0x1000000
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	jne	.LBB5_32
.LBB5_18:                               # %if.else47
	movzbl	48(%rax), %eax
	testq	$8, %rax
	je	.LBB5_20
# BB#19:                                # %land.lhs.true50
	movq	%rbx, %rdi
	callq	can_throw_internal
	testq	%rax, %rax
	jne	.LBB5_32
.LBB5_20:                               # %if.else53
	movabsq	$.L.str7, %rax
	movabsq	$.L__FUNCTION__.merge_if_block, %rcx
	movq	$1928, %rsi             # imm = 0x788
	jmp	.LBB5_13
.LBB5_27:                               # %if.else71
	movq	40(%r14), %rdi
	cmpq	$0, 8(%rdi)
	jne	.LBB5_29
# BB#28:                                # %lor.lhs.false74
	movq	24(%rdi), %rax
	cmpq	%r15, %rax
	jne	.LBB5_29
# BB#30:                                # %if.end79
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %r15
	je	.LBB5_32
# BB#31:                                # %if.then81
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	tidy_fallthru_edge
	jmp	.LBB5_32
.LBB5_8:                                # %if.then11
	movq	$26, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB5_32
# BB#9:                                 # %if.else
	movzbl	(%rbx), %eax
	cmpq	$32, %rax
	jne	.LBB5_12
# BB#10:                                # %land.lhs.true
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$52, %rcx
	jne	.LBB5_12
# BB#11:                                # %land.lhs.true18
	movq	8(%rax), %rax
	movq	const_true_rtx(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB5_12
.LBB5_32:                               # %if.end85
	orq	$1, 80(%r14)
	movl	num_updated_if_blocks(%rip), %eax
	incq	%rax
	movl	%eax, num_updated_if_blocks(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB5_29:                               # %if.then78
	movabsq	$.L.str7, %rax
	movabsq	$.L__FUNCTION__.merge_if_block, %rcx
	movq	$1956, %rsi             # imm = 0x7A4
	jmp	.LBB5_13
.LBB5_12:                               # %if.else27
	movabsq	$.L.str7, %rax
	movabsq	$.L__FUNCTION__.merge_if_block, %rcx
	movq	$1913, %rsi             # imm = 0x779
.LBB5_13:                               # %if.else27
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp80:
	.size	merge_if_block, .Ltmp80-merge_if_block
	.cfi_endproc

	.align	16, 0x90
	.type	noce_operand_ok,@function
noce_operand_ok:                        # @noce_operand_ok
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp84:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp85:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp86:
	.cfi_def_cfa_offset 32
.Ltmp87:
	.cfi_offset %rbx, -24
.Ltmp88:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB6_3
# BB#1:                                 # %if.then
	movq	8(%rbx), %rdi
	callq	side_effects_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_26
# BB#2:
	movabsq	$0, %rax
	jmp	.LBB6_27
.LBB6_3:                                # %if.end
	movq	%rbx, %rdi
	callq	side_effects_p
	movq	%rax, %rcx
	xorq	%rax, %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rcx
	jne	.LBB6_27
# BB#4:                                 # %if.end4
	movl	flag_trapping_math(%rip), %eax
	testq	%rax, %rax
	jne	.LBB6_24
# BB#5:                                 # %land.lhs.true
	movl	(%rbx), %eax
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movl	mode_class(,%rdx,4), %ecx
	cmpq	$8, %rcx
	ja	.LBB6_24
# BB#6:                                 # %land.lhs.true
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$324, %rsi              # imm = 0x144
	je	.LBB6_24
# BB#7:                                 # %if.then23
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-79(%rax), %rcx
	andq	%r14, %rcx
	cmpq	$3, %rcx
	ja	.LBB6_16
# BB#8:                                 # %sw.bb
	movq	16(%rbx), %rsi
	movzbl	(%rsi), %ecx
	xorq	%rax, %rax
	cmpq	$133, %rcx
	jg	.LBB6_11
# BB#9:                                 # %sw.bb
	addq	$-54, %rcx
	andq	%r14, %rcx
	cmpq	$14, %rcx
	ja	.LBB6_27
# BB#10:                                # %sw.bb
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	$24599, %rdi            # imm = 0x6017
	jne	.LBB6_13
	jmp	.LBB6_27
.LBB6_16:                               # %sw.default
	movsbq	rtx_class(%rax), %rax
	andq	%r14, %rax
	cmpq	$99, %rax
	je	.LBB6_21
# BB#17:                                # %sw.default
	cmpq	$50, %rax
	jne	.LBB6_18
.LBB6_21:                               # %sw.bb110
	movq	8(%rbx), %rdi
	callq	may_trap_p
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%r14, %rcx
	jne	.LBB6_27
# BB#22:                                # %land.rhs116
	movq	16(%rbx), %rdi
	callq	may_trap_p
	testq	%r14, %rax
	je	.LBB6_26
# BB#23:
	movabsq	$0, %rax
	jmp	.LBB6_27
.LBB6_11:                               # %sw.bb
	cmpq	$134, %rcx
	je	.LBB6_13
# BB#12:                                # %sw.bb
	cmpq	$140, %rcx
	jne	.LBB6_27
.LBB6_13:                               # %land.lhs.true81
	movq	const_tiny_rtx(,%rdx,8), %rcx
	cmpq	%rcx, %rsi
	je	.LBB6_27
# BB#14:                                # %land.rhs
	movq	8(%rbx), %rdi
	callq	may_trap_p
	testq	%r14, %rax
	je	.LBB6_26
# BB#15:
	movabsq	$0, %rax
	jmp	.LBB6_27
.LBB6_18:                               # %sw.default
	cmpq	$49, %rax
	jne	.LBB6_24
# BB#19:                                # %sw.bb102
	movq	8(%rbx), %rdi
	callq	may_trap_p
	testq	%r14, %rax
	je	.LBB6_26
# BB#20:
	movabsq	$0, %rax
	jmp	.LBB6_27
.LBB6_24:                               # %if.end127
	movq	%rbx, %rdi
	callq	may_trap_p
	testq	%r14, %rax
	jne	.LBB6_25
.LBB6_26:                               # %if.end127
	movabsq	$1, %rax
	jmp	.LBB6_27
.LBB6_25:
	movabsq	$0, %rax
.LBB6_27:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp89:
	.size	noce_operand_ok, .Ltmp89-noce_operand_ok
	.cfi_endproc

	.align	16, 0x90
	.type	noce_emit_cmove,@function
noce_emit_cmove:                        # @noce_emit_cmove
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp96:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp97:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp98:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp99:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp100:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp101:
	.cfi_def_cfa_offset 112
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
	movq	48(%rdi), %rax
	movq	64(%rdi), %rbx
	movq	112(%rsp), %r13
	movq	%r9, 56(%rsp)           # 8-byte Spill
	movq	%r8, %r12
	movq	%rcx, %r14
	movq	%rsi, %r15
	cmpq	%rax, %rbx
	jne	.LBB7_6
# BB#1:                                 # %if.then
	movq	56(%rdi), %rax
	movzbl	2(%rax), %esi
	movq	%rdx, %rdi
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	%r14, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movzbl	2(%r15), %esi
	movq	$72, %rdi
	movq	%rax, %rdx
	movq	%r13, %rcx
	movq	56(%rsp), %r8           # 8-byte Reload
	callq	gen_rtx_fmt_eee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	callq	start_sequence
	movq	%rbx, %rdi
	callq	emit_insn
	movslq	40(%rax), %rcx
	testq	%rcx, %rcx
	jns	.LBB7_3
# BB#2:                                 # %cond.end
	movq	%rax, %rdi
	callq	recog_memoized_1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB7_5
.LBB7_3:                                # %if.then14
	callq	get_insns
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	callq	emit_insns
	movq	%r15, %r13
	jmp	.LBB7_4
.LBB7_5:                                # %if.end
	callq	end_sequence
	movq	48(%rsp), %rdx          # 8-byte Reload
.LBB7_6:                                # %if.end17
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movq	%r13, %r15
	movzbl	2(%r14), %esi
	movq	%r14, %rdi
	callq	general_operand
	xorq	%r13, %r13
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB7_4
# BB#7:                                 # %lor.lhs.false
	movq	%r12, %rdi
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movzbl	2(%rdi), %esi
	callq	general_operand
	testq	%rbx, %rax
	je	.LBB7_4
# BB#8:                                 # %if.end28
	movabsq	$4294967292, %rax       # imm = 0xFFFFFFFC
	movq	48(%rsp), %rsi          # 8-byte Reload
	andq	%rsi, %rax
	cmpq	$108, %rax
	movq	%r15, %r9
	jne	.LBB7_9
# BB#10:                                # %if.end28
	movabsq	$1, %rax
	jmp	.LBB7_11
.LBB7_4:                                # %return
	movq	%r13, %rax
	jmp	.LBB7_12
.LBB7_9:
	movabsq	$0, %rax
.LBB7_11:                               # %if.end28
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	32(%rsp), %rcx          # 8-byte Reload
	movzbl	2(%rdi), %edx
	movq	%rax, 16(%rsp)
	movq	%rdx, 8(%rsp)
	xorq	%r8, %r8
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%r14, %rdx
	callq	emit_conditional_move
.LBB7_12:                               # %return
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp107:
	.size	noce_emit_cmove, .Ltmp107-noce_emit_cmove
	.cfi_endproc

	.align	16, 0x90
	.type	noce_emit_store_flag,@function
noce_emit_store_flag:                   # @noce_emit_store_flag
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp114:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp115:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp116:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp117:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp118:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp119:
	.cfi_def_cfa_offset 80
.Ltmp120:
	.cfi_offset %rbx, -48
.Ltmp121:
	.cfi_offset %r12, -40
.Ltmp122:
	.cfi_offset %r13, -32
.Ltmp123:
	.cfi_offset %r14, -24
.Ltmp124:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	56(%r12), %r15
	movq	8(%r15), %rdi
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rdx, %r13
	movq	%rsi, %r14
	xorq	%rsi, %rsi
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	callq	general_operand
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB8_2
# BB#1:                                 # %lor.end
	movq	16(%r15), %rdi
	xorq	%rsi, %rsi
	callq	general_operand
	testq	%rbx, %rax
	jne	.LBB8_3
.LBB8_2:                                # %if.then
	movq	48(%r12), %rdi
	callq	pc_set
	movq	16(%rax), %rax
	movq	8(%rax), %r15
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
.LBB8_3:                                # %if.end
	testq	%rbx, %r13
	je	.LBB8_5
# BB#4:                                 # %if.then16
	movq	48(%r12), %rsi
	movq	%r15, %rdi
	callq	reversed_comparison_code
	movq	%rax, %r10
	jmp	.LBB8_6
.LBB8_5:                                # %if.else
	movzbl	(%r15), %r10d
.LBB8_6:                                # %if.end19
	movq	48(%r12), %rax
	movq	64(%r12), %rcx
	cmpq	%rax, %rcx
	jne	.LBB8_7
# BB#8:                                 # %if.end19
	movabsq	$1, %rax
	jmp	.LBB8_9
.LBB8_7:
	movabsq	$0, %rax
.LBB8_9:                                # %if.end19
	movq	24(%rsp), %rcx          # 8-byte Reload
	orq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB8_15
# BB#10:                                # %if.end19
	movq	16(%rsp), %rax          # 8-byte Reload
	andq	%rax, %rbx
	cmpq	$1, %rbx
	ja	.LBB8_15
# BB#11:                                # %if.then25
	movq	8(%r15), %rdx
	movq	16(%r15), %rcx
	movq	%r14, %r13
	movzbl	2(%r13), %esi
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	callq	start_sequence
	movq	%r14, %rdi
	callq	emit_insn
	movslq	40(%rax), %rcx
	testq	%rcx, %rcx
	jns	.LBB8_13
# BB#12:                                # %cond.end
	movq	%rax, %rdi
	callq	recog_memoized_1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB8_14
.LBB8_13:                               # %if.then46
	callq	get_insns
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	callq	emit_insns
	movq	48(%r12), %rax
	movq	%rax, 64(%r12)
	movq	%r13, %rax
	jmp	.LBB8_20
.LBB8_14:                               # %if.end51
	movq	%r13, %r14
	callq	end_sequence
	movq	%rbx, %r10
.LBB8_15:                               # %if.end52
	xorq	%rax, %rax
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	jne	.LBB8_20
# BB#16:                                # %if.end55
	movq	8(%r15), %rdx
	movq	16(%r15), %rcx
	movabsq	$4294967292, %rax       # imm = 0xFFFFFFFC
	andq	%r10, %rax
	cmpq	$108, %rax
	jne	.LBB8_17
# BB#18:                                # %if.end55
	movabsq	$1, %r9
	jmp	.LBB8_19
.LBB8_17:
	movabsq	$0, %r9
.LBB8_19:                               # %if.end55
	movq	%r14, %rdi
	xorq	%r8, %r8
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%r10, %rsi
	callq	emit_store_flag
.LBB8_20:                               # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp125:
	.size	noce_emit_store_flag, .Ltmp125-noce_emit_store_flag
	.cfi_endproc

	.align	16, 0x90
	.type	noce_get_alt_condition,@function
noce_get_alt_condition:                 # @noce_get_alt_condition
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp132:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp133:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp134:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp135:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp136:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp137:
	.cfi_def_cfa_offset 96
.Ltmp138:
	.cfi_offset %rbx, -48
.Ltmp139:
	.cfi_offset %r12, -40
.Ltmp140:
	.cfi_offset %r13, -32
.Ltmp141:
	.cfi_offset %r14, -24
.Ltmp142:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movq	56(%r13), %rax
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	reg_mentioned_p
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB9_2
# BB#1:                                 # %if.then
	movq	64(%r13), %rax
	movq	%rax, (%r14)
	movq	56(%r13), %r12
.LBB9_52:                               # %return
	movq	%r12, %rax
	jmp	.LBB9_53
.LBB9_2:                                # %if.end
	movq	48(%r13), %rdi
	callq	pc_set
	movq	16(%rax), %rcx
	movq	24(%rcx), %rax
	movzbl	(%rax), %esi
	movq	8(%rcx), %r8
	xorq	%rdx, %rdx
	cmpq	$67, %rsi
	jne	.LBB9_6
# BB#3:                                 # %land.rhs
	movq	48(%r13), %rcx
	movq	8(%rax), %rax
	movq	64(%rcx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB9_4
# BB#5:                                 # %land.rhs
	movabsq	$1, %rdx
	jmp	.LBB9_6
.LBB9_4:
	movabsq	$0, %rdx
.LBB9_6:                                # %land.end
	movzbl	(%r15), %eax
	cmpq	$54, %rax
	jne	.LBB9_38
# BB#7:                                 # %if.then30
	movq	56(%r13), %rax
	movq	64(%r13), %rcx
	movzbl	(%rax), %edi
	movq	8(%rax), %r12
	movq	16(%rcx), %rbx
	movq	16(%rax), %rcx
	testq	%rbx, %rbx
	je	.LBB9_8
# BB#9:                                 # %land.lhs.true
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB9_8
# BB#10:                                # %land.lhs.true52
	movq	32(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$47, %rax
	jne	.LBB9_8
# BB#11:                                # %if.then60
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%r8, 40(%rsp)           # 8-byte Spill
	movq	%rbx, %rdi
	callq	find_reg_equal_equiv_note
	testq	%rax, %rax
	jne	.LBB9_13
# BB#12:                                # %if.then63
	movq	32(%rbx), %rax
	movq	16(%rax), %rax
.LBB9_13:                               # %if.end70
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB9_14
# BB#15:                                # %if.then75
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movq	32(%rbx), %rax
	movq	8(%rax), %rsi
	movq	%r12, %rdi
	callq	rtx_equal_p
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB9_17
# BB#16:
	movq	%r14, %rbx
	movq	(%rsp), %r12            # 8-byte Reload
	jmp	.LBB9_19
.LBB9_8:
	movq	%r12, %r9
	movq	%rcx, %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB9_23:                               # %if.end104
	movzbl	(%r12), %eax
	cmpq	$54, %rax
	jne	.LBB9_34
# BB#24:                                # %if.then109
	leaq	-104(%rdi), %rcx
	andq	%rbx, %rcx
	cmpq	$3, %rcx
	ja	.LBB9_34
# BB#25:                                # %if.then109
	movq	8(%r15), %rsi
	movq	8(%r12), %rax
	jmpq	*.LJTI9_0(,%rcx,8)
.LBB9_32:                               # %sw.bb135
	leaq	1(%rsi), %rcx
	movq	$104, %rdi
	cmpq	%rcx, %rax
	jne	.LBB9_34
# BB#33:                                # %if.then139
	xorq	%rdi, %rdi
	movq	%r8, 40(%rsp)           # 8-byte Spill
	movq	%rdx, %r12
	movq	%r9, 32(%rsp)           # 8-byte Spill
	callq	gen_rtx_CONST_INT
	movq	32(%rsp), %r9           # 8-byte Reload
	movq	%r12, %rdx
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	%rax, %r12
	movq	$105, %rdi
	jmp	.LBB9_34
.LBB9_14:
	movq	%r12, %r9
	movq	32(%rsp), %r12          # 8-byte Reload
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	16(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB9_23
.LBB9_30:                               # %sw.bb128
	leaq	-1(%rsi), %rcx
	movq	$105, %rdi
	cmpq	%rcx, %rax
	jne	.LBB9_34
# BB#31:                                # %if.then132
	xorq	%rdi, %rdi
	movq	%r8, 40(%rsp)           # 8-byte Spill
	movq	%rdx, %r12
	movq	%r9, 32(%rsp)           # 8-byte Spill
	callq	gen_rtx_CONST_INT
	movq	32(%rsp), %r9           # 8-byte Reload
	movq	%r12, %rdx
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	%rax, %r12
	movq	$104, %rdi
	jmp	.LBB9_34
.LBB9_28:                               # %sw.bb122
	leaq	-1(%rsi), %rcx
	movq	$106, %rdi
	cmpq	%rcx, %rax
	jne	.LBB9_34
# BB#29:                                # %if.then125
	xorq	%rdi, %rdi
	movq	%r8, 40(%rsp)           # 8-byte Spill
	movq	%rdx, %r12
	movq	%r9, 32(%rsp)           # 8-byte Spill
	callq	gen_rtx_CONST_INT
	movq	32(%rsp), %r9           # 8-byte Reload
	movq	%r12, %rdx
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	%rax, %r12
	movq	$107, %rdi
	jmp	.LBB9_34
.LBB9_26:                               # %sw.bb
	leaq	1(%rsi), %rcx
	movq	$107, %rdi
	cmpq	%rcx, %rax
	jne	.LBB9_34
# BB#27:                                # %if.then119
	xorq	%rdi, %rdi
	movq	%r8, 40(%rsp)           # 8-byte Spill
	movq	%rdx, %r12
	movq	%r9, 32(%rsp)           # 8-byte Spill
	callq	gen_rtx_CONST_INT
	movq	32(%rsp), %r9           # 8-byte Reload
	movq	%r12, %rdx
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	%rax, %r12
	movq	$106, %rdi
.LBB9_34:                               # %if.end142
	movq	56(%r13), %rax
	movzbl	(%rax), %ecx
	movq	%rdi, %rsi
	andq	%rbx, %rsi
	cmpq	%rcx, %rsi
	jne	.LBB9_37
# BB#35:                                # %lor.lhs.false
	movq	8(%rax), %rcx
	cmpq	%rcx, %r9
	jne	.LBB9_37
# BB#36:                                # %lor.lhs.false154
	movq	16(%rax), %rax
	cmpq	%rax, %r12
	je	.LBB9_38
.LBB9_37:                               # %if.then161
	movzbl	2(%r8), %esi
	movq	%r9, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	64(%r13), %rcx
	movq	%rcx, (%r14)
.LBB9_53:                               # %return
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB9_38:                               # %if.end167
	movq	48(%r13), %rdi
	movq	%r8, %rsi
	movq	%r14, %rcx
	movq	%r15, %r8
	callq	canonicalize_condition
	xorq	%r12, %r12
	testq	%rax, %rax
	je	.LBB9_52
# BB#39:                                # %lor.lhs.false171
	movq	%r15, %rdi
	movq	%rax, %rsi
	movq	%rax, 40(%rsp)          # 8-byte Spill
	callq	reg_mentioned_p
	testq	%rbx, %rax
	je	.LBB9_52
# BB#40:                                # %for.cond.preheader
	movq	48(%r13), %rbx
	movq	(%r14), %r15
	cmpq	%r15, %rbx
	je	.LBB9_51
	.align	16, 0x90
.LBB9_41:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB9_44
# BB#42:                                # %land.lhs.true186
                                        #   in Loop: Header=BB9_41 Depth=1
	movq	24(%r13), %rdi
	movq	%rbx, %rsi
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB9_52
# BB#43:                                # %land.lhs.true186.for.inc_crit_edge
                                        #   in Loop: Header=BB9_41 Depth=1
	movq	(%r14), %r15
.LBB9_44:                               # %for.inc
                                        #   in Loop: Header=BB9_41 Depth=1
	movq	16(%rbx), %rbx
	cmpq	%r15, %rbx
	jne	.LBB9_41
# BB#45:                                # %for.cond194.preheader
	movq	48(%r13), %rax
	cmpq	%rax, %r15
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	je	.LBB9_51
	.align	16, 0x90
.LBB9_46:                               # %for.body198
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r15), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB9_50
# BB#47:                                # %land.lhs.true206
                                        #   in Loop: Header=BB9_46 Depth=1
	movq	32(%r13), %rdi
	movq	%r15, %rsi
	callq	modified_in_p
	testq	%rbx, %rax
	jne	.LBB9_52
# BB#48:                                # %lor.lhs.false209
                                        #   in Loop: Header=BB9_46 Depth=1
	movq	40(%r13), %rdi
	movq	%r15, %rsi
	callq	modified_in_p
	testq	%rbx, %rax
	jne	.LBB9_52
# BB#49:                                # %lor.lhs.false209.for.inc214_crit_edge
                                        #   in Loop: Header=BB9_46 Depth=1
	movq	48(%r13), %rax
.LBB9_50:                               # %for.inc214
                                        #   in Loop: Header=BB9_46 Depth=1
	movq	24(%r15), %r15
	cmpq	%rax, %r15
	jne	.LBB9_46
.LBB9_51:
	movq	40(%rsp), %r12          # 8-byte Reload
	jmp	.LBB9_52
.LBB9_17:                               # %if.else
	movq	32(%rbx), %rax
	movq	8(%rax), %rsi
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	rtx_equal_p
	testq	%r14, %rax
	movq	%r14, %rbx
	je	.LBB9_19
# BB#18:                                # %if.else
	movq	(%rsp), %rax            # 8-byte Reload
	movq	%rax, 32(%rsp)          # 8-byte Spill
.LBB9_19:                               # %if.else
	movq	16(%rsp), %rdi          # 8-byte Reload
	movzbl	(%r12), %eax
	cmpq	$54, %rax
	movq	8(%rsp), %r14           # 8-byte Reload
	jne	.LBB9_20
# BB#21:                                # %if.then100
	callq	swap_condition
	movq	%rax, %rdi
	movq	32(%rsp), %r9           # 8-byte Reload
	jmp	.LBB9_22
.LBB9_20:
	movq	%r12, %r9
	movq	32(%rsp), %r12          # 8-byte Reload
.LBB9_22:                               # %if.end104
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB9_23
.Ltmp143:
	.size	noce_get_alt_condition, .Ltmp143-noce_get_alt_condition
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI9_0:
	.quad	.LBB9_32
	.quad	.LBB9_30
	.quad	.LBB9_28
	.quad	.LBB9_26

	.type	num_possible_if_blocks,@object # @num_possible_if_blocks
	.local	num_possible_if_blocks
	.comm	num_possible_if_blocks,4,4
	.type	num_updated_if_blocks,@object # @num_updated_if_blocks
	.local	num_updated_if_blocks
	.comm	num_updated_if_blocks,4,4
	.type	num_removed_blocks,@object # @num_removed_blocks
	.local	num_removed_blocks
	.comm	num_removed_blocks,4,4
	.type	life_data_ok,@object    # @life_data_ok
	.local	life_data_ok
	.comm	life_data_ok,1,1
	.type	post_dominators,@object # @post_dominators
	.local	post_dominators
	.comm	post_dominators,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"\n%d possible IF blocks searched.\n"
	.size	.L.str, 34

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"%d IF blocks converted.\n"
	.size	.L.str1, 25

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"%d basic blocks deleted.\n\n\n"
	.size	.L.str2, 28

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"Conversion succeeded.\n"
	.size	.L.str3, 23

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"\nIF-CASE-2 found, start %d, else %d\n"
	.size	.L.str4, 37

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"\nIF-CASE-1 found, start %d, then %d\n"
	.size	.L.str5, 37

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"\nTRAP-IF block found, start %d, trap %d\n"
	.size	.L.str6, 41

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"ifcvt.c"
	.size	.L.str7, 8

	.type	.L__FUNCTION__.merge_if_block,@object # @__FUNCTION__.merge_if_block
.L__FUNCTION__.merge_if_block:
	.asciz	"merge_if_block"
	.size	.L__FUNCTION__.merge_if_block, 15

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"\nIF-THEN-ELSE block found, start %d, then %d, else %d, join %d\n"
	.size	.L.str8, 64

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"\nIF-THEN block found, start %d, then %d, join %d\n"
	.size	.L.str9, 50


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
