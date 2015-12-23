	.file	"integrate.c.bc"
	.text
	.globl	get_label_from_map
	.align	16, 0x90
	.type	get_label_from_map,@function
get_label_from_map:                     # @get_label_from_map
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
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$32, %rcx
	movq	40(%r14), %rax
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	(%rax,%rbx,8), %rax
	testq	%rax, %rax
	jne	.LBB0_2
# BB#1:                                 # %if.then
	callq	gen_label_rtx
	movq	40(%r14), %rcx
	movq	%rax, (%rcx,%rbx,8)
.LBB0_2:                                # %if.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp8:
	.size	get_label_from_map, .Ltmp8-get_label_from_map
	.cfi_endproc

	.globl	function_attribute_inlinable_p
	.align	16, 0x90
	.type	function_attribute_inlinable_p,@function
function_attribute_inlinable_p:         # @function_attribute_inlinable_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp15:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp16:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp17:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp18:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp19:
	.cfi_def_cfa_offset 48
.Ltmp20:
	.cfi_offset %rbx, -48
.Ltmp21:
	.cfi_offset %r12, -40
.Ltmp22:
	.cfi_offset %r13, -32
.Ltmp23:
	.cfi_offset %r14, -24
.Ltmp24:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	136(%r15), %r12
	testq	%r12, %r12
	movq	$1, %rax
	je	.LBB1_9
# BB#1:                                 # %for.body.lr.ph
	movq	targetm+256(%rip), %rax
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB1_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_5 Depth 2
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB1_7
# BB#3:                                 #   in Loop: Header=BB1_2 Depth=1
	movq	24(%r12), %rbx
	movq	$32, %r14
	.align	16, 0x90
.LBB1_5:                                # %for.body3
                                        #   Parent Loop BB1_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rsi
	callq	is_attribute_p
	testq	%r13, %rax
	jne	.LBB1_6
# BB#4:                                 # %for.cond1
                                        #   in Loop: Header=BB1_5 Depth=2
	movq	targetm+256(%rip), %rax
	movq	(%rax,%r14), %rdi
	addq	$32, %r14
	testq	%rdi, %rdi
	jne	.LBB1_5
.LBB1_7:                                # %for.inc11
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	(%r12), %r12
	testq	%r12, %r12
	jne	.LBB1_2
# BB#8:
	movq	$1, %rax
	jmp	.LBB1_9
.LBB1_6:                                # %if.then14
	movq	%r15, %rdi
	callq	*targetm+288(%rip)
.LBB1_9:                                # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp25:
	.size	function_attribute_inlinable_p, .Ltmp25-function_attribute_inlinable_p
	.cfi_endproc

	.globl	function_cannot_inline_p
	.align	16, 0x90
	.type	function_cannot_inline_p,@function
function_cannot_inline_p:               # @function_cannot_inline_p
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
	subq	$16, %rsp
.Ltmp37:
	.cfi_def_cfa_offset 64
.Ltmp38:
	.cfi_offset %rbx, -48
.Ltmp39:
	.cfi_offset %r12, -40
.Ltmp40:
	.cfi_offset %r13, -32
.Ltmp41:
	.cfi_offset %r14, -24
.Ltmp42:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	8(%r15), %rax
	movq	24(%rax), %rdi
	callq	tree_last
	movzbl	49(%r15), %ecx
	movq	%rax, %r14
	testq	$8, %rcx
	je	.LBB2_2
# BB#1:                                 # %cond.true
	movq	compiler_params(%rip), %rax
	movl	8(%rax), %ebx
	movq	88(%r15), %rdi
	callq	list_length
	leaq	(%rbx,%rax,8), %r12
	jmp	.LBB2_5
.LBB2_2:                                # %cond.false
	movl	optimize_size(%rip), %ebx
	movq	88(%r15), %rdi
	callq	list_length
	testq	%rbx, %rbx
	je	.LBB2_4
# BB#3:                                 # %cond.true5
	leaq	(%rax,%rax,2), %rax
	movabsq	$2147483648, %r12       # imm = 0x80000000
	andq	%rax, %r12
	movq	$31, %rcx
	shrq	%cl, %r12
	addq	%rax, %r12
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$33, %rcx
	sarq	%cl, %r12
	incq	%r12
	jmp	.LBB2_5
.LBB2_4:                                # %cond.false11
	leaq	64(,%rax,8), %r12
.LBB2_5:                                # %cond.end17
	movzbl	52(%r15), %eax
	testq	$16, %rax
	je	.LBB2_7
# BB#6:
	movabsq	$.L.str, %r14
	jmp	.LBB2_66
.LBB2_7:                                # %if.end
	testq	%r14, %r14
	je	.LBB2_10
# BB#8:                                 # %land.lhs.true
	movq	32(%r14), %rax
	movq	global_trees+216(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB2_10
# BB#9:
	movabsq	$.L.str1, %r14
	jmp	.LBB2_66
.LBB2_10:                               # %lor.lhs.false
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	testq	$65536, %rcx            # imm = 0x10000
	je	.LBB2_12
# BB#11:
	movabsq	$.L.str1, %r14
	jmp	.LBB2_66
.LBB2_12:                               # %if.end32
	testq	$64, %rcx
	je	.LBB2_14
# BB#13:
	movabsq	$.L.str2, %r14
	jmp	.LBB2_66
.LBB2_14:                               # %if.end38
	testq	$16, %rcx
	je	.LBB2_16
# BB#15:
	movabsq	$.L.str3, %r14
	jmp	.LBB2_66
.LBB2_16:                               # %if.end44
	testq	$128, %rcx
	je	.LBB2_18
# BB#17:
	movabsq	$.L.str4, %r14
	jmp	.LBB2_66
.LBB2_18:                               # %if.end50
	testq	$1024, %rcx             # imm = 0x400
	je	.LBB2_20
# BB#19:
	movabsq	$.L.str5, %r14
	jmp	.LBB2_66
.LBB2_20:                               # %if.end56
	movq	16(%rax), %rcx
	cmpq	$0, 32(%rcx)
	je	.LBB2_22
# BB#21:
	movabsq	$.L.str6, %r14
	jmp	.LBB2_66
.LBB2_22:                               # %if.end59
	movq	136(%rax), %r14
	testq	%r14, %r14
	jne	.LBB2_66
# BB#23:                                # %if.end63
	callq	get_max_uid
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%r12,%r12,2), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	movabsq	$.L.str7, %r14
	jg	.LBB2_66
# BB#24:                                # %if.end68
	movq	cfun(%rip), %rax
	movzbl	424(%rax), %eax
	movabsq	$.L.str8, %rbx
	testq	$2, %rax
	je	.LBB2_26
# BB#25:
	movq	%rbx, %r14
	jmp	.LBB2_66
.LBB2_26:                               # %if.end74
	movq	8(%r15), %rax
	movq	8(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$5, %rcx
	je	.LBB2_29
# BB#27:                                # %land.lhs.true83
	movq	%rax, %rdi
	callq	int_size_in_bytes
	testq	%rax, %rax
	js	.LBB2_28
.LBB2_29:                               # %if.end91
	movq	88(%r15), %r13
	jmp	.LBB2_30
.LBB2_28:
	movabsq	$.L.str9, %r14
	jmp	.LBB2_66
.LBB2_36:                               # %for.inc
                                        #   in Loop: Header=BB2_30 Depth=1
	movq	(%r13), %r13
.LBB2_30:                               # %if.end91
                                        # =>This Inner Loop Header: Depth=1
	testq	%r13, %r13
	je	.LBB2_37
# BB#31:                                # %for.body
                                        #   in Loop: Header=BB2_30 Depth=1
	movq	8(%r13), %rax
	movq	%rax, %rdi
	callq	int_size_in_bytes
	testq	%rax, %rax
	js	.LBB2_32
# BB#33:                                # %if.else
                                        #   in Loop: Header=BB2_30 Depth=1
	movq	8(%r13), %rax
	movzbl	16(%rax), %ecx
	cmpq	$21, %rcx
	jne	.LBB2_36
# BB#34:                                # %land.lhs.true107
                                        #   in Loop: Header=BB2_30 Depth=1
	movzbl	62(%rax), %eax
	testq	$8, %rax
	je	.LBB2_36
# BB#35:
	movabsq	$.L.str11, %rax
	movq	%rax, %r14
	jmp	.LBB2_66
.LBB2_37:                               # %for.end
	callq	get_max_uid
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jle	.LBB2_51
# BB#38:                                # %if.then121
	callq	get_first_nonparm_insn
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB2_39
# BB#40:                                # %if.then121
	movabsq	$1, %r8
	jmp	.LBB2_41
.LBB2_32:
	movabsq	$.L.str10, %rax
	movq	%rax, %r14
	jmp	.LBB2_66
.LBB2_39:
	movabsq	$0, %r8
.LBB2_41:                               # %if.then121
	testq	%rax, %rax
	je	.LBB2_50
# BB#42:                                # %if.then121
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB2_50
# BB#43:
	xorq	%rdx, %rdx
	movabsq	$0, %rsi
	movabsq	$1, %rdi
.LBB2_44:                               # %for.body126
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	movq	%rsi, %rcx
	jne	.LBB2_46
# BB#45:                                # %for.body126
                                        #   in Loop: Header=BB2_44 Depth=1
	movq	%rdi, %rcx
.LBB2_46:                               # %for.body126
                                        #   in Loop: Header=BB2_44 Depth=1
	addq	%rcx, %rdx
	movq	$32, %rcx
	movq	%rdx, %r8
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movq	$32, %rcx
	movq	%r12, %r9
	shlq	%cl, %r9
	movq	24(%rax), %rax
	movq	$32, %rcx
	sarq	%cl, %r9
	cmpq	%r9, %r8
	movq	%rsi, %r8
	jge	.LBB2_48
# BB#47:                                # %for.body126
                                        #   in Loop: Header=BB2_44 Depth=1
	movq	%rdi, %r8
.LBB2_48:                               # %for.body126
                                        #   in Loop: Header=BB2_44 Depth=1
	testq	%rax, %rax
	je	.LBB2_50
# BB#49:                                # %for.body126
                                        #   in Loop: Header=BB2_44 Depth=1
	movq	$32, %rcx
	movq	%r12, %r9
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	$32, %rcx
	movq	%rdx, %r10
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	cmpq	%r9, %r10
	jl	.LBB2_44
.LBB2_50:                               # %for.end136
	testq	%r8, %r8
	je	.LBB2_66
.LBB2_51:                               # %if.end141
	movq	cfun(%rip), %rax
	movl	424(%rax), %eax
	testq	$2048, %rax             # imm = 0x800
	je	.LBB2_53
# BB#52:
	movabsq	$.L.str12, %r14
	jmp	.LBB2_66
.LBB2_53:                               # %if.end147
	testq	$512, %rax              # imm = 0x200
	je	.LBB2_55
# BB#54:
	movabsq	$.L.str13, %r14
	jmp	.LBB2_66
.LBB2_55:                               # %if.end153
	movq	96(%r15), %rax
	movq	144(%rax), %rax
	testq	%rax, %rax
	je	.LBB2_57
# BB#56:                                # %cond.end177
	movzbl	(%rax), %eax
	cmpq	$39, %rax
	movq	%rbx, %r14
	je	.LBB2_66
.LBB2_57:                               # %if.end185
	movq	136(%r15), %rbx
	xorq	%r14, %r14
	testq	%rbx, %rbx
	je	.LBB2_66
# BB#58:                                # %for.body.lr.ph.i
	movq	targetm+256(%rip), %rax
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
.LBB2_59:                               # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_62 Depth 2
	movq	(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB2_65
# BB#60:                                #   in Loop: Header=BB2_59 Depth=1
	movq	24(%rbx), %r13
	movq	$32, 8(%rsp)            # 8-byte Folded Spill
.LBB2_62:                               # %for.body3.i
                                        #   Parent Loop BB2_59 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, %rdi
	movq	%r13, %rsi
	callq	is_attribute_p
	testq	%r12, %rax
	jne	.LBB2_63
# BB#61:                                # %for.cond1.i
                                        #   in Loop: Header=BB2_62 Depth=2
	movq	targetm+256(%rip), %rax
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	(%rax,%rdx), %rcx
	addq	$32, %rdx
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	testq	%rcx, %rcx
	jne	.LBB2_62
.LBB2_65:                               # %for.inc11.i
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB2_59
	jmp	.LBB2_66
.LBB2_63:                               # %if.then14.i
	movq	%r15, %rdi
	callq	*targetm+288(%rip)
	xorq	%r14, %r14
	testq	%rax, %rax
	jne	.LBB2_66
# BB#64:                                # %if.then14.i
	movabsq	$.L.str14, %r14
.LBB2_66:                               # %return
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp43:
	.size	function_cannot_inline_p, .Ltmp43-function_cannot_inline_p
	.cfi_endproc

	.globl	copy_decl_for_inlining
	.align	16, 0x90
	.type	copy_decl_for_inlining,@function
copy_decl_for_inlining:                 # @copy_decl_for_inlining
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp49:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp50:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp51:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp52:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp53:
	.cfi_def_cfa_offset 48
.Ltmp54:
	.cfi_offset %rbx, -40
.Ltmp55:
	.cfi_offset %r12, -32
.Ltmp56:
	.cfi_offset %r14, -24
.Ltmp57:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movzbl	16(%r12), %eax
	movq	%rdx, %r14
	movq	%rsi, %r15
	addq	$-35, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$1, %rcx
	ja	.LBB3_2
# BB#1:                                 # %if.then
	movq	8(%r12), %rdx
	movq	72(%r12), %rsi
	movq	$34, %rdi
	callq	build_decl
	movq	%rax, %rbx
	movl	16(%r12), %ecx
	movl	16(%rbx), %eax
	andq	$1024, %rcx             # imm = 0x400
	andq	$-1025, %rax            # imm = 0xFFFFFFFFFFFFFBFF
	orq	%rcx, %rax
	movl	%eax, 16(%rbx)
	movl	16(%r12), %ecx
	andq	$4096, %rcx             # imm = 0x1000
	andq	$-4097, %rax            # imm = 0xFFFFFFFFFFFFEFFF
	orq	%rcx, %rax
	movl	%eax, 16(%rbx)
	movl	16(%r12), %ecx
	andq	$2048, %rcx             # imm = 0x800
	andq	$-2049, %rax            # imm = 0xFFFFFFFFFFFFF7FF
	orq	%rcx, %rax
	jmp	.LBB3_6
.LBB3_2:                                # %if.else
	movq	%r12, %rdi
	callq	copy_node
	movq	%rax, %rbx
	cmpq	$0, 200(%rbx)
	je	.LBB3_4
# BB#3:                                 # %if.then39
	movq	%rbx, %rdi
	callq	copy_lang_decl
.LBB3_4:                                # %if.end
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$31, %rcx
	jne	.LBB3_7
# BB#5:                                 # %if.then45
	movabsq	$4294966271, %rcx       # imm = 0xFFFFFBFF
	andq	%rcx, %rax
.LBB3_6:                                # %if.end51
	movl	%eax, 16(%rbx)
.LBB3_7:                                # %if.end51
	movq	112(%r12), %rcx
	testq	%rcx, %rcx
	movq	%r12, %rdx
	je	.LBB3_9
# BB#8:                                 # %if.end51
	movq	%rcx, %rdx
.LBB3_9:                                # %if.end51
	movq	%rdx, 112(%rbx)
	movq	$0, 144(%rbx)
	orq	$65536, %rax            # imm = 0x10000
	movl	%eax, 16(%rbx)
	movq	80(%r12), %rax
	testq	%rax, %rax
	je	.LBB3_13
# BB#10:                                # %if.end51
	cmpq	%r15, %rax
	jne	.LBB3_13
# BB#11:                                # %if.else71
	movzbl	18(%r12), %eax
	testq	$4, %rax
	jne	.LBB3_13
# BB#12:                                # %if.else78
	movq	%r14, 80(%rbx)
.LBB3_13:                               # %if.end83
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp58:
	.size	copy_decl_for_inlining, .Ltmp58-copy_decl_for_inlining
	.cfi_endproc

	.globl	save_for_inline
	.align	16, 0x90
	.type	save_for_inline,@function
save_for_inline:                        # @save_for_inline
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
	subq	$16, %rsp
.Ltmp70:
	.cfi_def_cfa_offset 64
.Ltmp71:
	.cfi_offset %rbx, -48
.Ltmp72:
	.cfi_offset %r12, -40
.Ltmp73:
	.cfi_offset %r13, -32
.Ltmp74:
	.cfi_offset %r14, -24
.Ltmp75:
	.cfi_offset %r15, -16
	movl	flag_no_inline(%rip), %eax
	movq	%rdi, %r15
	testq	%rax, %rax
	jne	.LBB4_2
# BB#1:                                 # %if.then
	movq	cfun(%rip), %rax
	movl	304(%rax), %edi
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, parmdecl_map(%rip)
.LBB4_2:                                # %if.end
	movq	cfun(%rip), %rax
	cmpq	$0, 200(%rax)
	jne	.LBB4_4
# BB#3:                                 # %if.then2
	callq	gen_label_rtx
	movq	cfun(%rip), %rcx
	movq	%rax, 200(%rcx)
	movq	cfun(%rip), %rax
	movq	200(%rax), %rdi
	callq	emit_label
.LBB4_4:                                # %if.end7
	movl	flag_no_inline(%rip), %eax
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	testq	%rax, %rax
	jne	.LBB4_23
# BB#5:                                 # %if.then9
	movq	cfun(%rip), %rax
	movl	304(%rax), %edx
	movq	parmdecl_map(%rip), %rdi
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movq	88(%r15), %rdi
	callq	list_length
	movq	%rax, %rdi
	callq	rtvec_alloc
	movq	88(%r15), %r12
	movq	%rax, 8(%rsp)           # 8-byte Spill
	testq	%r12, %r12
	je	.LBB4_23
# BB#6:                                 # %for.body.i.preheader
	movq	8(%rsp), %rax           # 8-byte Reload
	leaq	8(%rax), %r14
	.align	16, 0x90
.LBB4_7:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	144(%r12), %rbx
	testq	%rbx, %rbx
	jne	.LBB4_9
# BB#8:                                 # %cond.false.i
                                        #   in Loop: Header=BB4_7 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rbx
.LBB4_9:                                # %cond.end.i
                                        #   in Loop: Header=BB4_7 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB4_14
# BB#10:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB4_7 Depth=1
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$70, %rcx
	jne	.LBB4_14
# BB#11:                                # %land.lhs.true15.i
                                        #   in Loop: Header=BB4_7 Depth=1
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	je	.LBB4_13
# BB#12:                                # %land.lhs.true15.i
                                        #   in Loop: Header=BB4_7 Depth=1
	movq	%rbx, %rax
.LBB4_13:                               # %land.lhs.true15.i
                                        #   in Loop: Header=BB4_7 Depth=1
	movq	%rax, %rbx
.LBB4_14:                               # %if.end.i
                                        #   in Loop: Header=BB4_7 Depth=1
	movq	%rbx, (%r14)
	movzbl	(%rbx), %eax
	cmpq	$65, %rax
	jne	.LBB4_15
# BB#17:                                # %if.then46.i
                                        #   in Loop: Header=BB4_7 Depth=1
	movq	8(%rbx), %rax
	movzbl	2(%rax), %edi
	movq	%rbx, %rsi
	callq	gen_realpart
	movq	%rax, %r13
	movzbl	2(%r13), %edi
	movq	%rbx, %rsi
	callq	gen_imagpart
	movzbl	(%r13), %ecx
	cmpq	$61, %rcx
	jne	.LBB4_19
# BB#18:                                # %if.then61.i
                                        #   in Loop: Header=BB4_7 Depth=1
	movl	8(%r13), %ecx
	movq	parmdecl_map(%rip), %rdx
	movq	%r12, (%rdx,%rcx,8)
.LBB4_19:                               # %if.end67.i
                                        #   in Loop: Header=BB4_7 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB4_22
# BB#20:                                # %if.then72.i
                                        #   in Loop: Header=BB4_7 Depth=1
	movl	8(%rax), %eax
	jmp	.LBB4_21
	.align	16, 0x90
.LBB4_15:                               # %if.end.i
                                        #   in Loop: Header=BB4_7 Depth=1
	cmpq	$61, %rax
	jne	.LBB4_22
# BB#16:                                # %if.then37.i
                                        #   in Loop: Header=BB4_7 Depth=1
	movl	8(%rbx), %eax
.LBB4_21:                               # %if.end80.i
                                        #   in Loop: Header=BB4_7 Depth=1
	movq	parmdecl_map(%rip), %rcx
	movq	%r12, (%rcx,%rax,8)
.LBB4_22:                               # %if.end80.i
                                        #   in Loop: Header=BB4_7 Depth=1
	movl	16(%r12), %eax
	orq	$4096, %rax             # imm = 0x1000
	movl	%eax, 16(%r12)
	movq	(%r12), %r12
	addq	$8, %r14
	testq	%r12, %r12
	jne	.LBB4_7
.LBB4_23:                               # %if.end11
	callq	get_insns
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB4_29
# BB#24:
	movabsq	$4294967216, %r14       # imm = 0xFFFFFFB0
	.align	16, 0x90
.LBB4_25:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB4_28
# BB#26:                                # %land.lhs.true
                                        #   in Loop: Header=BB4_25 Depth=1
	movl	40(%rbx), %eax
	cmpq	%r14, %rax
	jne	.LBB4_28
# BB#27:                                # %if.then18
                                        #   in Loop: Header=BB4_25 Depth=1
	movq	%rbx, %rdi
	callq	delete_related_insns
.LBB4_28:                               # %for.inc
                                        #   in Loop: Header=BB4_25 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB4_25
.LBB4_29:                               # %for.end
	callq	get_insns
	movq	%rax, %rbx
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB4_37
# BB#30:                                # %if.end29
	movl	flag_no_inline(%rip), %eax
	testq	%rax, %rax
	jne	.LBB4_32
# BB#31:                                # %if.then31
	callq	get_first_nonparm_insn
	xorq	%rcx, %rcx
	movb	%cl, in_nonparm_insns(%rip)
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	save_parm_insns
	callq	max_label_num
	movq	cfun(%rip), %rcx
	movl	%eax, 384(%rcx)
	movq	296(%rcx), %rax
	movq	%rax, 376(%rcx)
	movq	cfun(%rip), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 360(%rax)
.LBB4_32:                               # %if.end34
	movq	104(%r15), %rax
	movq	cfun(%rip), %rcx
	movq	%rax, 368(%rcx)
	movl	write_symbols(%rip), %eax
	testq	%rax, %rax
	jne	.LBB4_33
# BB#34:                                # %if.end34
	movabsq	$1, %rax
	jmp	.LBB4_35
.LBB4_33:
	movabsq	$0, %rax
.LBB4_35:                               # %if.end34
	movq	cfun(%rip), %rcx
	movl	%eax, 356(%rcx)
	movq	%rcx, 160(%r15)
	movl	flag_no_inline(%rip), %eax
	testq	%rax, %rax
	je	.LBB4_38
# BB#36:                                # %if.end40
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB4_38:                               # %if.then39
	movq	parmdecl_map(%rip), %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.LBB4_37:                               # %if.then28
	movabsq	$.L.str15, %rax
	movabsq	$.L__FUNCTION__.save_for_inline, %rcx
	movq	$472, %rsi              # imm = 0x1D8
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp76:
	.size	save_for_inline, .Ltmp76-save_for_inline
	.cfi_endproc

	.align	16, 0x90
	.type	save_parm_insns,@function
save_parm_insns:                        # @save_parm_insns
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp82:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp83:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp84:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp85:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp86:
	.cfi_def_cfa_offset 48
.Ltmp87:
	.cfi_offset %rbx, -40
.Ltmp88:
	.cfi_offset %r12, -32
.Ltmp89:
	.cfi_offset %r14, -24
.Ltmp90:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	testq	%rdi, %rdi
	je	.LBB5_7
# BB#1:                                 # %for.cond.preheader
	movq	24(%rdi), %rbx
	testq	%rbx, %rbx
	je	.LBB5_7
# BB#2:
	movq	$1, %r12
	movabsq	$note_modified_parmregs, %r14
	.align	16, 0x90
.LBB5_3:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r15, %rbx
	jne	.LBB5_5
# BB#4:                                 # %if.then2
                                        #   in Loop: Header=BB5_3 Depth=1
	movb	%r12b, in_nonparm_insns(%rip)
.LBB5_5:                                # %if.end3
                                        #   in Loop: Header=BB5_3 Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB5_6
# BB#8:                                 # %if.then7
                                        #   in Loop: Header=BB5_3 Depth=1
	movq	32(%rbx), %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	note_stores
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB5_6
# BB#9:                                 # %land.lhs.true
                                        #   in Loop: Header=BB5_3 Depth=1
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$141, %rcx
	jne	.LBB5_6
# BB#10:                                # %for.cond23.preheader
                                        #   in Loop: Header=BB5_3 Depth=1
	movq	8(%rax), %rdi
	movq	%r15, %rsi
	callq	save_parm_insns
	movq	32(%rbx), %rax
	movq	16(%rax), %rdi
	movq	%r15, %rsi
	callq	save_parm_insns
	movq	32(%rbx), %rax
	movq	24(%rax), %rdi
	movq	%r15, %rsi
	callq	save_parm_insns
	.align	16, 0x90
.LBB5_6:                                # %for.cond.backedge
                                        #   in Loop: Header=BB5_3 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB5_3
.LBB5_7:                                # %for.end40
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp91:
	.size	save_parm_insns, .Ltmp91-save_parm_insns
	.cfi_endproc

	.globl	expand_inline_function
	.align	16, 0x90
	.type	expand_inline_function,@function
expand_inline_function:                 # @expand_inline_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp98:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp99:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp100:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp101:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp102:
	.cfi_def_cfa_offset 48
	subq	$208, %rsp
.Ltmp103:
	.cfi_def_cfa_offset 256
.Ltmp104:
	.cfi_offset %rbx, -48
.Ltmp105:
	.cfi_offset %r12, -40
.Ltmp106:
	.cfi_offset %r13, -32
.Ltmp107:
	.cfi_offset %r14, -24
.Ltmp108:
	.cfi_offset %r15, -16
	movq	160(%rdi), %rbx
	movq	24(%rbx), %rax
	movq	376(%rbx), %rdi
	movq	8(%rax), %r10
	movq	%r8, 64(%rsp)           # 8-byte Spill
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	movq	%rsi, %r15
	testq	%rdi, %rdi
	movq	%r10, %rdx
	je	.LBB6_2
# BB#1:                                 # %cond.true
	movq	24(%rdi), %rdx
.LBB6_2:                                # %cond.end
	movq	%r9, 56(%rsp)           # 8-byte Spill
	movslq	4(%rax), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movslq	384(%rbx), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	$0, 192(%rsp)
	movq	24(%rbx), %rax
	movl	(%rax), %esi
	leaq	3(%rsi), %r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	jle	.LBB6_3
# BB#5:                                 # %if.end
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%r10, 40(%rsp)          # 8-byte Spill
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	48(%rbx), %rax
	movq	%rax, 128(%rsp)         # 8-byte Spill
	movq	360(%rbx), %rcx
	movq	%rcx, 120(%rsp)         # 8-byte Spill
	movq	88(%rax), %rdi
	callq	list_length
	movq	cfun(%rip), %rcx
	movslq	404(%rcx), %rsi
	movslq	404(%rbx), %rdx
	movq	%rbx, 80(%rsp)          # 8-byte Spill
	movq	%rax, 144(%rsp)         # 8-byte Spill
	cmpq	%rdx, %rsi
	jge	.LBB6_7
# BB#6:                                 # %if.then8
	movl	%edx, 404(%rcx)
.LBB6_7:                                # %for.cond.preheader
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	88(%rax), %rax
	testq	%rax, %rax
	movq	%r15, %r14
	movq	%r14, %rbx
	je	.LBB6_16
# BB#8:                                 # %for.body.lr.ph
	movq	global_trees(%rip), %r9
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%r14, %rbx
	movq	$-1, %r13
	.align	16, 0x90
.LBB6_9:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB6_210
# BB#10:                                # %if.end17
                                        #   in Loop: Header=BB6_9 Depth=1
	movq	104(%rax), %rcx
	movl	60(%rcx), %edi
	movq	32(%rbx), %rsi
	movq	$9, %rcx
	shrq	%cl, %rdi
	cmpq	%r9, %rsi
	je	.LBB6_210
# BB#11:                                # %lor.lhs.false
                                        #   in Loop: Header=BB6_9 Depth=1
	movq	8(%rsi), %rsi
	movl	60(%rsi), %edx
	andq	$127, %rdi
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	andq	%r8, %rdi
	cmpq	%rdx, %rdi
	jne	.LBB6_210
# BB#12:                                # %lor.lhs.false29
                                        #   in Loop: Header=BB6_9 Depth=1
	cmpq	$51, %rdi
	jne	.LBB6_14
# BB#13:                                # %land.lhs.true
                                        #   in Loop: Header=BB6_9 Depth=1
	movq	8(%rax), %rcx
	movq	128(%rsi), %rdx
	movq	128(%rcx), %rcx
	cmpq	%rcx, %rdx
	jne	.LBB6_210
.LBB6_14:                               # %for.inc
                                        #   in Loop: Header=BB6_9 Depth=1
	movq	(%rax), %rax
	movq	(%rbx), %rbx
	testq	%rax, %rax
	jne	.LBB6_9
	jmp	.LBB6_16
	.align	16, 0x90
.LBB6_15:                               # %for.body46
                                        #   in Loop: Header=BB6_16 Depth=1
	movq	32(%rbx), %rdi
	movq	8(%rdi), %rax
	movl	60(%rax), %edx
	movq	const_int_rtx+512(%rip), %rsi
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	(%rbx), %rbx
.LBB6_16:                               # %for.body46
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB6_15
# BB#17:                                # %for.end62
	movq	$32, %rcx
	movq	144(%rsp), %rbx         # 8-byte Reload
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	%rbx, %rdi
	callq	xmalloc
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	callq	xmalloc
	movq	128(%rsp), %rcx         # 8-byte Reload
	movq	88(%rcx), %r15
	movq	%rax, 48(%rsp)          # 8-byte Spill
	testq	%r15, %r15
	je	.LBB6_45
# BB#18:                                # %for.body71.lr.ph
	movq	120(%rsp), %rax         # 8-byte Reload
	leaq	8(%rax), %rbx
	movq	48(%rsp), %r12          # 8-byte Reload
	movq	96(%rsp), %r13          # 8-byte Reload
	.align	16, 0x90
.LBB6_19:                               # %for.body71
                                        # =>This Inner Loop Header: Depth=1
	movq	%r13, 168(%rsp)         # 8-byte Spill
	movq	8(%r15), %rdi
	movq	32(%r14), %rsi
	callq	convert
	movq	8(%r15), %rdi
	movl	60(%rdi), %r13d
	movq	%rax, (%r12)
	movq	(%rbx), %r8
	movl	(%r8), %r9d
	movq	$9, %rcx
	shrq	%cl, %r13
	movq	%r9, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB6_24
# BB#20:                                # %land.lhs.true93
                                        #   in Loop: Header=BB6_19 Depth=1
	movq	8(%r8), %rdx
	movzbl	(%rdx), %esi
	cmpq	$61, %rsi
	jne	.LBB6_29
# BB#21:                                # %land.lhs.true101
                                        #   in Loop: Header=BB6_19 Depth=1
	movl	8(%rdx), %edx
	cmpq	$58, %rdx
	jb	.LBB6_23
# BB#22:                                # %if.then109
                                        #   in Loop: Header=BB6_19 Depth=1
	movq	%r8, 152(%rsp)          # 8-byte Spill
	movq	%r12, 160(%rsp)         # 8-byte Spill
	movq	8(%rax), %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	$1, 136(%rsp)           # 8-byte Folded Spill
	movq	%rax, %r13
	callq	assign_temp
	movq	%rax, %r12
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	store_expr
	movq	8(%r12), %r12
	jmp	.LBB6_28
.LBB6_23:                               # %if.else
                                        #   in Loop: Header=BB6_19 Depth=1
	cmpq	$66, %rcx
	jne	.LBB6_24
	.align	16, 0x90
.LBB6_29:                               # %if.end180.thread
                                        #   in Loop: Header=BB6_19 Depth=1
	movq	%r12, 160(%rsp)         # 8-byte Spill
	movq	168(%rsp), %r13         # 8-byte Reload
	movq	$0, (%r13)
	jmp	.LBB6_44
	.align	16, 0x90
.LBB6_24:                               # %if.then123
                                        #   in Loop: Header=BB6_19 Depth=1
	movq	%r8, 152(%rsp)          # 8-byte Spill
	movq	%r12, 160(%rsp)         # 8-byte Spill
	movq	8(%rax), %rdx
	andq	$127, %r13
	movq	$16, %rcx
	movl	60(%rdx), %edx
	shrq	%cl, %r9
	andq	$255, %r9
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	cmpq	%rdx, %r9
	jne	.LBB6_25
# BB#27:                                # %if.else171
                                        #   in Loop: Header=BB6_19 Depth=1
	xorq	%rsi, %rsi
	movq	$1, %rcx
	movq	$0, 136(%rsp)           # 8-byte Folded Spill
	movq	%rax, %rdi
	movq	%r13, %rdx
	callq	expand_expr
	movq	%rax, %r12
.LBB6_28:                               # %if.end180
                                        #   in Loop: Header=BB6_19 Depth=1
	movq	168(%rsp), %r13         # 8-byte Reload
	movq	%r12, (%r13)
	jmp	.LBB6_31
	.align	16, 0x90
.LBB6_25:                               # %if.then136
                                        #   in Loop: Header=BB6_19 Depth=1
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	%r14, 112(%rsp)         # 8-byte Spill
	movl	16(%rdi), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, 180(%rsp)
	movl	60(%rdi), %esi
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	xorq	%rcx, %rcx
	leaq	180(%rsp), %rdx
	callq	promote_mode
	movq	152(%rsp), %rcx         # 8-byte Reload
	movzbl	2(%rcx), %ecx
	movq	%rax, %r12
                                        # kill: RAX<def> R12<kill>
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	%rax, %rcx
	jne	.LBB6_26
# BB#30:                                # %if.end159
                                        #   in Loop: Header=BB6_19 Depth=1
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	8(%rdi), %rax
	movl	60(%rax), %r14d
	movq	$9, %rcx
	shrq	%cl, %r14
	andq	$127, %r14
	xorq	%rsi, %rsi
	movq	$1, %rcx
	movq	$0, 136(%rsp)           # 8-byte Folded Spill
	movq	%r13, %rdx
	callq	expand_expr
	movl	180(%rsp), %ecx
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	convert_modes
	movq	%rax, %r12
	movq	168(%rsp), %r13         # 8-byte Reload
	movq	%r12, (%r13)
	movq	112(%rsp), %r14         # 8-byte Reload
.LBB6_31:                               # %if.end180
                                        #   in Loop: Header=BB6_19 Depth=1
	testq	%r12, %r12
	movq	152(%rsp), %rdx         # 8-byte Reload
	je	.LBB6_44
# BB#32:                                # %land.lhs.true185
                                        #   in Loop: Header=BB6_19 Depth=1
	movzbl	17(%r15), %eax
	testq	$16, %rax
	je	.LBB6_39
# BB#33:                                # %lor.lhs.false191
                                        #   in Loop: Header=BB6_19 Depth=1
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB6_38
# BB#34:                                # %lor.lhs.false191
                                        #   in Loop: Header=BB6_19 Depth=1
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	jne	.LBB6_38
# BB#35:                                # %land.lhs.true196
                                        #   in Loop: Header=BB6_19 Depth=1
	movzbl	(%r12), %ecx
	addq	$-61, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$5, %rcx
	ja	.LBB6_38
# BB#36:                                # %land.lhs.true196
                                        #   in Loop: Header=BB6_19 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$37, %rax
	je	.LBB6_38
# BB#37:                                # %land.lhs.true217
                                        #   in Loop: Header=BB6_19 Depth=1
	movq	%r12, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	reg_overlap_mentioned_p
	movq	152(%rsp), %rdx         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB6_39
.LBB6_38:                               # %lor.lhs.false222
                                        #   in Loop: Header=BB6_19 Depth=1
	movzbl	(%r12), %eax
	cmpq	$63, %rax
	jne	.LBB6_40
.LBB6_39:                               # %if.then229
                                        #   in Loop: Header=BB6_19 Depth=1
	movzbl	2(%rdx), %edi
	movq	%r12, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %r12
	movq	%r12, (%r13)
.LBB6_40:                               # %if.end238
                                        #   in Loop: Header=BB6_19 Depth=1
	testq	%r12, %r12
	je	.LBB6_44
# BB#41:                                # %land.lhs.true243
                                        #   in Loop: Header=BB6_19 Depth=1
	movzbl	(%r12), %eax
	cmpq	$61, %rax
	jne	.LBB6_44
# BB#42:                                # %land.lhs.true250
                                        #   in Loop: Header=BB6_19 Depth=1
	movq	8(%r15), %rax
	movl	16(%rax), %ecx
	orq	$2, %rcx
	andq	$255, %rcx
	cmpq	$15, %rcx
	jne	.LBB6_44
# BB#43:                                # %if.then267
                                        #   in Loop: Header=BB6_19 Depth=1
	movq	8(%rax), %rax
	movl	64(%rax), %esi
	movq	%r12, %rdi
	callq	mark_reg_pointer
	.align	16, 0x90
.LBB6_44:                               # %for.inc276
                                        #   in Loop: Header=BB6_19 Depth=1
	movq	(%r14), %r14
	movq	(%r15), %r15
	addq	$8, %r13
	addq	$8, %rbx
	movq	160(%rsp), %r12         # 8-byte Reload
	addq	$8, %r12
	testq	%r15, %r15
	jne	.LBB6_19
.LBB6_45:                               # %for.end281
	movq	$1, %rdi
	movq	$648, %rsi              # imm = 0x288
	movq	$1, %r14
	callq	xcalloc
	movq	%rax, %rbx
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rbx)
	movabsq	$.L.str16, %rdx
	movq	$10, %rdi
	movq	$8, %rsi
	callq	varray_init
	movq	%rax, 24(%rbx)
	movq	$32, %rcx
	movq	24(%rsp), %rdi          # 8-byte Reload
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$8, %rsi
	callq	xcalloc
	movq	%rax, 32(%rbx)
	movq	$3, %rcx
	movq	72(%rsp), %rdi          # 8-byte Reload
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	%rax, 40(%rbx)
	movq	$0, 112(%rbx)
	movq	80(%rsp), %r12          # 8-byte Reload
	movq	24(%r12), %rax
	movl	40(%rax), %r13d
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$8, %rsi
	callq	xcalloc
	movq	%rax, 48(%rbx)
	xorq	%r15, %r15
	movl	%r15d, 56(%rbx)
	movl	%r13d, 60(%rbx)
	movl	%r14d, (%rbx)
	movq	$0, 632(%rbx)
	movl	%r15d, 640(%rbx)
	callq	max_reg_num
	movq	144(%rsp), %rcx         # 8-byte Reload
	leaq	(%rcx,%rcx,4), %rcx
	leaq	(%rcx,%rcx,2), %rcx
	movq	32(%rsp), %rdx          # 8-byte Reload
	addq	%rcx, %rdx
	leaq	-40(%rax,%rdx), %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movabsq	$.L__FUNCTION__.expand_inline_function, %rdx
	movq	$16, %rsi
	callq	varray_init
	movq	%rax, 64(%rbx)
	movl	%r15d, 72(%rbx)
	movq	%rbx, %r14
	callq	get_last_insn
	testq	%rax, %rax
	movq	%rax, 16(%r14)
	jne	.LBB6_47
# BB#46:                                # %if.then305
	movabsq	$4294967197, %rsi       # imm = 0xFFFFFF9D
	xorq	%rdi, %rdi
	callq	emit_note
	movq	%rax, 16(%r14)
.LBB6_47:                               # %if.end308
	movq	24(%r12), %rax
	movq	64(%rax), %rcx
	movq	%rcx, 120(%r14)
	movq	80(%rax), %rcx
	movq	cfun(%rip), %rax
	movq	%rcx, 128(%r14)
	movslq	76(%r12), %rcx
	movslq	76(%rax), %rdx
	cmpq	%rdx, %rcx
	jle	.LBB6_49
# BB#48:                                # %if.then316
	movl	%ecx, 76(%rax)
.LBB6_49:                               # %if.end319
	movl	424(%r12), %ecx
	testq	$2097152, %rcx          # imm = 0x200000
	movq	%r12, %rdx
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	120(%rsp), %r13         # 8-byte Reload
	je	.LBB6_51
# BB#50:                                # %if.then324
	movl	424(%rax), %ecx
	orq	$2097152, %rcx          # imm = 0x200000
	movl	%ecx, 424(%rax)
	movl	424(%rdx), %ecx
.LBB6_51:                               # %if.end328
	xorq	%r15, %r15
	testq	$8, %rcx
	movq	40(%rsp), %r12          # 8-byte Reload
	je	.LBB6_53
# BB#52:                                # %if.then333
	movq	128(%rsp), %rdi         # 8-byte Reload
	callq	lookup_static_chain
	movq	%rax, %r15
.LBB6_53:                               # %if.end335
	movzbl	(%r12), %eax
	cmpq	$37, %rax
	jne	.LBB6_57
# BB#54:                                # %land.lhs.true340
	movslq	40(%r12), %rsi
	testq	%rsi, %rsi
	jle	.LBB6_57
# BB#55:                                # %if.then345
	movq	32(%r12), %rdi
	callq	emit_note
	testq	%rax, %rax
	je	.LBB6_57
# BB#56:                                # %if.then353
	movl	(%rax), %ecx
	orq	$1073741824, %rcx       # imm = 0x40000000
	movl	%ecx, (%rax)
.LBB6_57:                               # %for.cond359.preheader
	movq	%r15, 152(%rsp)         # 8-byte Spill
	movq	%r14, 168(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	144(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB6_92
# BB#58:                                # %for.body362.lr.ph
	movq	$1, %r15
	jmp	.LBB6_59
.LBB6_84:                               # %if.else517
                                        #   in Loop: Header=BB6_59 Depth=1
	cmpq	$66, %rax
	jne	.LBB6_88
.LBB6_85:                               # %if.then522
                                        #   in Loop: Header=BB6_59 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	160(%rax), %rax
	movl	68(%rax), %eax
	testq	%rax, %rax
	je	.LBB6_91
# BB#86:                                # %if.then528
                                        #   in Loop: Header=BB6_59 Depth=1
	movq	global_rtl+48(%rip), %rdi
	xorq	%rdx, %rdx
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	copy_rtx_and_substitute
	jmp	.LBB6_91
.LBB6_72:                               # %if.then394
                                        #   in Loop: Header=BB6_59 Depth=1
	cmpq	$134, %rcx
	je	.LBB6_74
# BB#73:                                # %if.then394
                                        #   in Loop: Header=BB6_59 Depth=1
	cmpq	$140, %rcx
	jne	.LBB6_79
.LBB6_74:                               # %if.then474
                                        #   in Loop: Header=BB6_59 Depth=1
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	64(%rcx), %r13
	movl	8(%rax), %esi
	movq	(%r13), %rax
	cmpq	%rax, %rsi
	jb	.LBB6_78
# BB#75:                                # %if.then482
                                        #   in Loop: Header=BB6_59 Depth=1
	movq	global_const_equiv_varray(%rip), %rax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	incq	%rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	movq	%r13, %rdi
	movq	%rcx, %rbx
	callq	varray_grow
	cmpq	%r13, 160(%rsp)         # 8-byte Folded Reload
	movq	%rax, 64(%rbx)
	movq	%rax, %r13
	jne	.LBB6_77
# BB#76:                                # %if.then495
                                        #   in Loop: Header=BB6_59 Depth=1
	movq	%r13, global_const_equiv_varray(%rip)
.LBB6_77:                               # %if.end498
                                        #   in Loop: Header=BB6_59 Depth=1
	movq	96(%rsp), %rbx          # 8-byte Reload
.LBB6_78:                               # %if.end498
                                        #   in Loop: Header=BB6_59 Depth=1
	movq	184(%rsp), %rax
	movl	8(%rax), %eax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	%r12, 32(%r13,%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, 40(%r13,%rax)
	movq	184(%rsp), %rax
.LBB6_79:                               # %if.end506
                                        #   in Loop: Header=BB6_59 Depth=1
	movq	120(%rsp), %r13         # 8-byte Reload
.LBB6_80:                               # %if.end506
                                        #   in Loop: Header=BB6_59 Depth=1
	movq	8(%r14), %rcx
	movl	8(%rcx), %ecx
	movq	%rax, %r12
.LBB6_81:                               # %if.end507
                                        #   in Loop: Header=BB6_59 Depth=1
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	movq	%r12, (%rax,%rcx,8)
	jmp	.LBB6_91
	.align	16, 0x90
.LBB6_59:                               # %for.body362
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r13,%r15,8), %r14
	movzbl	(%r14), %eax
	movq	-8(%rbx,%r15,8), %r12
	cmpq	$61, %rax
	je	.LBB6_89
# BB#60:                                # %for.body362
                                        #   in Loop: Header=BB6_59 Depth=1
	cmpq	$65, %rax
	je	.LBB6_87
# BB#61:                                # %for.body362
                                        #   in Loop: Header=BB6_59 Depth=1
	cmpq	$66, %rax
	jne	.LBB6_88
# BB#62:                                # %land.lhs.true372
                                        #   in Loop: Header=BB6_59 Depth=1
	movq	8(%r14), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB6_85
# BB#63:                                # %land.lhs.true380
                                        #   in Loop: Header=BB6_59 Depth=1
	movl	8(%rcx), %ecx
	cmpq	$58, %rcx
	jb	.LBB6_82
# BB#64:                                # %if.then389
                                        #   in Loop: Header=BB6_59 Depth=1
	movzbl	(%r12), %eax
	cmpq	$61, %rax
	je	.LBB6_81
# BB#65:                                # %if.then394
                                        #   in Loop: Header=BB6_59 Depth=1
	movq	%r12, %rdi
	callq	copy_addr_to_reg
	movq	%rax, 184(%rsp)
	movzbl	(%r12), %ecx
	cmpq	$133, %rcx
	jg	.LBB6_72
# BB#66:                                # %if.then394
                                        #   in Loop: Header=BB6_59 Depth=1
	addq	$-54, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$21, %rcx
	ja	.LBB6_79
# BB#67:                                # %if.then394
                                        #   in Loop: Header=BB6_59 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB6_74
# BB#68:                                # %if.then394
                                        #   in Loop: Header=BB6_59 Depth=1
	cmpq	$21, %rcx
	jne	.LBB6_79
# BB#69:                                # %land.lhs.true440
                                        #   in Loop: Header=BB6_59 Depth=1
	movq	16(%r12), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$54, %rcx
	movq	120(%rsp), %r13         # 8-byte Reload
	jne	.LBB6_80
# BB#70:                                # %land.lhs.true448
                                        #   in Loop: Header=BB6_59 Depth=1
	movq	8(%r12), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB6_80
# BB#71:                                # %land.lhs.true456
                                        #   in Loop: Header=BB6_59 Depth=1
	movl	8(%rcx), %ecx
	addq	$-53, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$4, %rcx
	jbe	.LBB6_74
	jmp	.LBB6_79
.LBB6_82:                               # %if.else517
                                        #   in Loop: Header=BB6_59 Depth=1
	cmpq	$61, %rax
	je	.LBB6_89
# BB#83:                                # %if.else517
                                        #   in Loop: Header=BB6_59 Depth=1
	cmpq	$65, %rax
	jne	.LBB6_84
	.align	16, 0x90
.LBB6_87:                               # %if.then542
                                        #   in Loop: Header=BB6_59 Depth=1
	movq	8(%r14), %rax
	movzbl	2(%rax), %edi
	movq	%r14, %rsi
	callq	gen_realpart
	movq	8(%r14), %rcx
	movzbl	2(%rcx), %edi
	movq	%rax, %r13
	movq	%r14, %rsi
	callq	gen_imagpart
	movzbl	2(%r13), %edi
	movq	%rax, %rbx
	movq	%r12, %rsi
	callq	gen_realpart
	movzbl	2(%rbx), %edi
	movq	%rax, %r14
	movq	%r12, %rsi
	callq	gen_imagpart
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movq	168(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	120(%rsp), %r13         # 8-byte Reload
	movq	%r14, %rdx
	callq	process_reg_param
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	160(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB6_90
	.align	16, 0x90
.LBB6_89:                               # %if.then536
                                        #   in Loop: Header=BB6_59 Depth=1
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	%r14, %rsi
	movq	%r12, %rdx
.LBB6_90:                               # %for.inc570
                                        #   in Loop: Header=BB6_59 Depth=1
	callq	process_reg_param
.LBB6_91:                               # %for.inc570
                                        #   in Loop: Header=BB6_59 Depth=1
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	144(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r15
	cmpq	%rdx, %rax
	jl	.LBB6_59
.LBB6_92:                               # %for.end572
	movq	%r13, 120(%rsp)         # 8-byte Spill
	movq	inlining(%rip), %rax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	%rax, inlining(%rip)
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	88(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB6_103
# BB#93:                                # %for.body577.lr.ph
	movq	120(%rsp), %rax         # 8-byte Reload
	leaq	8(%rax), %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	48(%rsp), %r14          # 8-byte Reload
	.align	16, 0x90
.LBB6_94:                               # %for.body577
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r12), %r15
	movzbl	(%r15), %eax
	cmpq	$66, %rax
	jne	.LBB6_102
# BB#95:                                # %land.lhs.true585
                                        #   in Loop: Header=BB6_94 Depth=1
	movq	8(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB6_97
# BB#96:                                # %land.lhs.true593
                                        #   in Loop: Header=BB6_94 Depth=1
	movl	8(%rax), %eax
	cmpq	$57, %rax
	ja	.LBB6_102
.LBB6_97:                               # %if.then602
                                        #   in Loop: Header=BB6_94 Depth=1
	movq	24(%rbx), %rdi
	movl	32(%rbx), %esi
	callq	emit_note
	testq	%rax, %rax
	je	.LBB6_99
# BB#98:                                # %if.then608
                                        #   in Loop: Header=BB6_94 Depth=1
	movl	(%rax), %ecx
	orq	$1073741824, %rcx       # imm = 0x40000000
	movl	%ecx, (%rax)
.LBB6_99:                               # %if.end612
                                        #   in Loop: Header=BB6_94 Depth=1
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	168(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rsi
	callq	copy_rtx_and_substitute
	movq	%rax, 184(%rsp)
	xorq	%rsi, %rsi
	movq	$1, %rcx
	leaq	184(%rsp), %rdi
	movq	%r15, %rdx
	callq	subst_constants
	callq	apply_change_group
	movq	184(%rsp), %rax
	movq	8(%rax), %rsi
	movzbl	2(%rax), %edi
	callq	memory_address_p
	movq	184(%rsp), %rcx
	testq	%r13, %rax
	jne	.LBB6_101
# BB#100:                               # %if.then623
                                        #   in Loop: Header=BB6_94 Depth=1
	movq	8(%rcx), %rdx
	xorq	%rsi, %rsi
	movq	%rcx, %rdi
	callq	change_address
	movq	%rax, %rcx
	movq	%rcx, 184(%rsp)
.LBB6_101:                              # %if.end628
                                        #   in Loop: Header=BB6_94 Depth=1
	movq	(%r14), %rdi
	xorq	%rdx, %rdx
	movq	%rcx, %rsi
	callq	store_expr
.LBB6_102:                              # %for.inc633
                                        #   in Loop: Header=BB6_94 Depth=1
	movq	(%rbx), %rbx
	addq	$8, %r14
	addq	$8, %r12
	testq	%rbx, %rbx
	jne	.LBB6_94
.LBB6_103:                              # %for.end637
	movq	168(%rsp), %r15         # 8-byte Reload
	movq	$0, 80(%r15)
	movq	64(%rsp), %rax          # 8-byte Reload
	movl	60(%rax), %ebx
	movq	$9, %rcx
	shrq	%cl, %rbx
	andq	$127, %rbx
	je	.LBB6_104
# BB#105:                               # %if.else673
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	96(%rax), %rdi
	movq	144(%rdi), %r14
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB6_130
# BB#106:                               # %if.then678
	movq	8(%r14), %rax
	movzbl	(%rax), %eax
	cmpq	$70, %rax
	movq	80(%rsp), %rbx          # 8-byte Reload
	jne	.LBB6_108
# BB#107:                               # %if.then686
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	copy_rtx_and_substitute
	movq	%rax, 184(%rsp)
	leaq	184(%rsp), %rdi
	xorq	%rsi, %rsi
	movq	$1, %rcx
	movq	%r15, %rdx
	callq	subst_constants
	callq	apply_change_group
	movq	184(%rsp), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	jmp	.LBB6_173
.LBB6_104:
	movq	80(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB6_173
.LBB6_130:                              # %if.else844
	movq	%r15, %r13
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	(%rsp), %rdx            # 8-byte Reload
	testq	%r15, %rdx
	je	.LBB6_132
# BB#131:
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	%r13, %r15
	jmp	.LBB6_173
.LBB6_108:                              # %if.else689
	movq	%r15, %r12
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB6_110
# BB#109:                               # %lor.lhs.false691
	callq	aggregate_value_p
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB6_110
# BB#111:                               # %if.end697
	movq	8(%r14), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB6_172
# BB#112:                               # %if.then705
	xorq	%rsi, %rsi
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	force_operand
	movl	target_flags(%rip), %edi
	movq	%rax, 184(%rsp)
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%rax, %rsi
	callq	force_reg
	movq	%rax, 184(%rsp)
	movl	8(%rax), %ecx
	addq	$-53, %rcx
	andq	%r15, %rcx
	movq	%rbx, %r13
	cmpq	$4, %rcx
	ja	.LBB6_114
# BB#113:                               # %if.then721
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%rax, %rsi
	callq	copy_to_mode_reg
	movq	%rax, 184(%rsp)
.LBB6_114:                              # %if.end726
	movq	8(%r14), %rcx
	movl	8(%rcx), %ecx
	movq	32(%r12), %rdx
	movq	%rax, (%rdx,%rcx,8)
	movq	56(%rsp), %rdx          # 8-byte Reload
	movzbl	(%rdx), %ecx
	cmpq	$133, %rcx
	jg	.LBB6_121
# BB#115:                               # %if.end726
	addq	$-54, %rcx
	andq	%r15, %rcx
	cmpq	$21, %rcx
	ja	.LBB6_116
# BB#117:                               # %if.end726
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$90135, %rax            # imm = 0x16017
	jne	.LBB6_124
# BB#118:                               # %if.end726
	cmpq	$21, %rcx
	jne	.LBB6_116
# BB#119:                               # %land.lhs.true785
	movq	8(%rdx), %rax
	movq	global_rtl+56(%rip), %rcx
	cmpq	%rcx, %rax
	movq	%r13, %rbx
	je	.LBB6_123
# BB#120:
	movq	%r12, %r15
	jmp	.LBB6_173
.LBB6_132:                              # %if.else847
	cmpq	$65, %rcx
	jne	.LBB6_133
# BB#157:                               # %if.then986
	testq	%r14, %r14
	jne	.LBB6_159
# BB#158:                               # %cond.false1004
	xorq	%rsi, %rsi
	callq	make_decl_rtl
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	96(%rax), %rax
	movq	144(%rax), %rax
	movl	(%rax), %eax
.LBB6_159:                              # %cond.end1011
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	andq	%rbx, %r15
	cmpq	%rax, %r15
	jne	.LBB6_160
# BB#161:                               # %if.end1019
	movq	8(%r14), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB6_163
# BB#162:                               # %lor.lhs.false1027
	movq	16(%r14), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB6_163
# BB#164:                               # %if.end1036
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB6_167
# BB#165:                               # %lor.lhs.false1039
	movq	88(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB6_167
# BB#166:                               # %lor.lhs.false1044
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	cmpq	%r15, %rdx
	je	.LBB6_168
.LBB6_167:                              # %if.then1050
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movl	(%rax), %eax
.LBB6_168:                              # %if.end1052
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$65, %rax
	jne	.LBB6_169
# BB#170:                               # %if.end1058
	movq	8(%r14), %rax
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	8(%rsi), %rcx
	movl	8(%rax), %eax
	movq	%r13, %r15
	movq	32(%r15), %rdx
	movq	%rcx, (%rdx,%rax,8)
	movq	16(%r14), %rax
	movq	16(%rsi), %rcx
	movl	8(%rax), %eax
	movq	32(%r15), %rdx
	movq	%rcx, (%rdx,%rax,8)
	movq	80(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB6_173
.LBB6_172:                              # %if.else838
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%r12, %r15
	movq	%r15, %rsi
	callq	copy_rtx_and_substitute
	movq	%rax, 184(%rsp)
	leaq	184(%rsp), %rdi
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%r15, %rdx
	callq	subst_constants
	callq	apply_change_group
	movq	184(%rsp), %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	emit_move_insn
	jmp	.LBB6_173
.LBB6_133:                              # %if.else847
	cmpq	$61, %rcx
	jne	.LBB6_171
# BB#134:                               # %if.then852
	testq	%r14, %r14
	je	.LBB6_136
# BB#135:
	movq	%r14, %rdx
	jmp	.LBB6_137
.LBB6_121:                              # %if.end726
	cmpq	$134, %rcx
	je	.LBB6_124
# BB#122:                               # %if.end726
	cmpq	$140, %rcx
	jne	.LBB6_116
	jmp	.LBB6_124
.LBB6_136:                              # %cond.false868
	xorq	%rsi, %rsi
	callq	make_decl_rtl
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	96(%rax), %rdi
	movq	144(%rdi), %rdx
	movl	(%rdx), %eax
.LBB6_137:                              # %cond.end875
	movq	$16, %rcx
	movq	%rax, %r12
	shrq	%cl, %r12
	andq	$255, %r12
	movq	88(%rsp), %rcx          # 8-byte Reload
	testq	%rcx, %rcx
	je	.LBB6_140
# BB#138:                               # %lor.lhs.false882
	movl	(%rcx), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB6_140
# BB#139:                               # %lor.lhs.false887
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%rbx, %rcx
	andq	%r15, %rcx
	cmpq	%rcx, %rsi
	je	.LBB6_148
.LBB6_140:                              # %if.then893
	movq	%rbx, %rcx
	andq	%r15, %rcx
	cmpq	$51, %rcx
	jne	.LBB6_147
# BB#141:                               # %if.then896
	testq	%rdx, %rdx
	jne	.LBB6_143
# BB#142:                               # %cond.false907
	xorq	%rsi, %rsi
	callq	make_decl_rtl
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	96(%rax), %rdi
	movq	144(%rdi), %rdx
	movl	(%rdx), %eax
.LBB6_143:                              # %cond.end914
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB6_211
# BB#144:                               # %if.then920
	testq	%rdx, %rdx
	jne	.LBB6_146
# BB#145:                               # %cond.false931
	xorq	%rsi, %rsi
	callq	make_decl_rtl
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	96(%rax), %rax
	movq	144(%rax), %rax
	movl	(%rax), %eax
.LBB6_146:                              # %cond.end938
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rbx
	movq	%rax, %r12
.LBB6_147:                              # %if.end945
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB6_148:                              # %if.end947
	andq	%rbx, %r15
	cmpq	%r15, %r12
	jne	.LBB6_150
# BB#149:
	movq	88(%rsp), %r12          # 8-byte Reload
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	%r13, %r15
	jmp	.LBB6_154
.LBB6_150:                              # %if.then950
	movl	target_flags(%rip), %eax
	movzbl	mode_bitsize(%r12,%r12), %edx
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rax, %rcx
	cmpq	%rcx, %rdx
	movq	%r13, %r15
	jbe	.LBB6_152
# BB#151:                               # %if.then959
	movq	%r12, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r12
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	gen_lowpart
	movq	%rax, 88(%rsp)          # 8-byte Spill
	jmp	.LBB6_153
.LBB6_152:                              # %if.else962
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_SUBREG
	movq	%rax, %r12
.LBB6_153:                              # %if.end966
	movq	80(%rsp), %rbx          # 8-byte Reload
.LBB6_154:                              # %if.end966
	movzbl	3(%r14), %eax
	testq	$64, %rax
	je	.LBB6_156
# BB#155:                               # %if.then971
	movq	%r12, 80(%r15)
	jmp	.LBB6_173
.LBB6_156:                              # %if.else973
	movl	8(%r14), %eax
	movq	32(%r15), %rcx
	movq	%r12, (%rcx,%rax,8)
	jmp	.LBB6_173
.LBB6_123:                              # %land.lhs.true791
	movq	%rbx, %r13
	movq	16(%rdx), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB6_116
.LBB6_124:                              # %if.then799
	movq	184(%rsp), %rax
	movq	64(%r12), %rbx
	movl	8(%rax), %esi
	movq	(%rbx), %rax
	cmpq	%rax, %rsi
	jae	.LBB6_126
# BB#125:
	movq	%r12, %rsi
	jmp	.LBB6_129
.LBB6_116:
	movq	%r13, %rbx
	movq	%r12, %r15
	jmp	.LBB6_173
.LBB6_126:                              # %if.then809
	movq	global_const_equiv_varray(%rip), %r14
	incq	%rsi
	andq	%r15, %rsi
	movq	%rbx, %rdi
	callq	varray_grow
	cmpq	%rbx, %r14
	movq	%rax, 64(%r12)
	movq	%r12, %rsi
	movq	%rax, %rbx
	jne	.LBB6_128
# BB#127:                               # %if.then823
	movq	%rbx, global_const_equiv_varray(%rip)
.LBB6_128:                              # %if.end826
	movq	56(%rsp), %rdx          # 8-byte Reload
.LBB6_129:                              # %if.end826
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movq	184(%rsp), %rax
	movl	8(%rax), %eax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	%rdx, 32(%rbx,%rax)
	movl	%r15d, 40(%rbx,%rax)
	movq	%r13, %rbx
	movq	%rsi, %r15
.LBB6_173:                              # %if.end1088
	movq	%rbx, %rdi
	callq	get_exception_pointer
	testq	%rax, %rax
	movq	%rax, 184(%rsp)
	je	.LBB6_175
# BB#174:                               # %if.then1091
	movq	cfun(%rip), %rdi
	callq	get_exception_pointer
	movq	184(%rsp), %rcx
	movl	8(%rcx), %ecx
	movq	32(%r15), %rdx
	movq	%rax, (%rdx,%rcx,8)
.LBB6_175:                              # %if.end1099
	movq	40(%r15), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	leaq	(%rax,%rcx,8), %rdi
	movq	72(%rsp), %rdx          # 8-byte Reload
	subq	%rcx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, inline_function_decl(%rip)
	movq	88(%rax), %rbx
	testq	%rbx, %rbx
	movq	120(%rsp), %r13         # 8-byte Reload
	je	.LBB6_178
# BB#176:                               # %for.body.i.preheader
	addq	$8, %r13
	leaq	200(%rsp), %r14
	.align	16, 0x90
.LBB6_177:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r15), %rsi
	movq	current_function_decl(%rip), %rdx
	movq	%rbx, %rdi
	callq	copy_decl_for_inlining
	movq	(%r13), %rdi
	movq	%rax, %r12
	movq	$1, %rdx
	movq	%r15, %rsi
	callq	copy_rtx_and_substitute
	movq	%rax, 200(%rsp)
	xorq	%rsi, %rsi
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%r15, %rdx
	callq	subst_constants
	callq	apply_change_group
	movq	200(%rsp), %rax
	movq	%rax, 144(%r12)
	movq	(%rbx), %rbx
	addq	$8, %r13
	testq	%rbx, %rbx
	jne	.LBB6_177
.LBB6_178:                              # %integrate_parm_decls.exit
	movq	80(%rsp), %r13          # 8-byte Reload
	movq	368(%r13), %rdi
	movq	%r15, %rsi
	movq	%r15, %rbx
	callq	integrate_decl_tree
	movq	128(%rsp), %r15         # 8-byte Reload
	movq	112(%r15), %rcx
	testq	%rcx, %rcx
	movq	%r15, %rdx
	je	.LBB6_180
# BB#179:                               # %integrate_parm_decls.exit
	movq	%rcx, %rdx
.LBB6_180:                              # %integrate_parm_decls.exit
	movq	%rdx, 56(%rax)
	movq	$0, inline_function_decl(%rip)
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	movq	%rax, 144(%rsp)         # 8-byte Spill
	callq	expand_start_bindings_and_block
	movq	24(%rbx), %rdi
	movq	8(%rdi), %rsi
	addq	$32, %rdi
	movabsq	$compare_blocks, %rcx
	movq	$8, %rdx
	callq	specqsort
	callq	emit_queue
	callq	do_pending_stack_adjust
	movq	64(%rbx), %rax
	movq	%rax, global_const_equiv_varray(%rip)
	movzbl	424(%r13), %eax
	testq	$64, %rax
	je	.LBB6_182
# BB#181:                               # %if.then1129
	leaq	192(%rsp), %rsi
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	callq	emit_stack_save
.LBB6_182:                              # %if.end1130
	movq	144(%r13), %r15
	testq	%r15, %r15
	movq	%rbx, %r14
	je	.LBB6_186
# BB#183:                               # %for.cond.preheader.i
	movslq	(%r15), %rax
	testq	%rax, %rax
	jle	.LBB6_186
# BB#184:                               # %for.body.lr.ph.i1565
	movq	$1, %r12
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB6_185:                              # %for.body.i1569
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r15), %rax
	movq	cfun(%rip), %rdi
	movq	(%rax,%rbx), %rsi
	callq	get_func_hard_reg_initial_val
	movq	8(%r15), %rcx
	movq	8(%rcx,%rbx), %rcx
	movl	8(%rcx), %ecx
	movq	32(%r14), %rdx
	movq	%rax, (%rdx,%rcx,8)
	movslq	(%r15), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r12
	addq	$16, %rbx
	cmpq	%rax, %rdx
	jl	.LBB6_185
.LBB6_186:                              # %setup_initial_hard_reg_value_integration.exit
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	152(%rsp), %rdx         # 8-byte Reload
	callq	copy_insn_list
	movq	%r13, %r15
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	duplicate_eh_regions
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	copy_insn_notes
	movq	112(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB6_188
# BB#187:                               # %if.then1134
	callq	emit_label
.LBB6_188:                              # %if.end1137
	movzbl	424(%r15), %eax
	testq	$64, %rax
	je	.LBB6_190
# BB#189:                               # %if.then1143
	movq	192(%rsp), %rsi
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	callq	emit_stack_restore
.LBB6_190:                              # %if.end1144
	movq	cfun(%rip), %rax
	movzbl	426(%rax), %eax
	testq	$4, %rax
	je	.LBB6_191
# BB#192:                               # %if.else1150
	movq	current_function_decl(%rip), %rax
	movq	104(%rax), %rax
	movq	(%rax), %rax
	movq	144(%rsp), %rcx         # 8-byte Reload
	movq	%rax, (%rcx)
	movq	current_function_decl(%rip), %rax
	movq	104(%rax), %rax
	movq	%rcx, (%rax)
	jmp	.LBB6_193
.LBB6_191:                              # %if.then1149
	movq	144(%rsp), %rdi         # 8-byte Reload
	callq	insert_block
.LBB6_193:                              # %if.end1161
	movq	88(%rsp), %rbx          # 8-byte Reload
	movq	%r14, %r15
	xorq	%rdi, %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	xorq	%r13, %r13
	callq	expand_end_bindings
	movl	flag_test_coverage(%rip), %eax
	testq	%rax, %rax
	je	.LBB6_195
# BB#194:                               # %if.then1163
	movabsq	$4294967212, %rsi       # imm = 0xFFFFFFAC
	xorq	%rdi, %rdi
	callq	emit_note
.LBB6_195:                              # %if.end1165
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	testq	%rbx, %rbx
	je	.LBB6_201
# BB#196:                               # %land.lhs.true1168
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movq	8(%rax), %rdi
	movl	60(%rdi), %eax
	andq	$65024, %rax            # imm = 0xFE00
	cmpq	$26112, %rax            # imm = 0x6600
	jne	.LBB6_197
# BB#198:                               # %land.lhs.true1180
	callq	aggregate_value_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_200
# BB#199:
	movq	%rbx, %r13
	jmp	.LBB6_201
.LBB6_197:
	movq	%rbx, %r13
	jmp	.LBB6_201
.LBB6_200:                              # %if.then1187
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movq	8(%rax), %rdx
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	copy_blkmode_from_reg
	movq	%rax, %r13
.LBB6_201:                              # %if.end1193
	movq	56(%rsp), %rsi          # 8-byte Reload
	testq	%rsi, %rsi
	je	.LBB6_203
# BB#202:                               # %if.then1195
	movq	64(%rsp), %r14          # 8-byte Reload
	movl	60(%r14), %ebx
	movq	$9, %rcx
	shrq	%cl, %rbx
	andq	$127, %rbx
	movq	%rbx, %rdi
	callq	memory_address
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %r13
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	set_mem_attributes
.LBB6_203:                              # %if.end1208
	movq	136(%rsp), %rdi         # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB6_205
# BB#204:                               # %if.then1210
	callq	free
.LBB6_205:                              # %do.body
	movq	64(%r15), %rdi
	testq	%rdi, %rdi
	movq	96(%rsp), %rbx          # 8-byte Reload
	je	.LBB6_207
# BB#206:                               # %if.then1214
	callq	free
	movq	$0, 64(%r15)
.LBB6_207:                              # %do.end
	movq	32(%r15), %rdi
	callq	free
	movq	24(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB6_209
# BB#208:                               # %if.then1222
	callq	free
	movq	$0, 24(%r15)
.LBB6_209:                              # %do.end1226
	movq	48(%r15), %rdi
	callq	free
	movq	%r15, %rdi
	callq	free
	movq	%rbx, %rdi
	callq	free
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	160(%rsp), %rax         # 8-byte Reload
	movq	%rax, inlining(%rip)
.LBB6_210:                              # %return
	movq	%r13, %rax
	addq	$208, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB6_3:                                # %if.then
	movabsq	$.L.str15, %rax
	movabsq	$.L__FUNCTION__.expand_inline_function, %rcx
	movq	$686, %rsi              # imm = 0x2AE
	jmp	.LBB6_4
.LBB6_26:                               # %if.then158
	movabsq	$.L.str15, %rax
	movabsq	$.L__FUNCTION__.expand_inline_function, %rcx
	movq	$775, %rsi              # imm = 0x307
	jmp	.LBB6_4
.LBB6_88:                               # %if.else565
	movabsq	$.L.str15, %rax
	movabsq	$.L__FUNCTION__.expand_inline_function, %rcx
	movq	$964, %rsi              # imm = 0x3C4
	jmp	.LBB6_4
.LBB6_110:                              # %if.then696
	movabsq	$.L.str15, %rax
	movabsq	$.L__FUNCTION__.expand_inline_function, %rcx
	movq	$1027, %rsi             # imm = 0x403
	jmp	.LBB6_4
.LBB6_160:                              # %if.then1018
	movabsq	$.L.str15, %rax
	movabsq	$.L__FUNCTION__.expand_inline_function, %rcx
	movq	$1154, %rsi             # imm = 0x482
	jmp	.LBB6_4
.LBB6_163:                              # %if.then1035
	movabsq	$.L.str15, %rax
	movabsq	$.L__FUNCTION__.expand_inline_function, %rcx
	movq	$1157, %rsi             # imm = 0x485
	jmp	.LBB6_4
.LBB6_169:                              # %if.then1057
	movabsq	$.L.str15, %rax
	movabsq	$.L__FUNCTION__.expand_inline_function, %rcx
	movq	$1167, %rsi             # imm = 0x48F
	jmp	.LBB6_4
.LBB6_171:                              # %if.else1083
	movabsq	$.L.str15, %rax
	movabsq	$.L__FUNCTION__.expand_inline_function, %rcx
	movq	$1173, %rsi             # imm = 0x495
	jmp	.LBB6_4
.LBB6_211:                              # %if.else943
	movabsq	$.L.str15, %rax
	movabsq	$.L__FUNCTION__.expand_inline_function, %rcx
	movq	$1112, %rsi             # imm = 0x458
.LBB6_4:                                # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp109:
	.size	expand_inline_function, .Ltmp109-expand_inline_function
	.cfi_endproc

	.globl	copy_rtx_and_substitute
	.align	16, 0x90
	.type	copy_rtx_and_substitute,@function
copy_rtx_and_substitute:                # @copy_rtx_and_substitute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp116:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp117:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp118:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp119:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp120:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp121:
	.cfi_def_cfa_offset 112
.Ltmp122:
	.cfi_offset %rbx, -48
.Ltmp123:
	.cfi_offset %r12, -40
.Ltmp124:
	.cfi_offset %r13, -32
.Ltmp125:
	.cfi_offset %r14, -24
.Ltmp126:
	.cfi_offset %r15, -16
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	%rsi, %r8
	movq	%rdi, %rbx
	xorq	%rsi, %rsi
	testq	%rbx, %rbx
	je	.LBB7_132
# BB#1:                                 # %if.end
	movl	(%rbx), %r12d
	movq	%r12, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	movq	$16, %rcx
	movq	%r12, %r14
	shrq	%cl, %r14
	andq	$255, %r14
	leaq	-36(%rdx), %rax
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rax
	cmpq	$34, %rax
	ja	.LBB7_107
# BB#2:                                 # %if.end
	movq	%rbx, %rsi
	jmpq	*.LJTI7_0(,%rax,8)
.LBB7_43:                               # %sw.bb365
	movq	8(%rbx), %rdi
	cmpq	$49, %rdx
	movq	%rdx, %r14
	jne	.LBB7_44
# BB#45:                                # %sw.bb365
	movabsq	$1, %rdx
	jmp	.LBB7_46
.LBB7_51:                               # %sw.bb390
	movl	40(%rbx), %eax
	movabsq	$4294967208, %rcx       # imm = 0xFFFFFFA8
	cmpq	%rcx, %rax
	jne	.LBB7_107
.LBB7_52:                               # %sw.bb397
	movslq	48(%rbx), %r14
	movq	40(%r8), %rax
	movq	(%rax,%r14,8), %rax
	andq	$268435456, %r12        # imm = 0x10000000
	testq	%rax, %rax
	jne	.LBB7_54
# BB#53:                                # %if.then.i1461
	movq	%r8, %r15
	callq	gen_label_rtx
	movq	%r15, %r8
	movq	40(%r8), %rcx
	movq	%rax, (%rcx,%r14,8)
.LBB7_54:                               # %get_label_from_map.exit1463
	movl	(%rax), %ecx
	movabsq	$4026531839, %rdx       # imm = 0xEFFFFFFF
	andq	%rcx, %rdx
	orq	%r12, %rdx
	movl	%edx, (%rax)
	movslq	48(%rbx), %rbx
	movq	40(%r8), %rax
	movq	(%rax,%rbx,8), %rsi
	testq	%rsi, %rsi
	jne	.LBB7_132
# BB#55:                                # %if.then.i1452
	movq	%r8, %r14
	callq	gen_label_rtx
	movq	40(%r14), %rcx
	movq	%rax, %rsi
	movq	%rsi, (%rcx,%rbx,8)
	jmp	.LBB7_132
.LBB7_83:                               # %sw.bb550
	movq	88(%r8), %rax
	movq	32(%rbx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB7_107
# BB#84:                                # %if.then555
	movq	$41, %rdi
	movq	%r8, %r14
	callq	rtx_alloc
	movq	%rax, %rsi
	movl	(%rbx), %eax
	movl	(%rsi), %ecx
	andq	$134217728, %rax        # imm = 0x8000000
	andq	$-134217729, %rcx       # imm = 0xFFFFFFFFF7FFFFFF
	orq	%rax, %rcx
	movl	%ecx, (%rsi)
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	movabsq	$4278255615, %rdx       # imm = 0xFF00FFFF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, (%rsi)
	movq	8(%rbx), %rax
	movq	%rax, 8(%rsi)
	movq	16(%rbx), %rax
	movq	%rax, 16(%rsi)
	movl	24(%rbx), %eax
	movl	%eax, 24(%rsi)
	movq	96(%r14), %rax
	movq	%rax, 32(%rsi)
	movq	104(%r14), %rax
	movq	%rax, 40(%rsi)
	movq	48(%rbx), %rax
	movq	%rax, 48(%rsi)
	movl	56(%rbx), %eax
	movl	%eax, 56(%rsi)
	jmp	.LBB7_132
.LBB7_86:                               # %sw.bb695
	movq	8(%rbx), %rdi
	movq	global_rtl+56(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB7_88
# BB#87:                                # %sw.bb695
	movq	global_rtl+48(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB7_88
# BB#91:                                # %if.else755
	movq	$1, %rdx
	movq	%r8, %rsi
	movq	%r8, %r15
	callq	copy_rtx_and_substitute
	movq	16(%rbx), %rdi
	movq	%rax, %r14
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	callq	copy_rtx_and_substitute
	jmp	.LBB7_92
.LBB7_85:                               # %sw.bb609
	movq	8(%rbx), %rax
	movq	8(%rax), %rdi
	movzbl	2(%rax), %r14d
	xorq	%rdx, %rdx
	movq	%r8, %rsi
	movq	%r8, %r15
	callq	copy_rtx_and_substitute
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	8(%rbx), %rcx
	movq	%rax, %r12
	movl	(%rcx), %ecx
	movl	(%r12), %eax
	andq	$134217728, %rcx        # imm = 0x8000000
	andq	$-134217729, %rax       # imm = 0xFFFFFFFFF7FFFFFF
	orq	%rcx, %rax
	movl	%eax, (%r12)
	movq	8(%rbx), %rcx
	movl	(%rcx), %ecx
	andq	$268435456, %rcx        # imm = 0x10000000
	andq	$-268435457, %rax       # imm = 0xFFFFFFFFEFFFFFFF
	orq	%rcx, %rax
	movl	%eax, (%r12)
	movq	8(%rbx), %rcx
	movl	(%rcx), %ecx
	andq	$-2147483648, %rcx      # imm = 0xFFFFFFFF80000000
	andq	$2147483647, %rax       # imm = 0x7FFFFFFF
	orq	%rcx, %rax
	movl	%eax, (%r12)
	movq	8(%rbx), %rcx
	movl	(%rcx), %ecx
	andq	$67108864, %rcx         # imm = 0x4000000
	andq	$-67108865, %rax        # imm = 0xFFFFFFFFFBFFFFFF
	orq	%rcx, %rax
	movl	%eax, (%r12)
	movq	8(%rbx), %rcx
	movl	(%rcx), %ecx
	andq	$16777216, %rcx         # imm = 0x1000000
	movabsq	$4278190079, %rdx       # imm = 0xFEFFFFFF
	andq	%rax, %rdx
	orq	%rcx, %rdx
	movl	%edx, (%r12)
	movq	8(%rbx), %rax
	movq	16(%rax), %rax
	movq	%rax, 16(%r12)
	movq	16(%rbx), %rdi
	movzbl	2(%rbx), %r14d
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	callq	copy_rtx_and_substitute
	movq	$50, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	jmp	.LBB7_93
.LBB7_78:                               # %sw.bb519
	movl	mode_class(,%r14,4), %eax
	cmpq	$2, %rax
	jne	.LBB7_80
# BB#79:                                # %do.body
	addq	$16, %rbx
	movq	16(%rbx), %rax
	movq	%rax, 16(%rsp)
	movq	(%rbx), %rax
	movq	8(%rbx), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	callq	immed_real_const_1
	movq	%rax, %rsi
	jmp	.LBB7_132
.LBB7_81:                               # %sw.bb543
	testq	$1073741824, %r12       # imm = 0x40000000
	jne	.LBB7_82
.LBB7_107:                              # %sw.epilog
	movq	%r8, 40(%rsp)           # 8-byte Spill
	movq	%rdx, %rdi
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	callq	rtx_alloc
	movq	%rbx, %r8
	movl	(%rax), %edi
	movq	$16, %rcx
	shlq	%cl, %r14
	movabsq	$4278255615, %rcx       # imm = 0xFF00FFFF
	andq	%rdi, %rcx
	orq	%r14, %rcx
	movl	%ecx, (%rax)
	movl	(%r8), %edx
	andq	$268435456, %rdx        # imm = 0x10000000
	movabsq	$4026531839, %rsi       # imm = 0xEFFFFFFF
	andq	%rcx, %rsi
	orq	%rdx, %rsi
	movl	%esi, (%rax)
	movl	(%r8), %ecx
	andq	$134217728, %rcx        # imm = 0x8000000
	movabsq	$4160749567, %rdx       # imm = 0xF7FFFFFF
	andq	%rsi, %rdx
	orq	%rcx, %rdx
	movl	%edx, (%rax)
	movl	(%r8), %ecx
	movabsq	$4227858431, %rsi       # imm = 0xFBFFFFFF
	andq	%rdx, %rsi
	andq	$65535, %rdi            # imm = 0xFFFF
	movzbl	rtx_length(%rdi), %edx
	andq	$67108864, %rcx         # imm = 0x4000000
	orq	%rcx, %rsi
	movl	%esi, (%rax)
	movq	%rax, %rsi
	testq	%rdx, %rdx
	je	.LBB7_129
# BB#108:                               # %for.body.lr.ph
	movq	rtx_format(,%rdi,8), %rcx
	xorq	%rbx, %rbx
	jmp	.LBB7_109
	.align	16, 0x90
.LBB7_112:                              # %for.body
                                        #   in Loop: Header=BB7_109 Depth=1
	cmpq	$100, %rax
	jg	.LBB7_118
# BB#113:                               # %for.body
                                        #   in Loop: Header=BB7_109 Depth=1
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	andq	%r13, %rax
	cmpq	$69, %rax
	jne	.LBB7_125
# BB#114:                               # %sw.bb971
                                        #   in Loop: Header=BB7_109 Depth=1
	movq	8(%r8,%rbx,8), %rax
	movq	%rax, 8(%rsi,%rbx,8)
	movq	8(%r8,%rbx,8), %rax
	testq	%rax, %rax
	je	.LBB7_128
# BB#115:                               # %land.lhs.true986
                                        #   in Loop: Header=BB7_109 Depth=1
	movl	(%rax), %edi
	testq	%rdi, %rdi
	je	.LBB7_128
# BB#116:                               # %if.then993
                                        #   in Loop: Header=BB7_109 Depth=1
	movq	%r8, %r12
	movq	%rsi, %r14
	callq	rtvec_alloc
	movq	%r14, %rsi
	movq	%r12, %r8
	movq	%rax, 8(%rsi,%rbx,8)
	movslq	(%rax), %rax
	testq	%rax, %rax
	movq	$1, %r12
	jle	.LBB7_128
	.align	16, 0x90
.LBB7_117:                              # %for.body1012
                                        #   Parent Loop BB7_109 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%r8,%rbx,8), %rax
	movq	(%rax,%r12,8), %rdi
	movq	%rsi, %r14
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%r8, %r15
	callq	copy_rtx_and_substitute
	movq	%r14, %rsi
	movq	%r15, %r8
	movq	8(%rsi,%rbx,8), %rcx
	movq	%rax, (%rcx,%r12,8)
	movq	8(%rsi,%rbx,8), %rax
	movslq	(%rax), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r12
	cmpq	%rax, %rdx
	jl	.LBB7_117
	jmp	.LBB7_128
	.align	16, 0x90
.LBB7_118:                              # %for.body
                                        #   in Loop: Header=BB7_109 Depth=1
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	cmpq	$114, %rax
	jg	.LBB7_122
# BB#119:                               # %for.body
                                        #   in Loop: Header=BB7_109 Depth=1
	movq	%rsi, %r15
	andq	%r13, %rax
	cmpq	$101, %rax
	jne	.LBB7_120
# BB#126:                               # %sw.bb947
                                        #   in Loop: Header=BB7_109 Depth=1
	movq	8(%r8,%rbx,8), %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%r8, %r12
	callq	copy_rtx_and_substitute
	movq	%r12, %r8
	movq	%r15, %rsi
	jmp	.LBB7_127
.LBB7_122:                              # %for.body
                                        #   in Loop: Header=BB7_109 Depth=1
	addq	$-115, %rax
	andq	%r13, %rax
	cmpq	$4, %rax
	ja	.LBB7_125
# BB#123:                               # %for.body
                                        #   in Loop: Header=BB7_109 Depth=1
	jmpq	*.LJTI7_1(,%rax,8)
.LBB7_124:                              # %sw.bb957
                                        #   in Loop: Header=BB7_109 Depth=1
	movq	8(%r8,%rbx,8), %rax
	movslq	8(%rax), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	48(%rcx), %rcx
	movq	(%rcx,%rax,8), %rax
	jmp	.LBB7_127
.LBB7_120:                              # %for.body
                                        #   in Loop: Header=BB7_109 Depth=1
	cmpq	$105, %rax
	movq	%r15, %rsi
	jne	.LBB7_125
# BB#121:                               # %sw.bb1038
                                        #   in Loop: Header=BB7_109 Depth=1
	movl	8(%r8,%rbx,8), %eax
	movl	%eax, 8(%rsi,%rbx,8)
	jmp	.LBB7_128
	.align	16, 0x90
.LBB7_109:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_117 Depth 2
	movsbq	(%rcx), %rax
	incq	%rcx
	cmpq	$68, %rax
	jg	.LBB7_112
# BB#110:                               # %for.body
                                        #   in Loop: Header=BB7_109 Depth=1
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	andq	%r13, %rax
	cmpq	$48, %rax
	jne	.LBB7_125
.LBB7_111:                              # %sw.bb938
                                        #   in Loop: Header=BB7_109 Depth=1
	movq	8(%r8,%rbx,8), %rax
.LBB7_127:                              # %for.inc1067
                                        #   in Loop: Header=BB7_109 Depth=1
	movq	%rax, 8(%rsi,%rbx,8)
.LBB7_128:                              # %for.inc1067
                                        #   in Loop: Header=BB7_109 Depth=1
	movzbl	(%rsi), %eax
	incq	%rbx
	movzbl	rtx_length(%rax), %eax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	56(%rsp), %rcx          # 8-byte Reload
	jl	.LBB7_109
.LBB7_129:                              # %for.end1069
	cmpq	$41, 32(%rsp)           # 8-byte Folded Reload
	movq	40(%rsp), %r15          # 8-byte Reload
	jne	.LBB7_132
# BB#130:                               # %land.lhs.true1072
	cmpq	$0, 88(%r15)
	jne	.LBB7_132
# BB#131:                               # %if.then1076
	movq	32(%r8), %rax
	movq	%rax, 88(%r15)
	movq	32(%rsi), %rax
	movq	%rax, 96(%r15)
	movq	40(%rsi), %rax
	movq	%rax, 104(%r15)
	jmp	.LBB7_132
.LBB7_125:                              # %sw.default1065
	movabsq	$.L.str15, %rdi
	movabsq	$.L__FUNCTION__.copy_rtx_and_substitute, %rdx
	movq	$2387, %rsi             # imm = 0x953
	callq	fancy_abort
.LBB7_3:                                # %sw.bb
	movslq	8(%rbx), %r15
	cmpq	$58, %r15
	jl	.LBB7_6
# BB#4:                                 # %lor.lhs.false
	movl	(%r8), %eax
	testq	%rax, %rax
	je	.LBB7_31
# BB#5:                                 # %land.lhs.true
	movq	8(%r8), %rax
	movq	160(%rax), %rax
	movq	128(%rax), %rax
	cmpq	%rbx, %rax
	je	.LBB7_6
.LBB7_31:                               # %if.end194
	movq	32(%r8), %rax
	cmpq	$0, (%rax,%r15,8)
	jne	.LBB7_34
# BB#32:                                # %if.then200
	movq	%r14, %rdi
	movq	%r8, %r14
	callq	gen_reg_rtx
	movq	%r14, %r8
	movq	32(%r8), %rcx
	movq	%rax, (%rcx,%r15,8)
	movq	32(%r8), %rax
	movq	(%rax,%r15,8), %rax
	movl	(%rbx), %ecx
	movl	(%rax), %edx
	andq	$134217728, %rcx        # imm = 0x8000000
	movabsq	$4160749567, %rsi       # imm = 0xF7FFFFFF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, (%rax)
	movq	32(%r8), %rax
	movq	(%rax,%r15,8), %rax
	movl	(%rbx), %ecx
	movl	(%rax), %edx
	andq	$268435456, %rcx        # imm = 0x10000000
	movabsq	$4026531839, %rsi       # imm = 0xEFFFFFFF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, (%rax)
	movq	32(%r8), %rax
	movq	(%rax,%r15,8), %rax
	movl	(%rbx), %ecx
	movl	(%rax), %edx
	andq	$67108864, %rcx         # imm = 0x4000000
	movabsq	$4227858431, %rsi       # imm = 0xFBFFFFFF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, (%rax)
	movq	128(%r8), %rax
	movq	(%rax,%r15,8), %rax
	movslq	(%rax), %rax
	testq	%rax, %rax
	jns	.LBB7_34
# BB#33:                                # %if.then240
	movq	32(%r8), %rax
	movq	120(%r8), %rcx
	movq	(%rax,%r15,8), %rdi
	movzbl	(%rcx,%r15), %esi
	movq	%r8, %rbx
	callq	mark_reg_pointer
	movq	%rbx, %r8
.LBB7_34:                               # %if.end248
	movq	32(%r8), %rax
	movq	(%rax,%r15,8), %rsi
	jmp	.LBB7_132
.LBB7_35:                               # %sw.bb252
	movq	8(%rbx), %rdi
	movq	%r8, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	callq	copy_rtx_and_substitute
	movq	8(%rbx), %rdx
	movl	16(%rbx), %ecx
	movzbl	2(%rbx), %edi
	movzbl	2(%rdx), %edx
	movq	%rax, %rsi
	callq	simplify_gen_subreg
	movq	%rax, %rsi
	jmp	.LBB7_132
.LBB7_94:                               # %sw.bb765
	movq	inlining(%rip), %rdi
	movq	%r13, %r12
	testq	%rdi, %rdi
	je	.LBB7_95
# BB#99:                                # %land.lhs.true767
	movq	%r8, %r15
	movq	8(%rbx), %rax
	movl	(%rax), %ecx
	andq	$67174399, %rcx         # imm = 0x400FFFF
	cmpq	$67108932, %rcx         # imm = 0x4000044
	jne	.LBB7_100
# BB#101:                               # %if.then783
	movq	%rax, %rsi
	callq	get_pool_mode_for_function
	movq	inlining(%rip), %rdi
	movq	8(%rbx), %rsi
	movq	%rax, %r14
	callq	get_pool_constant_for_function
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	copy_rtx_and_substitute
	movq	%rax, %rsi
	movzbl	(%rsi), %ecx
	cmpq	$133, %rcx
	jg	.LBB7_104
# BB#102:                               # %if.then783
	addq	$-54, %rcx
	andq	%r12, %rcx
	cmpq	$14, %rcx
	ja	.LBB7_132
# BB#103:                               # %if.then783
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB7_106
	jmp	.LBB7_132
.LBB7_56:                               # %sw.bb414
	movq	8(%rbx), %rdx
	testq	$134217728, %r12        # imm = 0x8000000
	jne	.LBB7_59
# BB#57:                                # %cond.false
	movslq	48(%rdx), %r12
	movq	40(%r8), %rax
	movq	(%rax,%r12,8), %rdx
	testq	%rdx, %rdx
	jne	.LBB7_59
# BB#58:                                # %if.then.i
	movq	%r8, %r15
	callq	gen_label_rtx
	movq	40(%r15), %rcx
	movq	%rax, %rdx
	movq	%rdx, (%rcx,%r12,8)
.LBB7_59:                               # %cond.end
	movq	$67, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_fmt_u00
	movq	%rax, %rsi
	movq	%rbx, %rdi
	movl	(%rdi), %eax
	movl	(%rsi), %ecx
	andq	$268435456, %rax        # imm = 0x10000000
	movabsq	$4026531839, %rdx       # imm = 0xEFFFFFFF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, (%rsi)
	movzbl	3(%rdi), %ecx
	xorq	%rax, %rax
	testq	$8, %rcx
	je	.LBB7_65
# BB#60:                                # %land.rhs
	movq	8(%rsi), %rax
	movslq	48(%rax), %r14
	movq	%rdi, %r15
	movq	%rsi, %rbx
	callq	get_first_label_num
	movq	%rbx, %rsi
	movq	%r15, %rdi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r14
	movq	$1, %rax
	jl	.LBB7_65
# BB#61:                                # %land.rhs452
	movq	%rdi, %r14
	movq	8(%rsi), %rax
	movq	%rsi, %r15
	movslq	48(%rax), %rbx
	callq	max_label_num
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rbx
	jl	.LBB7_62
# BB#63:                                # %land.rhs452
	movabsq	$1, %rax
	jmp	.LBB7_64
.LBB7_68:                               # %sw.bb480
	testq	$67108864, %r12         # imm = 0x4000000
	je	.LBB7_69
# BB#70:                                # %if.then485
	movq	%r8, %r15
	movq	inlining(%rip), %r12
	testq	%r12, %r12
	jne	.LBB7_72
# BB#71:                                # %if.then485
	movq	cfun(%rip), %r12
.LBB7_72:                               # %if.then485
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	get_pool_constant_for_function
	movq	%rax, %r14
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	get_pool_mode_for_function
	cmpq	$0, inlining(%rip)
	movq	%rax, %r12
	je	.LBB7_75
# BB#73:                                # %if.then495
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	copy_rtx_and_substitute
	movq	%r12, %rdi
	jmp	.LBB7_74
.LBB7_36:                               # %sw.bb270
	movq	8(%rbx), %rdi
	movq	%r8, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%r8, %r15
	callq	copy_rtx_and_substitute
	movq	24(%rbx), %r8
	movq	$70, %rdi
	xorq	%rcx, %rcx
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_eit
	movq	%rbx, %rsi
	movq	%r15, %rdx
	movslq	16(%rsi), %rbx
	movq	32(%rdx), %rcx
	movq	(%rcx,%rbx,8), %rcx
	movq	%rax, %r12
	testq	%rcx, %rcx
	je	.LBB7_38
# BB#37:                                # %if.then285
	movl	8(%rcx), %ebx
	jmp	.LBB7_42
.LBB7_44:
	movabsq	$0, %rdx
.LBB7_46:                               # %sw.bb365
	movq	%r8, %rsi
	callq	copy_rtx_and_substitute
	movzbl	(%rax), %ecx
	cmpq	$63, %rcx
	jne	.LBB7_47
# BB#48:                                # %land.lhs.true376
	movq	8(%rbx), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$63, %rcx
	movq	%r14, %rdi
	je	.LBB7_50
# BB#49:                                # %if.then384
	movq	8(%rax), %rax
	jmp	.LBB7_50
.LBB7_47:
	movq	%r14, %rdi
.LBB7_50:                               # %if.end388
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rsi
	jmp	.LBB7_132
.LBB7_6:                                # %if.then5
	movq	32(%r8), %rax
	movq	(%rax,%r15,8), %rsi
	testq	%rsi, %rsi
	jne	.LBB7_132
# BB#7:                                 # %if.else
	movq	%r15, %rax
	andq	%r13, %rax
	cmpq	$54, %rax
	jne	.LBB7_8
# BB#10:                                # %if.then13
	movq	8(%r8), %rax
	movq	160(%rax), %rdi
	movq	%r8, %rbx
	callq	get_func_frame_size
	movq	8(%rbx), %rcx
	movq	%rbx, %r12
	movq	160(%rcx), %rcx
	movslq	400(%rcx), %rdx
	movq	%rax, %rbx
	movq	$60, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	andq	$7, %rax
	addq	%rdx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$35, %rcx
	sarq	%cl, %rax
	addq	$7, %rdx
	andq	%r13, %rdx
	cmpq	$15, %rdx
	jb	.LBB7_12
# BB#11:                                # %if.then23
	leaq	-1(%rbx,%rax), %rcx
	negq	%rax
	andq	%rcx, %rax
	movq	%rax, %rbx
.LBB7_12:                               # %if.end25
	callq	start_sequence
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$51, %rdi
	movq	$1, %rdx
	movq	%rbx, %rsi
	callq	assign_stack_temp
	movq	8(%rax), %rdi
	movq	%rbx, %rsi
	callq	plus_constant_wide
	movl	target_flags(%rip), %ebx
	movq	%rax, %r13
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	force_operand
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	force_reg
	movq	32(%r12), %rcx
	movq	%rax, %rbx
	movq	%rbx, (%rcx,%r15,8)
	movq	32(%r12), %rax
	movl	target_flags(%rip), %esi
	movq	(%rax,%r15,8), %rdi
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	callq	mark_reg_pointer
	movq	64(%r12), %r15
	movl	8(%rbx), %esi
	movq	%rbx, %r14
	movq	(%r15), %rax
	cmpq	%rax, %rsi
	jb	.LBB7_16
# BB#13:                                # %if.then51
	movq	global_const_equiv_varray(%rip), %rbx
	incq	%rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	movq	%r15, %rdi
	callq	varray_grow
	cmpq	%r15, %rbx
	movq	%rax, 64(%r12)
	movq	%rax, %r15
	movq	%r12, %rbx
	jne	.LBB7_15
# BB#14:                                # %if.then64
	movq	%r15, global_const_equiv_varray(%rip)
	movq	64(%rbx), %r15
.LBB7_15:                               # %if.end67
	movq	%rbx, %r12
.LBB7_16:                               # %if.end67
	movl	8(%r14), %eax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	%r13, 32(%r15,%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, 40(%r15,%rax)
	movq	%r14, %rbx
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
	movq	16(%r12), %rsi
	movq	%r14, %rdi
	callq	emit_insn_after
	movq	%rbx, %rsi
	jmp	.LBB7_132
.LBB7_88:                               # %if.then707
	movq	%r8, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%r8, %r14
	callq	copy_rtx_and_substitute
	movq	%r14, %rsi
	movq	%rbx, %rdi
	movq	8(%rdi), %r14
	movl	8(%r14), %eax
	movq	32(%rsi), %rcx
	movq	(%rcx,%rax,8), %rax
	movl	8(%rax), %eax
	movq	64(%rsi), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	32(%rdx,%rax), %rax
	movzbl	(%rax), %ecx
	xorq	%rbx, %rbx
	cmpq	$61, %rcx
	je	.LBB7_90
# BB#89:                                # %cond.false735
	movq	16(%rax), %rax
	movq	8(%rax), %rbx
.LBB7_90:                               # %cond.end742
	movq	16(%rdi), %rdi
	xorq	%rdx, %rdx
	callq	copy_rtx_and_substitute
	negq	%rbx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	plus_constant_wide
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	force_operand
.LBB7_92:                               # %return
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
.LBB7_93:                               # %return
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rsi
	jmp	.LBB7_132
.LBB7_80:                               # %if.else535
	movq	16(%rbx), %rdi
	movq	24(%rbx), %rsi
	xorq	%rdx, %rdx
	callq	immed_double_const
	movq	%rax, %rsi
	jmp	.LBB7_132
.LBB7_95:                               # %sw.bb765.if.end838_crit_edge
	movq	8(%rbx), %rax
	jmp	.LBB7_96
.LBB7_69:
	movq	%rbx, %rsi
	jmp	.LBB7_132
.LBB7_38:                               # %if.else292
	movq	%rdx, %r15
	cmpq	$58, %rbx
	jl	.LBB7_42
# BB#39:                                # %if.then295
	movq	8(%rsi), %r14
	movzbl	2(%r14), %edi
	callq	gen_reg_rtx
	movq	%r15, %rdx
	movq	32(%rdx), %rcx
	movq	%rax, (%rcx,%rbx,8)
	movq	32(%rdx), %rax
	movq	(%rax,%rbx,8), %rax
	movl	(%r14), %ecx
	movl	(%rax), %edi
	andq	$134217728, %rcx        # imm = 0x8000000
	movabsq	$4160749567, %rsi       # imm = 0xF7FFFFFF
	andq	%rdi, %rsi
	orq	%rcx, %rsi
	movl	%esi, (%rax)
	movq	32(%rdx), %rax
	movq	(%rax,%rbx,8), %rax
	movl	(%r14), %ecx
	movl	(%rax), %edi
	andq	$268435456, %rcx        # imm = 0x10000000
	movabsq	$4026531839, %rsi       # imm = 0xEFFFFFFF
	andq	%rdi, %rsi
	orq	%rcx, %rsi
	movl	%esi, (%rax)
	movq	32(%rdx), %rax
	movq	(%rax,%rbx,8), %rax
	movl	(%r14), %ecx
	movl	(%rax), %edi
	andq	$67108864, %rcx         # imm = 0x4000000
	movabsq	$4227858431, %rsi       # imm = 0xFBFFFFFF
	andq	%rdi, %rsi
	orq	%rcx, %rsi
	movl	%esi, (%rax)
	movq	128(%rdx), %rax
	movq	(%rax,%rbx,8), %rax
	movslq	(%rax), %rax
	testq	%rax, %rax
	jns	.LBB7_41
# BB#40:                                # %if.then345
	movq	32(%rdx), %rax
	movq	120(%rdx), %rcx
	movq	(%rax,%rbx,8), %rdi
	movzbl	(%rcx,%rbx), %esi
	movq	%rdx, %r14
	callq	mark_reg_pointer
	movq	%r14, %rdx
.LBB7_41:                               # %if.end353
	movq	32(%rdx), %rax
	movq	(%rax,%rbx,8), %rax
	movl	8(%rax), %ebx
.LBB7_42:                               # %if.end361
	movq	%r12, %rsi
	movl	%ebx, 16(%rsi)
	jmp	.LBB7_132
.LBB7_100:
	movq	%r15, %r8
.LBB7_96:                               # %if.end838
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%r8, %rsi
	callq	copy_rtx_and_substitute
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rsi
	movl	(%rbx), %ecx
	movl	(%rsi), %eax
	andq	$134217728, %rcx        # imm = 0x8000000
	andq	$-134217729, %rax       # imm = 0xFFFFFFFFF7FFFFFF
	orq	%rcx, %rax
	movl	%eax, (%rsi)
	movl	(%rbx), %ecx
	andq	$268435456, %rcx        # imm = 0x10000000
	andq	$-268435457, %rax       # imm = 0xFFFFFFFFEFFFFFFF
	orq	%rcx, %rax
	movl	%eax, (%rsi)
	movl	(%rbx), %ecx
	andq	$-2147483648, %rcx      # imm = 0xFFFFFFFF80000000
	andq	$2147483647, %rax       # imm = 0x7FFFFFFF
	orq	%rcx, %rax
	movl	%eax, (%rsi)
	movl	(%rbx), %ecx
	andq	$67108864, %rcx         # imm = 0x4000000
	andq	$-67108865, %rax        # imm = 0xFFFFFFFFFBFFFFFF
	orq	%rcx, %rax
	movl	%eax, (%rsi)
	movl	(%rbx), %ecx
	andq	$16777216, %rcx         # imm = 0x1000000
	andq	$-16777217, %rax        # imm = 0xFFFFFFFFFEFFFFFF
	orq	%rcx, %rax
	movl	%eax, (%rsi)
	movq	16(%rbx), %rcx
	movq	%rcx, 16(%rsi)
	cmpq	$0, inlining(%rip)
	je	.LBB7_132
# BB#97:                                # %if.end838
	movq	48(%rsp), %rcx          # 8-byte Reload
	testq	%r12, %rcx
	jne	.LBB7_132
# BB#98:                                # %if.then892
	movabsq	$4227858431, %rcx       # imm = 0xFBFFFFFF
	andq	%rcx, %rax
	movl	%eax, (%rsi)
	jmp	.LBB7_132
.LBB7_75:                               # %if.else502
	movzbl	(%r14), %eax
	cmpq	$67, %rax
	movq	%r15, %rsi
	jne	.LBB7_76
# BB#77:                                # %if.then507
	movzbl	2(%rbx), %ebx
	movq	%r14, %rdi
	movq	48(%rsp), %rdx          # 8-byte Reload
	callq	copy_rtx_and_substitute
	movq	%rbx, %rdi
.LBB7_74:                               # %return
	movq	%rax, %rsi
	callq	force_const_mem
	movq	8(%rax), %rsi
	jmp	.LBB7_132
.LBB7_104:                              # %if.then783
	cmpq	$134, %rcx
	je	.LBB7_106
# BB#105:                               # %if.then783
	cmpq	$140, %rcx
	jne	.LBB7_132
.LBB7_106:                              # %if.end835
	movq	%r14, %rdi
	callq	force_const_mem
	movq	%rax, %rdi
	callq	validize_mem
	movq	%rax, %rsi
	jmp	.LBB7_132
.LBB7_62:
	movabsq	$0, %rax
.LBB7_64:                               # %land.rhs452
	movq	%r14, %rdi
	movq	%r15, %rsi
.LBB7_65:                               # %land.end462
	movl	(%rsi), %edx
	movq	$27, %rcx
	movq	%rsi, %rbx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movabsq	$4160749567, %rcx       # imm = 0xF7FFFFFF
	andq	%rdx, %rcx
	orq	%rsi, %rcx
	movq	%rbx, %rsi
	movl	%ecx, (%rsi)
	movzbl	3(%rdi), %ecx
	testq	$8, %rcx
	je	.LBB7_132
# BB#66:                                # %land.end462
	testq	%rax, %rax
	jne	.LBB7_132
# BB#67:                                # %if.then477
	movq	cfun(%rip), %rax
	movl	152(%rax), %ecx
	incq	%rcx
	movl	%ecx, 152(%rax)
	jmp	.LBB7_132
.LBB7_76:
	movq	%rbx, %rsi
	jmp	.LBB7_132
.LBB7_8:                                # %if.else
	cmpq	$53, %rax
	jne	.LBB7_17
# BB#9:                                 # %if.else.if.then91_crit_edge
	movq	8(%r8), %rax
	movq	160(%rax), %rax
	jmp	.LBB7_19
.LBB7_17:                               # %lor.lhs.false80
	movl	(%r8), %eax
	testq	%rax, %rax
	je	.LBB7_23
# BB#18:                                # %land.lhs.true83
	movq	8(%r8), %rax
	movq	160(%rax), %rax
	movq	128(%rax), %rcx
	cmpq	%rbx, %rcx
	je	.LBB7_19
.LBB7_23:                               # %if.else158
	testq	$1073741824, %r12       # imm = 0x40000000
	je	.LBB7_24
# BB#25:                                # %if.then163
	movq	80(%r8), %rsi
	testq	%rsi, %rsi
	je	.LBB7_26
# BB#28:                                # %if.else174
	movzbl	2(%rsi), %eax
	cmpq	$51, %rax
	je	.LBB7_132
# BB#29:                                # %if.else174
	cmpq	%rax, %r14
	je	.LBB7_132
# BB#30:                                # %if.then188
	movq	%r14, %rdi
	callq	gen_lowpart
	movq	%rax, %rsi
	jmp	.LBB7_132
.LBB7_19:                               # %if.then91
	movslq	68(%rax), %rbx
	movq	%r8, %r14
	callq	start_sequence
	movq	$51, %rdi
	movq	$1, %rdx
	movq	%rbx, %rsi
	callq	assign_stack_temp
	movl	target_flags(%rip), %ebx
	movq	8(%rax), %rdi
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	xorq	%rsi, %rsi
	movq	%rdi, %r12
	callq	force_operand
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	force_reg
	movq	32(%r14), %rcx
	movq	%rax, %rbx
	movq	%rbx, (%rcx,%r15,8)
	movq	32(%r14), %rax
	movl	target_flags(%rip), %esi
	movq	(%rax,%r15,8), %rdi
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	callq	mark_reg_pointer
	movq	64(%r14), %r15
	movl	8(%rbx), %esi
	movq	%rbx, %r13
	movq	(%r15), %rax
	cmpq	%rax, %rsi
	jb	.LBB7_22
# BB#20:                                # %if.then127
	movq	global_const_equiv_varray(%rip), %rbx
	incq	%rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	movq	%r15, %rdi
	callq	varray_grow
	cmpq	%r15, %rbx
	movq	%rax, 64(%r14)
	movq	%rax, %r15
	jne	.LBB7_22
# BB#21:                                # %if.then141
	movq	%r15, global_const_equiv_varray(%rip)
	movq	64(%r14), %r15
.LBB7_22:                               # %if.end144
	movq	%r14, %rbx
	movl	8(%r13), %eax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	%r12, 32(%r15,%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, 40(%r15,%rax)
	callq	gen_sequence
	movq	%rax, %r14
	callq	end_sequence
	movq	16(%rbx), %rsi
	movq	%r14, %rdi
	callq	emit_insn_after
	movq	%r13, %rsi
	jmp	.LBB7_132
.LBB7_24:
	movq	%rbx, %rsi
	jmp	.LBB7_132
.LBB7_26:                               # %if.then166
	movl	rtx_equal_function_value_matters(%rip), %eax
	testq	%rax, %rax
	movq	%rbx, %rsi
	je	.LBB7_132
# BB#27:                                # %if.then168
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rsi
.LBB7_132:                              # %return
	movq	%rsi, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB7_82:                               # %if.then548
	movabsq	$.L.str15, %rax
	movabsq	$.L__FUNCTION__.copy_rtx_and_substitute, %rcx
	movq	$2202, %rsi             # imm = 0x89A
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp127:
	.size	copy_rtx_and_substitute, .Ltmp127-copy_rtx_and_substitute
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI7_0:
	.quad	.LBB7_52
	.quad	.LBB7_51
	.quad	.LBB7_107
	.quad	.LBB7_107
	.quad	.LBB7_107
	.quad	.LBB7_83
	.quad	.LBB7_107
	.quad	.LBB7_107
	.quad	.LBB7_107
	.quad	.LBB7_107
	.quad	.LBB7_107
	.quad	.LBB7_86
	.quad	.LBB7_43
	.quad	.LBB7_43
	.quad	.LBB7_85
	.quad	.LBB7_107
	.quad	.LBB7_107
	.quad	.LBB7_107
	.quad	.LBB7_132
	.quad	.LBB7_78
	.quad	.LBB7_132
	.quad	.LBB7_107
	.quad	.LBB7_81
	.quad	.LBB7_132
	.quad	.LBB7_107
	.quad	.LBB7_3
	.quad	.LBB7_107
	.quad	.LBB7_35
	.quad	.LBB7_107
	.quad	.LBB7_107
	.quad	.LBB7_94
	.quad	.LBB7_56
	.quad	.LBB7_68
	.quad	.LBB7_132
	.quad	.LBB7_36
.LJTI7_1:
	.quad	.LBB7_111
	.quad	.LBB7_111
	.quad	.LBB7_124
	.quad	.LBB7_125
	.quad	.LBB7_111

	.text
	.align	16, 0x90
	.type	process_reg_param,@function
process_reg_param:                      # @process_reg_param
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp134:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp135:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp136:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp137:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp138:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp139:
	.cfi_def_cfa_offset 64
.Ltmp140:
	.cfi_offset %rbx, -48
.Ltmp141:
	.cfi_offset %r12, -40
.Ltmp142:
	.cfi_offset %r13, -32
.Ltmp143:
	.cfi_offset %r14, -24
.Ltmp144:
	.cfi_offset %r15, -16
	movq	%rdx, %r13
	movl	(%r13), %eax
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB8_2
# BB#1:
	movq	%r13, %r12
	jmp	.LBB8_20
.LBB8_2:                                # %entry
	cmpq	$61, %rcx
	jne	.LBB8_7
# BB#3:                                 # %land.lhs.true7
	movl	(%r15), %ecx
	testq	$134217728, %rcx        # imm = 0x8000000
	je	.LBB8_5
# BB#4:                                 # %land.lhs.true7
	andq	$134217728, %rax        # imm = 0x8000000
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB8_7
.LBB8_5:                                # %land.lhs.true19
	movl	8(%r13), %eax
	cmpq	$52, %rax
	ja	.LBB8_6
.LBB8_7:                                # %if.then
	movzbl	2(%r15), %edi
	movq	%r13, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %r12
	movl	(%r15), %eax
	movl	(%r12), %ecx
	andq	$134217728, %rax        # imm = 0x8000000
	movabsq	$4160749567, %rdx       # imm = 0xF7FFFFFF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, (%r12)
	movzbl	(%r13), %ecx
	cmpq	$133, %rcx
	jg	.LBB8_14
# BB#8:                                 # %if.then
	addq	$-54, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$21, %rcx
	ja	.LBB8_20
# BB#9:                                 # %if.then
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB8_16
# BB#10:                                # %if.then
	cmpq	$21, %rcx
	jne	.LBB8_20
# BB#11:                                # %land.lhs.true64
	movq	16(%r13), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$54, %rcx
	jne	.LBB8_20
# BB#12:                                # %land.lhs.true70
	movq	8(%r13), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB8_20
# BB#13:                                # %land.lhs.true77
	movl	8(%rcx), %ecx
	addq	$-53, %rcx
	andq	%rax, %rcx
	cmpq	$4, %rcx
	jbe	.LBB8_16
	jmp	.LBB8_20
.LBB8_14:                               # %if.then
	cmpq	$134, %rcx
	je	.LBB8_16
# BB#15:                                # %if.then
	cmpq	$140, %rcx
	jne	.LBB8_20
.LBB8_16:                               # %if.then93
	movq	64(%r14), %rbx
	movl	8(%r12), %eax
	movq	(%rbx), %rcx
	cmpq	%rcx, %rax
	jb	.LBB8_19
# BB#17:                                # %if.then99
	movq	global_const_equiv_varray(%rip), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	incq	%rax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	movq	%rbx, %rdi
	callq	varray_grow
	cmpq	%rbx, 8(%rsp)           # 8-byte Folded Reload
	movq	%rax, 64(%r14)
	movq	%rax, %rbx
	jne	.LBB8_19
# BB#18:                                # %if.then111
	movq	%rbx, global_const_equiv_varray(%rip)
	movq	64(%r14), %rbx
.LBB8_19:                               # %if.end113
	movl	8(%r12), %eax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	%r13, 32(%rbx,%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, 40(%rbx,%rax)
	jmp	.LBB8_20
.LBB8_6:
	movq	%r13, %r12
.LBB8_20:                               # %if.end121
	movl	8(%r15), %eax
	movq	32(%r14), %rcx
	movq	%r12, (%rcx,%rax,8)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp145:
	.size	process_reg_param, .Ltmp145-process_reg_param
	.cfi_endproc

	.align	16, 0x90
	.type	subst_constants,@function
subst_constants:                        # @subst_constants
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp152:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp153:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp154:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp155:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp156:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp157:
	.cfi_def_cfa_offset 112
.Ltmp158:
	.cfi_offset %rbx, -48
.Ltmp159:
	.cfi_offset %r12, -40
.Ltmp160:
	.cfi_offset %r13, -32
.Ltmp161:
	.cfi_offset %r14, -24
.Ltmp162:
	.cfi_offset %r15, -16
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	(%rdi), %rbx
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	callq	num_validated_changes
	movl	(%rbx), %ecx
	movq	%rax, %r14
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$46, %rdx
	jg	.LBB9_9
# BB#1:                                 # %entry
	cmpq	$25, %rdx
	je	.LBB9_128
	jmp	.LBB9_2
.LBB9_9:                                # %entry
	leaq	-47(%rdx), %rax
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rax
	cmpq	$21, %rax
	ja	.LBB9_2
# BB#10:                                # %entry
	jmpq	*.LJTI9_0(,%rax,8)
.LBB9_11:                               # %sw.bb1
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdi
	movzbl	(%rdi), %eax
	cmpq	$66, %rax
	jne	.LBB9_128
# BB#12:                                # %if.then
	addq	$8, %rdi
	xorq	%rcx, %rcx
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	subst_constants
	jmp	.LBB9_128
.LBB9_28:                               # %sw.bb91
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx), %rax
	movl	(%rax), %edx
	movq	8(%rcx), %rbx
	movq	%rcx, %rsi
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	xorq	%rdi, %rdi
	cmpq	$74, %rcx
	jne	.LBB9_32
# BB#29:                                # %if.then102
	movq	$14, %rcx
	shrq	%cl, %rdx
	andq	$1020, %rdx             # imm = 0x3FC
	movl	mode_class(%rdx), %ecx
	cmpq	$4, %rcx
	jne	.LBB9_32
# BB#30:                                # %if.then113
	movq	8(%rax), %rcx
	movzbl	2(%rcx), %edi
	testq	%rdi, %rdi
	jne	.LBB9_32
# BB#31:                                # %if.then122
	movq	16(%rax), %rax
	movzbl	2(%rax), %edi
.LBB9_32:                               # %if.end131
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	%rsi, %rbx
	leaq	8(%rbx), %r12
	addq	$16, %rbx
	movq	%rbx, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	subst_constants
	movq	(%rbx), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	40(%rsp), %r14          # 8-byte Reload
	jmp	.LBB9_33
	.align	16, 0x90
.LBB9_37:                               # %if.end161
                                        #   in Loop: Header=BB9_33 Depth=1
	addq	$8, %rdi
	movq	%rdi, %r12
.LBB9_33:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r12), %rdi
	movzbl	(%rdi), %eax
	leaq	-63(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$2, %rcx
	jb	.LBB9_37
# BB#34:                                # %while.cond
                                        #   in Loop: Header=BB9_33 Depth=1
	cmpq	$66, %rax
	je	.LBB9_38
# BB#35:                                # %while.cond
                                        #   in Loop: Header=BB9_33 Depth=1
	cmpq	$133, %rax
	jne	.LBB9_39
# BB#36:                                # %if.then154
                                        #   in Loop: Header=BB9_33 Depth=1
	addq	$16, %rdi
	movq	%r13, %r15
	movq	32(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	subst_constants
	movq	(%r12), %rdi
	addq	$24, %rdi
	movq	%r13, %rsi
	movq	%r15, %r13
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	subst_constants
	movq	(%r12), %rdi
	jmp	.LBB9_37
.LBB9_13:                               # %sw.bb10
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB9_128
# BB#14:                                # %if.then11
	movq	48(%rsp), %rax          # 8-byte Reload
	movslq	8(%rax), %rax
	cmpq	$52, %rax
	jg	.LBB9_16
# BB#15:                                # %if.then11
	andq	$134217728, %rcx        # imm = 0x8000000
	testq	%r13, %rcx
	jne	.LBB9_128
.LBB9_16:                               # %land.lhs.true18
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	64(%rcx), %rsi
	movq	(%rsi), %rcx
	cmpq	%rcx, %rax
	jae	.LBB9_128
# BB#17:                                # %land.lhs.true21
	movq	$4, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	32(%rsi,%rdx), %rdx
	testq	%rdx, %rdx
	je	.LBB9_128
# BB#18:                                # %land.lhs.true27
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	40(%rsi,%rax), %eax
	movq	24(%rsp), %rcx          # 8-byte Reload
	movl	72(%rcx), %ecx
	cmpq	%rcx, %rax
	jb	.LBB9_128
# BB#19:                                # %if.then30
	movq	$1, %rcx
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB9_127
.LBB9_20:                               # %sw.bb35
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB9_2
# BB#21:                                # %land.lhs.true37
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB9_2
# BB#22:                                # %if.then45
	movq	%rax, 56(%rsp)
	leaq	56(%rsp), %rdi
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	subst_constants
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	8(%rdx), %rax
	movq	56(%rsp), %rsi
	movl	16(%rdx), %ecx
	movzbl	2(%rdx), %edi
	movzbl	2(%rax), %edx
	callq	simplify_gen_subreg
	testq	%rax, %rax
	jne	.LBB9_126
	jmp	.LBB9_23
.LBB9_2:                                # %sw.epilog
	movq	rtx_format(,%rdx,8), %rbx
	movzbl	(%rbx), %eax
	cmpq	$101, %rax
	movq	$59, %rcx
	jne	.LBB9_4
# BB#3:                                 # %if.then462
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	movzbl	2(%rcx), %ecx
.LBB9_4:                                # %if.end469
	movq	%rcx, (%rsp)            # 8-byte Spill
	movzbl	rtx_length(%rdx), %r14d
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	testq	%r14, %r14
	je	.LBB9_84
# BB#5:
	xorq	%r13, %r13
	jmp	.LBB9_6
	.align	16, 0x90
.LBB9_83:                               # %for.inc522.for.body_crit_edge
                                        #   in Loop: Header=BB9_6 Depth=1
	movzbl	(%rbx), %eax
.LBB9_6:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_81 Depth 2
	incq	%rbx
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	cmpq	$68, %rax
	jg	.LBB9_72
# BB#7:                                 # %for.body
                                        #   in Loop: Header=BB9_6 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$48, %rax
	je	.LBB9_82
	jmp	.LBB9_8
	.align	16, 0x90
.LBB9_72:                               # %for.body
                                        #   in Loop: Header=BB9_6 Depth=1
	addq	$-69, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$50, %rax
	ja	.LBB9_8
# BB#73:                                # %for.body
                                        #   in Loop: Header=BB9_6 Depth=1
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	movabsq	$1620748858818560, %rcx # imm = 0x5C21000000000
	testq	%rcx, %rdx
	jne	.LBB9_82
# BB#74:                                # %for.body
                                        #   in Loop: Header=BB9_6 Depth=1
	testq	%rax, %rax
	jne	.LBB9_75
# BB#78:                                # %sw.bb489
                                        #   in Loop: Header=BB9_6 Depth=1
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	8(%rax,%r13,8), %rdi
	testq	%rdi, %rdi
	je	.LBB9_82
# BB#79:                                # %land.lhs.true495
                                        #   in Loop: Header=BB9_6 Depth=1
	movslq	(%rdi), %rax
	testq	%rax, %rax
	jle	.LBB9_82
# BB#80:                                #   in Loop: Header=BB9_6 Depth=1
	movq	$8, %r12
	movq	$1, %r15
	.align	16, 0x90
.LBB9_81:                               # %for.body511
                                        #   Parent Loop BB9_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	addq	%r12, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	subst_constants
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	8(%rax,%r13,8), %rdi
	movslq	(%rdi), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r15
	addq	$8, %r12
	cmpq	%rax, %rdx
	jl	.LBB9_81
	jmp	.LBB9_82
.LBB9_75:                               # %for.body
                                        #   in Loop: Header=BB9_6 Depth=1
	cmpq	$32, %rax
	jne	.LBB9_8
# BB#76:                                # %sw.bb476
                                        #   in Loop: Header=BB9_6 Depth=1
	movq	48(%rsp), %rax          # 8-byte Reload
	cmpq	$0, 8(%rax,%r13,8)
	je	.LBB9_82
# BB#77:                                # %if.then482
                                        #   in Loop: Header=BB9_6 Depth=1
	movq	48(%rsp), %rax          # 8-byte Reload
	leaq	8(%rax,%r13,8), %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	subst_constants
	.align	16, 0x90
.LBB9_82:                               # %for.inc522
                                        #   in Loop: Header=BB9_6 Depth=1
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r14, %rax
	jl	.LBB9_83
.LBB9_84:                               # %for.end524
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB9_128
# BB#85:                                # %land.lhs.true526
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	rtx_class(%rax), %ebx
	cmpq	$99, %rbx
	je	.LBB9_88
# BB#86:                                # %land.lhs.true526
	cmpq	$102, 8(%rsp)           # 8-byte Folded Reload
	je	.LBB9_88
# BB#87:                                # %land.lhs.true526
	cmpq	$103, 8(%rsp)           # 8-byte Folded Reload
	jne	.LBB9_95
.LBB9_88:                               # %land.lhs.true538
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %r14
	movzbl	(%r14), %ecx
	leaq	8(%rax), %rsi
	cmpq	$133, %rcx
	jg	.LBB9_91
# BB#89:                                # %land.lhs.true538
	addq	$-54, %rcx
	andq	%r13, %rcx
	cmpq	$14, %rcx
	ja	.LBB9_95
# BB#90:                                # %land.lhs.true538
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB9_93
	jmp	.LBB9_95
.LBB9_8:                                # %sw.default520
	movabsq	$.L.str15, %rdi
	movabsq	$.L__FUNCTION__.subst_constants, %rdx
	movq	$2710, %rsi             # imm = 0xA96
	callq	fancy_abort
.LBB9_91:                               # %land.lhs.true538
	cmpq	$134, %rcx
	je	.LBB9_93
# BB#92:                                # %land.lhs.true538
	cmpq	$140, %rcx
	jne	.LBB9_95
.LBB9_93:                               # %land.lhs.true602
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rdx
	movzbl	(%rdx), %eax
	cmpq	$54, %rax
	je	.LBB9_95
# BB#94:                                # %if.then610
	movq	48(%rsp), %rax          # 8-byte Reload
	leaq	16(%rax), %r15
	movq	$1, %rcx
	movq	32(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rdi
	callq	validate_change
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	validate_change
.LBB9_95:                               # %if.then628
	movq	$56, %rcx
	shlq	%cl, %rbx
	movq	$56, %rcx
	sarq	%cl, %rbx
	cmpq	$97, %rbx
	jg	.LBB9_102
# BB#96:                                # %if.then628
	cmpq	$59, %rbx
	jg	.LBB9_109
# BB#97:                                # %if.then628
	andq	%r13, %rbx
	cmpq	$49, %rbx
	je	.LBB9_105
# BB#98:                                # %if.then628
	cmpq	$50, %rbx
	je	.LBB9_104
# BB#99:                                # %if.then628
	cmpq	$51, %rbx
	jne	.LBB9_128
	jmp	.LBB9_100
.LBB9_102:                              # %if.then628
	andq	%r13, %rbx
	cmpq	$98, %rbx
	jne	.LBB9_103
.LBB9_100:                              # %sw.bb679
	cmpq	$59, (%rsp)             # 8-byte Folded Reload
	je	.LBB9_101
# BB#113:                               # %if.end683
	cmpq	$72, 8(%rsp)            # 8-byte Folded Reload
	jne	.LBB9_124
# BB#114:                               # %if.then686
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %r14
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$60, %rcx
	jne	.LBB9_124
# BB#115:                               # %if.then686
	andq	$16711680, %rax         # imm = 0xFF0000
	testq	%r13, %rax
	jne	.LBB9_124
# BB#116:                               # %land.lhs.true703
	movq	%r14, %rdi
	callq	side_effects_p
	testq	%r13, %rax
	jne	.LBB9_124
# BB#117:                               # %land.lhs.true706
	movq	8(%r14), %rdx
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	632(%rax), %rax
	cmpq	%rax, %rdx
	jne	.LBB9_124
# BB#118:                               # %land.lhs.true713
	movq	16(%r14), %rcx
	movzbl	2(%rcx), %eax
	testq	%rax, %rax
	jne	.LBB9_124
# BB#119:                               # %if.then722
	movq	24(%rsp), %rax          # 8-byte Reload
	movl	640(%rax), %esi
	movzbl	(%r14), %edi
	callq	simplify_relational_operation
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB9_120
# BB#121:                               # %if.else739
	movq	const_int_rtx+520(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB9_124
# BB#122:                               # %if.then742
	movq	48(%rsp), %rax          # 8-byte Reload
	leaq	16(%rax), %rax
	jmp	.LBB9_123
.LBB9_109:                              # %if.then628
	andq	%r13, %rbx
	cmpq	$60, %rbx
	jne	.LBB9_128
# BB#110:                               # %sw.bb644
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdx
	movzbl	2(%rdx), %esi
	movq	16(%rax), %rcx
	testq	%rsi, %rsi
	jne	.LBB9_112
# BB#111:                               # %if.then653
	movzbl	2(%rcx), %esi
.LBB9_112:                              # %if.end660
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	simplify_relational_operation
	jmp	.LBB9_125
.LBB9_103:                              # %if.then628
	cmpq	$99, %rbx
	jne	.LBB9_128
.LBB9_104:                              # %sw.bb668
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdx
	movq	16(%rax), %rcx
	movzbl	2(%rax), %esi
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	simplify_binary_operation
	jmp	.LBB9_125
.LBB9_24:                               # %sw.bb68
	movq	48(%rsp), %rax          # 8-byte Reload
	leaq	8(%rax), %rdi
	xorq	%rcx, %rcx
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	subst_constants
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB9_128
# BB#25:                                # %sw.bb68
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	je	.LBB9_128
# BB#26:                                # %land.lhs.true76
	callq	num_validated_changes
	andq	%r13, %rax
	movq	%r14, %rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	je	.LBB9_128
# BB#27:                                # %land.lhs.true80
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rsi
	movzbl	2(%rax), %edi
	callq	memory_address_p
	testq	%r13, %rax
	jne	.LBB9_128
.LBB9_23:                               # %if.else
	movq	%r14, %rdi
	callq	cancel_changes
	jmp	.LBB9_128
.LBB9_105:                              # %sw.bb632
	movq	(%rsp), %rcx            # 8-byte Reload
	cmpq	$59, %rcx
	je	.LBB9_106
# BB#108:                               # %if.end636
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdx
	movzbl	2(%rax), %esi
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	simplify_unary_operation
	jmp	.LBB9_125
.LBB9_38:                               # %if.then169
	addq	$8, %rdi
	xorq	%rcx, %rcx
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	subst_constants
.LBB9_39:                               # %if.end173
	movq	16(%rsp), %rbx          # 8-byte Reload
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	movq	48(%rsp), %r14          # 8-byte Reload
	jne	.LBB9_45
# BB#40:                                # %land.lhs.true178
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	target_flags(%rip), %edx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	addq	$4, %rdx
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	ja	.LBB9_45
# BB#41:                                # %land.lhs.true188
	movq	8(%rbx), %rcx
	movzbl	2(%rcx), %edi
	movzbl	mode_size(%rdi), %ecx
	andq	%r13, %rdx
	cmpq	%rdx, %rcx
	ja	.LBB9_45
# BB#42:                                # %land.lhs.true188
	cmpq	%rax, %rcx
	ja	.LBB9_45
# BB#43:                                # %land.lhs.true221
	movq	%r14, %rsi
	callq	gen_lowpart_if_possible
	testq	%rax, %rax
	je	.LBB9_45
# BB#44:                                # %if.then230
	movq	8(%rbx), %rbx
	movq	%rax, %r14
.LBB9_45:                               # %if.end234
	movq	24(%rsp), %rax          # 8-byte Reload
	movslq	136(%rax), %rax
	cmpq	$29, %rax
	jg	.LBB9_128
# BB#46:                                # %land.lhs.true237
	movzbl	(%r14), %eax
	cmpq	$133, %rax
	jg	.LBB9_56
# BB#47:                                # %land.lhs.true237
	leaq	-54(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$21, %rcx
	ja	.LBB9_62
# BB#48:                                # %land.lhs.true237
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$1073175, %rdx          # imm = 0x106017
	jne	.LBB9_66
# BB#49:                                # %land.lhs.true237
	cmpq	$7, %rcx
	jne	.LBB9_50
# BB#58:                                # %land.lhs.true282
	movl	8(%r14), %ecx
	addq	$-53, %rcx
	andq	%r13, %rcx
	cmpq	$2, %rcx
	jae	.LBB9_62
	jmp	.LBB9_66
.LBB9_56:                               # %land.lhs.true237
	cmpq	$134, %rax
	je	.LBB9_66
# BB#57:                                # %land.lhs.true237
	cmpq	$140, %rax
	jne	.LBB9_62
	jmp	.LBB9_66
.LBB9_120:                              # %if.then735
	movq	48(%rsp), %rax          # 8-byte Reload
	leaq	24(%rax), %rax
.LBB9_123:                              # %if.end749
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB9_126
.LBB9_124:                              # %if.then751
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rcx
	movq	16(%rax), %r8
	movq	24(%rax), %r9
	movzbl	2(%rax), %esi
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	(%rsp), %rdx            # 8-byte Reload
	callq	simplify_ternary_operation
.LBB9_125:                              # %if.end767
	testq	%rax, %rax
	je	.LBB9_128
.LBB9_126:                              # %if.then769
	movq	$1, %rcx
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
.LBB9_127:                              # %if.end771
	callq	validate_change
.LBB9_128:                              # %if.end771
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB9_50:                               # %land.lhs.true237
	cmpq	$21, %rcx
	jne	.LBB9_62
# BB#51:                                # %land.lhs.true299
	movq	8(%r14), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB9_62
# BB#52:                                # %land.lhs.true307
	movl	8(%rcx), %ecx
	addq	$-53, %rcx
	andq	%r13, %rcx
	cmpq	$1, %rcx
	ja	.LBB9_61
# BB#53:                                # %land.lhs.true325
	movq	%rbx, %rsi
	movq	%r13, %rdx
	movq	16(%r14), %rcx
	movq	%r14, %rdi
	movzbl	(%rcx), %ecx
	cmpq	$133, %rcx
	jg	.LBB9_59
# BB#54:                                # %land.lhs.true325
	addq	$-54, %rcx
	andq	%rdx, %rcx
	cmpq	$14, %rcx
	movq	%rsi, %rbx
	movq	%rdi, %r14
	ja	.LBB9_61
# BB#55:                                # %land.lhs.true325
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB9_66
	jmp	.LBB9_61
.LBB9_101:                              # %if.then682
	movabsq	$.L.str15, %rax
	movabsq	$.L__FUNCTION__.subst_constants, %rcx
	movq	$2772, %rsi             # imm = 0xAD4
	jmp	.LBB9_107
.LBB9_106:                              # %if.then635
	movabsq	$.L.str15, %rax
	movabsq	$.L__FUNCTION__.subst_constants, %rcx
	movq	$2731, %rsi             # imm = 0xAAB
.LBB9_107:                              # %if.then635
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB9_59:                               # %land.lhs.true325
	cmpq	$134, %rcx
	movq	%rsi, %rbx
	movq	%rdi, %r14
	je	.LBB9_66
# BB#60:                                # %land.lhs.true325
	cmpq	$140, %rcx
	je	.LBB9_66
.LBB9_61:                               # %lor.lhs.false389
	cmpq	$74, %rax
	je	.LBB9_66
.LBB9_62:                               # %lor.lhs.false394
	movq	global_rtl(%rip), %rcx
	cmpq	%rcx, %rbx
	jne	.LBB9_128
# BB#63:                                # %land.lhs.true397
	cmpq	%rbx, %r14
	je	.LBB9_66
# BB#64:                                # %switch.early.test
	cmpq	$51, %rax
	je	.LBB9_66
# BB#65:                                # %switch.early.test
	cmpq	$67, %rax
	jne	.LBB9_128
.LBB9_66:                               # %if.then410
	movq	%r14, %rdi
	callq	copy_rtx
	movq	24(%rsp), %rsi          # 8-byte Reload
	movslq	136(%rsi), %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	%rax, 152(%rsi,%rdx)
	movslq	136(%rsi), %rdx
	leaq	1(%rdx), %rcx
	movl	%ecx, 136(%rsi)
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	%rbx, 144(%rsi,%rdx)
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	je	.LBB9_128
# BB#67:                                # %land.lhs.true422
	movl	(%r14), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$74, %rcx
	jne	.LBB9_128
# BB#68:                                # %land.lhs.true427
	movq	$14, %rcx
	shrq	%cl, %rdx
	andq	$1020, %rdx             # imm = 0x3FC
	movl	mode_class(%rdx), %ecx
	cmpq	$4, %rcx
	jne	.LBB9_128
# BB#69:                                # %land.lhs.true435
	movq	8(%r14), %rcx
	movzbl	2(%rcx), %ecx
	testq	%rcx, %rcx
	jne	.LBB9_128
# BB#70:                                # %land.lhs.true444
	movq	16(%r14), %rcx
	movzbl	2(%rcx), %ecx
	testq	%rcx, %rcx
	jne	.LBB9_128
# BB#71:                                # %if.then453
	movq	%rax, 632(%rsi)
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	%eax, 640(%rsi)
	jmp	.LBB9_128
.Ltmp163:
	.size	subst_constants, .Ltmp163-subst_constants
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI9_0:
	.quad	.LBB9_28
	.quad	.LBB9_11
	.quad	.LBB9_11
	.quad	.LBB9_2
	.quad	.LBB9_2
	.quad	.LBB9_2
	.quad	.LBB9_2
	.quad	.LBB9_128
	.quad	.LBB9_128
	.quad	.LBB9_128
	.quad	.LBB9_2
	.quad	.LBB9_128
	.quad	.LBB9_128
	.quad	.LBB9_2
	.quad	.LBB9_13
	.quad	.LBB9_2
	.quad	.LBB9_20
	.quad	.LBB9_2
	.quad	.LBB9_2
	.quad	.LBB9_24
	.quad	.LBB9_128
	.quad	.LBB9_128

	.text
	.align	16, 0x90
	.type	integrate_decl_tree,@function
integrate_decl_tree:                    # @integrate_decl_tree
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp170:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp171:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp172:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp173:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp174:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp175:
	.cfi_def_cfa_offset 64
.Ltmp176:
	.cfi_offset %rbx, -48
.Ltmp177:
	.cfi_offset %r12, -40
.Ltmp178:
	.cfi_offset %r13, -32
.Ltmp179:
	.cfi_offset %r14, -24
.Ltmp180:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	%rdi, %r14
	movq	$4, %rdi
	callq	make_node
	movq	24(%r12), %rcx
	movq	(%rcx), %rsi
	movq	8(%rcx), %rdx
	movq	%rax, %r15
	cmpq	%rsi, %rdx
	jb	.LBB10_2
# BB#1:                                 # %if.then
	addq	%rsi, %rsi
	movq	%rcx, %rdi
	callq	varray_grow
	movq	%rax, %rcx
	movq	%rcx, 24(%r12)
	movq	8(%rcx), %rdx
.LBB10_2:                               # %if.end
	leaq	1(%rdx), %rax
	movq	%rax, 8(%rcx)
	movq	%r15, 32(%rcx,%rdx,8)
	movq	32(%r14), %rbx
	movq	%r14, (%rsp)            # 8-byte Spill
	testq	%rbx, %rbx
	je	.LBB10_14
# BB#3:                                 # %for.body.lr.ph
	leaq	32(%r15), %r13
	.align	16, 0x90
.LBB10_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r12), %rsi
	movq	current_function_decl(%rip), %rdx
	movq	%rbx, %rdi
	callq	copy_decl_for_inlining
	movq	144(%rbx), %rdi
	movq	%rax, %r14
	testq	%rdi, %rdi
	je	.LBB10_13
# BB#5:                                 # %cond.end
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	$1, %rdx
	movq	%r12, %rsi
	callq	copy_rtx_and_substitute
	testq	%rax, %rax
	movq	%rax, 144(%r14)
	jne	.LBB10_7
# BB#6:                                 # %cond.false31
                                        #   in Loop: Header=BB10_4 Depth=1
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	make_decl_rtl
	movq	144(%r14), %rax
.LBB10_7:                               # %cond.end34
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	%rax, 8(%rsp)
	xorq	%rsi, %rsi
	movq	$1, %rcx
	leaq	8(%rsp), %rdi
	movq	%r12, %rdx
	callq	subst_constants
	movq	8(%rsp), %rax
	movq	%rax, 144(%r14)
	movq	8(%rsp), %rax
	movzbl	(%rax), %ecx
	cmpq	$65, %rcx
	jne	.LBB10_8
# BB#10:                                # %if.then45
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	cfun(%rip), %rcx
	movq	8(%rax), %rax
	movq	24(%rcx), %rcx
	movl	8(%rax), %eax
	movq	72(%rcx), %rcx
	movq	%r14, (%rcx,%rax,8)
	movq	8(%rsp), %rax
	movq	cfun(%rip), %rcx
	movq	16(%rax), %rax
	jmp	.LBB10_11
	.align	16, 0x90
.LBB10_8:                               # %cond.end34
                                        #   in Loop: Header=BB10_4 Depth=1
	cmpq	$61, %rcx
	jne	.LBB10_12
# BB#9:                                 # %if.then39
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	cfun(%rip), %rcx
.LBB10_11:                              # %if.end66
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	24(%rcx), %rcx
	movl	8(%rax), %eax
	movq	72(%rcx), %rcx
	movq	%r14, (%rcx,%rax,8)
.LBB10_12:                              # %if.end66
                                        #   in Loop: Header=BB10_4 Depth=1
	callq	apply_change_group
.LBB10_13:                              # %if.end68
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	%r14, (%r13)
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	movq	%r14, %r13
	jne	.LBB10_4
.LBB10_14:                              # %for.end
	movq	(%rsp), %r13            # 8-byte Reload
	movq	40(%r13), %rbx
	testq	%rbx, %rbx
	je	.LBB10_17
# BB#15:                                # %for.body76.lr.ph
	leaq	40(%r15), %r14
	.align	16, 0x90
.LBB10_16:                              # %for.body76
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	integrate_decl_tree
	movq	%rax, (%r14)
	movq	%r15, 48(%rax)
	movq	(%rbx), %rbx
	movq	(%r14), %r14
	testq	%rbx, %rbx
	jne	.LBB10_16
.LBB10_17:                              # %for.end84
	movl	16(%r13), %eax
	movl	16(%r15), %ecx
	andq	$65536, %rax            # imm = 0x10000
	movabsq	$4294901759, %rdx       # imm = 0xFFFEFFFF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 16(%r15)
	movq	%r13, 56(%r15)
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp181:
	.size	integrate_decl_tree, .Ltmp181-integrate_decl_tree
	.cfi_endproc

	.align	16, 0x90
	.type	compare_blocks,@function
compare_blocks:                         # @compare_blocks
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	movq	(%rsi), %rcx
	movq	56(%rax), %rdx
	movq	56(%rcx), %rcx
	xorq	%rax, %rax
	cmpq	%rcx, %rdx
	je	.LBB11_3
# BB#1:                                 # %if.end
	movq	$-1, %rax
	jb	.LBB11_3
# BB#2:                                 # %if.end
	movq	$1, %rax
.LBB11_3:                               # %return
	retq
.Ltmp182:
	.size	compare_blocks, .Ltmp182-compare_blocks
	.cfi_endproc

	.align	16, 0x90
	.type	copy_insn_list,@function
copy_insn_list:                         # @copy_insn_list
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp189:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp190:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp191:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp192:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp193:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp194:
	.cfi_def_cfa_offset 80
.Ltmp195:
	.cfi_offset %rbx, -48
.Ltmp196:
	.cfi_offset %r12, -40
.Ltmp197:
	.cfi_offset %r13, -32
.Ltmp198:
	.cfi_offset %r14, -24
.Ltmp199:
	.cfi_offset %r15, -16
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, %r14
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	%rdi, %r15
	testq	%r15, %r15
	je	.LBB12_112
# BB#1:                                 # %for.body.lr.ph
	movq	$0, (%rsp)              # 8-byte Folded Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB12_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB12_78 Depth 2
                                        #     Child Loop BB12_80 Depth 2
	movq	$0, 88(%r14)
	movzbl	(%r15), %eax
	addq	$-32, %rax
	andq	%r13, %rax
	cmpq	$5, %rax
	ja	.LBB12_106
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB12_2 Depth=1
	jmpq	*.LJTI12_0(,%rax,8)
.LBB12_4:                               # %sw.bb
                                        #   in Loop: Header=BB12_2 Depth=1
	movzbl	rtx_class+32(%rip), %eax
	movq	32(%r15), %r14
	xorq	%r13, %r13
	cmpq	$105, %rax
	jne	.LBB12_7
# BB#5:                                 # %cond.true
                                        #   in Loop: Header=BB12_2 Depth=1
	movzbl	(%r14), %eax
	cmpq	$47, %rax
	movq	%r14, %r13
	je	.LBB12_7
# BB#6:                                 # %cond.false
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	single_set_2
	movq	%rax, %r13
.LBB12_7:                               # %cond.end20
                                        #   in Loop: Header=BB12_2 Depth=1
	movzbl	(%r14), %eax
	cmpq	$48, %rax
	jne	.LBB12_10
# BB#8:                                 # %land.lhs.true
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	8(%r14), %rcx
	movl	(%rcx), %ecx
	andq	$1073807359, %rcx       # imm = 0x4000FFFF
	xorq	%r12, %r12
	cmpq	$1073741885, %rcx       # imm = 0x4000003D
	je	.LBB12_9
.LBB12_10:                              # %if.end
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	80(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB12_26
# BB#11:                                # %if.end
                                        #   in Loop: Header=BB12_2 Depth=1
	testq	%r13, %r13
	je	.LBB12_26
# BB#12:                                # %land.lhs.true45
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	8(%r13), %rdx
	movl	(%rdx), %edx
	andq	$1073807359, %rdx       # imm = 0x4000FFFF
	cmpq	$1073741885, %rdx       # imm = 0x4000003D
	jne	.LBB12_26
# BB#13:                                # %if.then61
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	16(%r13), %rdi
	callq	volatile_refs_p
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	jne	.LBB12_14
# BB#21:                                # %if.else
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	8(%r13), %rdi
	movq	16(%r13), %rsi
	callq	rtx_equal_p
	xorq	%r12, %r12
	testq	%rbx, %rax
	movq	%rbx, %r13
	jne	.LBB12_23
# BB#22:                                #   in Loop: Header=BB12_2 Depth=1
	movq	24(%rsp), %r14          # 8-byte Reload
	jmp	.LBB12_111
.LBB12_58:                              # %sw.bb329
                                        #   in Loop: Header=BB12_2 Depth=1
	movl	(%r14), %eax
	testq	%rax, %rax
	je	.LBB12_63
# BB#59:                                # %land.lhs.true331
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%r15, %rdi
	callq	returnjump_p
	testq	%r13, %rax
	jne	.LBB12_60
.LBB12_63:                              # %if.else343
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	32(%r15), %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	copy_rtx_and_substitute
	jmp	.LBB12_64
.LBB12_69:                              # %sw.bb367
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	32(%r15), %rdi
	movzbl	(%rdi), %eax
	cmpq	$141, %rax
	jne	.LBB12_77
# BB#70:                                # %for.cond376.preheader
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	8(%rdi), %r13
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	testq	%r13, %r13
	movabsq	$0, %r12
	je	.LBB12_72
# BB#71:                                # %if.then390
                                        #   in Loop: Header=BB12_2 Depth=1
	callq	start_sequence
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	16(%rsp), %rdx          # 8-byte Reload
	callq	copy_insn_list
	callq	get_insns
	movq	%rax, %r12
	callq	end_sequence
	movq	32(%r15), %rdi
.LBB12_72:                              # %for.inc
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%r14, %rbx
	movq	16(%rdi), %r14
	testq	%r14, %r14
	je	.LBB12_74
# BB#73:                                # %if.then390.1
                                        #   in Loop: Header=BB12_2 Depth=1
	callq	start_sequence
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	16(%rsp), %rdx          # 8-byte Reload
	callq	copy_insn_list
	callq	get_insns
	movq	%rax, 8(%rsp)           # 8-byte Spill
	callq	end_sequence
	movq	32(%r15), %rdi
.LBB12_74:                              # %for.inc.1
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	24(%rdi), %r14
	xorq	%r13, %r13
	testq	%r14, %r14
	je	.LBB12_76
# BB#75:                                # %if.then390.2
                                        #   in Loop: Header=BB12_2 Depth=1
	callq	start_sequence
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	16(%rsp), %rdx          # 8-byte Reload
	callq	copy_insn_list
	callq	get_insns
	movq	%rax, %r13
	callq	end_sequence
	movq	32(%r15), %rdi
.LBB12_76:                              # %for.inc.2
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	32(%rdi), %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %r14
	movq	%r14, %rsi
	callq	copy_rtx_and_substitute
	movq	$141, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%r13, %r8
	movq	%rax, %r9
	callq	gen_rtx_fmt_uuuu
	movq	%rax, %rdi
	callq	emit_call_insn
	movq	%rax, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jmp	.LBB12_108
.LBB12_85:                              # %sw.bb457
                                        #   in Loop: Header=BB12_2 Depth=1
	callq	emit_barrier
	movq	%rax, %r12
	jmp	.LBB12_108
.LBB12_81:                              # %sw.bb446
                                        #   in Loop: Header=BB12_2 Depth=1
	movslq	48(%r15), %rbx
	movq	40(%r14), %rax
	movq	(%rax,%rbx,8), %rdi
	testq	%rdi, %rdi
	jne	.LBB12_83
# BB#82:                                # %if.then.i
                                        #   in Loop: Header=BB12_2 Depth=1
	callq	gen_label_rtx
	movq	40(%r14), %rcx
	movq	%rax, %rdi
	movq	%rdi, (%rcx,%rbx,8)
.LBB12_83:                              # %get_label_from_map.exit
                                        #   in Loop: Header=BB12_2 Depth=1
	callq	emit_label
	movq	56(%r15), %rcx
.LBB12_84:                              # %sw.epilog
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%rax, %r12
	movq	%rcx, 56(%r12)
	movl	72(%r14), %eax
	incq	%rax
	movl	%eax, 72(%r14)
	jmp	.LBB12_108
.LBB12_86:                              # %sw.bb459
                                        #   in Loop: Header=BB12_2 Depth=1
	movslq	40(%r15), %rsi
	movq	%rsi, %rax
	andq	%r13, %rax
	cmpq	$-92, %rsi
	jg	.LBB12_89
# BB#87:                                # %sw.bb459
                                        #   in Loop: Header=BB12_2 Depth=1
	movabsq	$4294967197, %rcx       # imm = 0xFFFFFF9D
	cmpq	%rcx, %rax
	jne	.LBB12_97
# BB#88:                                #   in Loop: Header=BB12_2 Depth=1
	xorq	%r12, %r12
	jmp	.LBB12_111
.LBB12_26:                              # %if.else133
                                        #   in Loop: Header=BB12_2 Depth=1
	testq	%rcx, %rcx
	jne	.LBB12_29
# BB#27:                                # %if.else133
                                        #   in Loop: Header=BB12_2 Depth=1
	cmpq	$49, %rax
	jne	.LBB12_29
# BB#28:                                # %land.lhs.true142
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	8(%r14), %rax
	movl	(%rax), %eax
	andq	$1073807359, %rax       # imm = 0x4000FFFF
	xorq	%r12, %r12
	cmpq	$1073741885, %rax       # imm = 0x4000003D
	je	.LBB12_9
.LBB12_29:                              # %if.else159
                                        #   in Loop: Header=BB12_2 Depth=1
	cmpq	$0, 16(%rsp)            # 8-byte Folded Reload
	je	.LBB12_46
# BB#30:                                # %if.else159
                                        #   in Loop: Header=BB12_2 Depth=1
	testq	%r13, %r13
	je	.LBB12_46
# BB#31:                                # %land.lhs.true165
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	static_chain_incoming_rtx(%rip), %rsi
	movzbl	(%rsi), %eax
	cmpq	$66, %rax
	jne	.LBB12_38
# BB#32:                                # %land.lhs.true170
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	8(%r13), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB12_37
# BB#33:                                # %land.lhs.true178
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	16(%r13), %rdi
	movq	8(%rsi), %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB12_34
.LBB12_37:                              # %if.else197.land.lhs.true203_crit_edge
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	static_chain_incoming_rtx(%rip), %rsi
.LBB12_38:                              # %land.lhs.true203
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	8(%r13), %rdi
	callq	rtx_equal_p
	xorq	%r12, %r12
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB12_9
# BB#39:                                # %lor.lhs.false
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	(%rsp), %rsi            # 8-byte Reload
	testq	%rsi, %rsi
	je	.LBB12_41
# BB#40:                                # %land.lhs.true210
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	8(%r13), %rdi
	movq	%rsi, %rbx
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%rbx, %rsi
	jne	.LBB12_9
.LBB12_41:                              # %land.lhs.true223
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%rsi, (%rsp)            # 8-byte Spill
	movq	16(%r13), %rdi
	movq	static_chain_incoming_rtx(%rip), %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB12_44
# BB#42:                                # %lor.lhs.false229
                                        #   in Loop: Header=BB12_2 Depth=1
	cmpq	$0, (%rsp)              # 8-byte Folded Reload
	je	.LBB12_46
# BB#43:                                # %land.lhs.true231
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	16(%r13), %rdi
	movq	(%rsp), %rsi            # 8-byte Reload
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB12_44
.LBB12_46:                              # %if.else249
                                        #   in Loop: Header=BB12_2 Depth=1
	testq	%r13, %r13
	je	.LBB12_35
# BB#47:                                # %land.lhs.true252
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	8(%r13), %rdi
	movq	global_rtl+56(%rip), %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB12_35
# BB#48:                                # %if.then258
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	8(%r13), %rax
	movl	8(%rax), %eax
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	32(%rbx), %rcx
	movq	(%rcx,%rax,8), %rax
	movl	8(%rax), %eax
	movq	64(%rbx), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	32(%rdx,%rax), %r14
	movq	global_rtl+56(%rip), %rsi
	movq	%r14, %rdi
	callq	rtx_equal_p
	xorq	%r12, %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	jne	.LBB12_53
# BB#49:                                # %if.else275
                                        #   in Loop: Header=BB12_2 Depth=1
	movzbl	(%r14), %eax
	cmpq	$75, %rax
	jne	.LBB12_113
# BB#50:                                # %land.lhs.true280
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	8(%r14), %rdi
	movq	global_rtl+56(%rip), %rsi
	callq	rtx_equal_p
	testq	%rbx, %rax
	je	.LBB12_113
# BB#51:                                # %land.lhs.true286
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	16(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB12_113
# BB#52:                                # %if.then294
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	8(%rax), %r12
.LBB12_53:                              # %if.end302
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	16(%r13), %rdi
	movq	global_rtl+16(%rip), %rsi
	callq	rtx_equal_p
	movq	16(%r13), %rsi
	testq	%rbx, %rax
	movq	%rbx, %r13
	jne	.LBB12_55
# BB#54:                                # %if.else312
                                        #   in Loop: Header=BB12_2 Depth=1
	negq	%r12
	movq	%rsi, %rdi
	movq	%r12, %rsi
	callq	plus_constant_wide
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	force_operand
	movq	%rax, %rsi
.LBB12_55:                              # %if.end318
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	global_rtl+56(%rip), %rdi
	callq	emit_move_insn
	jmp	.LBB12_56
.LBB12_9:                               #   in Loop: Header=BB12_2 Depth=1
	movq	24(%rsp), %r14          # 8-byte Reload
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jmp	.LBB12_111
.LBB12_77:                              # %if.end407
                                        #   in Loop: Header=BB12_2 Depth=1
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
	movq	%r14, %rsi
	callq	copy_rtx_and_substitute
	movq	%rax, %rdi
	callq	emit_call_insn
	movq	%rax, %r12
	movl	(%r15), %eax
	movl	(%r12), %ecx
	andq	$16777216, %rax         # imm = 0x1000000
	andq	$-16777217, %rcx        # imm = 0xFFFFFFFFFEFFFFFF
	orq	%rax, %rcx
	movl	%ecx, (%r12)
	movl	(%r15), %eax
	andq	$67108864, %rax         # imm = 0x4000000
	movabsq	$4227858431, %rdx       # imm = 0xFBFFFFFF
	andq	%rdx, %rcx
	orq	%rax, %rcx
	movl	%ecx, (%r12)
	movq	64(%r15), %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	copy_rtx_and_substitute
	movq	%rax, 64(%r12)
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	try_constants
	movq	64(%r14), %rax
	leaq	32(%rax), %rdx
	.align	16, 0x90
.LBB12_78:                              # %vector.body
                                        #   Parent Loop BB12_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$4, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$0, (%rdx)
	movq	$0, 48(%rax,%rsi)
	movq	$0, 64(%rax,%rsi)
	movq	$0, 80(%rax,%rsi)
	addq	$4, %rbx
	addq	$64, %rdx
	cmpq	$52, %rbx
	jne	.LBB12_78
# BB#79:                                # %for.body436.preheader
                                        #   in Loop: Header=BB12_2 Depth=1
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB12_80:                              # %for.body436
                                        #   Parent Loop BB12_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$0, 864(%rax,%rcx)
	addq	$16, %rcx
	cmpq	$16, %rcx
	jne	.LBB12_80
	jmp	.LBB12_108
.LBB12_89:                              # %sw.bb459
                                        #   in Loop: Header=BB12_2 Depth=1
	movabsq	$4294967205, %rcx       # imm = 0xFFFFFFA5
	cmpq	%rcx, %rax
	je	.LBB12_90
# BB#91:                                # %sw.bb459
                                        #   in Loop: Header=BB12_2 Depth=1
	movabsq	$4294967209, %rcx       # imm = 0xFFFFFFA9
	cmpq	%rcx, %rax
	jne	.LBB12_93
# BB#92:                                #   in Loop: Header=BB12_2 Depth=1
	xorq	%r12, %r12
	jmp	.LBB12_111
.LBB12_60:                              # %if.then334
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	112(%r14), %rdi
	testq	%rdi, %rdi
	jne	.LBB12_62
# BB#61:                                # %if.then337
                                        #   in Loop: Header=BB12_2 Depth=1
	callq	gen_label_rtx
	movq	%rax, %rdi
	movq	%rdi, 112(%r14)
.LBB12_62:                              # %if.end340
                                        #   in Loop: Header=BB12_2 Depth=1
	callq	gen_jump
.LBB12_64:                              # %if.end348
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%rax, %rdi
	callq	emit_jump_insn
	movq	%rax, %r12
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	try_constants
	movq	%r15, %rdi
	callq	any_condjump_p
	testq	%r13, %rax
	je	.LBB12_108
# BB#65:                                # %land.lhs.true352
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%r15, %rdi
	callq	onlyjump_p
	testq	%r13, %rax
	je	.LBB12_108
# BB#66:                                # %land.lhs.true355
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	624(%r14), %rax
	testq	%rax, %rax
	je	.LBB12_108
# BB#67:                                # %if.then357
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	global_rtl(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB12_107
# BB#68:                                # %if.then361
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%r12, %rdi
	callq	delete_related_insns
	xorq	%r12, %r12
	jmp	.LBB12_111
.LBB12_90:                              #   in Loop: Header=BB12_2 Depth=1
	xorq	%r12, %r12
	jmp	.LBB12_111
.LBB12_93:                              # %sw.bb459
                                        #   in Loop: Header=BB12_2 Depth=1
	movabsq	$4294967208, %rcx       # imm = 0xFFFFFFA8
	cmpq	%rcx, %rax
	jne	.LBB12_97
# BB#94:                                # %if.then465
                                        #   in Loop: Header=BB12_2 Depth=1
	movslq	48(%r15), %rbx
	movq	40(%r14), %rax
	movq	(%rax,%rbx,8), %rdi
	testq	%rdi, %rdi
	jne	.LBB12_96
# BB#95:                                # %if.then.i733
                                        #   in Loop: Header=BB12_2 Depth=1
	callq	gen_label_rtx
	movq	40(%r14), %rcx
	movq	%rax, %rdi
	movq	%rdi, (%rcx,%rbx,8)
.LBB12_96:                              # %get_label_from_map.exit735
                                        #   in Loop: Header=BB12_2 Depth=1
	callq	emit_label
	movq	32(%r15), %rcx
	jmp	.LBB12_84
.LBB12_97:                              # %if.then497
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	32(%r15), %rdi
	callq	emit_note
	movq	%rax, %r12
	testq	%r12, %r12
	jne	.LBB12_99
# BB#98:                                #   in Loop: Header=BB12_2 Depth=1
	xorq	%r12, %r12
	jmp	.LBB12_111
.LBB12_99:                              # %land.lhs.true506
                                        #   in Loop: Header=BB12_2 Depth=1
	movl	40(%r12), %eax
	movq	%rax, %rcx
	movabsq	$4294967294, %rdx       # imm = 0xFFFFFFFE
	andq	%rdx, %rcx
	movabsq	$4294967198, %rdx       # imm = 0xFFFFFF9E
	cmpq	%rdx, %rcx
	jne	.LBB12_104
# BB#100:                               # %land.lhs.true518
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	32(%r15), %rdi
	testq	%rdi, %rdi
	jne	.LBB12_101
.LBB12_104:                             # %land.lhs.true539
                                        #   in Loop: Header=BB12_2 Depth=1
	movabsq	$4294967217, %rcx       # imm = 0xFFFFFFB1
	cmpq	%rcx, %rax
	jne	.LBB12_108
# BB#105:                               # %if.then545
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	32(%r15), %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	copy_rtx_and_substitute
	movq	%rax, 32(%r12)
	jmp	.LBB12_108
.LBB12_14:                              # %if.then67
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%rbx, %r13
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	copy_rtx_and_substitute
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rax, %r12
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB12_18
# BB#15:                                # %cond.true77
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	32(%r12), %r14
	movzbl	(%r14), %eax
	cmpq	$47, %rax
	je	.LBB12_17
# BB#16:                                # %cond.false89
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	single_set_2
	movq	%rax, %r14
.LBB12_17:                              # %cond.end97
                                        #   in Loop: Header=BB12_2 Depth=1
	testq	%r14, %r14
	je	.LBB12_18
# BB#20:                                # %if.end102
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	8(%r14), %rax
	movzbl	2(%rax), %edi
	callq	gen_reg_rtx
	movq	%rax, 8(%r14)
	movq	24(%rsp), %r14          # 8-byte Reload
	jmp	.LBB12_57
.LBB12_107:                             # %if.else363
                                        #   in Loop: Header=BB12_2 Depth=1
	callq	emit_barrier
	jmp	.LBB12_108
.LBB12_23:                              # %land.lhs.true121
                                        #   in Loop: Header=BB12_2 Depth=1
	cmpq	$0, 56(%r15)
	jne	.LBB12_25
# BB#24:                                #   in Loop: Header=BB12_2 Depth=1
	movq	24(%rsp), %r14          # 8-byte Reload
	jmp	.LBB12_111
.LBB12_44:                              # %if.then237
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	8(%r13), %rdi
	movq	$1, %rdx
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	callq	copy_rtx_and_substitute
	movq	%rax, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	emit_move_insn
	movq	static_chain_incoming_rtx(%rip), %rcx
	movzbl	(%rcx), %ecx
	movq	%rax, %r12
	cmpq	$66, %rcx
	je	.LBB12_36
# BB#45:                                # %if.then237
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jmp	.LBB12_57
.LBB12_101:                             # %if.then522
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	24(%r14), %rsi
	movq	8(%rsi), %rdx
	addq	$32, %rsi
	movq	$8, %rcx
	movabsq	$find_block, %r8
	callq	bsearch
	testq	%rax, %rax
	je	.LBB12_102
# BB#103:                               # %if.else532
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	(%rax), %rax
	movq	%rax, 32(%r12)
	jmp	.LBB12_108
.LBB12_34:                              # %if.then187
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	static_chain_incoming_rtx(%rip), %rax
	movq	8(%r13), %rsi
	movzbl	2(%rax), %edi
	callq	gen_rtx_MEM
	movq	%rax, (%rsp)            # 8-byte Spill
.LBB12_35:                              # %if.else320
                                        #   in Loop: Header=BB12_2 Depth=1
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	callq	copy_rtx_and_substitute
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rax, %r12
.LBB12_36:                              # %if.end328
                                        #   in Loop: Header=BB12_2 Depth=1
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jmp	.LBB12_57
.LBB12_25:                              # %if.then127
                                        #   in Loop: Header=BB12_2 Depth=1
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	callq	copy_rtx_and_substitute
	movq	%rax, %rdi
	callq	emit_insn
.LBB12_56:                              # %if.end328
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%rax, %r12
.LBB12_57:                              # %if.end328
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	try_constants
	.align	16, 0x90
.LBB12_108:                             # %sw.epilog
                                        #   in Loop: Header=BB12_2 Depth=1
	testq	%r12, %r12
	jne	.LBB12_110
# BB#109:                               #   in Loop: Header=BB12_2 Depth=1
	xorq	%r12, %r12
	jmp	.LBB12_111
	.align	16, 0x90
.LBB12_110:                             # %if.then558
                                        #   in Loop: Header=BB12_2 Depth=1
	movl	(%r12), %eax
	orq	$1073741824, %rax       # imm = 0x40000000
	movl	%eax, (%r12)
.LBB12_111:                             # %if.end562
                                        #   in Loop: Header=BB12_2 Depth=1
	movslq	8(%r15), %rax
	movq	48(%r14), %rcx
	movq	%r12, (%rcx,%rax,8)
	movq	24(%r15), %r15
	testq	%r15, %r15
	jne	.LBB12_2
.LBB12_112:                             # %for.end572
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB12_106:                             # %sw.default
	movabsq	$.L.str15, %rdi
	movabsq	$.L__FUNCTION__.copy_insn_list, %rdx
	movq	$1686, %rsi             # imm = 0x696
	callq	fancy_abort
.LBB12_113:                             # %if.else300
	movabsq	$.L.str15, %rax
	movabsq	$.L__FUNCTION__.copy_insn_list, %rcx
	movq	$1474, %rsi             # imm = 0x5C2
	jmp	.LBB12_19
.LBB12_18:                              # %if.then101
	movabsq	$.L.str15, %rax
	movabsq	$.L__FUNCTION__.copy_insn_list, %rcx
	movq	$1373, %rsi             # imm = 0x55D
	jmp	.LBB12_19
.LBB12_102:                             # %if.then531
	movabsq	$.L.str15, %rax
	movabsq	$.L__FUNCTION__.copy_insn_list, %rcx
	movq	$1671, %rsi             # imm = 0x687
.LBB12_19:                              # %if.then101
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp200:
	.size	copy_insn_list, .Ltmp200-copy_insn_list
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI12_0:
	.quad	.LBB12_4
	.quad	.LBB12_58
	.quad	.LBB12_69
	.quad	.LBB12_85
	.quad	.LBB12_81
	.quad	.LBB12_86

	.text
	.align	16, 0x90
	.type	copy_insn_notes,@function
copy_insn_notes:                        # @copy_insn_notes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp207:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp208:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp209:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp210:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp211:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp212:
	.cfi_def_cfa_offset 64
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
	movq	%rsi, %r15
	movl	72(%r15), %eax
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	%rdi, %rbx
	incq	%rax
	testq	%rbx, %rbx
	movl	%eax, 72(%r15)
	je	.LBB13_21
# BB#1:                                 # %for.body.lr.ph
	movq	$32, %rcx
	movq	(%rsp), %r14            # 8-byte Reload
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	.align	16, 0x90
.LBB13_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_6 Depth 2
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB13_20
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB13_2 Depth=1
	movslq	8(%rbx), %rcx
	movq	48(%r15), %rdx
	movq	(%rdx,%rcx,8), %r13
	testq	%r13, %r13
	je	.LBB13_20
# BB#4:                                 # %if.end7
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	56(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB13_14
# BB#5:                                 # %if.then11
                                        #   in Loop: Header=BB13_2 Depth=1
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	callq	copy_rtx_and_substitute
	movq	%rax, 8(%rsp)
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	leaq	8(%rsp), %rdi
	movq	%r15, %rdx
	callq	subst_constants
	callq	apply_change_group
	movq	8(%rsp), %rax
	movq	%rax, 56(%r13)
	movq	8(%rsp), %r12
	jmp	.LBB13_6
	.align	16, 0x90
.LBB13_12:                              # %for.inc
                                        #   in Loop: Header=BB13_6 Depth=2
	movq	%r12, 8(%rsp)
.LBB13_6:                               # %if.then11
                                        #   Parent Loop BB13_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r12, %r12
	je	.LBB13_13
# BB#7:                                 # %for.body21
                                        #   in Loop: Header=BB13_6 Depth=2
	movq	%r12, %rsi
	movzbl	2(%rsi), %eax
	movq	16(%rsi), %r12
	cmpq	$23, %rax
	jne	.LBB13_8
# BB#10:                                # %land.lhs.true
                                        #   in Loop: Header=BB13_6 Depth=2
	movq	8(%rsi), %rax
	movq	8(%rax), %rsi
	testq	%rsi, %rsi
	jle	.LBB13_12
# BB#11:                                # %if.then42
                                        #   in Loop: Header=BB13_6 Depth=2
	addq	%r14, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	8(%rsp), %rcx
	movq	%rax, 8(%rcx)
	jmp	.LBB13_12
	.align	16, 0x90
.LBB13_8:                               # %for.body21
                                        #   in Loop: Header=BB13_6 Depth=2
	cmpq	$13, %rax
	jne	.LBB13_12
# BB#9:                                 # %if.then29
                                        #   in Loop: Header=BB13_6 Depth=2
	movq	%r13, %rdi
	callq	remove_note
	movq	%r12, 8(%rsp)
	jmp	.LBB13_6
.LBB13_13:                              # %if.end56.loopexit
                                        #   in Loop: Header=BB13_2 Depth=1
	movl	(%rbx), %eax
.LBB13_14:                              # %if.end56
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$34, %rcx
	jne	.LBB13_17
# BB#15:                                # %land.lhs.true61
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	32(%rbx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$141, %rdx
	jne	.LBB13_17
# BB#16:                                # %for.cond70.preheader
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	8(%rcx), %rdi
	movq	%r15, %rsi
	movq	(%rsp), %r12            # 8-byte Reload
	movq	%r12, %rdx
	callq	copy_insn_notes
	movq	32(%rbx), %rax
	movq	16(%rax), %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	copy_insn_notes
	movq	32(%rbx), %rax
	movq	24(%rax), %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	copy_insn_notes
	movl	(%rbx), %eax
.LBB13_17:                              # %if.end84
                                        #   in Loop: Header=BB13_2 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$33, %rax
	jne	.LBB13_20
# BB#18:                                # %land.lhs.true89
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	32(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$53, %rax
	jne	.LBB13_20
# BB#19:                                # %if.then97
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	32(%r13), %rax
	movl	8(%rax), %ecx
	movq	(%rsp), %rdx            # 8-byte Reload
	addq	%rdx, %rcx
	movl	%ecx, 8(%rax)
	.align	16, 0x90
.LBB13_20:                              # %for.inc106
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB13_2
.LBB13_21:                              # %for.end110
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp218:
	.size	copy_insn_notes, .Ltmp218-copy_insn_notes
	.cfi_endproc

	.globl	try_constants
	.align	16, 0x90
	.type	try_constants,@function
try_constants:                          # @try_constants
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
	subq	$16, %rsp
.Ltmp230:
	.cfi_def_cfa_offset 64
.Ltmp231:
	.cfi_offset %rbx, -48
.Ltmp232:
	.cfi_offset %r12, -40
.Ltmp233:
	.cfi_offset %r13, -32
.Ltmp234:
	.cfi_offset %r14, -24
.Ltmp235:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	xorq	%rax, %rax
	movl	%eax, 136(%r14)
	leaq	32(%rbx), %r15
	movq	$1, %rcx
	movq	$1, %r12
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	subst_constants
	callq	apply_change_group
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	subst_constants
	callq	apply_change_group
	movq	32(%rbx), %rdi
	movabsq	$mark_stores, %rsi
	xorq	%rdx, %rdx
	callq	note_stores
	movq	$0, 624(%r14)
	movslq	136(%r14), %rax
	testq	%rax, %rax
	jle	.LBB14_16
# BB#1:                                 # %for.body.lr.ph
	leaq	152(%r14), %rbx
	.align	16, 0x90
.LBB14_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB14_13
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	64(%r14), %r15
	movslq	8(%rax), %r13
	movq	(%r15), %rax
	cmpq	%rax, %r13
	jb	.LBB14_6
# BB#4:                                 # %if.then19
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	global_const_equiv_varray(%rip), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	leaq	1(%r13), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%r15, %rdi
	callq	varray_grow
	cmpq	%r15, 8(%rsp)           # 8-byte Folded Reload
	movq	%rax, 64(%r14)
	movq	%rax, %r15
	jne	.LBB14_6
# BB#5:                                 # %if.then27
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r15, global_const_equiv_varray(%rip)
	movq	64(%r14), %r15
.LBB14_6:                               # %if.end29
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	$4, %rcx
	shlq	%cl, %r13
	movq	32(%r15,%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB14_9
# BB#7:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	(%rbx), %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB14_15
# BB#8:                                 # %lor.lhs.false.if.then47_crit_edge
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	64(%r14), %r15
.LBB14_9:                               # %if.then47
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	-8(%rbx), %rax
	movl	8(%rax), %esi
	movq	(%r15), %rax
	cmpq	%rax, %rsi
	jb	.LBB14_12
# BB#10:                                # %if.then60
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	global_const_equiv_varray(%rip), %r13
	incq	%rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	movq	%r15, %rdi
	callq	varray_grow
	cmpq	%r15, %r13
	movq	%rax, 64(%r14)
	movq	%rax, %r15
	jne	.LBB14_12
# BB#11:                                # %if.then78
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r15, global_const_equiv_varray(%rip)
	movq	64(%r14), %r15
.LBB14_12:                              # %if.end81
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	-8(%rbx), %rax
	movl	8(%rax), %eax
	movq	(%rbx), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	%rdx, 32(%r15,%rax)
	movl	72(%r14), %ecx
	movl	%ecx, 40(%r15,%rax)
	jmp	.LBB14_15
	.align	16, 0x90
.LBB14_13:                              # %if.else
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	global_rtl(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB14_15
# BB#14:                                # %if.then106
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	(%rbx), %rax
	movq	%rax, 624(%r14)
.LBB14_15:                              # %for.inc
                                        #   in Loop: Header=BB14_2 Depth=1
	movslq	136(%r14), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r12
	addq	$16, %rbx
	cmpq	%rax, %rdx
	jl	.LBB14_2
.LBB14_16:                              # %for.end
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp236:
	.size	try_constants, .Ltmp236-try_constants
	.cfi_endproc

	.align	16, 0x90
	.type	mark_stores,@function
mark_stores:                            # @mark_stores
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp240:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp241:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp242:
	.cfi_def_cfa_offset 32
.Ltmp243:
	.cfi_offset %rbx, -24
.Ltmp244:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movl	(%r14), %r8d
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB15_1
# BB#3:                                 # %land.lhs.true
	movq	8(%r14), %rax
	movl	(%rax), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB15_31
# BB#4:                                 # %if.then11
	movslq	8(%rax), %rbx
	cmpq	$52, %rbx
	jg	.LBB15_5
# BB#6:                                 # %if.then19
	movq	$16, %rcx
	shrq	%cl, %rsi
	movq	$16, %rcx
	movl	16(%r14), %edx
	shrq	%cl, %r8
	andq	$255, %rsi
	andq	$255, %r8
	movq	%rbx, %rdi
	movq	%r8, %rcx
	callq	subreg_regno_offset
	movq	8(%r14), %rcx
	movl	(%rcx), %r8d
	addq	%rax, %rbx
	jmp	.LBB15_7
.LBB15_1:                               # %entry
	cmpq	$61, %rax
	jne	.LBB15_31
# BB#2:                                 # %if.then
	movl	8(%r14), %ebx
.LBB15_7:                               # %if.end45
	movq	$16, %rcx
	shrq	%cl, %r8
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB15_31
# BB#8:                                 # %if.then47
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rbx, %rcx
	andq	%rax, %rcx
	cmpq	$52, %rcx
	jbe	.LBB15_10
# BB#9:
	movq	%rbx, %rax
	jmp	.LBB15_25
.LBB15_5:
	movq	%rbx, %rax
	jmp	.LBB15_25
.LBB15_10:                              # %cond.false
	andq	$255, %r8
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rbx, %rcx
	cmpq	$8, %rcx
	je	.LBB15_14
# BB#11:                                # %cond.false
	leaq	-21(%rbx), %rcx
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB15_14
# BB#12:                                # %cond.false
	leaq	-45(%rbx), %rcx
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB15_14
# BB#13:                                # %cond.false
	leaq	-29(%rbx), %rcx
	andq	%rax, %rcx
	cmpq	$7, %rcx
	jbe	.LBB15_14
# BB#19:                                # %cond.false69
	cmpq	$24, %r8
	jne	.LBB15_20
# BB#22:                                # %cond.true75
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB15_24
.LBB15_14:                              # %cond.true63
	movl	mode_class(,%r8,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB15_24
# BB#15:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB15_16
# BB#17:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB15_24
.LBB15_20:                              # %cond.false69
	cmpq	$18, %r8
	jne	.LBB15_23
# BB#21:                                # %cond.true71
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB15_24
.LBB15_16:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB15_24
.LBB15_23:                              # %cond.false79
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%r8), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB15_24:                              # %cond.end92
	leaq	-1(%rbx,%rax), %rax
.LBB15_25:                              # %cond.end96
	leaq	-53(%rbx), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$2, %rcx
	jb	.LBB15_31
# BB#26:                                # %cond.end96
	andq	%rdx, %rax
	jmp	.LBB15_27
	.align	16, 0x90
.LBB15_30:                              # %for.inc
                                        #   in Loop: Header=BB15_27 Depth=1
	incq	%rbx
.LBB15_27:                              # %cond.end96
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rcx
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	ja	.LBB15_31
# BB#28:                                # %for.body
                                        #   in Loop: Header=BB15_27 Depth=1
	movq	global_const_equiv_varray(%rip), %rsi
	movq	(%rsi), %rcx
	movq	%rbx, %rdi
	andq	%rdx, %rdi
	cmpq	%rcx, %rdi
	jae	.LBB15_30
# BB#29:                                # %if.then109
                                        #   in Loop: Header=BB15_27 Depth=1
	movq	$4, %rcx
	shlq	%cl, %rdi
	movq	$0, 32(%rsi,%rdi)
	jmp	.LBB15_30
.LBB15_31:                              # %if.end116
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp245:
	.size	mark_stores, .Ltmp245-mark_stores
	.cfi_endproc

	.globl	set_decl_origin_self
	.align	16, 0x90
	.type	set_decl_origin_self,@function
set_decl_origin_self:                   # @set_decl_origin_self
	.cfi_startproc
# BB#0:                                 # %entry
	cmpq	$0, 112(%rdi)
	jne	.LBB16_7
# BB#1:                                 # %if.then
	movq	%rdi, 112(%rdi)
	movzbl	16(%rdi), %eax
	cmpq	$30, %rax
	jne	.LBB16_7
# BB#2:                                 # %if.then5
	movq	88(%rdi), %rax
	jmp	.LBB16_4
	.align	16, 0x90
.LBB16_3:                               # %for.body
                                        #   in Loop: Header=BB16_4 Depth=1
	movq	%rax, 112(%rax)
	movq	(%rax), %rax
.LBB16_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB16_3
# BB#5:                                 # %for.end
	movq	104(%rdi), %rdi
	testq	%rdi, %rdi
	je	.LBB16_7
# BB#6:                                 # %for.end
	movq	global_trees(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB16_7
# BB#8:                                 # %if.then15
	jmp	set_block_origin_self  # TAILCALL
.LBB16_7:                               # %if.end19
	retq
.Ltmp246:
	.size	set_decl_origin_self, .Ltmp246-set_decl_origin_self
	.cfi_endproc

	.align	16, 0x90
	.type	set_block_origin_self,@function
set_block_origin_self:                  # @set_block_origin_self
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp250:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp251:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp252:
	.cfi_def_cfa_offset 32
.Ltmp253:
	.cfi_offset %rbx, -24
.Ltmp254:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	cmpq	$0, 56(%r14)
	jne	.LBB17_15
# BB#1:                                 # %if.then
	movq	%r14, 56(%r14)
	movq	32(%r14), %rbx
	jmp	.LBB17_2
	.align	16, 0x90
.LBB17_11:                              # %set_decl_origin_self.exit
                                        #   in Loop: Header=BB17_2 Depth=1
	movq	(%rbx), %rbx
.LBB17_2:                               # %if.then
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB17_7 Depth 2
	testq	%rbx, %rbx
	je	.LBB17_12
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB17_2 Depth=1
	cmpq	$0, 112(%rbx)
	jne	.LBB17_11
# BB#4:                                 # %if.then.i
                                        #   in Loop: Header=BB17_2 Depth=1
	movq	%rbx, 112(%rbx)
	movzbl	16(%rbx), %eax
	cmpq	$30, %rax
	jne	.LBB17_11
# BB#5:                                 # %if.then5.i
                                        #   in Loop: Header=BB17_2 Depth=1
	movq	88(%rbx), %rax
	jmp	.LBB17_7
	.align	16, 0x90
.LBB17_6:                               # %for.body.i
                                        #   in Loop: Header=BB17_7 Depth=2
	movq	%rax, 112(%rax)
	movq	(%rax), %rax
.LBB17_7:                               # %for.body.i
                                        #   Parent Loop BB17_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rax, %rax
	jne	.LBB17_6
# BB#8:                                 # %for.end.i
                                        #   in Loop: Header=BB17_2 Depth=1
	movq	104(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB17_11
# BB#9:                                 # %for.end.i
                                        #   in Loop: Header=BB17_2 Depth=1
	movq	global_trees(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB17_11
# BB#10:                                # %if.then15.i
                                        #   in Loop: Header=BB17_2 Depth=1
	callq	set_block_origin_self
	jmp	.LBB17_11
.LBB17_12:                              # %for.end
	movq	40(%r14), %rbx
	jmp	.LBB17_14
	.align	16, 0x90
.LBB17_13:                              # %for.body8
                                        #   in Loop: Header=BB17_14 Depth=1
	movq	%rbx, %rdi
	callq	set_block_origin_self
	movq	(%rbx), %rbx
.LBB17_14:                              # %for.body8
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB17_13
.LBB17_15:                              # %if.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp255:
	.size	set_block_origin_self, .Ltmp255-set_block_origin_self
	.cfi_endproc

	.globl	set_decl_abstract_flags
	.align	16, 0x90
	.type	set_decl_abstract_flags,@function
set_decl_abstract_flags:                # @set_decl_abstract_flags
	.cfi_startproc
# BB#0:                                 # %entry
	movq	48(%rdi), %rcx
	movq	%rsi, %rax
	andq	$1, %rax
	shlq	$15, %rax
	andq	$-32769, %rcx           # imm = 0xFFFFFFFFFFFF7FFF
	orq	%rax, %rcx
	movq	%rcx, 48(%rdi)
	movzbl	16(%rdi), %ecx
	cmpq	$30, %rcx
	jne	.LBB18_6
# BB#1:                                 # %if.then
	movq	88(%rdi), %rcx
	jmp	.LBB18_3
	.align	16, 0x90
.LBB18_2:                               # %for.body
                                        #   in Loop: Header=BB18_3 Depth=1
	movq	48(%rcx), %rdx
	andq	$-32769, %rdx           # imm = 0xFFFFFFFFFFFF7FFF
	orq	%rax, %rdx
	movq	%rdx, 48(%rcx)
	movq	(%rcx), %rcx
.LBB18_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	jne	.LBB18_2
# BB#4:                                 # %for.end
	movq	104(%rdi), %rdi
	testq	%rdi, %rdi
	je	.LBB18_6
# BB#5:                                 # %for.end
	movq	global_trees(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB18_6
# BB#7:                                 # %if.then19
	jmp	set_block_abstract_flags  # TAILCALL
.LBB18_6:                               # %if.end22
	retq
.Ltmp256:
	.size	set_decl_abstract_flags, .Ltmp256-set_decl_abstract_flags
	.cfi_endproc

	.align	16, 0x90
	.type	set_block_abstract_flags,@function
set_block_abstract_flags:               # @set_block_abstract_flags
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp262:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp263:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp264:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp265:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp266:
	.cfi_def_cfa_offset 48
.Ltmp267:
	.cfi_offset %rbx, -40
.Ltmp268:
	.cfi_offset %r12, -32
.Ltmp269:
	.cfi_offset %r14, -24
.Ltmp270:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r14
	movl	24(%r14), %eax
	movq	%r15, %rcx
	addq	%rcx, %rcx
	andq	$2, %rcx
	movabsq	$4294967293, %rdx       # imm = 0xFFFFFFFD
	andq	%rax, %rdx
	orq	%rcx, %rdx
	movl	%edx, 24(%r14)
	movq	32(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB19_10
# BB#1:                                 # %for.body.lr.ph
	movq	%r15, %r12
	andq	$1, %r12
	shlq	$15, %r12
	.align	16, 0x90
.LBB19_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_5 Depth 2
	movq	48(%rbx), %rax
	andq	$-32769, %rax           # imm = 0xFFFFFFFFFFFF7FFF
	orq	%r12, %rax
	movq	%rax, 48(%rbx)
	movzbl	16(%rbx), %eax
	cmpq	$30, %rax
	jne	.LBB19_9
# BB#3:                                 # %if.then.i
                                        #   in Loop: Header=BB19_2 Depth=1
	movq	88(%rbx), %rax
	jmp	.LBB19_5
	.align	16, 0x90
.LBB19_4:                               # %for.body.i
                                        #   in Loop: Header=BB19_5 Depth=2
	movq	48(%rax), %rcx
	andq	$-32769, %rcx           # imm = 0xFFFFFFFFFFFF7FFF
	orq	%r12, %rcx
	movq	%rcx, 48(%rax)
	movq	(%rax), %rax
.LBB19_5:                               # %for.body.i
                                        #   Parent Loop BB19_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rax, %rax
	jne	.LBB19_4
# BB#6:                                 # %for.end.i
                                        #   in Loop: Header=BB19_2 Depth=1
	movq	104(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB19_9
# BB#7:                                 # %for.end.i
                                        #   in Loop: Header=BB19_2 Depth=1
	movq	global_trees(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB19_9
# BB#8:                                 # %if.then19.i
                                        #   in Loop: Header=BB19_2 Depth=1
	movq	%r15, %rsi
	callq	set_block_abstract_flags
.LBB19_9:                               # %set_decl_abstract_flags.exit
                                        #   in Loop: Header=BB19_2 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB19_2
.LBB19_10:                              # %for.end
	movq	40(%r14), %rbx
	jmp	.LBB19_12
	.align	16, 0x90
.LBB19_11:                              # %for.body5
                                        #   in Loop: Header=BB19_12 Depth=1
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	set_block_abstract_flags
	movq	(%rbx), %rbx
.LBB19_12:                              # %for.body5
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB19_11
# BB#13:                                # %for.end9
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp271:
	.size	set_block_abstract_flags, .Ltmp271-set_block_abstract_flags
	.cfi_endproc

	.globl	output_inline_function
	.align	16, 0x90
	.type	output_inline_function,@function
output_inline_function:                 # @output_inline_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp278:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp279:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp280:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp281:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp282:
	.cfi_def_cfa_offset 48
.Ltmp283:
	.cfi_offset %rbx, -48
.Ltmp284:
	.cfi_offset %r12, -40
.Ltmp285:
	.cfi_offset %r13, -32
.Ltmp286:
	.cfi_offset %r14, -24
.Ltmp287:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	cfun(%rip), %r12
	movl	write_symbols(%rip), %r14d
	movq	160(%rbx), %r13
	movq	debug_hooks(%rip), %r15
	movq	%r13, cfun(%rip)
	movq	%rbx, current_function_decl(%rip)
	callq	clear_emit_caches
	movl	384(%r13), %edi
	callq	set_new_last_label_num
	andq	$-262145, 48(%rbx)      # imm = 0xFFFFFFFFFFFBFFFF
	movl	356(%r13), %eax
	testq	%rax, %rax
	je	.LBB20_2
# BB#1:                                 # %if.then
	xorq	%rax, %rax
	movl	%eax, write_symbols(%rip)
	movq	$do_nothing_debug_hooks, debug_hooks(%rip)
.LBB20_2:                               # %if.end
	movq	%rbx, %rdi
	callq	rest_of_compilation
	andq	$-2049, 48(%rbx)        # imm = 0xFFFFFFFFFFFFF7FF
	movq	%r12, cfun(%rip)
	xorq	%rax, %rax
	testq	%r12, %r12
	je	.LBB20_4
# BB#3:                                 # %cond.true
	movq	48(%r12), %rax
.LBB20_4:                               # %cond.end
	movq	%rax, current_function_decl(%rip)
	movl	%r14d, write_symbols(%rip)
	movq	%r15, debug_hooks(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp288:
	.size	output_inline_function, .Ltmp288-output_inline_function
	.cfi_endproc

	.globl	get_hard_reg_initial_reg
	.align	16, 0x90
	.type	get_hard_reg_initial_reg,@function
get_hard_reg_initial_reg:               # @get_hard_reg_initial_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp295:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp296:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp297:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp298:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp299:
	.cfi_def_cfa_offset 48
.Ltmp300:
	.cfi_offset %rbx, -48
.Ltmp301:
	.cfi_offset %r12, -40
.Ltmp302:
	.cfi_offset %r13, -32
.Ltmp303:
	.cfi_offset %r14, -24
.Ltmp304:
	.cfi_offset %r15, -16
	movq	144(%rdi), %r15
	movq	%rsi, %r14
	xorq	%rax, %rax
	testq	%r15, %r15
	je	.LBB21_6
# BB#1:                                 # %for.cond.preheader
	movslq	(%r15), %rcx
	testq	%rcx, %rcx
	jle	.LBB21_6
# BB#2:                                 # %for.body.lr.ph
	xorq	%r12, %r12
	movq	$1, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB21_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r15), %rax
	movq	8(%rax,%r12), %rdi
	movq	%r14, %rsi
	callq	rtx_equal_p
	testq	%r13, %rax
	jne	.LBB21_5
# BB#3:                                 # %for.cond
                                        #   in Loop: Header=BB21_4 Depth=1
	movslq	(%r15), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	$16, %r12
	incq	%rbx
	xorq	%rax, %rax
	cmpq	%rdx, %rsi
	jl	.LBB21_4
	jmp	.LBB21_6
.LBB21_5:                               # %if.then2
	movq	8(%r15), %rax
	movq	(%rax,%r12), %rax
.LBB21_6:                               # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp305:
	.size	get_hard_reg_initial_reg, .Ltmp305-get_hard_reg_initial_reg
	.cfi_endproc

	.globl	has_func_hard_reg_initial_val
	.align	16, 0x90
	.type	has_func_hard_reg_initial_val,@function
has_func_hard_reg_initial_val:          # @has_func_hard_reg_initial_val
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp312:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp313:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp314:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp315:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp316:
	.cfi_def_cfa_offset 48
.Ltmp317:
	.cfi_offset %rbx, -48
.Ltmp318:
	.cfi_offset %r12, -40
.Ltmp319:
	.cfi_offset %r13, -32
.Ltmp320:
	.cfi_offset %r14, -24
.Ltmp321:
	.cfi_offset %r15, -16
	movq	144(%rdi), %r15
	movq	%rsi, %r14
	xorq	%rax, %rax
	testq	%r15, %r15
	je	.LBB22_6
# BB#1:                                 # %for.cond.preheader
	movslq	(%r15), %rcx
	testq	%rcx, %rcx
	jle	.LBB22_6
# BB#2:                                 # %for.body.lr.ph
	xorq	%r12, %r12
	movq	$1, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB22_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r15), %rax
	movq	(%rax,%r12), %rdi
	movq	%r14, %rsi
	callq	rtx_equal_p
	testq	%r13, %rax
	jne	.LBB22_5
# BB#3:                                 # %for.cond
                                        #   in Loop: Header=BB22_4 Depth=1
	movslq	(%r15), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	$16, %r12
	incq	%rbx
	xorq	%rax, %rax
	cmpq	%rdx, %rsi
	jl	.LBB22_4
	jmp	.LBB22_6
.LBB22_5:                               # %if.then2
	movq	8(%r15), %rax
	movq	8(%rax,%r12), %rax
.LBB22_6:                               # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp322:
	.size	has_func_hard_reg_initial_val, .Ltmp322-has_func_hard_reg_initial_val
	.cfi_endproc

	.globl	get_func_hard_reg_initial_val
	.align	16, 0x90
	.type	get_func_hard_reg_initial_val,@function
get_func_hard_reg_initial_val:          # @get_func_hard_reg_initial_val
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp329:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp330:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp331:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp332:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp333:
	.cfi_def_cfa_offset 48
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
	movq	%rdi, %r15
	movq	144(%r15), %r12
	movq	%rsi, %r14
	testq	%r12, %r12
	je	.LBB23_6
# BB#1:                                 # %for.cond.preheader.i
	movslq	(%r12), %rax
	testq	%rax, %rax
	jle	.LBB23_7
# BB#2:                                 # %for.body.lr.ph.i
	xorq	%r15, %r15
	movq	$1, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB23_4:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r12), %rax
	movq	(%rax,%r15), %rdi
	movq	%r14, %rsi
	callq	rtx_equal_p
	testq	%r13, %rax
	jne	.LBB23_5
# BB#3:                                 # %for.cond.i
                                        #   in Loop: Header=BB23_4 Depth=1
	movslq	(%r12), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$16, %r15
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB23_4
	jmp	.LBB23_7
.LBB23_6:                               # %if.then1
	movq	$16, %rdi
	callq	xmalloc
	movq	%rax, %r12
	movq	%r12, 144(%r15)
	movabsq	$21474836480, %rax      # imm = 0x500000000
	movq	%rax, (%r12)
	movq	$80, %rdi
	callq	xmalloc
	movq	%rax, 8(%r12)
.LBB23_7:                               # %if.end6
	movslq	(%r12), %rdx
	movslq	4(%r12), %rsi
	cmpq	%rsi, %rdx
	jge	.LBB23_9
# BB#8:                                 # %if.end6.if.end16_crit_edge
	movq	8(%r12), %rax
	jmp	.LBB23_10
.LBB23_9:                               # %if.then10
	addq	$5, %rsi
	movl	%esi, 4(%r12)
	movq	8(%r12), %rdi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movq	%rax, 8(%r12)
	movl	(%r12), %edx
.LBB23_10:                              # %if.end16
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	%r14, (%rax,%rdx)
	movzbl	2(%r14), %edi
	callq	gen_reg_rtx
	movslq	(%r12), %rdx
	movq	8(%r12), %rsi
	movq	$4, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	%rax, 8(%rsi,%rdi)
	incq	%rdx
	movl	%edx, (%r12)
.LBB23_11:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB23_5:                               # %has_func_hard_reg_initial_val.exit
	movq	8(%r12), %rax
	movq	8(%rax,%r15), %rax
	testq	%rax, %rax
	je	.LBB23_7
	jmp	.LBB23_11
.Ltmp339:
	.size	get_func_hard_reg_initial_val, .Ltmp339-get_func_hard_reg_initial_val
	.cfi_endproc

	.globl	get_hard_reg_initial_val
	.align	16, 0x90
	.type	get_hard_reg_initial_val,@function
get_hard_reg_initial_val:               # @get_hard_reg_initial_val
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp342:
	.cfi_def_cfa_offset 16
.Ltmp343:
	.cfi_offset %rbx, -16
	movq	cfun(%rip), %rbx
	callq	gen_rtx_REG
	movq	%rbx, %rdi
	movq	%rax, %rsi
	popq	%rbx
	jmp	get_func_hard_reg_initial_val  # TAILCALL
.Ltmp344:
	.size	get_hard_reg_initial_val, .Ltmp344-get_hard_reg_initial_val
	.cfi_endproc

	.globl	has_hard_reg_initial_val
	.align	16, 0x90
	.type	has_hard_reg_initial_val,@function
has_hard_reg_initial_val:               # @has_hard_reg_initial_val
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp351:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp352:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp353:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp354:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp355:
	.cfi_def_cfa_offset 48
.Ltmp356:
	.cfi_offset %rbx, -48
.Ltmp357:
	.cfi_offset %r12, -40
.Ltmp358:
	.cfi_offset %r13, -32
.Ltmp359:
	.cfi_offset %r14, -24
.Ltmp360:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rbx
	callq	gen_rtx_REG
	movq	144(%rbx), %r15
	movq	%rax, %r14
	xorq	%rax, %rax
	testq	%r15, %r15
	je	.LBB25_6
# BB#1:                                 # %for.cond.preheader.i
	movslq	(%r15), %rcx
	testq	%rcx, %rcx
	jle	.LBB25_6
# BB#2:                                 # %for.body.lr.ph.i
	xorq	%r12, %r12
	movq	$1, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB25_4:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r15), %rax
	movq	(%rax,%r12), %rdi
	movq	%r14, %rsi
	callq	rtx_equal_p
	testq	%r13, %rax
	jne	.LBB25_5
# BB#3:                                 # %for.cond.i
                                        #   in Loop: Header=BB25_4 Depth=1
	movslq	(%r15), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	$16, %r12
	incq	%rbx
	xorq	%rax, %rax
	cmpq	%rdx, %rsi
	jl	.LBB25_4
	jmp	.LBB25_6
.LBB25_5:                               # %if.then2.i
	movq	8(%r15), %rax
	movq	8(%rax,%r12), %rax
.LBB25_6:                               # %has_func_hard_reg_initial_val.exit
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp361:
	.size	has_hard_reg_initial_val, .Ltmp361-has_hard_reg_initial_val
	.cfi_endproc

	.globl	mark_hard_reg_initial_vals
	.align	16, 0x90
	.type	mark_hard_reg_initial_vals,@function
mark_hard_reg_initial_vals:             # @mark_hard_reg_initial_vals
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp368:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp369:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp370:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp371:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp372:
	.cfi_def_cfa_offset 48
.Ltmp373:
	.cfi_offset %rbx, -48
.Ltmp374:
	.cfi_offset %r12, -40
.Ltmp375:
	.cfi_offset %r13, -32
.Ltmp376:
	.cfi_offset %r14, -24
.Ltmp377:
	.cfi_offset %r15, -16
	movq	144(%rdi), %r14
	testq	%r14, %r14
	je	.LBB26_10
# BB#1:                                 # %for.cond.preheader
	movslq	(%r14), %rax
	testq	%rax, %rax
	jle	.LBB26_10
# BB#2:                                 # %do.body.lr.ph
	movq	$1, %r12
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB26_3:                               # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r14), %rax
	movq	(%rax,%rbx), %r15
	testq	%r15, %r15
	je	.LBB26_6
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB26_3 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB26_6
# BB#5:                                 # %if.then3
                                        #   in Loop: Header=BB26_3 Depth=1
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children
.LBB26_6:                               # %do.body5
                                        #   in Loop: Header=BB26_3 Depth=1
	movq	8(%r14), %rax
	movq	8(%rax,%rbx), %r15
	testq	%r15, %r15
	je	.LBB26_9
# BB#7:                                 # %land.lhs.true11
                                        #   in Loop: Header=BB26_3 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB26_9
# BB#8:                                 # %if.then14
                                        #   in Loop: Header=BB26_3 Depth=1
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children
.LBB26_9:                               # %for.inc
                                        #   in Loop: Header=BB26_3 Depth=1
	movslq	(%r14), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r12
	addq	$16, %rbx
	cmpq	%rax, %rdx
	jl	.LBB26_3
.LBB26_10:                              # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp378:
	.size	mark_hard_reg_initial_vals, .Ltmp378-mark_hard_reg_initial_vals
	.cfi_endproc

	.globl	emit_initial_value_sets
	.align	16, 0x90
	.type	emit_initial_value_sets,@function
emit_initial_value_sets:                # @emit_initial_value_sets
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp383:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp384:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp385:
	.cfi_def_cfa_offset 32
.Ltmp386:
	.cfi_offset %rbx, -32
.Ltmp387:
	.cfi_offset %r14, -24
.Ltmp388:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	144(%rax), %r14
	testq	%r14, %r14
	je	.LBB27_5
# BB#1:                                 # %if.end
	callq	start_sequence
	movslq	(%r14), %rax
	testq	%rax, %rax
	jle	.LBB27_4
# BB#2:                                 # %for.body.lr.ph
	movq	$1, %r15
	movq	$8, %rbx
	.align	16, 0x90
.LBB27_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r14), %rax
	movq	-8(%rax,%rbx), %rsi
	movq	(%rax,%rbx), %rdi
	callq	emit_move_insn
	movslq	(%r14), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r15
	addq	$16, %rbx
	cmpq	%rax, %rdx
	jl	.LBB27_3
.LBB27_4:                               # %for.end
	callq	get_insns
	movq	%rax, %rbx
	callq	end_sequence
	callq	get_insns
	movq	%rbx, %rdi
	movq	%rax, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	emit_insns_after  # TAILCALL
.LBB27_5:                               # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp389:
	.size	emit_initial_value_sets, .Ltmp389-emit_initial_value_sets
	.cfi_endproc

	.globl	allocate_initial_values
	.align	16, 0x90
	.type	allocate_initial_values,@function
allocate_initial_values:                # @allocate_initial_values
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp390:
	.size	allocate_initial_values, .Ltmp390-allocate_initial_values
	.cfi_endproc

	.align	16, 0x90
	.type	find_block,@function
find_block:                             # @find_block
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rsi), %rax
	movq	56(%rax), %rcx
	xorq	%rax, %rax
	cmpq	%rdi, %rcx
	je	.LBB29_3
# BB#1:                                 # %if.end
	movq	$-1, %rax
	ja	.LBB29_3
# BB#2:                                 # %if.end
	movq	$1, %rax
.LBB29_3:                               # %return
	retq
.Ltmp391:
	.size	find_block, .Ltmp391-find_block
	.cfi_endproc

	.align	16, 0x90
	.type	note_modified_parmregs,@function
note_modified_parmregs:                 # @note_modified_parmregs
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB30_6
# BB#1:                                 # %entry
	movzbl	in_nonparm_insns(%rip), %eax
	testq	$1, %rax
	je	.LBB30_6
# BB#2:                                 # %land.lhs.true1
	movq	cfun(%rip), %rcx
	movl	8(%rdi), %eax
	movl	304(%rcx), %ecx
	cmpq	%rcx, %rax
	jae	.LBB30_6
# BB#3:                                 # %land.lhs.true1
	cmpq	$53, %rax
	jb	.LBB30_6
# BB#4:                                 # %land.lhs.true8
	movq	parmdecl_map(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	testq	%rax, %rax
	je	.LBB30_6
# BB#5:                                 # %if.then
	movl	16(%rax), %ecx
	movabsq	$4294963199, %rdx       # imm = 0xFFFFEFFF
	andq	%rcx, %rdx
	movl	%edx, 16(%rax)
.LBB30_6:                               # %if.end
	retq
.Ltmp392:
	.size	note_modified_parmregs, .Ltmp392-note_modified_parmregs
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"function cannot be inline"
	.size	.L.str, 26

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"varargs function cannot be inline"
	.size	.L.str1, 34

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"function using alloca cannot be inline"
	.size	.L.str2, 39

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"function using setjmp cannot be inline"
	.size	.L.str3, 39

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"function uses __builtin_eh_return"
	.size	.L.str4, 34

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"function with nested functions cannot be inline"
	.size	.L.str5, 48

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"function with label addresses used in initializers cannot inline"
	.size	.L.str6, 65

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"function too large to be inline"
	.size	.L.str7, 32

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"inline functions not supported for this return value type"
	.size	.L.str8, 58

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"function with varying-size return value cannot be inline"
	.size	.L.str9, 57

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"function with varying-size parameter cannot be inline"
	.size	.L.str10, 54

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"function with transparent unit parameter cannot be inline"
	.size	.L.str11, 58

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"function with computed jump cannot inline"
	.size	.L.str12, 42

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"function with nonlocal goto cannot be inline"
	.size	.L.str13, 45

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"function with target specific attribute(s) cannot be inlined"
	.size	.L.str14, 61

	.type	parmdecl_map,@object    # @parmdecl_map
	.local	parmdecl_map
	.comm	parmdecl_map,8,8
	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"integrate.c"
	.size	.L.str15, 12

	.type	.L__FUNCTION__.save_for_inline,@object # @__FUNCTION__.save_for_inline
.L__FUNCTION__.save_for_inline:
	.asciz	"save_for_inline"
	.size	.L__FUNCTION__.save_for_inline, 16

	.type	in_nonparm_insns,@object # @in_nonparm_insns
	.local	in_nonparm_insns
	.comm	in_nonparm_insns,1,1
	.type	.L__FUNCTION__.expand_inline_function,@object # @__FUNCTION__.expand_inline_function
.L__FUNCTION__.expand_inline_function:
	.asciz	"expand_inline_function"
	.size	.L__FUNCTION__.expand_inline_function, 23

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"block_map"
	.size	.L.str16, 10

	.type	global_const_equiv_varray,@object # @global_const_equiv_varray
	.comm	global_const_equiv_varray,8,8
	.type	inlining,@object        # @inlining
	.local	inlining
	.comm	inlining,8,8
	.type	.L__FUNCTION__.copy_rtx_and_substitute,@object # @__FUNCTION__.copy_rtx_and_substitute
.L__FUNCTION__.copy_rtx_and_substitute:
	.asciz	"copy_rtx_and_substitute"
	.size	.L__FUNCTION__.copy_rtx_and_substitute, 24

	.type	.L__FUNCTION__.subst_constants,@object # @__FUNCTION__.subst_constants
.L__FUNCTION__.subst_constants:
	.asciz	"subst_constants"
	.size	.L__FUNCTION__.subst_constants, 16

	.type	.L__FUNCTION__.copy_insn_list,@object # @__FUNCTION__.copy_insn_list
.L__FUNCTION__.copy_insn_list:
	.asciz	"copy_insn_list"
	.size	.L__FUNCTION__.copy_insn_list, 15


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
