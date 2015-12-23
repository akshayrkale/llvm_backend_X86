	.file	"c-typeck.c.bc"
	.text
	.globl	require_complete_type
	.align	16, 0x90
	.type	require_complete_type,@function
require_complete_type:                  # @require_complete_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	movq	global_trees(%rip), %rax
	cmpq	%rdi, %rax
	je	.LBB0_5
# BB#1:                                 # %entry
	movq	8(%rdi), %rsi
	cmpq	%rax, %rsi
	je	.LBB0_5
# BB#2:                                 # %if.end
	cmpq	$0, 32(%rsi)
	je	.LBB0_4
# BB#3:
	movq	%rdi, %rax
	popq	%rdx
	retq
.LBB0_4:                                # %if.end6
	callq	incomplete_type_error
	movq	global_trees(%rip), %rax
.LBB0_5:                                # %return
	popq	%rdx
	retq
.Ltmp2:
	.size	require_complete_type, .Ltmp2-require_complete_type
	.cfi_endproc

	.globl	incomplete_type_error
	.align	16, 0x90
	.type	incomplete_type_error,@function
incomplete_type_error:                  # @incomplete_type_error
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp4:
	.cfi_def_cfa_offset 16
	movl	16(%rsi), %eax
	testq	$255, %rax
	je	.LBB1_24
# BB#1:                                 # %if.end
	testq	%rdi, %rdi
	je	.LBB1_6
# BB#2:                                 # %land.lhs.true
	movl	16(%rdi), %ecx
	andq	$254, %rcx
	cmpq	$34, %rcx
	jne	.LBB1_6
# BB#3:                                 # %if.then12
	movq	72(%rdi), %rax
	movq	32(%rax), %rsi
	movabsq	$.L.str, %rdi
	jmp	.LBB1_4
.LBB1_24:                               # %if.end54
	popq	%rax
	retq
.LBB1_5:                                # %if.end28
                                        #   in Loop: Header=BB1_6 Depth=1
	movq	8(%rsi), %rsi
	movl	16(%rsi), %eax
	.align	16, 0x90
.LBB1_6:                                # %retry
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rax
	cmpq	$17, %rax
	jle	.LBB1_7
# BB#10:                                # %retry
                                        #   in Loop: Header=BB1_6 Depth=1
	cmpq	$18, %rax
	jne	.LBB1_11
# BB#15:                                # %sw.bb20
                                        #   in Loop: Header=BB1_6 Depth=1
	movq	24(%rsi), %rax
	testq	%rax, %rax
	je	.LBB1_18
# BB#16:                                # %if.then22
                                        #   in Loop: Header=BB1_6 Depth=1
	cmpq	$0, 112(%rax)
	jne	.LBB1_5
# BB#17:                                # %if.then27
	movabsq	$.L.str5, %rdi
	jmp	.LBB1_14
.LBB1_7:                                # %retry
	cmpq	$5, %rax
	jne	.LBB1_8
# BB#13:                                # %sw.bb19
	movabsq	$.L.str4, %rdi
.LBB1_14:                               # %sw.bb19
	xorq	%rax, %rax
	popq	%rdx
	jmp	error  # TAILCALL
.LBB1_11:                               # %retry
	cmpq	$20, %rax
	jne	.LBB1_19
# BB#12:
	movabsq	$.L.str1, %rcx
	jmp	.LBB1_22
.LBB1_8:                                # %retry
	cmpq	$10, %rax
	jne	.LBB1_9
# BB#21:                                # %sw.epilog.loopexit1
	movabsq	$.L.str3, %rcx
	jmp	.LBB1_22
.LBB1_19:                               # %retry
	cmpq	$21, %rax
	jne	.LBB1_9
# BB#20:                                # %sw.epilog.loopexit80
	movabsq	$.L.str2, %rcx
.LBB1_22:                               # %sw.epilog
	movq	96(%rsi), %rax
	movzbl	16(%rax), %edx
	cmpq	$1, %rdx
	jne	.LBB1_23
# BB#25:                                # %if.then39
	movq	32(%rax), %rdx
	movabsq	$.L.str8, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	popq	%rcx
	jmp	error  # TAILCALL
.LBB1_23:                               # %if.else45
	movq	72(%rax), %rax
	movq	32(%rax), %rsi
	movabsq	$.L.str9, %rdi
.LBB1_4:                                # %if.then12
	xorq	%rax, %rax
	popq	%rdx
	jmp	error  # TAILCALL
.LBB1_18:                               # %if.end31
	movabsq	$.L.str6, %rdi
	jmp	.LBB1_14
.LBB1_9:                                # %sw.default
	movabsq	$.L.str7, %rax
	movabsq	$.L__FUNCTION__.incomplete_type_error, %rcx
	movq	$163, %rsi
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp5:
	.size	incomplete_type_error, .Ltmp5-incomplete_type_error
	.cfi_endproc

	.globl	common_type
	.align	16, 0x90
	.type	common_type,@function
common_type:                            # @common_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp12:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp13:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp14:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp15:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp16:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp17:
	.cfi_def_cfa_offset 64
.Ltmp18:
	.cfi_offset %rbx, -48
.Ltmp19:
	.cfi_offset %r12, -40
.Ltmp20:
	.cfi_offset %r13, -32
.Ltmp21:
	.cfi_offset %r14, -24
.Ltmp22:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %rbx
	cmpq	%r15, %rbx
	je	.LBB2_3
# BB#1:                                 # %if.end
	movq	global_trees(%rip), %rax
	cmpq	%rbx, %rax
	je	.LBB2_2
# BB#4:                                 # %if.end3
	cmpq	%r15, %rax
	je	.LBB2_3
# BB#5:                                 # %if.end6
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	*targetm+248(%rip)
	movzbl	16(%rbx), %ecx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	cmpq	$10, %rcx
	jne	.LBB2_7
# BB#6:                                 # %if.then8
	movl	60(%rbx), %edi
	andq	$511, %rdi              # imm = 0x1FF
	movq	$1, %rsi
	callq	type_for_size
	movq	%rax, %rbx
.LBB2_7:                                # %if.end12
	movl	16(%r15), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$10, %rcx
	jne	.LBB2_9
# BB#8:                                 # %if.then18
	movl	60(%r15), %edi
	andq	$511, %rdi              # imm = 0x1FF
	movq	$1, %rsi
	callq	type_for_size
	movq	%rax, %r15
	movl	16(%r15), %eax
.LBB2_9:                                # %if.end24
	movl	16(%rbx), %ecx
	movq	%rcx, %r14
	andq	$255, %r14
	movq	%rax, %r12
	andq	$255, %r12
	cmpq	$8, %r14
	je	.LBB2_11
# BB#10:                                # %if.end24
	cmpq	$8, %r12
	je	.LBB2_11
# BB#22:                                # %if.end63
	cmpq	$22, %r14
	jg	.LBB2_33
# BB#23:                                # %if.end63
	leaq	-6(%r14), %rsi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rsi
	cmpq	$2, %rsi
	jae	.LBB2_24
# BB#41:                                # %sw.bb
	cmpq	$7, %r14
	jne	.LBB2_43
# BB#42:                                # %sw.bb
	cmpq	$7, %r12
	jne	.LBB2_17
.LBB2_43:                               # %if.end69
	cmpq	$7, %r12
	jne	.LBB2_45
# BB#44:                                # %if.end69
	cmpq	$7, %r14
	jne	.LBB2_20
.LBB2_45:                               # %if.end75
	movl	60(%rbx), %esi
	movl	60(%r15), %edi
	andq	$511, %rsi              # imm = 0x1FF
	andq	$511, %rdi              # imm = 0x1FF
	cmpq	%rdi, %rsi
	ja	.LBB2_17
# BB#46:                                # %if.else87
	andq	%rdx, %rsi
	andq	%rdx, %rdi
	cmpq	%rsi, %rdi
	ja	.LBB2_20
# BB#47:                                # %if.end100
	movq	128(%rbx), %rdx
	movq	integer_types+64(%rip), %rdi
	cmpq	%rdi, %rdx
	je	.LBB2_69
# BB#48:                                # %lor.lhs.false103
	movq	128(%r15), %rsi
	cmpq	%rdi, %rsi
	je	.LBB2_69
# BB#49:                                # %if.end109
	movq	integer_types+56(%rip), %r8
	cmpq	%r8, %rdx
	je	.LBB2_51
# BB#50:                                # %if.end109
	cmpq	%r8, %rsi
	je	.LBB2_51
# BB#56:                                # %if.end132
	movq	global_trees+208(%rip), %rdi
	cmpq	%rdi, %rdx
	je	.LBB2_69
# BB#57:                                # %if.end132
	cmpq	%rdi, %rsi
	je	.LBB2_69
# BB#58:                                # %if.end142
	testq	$8192, %rcx             # imm = 0x2000
	jne	.LBB2_17
	jmp	.LBB2_20
.LBB2_2:
	movq	%r15, %rbx
.LBB2_3:                                # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_11:                               # %if.then35
	cmpq	$8, %r14
	movq	%rbx, %rdi
	jne	.LBB2_13
# BB#12:                                # %cond.true
	movq	8(%rbx), %rdi
.LBB2_13:                               # %cond.end
	cmpq	$8, %r12
	movq	%r15, %rsi
	jne	.LBB2_15
# BB#14:                                # %cond.true40
	movq	8(%r15), %rsi
.LBB2_15:                               # %cond.end44
	callq	common_type
	cmpq	$8, %r14
	jne	.LBB2_18
# BB#16:                                # %land.lhs.true
	movq	8(%rbx), %rcx
	cmpq	%rax, %rcx
	je	.LBB2_17
.LBB2_18:                               # %if.else
	cmpq	$8, %r12
	jne	.LBB2_21
# BB#19:                                # %land.lhs.true54
	movq	8(%r15), %rcx
	cmpq	%rax, %rcx
	je	.LBB2_20
.LBB2_21:                               # %if.else60
	movq	%rax, %rdi
	callq	build_complex_type
.LBB2_68:                               # %if.then294
	movq	%rax, %rdi
	jmp	.LBB2_69
.LBB2_33:                               # %if.end63
	cmpq	$23, %r14
	jne	.LBB2_17
# BB#34:                                # %sw.bb261
	movq	8(%rbx), %rdi
	movq	8(%r15), %rsi
	callq	common_type
	movq	8(%rbx), %rcx
	movq	24(%rbx), %r13
	movq	24(%r15), %r12
	cmpq	%rcx, %rax
	jne	.LBB2_36
# BB#35:                                # %sw.bb261
	testq	%r12, %r12
	je	.LBB2_17
.LBB2_36:                               # %if.end280
	movq	8(%r15), %rcx
	cmpq	%rcx, %rax
	jne	.LBB2_38
# BB#37:                                # %if.end280
	testq	%r13, %r13
	je	.LBB2_20
.LBB2_38:                               # %if.end290
	testq	%r13, %r13
	je	.LBB2_39
# BB#70:                                # %if.end299
	testq	%r12, %r12
	je	.LBB2_71
# BB#72:                                # %if.end308
	movq	%rax, (%rsp)            # 8-byte Spill
	xorq	%rdi, %rdi
	callq	pushlevel
	movq	$1, %rdi
	callq	declare_parm_level
	movq	%r13, %rdi
	callq	list_length
	movq	%rax, %rbx
	movq	$32, %rcx
                                        # kill: RAX<def> RBX<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB2_73
# BB#77:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rbx
	xorq	%r15, %r15
	xorq	%rax, %rax
.LBB2_78:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	incq	%r15
	movq	%r15, %rcx
	andq	%r14, %rcx
	cmpq	%rbx, %rcx
	jne	.LBB2_78
	jmp	.LBB2_74
.LBB2_24:                               # %if.end63
	cmpq	$13, %r14
	jne	.LBB2_25
# BB#59:                                # %sw.bb153
	movq	8(%rbx), %rbx
	movq	8(%r15), %r14
	movq	128(%rbx), %rdi
	movq	128(%r14), %rsi
	callq	common_type
	movl	16(%rbx), %edx
	movq	$12, %rcx
	movq	%rdx, %r10
	shrq	%cl, %r10
	movq	$10, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	movl	60(%rbx), %r11d
	movq	$19, %rcx
	shrq	%cl, %r11
	andq	$255, %rdx
	xorq	%r8, %r8
	cmpq	$20, %rdx
	movabsq	$0, %r9
	jne	.LBB2_63
# BB#60:                                # %land.rhs
	cmpq	$0, 8(%rbx)
	je	.LBB2_61
# BB#62:                                # %land.rhs
	movabsq	$1, %r9
	jmp	.LBB2_63
.LBB2_25:                               # %if.end63
	cmpq	$18, %r14
	jne	.LBB2_17
# BB#26:                                # %sw.bb225
	movq	8(%rbx), %rdi
	movq	8(%r15), %rsi
	callq	common_type
	movq	8(%rbx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB2_28
# BB#27:                                # %land.lhs.true234
	cmpq	$0, 24(%rbx)
	je	.LBB2_28
.LBB2_17:                               # %if.then51
	movq	%rbx, %rdi
	jmp	.LBB2_69
.LBB2_28:                               # %if.end239
	movq	8(%r15), %rcx
	cmpq	%rcx, %rax
	jne	.LBB2_30
# BB#29:                                # %land.lhs.true243
	cmpq	$0, 24(%r15)
	je	.LBB2_30
.LBB2_20:                               # %if.then58
	movq	%r15, %rdi
.LBB2_69:                               # %if.then107
	movq	8(%rsp), %rsi           # 8-byte Reload
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	build_type_attribute_variant  # TAILCALL
.LBB2_30:                               # %if.end249
	cmpq	$0, 24(%rbx)
	jne	.LBB2_32
# BB#31:                                # %select.mid
	movq	%r15, %rbx
.LBB2_32:                               # %select.end
	movq	24(%rbx), %rsi
	movq	%rax, %rdi
	callq	build_array_type
	jmp	.LBB2_68
.LBB2_61:
	movabsq	$0, %r9
.LBB2_63:                               # %land.end
	movq	$3, %rcx
	movl	16(%r14), %ebx
	shlq	%cl, %r9
	movq	$12, %rcx
	movq	%rbx, %r15
	shrq	%cl, %r15
	movq	$10, %rcx
	movq	%rbx, %rdi
	movl	60(%r14), %edx
	shrq	%cl, %rdi
	movq	$19, %rcx
	shrq	%cl, %rdx
	andq	$255, %rbx
	cmpq	$20, %rbx
	jne	.LBB2_67
# BB#64:                                # %land.rhs213
	cmpq	$0, 8(%r14)
	je	.LBB2_65
# BB#66:                                # %land.rhs213
	movabsq	$1, %r8
	jmp	.LBB2_67
.LBB2_65:
	movabsq	$0, %r8
.LBB2_67:                               # %land.end217
	andq	$1, %r10
	andq	$2, %rsi
	andq	$4, %r11
	andq	$1, %r15
	andq	$2, %rdi
	andq	$4, %rdx
	movq	$3, %rcx
	shlq	%cl, %r8
	orq	%r10, %rsi
	orq	%r11, %rsi
	orq	%r9, %rsi
	orq	%rdi, %rsi
	orq	%r15, %rsi
	orq	%rdx, %rsi
	orq	%r8, %rsi
	movq	%rax, %rdi
	callq	c_build_qualified_type
	movq	%rax, %rdi
	callq	build_pointer_type
	jmp	.LBB2_68
.LBB2_39:                               # %if.then294
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	build_function_type
	jmp	.LBB2_68
.LBB2_71:                               # %if.then303
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	build_function_type
	jmp	.LBB2_68
.LBB2_73:
	xorq	%rax, %rax
.LBB2_74:                               # %for.body314.lr.ph
	movq	%rax, %r15
	movq	%rax, %r14
	jmp	.LBB2_75
.LBB2_93:                               # %if.then399
                                        #   in Loop: Header=BB2_75 Depth=1
	movq	32(%r13), %rax
.LBB2_86:                               # %if.then358
                                        #   in Loop: Header=BB2_75 Depth=1
	movq	%rax, 32(%r15)
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB2_99
# BB#87:                                # %if.then364
                                        #   in Loop: Header=BB2_75 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str10, %rdi
	callq	pedwarn
	jmp	.LBB2_99
.LBB2_75:                               # %for.body314
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_83 Depth 2
                                        #     Child Loop BB2_92 Depth 2
	movq	32(%r13), %rax
	movq	32(%r12), %rsi
	testq	%rax, %rax
	jne	.LBB2_79
# BB#76:                                # %if.then316
                                        #   in Loop: Header=BB2_75 Depth=1
	movq	%rsi, 32(%r15)
	jmp	.LBB2_99
.LBB2_79:                               # %if.end321
                                        #   in Loop: Header=BB2_75 Depth=1
	testq	%rsi, %rsi
	je	.LBB2_98
# BB#80:                                # %if.end330
                                        #   in Loop: Header=BB2_75 Depth=1
	movzbl	16(%rax), %ecx
	cmpq	$21, %rcx
	jne	.LBB2_89
# BB#81:                                # %if.end330
                                        #   in Loop: Header=BB2_75 Depth=1
	cmpq	%rsi, %rax
	je	.LBB2_89
# BB#82:                                # %if.then344
                                        #   in Loop: Header=BB2_75 Depth=1
	movq	24(%rax), %rbx
	jmp	.LBB2_83
.LBB2_88:                               # %for.inc367
                                        #   in Loop: Header=BB2_83 Depth=2
	movq	(%rbx), %rbx
	movq	32(%r12), %rsi
.LBB2_83:                               # %if.then344
                                        #   Parent Loop BB2_75 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB2_89
# BB#84:                                # %for.body351
                                        #   in Loop: Header=BB2_83 Depth=2
	movq	8(%rbx), %rdi
	callq	comptypes
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_88
# BB#85:                                # %if.then358
                                        #   in Loop: Header=BB2_75 Depth=1
	movq	32(%r12), %rax
	jmp	.LBB2_86
.LBB2_89:                               # %if.end370
                                        #   in Loop: Header=BB2_75 Depth=1
	movzbl	16(%rsi), %eax
	cmpq	$21, %rax
	jne	.LBB2_97
# BB#90:                                # %land.lhs.true378
                                        #   in Loop: Header=BB2_75 Depth=1
	movq	32(%r13), %rax
	cmpq	%rax, %rsi
	je	.LBB2_97
# BB#91:                                # %if.then384
                                        #   in Loop: Header=BB2_75 Depth=1
	movq	24(%rsi), %rbx
	testq	%rbx, %rbx
	je	.LBB2_97
	jmp	.LBB2_92
.LBB2_95:                               # %for.inc408.for.body392_crit_edge
                                        #   in Loop: Header=BB2_92 Depth=2
	movq	32(%r13), %rax
.LBB2_92:                               # %for.body392
                                        #   Parent Loop BB2_75 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx), %rdi
	movq	%rax, %rsi
	callq	comptypes
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_93
# BB#94:                                # %for.inc408
                                        #   in Loop: Header=BB2_92 Depth=2
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB2_95
# BB#96:                                # %for.cond390.if.end412.loopexit_crit_edge
                                        #   in Loop: Header=BB2_75 Depth=1
	movq	32(%r12), %rsi
.LBB2_97:                               # %if.end412
                                        #   in Loop: Header=BB2_75 Depth=1
	movq	32(%r13), %rdi
	callq	common_type
.LBB2_98:                               # %for.inc420
                                        #   in Loop: Header=BB2_75 Depth=1
	movq	%rax, 32(%r15)
.LBB2_99:                               # %for.inc420
                                        #   in Loop: Header=BB2_75 Depth=1
	movq	(%r13), %r13
	movq	(%r12), %r12
	movq	(%r15), %r15
	testq	%r13, %r13
	jne	.LBB2_75
# BB#100:                               # %for.end427
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	poplevel
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%r14, %rsi
	callq	build_function_type
	movq	%rax, %rbx
	movq	%rbx, %rdi
	jmp	.LBB2_69
.LBB2_51:                               # %if.then117
	testq	$8192, %rcx             # imm = 0x2000
	jne	.LBB2_69
# BB#52:                                # %lor.lhs.false121
	testq	$8192, %rax             # imm = 0x2000
	je	.LBB2_53
# BB#54:                                # %lor.lhs.false121
	movabsq	$integer_types+64, %rax
	movq	(%rax), %rdi
	jmp	.LBB2_69
.LBB2_53:
	movabsq	$integer_types+56, %rax
	movq	(%rax), %rdi
	jmp	.LBB2_69
.Ltmp23:
	.size	common_type, .Ltmp23-common_type
	.cfi_endproc

	.globl	comptypes
	.align	16, 0x90
	.type	comptypes,@function
comptypes:                              # @comptypes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp30:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp31:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp32:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp33:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp34:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp35:
	.cfi_def_cfa_offset 64
.Ltmp36:
	.cfi_offset %rbx, -48
.Ltmp37:
	.cfi_offset %r12, -40
.Ltmp38:
	.cfi_offset %r13, -32
.Ltmp39:
	.cfi_offset %r14, -24
.Ltmp40:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %rbx
	cmpq	%r15, %rbx
	movq	$1, %r13
	je	.LBB3_90
# BB#1:                                 # %entry
	testq	%rbx, %rbx
	je	.LBB3_90
# BB#2:                                 # %entry
	testq	%r15, %r15
	je	.LBB3_90
# BB#3:                                 # %lor.lhs.false3
	movzbl	16(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB3_90
# BB#4:                                 # %lor.lhs.false5
	movzbl	16(%r15), %eax
	testq	%rax, %rax
	je	.LBB3_90
# BB#5:                                 # %if.end
	cmpq	$6, %rcx
	jne	.LBB3_9
# BB#6:                                 # %land.lhs.true
	movzbl	62(%rbx), %ecx
	testq	$2, %rcx
	je	.LBB3_9
# BB#7:                                 # %land.lhs.true19
	movq	24(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB3_9
# BB#8:                                 # %if.then22
	movq	%rcx, %rbx
.LBB3_9:                                # %if.end25
	cmpq	$6, %rax
	jne	.LBB3_13
# BB#10:                                # %land.lhs.true31
	movzbl	62(%r15), %eax
	testq	$2, %rax
	je	.LBB3_13
# BB#11:                                # %land.lhs.true38
	movq	24(%r15), %rax
	testq	%rax, %rax
	je	.LBB3_13
# BB#12:                                # %if.then42
	movq	%rax, %r15
.LBB3_13:                               # %if.end45
	movl	16(%rbx), %esi
	movq	%rsi, %rax
	andq	$255, %rax
	cmpq	$10, %rax
	jne	.LBB3_15
# BB#14:                                # %if.then51
	movl	60(%rbx), %edi
	andq	$511, %rdi              # imm = 0x1FF
	movq	$13, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	callq	type_for_size
	movq	%rax, %rbx
.LBB3_15:                               # %if.end59
	movl	16(%r15), %esi
	movq	%rsi, %rax
	andq	$255, %rax
	cmpq	$10, %rax
	jne	.LBB3_17
# BB#16:                                # %if.then65
	movl	60(%r15), %edi
	andq	$511, %rdi              # imm = 0x1FF
	movq	$13, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	callq	type_for_size
	movq	%rax, %r15
.LBB3_17:                               # %if.end76
	cmpq	%r15, %rbx
	je	.LBB3_90
# BB#18:                                # %if.end79
	movl	16(%rbx), %edi
	movl	16(%r15), %r11d
	movq	%rdi, %rsi
	andq	$255, %rsi
	movq	%r11, %rax
	andq	$255, %rax
	xorq	%r13, %r13
	cmpq	%rax, %rsi
	jne	.LBB3_90
# BB#19:                                # %if.end90
	movq	$12, %rcx
	movq	%rdi, %r8
	shrq	%cl, %r8
	movq	$10, %rcx
	shrq	%cl, %rdi
	movl	60(%rbx), %edx
	movq	$19, %rcx
	shrq	%cl, %rdx
	xorq	%r10, %r10
	cmpq	$20, %rsi
	movabsq	$0, %r9
	jne	.LBB3_23
# BB#20:                                # %land.rhs
	cmpq	$0, 8(%rbx)
	je	.LBB3_21
# BB#22:                                # %land.rhs
	movabsq	$1, %r9
	jmp	.LBB3_23
.LBB3_21:
	movabsq	$0, %r9
.LBB3_23:                               # %land.end
	movq	$3, %rcx
	shlq	%cl, %r9
	movq	$12, %rcx
	movq	%r11, %r14
	shrq	%cl, %r14
	movl	60(%r15), %eax
	movq	$10, %rcx
	shrq	%cl, %r11
	movq	$19, %rcx
	shrq	%cl, %rax
	cmpq	$20, %rsi
	jne	.LBB3_27
# BB#24:                                # %land.rhs141
	cmpq	$0, 8(%r15)
	je	.LBB3_25
# BB#26:                                # %land.rhs141
	movabsq	$1, %r10
	jmp	.LBB3_27
.LBB3_25:
	movabsq	$0, %r10
.LBB3_27:                               # %land.end145
	andq	$1, %r8
	andq	$2, %rdi
	orq	%r8, %rdi
	andq	$4, %rdx
	orq	%rdi, %rdx
	orq	%r9, %rdx
	andq	$1, %r14
	andq	$2, %r11
	orq	%r14, %r11
	andq	$4, %rax
	orq	%r11, %rax
	movq	$3, %rcx
	shlq	%cl, %r10
	orq	%rax, %r10
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %r10
	andq	%r12, %rdx
	xorq	%r13, %r13
	cmpq	%r10, %rdx
	jne	.LBB3_90
# BB#28:                                # %if.end151
	movq	128(%rbx), %rax
	movq	128(%r15), %rcx
	cmpq	%rcx, %rax
	movq	$1, %r13
	je	.LBB3_90
# BB#29:                                # %if.end157
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	*targetm+264(%rip)
	movq	%rax, %r14
	testq	%r12, %r14
	movabsq	$0, %r13
	je	.LBB3_90
# BB#30:                                # %if.end161
	movzbl	16(%rbx), %eax
	xorq	%r8, %r8
	cmpq	$17, %rax
	jg	.LBB3_34
# BB#31:                                # %if.end161
	cmpq	$13, %rax
	jne	.LBB3_84
# BB#32:                                # %sw.bb
	movq	8(%rbx), %rdi
	movq	8(%r15), %rsi
	cmpq	%rsi, %rdi
	movq	$1, %r8
	je	.LBB3_84
# BB#33:                                # %cond.false
	callq	comptypes
	movq	%rax, %r8
	jmp	.LBB3_84
.LBB3_34:                               # %if.end161
	cmpq	$18, %rax
	je	.LBB3_56
# BB#35:                                # %if.end161
	cmpq	$20, %rax
	jne	.LBB3_36
# BB#81:                                # %sw.bb279
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	maybe_objc_comptypes
	andq	%r12, %rax
	cmpq	$1, %rax
	jne	.LBB3_82
# BB#83:                                # %sw.bb279
	movabsq	$1, %r8
	jmp	.LBB3_84
.LBB3_56:                               # %sw.bb178
	movq	8(%rbx), %rdi
	movq	24(%rbx), %rbx
	movq	8(%r15), %rsi
	movq	24(%r15), %r15
	cmpq	%rsi, %rdi
	movq	$1, %r8
	je	.LBB3_58
# BB#57:                                # %land.lhs.true188
	callq	comptypes
	movq	%rax, %r8
	xorq	%r13, %r13
	testq	%r12, %r8
	je	.LBB3_90
.LBB3_58:                               # %if.end196
	testq	%rbx, %rbx
	je	.LBB3_84
# BB#59:                                # %if.end196
	testq	%r15, %r15
	je	.LBB3_84
# BB#60:                                # %if.end196
	cmpq	%r15, %rbx
	je	.LBB3_84
# BB#61:                                # %if.end203
	movq	112(%rbx), %rcx
	movq	112(%r15), %rax
	xorq	%rsi, %rsi
	testq	%rcx, %rcx
	movabsq	$0, %rdx
	je	.LBB3_66
# BB#62:                                # %land.rhs212
	movq	104(%rbx), %rdx
	movzbl	16(%rdx), %edx
	cmpq	$25, %rdx
	movq	$1, %rdx
	jne	.LBB3_66
# BB#63:                                # %lor.rhs
	movzbl	16(%rcx), %edx
	cmpq	$25, %rdx
	je	.LBB3_64
# BB#65:                                # %lor.rhs
	movabsq	$1, %rdx
	jmp	.LBB3_66
.LBB3_36:                               # %if.end161
	cmpq	$23, %rax
	jne	.LBB3_84
# BB#37:                                # %sw.bb176
	movq	8(%rbx), %rdi
	movq	8(%r15), %rsi
	cmpq	%rsi, %rdi
	movq	$1, %rax
	je	.LBB3_40
# BB#38:                                # %lor.lhs.false.i
	callq	comptypes
	testq	%r12, %rax
	je	.LBB3_39
.LBB3_40:                               # %if.end.i
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	24(%rbx), %r13
	movq	24(%r15), %rsi
	testq	%r13, %r13
	je	.LBB3_41
# BB#47:                                # %if.end24.i
	testq	%rsi, %rsi
	je	.LBB3_48
# BB#54:                                # %if.end41.i
	movq	%r13, %rdi
	callq	type_lists_compatible_p
	movq	%rax, %r8
                                        # kill: RAX<def> R8<kill>
	andq	%r12, %rax
	cmpq	$1, %rax
	jne	.LBB3_84
# BB#55:                                # %if.end41.i
	movq	8(%rsp), %r8            # 8-byte Reload
	jmp	.LBB3_84
.LBB3_82:
	movabsq	$0, %r8
	jmp	.LBB3_84
.LBB3_41:                               # %if.then11.i
	movq	%rsi, %rdi
	movq	%rsi, %r15
	callq	self_promoting_args_p
	testq	%r12, %rax
	movabsq	$0, %r8
	je	.LBB3_84
# BB#42:                                # %if.end15.i
	movq	136(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB3_43
# BB#44:                                # %land.lhs.true.i
	movq	%r15, %rdi
	callq	type_lists_compatible_p
	andq	%r12, %rax
	cmpq	$1, %rax
	je	.LBB3_46
# BB#45:                                # %land.lhs.true.i
	movq	$2, 8(%rsp)             # 8-byte Folded Spill
.LBB3_46:                               # %land.lhs.true.i
	movq	8(%rsp), %r8            # 8-byte Reload
	jmp	.LBB3_84
.LBB3_39:
	xorq	%r8, %r8
	jmp	.LBB3_84
.LBB3_48:                               # %if.then26.i
	movq	%r13, %rdi
	callq	self_promoting_args_p
	testq	%r12, %rax
	movabsq	$0, %r8
	je	.LBB3_84
# BB#49:                                # %if.end30.i
	movq	136(%r15), %rax
	testq	%rax, %rax
	je	.LBB3_50
# BB#51:                                # %land.lhs.true34.i
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	type_lists_compatible_p
	andq	%r12, %rax
	cmpq	$1, %rax
	je	.LBB3_53
# BB#52:                                # %land.lhs.true34.i
	movq	$2, 8(%rsp)             # 8-byte Folded Spill
.LBB3_53:                               # %land.lhs.true34.i
	movq	8(%rsp), %r8            # 8-byte Reload
	jmp	.LBB3_84
.LBB3_64:
	movabsq	$0, %rdx
.LBB3_66:                               # %land.end226
	testq	%rax, %rax
	je	.LBB3_71
# BB#67:                                # %land.rhs230
	movq	104(%r15), %rsi
	movzbl	16(%rsi), %esi
	cmpq	$25, %rsi
	jne	.LBB3_84
# BB#68:                                # %lor.rhs238
	movzbl	16(%rax), %esi
	cmpq	$25, %rsi
	je	.LBB3_69
# BB#70:                                # %lor.rhs238
	movabsq	$1, %rsi
	jmp	.LBB3_71
.LBB3_69:
	movabsq	$0, %rsi
.LBB3_71:                               # %land.end247
	movq	%rcx, %rdi
	orq	%rax, %rdi
	jne	.LBB3_72
# BB#73:                                # %land.end247
	movabsq	$1, %rdi
	jmp	.LBB3_74
.LBB3_72:
	movabsq	$0, %rdi
.LBB3_74:                               # %land.end247
	orq	%rsi, %rdx
	jne	.LBB3_84
# BB#75:                                # %land.end247
	testq	%rdi, %rdi
	jne	.LBB3_84
# BB#76:                                # %if.end259
	testq	%rcx, %rcx
	je	.LBB3_80
# BB#77:                                # %if.end259
	testq	%rax, %rax
	je	.LBB3_80
# BB#78:                                # %lor.lhs.false263
	movq	104(%rbx), %rax
	movq	104(%r15), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	movq	%r8, %r13
	callq	tree_int_cst_equal
	movq	%r13, %rdx
	testq	%r12, %rax
	je	.LBB3_80
# BB#79:                                # %lor.lhs.false270
	movq	112(%rbx), %rax
	movq	112(%r15), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	movq	%rdx, %rbx
	callq	tree_int_cst_equal
	movq	%rbx, %r8
	testq	%r12, %rax
	jne	.LBB3_84
.LBB3_80:                               # %if.then277
	xorq	%r8, %r8
	jmp	.LBB3_84
.LBB3_43:
	movq	8(%rsp), %r8            # 8-byte Reload
	jmp	.LBB3_84
.LBB3_50:
	movq	8(%rsp), %r8            # 8-byte Reload
.LBB3_84:                               # %sw.epilog
	andq	%r12, %r14
	cmpq	$2, %r14
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	jne	.LBB3_86
# BB#85:                                # %sw.epilog
	movq	%rdx, %rcx
.LBB3_86:                               # %sw.epilog
	andq	%r8, %r12
	cmpq	$1, %r12
	jne	.LBB3_88
# BB#87:                                # %sw.epilog
	movq	%rdx, %rax
.LBB3_88:                               # %sw.epilog
	testq	%rax, %rcx
	movq	$2, %r13
	jne	.LBB3_90
# BB#89:                                # %sw.epilog
	movq	%r8, %r13
.LBB3_90:                               # %return
	movq	%r13, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp41:
	.size	comptypes, .Ltmp41-comptypes
	.cfi_endproc

	.globl	c_sizeof
	.align	16, 0x90
	.type	c_sizeof,@function
c_sizeof:                               # @c_sizeof
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp44:
	.cfi_def_cfa_offset 16
.Ltmp45:
	.cfi_offset %rbx, -16
	movzbl	16(%rdi), %eax
	testq	%rax, %rax
	je	.LBB4_8
# BB#1:                                 # %entry
	cmpq	$5, %rax
	jne	.LBB4_2
# BB#6:                                 # %if.then5
	movl	pedantic(%rip), %eax
	movl	warn_pointer_arith(%rip), %ecx
	orq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	je	.LBB4_8
# BB#7:                                 # %if.then9
	movabsq	$.L.str12, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB4_8:                                # %if.then13
	movq	global_trees+128(%rip), %rdx
	jmp	.LBB4_12
.LBB4_2:                                # %entry
	cmpq	$23, %rax
	jne	.LBB4_9
# BB#3:                                 # %if.then
	movl	pedantic(%rip), %eax
	movl	warn_pointer_arith(%rip), %ecx
	orq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	je	.LBB4_5
# BB#4:                                 # %if.then3
	movabsq	$.L.str11, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB4_5:                                # %if.end
	movq	global_trees+128(%rip), %rdx
	jmp	.LBB4_12
.LBB4_9:                                # %if.else14
	cmpq	$0, 32(%rdi)
	je	.LBB4_10
# BB#11:                                # %if.else19
	movq	integer_types(%rip), %rax
	movl	60(%rax), %eax
	movq	40(%rdi), %rbx
	movq	$3, %rcx
	shrq	%cl, %rax
	andq	$63, %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	size_int_wide
	movq	$63, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, %rdx
	jmp	.LBB4_12
.LBB4_10:                               # %if.then18
	movabsq	$.L.str13, %rdi
	xorq	%rax, %rax
	callq	error
	movq	global_trees+120(%rip), %rdx
.LBB4_12:                               # %if.end28
	movq	c_global_trees+32(%rip), %rsi
	movq	$115, %rdi
	callq	build1
	movq	%rax, %rdi
	popq	%rbx
	jmp	fold  # TAILCALL
.Ltmp46:
	.size	c_sizeof, .Ltmp46-c_sizeof
	.cfi_endproc

	.globl	c_sizeof_nowarn
	.align	16, 0x90
	.type	c_sizeof_nowarn,@function
c_sizeof_nowarn:                        # @c_sizeof_nowarn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp49:
	.cfi_def_cfa_offset 16
.Ltmp50:
	.cfi_offset %rbx, -16
	movzbl	16(%rdi), %ecx
	cmpq	$23, %rcx
	ja	.LBB5_3
# BB#1:                                 # %entry
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$8388641, %rax          # imm = 0x800021
	je	.LBB5_3
# BB#2:                                 # %if.then
	movq	global_trees+128(%rip), %rdx
	jmp	.LBB5_6
.LBB5_3:                                # %if.else
	cmpq	$0, 32(%rdi)
	je	.LBB5_4
# BB#5:                                 # %if.else9
	movq	integer_types(%rip), %rax
	movl	60(%rax), %eax
	movq	40(%rdi), %rbx
	movq	$3, %rcx
	shrq	%cl, %rax
	andq	$63, %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	size_int_wide
	movq	$63, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, %rdx
	jmp	.LBB5_6
.LBB5_4:                                # %if.then8
	movq	global_trees+120(%rip), %rdx
.LBB5_6:                                # %if.end15
	movq	c_global_trees+32(%rip), %rsi
	movq	$115, %rdi
	callq	build1
	movq	%rax, %rdi
	popq	%rbx
	jmp	fold  # TAILCALL
.Ltmp51:
	.size	c_sizeof_nowarn, .Ltmp51-c_sizeof_nowarn
	.cfi_endproc

	.globl	c_size_in_bytes
	.align	16, 0x90
	.type	c_size_in_bytes,@function
c_size_in_bytes:                        # @c_size_in_bytes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp54:
	.cfi_def_cfa_offset 16
.Ltmp55:
	.cfi_offset %rbx, -16
	movzbl	16(%rdi), %ecx
	cmpq	$23, %rcx
	ja	.LBB6_2
# BB#1:                                 # %entry
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$8388641, %rax          # imm = 0x800021
	jne	.LBB6_5
.LBB6_2:                                # %if.end
	cmpq	$0, 32(%rdi)
	jne	.LBB6_6
# BB#3:                                 # %if.end
	cmpq	$5, %rcx
	je	.LBB6_6
# BB#4:                                 # %if.then13
	movabsq	$.L.str14, %rdi
	xorq	%rax, %rax
	callq	error
.LBB6_5:                                # %return
	movq	global_trees+128(%rip), %rax
	popq	%rbx
	retq
.LBB6_6:                                # %if.end14
	movq	integer_types(%rip), %rax
	movl	60(%rax), %eax
	movq	40(%rdi), %rbx
	movq	$3, %rcx
	shrq	%cl, %rax
	andq	$63, %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	size_int_wide
	movq	$63, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	size_binop  # TAILCALL
.Ltmp56:
	.size	c_size_in_bytes, .Ltmp56-c_size_in_bytes
	.cfi_endproc

	.globl	decl_constant_value
	.align	16, 0x90
	.type	decl_constant_value,@function
decl_constant_value:                    # @decl_constant_value
	.cfi_startproc
# BB#0:                                 # %entry
	cmpq	$0, current_function_decl(%rip)
	je	.LBB7_6
# BB#1:                                 # %land.lhs.true
	movl	16(%rdi), %eax
	andq	$6144, %rax             # imm = 0x1800
	cmpq	$4096, %rax             # imm = 0x1000
	jne	.LBB7_6
# BB#2:                                 # %land.lhs.true7
	movq	104(%rdi), %rax
	testq	%rax, %rax
	je	.LBB7_6
# BB#3:                                 # %land.lhs.true10
	movl	16(%rax), %ecx
	testq	$512, %rcx              # imm = 0x200
	je	.LBB7_6
# BB#4:                                 # %switch.early.test
	andq	$255, %rcx
	je	.LBB7_6
# BB#5:                                 # %switch.early.test
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$46, %rcx
	jne	.LBB7_7
.LBB7_6:                                # %if.end
	movq	%rdi, %rax
.LBB7_7:                                # %return
	retq
.Ltmp57:
	.size	decl_constant_value, .Ltmp57-decl_constant_value
	.cfi_endproc

	.globl	default_conversion
	.align	16, 0x90
	.type	default_conversion,@function
default_conversion:                     # @default_conversion
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp62:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp63:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp64:
	.cfi_def_cfa_offset 32
.Ltmp65:
	.cfi_offset %rbx, -32
.Ltmp66:
	.cfi_offset %r14, -24
.Ltmp67:
	.cfi_offset %r15, -16
	movq	8(%rdi), %r14
	movzbl	16(%r14), %r15d
	cmpq	$18, %r15
	je	.LBB8_54
# BB#1:                                 # %entry
	cmpq	$23, %r15
	jne	.LBB8_2
.LBB8_54:                               # %if.then
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	default_function_array_conversion  # TAILCALL
.LBB8_2:                                # %if.end
	movl	16(%rdi), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$32, %rcx
	jne	.LBB8_4
# BB#3:                                 # %if.then10
	movq	104(%rdi), %rdi
	jmp	.LBB8_17
.LBB8_4:                                # %if.else
	movl	optimize(%rip), %edx
	testq	%rdx, %rdx
	je	.LBB8_17
# BB#5:                                 # %land.lhs.true
	cmpq	$34, %rcx
	jne	.LBB8_17
# BB#6:                                 # %land.lhs.true
	cmpq	$18, %r15
	je	.LBB8_17
# BB#7:                                 # %if.then18
	movl	pedantic(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB8_15
# BB#8:                                 # %lor.lhs.false.i
	movzbl	48(%rdi), %ecx
	cmpq	$51, %rcx
	je	.LBB8_15
# BB#9:                                 # %if.else.i
	cmpq	$0, current_function_decl(%rip)
	je	.LBB8_15
# BB#10:                                # %if.else.i
	andq	$6144, %rax             # imm = 0x1800
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$4096, %rax             # imm = 0x1000
	jne	.LBB8_15
# BB#11:                                # %land.lhs.true7.i.i
	movq	104(%rdi), %rax
	testq	%rax, %rax
	je	.LBB8_15
# BB#12:                                # %land.lhs.true10.i.i
	movl	16(%rax), %edx
	testq	$512, %rdx              # imm = 0x200
	je	.LBB8_15
# BB#13:                                # %switch.early.test.i.i
	andq	$255, %rdx
	je	.LBB8_15
# BB#14:                                # %switch.early.test.i.i
	andq	%rcx, %rdx
	cmpq	$46, %rdx
	jne	.LBB8_16
.LBB8_15:                               # %if.end.i.i
	movq	%rdi, %rax
.LBB8_16:                               # %decl_constant_value_for_broken_optimization.exit
	movq	8(%rax), %r14
	movq	%rax, %rdi
.LBB8_17:                               # %if.end23
	movq	%rdi, %rcx
	jmp	.LBB8_18
	.align	16, 0x90
.LBB8_20:                               # %while.cond.while.body_crit_edge
                                        #   in Loop: Header=BB8_18 Depth=1
	movq	32(%rbx), %rcx
.LBB8_18:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rbx
	movzbl	16(%rbx), %eax
	cmpq	$115, %rax
	jne	.LBB8_19
# BB#21:                                # %land.rhs
                                        #   in Loop: Header=BB8_18 Depth=1
	movq	32(%rbx), %rcx
	movq	8(%rbx), %rdx
	movq	8(%rcx), %rsi
	cmpq	%rdx, %rsi
	je	.LBB8_18
	jmp	.LBB8_22
	.align	16, 0x90
.LBB8_19:                               # %while.cond
                                        #   in Loop: Header=BB8_18 Depth=1
	cmpq	$116, %rax
	je	.LBB8_20
.LBB8_22:                               # %while.end
	movzbl	tree_code_type(%rax), %ecx
	addq	$-49, %rcx
	andq	$255, %rcx
	cmpq	$52, %rcx
	ja	.LBB8_25
# BB#23:                                # %while.end
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$4503599627372547, %rcx # imm = 0x10000000000803
	testq	%rcx, %rdx
	je	.LBB8_25
# BB#24:                                # %if.then80
	movl	24(%rdi), %ecx
	movl	%ecx, 24(%rbx)
.LBB8_25:                               # %if.end84
	cmpq	$10, %r15
	jne	.LBB8_32
# BB#26:                                # %if.then87
	movq	integer_types+40(%rip), %rax
	movl	60(%r14), %edi
	movl	60(%rax), %edx
	movq	%rdi, %rax
	andq	$511, %rax              # imm = 0x1FF
	movq	%rdx, %rcx
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rcx, %rax
	ja	.LBB8_28
# BB#27:                                # %if.then87
	movq	%rdx, %rdi
.LBB8_28:                               # %if.then87
	movl	flag_traditional(%rip), %edx
	andq	$511, %rdi              # imm = 0x1FF
	testq	%rdx, %rdx
	jne	.LBB8_30
# BB#29:                                # %if.then87
	xorq	%rsi, %rsi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	andq	%rdx, %rax
	cmpq	%rcx, %rax
	jb	.LBB8_31
.LBB8_30:                               # %land.rhs117
	movzbl	17(%r14), %esi
	andq	$32, %rsi
	movq	$5, %rcx
	shrq	%cl, %rsi
.LBB8_31:                               # %land.end122
	callq	type_for_size
	movq	%rax, %rdi
	jmp	.LBB8_46
.LBB8_32:                               # %if.end125
	cmpq	$39, %rax
	jne	.LBB8_40
# BB#33:                                # %land.lhs.true132
	movq	40(%rbx), %rax
	movzbl	53(%rax), %ecx
	testq	$2, %rcx
	je	.LBB8_40
# BB#34:                                # %land.lhs.true142
	movq	integer_types+40(%rip), %rcx
	movl	60(%rcx), %esi
	movq	40(%rax), %rdi
	andq	$511, %rsi              # imm = 0x1FF
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB8_35
.LBB8_40:                               # %if.end169
	movq	%r14, %rdi
	callq	c_promoting_integer_type_p
	testq	%rax, %rax
	je	.LBB8_47
# BB#41:                                # %if.then171
	movzbl	17(%r14), %eax
	testq	$32, %rax
	je	.LBB8_42
# BB#43:                                # %land.lhs.true178
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	jne	.LBB8_45
# BB#44:                                # %lor.lhs.false180
	movq	integer_types+40(%rip), %rdi
	movl	60(%r14), %eax
	movl	60(%rdi), %ecx
	xorq	%rax, %rcx
	testq	$511, %rcx              # imm = 0x1FF
	jne	.LBB8_46
.LBB8_45:                               # %if.then191
	movq	integer_types+48(%rip), %rdi
	jmp	.LBB8_46
.LBB8_47:                               # %if.end195
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB8_51
# BB#48:                                # %if.end195
	movl	flag_allow_single_precision(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB8_51
# BB#49:                                # %land.lhs.true199
	movq	128(%r14), %rax
	movq	global_trees+192(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB8_50
.LBB8_51:                               # %if.end205
	cmpq	$5, %r15
	jne	.LBB8_53
# BB#52:                                # %if.then208
	movabsq	$.L.str15, %rdi
	xorq	%rax, %rax
	callq	error
	movq	global_trees(%rip), %rbx
.LBB8_53:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB8_42:                               # %if.then171.if.end193_crit_edge
	movq	integer_types+40(%rip), %rdi
	jmp	.LBB8_46
.LBB8_35:                               # %if.then155
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB8_38
# BB#36:                                # %land.lhs.true157
	movzbl	17(%r14), %eax
	testq	$32, %rax
	je	.LBB8_38
# BB#37:
	movabsq	$integer_types+48, %rax
	movq	(%rax), %rdi
	jmp	.LBB8_46
.LBB8_50:                               # %if.then203
	movq	global_trees+200(%rip), %rdi
	jmp	.LBB8_46
.LBB8_38:                               # %cond.false165
	movabsq	$integer_types+40, %rax
	movq	(%rax), %rdi
.LBB8_46:                               # %if.end193
	movq	%rbx, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	convert  # TAILCALL
.Ltmp68:
	.size	default_conversion, .Ltmp68-default_conversion
	.cfi_endproc

	.align	16, 0x90
	.type	default_function_array_conversion,@function
default_function_array_conversion:      # @default_function_array_conversion
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp75:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp76:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp77:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp78:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp79:
	.cfi_def_cfa_offset 48
.Ltmp80:
	.cfi_offset %rbx, -48
.Ltmp81:
	.cfi_offset %r12, -40
.Ltmp82:
	.cfi_offset %r13, -32
.Ltmp83:
	.cfi_offset %r14, -24
.Ltmp84:
	.cfi_offset %r15, -16
	movq	8(%rdi), %r9
	movzbl	16(%r9), %r8d
	xorq	%r14, %r14
	movq	%rdi, %rcx
	jmp	.LBB9_1
.LBB9_3:                                # %lor.end.thread
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	32(%r13), %rcx
	movq	$1, %r14
	.align	16, 0x90
.LBB9_1:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %r13
	movl	16(%r13), %edx
	movq	%rdx, %rsi
	andq	$255, %rsi
	cmpq	$115, %rsi
	jne	.LBB9_2
# BB#5:                                 # %lor.end
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	32(%r13), %rcx
	movq	8(%r13), %rax
	movq	8(%rcx), %rbx
	cmpq	%rax, %rbx
	movq	$115, %rax
	je	.LBB9_1
	jmp	.LBB9_6
.LBB9_2:                                # %while.cond
                                        #   in Loop: Header=BB9_1 Depth=1
	cmpq	$116, %rsi
	je	.LBB9_3
# BB#4:                                 # %lor.end.thread303
	movq	%rsi, %rax
.LBB9_6:                                # %while.end
	movzbl	tree_code_type(%rax), %ecx
	addq	$-49, %rcx
	andq	$255, %rcx
	cmpq	$52, %rcx
	ja	.LBB9_9
# BB#7:                                 # %while.end
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$4503599627372547, %rcx # imm = 0x10000000000803
	testq	%rcx, %rax
	je	.LBB9_9
# BB#8:                                 # %if.then63
	movl	24(%rdi), %eax
	movl	%eax, 24(%r13)
.LBB9_9:                                # %if.end67
	cmpq	$18, %r8
	jne	.LBB9_10
# BB#12:                                # %if.then74
	movzbl	tree_code_type(%rsi), %eax
	movq	8(%r9), %rdi
	cmpq	$100, %rax
	je	.LBB9_14
# BB#13:                                # %if.then74
	xorq	%r8, %r8
	cmpq	$114, %rax
	movabsq	$0, %rsi
	jne	.LBB9_15
.LBB9_14:                               # %if.then96
	movq	$12, %rcx
	movq	%rdx, %r8
	shrq	%cl, %r8
	andq	$1, %r8
	movq	$11, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$1, %rsi
.LBB9_15:                               # %if.end104
	movl	16(%r9), %r15d
	movq	$12, %rcx
	movq	%r15, %r10
	shrq	%cl, %r10
	movq	$10, %rcx
	movq	%r15, %r11
	shrq	%cl, %r11
	movl	60(%r9), %r9d
	movq	$19, %rcx
	shrq	%cl, %r9
	andq	$255, %r15
	cmpq	$20, %r15
	movabsq	$0, %rcx
	movabsq	$1, %r12
	movq	%rcx, %rax
	jne	.LBB9_17
# BB#16:                                # %if.end104
	movq	%r12, %rax
.LBB9_17:                               # %if.end104
	andq	$1, %r10
	andq	$2, %r11
	andq	$4, %r9
	testq	%rdi, %rdi
	je	.LBB9_19
# BB#18:                                # %if.end104
	movq	%r12, %rcx
.LBB9_19:                               # %if.end104
	andq	%rcx, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	movq	%rsi, %rcx
	orq	%r8, %rcx
	orq	%r11, %rcx
	orq	%r10, %rcx
	orq	%r9, %rcx
	orq	%rax, %rcx
	je	.LBB9_25
# BB#20:                                # %if.then139
	cmpq	$20, %r15
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, %rax
	jne	.LBB9_22
# BB#21:                                # %if.then139
	movq	%rdx, %rax
.LBB9_22:                               # %if.then139
	testq	%rdi, %rdi
	je	.LBB9_24
# BB#23:                                # %if.then139
	movq	%rdx, %rcx
.LBB9_24:                               # %if.then139
	andq	%rcx, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	addq	%rsi, %rsi
	orq	%r8, %rsi
	orq	%r11, %rsi
	orq	%r10, %rsi
	orq	%r9, %rsi
	orq	%rax, %rsi
	callq	c_build_qualified_type
	movl	16(%r13), %edx
	movq	%rax, %rdi
.LBB9_25:                               # %if.end179
	movq	%rdx, %rax
	andq	$255, %rax
	cmpq	$47, %rax
	jne	.LBB9_26
# BB#29:                                # %if.then199
	movq	40(%r13), %rdi
	callq	default_conversion
	movq	%rax, %rcx
	movq	8(%rcx), %rsi
	movq	32(%r13), %rdx
	movq	$47, %rdi
	xorq	%rax, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	build  # TAILCALL
.LBB9_10:                               # %if.end67
	cmpq	$23, %r8
	jne	.LBB9_62
# BB#11:                                # %if.then70
	movq	$121, %rdi
	xorq	%rdx, %rdx
	movq	%r13, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	build_unary_op  # TAILCALL
.LBB9_26:                               # %if.end179
	cmpq	$41, %rax
	jne	.LBB9_30
# BB#27:                                # %if.then186
	movq	72(%rdi), %rdi
	movq	32(%r13), %rsi
	jmp	.LBB9_28
.LBB9_30:                               # %if.end210
	xorq	%rax, %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %r14
	jne	.LBB9_55
# BB#31:
	movq	$1, %rax
	movabsq	$11115375362048, %r8    # imm = 0xA1C00000000
	movq	%r13, %rsi
	jmp	.LBB9_32
.LBB9_46:                               # %sw.bb.i
                                        #   in Loop: Header=BB9_32 Depth=1
	movq	32(%rsi), %rsi
	movl	16(%rsi), %edx
.LBB9_32:                               # %tailrecurse.i
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rdx
	cmpq	$169, %rdx
	jg	.LBB9_53
# BB#33:                                # %tailrecurse.i
                                        #   in Loop: Header=BB9_32 Depth=1
	cmpq	$28, %rdx
	jle	.LBB9_34
# BB#40:                                # %tailrecurse.i
                                        #   in Loop: Header=BB9_32 Depth=1
	cmpq	$119, %rdx
	jg	.LBB9_47
# BB#41:                                # %tailrecurse.i
                                        #   in Loop: Header=BB9_32 Depth=1
	cmpq	$51, %rdx
	jg	.LBB9_49
# BB#42:                                # %tailrecurse.i
                                        #   in Loop: Header=BB9_32 Depth=1
	cmpq	$43, %rdx
	ja	.LBB9_54
# BB#43:                                # %tailrecurse.i
                                        #   in Loop: Header=BB9_32 Depth=1
	movq	$1, %rbx
	movq	%rdx, %rcx
	shlq	%cl, %rbx
	testq	%r8, %rbx
	jne	.LBB9_35
# BB#44:                                # %tailrecurse.i
                                        #   in Loop: Header=BB9_32 Depth=1
	cmpq	$29, %rdx
	je	.LBB9_55
# BB#45:                                # %tailrecurse.i
                                        #   in Loop: Header=BB9_32 Depth=1
	cmpq	$39, %rdx
	je	.LBB9_46
	jmp	.LBB9_54
.LBB9_47:                               # %tailrecurse.i
                                        #   in Loop: Header=BB9_32 Depth=1
	leaq	-127(%rdx), %rcx
	andq	%r15, %rcx
	cmpq	$2, %rcx
	jb	.LBB9_46
# BB#48:                                # %tailrecurse.i
	cmpq	$120, %rdx
	je	.LBB9_50
	jmp	.LBB9_54
.LBB9_53:                               # %tailrecurse.i
	cmpq	$170, %rdx
	je	.LBB9_55
	jmp	.LBB9_54
.LBB9_34:                               # %tailrecurse.i
	testq	%rdx, %rdx
	jne	.LBB9_54
.LBB9_35:                               # %sw.bb3.i
	movq	8(%rsi), %rax
	movzbl	16(%rax), %esi
	cmpq	$16, %rsi
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, %rax
	je	.LBB9_37
# BB#36:                                # %sw.bb3.i
	movq	%rdx, %rax
.LBB9_37:                               # %sw.bb3.i
	cmpq	$23, %rsi
	je	.LBB9_39
# BB#38:                                # %sw.bb3.i
	movq	%rdx, %rcx
.LBB9_39:                               # %sw.bb3.i
	andq	%rcx, %rax
	jmp	.LBB9_55
.LBB9_49:                               # %tailrecurse.i
	cmpq	$52, %rdx
	jne	.LBB9_54
.LBB9_50:                               # %sw.bb16.i
	movq	8(%rsi), %rax
	movzbl	16(%rax), %eax
	cmpq	$18, %rax
	jne	.LBB9_51
# BB#52:                                # %sw.bb16.i
	movabsq	$1, %rax
	jmp	.LBB9_55
.LBB9_54:                               # %lvalue_p.exit.loopexit
	xorq	%rax, %rax
.LBB9_55:                               # %land.end215
	movl	flag_isoc99(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB9_57
# BB#56:                                # %land.end215
	testq	%rax, %rax
	je	.LBB9_62
.LBB9_57:                               # %if.end220
	callq	build_pointer_type
	movzbl	16(%r13), %ecx
	movq	%rax, %r14
	movq	$121, %rdi
	cmpq	$34, %rcx
	jne	.LBB9_60
# BB#58:                                # %if.then228
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	build1
	movq	%rax, %r14
	movq	%r13, %rdi
	callq	mark_addressable
	testq	%r15, %rax
	je	.LBB9_59
# BB#61:                                # %if.end234
	movq	%r13, %rdi
	callq	staticp
	movl	16(%r14), %ecx
	andq	$1, %rax
	shlq	$9, %rax
	movabsq	$4294966527, %rdx       # imm = 0xFFFFFCFF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 16(%r14)
	movq	%r14, %r13
	jmp	.LBB9_62
.LBB9_60:                               # %if.end242
	movq	$1, %rdx
	movq	%r13, %rsi
	callq	build_unary_op
	movq	%r14, %rdi
	movq	%rax, %rsi
.LBB9_28:                               # %if.then186
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	convert  # TAILCALL
.LBB9_59:                               # %if.then233
	movq	global_trees(%rip), %r13
.LBB9_62:                               # %return
	movq	%r13, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB9_51:
	movabsq	$0, %rax
	jmp	.LBB9_55
.Ltmp85:
	.size	default_function_array_conversion, .Ltmp85-default_function_array_conversion
	.cfi_endproc

	.globl	build_component_ref
	.align	16, 0x90
	.type	build_component_ref,@function
build_component_ref:                    # @build_component_ref
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp91:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp92:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp93:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp94:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp95:
	.cfi_def_cfa_offset 48
.Ltmp96:
	.cfi_offset %rbx, -40
.Ltmp97:
	.cfi_offset %r12, -32
.Ltmp98:
	.cfi_offset %r14, -24
.Ltmp99:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movzbl	16(%r14), %eax
	movq	%rsi, %r12
	cmpq	$47, %rax
	jne	.LBB10_1
# BB#23:                                # %sw.bb
	movq	40(%r14), %rdi
	movq	%r12, %rsi
	callq	build_component_ref
	movq	8(%rax), %r15
	movq	32(%r14), %rbx
	movq	%rax, %rdi
	callq	pedantic_non_lvalue
	movq	%rax, %rcx
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rbx, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	build  # TAILCALL
.LBB10_1:                               # %sw.epilog
	movq	8(%r14), %rax
	movl	16(%rax), %ecx
	movq	%rcx, %rbx
	andq	$255, %rbx
	andq	$254, %rcx
	cmpq	$20, %rcx
	jne	.LBB10_19
# BB#2:                                 # %if.then
	cmpq	$0, 32(%rax)
	je	.LBB10_3
# BB#4:                                 # %if.end
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	lookup_field
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB10_5
	.align	16, 0x90
.LBB10_9:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r15), %r12
	movq	8(%r12), %rbx
	movq	global_trees(%rip), %rax
	cmpq	%rax, %rbx
	je	.LBB10_22
# BB#10:                                # %if.end29
                                        #   in Loop: Header=BB10_9 Depth=1
	movq	$39, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%r12, %rcx
	callq	build
	movl	16(%r14), %ecx
	movq	%rax, %rbx
	testq	$4096, %rcx             # imm = 0x1000
	jne	.LBB10_12
# BB#11:                                # %lor.lhs.false37
                                        #   in Loop: Header=BB10_9 Depth=1
	movzbl	17(%r12), %eax
	testq	$16, %rax
	je	.LBB10_13
.LBB10_12:                              # %if.then44
                                        #   in Loop: Header=BB10_9 Depth=1
	movl	16(%rbx), %eax
	orq	$4096, %rax             # imm = 0x1000
	movl	%eax, 16(%rbx)
	movl	16(%r14), %ecx
.LBB10_13:                              # %if.end49
                                        #   in Loop: Header=BB10_9 Depth=1
	testq	$2048, %rcx             # imm = 0x800
	jne	.LBB10_15
# BB#14:                                # %lor.lhs.false55
                                        #   in Loop: Header=BB10_9 Depth=1
	movl	16(%r12), %eax
	testq	$2048, %rax             # imm = 0x800
	je	.LBB10_16
.LBB10_15:                              # %if.then62
                                        #   in Loop: Header=BB10_9 Depth=1
	movl	16(%rbx), %eax
	orq	$2048, %rax             # imm = 0x800
	movl	%eax, 16(%rbx)
	movl	16(%r12), %eax
.LBB10_16:                              # %if.end68
                                        #   in Loop: Header=BB10_9 Depth=1
	testq	$8388608, %rax          # imm = 0x800000
	je	.LBB10_18
# BB#17:                                # %if.then74
                                        #   in Loop: Header=BB10_9 Depth=1
	movq	%r12, %rdi
	callq	warn_deprecated_use
.LBB10_18:                              # %if.end75
                                        #   in Loop: Header=BB10_9 Depth=1
	movq	(%r15), %r15
	testq	%r15, %r15
	movq	%rbx, %r14
	jne	.LBB10_9
	jmp	.LBB10_22
.LBB10_19:                              # %if.else
	testq	%rbx, %rbx
	je	.LBB10_21
# BB#20:                                # %if.then78
	movq	32(%r12), %rsi
	movabsq	$.L.str18, %rdi
	xorq	%rax, %rax
	callq	error
	jmp	.LBB10_21
.LBB10_3:                               # %if.then18
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	incomplete_type_error
	jmp	.LBB10_21
.LBB10_5:                               # %if.then20
	cmpq	$20, %rbx
	je	.LBB10_6
# BB#7:                                 # %if.then20
	movabsq	$.L.str2, %rsi
	jmp	.LBB10_8
.LBB10_6:
	movabsq	$.L.str17, %rsi
.LBB10_8:                               # %if.then20
	movq	32(%r12), %rdx
	movabsq	$.L.str16, %rdi
	xorq	%rax, %rax
	callq	error
.LBB10_21:                              # %if.end83
	movq	global_trees(%rip), %rbx
.LBB10_22:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp100:
	.size	build_component_ref, .Ltmp100-build_component_ref
	.cfi_endproc

	.align	16, 0x90
	.type	lookup_field,@function
lookup_field:                           # @lookup_field
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
	movq	160(%rdi), %r12
	movq	24(%rdi), %r15
	movq	%rsi, %r14
	testq	%r12, %r12
	je	.LBB11_1
# BB#8:                                 # %if.then
	movl	(%r12), %ecx
	xorq	%rax, %rax
	movq	%r15, %r13
	jmp	.LBB11_9
	.align	16, 0x90
.LBB11_7:                               # %for.inc
                                        #   in Loop: Header=BB11_1 Depth=1
	movq	(%r15), %r15
.LBB11_1:                               # %for.cond.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%r15, %r15
	je	.LBB11_29
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB11_1 Depth=1
	movq	72(%r15), %rax
	testq	%rax, %rax
	jne	.LBB11_6
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB11_1 Depth=1
	movq	8(%r15), %rdi
	movl	16(%rdi), %ecx
	andq	$254, %rcx
	xorq	%rax, %rax
	cmpq	$20, %rcx
	jne	.LBB11_6
# BB#4:                                 # %if.then93
                                        #   in Loop: Header=BB11_1 Depth=1
	movq	%r14, %rsi
	callq	lookup_field
	testq	%rax, %rax
	jne	.LBB11_30
# BB#5:                                 # %if.then93.if.end100_crit_edge
                                        #   in Loop: Header=BB11_1 Depth=1
	movq	72(%r15), %rax
.LBB11_6:                               # %if.end100
                                        #   in Loop: Header=BB11_1 Depth=1
	cmpq	%r14, %rax
	jne	.LBB11_7
# BB#31:                                # %for.end
	testq	%r15, %r15
	jne	.LBB11_32
	jmp	.LBB11_29
.LBB11_26:                              # %if.end47
                                        #   in Loop: Header=BB11_9 Depth=1
	movq	%rdx, %rax
	.align	16, 0x90
.LBB11_9:                               # %while.cond.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB11_10 Depth 2
                                        #       Child Loop BB11_13 Depth 3
	movq	%rcx, %r15
.LBB11_10:                              # %while.cond
                                        #   Parent Loop BB11_9 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB11_13 Depth 3
	movq	%r15, %rdx
	subq	%rax, %rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$2, %rsi
	jl	.LBB11_27
# BB#11:                                # %while.body
                                        #   in Loop: Header=BB11_10 Depth=2
	incq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$33, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	8(%r12,%rsi,8), %r13
	movq	72(%r13), %rcx
	testq	%rcx, %rcx
	jne	.LBB11_21
# BB#12:                                # %while.cond14.preheader
                                        #   in Loop: Header=BB11_10 Depth=2
	movq	$32, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	8(%r12,%rbx,8), %rcx
	cmpq	$0, 72(%rcx)
	jne	.LBB11_17
	.align	16, 0x90
.LBB11_13:                              # %while.body20
                                        #   Parent Loop BB11_9 Depth=1
                                        #     Parent Loop BB11_10 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rcx, %r13
	movq	8(%r13), %rdi
	movl	16(%rdi), %eax
	andq	$254, %rax
	cmpq	$20, %rax
	jne	.LBB11_15
# BB#14:                                # %if.then34
                                        #   in Loop: Header=BB11_13 Depth=3
	movq	%r14, %rsi
	callq	lookup_field
	testq	%rax, %rax
	jne	.LBB11_18
.LBB11_15:                              # %while.cond14.backedge
                                        #   in Loop: Header=BB11_13 Depth=3
	movq	16(%r12,%rbx,8), %rcx
	incq	%rbx
	cmpq	$0, 72(%rcx)
	je	.LBB11_13
# BB#16:                                # %while.cond14.while.end_crit_edge
                                        #   in Loop: Header=BB11_10 Depth=2
	movq	%rbx, %rax
.LBB11_17:                              # %while.end
                                        #   in Loop: Header=BB11_10 Depth=2
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
	cmpq	%rdx, %rsi
	jle	.LBB11_10
	jmp	.LBB11_29
.LBB11_21:                              # %if.end42
                                        #   in Loop: Header=BB11_9 Depth=1
	cmpq	%r14, %rcx
	je	.LBB11_27
# BB#22:                                # %if.end47
                                        #   in Loop: Header=BB11_9 Depth=1
	jb	.LBB11_24
# BB#23:                                # %if.end47
                                        #   in Loop: Header=BB11_9 Depth=1
	movq	%rdx, %r15
.LBB11_24:                              # %if.end47
                                        #   in Loop: Header=BB11_9 Depth=1
	movq	%r15, %rcx
	jb	.LBB11_26
# BB#25:                                # %if.end47
                                        #   in Loop: Header=BB11_9 Depth=1
	movq	%rax, %rdx
	jmp	.LBB11_26
.LBB11_18:                              # %if.then36
	xorq	%rdi, %rdi
	movq	%r13, %rsi
.LBB11_19:                              # %if.then36
	movq	%rax, %rdx
.LBB11_20:                              # %if.then36
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	tree_cons  # TAILCALL
.LBB11_27:                              # %while.end55
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	8(%r12,%rax,8), %r15
	movq	72(%r15), %rax
	cmpq	%r14, %rax
	je	.LBB11_32
# BB#28:                                # %if.else64
	movq	72(%r13), %rax
	cmpq	%r14, %rax
	movq	%r13, %r15
	je	.LBB11_32
.LBB11_29:                              # %return
	xorq	%rax, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB11_32:                              # %if.end110
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	jmp	.LBB11_20
.LBB11_30:                              # %if.then97
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	jmp	.LBB11_19
.Ltmp117:
	.size	lookup_field, .Ltmp117-lookup_field
	.cfi_endproc

	.globl	build_indirect_ref
	.align	16, 0x90
	.type	build_indirect_ref,@function
build_indirect_ref:                     # @build_indirect_ref
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp122:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp123:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp124:
	.cfi_def_cfa_offset 32
.Ltmp125:
	.cfi_offset %rbx, -32
.Ltmp126:
	.cfi_offset %r14, -24
.Ltmp127:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	callq	default_conversion
	movq	%rax, %rbx
	movq	8(%rbx), %rax
	movzbl	16(%rax), %edx
	movzbl	16(%rbx), %ecx
	cmpq	$13, %rdx
	jne	.LBB12_18
# BB#1:                                 # %if.then
	cmpq	$121, %rcx
	jne	.LBB12_3
# BB#2:                                 # %if.then
	movl	flag_volatile(%rip), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	jne	.LBB12_3
# BB#4:                                 # %land.lhs.true8
	movq	32(%rbx), %r14
	movq	8(%r14), %rcx
	movq	8(%rax), %r15
	cmpq	%r15, %rcx
	jne	.LBB12_5
	jmp	.LBB12_21
.LBB12_18:                              # %if.else83
	testq	%rcx, %rcx
	je	.LBB12_20
# BB#19:                                # %if.then89
	movabsq	$.L.str21, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	error
.LBB12_20:                              # %if.end91
	movq	global_trees(%rip), %r14
	jmp	.LBB12_21
.LBB12_3:                               # %if.then.if.else_crit_edge
	movq	8(%rax), %r15
.LBB12_5:                               # %if.else
	movq	128(%r15), %rsi
	movq	$41, %rdi
	movq	%rbx, %rdx
	callq	build1
	cmpq	$0, 32(%r15)
	movl	16(%r15), %ecx
	movq	%rax, %r14
	jne	.LBB12_9
# BB#6:                                 # %lor.lhs.false
	movq	%rcx, %rax
	andq	$255, %rax
	cmpq	$5, %rax
	je	.LBB12_9
# BB#7:                                 # %lor.lhs.false
	cmpq	$18, %rax
	jne	.LBB12_8
.LBB12_9:                               # %if.end
	movq	%rcx, %rax
	andq	$255, %rax
	cmpq	$5, %rax
	jne	.LBB12_12
# BB#10:                                # %if.end
	movl	skip_evaluation(%rip), %eax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	jne	.LBB12_12
# BB#11:                                # %if.then43
	movabsq	$.L.str20, %rdi
	xorq	%rax, %rax
	callq	warning
	movl	16(%r15), %ecx
.LBB12_12:                              # %if.end44
	movl	16(%r14), %eax
	andq	$4096, %rcx             # imm = 0x1000
	andq	$-4097, %rax            # imm = 0xFFFFFFFFFFFFEFFF
	orq	%rcx, %rax
	movl	%eax, 16(%r14)
	movzbl	17(%r15), %ecx
	movq	$1, %rdx
	testq	$8, %rcx
	jne	.LBB12_17
# BB#13:                                # %lor.lhs.false57
	movzbl	17(%rbx), %ecx
	testq	$1, %rcx
	jne	.LBB12_17
# BB#14:                                # %lor.rhs
	movl	flag_volatile(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB12_15
# BB#16:                                # %lor.rhs
	movabsq	$1, %rdx
	jmp	.LBB12_17
.LBB12_8:                               # %if.then35
	movabsq	$.L.str19, %rdi
	xorq	%rax, %rax
	callq	error
	jmp	.LBB12_20
.LBB12_15:
	movabsq	$0, %rdx
.LBB12_17:                              # %lor.end
	movq	$8, %rcx
	shlq	%cl, %rdx
	andq	$-257, %rax             # imm = 0xFFFFFFFFFFFFFEFF
	orq	%rdx, %rax
	movl	%eax, 16(%r14)
	movl	16(%r15), %ecx
	andq	$2048, %rcx             # imm = 0x800
	movabsq	$4294965247, %rdx       # imm = 0xFFFFF7FF
	andq	%rax, %rdx
	orq	%rcx, %rdx
	movl	%edx, 16(%r14)
.LBB12_21:                              # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp128:
	.size	build_indirect_ref, .Ltmp128-build_indirect_ref
	.cfi_endproc

	.globl	build_array_ref
	.align	16, 0x90
	.type	build_array_ref,@function
build_array_ref:                        # @build_array_ref
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp133:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp134:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp135:
	.cfi_def_cfa_offset 32
.Ltmp136:
	.cfi_offset %rbx, -32
.Ltmp137:
	.cfi_offset %r14, -24
.Ltmp138:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r15
	testq	%rbx, %rbx
	je	.LBB13_1
# BB#3:                                 # %if.end
	movq	8(%r15), %rcx
	movq	global_trees(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB13_69
# BB#4:                                 # %lor.lhs.false
	movq	8(%rbx), %rax
	cmpq	%rdx, %rax
	je	.LBB13_69
# BB#5:                                 # %if.end6
	movzbl	16(%rcx), %ecx
	cmpq	$18, %rcx
	jne	.LBB13_53
# BB#6:                                 # %land.lhs.true
	movzbl	16(%r15), %ecx
	cmpq	$41, %rcx
	jne	.LBB13_7
.LBB13_53:                              # %if.end192
	movq	%r15, %rdi
	callq	default_conversion
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	default_conversion
	movl	warn_char_subscripts(%rip), %ecx
	movq	%rax, %r15
	testq	%rcx, %rcx
	je	.LBB13_57
# BB#54:                                # %land.lhs.true196
	movq	8(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$6, %rcx
	jne	.LBB13_57
# BB#55:                                # %land.lhs.true204
	movq	128(%rax), %rax
	movq	integer_types(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB13_57
# BB#56:                                # %if.then210
	movabsq	$.L.str27, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB13_57:                              # %if.end211
	movq	8(%r14), %rax
	movzbl	16(%rax), %eax
	cmpq	$6, %rax
	movq	%r15, %rdx
	je	.LBB13_59
# BB#58:                                # %if.end211
	movq	%r14, %rdx
.LBB13_59:                              # %if.end211
	je	.LBB13_61
# BB#60:                                # %if.end211
	movq	%r15, %r14
.LBB13_61:                              # %if.end211
	movq	global_trees(%rip), %rax
	cmpq	%rax, %rdx
	je	.LBB13_69
# BB#62:                                # %if.end223
	movq	8(%rdx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB13_64
# BB#63:                                # %lor.lhs.false231
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$23, %rax
	jne	.LBB13_65
.LBB13_64:                              # %if.then241
	movabsq	$.L.str28, %rdi
	jmp	.LBB13_2
.LBB13_1:                               # %if.then
	movabsq	$.L.str22, %rdi
.LBB13_2:                               # %return
	xorq	%rax, %rax
	callq	error
.LBB13_68:                              # %if.then61
	movq	global_trees(%rip), %rdx
.LBB13_69:                              # %return
	movq	%rdx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB13_7:                               # %if.then16
	movl	warn_char_subscripts(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB13_10
# BB#8:                                 # %land.lhs.true18
	movq	128(%rax), %rax
	movq	integer_types(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB13_10
# BB#9:                                 # %if.then23
	movabsq	$.L.str23, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB13_10:                              # %if.end24
	movq	%rbx, %rdi
	callq	default_conversion
	movq	%rax, %r14
	movq	8(%r14), %rax
	movzbl	16(%rax), %eax
	cmpq	$6, %rax
	jne	.LBB13_11
# BB#12:                                # %if.end33
	movl	16(%r14), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$25, %rcx
	jne	.LBB13_15
# BB#13:                                # %lor.lhs.false39
	movq	8(%r15), %rcx
	movq	8(%rcx), %rcx
	movq	32(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB13_17
# BB#14:                                # %land.lhs.true46
	movzbl	16(%rcx), %ecx
	cmpq	$25, %rcx
	je	.LBB13_17
.LBB13_15:                              # %if.then58
	movq	%r15, %rdi
	callq	mark_addressable
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_68
# BB#16:                                # %if.then58.if.end63_crit_edge
	movl	16(%r14), %eax
.LBB13_17:                              # %if.end63
	andq	$255, %rax
	cmpq	$25, %rax
	jne	.LBB13_21
# BB#18:                                # %land.lhs.true69
	movq	8(%r15), %rax
	movq	24(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB13_21
# BB#19:                                # %land.lhs.true74
	movq	%r14, %rdi
	callq	int_fits_type_p
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	jne	.LBB13_21
# BB#20:                                # %if.then81
	movq	%r15, %rdi
	callq	mark_addressable
	testq	%rbx, %rax
	je	.LBB13_68
.LBB13_21:                              # %if.end86
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB13_49
# BB#22:
	movq	%r15, %rax
	jmp	.LBB13_23
	.align	16, 0x90
.LBB13_25:                              # %while.body
                                        #   in Loop: Header=BB13_23 Depth=1
	movq	32(%rax), %rax
.LBB13_23:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rax), %ecx
	movq	%rcx, %rdx
	andq	$255, %rdx
	cmpq	$34, %rdx
	je	.LBB13_26
# BB#24:                                # %while.cond
                                        #   in Loop: Header=BB13_23 Depth=1
	cmpq	$39, %rdx
	je	.LBB13_25
	jmp	.LBB13_28
.LBB13_65:                              # %if.end242
	movq	8(%r14), %rax
	movzbl	16(%rax), %eax
	cmpq	$6, %rax
	jne	.LBB13_11
# BB#66:                                # %if.end251
	movq	$59, %rdi
	xorq	%rcx, %rcx
	movq	%rdx, %rsi
	movq	%r14, %rdx
	callq	build_binary_op
	movabsq	$.L.str29, %rsi
	movq	%rax, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	build_indirect_ref  # TAILCALL
.LBB13_11:                              # %if.then32
	movabsq	$.L.str24, %rdi
	jmp	.LBB13_2
.LBB13_26:                              # %land.lhs.true99
	movzbl	49(%rax), %edx
	testq	$4, %rdx
	je	.LBB13_28
# BB#27:                                # %if.then103
	movabsq	$.L.str25, %rdi
	jmp	.LBB13_48
.LBB13_28:                              # %if.else
	movl	flag_isoc99(%rip), %edx
	testq	%rdx, %rdx
	jne	.LBB13_49
# BB#29:
	movabsq	$11115375362048, %rdx   # imm = 0xA1C00000000
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	jmp	.LBB13_30
.LBB13_41:                              # %sw.bb.i
                                        #   in Loop: Header=BB13_30 Depth=1
	movq	32(%rax), %rax
	movl	16(%rax), %ecx
.LBB13_30:                              # %tailrecurse.i
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rcx
	cmpq	$169, %rcx
	jg	.LBB13_44
# BB#31:                                # %tailrecurse.i
                                        #   in Loop: Header=BB13_30 Depth=1
	cmpq	$28, %rcx
	jle	.LBB13_32
# BB#35:                                # %tailrecurse.i
                                        #   in Loop: Header=BB13_30 Depth=1
	cmpq	$119, %rcx
	jg	.LBB13_42
# BB#36:                                # %tailrecurse.i
                                        #   in Loop: Header=BB13_30 Depth=1
	cmpq	$51, %rcx
	jg	.LBB13_45
# BB#37:                                # %tailrecurse.i
                                        #   in Loop: Header=BB13_30 Depth=1
	cmpq	$43, %rcx
	ja	.LBB13_47
# BB#38:                                # %tailrecurse.i
                                        #   in Loop: Header=BB13_30 Depth=1
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%rdx, %rdi
	jne	.LBB13_33
# BB#39:                                # %tailrecurse.i
                                        #   in Loop: Header=BB13_30 Depth=1
	cmpq	$29, %rcx
	je	.LBB13_49
# BB#40:                                # %tailrecurse.i
                                        #   in Loop: Header=BB13_30 Depth=1
	cmpq	$39, %rcx
	je	.LBB13_41
	jmp	.LBB13_47
.LBB13_42:                              # %tailrecurse.i
                                        #   in Loop: Header=BB13_30 Depth=1
	leaq	-127(%rcx), %rdi
	andq	%rsi, %rdi
	cmpq	$2, %rdi
	jb	.LBB13_41
# BB#43:                                # %tailrecurse.i
	cmpq	$120, %rcx
	je	.LBB13_46
	jmp	.LBB13_47
.LBB13_44:                              # %tailrecurse.i
	cmpq	$170, %rcx
	jne	.LBB13_47
	jmp	.LBB13_49
.LBB13_32:                              # %tailrecurse.i
	testq	%rcx, %rcx
	jne	.LBB13_47
.LBB13_33:                              # %sw.bb3.i
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$16, %rax
	je	.LBB13_47
# BB#34:                                # %sw.bb3.i
	cmpq	$23, %rax
	je	.LBB13_47
	jmp	.LBB13_49
.LBB13_45:                              # %tailrecurse.i
	cmpq	$52, %rcx
	jne	.LBB13_47
.LBB13_46:                              # %lvalue_p.exit
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$18, %rax
	je	.LBB13_49
.LBB13_47:                              # %if.then108
	movabsq	$.L.str26, %rdi
.LBB13_48:                              # %if.end111
	xorq	%rax, %rax
	callq	pedwarn
.LBB13_49:                              # %if.end111
	movq	8(%r15), %rax
	movq	8(%rax), %rax
	movq	128(%rax), %rsi
	movq	$43, %rdi
	xorq	%rax, %rax
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	build
	movq	8(%r15), %rcx
	movq	8(%rcx), %rcx
	movl	16(%rcx), %ecx
	movl	16(%r15), %edx
	movl	16(%rax), %esi
	orq	%rcx, %rdx
	andq	$4096, %rdx             # imm = 0x1000
	orq	%rsi, %rdx
	movl	%edx, 16(%rax)
	movq	8(%r15), %rcx
	movq	8(%rcx), %rcx
	movl	16(%rcx), %edi
	movl	16(%r15), %ebx
	movq	$3, %rcx
	shrq	%cl, %rdi
	orq	%rsi, %rbx
	orq	%rdi, %rbx
	andq	$256, %rbx              # imm = 0x100
	andq	$-257, %rdx             # imm = 0xFFFFFFFFFFFFFEFF
	orq	%rbx, %rdx
	movl	%edx, 16(%rax)
	movq	8(%r15), %rcx
	movq	8(%rcx), %rcx
	movl	16(%rcx), %ecx
	movl	16(%r15), %edi
	orq	%rsi, %rcx
	orq	%rdi, %rcx
	andq	$2048, %rcx             # imm = 0x800
	movabsq	$4294965247, %rsi       # imm = 0xFFFFF7FF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, 16(%rax)
	movq	%rax, %rdi
	callq	fold
	movq	global_trees(%rip), %rdx
	cmpq	%rax, %rdx
	je	.LBB13_69
# BB#50:                                # %if.end111
	movq	8(%rax), %rsi
	cmpq	%rdx, %rsi
	je	.LBB13_69
# BB#51:                                # %if.end.i
	cmpq	$0, 32(%rsi)
	je	.LBB13_67
# BB#52:
	movq	%rax, %rdx
	jmp	.LBB13_69
.LBB13_67:                              # %if.end6.i
	movq	%rax, %rdi
	callq	incomplete_type_error
	jmp	.LBB13_68
.Ltmp139:
	.size	build_array_ref, .Ltmp139-build_array_ref
	.cfi_endproc

	.globl	mark_addressable
	.align	16, 0x90
	.type	mark_addressable,@function
mark_addressable:                       # @mark_addressable
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp143:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp144:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp145:
	.cfi_def_cfa_offset 32
.Ltmp146:
	.cfi_offset %rbx, -24
.Ltmp147:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	$1, %r14
	jmp	.LBB14_1
	.align	16, 0x90
.LBB14_7:                               # %sw.bb8
                                        #   in Loop: Header=BB14_1 Depth=1
	movq	32(%rbx), %rbx
.LBB14_1:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$169, %rcx
	jg	.LBB14_8
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB14_1 Depth=1
	cmpq	$120, %rcx
	jg	.LBB14_5
# BB#3:                                 # %while.body
                                        #   in Loop: Header=BB14_1 Depth=1
	addq	$-30, %rcx
	andq	%rdx, %rcx
	cmpq	$16, %rcx
	ja	.LBB14_27
# BB#4:                                 # %while.body
                                        #   in Loop: Header=BB14_1 Depth=1
	jmpq	*.LJTI14_0(,%rcx,8)
.LBB14_9:                               # %sw.bb
                                        #   in Loop: Header=BB14_1 Depth=1
	movq	40(%rbx), %rax
	movzbl	53(%rax), %ecx
	testq	$2, %rcx
	je	.LBB14_7
	jmp	.LBB14_10
	.align	16, 0x90
.LBB14_5:                               # %while.body
                                        #   in Loop: Header=BB14_1 Depth=1
	addq	$-121, %rcx
	andq	%rdx, %rcx
	cmpq	$7, %rcx
	ja	.LBB14_27
# BB#6:                                 # %while.body
                                        #   in Loop: Header=BB14_1 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$193, %rax
	jne	.LBB14_7
	jmp	.LBB14_27
.LBB14_8:                               # %while.body
	cmpq	$170, %rcx
	je	.LBB14_26
	jmp	.LBB14_27
.LBB14_11:                              # %sw.bb16
	movq	48(%rbx), %rcx
	testq	$1024, %rcx             # imm = 0x400
	je	.LBB14_25
# BB#12:                                # %land.lhs.true
	testq	$1024, %rax             # imm = 0x400
	jne	.LBB14_18
# BB#13:                                # %land.lhs.true
	andq	$512, %rcx              # imm = 0x200
	je	.LBB14_18
# BB#14:                                # %if.then35
	movq	72(%rbx), %rcx
	movq	32(%rcx), %rsi
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB14_17
# BB#15:                                # %if.then41
	movabsq	$.L.str86, %rdi
	jmp	.LBB14_16
.LBB14_18:                              # %land.lhs.true60
	testq	$1024, %rax             # imm = 0x400
	jne	.LBB14_25
# BB#19:                                # %if.then67
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB14_21
# BB#20:                                # %if.then74
	movq	72(%rbx), %rax
	movq	32(%rax), %rsi
	movabsq	$.L.str88, %rdi
	jmp	.LBB14_16
.LBB14_10:                              # %if.then
	movq	72(%rax), %rax
	movq	32(%rax), %rsi
	movabsq	$.L.str85, %rdi
.LBB14_16:                              # %return
	xorq	%rax, %rax
	xorq	%r14, %r14
	callq	error
	jmp	.LBB14_27
.LBB14_17:                              # %if.end47
	movabsq	$.L.str87, %rdi
.LBB14_24:                              # %if.end96
	xorq	%rax, %rax
	callq	pedwarn
.LBB14_25:                              # %if.end96
	movq	%rbx, %rdi
	callq	put_var_into_stack
	movl	16(%rbx), %eax
.LBB14_26:                              # %sw.bb97
	orq	$1024, %rax             # imm = 0x400
	movl	%eax, 16(%rbx)
.LBB14_27:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB14_21:                              # %if.else80
	movq	8(%rbx), %rax
	movzbl	19(%rax), %eax
	testq	$4, %rax
	je	.LBB14_23
# BB#22:                                # %if.then87
	movabsq	$.L.str89, %rdi
	xorq	%rax, %rax
	xorq	%r14, %r14
	callq	error
	jmp	.LBB14_27
.LBB14_23:                              # %if.end89
	movq	72(%rbx), %rax
	movq	32(%rax), %rsi
	movabsq	$.L.str90, %rdi
	jmp	.LBB14_24
.Ltmp148:
	.size	mark_addressable, .Ltmp148-mark_addressable
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI14_0:
	.quad	.LBB14_26
	.quad	.LBB14_27
	.quad	.LBB14_11
	.quad	.LBB14_27
	.quad	.LBB14_11
	.quad	.LBB14_11
	.quad	.LBB14_11
	.quad	.LBB14_27
	.quad	.LBB14_27
	.quad	.LBB14_9
	.quad	.LBB14_27
	.quad	.LBB14_27
	.quad	.LBB14_27
	.quad	.LBB14_7
	.quad	.LBB14_27
	.quad	.LBB14_27
	.quad	.LBB14_26

	.text
	.globl	lvalue_p
	.align	16, 0x90
	.type	lvalue_p,@function
lvalue_p:                               # @lvalue_p
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$11115375362048, %r8    # imm = 0xA1C00000000
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	xorq	%rax, %rax
	jmp	.LBB15_1
.LBB15_15:                              # %sw.bb
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	32(%rdi), %rdi
	.align	16, 0x90
.LBB15_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rdi), %ecx
	cmpq	$169, %rcx
	jg	.LBB15_22
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB15_1 Depth=1
	cmpq	$28, %rcx
	jle	.LBB15_3
# BB#9:                                 # %tailrecurse
                                        #   in Loop: Header=BB15_1 Depth=1
	cmpq	$119, %rcx
	jg	.LBB15_16
# BB#10:                                # %tailrecurse
                                        #   in Loop: Header=BB15_1 Depth=1
	cmpq	$51, %rcx
	jg	.LBB15_18
# BB#11:                                # %tailrecurse
                                        #   in Loop: Header=BB15_1 Depth=1
	cmpq	$43, %rcx
	ja	.LBB15_24
# BB#12:                                # %tailrecurse
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%r8, %rdx
	jne	.LBB15_4
# BB#13:                                # %tailrecurse
                                        #   in Loop: Header=BB15_1 Depth=1
	cmpq	$29, %rcx
	je	.LBB15_23
# BB#14:                                # %tailrecurse
                                        #   in Loop: Header=BB15_1 Depth=1
	cmpq	$39, %rcx
	je	.LBB15_15
	jmp	.LBB15_24
.LBB15_16:                              # %tailrecurse
                                        #   in Loop: Header=BB15_1 Depth=1
	leaq	-127(%rcx), %rdx
	andq	%rsi, %rdx
	cmpq	$2, %rdx
	jb	.LBB15_15
# BB#17:                                # %tailrecurse
	cmpq	$120, %rcx
	je	.LBB15_19
	jmp	.LBB15_24
.LBB15_22:                              # %tailrecurse
	cmpq	$170, %rcx
	jne	.LBB15_24
.LBB15_23:                              # %return.loopexit
	movq	$1, %rax
	jmp	.LBB15_24
.LBB15_3:                               # %tailrecurse
	testq	%rcx, %rcx
	jne	.LBB15_24
.LBB15_4:                               # %sw.bb3
	movq	8(%rdi), %rax
	movzbl	16(%rax), %esi
	cmpq	$16, %rsi
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, %rax
	je	.LBB15_6
# BB#5:                                 # %sw.bb3
	movq	%rdx, %rax
.LBB15_6:                               # %sw.bb3
	cmpq	$23, %rsi
	je	.LBB15_8
# BB#7:                                 # %sw.bb3
	movq	%rdx, %rcx
.LBB15_8:                               # %sw.bb3
	andq	%rcx, %rax
	retq
.LBB15_18:                              # %tailrecurse
	cmpq	$52, %rcx
	jne	.LBB15_24
.LBB15_19:                              # %sw.bb16
	movq	8(%rdi), %rax
	movzbl	16(%rax), %eax
	cmpq	$18, %rax
	jne	.LBB15_20
# BB#21:                                # %sw.bb16
	movabsq	$1, %rax
	retq
.LBB15_24:                              # %return
	retq
.LBB15_20:
	movabsq	$0, %rax
	retq
.Ltmp149:
	.size	lvalue_p, .Ltmp149-lvalue_p
	.cfi_endproc

	.globl	build_binary_op
	.align	16, 0x90
	.type	build_binary_op,@function
build_binary_op:                        # @build_binary_op
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp156:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp157:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp158:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp159:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp160:
	.cfi_def_cfa_offset 48
	subq	$160, %rsp
.Ltmp161:
	.cfi_def_cfa_offset 208
.Ltmp162:
	.cfi_offset %rbx, -48
.Ltmp163:
	.cfi_offset %r12, -40
.Ltmp164:
	.cfi_offset %r13, -32
.Ltmp165:
	.cfi_offset %r14, -24
.Ltmp166:
	.cfi_offset %r15, -16
	movq	%rdx, %r13
	movq	%rsi, %r15
	movq	%rdi, %r12
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rcx
	movq	%r13, %rcx
	movq	%r15, %rbx
	je	.LBB16_2
# BB#1:                                 # %if.then
	movq	%r15, %rdi
	movq	%r8, %r14
	callq	default_conversion
	movq	%rax, %rbx
	movq	%r13, %rdi
	callq	default_conversion
	movq	%r14, %r8
	movq	%rax, %rcx
.LBB16_2:                               # %if.end
	movq	%r13, 88(%rsp)          # 8-byte Spill
	movq	%r15, 96(%rsp)          # 8-byte Spill
	movq	8(%rbx), %r15
	movq	8(%rcx), %rdi
	movl	16(%r15), %r9d
	movl	16(%rdi), %r11d
	movq	global_trees(%rip), %rax
	andq	$255, %r9
	movq	%r11, %r10
	andq	$255, %r10
	.align	16, 0x90
.LBB16_3:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %r14
	movzbl	16(%r14), %edx
	addq	$-114, %rdx
	andq	%r8, %rdx
	cmpq	$2, %rdx
	ja	.LBB16_6
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	32(%r14), %rbx
	cmpq	%rax, %rbx
	je	.LBB16_6
# BB#5:                                 # %land.rhs
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	8(%rbx), %rdx
	cmpq	%rdx, %r15
	je	.LBB16_3
	.align	16, 0x90
.LBB16_6:                               # %while.cond38
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %r13
	movzbl	16(%r13), %edx
	leaq	-114(%rdx), %rcx
	andq	%r8, %rcx
	cmpq	$2, %rcx
	ja	.LBB16_9
# BB#7:                                 # %land.lhs.true56
                                        #   in Loop: Header=BB16_6 Depth=1
	movq	32(%r13), %rcx
	cmpq	%rax, %rcx
	je	.LBB16_9
# BB#8:                                 # %land.rhs61
                                        #   in Loop: Header=BB16_6 Depth=1
	movq	8(%rcx), %rsi
	cmpq	%rsi, %rdi
	je	.LBB16_6
.LBB16_9:                               # %while.end75
	testq	%r9, %r9
	je	.LBB16_434
# BB#10:                                # %while.end75
	testq	%r10, %r10
	je	.LBB16_434
# BB#11:                                # %if.end80
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	%r12, %rsi
	leaq	-59(%rsi), %rax
	andq	%r8, %rax
	cmpq	$50, %rax
	ja	.LBB16_12
# BB#13:                                # %if.end80
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movabsq	$0, %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	xorq	%rbx, %rbx
	xorq	%rdi, %rdi
	movq	%rsi, %rcx
	jmpq	*.LJTI16_0(,%rax,8)
.LBB16_290:                             # %sw.bb667
	cmpq	$7, %r9
	jne	.LBB16_292
# BB#291:                               # %sw.bb667
	cmpq	$7, %r10
	jne	.LBB16_292
# BB#293:                               # %if.end674
	xorq	%r12, %r12
	movq	$1, 48(%rsp)            # 8-byte Folded Spill
	movq	integer_types+40(%rip), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	jmp	.LBB16_294
.LBB16_62:                              # %sw.bb114
	movl	warn_div_by_zero(%rip), %eax
	testq	%rax, %rax
	je	.LBB16_66
# BB#63:                                # %sw.bb114
	movl	skip_evaluation(%rip), %eax
	testq	%r8, %rax
	jne	.LBB16_66
# BB#64:                                # %land.lhs.true118
	movq	%r13, %rdi
	movq	%rsi, %rbx
	movq	%r9, %r12
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movq	%r11, 16(%rsp)          # 8-byte Spill
	callq	integer_zerop
	movq	16(%rsp), %r11          # 8-byte Reload
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	%r12, %r9
	movq	%rbx, %rsi
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rax
	je	.LBB16_66
# BB#65:                                # %if.then121
	movabsq	$.L.str45, %rdi
	xorq	%rax, %rax
	movq	%r8, %rbx
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%r9, %r12
	callq	warning
	movq	16(%rsp), %r11          # 8-byte Reload
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	%r12, %r9
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %r8
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
.LBB16_66:                              # %if.end122
	movl	16(%r15), %eax
	andq	$254, %rax
	cmpq	$6, %rax
	je	.LBB16_68
# BB#67:                                # %if.end122
	cmpq	$8, %r9
	jne	.LBB16_221
.LBB16_68:                              # %land.lhs.true128
	movq	%r11, %rax
	andq	$254, %rax
	cmpq	$6, %rax
	je	.LBB16_70
# BB#69:                                # %land.lhs.true128
	cmpq	$8, %r10
	jne	.LBB16_221
.LBB16_70:                              # %if.then134
	xorq	%r12, %r12
	cmpq	$6, %r9
	movq	$70, %rcx
	jne	.LBB16_71
# BB#73:                                # %if.then134
	cmpq	$6, %r10
	jne	.LBB16_71
# BB#74:                                # %if.else139
	movq	%r8, %rbx
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movzbl	17(%rax), %eax
	movq	$1, 48(%rsp)            # 8-byte Folded Spill
	testq	$32, %rax
	movq	$1, 80(%rsp)            # 8-byte Folded Spill
	jne	.LBB16_80
# BB#75:                                # %lor.rhs
	movzbl	16(%r13), %eax
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	cmpq	$25, %rax
	jne	.LBB16_80
# BB#76:                                # %land.rhs151
	movq	%r11, 16(%rsp)          # 8-byte Spill
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%rsi, %r12
	movq	%r13, %rdi
	callq	integer_all_onesp
	testq	%rbx, %rax
	je	.LBB16_78
# BB#77:
	movabsq	$0, %rax
	jmp	.LBB16_79
.LBB16_98:                              # %sw.bb198
	cmpq	$13, %r9
	ja	.LBB16_221
# BB#99:                                # %sw.bb198
	movq	$1, %rax
	movq	%r9, %rcx
	shlq	%cl, %rax
	testq	$8640, %rax             # imm = 0x21C0
	je	.LBB16_221
# BB#100:                               # %land.lhs.true206
	cmpq	$13, %r10
	ja	.LBB16_221
# BB#101:                               # %land.lhs.true206
	movq	%r11, 16(%rsp)          # 8-byte Spill
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	$1, %rax
	movq	%r10, %rcx
	shlq	%cl, %rax
	testq	$8640, %rax             # imm = 0x21C0
	je	.LBB16_102
# BB#105:                               # %if.then214
	movq	integer_types+40(%rip), %rbx
	movq	%r14, %rdi
	movq	%r10, %r12
	callq	truthvalue_conversion
	movq	%rax, %r14
	movq	%r13, %rdi
	callq	truthvalue_conversion
	movq	%rbx, %rdi
	movq	$1, %rbx
	movq	%r12, %r10
	movq	%rax, %r13
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	jmp	.LBB16_103
.LBB16_83:                              # %sw.bb157
	cmpq	$6, %r9
	movabsq	$0, %rax
	movabsq	$1, %rcx
	movq	%rax, %rdx
	jne	.LBB16_85
# BB#84:                                # %sw.bb157
	movq	%rcx, %rdx
.LBB16_85:                              # %sw.bb157
	cmpq	$6, %r10
	jne	.LBB16_87
# BB#86:                                # %sw.bb157
	movq	%rcx, %rax
.LBB16_87:                              # %sw.bb157
	andq	%rax, %rdx
	movq	$63, %rcx
	shlq	%cl, %rdx
	movq	$63, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	jmp	.LBB16_218
.LBB16_238:                             # %sw.bb548
	movq	integer_types+40(%rip), %rcx
	movl	16(%r15), %eax
	andq	$254, %rax
	cmpq	$6, %rax
	jne	.LBB16_241
# BB#239:                               # %sw.bb548
	movq	%r11, %rax
	andq	$254, %rax
	cmpq	$6, %rax
	jne	.LBB16_241
# BB#240:
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	$1, 64(%rsp)            # 8-byte Folded Spill
	xorq	%r12, %r12
	jmp	.LBB16_294
.LBB16_292:                             # %if.then673
	movabsq	$.L.str59, %rdi
	xorq	%rax, %rax
	callq	error
	movq	global_trees(%rip), %rax
	jmp	.LBB16_434
.LBB16_88:                              # %sw.bb163
	movl	warn_div_by_zero(%rip), %eax
	testq	%rax, %rax
	je	.LBB16_92
# BB#89:                                # %sw.bb163
	movl	skip_evaluation(%rip), %eax
	testq	%r8, %rax
	jne	.LBB16_92
# BB#90:                                # %land.lhs.true167
	movq	%r13, %rdi
	movq	%rsi, %rbx
	movq	%r9, %r12
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movq	%r11, 16(%rsp)          # 8-byte Spill
	callq	integer_zerop
	movq	16(%rsp), %r11          # 8-byte Reload
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	%r12, %r9
	movq	%rbx, %rsi
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rax
	je	.LBB16_92
# BB#91:                                # %if.then170
	movabsq	$.L.str45, %rdi
	xorq	%rax, %rax
	movq	%r8, %rbx
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%r9, %r12
	callq	warning
	movq	16(%rsp), %r11          # 8-byte Reload
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	%r12, %r9
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %r8
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
.LBB16_92:                              # %if.end171
	cmpq	$6, %r9
	jne	.LBB16_221
# BB#93:                                # %if.end171
	cmpq	$6, %r10
	jne	.LBB16_221
# BB#94:                                # %if.then175
	movq	%r8, %rbx
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movzbl	17(%rax), %eax
	movq	$1, 48(%rsp)            # 8-byte Folded Spill
	testq	$32, %rax
	movq	$1, 80(%rsp)            # 8-byte Folded Spill
	jne	.LBB16_80
# BB#95:                                # %lor.rhs184
	movzbl	16(%r13), %eax
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	cmpq	$25, %rax
	jne	.LBB16_80
# BB#96:                                # %land.rhs190
	movq	%r11, 16(%rsp)          # 8-byte Spill
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%rsi, %r12
	movq	%r13, %rdi
	callq	integer_all_onesp
	testq	%rbx, %rax
	jne	.LBB16_97
.LBB16_78:                              # %land.rhs151
	movabsq	$1, %rax
	jmp	.LBB16_79
.LBB16_215:                             # %sw.bb508
	movl	16(%r15), %eax
	andq	$254, %rax
	cmpq	$6, %rax
	jne	.LBB16_220
# BB#216:                               # %sw.bb508
	movq	%r11, %rax
	andq	$254, %rax
	cmpq	$6, %rax
	jne	.LBB16_220
# BB#217:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	$1, 80(%rsp)            # 8-byte Folded Spill
.LBB16_218:                             # %sw.epilog
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	jmp	.LBB16_295
.LBB16_138:                             # %sw.bb308
	cmpq	$6, %r9
	jne	.LBB16_221
# BB#139:                               # %sw.bb308
	cmpq	$6, %r10
	jne	.LBB16_221
# BB#140:                               # %if.then314
	movq	%r10, 8(%rsp)           # 8-byte Spill
	cmpq	$25, %rdx
	jne	.LBB16_146
# BB#141:                               # %if.then314
	movl	skip_evaluation(%rip), %eax
	testq	%r8, %rax
	jne	.LBB16_146
# BB#142:                               # %if.then324
	movq	%r11, 16(%rsp)          # 8-byte Spill
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%rsi, %r12
	movq	%r8, %rbx
	movq	%r13, %rdi
	callq	tree_int_cst_sgn
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB16_143
# BB#144:                               # %if.else329
	movl	60(%r15), %esi
	andq	$511, %rsi              # imm = 0x1FF
	movq	%r13, %rdi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rbx, %r8
	movq	%r12, %rsi
	movabsq	$0, %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	(%rsp), %r9             # 8-byte Reload
	movq	16(%rsp), %r11          # 8-byte Reload
	js	.LBB16_146
# BB#145:                               # %if.then338
	movabsq	$.L.str51, %rdi
	xorq	%rax, %rax
	movq	%r8, %rbx
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%r9, %r12
	callq	warning
	movq	16(%rsp), %r11          # 8-byte Reload
	movq	%r12, %r9
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %r8
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB16_146
.LBB16_151:                             # %sw.bb355
	movl	warn_float_equal(%rip), %eax
	testq	%rax, %rax
	je	.LBB16_155
# BB#152:                               # %land.lhs.true357
	cmpq	$7, %r9
	je	.LBB16_154
# BB#153:                               # %land.lhs.true357
	cmpq	$7, %r10
	jne	.LBB16_155
.LBB16_154:                             # %if.then363
	movabsq	$.L.str52, %rdi
	xorq	%rax, %rax
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%r9, %r12
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movq	%r11, %rbx
	callq	warning
	movq	%rbx, %r11
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	%r12, %r9
	movq	32(%rsp), %rsi          # 8-byte Reload
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
.LBB16_155:                             # %if.end364
	movq	integer_types+40(%rip), %rcx
	movl	16(%r15), %eax
	andq	$254, %rax
	cmpq	$6, %rax
	je	.LBB16_157
# BB#156:                               # %if.end364
	cmpq	$8, %r9
	jne	.LBB16_161
.LBB16_157:                             # %land.lhs.true373
	movq	%r11, %rax
	andq	$254, %rax
	cmpq	$6, %rax
	movq	$1, 64(%rsp)            # 8-byte Folded Spill
	je	.LBB16_158
# BB#160:                               # %land.lhs.true373
	cmpq	$8, %r10
	je	.LBB16_158
.LBB16_161:                             # %if.else383
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	%r8, %r12
	cmpq	$13, %r9
	jne	.LBB16_193
# BB#162:                               # %if.else383
	cmpq	$13, %r10
	jne	.LBB16_193
# BB#163:                               # %if.then389
	movq	%r11, 16(%rsp)          # 8-byte Spill
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	8(%r15), %rbx
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	8(%rsi), %rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	$1, %rdx
	movq	%r15, %rdi
	callq	maybe_objc_comptypes
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jns	.LBB16_166
# BB#164:                               # %if.end.i1686
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movq	8(%r15), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	movq	128(%rax), %rdi
	movq	128(%rcx), %rsi
	callq	comptypes
	movq	%rax, %rcx
	movq	%r12, %rbx
	andq	%rbx, %rcx
	cmpq	$2, %rcx
	jne	.LBB16_165
# BB#170:                               # %if.end.i1686
	movl	pedantic(%rip), %ecx
	testq	%rbx, %rcx
	movq	%rbx, %r12
	movabsq	$0, %rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	64(%rsp), %rbx          # 8-byte Reload
	je	.LBB16_166
# BB#171:                               # %comp_target_types.exit1689.thread
	movabsq	$.L.str182, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	jmp	.LBB16_172
.LBB16_241:                             # %if.else561
	cmpq	$13, %r9
	jne	.LBB16_261
# BB#242:                               # %if.else561
	cmpq	$13, %r10
	jne	.LBB16_261
# BB#243:                               # %if.then567
	movq	%r11, 16(%rsp)          # 8-byte Spill
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%r8, %rbx
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	maybe_objc_comptypes
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jns	.LBB16_247
# BB#244:                               # %if.end.i1710
	movq	8(%r15), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	movq	128(%rax), %rdi
	movq	128(%rcx), %rsi
	callq	comptypes
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	$2, %rcx
	jne	.LBB16_247
# BB#245:                               # %if.end.i1710
	movl	pedantic(%rip), %ecx
	testq	%rbx, %rcx
	je	.LBB16_247
# BB#246:                               # %comp_target_types.exit1713.thread
	movabsq	$.L.str182, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	jmp	.LBB16_248
.LBB16_14:                              # %sw.bb
	cmpq	$13, %r9
	jne	.LBB16_18
# BB#15:                                # %sw.bb
	cmpq	$6, %r10
	jne	.LBB16_18
# BB#16:                                # %if.then84
	movq	$59, %rdi
	jmp	.LBB16_17
.LBB16_21:                              # %sw.bb95
	cmpq	$13, %r9
	jne	.LBB16_58
# BB#22:                                # %sw.bb95
	cmpq	$13, %r10
	jne	.LBB16_58
# BB#23:                                # %land.lhs.true99
	movq	%r11, 16(%rsp)          # 8-byte Spill
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%rsi, %r12
	movq	%r8, %rbx
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	maybe_objc_comptypes
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jns	.LBB16_27
# BB#24:                                # %if.end.i
	movq	8(%r15), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	movq	128(%rax), %rdi
	movq	128(%rcx), %rsi
	callq	comptypes
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	$2, %rcx
	jne	.LBB16_27
# BB#25:                                # %if.end.i
	movl	pedantic(%rip), %ecx
	testq	%rbx, %rcx
	je	.LBB16_27
# BB#26:                                # %comp_target_types.exit.thread
	movabsq	$.L.str182, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	movq	%rbx, %r8
	jmp	.LBB16_28
.LBB16_61:                              # %sw.bb113
	movq	$61, %rcx
	xorq	%r12, %r12
.LBB16_71:
	movq	$1, 48(%rsp)            # 8-byte Folded Spill
	jmp	.LBB16_72
.LBB16_124:                             # %sw.bb261
	cmpq	$6, %r9
	movq	$82, %rcx
	jne	.LBB16_125
# BB#126:                               # %sw.bb261
	cmpq	$6, %r10
	jne	.LBB16_125
# BB#127:                               # %if.then267
	movq	%r10, 8(%rsp)           # 8-byte Spill
	cmpq	$25, %rdx
	jne	.LBB16_133
# BB#128:                               # %if.then267
	movl	skip_evaluation(%rip), %eax
	testq	%r8, %rax
	jne	.LBB16_133
# BB#129:                               # %if.then277
	movq	%r11, 16(%rsp)          # 8-byte Spill
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%rsi, %r12
	movq	%r8, %rbx
	movq	%r13, %rdi
	callq	tree_int_cst_sgn
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB16_130
# BB#131:                               # %if.else282
	movl	60(%r15), %esi
	andq	$511, %rsi              # imm = 0x1FF
	movq	%r13, %rdi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rbx, %r8
	movq	%r12, %rsi
	movabsq	$0, %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	(%rsp), %r9             # 8-byte Reload
	movq	16(%rsp), %r11          # 8-byte Reload
	movq	$82, %rcx
	js	.LBB16_133
# BB#132:                               # %if.then291
	movabsq	$.L.str49, %rdi
	xorq	%rax, %rax
	movq	%r8, %rbx
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%r9, %r12
	callq	warning
	movq	$82, %rcx
	movq	16(%rsp), %r11          # 8-byte Reload
	movq	%r12, %r9
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %r8
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB16_133
.LBB16_106:                             # %sw.bb218
	cmpq	$6, %r9
	movq	$83, %rcx
	jne	.LBB16_125
# BB#107:                               # %sw.bb218
	cmpq	$6, %r10
	jne	.LBB16_125
# BB#108:                               # %if.then222
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	cmpq	$25, %rdx
	movabsq	$0, %r12
	jne	.LBB16_118
# BB#109:                               # %if.then222
	movl	skip_evaluation(%rip), %eax
	testq	%r8, %rax
	movabsq	$0, %r12
	jne	.LBB16_118
# BB#110:                               # %if.then230
	movq	%r11, 16(%rsp)          # 8-byte Spill
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%rsi, %r12
	movq	%r8, %rbx
	movq	%r13, %rdi
	callq	tree_int_cst_sgn
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB16_111
# BB#112:                               # %if.else234
	movq	%r13, %rdi
	callq	integer_zerop
	testq	%rbx, %rax
	jne	.LBB16_113
# BB#114:                               # %if.else234
	movabsq	$1, %rax
	jmp	.LBB16_115
.LBB16_220:                             # %if.else521
	cmpq	$13, %r9
	jne	.LBB16_221
# BB#222:                               # %if.else521
	movq	%r8, %rbx
	cmpq	$13, %r10
	jne	.LBB16_223
# BB#224:                               # %if.then527
	movq	%r11, 16(%rsp)          # 8-byte Spill
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	maybe_objc_comptypes
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jns	.LBB16_228
# BB#225:                               # %if.end.i1698
	movq	8(%r15), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	movq	128(%rax), %rdi
	movq	128(%rcx), %rsi
	callq	comptypes
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	$2, %rcx
	jne	.LBB16_228
# BB#226:                               # %if.end.i1698
	movl	pedantic(%rip), %ecx
	testq	%rbx, %rcx
	je	.LBB16_228
# BB#227:                               # %comp_target_types.exit1701.thread
	movabsq	$.L.str182, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	jmp	.LBB16_229
.LBB16_221:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
.LBB16_12:
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
.LBB16_295:                             # %sw.epilog
	xorq	%rbx, %rbx
	xorq	%rdi, %rdi
.LBB16_296:                             # %sw.epilog
	movq	%rsi, %rcx
.LBB16_297:                             # %sw.epilog
	movl	16(%r15), %eax
	andq	$254, %rax
	cmpq	$6, %rax
	je	.LBB16_299
# BB#298:                               # %sw.epilog
	cmpq	$8, %r9
	jne	.LBB16_418
.LBB16_299:                             # %land.lhs.true683
	andq	$254, %r11
	cmpq	$6, %r11
	je	.LBB16_301
# BB#300:                               # %land.lhs.true683
	cmpq	$8, %r10
	jne	.LBB16_418
.LBB16_301:                             # %if.then692
	movq	%r12, 24(%rsp)          # 8-byte Spill
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	cmpq	$8, %r10
	movabsq	$0, %r12
	movabsq	$1, %rax
	movq	%r12, %rbx
	je	.LBB16_303
# BB#302:                               # %if.then692
	movq	%rax, %rbx
.LBB16_303:                             # %if.then692
	cmpq	$8, %r9
	je	.LBB16_305
# BB#304:                               # %if.then692
	movq	%rax, %r12
.LBB16_305:                             # %if.then692
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	64(%rsp), %rcx          # 8-byte Reload
	orq	%rcx, %rax
	movq	80(%rsp), %rcx          # 8-byte Reload
	orq	%rcx, %rax
	testq	%r8, %rax
	je	.LBB16_307
# BB#306:                               # %if.then704
	movq	%r15, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%r8, %r15
	callq	common_type
	movq	%r15, %r8
	movq	%rax, %rdi
.LBB16_307:                             # %if.end706
	andq	%r12, %rbx
	movq	80(%rsp), %rax          # 8-byte Reload
	testq	%r8, %rax
	je	.LBB16_308
# BB#309:                               # %if.end706
	movabsq	$1, %rax
	jmp	.LBB16_310
.LBB16_308:
	movabsq	$0, %rax
.LBB16_310:                             # %if.end706
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	%rdi, %r15
	xorq	%rcx, %rcx
	testq	%rbx, %rax
	je	.LBB16_311
# BB#312:                               # %if.then710
	movq	%rdx, %r12
	leaq	156(%rsp), %rsi
	movq	%r14, %rdi
	callq	get_narrower
	movq	%rax, %rbx
	leaq	152(%rsp), %rsi
	movq	%r13, %rdi
	callq	get_narrower
	movq	%rbx, %r10
	movq	8(%r14), %rsi
	movq	8(%r10), %rdi
	movl	16(%r15), %edx
	movl	60(%rsi), %r8d
	movl	60(%rdi), %ebx
	movq	%rax, %r9
	movq	$13, %rcx
	shrq	%cl, %rdx
	xorq	%r8, %rbx
	testq	$511, %rbx              # imm = 0x1FF
	jne	.LBB16_315
# BB#313:                               # %if.then710
	cmpq	%r15, %rsi
	je	.LBB16_315
# BB#314:                               # %if.then738
	movl	16(%rsi), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, 156(%rsp)
.LBB16_315:                             # %if.end746
	movq	%r12, 32(%rsp)          # 8-byte Spill
	movq	8(%r13), %rax
	movq	8(%r9), %rsi
	movl	60(%rax), %ecx
	movl	60(%rsi), %ebx
	xorq	%rcx, %rbx
	testq	$511, %rbx              # imm = 0x1FF
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	jne	.LBB16_318
# BB#316:                               # %if.end746
	cmpq	%r15, %rax
	je	.LBB16_318
# BB#317:                               # %if.then766
	movl	16(%rax), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, 152(%rsp)
.LBB16_318:                             # %if.end774
	movl	156(%rsp), %ebx
	movq	80(%rsp), %rax          # 8-byte Reload
	andq	%r8, %rax
	cmpq	%r8, %rax
	movq	%rbx, %r12
	je	.LBB16_320
# BB#319:                               # %if.end774
	andq	$1, %rdx
	movq	%rdx, %r12
.LBB16_320:                             # %if.end774
	movl	60(%rdi), %ecx
	movl	60(%r15), %eax
	andq	$511, %rcx              # imm = 0x1FF
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rax, %rcx
	jae	.LBB16_326
# BB#321:                               # %land.lhs.true791
	movl	60(%rsi), %edx
	andq	$511, %rdx              # imm = 0x1FF
	andq	%r8, %rcx
	cmpq	%rcx, %rdx
	jne	.LBB16_326
# BB#322:                               # %land.lhs.true791
	movl	152(%rsp), %ecx
	andq	%r8, %rcx
	movq	%rbx, %rdx
	andq	%r8, %rdx
	cmpq	%rcx, %rdx
	jne	.LBB16_326
# BB#323:                               # %land.lhs.true809
	testq	%r8, %rbx
	jne	.LBB16_325
# BB#324:                               # %land.lhs.true809
	testq	%r8, %r12
	je	.LBB16_325
.LBB16_326:                             # %if.else820
	movzbl	16(%r10), %ecx
	cmpq	$25, %rcx
	jne	.LBB16_331
# BB#327:                               # %land.lhs.true827
	movl	152(%rsp), %edi
	testq	%rdi, %rdi
	jne	.LBB16_329
# BB#328:                               # %land.lhs.true827
	testq	%r8, %r12
	jne	.LBB16_331
.LBB16_329:                             # %land.lhs.true831
	movl	60(%rsi), %ecx
	andq	$511, %rcx              # imm = 0x1FF
	andq	%r8, %rax
	cmpq	%rax, %rcx
	jae	.LBB16_331
# BB#330:                               # %land.lhs.true844
	movq	%r9, 48(%rsp)           # 8-byte Spill
	movq	%r10, 80(%rsp)          # 8-byte Spill
	callq	signed_or_unsigned_type
	movq	%rax, %rbx
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	callq	int_fits_type_p
	movq	80(%rsp), %r10          # 8-byte Reload
	movq	48(%rsp), %r9           # 8-byte Reload
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rax
	jne	.LBB16_339
.LBB16_331:                             # %if.else851
	movzbl	16(%r9), %eax
	cmpq	$25, %rax
	jne	.LBB16_332
# BB#333:                               # %land.lhs.true858
	movq	%r9, %rbx
	movl	156(%rsp), %edi
	testq	%rdi, %rdi
	jne	.LBB16_335
# BB#334:                               # %land.lhs.true858
	testq	%r8, %r12
	je	.LBB16_335
.LBB16_332:
	movq	%r15, %rcx
	movq	%r15, %rbx
	jmp	.LBB16_340
.LBB16_311:
	movq	%r15, %rbx
	jmp	.LBB16_340
.LBB16_335:                             # %land.lhs.true862
	movq	%r8, %r12
	movq	8(%r10), %rsi
	movl	60(%rsi), %eax
	movl	60(%r15), %ecx
	andq	$511, %rax              # imm = 0x1FF
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rcx, %rax
	jae	.LBB16_336
# BB#337:                               # %land.lhs.true875
	callq	signed_or_unsigned_type
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	int_fits_type_p
	movq	%r12, %r8
	testq	%r8, %rax
	movq	%r15, %rbx
	je	.LBB16_339
# BB#338:                               # %land.lhs.true875
	movq	80(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB16_339
.LBB16_325:                             # %if.then813
	movq	%r8, %r12
	callq	common_type
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	signed_or_unsigned_type
	movq	%r12, %r8
	movq	%rax, %rbx
.LBB16_339:                             # %land.lhs.true875
	movq	%r15, %rcx
	jmp	.LBB16_340
.LBB16_261:                             # %if.else605
	cmpq	$13, %r9
	jne	.LBB16_270
# BB#262:                               # %if.else605
	cmpq	$25, %rdx
	jne	.LBB16_270
# BB#263:                               # %land.lhs.true615
	movq	%r13, %rdi
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	%rsi, %rbx
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%r10, %r12
	movq	%r11, 16(%rsp)          # 8-byte Spill
	callq	integer_zerop
	movq	16(%rsp), %r11          # 8-byte Reload
	movq	%r12, %r10
	movq	(%rsp), %r9             # 8-byte Reload
	movq	%rbx, %rsi
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	72(%rsp), %rcx          # 8-byte Reload
	testq	%r8, %rax
	je	.LBB16_270
# BB#264:                               # %if.then618
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movl	pedantic(%rip), %eax
	movl	extra_warnings(%rip), %ecx
	orq	%rax, %rcx
	testq	%r8, %rcx
	je	.LBB16_265
# BB#269:                               # %if.then622
	movabsq	$.L.str58, %rdi
	xorq	%rax, %rax
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	%r8, %rbx
	movq	%r10, %r12
	callq	pedwarn
	movq	%r12, %r10
	movq	(%rsp), %r9             # 8-byte Reload
	movq	%rbx, %r8
	jmp	.LBB16_266
.LBB16_125:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	xorq	%r12, %r12
.LBB16_72:                              # %sw.epilog
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	xorq	%rdi, %rdi
	jmp	.LBB16_297
.LBB16_336:
	movq	%r15, %rcx
	movq	%r15, %rbx
	movq	%r12, %r8
.LBB16_340:                             # %if.end885
	movq	24(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	je	.LBB16_341
# BB#342:                               # %if.then887
	movq	%r8, %r12
	leaq	148(%rsp), %rsi
	movq	%r14, %rdi
	callq	get_narrower
	movq	%rax, %r15
	cmpq	%r14, %r15
	jne	.LBB16_345
# BB#343:                               # %land.lhs.true892
	movq	8(%r14), %rax
	cmpq	%rax, %rbx
	jne	.LBB16_345
# BB#344:                               # %if.then897
	movl	16(%rbx), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, 148(%rsp)
.LBB16_345:                             # %if.end905
	movq	8(%r15), %rax
	movl	60(%rax), %esi
	movl	60(%rbx), %eax
	andq	$511, %rsi              # imm = 0x1FF
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rax, %rsi
	jae	.LBB16_346
# BB#347:                               # %land.lhs.true918
	andq	%r12, %rsi
	movq	%r13, %rdi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB16_348
.LBB16_346:
	movq	%rbx, %r15
	movq	%rbx, %rdi
.LBB16_355:                             # %if.end950
	movq	%r12, %r8
	jmp	.LBB16_356
.LBB16_341:
	movq	%rcx, %r15
	movq	%rbx, %rdi
.LBB16_356:                             # %if.end950
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	%r8, %rbx
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB16_357
# BB#358:                               # %if.then952
	movq	%r14, 136(%rsp)
	movq	%r13, 128(%rsp)
	movq	%rdi, 120(%rsp)
	movl	%ecx, 116(%rsp)
	movq	%rdi, %r14
	leaq	136(%rsp), %rdi
	leaq	128(%rsp), %rsi
	leaq	120(%rsp), %rdx
	leaq	116(%rsp), %rcx
	callq	shorten_compare
	testq	%rax, %rax
	movq	%rbx, %rdx
	jne	.LBB16_434
# BB#359:                               # %if.end957
	movq	%r14, %rdi
	movq	136(%rsp), %r14
	movq	128(%rsp), %r13
	movslq	warn_sign_compare(%rip), %rax
	movl	116(%rsp), %ecx
	testq	%rax, %rax
	js	.LBB16_360
# BB#361:                               # %cond.false
	testq	%rdx, %rax
	jmp	.LBB16_362
.LBB16_357:
	movq	%r15, 56(%rsp)          # 8-byte Spill
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	16(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB16_418
.LBB16_348:                             # %land.lhs.true929
	movzbl	17(%rbx), %eax
	movl	148(%rsp), %edi
	testq	$32, %rax
	movq	%r12, %r8
	je	.LBB16_352
# BB#349:                               # %land.lhs.true929
	testq	%r8, %rdi
	jne	.LBB16_352
# BB#350:
	movq	%rbx, %r15
	movq	%rbx, %rdi
	jmp	.LBB16_356
.LBB16_360:                             # %cond.true
	movl	extra_warnings(%rip), %eax
	testq	%rax, %rax
.LBB16_362:                             # %cond.false
	movq	$1, %rbx
	movq	%r15, 56(%rsp)          # 8-byte Spill
	movq	32(%rsp), %rsi          # 8-byte Reload
	je	.LBB16_418
# BB#363:                               # %cond.false
	movl	skip_evaluation(%rip), %eax
	testq	%rdx, %rax
	jne	.LBB16_418
# BB#364:                               # %if.then966
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	96(%rsp), %r15          # 8-byte Reload
	movq	8(%r15), %rax
	movq	88(%rsp), %r12          # 8-byte Reload
	movq	8(%r12), %rcx
	movl	16(%rax), %eax
	andq	$8192, %rax             # imm = 0x2000
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movl	16(%rcx), %esi
	movq	$13, %rcx
	shrq	%cl, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	$13, %rcx
	shrq	%cl, %rsi
	movq	%rsi, 64(%rsp)          # 8-byte Spill
	leaq	112(%rsp), %rsi
	movq	%r14, %rdi
	movq	%rdx, %rbx
	callq	get_narrower
	movq	%rax, 80(%rsp)          # 8-byte Spill
	leaq	108(%rsp), %rsi
	movq	%r13, %rdi
	callq	get_narrower
	movq	%r15, 136(%rsp)
	movq	%r12, 128(%rsp)
	movq	%r12, %r8
	movzbl	16(%r15), %ecx
	movq	%r15, %rdx
	addq	$-114, %rcx
	andq	%rbx, %rcx
	cmpq	$2, %rcx
	ja	.LBB16_365
# BB#368:                               # %land.lhs.true1010.lr.ph
	movq	global_trees(%rip), %r9
	movq	%rdx, %rdi
.LBB16_369:                             # %land.lhs.true1010
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rdi), %rcx
	cmpq	%r9, %rcx
	je	.LBB16_366
# BB#370:                               # %land.rhs1016
                                        #   in Loop: Header=BB16_369 Depth=1
	movq	8(%rdi), %rdx
	movq	8(%rcx), %rsi
	cmpq	%rsi, %rdx
	jne	.LBB16_366
# BB#371:                               # %while.body1028
                                        #   in Loop: Header=BB16_369 Depth=1
	movq	%rcx, 136(%rsp)
	movzbl	16(%rcx), %edx
	addq	$-114, %rdx
	andq	%rbx, %rdx
	cmpq	$3, %rdx
	movq	%rcx, %rdi
	jb	.LBB16_369
	jmp	.LBB16_366
.LBB16_352:                             # %if.then938
	movq	%r8, %r12
	movq	8(%r15), %rsi
	callq	signed_or_unsigned_type
	movq	8(%r14), %rcx
	movq	%rax, %rdi
	cmpq	%rdi, %rcx
	movq	$1, 16(%rsp)            # 8-byte Folded Spill
	je	.LBB16_354
# BB#353:                               # %if.then946
	movq	%rdi, %r15
                                        # kill: RDI<def> R15<kill>
	movq	%r14, %rsi
	callq	convert
	movq	%r15, %rdi
	movq	%rax, %r14
.LBB16_354:                             # %if.end950
	movq	%rbx, %r15
	jmp	.LBB16_355
.LBB16_365:
	movq	%rdx, %rdi
.LBB16_366:                             # %while.cond1033.preheader
	movq	%rax, %r15
	movzbl	16(%r8), %eax
	addq	$-114, %rax
	andq	%rbx, %rax
	cmpq	$2, %rax
	ja	.LBB16_367
# BB#372:                               # %land.lhs.true1054.lr.ph
	movq	global_trees(%rip), %r9
	movq	%r8, %rax
.LBB16_373:                             # %land.lhs.true1054
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax), %rcx
	cmpq	%r9, %rcx
	je	.LBB16_376
# BB#374:                               # %land.rhs1060
                                        #   in Loop: Header=BB16_373 Depth=1
	movq	8(%rax), %rdx
	movq	8(%rcx), %rsi
	cmpq	%rsi, %rdx
	jne	.LBB16_376
# BB#375:                               # %while.body1072
                                        #   in Loop: Header=BB16_373 Depth=1
	movq	%rcx, 128(%rsp)
	movzbl	16(%rcx), %edx
	addq	$-114, %rdx
	andq	%rbx, %rdx
	cmpq	$3, %rdx
	movq	%rcx, %rax
	jb	.LBB16_373
	jmp	.LBB16_376
.LBB16_367:
	movq	%r8, %rax
.LBB16_376:                             # %while.end1076
	movq	%rax, %rcx
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	17(%rax), %eax
	testq	$32, %rax
	je	.LBB16_391
# BB#377:                               # %while.end1076
	movq	64(%rsp), %rax          # 8-byte Reload
	andq	$1, %rax
	cmpq	%rax, 48(%rsp)          # 8-byte Folded Reload
	je	.LBB16_391
# BB#378:                               # %if.else1088
	movq	40(%rsp), %rax          # 8-byte Reload
	andq	%rbx, %rax
	movq	%rcx, %r12
	je	.LBB16_380
# BB#379:                               # %if.else1088
	movq	%rdi, %r12
.LBB16_380:                             # %if.else1088
	testq	%rax, %rax
	je	.LBB16_382
# BB#381:                               # %if.else1088
	movq	%rcx, %rdi
.LBB16_382:                             # %if.else1088
	callq	tree_expr_nonnegative_p
	testq	%rbx, %rax
	jne	.LBB16_391
# BB#383:                               # %if.else1096
	movl	16(%r12), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$25, %rcx
	jne	.LBB16_387
# BB#384:                               # %if.else1096
	movl	116(%rsp), %ecx
	leaq	-101(%rcx), %rcx
	andq	%rbx, %rcx
	cmpq	$1, %rcx
	ja	.LBB16_387
# BB#385:                               # %land.lhs.true1109
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	signed_type
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	int_fits_type_p
	testq	%rbx, %rax
	jne	.LBB16_391
# BB#386:                               # %land.lhs.true1109.if.else1114_crit_edge
	movl	16(%r12), %eax
.LBB16_387:                             # %if.else1114
	andq	$255, %rax
	cmpq	$25, %rax
	jne	.LBB16_390
# BB#388:                               # %land.lhs.true1121
	movq	8(%r12), %rax
	movzbl	16(%rax), %ecx
	cmpq	$10, %rcx
	jne	.LBB16_390
# BB#389:                               # %land.lhs.true1130
	movq	112(%rax), %r12
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	signed_type
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	int_fits_type_p
	testq	%rbx, %rax
	jne	.LBB16_391
.LBB16_390:                             # %if.else1138
	movabsq	$.L.str60, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB16_391:                             # %if.end1143
	movq	80(%rsp), %rax          # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$90, %rax
	movabsq	$0, %rcx
	movabsq	$1, %rsi
	movq	%rcx, %rdx
	jne	.LBB16_393
# BB#392:                               # %if.end1143
	movq	%rsi, %rdx
.LBB16_393:                             # %if.end1143
	movzbl	16(%r15), %edi
	cmpq	$90, %rdi
	movq	32(%rsp), %r12          # 8-byte Reload
	jne	.LBB16_395
# BB#394:                               # %if.end1143
	movq	%rsi, %rcx
.LBB16_395:                             # %if.end1143
	xorq	%rcx, %rdx
	movq	24(%rsp), %rdi          # 8-byte Reload
	je	.LBB16_396
# BB#397:                               # %if.then1158
	cmpq	$90, %rax
	jne	.LBB16_399
# BB#398:                               # %if.then1165
	movq	%r15, 96(%rsp)          # 8-byte Spill
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rdi
	leaq	112(%rsp), %rsi
	callq	get_narrower
	movq	%rax, %rdi
	jmp	.LBB16_400
.LBB16_396:
	movq	%r12, %rsi
	jmp	.LBB16_406
.LBB16_247:                             # %comp_target_types.exit1713
	testq	%rbx, %rax
	je	.LBB16_260
.LBB16_248:                             # %if.then570
	movq	%r15, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	common_type
	movq	8(%r15), %rcx
	cmpq	$0, 32(%rcx)
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
	movabsq	$1, %rsi
	movq	%rax, %rdx
	jne	.LBB16_250
# BB#249:                               # %if.then570
	movq	%rsi, %rdx
.LBB16_250:                             # %if.then570
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	8(%rdi), %rdi
	cmpq	$0, 32(%rdi)
	jne	.LBB16_252
# BB#251:                               # %if.then570
	movq	%rsi, %rax
.LBB16_252:                             # %if.then570
	cmpq	%rax, %rdx
	je	.LBB16_254
# BB#253:                               # %if.then588
	movabsq	$.L.str57, %rdi
	xorq	%rax, %rax
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	callq	pedwarn
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rcx
	movq	%rbx, %r8
	xorq	%rbx, %rbx
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	(%rsp), %r9             # 8-byte Reload
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	16(%rsp), %r11          # 8-byte Reload
	movq	24(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB16_297
.LBB16_193:                             # %if.else454
	cmpq	$13, %r9
	jne	.LBB16_197
# BB#194:                               # %land.lhs.true457
	movzbl	16(%r13), %eax
	cmpq	$25, %rax
	jne	.LBB16_197
# BB#195:                               # %land.lhs.true464
	movq	%r13, %rdi
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%r9, %rbx
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movq	%r11, 16(%rsp)          # 8-byte Spill
	callq	integer_zerop
	movq	16(%rsp), %r11          # 8-byte Reload
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	%rbx, %r9
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	testq	%r12, %rax
	je	.LBB16_197
# BB#196:
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	movq	%r15, %rdi
	jmp	.LBB16_208
.LBB16_270:                             # %if.else624
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	cmpq	$13, %r10
	jne	.LBB16_277
# BB#271:                               # %land.lhs.true627
	movq	%r8, %rbx
	movzbl	16(%r14), %eax
	cmpq	$25, %rax
	jne	.LBB16_282
# BB#272:                               # %land.lhs.true634
	movq	%r14, %rdi
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%r9, %r12
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movq	%r11, 16(%rsp)          # 8-byte Spill
	callq	integer_zerop
	movq	16(%rsp), %r11          # 8-byte Reload
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	%r12, %r9
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %r8
	testq	%r8, %rax
	je	.LBB16_277
# BB#273:                               # %if.then637
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB16_274
# BB#276:                               # %if.then639
	movabsq	$.L.str58, %rdi
	xorq	%rax, %rax
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	%r8, %rbx
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%r10, %r12
	callq	pedwarn
	movq	%r12, %r10
	movq	(%rsp), %r9             # 8-byte Reload
	movq	%rbx, %r8
	jmp	.LBB16_275
.LBB16_197:                             # %if.else468
	cmpq	$13, %r10
	jne	.LBB16_201
# BB#198:                               # %land.lhs.true471
	movq	%rsi, %rbx
	movzbl	16(%r14), %eax
	cmpq	$25, %rax
	jne	.LBB16_206
# BB#199:                               # %land.lhs.true478
	movq	%r14, %rdi
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movq	%r11, 16(%rsp)          # 8-byte Spill
	callq	integer_zerop
	movq	16(%rsp), %r11          # 8-byte Reload
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	(%rsp), %r9             # 8-byte Reload
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	testq	%r12, %rax
	movq	%rbx, %rsi
	je	.LBB16_201
# BB#200:
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	movq	40(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB16_208
.LBB16_18:                              # %if.else86
	xorq	%r12, %r12
	cmpq	$13, %r10
	movq	$59, %rcx
	jne	.LBB16_71
# BB#19:                                # %if.else86
	cmpq	$6, %r9
	jne	.LBB16_71
# BB#20:                                # %if.then90
	movq	$59, %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	pointer_int_sum
	jmp	.LBB16_434
.LBB16_399:                             # %if.else1170
	movq	32(%r15), %rdi
	leaq	108(%rsp), %rsi
	callq	get_narrower
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	80(%rsp), %rdi          # 8-byte Reload
.LBB16_400:                             # %if.end1175
	xorq	%rsi, %rsi
	movq	%rdi, %r15
	callq	host_integerp
	movq	%rbx, %rdx
	testq	%rdx, %rax
	jne	.LBB16_402
# BB#401:                               # %lor.lhs.false1178
	xorq	%rsi, %rsi
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rdx, %rbx
	callq	host_integerp
	movq	%rbx, %rdx
	testq	%rdx, %rax
	je	.LBB16_411
.LBB16_402:                             # %if.then1181
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	movq	%rdx, %rbx
	callq	host_integerp
	testq	%rbx, %rax
	je	.LBB16_404
# BB#403:                               # %if.then1184
	movl	108(%rsp), %eax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	tree_low_cst
	movq	96(%rsp), %r15          # 8-byte Reload
	jmp	.LBB16_405
.LBB16_158:
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	xorq	%r12, %r12
.LBB16_294:                             # %sw.epilog
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	jmp	.LBB16_295
.LBB16_277:                             # %if.else641
	movq	%r8, %rbx
	cmpq	$13, %r9
	jne	.LBB16_282
# BB#278:                               # %if.else641
	cmpq	$6, %r10
	jne	.LBB16_282
# BB#279:                               # %if.then647
	movl	flag_traditional(%rip), %eax
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	testq	%rax, %rax
	jne	.LBB16_280
# BB#281:                               # %if.then649
	movabsq	$.L.str55, %rdi
	xorq	%rax, %rax
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movq	%r11, 16(%rsp)          # 8-byte Spill
	callq	pedwarn
	movq	16(%rsp), %r11          # 8-byte Reload
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	(%rsp), %r9             # 8-byte Reload
	movq	32(%rsp), %rsi          # 8-byte Reload
.LBB16_280:
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	%r15, %rdi
	movq	%rsi, %rcx
	movq	%rbx, %r8
	xorq	%rbx, %rbx
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	xorq	%r12, %r12
	jmp	.LBB16_297
.LBB16_27:                              # %comp_target_types.exit
	movq	%rbx, %r8
	testq	%r8, %rax
	movq	%r12, %rsi
	movabsq	$0, %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	(%rsp), %r9             # 8-byte Reload
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	16(%rsp), %r11          # 8-byte Reload
	je	.LBB16_58
.LBB16_28:                              # %if.then102
	movq	8(%r14), %rax
	movl	pedantic(%rip), %ecx
	movl	warn_pointer_arith(%rip), %edx
	movq	global_trees+240(%rip), %r12
	movq	8(%rax), %rbx
	orq	%rcx, %rdx
	testq	%r8, %rdx
	je	.LBB16_33
# BB#29:                                # %if.then.i
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$5, %rcx
	jne	.LBB16_31
# BB#30:                                # %if.then5.i
	movabsq	$.L.str180, %rdi
	xorq	%rax, %rax
	movq	%r8, %r15
	callq	pedwarn
	movq	%r15, %r8
	movl	16(%rbx), %eax
.LBB16_31:                              # %if.end.i1716
	andq	$255, %rax
	cmpq	$23, %rax
	jne	.LBB16_33
# BB#32:                                # %if.then11.i
	movabsq	$.L.str181, %rdi
	xorq	%rax, %rax
	movq	%r8, %r15
	callq	pedwarn
	movq	%r15, %r8
.LBB16_33:                              # %if.end13.i
	movzbl	16(%r14), %eax
	cmpq	$115, %rax
	movq	%r14, %rdi
	jne	.LBB16_35
# BB#34:                                # %cond.true.i
	movq	32(%r14), %rdi
.LBB16_35:                              # %cond.end.i
	movq	%r12, 96(%rsp)          # 8-byte Spill
	movzbl	16(%r13), %eax
	cmpq	$115, %rax
	movq	%r13, %rsi
	jne	.LBB16_37
# BB#36:                                # %cond.true24.i
	movq	32(%r13), %rsi
.LBB16_37:                              # %cond.end29.i
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movzbl	16(%rdi), %eax
	cmpq	$59, %rax
	movabsq	$global_trees+88, %rax
	jne	.LBB16_38
# BB#39:                                # %if.then36.i
	movq	32(%rdi), %rdx
	movq	%rdi, %rcx
	addq	$40, %rcx
	movq	%rdx, %rdi
	jmp	.LBB16_40
.LBB16_58:                              # %if.else104
	xorq	%r12, %r12
	cmpq	$13, %r9
	movq	$60, %rcx
	jne	.LBB16_71
# BB#59:                                # %if.else104
	cmpq	$6, %r10
	jne	.LBB16_71
# BB#60:                                # %if.then108
	movq	$60, %rdi
.LBB16_17:                              # %return
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	pointer_int_sum
	jmp	.LBB16_434
.LBB16_228:                             # %comp_target_types.exit1701
	testq	%rbx, %rax
	je	.LBB16_237
.LBB16_229:                             # %if.then530
	movq	%r15, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	common_type
	movl	pedantic(%rip), %ecx
	movq	%rax, %rdi
	testq	%rcx, %rcx
	je	.LBB16_230
# BB#234:                               # %land.lhs.true533
	movq	8(%r15), %rax
	movzbl	16(%rax), %eax
	cmpq	$23, %rax
	jne	.LBB16_230
# BB#235:                               # %if.then542
	movq	%rdi, %r12
	movabsq	$.L.str56, %rdi
	jmp	.LBB16_236
.LBB16_201:                             # %if.else482
	movq	%rsi, %rbx
	cmpq	$13, %r9
	jne	.LBB16_206
# BB#202:                               # %if.else482
	cmpq	$6, %r10
	jne	.LBB16_206
# BB#203:                               # %if.then488
	movq	%r11, 16(%rsp)          # 8-byte Spill
	movq	%r9, (%rsp)             # 8-byte Spill
	movl	flag_traditional(%rip), %eax
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	testq	%rax, %rax
	movq	%rbx, %rsi
	jne	.LBB16_204
# BB#205:                               # %if.then490
	movabsq	$.L.str55, %rdi
	xorq	%rax, %rax
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	%rsi, %rbx
	movq	%r10, 8(%rsp)           # 8-byte Spill
	callq	pedwarn
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	%rbx, %rsi
.LBB16_204:
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	movq	%r15, %rdi
	movq	%rsi, %rcx
	movq	%r12, %r8
	xorq	%r12, %r12
	jmp	.LBB16_104
.LBB16_282:                             # %if.else651
	cmpq	$6, %r9
	jne	.LBB16_283
# BB#284:                               # %if.else651
	cmpq	$13, %r10
	movq	%rbx, %r8
	jne	.LBB16_285
# BB#286:                               # %if.then657
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB16_289
# BB#287:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	jmp	.LBB16_288
.LBB16_254:                             # %if.else589
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	movq	%rbx, %r8
	movq	(%rsp), %r9             # 8-byte Reload
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	16(%rsp), %r11          # 8-byte Reload
	movq	24(%rsp), %rdi          # 8-byte Reload
	je	.LBB16_255
# BB#256:                               # %land.lhs.true591
	movzbl	16(%rcx), %eax
	cmpq	$23, %rax
	jne	.LBB16_257
# BB#259:                               # %if.then600
	movabsq	$.L.str56, %rdi
	xorq	%rax, %rax
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	%r9, %rbx
	movq	%r10, %r12
	callq	pedwarn
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %r10
	movq	%rbx, %r9
	jmp	.LBB16_258
.LBB16_206:                             # %if.else492
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	cmpq	$6, %r9
	movq	%rbx, %rsi
	jne	.LBB16_207
# BB#209:                               # %if.else492
	cmpq	$13, %r10
	movq	%r12, %r8
	jne	.LBB16_210
# BB#212:                               # %if.then498
	movq	%r11, 16(%rsp)          # 8-byte Spill
	movq	%r9, %r12
	movq	%r8, %rbx
	movl	flag_traditional(%rip), %eax
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	testq	%rax, %rax
	jne	.LBB16_213
# BB#214:                               # %if.then500
	movabsq	$.L.str55, %rdi
	xorq	%rax, %rax
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%r10, 8(%rsp)           # 8-byte Spill
	callq	pedwarn
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	32(%rsp), %rsi          # 8-byte Reload
.LBB16_213:
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%rsi, %rcx
	movq	%rbx, %r8
	xorq	%rbx, %rbx
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	%r12, %r9
	xorq	%r12, %r12
	movq	16(%rsp), %r11          # 8-byte Reload
	jmp	.LBB16_297
.LBB16_404:                             # %if.else1186
	movl	112(%rsp), %eax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	tree_low_cst
.LBB16_405:                             # %if.end1188
	movq	8(%r15), %rcx
	movl	60(%rcx), %ecx
	movq	24(%rsp), %rdi          # 8-byte Reload
	movl	60(%rdi), %edx
	andq	$511, %rcx              # imm = 0x1FF
	andq	$511, %rdx              # imm = 0x1FF
	cmpq	%rdx, %rcx
	movq	%r12, %rsi
	jae	.LBB16_406
# BB#407:                               # %if.end1188
	movq	%rcx, %rdx
	andq	%rbx, %rdx
	cmpq	$63, %rdx
	ja	.LBB16_406
# BB#408:                               # %if.end1188
	movq	88(%rsp), %rdx          # 8-byte Reload
	testq	%rbx, %rdx
	je	.LBB16_406
# BB#409:                               # %if.then1206
	movq	$-1, %rdx
	shlq	%cl, %rdx
	andq	%rdx, %rax
	cmpq	%rdx, %rax
	je	.LBB16_406
# BB#410:                               # %if.then1209
	movq	%rdi, %r15
	movabsq	$.L.str61, %rdi
	xorq	%rax, %rax
	movq	%rsi, %rbx
	callq	warning
	movq	%r15, %rdi
	movq	%rbx, %rsi
.LBB16_406:
	movl	116(%rsp), %ecx
	movq	$1, %rbx
.LBB16_418:                             # %if.end1248
	testq	%rdi, %rdi
	je	.LBB16_419
# BB#420:                               # %if.end1251
	testq	%rbx, %rbx
	je	.LBB16_422
# BB#421:
	movq	%rdi, %r12
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	72(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB16_428
.LBB16_419:                             # %if.then1250
	movq	%rsi, %rdi
	callq	binary_op_error
	movq	global_trees(%rip), %rax
	jmp	.LBB16_434
.LBB16_422:                             # %if.then1253
	movq	8(%r14), %rax
	movq	%rcx, %r15
	cmpq	%rdi, %rax
	je	.LBB16_424
# BB#423:                               # %if.then1258
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	movq	%r14, %rsi
	callq	convert
	movq	%rbx, %rdi
	movq	%rax, %r14
.LBB16_424:                             # %if.end1260
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	8(%r13), %rax
	cmpq	%rdi, %rax
	je	.LBB16_425
# BB#426:                               # %if.then1265
	movq	%rdi, %r12
	movq	%r13, %rsi
	callq	convert
	movq	%rax, %r13
	jmp	.LBB16_427
.LBB16_425:
	movq	%rdi, %r12
.LBB16_427:                             # %if.end1268
	movq	%r15, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
.LBB16_428:                             # %if.end1268
	movq	%rax, %r15
	testq	%rbx, %rbx
	je	.LBB16_430
# BB#429:                               # %if.end1268
	movq	%rbx, %r12
.LBB16_430:                             # %if.end1268
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	%r13, %rcx
	callq	build
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	fold
	cmpq	%rbx, %rax
	jne	.LBB16_432
# BB#431:                               # %if.then1277
	movl	16(%r14), %ecx
	movl	16(%r13), %edx
	movl	16(%rbx), %esi
	andq	%rcx, %rdx
	andq	$512, %rdx              # imm = 0x200
	movabsq	$4294966783, %rcx       # imm = 0xFFFFFDFF
	andq	%rsi, %rcx
	orq	%rdx, %rcx
	movl	%ecx, 16(%rbx)
.LBB16_432:                             # %if.end1292
	movq	%r15, %rdi
	testq	%rdi, %rdi
	je	.LBB16_434
# BB#433:                               # %if.then1295
	movq	%rax, %rsi
	callq	convert
.LBB16_434:                             # %return
	addq	$160, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB16_260:                             # %if.else603
	movq	global_trees+224(%rip), %r12
	movabsq	$.L.str54, %rdi
	xorq	%rax, %rax
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	callq	pedwarn
	movq	%r12, %rdi
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	32(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB16_232
.LBB16_223:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	jmp	.LBB16_81
.LBB16_230:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	jmp	.LBB16_231
.LBB16_102:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	xorq	%rdi, %rdi
.LBB16_103:                             # %sw.epilog
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rcx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
.LBB16_104:                             # %sw.epilog
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	(%rsp), %r9             # 8-byte Reload
	movq	16(%rsp), %r11          # 8-byte Reload
	jmp	.LBB16_297
.LBB16_143:                             # %if.then328
	movabsq	$.L.str50, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	%rbx, %r8
	movq	%r12, %rsi
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	(%rsp), %r9             # 8-byte Reload
	movq	16(%rsp), %r11          # 8-byte Reload
.LBB16_146:                             # %if.end341
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB16_148
# BB#147:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	movq	%r15, %rdi
	movq	%rsi, %rcx
	movq	8(%rsp), %r10           # 8-byte Reload
	jmp	.LBB16_297
.LBB16_148:                             # %if.then343
	movq	%r11, 16(%rsp)          # 8-byte Spill
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%rsi, %r12
	movq	8(%r13), %rax
	movq	128(%rax), %rax
	movq	integer_types+40(%rip), %rdi
	cmpq	%rdi, %rax
	movq	$1, %rbx
	je	.LBB16_149
# BB#150:                               # %if.then350
	movq	%r13, %rsi
	movq	%r8, %rbx
	callq	convert
	movq	%rbx, %r8
	movq	$1, %rbx
	movq	%rax, %r13
.LBB16_149:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	%r15, %rdi
	movq	%r12, %rsi
	xorq	%r12, %r12
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	movq	%rsi, %rcx
	jmp	.LBB16_233
.LBB16_38:
	movq	%rax, %rcx
.LBB16_40:                              # %if.end43.i
	movq	%r8, %rbx
	movzbl	16(%rsi), %edx
	movq	(%rcx), %r12
	cmpq	$59, %rdx
	jne	.LBB16_42
# BB#41:                                # %if.then49.i
	movq	32(%rsi), %rcx
	movq	%rsi, %rax
	addq	$40, %rax
	movq	%rcx, %rsi
.LBB16_42:                              # %if.end57.i
	movq	(%rax), %r15
	xorq	%rdx, %rdx
	callq	operand_equal_p
	andq	%rbx, %rax
	movq	%r13, %rbx
	je	.LBB16_44
# BB#43:                                # %if.end57.i
	movq	%r15, %rbx
.LBB16_44:                              # %if.end57.i
	testq	%rax, %rax
	je	.LBB16_46
# BB#45:                                # %if.end57.i
	movq	%r12, %r14
.LBB16_46:                              # %if.end57.i
	movq	96(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	convert
	movq	%rax, %r14
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	$60, %rdi
	xorq	%rcx, %rcx
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	build_binary_op
	movq	8(%r13), %rcx
	movq	8(%rcx), %rcx
	cmpq	$0, 32(%rcx)
	movq	%rax, %r14
	jne	.LBB16_49
# BB#47:                                # %lor.lhs.false70.i
	movzbl	16(%rcx), %eax
	cmpq	$5, %rax
	je	.LBB16_49
# BB#48:                                # %if.then80.i
	movabsq	$.L.str14, %rdi
	xorq	%rax, %rax
	callq	error
.LBB16_49:                              # %if.end81.i
	movq	88(%rsp), %rdx          # 8-byte Reload
	movzbl	16(%rdx), %ecx
	cmpq	$23, %rcx
	ja	.LBB16_52
# BB#50:                                # %if.end81.i
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$8388641, %rax          # imm = 0x800021
	je	.LBB16_52
# BB#51:                                # %if.then.i.i
	movq	global_trees+128(%rip), %r12
	jmp	.LBB16_56
.LBB16_52:                              # %if.end.i.i
	cmpq	$0, 32(%rdx)
	jne	.LBB16_55
# BB#53:                                # %if.end.i.i
	cmpq	$5, %rcx
	je	.LBB16_55
# BB#54:                                # %if.then13.i.i
	movabsq	$.L.str14, %rdi
	xorq	%rax, %rax
	callq	error
	movq	global_trees+128(%rip), %r12
	jmp	.LBB16_56
.LBB16_237:                             # %if.else544
	movq	global_trees+224(%rip), %r12
	movabsq	$.L.str54, %rdi
.LBB16_236:                             # %sw.epilog
	xorq	%rax, %rax
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	callq	pedwarn
	movq	%r12, %rdi
.LBB16_231:                             # %sw.epilog
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
.LBB16_232:                             # %sw.epilog
	movq	%rsi, %rcx
	movq	%rbx, %r8
	xorq	%rbx, %rbx
	jmp	.LBB16_233
.LBB16_411:                             # %if.else1212
	movq	%r15, %rcx
	movl	112(%rsp), %eax
	testq	%rax, %rax
	movq	$1, %rbx
	movq	96(%rsp), %r15          # 8-byte Reload
	je	.LBB16_412
# BB#413:                               # %if.else1212
	movl	108(%rsp), %eax
	testq	%rdx, %rax
	movq	%r12, %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	je	.LBB16_417
# BB#414:                               # %land.lhs.true1216
	movq	8(%rcx), %rax
	movl	60(%rax), %ecx
	movl	60(%rdi), %eax
	andq	$511, %rcx              # imm = 0x1FF
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rax, %rcx
	jae	.LBB16_417
# BB#415:                               # %land.lhs.true1229
	movq	8(%r15), %rcx
	movl	60(%rcx), %ecx
	andq	$511, %rcx              # imm = 0x1FF
	andq	%rdx, %rax
	cmpq	%rax, %rcx
	jae	.LBB16_417
# BB#416:                               # %if.then1242
	movq	%rdi, %r12
	movabsq	$.L.str62, %rdi
	xorq	%rax, %rax
	movq	%rsi, %rbx
	callq	warning
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	$1, %rbx
.LBB16_417:                             # %if.end1248
	movl	116(%rsp), %ecx
	jmp	.LBB16_418
.LBB16_255:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	movq	32(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB16_211
.LBB16_265:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
.LBB16_266:                             # %sw.epilog
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	movq	%r15, %rdi
	jmp	.LBB16_267
.LBB16_257:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
.LBB16_258:                             # %sw.epilog
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rcx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	jmp	.LBB16_268
.LBB16_55:                              # %if.end14.i.i
	movq	integer_types(%rip), %rax
	movl	60(%rax), %edi
	movq	40(%rdx), %rbx
	movq	$3, %rcx
	shrq	%cl, %rdi
	andq	$63, %rdi
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	$63, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, %r12
.LBB16_56:                              # %c_size_in_bytes.exit.i
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	convert
	movq	%rax, %rcx
	movq	$71, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	build
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	fold
	cmpq	%rbx, %rax
	jne	.LBB16_434
# BB#57:                                # %if.then87.i
	movl	16(%r14), %eax
	movl	16(%r12), %ecx
	movl	16(%rbx), %edx
	andq	%rax, %rcx
	andq	$512, %rcx              # imm = 0x200
	movabsq	$4294966783, %rax       # imm = 0xFFFFFDFF
	andq	%rdx, %rax
	orq	%rcx, %rax
	movl	%eax, 16(%rbx)
	movq	%rbx, %rax
	jmp	.LBB16_434
.LBB16_97:
	movabsq	$0, %rax
.LBB16_79:                              # %land.rhs151
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	%r12, %rsi
	movq	(%rsp), %r9             # 8-byte Reload
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	16(%rsp), %r11          # 8-byte Reload
.LBB16_80:                              # %lor.end
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
.LBB16_81:                              # %sw.epilog
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
.LBB16_82:                              # %sw.epilog
	xorq	%rdi, %rdi
	movq	%rsi, %rcx
	movq	%rbx, %r8
	xorq	%rbx, %rbx
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB16_297
.LBB16_130:                             # %if.then281
	movabsq	$.L.str48, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	%rbx, %r8
	movq	%r12, %rsi
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	(%rsp), %r9             # 8-byte Reload
	movq	16(%rsp), %r11          # 8-byte Reload
	movq	$82, %rcx
.LBB16_133:                             # %if.end294
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB16_135
# BB#134:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	movq	%r15, %rdi
	movq	8(%rsp), %r10           # 8-byte Reload
	jmp	.LBB16_297
.LBB16_135:                             # %if.then296
	movq	%r11, 16(%rsp)          # 8-byte Spill
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	8(%r13), %rax
	movq	128(%rax), %rax
	movq	integer_types+40(%rip), %rdi
	cmpq	%rdi, %rax
	movq	$1, %rbx
	je	.LBB16_136
# BB#137:                               # %if.then303
	movq	%rsi, %r12
	movq	%r13, %rsi
	movq	%r8, %rbx
	callq	convert
	movq	$82, %rcx
	movq	%r12, %rsi
	movq	%rbx, %r8
	movq	$1, %rbx
	movq	%rax, %r13
.LBB16_136:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	xorq	%r12, %r12
	jmp	.LBB16_122
.LBB16_111:                             # %if.then233
	movabsq	$.L.str46, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	%rbx, %r8
	movq	%r12, %rsi
	xorq	%r12, %r12
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	(%rsp), %r9             # 8-byte Reload
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	16(%rsp), %r11          # 8-byte Reload
	movq	$83, %rcx
	jmp	.LBB16_118
.LBB16_283:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	jmp	.LBB16_82
.LBB16_285:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
.LBB16_210:
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	xorq	%rdi, %rdi
	jmp	.LBB16_211
.LBB16_113:
	movabsq	$0, %rax
.LBB16_115:                             # %if.else234
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movl	60(%r15), %esi
	andq	$511, %rsi              # imm = 0x1FF
	movq	%r13, %rdi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB16_117
# BB#116:                               # %if.then245
	movabsq	$.L.str47, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB16_117:                             # %if.end248
	movq	%rbx, %r8
	movq	%r12, %rsi
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	(%rsp), %r9             # 8-byte Reload
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	16(%rsp), %r11          # 8-byte Reload
	movq	$83, %rcx
	movq	24(%rsp), %r12          # 8-byte Reload
.LBB16_118:                             # %if.end248
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB16_120
# BB#119:
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	movq	%r15, %rdi
	jmp	.LBB16_297
.LBB16_120:                             # %if.then250
	movq	%r11, 16(%rsp)          # 8-byte Spill
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	8(%r13), %rax
	movq	128(%rax), %rax
	movq	integer_types+40(%rip), %rdi
	cmpq	%rdi, %rax
	movq	$1, %rbx
	je	.LBB16_121
# BB#123:                               # %if.then256
	movq	%r12, 24(%rsp)          # 8-byte Spill
	movq	%rsi, %r12
	movq	%r13, %rsi
	movq	%r8, %rbx
	callq	convert
	movq	$83, %rcx
	movq	%r12, %rsi
	movq	24(%rsp), %r12          # 8-byte Reload
	movq	%rbx, %r8
	movq	$1, %rbx
	movq	%rax, %r13
.LBB16_121:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
.LBB16_122:                             # %sw.epilog
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	movq	%r15, %rdi
.LBB16_233:                             # %sw.epilog
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	(%rsp), %r9             # 8-byte Reload
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	16(%rsp), %r11          # 8-byte Reload
	jmp	.LBB16_297
.LBB16_207:
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	xorq	%rdi, %rdi
.LBB16_208:                             # %sw.epilog
	movq	%rsi, %rcx
	movq	%r12, %r8
	xorq	%r12, %r12
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB16_297
.LBB16_165:
	movq	%rbx, %r12
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	64(%rsp), %rbx          # 8-byte Reload
.LBB16_166:                             # %comp_target_types.exit1689
	testq	%r12, %rax
	je	.LBB16_167
.LBB16_172:                             # %if.then396
	movq	%r15, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	common_type
	movq	%rax, %rdi
.LBB16_190:                             # %if.end449
	testq	%rdi, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	(%rsp), %r9             # 8-byte Reload
	movq	16(%rsp), %r11          # 8-byte Reload
	jne	.LBB16_192
# BB#191:
	movq	global_trees+224(%rip), %rdi
.LBB16_192:                             # %if.end449
	movq	%r12, %r8
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	jmp	.LBB16_296
.LBB16_167:                             # %if.else398
	movzbl	16(%rbx), %eax
	cmpq	$5, %rax
	jne	.LBB16_178
# BB#168:                               # %if.then405
	movl	pedantic(%rip), %eax
	xorq	%rdi, %rdi
	testq	%rax, %rax
	je	.LBB16_169
# BB#173:                               # %land.lhs.true407
	movq	%r14, %rdi
	callq	integer_zerop
	movq	%r12, %rbx
	testq	%rbx, %rax
	je	.LBB16_176
# BB#174:                               # %land.lhs.true407
	movq	96(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r14
	jne	.LBB16_176
# BB#175:
	movq	%rbx, %r12
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB16_189
.LBB16_178:                             # %if.else422
	movq	80(%rsp), %rax          # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$5, %rax
	movabsq	$0, %rcx
	jne	.LBB16_186
# BB#179:                               # %if.then429
	movl	pedantic(%rip), %eax
	xorq	%rdi, %rdi
	testq	%rax, %rax
	je	.LBB16_180
# BB#181:                               # %land.lhs.true431
	movq	%r13, %rdi
	callq	integer_zerop
	testq	%r12, %rax
	je	.LBB16_184
# BB#182:                               # %land.lhs.true431
	movq	88(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r13
	jne	.LBB16_184
# BB#183:
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB16_189
.LBB16_289:                             # %if.then659
	movabsq	$.L.str55, %rdi
	xorq	%rax, %rax
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%r9, %r12
	movq	%r10, %rbx
	movq	%r11, 16(%rsp)          # 8-byte Spill
	callq	pedwarn
	movq	16(%rsp), %r11          # 8-byte Reload
	movq	%rbx, %r10
	movq	%r12, %r9
	movq	32(%rsp), %rsi          # 8-byte Reload
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
.LBB16_288:                             # %sw.epilog
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	movq	40(%rsp), %rdi          # 8-byte Reload
.LBB16_211:                             # %sw.epilog
	movq	%rsi, %rcx
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB16_297
.LBB16_186:                             # %if.else446
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movabsq	$.L.str54, %rdi
	jmp	.LBB16_187
.LBB16_169:
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB16_190
.LBB16_274:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
.LBB16_275:                             # %sw.epilog
	xorq	%r12, %r12
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	movq	40(%rsp), %rdi          # 8-byte Reload
.LBB16_267:                             # %sw.epilog
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rcx
.LBB16_268:                             # %sw.epilog
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	16(%rsp), %r11          # 8-byte Reload
	jmp	.LBB16_297
.LBB16_412:
	movq	%r12, %rsi
	movl	116(%rsp), %ecx
	movq	24(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB16_418
.LBB16_176:                             # %land.lhs.true413
	movq	%rbx, %r12
	movq	80(%rsp), %rax          # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$23, %rax
	movabsq	$0, %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movabsq	$0, %rdi
	jne	.LBB16_190
# BB#177:                               # %if.then420
	movabsq	$.L.str53, %rdi
.LBB16_187:                             # %if.end449
	xorq	%rax, %rax
.LBB16_188:                             # %if.end449
	callq	pedwarn
.LBB16_189:                             # %if.end449
	xorq	%rdi, %rdi
	jmp	.LBB16_190
.LBB16_180:
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	jmp	.LBB16_190
.LBB16_184:                             # %land.lhs.true437
	movzbl	16(%rbx), %eax
	cmpq	$23, %rax
	movabsq	$0, %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movabsq	$0, %rdi
	jne	.LBB16_190
# BB#185:                               # %if.then444
	movabsq	$.L.str53, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	jmp	.LBB16_188
.Ltmp167:
	.size	build_binary_op, .Ltmp167-build_binary_op
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI16_0:
	.quad	.LBB16_14
	.quad	.LBB16_21
	.quad	.LBB16_61
	.quad	.LBB16_62
	.quad	.LBB16_62
	.quad	.LBB16_62
	.quad	.LBB16_62
	.quad	.LBB16_88
	.quad	.LBB16_297
	.quad	.LBB16_88
	.quad	.LBB16_297
	.quad	.LBB16_297
	.quad	.LBB16_62
	.quad	.LBB16_297
	.quad	.LBB16_297
	.quad	.LBB16_297
	.quad	.LBB16_297
	.quad	.LBB16_297
	.quad	.LBB16_297
	.quad	.LBB16_215
	.quad	.LBB16_215
	.quad	.LBB16_297
	.quad	.LBB16_297
	.quad	.LBB16_124
	.quad	.LBB16_106
	.quad	.LBB16_138
	.quad	.LBB16_138
	.quad	.LBB16_83
	.quad	.LBB16_83
	.quad	.LBB16_83
	.quad	.LBB16_83
	.quad	.LBB16_297
	.quad	.LBB16_98
	.quad	.LBB16_98
	.quad	.LBB16_98
	.quad	.LBB16_98
	.quad	.LBB16_98
	.quad	.LBB16_297
	.quad	.LBB16_238
	.quad	.LBB16_238
	.quad	.LBB16_238
	.quad	.LBB16_238
	.quad	.LBB16_151
	.quad	.LBB16_151
	.quad	.LBB16_290
	.quad	.LBB16_290
	.quad	.LBB16_290
	.quad	.LBB16_290
	.quad	.LBB16_290
	.quad	.LBB16_290
	.quad	.LBB16_290

	.text
	.globl	build_external_ref
	.align	16, 0x90
	.type	build_external_ref,@function
build_external_ref:                     # @build_external_ref
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp173:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp174:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp175:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp176:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp177:
	.cfi_def_cfa_offset 48
.Ltmp178:
	.cfi_offset %rbx, -40
.Ltmp179:
	.cfi_offset %r12, -32
.Ltmp180:
	.cfi_offset %r14, -24
.Ltmp181:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	lookup_name
	movq	%rax, %r15
	movq	%r14, %rdi
	callq	lookup_objc_ivar
	movq	%rax, %rbx
	testq	%r15, %r15
	je	.LBB17_5
# BB#1:                                 # %land.lhs.true
	movzbl	18(%r15), %eax
	testq	$128, %rax
	je	.LBB17_3
# BB#2:                                 # %if.then
	movq	%r15, %rdi
	callq	warn_deprecated_use
.LBB17_3:                               # %if.end
	movq	global_trees(%rip), %rax
	cmpq	%rax, %r15
	je	.LBB17_5
# BB#4:                                 # %lor.lhs.false4
	movzbl	53(%r15), %eax
	testq	$1, %rax
	je	.LBB17_21
.LBB17_5:                               # %if.then10
	testq	%rbx, %rbx
	jne	.LBB17_26
# BB#6:                                 # %if.else
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	je	.LBB17_14
# BB#7:                                 # %if.then14
	testq	%r15, %r15
	jne	.LBB17_8
# BB#9:                                 # %if.then14
	movabsq	$1, %rax
	jmp	.LBB17_10
.LBB17_21:                              # %if.else47
	testq	%rbx, %rbx
	je	.LBB17_25
# BB#22:                                # %if.else47
	cmpq	%rbx, %r15
	je	.LBB17_25
# BB#23:                                # %land.lhs.true52
	cmpq	$0, 64(%r14)
	je	.LBB17_26
# BB#24:                                # %if.then54
	movq	32(%r14), %rsi
	movabsq	$.L.str34, %rdi
	xorq	%rax, %rax
	callq	warning
	jmp	.LBB17_25
.LBB17_14:                              # %if.else26
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	je	.LBB17_15
# BB#16:                                # %if.else30
	movq	56(%r14), %rcx
	movq	global_trees(%rip), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB17_18
# BB#17:                                # %lor.lhs.false32
	movq	88(%r14), %rcx
	cmpq	%rax, %rcx
	je	.LBB17_20
.LBB17_18:                              # %if.then34
	movq	32(%r14), %rsi
	movabsq	$.L.str31, %rdi
	xorq	%rax, %rax
	callq	error
	movzbl	undeclared_variable_notice(%rip), %eax
	testq	%rax, %rax
	jne	.LBB17_20
# BB#19:                                # %if.then39
	movabsq	$.L.str32, %rdi
	xorq	%rax, %rax
	callq	error
	movabsq	$.L.str33, %rdi
	xorq	%rax, %rax
	callq	error
	movq	$1, %rax
	movb	%al, undeclared_variable_notice(%rip)
.LBB17_20:                              # %if.end41
	movq	global_trees(%rip), %rax
	movq	%rax, 56(%r14)
	movq	current_function_decl(%rip), %rax
	movq	%rax, 88(%r14)
	movq	global_trees(%rip), %rax
	jmp	.LBB17_30
.LBB17_8:
	movabsq	$0, %rax
.LBB17_10:                              # %if.then14
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %r15
	je	.LBB17_12
# BB#11:                                # %if.then14
	testq	%rax, %rax
	jne	.LBB17_12
# BB#13:                                # %if.else20
	movq	%r14, %rdi
	callq	implicit_decl_warning
	movabsq	$-1099511627777, %rax   # imm = 0xFFFFFEFFFFFFFFFF
	andq	%rax, 48(%r15)
.LBB17_25:                              # %if.end61
	movq	%r15, %rbx
	jmp	.LBB17_26
.LBB17_12:                              # %if.then18
	movq	%r14, %rdi
	callq	implicitly_declare
	movq	%rax, %rbx
.LBB17_26:                              # %if.end61
	movq	8(%rbx), %rax
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB17_30
# BB#27:                                # %if.end65
	movq	%rbx, %rdi
	callq	assemble_external
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	orq	$65536, %rcx            # imm = 0x10000
	movl	%ecx, 16(%rbx)
	andq	$255, %rax
	cmpq	$32, %rax
	jne	.LBB17_28
# BB#29:                                # %if.then73
	movq	104(%rbx), %rax
	movl	16(%rax), %ecx
	orq	$512, %rcx              # imm = 0x200
	movl	%ecx, 16(%rax)
	jmp	.LBB17_30
.LBB17_28:
	movq	%rbx, %rax
.LBB17_30:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB17_15:                              # %if.then28
	movq	32(%r14), %rsi
	movabsq	$.L.str30, %rdi
	xorq	%rax, %rax
	callq	error
	movq	global_trees(%rip), %rax
	jmp	.LBB17_30
.Ltmp182:
	.size	build_external_ref, .Ltmp182-build_external_ref
	.cfi_endproc

	.globl	build_function_call
	.align	16, 0x90
	.type	build_function_call,@function
build_function_call:                    # @build_function_call
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
	subq	$64, %rsp
.Ltmp194:
	.cfi_def_cfa_offset 112
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
	movq	global_trees(%rip), %rax
	movq	%rsi, %r15
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB18_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %r12
	movl	16(%r12), %esi
	movq	%rsi, %rcx
	andq	$255, %rcx
	leaq	-114(%rcx), %rdx
	andq	%rbx, %rdx
	cmpq	$2, %rdx
	ja	.LBB18_5
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	32(%r12), %rdi
	cmpq	%rax, %rdi
	je	.LBB18_4
# BB#3:                                 # %land.rhs
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	8(%r12), %rcx
	movq	8(%rdi), %rdx
	cmpq	%rdx, %rcx
	je	.LBB18_1
	jmp	.LBB18_4
.LBB18_5:                               # %while.end
	cmpq	$30, %rcx
	jne	.LBB18_4
# BB#6:                                 # %if.then
	movq	72(%r12), %r14
	cmpq	$0, 120(%r12)
	jne	.LBB18_8
# BB#7:                                 # %cond.false
	movq	%r12, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movl	16(%r12), %esi
.LBB18_8:                               # %cond.end
	movq	8(%r12), %rdi
	andq	%rbx, %rsi
	movq	$12, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	andq	$1, %rax
	movq	$10, %rcx
	shrq	%cl, %rsi
	andq	$2, %rsi
	orq	%rax, %rsi
	callq	build_qualified_type
	movq	%rax, %rdi
	callq	build_pointer_type
	movq	$121, %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	callq	build1
	jmp	.LBB18_9
.LBB18_4:                               # %if.else
	movq	%r12, %rdi
	callq	default_conversion
	xorq	%r12, %r12
	xorq	%r14, %r14
.LBB18_9:                               # %if.end
	movq	8(%rax), %rdx
	movzbl	16(%rdx), %ecx
	cmpq	$13, %rcx
	jne	.LBB18_10
# BB#11:                                # %land.lhs.true62
	movq	8(%rdx), %rdx
	movzbl	16(%rdx), %ecx
	cmpq	$23, %rcx
	jne	.LBB18_12
# BB#13:                                # %if.end71
	movq	%r14, 40(%rsp)          # 8-byte Spill
	testq	%r12, %r12
	je	.LBB18_16
# BB#14:                                # %land.lhs.true73
	movzbl	17(%r12), %ecx
	testq	$8, %rcx
	je	.LBB18_16
# BB#15:                                # %if.then80
	movq	$1, %rcx
	movl	%ecx, current_function_returns_abnormally(%rip)
.LBB18_16:                              # %if.end81
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	24(%rdx), %rcx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	xorq	%rbx, %rbx
	testq	%r15, %r15
	je	.LBB18_17
# BB#18:
	movq	$1, %r14
	movq	%r15, %r13
	movq	%r15, 16(%rsp)          # 8-byte Spill
	jmp	.LBB18_19
	.align	16, 0x90
.LBB18_26:                              # %if.end6.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	32(%r13), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$116, %rax
	jne	.LBB18_28
# BB#27:                                # %if.then8.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	32(%rdi), %rdi
.LBB18_28:                              # %if.end9.i
                                        #   in Loop: Header=BB18_19 Depth=1
	callq	default_function_array_conversion
	movq	global_trees(%rip), %r15
	cmpq	%rax, %r15
	je	.LBB18_33
# BB#29:                                # %if.end9.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	8(%rax), %rsi
	cmpq	%r15, %rsi
	je	.LBB18_33
# BB#30:                                # %if.end.i.i
                                        #   in Loop: Header=BB18_19 Depth=1
	cmpq	$0, 32(%rsi)
	jne	.LBB18_31
# BB#32:                                # %if.end6.i.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	%rax, %rdi
	callq	incomplete_type_error
	movq	global_trees(%rip), %r15
	jmp	.LBB18_33
	.align	16, 0x90
.LBB18_31:                              #   in Loop: Header=BB18_19 Depth=1
	movq	%rax, %r15
.LBB18_33:                              # %require_complete_type.exit.i
                                        #   in Loop: Header=BB18_19 Depth=1
	testq	%rbx, %rbx
	jne	.LBB18_34
# BB#89:                                # %if.else448.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	8(%r15), %rax
	movzbl	16(%rax), %ecx
	cmpq	$7, %rcx
	jne	.LBB18_92
# BB#90:                                # %land.lhs.true456.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	global_trees+200(%rip), %rdi
	movl	60(%rax), %eax
	movl	60(%rdi), %ecx
	andq	$511, %rax              # imm = 0x1FF
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rcx, %rax
	jae	.LBB18_92
# BB#91:                                # %if.then468.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	%r15, %rsi
	callq	convert
	jmp	.LBB18_93
	.align	16, 0x90
.LBB18_34:                              # %if.then12.i
                                        #   in Loop: Header=BB18_19 Depth=1
	cmpq	$0, 32(%rbx)
	jne	.LBB18_36
# BB#35:                                # %if.then15.i
                                        #   in Loop: Header=BB18_19 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str185, %rdi
	movq	%r14, %rsi
	callq	error
	jmp	.LBB18_88
	.align	16, 0x90
.LBB18_92:                              # %if.else471.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	%r15, %rdi
	callq	default_conversion
.LBB18_93:                              # %if.end475.i
                                        #   in Loop: Header=BB18_19 Depth=1
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB18_94
	.align	16, 0x90
.LBB18_36:                              # %if.else16.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movl	warn_conversion(%rip), %eax
	movl	warn_traditional(%rip), %ecx
	orq	%rax, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rcx
	je	.LBB18_83
# BB#37:                                # %if.then19.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movl	16(%rbx), %eax
	movl	60(%rbx), %r9d
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$12, %rcx
	ja	.LBB18_41
# BB#38:                                # %if.then19.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$7232, %rdx             # imm = 0x1C40
	je	.LBB18_41
# BB#39:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	8(%r15), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$7, %rcx
	jne	.LBB18_41
# BB#40:                                # %if.then53.i
                                        #   in Loop: Header=BB18_19 Depth=1
	xorq	%rsi, %rsi
	movabsq	$.L.str186, %rdi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%r14, %rcx
	callq	warn_for_assignment
	movl	60(%rbx), %r9d
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movl	16(%rbx), %eax
.LBB18_41:                              # %if.end55.i
                                        #   in Loop: Header=BB18_19 Depth=1
	andq	$511, %r9               # imm = 0x1FF
	andq	$255, %rax
	leaq	-6(%rax), %rcx
	andq	%rsi, %rcx
	cmpq	$6, %rcx
	ja	.LBB18_60
# BB#42:                                # %if.end55.i
                                        #   in Loop: Header=BB18_19 Depth=1
	jmpq	*.LJTI18_0(,%rcx,8)
.LBB18_44:                              # %land.lhs.true79.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	%rsi, %r8
	movq	8(%r15), %rcx
	movl	16(%rcx), %ecx
	movq	%rcx, %rdx
	andq	$255, %rdx
	cmpq	$8, %rdx
	jne	.LBB18_46
# BB#45:                                # %if.then87.i
                                        #   in Loop: Header=BB18_19 Depth=1
	xorq	%rsi, %rsi
	movabsq	$.L.str187, %rdi
	jmp	.LBB18_82
.LBB18_46:                              # %if.else89.i
                                        #   in Loop: Header=BB18_19 Depth=1
	cmpq	$7, %rax
	movq	%r8, %rsi
	je	.LBB18_53
# BB#47:                                # %if.else89.i
                                        #   in Loop: Header=BB18_19 Depth=1
	cmpq	$8, %rax
	jne	.LBB18_60
	jmp	.LBB18_48
.LBB18_52:                              # %if.end55.i.land.lhs.true111.i_crit_edge
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	8(%r15), %rcx
	movl	16(%rcx), %ecx
.LBB18_53:                              # %land.lhs.true111.i
                                        #   in Loop: Header=BB18_19 Depth=1
	andq	$255, %rcx
	addq	$-6, %rcx
	andq	%rsi, %rcx
	cmpq	$6, %rcx
	ja	.LBB18_60
# BB#54:                                # %land.lhs.true111.i
                                        #   in Loop: Header=BB18_19 Depth=1
	jmpq	*.LJTI18_1(,%rcx,8)
.LBB18_55:                              # %if.then143.i
                                        #   in Loop: Header=BB18_19 Depth=1
	xorq	%rsi, %rsi
	movabsq	$.L.str189, %rdi
	jmp	.LBB18_82
.LBB18_43:                              # %if.end55.i.land.lhs.true95.i_crit_edge
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	8(%r15), %rcx
	movl	16(%rcx), %ecx
.LBB18_48:                              # %land.lhs.true95.i
                                        #   in Loop: Header=BB18_19 Depth=1
	andq	$255, %rcx
	leaq	-10(%rcx), %rdx
	andq	%rsi, %rdx
	cmpq	$3, %rdx
	jb	.LBB18_56
# BB#49:                                # %land.lhs.true95.i
                                        #   in Loop: Header=BB18_19 Depth=1
	cmpq	$6, %rcx
	jne	.LBB18_50
.LBB18_56:                              # %if.then183.i
                                        #   in Loop: Header=BB18_19 Depth=1
	xorq	%rsi, %rsi
	movabsq	$.L.str190, %rdi
.LBB18_82:                              # %if.end405.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%r14, %rcx
	callq	warn_for_assignment
	jmp	.LBB18_83
.LBB18_50:                              # %land.lhs.true95.i
                                        #   in Loop: Header=BB18_19 Depth=1
	cmpq	$7, %rcx
	jne	.LBB18_60
# BB#51:                                # %if.then103.i
                                        #   in Loop: Header=BB18_19 Depth=1
	xorq	%rsi, %rsi
	movabsq	$.L.str188, %rdi
	jmp	.LBB18_82
.LBB18_60:                              # %if.else224.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movl	warn_conversion(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB18_83
# BB#61:                                # %land.lhs.true226.i
                                        #   in Loop: Header=BB18_19 Depth=1
	cmpq	$12, %rax
	ja	.LBB18_83
# BB#62:                                # %land.lhs.true226.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$7232, %rdx             # imm = 0x1C40
	je	.LBB18_83
# BB#63:                                # %land.lhs.true250.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	8(%r15), %rax
	movzbl	16(%rax), %ecx
	cmpq	$12, %rcx
	ja	.LBB18_83
# BB#64:                                # %land.lhs.true250.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$7232, %rax             # imm = 0x1C40
	je	.LBB18_83
# BB#65:                                # %if.then282.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	%r15, %rdi
	movq	%r9, 8(%rsp)            # 8-byte Spill
	callq	default_conversion
	movq	8(%rsp), %rdi           # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %r9
	movl	16(%rbx), %r8d
	movq	8(%rax), %rdx
	movq	%r8, %rax
	andq	$255, %rax
	cmpq	$10, %rax
	jne	.LBB18_67
# BB#66:                                # %land.lhs.true291.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	8(%r15), %rsi
	movq	128(%rbx), %rcx
	movq	128(%rsi), %rsi
	cmpq	%rsi, %rcx
	je	.LBB18_83
.LBB18_67:                              # %if.else299.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movl	60(%rdx), %esi
	andq	$511, %rsi              # imm = 0x1FF
	andq	%r9, %rdi
	cmpq	%rsi, %rdi
	jne	.LBB18_68
# BB#69:                                # %if.else307.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movl	16(%rdx), %edx
	xorq	%r8, %rdx
	testq	$8192, %rdx             # imm = 0x2000
	je	.LBB18_83
# BB#70:                                # %if.else307.i
                                        #   in Loop: Header=BB18_19 Depth=1
	cmpq	$10, %rax
	je	.LBB18_83
# BB#71:                                # %if.else325.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movl	16(%r15), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$25, %rcx
	jne	.LBB18_74
# BB#72:                                # %land.lhs.true331.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	int_fits_type_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB18_83
# BB#73:                                # %land.lhs.true331.if.else335_crit_edge.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movl	16(%r15), %eax
.LBB18_74:                              # %if.else335.i
                                        #   in Loop: Header=BB18_19 Depth=1
	andq	$255, %rax
	cmpq	$115, %rax
	jne	.LBB18_77
# BB#75:                                # %land.lhs.true341.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	32(%r15), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB18_77
# BB#76:                                # %land.lhs.true350.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	%rbx, %rsi
	callq	int_fits_type_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB18_83
.LBB18_77:                              # %if.else357.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	8(%r15), %rax
	movl	60(%rax), %ecx
	movl	60(%rbx), %edx
	andq	$511, %rcx              # imm = 0x1FF
	andq	$511, %rdx              # imm = 0x1FF
	cmpq	%rdx, %rcx
	jae	.LBB18_79
# BB#78:                                # %land.lhs.true369.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movzbl	17(%rax), %eax
	testq	$32, %rax
	jne	.LBB18_83
.LBB18_79:                              # %if.else379.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movzbl	17(%rbx), %eax
	xorq	%rsi, %rsi
	testq	$32, %rax
	jne	.LBB18_80
# BB#81:                                # %if.else388.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movabsq	$.L.str195, %rdi
	jmp	.LBB18_82
.LBB18_68:                              # %if.then305.i
                                        #   in Loop: Header=BB18_19 Depth=1
	xorq	%rsi, %rsi
	movabsq	$.L.str193, %rdi
	jmp	.LBB18_82
.LBB18_58:                              # %if.then215.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	global_trees+192(%rip), %rax
	movl	60(%rax), %eax
	andq	$511, %rax              # imm = 0x1FF
	andq	%rsi, %r9
	cmpq	%rax, %r9
	jne	.LBB18_83
# BB#59:                                # %if.then221.i
                                        #   in Loop: Header=BB18_19 Depth=1
	xorq	%rsi, %rsi
	movabsq	$.L.str192, %rdi
	jmp	.LBB18_82
	.align	16, 0x90
.LBB18_83:                              # %if.end405.i
                                        #   in Loop: Header=BB18_19 Depth=1
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r12, %rcx
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	%r14, %r9
	callq	convert_for_assignment
	movzbl	target_flags+3(%rip), %ecx
	movq	%rax, %r15
	testq	$2, %rcx
	jne	.LBB18_88
# BB#84:                                # %land.lhs.true409.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movzbl	16(%rbx), %ecx
	cmpq	$12, %rcx
	ja	.LBB18_88
# BB#85:                                # %land.lhs.true409.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$7232, %rax             # imm = 0x1C40
	je	.LBB18_88
# BB#86:                                # %land.lhs.true433.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	integer_types+40(%rip), %rax
	movl	60(%rbx), %ecx
	movl	60(%rax), %eax
	andq	$511, %rcx              # imm = 0x1FF
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rax, %rcx
	jae	.LBB18_88
# BB#87:                                # %if.then443.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	%r15, %rdi
	callq	default_conversion
	movq	%rax, %r15
	.align	16, 0x90
.LBB18_88:                              # %if.end446.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	56(%rsp), %rdx          # 8-byte Reload
	xorq	%rdi, %rdi
	movq	%r15, %rsi
.LBB18_94:                              # %if.end475.i
                                        #   in Loop: Header=BB18_19 Depth=1
	callq	tree_cons
	movq	%rax, %rbx
	xorq	%rax, %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	testq	%rcx, %rcx
	je	.LBB18_96
# BB#95:                                # %if.then477.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	(%rcx), %rax
.LBB18_96:                              # %for.inc.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	(%r13), %r13
	incq	%r14
	testq	%r13, %r13
	movq	%rax, %rcx
	jne	.LBB18_19
	jmp	.LBB18_97
.LBB18_57:                              # %if.then199.i
                                        #   in Loop: Header=BB18_19 Depth=1
	xorq	%rsi, %rsi
	movabsq	$.L.str191, %rdi
	jmp	.LBB18_82
.LBB18_80:                              # %if.then386.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movabsq	$.L.str194, %rdi
	jmp	.LBB18_82
	.align	16, 0x90
.LBB18_19:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	movabsq	$0, %rbx
	je	.LBB18_21
# BB#20:                                # %cond.true.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	32(%rcx), %rbx
.LBB18_21:                              # %cond.end.i
                                        #   in Loop: Header=BB18_19 Depth=1
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	global_trees+216(%rip), %rax
	cmpq	%rax, %rbx
	jne	.LBB18_26
# BB#22:                                # %if.then.i
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	je	.LBB18_24
# BB#23:                                # %if.then5.i
	movq	32(%rax), %rsi
	movabsq	$.L.str183, %rdi
	xorq	%rax, %rax
	callq	error
	jmp	.LBB18_25
.LBB18_10:                              # %if.end
	testq	%rcx, %rcx
	je	.LBB18_115
.LBB18_12:                              # %if.then70
	movabsq	$.L.str35, %rdi
	xorq	%rax, %rax
	callq	error
.LBB18_115:                             # %return
	movq	global_trees(%rip), %rax
.LBB18_116:                             # %return
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB18_17:
	movq	%r15, 16(%rsp)          # 8-byte Spill
	movq	%rcx, %rax
	jmp	.LBB18_97
.LBB18_24:                              # %if.else.i
	movabsq	$.L.str184, %rdi
	xorq	%rax, %rax
	callq	error
.LBB18_25:                              # %for.end.i
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	56(%rsp), %rbx          # 8-byte Reload
.LBB18_97:                              # %for.end.i
	testq	%rax, %rax
	movq	32(%rsp), %r14          # 8-byte Reload
	je	.LBB18_102
# BB#98:                                # %land.lhs.true483.i
	movq	32(%rax), %rax
	movq	global_trees+216(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB18_102
# BB#99:                                # %if.then487.i
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	je	.LBB18_101
# BB#100:                               # %if.then489.i
	movq	32(%rax), %rsi
	movabsq	$.L.str196, %rdi
	xorq	%rax, %rax
	callq	error
	jmp	.LBB18_102
.LBB18_101:                             # %if.else493.i
	movabsq	$.L.str197, %rdi
	xorq	%rax, %rax
	callq	error
.LBB18_102:                             # %convert_arguments.exit
	movq	%rbx, %rdi
	callq	nreverse
	movl	warn_format(%rip), %ecx
	movq	%rax, %r15
	testq	%rcx, %rcx
	movq	24(%rsp), %rbx          # 8-byte Reload
	je	.LBB18_104
# BB#103:                               # %if.then87
	movq	48(%rbx), %rsi
	xorq	%rdi, %rdi
	movq	%r15, %rdx
	callq	check_function_format
.LBB18_104:                             # %if.end89
	movzbl	16(%r14), %eax
	cmpq	$121, %rax
	movq	16(%rsp), %rsi          # 8-byte Reload
	jne	.LBB18_108
# BB#105:                               # %land.lhs.true95
	movq	32(%r14), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$30, %rax
	jne	.LBB18_108
# BB#106:                               # %land.lhs.true104
	movzbl	51(%rdi), %eax
	testq	$96, %rax
	je	.LBB18_108
# BB#107:                               # %if.then113
	movq	%r15, %rdx
	callq	expand_tree_builtin
	testq	%rax, %rax
	jne	.LBB18_116
.LBB18_108:                             # %if.end121
	movq	8(%rbx), %rsi
	movq	$53, %rdi
	xorq	%r8, %r8
	xorq	%rax, %rax
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	build
	movl	16(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	movq	8(%rcx), %rsi
	movzbl	16(%rsi), %eax
	cmpq	$5, %rax
	jne	.LBB18_110
# BB#109:
	movq	%rcx, %rax
	jmp	.LBB18_116
.LBB18_110:                             # %if.end137
	movq	global_trees(%rip), %rax
	cmpq	%rcx, %rax
	je	.LBB18_116
# BB#111:                               # %if.end137
	cmpq	%rax, %rsi
	je	.LBB18_116
# BB#112:                               # %if.end.i
	cmpq	$0, 32(%rsi)
	je	.LBB18_114
# BB#113:
	movq	%rcx, %rax
	jmp	.LBB18_116
.LBB18_114:                             # %if.end6.i181
	movq	%rcx, %rdi
	callq	incomplete_type_error
	jmp	.LBB18_115
.Ltmp200:
	.size	build_function_call, .Ltmp200-build_function_call
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI18_0:
	.quad	.LBB18_44
	.quad	.LBB18_52
	.quad	.LBB18_43
	.quad	.LBB18_60
	.quad	.LBB18_44
	.quad	.LBB18_44
	.quad	.LBB18_44
.LJTI18_1:
	.quad	.LBB18_55
	.quad	.LBB18_58
	.quad	.LBB18_57
	.quad	.LBB18_60
	.quad	.LBB18_55
	.quad	.LBB18_55
	.quad	.LBB18_55

	.text
	.globl	parser_build_binary_op
	.align	16, 0x90
	.type	parser_build_binary_op,@function
parser_build_binary_op:                 # @parser_build_binary_op
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
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$1, %rcx
                                        # kill: RDI<def> R14<kill>
                                        # kill: RSI<def> RBX<kill>
                                        # kill: RDX<def> R15<kill>
	callq	build_binary_op
	movq	%rax, %r12
	movzbl	16(%r12), %eax
	testq	%rax, %rax
	je	.LBB19_1
# BB#2:                                 # %if.end
	movzbl	16(%rbx), %eax
	movzbl	16(%r15), %edx
	movzbl	tree_code_type(%rax), %ecx
	movzbl	tree_code_type(%rdx), %eax
	xorq	%rsi, %rsi
	addq	$-49, %rcx
	andq	$255, %rcx
	cmpq	$52, %rcx
	movabsq	$0, %r13
	ja	.LBB19_5
# BB#3:                                 # %if.end
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$4503599627372547, %rcx # imm = 0x10000000000803
	testq	%rcx, %rdx
	movabsq	$0, %r13
	je	.LBB19_5
# BB#4:                                 # %if.then26
	movl	24(%rbx), %r13d
.LBB19_5:                               # %if.end27
	addq	$-49, %rax
	andq	$255, %rax
	cmpq	$52, %rax
	jbe	.LBB19_7
# BB#6:
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	jmp	.LBB19_9
.LBB19_1:                               # %if.then
	movq	global_trees(%rip), %r12
	jmp	.LBB19_59
.LBB19_7:                               # %if.end27
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	movabsq	$4503599627372547, %rax # imm = 0x10000000000803
	testq	%rax, %rdx
	je	.LBB19_9
# BB#8:                                 # %if.then43
	movl	24(%r15), %esi
.LBB19_9:                               # %if.end46
	movq	%r15, (%rsp)            # 8-byte Spill
	movl	warn_parentheses(%rip), %eax
	testq	%rax, %rax
	je	.LBB19_50
# BB#10:                                # %if.then47
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	andq	%r14, %rax
	cmpq	$82, %rax
	jne	.LBB19_15
# BB#11:                                # %if.then53
	leaq	-59(%r13), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$2, %rcx
	jb	.LBB19_14
# BB#12:                                # %if.then53
	andq	%rsi, %rax
	cmpq	$59, %rax
	je	.LBB19_14
# BB#13:                                # %if.then53
	cmpq	$60, %rax
	jne	.LBB19_15
.LBB19_14:                              # %if.then65
	movabsq	$.L.str36, %rdi
	xorq	%rax, %rax
	movq	%rsi, %rbx
	callq	warning
	movq	%rbx, %rsi
.LBB19_15:                              # %if.end67
	leaq	-86(%r14), %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	cmpq	$6, %rax
	ja	.LBB19_50
# BB#16:                                # %if.end67
	jmpq	*.LJTI19_0(,%rax,8)
.LBB19_20:                              # %if.then81
	leaq	-87(%r13), %rax
	andq	%r15, %rax
	cmpq	$2, %rax
	jb	.LBB19_27
# BB#21:                                # %if.then81
	movq	%r13, %rax
	andq	%r15, %rax
	cmpq	$59, %rax
	je	.LBB19_27
# BB#22:                                # %if.then81
	cmpq	$60, %rax
	je	.LBB19_27
# BB#23:                                # %if.then81
	andq	%rsi, %r15
	cmpq	$88, %r15
	je	.LBB19_27
# BB#24:                                # %if.then81
	cmpq	$87, %r15
	je	.LBB19_27
# BB#25:                                # %if.then81
	cmpq	$59, %r15
	je	.LBB19_27
# BB#26:                                # %if.then81
	cmpq	$60, %r15
	jne	.LBB19_28
.LBB19_27:                              # %if.then105
	movabsq	$.L.str38, %rdi
	xorq	%rax, %rax
	movq	%rsi, %rbx
	callq	warning
	movq	%rbx, %rsi
.LBB19_28:                              # %if.end106
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$60, %rax
	je	.LBB19_30
# BB#29:                                # %lor.lhs.false112
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$60, %rax
	jne	.LBB19_50
.LBB19_30:                              # %if.then118
	movabsq	$.L.str39, %rdi
	jmp	.LBB19_49
.LBB19_31:                              # %if.then123
	movq	%r13, %rax
	andq	%r15, %rax
	cmpq	$88, %rax
	je	.LBB19_37
# BB#32:                                # %if.then123
	cmpq	$59, %rax
	je	.LBB19_37
# BB#33:                                # %if.then123
	cmpq	$60, %rax
	je	.LBB19_37
# BB#34:                                # %if.then123
	movq	%rsi, %rax
	andq	%r15, %rax
	cmpq	$88, %rax
	je	.LBB19_37
# BB#35:                                # %if.then123
	cmpq	$59, %rax
	je	.LBB19_37
# BB#36:                                # %if.then123
	cmpq	$60, %rax
	jne	.LBB19_38
.LBB19_37:                              # %if.then141
	movabsq	$.L.str40, %rdi
	xorq	%rax, %rax
	movq	%rsi, %rbx
	callq	warning
	movq	%rbx, %rsi
.LBB19_38:                              # %if.end142
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$60, %rax
	je	.LBB19_40
# BB#39:                                # %lor.lhs.false148
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$60, %rax
	jne	.LBB19_41
.LBB19_40:                              # %if.then154
	movabsq	$.L.str41, %rdi
	xorq	%rax, %rax
	movq	%rsi, %rbx
	callq	warning
	movq	%rbx, %rsi
.LBB19_41:                              # %if.end156
	movq	%r14, %rax
	andq	%r15, %rax
	cmpq	$88, %rax
	jne	.LBB19_50
.LBB19_42:                              # %if.then159
	leaq	-59(%r13), %rax
	andq	%r15, %rax
	cmpq	$2, %rax
	jb	.LBB19_45
# BB#43:                                # %if.then159
	andq	%rsi, %r15
	cmpq	$59, %r15
	je	.LBB19_45
# BB#44:                                # %if.then159
	cmpq	$60, %r15
	jne	.LBB19_46
.LBB19_45:                              # %if.then171
	movabsq	$.L.str42, %rdi
	xorq	%rax, %rax
	movq	%rsi, %rbx
	callq	warning
	movq	%rbx, %rsi
.LBB19_46:                              # %if.end172
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$60, %rax
	je	.LBB19_48
# BB#47:                                # %lor.lhs.false178
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$60, %rax
	jne	.LBB19_50
.LBB19_48:                              # %if.then184
	movabsq	$.L.str43, %rdi
	jmp	.LBB19_49
.LBB19_17:                              # %if.then70
	movq	%r13, %rax
	andq	%r15, %rax
	cmpq	$91, %rax
	je	.LBB19_19
# BB#18:                                # %if.then70
	andq	%rsi, %r15
	cmpq	$91, %r15
	jne	.LBB19_50
.LBB19_19:                              # %if.end78.thread334
	movabsq	$.L.str37, %rdi
.LBB19_49:                              # %if.end187
	xorq	%rax, %rax
	movq	%rsi, %rbx
	callq	warning
	movq	%rbx, %rsi
.LBB19_50:                              # %if.end187
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$60, %rax
	movq	(%rsp), %rbx            # 8-byte Reload
	jne	.LBB19_55
# BB#51:                                # %if.end187
	movl	extra_warnings(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB19_55
# BB#52:                                # %land.lhs.true194
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movzbl	tree_code_type(%r13), %eax
	cmpq	$60, %rax
	je	.LBB19_54
# BB#53:                                # %lor.lhs.false200
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	tree_code_type(%rsi), %eax
	cmpq	$60, %rax
	jne	.LBB19_55
.LBB19_54:                              # %if.then206
	movabsq	$.L.str44, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB19_55:                              # %if.end207
	movq	%r12, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	unsigned_conversion_warning
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	unsigned_conversion_warning
	movq	%r12, %rdi
	callq	overflow_warning
	movl	16(%r12), %ebx
	movq	%rbx, %rax
	andq	$255, %rax
	movzbl	tree_code_type(%rax), %ecx
	addq	$-49, %rcx
	andq	$255, %rcx
	cmpq	$52, %rcx
	ja	.LBB19_58
# BB#56:                                # %if.end207
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$4503599627372547, %rcx # imm = 0x10000000000803
	testq	%rcx, %rax
	je	.LBB19_58
# BB#57:                                # %if.then229
	movl	%r14d, 24(%r12)
	jmp	.LBB19_59
.LBB19_58:                              # %if.else
	movq	8(%r12), %rsi
	andq	$512, %rbx              # imm = 0x200
	movq	$116, %rdi
	movq	%r12, %rdx
	callq	build1
	movq	%rax, %r12
	movl	%r14d, 24(%r12)
	movl	16(%r12), %eax
	movabsq	$4294966783, %rcx       # imm = 0xFFFFFDFF
	andq	%rax, %rcx
	orq	%rbx, %rcx
	movl	%ecx, 16(%r12)
.LBB19_59:                              # %return
	movq	%r12, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp218:
	.size	parser_build_binary_op, .Ltmp218-parser_build_binary_op
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI19_0:
	.quad	.LBB19_20
	.quad	.LBB19_31
	.quad	.LBB19_42
	.quad	.LBB19_50
	.quad	.LBB19_50
	.quad	.LBB19_50
	.quad	.LBB19_17

	.text
	.globl	build_unary_op
	.align	16, 0x90
	.type	build_unary_op,@function
build_unary_op:                         # @build_unary_op
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
	subq	$32, %rsp
.Ltmp230:
	.cfi_def_cfa_offset 80
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
	movq	%rsi, %r12
	movq	8(%r12), %rax
	movl	16(%rax), %ecx
	movq	%rdx, %r15
	movq	%rdi, %r14
	movq	%rcx, %rdx
	andq	$255, %rdx
	je	.LBB20_1
# BB#2:                                 # %if.end
	andq	$254, %rcx
	cmpq	$10, %rcx
	movq	$6, %r13
	je	.LBB20_4
# BB#3:                                 # %if.end
	movq	%rdx, %r13
.LBB20_4:                               # %if.end
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$113, %rsi
	jg	.LBB20_21
# BB#5:                                 # %if.end
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$95, %rax
	jg	.LBB20_14
# BB#6:                                 # %if.end
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%r14, %rax
	andq	%rbx, %rax
	cmpq	$77, %rax
	je	.LBB20_31
# BB#7:                                 # %if.end
	cmpq	$80, %rax
	jne	.LBB20_8
# BB#38:                                # %sw.bb49
	movq	%r13, %rax
	andq	$254, %rax
	cmpq	$6, %rax
	je	.LBB20_41
# BB#39:                                # %sw.bb49
	andq	%rbx, %r13
	cmpq	$8, %r13
	je	.LBB20_41
# BB#40:                                # %if.then55
	movabsq	$.L.str67, %rdi
	jmp	.LBB20_26
.LBB20_1:                               # %if.then
	movq	global_trees(%rip), %rbx
	jmp	.LBB20_256
.LBB20_21:                              # %if.end
	leaq	-114(%r14), %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$18, %rcx
	ja	.LBB20_30
# BB#22:                                # %if.end
	jmpq	*.LJTI20_0(,%rcx,8)
.LBB20_58:                              # %sw.bb140
	movq	%rsi, %r15
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	unary_complex_lvalue
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB20_256
# BB#59:                                # %if.end144
	leaq	-6(%r13), %rax
	movq	%r15, %rcx
	andq	%rcx, %rax
	cmpq	$2, %rax
	jb	.LBB20_84
# BB#60:                                # %if.end144
	movq	%r13, %rax
	andq	%rcx, %rax
	cmpq	$13, %rax
	jne	.LBB20_61
.LBB20_84:                              # %if.end172
	movq	8(%r12), %r15
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	movq	%rcx, %rbx
	callq	get_unwidened
	movq	%rbx, %rsi
	movq	%rax, %r12
	movq	8(%r12), %rdi
	andq	%rsi, %r13
	cmpq	$13, %r13
	jne	.LBB20_109
# BB#85:                                # %if.then179
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	8(%r15), %rax
	cmpq	$0, 32(%rax)
	jne	.LBB20_92
# BB#86:                                # %lor.lhs.false184
	movzbl	16(%rax), %ecx
	cmpq	$5, %rcx
	jne	.LBB20_87
.LBB20_92:                              # %if.else199
	movl	pedantic(%rip), %ecx
	movl	warn_pointer_arith(%rip), %edx
	orq	%rcx, %rdx
	testq	%rsi, %rdx
	je	.LBB20_101
# BB#93:                                # %land.lhs.true203
	movzbl	16(%rax), %eax
	cmpq	$5, %rax
	je	.LBB20_95
# BB#94:                                # %land.lhs.true203
	cmpq	$23, %rax
	jne	.LBB20_101
.LBB20_95:                              # %if.then219
	movq	%r14, %rax
	andq	%rsi, %rax
	movq	%rsi, %rbx
	cmpq	$130, %rax
	je	.LBB20_97
# BB#96:                                # %if.then219
	cmpq	$132, %rax
	jne	.LBB20_98
.LBB20_97:                              # %if.then223
	movabsq	$.L.str71, %rdi
	jmp	.LBB20_99
.LBB20_14:                              # %if.end
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r14, %rcx
	andq	%rax, %rcx
	cmpq	$96, %rcx
	jne	.LBB20_30
# BB#15:                                # %sw.bb75
	movq	%r13, %rcx
	andq	$254, %rcx
	cmpq	$6, %rcx
	je	.LBB20_46
# BB#16:                                # %sw.bb75
	andq	%rax, %r13
	cmpq	$13, %r13
	je	.LBB20_46
# BB#17:                                # %sw.bb75
	cmpq	$8, %r13
	je	.LBB20_46
# BB#18:                                # %sw.bb75
	cmpq	$18, %r13
	je	.LBB20_46
# BB#19:                                # %sw.bb75
	cmpq	$23, %r13
	je	.LBB20_46
# BB#20:                                # %if.then86
	movabsq	$.L.str69, %rdi
	jmp	.LBB20_26
.LBB20_31:                              # %sw.bb16
	movq	%r13, %rax
	andq	$254, %rax
	cmpq	$6, %rax
	je	.LBB20_34
# BB#32:                                # %sw.bb16
	andq	%rbx, %r13
	cmpq	$8, %r13
	je	.LBB20_34
# BB#33:                                # %if.then22
	movabsq	$.L.str64, %rdi
	jmp	.LBB20_26
.LBB20_8:                               # %if.end
	cmpq	$90, %rax
	jne	.LBB20_30
# BB#9:                                 # %sw.bb29
	andq	%rbx, %r13
	cmpq	$6, %r13
	jne	.LBB20_10
# BB#37:                                # %if.else42
	movq	$90, %r14
	jmp	.LBB20_35
.LBB20_46:                              # %if.end87
	movq	%r12, %rdi
	callq	truthvalue_conversion
	movq	%rax, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	invert_truthvalue  # TAILCALL
.LBB20_41:                              # %if.else56
	movq	$80, %r14
	jmp	.LBB20_35
.LBB20_34:                              # %if.else23
	movq	$77, %r14
	jmp	.LBB20_35
.LBB20_23:                              # %sw.bb
	movq	%r13, %rax
	andq	$254, %rax
	cmpq	$6, %rax
	je	.LBB20_27
# BB#24:                                # %sw.bb
	andq	%rsi, %r13
	cmpq	$8, %r13
	je	.LBB20_27
# BB#25:                                # %if.then12
	movabsq	$.L.str63, %rdi
	jmp	.LBB20_26
.LBB20_182:                             # %sw.bb360
	movq	%rsi, %r14
	movzbl	16(%r12), %eax
	cmpq	$43, %rax
	jne	.LBB20_183
# BB#205:                               # %if.then387
	movq	32(%r12), %rdi
	callq	mark_addressable
	testq	%r14, %rax
	je	.LBB20_206
# BB#207:                               # %if.end394
	movq	32(%r12), %rsi
	movq	40(%r12), %rdx
	movq	$59, %rdi
	movq	$1, %rcx
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	build_binary_op  # TAILCALL
.LBB20_42:                              # %sw.bb62
	movq	%r13, %rax
	andq	$254, %rax
	cmpq	$6, %rax
	je	.LBB20_45
# BB#43:                                # %sw.bb62
	andq	%rsi, %r13
	cmpq	$8, %r13
	je	.LBB20_45
# BB#44:                                # %if.then68
	movabsq	$.L.str68, %rdi
	jmp	.LBB20_26
.LBB20_47:                              # %sw.bb90
	movzbl	16(%r12), %ecx
	cmpq	$27, %rcx
	jne	.LBB20_49
# BB#48:                                # %if.then96
	movq	32(%r12), %rbx
	jmp	.LBB20_256
.LBB20_53:                              # %sw.bb113
	movzbl	16(%r12), %ecx
	cmpq	$27, %rcx
	jne	.LBB20_55
# BB#54:                                # %if.then119
	movq	40(%r12), %rbx
	jmp	.LBB20_256
.LBB20_10:                              # %sw.bb29
	cmpq	$8, %r13
	jne	.LBB20_36
# BB#11:                                # %if.then31
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB20_13
# BB#12:                                # %if.then33
	movabsq	$.L.str65, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB20_13:                              # %if.end34
	movq	$126, %r14
.LBB20_35:                              # %if.else23
	testq	%rbx, %r15
	jne	.LBB20_30
	jmp	.LBB20_29
.LBB20_109:                             # %if.else231
	movq	%rsi, %rbx
	movq	global_trees+96(%rip), %rsi
	jmp	.LBB20_110
.LBB20_27:                              # %if.else
	movq	$114, %r14
	jmp	.LBB20_28
.LBB20_183:                             # %sw.bb360
	cmpq	$41, %rax
	jne	.LBB20_208
# BB#184:                               # %if.then366
	addq	$32, %r12
	movabsq	$11115375362048, %rdx   # imm = 0xA1C00000000
	movq	%r12, %rax
	movq	%r14, %rdi
	jmp	.LBB20_185
.LBB20_45:                              # %if.else69
	movq	$126, %r14
.LBB20_28:                              # %if.else
	testq	%rsi, %r15
	jne	.LBB20_30
.LBB20_29:                              # %if.then13
	movq	%r12, %rdi
	callq	default_conversion
	movq	%rax, %r12
.LBB20_30:                              # %if.then524
	movq	8(%r12), %rsi
	movq	%r14, %rdi
.LBB20_52:                              # %if.then105
	movq	%r12, %rdx
	callq	build1
	movq	%rax, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fold  # TAILCALL
.LBB20_49:                              # %if.else97
	andq	%rsi, %rdx
	cmpq	$8, %rdx
	jne	.LBB20_50
# BB#51:                                # %if.then105
	movq	8(%rax), %rsi
	movq	$127, %rdi
	jmp	.LBB20_52
.LBB20_55:                              # %if.else121
	andq	%rsi, %rdx
	cmpq	$8, %rdx
	jne	.LBB20_57
# BB#56:                                # %if.then129
	movq	8(%rax), %rsi
	movq	$128, %rdi
	jmp	.LBB20_52
.LBB20_36:                              # %if.then41
	movabsq	$.L.str66, %rdi
	jmp	.LBB20_26
.LBB20_61:                              # %if.end144
	cmpq	$8, %rax
	jne	.LBB20_80
# BB#62:                                # %if.then146
	movq	%rcx, %r13
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB20_64
# BB#63:                                # %if.then150
	movabsq	$.L.str70, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB20_64:                              # %if.end151
	movq	%r12, %rdi
	callq	stabilize_reference
	movq	%rax, %rbx
	movq	8(%rbx), %rdi
	movl	16(%rdi), %ecx
	movq	%rcx, %rax
	andq	$255, %rax
	je	.LBB20_65
# BB#66:                                # %if.end.i757
	movzbl	16(%rbx), %edx
	cmpq	$27, %rdx
	movq	%r13, %r8
	jne	.LBB20_68
# BB#67:                                # %if.then96.i
	movq	32(%rbx), %r15
	jmp	.LBB20_71
.LBB20_208:                             # %if.end402
	movq	$121, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	unary_complex_lvalue
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB20_256
# BB#209:                               # %if.else406
	movq	%r14, %rbx
	andq	%rbx, %r13
	cmpq	$23, %r13
	je	.LBB20_240
# BB#210:                               # %if.else406
	testq	%rbx, %r15
	jne	.LBB20_240
# BB#211:
	movabsq	$11115375362048, %rdx   # imm = 0xA1C00000000
	movq	%r12, %rax
	jmp	.LBB20_212
.LBB20_50:
	movq	%r12, %rbx
	jmp	.LBB20_256
.LBB20_57:                              # %if.else136
	movq	global_trees+88(%rip), %rsi
	movq	%rax, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	convert  # TAILCALL
.LBB20_206:                             # %if.then393
	movq	global_trees(%rip), %rbx
	jmp	.LBB20_256
.LBB20_80:                              # %if.then165
	andq	%rcx, %r14
	cmpq	$130, %r14
	je	.LBB20_82
# BB#81:                                # %if.then165
	cmpq	$132, %r14
	jne	.LBB20_83
.LBB20_82:                              # %if.then169
	movabsq	$.L.str71, %rdi
	jmp	.LBB20_26
.LBB20_87:                              # %if.then192
	movq	%r14, %rax
	andq	%rsi, %rax
	cmpq	$130, %rax
	je	.LBB20_89
# BB#88:                                # %if.then192
	cmpq	$132, %rax
	jne	.LBB20_91
.LBB20_89:                              # %if.then196
	movabsq	$.L.str73, %rdi
	jmp	.LBB20_90
.LBB20_65:                              # %if.then.i756
	movq	global_trees(%rip), %r15
	movq	%r13, %r8
	jmp	.LBB20_71
.LBB20_68:                              # %if.else97.i
	andq	%r8, %rax
	cmpq	$8, %rax
	jne	.LBB20_69
# BB#70:                                # %if.then105.i
	movq	8(%rdi), %rsi
	movq	$127, %rdi
	movq	%rbx, %rdx
	movq	%r8, %r15
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%r15, %r8
	movq	8(%rbx), %rdi
	movl	16(%rdi), %ecx
	movq	%rax, %r15
	jmp	.LBB20_71
.LBB20_98:                              # %if.else224
	movabsq	$.L.str72, %rdi
.LBB20_99:                              # %if.end227
	xorq	%rax, %rax
	callq	pedwarn
	jmp	.LBB20_100
.LBB20_83:                              # %if.else170
	movabsq	$.L.str72, %rdi
.LBB20_26:                              # %return
	xorq	%rax, %rax
	callq	error
	movq	global_trees(%rip), %rbx
.LBB20_256:                             # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB20_196:                             # %sw.bb.i
                                        #   in Loop: Header=BB20_185 Depth=1
	addq	$32, %rax
.LBB20_185:                             # %tailrecurse.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$169, %rcx
	jg	.LBB20_199
# BB#186:                               # %tailrecurse.i
                                        #   in Loop: Header=BB20_185 Depth=1
	cmpq	$28, %rcx
	jle	.LBB20_187
# BB#190:                               # %tailrecurse.i
                                        #   in Loop: Header=BB20_185 Depth=1
	cmpq	$119, %rcx
	jg	.LBB20_197
# BB#191:                               # %tailrecurse.i
                                        #   in Loop: Header=BB20_185 Depth=1
	cmpq	$51, %rcx
	jg	.LBB20_202
# BB#192:                               # %tailrecurse.i
                                        #   in Loop: Header=BB20_185 Depth=1
	cmpq	$43, %rcx
	ja	.LBB20_200
# BB#193:                               # %tailrecurse.i
                                        #   in Loop: Header=BB20_185 Depth=1
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%rdx, %rsi
	jne	.LBB20_188
# BB#194:                               # %tailrecurse.i
                                        #   in Loop: Header=BB20_185 Depth=1
	cmpq	$29, %rcx
	je	.LBB20_201
# BB#195:                               # %tailrecurse.i
                                        #   in Loop: Header=BB20_185 Depth=1
	cmpq	$39, %rcx
	je	.LBB20_196
	jmp	.LBB20_200
.LBB20_197:                             # %tailrecurse.i
                                        #   in Loop: Header=BB20_185 Depth=1
	leaq	-127(%rcx), %rsi
	andq	%rdi, %rsi
	cmpq	$2, %rsi
	jb	.LBB20_196
# BB#198:                               # %tailrecurse.i
	cmpq	$120, %rcx
	jne	.LBB20_200
	jmp	.LBB20_203
.LBB20_199:                             # %tailrecurse.i
	cmpq	$170, %rcx
	jne	.LBB20_200
.LBB20_201:                             # %lvalue_p.exit.thread773
	movq	(%r12), %rbx
.LBB20_204:                             # %if.then372
	movq	%rbx, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	non_lvalue  # TAILCALL
.LBB20_187:                             # %tailrecurse.i
	testq	%rcx, %rcx
	jne	.LBB20_200
.LBB20_188:                             # %sw.bb3.i
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	movq	(%r12), %rbx
	cmpq	$16, %rax
	je	.LBB20_256
# BB#189:                               # %sw.bb3.i
	cmpq	$23, %rax
	jne	.LBB20_204
	jmp	.LBB20_256
.LBB20_69:
	movq	%rbx, %r15
.LBB20_71:                              # %build_unary_op.exit
	andq	$255, %rcx
	je	.LBB20_72
# BB#73:                                # %if.end.i766
	movzbl	16(%rbx), %eax
	cmpq	$27, %rax
	jne	.LBB20_75
# BB#74:                                # %if.then119.i
	movq	40(%rbx), %r12
	jmp	.LBB20_79
.LBB20_72:                              # %if.then.i764
	movq	global_trees(%rip), %r12
	jmp	.LBB20_79
.LBB20_75:                              # %if.else121.i
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jne	.LBB20_77
# BB#76:                                # %if.then129.i
	movq	8(%rdi), %rsi
	movq	$128, %rdi
	movq	%rbx, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	jmp	.LBB20_78
.LBB20_77:                              # %if.else136.i
	movq	global_trees+88(%rip), %rsi
	callq	convert
.LBB20_78:                              # %build_unary_op.exit768
	movq	%rax, %r12
.LBB20_79:                              # %build_unary_op.exit768
	movq	8(%rbx), %rbx
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	build_unary_op
	movq	%rax, %rcx
	movq	$125, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	movq	%r12, %rcx
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	build  # TAILCALL
.LBB20_91:                              # %if.else197
	movabsq	$.L.str74, %rdi
.LBB20_90:                              # %if.end227
	xorq	%rax, %rax
	movq	%rsi, %rbx
	callq	error
.LBB20_100:                             # %if.end227
	movq	%rbx, %rsi
.LBB20_101:                             # %if.end227
	movq	8(%r15), %rax
	movzbl	16(%rax), %ecx
	cmpq	$23, %rcx
	ja	.LBB20_105
# BB#102:                               # %if.end227
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$8388641, %rdx          # imm = 0x800021
	je	.LBB20_105
# BB#103:                               # %if.then.i751
	movq	%rsi, %rbx
	jmp	.LBB20_104
.LBB20_105:                             # %if.end.i
	movq	%rsi, %rbx
	cmpq	$0, 32(%rax)
	jne	.LBB20_108
# BB#106:                               # %if.end.i
	cmpq	$5, %rcx
	je	.LBB20_108
# BB#107:                               # %if.then13.i
	movabsq	$.L.str14, %rdi
	xorq	%rax, %rax
	callq	error
.LBB20_104:                             # %if.end232
	movq	global_trees+128(%rip), %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB20_110
.LBB20_108:                             # %if.end14.i
	movq	integer_types(%rip), %rcx
	movl	60(%rcx), %edi
	movq	%r15, %r13
	movq	40(%rax), %r15
	movq	$3, %rcx
	shrq	%cl, %rdi
	andq	$63, %rdi
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	$63, %rdi
	movq	%r15, %rsi
	movq	%r13, %r15
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
.LBB20_110:                             # %if.end232
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	%r15, 16(%rsp)          # 8-byte Spill
	callq	convert
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$13194139533343, %r13   # imm = 0xC000000001F
	movq	%rbx, %r9
.LBB20_111:                             # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, %r15
	movl	16(%r15), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	addq	$-72, %rcx
	andq	%r9, %rcx
	cmpq	$43, %rcx
	ja	.LBB20_128
# BB#112:                               # %while.body
                                        #   in Loop: Header=BB20_111 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%r13, %rdx
	je	.LBB20_128
# BB#113:                               # %sw.bb238
                                        #   in Loop: Header=BB20_111 Depth=1
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB20_115
# BB#114:                               # %if.then.i747
                                        #   in Loop: Header=BB20_111 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str179, %rdi
	movq	%r9, %rbx
	callq	pedwarn
	movq	%rbx, %r9
.LBB20_115:                             # %pedantic_lvalue_warning.exit
                                        #   in Loop: Header=BB20_111 Depth=1
	movq	32(%r15), %r12
	movq	8(%r15), %rcx
	movq	8(%r12), %rdx
	movl	16(%rcx), %eax
	movl	16(%rdx), %esi
	xorq	%rax, %rsi
	testq	$255, %rsi
	jne	.LBB20_117
# BB#116:                               # %land.lhs.true252
                                        #   in Loop: Header=BB20_111 Depth=1
	movl	60(%rcx), %ecx
	movl	60(%rdx), %edx
	xorq	%rcx, %rdx
	testq	$65024, %rdx            # imm = 0xFE00
	je	.LBB20_111
.LBB20_117:                             # %if.else273
	movq	%r9, %rbx
	andq	$255, %rax
	cmpq	$11, %rax
	jne	.LBB20_119
# BB#118:                               # %if.then281
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	boolean_increment
	jmp	.LBB20_127
.LBB20_128:                             # %give_up
	movq	%r14, %r12
	andq	%r9, %r12
	cmpq	$130, %r12
	jne	.LBB20_130
# BB#129:
	movabsq	$.L.str75, %rsi
	jmp	.LBB20_133
.LBB20_130:                             # %lor.rhs309
	cmpq	$132, %r12
	je	.LBB20_131
# BB#132:                               # %lor.rhs309
	movabsq	$.L.str76, %rsi
	jmp	.LBB20_133
.LBB20_131:
	movabsq	$.L.str75, %rsi
.LBB20_133:                             # %lor.end311
	movabsq	$11115375362048, %r8    # imm = 0xA1C00000000
	movq	%rax, %rcx
	movq	%r15, %rdx
	jmp	.LBB20_134
.LBB20_151:                             # %sw.bb.i.i726
                                        #   in Loop: Header=BB20_134 Depth=1
	movq	32(%rdx), %rdx
	movl	16(%rdx), %ecx
.LBB20_134:                             # %tailrecurse.i.i723
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rcx
	cmpq	$169, %rcx
	jg	.LBB20_161
# BB#135:                               # %tailrecurse.i.i723
                                        #   in Loop: Header=BB20_134 Depth=1
	cmpq	$28, %rcx
	jle	.LBB20_136
# BB#145:                               # %tailrecurse.i.i723
                                        #   in Loop: Header=BB20_134 Depth=1
	cmpq	$119, %rcx
	jg	.LBB20_152
# BB#146:                               # %tailrecurse.i.i723
                                        #   in Loop: Header=BB20_134 Depth=1
	cmpq	$51, %rcx
	jg	.LBB20_154
# BB#147:                               # %tailrecurse.i.i723
                                        #   in Loop: Header=BB20_134 Depth=1
	cmpq	$43, %rcx
	ja	.LBB20_159
# BB#148:                               # %tailrecurse.i.i723
                                        #   in Loop: Header=BB20_134 Depth=1
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%r8, %rdi
	jne	.LBB20_137
# BB#149:                               # %tailrecurse.i.i723
                                        #   in Loop: Header=BB20_134 Depth=1
	cmpq	$29, %rcx
	je	.LBB20_162
# BB#150:                               # %tailrecurse.i.i723
                                        #   in Loop: Header=BB20_134 Depth=1
	cmpq	$39, %rcx
	je	.LBB20_151
	jmp	.LBB20_159
.LBB20_152:                             # %tailrecurse.i.i723
                                        #   in Loop: Header=BB20_134 Depth=1
	leaq	-127(%rcx), %rdi
	andq	%r9, %rdi
	cmpq	$2, %rdi
	jb	.LBB20_151
# BB#153:                               # %tailrecurse.i.i723
	cmpq	$120, %rcx
	je	.LBB20_155
	jmp	.LBB20_159
.LBB20_161:                             # %tailrecurse.i.i723
	cmpq	$170, %rcx
	jne	.LBB20_159
	jmp	.LBB20_162
.LBB20_136:                             # %tailrecurse.i.i723
	testq	%rcx, %rcx
	jne	.LBB20_159
.LBB20_137:                             # %sw.bb3.i.i735
	movq	8(%rdx), %rcx
	movzbl	16(%rcx), %edi
	cmpq	$16, %rdi
	movabsq	$0, %rdx
	movabsq	$1, %r8
	movq	%rdx, %rcx
	je	.LBB20_139
# BB#138:                               # %sw.bb3.i.i735
	movq	%r8, %rcx
.LBB20_139:                             # %sw.bb3.i.i735
	cmpq	$23, %rdi
	je	.LBB20_141
# BB#140:                               # %sw.bb3.i.i735
	movq	%r8, %rdx
.LBB20_141:                             # %sw.bb3.i.i735
	cmpq	$16, %rdi
	je	.LBB20_159
# BB#142:                               # %sw.bb3.i.i735
	cmpq	$23, %rdi
	je	.LBB20_159
# BB#143:
	andq	%rdx, %rcx
	jmp	.LBB20_144
.LBB20_119:                             # %if.else283
	movq	%r15, %rdi
	callq	stabilize_reference
	movq	%rax, %r12
	leaq	-129(%r14), %rax
	movq	%rbx, %rcx
	andq	%rcx, %rax
	cmpq	$2, %rax
	movq	%r12, %r15
	jb	.LBB20_121
# BB#120:                               # %if.else289
	movq	%r12, %rdi
	movq	%rcx, %r15
	callq	save_expr
	movq	%r15, %rcx
	movq	%rax, %r15
.LBB20_121:                             # %if.end291
	andq	%rcx, %r14
	movq	$59, %rdi
	cmpq	$130, %r14
	je	.LBB20_126
# BB#122:                               # %lor.rhs
	cmpq	$132, %r14
	jne	.LBB20_123
# BB#124:                               # %lor.rhs
	movabsq	$1, %rdi
	jmp	.LBB20_125
.LBB20_154:                             # %tailrecurse.i.i723
	cmpq	$52, %rcx
	jne	.LBB20_159
.LBB20_155:                             # %lvalue_p.exit.i742
	movq	8(%rdx), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$18, %rcx
	jne	.LBB20_156
# BB#157:                               # %lvalue_p.exit.i742
	movabsq	$1, %rcx
	jmp	.LBB20_158
.LBB20_123:
	movabsq	$0, %rdi
.LBB20_125:                             # %lor.rhs
	xorq	$1, %rdi
	addq	$59, %rdi
.LBB20_126:                             # %lor.end
	xorq	%rax, %rax
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	callq	build
	movl	16(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
	movq	$115, %rsi
	movq	%r12, %rdi
	movq	%rax, %rdx
	callq	build_modify_expr
	movq	8(%r12), %rsi
	movq	%rax, %rcx
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rdx
	movq	%r15, %rcx
	callq	build
.LBB20_127:                             # %if.end302
	movq	%rax, %rbx
	movl	16(%rbx), %eax
	orq	$65536, %rax            # imm = 0x10000
	jmp	.LBB20_255
.LBB20_202:                             # %tailrecurse.i
	cmpq	$52, %rcx
	jne	.LBB20_200
.LBB20_203:                             # %lvalue_p.exit
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	movq	(%r12), %rbx
	cmpq	$18, %rax
	jne	.LBB20_256
	jmp	.LBB20_204
.LBB20_200:                             # %lvalue_p.exit.thread
	movq	(%r12), %rbx
	jmp	.LBB20_256
.LBB20_156:
	movabsq	$0, %rcx
.LBB20_158:                             # %lvalue_p.exit.i742
	jne	.LBB20_159
.LBB20_144:                             # %lvalue_or_else.exit745
	testq	%rcx, %rcx
	je	.LBB20_160
.LBB20_162:                             # %if.end316
	testq	$4096, %rax             # imm = 0x1000
	je	.LBB20_177
# BB#163:                               # %if.then322
	cmpq	$130, %r12
	jne	.LBB20_165
# BB#164:
	movabsq	$.L.str77, %rsi
	jmp	.LBB20_168
.LBB20_159:                             # %lvalue_or_else.exit745.thread
	movabsq	$.L.str81, %rdi
	xorq	%rax, %rax
	callq	error
.LBB20_160:                             # %if.then315
	movq	global_trees(%rip), %rbx
	jmp	.LBB20_256
.LBB20_165:                             # %lor.rhs324
	cmpq	$132, %r12
	je	.LBB20_166
# BB#167:                               # %lor.rhs324
	movabsq	$.L.str78, %rsi
	jmp	.LBB20_168
.LBB20_166:
	movabsq	$.L.str77, %rsi
.LBB20_168:                             # %lor.end326
	movq	%r15, %rcx
	jmp	.LBB20_169
.LBB20_172:                             # %if.then.i718.tailrecurse.i714_crit_edge
                                        #   in Loop: Header=BB20_169 Depth=1
	movl	16(%rdx), %eax
	movq	%rdx, %rcx
.LBB20_169:                             # %tailrecurse.i714
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rax
	cmpq	$34, %rax
	je	.LBB20_174
# BB#170:                               # %tailrecurse.i714
                                        #   in Loop: Header=BB20_169 Depth=1
	cmpq	$39, %rax
	jne	.LBB20_176
# BB#171:                               # %if.then.i718
                                        #   in Loop: Header=BB20_169 Depth=1
	movq	32(%rcx), %rdx
	movq	8(%rdx), %rax
	movzbl	17(%rax), %eax
	testq	$16, %rax
	jne	.LBB20_172
# BB#173:                               # %if.else.i
	movq	40(%rcx), %rax
	movq	72(%rax), %rax
	movq	32(%rax), %rdx
	movabsq	$.L.str82, %rdi
	jmp	.LBB20_175
.LBB20_174:                             # %if.then18.i
	movq	72(%rcx), %rax
	movq	32(%rax), %rdx
	movabsq	$.L.str83, %rdi
.LBB20_175:                             # %if.end328
	xorq	%rax, %rax
	callq	pedwarn
	jmp	.LBB20_177
.LBB20_176:                             # %if.else24.i
	movabsq	$.L.str84, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB20_177:                             # %if.end328
	movq	8(%r15), %rsi
	movzbl	16(%rsi), %eax
	cmpq	$11, %rax
	jne	.LBB20_179
# BB#178:                               # %if.then336
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	boolean_increment
	jmp	.LBB20_180
.LBB20_179:                             # %if.else338
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	callq	build
.LBB20_180:                             # %if.end342
	movl	16(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %rbx
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	%r12, %rcx
	je	.LBB20_256
# BB#181:                               # %if.then354
	orq	$262144, %rax           # imm = 0x40000
.LBB20_255:                             # %return
	movl	%eax, 16(%rbx)
	jmp	.LBB20_256
.LBB20_229:                             # %sw.bb.i.i
                                        #   in Loop: Header=BB20_212 Depth=1
	movq	32(%rax), %rax
.LBB20_212:                             # %tailrecurse.i.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rax), %ecx
	cmpq	$169, %rcx
	jg	.LBB20_239
# BB#213:                               # %tailrecurse.i.i
                                        #   in Loop: Header=BB20_212 Depth=1
	cmpq	$28, %rcx
	jle	.LBB20_214
# BB#223:                               # %tailrecurse.i.i
                                        #   in Loop: Header=BB20_212 Depth=1
	cmpq	$119, %rcx
	jg	.LBB20_230
# BB#224:                               # %tailrecurse.i.i
                                        #   in Loop: Header=BB20_212 Depth=1
	cmpq	$51, %rcx
	jg	.LBB20_232
# BB#225:                               # %tailrecurse.i.i
                                        #   in Loop: Header=BB20_212 Depth=1
	cmpq	$43, %rcx
	ja	.LBB20_237
# BB#226:                               # %tailrecurse.i.i
                                        #   in Loop: Header=BB20_212 Depth=1
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%rdx, %rsi
	jne	.LBB20_215
# BB#227:                               # %tailrecurse.i.i
                                        #   in Loop: Header=BB20_212 Depth=1
	cmpq	$29, %rcx
	je	.LBB20_240
# BB#228:                               # %tailrecurse.i.i
                                        #   in Loop: Header=BB20_212 Depth=1
	cmpq	$39, %rcx
	je	.LBB20_229
	jmp	.LBB20_237
.LBB20_230:                             # %tailrecurse.i.i
                                        #   in Loop: Header=BB20_212 Depth=1
	leaq	-127(%rcx), %rsi
	andq	%rbx, %rsi
	cmpq	$2, %rsi
	jb	.LBB20_229
# BB#231:                               # %tailrecurse.i.i
	cmpq	$120, %rcx
	je	.LBB20_233
	jmp	.LBB20_237
.LBB20_239:                             # %tailrecurse.i.i
	cmpq	$170, %rcx
	jne	.LBB20_237
	jmp	.LBB20_240
.LBB20_214:                             # %tailrecurse.i.i
	testq	%rcx, %rcx
	jne	.LBB20_237
.LBB20_215:                             # %sw.bb3.i.i
	movq	8(%rax), %rax
	movzbl	16(%rax), %edx
	cmpq	$16, %rdx
	movabsq	$0, %rcx
	movabsq	$1, %rsi
	movq	%rcx, %rax
	je	.LBB20_217
# BB#216:                               # %sw.bb3.i.i
	movq	%rsi, %rax
.LBB20_217:                             # %sw.bb3.i.i
	cmpq	$23, %rdx
	je	.LBB20_219
# BB#218:                               # %sw.bb3.i.i
	movq	%rsi, %rcx
.LBB20_219:                             # %sw.bb3.i.i
	cmpq	$16, %rdx
	je	.LBB20_237
# BB#220:                               # %sw.bb3.i.i
	cmpq	$23, %rdx
	je	.LBB20_237
# BB#221:
	andq	%rcx, %rax
	jmp	.LBB20_222
.LBB20_232:                             # %tailrecurse.i.i
	cmpq	$52, %rcx
	jne	.LBB20_237
.LBB20_233:                             # %lvalue_p.exit.i
	movq	%rbx, %rcx
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$18, %rax
	jne	.LBB20_234
# BB#235:                               # %lvalue_p.exit.i
	movabsq	$1, %rax
	jmp	.LBB20_236
.LBB20_234:
	movabsq	$0, %rax
.LBB20_236:                             # %lvalue_p.exit.i
	movq	%rcx, %rbx
	jne	.LBB20_237
.LBB20_222:                             # %lvalue_or_else.exit
	testq	%rax, %rax
	je	.LBB20_238
.LBB20_240:                             # %if.end415
	movl	16(%r12), %esi
	movq	%rsi, %rax
	andq	$255, %rax
	movzbl	tree_code_type(%rax), %eax
	movq	8(%r12), %rdi
	cmpq	$100, %rax
	je	.LBB20_242
# BB#241:                               # %if.end415
	cmpq	$114, %rax
	jne	.LBB20_244
.LBB20_242:                             # %land.lhs.true435
	testq	$6144, %rsi             # imm = 0x1800
	je	.LBB20_244
# BB#243:                               # %if.then448
	movq	$12, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	andq	$1, %rax
	movq	$10, %rcx
	shrq	%cl, %rsi
	andq	$2, %rsi
	orq	%rax, %rsi
	callq	c_build_qualified_type
	movq	%rax, %rdi
.LBB20_244:                             # %if.end464
	callq	build_pointer_type
	movq	%rax, %r13
	movq	%r12, %rdi
	callq	mark_addressable
	testq	%rbx, %rax
	je	.LBB20_245
# BB#246:                               # %if.end470
	movzbl	16(%r12), %eax
	cmpq	$39, %rax
	jne	.LBB20_250
# BB#247:                               # %if.then477
	movq	32(%r12), %rsi
	movq	40(%r12), %r14
	movq	$121, %rdi
	movq	%r15, %rdx
	callq	build_unary_op
	movzbl	53(%r14), %ecx
	testq	$2, %rcx
	je	.LBB20_249
# BB#248:                               # %if.then490
	movq	72(%r14), %rax
	movq	32(%rax), %rsi
	movabsq	$.L.str80, %rdi
	xorq	%rax, %rax
	callq	error
	movq	global_trees(%rip), %rbx
	jmp	.LBB20_256
.LBB20_237:                             # %lvalue_or_else.exit.thread
	movabsq	$.L.str81, %rdi
	movabsq	$.L.str79, %rsi
	xorq	%rax, %rax
	callq	error
.LBB20_238:                             # %if.then413
	movq	global_trees(%rip), %rbx
	jmp	.LBB20_256
.LBB20_245:                             # %if.then469
	movq	global_trees(%rip), %rbx
	jmp	.LBB20_256
.LBB20_250:                             # %if.else498
	movq	$121, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	callq	build1
	jmp	.LBB20_251
.LBB20_249:                             # %if.end492
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %r15
	movq	%r14, %rdi
	callq	byte_position
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %rcx
	movq	$59, %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
.LBB20_251:                             # %if.end500
	movq	%rax, %rbx
	movq	%r12, %rdi
	callq	staticp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB20_256
# BB#252:                               # %land.lhs.true503
	movzbl	16(%r12), %eax
	cmpq	$30, %rax
	jne	.LBB20_254
# BB#253:                               # %land.lhs.true510
	cmpq	$0, 80(%r12)
	jne	.LBB20_256
.LBB20_254:                             # %if.then514
	movl	16(%rbx), %eax
	orq	$512, %rax              # imm = 0x200
	jmp	.LBB20_255
.Ltmp236:
	.size	build_unary_op, .Ltmp236-build_unary_op
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI20_0:
	.quad	.LBB20_23
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_182
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_42
	.quad	.LBB20_47
	.quad	.LBB20_53
	.quad	.LBB20_58
	.quad	.LBB20_58
	.quad	.LBB20_58
	.quad	.LBB20_58

	.text
	.align	16, 0x90
	.type	unary_complex_lvalue,@function
unary_complex_lvalue:                   # @unary_complex_lvalue
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp242:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp243:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp244:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp245:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp246:
	.cfi_def_cfa_offset 48
.Ltmp247:
	.cfi_offset %rbx, -40
.Ltmp248:
	.cfi_offset %r12, -32
.Ltmp249:
	.cfi_offset %r14, -24
.Ltmp250:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movzbl	16(%rbx), %eax
	movq	%rdx, %r14
	movq	%rdi, %r15
	cmpq	$51, %rax
	jne	.LBB21_1
# BB#7:                                 # %if.then21
	movl	pedantic(%rip), %eax
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %r14
	jne	.LBB21_10
# BB#8:                                 # %if.then21
	testq	%r12, %rax
	je	.LBB21_10
# BB#9:                                 # %if.then.i66
	movabsq	$.L.str177, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	movl	pedantic(%rip), %eax
.LBB21_10:                              # %if.end24
	movq	8(%rbx), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$23, %rcx
	je	.LBB21_14
# BB#11:                                # %if.end24
	testq	%r12, %r14
	jne	.LBB21_14
# BB#12:                                # %if.end24
	testq	%r12, %rax
	je	.LBB21_14
# BB#13:                                # %if.then.i
	movabsq	$.L.str178, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB21_14:                              # %if.end35
	movq	32(%rbx), %r12
	movq	40(%rbx), %rsi
	movq	%r15, %rdi
	movq	%r14, %rdx
	callq	build_unary_op
	movq	48(%rbx), %rsi
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%r14, %rdx
	callq	build_unary_op
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	build_conditional_expr  # TAILCALL
.LBB21_1:                               # %entry
	cmpq	$47, %rax
	jne	.LBB21_15
# BB#2:                                 # %if.then
	movq	40(%rbx), %rsi
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	callq	build_unary_op
	movq	8(%rbx), %rcx
	movzbl	16(%rcx), %ecx
	movq	%rax, %r15
	cmpq	$23, %rcx
	je	.LBB21_6
# BB#3:                                 # %if.then
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	jne	.LBB21_6
# BB#4:                                 # %if.then
	movl	pedantic(%rip), %ecx
	testq	%rax, %rcx
	je	.LBB21_6
# BB#5:                                 # %if.then.i69
	movabsq	$.L.str178, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB21_6:                               # %if.end
	movq	8(%r15), %rsi
	movq	32(%rbx), %rdx
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r15, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	build  # TAILCALL
.LBB21_15:                              # %return
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp251:
	.size	unary_complex_lvalue, .Ltmp251-unary_complex_lvalue
	.cfi_endproc

	.globl	build_modify_expr
	.align	16, 0x90
	.type	build_modify_expr,@function
build_modify_expr:                      # @build_modify_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp258:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp259:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp260:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp261:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp262:
	.cfi_def_cfa_offset 48
.Ltmp263:
	.cfi_offset %rbx, -48
.Ltmp264:
	.cfi_offset %r12, -40
.Ltmp265:
	.cfi_offset %r13, -32
.Ltmp266:
	.cfi_offset %r14, -24
.Ltmp267:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movq	global_trees(%rip), %rbx
	movq	8(%r13), %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	cmpq	%r13, %rbx
	je	.LBB22_4
# BB#1:                                 # %entry
	cmpq	%rbx, %r14
	je	.LBB22_4
# BB#2:                                 # %if.end.i
	cmpq	$0, 32(%r14)
	jne	.LBB22_5
# BB#3:                                 # %if.end6.i
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	incomplete_type_error
	movq	global_trees(%rip), %rbx
.LBB22_4:                               # %require_complete_type.exit
	movq	%rbx, %r13
.LBB22_5:                               # %require_complete_type.exit
	movl	16(%r13), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	je	.LBB22_79
# BB#6:                                 # %lor.lhs.false
	movzbl	16(%r15), %ecx
	testq	%rcx, %rcx
	je	.LBB22_79
# BB#7:                                 # %lor.lhs.false
	cmpq	$116, %rcx
	jne	.LBB22_9
# BB#8:                                 # %if.then12
	movq	32(%r15), %r15
.LBB22_9:                               # %if.end13
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	cmpq	$51, %rax
	jne	.LBB22_10
# BB#17:                                # %sw.bb33
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB22_19
# BB#18:                                # %if.then.i250
	movabsq	$.L.str177, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB22_19:                              # %pedantic_lvalue_warning.exit252
	movq	%r15, %rdi
	callq	save_expr
	movq	32(%r13), %r15
	movq	40(%r13), %rdi
	movq	%rax, %r14
	movq	%r12, %rsi
	movq	%r14, %rdx
	callq	build_modify_expr
	movq	48(%r13), %rdi
	movq	%rax, %rbx
	movq	%r12, %rsi
	movq	%r14, %rdx
	callq	build_modify_expr
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_conditional_expr
	movq	%rax, %rbx
	movzbl	16(%rbx), %eax
	testq	%rax, %rax
	je	.LBB22_79
# BB#20:                                # %if.end53
	movq	8(%r13), %r15
	movq	global_trees+216(%rip), %rdi
	movq	%r14, %rsi
	callq	convert
	movq	%rax, %rcx
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	jmp	.LBB22_16
.LBB22_10:                              # %if.end13
	cmpq	$47, %rax
	jne	.LBB22_21
# BB#11:                                # %sw.bb
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB22_13
# BB#12:                                # %if.then.i
	movabsq	$.L.str178, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB22_13:                              # %pedantic_lvalue_warning.exit
	movq	40(%r13), %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	build_modify_expr
	movq	%rax, %rcx
	movzbl	16(%rcx), %eax
	testq	%rax, %rax
	je	.LBB22_14
# BB#15:                                # %if.end28
	movq	32(%r13), %rdx
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
.LBB22_16:                              # %if.end28
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	build  # TAILCALL
.LBB22_21:                              # %sw.epilog
	movq	%r12, %rax
	andq	%rbx, %rax
	cmpq	$115, %rax
	je	.LBB22_23
# BB#22:                                # %if.then59
	movq	%r13, %rdi
	callq	stabilize_reference
	movq	%rax, %r13
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	build_binary_op
	movl	16(%r13), %edx
	movq	%rax, %r15
.LBB22_23:                              # %if.end62
	movq	%rdx, %rax
	andq	$255, %rax
	addq	$-72, %rax
	andq	%rbx, %rax
	cmpq	$43, %rax
	movq	%rdx, %rcx
	movq	%r13, %rsi
	ja	.LBB22_29
# BB#24:                                # %if.end62
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	movabsq	$13194139533343, %rax   # imm = 0xC000000001F
	testq	%rax, %rsi
	movq	%rdx, %rcx
	movq	%r13, %rsi
	je	.LBB22_29
# BB#25:                                # %sw.bb67
	movq	%r15, %rdi
	callq	default_function_array_conversion
	movq	32(%r13), %rbx
	movq	8(%rbx), %r15
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	$115, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	build_modify_expr
	movq	%rax, %rbx
	movzbl	16(%rbx), %eax
	testq	%rax, %rax
	je	.LBB22_79
# BB#26:                                # %if.end84
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB22_28
# BB#27:                                # %if.then.i254
	movabsq	$.L.str179, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB22_28:                              # %pedantic_lvalue_warning.exit256
	movq	8(%r13), %rdi
	movq	%rbx, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	convert  # TAILCALL
.LBB22_14:                              # %if.then27
	movq	global_trees(%rip), %rbx
	jmp	.LBB22_79
.LBB22_46:                              # %sw.bb.i.i
                                        #   in Loop: Header=BB22_29 Depth=1
	movq	32(%rsi), %rsi
	movl	16(%rsi), %ecx
.LBB22_29:                              # %tailrecurse.i.i
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rcx
	cmpq	$169, %rcx
	jg	.LBB22_56
# BB#30:                                # %tailrecurse.i.i
                                        #   in Loop: Header=BB22_29 Depth=1
	cmpq	$28, %rcx
	jle	.LBB22_31
# BB#40:                                # %tailrecurse.i.i
                                        #   in Loop: Header=BB22_29 Depth=1
	cmpq	$119, %rcx
	jg	.LBB22_47
# BB#41:                                # %tailrecurse.i.i
                                        #   in Loop: Header=BB22_29 Depth=1
	cmpq	$51, %rcx
	jg	.LBB22_49
# BB#42:                                # %tailrecurse.i.i
                                        #   in Loop: Header=BB22_29 Depth=1
	cmpq	$43, %rcx
	ja	.LBB22_54
# BB#43:                                # %tailrecurse.i.i
                                        #   in Loop: Header=BB22_29 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$11115375362048, %rdi   # imm = 0xA1C00000000
	testq	%rdi, %rax
	jne	.LBB22_32
# BB#44:                                # %tailrecurse.i.i
                                        #   in Loop: Header=BB22_29 Depth=1
	cmpq	$29, %rcx
	je	.LBB22_57
# BB#45:                                # %tailrecurse.i.i
                                        #   in Loop: Header=BB22_29 Depth=1
	cmpq	$39, %rcx
	je	.LBB22_46
	jmp	.LBB22_54
.LBB22_47:                              # %tailrecurse.i.i
                                        #   in Loop: Header=BB22_29 Depth=1
	leaq	-127(%rcx), %rax
	andq	%rbx, %rax
	cmpq	$2, %rax
	jb	.LBB22_46
# BB#48:                                # %tailrecurse.i.i
	cmpq	$120, %rcx
	je	.LBB22_50
	jmp	.LBB22_54
.LBB22_56:                              # %tailrecurse.i.i
	cmpq	$170, %rcx
	jne	.LBB22_54
	jmp	.LBB22_57
.LBB22_31:                              # %tailrecurse.i.i
	testq	%rcx, %rcx
	jne	.LBB22_54
.LBB22_32:                              # %sw.bb3.i.i
	movq	8(%rsi), %rax
	movzbl	16(%rax), %esi
	cmpq	$16, %rsi
	movabsq	$0, %rcx
	movabsq	$1, %rdi
	movq	%rcx, %rax
	je	.LBB22_34
# BB#33:                                # %sw.bb3.i.i
	movq	%rdi, %rax
.LBB22_34:                              # %sw.bb3.i.i
	cmpq	$23, %rsi
	je	.LBB22_36
# BB#35:                                # %sw.bb3.i.i
	movq	%rdi, %rcx
.LBB22_36:                              # %sw.bb3.i.i
	cmpq	$16, %rsi
	je	.LBB22_54
# BB#37:                                # %sw.bb3.i.i
	cmpq	$23, %rsi
	je	.LBB22_54
# BB#38:
	andq	%rcx, %rax
	jmp	.LBB22_39
.LBB22_49:                              # %tailrecurse.i.i
	cmpq	$52, %rcx
	jne	.LBB22_54
.LBB22_50:                              # %lvalue_p.exit.i
	movq	8(%rsi), %rax
	movzbl	16(%rax), %eax
	cmpq	$18, %rax
	jne	.LBB22_51
# BB#52:                                # %lvalue_p.exit.i
	movabsq	$1, %rax
	jmp	.LBB22_53
.LBB22_51:
	movabsq	$0, %rax
.LBB22_53:                              # %lvalue_p.exit.i
	jne	.LBB22_54
.LBB22_39:                              # %lvalue_or_else.exit
	testq	%rax, %rax
	je	.LBB22_55
.LBB22_57:                              # %if.end92
	testq	$4096, %rdx             # imm = 0x1000
	je	.LBB22_59
# BB#58:
	movq	%r13, %rax
	jmp	.LBB22_62
.LBB22_54:                              # %lvalue_or_else.exit.thread
	movabsq	$.L.str81, %rdi
	movabsq	$.L.str108, %rsi
	xorq	%rax, %rax
	callq	error
.LBB22_55:                              # %if.then91
	movq	global_trees(%rip), %rbx
.LBB22_79:                              # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB22_59:                              # %lor.lhs.false97
	movl	16(%r14), %eax
	testq	$4096, %rax             # imm = 0x1000
	jne	.LBB22_60
# BB#61:                                # %lor.lhs.false97
	andq	$33554686, %rax         # imm = 0x20000FE
	andq	%rbx, %rax
	cmpq	$33554452, %rax         # imm = 0x2000014
	movq	%r13, %rax
	jne	.LBB22_70
	jmp	.LBB22_62
.LBB22_60:
	movq	%r13, %rax
	jmp	.LBB22_62
.LBB22_65:                              # %if.then.i260.tailrecurse.i_crit_edge
                                        #   in Loop: Header=BB22_62 Depth=1
	movl	16(%rcx), %edx
	movq	%rcx, %rax
.LBB22_62:                              # %tailrecurse.i
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rdx
	cmpq	$34, %rdx
	je	.LBB22_67
# BB#63:                                # %tailrecurse.i
                                        #   in Loop: Header=BB22_62 Depth=1
	cmpq	$39, %rdx
	jne	.LBB22_69
# BB#64:                                # %if.then.i260
                                        #   in Loop: Header=BB22_62 Depth=1
	movq	32(%rax), %rcx
	movq	8(%rcx), %rdx
	movzbl	17(%rdx), %edx
	testq	$16, %rdx
	jne	.LBB22_65
# BB#66:                                # %if.else.i
	movq	40(%rax), %rax
	movq	72(%rax), %rax
	movq	32(%rax), %rdx
	movabsq	$.L.str82, %rdi
	jmp	.LBB22_68
.LBB22_67:                              # %if.then18.i
	movq	72(%rax), %rax
	movq	32(%rax), %rdx
	movabsq	$.L.str83, %rdi
.LBB22_68:                              # %if.end122
	movabsq	$.L.str109, %rsi
	xorq	%rax, %rax
	callq	pedwarn
	jmp	.LBB22_70
.LBB22_69:                              # %if.else24.i
	movabsq	$.L.str84, %rdi
	movabsq	$.L.str109, %rsi
	xorq	%rax, %rax
	callq	pedwarn
.LBB22_70:                              # %if.end122
	movzbl	16(%r13), %eax
	cmpq	$39, %rax
	movq	%r14, %rbx
	jne	.LBB22_74
# BB#71:                                # %land.lhs.true128
	movzbl	16(%r14), %ecx
	cmpq	$11, %rcx
	movq	%r14, %rbx
	ja	.LBB22_74
# BB#72:                                # %land.lhs.true128
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$3264, %rax             # imm = 0xCC0
	movq	%r14, %rbx
	je	.LBB22_74
# BB#73:                                # %if.then152
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	get_unwidened
	movq	8(%rax), %rbx
.LBB22_74:                              # %if.end156
	movq	8(%r13), %rax
	cmpq	%rax, %rbx
	je	.LBB22_76
# BB#75:                                # %if.then160
	movq	%r13, %rdi
	callq	copy_node
	movq	%rax, %r13
	movq	%rbx, 8(%r13)
.LBB22_76:                              # %if.end164
	movabsq	$.L.str109, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	convert_for_assignment
	movq	%rax, %rcx
	movzbl	16(%rcx), %eax
	testq	%rax, %rax
	je	.LBB22_77
# BB#78:                                # %if.end172
	movq	$48, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	build
	movq	%rax, %rbx
	movl	16(%rbx), %eax
	orq	$256, %rax              # imm = 0x100
	movl	%eax, 16(%rbx)
	movq	8(%rbx), %rax
	cmpq	%rax, %r14
	je	.LBB22_79
# BB#80:                                # %if.end181
	movabsq	$.L.str109, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%r14, %rdi
	movq	%rbx, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	convert_for_assignment  # TAILCALL
.LBB22_77:                              # %if.then171
	movq	global_trees(%rip), %rbx
	jmp	.LBB22_79
.Ltmp268:
	.size	build_modify_expr, .Ltmp268-build_modify_expr
	.cfi_endproc

	.globl	lvalue_or_else
	.align	16, 0x90
	.type	lvalue_or_else,@function
lvalue_or_else:                         # @lvalue_or_else
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp271:
	.cfi_def_cfa_offset 16
.Ltmp272:
	.cfi_offset %rbx, -16
	movq	$1, %rbx
	movabsq	$11115375362048, %r8    # imm = 0xA1C00000000
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	jmp	.LBB23_1
.LBB23_17:                              # %sw.bb.i
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	32(%rdi), %rdi
	.align	16, 0x90
.LBB23_1:                               # %tailrecurse.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rdi), %ecx
	cmpq	$169, %rcx
	jg	.LBB23_20
# BB#2:                                 # %tailrecurse.i
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$28, %rcx
	jle	.LBB23_3
# BB#11:                                # %tailrecurse.i
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$119, %rcx
	jg	.LBB23_18
# BB#12:                                # %tailrecurse.i
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$51, %rcx
	jg	.LBB23_21
# BB#13:                                # %tailrecurse.i
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$43, %rcx
	ja	.LBB23_26
# BB#14:                                # %tailrecurse.i
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%r8, %rax
	jne	.LBB23_4
# BB#15:                                # %tailrecurse.i
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$29, %rcx
	je	.LBB23_27
# BB#16:                                # %tailrecurse.i
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$39, %rcx
	je	.LBB23_17
	jmp	.LBB23_26
.LBB23_18:                              # %tailrecurse.i
                                        #   in Loop: Header=BB23_1 Depth=1
	leaq	-127(%rcx), %rax
	andq	%rdx, %rax
	cmpq	$2, %rax
	jb	.LBB23_17
# BB#19:                                # %tailrecurse.i
	cmpq	$120, %rcx
	je	.LBB23_22
	jmp	.LBB23_26
.LBB23_20:                              # %tailrecurse.i
	cmpq	$170, %rcx
	jne	.LBB23_26
	jmp	.LBB23_27
.LBB23_3:                               # %tailrecurse.i
	testq	%rcx, %rcx
	jne	.LBB23_26
.LBB23_4:                               # %sw.bb3.i
	movq	8(%rdi), %rax
	movzbl	16(%rax), %ecx
	cmpq	$16, %rcx
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rbx
	je	.LBB23_6
# BB#5:                                 # %sw.bb3.i
	movq	%rdx, %rbx
.LBB23_6:                               # %sw.bb3.i
	cmpq	$23, %rcx
	je	.LBB23_8
# BB#7:                                 # %sw.bb3.i
	movq	%rdx, %rax
.LBB23_8:                               # %sw.bb3.i
	cmpq	$16, %rcx
	je	.LBB23_26
# BB#9:                                 # %sw.bb3.i
	cmpq	$23, %rcx
	je	.LBB23_26
# BB#10:
	andq	%rax, %rbx
	jmp	.LBB23_27
.LBB23_21:                              # %tailrecurse.i
	cmpq	$52, %rcx
	jne	.LBB23_26
.LBB23_22:                              # %lvalue_p.exit
	movq	8(%rdi), %rax
	movzbl	16(%rax), %eax
	cmpq	$18, %rax
	jne	.LBB23_23
# BB#24:                                # %lvalue_p.exit
	movabsq	$1, %rbx
	jmp	.LBB23_25
.LBB23_23:
	movabsq	$0, %rbx
.LBB23_25:                              # %lvalue_p.exit
	je	.LBB23_27
.LBB23_26:                              # %if.then
	movabsq	$.L.str81, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	error
.LBB23_27:                              # %if.end
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp273:
	.size	lvalue_or_else, .Ltmp273-lvalue_or_else
	.cfi_endproc

	.globl	readonly_warning
	.align	16, 0x90
	.type	readonly_warning,@function
readonly_warning:                       # @readonly_warning
	.cfi_startproc
# BB#0:                                 # %entry
	.align	16, 0x90
.LBB24_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rax
	movzbl	16(%rax), %ecx
	cmpq	$34, %rcx
	je	.LBB24_5
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB24_1 Depth=1
	cmpq	$39, %rcx
	jne	.LBB24_6
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB24_1 Depth=1
	movq	32(%rax), %rdi
	movq	8(%rdi), %rcx
	movzbl	17(%rcx), %ecx
	testq	$16, %rcx
	jne	.LBB24_1
# BB#4:                                 # %if.else
	movq	40(%rax), %rax
	movq	72(%rax), %rax
	movq	32(%rax), %rdx
	movabsq	$.L.str82, %rdi
	xorq	%rax, %rax
	jmp	pedwarn  # TAILCALL
.LBB24_5:                               # %if.then18
	movq	72(%rax), %rax
	movq	32(%rax), %rdx
	movabsq	$.L.str83, %rdi
	xorq	%rax, %rax
	jmp	pedwarn  # TAILCALL
.LBB24_6:                               # %if.else24
	movabsq	$.L.str84, %rdi
	xorq	%rax, %rax
	jmp	pedwarn  # TAILCALL
.Ltmp274:
	.size	readonly_warning, .Ltmp274-readonly_warning
	.cfi_endproc

	.globl	build_conditional_expr
	.align	16, 0x90
	.type	build_conditional_expr,@function
build_conditional_expr:                 # @build_conditional_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp281:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp282:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp283:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp284:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp285:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp286:
	.cfi_def_cfa_offset 80
.Ltmp287:
	.cfi_offset %rbx, -48
.Ltmp288:
	.cfi_offset %r12, -40
.Ltmp289:
	.cfi_offset %r13, -32
.Ltmp290:
	.cfi_offset %r14, -24
.Ltmp291:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	callq	default_conversion
	movq	%rax, %rdi
	callq	truthvalue_conversion
	movq	8(%r15), %rcx
	movzbl	16(%rcx), %ecx
	movq	%rax, %r13
	cmpq	$5, %rcx
	movq	%r15, %r8
	je	.LBB25_2
# BB#1:                                 # %if.then
	movq	%r15, %rdi
	callq	default_conversion
	movq	%rax, %r8
.LBB25_2:                               # %if.end
	movq	8(%r14), %rax
	movzbl	16(%rax), %eax
	cmpq	$5, %rax
	movq	%r14, %rax
	je	.LBB25_4
# BB#3:                                 # %if.then11
	movq	%r14, %rdi
	movq	%r8, %rbx
	callq	default_conversion
	movq	%rbx, %r8
.LBB25_4:                               # %if.end13
	movzbl	16(%r13), %ecx
	testq	%rcx, %rcx
	je	.LBB25_124
# BB#5:                                 # %lor.lhs.false
	movq	8(%r8), %r12
	movl	16(%r12), %ecx
	movq	%rcx, %rdi
	andq	$255, %rdi
	je	.LBB25_124
# BB#6:                                 # %lor.lhs.false26
	movq	8(%rax), %rsi
	movl	16(%rsi), %edx
	movq	%rdx, %r10
	andq	$255, %r10
	je	.LBB25_124
# BB#7:                                 # %if.end35
	movq	%r13, 16(%rsp)          # 8-byte Spill
	movq	128(%r12), %r9
	movq	128(%rsi), %rbx
	cmpq	%rbx, %r9
	je	.LBB25_8
# BB#10:                                # %if.else58
	movq	%rax, 24(%rsp)          # 8-byte Spill
	andq	$254, %rcx
	cmpq	$6, %rcx
	je	.LBB25_12
# BB#11:                                # %if.else58
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rdi, %rax
	cmpq	$8, %rax
	jne	.LBB25_27
.LBB25_12:                              # %land.lhs.true
	andq	$254, %rdx
	cmpq	$6, %rdx
	je	.LBB25_14
# BB#13:                                # %land.lhs.true
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%r10, %rdx
	cmpq	$8, %rdx
	je	.LBB25_14
.LBB25_27:                              # %if.else113
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rdi
	cmpq	$5, %rdi
	je	.LBB25_29
# BB#28:                                # %if.else113
	movq	%r10, %rdx
	andq	%rbx, %rdx
	cmpq	$5, %rdx
	je	.LBB25_29
# BB#34:                                # %if.else125
	movq	%r8, 8(%rsp)            # 8-byte Spill
	cmpq	$13, %rdi
	jne	.LBB25_97
# BB#35:                                # %if.else125
	cmpq	$13, %rdx
	jne	.LBB25_97
# BB#36:                                # %if.then129
	movq	$1, %rdx
	movq	%r12, %rdi
	movq	%rsi, %r13
	callq	maybe_objc_comptypes
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jns	.LBB25_40
# BB#37:                                # %if.end.i
	movq	8(%r12), %rax
	movq	8(%r13), %rcx
	movq	128(%rax), %rdi
	movq	128(%rcx), %rsi
	callq	comptypes
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	$2, %rcx
	jne	.LBB25_40
# BB#38:                                # %if.end.i
	movl	pedantic(%rip), %ecx
	testq	%rbx, %rcx
	je	.LBB25_40
# BB#39:                                # %comp_target_types.exit.thread
	movabsq	$.L.str182, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	jmp	.LBB25_41
.LBB25_8:                               # %if.then52
	movq	%rax, 24(%rsp)          # 8-byte Spill
	cmpq	%rsi, %r12
	je	.LBB25_107
# BB#9:                                 # %if.then52
	movq	%r9, %r12
	jmp	.LBB25_107
.LBB25_29:                              # %if.then117
	movl	pedantic(%rip), %edx
	testq	%rdx, %rdx
	je	.LBB25_33
# BB#30:                                # %land.lhs.true119
	cmpq	$5, %rdi
	jne	.LBB25_32
# BB#31:                                # %land.lhs.true119
	andq	%rbx, %r10
	cmpq	$5, %r10
	je	.LBB25_33
.LBB25_32:                              # %if.then123
	movabsq	$.L.str92, %rdi
	xorq	%rax, %rax
	movq	%r8, %rbx
	callq	pedwarn
	movq	%rbx, %r8
.LBB25_33:                              # %if.end124
	movq	global_trees+216(%rip), %r12
	jmp	.LBB25_107
.LBB25_14:                              # %if.then69
	movq	%r8, %rbx
	movq	%r12, %rdi
	callq	common_type
	movslq	warn_sign_compare(%rip), %rcx
	movq	%rax, %r12
	testq	%rcx, %rcx
	js	.LBB25_15
# BB#17:                                # %cond.false
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	movq	%rbx, %r8
	je	.LBB25_107
# BB#18:                                # %cond.false
	movl	skip_evaluation(%rip), %ecx
	testq	%rax, %rcx
	je	.LBB25_19
	jmp	.LBB25_107
.LBB25_97:                              # %if.else223
	cmpq	$13, %rdi
	movq	24(%rsp), %rax          # 8-byte Reload
	jne	.LBB25_102
# BB#98:                                # %if.else223
	cmpq	$6, %rdx
	jne	.LBB25_102
# BB#99:                                # %if.then227
	movq	%rax, %rdi
	callq	integer_zerop
	testq	%rbx, %rax
	je	.LBB25_100
# BB#101:                               # %if.else231
	movq	global_trees+112(%rip), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	8(%rsp), %r8            # 8-byte Reload
	jmp	.LBB25_107
.LBB25_40:                              # %comp_target_types.exit
	testq	%rbx, %rax
	je	.LBB25_42
.LBB25_41:                              # %if.then132
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	common_type
	jmp	.LBB25_55
.LBB25_15:                              # %cond.true
	movl	extra_warnings(%rip), %eax
	testq	%rax, %rax
	movq	%rbx, %r8
	je	.LBB25_107
# BB#16:                                # %cond.true
	movl	skip_evaluation(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB25_107
.LBB25_19:                              # %if.then75
	movq	8(%r15), %rax
	movq	8(%r14), %rdx
	movl	16(%rax), %r14d
	movq	$13, %rcx
	movl	16(%rdx), %eax
	shrq	%cl, %r14
	andq	$1, %r14
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	cmpq	%rax, %r14
	je	.LBB25_107
# BB#20:                                # %if.then89
	movzbl	17(%r12), %ecx
	testq	$32, %rcx
	je	.LBB25_107
# BB#21:                                # %if.else97
	testq	%rax, %rax
	je	.LBB25_23
# BB#22:                                # %land.lhs.true99
	movq	%r8, %rdi
	movq	%r8, %rbx
	callq	tree_expr_nonnegative_p
	movq	%rbx, %r8
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB25_107
.LBB25_23:                              # %lor.lhs.false102
	movq	%r8, %rbx
	testq	%r14, %r14
	je	.LBB25_26
# BB#24:                                # %land.lhs.true104
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	tree_expr_nonnegative_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB25_26
# BB#25:
	movq	%rbx, %r8
	jmp	.LBB25_107
.LBB25_102:                             # %if.else233
	movq	%rsi, %r15
	cmpq	$13, %rdx
	movq	8(%rsp), %r8            # 8-byte Reload
	jne	.LBB25_108
# BB#103:                               # %if.else233
	cmpq	$6, %rdi
	jne	.LBB25_108
# BB#104:                               # %if.then237
	movq	%r8, %rdi
	movq	%r8, %r14
	callq	integer_zerop
	testq	%rbx, %rax
	je	.LBB25_105
# BB#106:                               # %if.else241
	movq	global_trees+112(%rip), %r8
	movq	%r15, %r12
	jmp	.LBB25_107
.LBB25_42:                              # %if.else134
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	integer_zerop
	testq	%rbx, %rax
	je	.LBB25_56
# BB#43:                                # %land.lhs.true137
	movq	8(%r12), %rax
	movq	global_trees+216(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB25_56
# BB#44:                                # %land.lhs.true141
	movzbl	16(%r15), %ecx
	cmpq	$115, %rcx
	jne	.LBB25_45
.LBB25_56:                              # %if.else149
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	integer_zerop
	testq	%rbx, %rax
	je	.LBB25_68
# BB#57:                                # %land.lhs.true152
	movq	8(%r13), %rax
	movq	global_trees+216(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB25_68
# BB#58:                                # %land.lhs.true156
	movzbl	16(%r14), %ecx
	cmpq	$115, %rcx
	jne	.LBB25_59
.LBB25_68:                              # %if.else164
	movq	8(%r12), %rdi
	movl	16(%rdi), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$5, %rcx
	jne	.LBB25_83
# BB#69:                                # %if.then172
	movl	pedantic(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB25_72
# BB#70:                                # %land.lhs.true174
	movq	8(%r13), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$23, %rcx
	jne	.LBB25_72
# BB#71:                                # %if.then182
	movabsq	$.L.str93, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	movq	8(%r12), %rdi
	movl	16(%rdi), %eax
.LBB25_72:                              # %if.end183
	movq	8(%r13), %r8
	andq	%rax, %rbx
	movq	$12, %rcx
	movq	%rbx, %r11
	shrq	%cl, %r11
	movq	$10, %rcx
	shrq	%cl, %rbx
	movl	60(%rdi), %r14d
	movq	$19, %rcx
	shrq	%cl, %r14
	andq	$255, %rax
	xorq	%r9, %r9
	cmpq	$20, %rax
	movabsq	$0, %r10
	jne	.LBB25_76
# BB#73:                                # %land.rhs.i420
	cmpq	$0, 8(%rdi)
	je	.LBB25_74
# BB#75:                                # %land.rhs.i420
	movabsq	$1, %r10
	jmp	.LBB25_76
.LBB25_100:                             # %if.then230
	movabsq	$.L.str95, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	movq	8(%rsp), %r8            # 8-byte Reload
	jmp	.LBB25_107
.LBB25_83:                              # %if.else190
	movq	8(%r13), %r10
	movl	16(%r10), %eax
	movq	%rax, %rdx
	andq	$255, %rdx
	cmpq	$5, %rdx
	jne	.LBB25_96
# BB#84:                                # %if.then198
	movl	pedantic(%rip), %edx
	testq	%rdx, %rdx
	je	.LBB25_87
# BB#85:                                # %if.then198
	cmpq	$23, %rcx
	jne	.LBB25_87
# BB#86:                                # %if.then208
	movabsq	$.L.str93, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	movq	8(%r13), %r10
	movq	8(%r12), %rdi
	movl	16(%r10), %eax
.LBB25_87:                              # %if.end209
	andq	%rax, %rbx
	movq	$12, %rcx
	movq	%rbx, %r11
	shrq	%cl, %r11
	movq	$10, %rcx
	shrq	%cl, %rbx
	movq	%rbx, %rsi
	movl	60(%r10), %r14d
	movq	$19, %rcx
	shrq	%cl, %r14
	andq	$255, %rax
	xorq	%r8, %r8
	cmpq	$20, %rax
	movabsq	$0, %r9
	jne	.LBB25_91
# BB#88:                                # %land.rhs.i
	cmpq	$0, 8(%r10)
	je	.LBB25_89
# BB#90:                                # %land.rhs.i
	movabsq	$1, %r9
	jmp	.LBB25_91
.LBB25_105:                             # %if.then240
	movabsq	$.L.str95, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	movq	%r15, %r12
	movq	%r14, %r8
	jmp	.LBB25_107
.LBB25_26:                              # %if.else108
	movabsq	$.L.str91, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	%rbx, %r8
	jmp	.LBB25_107
.LBB25_96:                              # %if.else216
	movabsq	$.L.str94, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	movq	global_trees+216(%rip), %rdi
	callq	build_pointer_type
	jmp	.LBB25_55
.LBB25_45:                              # %if.then147
	movl	16(%r13), %edx
	movq	$12, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	movq	$10, %rcx
	movq	%rdx, %r10
	shrq	%cl, %r10
	movl	60(%r13), %r9d
	movq	$19, %rcx
	shrq	%cl, %r9
	andq	$255, %rdx
	xorq	%r8, %r8
	cmpq	$20, %rdx
	jne	.LBB25_49
# BB#46:                                # %land.rhs.i507
	cmpq	$0, 8(%r13)
	je	.LBB25_47
# BB#48:                                # %land.rhs.i507
	movabsq	$1, %r8
	jmp	.LBB25_49
.LBB25_59:                              # %if.then162
	movl	16(%r12), %edx
	movq	$12, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	movq	$10, %rcx
	movq	%rdx, %r10
	shrq	%cl, %r10
	movl	60(%r12), %r9d
	movq	$19, %rcx
	shrq	%cl, %r9
	andq	$255, %rdx
	xorq	%r8, %r8
	cmpq	$20, %rdx
	jne	.LBB25_63
# BB#60:                                # %land.rhs.i462
	cmpq	$0, 8(%r12)
	je	.LBB25_61
# BB#62:                                # %land.rhs.i462
	movabsq	$1, %r8
	jmp	.LBB25_63
.LBB25_74:
	movabsq	$0, %r10
.LBB25_76:                              # %land.end.i434
	movq	%rbx, %r15
	movq	$3, %rcx
	movl	16(%r8), %esi
	shlq	%cl, %r10
	movq	$12, %rcx
	movq	%rsi, %rdx
	shrq	%cl, %rdx
	movq	$10, %rcx
	movq	%rsi, %rax
	movl	60(%r8), %ebx
	shrq	%cl, %rax
	movq	$19, %rcx
	shrq	%cl, %rbx
	andq	$255, %rsi
	cmpq	$20, %rsi
	jne	.LBB25_80
# BB#77:                                # %land.rhs44.i437
	cmpq	$0, 8(%r8)
	je	.LBB25_78
# BB#79:                                # %land.rhs44.i437
	movabsq	$1, %r9
	jmp	.LBB25_80
.LBB25_78:
	movabsq	$0, %r9
.LBB25_80:                              # %qualify_type.exit448
	andq	$1, %r11
	movq	%r15, %rsi
	andq	$2, %rsi
	andq	$4, %r14
	andq	$1, %rdx
	andq	$2, %rax
	andq	$4, %rbx
	movq	$3, %rcx
	shlq	%cl, %r9
	orq	%r11, %rsi
	orq	%r14, %rsi
	orq	%r10, %rsi
	orq	%rax, %rsi
	orq	%rdx, %rsi
	orq	%rbx, %rsi
	orq	%r9, %rsi
	jmp	.LBB25_81
.LBB25_89:
	movabsq	$0, %r9
.LBB25_91:                              # %land.end.i
	movq	$3, %rcx
	movl	16(%rdi), %ebx
	shlq	%cl, %r9
	movq	$12, %rcx
	movq	%rbx, %r15
	shrq	%cl, %r15
	movq	$10, %rcx
	movq	%rbx, %rdx
	movl	60(%rdi), %eax
	shrq	%cl, %rdx
	movq	$19, %rcx
	shrq	%cl, %rax
	andq	$255, %rbx
	cmpq	$20, %rbx
	jne	.LBB25_95
# BB#92:                                # %land.rhs44.i
	cmpq	$0, 8(%rdi)
	je	.LBB25_93
# BB#94:                                # %land.rhs44.i
	movabsq	$1, %r8
	jmp	.LBB25_95
.LBB25_93:
	movabsq	$0, %r8
.LBB25_95:                              # %qualify_type.exit
	andq	$1, %r11
	andq	$2, %rsi
	andq	$4, %r14
	andq	$1, %r15
	andq	$2, %rdx
	andq	$4, %rax
	movq	$3, %rcx
	shlq	%cl, %r8
	orq	%r11, %rsi
	orq	%r14, %rsi
	orq	%r9, %rsi
	orq	%rdx, %rsi
	orq	%r15, %rsi
	orq	%rax, %rsi
	orq	%r8, %rsi
	movq	%r10, %rdi
.LBB25_81:                              # %if.end248
	callq	c_build_qualified_type
	movq	%rax, %rdi
	callq	build_pointer_type
	jmp	.LBB25_55
.LBB25_47:
	movabsq	$0, %r8
.LBB25_49:                              # %land.end.i521
	movq	%r13, %rdi
	movq	$3, %rcx
	movl	16(%r12), %ebx
	shlq	%cl, %r8
	movq	$12, %rcx
	movq	%rbx, %r11
	shrq	%cl, %r11
	movq	$10, %rcx
	movq	%rbx, %rdx
	shrq	%cl, %rdx
	movl	60(%r12), %r15d
	movq	$19, %rcx
	shrq	%cl, %r15
	andq	$255, %rbx
	cmpq	$20, %rbx
	movabsq	$0, %rcx
	movabsq	$1, %r14
	movq	%rcx, %rbx
	jne	.LBB25_51
# BB#50:                                # %land.end.i521
	movq	%r14, %rbx
.LBB25_51:                              # %land.end.i521
	andq	$1, %rsi
	andq	$2, %r10
	andq	$4, %r9
	andq	$1, %r11
	andq	$2, %rdx
	andq	$4, %r15
	testq	%rax, %rax
	je	.LBB25_53
# BB#52:                                # %land.end.i521
	movq	%r14, %rcx
.LBB25_53:                              # %land.end.i521
	andq	%rcx, %rbx
	movq	$3, %rcx
	shlq	%cl, %rbx
	orq	%r10, %rsi
	orq	%r9, %rsi
	orq	%r8, %rsi
	orq	%rdx, %rsi
	orq	%r11, %rsi
	orq	%r15, %rsi
	orq	%rbx, %rsi
	jmp	.LBB25_54
.LBB25_61:
	movabsq	$0, %r8
.LBB25_63:                              # %land.end.i476
	movq	$3, %rcx
	movl	16(%r13), %edx
	shlq	%cl, %r8
	movq	$12, %rcx
	movq	%rdx, %r11
	shrq	%cl, %r11
	movq	$10, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	movl	60(%r13), %r14d
	movq	$19, %rcx
	shrq	%cl, %r14
	andq	$255, %rdx
	cmpq	$20, %rdx
	movabsq	$0, %rcx
	movabsq	$1, %r15
	movq	%rcx, %rdx
	jne	.LBB25_65
# BB#64:                                # %land.end.i476
	movq	%r15, %rdx
.LBB25_65:                              # %land.end.i476
	andq	$1, %rsi
	andq	$2, %r10
	andq	$4, %r9
	andq	$1, %r11
	andq	$2, %rdi
	andq	$4, %r14
	testq	%rax, %rax
	je	.LBB25_67
# BB#66:                                # %land.end.i476
	movq	%r15, %rcx
.LBB25_67:                              # %land.end.i476
	andq	%rcx, %rdx
	movq	$3, %rcx
	shlq	%cl, %rdx
	orq	%r10, %rsi
	orq	%r9, %rsi
	orq	%r8, %rsi
	orq	%rdi, %rsi
	orq	%r11, %rsi
	orq	%r14, %rsi
	orq	%rdx, %rsi
	movq	%r12, %rdi
.LBB25_54:                              # %if.end248
	callq	c_build_qualified_type
.LBB25_55:                              # %if.end248
	movq	%rax, %r12
	movq	8(%rsp), %r8            # 8-byte Reload
.LBB25_107:                             # %if.end248
	testq	%r12, %r12
	jne	.LBB25_110
.LBB25_108:                             # %if.then250
	movl	flag_cond_mismatch(%rip), %eax
	testq	%rax, %rax
	je	.LBB25_123
# BB#109:                               # %if.then252
	movq	global_trees+216(%rip), %r12
.LBB25_110:                             # %if.end255
	movl	16(%r8), %edx
	movq	%r8, %rbx
	testq	$4096, %rdx             # imm = 0x1000
	movq	$1, %rax
	movq	24(%rsp), %r13          # 8-byte Reload
	jne	.LBB25_112
# BB#111:                               # %lor.rhs
	movzbl	17(%r13), %eax
	andq	$16, %rax
	movq	$4, %rcx
	shrq	%cl, %rax
.LBB25_112:                             # %lor.end
	testq	$2048, %rdx             # imm = 0x800
	movq	$2, %rsi
	jne	.LBB25_114
# BB#113:                               # %lor.rhs272
	movl	16(%r13), %esi
	movq	$10, %rcx
	shrq	%cl, %rsi
	andq	$2, %rsi
.LBB25_114:                             # %lor.end279
	orq	%rax, %rsi
	movq	%r12, %rdi
	callq	build_qualified_type
	movq	%rbx, %rdx
	movq	8(%rdx), %rcx
	movq	%rax, %r15
	cmpq	%rcx, %r15
	je	.LBB25_116
# BB#115:                               # %if.then285
	movq	%r15, %rdi
	movq	%rdx, %rsi
	callq	convert_and_check
	movq	%rax, %rdx
.LBB25_116:                             # %if.end287
	movq	%rdx, %r14
	movq	%r13, %rbx
	movq	8(%rbx), %rax
	cmpq	%rax, %r15
	je	.LBB25_118
# BB#117:                               # %if.then291
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	convert_and_check
	movq	%rax, %rbx
.LBB25_118:                             # %if.end293
	movq	16(%rsp), %rdx          # 8-byte Reload
	movzbl	16(%rdx), %eax
	cmpq	$25, %rax
	jne	.LBB25_122
# BB#119:                               # %if.then299
	movq	%rdx, %rdi
	callq	integer_zerop
	movq	%rbx, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB25_121
# BB#120:                               # %if.then299
	movq	%r14, %rdi
.LBB25_121:                             # %if.then299
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	pedantic_non_lvalue  # TAILCALL
.LBB25_122:                             # %if.end306
	movq	$51, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r14, %rcx
	movq	%rbx, %r8
	callq	build
	movq	%rax, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fold  # TAILCALL
.LBB25_123:                             # %if.else253
	movabsq	$.L.str96, %rdi
	xorq	%rax, %rax
	callq	error
.LBB25_124:                             # %return
	movq	global_trees(%rip), %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp292:
	.size	build_conditional_expr, .Ltmp292-build_conditional_expr
	.cfi_endproc

	.globl	build_compound_expr
	.align	16, 0x90
	.type	build_compound_expr,@function
build_compound_expr:                    # @build_compound_expr
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rsi
	jmp	internal_build_compound_expr  # TAILCALL
.Ltmp293:
	.size	build_compound_expr, .Ltmp293-build_compound_expr
	.cfi_endproc

	.align	16, 0x90
	.type	internal_build_compound_expr,@function
internal_build_compound_expr:           # @internal_build_compound_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp297:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp298:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp299:
	.cfi_def_cfa_offset 32
.Ltmp300:
	.cfi_offset %rbx, -24
.Ltmp301:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB27_1
# BB#5:                                 # %if.end17
	xorq	%rsi, %rsi
	callq	internal_build_compound_expr
	movq	32(%rbx), %rdi
	movl	16(%rdi), %ecx
	movq	%rax, %r14
	testq	$256, %rcx              # imm = 0x100
	je	.LBB27_6
# BB#11:                                # %if.else
	movl	warn_unused_value(%rip), %eax
	testq	%rax, %rax
	je	.LBB27_13
# BB#12:                                # %if.then50
	callq	warn_if_unused_value
	jmp	.LBB27_13
.LBB27_1:                               # %if.then
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rsi
	jne	.LBB27_3
# BB#2:                                 # %if.then1
	movq	32(%rbx), %rdi
	callq	default_function_array_conversion
	movq	%rax, 32(%rbx)
	movq	%rax, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB27_3
# BB#14:                                # %if.then10
	movq	32(%rbx), %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	non_lvalue  # TAILCALL
.LBB27_3:                               # %if.end14
	movq	32(%rbx), %r14
	jmp	.LBB27_4
.LBB27_6:                               # %if.then25
	movl	extra_warnings(%rip), %eax
	movl	warn_unused_value(%rip), %edx
	orq	%rax, %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	je	.LBB27_10
# BB#7:                                 # %land.lhs.true28
	andq	$255, %rcx
	cmpq	$114, %rcx
	jne	.LBB27_9
# BB#8:                                 # %land.lhs.true35
	movq	8(%rdi), %rax
	movzbl	16(%rax), %eax
	cmpq	$5, %rax
	je	.LBB27_10
.LBB27_9:                               # %if.then44
	movabsq	$.L.str176, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB27_10:                              # %if.end45
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB27_4
.LBB27_13:                              # %if.end55
	movq	8(%r14), %rsi
	movq	32(%rbx), %rdx
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r14, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	build  # TAILCALL
.LBB27_4:                               # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp302:
	.size	internal_build_compound_expr, .Ltmp302-internal_build_compound_expr
	.cfi_endproc

	.globl	build_c_cast
	.align	16, 0x90
	.type	build_c_cast,@function
build_c_cast:                           # @build_c_cast
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp309:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp310:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp311:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp312:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp313:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp314:
	.cfi_def_cfa_offset 96
.Ltmp315:
	.cfi_offset %rbx, -48
.Ltmp316:
	.cfi_offset %r12, -40
.Ltmp317:
	.cfi_offset %r13, -32
.Ltmp318:
	.cfi_offset %r14, -24
.Ltmp319:
	.cfi_offset %r15, -16
	movq	global_trees(%rip), %rax
	movq	%rsi, %rbx
	cmpq	%rdi, %rax
	je	.LBB28_67
# BB#1:                                 # %entry
	cmpq	%rbx, %rax
	je	.LBB28_67
# BB#2:                                 # %if.end
	movq	128(%rdi), %r14
	movl	16(%r14), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$23, %rcx
	jne	.LBB28_3
# BB#6:                                 # %if.then11
	movabsq	$.L.str98, %rdi
	jmp	.LBB28_5
.LBB28_3:                               # %if.end
	cmpq	$18, %rcx
	jne	.LBB28_7
# BB#4:                                 # %if.then4
	movabsq	$.L.str97, %rdi
	jmp	.LBB28_5
.LBB28_7:                               # %if.end12
	movq	8(%rbx), %rdx
	movq	128(%rdx), %rdx
	cmpq	%rdx, %r14
	je	.LBB28_8
# BB#12:                                # %if.else
	cmpq	$21, %rcx
	jne	.LBB28_21
# BB#13:                                # %if.then39
	movq	%rbx, %rdi
	callq	default_function_array_conversion
	movq	24(%r14), %rbx
	movq	%rax, %r15
	testq	%rbx, %rbx
	je	.LBB28_17
# BB#14:                                # %for.body.lr.ph
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB28_15:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movq	8(%r15), %rcx
	movq	128(%rax), %rdi
	movq	128(%rcx), %rsi
	callq	comptypes
	testq	%r12, %rax
	jne	.LBB28_18
# BB#16:                                # %for.inc
                                        #   in Loop: Header=BB28_15 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB28_15
.LBB28_17:                              # %if.end94
	movabsq	$.L.str102, %rdi
.LBB28_5:                               # %return
	xorq	%rax, %rax
	callq	error
	movq	global_trees(%rip), %rax
.LBB28_67:                              # %return
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB28_8:                               # %if.then18
	movl	pedantic(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB28_9
# BB#10:                                # %if.then18
	andq	$254, %rax
	cmpq	$20, %rax
	movq	%rbx, %rax
	jne	.LBB28_60
# BB#11:                                # %if.then31
	movabsq	$.L.str99, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	movq	%rbx, %rax
	jmp	.LBB28_59
.LBB28_21:                              # %if.else95
	movq	global_trees+216(%rip), %rax
	cmpq	%rax, %r14
	je	.LBB28_68
# BB#22:                                # %if.end99
	movq	%rbx, %rdi
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	callq	default_function_array_conversion
	movq	%rax, %r15
	movl	warn_cast_qual(%rip), %eax
	movq	8(%r15), %rdx
	testq	%rax, %rax
	je	.LBB28_42
# BB#23:                                # %land.lhs.true
	movzbl	16(%r14), %eax
	cmpq	$13, %rax
	jne	.LBB28_42
# BB#24:                                # %land.lhs.true109
	movzbl	16(%rdx), %eax
	cmpq	$13, %rax
	jne	.LBB28_42
# BB#25:
	movq	%r15, 16(%rsp)          # 8-byte Spill
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	movq	%r14, %rbx
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	%rdx, %r15
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	xorq	%r12, %r12
.LBB28_26:                              # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r15), %r15
	movl	16(%r15), %edx
	movq	8(%rbx), %rbx
	movq	%rdx, %r14
	andq	$255, %r14
	cmpq	$23, %r14
	jne	.LBB28_29
# BB#27:                                # %land.lhs.true125
                                        #   in Loop: Header=BB28_26 Depth=1
	movl	16(%rbx), %r11d
	movq	%r11, %rax
	andq	$255, %rax
	cmpq	$23, %rax
	jne	.LBB28_29
# BB#28:                                # %land.end186
                                        #   in Loop: Header=BB28_26 Depth=1
	movq	$12, %rcx
	movq	%r11, %rax
	shrq	%cl, %rax
	andq	$1, %rax
	movq	$10, %rcx
	movq	%r11, %rdi
	shrq	%cl, %rdi
	andq	$2, %rdi
	orq	%rax, %rdi
	movl	60(%rbx), %eax
	movq	$19, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	orq	%rdi, %rax
	movq	$12, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	andq	$1, %rdi
	movq	$10, %rcx
	shrq	%cl, %rdx
	movl	60(%r15), %esi
	andq	$2, %rdx
	orq	%rdi, %rdx
	movq	$19, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	orq	%rdx, %rsi
	notq	%rsi
	andq	%rax, %rsi
	orq	%rsi, 32(%rsp)          # 8-byte Folded Spill
	jmp	.LBB28_36
.LBB28_29:                              # %if.else191
                                        #   in Loop: Header=BB28_26 Depth=1
	movq	%r12, %rsi
	movq	$12, %rcx
	movq	%rdx, %r12
	shrq	%cl, %r12
	movq	$10, %rcx
	shrq	%cl, %rdx
	movl	60(%r15), %r8d
	movq	$19, %rcx
	shrq	%cl, %r8
	xorq	%r9, %r9
	cmpq	$20, %r14
	movabsq	$0, %r10
	jne	.LBB28_32
# BB#30:                                # %land.rhs217
                                        #   in Loop: Header=BB28_26 Depth=1
	cmpq	$0, 8(%r15)
	movabsq	$0, %r10
	je	.LBB28_32
# BB#31:                                # %land.rhs217
                                        #   in Loop: Header=BB28_26 Depth=1
	movabsq	$1, %r10
.LBB28_32:                              # %land.end221
                                        #   in Loop: Header=BB28_26 Depth=1
	movq	%r15, %rdi
	movq	$3, %rcx
	movl	16(%rbx), %r11d
	shlq	%cl, %r10
	movq	$12, %rcx
	movq	%r11, %r13
	shrq	%cl, %r13
	movq	$10, %rcx
	movq	%r11, %rax
	movl	60(%rbx), %r15d
	shrq	%cl, %rax
	movq	$19, %rcx
	shrq	%cl, %r15
	movq	%r11, %rcx
	andq	$255, %rcx
	cmpq	$20, %rcx
	jne	.LBB28_35
# BB#33:                                # %land.rhs250
                                        #   in Loop: Header=BB28_26 Depth=1
	cmpq	$0, 8(%rbx)
	movabsq	$0, %r9
	je	.LBB28_35
# BB#34:                                # %land.rhs250
                                        #   in Loop: Header=BB28_26 Depth=1
	movabsq	$1, %r9
.LBB28_35:                              # %land.end254
                                        #   in Loop: Header=BB28_26 Depth=1
	andq	$1, %r12
	andq	$2, %rdx
	orq	%r12, %rdx
	andq	$4, %r8
	orq	%rdx, %r8
	orq	%r10, %r8
	andq	$1, %r13
	andq	$2, %rax
	orq	%r13, %rax
	andq	$4, %r15
	orq	%rax, %r15
	movq	$3, %rcx
	shlq	%cl, %r9
	orq	%r15, %r9
	notq	%r9
	andq	%r8, %r9
	movq	%rsi, %r12
	orq	%r9, %r12
	movq	%rdi, %r15
.LBB28_36:                              # %do.cond
                                        #   in Loop: Header=BB28_26 Depth=1
	andq	$255, %r11
	cmpq	$13, %r11
	jne	.LBB28_38
# BB#37:                                # %do.cond
                                        #   in Loop: Header=BB28_26 Depth=1
	cmpq	$13, %r14
	je	.LBB28_26
.LBB28_38:                              # %do.end
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	32(%rsp), %rax          # 8-byte Reload
	testq	%r15, %rax
	je	.LBB28_40
# BB#39:                                # %if.then276
	movabsq	$.L.str103, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB28_40:                              # %if.end277
	testq	%r15, %r12
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	16(%rsp), %r15          # 8-byte Reload
	movq	8(%rsp), %rdx           # 8-byte Reload
	je	.LBB28_42
# BB#41:                                # %if.then279
	movabsq	$.L.str104, %rdi
	xorq	%rax, %rax
	movq	%rdx, %r12
	callq	warning
	movq	%r12, %rdx
.LBB28_42:                              # %if.end281
	movzbl	16(%r14), %eax
	cmpq	$6, %rax
	jne	.LBB28_47
# BB#43:                                # %land.lhs.true287
	movzbl	16(%rdx), %eax
	cmpq	$13, %rax
	jne	.LBB28_47
# BB#44:                                # %land.lhs.true293
	movl	60(%r14), %eax
	movl	60(%rdx), %ecx
	xorq	%rax, %rcx
	testq	$511, %rcx              # imm = 0x1FF
	je	.LBB28_47
# BB#45:                                # %land.lhs.true302
	movzbl	17(%r15), %eax
	testq	$2, %rax
	jne	.LBB28_47
# BB#46:                                # %if.then309
	movabsq	$.L.str105, %rdi
	xorq	%rax, %rax
	movq	%rdx, %r12
	callq	warning
	movq	%r12, %rdx
.LBB28_47:                              # %if.end310
	movl	warn_bad_function_cast(%rip), %eax
	testq	%rax, %rax
	je	.LBB28_51
# BB#48:                                # %land.lhs.true312
	movzbl	16(%r15), %eax
	cmpq	$53, %rax
	jne	.LBB28_51
# BB#49:                                # %land.lhs.true318
	movl	16(%r14), %eax
	movl	16(%rdx), %ecx
	xorq	%rax, %rcx
	testq	$255, %rcx
	je	.LBB28_51
# BB#50:                                # %if.then328
	movabsq	$.L.str106, %rdi
	xorq	%rax, %rax
	movq	%rdx, %r12
	callq	warning
	movq	%r12, %rdx
.LBB28_51:                              # %if.end329
	movzbl	16(%r14), %eax
	cmpq	$13, %rax
	jne	.LBB28_56
# BB#52:                                # %land.lhs.true335
	movzbl	16(%rdx), %eax
	cmpq	$6, %rax
	jne	.LBB28_56
# BB#53:                                # %land.lhs.true341
	movl	60(%r14), %eax
	movl	60(%rdx), %ecx
	xorq	%rax, %rcx
	testq	$511, %rcx              # imm = 0x1FF
	je	.LBB28_56
# BB#54:                                # %land.lhs.true351
	movzbl	17(%r15), %eax
	testq	$2, %rax
	jne	.LBB28_56
# BB#55:                                # %if.then358
	movabsq	$.L.str107, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB28_56:                              # %if.end359
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	convert
	movl	16(%rax), %ecx
	movq	%rcx, %rdx
	andq	$255, %rdx
	cmpq	$25, %rdx
	jne	.LBB28_58
# BB#57:                                # %if.then366
	movl	16(%r15), %edx
	andq	$524288, %rdx           # imm = 0x80000
	movabsq	$4294443007, %rsi       # imm = 0xFFF7FFFF
	andq	%rsi, %rcx
	orq	%rdx, %rcx
	movl	%ecx, 16(%rax)
	movl	16(%r15), %edx
	andq	$262144, %rdx           # imm = 0x40000
	movabsq	$4294705151, %rsi       # imm = 0xFFFBFFFF
	andq	%rcx, %rsi
	orq	%rdx, %rsi
	movl	%esi, 16(%rax)
.LBB28_58:                              # %if.end391thread-pre-split
	movq	40(%rsp), %rbx          # 8-byte Reload
.LBB28_59:                              # %if.end391thread-pre-split
	movl	pedantic(%rip), %ecx
	jmp	.LBB28_60
.LBB28_68:                              # %if.then97
	movq	$114, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	build1  # TAILCALL
.LBB28_18:                              # %if.then56
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB28_20
# BB#19:                                # %if.then58
	movabsq	$.L.str100, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB28_20:                              # %if.end59
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	build_tree_list
	movq	%rax, %rcx
	movq	$46, %rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	build
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	digest_init
	movl	16(%r15), %ecx
	movl	16(%rax), %edx
	andq	$512, %rcx              # imm = 0x200
	movabsq	$4294966783, %rsi       # imm = 0xFFFFFDFF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, 16(%rax)
	jmp	.LBB28_67
.LBB28_9:
	movq	%rbx, %rax
.LBB28_60:                              # %if.end391
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rcx
	je	.LBB28_67
# BB#61:                                # %land.lhs.true393
	movzbl	16(%rax), %edx
	cmpq	$25, %rdx
	jne	.LBB28_65
# BB#62:                                # %land.lhs.true399
	movzbl	16(%rbx), %edx
	cmpq	$25, %rdx
	jne	.LBB28_65
# BB#63:                                # %land.lhs.true405
	movq	8(%rbx), %rdx
	movzbl	16(%rdx), %edx
	cmpq	$6, %rdx
	je	.LBB28_65
# BB#64:                                # %if.then413
	movq	%rax, %rdi
	callq	non_lvalue
	movl	pedantic(%rip), %ecx
.LBB28_65:                              # %if.end415
	cmpq	%rbx, %rax
	jne	.LBB28_67
# BB#66:                                # %if.end415
	testq	%r14, %rcx
	je	.LBB28_67
# BB#69:                                # %if.then419
	movq	%rbx, %rdi
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	non_lvalue  # TAILCALL
.Ltmp320:
	.size	build_c_cast, .Ltmp320-build_c_cast
	.cfi_endproc

	.align	16, 0x90
	.type	digest_init,@function
digest_init:                            # @digest_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp327:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp328:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp329:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp330:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp331:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp332:
	.cfi_def_cfa_offset 64
.Ltmp333:
	.cfi_offset %rbx, -48
.Ltmp334:
	.cfi_offset %r12, -40
.Ltmp335:
	.cfi_offset %r13, -32
.Ltmp336:
	.cfi_offset %r14, -24
.Ltmp337:
	.cfi_offset %r15, -16
	movq	global_trees(%rip), %rbx
	movq	%rsi, %r12
	movq	%rdi, %r15
	cmpq	%r15, %rbx
	je	.LBB29_77
# BB#1:                                 # %entry
	cmpq	%r12, %rbx
	je	.LBB29_77
# BB#2:                                 # %lor.lhs.false3
	movq	8(%r12), %rax
	cmpq	%rbx, %rax
	je	.LBB29_77
# BB#3:                                 # %if.end
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movl	16(%r15), %r13d
	movzbl	16(%r12), %eax
	andq	$255, %r13
	cmpq	$116, %rax
	movq	%r12, %rdi
	jne	.LBB29_5
# BB#4:                                 # %if.then12
	movq	32(%r12), %rdi
.LBB29_5:                               # %if.end13
	callq	fold
	movq	%rax, %rbx
	cmpq	$18, %r13
	jne	.LBB29_12
# BB#6:                                 # %if.then15
	movq	8(%r15), %rax
	movq	128(%rax), %r14
	movq	integer_types(%rip), %rax
	cmpq	%rax, %r14
	je	.LBB29_17
# BB#7:                                 # %if.then15
	movq	integer_types+8(%rip), %rax
	cmpq	%rax, %r14
	je	.LBB29_17
# BB#8:                                 # %if.then15
	movq	integer_types+16(%rip), %rax
	cmpq	%rax, %r14
	je	.LBB29_17
# BB#9:                                 # %if.then15
	movq	c_global_trees+16(%rip), %rax
	cmpq	%rax, %r14
	je	.LBB29_17
# BB#10:                                # %lor.lhs.false26
	movq	c_global_trees+8(%rip), %rax
	cmpq	%rax, %r14
	jne	.LBB29_12
# BB#11:                                # %lor.lhs.false26
	testq	%rbx, %rbx
	jne	.LBB29_18
.LBB29_12:                              # %if.end118
	testq	%rbx, %rbx
	jne	.LBB29_13
	jmp	.LBB29_34
.LBB29_17:                              # %land.lhs.true
	testq	%rbx, %rbx
	je	.LBB29_34
.LBB29_18:                              # %land.lhs.true28
	movzbl	16(%rbx), %eax
	cmpq	$29, %rax
	jne	.LBB29_13
# BB#19:                                # %if.then34
	movq	8(%rbx), %rax
	movq	128(%rax), %rdi
	movq	128(%r15), %rsi
	callq	comptypes
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB29_77
# BB#20:                                # %if.end44
	movq	8(%rbx), %rax
	movq	8(%rax), %rax
	movq	128(%rax), %rcx
	movq	integer_types(%rip), %rdx
	movl	60(%r14), %eax
	cmpq	%rdx, %rcx
	je	.LBB29_23
# BB#21:                                # %land.lhs.true52
	movl	60(%rdx), %ecx
	xorq	%rax, %rcx
	testq	$511, %rcx              # imm = 0x1FF
	jne	.LBB29_25
# BB#22:                                # %if.then61
	movabsq	$.L.str155, %rdi
	callq	error_init
	jmp	.LBB29_76
.LBB29_13:                              # %land.lhs.true120
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.LBB29_34
# BB#14:                                # %land.lhs.true125
	movq	128(%rax), %rdi
	movq	128(%r15), %rsi
	callq	comptypes
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	jne	.LBB29_46
# BB#15:                                # %lor.lhs.false134
	cmpq	$18, %r13
	je	.LBB29_16
# BB#32:                                # %lor.lhs.false142
	cmpq	$13, %r13
	jne	.LBB29_33
# BB#43:                                # %land.lhs.true153
	movq	8(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$18, %rcx
	je	.LBB29_45
# BB#44:                                # %land.lhs.true153
	cmpq	$23, %rcx
	jne	.LBB29_34
.LBB29_45:                              # %land.lhs.true171
	movq	8(%rax), %rdi
	movq	8(%r15), %rsi
	callq	comptypes
	testq	%r14, %rax
	je	.LBB29_34
.LBB29_46:                              # %if.then180
	cmpq	$13, %r13
	jne	.LBB29_48
# BB#47:                                # %if.then183
	movq	%rbx, %rdi
	callq	default_function_array_conversion
	movq	%rax, %rbx
	jmp	.LBB29_48
.LBB29_33:                              # %lor.lhs.false142
	cmpq	$9, %r13
	jne	.LBB29_34
.LBB29_16:                              # %land.lhs.true137
	movq	8(%rbx), %rdi
	movq	%r15, %rsi
	callq	comptypes
	testq	%r14, %rax
	je	.LBB29_34
.LBB29_48:                              # %if.end185
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%r14, %rax
	je	.LBB29_52
# BB#49:                                # %if.end185
	movl	flag_isoc99(%rip), %eax
	testq	%r14, %rax
	jne	.LBB29_52
# BB#50:                                # %land.lhs.true189
	movzbl	16(%rbx), %eax
	cmpq	$170, %rax
	jne	.LBB29_52
# BB#51:                                # %if.then196
	movq	32(%rbx), %rax
	movq	32(%rax), %rax
	movq	104(%rax), %rbx
.LBB29_52:                              # %if.end204
	cmpq	$18, %r13
	jne	.LBB29_56
# BB#53:                                # %land.lhs.true207
	movzbl	16(%rbx), %eax
	cmpq	$29, %rax
	je	.LBB29_56
# BB#54:                                # %land.lhs.true207
	cmpq	$46, %rax
	jne	.LBB29_55
.LBB29_56:                              # %if.end222
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB29_57
# BB#63:                                # %land.lhs.true224
	movl	16(%rbx), %eax
	movl	pedantic(%rip), %ecx
	movq	%rax, %rdx
	andq	$255, %rdx
	cmpq	$34, %rdx
	movq	8(%rsp), %rdx           # 8-byte Reload
	jne	.LBB29_58
# BB#64:                                # %land.lhs.true224
	testq	%r14, %rcx
	jne	.LBB29_58
# BB#65:                                # %lor.lhs.false.i
	movzbl	48(%rbx), %ecx
	cmpq	$51, %rcx
	je	.LBB29_72
# BB#66:                                # %if.else.i
	cmpq	$0, current_function_decl(%rip)
	je	.LBB29_72
# BB#67:                                # %if.else.i
	andq	$6144, %rax             # imm = 0x1800
	andq	%r14, %rax
	cmpq	$4096, %rax             # imm = 0x1000
	jne	.LBB29_72
# BB#68:                                # %land.lhs.true7.i.i
	movq	104(%rbx), %rax
	testq	%rax, %rax
	je	.LBB29_72
# BB#69:                                # %land.lhs.true10.i.i
	movl	16(%rax), %ecx
	testq	$512, %rcx              # imm = 0x200
	je	.LBB29_72
# BB#70:                                # %switch.early.test.i.i
	andq	$255, %rcx
	je	.LBB29_72
# BB#71:                                # %switch.early.test.i.i
	andq	%r14, %rcx
	cmpq	$46, %rcx
	jne	.LBB29_73
.LBB29_72:                              # %if.end.i.i
	movq	%rbx, %rax
.LBB29_73:                              # %if.end233.thread
	testq	%r14, %rdx
	movq	%rax, %rbx
	jne	.LBB29_80
	jmp	.LBB29_77
.LBB29_34:                              # %if.end270
	movl	16(%r15), %eax
	andq	$254, %rax
	cmpq	$6, %rax
	je	.LBB29_39
# BB#35:                                # %if.end270
	cmpq	$13, %r13
	je	.LBB29_39
# BB#36:                                # %if.end270
	cmpq	$10, %r13
	je	.LBB29_39
# BB#37:                                # %if.end270
	cmpq	$11, %r13
	je	.LBB29_39
# BB#38:                                # %if.end270
	cmpq	$8, %r13
	je	.LBB29_39
# BB#83:                                # %if.end310
	movq	32(%r15), %rax
	testq	%rax, %rax
	je	.LBB29_86
# BB#84:                                # %land.lhs.true315
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB29_85
.LBB29_86:                              # %if.end325
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB29_99
# BB#87:
	movq	$0, (%rsp)              # 8-byte Folded Spill
	xorq	%r14, %r14
	movq	%r15, %rbx
	jmp	.LBB29_88
.LBB29_94:                              # %while.cond
                                        #   in Loop: Header=BB29_88 Depth=1
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%r13, %r14
.LBB29_88:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx), %ecx
	cmpq	$22, %rcx
	ja	.LBB29_97
# BB#89:                                # %while.cond
                                        #   in Loop: Header=BB29_88 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$7602176, %rax          # imm = 0x740000
	je	.LBB29_97
# BB#90:                                # %while.body
                                        #   in Loop: Header=BB29_88 Depth=1
	movq	$46, %rdi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	build
	movq	%rax, %r13
	testq	%r14, %r14
                                        # kill: RAX<def> R13<kill>
	je	.LBB29_92
# BB#91:                                # %if.else358
                                        #   in Loop: Header=BB29_88 Depth=1
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	callq	build_tree_list
	movq	%rax, 40(%r14)
	movq	(%rsp), %rax            # 8-byte Reload
.LBB29_92:                              # %if.end363
                                        #   in Loop: Header=BB29_88 Depth=1
	movzbl	16(%rbx), %ecx
	cmpq	$18, %rcx
	jne	.LBB29_95
# BB#93:                                # %if.then370
                                        #   in Loop: Header=BB29_88 Depth=1
	addq	$8, %rbx
	movq	(%rbx), %rbx
	jmp	.LBB29_94
.LBB29_95:                              # %if.else373
                                        #   in Loop: Header=BB29_88 Depth=1
	movq	24(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB29_99
# BB#96:                                # %if.then377
                                        #   in Loop: Header=BB29_88 Depth=1
	addq	$8, %rcx
	movq	(%rcx), %rbx
	jmp	.LBB29_94
.LBB29_39:                              # %if.then288
	movabsq	$.L.str159, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	convert_for_assignment
	movq	%rax, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rcx           # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB29_77
# BB#40:                                # %land.lhs.true291
	movzbl	17(%rbx), %eax
	testq	$2, %rax
	je	.LBB29_82
# BB#41:                                # %land.lhs.true301
	movq	8(%rbx), %rsi
	movq	%rbx, %rdi
	callq	initializer_constant_valid_p
	testq	%rax, %rax
	jne	.LBB29_77
# BB#42:                                # %if.then307
	movabsq	$.L.str151, %rdi
	callq	error_init
	jmp	.LBB29_76
.LBB29_97:                              # %while.end
	cmpq	%r15, %rbx
	je	.LBB29_76
# BB#98:                                # %if.then387
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	digest_init
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	build_tree_list
	movq	%rax, 40(%r14)
	movq	(%rsp), %rax            # 8-byte Reload
	jmp	.LBB29_78
.LBB29_85:                              # %if.then324
	movabsq	$.L.str160, %rdi
	callq	error_init
	jmp	.LBB29_76
.LBB29_57:                              # %if.end222.if.end233_crit_edge
	movl	pedantic(%rip), %ecx
	movq	8(%rsp), %rdx           # 8-byte Reload
.LBB29_58:                              # %if.end233
	testq	%r14, %rdx
	je	.LBB29_79
# BB#59:                                # %if.end233
	testq	%r14, %rcx
	je	.LBB29_79
# BB#60:                                # %land.lhs.true237
	movzbl	16(%rbx), %eax
	cmpq	$47, %rax
	jne	.LBB29_79
# BB#61:                                # %if.then244
	movq	8(%rbx), %rsi
	movq	%rbx, %rdi
	callq	valid_compound_expr_initializer
	movq	global_trees(%rip), %rcx
	movq	%rax, %rbx
	movabsq	$.L.str150, %rdi
	cmpq	%rcx, %rbx
	je	.LBB29_62
# BB#74:                                # %if.else
	callq	pedwarn_init
	jmp	.LBB29_75
.LBB29_79:                              # %if.else255
	testq	%r14, %rdx
	je	.LBB29_77
.LBB29_80:                              # %land.lhs.true257
	movzbl	17(%rbx), %eax
	testq	$2, %rax
	je	.LBB29_82
# BB#81:                                # %lor.lhs.false262
	movq	8(%rbx), %rsi
	movq	%rbx, %rdi
	callq	initializer_constant_valid_p
	testq	%rax, %rax
	jne	.LBB29_77
.LBB29_82:                              # %if.then267
	movabsq	$.L.str150, %rdi
	callq	error_init
	jmp	.LBB29_76
.LBB29_99:                              # %if.end394
	movabsq	$.L.str161, %rdi
	callq	error_init
	jmp	.LBB29_76
.LBB29_55:                              # %if.then221
	movabsq	$.L.str158, %rdi
	callq	error_init
	jmp	.LBB29_76
.LBB29_23:                              # %land.lhs.true70
	movl	60(%rcx), %ecx
	xorq	%rax, %rcx
	testq	$511, %rcx              # imm = 0x1FF
	je	.LBB29_25
# BB#24:                                # %if.then80
	movabsq	$.L.str156, %rdi
	callq	error_init
	jmp	.LBB29_76
.LBB29_25:                              # %if.end81
	movq	%r15, 8(%rbx)
	cmpq	$0, 24(%r15)
	je	.LBB29_77
# BB#26:                                # %land.lhs.true86
	movq	32(%r15), %rax
	testq	%rax, %rax
	je	.LBB29_77
# BB#27:                                # %land.lhs.true89
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB29_77
# BB#28:                                # %land.lhs.true97
	movq	integer_types(%rip), %rax
	movl	60(%r14), %ecx
	movl	60(%rax), %eax
	movq	40(%r15), %rdi
	movl	32(%rbx), %esi
	xorq	%rcx, %rax
	testq	$511, %rax              # imm = 0x1FF
	movq	$1, %rax
	je	.LBB29_30
# BB#29:                                # %cond.true
	movq	c_global_trees(%rip), %rax
	movl	60(%rax), %eax
	movq	$3, %rcx
	shrq	%cl, %rax
	andq	$63, %rax
.LBB29_30:                              # %cond.end
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
	jns	.LBB29_77
# BB#31:                                # %if.then115
	movabsq	$.L.str157, %rdi
	callq	pedwarn_init
	jmp	.LBB29_77
.LBB29_62:                              # %if.then250
	callq	error_init
.LBB29_75:                              # %if.end251
	movl	flag_pedantic_errors(%rip), %eax
	testq	%rax, %rax
	je	.LBB29_77
.LBB29_76:                              # %select.mid
	movq	global_trees(%rip), %rbx
.LBB29_77:                              # %select.end
	movq	%rbx, %rax
.LBB29_78:                              # %select.end
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp338:
	.size	digest_init, .Ltmp338-digest_init
	.cfi_endproc

	.globl	c_cast_expr
	.align	16, 0x90
	.type	c_cast_expr,@function
c_cast_expr:                            # @c_cast_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp342:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp343:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp344:
	.cfi_def_cfa_offset 32
.Ltmp345:
	.cfi_offset %rbx, -24
.Ltmp346:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movzbl	16(%rbx), %eax
	movl	warn_strict_prototypes(%rip), %r14d
	cmpq	$25, %rax
	jne	.LBB30_2
# BB#1:                                 # %if.then
	xorq	%rax, %rax
	movl	%eax, warn_strict_prototypes(%rip)
.LBB30_2:                               # %if.end
	callq	groktypename
	movl	%r14d, warn_strict_prototypes(%rip)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	build_c_cast  # TAILCALL
.Ltmp347:
	.size	c_cast_expr, .Ltmp347-c_cast_expr
	.cfi_endproc

	.align	16, 0x90
	.type	convert_for_assignment,@function
convert_for_assignment:                 # @convert_for_assignment
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp354:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp355:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp356:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp357:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp358:
	.cfi_def_cfa_offset 48
	subq	$128, %rsp
.Ltmp359:
	.cfi_def_cfa_offset 176
.Ltmp360:
	.cfi_offset %rbx, -48
.Ltmp361:
	.cfi_offset %r12, -40
.Ltmp362:
	.cfi_offset %r13, -32
.Ltmp363:
	.cfi_offset %r14, -24
.Ltmp364:
	.cfi_offset %r15, -16
	movzbl	16(%rsi), %eax
	movl	16(%rdi), %r14d
	movq	%rdi, %r12
	movq	%r9, 96(%rsp)           # 8-byte Spill
	movq	%r8, 104(%rsp)          # 8-byte Spill
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	cmpq	$116, %rax
	jne	.LBB31_2
# BB#1:                                 # %if.then
	movq	32(%rsi), %rsi
.LBB31_2:                               # %if.end
	movq	8(%rsi), %rax
	movzbl	16(%rax), %eax
	movq	%r14, %r13
	andq	$255, %r13
	cmpq	$18, %rax
	je	.LBB31_4
# BB#3:                                 # %if.end
	cmpq	$23, %rax
	jne	.LBB31_5
.LBB31_4:                               # %if.then19
	movq	%rdx, 112(%rsp)         # 8-byte Spill
	movq	%rsi, %rdi
	callq	default_conversion
	movq	%rax, %rdx
	jmp	.LBB31_16
.LBB31_5:                               # %if.else
	movq	%rdx, 112(%rsp)         # 8-byte Spill
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB31_15
# BB#6:                                 # %land.lhs.true
	movl	16(%rsi), %ecx
	movq	%rcx, %rax
	andq	$255, %rax
	cmpq	$34, %rax
	jne	.LBB31_15
# BB#7:                                 # %land.lhs.true
	movl	pedantic(%rip), %edx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	jne	.LBB31_15
# BB#8:                                 # %lor.lhs.false.i
	movzbl	48(%rsi), %edx
	cmpq	$51, %rdx
	je	.LBB31_15
# BB#9:                                 # %if.else.i
	cmpq	$0, current_function_decl(%rip)
	je	.LBB31_15
# BB#10:                                # %if.else.i
	andq	$6144, %rcx             # imm = 0x1800
	andq	%rax, %rcx
	cmpq	$4096, %rcx             # imm = 0x1000
	jne	.LBB31_15
# BB#11:                                # %land.lhs.true7.i.i
	movq	104(%rsi), %rdx
	testq	%rdx, %rdx
	je	.LBB31_15
# BB#12:                                # %land.lhs.true10.i.i
	movl	16(%rdx), %ecx
	testq	$512, %rcx              # imm = 0x200
	je	.LBB31_15
# BB#13:                                # %switch.early.test.i.i
	andq	$255, %rcx
	je	.LBB31_15
# BB#14:                                # %switch.early.test.i.i
	andq	%rax, %rcx
	cmpq	$46, %rcx
	jne	.LBB31_16
.LBB31_15:                              # %if.end.i.i
	movq	%rsi, %rdx
.LBB31_16:                              # %if.end28
	movq	8(%rdx), %rsi
	movq	%rsi, 120(%rsp)         # 8-byte Spill
	movl	16(%rsi), %ebx
	movq	%rbx, %r15
	andq	$255, %r15
	je	.LBB31_183
# BB#17:                                # %if.end37
	movq	%r12, %r8
	movq	128(%r8), %rax
	movq	128(%rsi), %rcx
	cmpq	%rcx, %rax
	je	.LBB31_18
# BB#19:                                # %if.end44
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %r15
	cmpq	$5, %r15
	jne	.LBB31_21
# BB#20:                                # %if.then46
	movabsq	$.L.str15, %rdi
	xorq	%rax, %rax
	callq	error
.LBB31_183:                             # %if.end974
	movq	global_trees(%rip), %rax
	jmp	.LBB31_184
.LBB31_18:                              # %if.then42
	movq	%rdx, %rdi
	movq	%r8, %r15
	movq	%rdx, %r14
	movq	%rsi, %rbx
	callq	overflow_warning
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	maybe_objc_comptypes
	movq	%r14, %rax
.LBB31_184:                             # %return
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB31_21:                              # %if.end47
	cmpq	$15, %r13
	jne	.LBB31_27
# BB#22:                                # %land.lhs.true49
	movq	8(%r8), %rdi
	movq	%r8, 80(%rsp)           # 8-byte Spill
	movq	%rdx, %r12
	movq	%rsi, 120(%rsp)         # 8-byte Spill
	callq	comptypes
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rdx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	80(%rsp), %r8           # 8-byte Reload
	andq	%r12, %rax
	cmpq	$1, %rax
	jne	.LBB31_27
# BB#23:                                # %if.then56
	movq	%r8, %r14
	movq	%rdx, %rdi
	movq	%rdx, %rbx
	callq	mark_addressable
	testq	%r12, %rax
	je	.LBB31_183
# BB#24:                                # %if.end60
	movq	8(%rbx), %rdi
	callq	build_pointer_type
	movq	$121, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	build1
	movq	%rax, %rbx
	movq	8(%rbx), %rax
	movq	8(%rax), %rax
	movq	%r14, %rsi
	movq	8(%rsi), %rdi
	cmpq	%rdi, %rax
	je	.LBB31_26
# BB#25:                                # %if.then72
	movq	%rsi, %r14
	callq	build_pointer_type
	movq	$115, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	build1
	movq	%r14, %rsi
	movq	%rax, %rbx
.LBB31_26:                              # %if.end77
	movq	$115, %rdi
	movq	%rbx, %rdx
	jmp	.LBB31_101
.LBB31_27:                              # %if.else79
	movq	%r14, %rax
	andq	$254, %rax
	cmpq	$6, %rax
	je	.LBB31_31
# BB#28:                                # %if.else79
	cmpq	$10, %r13
	je	.LBB31_31
# BB#29:                                # %if.else79
	cmpq	$8, %r13
	je	.LBB31_31
# BB#30:                                # %if.else79
	cmpq	$11, %r13
	jne	.LBB31_35
.LBB31_31:                              # %land.lhs.true89
	movq	%rbx, %rax
	andq	$254, %rax
	cmpq	$6, %rax
	je	.LBB31_185
# BB#32:                                # %land.lhs.true89
	cmpq	$10, %r15
	je	.LBB31_185
# BB#33:                                # %land.lhs.true89
	cmpq	$8, %r15
	je	.LBB31_185
# BB#34:                                # %land.lhs.true89
	cmpq	$11, %r15
	je	.LBB31_185
.LBB31_35:                              # %if.else101
	cmpq	$21, %r13
	jne	.LBB31_102
# BB#36:                                # %land.lhs.true103
	movzbl	62(%r8), %eax
	testq	$8, %rax
	je	.LBB31_102
# BB#37:                                # %land.lhs.true103
	cmpq	$0, 112(%rsp)           # 8-byte Folded Reload
	jne	.LBB31_102
# BB#38:                                # %if.then110
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	24(%r8), %r14
	movq	%r8, 80(%rsp)           # 8-byte Spill
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	testq	%r14, %r14
	je	.LBB31_78
# BB#39:                                # %for.body.lr.ph
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
.LBB31_40:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r14), %rbx
	movq	128(%rbx), %rdi
	movq	128(%rsi), %rsi
	callq	comptypes
	testq	%r12, %rax
	jne	.LBB31_96
# BB#41:                                # %if.end122
                                        #   in Loop: Header=BB31_40 Depth=1
	movzbl	16(%rbx), %eax
	cmpq	$13, %rax
	jne	.LBB31_77
# BB#42:                                # %if.end129
                                        #   in Loop: Header=BB31_40 Depth=1
	cmpq	$13, %r15
	jne	.LBB31_74
# BB#43:                                # %if.then131
                                        #   in Loop: Header=BB31_40 Depth=1
	movq	8(%rbx), %rdi
	movzbl	16(%rdi), %eax
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	8(%rsi), %r9
	cmpq	$5, %rax
	je	.LBB31_50
# BB#44:                                # %lor.lhs.false141
                                        #   in Loop: Header=BB31_40 Depth=1
	movzbl	16(%r9), %eax
	cmpq	$5, %rax
	je	.LBB31_50
# BB#45:                                # %lor.lhs.false147
                                        #   in Loop: Header=BB31_40 Depth=1
	movq	%r9, 16(%rsp)           # 8-byte Spill
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rsi, %r13
	callq	maybe_objc_comptypes
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jns	.LBB31_49
# BB#46:                                # %if.end.i
                                        #   in Loop: Header=BB31_40 Depth=1
	movq	8(%rbx), %rax
	movq	8(%r13), %rcx
	movq	128(%rax), %rdi
	movq	128(%rcx), %rsi
	callq	comptypes
	movq	%rax, %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	jne	.LBB31_49
# BB#47:                                # %if.end.i
                                        #   in Loop: Header=BB31_40 Depth=1
	movl	pedantic(%rip), %ecx
	testq	%r12, %rcx
	je	.LBB31_49
# BB#48:                                # %comp_target_types.exit.thread
                                        #   in Loop: Header=BB31_40 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str182, %rdi
	callq	pedwarn
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	16(%rsp), %r9           # 8-byte Reload
	jmp	.LBB31_50
.LBB31_49:                              # %comp_target_types.exit
                                        #   in Loop: Header=BB31_40 Depth=1
	testq	%r12, %rax
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	16(%rsp), %r9           # 8-byte Reload
	je	.LBB31_74
.LBB31_50:                              # %if.then150
                                        #   in Loop: Header=BB31_40 Depth=1
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movl	16(%rdi), %eax
	movq	$12, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	movq	$10, %rcx
	movq	%rax, %r13
	shrq	%cl, %r13
	movl	60(%rdi), %edx
	movq	$19, %rcx
	shrq	%cl, %rdx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	andq	$255, %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%r8, %r8
	cmpq	$20, %rax
	movabsq	$0, %r11
	jne	.LBB31_53
# BB#51:                                # %land.rhs
                                        #   in Loop: Header=BB31_40 Depth=1
	cmpq	$0, 8(%rdi)
	movabsq	$0, %r11
	je	.LBB31_53
# BB#52:                                # %land.rhs
                                        #   in Loop: Header=BB31_40 Depth=1
	movabsq	$1, %r11
.LBB31_53:                              # %land.end
                                        #   in Loop: Header=BB31_40 Depth=1
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	$3, %rcx
	movl	16(%r9), %eax
	shlq	%cl, %r11
	movq	$12, %rcx
	movq	%rax, %r10
	shrq	%cl, %r10
	movq	$10, %rcx
	movq	%rax, %rdx
	movl	60(%r9), %edi
	shrq	%cl, %rdx
	movq	$19, %rcx
	shrq	%cl, %rdi
	andq	$255, %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	cmpq	$20, %rax
	jne	.LBB31_56
# BB#54:                                # %land.rhs201
                                        #   in Loop: Header=BB31_40 Depth=1
	cmpq	$0, 8(%r9)
	movabsq	$0, %r8
	je	.LBB31_56
# BB#55:                                # %land.rhs201
                                        #   in Loop: Header=BB31_40 Depth=1
	movabsq	$1, %r8
.LBB31_56:                              # %land.end205
                                        #   in Loop: Header=BB31_40 Depth=1
	movq	%r9, 16(%rsp)           # 8-byte Spill
	andq	$1, %rsi
	movq	%rsi, (%rsp)            # 8-byte Spill
	andq	$2, %r13
	movq	%rsi, %rbx
	orq	%r13, %rbx
	movq	40(%rsp), %rax          # 8-byte Reload
	andq	$4, %rax
	orq	%rax, %rbx
	orq	%rbx, %r11
	andq	$1, %r10
	movq	%r10, 8(%rsp)           # 8-byte Spill
	andq	$2, %rdx
	movq	%r10, %rsi
	movq	%rsi, %r9
	orq	%rdx, %r9
	movq	%rdx, %rsi
	andq	$4, %rdi
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	%r9, %r10
	orq	%rdi, %r10
	movq	$3, %rcx
	shlq	%cl, %r8
	orq	%r10, %r8
	andq	%r12, %r8
	andq	%r12, %r11
	cmpq	%r8, %r11
	je	.LBB31_96
# BB#57:                                # %lor.lhs.false210
                                        #   in Loop: Header=BB31_40 Depth=1
	cmpq	$23, 56(%rsp)           # 8-byte Folded Reload
	movq	24(%rsp), %r11          # 8-byte Reload
	jne	.LBB31_60
# BB#58:                                # %lor.lhs.false210
                                        #   in Loop: Header=BB31_40 Depth=1
	cmpq	$23, %r11
	jne	.LBB31_60
# BB#59:                                # %land.end318
                                        #   in Loop: Header=BB31_40 Depth=1
	orq	%r13, %r9
	movq	(%rsp), %rcx            # 8-byte Reload
	orq	%rcx, %r9
	orq	%rax, %r9
	movq	40(%rsp), %rax          # 8-byte Reload
	orq	%rax, %r9
	andq	%r12, %r9
	andq	%r12, %r10
	cmpq	%r10, %r9
	jmp	.LBB31_70
.LBB31_60:                              # %cond.false
                                        #   in Loop: Header=BB31_40 Depth=1
	xorq	%rdx, %rdx
	cmpq	$20, %r11
	movabsq	$0, %rax
	movq	48(%rsp), %r8           # 8-byte Reload
	jne	.LBB31_63
# BB#61:                                # %land.rhs348
                                        #   in Loop: Header=BB31_40 Depth=1
	cmpq	$0, 8(%r8)
	movabsq	$0, %rax
	je	.LBB31_63
# BB#62:                                # %land.rhs348
                                        #   in Loop: Header=BB31_40 Depth=1
	movabsq	$1, %rax
.LBB31_63:                              # %land.end352
                                        #   in Loop: Header=BB31_40 Depth=1
	movq	$3, %rcx
	shlq	%cl, %rax
	cmpq	$20, 56(%rsp)           # 8-byte Folded Reload
	movq	40(%rsp), %r9           # 8-byte Reload
	movq	8(%rsp), %r10           # 8-byte Reload
	movq	%rsi, %rdi
	jne	.LBB31_66
# BB#64:                                # %land.rhs381
                                        #   in Loop: Header=BB31_40 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	cmpq	$0, 8(%rcx)
	movabsq	$0, %rdx
	je	.LBB31_66
# BB#65:                                # %land.rhs381
                                        #   in Loop: Header=BB31_40 Depth=1
	movabsq	$1, %rdx
.LBB31_66:                              # %land.end385
                                        #   in Loop: Header=BB31_40 Depth=1
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	cmpq	$20, %r11
	jne	.LBB31_69
# BB#67:                                # %land.rhs415
                                        #   in Loop: Header=BB31_40 Depth=1
	cmpq	$0, 8(%r8)
	movabsq	$0, %rsi
	je	.LBB31_69
# BB#68:                                # %land.rhs415
                                        #   in Loop: Header=BB31_40 Depth=1
	movabsq	$1, %rsi
.LBB31_69:                              # %land.end419
                                        #   in Loop: Header=BB31_40 Depth=1
	orq	%rbx, %rax
	orq	%rdi, %rax
	orq	%r10, %rax
	orq	%r9, %rax
	orq	%rdx, %rax
	movq	$3, %rcx
	shlq	%cl, %rsi
	orq	%rbx, %rsi
	andq	%r12, %rsi
	andq	%r12, %rax
	cmpq	%rsi, %rax
.LBB31_70:                              # %land.end419
                                        #   in Loop: Header=BB31_40 Depth=1
	movq	32(%rsp), %rcx          # 8-byte Reload
	je	.LBB31_96
# BB#71:                                # %if.end425
                                        #   in Loop: Header=BB31_40 Depth=1
	movq	64(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	je	.LBB31_73
# BB#72:                                # %if.end425
                                        #   in Loop: Header=BB31_40 Depth=1
	movq	%rax, %rcx
.LBB31_73:                              # %if.end425
                                        #   in Loop: Header=BB31_40 Depth=1
	movq	%rcx, 64(%rsp)          # 8-byte Spill
.LBB31_74:                              # %if.end430
                                        #   in Loop: Header=BB31_40 Depth=1
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, %rbx
	callq	integer_zerop
	testq	%r12, %rax
	jne	.LBB31_84
# BB#75:                                # %lor.lhs.false433
                                        #   in Loop: Header=BB31_40 Depth=1
	movzbl	16(%rbx), %eax
	cmpq	$115, %rax
	jne	.LBB31_76
# BB#83:                                # %land.lhs.true439
                                        #   in Loop: Header=BB31_40 Depth=1
	movq	32(%rbx), %rdi
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	callq	integer_zerop
	testq	%r12, %rax
	je	.LBB31_77
	jmp	.LBB31_84
.LBB31_76:                              #   in Loop: Header=BB31_40 Depth=1
	movq	%rbx, 72(%rsp)          # 8-byte Spill
.LBB31_77:                              # %for.inc
                                        #   in Loop: Header=BB31_40 Depth=1
	movq	(%r14), %r14
	testq	%r14, %r14
	movq	120(%rsp), %rsi         # 8-byte Reload
	jne	.LBB31_40
.LBB31_78:                              # %for.end
	movq	64(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	je	.LBB31_173
# BB#79:                                # %if.then453
	movq	8(%rsi), %r9
	movl	16(%r9), %r14d
	movq	8(%rax), %r8
	movq	%r14, %rsi
	andq	$255, %rsi
	cmpq	$23, %rsi
	jne	.LBB31_85
# BB#80:                                # %land.lhs.true465
	movl	16(%r8), %edx
	movq	%rdx, %rcx
	andq	$255, %rcx
	cmpq	$23, %rcx
	jne	.LBB31_85
# BB#81:                                # %land.end534
	movq	$12, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$1, %rsi
	movq	$10, %rcx
	shrq	%cl, %rdx
	andq	$2, %rdx
	orq	%rsi, %rdx
	movl	60(%r8), %esi
	movq	$19, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	orq	%rdx, %rsi
	movq	$12, %rcx
	movq	%r14, %rdx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	$10, %rcx
	shrq	%cl, %r14
	movl	60(%r9), %edi
	andq	$2, %r14
	orq	%rdx, %r14
	movq	$19, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	orq	%r14, %rdi
	notq	%rdi
	testq	%rdi, %rsi
	je	.LBB31_96
# BB#82:                                # %if.then539
	movabsq	$.L.str163, %rdi
	jmp	.LBB31_95
.LBB31_102:                             # %if.else625
	andq	$253, %r14
	cmpq	$13, %r14
	jne	.LBB31_154
# BB#103:                               # %if.else625
	andq	$253, %rbx
	cmpq	$13, %rbx
	jne	.LBB31_154
# BB#104:                               # %if.then633
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	8(%r8), %r14
	movzbl	16(%r14), %eax
	movq	8(%rsi), %r13
	cmpq	$5, %rax
	je	.LBB31_112
# BB#105:                               # %lor.lhs.false645
	movzbl	16(%r13), %eax
	cmpq	$5, %rax
	je	.LBB31_112
# BB#106:                               # %lor.lhs.false651
	movq	$1, %rdx
	movq	%r8, %rdi
	movq	%r8, %r15
	movq	%rsi, %rbx
	callq	maybe_objc_comptypes
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jns	.LBB31_110
# BB#107:                               # %if.end.i1223
	movq	8(%r15), %rax
	movq	8(%rbx), %rcx
	movq	128(%rax), %rdi
	movq	128(%rcx), %rsi
	callq	comptypes
	movq	%rax, %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	jne	.LBB31_110
# BB#108:                               # %if.end.i1223
	movl	pedantic(%rip), %ecx
	testq	%r12, %rcx
	je	.LBB31_110
# BB#109:                               # %comp_target_types.exit1226.thread
	movabsq	$.L.str182, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	movq	%r15, %r8
	movq	%rbx, %rsi
	jmp	.LBB31_112
.LBB31_154:                             # %if.else893
	movq	%r8, %r14
	cmpq	$13, %r13
	jne	.LBB31_165
# BB#155:                               # %if.else893
	cmpq	$6, %r15
	jne	.LBB31_165
# BB#156:                               # %if.then897
	movl	16(%rdx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$25, %rcx
	jne	.LBB31_159
# BB#157:                               # %land.lhs.true903
	movq	%rdx, %rdi
	movq	%rdx, %rbx
	callq	integer_zerop
	movq	%rbx, %rdx
	testq	%r12, %rax
	jne	.LBB31_164
# BB#158:                               # %land.lhs.true903.land.lhs.true906_crit_edge
	movl	16(%rdx), %eax
.LBB31_159:                             # %land.lhs.true906
	andq	$255, %rax
	cmpq	$115, %rax
	jne	.LBB31_163
# BB#160:                               # %land.lhs.true912
	movq	8(%rdx), %rax
	movzbl	16(%rax), %eax
	cmpq	$6, %rax
	jne	.LBB31_163
# BB#161:                               # %land.lhs.true920
	movq	32(%rdx), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB31_163
# BB#162:                               # %land.lhs.true929
	movq	%rdx, %rbx
	callq	integer_zerop
	movq	%rbx, %rdx
	testq	%r12, %rax
	je	.LBB31_163
.LBB31_164:                             # %if.end937
	movq	global_trees+112(%rip), %rax
	jmp	.LBB31_184
.LBB31_185:                             # %if.then99
	movq	%r8, %rdi
	movq	%rdx, %rsi
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	convert_and_check  # TAILCALL
.LBB31_165:                             # %if.else938
	movq	%rdx, %rbx
	cmpq	$6, %r13
	jne	.LBB31_169
# BB#166:                               # %if.else938
	cmpq	$13, %r15
	jne	.LBB31_169
# BB#167:                               # %if.then942
	movabsq	$.L.str170, %rdi
	movq	112(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB31_168
.LBB31_163:                             # %if.then935
	movabsq	$.L.str169, %rdi
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	%rdx, %rbx
.LBB31_168:                             # %if.then942
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	96(%rsp), %rcx          # 8-byte Reload
	callq	warn_for_assignment
	jmp	.LBB31_171
.LBB31_169:                             # %if.else944
	cmpq	$11, %r13
	jne	.LBB31_173
# BB#170:                               # %if.else944
	cmpq	$13, %r15
	jne	.LBB31_173
.LBB31_171:                             # %if.then948
	movq	%r14, %rdi
	movq	%rbx, %rsi
	jmp	.LBB31_172
.LBB31_173:                             # %if.end956
	movq	112(%rsp), %rsi         # 8-byte Reload
	testq	%rsi, %rsi
	je	.LBB31_174
# BB#181:                               # %if.else973
	movabsq	$.L.str173, %rdi
	xorq	%rax, %rax
	jmp	.LBB31_182
.LBB31_110:                             # %comp_target_types.exit1226
	testq	%r12, %rax
	movq	%r15, %r8
	movq	%rbx, %rsi
	jne	.LBB31_112
# BB#111:                               # %lor.lhs.false654
	movq	128(%r14), %rdi
	movq	%r8, 80(%rsp)           # 8-byte Spill
	movq	%rsi, %rbx
	callq	unsigned_type
	movq	128(%r13), %rdi
	movq	%rax, %r15
	callq	unsigned_type
	movq	%rbx, %rsi
	movq	80(%rsp), %r8           # 8-byte Reload
	cmpq	%rax, %r15
	je	.LBB31_112
# BB#151:                               # %if.else890
	movabsq	$.L.str168, %rdi
	jmp	.LBB31_152
.LBB31_112:                             # %if.then662
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB31_113
# BB#119:                               # %land.lhs.true664
	movq	%r8, 80(%rsp)           # 8-byte Spill
	movl	16(%r13), %eax
	movzbl	16(%r14), %ecx
	andq	$255, %rax
	cmpq	$5, %rcx
	jne	.LBB31_121
# BB#120:                               # %land.lhs.true664
	cmpq	$23, %rax
	je	.LBB31_126
.LBB31_121:                             # %lor.lhs.false676
	leaq	16(%r13), %rbx
	cmpq	$5, %rax
	jne	.LBB31_114
# BB#122:                               # %land.lhs.true682
	movq	%rsi, 120(%rsp)         # 8-byte Spill
	movq	72(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	callq	integer_zerop
	testq	%r12, %rax
	je	.LBB31_125
# BB#123:                               # %lor.lhs.false685
	movzbl	16(%r15), %eax
	cmpq	$115, %rax
	jne	.LBB31_124
.LBB31_125:                             # %land.lhs.true691
	movzbl	16(%r14), %eax
	cmpq	$23, %rax
	movq	120(%rsp), %rsi         # 8-byte Reload
	jne	.LBB31_114
.LBB31_126:                             # %if.then697
	movabsq	$.L.str166, %rdi
	jmp	.LBB31_152
.LBB31_113:                             # %if.then662.if.else698_crit_edge
	movq	%r8, 80(%rsp)           # 8-byte Spill
	leaq	16(%r13), %rbx
.LBB31_114:                             # %if.else698
	movq	%rsi, 120(%rsp)         # 8-byte Spill
	movl	(%rbx), %eax
	movl	16(%r14), %edx
	movq	%rax, %rsi
	andq	$255, %rsi
	cmpq	$23, %rsi
	je	.LBB31_144
# BB#115:                               # %land.lhs.true704
	movq	%rdx, %r8
	andq	$255, %r8
	cmpq	$23, %r8
	jne	.LBB31_116
.LBB31_144:                             # %if.else803
	movq	%rdx, %rcx
	andq	$255, %rcx
	cmpq	$23, %rsi
	je	.LBB31_145
# BB#146:                               # %if.else803
	movabsq	$1, %rsi
	jmp	.LBB31_147
.LBB31_145:
	movabsq	$0, %rsi
.LBB31_147:                             # %if.else803
	cmpq	$23, %rcx
	jne	.LBB31_153
# BB#148:                               # %if.else803
	testq	%rsi, %rsi
	jne	.LBB31_153
# BB#149:                               # %land.end878
	andq	%r12, %rdx
	movq	$12, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$1, %rsi
	movq	$10, %rcx
	shrq	%cl, %rdx
	andq	$2, %rdx
	orq	%rsi, %rdx
	movl	60(%r14), %esi
	movq	$19, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	orq	%rdx, %rsi
	movq	$12, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	$10, %rcx
	shrq	%cl, %rax
	movl	60(%r13), %edi
	andq	$2, %rax
	orq	%rdx, %rax
	movq	$19, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	orq	%rax, %rdi
	notq	%rdi
	testq	%rdi, %rsi
	je	.LBB31_153
# BB#150:                               # %if.then885
	movabsq	$.L.str163, %rdi
	jmp	.LBB31_152
.LBB31_85:                              # %if.else541
	movq	$12, %rcx
	movq	%r14, %r11
	shrq	%cl, %r11
	movq	$10, %rcx
	shrq	%cl, %r14
	movl	60(%r9), %edx
	movq	$19, %rcx
	shrq	%cl, %rdx
	xorq	%rdi, %rdi
	cmpq	$20, %rsi
	movabsq	$0, %r10
	jne	.LBB31_89
# BB#86:                                # %land.rhs567
	cmpq	$0, 8(%r9)
	je	.LBB31_87
# BB#88:                                # %land.rhs567
	movabsq	$1, %r10
	jmp	.LBB31_89
.LBB31_116:                             # %if.then710
	movq	$12, %rcx
	movq	%rax, %r9
	shrq	%cl, %r9
	movq	$10, %rcx
	shrq	%cl, %rax
	movl	60(%r13), %r11d
	movq	$19, %rcx
	shrq	%cl, %r11
	xorq	%rdi, %rdi
	cmpq	$20, %rsi
	movabsq	$0, %r10
	jne	.LBB31_128
# BB#117:                               # %land.rhs736
	cmpq	$0, 8(%r13)
	je	.LBB31_118
# BB#127:                               # %land.rhs736
	movabsq	$1, %r10
	jmp	.LBB31_128
.LBB31_174:                             # %if.then958
	movq	104(%rsp), %rbx         # 8-byte Reload
	testq	%rbx, %rbx
	je	.LBB31_180
# BB#175:                               # %if.then960
	callq	maybe_building_objc_message_expr
	testq	%rax, %rax
	movq	96(%rsp), %rsi          # 8-byte Reload
	je	.LBB31_178
# BB#176:                               # %if.then960
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB31_178
# BB#177:                               # %if.then965
	movq	32(%rax), %rdx
	addq	$-2, %rsi
	jmp	.LBB31_179
.LBB31_84:                              # %if.then445
	movq	global_trees+112(%rip), %rdx
	movq	80(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB31_97
.LBB31_124:
	movq	120(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB31_114
.LBB31_87:
	movabsq	$0, %r10
.LBB31_89:                              # %land.end571
	movq	$3, %rcx
	movl	16(%r8), %eax
	shlq	%cl, %r10
	movq	$12, %rcx
	movq	%rax, %r9
	shrq	%cl, %r9
	movq	$10, %rcx
	movq	%rax, %rbx
	movl	60(%r8), %esi
	shrq	%cl, %rbx
	movq	$19, %rcx
	shrq	%cl, %rsi
	andq	$255, %rax
	cmpq	$20, %rax
	jne	.LBB31_93
# BB#90:                                # %land.rhs600
	cmpq	$0, 8(%r8)
	je	.LBB31_91
# BB#92:                                # %land.rhs600
	movabsq	$1, %rdi
	jmp	.LBB31_93
.LBB31_91:
	movabsq	$0, %rdi
.LBB31_93:                              # %land.end604
	andq	$1, %r11
	andq	$2, %r14
	orq	%r11, %r14
	andq	$4, %rdx
	orq	%r14, %rdx
	orq	%r10, %rdx
	andq	$1, %r9
	andq	$2, %rbx
	orq	%r9, %rbx
	andq	$4, %rsi
	orq	%rbx, %rsi
	movq	$3, %rcx
	shlq	%cl, %rdi
	orq	%rsi, %rdi
	notq	%rdi
	andq	%rdx, %rdi
	testq	%r12, %rdi
	je	.LBB31_96
# BB#94:                                # %if.then611
	movabsq	$.L.str164, %rdi
.LBB31_95:                              # %if.end614
	xorq	%rsi, %rsi
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	96(%rsp), %rcx          # 8-byte Reload
	callq	warn_for_assignment
.LBB31_96:                              # %if.end614
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
.LBB31_97:                              # %if.end614
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB31_100
# BB#98:                                # %land.lhs.true616
	movq	88(%rsp), %rax          # 8-byte Reload
	movzbl	50(%rax), %eax
	testq	$1, %rax
	jne	.LBB31_100
# BB#99:                                # %if.then621
	movabsq	$.L.str165, %rdi
	xorq	%rax, %rax
	movq	%rsi, %rbx
	movq	%rdx, %r14
	callq	pedwarn
	movq	%r14, %rdx
	movq	%rbx, %rsi
.LBB31_100:                             # %if.end622
	movq	$115, %rdi
.LBB31_101:                             # %if.end622
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	build1  # TAILCALL
.LBB31_118:
	movabsq	$0, %r10
.LBB31_128:                             # %land.end740
	movq	$3, %rcx
	shlq	%cl, %r10
	andq	%r12, %rdx
	movq	$12, %rcx
	movq	%rdx, %r15
	shrq	%cl, %r15
	movl	60(%r14), %ebx
	movq	$10, %rcx
	shrq	%cl, %rdx
	movq	$19, %rcx
	shrq	%cl, %rbx
	cmpq	$20, %r8
	jne	.LBB31_132
# BB#129:                               # %land.rhs769
	cmpq	$0, 8(%r14)
	je	.LBB31_130
# BB#131:                               # %land.rhs769
	movabsq	$1, %rdi
	jmp	.LBB31_132
.LBB31_130:
	movabsq	$0, %rdi
.LBB31_132:                             # %land.end773
	andq	$1, %r9
	andq	$2, %rax
	orq	%r9, %rax
	andq	$4, %r11
	orq	%rax, %r11
	orq	%r10, %r11
	andq	$1, %r15
	andq	$2, %rdx
	orq	%r15, %rdx
	andq	$4, %rbx
	orq	%rdx, %rbx
	movq	$3, %rcx
	shlq	%cl, %rdi
	orq	%rbx, %rdi
	notq	%rdi
	andq	%r11, %rdi
	testq	%r12, %rdi
	je	.LBB31_134
# BB#133:                               # %if.then780
	movabsq	$.L.str164, %rdi
.LBB31_152:                             # %if.end891
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	96(%rsp), %rcx          # 8-byte Reload
	callq	warn_for_assignment
.LBB31_153:                             # %if.end891
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	72(%rsp), %rsi          # 8-byte Reload
.LBB31_172:                             # %if.then948
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	convert  # TAILCALL
.LBB31_134:                             # %if.else781
	movq	%r12, %r14
	cmpq	$5, %r8
	je	.LBB31_153
# BB#135:                               # %if.else781
	cmpq	$5, %rsi
	je	.LBB31_153
# BB#136:                               # %lor.lhs.false793
	movq	$1, %rdx
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	120(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rsi
	callq	maybe_objc_comptypes
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB31_141
# BB#137:                               # %lor.lhs.false793.comp_target_types.exit1238_crit_edge
	movl	pedantic(%rip), %ecx
.LBB31_138:                             # %comp_target_types.exit1238
	testq	%r14, %rax
	jne	.LBB31_153
# BB#139:                               # %comp_target_types.exit1238
	testq	%r14, %rcx
	je	.LBB31_153
# BB#140:                               # %if.then799
	movabsq	$.L.str167, %rdi
	jmp	.LBB31_152
.LBB31_180:                             # %if.else971
	movabsq	$.L.str172, %rdi
	xorq	%rax, %rax
	movq	96(%rsp), %rsi          # 8-byte Reload
.LBB31_182:                             # %if.end974
	callq	error
	jmp	.LBB31_183
.LBB31_178:                             # %if.else966
	movq	32(%rbx), %rdx
.LBB31_179:                             # %if.end974
	movabsq	$.L.str171, %rdi
	xorq	%rax, %rax
	callq	error
	jmp	.LBB31_183
.LBB31_141:                             # %if.end.i1235
	movq	8(%rbx), %rax
	movq	8(%r15), %rcx
	movq	128(%rax), %rdi
	movq	128(%rcx), %rsi
	callq	comptypes
	movl	pedantic(%rip), %ecx
	movq	%rax, %rdx
	andq	%r14, %rdx
	cmpq	$2, %rdx
	jne	.LBB31_138
# BB#142:                               # %if.end.i1235
	testq	%r14, %rcx
	je	.LBB31_138
# BB#143:                               # %comp_target_types.exit1238.thread
	movabsq	$.L.str182, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	jmp	.LBB31_153
.Ltmp365:
	.size	convert_for_assignment, .Ltmp365-convert_for_assignment
	.cfi_endproc

	.globl	c_convert_parm_for_inlining
	.align	16, 0x90
	.type	c_convert_parm_for_inlining,@function
c_convert_parm_for_inlining:            # @c_convert_parm_for_inlining
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp368:
	.cfi_def_cfa_offset 16
.Ltmp369:
	.cfi_offset %rbx, -16
	movq	%rdx, %rcx
	xorq	%rax, %rax
	testq	%rsi, %rsi
	je	.LBB32_7
# BB#1:                                 # %lor.lhs.false
	movq	8(%rcx), %rax
	cmpq	$0, 24(%rax)
	je	.LBB32_3
# BB#2:
	movq	%rsi, %rax
	popq	%rbx
	retq
.LBB32_3:                               # %if.end
	movq	8(%rdi), %rbx
	movq	72(%rcx), %r8
	xorq	%rdx, %rdx
	xorq	%r9, %r9
	movq	%rbx, %rdi
	callq	convert_for_assignment
	movzbl	target_flags+3(%rip), %ecx
	testq	$2, %rcx
	jne	.LBB32_7
# BB#4:                                 # %land.lhs.true
	movzbl	16(%rbx), %ecx
	cmpq	$12, %rcx
	ja	.LBB32_7
# BB#5:                                 # %land.lhs.true
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$7232, %rdx             # imm = 0x1C40
	je	.LBB32_7
# BB#6:                                 # %land.lhs.true26
	movq	integer_types+40(%rip), %rcx
	movl	60(%rbx), %edx
	movl	60(%rcx), %ecx
	andq	$511, %rdx              # imm = 0x1FF
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rcx, %rdx
	jae	.LBB32_7
# BB#8:                                 # %if.then35
	movq	%rax, %rdi
	popq	%rbx
	jmp	default_conversion  # TAILCALL
.LBB32_7:                               # %return
	popq	%rbx
	retq
.Ltmp370:
	.size	c_convert_parm_for_inlining, .Ltmp370-c_convert_parm_for_inlining
	.cfi_endproc

	.globl	store_init_value
	.align	16, 0x90
	.type	store_init_value,@function
store_init_value:                       # @store_init_value
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp377:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp378:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp379:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp380:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp381:
	.cfi_def_cfa_offset 48
.Ltmp382:
	.cfi_offset %rbx, -48
.Ltmp383:
	.cfi_offset %r12, -40
.Ltmp384:
	.cfi_offset %r13, -32
.Ltmp385:
	.cfi_offset %r14, -24
.Ltmp386:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	8(%r12), %r15
	movzbl	16(%r15), %eax
	movq	%rsi, %r14
	testq	%rax, %rax
	je	.LBB33_17
# BB#1:                                 # %if.end
	movl	16(%r12), %edx
	movq	$18, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	digest_init
	movl	warn_traditional(%rip), %ecx
	movq	%rax, %r13
	testq	%rcx, %rcx
	je	.LBB33_6
# BB#2:                                 # %if.end
	movl	in_system_header(%rip), %ecx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	jne	.LBB33_6
# BB#3:                                 # %land.lhs.true15
	movq	8(%r12), %rcx
	movzbl	16(%rcx), %ecx
	addq	$-18, %rcx
	andq	%rax, %rcx
	cmpq	$4, %rcx
	ja	.LBB33_6
# BB#4:                                 # %land.lhs.true54
	movzbl	18(%r12), %eax
	testq	$4, %rax
	jne	.LBB33_6
# BB#5:                                 # %if.then61
	movabsq	$.L.str110, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB33_6:                               # %if.end62
	movq	%r13, 104(%r12)
	movq	global_trees(%rip), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB33_7:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%r13, %rbx
	movzbl	16(%rbx), %edx
	addq	$-114, %rdx
	andq	%rcx, %rdx
	cmpq	$2, %rdx
	ja	.LBB33_10
# BB#8:                                 # %land.lhs.true81
                                        #   in Loop: Header=BB33_7 Depth=1
	movq	32(%rbx), %r13
	cmpq	%rax, %r13
	je	.LBB33_10
# BB#9:                                 # %land.rhs83
                                        #   in Loop: Header=BB33_7 Depth=1
	movq	8(%rbx), %rdx
	movq	8(%r13), %rsi
	cmpq	%rsi, %rdx
	je	.LBB33_7
.LBB33_10:                              # %while.end
	movq	%rbx, %rdi
	callq	constant_expression_warning
	movzbl	16(%r15), %eax
	cmpq	$18, %rax
	jne	.LBB33_17
# BB#11:                                # %land.lhs.true101
	cmpq	$0, 24(%r15)
	jne	.LBB33_17
# BB#12:                                # %land.lhs.true101
	movq	global_trees(%rip), %rax
	cmpq	%rax, %rbx
	je	.LBB33_17
# BB#13:                                # %if.then106
	movzbl	16(%r14), %eax
	cmpq	$116, %rax
	jne	.LBB33_15
# BB#14:                                # %if.then112
	movq	32(%r14), %r14
.LBB33_15:                              # %if.end116
	movq	%r14, %rdi
	callq	fold
	movzbl	16(%rax), %ecx
	cmpq	$170, %rcx
	jne	.LBB33_17
# BB#16:                                # %if.then123
	movq	32(%rax), %rax
	movq	32(%rax), %rbx
	movq	8(%rbx), %rax
	movq	24(%rax), %rax
	testq	%rax, %rax
	je	.LBB33_17
# BB#18:                                # %if.then136
	movq	%rax, 24(%r15)
	movq	%r15, %rdi
	callq	layout_type
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	layout_decl  # TAILCALL
.LBB33_17:                              # %if.end145
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp387:
	.size	store_init_value, .Ltmp387-store_init_value
	.cfi_endproc

	.globl	error_init
	.align	16, 0x90
	.type	error_init,@function
error_init:                             # @error_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp391:
	.cfi_def_cfa_offset 16
.Ltmp392:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp393:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	pushq	%rax
.Ltmp394:
	.cfi_offset %rbx, -56
.Ltmp395:
	.cfi_offset %r12, -48
.Ltmp396:
	.cfi_offset %r13, -40
.Ltmp397:
	.cfi_offset %r14, -32
.Ltmp398:
	.cfi_offset %r15, -24
	movq	%rdi, %rcx
	movabsq	$.L.str81, %rdi
	xorq	%rax, %rax
	xorq	%r13, %r13
	movq	%rcx, %rsi
	callq	error
	movq	spelling_base(%rip), %r12
	movq	spelling(%rip), %r15
	cmpq	%r15, %r12
	jae	.LBB34_20
# BB#1:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	%r12, %rbx
	.align	16, 0x90
.LBB34_2:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rbx), %eax
	cmpq	$3, %rax
	jne	.LBB34_4
# BB#3:                                 # %if.then.i
                                        #   in Loop: Header=BB34_2 Depth=1
	addq	$25, %r13
	jmp	.LBB34_5
	.align	16, 0x90
.LBB34_4:                               # %if.else.i
                                        #   in Loop: Header=BB34_2 Depth=1
	movq	8(%rbx), %rdi
	callq	strlen
	andq	%r14, %r13
	leaq	1(%r13,%rax), %r13
.LBB34_5:                               # %for.inc.i
                                        #   in Loop: Header=BB34_2 Depth=1
	addq	$16, %rbx
	cmpq	%r15, %rbx
	jb	.LBB34_2
# BB#6:                                 # %spelling_length.exit
	incq	%r13
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	addq	$15, %r13
	andq	$-16, %r13
	movq	%rsp, %r14
	subq	%r13, %r14
	movq	%r14, %rsp
	cmpq	%r15, %r12
	jae	.LBB34_7
# BB#8:
	movq	$46, %r13
	movabsq	$.L.str162, %r15
	movq	%r14, %rbx
	.align	16, 0x90
.LBB34_9:                               # %for.body.i4
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB34_15 Depth 2
	movl	(%r12), %eax
	cmpq	$2, %rax
	jne	.LBB34_10
# BB#12:                                # %if.then5.i
                                        #   in Loop: Header=BB34_9 Depth=1
	movb	%r13b, (%rbx)
	incq	%rbx
	jmp	.LBB34_13
	.align	16, 0x90
.LBB34_10:                              # %for.body.i4
                                        #   in Loop: Header=BB34_9 Depth=1
	cmpq	$3, %rax
	jne	.LBB34_13
# BB#11:                                # %if.then.i6
                                        #   in Loop: Header=BB34_9 Depth=1
	movl	8(%r12), %edx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	sprintf
	movq	%rbx, %rdi
	callq	strlen
	addq	%rax, %rbx
	jmp	.LBB34_16
	.align	16, 0x90
.LBB34_13:                              # %if.end.i
                                        #   in Loop: Header=BB34_9 Depth=1
	movq	8(%r12), %rax
	movzbl	(%rax), %ecx
	testq	%rcx, %rcx
	movb	%cl, (%rbx)
	je	.LBB34_16
# BB#14:                                # %for.inc.i8.preheader
                                        #   in Loop: Header=BB34_9 Depth=1
	incq	%rax
	.align	16, 0x90
.LBB34_15:                              # %for.inc.i8
                                        #   Parent Loop BB34_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %ecx
	incq	%rax
	testq	%rcx, %rcx
	movb	%cl, 1(%rbx)
	leaq	1(%rbx), %rbx
	jne	.LBB34_15
.LBB34_16:                              # %for.inc13.i
                                        #   in Loop: Header=BB34_9 Depth=1
	movq	spelling(%rip), %rax
	addq	$16, %r12
	cmpq	%rax, %r12
	jb	.LBB34_9
	jmp	.LBB34_17
.LBB34_20:                              # %spelling_length.exit.thread
	movq	%rsp, %r14
	addq	$-16, %r14
	movq	%r14, %rsp
	movq	%r14, %rbx
	jmp	.LBB34_17
.LBB34_7:
	movq	%r14, %rbx
.LBB34_17:                              # %print_spelling.exit
	xorq	%rax, %rax
	movb	%al, (%rbx)
	movzbl	(%r14), %eax
	testq	%rax, %rax
	je	.LBB34_19
# BB#18:                                # %if.then
	movabsq	$.L.str111, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	error
.LBB34_19:                              # %if.end
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp399:
	.size	error_init, .Ltmp399-error_init
	.cfi_endproc

	.globl	pedwarn_init
	.align	16, 0x90
	.type	pedwarn_init,@function
pedwarn_init:                           # @pedwarn_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp403:
	.cfi_def_cfa_offset 16
.Ltmp404:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp405:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	pushq	%rax
.Ltmp406:
	.cfi_offset %rbx, -56
.Ltmp407:
	.cfi_offset %r12, -48
.Ltmp408:
	.cfi_offset %r13, -40
.Ltmp409:
	.cfi_offset %r14, -32
.Ltmp410:
	.cfi_offset %r15, -24
	movq	%rdi, %rcx
	movabsq	$.L.str81, %rdi
	xorq	%rax, %rax
	xorq	%r13, %r13
	movq	%rcx, %rsi
	callq	pedwarn
	movq	spelling_base(%rip), %r12
	movq	spelling(%rip), %r15
	cmpq	%r15, %r12
	jae	.LBB35_20
# BB#1:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	%r12, %rbx
	.align	16, 0x90
.LBB35_2:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rbx), %eax
	cmpq	$3, %rax
	jne	.LBB35_4
# BB#3:                                 # %if.then.i
                                        #   in Loop: Header=BB35_2 Depth=1
	addq	$25, %r13
	jmp	.LBB35_5
	.align	16, 0x90
.LBB35_4:                               # %if.else.i
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	8(%rbx), %rdi
	callq	strlen
	andq	%r14, %r13
	leaq	1(%r13,%rax), %r13
.LBB35_5:                               # %for.inc.i
                                        #   in Loop: Header=BB35_2 Depth=1
	addq	$16, %rbx
	cmpq	%r15, %rbx
	jb	.LBB35_2
# BB#6:                                 # %spelling_length.exit
	incq	%r13
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	addq	$15, %r13
	andq	$-16, %r13
	movq	%rsp, %r14
	subq	%r13, %r14
	movq	%r14, %rsp
	cmpq	%r15, %r12
	jae	.LBB35_7
# BB#8:
	movq	$46, %r13
	movabsq	$.L.str162, %r15
	movq	%r14, %rbx
	.align	16, 0x90
.LBB35_9:                               # %for.body.i4
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB35_15 Depth 2
	movl	(%r12), %eax
	cmpq	$2, %rax
	jne	.LBB35_10
# BB#12:                                # %if.then5.i
                                        #   in Loop: Header=BB35_9 Depth=1
	movb	%r13b, (%rbx)
	incq	%rbx
	jmp	.LBB35_13
	.align	16, 0x90
.LBB35_10:                              # %for.body.i4
                                        #   in Loop: Header=BB35_9 Depth=1
	cmpq	$3, %rax
	jne	.LBB35_13
# BB#11:                                # %if.then.i6
                                        #   in Loop: Header=BB35_9 Depth=1
	movl	8(%r12), %edx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	sprintf
	movq	%rbx, %rdi
	callq	strlen
	addq	%rax, %rbx
	jmp	.LBB35_16
	.align	16, 0x90
.LBB35_13:                              # %if.end.i
                                        #   in Loop: Header=BB35_9 Depth=1
	movq	8(%r12), %rax
	movzbl	(%rax), %ecx
	testq	%rcx, %rcx
	movb	%cl, (%rbx)
	je	.LBB35_16
# BB#14:                                # %for.inc.i8.preheader
                                        #   in Loop: Header=BB35_9 Depth=1
	incq	%rax
	.align	16, 0x90
.LBB35_15:                              # %for.inc.i8
                                        #   Parent Loop BB35_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %ecx
	incq	%rax
	testq	%rcx, %rcx
	movb	%cl, 1(%rbx)
	leaq	1(%rbx), %rbx
	jne	.LBB35_15
.LBB35_16:                              # %for.inc13.i
                                        #   in Loop: Header=BB35_9 Depth=1
	movq	spelling(%rip), %rax
	addq	$16, %r12
	cmpq	%rax, %r12
	jb	.LBB35_9
	jmp	.LBB35_17
.LBB35_20:                              # %spelling_length.exit.thread
	movq	%rsp, %r14
	addq	$-16, %r14
	movq	%r14, %rsp
	movq	%r14, %rbx
	jmp	.LBB35_17
.LBB35_7:
	movq	%r14, %rbx
.LBB35_17:                              # %print_spelling.exit
	xorq	%rax, %rax
	movb	%al, (%rbx)
	movzbl	(%r14), %eax
	testq	%rax, %rax
	je	.LBB35_19
# BB#18:                                # %if.then
	movabsq	$.L.str111, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	pedwarn
.LBB35_19:                              # %if.end
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp411:
	.size	pedwarn_init, .Ltmp411-pedwarn_init
	.cfi_endproc

	.globl	start_init
	.align	16, 0x90
	.type	start_init,@function
start_init:                             # @start_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp416:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp417:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp418:
	.cfi_def_cfa_offset 32
.Ltmp419:
	.cfi_offset %rbx, -32
.Ltmp420:
	.cfi_offset %r14, -24
.Ltmp421:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$72, %rdi
	callq	xmalloc
	xorq	%rcx, %rcx
	testq	%rbx, %rbx
	movabsq	$0, %rdx
	je	.LBB36_2
# BB#1:                                 # %if.then
	movq	40(%rbx), %rdx
.LBB36_2:                               # %if.end
	movq	constructor_decl(%rip), %rsi
	movq	%rsi, 8(%rax)
	movq	constructor_asmspec(%rip), %rsi
	movq	%rsi, 16(%rax)
	movzbl	require_constant_value(%rip), %esi
	movb	%sil, 69(%rax)
	movzbl	require_constant_elements(%rip), %esi
	movb	%sil, 70(%rax)
	movq	constructor_stack(%rip), %rsi
	movq	%rsi, 24(%rax)
	movq	constructor_range_stack(%rip), %rsi
	movq	%rsi, 32(%rax)
	movq	constructor_elements(%rip), %rsi
	movq	%rsi, 40(%rax)
	movq	spelling(%rip), %rsi
	movq	%rsi, 48(%rax)
	movq	spelling_base(%rip), %rsi
	movq	%rsi, 56(%rax)
	movl	spelling_size(%rip), %esi
	movl	%esi, 64(%rax)
	movzbl	constructor_subconstants_deferred(%rip), %esi
	movb	%sil, 71(%rax)
	movzbl	constructor_top_level(%rip), %esi
	movb	%sil, 68(%rax)
	movq	initializer_stack(%rip), %rsi
	movq	%rsi, (%rax)
	movq	%rax, initializer_stack(%rip)
	movq	%r15, constructor_decl(%rip)
	testq	%r15, %r15
	movq	%rdx, constructor_asmspec(%rip)
	movl	%ecx, constructor_subconstants_deferred(%rip)
	movl	%ecx, constructor_designated(%rip)
	movl	%r14d, constructor_top_level(%rip)
	je	.LBB36_9
# BB#3:                                 # %if.then8
	movl	16(%r15), %eax
	movq	$18, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, require_constant_value(%rip)
	movzbl	18(%r15), %eax
	testq	$4, %rax
	jne	.LBB36_6
# BB#4:                                 # %lor.lhs.false
	movl	pedantic(%rip), %ecx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB36_8
# BB#5:                                 # %lor.lhs.false
	movl	flag_isoc99(%rip), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	jne	.LBB36_8
.LBB36_6:                               # %land.rhs
	movq	8(%r15), %rax
	movzbl	16(%rax), %ecx
	addq	$-18, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	xorq	%rax, %rax
	cmpq	$4, %rdx
	ja	.LBB36_8
# BB#7:                                 # %switch.lookup
	andq	$31, %rcx
	movq	$29, %rax
	shrq	%cl, %rax
	andq	$1, %rax
.LBB36_8:                               # %land.end
	movl	%eax, require_constant_elements(%rip)
	movq	72(%r15), %rax
	movq	32(%rax), %rbx
	jmp	.LBB36_10
.LBB36_9:                               # %if.else
	xorq	%rax, %rax
	movl	%eax, require_constant_value(%rip)
	movl	%eax, require_constant_elements(%rip)
	movabsq	$.L.str112, %rbx
.LBB36_10:                              # %if.end50
	movq	$0, constructor_stack(%rip)
	movq	$0, constructor_range_stack(%rip)
	xorq	%rax, %rax
	movb	%al, missing_braces_mentioned(%rip)
	movq	$0, spelling_base(%rip)
	movl	%eax, spelling_size(%rip)
	movq	$0, spelling(%rip)
	testq	%rbx, %rbx
	je	.LBB36_12
# BB#11:                                # %push_string.exit
	movq	$10, %rax
	movl	%eax, spelling_size(%rip)
	movq	$160, %rdi
	callq	xmalloc
	movq	%rax, spelling_base(%rip)
	movq	%rax, spelling(%rip)
	movq	$1, %rcx
	movl	%ecx, (%rax)
	movq	%rbx, 8(%rax)
	addq	$16, spelling(%rip)
.LBB36_12:                              # %if.end53
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp422:
	.size	start_init, .Ltmp422-start_init
	.cfi_endproc

	.globl	finish_init
	.align	16, 0x90
	.type	finish_init,@function
finish_init:                            # @finish_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp425:
	.cfi_def_cfa_offset 16
.Ltmp426:
	.cfi_offset %rbx, -16
	movl	constructor_subconstants_deferred(%rip), %eax
	movq	initializer_stack(%rip), %rbx
	testq	%rax, %rax
	je	.LBB37_3
# BB#1:                                 # %if.then
	callq	output_deferred_addressed_constants
	jmp	.LBB37_3
	.align	16, 0x90
.LBB37_2:                               # %while.body
                                        #   in Loop: Header=BB37_3 Depth=1
	movq	(%rdi), %rax
	movq	%rax, constructor_stack(%rip)
	callq	free
.LBB37_3:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	constructor_stack(%rip), %rdi
	testq	%rdi, %rdi
	jne	.LBB37_2
# BB#4:                                 # %while.end
	cmpq	$0, constructor_range_stack(%rip)
	jne	.LBB37_5
# BB#6:                                 # %if.end4
	movq	8(%rbx), %rax
	movq	%rax, constructor_decl(%rip)
	movq	16(%rbx), %rax
	movq	%rax, constructor_asmspec(%rip)
	movsbq	69(%rbx), %rax
	movl	%eax, require_constant_value(%rip)
	movsbq	70(%rbx), %rax
	movl	%eax, require_constant_elements(%rip)
	movq	24(%rbx), %rax
	movq	%rax, constructor_stack(%rip)
	movq	32(%rbx), %rax
	movq	%rax, constructor_range_stack(%rip)
	movq	40(%rbx), %rax
	movq	%rax, constructor_elements(%rip)
	movq	48(%rbx), %rax
	movq	%rax, spelling(%rip)
	movq	56(%rbx), %rax
	movq	%rax, spelling_base(%rip)
	movl	64(%rbx), %eax
	movl	%eax, spelling_size(%rip)
	movsbq	71(%rbx), %rax
	movl	%eax, constructor_subconstants_deferred(%rip)
	movsbq	68(%rbx), %rax
	movl	%eax, constructor_top_level(%rip)
	movq	(%rbx), %rax
	movq	%rax, initializer_stack(%rip)
	movq	%rbx, %rdi
	popq	%rbx
	jmp	free  # TAILCALL
.LBB37_5:                               # %if.then3
	movabsq	$.L.str7, %rax
	movabsq	$.L__FUNCTION__.finish_init, %rcx
	movq	$5151, %rsi             # imm = 0x141F
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp427:
	.size	finish_init, .Ltmp427-finish_init
	.cfi_endproc

	.globl	really_start_incremental_init
	.align	16, 0x90
	.type	really_start_incremental_init,@function
really_start_incremental_init:          # @really_start_incremental_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp430:
	.cfi_def_cfa_offset 16
.Ltmp431:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$112, %rdi
	callq	xmalloc
	testq	%rbx, %rbx
	jne	.LBB38_2
# BB#1:                                 # %if.then
	movq	constructor_decl(%rip), %rcx
	movq	8(%rcx), %rbx
.LBB38_2:                               # %if.end
	movq	constructor_type(%rip), %rcx
	movq	%rcx, 8(%rax)
	movq	constructor_fields(%rip), %rcx
	movq	%rcx, 16(%rax)
	movq	constructor_index(%rip), %rcx
	movq	%rcx, 24(%rax)
	movq	constructor_max_index(%rip), %rcx
	movq	%rcx, 32(%rax)
	movq	constructor_unfilled_index(%rip), %rcx
	movq	%rcx, 40(%rax)
	movq	constructor_unfilled_fields(%rip), %rcx
	movq	%rcx, 48(%rax)
	movq	constructor_bit_index(%rip), %rcx
	movq	%rcx, 56(%rax)
	movq	constructor_elements(%rip), %rcx
	movq	%rcx, 64(%rax)
	movzbl	constructor_constant(%rip), %ecx
	movb	%cl, 104(%rax)
	movzbl	constructor_simple(%rip), %ecx
	movb	%cl, 105(%rax)
	movzbl	constructor_erroneous(%rip), %ecx
	movb	%cl, 107(%rax)
	movq	constructor_pending_elts(%rip), %rcx
	movq	%rcx, 72(%rax)
	movl	constructor_depth(%rip), %ecx
	movl	%ecx, 84(%rax)
	xorq	%rdx, %rdx
	movb	%dl, 106(%rax)
	movb	%dl, 108(%rax)
	movq	$0, 96(%rax)
	movq	$0, 88(%rax)
	movzbl	constructor_incremental(%rip), %ecx
	movb	%cl, 109(%rax)
	movzbl	constructor_designated(%rip), %ecx
	movb	%cl, 110(%rax)
	movq	$0, (%rax)
	movq	%rax, constructor_stack(%rip)
	movq	$1, %rax
	movl	%eax, constructor_constant(%rip)
	movl	%eax, constructor_simple(%rip)
	movq	spelling(%rip), %rsi
	movq	spelling_base(%rip), %rcx
	subq	%rcx, %rsi
	movq	$4, %rcx
	shrq	%cl, %rsi
	movl	%esi, constructor_depth(%rip)
	movq	$0, constructor_elements(%rip)
	movq	$0, constructor_pending_elts(%rip)
	movq	%rbx, constructor_type(%rip)
	movl	%eax, constructor_incremental(%rip)
	movl	%edx, constructor_designated(%rip)
	movl	%edx, designator_depth(%rip)
	movb	%dl, designator_errorneous(%rip)
	movzbl	16(%rbx), %eax
	cmpq	$18, %rax
	je	.LBB38_13
# BB#3:                                 # %if.end
	cmpq	$9, %rax
	je	.LBB38_22
# BB#4:                                 # %if.end
	addq	$-20, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$1, %rcx
	ja	.LBB38_25
# BB#5:                                 # %if.then17
	movq	24(%rbx), %rax
	testq	%rax, %rax
	movq	%rax, constructor_fields(%rip)
	je	.LBB38_6
	.align	16, 0x90
.LBB38_7:                               # %land.lhs.true
                                        # =>This Inner Loop Header: Depth=1
	movzbl	53(%rax), %ecx
	testq	$2, %rcx
	je	.LBB38_8
# BB#9:                                 # %land.end
                                        #   in Loop: Header=BB38_7 Depth=1
	cmpq	$0, 72(%rax)
	jne	.LBB38_10
# BB#11:                                # %while.body
                                        #   in Loop: Header=BB38_7 Depth=1
	movq	(%rax), %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	movq	%rax, constructor_fields(%rip)
	jne	.LBB38_7
	jmp	.LBB38_12
.LBB38_13:                              # %if.then35
	movq	24(%rbx), %rax
	testq	%rax, %rax
	je	.LBB38_20
# BB#14:                                # %if.then38
	movq	112(%rax), %rax
	testq	%rax, %rax
	movq	%rax, constructor_max_index(%rip)
	jne	.LBB38_17
# BB#15:                                # %land.lhs.true44
	cmpq	$0, 32(%rbx)
	je	.LBB38_19
# BB#16:                                # %if.end49
	movq	$-1, %rdi
	movq	$-1, %rsi
	callq	build_int_2_wide
	testq	%rax, %rax
	movq	%rax, constructor_max_index(%rip)
	je	.LBB38_19
.LBB38_17:                              # %land.lhs.true51
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	je	.LBB38_19
# BB#18:                                # %if.then58
	movq	$-1, %rdi
	movq	$-1, %rsi
	callq	build_int_2_wide
	movq	%rax, constructor_max_index(%rip)
.LBB38_19:                              # %if.end60
	movq	constructor_type(%rip), %rax
	movq	24(%rax), %rax
	movq	sizetype_tab+24(%rip), %rdi
	movq	104(%rax), %rsi
	callq	convert
	jmp	.LBB38_21
.LBB38_22:                              # %if.then74
	movl	60(%rbx), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	movzbl	mode_unit_size(%rax), %ecx
	testq	%rcx, %rcx
	movq	$-1, %rdi
	je	.LBB38_24
# BB#23:                                # %cond.false
	movzbl	mode_size(%rax), %eax
	xorq	%rdx, %rdx
	divq	%rcx
	movq	%rax, %rdi
	decq	%rdi
.LBB38_24:                              # %cond.end
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	sizetype_tab+24(%rip), %rdi
	movq	global_trees+88(%rip), %rsi
	movq	%rax, constructor_max_index(%rip)
	callq	convert
	jmp	.LBB38_21
.LBB38_25:                              # %if.else101
	movq	%rbx, constructor_fields(%rip)
	movq	%rbx, constructor_unfilled_fields(%rip)
	popq	%rbx
	retq
.LBB38_6:
	movq	%rax, %rcx
	jmp	.LBB38_12
.LBB38_20:                              # %if.else65
	movq	global_trees+136(%rip), %rax
.LBB38_21:                              # %if.end66
	movq	%rax, constructor_index(%rip)
	movq	%rax, constructor_unfilled_index(%rip)
	popq	%rbx
	retq
.LBB38_8:
	movq	%rax, %rcx
	jmp	.LBB38_12
.LBB38_10:
	movq	%rax, %rcx
.LBB38_12:                              # %while.end
	movq	global_trees+136(%rip), %rax
	movq	%rcx, constructor_unfilled_fields(%rip)
	movq	%rax, constructor_bit_index(%rip)
	popq	%rbx
	retq
.Ltmp432:
	.size	really_start_incremental_init, .Ltmp432-really_start_incremental_init
	.cfi_endproc

	.globl	push_init_level
	.align	16, 0x90
	.type	push_init_level,@function
push_init_level:                        # @push_init_level
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp439:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp440:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp441:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp442:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp443:
	.cfi_def_cfa_offset 48
	subq	$128, %rsp
.Ltmp444:
	.cfi_def_cfa_offset 176
.Ltmp445:
	.cfi_offset %rbx, -48
.Ltmp446:
	.cfi_offset %r12, -40
.Ltmp447:
	.cfi_offset %r13, -32
.Ltmp448:
	.cfi_offset %r14, -24
.Ltmp449:
	.cfi_offset %r15, -16
	movq	constructor_stack(%rip), %rax
	movzbl	106(%rax), %eax
	movq	%rdi, %r12
	testq	%rax, %rax
	je	.LBB39_9
# BB#1:
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB39_2:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	constructor_type(%rip), %rax
	movzbl	16(%rax), %eax
	cmpq	$20, %rax
	jne	.LBB39_3
# BB#7:                                 # %land.lhs.true
                                        #   in Loop: Header=BB39_2 Depth=1
	cmpq	$0, constructor_fields(%rip)
	je	.LBB39_8
	jmp	.LBB39_9
	.align	16, 0x90
.LBB39_3:                               # %lor.lhs.false
                                        #   in Loop: Header=BB39_2 Depth=1
	cmpq	$21, %rax
	jne	.LBB39_5
# BB#4:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB39_2 Depth=1
	movq	constructor_fields(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB39_8
.LBB39_5:                               # %if.else
                                        #   in Loop: Header=BB39_2 Depth=1
	cmpq	$18, %rax
	jne	.LBB39_9
# BB#6:                                 # %land.lhs.true13
                                        #   in Loop: Header=BB39_2 Depth=1
	movq	constructor_max_index(%rip), %rdi
	movq	constructor_index(%rip), %rsi
	callq	tree_int_cst_lt
	testq	%rbx, %rax
	je	.LBB39_9
.LBB39_8:                               # %if.then16
                                        #   in Loop: Header=BB39_2 Depth=1
	movq	$1, %rdi
	callq	pop_init_level
	movq	%rax, %rdi
	callq	process_init_element
	movq	constructor_stack(%rip), %rax
	movzbl	106(%rax), %eax
	testq	%rax, %rax
	jne	.LBB39_2
.LBB39_9:                               # %while.end
	xorq	%r15, %r15
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %r12
	movabsq	$0, %rax
	je	.LBB39_17
# BB#10:                                # %if.then21
	movq	constructor_type(%rip), %rax
	movzbl	16(%rax), %eax
	cmpq	$20, %rax
	jne	.LBB39_11
# BB#15:                                # %land.lhs.true33
	movq	constructor_fields(%rip), %rdi
	testq	%rdi, %rdi
	movabsq	$0, %rax
	jne	.LBB39_16
	jmp	.LBB39_17
.LBB39_11:                              # %lor.lhs.false27
	cmpq	$21, %rax
	jne	.LBB39_13
# BB#12:                                # %lor.lhs.false27
	movq	constructor_fields(%rip), %rdi
	testq	%rdi, %rdi
	jne	.LBB39_16
.LBB39_13:                              # %if.else37
	cmpq	$18, %rax
	movabsq	$0, %rax
	jne	.LBB39_17
# BB#14:                                # %if.then43
	movq	constructor_index(%rip), %rdi
.LBB39_16:                              # %if.end47
	callq	find_init_member
.LBB39_17:                              # %if.end47
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	$112, %rdi
	callq	xmalloc
	movq	constructor_type(%rip), %rsi
	movq	%rsi, 8(%rax)
	movq	constructor_fields(%rip), %r9
	movq	%r9, 16(%rax)
	movq	constructor_index(%rip), %r8
	movq	%r8, 24(%rax)
	movq	constructor_max_index(%rip), %rcx
	movq	%rcx, 32(%rax)
	movq	constructor_unfilled_index(%rip), %rcx
	movq	%rcx, 40(%rax)
	movq	constructor_unfilled_fields(%rip), %rcx
	movq	%rcx, 48(%rax)
	movq	constructor_bit_index(%rip), %rcx
	movq	%rcx, 56(%rax)
	movq	constructor_elements(%rip), %rcx
	movq	%rcx, 64(%rax)
	movzbl	constructor_constant(%rip), %ecx
	movb	%cl, 104(%rax)
	movzbl	constructor_simple(%rip), %ecx
	movb	%cl, 105(%rax)
	movzbl	constructor_erroneous(%rip), %ecx
	movb	%cl, 107(%rax)
	movq	constructor_pending_elts(%rip), %rcx
	movq	%rcx, 72(%rax)
	movl	constructor_depth(%rip), %ecx
	movl	%ecx, 84(%rax)
	movq	$0, 88(%rax)
	movb	%r12b, 106(%rax)
	movb	%r15b, 108(%rax)
	movzbl	constructor_incremental(%rip), %ecx
	movb	%cl, 109(%rax)
	movzbl	constructor_designated(%rip), %ecx
	movb	%cl, 110(%rax)
	movq	constructor_stack(%rip), %rcx
	movq	%rcx, (%rax)
	movq	$0, 96(%rax)
	movq	%rax, constructor_stack(%rip)
	movq	$1, %rdx
	movq	spelling(%rip), %r10
	movq	spelling_base(%rip), %rdi
	movl	%edx, constructor_constant(%rip)
	movl	%edx, constructor_simple(%rip)
	movq	%r10, %r13
	subq	%rdi, %r13
	movq	$4, %rcx
	movq	%r13, %rbx
	shrq	%cl, %rbx
	movq	%r14, %r11
	testq	%r11, %r12
	movl	%ebx, constructor_depth(%rip)
	movq	$0, constructor_elements(%rip)
	movl	%edx, constructor_incremental(%rip)
	movl	%r15d, constructor_designated(%rip)
	movq	$0, constructor_pending_elts(%rip)
	jne	.LBB39_19
# BB#18:                                # %if.then57
	movq	constructor_range_stack(%rip), %rcx
	movq	%rcx, 96(%rax)
	movq	$0, constructor_range_stack(%rip)
	xorq	%rax, %rax
	movl	%eax, designator_depth(%rip)
	movb	%al, designator_errorneous(%rip)
.LBB39_19:                              # %if.end59
	testq	%rsi, %rsi
	je	.LBB39_40
# BB#20:                                # %if.else63
	movzbl	16(%rsi), %eax
	cmpq	$18, %rax
	je	.LBB39_33
# BB#21:                                # %if.else63
	addq	$-20, %rax
	andq	%r11, %rax
	cmpq	$1, %rax
	ja	.LBB39_39
# BB#22:                                # %if.then77
	testq	%r9, %r9
	je	.LBB39_23
# BB#24:                                # %if.else81
	movq	8(%r9), %rax
	movq	%rax, constructor_type(%rip)
	movq	72(%r9), %rax
	testq	%rax, %rax
	je	.LBB39_25
# BB#26:                                # %cond.true.i
	movq	32(%rax), %r15
	jmp	.LBB39_27
.LBB39_33:                              # %if.then92
	movq	%r11, %r14
	movq	8(%rsi), %rax
	movq	%rax, constructor_type(%rip)
	xorq	%rsi, %rsi
	movq	%r8, %rdi
	callq	tree_low_cst
	movq	spelling(%rip), %rdx
	movq	spelling_base(%rip), %rdi
	movq	%rax, %r15
	movq	%rdx, %rbx
	subq	%rdi, %rbx
	movq	$28, %rcx
	movq	%rbx, %rax
	movslq	spelling_size(%rip), %rsi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rsi, %rax
	jl	.LBB39_38
# BB#34:                                # %if.then.i310
	addq	$10, %rsi
	movl	%esi, spelling_size(%rip)
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	testq	%rdi, %rdi
	je	.LBB39_35
# BB#36:                                # %if.else.i313
	callq	xrealloc
	jmp	.LBB39_37
.LBB39_23:                              # %if.then80
	movq	$0, constructor_type(%rip)
	jmp	.LBB39_40
.LBB39_25:
	movabsq	$.L.str154, %r15
.LBB39_27:                              # %cond.end.i
	movq	$32, %rcx
	movslq	spelling_size(%rip), %rsi
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rsi, %rbx
	jl	.LBB39_32
# BB#28:                                # %if.then.i323
	movq	%r11, %rbx
	addq	$10, %rsi
	movl	%esi, spelling_size(%rip)
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	testq	%rdi, %rdi
	je	.LBB39_29
# BB#30:                                # %if.else.i326
	callq	xrealloc
	jmp	.LBB39_31
.LBB39_35:                              # %if.then4.i
	movq	%rsi, %rdi
	callq	xmalloc
.LBB39_37:                              # %if.end.i316
	movq	%rax, spelling_base(%rip)
	movq	$28, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$4, %rcx
	shlq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, spelling(%rip)
	movq	%rbx, %rdx
.LBB39_38:                              # %push_array_bounds.exit
	movq	$3, %rax
	movl	%eax, (%rdx)
	movl	%r15d, 8(%rdx)
	movl	constructor_depth(%rip), %eax
	addq	$16, %rdx
	movq	%rdx, spelling(%rip)
	incq	%rax
	movl	%eax, constructor_depth(%rip)
	movq	%r14, %r11
	jmp	.LBB39_39
.LBB39_29:                              # %if.then7.i
	movq	%rsi, %rdi
	callq	xmalloc
.LBB39_31:                              # %if.end.i332
	movq	%rax, spelling_base(%rip)
	movq	$28, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$4, %rcx
	shlq	%cl, %r13
	addq	%rax, %r13
	movq	%r13, spelling(%rip)
	movq	%r13, %r10
	movq	%rbx, %r11
.LBB39_32:                              # %push_member_name.exit
	movq	$2, %rax
	movl	%eax, (%r10)
	movq	%r15, 8(%r10)
	addq	$16, spelling(%rip)
	movl	constructor_depth(%rip), %eax
	incq	%rax
	movl	%eax, constructor_depth(%rip)
.LBB39_39:                              # %if.end100
	movq	constructor_type(%rip), %rax
	testq	%rax, %rax
	je	.LBB39_40
# BB#41:                                # %if.end104
	movq	104(%rsp), %rbx         # 8-byte Reload
	testq	%rbx, %rbx
	je	.LBB39_47
# BB#42:                                # %land.lhs.true106
	movl	16(%rbx), %edx
	movq	%rdx, %rcx
	andq	$255, %rcx
	cmpq	$46, %rcx
	jne	.LBB39_47
# BB#43:                                # %if.then113
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movl	%edx, constructor_constant(%rip)
	movl	16(%rbx), %edx
	movq	$18, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movl	%edx, constructor_simple(%rip)
	movq	40(%rbx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, constructor_elements(%rip)
	je	.LBB39_47
# BB#44:                                # %land.lhs.true122
	movzbl	16(%rax), %eax
	cmpq	$18, %rax
	je	.LBB39_46
# BB#45:                                # %land.lhs.true122
	cmpq	$20, %rax
	jne	.LBB39_47
.LBB39_46:                              # %if.then136
	movq	%r11, %r14
	callq	set_nonincremental_init
	movq	%r14, %r11
.LBB39_47:                              # %if.end138
	movl	warn_missing_braces(%rip), %eax
	testq	%rax, %rax
	je	.LBB39_51
# BB#48:                                # %if.end138
	andq	%r11, %r12
	cmpq	$1, %r12
	jne	.LBB39_51
# BB#49:                                # %if.end138
	movzbl	missing_braces_mentioned(%rip), %eax
	testq	$1, %rax
	jne	.LBB39_51
# BB#50:                                # %if.then145
	movq	$1, %rax
	movb	%al, missing_braces_mentioned(%rip)
	movabsq	$.L.str114, %rdi
	movq	%r11, %r14
	callq	warning_init
	movq	%r14, %r11
.LBB39_51:                              # %if.end146
	movq	constructor_type(%rip), %rcx
	movzbl	16(%rcx), %eax
	cmpq	$18, %rax
	je	.LBB39_65
# BB#52:                                # %if.end146
	cmpq	$9, %rax
	je	.LBB39_62
# BB#53:                                # %if.end146
	addq	$-20, %rax
	andq	%r11, %rax
	cmpq	$1, %rax
	ja	.LBB39_113
# BB#54:                                # %if.then160
	movq	24(%rcx), %rax
	testq	%rax, %rax
	movq	%rax, constructor_fields(%rip)
	je	.LBB39_55
	.align	16, 0x90
.LBB39_56:                              # %land.lhs.true165
                                        # =>This Inner Loop Header: Depth=1
	movzbl	53(%rax), %ecx
	testq	$2, %rcx
	je	.LBB39_57
# BB#58:                                # %land.end
                                        #   in Loop: Header=BB39_56 Depth=1
	cmpq	$0, 72(%rax)
	jne	.LBB39_59
# BB#60:                                # %while.body174
                                        #   in Loop: Header=BB39_56 Depth=1
	movq	(%rax), %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	movq	%rax, constructor_fields(%rip)
	jne	.LBB39_56
	jmp	.LBB39_61
.LBB39_40:                              # %if.then103
	movabsq	$.L.str113, %rdi
	callq	error_init
	movq	$0, constructor_fields(%rip)
	movq	$0, constructor_unfilled_fields(%rip)
.LBB39_114:                             # %if.end265
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB39_65:                              # %if.then219
	movq	24(%rcx), %rax
	movq	%r11, %r14
	testq	%rax, %rax
	je	.LBB39_72
# BB#66:                                # %if.then223
	movq	112(%rax), %rax
	testq	%rax, %rax
	movq	%rax, constructor_max_index(%rip)
	jne	.LBB39_69
# BB#67:                                # %land.lhs.true229
	cmpq	$0, 32(%rcx)
	je	.LBB39_71
# BB#68:                                # %if.end234
	movq	$-1, %rdi
	movq	$-1, %rsi
	callq	build_int_2_wide
	testq	%rax, %rax
	movq	%rax, constructor_max_index(%rip)
	je	.LBB39_71
.LBB39_69:                              # %land.lhs.true236
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	je	.LBB39_71
# BB#70:                                # %if.then243
	movq	$-1, %rdi
	movq	$-1, %rsi
	callq	build_int_2_wide
	movq	%rax, constructor_max_index(%rip)
.LBB39_71:                              # %if.end245
	movq	constructor_type(%rip), %rax
	movq	24(%rax), %rax
	movq	sizetype_tab+24(%rip), %rdi
	movq	104(%rax), %rsi
	callq	convert
	jmp	.LBB39_73
.LBB39_62:                              # %if.then184
	movl	60(%rcx), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	movzbl	mode_unit_size(%rax), %ecx
	testq	%rcx, %rcx
	movq	$-1, %rdi
	je	.LBB39_64
# BB#63:                                # %cond.false
	movzbl	mode_size(%rax), %eax
	xorq	%rdx, %rdx
	divq	%rcx
	movq	%rax, %rdi
	decq	%rdi
.LBB39_64:                              # %cond.end
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	sizetype_tab+24(%rip), %rdi
	movq	global_trees+88(%rip), %rsi
	movq	%rax, constructor_max_index(%rip)
	callq	convert
	movq	%rax, constructor_index(%rip)
	movq	%rax, constructor_unfilled_index(%rip)
	jmp	.LBB39_114
.LBB39_113:                             # %if.else262
	movabsq	$.L.str115, %rdi
	callq	warning_init
	movq	constructor_type(%rip), %rax
	movq	%rax, constructor_fields(%rip)
	movq	%rax, constructor_unfilled_fields(%rip)
	jmp	.LBB39_114
.LBB39_55:
	movq	%rax, %rcx
	jmp	.LBB39_61
.LBB39_72:                              # %if.else250
	movq	global_trees+136(%rip), %rax
.LBB39_73:                              # %if.end251
	testq	%rbx, %rbx
	movq	%rax, constructor_index(%rip)
	movq	%rax, constructor_unfilled_index(%rip)
	je	.LBB39_114
# BB#74:                                # %land.lhs.true253
	movzbl	16(%rbx), %eax
	cmpq	$29, %rax
	jne	.LBB39_114
# BB#75:                                # %if.then260
	movq	constructor_type(%rip), %rax
	movzbl	16(%rax), %ecx
	cmpq	$18, %rcx
	jne	.LBB39_76
# BB#78:                                # %if.end.i
	movq	8(%rbx), %rcx
	movq	8(%rcx), %rcx
	movq	integer_types(%rip), %rsi
	movl	60(%rcx), %edx
	movl	60(%rsi), %r13d
	andq	$511, %rdx              # imm = 0x1FF
	andq	$511, %r13              # imm = 0x1FF
	cmpq	%r13, %rdx
	movq	$1, %r12
	je	.LBB39_81
# BB#79:                                # %if.else.i
	movq	%rbx, %rdi
	movq	c_global_trees(%rip), %rcx
	movl	60(%rcx), %ecx
	andq	$511, %rcx              # imm = 0x1FF
	movq	%rdx, %rsi
	movq	%r14, %rbx
	andq	%rbx, %rsi
	cmpq	%rcx, %rsi
	jne	.LBB39_115
# BB#80:                                # %if.then27.i
	movq	%rbx, %r14
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rdx
	movq	$3, %rcx
	shrq	%cl, %rdx
	movq	%rdx, %r12
	movq	%rdi, %rbx
.LBB39_81:                              # %if.end34.i
	movq	40(%rbx), %rcx
	movq	%rcx, 96(%rsp)          # 8-byte Spill
	movslq	32(%rbx), %rdx
	addq	%rcx, %rdx
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	cmpq	%rdx, %rcx
	jae	.LBB39_112
# BB#82:                                # %land.rhs.lr.ph.i
	movq	8(%rax), %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	global_trees+136(%rip), %rdx
	movq	%r12, %rsi
	andq	%r14, %rsi
	leaq	-1(%r12), %r8
	movq	%r8, %rax
	imulq	%r13, %rax
	leaq	8(%rax), %rdi
	leaq	7(%rax), %rcx
	movq	$1, %rbx
	shlq	%cl, %rbx
	movq	%rbx, 80(%rsp)          # 8-byte Spill
	movq	$-1, %rbx
	movq	%rdi, %rcx
	shlq	%cl, %rbx
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	leaq	-57(%rax), %rcx
	movq	$1, %rbx
	shlq	%cl, %rbx
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	$-1, %rbx
	addq	$-56, %rax
	movq	%rax, %rcx
	shlq	%cl, %rbx
	movq	$1, 72(%rsp)            # 8-byte Folded Spill
	cmpq	$1, %rsi
	jne	.LBB39_86
# BB#83:                                # %land.rhs.us.i.preheader
	movq	%r14, %r12
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$64, %rdi
	jge	.LBB39_84
.LBB39_93:                              # %land.rhs.us.i
                                        # =>This Inner Loop Header: Depth=1
	movq	constructor_max_index(%rip), %rdi
	movq	%rdx, %rsi
	movq	%rdx, %rbx
	callq	tree_int_cst_lt
	movq	%rbx, %rcx
	testq	%r12, %rax
	jne	.LBB39_112
# BB#94:                                # %if.end58.us.i
                                        #   in Loop: Header=BB39_93 Depth=1
	movq	96(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %edi
	movq	%rdi, 120(%rsp)
	movq	$0, 112(%rsp)
	movq	104(%rsp), %rax         # 8-byte Reload
	movzbl	17(%rax), %eax
	xorq	%rsi, %rsi
	movq	%rcx, %rbx
	testq	$32, %rax
	jne	.LBB39_97
# BB#95:                                # %if.end58.us.i
                                        #   in Loop: Header=BB39_93 Depth=1
	movq	%rdi, %rax
	movq	80(%rsp), %rcx          # 8-byte Reload
	andq	%rcx, %rax
	je	.LBB39_97
# BB#96:                                # %if.then73.us.i
                                        #   in Loop: Header=BB39_93 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	orq	%rax, %rdi
	movq	%rdi, 120(%rsp)
	movq	$-1, 112(%rsp)
	movq	$-1, %rsi
.LBB39_97:                              # %if.end107.us.i
                                        #   in Loop: Header=BB39_93 Depth=1
	movq	96(%rsp), %r14          # 8-byte Reload
	incq	%r14
	movq	%r14, 96(%rsp)          # 8-byte Spill
	callq	build_int_2_wide
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 8(%rax)
	movq	%rbx, %r15
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	add_pending_init
	movq	global_trees+144(%rip), %rdx
	movq	$59, %rdi
	movq	%r15, %rsi
	callq	size_binop
	movq	%rax, %rdx
	movq	88(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r14
	jb	.LBB39_93
	jmp	.LBB39_112
.LBB39_84:                              # %land.rhs.us.i.us
                                        # =>This Inner Loop Header: Depth=1
	movq	constructor_max_index(%rip), %rdi
	movq	%rdx, %rsi
	movq	%rdx, %rbx
	callq	tree_int_cst_lt
	movq	%rbx, %rcx
	testq	%r12, %rax
	jne	.LBB39_112
# BB#85:                                # %if.end107.us.i.us
                                        #   in Loop: Header=BB39_84 Depth=1
	movq	96(%rsp), %r14          # 8-byte Reload
	movzbl	(%r14), %edi
	incq	%r14
	movq	%r14, 96(%rsp)          # 8-byte Spill
	movq	%rdi, 120(%rsp)
	movq	$0, 112(%rsp)
	xorq	%rsi, %rsi
	movq	%rcx, %r15
	callq	build_int_2_wide
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 8(%rax)
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	add_pending_init
	movq	global_trees+144(%rip), %rdx
	movq	$59, %rdi
	movq	%r15, %rsi
	callq	size_binop
	movq	%rax, %rdx
	movq	88(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r14
	jb	.LBB39_84
	jmp	.LBB39_112
.LBB39_57:
	movq	%rax, %rcx
	jmp	.LBB39_61
.LBB39_59:
	movq	%rax, %rcx
.LBB39_61:                              # %while.end176
	movq	global_trees+136(%rip), %rax
	movq	%rcx, constructor_unfilled_fields(%rip)
	movq	%rax, constructor_bit_index(%rip)
	jmp	.LBB39_114
.LBB39_86:                              # %land.rhs.lr.ph.land.rhs.lr.ph.split_crit_edge.i
	cmpq	$1, %rsi
	ja	.LBB39_87
# BB#88:                                # %land.rhs.lr.ph.land.rhs.lr.ph.split_crit_edge.i
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	jmp	.LBB39_89
.LBB39_87:
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	andq	%r14, %r8
	incq	%r8
	movq	%r8, 72(%rsp)           # 8-byte Spill
.LBB39_89:                              # %land.rhs.lr.ph.land.rhs.lr.ph.split_crit_edge.i
	movq	%rdi, %rax
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	andq	%r14, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movabsq	$0, %r15
	leaq	112(%rsp), %r14
.LBB39_90:                              # %land.rhs.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB39_98 Depth 2
	movq	constructor_max_index(%rip), %rdi
	movq	%rdx, %rsi
	movq	%rdx, %rbx
	callq	tree_int_cst_lt
	movq	%rbx, %r9
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB39_112
# BB#91:                                # %if.else46.i
                                        #   in Loop: Header=BB39_90 Depth=1
	xorq	%rdx, %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	movq	$0, 120(%rsp)
	movq	$0, 112(%rsp)
	movq	96(%rsp), %rsi          # 8-byte Reload
	movabsq	$4294967232, %r10       # imm = 0xFFFFFFC0
	movabsq	$1, %r11
	jne	.LBB39_98
# BB#92:                                #   in Loop: Header=BB39_90 Depth=1
	movq	%r9, %rbx
	jmp	.LBB39_102
	.align	16, 0x90
.LBB39_98:                              # %for.body52.i
                                        #   Parent Loop BB39_90 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rdx, %rax
	imulq	%r13, %rax
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movzbl	(%rsi), %r8d
	movq	$57, %rcx
	movq	%rdi, %rbx
	shrq	%cl, %rbx
	andq	$63, %rbx
	addq	%rax, %rbx
	andq	%r10, %rbx
	subq	%rbx, %rax
	movq	%rax, %rcx
	shlq	%cl, %r8
	cmpq	$64, %rdi
	movq	%r15, %rax
	jge	.LBB39_100
# BB#99:                                # %for.body52.i
                                        #   in Loop: Header=BB39_98 Depth=2
	movq	%r11, %rax
.LBB39_100:                             # %for.body52.i
                                        #   in Loop: Header=BB39_98 Depth=2
	incq	%rsi
	movq	$3, %rcx
	shlq	%cl, %rax
	orq	%r14, %rax
	orq	%r8, (%rax)
	incq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r12, %rax
	jl	.LBB39_98
# BB#101:                               # %for.cond49.if.end58.loopexit_crit_edge.i
                                        #   in Loop: Header=BB39_90 Depth=1
	movq	%r9, %rbx
	movq	72(%rsp), %rax          # 8-byte Reload
	addq	%rax, 96(%rsp)          # 8-byte Folded Spill
.LBB39_102:                             # %if.end58.i
                                        #   in Loop: Header=BB39_90 Depth=1
	movq	104(%rsp), %rax         # 8-byte Reload
	movzbl	17(%rax), %eax
	testq	$32, %rax
	jne	.LBB39_111
# BB#103:                               # %if.then63.i
                                        #   in Loop: Header=BB39_90 Depth=1
	movq	$32, %rcx
	movq	64(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$63, %rax
	jg	.LBB39_106
# BB#104:                               # %if.then67.i
                                        #   in Loop: Header=BB39_90 Depth=1
	movq	120(%rsp), %rax
	movq	80(%rsp), %rcx          # 8-byte Reload
	testq	%rcx, %rax
	je	.LBB39_111
# BB#105:                               # %if.then73.i
                                        #   in Loop: Header=BB39_90 Depth=1
	movq	56(%rsp), %rcx          # 8-byte Reload
	orq	%rcx, %rax
	movq	%rax, 120(%rsp)
	movq	$-1, 112(%rsp)
	jmp	.LBB39_111
.LBB39_106:                             # %if.else80.i
                                        #   in Loop: Header=BB39_90 Depth=1
	cmpq	$64, 48(%rsp)           # 8-byte Folded Reload
	jne	.LBB39_109
# BB#107:                               # %if.then83.i
                                        #   in Loop: Header=BB39_90 Depth=1
	cmpq	$0, 120(%rsp)
	jns	.LBB39_111
# BB#108:                               # %if.then87.i
                                        #   in Loop: Header=BB39_90 Depth=1
	movq	$-1, 112(%rsp)
	jmp	.LBB39_111
.LBB39_109:                             # %if.else90.i
                                        #   in Loop: Header=BB39_90 Depth=1
	movq	112(%rsp), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	testq	%rcx, %rax
	je	.LBB39_111
# BB#110:                               # %if.then98.i
                                        #   in Loop: Header=BB39_90 Depth=1
	movq	32(%rsp), %rcx          # 8-byte Reload
	orq	%rcx, %rax
	movq	%rax, 112(%rsp)
.LBB39_111:                             # %if.end107.i
                                        #   in Loop: Header=BB39_90 Depth=1
	movq	112(%rsp), %rsi
	movq	120(%rsp), %rdi
	callq	build_int_2_wide
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 8(%rax)
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	add_pending_init
	movq	global_trees+144(%rip), %rdx
	movq	$59, %rdi
	movq	%rbx, %rsi
	callq	size_binop
	movq	%rax, %rdx
	movq	88(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, 96(%rsp)          # 8-byte Folded Reload
	jb	.LBB39_90
.LBB39_112:                             # %set_nonincremental_init_from_string.exit
	xorq	%rax, %rax
	movl	%eax, constructor_incremental(%rip)
	jmp	.LBB39_114
.LBB39_76:                              # %if.then.i
	movabsq	$.L.str7, %rax
	movabsq	$.L__FUNCTION__.set_nonincremental_init_from_string, %rcx
	movq	$6091, %rsi             # imm = 0x17CB
	jmp	.LBB39_77
.LBB39_115:                             # %if.else32.i
	movabsq	$.L.str7, %rax
	movabsq	$.L__FUNCTION__.set_nonincremental_init_from_string, %rcx
	movq	$6100, %rsi             # imm = 0x17D4
.LBB39_77:                              # %if.then.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp450:
	.size	push_init_level, .Ltmp450-push_init_level
	.cfi_endproc

	.globl	process_init_element
	.align	16, 0x90
	.type	process_init_element,@function
process_init_element:                   # @process_init_element
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp457:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp458:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp459:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp460:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp461:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp462:
	.cfi_def_cfa_offset 80
.Ltmp463:
	.cfi_offset %rbx, -48
.Ltmp464:
	.cfi_offset %r12, -40
.Ltmp465:
	.cfi_offset %r13, -32
.Ltmp466:
	.cfi_offset %r14, -24
.Ltmp467:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB40_1
# BB#4:                                 # %land.end
	movzbl	16(%rbx), %ecx
	xorq	%rax, %rax
	cmpq	$29, %rcx
	jne	.LBB40_5
# BB#6:                                 # %land.end
	movabsq	$1, %rcx
	jmp	.LBB40_7
.LBB40_1:                               # %land.end.thread
	xorq	%rax, %rax
	movl	%eax, designator_depth(%rip)
	movb	%al, designator_errorneous(%rip)
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	jmp	.LBB40_2
.LBB40_5:
	movabsq	$0, %rcx
.LBB40_7:                               # %land.end
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movl	%eax, designator_depth(%rip)
	movb	%al, designator_errorneous(%rip)
	jne	.LBB40_2
# BB#8:                                 # %land.end
	movq	constructor_type(%rip), %rax
	testq	%rax, %rax
	je	.LBB40_2
# BB#9:                                 # %land.lhs.true3
	movzbl	16(%rax), %ecx
	cmpq	$18, %rcx
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	jne	.LBB40_2
# BB#10:                                # %land.lhs.true9
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$6, %rax
	jne	.LBB40_2
# BB#11:                                # %land.lhs.true16
	movq	constructor_unfilled_index(%rip), %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB40_2
# BB#12:                                # %if.then
	movq	constructor_stack(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB40_14
# BB#13:                                # %if.then19
	movabsq	$.L.str128, %rdi
	callq	error_init
	movq	constructor_stack(%rip), %rax
.LBB40_14:                              # %if.end
	movq	%rbx, 88(%rax)
	jmp	.LBB40_15
.LBB40_2:                               # %if.end21
	movq	constructor_stack(%rip), %rcx
	cmpq	$0, 88(%rcx)
	je	.LBB40_16
# BB#3:                                 # %if.then24
	movabsq	$.L.str129, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	error_init  # TAILCALL
.LBB40_16:                              # %if.end25
	movq	constructor_type(%rip), %rax
	testq	%rax, %rax
	je	.LBB40_15
# BB#17:                                # %while.cond.preheader
	movzbl	106(%rcx), %ecx
	testq	%rcx, %rcx
	je	.LBB40_25
# BB#18:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jmp	.LBB40_19
	.align	16, 0x90
.LBB40_39:                              # %while.cond.backedge.while.body_crit_edge
                                        #   in Loop: Header=BB40_19 Depth=1
	movq	constructor_type(%rip), %rax
.LBB40_19:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rax), %eax
	cmpq	$20, %rax
	jne	.LBB40_20
# BB#37:                                # %land.lhs.true40
                                        #   in Loop: Header=BB40_19 Depth=1
	cmpq	$0, constructor_fields(%rip)
	je	.LBB40_38
	jmp	.LBB40_25
	.align	16, 0x90
.LBB40_20:                              # %lor.lhs.false
                                        #   in Loop: Header=BB40_19 Depth=1
	cmpq	$21, %rax
	jne	.LBB40_22
# BB#21:                                # %lor.lhs.false
                                        #   in Loop: Header=BB40_19 Depth=1
	movq	constructor_fields(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB40_38
.LBB40_22:                              # %if.else
                                        #   in Loop: Header=BB40_19 Depth=1
	cmpq	$18, %rax
	jne	.LBB40_25
# BB#23:                                # %land.lhs.true49
                                        #   in Loop: Header=BB40_19 Depth=1
	movq	constructor_max_index(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB40_38
# BB#24:                                # %lor.lhs.false51
                                        #   in Loop: Header=BB40_19 Depth=1
	movq	constructor_index(%rip), %rsi
	callq	tree_int_cst_lt
	testq	%r14, %rax
	je	.LBB40_25
	.align	16, 0x90
.LBB40_38:                              # %if.then54
                                        #   in Loop: Header=BB40_19 Depth=1
	movq	$1, %rdi
	callq	pop_init_level
	movq	%rax, %rdi
	callq	process_init_element
	movq	constructor_stack(%rip), %rax
	movzbl	106(%rax), %eax
	testq	%rax, %rax
	jne	.LBB40_39
.LBB40_25:                              # %while.end
	cmpq	$0, constructor_range_stack(%rip)
	movq	%rbx, %r12
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	je	.LBB40_30
# BB#26:                                # %if.then60
	movl	require_constant_value(%rip), %eax
	testq	%rax, %rax
	je	.LBB40_29
# BB#27:                                # %if.then60
	movq	16(%rsp), %rax          # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$170, %rax
	jne	.LBB40_29
# BB#28:                                # %if.then60
	movl	flag_isoc99(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	16(%rsp), %r12          # 8-byte Reload
	je	.LBB40_30
.LBB40_29:                              # %if.then70
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	save_expr
	movq	%rax, %r12
	jmp	.LBB40_30
	.align	16, 0x90
.LBB40_174:                             # %if.then446
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	%r15, constructor_range_stack(%rip)
.LBB40_30:                              # %while.body75.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB40_31 Depth 2
                                        #     Child Loop BB40_78 Depth 2
                                        #     Child Loop BB40_81 Depth 2
                                        #     Child Loop BB40_153 Depth 2
                                        #     Child Loop BB40_156 Depth 2
                                        #     Child Loop BB40_164 Depth 2
	movabsq	$0, %r13
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movabsq	$2199023255552, %rbx    # imm = 0x20000000000
	jmp	.LBB40_31
	.align	16, 0x90
.LBB40_67:                              # %if.then138
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	$1, %rdi
	callq	push_init_level
.LBB40_31:                              # %while.body75
                                        #   Parent Loop BB40_30 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	constructor_type(%rip), %rsi
	movzbl	16(%rsi), %eax
	cmpq	$17, %rax
	jle	.LBB40_32
# BB#40:                                # %while.body75
                                        #   in Loop: Header=BB40_31 Depth=2
	cmpq	$18, %rax
	je	.LBB40_119
# BB#41:                                # %while.body75
                                        #   in Loop: Header=BB40_31 Depth=2
	cmpq	$21, %rax
	jne	.LBB40_42
# BB#84:                                # %if.then199
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	constructor_fields(%rip), %rax
	testq	%rax, %rax
	je	.LBB40_85
# BB#86:                                # %if.end204
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	%r13, %r15
	movq	8(%rax), %r13
	movq	global_trees(%rip), %rax
	cmpq	%rax, %r13
	je	.LBB40_88
# BB#87:                                # %if.then208
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	128(%r13), %r13
.LBB40_88:                              # %if.end211
                                        #   in Loop: Header=BB40_31 Depth=2
	movl	warn_traditional(%rip), %eax
	movzbl	16(%r13), %ebx
	testq	%rax, %rax
	je	.LBB40_95
# BB#89:                                # %if.end211
                                        #   in Loop: Header=BB40_31 Depth=2
	movl	in_system_header(%rip), %eax
	testq	%r14, %rax
	jne	.LBB40_95
# BB#90:                                # %if.end211
                                        #   in Loop: Header=BB40_31 Depth=2
	movl	constructor_designated(%rip), %eax
	testq	%r14, %rax
	jne	.LBB40_95
# BB#91:                                # %land.lhs.true221
                                        #   in Loop: Header=BB40_31 Depth=2
	testq	%r12, %r12
	je	.LBB40_94
# BB#92:                                # %land.lhs.true223
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	%r12, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	jne	.LBB40_95
# BB#93:                                # %lor.lhs.false226
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	%r12, %rdi
	callq	real_zerop
	testq	%r14, %rax
	jne	.LBB40_95
.LBB40_94:                              # %if.then229
                                        #   in Loop: Header=BB40_31 Depth=2
	xorq	%rax, %rax
	movabsq	$.L.str132, %rdi
	callq	warning
.LBB40_95:                              # %if.end230
                                        #   in Loop: Header=BB40_31 Depth=2
	testq	%r12, %r12
	je	.LBB40_104
# BB#96:                                # %if.end230
                                        #   in Loop: Header=BB40_31 Depth=2
	cmpq	$18, %rbx
	jne	.LBB40_104
# BB#97:                                # %land.lhs.true234
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	8(%r13), %rax
	movzbl	16(%rax), %eax
	cmpq	$6, %rax
	movq	%r15, %rax
	jne	.LBB40_99
# BB#98:                                # %land.lhs.true234
                                        #   in Loop: Header=BB40_31 Depth=2
	movabsq	$1, %rax
.LBB40_99:                              # %land.lhs.true234
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	24(%rsp), %rcx          # 8-byte Reload
	testq	%rcx, %rax
	jne	.LBB40_100
.LBB40_104:                             # %if.else245
                                        #   in Loop: Header=BB40_31 Depth=2
	testq	%r12, %r12
	je	.LBB40_101
# BB#105:                               # %if.else245
                                        #   in Loop: Header=BB40_31 Depth=2
	movl	constructor_no_implicit(%rip), %eax
	testq	%r14, %rax
	jne	.LBB40_101
# BB#106:                               # %if.else245
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	global_trees(%rip), %rax
	cmpq	%rax, %r12
	je	.LBB40_101
# BB#107:                               # %land.lhs.true251
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	8(%r12), %rax
	movq	128(%rax), %rax
	cmpq	%r13, %rax
	je	.LBB40_101
# BB#108:                               # %land.lhs.true257
                                        #   in Loop: Header=BB40_31 Depth=2
	cmpq	$21, %rbx
	ja	.LBB40_101
# BB#109:                               # %land.lhs.true257
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	testq	$3407872, %rax          # imm = 0x340000
	je	.LBB40_101
# BB#110:                               # %if.then263
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	$1, %rdi
	callq	push_init_level
	movabsq	$2199023255552, %rbx    # imm = 0x20000000000
	movq	%r15, %r13
	jmp	.LBB40_31
.LBB40_119:                             # %if.then282
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	8(%rsi), %rax
	movq	128(%rax), %r15
	movzbl	16(%r15), %ecx
	testq	%r12, %r12
	je	.LBB40_125
# BB#120:                               # %if.then282
                                        #   in Loop: Header=BB40_31 Depth=2
	cmpq	$18, %rcx
	jne	.LBB40_125
# BB#121:                               # %land.lhs.true294
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	8(%r15), %rax
	movzbl	16(%rax), %eax
	cmpq	$6, %rax
	movq	%r13, %rax
	jne	.LBB40_123
# BB#122:                               # %land.lhs.true294
                                        #   in Loop: Header=BB40_31 Depth=2
	movabsq	$1, %rax
.LBB40_123:                             # %land.lhs.true294
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	24(%rsp), %rdx          # 8-byte Reload
	testq	%rdx, %rax
	jne	.LBB40_124
.LBB40_125:                             # %if.else305
                                        #   in Loop: Header=BB40_31 Depth=2
	testq	%r12, %r12
	je	.LBB40_131
# BB#126:                               # %if.else305
                                        #   in Loop: Header=BB40_31 Depth=2
	movl	constructor_no_implicit(%rip), %eax
	testq	%r14, %rax
	jne	.LBB40_131
# BB#127:                               # %if.else305
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	global_trees(%rip), %rax
	cmpq	%rax, %r12
	je	.LBB40_131
# BB#128:                               # %land.lhs.true311
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	8(%r12), %rax
	movq	128(%rax), %rax
	cmpq	%r15, %rax
	je	.LBB40_131
# BB#129:                               # %land.lhs.true317
                                        #   in Loop: Header=BB40_31 Depth=2
	cmpq	$21, %rcx
	ja	.LBB40_131
# BB#130:                               # %land.lhs.true317
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$3407872, %rax          # imm = 0x340000
	jne	.LBB40_67
	jmp	.LBB40_131
.LBB40_42:                              # %while.body75
                                        #   in Loop: Header=BB40_31 Depth=2
	cmpq	$20, %rax
	jne	.LBB40_146
# BB#43:                                # %if.then81
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	constructor_fields(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB40_44
# BB#45:                                # %if.end84
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	8(%rdi), %r15
	movq	global_trees(%rip), %rax
	cmpq	%rax, %r15
	je	.LBB40_47
# BB#46:                                # %if.then88
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	128(%r15), %r15
.LBB40_47:                              # %if.end90
                                        #   in Loop: Header=BB40_31 Depth=2
	movzbl	16(%r15), %ecx
	cmpq	$18, %rcx
	jne	.LBB40_52
# BB#48:                                # %if.end90
                                        #   in Loop: Header=BB40_31 Depth=2
	movl	require_constant_value(%rip), %edx
	testq	%r14, %rdx
	jne	.LBB40_52
# BB#49:                                # %land.lhs.true98
                                        #   in Loop: Header=BB40_31 Depth=2
	cmpq	$0, 32(%r15)
	jne	.LBB40_52
# BB#50:                                # %land.lhs.true101
                                        #   in Loop: Header=BB40_31 Depth=2
	cmpq	$0, (%rdi)
	je	.LBB40_51
.LBB40_52:                              # %if.end105
                                        #   in Loop: Header=BB40_31 Depth=2
	testq	%r12, %r12
	je	.LBB40_61
# BB#53:                                # %if.end105
                                        #   in Loop: Header=BB40_31 Depth=2
	cmpq	$18, %rcx
	jne	.LBB40_61
# BB#54:                                # %land.lhs.true109
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	8(%r15), %rdx
	movzbl	16(%rdx), %edx
	cmpq	$6, %rdx
	movq	%r13, %rdx
	jne	.LBB40_56
# BB#55:                                # %land.lhs.true109
                                        #   in Loop: Header=BB40_31 Depth=2
	movabsq	$1, %rdx
.LBB40_56:                              # %land.lhs.true109
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	24(%rsp), %rsi          # 8-byte Reload
	testq	%rsi, %rdx
	jne	.LBB40_57
.LBB40_61:                              # %if.else120
                                        #   in Loop: Header=BB40_31 Depth=2
	testq	%r12, %r12
	je	.LBB40_58
# BB#62:                                # %if.else120
                                        #   in Loop: Header=BB40_31 Depth=2
	movl	constructor_no_implicit(%rip), %edx
	testq	%r14, %rdx
	jne	.LBB40_58
# BB#63:                                # %if.else120
                                        #   in Loop: Header=BB40_31 Depth=2
	cmpq	%rax, %r12
	je	.LBB40_58
# BB#64:                                # %land.lhs.true126
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	8(%r12), %rax
	movq	128(%rax), %rax
	cmpq	%r15, %rax
	je	.LBB40_59
# BB#65:                                # %land.lhs.true132
                                        #   in Loop: Header=BB40_31 Depth=2
	cmpq	$21, %rcx
	ja	.LBB40_58
# BB#66:                                # %land.lhs.true132
                                        #   in Loop: Header=BB40_31 Depth=2
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$3407872, %rax          # imm = 0x340000
	jne	.LBB40_67
	jmp	.LBB40_58
	.align	16, 0x90
.LBB40_32:                              # %while.body75
                                        #   in Loop: Header=BB40_30 Depth=1
	cmpq	$9, %rax
	jne	.LBB40_146
# BB#33:                                # %if.then352
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	8(%rsi), %rax
	movq	128(%rax), %rbx
	movq	constructor_max_index(%rip), %rdi
	movq	constructor_index(%rip), %rsi
	callq	tree_int_cst_lt
	testq	%r14, %rax
	jne	.LBB40_34
# BB#144:                               # %if.end361
                                        #   in Loop: Header=BB40_30 Depth=1
	testq	%r12, %r12
	je	.LBB40_142
# BB#145:                               # %if.then363
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	constructor_index(%rip), %rdx
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	output_init_element
.LBB40_142:                             # %if.end340
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	constructor_index(%rip), %rsi
	movq	global_trees+144(%rip), %rdx
	movq	$59, %rdi
	callq	size_binop
	testq	%r12, %r12
	movq	%rax, constructor_index(%rip)
	jne	.LBB40_151
# BB#143:                               # %if.then343
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	%rax, constructor_unfilled_index(%rip)
	xorq	%r12, %r12
	jmp	.LBB40_151
	.align	16, 0x90
.LBB40_146:                             # %if.else369
                                        #   in Loop: Header=BB40_30 Depth=1
	cmpq	$0, constructor_fields(%rip)
	je	.LBB40_147
# BB#148:                               # %if.else373
                                        #   in Loop: Header=BB40_30 Depth=1
	testq	%r12, %r12
	je	.LBB40_150
# BB#149:                               # %if.then375
                                        #   in Loop: Header=BB40_30 Depth=1
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%r12, %rdi
	callq	output_init_element
.LBB40_150:                             # %if.end376
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	$0, constructor_fields(%rip)
.LBB40_151:                             # %if.end381
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	constructor_range_stack(%rip), %r15
	testq	%r15, %r15
	je	.LBB40_36
# BB#152:                               # %if.then383
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	$0, constructor_range_stack(%rip)
	movq	constructor_stack(%rip), %rax
	movq	16(%r15), %rcx
	cmpq	%rcx, %rax
	movq	%r15, %rbx
	je	.LBB40_156
	.align	16, 0x90
.LBB40_153:                             # %while.body387
                                        #   Parent Loop BB40_30 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	106(%rax), %eax
	testq	%rax, %rax
	je	.LBB40_175
# BB#154:                               # %if.end391
                                        #   in Loop: Header=BB40_153 Depth=2
	movq	$1, %rdi
	callq	pop_init_level
	movq	%rax, %rdi
	callq	process_init_element
	movq	constructor_stack(%rip), %rax
	movq	16(%r15), %rcx
	cmpq	%rcx, %rax
	jne	.LBB40_153
# BB#155:                               #   in Loop: Header=BB40_30 Depth=1
	movq	%r15, %rbx
	jmp	.LBB40_156
	.align	16, 0x90
.LBB40_159:                             # %if.end401
                                        #   in Loop: Header=BB40_156 Depth=2
	movq	$1, %rdi
	callq	pop_init_level
	movq	%rax, %rdi
	callq	process_init_element
	movq	8(%rbx), %rbx
.LBB40_156:                             # %for.cond
                                        #   Parent Loop BB40_30 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	40(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB40_158
# BB#157:                               # %lor.rhs
                                        #   in Loop: Header=BB40_156 Depth=2
	movq	32(%rbx), %rdi
	callq	tree_int_cst_equal
	testq	%r14, %rax
	je	.LBB40_160
.LBB40_158:                             # %for.body
                                        #   in Loop: Header=BB40_156 Depth=2
	movq	constructor_stack(%rip), %rax
	movzbl	106(%rax), %eax
	testq	%rax, %rax
	jne	.LBB40_159
	jmp	.LBB40_176
	.align	16, 0x90
.LBB40_160:                             # %for.end
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	32(%rbx), %rsi
	movq	global_trees+144(%rip), %rdx
	movq	$59, %rdi
	callq	size_binop
	movq	%rax, 32(%rbx)
	movq	40(%rbx), %rsi
	movq	%rax, %rdi
	callq	tree_int_cst_equal
	xorq	%rcx, %rcx
	testq	%r14, %rax
	je	.LBB40_164
# BB#161:                               # %land.lhs.true410
                                        #   in Loop: Header=BB40_30 Depth=1
	cmpq	$0, 8(%rbx)
	jne	.LBB40_162
# BB#163:                               # %land.lhs.true410
                                        #   in Loop: Header=BB40_30 Depth=1
	movabsq	$1, %rcx
	jmp	.LBB40_164
	.align	16, 0x90
.LBB40_162:                             #   in Loop: Header=BB40_30 Depth=1
	movabsq	$0, %rcx
	jmp	.LBB40_164
	.align	16, 0x90
.LBB40_172:                             # %if.then440
                                        #   in Loop: Header=BB40_164 Depth=2
	movq	24(%rbx), %rax
	movq	%rax, 32(%rbx)
	movq	%r13, %rcx
.LBB40_164:                             # %while.body416
                                        #   Parent Loop BB40_30 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	32(%rbx), %rax
	movq	%rax, constructor_index(%rip)
	movq	48(%rbx), %rax
	movq	%rax, constructor_fields(%rip)
	testq	%r14, %rcx
	movabsq	$0, %r13
	je	.LBB40_169
# BB#165:                               # %land.lhs.true419
                                        #   in Loop: Header=BB40_164 Depth=2
	cmpq	$0, 40(%rbx)
	jne	.LBB40_167
# BB#166:                               #   in Loop: Header=BB40_164 Depth=2
	movq	%rcx, %r13
	jmp	.LBB40_169
	.align	16, 0x90
.LBB40_167:                             # %land.lhs.true422
                                        #   in Loop: Header=BB40_164 Depth=2
	movq	24(%rbx), %rax
	movq	32(%rbx), %rdx
	cmpq	%rax, %rdx
	movq	%rcx, %r13
	jne	.LBB40_169
# BB#168:                               # %if.then426
                                        #   in Loop: Header=BB40_164 Depth=2
	movq	$0, 8(%rbx)
	xorq	%r13, %r13
	.align	16, 0x90
.LBB40_169:                             # %if.end428
                                        #   in Loop: Header=BB40_164 Depth=2
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB40_173
# BB#170:                               # %if.end431
                                        #   in Loop: Header=BB40_164 Depth=2
	movq	$2, %rdi
	callq	push_init_level
	movq	constructor_stack(%rip), %rax
	movq	%rax, 16(%rbx)
	movq	40(%rbx), %rsi
	testq	%rsi, %rsi
	movq	%r13, %rcx
	je	.LBB40_164
# BB#171:                               # %land.lhs.true435
                                        #   in Loop: Header=BB40_164 Depth=2
	movq	32(%rbx), %rdi
	callq	tree_int_cst_equal
	testq	%r14, %rax
	movq	%r13, %rcx
	je	.LBB40_164
	jmp	.LBB40_172
	.align	16, 0x90
.LBB40_173:                             # %while.end444
                                        #   in Loop: Header=BB40_30 Depth=1
	testq	%r14, %r13
	jne	.LBB40_30
	jmp	.LBB40_174
.LBB40_124:                             #   in Loop: Header=BB40_30 Depth=1
	movq	16(%rsp), %r12          # 8-byte Reload
.LBB40_131:                             # %if.end325
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	constructor_max_index(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB40_135
# BB#132:                               # %land.lhs.true327
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	constructor_index(%rip), %rsi
	callq	tree_int_cst_lt
	testq	%r14, %rax
	jne	.LBB40_134
# BB#133:                               # %lor.lhs.false330
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	constructor_max_index(%rip), %rdi
	callq	integer_all_onesp
	testq	%r14, %rax
	jne	.LBB40_134
.LBB40_135:                             # %if.end334
                                        #   in Loop: Header=BB40_30 Depth=1
	testq	%r12, %r12
	je	.LBB40_142
# BB#136:                               # %if.then336
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	constructor_index(%rip), %rdi
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	spelling(%rip), %rsi
	movq	spelling_base(%rip), %rdi
	movq	%rax, %rbx
	movq	%rsi, %r13
	subq	%rdi, %r13
	movq	$28, %rcx
	movq	%r13, %rdx
	movslq	spelling_size(%rip), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB40_141
# BB#137:                               # %if.then.i626
                                        #   in Loop: Header=BB40_30 Depth=1
	addq	$10, %rax
	movl	%eax, spelling_size(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	testq	%rdi, %rdi
	jne	.LBB40_139
# BB#138:                               # %if.then4.i
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	%rax, %rdi
	callq	xmalloc
	jmp	.LBB40_140
.LBB40_139:                             # %if.else.i629
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	%rax, %rsi
	callq	xrealloc
.LBB40_140:                             # %if.end.i635
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	%rax, spelling_base(%rip)
	movq	$28, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$4, %rcx
	shlq	%cl, %r13
	addq	%rax, %r13
	movq	%r13, spelling(%rip)
	movq	%r13, %rsi
.LBB40_141:                             # %push_array_bounds.exit
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	$3, %rax
	movl	%eax, (%rsi)
	movl	%ebx, 8(%rsi)
	movq	constructor_index(%rip), %rdx
	addq	$16, %rsi
	movq	%rsi, spelling(%rip)
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	output_init_element
	movslq	constructor_depth(%rip), %rax
	movq	spelling_base(%rip), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	addq	%rdx, %rax
	movq	%rax, spelling(%rip)
	jmp	.LBB40_142
.LBB40_100:                             #   in Loop: Header=BB40_30 Depth=1
	movq	16(%rsp), %r12          # 8-byte Reload
.LBB40_101:                             # %if.end265
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	constructor_fields(%rip), %rax
	testq	%r12, %r12
	jne	.LBB40_102
# BB#118:                               # %if.else270
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	40(%rax), %rcx
	movq	%rcx, constructor_bit_index(%rip)
	movq	(%rax), %rax
	movq	%rax, constructor_unfilled_fields(%rip)
	jmp	.LBB40_150
.LBB40_102:                             # %if.then267
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	72(%rax), %rax
	testq	%rax, %rax
	jne	.LBB40_111
# BB#103:                               #   in Loop: Header=BB40_30 Depth=1
	movabsq	$.L.str154, %r15
	jmp	.LBB40_112
.LBB40_111:                             # %cond.true.i590
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	32(%rax), %r15
.LBB40_112:                             # %cond.end.i598
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	spelling(%rip), %rdx
	movq	spelling_base(%rip), %rdi
	movq	%rdx, %rbx
	subq	%rdi, %rbx
	movq	$28, %rcx
	movq	%rbx, %rsi
	movslq	spelling_size(%rip), %rax
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rax, %rsi
	jl	.LBB40_117
# BB#113:                               # %if.then.i601
                                        #   in Loop: Header=BB40_30 Depth=1
	addq	$10, %rax
	movl	%eax, spelling_size(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	testq	%rdi, %rdi
	jne	.LBB40_115
# BB#114:                               # %if.then7.i605
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	%rax, %rdi
	callq	xmalloc
	jmp	.LBB40_116
.LBB40_115:                             # %if.else.i609
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	%rax, %rsi
	callq	xrealloc
.LBB40_116:                             # %if.end.i615
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	%rax, spelling_base(%rip)
	movq	$28, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$4, %rcx
	shlq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, spelling(%rip)
	movq	%rbx, %rdx
.LBB40_117:                             # %push_member_name.exit619
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	$2, %rax
	movl	%eax, (%rdx)
	movq	%r15, 8(%rdx)
	addq	$16, spelling(%rip)
	movq	constructor_fields(%rip), %rdx
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	output_init_element
	movslq	constructor_depth(%rip), %rax
	movq	spelling_base(%rip), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	addq	%rdx, %rax
	movq	%rax, spelling(%rip)
	jmp	.LBB40_150
.LBB40_57:                              #   in Loop: Header=BB40_30 Depth=1
	movq	16(%rsp), %r12          # 8-byte Reload
.LBB40_58:                              # %if.end140
                                        #   in Loop: Header=BB40_30 Depth=1
	testq	%r12, %r12
	jne	.LBB40_59
# BB#75:                                # %if.else143
                                        #   in Loop: Header=BB40_30 Depth=1
	cmpq	$0, 40(%rdi)
	je	.LBB40_77
# BB#76:                                # %if.then146
                                        #   in Loop: Header=BB40_30 Depth=1
	callq	bit_position
	movq	constructor_fields(%rip), %rcx
	movq	40(%rcx), %rdx
	movq	$59, %rdi
	movq	%rax, %rsi
	callq	size_binop
	movq	constructor_fields(%rip), %rdi
	movq	%rax, constructor_bit_index(%rip)
.LBB40_77:                              # %if.end151
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	%rdi, %rax
	.align	16, 0x90
.LBB40_78:                              # %while.cond154
                                        #   Parent Loop BB40_30 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	movq	%rax, constructor_unfilled_fields(%rip)
	je	.LBB40_81
# BB#79:                                # %land.lhs.true156
                                        #   in Loop: Header=BB40_78 Depth=2
	movq	48(%rax), %rcx
	testq	%rbx, %rcx
	je	.LBB40_81
# BB#80:                                # %land.rhs161
                                        #   in Loop: Header=BB40_78 Depth=2
	cmpq	$0, 72(%rax)
	je	.LBB40_78
	jmp	.LBB40_81
.LBB40_59:                              # %if.then142
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	72(%rdi), %rax
	testq	%rax, %rax
	jne	.LBB40_68
# BB#60:                                #   in Loop: Header=BB40_30 Depth=1
	movabsq	$.L.str154, %rbx
	jmp	.LBB40_69
.LBB40_68:                              # %cond.true.i
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	32(%rax), %rbx
.LBB40_69:                              # %cond.end.i
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	spelling(%rip), %rdx
	movq	spelling_base(%rip), %rdi
	movq	%rdx, %r13
	subq	%rdi, %r13
	movq	$28, %rcx
	movq	%r13, %rsi
	movslq	spelling_size(%rip), %rax
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rax, %rsi
	jl	.LBB40_74
# BB#70:                                # %if.then.i
                                        #   in Loop: Header=BB40_30 Depth=1
	addq	$10, %rax
	movl	%eax, spelling_size(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	testq	%rdi, %rdi
	jne	.LBB40_72
# BB#71:                                # %if.then7.i
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	%rax, %rdi
	callq	xmalloc
	jmp	.LBB40_73
.LBB40_72:                              # %if.else.i
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	%rax, %rsi
	callq	xrealloc
.LBB40_73:                              # %if.end.i
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	%rax, spelling_base(%rip)
	movq	$28, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$4, %rcx
	shlq	%cl, %r13
	addq	%rax, %r13
	movq	%r13, spelling(%rip)
	movq	%r13, %rdx
.LBB40_74:                              # %push_member_name.exit
                                        #   in Loop: Header=BB40_30 Depth=1
	movq	$2, %rax
	movl	%eax, (%rdx)
	movq	%rbx, 8(%rdx)
	addq	$16, spelling(%rip)
	movq	constructor_fields(%rip), %rdx
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	output_init_element
	movslq	constructor_depth(%rip), %rax
	movq	spelling_base(%rip), %rdx
	movq	$4, %rcx
	movq	constructor_fields(%rip), %rdi
	shlq	%cl, %rax
	addq	%rdx, %rax
	movq	%rax, spelling(%rip)
	movabsq	$2199023255552, %rbx    # imm = 0x20000000000
	.align	16, 0x90
.LBB40_81:                              # %while.cond173
                                        #   Parent Loop BB40_30 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	movq	%rdi, constructor_fields(%rip)
	je	.LBB40_151
# BB#82:                                # %land.lhs.true175
                                        #   in Loop: Header=BB40_81 Depth=2
	movq	48(%rdi), %rax
	testq	%rbx, %rax
	je	.LBB40_151
# BB#83:                                # %land.rhs183
                                        #   in Loop: Header=BB40_81 Depth=2
	cmpq	$0, 72(%rdi)
	je	.LBB40_81
	jmp	.LBB40_151
.LBB40_147:                             # %if.then372
	movabsq	$.L.str135, %rdi
	jmp	.LBB40_35
.LBB40_34:                              # %if.then360
	movabsq	$.L.str134, %rdi
	jmp	.LBB40_35
.LBB40_85:                              # %if.then203
	movabsq	$.L.str131, %rdi
	jmp	.LBB40_35
.LBB40_44:                              # %if.then83
	movabsq	$.L.str129, %rdi
	jmp	.LBB40_35
.LBB40_134:                             # %if.then333
	movabsq	$.L.str133, %rdi
.LBB40_35:                              # %while.end449
	callq	pedwarn_init
.LBB40_36:                              # %while.end449
	movq	$0, constructor_range_stack(%rip)
.LBB40_15:                              # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB40_51:                              # %if.then104
	movabsq	$.L.str130, %rdi
	callq	error_init
	movq	$0, constructor_range_stack(%rip)
	jmp	.LBB40_15
.LBB40_176:                             # %if.then400
	movabsq	$.L.str7, %rdi
	movabsq	$.L__FUNCTION__.process_init_element, %rdx
	movq	$6838, %rsi             # imm = 0x1AB6
	callq	fancy_abort
.LBB40_175:                             # %if.then390
	movabsq	$.L.str7, %rax
	movabsq	$.L__FUNCTION__.process_init_element, %rcx
	movq	$6830, %rsi             # imm = 0x1AAE
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp468:
	.size	process_init_element, .Ltmp468-process_init_element
	.cfi_endproc

	.globl	pop_init_level
	.align	16, 0x90
	.type	pop_init_level,@function
pop_init_level:                         # @pop_init_level
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp474:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp475:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp476:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp477:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp478:
	.cfi_def_cfa_offset 48
.Ltmp479:
	.cfi_offset %rbx, -40
.Ltmp480:
	.cfi_offset %r12, -32
.Ltmp481:
	.cfi_offset %r14, -24
.Ltmp482:
	.cfi_offset %r15, -16
	movq	constructor_stack(%rip), %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rdi
	jne	.LBB41_1
	jmp	.LBB41_9
	.align	16, 0x90
.LBB41_8:                               # %while.body
                                        #   in Loop: Header=BB41_9 Depth=1
	movq	$1, %rdi
	callq	pop_init_level
	movq	%rax, %rdi
	callq	process_init_element
	movq	constructor_stack(%rip), %rbx
.LBB41_9:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	106(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB41_8
# BB#10:                                # %while.end
	cmpq	$0, constructor_range_stack(%rip)
	jne	.LBB41_11
.LBB41_1:                               # %if.end4
	movq	constructor_type(%rip), %rax
	testq	%rax, %rax
	je	.LBB41_20
# BB#2:                                 # %if.end4
	movq	constructor_fields(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB41_20
# BB#3:                                 # %land.lhs.true7
	movzbl	16(%rax), %ecx
	cmpq	$18, %rcx
	jne	.LBB41_20
# BB#4:                                 # %land.lhs.true9
	movq	24(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB41_20
# BB#5:                                 # %land.lhs.true11
	cmpq	$0, 112(%rcx)
	jne	.LBB41_20
# BB#6:                                 # %if.then16
	movq	constructor_unfilled_index(%rip), %rdi
	callq	integer_zerop
	testq	%r12, %rax
	je	.LBB41_13
# BB#7:                                 # %if.then19
	movq	$0, constructor_type(%rip)
	jmp	.LBB41_40
.LBB41_13:                              # %if.else
	movq	constructor_type(%rip), %rax
	cmpq	$0, 32(%rax)
	jne	.LBB41_28
# BB#14:                                # %if.then22
	movslq	constructor_depth(%rip), %rax
	cmpq	$3, %rax
	jl	.LBB41_16
# BB#15:                                # %if.then24
	movabsq	$.L.str116, %rdi
	callq	error_init
	jmp	.LBB41_18
.LBB41_16:                              # %if.else25
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB41_18
# BB#17:                                # %if.then27
	movabsq	$.L.str117, %rdi
	callq	pedwarn_init
.LBB41_18:                              # %if.end29
	movq	constructor_fields(%rip), %rax
	cmpq	$0, (%rax)
	je	.LBB41_19
# BB#27:                                # %if.then32
	movq	$0, constructor_type(%rip)
	jmp	.LBB41_40
.LBB41_19:                              # %if.end29.if.end37_crit_edge
	movq	constructor_type(%rip), %rax
.LBB41_20:                              # %if.end37
	movl	extra_warnings(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB41_40
# BB#21:                                # %if.end37
	testq	%rax, %rax
	je	.LBB41_40
# BB#22:                                # %land.lhs.true41
	movzbl	16(%rax), %eax
	cmpq	$20, %rax
	jne	.LBB41_40
# BB#23:                                # %land.lhs.true41
	movq	constructor_unfilled_fields(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB41_40
	.align	16, 0x90
.LBB41_24:                              # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movq	40(%rcx), %rdi
	testq	%rdi, %rdi
	je	.LBB41_26
# BB#25:                                # %land.end
                                        #   in Loop: Header=BB41_24 Depth=1
	callq	integer_zerop
	movq	constructor_unfilled_fields(%rip), %rcx
	testq	%r12, %rax
	je	.LBB41_29
.LBB41_26:                              # %while.body58
                                        #   in Loop: Header=BB41_24 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, constructor_unfilled_fields(%rip)
	jne	.LBB41_24
	jmp	.LBB41_40
.LBB41_29:                              # %while.end61
	testq	%rcx, %rcx
	je	.LBB41_40
# BB#30:                                # %while.end61
	movl	constructor_designated(%rip), %eax
	testq	%r12, %rax
	jne	.LBB41_40
# BB#31:                                # %if.then65
	movq	72(%rcx), %rax
	testq	%rax, %rax
	je	.LBB41_32
# BB#33:                                # %cond.true.i
	movq	32(%rax), %r14
	jmp	.LBB41_34
.LBB41_32:
	movabsq	$.L.str154, %r14
.LBB41_34:                              # %cond.end.i
	movq	spelling(%rip), %rax
	movq	spelling_base(%rip), %rdi
	movq	%rax, %r15
	subq	%rdi, %r15
	movq	$28, %rcx
	movq	%r15, %rdx
	movslq	spelling_size(%rip), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rsi, %rdx
	jl	.LBB41_39
# BB#35:                                # %if.then.i
	addq	$10, %rsi
	movl	%esi, spelling_size(%rip)
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	testq	%rdi, %rdi
	je	.LBB41_36
# BB#37:                                # %if.else.i
	callq	xrealloc
	jmp	.LBB41_38
.LBB41_36:                              # %if.then7.i
	movq	%rsi, %rdi
	callq	xmalloc
.LBB41_38:                              # %if.end.i
	movq	%rax, spelling_base(%rip)
	movq	$28, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	$4, %rcx
	shlq	%cl, %r15
	addq	%rax, %r15
	movq	%r15, spelling(%rip)
	movq	%r15, %rax
.LBB41_39:                              # %push_member_name.exit
	movq	$2, %rcx
	movl	%ecx, (%rax)
	movq	%r14, 8(%rax)
	addq	$16, spelling(%rip)
	movabsq	$.L.str118, %rdi
	callq	warning_init
	movslq	constructor_depth(%rip), %rax
	movq	spelling_base(%rip), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	addq	%rdx, %rax
	movq	%rax, spelling(%rip)
.LBB41_40:                              # %if.end67
	movq	$1, %rax
	movl	%eax, constructor_incremental(%rip)
	movq	$1, %rdi
	callq	output_pending_init_elements
	movq	88(%rbx), %r14
	testq	%r14, %r14
	jne	.LBB41_57
# BB#41:                                # %if.else71
	movq	constructor_type(%rip), %r15
	xorq	%r14, %r14
	testq	%r15, %r15
	je	.LBB41_57
# BB#42:                                # %if.else74
	movzbl	16(%r15), %ecx
	cmpq	$21, %rcx
	ja	.LBB41_44
# BB#43:                                # %if.else74
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$3408384, %rax          # imm = 0x340200
	je	.LBB41_44
# BB#51:                                # %if.else114
	movl	constructor_erroneous(%rip), %eax
	testq	%rax, %rax
	je	.LBB41_53
# BB#52:                                # %if.then116
	movq	global_trees(%rip), %r14
	jmp	.LBB41_57
.LBB41_44:                              # %if.then98
	movq	constructor_elements(%rip), %rax
	testq	%rax, %rax
	je	.LBB41_45
# BB#48:                                # %if.else104
	cmpq	$0, (%rax)
	je	.LBB41_50
# BB#49:                                # %if.then108
	movabsq	$.L.str120, %rdi
	callq	error_init
	movq	constructor_elements(%rip), %rax
.LBB41_50:                              # %if.else109
	movq	32(%rax), %r14
	jmp	.LBB41_57
.LBB41_45:                              # %if.then100
	movl	constructor_erroneous(%rip), %eax
	testq	%rax, %rax
	jne	.LBB41_47
# BB#46:                                # %if.then102
	movabsq	$.L.str119, %rdi
	callq	error_init
.LBB41_47:                              # %if.end103
	movq	global_trees(%rip), %r14
	jmp	.LBB41_57
.LBB41_53:                              # %if.else117
	movq	constructor_elements(%rip), %rdi
	callq	nreverse
	movq	%rax, %rcx
	movq	$46, %rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	build
	movl	constructor_constant(%rip), %ecx
	movq	%rax, %r14
	testq	%rcx, %rcx
	je	.LBB41_57
# BB#54:                                # %if.end125
	movl	16(%r14), %eax
	orq	$512, %rax              # imm = 0x200
	movl	%eax, 16(%r14)
	movl	constructor_constant(%rip), %eax
	testq	%rax, %rax
	je	.LBB41_57
# BB#55:                                # %if.end125
	movl	constructor_simple(%rip), %eax
	testq	%r12, %rax
	je	.LBB41_57
# BB#56:                                # %if.then129
	movl	16(%r14), %eax
	orq	$262144, %rax           # imm = 0x40000
	movl	%eax, 16(%r14)
.LBB41_57:                              # %if.end138
	movq	8(%rbx), %rax
	movq	%rax, constructor_type(%rip)
	movq	16(%rbx), %rax
	movq	%rax, constructor_fields(%rip)
	movq	24(%rbx), %rax
	movq	%rax, constructor_index(%rip)
	movq	32(%rbx), %rax
	movq	%rax, constructor_max_index(%rip)
	movq	40(%rbx), %rax
	movq	%rax, constructor_unfilled_index(%rip)
	movq	48(%rbx), %rax
	movq	%rax, constructor_unfilled_fields(%rip)
	movq	56(%rbx), %rax
	movq	%rax, constructor_bit_index(%rip)
	movq	64(%rbx), %rax
	movq	%rax, constructor_elements(%rip)
	movsbq	104(%rbx), %rax
	movl	%eax, constructor_constant(%rip)
	movsbq	105(%rbx), %rax
	movl	%eax, constructor_simple(%rip)
	movsbq	107(%rbx), %rax
	movl	%eax, constructor_erroneous(%rip)
	movsbq	109(%rbx), %rax
	movl	%eax, constructor_incremental(%rip)
	movsbq	110(%rbx), %rax
	movl	%eax, constructor_designated(%rip)
	movq	72(%rbx), %rax
	movq	%rax, constructor_pending_elts(%rip)
	movslq	84(%rbx), %rax
	movl	%eax, constructor_depth(%rip)
	movzbl	106(%rbx), %ecx
	testq	%rcx, %rcx
	jne	.LBB41_59
# BB#58:                                # %if.then146
	movq	96(%rbx), %rcx
	movq	%rcx, constructor_range_stack(%rip)
.LBB41_59:                              # %if.end147
	movq	spelling_base(%rip), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	addq	%rdx, %rax
	movq	%rax, spelling(%rip)
	movq	(%rbx), %rax
	movq	%rax, constructor_stack(%rip)
	movq	%rbx, %rdi
	callq	free
	testq	%r14, %r14
	je	.LBB41_60
# BB#64:                                # %return
	movq	%r14, %rax
	jmp	.LBB41_63
.LBB41_60:                              # %if.then152
	cmpq	$0, constructor_stack(%rip)
	je	.LBB41_61
# BB#62:                                # %select.mid
	xorq	%rax, %rax
	jmp	.LBB41_63
.LBB41_61:
	movq	global_trees(%rip), %rax
.LBB41_63:                              # %select.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB41_11:                              # %if.then3
	movabsq	$.L.str7, %rax
	movabsq	$.L__FUNCTION__.pop_init_level, %rcx
	movq	$5488, %rsi             # imm = 0x1570
	jmp	.LBB41_12
.LBB41_28:                              # %if.else34
	movabsq	$.L.str7, %rax
	movabsq	$.L__FUNCTION__.pop_init_level, %rcx
	movq	$5520, %rsi             # imm = 0x1590
.LBB41_12:                              # %if.then3
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp483:
	.size	pop_init_level, .Ltmp483-pop_init_level
	.cfi_endproc

	.align	16, 0x90
	.type	find_init_member,@function
find_init_member:                       # @find_init_member
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
.Ltmp495:
	.cfi_offset %rbx, -48
.Ltmp496:
	.cfi_offset %r12, -40
.Ltmp497:
	.cfi_offset %r13, -32
.Ltmp498:
	.cfi_offset %r14, -24
.Ltmp499:
	.cfi_offset %r15, -16
	movq	constructor_type(%rip), %rax
	movzbl	16(%rax), %eax
	movq	%rdi, %r12
	cmpq	$20, %rax
	jne	.LBB42_1
# BB#12:                                # %if.then20
	movq	%r12, %rdi
	callq	bit_position
	movl	constructor_incremental(%rip), %ecx
	movq	%rax, %r15
	testq	%rcx, %rcx
	je	.LBB42_16
# BB#13:                                # %land.lhs.true23
	movq	constructor_unfilled_fields(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB42_15
# BB#14:                                # %lor.lhs.false
	callq	bit_position
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	tree_int_cst_lt
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB42_16
.LBB42_15:                              # %if.then28
	callq	set_nonincremental_init
.LBB42_16:                              # %while.cond30.preheader
	movq	constructor_pending_elts(%rip), %rbx
	xorq	%r14, %r14
	testq	%rbx, %rbx
	je	.LBB42_27
# BB#17:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB42_18:                              # %while.body32
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rdi
	cmpq	%r12, %rdi
	je	.LBB42_19
# BB#20:                                # %if.else37
                                        #   in Loop: Header=BB42_18 Depth=1
	callq	bit_position
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	tree_int_cst_lt
	testq	%r13, %rax
	jne	.LBB42_22
# BB#21:                                #   in Loop: Header=BB42_18 Depth=1
	addq	$8, %rbx
.LBB42_22:                              # %if.else37
                                        #   in Loop: Header=BB42_18 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB42_18
	jmp	.LBB42_27
.LBB42_1:                               # %entry
	cmpq	$18, %rax
	jne	.LBB42_23
# BB#2:                                 # %if.then
	movl	constructor_incremental(%rip), %eax
	testq	%rax, %rax
	je	.LBB42_5
# BB#3:                                 # %land.lhs.true
	movq	constructor_unfilled_index(%rip), %rsi
	movq	%r12, %rdi
	callq	tree_int_cst_lt
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB42_5
# BB#4:                                 # %if.then2
	callq	set_nonincremental_init
.LBB42_5:                               # %while.cond.preheader
	movq	constructor_pending_elts(%rip), %rbx
	xorq	%r14, %r14
	testq	%rbx, %rbx
	je	.LBB42_27
# BB#6:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB42_7:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rsi
	movq	%r12, %rdi
	callq	tree_int_cst_lt
	testq	%r15, %rax
	jne	.LBB42_10
# BB#8:                                 # %if.else
                                        #   in Loop: Header=BB42_7 Depth=1
	movq	32(%rbx), %rdi
	movq	%r12, %rsi
	callq	tree_int_cst_lt
	testq	%r15, %rax
	je	.LBB42_11
# BB#9:                                 # %if.then10
                                        #   in Loop: Header=BB42_7 Depth=1
	addq	$8, %rbx
.LBB42_10:                              # %while.cond.backedge
                                        #   in Loop: Header=BB42_7 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB42_7
	jmp	.LBB42_27
.LBB42_23:                              # %if.else49
	xorq	%r14, %r14
	cmpq	$21, %rax
	jne	.LBB42_27
# BB#24:                                # %if.else49
	movq	constructor_elements(%rip), %rax
	testq	%rax, %rax
	je	.LBB42_27
# BB#25:                                # %land.lhs.true57
	movq	24(%rax), %rcx
	cmpq	%r12, %rcx
	jne	.LBB42_27
# BB#26:                                # %if.then60
	movq	32(%rax), %r14
	jmp	.LBB42_27
.LBB42_19:                              # %if.then35
	movq	40(%rbx), %r14
	jmp	.LBB42_27
.LBB42_11:                              # %if.else11
	movq	40(%rbx), %r14
.LBB42_27:                              # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp500:
	.size	find_init_member, .Ltmp500-find_init_member
	.cfi_endproc

	.align	16, 0x90
	.type	set_nonincremental_init,@function
set_nonincremental_init:                # @set_nonincremental_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp503:
	.cfi_def_cfa_offset 16
.Ltmp504:
	.cfi_offset %rbx, -16
	movq	constructor_type(%rip), %rax
	movzbl	16(%rax), %ecx
	cmpq	$18, %rcx
	je	.LBB43_2
# BB#1:                                 # %entry
	cmpq	$20, %rcx
	jne	.LBB43_17
.LBB43_2:                               # %for.cond.preheader
	movq	constructor_elements(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB43_5
	.align	16, 0x90
.LBB43_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rdi
	movq	32(%rbx), %rsi
	callq	add_pending_init
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB43_3
# BB#4:                                 # %for.cond.for.end_crit_edge
	movq	constructor_type(%rip), %rax
.LBB43_5:                               # %for.end
	movq	$0, constructor_elements(%rip)
	movzbl	16(%rax), %ecx
	cmpq	$18, %rcx
	jne	.LBB43_6
# BB#12:                                # %if.then29
	movq	24(%rax), %rax
	testq	%rax, %rax
	je	.LBB43_14
# BB#13:                                # %if.then33
	movq	sizetype_tab+24(%rip), %rdi
	movq	104(%rax), %rsi
	callq	convert
	jmp	.LBB43_15
.LBB43_6:                               # %for.end
	cmpq	$20, %rcx
	jne	.LBB43_16
# BB#7:                                 # %if.then14
	movq	24(%rax), %rax
	jmp	.LBB43_8
	.align	16, 0x90
.LBB43_11:                              # %while.body
                                        #   in Loop: Header=BB43_8 Depth=1
	movq	(%rax), %rax
.LBB43_8:                               # %if.then14
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	movq	%rax, constructor_unfilled_fields(%rip)
	je	.LBB43_16
# BB#9:                                 # %land.lhs.true16
                                        #   in Loop: Header=BB43_8 Depth=1
	movzbl	53(%rax), %ecx
	testq	$2, %rcx
	je	.LBB43_16
# BB#10:                                # %land.rhs
                                        #   in Loop: Header=BB43_8 Depth=1
	cmpq	$0, 72(%rax)
	je	.LBB43_11
	jmp	.LBB43_16
.LBB43_14:                              # %if.else37
	movq	global_trees+136(%rip), %rax
.LBB43_15:                              # %if.end40
	movq	%rax, constructor_unfilled_index(%rip)
.LBB43_16:                              # %if.end40
	xorq	%rax, %rax
	movl	%eax, constructor_incremental(%rip)
.LBB43_17:                              # %return
	popq	%rbx
	retq
.Ltmp505:
	.size	set_nonincremental_init, .Ltmp505-set_nonincremental_init
	.cfi_endproc

	.align	16, 0x90
	.type	warning_init,@function
warning_init:                           # @warning_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp509:
	.cfi_def_cfa_offset 16
.Ltmp510:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp511:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	pushq	%rax
.Ltmp512:
	.cfi_offset %rbx, -56
.Ltmp513:
	.cfi_offset %r12, -48
.Ltmp514:
	.cfi_offset %r13, -40
.Ltmp515:
	.cfi_offset %r14, -32
.Ltmp516:
	.cfi_offset %r15, -24
	movq	%rdi, %rcx
	movabsq	$.L.str81, %rdi
	xorq	%rax, %rax
	xorq	%r13, %r13
	movq	%rcx, %rsi
	callq	warning
	movq	spelling_base(%rip), %r12
	movq	spelling(%rip), %r15
	cmpq	%r15, %r12
	jae	.LBB44_20
# BB#1:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	%r12, %rbx
	.align	16, 0x90
.LBB44_2:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rbx), %eax
	cmpq	$3, %rax
	jne	.LBB44_4
# BB#3:                                 # %if.then.i
                                        #   in Loop: Header=BB44_2 Depth=1
	addq	$25, %r13
	jmp	.LBB44_5
	.align	16, 0x90
.LBB44_4:                               # %if.else.i
                                        #   in Loop: Header=BB44_2 Depth=1
	movq	8(%rbx), %rdi
	callq	strlen
	andq	%r14, %r13
	leaq	1(%r13,%rax), %r13
.LBB44_5:                               # %for.inc.i
                                        #   in Loop: Header=BB44_2 Depth=1
	addq	$16, %rbx
	cmpq	%r15, %rbx
	jb	.LBB44_2
# BB#6:                                 # %spelling_length.exit
	incq	%r13
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	addq	$15, %r13
	andq	$-16, %r13
	movq	%rsp, %r14
	subq	%r13, %r14
	movq	%r14, %rsp
	cmpq	%r15, %r12
	jae	.LBB44_7
# BB#8:
	movq	$46, %r13
	movabsq	$.L.str162, %r15
	movq	%r14, %rbx
	.align	16, 0x90
.LBB44_9:                               # %for.body.i4
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB44_15 Depth 2
	movl	(%r12), %eax
	cmpq	$2, %rax
	jne	.LBB44_10
# BB#12:                                # %if.then5.i
                                        #   in Loop: Header=BB44_9 Depth=1
	movb	%r13b, (%rbx)
	incq	%rbx
	jmp	.LBB44_13
	.align	16, 0x90
.LBB44_10:                              # %for.body.i4
                                        #   in Loop: Header=BB44_9 Depth=1
	cmpq	$3, %rax
	jne	.LBB44_13
# BB#11:                                # %if.then.i6
                                        #   in Loop: Header=BB44_9 Depth=1
	movl	8(%r12), %edx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	sprintf
	movq	%rbx, %rdi
	callq	strlen
	addq	%rax, %rbx
	jmp	.LBB44_16
	.align	16, 0x90
.LBB44_13:                              # %if.end.i
                                        #   in Loop: Header=BB44_9 Depth=1
	movq	8(%r12), %rax
	movzbl	(%rax), %ecx
	testq	%rcx, %rcx
	movb	%cl, (%rbx)
	je	.LBB44_16
# BB#14:                                # %for.inc.i8.preheader
                                        #   in Loop: Header=BB44_9 Depth=1
	incq	%rax
	.align	16, 0x90
.LBB44_15:                              # %for.inc.i8
                                        #   Parent Loop BB44_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %ecx
	incq	%rax
	testq	%rcx, %rcx
	movb	%cl, 1(%rbx)
	leaq	1(%rbx), %rbx
	jne	.LBB44_15
.LBB44_16:                              # %for.inc13.i
                                        #   in Loop: Header=BB44_9 Depth=1
	movq	spelling(%rip), %rax
	addq	$16, %r12
	cmpq	%rax, %r12
	jb	.LBB44_9
	jmp	.LBB44_17
.LBB44_20:                              # %spelling_length.exit.thread
	movq	%rsp, %r14
	addq	$-16, %r14
	movq	%r14, %rsp
	movq	%r14, %rbx
	jmp	.LBB44_17
.LBB44_7:
	movq	%r14, %rbx
.LBB44_17:                              # %print_spelling.exit
	xorq	%rax, %rax
	movb	%al, (%rbx)
	movzbl	(%r14), %eax
	testq	%rax, %rax
	je	.LBB44_19
# BB#18:                                # %if.then
	movabsq	$.L.str111, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	warning
.LBB44_19:                              # %if.end
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp517:
	.size	warning_init, .Ltmp517-warning_init
	.cfi_endproc

	.align	16, 0x90
	.type	output_pending_init_elements,@function
output_pending_init_elements:           # @output_pending_init_elements
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp524:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp525:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp526:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp527:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp528:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp529:
	.cfi_def_cfa_offset 64
.Ltmp530:
	.cfi_offset %rbx, -48
.Ltmp531:
	.cfi_offset %r12, -40
.Ltmp532:
	.cfi_offset %r13, -32
.Ltmp533:
	.cfi_offset %r14, -24
.Ltmp534:
	.cfi_offset %r15, -16
	movq	constructor_pending_elts(%rip), %rax
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	testq	%rax, %rax
	je	.LBB45_18
# BB#1:                                 # %while.body.lr.ph
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	jmp	.LBB45_2
.LBB45_7:                               # %if.then7
                                        #   in Loop: Header=BB45_2 Depth=1
	movq	(%r14), %rax
	testq	%rax, %rax
	jne	.LBB45_2
	jmp	.LBB45_28
.LBB45_27:                              #   in Loop: Header=BB45_2 Depth=1
	leaq	32(%rbx), %r15
	movq	%rbx, %r14
.LBB45_28:                              # %while.end106
                                        #   in Loop: Header=BB45_2 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%r12, %rax
	je	.LBB45_18
# BB#29:                                # %while.end106
                                        #   in Loop: Header=BB45_2 Depth=1
	movq	(%r15), %rax
	testq	%rax, %rax
	je	.LBB45_18
# BB#30:                                # %if.end111
                                        #   in Loop: Header=BB45_2 Depth=1
	movq	constructor_type(%rip), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$18, %rcx
	je	.LBB45_33
# BB#31:                                # %if.end111
                                        #   in Loop: Header=BB45_2 Depth=1
	addq	$-20, %rcx
	andq	%r12, %rcx
	cmpq	$1, %rcx
	ja	.LBB45_17
# BB#32:                                # %if.then123
                                        #   in Loop: Header=BB45_2 Depth=1
	movq	%rax, constructor_unfilled_fields(%rip)
	jmp	.LBB45_17
.LBB45_33:                              # %if.then130
                                        #   in Loop: Header=BB45_2 Depth=1
	movq	%rax, constructor_unfilled_index(%rip)
	jmp	.LBB45_17
	.align	16, 0x90
.LBB45_2:                               # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB45_9 Depth 2
                                        #     Child Loop BB45_23 Depth 2
	movq	constructor_type(%rip), %rcx
	movzbl	16(%rcx), %ecx
	movq	%rax, %r14
	leaq	-20(%rcx), %rax
	andq	%r12, %rax
	cmpq	$2, %rax
	jae	.LBB45_3
# BB#13:                                # %if.then47
                                        #   in Loop: Header=BB45_2 Depth=1
	movq	constructor_unfilled_fields(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB45_18
# BB#14:                                # %if.end50
                                        #   in Loop: Header=BB45_2 Depth=1
	callq	bit_position
	movq	32(%r14), %rdi
	movq	%rax, %r15
	callq	bit_position
	movq	%rax, %r13
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	tree_int_cst_equal
	testq	%r12, %rax
	jne	.LBB45_15
# BB#19:                                # %if.else63
                                        #   in Loop: Header=BB45_2 Depth=1
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	tree_int_cst_lt
	testq	%r12, %rax
	jne	.LBB45_20
# BB#22:                                # %if.else74
                                        #   in Loop: Header=BB45_2 Depth=1
	movq	8(%r14), %rax
	testq	%rax, %rax
	jne	.LBB45_2
	.align	16, 0x90
.LBB45_23:                              # %while.cond80
                                        #   Parent Loop BB45_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	16(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB45_18
# BB#24:                                # %land.rhs83
                                        #   in Loop: Header=BB45_23 Depth=2
	movq	8(%rbx), %rax
	cmpq	%r14, %rax
	movq	%rbx, %r14
	je	.LBB45_23
# BB#25:                                # %land.lhs.true93
                                        #   in Loop: Header=BB45_2 Depth=1
	movq	32(%rbx), %rdi
	callq	bit_position
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	tree_int_cst_lt
	testq	%r12, %rax
	jne	.LBB45_27
# BB#26:                                #   in Loop: Header=BB45_2 Depth=1
	movq	%rbx, %r14
	jmp	.LBB45_17
	.align	16, 0x90
.LBB45_3:                               # %while.body
                                        #   in Loop: Header=BB45_2 Depth=1
	cmpq	$18, %rcx
	jne	.LBB45_17
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB45_2 Depth=1
	movq	32(%r14), %rdi
	movq	constructor_unfilled_index(%rip), %rsi
	callq	tree_int_cst_equal
	testq	%r12, %rax
	jne	.LBB45_5
# BB#6:                                 # %if.else
                                        #   in Loop: Header=BB45_2 Depth=1
	leaq	32(%r14), %r15
	movq	constructor_unfilled_index(%rip), %rdi
	movq	(%r15), %rsi
	callq	tree_int_cst_lt
	testq	%r12, %rax
	jne	.LBB45_7
# BB#8:                                 # %if.else13
                                        #   in Loop: Header=BB45_2 Depth=1
	movq	8(%r14), %rax
	testq	%rax, %rax
	jne	.LBB45_2
	.align	16, 0x90
.LBB45_9:                               # %while.cond18
                                        #   Parent Loop BB45_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	16(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB45_18
# BB#10:                                # %land.rhs
                                        #   in Loop: Header=BB45_9 Depth=2
	movq	8(%rbx), %rax
	cmpq	%r14, %rax
	movq	%rbx, %r14
	je	.LBB45_9
# BB#11:                                # %land.lhs.true
                                        #   in Loop: Header=BB45_2 Depth=1
	movq	constructor_unfilled_index(%rip), %rdi
	movq	32(%rbx), %rsi
	callq	tree_int_cst_lt
	testq	%r12, %rax
	jne	.LBB45_27
# BB#12:                                #   in Loop: Header=BB45_2 Depth=1
	movq	%rbx, %r14
	jmp	.LBB45_17
	.align	16, 0x90
.LBB45_15:                              # %if.then56
                                        #   in Loop: Header=BB45_2 Depth=1
	movq	32(%r14), %rax
	movq	%rax, constructor_unfilled_fields(%rip)
	movq	32(%r14), %rdx
	movq	40(%r14), %rdi
	movq	8(%rdx), %rsi
	jmp	.LBB45_16
.LBB45_20:                              # %if.then66
                                        #   in Loop: Header=BB45_2 Depth=1
	movq	(%r14), %rax
	testq	%rax, %rax
	jne	.LBB45_2
# BB#21:                                #   in Loop: Header=BB45_2 Depth=1
	leaq	32(%r14), %r15
	jmp	.LBB45_28
.LBB45_5:                               # %if.then2
                                        #   in Loop: Header=BB45_2 Depth=1
	movq	constructor_type(%rip), %rax
	movq	40(%r14), %rdi
	movq	8(%rax), %rsi
	movq	constructor_unfilled_index(%rip), %rdx
.LBB45_16:                              # %while.cond.backedge
                                        #   in Loop: Header=BB45_2 Depth=1
	xorq	%rcx, %rcx
	callq	output_init_element
.LBB45_17:                              # %while.cond.backedge
                                        #   in Loop: Header=BB45_2 Depth=1
	testq	%r14, %r14
	movq	%r14, %rax
	jne	.LBB45_2
.LBB45_18:                              # %if.then110
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp535:
	.size	output_pending_init_elements, .Ltmp535-output_pending_init_elements
	.cfi_endproc

	.globl	set_init_index
	.align	16, 0x90
	.type	set_init_index,@function
set_init_index:                         # @set_init_index
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp542:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp543:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp544:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp545:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp546:
	.cfi_def_cfa_offset 48
.Ltmp547:
	.cfi_offset %rbx, -48
.Ltmp548:
	.cfi_offset %r12, -40
.Ltmp549:
	.cfi_offset %r13, -32
.Ltmp550:
	.cfi_offset %r14, -24
.Ltmp551:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movq	$1, %rdi
	movq	$1, %r14
	callq	set_designator
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	jne	.LBB46_30
# BB#1:                                 # %if.end
	movb	%r14b, designator_errorneous(%rip)
	.align	16, 0x90
.LBB46_2:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %r12
	movzbl	16(%r12), %eax
	leaq	-114(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$2, %rcx
	ja	.LBB46_4
# BB#3:                                 # %land.rhs
                                        #   in Loop: Header=BB46_2 Depth=1
	movq	32(%r12), %rbx
	movq	8(%r12), %rcx
	movq	8(%rbx), %rdx
	movl	60(%rcx), %ecx
	movl	60(%rdx), %edx
	xorq	%rcx, %rdx
	testq	$65024, %rdx            # imm = 0xFE00
	je	.LBB46_2
.LBB46_4:                               # %while.end
	xorq	%r14, %r14
	testq	%r15, %r15
	je	.LBB46_7
	.align	16, 0x90
.LBB46_5:                               # %while.cond29
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %r14
	movzbl	16(%r14), %ecx
	addq	$-114, %rcx
	andq	%r13, %rcx
	cmpq	$2, %rcx
	ja	.LBB46_7
# BB#6:                                 # %land.rhs47
                                        #   in Loop: Header=BB46_5 Depth=1
	movq	32(%r14), %r15
	movq	8(%r14), %rcx
	movq	8(%r15), %rdx
	movl	60(%rcx), %ecx
	movl	60(%rdx), %edx
	xorq	%rcx, %rdx
	testq	$65024, %rdx            # imm = 0xFE00
	je	.LBB46_5
.LBB46_7:                               # %if.end72
	cmpq	$25, %rax
	jne	.LBB46_8
# BB#10:                                # %if.else
	testq	%r14, %r14
	je	.LBB46_12
# BB#11:                                # %land.lhs.true
	movzbl	16(%r14), %eax
	cmpq	$25, %rax
	jne	.LBB46_8
.LBB46_12:                              # %if.else86
	movq	constructor_type(%rip), %rax
	movzbl	16(%rax), %eax
	cmpq	$18, %rax
	jne	.LBB46_13
# BB#14:                                # %if.else93
	movq	constructor_max_index(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB46_17
# BB#15:                                # %land.lhs.true95
	movq	%r12, %rsi
	callq	tree_int_cst_lt
	testq	%r13, %rax
	je	.LBB46_17
# BB#16:                                # %if.then98
	movabsq	$.L.str123, %rdi
	jmp	.LBB46_9
.LBB46_8:                               # %if.then78
	movabsq	$.L.str121, %rdi
	jmp	.LBB46_9
.LBB46_13:                              # %if.then92
	movabsq	$.L.str122, %rdi
.LBB46_9:                               # %if.then78
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	error_init  # TAILCALL
.LBB46_17:                              # %if.else99
	movq	sizetype_tab+24(%rip), %rdi
	movq	%r12, %rsi
	callq	convert
	xorq	%rbx, %rbx
	testq	%r14, %r14
	movq	%rax, constructor_index(%rip)
	movabsq	$0, %r15
	je	.LBB46_25
# BB#18:                                # %if.then102
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	tree_int_cst_equal
	testq	%r13, %rax
	movabsq	$0, %r15
	jne	.LBB46_25
# BB#19:                                # %if.else106
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	tree_int_cst_lt
	testq	%r13, %rax
	je	.LBB46_21
# BB#20:                                # %if.then109
	movabsq	$.L.str124, %rdi
	jmp	.LBB46_24
.LBB46_21:                              # %if.else110
	movq	sizetype_tab+24(%rip), %rdi
	movq	%r14, %rsi
	callq	convert
	movq	constructor_max_index(%rip), %rdi
	movq	%rax, %r15
	testq	%rdi, %rdi
	je	.LBB46_25
# BB#22:                                # %land.lhs.true113
	movq	%r15, %rsi
	callq	tree_int_cst_lt
	testq	%r13, %rax
	je	.LBB46_25
# BB#23:                                # %if.then116
	movabsq	$.L.str125, %rdi
.LBB46_24:                              # %if.end120
	callq	error_init
	xorq	%r15, %r15
.LBB46_25:                              # %if.end120
	movl	designator_depth(%rip), %eax
	incq	%rax
	cmpq	$0, constructor_range_stack(%rip)
	movl	%eax, designator_depth(%rip)
	movb	%bl, designator_errorneous(%rip)
	jne	.LBB46_27
# BB#26:                                # %if.end120
	testq	%r15, %r15
	je	.LBB46_30
.LBB46_27:                              # %if.then124
	movq	$56, %rdi
	callq	ggc_alloc
	movq	constructor_range_stack(%rip), %rcx
	movq	%rcx, 8(%rax)
	movq	$0, (%rax)
	movq	constructor_fields(%rip), %rcx
	movq	%rcx, 48(%rax)
	movq	constructor_index(%rip), %rcx
	movq	%rcx, 24(%rax)
	movq	constructor_index(%rip), %rcx
	movq	%rcx, 32(%rax)
	movq	constructor_stack(%rip), %rcx
	movq	%rcx, 16(%rax)
	movq	%r15, 40(%rax)
	movq	constructor_range_stack(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB46_29
# BB#28:                                # %if.then.i
	movq	%rax, (%rcx)
.LBB46_29:                              # %push_range_stack.exit
	movq	%rax, constructor_range_stack(%rip)
.LBB46_30:                              # %if.end129
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp552:
	.size	set_init_index, .Ltmp552-set_init_index
	.cfi_endproc

	.align	16, 0x90
	.type	set_designator,@function
set_designator:                         # @set_designator
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp555:
	.cfi_def_cfa_offset 16
.Ltmp556:
	.cfi_offset %rbx, -16
	movq	constructor_type(%rip), %rax
	testq	%rax, %rax
	movq	$1, %rbx
	je	.LBB47_25
# BB#1:                                 # %if.end
	movzbl	designator_errorneous(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB47_25
# BB#2:                                 # %if.end2
	movl	designator_depth(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB47_3
# BB#9:                                 # %if.end9
	movl	constructor_no_implicit(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB47_11
# BB#10:                                # %if.then11
	movabsq	$.L.str153, %rdi
	callq	error_init
	jmp	.LBB47_25
.LBB47_3:                               # %if.then4
	cmpq	$0, constructor_range_stack(%rip)
	je	.LBB47_7
# BB#4:                                 # %if.then6
	movabsq	$.L.str7, %rax
	movabsq	$.L__FUNCTION__.set_designator, %rcx
	movq	$5645, %rsi             # imm = 0x160D
	jmp	.LBB47_5
	.align	16, 0x90
.LBB47_6:                               # %while.body
                                        #   in Loop: Header=BB47_7 Depth=1
	movq	$1, %rdi
	callq	pop_init_level
	movq	%rax, %rdi
	callq	process_init_element
.LBB47_7:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	constructor_stack(%rip), %rax
	movzbl	106(%rax), %eax
	testq	%rax, %rax
	jne	.LBB47_6
# BB#8:                                 # %while.end
	movq	$1, %rax
	movl	%eax, constructor_designated(%rip)
.LBB47_24:                              # %return
	xorq	%rbx, %rbx
	jmp	.LBB47_25
.LBB47_11:                              # %if.end12
	movzbl	16(%rax), %ecx
	cmpq	$18, %rcx
	je	.LBB47_15
# BB#12:                                # %if.end12
	addq	$-20, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	ja	.LBB47_14
# BB#13:                                # %if.then19
	movq	constructor_fields(%rip), %rax
	movq	8(%rax), %rax
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB47_16
	jmp	.LBB47_17
.LBB47_15:                              # %if.then30
	movq	8(%rax), %rax
.LBB47_16:                              # %if.end37
	movq	128(%rax), %rax
.LBB47_17:                              # %if.end37
	movl	16(%rax), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdi
	je	.LBB47_20
# BB#18:                                # %if.end37
	movq	%rax, %rdx
	andq	$255, %rdx
	cmpq	$18, %rdx
	je	.LBB47_20
# BB#19:                                # %if.then44
	movabsq	$.L.str122, %rdi
	callq	error_init
	jmp	.LBB47_25
.LBB47_20:                              # %if.else45
	testq	%rcx, %rdi
	jne	.LBB47_23
# BB#21:                                # %if.else45
	andq	$254, %rax
	cmpq	$20, %rax
	je	.LBB47_23
# BB#22:                                # %if.then51
	movabsq	$.L.str126, %rdi
	callq	error_init
.LBB47_25:                              # %return
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB47_23:                              # %if.end53
	movq	$1, %rax
	movl	%eax, constructor_designated(%rip)
	movq	$2, %rdi
	callq	push_init_level
	jmp	.LBB47_24
.LBB47_14:                              # %if.else35
	movabsq	$.L.str7, %rax
	movabsq	$.L__FUNCTION__.set_designator, %rcx
	movq	$5673, %rsi             # imm = 0x1629
.LBB47_5:                               # %if.then6
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp557:
	.size	set_designator, .Ltmp557-set_designator
	.cfi_endproc

	.globl	set_init_label
	.align	16, 0x90
	.type	set_init_label,@function
set_init_label:                         # @set_init_label
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp560:
	.cfi_def_cfa_offset 16
.Ltmp561:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	xorq	%rdi, %rdi
	callq	set_designator
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB48_12
# BB#1:                                 # %if.end
	movq	constructor_type(%rip), %rax
	movq	$1, %rcx
	movb	%cl, designator_errorneous(%rip)
	movl	16(%rax), %ecx
	andq	$254, %rcx
	cmpq	$20, %rcx
	jne	.LBB48_13
# BB#2:                                 # %if.end7
	movq	24(%rax), %rax
	jmp	.LBB48_3
	.align	16, 0x90
.LBB48_5:                               # %for.inc
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	(%rax), %rax
.LBB48_3:                               # %if.end7
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB48_6
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB48_3 Depth=1
	movq	72(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB48_5
# BB#7:                                 # %for.end
	testq	%rax, %rax
	je	.LBB48_6
# BB#8:                                 # %if.else
	movl	designator_depth(%rip), %ecx
	incq	%rcx
	xorq	%rdx, %rdx
	cmpq	$0, constructor_range_stack(%rip)
	movq	%rax, constructor_fields(%rip)
	movl	%ecx, designator_depth(%rip)
	movb	%dl, designator_errorneous(%rip)
	je	.LBB48_12
# BB#9:                                 # %if.then16
	movq	$56, %rdi
	callq	ggc_alloc
	movq	constructor_range_stack(%rip), %rcx
	movq	%rcx, 8(%rax)
	movq	$0, (%rax)
	movq	constructor_fields(%rip), %rcx
	movq	%rcx, 48(%rax)
	movq	constructor_index(%rip), %rcx
	movq	%rcx, 24(%rax)
	movq	constructor_index(%rip), %rcx
	movq	%rcx, 32(%rax)
	movq	constructor_stack(%rip), %rcx
	movq	%rcx, 16(%rax)
	movq	$0, 40(%rax)
	movq	constructor_range_stack(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB48_11
# BB#10:                                # %if.then.i
	movq	%rax, (%rcx)
.LBB48_11:                              # %push_range_stack.exit
	movq	%rax, constructor_range_stack(%rip)
.LBB48_12:                              # %if.end18
	popq	%rbx
	retq
.LBB48_13:                              # %if.then6
	movabsq	$.L.str126, %rdi
	popq	%rbx
	jmp	error_init  # TAILCALL
.LBB48_6:                               # %if.then14
	movq	32(%rbx), %rsi
	movabsq	$.L.str127, %rdi
	xorq	%rax, %rax
	popq	%rbx
	jmp	error  # TAILCALL
.Ltmp562:
	.size	set_init_label, .Ltmp562-set_init_label
	.cfi_endproc

	.align	16, 0x90
	.type	output_init_element,@function
output_init_element:                    # @output_init_element
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp569:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp570:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp571:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp572:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp573:
	.cfi_def_cfa_offset 48
.Ltmp574:
	.cfi_offset %rbx, -48
.Ltmp575:
	.cfi_offset %r12, -40
.Ltmp576:
	.cfi_offset %r13, -32
.Ltmp577:
	.cfi_offset %r14, -24
.Ltmp578:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	8(%rbx), %rax
	movzbl	16(%rax), %edi
	movq	%rcx, %r14
	movq	%rdx, %r12
	movq	%rsi, %r15
	cmpq	$23, %rdi
	je	.LBB49_6
# BB#1:                                 # %entry
	cmpq	$18, %rdi
	jne	.LBB49_7
# BB#2:                                 # %land.lhs.true
	movzbl	16(%rbx), %ecx
	cmpq	$29, %rcx
	jne	.LBB49_5
# BB#3:                                 # %land.lhs.true15
	movzbl	16(%r15), %ecx
	cmpq	$18, %rcx
	jne	.LBB49_5
# BB#4:                                 # %land.lhs.true21
	movq	8(%r15), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$6, %rcx
	je	.LBB49_7
.LBB49_5:                               # %land.lhs.true29
	movq	128(%rax), %rdi
	movq	128(%r15), %rsi
	callq	comptypes
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB49_7
.LBB49_6:                               # %if.then
	movq	%rbx, %rdi
	callq	default_conversion
	movq	%rax, %rbx
.LBB49_7:                               # %if.end
	movzbl	16(%rbx), %eax
	movl	require_constant_value(%rip), %esi
	cmpq	$170, %rax
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	jne	.LBB49_9
# BB#8:                                 # %if.end
	movq	%rdx, %rcx
.LBB49_9:                               # %if.end
	testq	%rsi, %rsi
	movq	%rax, %rsi
	je	.LBB49_11
# BB#10:                                # %if.end
	movq	%rdx, %rsi
.LBB49_11:                              # %if.end
	movl	flag_isoc99(%rip), %edi
	testq	%rdi, %rdi
	jne	.LBB49_13
# BB#12:                                # %if.end
	movq	%rdx, %rax
.LBB49_13:                              # %if.end
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %r14
	je	.LBB49_16
# BB#14:                                # %if.end
	andq	%rcx, %rsi
	andq	%rax, %rsi
	je	.LBB49_16
# BB#15:                                # %if.then47
	movq	32(%rbx), %rax
	movq	32(%rax), %rax
	movq	104(%rax), %rbx
.LBB49_16:                              # %if.end52
	movq	global_trees(%rip), %rax
	cmpq	%rax, %rbx
	je	.LBB49_17
# BB#18:                                # %if.else
	movzbl	17(%rbx), %eax
	testq	$2, %rax
	je	.LBB49_19
# BB#20:                                # %if.else60
	movq	8(%rbx), %rsi
	movq	%rbx, %rdi
	callq	initializer_constant_valid_p
	testq	%rax, %rax
	je	.LBB49_24
# BB#21:                                # %lor.lhs.false65
	movq	constructor_type(%rip), %rax
	movl	16(%rax), %eax
	andq	$254, %rax
	cmpq	$20, %rax
	jne	.LBB49_25
# BB#22:                                # %land.lhs.true77
	movq	48(%r12), %rax
	movabsq	$2199023255552, %rcx    # imm = 0x20000000000
	testq	%rcx, %rax
	je	.LBB49_25
# BB#23:                                # %land.lhs.true83
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	je	.LBB49_25
.LBB49_24:                              # %if.then89
	xorq	%rax, %rax
	movl	%eax, constructor_simple(%rip)
	jmp	.LBB49_25
.LBB49_17:                              # %if.then54
	movq	$1, %rax
	movl	%eax, constructor_erroneous(%rip)
	jmp	.LBB49_25
.LBB49_19:                              # %if.then59
	xorq	%rax, %rax
	movl	%eax, constructor_constant(%rip)
.LBB49_25:                              # %if.end92
	movl	require_constant_value(%rip), %eax
	testq	%rax, %rax
	je	.LBB49_28
# BB#26:                                # %land.lhs.true94
	movzbl	17(%rbx), %eax
	testq	$2, %rax
	je	.LBB49_27
.LBB49_28:                              # %if.else102
	movl	require_constant_elements(%rip), %eax
	testq	%rax, %rax
	je	.LBB49_31
# BB#29:                                # %land.lhs.true104
	movq	8(%rbx), %rsi
	movq	%rbx, %rdi
	callq	initializer_constant_valid_p
	testq	%rax, %rax
	jne	.LBB49_31
# BB#30:                                # %if.then109
	movabsq	$.L.str151, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	jmp	.LBB49_31
.LBB49_27:                              # %if.then101
	movabsq	$.L.str150, %rdi
	callq	error_init
	movq	global_trees(%rip), %rbx
.LBB49_31:                              # %if.end111
	testq	%r12, %r12
	je	.LBB49_37
# BB#32:                                # %land.lhs.true113
	movq	8(%r12), %rax
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB49_73
# BB#33:                                # %lor.lhs.false117
	movq	32(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB49_37
# BB#34:                                # %land.lhs.true122
	callq	integer_zerop
	testq	%r13, %rax
	je	.LBB49_37
# BB#35:                                # %land.lhs.true129
	movq	constructor_type(%rip), %rax
	movzbl	16(%rax), %eax
	cmpq	$18, %rax
	je	.LBB49_73
# BB#36:                                # %lor.lhs.false135
	cmpq	$0, (%r12)
	jne	.LBB49_73
.LBB49_37:                              # %if.end139
	movl	require_constant_value(%rip), %edx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	digest_init
	movq	global_trees(%rip), %rcx
	movq	%rax, %rbx
	cmpq	%rcx, %rbx
	je	.LBB49_38
# BB#39:                                # %if.end143
	movq	constructor_type(%rip), %rax
	movl	16(%rax), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$18, %rcx
	jne	.LBB49_43
# BB#40:                                # %land.lhs.true149
	movl	constructor_incremental(%rip), %eax
	testq	%rax, %rax
	je	.LBB49_51
# BB#41:                                # %lor.lhs.false151
	movq	constructor_unfilled_index(%rip), %rsi
	movq	%r12, %rdi
	callq	tree_int_cst_equal
	testq	%r13, %rax
	je	.LBB49_47
# BB#42:                                # %lor.lhs.false151.if.else161_crit_edge
	movq	constructor_type(%rip), %rax
	movl	16(%rax), %eax
.LBB49_43:                              # %if.else161
	andq	$255, %rax
	cmpq	$20, %rax
	jne	.LBB49_55
# BB#44:                                # %land.lhs.true167
	movl	constructor_incremental(%rip), %ecx
	movq	constructor_unfilled_fields(%rip), %rax
	testq	%rcx, %rcx
	je	.LBB49_52
# BB#45:                                # %land.lhs.true167
	cmpq	%r12, %rax
	jne	.LBB49_52
# BB#46:                                # %if.else185.thread
	movq	constructor_elements(%rip), %rdx
	jmp	.LBB49_60
.LBB49_38:                              # %if.then142
	movq	$1, %rax
	movl	%eax, constructor_erroneous(%rip)
	jmp	.LBB49_73
.LBB49_55:                              # %if.else185
	movq	constructor_elements(%rip), %rdx
	cmpq	$21, %rax
	jne	.LBB49_60
# BB#56:                                # %if.else185
	testq	%rdx, %rdx
	je	.LBB49_60
# BB#57:                                # %if.then193
	movq	32(%rdx), %rax
	movzbl	17(%rax), %eax
	testq	$1, %rax
	je	.LBB49_59
# BB#58:                                # %if.then200
	movabsq	$.L.str152, %rdi
	callq	warning_init
.LBB49_59:                              # %if.end201
	movq	$0, constructor_elements(%rip)
	xorq	%rdx, %rdx
.LBB49_60:                              # %if.end204
	xorq	%rdi, %rdi
	testq	%r12, %r12
	je	.LBB49_63
# BB#61:                                # %land.lhs.true206
	movzbl	16(%r12), %eax
	movq	%r12, %rdi
	cmpq	$25, %rax
	jne	.LBB49_63
# BB#62:                                # %if.then212
	callq	copy_node
	movq	constructor_elements(%rip), %rdx
	movq	%rax, %rdi
.LBB49_63:                              # %if.end214
	movq	%rbx, %rsi
	callq	tree_cons
	movq	constructor_type(%rip), %rcx
	movq	%rax, constructor_elements(%rip)
	movzbl	16(%rcx), %eax
	cmpq	$21, %rax
	je	.LBB49_71
# BB#64:                                # %if.end214
	cmpq	$20, %rax
	movabsq	$2199023255552, %rdx    # imm = 0x20000000000
	jne	.LBB49_65
# BB#67:                                # %if.then229
	movq	constructor_unfilled_fields(%rip), %rax
	.align	16, 0x90
.LBB49_68:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	movq	%rax, constructor_unfilled_fields(%rip)
	je	.LBB49_72
# BB#69:                                # %land.lhs.true233
                                        #   in Loop: Header=BB49_68 Depth=1
	movq	48(%rax), %rcx
	testq	%rdx, %rcx
	je	.LBB49_72
# BB#70:                                # %land.rhs
                                        #   in Loop: Header=BB49_68 Depth=1
	cmpq	$0, 72(%rax)
	je	.LBB49_68
	jmp	.LBB49_72
.LBB49_52:                              # %if.then171
	testq	%rcx, %rcx
	je	.LBB49_51
# BB#53:                                # %if.then173
	testq	%rax, %rax
	je	.LBB49_50
# BB#54:                                # %if.else176
	movq	%r12, %rdi
	callq	bit_position
	movq	constructor_unfilled_fields(%rip), %rdi
	movq	%rax, %r14
	callq	bit_position
	movq	%r14, %rdi
	movq	%rax, %rsi
	jmp	.LBB49_49
.LBB49_71:                              # %if.then251
	movq	$0, constructor_unfilled_fields(%rip)
	jmp	.LBB49_72
.LBB49_65:                              # %if.end214
	cmpq	$18, %rax
	jne	.LBB49_72
# BB#66:                                # %if.then221
	movq	constructor_unfilled_index(%rip), %rsi
	movq	global_trees+144(%rip), %rdx
	movq	$59, %rdi
	callq	size_binop
	movq	%rax, constructor_unfilled_index(%rip)
.LBB49_72:                              # %if.end254
	testq	%r13, %r14
	je	.LBB49_73
# BB#74:                                # %if.then256
	xorq	%rdi, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	output_pending_init_elements  # TAILCALL
.LBB49_73:                              # %if.end257
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB49_47:                              # %if.then154
	movl	constructor_incremental(%rip), %eax
	testq	%rax, %rax
	je	.LBB49_51
# BB#48:                                # %land.lhs.true156
	movq	constructor_unfilled_index(%rip), %rsi
	movq	%r12, %rdi
.LBB49_49:                              # %land.lhs.true156
	callq	tree_int_cst_lt
	testq	%r13, %rax
	je	.LBB49_51
.LBB49_50:                              # %if.then159
	callq	set_nonincremental_init
.LBB49_51:                              # %if.end160
	movq	%r12, %rdi
	movq	%rbx, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	add_pending_init  # TAILCALL
.Ltmp579:
	.size	output_init_element, .Ltmp579-output_init_element
	.cfi_endproc

	.globl	simple_asm_stmt
	.align	16, 0x90
	.type	simple_asm_stmt,@function
simple_asm_stmt:                        # @simple_asm_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp582:
	.cfi_def_cfa_offset 16
.Ltmp583:
	.cfi_offset %rbx, -16
	movq	global_trees(%rip), %rcx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB50_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rdx
	movl	16(%rdx), %eax
	movq	%rax, %rdi
	andq	$255, %rdi
	leaq	-114(%rdi), %rbx
	andq	%r8, %rbx
	cmpq	$2, %rbx
	ja	.LBB50_4
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB50_1 Depth=1
	movq	32(%rdx), %rdi
	cmpq	%rcx, %rdi
	je	.LBB50_6
# BB#3:                                 # %land.rhs
                                        #   in Loop: Header=BB50_1 Depth=1
	movq	8(%rdx), %rbx
	movq	8(%rdi), %rsi
	movl	60(%rbx), %ebx
	movl	60(%rsi), %esi
	xorq	%rbx, %rsi
	testq	$65024, %rsi            # imm = 0xFE00
	je	.LBB50_1
	jmp	.LBB50_6
.LBB50_4:                               # %while.end
	cmpq	$121, %rdi
	jne	.LBB50_6
# BB#5:                                 # %if.then
	movq	32(%rdx), %rdx
	movl	16(%rdx), %eax
.LBB50_6:                               # %if.end
	andq	$255, %rax
	cmpq	$29, %rax
	jne	.LBB50_10
# BB#7:                                 # %if.then44
	cmpq	$0, (%rdx)
	je	.LBB50_9
# BB#8:                                 # %if.then46
	movq	%rdx, %rdi
	callq	combine_strings
	movq	%rax, %rdx
.LBB50_9:                               # %if.end47
	movq	$165, %rdi
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	xorq	%rax, %rax
	callq	build_stmt
	movq	%rax, %rdi
	callq	add_stmt
	movq	%rax, %rbx
	movl	16(%rbx), %eax
	orq	$16777216, %rax         # imm = 0x1000000
	movl	%eax, 16(%rbx)
	jmp	.LBB50_11
.LBB50_10:                              # %if.end53
	movabsq	$.L.str136, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	error
.LBB50_11:                              # %return
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp584:
	.size	simple_asm_stmt, .Ltmp584-simple_asm_stmt
	.cfi_endproc

	.globl	build_asm_stmt
	.align	16, 0x90
	.type	build_asm_stmt,@function
build_asm_stmt:                         # @build_asm_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp591:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp592:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp593:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp594:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp595:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp596:
	.cfi_def_cfa_offset 96
.Ltmp597:
	.cfi_offset %rbx, -48
.Ltmp598:
	.cfi_offset %r12, -40
.Ltmp599:
	.cfi_offset %r13, -32
.Ltmp600:
	.cfi_offset %r14, -24
.Ltmp601:
	.cfi_offset %r15, -16
	cmpq	$0, (%rsi)
	movq	%r8, %r14
	movq	%rcx, %rbx
	movq	%rdx, %r12
	movq	%rdi, %r15
	je	.LBB51_2
# BB#1:                                 # %if.then
	movq	%rsi, %rdi
	callq	combine_strings
	movq	%rax, %rsi
.LBB51_2:                               # %if.end
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movzbl	16(%rsi), %eax
	cmpq	$29, %rax
	jne	.LBB51_47
# BB#3:                                 # %if.end3
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%r14, 24(%rsp)          # 8-byte Spill
	xorq	%rbx, %rbx
	testq	%r15, %r15
	je	.LBB51_7
# BB#4:                                 # %land.lhs.true
	movq	ridpointers(%rip), %rax
	movq	72(%rax), %rax
	cmpq	%r15, %rax
	je	.LBB51_5
# BB#6:                                 # %if.then6
	movq	32(%r15), %rsi
	movabsq	$.L.str138, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	warning
	jmp	.LBB51_7
.LBB51_47:                              # %return
	movabsq	$.L.str137, %rdi
	xorq	%rax, %rax
	callq	error
	xorq	%rax, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB51_5:
	movq	%r15, %rbx
.LBB51_7:                               # %if.end7
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	testq	%r12, %r12
	movq	%r12, 40(%rsp)          # 8-byte Spill
	je	.LBB51_24
# BB#8:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movabsq	$.L.str81, %rbx
	movabsq	$.L.str139, %r14
	movabsq	$13194139533343, %r12   # imm = 0xC000000001F
	movq	40(%rsp), %r15          # 8-byte Reload
	.align	16, 0x90
.LBB51_9:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB51_10 Depth 2
                                        #     Child Loop BB51_14 Depth 2
                                        #     Child Loop BB51_28 Depth 2
	movq	global_trees(%rip), %rcx
	movq	32(%r15), %rdx
	.align	16, 0x90
.LBB51_10:                              # %while.cond
                                        #   Parent Loop BB51_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rdx, %rax
	movzbl	16(%rax), %edx
	addq	$-114, %rdx
	andq	%r13, %rdx
	cmpq	$2, %rdx
	ja	.LBB51_13
# BB#11:                                # %land.lhs.true25
                                        #   in Loop: Header=BB51_10 Depth=2
	movq	32(%rax), %rdx
	cmpq	%rcx, %rdx
	je	.LBB51_13
# BB#12:                                # %land.rhs
                                        #   in Loop: Header=BB51_10 Depth=2
	movq	8(%rax), %rsi
	movq	8(%rdx), %rdi
	movl	60(%rsi), %esi
	movl	60(%rdi), %edi
	xorq	%rsi, %rdi
	testq	$65024, %rdi            # imm = 0xFE00
	je	.LBB51_10
.LBB51_13:                              # %while.end
                                        #   in Loop: Header=BB51_9 Depth=1
	movq	%rax, 32(%r15)
	movq	%rax, %rdx
	jmp	.LBB51_14
	.align	16, 0x90
.LBB51_16:                              # %while.body89
                                        #   in Loop: Header=BB51_14 Depth=2
	movq	32(%rdx), %rdx
.LBB51_14:                              # %while.cond48
                                        #   Parent Loop BB51_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	16(%rdx), %ecx
	addq	$-72, %rcx
	andq	%r13, %rcx
	cmpq	$43, %rcx
	ja	.LBB51_28
# BB#15:                                # %while.cond48
                                        #   in Loop: Header=BB51_14 Depth=2
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%r12, %rsi
	jne	.LBB51_16
	jmp	.LBB51_28
.LBB51_39:                              # %sw.bb.i.i
                                        #   in Loop: Header=BB51_28 Depth=2
	movq	32(%rax), %rax
	.align	16, 0x90
.LBB51_28:                              # %tailrecurse.i.i
                                        #   Parent Loop BB51_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	16(%rax), %ecx
	cmpq	$169, %rcx
	jg	.LBB51_42
# BB#29:                                # %tailrecurse.i.i
                                        #   in Loop: Header=BB51_28 Depth=2
	cmpq	$28, %rcx
	jle	.LBB51_30
# BB#33:                                # %tailrecurse.i.i
                                        #   in Loop: Header=BB51_28 Depth=2
	cmpq	$119, %rcx
	jg	.LBB51_40
# BB#34:                                # %tailrecurse.i.i
                                        #   in Loop: Header=BB51_28 Depth=2
	cmpq	$51, %rcx
	jg	.LBB51_43
# BB#35:                                # %tailrecurse.i.i
                                        #   in Loop: Header=BB51_28 Depth=2
	cmpq	$43, %rcx
	ja	.LBB51_45
# BB#36:                                # %tailrecurse.i.i
                                        #   in Loop: Header=BB51_28 Depth=2
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$11115375362048, %rsi   # imm = 0xA1C00000000
	testq	%rsi, %rdx
	jne	.LBB51_31
# BB#37:                                # %tailrecurse.i.i
                                        #   in Loop: Header=BB51_28 Depth=2
	cmpq	$29, %rcx
	je	.LBB51_46
# BB#38:                                # %tailrecurse.i.i
                                        #   in Loop: Header=BB51_28 Depth=2
	cmpq	$39, %rcx
	je	.LBB51_39
	jmp	.LBB51_45
.LBB51_40:                              # %tailrecurse.i.i
                                        #   in Loop: Header=BB51_28 Depth=2
	leaq	-127(%rcx), %rdx
	andq	%r13, %rdx
	cmpq	$2, %rdx
	jb	.LBB51_39
# BB#41:                                # %tailrecurse.i.i
                                        #   in Loop: Header=BB51_9 Depth=1
	cmpq	$120, %rcx
	je	.LBB51_44
	jmp	.LBB51_45
	.align	16, 0x90
.LBB51_42:                              # %tailrecurse.i.i
                                        #   in Loop: Header=BB51_9 Depth=1
	cmpq	$170, %rcx
	jne	.LBB51_45
	jmp	.LBB51_46
	.align	16, 0x90
.LBB51_30:                              # %tailrecurse.i.i
                                        #   in Loop: Header=BB51_9 Depth=1
	testq	%rcx, %rcx
	jne	.LBB51_45
.LBB51_31:                              # %sw.bb3.i.i
                                        #   in Loop: Header=BB51_9 Depth=1
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$16, %rax
	je	.LBB51_45
# BB#32:                                # %sw.bb3.i.i
                                        #   in Loop: Header=BB51_9 Depth=1
	cmpq	$23, %rax
	je	.LBB51_45
	jmp	.LBB51_46
.LBB51_43:                              # %tailrecurse.i.i
                                        #   in Loop: Header=BB51_9 Depth=1
	cmpq	$52, %rcx
	jne	.LBB51_45
.LBB51_44:                              # %lvalue_p.exit.i
                                        #   in Loop: Header=BB51_9 Depth=1
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$18, %rax
	je	.LBB51_46
	.align	16, 0x90
.LBB51_45:                              # %if.then.i
                                        #   in Loop: Header=BB51_9 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	error
.LBB51_46:                              # %lvalue_or_else.exit
                                        #   in Loop: Header=BB51_9 Depth=1
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB51_9
# BB#17:                                # %for.cond99.preheader
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB51_24
# BB#18:
	movq	40(%rsp), %rax          # 8-byte Reload
	.align	16, 0x90
.LBB51_19:                              # %for.body101
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB51_20 Depth 2
	movq	global_trees(%rip), %rcx
	movq	32(%rax), %rsi
	.align	16, 0x90
.LBB51_20:                              # %while.cond105
                                        #   Parent Loop BB51_19 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rsi, %rdx
	movzbl	16(%rdx), %esi
	addq	$-114, %rsi
	andq	%r13, %rsi
	cmpq	$2, %rsi
	ja	.LBB51_23
# BB#21:                                # %land.lhs.true123
                                        #   in Loop: Header=BB51_20 Depth=2
	movq	32(%rdx), %rsi
	cmpq	%rcx, %rsi
	je	.LBB51_23
# BB#22:                                # %land.rhs128
                                        #   in Loop: Header=BB51_20 Depth=2
	movq	8(%rdx), %rdi
	movq	8(%rsi), %rbx
	movl	60(%rdi), %edi
	movl	60(%rbx), %ebx
	xorq	%rdi, %rbx
	testq	$65024, %rbx            # imm = 0xFE00
	je	.LBB51_20
.LBB51_23:                              # %while.end152
                                        #   in Loop: Header=BB51_19 Depth=1
	movq	%rdx, 32(%rax)
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB51_19
.LBB51_24:                              # %for.cond159.preheader
	movq	32(%rsp), %r14          # 8-byte Reload
	testq	%r14, %r14
	je	.LBB51_27
# BB#25:
	movq	%r14, %rbx
	.align	16, 0x90
.LBB51_26:                              # %for.body161
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rdi
	callq	default_function_array_conversion
	movq	%rax, 32(%rbx)
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB51_26
.LBB51_27:                              # %for.end170
	movq	$165, %rdi
	xorq	%rax, %rax
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%r14, %r8
	movq	24(%rsp), %r9           # 8-byte Reload
	callq	build_stmt
	movq	%rax, %rdi
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	add_stmt  # TAILCALL
.Ltmp602:
	.size	build_asm_stmt, .Ltmp602-build_asm_stmt
	.cfi_endproc

	.globl	c_expand_asm_operands
	.align	16, 0x90
	.type	c_expand_asm_operands,@function
c_expand_asm_operands:                  # @c_expand_asm_operands
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp606:
	.cfi_def_cfa_offset 16
.Ltmp607:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp608:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$24, %rsp
.Ltmp609:
	.cfi_offset %rbx, -56
.Ltmp610:
	.cfi_offset %r12, -48
.Ltmp611:
	.cfi_offset %r13, -40
.Ltmp612:
	.cfi_offset %r14, -32
.Ltmp613:
	.cfi_offset %r15, -24
	movq	%r9, -48(%rbp)          # 8-byte Spill
	movq	%r8, -56(%rbp)          # 8-byte Spill
	movq	%rcx, %r14
	movq	%rdx, %r12
	movq	%rsi, %r13
	movq	%rdi, %rbx
	movq	%r13, %rdi
	callq	list_length
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	15(,%rax,8), %rax
	andq	$-16, %rax
	movq	%rsp, %r15
	subq	%rax, %r15
	movq	%r15, %rsp
	testq	%r13, %r13
	je	.LBB52_20
# BB#1:
	movq	%rbx, %rdi
	movq	16(%rbp), %r10
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	-56(%rbp), %r8          # 8-byte Reload
	movq	-48(%rbp), %r9          # 8-byte Reload
	movq	%r15, %rax
	movq	%r13, %rsi
	.align	16, 0x90
.LBB52_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rsi), %rbx
	movq	%rbx, (%rax)
	movq	(%rsi), %rsi
	addq	$8, %rax
	testq	%rsi, %rsi
	jne	.LBB52_2
# BB#3:                                 # %for.end
	subq	$16, %rsp
	movq	%r10, (%rsp)
	movq	%r13, %rsi
	callq	expand_asm_operands
	addq	$16, %rsp
	testq	%r13, %r13
	je	.LBB52_19
# BB#4:
	xorq	%r12, %r12
	movabsq	$.L.str140, %rbx
	.align	16, 0x90
.LBB52_5:                               # %for.body3
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB52_10 Depth 2
	movq	(%r15,%r12,8), %r14
	movq	32(%r13), %rdx
	cmpq	%rdx, %r14
	jne	.LBB52_6
# BB#7:                                 # %if.else
                                        #   in Loop: Header=BB52_5 Depth=1
	movl	16(%r14), %eax
	testq	$4096, %rax             # imm = 0x1000
	jne	.LBB52_10
# BB#8:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB52_5 Depth=1
	movq	8(%r14), %rcx
	movl	16(%rcx), %ecx
	testq	$4096, %rcx             # imm = 0x1000
	jne	.LBB52_10
# BB#9:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB52_5 Depth=1
	andq	$33554686, %rcx         # imm = 0x20000FE
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$33554452, %rcx         # imm = 0x2000014
	jne	.LBB52_18
	jmp	.LBB52_10
	.align	16, 0x90
.LBB52_13:                              # %if.then.i.tailrecurse.i_crit_edge
                                        #   in Loop: Header=BB52_10 Depth=2
	movl	16(%rcx), %eax
	movq	%rcx, %r14
.LBB52_10:                              # %tailrecurse.i
                                        #   Parent Loop BB52_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rax
	cmpq	$34, %rax
	je	.LBB52_15
# BB#11:                                # %tailrecurse.i
                                        #   in Loop: Header=BB52_10 Depth=2
	cmpq	$39, %rax
	jne	.LBB52_17
# BB#12:                                # %if.then.i
                                        #   in Loop: Header=BB52_10 Depth=2
	movq	32(%r14), %rcx
	movq	8(%rcx), %rax
	movzbl	17(%rax), %eax
	testq	$16, %rax
	jne	.LBB52_13
# BB#14:                                # %if.else.i
                                        #   in Loop: Header=BB52_5 Depth=1
	movq	40(%r14), %rax
	movq	72(%rax), %rax
	movq	32(%rax), %rdx
	xorq	%rax, %rax
	movabsq	$.L.str82, %rdi
	jmp	.LBB52_16
	.align	16, 0x90
.LBB52_6:                               # %if.then
                                        #   in Loop: Header=BB52_5 Depth=1
	movq	$115, %rsi
	movq	%r14, %rdi
	callq	build_modify_expr
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	callq	free_temp_slots
	movq	%r14, 32(%r13)
	jmp	.LBB52_18
.LBB52_15:                              # %if.then18.i
                                        #   in Loop: Header=BB52_5 Depth=1
	movq	72(%r14), %rax
	movq	32(%rax), %rdx
	xorq	%rax, %rax
	movabsq	$.L.str83, %rdi
.LBB52_16:                              # %for.inc55
                                        #   in Loop: Header=BB52_5 Depth=1
	movq	%rbx, %rsi
	callq	pedwarn
	jmp	.LBB52_18
.LBB52_17:                              # %if.else24.i
                                        #   in Loop: Header=BB52_5 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str84, %rdi
	movq	%rbx, %rsi
	callq	pedwarn
	.align	16, 0x90
.LBB52_18:                              # %for.inc55
                                        #   in Loop: Header=BB52_5 Depth=1
	movq	(%r13), %r13
	incq	%r12
	testq	%r13, %r13
	jne	.LBB52_5
	jmp	.LBB52_19
.LBB52_20:                              # %for.end.thread
	subq	$16, %rsp
	movq	16(%rbp), %rax
	movq	%rax, (%rsp)
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	-56(%rbp), %r8          # 8-byte Reload
	movq	-48(%rbp), %r9          # 8-byte Reload
	callq	expand_asm_operands
	addq	$16, %rsp
.LBB52_19:                              # %for.end59
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	jmp	emit_queue  # TAILCALL
.Ltmp614:
	.size	c_expand_asm_operands, .Ltmp614-c_expand_asm_operands
	.cfi_endproc

	.globl	c_expand_return
	.align	16, 0x90
	.type	c_expand_return,@function
c_expand_return:                        # @c_expand_return
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp618:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp619:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp620:
	.cfi_def_cfa_offset 32
.Ltmp621:
	.cfi_offset %rbx, -24
.Ltmp622:
	.cfi_offset %r14, -16
	movq	current_function_decl(%rip), %rax
	movq	8(%rax), %rcx
	movzbl	17(%rax), %eax
	movq	8(%rcx), %rbx
	movq	%rdi, %r14
	testq	$8, %rax
	je	.LBB53_2
# BB#1:                                 # %if.then
	movabsq	$.L.str141, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB53_2:                               # %if.end
	testq	%r14, %r14
	je	.LBB53_3
# BB#7:                                 # %if.else
	testq	%rbx, %rbx
	je	.LBB53_9
# BB#8:                                 # %lor.lhs.false17
	movzbl	16(%rbx), %eax
	cmpq	$5, %rax
	jne	.LBB53_12
.LBB53_9:                               # %if.then23
	movl	pedantic(%rip), %eax
	movq	$1, %rcx
	movl	%ecx, current_function_returns_null(%rip)
	testq	%rax, %rax
	jne	.LBB53_11
# BB#10:                                # %lor.lhs.false25
	movq	8(%r14), %rax
	movzbl	16(%rax), %eax
	cmpq	$5, %rax
	je	.LBB53_32
.LBB53_11:                              # %if.then33
	movabsq	$.L.str143, %rdi
	xorq	%rax, %rax
	callq	pedwarn
	jmp	.LBB53_32
.LBB53_3:                               # %if.then6
	movq	$1, %rax
	movl	warn_return_type(%rip), %ecx
	movl	flag_isoc99(%rip), %edx
	movl	%eax, current_function_returns_null(%rip)
	orq	%rcx, %rdx
	xorq	%r14, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	je	.LBB53_32
# BB#4:                                 # %if.then6
	testq	%rbx, %rbx
	je	.LBB53_32
# BB#5:                                 # %land.lhs.true9
	movzbl	16(%rbx), %eax
	cmpq	$5, %rax
	je	.LBB53_32
# BB#6:                                 # %if.then14
	movabsq	$.L.str142, %rdi
	xorq	%rax, %rax
	xorq	%r14, %r14
	callq	pedwarn_c99
	jmp	.LBB53_32
.LBB53_12:                              # %if.else35
	movabsq	$.L.str144, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	convert_for_assignment
	movq	current_function_decl(%rip), %rcx
	movq	global_trees(%rip), %rdx
	movq	96(%rcx), %rbx
	movq	$1, %rcx
	movl	%ecx, current_function_returns_value(%rip)
	cmpq	%rdx, %rax
	je	.LBB53_33
# BB#13:                                # %if.end38
	movq	8(%rbx), %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r14, %rcx
	jmp	.LBB53_14
	.align	16, 0x90
.LBB53_24:                              # %while.body.backedge
                                        #   in Loop: Header=BB53_14 Depth=1
	movq	(%rdx), %rcx
.LBB53_14:                              # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB53_18 Depth 2
	movzbl	16(%rcx), %edx
	cmpq	$113, %rdx
	jg	.LBB53_21
# BB#15:                                # %while.body
                                        #   in Loop: Header=BB53_14 Depth=1
	cmpq	$59, %rdx
	je	.LBB53_23
# BB#16:                                # %while.body
                                        #   in Loop: Header=BB53_14 Depth=1
	cmpq	$60, %rdx
	jne	.LBB53_31
# BB#17:                                # %sw.bb46
                                        #   in Loop: Header=BB53_14 Depth=1
	leaq	32(%rcx), %rdx
	addq	$40, %rcx
	.align	16, 0x90
.LBB53_18:                              # %while.cond50
                                        #   Parent Loop BB53_14 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rcx), %rcx
	movq	8(%rcx), %rsi
	movzbl	16(%rsi), %esi
	orq	$2, %rsi
	cmpq	$15, %rsi
	je	.LBB53_31
# BB#19:                                # %land.rhs
                                        #   in Loop: Header=BB53_18 Depth=2
	movzbl	16(%rcx), %edi
	addq	$-114, %rdi
	andq	%rax, %rdi
	addq	$32, %rcx
	cmpq	$3, %rdi
	jb	.LBB53_18
# BB#20:                                # %while.end
                                        #   in Loop: Header=BB53_14 Depth=1
	cmpq	$15, %rsi
	jne	.LBB53_24
	jmp	.LBB53_31
	.align	16, 0x90
.LBB53_21:                              # %while.body
                                        #   in Loop: Header=BB53_14 Depth=1
	cmpq	$121, %rdx
	je	.LBB53_25
# BB#22:                                # %while.body
                                        #   in Loop: Header=BB53_14 Depth=1
	addq	$-114, %rdx
	andq	%rax, %rdx
	cmpq	$2, %rdx
	ja	.LBB53_31
.LBB53_23:                              # %sw.bb
                                        #   in Loop: Header=BB53_14 Depth=1
	addq	$32, %rcx
	movq	%rcx, %rdx
	jmp	.LBB53_24
	.align	16, 0x90
.LBB53_25:                              # %while.cond110
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rcx), %rcx
	movl	16(%rcx), %edx
	movq	%rdx, %rsi
	andq	$255, %rsi
	movzbl	tree_code_type(%rsi), %edi
	cmpq	$114, %rdi
	je	.LBB53_25
# BB#26:                                # %while.end122
	cmpq	$34, %rsi
	jne	.LBB53_31
# BB#27:                                # %land.lhs.true129
	testq	$256, 48(%rcx)          # imm = 0x100
	jne	.LBB53_31
# BB#28:                                # %land.lhs.true129
	andq	$262144, %rdx           # imm = 0x40000
	testq	%rax, %rdx
	jne	.LBB53_31
# BB#29:                                # %land.lhs.true141
	movq	80(%rcx), %rax
	movq	current_function_decl(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB53_31
# BB#30:                                # %if.then145
	movabsq	$.L.str145, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB53_31:                              # %while.end147
	movq	8(%rbx), %rsi
	movq	$48, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	build
	movq	%rax, %r14
.LBB53_32:                              # %if.end152
	movq	%r14, %rdi
	callq	build_return_stmt
	movq	%rax, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	add_stmt  # TAILCALL
.LBB53_33:                              # %return
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp623:
	.size	c_expand_return, .Ltmp623-c_expand_return
	.cfi_endproc

	.globl	c_start_case
	.align	16, 0x90
	.type	c_start_case,@function
c_start_case:                           # @c_start_case
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp628:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp629:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp630:
	.cfi_def_cfa_offset 32
.Ltmp631:
	.cfi_offset %rbx, -32
.Ltmp632:
	.cfi_offset %r14, -24
.Ltmp633:
	.cfi_offset %r15, -16
	movq	global_trees(%rip), %rax
	movq	%rdi, %r15
	cmpq	%r15, %rax
	je	.LBB54_1
# BB#2:                                 # %if.then
	movq	8(%r15), %r14
	movzbl	16(%r14), %ecx
	cmpq	$12, %rcx
	ja	.LBB54_4
# BB#3:                                 # %if.then
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$7233, %rax             # imm = 0x1C41
	je	.LBB54_4
# BB#5:                                 # %if.else
	movl	warn_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB54_10
# BB#6:                                 # %if.else
	movl	in_system_header(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB54_10
# BB#7:                                 # %land.lhs.true35
	movq	128(%r14), %rax
	movq	integer_types+56(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB54_9
# BB#8:                                 # %land.lhs.true35
	movq	integer_types+64(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB54_10
.LBB54_9:                               # %if.then39
	movabsq	$.L.str147, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB54_10:                              # %if.end
	movq	%r15, %rdi
	callq	default_conversion
	movq	%rax, %r15
	jmp	.LBB54_11
.LBB54_1:
	movq	%r15, %r14
	jmp	.LBB54_11
.LBB54_4:                               # %if.then29
	movabsq	$.L.str146, %rdi
	xorq	%rax, %rax
	callq	error
	movq	global_trees+88(%rip), %r15
.LBB54_11:                              # %if.end43
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	movq	$162, %rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r14, %rcx
	callq	build_stmt
	movq	%rax, %r14
	movq	%r14, (%rbx)
	movabsq	$case_compare, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	splay_tree_new
	movq	switch_stack(%rip), %rcx
	movq	%rax, 8(%rbx)
	movq	%rcx, 16(%rbx)
	movq	%rbx, switch_stack(%rip)
	movq	%r14, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	add_stmt  # TAILCALL
.Ltmp634:
	.size	c_start_case, .Ltmp634-c_start_case
	.cfi_endproc

	.globl	do_case
	.align	16, 0x90
	.type	do_case,@function
do_case:                                # @do_case
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp637:
	.cfi_def_cfa_offset 16
.Ltmp638:
	.cfi_offset %rbx, -16
	movq	switch_stack(%rip), %rdx
	movq	%rsi, %rax
	movq	%rdi, %rcx
	testq	%rdx, %rdx
	je	.LBB55_3
# BB#1:                                 # %if.then
	movq	(%rdx), %rsi
	movq	8(%rdx), %rdi
	movq	32(%rsi), %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	c_add_case_label
	movq	global_trees(%rip), %rcx
	xorq	%rbx, %rbx
	cmpq	%rcx, %rax
	je	.LBB55_7
# BB#2:                                 # %select.mid
	movq	%rax, %rbx
	jmp	.LBB55_7
.LBB55_3:                               # %if.else
	testq	%rcx, %rcx
	je	.LBB55_5
# BB#4:                                 # %if.then3
	movabsq	$.L.str148, %rdi
	jmp	.LBB55_6
.LBB55_5:                               # %if.else4
	movabsq	$.L.str149, %rdi
.LBB55_6:                               # %if.end6
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	error
.LBB55_7:                               # %if.end6
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp639:
	.size	do_case, .Ltmp639-do_case
	.cfi_endproc

	.globl	c_finish_case
	.align	16, 0x90
	.type	c_finish_case,@function
c_finish_case:                          # @c_finish_case
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp643:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp644:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp645:
	.cfi_def_cfa_offset 32
.Ltmp646:
	.cfi_offset %rbx, -24
.Ltmp647:
	.cfi_offset %r14, -16
	movq	switch_stack(%rip), %rbx
	movq	(%rbx), %rax
	movq	(%rax), %rcx
	movq	%rcx, 40(%rax)
	movq	(%rbx), %r14
	movq	$0, (%r14)
	callq	current_stmt_tree
	movq	%r14, (%rax)
	movq	switch_stack(%rip), %rax
	movq	16(%rax), %rax
	movq	%rax, switch_stack(%rip)
	movq	8(%rbx), %rdi
	callq	splay_tree_delete
	movq	%rbx, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	free  # TAILCALL
.Ltmp648:
	.size	c_finish_case, .Ltmp648-c_finish_case
	.cfi_endproc

	.align	16, 0x90
	.type	add_pending_init,@function
add_pending_init:                       # @add_pending_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp655:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp656:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp657:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp658:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp659:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp660:
	.cfi_def_cfa_offset 64
.Ltmp661:
	.cfi_offset %rbx, -48
.Ltmp662:
	.cfi_offset %r12, -40
.Ltmp663:
	.cfi_offset %r13, -32
.Ltmp664:
	.cfi_offset %r14, -24
.Ltmp665:
	.cfi_offset %r15, -16
	movq	constructor_type(%rip), %rax
	movzbl	16(%rax), %eax
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %r12
	cmpq	$18, %rax
	jne	.LBB57_12
# BB#1:                                 # %while.cond.preheader
	movq	constructor_pending_elts(%rip), %rax
	testq	%rax, %rax
	je	.LBB57_2
# BB#3:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB57_4:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rbx
	movq	32(%rbx), %rsi
	movq	%r12, %rdi
	callq	tree_int_cst_lt
	testq	%r15, %rax
	jne	.LBB57_5
# BB#6:                                 # %if.else
                                        #   in Loop: Header=BB57_4 Depth=1
	movq	32(%rbx), %rdi
	movq	%r12, %rsi
	callq	tree_int_cst_lt
	testq	%r15, %rax
	je	.LBB57_9
# BB#7:                                 # %if.then7
                                        #   in Loop: Header=BB57_4 Depth=1
	leaq	8(%rbx), %r13
	jmp	.LBB57_8
	.align	16, 0x90
.LBB57_5:                               # %if.then3
                                        #   in Loop: Header=BB57_4 Depth=1
	movq	%rbx, %r13
.LBB57_8:                               # %while.cond.backedge
                                        #   in Loop: Header=BB57_4 Depth=1
	movq	(%r13), %rax
	testq	%rax, %rax
	jne	.LBB57_4
	jmp	.LBB57_19
.LBB57_12:                              # %if.else18
	movq	%r12, %rdi
	callq	bit_position
	movq	constructor_pending_elts(%rip), %rcx
	movq	%rax, %r15
	testq	%rcx, %rcx
	je	.LBB57_2
# BB#13:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB57_14:                              # %while.body22
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rbx
	movq	32(%rbx), %rdi
	callq	bit_position
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	tree_int_cst_lt
	testq	%r14, %rax
	jne	.LBB57_15
# BB#16:                                # %if.else29
                                        #   in Loop: Header=BB57_14 Depth=1
	movq	32(%rbx), %rax
	cmpq	%r12, %rax
	je	.LBB57_9
# BB#17:                                # %if.then32
                                        #   in Loop: Header=BB57_14 Depth=1
	leaq	8(%rbx), %r13
	jmp	.LBB57_18
	.align	16, 0x90
.LBB57_15:                              # %if.then27
                                        #   in Loop: Header=BB57_14 Depth=1
	movq	%rbx, %r13
.LBB57_18:                              # %while.cond20.backedge
                                        #   in Loop: Header=BB57_14 Depth=1
	movq	(%r13), %rcx
	testq	%rcx, %rcx
	jne	.LBB57_14
	jmp	.LBB57_19
.LBB57_2:
	xorq	%rbx, %rbx
	movabsq	$constructor_pending_elts, %r13
.LBB57_19:                              # %if.end48
	movq	$48, %rdi
	callq	ggc_alloc
	movq	%r12, 32(%rax)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 40(%rax)
	movq	%rax, (%r13)
	movq	%rbx, 16(%rax)
	xorq	%rcx, %rcx
	testq	%rbx, %rbx
	movl	%ecx, 24(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	je	.LBB57_66
# BB#20:
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB57_21:                              # %while.body56
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdx
	movq	(%rdx), %rdi
	movl	24(%rdx), %esi
	cmpq	%rdi, %rax
	jne	.LBB57_43
# BB#22:                                # %if.then59
                                        #   in Loop: Header=BB57_21 Depth=1
	testq	%rcx, %rsi
	movq	%rcx, %rdi
	je	.LBB57_65
	jmp	.LBB57_23
	.align	16, 0x90
.LBB57_43:                              # %if.else144
                                        #   in Loop: Header=BB57_21 Depth=1
	movq	$1, %rdi
	testq	%rcx, %rsi
	jne	.LBB57_44
.LBB57_65:                              # %if.end236
                                        #   in Loop: Header=BB57_21 Depth=1
	movl	%edi, 24(%rdx)
	movq	16(%rdx), %rbx
	testq	%rbx, %rbx
	movq	%rdx, %rax
	jne	.LBB57_21
	jmp	.LBB57_66
.LBB57_9:                               # %if.else8
	movq	40(%rbx), %rax
	movzbl	17(%rax), %eax
	testq	$1, %rax
	je	.LBB57_11
# BB#10:                                # %if.then14
	movabsq	$.L.str152, %rdi
	callq	warning_init
.LBB57_11:                              # %if.end
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, 40(%rbx)
	jmp	.LBB57_66
.LBB57_23:                              # %if.else64
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jns	.LBB57_64
# BB#24:                                # %if.then67
	movslq	24(%rax), %rcx
	movq	8(%rax), %rsi
	testq	%rcx, %rcx
	js	.LBB57_25
# BB#32:                                # %if.else96
	movq	(%rsi), %rcx
	testq	%rcx, %rcx
	movq	%rcx, 8(%rax)
	je	.LBB57_34
# BB#33:                                # %if.then102
	movq	%rax, 16(%rcx)
.LBB57_34:                              # %if.end105
	movq	%rax, (%rsi)
	movq	8(%rsi), %rcx
	testq	%rcx, %rcx
	movq	%rcx, (%rdx)
	je	.LBB57_36
# BB#35:                                # %if.then111
	movq	%rdx, 16(%rcx)
.LBB57_36:                              # %if.end114
	movq	%rdx, 8(%rsi)
	movl	24(%rsi), %edi
	movq	$31, %rcx
	shrq	%cl, %rdi
	movl	%edi, 24(%rdx)
	movslq	24(%rsi), %rdi
	xorq	%rcx, %rcx
	testq	%rdi, %rdi
	movq	$-1, %rdi
	jg	.LBB57_38
# BB#37:                                # %if.end114
	xorq	%rdi, %rdi
.LBB57_38:                              # %if.end114
	movl	%edi, 24(%rax)
	movl	%ecx, 24(%rsi)
	movq	16(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%rsi, 16(%rdx)
	movq	%rsi, 16(%rax)
	movq	%rcx, 16(%rsi)
	je	.LBB57_42
# BB#39:                                # %if.then129
	movq	(%rcx), %rax
	cmpq	%rdx, %rax
	je	.LBB57_40
# BB#41:                                # %if.else135
	movq	%rsi, 8(%rcx)
	jmp	.LBB57_66
.LBB57_44:                              # %if.else150
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jle	.LBB57_64
# BB#45:                                # %if.then154
	movslq	24(%rax), %rcx
	movq	(%rax), %rsi
	testq	%rcx, %rcx
	jle	.LBB57_53
# BB#46:                                # %if.then158
	movq	%rsi, 8(%rdx)
	testq	%rsi, %rsi
	je	.LBB57_48
# BB#47:                                # %if.then163
	movq	%rdx, 16(%rsi)
.LBB57_48:                              # %if.end166
	movq	%rdx, (%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 24(%rdx)
	movl	%ecx, 24(%rax)
	movq	16(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%rax, 16(%rdx)
	movq	%rcx, 16(%rax)
	je	.LBB57_52
# BB#49:                                # %if.then174
	movq	(%rcx), %rsi
	cmpq	%rdx, %rsi
	je	.LBB57_50
# BB#51:                                # %if.else180
	movq	%rax, 8(%rcx)
	jmp	.LBB57_66
.LBB57_64:                              # %if.else141
	xorq	%rax, %rax
	movl	%eax, 24(%rdx)
.LBB57_66:                              # %while.end238
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB57_53:                              # %if.else185
	movq	8(%rsi), %rcx
	testq	%rcx, %rcx
	movq	%rcx, (%rax)
	je	.LBB57_55
# BB#54:                                # %if.then192
	movq	%rax, 16(%rcx)
.LBB57_55:                              # %if.end195
	movq	%rax, 8(%rsi)
	movq	(%rsi), %rcx
	testq	%rcx, %rcx
	movq	%rcx, 8(%rdx)
	je	.LBB57_57
# BB#56:                                # %if.then201
	movq	%rdx, 16(%rcx)
.LBB57_57:                              # %if.end204
	movq	%rdx, (%rsi)
	movl	24(%rsi), %edi
	movq	$31, %rcx
	shrq	%cl, %rdi
	movl	%edi, 24(%rax)
	movslq	24(%rsi), %rdi
	xorq	%rcx, %rcx
	testq	%rdi, %rdi
	movq	$-1, %rdi
	jg	.LBB57_59
# BB#58:                                # %if.end204
	xorq	%rdi, %rdi
.LBB57_59:                              # %if.end204
	movl	%edi, 24(%rdx)
	movl	%ecx, 24(%rsi)
	movq	16(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%rsi, 16(%rdx)
	movq	%rsi, 16(%rax)
	movq	%rcx, 16(%rsi)
	je	.LBB57_63
# BB#60:                                # %if.then221
	movq	(%rcx), %rax
	cmpq	%rdx, %rax
	je	.LBB57_61
# BB#62:                                # %if.else227
	movq	%rsi, 8(%rcx)
	jmp	.LBB57_66
.LBB57_52:                              # %if.else183
	movq	%rax, constructor_pending_elts(%rip)
	jmp	.LBB57_66
.LBB57_25:                              # %if.then70
	movq	%rsi, (%rdx)
	testq	%rsi, %rsi
	je	.LBB57_27
# BB#26:                                # %if.then75
	movq	%rdx, 16(%rsi)
.LBB57_27:                              # %if.end78
	movq	%rdx, 8(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 24(%rdx)
	movl	%ecx, 24(%rax)
	movq	16(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%rax, 16(%rdx)
	movq	%rcx, 16(%rax)
	je	.LBB57_31
# BB#28:                                # %if.then86
	movq	(%rcx), %rsi
	cmpq	%rdx, %rsi
	je	.LBB57_29
# BB#30:                                # %if.else91
	movq	%rax, 8(%rcx)
	jmp	.LBB57_66
.LBB57_50:                              # %if.then178
	movq	%rax, (%rcx)
	jmp	.LBB57_66
.LBB57_42:                              # %if.else138
	movq	%rsi, constructor_pending_elts(%rip)
	jmp	.LBB57_66
.LBB57_63:                              # %if.else230
	movq	%rsi, constructor_pending_elts(%rip)
	jmp	.LBB57_66
.LBB57_40:                              # %if.then133
	movq	%rsi, (%rcx)
	jmp	.LBB57_66
.LBB57_61:                              # %if.then225
	movq	%rsi, (%rcx)
	jmp	.LBB57_66
.LBB57_31:                              # %if.else94
	movq	%rax, constructor_pending_elts(%rip)
	jmp	.LBB57_66
.LBB57_29:                              # %if.then89
	movq	%rax, (%rcx)
	jmp	.LBB57_66
.Ltmp666:
	.size	add_pending_init, .Ltmp666-add_pending_init
	.cfi_endproc

	.align	16, 0x90
	.type	valid_compound_expr_initializer,@function
valid_compound_expr_initializer:        # @valid_compound_expr_initializer
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp670:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp671:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp672:
	.cfi_def_cfa_offset 32
.Ltmp673:
	.cfi_offset %rbx, -24
.Ltmp674:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movl	16(%rbx), %eax
	movq	%rsi, %r14
	jmp	.LBB58_1
	.align	16, 0x90
.LBB58_3:                               # %if.end
                                        #   in Loop: Header=BB58_1 Depth=1
	movq	40(%rbx), %rbx
	movl	16(%rbx), %eax
.LBB58_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$47, %rcx
	jne	.LBB58_4
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB58_1 Depth=1
	movq	32(%rbx), %rdi
	movq	%r14, %rsi
	callq	valid_compound_expr_initializer
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB58_3
	jmp	.LBB58_8
.LBB58_4:                               # %if.else
	testq	$512, %rax              # imm = 0x200
	jne	.LBB58_7
# BB#5:                                 # %land.lhs.true
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	initializer_constant_valid_p
	testq	%rax, %rax
	jne	.LBB58_7
# BB#6:
	movq	global_trees(%rip), %rbx
.LBB58_7:                               # %land.lhs.true
	movq	%rbx, %rax
.LBB58_8:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp675:
	.size	valid_compound_expr_initializer, .Ltmp675-valid_compound_expr_initializer
	.cfi_endproc

	.align	16, 0x90
	.type	warn_for_assignment,@function
warn_for_assignment:                    # @warn_for_assignment
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp679:
	.cfi_def_cfa_offset 16
.Ltmp680:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp681:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
.Ltmp682:
	.cfi_offset %rbx, -48
.Ltmp683:
	.cfi_offset %r12, -40
.Ltmp684:
	.cfi_offset %r14, -32
.Ltmp685:
	.cfi_offset %r15, -24
	movq	%rcx, %r12
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r14
	testq	%rbx, %rbx
	jne	.LBB59_12
# BB#1:                                 # %if.then
	callq	maybe_building_objc_message_expr
	testq	%rax, %rax
	movabsq	$0, %rsi
	movabsq	$1, %rdi
	movq	%rsi, %rdx
	je	.LBB59_3
# BB#2:                                 # %if.then
	movq	%rdi, %rdx
.LBB59_3:                               # %if.then
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	$2, %rbx
	jle	.LBB59_5
# BB#4:                                 # %if.then
	movq	%rdi, %rsi
.LBB59_5:                               # %if.then
	andq	%rsi, %rdx
	je	.LBB59_7
# BB#6:
	addq	$-2, %r12
.LBB59_7:                               # %if.then
	testq	%rdx, %rdx
	jne	.LBB59_9
# BB#8:                                 # %if.then
	movq	%r15, %rax
.LBB59_9:                               # %if.then
	testq	%rax, %rax
	je	.LBB59_11
# BB#10:                                # %if.then4
	movl	24(%rax), %ecx
	movq	%rsp, %rbx
	addq	$64, %rcx
	andq	$-16, %rcx
	subq	%rcx, %rbx
	movq	%rbx, %rsp
	movq	32(%rax), %rcx
	movabsq	$.L.str174, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r12, %rdx
	callq	sprintf
	jmp	.LBB59_12
.LBB59_11:                              # %if.else
	movq	%rsp, %rbx
	addq	$-64, %rbx
	movq	%rbx, %rsp
	movabsq	$.L.str175, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r12, %rdx
	callq	sprintf
.LBB59_12:                              # %if.end18
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	pedwarn
	leaq	-32(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp686:
	.size	warn_for_assignment, .Ltmp686-warn_for_assignment
	.cfi_endproc

	.align	16, 0x90
	.type	type_lists_compatible_p,@function
type_lists_compatible_p:                # @type_lists_compatible_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp693:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp694:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp695:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp696:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp697:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp698:
	.cfi_def_cfa_offset 64
.Ltmp699:
	.cfi_offset %rbx, -48
.Ltmp700:
	.cfi_offset %r12, -40
.Ltmp701:
	.cfi_offset %r13, -32
.Ltmp702:
	.cfi_offset %r14, -24
.Ltmp703:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r12
	testq	%r12, %r12
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, %rax
	jne	.LBB60_2
# BB#1:                                 # %entry
	movq	%rdx, %rax
.LBB60_2:                               # %entry
	testq	%r14, %r14
	jne	.LBB60_4
# BB#3:                                 # %entry
	movq	%rdx, %rcx
.LBB60_4:                               # %entry
	movq	%r12, %rdx
	orq	%r14, %rdx
	movq	$1, %rbx
	jne	.LBB60_6
# BB#5:
	movq	$1, %r15
	jmp	.LBB60_41
.LBB60_6:
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	.align	16, 0x90
.LBB60_7:                               # %if.end
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB60_20 Depth 2
                                        #     Child Loop BB60_30 Depth 2
	xorq	%r15, %r15
	testq	$1, %rax
	jne	.LBB60_41
# BB#8:                                 # %if.end
                                        #   in Loop: Header=BB60_7 Depth=1
	testq	$1, %rcx
	jne	.LBB60_41
# BB#9:                                 # %if.end5
                                        #   in Loop: Header=BB60_7 Depth=1
	movq	32(%r12), %rdi
	movq	32(%r14), %rax
	testq	%rdi, %rdi
	jne	.LBB60_11
# BB#10:                                # %if.then7
                                        #   in Loop: Header=BB60_7 Depth=1
	movq	%rax, %rdi
	jmp	.LBB60_12
	.align	16, 0x90
.LBB60_11:                              # %if.else
                                        #   in Loop: Header=BB60_7 Depth=1
	testq	%rax, %rax
	jne	.LBB60_13
.LBB60_12:                              # %if.then16
                                        #   in Loop: Header=BB60_7 Depth=1
	callq	simple_type_promotes_to
	testq	%rax, %rax
	jne	.LBB60_41
.LBB60_34:                              # %if.end157
                                        #   in Loop: Header=BB60_7 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	%rsi, %r15
	jge	.LBB60_36
# BB#35:                                # %if.end157
                                        #   in Loop: Header=BB60_7 Depth=1
	movq	%rbx, %r15
.LBB60_36:                              # %if.end157
                                        #   in Loop: Header=BB60_7 Depth=1
	movq	(%r12), %r12
	movq	(%r14), %r14
	testq	%r12, %r12
	movabsq	$0, %rax
	jne	.LBB60_38
# BB#37:                                # %if.end157
                                        #   in Loop: Header=BB60_7 Depth=1
	movabsq	$1, %rax
.LBB60_38:                              # %if.end157
                                        #   in Loop: Header=BB60_7 Depth=1
	testq	%r14, %r14
	movabsq	$0, %rcx
	jne	.LBB60_40
# BB#39:                                # %if.end157
                                        #   in Loop: Header=BB60_7 Depth=1
	movabsq	$1, %rcx
.LBB60_40:                              # %if.end157
                                        #   in Loop: Header=BB60_7 Depth=1
	movq	%r12, %rdx
	orq	%r14, %rdx
	movq	%r15, %rbx
	jne	.LBB60_7
	jmp	.LBB60_41
	.align	16, 0x90
.LBB60_13:                              # %if.else23
                                        #   in Loop: Header=BB60_7 Depth=1
	movq	128(%rdi), %rdi
	movq	128(%rax), %rsi
	callq	comptypes
	movq	%rax, %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	jne	.LBB60_34
# BB#14:                                # %if.then31
                                        #   in Loop: Header=BB60_7 Depth=1
	movq	32(%r12), %rax
	movzbl	16(%rax), %ecx
	cmpq	$21, %rcx
	jne	.LBB60_24
# BB#15:                                # %land.lhs.true35
                                        #   in Loop: Header=BB60_7 Depth=1
	cmpq	$0, 96(%rax)
	je	.LBB60_17
# BB#16:                                # %lor.lhs.false40
                                        #   in Loop: Header=BB60_7 Depth=1
	movzbl	62(%rax), %ecx
	testq	$8, %rcx
	je	.LBB60_24
.LBB60_17:                              # %land.lhs.true47
                                        #   in Loop: Header=BB60_7 Depth=1
	movq	32(%rax), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB60_24
# BB#18:                                # %land.lhs.true56
                                        #   in Loop: Header=BB60_7 Depth=1
	movq	32(%r14), %rax
	movq	32(%rax), %rsi
	callq	tree_int_cst_equal
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB60_19
.LBB60_24:                              # %if.else84
                                        #   in Loop: Header=BB60_7 Depth=1
	movq	%rbx, %r13
	movq	32(%r14), %rax
	movzbl	16(%rax), %ecx
	cmpq	$21, %rcx
	jne	.LBB60_41
# BB#25:                                # %land.lhs.true92
                                        #   in Loop: Header=BB60_7 Depth=1
	cmpq	$0, 96(%rax)
	je	.LBB60_27
# BB#26:                                # %lor.lhs.false98
                                        #   in Loop: Header=BB60_7 Depth=1
	movzbl	62(%rax), %ecx
	testq	$8, %rcx
	je	.LBB60_41
.LBB60_27:                              # %land.lhs.true107
                                        #   in Loop: Header=BB60_7 Depth=1
	movq	32(%rax), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB60_41
# BB#28:                                # %land.lhs.true117
                                        #   in Loop: Header=BB60_7 Depth=1
	movq	32(%r12), %rax
	movq	32(%rax), %rsi
	callq	tree_int_cst_equal
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB60_41
# BB#29:                                # %if.then128
                                        #   in Loop: Header=BB60_7 Depth=1
	movq	32(%r14), %rax
	movq	24(%rax), %rbx
	jmp	.LBB60_30
	.align	16, 0x90
.LBB60_32:                              # %for.inc145
                                        #   in Loop: Header=BB60_30 Depth=2
	movq	(%rbx), %rbx
.LBB60_30:                              # %if.then128
                                        #   Parent Loop BB60_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB60_41
# BB#31:                                # %for.body136
                                        #   in Loop: Header=BB60_30 Depth=2
	movq	8(%rbx), %rdi
	movq	32(%r12), %rsi
	callq	comptypes
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB60_32
# BB#33:                                # %for.end148
                                        #   in Loop: Header=BB60_7 Depth=1
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	testq	%rbx, %rbx
	movabsq	$0, %r15
	movq	%r13, %rbx
	jne	.LBB60_34
	jmp	.LBB60_41
.LBB60_19:                              # %if.then67
                                        #   in Loop: Header=BB60_7 Depth=1
	movq	32(%r12), %rax
	movq	24(%rax), %r13
	jmp	.LBB60_20
	.align	16, 0x90
.LBB60_22:                              # %for.inc
                                        #   in Loop: Header=BB60_20 Depth=2
	movq	(%r13), %r13
.LBB60_20:                              # %if.then67
                                        #   Parent Loop BB60_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r13, %r13
	je	.LBB60_41
# BB#21:                                # %for.body
                                        #   in Loop: Header=BB60_20 Depth=2
	movq	8(%r13), %rdi
	movq	32(%r14), %rsi
	callq	comptypes
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB60_22
# BB#23:                                # %for.end
                                        #   in Loop: Header=BB60_7 Depth=1
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	testq	%r13, %r13
	movabsq	$0, %r15
	jne	.LBB60_34
.LBB60_41:                              # %return
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp704:
	.size	type_lists_compatible_p, .Ltmp704-type_lists_compatible_p
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"`%s' has an incomplete type"
	.size	.L.str, 28

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"struct"
	.size	.L.str1, 7

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"union"
	.size	.L.str2, 6

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"enum"
	.size	.L.str3, 5

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"invalid use of void expression"
	.size	.L.str4, 31

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"invalid use of flexible array member"
	.size	.L.str5, 37

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"invalid use of array with unspecified bounds"
	.size	.L.str6, 45

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"c-typeck.c"
	.size	.L.str7, 11

	.type	.L__FUNCTION__.incomplete_type_error,@object # @__FUNCTION__.incomplete_type_error
.L__FUNCTION__.incomplete_type_error:
	.asciz	"incomplete_type_error"
	.size	.L__FUNCTION__.incomplete_type_error, 22

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"invalid use of undefined type `%s %s'"
	.size	.L.str8, 38

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"invalid use of incomplete typedef `%s'"
	.size	.L.str9, 39

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"function types not truly compatible in ISO C"
	.size	.L.str10, 45

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"sizeof applied to a function type"
	.size	.L.str11, 34

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"sizeof applied to a void type"
	.size	.L.str12, 30

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"sizeof applied to an incomplete type"
	.size	.L.str13, 37

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"arithmetic on pointer to an incomplete type"
	.size	.L.str14, 44

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"void value not ignored as it ought to be"
	.size	.L.str15, 41

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"%s has no member named `%s'"
	.size	.L.str16, 28

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"structure"
	.size	.L.str17, 10

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"request for member `%s' in something not a structure or union"
	.size	.L.str18, 62

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"dereferencing pointer to incomplete type"
	.size	.L.str19, 41

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"dereferencing `void *' pointer"
	.size	.L.str20, 31

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"invalid type argument of `%s'"
	.size	.L.str21, 30

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"subscript missing in array reference"
	.size	.L.str22, 37

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"array subscript has type `char'"
	.size	.L.str23, 32

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"array subscript is not an integer"
	.size	.L.str24, 34

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"ISO C forbids subscripting `register' array"
	.size	.L.str25, 44

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"ISO C89 forbids subscripting non-lvalue array"
	.size	.L.str26, 46

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"subscript has type `char'"
	.size	.L.str27, 26

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"subscripted value is neither array nor pointer"
	.size	.L.str28, 47

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"array indexing"
	.size	.L.str29, 15

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"`%s' undeclared here (not in a function)"
	.size	.L.str30, 41

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"`%s' undeclared (first use in this function)"
	.size	.L.str31, 45

	.type	undeclared_variable_notice,@object # @undeclared_variable_notice
	.local	undeclared_variable_notice
	.comm	undeclared_variable_notice,1,1
	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"(Each undeclared identifier is reported only once"
	.size	.L.str32, 50

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"for each function it appears in.)"
	.size	.L.str33, 34

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"local declaration of `%s' hides instance variable"
	.size	.L.str34, 50

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"called object is not a function"
	.size	.L.str35, 32

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"suggest parentheses around + or - inside shift"
	.size	.L.str36, 47

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"suggest parentheses around && within ||"
	.size	.L.str37, 40

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"suggest parentheses around arithmetic in operand of |"
	.size	.L.str38, 54

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"suggest parentheses around comparison in operand of |"
	.size	.L.str39, 54

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"suggest parentheses around arithmetic in operand of ^"
	.size	.L.str40, 54

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"suggest parentheses around comparison in operand of ^"
	.size	.L.str41, 54

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"suggest parentheses around + or - in operand of &"
	.size	.L.str42, 50

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"suggest parentheses around comparison in operand of &"
	.size	.L.str43, 54

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"comparisons like X<=Y<=Z do not have their mathematical meaning"
	.size	.L.str44, 64

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"division by zero"
	.size	.L.str45, 17

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"right shift count is negative"
	.size	.L.str46, 30

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"right shift count >= width of type"
	.size	.L.str47, 35

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"left shift count is negative"
	.size	.L.str48, 29

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"left shift count >= width of type"
	.size	.L.str49, 34

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"shift count is negative"
	.size	.L.str50, 24

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"shift count >= width of type"
	.size	.L.str51, 29

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"comparing floating point with == or != is unsafe"
	.size	.L.str52, 49

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"ISO C forbids comparison of `void *' with function pointer"
	.size	.L.str53, 59

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"comparison of distinct pointer types lacks a cast"
	.size	.L.str54, 50

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"comparison between pointer and integer"
	.size	.L.str55, 39

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"ISO C forbids ordered comparisons of pointers to functions"
	.size	.L.str56, 59

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"comparison of complete and incomplete pointers"
	.size	.L.str57, 47

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"ordered comparison of pointer with integer zero"
	.size	.L.str58, 48

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"unordered comparison on non-floating point argument"
	.size	.L.str59, 52

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"comparison between signed and unsigned"
	.size	.L.str60, 39

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"comparison of promoted ~unsigned with constant"
	.size	.L.str61, 47

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"comparison of promoted ~unsigned with unsigned"
	.size	.L.str62, 47

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"wrong type argument to unary plus"
	.size	.L.str63, 34

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"wrong type argument to unary minus"
	.size	.L.str64, 35

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"ISO C does not support `~' for complex conjugation"
	.size	.L.str65, 51

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"wrong type argument to bit-complement"
	.size	.L.str66, 38

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"wrong type argument to abs"
	.size	.L.str67, 27

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"wrong type argument to conjugation"
	.size	.L.str68, 35

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"wrong type argument to unary exclamation mark"
	.size	.L.str69, 46

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"ISO C does not support `++' and `--' on complex types"
	.size	.L.str70, 54

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"wrong type argument to increment"
	.size	.L.str71, 33

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"wrong type argument to decrement"
	.size	.L.str72, 33

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"increment of pointer to unknown structure"
	.size	.L.str73, 42

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"decrement of pointer to unknown structure"
	.size	.L.str74, 42

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"invalid lvalue in increment"
	.size	.L.str75, 28

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"invalid lvalue in decrement"
	.size	.L.str76, 28

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"increment"
	.size	.L.str77, 10

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"decrement"
	.size	.L.str78, 10

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"invalid lvalue in unary `&'"
	.size	.L.str79, 28

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"attempt to take address of bit-field structure member `%s'"
	.size	.L.str80, 59

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"%s"
	.size	.L.str81, 3

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"%s of read-only member `%s'"
	.size	.L.str82, 28

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"%s of read-only variable `%s'"
	.size	.L.str83, 30

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"%s of read-only location"
	.size	.L.str84, 25

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"cannot take address of bit-field `%s'"
	.size	.L.str85, 38

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"global register variable `%s' used in nested function"
	.size	.L.str86, 54

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"register variable `%s' used in nested function"
	.size	.L.str87, 47

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"address of global register variable `%s' requested"
	.size	.L.str88, 51

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"cannot put object with volatile field into register"
	.size	.L.str89, 52

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"address of register variable `%s' requested"
	.size	.L.str90, 44

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	"signed and unsigned type in conditional expression"
	.size	.L.str91, 51

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"ISO C forbids conditional expr with only one void side"
	.size	.L.str92, 55

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	"ISO C forbids conditional expr between `void *' and function pointer"
	.size	.L.str93, 69

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"pointer type mismatch in conditional expression"
	.size	.L.str94, 48

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	"pointer/integer type mismatch in conditional expression"
	.size	.L.str95, 56

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"type mismatch in conditional expression"
	.size	.L.str96, 40

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"cast specifies array type"
	.size	.L.str97, 26

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"cast specifies function type"
	.size	.L.str98, 29

	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	"ISO C forbids casting nonscalar to the same type"
	.size	.L.str99, 49

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"ISO C forbids casts to union type"
	.size	.L.str100, 34

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"cast to union type from type not present in union"
	.size	.L.str102, 50

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	"cast adds new qualifiers to function type"
	.size	.L.str103, 42

	.type	.L.str104,@object       # @.str104
.L.str104:
	.asciz	"cast discards qualifiers from pointer target type"
	.size	.L.str104, 50

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	"cast from pointer to integer of different size"
	.size	.L.str105, 47

	.type	.L.str106,@object       # @.str106
.L.str106:
	.asciz	"cast does not match function type"
	.size	.L.str106, 34

	.type	.L.str107,@object       # @.str107
.L.str107:
	.asciz	"cast to pointer from integer of different size"
	.size	.L.str107, 47

	.type	.L.str108,@object       # @.str108
.L.str108:
	.asciz	"invalid lvalue in assignment"
	.size	.L.str108, 29

	.type	.L.str109,@object       # @.str109
.L.str109:
	.asciz	"assignment"
	.size	.L.str109, 11

	.type	.L.str110,@object       # @.str110
.L.str110:
	.asciz	"traditional C rejects automatic aggregate initialization"
	.size	.L.str110, 57

	.type	.L.str111,@object       # @.str111
.L.str111:
	.asciz	"(near initialization for `%s')"
	.size	.L.str111, 31

	.type	constructor_no_implicit,@object # @constructor_no_implicit
	.bss
	.globl	constructor_no_implicit
	.align	4
constructor_no_implicit:
	.long	0                       # 0x0
	.size	constructor_no_implicit, 4

	.type	constructor_decl,@object # @constructor_decl
	.local	constructor_decl
	.comm	constructor_decl,8,8
	.type	constructor_asmspec,@object # @constructor_asmspec
	.local	constructor_asmspec
	.comm	constructor_asmspec,8,8
	.type	require_constant_value,@object # @require_constant_value
	.local	require_constant_value
	.comm	require_constant_value,4,4
	.type	require_constant_elements,@object # @require_constant_elements
	.local	require_constant_elements
	.comm	require_constant_elements,4,4
	.type	constructor_stack,@object # @constructor_stack
	.comm	constructor_stack,8,8
	.type	constructor_range_stack,@object # @constructor_range_stack
	.comm	constructor_range_stack,8,8
	.type	constructor_elements,@object # @constructor_elements
	.local	constructor_elements
	.comm	constructor_elements,8,8
	.type	spelling,@object        # @spelling
	.local	spelling
	.comm	spelling,8,8
	.type	spelling_base,@object   # @spelling_base
	.local	spelling_base
	.comm	spelling_base,8,8
	.type	spelling_size,@object   # @spelling_size
	.local	spelling_size
	.comm	spelling_size,4,4
	.type	constructor_subconstants_deferred,@object # @constructor_subconstants_deferred
	.local	constructor_subconstants_deferred
	.comm	constructor_subconstants_deferred,4,4
	.type	constructor_top_level,@object # @constructor_top_level
	.local	constructor_top_level
	.comm	constructor_top_level,4,4
	.type	initializer_stack,@object # @initializer_stack
	.comm	initializer_stack,8,8
	.type	constructor_designated,@object # @constructor_designated
	.local	constructor_designated
	.comm	constructor_designated,4,4
	.type	.L.str112,@object       # @.str112
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str112:
	.asciz	"(anonymous)"
	.size	.L.str112, 12

	.type	missing_braces_mentioned,@object # @missing_braces_mentioned
	.local	missing_braces_mentioned
	.comm	missing_braces_mentioned,1,1
	.type	.L__FUNCTION__.finish_init,@object # @__FUNCTION__.finish_init
.L__FUNCTION__.finish_init:
	.asciz	"finish_init"
	.size	.L__FUNCTION__.finish_init, 12

	.type	constructor_type,@object # @constructor_type
	.local	constructor_type
	.comm	constructor_type,8,8
	.type	constructor_fields,@object # @constructor_fields
	.local	constructor_fields
	.comm	constructor_fields,8,8
	.type	constructor_index,@object # @constructor_index
	.local	constructor_index
	.comm	constructor_index,8,8
	.type	constructor_max_index,@object # @constructor_max_index
	.local	constructor_max_index
	.comm	constructor_max_index,8,8
	.type	constructor_unfilled_index,@object # @constructor_unfilled_index
	.local	constructor_unfilled_index
	.comm	constructor_unfilled_index,8,8
	.type	constructor_unfilled_fields,@object # @constructor_unfilled_fields
	.local	constructor_unfilled_fields
	.comm	constructor_unfilled_fields,8,8
	.type	constructor_bit_index,@object # @constructor_bit_index
	.local	constructor_bit_index
	.comm	constructor_bit_index,8,8
	.type	constructor_constant,@object # @constructor_constant
	.local	constructor_constant
	.comm	constructor_constant,4,4
	.type	constructor_simple,@object # @constructor_simple
	.local	constructor_simple
	.comm	constructor_simple,4,4
	.type	constructor_erroneous,@object # @constructor_erroneous
	.local	constructor_erroneous
	.comm	constructor_erroneous,4,4
	.type	constructor_pending_elts,@object # @constructor_pending_elts
	.local	constructor_pending_elts
	.comm	constructor_pending_elts,8,8
	.type	constructor_depth,@object # @constructor_depth
	.local	constructor_depth
	.comm	constructor_depth,4,4
	.type	constructor_incremental,@object # @constructor_incremental
	.local	constructor_incremental
	.comm	constructor_incremental,4,4
	.type	designator_depth,@object # @designator_depth
	.local	designator_depth
	.comm	designator_depth,4,4
	.type	designator_errorneous,@object # @designator_errorneous
	.local	designator_errorneous
	.comm	designator_errorneous,1,1
	.type	.L.str113,@object       # @.str113
.L.str113:
	.asciz	"extra brace group at end of initializer"
	.size	.L.str113, 40

	.type	.L.str114,@object       # @.str114
.L.str114:
	.asciz	"missing braces around initializer"
	.size	.L.str114, 34

	.type	.L.str115,@object       # @.str115
.L.str115:
	.asciz	"braces around scalar initializer"
	.size	.L.str115, 33

	.type	.L__FUNCTION__.pop_init_level,@object # @__FUNCTION__.pop_init_level
.L__FUNCTION__.pop_init_level:
	.asciz	"pop_init_level"
	.size	.L__FUNCTION__.pop_init_level, 15

	.type	.L.str116,@object       # @.str116
.L.str116:
	.asciz	"initialization of flexible array member in a nested context"
	.size	.L.str116, 60

	.type	.L.str117,@object       # @.str117
.L.str117:
	.asciz	"initialization of a flexible array member"
	.size	.L.str117, 42

	.type	.L.str118,@object       # @.str118
.L.str118:
	.asciz	"missing initializer"
	.size	.L.str118, 20

	.type	.L.str119,@object       # @.str119
.L.str119:
	.asciz	"empty scalar initializer"
	.size	.L.str119, 25

	.type	.L.str120,@object       # @.str120
.L.str120:
	.asciz	"extra elements in scalar initializer"
	.size	.L.str120, 37

	.type	.L.str121,@object       # @.str121
.L.str121:
	.asciz	"nonconstant array index in initializer"
	.size	.L.str121, 39

	.type	.L.str122,@object       # @.str122
.L.str122:
	.asciz	"array index in non-array initializer"
	.size	.L.str122, 37

	.type	.L.str123,@object       # @.str123
.L.str123:
	.asciz	"array index in initializer exceeds array bounds"
	.size	.L.str123, 48

	.type	.L.str124,@object       # @.str124
.L.str124:
	.asciz	"empty index range in initializer"
	.size	.L.str124, 33

	.type	.L.str125,@object       # @.str125
.L.str125:
	.asciz	"array index range in initializer exceeds array bounds"
	.size	.L.str125, 54

	.type	.L.str126,@object       # @.str126
.L.str126:
	.asciz	"field name not in record or union initializer"
	.size	.L.str126, 46

	.type	.L.str127,@object       # @.str127
.L.str127:
	.asciz	"unknown field `%s' specified in initializer"
	.size	.L.str127, 44

	.type	.L.str128,@object       # @.str128
.L.str128:
	.asciz	"excess elements in char array initializer"
	.size	.L.str128, 42

	.type	.L.str129,@object       # @.str129
.L.str129:
	.asciz	"excess elements in struct initializer"
	.size	.L.str129, 38

	.type	.L.str130,@object       # @.str130
.L.str130:
	.asciz	"non-static initialization of a flexible array member"
	.size	.L.str130, 53

	.type	.L.str131,@object       # @.str131
.L.str131:
	.asciz	"excess elements in union initializer"
	.size	.L.str131, 37

	.type	.L.str132,@object       # @.str132
.L.str132:
	.asciz	"traditional C rejects initialization of unions"
	.size	.L.str132, 47

	.type	.L.str133,@object       # @.str133
.L.str133:
	.asciz	"excess elements in array initializer"
	.size	.L.str133, 37

	.type	.L.str134,@object       # @.str134
.L.str134:
	.asciz	"excess elements in vector initializer"
	.size	.L.str134, 38

	.type	.L.str135,@object       # @.str135
.L.str135:
	.asciz	"excess elements in scalar initializer"
	.size	.L.str135, 38

	.type	.L__FUNCTION__.process_init_element,@object # @__FUNCTION__.process_init_element
.L__FUNCTION__.process_init_element:
	.asciz	"process_init_element"
	.size	.L__FUNCTION__.process_init_element, 21

	.type	.L.str136,@object       # @.str136
.L.str136:
	.asciz	"argument of `asm' is not a constant string"
	.size	.L.str136, 43

	.type	.L.str137,@object       # @.str137
.L.str137:
	.asciz	"asm template is not a string constant"
	.size	.L.str137, 38

	.type	.L.str138,@object       # @.str138
.L.str138:
	.asciz	"%s qualifier ignored on asm"
	.size	.L.str138, 28

	.type	.L.str139,@object       # @.str139
.L.str139:
	.asciz	"invalid lvalue in asm statement"
	.size	.L.str139, 32

	.type	.L.str140,@object       # @.str140
.L.str140:
	.asciz	"modification by `asm'"
	.size	.L.str140, 22

	.type	.L.str141,@object       # @.str141
.L.str141:
	.asciz	"function declared `noreturn' has a `return' statement"
	.size	.L.str141, 54

	.type	.L.str142,@object       # @.str142
.L.str142:
	.asciz	"`return' with no value, in function returning non-void"
	.size	.L.str142, 55

	.type	.L.str143,@object       # @.str143
.L.str143:
	.asciz	"`return' with a value, in function returning void"
	.size	.L.str143, 50

	.type	.L.str144,@object       # @.str144
.L.str144:
	.asciz	"return"
	.size	.L.str144, 7

	.type	.L.str145,@object       # @.str145
.L.str145:
	.asciz	"function returns address of local variable"
	.size	.L.str145, 43

	.type	.L.str146,@object       # @.str146
.L.str146:
	.asciz	"switch quantity not an integer"
	.size	.L.str146, 31

	.type	.L.str147,@object       # @.str147
.L.str147:
	.asciz	"`long' switch expression not converted to `int' in ISO C"
	.size	.L.str147, 57

	.type	switch_stack,@object    # @switch_stack
	.local	switch_stack
	.comm	switch_stack,8,8
	.type	.L.str148,@object       # @.str148
.L.str148:
	.asciz	"case label not within a switch statement"
	.size	.L.str148, 41

	.type	.L.str149,@object       # @.str149
.L.str149:
	.asciz	"`default' label not within a switch statement"
	.size	.L.str149, 46

	.type	.L.str150,@object       # @.str150
.L.str150:
	.asciz	"initializer element is not constant"
	.size	.L.str150, 36

	.type	.L.str151,@object       # @.str151
.L.str151:
	.asciz	"initializer element is not computable at load time"
	.size	.L.str151, 51

	.type	.L.str152,@object       # @.str152
.L.str152:
	.asciz	"initialized field with side-effects overwritten"
	.size	.L.str152, 48

	.type	.L__FUNCTION__.set_nonincremental_init_from_string,@object # @__FUNCTION__.set_nonincremental_init_from_string
.L__FUNCTION__.set_nonincremental_init_from_string:
	.asciz	"set_nonincremental_init_from_string"
	.size	.L__FUNCTION__.set_nonincremental_init_from_string, 36

	.type	.L__FUNCTION__.set_designator,@object # @__FUNCTION__.set_designator
.L__FUNCTION__.set_designator:
	.asciz	"set_designator"
	.size	.L__FUNCTION__.set_designator, 15

	.type	.L.str153,@object       # @.str153
.L.str153:
	.asciz	"initialization designators may not nest"
	.size	.L.str153, 40

	.type	.L.str154,@object       # @.str154
.L.str154:
	.asciz	"<anonymous>"
	.size	.L.str154, 12

	.type	.L.str155,@object       # @.str155
.L.str155:
	.asciz	"char-array initialized from wide string"
	.size	.L.str155, 40

	.type	.L.str156,@object       # @.str156
.L.str156:
	.asciz	"int-array initialized from non-wide string"
	.size	.L.str156, 43

	.type	.L.str157,@object       # @.str157
.L.str157:
	.asciz	"initializer-string for array of chars is too long"
	.size	.L.str157, 50

	.type	.L.str158,@object       # @.str158
.L.str158:
	.asciz	"array initialized from non-constant array expression"
	.size	.L.str158, 53

	.type	.L.str159,@object       # @.str159
.L.str159:
	.asciz	"initialization"
	.size	.L.str159, 15

	.type	.L.str160,@object       # @.str160
.L.str160:
	.asciz	"variable-sized object may not be initialized"
	.size	.L.str160, 45

	.type	.L.str161,@object       # @.str161
.L.str161:
	.asciz	"invalid initializer"
	.size	.L.str161, 20

	.type	.L.str162,@object       # @.str162
.L.str162:
	.asciz	"[%d]"
	.size	.L.str162, 5

	.type	.L.str163,@object       # @.str163
.L.str163:
	.asciz	"%s makes qualified function pointer from unqualified"
	.size	.L.str163, 53

	.type	.L.str164,@object       # @.str164
.L.str164:
	.asciz	"%s discards qualifiers from pointer target type"
	.size	.L.str164, 48

	.type	.L.str165,@object       # @.str165
.L.str165:
	.asciz	"ISO C prohibits argument conversion to union type"
	.size	.L.str165, 50

	.type	.L.str166,@object       # @.str166
.L.str166:
	.asciz	"ISO C forbids %s between function pointer and `void *'"
	.size	.L.str166, 55

	.type	.L.str167,@object       # @.str167
.L.str167:
	.asciz	"pointer targets in %s differ in signedness"
	.size	.L.str167, 43

	.type	.L.str168,@object       # @.str168
.L.str168:
	.asciz	"%s from incompatible pointer type"
	.size	.L.str168, 34

	.type	.L.str169,@object       # @.str169
.L.str169:
	.asciz	"%s makes pointer from integer without a cast"
	.size	.L.str169, 45

	.type	.L.str170,@object       # @.str170
.L.str170:
	.asciz	"%s makes integer from pointer without a cast"
	.size	.L.str170, 45

	.type	.L.str171,@object       # @.str171
.L.str171:
	.asciz	"incompatible type for argument %d of `%s'"
	.size	.L.str171, 42

	.type	.L.str172,@object       # @.str172
.L.str172:
	.asciz	"incompatible type for argument %d of indirect function call"
	.size	.L.str172, 60

	.type	.L.str173,@object       # @.str173
.L.str173:
	.asciz	"incompatible types in %s"
	.size	.L.str173, 25

	.type	.L.str174,@object       # @.str174
.L.str174:
	.asciz	"passing arg %d of `%s'"
	.size	.L.str174, 23

	.type	.L.str175,@object       # @.str175
.L.str175:
	.asciz	"passing arg %d of pointer to function"
	.size	.L.str175, 38

	.type	.L.str176,@object       # @.str176
.L.str176:
	.asciz	"left-hand operand of comma expression has no effect"
	.size	.L.str176, 52

	.type	.L.str177,@object       # @.str177
.L.str177:
	.asciz	"ISO C forbids use of conditional expressions as lvalues"
	.size	.L.str177, 56

	.type	.L.str178,@object       # @.str178
.L.str178:
	.asciz	"ISO C forbids use of compound expressions as lvalues"
	.size	.L.str178, 53

	.type	.L.str179,@object       # @.str179
.L.str179:
	.asciz	"ISO C forbids use of cast expressions as lvalues"
	.size	.L.str179, 49

	.type	.L.str180,@object       # @.str180
.L.str180:
	.asciz	"pointer of type `void *' used in subtraction"
	.size	.L.str180, 45

	.type	.L.str181,@object       # @.str181
.L.str181:
	.asciz	"pointer to a function used in subtraction"
	.size	.L.str181, 42

	.type	.L.str182,@object       # @.str182
.L.str182:
	.asciz	"types are not quite compatible"
	.size	.L.str182, 31

	.type	.L.str183,@object       # @.str183
.L.str183:
	.asciz	"too many arguments to function `%s'"
	.size	.L.str183, 36

	.type	.L.str184,@object       # @.str184
.L.str184:
	.asciz	"too many arguments to function"
	.size	.L.str184, 31

	.type	.L.str185,@object       # @.str185
.L.str185:
	.asciz	"type of formal parameter %d is incomplete"
	.size	.L.str185, 42

	.type	.L.str186,@object       # @.str186
.L.str186:
	.asciz	"%s as integer rather than floating due to prototype"
	.size	.L.str186, 52

	.type	.L.str187,@object       # @.str187
.L.str187:
	.asciz	"%s as integer rather than complex due to prototype"
	.size	.L.str187, 51

	.type	.L.str188,@object       # @.str188
.L.str188:
	.asciz	"%s as complex rather than floating due to prototype"
	.size	.L.str188, 52

	.type	.L.str189,@object       # @.str189
.L.str189:
	.asciz	"%s as floating rather than integer due to prototype"
	.size	.L.str189, 52

	.type	.L.str190,@object       # @.str190
.L.str190:
	.asciz	"%s as complex rather than integer due to prototype"
	.size	.L.str190, 51

	.type	.L.str191,@object       # @.str191
.L.str191:
	.asciz	"%s as floating rather than complex due to prototype"
	.size	.L.str191, 52

	.type	.L.str192,@object       # @.str192
.L.str192:
	.asciz	"%s as `float' rather than `double' due to prototype"
	.size	.L.str192, 52

	.type	.L.str193,@object       # @.str193
.L.str193:
	.asciz	"%s with different width due to prototype"
	.size	.L.str193, 41

	.type	.L.str194,@object       # @.str194
.L.str194:
	.asciz	"%s as unsigned due to prototype"
	.size	.L.str194, 32

	.type	.L.str195,@object       # @.str195
.L.str195:
	.asciz	"%s as signed due to prototype"
	.size	.L.str195, 30

	.type	.L.str196,@object       # @.str196
.L.str196:
	.asciz	"too few arguments to function `%s'"
	.size	.L.str196, 35

	.type	.L.str197,@object       # @.str197
.L.str197:
	.asciz	"too few arguments to function"
	.size	.L.str197, 30


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
