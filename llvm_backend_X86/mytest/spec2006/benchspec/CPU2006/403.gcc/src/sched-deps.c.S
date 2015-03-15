	.file	"sched-deps.c.bc"
	.text
	.globl	find_insn_list
	.align	16, 0x90
	.type	find_insn_list,@function
find_insn_list:                         # @find_insn_list
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	jmp	.LBB0_1
	.align	16, 0x90
.LBB0_5:                                # %if.end
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	16(%rsi), %rsi
.LBB0_1:                                # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rsi, %rsi
	je	.LBB0_4
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	8(%rsi), %rcx
	cmpq	%rdi, %rcx
	jne	.LBB0_5
# BB#3:
	movq	%rsi, %rax
.LBB0_4:                                # %return
	retq
.Ltmp0:
	.size	find_insn_list, .Ltmp0-find_insn_list
	.cfi_endproc

	.globl	add_dependence
	.align	16, 0x90
	.type	add_dependence,@function
add_dependence:                         # @add_dependence
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp8:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp9:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp10:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp11:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp12:
	.cfi_def_cfa_offset 64
.Ltmp13:
	.cfi_offset %rbx, -48
.Ltmp14:
	.cfi_offset %r12, -40
.Ltmp15:
	.cfi_offset %r13, -32
.Ltmp16:
	.cfi_offset %r14, -24
.Ltmp17:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	cmpq	%rbx, %r15
	je	.LBB1_64
# BB#1:                                 # %if.end
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	je	.LBB1_64
# BB#2:                                 # %if.end3
	movzbl	(%r15), %ecx
	cmpq	$34, %rcx
	je	.LBB1_13
# BB#3:                                 # %if.end3
	cmpq	$34, %rax
	je	.LBB1_13
# BB#4:                                 # %if.then10
	movq	%r15, %rdi
	callq	get_condition
	movq	%rax, %r13
	movq	%rbx, %rdi
	callq	get_condition
	movq	%rax, %r12
	testq	%r13, %r13
	je	.LBB1_13
# BB#5:                                 # %if.then10
	testq	%r12, %r12
	je	.LBB1_13
# BB#6:                                 # %land.lhs.true14
	movzbl	(%r13), %ecx
	movzbl	rtx_class(%rcx), %eax
	cmpq	$60, %rax
	jne	.LBB1_13
# BB#7:                                 # %land.lhs.true.i
	movzbl	(%r12), %edi
	movzbl	rtx_class(%rdi), %eax
	cmpq	$60, %rax
	jne	.LBB1_13
# BB#8:                                 # %land.lhs.true9.i
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	callq	reverse_condition
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	%rax, 8(%rsp)           # 8-byte Folded Reload
	jne	.LBB1_13
# BB#9:                                 # %land.lhs.true16.i
	movq	8(%r13), %rax
	movq	8(%r12), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_13
# BB#10:                                # %land.lhs.true23.i
	movq	16(%r13), %rax
	movq	16(%r12), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_13
# BB#11:                                # %land.lhs.true17
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	%rdx, %r13
	callq	modified_in_p
	movq	%r13, %rcx
	testq	%rcx, %rax
	jne	.LBB1_13
# BB#12:                                # %land.lhs.true20
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rcx, %r12
	callq	modified_in_p
	testq	%r12, %rax
	je	.LBB1_64
.LBB1_13:                               # %if.end25
	movq	%rbx, %rdi
	callq	next_nonnote_insn
	testq	%rax, %rax
	je	.LBB1_20
# BB#14:                                # %land.lhs.true28
	movl	(%rax), %ecx
	testq	$268435456, %rcx        # imm = 0x10000000
	je	.LBB1_20
# BB#15:                                # %land.lhs.true28
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$36, %rcx
	je	.LBB1_20
	.align	16, 0x90
.LBB1_16:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	next_nonnote_insn
	testq	%rax, %rax
	je	.LBB1_19
# BB#17:                                # %land.lhs.true39
                                        #   in Loop: Header=BB1_16 Depth=1
	movl	(%rax), %ecx
	testq	$268435456, %rcx        # imm = 0x10000000
	je	.LBB1_19
# BB#18:                                # %land.lhs.true39
                                        #   in Loop: Header=BB1_16 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$36, %rcx
	jne	.LBB1_16
.LBB1_19:                               # %while.end
	cmpq	%r15, %rbx
	je	.LBB1_64
.LBB1_20:                               # %if.end50
	movq	true_dependency_cache(%rip), %r8
	testq	%r8, %r8
	je	.LBB1_42
# BB#21:                                # %if.then52
	movq	anti_dependency_cache(%rip), %r10
	testq	%r10, %r10
	je	.LBB1_23
# BB#22:                                # %if.then52
	movq	output_dependency_cache(%rip), %r9
	testq	%r9, %r9
	je	.LBB1_23
# BB#25:                                # %if.end56
	movslq	8(%rbx), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	16(%rdx,%rax), %r11d
	movslq	8(%r15), %rdi
	movq	$6, %rcx
	movq	%r11, %rax
	shrq	%cl, %rax
	leaq	(%rdi,%rdi,2), %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	movslq	16(%rdx,%rdi), %rdi
	movq	(%r8,%rdi,8), %rcx
	movq	16(%rcx,%rax,8), %rsi
	movq	$1, %rdx
	movq	%r11, %rcx
	shlq	%cl, %rdx
	xorq	%r11, %r11
	testq	%rdx, %rsi
	movabsq	$0, %rsi
	jne	.LBB1_33
# BB#26:                                # %if.else
	movq	(%r10,%rdi,8), %rcx
	movq	16(%rcx,%rax,8), %rcx
	testq	%rdx, %rcx
	je	.LBB1_28
# BB#27:
	movq	$14, %rsi
	xorq	%r11, %r11
	jmp	.LBB1_33
.LBB1_28:                               # %if.else106
	movq	(%r9,%rdi,8), %rcx
	movq	16(%rcx,%rax,8), %rsi
	andq	%rdx, %rsi
	jne	.LBB1_29
# BB#30:                                # %if.else106
	movabsq	$1, %r11
	jmp	.LBB1_31
.LBB1_29:
	movabsq	$0, %r11
.LBB1_31:                               # %if.else106
	testq	%rsi, %rsi
	je	.LBB1_33
# BB#32:                                # %if.else106
	movq	$15, %rsi
.LBB1_33:                               # %if.end140
	testq	%r11, %r11
	jne	.LBB1_35
# BB#34:                                # %if.end140
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rax, %rsi
	jle	.LBB1_64
.LBB1_35:                               # %if.end146
	testq	%r11, %r11
	je	.LBB1_42
# BB#36:                                # %if.end146.if.end350_crit_edge
	movq	48(%r15), %rsi
	leaq	8(%r15), %r12
	jmp	.LBB1_37
.LBB1_42:                               # %if.then148
	movq	48(%r15), %rax
	leaq	8(%r15), %r12
	xorq	%rsi, %rsi
	testq	%rax, %rax
	je	.LBB1_37
# BB#43:
	movq	%rax, %rsi
	.align	16, 0x90
.LBB1_44:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rsi), %rcx
	cmpq	%rbx, %rcx
	je	.LBB1_47
# BB#45:                                # %for.cond
                                        #   in Loop: Header=BB1_44 Depth=1
	movq	16(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB1_44
# BB#46:
	movq	%rax, %rsi
.LBB1_37:                               # %if.end350
	movq	%rbx, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, 48(%r15)
	movb	%r14b, 2(%rax)
	movq	true_dependency_cache(%rip), %rdx
	testq	%rdx, %rdx
	je	.LBB1_64
# BB#38:                                # %if.then364
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r14, %rcx
	andq	%rax, %rcx
	cmpq	$15, %rcx
	je	.LBB1_62
# BB#39:                                # %if.then364
	cmpq	$14, %rcx
	je	.LBB1_61
# BB#40:                                # %if.then364
	testq	%rax, %r14
	jne	.LBB1_64
# BB#41:                                # %if.then366
	movslq	8(%rbx), %rax
	movq	h_i_d(%rip), %rsi
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	16(%rsi,%rax), %eax
	movslq	(%r12), %rdi
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	leaq	(%rdi,%rdi,2), %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	movslq	16(%rsi,%rdi), %rcx
	movq	(%rdx,%rcx,8), %rcx
	shrq	$6, %rax
	orq	%rbx, 16(%rcx,%rax,8)
	jmp	.LBB1_64
.LBB1_47:                               # %if.then156
	testq	%r8, %r8
	je	.LBB1_54
# BB#48:                                # %if.then158
	movzbl	2(%rsi), %eax
	cmpq	$14, %rax
	jne	.LBB1_50
# BB#49:                                # %if.then163
	movslq	8(%rbx), %rax
	movq	h_i_d(%rip), %r9
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	16(%r9,%rax), %eax
	movq	$1, %rdi
	movslq	(%r12), %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdi
	leaq	(%rdx,%rdx,2), %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movslq	16(%r9,%rdx), %rcx
	movq	anti_dependency_cache(%rip), %rdx
	movq	(%rdx,%rcx,8), %rcx
	jmp	.LBB1_53
.LBB1_50:                               # %if.else191
	cmpq	$15, %rax
	jne	.LBB1_65
# BB#51:                                # %if.else191
	movq	output_dependency_cache(%rip), %r9
	testq	%r9, %r9
	je	.LBB1_65
# BB#52:                                # %if.then198
	movslq	8(%rbx), %rax
	movq	h_i_d(%rip), %r10
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	16(%r10,%rax), %eax
	movslq	(%r12), %rdx
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	leaq	(%rdx,%rdx,2), %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movslq	16(%r10,%rdx), %rcx
	movq	(%r9,%rcx,8), %rcx
.LBB1_53:                               # %if.end231
	notq	%rdi
	shrq	$6, %rax
	andq	%rdi, 16(%rcx,%rax,8)
.LBB1_54:                               # %if.end231
	movl	(%rsi), %eax
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movq	$32, %rcx
	movq	%r14, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rdx
	jle	.LBB1_56
# BB#55:                                # %if.then236
	movq	$16, %rcx
	shlq	%cl, %r14
	andq	$16711680, %r14         # imm = 0xFF0000
	andq	$-16711681, %rax        # imm = 0xFFFFFFFFFF00FFFF
	orq	%r14, %rax
	movl	%eax, (%rsi)
	movq	true_dependency_cache(%rip), %r8
.LBB1_56:                               # %if.end239
	testq	%r8, %r8
	je	.LBB1_64
# BB#57:                                # %if.then241
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$15, %rax
	je	.LBB1_62
# BB#58:                                # %if.then241
	cmpq	$14, %rax
	jne	.LBB1_59
.LBB1_61:                               # %if.then279
	movslq	8(%rbx), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	16(%rdx,%rax), %eax
	movq	$1, %rsi
	movslq	(%r12), %rdi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	leaq	(%rdi,%rdi,2), %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	movslq	16(%rdx,%rdi), %rcx
	movq	anti_dependency_cache(%rip), %rdx
	jmp	.LBB1_63
.LBB1_62:                               # %if.then428
	movslq	8(%rbx), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	16(%rdx,%rax), %eax
	movq	$1, %rsi
	movslq	(%r12), %rdi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	leaq	(%rdi,%rdi,2), %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	movslq	16(%rdx,%rdi), %rcx
	movq	output_dependency_cache(%rip), %rdx
.LBB1_63:                               # %if.end460
	movq	(%rdx,%rcx,8), %rcx
	shrq	$6, %rax
	orq	%rsi, 16(%rcx,%rax,8)
.LBB1_64:                               # %if.end460
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_59:                               # %if.then241
	testq	%rax, %rax
	jne	.LBB1_64
# BB#60:                                # %if.then246
	movslq	8(%rbx), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	16(%rdx,%rax), %eax
	movslq	(%r12), %rsi
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	leaq	(%rsi,%rsi,2), %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movslq	16(%rdx,%rsi), %rcx
	movq	(%r8,%rcx,8), %rcx
	shrq	$6, %rax
	orq	%rdi, 16(%rcx,%rax,8)
	jmp	.LBB1_64
.LBB1_23:                               # %if.then55
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.add_dependence, %rcx
	movq	$273, %rsi              # imm = 0x111
	jmp	.LBB1_24
.LBB1_65:                               # %if.else228
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.add_dependence, %rcx
	movq	$308, %rsi              # imm = 0x134
.LBB1_24:                               # %if.then55
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp18:
	.size	add_dependence, .Ltmp18-add_dependence
	.cfi_endproc

	.align	16, 0x90
	.type	get_condition,@function
get_condition:                          # @get_condition
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp21:
	.cfi_def_cfa_offset 16
.Ltmp22:
	.cfi_offset %rbx, -16
	movq	32(%rdi), %rdx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	je	.LBB2_12
# BB#1:                                 # %if.end
	movzbl	(%rdx), %ecx
	cmpq	$38, %rcx
	jne	.LBB2_3
# BB#2:                                 # %if.then2
	movq	8(%rdx), %rax
	popq	%rbx
	retq
.LBB2_3:                                # %if.end6
	movzbl	(%rdi), %esi
	cmpq	$33, %rsi
	jne	.LBB2_12
# BB#4:                                 # %if.end6
	cmpq	$47, %rcx
	jne	.LBB2_12
# BB#5:                                 # %lor.lhs.false
	movq	16(%rdx), %rcx
	movq	global_rtl(%rip), %rsi
	cmpq	%rsi, %rcx
	jne	.LBB2_12
# BB#6:                                 # %if.end20
	movq	8(%rdx), %rdx
	movzbl	(%rdx), %esi
	cmpq	$72, %rsi
	jne	.LBB2_12
# BB#7:                                 # %if.end28
	movq	8(%rdx), %rbx
	movq	16(%rbx), %rdx
	movzbl	(%rdx), %edi
	movq	24(%rbx), %rsi
	cmpq	$67, %rdi
	jne	.LBB2_10
# BB#8:                                 # %if.end28
	cmpq	%rcx, %rsi
	jne	.LBB2_10
# BB#9:
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB2_10:                               # %if.else
	movzbl	(%rsi), %esi
	cmpq	$67, %rsi
	jne	.LBB2_12
# BB#11:                                # %if.else
	cmpq	%rcx, %rdx
	jne	.LBB2_12
# BB#13:                                # %if.then57
	movzbl	(%rbx), %edi
	callq	reverse_condition
	movq	8(%rbx), %rdx
	movq	16(%rbx), %rcx
	movzbl	2(%rbx), %esi
	movq	%rax, %rdi
	popq	%rbx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.LBB2_12:                               # %return
	popq	%rbx
	retq
.Ltmp23:
	.size	get_condition, .Ltmp23-get_condition
	.cfi_endproc

	.globl	add_insn_mem_dependence
	.align	16, 0x90
	.type	add_insn_mem_dependence,@function
add_insn_mem_dependence:                # @add_insn_mem_dependence
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp29:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp30:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp31:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp32:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp33:
	.cfi_def_cfa_offset 48
.Ltmp34:
	.cfi_offset %rbx, -40
.Ltmp35:
	.cfi_offset %r12, -32
.Ltmp36:
	.cfi_offset %r14, -24
.Ltmp37:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	(%rbx), %rsi
	movq	%r8, %r12
	movq	%rdx, %r15
	movq	%rdi, %r14
	movq	%rcx, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, (%rbx)
	movq	current_sched_info(%rip), %rax
	movzbl	96(%rax), %eax
	testq	$2, %rax
	je	.LBB3_2
# BB#1:                                 # %if.then
	movq	%r12, %rdi
	callq	shallow_copy_rtx
	movq	%rax, %r12
	movq	8(%r12), %rdi
	callq	cselib_subst_to_values
	movq	%rax, 8(%r12)
.LBB3_2:                                # %if.end
	movq	(%r15), %rdx
	xorq	%rdi, %rdi
	movq	%r12, %rsi
	callq	alloc_EXPR_LIST
	movq	%rax, (%r15)
	movl	32(%r14), %eax
	incq	%rax
	movl	%eax, 32(%r14)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp38:
	.size	add_insn_mem_dependence, .Ltmp38-add_insn_mem_dependence
	.cfi_endproc

	.globl	sched_analyze
	.align	16, 0x90
	.type	sched_analyze,@function
sched_analyze:                          # @sched_analyze
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
	subq	$48, %rsp
.Ltmp50:
	.cfi_def_cfa_offset 96
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
	movq	current_sched_info(%rip), %rax
	movzbl	96(%rax), %eax
	movq	%rdx, %r13
	movq	%r13, 40(%rsp)          # 8-byte Spill
	movq	%rsi, %r14
	movq	%rdi, %r15
	testq	$2, %rax
	je	.LBB4_2
# BB#1:                                 # %if.then
	callq	cselib_init
.LBB4_2:                                # %for.cond.preheader
	leaq	8(%r15), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	leaq	24(%r15), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	leaq	48(%r15), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	xorq	%r12, %r12
	jmp	.LBB4_3
	.align	16, 0x90
.LBB4_57:                               # %for.inc192
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	24(%r14), %r14
.LBB4_3:                                # %for.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_9 Depth 2
                                        #     Child Loop BB4_11 Depth 2
                                        #     Child Loop BB4_13 Depth 2
                                        #     Child Loop BB4_19 Depth 2
                                        #     Child Loop BB4_32 Depth 2
                                        #     Child Loop BB4_35 Depth 2
                                        #     Child Loop BB4_38 Depth 2
                                        #     Child Loop BB4_40 Depth 2
	movl	(%r14), %ecx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$37, %rax
	je	.LBB4_43
# BB#4:                                 # %for.cond
                                        #   in Loop: Header=BB4_3 Depth=1
	cmpq	$34, %rax
	je	.LBB4_17
# BB#5:                                 # %for.cond
                                        #   in Loop: Header=BB4_3 Depth=1
	addq	$-32, %rax
	andq	%rbx, %rax
	cmpq	$1, %rax
	ja	.LBB4_52
# BB#6:                                 # %if.then6
                                        #   in Loop: Header=BB4_3 Depth=1
	leaq	48(%r14), %rdi
	callq	free_INSN_LIST_list
	movl	(%r14), %eax
	movq	%rax, %rcx
	movabsq	$4026531839, %rdx       # imm = 0xEFFFFFFF
	andq	%rdx, %rcx
	movl	%ecx, (%r14)
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$33, %rax
	jne	.LBB4_16
# BB#7:                                 # %if.then12
                                        #   in Loop: Header=BB4_3 Depth=1
	movslq	36(%r15), %rax
	leaq	1(%rax), %rcx
	movl	%ecx, 36(%r15)
	movq	compiler_params(%rip), %rcx
	movslq	80(%rcx), %rcx
	cmpq	%rcx, %rax
	jle	.LBB4_15
# BB#8:                                 # %if.then15
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	(%r15), %rbx
	testq	%rbx, %rbx
	movq	$0, (%r15)
	je	.LBB4_10
	.align	16, 0x90
.LBB4_9:                                # %for.body.i277
                                        #   Parent Loop BB4_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r13
	movq	$14, %rdx
	movq	%r14, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r13, %r13
	movq	%r13, %rbx
	jne	.LBB4_9
.LBB4_10:                               # %add_dependence_list_and_free.exit278
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	free_EXPR_LIST_list
	movq	16(%r15), %rbx
	testq	%rbx, %rbx
	movq	$0, 16(%r15)
	je	.LBB4_12
	.align	16, 0x90
.LBB4_11:                               # %for.body.i286
                                        #   Parent Loop BB4_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r13
	movq	$14, %rdx
	movq	%r14, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r13, %r13
	movq	%r13, %rbx
	jne	.LBB4_11
.LBB4_12:                               # %add_dependence_list_and_free.exit287
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	free_EXPR_LIST_list
	xorq	%rax, %rax
	movl	%eax, 32(%r15)
	movq	40(%r15), %rbx
	testq	%rbx, %rbx
	movq	$0, 40(%r15)
	je	.LBB4_14
	.align	16, 0x90
.LBB4_13:                               # %for.body.i295
                                        #   Parent Loop BB4_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r13
	movq	$14, %rdx
	movq	%r14, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r13, %r13
	movq	%r13, %rbx
	jne	.LBB4_13
.LBB4_14:                               # %add_dependence_list_and_free.exit296
                                        #   in Loop: Header=BB4_3 Depth=1
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, 40(%r15)
	movq	$1, %rax
	movl	%eax, 36(%r15)
	movq	40(%rsp), %r13          # 8-byte Reload
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB4_16
	.align	16, 0x90
.LBB4_43:                               # %land.lhs.true85
                                        #   in Loop: Header=BB4_3 Depth=1
	movl	40(%r14), %eax
	addq	$83, %rax
	andq	%rbx, %rax
	cmpq	$1, %rax
	ja	.LBB4_45
# BB#44:                                # %if.then95
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	32(%r14), %rsi
	movq	$24, %rdi
	movq	%r12, %rdx
	callq	alloc_EXPR_LIST
	movslq	40(%r14), %rsi
	movq	%rbx, %r12
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$24, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	movq	%r12, %rbx
	callq	alloc_EXPR_LIST
	movq	%rax, %r12
	jmp	.LBB4_52
	.align	16, 0x90
.LBB4_17:                               # %if.then26
                                        #   in Loop: Header=BB4_3 Depth=1
	movabsq	$4026531839, %rax       # imm = 0xEFFFFFFF
	andq	%rax, %rcx
	movl	%ecx, (%r14)
	movslq	8(%r14), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movzbl	46(%rdx,%rax), %ecx
	orq	$1, %rcx
	movb	%cl, 46(%rdx,%rax)
	leaq	48(%r14), %rdi
	callq	free_INSN_LIST_list
	movq	$28, %rsi
	xorq	%rdx, %rdx
	xorq	%r13, %r13
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB4_18
	.align	16, 0x90
.LBB4_19:                               # %for.body
                                        #   Parent Loop BB4_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	global_regs(%r13), %eax
	testq	%rax, %rax
	jne	.LBB4_20
# BB#21:                                # %if.else47
                                        #   in Loop: Header=BB4_19 Depth=2
	movq	regs_invalidated_by_call(%rip), %rax
	movq	$1, %rdx
	movq	%r13, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	jne	.LBB4_22
# BB#23:                                # %if.else50
                                        #   in Loop: Header=BB4_19 Depth=2
	movzbl	fixed_regs(%r13), %eax
	testq	%rax, %rax
	jne	.LBB4_28
# BB#24:                                # %if.else55
                                        #   in Loop: Header=BB4_19 Depth=2
	movq	%r13, %rax
	andq	%rbx, %rax
	cmpq	$20, %rax
	je	.LBB4_28
# BB#25:                                # %if.else55
                                        #   in Loop: Header=BB4_19 Depth=2
	cmpq	$6, %rax
	jne	.LBB4_30
# BB#26:                                # %land.lhs.true
                                        #   in Loop: Header=BB4_19 Depth=2
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB4_28
# BB#27:                                # %land.lhs.true
                                        #   in Loop: Header=BB4_19 Depth=2
	movl	frame_pointer_needed(%rip), %eax
	testq	%rbx, %rax
	je	.LBB4_30
	jmp	.LBB4_28
	.align	16, 0x90
.LBB4_20:                               # %if.then46
                                        #   in Loop: Header=BB4_19 Depth=2
	movq	reg_pending_sets(%rip), %rdi
	movq	%r13, %rsi
	callq	bitmap_set_bit
.LBB4_28:                               # %if.then62
                                        #   in Loop: Header=BB4_19 Depth=2
	movq	reg_pending_uses(%rip), %rdi
	jmp	.LBB4_29
	.align	16, 0x90
.LBB4_22:                               # %if.then49
                                        #   in Loop: Header=BB4_19 Depth=2
	movq	reg_pending_clobbers(%rip), %rdi
.LBB4_29:                               # %for.inc
                                        #   in Loop: Header=BB4_19 Depth=2
	movq	%r13, %rsi
	callq	bitmap_set_bit
.LBB4_30:                               # %for.inc
                                        #   in Loop: Header=BB4_19 Depth=2
	incq	%r13
	cmpq	$53, %r13
	jne	.LBB4_19
	jmp	.LBB4_31
.LBB4_45:                               # %land.lhs.true110
                                        #   in Loop: Header=BB4_3 Depth=1
	movl	40(%r14), %eax
	leaq	96(%rax), %rcx
	andq	%rbx, %rcx
	cmpq	$11, %rcx
	ja	.LBB4_52
# BB#46:                                # %land.lhs.true110
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$3075, %rdx             # imm = 0xC03
	je	.LBB4_52
# BB#47:                                # %if.then134
                                        #   in Loop: Header=BB4_3 Depth=1
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rcx, %rax
	movabsq	$4294967210, %rcx       # imm = 0xFFFFFFAA
	cmpq	%rcx, %rax
	jne	.LBB4_49
# BB#48:                                # %if.then146
                                        #   in Loop: Header=BB4_3 Depth=1
	movslq	32(%r14), %rsi
	xorq	%rdi, %rdi
	jmp	.LBB4_50
.LBB4_18:                               # %if.then39
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	$1, %rax
	movb	%al, reg_pending_barrier(%rip)
.LBB4_31:                               # %if.end68
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	56(%r15), %rbx
	testq	%rbx, %rbx
	movq	$0, 56(%r15)
	je	.LBB4_33
	.align	16, 0x90
.LBB4_32:                               # %for.body.i
                                        #   Parent Loop BB4_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r13
	movq	$14, %rdx
	movq	%r14, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r13, %r13
	movq	%r13, %rbx
	jne	.LBB4_32
.LBB4_33:                               # %add_dependence_list_and_free.exit
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	32(%r14), %rsi
	movq	%r15, %rdi
	movq	%r14, %rdx
	movq	%r12, %rcx
	callq	sched_analyze_insn
	movzbl	3(%r14), %eax
	testq	$4, %rax
	jne	.LBB4_37
# BB#34:                                # %if.then.i
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	(%r15), %rbx
	testq	%rbx, %rbx
	movq	$0, (%r15)
	je	.LBB4_36
	.align	16, 0x90
.LBB4_35:                               # %for.body.i304
                                        #   Parent Loop BB4_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r12
	movq	$14, %rdx
	movq	%r14, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r12, %r12
	movq	%r12, %rbx
	jne	.LBB4_35
.LBB4_36:                               # %add_dependence_list_and_free.exit305
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	free_EXPR_LIST_list
.LBB4_37:                               # %flush_pending_lists.exit
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	16(%r15), %rbx
	testq	%rbx, %rbx
	movq	$0, 16(%r15)
	movq	40(%rsp), %r13          # 8-byte Reload
	je	.LBB4_39
	.align	16, 0x90
.LBB4_38:                               # %for.body.i313
                                        #   Parent Loop BB4_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r12
	movq	$14, %rdx
	movq	%r14, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r12, %r12
	movq	%r12, %rbx
	jne	.LBB4_38
.LBB4_39:                               # %add_dependence_list_and_free.exit314
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	free_EXPR_LIST_list
	xorq	%rax, %rax
	movl	%eax, 32(%r15)
	movq	40(%r15), %rbx
	testq	%rbx, %rbx
	movq	$0, 40(%r15)
	je	.LBB4_41
	.align	16, 0x90
.LBB4_40:                               # %for.body.i322
                                        #   Parent Loop BB4_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r12
	movq	$14, %rdx
	movq	%r14, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r12, %r12
	movq	%r12, %rbx
	jne	.LBB4_40
.LBB4_41:                               # %add_dependence_list_and_free.exit323
                                        #   in Loop: Header=BB4_3 Depth=1
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	%r14, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, 40(%r15)
	movq	$1, %rbx
	movl	%ebx, 36(%r15)
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	free_INSN_LIST_list
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, 48(%r15)
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	jne	.LBB4_51
# BB#42:                                # %if.then79
                                        #   in Loop: Header=BB4_3 Depth=1
	movb	%bl, 64(%r15)
	jmp	.LBB4_51
.LBB4_15:                               # %if.else
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	40(%r15), %rsi
	movq	%r14, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, 40(%r15)
.LBB4_16:                               # %if.end18
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	32(%r14), %rsi
	movq	%r15, %rdi
	movq	%r14, %rdx
	movq	%r12, %rcx
	callq	sched_analyze_insn
	xorq	%r12, %r12
	jmp	.LBB4_52
.LBB4_49:                               # %if.else152
                                        #   in Loop: Header=BB4_3 Depth=1
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
.LBB4_50:                               # %if.end154
                                        #   in Loop: Header=BB4_3 Depth=1
	callq	gen_rtx_CONST_INT
	movq	$24, %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	callq	alloc_EXPR_LIST
	movslq	40(%r14), %rsi
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$24, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	alloc_EXPR_LIST
	movq	%rax, %r12
	movl	(%r14), %eax
	movl	(%r12), %ecx
	andq	$67108864, %rax         # imm = 0x4000000
	movabsq	$4227858431, %rdx       # imm = 0xFBFFFFFF
	andq	%rdx, %rcx
	orq	%rax, %rcx
	movl	%ecx, (%r12)
	.align	16, 0x90
.LBB4_51:                               # %if.end171
                                        #   in Loop: Header=BB4_3 Depth=1
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB4_52:                               # %if.end171
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	current_sched_info(%rip), %rax
	movzbl	96(%rax), %eax
	testq	$2, %rax
	je	.LBB4_54
# BB#53:                                # %if.then178
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	%r14, %rdi
	callq	cselib_process_insn
.LBB4_54:                               # %if.end179
                                        #   in Loop: Header=BB4_3 Depth=1
	cmpq	%r13, %r14
	jne	.LBB4_57
# BB#55:                                # %if.then182
	movq	current_sched_info(%rip), %rax
	movzbl	96(%rax), %eax
	addq	$48, %rsp
	testq	$2, %rax
	je	.LBB4_56
# BB#58:                                # %if.then189
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	cselib_finish  # TAILCALL
.LBB4_56:                               # %if.end190
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp56:
	.size	sched_analyze, .Ltmp56-sched_analyze
	.cfi_endproc

	.align	16, 0x90
	.type	sched_analyze_insn,@function
sched_analyze_insn:                     # @sched_analyze_insn
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
	subq	$112, %rsp
.Ltmp68:
	.cfi_def_cfa_offset 160
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
	movq	%rsi, %r13
	movzbl	(%r13), %eax
	movq	%rcx, %rbx
	cmpq	$38, %rax
	jne	.LBB5_1
# BB#2:                                 # %if.then
	movq	8(%r13), %rsi
	movq	%rdi, %r15
	movq	%rdx, %r12
	callq	sched_analyze_2
	movq	16(%r13), %r13
	movzbl	(%r13), %eax
	jmp	.LBB5_3
.LBB5_1:
	movq	%rdi, %r15
	movq	%rdx, %r12
.LBB5_3:                                # %if.end
	movq	%rbx, 80(%rsp)          # 8-byte Spill
	cmpq	$39, %rax
	je	.LBB5_7
# BB#4:                                 # %if.end
	cmpq	$47, %rax
	je	.LBB5_6
# BB#5:                                 # %if.end
	cmpq	$49, %rax
	jne	.LBB5_16
.LBB5_6:                                # %if.then8
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	callq	sched_analyze_1
	jmp	.LBB5_17
.LBB5_7:                                # %if.then10
	movq	8(%r13), %rax
	movslq	(%rax), %r14
	testq	%r14, %r14
	jg	.LBB5_8
	jmp	.LBB5_17
	.align	16, 0x90
.LBB5_14:                               # %for.cond.backedge.for.body_crit_edge
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	8(%r13), %rax
.LBB5_8:                                # %for.body.lr.ph
                                        # =>This Inner Loop Header: Depth=1
	decq	%r14
	movq	8(%rax,%r14,8), %rbx
	movzbl	(%rbx), %eax
	cmpq	$38, %rax
	jne	.LBB5_10
# BB#9:                                 # %if.then23
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	8(%rbx), %rsi
	movq	%r15, %rdi
	movq	%r12, %rdx
	callq	sched_analyze_2
	movq	16(%rbx), %rbx
	movzbl	(%rbx), %eax
.LBB5_10:                               # %if.end32
                                        #   in Loop: Header=BB5_8 Depth=1
	cmpq	$47, %rax
	je	.LBB5_12
# BB#11:                                # %if.end32
                                        #   in Loop: Header=BB5_8 Depth=1
	cmpq	$49, %rax
	jne	.LBB5_15
.LBB5_12:                               # %if.then36
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	callq	sched_analyze_1
	jmp	.LBB5_13
	.align	16, 0x90
.LBB5_15:                               # %if.else37
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	callq	sched_analyze_2
.LBB5_13:                               # %for.cond.backedge
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB5_14
	jmp	.LBB5_17
.LBB5_16:                               # %if.else39
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	callq	sched_analyze_2
.LBB5_17:                               # %if.end41
	movq	%r12, %r13
	movzbl	(%r13), %eax
	cmpq	$34, %rax
	jne	.LBB5_26
# BB#18:                                # %if.then45
	movq	64(%r13), %rbx
	jmp	.LBB5_19
	.align	16, 0x90
.LBB5_21:                               # %if.then57
                                        #   in Loop: Header=BB5_19 Depth=1
	callq	sched_analyze_1
	movq	16(%rbx), %rbx
.LBB5_19:                               # %if.then45
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB5_24
# BB#20:                                # %for.body50
                                        #   in Loop: Header=BB5_19 Depth=1
	movq	8(%rbx), %rsi
	movzbl	(%rsi), %eax
	movq	%r15, %rdi
	movq	%r13, %rdx
	cmpq	$49, %rax
	je	.LBB5_21
# BB#22:                                # %if.else61
                                        #   in Loop: Header=BB5_19 Depth=1
	callq	sched_analyze_2
	movq	16(%rbx), %rbx
	jmp	.LBB5_19
.LBB5_24:                               # %for.end70
	movq	$28, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB5_26
# BB#25:                                # %if.then72
	movq	$1, %rax
	movb	%al, reg_pending_barrier(%rip)
.LBB5_26:                               # %if.end74
	movzbl	(%r13), %eax
	cmpq	$33, %rax
	jne	.LBB5_41
# BB#27:                                # %if.then78
	movq	%r13, %rdi
	callq	next_nonnote_insn
	testq	%rax, %rax
	je	.LBB5_30
# BB#28:                                # %land.lhs.true
	movzbl	(%rax), %eax
	cmpq	$35, %rax
	jne	.LBB5_30
# BB#29:                                # %if.then84
	movq	$1, %rax
	movb	%al, reg_pending_barrier(%rip)
	jmp	.LBB5_41
.LBB5_30:                               # %if.else85
	leaq	88(%rsp), %rbx
	movq	%rbx, %rdi
	callq	bitmap_initialize
	movq	current_sched_info(%rip), %rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	*56(%rax)
	movq	reg_pending_uses(%rip), %rdi
	movq	$2, %rcx
	movq	%rdi, %rsi
	movq	%rbx, %rdx
	callq	bitmap_operation
	movq	%rbx, %rdi
	callq	bitmap_clear
	movq	16(%r15), %rbx
	jmp	.LBB5_32
	.align	16, 0x90
.LBB5_31:                               # %while.body
                                        #   in Loop: Header=BB5_32 Depth=1
	movq	8(%rbx), %rsi
	movq	$15, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
.LBB5_32:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB5_31
# BB#33:                                # %while.end
	movq	(%r15), %r14
	testq	%r14, %r14
	je	.LBB5_38
# BB#34:                                # %while.body100.lr.ph
	leaq	8(%r15), %rbx
	.align	16, 0x90
.LBB5_35:                               # %while.body100
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rbx
	movq	8(%rbx), %rax
	movzbl	3(%rax), %eax
	testq	$8, %rax
	je	.LBB5_37
# BB#36:                                # %if.then107
                                        #   in Loop: Header=BB5_35 Depth=1
	movq	8(%r14), %rsi
	movq	$15, %rdx
	movq	%r13, %rdi
	callq	add_dependence
.LBB5_37:                               # %if.end111
                                        #   in Loop: Header=BB5_35 Depth=1
	movq	16(%r14), %r14
	addq	$16, %rbx
	testq	%r14, %r14
	jne	.LBB5_35
.LBB5_38:                               # %while.end118
	movq	40(%r15), %rbx
	jmp	.LBB5_40
	.align	16, 0x90
.LBB5_39:                               # %for.body.i
                                        #   in Loop: Header=BB5_40 Depth=1
	movq	8(%rbx), %rsi
	movq	$14, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
.LBB5_40:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB5_39
.LBB5_41:                               # %if.end120
	movq	80(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB5_51
# BB#42:                                # %while.cond125.preheader
	movq	16(%rdi), %rsi
	testq	%rsi, %rsi
	je	.LBB5_43
# BB#44:
	movq	$1, %rdx
	movq	%rdi, %rax
	.align	16, 0x90
.LBB5_45:                               # %while.body130
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rax
	movq	8(%rax), %rcx
	movq	%rsi, %rax
	addq	$96, %rcx
	cmpq	$11, %rcx
	ja	.LBB5_48
# BB#46:                                # %while.body130
                                        #   in Loop: Header=BB5_45 Depth=1
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$3075, %rsi             # imm = 0xC03
	je	.LBB5_48
# BB#47:                                # %if.then161
                                        #   in Loop: Header=BB5_45 Depth=1
	movb	%dl, reg_pending_barrier(%rip)
.LBB5_48:                               # %if.end162
                                        #   in Loop: Header=BB5_45 Depth=1
	movq	16(%rax), %rsi
	testq	%rsi, %rsi
	jne	.LBB5_45
# BB#49:
	addq	$16, %rax
	jmp	.LBB5_50
.LBB5_43:
	leaq	16(%rdi), %rax
.LBB5_50:                               # %while.end166
	movq	56(%r13), %rcx
	movq	%rcx, (%rax)
	movq	%rdi, 56(%r13)
.LBB5_51:                               # %if.end176
	movq	%r13, %rdi
	callq	can_throw_internal
	testq	%rax, %rax
	je	.LBB5_81
# BB#52:                                # %if.end179.thread
	movq	$1, %rax
	movb	%al, reg_pending_barrier(%rip)
	movq	32(%r13), %rax
	movzbl	(%rax), %eax
	movq	80(%r15), %rdx
	leaq	32(%r13), %rcx
	jmp	.LBB5_53
.LBB5_81:                               # %if.end179
	movq	32(%r13), %rax
	movzbl	reg_pending_barrier(%rip), %ecx
	movzbl	(%rax), %eax
	testq	%rcx, %rcx
	leaq	32(%r13), %rcx
	je	.LBB5_101
# BB#82:                                # %if.then181
	movq	80(%r15), %rdx
.LBB5_53:                               # %if.end179.thread
	cmpq	$38, %rax
	leaq	80(%r15), %rax
	jne	.LBB5_54
# BB#83:                                # %for.cond205.preheader
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	testq	%rdx, %rdx
	je	.LBB5_71
# BB#84:                                # %for.cond208.preheader.lr.ph
	movq	%r15, 56(%rsp)          # 8-byte Spill
	movq	%r13, %r12
	.align	16, 0x90
.LBB5_85:                               # %for.cond208.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_86 Depth 2
                                        #       Child Loop BB5_89 Depth 3
                                        #       Child Loop BB5_92 Depth 3
                                        #       Child Loop BB5_95 Depth 3
                                        #     Child Loop BB5_99 Depth 2
                                        #       Child Loop BB5_254 Depth 3
                                        #       Child Loop BB5_257 Depth 3
                                        #       Child Loop BB5_260 Depth 3
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	24(%rdx), %rsi
	testq	%rsi, %rsi
	movabsq	$0, %r13
	je	.LBB5_98
	.align	16, 0x90
.LBB5_86:                               # %for.body218
                                        #   Parent Loop BB5_85 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_89 Depth 3
                                        #       Child Loop BB5_92 Depth 3
                                        #       Child Loop BB5_95 Depth 3
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	testq	%rsi, %rax
	je	.LBB5_97
# BB#87:                                # %if.then221
                                        #   in Loop: Header=BB5_86 Depth=2
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movl	16(%rdx), %r15d
	movq	$7, %rcx
	shlq	%cl, %r15
	addq	%r13, %r15
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	72(%rax), %rbx
	sarq	%cl, %r15
	movq	$5, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	(%rbx,%rax), %r14
	jmp	.LBB5_89
	.align	16, 0x90
.LBB5_88:                               # %for.body.i1328
                                        #   in Loop: Header=BB5_89 Depth=3
	movq	8(%r14), %rsi
	movq	$14, %rdx
	movq	%r12, %rdi
	callq	add_dependence
	movq	16(%r14), %r14
.LBB5_89:                               # %for.body.i1328
                                        #   Parent Loop BB5_85 Depth=1
                                        #     Parent Loop BB5_86 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r14, %r14
	jne	.LBB5_88
# BB#90:                                # %add_dependence_list.exit1329
                                        #   in Loop: Header=BB5_86 Depth=2
	movq	$5, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	%rbx, %rcx
	movq	8(%rcx,%rax), %rbx
	movq	%rcx, %r14
	jmp	.LBB5_92
	.align	16, 0x90
.LBB5_91:                               # %for.body.i1337
                                        #   in Loop: Header=BB5_92 Depth=3
	movq	8(%rbx), %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
.LBB5_92:                               # %for.body.i1337
                                        #   Parent Loop BB5_85 Depth=1
                                        #     Parent Loop BB5_86 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	jne	.LBB5_91
# BB#93:                                # %add_dependence_list.exit1338
                                        #   in Loop: Header=BB5_86 Depth=2
	movq	$5, %rcx
	shlq	%cl, %r15
	movq	16(%r14,%r15), %rbx
	jmp	.LBB5_95
	.align	16, 0x90
.LBB5_94:                               # %for.body.i1346
                                        #   in Loop: Header=BB5_95 Depth=3
	movq	8(%rbx), %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
.LBB5_95:                               # %for.body.i1346
                                        #   Parent Loop BB5_85 Depth=1
                                        #     Parent Loop BB5_86 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	jne	.LBB5_94
# BB#96:                                # %add_dependence_list.exit1347
                                        #   in Loop: Header=BB5_86 Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
	notq	%rax
	movq	80(%rsp), %rsi          # 8-byte Reload
	andq	%rax, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	je	.LBB5_98
.LBB5_97:                               # %for.inc234
                                        #   in Loop: Header=BB5_86 Depth=2
	incq	%r13
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB5_86
.LBB5_98:                               # %if.end236
                                        #   in Loop: Header=BB5_85 Depth=1
	movq	32(%rdx), %rsi
	xorq	%r13, %r13
	testq	%rsi, %rsi
	je	.LBB5_263
	.align	16, 0x90
.LBB5_99:                               # %for.body218.1
                                        #   Parent Loop BB5_85 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_254 Depth 3
                                        #       Child Loop BB5_257 Depth 3
                                        #       Child Loop BB5_260 Depth 3
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	testq	%rsi, %rax
	je	.LBB5_262
# BB#100:                               # %if.then221.1
                                        #   in Loop: Header=BB5_99 Depth=2
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movl	16(%rdx), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	64(%r13,%rax), %r15
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	72(%rax), %rbx
	sarq	%cl, %r15
	movq	$5, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	(%rbx,%rax), %r14
	jmp	.LBB5_254
	.align	16, 0x90
.LBB5_253:                              # %for.body.i1328.1
                                        #   in Loop: Header=BB5_254 Depth=3
	movq	8(%r14), %rsi
	movq	$14, %rdx
	movq	%r12, %rdi
	callq	add_dependence
	movq	16(%r14), %r14
.LBB5_254:                              # %for.body.i1328.1
                                        #   Parent Loop BB5_85 Depth=1
                                        #     Parent Loop BB5_99 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r14, %r14
	jne	.LBB5_253
# BB#255:                               # %add_dependence_list.exit1329.1
                                        #   in Loop: Header=BB5_99 Depth=2
	movq	$5, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	%rbx, %rcx
	movq	8(%rcx,%rax), %rbx
	movq	%rcx, %r14
	jmp	.LBB5_257
	.align	16, 0x90
.LBB5_256:                              # %for.body.i1337.1
                                        #   in Loop: Header=BB5_257 Depth=3
	movq	8(%rbx), %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
.LBB5_257:                              # %for.body.i1337.1
                                        #   Parent Loop BB5_85 Depth=1
                                        #     Parent Loop BB5_99 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	jne	.LBB5_256
# BB#258:                               # %add_dependence_list.exit1338.1
                                        #   in Loop: Header=BB5_99 Depth=2
	movq	$5, %rcx
	shlq	%cl, %r15
	movq	16(%r14,%r15), %rbx
	jmp	.LBB5_260
	.align	16, 0x90
.LBB5_259:                              # %for.body.i1346.1
                                        #   in Loop: Header=BB5_260 Depth=3
	movq	8(%rbx), %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
.LBB5_260:                              # %for.body.i1346.1
                                        #   Parent Loop BB5_85 Depth=1
                                        #     Parent Loop BB5_99 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	jne	.LBB5_259
# BB#261:                               # %add_dependence_list.exit1347.1
                                        #   in Loop: Header=BB5_99 Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
	notq	%rax
	movq	80(%rsp), %rsi          # 8-byte Reload
	andq	%rax, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	je	.LBB5_263
.LBB5_262:                              # %for.inc234.1
                                        #   in Loop: Header=BB5_99 Depth=2
	incq	%r13
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB5_99
.LBB5_263:                              # %if.end236.1
                                        #   in Loop: Header=BB5_85 Depth=1
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB5_85
# BB#264:
	movq	%r12, %r13
	jmp	.LBB5_70
.LBB5_54:                               # %for.cond270.preheader
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	testq	%rdx, %rdx
	je	.LBB5_71
# BB#55:
	movq	%r15, 56(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB5_56:                               # %for.cond273.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_57 Depth 2
                                        #       Child Loop BB5_59 Depth 3
                                        #         Child Loop BB5_61 Depth 4
                                        #         Child Loop BB5_63 Depth 4
                                        #         Child Loop BB5_65 Depth 4
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%r13, %r15
	xorq	%rax, %rax
	.align	16, 0x90
.LBB5_57:                               # %for.body275
                                        #   Parent Loop BB5_56 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_59 Depth 3
                                        #         Child Loop BB5_61 Depth 4
                                        #         Child Loop BB5_63 Depth 4
                                        #         Child Loop BB5_65 Depth 4
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	24(%rdx,%rax,8), %rsi
	testq	%rsi, %rsi
	je	.LBB5_68
# BB#58:                                # %for.body285.preheader
                                        #   in Loop: Header=BB5_57 Depth=2
	movq	$6, %rcx
	movq	24(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	.align	16, 0x90
.LBB5_59:                               # %for.body285
                                        #   Parent Loop BB5_56 Depth=1
                                        #     Parent Loop BB5_57 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB5_61 Depth 4
                                        #         Child Loop BB5_63 Depth 4
                                        #         Child Loop BB5_65 Depth 4
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	testq	%rsi, %rax
	je	.LBB5_67
# BB#60:                                # %if.then292
                                        #   in Loop: Header=BB5_59 Depth=3
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	movl	16(%rdx), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	leaq	(%r13,%rcx), %r12
	addq	%rax, %r12
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	72(%rax), %rdx
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	sarq	%cl, %r12
	movq	$5, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	(%rdx,%rax), %rbx
	testq	%rbx, %rbx
	movq	$0, (%rdx,%rax)
	je	.LBB5_62
	.align	16, 0x90
.LBB5_61:                               # %for.body.i1352
                                        #   Parent Loop BB5_56 Depth=1
                                        #     Parent Loop BB5_57 Depth=2
                                        #       Parent Loop BB5_59 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r14
	movq	$14, %rdx
	movq	%r15, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r14, %r14
	movq	%r14, %rbx
	jne	.LBB5_61
.LBB5_62:                               # %add_dependence_list_and_free.exit
                                        #   in Loop: Header=BB5_59 Depth=3
	movq	$5, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx,%rax), %rbx
	testq	%rbx, %rbx
	movq	$0, 8(%rcx,%rax)
	je	.LBB5_64
	.align	16, 0x90
.LBB5_63:                               # %for.body.i1360
                                        #   Parent Loop BB5_56 Depth=1
                                        #     Parent Loop BB5_57 Depth=2
                                        #       Parent Loop BB5_59 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r14
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r14, %r14
	movq	%r14, %rbx
	jne	.LBB5_63
.LBB5_64:                               # %add_dependence_list_and_free.exit1361
                                        #   in Loop: Header=BB5_59 Depth=3
	movq	$5, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx,%rax), %rbx
	testq	%rbx, %rbx
	movq	$0, 16(%rcx,%rax)
	je	.LBB5_66
	.align	16, 0x90
.LBB5_65:                               # %for.body.i1369
                                        #   Parent Loop BB5_56 Depth=1
                                        #     Parent Loop BB5_57 Depth=2
                                        #       Parent Loop BB5_59 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r14
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r14, %r14
	movq	%r14, %rbx
	jne	.LBB5_65
.LBB5_66:                               # %add_dependence_list_and_free.exit1370
                                        #   in Loop: Header=BB5_59 Depth=3
	movq	64(%rsp), %rax          # 8-byte Reload
	notq	%rax
	movq	72(%rsp), %rsi          # 8-byte Reload
	andq	%rax, %rsi
	movq	$5, %rcx
	shlq	%cl, %r12
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	$0, 24(%rax,%r12)
	testq	%rsi, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	je	.LBB5_68
.LBB5_67:                               # %for.inc312
                                        #   in Loop: Header=BB5_59 Depth=3
	incq	%r13
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB5_59
.LBB5_68:                               # %if.end315
                                        #   in Loop: Header=BB5_57 Depth=2
	movq	24(%rsp), %rax          # 8-byte Reload
	incq	%rax
	cmpq	$2, %rax
	jne	.LBB5_57
# BB#69:                                # %for.end318
                                        #   in Loop: Header=BB5_56 Depth=1
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	movq	%r15, %r13
	jne	.LBB5_56
.LBB5_70:
	movq	56(%rsp), %r15          # 8-byte Reload
.LBB5_71:                               # %for.cond324.preheader
	movslq	68(%r15), %rax
	testq	%rax, %rax
	jle	.LBB5_74
# BB#72:                                # %for.body326.lr.ph
	movq	$1, %r14
	movq	$8, %rbx
	.align	16, 0x90
.LBB5_73:                               # %for.body326
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %r12
	movq	72(%r12), %r15
	movq	(%r15,%rbx), %rsi
	movq	%r13, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, (%r15,%rbx)
	movq	%r12, %r15
	movq	16(%rsp), %rdi          # 8-byte Reload
	leaq	-1(%r14), %rsi
	callq	bitmap_set_bit
	movslq	68(%r15), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r14
	addq	$32, %rbx
	cmpq	%rax, %rdx
	jl	.LBB5_73
.LBB5_74:                               # %for.end338
	movq	(%r15), %rbx
	testq	%rbx, %rbx
	movq	$0, (%r15)
	je	.LBB5_76
	.align	16, 0x90
.LBB5_75:                               # %for.body.i1553
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r14
	movq	$14, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r14, %r14
	movq	%r14, %rbx
	jne	.LBB5_75
.LBB5_76:                               # %add_dependence_list_and_free.exit1554
	leaq	8(%r15), %rdi
	callq	free_EXPR_LIST_list
	movq	16(%r15), %rbx
	testq	%rbx, %rbx
	movq	$0, 16(%r15)
	je	.LBB5_78
	.align	16, 0x90
.LBB5_77:                               # %for.body.i1562
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r14
	movq	$14, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r14, %r14
	movq	%r14, %rbx
	jne	.LBB5_77
.LBB5_78:                               # %add_dependence_list_and_free.exit1563
	leaq	24(%r15), %rdi
	callq	free_EXPR_LIST_list
	xorq	%r14, %r14
	movl	%r14d, 32(%r15)
	movq	40(%r15), %rbx
	testq	%rbx, %rbx
	movq	$0, 40(%r15)
	movq	%r15, %r12
	je	.LBB5_80
	.align	16, 0x90
.LBB5_79:                               # %for.body.i1571
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r15
	movq	$14, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r15, %r15
	movq	%r15, %rbx
	jne	.LBB5_79
.LBB5_80:                               # %add_dependence_list_and_free.exit1572
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	alloc_INSN_LIST
	movq	%r12, %rbx
	movq	%rax, 40(%rbx)
	movq	$1, %rax
	movl	%eax, 36(%rbx)
	movb	%r14b, reg_pending_barrier(%rip)
	jmp	.LBB5_206
.LBB5_101:                              # %if.else339
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%r15, 56(%rsp)          # 8-byte Spill
	movq	reg_pending_uses(%rip), %rcx
	movq	(%rcx), %rdx
	cmpq	$38, %rax
	je	.LBB5_106
	jmp	.LBB5_102
	.align	16, 0x90
.LBB5_286:                              # %if.end421.1
                                        #   in Loop: Header=BB5_106 Depth=1
	movq	(%rdx), %rdx
.LBB5_106:                              # %for.cond372.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_108 Depth 2
                                        #       Child Loop BB5_111 Depth 3
                                        #       Child Loop BB5_113 Depth 3
                                        #     Child Loop BB5_117 Depth 2
                                        #       Child Loop BB5_281 Depth 3
                                        #       Child Loop BB5_283 Depth 3
	testq	%rdx, %rdx
	je	.LBB5_119
# BB#107:                               # %for.cond375.preheader
                                        #   in Loop: Header=BB5_106 Depth=1
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	movq	24(%rdx), %rbx
	testq	%rbx, %rbx
	movabsq	$0, %r12
	je	.LBB5_116
	.align	16, 0x90
.LBB5_108:                              # %for.body387
                                        #   Parent Loop BB5_106 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_111 Depth 3
                                        #       Child Loop BB5_113 Depth 3
	movq	$1, %rax
	movq	%r12, %rcx
	shlq	%cl, %rax
	testq	%rbx, %rax
	je	.LBB5_115
# BB#109:                               # %if.then394
                                        #   in Loop: Header=BB5_108 Depth=2
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movl	16(%rdx), %r15d
	movq	$7, %rcx
	shlq	%cl, %r15
	addq	%r12, %r15
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	72(%rax), %rdx
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	sarq	%cl, %r15
	movq	$5, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	%r13, %r14
	movq	8(%rdx,%rax), %r13
	jmp	.LBB5_111
	.align	16, 0x90
.LBB5_110:                              # %for.body.i1378
                                        #   in Loop: Header=BB5_111 Depth=3
	movq	8(%r13), %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	add_dependence
	movq	16(%r13), %r13
.LBB5_111:                              # %for.body.i1378
                                        #   Parent Loop BB5_106 Depth=1
                                        #     Parent Loop BB5_108 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r13, %r13
	jne	.LBB5_110
# BB#112:                               # %add_dependence_list.exit1379
                                        #   in Loop: Header=BB5_108 Depth=2
	movq	$5, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx,%rax), %rbx
	testq	%rbx, %rbx
	movq	%r14, %r13
	je	.LBB5_114
	.align	16, 0x90
.LBB5_113:                              # %for.body.i1387
                                        #   Parent Loop BB5_106 Depth=1
                                        #     Parent Loop BB5_108 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%rbx), %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB5_113
.LBB5_114:                              # %add_dependence_list.exit1388
                                        #   in Loop: Header=BB5_108 Depth=2
	movq	$5, %rcx
	shlq	%cl, %r15
	movq	72(%rsp), %r14          # 8-byte Reload
	movq	(%r14,%r15), %rsi
	movq	48(%rsp), %rax          # 8-byte Reload
	notq	%rax
	movq	64(%rsp), %rbx          # 8-byte Reload
	andq	%rax, %rbx
	movq	%r13, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, (%r14,%r15)
	movl	24(%r14,%r15), %eax
	incq	%rax
	movl	%eax, 24(%r14,%r15)
	testq	%rbx, %rbx
	movq	80(%rsp), %rdx          # 8-byte Reload
	je	.LBB5_116
.LBB5_115:                              # %for.inc418
                                        #   in Loop: Header=BB5_108 Depth=2
	incq	%r12
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB5_108
.LBB5_116:                              # %if.end421
                                        #   in Loop: Header=BB5_106 Depth=1
	movq	32(%rdx), %rbx
	xorq	%r12, %r12
	testq	%rbx, %rbx
	je	.LBB5_286
	.align	16, 0x90
.LBB5_117:                              # %for.body387.1
                                        #   Parent Loop BB5_106 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_281 Depth 3
                                        #       Child Loop BB5_283 Depth 3
	movq	$1, %rax
	movq	%r12, %rcx
	shlq	%cl, %rax
	testq	%rbx, %rax
	je	.LBB5_285
# BB#118:                               # %if.then394.1
                                        #   in Loop: Header=BB5_117 Depth=2
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movl	16(%rdx), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	64(%r12,%rax), %r15
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	72(%rax), %rbx
	sarq	%cl, %r15
	movq	$5, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	%r13, %r14
	movq	8(%rbx,%rax), %r13
	jmp	.LBB5_281
	.align	16, 0x90
.LBB5_280:                              # %for.body.i1378.1
                                        #   in Loop: Header=BB5_281 Depth=3
	movq	8(%r13), %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	add_dependence
	movq	16(%r13), %r13
.LBB5_281:                              # %for.body.i1378.1
                                        #   Parent Loop BB5_106 Depth=1
                                        #     Parent Loop BB5_117 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r13, %r13
	jne	.LBB5_280
# BB#282:                               # %add_dependence_list.exit1379.1
                                        #   in Loop: Header=BB5_117 Depth=2
	movq	$5, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	16(%rbx,%rax), %rbx
	testq	%rbx, %rbx
	movq	%r14, %r13
	je	.LBB5_284
	.align	16, 0x90
.LBB5_283:                              # %for.body.i1387.1
                                        #   Parent Loop BB5_106 Depth=1
                                        #     Parent Loop BB5_117 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%rbx), %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB5_283
.LBB5_284:                              # %add_dependence_list.exit1388.1
                                        #   in Loop: Header=BB5_117 Depth=2
	movq	$5, %rcx
	shlq	%cl, %r15
	movq	48(%rsp), %r14          # 8-byte Reload
	movq	(%r14,%r15), %rsi
	movq	64(%rsp), %rax          # 8-byte Reload
	notq	%rax
	movq	72(%rsp), %rbx          # 8-byte Reload
	andq	%rax, %rbx
	movq	%r13, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, (%r14,%r15)
	movl	24(%r14,%r15), %eax
	incq	%rax
	movl	%eax, 24(%r14,%r15)
	testq	%rbx, %rbx
	movq	80(%rsp), %rdx          # 8-byte Reload
	je	.LBB5_286
.LBB5_285:                              # %for.inc418.1
                                        #   in Loop: Header=BB5_117 Depth=2
	incq	%r12
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB5_117
	jmp	.LBB5_286
	.align	16, 0x90
.LBB5_293:                              # %if.end667.1
                                        #   in Loop: Header=BB5_102 Depth=1
	movq	(%rdx), %rdx
.LBB5_102:                              # %for.cond618.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_104 Depth 2
                                        #       Child Loop BB5_153 Depth 3
                                        #       Child Loop BB5_155 Depth 3
                                        #     Child Loop BB5_159 Depth 2
                                        #       Child Loop BB5_288 Depth 3
                                        #       Child Loop BB5_290 Depth 3
	testq	%rdx, %rdx
	je	.LBB5_161
# BB#103:                               # %for.cond621.preheader
                                        #   in Loop: Header=BB5_102 Depth=1
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	movq	24(%rdx), %rbx
	testq	%rbx, %rbx
	movabsq	$0, %r12
	je	.LBB5_158
	.align	16, 0x90
.LBB5_104:                              # %for.body633
                                        #   Parent Loop BB5_102 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_153 Depth 3
                                        #       Child Loop BB5_155 Depth 3
	movq	$1, %rax
	movq	%r12, %rcx
	shlq	%cl, %rax
	testq	%rbx, %rax
	je	.LBB5_157
# BB#105:                               # %if.then640
                                        #   in Loop: Header=BB5_104 Depth=2
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movl	16(%rdx), %r15d
	movq	$7, %rcx
	shlq	%cl, %r15
	addq	%r12, %r15
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	72(%rax), %rdx
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	sarq	%cl, %r15
	movq	$5, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	%r13, %r14
	movq	8(%rdx,%rax), %r13
	jmp	.LBB5_153
	.align	16, 0x90
.LBB5_152:                              # %for.body.i1441
                                        #   in Loop: Header=BB5_153 Depth=3
	movq	8(%r13), %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	add_dependence
	movq	16(%r13), %r13
.LBB5_153:                              # %for.body.i1441
                                        #   Parent Loop BB5_102 Depth=1
                                        #     Parent Loop BB5_104 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r13, %r13
	jne	.LBB5_152
# BB#154:                               # %add_dependence_list.exit1442
                                        #   in Loop: Header=BB5_104 Depth=2
	movq	$5, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx,%rax), %rbx
	testq	%rbx, %rbx
	movq	%r14, %r13
	je	.LBB5_156
	.align	16, 0x90
.LBB5_155:                              # %for.body.i1450
                                        #   Parent Loop BB5_102 Depth=1
                                        #     Parent Loop BB5_104 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%rbx), %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB5_155
.LBB5_156:                              # %add_dependence_list.exit1451
                                        #   in Loop: Header=BB5_104 Depth=2
	movq	$5, %rcx
	shlq	%cl, %r15
	movq	72(%rsp), %r14          # 8-byte Reload
	movl	24(%r14,%r15), %eax
	incq	%rax
	movl	%eax, 24(%r14,%r15)
	movq	(%r14,%r15), %rsi
	movq	48(%rsp), %rax          # 8-byte Reload
	notq	%rax
	movq	64(%rsp), %rbx          # 8-byte Reload
	andq	%rax, %rbx
	movq	%r13, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, (%r14,%r15)
	testq	%rbx, %rbx
	movq	80(%rsp), %rdx          # 8-byte Reload
	je	.LBB5_158
.LBB5_157:                              # %for.inc664
                                        #   in Loop: Header=BB5_104 Depth=2
	incq	%r12
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB5_104
.LBB5_158:                              # %if.end667
                                        #   in Loop: Header=BB5_102 Depth=1
	movq	32(%rdx), %rbx
	xorq	%r12, %r12
	testq	%rbx, %rbx
	je	.LBB5_293
	.align	16, 0x90
.LBB5_159:                              # %for.body633.1
                                        #   Parent Loop BB5_102 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_288 Depth 3
                                        #       Child Loop BB5_290 Depth 3
	movq	$1, %rax
	movq	%r12, %rcx
	shlq	%cl, %rax
	testq	%rbx, %rax
	je	.LBB5_292
# BB#160:                               # %if.then640.1
                                        #   in Loop: Header=BB5_159 Depth=2
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movl	16(%rdx), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	64(%r12,%rax), %r15
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	72(%rax), %rbx
	sarq	%cl, %r15
	movq	$5, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	%r13, %r14
	movq	8(%rbx,%rax), %r13
	jmp	.LBB5_288
	.align	16, 0x90
.LBB5_287:                              # %for.body.i1441.1
                                        #   in Loop: Header=BB5_288 Depth=3
	movq	8(%r13), %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	add_dependence
	movq	16(%r13), %r13
.LBB5_288:                              # %for.body.i1441.1
                                        #   Parent Loop BB5_102 Depth=1
                                        #     Parent Loop BB5_159 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r13, %r13
	jne	.LBB5_287
# BB#289:                               # %add_dependence_list.exit1442.1
                                        #   in Loop: Header=BB5_159 Depth=2
	movq	$5, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	16(%rbx,%rax), %rbx
	testq	%rbx, %rbx
	movq	%r14, %r13
	je	.LBB5_291
	.align	16, 0x90
.LBB5_290:                              # %for.body.i1450.1
                                        #   Parent Loop BB5_102 Depth=1
                                        #     Parent Loop BB5_159 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%rbx), %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB5_290
.LBB5_291:                              # %add_dependence_list.exit1451.1
                                        #   in Loop: Header=BB5_159 Depth=2
	movq	$5, %rcx
	shlq	%cl, %r15
	movq	48(%rsp), %r14          # 8-byte Reload
	movl	24(%r14,%r15), %eax
	incq	%rax
	movl	%eax, 24(%r14,%r15)
	movq	(%r14,%r15), %rsi
	movq	64(%rsp), %rax          # 8-byte Reload
	notq	%rax
	movq	72(%rsp), %rbx          # 8-byte Reload
	andq	%rax, %rbx
	movq	%r13, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, (%r14,%r15)
	testq	%rbx, %rbx
	movq	80(%rsp), %rdx          # 8-byte Reload
	je	.LBB5_293
.LBB5_292:                              # %for.inc664.1
                                        #   in Loop: Header=BB5_159 Depth=2
	incq	%r12
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB5_159
	jmp	.LBB5_293
.LBB5_119:                              # %do.body429
	movq	reg_pending_clobbers(%rip), %rax
	movq	(%rax), %rdx
	jmp	.LBB5_120
	.align	16, 0x90
.LBB5_279:                              # %if.end503.1
                                        #   in Loop: Header=BB5_120 Depth=1
	movq	(%rdx), %rdx
.LBB5_120:                              # %do.body429
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_122 Depth 2
                                        #       Child Loop BB5_125 Depth 3
                                        #       Child Loop BB5_127 Depth 3
                                        #     Child Loop BB5_131 Depth 2
                                        #       Child Loop BB5_274 Depth 3
                                        #       Child Loop BB5_276 Depth 3
	testq	%rdx, %rdx
	je	.LBB5_133
# BB#121:                               # %for.cond457.preheader
                                        #   in Loop: Header=BB5_120 Depth=1
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	movq	24(%rdx), %rbx
	xorq	%r12, %r12
	testq	%rbx, %rbx
	je	.LBB5_130
	.align	16, 0x90
.LBB5_122:                              # %for.body469
                                        #   Parent Loop BB5_120 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_125 Depth 3
                                        #       Child Loop BB5_127 Depth 3
	movq	$1, %rax
	movq	%r12, %rcx
	shlq	%cl, %rax
	testq	%rbx, %rax
	je	.LBB5_129
# BB#123:                               # %if.then476
                                        #   in Loop: Header=BB5_122 Depth=2
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movl	16(%rdx), %r14d
	movq	$7, %rcx
	shlq	%cl, %r14
	addq	%r12, %r14
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	72(%rax), %rbx
	sarq	%cl, %r14
	movq	$5, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	%r13, %r15
	movq	8(%rbx,%rax), %r13
	jmp	.LBB5_125
	.align	16, 0x90
.LBB5_124:                              # %for.body.i1396
                                        #   in Loop: Header=BB5_125 Depth=3
	movq	8(%r13), %rsi
	movq	$15, %rdx
	movq	%r15, %rdi
	callq	add_dependence
	movq	16(%r13), %r13
.LBB5_125:                              # %for.body.i1396
                                        #   Parent Loop BB5_120 Depth=1
                                        #     Parent Loop BB5_122 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r13, %r13
	jne	.LBB5_124
# BB#126:                               # %add_dependence_list.exit1397
                                        #   in Loop: Header=BB5_122 Depth=2
	movq	$5, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	(%rbx,%rax), %rbx
	testq	%rbx, %rbx
	movq	%r15, %r13
	je	.LBB5_128
	.align	16, 0x90
.LBB5_127:                              # %for.body.i1405
                                        #   Parent Loop BB5_120 Depth=1
                                        #     Parent Loop BB5_122 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%rbx), %rsi
	movq	$14, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB5_127
.LBB5_128:                              # %add_dependence_list.exit1406
                                        #   in Loop: Header=BB5_122 Depth=2
	movq	$5, %rcx
	shlq	%cl, %r14
	movq	48(%rsp), %r15          # 8-byte Reload
	movq	16(%r15,%r14), %rsi
	movq	64(%rsp), %rax          # 8-byte Reload
	notq	%rax
	movq	72(%rsp), %rbx          # 8-byte Reload
	andq	%rax, %rbx
	movq	%r13, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, 16(%r15,%r14)
	movl	28(%r15,%r14), %eax
	incq	%rax
	movl	%eax, 28(%r15,%r14)
	testq	%rbx, %rbx
	movq	80(%rsp), %rdx          # 8-byte Reload
	je	.LBB5_130
.LBB5_129:                              # %for.inc500
                                        #   in Loop: Header=BB5_122 Depth=2
	incq	%r12
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB5_122
.LBB5_130:                              # %if.end503
                                        #   in Loop: Header=BB5_120 Depth=1
	movq	32(%rdx), %rbx
	xorq	%r12, %r12
	testq	%rbx, %rbx
	je	.LBB5_279
	.align	16, 0x90
.LBB5_131:                              # %for.body469.1
                                        #   Parent Loop BB5_120 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_274 Depth 3
                                        #       Child Loop BB5_276 Depth 3
	movq	$1, %rax
	movq	%r12, %rcx
	shlq	%cl, %rax
	testq	%rbx, %rax
	je	.LBB5_278
# BB#132:                               # %if.then476.1
                                        #   in Loop: Header=BB5_131 Depth=2
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movl	16(%rdx), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	64(%r12,%rax), %r14
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	72(%rax), %rbx
	sarq	%cl, %r14
	movq	$5, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	%r13, %r15
	movq	8(%rbx,%rax), %r13
	jmp	.LBB5_274
	.align	16, 0x90
.LBB5_273:                              # %for.body.i1396.1
                                        #   in Loop: Header=BB5_274 Depth=3
	movq	8(%r13), %rsi
	movq	$15, %rdx
	movq	%r15, %rdi
	callq	add_dependence
	movq	16(%r13), %r13
.LBB5_274:                              # %for.body.i1396.1
                                        #   Parent Loop BB5_120 Depth=1
                                        #     Parent Loop BB5_131 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r13, %r13
	jne	.LBB5_273
# BB#275:                               # %add_dependence_list.exit1397.1
                                        #   in Loop: Header=BB5_131 Depth=2
	movq	$5, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	(%rbx,%rax), %rbx
	testq	%rbx, %rbx
	movq	%r15, %r13
	je	.LBB5_277
	.align	16, 0x90
.LBB5_276:                              # %for.body.i1405.1
                                        #   Parent Loop BB5_120 Depth=1
                                        #     Parent Loop BB5_131 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%rbx), %rsi
	movq	$14, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB5_276
.LBB5_277:                              # %add_dependence_list.exit1406.1
                                        #   in Loop: Header=BB5_131 Depth=2
	movq	$5, %rcx
	shlq	%cl, %r14
	movq	48(%rsp), %r15          # 8-byte Reload
	movq	16(%r15,%r14), %rsi
	movq	64(%rsp), %rax          # 8-byte Reload
	notq	%rax
	movq	72(%rsp), %rbx          # 8-byte Reload
	andq	%rax, %rbx
	movq	%r13, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, 16(%r15,%r14)
	movl	28(%r15,%r14), %eax
	incq	%rax
	movl	%eax, 28(%r15,%r14)
	testq	%rbx, %rbx
	movq	80(%rsp), %rdx          # 8-byte Reload
	je	.LBB5_279
.LBB5_278:                              # %for.inc500.1
                                        #   in Loop: Header=BB5_131 Depth=2
	incq	%r12
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB5_131
	jmp	.LBB5_279
.LBB5_161:                              # %do.body675
	movq	%r13, 32(%rsp)          # 8-byte Spill
	movq	reg_pending_clobbers(%rip), %rax
	jmp	.LBB5_162
	.align	16, 0x90
.LBB5_188:                              # %for.end770
                                        #   in Loop: Header=BB5_162 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
.LBB5_162:                              # %do.body675
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_164 Depth 2
                                        #       Child Loop BB5_166 Depth 3
                                        #         Child Loop BB5_168 Depth 4
                                        #         Child Loop BB5_171 Depth 4
                                        #         Child Loop BB5_182 Depth 4
                                        #         Child Loop BB5_184 Depth 4
                                        #         Child Loop BB5_175 Depth 4
                                        #         Child Loop BB5_177 Depth 4
                                        #         Child Loop BB5_179 Depth 4
	movq	(%rax), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB5_189
# BB#163:                               # %for.cond703.preheader
                                        #   in Loop: Header=BB5_162 Depth=1
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB5_164:                              # %for.body705
                                        #   Parent Loop BB5_162 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_166 Depth 3
                                        #         Child Loop BB5_168 Depth 4
                                        #         Child Loop BB5_171 Depth 4
                                        #         Child Loop BB5_182 Depth 4
                                        #         Child Loop BB5_184 Depth 4
                                        #         Child Loop BB5_175 Depth 4
                                        #         Child Loop BB5_177 Depth 4
                                        #         Child Loop BB5_179 Depth 4
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	24(%rax,%rcx,8), %rdx
	testq	%rdx, %rdx
	je	.LBB5_187
# BB#165:                               # %for.body715.preheader
                                        #   in Loop: Header=BB5_164 Depth=2
	movq	$6, %rcx
	movq	16(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	jmp	.LBB5_166
.LBB5_181:                              # %if.else751
                                        #   in Loop: Header=BB5_166 Depth=3
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	movq	%r15, %r13
	je	.LBB5_183
	.align	16, 0x90
.LBB5_182:                              # %for.body.i1504
                                        #   Parent Loop BB5_162 Depth=1
                                        #     Parent Loop BB5_164 Depth=2
                                        #       Parent Loop BB5_166 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%rbx), %rsi
	movq	$15, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB5_182
.LBB5_183:                              # %add_dependence_list.exit1505
                                        #   in Loop: Header=BB5_166 Depth=3
	movq	(%r12), %rbx
	testq	%rbx, %rbx
	movq	80(%rsp), %r15          # 8-byte Reload
	je	.LBB5_185
	.align	16, 0x90
.LBB5_184:                              # %for.body.i1513
                                        #   Parent Loop BB5_162 Depth=1
                                        #     Parent Loop BB5_164 Depth=2
                                        #       Parent Loop BB5_166 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%rbx), %rsi
	movq	$14, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB5_184
	jmp	.LBB5_185
	.align	16, 0x90
.LBB5_166:                              # %for.body715
                                        #   Parent Loop BB5_162 Depth=1
                                        #     Parent Loop BB5_164 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB5_168 Depth 4
                                        #         Child Loop BB5_171 Depth 4
                                        #         Child Loop BB5_182 Depth 4
                                        #         Child Loop BB5_184 Depth 4
                                        #         Child Loop BB5_175 Depth 4
                                        #         Child Loop BB5_177 Depth 4
                                        #         Child Loop BB5_179 Depth 4
	movq	$1, %r13
	movq	%rsi, %rcx
	shlq	%cl, %r13
	testq	%rdx, %r13
	je	.LBB5_186
# BB#167:                               # %if.then722
                                        #   in Loop: Header=BB5_166 Depth=3
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	16(%rax), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	leaq	(%rsi,%rcx), %r14
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	addq	%rax, %r14
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	72(%rax), %rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	sarq	%cl, %r14
	movq	$5, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	8(%rax,%rdx), %rbx
	testq	%rbx, %rbx
	movq	32(%rsp), %r15          # 8-byte Reload
	je	.LBB5_169
	.align	16, 0x90
.LBB5_168:                              # %for.body.i1459
                                        #   Parent Loop BB5_162 Depth=1
                                        #     Parent Loop BB5_164 Depth=2
                                        #       Parent Loop BB5_166 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%rbx), %rsi
	movq	$15, %rdx
	movq	%r15, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB5_168
.LBB5_169:                              # %add_dependence_list.exit1460
                                        #   in Loop: Header=BB5_166 Depth=3
	movq	$5, %rcx
	movq	%r14, %r12
	shlq	%cl, %r12
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r12), %rbx
	jmp	.LBB5_171
	.align	16, 0x90
.LBB5_170:                              # %for.body.i1468
                                        #   in Loop: Header=BB5_171 Depth=4
	movq	8(%rbx), %rsi
	movq	$14, %rdx
	movq	%r15, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
.LBB5_171:                              # %for.body.i1468
                                        #   Parent Loop BB5_162 Depth=1
                                        #     Parent Loop BB5_164 Depth=2
                                        #       Parent Loop BB5_166 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	testq	%rbx, %rbx
	jne	.LBB5_170
# BB#172:                               # %add_dependence_list.exit1469
                                        #   in Loop: Header=BB5_166 Depth=3
	notq	%r13
	movq	compiler_params(%rip), %rax
	movq	$5, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	80(%rsp), %rdi          # 8-byte Reload
	movslq	24(%rdi,%rbx), %rcx
	movslq	80(%rax), %rdx
	andq	%r13, 72(%rsp)          # 8-byte Folded Spill
	movq	64(%rsp), %rax          # 8-byte Reload
	leaq	8(%rdi,%rax), %rax
	addq	%rdi, %r12
	cmpq	%rdx, %rcx
	jg	.LBB5_174
# BB#173:                               # %lor.lhs.false740
                                        #   in Loop: Header=BB5_166 Depth=3
	movq	$5, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movslq	28(%rdi,%rsi), %rcx
	cmpq	%rdx, %rcx
	jle	.LBB5_181
.LBB5_174:                              # %if.then745
                                        #   in Loop: Header=BB5_166 Depth=3
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	movq	$0, (%rax)
	je	.LBB5_176
	.align	16, 0x90
.LBB5_175:                              # %for.body.i1477
                                        #   Parent Loop BB5_162 Depth=1
                                        #     Parent Loop BB5_164 Depth=2
                                        #       Parent Loop BB5_166 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r13
	movq	$15, %rdx
	movq	%r15, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r13, %r13
	movq	%r13, %rbx
	jne	.LBB5_175
.LBB5_176:                              # %add_dependence_list_and_free.exit1478
                                        #   in Loop: Header=BB5_166 Depth=3
	movq	(%r12), %rbx
	testq	%rbx, %rbx
	movq	$0, (%r12)
	movq	%r15, %r13
	je	.LBB5_178
	.align	16, 0x90
.LBB5_177:                              # %for.body.i1486
                                        #   Parent Loop BB5_162 Depth=1
                                        #     Parent Loop BB5_164 Depth=2
                                        #       Parent Loop BB5_166 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r12
	movq	$14, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r12, %r12
	movq	%r12, %rbx
	jne	.LBB5_177
.LBB5_178:                              # %add_dependence_list_and_free.exit1487
                                        #   in Loop: Header=BB5_166 Depth=3
	movq	$5, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	80(%rsp), %r15          # 8-byte Reload
	movq	16(%r15,%rax), %rbx
	testq	%rbx, %rbx
	movq	$0, 16(%r15,%rax)
	je	.LBB5_180
	.align	16, 0x90
.LBB5_179:                              # %for.body.i1495
                                        #   Parent Loop BB5_162 Depth=1
                                        #     Parent Loop BB5_164 Depth=2
                                        #       Parent Loop BB5_166 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r12
	movq	$15, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r12, %r12
	movq	%r12, %rbx
	jne	.LBB5_179
.LBB5_180:                              # %add_dependence_list_and_free.exit1496
                                        #   in Loop: Header=BB5_166 Depth=3
	movq	64(%rsp), %rax          # 8-byte Reload
	leaq	24(%r15,%rax), %rax
	movq	$5, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	xorq	%rcx, %rcx
	movl	%ecx, 28(%r15,%rdx)
	movl	%ecx, (%rax)
.LBB5_185:                              # %if.end754
                                        #   in Loop: Header=BB5_166 Depth=3
	movq	$5, %rcx
	shlq	%cl, %r14
	movl	28(%r15,%r14), %eax
	incq	%rax
	movl	%eax, 28(%r15,%r14)
	movq	16(%r15,%r14), %rsi
	movq	%r13, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, 16(%r15,%r14)
	movq	72(%rsp), %rdx          # 8-byte Reload
	testq	%rdx, %rdx
	movq	48(%rsp), %rsi          # 8-byte Reload
	je	.LBB5_187
.LBB5_186:                              # %for.inc764
                                        #   in Loop: Header=BB5_166 Depth=3
	incq	%rsi
	movq	%rsi, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB5_166
.LBB5_187:                              # %if.end767
                                        #   in Loop: Header=BB5_164 Depth=2
	movq	16(%rsp), %rcx          # 8-byte Reload
	incq	%rcx
	cmpq	$2, %rcx
	jne	.LBB5_164
	jmp	.LBB5_188
.LBB5_133:                              # %do.body511
	movq	%r13, 32(%rsp)          # 8-byte Spill
	movq	reg_pending_sets(%rip), %rax
	movq	(%rax), %rdx
	jmp	.LBB5_134
	.align	16, 0x90
.LBB5_272:                              # %if.end584.1
                                        #   in Loop: Header=BB5_134 Depth=1
	movq	(%rdx), %rdx
.LBB5_134:                              # %do.body511
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_136 Depth 2
                                        #       Child Loop BB5_138 Depth 3
                                        #       Child Loop BB5_141 Depth 3
                                        #       Child Loop BB5_144 Depth 3
                                        #     Child Loop BB5_148 Depth 2
                                        #       Child Loop BB5_150 Depth 3
                                        #       Child Loop BB5_266 Depth 3
                                        #       Child Loop BB5_269 Depth 3
	testq	%rdx, %rdx
	je	.LBB5_205
# BB#135:                               # %for.cond539.preheader
                                        #   in Loop: Header=BB5_134 Depth=1
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	movq	24(%rdx), %rbx
	xorq	%r13, %r13
	testq	%rbx, %rbx
	je	.LBB5_147
	.align	16, 0x90
.LBB5_136:                              # %for.body551
                                        #   Parent Loop BB5_134 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_138 Depth 3
                                        #       Child Loop BB5_141 Depth 3
                                        #       Child Loop BB5_144 Depth 3
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	testq	%rbx, %rax
	je	.LBB5_146
# BB#137:                               # %if.then558
                                        #   in Loop: Header=BB5_136 Depth=2
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movl	16(%rdx), %r14d
	movq	$7, %rcx
	shlq	%cl, %r14
	addq	%r13, %r14
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	72(%rax), %rbx
	sarq	%cl, %r14
	movq	$5, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	8(%rbx,%rax), %r15
	testq	%r15, %r15
	movq	32(%rsp), %r12          # 8-byte Reload
	je	.LBB5_139
	.align	16, 0x90
.LBB5_138:                              # %for.body.i1414
                                        #   Parent Loop BB5_134 Depth=1
                                        #     Parent Loop BB5_136 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%r15), %rsi
	movq	$15, %rdx
	movq	%r12, %rdi
	callq	add_dependence
	movq	16(%r15), %r15
	testq	%r15, %r15
	jne	.LBB5_138
.LBB5_139:                              # %add_dependence_list.exit1415
                                        #   in Loop: Header=BB5_136 Depth=2
	movq	$5, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	%rbx, %r15
	movq	16(%r15,%rax), %rbx
	jmp	.LBB5_141
	.align	16, 0x90
.LBB5_140:                              # %for.body.i1423
                                        #   in Loop: Header=BB5_141 Depth=3
	movq	8(%rbx), %rsi
	movq	$15, %rdx
	movq	%r12, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
.LBB5_141:                              # %for.body.i1423
                                        #   Parent Loop BB5_134 Depth=1
                                        #     Parent Loop BB5_136 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	jne	.LBB5_140
# BB#142:                               # %add_dependence_list.exit1424
                                        #   in Loop: Header=BB5_136 Depth=2
	movq	$5, %rcx
	shlq	%cl, %r14
	movq	(%r15,%r14), %rbx
	jmp	.LBB5_144
	.align	16, 0x90
.LBB5_143:                              # %for.body.i1432
                                        #   in Loop: Header=BB5_144 Depth=3
	movq	8(%rbx), %rsi
	movq	$14, %rdx
	movq	%r12, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
.LBB5_144:                              # %for.body.i1432
                                        #   Parent Loop BB5_134 Depth=1
                                        #     Parent Loop BB5_136 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	jne	.LBB5_143
# BB#145:                               # %add_dependence_list.exit1433
                                        #   in Loop: Header=BB5_136 Depth=2
	movq	48(%rsp), %rax          # 8-byte Reload
	leaq	8(%r15,%rax), %r14
	movq	(%r14), %rsi
	movq	64(%rsp), %rax          # 8-byte Reload
	notq	%rax
	movq	72(%rsp), %rbx          # 8-byte Reload
	andq	%rax, %rbx
	movq	%r12, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, (%r14)
	testq	%rbx, %rbx
	movq	80(%rsp), %rdx          # 8-byte Reload
	je	.LBB5_147
.LBB5_146:                              # %for.inc581
                                        #   in Loop: Header=BB5_136 Depth=2
	incq	%r13
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB5_136
.LBB5_147:                              # %if.end584
                                        #   in Loop: Header=BB5_134 Depth=1
	movq	32(%rdx), %rbx
	xorq	%r13, %r13
	testq	%rbx, %rbx
	je	.LBB5_272
	.align	16, 0x90
.LBB5_148:                              # %for.body551.1
                                        #   Parent Loop BB5_134 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_150 Depth 3
                                        #       Child Loop BB5_266 Depth 3
                                        #       Child Loop BB5_269 Depth 3
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	testq	%rbx, %rax
	je	.LBB5_271
# BB#149:                               # %if.then558.1
                                        #   in Loop: Header=BB5_148 Depth=2
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movl	16(%rdx), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	64(%r13,%rax), %r14
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	72(%rax), %rbx
	sarq	%cl, %r14
	movq	$5, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	8(%rbx,%rax), %r15
	testq	%r15, %r15
	movq	32(%rsp), %r12          # 8-byte Reload
	je	.LBB5_151
	.align	16, 0x90
.LBB5_150:                              # %for.body.i1414.1
                                        #   Parent Loop BB5_134 Depth=1
                                        #     Parent Loop BB5_148 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%r15), %rsi
	movq	$15, %rdx
	movq	%r12, %rdi
	callq	add_dependence
	movq	16(%r15), %r15
	testq	%r15, %r15
	jne	.LBB5_150
.LBB5_151:                              # %add_dependence_list.exit1415.1
                                        #   in Loop: Header=BB5_148 Depth=2
	movq	$5, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	%rbx, %r15
	movq	16(%r15,%rax), %rbx
	jmp	.LBB5_266
	.align	16, 0x90
.LBB5_265:                              # %for.body.i1423.1
                                        #   in Loop: Header=BB5_266 Depth=3
	movq	8(%rbx), %rsi
	movq	$15, %rdx
	movq	%r12, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
.LBB5_266:                              # %for.body.i1423.1
                                        #   Parent Loop BB5_134 Depth=1
                                        #     Parent Loop BB5_148 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	jne	.LBB5_265
# BB#267:                               # %add_dependence_list.exit1424.1
                                        #   in Loop: Header=BB5_148 Depth=2
	movq	$5, %rcx
	shlq	%cl, %r14
	movq	(%r15,%r14), %rbx
	jmp	.LBB5_269
	.align	16, 0x90
.LBB5_268:                              # %for.body.i1432.1
                                        #   in Loop: Header=BB5_269 Depth=3
	movq	8(%rbx), %rsi
	movq	$14, %rdx
	movq	%r12, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
.LBB5_269:                              # %for.body.i1432.1
                                        #   Parent Loop BB5_134 Depth=1
                                        #     Parent Loop BB5_148 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	jne	.LBB5_268
# BB#270:                               # %add_dependence_list.exit1433.1
                                        #   in Loop: Header=BB5_148 Depth=2
	movq	48(%rsp), %rax          # 8-byte Reload
	leaq	8(%r15,%rax), %r14
	movq	(%r14), %rsi
	movq	64(%rsp), %rax          # 8-byte Reload
	notq	%rax
	movq	72(%rsp), %rbx          # 8-byte Reload
	andq	%rax, %rbx
	movq	%r12, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, (%r14)
	testq	%rbx, %rbx
	movq	80(%rsp), %rdx          # 8-byte Reload
	je	.LBB5_272
.LBB5_271:                              # %for.inc581.1
                                        #   in Loop: Header=BB5_148 Depth=2
	incq	%r13
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB5_148
	jmp	.LBB5_272
.LBB5_189:                              # %do.body775
	movq	reg_pending_sets(%rip), %rax
	movq	(%rax), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB5_205
# BB#190:                               # %for.cond803.preheader.lr.ph
	movq	32(%rsp), %r14          # 8-byte Reload
	.align	16, 0x90
.LBB5_191:                              # %for.cond803.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_192 Depth 2
                                        #       Child Loop BB5_194 Depth 3
                                        #         Child Loop BB5_196 Depth 4
                                        #         Child Loop BB5_198 Depth 4
                                        #         Child Loop BB5_200 Depth 4
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB5_192:                              # %for.body805
                                        #   Parent Loop BB5_191 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_194 Depth 3
                                        #         Child Loop BB5_196 Depth 4
                                        #         Child Loop BB5_198 Depth 4
                                        #         Child Loop BB5_200 Depth 4
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	24(%rax,%rcx,8), %rdx
	testq	%rdx, %rdx
	je	.LBB5_203
# BB#193:                               # %for.body815.preheader
                                        #   in Loop: Header=BB5_192 Depth=2
	movq	$6, %rcx
	movq	16(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%r15, %r15
	.align	16, 0x90
.LBB5_194:                              # %for.body815
                                        #   Parent Loop BB5_191 Depth=1
                                        #     Parent Loop BB5_192 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB5_196 Depth 4
                                        #         Child Loop BB5_198 Depth 4
                                        #         Child Loop BB5_200 Depth 4
	movq	$1, %rax
	movq	%r15, %rcx
	shlq	%cl, %rax
	testq	%rdx, %rax
	je	.LBB5_202
# BB#195:                               # %if.then822
                                        #   in Loop: Header=BB5_194 Depth=3
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	16(%rax), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	leaq	(%r15,%rcx), %r13
	addq	%rax, %r13
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	72(%rax), %rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	sarq	%cl, %r13
	movq	$5, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	8(%rax,%rdx), %rbx
	testq	%rbx, %rbx
	movq	$0, 8(%rax,%rdx)
	je	.LBB5_197
	.align	16, 0x90
.LBB5_196:                              # %for.body.i1522
                                        #   Parent Loop BB5_191 Depth=1
                                        #     Parent Loop BB5_192 Depth=2
                                        #       Parent Loop BB5_194 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r12
	movq	$15, %rdx
	movq	%r14, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r12, %r12
	movq	%r12, %rbx
	jne	.LBB5_196
.LBB5_197:                              # %add_dependence_list_and_free.exit1523
                                        #   in Loop: Header=BB5_194 Depth=3
	movq	$5, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx,%rax), %rbx
	testq	%rbx, %rbx
	movq	$0, 16(%rcx,%rax)
	je	.LBB5_199
	.align	16, 0x90
.LBB5_198:                              # %for.body.i1531
                                        #   Parent Loop BB5_191 Depth=1
                                        #     Parent Loop BB5_192 Depth=2
                                        #       Parent Loop BB5_194 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r12
	movq	$15, %rdx
	movq	%r14, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r12, %r12
	movq	%r12, %rbx
	jne	.LBB5_198
.LBB5_199:                              # %add_dependence_list_and_free.exit1532
                                        #   in Loop: Header=BB5_194 Depth=3
	movq	$5, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax), %rbx
	testq	%rbx, %rbx
	movq	$0, (%rcx,%rax)
	je	.LBB5_201
	.align	16, 0x90
.LBB5_200:                              # %for.body.i1540
                                        #   Parent Loop BB5_191 Depth=1
                                        #     Parent Loop BB5_192 Depth=2
                                        #       Parent Loop BB5_194 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%rbx), %rsi
	movq	16(%rbx), %r12
	movq	$14, %rdx
	movq	%r14, %rdi
	callq	add_dependence
	movq	%rbx, %rdi
	callq	free_INSN_LIST_node
	testq	%r12, %r12
	movq	%r12, %rbx
	jne	.LBB5_200
.LBB5_201:                              # %add_dependence_list_and_free.exit1541
                                        #   in Loop: Header=BB5_194 Depth=3
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	80(%rsp), %rcx          # 8-byte Reload
	leaq	8(%rcx,%rax), %rbx
	movq	(%rbx), %rsi
	movq	64(%rsp), %rax          # 8-byte Reload
	notq	%rax
	movq	72(%rsp), %r12          # 8-byte Reload
	andq	%rax, %r12
	movq	%r14, %rdi
	callq	alloc_INSN_LIST
	movq	%r12, %rdx
	movq	%rax, (%rbx)
	movq	$5, %rcx
	shlq	%cl, %r13
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	$0, 24(%rax,%r13)
	testq	%rdx, %rdx
	je	.LBB5_203
.LBB5_202:                              # %for.inc847
                                        #   in Loop: Header=BB5_194 Depth=3
	incq	%r15
	movq	%r15, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB5_194
.LBB5_203:                              # %if.end850
                                        #   in Loop: Header=BB5_192 Depth=2
	movq	16(%rsp), %rcx          # 8-byte Reload
	incq	%rcx
	cmpq	$2, %rcx
	jne	.LBB5_192
# BB#204:                               # %for.end853
                                        #   in Loop: Header=BB5_191 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB5_191
.LBB5_205:                              # %if.end858
	movq	reg_pending_uses(%rip), %rdx
	movq	56(%rsp), %r14          # 8-byte Reload
	leaq	80(%r14), %rbx
	movq	$2, %rcx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	bitmap_operation
	movq	reg_pending_clobbers(%rip), %rdx
	movq	$2, %rcx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	bitmap_operation
	movq	reg_pending_sets(%rip), %rdx
	movq	$2, %rcx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	bitmap_operation
	movq	32(%rsp), %r13          # 8-byte Reload
	movq	%r14, %rbx
.LBB5_206:                              # %if.end868
	movq	reg_pending_uses(%rip), %rdi
	callq	bitmap_clear
	movq	reg_pending_clobbers(%rip), %rdi
	callq	bitmap_clear
	movq	reg_pending_sets(%rip), %rdi
	callq	bitmap_clear
	movzbl	64(%rbx), %eax
	testq	%rax, %rax
	je	.LBB5_252
# BB#207:                               # %if.then870
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB5_251
# BB#208:                               # %cond.true
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB5_210
# BB#209:                               # %cond.false
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB5_210:                              # %cond.end896
	testq	%rax, %rax
	je	.LBB5_251
# BB#211:                               # %if.end903
	movq	8(%rax), %rcx
	movl	(%rcx), %edx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$63, %rsi
	jne	.LBB5_213
# BB#212:                               # %if.then911
	movq	8(%rcx), %rcx
	movl	(%rcx), %edx
.LBB5_213:                              # %if.end915
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB5_251
# BB#214:                               # %if.then920
	movq	16(%rax), %rdx
	movl	(%rdx), %esi
	movl	8(%rcx), %eax
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB5_216
# BB#215:                               # %if.then932
	movq	8(%rdx), %rdx
	movl	(%rdx), %esi
.LBB5_216:                              # %if.end936
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$61, %rsi
	jne	.LBB5_251
# BB#217:                               # %if.then941
	movslq	8(%rdx), %rcx
	cmpq	$53, %rcx
	jl	.LBB5_219
# BB#218:                               # %if.then941
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	jle	.LBB5_219
.LBB5_251:                              # %end_call_group
	xorq	%rax, %rax
	movb	%al, 64(%rbx)
.LBB5_252:                              # %if.end962
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB5_219:                              # %if.then952
	movl	(%r13), %eax
	orq	$268435456, %rax        # imm = 0x10000000
	movl	%eax, (%r13)
	movq	%r13, %rdi
	callq	prev_nonnote_insn
	movq	48(%r13), %rbx
	movq	%rax, %r14
	testq	%rbx, %rbx
	je	.LBB5_247
# BB#220:
	movq	%rbx, %rax
	.align	16, 0x90
.LBB5_221:                              # %while.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rcx
	cmpq	%r14, %rcx
	je	.LBB5_223
# BB#222:                               # %if.end.i.i
                                        #   in Loop: Header=BB5_221 Depth=1
	movq	16(%rax), %rax
	testq	%rax, %rax
	jne	.LBB5_221
	jmp	.LBB5_247
.LBB5_223:                              # %for.body.lr.ph.lr.ph.i.i
	xorq	%rax, %rax
	xorq	%r15, %r15
.LBB5_225:                              # %for.body.lr.ph.i.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_236 Depth 2
                                        #     Child Loop BB5_226 Depth 2
	testq	%r15, %r15
	jne	.LBB5_236
.LBB5_226:                              # %for.body.us.i.i
                                        #   Parent Loop BB5_225 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	movq	8(%rdi), %rcx
	movq	16(%rdi), %rbx
	cmpq	%r14, %rcx
	jne	.LBB5_224
# BB#227:                               # %if.end.us.i.i
                                        #   in Loop: Header=BB5_226 Depth=2
	movq	%rbx, 48(%r13)
	movq	true_dependency_cache(%rip), %rdx
	testq	%rdx, %rdx
	je	.LBB5_235
# BB#228:                               # %if.then16.us.i.i
                                        #   in Loop: Header=BB5_226 Depth=2
	movzbl	2(%rdi), %eax
	testq	%rax, %rax
	je	.LBB5_234
# BB#229:                               # %if.then16.us.i.i
                                        #   in Loop: Header=BB5_226 Depth=2
	cmpq	$14, %rax
	jne	.LBB5_230
# BB#233:                               # %if.then43.us.i.i
                                        #   in Loop: Header=BB5_226 Depth=2
	movslq	8(%r14), %rax
	movq	h_i_d(%rip), %r8
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	16(%r8,%rax), %eax
	movq	$1, %rsi
	movslq	8(%r13), %rdx
	movq	%rax, %rcx
	shlq	%cl, %rsi
	leaq	(%rdx,%rdx,2), %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movslq	16(%r8,%rdx), %rcx
	movq	anti_dependency_cache(%rip), %rdx
	jmp	.LBB5_232
.LBB5_234:                              # %if.then18.us.i.i
                                        #   in Loop: Header=BB5_226 Depth=2
	movslq	8(%r14), %rax
	movq	h_i_d(%rip), %r8
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	16(%r8,%rax), %r10d
	movslq	8(%r13), %r9
	movq	$1, %rsi
	movq	%r10, %rcx
	shlq	%cl, %rsi
	leaq	(%r9,%r9,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movslq	16(%r8,%rax), %rax
	movq	(%rdx,%rax,8), %rax
	notq	%rsi
	shrq	$6, %r10
	andq	%rsi, 16(%rax,%r10,8)
	jmp	.LBB5_235
.LBB5_230:                              # %if.then16.us.i.i
                                        #   in Loop: Header=BB5_226 Depth=2
	cmpq	$15, %rax
	jne	.LBB5_235
# BB#231:                               # %if.then78.us.i.i
                                        #   in Loop: Header=BB5_226 Depth=2
	movslq	8(%r14), %rax
	movq	h_i_d(%rip), %r8
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	16(%r8,%rax), %eax
	movq	$1, %rsi
	movslq	8(%r13), %rdx
	movq	%rax, %rcx
	shlq	%cl, %rsi
	leaq	(%rdx,%rdx,2), %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movslq	16(%r8,%rdx), %rcx
	movq	output_dependency_cache(%rip), %rdx
.LBB5_232:                              # %if.end111.us.i.i
                                        #   in Loop: Header=BB5_226 Depth=2
	movq	(%rdx,%rcx,8), %rcx
	notq	%rsi
	shrq	$6, %rax
	andq	%rsi, 16(%rcx,%rax,8)
.LBB5_235:                              # %if.end111.us.i.i
                                        #   in Loop: Header=BB5_226 Depth=2
	callq	free_INSN_LIST_node
	movq	$1, %rax
	testq	%rbx, %rbx
	jne	.LBB5_226
	jmp	.LBB5_247
.LBB5_236:                              # %for.body.i.i
                                        #   Parent Loop BB5_225 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	movq	8(%rdi), %rcx
	movq	16(%rdi), %rbx
	cmpq	%r14, %rcx
	jne	.LBB5_224
# BB#237:                               # %if.end.i28.i
                                        #   in Loop: Header=BB5_236 Depth=2
	movq	%rbx, 16(%r15)
	movq	true_dependency_cache(%rip), %rdx
	testq	%rdx, %rdx
	je	.LBB5_245
# BB#238:                               # %if.then16.i.i
                                        #   in Loop: Header=BB5_236 Depth=2
	movzbl	2(%rdi), %eax
	cmpq	$15, %rax
	je	.LBB5_243
# BB#239:                               # %if.then16.i.i
                                        #   in Loop: Header=BB5_236 Depth=2
	cmpq	$14, %rax
	jne	.LBB5_240
# BB#242:                               # %if.then43.i.i
                                        #   in Loop: Header=BB5_236 Depth=2
	movslq	8(%r14), %rax
	movq	h_i_d(%rip), %r8
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	16(%r8,%rax), %eax
	movq	$1, %rsi
	movslq	8(%r13), %rdx
	movq	%rax, %rcx
	shlq	%cl, %rsi
	leaq	(%rdx,%rdx,2), %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movslq	16(%r8,%rdx), %rcx
	movq	anti_dependency_cache(%rip), %rdx
	jmp	.LBB5_244
.LBB5_243:                              # %if.then78.i.i
                                        #   in Loop: Header=BB5_236 Depth=2
	movslq	8(%r14), %rax
	movq	h_i_d(%rip), %r8
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	16(%r8,%rax), %eax
	movq	$1, %rsi
	movslq	8(%r13), %rdx
	movq	%rax, %rcx
	shlq	%cl, %rsi
	leaq	(%rdx,%rdx,2), %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movslq	16(%r8,%rdx), %rcx
	movq	output_dependency_cache(%rip), %rdx
.LBB5_244:                              # %if.end111.i.i
                                        #   in Loop: Header=BB5_236 Depth=2
	movq	(%rdx,%rcx,8), %rcx
	notq	%rsi
	shrq	$6, %rax
	andq	%rsi, 16(%rcx,%rax,8)
	jmp	.LBB5_245
.LBB5_240:                              # %if.then16.i.i
                                        #   in Loop: Header=BB5_236 Depth=2
	testq	%rax, %rax
	jne	.LBB5_245
# BB#241:                               # %if.then18.i.i
                                        #   in Loop: Header=BB5_236 Depth=2
	movslq	8(%r14), %rax
	movq	h_i_d(%rip), %r8
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	16(%r8,%rax), %r10d
	movslq	8(%r13), %r9
	movq	$1, %rsi
	movq	%r10, %rcx
	shlq	%cl, %rsi
	leaq	(%r9,%r9,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movslq	16(%r8,%rax), %rax
	movq	(%rdx,%rax,8), %rax
	notq	%rsi
	shrq	$6, %r10
	andq	%rsi, 16(%rax,%r10,8)
.LBB5_245:                              # %if.end111.i.i
                                        #   in Loop: Header=BB5_236 Depth=2
	callq	free_INSN_LIST_node
	movq	$1, %rax
	testq	%rbx, %rbx
	jne	.LBB5_236
	jmp	.LBB5_247
.LBB5_224:                              # %for.cond.outer.loopexit.i.i
                                        #   in Loop: Header=BB5_225 Depth=1
	testq	%rbx, %rbx
	movq	%rdi, %r15
	jne	.LBB5_225
# BB#246:                               # %for.end.i.i
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB5_294
.LBB5_247:                              # %if.end.i
	movq	48(%r14), %rbx
	jmp	.LBB5_249
	.align	16, 0x90
.LBB5_248:                              # %for.body.i1545
                                        #   in Loop: Header=BB5_249 Depth=1
	movq	8(%rbx), %rsi
	movzbl	2(%rbx), %edx
	movq	%r13, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
.LBB5_249:                              # %for.body.i1545
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB5_248
# BB#250:                               # %set_sched_group_p.exit
	movslq	8(%r13), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movzbl	46(%rdx,%rax), %ecx
	orq	$1, %rcx
	movb	%cl, 46(%rdx,%rax)
	jmp	.LBB5_252
.LBB5_294:                              # %if.then115.i.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.remove_dependence, %rcx
	movq	$433, %rsi              # imm = 0x1B1
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp74:
	.size	sched_analyze_insn, .Ltmp74-sched_analyze_insn
	.cfi_endproc

	.globl	compute_forward_dependences
	.align	16, 0x90
	.type	compute_forward_dependences,@function
compute_forward_dependences:            # @compute_forward_dependences
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp80:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp81:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp82:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp83:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp84:
	.cfi_def_cfa_offset 48
.Ltmp85:
	.cfi_offset %rbx, -40
.Ltmp86:
	.cfi_offset %r12, -32
.Ltmp87:
	.cfi_offset %r14, -24
.Ltmp88:
	.cfi_offset %r15, -16
	movq	24(%rsi), %r15
	movq	%rdi, %rax
	cmpq	%rax, %r15
	je	.LBB6_15
	.align	16, 0x90
.LBB6_1:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_2 Depth 2
                                        #     Child Loop BB6_6 Depth 2
                                        #       Child Loop BB6_8 Depth 3
	movzbl	(%rax), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	movq	%rax, %r14
	jne	.LBB6_14
	.align	16, 0x90
.LBB6_2:                                # %do.body.i
                                        #   Parent Loop BB6_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %r14
	movq	%r14, %rdi
	callq	next_nonnote_insn
	testq	%rax, %rax
	je	.LBB6_5
# BB#3:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB6_2 Depth=2
	movl	(%rax), %ecx
	testq	$268435456, %rcx        # imm = 0x10000000
	je	.LBB6_5
# BB#4:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB6_2 Depth=2
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$36, %rcx
	jne	.LBB6_2
.LBB6_5:                                # %group_leader.exit
                                        #   in Loop: Header=BB6_1 Depth=1
	movq	48(%r14), %r12
	jmp	.LBB6_6
	.align	16, 0x90
.LBB6_13:                               # %for.inc
                                        #   in Loop: Header=BB6_6 Depth=2
	movq	16(%r12), %r12
.LBB6_6:                                # %group_leader.exit
                                        #   Parent Loop BB6_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB6_8 Depth 3
	testq	%r12, %r12
	je	.LBB6_14
# BB#7:                                 # %for.body8
                                        #   in Loop: Header=BB6_6 Depth=2
	movq	8(%r12), %rax
	.align	16, 0x90
.LBB6_8:                                # %do.body.i65
                                        #   Parent Loop BB6_1 Depth=1
                                        #     Parent Loop BB6_6 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	next_nonnote_insn
	testq	%rax, %rax
	je	.LBB6_11
# BB#9:                                 # %land.lhs.true.i72
                                        #   in Loop: Header=BB6_8 Depth=3
	movl	(%rax), %ecx
	testq	$268435456, %rcx        # imm = 0x10000000
	je	.LBB6_11
# BB#10:                                # %land.lhs.true.i72
                                        #   in Loop: Header=BB6_8 Depth=3
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$36, %rcx
	jne	.LBB6_8
.LBB6_11:                               # %group_leader.exit73
                                        #   in Loop: Header=BB6_6 Depth=2
	movq	8(%r12), %rax
	cmpq	%rax, %rbx
	jne	.LBB6_13
# BB#12:                                # %if.end19
                                        #   in Loop: Header=BB6_6 Depth=2
	movslq	8(%rbx), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	(%rdx,%rax), %rsi
	movq	%r14, %rdi
	callq	alloc_INSN_LIST
	movl	(%r12), %edx
	movq	$16, %rcx
	shrq	%cl, %rdx
	movb	%dl, 2(%rax)
	movslq	8(%rbx), %rcx
	movq	h_i_d(%rip), %rdx
	leaq	(%rcx,%rcx,2), %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movq	%rax, (%rdx,%rsi)
	movslq	8(%r14), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	24(%rdx,%rax), %ecx
	incq	%rcx
	movl	%ecx, 24(%rdx,%rax)
	jmp	.LBB6_13
	.align	16, 0x90
.LBB6_14:                               # %for.inc43
                                        #   in Loop: Header=BB6_1 Depth=1
	movq	24(%r14), %rax
	cmpq	%r15, %rax
	jne	.LBB6_1
.LBB6_15:                               # %for.end47
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp89:
	.size	compute_forward_dependences, .Ltmp89-compute_forward_dependences
	.cfi_endproc

	.globl	init_deps
	.align	16, 0x90
	.type	init_deps,@function
init_deps:                              # @init_deps
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp92:
	.cfi_def_cfa_offset 16
.Ltmp93:
	.cfi_offset %rbx, -16
	movl	reload_completed(%rip), %eax
	movq	%rdi, %rbx
	testq	%rax, %rax
	movq	$53, %rdi
	jne	.LBB7_2
# BB#1:                                 # %cond.false
	callq	max_reg_num
	movq	%rax, %rdi
.LBB7_2:                                # %cond.end
	movl	%edi, 68(%rbx)
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rsi
	callq	xcalloc
	movq	%rax, 72(%rbx)
	leaq	80(%rbx), %rdi
	callq	bitmap_initialize
	xorq	%rax, %rax
	movb	%al, 64(%rbx)
	movq	$0, 56(%rbx)
	movq	$0, 48(%rbx)
	movq	$0, 40(%rbx)
	movq	$0, 32(%rbx)
	movq	$0, 24(%rbx)
	movq	$0, 16(%rbx)
	movq	$0, 8(%rbx)
	movq	$0, (%rbx)
	popq	%rbx
	retq
.Ltmp94:
	.size	init_deps, .Ltmp94-init_deps
	.cfi_endproc

	.globl	free_deps
	.align	16, 0x90
	.type	free_deps,@function
free_deps:                              # @free_deps
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
	subq	$16, %rsp
.Ltmp106:
	.cfi_def_cfa_offset 64
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
	movq	%rdi, %rbx
	movq	%rbx, (%rsp)            # 8-byte Spill
                                        # kill: RDI<def> RBX<kill>
	callq	free_INSN_LIST_list
	leaq	8(%rbx), %rdi
	callq	free_EXPR_LIST_list
	leaq	16(%rbx), %rdi
	callq	free_INSN_LIST_list
	leaq	24(%rbx), %rdi
	callq	free_EXPR_LIST_list
	leaq	40(%rbx), %rdi
	callq	free_INSN_LIST_list
	movq	80(%rbx), %rax
	jmp	.LBB8_1
	.align	16, 0x90
.LBB8_10:                               # %if.end27.1
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax), %rax
.LBB8_1:                                # %entry
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_3 Depth 2
                                        #     Child Loop BB8_7 Depth 2
	testq	%rax, %rax
	je	.LBB8_11
# BB#2:                                 # %for.cond6.preheader
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	24(%rax), %r13
	testq	%r13, %r13
	movq	$1, %r14
	je	.LBB8_6
	.align	16, 0x90
.LBB8_3:                                # %for.body13
                                        #   Parent Loop BB8_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%r14), %rbx
	movq	$1, %r12
	movq	%rbx, %rcx
	shlq	%cl, %r12
	testq	%r13, %r12
	je	.LBB8_5
# BB#4:                                 # %if.then15
                                        #   in Loop: Header=BB8_3 Depth=2
	notq	%r12
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	16(%rax), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	addq	%rax, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	movq	(%rsp), %rax            # 8-byte Reload
	movq	72(%rax), %r15
	sarq	%cl, %rbx
	movq	$5, %rcx
	shlq	%cl, %rbx
	leaq	(%r15,%rbx), %rdi
	callq	free_INSN_LIST_list
	leaq	8(%r15,%rbx), %rdi
	callq	free_INSN_LIST_list
	leaq	16(%r15,%rbx), %rdi
	callq	free_INSN_LIST_list
	andq	%r12, %r13
	je	.LBB8_6
.LBB8_5:                                # %for.inc
                                        #   in Loop: Header=BB8_3 Depth=2
	movq	%r14, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	incq	%r14
	cmpq	$64, %rax
	jb	.LBB8_3
.LBB8_6:                                # %if.end27
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	32(%rax), %r13
	xorq	%r15, %r15
	testq	%r13, %r13
	je	.LBB8_10
	.align	16, 0x90
.LBB8_7:                                # %for.body13.1
                                        #   Parent Loop BB8_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %r14
	movq	%r15, %rcx
	shlq	%cl, %r14
	testq	%r13, %r14
	je	.LBB8_9
# BB#8:                                 # %if.then15.1
                                        #   in Loop: Header=BB8_7 Depth=2
	notq	%r14
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	16(%rax), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	64(%r15,%rax), %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	movq	(%rsp), %rax            # 8-byte Reload
	movq	72(%rax), %r12
	sarq	%cl, %rbx
	movq	$5, %rcx
	shlq	%cl, %rbx
	leaq	(%r12,%rbx), %rdi
	callq	free_INSN_LIST_list
	leaq	8(%r12,%rbx), %rdi
	callq	free_INSN_LIST_list
	leaq	16(%r12,%rbx), %rdi
	callq	free_INSN_LIST_list
	andq	%r14, %r13
	je	.LBB8_10
.LBB8_9:                                # %for.inc.1
                                        #   in Loop: Header=BB8_7 Depth=2
	incq	%r15
	movq	%r15, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB8_7
	jmp	.LBB8_10
.LBB8_11:                               # %do.end
	movq	(%rsp), %rbx            # 8-byte Reload
	leaq	80(%rbx), %rdi
	callq	bitmap_clear
	movq	72(%rbx), %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.Ltmp112:
	.size	free_deps, .Ltmp112-free_deps
	.cfi_endproc

	.globl	init_dependency_caches
	.align	16, 0x90
	.type	init_dependency_caches,@function
init_dependency_caches:                 # @init_dependency_caches
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp115:
	.cfi_def_cfa_offset 16
.Ltmp116:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	movslq	n_basic_blocks(%rip), %rsi
	sarq	%cl, %rax
	cqto
	idivq	%rsi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$501, %rax              # imm = 0x1F5
	jl	.LBB9_1
# BB#2:                                 # %if.then
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, true_dependency_cache(%rip)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_zero
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, anti_dependency_cache(%rip)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_zero
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, output_dependency_cache(%rip)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	popq	%rbx
	jmp	sbitmap_vector_zero  # TAILCALL
.LBB9_1:                                # %if.end
	popq	%rbx
	retq
.Ltmp117:
	.size	init_dependency_caches, .Ltmp117-init_dependency_caches
	.cfi_endproc

	.globl	free_dependency_caches
	.align	16, 0x90
	.type	free_dependency_caches,@function
free_dependency_caches:                 # @free_dependency_caches
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp119:
	.cfi_def_cfa_offset 16
	movq	true_dependency_cache(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB10_2
# BB#1:                                 # %if.then
	callq	free
	movq	anti_dependency_cache(%rip), %rdi
	movq	$0, true_dependency_cache(%rip)
	callq	free
	movq	output_dependency_cache(%rip), %rdi
	movq	$0, anti_dependency_cache(%rip)
	callq	free
	movq	$0, output_dependency_cache(%rip)
.LBB10_2:                               # %if.end
	popq	%rax
	retq
.Ltmp120:
	.size	free_dependency_caches, .Ltmp120-free_dependency_caches
	.cfi_endproc

	.globl	init_deps_global
	.align	16, 0x90
	.type	init_deps_global,@function
init_deps_global:                       # @init_deps_global
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp122:
	.cfi_def_cfa_offset 16
	movabsq	$reg_pending_sets_head, %rdi
	callq	bitmap_initialize
	movq	%rax, reg_pending_sets(%rip)
	movabsq	$reg_pending_clobbers_head, %rdi
	callq	bitmap_initialize
	movq	%rax, reg_pending_clobbers(%rip)
	movabsq	$reg_pending_uses_head, %rdi
	callq	bitmap_initialize
	movq	%rax, reg_pending_uses(%rip)
	xorq	%rax, %rax
	movb	%al, reg_pending_barrier(%rip)
	popq	%rax
	retq
.Ltmp123:
	.size	init_deps_global, .Ltmp123-init_deps_global
	.cfi_endproc

	.globl	finish_deps_global
	.align	16, 0x90
	.type	finish_deps_global,@function
finish_deps_global:                     # @finish_deps_global
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp125:
	.cfi_def_cfa_offset 16
	movq	reg_pending_sets(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB12_2
# BB#1:                                 # %if.then
	callq	bitmap_clear
	movq	$0, reg_pending_sets(%rip)
.LBB12_2:                               # %do.body1
	movq	reg_pending_clobbers(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB12_4
# BB#3:                                 # %if.then3
	callq	bitmap_clear
	movq	$0, reg_pending_clobbers(%rip)
.LBB12_4:                               # %do.body6
	movq	reg_pending_uses(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB12_6
# BB#5:                                 # %if.then8
	callq	bitmap_clear
	movq	$0, reg_pending_uses(%rip)
.LBB12_6:                               # %do.end10
	popq	%rax
	retq
.Ltmp126:
	.size	finish_deps_global, .Ltmp126-finish_deps_global
	.cfi_endproc

	.align	16, 0x90
	.type	sched_analyze_2,@function
sched_analyze_2:                        # @sched_analyze_2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp133:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp134:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp135:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp136:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp137:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp138:
	.cfi_def_cfa_offset 80
.Ltmp139:
	.cfi_offset %rbx, -48
.Ltmp140:
	.cfi_offset %r12, -40
.Ltmp141:
	.cfi_offset %r13, -32
.Ltmp142:
	.cfi_offset %r14, -24
.Ltmp143:
	.cfi_offset %r15, -16
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, %r15
	testq	%r15, %r15
	je	.LBB13_72
# BB#1:                                 # %if.end.lr.ph
	leaq	8(%rdi), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	leaq	24(%rdi), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB13_2:                               # %if.end
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_36 Depth 2
                                        #     Child Loop BB13_40 Depth 2
                                        #     Child Loop BB13_44 Depth 2
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$95, %rcx
	jg	.LBB13_5
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB13_2 Depth=1
	leaq	-40(%rcx), %rdx
	andq	%r13, %rdx
	cmpq	$28, %rdx
	ja	.LBB13_61
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB13_2 Depth=1
	jmpq	*.LJTI13_0(,%rdx,8)
.LBB13_33:                              # %sw.bb112
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	current_sched_info(%rip), %rax
	movzbl	96(%rax), %eax
	testq	$2, %rax
	movq	%r15, %r12
	je	.LBB13_35
# BB#34:                                # %if.then117
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	%rdi, %rbx
	movq	%r15, %rdi
	callq	shallow_copy_rtx
	movl	target_flags(%rip), %esi
	movq	%rax, %r12
	movq	8(%r12), %rdi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$1, %rdx
	callq	cselib_lookup
	movq	8(%r12), %rdi
	callq	cselib_subst_to_values
	movq	%rbx, %rdi
	movq	%rax, 8(%r12)
.LBB13_35:                              # %if.end133
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	%r15, 16(%rsp)          # 8-byte Spill
	movq	(%rdi), %r14
	movq	%rdi, %r15
	testq	%r14, %r14
	movq	(%rsp), %rbx            # 8-byte Reload
	je	.LBB13_39
	.align	16, 0x90
.LBB13_36:                              # %while.body136
                                        #   Parent Loop BB13_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rbx), %rbx
	movq	8(%rbx), %rdi
	movq	%r12, %rsi
	callq	read_dependence
	testq	%r13, %rax
	je	.LBB13_38
# BB#37:                                # %if.then142
                                        #   in Loop: Header=BB13_36 Depth=2
	movq	8(%r14), %rsi
	movq	$14, %rdx
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	add_dependence
.LBB13_38:                              # %if.end146
                                        #   in Loop: Header=BB13_36 Depth=2
	movq	16(%r14), %r14
	addq	$16, %rbx
	testq	%r14, %r14
	jne	.LBB13_36
.LBB13_39:                              # %while.end153
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	16(%r15), %rbx
	testq	%rbx, %rbx
	movq	8(%rsp), %r14           # 8-byte Reload
	je	.LBB13_43
	.align	16, 0x90
.LBB13_40:                              # %while.body156
                                        #   Parent Loop BB13_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r14), %r14
	movq	8(%r14), %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movabsq	$rtx_varies_p, %rcx
	callq	true_dependence
	testq	%r13, %rax
	je	.LBB13_42
# BB#41:                                # %if.then162
                                        #   in Loop: Header=BB13_40 Depth=2
	movq	8(%rbx), %rsi
	xorq	%rdx, %rdx
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	add_dependence
.LBB13_42:                              # %if.end166
                                        #   in Loop: Header=BB13_40 Depth=2
	movq	16(%rbx), %rbx
	addq	$16, %r14
	testq	%rbx, %rbx
	jne	.LBB13_40
.LBB13_43:                              # %while.end173
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	40(%r15), %rbx
	testq	%rbx, %rbx
	movq	24(%rsp), %r14          # 8-byte Reload
	je	.LBB13_53
	.align	16, 0x90
.LBB13_44:                              # %for.body
                                        #   Parent Loop BB13_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx), %rsi
	movzbl	(%rsi), %eax
	cmpq	$33, %rax
	jne	.LBB13_51
# BB#45:                                # %lor.lhs.false182
                                        #   in Loop: Header=BB13_44 Depth=2
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdi
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB13_49
# BB#46:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB13_44 Depth=2
	movl	8(%rdi), %eax
	cmpq	$53, %rax
	jb	.LBB13_49
# BB#47:                                # %land.lhs.true4.i
                                        #   in Loop: Header=BB13_44 Depth=2
	movq	reg_known_value(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	testq	%rax, %rax
	je	.LBB13_49
# BB#48:                                # %land.lhs.true4.i
                                        #   in Loop: Header=BB13_44 Depth=2
	movq	%rax, %rdi
	.align	16, 0x90
.LBB13_49:                              # %deps_may_trap_p.exit
                                        #   in Loop: Header=BB13_44 Depth=2
	callq	rtx_addr_can_trap_p
	testq	%r13, %rax
	je	.LBB13_52
# BB#50:                                # %deps_may_trap_p.exit.if.then185_crit_edge
                                        #   in Loop: Header=BB13_44 Depth=2
	movq	8(%rbx), %rsi
.LBB13_51:                              # %if.then185
                                        #   in Loop: Header=BB13_44 Depth=2
	movq	$14, %rdx
	movq	%r14, %rdi
	callq	add_dependence
.LBB13_52:                              # %for.inc
                                        #   in Loop: Header=BB13_44 Depth=2
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB13_44
.LBB13_53:                              # %for.end
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	(%r15), %rsi
	movq	%r14, %rdi
	callq	alloc_INSN_LIST
	movq	%r15, %rcx
	movq	%rax, (%rcx)
	movq	current_sched_info(%rip), %rax
	movzbl	96(%rax), %eax
	testq	$2, %rax
	movq	16(%rsp), %r15          # 8-byte Reload
	movq	%r15, %r12
	je	.LBB13_55
# BB#54:                                # %if.then.i
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	%rcx, %rbx
	movq	%r15, %rdi
	callq	shallow_copy_rtx
	movq	%rax, %r12
	movq	8(%r12), %rdi
	callq	cselib_subst_to_values
	movq	%rbx, %rcx
	movq	%rax, 8(%r12)
.LBB13_55:                              # %add_insn_mem_dependence.exit
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	8(%rcx), %rdx
	movq	%rcx, %rbx
	xorq	%rdi, %rdi
	movq	%r12, %rsi
	callq	alloc_EXPR_LIST
	movq	%rbx, %rdi
	movq	%rax, 8(%rdi)
	movl	32(%rdi), %eax
	incq	%rax
	movl	%eax, 32(%rdi)
	movq	8(%r15), %r15
	testq	%r15, %r15
	jne	.LBB13_2
	jmp	.LBB13_72
.LBB13_5:                               # %if.end
	leaq	-96(%rcx), %rax
	andq	%r13, %rax
	cmpq	$4, %rax
	jae	.LBB13_6
# BB#73:                                # %sw.bb226
	movq	8(%r15), %rsi
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	movq	24(%rsp), %r14          # 8-byte Reload
	jmp	.LBB13_74
.LBB13_6:                               # %if.end
	leaq	-100(%rcx), %rax
	andq	%r13, %rax
	cmpq	$2, %rax
	jae	.LBB13_61
# BB#7:                                 # %sw.bb230
	movq	8(%r15), %rsi
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdx
	callq	sched_analyze_2
	movq	16(%r15), %rsi
	movq	%rbx, %rdi
.LBB13_74:                              # %sw.bb226
	movq	%r14, %rdx
	callq	sched_analyze_2
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	sched_analyze_1  # TAILCALL
.LBB13_68:                              # %if.end208
	movq	$1, %rax
	movb	%al, reg_pending_barrier(%rip)
	cmpq	$41, %rcx
	jne	.LBB13_61
	jmp	.LBB13_69
.LBB13_66:                              # %lor.lhs.false202
	testq	$134217728, %rax        # imm = 0x8000000
	je	.LBB13_69
# BB#67:                                # %if.end208.thread430
	movq	$1, %rax
	movb	%al, reg_pending_barrier(%rip)
.LBB13_69:                              # %for.cond212.preheader
	movq	32(%r15), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB13_72
# BB#70:
	movq	$1, %rbx
.LBB13_71:                              # %for.body217
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %rsi
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	sched_analyze_2
	movq	%r14, %rdi
	movq	32(%r15), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB13_71
	jmp	.LBB13_72
.LBB13_56:                              # %sw.bb198
	movq	%r15, %r13
	movq	16(%rdi), %r12
	testq	%r12, %r12
	movq	$0, 16(%rdi)
	movq	%rdi, %r14
	movq	24(%rsp), %r15          # 8-byte Reload
	je	.LBB13_58
.LBB13_57:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r12), %rsi
	movq	16(%r12), %rbx
	movq	$14, %rdx
	movq	%r15, %rdi
	callq	add_dependence
	movq	%r12, %rdi
	callq	free_INSN_LIST_node
	testq	%rbx, %rbx
	movq	%rbx, %r12
	jne	.LBB13_57
.LBB13_58:                              # %add_dependence_list_and_free.exit
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	free_EXPR_LIST_list
	xorq	%rax, %rax
	movl	%eax, 32(%r14)
	movq	40(%r14), %r12
	testq	%r12, %r12
	movq	$0, 40(%r14)
	je	.LBB13_60
.LBB13_59:                              # %for.body.i427
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r12), %rsi
	movq	16(%r12), %rbx
	movq	$14, %rdx
	movq	%r15, %rdi
	callq	add_dependence
	movq	%r12, %rdi
	callq	free_INSN_LIST_node
	testq	%rbx, %rbx
	movq	%rbx, %r12
	jne	.LBB13_59
.LBB13_60:                              # %add_dependence_list_and_free.exit428
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	alloc_INSN_LIST
	movq	%r14, %rdi
	movq	%rax, 40(%rdi)
	movq	$1, %rax
	movl	%eax, 36(%rdi)
	movq	$52, %rcx
	movq	%r13, %r15
.LBB13_61:                              # %sw.epilog
	movzbl	rtx_length(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB13_72
# BB#62:                                # %for.body246.lr.ph
	movq	rtx_format(,%rcx,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB13_63:                              # %for.body246
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_77 Depth 2
	movq	%rdx, %r14
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	-1(%rax,%r14), %eax
	leaq	-1(%r14), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB13_64
# BB#76:                                # %for.cond264.preheader
                                        #   in Loop: Header=BB13_63 Depth=1
	movq	(%r15,%r14,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %rbx
	movq	24(%rsp), %r13          # 8-byte Reload
	jle	.LBB13_65
	.align	16, 0x90
.LBB13_77:                              # %for.body272
                                        #   Parent Loop BB13_63 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%rbx,8), %rsi
	movq	%r15, %r12
	movq	%rdi, %r15
                                        # kill: RDI<def> R15<kill>
	movq	%r13, %rdx
	callq	sched_analyze_2
	movq	%r15, %rdi
	movq	%r12, %r15
	movq	(%r15,%r14,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB13_77
	jmp	.LBB13_65
	.align	16, 0x90
.LBB13_64:                              # %for.body246
                                        #   in Loop: Header=BB13_63 Depth=1
	cmpq	$101, %rax
	movq	24(%rsp), %r13          # 8-byte Reload
	jne	.LBB13_65
# BB#75:                                # %if.then252
                                        #   in Loop: Header=BB13_63 Depth=1
	movq	(%r15,%r14,8), %rsi
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	movq	%r13, %rdx
	callq	sched_analyze_2
	movq	%rbx, %rdi
.LBB13_65:                              # %for.cond243.backedge
                                        #   in Loop: Header=BB13_63 Depth=1
	movq	%r13, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_63
	jmp	.LBB13_72
.LBB13_8:                               # %sw.bb1
	movslq	8(%r15), %r12
	cmpq	$52, %r12
	jg	.LBB13_24
# BB#9:                                 # %if.then3
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%r12, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB13_13
# BB#10:                                # %if.then3
	leaq	-21(%r12), %rcx
	andq	%r13, %rcx
	cmpq	$8, %rcx
	jb	.LBB13_13
# BB#11:                                # %if.then3
	leaq	-45(%r12), %rcx
	andq	%r13, %rcx
	cmpq	$8, %rcx
	jb	.LBB13_13
# BB#12:                                # %if.then3
	leaq	-29(%r12), %rcx
	andq	%r13, %rcx
	cmpq	$7, %rcx
	jbe	.LBB13_13
# BB#18:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB13_19
# BB#21:                                # %cond.true39
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB13_23
.LBB13_24:                              # %if.else
	movslq	68(%rdi), %rax
	cmpq	%rax, %r12
	jge	.LBB13_25
# BB#27:                                # %if.else82
	movq	%rdi, %rbx
	movq	reg_pending_uses(%rip), %rdi
	movq	%r12, %rsi
	callq	bitmap_set_bit
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	jne	.LBB13_31
# BB#28:                                # %land.lhs.true84
	movq	reg_known_equiv_p(%rip), %rax
	movzbl	(%rax,%r12), %eax
	testq	%rax, %rax
	je	.LBB13_31
# BB#29:                                # %land.lhs.true89
	movq	reg_known_value(%rip), %rax
	movq	(%rax,%r12,8), %rax
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB13_31
# BB#30:                                # %if.then96
	movq	8(%rax), %rsi
	movq	%rbx, %rdi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	sched_analyze_2
.LBB13_31:                              # %if.end102
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r12,8), %rax
	movl	32(%rax), %eax
	testq	%rax, %rax
	jne	.LBB13_72
# BB#32:                                # %if.then107
	movq	56(%rbx), %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	alloc_INSN_LIST
	movq	%rax, 56(%rbx)
	jmp	.LBB13_72
.LBB13_25:                              # %if.then65
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$48, %rax
	je	.LBB13_72
# BB#26:                                # %if.then80
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.sched_analyze_2, %rcx
	movq	$758, %rsi              # imm = 0x2F6
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB13_13:                              # %cond.true
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB13_23
# BB#14:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB13_15
# BB#16:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB13_23
.LBB13_19:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB13_22
# BB#20:                                # %cond.true32
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB13_23
.LBB13_15:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB13_23
.LBB13_22:                              # %while.cond.preheader
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
	jle	.LBB13_72
.LBB13_23:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	leaq	-1(%rax), %r14
	movq	$32, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	reg_pending_uses(%rip), %rdi
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	-1(%rax,%r12), %rsi
	callq	bitmap_set_bit
	testq	%rbx, %rbx
	movq	%r14, %rax
	jg	.LBB13_23
.LBB13_72:                              # %for.end287
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp144:
	.size	sched_analyze_2, .Ltmp144-sched_analyze_2
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI13_0:
	.quad	.LBB13_68
	.quad	.LBB13_66
	.quad	.LBB13_61
	.quad	.LBB13_68
	.quad	.LBB13_61
	.quad	.LBB13_61
	.quad	.LBB13_61
	.quad	.LBB13_61
	.quad	.LBB13_61
	.quad	.LBB13_61
	.quad	.LBB13_61
	.quad	.LBB13_61
	.quad	.LBB13_56
	.quad	.LBB13_61
	.quad	.LBB13_72
	.quad	.LBB13_72
	.quad	.LBB13_72
	.quad	.LBB13_61
	.quad	.LBB13_72
	.quad	.LBB13_61
	.quad	.LBB13_61
	.quad	.LBB13_8
	.quad	.LBB13_61
	.quad	.LBB13_61
	.quad	.LBB13_61
	.quad	.LBB13_61
	.quad	.LBB13_33
	.quad	.LBB13_72
	.quad	.LBB13_72

	.text
	.align	16, 0x90
	.type	sched_analyze_1,@function
sched_analyze_1:                        # @sched_analyze_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp151:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp152:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp153:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp154:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp155:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp156:
	.cfi_def_cfa_offset 64
.Ltmp157:
	.cfi_offset %rbx, -48
.Ltmp158:
	.cfi_offset %r12, -40
.Ltmp159:
	.cfi_offset %r13, -32
.Ltmp160:
	.cfi_offset %r14, -24
.Ltmp161:
	.cfi_offset %r15, -16
	movq	8(%rsi), %r15
	movq	%rdx, %r13
	testq	%r15, %r15
	je	.LBB14_84
# BB#1:                                 # %if.end
	movq	%rdi, %r12
	movl	(%r15), %eax
	movl	(%rsi), %edx
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$39, %rcx
	jne	.LBB14_2
# BB#17:                                # %if.then4
	movq	%rdx, %rcx
	movq	8(%r15), %rax
	movslq	(%rax), %rbx
	testq	%rbx, %rbx
	jle	.LBB14_24
# BB#18:                                # %for.body.lr.ph
	movq	%rsi, (%rsp)            # 8-byte Spill
	decq	%rbx
	movq	%r12, %r14
	jmp	.LBB14_19
	.align	16, 0x90
.LBB14_21:                              # %for.cond.backedge.for.body_crit_edge
                                        #   in Loop: Header=BB14_19 Depth=1
	movq	8(%r15), %rax
	decq	%rbx
.LBB14_19:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax,%rbx,8), %rax
	movq	8(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB14_20
# BB#22:                                # %if.then16
                                        #   in Loop: Header=BB14_19 Depth=1
	movq	$49, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	%r13, %rdx
	callq	sched_analyze_1
.LBB14_20:                              # %for.cond.backedge
                                        #   in Loop: Header=BB14_19 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB14_21
# BB#23:                                # %for.cond.for.end_crit_edge
	movq	%r14, %r12
	movq	(%rsp), %rsi            # 8-byte Reload
	movl	(%rsi), %ecx
.LBB14_24:                              # %for.end
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$47, %rcx
	movq	%r12, %rdi
	jne	.LBB14_84
# BB#25:                                # %if.then30
	movq	16(%rsi), %rsi
	jmp	.LBB14_26
.LBB14_2:
	movq	%rsi, (%rsp)            # 8-byte Spill
	andq	$65535, %rdx            # imm = 0xFFFF
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB14_3
	.align	16, 0x90
.LBB14_30:                              # %if.end63
                                        #   in Loop: Header=BB14_3 Depth=1
	movq	8(%r15), %r15
	movl	(%r15), %eax
.LBB14_3:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$131, %rcx
	jg	.LBB14_27
# BB#4:                                 # %while.cond
                                        #   in Loop: Header=BB14_3 Depth=1
	leaq	-63(%rcx), %rdx
	andq	%rbx, %rdx
	cmpq	$2, %rdx
	jb	.LBB14_28
	jmp	.LBB14_5
	.align	16, 0x90
.LBB14_27:                              # %while.cond
                                        #   in Loop: Header=BB14_3 Depth=1
	addq	$-132, %rcx
	andq	%rbx, %rcx
	cmpq	$1, %rcx
	ja	.LBB14_82
.LBB14_28:                              # %while.body
                                        #   in Loop: Header=BB14_3 Depth=1
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$132, %rax
	jne	.LBB14_30
# BB#29:                                # %if.then56
                                        #   in Loop: Header=BB14_3 Depth=1
	movq	16(%r15), %rsi
	movq	%rbx, %r14
	movq	%r12, %rbx
	movq	%rbx, %rdi
	movq	%r13, %rdx
	callq	sched_analyze_2
	movq	24(%r15), %rsi
	movq	%rbx, %rdi
	movq	%rbx, %r12
	movq	%r14, %rbx
	movq	%r13, %rdx
	callq	sched_analyze_2
	jmp	.LBB14_30
.LBB14_5:                               # %while.cond
	cmpq	$61, %rcx
	jne	.LBB14_6
# BB#31:                                # %if.then70
	movslq	8(%r15), %r15
	cmpq	$52, %r15
	jg	.LBB14_51
# BB#32:                                # %if.then74
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%r15, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB14_36
# BB#33:                                # %if.then74
	leaq	-21(%r15), %rcx
	andq	%rbx, %rcx
	cmpq	$8, %rcx
	jb	.LBB14_36
# BB#34:                                # %if.then74
	leaq	-45(%r15), %rcx
	andq	%rbx, %rcx
	cmpq	$8, %rcx
	jb	.LBB14_36
# BB#35:                                # %if.then74
	leaq	-29(%r15), %rcx
	andq	%rbx, %rcx
	cmpq	$7, %rcx
	jbe	.LBB14_36
# BB#41:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB14_42
# BB#44:                                # %cond.true114
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	jmp	.LBB14_46
.LBB14_6:                               # %while.cond
	cmpq	$66, %rcx
	jne	.LBB14_82
# BB#7:                                 # %if.then214
	movq	current_sched_info(%rip), %rax
	movzbl	96(%rax), %eax
	testq	$2, %rax
	movq	%r15, 8(%rsp)           # 8-byte Spill
	je	.LBB14_9
# BB#8:                                 # %if.then219
	movq	%r15, %rdi
	callq	shallow_copy_rtx
	movl	target_flags(%rip), %esi
	movq	%rax, %rbx
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	8(%rbx), %rdi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$1, %rdx
	callq	cselib_lookup
	movq	8(%rbx), %rdi
	callq	cselib_subst_to_values
	movq	%rax, 8(%rbx)
.LBB14_9:                               # %if.end235
	movq	compiler_params(%rip), %rax
	movslq	32(%r12), %rcx
	movslq	80(%rax), %rax
	movq	(%r12), %r14
	cmpq	%rax, %rcx
	jle	.LBB14_65
# BB#10:                                # %if.then239
	movq	$0, (%r12)
	testq	%r14, %r14
	je	.LBB14_12
	.align	16, 0x90
.LBB14_11:                              # %for.body.i423
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r14), %rsi
	movq	16(%r14), %rbx
	movq	$14, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	%r14, %rdi
	callq	free_INSN_LIST_node
	testq	%rbx, %rbx
	movq	%rbx, %r14
	jne	.LBB14_11
.LBB14_12:                              # %add_dependence_list_and_free.exit
	leaq	8(%r12), %rdi
	callq	free_EXPR_LIST_list
	movq	16(%r12), %r14
	testq	%r14, %r14
	movq	$0, 16(%r12)
	je	.LBB14_14
	.align	16, 0x90
.LBB14_13:                              # %for.body.i431
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r14), %rsi
	movq	16(%r14), %rbx
	movq	$15, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	%r14, %rdi
	callq	free_INSN_LIST_node
	testq	%rbx, %rbx
	movq	%rbx, %r14
	jne	.LBB14_13
.LBB14_14:                              # %add_dependence_list_and_free.exit432
	leaq	24(%r12), %rdi
	callq	free_EXPR_LIST_list
	xorq	%rax, %rax
	movl	%eax, 32(%r12)
	movq	40(%r12), %r14
	testq	%r14, %r14
	movq	$0, 40(%r12)
	je	.LBB14_16
	.align	16, 0x90
.LBB14_15:                              # %for.body.i440
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r14), %rsi
	movq	16(%r14), %rbx
	movq	$15, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	%r14, %rdi
	callq	free_INSN_LIST_node
	testq	%rbx, %rbx
	movq	%rbx, %r14
	jne	.LBB14_15
.LBB14_16:                              # %add_dependence_list_and_free.exit441
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	alloc_INSN_LIST
	movq	%r12, %rbx
	movq	%rax, 40(%rbx)
	movq	$1, %rax
	movl	%eax, 36(%rbx)
	jmp	.LBB14_81
.LBB14_51:                              # %if.else154
	movslq	68(%r12), %rax
	cmpq	%rax, %r15
	movq	8(%rsp), %rax           # 8-byte Reload
	jge	.LBB14_52
# BB#54:                                # %if.else175
	cmpq	$47, %rax
	movq	%r13, %rbx
	jne	.LBB14_56
# BB#55:                                # %if.then178
	movq	reg_pending_sets(%rip), %rdi
	jmp	.LBB14_57
.LBB14_52:                              # %if.then157
	movq	32(%r13), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$48, %rax
	je	.LBB14_82
# BB#53:                                # %if.then173
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.sched_analyze_1, %rcx
	movq	$620, %rsi              # imm = 0x26C
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB14_65:                              # %if.else240
	testq	%r14, %r14
	je	.LBB14_70
# BB#66:                                # %while.body243.lr.ph
	leaq	8(%r12), %rbx
	.align	16, 0x90
.LBB14_67:                              # %while.body243
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rbx
	movq	8(%rbx), %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	anti_dependence
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_69
# BB#68:                                # %if.then249
                                        #   in Loop: Header=BB14_67 Depth=1
	movq	8(%r14), %rsi
	movq	$14, %rdx
	movq	%r13, %rdi
	callq	add_dependence
.LBB14_69:                              # %if.end253
                                        #   in Loop: Header=BB14_67 Depth=1
	movq	16(%r14), %r14
	addq	$16, %rbx
	testq	%r14, %r14
	jne	.LBB14_67
.LBB14_70:                              # %while.end260
	movq	16(%r12), %r14
	testq	%r14, %r14
	je	.LBB14_75
# BB#71:
	leaq	24(%r12), %rbx
	.align	16, 0x90
.LBB14_72:                              # %while.body263
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rbx
	movq	8(%rbx), %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	output_dependence
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_74
# BB#73:                                # %if.then269
                                        #   in Loop: Header=BB14_72 Depth=1
	movq	8(%r14), %rsi
	movq	$15, %rdx
	movq	%r13, %rdi
	callq	add_dependence
.LBB14_74:                              # %if.end273
                                        #   in Loop: Header=BB14_72 Depth=1
	movq	16(%r14), %r14
	addq	$16, %rbx
	testq	%r14, %r14
	jne	.LBB14_72
.LBB14_75:                              # %while.end280
	movq	40(%r12), %rbx
	jmp	.LBB14_77
	.align	16, 0x90
.LBB14_76:                              # %for.body.i412
                                        #   in Loop: Header=BB14_77 Depth=1
	movq	8(%rbx), %rsi
	movq	$14, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
.LBB14_77:                              # %for.body.i412
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB14_76
# BB#78:                                # %add_dependence_list.exit413
	movq	16(%r12), %rsi
	movq	%r13, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, 16(%r12)
	movq	current_sched_info(%rip), %rax
	movzbl	96(%rax), %eax
	testq	$2, %rax
	movq	%r15, %r14
	je	.LBB14_80
# BB#79:                                # %if.then.i
	movq	%r15, %rdi
	callq	shallow_copy_rtx
	movq	%rax, %r14
	movq	8(%r14), %rdi
	callq	cselib_subst_to_values
	movq	%rax, 8(%r14)
.LBB14_80:                              # %add_insn_mem_dependence.exit
	movq	24(%r12), %rdx
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	movq	%r12, %rbx
	callq	alloc_EXPR_LIST
	movq	%rax, 24(%rbx)
	movl	32(%rbx), %eax
	incq	%rax
	movl	%eax, 32(%rbx)
.LBB14_81:                              # %if.end283
	movq	8(%r15), %rsi
	movq	%rbx, %rdi
	movq	%rbx, %r12
	movq	%r13, %rdx
	callq	sched_analyze_2
	jmp	.LBB14_82
.LBB14_36:                              # %cond.true
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	je	.LBB14_46
# BB#37:                                # %lor.rhs95
	cmpq	$6, %rcx
	jne	.LBB14_38
# BB#39:                                # %lor.rhs95
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB14_46
.LBB14_56:                              # %if.else179
	movq	reg_pending_clobbers(%rip), %rdi
.LBB14_57:                              # %if.end180
	movq	%r15, %rsi
	callq	bitmap_set_bit
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	jne	.LBB14_61
# BB#58:                                # %land.lhs.true182
	movq	reg_known_equiv_p(%rip), %rax
	movzbl	(%rax,%r15), %eax
	testq	%rax, %rax
	je	.LBB14_61
# BB#59:                                # %land.lhs.true187
	movq	reg_known_value(%rip), %rax
	movq	(%rax,%r15,8), %rax
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB14_61
# BB#60:                                # %if.then194
	movq	8(%rax), %rsi
	movq	%r12, %rdi
	movq	%rbx, %rdx
	callq	sched_analyze_2
.LBB14_61:                              # %if.end200
	movq	%rbx, %r13
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r15,8), %rax
	movl	32(%rax), %eax
	testq	%rax, %rax
	jne	.LBB14_82
# BB#62:                                # %if.then205
	movq	48(%r12), %rbx
	jmp	.LBB14_63
.LBB14_64:                              # %for.body.i
                                        #   in Loop: Header=BB14_63 Depth=1
	movq	8(%rbx), %rsi
	movq	$14, %rdx
	movq	%r13, %rdi
	callq	add_dependence
	movq	16(%rbx), %rbx
.LBB14_63:                              # %if.then205
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB14_64
	jmp	.LBB14_82
.LBB14_42:                              # %cond.false
	cmpq	$18, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	jne	.LBB14_45
# BB#43:                                # %cond.true107
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB14_46
.LBB14_45:                              # %cond.false118
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	jmp	.LBB14_46
.LBB14_38:
	movabsq	$0, %rax
	incq	%rax
.LBB14_46:                              # %cond.end134
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$47, %rdi
	jne	.LBB14_47
# BB#49:                                # %while.cond139.preheader
	testq	%rdx, %rdx
	jle	.LBB14_82
	.align	16, 0x90
.LBB14_50:                              # %while.body143
                                        # =>This Inner Loop Header: Depth=1
	leaq	-1(%rax), %r14
	movq	$32, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	reg_pending_sets(%rip), %rdi
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	-1(%rax,%r15), %rsi
	callq	bitmap_set_bit
	testq	%rbx, %rbx
	movq	%r14, %rax
	jg	.LBB14_50
	jmp	.LBB14_82
.LBB14_47:                              # %while.cond146.preheader
	testq	%rdx, %rdx
	jle	.LBB14_82
	.align	16, 0x90
.LBB14_48:                              # %while.body150
                                        # =>This Inner Loop Header: Depth=1
	leaq	-1(%rax), %r14
	movq	$32, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	reg_pending_clobbers(%rip), %rdi
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	-1(%rax,%r15), %rsi
	callq	bitmap_set_bit
	testq	%rbx, %rbx
	movq	%r14, %rax
	jg	.LBB14_48
.LBB14_82:                              # %if.end288
	movq	(%rsp), %rcx            # 8-byte Reload
	movzbl	(%rcx), %eax
	cmpq	$47, %rax
	movq	%r12, %rdi
	jne	.LBB14_84
# BB#83:                                # %if.then293
	movq	16(%rcx), %rsi
.LBB14_26:                              # %if.then30
	movq	%r13, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	sched_analyze_2  # TAILCALL
.LBB14_84:                              # %if.end297
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp162:
	.size	sched_analyze_1, .Ltmp162-sched_analyze_1
	.cfi_endproc

	.type	true_dependency_cache,@object # @true_dependency_cache
	.local	true_dependency_cache
	.comm	true_dependency_cache,8,8
	.type	anti_dependency_cache,@object # @anti_dependency_cache
	.local	anti_dependency_cache
	.comm	anti_dependency_cache,8,8
	.type	output_dependency_cache,@object # @output_dependency_cache
	.local	output_dependency_cache
	.comm	output_dependency_cache,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"sched-deps.c"
	.size	.L.str, 13

	.type	.L__FUNCTION__.add_dependence,@object # @__FUNCTION__.add_dependence
.L__FUNCTION__.add_dependence:
	.asciz	"add_dependence"
	.size	.L__FUNCTION__.add_dependence, 15

	.type	reg_pending_barrier,@object # @reg_pending_barrier
	.local	reg_pending_barrier
	.comm	reg_pending_barrier,1,1
	.type	reg_pending_sets,@object # @reg_pending_sets
	.local	reg_pending_sets
	.comm	reg_pending_sets,8,8
	.type	reg_pending_uses,@object # @reg_pending_uses
	.local	reg_pending_uses
	.comm	reg_pending_uses,8,8
	.type	reg_pending_clobbers,@object # @reg_pending_clobbers
	.local	reg_pending_clobbers
	.comm	reg_pending_clobbers,8,8
	.type	reg_pending_sets_head,@object # @reg_pending_sets_head
	.local	reg_pending_sets_head
	.comm	reg_pending_sets_head,24,8
	.type	reg_pending_clobbers_head,@object # @reg_pending_clobbers_head
	.local	reg_pending_clobbers_head
	.comm	reg_pending_clobbers_head,24,8
	.type	reg_pending_uses_head,@object # @reg_pending_uses_head
	.local	reg_pending_uses_head
	.comm	reg_pending_uses_head,24,8
	.type	.L__FUNCTION__.remove_dependence,@object # @__FUNCTION__.remove_dependence
.L__FUNCTION__.remove_dependence:
	.asciz	"remove_dependence"
	.size	.L__FUNCTION__.remove_dependence, 18

	.type	.L__FUNCTION__.sched_analyze_1,@object # @__FUNCTION__.sched_analyze_1
.L__FUNCTION__.sched_analyze_1:
	.asciz	"sched_analyze_1"
	.size	.L__FUNCTION__.sched_analyze_1, 16

	.type	.L__FUNCTION__.sched_analyze_2,@object # @__FUNCTION__.sched_analyze_2
.L__FUNCTION__.sched_analyze_2:
	.asciz	"sched_analyze_2"
	.size	.L__FUNCTION__.sched_analyze_2, 16


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
