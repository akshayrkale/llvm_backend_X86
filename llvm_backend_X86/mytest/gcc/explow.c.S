	.file	"explow.c.bc"
	.text
	.globl	trunc_int_for_mode
	.align	16, 0x90
	.type	trunc_int_for_mode,@function
trunc_int_for_mode:                     # @trunc_int_for_mode
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rsi, %rdx
	andq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$1, %rdx
	jne	.LBB0_2
# BB#1:                                 # %if.then
	andq	$1, %rdi
	movq	%rdi, %rax
	retq
.LBB0_2:                                # %if.end
	movzbl	mode_bitsize(%rsi,%rsi), %ecx
	testq	%rcx, %rcx
	je	.LBB0_5
# BB#3:                                 # %if.end
	cmpq	$63, %rcx
	ja	.LBB0_5
# BB#4:                                 # %if.then7
	addq	%rax, %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	leaq	-1(%rax,%rax), %rcx
	andq	%rdi, %rcx
	xorq	%rax, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rdi
.LBB0_5:                                # %return
	movq	%rdi, %rax
	retq
.Ltmp0:
	.size	trunc_int_for_mode, .Ltmp0-trunc_int_for_mode
	.cfi_endproc

	.globl	plus_constant_wide
	.align	16, 0x90
	.type	plus_constant_wide,@function
plus_constant_wide:                     # @plus_constant_wide
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
	subq	$32, %rsp
.Ltmp12:
	.cfi_def_cfa_offset 80
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
	movq	%rsi, %r15
	movq	%rdi, %r14
	testq	%r15, %r15
	je	.LBB1_1
# BB#2:
	xorq	%r13, %r13
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	jmp	.LBB1_3
	.align	16, 0x90
.LBB1_21:                               # %if.end78
                                        #   in Loop: Header=BB1_3 Depth=1
	movq	8(%r14), %r14
	movq	%rax, %r15
.LBB1_3:                                # %restart
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r14), %r12d
	movq	%r12, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$74, %rax
	movq	%r14, 24(%rsp)
	jg	.LBB1_13
# BB#4:                                 # %restart
                                        #   in Loop: Header=BB1_3 Depth=1
	addq	$-54, %rax
	andq	%rsi, %rax
	cmpq	$14, %rax
	ja	.LBB1_32
# BB#5:                                 # %restart
                                        #   in Loop: Header=BB1_3 Depth=1
	jmpq	*.LJTI1_0(,%rax,8)
.LBB1_11:                               # %sw.bb47
                                        #   in Loop: Header=BB1_3 Depth=1
	movq	8(%r14), %r14
	movq	$1, %r13
	jmp	.LBB1_3
	.align	16, 0x90
.LBB1_13:                               # %restart
                                        #   in Loop: Header=BB1_3 Depth=1
	cmpq	$75, %rax
	jne	.LBB1_32
# BB#14:                                # %sw.bb52
                                        #   in Loop: Header=BB1_3 Depth=1
	movq	16(%r14), %rdi
	movzbl	(%rdi), %eax
	cmpq	$54, %rax
	jne	.LBB1_22
# BB#15:                                # %if.then60
                                        #   in Loop: Header=BB1_3 Depth=1
	movq	8(%rdi), %rax
	movzbl	2(%r14), %ecx
	addq	%r15, %rax
	testq	%rcx, %rcx
	je	.LBB1_21
# BB#16:                                # %if.then73
                                        #   in Loop: Header=BB1_3 Depth=1
	cmpq	$1, %rcx
	jne	.LBB1_18
# BB#17:                                # %if.then.i
                                        #   in Loop: Header=BB1_3 Depth=1
	andq	$1, %rax
	jmp	.LBB1_21
.LBB1_18:                               # %if.end.i
                                        #   in Loop: Header=BB1_3 Depth=1
	movzbl	mode_bitsize(%rcx,%rcx), %ecx
	testq	%rcx, %rcx
	je	.LBB1_21
# BB#19:                                # %if.end.i
                                        #   in Loop: Header=BB1_3 Depth=1
	cmpq	$63, %rcx
	ja	.LBB1_21
# BB#20:                                # %if.then7.i
                                        #   in Loop: Header=BB1_3 Depth=1
	addq	%rsi, %rcx
	movq	$1, %rdx
	shlq	%cl, %rdx
	leaq	-1(%rdx,%rdx), %rcx
	andq	%rax, %rcx
	xorq	%rdx, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rax
	jmp	.LBB1_21
.LBB1_1:
	movq	%r14, %rbx
	jmp	.LBB1_42
.LBB1_12:                               # %sw.bb51
	movq	$16, %rcx
	shrq	%cl, %r12
	andq	$255, %r12
	movq	$1, %r13
	jmp	.LBB1_33
.LBB1_32:                               # %sw.epilog.loopexit
	movq	$16, %rcx
	shrq	%cl, %r12
	andq	$255, %r12
	jmp	.LBB1_33
.LBB1_6:                                # %sw.bb
	movq	8(%r14), %rsi
	addq	%r15, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	jmp	.LBB1_41
.LBB1_7:                                # %sw.bb3
	movq	16(%r14), %rdi
	movq	24(%r14), %rsi
	movq	$63, %rcx
	movq	%r15, %rax
	sarq	%cl, %rax
	leaq	16(%rsp), %r8
	leaq	8(%rsp), %r9
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	add_double
	movq	16(%rsp), %rdi
	movq	8(%rsp), %rsi
	xorq	%rdx, %rdx
	callq	immed_double_const
	jmp	.LBB1_41
.LBB1_8:                                # %sw.bb13
	movq	8(%r14), %rdi
	movl	(%rdi), %eax
	movq	$16, %rcx
	shrq	%cl, %r12
	andq	$255, %r12
	andq	$67174399, %rax         # imm = 0x400FFFF
	cmpq	$67108932, %rax         # imm = 0x4000044
	jne	.LBB1_9
# BB#10:                                # %if.then26
	movzbl	2(%r14), %ebx
	callq	get_pool_constant
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	plus_constant_wide
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	force_const_mem
	movq	%rax, %rbx
	movq	8(%rbx), %rsi
	movzbl	2(%rbx), %edi
	callq	memory_address_p
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rax
	jne	.LBB1_42
	jmp	.LBB1_33
.LBB1_9:
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
.LBB1_33:                               # %sw.epilog
	testq	%r15, %r15
	je	.LBB1_36
# BB#34:                                # %if.then165
	xorq	%rdi, %rdi
	movq	%rsi, %rbx
	movq	%r15, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rbx, %rsi
.LBB1_35:                               # %if.end168
	movq	%rax, %r14
.LBB1_36:                               # %if.end168
	movzbl	(%r14), %eax
	addq	$-67, %rax
	andq	%rsi, %rax
	cmpq	$2, %rax
	jb	.LBB1_37
# BB#38:                                # %if.end168
	testq	%rsi, %r13
	je	.LBB1_39
# BB#40:                                # %if.then181
	movq	$58, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
.LBB1_41:                               # %return
	movq	%rax, %rbx
	jmp	.LBB1_42
.LBB1_37:
	movq	%r14, %rbx
	jmp	.LBB1_42
.LBB1_39:
	movq	%r14, %rbx
.LBB1_42:                               # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_22:                               # %if.else
	movq	$16, %rcx
	shrq	%cl, %r12
	andq	$255, %r12
	leaq	-67(%rax), %rcx
	andq	%rsi, %rcx
	cmpq	$2, %rcx
	jb	.LBB1_30
# BB#23:                                # %lor.lhs.false96
	cmpq	$133, %rax
	jg	.LBB1_28
# BB#24:                                # %lor.lhs.false96
	cmpq	$58, %rax
	ja	.LBB1_26
# BB#25:                                # %lor.lhs.false96
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	movabsq	$414331165718085632, %rax # imm = 0x5C0000000000000
	testq	%rax, %rdx
	jne	.LBB1_30
	jmp	.LBB1_26
.LBB1_28:                               # %lor.lhs.false96
	cmpq	$134, %rax
	je	.LBB1_30
# BB#29:                                # %lor.lhs.false96
	cmpq	$140, %rax
	jne	.LBB1_26
.LBB1_30:                               # %if.then144
	movq	8(%r14), %rbx
	movq	%rsi, %r14
	movq	%r15, %rsi
	callq	plus_constant_wide
	movq	$75, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%r14, %rsi
	jmp	.LBB1_35
.LBB1_26:                               # %if.else153
	leaq	24(%rsp), %rdi
	callq	find_constant_term_loc
	testq	%rax, %rax
	je	.LBB1_27
# BB#31:                                # %if.then156
	movq	%r14, %rdi
	callq	copy_rtx
	movq	%rax, (%rsp)
	leaq	(%rsp), %rdi
	callq	find_constant_term_loc
	movq	%rax, %rbx
	movq	(%rbx), %rdi
	movq	%r15, %rsi
	callq	plus_constant_wide
	movq	%rax, (%rbx)
	movq	(%rsp), %r14
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	jmp	.LBB1_36
.LBB1_27:
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	jmp	.LBB1_33
.Ltmp18:
	.size	plus_constant_wide, .Ltmp18-plus_constant_wide
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI1_0:
	.quad	.LBB1_6
	.quad	.LBB1_7
	.quad	.LBB1_32
	.quad	.LBB1_32
	.quad	.LBB1_11
	.quad	.LBB1_32
	.quad	.LBB1_32
	.quad	.LBB1_32
	.quad	.LBB1_32
	.quad	.LBB1_32
	.quad	.LBB1_32
	.quad	.LBB1_32
	.quad	.LBB1_8
	.quad	.LBB1_12
	.quad	.LBB1_12

	.text
	.globl	eliminate_constant_term
	.align	16, 0x90
	.type	eliminate_constant_term,@function
eliminate_constant_term:                # @eliminate_constant_term
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp24:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp25:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp26:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp27:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp28:
	.cfi_def_cfa_offset 48
.Ltmp29:
	.cfi_offset %rbx, -40
.Ltmp30:
	.cfi_offset %r12, -32
.Ltmp31:
	.cfi_offset %r14, -24
.Ltmp32:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	(%rbx), %eax
	movq	%rsi, %r14
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$75, %rcx
	jne	.LBB2_8
# BB#1:                                 # %if.end
	movq	16(%rbx), %r8
	movzbl	(%r8), %ecx
	cmpq	$54, %rcx
	jne	.LBB2_4
# BB#2:                                 # %land.lhs.true
	movq	(%r14), %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	$75, %rdi
	movq	%rax, %rsi
	movq	%r8, %rcx
	callq	simplify_binary_operation
	testq	%rax, %rax
	movq	%rax, (%rsp)
	je	.LBB2_4
# BB#3:                                 # %land.lhs.true10
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB2_4
# BB#9:                                 # %if.then14
	movq	%rax, (%r14)
	movq	8(%rbx), %rdi
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	eliminate_constant_term  # TAILCALL
.LBB2_4:                                # %if.end19
	movq	const_int_rtx+512(%rip), %rax
	movq	%rax, (%rsp)
	movq	8(%rbx), %rdi
	leaq	(%rsp), %r12
	movq	%r12, %rsi
	callq	eliminate_constant_term
	movq	16(%rbx), %rdi
	movq	%rax, %r15
	movq	%r12, %rsi
	callq	eliminate_constant_term
	movq	16(%rbx), %rcx
	movq	%rax, %r12
	cmpq	%rcx, %r12
	jne	.LBB2_6
# BB#5:                                 # %lor.lhs.false
	movq	8(%rbx), %rax
	cmpq	%rax, %r15
	je	.LBB2_8
.LBB2_6:                                # %land.lhs.true36
	movq	(%r14), %rdx
	movq	(%rsp), %rcx
	movzbl	2(%rbx), %esi
	movq	$75, %rdi
	callq	simplify_binary_operation
	testq	%rax, %rax
	movq	%rax, (%rsp)
	je	.LBB2_8
# BB#7:                                 # %land.lhs.true42
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB2_8
# BB#10:                                # %if.then46
	movq	%rax, (%r14)
	movzbl	2(%rbx), %esi
	movq	$75, %rdi
	movq	%r15, %rdx
	movq	%r12, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.LBB2_8:                                # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp33:
	.size	eliminate_constant_term, .Ltmp33-eliminate_constant_term
	.cfi_endproc

	.globl	find_next_ref
	.align	16, 0x90
	.type	find_next_ref,@function
find_next_ref:                          # @find_next_ref
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp40:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp41:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp42:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp43:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp44:
	.cfi_def_cfa_offset 48
.Ltmp45:
	.cfi_offset %rbx, -48
.Ltmp46:
	.cfi_offset %r12, -40
.Ltmp47:
	.cfi_offset %r13, -32
.Ltmp48:
	.cfi_offset %r14, -24
.Ltmp49:
	.cfi_offset %r15, -16
	movq	24(%rsi), %rbx
	movq	%rdi, %r15
	xorq	%r14, %r14
	testq	%rbx, %rbx
	je	.LBB3_15
# BB#1:                                 # %for.body.lr.ph
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
.LBB3_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	movq	24(%rbx), %r13
	leaq	-32(%rax), %rcx
	andq	%r12, %rcx
	cmpq	$3, %rcx
	jae	.LBB3_3
# BB#6:                                 # %if.then23
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	reg_set_p
	testq	%r12, %rax
	jne	.LBB3_15
# BB#7:                                 # %if.end26
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	32(%rbx), %rsi
	movq	%r15, %rdi
	callq	reg_mentioned_p
	testq	%r12, %rax
	jne	.LBB3_8
# BB#9:                                 # %if.end33
                                        #   in Loop: Header=BB3_2 Depth=1
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$33, %rcx
	jne	.LBB3_12
# BB#10:                                # %if.then37
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	%rbx, %rdi
	callq	any_uncondjump_p
	testq	%r12, %rax
	je	.LBB3_15
# BB#11:                                # %if.then40
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	64(%rbx), %r13
	movl	(%rbx), %eax
.LBB3_12:                               # %if.end45
                                        #   in Loop: Header=BB3_2 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$34, %rax
	jne	.LBB3_5
# BB#13:                                # %land.lhs.true
                                        #   in Loop: Header=BB3_2 Depth=1
	movl	8(%r15), %eax
	cmpq	$52, %rax
	ja	.LBB3_5
# BB#14:                                # %land.lhs.true52
                                        #   in Loop: Header=BB3_2 Depth=1
	movzbl	call_used_regs(%rax), %eax
	testq	%rax, %rax
	je	.LBB3_5
	jmp	.LBB3_15
	.align	16, 0x90
.LBB3_3:                                # %for.body
                                        #   in Loop: Header=BB3_2 Depth=1
	leaq	-35(%rax), %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	jb	.LBB3_15
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB3_2 Depth=1
	cmpq	$37, %rax
	jne	.LBB3_16
.LBB3_5:                                # %for.cond.backedge
                                        #   in Loop: Header=BB3_2 Depth=1
	testq	%r13, %r13
	movq	%r13, %rbx
	jne	.LBB3_2
	jmp	.LBB3_15
.LBB3_8:
	movq	%rbx, %r14
.LBB3_15:                               # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB3_16:                               # %if.else60
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_next_ref, %rcx
	movq	$279, %rsi              # imm = 0x117
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp50:
	.size	find_next_ref, .Ltmp50-find_next_ref
	.cfi_endproc

	.globl	expr_size
	.align	16, 0x90
	.type	expr_size,@function
expr_size:                              # @expr_size
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp54:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp55:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp56:
	.cfi_def_cfa_offset 32
.Ltmp57:
	.cfi_offset %rbx, -24
.Ltmp58:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movzbl	16(%r14), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$100, %rax
	jne	.LBB4_2
# BB#1:                                 # %land.lhs.true
	movq	64(%r14), %rbx
	testq	%rbx, %rbx
	jne	.LBB4_3
.LBB4_2:                                # %if.else
	movq	8(%r14), %rdi
	callq	size_in_bytes
	movq	%rax, %rbx
.LBB4_3:                                # %if.end
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	je	.LBB4_6
# BB#4:                                 # %land.lhs.true13
	movq	%rbx, %rdi
	callq	contains_placeholder_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB4_6
# BB#5:                                 # %if.then15
	movq	sizetype_tab(%rip), %rsi
	movq	$58, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	build
	movq	%rax, %rbx
.LBB4_6:                                # %if.end17
	movq	sizetype_tab(%rip), %rax
	movl	60(%rax), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	expand_expr  # TAILCALL
.Ltmp59:
	.size	expr_size, .Ltmp59-expr_size
	.cfi_endproc

	.globl	copy_all_regs
	.align	16, 0x90
	.type	copy_all_regs,@function
copy_all_regs:                          # @copy_all_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp63:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp64:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp65:
	.cfi_def_cfa_offset 32
.Ltmp66:
	.cfi_offset %rbx, -24
.Ltmp67:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movl	(%rbx), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$74, %rax
	jg	.LBB5_3
# BB#1:                                 # %entry
	cmpq	$61, %rax
	jne	.LBB5_2
# BB#8:                                 # %if.then
	movl	8(%rbx), %eax
	cmpq	$6, %rax
	je	.LBB5_7
# BB#9:                                 # %if.then
	cmpq	$20, %rax
	je	.LBB5_7
	jmp	.LBB5_10
.LBB5_3:                                # %entry
	addq	$-75, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$3, %rcx
	ja	.LBB5_7
# BB#4:                                 # %entry
	cmpq	$2, %rcx
	je	.LBB5_7
# BB#5:                                 # %if.then23
	movq	8(%rbx), %rdi
	callq	copy_all_regs
	movq	16(%rbx), %rdi
	movq	%rax, %r14
	callq	copy_all_regs
	movq	8(%rbx), %rcx
	cmpq	%rcx, %r14
	jne	.LBB5_15
# BB#6:                                 # %lor.lhs.false35
	movq	16(%rbx), %rcx
	cmpq	%rcx, %rax
	je	.LBB5_7
.LBB5_15:                               # %if.then40
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	movzbl	(%rbx), %edi
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.LBB5_2:                                # %entry
	cmpq	$66, %rax
	jne	.LBB5_7
.LBB5_10:                               # %if.then6
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r14
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	general_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB5_12
# BB#11:                                # %if.then.i
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	force_operand
	movq	%rax, %rbx
.LBB5_12:                               # %if.end.i
	cmpq	%r14, %rbx
	je	.LBB5_14
# BB#13:                                # %if.then3.i
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
	jmp	.LBB5_14
.LBB5_7:
	movq	%rbx, %r14
.LBB5_14:                               # %if.end47
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp68:
	.size	copy_all_regs, .Ltmp68-copy_all_regs
	.cfi_endproc

	.globl	copy_to_reg
	.align	16, 0x90
	.type	copy_to_reg,@function
copy_to_reg:                            # @copy_to_reg
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
	movq	%rdi, %rbx
	movzbl	2(%rbx), %edi
	callq	gen_reg_rtx
	movq	%rax, %r14
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	general_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB6_2
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	force_operand
	movq	%rax, %rbx
.LBB6_2:                                # %if.end
	cmpq	%r14, %rbx
	je	.LBB6_4
# BB#3:                                 # %if.then3
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
.LBB6_4:                                # %if.end5
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp77:
	.size	copy_to_reg, .Ltmp77-copy_to_reg
	.cfi_endproc

	.globl	memory_address
	.align	16, 0x90
	.type	memory_address,@function
memory_address:                         # @memory_address
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp84:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp85:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp86:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp87:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp88:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp89:
	.cfi_def_cfa_offset 80
.Ltmp90:
	.cfi_offset %rbx, -48
.Ltmp91:
	.cfi_offset %r12, -40
.Ltmp92:
	.cfi_offset %r13, -32
.Ltmp93:
	.cfi_offset %r14, -24
.Ltmp94:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movzbl	(%r12), %eax
	movq	%rdi, %r15
	cmpq	$70, %rax
	jne	.LBB7_2
# BB#1:
	movq	%r12, %rbx
	jmp	.LBB7_62
.LBB7_2:                                # %if.end
	movl	cse_not_expected(%rip), %edx
	testq	%rdx, %rdx
	je	.LBB7_3
# BB#10:                                # %if.else
	cmpq	$71, %rax
	jne	.LBB7_6
	jmp	.LBB7_11
.LBB7_3:                                # %land.lhs.true
	leaq	-54(%rax), %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$17, %rcx
	ja	.LBB7_6
# BB#4:                                 # %land.lhs.true
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24595, %rsi            # imm = 0x6013
	je	.LBB7_5
# BB#8:                                 # %if.then51
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%r12, %rsi
	callq	force_reg
	movq	%rax, %rbx
	jmp	.LBB7_29
.LBB7_5:                                # %land.lhs.true
	cmpq	$17, %rcx
	jne	.LBB7_6
.LBB7_11:                               # %land.lhs.true56
	movq	8(%r12), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$61, %rcx
	jne	.LBB7_6
# BB#12:
	movq	%r12, %rbx
	jmp	.LBB7_62
.LBB7_6:                                # %if.else61
	testq	%rdx, %rdx
	je	.LBB7_7
# BB#13:                                # %if.else61
	movabsq	$1, %rcx
	jmp	.LBB7_14
.LBB7_7:
	movabsq	$0, %rcx
.LBB7_14:                               # %if.else61
	cmpq	$61, %rax
	movq	%r12, %rbx
	je	.LBB7_17
# BB#15:                                # %if.else61
	testq	%rcx, %rcx
	movq	%r12, %rbx
	jne	.LBB7_17
# BB#16:                                # %if.then67
	movq	%r12, %rdi
	callq	break_out_memory_refs
	movq	%rax, %rbx
.LBB7_17:                               # %do.body
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	legitimate_address_p
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB7_21
# BB#18:
	movq	%rbx, %r14
	jmp	.LBB7_19
.LBB7_21:                               # %do.end
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	memory_address_p
	testq	%r13, %rax
	movq	%r12, %r14
	jne	.LBB7_19
# BB#22:                                # %do.body78
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	legitimize_address
	movq	%rax, %r14
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	memory_address_p
	testq	%r13, %rax
	je	.LBB7_23
.LBB7_19:                               # %win
	movl	flag_force_addr(%rip), %eax
	testq	%rax, %rax
	je	.LBB7_20
# BB#43:                                # %win
	movl	cse_not_expected(%rip), %eax
	testq	%r13, %rax
	jne	.LBB7_44
# BB#45:                                # %land.lhs.true135
	movzbl	(%r14), %eax
	cmpq	$61, %rax
	jne	.LBB7_47
# BB#46:
	movq	%r14, %rbx
	jmp	.LBB7_29
.LBB7_20:
	movq	%r14, %rbx
	jmp	.LBB7_29
.LBB7_44:
	movq	%r14, %rbx
	jmp	.LBB7_29
.LBB7_47:                               # %land.lhs.true135
	cmpq	$75, %rax
	jne	.LBB7_52
# BB#48:                                # %land.lhs.true143
	movq	8(%r14), %rax
	movq	global_rtl+56(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB7_49
# BB#50:                                # %land.lhs.true143
	movq	global_rtl+48(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB7_51
.LBB7_52:                               # %if.then153
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	%r14, %rdi
	callq	general_operand
	testq	%r13, %rax
	jne	.LBB7_53
	jmp	.LBB7_41
.LBB7_23:                               # %do.end84
	movl	(%r14), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$74, %rax
	jg	.LBB7_31
# BB#24:                                # %do.end84
	cmpq	$61, %rax
	jne	.LBB7_53
# BB#25:                                # %if.then122
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	general_operand
	testq	%r13, %rax
	jne	.LBB7_27
# BB#26:                                # %if.then.i271
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	force_operand
	movq	%rax, %r14
.LBB7_27:                               # %if.end.i274
	cmpq	%rbx, %r14
	je	.LBB7_29
# BB#28:                                # %if.then3.i276
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	emit_move_insn
	jmp	.LBB7_29
.LBB7_49:
	movq	%r14, %rbx
	jmp	.LBB7_29
.LBB7_51:
	movq	%r14, %rbx
	jmp	.LBB7_29
.LBB7_31:                               # %do.end84
	cmpq	$78, %rax
	je	.LBB7_41
# BB#32:                                # %do.end84
	cmpq	$76, %rax
	je	.LBB7_41
# BB#33:                                # %do.end84
	cmpq	$75, %rax
	jne	.LBB7_53
# BB#34:                                # %if.then88
	movq	const_int_rtx+512(%rip), %rax
	movq	%rax, 24(%rsp)
	leaq	24(%rsp), %rsi
	movq	%r14, %rdi
	callq	eliminate_constant_term
	movq	24(%rsp), %rdx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %rbx
	cmpq	%rcx, %rdx
	je	.LBB7_41
# BB#35:                                # %lor.lhs.false91
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	memory_address_p
	testq	%r13, %rax
	je	.LBB7_41
# BB#36:                                # %if.else96
	movzbl	2(%r14), %eax
	movq	%rax, (%rsp)            # 8-byte Spill
	movzbl	2(%rbx), %edi
	callq	gen_reg_rtx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	general_operand
	testq	%r13, %rax
	jne	.LBB7_38
# BB#37:                                # %if.then.i
	movq	%rbx, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	force_operand
	movq	%rax, %rbx
.LBB7_38:                               # %if.end.i
	movq	8(%rsp), %rdx           # 8-byte Reload
	cmpq	%rdx, %rbx
	je	.LBB7_40
# BB#39:                                # %if.then3.i
	movq	%rdx, %rdi
	movq	%rbx, %rsi
	movq	%rdx, %rbx
	callq	emit_move_insn
	movq	%rbx, %rdx
.LBB7_40:                               # %copy_to_reg.exit
	movq	$75, %rdi
	movq	(%rsp), %rsi            # 8-byte Reload
	movq	16(%rsp), %rcx          # 8-byte Reload
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	memory_address_p
	testq	%r13, %rax
	jne	.LBB7_29
.LBB7_41:                               # %if.else164
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	force_operand
	movq	%rax, %rbx
	jmp	.LBB7_29
.LBB7_53:                               # %if.else124
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%r14, %rsi
	callq	force_reg
	movq	%rax, %rbx
.LBB7_29:                               # %done
	cmpq	%r12, %rbx
	je	.LBB7_30
# BB#54:                                # %if.else172
	movzbl	(%rbx), %eax
	cmpq	$75, %rax
	jne	.LBB7_55
# BB#57:                                # %land.lhs.true181
	movq	8(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB7_61
# BB#58:                                # %land.lhs.true188
	movq	16(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB7_61
# BB#59:                                # %if.then195
	movq	$8, %rsi
	jmp	.LBB7_60
.LBB7_30:
	movq	%r12, %rbx
	jmp	.LBB7_62
.LBB7_55:                               # %if.else172
	cmpq	$61, %rax
	jne	.LBB7_61
# BB#56:                                # %if.then176
	movq	$8, %rsi
	movq	%rbx, %rdi
.LBB7_60:                               # %if.end201
	callq	mark_reg_pointer
.LBB7_61:                               # %if.end201
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	update_temp_slot_address
.LBB7_62:                               # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp95:
	.size	memory_address, .Ltmp95-memory_address
	.cfi_endproc

	.globl	force_reg
	.align	16, 0x90
	.type	force_reg,@function
force_reg:                              # @force_reg
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
	movq	%rsi, %r12
	movzbl	(%r12), %eax
	movq	%rdi, %rbx
	cmpq	$61, %rax
	jne	.LBB8_2
# BB#1:
	movq	%r12, %r14
	jmp	.LBB8_18
.LBB8_2:                                # %if.end
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	general_operand
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB8_4
# BB#3:                                 # %if.then1
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r14
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	emit_move_insn
	movq	%rax, %r15
	jmp	.LBB8_7
.LBB8_4:                                # %if.else
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	force_operand
	movq	%rax, %r14
	movzbl	(%r14), %eax
	cmpq	$61, %rax
	jne	.LBB8_6
# BB#5:                                 # %if.then8
	callq	get_last_insn
	movq	%rax, %r15
	jmp	.LBB8_7
.LBB8_6:                                # %if.else10
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	emit_move_insn
	movq	%rax, %r15
	movq	%rbx, %r14
.LBB8_7:                                # %if.end14
	movzbl	(%r12), %ecx
	cmpq	$133, %rcx
	jg	.LBB8_10
# BB#8:                                 # %if.end14
	addq	$-54, %rcx
	andq	%r13, %rcx
	cmpq	$14, %rcx
	ja	.LBB8_18
# BB#9:                                 # %if.end14
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB8_12
	jmp	.LBB8_18
.LBB8_10:                               # %if.end14
	cmpq	$134, %rcx
	je	.LBB8_12
# BB#11:                                # %if.end14
	cmpq	$140, %rcx
	jne	.LBB8_18
.LBB8_12:                               # %land.lhs.true
	movzbl	(%r15), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB8_18
# BB#13:                                # %cond.true
	movq	32(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB8_15
# BB#14:                                # %cond.false
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB8_15:                               # %cond.end63
	testq	%rax, %rax
	je	.LBB8_18
# BB#16:                                # %land.lhs.true67
	movq	8(%rax), %rax
	cmpq	%r14, %rax
	jne	.LBB8_18
# BB#17:                                # %if.then73
	movq	$4, %rsi
	movq	%r15, %rdi
	movq	%r12, %rdx
	callq	set_unique_reg_note
.LBB8_18:                               # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp112:
	.size	force_reg, .Ltmp112-force_reg
	.cfi_endproc

	.align	16, 0x90
	.type	break_out_memory_refs,@function
break_out_memory_refs:                  # @break_out_memory_refs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp116:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp117:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp118:
	.cfi_def_cfa_offset 32
.Ltmp119:
	.cfi_offset %rbx, -24
.Ltmp120:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movl	(%rbx), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-54(%rax), %rcx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %rcx
	cmpq	$24, %rcx
	ja	.LBB9_10
# BB#1:                                 # %entry
	movq	$1, %rsi
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24595, %rdx            # imm = 0x6013
	je	.LBB9_2
# BB#5:                                 # %land.lhs.true51
	testq	$16711680, %rdi         # imm = 0xFF0000
	jne	.LBB9_4
# BB#6:                                 # %if.else
	addq	$-75, %rax
	andq	%r8, %rax
	cmpq	$3, %rax
	ja	.LBB9_10
# BB#7:                                 # %if.else
	cmpq	$2, %rax
	jne	.LBB9_8
	jmp	.LBB9_10
.LBB9_2:                                # %entry
	shlq	%cl, %rsi
	testq	$23068672, %rsi         # imm = 0x1600000
	je	.LBB9_3
.LBB9_8:                                # %if.then69
	movq	8(%rbx), %rdi
	callq	break_out_memory_refs
	movq	16(%rbx), %rdi
	movq	%rax, %r14
	callq	break_out_memory_refs
	movq	8(%rbx), %rcx
	cmpq	%rcx, %r14
	jne	.LBB9_11
# BB#9:                                 # %lor.lhs.false79
	movq	16(%rbx), %rcx
	cmpq	%rcx, %rax
	je	.LBB9_10
.LBB9_11:                               # %if.then84
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	movzbl	(%rbx), %edi
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.LBB9_3:                                # %entry
	cmpq	$12, %rcx
	jne	.LBB9_10
.LBB9_4:                                # %if.then
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movq	%rbx, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	force_reg  # TAILCALL
.LBB9_10:                               # %if.end89
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp121:
	.size	break_out_memory_refs, .Ltmp121-break_out_memory_refs
	.cfi_endproc

	.globl	memory_address_noforce
	.align	16, 0x90
	.type	memory_address_noforce,@function
memory_address_noforce:                 # @memory_address_noforce
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp124:
	.cfi_def_cfa_offset 16
.Ltmp125:
	.cfi_offset %rbx, -16
	movl	flag_force_addr(%rip), %ebx
	xorq	%rax, %rax
	movl	%eax, flag_force_addr(%rip)
	callq	memory_address
	movl	%ebx, flag_force_addr(%rip)
	popq	%rbx
	retq
.Ltmp126:
	.size	memory_address_noforce, .Ltmp126-memory_address_noforce
	.cfi_endproc

	.globl	validize_mem
	.align	16, 0x90
	.type	validize_mem,@function
validize_mem:                           # @validize_mem
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp129:
	.cfi_def_cfa_offset 16
.Ltmp130:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movl	(%rbx), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB11_7
# BB#1:                                 # %if.end
	movl	flag_force_addr(%rip), %eax
	movq	8(%rbx), %rsi
	testq	%rax, %rax
	je	.LBB11_4
# BB#2:                                 # %land.lhs.true
	movzbl	(%rsi), %eax
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB11_4
# BB#3:                                 # %land.lhs.true
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24595, %rax            # imm = 0x6013
	jne	.LBB11_6
.LBB11_4:                               # %land.lhs.true31
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	callq	memory_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB11_5
.LBB11_7:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	retq
.LBB11_5:                               # %land.lhs.true31.if.end39_crit_edge
	movq	8(%rbx), %rsi
.LBB11_6:                               # %if.end39
	movq	%rbx, %rdi
	popq	%rbx
	jmp	replace_equiv_address  # TAILCALL
.Ltmp131:
	.size	validize_mem, .Ltmp131-validize_mem
	.cfi_endproc

	.globl	maybe_set_unchanging
	.align	16, 0x90
	.type	maybe_set_unchanging,@function
maybe_set_unchanging:                   # @maybe_set_unchanging
	.cfi_startproc
# BB#0:                                 # %entry
	movl	16(%rsi), %ecx
	movq	%rcx, %rax
	andq	$255, %rax
	testq	$4096, %rcx             # imm = 0x1000
	je	.LBB12_5
# BB#1:                                 # %land.lhs.true
	movzbl	tree_code_type(%rax), %ecx
	cmpq	$100, %rcx
	jne	.LBB12_5
# BB#2:                                 # %land.lhs.true5
	cmpq	$35, %rax
	je	.LBB12_6
# BB#3:                                 # %lor.lhs.false
	movq	104(%rsi), %rcx
	testq	%rcx, %rcx
	je	.LBB12_6
# BB#4:                                 # %lor.lhs.false14
	movzbl	17(%rcx), %ecx
	testq	$2, %rcx
	jne	.LBB12_6
.LBB12_5:                               # %lor.lhs.false22
	movzbl	tree_code_type(%rax), %eax
	cmpq	$99, %rax
	jne	.LBB12_7
.LBB12_6:                               # %if.then
	movl	(%rdi), %eax
	orq	$67108864, %rax         # imm = 0x4000000
	movl	%eax, (%rdi)
.LBB12_7:                               # %if.end
	retq
.Ltmp132:
	.size	maybe_set_unchanging, .Ltmp132-maybe_set_unchanging
	.cfi_endproc

	.globl	stabilize
	.align	16, 0x90
	.type	stabilize,@function
stabilize:                              # @stabilize
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp136:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp137:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp138:
	.cfi_def_cfa_offset 32
.Ltmp139:
	.cfi_offset %rbx, -24
.Ltmp140:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	jne	.LBB13_2
# BB#1:                                 # %lor.lhs.false
	movq	8(%r14), %rdi
	callq	rtx_unstable_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_2
# BB#3:                                 # %if.end
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	movq	8(%r14), %rdi
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	callq	copy_all_regs
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	force_reg
	movq	%r14, %rdi
	movq	%rax, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	replace_equiv_address  # TAILCALL
.LBB13_2:                               # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp141:
	.size	stabilize, .Ltmp141-stabilize
	.cfi_endproc

	.globl	copy_addr_to_reg
	.align	16, 0x90
	.type	copy_addr_to_reg,@function
copy_addr_to_reg:                       # @copy_addr_to_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp147:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp148:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp149:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp150:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp151:
	.cfi_def_cfa_offset 48
.Ltmp152:
	.cfi_offset %rbx, -40
.Ltmp153:
	.cfi_offset %r12, -32
.Ltmp154:
	.cfi_offset %r14, -24
.Ltmp155:
	.cfi_offset %r15, -16
	movl	target_flags(%rip), %ebx
	movq	%rdi, %r15
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r14
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	general_operand
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	jne	.LBB14_2
# BB#1:                                 # %if.then.i
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	force_operand
	movq	%rax, %r15
.LBB14_2:                               # %if.end.i
	movzbl	2(%r15), %eax
	andq	%r12, %rbx
	cmpq	%rbx, %rax
	je	.LBB14_4
# BB#3:                                 # %if.end.i
	testq	%rax, %rax
	jne	.LBB14_7
.LBB14_4:                               # %if.end8.i
	cmpq	%r14, %r15
	je	.LBB14_6
# BB#5:                                 # %if.then10.i
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	emit_move_insn
.LBB14_6:                               # %copy_to_mode_reg.exit
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB14_7:                               # %if.then7.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.copy_to_mode_reg, %rcx
	movq	$713, %rsi              # imm = 0x2C9
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp156:
	.size	copy_addr_to_reg, .Ltmp156-copy_addr_to_reg
	.cfi_endproc

	.globl	copy_to_mode_reg
	.align	16, 0x90
	.type	copy_to_mode_reg,@function
copy_to_mode_reg:                       # @copy_to_mode_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp162:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp163:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp164:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp165:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp166:
	.cfi_def_cfa_offset 48
.Ltmp167:
	.cfi_offset %rbx, -40
.Ltmp168:
	.cfi_offset %r12, -32
.Ltmp169:
	.cfi_offset %r14, -24
.Ltmp170:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	gen_reg_rtx
	movq	%rax, %r14
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	general_operand
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	jne	.LBB15_2
# BB#1:                                 # %if.then
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	force_operand
	movq	%rax, %r15
.LBB15_2:                               # %if.end
	movzbl	2(%r15), %eax
	andq	%r12, %rbx
	cmpq	%rbx, %rax
	je	.LBB15_4
# BB#3:                                 # %if.end
	testq	%rax, %rax
	jne	.LBB15_7
.LBB15_4:                               # %if.end8
	cmpq	%r14, %r15
	je	.LBB15_6
# BB#5:                                 # %if.then10
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	emit_move_insn
.LBB15_6:                               # %if.end12
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB15_7:                               # %if.then7
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.copy_to_mode_reg, %rcx
	movq	$713, %rsi              # imm = 0x2C9
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp171:
	.size	copy_to_mode_reg, .Ltmp171-copy_to_mode_reg
	.cfi_endproc

	.globl	force_not_mem
	.align	16, 0x90
	.type	force_not_mem,@function
force_not_mem:                          # @force_not_mem
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp175:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp176:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp177:
	.cfi_def_cfa_offset 32
.Ltmp178:
	.cfi_offset %rbx, -24
.Ltmp179:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movl	(%rbx), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB16_3
# BB#1:                                 # %lor.lhs.false
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	cmpq	$51, %rdi
	je	.LBB16_3
# BB#2:                                 # %if.end
	callq	gen_reg_rtx
	movq	%rax, %r14
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
	movq	%r14, %rbx
.LBB16_3:                               # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp180:
	.size	force_not_mem, .Ltmp180-force_not_mem
	.cfi_endproc

	.globl	copy_to_suggested_reg
	.align	16, 0x90
	.type	copy_to_suggested_reg,@function
copy_to_suggested_reg:                  # @copy_to_suggested_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp184:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp185:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp186:
	.cfi_def_cfa_offset 32
.Ltmp187:
	.cfi_offset %rbx, -24
.Ltmp188:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	testq	%rbx, %rbx
	je	.LBB17_2
# BB#1:                                 # %land.lhs.true
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	je	.LBB17_3
.LBB17_2:                               # %if.else
	movq	%rdx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rbx
.LBB17_3:                               # %if.end
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	emit_move_insn
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp189:
	.size	copy_to_suggested_reg, .Ltmp189-copy_to_suggested_reg
	.cfi_endproc

	.globl	promote_mode
	.align	16, 0x90
	.type	promote_mode,@function
promote_mode:                           # @promote_mode
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	16(%rdi), %ecx
	movl	(%rdx), %eax
	cmpq	$14, %rcx
	ja	.LBB18_7
# BB#1:                                 # %entry
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	$23744, %rdi            # imm = 0x5CC0
	je	.LBB18_7
# BB#2:                                 # %do.body
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	%rsi, %rcx
	andq	%rdi, %rcx
	cmpq	$2, %rcx
	jne	.LBB18_3
# BB#5:                                 # %land.lhs.true3
	movl	ix86_cpu(%rip), %ecx
	movl	x86_promote_qi_regs(%rip), %r8d
	movq	$1, %rsi
	shlq	%cl, %rsi
	andq	%r8, %rsi
	testq	%rdi, %rsi
	movq	$2, %rsi
	jne	.LBB18_6
	jmp	.LBB18_7
.LBB18_3:                               # %do.body
	cmpq	$3, %rcx
	jne	.LBB18_7
# BB#4:                                 # %land.lhs.true
	movl	ix86_cpu(%rip), %ecx
	movl	x86_promote_hi_regs(%rip), %r8d
	movq	$1, %rsi
	shlq	%cl, %rsi
	andq	%r8, %rsi
	testq	%rdi, %rsi
	movq	$3, %rsi
	je	.LBB18_7
.LBB18_6:                               # %if.then
	movq	$4, %rsi
.LBB18_7:                               # %sw.epilog
	movl	%eax, (%rdx)
	movq	%rsi, %rax
	retq
.Ltmp190:
	.size	promote_mode, .Ltmp190-promote_mode
	.cfi_endproc

	.globl	adjust_stack
	.align	16, 0x90
	.type	adjust_stack,@function
adjust_stack:                           # @adjust_stack
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp192:
	.cfi_def_cfa_offset 16
	xorq	%rsi, %rsi
	callq	protect_from_queue
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB19_4
# BB#1:                                 # %if.end
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB19_3
# BB#2:                                 # %if.then2
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	8(%rax), %rdx
	movl	8(%rcx), %esi
	subq	%rdx, %rsi
	movl	%esi, 8(%rcx)
.LBB19_3:                               # %if.end4
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	optab_table(%rip), %rsi
	movq	global_rtl+16(%rip), %rdx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rax, %rcx
	movq	%rdx, %r8
	callq	expand_binop
	movq	global_rtl+16(%rip), %rdi
	cmpq	%rdi, %rax
	je	.LBB19_4
# BB#5:                                 # %if.then8
	movq	%rax, %rsi
	popq	%rax
	jmp	emit_move_insn  # TAILCALL
.LBB19_4:                               # %if.end10
	popq	%rax
	retq
.Ltmp193:
	.size	adjust_stack, .Ltmp193-adjust_stack
	.cfi_endproc

	.globl	anti_adjust_stack
	.align	16, 0x90
	.type	anti_adjust_stack,@function
anti_adjust_stack:                      # @anti_adjust_stack
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp195:
	.cfi_def_cfa_offset 16
	xorq	%rsi, %rsi
	callq	protect_from_queue
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB20_4
# BB#1:                                 # %if.end
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB20_3
# BB#2:                                 # %if.then2
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	8(%rax), %rdx
	movl	8(%rcx), %esi
	addq	%rdx, %rsi
	movl	%esi, 8(%rcx)
.LBB20_3:                               # %if.end4
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	optab_table+16(%rip), %rsi
	movq	global_rtl+16(%rip), %rdx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rax, %rcx
	movq	%rdx, %r8
	callq	expand_binop
	movq	global_rtl+16(%rip), %rdi
	cmpq	%rdi, %rax
	je	.LBB20_4
# BB#5:                                 # %if.then8
	movq	%rax, %rsi
	popq	%rax
	jmp	emit_move_insn  # TAILCALL
.LBB20_4:                               # %if.end10
	popq	%rax
	retq
.Ltmp196:
	.size	anti_adjust_stack, .Ltmp196-anti_adjust_stack
	.cfi_endproc

	.globl	round_push
	.align	16, 0x90
	.type	round_push,@function
round_push:                             # @round_push
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp202:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp203:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp204:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp205:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp206:
	.cfi_def_cfa_offset 48
.Ltmp207:
	.cfi_offset %rbx, -40
.Ltmp208:
	.cfi_offset %r12, -32
.Ltmp209:
	.cfi_offset %r14, -24
.Ltmp210:
	.cfi_offset %r15, -16
	movslq	ix86_preferred_stack_boundary(%rip), %rax
	movq	%rdi, %r14
	movq	$60, %rcx
	movq	%rax, %rbx
	shrq	%cl, %rbx
	andq	$7, %rbx
	addq	%rax, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$35, %rcx
	sarq	%cl, %rbx
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rax, %rcx
	cmpq	$8, %rcx
	je	.LBB21_5
# BB#1:                                 # %if.end
	movzbl	(%r14), %eax
	cmpq	$54, %rax
	jne	.LBB21_4
# BB#2:                                 # %if.then2
	movq	8(%r14), %rdi
	leaq	-1(%rbx,%rdi), %rsi
	movq	%rsi, %rax
	cqto
	idivq	%rbx
	subq	%rdx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdi
	jne	.LBB21_3
.LBB21_5:                               # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB21_4:                               # %if.else
	movl	target_flags(%rip), %r15d
	movq	$25, %rcx
	shrq	%cl, %r15
	andq	$1, %r15
	orq	$4, %r15
	movq	optab_table(%rip), %r12
	leaq	-1(%rbx), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	xorq	%r8, %r8
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	expand_binop
	movl	target_flags(%rip), %r14d
	movq	%rax, %r15
	movq	$25, %rcx
	shrq	%cl, %r14
	andq	$1, %r14
	orq	$4, %r14
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_CONST_INT
	xorq	%rdi, %rdi
	movq	$62, %rsi
	xorq	%r9, %r9
	movq	$1, (%rsp)
	movq	%r14, %rdx
	movq	%r15, %rcx
	movq	%rax, %r8
	callq	expand_divmod
	movl	target_flags(%rip), %r14d
	movq	%rax, %r15
	movq	$25, %rcx
	shrq	%cl, %r14
	andq	$1, %r14
	orq	$4, %r14
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_CONST_INT
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	expand_mult  # TAILCALL
.LBB21_3:                               # %if.then13
	xorq	%rdi, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_CONST_INT  # TAILCALL
.Ltmp211:
	.size	round_push, .Ltmp211-round_push
	.cfi_endproc

	.globl	emit_stack_save
	.align	16, 0x90
	.type	emit_stack_save,@function
emit_stack_save:                        # @emit_stack_save
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp216:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp217:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp218:
	.cfi_def_cfa_offset 32
.Ltmp219:
	.cfi_offset %rbx, -32
.Ltmp220:
	.cfi_offset %r14, -24
.Ltmp221:
	.cfi_offset %r15, -16
	movl	target_flags(%rip), %eax
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	(%rbx), %r15
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	$4, %rax
	testq	%r15, %r15
	je	.LBB22_1
# BB#5:                                 # %lor.lhs.false
	movzbl	2(%r15), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	%rax, %rcx
	je	.LBB22_6
# BB#29:                                # %if.then12
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.emit_stack_save, %rcx
	movq	$1001, %rsi             # imm = 0x3E9
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB22_1:                               # %if.then2
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdi
	cmpq	$2, %rdi
	jne	.LBB22_3
# BB#2:                                 # %if.then4
	andq	%rax, %rcx
	movzbl	mode_size(%rcx), %esi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	assign_stack_local
	jmp	.LBB22_4
.LBB22_3:                               # %if.else
	movq	%rax, %rdi
	callq	gen_reg_rtx
.LBB22_4:                               # %if.end14
	movq	%rax, %r15
	movq	%r15, (%rbx)
.LBB22_6:                               # %if.end14
	testq	%r14, %r14
	je	.LBB22_18
# BB#7:                                 # %if.then16
	callq	start_sequence
	xorq	%rdi, %rdi
	testq	%r15, %r15
	je	.LBB22_17
# BB#8:                                 # %if.then19
	movl	(%r15), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB22_9
# BB#10:                                # %if.end.i
	movl	flag_force_addr(%rip), %eax
	movq	8(%r15), %rsi
	testq	%rax, %rax
	je	.LBB22_13
# BB#11:                                # %land.lhs.true.i
	movzbl	(%rsi), %eax
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB22_13
# BB#12:                                # %land.lhs.true.i
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24595, %rax            # imm = 0x6013
	jne	.LBB22_16
.LBB22_13:                              # %land.lhs.true31.i
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	callq	memory_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB22_15
# BB#14:
	movq	%r15, %rdi
	jmp	.LBB22_17
.LBB22_18:                              # %if.else26
	xorq	%rdi, %rdi
	testq	%r15, %r15
	je	.LBB22_28
# BB#19:                                # %if.then29
	movl	(%r15), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB22_20
# BB#21:                                # %if.end.i57
	movl	flag_force_addr(%rip), %eax
	movq	8(%r15), %rsi
	testq	%rax, %rax
	je	.LBB22_24
# BB#22:                                # %land.lhs.true.i60
	movzbl	(%rsi), %eax
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB22_24
# BB#23:                                # %land.lhs.true.i60
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24595, %rax            # imm = 0x6013
	jne	.LBB22_27
.LBB22_24:                              # %land.lhs.true31.i65
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	callq	memory_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB22_26
# BB#25:
	movq	%r15, %rdi
	jmp	.LBB22_28
.LBB22_9:
	movq	%r15, %rdi
	jmp	.LBB22_17
.LBB22_20:
	movq	%r15, %rdi
	jmp	.LBB22_28
.LBB22_15:                              # %land.lhs.true31.if.end39_crit_edge.i
	movq	8(%r15), %rsi
.LBB22_16:                              # %if.end39.i
	movq	%r15, %rdi
	callq	replace_equiv_address
	movq	%rax, %rdi
.LBB22_17:                              # %if.end21
	movq	global_rtl+16(%rip), %rsi
	callq	gen_move_insn
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	movq	%r14, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	emit_insn_after  # TAILCALL
.LBB22_26:                              # %land.lhs.true31.if.end39_crit_edge.i69
	movq	8(%r15), %rsi
.LBB22_27:                              # %if.end39.i71
	movq	%r15, %rdi
	callq	replace_equiv_address
	movq	%rax, %rdi
.LBB22_28:                              # %if.end31
	movq	global_rtl+16(%rip), %rsi
	callq	gen_move_insn
	movq	%rax, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	emit_insn  # TAILCALL
.Ltmp222:
	.size	emit_stack_save, .Ltmp222-emit_stack_save
	.cfi_endproc

	.globl	emit_stack_restore
	.align	16, 0x90
	.type	emit_stack_restore,@function
emit_stack_restore:                     # @emit_stack_restore
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp227:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp228:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp229:
	.cfi_def_cfa_offset 32
.Ltmp230:
	.cfi_offset %rbx, -32
.Ltmp231:
	.cfi_offset %r14, -24
.Ltmp232:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	xorq	%rbx, %rbx
	testq	%r15, %r15
	je	.LBB23_10
# BB#1:                                 # %if.then
	movl	(%r15), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB23_2
# BB#3:                                 # %if.end.i
	movl	flag_force_addr(%rip), %eax
	movq	8(%r15), %rsi
	testq	%rax, %rax
	je	.LBB23_6
# BB#4:                                 # %land.lhs.true.i
	movzbl	(%rsi), %eax
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB23_6
# BB#5:                                 # %land.lhs.true.i
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24595, %rax            # imm = 0x6013
	jne	.LBB23_9
.LBB23_6:                               # %land.lhs.true31.i
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	callq	memory_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB23_8
# BB#7:
	movq	%r15, %rbx
	jmp	.LBB23_10
.LBB23_2:
	movq	%r15, %rbx
	jmp	.LBB23_10
.LBB23_8:                               # %land.lhs.true31.if.end39_crit_edge.i
	movq	8(%r15), %rsi
.LBB23_9:                               # %if.end39.i
	movq	%r15, %rdi
	callq	replace_equiv_address
	movq	%rax, %rbx
.LBB23_10:                              # %if.end
	testq	%r14, %r14
	je	.LBB23_12
# BB#11:                                # %if.then1
	callq	start_sequence
	movq	global_rtl+16(%rip), %rdi
	movq	%rbx, %rsi
	callq	gen_move_insn
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	movq	%r14, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	emit_insn_after  # TAILCALL
.LBB23_12:                              # %if.else
	movq	global_rtl+16(%rip), %rdi
	movq	%rbx, %rsi
	callq	gen_move_insn
	movq	%rax, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	emit_insn  # TAILCALL
.Ltmp233:
	.size	emit_stack_restore, .Ltmp233-emit_stack_restore
	.cfi_endproc

	.globl	allocate_dynamic_stack_space
	.align	16, 0x90
	.type	allocate_dynamic_stack_space,@function
allocate_dynamic_stack_space:           # @allocate_dynamic_stack_space
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp240:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp241:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp242:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp243:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp244:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp245:
	.cfi_def_cfa_offset 64
.Ltmp246:
	.cfi_offset %rbx, -48
.Ltmp247:
	.cfi_offset %r12, -40
.Ltmp248:
	.cfi_offset %r13, -32
.Ltmp249:
	.cfi_offset %r14, -24
.Ltmp250:
	.cfi_offset %r15, -16
	movq	const_int_rtx+512(%rip), %rax
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%rdi, %r12
	cmpq	%r12, %rax
	je	.LBB24_1
# BB#2:                                 # %if.end
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$64, %rcx
	movl	%ecx, 424(%rax)
	movzbl	2(%r12), %eax
	testq	%rax, %rax
	je	.LBB24_5
# BB#3:                                 # %land.lhs.true
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	cmpq	%rdi, %rax
	je	.LBB24_5
# BB#4:                                 # %if.then8
	movq	$1, %rdx
	movq	%r12, %rsi
	callq	convert_to_mode
	movq	%rax, %r12
.LBB24_5:                               # %if.end12
	movl	ix86_preferred_stack_boundary(%rip), %eax
	movq	cfun(%rip), %rcx
	movl	%eax, 404(%rcx)
	movslq	ix86_preferred_stack_boundary(%rip), %rsi
	cmpq	$127, %rsi
	jg	.LBB24_7
# BB#6:                                 # %if.end17
	movq	$15, %rsi
	movq	%r12, %rdi
	callq	plus_constant_wide
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	force_operand
	movslq	ix86_preferred_stack_boundary(%rip), %rsi
	movq	%rax, %r12
	cmpq	$128, %rsi
	jl	.LBB24_8
.LBB24_7:                               # %lor.lhs.false
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	je	.LBB24_9
.LBB24_8:                               # %if.then20
	movq	%r12, %rdi
	callq	round_push
	movq	%rax, %r12
.LBB24_9:                               # %if.end22
	callq	do_pending_stack_adjust
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movslq	ix86_preferred_stack_boundary(%rip), %rdx
	movslq	8(%rax), %rax
	movq	$60, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$7, %rsi
	addq	%rdx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$35, %rcx
	sarq	%cl, %rsi
	cqto
	idivq	%rsi
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rdx
	jne	.LBB24_10
# BB#12:                                # %if.end26
	movl	flag_stack_check(%rip), %eax
	testq	%rax, %rax
	je	.LBB24_14
# BB#13:                                # %if.then28
	movl	target_flags(%rip), %eax
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	imulq	$75, %rax, %rcx
	movq	$4392, %rdi             # imm = 0x1128
	subq	%rax, %rdi
	addq	%rcx, %rdi
	movq	%r12, %rsi
	callq	probe_stack_range
.LBB24_14:                              # %if.end35
	testq	%r14, %r14
	je	.LBB24_18
# BB#15:                                # %lor.lhs.false38
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB24_18
# BB#16:                                # %lor.lhs.false43
	movl	8(%r14), %ecx
	cmpq	$53, %rcx
	jb	.LBB24_18
# BB#17:                                # %lor.lhs.false46
	movq	$16, %rcx
	movl	target_flags(%rip), %edx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	orq	$4, %rdx
	cmpq	%rdx, %rax
	je	.LBB24_19
.LBB24_18:                              # %if.then55
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r14
.LBB24_19:                              # %if.end60
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	mark_reg_pointer
	movzbl	target_flags+1(%rip), %eax
	testq	$1, %rax
	je	.LBB24_34
# BB#20:                                # %if.then63
	movl	word_mode(%rip), %ebx
	movq	insn_data+49384(%rip), %rax
	testq	%rbx, %rbx
	jne	.LBB24_22
# BB#21:                                # %if.then66
	movzbl	40(%rax), %ebx
.LBB24_22:                              # %if.end70
	movq	24(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB24_23
# BB#24:                                # %land.lhs.true73
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	*%rcx
	testq	%r13, %rax
	je	.LBB24_26
# BB#25:
	movq	%r12, %r15
	jmp	.LBB24_33
.LBB24_1:                               # %if.then
	movq	global_rtl+64(%rip), %r14
	jmp	.LBB24_46
.LBB24_34:                              # %if.else
	movq	cfun(%rip), %rax
	movzbl	425(%rax), %eax
	testq	$128, %rax
	je	.LBB24_36
# BB#35:                                # %if.then85
	callq	gen_label_rtx
	movq	%rax, %r15
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	optab_table+16(%rip), %rsi
	movq	global_rtl+16(%rip), %rdx
	movq	stack_limit_rtx(%rip), %rax
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	$2, (%rsp)
	movq	%rax, %rcx
	callq	expand_binop
	movl	target_flags(%rip), %r8d
	movq	$25, %rcx
	shrq	%cl, %r8
	andq	$1, %r8
	orq	$4, %r8
	movq	$108, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r9
	movq	%r15, (%rsp)
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	emit_cmp_and_jump_insns
	callq	gen_trap
	movq	%rax, %rdi
	callq	emit_insn
	callq	emit_barrier
	movq	%r15, %rdi
	callq	emit_label
.LBB24_36:                              # %if.end98
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	protect_from_queue
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB24_41
# BB#37:                                # %if.end.i154
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB24_39
# BB#38:                                # %if.then2.i
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	8(%rax), %rdx
	movl	8(%rcx), %esi
	addq	%rdx, %rsi
	movl	%esi, 8(%rcx)
.LBB24_39:                              # %if.end4.i
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	optab_table+16(%rip), %rsi
	movq	global_rtl+16(%rip), %rdx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rax, %rcx
	movq	%rdx, %r8
	callq	expand_binop
	movq	global_rtl+16(%rip), %rdi
	cmpq	%rdi, %rax
	je	.LBB24_41
# BB#40:                                # %if.then8.i
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB24_41:                              # %anti_adjust_stack.exit
	movq	global_rtl+64(%rip), %rsi
	movq	%r14, %rdi
	callq	emit_move_insn
	jmp	.LBB24_42
.LBB24_23:
	movq	%r12, %r15
	jmp	.LBB24_33
.LBB24_26:                              # %if.then76
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	general_operand
	testq	%r13, %rax
	jne	.LBB24_28
# BB#27:                                # %if.then.i
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	force_operand
	movq	%rax, %r12
.LBB24_28:                              # %if.end.i
	movzbl	2(%r12), %eax
	cmpq	%rbx, %rax
	je	.LBB24_31
# BB#29:                                # %if.end.i
	testq	%rax, %rax
	jne	.LBB24_30
.LBB24_31:                              # %if.end8.i
	cmpq	%r15, %r12
	je	.LBB24_33
# BB#32:                                # %if.then10.i
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	emit_move_insn
.LBB24_33:                              # %if.end78
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	gen_allocate_stack
	movq	%rax, %rdi
	callq	emit_insn
.LBB24_42:                              # %if.end100
	movslq	ix86_preferred_stack_boundary(%rip), %rax
	cmpq	$127, %rax
	jg	.LBB24_44
# BB#43:                                # %if.then103
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	movq	optab_table(%rip), %r15
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	xorq	%rdi, %rdi
	movq	$15, %rsi
	callq	gen_rtx_CONST_INT
	xorq	%r8, %r8
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	expand_binop
	movl	target_flags(%rip), %ebx
	movq	%rax, %r14
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	xorq	%rdi, %rdi
	movq	$16, %rsi
	callq	gen_rtx_CONST_INT
	xorq	%rdi, %rdi
	movq	$62, %rsi
	xorq	%r9, %r9
	movq	$1, (%rsp)
	movq	%rbx, %rdx
	movq	%r14, %rcx
	movq	%rax, %r8
	callq	expand_divmod
	movl	target_flags(%rip), %ebx
	movq	%rax, %r14
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	xorq	%rdi, %rdi
	movq	$16, %rsi
	callq	gen_rtx_CONST_INT
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	expand_mult
	movq	%rax, %r14
.LBB24_44:                              # %if.end119
	movq	cfun(%rip), %rsi
	cmpq	$0, 168(%rsi)
	je	.LBB24_46
# BB#45:                                # %if.then122
	addq	$184, %rsi
	movq	$2, %rdi
	xorq	%rdx, %rdx
	callq	emit_stack_save
.LBB24_46:                              # %return
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB24_10:                              # %if.then25
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.allocate_dynamic_stack_space, %rcx
	movq	$1306, %rsi             # imm = 0x51A
	jmp	.LBB24_11
.LBB24_30:                              # %if.then7.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.copy_to_mode_reg, %rcx
	movq	$713, %rsi              # imm = 0x2C9
.LBB24_11:                              # %if.then25
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp251:
	.size	allocate_dynamic_stack_space, .Ltmp251-allocate_dynamic_stack_space
	.cfi_endproc

	.globl	probe_stack_range
	.align	16, 0x90
	.type	probe_stack_range,@function
probe_stack_range:                      # @probe_stack_range
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
	subq	$16, %rsp
.Ltmp263:
	.cfi_def_cfa_offset 64
.Ltmp264:
	.cfi_offset %rbx, -48
.Ltmp265:
	.cfi_offset %r12, -40
.Ltmp266:
	.cfi_offset %r13, -32
.Ltmp267:
	.cfi_offset %r14, -24
.Ltmp268:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movzbl	2(%r12), %eax
	movq	%rdi, %r14
	testq	%rax, %rax
	je	.LBB25_3
# BB#1:                                 # %land.lhs.true
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	cmpq	%rdi, %rax
	je	.LBB25_3
# BB#2:                                 # %if.then
	movq	$1, %rdx
	movq	%r12, %rsi
	callq	convert_to_mode
	movq	%rax, %r12
.LBB25_3:                               # %if.end
	cmpq	$0, stack_check_libfunc(%rip)
	je	.LBB25_4
# BB#16:                                # %if.then9
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	movq	global_rtl+16(%rip), %r15
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	plus_constant_wide
	movq	%rax, %rcx
	movq	$76, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	gen_rtx
	movq	$2, %rdi
	movq	%rax, %rsi
	callq	memory_address
	movq	%rax, %rbx
	movq	stack_check_libfunc(%rip), %rdi
	movl	ptr_mode(%rip), %r9d
	movabsq	$0, %rsi
	movabsq	$0, %rdx
	movq	$1, %rcx
	xorq	%rax, %rax
	movq	%rbx, %r8
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_library_call  # TAILCALL
.LBB25_4:                               # %if.else
	movzbl	(%r12), %eax
	cmpq	$54, %rax
	jne	.LBB25_11
# BB#5:                                 # %land.lhs.true19
	movq	8(%r12), %rax
	cmpq	$40959, %rax            # imm = 0x9FFF
	jg	.LBB25_11
# BB#6:                                 # %for.cond.preheader
	movl	target_flags(%rip), %ebx
	leaq	4096(%r14), %r15
	jmp	.LBB25_8
	.align	16, 0x90
.LBB25_7:                               # %for.body
                                        #   in Loop: Header=BB25_8 Depth=1
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_CONST_INT
	movq	$76, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movl	word_mode(%rip), %edi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movl	(%rax), %ecx
	orq	$134217728, %rcx        # imm = 0x8000000
	movl	%ecx, (%rax)
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rax, %rdi
	callq	emit_move_insn
	addq	$4096, %r15             # imm = 0x1000
	movl	target_flags(%rip), %ebx
	movq	8(%r12), %rax
.LBB25_8:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$25, %rcx
	shrq	%cl, %rbx
	movq	global_rtl+16(%rip), %r13
	andq	$1, %rbx
	orq	$4, %rbx
	cmpq	%rax, %r15
	jl	.LBB25_7
# BB#9:                                 # %for.end
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	plus_constant_wide
	movq	$76, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movl	word_mode(%rip), %edi
	movq	%rax, %rsi
	jmp	.LBB25_10
.LBB25_11:                              # %if.else37
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	movq	global_rtl+16(%rip), %r15
	andq	$1, %rbx
	orq	$4, %rbx
	leaq	4096(%r14), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$76, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	force_operand
	movl	target_flags(%rip), %ebx
	movq	%rax, %r15
	movq	$25, %rcx
	movq	global_rtl+16(%rip), %r13
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	plus_constant_wide
	movq	$76, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	force_operand
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$4096, %rsi             # imm = 0x1000
	callq	gen_rtx_CONST_INT
	movq	%rax, 8(%rsp)           # 8-byte Spill
	callq	gen_label_rtx
	movq	%rax, %rbx
	callq	gen_label_rtx
	movq	%rax, %r13
	callq	gen_label_rtx
	movzbl	(%r15), %ecx
	movq	%rax, %r12
	cmpq	$61, %rcx
	jne	.LBB25_13
# BB#12:                                # %lor.lhs.false
	movl	8(%r15), %eax
	cmpq	$52, %rax
	ja	.LBB25_14
.LBB25_13:                              # %if.then61
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%r15, %rsi
	callq	force_reg
	movq	%rax, %r15
.LBB25_14:                              # %if.end66
	movabsq	$4294967200, %rsi       # imm = 0xFFFFFFA0
	xorq	%rdi, %rdi
	callq	emit_note
	movq	%r13, %rdi
	callq	emit_jump
	movq	%rbx, %rdi
	callq	emit_label
	movl	word_mode(%rip), %edi
	movq	%r15, %rsi
	callq	gen_rtx_MEM
	movl	(%rax), %ecx
	orq	$134217728, %rcx        # imm = 0x8000000
	movl	%ecx, (%rax)
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rax, %rdi
	callq	emit_move_insn
	movabsq	$4294967202, %rsi       # imm = 0xFFFFFFA2
	xorq	%rdi, %rdi
	callq	emit_note
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	optab_table+16(%rip), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	$1, %r9
	movq	$2, (%rsp)
	movq	%r15, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%r15, %r8
	callq	expand_binop
	cmpq	%r15, %rax
	jne	.LBB25_17
# BB#15:                                # %if.end76
	movq	%r13, %rdi
	callq	emit_label
	movl	target_flags(%rip), %r8d
	movq	$25, %rcx
	shrq	%cl, %r8
	andq	$1, %r8
	orq	$4, %r8
	movq	$109, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r9
	movq	%rbx, (%rsp)
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	emit_cmp_and_jump_insns
	movq	%r12, %rdi
	callq	emit_jump
	movabsq	$4294967201, %rsi       # imm = 0xFFFFFFA1
	xorq	%rdi, %rdi
	callq	emit_note
	movq	%r12, %rdi
	callq	emit_label
	movl	word_mode(%rip), %edi
	movq	%r14, %rsi
.LBB25_10:                              # %for.end
	callq	gen_rtx_MEM
	movl	(%rax), %ecx
	orq	$134217728, %rcx        # imm = 0x8000000
	movl	%ecx, (%rax)
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rax, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_move_insn  # TAILCALL
.LBB25_17:                              # %if.then75
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.probe_stack_range, %rcx
	movq	$1577, %rsi             # imm = 0x629
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp269:
	.size	probe_stack_range, .Ltmp269-probe_stack_range
	.cfi_endproc

	.globl	set_stack_check_libfunc
	.align	16, 0x90
	.type	set_stack_check_libfunc,@function
set_stack_check_libfunc:                # @set_stack_check_libfunc
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$stack_check_libfunc, %rax
	movq	$1, %rsi
	movq	%rdi, stack_check_libfunc(%rip)
	movq	%rax, %rdi
	jmp	ggc_add_rtx_root  # TAILCALL
.Ltmp270:
	.size	set_stack_check_libfunc, .Ltmp270-set_stack_check_libfunc
	.cfi_endproc

	.globl	hard_function_value
	.align	16, 0x90
	.type	hard_function_value,@function
hard_function_value:                    # @hard_function_value
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp274:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp275:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp276:
	.cfi_def_cfa_offset 32
.Ltmp277:
	.cfi_offset %rbx, -24
.Ltmp278:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	ix86_function_value
	movq	%rax, %r14
	movl	(%r14), %eax
	andq	$16777215, %rax         # imm = 0xFFFFFF
	cmpq	$3342397, %rax          # imm = 0x33003D
	jne	.LBB27_7
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	callq	int_size_in_bytes
	movl	class_narrowest_mode+4(%rip), %edx
	jmp	.LBB27_2
	.align	16, 0x90
.LBB27_4:                               # %for.inc
                                        #   in Loop: Header=BB27_2 Depth=1
	movzbl	mode_wider_mode(%rsi), %edx
.LBB27_2:                               # %if.then
                                        # =>This Inner Loop Header: Depth=1
	testq	%rdx, %rdx
	je	.LBB27_5
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB27_2 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	mode_size(%rsi), %ecx
	cmpq	%rax, %rcx
	jb	.LBB27_4
# BB#6:                                 # %if.end15
	movb	%dl, 2(%r14)
.LBB27_7:                               # %if.end18
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB27_5:                               # %if.then14
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.hard_function_value, %rcx
	movq	$1635, %rsi             # imm = 0x663
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp279:
	.size	hard_function_value, .Ltmp279-hard_function_value
	.cfi_endproc

	.globl	hard_libcall_value
	.align	16, 0x90
	.type	hard_libcall_value,@function
hard_libcall_value:                     # @hard_libcall_value
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	ix86_libcall_value  # TAILCALL
.Ltmp280:
	.size	hard_libcall_value, .Ltmp280-hard_libcall_value
	.cfi_endproc

	.globl	rtx_to_tree_code
	.align	16, 0x90
	.type	rtx_to_tree_code,@function
rtx_to_tree_code:                       # @rtx_to_tree_code
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$91, %rax
	jg	.LBB29_3
# BB#1:                                 # %entry
	addq	$-75, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$4, %rcx
	ja	.LBB29_10
# BB#2:                                 # %entry
	movq	$59, %rax
	jmpq	*.LJTI29_0(,%rcx,8)
.LBB29_6:                               # %sw.bb1
	movq	$60, %rax
	retq
.LBB29_3:                               # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdi
	cmpq	$92, %rdi
	jne	.LBB29_4
# BB#9:                                 # %sw.bb4
	movq	$78, %rax
	retq
.LBB29_4:                               # %entry
	cmpq	$93, %rdi
	jne	.LBB29_10
# BB#5:                                 # %sw.bb5
	movq	$79, %rax
	retq
.LBB29_10:                              # %sw.default
	movq	$147, %rax
.LBB29_11:                              # %sw.epilog
	retq
.LBB29_7:                               # %sw.bb2
	movq	$61, %rax
	retq
.LBB29_8:                               # %sw.bb3
	movq	$70, %rax
	retq
.Ltmp281:
	.size	rtx_to_tree_code, .Ltmp281-rtx_to_tree_code
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI29_0:
	.quad	.LBB29_11
	.quad	.LBB29_6
	.quad	.LBB29_10
	.quad	.LBB29_7
	.quad	.LBB29_8

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"explow.c"
	.size	.L.str, 9

	.type	.L__FUNCTION__.find_next_ref,@object # @__FUNCTION__.find_next_ref
.L__FUNCTION__.find_next_ref:
	.asciz	"find_next_ref"
	.size	.L__FUNCTION__.find_next_ref, 14

	.type	.L__FUNCTION__.copy_to_mode_reg,@object # @__FUNCTION__.copy_to_mode_reg
.L__FUNCTION__.copy_to_mode_reg:
	.asciz	"copy_to_mode_reg"
	.size	.L__FUNCTION__.copy_to_mode_reg, 17

	.type	.L__FUNCTION__.emit_stack_save,@object # @__FUNCTION__.emit_stack_save
.L__FUNCTION__.emit_stack_save:
	.asciz	"emit_stack_save"
	.size	.L__FUNCTION__.emit_stack_save, 16

	.type	.L__FUNCTION__.allocate_dynamic_stack_space,@object # @__FUNCTION__.allocate_dynamic_stack_space
.L__FUNCTION__.allocate_dynamic_stack_space:
	.asciz	"allocate_dynamic_stack_space"
	.size	.L__FUNCTION__.allocate_dynamic_stack_space, 29

	.type	stack_check_libfunc,@object # @stack_check_libfunc
	.local	stack_check_libfunc
	.comm	stack_check_libfunc,8,8
	.type	.L__FUNCTION__.probe_stack_range,@object # @__FUNCTION__.probe_stack_range
.L__FUNCTION__.probe_stack_range:
	.asciz	"probe_stack_range"
	.size	.L__FUNCTION__.probe_stack_range, 18

	.type	.L__FUNCTION__.hard_function_value,@object # @__FUNCTION__.hard_function_value
.L__FUNCTION__.hard_function_value:
	.asciz	"hard_function_value"
	.size	.L__FUNCTION__.hard_function_value, 20


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
